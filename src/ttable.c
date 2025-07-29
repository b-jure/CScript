/*
** ttable.c
** Hash Table
** See Copyright Notice in tokudae.h
*/

#define ttable_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include <math.h>

#include "tstring.h"
#include "ttable.h"
#include "tokudaeconf.h"
#include "tgc.h"
#include "tokudae.h"
#include "tokudaelimits.h"
#include "tmem.h"
#include "tdebug.h"
#include "tobject.h"
#include "tobject.h"
#include "tstate.h"


/*
** MAXHBITS is the largest integer such that 2^MAXHBITS fits in a
** signed int.
*/
#define MAXHBITS        cast_int(sizeof(int) * CHAR_BIT - 2)


/*
** MAXHSIZE is the maximum size of the hash array. It is the minimum
** between 2^MAXHBITS and the maximum size such that, measured in bytes,
** it fits in a 'size_t'.
*/
#define MAXHSIZE        cast_sizet(tokuM_limitN(1 << MAXHBITS, Node))


/*
** MINHSIZE is the minimum size for the hash array.
*/
#define MINHSIZE        4


/*
** When the original hash value is good, hashing by a power of 2
** avoids the cost of '%'.
*/
#define hashpow2(t,h)       htnode(t, tmod(h, htsize(t)))


/*
** For other types, it is better to avoid modulo by power of 2, as
** they can have many 2 factors.
*/
#define hashmod(t,n)    (htnode(t, ((n) % ((htsize(t)-1u)|1u))))


#define hashstr(t,s)       hashpow2(t, (s)->hash)
#define hashboolean(t,b)   hashpow2(t, b)


#define hashpointer(t,p)   hashmod(t, pointer2uint(p))


/*
** Table size invariant.
*/
#define tablesize_invariant(t,sz) \
    ((t && isdummy(t)) || (4 <= (sz) && !((sz) & ((sz)-1)) && (sz) < MAXHSIZE))


#define dummynode	(&dummynode_)

/*
** Common hash part for tables with empty hashes. That allows all
** tables to have a hash, avoiding an extra check ("is there a hash
** part?") when indexing. Its sole node has an empty value and a key
** (DEADKEY, NULL) that is different from any valid TValue.
*/
static const Node dummynode_ = {
    {{NULL}, TOKU_VEMPTY, /* value's value and type */
    TOKU_TDEADKEY, 0, {NULL}} /* key type, next, and key value */
};


/* empty key constant */
static const TValue absentkey = {ABSTKEYCONSTANT};


/*
** Hash for integers. To allow a good hash, use the remainder operator
** ('%'). If integer fits as a non-negative int, compute an int
** remainder, which is faster. Otherwise, use an unsigned-integer
** remainder, which uses all bits and ensures a non-negative result.
*/
static Node *hashint(const Table *t, toku_Integer i) {
    toku_Unsigned ui = t_castS2U(i);
    if (ui <= cast_uint(INT_MAX))
        return htnode(t, cast_int(ui) % cast_int((htsize(t)-1) | 1));
    else
        return hashmod(t, ui);
}


/*
** Hash for floating-point numbers.
** The main computation should be just
**     n = frexp(n, &i); return (n * INT_MAX) + i
** but there are some numerical subtleties.
** In a two-complement representation, INT_MAX does not have an exact
** representation as a float, but INT_MIN does; because the absolute
** value of 'frexp' is smaller than 1 (unless 'n' is inf/NaN), the
** absolute value of the product 'frexp * -INT_MIN' is smaller or equal
** to INT_MAX. Next, the use of 't_uint' avoids overflows when ** adding 'i';
** the use of '~u' (instead of '-u') avoids problems with INT_MIN.
*/
#if !defined(t_hashfloat)
static t_uint t_hashfloat(toku_Number n) {
    int i;
    toku_Integer ni;
    n = t_mathop(frexp)(n, &i) * -cast_num(INT_MIN);
    if (!toku_number2integer(n, &ni)) { /* is 'n' inf/-inf/NaN? */
        toku_assert(t_numisnan(n) || t_mathop(fabs)(n) == cast_num(HUGE_VAL));
        return 0;
    } else { /* normal case */
        t_uint u = cast_uint(i) + cast_uint(ni);
        return (u <= cast_uint(INT_MAX) ? u : ~u);
    }
}
#endif


/*
** Find the main position (slot) for key 'k' inside
** the hash array.
*/
static Node *mainposition(const Table *t, const TValue *k) {
    switch (ttypetag(k)) {
        case TOKU_VTRUE: return hashboolean(t, 1);
        case TOKU_VFALSE: return hashboolean(t, 0);
        case TOKU_VSHRSTR: {
            OString *s = strval(k);
            return hashstr(t, s);
        }
        case TOKU_VLNGSTR: {
            OString *s = strval(k);
            return hashpow2(t, tokuS_hashlngstr(s));
        }
        case TOKU_VNUMINT: {
            toku_Integer i = ival(k);
            return hashint(t, i);
        }
        case TOKU_VNUMFLT: {
            toku_Number n = fval(k);
            return hashpow2(t, t_hashfloat(n));
        }
        case TOKU_VLIGHTUSERDATA: {
            void *p = pval(k);
            return hashpointer(t, p);
        }
        case TOKU_VLCF: {
            toku_CFunction lcf = lcfval(k);
            return hashpointer(t, lcf);
        }
        default: {
            GCObject *o = gcoval(k);
            return hashpointer(t, o);
        }
    }
}


static inline Node *mainposfromnode(Table *t, Node *mp) {
    TValue key;
    getnodekey(cast(toku_State *, NULL), &key, mp);
    return mainposition(t, &key);
}


static Node *getfreepos(Table *t) {
    while (t->lastfree > t->node) {
        t->lastfree--;
        if (keyisnil(t->lastfree))
            return t->lastfree;
    }
    return NULL; /* no free position */
}


/*
** Check whether key 'k' is equal to the key in node 'n'. This
** equality is raw, so there are no metamethods. Floats with integer
** values have been normalized, so integers cannot be equal to
** floats. It is assumed that 'eqshrstr' is simply pointer equality, so
** that short strings are handled in the default case.
** A true 'deadok' means to accept dead keys as equal to their original
** values. All dead keys are compared in the default case, by pointer
** identity. (Only collectable objects can produce dead keys.) Note that
** dead long strings are also compared by identity.
** Once a key is dead, its corresponding value may be collected, and
** then another value can be created with the same address. If this
** other value is given to 'next', 'eqkey' will signal a false
** positive. In a regular traversal, this situation should never happen,
** as all keys given to 'next' came from the table itself, and therefore
** could not have been collected. Outside a regular traversal, we
** have garbage in, garbage out. What is relevant is that this false
** positive does not break anything. (In particular, 'next' will return
** some other valid item on the table or nil.)
*/
static int eqkey(const TValue *k, const Node *n, int deadok) {
    if ((rawtt(k) != keytt(n)) && /* not the same variant? */
            !(deadok && keyisdead(n) && iscollectable(k)))
        return 0;
    switch (ttypetag(k)) {
        case TOKU_VNIL: case TOKU_VTRUE: case TOKU_VFALSE:
            return 1;
        case TOKU_VNUMINT:
            return (ival(k) == keyival(n));
        case TOKU_VNUMFLT:
            return t_numeq(fval(k), keyfval(n));
        case TOKU_VLIGHTUSERDATA:
            return (pval(k) == keypval(n));
        case TOKU_VLCF:
            return (lcfval(k) == keycfval(n));
        case TOKU_VSHRSTR:
            return eqshrstr(strval(k), keystrval(n));
        case TOKU_VLNGSTR:
            return tokuS_eqlngstr(strval(k), keystrval(n));
        default: /* rest of the objects are compared by pointer identity */
            toku_assert(iscollectable(k));
            return (gcoval(k) == keygcoval(n));
        }
    return 0;
}


static const TValue *getgeneric(Table *t, const TValue *key, int deadok) {
    Node *n = mainposition(t, key);
    for (;;) {
        if (eqkey(key, n, deadok)) {
            return nodeval(n);
        } else {
            int next = nodenext(n);
            if (next == 0) /* end of node list ? */
                return &absentkey;
            n += next;
        }
    }
}


/*
** Inserts a new key into a hash table; first, check whether key's main
** position is free. If not, check whether colliding node is in its main
** position or not: if it is not, move colliding node to an empty place
** and put new key in its main position; otherwise (colliding node is in
** its main position), new key goes to an empty position. Return 0 if
** could not insert key (could not find a free space).
*/
static int insertkey(Table *t, const TValue *key, const TValue *value) {
    Node *mp = mainposition(t, key); /* get main position for 'key' */
    toku_assert(isabstkey(getgeneric(t, key, 0)));
    if (!isempty(nodeval(mp)) || isdummy(t)) { /* mainposition taken? */
        Node *othern;
        Node *f = getfreepos(t); /* get next free position */
        if (f == NULL) /* no free position? */
            return 0;
        toku_assert(!isdummy(t));
        othern = mainposfromnode(t, mp);
        if (othern != mp) { /* colliding node out of its main position? */
            /* yes; move colliding node into free position */
            while (othern + nodenext(othern) != mp) /* find previous */
                othern += nodenext(othern);
            nodenext(othern) = f - othern; /* rechain to point to 'f' */
            *f = *mp; /* copy colliding node into free pos. (mp->next also goes) */
            if (nodenext(mp) != 0) {
                nodenext(f) += mp - f; /* correct 'next' */
                nodenext(mp) = 0; /* now 'mp' is free */
            }
            setemptyval(nodeval(mp));
        } else { /* colliding node is in its own main position */
            /* new node will go into free position */
            if (nodenext(mp) != 0)
                nodenext(f) = mp + nodenext(mp) - f; /* chain new position */
            else toku_assert(nodenext(f) == 0);
            nodenext(mp) = f - mp;
            mp = f;
        }
    }
    setnodekey(cast(toku_State *, 0), mp, key); /* set key */
    toku_assert(isempty(nodeval(mp))); /* value slot must be empty */
    setobj(cast(toku_State *, 0), nodeval(mp), value); /* set value */
    return 1;
}


static void rehash(toku_State *T, Table *t) {
    int nhash = 0;
    if (!isdummy(t)) {
        t_uint size = htsize(t);
        toku_assert(tablesize_invariant(t, size));
        const Node *n = htnode(t, 0);
        t_uint i;
        for (i = 0; i < size; i += 4) { /* unroll */
            nhash += !isempty(nodeval(n)); n++;
            nhash += !isempty(nodeval(n)); n++;
            nhash += !isempty(nodeval(n)); n++;
            nhash += !isempty(nodeval(n)); n++;
        }
        toku_assert(i == size);
    }
    nhash++; /* +1 for extra key being inserted */
    tokuH_resize(T, t, nhash);
}


/*
** Insert a key in a table where there is space for that key, the
** key is valid, and the value is not nil.
*/
static void newcheckedkey(Table *t, const TValue *key, const TValue *value) {
    int done = insertkey(t, key, value); /* insert key */
    toku_assert(done); /* it cannot fail */
    UNUSED(done); /* to avoid warnings */
}


static void newkey(toku_State *T, Table *t, const TValue *key,
                                            const TValue *value) {
    if (!ttisnil(value)) { /* do not insert nil values */
        int done = insertkey(t, key, value);
        if (!done) { /* could not find a free place? */
            rehash(T, t); /* grow table */
            newcheckedkey(t, key, value); /* insert key in grown table */
        }
        tokuG_barrierback(T, obj2gco(t), key);
        /* for debugging only: any new key may force an emergency collection */
        condchangemem(T, (void)0, (void)0, 1);
    }
}


t_sinline t_ubyte finishget(const TValue *slot, TValue *res) {
    if (!ttisnil(slot)) {
        setobj(cast(toku_State *, NULL), res, slot);
    }
    return ttypetag(slot);
}


/*
** Returns the index of a 'key' for table traversals.
** The beginning of a traversal is signaled by 0.
*/
static t_uint getindex(toku_State *T, Table *t, const TValue *k) {
    const TValue *slot;
    if (ttisnil(k)) return 0; /* first iteration */
    slot = getgeneric(t, k, 1);
    if (t_unlikely(isabstkey(slot)))
        tokuD_runerror(T, "invalid key passed to 'nextfield'"); /* not found */
    t_uint i = cast(Node *, slot) - htnode(t, 0); /* key index in the array */
    return i + 1; /* return next slot index */
}


int tokuH_next(toku_State *T, Table *t, SPtr key) {
    t_uint size = htsize(t);
    t_uint i = getindex(T, t, s2v(key));
    for (; i < size; i++) {
        Node *slot = htnode(t, i);
        if (!isempty(nodeval(slot))) {
            getnodekey(T, s2v(key), slot);
            setobj2s(T, key + 1, nodeval(slot));
            return 1;
        }
    }
    return 0;
}


/*
** Length of a table is the number of key-(non-nil)value pairs.
*/
int tokuH_len(Table *t) {
    int len = 0;
    if (!isdummy(t)) {
        t_uint size = check_exp(htsize(t) <= INT_MAX, htsize(t));
        const Node *n = htnode(t, 0);
        t_uint i;
        toku_assert(tablesize_invariant(t, size));
        for (i = 0; i < size; i += 4) { /* unroll */
            len += !isempty(nodeval(n)); n++;
            len += !isempty(nodeval(n)); n++;
            len += !isempty(nodeval(n)); n++;
            len += !isempty(nodeval(n)); n++;
        }
        toku_assert(i == size);
    }
    return len;
}


t_sinline void copynode(toku_State *T, const Node *n, Table *t) {
    if (!isempty(nodeval(n))) {
        TValue k;
        getnodekey(T, &k, n);
        tokuH_set(T, t, &k, nodeval(n));
        tokuG_barrierback(T, obj2gco(t), nodeval(n));
    }
}


/*
** Insert all key-(non-nil)value pairs from source table into
** destination table.
** WARNING: when using this function the caller probably needs to
** invalidate the TM cache. (This function takes care of GC barrier.)
*/
void tokuH_copy(toku_State *T, Table *dest, Table *src) {
    if  (!isdummy(src)) {
        const Node *n = htnode(src, 0);
        t_uint size = htsize(src);
        t_uint lsrc = cast_uint(tokuH_len(src));
        t_uint i;
        if (isdummy(dest) || htsize(dest) < lsrc)
            tokuH_resize(T, dest, lsrc);
        toku_assert(tablesize_invariant(src, size));
        toku_assert(tablesize_invariant(dest, htsize(dest)));
        for (i = 0; i < size; i += 4) { /* unroll */
            copynode(T, n++, dest);
            copynode(T, n++, dest);
            copynode(T, n++, dest);
            copynode(T, n++, dest);
        }
        toku_assert(i == size);
    }
}


const TValue *tokuH_Hgetshortstr(Table *t, OString *key) {
    Node *n = hashstr(t, key);
    toku_assert(strisshr(key));
    for (;;) {
        if (keyisshrstr(n) && eqshrstr(key, keystrval(n)))
            return nodeval(n);
        else {
            int next = nodenext(n);
            if (next == 0) break;
            n += next;
        }
    }
    return &absentkey; /* not found */
}


static const TValue *Hgetlongstr(Table *t, OString *key) {
    TValue k;
    toku_assert(!strisshr(key));
    setstrval(cast(toku_State *, NULL), &k, key);
    return getgeneric(t, &k, 0); /* for long strings, use generic case */
}


const TValue *Hgetstr(Table *t, OString *key) {
    if (strisshr(key))
        return tokuH_Hgetshortstr(t, key);
    else /* otherwise long string */
        return Hgetlongstr(t, key);
}


t_ubyte tokuH_getstr(Table *t, OString *key, TValue *res) {
    return finishget(Hgetstr(t, key), res);
}


const TValue *Hgetint(Table *t, toku_Integer key) {
    Node *n = hashint(t, key);
    for (;;) {
        if (keyisint(n) && keyival(n) == key)
            return nodeval(n);
        else {
            int next = nodenext(n);
            if (next == 0)
                return &absentkey;
            n += next;
        }
    }
}


t_ubyte tokuH_getint(Table *t, toku_Integer key, TValue *res) {
    return finishget(Hgetint(t, key), res);
}


/*
** Main search function.
*/
t_ubyte tokuH_get(Table *t, const TValue *key, TValue *res) {
    const TValue *slot;
    switch (ttypetag(key)) {
        case TOKU_VSHRSTR:
            slot = tokuH_Hgetshortstr(t, strval(key));
            break;
        case TOKU_VNUMINT:
            return tokuH_getint(t, ival(key), res);
        case TOKU_VNIL:
            slot = &absentkey;
            break;
        case TOKU_VNUMFLT: {
            toku_Integer i;
            if (tokuO_n2i(fval(key), &i, N2IEQ)) /* integral index? */
                return tokuH_getint(t, i, res);
            /* else... */
        } /* fall through */
        default:
            slot = getgeneric(t, key, 0);
            break;
    }
    return finishget(slot, res);
}


t_ubyte tokuH_getshortstr(Table *t, OString *key, TValue *res) {
    return finishget(tokuH_Hgetshortstr(t, key), res);
}


/*
** When a 'pset' cannot be completed, this function returns an encoding
** of its result, to be used by 'luaH_finishset'.
*/
static int retpsetcode (Table *t, const TValue *slot) {
    if (isabstkey(slot))
        return HNOTFOUND; /* no slot with that key */
    else /* return node encoded */
        return cast_int((cast(Node*, slot) - t->node)) + HFIRSTNODE;
}


t_sinline int finishset(Table *t, const TValue *slot, const TValue *value) {
    if (!ttisnil(slot)) {
        setobj(cast(toku_State *, NULL), cast(TValue *, slot), value);
        return HOK;  /* success */
    } else
        return retpsetcode(t, slot);
}


int tokuH_psetint(Table *t, toku_Integer key, const TValue *value) {
    return finishset(t, Hgetint(t, key), value);
}


int tokuH_psetshortstr(Table *t, OString *key, const TValue *value) {
    return finishset(t, tokuH_Hgetshortstr(t, key), value);
}


static int psetlongstr(Table *t, OString *key, const TValue *value) {
    return finishset(t, Hgetlongstr(t, key), value);
}


int tokuH_psetstr(Table *t, OString *key, const TValue *value) {
    if (strisshr(key))
        return tokuH_psetshortstr(t, key, value);
    else
        return psetlongstr(t, key, value);
}


int tokuH_pset(Table *t, const TValue *key, const TValue *value) {
    switch (ttypetag(key)) {
        case TOKU_VLNGSTR: return psetlongstr(t, strval(key), value);
        case TOKU_VSHRSTR: return tokuH_psetshortstr(t, strval(key), value);
        case TOKU_VNUMINT: return tokuH_psetint(t, ival(key), value);
        case TOKU_VNIL: return HNOTFOUND;
        case TOKU_VNUMFLT: {
            toku_Integer k;
            if (tokuO_n2i(fval(key), &k, N2IEQ)) /* integral index? */
                return tokuH_psetint(t, k, value);
            /* else... */
        } /* fall through */
        default: return finishset(t, getgeneric(t, key, 0), value);
    }
}


/*
** Finish a raw "set table" operation.
** WARNING: when using this function the caller probably needs to
** check a GC barrier and invalidate the TM cache.
*/
void tokuH_finishset(toku_State *T, Table *t, const TValue *key,
                                    const TValue *value, int hres) {
    toku_assert(hres != HOK && hres > 0);
    if (hres == HNOTFOUND) {
        TValue aux;
        if (t_unlikely(ttisnil(key)))
            tokuD_runerror(T, "table index is nil");
        else if (ttisflt(key)) {
            toku_Number f = fval(key);
            toku_Integer k;
            if (tokuO_n2i(f, &k, N2IEQ)) {
                setival(&aux, k); /* key is equal to an integer */
                key = &aux; /* insert it as an integer */
            } else if (t_unlikely(t_numisnan(f)))
                tokuD_runerror(T, "table index is NaN");
        }
        newkey(T, t, key, value);
    } else /* otherwise node index */
        setobj(T, nodeval(htnode(t, hres - HFIRSTNODE)), value);
}


/*
** WARNING: when using this function the caller probably needs to
** check a GC barrier and invalidate the TM cache.
*/
void tokuH_set(toku_State *T, Table *t, const TValue *key,
                                        const TValue *value) {
    int hres = tokuH_pset(t, key, value);
    if (hres != HOK)
        tokuH_finishset(T, t, key, value, hres);
}


static int rawfinishset(const TValue *slot, const TValue *value) {
    if (isabstkey(slot))
        return 0;  /* no slot with that key */
    else {
        setobj(((toku_State*)NULL), cast(TValue *, slot), value);
        return 1; /* success */
    }
}


/*
** WARNING: when using this function the caller probably needs to
** check a GC barrier and invalidate the TM cache.
*/
void tokuH_setstr(toku_State *T, Table *t, OString *key, const TValue *value) {
    if (!rawfinishset(Hgetstr(t, key), value)) {
        TValue k;
        setstrval(T, &k, key);
        newkey(T, t, &k, value);
    }
}


/*
** WARNING: when using this function the caller probably needs to
** check a GC barrier. (No need to invalidate TM cache, as integers
** cannot be keys to metamethods.)
*/
void tokuH_setint(toku_State *T, Table *t, toku_Integer key,
                                           const TValue *value) {
    if (!rawfinishset(Hgetint(t, key), value)) {
        TValue k;
        setival(&k, key);
        newkey(T, t, &k, value);
    }
}


/*
** Exchange the hash part of 't1' and 't2'. (In 'flags', only the
** dummy bit must be exchanged: The metamethod bits do not change
** during a resize, so the "real" table can keep their values.)
*/
static void exchangehashes(Table *t1, Table *t2) {
    t_ubyte sz = t1->size;
    Node *node = t1->node;
    Node *lastfree = t1->lastfree;
    int bitdummy1 = t1->flags & BITDUMMY;
    t1->size = t2->size;
    t1->node = t2->node;
    t1->lastfree = t2->lastfree;
    t1->flags = cast_byte((t1->flags & NOTBITDUMMY) | (t2->flags & BITDUMMY));
    t2->size = sz;
    t2->node = node;
    t2->lastfree = lastfree;
    t2->flags = cast_byte((t2->flags & NOTBITDUMMY) | bitdummy1);
}


t_sinline void reinsertnode(toku_State *T, const Node *oldn, Table *t) {
    if (!isempty(nodeval(oldn))) {
        TValue key;
        getnodekey(T, &key, oldn);
        newcheckedkey(t, &key, nodeval(oldn));
    }
}


static void reinserthash(toku_State *T, Table *ot, Table *t) {
    if (!isdummy(ot)) {
        t_uint size = htsize(ot);
        const Node *oldn = htnode(ot, 0);
        t_uint i;
        toku_assert(tablesize_invariant(ot, size));
        for (i = 0; i < size; i += 4) { /* unroll */
            reinsertnode(T, oldn, t); oldn++;
            reinsertnode(T, oldn, t); oldn++;
            reinsertnode(T, oldn, t); oldn++;
            reinsertnode(T, oldn, t); oldn++;
        }
        toku_assert(i == size);
    }
}


t_sinline void initnode(Node *n) {
    nodenext(n) = 0;
    setnilkey(n);
    setemptyval(nodeval(n));
}


/* allocate hash array */
static void newhasharray(toku_State *cr, Table *t, t_uint size) {
    if (size == 0) { /* no elements? */
        t->node = cast(Node *, dummynode); /* use common 'dummynode' */
        t->size = 0;
        t->lastfree = NULL;
        setdummy(t); /* signal that it is using dummy node */
    } else {
        Node *n;
        int nbits;
        size = (MINHSIZE <= size) ? size : MINHSIZE;
        nbits = tokuO_ceillog2(size);
        if (t_unlikely(MAXHBITS < nbits || cast_uint(MAXHSIZE) < twoto(nbits)))
            tokuD_runerror(cr, "table overflow");
        size = twoto(nbits);
        t->node = tokuM_newarray(cr, size, Node);
        t->size = nbits;
        t->lastfree = htnode(t, size);
        setnodummy(t);
        toku_assert(tablesize_invariant(t, size));
        n = htnode(t, 0);
        for (t_uint i = 0; i < size; i += 4) { /* unroll */
            initnode(n++);
            initnode(n++);
            initnode(n++);
            initnode(n++);
        }
    }
}


Table *tokuH_new(toku_State *T) {
    GCObject *o = tokuG_new(T, sizeof(Table), TOKU_VTABLE);
    Table *t = gco2ht(o);
    t->flags = maskflags;  /* table has no metamethod fields */
    t->gclist = NULL;
    newhasharray(T, t, 0);
    return t;
}


static inline void freehash(toku_State *T, Table *t) {
    if (!isdummy(t))
        tokuM_freearray(T, t->node, htsize(t));
}


/*
** (Re)insert all elements from the hash part of 'ot' into table 't'.
*/
void tokuH_resize(toku_State *T, Table *t, t_uint newsize) {
    Table newt = {0};
    newhasharray(T, &newt, newsize);
    exchangehashes(t, &newt);
    reinserthash(T, &newt, t);
    freehash(T, &newt);
    toku_assert(tablesize_invariant(t, htsize(t)));
}


void tokuH_free(toku_State *T, Table *t) {
    freehash(T, t);
    tokuM_free(T, t);
}
