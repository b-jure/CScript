/*
** ttable.c
** Hash Table
** See Copyright Notice in tokudae.h
*/

#define ttable_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include <string.h>
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


/* largest integer such that 2^MAXHBITS fits in 'int' */
#define MAXHBITS        ((int)(sizeof(int) * CHAR_BIT - 1))

/* must be less than MAXHBITS */
#define MINHTBITS       3


/* maximum size for the hash array */
#define MAXHSIZE        (1u << MAXHBITS)

/* smallest size for the hash array */
#define MINHSIZE        twoto(MINHTBITS)


/* get hashtable 'node' slot from hash 'h' */
#define hashpow2(t,h)      htnode(t, hashmod(h, htsize(t)))


#define hashstr(t,s)       hashpow2(t, (s)->hash)
#define hashboolean(t,b)   hashpow2(t, b)
#define hashpointer(t,p)   hashpow2(t, pointer2uint(p))



/* empty key constant */
static const TValue absentkey = {ABSTKEYCONSTANT};


static t_uint hashflt(toku_Number n) {
    toku_Integer ni;
    int exp;
    n = t_mathop(frexp(n, &exp)) * -cast_num(INT_MIN);
    if (t_likely(toku_number2integer(n, &ni))) {
        t_uint ui = cast_uint(exp) + cast_uint(ni);
        return (ui <= cast_uint(TOKU_MAXINT) ? ui : cast_uint(~ui));
    }
    /* nan or -inf/+inf */
    toku_assert(t_numisnan(n) || t_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
}


static Node *hashint(const Table *t, toku_Integer i) {
    toku_Unsigned ui = t_castS2U(i);
    if (ui <= cast_uint(TOKU_MAXINT))
        return hashpow2(t, cast_int(ui));
    else
        return hashpow2(t, ui);
}


static inline void inithash(Node *n, int limit) {
    toku_assert(!(limit & (limit - 1)) && limit >= 4);
    for (int i = 0; i < limit; i += 4) { /* unroll */
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
    }
}


/* allocate hash array */
static void newhasharray(toku_State *cr, Table *t, t_uint size) {
    int nbits;
    if (size < MINHSIZE)
        size = MINHSIZE;
    nbits = tokuO_ceillog2(size);
    if (t_unlikely(nbits > MAXHBITS || (1u << nbits) > MAXHSIZE))
        tokuD_runerror(cr, "hashtable overflow");
    size = twoto(nbits);
    t->node = tokuM_newarray(cr, size, Node);
    inithash(htnode(t, 0), size);
    t->size = nbits;
    t->lastfree = htnode(t, size);
}


t_sinline void htpreinit(Table *t) {
    t->size = 0;
    t->node = t->lastfree = NULL;
    t->gclist = NULL;
}


/* create new table of specific size */
Table *tokuH_newsz(toku_State *T, int size) {
    GCObject *o = tokuG_new(T, sizeof(Table), TOKU_VTABLE);
    Table *t = gco2ht(o);
    htpreinit(t);
    settval2s(T, T->sp.p++, t); /* assume EXTRA_STACK */
    newhasharray(T, t, size);
    T->sp.p--; /* remove t */
    return t;
}


/* create new hashtable with minimum size */
Table *tokuH_new(toku_State *T) {
    return tokuH_newsz(T, MINHSIZE);
}


static inline void freehash(toku_State *T, Table *t) {
    tokuM_freearray(T, t->node, htsize(t));
}


void tokuH_free(toku_State *T, Table *t) {
    freehash(T, t);
    tokuM_free(T, t);
}


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
            return hashpow2(t, hashflt(n));
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


/* get next free hash array position or NULL */
static Node *getfreepos(Table *t) {
    while (t->lastfree > t->node) {
        t->lastfree--;
        if (keyisnil(t->lastfree))
            return t->lastfree;
    }
    return NULL;
}


static void exchangehashes(Table *ht1, Table *ht2) {
    Node *node = ht1->node;
    Node *lastfree = ht1->lastfree;
    t_ubyte sz = ht1->size;
    ht1->size = ht2->size;
    ht1->node = ht2->node;
    ht1->lastfree = ht2->lastfree;
    ht2->size = sz;
    ht2->node = node;
    ht2->lastfree = lastfree;
}


/* 
** Insert all the elements from 'src' hashtable to 'dest'
** hashtable.
*/
static void insertfrom(toku_State *T, Table *src, Table *dest) {
    int size = htsize(src);
    for (int i = 0; i < size; i++) {
        Node *oldn = htnode(src, i);
        if (!isempty(nodeval(oldn))) {
            TValue key;
            getnodekey(T, &key, oldn);
            tokuH_set(T, dest, &key, nodeval(oldn));
        }
    }
}


/* resize hashtable to new size */
void tokuH_resize(toku_State *T, Table *t, t_uint newsize) {
    Table newht;
    if (t_unlikely(newsize < MINHSIZE))
        newsize = MINHSIZE;
    newhasharray(T, &newht, newsize);
    exchangehashes(t, &newht);
    insertfrom(T, &newht, t);
    freehash(T, &newht);
}


/* rehash hashtable keys */
static void rehash(toku_State *T, Table *t) {
    int arrsize = htsize(t);
    int usednodes = 0;
    for (int i = 0; i < arrsize; i++) {
        const Node *n = htnode(t, i);
        usednodes += !isempty(nodeval(n));
    }
    usednodes++; /* for one extra key */
    tokuH_resize(T, t, usednodes);
}


/*
** WARNING: when using this function the caller probably needs to
** check a GC barrier.
*/
void tokuH_newkey(toku_State *T, Table *t, const TValue *key, const TValue *val) {
    Node *mp;
    TValue aux;
    if (t_unlikely(ttisnil(key))) {
        tokuD_runerror(T, "index is nil");
    } else if (ttisflt(key)) {
        toku_Number f = fval(key);
        toku_Integer k;
        if (tokuO_n2i(f, &k, N2IEQ)) { /* does key fit in an integer? */
            setival(&aux, k);
            key = &aux; /* insert it as an integer */
        }
        else if (t_unlikely(t_numisnan(f))) /* float is NaN? */
            tokuD_runerror(T, "index is NaN");
        /* else */
    } /* fall through */
    if (ttisnil(val))
        return;  /* do not insert nil values */
    mp = mainposition(t, key); /* get main position for 'key' */
    if (!isempty(nodeval(mp))) { /* mainposition already taken ? */
        Node *othern;
        Node *f = getfreepos(t); /* get next free position */
        if (f == NULL) { /* no free position ? */
            rehash(T, t); /* grow table */
            tokuH_set(T, t, key, val); /* insert key */
            return; /* done, key must be a new key */
        }
        othern = mainposfromnode(t, mp);
        if (othern != mp) { /* is colliding node out of its main position? */
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
        }
        else { /* colliding node is in its own main position */
            /* new node will go into free position */
            if (nodenext(mp) != 0)
                nodenext(f) = mp + nodenext(mp) - f; /* chain new position */
            else toku_assert(nodenext(f) == 0);
            nodenext(mp) = f - mp;
            mp = f;
        }
    }
    setnodekey(T, mp, key); /* set key */
    tokuG_barrierback(T, obj2gco(t), key); /* set 't' as gray */
    toku_assert(isempty(nodeval(mp))); /* value slot must be empty */
    setobj(T, nodeval(mp), val); /* set value */
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
** Returns the index of a 'key' for table traversals.
** The beginning of a traversal is signaled by 0.
*/
static t_uint getindex(toku_State *T, Table *t, const TValue *k) {
    const TValue *slot;
    if (ttisnil(k)) return 0; /* first iteration */
    slot = getgeneric(t, k, 1);
    if (t_unlikely(isabstkey(slot))) {
        toku_assert(0);
        tokuD_runerror(T, "invalid key passed to 'nextfield'"); /* not found */
    }
    t_uint i = cast(Node *, slot) - htnode(t, 0); /* key index in hash table */
    return i + 1; /* return next slot index */
}


int tokuH_next(toku_State *T, Table *t, SPtr key) {
    t_uint i = getindex(T, t, s2v(key));
    for (; cast_int(i) < htsize(t); i++) {
        Node *slot = htnode(t, i);
        if (!isempty(nodeval(slot))) {
            getnodekey(T, s2v(key), slot);
            setobj2s(T, key + 1, nodeval(slot));
            return 1;
        }
    }
    return 0;
}


/* insert all the key-value pairs from src into dest */
void tokuH_copykeys(toku_State *T, Table *dest, Table *src) {
    TValue k;
    int sz = htsize(src);
    for (int i = 0; i < sz; i++) {
        Node *n = htnode(src, i);
        if (!isempty(nodeval(n))) {
            getnodekey(T, &k, n);
            tokuH_set(T, dest, &k, nodeval(n));
            tokuG_barrierback(T, obj2gco(dest), nodeval(n));
        }
    }
}


const TValue *tokuH_getshortstr(Table *t, OString *key) {
    Node *n = hashstr(t, key);
    for (;;) {
        if (keyisshrstr(n) && eqshrstr(key, keystrval(n)))
            return nodeval(n);
        else {
            int next = nodenext(n);
            if (next == 0)
                return &absentkey;
            n += next;
        }
    }
}


const TValue *tokuH_getstr(Table *t, OString *key) {
    if (key->tt_ == TOKU_VSHRSTR)
        return tokuH_getshortstr(t, key);
    else {
        TValue k;
        setstrval(cast(toku_State *, NULL), &k, key);
        return getgeneric(t, &k, 0);
    }
}


const TValue *tokuH_getint(Table *t, toku_Integer key) {
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


const TValue *tokuH_get(Table *t, const TValue *key) {
    switch (ttypetag(key)) {
        case TOKU_VSHRSTR: return tokuH_getstr(t, strval(key));
        case TOKU_VNUMINT: return tokuH_getint(t, ival(key));
        case TOKU_VNIL: return &absentkey;
        case TOKU_VNUMFLT: {
            toku_Integer i;
            if (tokuO_tointeger(key, &i, N2IEQ))
                return tokuH_getint(t, i);
        } /* else fall through */
        default:  {
            toku_assert(!ttisnil(key));
            return getgeneric(t, key, 0);
        }
    }
}


/*
** WARNING: when using this function the caller probably needs to
** check a GC barrier.
*/
void tokuH_finishset(toku_State *T, Table *t, const TValue *slot,
                                const TValue *key, const TValue *val) {
    if (isabstkey(slot))
        tokuH_newkey(T, t, key, val);
    else
        setobj(T, cast(TValue *, slot), val);
}


/*
** Ditto for a GC barrier.
*/
void tokuH_set(toku_State *T, Table *t, const TValue *key, const TValue *val) {
    const TValue *slot = tokuH_get(t, key);
    tokuH_finishset(T, t, slot, key, val);
}


/*
** Ditto for a GC barrier.
*/
void tokuH_setstr(toku_State *T, Table *t, OString *key, const TValue *val) {
    const TValue *slot = tokuH_getstr(t, key);
    if (isabstkey(slot)) {
        TValue k;
        setstrval(T, &k, key);
        tokuH_newkey(T, t, &k, val);
    } else
        setobj(T, cast(TValue *, slot), val);
}


/*
** Ditto for a GC barrier.
*/
void tokuH_setint(toku_State *T, Table *t, toku_Integer key, const TValue *val) {
    const TValue *slot = tokuH_getint(t, key);
    if (isabstkey(slot)) {
        TValue k;
        setival(&k, key);
        tokuH_newkey(T, t, &k, val);
    } else
        setobj(T, cast(TValue *, slot), val);
}


/* length of a table is the number of key-value pairs */
int tokuH_len(const Table *t) {
    int len = 0;
    Node *n = htnode(t, 0);
    toku_assert(!(htsize(t)&(htsize(t)-1)) && htsize(t) >= 4);
    while (n != htnodelast(t)) {
        len += !isempty(nodeval(n)); n++;
        len += !isempty(nodeval(n)); n++;
        len += !isempty(nodeval(n)); n++;
        len += !isempty(nodeval(n)); n++;
    }
    return len;
}
