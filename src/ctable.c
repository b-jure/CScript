/*
** ctable.c
** Hash Table
** See Copyright Notice in cscript.h
*/


#define CS_CORE

#include "cprefix.h"

#include <string.h>
#include <math.h>

#include "cstring.h"
#include "ctable.h"
#include "cscriptconf.h"
#include "cgc.h"
#include "cscript.h"
#include "climits.h"
#include "cmem.h"
#include "cdebug.h"
#include "cobject.h"
#include "cobject.h"
#include "cstate.h"


/* largest integer such that 2^MAXHBITS fits in 'int' */
#define MAXHBITS        ((int)(sizeof(int) * CHAR_BIT - 1))

/* must be less than MAXHBITS */
#define MINHTBITS       3


/* maximum size for the hash array */
#define MAXHSIZE        (1u << MAXHBITS)

/* smallest size for the hash array */
#define MINHSIZE        twoto(MINHTBITS)


/* get hashtable 'node' slot from hash 'h' */
#define hashpow2(ht,h)      htnode(ht, hashmod(h, htsize(ht)))


#define hashstr(ht,s)       hashpow2(ht, (s)->hash)
#define hashboolean(ht,b)   hashpow2(ht, b)
#define hashpointer(ht,p)   hashpow2(ht, pointer2uint(p))



/* empty key constant */
static const TValue absentkey = {ABSTKEYCONSTANT};


static uint hashflt(cs_Number n) {
    cs_Integer ni;
    int exp;
    n = c_mathop(frexp(n, &exp)) * -cast_num(INT_MIN);
    if (c_likely(cs_number2integer(n, &ni))) {
        uint ui = cast_uint(exp) + cast_uint(ni);
        return (ui <= cast_uint(MAXINT) ? ui : cast_uint(~ui));
    }
    /* nan or -inf/+inf */
    cs_assert(c_numisnan(n) || c_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
}


static Node *hashint(const Table *ht, cs_Integer i) {
    cs_Unsigned ui = c_castS2U(i);
    if (ui <= cast_uint(MAXINT))
        return hashpow2(ht, cast_int(ui));
    else
        return hashpow2(ht, ui);
}


static inline void inithash(Node *n, int limit) {
    cs_assert(!(limit & (limit - 1)) && limit >= 4);
    for (int i = 0; i < limit; i += 4) { /* unroll */
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
        nodenext(n) = 0; setnilkey(n); setemptyval(nodeval(n)); n++;
    }
}


/* allocate hash array */
static void newhasharray(cs_State *cr, Table *ht, uint size) {
    int nbits;
    if (size < MINHSIZE)
        size = MINHSIZE;
    nbits = csO_ceillog2(size);
    if (c_unlikely(nbits > MAXHBITS || (1u << nbits) > MAXHSIZE))
        csD_runerror(cr, "hashtable overflow");
    size = twoto(nbits);
    ht->node = csM_newarray(cr, size, Node);
    inithash(htnode(ht, 0), size);
    ht->size = nbits;
    ht->lastfree = htnode(ht, size);
}


c_sinline void htpreinit(Table *ht) {
    ht->size = 0;
    ht->node = ht->lastfree = NULL;
    ht->gclist = NULL;
}


/* create new table of specific size */
Table *csH_newsz(cs_State *C, int size) {
    GCObject *o = csG_new(C, sizeof(Table), CS_VTABLE);
    Table *ht = gco2ht(o);
    htpreinit(ht);
    settval2s(C, C->sp.p++, ht); /* assume EXTRA_STACK */
    newhasharray(C, ht, size);
    C->sp.p--; /* remove ht */
    return ht;
}


/* create new hashtable with minimum size */
Table *csH_new(cs_State *C) {
    return csH_newsz(C, MINHSIZE);
}


static inline void freehash(cs_State *C, Table *ht) {
    csM_freearray(C, ht->node, htsize(ht));
}


void csH_free(cs_State *C, Table *ht) {
    freehash(C, ht);
    csM_free(C, ht);
}


/*
** Find the main position (slot) for key 'k' inside
** the hash array.
*/
static Node *mainposition(const Table *ht, const TValue *k) {
    switch (ttypetag(k)) {
        case CS_VTRUE: return hashboolean(ht, 1);
        case CS_VFALSE: return hashboolean(ht, 0);
        case CS_VSHRSTR: {
            OString *s = strval(k);
            return hashstr(ht, s);
        }
        case CS_VLNGSTR: {
            OString *s = strval(k);
            return hashpow2(ht, csS_hashlngstr(s));
        }
        case CS_VNUMINT: {
            cs_Integer i = ival(k);
            return hashint(ht, i);
        }
        case CS_VNUMFLT: {
            cs_Number n = fval(k);
            return hashpow2(ht, hashflt(n));
        }
        case CS_VLIGHTUSERDATA: {
            void *p = pval(k);
            return hashpointer(ht, p);
        }
        case CS_VLCF: {
            cs_CFunction lcf = lcfval(k);
            return hashpointer(ht, lcf);
        }
        default: {
            GCObject *o = gcoval(k);
            return hashpointer(ht, o);
        }
    }
}


static inline Node *mainposfromnode(Table *ht, Node *mp) {
    TValue key;
    getnodekey(cast(cs_State *, NULL), &key, mp);
    return mainposition(ht, &key);
}


/* get next free hash array position or NULL */
static Node *getfreepos(Table *ht) {
    while (ht->lastfree > ht->node) {
        ht->lastfree--;
        if (keyisnil(ht->lastfree))
            return ht->lastfree;
    }
    return NULL;
}


static void exchangehashes(Table *ht1, Table *ht2) {
    Node *node = ht1->node;
    Node *lastfree = ht1->lastfree;
    c_byte sz = ht1->size;
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
static void insertfrom(cs_State *C, Table *src, Table *dest) {
    int size = htsize(src);
    for (int i = 0; i < size; i++) {
        Node *oldn = htnode(src, i);
        if (!isempty(nodeval(oldn))) {
            TValue key;
            getnodekey(C, &key, oldn);
            csH_set(C, dest, &key, nodeval(oldn));
        }
    }
}


/* resize hashtable to new size */
void csH_resize(cs_State *C, Table *ht, uint newsize) {
    Table newht;
    if (c_unlikely(newsize < MINHSIZE))
        newsize = MINHSIZE;
    newhasharray(C, &newht, newsize);
    exchangehashes(ht, &newht);
    insertfrom(C, &newht, ht);
    freehash(C, &newht);
}


/* rehash hashtable keys */
static void rehash(cs_State *C, Table *ht) {
    int arrsize = htsize(ht);
    int usednodes = 0;
    for (int i = 0; i < arrsize; i++) {
        const Node *n = htnode(ht, i);
        usednodes += !isempty(nodeval(n));
    }
    usednodes++; /* for one extra key */
    csH_resize(C, ht, usednodes);
}


/*
** Warning: when using this function the caller probably needs to
** check a GC barrier.
*/
void csH_newkey(cs_State *C, Table *ht, const TValue *key,
                const TValue *val) {
    Node *mp;
    TValue aux;
    if (c_unlikely(ttisnil(key))) {
        csD_runerror(C, "index is nil");
    } else if (ttisflt(key)) {
        cs_Number f = fval(key);
        cs_Integer k;
        if (csO_n2i(f, &k, N2IEXACT)) { /* does key fit in an integer? */
            setival(&aux, k);
            key = &aux; /* insert it as an integer */
        }
        else if (c_unlikely(c_numisnan(f))) /* float is NaN? */
            csD_runerror(C, "index is NaN");
        /* else */
    } /* fall through */
    if (ttisnil(val))
        return;  /* do not insert nil values */
    mp = mainposition(ht, key); /* get main position for 'key' */
    if (!isempty(nodeval(mp))) { /* mainposition already taken ? */
        Node *othern;
        Node *f = getfreepos(ht); /* get next free position */
        if (f == NULL) { /* no free position ? */
            rehash(C, ht); /* grow table */
            csH_set(C, ht, key, val); /* insert key */
            return; /* done, key must be a new key */
        }
        othern = mainposfromnode(ht, mp);
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
            else cs_assert(nodenext(f) == 0);
            nodenext(mp) = f - mp;
            mp = f;
        }
    }
    setnodekey(C, mp, key); /* set key */
    csG_barrierback(C, obj2gco(ht), key); /* set 'ht' as gray */
    cs_assert(isempty(nodeval(mp))); /* value slot must be empty */
    setobj(C, nodeval(mp), val); /* set value */
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
        case CS_VNIL: case CS_VTRUE: case CS_VFALSE:
            return 1;
        case CS_VNUMINT:
            return (ival(k) == keyival(n));
        case CS_VNUMFLT:
            return c_numeq(fval(k), keyfval(n));
        case CS_VLIGHTUSERDATA:
            return (pval(k) == keypval(n));
        case CS_VLCF:
            return (lcfval(k) == keycfval(n));
        case CS_VSHRSTR:
            return eqshrstr(strval(k), keystrval(n));
        case CS_VLNGSTR:
            return csS_eqlngstr(strval(k), keystrval(n));
        default: /* rest of the objects are compared by pointer identity */
            cs_assert(iscollectable(k));
            return (gcoval(k) == keygcoval(n));
        }
    return 0;
}


static const TValue *getgeneric(Table *ht, const TValue *key, int deadok) {
    Node *n = mainposition(ht, key);
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
static uint getindex(cs_State *C, Table *ht, const TValue *k) {
    const TValue *slot;
    if (ttisnil(k)) return 0; /* first iteration */
    slot = getgeneric(ht, k, 1);
    if (c_unlikely(isabstkey(slot)))
        csD_runerror(C, "invalid key passed to 'next'"); /* key not found */
    uint i = cast(Node *, slot) - htnode(ht, 0); /* key index in hash table */
    return i + 1; /* return next slot index */
}


int csH_next(cs_State *C, Table *ht, SPtr key) {
    uint i = getindex(C, ht, s2v(key));
    for (; cast_int(i) < htsize(ht); i++) {
        Node *slot = htnode(ht, i);
        if (!isempty(nodeval(slot))) {
            getnodekey(C, s2v(key), slot);
            setobj2s(C, key + 1, nodeval(slot));
            return 1;
        }
    }
    return 0;
}


/* insert all the key-value pairs from src into dest */
void csH_copykeys(cs_State *C, Table *dest, Table *src) {
    TValue k;
    int sz = htsize(src);
    for (int i = 0; i < sz; i++) {
        Node *n = htnode(src, i);
        if (!isempty(nodeval(n))) {
            getnodekey(C, &k, n);
            csH_set(C, dest, &k, nodeval(n));
            csG_barrierback(C, obj2gco(dest), nodeval(n));
        }
    }
}


const TValue *csH_getshortstr(Table *ht, OString *key) {
    Node *n = hashstr(ht, key);
    for (;;) {
        if (keyisshrstr(n) && eqshrstr(key, keystrval(n))) {
            return nodeval(n);
        } else {
            int next = nodenext(n);
            if (next == 0)
                return &absentkey;
            n += next;
        }
    }
}


const TValue *csH_getstr(Table *ht, OString *key) {
    if (key->tt_ == CS_VSHRSTR) {
        return csH_getshortstr(ht, key);
    } else {
        TValue k;
        setstrval(cast(cs_State *, NULL), &k, key);
        return getgeneric(ht, &k, 0);
    }
}


const TValue *csH_getint(Table *ht, cs_Integer key) {
    Node *n = hashint(ht, key);
    for (;;) {
        if (keyisint(n) && keyival(n) == key) {
            return nodeval(n);
        } else {
            int next = nodenext(n);
            if (next == 0)
                return &absentkey;
            n += next;
        }
    }
}


const TValue *csH_get(Table *ht, const TValue *key) {
    switch (ttypetag(key)) {
        case CS_VSHRSTR: return csH_getstr(ht, strval(key));
        case CS_VNUMINT: return csH_getint(ht, ival(key));
        case CS_VNIL: return &absentkey;
        case CS_VNUMFLT: {
            cs_Integer i;
            if (csO_tointeger(key, &i, N2IEXACT))
                return csH_getint(ht, i);
        } /* else fall through */
        default:  {
            cs_assert(!ttisnil(key));
            return getgeneric(ht, key, 0);
        }
    }
}


/*
** Warning: when using this function the caller probably needs to
** check a GC barrier.
*/
void csH_finishset(cs_State *C, Table *ht, const TValue *slot,
                   const TValue *key, const TValue *val) {
    if (isabstkey(slot))
        csH_newkey(C, ht, key, val);
    else
        setobj(C, cast(TValue *, slot), val);
}


/*
** Warning: when using this function the caller probably needs to
** check a GC barrier.
*/
void csH_set(cs_State *C, Table *ht, const TValue *key, const TValue *val) {
    const TValue *slot = csH_get(ht, key);
    csH_finishset(C, ht, slot, key, val);
}


/* length of a table is the number of key-value pairs */
int csH_len(const Table *ht) {
    int len = 0;
    Node *n = htnode(ht, 0);
    cs_assert(!(htsize(ht)&(htsize(ht)-1)) && htsize(ht) >= 4);
    while (n != htnodelast(ht)) {
        len += !isempty(nodeval(n)); n++;
        len += !isempty(nodeval(n)); n++;
        len += !isempty(nodeval(n)); n++;
        len += !isempty(nodeval(n)); n++;
    }
    return len;
}
