/*
** chashtable.c
** Hash Table
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include <string.h>
#include <math.h>

#include "cstring.h"
#include "chashtable.h"
#include "csconf.h"
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


/* maximum size for the hash array */
#define MAXHSIZE        (1u << MAXHBITS)


/* smallest size for the hash array */
#define MINHSIZE        twoto(CSI_MINHTBITS)


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
    n = cs_mathop(frexp(n, &exp)) * -cast_num(INT_MIN);
    if (c_likely(cs_number2integer(n, &ni))) {
        uint ui = cast_uint(exp) + cast_uint(ni);
        return (ui <= cast_uint(INT_MAX) ? ui : cast_uint(~ui));
    }
    cs_assert(csi_numisnan(n) || cs_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
}


static Node *hashint(const HTable *ht, cs_Integer i) {
    cs_Unsigned ui = csi_castS2U(i);
    if (ui <= cast_uint(INT_MAX))
        return hashpow2(ht, cast_int(ui));
    else
        return hashpow2(ht, ui);
}


static inline void inithash(Node *n, int limit) {
    cs_assert(!(limit & (limit - 1)) && limit >= 4);
    for (int i = 0; i < limit; i += 4) {
        TValue *aux = nodeval(n);
        nodenext(n) = 0; setnilkey(n); setemptyval(aux); n++; aux = nodeval(n);
        nodenext(n) = 0; setnilkey(n); setemptyval(aux); n++; aux = nodeval(n);
        nodenext(n) = 0; setnilkey(n); setemptyval(aux); n++; aux = nodeval(n);
        nodenext(n) = 0; setnilkey(n); setemptyval(aux); n++;
    }
}


/* allocate hash array */
static void newhasharray(cs_State *cr, HTable *ht, uint size) {
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


cs_sinline void htpreinit(HTable *ht) {
    ht->size = 0;
    ht->node = ht->lastfree = NULL;
    ht->gclist = NULL;
}


/* create new hashtable */
HTable *csH_new(cs_State *ts) {
    HTable *ht = csG_new(ts, sizeof(*ht), CS_VHTABLE, HTable);
    htpreinit(ht);
    sethtval2s(ts, ts->sp.p++, ht); /* anchor */
    newhasharray(ts, ht, MINHSIZE);
    ts->sp.p--; /* remove ht */
    return ht;
}


/* create new sized hashtable */
HTable *csH_newsize(cs_State *ts, uint size) {
    HTable *ht = csG_new(ts, sizeof(*ht), CS_VHTABLE, HTable);
    htpreinit(ht);
    sethtval2s(ts, ts->sp.p++, ht); /* anchor */
    newhasharray(ts, ht, size);
    ts->sp.p--; /* remove ht */
    return ht;
}


static inline void freehash(cs_State *ts, HTable *ht) {
    csM_freearray(ts, ht->node, htsize(ht), TValue);
}


void csH_free(cs_State *ts, HTable *ht) {
    freehash(ts, ht);
    csM_free(ts, obj2gco(ht));
}


/*
** Find the main position (slot) for key 'k' inside
** the hash array.
*/
static Node *mainposition(const HTable *ht, const TValue *k) {
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


static inline Node *mainposfromnode(HTable *ht, Node *mp) {
    TValue key;
    getnodekey(cast(cs_State *, NULL), &key, mp);
    return mainposition(ht, &key);
}


/* get next free hash array position or NULL */
static Node *getfreepos(HTable *ht) {
    while (ht->lastfree > ht->node) {
        ht->lastfree--;
        if (keyisnil(ht->lastfree))
            return ht->lastfree;
    }
    return NULL;
}


static void exchangehashes(HTable *ht1, HTable *ht2) {
    Node *node = ht1->node;
    Node *lastfree = ht1->lastfree;
    cs_ubyte sz = ht1->size;
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
static void insertfrom(cs_State *ts, HTable *src, HTable *dest) {
    int size = htsize(src);
    for (int i = 0; i < size; i++) {
        Node *oldn = htnode(src, i);
        if (!isempty(nodeval(oldn))) {
            TValue key;
            getnodekey(ts, &key, oldn);
            csH_set(ts, dest, &key, nodeval(oldn));
        }
    }
}


/* resize hashtable to new size */
void csH_resize(cs_State *ts, HTable *ht, uint newsize) {
    HTable newht;
    if (c_unlikely(newsize < MINHSIZE))
        newsize = MINHSIZE;
    newhasharray(ts, &newht, newsize);
    exchangehashes(ht, &newht);
    insertfrom(ts, &newht, ht);
    freehash(ts, &newht);
}


/* rehash hashtable keys */
static void rehash(cs_State *ts, HTable *ht) {
    int arrsize = htsize(ht);
    int usednodes = 0;
    for (int i = 0; i < arrsize; i++) {
        const Node *n = htnode(ht, i);
        usednodes += !isempty(nodeval(n));
    }
    usednodes++; /* for one extra key */
    csH_resize(ts, ht, usednodes);
}


/* insert new key */
void csH_newkey(cs_State *ts, HTable *ht, const TValue *key,
                const TValue *val) {
    Node *mp;
    TValue aux;
    if (c_unlikely(ttisnil(key))) {
        csD_runerror(ts, "index is nil");
    } else if (ttisflt(key)) {
        cs_Number f = fval(key);
        cs_Integer k;
        if (csO_n2i(f, &k, N2IEXACT)) { /* does key fit in an integer? */
            setival(&aux, k);
            key = &aux; /* insert it as an integer */
        }
        else if (c_unlikely(csi_numisnan(f))) { /* float is NaN? */
            csD_runerror(ts, "index is NaN");
        }
    }
    if (ttisnil(val))
        return;  /* do not insert nil values */
    mp = mainposition(ht, key); /* get main position for 'key' */
    if (!isempty(nodeval(mp))) { /* mainposition already taken ? */
        Node *othern;
        Node *f = getfreepos(ht); /* get next free position */
        if (f == NULL) { /* no free position ? */
            rehash(ts, ht); /* grow table */
            csH_set(ts, ht, key, val); /* insert key */
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
    setnodekey(ts, mp, key); /* set key */
    csG_barrierback(ts, obj2gco(ht), key); /* set 'ht' as gray */
    cs_assert(isempty(nodeval(mp))); /* value slot must be empty */
    setobj(ts, nodeval(mp), val); /* set value */
}


/* 
** Raw equality without calling vmt methods.
** 'deadok' means that 'dead' keys  are allowed to be compared with the
** key 'k', meaning even if 'k' and node 'n' key have different variant types they still can be equal
** if node key is dead key (collectable object marked as dead) and key 'k'
** is also a collectable object. In that case they would get compared by
** pointer identity. 'csH_next' function calls this with 'deadok' set in
** order  to traverse the table and find all the valid values inside the
** table, because dead key nodes still have valid 'next' field.
** Note: as of current version CScript does not expose or implement
** weak tables in its API.
*/
static int eqkey(const TValue *k, const Node *n, int deadok) {
    UNUSED(deadok);
    if ((rawtt(k) != keytt(n)) && /* not the same variant? */
            !(deadok && keyisdead(n) && iscollectable(k)))
        return 0;
    switch (ttypetag(k)) {
        case CS_VNIL: case CS_VTRUE: case CS_VFALSE:
            return 1;
        case CS_VNUMINT:
            return (ival(k) == keyival(n));
        case CS_VNUMFLT:
            return csi_numeq(fval(k), keyfval(n));
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


static const TValue *getgeneric(HTable *ht, const TValue *key, int deadok) {
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


/* auxliary function to 'csH_next' */
static uint getindex(cs_State *ts, HTable *ht, const TValue *k) {
    if (ttisnil(k)) return 0;
    const TValue *slot = getgeneric(ht, k, 1);
    if (c_unlikely(isabstkey(slot)))
        csD_runerror(ts, "invalid key passed to 'next'");
    uint i = cast(Node *, slot) - htnode(ht, 0); /* key index in hash table */
    return i + 1; /* return next slot index */
}


/*
 * Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 */
int csH_next(cs_State *ts, HTable *ht, SPtr key) {
    uint i = getindex(ts, ht, s2v(key));
    for (; cast_int(i) < htsize(ht); i++) {
        Node *slot = htnode(ht, i);
        if (!isempty(nodeval(slot))) {
            getnodekey(ts, s2v(key), slot);
            setobj2s(ts, key + 1, nodeval(slot));
            return 1;
        }
    }
    return 0;
}


/* insert all the 'keys' from 'stab' into 'dtab' */
void csH_copykeys(cs_State *ts, HTable *stab, HTable *dtab) {
    TValue k;
    for (int i = 0; i < htsize(stab); i++) {
        Node *n = htnode(stab, i);
        if (!isempty(nodeval(n))) {
            getnodekey(ts, &k, n);
            csH_set(ts, dtab, &k, nodeval(n));
        }
    }
}


const TValue *csH_getshortstr(HTable *ht, OString *key) {
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


const TValue *csH_getstr(HTable *ht, OString *key) {
    if (key->tt_ == CS_VSHRSTR) {
        return csH_getshortstr(ht, key);
    } else {
        TValue k;
        setstrval(cast(cs_State *, NULL), &k, key);
        return getgeneric(ht, &k, 0);
    }
}


const TValue *csH_getint(HTable *ht, cs_Integer key) {
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


const TValue *csH_get(HTable *ht, const TValue *key) {
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


void csH_finishset(cs_State *ts, HTable *ht, const TValue *slot,
                   const TValue *key, const TValue *val) {
    if (isabstkey(slot))
        csH_newkey(ts, ht, key, val);
    else
        setobj(ts, cast(TValue *, slot), val);
}


void csH_set(cs_State *ts, HTable *ht, const TValue *key, const TValue *val) {
    const TValue *slot = csH_get(ht, key);
    csH_finishset(ts, ht, slot, key, val);
}


int csH_len(const HTable *ht) {
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
