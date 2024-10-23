/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include <string.h>

#include "chashtable.h"
#include "cconf.h"
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
#define MINHSIZE        8


/* module operation for hashing, 'sz' is always power of 2 */
#define hashmod(h,sz)       ((h) & (((sz) - 1)))


/* get dictionary 'node' slot from hash 'h' */
#define hashslot(ht,h)      htnode(ht, hashmod(h, htsize(ht)))


#define hashstr(ht,s)       hashslot(ht, (s)->hash)



/* empty key constant */
static const TValue absentkey = {ABSTKEYCONSTANT};



/* allocate hash array */
static void newhasharray(cr_State *cr, HTable *ht, uint size) {
    int nbits = crO_ceillog2(size);
    if (cr_unlikely(nbits > MAXHBITS || (1u << nbits) > MAXHSIZE))
        crD_runerror(cr, "hashtable overflow");
    size = twoto(nbits);
    ht->node = crM_newarray(cr, size, Node);
    for (int i = 0; i < (int)size; i++) {
        Node *n = htnode(ht, i);
        nodenext(n) = 0;
        setemptykey(n);
        setemptyval(nodeval(n));
    }
    ht->size = nbits;
    ht->lastfree = htnode(ht, size);
}


cr_sinline void htpreinit(HTable *ht) {
    ht->size = 0;
    ht->isweak = 0;
    ht->node = ht->lastfree = NULL;
    ht->gclist = NULL;
}


/* create new hashtable */
HTable *crH_new(cr_State *ts) {
    HTable *ht = crG_new(ts, sizeof(*ht), CR_VHTABLE, HTable);
    htpreinit(ht);
    newhasharray(ts, ht, MINHSIZE);
    return ht;
}


/* create new sized hashtable */
HTable *crH_newsize(cr_State *ts, uint size) {
    HTable *ht = crG_new(ts, sizeof(*ht), CR_VHTABLE, HTable);
    htpreinit(ht);
    newhasharray(ts, ht, size);
    return ht;
}


static inline void freehash(cr_State *ts, HTable *ht) {
    crM_freearray(ts, ht->node, htsize(ht), TValue);
}


void crH_free(cr_State *ts, HTable *ht) {
    freehash(ts, ht);
    crM_free(ts, obj2gco(ht), sizeof(*ht));
}


/*
 * Find the main position (slot) for key 'k' inside
 * the table array.
 */
static Node *mainposition(const HTable *ht, const TValue *k) {
    switch (ttypetag(k)) {
    case CR_VTRUE: return hashslot(ht, crO_hashbool(1));
    case CR_VFALSE: return hashslot(ht, crO_hashbool(0));
    case CR_VNUMINT:
        return hashslot(ht, crO_hashint(ival(k)));
    case CR_VNUMFLT:
        return hashslot(ht, crO_hashnum(fval(k)));
    case CR_VLUDATA: {
        void *p = pval(k);
        return hashslot(ht, crO_hashp(p));
    }
    case CR_VCFUNCTION: {
        cr_CFunction f = cfval(k);
        return hashslot(ht, crO_hashp(f));
    }
    case CR_VSTRING: {
        OString *str = strval(k);
        return hashstr(ht, str);
    }
    default:
        cr_assert(!ttisnil(k) && iscollectable(k));
        return hashslot(ht, crO_hashp(gcoval(k)));
    }
}


static inline Node *mainposfromnode(HTable *ht, Node *mp) {
    TValue key;
    getnodekey(NULL, &key, mp);
    return mainposition(ht, &key);
}


/* get next free hash array position or NULL */
static Node *freepos(HTable *ht) {
    while (ht->lastfree > ht->node) {
        ht->lastfree--;
        if (keyisempty(ht->lastfree))
            return ht->lastfree;
    }
    return NULL;
}


static void exchangedicts(HTable *d1, HTable *d2) {
    Node *node = d1->node;
    Node *lastfree = d1->lastfree;
    cr_ubyte sz = d1->size;
    d1->size = d2->size;
    d1->node = d2->node;
    d1->lastfree = d2->lastfree;
    d2->size = sz;
    d2->node = node;
    d2->lastfree = lastfree;
}


/* 
** Insert all the elements from 'src' hashtable to 'dest'
** hashtable.
*/
static void insertfrom(cr_State *ts, HTable *src, HTable *dest) {
    int size = htsize(src);
    for (int i = 0; i < size; i++) {
        Node *n = htnode(src, i);
        if (!keyisempty(n)) {
            TValue key;
            getnodekey(ts, &key, n);
            crH_set(ts, dest, &key, nodeval(n));
        }
    }
}


/* resize hashtable to new size */
void crH_resize(cr_State *ts, HTable *ht, int size) {
    HTable newdict;
    newhasharray(ts, &newdict, size);
    exchangedicts(ht, &newdict);
    insertfrom(ts, &newdict, ht);
    freehash(ts, &newdict);
}


/* rehash hashtable keys */
static void rehash(cr_State *ts, HTable *ht) {
    int arrsize = htsize(ht);
    int usednodes = 0;
    for (int i = 0; i < arrsize; i++) {
        const Node *n = htnode(ht, i);
        usednodes += !keyisempty(n);
    }
    usednodes++; /* for one extra key */
    crH_resize(ts, ht, usednodes);
}


/* insert new key */
void crH_newkey(cr_State *ts, HTable *ht, const TValue *key,
                const TValue *val) {
    HTable old;
    Node *mp = mainposition(ht, key); /* get main position for 'key' */
    if (!keyisempty(mp)) { /* mainposition already taken ? */
        Node *f = freepos(ht); /* get next free position */
        if (cr_unlikely(f == NULL)) { /* no free position ? */
            rehash(ts, &old);
            crH_set(ts, ht, key, val);
            return;
        }
        Node *n = mainposfromnode(ht, mp);
        if (n != mp) { /* is colliding node out of its main position? */
            /* yes; move colliding node into free position */
            while (n + nodenext(n) != mp) /* find previous */
                n += nodenext(n);
            nodenext(n) = f - n; /* rechain to point to 'f' */
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
            else cr_assert(nodenext(f) == 0);
            nodenext(mp) = f - mp;
            mp = f;
        }
    }
    setnodekey(ts, mp, key); /* set key */
    crG_barrierback(ts, obj2gco(ht), key); /* set 'ht' as gray */
    cr_assert(ttisempty(nodeval(mp))); /* value slot must be empty */
    setobj(ts, nodeval(mp), val); /* set value */
}


/* 
** Raw equality without calling vmt methods.
** 'deadok' means that 'dead' keys - the keys that are white and
** part of weak table (table with weak references) - are allowed
** to be compared with the key 'k', meaning even if 'k' and node
** 'n' key have different variant types they still can be equal
** if node key is dead key (collectable object marked as dead)
** and key 'k' is also a collectable object.
** In that case they would get compared by pointer identity.
** 'crH_next' function calls this with 'deadok' set in order  to
** traverse the table and find all the valid values inside the table,
** because dead key nodes still have valid 'next' field.
**
** Note: as of version 1.0.0, Cript only has a single weak table
** that is managed internally, so 'deadkey' is irrelevant but might
** become usefull if weak hashtables are exposed in core API.
*/
static int eqkey(const TValue *k, const Node *n, int deadok) {
    if ((ttypetag(k) != keytt(n)) && /* not the same variant */
            !(deadok && keyisdead(n) && iscollectable(k)))
        return 0;
    switch (ttypetag(k)) {
    case CR_VTRUE: case CR_VFALSE:
        return 1;
    case CR_VNUMINT:
        return (ival(k) == keyival(n));
    case CR_VNUMFLT:
        return cri_numeq(fval(k), keyfval(n));
    case CR_VLUDATA:
        return (pval(k) == keypval(n));
    case CR_VCFUNCTION:
        return (cfval(k) == keycfval(n));
    default: /* all equal objects have equal pointers */
        cr_assert(iscollectable(k));
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


/* auxliary function to 'crH_next' */
static uint getindex(cr_State *ts, HTable *ht, const TValue *k) {
    if (ttisnil(k)) return 0;
    const TValue *slot = getgeneric(ht, k, 1);
    if (cr_unlikely(isabstkey(slot)))
        crD_runerror(ts, "invalid key passed to 'next'");
    uint i = cast(Node *, slot) - htnode(ht, 0); /* key index in hash table */
    return i + 1; /* return next slot index */
}


/*
 * Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 */
int crH_next(cr_State *ts, HTable *ht, SPtr key) {
    uint i = getindex(ts, ht, s2v(key));
    for (; cast_int(i) < htsize(ht); i++) {
        Node *slot = htnode(ht, i);
        if (!ttisempty(nodeval(slot))) {
            getnodekey(ts, s2v(key), slot);
            setobj2s(ts, key + 1, nodeval(slot));
            return 1;
        }
    }
    return 0;
}


/* insert all the 'keys' from 'stab' into 'dtab' */
void crH_copykeys(cr_State *ts, HTable *stab, HTable *dtab) {
    TValue k;
    for (int i = 0; i < htsize(stab); i++) {
        Node *slot = htnode(stab, i);
        if (!keyisempty(slot)) {
            getnodekey(ts, &k, slot);
            crH_set(ts, dtab, &k, nodeval(slot));
        }
    }
}


/* get key value */
const TValue *crH_get(HTable *ht, const TValue *key) {
    if (ttisnil(key)) 
        return &absentkey;
    else
        return getgeneric(ht, key, 0);
}


void crH_finishset(cr_State *ts, HTable *ht, const TValue *slot,
                   const TValue *key, const TValue *val) {
    if (isabstkey(slot))
        crH_newkey(ts, ht, key, val);
    else
        setobj(ts, cast(TValue *, slot), val);
}


void crH_set(cr_State *ts, HTable *ht, const TValue *key, const TValue *val) {
    const TValue *slot = crH_get(ht, key);
    crH_finishset(ts, ht, slot, key, val);
}


int crH_len(const HTable *ht) {
    int len = 0;
    Node *n = htnode(ht, 0);
    cr_assert(htsize(ht) % 4 == 0);
    while (n != htnodelast(ht)) {
        len += !keyisempty(n++);
        len += !keyisempty(n++);
        len += !keyisempty(n++);
        len += !keyisempty(n++);
    }
    return len;
}


OString *crH_getinterned(cr_State *ts, HTable *ht, const char *str, size_t len,
                         uint hash) {
    for (Node *n = hashslot(ht, hash); n < htnodelast(ht); n++) {
        if (!ttisempty(nodeval(n))) {
            OString *s = keystrval(n);
            if (s->hash == hash && s->len == len /* if same hash, length */
                    && memcmp(s->bytes, str, len) == 0) { /* and contents */
                if (isdead(&G_(ts)->gc, s)) /* resurrect if dead */
                    changewhite(s);
                return s;
            }
        }
    }
    return NULL;
}
