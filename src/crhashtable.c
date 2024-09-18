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

#include "crhashtable.h"
#include "crconf.h"
#include "crgc.h"
#include "crmem.h"
#include "crdebug.h"
#include "crobject.h"
#include "crobject.h"
#include "crstate.h"


/* set/check tombstone node */
#define istomb(n)	(keytt(n) == CR_VTOMB)
#define puttomb(n)	(keytt(n) = CR_VTOMB)


/* get table load factor */
#define loadfactor(ts,t) \
    cri_numdiv((ts), cast_num((t)->nnodes), cast_num(htsize(t)))


/* slots left until table needs to grow */
#define slotsleft(ts,t) \
    cast_int((cast_num(CRI_MAXHTABLOAD) - loadfactor((ts),(t))) * htsize(t))



/* get hash slot */
#define hashslot(b,h,s)	    (&b[(h) & ((s) - 1)])



/* empty node constant */
static const Node emptynode = {{{0},CR_VEMPTY,0,CR_VEMPTY,{0}}};


/* empty key constant */
static const TValue emptykey = {{0},CR_VEMPTY,0};



/* initialize hash table */
HTable *crH_new(cr_State *ts) {
    HTable *ht = crG_new(ts, sizeof(HTable), CR_VHTABLE, HTable);
    ht->size = 0;
    ht->isweak = 0;
    ht->left = 0;
    ht->nnodes = 0;
    ht->mem = NULL;
    ht->gclist = NULL;
    return ht;
}


/* create string hash table */
void crH_newstab(cr_State *ts, HTable *ht) {
    ht->size = crO_ceillog2(CRI_MINSTRHTABSIZE);
    ht->nnodes = 0;
    ht->left = slotsleft(ts, ht);
    ht->mem = crM_newarray(ts, CRI_MINSTRHTABSIZE, Node);
}


/*
 * Find the main position (slot) for key 'k' inside
 * the table array.
 */
static Node *mainposition(const Node *mem, int size, const TValue *k) {
    switch (ttypetag(k)) {
    case CR_VTRUE: return cast_node(hashslot(mem, crO_hashbool(1), size));
    case CR_VFALSE: return cast_node(hashslot(mem, crO_hashbool(0), size));
    case CR_VNUMINT:
        return cast_node(hashslot(mem, crO_hashint(ival(k)), size));
    case CR_VNUMFLT:
        return cast_node(hashslot(mem, crO_hashnum(fval(k)), size));
    case CR_VLUDATA: {
        void *p = pval(k);
        return cast_node(hashslot(mem, crO_hashp(p), size));
    }
    case CR_VCFUNCTION: {
        cr_CFunction f = cfval(k);
        return cast_node(hashslot(mem, crO_hashp(f), size));
    }
    case CR_VSTRING: {
        OString *str = strval(k);
        cr_assert(hashash(str));
        return cast_node(hashslot(mem, str->hash, size));
    }
    default:
        cr_assert(!ttisnil(k) && ttiso(k));
        return cast_node(hashslot(mem, crO_hashp(gcoval(k)), size));
    }
}


/* raw equality without calling vtable methods */
static int eqkey(const TValue *k, const Node *n) {
    cr_assert(!ttisempty(k));
    if (ttypetag(k) != keytt(n))
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
        cr_assert(ttypetag(k) == CR_TOBJECT);
        return (gcoval(k) == keygcoval(n));
    }
    return 0;
}


/*
 * Find slot by linear probing.
 * Size of the table 'mem' array is always power of 2.
 * In case of finding a 'tomb' node keep probing until the
 * same 'key' was found or empty spot.
 * If the empty slot was found and there was a 'tomb', then
 * return the tomb, otherwise return the entry containing the
 * same key. 'slot' won't overflow because load factor is tracked.
 */
cr_sinline Node *gethtnode(const Node *mem, int size, const TValue *k) {
    Node *tomb = NULL;
    Node *slot = mainposition(mem, size, k);
    for (;;) { /* linear probing */
        if (keyisempty(slot)) {
            if (!istomb(slot))
                return (tomb ? tomb : slot);
            if (!tomb)
                tomb = slot;
        } else if (eqkey(k, slot)) {
            return slot;
        }
        slot++;
    }
}


/* auxliary function to 'crH_next' */
static uint getindex(cr_State *ts, HTable *ht, const TValue *k) {
    Node *slot = gethtnode(ht->mem, htsize(ht), k);
    if (cr_unlikely(keyisempty(slot)))
        crD_runerror(ts, "invalid key passed to 'next'");
    return cast_int(slot - htnode(ht, 0));
}


/*
 * Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 */
int crH_next(cr_State *ts, HTable *ht, SIndex *k) {
    TValue *v = s2v(k->p);
    uint i = getindex(ts, ht, v);
    for (; i < htsize(ht); i++) {
        if (!keyisempty(htnode(ht, i))) {
            Node *slot = htnode(ht, i);
            getnodekey(ts, v, slot);
            setobj(ts, v+1, htnodevalue(slot));
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
            crH_set(ts, dtab, &k, htnodevalue(slot));
        }
    }
}


static void rehash(const Node *omem, int osize, Node *nmem, int nsize) {
    const Node *slot;
    TValue k;
    for (int i = 0; i < osize; i++) {
        slot = omem + i;
        if (keyisempty(slot))
            continue;
        getnodekey(ts, &k, slot);
        Node *dest = gethtnode(nmem, nsize, &k);
        *dest = *slot;
    }
}


/* sets table array slots to 'emptynode' */
cr_sinline void auxsetempty(Node * restrict mem, uint size) {
    cr_assert(ispow2(size) && size >= 4);
    Node *slot = mem;
    for (uint i = 0; i < size; i+=4) { /* unroll */
        *slot++ = emptynode;
        *slot++ = emptynode;
        *slot++ = emptynode;
        *slot++ = emptynode;
    }
}


/* expand hash table array */
static void expandmem(cr_State *ts, HTable *ht) {
    int osize = twoto(ht->size++);
    uint nsize = twoto(ht->size);
    if (nsize < CRI_MINHTABSIZE) {
        int temp = (CRI_MINHTABSIZE >> 1);
        while (temp >> 1)
            ht->size++;
        nsize = CRI_MINHTABSIZE;
    }
    if (cr_unlikely(nsize >= CRI_MAXHTABSIZE))
        crD_runerror(ts, "hashtable overflow");
    Node *newmem = crM_newarray(ts, nsize, Node);
    auxsetempty(newmem, nsize);
    rehash(ht->mem, osize, newmem, cast_int(nsize));
    if (ht->mem != NULL)
        crM_freearray(ts, ht->mem, osize);
    ht->mem = newmem;
    ht->size = nsize;
    ht->left = slotsleft(ts, ht);
}


/*
 * Set value for the given key.
 * If the 'key' was not found insert it together with the 'value'.
 * If the 'key' already exists set its 'value'.
 */
int crH_set(cr_State *ts, HTable *ht, const TValue *key, const TValue *val) {
    Node *slot = gethtnode(ht->mem, htsize(ht), key);
    int newk = keyisempty(slot);
    if (newk) { /* new key */
        if (!istomb(slot)) ht->left--;
        if (cr_unlikely(ht->left <= 0)) {
            expandmem(ts, ht);
            crH_set(ts, ht, key, val);
            return 1;
        }
        ht->nnodes++;
    }
    setnodekey(ts, slot, key);
    *htnodevalue(slot) = *val;
    return newk;
}


/*
 * Removes the slot belonging to the table 'ht' directly
 * without probing for it.
 */
void crH_removedirect(HTable *ht, Node *slot) {
    cr_assert(!istomb(n) && !keyisempty(n));
    cr_assert(htfirstnode(ht) <= slot && slot <= htlastnode(ht));
    puttomb(slot);
    ht->nnodes--;
}


/*
 * Remove 'key' from the table.
 * If the 'key' was found (and removed) return non-zero and place the tombstone.
 * Tombstones count as entries so this will not decrement 'left'.
 */
int crH_remove(HTable *ht, const TValue *key) {
    Node *slot = gethtnode(ht->mem, htsize(ht), key);
    if (keyisempty(slot))
        return 0;
    puttomb(slot);
    ht->nnodes--;
    return 1;
}


/* try to get interned string */
OString *crH_getstring(HTable *ht, const char *str, size_t len,
                       uint hash)
{
    if (ht->nnodes == 0)
        return NULL;
    int size = htsize(ht);
    for (Node *slot = hashslot(ht->mem, hash, size);; slot++) {
        if (keyisempty(slot)) {
            if (!istomb(slot)) 
                return NULL;
        } else {
            OString *s = keystrval(slot);
            if (s->hash == hash && s->len == len  /* if same hash, length */
                    && memcmp(s->bytes, str, len) == 0) { /* and contents */
                return s;
            }
        }
    };
}


/* 
** Get pointer to the value of key string; should be used to extract
** the underlying object and take the pointer to object instead,
** otherwise pointer to the 'TValue' might get invalidated after
** mutation of the 'ht'.
*/
const TValue *crH_getp(HTable *ht, OString *str) {
    cr_assert(str != NULL && shashash(str));
    Node *tomb = NULL;
    Node *slot = hashslot(ht->mem, str->hash, htsize(ht));
    for (;;) {
        if (keyisempty(slot)) {
            if (!istomb(slot))
                return (tomb ? &emptykey : htnodevalue(slot));
            if (!tomb)
                tomb = slot;
        } else if (str == keystrval(slot)) {
            return htnodevalue(slot);
        }
        slot++;
    }
}


/* get key value */
int crH_get(HTable *ht, const TValue *key, TValue *o) {
    cr_assert(o != NULL);
    if (ht->nnodes == 0) 
        return 0;
    Node *slot = gethtnode(ht->mem, htsize(ht), key);
    if (keyisempty(slot)) 
        return 0;
    setobj(cast(cr_State*, NULL), o, htnodevalue(slot));
    return 1;
}


void crH_free(cr_State *ts, HTable *ht) {
    if (ht->mem != NULL)
        crM_free(ts, ht->mem, twoto(ht->size) * sizeof(Node));
    crM_free(ts, ht, sizeof(HTable));
}
