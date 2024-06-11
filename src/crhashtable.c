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

#include "crvm.h"
#include "crhashtable.h"
#include "crconf.h"
#include "crgc.h"
#include "crmem.h"
#include "crobject.h"
#include "crvalue.h"



/* set/check tombstone node */
#define istomb(n)	(keytt(n) == CR_VTOMB)
#define puttomb(n)	(keytt(n) = CR_VTOMB)


/* get table load factor */
#define loadfactor(vm,t) \
	cri_numdiv((vm), cast_num((t)->nnodes), cast_num(tsize(t)))


/* slots left until table needs to grow */
#define slotsleft(vm,t) \
	cast_int((cast_num(CR_MAXHTABLOAD) - loadfactor((vm),(t))) * tsize(t))



/* get hash slot */
#define hashslot(b,h,s)		(&b[(h)&((s)-1)])


/* empty node constant */
const Node emptynode = {{0,CR_VEMPTY,0,CR_VEMPTY,0}};



/* initialize hash table */
void cr_ht_init(HTable *tab)
{
	tab->size = 0;
	tab->nnodes = 0;
	tab->left = 0;
	tab->mem = NULL;
}


/* create string hash table */
void cr_ht_newstab(VM *vm, HTable *tab)
{
	tab->size = cr_ve_ceillog2(CR_MINSTRHTABSIZE);
	tab->nnodes = 0;
	tab->left = slotsleft(vm, tab);
	tab->mem = cr_mm_newarray(vm, CR_MINSTRHTABSIZE, Node);
}


/* 
 * Find the main position (slot) for key 'k' inside
 * the table array.
 */
static Node *mainposition(const Node *mem, int size, const TValue *k)
{
	void *p;
	cr_cfunc f;
	OString *str;

	switch (vtt(k)) {
		case CR_VTRUE:
			return cast_node(hashslot(mem, cr_hh_boolean(1), size));
		case CR_VFALSE:
			return cast_node(hashslot(mem, cr_hh_boolean(0), size));
		case CR_VNUMINT:
			return cast_node(hashslot(mem, cr_hh_integer(ivalue(k)), size));
		case CR_VNUMFLT:
			return cast_node(hashslot(mem, cr_hh_number(fvalue(k)), size));
		case CR_VLUDATA:
			p = pvalue(k);
			return cast_node(hashslot(mem, cr_hh_pointer(p), size));
		case CR_VCFUNCTION:
			f = cfvalue(k);
			return cast_node(hashslot(mem, cr_hh_pointer(p), size));
		case CR_VSTRING:
			str = strvalue(k);
			/* v1.0.0: all strings are interned so this
			 * check doesn't make any sense, meaning all
			 * strings require the has the moment they
			 * are created, so 'hashash' is always non zero. */
#if CR_VERSION_NUMBER != 100
			if (str->hashash == 0) {
				str->hash = cr_hh_string(str->bytes, str->len, str->hash);
				str->hashash = 1;
			}
#endif
			return cast_node(hashslot(mem, str->hash, size));
		default:
			cr_assert(!ttisnil(k) && ttiso(k));
			return cast_node(hashslot(mem, cr_hh_pointer(ovalue(k)), size));
	}
}


/* raw equality without calling vtable methods */
static int eqkey(const TValue *k, const Node *n)
{
	cr_assert(!ttisempty(k));
	if (vtt(k) != keytt(n))
		return 0;
	switch (vtt(k)) {
		case CR_VTRUE: case CR_VFALSE:
			return 1;
		case CR_VNUMINT:
			return (ivalue(k) == keyivalue(n));
		case CR_VNUMFLT:
			return cri_numeq(fvalue(k), keyfvalue(n));
		case CR_VLUDATA:
			return (pvalue(k) == keypvalue(n));
		case CR_VCFUNCTION:
			return (cfvalue(k) == keycfvalue(n));
		case CR_VSTRING:
			return cr_ot_eqstring(strvalue(k), keystrvalue(n));
		default:
			return (ovalue(k) == keyovalue(n));
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
cr_sinline Node *getslot(const Node *mem, int size, const TValue *k)
{
	Node *tomb;
	Node *slot;

	tomb = NULL;
	for (slot = mainposition(mem, size, k);;slot++) { /* linear probing */
		if (keyisempty(slot)) {
			if (!istomb(slot))
				return (tomb ? tomb : slot);
			if (!tomb)
				tomb = slot;
		} else if (eqkey(k, slot)) {
			return slot;
		}
	}
}


/* auxliary function to 'cr_ht_next' */
static unsigned int getindex(VM *vm, HTable *tab, const TValue *k)
{
	Node *slot;

	slot = getslot(tab->mem, tsize(tab), k);
	if (cr_unlikely(keyisempty(slot)))
		cr_assert(0 && "invalid key passed to 'next'");
	return cast_int(slot - tslot(tab, 0));
}


/* 
 * Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 */
int cr_ht_next(VM *vm, HTable *tab, SIndex *k)
{
	Node *slot;
	TValue *v;
	unsigned int i;

	v = s2v(k->p);
	i = getindex(vm, tab, v);
	for (; i < tsize(tab); i++) {
		if (!keyisempty(tslot(tab, i))) {
			slot = tslot(tab, i);
			getnodekey(vm, v, slot);
			setv(vm, v+1, nval(slot));
			return 1;
		}
	}
	return 0;
}


/* insert all the 'keys' from 'stab' into 'dtab' */
void cr_ht_copykeys(VM *vm, HTable *stab, HTable *dtab)
{
	Node *slot;
	TValue k;
	int i;

	for (i = 0; i < tsize(stab); i++) {
		slot = tslot(stab, i);
		if (!keyisempty(slot)) {
			getnodekey(vm, &k, slot);
			cr_ht_set(vm, dtab, &k, nval(slot));
		}
	}
}


/* intern string object */
static void internstring(VM *vm, OString *s)
{
	lmarkgco(s);
	cr_mm_growvec(vm, &vm->interned); /* NOLINT(bugprone-sizeof-expression) */
	lunmarkgco(s);
	vm->interned.ptr[vm->interned.len++] = s;
}


/* intern string literal */
void cr_ht_intern(VM *vm, const char *string)
{
	OString *s;

	s = cr_ot_newstring(vm, string, strlen(string));
	internstring(vm, s);
}


/* intern formatted string */
void cr_ht_internf(VM *vm, const char *fmt, ...)
{
	OString *s;
	va_list argp;

	va_start(argp, fmt);
	s = cr_ot_newstringf(vm, fmt, argp);
	va_end(argp);
	internstring(vm, s);
}


static void rehash(VM *vm, const Node *omem, int osize, Node *nmem, int nsize)
{
	const Node *slot;
	Node *dest;
	TValue *k;
	int i;

	for (i = 0; i < osize; i++) {
		slot = omem + i;
		if (keyisempty(slot))
			continue;
		getnodekey(vm, k, slot);
		dest = getslot(nmem, nsize, k);
		*dest = *slot;
	}
}


/* sets table array slots to 'emptynode' */
cr_sinline void auxsetempty(Node * restrict mem, unsigned int size)
{
	Node *slot;
	int i;

	cr_assert(ispow2(size) && size >= 4);
	slot = mem;
	for (i = 0; i < size; i+=4) { /* unroll */
		*slot++ = emptynode;
		*slot++ = emptynode;
		*slot++ = emptynode;
		*slot++ = emptynode;
	}
}

/* expand hash table array */
static void expandmem(VM *vm, HTable *tab)
{
	Node *newmem;
	unsigned int nsize;
	int osize;

	osize = twoto(tab->size++);
	nsize = twoto(tab->size);
	if (cr_unlikely(nsize >= CR_MAXHTABSIZE))
		cr_assert(0 && "hashtable overflow");
	newmem = cr_mm_newarray(vm, nsize, Node);
	auxsetempty(newmem, nsize);
	rehash(vm, tab->mem, osize, newmem, cast_int(nsize));
	if (tab->mem != NULL)
		cr_mm_freearray(vm, tab->mem, osize);
	tab->mem = newmem;
	tab->size = nsize;
	tab->left = slotsleft(vm, tab);
}


/* 
 * Set value for the given key.
 * If the 'key' was not found insert it together with the 'value'.
 * If the 'key' already exists set its 'value'. 
 */
int cr_ht_set(VM *vm, HTable *tab, const TValue *key, const TValue *val)
{
	Node *slot;
	int newk;

	slot = getslot(tab->mem, tsize(tab), key);
	if ((newk = keyisempty(slot))) { /* new key */
		if (!istomb(slot)) tab->left--;
		if (cr_unlikely(tab->left <= 0)) {
			expandmem(vm, tab);
			cr_ht_set(vm, tab, key, val);
			return 1;
		}
		tab->nnodes++;
	}
	setnodekey(vm, slot, key);
	*nval(slot) = *val;
	return newk;
}


/* 
 * Remove 'key' from the table.
 * If the 'key' was found (and removed) return non-zero and place the tombstone.
 * Tombstones count as entries so this will not decrement 'left'.
 */
int cr_ht_remove(VM *vm, HTable *tab, const TValue *key)
{
	Node *slot;

	slot = getslot(tab->mem, tsize(tab), key);
	if (keyisempty(slot))
		return 0;
	puttomb(slot);
	tab->nnodes--;
	return 1;
}


/* get interned string */
OString *cr_ht_getinterned(HTable *tab, const char *str, size_t len, unsigned int hash)
{
	int size;
	Node *slot;
	OString *s;

	if (tab->nnodes == 0)
		return NULL;
	size = tsize(tab);
	for (slot = hashslot(tab->mem, hash, size);;slot++) {
		if (keyisempty(slot)) {
			if (!istomb(slot)) return NULL;
		} else {
			s = keystrvalue(slot);
			if (streqlit(s, str, len, hash))
				return s;
		}
	};
}


/* 
 * Get key value.
 * If key was found return non-zero and copy its value into 'o'.
 */
int cr_ht_get(VM *vm, HTable *tab, TValue *key, TValue *o)
{
	Node *slot;

	if (tab->nnodes == 0)
		return 0;
	slot = getslot(tab->mem, tsize(tab), key);
	if (keyisempty(slot))
		return 0;
	setv(vm, o, nval(slot));
	return 1;
}


/* free hash table memory and reinitialize it */
void cr_ht_free(VM *vm, HTable *tab)
{
	int size;

	size = tsize(tab);
	cr_mm_freearray(vm, tab->mem, size);
	cr_ht_init(tab);
}
