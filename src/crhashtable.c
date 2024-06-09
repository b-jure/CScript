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
#define lfact(t)	cr_numdiv(cast_num((t)->len), cast_num(tsize(t)))


/* slots left until table needs to grow */
#define slotsleft(t) \
	cast_int((cast_num(CR_MAXTABLOAD) - lfact(t)) * tsize(t))



/* get table hash slot */
#define hashslot(t,h)	node((t), (h) & ((t)->size - 1))



/* initialize hash table */
void cr_ht_init(HashTable *tab)
{
	tab->size = 0;
	tab->nnodes = 0;
	tab->left = 0;
	tab->mem = NULL;
}


/* 
 * Find the main position (slot) for key 'k' inside
 * the table array.
 */
static Node *mainposition(HashTable *tab, const TValue *k)
{
	void *p;
	cr_cfunc f;
	OString *str;

	switch (vtt(k)) {
	case CR_VTRUE:
		return hashslot(tab, cr_hh_boolean(1));
	case CR_VFALSE:
		return hashslot(tab, cr_hh_boolean(0));
	case CR_VNUMINT:
		return hashslot(tab, cr_hh_integer(ivalue(k)));
	case CR_VNUMFLT:
		return hashslot(tab, cr_hh_number(fvalue(k)));
	case CR_VLUDATA:
		p = pvalue(k);
		return hashslot(tab, cr_hh_pointer(p));
	case CR_VCFUNCTION:
		f = cfvalue(k);
		return hashslot(tab, cr_hh_pointer(p));
	case CR_VSTRING:
		str = strvalue(k);
		/* v1.0.0: all strings are interned so this
		 * check doesn't make any sense, meaning all
		 * strings already have the hash the moment 
		 * they are created. */
#if CR_VERSION_NUMBER != 100
		if (str->hashash == 0) {
			str->hash = cr_hh_string(str->bytes, str->len, str->hash);
			str->hashash = 1;
		}
#endif
		return hashslot(tab, str->hash);
	default:
		cr_assert(!ttisnil(k) && ttiso(k));
		return hashslot(tab, cr_hh_pointer(ovalue(k)));
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
 * Size of the table is always power of 2.
 * In case of finding a 'tomb' node keep probing until the
 * same 'key' was found or empty spot.
 * If the empty spot was found and there was a 'tomb', then
 * return the tomb, otherwise return the entry containing the
 * same key.
 * Safety: 'slot' won't overflow because load factor is tracked.
 */
cr_sinline Node *getslot(HashTable *tab, const TValue *k)
{
	Node *tomb;
	Node *slot;

	tomb = NULL;
	for (slot = mainposition(tab, k);;slot++) { /* linear probing */
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
static unsigned int getindex(VM *vm, HashTable *tab, const TValue *k)
{
	int size = tsize(tab);
	unsigned int i;
	Node *slot;

	slot = getslot(tab, k);
	if (cr_unlikely(keyisempty(slot)))
		cr_assert(0 && "invalid key passed to 'next'");
	return cast_int(slot - node(tab, 0));
}


/* 
 * Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 */
cr_ubyte cr_ht_next(VM *vm, HashTable *tab, SIndex *k)
{
	Node *slot;
	TValue *v;
	unsigned int i;

	v = s2v(k->p);
	i = getindex(vm, tab, v);
	for (; i < tsize(tab); i++) {
		if (!keyisempty(node(tab, i))) {
			slot = node(tab, i);
			getnodekey(vm, v, slot);
			settv(vm, v+1, nval(slot));
			return 1;
		}
	}
	return 0;
}


/* insert all the 'keys' from 'stab' into 'dtab' */
void cr_ht_insertfrom(VM *vm, HashTable *stab, HashTable *dtab)
{
	Node *slot;
	TValue k;
	int i;

	for (i = 0; i < tsize(stab); i++) {
		slot = node(stab, i);
		if (!keyisempty(slot)) {
			getnodekey(vm, &k, slot);
			cr_ht_insert(vm, dtab, k, *nval(slot));
		}
	}
}


/* TODO: REMOVE THIS ? ... auxiliary to 'cript.c' source file */
int resizetable(uint32_t wanted)
{
	// Safety: We already ensured wanted != 0
	if (ispow2(wanted))
		return wanted;
	else {
		// https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
		unsigned int cap = wanted - 1;
		cap |= (cap >> 1);
		cap |= (cap >> 2);
		cap |= (cap >> 4);
		cap |= (cap >> 8);
		cap |= (cap >> 16);
		cap++;
		return cap;
	}
}


static void internstring(VM *vm, OString *s)
{
	lmarkgco(s);
	cr_mm_growvec(vm, &vm->interned);
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
void cr_ht_internfmt(VM *vm, const char *fmt, ...)
{
	OString *s;
	va_list argp;

	va_start(argp, fmt);
	s = cr_ot_newstringf(vm, fmt, argp);
	va_end(argp);
	internstring(vm, s);
}


/* expands the table by rehashing all the keys into a new bigger table array. */
static cr_inline void HashTable_expand(VM *vm, HashTable *table, cr_ubyte raw)
{
	uint32_t new_cap = GROW_ARRAY_CAPACITY(table->size, MINSTRTABSIZE);
	Node *entries = GC_MALLOC(vm, new_cap * sizeof(Node));
	for (uint32_t i = 0; i < new_cap; i++) {
		entries[i].key = EMPTY_VAL;
		entries[i].value = EMPTY_VAL;
	}
	for (uint32_t i = 0; i < table->size; i++) {
		Node *entry = &table->mem[i];
		if (IS_EMPTY(entry->key))
			continue;
		Node *dest = findemptyslot(vm, entries, new_cap, entry->key, raw);
		memcpy(dest, entry, sizeof(Node));
	}
	if (table->mem != NULL)
		GC_FREE(vm, table->mem, table->size * sizeof(Node));
	table->mem = entries;
	table->size = new_cap;
	table->left = INSERTS_UNTIL_EXPAND(table);
}

// Insert 'key'/'value' pair into the table.
// If the 'key' was not found insert it together with the 'value' and return
// true. If the 'key' already exists overwrite the 'value' and return false.
cr_ubyte HashTable_insert(VM *vm, HashTable *table, TValue key, TValue val, cr_ubyte raw)
{
	if (table->left == 0)
		HashTable_expand(vm, table, raw);
	Node *entry = findemptyslot(vm, table->mem, table->size, key, raw);
	cr_ubyte new_key = IS_EMPTY(entry->key);
	if (new_key) {
		if (!valistombstone(entry))
			// Only decrement if this entry was never inserted into
			table->left--;
		table->nnodes++;
	}
	entry->key = key;
	entry->value = val;
	return new_key;
}

/* 
 * Remove 'key' from the table.
 * If the 'key' was found (and removed) return non-zero and place the tombstone.
 * Also tombstones count as nodes.
 */
cr_ubyte HashTable_remove(VM *vm, HashTable *table, TValue key, cr_ubyte raw)
{
	Node *entry = findemptyslot(vm, table->mem, table->size, key, raw);
	if (IS_EMPTY(entry->key))
		return 0;
	puttombstone(entry);
	/* Don't increment table->left, we
	 * count tombstones as entries */
	table->nnodes--;
	return 1;
}

// VM specific function, used for finding interned strings before creating
// new 'ObjString' objects.
CRString *HashTable_get_intern(HashTable *table, const char *str, size_t len, cr_hash hash)
{
	if (table->nnodes == 0)
		return NULL;
	uint64_t mask = table->size - 1; // 'cap' is 2^n
	uint64_t index = hash & mask;
	for (;;) {
		Node *entry = &table->mem[index];
		if (IS_EMPTY(entry->key)) {
			if (!valistombstone(entry))
				return NULL;
		} else {
			CRString *string = AS_STRING(entry->key);
			if (string->len == len && string->hash == hash && memcmp(string->storage, str, len) == 0)
				return string;
		}
		index = (index + 1) & mask;
	};
}

// Fetch 'TValue' for given 'key'.
// If 'key' was not found return false, otherwise copy the 'TValue'
// for the given 'key' into 'out' and return true.
cr_ubyte HashTable_get(VM *vm, HashTable *table, TValue key, TValue *out, cr_ubyte raw)
{
	if (table->nnodes == 0)
		return 0;
	Node *entry = findemptyslot(vm, table->mem, table->size, key, raw);
	if (IS_EMPTY(entry->key))
		return 0;
	*out = entry->value;
	return 1;
}

// Free 'table' array and reinitialize the 'table'.
void HashTable_free(VM *vm, HashTable *table)
{
	GC_FREE(vm, table->mem, table->size * sizeof(Node));
	cr_ht_init(table);
}
