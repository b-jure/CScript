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

#include "crdebug.h"
#include "crhashtable.h"
#include "crmem.h"
#include "crobject.h"
#include "crvalue.h"



/* tombstone Node */
static const Node tombstone = { EMPTY_VAL, BOOL_VAL(1) };

/* check if 'node' value is tombstone value */
#define valistombstone(node)	(IS_BOOL((node)->value) && AS_BOOL((node)->value))

/* mark 'node' as tombstone */
#define puttombstone(node)	(*(node) = tombstone)


/* get table load factor */
#define loadfactor(t)	cr_numdiv(cast_num((t)->len),cast_num((t)->size))


/* calculates how many inserts are left until we need to expand the table */
#define insertsleft(t) \
	cast_int((cast_num(MAXTABLOAD) - loadfactor(t)) * (t)->size)



#define tnode(t,i)	(&(t)->mem[(i)])

#define hashslot(t,h)	tnode((t), (h)&((t)->size-1))



/* initialize hash table */
void cr_ht_init(HashTable *tab)
{
	tab->size = 0;
	tab->len = 0;
	tab->left = 0;
	tab->mem = NULL;
}


static Node *mainposition(HashTable *tab, const Value *k)
{
	void *p;
	cr_cfunc f;
	GCObject* o;

	switch (vtt(k)) {
	case VTBOOL:
		return hashslot(tab, hashboolean(asbool(k)));
	case VTINTEGER:
		return hashslot(tab, hashinteger(asint(k)));
	case VTNUMBER:
		return hashslot(tab, hashnumber(asnum(k)));
	case VTLUDATA:
		p = asludata(k);
		return hashslot(tab, hashpointer(p));
	case VTCFUNC:
		f = ascfunc(k);
		return hashslot(tab, hashpointer(f));
	case VTOBJ:
		if (IS_STRING
	default:
		cr_assert(!isemptyval(k) && !isnilval(k) && isobjval(k));
		o = asobj(k);
		return hashslot(tab, hashpointer(o));
	}
}


/* 
 * Find entry by linear probing. 
 * Size of the table is always power of 2.
 * In case of finding a 'tombstone' keep probing until the
 * same 'key' was found or empty spot.
 * If the empty spot was found and there was a 'tombstone', then return
 * the tombstone, otherwise return the entry containing the same key.
 * Safety: There can't be an infinite cycle, because load factor is being
 * tracked.
 */
cr_sinline Node *Entry_find(Node *entries, uint32_t size, Value key, cr_ubyte raw)
{
	unsigned int hash, mask, index;
	Node *tombstone;

	mask = size - 1;
	for (;;) {

	}
	unsigned int mask = size - 1;
	unsigned int index = hash & mask;
	Node *tombstone = NULL;
	while (1) {
		Node *entry = &entries[index];
		if (IS_EMPTY(entry->key)) {
			if (!valistombstone(entry))
				return (tombstone ? tombstone : entry);
			else if (tombstone == NULL)
				tombstone = entry;
		} else if (eqop_raw(key, entry->key))
			return entry;
		index = (index + 1) & mask;
	};
}

/* Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 * Otherwise 0 is returned. */
cr_ubyte HashTable_next(VM *vm, HashTable *table, Value *key)
{
	Node *last = NULL;
	Node *e = Entry_find(vm, table->mem, table->size, *key, 0);
	if (e == NULL || IS_EMPTY(e->key))
		return 0;
	last = table->mem + table->size;
	for (; e < last; e++) {
		if (!IS_EMPTY(e->key)) { // non-empty entry
			*key = e->key;
			*(key + 1) = e->value;
			return 1;
		}
	}
	return 0;
}

// Rehash all the 'keys' from the 'src' table into the 'dest' table.
void HashTable_into(VM *vm, HashTable *src, HashTable *dest, cr_ubyte raw)
{
	for (uint32_t i = 0; i < src->size; i++) {
		Node *entry = &src->mem[i];
		if (!IS_EMPTY(entry->key))
			HashTable_insert(vm, dest, entry->key, entry->value, raw);
	}
}

// Calculate HashTable 'load factor'.
static cr_inline double HashTable_lf(HashTable *table)
{
}


// Auxiliary to 'cript.c' source file
uint32_t resizetable(uint32_t wanted)
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
		ASSERT(ispow2(cap) && cap / 2 < wanted && wanted < cap, "invalid size");
		return cap;
	}
}


// cr_interns string literal.
void internliteral(VM *vm, const char *string)
{
	push(vm, OBJ_VAL(OString_new(vm, string, strlen(string))));
	Array_OSRef_push(&vm->interned, AS_STRING(*stkpeek(0)));
	pop(vm); // string
}


// cr_intern formatted string.
void internfmt(VM *vm, const char *fmt, ...)
{
	va_list argp;
	va_start(argp, fmt);
	push(vm, OBJ_VAL(OString_fmt_from(vm, fmt, argp)));
	Array_OSRef_push(&vm->interned, AS_STRING(*stkpeek(0)));
	pop(vm); // string
	va_end(argp);
}


// Expands the table by rehashing all the keys into a new bigger table array.
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
		Node *dest = Entry_find(vm, entries, new_cap, entry->key, raw);
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
cr_ubyte HashTable_insert(VM *vm, HashTable *table, Value key, Value val, cr_ubyte raw)
{
	if (table->left == 0)
		HashTable_expand(vm, table, raw);
	Node *entry = Entry_find(vm, table->mem, table->size, key, raw);
	cr_ubyte new_key = IS_EMPTY(entry->key);
	if (new_key) {
		if (!valistombstone(entry))
			// Only decrement if this entry was never inserted into
			table->left--;
		table->len++;
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
cr_ubyte HashTable_remove(VM *vm, HashTable *table, Value key, cr_ubyte raw)
{
	Node *entry = Entry_find(vm, table->mem, table->size, key, raw);
	if (IS_EMPTY(entry->key))
		return 0;
	puttombstone(entry);
	/* Don't increment table->left, we
	 * count tombstones as entries */
	table->len--;
	return 1;
}

// VM specific function, used for finding interned strings before creating
// new 'ObjString' objects.
CRString *HashTable_get_intern(HashTable *table, const char *str, size_t len, cr_hash hash)
{
	if (table->len == 0)
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

// Fetch 'Value' for given 'key'.
// If 'key' was not found return false, otherwise copy the 'Value'
// for the given 'key' into 'out' and return true.
cr_ubyte HashTable_get(VM *vm, HashTable *table, Value key, Value *out, cr_ubyte raw)
{
	if (table->len == 0)
		return 0;
	Node *entry = Entry_find(vm, table->mem, table->size, key, raw);
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
