/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "skarray.h"
#include "skdebug.h"
#include "skhashtable.h"
#include "skmem.h"
#include "skobject.h"
#include "skvalue.h"

// Max table load factor before needing to expand.
#define TABLE_MAX_LOAD 0.70


// Tombstone is a sentinel value indicating there was a collision,
// its key value is VAL_EMPTY and its value is VAL_BOOL 'true'.
#define IS_TOMBSTONE(entry) (IS_BOOL(entry->value) && AS_BOOL(entry->value))
#define PLACE_TOMBSTONE(entry) (entry->value = BOOL_VAL(1))


// Calculate and cache inserts until we need to expand the table.
//
// This saves us from needing to calculate loadfactor each time we insert.
// Instead we check if 'table->left' integer is zero and then expand
// recalculating the load factor only then.
#define INSERTS_UNTIL_EXPAND(table)                                                                \
    ((uint64_t)(((double)TABLE_MAX_LOAD - HashTable_lf(table)) * (table)->cap))


// Initial table size when we expand for the first time (on first insert)
//
// Keep this number '2^n >= 2'.
#define TABLE_INITIAL_SIZE 8


// Initialize the HashTable
void HashTable_init(HashTable* table)
{
    table->cap = 0;
    table->len = 0;
    table->left = 0;
    table->entries = NULL;
}

// Find entry by linear probing, size of the table is always '2^n >=
// TABLE_INITIAL_SIZE'.
//
// In case of finding a 'tombstone' keep probing until the same 'key' was found
// or empty spot.
//
// If the empty spot was found and there was a 'tombstone', then return
// the tombstone, otherwise return the entry containing the same key.
//
// Safety: There can't be an infinite cycle, because load factor is being
// tracked. Hashing: For info about how each 'Value' gets hashed refer to the
// [value.c].
static force_inline Entry*
Entry_find(VM* vm, Entry* entries, uint32_t capacity, Value key, uint8_t raw)
{
    sk_hash hash = vhash(vm, key, raw);
    uint32_t mask = capacity - 1; // 'capacity' is 2^n
    uint64_t index = hash & mask;
    Entry* tombstone = NULL;
    while(1) {
        Entry* entry = &entries[index];
        if(IS_EMPTY(entry->key)) {
            if(!IS_TOMBSTONE(entry)) return (tombstone ? tombstone : entry);
            else if(tombstone == NULL) tombstone = entry;
        } else if(raweq(key, entry->key)) return entry;
        index = (index + 1) & mask;
    };
}

/* Find next table entry after 'key' entry.
 * If table had next entry then top of the stack will contain
 * key of that entry and its value (in that order).
 * Otherwise 0 is returned. */
uint8_t HashTable_next(VM* vm, HashTable* table, Value* key)
{
    Entry* last = NULL;
    Entry* e = Entry_find(vm, table->entries, table->cap, *key, 0);
    if(e == NULL || IS_EMPTY(e->key)) return 0;
    last = table->entries + table->cap;
    for(; e < last; e++) {
        if(!IS_EMPTY(e->key)) { // non-empty entry
            *key = e->key;
            *(key + 1) = e->value;
            return 1;
        }
    }
    return 0;
}

// Rehash all the 'keys' from the 'src' table into the 'dest' table.
void HashTable_into(VM* vm, HashTable* src, HashTable* dest, uint8_t raw)
{
    for(uint32_t i = 0; i < src->cap; i++) {
        Entry* entry = &src->entries[i];
        if(!IS_EMPTY(entry->key)) HashTable_insert(vm, dest, entry->key, entry->value, raw);
    }
}

// Calculate HashTable 'load factor'.
static force_inline double HashTable_lf(HashTable* table)
{
    return (double)table->len / (double)table->cap;
}


// Auxiliary to 'skooma.c' source file
uint32_t resizetable(uint32_t wanted)
{
    // Safety: We already ensured wanted != 0
    if(ispow2(wanted)) return wanted;
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


// Interns string literal.
void internliteral(VM* vm, const char* string)
{
    push(vm, OBJ_VAL(OString_new(vm, string, strlen(string))));
    Array_OSRef_push(&vm->interned, AS_STRING(*stackpeek(0)));
    pop(vm); // string
}


// Intern formatted string.
void internfmt(VM* vm, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    push(vm, OBJ_VAL(OString_fmt_from(vm, fmt, argp)));
    Array_OSRef_push(&vm->interned, AS_STRING(*stackpeek(0)));
    pop(vm); // string
    va_end(argp);
}


// Expands the table by rehashing all the keys into a new bigger table array.
static force_inline void HashTable_expand(VM* vm, HashTable* table, uint8_t raw)
{
    uint32_t new_cap = GROW_ARRAY_CAPACITY(table->cap, TABLE_INITIAL_SIZE);
    Entry* entries = GC_MALLOC(vm, new_cap * sizeof(Entry));
    for(uint32_t i = 0; i < new_cap; i++) {
        entries[i].key = EMPTY_VAL;
        entries[i].value = EMPTY_VAL;
    }
    for(uint32_t i = 0; i < table->cap; i++) {
        Entry* entry = &table->entries[i];
        if(IS_EMPTY(entry->key)) continue;
        Entry* dest = Entry_find(vm, entries, new_cap, entry->key, raw);
        memcpy(dest, entry, sizeof(Entry));
    }
    if(table->entries != NULL) GC_FREE(vm, table->entries, table->cap * sizeof(Entry));
    table->entries = entries;
    table->cap = new_cap;
    table->left = INSERTS_UNTIL_EXPAND(table);
}

// Insert 'key'/'value' pair into the table.
// If the 'key' was not found insert it together with the 'value' and return
// true. If the 'key' already exists overwrite the 'value' and return false.
uint8_t HashTable_insert(VM* vm, HashTable* table, Value key, Value val, uint8_t raw)
{
    if(table->left == 0) HashTable_expand(vm, table, raw);
    Entry* entry = Entry_find(vm, table->entries, table->cap, key, raw);
    uint8_t new_key = IS_EMPTY(entry->key);
    if(new_key) {
        if(!IS_TOMBSTONE(entry))
            // Only decrement if this entry was never inserted into
            table->left--;
        table->len++;
    }
    entry->key = key;
    entry->value = val;
    return new_key;
}

// Remove 'key' from the table.
// If the 'key' was found (and removed) return true and place the tombstone.
// If the 'key' was not found return false.
uint8_t HashTable_remove(VM* vm, HashTable* table, Value key, uint8_t raw)
{
    Entry* entry = Entry_find(vm, table->entries, table->cap, key, raw);
    if(IS_EMPTY(entry->key)) return 0;
    entry->key = EMPTY_VAL;
    PLACE_TOMBSTONE(entry);
    // Don't increment table->left, we
    // count tombstones as entries
    table->len--;
    return 1;
}

// VM specific function, used for finding interned strings before creating
// new 'ObjString' objects.
OString* HashTable_get_intern(HashTable* table, const char* str, size_t len, sk_hash hash)
{
    if(table->len == 0) return NULL;
    uint64_t mask = table->cap - 1; // 'cap' is 2^n
    uint64_t index = hash & mask;
    for(;;) {
        Entry* entry = &table->entries[index];
        if(IS_EMPTY(entry->key)) {
            if(!IS_TOMBSTONE(entry)) return NULL;
        } else {
            OString* string = AS_STRING(entry->key);
            if(string->len == len && string->hash == hash && memcmp(string->storage, str, len) == 0)
                return string;
        }
        index = (index + 1) & mask;
    };
}

// Fetch 'Value' for given 'key'.
// If 'key' was not found return false, otherwise copy the 'Value'
// for the given 'key' into 'out' and return true.
uint8_t HashTable_get(VM* vm, HashTable* table, Value key, Value* out, uint8_t raw)
{
    if(table->len == 0) return 0;
    Entry* entry = Entry_find(vm, table->entries, table->cap, key, raw);
    if(IS_EMPTY(entry->key)) return 0;
    *out = entry->value;
    return 1;
}

// Free 'table' array and reinitialize the 'table'.
void HashTable_free(VM* vm, HashTable* table)
{
    GC_FREE(vm, table->entries, table->cap * sizeof(Entry));
    HashTable_init(table);
}
