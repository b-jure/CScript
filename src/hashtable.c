#include "hashtable.h"
#include "mem.h"
#include "object.h"

#include <stdio.h>
#include <stdlib.h>

#define GROW_TABLE_CAPACITY(prime) get_prime_capacity(prime)

#define PRIME_TABLE_LEN sizeof(prime_table) / sizeof(prime_table[0])

#define TABLE_MAX_LOAD 0.50

#define TABLE_MAX_SIZE prime_table[PRIME_TABLE_LEN - 1]

#define QUADRATIC_PROBE(hash, i, capacity) (((hash) + ((i) * (i))) % capacity)

#define IS_TOMBSTONE(entry) IS_BOOL(entry->value)

#define PLACE_TOMBSTONE(entry) (entry->value = BOOL_VAL(true))

#define INSERTS_UNTIL_EXPAND(table)                                                      \
    ((UInt)(((double)TABLE_MAX_LOAD - HashTable_lf(table)) * (table)->cap))

/* List of possible table sizes up to the 2^31 - 1, they are all
 * prime numbers, that is because using open addressing with quadratic probing and
 * having a table size be a prime number guarantees we will visit at least half of the
 * buckets in the table before needing to expand the table array to prevent cycles. */
static const UInt prime_table[] = {
    13,       31,       61,        127,       251,       509,        1021,
    2039,     4093,     8191,      16381,     32749,     65521,      131071,
    262139,   524287,   1048573,   2097143,   4194301,   8388593,    16777213,
    33554393, 67108859, 134217689, 268435399, 536870909, 1073741789, 2147483647,
};

/* Init the HashTable */
void HashTable_init(HashTable* table)
{
    table->cap     = 0;
    table->len     = 0;
    table->left    = 0;
    table->prime   = 0;
    table->entries = NULL;
}

/* Return the next prime capacity or exit if table size limit reached */
static _force_inline size_t get_prime_capacity(uint8_t old_prime)
{
    if(_unlikely(old_prime >= PRIME_TABLE_LEN)) {
        fprintf(
            stderr,
            "HashTable size exceeded (LIMIT: %u entries): %d:%s [%s]\n",
            prime_table[PRIME_TABLE_LEN - 1],
            __LINE__,
            __FILE__,
            __func__);
        exit(EXIT_FAILURE);
    } else {
        return prime_table[old_prime];
    }
}

static _force_inline Entry* Entry_find(Entry* entries, UInt len, Value key)
{
    UInt   i           = 0;
    Hash   hash        = Value_hash(key);
    UInt   index       = hash % len;
    UInt   start_index = index;
    Entry* tombstone   = NULL;

    do {
        Entry* entry = &entries[index];

        if(IS_EMPTY(entry->key)) {
            if(!IS_TOMBSTONE(entry)) {
                return (tombstone ? tombstone : entry);
            } else if(tombstone == NULL) {
                tombstone = entry;
            }
        } else if(Value_eq(key, entry->key)) {
            return entry;
        }

        ++i;
        index = QUADRATIC_PROBE(hash, i, len);
    } while(_likely(start_index != index));

    /* We did a cycle so we have a tombstone */
    return tombstone;
}

/* Rehash all the keys from the table 'from' into the table 'to' */
static _force_inline void HashTable_into(HashTable* from, HashTable* to)
{
    for(UInt i = 0; i < from->cap; i++) {
        Entry* entry = &from->entries[i];
        if(!IS_EMPTY(entry->key)) {
            HashTable_insert(to, entry->key, entry->value);
        }
    }
}

/* HashTable load factor */
static _force_inline double HashTable_lf(HashTable* table)
{
    return (double)table->len / (double)table->cap;
}

/* Expand the table array */
static _force_inline void HashTable_expand(HashTable* table)
{
    UInt   new_cap = GROW_TABLE_CAPACITY(table->prime++);
    Entry* entries = ALLOC_ARRAY(Entry, new_cap);
    for(UInt i = 0; i < new_cap; i++) {
        entries[i].key   = EMPTY_VAL;
        entries[i].value = EMPTY_VAL;
    }

    for(UInt i = 0; i < table->cap; i++) {
        Entry* entry = &table->entries[i];
        if(entry == NULL) {
            continue;
        }
        Entry* dest = Entry_find(entries, new_cap, entry->key);
        *dest       = *entry;
    }

    if(table->entries != NULL) {
        MFREE_ARRAY(Entry, table->entries, table->cap);
    }

    table->entries = entries;
    table->cap     = new_cap;
    table->left    = INSERTS_UNTIL_EXPAND(table);
}

/* Return 'true' only when we insert the whole key/value pair,
 * otherwise change the value of the existing key and return false */
bool HashTable_insert(HashTable* table, Value key, Value value)
{
    if(table->left == 0) {
        HashTable_expand(table);
    }

    Entry* entry   = Entry_find(table->entries, table->cap, key);
    bool   new_key = IS_EMPTY(entry->key);

    if(new_key) {
        table->left--;
        table->len++;
    }

    entry->key   = key;
    entry->value = value;
    return new_key;
}

bool HashTable_remove(HashTable* table, Value key)
{
    Entry* entry = Entry_find(table->entries, table->cap, key);

    if(IS_EMPTY(entry->key)) {
        return false;
    }

    entry->key = EMPTY_VAL;
    PLACE_TOMBSTONE(entry);

    table->len--;
    table->left++;
    return true;
}

/* NOTE: This is used only for VM strings table */
ObjString* HashTable_get_intern(HashTable* table, const char* str, size_t len, Hash hash)
{
    if(table->len == 0) {
        return NULL;
    }

    UInt i           = 0;
    UInt index       = hash % table->cap;
    UInt start_index = index;

    do {
        Entry* entry = &table->entries[index];

        if(IS_EMPTY(entry->key)) {
            if(!IS_TOMBSTONE(entry)) {
                return NULL;
            }
        } else {
            ObjString* string = AS_STRING(entry->key);

            if(string->len == len && string->hash == hash &&
               memcmp(string->storage, str, len) == 0)
            {
                return string;
            }
        }

        ++i;
        index = QUADRATIC_PROBE(hash, i, table->cap);
    } while(_likely(start_index != index));

    return NULL;
}

bool HashTable_get(HashTable* table, Value key, Value* out)
{
    if(table->len == 0) {
        return false;
    }

    Entry* entry = Entry_find(table->entries, table->cap, key);

    if(IS_EMPTY(entry->key)) {
        return false;
    }

    *out = entry->value;
    return true;
}

/* Free table array and reset table fields */
void HashTable_free(HashTable* table)
{
    MFREE_ARRAY(Entry, table->entries, table->cap);
    HashTable_init(table);
}
