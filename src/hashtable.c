#include "array.h"
#include "hashtable.h"
#include "mem.h"

#include <stdio.h>
#include <stdlib.h>

#define PRIME_TABLE_LEN                    sizeof(prime_table) / sizeof(prime_table[0])
#define TABLE_MAX_LOAD                     0.50
#define TABLE_MAX_SIZE                     prime_table[PRIME_TABLE_LEN - 1]
#define QUADRATIC_PROBE(hash, i, capacity) (((hash) + ((i) * (i))) % capacity)
#define IS_TOMBSTONE(value)                IS_BOOL((value))
#define INSERTS_UNTIL_EXPAND(table)                                                      \
    ((UInt)((double)TABLE_MAX_LOAD - HashTable_lf(table)) * (table)->cap)

/* List of possible table sizes up to the 2^31 - 1, they are all
 * prime numbers, that is because using open addressing with quadratic probing and
 * having a table size be a prime number guarantees we will visit at least half of the
 * buckets in the table before needing to expand the table array. */
static const UInt prime_table[] = {
    13,       31,       61,        127,       251,       509,        1021,
    2039,     4093,     8191,      16381,     32749,     65521,      131071,
    262139,   524287,   1048573,   2097143,   4194301,   8388593,    16777213,
    33554393, 67108859, 134217689, 268435399, 536870909, 1073741789, 2147483647,
};

/* Init the HashTable (same as memset(table, 0, sizeof(HashTable))) */
void HashTable_init(HashTable* table)
{
    table->cap = 0;
    table->len = 0;
    table->left = 0;
    table->prime = 0;
    table->entries = NULL;
}

/* Return the next prime capacity, or exit() if table size limit reached */
static _force_inline size_t get_prime_capacity(uint8_t old_prime)
{
    if(_unlikely(old_prime >= PRIME_TABLE_LEN)) {
        fprintf(
            stderr,
            "Out of memory: HashTable %d:%s [%s]\n",
            __LINE__,
            __FILE__,
            __func__);
        exit(EXIT_FAILURE);
    } else {
        return prime_table[old_prime + 1];
    }
}

/* Return pointer to the 'entry' for the given 'key' */
static Entry* HashTable_get(HashTable* table, ObjString* key)
{
    UInt i = 0;
    UInt capacity = table->cap;
    UInt index = key->hash % capacity;
    while(true) {
        Entry* entry = &table->entries[index];
        if(entry->key == key || (entry->key == NULL && !IS_TOMBSTONE(entry->value))) {
            return entry;
        }
        ++i;
        index = QUADRATIC_PROBE(key->hash, i, capacity);
    }
}

/* Rehash all the keys in the 'from' table into 'to' table */
static _force_inline void HashTable_rehash(HashTable* from, HashTable* to)
{
    for(UInt i = 0; i < from->cap; i++) {
        Entry* entry = &from->entries[i];
        if(entry->key == NULL) {
            continue;
        }

        Entry* dest = HashTable_get(to, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
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

    UInt   new_cap = get_prime_capacity(table->prime++);
    Entry* newmem = MALLOC(new_cap);
    for(UInt i = 0; i < table->cap; i++) {
        newmem[i].key = NULL;
        newmem[i].value = NIL_VAL;
    }

    HashTable new_table;
    new_table.len = table->len;
    new_table.cap = new_cap;
    new_table.entries = newmem;
    new_table.prime = table->prime + 1;
    new_table.left = INSERTS_UNTIL_EXPAND(&new_table);

    HashTable_rehash(table, &new_table);
    MFREE_ARRAY(Entry, table->entries, table->cap);
    *table = new_table;
}

/* Return 'true' only when we insert the whole key/value pair,
 * otherwise change the value of the existing key and return false */
bool HashTable_insert(HashTable* table, ObjString* key, Value value)
{
    if(table->left == 0) {
        HashTable_expand(table);
    }

    Entry* entry = HashTable_get(table, key);

    if(entry->key == NULL) {
        entry->key = key;
        entry->value = value;
        table->left--;
        table->len++;
        return true;
    }

    entry->value = value;
    return false;
}

void HashTable_free(HashTable* table)
{
    MFREE_ARRAY(Entry, table->entries, table->cap);
    HashTable_init(table);
}
