#ifndef SKHASHTABLE_H
#define SKHASHTABLE_H

#include "common.h"
#include "value.h"

#ifndef SKOOMA_VMACHINE_H
typedef struct VM VM;
#endif

typedef struct {
    Value key;
    Value value;
} Entry;

typedef struct {
    UInt cap; // table capacity
    UInt len; // table length
    UInt left; // inserts until load factor exceeded
    Entry* entries; // table array (array of Entry)
} HashTable;

void HashTable_init(HashTable* table);
bool HashTable_insert(VM* vm, HashTable* table, Value key, Value value);
void HashTable_into(VM* vm, HashTable* from, HashTable* to);
bool HashTable_remove(HashTable* table, Value key);
OString*
HashTable_get_intern(HashTable* table, const char* str, size_t len, uint64_t hash);
bool HashTable_get(HashTable* table, Value key, Value* out);
void HashTable_free(VM* vm, HashTable* table);
unsigned int resizetable(unsigned int wanted);

#endif
