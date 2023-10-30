#ifndef __SKOOMA_HASHTABLE_H__
#define __SKOOMA_HASHTABLE_H__

#include "common.h"
#include "value.h"

#ifndef __SKOOMA_COMPILER_H__
typedef struct Compiler Compiler;
#endif

#ifndef __SKOOMA_VMACHINE_H__
typedef struct VM VM;
#endif

typedef struct {
    Value key;
    Value value;
} Entry;

typedef struct {
    UInt   cap;     // table capacity
    UInt   len;     // table length
    UInt   left;    // inserts until load factor exceeded
    Entry* entries; // table array (array of Entry)
} HashTable;

void HashTable_init(HashTable* table);
bool HashTable_insert(VM* vm, HashTable* table, Value key, Value value);
void HashTable_into(VM* vm, HashTable* from, HashTable* to);
bool HashTable_remove(HashTable* table, Value key);
ObjString*
     HashTable_get_intern(HashTable* table, const char* str, size_t len, uint64_t hash);
bool HashTable_get(HashTable* table, Value key, Value* out);
void HashTable_free(VM* vm, HashTable* table);

#endif
