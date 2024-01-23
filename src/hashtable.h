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
void HashTable_free(VM* vm, HashTable* table);

bool HashTable_insert(VM* vm, HashTable* table, Value key, Value value);
bool HashTable_remove(HashTable* table, Value key);

bool HashTable_get(HashTable* table, Value key, Value* out);
OString* HashTable_get_intern(HashTable* table, const char* str, size_t len, Hash hash);
uint8_t HashTable_next(VM* vm, HashTable* table, Value* key);


void HashTable_into(VM* vm, HashTable* from, HashTable* to);


uint32_t resizetable(uint32_t wanted);


void internliteral(VM* vm, const char* string);
void internfmt(VM* vm, const char* fmt, ...);



// Raw table access
#define rawget(table, key, out) (HashTable_get(table, key, out))
#define rawset(table, key, value) (HashTable_insert(vm, table, key, value))

#define SK_RAWFIELD 0
#define SK_RAWMETHOD 1

#define rawgettable(vm, instance, what)                                                            \
    (what == SK_RAWFIELD ? &(instance)->fields : &(instance)->oclass->methods)

#endif
