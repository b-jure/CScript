#ifndef __SKOOMA_HASHTABLE_H__
#define __SKOOMA_HASHTABLE_H__

#include "common.h"
#include "object.h"

typedef struct {
  ObjString *key;
  Value value;
} Entry;

typedef struct {
  UInt cap;
  UInt len;
  UInt left;
  uint8_t prime;
  Entry *entries;
} HashTable;

void HashTable_init(HashTable *table);
bool HashTable_insert(HashTable *table, ObjString *key, Value value);
bool HashTable_remove(HashTable *table, ObjString *key);
bool HashTable_get(HashTable *table, ObjString *key, Value *out);
void HashTable_free(HashTable *table);

#endif
