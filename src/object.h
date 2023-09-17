#ifndef __SKOOMA_OBJECT_H__
#define __SKOOMA_OBJECT_H__

#include "chunk.h"
#include "common.h"
#include "hash.h"
#include "value.h"
#include "vmachine.h"

#define IS_STRING(value) is_object_type(value, OBJ_STRING)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->storage)

#define IS_FUNCTION(value) is_object_type(value, OBJ_FUNCTION)
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))

typedef enum {
  OBJ_STRING,
  OBJ_FUNCTION,
} ObjType;

struct Obj {
  ObjType type;
  Obj *next;
};

struct ObjString {
  Obj obj;
  size_t len;
  Hash hash;
  char storage[];
};

struct ObjFunction {
  Obj obj;
  Int arity;
  Chunk chunk;
  ObjString *name;
};

static force_inline bool is_object_type(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

uint64_t Obj_hash(Value value);
ObjString *ObjString_from(VM *vm, const char *chars, size_t len);
ObjFunction *ObjFunction_new(VM *vm);
void Object_print(const Value value);
void Obj_free(Obj *object);

#endif
