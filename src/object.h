#ifndef __SKOOMA_OBJECT_H__
#define __SKOOMA_OBJECT_H__

#include "hash.h"
#include "common.h"
#include "value.h"
#include "vmachine.h"

#define IS_STRING(value) is_object_type(value, OBJ_STRING)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->storage)

typedef enum {
  OBJ_STRING,
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

static _force_inline bool is_object_type(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

uint64_t Obj_hash(Value value);
ObjString *ObjString_from(VM *vm, const char *chars, size_t len);
ObjString *ObjString_from_concat(VM *vm, const char *left, size_t left_len,
                                 const char *right, size_t right_len);
void Object_print(const Value value);
void Obj_free(Obj *object);

#endif
