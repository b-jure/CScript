#ifndef __SKOOMA_OBJECT_H__
#define __SKOOMA_OBJECT_H__

#include "chunk.h"
#include "common.h"
#include "hash.h"
#include "mem.h"
#include "value.h"
#include "vmachine.h"

#define IS_STRING(value) is_object_type(value, OBJ_STRING)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->storage)

#define IS_FUNCTION(value) is_object_type(value, OBJ_FUNCTION)
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))

#define IS_NATIVE(value) is_object_type(value, OBJ_NATIVE)
#define AS_NATIVE(value) ((ObjNative *)AS_OBJ(value))
#define AS_NATIVE_FN(value) (((ObjNative *)AS_OBJ(value))->fn)

#define IS_CLOSURE(value) is_object_type(value, OBJ_CLOSURE)
#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))

#define IS_UPVAL(value) is_object_type(value, OBJ_UPVAL)
#define AS_UPVAL(value) ((ObjUpvalue *)AS_OBJ(value))

typedef enum { // 1 for marked
  OBJ_STRING = 2,
  OBJ_FUNCTION = 4,
  OBJ_CLOSURE = 8,
  OBJ_NATIVE = 16,
  OBJ_UPVAL = 32,
} ObjType;

struct Obj { // typedef is inside 'value.h'
  ObjType otype;
  Obj *next;
};

struct ObjString { // typedef is inside 'value.h'
  Obj obj;
  size_t len;
  Hash hash;
  char storage[];
};

struct ObjUpvalue { // typedef is inside 'value.h'
  Obj obj;
  Value closed;
  Value *location;
  ObjUpvalue *next;
};

struct ObjFunction { // typedef is inside 'value.h'
  Obj obj;
  UInt arity;
  Chunk chunk;
  ObjString *name;
  UInt upvalc; // Count of upvalues
};

struct ObjClosure { // typedef is inisde 'value.h'
  Obj obj;
  ObjFunction *fn;
  ObjUpvalue **upvals; // size of fn->upvalc
  UInt upvalc;
};

typedef bool (*NativeFn)(VM *vm, Int argc, Value *argv);

typedef struct {
  Obj obj;
  NativeFn fn;
  UInt arity;
} ObjNative;

static force_inline bool is_object_type(Value value, ObjType type) {
  return IS_OBJ(value) && (AS_OBJ(value)->otype & ~1) == type;
}

void ObjType_print(ObjType type); // Debug
ObjUpvalue *ObjUpvalue_new(Roots *roots, Value *var_ref);
ObjClosure *ObjClosure_new(Roots *roots, ObjFunction *fn);
ObjNative *ObjNative_new(Roots *roots, NativeFn fn, UInt arity);
uint64_t Obj_hash(Value value);
ObjString *ObjString_from(Roots *roots, const char *chars, size_t len);
ObjFunction *ObjFunction_new(Roots *roots);
void Object_print(const Value value);
void Obj_free(Roots *roots, Obj *object);

#endif
