#ifndef __SKOOMA_OBJECT_H__
#define __SKOOMA_OBJECT_H__

#include "chunk.h"
#include "common.h"
#include "hash.h"
#include "mem.h"
#include "value.h"
#include "vmachine.h"

#define IS_STRING(value)  is_object_type(value, OBJ_STRING)
#define AS_STRING(value)  ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->storage)

#define IS_FUNCTION(value) is_object_type(value, OBJ_FUNCTION)
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))

#define IS_NATIVE(value)    is_object_type(value, OBJ_NATIVE)
#define AS_NATIVE(value)    ((ObjNative*)AS_OBJ(value))
#define AS_NATIVE_FN(value) (((ObjNative*)AS_OBJ(value))->fn)

#define IS_CLOSURE(value) is_object_type(value, OBJ_CLOSURE)
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))

#define IS_UPVAL(value) is_object_type(value, OBJ_UPVAL)
#define AS_UPVAL(value) ((ObjUpvalue*)AS_OBJ(value))


typedef enum { // 1 for marked
    OBJ_STRING   = 2,
    OBJ_FUNCTION = 4,
    OBJ_CLOSURE  = 8,
    OBJ_NATIVE   = 16,
    OBJ_UPVAL    = 32,
} ObjType;

/*
 * 'header' is laid out like this:
 *
 * .....TTT | .......M | PPPPPPPP | PPPPPPPP | PPPPPPPP | PPPPPPPP | PPPPPPPP | PPPPP...
 *
 * . = empty
 * T = ObjType bits
 * M = Marked bit
 * P = Pointer bits (to the next object)
 */
struct Obj { // typedef is inside 'value.h'
    uint64_t header;
};

static force_inline ObjType Obj_type(Obj* object)
{
    return (ObjType)((object->header >> 56) & 0xff);
}

static force_inline void Obj_type_set(Obj* object, ObjType type)
{
    object->header = (object->header & 0x00ffffffffffffff) | ((uint64_t)type << 56);
}

static force_inline bool Obj_marked(Obj* object)
{
    return (bool)((object->header >> 48) & 0x01);
}

static force_inline void Obj_mark_set(Obj* object, bool mark)
{
    object->header = (object->header & 0xff00ffffffffffff) | ((uint64_t)mark << 48);
}

static force_inline Obj* Obj_next(Obj* object)
{
    return (Obj*)(object->header & 0x0000ffffffffffff);
}

static force_inline void Obj_next_set(Obj* object, Obj* next)
{
    object->header = (object->header & 0xffff000000000000) | (uint64_t)next;
}

static force_inline bool is_object_type(Value value, ObjType type)
{
    return IS_OBJ(value) && Obj_type(AS_OBJ(value)) == type;
}

struct ObjString { // typedef is inside 'value.h'
    Obj    obj;
    size_t len;
    Hash   hash;
    char   storage[];
};

struct ObjUpvalue { // typedef is inside 'value.h'
    Obj         obj;
    Value       closed;
    Value*      location;
    ObjUpvalue* next;
};

struct ObjFunction { // typedef is inside 'value.h'
    Obj        obj;
    UInt       arity;
    Chunk      chunk;
    ObjString* name;
    UInt       upvalc; // Count of upvalues
};

struct ObjClosure { // typedef is inisde 'value.h'
    Obj          obj;
    ObjFunction* fn;
    ObjUpvalue** upvals; // size of fn->upvalc
    UInt         upvalc;
};

typedef bool (*NativeFn)(VM* vm, Int argc, Value* argv);

typedef struct {
    Obj      obj;
    NativeFn fn;
    UInt     arity;
} ObjNative;

void         ObjType_print(ObjType type); // Debug
ObjUpvalue*  ObjUpvalue_new(Roots* roots, Value* var_ref);
ObjClosure*  ObjClosure_new(Roots* roots, ObjFunction* fn);
ObjNative*   ObjNative_new(Roots* roots, NativeFn fn, UInt arity);
uint64_t     Obj_hash(Value value);
ObjString*   ObjString_from(Roots* roots, const char* chars, size_t len);
ObjFunction* ObjFunction_new(Roots* roots);
void         Object_print(const Value value);
void         Obj_free(Roots* roots, Obj* object);

#endif
