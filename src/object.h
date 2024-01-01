#ifndef SKOBJECT_H
#define SKOBJECT_H

#include "chunk.h"
#include "common.h"
#include "hash.h"
#include "mem.h"
#include "skooma.h"
#include "value.h"

#define IS_STRING(value)  isotype(value, OBJ_STRING)
#define AS_STRING(value)  ((OString*)AS_OBJ(value))
#define AS_CSTRING(value) (((OString*)AS_OBJ(value))->storage)

#define IS_FUNCTION(value) isotype(value, OBJ_FUNCTION)
#define AS_FUNCTION(value) ((OFunction*)AS_OBJ(value))

#define IS_NATIVE(value)    isotype(value, OBJ_NATIVE)
#define AS_NATIVE(value)    ((ONative*)AS_OBJ(value))
#define AS_NATIVE_FN(value) (((ONative*)AS_OBJ(value))->fn)

#define IS_CLOSURE(value) isotype(value, OBJ_CLOSURE)
#define AS_CLOSURE(value) ((OClosure*)AS_OBJ(value))

#define IS_UPVAL(value) isotype(value, OBJ_UPVAL)
#define AS_UPVAL(value) ((OUpvalue*)AS_OBJ(value))

#define IS_CLASS(value) isotype(value, OBJ_CLASS)
#define AS_CLASS(value) ((OClass*)AS_OBJ(value))

#define IS_INSTANCE(value) isotype(value, OBJ_INSTANCE)
#define AS_INSTANCE(value) ((OInstance*)AS_OBJ(value))

#define IS_BOUND_METHOD(value) isotype(value, OBJ_BOUND_METHOD)
#define AS_BOUND_METHOD(value) ((OBoundMethod*)AS_OBJ(value))

typedef enum {
    OBJ_STRING = 0,
    OBJ_FUNCTION,
    OBJ_CLOSURE,
    OBJ_NATIVE,
    OBJ_UPVAL,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_BOUND_METHOD,
} OType;

/*
 * 'header' is laid out like this:
 *
 * .....TTT | .......M | PPPPPPPP | PPPPPPPP | PPPPPPPP | PPPPPPPP | PPPPPPPP |
 * PPPPP...
 *
 * . = empty
 * T = OType bits
 * M = Marked bit
 * P = Pointer bits (to the next object)
 */
struct O { // typedef is inside 'value.h'
    uint64_t header;
};

static force_inline OType otype(O* object)
{
    return (OType)((object->header >> 56) & 0xff);
}

static force_inline void otypeset(O* object, OType type)
{
    object->header = (object->header & 0x00ffffffffffffff) | ((uint64_t)type << 56);
}

static force_inline bool oismarked(O* object)
{
    return (bool)((object->header >> 48) & 0x01);
}

static force_inline void osetmark(O* object, bool mark)
{
    object->header = (object->header & 0xff00ffffffffffff) | ((uint64_t)mark << 48);
}

static force_inline O* onext(O* object)
{
    return (O*)(object->header & 0x0000ffffffffffff);
}

static force_inline void osetnext(O* object, O* next)
{
    object->header = (object->header & 0xffff000000000000) | (uint64_t)next;
}

static force_inline bool isotype(Value value, OType type)
{
    return IS_OBJ(value) && otype(AS_OBJ(value)) == type;
}

struct OString { // typedef is inside 'value.h'
    O obj; // shared header
    size_t len; // string length (excluding null term)
    Hash hash; // cached hash
    char storage[]; // bytes (chars)
};

struct OUpvalue { // typedef is inside 'value.h'
    O obj; // shared header
    Variable closed; // is upvalue closed over
    Value* location; // ptr to 'closed' or VM stack
    OUpvalue* next; // chain
};

struct OFunction { // typedef is inside 'value.h'
    O obj; // shared header
    Chunk chunk; // bytecode and constants
    OString* name; // script name
    UInt upvalc; // number of upvalues
    UInt arity; // Min amount of arguments required
    Byte isva : 1; // If this function takes valist
    Byte isinit : 1; // If this function is class initializer
    Byte gotret : 1; // last instruction is 'OP_TOP/RET'
};

struct OClosure { // typedef is inside 'value.h'
    O obj; // shared header
    OFunction* fn; // wrapped function
    OUpvalue** upvalue; // array of ptr to OUpvalue
    UInt upvalc; // array len
};

struct OClass { // typedef is inside 'value.h'
    O obj; // shared header
    OString* name; // class name
    HashTable methods; // class methods
    OClosure* omethods[OM_CNT]; // overloaded methods
    Value sfields[SF_CNT]; // special fields
};

struct OInstance { // typedef is inside 'value.h'
    O obj; // shared header
    OClass* oclass; // ptr to class we instantiated from
    HashTable fields; // Instance fields
};

struct OBoundMethod { // typedef is inside 'value.h'
    O obj; // shared header
    Value receiver; // ptr to OInstance this method is bound to
    OClosure* method;
};

typedef struct {
    O obj; // shared header
    CFunction fn; // native functions signature
    OString* name; // native function name
    Int arity; // how many arguments
    bool isva; // is this vararg function
    UInt upvalc;
    Value upvalue[1]; // list of upvalues
} ONative; // Native function written in C

OString* OString_new(VM* vm, const char* chars, size_t len);
OString* OString_fmt_from(VM* vm, const char* fmt, va_list argp);
OString* OString_fmt(VM* vm, const char* fmt, ...);
OString* concatenate(VM* vm, Value a, Value b);
OString* unescape(VM* vm, OString* string);
OBoundMethod* OBoundMethod_new(VM* vm, Value receiver, OClosure* method);
OInstance* OInstance_new(VM* vm, OClass* cclass);
OClass* OClass_new(VM* vm, OString* name);
OUpvalue* OUpvalue_new(VM* vm, Value* var_ref);
OClosure* OClosure_new(VM* vm, OFunction* fn);
ONative*
ONative_new(VM* vm, OString* name, CFunction fn, Int arity, bool isva, UInt upvals);
OFunction* OFunction_new(VM* vm);
void otypeprint(OType type); // Debug
OString* otostr(VM* vm, O* object);

void oprint(VM* vm, Value value);
Hash ohash(Value value);
void ofree(VM* vm, O* object);

#endif
