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

#define ISFALSE(value) (IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value)))

/* Object types */
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

/* Get object type */
static force_inline OType otype(O* object)
{
    return (OType)((object->header >> 56) & 0xff);
}

/* Set object type */
static force_inline void otypeset(O* object, OType type)
{
    object->header = (object->header & 0x00ffffffffffffff) | ((uint64_t)type << 56);
}

/* Check if object is marked */
static force_inline bool oismarked(O* object)
{
    return (bool)((object->header >> 48) & 0x01);
}

/* Toggle object mark */
static force_inline void osetmark(O* object, bool mark)
{
    object->header = (object->header & 0xff00ffffffffffff) | ((uint64_t)mark << 48);
}

/* Get pointer to the next object */
static force_inline O* onext(O* object)
{
    return (O*)(object->header & 0x0000ffffffffffff);
}

/* Set object next pointer to point to 'next' */
static force_inline void osetnext(O* object, O* next)
{
    object->header = (object->header & 0xffff000000000000) | (uint64_t)next;
}

/* Check if object is of type 'type' */
static force_inline bool isotype(Value value, OType type)
{
    return IS_OBJ(value) && otype(AS_OBJ(value)) == type;
}






/*
 * ================ objects ================
 */
struct OString { // typedef is inside 'value.h'
    O obj; // common header
    uint32_t len; // string length (excluding null term)
    Hash hash; // cached hash
    char storage[]; // bytes (chars)
};

struct OUpvalue { // typedef is inside 'value.h'
    O obj; // common header
    Value closed;
    Value* location; // ptr to 'closed' or VM stack
    OUpvalue* next; // chain
};

// Function prototype
struct FnPrototype {
    OString* name; // for function declarations and native C functions
    OString* source; // source name (script name or function)
    int32_t defline; // debug info
    int32_t deflastline; // debug info
    int32_t arity; // min number of parameters
    uint32_t upvalc; // upvalues count
    uint8_t isvararg;
};

struct OFunction { // typedef is inside 'value.h'
    O o; // common header
    FnPrototype p; // function prototype
    Chunk chunk; // bytecode and constants
    uint8_t gotret : 1; // @maybe_remove
};

typedef struct {
    O obj; // common header
    FnPrototype p; // function prototype
    CFunction fn; // C function
    Value upvalue[1]; // list of upvalues
} ONative; // Native function written in C

struct OClosure { // typedef is inside 'value.h'
    O obj; // common header
    OFunction* fn; // wrapped function
    OUpvalue** upvalue; // array of ptr to OUpvalue
};

struct OClass { // typedef is inside 'value.h'
    O obj; // common header
    OString* name; // class name
    HashTable methods; // class methods
    OClosure* omethods[OM_CNT]; // overloaded methods
    Value sfields[SF_CNT]; // special fields
};

struct OInstance { // typedef is inside 'value.h'
    O obj; // common header
    OClass* oclass; // ptr to class we instantiated from
    HashTable fields; // Instance fields
};

struct OBoundMethod { // typedef is inside 'value.h'
    O obj; // common header
    Value receiver; // ptr to OInstance this method is bound to
    OClosure* method;
};

/* --------------------------------------------------------- */ // objects






/*
 * ================== Object functions ==================
 */

/* Construct object strings */
OString* OString_new(VM* vm, const char* chars, size_t len);
OString* OString_fmt_from(VM* vm, const char* fmt, va_list argp);
OString* OString_fmt(VM* vm, const char* fmt, ...);
OString* concatenate(VM* vm, Value a, Value b);
OString* unescape(VM* vm, OString* string);


/* Create wrapper around instance method */
OBoundMethod* OBoundMethod_new(VM* vm, Value receiver, OClosure* method);


/* Create class instance */
OInstance* OInstance_new(VM* vm, OClass* cclass);


/* Create class */
OClass* OClass_new(VM* vm, OString* name);


/* Get the value of the class special field */
#define getsfield(instance, sftag) (instance)->oclass->sfields[sftag]


/* Create upvalue */
OUpvalue* OUpvalue_new(VM* vm, Value* var_ref);


/* Create skooma closure */
OClosure* OClosure_new(VM* vm, OFunction* fn);


/* Create native C function */
ONative*
ONative_new(VM* vm, OString* name, CFunction fn, int32_t arity, uint8_t isvararg, uint32_t upvals);


/* Create skoomoa function */
OFunction* OFunction_new(VM* vm);


/* debug only, prints object type name */
void otypeprint(OType type); // Debug


/* Gets/Creates object string from object */
OString* otostr(VM* vm, O* object);


/* Tries calling binary or unary operator overload method */
void otryop(VM* vm, Value a, Value b, OMTag op, Value* res);


/* Prints the object value */
void oprint(VM* vm, Value value, FILE* stream);


/* Ordering */
#if defined(SK_OVERLOAD_OPS)
void oeq(VM* vm, Value l, Value r);
void one(VM* vm, Value l, Value r);
void olt(VM* vm, Value l, Value r);
void ogt(VM* vm, Value l, Value r);
void ole(VM* vm, Value l, Value r);
void oge(VM* vm, Value l, Value r);
#else
#define oeq(vm, l, r) push(BOOL_VAL((l) == (r)))
#define one(vm, l, r) push(BOOL_VAL((l) != (r)))
#endif


/* Hashes the object value */
Hash ohash(Value value);


/* Free object memory */
void ofree(VM* vm, O* object);

/* ------------------------------------------------------ */ // object functions





/* Array holding 'retcnt' for each overload-able method (excluding operators) */
extern const int32_t overloadret[OM_DISPLAY + 1];


#endif
