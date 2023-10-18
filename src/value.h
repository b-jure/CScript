#ifndef __SKOOMA_VALUE_H__
#define __SKOOMA_VALUE_H__

#include "array.h"
#include "common.h"
#include "hash.h"

#define AS_OBJ(value)       ((value).as.object)
#define AS_OBJREF(value)    ((value)->as.object)
#define OBJ_TYPE(value)     (Obj_type(AS_OBJ(value)))
#define OBJ_REF_TYPE(value) (Obj_type(AS_OBJREF(value)))
#define IS_OBJ(value)       ((value).type == VAL_OBJ)
#define OBJ_VAL(value)      ((Value){.type = VAL_OBJ, {.object = (Obj*)value}})

#define AS_BOOL(value)     ((value).as.boolean)
#define AS_BOOL_REF(value) ((value)->as.boolean)
#define IS_BOOL(value)     ((value).type == VAL_BOOL)
#define BOOL_VAL(value)    ((Value){.type = VAL_BOOL, {.boolean = value}})

#define AS_NUMBER(value)     ((value).as.number)
#define AS_NUMBER_REF(value) ((value)->as.number)
#define IS_NUMBER(value)     ((value).type == VAL_NUMBER)
#define NUMBER_VAL(value)    ((Value){.type = VAL_NUMBER, {.number = value}})

#define IS_NIL(value) ((value).type == VAL_NIL)
#define NIL_VAL       ((Value){.type = VAL_NIL, {0}})

#define IS_EMPTY(value)   ((value).type == VAL_EMPTY)
#define EMPTY_VAL         ((Value){.type = VAL_EMPTY, {0}})
#define UNDEFINED_VAL     EMPTY_VAL
#define IS_UNDEFINED(val) IS_EMPTY(val)

#define IS_DECLARED(value) ((value).type == VAL_DECLARED)
#define DECLARED_VAL       ((Value){.type = VAL_DECLARED, {0}})

typedef struct Obj         Obj;
typedef struct ObjString   ObjString;
typedef struct ObjFunction ObjFunction;
typedef struct ObjClosure  ObjClosure;
typedef struct ObjUpvalue  ObjUpvalue;
typedef struct ObjClass    ObjClass;
typedef struct ObjInstance ObjInstance;

typedef enum {
    VAL_BOOL = 0,
    VAL_NUMBER,
    VAL_NIL,
    VAL_OBJ,
    VAL_EMPTY,
    VAL_DECLARED,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool   boolean;
        double number;
        Obj*   object;
    } as;
} Value;

ARRAY_NEW(Array_Value, Value);

void Value_print(Value value);
bool Value_eq(Value a, Value b);
Hash Value_hash(Value value);

#endif
