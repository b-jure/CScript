#ifndef __SKOOMA_VALUE_H__
#define __SKOOMA_VALUE_H__

#include "array.h"
#include "common.h"
#include "hash.h"

typedef struct Obj            Obj;
typedef struct ObjString      ObjString;
typedef struct ObjFunction    ObjFunction;
typedef struct ObjClosure     ObjClosure;
typedef struct ObjUpvalue     ObjUpvalue;
typedef struct ObjClass       ObjClass;
typedef struct ObjInstance    ObjInstance;
typedef struct ObjBoundMethod ObjBoundMethod;

#ifdef NAN_BOXING

// Skooma 'Value' is NAN boxed except 'double'.
//
// Here is what each bit represents [0..63].
// bits 0..3              -> type
// bits 4..48             -> value,
// bits 49..50            -> unused,
// bit  51                -> QNaN Floating-Point Indefinite bit
//                           (Intel Manual Volume 1: Chapter 4, 4-3 Table),
// bit  52                -> Quiet NaN bit,
// bits 53..62            -> NaN bits (exponent).
typedef uint64_t Value;

    // NAN 'box' mask
    #define QNAN 0x7ffc000000000000

    // Value type tags
    #define NIL_TAG      0x01
    #define FALSE_TAG    0x02
    #define TRUE_TAG     0x03
    #define EMPTY_TAG    0x04
    #define DECLARED_TAG 0x05
    #define OBJECT_TAG   0x06 // 'Obj*' is 8 bytes aligned (first 3 bits are 0)

    #define AS_OBJ(val)        ((Obj*)((uintptr_t)((val) & 0x0000fffffffffff8)))
    #define AS_BOOL(val)       ((bool)((val) == TRUE_VAL))
    #define AS_NUMBER(val)     (vton(val))
    #define AS_NUMBER_REF(val) *(val)

    #define NUMBER_VAL(num) (ntov(num))
    #define OBJ_VAL(ptr)                                                                 \
        ((Value)((((uint64_t)(ptr)) & 0x0000fffffffffff8) | (OBJECT_TAG | QNAN)))
    #define BOOL_VAL(boolean) ((Value)((FALSE_TAG | ((boolean) & 0x01)) | QNAN))
    #define TRUE_VAL          ((Value)(TRUE_TAG | QNAN))
    #define FALSE_VAL         ((Value)(FALSE_TAG | QNAN))
    #define NIL_VAL           ((Value)(QNAN | NIL_TAG))
    #define EMPTY_VAL         ((Value)(QNAN | EMPTY_TAG))
    #define DECLARED_VAL      ((Value)(QNAN | DECLARED_TAG))
    #define UNDEFINED_VAL     EMPTY_VAL

    #define IS_NUMBER(val)    (((val) & QNAN) != QNAN)
    #define IS_NIL(val)       ((val) == NIL_VAL)
    #define IS_OBJ(val)       (((val) & (OBJECT_TAG | QNAN)) == (OBJECT_TAG | QNAN))
    #define IS_BOOL(val)      (((val) | 0x01) == TRUE_VAL)
    #define IS_EMPTY(val)     ((val) == EMPTY_VAL)
    #define IS_DECLARED(val)  ((val) == DECLARED_VAL)
    #define IS_UNDEFINED(val) IS_EMPTY(val)

    #define OBJ_TYPE(val) (Obj_type(AS_OBJ(val)))

// https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html (-fstrict-aliasing)
static inline Value ntov(double n)
{
    union {
        double n;
        Value  val;
    } bitcast;
    bitcast.n = n;
    return bitcast.val;
}

static inline double vton(Value val)
{
    union {
        double n;
        Value  val;
    } bitcast;
    bitcast.val = val;
    return bitcast.n;
}

#else

typedef enum {
    VAL_BOOL = 0,
    VAL_NUMBER,
    VAL_NIL,
    VAL_OBJ,
    VAL_EMPTY,
    VAL_DECLARED,
} ValueType;


    #define AS_OBJ(value)        ((value).as.object)
    #define AS_BOOL(value)       ((value).as.boolean)
    #define AS_NUMBER(value)     ((value).as.number)
    #define AS_NUMBER_REF(value) ((value)->as.number)

    #define IS_OBJ(value)      ((value).type == VAL_OBJ)
    #define IS_BOOL(value)     ((value).type == VAL_BOOL)
    #define IS_NUMBER(value)   ((value).type == VAL_NUMBER)
    #define IS_NIL(value)      ((value).type == VAL_NIL)
    #define IS_EMPTY(value)    ((value).type == VAL_EMPTY)
    #define IS_DECLARED(value) ((value).type == VAL_DECLARED)
    #define IS_UNDEFINED(val)  IS_EMPTY(val)

    #define OBJ_VAL(value)    ((Value){.type = VAL_OBJ, {.object = (Obj*)value}})
    #define BOOL_VAL(value)   ((Value){.type = VAL_BOOL, {.boolean = value}})
    #define NUMBER_VAL(value) ((Value){.type = VAL_NUMBER, {.number = value}})
    #define EMPTY_VAL         ((Value){.type = VAL_EMPTY, {0}})
    #define DECLARED_VAL      ((Value){.type = VAL_DECLARED, {0}})
    #define NIL_VAL           ((Value){.type = VAL_NIL, {0}})
    #define UNDEFINED_VAL     EMPTY_VAL

    #define OBJ_TYPE(value)   (Obj_type(AS_OBJ(value)))
    #define VALUE_TYPE(value) ((value).type)

typedef struct {
    ValueType type;
    union {
        bool   boolean;
        double number;
        Obj*   object;
    } as;
} Value;

#endif

ARRAY_NEW(Array_Value, Value);

#ifndef __SKOOMA_COMPILER_H__
typedef struct Compiler Compiler;
#endif
#ifndef __SKOOMA_VMACHINE_H__
typedef struct VM VM;
#endif

#define VALSTR(vm, val) (Value_to_str(vm, NULL, val))

ObjString* Value_to_str(VM* vm, Compiler* C, Value value);
void       Value_print(Value value);
bool       Value_eq(Value a, Value b);
Hash       Value_hash(Value value);

Byte dbl_to_str_generic(double dbl, char* dest, UInt len);
Byte bool_to_str_generic(bool boolean, char* dest, UInt len);
Byte nil_to_str_generic(char* dest, UInt len);

#endif
