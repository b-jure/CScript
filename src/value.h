#ifndef __SKOOMA_VALUE_H__
#define __SKOOMA_VALUE_H__

#include "common.h"

#define AS_OBJ(value) ((value).as.object)
#define OBJ_TYPE(value) (AS_OBJ(value)->type)
#define IS_OBJ(value) ((value).type == VAL_OBJ)
#define OBJ_VAL(value) ((Value){.type = VAL_OBJ, {.object = (Obj *)value}})

#define AS_BOOL(value) ((value).as.boolean)
#define AS_BOOL_REF(value) ((value)->as.boolean)
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define BOOL_VAL(value) ((Value){.type = VAL_BOOL, {.boolean = value}})

#define AS_NUMBER(value) ((value).as.number)
#define AS_NUMBER_REF(value) ((value)->as.number)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define NUMBER_VAL(value) ((Value){.type = VAL_NUMBER, {.number = value}})

#define IS_NIL(value) ((value).type == VAL_NIL)
#define NIL_VAL ((Value){.type = VAL_NIL, {.number = 0}})

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
  VAL_BOOL = 0,
  VAL_NUMBER,
  VAL_NIL,
  VAL_OBJ,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj *object;
  } as;
} Value;

void Value_print(Value value);
bool Value_eq(Value a, Value b);

#endif
