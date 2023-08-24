#ifndef __SKOOMA_VALUE_H__
#define __SKOOMA_VALUE_H__

#include "common.h"

typedef enum {
  VAL_BOOL,
  VAL_NUMBER,
  VAL_NIL,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
  } as;
} Value;

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_BOOL_REF(value) ((value)->as.boolean)
#define AS_NUMBER_REF(value) ((value)->as.number)

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_NIL(value) ((value).type == VAL_NIL)

#define BOOL_VAL(value) ((Value){.type = VAL_BOOL, {.boolean = value}})
#define NUMBER_VAL(value) ((Value){.type = VAL_NUMBER, {.number = value}})
#define NIL_VAL ((Value){.type = VAL_NIL, {.number = 0}})

void Value_print(Value value);
bool Value_eq(Value a, Value b); /* Equal */

#endif
