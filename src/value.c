#include "memory.h"
#include "object.h"
#include "value.h"

#include <memory.h>
#include <stdio.h>

void Value_print(Value value)
{
    switch(value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NUMBER:
            printf("%f", AS_NUMBER(value));
            break;
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_OBJ:
            Object_print(value);
            break;
        default:
            _unreachable;
    }
}

bool Value_eq(Value a, Value b)
{
    if(a.type != b.type) {
        return false;
    }

    switch(a.type) {
        case VAL_BOOL:
            return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NUMBER:
            return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_NIL:
            return true;
        case VAL_OBJ:
            return Object_eq(a, b);
        default:
            _unreachable;
    }
}
