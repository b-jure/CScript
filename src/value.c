#include "memory.h"
#include "value.h"

#include <stdio.h>

void Value_print(Value value)
{
    switch(value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NUMBER:
            printf("%g", AS_NUMBER(value));
            break;
        case VAL_NIL:
            printf("nil");
            break;
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
        default:
            _unreachable;
    }
}
