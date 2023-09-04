#include "memory.h"
#include "object.h"
#include "value.h"

#include <math.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

void Value_print(Value value)
{
    switch(value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NUMBER:
            if(floor(AS_NUMBER(value)) != AS_NUMBER(value)) {
                printf("%f", AS_NUMBER(value));
            } else {
                printf("%ld", (int64_t)AS_NUMBER(value));
            }
            break;
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_OBJ:
            Object_print(value);
            break;
        case VAL_EMPTY:
            printf("<empty>");
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
        case VAL_EMPTY:
        case VAL_NIL:
            return true;
        case VAL_OBJ:
            return AS_OBJ(a) == AS_OBJ(b);
        default:
            _unreachable;
    }
}

Hash Value_hash(Value value)
{
    switch(value.type) {
        case VAL_NIL:
            return 7;
        case VAL_BOOL:
            return AS_BOOL(value) ? 3 : 5;
        case VAL_NUMBER:
            return Hash_double(AS_NUMBER(value));
        case VAL_OBJ:
            return Obj_hash(value);
        case VAL_EMPTY:
            return 0;
        default:
            _unreachable;
    }
}
