#include "memory.h"
#include "object.h"
#include "value.h"

#include <math.h>
#include <memory.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

ObjString* dbl_to_str(VM* vm, Compiler* C, double n)
{
    static char buff[UINT8_MAX];
    size_t      len;

    if(floor(n) != n) {
        len = snprintf(buff, sizeof(buff), "%f", n);
    } else {
        len = snprintf(buff, sizeof(buff), "%ld", (ssize_t)n);
    }

    int c;
    /* Trim excess zeroes */
    for(Byte i = len - 1; i > 0; i--) {
        switch((c = buff[i])) {
            case '0':
                len--;
                break;
            default:
                break;
        }
    }

    return ObjString_from(vm, C, buff, len);
}

ObjString* bool_to_str(VM* vm, Compiler* C, bool boolean)
{
    char*  str = NULL;
    ushort len = 0;
    if(boolean) {
        len = sizeof("true") - 1;
        str = "true";
    } else {
        len = sizeof("false") - 1;
        str = "false";
    }
    return ObjString_from(vm, C, str, len);
}

ObjString* Value_to_str(VM* vm, Compiler* C, Value value)
{
    switch(value.type) {
        case VAL_BOOL:
            return bool_to_str(vm, C, AS_BOOL(value));
        case VAL_NUMBER:
            return dbl_to_str(vm, C, AS_NUMBER(value));
        case VAL_OBJ:
            return Obj_to_str(vm, C, AS_OBJ(value));
        case VAL_NIL:
            return ObjString_from(vm, C, "nil", sizeof("nil") - 1);
        case VAL_EMPTY:
        case VAL_DECLARED:
        default:
            unreachable;
    }
}

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
        case VAL_DECLARED:
        default:
            unreachable;
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
        case VAL_DECLARED:
        default:
            unreachable;
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
        case VAL_DECLARED:
        default:
            unreachable;
    }
}
