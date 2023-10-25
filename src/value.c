#include "memory.h"
#include "object.h"
#include "skconf.h"
#include "value.h"

#include <math.h>
#include <memory.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define sizeoffalse (sizeof("false") - 1)
#define sizeoftrue  (sizeof("true") - 1)
#define sizeofnil   (sizeof("nil") - 1)

Byte dbl_to_str_generic(double dbl, char* dest, UInt len)
{
    if(floor(dbl) != dbl) {
        return snprintf(dest, len, "%f", dbl);
    } else {
        return snprintf(dest, len, "%ld", (ssize_t)dbl);
    }
}

Byte bool_to_str_generic(bool boolean, char* dest, UInt len)
{
    if(boolean) {
        len = MIN(len, sizeoftrue);
        memcpy(dest, "true", len);
        return len;
    }
    len = MIN(len, sizeoffalse);
    memcpy(dest, "false", len);
    return len;
}

Byte nil_to_str_generic(char* dest, UInt len)
{
    len = MIN(len, sizeofnil);
    memcpy(dest, "nil", len);
    return len;
}

SK_INTERNAL(force_inline ObjString*) dbl_to_str(VM* vm, Compiler* C, double n)
{
    static char buff[30];
    size_t      len = dbl_to_str_generic(n, buff, UINT8_MAX);
    return ObjString_from(vm, C, buff, len);
}

SK_INTERNAL(force_inline ObjString*) bool_to_str(VM* vm, Compiler* C, bool boolean)
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
#ifdef THREADED_CODE
    #define VAL_TABLE
    #include "jmptable.h"
    #undef VAL_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
#endif
    DISPATCH(value.type)
    {
        CASE(VAL_BOOL)
        {
            return bool_to_str(vm, C, AS_BOOL(value));
        }
        CASE(VAL_NUMBER)
        {
            return dbl_to_str(vm, C, AS_NUMBER(value));
        }
        CASE(VAL_NIL)
        {
            return ObjString_from(vm, C, "nil", sizeof("nil") - 1);
        }
        CASE(VAL_OBJ)
        {
            return Obj_to_str(vm, C, AS_OBJ(value));
        }
    }

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

void Value_print(Value value)
{
#ifdef THREADED_CODE
    #define VAL_TABLE
    #include "jmptable.h"
    #undef BREAK
    #define BREAK return
    #undef VAL_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
#endif
    DISPATCH(value.type)
    {
        CASE(VAL_BOOL)
        {
            printf(AS_BOOL(value) ? "true" : "false");
            BREAK;
        }
        CASE(VAL_NUMBER)
        {
            if(floor(AS_NUMBER(value)) != AS_NUMBER(value)) {
                printf("%f", AS_NUMBER(value));
            } else {
                printf("%ld", (int64_t)AS_NUMBER(value));
            }
            BREAK;
        }
        CASE(VAL_NIL)
        {
            printf("nil");
            BREAK;
        }
        CASE(VAL_OBJ)
        {
            Obj_print(value);
            BREAK;
        }
    }

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

bool Value_eq(Value a, Value b)
{
    if(a.type != b.type) {
        return false;
    }

#ifdef THREADED_CODE
    #define VAL_TABLE
    #include "jmptable.h"
    #undef VAL_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
#endif
    DISPATCH(a.type)
    {
        CASE(VAL_BOOL)
        {
            return AS_BOOL(a) == AS_BOOL(b);
        }
        CASE(VAL_NUMBER)
        {
            return AS_NUMBER(a) == AS_NUMBER(b);
        }
        CASE(VAL_NIL)
        {
            return true;
        }
        CASE(VAL_OBJ)
        {
            return AS_OBJ(a) == AS_OBJ(b);
        }
    }

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

Hash Value_hash(Value value)
{
#ifdef THREADED_CODE
    #define VAL_TABLE
    #include "jmptable.h"
    #undef VAL_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
#endif
    DISPATCH(value.type)
    {
        CASE(VAL_BOOL)
        {
            return AS_BOOL(value) ? 3 : 5;
        }
        CASE(VAL_NUMBER)
        {
            return Hash_double(AS_NUMBER(value));
        }
        CASE(VAL_NIL)
        {
            return 7;
        }
        CASE(VAL_OBJ)
        {
            return Obj_hash(value);
        }
    }
}
