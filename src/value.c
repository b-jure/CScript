#include "hash.h"
#include "memory.h"
#include "object.h"
#include "skconf.h"
#include "skmath.h"
#include "value.h"

#include <memory.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define sizeoffalse (sizeof("false") - 1)
#define sizeoftrue  (sizeof("true") - 1)
#define sizeofnil   (sizeof("nil") - 1)

Byte dtos_generic(double dbl, char* dest, UInt limit)
{
    if(floor(dbl) != dbl) return snprintf(dest, limit, "%g", dbl);
    else return snprintf(dest, limit, "%ld", (int64_t)dbl);
}

static force_inline OString* dtostr(VM* vm, double n)
{
    static char buff[50];
    size_t      len    = dtos_generic(n, buff, 45);
    OString*    string = OString_new(vm, buff, len);
    return string;
}

static force_inline OString* booltostr(VM* vm, bool boolean)
{
    if(boolean) return vm->statics[SS_TRUE];
    else return vm->statics[SS_FALSE];
}

OString* vtostr(VM* vm, Value value)
{
#ifdef S_NAN_BOX
    if(IS_BOOL(value)) return booltostr(vm, AS_BOOL(value));
    else if(IS_NIL(value)) return vm->statics[SS_NIL];
    else if(IS_OBJ(value)) return otostr(vm, AS_OBJ(value));
    else if(IS_NUMBER(value)) return dtostr(vm, AS_NUMBER(value));
#else
#ifdef S_PRECOMPUTED_GOTO
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#endif
    DISPATCH(VALUE_TYPE(value))
    {
        CASE(VAL_BOOL)
        {
            return booltostr(vm, AS_BOOL(value));
        }
        CASE(VAL_NUMBER)
        {
            return dtostr(vm, AS_NUMBER(value));
        }
        CASE(VAL_NIL)
        {
            return vm->statics[SS_NIL];
        }
        CASE(VAL_OBJ)
        {
            return otostr(vm, AS_OBJ(value));
        }
    }
#ifdef SKOOMA_JMPTABLE_H
#undef SKOOMA_JMPTABLE_H
#endif
#endif
    unreachable;
}

void vprint(Value value)
{
#ifdef S_NAN_BOX
    if(IS_BOOL(value)) printf(AS_BOOL(value) ? "true" : "false");
    else if(IS_NIL(value)) printf("nil");
    else if(IS_OBJ(value)) oprint(value);
    else if(IS_NUMBER(value)) {
        if(floor(AS_NUMBER(value)) != AS_NUMBER(value)) printf("%lg", AS_NUMBER(value));
        else printf("%ld", (int64_t)AS_NUMBER(value));
    } else unreachable;
#else
#ifdef S_PRECOMPUTED_GOTO
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
            if(floor(AS_NUMBER(value)) != AS_NUMBER(value))
                printf("%f", AS_NUMBER(value));
            else printf("%ld", (int64_t)AS_NUMBER(value));
            BREAK;
        }
        CASE(VAL_NIL)
        {
            printf("nil");
            BREAK;
        }
        CASE(VAL_OBJ)
        {
            oprint(value);
            BREAK;
        }
    }
    unreachable;
#ifdef SKOOMA_JMPTABLE_H
#undef SKOOMA_JMPTABLE_H
#endif
#endif
}

#ifndef S_NAN_BOX
bool Value_eq(Value a, Value b)
{
    if(a.type != b.type) return false;
#ifdef S_PRECOMPUTED_GOTO
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
#ifdef SKOOMA_JMPTABLE_H
#undef SKOOMA_JMPTABLE_H
#endif
}
#endif

Hash vhash(Value value)
{
#ifdef S_NAN_BOX
    if(IS_BOOL(value)) return AS_BOOL(value) ? 1 : 0;
    else if(IS_OBJ(value)) return ohash(value);
    else if(IS_NUMBER(value)) {
        double num = AS_NUMBER(value);
        if(floor(num) != num || num < 0) return dblhash(num);
        else return num;
    }
#else
#ifdef S_PRECOMPUTED_GOTO
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
            return AS_BOOL(value) ? 1 : 0;
        }
        CASE(VAL_NUMBER)
        {
            double num = AS_NUMBER(value);
            if(floor(AS_NUMBER(value)) != AS_NUMBER(value) || AS_NUMBER(value) < 0)
                return dblhash(num);
            else return num;
        }
        CASE(VAL_OBJ)
        {
            return ohash(value);
        }
    }
#endif
    unreachable;
}
