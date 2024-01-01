#include "debug.h"
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



/*
 * If the hardware supports 'find first set' (has the instruction)
 * bit operation then enable this define
 */
#if defined(SK_PRECOMPUTED_GOTO) && __has_builtin(__builtin_ctz)
/* Create type bitmask from the value.
 * First least significant set bit acts as a type tag.
 * bit 0 is set -> number
 * bit 1 is set -> string
 * bit 2 is set -> callable
 * bit 3 is set -> bool
 * bit 4 is set -> nil
 * bit 5 is set -> instance
 * bit 6 is set -> class */
#define val2tbmask(v)                                                                    \
    cast_uint(                                                                           \
        0 | (IS_NIL(v) * 1) | (IS_NUMBER(v) * 2) | (IS_STRING(v) * 4) |                  \
        (IS_BOOL(v) * 8) | (IS_CLASS(v) * 16) | (IS_INSTANCE(v) * 32) |                  \
        (IS_FUNCTION(v) * 64) | (IS_CLOSURE(v) * 128) | (IS_NATIVE(v) * 256) |           \
        (IS_BOUND_METHOD(v) * 512))
#endif


/* Get value type */
int val2type(Value value)
{
#if defined(val2tbmask)
    static const int typetable[] = {
        TT_NIL,
        TT_NUMBER,
        TT_STRING,
        TT_BOOL,
        TT_CLASS,
        TT_INSTANCE,
        TT_FUNCTION,
        TT_CLOSURE,
        TT_NATIVE,
        TT_METHOD,
    };
    unsigned char bitidx = sk_ctz(val2tbmask(value));
    return typetable[bitidx];
#else
    if(IS_NIL(value)) return TT_NIL;
    else if(IS_NUMBER(value)) return TT_NUMBER;
    else if(IS_STRING(value)) return TT_STRING;
    else if(IS_BOOL(value)) return TT_BOOL;
    else if(IS_CLASS(value)) return TT_CLASS;
    else if(IS_INSTANCE(value)) return TT_INSTANCE;
    else if(IS_FUNCTION(value)) return TT_FUNCTION;
    else if(IS_CLOSURE(value)) return TT_CLOSURE;
    else if(IS_NATIVE(value)) return TT_NATIVE;
    else if(IS_BOUND_METHOD(value)) return TT_METHOD;
#endif
    unreachable;
}


const char* vname(VM* vm, Value* val)
{
#if defined(SK_PRECOMPUTED_GOTO)
    TypeTag tag = val2type(*val);
    static const void* jmptable[TT_CNT] = {
        &&number,
        &&string,
        &&function,
        &&boolean,
        &&nil,
        &&instance,
        &&class,
        &&native,
    };
nil:
number:
string:
boolean:
    return vm->statics[tag]->storage;
    class : return AS_CLASS(*val)->name->storage;
instance:;
    OInstance* instance = AS_INSTANCE(*val);
    Value name;
    if(HashTable_get(&instance->fields, OBJ_VAL(vm->statics[SS_DBG]), &name) &&
       IS_STRING(name))
        return AS_CSTRING(name);
    return instance->oclass->name->storage;
function:
    return AS_FUNCTION(*val)->name->storage;
closure:
    return AS_CLOSURE(*val)->fn->name->storage;
native:
    return AS_NATIVE(*val)->name->storage;
#else
#endif
}





/*
 * CMP
 */

// Values are equal
bool veq(Value a, Value b)
{
#ifdef SK_NAN_BOX
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) == AS_NUMBER(b);
    return a == b;
#else
    if(a.type != b.type) return false;
#ifdef SK_PRECOMPUTED_GOTO
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
#endif
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}

// Value less than
CMPFN(vlt)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) < AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) < 0;
    ordererror(vm, a, b);
}


// Value greater than
CMPFN(vgt)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) > AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) > 0;
    ordererror(vm, a, b);
}


// Value less equal
CMPFN(vle)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) <= AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) <= 0;
    ordererror(vm, a, b);
}


// Value greater equal
CMPFN(vge)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) >= AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) >= 0;
    ordererror(vm, a, b);
}






Byte dtos_generic(double dbl, char* dest, UInt limit)
{
    if(floor(dbl) != dbl) return snprintf(dest, limit, "%g", dbl);
    else return snprintf(dest, limit, "%ld", (int64_t)dbl);
}

static force_inline OString* dtostr(VM* vm, double n)
{
    static char buff[50];
    size_t len = dtos_generic(n, buff, 45);
    OString* string = OString_new(vm, buff, len);
    return string;
}

static force_inline OString* booltostr(VM* vm, bool boolean)
{
    if(boolean) return vm->statics[SS_TRUE];
    else return vm->statics[SS_FALSE];
}

/* Converts value to OString */
OString* vtostr(VM* vm, Value value)
{
#ifdef SK_NAN_BOX
    if(IS_BOOL(value)) return booltostr(vm, AS_BOOL(value));
    else if(IS_NIL(value)) return vm->statics[SS_NIL];
    else if(IS_OBJ(value)) return otostr(vm, AS_OBJ(value));
    else if(IS_NUMBER(value)) return dtostr(vm, AS_NUMBER(value));
#else
#ifdef SK_PRECOMPUTED_GOTO
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
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
#endif
    unreachable;
}

void vprint(VM* vm, Value value)
{
#ifdef SK_NAN_BOX
    if(IS_BOOL(value)) printf(AS_BOOL(value) ? "true" : "false");
    else if(IS_NIL(value)) printf("nil");
    else if(IS_OBJ(value)) oprint(vm, value);
    else if(IS_NUMBER(value)) {
        if(floor(AS_NUMBER(value)) != AS_NUMBER(value)) printf("%lg", AS_NUMBER(value));
        else printf("%ld", (int64_t)AS_NUMBER(value));
    } else unreachable;
#else
#ifdef SK_PRECOMPUTED_GOTO
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
            oprint(vm, value);
            BREAK;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
#endif
}

Hash vhash(Value value)
{
#ifdef SK_NAN_BOX
    if(IS_BOOL(value)) return AS_BOOL(value) ? 1 : 0;
    else if(IS_OBJ(value)) return ohash(value);
    else if(IS_NUMBER(value)) {
        double num = AS_NUMBER(value);
        if(floor(num) != num || num < 0) return dblhash(num);
        else return num;
    }
#else
#ifdef SK_PRECOMPUTED_GOTO
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
