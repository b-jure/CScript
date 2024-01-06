#include "debug.h"
#include "err.h"
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

/* Perform arithmetic operation on numbers. */
static sk_number narith(VM* vm, sk_number a, sk_number b, Ar op)
{
    switch(op) {
        case AR_ADD:
            return sk_nadd(vm, a, b);
        case AR_SUB:
            return sk_nsub(vm, a, b);
        case AR_MUL:
            return sk_nmul(vm, a, b);
        case AR_DIV:
            return sk_ndiv(vm, a, b);
        case AR_MOD:
            return sk_nmod(vm, a, b);
        case AR_POW:
            return sk_npow(vm, a, b);
        case AR_UMIN:
            return sk_numin(vm, a);
        case AR_NOT:
            return cast(sk_number, 0); // never 'falsey'
        default:
            unreachable;
            return 0;
    }
}


/* Perform arithmetic operation 'op' on skooma values.
 * If arithmetic operation was executed successfully then this
 * returns 1, otherwise 0. */
int varith(VM* vm, Value a, Value b, Ar op, Value* res)
{
    if(arisbin(op)) { // binary operation
        if(IS_NUMBER(a) && IS_NUMBER(b)) goto unarynum;
        else if(IS_STRING(a) && IS_STRING(b)) *res = OBJ_VAL(concatenate(vm, a, b));
        else return 0;
    } else { // unary operation
        if(IS_NUMBER(a)) {
        unarynum:;
            sk_number ret = narith(vm, AS_NUMBER(a), AS_NUMBER(b), op);
            *res = (op != AR_NOT ? NUMBER_VAL(ret) : FALSE_VAL);
        } else if(op == AR_NOT) {
            if(IS_BOOL(a)) *res = !AS_BOOL(a);
            else if(IS_STRING(a)) *res = FALSE_VAL;
            else return 0;
        } else return 0;
    }
    return 1;
}

/* Perform binary/unary operation on values. */
void arith(VM* vm, Value a, Value b, Ar op, Value* res)
{
    if(!varith(vm, a, b, op, res)) {
        otryop(vm, a, b, (op - AR_ADD) + OM_ADD, res);
    }
}


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
    TypeTag tag = val2type(*val);
#if defined(SK_PRECOMPUTED_GOTO)
    static const void* jmptable[TT_CNT] = {
        &&nil,
        &&number,
        &&string,
        &&boolean,
        &&oclass,
        &&instance,
        &&function,
        &&closure,
        &&native,
        &&method,
    };
    goto* jmptable[tag];
nil:
number:
string:
boolean:
    return vm->statics[tag]->storage;
oclass:
    return AS_CLASS(*val)->name->storage;
instance:;
    OInstance* instance = AS_INSTANCE(*val);
    Value debug = getsfield(instance, SF_DEBUG);
    if(IS_STRING(debug)) return AS_CSTRING(debug);
    return instance->oclass->name->storage;
function:
    return AS_FUNCTION(*val)->name->storage;
closure:
    return AS_CLOSURE(*val)->fn->name->storage;
native:
    return AS_NATIVE(*val)->name->storage;
method:
    return AS_BOUND_METHOD(*val)->method->fn->name->storage;
#else
    switch(tag) {
        case TT_NIL:
        case TT_NUMBER:
        case TT_STRING:
        case TT_BOOL:
            return vm->statics[tag]->storage;
        case TT_CLASS:
            return AS_CLASS(*val)->name->storage;
        case TT_INSTANCE: {
            OInstance* instance = AS_INSTANCE(*val);
            Value debug = getsfield(instance, SF_DEBUG);
            if(IS_STRING(debug)) return AS_CSTRING(debug);
            return instance->oclass->name->storage;
        }
        case TT_FUNCTION:
            return AS_FUNCTION(*val)->name->storage;
        case TT_CLOSURE:
            return AS_CLOSURE(*val)->fn->name->storage;
        case TT_NATIVE:
            return AS_NATIVE(*val)->name->storage;
        case TT_METHOD:
            return AS_BOUND_METHOD(*val)->method->fn->name->storage;
        default:
            unreachable;
            return 0;
    }
#endif
}





/* ================= Compare ================= */

// Values are equal
bool veq(VM* vm, Value a, Value b)
{
    UNUSED(vm);
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
bool vlt(VM* vm, Value a, Value b)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) < AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) < 0;
    ordererror(vm, a, b);
}


// Value greater than
bool vgt(VM* vm, Value a, Value b)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) > AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) > 0;
    ordererror(vm, a, b);
}


// Value less equal
bool vle(VM* vm, Value a, Value b)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) <= AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) <= 0;
    ordererror(vm, a, b);
}


// Value greater equal
bool vge(VM* vm, Value a, Value b)
{
    if(IS_NUMBER(a) && IS_NUMBER(b)) return AS_NUMBER(a) >= AS_NUMBER(b);
    if(IS_STRING(a) && IS_STRING(b)) return strcmp(AS_CSTRING(a), AS_CSTRING(b)) >= 0;
    ordererror(vm, a, b);
}




OString* dtostr(VM* vm, sk_number n)
{
    static char buff[50];
    size_t len;
    if(floor(n) != n) len = snprintf(buff, 45, "%g", n);
    else len = snprintf(buff, 45, "%ld", cast(int64_t, n));
    return OString_new(vm, buff, len);
}

OString* btostr(VM* vm, int b)
{
    return (b ? vm->statics[SS_TRUE] : vm->statics[SS_FALSE]);
}

/* Converts value to OString */
OString* vtostr(VM* vm, Value value)
{
#if defined(val2tbmask_1)
    static const void* jmptable[] = {
        &&nil,
        &&number,
        &&boolean,
        &&obj,
    };
    uint8_t idx = sk_ctz(val2tbmask_1(value));
    goto* jmptable[idx];
nil:
    return niltostr(vm);
number:
    return dtostr(vm, AS_NUMBER(value));
boolean:
    return btostr(vm, AS_BOOL(value));
obj:
    return otostr(vm, AS_OBJ(value));
#elif defined(SK_NAN_BOX)
    if(IS_BOOL(value)) return booltostr(vm, AS_BOOL(value));
    else if(IS_NIL(value)) return vm->statics[SS_NIL];
    else if(IS_OBJ(value)) return otostr(vm, AS_OBJ(value));
    else if(IS_NUMBER(value)) return dtostr(vm, AS_NUMBER(value));
#else
#if defined(SK_PRECOMPUTED_GOTO)
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
#endif // val2tbmask_1
    unreachable;
}

void vprint(VM* vm, Value value)
{
#if defined(val2tbmask_1)
    static const void* jmptable[] = {
        &&nil,
        &&number,
        &&boolean,
        &&object,

    };
    goto* jmptable[val2tbmask_1(value)];
nil:
    printf("nil");
    return;
number:
    if(floor(AS_NUMBER(value)) != AS_NUMBER(value)) printf("%lg", AS_NUMBER(value));
    else printf("%ld", (int64_t)AS_NUMBER(value));
    return;
boolean:
    printf(AS_BOOL(value) ? "true" : "false");
    return;
object:
    oprint(vm, value);
    return;
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
#endif // defined(val2tbmask_1)
    return;
}

Hash vhash(Value value)
{
#if defined(val2tbmask_1)
    static const void* jmptable[] = {
        &&nil,
        &&number,
        &&boolean,
        &&object,

    };
    goto* jmptable[val2tbmask_1(value)];
nil:
    unreachable; // nil can't be used for indexing
number:;
    double num = AS_NUMBER(value);
    if(floor(num) != num || num < 0) return dblhash(num);
    else return cast(Hash, num);
boolean:
    return cast(Hash, AS_BOOL(value));
object:
    return ohash(value);
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
#endif // defined(val2tbmask_1)
    unreachable;
    return 0;
}
