/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "skhash.h"
#include "skobject.h"
#include "skconf.h"
#include "skmath.h"
#include "skvalue.h"

#include <memory.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Perform arithmetic operation on numbers. */
static sk_number narith(VM* vm, sk_number a, sk_number b, sk_ar op)
{
    switch(op) {
        case AR_ADD: return sk_nadd(vm, a, b);
        case AR_SUB: return sk_nsub(vm, a, b);
        case AR_MUL: return sk_nmul(vm, a, b);
        case AR_DIV: return sk_ndiv(vm, a, b);
        case AR_MOD: return sk_nmod(vm, a, b);
        case AR_POW: return sk_npow(vm, a, b);
        case AR_UMIN: return sk_numin(vm, a);
        case AR_NOT: return cast(sk_number, 0); // never 'falsey'
        default: unreachable; return 0;
    }
}



/* Perform arithmetic operation 'op' on skooma values.
 * If arithmetic operation was executed successfully then this
 * returns 1, otherwise 0. */
int varith(VM* vm, Value a, Value b, sk_ar op, Value* res)
{
    if(arisbin(op)) { // binary operation
        if(IS_NUMBER(a) && IS_NUMBER(b)) goto l_unarynum;
        else if(IS_STRING(a) && IS_STRING(b)) *res = OBJ_VAL(concatenate(vm, a, b));
        else return 0;
    } else { // unary operation
        if(IS_NUMBER(a)) {
        l_unarynum:;
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
void arith(VM* vm, Value a, Value b, sk_ar op, Value* res)
{
    if(!varith(vm, a, b, op, res)) {
#if defined(SK_OVERLOAD_OPS)
        otryop(vm, a, b, (op - AR_ADD) + OM_ADD, res);
#else
        operror(vm, a, b, op);
#endif
    }
}


/* Get value type */
sk_tt val2type(Value value)
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
    };
    uint8_t bitidx = sk_ctz(val2tbmask(value));
    return typetable[bitidx];

#elif defined(SK_NAN_BOX)

    if(IS_NIL(value)) return TT_NIL;
    else if(IS_NUMBER(value)) return TT_NUMBER;
    else if(IS_STRING(value)) return TT_STRING;
    else if(IS_BOOL(value)) return TT_BOOL;
    else if(IS_CLASS(value)) return TT_CLASS;
    else if(IS_INSTANCE(value)) return TT_INSTANCE;
    else if(IS_FUNCTION(value) || IS_CLOSURE(value) || IS_NATIVE(value) || IS_BOUND_METHOD(value))
        return TT_FUNCTION;
    unreachable;

#else

#if defined(SK_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(l) case l:
#endif
    DISPATCH(value.type)
    {
        CASE(VAL_NIL)
        {
            return TT_NIL;
        }
        CASE(VAL_NUMBER)
        {
            return TT_NUMBER;
        }
        CASE(VAL_BOOL)
        {
            return TT_BOOL;
        }
        CASE(VAL_OBJ)
        {
            switch(otype(AS_OBJ(value))) {
                case OBJ_STRING: return TT_STRING;
                case OBJ_CLASS: return TT_CLASS;
                case OBJ_NATIVE:
                case OBJ_FUNCTION:
                case OBJ_CLOSURE:
                case OBJ_BOUND_METHOD: return TT_FUNCTION;
                case OBJ_INSTANCE: return TT_INSTANCE;
                default: unreachable; // upvalue
            }
        }
    }

#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
#endif
}


/* @TODO: What is this used for ?? */
const char* vname(VM* vm, Value val)
{
    sk_tt tag = val2type(val);
#if defined(SK_PRECOMPUTED_GOTO)
    static const void* jmptable[TT_CNT] = {
        &&nil,
        &&number,
        &&string,
        &&boolean,
        &&oclass,
        &&instance,
        &&function,
    };
    goto* jmptable[tag];
nil:
number:
string:
boolean:
    return vm->faststatic[tag]->storage;
oclass:
    return AS_CLASS(val)->name->storage;
instance:;
    OInstance* instance = AS_INSTANCE(val);
    Value debug;
    if(rawget(&instance->fields, OBJ_VAL(vm->faststatic[SS_DBG]), &debug) && IS_STRING(debug))
        return AS_CSTRING(debug);
    return instance->oclass->name->storage;
function:;
    Value value = val;
    if(IS_CLOSURE(value)) return AS_CLOSURE(value)->fn->p.name->storage;
    else if(IS_NATIVE(value)) return AS_NATIVE(value)->p.name->storage;
    else if(IS_BOUND_METHOD(value)) return AS_BOUND_METHOD(value)->method->fn->p.name->storage;
    else return AS_FUNCTION(val)->p.name->storage; // is this reachable ??
#else
    switch(tag) {
        case TT_NIL:
        case TT_NUMBER:
        case TT_STRING:
        case TT_BOOL: return vm->statics[tag]->storage;
        case TT_CLASS: return AS_CLASS(val)->name->storage;
        case TT_INSTANCE: {
            OInstance* instance = AS_INSTANCE(val);
            Value debug = getsfield(instance, SF_DEBUG);
            if(IS_STRING(debug)) return AS_CSTRING(debug);
            return instance->oclass->name->storage;
        }
        case TT_FUNCTION: {
            Value value = val;
            if(IS_CLOSURE(value)) return AS_CLOSURE(value)->fn->name->storage;
            else if(IS_NATIVE(value)) return AS_NATIVE(value)->name->storage;
            else if(IS_BOUND_METHOD(value))
                return AS_BOUND_METHOD(value)->method->fn->name->storage;
            else return AS_FUNCTION(val)->name->storage; // is this reachable ??
        }
        default: unreachable; return 0;
#endif
}





/* ================= ordering ================= */


/* Special equality ordering that preserves left operand ('switch' statement) */
void eq_preserveL(VM* vm, Value l, Value r)
{
    push(vm, r);
    *stackpeek(1) = l;
    veq(vm, l, r);
}

/* Raw equality.
 * No overloaded method will be invoked, instead the result is directly
 * returned from the function. */
uint8_t raweq(Value l, Value r)
{
#if defined(SK_NAN_BOX)
    // NaN != NaN
    if(IS_NUMBER(l) && IS_NUMBER(r)) return AS_NUMBER(l) == AS_NUMBER(r);
    else return l == r;
#else
        if(l.type != r.type) return 0;
#if defined(SK_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#endif
        DISPATCH(l.type)
        {
            CASE(VAL_BOOL)
            {
                return AS_BOOL(l) == AS_BOOL(r);
            }
            CASE(VAL_NUMBER)
            {
                return AS_NUMBER(l) == AS_NUMBER(r);
            }
            CASE(VAL_NIL)
            {
                return 1;
            }
            CASE(VAL_OBJ)
            {
                return l == r;
            }
        }
#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
#endif
}



/* != */
void vne(VM* vm, Value l, Value r)
{
#if defined(SK_NAN_BOX)
    if(IS_NUMBER(l) && IS_NUMBER(r)) push(vm, BOOL_VAL(AS_NUMBER(l) != AS_NUMBER(r)));
    else one(vm, l, r);
#else
        if(l.type != r.type) push(vm, TRUE_VAL);
#if defined(SK_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#undef BREAK
#define BREAK return
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK break
#endif
        DISPATCH(l.type)
        {
            CASE(VAL_BOOL)
            {
                push(vm, BOOL_VAL(AS_BOOL(l) != AS_BOOL(r)));
                BREAK;
            }
            CASE(VAL_NUMBER)
            {
                push(vm, BOOL_VAL(AS_NUMBER(l) != AS_NUMBER(r)));
                BREAK;
            }
            CASE(VAL_NIL)
            {
                push(vm, FALSE_VAL);
                BREAK;
            }
            CASE(VAL_OBJ)
            {
                one(vm, l, r);
                BREAK;
            }
        }
#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
#endif
}


/* == */
void veq(VM* vm, Value l, Value r)
{
#if defined(SK_NAN_BOX)
    if(IS_NUMBER(l) && IS_NUMBER(r)) push(vm, BOOL_VAL(AS_NUMBER(l) == AS_NUMBER(r)));
    else oeq(vm, l, r);
#else
        if(l.type != r.type) push(vm, FALSE_VAL);
#if defined(SK_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#undef BREAK
#define BREAK return
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK break
#endif
        DISPATCH(l.type)
        {
            CASE(VAL_BOOL)
            {
                push(vm, BOOL_VAL(AS_BOOL(l) == AS_BOOL(r)));
                BREAK;
            }
            CASE(VAL_NUMBER)
            {
                push(vm, BOOL_VAL(AS_NUMBER(l) == AS_NUMBER(r)));
                BREAK;
            }
            CASE(VAL_NIL)
            {
                push(vm, TRUE_VAL);
                BREAK;
            }
            CASE(VAL_OBJ)
            {
                oeq(vm, l, r);
                BREAK;
            }
        }
#endif
#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
}

// Value less than
void vlt(VM* vm, Value l, Value r)
{
    if(IS_NUMBER(l) && IS_NUMBER(r)) push(vm, BOOL_VAL(AS_NUMBER(l) < AS_NUMBER(r)));
#if defined(SK_OVERLOAD_OPS)
    else olt(vm, l, r);
#else
        else if(IS_STRING(l) && IS_STRING(r))
            push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) < 0));
        else ordererror(vm, l, r);
#endif
}


// Value greater than
void vgt(VM* vm, Value l, Value r)
{
    if(IS_NUMBER(l) && IS_NUMBER(r)) push(vm, BOOL_VAL(AS_NUMBER(l) > AS_NUMBER(r)));
#if defined(SK_OVERLOAD_OPS)
    else ogt(vm, l, r);
#else
        else if(IS_STRING(l) && IS_STRING(r))
            push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) > 0));
        else ordererror(vm, l, r);
#endif
}


// Value less equal
void vle(VM* vm, Value l, Value r)
{
    if(IS_NUMBER(l) && IS_NUMBER(r)) push(vm, BOOL_VAL(AS_NUMBER(l) <= AS_NUMBER(r)));
#if defined(SK_OVERLOAD_OPS)
    else ole(vm, l, r);
#else
        else if(IS_STRING(l) && IS_STRING(r))
            push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) <= 0));
        else ordererror(vm, l, r);
#endif
}


// Value greater equal
void vge(VM* vm, Value l, Value r)
{
    if(IS_NUMBER(l) && IS_NUMBER(r)) push(vm, BOOL_VAL(AS_NUMBER(l) >= AS_NUMBER(r)));
#if defined(SK_OVERLOAD_OPS)
    else oge(vm, l, r);
#else
        else if(IS_STRING(l) && IS_STRING(r))
            push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) >= 0));
        else ordererror(vm, l, r);
#endif
}

/* ---------------------------------------- */ // ordering


const char* dtostr(sk_number n, uint8_t* lenp)
{
    static char buff[SK_NDIGITS];
    if(sk_floor(n) != n) *lenp = snprintf(buff, SK_NDIGITS, "%g", n);
    else *lenp = snprintf(buff, SK_NDIGITS, "%ld", cast(int64_t, n));
    return buff;
}

OString* dtoostr(VM* vm, sk_number n)
{
    uint8_t len;
    const char* buff = dtostr(n, &len);
    return OString_new(vm, buff, len);
}

OString* btostr(VM* vm, int b)
{
    return (b ? vm->faststatic[SS_TRUE] : vm->faststatic[SS_FALSE]);
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
    return dtoostr(vm, AS_NUMBER(value));
boolean:
    return btostr(vm, AS_BOOL(value));
obj:
    return otostr(vm, AS_OBJ(value));
#elif defined(SK_NAN_BOX)
        if(IS_BOOL(value)) return btostr(vm, AS_BOOL(value));
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
#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
#endif
    unreachable;
}


/* Print value */
void vprint(VM* vm, Value value, FILE* stream)
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
    fprintf(stream, "nil");
    return;
number:
    if(sk_floor(AS_NUMBER(value)) != AS_NUMBER(value)) fprintf(stream, "%lg", AS_NUMBER(value));
    else fprintf(stream, "%ld", (int64_t)AS_NUMBER(value));
    return;
boolean:
    fprintf(stream, AS_BOOL(value) ? "true" : "false");
    return;
object:
    oprint(vm, value, stream);
    return;
#elif defined(SK_NAN_BOX)
        if(IS_NIL(value)) printf("nil");
        else if(IS_NUMBER(value)) {
            if(sk_floor(AS_NUMBER(value)) != AS_NUMBER(value)) printf("%lg", AS_NUMBER(value));
            else printf("%ld", (int64_t)AS_NUMBER(value));
        } else if(IS_BOOL(value)) printf(AS_BOOL(value) ? "true" : "false");
        else oprint(vm, value);
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
#define BREAK break
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
            if(sk_floor(AS_NUMBER(value)) != AS_NUMBER(value)) printf("%f", AS_NUMBER(value));
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
#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
#endif // defined(val2tbmask_1)
}


/* Hash value */
Hash vhash(Value value)
{
#if defined(val2tbmask_1)
    static const void* jmptable[] = {
        NULL, // null can't be used for indexing
        &&number,
        &&boolean,
        &&object,

    };
    goto* jmptable[val2tbmask_1(value)];
number:;
    sk_number num = AS_NUMBER(value);
    if(num < 0 || sk_floor(num) != num) return dblhash(num);
    else return cast(Hash, num);
boolean:
    return cast(Hash, AS_BOOL(value));
object:
    return ohash(value);
#elif defined(SK_NAN_BOX)
        if(IS_NUMBER(value)) {
            sk_number num = AS_NUMBER(value);
            if(num < 0 || sk_floor(num) != num) return dblhash(num);
            else return cast(Hash, num);
        } else if(IS_BOOL(value)) return cast(Hash, AS_BOOL(value));
        else return ohash(value);
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
        CASE(VAL_NIL) // only to avoid unused 'label' warning
        {
            unreachable;
        }
        CASE(VAL_BOOL)
        {
            return cast(Hash, AS_BOOL(value));
        }
        CASE(VAL_NUMBER)
        {
            double num = AS_NUMBER(value);
            if(AS_NUMBER(value) < 0 || sk_floor(AS_NUMBER(value)) != AS_NUMBER(value))
                return dblhash(num);
            else return cast(Hash, num);
        }
        CASE(VAL_OBJ)
        {
            return ohash(value);
        }
    }
#endif
#if defined(SKJMPTABLE_H)
#undef SKJMPTABLE_H
#endif
}
