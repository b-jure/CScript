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

#include "skcommon.h"
#include "skconf.h"
#include "skerr.h"
#include "skhashtable.h"
#include "skmath.h"
#include "skmem.h"
#include "skobject.h"
#include "skvalue.h"
#include "stdarg.h"

#include <stdio.h>
#include <stdlib.h>




#define ALLOC_OBJ(vm, object, type) ((object*)onew(vm, sizeof(object), type))

#define ALLOC_NATIVE(vm, upvals)                                                                   \
    ((ONative*)onew(vm, sizeof(ONative) + ((upvals) * sizeof(Value)), OBJ_NATIVE))

#define ALLOC_STRING(vm, len) ((OString*)onew(vm, sizeof(OString) + (len) + 1, OBJ_STRING))



// @TODO: Update 'calloverload()'
/* Call info for overload-able methods */
const Tuple ominfo[] = {
  // {arity, retcnt}
    {0, 1}, // __init__     { args: self             - return: instance }
    {0, 1}, // __tostring__ { args: self             - return: string }
    {1, 1}, // __getidx__   { args: self, idx        - return: value }
    {2, 0}, // __setidx__   { args: self, idx, value - return: none }
    {0, 1}, // __hash__     { args: self             - return: number }
    {0, 0}, // __free__     { args: self             - return: none }
#if defined(SK_OVERLOAD_OPS)
    {2, 1}, // __add__  { args: self, lhs, rhs - return: value }
    {2, 1}, // __sub__  { args: self, lhs, rhs - return: value }
    {2, 1}, // __mul__  { args: self, lhs, rhs - return: value }
    {2, 1}, // __div__  { args: self, lhs, rhs - return: value }
    {2, 1}, // __mod__  { args: self, lhs, rhs - return: value }
    {2, 1}, // __pow__  { args: self, lhs, rhs - return: value }
    {1, 1}, // __not__  { args: self, lhs      - return: value }
    {1, 1}, // __umin__ { args: self, lhs      - return: value }
    {2, 1}, // __ne__   { args: self, lhs, rhs - return: value }
    {2, 1}, // __eq__   { args: self, lhs, rhs - return: value }
    {2, 1}, // __lt__   { args: self, lhs, rhs - return: value }
    {2, 1}, // __le__   { args: self, lhs, rhs - return: value }
    {2, 1}, // __gt__   { args: self, lhs, rhs - return: value }
    {2, 1}, // __ge__   { args: self, lhs, rhs - return: value }
#endif
};



static force_inline O* onew(VM* vm, size_t size, OType type)
{
    O* object = GC_MALLOC(vm, size);
    object->header = cast(uint64_t, vm->objects) | (cast(uint64_t, type) << 56);
    osetmark(object, 0);
    vm->objects = object; // Add object to the GC list
#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for ", (void*)object, size);
    otypeprint(type);
    printf("\n");
#endif
    return object;
}


/* Note: Objects with the same pointer are the same
 * objects in memory... */
int32_t id2omtag(VM* vm, Value id)
{
    uintptr_t ptr = cast(uintptr_t, AS_STRING(id));
    uintptr_t start = cast(uintptr_t, vm->faststatic[SS_INIT]);
    uintptr_t end = cast(uintptr_t, vm->faststatic[SS_DBG]);
    if(ptr < start || ptr >= end) return -1;
    return SS_INIT + (ptr - start);
}


static force_inline OString* OString_alloc(VM* vm, uint32_t len)
{
    OString* string = ALLOC_STRING(vm, len);
    string->len = len;
    return string;
}

OString* OString_new(VM* vm, const char* chars, size_t len)
{
    sk_hash hash = stringhash(chars, len, vm->seed);
    OString* interned = HashTable_get_intern(&vm->weakrefs, chars, len, hash);
    if(interned) return interned; // Return interned string
    OString* string = OString_alloc(vm, len);
    /* According to C standard passing NULL pointer to memcpy
     * is considered undefined behaviour even if the number
     * of bytes to copy were 0. */
    if(len != 0) memcpy(string->storage, chars, len);
    string->storage[len] = '\0';
    string->hash = hash;
    push(vm, OBJ_VAL(string));
    rawset(vm, &vm->weakrefs, OBJ_VAL(string), NIL_VAL);
    pop(vm);
    return string;
}

OString* OString_fmt_from(VM* vm, const char* fmt, va_list argp)
{
    uint8_t c;
    Array_Byte buff;
    const char* fmttype = NULL;
    Array_Byte_init(&buff, vm);
    for(;;) {
        while((c = *fmt++) != '%')
            Array_Byte_push(&buff, c);
        c = *fmt++;
        switch(c) {
            case '%': {
                Array_Byte_push(&buff, '%');
                break;
            }
            case 'd': /* int64_t */
            {
                int64_t n = va_arg(argp, int64_t);
                Array_Byte_ensure(&buff, SK_NDIGITS);
                buff.len += snprintf((char*)&buff.data[buff.len], SK_NDIGITS, "%ld", n);
                break;
            }
            case 'n': /* sk_number (as double) */
            {
                fmttype = "%g";
                goto l_sknum;
            }
            case 'f': /* sk_number (as float) */
            {
                fmttype = "%f";
                goto l_sknum;
            }
            l_sknum: {
                sk_number n = va_arg(argp, sk_number);
                Array_Byte_ensure(&buff, SK_NDIGITS);
                char* s = cast_charp(&buff.data[buff.len]);
                buff.len += snprintf(s, SK_NDIGITS, fmttype, n);
                break;
            }
            case 's': { /* string */
                const char* str = va_arg(argp, char*);
                if(str == NULL) str = "(null)";
                size_t len = strlen(str);
                Array_Byte_ensure(&buff, len);
                memcpy(&buff.data[buff.len], str, len);
                buff.len += len;
                break;
            }
            case 'c': /* char */
            {
                int8_t c = va_arg(argp, int);
                Array_Byte_push(&buff, c);
                break;
            }
            case 'p': /* pointer */
            {
                Array_Byte_ensure(&buff, 11);
                void* ptr = va_arg(argp, void*);
                snprintf((char*)&buff.data[buff.len], 11, "%p", ptr);
                break;
            }
            default: /* invalid format specifier */
            {
                Array_Byte_free(&buff, NULL);
                ofmterror(vm, c, *last_frame(vm).callee);
            }
        }
    }
    OString* fstr = OString_new(vm, (char*)buff.data, buff.len);
    Array_Byte_free(&buff, NULL);
    return fstr;
}

OString* OString_fmt(VM* vm, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    OString* str = OString_fmt_from(vm, fmt, argp);
    va_end(argp);
    return str;
}

static force_inline void OString_free(VM* vm, OString* string)
{
    GC_FREE(vm, string, sizeof(OString) + string->len + 1);
}


OString* concatenate(VM* vm, Value l, Value r)
{
    OString* left = AS_STRING(l);
    OString* right = AS_STRING(r);
    size_t length = left->len + right->len;
    char buffer[length + 1];
    memcpy(buffer, left->storage, left->len);
    memcpy(buffer + left->len, right->storage, right->len);
    buffer[length] = '\0';
    OString* string = OString_new(vm, buffer, length);
    return string;
}


ONative*
ONative_new(VM* vm, OString* name, sk_cfunc fn, int32_t arity, uint8_t isvararg, uint32_t upvalc)
{
    ONative* native = ALLOC_NATIVE(vm, upvalc);
    native->fn = fn;
    FnPrototype* p = &native->p;
    p->name = (name ? name : vm->faststatic[SS_UNKNOWN]);
    p->source = vm->faststatic[SS_CSRC];
    p->arity = arity;
    p->isvararg = isvararg;
    p->upvalc = upvalc;
    p->defline = -1;
    p->deflastline = -1;
    for(int32_t i = 0; i < upvalc; i++)
        native->upvalue[i] = NIL_VAL;
    return native;
}

static force_inline void ONative_free(VM* vm, ONative* native)
{
    GC_FREE(vm, native, sizeof(ONative) + (native->p.upvalc * sizeof(Value)));
}




OFunction* OFunction_new(VM* vm)
{
    OFunction* fn = ALLOC_OBJ(vm, OFunction, OBJ_FUNCTION);
    FnPrototype* p = &fn->p;
    memset(p, 0, sizeof(FnPrototype));
    Chunk_init(&fn->chunk, vm);
    return fn;
}

static force_inline void ObjFunction_free(VM* vm, OFunction* fn)
{
    Chunk_free(&fn->chunk);
    GC_FREE(vm, fn, sizeof(OFunction));
}

static force_inline void fnprint(OFunction* fn, FILE* stream)
{
    if(unlikely(fn->p.name == NULL))
        fprintf(stream, "<script-fn %s>: %p", fn->p.source->storage, fn);
    else fprintf(stream, "<fn %s>: %p", fn->p.name->storage, fn);
}





OClosure* OClosure_new(VM* vm, OFunction* fn)
{
    FnPrototype* p = &fn->p;
    OUpvalue** upvals = GC_MALLOC(vm, sizeof(OUpvalue*) * p->upvalc);
    for(uint32_t i = 0; i < p->upvalc; i++)
        upvals[i] = NULL;
    OClosure* closure = ALLOC_OBJ(vm, OClosure, OBJ_CLOSURE);
    closure->fn = fn;
    closure->upvalue = upvals;
    return closure;
}

static force_inline void OClosure_free(VM* vm, OClosure* closure)
{
    GC_FREE(vm, closure->upvalue, closure->fn->p.upvalc * sizeof(OUpvalue*));
    GC_FREE(vm, closure, sizeof(OClosure));
}




OUpvalue* OUpvalue_new(VM* vm, Value* valp)
{
    OUpvalue* upval = ALLOC_OBJ(vm, OUpvalue, OBJ_UPVAL);
    upval->closed = EMPTY_VAL;
    upval->location = valp;
    upval->next = NULL;
    return upval;
}

static force_inline void OUpvalue_free(VM* vm, OUpvalue* upval)
{
    GC_FREE(vm, upval, sizeof(OUpvalue));
}




OClass* OClass_new(VM* vm, OString* name)
{
    OClass* oclass = ALLOC_OBJ(vm, OClass, OBJ_CLASS);
    oclass->name = name;
    HashTable_init(&oclass->methods);
    memset(oclass->omethods, 0, sizeof(oclass->omethods) / sizeof(oclass->omethods[0]));
    return oclass;
}

static force_inline void OClass_free(VM* vm, OClass* oclass)
{
    HashTable_free(vm, &oclass->methods);
    GC_FREE(vm, oclass, sizeof(OClass));
}




OInstance* OInstance_new(VM* vm, OClass* oclass)
{
    OInstance* instance = ALLOC_OBJ(vm, OInstance, OBJ_INSTANCE);
    instance->oclass = oclass;
    HashTable_init(&instance->fields);
    return instance;
}

/* Return overloaded method or NULL if value
 * is not an instance value or method is not overloaded. */
static force_inline O* getomethod(VM* vm, Value val, sk_om om)
{
    if(IS_INSTANCE(val)) return AS_INSTANCE(val)->oclass->omethods[om];
    return NULL;
}


static force_inline void OInstance_free(VM* vm, OInstance* instance)
{
    HashTable_free(vm, &instance->fields);
    GC_FREE(vm, instance, sizeof(OInstance));
}




OBoundMethod* OBoundMethod_new(VM* vm, Value receiver, OClosure* method)
{
    OBoundMethod* bound_method = ALLOC_OBJ(vm, OBoundMethod, OBJ_BOUND_METHOD);
    bound_method->receiver = receiver;
    bound_method->method = method;
    return bound_method;
}

static force_inline void OBoundMethod_free(VM* vm, OBoundMethod* bound_method)
{
    GC_FREE(vm, bound_method, sizeof(OBoundMethod));
}





/* Debug */
sdebug void otypeprint(OType type)
{
    switch(type) {
        case OBJ_STRING: printf("OBJ_STRING"); break;
        case OBJ_FUNCTION: printf("OBJ_FUNCTION"); break;
        case OBJ_CLOSURE: printf("OBJ_CLOSURE"); break;
        case OBJ_NATIVE: printf("OBJ_NATIVE"); break;
        case OBJ_UPVAL: printf("OBJ_UPVAL"); break;
        case OBJ_CLASS: printf("OBJ_CLASS"); break;
        case OBJ_INSTANCE: printf("OBJ_INSTANCE"); break;
        case OBJ_BOUND_METHOD: printf("OBJ_BOUND_METHOD"); break;
        default: unreachable;
    }
}


/* Hash object value */
sk_hash ohash(VM* vm, Value value, uint8_t raw)
{
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING: return AS_STRING(value)->hash;
        default: {
            sk_hash hash = 0;
            if(!raw && calloverload(vm, value, OM_HASH)) {
                Value result = *stackpeek(0);
                if(!IS_NUMBER(result)) omreterror(vm, "number", OM_HASH);
                hash = sk_floor(AS_NUMBER(*stackpeek(0)));
            } else hash = ptrhash(cast(const void*, AS_OBJ(value)));
            return hash;
        }
    }
}


/* Free object memory, invokes '__free__' if defined. */
void ofree(VM* vm, O* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    otypeprint(otype(object));
    printf("\n");
#endif
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "skjmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK return
#endif
    DISPATCH(otype(object))
    {
        CASE(OBJ_STRING)
        {
            OString_free(vm, cast(OString*, object));
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            ObjFunction_free(vm, cast(OFunction*, object));
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            OClosure_free(vm, cast(OClosure*, object));
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ONative_free(vm, cast(ONative*, object));
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            OUpvalue_free(vm, cast(OUpvalue*, object));
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            OClass_free(vm, cast(OClass*, object));
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            OInstance* instance = cast(OInstance*, object);
            calloverload(vm, OBJ_VAL(instance), OM_FREE); // try call '__free__'
            OInstance_free(vm, instance);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            OBoundMethod_free(vm, cast(OBoundMethod*, object));
            BREAK;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}








/* =============== raw access =============== */

static force_inline uint8_t
rawgetproperty(VM* vm, OInstance* instance, Value key, Value* out, uint8_t what)
{
    HashTable* table = rawgettable(vm, instance, what);
    return rawget(vm, table, key, out);
}

static force_inline uint8_t
rawsetproperty(VM* vm, OInstance* instance, Value key, Value value, uint8_t what)
{
    HashTable* table = rawgettable(vm, instance, what);
    return rawset(vm, table, key, value);
}


/* Perform raw index access on instance object.
 * 'what' determines if we are getting or setting the indexed value.
 * Note: Invokes runtime error if the 'index' value is 'nil'. */
uint8_t rawindex(VM* vm, Value value, uint8_t what)
{
    OInstance* instance = AS_INSTANCE(value);
    Value idx, out;
    uint8_t res = 0;
    if(what == SK_RAWSET) {
        Value rhs = *stackpeek(0); // rhs
        idx = *stackpeek(1);
        if(unlikely(IS_NIL(idx))) nilidxerror(vm);
        rawsetproperty(vm, instance, idx, rhs, 0);
        popn(vm, 2); // pop [index] and [rhs]
        res = 1;
    } else {
        Value* idxstk = stackpeek(0);
        if(unlikely(IS_NIL(idx))) nilidxerror(vm);
        if(rawgetproperty(vm, instance, idx, &out, 0) || rawgetproperty(vm, instance, idx, &out, 1))
        {
            *idxstk = out; // replace [index] with property
            res = 1;
        }
    }
    return res;
}

/* ---------------------------------------------------- */






/* =============== call overload-able methods =============== */

uint8_t calloverload(VM* vm, Value instance, sk_om tag)
{
    O* const fn = getomethod(vm, instance, tag);
    if(fn) {
        Value* const retstart = vm->sp;
        int32_t const retcnt = ominfo[tag].retcnt;
        int32_t const arity = ominfo[tag].arity;
        push(vm, instance); // push 'self'
        for(int32_t i = 0; i < arity; i++) // push args
            push(vm, *stackpeek(arity - 1));
        ncall(vm, retstart, OBJ_VAL(fn), retcnt);
        return 1;
    }
    return 0;
}

/* ---------------------------------------------------- */






#if defined(SK_OVERLOAD_OPS)


/* =============== operator overloading =============== */

/* Tries to call class overloaded unary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
static force_inline int callunop(VM* vm, Value lhs, sk_om op, Value* res)
{
    O* om = getomethod(vm, lhs, op);
    if(om == NULL) return 0;
    Value* retstart = vm->sp;
    push(vm, lhs); // 'self'
    push(vm, lhs);
    ncall(vm, retstart, OBJ_VAL(om), ominfo[op].retcnt);
    *res = pop(vm); // assign and pop the method result
    return 1;
}


/* Tries to call class overloaded binary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
static force_inline int callbinop(VM* vm, Value lhs, Value rhs, sk_om op, Value* res)
{
    Value instance;
    O* om = getomethod(vm, lhs, op);
    if(om == NULL) {
        om = getomethod(vm, rhs, op);
        if(om == NULL) return 0;
        instance = rhs;
    } else instance = lhs;
    Value* retstart = vm->sp;
    push(vm, instance); // 'self'
    push(vm, lhs);
    push(vm, rhs);
    ncall(vm, retstart, OBJ_VAL(om), ominfo[op].retcnt);
    *res = pop(vm); // assign and pop the method result
    return 1;
}


/* Tries calling binary or unary overloaded operator method, errors on failure. */
void otryop(VM* vm, Value lhs, Value rhs, sk_om op, Value* res)
{
    if(!omisunop(op)) {
        if(unlikely(!callbinop(vm, lhs, rhs, op, res))) binoperror(vm, lhs, rhs, op - OM_ADD);
    } else if(unlikely(!callunop(vm, lhs, op, res))) unoperror(vm, lhs, op - OM_ADD);
}

/* ---------------------------------------------------- */





/* =============== ordering =============== */

static force_inline int omcallorder(VM* vm, Value lhs, Value rhs, sk_om ordop)
{
    sk_assert(vm, ordop >= OM_NE && ordop <= OM_GE, "invalid sk_om for order");
    if(callbinop(vm, lhs, rhs, ordop, stackpeek(1))) { // try overload
        pop(vm); // remove second operand
        return 1;
    }
    // Instances (and Skooma objects) can always have equality comparison.
    // If their pointers are the same then the underlying objects are equal;
    // otherwise they are not equal.
    if(unlikely(ordop != OM_EQ && ordop != OM_NE)) ordererror(vm, lhs, rhs);
    return 0;
}

/* != */
void one(VM* vm, Value lhs, Value rhs)
{
    if(IS_STRING(lhs) && IS_STRING(rhs)) push(vm, BOOL_VAL(lhs != rhs));
    else if(!omcallorder(vm, lhs, rhs, OM_NE)) push(vm, BOOL_VAL(lhs != rhs));
}

/* == */
void oeq(VM* vm, Value lhs, Value rhs)
{
    if(IS_STRING(lhs) && IS_STRING(rhs)) push(vm, BOOL_VAL(lhs == rhs));
    else if(!omcallorder(vm, lhs, rhs, OM_EQ)) push(vm, BOOL_VAL(lhs == rhs));
}

/* < */
void olt(VM* vm, Value lhs, Value rhs)
{
    if(IS_STRING(lhs) && IS_STRING(rhs))
        push(vm, BOOL_VAL(strcmp(AS_CSTRING(lhs), AS_CSTRING(rhs)) < 0));
    else omcallorder(vm, lhs, rhs, OM_LT);
}

/* > */
void ogt(VM* vm, Value lhs, Value rhs)
{
    if(IS_STRING(lhs) && IS_STRING(rhs))
        push(vm, BOOL_VAL(strcmp(AS_CSTRING(lhs), AS_CSTRING(rhs)) > 0));
    else omcallorder(vm, lhs, rhs, OM_GT);
}

/* <= */
void ole(VM* vm, Value lhs, Value rhs)
{
    if(IS_STRING(lhs) && IS_STRING(rhs))
        push(vm, BOOL_VAL(strcmp(AS_CSTRING(lhs), AS_CSTRING(rhs)) <= 0));
    else omcallorder(vm, lhs, rhs, OM_LE);
}

/* >= */
void oge(VM* vm, Value lhs, Value rhs)
{
    if(IS_STRING(lhs) && IS_STRING(rhs))
        push(vm, BOOL_VAL(strcmp(AS_CSTRING(lhs), AS_CSTRING(rhs)) >= 0));
    else omcallorder(vm, lhs, rhs, OM_GE);
}

/* ---------------------------------------------------- */

#endif // if defined(SK_OVERLOAD_OPS)






/* Get object string from 'object' */
OString* otostr(VM* vm, Value* ostk, Value oval, uint8_t raw)
{
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "skjmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#endif
    OString* ostr = NULL;
    DISPATCH(otype(AS_OBJ(*ostk)))
    {
        CASE(OBJ_STRING)
        {
            ostr = AS_STRING(oval);
            goto l_ret;
        }
        CASE(OBJ_FUNCTION)
        {
            ostr = AS_FUNCTION(oval)->p.name;
            goto l_ret;
        }
        CASE(OBJ_CLOSURE)
        {
            ostr = AS_CLOSURE(oval)->fn->p.name;
            goto l_ret;
        }
        CASE(OBJ_NATIVE)
        {
            ostr = AS_NATIVE(oval)->p.name;
            goto l_ret;
        }
        CASE(OBJ_UPVAL)
        { // recursion
            return vtostr(vm, ostk, AS_UPVAL(oval)->closed, raw);
        }
        CASE(OBJ_CLASS)
        {
            ostr = AS_CLASS(oval)->name;
            goto l_ret;
        }
        CASE(OBJ_INSTANCE)
        {
            if(!raw && calloverload(vm, oval, OM_TOSTRING)) {
                Value result = *stackpeek(0);
                if(!IS_STRING(result)) omreterror(vm, "string", OM_TOSTRING);
                *ostk = pop(vm);
                return AS_STRING(result);
            } else {
                OInstance* instance = AS_INSTANCE(oval);
                Value debug;
                Value key = OBJ_VAL(vm->faststatic[SS_DBG]);
                if(rawget(vm, &instance->fields, key, &debug) && IS_STRING(debug)) {
                    *ostk = debug;
                    return AS_STRING(debug); // have debug name
                }
                return AS_STRING(*ostk = OBJ_VAL(instance->oclass->name));
            }
        }
        CASE(OBJ_BOUND_METHOD)
        {
            ostr = AS_BOUND_METHOD(oval)->method->fn->p.name;
            goto l_ret;
        }
    }
l_ret:
    *ostk = OBJ_VAL(ostr);
    return ostr;

#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}


/* Print the object value */
void oprint(VM* vm, Value value, uint8_t raw, FILE* stream)
{
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "skjmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK return
#endif
    DISPATCH(OBJ_TYPE(value))
    {
        CASE(OBJ_STRING)
        {
            fprintf(stream, "%s", AS_CSTRING(value));
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            fnprint(AS_FUNCTION(value), stream);
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            fnprint(AS_CLOSURE(value)->fn, stream);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            fprintf(stream, "<native-fn %s>", AS_NATIVE(value)->p.name->storage);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        { // recursion
            OUpvalue* upval = AS_UPVAL(value);
            vprint(vm, *upval->location, raw, stream);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            fprintf(stream, "%p-%s", AS_OBJ(value), AS_CLASS(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            if(!raw && calloverload(vm, value, OM_TOSTRING)) { // have '__tostring__' ?
                Value result = *stackpeek(0);
                if(!IS_STRING(result)) omreterror(vm, "string", OM_TOSTRING);
                fprintf(stream, "%s", AS_CSTRING(*stackpeek(0)));
                pop(vm); // pop result
            } else { // no overload
                OInstance* instance = AS_INSTANCE(value);
                Value debug;
                Value key = OBJ_VAL(vm->faststatic[SS_DBG]);
                if(rawget(vm, &instance->fields, key, &debug) && IS_STRING(debug)) {
                    fprintf(stream, "%s", AS_CSTRING(debug)); // __debug field
                } else { // default print
                    fprintf(
                        stream,
                        "%p-%s instance",
                        AS_OBJ(value),
                        AS_INSTANCE(value)->oclass->name->storage);
                }
            }
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            fnprint(AS_BOUND_METHOD(value)->method->fn, stream);
            BREAK;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}



const InternedString static_strings[] = {
  /* Value types */
    {"nil",                SSS("nil")               },
    {"number",             SSS("number")            },
    {"string",             SSS("string")            },
    {"bool",               SSS("bool")              },
    {"class",              SSS("class")             },
    {"instance",           SSS("instance")          },
    {"function",           SSS("function")          },
    {"closure",            SSS("closure")           },
    {"native",             SSS("native")            },
    {"upvalue",            SSS("upvalue")           },
    {"method",             SSS("method")            },
 /* Boolean strings */
    {"true",               SSS("true")              },
    {"false",              SSS("false")             },
 /* Class overload-able method names. */
    {"__init__",           SSS("__init__")          },
    {"__tostring__",       SSS("__tostring__")      },
    {"__getidx__",         SSS("__getidx__")        },
    {"__setidx__",         SSS("__setidx__")        },
    {"__hash__",           SSS("__hash__")          },
    {"__free__",           SSS("__free__")          },
#if defined(SK_OVERLOAD_OPS)  // operator overloading enabled?
  /* Overload-able arithmetic operators */
    {"__add__",            SSS("__add__")           },
    {"__sub__",            SSS("__sub__")           },
    {"__mul__",            SSS("__mul__")           },
    {"__div__",            SSS("__div__")           },
    {"__mod__",            SSS("__mod__")           },
    {"__pow__",            SSS("__pow__")           },
    {"__not__",            SSS("__not__")           },
    {"__umin__",           SSS("__umin__")          },
 /* Overload-able ordering operators */
    {"__ne__",             SSS("__ne__")            },
    {"__eq__",             SSS("__eq__")            },
    {"__lt__",             SSS("__lt__")            },
    {"__le__",             SSS("__le__")            },
    {"__gt__",             SSS("__gt__")            },
    {"__ge__",             SSS("__ge__")            },
#endif
  /* Class special field names. */
    {"__debug",            SSS("__debug")           },
 /* Operator strings */
    {"addition [+]",       SSS("addition [+]")      },
    {"subtraction [-]",    SSS("subtraction [-]")   },
    {"multiplication [*]", SSS("multiplication [*]")},
    {"division [/]",       SSS("division [/]")      },
    {"modulo [%]",         SSS("modulo [%]")        },
    {"exponentiation [^]", SSS("exponentiation [^]")},
    {"not [!]",            SSS("not [!]")           },
    {"negation [-]",       SSS("negation [-]")      },
    {"ne [!=]",            SSS("ne [!=]")           },
    {"eq [==]",            SSS("eq [==]")           },
    {"lt [<]",             SSS("lt [<]")            },
    {"le [<=]",            SSS("le [<=]")           },
    {"gt [>]",             SSS("gt [>]")            },
    {"ge [>=]",            SSS("ge [>=]")           },
    {"and [and]",          SSS("and [and]")         },
    {"or [or]",            SSS("or [or]")           },
 /* Other statics */
    {"?",                  SSS("?")                 },
    {"[C]",                SSS("[C]")               },
};
