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

#include "skapi.h"
#include "skchunk.h"
#include "skcommon.h"
#include "skdebug.h"
#include "skerr.h"
#include "sklimits.h"
#include "skmem.h"
#include "skobject.h"
#include "skparser.h"
#include "skvalue.h"
#include "skvm.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>



#define CALL_SKOOMAFN 0
#define CALL_NATIVEFN 1
#define CALL_CLASS 2


volatile uint8_t runtime = 0; // VM is running?



/* Push the value on the stack */
void push(VM* vm, Value val)
{
    if(likely(cast_int(vm->sp - vm->stack) < VM_STACK_LIMIT)) *vm->sp++ = val;
    else sovferror(vm);
}

/* Bind class method in order to preserve the receiver.
 * By doing so the interpreter can then push the receiver on the
 * stack before running the function.
 * This is needed because class methods expect the receiver to be
 * the first argument ('self' automatic var). */
uint8_t bindmethod(VM* vm, OClass* oclass, Value name, Value receiver)
{
    Value method;
    if(!tableget(vm, &oclass->methods, name, &method)) return 0;
    *stackpeek(0) = OBJ_VAL(OBoundMethod_new(vm, receiver, AS_CLOSURE(method)));
    return 1;
}


/* Adjust return values after native call finishes. */
static force_inline void moveresults(VM* vm, Value* fn, int32_t got, int32_t expect)
{
    Value* retstart = vm->sp - got; // start of return values
    if(expect == 0) expect = got; // all results (MULRET)
    if(got > expect) got = expect; // remove extra results
    memcpy(fn, retstart, got); // Safety: 'retstart' >= 'nativefn'
    for(int32_t i = got; i < expect; i++) // replace missing values with nil
        fn[i] = NIL_VAL;
    vm->sp = fn + expect;
}


/* Call native function. */
static force_inline int32_t callnative(VM* vm, Value fn)
{
    sk_unlock(vm);
    int32_t n = AS_NATIVE(fn)->fn(vm);
    sk_lock(vm);
    skapi_checkelems(vm, n);
    CallFrame* f = &last_frame(vm);
    moveresults(vm, f->callee, n, f->retcnt);
    vm->fc--; // pop frame
    return n;
}



/* Calls have lots of checks and there are two main reasons why.
 * Skooma does not allow extra arguments if the function does not
 * accept variable number of arguments and you are not allowed to
 * provide less arguments than the function arity.
 * In case 'callee' is a Skooma closure then just return the new 'CallFrame',
 * otherwise run the C closure and return NULL. */
static CallFrame* precall(VM* vm, Value callee, int32_t argc, int32_t retcnt)
{
    FnPrototype* p = NULL;
    CallFrame* frame = &vm->frames[vm->fc];
    if(!IS_NATIVE(callee)) {
        OClosure* closure = AS_CLOSURE(callee);
        OFunction* fn = closure->fn;
        p = &fn->p;
        frame->closure = closure;
        frame->ip = fn->chunk.code.data;
        frame->cfinfo = 0;
        retcnt = (retcnt == SK_MULRET ? 0 : retcnt); // adjust return count
    } else {
        ONative* native = AS_NATIVE(callee);
        p = &native->p;
        frame->closure = NULL;
        frame->ip = NULL;
        frame->cfinfo = CFI_CCALL;
    }
#if defined(callbitmask)
    const static void* jmptable[] = {
        &&l_stack_overflow,
        &&l_invalid_argc,
        &&l_callstack_overflow,
        &&l_ok,
    };
    uint32_t bitmask = callbitmask(vm, p->isvararg, p->arity, argc, retcnt);
    uint8_t idx = sk_ctz(bitmask);
    goto* jmptable[idx];
l_stack_overflow:
    retovferror(vm, p->name->storage);
l_invalid_argc:
    arityerror(vm, p->arity, argc);
l_callstack_overflow:
    fcovferror(vm);
#else
    if(unlikely(!sk_ensurestack(vm, retcnt))) {
        retovferror(vm, p->name->storage);
    } else if(unlikely((p->isvararg && p->arity > argc) || (!p->isvararg && p->arity != argc))) {
        arityerror(vm, p->arity, argc);
    } else if(unlikely(vm->fc == VM_CALLSTACK_LIMIT)) {
        fcovferror(vm);
    } else goto ok;
#endif
l_ok:
    frame->vacnt = argc - p->arity;
    frame->retcnt = retcnt;
    frame->callee = vm->sp - argc - 1;
    vm->fc++;
    if(frame->cfinfo & CFI_CCALL) {
        callnative(vm, callee);
        return NULL;
    } else return &last_frame(vm);
}

/* Call '()' a value (closure, method, class). */
static CallFrame* call(VM* vm, Value callee, int32_t argc, int32_t retcnt)
{
    if(unlikely(!IS_OBJ(callee))) callerror(vm, callee);
    switch(OBJ_TYPE(callee)) {
        case OBJ_BOUND_METHOD: {
            OBoundMethod* bound = AS_BOUND_METHOD(callee);
            vm->sp[-argc - 1] = bound->receiver; // class instance (self)
            return precall(vm, OBJ_VAL(bound->method), argc, retcnt);
        }
        case OBJ_CLASS: {
            OClass* oclass = AS_CLASS(callee);
            Value instance = OBJ_VAL(OInstance_new(vm, oclass));
            if(!calloverload(vm, instance, OM_INIT)) { // not overloaded ?
                int32_t arity = ominfo[OM_INIT].arity; // default arity
                if(unlikely(argc != arity)) arityerror(vm, arity, argc);
                *stackpeek(argc) = instance; // replace class with instance
            }
            return NULL;
        }
        case OBJ_CLOSURE:
        case OBJ_FUNCTION:
        case OBJ_NATIVE: return precall(vm, callee, argc, retcnt);
        default: unreachable;
    }
}

/* Public interface for normal call (unprotected). */
void ncall(VM* vm, Value* retstart, Value fn, int32_t retcnt)
{
    int32_t argc = vm->sp - retstart - 1;
    if(call(vm, fn, argc, retcnt) != NULL) run(vm);
}


/* Protected call with longjmp.
 * Performs a protected call, calling the wrapper 'ProtectedFn' around
 * a Skooma function or native C function.
 * Returns status of the called function, this status is modified
 * by function that errors and performs the long jump or it
 * stays unchanged and the wrapper function just returns and
 * execution continues. */
static force_inline int32_t protectedcall(VM* vm, ProtectedFn fn, void* userdata)
{
    int32_t oldfc = vm->fc;
    struct sk_longjmp lj;
    lj.status = S_OK;
    lj.prev = vm->errjmp;
    vm->errjmp = &lj;
    if(setjmp(lj.buf) == 0) // setter ?
        (*fn)(vm, userdata); // perform the call
    vm->errjmp = lj.prev;
    vm->fc = oldfc;
    return lj.status;
}

/* Public interface to 'protectedcall'.
 * In case of errors it performs a recovery by closing all
 * open upvalues (values to be closed) and restoring the
 * old stack pointer (oldtop). */
int32_t pcall(VM* vm, ProtectedFn fn, void* userdata, ptrdiff_t oldtop)
{
    int8_t status = protectedcall(vm, fn, userdata);
    if(unlikely(status != S_OK)) {
        closeupval(vm, vm->sp);
        Value* oldsp = restore_stack(vm, oldtop);
        *oldsp = vm->sp[-1];
        vm->sp = oldsp + 1;
    }
    return status;
}


/* Private to the interpreter.
 * Tries to call the superclass method 'name'. */
static force_inline void
invokefrom(VM* vm, OClass* oclass, Value name, int32_t argc, int32_t retcnt)
{
    Value method;
    if(unlikely(!rawget(vm, &oclass->methods, name, &method))) udperror(vm, name, oclass);
    call(vm, method, argc, retcnt);
}


/* Check if receiver and key are valid for indexing. */
static force_inline void checkindex(VM* vm, Value receiver, Value key)
{
    if(unlikely(!IS_INSTANCE(receiver))) ipaerror(vm, receiver);
    else if(unlikely(IS_NIL(key))) nilidxerror(vm);
}


/* Private to interpreter.
 * Invokes the field/method of the instance class directly in a single
 * instruction.
 * First it tries to find the field with the 'name', if it was not
 * found it calls the method of its own class or errors if the method
 * was not found. */
static force_inline void invoke(VM* vm, Value name, int32_t argc, int32_t retcnt)
{
    Value receiver = *stackpeek(argc);
    if(unlikely(!IS_INSTANCE(receiver))) ipaerror(vm, receiver);
    OInstance* instance = AS_INSTANCE(receiver);
    Value field;
    if(rawget(vm, &instance->fields, name, &field)) {
        *stackpeek(argc) = field; // swap receiver with field
        call(vm, field, argc, retcnt);
    }
    invokefrom(vm, instance->oclass, name, argc, retcnt);
}

/* Private to interpreter.
 * Used when creating a skooma closure. */
static force_inline OUpvalue* captureupval(VM* vm, Value* valp)
{
    OUpvalue** upvalpp = &vm->open_upvals;
    while(*upvalpp != NULL && (*upvalpp)->location > valp)
        upvalpp = &(*upvalpp)->next;
    if(*upvalpp != NULL && (*upvalpp)->location == valp) return *upvalpp;
    OUpvalue* upvalp = OUpvalue_new(vm, valp);
    upvalp->next = *upvalpp;
    *upvalpp = upvalp;
    return upvalp;
}

/* Closes all of the captured variables moving
 * them from the stack onto the heap (open_upvals array),
 * making them reachable for gc. */
void closeupval(VM* vm, Value* last)
{
    while(vm->open_upvals != NULL && vm->open_upvals->location >= last) {
        OUpvalue* upvalp = vm->open_upvals;
        upvalp->closed = *upvalp->location;
        upvalp->location = &upvalp->closed;
        vm->open_upvals = upvalp->next;
    }
}

/**
 * Searches the entire table for the matching index in order to
 * provide more descriptive runtime error.
 * It is okay if the lookup is slow, this only gets called when runtime error
 * occurs (which is follows the end of the program execution).
 **/
static force_inline OString* globalname(VM* vm, uint32_t idx)
{
    for(uint32_t i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];
        if(!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
            return (OString*)AS_OBJ(entry->key);
    }
    unreachable;
}


void run(VM* vm)
{
#define saveip() (frame->ip = ip)
#define updatestate() (frame = &last_frame(vm), ip = frame->ip)
#define throwerr(vm) runerror(vm, vtostr(vm, *stackpeek(0))->storage)
#define READ_BYTE() (*ip++)
#define READ_BYTEL() (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT() FFN(frame)->chunk.constants.data[READ_BYTEL()]
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define bcstart() (FFN(frame).chunk.code.data)
#define ipinbounds() (ip - bcstart() < VM_STACK_LIMIT && ip >= bcstart())
#define BINARY_OP(vm, op)                                                                          \
    do {                                                                                           \
        saveip();                                                                                  \
        Value* l = stackpeek(1);                                                                   \
        Value r = *stackpeek(0);                                                                   \
        arith(vm, *l, r, op, l);                                                                   \
    } while(0)
#define UNARY_OP(vm, op)                                                                           \
    do {                                                                                           \
        saveip();                                                                                  \
        Value* l = stackpeek(0);                                                                   \
        arith(vm, *l, NIL_VAL, op, l);                                                             \
    } while(0)
#define ORDER_OP(vm, fnop)                                                                         \
    do {                                                                                           \
        saveip();                                                                                  \
        Value l = *stackpeek(1);                                                                   \
        Value r = *stackpeek(0);                                                                   \
        fnop(vm, l, r);                                                                            \
    } while(0)


    last_frame(vm).cfinfo = CFI_FRESH; // mark as fresh execute of Skooma script
    runtime = 1;
    register CallFrame* frame = &vm->frames[vm->fc - 1];
    register uint8_t* ip = frame->ip;
#ifdef DEBUG_TRACE_EXECUTION
    printf("\n=== VM - execution ===\n");
#endif
    for(;;) {
#ifdef SK_PRECOMPUTED_GOTO
#define OP_TABLE
#include "skjmptable.h"
#undef OP_TABLE
#ifdef DEBUG_TRACE_EXECUTION
#undef BREAK
#define BREAK                                                                                      \
    dumpstack(vm, frame, ip);                                                                      \
    DISPATCH(READ_BYTE())
#endif
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#ifdef DEBUG_TRACE_EXECUTION
#define BREAK                                                                                      \
    dumpstack(vm, frame, ip);                                                                      \
    break
#else
#define BREAK break
#endif
#endif
        DISPATCH(READ_BYTE())
        {
            CASE(OP_TRUE)
            {
                saveip();
                push(vm, BOOL_VAL(1));
                BREAK;
            }
            CASE(OP_FALSE)
            {
                saveip();
                push(vm, BOOL_VAL(0));
                BREAK;
            }
            CASE(OP_NIL)
            {
                saveip();
                push(vm, NIL_VAL);
                BREAK;
            }
            CASE(OP_NILN)
            {
                saveip();
                pushn(vm, READ_BYTEL(), NIL_VAL);
                BREAK;
            }
            CASE(OP_ADD)
            {
                BINARY_OP(vm, AR_ADD);
                BREAK;
            }
            CASE(OP_SUB)
            {
                BINARY_OP(vm, AR_SUB);
                BREAK;
            }
            CASE(OP_MUL)
            {
                BINARY_OP(vm, AR_MUL);
                BREAK;
            }
            CASE(OP_MOD)
            {
                BINARY_OP(vm, AR_MOD);
                BREAK;
            }
            CASE(OP_POW)
            {
                BINARY_OP(vm, AR_POW);
                BREAK;
            }
            CASE(OP_DIV)
            {
                BINARY_OP(vm, AR_DIV);
                BREAK;
            }
            CASE(OP_NEG)
            {
                UNARY_OP(vm, AR_UMIN);
                BREAK;
            }
            CASE(OP_NOT)
            {
                UNARY_OP(vm, AR_NOT);
                BREAK;
            }
            CASE(OP_VALIST)
            {
                saveip();
                OFunction* fn = FFN(frame);
                uint32_t vacnt = READ_BYTEL();
                if(vacnt == 0) vacnt = frame->vacnt;
                for(uint32_t i = 1; i <= vacnt; i++) {
                    Value* next = frame->callee + fn->p.arity + i;
                    push(vm, *next);
                }
                BREAK;
            }
            CASE(OP_NOT_EQUAL)
            {
                ORDER_OP(vm, vne);
                BREAK;
            }
            CASE(OP_EQUAL)
            {
                ORDER_OP(vm, veq);
                BREAK;
            }
            CASE(OP_EQ) // same as OP_EQUAL except we don't pop the first operand
            {
                ORDER_OP(vm, eq_preserveL);
                BREAK;
            }
            CASE(OP_GREATER)
            {
                ORDER_OP(vm, vgt);
                BREAK;
            }
            CASE(OP_GREATER_EQUAL)
            {
                ORDER_OP(vm, vge);
                BREAK;
            }
            CASE(OP_LESS)
            {
                ORDER_OP(vm, vle);
                BREAK;
            }
            CASE(OP_LESS_EQUAL)
            {
                ORDER_OP(vm, vle);
                BREAK;
            }
            CASE(OP_POP)
            {
                pop(vm);
                BREAK;
            }
            CASE(OP_POPN)
            {
                popn(vm, READ_BYTEL());
                BREAK;
            }
            CASE(OP_CONST)
            {
                saveip();
                push(vm, READ_CONSTANT());
                BREAK;
            }
            {
                int32_t argc; // shared
                CASE(OP_CALL0)
                {
                    argc = 0;
                    goto l_call;
                }
                CASE(OP_CALL1)
                {
                    argc = 1;
                    goto l_call;
                }
                CASE(OP_CALL)
                {
                    argc = cast(int32_t, vm->sp - Array_VRef_pop(&vm->callstart));
                l_call:;
                    {
                        int32_t retcnt = READ_BYTEL();
                        Value callee = *stackpeek(argc);
                        saveip();
                        call(vm, callee, argc, retcnt);
                        updatestate();
                        BREAK;
                    }
                }
            }
            CASE(OP_METHOD)
            {
                Value methodname = READ_CONSTANT();
                Value method = *stackpeek(0); // OClosure or ONative
                OClass* oclass = AS_CLASS(*stackpeek(1));
                rawset(vm, &oclass->methods, methodname, method);
                pop(vm); // pop method
                BREAK;
            }
            {
                int32_t argc; // shared
                CASE(OP_INVOKE0)
                {
                    argc = 0;
                    goto l_invoke;
                }
                CASE(OP_INVOKE1)
                {
                    argc = 1;
                    goto l_invoke;
                }
                CASE(OP_INVOKE)
                {
                    argc = cast(int32_t, vm->sp - Array_VRef_pop(&vm->callstart));
                l_invoke:;
                    {
                        Value methodname = READ_CONSTANT();
                        int32_t retcnt = READ_BYTEL();
                        saveip();
                        invoke(vm, methodname, argc, retcnt);
                        updatestate();
                        BREAK;
                    }
                }
            }
            CASE(OP_GET_SUPER)
            {
                sk_assert(vm, IS_CLASS(*stackpeek(0)), "Expect OClass.");
                Value methodname = READ_CONSTANT();
                OClass* superclass = AS_CLASS(pop(vm));
                saveip();
                if(unlikely(!bindmethod(vm, superclass, methodname, *stackpeek(0))))
                    udperror(vm, methodname, superclass);
                BREAK;
            }
            {
                int32_t argc; // shared
                CASE(OP_INVOKE_SUPER0)
                {
                    argc = 0;
                    goto l_invoke_super;
                }
                CASE(OP_INVOKE_SUPER1)
                {
                    argc = 1;
                    goto l_invoke_super;
                }
                CASE(OP_INVOKE_SUPER)
                {
                    argc = cast(int32_t, vm->sp - Array_VRef_pop(&vm->callstart));
                l_invoke_super:;
                    {
                        Value methodname = READ_CONSTANT();
                        int32_t retcnt = READ_BYTEL();
                        sk_assert(vm, IS_CLASS(*stackpeek(0)), "superclass must be class.");
                        OClass* superclass = AS_CLASS(pop(vm));
                        saveip();
                        invokefrom(vm, superclass, methodname, argc, retcnt);
                        updatestate();
                        BREAK;
                    }
                }
            }
            CASE(OP_SET_PROPERTY) // 'instance.property_name = property'
            {
                Value property_name = READ_CONSTANT();
                Value property = *stackpeek(0);
                Value receiver = *stackpeek(1);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    saveip();
                    ipaerror(vm, receiver);
                }
                OInstance* instance = AS_INSTANCE(receiver);
                rawset(vm, &instance->fields, property_name, property);
                popn(vm, 2); // instance + property
                BREAK;
            }
            CASE(OP_GET_PROPERTY) // 'instance.property_name'
            {
                Value property_name = READ_CONSTANT();
                Value receiver = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    saveip();
                    ipaerror(vm, receiver);
                }
                OInstance* instance = AS_INSTANCE(receiver);
                Value property;
                if(rawget(vm, &instance->fields, property_name, &property)) {
                    *stackpeek(0) = property;
                    BREAK;
                }
                saveip();
                if(unlikely(!bindmethod(vm, instance->oclass, property_name, receiver)))
                    udperror(vm, property_name, instance->oclass);
                BREAK;
            }
            {
                int32_t bytecode_param; // shared
                CASE(OP_DEFINE_GLOBAL)
                {
                    bytecode_param = READ_BYTE();
                    goto l_define_global;
                }
                CASE(OP_DEFINE_GLOBALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto l_define_global;
                }
            l_define_global:;
                {
                    Value* gvalue = &vm->globvars.data[bytecode_param].value;
                    if(unlikely(*gvalue != EMPTY_VAL)) {
                        saveip();
                        redefgerror(vm, globalname(vm, bytecode_param)->storage);
                    }
                    *gvalue = pop(vm);
                    BREAK;
                }
                CASE(OP_GET_GLOBAL)
                {
                    bytecode_param = READ_BYTE();
                    goto l_get_global;
                }
                CASE(OP_GET_GLOBALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto l_get_global;
                }
            l_get_global:;
                {
                    Value* gvalue = &vm->globvars.data[bytecode_param].value;
                    if(unlikely(*gvalue == EMPTY_VAL)) {
                        saveip();
                        udgerror(vm, globalname(vm, bytecode_param)->storage);
                    }
                    push(vm, *gvalue);
                    BREAK;
                }
                CASE(OP_SET_GLOBAL)
                {
                    bytecode_param = READ_BYTE();
                    goto l_set_global;
                }
                CASE(OP_SET_GLOBALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto l_set_global;
                }
            l_set_global:;
                {
                    Variable* gvar = &vm->globvars.data[bytecode_param];
                    if(unlikely(gvar->value == EMPTY_VAL)) {
                        saveip();
                        udgerror(vm, globalname(vm, bytecode_param)->storage);
                    } else if(unlikely(ISFIXED(gvar))) {
                        saveip();
                        fixederror(vm, globalname(vm, bytecode_param)->storage);
                    }
                    gvar->value = pop(vm);
                    BREAK;
                }
                CASE(OP_GET_LOCAL)
                {
                    bytecode_param = READ_BYTE();
                    goto l_get_local;
                }
                CASE(OP_GET_LOCALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto l_get_local;
                }
            l_get_local:;
                {
                    push(vm, frame->callee[bytecode_param]);
                    BREAK;
                }
                CASE(OP_SET_LOCAL)
                {
                    bytecode_param = READ_BYTE();
                    goto l_set_local;
                }
                CASE(OP_SET_LOCALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto l_set_local;
                }
            l_set_local:;
                {
                    frame->callee[bytecode_param] = pop(vm);
                    BREAK;
                }
            }
            CASE(OP_JMP_IF_FALSE) // unused, using 'optimized' versions with pop
            {
                unreachable;
                uint32_t skip_offset = READ_BYTEL();
                ip += ISFALSE(*stackpeek(0)) * skip_offset;
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_POP)
            {
                UNARY_OP(vm, AR_NOT);
                uint32_t skip_offset = READ_BYTEL();
                ip += ISFALSE(*stackpeek(0)) * skip_offset;
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                pop(vm);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_OR_POP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += (ISFALSE(*stackpeek(0)) ? skip_offset : (pop(vm), 0));
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_AND_POP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += (ISFALSE(*stackpeek(0)) ? (pop(vm), skip_offset) : 0);
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += skip_offset;
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP_AND_POP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += skip_offset;
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                pop(vm);
                BREAK;
            }
            CASE(OP_LOOP)
            {
                uint32_t offset = READ_BYTEL();
                ip -= offset;
                sk_assert(vm, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_CLOSURE)
            {
                OFunction* fn = AS_FUNCTION(READ_CONSTANT());
                OClosure* closure = OClosure_new(vm, fn);
                push(vm, OBJ_VAL(closure));
                for(uint32_t i = 0; i < closure->fn->p.upvalc; i++) {
                    uint8_t local = READ_BYTE();
                    uint32_t idx = READ_BYTEL();
                    if(local) closure->upvalue[i] = captureupval(vm, frame->callee + idx);
                    else closure->upvalue[i] = frame->closure->upvalue[idx];
                }
                BREAK;
            }
            CASE(OP_GET_UPVALUE)
            {
                uint32_t idx = READ_BYTEL();
                push(vm, *frame->closure->upvalue[idx]->location);
                BREAK;
            }
            CASE(OP_SET_UPVALUE)
            {
                uint32_t idx = READ_BYTEL();
                *frame->closure->upvalue[idx]->location = pop(vm);
                BREAK;
            }
            CASE(OP_CLOSE_UPVAL)
            {
                closeupval(vm, vm->sp - 1);
                pop(vm);
                BREAK;
            }
            CASE(OP_CLOSE_UPVALN)
            {
                uint32_t last = READ_BYTEL();
                closeupval(vm, vm->sp - last);
                popn(vm, last);
                BREAK;
            }
            CASE(OP_CLASS)
            {
                push(vm, OBJ_VAL(OClass_new(vm, READ_STRING())));
                BREAK;
            }
            CASE(OP_INDEX) // 'instance[key]'
            {
                Value receiver = *stackpeek(1);
                Value key = *stackpeek(0);
                saveip();
                checkindex(vm, receiver, key);
                if(!calloverload(vm, receiver, OM_GETIDX)) { // not overloaded ?
                    OInstance* instance = AS_INSTANCE(receiver);
                    Value property;
                    if(tableget(vm, &instance->fields, key, &property)) {
                        *stackpeek(1) = property; // replace receiver with field value
                        pop(vm); // pop key
                        BREAK;
                    } // else try get method
                    if(unlikely(!bindmethod(vm, instance->oclass, key, receiver)))
                        udperror(vm, key, instance->oclass);
                    *stackpeek(1) = pop(vm); // replace receiver with popped method
                } else updatestate();
                BREAK;
            }
            CASE(OP_SET_INDEX) // 'instance[key] = value;'
            {
                Value receiver = *stackpeek(2);
                Value key = *stackpeek(1);
                Value value = *stackpeek(0);
                saveip();
                checkindex(vm, receiver, key);
                OInstance* instance = AS_INSTANCE(receiver);
                if(!calloverload(vm, receiver, OM_SETIDX)) { // not overloaded ?
                    tableset(vm, &instance->fields, key, value);
                    popn(vm, 3); // pop instance, key and value
                } else updatestate();
                BREAK;
            }
            CASE(OP_OVERLOAD) // 'fn __omethod__ { ... }'
            {
                sk_om tag = READ_BYTE();
                OClass* oclass = AS_CLASS(*stackpeek(1));
                oclass->omethods[tag] = AS_OBJ(pop(vm));
                sk_assert(vm, *ip == OP_METHOD, "Expected 'OP_METHOD'.");
                BREAK;
            }
            CASE(OP_INHERIT) // 'class A impl B { ... }'
            {
                sk_assert(vm, IS_CLASS(*stackpeek(0)), "subclass must be class.");
                OClass* subclass = AS_CLASS(*stackpeek(0));
                Value superclass = *stackpeek(1);
                if(unlikely(!IS_CLASS(superclass))) {
                    saveip();
                    inheriterror(vm, superclass);
                }
                HashTable_into(vm, &AS_CLASS(superclass)->methods, &subclass->methods, 0);
                memcpy(
                    subclass->omethods,
                    AS_CLASS(superclass)->omethods,
                    sizeof(subclass->omethods));
                pop(vm); // pop subclass
                BREAK;
            }
            CASE(OP_FOREACH_PREP)
            {
                int32_t vars = READ_BYTEL();
                memcpy(vm->sp, stackpeek(2), 3 * sizeof(Value));
                vm->sp += 3;
                saveip();
                Value fn = *stackpeek(2);
                call(vm, fn, 2, vars);
                updatestate();
                BREAK;
            }
            CASE(OP_FOREACH)
            {
                int32_t vars = READ_BYTEL();
                Value* cntlvar = stackpeek(vars);
                *cntlvar = *stackpeek(vars - 1);
                sk_assert(vm, *ip == OP_JMP, "Expect 'OP_JMP'.");
                if(!IS_NIL(*cntlvar)) ip += 4;
                BREAK;
            }
            CASE(OP_CALLSTART)
            {
                Array_VRef_push(&vm->callstart, vm->sp);
                BREAK;
            }
            CASE(OP_RETSTART)
            {
                Array_VRef_push(&vm->retstart, vm->sp);
                BREAK;
            }
            CASE(OP_RET0) // should not return anything
            {
                // When returning from overload-able methods that do
                // not have return value (such as '__setidx__').
                frame->retcnt = 0;
                goto l_ret;
            }
            CASE(OP_RET1) // single value return
            {
                // Stack already contains only a single return value;
                // this instruction comes in handy when returning from
                // overloaded method.
                // Because grammar and parser ensure there can only be
                // a single return value from __init__, __getidx__, __add__,
                // __sub__, __eq__, etc...
                // So instead of shuffling and checking if stack has enough
                // or lack of values, we skip the check instead.
                sk_assert(vm, frame->retcnt == 1, "invalid retcnt");
                goto l_ret;
            }
            CASE(OP_RET) // function return
            {
                int32_t retvalcnt, unaccounted;
                retvalcnt = cast(int32_t, vm->sp - Array_VRef_pop(&vm->retstart));
                if(frame->retcnt == 0) { // multiple return values ?
                    unaccounted = 0;
                    frame->retcnt = retvalcnt;
                } else unaccounted = frame->retcnt - retvalcnt;
                if(unaccounted < 0) popn(vm, -unaccounted);
                else pushn(vm, unaccounted, NIL_VAL);
            l_ret:
                saveip();
                sk_assert(vm, vm->temp.len == 0, "Temporary array not empty.");
                for(int32_t returns = frame->retcnt; returns--;)
                    Array_Value_push(&vm->temp, *--vm->sp);
                closeupval(vm, frame->callee); // close any open upvalues
                vm->fc--; // pop the frame
                vm->sp = frame->callee; // adjust the stack pointer
                while(vm->temp.len > 0) // push return values
                    push(vm, Array_Value_pop(&vm->temp));
                sk_assert(vm, vm->temp.len == 0, "Temporary array not empty.");
                if(last_frame(vm).cfinfo & CFI_FRESH) return;
                sk_assert(vm, vm->fc > 0, "Invalid cfinfo.");
                updatestate();
                BREAK;
            }
        }
    }

    unreachable;

#undef READ_BYTE
#undef READ_BYTEL
#undef READ_CONSTANT
#undef READ_CONSTANTL
#undef READ_STRING
#undef READ_STRINGL
#undef DISPATCH
#undef CASE
#undef BREAK
#undef VM_BINARY_OP
}


/* Interprets (compiles and runs) the 'source'. */
void interpret(VM* vm, const char* source, const char* path)
{
    TODO("Refactor")
    Value name = OBJ_VAL(OString_new(vm, path, strlen(path)));
    OClosure* closure = NULL; // TODO: compile(vm, source, name, true);
    if(closure == NULL) printandpanic(vm);
    sk_pcall(vm, 0, 0);
}


/* Initialize the allocated VM */
void VM_init(VM* vm)
{
    srand(time(0));
    vm->seed = rand();
    vm->fc = 0;
    vm->objects = NULL;
    vm->F = NULL;
    vm->open_upvals = NULL;
    vm->gc.gc_heapmin = GC_HEAP_MIN;
    vm->gc.gc_nextgc = GC_HEAP_INIT; // 1 MiB
    vm->gc.gc_allocated = 0;
    vm->gc.gc_growfactor = GC_HEAP_GROW_FACTOR;
    vm->gc.gc_stopped = 0;
    vm->sp = vm->stack;
    HashTable_init(&vm->loaded); // Loaded scripts and their functions
    HashTable_init(&vm->globids); // Global variable identifiers
    GSARRAY_INIT(vm); // Gray stack array (no GC)
    Array_Variable_init(&vm->globvars, vm);
    Array_Value_init(&vm->temp, vm); // Temp values storage (return values)
    Array_VRef_init(&vm->callstart, vm);
    Array_VRef_init(&vm->retstart, vm);
    Array_OSRef_init(&vm->interned, vm);
    HashTable_init(&vm->weakrefs); // Interned strings table (Weak_refs)
    memset(vm->faststatic, 0, sizeof(vm->faststatic));
    for(uint8_t i = 0; i < SS_SIZE; i++)
        vm->faststatic[i] = OString_new(vm, static_strings[i].name, static_strings[i].len);
}



/*
 * Reset virtual machine call stack and close all upvalues.
 * Additionally set the error object on top of the stack
 * if the 'status' is error code.
 */
void resetvm(VM* vm, sk_status status)
{
    Value* top = vm->stack;
    closeupval(vm, top + 1); // close all open upvalues
    vm->fc = 0; // reset call stack
    if(status != S_OK) {
        if(status == S_EMEM) *top = OBJ_VAL(vm->memerror);
        else *top = *stackpeek(0); // err obj on top
        vm->sp = top + 1;
    } else vm->sp = top;
}


/*
 * Frees the VM and nulls out its pointer.
 */
void _cleanupvm(VM** vmp)
{
    _cleanup_function((*vmp)->F);
    sk_destroy(vmp);
}
