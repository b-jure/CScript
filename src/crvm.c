/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crconf.h"
#include "cript.h"
#include "criptapi.h"
#include "crdebug.h"
#include "crlimits.h"
#include "crmem.h"
#include "crobject.h"
#include "crparser.h"
#include "crvalue.h"
#include "crvm.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>



/* Adjust return values after native call finishes. */
static cr_inline void moveresults(cr_State *ts, Value *fn, int32_t got, int32_t expect)
{
    Value *retstart = ts->sp - got; // start of return values
    if (expect == 0)
        expect = got; // all results (MULRET)
    if (got > expect)
        got = expect; // remove extra results
    memcpy(fn, retstart, got); // Safety: 'retstart' >= 'nativefn'
    for (int32_t i = got; i < expect; i++) // replace missing values with nil
        fn[i] = NIL_VAL;
    ts->sp = fn + expect;
}


/* Call native function. */
static cr_inline int32_t callnative(cr_State *ts, Value fn)
{
    cr_unlock(ts);
    int32_t n = ascfn(fn)->fn(ts);
    cr_lock(ts);
    criptapi_checkelems(ts, n);
    CallFrame *f = &last_frame(ts);
    moveresults(ts, f->callee, n, f->retcnt);
    ts->fc--; // pop frame
    return n;
}



/* Calls have lots of checks and there are two main reasons why.
 * cript does not allow extra arguments if the function does not
 * accept variable number of arguments and you are not allowed to
 * provide less arguments than the function arity.
 * In case 'callee' is a cript closure then just return the new 'CallFrame',
 * otherwise run the C closure and return NULL. */
static CallFrame *precall(cr_State *ts, Value callee, int32_t argc, int32_t retcnt)
{
    FnInfo *p = NULL;
    CallFrame *frame = &ts->frames[ts->fc];
    if (!iscfunction(callee)) {
        CrClosure *closure = asclosure(callee);
        Function *fn = closure->fn;
        p = &fn->p;
        frame->closure = closure;
        frame->ip = fn->chunk.code.data;
        frame->cfinfo = 0;
        retcnt = (retcnt == CR_MULRET ? 0 : retcnt); // adjust return count
    } else {
        CClosure *native = ascfn(callee);
        p = &native->p;
        frame->closure = NULL;
        frame->ip = NULL;
        frame->cfinfo = CFI_CCALL;
    }
#if defined(callbitmask)
    const static void *jmptable[] = {
        &&l_stack_overflow,
        &&l_invalid_argc,
        &&l_callstack_overflow,
        &&l_ok,
    };
    uint32_t bitmask = callbitmask(ts, p->isvararg, p->arity, argc, retcnt);
    cr_ubyte idx = cr_ctz(bitmask);
    goto *jmptable[idx];
l_stack_overflow:
    retovferror(ts, p->name->storage);
l_invalid_argc:
    arityerror(ts, p->arity, argc);
l_callstack_overflow:
    fcovferror(ts);
#else
    if (cr_unlikely(!cr_ensurestack(ts, retcnt))) {
        retovferror(ts, p->name->storage);
    } else if (cr_unlikely((p->isvararg && p->arity > argc) || (!p->isvararg && p->arity != argc))) {
        arityerror(ts, p->arity, argc);
    } else if (cr_unlikely(ts->fc == ts_CALLSTACK_LIMIT)) {
        fcovferror(ts);
    } else
        goto ok;
#endif
l_ok:
    frame->vacnt = argc - p->arity;
    frame->retcnt = retcnt;
    frame->callee = ts->sp - argc - 1;
    ts->fc++;
    if (frame->cfinfo & CFI_CCALL) {
        callnative(ts, callee);
        return NULL;
    } else
        return &last_frame(ts);
}


static CallFrame *precall(cr_State *ts, SPtr fn, int retcnt)
{
}


/* Call '()' a value (closure, method, class). */
cr_sinline void call(cr_State *ts, SPtr fn, int retcnt)
{
    int argc;

    argc = ts->sp.p - fn - 1;
    if (cr_unlikely(!IS_OBJ(callee)))
        callerror(ts, callee);
    switch (OBJ_TYPE(callee)) {
        case OBJ_BOUND_METHOD: {
            InstanceMethod *bound = asboundmethod(callee);
            ts->sp[-argc - 1] = bound->receiver; // class instance (self)
            return precall(ts, OBJ_VAL(bound->method), argc, retcnt);
        }
        case OBJ_CLASS: {
            OClass *oclass = asclass(callee);
            Value instance = OBJ_VAL(Instance_new(ts, oclass));
            if (!calloverload(ts, instance, OM_INIT)) { // not overloaded ?
                *stkpeek(argc) = instance; // 'self'
                int32_t arity = ominfo[OM_INIT].arity; // default arity
                if (cr_unlikely(argc != arity))
                    arityerror(ts, arity, argc);
            }
            return NULL;
        }
        case OBJ_CLOSURE:
        case OBJ_FUNCTION:
        case OBJ_CFUNCTION:
            return precall(ts, callee, argc, retcnt);
        default:
            cr_unreachable;
    }
}


/* external interface for 'call'. */
void crVm_call(cr_State *ts, SPtr fn, int nreturns) {
    call(ts, fn, nreturns);
}


/* Protected call with longjmp.
 * Performs a protected call, calling the wrapper 'ProtectedFn' around
 * a cript function or native C function.
 * Returns status of the called function, this status is modified
 * by function that errors and performs the long jump or it
 * stays unchanged and the wrapper function just returns and
 * execution continues. */
cr_sinline int32_t protectedcall(cr_State *ts, ProtectedFn fn, void *userdata) {
    int32_t oldfc = ts->fc;
    struct cr_longjmp lj;
    lj.status = S_OK;
    lj.prev = ts->errjmp;
    ts->errjmp = &lj;
    if (setjmp(lj.buf) == 0)
        (*fn)(ts, userdata);
    ts->errjmp = lj.prev;
    ts->fc = oldfc;
    return lj.status;
}


/* Public interface to 'protectedcall'.
 * In case of errors it performs a recovery by closing all
 * open upvalues (values to be closed) and restoring the
 * old stack pointer (oldtop). */
int32_t crVm_pcall(cr_State *ts, ProtectedFn fn, void *userdata, ptrdiff_t oldtop)
{
    int status;
    SPtr oldsp;

    status = protectedcall(ts, fn, userdata);
    if (cr_unlikely(status != CR_OK)) {
        closeupval(ts, ts->sp);
        oldsp = restorestack(ts, oldtop);
        *oldsp = ts->sp[-1];
        ts->sp = oldsp + 1;
    }
    return status;
}


/* Check if receiver and key are valid for indexing. */
static cr_inline void checkindex(cr_State *ts, Value receiver, Value key)
{
    if (cr_unlikely(!isinstance(receiver)))
        ipaerror(ts, receiver);
    else if (cr_unlikely(IS_NIL(key)))
        nilidxerror(ts);
}


/* Private to interpreter.
 * Used when creating a cript closure. */
static cr_inline OUpvalue *captureupval(cr_State *ts, Value *valp)
{
    OUpvalue **upvalpp = &ts->open_upvals;
    while (*upvalpp != NULL && (*upvalpp)->location > valp)
        upvalpp = &(*upvalpp)->next;
    if (*upvalpp != NULL && (*upvalpp)->location == valp)
        return *upvalpp;
    OUpvalue *upvalp = OUpvalue_new(ts, valp);
    upvalp->next = *upvalpp;
    *upvalpp = upvalp;
    return upvalp;
}

/* Closes all of the captured variables moving
 * them from the stack onto the heap (open_upvals array),
 * making them reachable for gc. */
void closeupval(cr_State *ts, Value *last)
{
    while (ts->openuvals != NULL && ts->openuvals->location >= last) {
        OUpvalue *upvalp = ts->open_upvals;
        upvalp->closed = *upvalp->location;
        upvalp->location = &upvalp->closed;
        ts->open_upvals = upvalp->next;
    }
}

/**
 * Searches the entire table for the matching index in order to
 * provide more descriptive runtime error.
 **/
CRString *globalname(cr_State *ts, uint32_t idx)
{
    for (uint32_t i = 0; i < ts->globids.cap; i++) {
        Entry *entry = &ts->globids.entries[i];
        if (!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
            return (OString *)asobj(entry->key);
    }
    cr_unreachable;
}



/* -------------------------------------------------------------------------
 * Interpreter loop
 * -------------------------------------------------------------------------- */


/* save program counter */
#define savepc()        ((cf)->pc = pc)

/* save program counter and stack top */
#define savestate()     (savepc(), (ts)->stacktop.p = (cf)->stacktop.p)

/* protect code that can raise errors or change the stack */
#define protect(e)      (savestate(), (e))


/* fetch an instruction */
#define fetch()         (pc += INSTRSIZE, pc[-INSTRSIZE])

/* fetch short instruction parameter */
#define fetchshort() \
    (pc += SPARAMSIZE, GETSPARAMV(&pc[-SPARAMSIZE-INSTRSIZE], 0))

/* fetch long instruction parameter */
#define fetchlong() \
    (pc += LPARAMSIZE, GETLPARAMV(&pc[-LPARAMSIZE-INSTRSIZE], 0))


/* get constant */
#define fetchconstant()         (cffn(cf)->constants.ptr[fetchlong(pc)])

/* get string constant */
#define fetchstring()           (strvalue(fetchconstant()))


#define DISPATCH(x)     switch(x)
#define CASE(l)         case l:
#define BREAK           break


void run(cr_State *ts)
{
    register CallFrame *cf;
    register const Instruction *pc;
    int codeparam1;
    int codeparam2;
    int codeparam3;

    cf = ts->frame;
    pc = frame->pc;
    for (;;) {
#ifdef PRECOMPUTED_GOTO
#include "crjmptable.h"
#endif
        DISPATCH(fetch(pc)) {
            CASE(OP_TRUE) {
                setbtvalue(s2v(ts->sp.p++));
                BREAK;
            }
            CASE(OP_FALSE) {
                setbfvalue(s2v(ts->sp.p++));
                BREAK;
            }
            CASE(OP_NIL) {
                setnilvalue(s2v(ts->sp.p++));
                BREAK;
            }
            CASE(OP_NILN) {
                codeparam1 = longparam();
                while (codeparam1--)
                    setnilvalue(s2v(ts->sp.p++));
                BREAK;
            } CASE(OP_ADD) {
                BINARY_OP(ts, AR_ADD);
                BREAK;
            }
            CASE(OP_SUB) {
                BINARY_OP(ts, AR_SUB);
                BREAK;
            }
            CASE(OP_MUL) {
                BINARY_OP(ts, AR_MUL);
                BREAK;
            }
            CASE(OP_MOD) {
                BINARY_OP(ts, AR_MOD);
                BREAK;
            }
            CASE(OP_POW) {
                BINARY_OP(ts, AR_POW);
                BREAK;
            }
            CASE(OP_DIV) {
                BINARY_OP(ts, AR_DIV);
                BREAK;
            }
            CASE(OP_NEG) {
                UNARY_OP(ts, AR_UMIN);
                BREAK;
            }
            CASE(OP_NOT) {
                UNARY_OP(ts, AR_NOT);
                BREAK;
            }
            CASE(OP_VARARG)
            {
                savepc();
                Function *fn = FFN(frame);
                uint32_t vacnt = READ_BYTEL();
                if (vacnt == 0)
                    vacnt = frame->vacnt;
                for (uint32_t i = 1; i <= vacnt; i++) {
                    Value *next = frame->callee + fn->p.arity + i;
                    push(ts, *next);
                }
                BREAK;
            }
            CASE(OP_NOT_EQUAL)
            {
                ORDER_OP(ts, vne);
                BREAK;
            }
            CASE(OP_EQUAL)
            {
                ORDER_OP(ts, veq);
                BREAK;
            }
            CASE(OP_EQ) // same as OP_EQUAL except we don't pop the first operand
            {
                ORDER_OP(ts, eq_preserveL);
                BREAK;
            }
            CASE(OP_GREATER)
            {
                ORDER_OP(ts, vgt);
                BREAK;
            }
            CASE(OP_GREATER_EQUAL)
            {
                ORDER_OP(ts, vge);
                BREAK;
            }
            CASE(OP_LESS)
            {
                ORDER_OP(ts, vle);
                BREAK;
            }
            CASE(OP_LESS_EQUAL)
            {
                ORDER_OP(ts, vle);
                BREAK;
            }
            CASE(OP_POP)
            {
                pop(ts);
                BREAK;
            }
            CASE(OP_POPN)
            {
                popn(ts, READ_BYTEL());
                BREAK;
            }
            CASE(OP_CONST)
            {
                savepc();
                push(ts, READ_CONSTANT());
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
                    argc = cast(int32_t, ts->sp - Array_VRef_pop(&ts->callstart));
l_call:;
       {
           int32_t retcnt = READ_BYTEL();
           Value callee = *stkpeek(argc);
           savepc();
           call(ts, callee, argc, retcnt);
           updatestate();
           BREAK;
       }
                }
            }
            CASE(OP_METHOD)
            {
                Value methodname = READ_CONSTANT();
                Value method = *stkpeek(0); // OClosure or ONative
                OClass *oclass = asclass(*stkpeek(1));
                rawset(ts, &oclass->methods, methodname, method);
                pop(ts); // pop method
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
                    argc = cast(int32_t, ts->sp - Array_VRef_pop(&ts->callstart));
l_invoke:;
         {
             Value methodname = READ_CONSTANT();
             int32_t retcnt = READ_BYTEL();
             savepc();
             invoke(ts, methodname, argc, retcnt);
             updatestate();
             BREAK;
         }
                }
            }
            CASE(OP_GET_SUPER)
            {
                cr_assert(ts, isclassobj(*stkpeek(0)), "Expect OClass.");
                Value methodname = READ_CONSTANT();
                OClass *superclass = asclass(pop(ts));
                savepc();
                if (cr_unlikely(!bindmethod(ts, superclass, methodname, *stkpeek(0))))
                    udperror(ts, methodname, superclass);
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
                    argc = cast(int32_t, ts->sp - Array_VRef_pop(&ts->callstart));
l_invoke_super:;
               {
                   Value methodname = READ_CONSTANT();
                   int32_t retcnt = READ_BYTEL();
                   cr_assert(ts, isclassobj(*stkpeek(0)), "superclass must be class.");
                   OClass *superclass = asclass(pop(ts));
                   savepc();
                   invokefrom(ts, superclass, methodname, argc, retcnt);
                   updatestate();
                   BREAK;
               }
                }
            }
            CASE(OP_SET_PROPERTY) // 'instance.property_name = property'
            {
                Value property_name = READ_CONSTANT();
                Value property = *stkpeek(0);
                Value receiver = *stkpeek(1);
                if (cr_unlikely(!isinstance(receiver))) {
                    savepc();
                    ipaerror(ts, receiver);
                }
                Instance *instance = asinstance(receiver);
                rawset(ts, &instance->fields, property_name, property);
                popn(ts, 2); // instance + property
                BREAK;
            }
            CASE(OP_GET_PROPERTY) // 'instance.property_name'
            {
                Value property_name = READ_CONSTANT();
                Value receiver = *stkpeek(0);
                if (cr_unlikely(!isinstance(receiver))) {
                    savepc();
                    ipaerror(ts, receiver);
                }
                Instance *instance = asinstance(receiver);
                Value property;
                if (rawget(ts, &instance->fields, property_name, &property)) {
                    *stkpeek(0) = property;
                    BREAK;
                }
                savepc();
                if (cr_unlikely(!bindmethod(ts, instance->oclass, property_name, receiver)))
                    udperror(ts, property_name, instance->oclass);
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
                    Value *gvalue = &ts->globvars.data[bytecode_param].value;
                    if (cr_unlikely(*gvalue != EMPTY_VAL)) {
                        savepc();
                        redefgerror(ts, globalname(ts, bytecode_param)->storage);
                    }
                    *gvalue = pop(ts);
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
                 Value *gvalue = &ts->globvars.data[bytecode_param].value;
                 if (cr_unlikely(*gvalue == EMPTY_VAL)) {
                     savepc();
                     udgerror(ts, globalname(ts, bytecode_param)->storage);
                 }
                 push(ts, *gvalue);
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
                 Variable *gvar = &ts->globvars.data[bytecode_param];
                 if (cr_unlikely(gvar->value == EMPTY_VAL)) {
                     savepc();
                     udgerror(ts, globalname(ts, bytecode_param)->storage);
                 } else if (cr_unlikely(VISCONST(gvar))) {
                     savepc();
                     fixederror(ts, globalname(ts, bytecode_param)->storage);
                 }
                 gvar->value = pop(ts);
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
                push(ts, frame->callee[bytecode_param]);
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
                frame->callee[bytecode_param] = pop(ts);
                BREAK;
            }
            }
            CASE(OP_JMP_IF_FALSE) // unused, using 'optimized' versions with pop
            {
                cr_unreachable;
                uint32_t skip_offset = READ_BYTEL();
                ip += ISFALSE(*stkpeek(0)) * skip_offset;
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_POP)
            {
                UNARY_OP(ts, AR_NOT);
                uint32_t skip_offset = READ_BYTEL();
                ip += ISFALSE(*stkpeek(0)) * skip_offset;
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                pop(ts);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_OR_POP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += (ISFALSE(*stkpeek(0)) ? skip_offset : (pop(ts), 0));
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_AND_POP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += (ISFALSE(*stkpeek(0)) ? (pop(ts), skip_offset) : 0);
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += skip_offset;
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_JMP_AND_POP)
            {
                uint32_t skip_offset = READ_BYTEL();
                ip += skip_offset;
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                pop(ts);
                BREAK;
            }
            CASE(OP_LOOP)
            {
                uint32_t offset = READ_BYTEL();
                ip -= offset;
                cr_assert(ts, ipinbounds(), "Invalid jump.");
                BREAK;
            }
            CASE(OP_CLOSURE)
            {
                Function *fn = asfn(READ_CONSTANT());
                CrClosure *closure = OClosure_new(ts, fn);
                push(ts, OBJ_VAL(closure));
                for (uint32_t i = 0; i < closure->fn->p.upvalc; i++) {
                    cr_ubyte local = READ_BYTE();
                    uint32_t idx = READ_BYTEL();
                    if (local)
                        closure->upvalue[i] = captureupval(ts, frame->callee + idx);
                    else
                        closure->upvalue[i] = frame->closure->upvalue[idx];
                }
                BREAK;
            }
            CASE(OP_GET_UPVALUE)
            {
                uint32_t idx = READ_BYTEL();
                push(ts, *frame->closure->upvalue[idx]->location);
                BREAK;
            }
            CASE(OP_SET_UPVALUE)
            {
                uint32_t idx = READ_BYTEL();
                *frame->closure->upvalue[idx]->location = pop(ts);
                BREAK;
            }
            CASE(OP_CLOSE_UPVAL)
            {
                closeupval(ts, ts->sp - 1);
                pop(ts);
                BREAK;
            }
            CASE(OP_CLOSE_UPVALN)
            {
                uint32_t last = READ_BYTEL();
                closeupval(ts, ts->sp - last);
                popn(ts, last);
                BREAK;
            }
            CASE(OP_CLASS)
            {
                push(ts, OBJ_VAL(OClass_new(ts, READ_STRING())));
                BREAK;
            }
            CASE(OP_INDEX) // 'instance[key]'
            {
                Value receiver = *stkpeek(1);
                Value key = *stkpeek(0);
                savepc();
                checkindex(ts, receiver, key);
                if (!calloverload(ts, receiver, OM_GETIDX)) { // not overloaded ?
                    Instance *instance = asinstance(receiver);
                    Value property;
                    if (tableget(ts, &instance->fields, key, &property)) {
                        *stkpeek(1) = property; // replace receiver with field value
                        pop(ts); // pop key
                        BREAK;
                    } // else try get method
                    if (cr_unlikely(!bindmethod(ts, instance->oclass, key, receiver)))
                        udperror(ts, key, instance->oclass);
                    *stkpeek(1) = pop(ts); // replace receiver with popped method
                } else
                    updatestate();
                BREAK;
            }
            CASE(OP_SET_INDEX) // 'instance[key] = value;'
            {
                Value receiver = *stkpeek(2);
                Value key = *stkpeek(1);
                Value value = *stkpeek(0);
                savepc();
                checkindex(ts, receiver, key);
                Instance *instance = asinstance(receiver);
                if (!calloverload(ts, receiver, OM_SETIDX)) { // not overloaded ?
                    tableset(ts, &instance->fields, key, value);
                    popn(ts, 3); // pop instance, key and value
                } else
                    updatestate();
                BREAK;
            }
            CASE(OP_OVERLOAD) // 'fn __omethod__ { ... }'
            {
                cr_om tag = READ_BYTE();
                OClass *oclass = asclass(*stkpeek(1));
                oclass->omethods[tag] = asobj(pop(ts));
                cr_assert(ts, *ip == OP_METHOD, "Expected 'OP_METHOD'.");
                BREAK;
            }
            CASE(OP_INHERIT) // 'class A impl B { ... }'
            {
                cr_assert(ts, isclassobj(*stkpeek(0)), "subclass must be class.");
                OClass *subclass = asclass(*stkpeek(0));
                Value superclass = *stkpeek(1);
                if (cr_unlikely(!isclassobj(superclass))) {
                    savepc();
                    inheriterror(ts, superclass);
                }
                HTable_into(ts, &asclass(superclass)->mtab, &subclass->methods, 0);
                memcpy(subclass->vtable, asclass(superclass)->vtable, sizeof(subclass->vtable));
                pop(ts); // pop subclass
                BREAK;
            }
            CASE(OP_FOREACH_PREP)
            {
                int32_t vars = READ_BYTEL();
                memcpy(ts->sp, stkpeek(2), 3 * sizeof(Value));
                ts->sp += 3;
                savepc();
                Value fn = *stkpeek(2);
                call(ts, fn, 2, vars);
                updatestate();
                BREAK;
            }
            CASE(OP_FOREACH)
            {
                int32_t vars = READ_BYTEL();
                Value *cntlvar = stkpeek(vars);
                *cntlvar = *stkpeek(vars - 1);
                cr_assert(ts, *ip == OP_JMP, "Expect 'OP_JMP'.");
                if (!IS_NIL(*cntlvar))
                    ip += 4;
                BREAK;
            }
            CASE(OP_CALLSTART)
            {
                Array_VRef_push(&ts->callstart, ts->sp);
                BREAK;
            }
            CASE(OP_RETSTART)
            {
                Array_VRef_push(&ts->retstart, ts->sp);
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
                cr_assert(ts, frame->retcnt == 1, "invalid retcnt");
                goto l_ret;
            }
            CASE(OP_RET) // function return
            {
                int32_t retvalcnt, unaccounted;
                retvalcnt = cast(int32_t, ts->sp - Array_VRef_pop(&ts->retstart));
                if (frame->retcnt == 0) { // multiple return values ?
                    unaccounted = 0;
                    frame->retcnt = retvalcnt;
                } else
                    unaccounted = frame->retcnt - retvalcnt;
                if (unaccounted < 0)
                    popn(ts, -unaccounted);
                else
                    pushn(ts, unaccounted, NIL_VAL);
l_ret:
                savepc();
                cr_assert(ts, ts->temp.len == 0, "Temporary array not empty.");
                for (int32_t returns = frame->retcnt; returns--;)
                    Array_Value_push(&ts->temp, *--ts->sp);
                closeupval(ts, frame->callee); // close any open upvalues
                ts->fc--; // pop the frame
                ts->sp = frame->callee; // adjust the stack pointer
                while (ts->temp.len > 0) // push return values
                    push(ts, Array_Value_pop(&ts->temp));
                cr_assert(ts, ts->temp.len == 0, "Temporary array not empty.");
                if (last_frame(ts).cfinfo & CFI_FRESH)
                    return;
                cr_assert(ts, ts->fc > 0, "Invalid cfinfo.");
                updatestate();
                BREAK;
            }
        }
    }

    cr_unreachable;
}
