#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "err.h"
#include "mem.h"
#include "object.h"
#include "parser.h"
#include "skapi.h"
#include "sklimits.h"
#include "value.h"
#include "vmachine.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>



#define CALL_SKOOMAFN 0
#define CALL_NATIVEFN 1
#define CALL_CLASS    2


volatile uint8_t runtime = 0; // VM is running?



/* Push the value on the stack */
void push(VM* vm, Value val)
{
    if(likely(cast_int(vm->sp - vm->stack) < VM_STACK_LIMIT)) *vm->sp++ = val;
    else sovferror(vm);
}

/* Private to interpreter.
 * Bind class method in order to preserve the receiver.
 * By doing so the interpreter can then push the receiver on the
 * stack before running the function.
 * This is needed because class methods expect the receiver to be
 * the first argument ('self' local variable). */
int8_t bindmethod(VM* vm, OClass* oclass, Value name, Value receiver)
{
    Value method;
    if(!HashTable_get(&oclass->methods, name, &method)) return 0;
    *stackpeek(0) = OBJ_VAL(OBoundMethod_new(vm, receiver, AS_CLOSURE(method)));
    return 1;
}



/* Calls have lots of checks and there are two main reasons why.
 * Skooma does not allow extra arguments if the function does not
 * accept variable number of arguments and you are not allowed to
 * provide less arguments than the function arity. */
static int32_t precall(VM* vm, Value callee, int32_t argc, int32_t retcnt)
{
    FnPrototype* p = NULL;
    CallFrame* frame = &vm->frames[vm->fc];
    int32_t type = CALL_SKOOMAFN;
    if(!IS_NATIVE(callee)) {
        OClosure* closure = AS_CLOSURE(callee);
        OFunction* fn = closure->fn;
        p = &fn->p;
        frame->closure = closure;
        frame->ip = fn->chunk.code.data;
    } else {
        ONative* native = AS_NATIVE(callee);
        p = &native->p;
        frame->closure = NULL;
        frame->ip = NULL;
        type = CALL_NATIVEFN;
    }
#if defined(callbitmask)
    const static void* jmptable[] = {
        &&stack_overflow,
        &&invalid_argc,
        &&callstack_overflow,
        &&ok,
    };
    uint32_t bitmask = callbitmask(vm, p->isvararg, p->arity, argc, retcnt);
    uint8_t idx = sk_ctz(bitmask);
    goto* jmptable[idx];
stack_overflow:
    retovferror(vm, p->name->storage);
invalid_argc:
    arityerror(vm, p->arity, argc);
callstack_overflow:
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
ok:
    frame->vacnt = argc - p->arity;
    frame->retcnt = retcnt;
    frame->callee = vm->sp - argc - 1;
    frame->cfinfo = 0;
    vm->fc++;
    return type;
}

/* Tries to call a value.
 * If value is a callable skooma function it sets up
 * the new call frame and returns 'CALL_SKOOMAFN'.
 * If value is a native C function, it also sets up the
 * new call frame and returns 'CALL_NATIVEFN'.
 * If the value is a class object it creates a new class instance,
 * additionally if there is overload method for that class it
 * sets up the new call frame and returns 'CALL_SKOOMAFN'.
 * If there is no overloaded method it just returns 'CALL_CLASS'.
 * Overloaded methods are only allowed to be Skooma closures. */
static int8_t trycall(VM* vm, Value callee, int32_t argc, int32_t retcnt)
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
            vm->sp[-argc - 1] = OBJ_VAL(OInstance_new(vm, oclass));
            OClosure* init = oclass->omethods[OM_INIT];
            if(init) return precall(vm, OBJ_VAL(init), argc, 1);
            if(unlikely(argc != 0)) arityerror(vm, 0, argc);
            return CALL_CLASS;
        }
        case OBJ_CLOSURE:
        case OBJ_FUNCTION:
        case OBJ_NATIVE:
            return precall(vm, callee, argc, retcnt);
        default:
            unreachable;
    }
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

/* Call native function without locking (for use inside the interpreter). */
static force_inline int32_t callnative_nolock(VM* vm, Value* retstart, Value fn, int32_t retcnt)
{
    int32_t n = AS_NATIVE(fn)->fn(vm);
    skapi_checkelems(vm, n);
    moveresults(vm, retstart, n, retcnt);
    vm->fc--;
    return n;
}

/* Call native function. */
static force_inline int32_t callnative(VM* vm, Value* retstart, Value fn, int32_t retcnt)
{
    sk_unlock(vm);
    int32_t n = AS_NATIVE(fn)->fn(vm);
    sk_lock(vm);
    skapi_checkelems(vm, n);
    moveresults(vm, retstart, n, retcnt);
    vm->fc--; // pop frame
    return n;
}

/* Public interface for normal call (unprotected).
 * Performs a normal function call, in case the function is
 * the skooma function it runs the interpreter and in
 * case of the native C function it calls it directly. */
int32_t ncall(VM* vm, Value* retstart, Value fn, int32_t retcnt)
{
    int32_t argc = vm->sp - retstart - 1;
    int8_t calltype = trycall(vm, fn, argc, retcnt);
    if(calltype == CALL_SKOOMAFN) run(vm);
    else if(calltype == CALL_NATIVEFN) callnative(vm, retstart, fn, retcnt);
    return calltype;
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
 * Tries to call the superclass method named 'name'. */
static force_inline void
invokefrom(VM* vm, OClass* oclass, Value name, int32_t argc, int32_t retcnt)
{
    Value method;
    if(unlikely(!HashTable_get(&oclass->methods, name, &method))) udperror(vm, name, oclass);
    if(trycall(vm, method, argc, retcnt) == CALL_NATIVEFN)
        callnative_nolock(vm, last_frame(vm).callee, method, retcnt);
}

/* Private to interpreter.
 * Small optimization, tries to get and call the indexed
 * value directly as a part of a single instruction. */
static force_inline void invokeindex(VM* vm, int32_t argc, int32_t retcnt)
{
    Value key = *stackpeek(argc);
    Value receiver = *stackpeek(argc + 1);
    if(unlikely(!IS_INSTANCE(receiver))) ipaerror(vm, receiver);
    OInstance* instance = AS_INSTANCE(receiver);
    Value property;
    if(HashTable_get(&instance->fields, key, &property)) goto call;
    if(unlikely(!HashTable_get(&instance->oclass->methods, key, &property)))
        udperror(vm, key, instance->oclass);
call:
    if(trycall(vm, property, argc, retcnt) == CALL_NATIVEFN)
        callnative_nolock(vm, stackpeek(argc + 1), property, retcnt);
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
    if(HashTable_get(&instance->fields, name, &field)) {
        *stackpeek(argc) = field; // swap receiver with field
        if(trycall(vm, field, argc, retcnt) == CALL_NATIVEFN)
            callnative_nolock(vm, stackpeek(argc), field, retcnt);
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
#define saveip()        (frame->ip = ip)
#define updatestate()   (frame = &last_frame(vm), ip = frame->ip)
#define throwerr(vm)    runerror(vm, vtostr(vm, *stackpeek(0))->storage)
#define READ_BYTE()     (*ip++)
#define READ_BYTEL()    (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT() FFN(frame)->chunk.constants.data[READ_BYTEL()]
#define READ_STRING()   AS_STRING(READ_CONSTANT())
#define bcstart()       (FFN(frame).chunk.code.data)
#define ipinbounds()    (ip - bcstart() < VM_STACK_LIMIT && ip >= bcstart())
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


    last_frame(vm).cfinfo = CFI_FRESH;
    runtime = 1;
    register CallFrame* frame = &vm->frames[vm->fc - 1];
    register uint8_t* ip = frame->ip;
#ifdef DEBUG_TRACE_EXECUTION
    printf("\n=== VM - execution ===\n");
#endif
    while(true) {
#ifdef SK_PRECOMPUTED_GOTO
#define OP_TABLE
#include "jmptable.h"
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
                push(vm, BOOL_VAL(true));
                BREAK;
            }
            CASE(OP_FALSE)
            {
                saveip();
                push(vm, BOOL_VAL(false));
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
            CASE(OP_CALL)
            {
                int32_t retcnt = READ_BYTEL();
                int32_t argc = vm->sp - Array_VRef_pop(&vm->callstart);
                Value callee = *stackpeek(argc);
                frame->ip = ip;
                saveip();
                if(trycall(vm, callee, argc, retcnt) == CALL_NATIVEFN)
                    callnative_nolock(vm, last_frame(vm).callee, callee, retcnt);
                updatestate();
                BREAK;
            }
            CASE(OP_METHOD)
            {
                Value methodname = READ_CONSTANT();
                Value method = *stackpeek(0); // OFunction or OClosure
                OClass* oclass = AS_CLASS(*stackpeek(1));
                HashTable_insert(vm, &oclass->methods, methodname, method);
                pop(vm); // pop method
                BREAK;
            }
            CASE(OP_INVOKE)
            {
                Value methodname = READ_CONSTANT();
                int32_t retcnt = READ_BYTEL();
                int32_t argc = vm->sp - Array_VRef_pop(&vm->callstart);
                saveip();
                invoke(vm, methodname, argc, retcnt);
                updatestate();
                BREAK;
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
            CASE(OP_INVOKE_SUPER)
            {
                Value methodname = READ_CONSTANT();
                sk_assert(vm, IS_CLASS(*stackpeek(0)), "superclass must be class.");
                OClass* superclass = AS_CLASS(pop(vm));
                uint32_t argc = vm->sp - Array_VRef_pop(&vm->callstart);
                int32_t retcnt = READ_BYTEL();
                saveip();
                invokefrom(vm, superclass, methodname, argc, retcnt);
                updatestate();
                BREAK;
            }
            CASE(OP_SET_PROPERTY)
            {
                Value property_name = READ_CONSTANT();
                Value receiver = *stackpeek(1);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    saveip();
                    ipaerror(vm, receiver);
                }
                HashTable_insert(vm, &AS_INSTANCE(receiver)->fields, property_name, *stackpeek(0));
                popn(vm, 2); // receiver + new property value
                BREAK;
            }
            CASE(OP_GET_PROPERTY)
            {
                Value property_name = READ_CONSTANT();
                Value receiver = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    saveip();
                    ipaerror(vm, receiver);
                }
                OInstance* instance = AS_INSTANCE(receiver);
                Value property;
                if(HashTable_get(&instance->fields, property_name, &property)) {
                    *stackpeek(0) = property;
                    BREAK;
                }
                saveip();
                if(unlikely(!bindmethod(vm, instance->oclass, property_name, receiver)))
                    udperror(vm, property_name, instance->oclass);
                BREAK;
            }
            {
                // Short and long instructions have identical code
                int32_t bcp; // Bytecode parameter
                CASE(OP_DEFINE_GLOBAL)
                {
                    bcp = READ_BYTE();
                    goto define_global_fin;
                }
                CASE(OP_DEFINE_GLOBALL)
                {
                    bcp = READ_BYTEL();
                    goto define_global_fin;
                }
            define_global_fin:;
                {
                    Value* gvalue = &vm->globvars.data[bcp].value;
                    if(unlikely(*gvalue != EMPTY_VAL)) {
                        saveip();
                        redefgerror(vm, globalname(vm, bcp)->storage);
                    }
                    *gvalue = pop(vm);
                    BREAK;
                }
                CASE(OP_GET_GLOBAL)
                {
                    bcp = READ_BYTE();
                    goto get_global_fin;
                }
                CASE(OP_GET_GLOBALL)
                {
                    bcp = READ_BYTEL();
                    goto get_global_fin;
                }
            get_global_fin:;
                {
                    Value* gvalue = &vm->globvars.data[bcp].value;
                    if(unlikely(*gvalue == EMPTY_VAL)) {
                        saveip();
                        udgerror(vm, globalname(vm, bcp)->storage);
                    }
                    push(vm, *gvalue);
                    BREAK;
                }
                CASE(OP_SET_GLOBAL)
                {
                    bcp = READ_BYTE();
                    goto set_global_fin;
                }
                CASE(OP_SET_GLOBALL)
                {
                    bcp = READ_BYTEL();
                    goto set_global_fin;
                }
            set_global_fin:;
                {
                    Variable* gvar = &vm->globvars.data[bcp];
                    if(unlikely(gvar->value == EMPTY_VAL)) {
                        saveip();
                        udgerror(vm, globalname(vm, bcp)->storage);
                    } else if(unlikely(ISFIXED(gvar))) {
                        saveip();
                        fixederror(vm, globalname(vm, bcp)->storage);
                    }
                    gvar->value = pop(vm);
                    BREAK;
                }
                CASE(OP_GET_LOCAL)
                {
                    bcp = READ_BYTE();
                    goto get_local_fin;
                }
                CASE(OP_GET_LOCALL)
                {
                    bcp = READ_BYTEL();
                    goto get_local_fin;
                }
            get_local_fin:;
                {
                    push(vm, frame->callee[bcp]);
                    BREAK;
                }
                CASE(OP_SET_LOCAL)
                {
                    bcp = READ_BYTE();
                    goto set_local_fin;
                }
                CASE(OP_SET_LOCALL)
                {
                    bcp = READ_BYTEL();
                    goto set_local_fin;
                }
            set_local_fin:;
                {
                    frame->callee[bcp] = pop(vm);
                    BREAK;
                }
                CASE(OP_RET) // function return
                {
                ret_fin:;
                    saveip();
                    int32_t retcnt = vm->sp - Array_VRef_pop(&vm->retstart);
                    int32_t pushc;
                    if(frame->retcnt == 0) { // multiple return values ?
                        pushc = 0;
                        frame->retcnt = retcnt;
                    } else pushc = frame->retcnt - retcnt;
                    if(pushc < 0) popn(vm, -pushc);
                    else pushn(vm, pushc, NIL_VAL);
                    sk_assert(vm, vm->temp.len == 0, "Temporary array not empty.");
                    for(int32_t returns = frame->retcnt; returns--;) {
                        Array_Value_push(&vm->temp, *stackpeek(0));
                        pop(vm);
                    }
                    closeupval(vm, frame->callee);
                    vm->fc--;
                    vm->sp = frame->callee;
                    while(vm->temp.len > 0)
                        push(vm, Array_Value_pop(&vm->temp));
                    sk_assert(vm, vm->temp.len == 0, "Temporary array not empty.");
                    if(last_frame(vm).cfinfo == CFI_FRESH) return;
                    sk_assert(vm, vm->fc > 0, "Invalid CallFrame status.");
                    updatestate();
                    BREAK;
                }
            }
            CASE(OP_JMP_IF_FALSE)
            {
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
                push(vm, *frame->closure->upvalue[READ_BYTEL()]->location);
                BREAK;
            }
            CASE(OP_SET_UPVALUE)
            {
                *frame->closure->upvalue[READ_BYTEL()]->location = pop(vm);
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
            CASE(OP_INDEX)
            {
                // TODO: Fix this up when overloading is implemented
                Value receiver = *stackpeek(1);
                Value key = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    saveip();
                    ipaerror(vm, receiver);
                } else if(unlikely(IS_NIL(key))) {
                    saveip();
                    nilidxerror(vm);
                }
                Value field;
                OInstance* instance = AS_INSTANCE(receiver);
                if(HashTable_get(&instance->fields, key, &field)) {
                    *stackpeek(1) = field; // replace receiver with field
                    pop(vm); // pop key
                    BREAK;
                }
                saveip();
                if(unlikely(!bindmethod(vm, instance->oclass, key, receiver)))
                    udperror(vm, key, instance->oclass);
                *stackpeek(1) = *stackpeek(0); // replace receiver with method
                pop(vm); // pop key
                BREAK;
            }
            CASE(OP_SET_INDEX)
            {
                // @TODO: Fix this up when overloading gets implemented
                Value receiver = *stackpeek(2);
                Value property = *stackpeek(1);
                Value value = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    saveip();
                    ipaerror(vm, receiver);
                } else if(unlikely(IS_NIL(property))) {
                    saveip();
                    nilidxerror(vm);
                }
                HashTable_insert(vm, &AS_INSTANCE(receiver)->fields, property, value);
                popn(vm, 3);
                push(vm, value);
                BREAK;
            }
            CASE(OP_INVOKE_INDEX)
            {
                // @TODO: Fix this up when overloading gets implemented
                int32_t retcnt = READ_BYTEL();
                int32_t argc = vm->sp - Array_VRef_pop(&vm->callstart);
                saveip();
                invokeindex(vm, argc, retcnt);
                updatestate();
                BREAK;
            }
            CASE(OP_OVERLOAD)
            {
                OClass* oclass = AS_CLASS(*stackpeek(1));
                uint8_t opn = READ_BYTE();
                oclass->omethods[opn] = AS_CLOSURE(*stackpeek(0));
                sk_assert(vm, *ip == OP_METHOD, "Expected 'OP_METHOD'.");
                BREAK;
            }
            CASE(OP_INHERIT)
            {
                sk_assert(vm, IS_CLASS(*stackpeek(0)), "subclass must be class.");
                OClass* subclass = AS_CLASS(*stackpeek(0));
                Value superclass = *stackpeek(1);
                if(unlikely(!IS_CLASS(superclass))) {
                    saveip();
                    inheriterror(vm, superclass);
                }
                HashTable_into(vm, &AS_CLASS(superclass)->methods, &subclass->methods);
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
                if(trycall(vm, fn, 2, vars) == CALL_NATIVEFN)
                    callnative_nolock(vm, last_frame(vm).callee, fn, vars);
                updatestate();
                BREAK;
            }
            CASE(OP_FOREACH)
            {
                int32_t vars = READ_BYTEL();
                *stackpeek(vars) = *stackpeek(vars - 1); // cntlvar
                sk_assert(vm, *ip == OP_JMP, "Expect 'OP_JMP'.");
                if(!IS_NIL(*stackpeek(vars))) ip += 4;
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



/*
 * Interprets (compiles and runs) the 'source'.
 */
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
void resetvm(VM* vm, Status status)
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
