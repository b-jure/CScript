#include "array.h"
#include "chunk.h"
#include "common.h"
#include "corelib.h"
#include "debug.h"
#include "err.h"
#include "hash.h"
#include "mem.h"
#include "object.h"
#include "parser.h"
#include "skapi.h"
#include "skmath.h"
#include "value.h"
#include "vmachine.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>



#define CALL_SKOOMAFN 0
#define CALL_NATIVEFN 1
#define CALL_CLASS    2


volatile Int runtime = 0; // VM is running?



/* Push the value on the stack */
void push(VM* vm, Value val)
{
    if(likely(vm->sp - vm->stack < (UInt)VM_STACK_MAX)) *vm->sp++ = val;
    else {
        ASSERT(vm->sp - vm->stack > 0, "invalid VM_STACK_MAX");
        vm->sp--; // make some space
        push(vm, OBJ_VAL(VM_STACK_OVERFLOW(vm, VM_STACK_MAX)));
        runerror(vm, S_ESOVERFLOW);
    }
}

/* Bind class method in order to preserve the receiver.
 * By doing so the interpreter can then push the receiver on the
 * stack before running the function.
 * This is needed because class methods expect the receiver to be
 * the first argument ('self' local variable). */
void bindmethod(VM* vm, OClass* oclass, Value name, Value receiver)
{
    Value method;
    if(unlikely(!HashTable_get(&oclass->methods, name, &method))) {
        const char* classname = oclass->name->storage;
        push(vm, OBJ_VAL(UNDEFINED_PROPERTY_ERR(vm, AS_CSTRING(name), classname)));
        runerror(vm, S_EUDPROPERTY);
    }
    *stackpeek(0) = OBJ_VAL(OBoundMethod_new(vm, receiver, AS_CLOSURE(method)));
}


/* Configuration.
 * Contains various hooks and configurable options.
 * 'reallocate' - allocator function.
 * 'userdata' - additional data for allocator.
 * 'panic' - panic handler in case of runtime errors.
 * 'gc_heapinit' - starting treshold when gc triggers.
 * 'gc_growfactor' - garbage collector grow factor (incremental gc). */
void Config_init(Config* config)
{
    config->reallocate = reallocate;
    config->userdata = NULL;
    config->panic = NULL;
    config->gc_heapinit = GC_HEAP_INIT;
    config->gc_growfactor = GC_HEAP_GROW_FACTOR;
}


/* Calls have lots of checks and there are two main reasons why.
 * Skooma does not allow extra arguments if the function does not
 * accept variable number of arguments and you are not allowed to
 * provide less arguments than the function arity. */
static int precall(VM* vm, Value callee, Int argc, Int retcnt)
{
    CallFrame* frame = &vm->frames[vm->fc];
    OString* err;
    Int status;
    Int arity, isva, isnative;
    Int type = CALL_SKOOMAFN;
    const char* name = NULL;
    isnative = IS_NATIVE(callee);
    if(!isnative) {
        OClosure* closure = AS_CLOSURE(callee);
        OFunction* fn = closure->fn;
        frame->closure = closure;
        frame->ip = fn->chunk.code.data;
        arity = fn->arity;
        isva = fn->isva;
        name = fn->name->storage;
    } else {
        ONative* native = AS_NATIVE(callee);
        frame->closure = NULL;
        frame->ip = NULL;
        arity = native->arity;
        isva = native->isva;
        type = CALL_NATIVEFN;
        name = native->name->storage;
    }
#if defined(callbitmask)
    const static void* jmptable[] = {
        &&eoverflow,
        &&eargcva,
        &&eargc,
        &&eframe,
        &&callval,
    };
    UInt bitmask = callbitmask(vm, isva, arity, argc, retcnt);
    uint8_t idx = sk_ctz(bitmask);
    goto* jmptable[idx];
eoverflow:
    err = RETCNT_STACK_OVERFLOW(vm, name);
    status = S_ESOVERFLOW;
    goto err;
eargcva:
    err = FN_VA_ARGC_ERR(vm, arity, argc);
    status = S_EARGCMIN;
    goto err;
eargc:
    err = FN_ARGC_ERR(vm, arity, argc);
    status = S_EARGC;
    goto err;
eframe:
    err = FRAME_LIMIT_ERR(vm, VM_FRAMES_MAX);
    status = S_EFOVERFLOW;
err:
    push(vm, OBJ_VAL(err));
    runerror(vm, status);
#else
    if(unlikely(!sk_ensurestack(vm, retcnt))) {
        err = RETCNT_STACK_OVERFLOW(vm, name);
        status = S_ESOVERFLOW;
    } else if(unlikely(isva && arity > argc)) {
        err = FN_VA_ARGC_ERR(vm, arity, argc);
        status = S_EARGCMIN;
    } else if(unlikely(!isva && arity != argc)) {
        err = FN_ARGC_ERR(vm, arity, argc);
        status = S_EARGC;
    } else if(unlikely(vm->fc == VM_FRAMES_MAX)) {
        err = FRAME_LIMIT_ERR(vm, VM_FRAMES_MAX);
        status = S_EFOVERFLOW;
    } else goto callval;
    push(vm, OBJ_VAL(err));
    runerror(vm, status);
#endif
callval:
    frame->vacnt = argc - arity;
    frame->retcnt = retcnt;
    frame->callee = vm->sp - argc - 1;
    frame->status = S_OK;
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
static int trycall(VM* vm, Value callee, Int argc, Int retcnt)
{
    if(unlikely(!IS_OBJ(callee))) {
        push(vm, OBJ_VAL(vtostr(vm, callee)));
        push(vm, OBJ_VAL(NONCALLABLE_ERR(vm, AS_CSTRING(*stackpeek(0)))));
        runerror(vm, S_ECALLVAL);
    }
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
            else if(unlikely(argc != 0)) {
                push(vm, OBJ_VAL(FN_ARGC_ERR(vm, 0, argc)));
                runerror(vm, S_EARGC);
            }
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
static force_inline void moveresults(VM* vm, Value* fn, Int got, Int expect)
{
    Value* retstart = vm->sp - got; // start of return values
    if(expect == 0) expect = got; // all results (MULRET)
    if(got > expect) got = expect; // remove extra results
    memcpy(fn, retstart, got); // Safety: 'retstart' >= 'nativefn'
    for(int i = got; i < expect; i++) // replace missing values with nil
        fn[i] = NIL_VAL;
    vm->sp = fn + expect;
}

/* Call native function without locking (for use inside the interpreter). */
static force_inline int callnative_nolock(VM* vm, Value* retstart, Value fn, int retcnt)
{
    int n = AS_NATIVE(fn)->fn(vm);
    skapi_checkelems(vm, n);
    moveresults(vm, retstart, n, retcnt);
    return n;
}

/* Call native function. */
static force_inline int callnative(VM* vm, Value* retstart, Value fn, int retcnt)
{
    sk_unlock(vm);
    int n = AS_NATIVE(fn)->fn(vm);
    sk_lock(vm);
    skapi_checkelems(vm, n);
    moveresults(vm, retstart, n, retcnt);
    return n;
}

/* Public interface for normal call (unprotected).
 * Performs a normal function call, in case the function is
 * the skooma function it runs the interpreter and in
 * case of the native C function it calls it directly. */
int ncall(VM* vm, Value* retstart, Value fn, Int retcnt)
{
    int argc = vm->sp - retstart - 1;
    int calltype = trycall(vm, fn, argc, retcnt);
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
static force_inline int protectedcall(VM* vm, ProtectedFn fn, void* userdata)
{
    int oldfc = vm->fc;
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
int pcall(VM* vm, ProtectedFn fn, void* userdata, ptrdiff_t oldtop)
{
    int status = protectedcall(vm, fn, userdata);
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
invokefrom(VM* vm, OClass* oclass, Value name, Int argc, Int retcnt)
{
    Value method;
    if(unlikely(!HashTable_get(&oclass->methods, name, &method))) {
        const char* classname = oclass->name->storage;
        push(vm, OBJ_VAL(UNDEFINED_PROPERTY_ERR(vm, AS_CSTRING(name), classname)));
        runerror(vm, S_EUDPROPERTY);
    }
    if(trycall(vm, method, argc, retcnt) == CALL_NATIVEFN)
        callnative_nolock(vm, last_frame(vm).callee, method, retcnt);
}

/* Private to interpreter.
 * Small optimization, tries to get and call the indexed
 * value directly as a part of a single instruction. */
static force_inline void invokeindex(VM* vm, Int argc, Int retcnt)
{
    Value key = *stackpeek(argc);
    Value receiver = *stackpeek(argc + 1);
    if(unlikely(!IS_INSTANCE(receiver))) {
        push(vm, OBJ_VAL(vtostr(vm, receiver)));
        push(vm, OBJ_VAL(NOT_INSTANCE_ERR(vm, AS_CSTRING(*stackpeek(0)))));
        runerror(vm, S_EPACCESS);
    }
    OInstance* instance = AS_INSTANCE(receiver);
    Value property;
    if(HashTable_get(&instance->fields, key, &property)) goto call;
    if(unlikely(!HashTable_get(&instance->oclass->methods, key, &property))) {
        const char* classname = instance->oclass->name->storage;
        push(vm, OBJ_VAL(vtostr(vm, key)));
        const char* propname = AS_CSTRING(*stackpeek(0));
        push(vm, OBJ_VAL(UNDEFINED_PROPERTY_ERR(vm, propname, classname)));
        runerror(vm, S_EUDPROPERTY);
    }
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
static force_inline void invoke(VM* vm, Value name, Int argc, Int retcnt)
{
    Value receiver = *stackpeek(argc);
    if(unlikely(!IS_INSTANCE(receiver))) {
        push(vm, OBJ_VAL(vtostr(vm, receiver)));
        push(vm, OBJ_VAL(NOT_INSTANCE_ERR(vm, *stackpeek(0))));
        runerror(vm, S_EARG);
    }
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
        upvalp->closed.value = *upvalp->location;
        upvalp->location = &upvalp->closed.value;
        vm->open_upvals = upvalp->next;
    }
}

/**
 * Searches the entire table for the matching index in order to
 * provide more descriptive runtime error.
 * It is okay if the lookup is slow, this only gets called when runtime error
 * occurs (which is follows the end of the program execution).
 **/
static force_inline OString* globalname(VM* vm, UInt idx)
{
    for(UInt i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];
        if(!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
            return (OString*)AS_OBJ(entry->key);
    }
    unreachable;
}

void run(VM* vm)
{
#define throwerr(vm)    runerror(vm, vtostr(vm, *stackpeek(0))->storage)
#define READ_BYTE()     (*ip++)
#define READ_BYTEL()    (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT() FFN(frame)->chunk.constants.data[READ_BYTEL()]
#define READ_STRING()   AS_STRING(READ_CONSTANT())
#define BINARY_OP(vm, type, op)                                                          \
    do {                                                                                 \
        if(unlikely(!IS_NUMBER(*stackpeek(0)) || !IS_NUMBER(*stackpeek(1)))) {           \
            frame->ip = ip;                                                              \
            (vm)->sp[-1] = OBJ_VAL(BINARYOP_ERR(vm, op));                                \
            runerror(vm, S_EBINOP);                                                      \
        }                                                                                \
        double b = AS_NUMBER(pop(vm));                                                   \
        double a = AS_NUMBER(pop(vm));                                                   \
        push(vm, type(a op b));                                                          \
    } while(false)
    runtime = 1;
    // cache these hopefully in a register
    register CallFrame* frame = &vm->frames[vm->fc - 1];
    register Byte* ip = frame->ip;
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
#define BREAK                                                                            \
    dumpstack(vm, frame, ip);                                                            \
    DISPATCH(READ_BYTE())
#endif
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#ifdef DEBUG_TRACE_EXECUTION
#define BREAK                                                                            \
    dumpstack(vm, frame, ip);                                                            \
    break
#else
#define BREAK break
#endif
#endif
        DISPATCH(READ_BYTE())
        {
            CASE(OP_TRUE)
            {
                push(vm, BOOL_VAL(true));
                BREAK;
            }
            CASE(OP_FALSE)
            {
                push(vm, BOOL_VAL(false));
                BREAK;
            }
            CASE(OP_NIL)
            {
                push(vm, NIL_VAL);
                BREAK;
            }
            CASE(OP_NILN)
            {
                pushn(vm, READ_BYTEL(), NIL_VAL);
                BREAK;
            }
            CASE(OP_NEG)
            {
                Value val = *stackpeek(0);
                if(unlikely(!IS_NUMBER(val))) {
                    frame->ip = ip;
                    push(vm, OBJ_VAL(vtostr(vm, val)));
                    vm->sp[-2] = OBJ_VAL(UNARYNEG_ERR(vm, AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    runerror(vm, S_EBINOP);
                }
                AS_NUMBER_REF(vm->sp - 1) = NUMBER_VAL(-AS_NUMBER(val));
                BREAK;
            }
            CASE(OP_ADD)
            {
                Value b = *stackpeek(0);
                Value a = *stackpeek(1);
                if(IS_NUMBER(b) && IS_NUMBER(a)) {
                    double b = AS_NUMBER(pop(vm));
                    double a = AS_NUMBER(pop(vm));
                    push(vm, NUMBER_VAL((a + b)));
                } else if(IS_STRING(b) && IS_STRING(a)) {
                    push(vm, OBJ_VAL(concatenate(vm, a, b)));
                } else { // @TODO: Operator overloading or not?
                    frame->ip = ip;
                    vm->sp[-1] = OBJ_VAL(ADD_OPERATOR_ERR(vm, a, b));
                    runerror(vm, S_EBINOP);
                }
                BREAK;
            }
            CASE(OP_SUB)
            {
                BINARY_OP(vm, NUMBER_VAL, -);
                BREAK;
            }
            CASE(OP_MUL)
            {
                BINARY_OP(vm, NUMBER_VAL, *);
                BREAK;
            }
            CASE(OP_MOD)
            {
                TODO("Implement OP_MOD");
                BREAK;
            }
            CASE(OP_POW)
            {
                TODO("Implement OP_POW");
                BREAK;
            }
            CASE(OP_DIV)
            {
                BINARY_OP(vm, NUMBER_VAL, /);
                BREAK;
            }
            CASE(OP_NOT)
            {
                *stackpeek(0) = BOOL_VAL(ISFALSEY(*stackpeek(0)));
                BREAK;
            }
            CASE(OP_VALIST)
            {
                OFunction* fn = FFN(frame);
                UInt vacnt = READ_BYTEL();
                if(vacnt == 0) vacnt = frame->vacnt;
                for(UInt i = 1; i <= vacnt; i++) {
                    Value* next = frame->callee + fn->arity + i;
                    push(vm, *next);
                }
                BREAK;
            }
            CASE(OP_NOT_EQUAL)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(!veq(vm, a, b)));
                BREAK;
            }
            CASE(OP_EQUAL)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(veq(vm, a, b)));
                BREAK;
            }
            CASE(OP_EQ)
            {
                Value b = pop(vm);
                Value a = *stackpeek(0);
                push(vm, BOOL_VAL(veq(vm, a, b)));
                BREAK;
            }
            CASE(OP_GREATER)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(vgt(vm, a, b)));
                BREAK;
            }
            CASE(OP_GREATER_EQUAL)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(vge(vm, a, b)));
                BREAK;
            }
            CASE(OP_LESS)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(vlt(vm, a, b)));
                BREAK;
            }
            CASE(OP_LESS_EQUAL)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(vle(vm, a, b)));
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
                push(vm, READ_CONSTANT());
                BREAK;
            }
            CASE(OP_CALL)
            {
                Int retcnt = READ_BYTEL();
                Int argc = vm->sp - Array_VRef_pop(&vm->callstart);
                frame->ip = ip;
                if(trycall(vm, *stackpeek(argc), argc, retcnt))
                    frame = &vm->frames[vm->fc - 1];
                ip = frame->ip;
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
                Int retcnt = READ_BYTEL();
                Int argc = vm->sp - Array_VRef_pop(&vm->callstart);
                frame->ip = ip;
                if(unlikely(!invoke(vm, methodname, argc, retcnt)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip = frame->ip;
                BREAK;
            }
            CASE(OP_GET_SUPER)
            {
                Value methodname = READ_CONSTANT();
                OClass* superclass = AS_CLASS(pop(vm));
                frame->ip = ip;
                bindmethod(vm, superclass, methodname, *stackpeek(0));
                BREAK;
            }
            CASE(OP_INVOKE_SUPER)
            {
                Value methodname = READ_CONSTANT();
                ASSERT(IS_CLASS(*stackpeek(0)), "superclass must be class.");
                OClass* superclass = AS_CLASS(pop(vm));
                UInt argc = vm->sp - Array_VRef_pop(&vm->callstart);
                Int retcnt = READ_BYTEL();
                frame->ip = ip;
                invokefrom(vm, superclass, methodname, argc, retcnt);
                frame = &vm->frames[vm->fc - 1];
                ip = frame->ip;
                BREAK;
            }
            CASE(OP_SET_PROPERTY)
            {
                Value property_name = READ_CONSTANT();
                Value receiver = *stackpeek(1);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    push(vm, OBJ_VAL(vtostr(vm, receiver)));
                    vm->sp[-2] = OBJ_VAL(NOT_INSTANCE_ERR(vm, AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    frame->status = S_EPACCESS;
                    return INTERPRET_RUNTIME_ERROR;
                }
                HashTable_insert(
                    vm,
                    &AS_INSTANCE(receiver)->fields,
                    property_name,
                    *stackpeek(0));
                popn(vm, 2);
                BREAK;
            }
            CASE(OP_GET_PROPERTY)
            {
                Value property_name = READ_CONSTANT();
                Value receiver = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    push(vm, OBJ_VAL(vtostr(vm, receiver)));
                    vm->sp[-2] = OBJ_VAL(NOT_INSTANCE_ERR(vm, AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    frame->status = S_EPACCESS;
                    return INTERPRET_RUNTIME_ERROR;
                }
                OInstance* instance = AS_INSTANCE(receiver);
                Value property;
                if(HashTable_get(&instance->fields, property_name, &property)) {
                    *(vm->sp - 1) = property;
                    BREAK;
                }
                frame->ip = ip;
                bindmethod(vm, instance->oclass, property_name, receiver);
                BREAK;
            }
            {
                // Short and long instructions have identical code
                Int bcp; // Bytecode parameter
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
                    if(unlikely(!veq(vm->globvals[bcp].value, EMPTY_VAL))) {
                        frame->ip = ip;
                        vm->sp[-1] = OBJ_VAL(
                            GLOBALVAR_REDEFINITION_ERR(vm, globalname(vm, bcp)->storage));
                        frame->status = S_GLOBALREDEF;
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm->globvals[bcp].value = *stackpeek(0);
                    pop(vm);
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
                    Variable* global = &vm->globvals[bcp];
                    if(unlikely(IS_UNDEFINED(global->value))) {
                        frame->ip = ip;
                        vm->sp[-1] = OBJ_VAL(
                            UNDEFINED_GLOBAL_ERR(vm, globalname(vm, bcp)->storage));
                        frame->status = S_UDGLOBAL;
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    push(vm, global->value);
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
                    Variable* global = &vm->globvals[bcp];
                    if(unlikely(IS_UNDEFINED(global->value))) {
                        frame->ip = ip;
                        vm->sp[-1] = OBJ_VAL(
                            UNDEFINED_GLOBAL_ERR(vm, globalname(vm, bcp)->storage));
                        frame->status = S_UDGLOBAL;
                        return INTERPRET_RUNTIME_ERROR;
                    } else if(unlikely(ISFIXED(global))) {
                        frame->ip = ip;
                        OString* s = globalname(vm, bcp);
                        vm->sp[-1] = OBJ_VAL(VARIABLE_FIXED_ERR(vm, s->len, s->storage));
                        frame->status = S_EFIXEDASSING;
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    global->value = pop(vm);
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
                CASE(OP_TOPRET) // return from script
                {
                    HashTable_insert(
                        vm,
                        &vm->loaded,
                        OBJ_VAL(FFN(frame)->name),
                        TRUE_VAL);
                    goto ret_fin;
                }
                CASE(OP_RET) // function return
                {
                ret_fin:;
                    Int retcnt = vm->sp - Array_VRef_pop(&vm->retstart);
                    Int pushc;
                    if(frame->retcnt == 0) { // multiple return values ?
                        pushc = 0;
                        frame->retcnt = retcnt;
                    } else pushc = frame->retcnt - retcnt;
                    if(pushc < 0) popn(vm, -pushc);
                    else pushn(vm, pushc, NIL_VAL);
                    ASSERT(vm->temp.len == 0, "Temporary array must be empty.");
                    for(Int returns = frame->retcnt; returns--;) {
                        Array_Value_push(&vm->temp, *stackpeek(0));
                        pop(vm);
                    }
                    closeupval(vm, frame->callee);
                    vm->fc--;
                    vm->sp = frame->callee;
                    while(vm->temp.len > 0)
                        push(vm, Array_Value_pop(&vm->temp));
                    ASSERT(vm->temp.len == 0, "Temporary array must be empty.");
                    if(vm->fc == 0 || vm->lastskframe == vm->fc) return;
                    frame = &vm->frames[vm->fc - 1];
                    ip = frame->ip;
                    BREAK;
                }
            }
            CASE(OP_JMP_IF_FALSE)
            {
                UInt skip_offset = READ_BYTEL();
                ip += ((Byte)ISFALSEY(*stackpeek(0)) * skip_offset);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_POP)
            {
                UInt skip_offset = READ_BYTEL();
                ip += ISFALSEY(*stackpeek(0)) * skip_offset;
                pop(vm);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_OR_POP)
            {
                UInt skip_offset = READ_BYTEL();
                if(ISFALSEY(*stackpeek(0))) ip += skip_offset;
                else pop(vm);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_AND_POP)
            {
                UInt skip_offset = READ_BYTEL();
                if(ISFALSEY(*stackpeek(0))) {
                    ip += skip_offset;
                    pop(vm);
                }
                BREAK;
            }
            CASE(OP_JMP)
            {
                UInt skip_offset = READ_BYTEL();
                ip += skip_offset;
                BREAK;
            }
            CASE(OP_JMP_AND_POP)
            {
                UInt skip_offset = READ_BYTEL();
                ip += skip_offset;
                pop(vm);
                BREAK;
            }
            CASE(OP_LOOP)
            {
                UInt offset = READ_BYTEL();
                ip -= offset;
                BREAK;
            }
            CASE(OP_CLOSURE)
            {
                OFunction* fn = AS_FUNCTION(READ_CONSTANT());
                OClosure* closure = OClosure_new(vm, fn);
                push(vm, OBJ_VAL(closure));
                for(UInt i = 0; i < closure->upvalc; i++) {
                    Byte local = READ_BYTE();
                    Byte flags = READ_BYTE();
                    UInt idx = READ_BYTEL();
                    if(local) closure->upvals[i] = captureupval(vm, frame->callee + idx);
                    else closure->upvals[i] = frame->closure->upvals[idx];
                    closure->upvals[i]->closed.flags = flags;
                }
                BREAK;
            }
            CASE(OP_GET_UPVALUE)
            {
                UInt idx = READ_BYTEL();
                push(vm, *frame->closure->upvals[idx]->location);
                BREAK;
            }
            CASE(OP_SET_UPVALUE)
            {
                UInt idx = READ_BYTEL();
                OUpvalue* upval = frame->closure->upvals[idx];
                if(unlikely(VAR_CHECK(&upval->closed, VAR_FIXED_BIT))) {
                    frame->ip = ip;
                    OString* s = globalname(vm, idx);
                    vm->sp[-1] = OBJ_VAL(VARIABLE_FIXED_ERR(vm, s->len, s->storage));
                    frame->status = S_EFIXEDASSING;
                    return INTERPRET_RUNTIME_ERROR;
                }
                *upval->location = pop(vm);
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
                UInt last = READ_BYTEL();
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
                Value receiver = *stackpeek(1);
                Value key = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    push(vm, OBJ_VAL(vtostr(vm, receiver)));
                    vm->sp[-2] =
                        OBJ_VAL(INDEX_RECEIVER_ERR(vm, AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    frame->status = S_EPACCESS;
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(IS_NIL(key))) {
                    frame->ip = ip;
                    vm->sp[-1] = OBJ_VAL(NIL_INDEX_ERR(vm));
                    frame->status = S_EPACCESS;
                    return INTERPRET_RUNTIME_ERROR;
                }
                Value value;
                OInstance* instance = AS_INSTANCE(receiver);
                if(HashTable_get(&instance->fields, key, &value)) {
                    popn(vm, 2); // Pop key and receiver
                    push(vm, value); // Push the field value
                    BREAK;
                }
                frame->ip = ip;
                bindmethod(vm, instance->oclass, key, receiver);
                *stackpeek(2) = *stackpeek(0); // replace key with the bmethod
                vm->sp -= 2; // adjust stack pointer
                BREAK;
            }
            CASE(OP_SET_INDEX)
            {
                Value receiver = *stackpeek(2);
                Value property = *stackpeek(1);
                Value field = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    push(vm, OBJ_VAL(vtostr(vm, receiver)));
                    vm->sp[-2] =
                        OBJ_VAL(INDEX_RECEIVER_ERR(vm, AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    frame->status = S_EPACCESS;
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(IS_NIL(property))) {
                    frame->ip = ip;
                    vm->sp[-1] = OBJ_VAL(NIL_INDEX_ERR(vm));
                    frame->status = S_EPACCESS;
                    return INTERPRET_RUNTIME_ERROR;
                }
                // @TODO: Fix this up when overloading gets implemented
                HashTable_insert(vm, &AS_INSTANCE(receiver)->fields, property, field);
                popn(vm, 3);
                push(vm, field);
                BREAK;
            }
            CASE(OP_INVOKE_INDEX)
            {
                Int retcnt = READ_BYTEL();
                Int argc = vm->sp - Array_VRef_pop(&vm->callstart);
                frame->ip = ip;
                if(unlikely(!invokeindex(vm, argc, retcnt)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip = frame->ip;
                BREAK;
            }
            CASE(OP_OVERLOAD)
            {
                OClass* oclass = AS_CLASS(*stackpeek(1));
                // Right now the only thing that can be overloaded
                // is class initializer, so this parameter is not useful,
                // but if the operator overloading gets implemented
                // this will actually be index into the array of
                // overload-able methods/operators.
                Byte opn = READ_BYTE();
                UNUSED(opn);
                oclass->omethods = AS_CLOSURE(*stackpeek(0));
                ASSERT(*ip == OP_METHOD, "Expected 'OP_METHOD'.");
                BREAK;
            }
            CASE(OP_INHERIT)
            {
                ASSERT(IS_CLASS(*stackpeek(0)), "subclass must be class.");
                OClass* subclass = AS_CLASS(*stackpeek(0));
                Value superclass = *stackpeek(1);
                if(unlikely(!IS_CLASS(superclass))) {
                    frame->ip = ip;
                    push(vm, OBJ_VAL(vtostr(vm, superclass)));
                    vm->sp[-2] = OBJ_VAL(INHERIT_ERR(
                        vm,
                        otostr(vm, (O*)subclass)->storage,
                        AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    frame->status = S_EINHERIT;
                    return INTERPRET_RUNTIME_ERROR;
                }
                HashTable_into(vm, &AS_CLASS(superclass)->methods, &subclass->methods);
                subclass->omethods = AS_CLASS(superclass)->omethods;
                pop(vm); // pop subclass
                BREAK;
            }
            CASE(OP_FOREACH_PREP)
            {
                Int vars = READ_BYTEL();
                memcpy(vm->sp, stackpeek(2), 3 * sizeof(Value));
                vm->sp += 3;
                frame->ip = ip;
                if(unlikely(trycall(vm, *stackpeek(2), 2, vars) == CALL_ERR))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip = frame->ip;
                BREAK;
            }
            CASE(OP_FOREACH)
            {
                Int vars = READ_BYTEL();
                *stackpeek(vars) = *stackpeek(vars - 1); // cntlvar
                ASSERT(*ip == OP_JMP, "Expect 'OP_JMP'.");
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



void interpret(VM* vm, const char* source, const char* path)
{
    Value name = OBJ_VAL(OString_new(vm, path, strlen(path)));
    OClosure* closure = compile(vm, source, name);
    if(closure == NULL) return INTERPRET_COMPILE_ERROR;
    trycall(vm, OBJ_VAL(closure), 0, 1);
    run(vm);
}

void _cleanupvm(VM** vmp)
{
    _cleanup_function((*vmp)->F);
    sk_destroy(vmp);
}
