#include "array.h"
#include "chunk.h"
#include "common.h"
#include "core.h"
#include "debug.h"
#include "err.h"
#include "hash.h"
#include "mem.h"
#include "object.h"
#include "parser.h"
#include "skconf.h"
#include "skmath.h"
#include "value.h"
#include "vmachine.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>





#define stackpeek(top)  ((vm)->sp - ((top) + 1))
#define stack_reset(vm) (vm)->sp = (vm)->stack
#define stack_size(vm)  ((vm)->sp - (vm)->stack)






volatile Int runtime = 0; // VM is running?


#define FFN(frame) frame->closure->fn

void runerror(VM* vm, const char* errfmt, ...)
{
    fputs("\nSkooma: [runtime error]\nSkooma: ", stderr);
    va_list ap;
    va_start(ap, errfmt);
    vfprintf(stderr, errfmt, ap);
    va_end(ap);
    putc('\n', stderr);
    for(Int i = vm->fc - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        Chunk*     chunk = &frame->closure->fn->chunk;
        UInt       line = Chunk_getline(chunk, frame->ip - chunk->code.data - 1);
        Value      _;
        bool       loaded = false;
        if(HashTable_get(&vm->loaded, OBJ_VAL(FFN(frame)->name), &_)) {
            vm->script = OBJ_VAL(FFN(frame)->name);
            loaded     = true;
        }
        fprintf(
            stderr,
            "Skooma: ['%s' on line %u] in ",
            AS_CSTRING(vm->script),
            line);
        if(loaded) fprintf(stderr, "script\n");
        else fprintf(stderr, "%s()\n", FFN(frame)->name->storage);
    }
    stack_reset(vm);
}

void push(VM* vm, Value val)
{
    if(likely(vm->sp - vm->stack < (UInt)VM_STACK_MAX)) *vm->sp++ = val;
    else {
        fprintf(stderr, "VM stack overflow. Limit [%u].\n", (UInt)VM_STACK_MAX);
        CLEANUP(vm);
    }
}

force_inline Value pop(VM* vm)
{
    return *--vm->sp;
}

sstatic force_inline void pushn(VM* vm, Int n, Value val)
{
    while(n-- > 0)
        push(vm, val);
}

sstatic force_inline void popn(VM* vm, UInt n)
{
    vm->sp -= n;
}

sstatic force_inline Byte concat_bool(bool boolean, char** dest)
{
    if(boolean) {
        *dest = "true";
        return sizeof("true") - 1;
    }
    *dest = "false";
    return sizeof("false") - 1;
}

sstatic force_inline Byte concat_nil(char** str)
{
    *str = "nil";
    return sizeof("nil") - 1;
}

sstatic OString* concatenate(VM* vm, Value a, Value b)
{
    OString* left   = AS_STRING(a);
    OString* right  = AS_STRING(b);
    size_t   length = left->len + right->len;
    char     buffer[length + 1];
    memcpy(buffer, left->storage, left->len);
    memcpy(buffer + left->len, right->storage, right->len);
    buffer[length]  = '\0';
    OString* string = OString_from(vm, buffer, length);
    popn(vm, 2);
    return string;
}

sstatic force_inline OBoundMethod*
bindmethod(VM* vm, OClass* oclass, Value name, Value receiver)
{
    Value method;
    if(unlikely(!HashTable_get(&oclass->methods, name, &method))) {
        VM_PROPERTY_ERR(vm, AS_CSTRING(name), oclass->name->storage);
        return NULL;
    }
    OBoundMethod* bound_method =
        OBoundMethod_new(vm, receiver, AS_CLOSURE(method));
    return bound_method;
}

sstatic force_inline void VM_define_native(
    VM*         vm,
    const char* name,
    NativeFn    native,
    UInt        arity,
    bool        isva)
{
    push(vm, OBJ_VAL(OString_from(vm, name, strlen(name))));
    push(
        vm,
        OBJ_VAL(ONative_new(vm, AS_STRING(*stackpeek(0)), native, arity, isva)));
    UInt idx =
        GARRAY_PUSH(vm, ((Variable){.value = vm->stack[1], .flags = 0x00}));
    HashTable_insert(vm, &vm->globids, vm->stack[0], NUMBER_VAL((double)idx));
    popn(vm, 2);
}

void Config_init(Config* config)
{
    config->reallocate        = reallocate;
    config->userdata          = NULL;
    config->load_script       = NULL;
    config->rename_script     = NULL;
    config->gc_init_heap_size = 10 * (1 << 20); // 10 MiB
    config->gc_min_heap_size  = (1 << 20); // 1 MiB
    config->gc_grow_factor    = GC_HEAP_GROW_FACTOR;
}

VM* VM_new(Config* config)
{
    AllocatorFn allocate = reallocate;
    void*       userdata = NULL;
    if(config != NULL) {
        userdata = config->userdata;
        allocate = config->reallocate ? config->reallocate : reallocate;
    }
    VM* vm = allocate(NULL, sizeof(VM), userdata);
    memset(&vm->config, 0, sizeof(Config));
    if(config != NULL) {
        memcpy(&vm->config, config, sizeof(Config));
        vm->config.reallocate = allocate;
    } else Config_init(&vm->config);
    vm->fc           = 0;
    vm->objects      = NULL;
    vm->F            = NULL;
    vm->open_upvals  = NULL;
    vm->script       = NIL_VAL;
    vm->gc_allocated = 0;
    vm->gc_next      = (1 << 20); // 1 MiB
    vm->gc_flags     = 0;
    stack_reset(vm);
    HashTable_init(&vm->loaded); // Loaded scripts and their functions
    HashTable_init(&vm->globids); // Global variable identifiers
    GARRAY_INIT(vm); // Global values array
    GSARRAY_INIT(vm); // Gray stack array (no GC)
    Array_Value_init(&vm->temp, vm); // Temp values storage (return values)
    Array_VRef_init(&vm->callstart, vm);
    Array_VRef_init(&vm->retstart, vm);
    HashTable_init(&vm->strings); // Interned strings table (Weak_refs)
    memset(vm->statics, 0, sizeof(vm->statics));
    for(UInt i = 0; i < SS_SIZE; i++)
        vm->statics[i] = OString_from(vm, static_str[i].name, static_str[i].len);
    // @REFACTOR?: Maybe make the native functions private and only
    //             callable inside class instances?
    //             Upside: less branching resulting in more straightforward code.
    //             Downside: slower function call (maybe not so bad because
    //             of removal of type checking inside the native functions, needs
    //             testing)
    //
    // @TODO?: Change NativeFn signature to accept variable amount of arguments.
    //         Upside: More expressive and flexible functions.
    //         Downside: va_list parsing resulting in slower function call
    //         processing
    VM_define_native(vm, "clock", native_clock, 0, false); // GC
    VM_define_native(vm, "isfield", native_isfield, 2, false); // GC
    VM_define_native(vm, "printl", native_printl, 1, false); // GC
    VM_define_native(vm, "print", native_print, 1, false); // GC
    VM_define_native(vm, "tostr", native_tostr, 1, false); // GC
    VM_define_native(vm, "isstr", native_isstr, 1, false); // GC
    VM_define_native(vm, "strlen", native_strlen, 1, false); // GC
    VM_define_native(vm, "strpat", native_strpat, 2, false); // GC
    VM_define_native(vm, "strsub", native_strsub, 3, false); // GC
    VM_define_native(vm, "strbyte", native_strbyte, 2, false); // GC
    VM_define_native(vm, "strlower", native_strlower, 1, false); // GC
    VM_define_native(vm, "strupper", native_strupper, 1, false); // GC
    VM_define_native(vm, "strrev", native_strrev, 1, false); // GC
    VM_define_native(vm, "strconcat", native_strconcat, 2, false); // GC
    VM_define_native(vm, "byte", native_byte, 1, false); // GC
    VM_define_native(vm, "gcfactor", native_gcfactor, 1, false); // GC
    VM_define_native(vm, "gcmode", native_gcmode, 1, false); // GC
    VM_define_native(vm, "gccollect", native_gccollect, 0, false); // GC
    VM_define_native(vm, "gcleft", native_gcleft, 0, false); // GC
    VM_define_native(vm, "gcusage", native_gcusage, 0, false); // GC
    VM_define_native(vm, "gcnext", native_gcnext, 0, false); // GC
    VM_define_native(vm, "gcset", native_gcset, 1, false); // GC
    VM_define_native(vm, "gcisauto", native_gcisauto, 0, false); // GC
    VM_define_native(vm, "assert", native_assert, 1, false); // GC
    VM_define_native(vm, "assertf", native_assertf, 2, false); // GC
    VM_define_native(vm, "error", native_error, 1, false); // GC
    VM_define_native(vm, "typeof", native_typeof, 1, false); // GC
    VM_define_native(vm, "loadscript", native_loadscript, 1, false); // GC
    return vm;
}

bool fncall(VM* vm, OClosure* callee, Int argc, Int retcnt)
{
    OFunction* fn = callee->fn;
    if(unlikely(!fn->isva && (Int)fn->arity != argc)) {
        FN_ARGC_ERR(vm, fn->arity, argc);
        return false;
    }
    if(unlikely(fn->isva && (Int)fn->arity > argc)) {
        FN_VA_ARGC_ERR(vm, fn->arity, argc);
        return false;
    }
    if(unlikely(vm->fc == VM_FRAMES_MAX)) {
        VM_FRAME_LIMIT_ERR(vm, VM_FRAMES_MAX);
        return false;
    }
    fn->vacnt        = argc - fn->arity;
    CallFrame* frame = &vm->frames[vm->fc++];
    frame->retcnt    = retcnt;
    frame->closure   = callee;
    frame->ip        = fn->chunk.code.data;
    frame->sp        = vm->sp - argc - 1;
    return true;
}

sstatic force_inline bool nativecall(VM* vm, ONative* native, Int argc)
{
    if(unlikely(native->isva && native->arity > argc)) {
        FN_VA_ARGC_ERR(vm, native->arity, argc);
        return false;
    }
    if(unlikely(!native->isva && native->arity != argc)) {
        FN_ARGC_ERR(vm, native->arity, argc);
        return false;
    }
    if(likely(native->fn(vm, vm->sp - argc, argc))) {
        vm->sp -= argc;
        return true;
    } else {
        runerror(vm, AS_CSTRING(vm->sp[-argc - 1]));
        return false;
    }
}

sstatic force_inline bool instancecall(VM* vm, OClass* oclass, Int argc)
{
    vm->sp[-argc - 1] = OBJ_VAL(OInstance_new(vm, oclass));
    OClosure* init    = oclass->overloaded;
    if(init != NULL) return fncall(vm, init, argc, 1);
    else if(unlikely(argc != 0)) {
        FN_ARGC_ERR(vm, 0, argc);
        return false;
    }
    return true;
}

sstatic force_inline bool vcall(VM* vm, Value callee, Int argc, Int retcnt)
{
    if(IS_OBJ(callee)) {
        switch(OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                OBoundMethod* bound = AS_BOUND_METHOD(callee);
                vm->sp[-argc - 1]   = bound->receiver; // class instance (self)
                return fncall(vm, bound->method, argc, retcnt);
            }
            case OBJ_CLOSURE:
            case OBJ_FUNCTION:
                return fncall(vm, AS_CLOSURE(callee), argc, retcnt);
            case OBJ_CLASS:
                return instancecall(vm, AS_CLASS(callee), argc);
            case OBJ_NATIVE:
                return nativecall(vm, AS_NATIVE(callee), argc);
            default:
                break;
        }
    }
    VM_NONCALLABLE_ERR(vm, vtostr(vm, callee)->storage);
    return false;
}

sstatic force_inline bool
invokefrom(VM* vm, OClass* oclass, Value methodname, Int argc, Int retcnt)
{
    Value method;
    if(unlikely(!HashTable_get(&oclass->methods, methodname, &method))) {
        VM_PROPERTY_ERR(vm, AS_CSTRING(methodname), oclass->name->storage);
        return false;
    }
    return vcall(vm, method, argc, retcnt);
}

sstatic force_inline bool invokeindex(VM* vm, Value name, Int argc, Int retcnt)
{
    Value receiver = *stackpeek(argc);
    if(unlikely(!IS_INSTANCE(receiver))) {
        VM_INSTANCE_ERR(vm, vtostr(vm, receiver)->storage);
        return false;
    }
    OInstance* instance = AS_INSTANCE(receiver);
    Value      value;
    if(HashTable_get(&instance->fields, name, &value)) {
        vm->sp[-argc - 1] = value;
        vm->sp--; // additional argument on stack ('name')
        return vcall(vm, value, argc - 1, retcnt);
    }
    if(unlikely(!HashTable_get(&instance->oclass->methods, name, &value))) {
        VM_PROPERTY_ERR(
            vm,
            vtostr(vm, name)->storage,
            instance->oclass->name->storage);
        return false;
    }
    pop(vm); // pop the name
    return vcall(vm, value, argc - 1, retcnt);
}

sstatic force_inline bool invoke(VM* vm, Value name, Int argc, Int retcnt)
{
    Value receiver = *stackpeek(argc);
    if(unlikely(!IS_INSTANCE(receiver))) {
        VM_INSTANCE_ERR(vm, vtostr(vm, receiver)->storage);
        return false;
    }
    OInstance* instance = AS_INSTANCE(receiver);
    Value      value;
    if(HashTable_get(&instance->fields, name, &value)) {
        vm->sp[-argc - 1] = value;
        return vcall(vm, value, argc, retcnt);
    }
    return invokefrom(vm, instance->oclass, name, argc, retcnt);
}

sstatic force_inline OUpvalue* captureupval(VM* vm, Value* valp)
{
    OUpvalue** upvalpp = &vm->open_upvals;
    while(*upvalpp != NULL && (*upvalpp)->location > valp)
        upvalpp = &(*upvalpp)->next;
    if(*upvalpp != NULL && (*upvalpp)->location == valp) return *upvalpp;
    OUpvalue* upvalp = OUpvalue_new(vm, valp);
    upvalp->next     = *upvalpp;
    *upvalpp         = upvalp;
    return upvalp;
}

sstatic force_inline void closeupval(VM* vm, Value* last)
{
    while(vm->open_upvals != NULL && vm->open_upvals->location >= last) {
        OUpvalue* upvalp     = vm->open_upvals;
        upvalp->closed.value = *upvalp->location;
        upvalp->location     = &upvalp->closed.value;
        vm->open_upvals      = upvalp->next;
    }
}

/* Unescape strings before printing them when ERROR occurs. */
sstatic OString* unescape(VM* vm, OString* string)
{
    Array_Byte new;
    Array_Byte_init(&new, vm);
    Array_Byte_init_cap(&new, string->len + 1);
    for(UInt i = 0; i < string->len; i++) {
        switch(string->storage[i]) {
            case '\n':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'n');
                break;
            case '\0':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, '0');
                break;
            case '\a':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'a');
                break;
            case '\b':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'b');
                break;
            case '\33':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'e');
                break;
            case '\f':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'f');
                break;
            case '\r':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'r');
                break;
            case '\t':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 't');
                break;
            case '\v':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'v');
                break;
            default:
                Array_Byte_push(&new, string->storage[i]);
        }
    }
    OString* unescaped = OString_from(vm, (void*)new.data, new.len);
    push(vm, OBJ_VAL(unescaped));
    Array_Byte_free(&new, NULL);
    pop(vm);
    return unescaped;
}

/**
 * Searches the entire table for the matching index in order to
 * provide more descriptive runtime error.
 * It is okay if the lookup is slow, this only gets called when runtime error
 * occurs (which is the end of the program execution).
 **/
sstatic force_inline OString* globalname(VM* vm, UInt idx)
{
    for(UInt i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];
        if(!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
            return (OString*)AS_OBJ(entry->key);
    }
    unreachable;
}

sstatic sdebug void dumpstack(VM* vm, CallFrame* frame, Byte* ip)
{
    printf("           ");
    for(Value* ptr = vm->stack; ptr < vm->sp; ptr++) {
        printf("[");
        vprint(*ptr);
        printf("]");
    }
    printf("\n");
    Instruction_debug(
        &FFN(frame)->chunk,
        (UInt)(ip - FFN(frame)->chunk.code.data));
}

sstatic InterpretResult run(VM* vm)
{
#define READ_BYTE()     (*ip++)
#define READ_BYTEL()    (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT() FFN(frame)->chunk.constants.data[READ_BYTEL()]
#define READ_STRING()   AS_STRING(READ_CONSTANT())
#define BINARY_OP(value_type, op)                                               \
    do {                                                                        \
        if(unlikely(!IS_NUMBER(*stackpeek(0)) || !IS_NUMBER(*stackpeek(1)))) {  \
            frame->ip = ip;                                                     \
            runerror(vm, "Operands must be numbers (operator '" #op "').");     \
            return INTERPRET_RUNTIME_ERROR;                                     \
        }                                                                       \
        double b = AS_NUMBER(pop(vm));                                          \
        double a = AS_NUMBER(pop(vm));                                          \
        push(vm, value_type(a op b));                                           \
    } while(false)

    runtime = 1;
    // cache these hopefully in a register
    register CallFrame* frame = &vm->frames[vm->fc - 1];
    register Byte*      ip    = frame->ip;
#ifdef DEBUG_TRACE_EXECUTION
    printf("\n=== VM - execution ===\n");
#endif
    while(true) {
#ifdef S_PRECOMPUTED_GOTO
    #define OP_TABLE
    #include "jmptable.h"
    #undef OP_TABLE
    #ifdef DEBUG_TRACE_EXECUTION
        #undef BREAK
        #define BREAK                                                           \
            dumpstack(vm, frame, ip);                                           \
            DISPATCH(READ_BYTE())
    #endif
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #ifdef DEBUG_TRACE_EXECUTION
        #define BREAK                                                           \
            dumpstack(vm, frame, ip);                                           \
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
                    VM_UNARY_NEGATION_ERR(vm, vtostr(vm, val)->storage);
                    return INTERPRET_RUNTIME_ERROR;
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
                } else {
                    frame->ip = ip;
                    ADD_OPERATOR_ERR(vm, a, b);
                    return INTERPRET_RUNTIME_ERROR;
                }
                BREAK;
            }
            CASE(OP_SUB)
            {
                BINARY_OP(NUMBER_VAL, -);
                BREAK;
            }
            CASE(OP_MUL)
            {
                BINARY_OP(NUMBER_VAL, *);
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
                BINARY_OP(NUMBER_VAL, /);
                BREAK;
            }
            CASE(OP_NOT)
            {
                *(vm->sp - 1) = BOOL_VAL(ISFALSEY(*stackpeek(0)));
                BREAK;
            }
            CASE(OP_VALIST)
            {
                OFunction* fn    = FFN(frame);
                UInt       vacnt = READ_BYTEL();
                vacnt            = (vacnt == 0 ? fn->vacnt : vacnt);
                for(UInt i = 1; i <= vacnt; i++) {
                    Value* next = frame->sp + fn->arity + i;
                    push(vm, *next);
                }
                BREAK;
            }
            CASE(OP_NOT_EQUAL)
            {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(!veq(a, b)));
                BREAK;
            }
            {
                Value a, b;
                CASE(OP_EQUAL)
                {
                    b = pop(vm);
                    a = pop(vm);
                    goto op_equal_fin;
                }
                CASE(OP_EQ)
                {
                    b = pop(vm);
                    a = *stackpeek(0);
                op_equal_fin:
                    push(vm, BOOL_VAL(veq(a, b)));
                    BREAK;
                }
            }
            CASE(OP_GREATER)
            {
                BINARY_OP(BOOL_VAL, >);
                BREAK;
            }
            CASE(OP_GREATER_EQUAL)
            {
                BINARY_OP(BOOL_VAL, >=);
                BREAK;
            }
            CASE(OP_LESS)
            {
                BINARY_OP(BOOL_VAL, <);
                BREAK;
            }
            CASE(OP_LESS_EQUAL)
            {
                BINARY_OP(BOOL_VAL, <=);
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
                Int argc   = vm->sp - Array_VRef_pop(&vm->callstart);
                frame->ip  = ip;
                if(unlikely(!vcall(vm, *stackpeek(argc), argc, retcnt)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_METHOD)
            {
                Value   methodname = READ_CONSTANT();
                Value   method     = *stackpeek(0); // OFunction or OClosure
                OClass* oclass     = AS_CLASS(*stackpeek(1));
                HashTable_insert(vm, &oclass->methods, methodname, method);
                pop(vm); // pop method
                BREAK;
            }
            CASE(OP_INVOKE)
            {
                Value methodname = READ_CONSTANT();
                Int   retcnt     = READ_BYTEL();
                Int   argc       = vm->sp - Array_VRef_pop(&vm->callstart);
                frame->ip        = ip;
                if(unlikely(!invoke(vm, methodname, argc, retcnt)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_GET_SUPER)
            {
                Value   methodname = READ_CONSTANT();
                OClass* superclass = AS_CLASS(pop(vm));
                frame->ip          = ip;
                OBoundMethod* bound =
                    bindmethod(vm, superclass, methodname, *stackpeek(0));
                if(unlikely(bound == NULL)) return INTERPRET_RUNTIME_ERROR;
                vm->sp[-1] = OBJ_VAL(bound);
                BREAK;
            }
            CASE(OP_INVOKE_SUPER)
            {
                Value methodname = READ_CONSTANT();
                ASSERT(IS_CLASS(*stackpeek(0)), "superclass must be class.");
                OClass* superclass = AS_CLASS(pop(vm));
                UInt    argc       = vm->sp - Array_VRef_pop(&vm->callstart);
                Int     retcnt     = READ_BYTEL();
                frame->ip          = ip;
                if(unlikely(
                       !invokefrom(vm, superclass, methodname, argc, retcnt)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_SET_PROPERTY)
            {
                Value property_name = READ_CONSTANT();
                Value receiver      = *stackpeek(1);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    VM_INSTANCE_ERR(vm, vtostr(vm, receiver)->storage);
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
                Value receiver      = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    VM_INSTANCE_ERR(vm, vtostr(vm, receiver)->storage);
                    return INTERPRET_RUNTIME_ERROR;
                }
                OInstance* instance = AS_INSTANCE(receiver);
                Value      property;
                if(HashTable_get(&instance->fields, property_name, &property)) {
                    *(vm->sp - 1) = property;
                    BREAK;
                }
                frame->ip = ip;
                OBoundMethod* bound =
                    bindmethod(vm, instance->oclass, property_name, receiver);
                if(unlikely(bound == NULL)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                *(vm->sp - 1) = OBJ_VAL(bound);
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
                        VM_GLOBAL_UNDEFINED_ERR(
                            vm,
                            globalname(vm, bcp)->storage);
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
                        VM_GLOBAL_UNDEFINED_ERR(
                            vm,
                            globalname(vm, bcp)->storage);
                        return INTERPRET_RUNTIME_ERROR;
                    } else if(unlikely(VAR_CHECK(global, VAR_FIXED_BIT))) {
                        frame->ip     = ip;
                        OString* name = globalname(vm, bcp);
                        VM_VARIABLE_FIXED_ERR(vm, name->len, name->storage);
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
                    push(vm, frame->sp[bcp]);
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
                    frame->sp[bcp] = pop(vm);
                    BREAK;
                }
                CASE(OP_TOPRET) // return from top-level code
                {
                    Value* first = frame->sp + 1;
                    if(veq(*first, NIL_VAL)) *first = TRUE_VAL;
                    HashTable_insert(
                        vm,
                        &vm->loaded,
                        OBJ_VAL(FFN(frame)->name),
                        *first);
                    goto ret_fin;
                }
                CASE(OP_RET) // function return
                {
                ret_fin:;
                    Int retcnt = vm->sp - Array_VRef_pop(&vm->retstart);
                    Int pushc;
                    if(frame->retcnt == 0) {
                        pushc         = 0;
                        frame->retcnt = retcnt;
                    } else pushc = frame->retcnt - retcnt;
                    if(pushc < 0) popn(vm, sabs(pushc));
                    else pushn(vm, pushc, NIL_VAL);
                    ASSERT(vm->temp.len == 0, "Temporary array not empty.");
                    for(Int expected_retcnt = frame->retcnt; expected_retcnt--;)
                    {
                        Array_Value_push(&vm->temp, *stackpeek(0));
                        pop(vm);
                    }
                    closeupval(vm, frame->sp);
                    vm->fc--;
                    if(vm->fc == 0) { // end of main script
                        popn(vm, vm->sp - vm->stack);
                        return INTERPRET_OK;
                    }
                    vm->sp = frame->sp;
                    while(vm->temp.len > 0)
                        push(vm, Array_Value_pop(&vm->temp));
                    ASSERT(vm->temp.len == 0, "Temporary array not empty.");
                    frame = &vm->frames[vm->fc - 1];
                    ip    = frame->ip;
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
                if(ISFALSEY(*stackpeek(0))) ip += skip_offset;
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
                UInt skip_offset  = READ_BYTEL();
                ip               += skip_offset;
                BREAK;
            }
            CASE(OP_JMP_AND_POP)
            {
                UInt skip_offset  = READ_BYTEL();
                ip               += skip_offset;
                pop(vm);
                BREAK;
            }
            CASE(OP_LOOP)
            {
                UInt offset  = READ_BYTEL();
                ip          -= offset;
                BREAK;
            }
            CASE(OP_CLOSURE)
            {
                OFunction* fn      = AS_FUNCTION(READ_CONSTANT());
                OClosure*  closure = OClosure_new(vm, fn);
                push(vm, OBJ_VAL(closure));
                for(UInt i = 0; i < closure->upvalc; i++) {
                    Byte local = READ_BYTE();
                    Byte flags = READ_BYTE();
                    UInt idx   = READ_BYTEL();
                    if(local)
                        closure->upvals[i] = captureupval(vm, frame->sp + idx);
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
                UInt      idx   = READ_BYTEL();
                OUpvalue* upval = frame->closure->upvals[idx];
                if(unlikely(VAR_CHECK(&upval->closed, VAR_FIXED_BIT))) {
                    frame->ip = ip;
                    runerror(
                        vm,
                        "Can't assign to a variable declared as 'fixed'.");
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
                Value key      = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    VM_INDEX_RECEIVER_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(IS_NIL(key))) {
                    frame->ip = ip;
                    VM_INDEX_NIL_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }
                Value      value;
                OInstance* instance = AS_INSTANCE(receiver);
                if(HashTable_get(&instance->fields, key, &value)) {
                    popn(vm, 2); // Pop key and receiver
                    push(vm, value); // Push the field value
                    BREAK;
                }
                frame->ip = ip;
                OBoundMethod* bound =
                    bindmethod(vm, instance->oclass, key, receiver);
                if(unlikely(bound == NULL)) return INTERPRET_RUNTIME_ERROR;
                popn(vm, 2); // Pop key and receiver
                push(vm, OBJ_VAL(bound)); // Push bound method
                BREAK;
            }
            CASE(OP_SET_INDEX)
            {
                Value receiver = *stackpeek(2);
                Value property = *stackpeek(1);
                Value field    = *stackpeek(0);
                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    VM_INDEX_RECEIVER_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(IS_NIL(property))) {
                    frame->ip = ip;
                    VM_INDEX_NIL_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }
                HashTable_insert(
                    vm,
                    &AS_INSTANCE(receiver)->fields,
                    property,
                    field);
                popn(vm, 3);
                push(vm, field);
                BREAK;
            }
            CASE(OP_INVOKE_INDEX)
            {
                Int retcnt = READ_BYTEL();
                Int argc   = vm->sp - Array_VRef_pop(&vm->callstart);
                frame->ip  = ip;
                if(unlikely(
                       !invokeindex(vm, *stackpeek(argc), argc + 1, retcnt)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
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
                oclass->overloaded = AS_CLOSURE(*stackpeek(0));
                ASSERT(*ip == OP_METHOD, "Expected 'OP_METHOD'.");
                BREAK;
            }
            CASE(OP_INHERIT)
            {
                ASSERT(IS_CLASS(*stackpeek(0)), "subclass must be class.");
                OClass* subclass   = AS_CLASS(*stackpeek(0));
                Value   superclass = *stackpeek(1);
                if(unlikely(!IS_CLASS(superclass))) {
                    frame->ip = ip;
                    VM_INHERIT_ERR(
                        vm,
                        otostr(vm, (O*)subclass)->storage,
                        vtostr(vm, superclass)->storage);
                    return INTERPRET_RUNTIME_ERROR;
                }
                HashTable_into(
                    vm,
                    &AS_CLASS(superclass)->methods,
                    &subclass->methods);
                subclass->overloaded = AS_CLASS(superclass)->overloaded;
                pop(vm); // pop subclass
                BREAK;
            }
            CASE(OP_FOREACH_PREP)
            {
                Int vars = READ_BYTEL();
                memcpy(vm->sp, stackpeek(2), 3 * sizeof(Value));
                vm->sp    += 3;
                frame->ip  = ip;
                if(unlikely(!vcall(vm, *stackpeek(2), 2, vars)))
                    return INTERPRET_RUNTIME_ERROR;
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_FOREACH)
            {
                Int vars         = READ_BYTEL();
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



InterpretResult interpret(VM* vm, const char* source, const char* path)
{
    Value     name    = OBJ_VAL(OString_from(vm, path, strlen(path)));
    OClosure* closure = compile(vm, source, name);
    if(closure == NULL) return INTERPRET_COMPILE_ERROR;
    fncall(vm, closure, 0, 1);
    return run(vm);
}

void VM_free(VM* vm)
{
    if(vm == NULL) return;
    HashTable_free(vm, &vm->loaded);
    HashTable_free(vm, &vm->globids);
    GARRAY_FREE(vm);
    GSARRAY_FREE(vm);
    Array_Value_free(&vm->temp, NULL);
    Array_VRef_free(&vm->callstart, NULL);
    Array_VRef_free(&vm->retstart, NULL);
    HashTable_free(vm, &vm->strings);
    O* next;
    for(O* head = vm->objects; head != NULL; head = next) {
        next = onext(head);
        ofree(vm, head);
    }
    FREE(vm, vm);
}

void _cleanupvm(VM* vm)
{
    _cleanup_function(vm->F);
    VM_free(vm);
}
