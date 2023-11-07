#include "array.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "core.h"
#include "debug.h"
#include "err.h"
#include "hash.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "skmath.h"
#include "value.h"
#include "vmachine.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define stack_peek(top) (*((vm)->sp - ((top) + 1)))
#define stack_reset(vm) (vm)->sp = (vm)->stack
#define stack_size(vm)  ((vm)->sp - (vm)->stack)

Int runtime = 0;


void VM_error(VM* vm, const char* errfmt, ...)
{
    fputs("\n======= runtime error =======\n", stderr);
    va_list ap;
    va_start(ap, errfmt);
    vfprintf(stderr, errfmt, ap);
    va_end(ap);
    putc('\n', stderr);

    for(Int i = vm->fc - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        Chunk*     chunk = &FRAME_FN(frame)->chunk;
        UInt       line  = Chunk_getline(chunk, frame->ip - chunk->code.data - 1);

        ASSERT(FRAME_FN(frame)->name != NULL, "CallFrame function name can't be NULL.");

        Value _dummy;
        bool  loaded = false;
        if(HashTable_get(&vm->loaded, OBJ_VAL(FRAME_FN(frame)->name), &_dummy)) {
            vm->script = OBJ_VAL(FRAME_FN(frame)->name);
            loaded     = true;
        }

        fprintf(stderr, "[FILE '%s']:[line: %u] in ", AS_CSTRING(vm->script), line);

        if(loaded) {
            fprintf(stderr, "%s\n", AS_CSTRING(vm->script));
        } else {
            fprintf(stderr, "%s()\n", FRAME_FN(frame)->name->storage);
        }
    }

    stack_reset(vm);
}

void VM_push(VM* vm, Value val)
{
    if(likely(vm->sp - vm->stack < (UInt)VM_STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(stderr, "VM stack overflow. Limit [%u].\n", (UInt)VM_STACK_MAX);
        CLEANUP(vm);
    }
}

Value VM_pop(VM* vm)
{
    return *--vm->sp;
}

void VM_push_temp(VM* vm, Value temp)
{
    if(likely(vm->tempc + 1 <= S_TEMP_MAX)) {
        vm->temp[vm->tempc++] = temp;
    } else {
        fprintf(stderr, "VM temporary stack overflow. Limit [%u].\n", (UInt)S_TEMP_MAX);
        CLEANUP(vm);
    }
}

Value VM_pop_temp(VM* vm)
{
    return vm->temp[vm->tempc--];
}

sstatic force_inline void VM_popn(VM* vm, UInt n)
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

sstatic ObjString* concatenate(VM* vm, Value a, Value b)
{
    ObjString* left  = AS_STRING(a);
    ObjString* right = AS_STRING(b);

    size_t length = left->len + right->len;
    char   buffer[length + 1];
    memcpy(buffer, left->storage, left->len);
    memcpy(buffer + left->len, right->storage, right->len);
    buffer[length] = '\0';

    ObjString* string = ObjString_from(vm, buffer, length);

    VM_pop(vm); // GC
    VM_pop(vm); // GC

    return string;
}

sstatic force_inline ObjBoundMethod*
VM_bind_method(VM* vm, ObjClass* cclass, Value name, Value receiver)
{
    Value method;
    if(unlikely(!HashTable_get(&cclass->methods, name, &method))) {
        RUNTIME_INSTANCE_PROPERTY_ERR(vm, AS_CSTRING(name), cclass->name->storage);
        return NULL;
    }

    ObjBoundMethod* bound_method = ObjBoundMethod_new(vm, receiver, AS_OBJ(method)); // GC
    return bound_method;
}

sstatic force_inline void VM_define_native(VM* vm, const char* name, NativeFn native, UInt arity)
{
    VM_push(vm, OBJ_VAL(ObjString_from(vm, name, strlen(name))));
    VM_push(vm, OBJ_VAL(ObjNative_new(vm, AS_STRING(stack_peek(0)), native, arity)));

    UInt idx = GARRAY_PUSH(vm, ((Variable){.value = vm->stack[1], .flags = 0x00}));
    HashTable_insert(vm, &vm->globids, vm->stack[0], NUMBER_VAL((double)idx));

    VM_popn(vm, 2);
}

void Config_init(Config* config)
{
    config->reallocate        = reallocate;
    config->userdata          = NULL;
    config->load_script       = NULL;
    config->rename_script     = NULL;
    config->gc_init_heap_size = 10 * (1 << 20); // 10 MiB
    config->gc_min_heap_size  = (1 << 20);      // 1 MiB
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
        // Make sure we have a valid allocator
        vm->config.reallocate = allocate;
    } else {
        Config_init(&vm->config);
    }

    vm->fc           = 0;
    vm->tempc        = 0;
    vm->objects      = NULL;
    vm->compiler     = NULL;
    vm->script       = NIL_VAL;
    vm->open_upvals  = NULL;
    vm->gc_allocated = 0;
    vm->gc_next      = (1 << 20); // 1 MiB
    vm->gc_flags     = 0;
    stack_reset(vm);

    HashTable_init(&vm->loaded);  // Loaded scripts and their functions
    HashTable_init(&vm->globids); // Global variable identifiers (GC)
    GARRAY_INIT(vm);              // Global values array (GC)
    GSARRAY_INIT(vm);             // Gray stack array (no GC)
    HashTable_init(&vm->strings); // Interned strings table (Weak_refs)

    for(UInt i = 0; i < SS_SIZE; i++) {
        vm->statics[i] = NULL;
        vm->statics[i] = ObjString_from(vm, static_str[i].name, static_str[i].len);
    }

    // @REFACTOR?: Maybe make the native functions private and only
    //             callable inside class instances?
    //             Upside: less branching resulting in more straightforward code.
    //             Downside: slower function call (maybe not so bad because
    //             of removal of type checking inside the native functions, needs testing)
    //
    // @TODO?: Change NativeFn signature to accept variable amount of arguments.
    //         Upside: More expressive and flexible functions.
    //         Downside: va_list parsing resulting in slower function call processing

    // Native function definitions
    VM_define_native(vm, "clock", native_clock, 0);           // GC
    VM_define_native(vm, "isfield", native_isfield, 2);       // GC
    VM_define_native(vm, "printl", native_printl, 1);         // GC
    VM_define_native(vm, "print", native_print, 1);           // GC
    VM_define_native(vm, "tostr", native_tostr, 1);           // GC
    VM_define_native(vm, "isstr", native_isstr, 1);           // GC
    VM_define_native(vm, "strlen", native_strlen, 1);         // GC
    VM_define_native(vm, "strpat", native_strpat, 2);         // GC
    VM_define_native(vm, "strsub", native_strsub, 3);         // GC
    VM_define_native(vm, "strbyte", native_strbyte, 2);       // GC
    VM_define_native(vm, "strlower", native_strlower, 1);     // GC
    VM_define_native(vm, "strupper", native_strupper, 1);     // GC
    VM_define_native(vm, "strrev", native_strrev, 1);         // GC
    VM_define_native(vm, "strconcat", native_strconcat, 2);   // GC
    VM_define_native(vm, "byte", native_byte, 1);             // GC
    VM_define_native(vm, "gcfactor", native_gcfactor, 1);     // GC
    VM_define_native(vm, "gcmode", native_gcmode, 1);         // GC
    VM_define_native(vm, "gccollect", native_gccollect, 0);   // GC
    VM_define_native(vm, "gcleft", native_gcleft, 0);         // GC
    VM_define_native(vm, "gcusage", native_gcusage, 0);       // GC
    VM_define_native(vm, "gcnext", native_gcnext, 0);         // GC
    VM_define_native(vm, "gcset", native_gcset, 1);           // GC
    VM_define_native(vm, "gcisauto", native_gcisauto, 0);     // GC
    VM_define_native(vm, "assert", native_assert, 1);         // GC
    VM_define_native(vm, "assertf", native_assertf, 2);       // GC
    VM_define_native(vm, "error", native_error, 1);           // GC
    VM_define_native(vm, "typeof", native_typeof, 1);         // GC
    VM_define_native(vm, "loadscript", native_loadscript, 1); // GC
    return vm;
}

bool VM_call_fn(VM* vm, Obj* callee, Int argc, bool init, ObjClass* debug)
{
    ObjFunction* fn = GET_FN(callee);

    if(unlikely(!init && vm->statics[SS_INIT] == fn->name)) {
        RUNTIME_EXPLICIT_INIT_CALL(vm);
        return false;
    } else if(unlikely((Int)fn->arity != argc)) {
        if(debug != NULL) {
            ObjString* debugstr = Obj_to_str(vm, (Obj*)debug);
            VM_push(vm, OBJ_VAL(debugstr));
            RUNTIME_INSTANCE_ARGC_ERR(
                vm,
                debugstr->storage,
                Obj_to_str(vm, (Obj*)fn)->storage,
                fn->arity,
                argc);
            VM_pop(vm);
        } else {
            RUNTIME_ARGC_ERR(vm, Obj_to_str(vm, (Obj*)fn)->storage, fn->arity, argc);
        }
        return false;
    } else if(unlikely(vm->fc == VM_FRAMES_MAX)) {
        RUNTIME_INTERNAL_FRAME_LIMIT_ERR(vm, VM_FRAMES_MAX);
        return false;
    }

    CallFrame* frame = &vm->frames[vm->fc++];
    frame->fn        = callee;
    frame->ip        = fn->chunk.code.data;
    frame->sp        = vm->sp - argc - 1;

    return true;
}

sstatic force_inline bool VM_call_native(VM* vm, ObjNative* native, Int argc)
{
    if(unlikely(native->arity != argc)) {
        RUNTIME_ARGC_ERR(vm, Obj_to_str(vm, (Obj*)native)->storage, native->arity, argc);
        return false;
    }

    if(likely(native->fn(vm, vm->sp - argc, argc))) {
        vm->sp -= argc;
        return true;
    } else {
        VM_error(vm, AS_CSTRING(vm->sp[-argc - 1]));
        return false;
    }
}

sstatic force_inline bool VM_call_instance(VM* vm, ObjClass* cclass, Int argc)
{
    vm->sp[-argc - 1] = OBJ_VAL(ObjInstance_new(vm, cclass));
    Obj* init         = cclass->overloaded;
    if(init != NULL) {
        return VM_call_fn(vm, init, argc, true, cclass);
    } else if(unlikely(argc != 0)) {
        RUNTIME_INSTANCE_INIT_ARGC_ERR(vm, Obj_to_str(vm, (Obj*)cclass)->storage, argc);
        return false;
    }
    return true;
}

sstatic force_inline bool VM_call_val(VM* vm, Value fnval, Int argc)
{
    if(IS_OBJ(fnval)) {
        switch(OBJ_TYPE(fnval)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(fnval);
                vm->sp[-argc - 1]     = bound->receiver; // class instance (self)
                return VM_call_fn(
                    vm,
                    bound->method,
                    argc,
                    false,
                    AS_INSTANCE(bound->receiver)->cclass);
            }
            case OBJ_CLOSURE:
            case OBJ_FUNCTION:
                return VM_call_fn(vm, AS_OBJ(fnval), argc, false, NULL);
            case OBJ_CLASS:
                return VM_call_instance(vm, AS_CLASS(fnval), argc);
            case OBJ_NATIVE:
                return VM_call_native(vm, AS_NATIVE(fnval), argc);
            default:
                break;
        }
    }


    RUNTIME_NONCALLABLE_ERR(vm, Value_to_str(vm, fnval)->storage);
    return false;
}

sstatic force_inline bool
VM_invoke_from_class(VM* vm, ObjClass* cclass, Value method_name, Int argc)
{
    Value method;
    if(unlikely(!HashTable_get(&cclass->methods, method_name, &method))) {
        RUNTIME_INSTANCE_PROPERTY_ERR(vm, AS_CSTRING(method_name), cclass->name->storage);
        return false;
    }
    return VM_call_val(vm, method, argc);
}

sstatic force_inline bool VM_invoke_index(VM* vm, Value name, Int argc)
{
    Value receiver = stack_peek(argc);

    if(unlikely(!IS_INSTANCE(receiver))) {
        RUNTIME_INSTANCE_ERR(vm, Value_to_str(vm, receiver)->storage);
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if(HashTable_get(&instance->fields, name, &value)) {
        vm->sp[-argc - 1] = value;
        vm->sp--; // Remember we have additional argument on stack ('name')
        return VM_call_val(vm, value, argc - 1);
    }

    if(unlikely(!HashTable_get(&instance->cclass->methods, name, &value))) {
        RUNTIME_INSTANCE_PROPERTY_ERR(
            vm,
            Value_to_str(vm, name)->storage,
            instance->cclass->name->storage);
        return false;
    }

    VM_pop(vm); // pop the name
    return VM_call_val(vm, value, argc - 1);
}

sstatic force_inline bool VM_invoke(VM* vm, Value name, Int argc)
{
    Value receiver = stack_peek(argc);

    if(unlikely(!IS_INSTANCE(receiver))) {
        RUNTIME_INSTANCE_ERR(vm, Value_to_str(vm, receiver)->storage);
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if(HashTable_get(&instance->fields, name, &value)) {
        vm->sp[-argc - 1] = value;
        return VM_call_val(vm, value, argc);
    }

    return VM_invoke_from_class(vm, instance->cclass, name, argc);
}


// Bit confusing but keep in mind that pp is a double pointer to 'ObjUpValue'.
// 'pp' holds the memory location of 'UpValue->next' excluding the first iteration
// where it holds list head.
// So the 'next' field is what we are holding onto, then when we dereference
// the 'pp' we automatically dereference the 'next' field of the previous 'UpValue'.
// This basically inserts new ObjUpvalue into the vm->open_upvals singly linked list
// (in reverse stack order head:high -> tail:low) or it returns already
// inserted/existing Upvalue.
sstatic force_inline ObjUpvalue* VM_capture_upval(VM* vm, Value* var_ref)
{
    ObjUpvalue** pp = &vm->open_upvals;

    while(*pp != NULL && (*pp)->location > var_ref) {
        pp = &(*pp)->next;
    }

    // If pointers are the same we already captured
    if(*pp != NULL && (*pp)->location == var_ref) {
        return *pp;
    }

    ObjUpvalue* upval = ObjUpvalue_new(vm, var_ref);
    upval->next       = *pp;
    *pp               = upval;

    return upval;
}

sstatic force_inline void VM_close_upval(VM* vm, Value* last)
{
    while(vm->open_upvals != NULL && vm->open_upvals->location >= last) {
        // This is where closing happens, stack values
        // get new 'location' (this 'Obj').
        // Meaning when GC triggers they will get marked
        // because open_upvals is considered as a root.
        ObjUpvalue* upval = vm->open_upvals;
        upval->closed     = *upval->location;
        upval->location   = &upval->closed;
        vm->open_upvals   = upval->next;
    }
}

/* Unescape strings before printing them when error occurs. */
sstatic ObjString* unescape(VM* vm, ObjString* string)
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

    ObjString* unescaped = ObjString_from(vm, (void*)new.data, new.len);
    VM_push(vm, OBJ_VAL(unescaped));
    Array_Byte_free(&new, NULL);
    VM_pop(vm);
    return unescaped;
}

/**
 * Searches the entire table for the matching index in order to
 * provide more precise runtime error output.
 * It is okay if the lookup is slow, this only gets called when runtime error occurs.
 **/
sstatic force_inline ObjString* VM_find_glob_name(VM* vm, UInt idx)
{
    for(UInt i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];
        if(!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx) {
            return (ObjString*)AS_OBJ(entry->key);
        }
    }
    unreachable;
}

sstatic InterpretResult VM_run(VM* vm)
{
#define READ_BYTE()      (*ip++)
#define READ_BYTEL()     (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT()  FRAME_FN(frame)->chunk.constants[READ_BYTE()]
#define READ_CONSTANTL() FRAME_FN(frame)->chunk.constants[READ_BYTEL()]
#define READ_STRING()    AS_STRING(READ_CONSTANT())
#define READ_STRINGL()   AS_STRING(READ_CONSTANTL())
#define BINARY_OP(value_type, op)                                                                  \
    do {                                                                                           \
        if(unlikely(!IS_NUMBER(stack_peek(0)) || !IS_NUMBER(stack_peek(1)))) {                     \
            frame->ip = ip;                                                                        \
            VM_error(vm, "Operands must be numbers (operator '" #op "').");                        \
            return INTERPRET_RUNTIME_ERROR;                                                        \
        }                                                                                          \
        double b = AS_NUMBER(VM_pop(vm));                                                          \
        double a = AS_NUMBER(VM_pop(vm));                                                          \
        VM_push(vm, value_type(a op b));                                                           \
    } while(false)

#define CONCAT_OR_ADD(vm)                                                                          \
    do {                                                                                           \
        Value b = stack_peek(0);                                                                   \
        Value a = stack_peek(1);                                                                   \
        if(IS_NUMBER(b) && IS_NUMBER(a)) {                                                         \
            double b = AS_NUMBER(VM_pop(vm));                                                      \
            double a = AS_NUMBER(VM_pop(vm));                                                      \
            VM_push(vm, NUMBER_VAL((a + b)));                                                      \
        } else if(IS_STRING(b) && IS_STRING(a)) {                                                  \
            VM_push(vm, OBJ_VAL(concatenate(vm, a, b)));                                           \
        } else {                                                                                   \
            ObjString* astr = Value_to_str(vm, a);                                                 \
            VM_push(vm, OBJ_VAL(astr));                                                            \
            ObjString* bstr = Value_to_str(vm, b);                                                 \
            VM_push(vm, OBJ_VAL(bstr));                                                            \
            ObjString* unescaped_b = unescape(vm, bstr);                                           \
            VM_pop(vm); /* pop 'bstr' */                                                           \
            VM_push(vm, OBJ_VAL(unescaped_b));                                                     \
            ObjString* unescaped_a = unescape(vm, astr);                                           \
            VM_pop(vm); /* pop 'astr' */                                                           \
            VM_push(vm, OBJ_VAL(unescaped_a));                                                     \
            VM_error(                                                                              \
                vm,                                                                                \
                "Only two numbers can be added together or two strings "                           \
                "concatenated.\nThis is invalid: ...\"%s\" + \"%s\"...\nTry "                      \
                "instead: "                                                                        \
                "...\"%s%s%s\" + "                                                                 \
                "\"%s%s%s\"...",                                                                   \
                unescaped_a->storage,                                                              \
                unescaped_b->storage,                                                              \
                IS_STRING(a) ? "" : "tostr(",                                                      \
                unescaped_a->storage,                                                              \
                IS_STRING(a) ? "" : ")",                                                           \
                IS_STRING(b) ? "" : "tostr(",                                                      \
                unescaped_b->storage,                                                              \
                IS_STRING(b) ? "" : ")");                                                          \
            VM_pop(vm);                                                                            \
            VM_pop(vm);                                                                            \
            return INTERPRET_RUNTIME_ERROR;                                                        \
        }                                                                                          \
    } while(false)

    // Flag for gc to skip marking compiler roots ('mem.c' -> 'gc()')
    runtime = 1;
    // First frame is 'global' frame, implicit
    // function that contains all other code and/or functions
    register CallFrame* frame = &vm->frames[vm->fc - 1];
    // Keep instruction pointer in a local variable to encourage
    // compiler to keep it in a register.
    register Byte* ip = frame->ip;

#ifdef DEBUG_TRACE_EXECUTION
    printf("\n=== vmachine ===\n");
#endif
    while(true) {
#ifdef S_PRECOMPUTED_GOTO
    #define OP_TABLE
    #include "jmptable.h"
    #undef OP_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
#endif
#ifdef DEBUG_TRACE_EXECUTION
    #undef BREAK
    #define BREAK continue
        printf("           ");
        for(Value* ptr = vm->stack; ptr < vm->sp; ptr++) {
            printf("[");
            Value_print(*ptr);
            printf("]");
        }
        printf("\n");
        Instruction_debug(&FRAME_FN(frame)->chunk, (UInt)(ip - FRAME_FN(frame)->chunk.code.data));
#endif
        DISPATCH(READ_BYTE())
        {
            CASE(OP_TRUE)
            {
                VM_push(vm, BOOL_VAL(true));
                BREAK;
            }
            CASE(OP_FALSE)
            {
                VM_push(vm, BOOL_VAL(false));
                BREAK;
            }
            CASE(OP_NIL)
            {
                VM_push(vm, NIL_VAL);
                BREAK;
            }
            CASE(OP_NEG)
            {
                Value val = stack_peek(0);
                if(unlikely(!IS_NUMBER(val))) {
                    frame->ip = ip;
                    RUNTIME_UNARY_NEGATION_ERR(vm, Value_to_str(vm, val)->storage);
                    return INTERPRET_RUNTIME_ERROR;
                }
                AS_NUMBER_REF(vm->sp - 1) = NUMBER_VAL(-AS_NUMBER(val));
                BREAK;
            }
            CASE(OP_ADD)
            {
                frame->ip = ip;
                CONCAT_OR_ADD(vm);
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
            CASE(OP_DIV)
            {
                BINARY_OP(NUMBER_VAL, /);
                BREAK;
            }
            CASE(OP_NOT)
            {
                *(vm->sp - 1) = BOOL_VAL(ISFALSEY(stack_peek(0)));
                BREAK;
            }
            CASE(OP_NOT_EQUAL)
            {
                Value a = VM_pop(vm);
                Value b = VM_pop(vm);
                VM_push(vm, BOOL_VAL(!Value_eq(b, a)));
                BREAK;
            }
            CASE(OP_EQUAL)
            {
                Value b = VM_pop(vm);
                Value a = VM_pop(vm);
                VM_push(vm, BOOL_VAL(Value_eq(a, b)));
                BREAK;
            }
            CASE(OP_EQ)
            {
                Value b = VM_pop(vm);
                Value a = stack_peek(0);
                VM_push(vm, BOOL_VAL(Value_eq(a, b)));
                BREAK;
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
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_POPN)
            {
                VM_popn(vm, READ_BYTEL());
                BREAK;
            }
            {
                // Short and long instructions have identical code
                UInt  bytecode_param;
                Value constant;

                CASE(OP_CONST)
                {
                    constant = READ_CONSTANT();
                    goto const_fin;
                }
                CASE(OP_CONSTL)
                {
                    constant = READ_CONSTANTL();
                    goto const_fin;
                }
            const_fin:;
                {
                    VM_push(vm, constant);
                    BREAK;
                }
                CASE(OP_DEFINE_GLOBAL)
                {
                    bytecode_param = READ_BYTE();
                    goto define_global_fin;
                }
                CASE(OP_DEFINE_GLOBALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto define_global_fin;
                }
            define_global_fin:;
                {
                    vm->globvals[bytecode_param].value = stack_peek(0);
                    VM_pop(vm);
                    BREAK;
                }
                CASE(OP_GET_GLOBAL)
                {
                    bytecode_param = READ_BYTE();
                    goto get_global_fin;
                }
                CASE(OP_GET_GLOBALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto get_global_fin;
                }
            get_global_fin:;
                {
                    Variable* global = &vm->globvals[bytecode_param];
                    if(unlikely(IS_UNDEFINED(global->value))) {
                        frame->ip = ip;
                        RUNTIME_GLOBAL_UNDEFINED_ERR(
                            vm,
                            VM_find_glob_name(vm, bytecode_param)->storage);
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    VM_push(vm, global->value);
                    BREAK;
                }
                CASE(OP_SET_GLOBAL)
                {
                    bytecode_param = READ_BYTE();
                    goto set_global_fin;
                }
                CASE(OP_SET_GLOBALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto set_global_fin;
                }
            set_global_fin:;
                {
                    Variable* global = &vm->globvals[bytecode_param];

                    if(unlikely(IS_UNDEFINED(global->value))) {
                        frame->ip = ip;
                        RUNTIME_GLOBAL_UNDEFINED_ERR(
                            vm,
                            VM_find_glob_name(vm, bytecode_param)->storage);
                        return INTERPRET_RUNTIME_ERROR;
                    } else if(unlikely(VAR_CHECK(global, VAR_FIXED_BIT))) {
                        frame->ip       = ip;
                        ObjString* name = VM_find_glob_name(vm, bytecode_param);
                        RUNTIME_VARIABLE_FIXED_ERR(vm, name->len, name->storage);
                        return INTERPRET_RUNTIME_ERROR;
                    }

                    global->value = stack_peek(0);
                    BREAK;
                }
                CASE(OP_GET_LOCAL)
                {
                    bytecode_param = READ_BYTE();
                    goto get_local_fin;
                }
                CASE(OP_GET_LOCALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto get_local_fin;
                }
            get_local_fin:;
                {
                    VM_push(vm, frame->sp[bytecode_param]);
                    BREAK;
                }
                CASE(OP_SET_LOCAL)
                {
                    bytecode_param = READ_BYTE();
                    goto set_local_fin;
                }
                CASE(OP_SET_LOCALL)
                {
                    bytecode_param = READ_BYTEL();
                    goto set_local_fin;
                }
            set_local_fin:;
                {
                    frame->sp[bytecode_param] = stack_peek(0);
                    BREAK;
                }
                CASE(OP_CALL)
                {
                    bytecode_param = READ_BYTE();
                    goto call_fin;
                }
                CASE(OP_CALLL)
                {
                    bytecode_param = READ_BYTEL();
                    goto call_fin;
                }
            call_fin:;
                {
                    frame->ip = ip;
                    if(unlikely(!VM_call_val(vm, stack_peek(bytecode_param), bytecode_param))) {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &vm->frames[vm->fc - 1];
                    ip    = frame->ip;
                    BREAK;
                }
                CASE(OP_METHOD)
                {
                    constant = READ_CONSTANT();
                    goto method_fin;
                }
                CASE(OP_METHODL)
                {
                    constant = READ_CONSTANTL();
                    goto method_fin;
                }
            method_fin:;
                {
                    Value      method      = stack_peek(0); // Function or closure
                    ObjClass*  cclass      = AS_CLASS(stack_peek(1));
                    ObjString* method_name = AS_STRING(constant);
                    HashTable_insert(vm, &cclass->methods, OBJ_VAL(method_name), method);
                    VM_pop(vm); // pop the method (function/closure)
                    BREAK;
                }
                CASE(OP_INVOKE)
                {
                    constant       = READ_CONSTANT();
                    bytecode_param = READ_BYTEL();
                    goto invoke_fin;
                }
                CASE(OP_INVOKEL)
                {
                    constant       = READ_CONSTANTL();
                    bytecode_param = READ_BYTEL();
                    goto invoke_fin;
                }
            invoke_fin:;
                {
                    ObjString* method_name = AS_STRING(constant);
                    frame->ip              = ip;
                    if(unlikely(!VM_invoke(vm, OBJ_VAL(method_name), bytecode_param))) {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &vm->frames[vm->fc - 1];
                    ip    = frame->ip;
                    BREAK;
                }
                CASE(OP_GET_SUPER)
                {
                    constant = READ_CONSTANT();
                    goto get_super_fin;
                }
                CASE(OP_GET_SUPERL)
                {
                    constant = READ_CONSTANTL();
                    goto get_super_fin;
                }
            get_super_fin:;
                {
                    ObjClass* superclass  = AS_CLASS(VM_pop(vm));
                    frame->ip             = ip;
                    ObjBoundMethod* bound = VM_bind_method(vm, superclass, constant, stack_peek(0));
                    if(unlikely(bound == NULL)) {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm->sp[-1] = OBJ_VAL(bound); // Replace instance with bound method
                    BREAK;
                }
                CASE(OP_INVOKE_SUPER)
                {
                    constant = READ_CONSTANT();
                    goto invoke_super_fin;
                }
                CASE(OP_INVOKE_SUPERL)
                {
                    constant = READ_CONSTANT();
                    goto invoke_super_fin;
                }
            invoke_super_fin:;
                {
                    ObjClass* superclass = AS_CLASS(VM_pop(vm));
                    Int       argc       = READ_BYTEL();
                    frame->ip            = ip;

                    if(unlikely(!VM_invoke_from_class(vm, superclass, constant, argc))) {
                        return INTERPRET_RUNTIME_ERROR;
                    }

                    frame = &vm->frames[vm->fc - 1];
                    ip    = frame->ip;
                    BREAK;
                }
                CASE(OP_SET_PROPERTY)
                {
                    constant = READ_CONSTANT();
                    goto set_property_fin;
                }
                CASE(OP_SET_PROPERTYL)
                {
                    constant = READ_CONSTANTL();
                    goto set_property_fin;
                }
            set_property_fin:;
                {
                    Value val = stack_peek(1);

                    if(unlikely(!IS_INSTANCE(val))) {
                        frame->ip = ip;
                        RUNTIME_INSTANCE_ERR(vm, Value_to_str(vm, val)->storage);
                        return INTERPRET_RUNTIME_ERROR;
                    }

                    HashTable_insert(vm, &AS_INSTANCE(val)->fields, constant, stack_peek(0));
                    Value ret = VM_pop(vm); // Get assigned value
                    VM_pop(vm);             // Pop instance
                    VM_push(vm, ret);       // Push back the assigned value
                    BREAK;
                }
                CASE(OP_GET_PROPERTY)
                {
                    constant = READ_CONSTANT();
                    goto get_property_fin;
                }
                CASE(OP_GET_PROPERTYL)
                {
                    constant = READ_CONSTANTL();
                    goto get_property_fin;
                }
            get_property_fin:;
                {
                    Value receiver = stack_peek(0);

                    if(unlikely(!IS_INSTANCE(receiver))) {
                        frame->ip = ip;
                        RUNTIME_INSTANCE_ERR(vm, Value_to_str(vm, receiver)->storage);
                        return INTERPRET_RUNTIME_ERROR;
                    }

                    ObjInstance* instance = AS_INSTANCE(receiver);

                    Value property;
                    if(HashTable_get(&instance->fields, constant, &property)) {
                        VM_pop(vm);
                        VM_push(vm, property);
                        BREAK;
                    }

                    frame->ip = ip;
                    ObjBoundMethod* bound =
                        VM_bind_method(vm, instance->cclass, constant, receiver);

                    if(unlikely(bound == NULL)) {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    VM_pop(vm);                  // pop instance
                    VM_push(vm, OBJ_VAL(bound)); // Push bound method
                    BREAK;
                }
            }
            CASE(OP_JMP_IF_FALSE)
            {
                UInt skip_offset = READ_BYTEL();
                //
                ip += ((Byte)ISFALSEY(stack_peek(0)) * skip_offset);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_POP)
            {
                UInt skip_offset = READ_BYTEL();
                if(ISFALSEY(stack_peek(0))) {
                    ip += skip_offset;
                }
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_OR_POP)
            {
                UInt skip_offset = READ_BYTEL();
                if(ISFALSEY(stack_peek(0))) {
                    ip += skip_offset;
                } else {
                    VM_pop(vm);
                }
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_AND_POP)
            {
                UInt skip_offset = READ_BYTEL();
                if(ISFALSEY(stack_peek(0))) {
                    ip += skip_offset;
                    VM_pop(vm);
                }
                BREAK;
            }
            CASE(OP_JMP)
            {
                UInt skip_offset = READ_BYTEL();
                //
                ip += skip_offset;
                BREAK;
            }
            CASE(OP_JMP_AND_POP)
            {
                UInt skip_offset = READ_BYTEL();
                //
                ip += skip_offset;
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_LOOP)
            {
                UInt offset = READ_BYTEL();
                //
                ip -= offset;
                BREAK;
            }
            CASE(OP_CLOSURE)
            {
                ObjFunction* fn      = AS_FUNCTION(READ_CONSTANTL());
                ObjClosure*  closure = ObjClosure_new(vm, fn);
                VM_push(vm, OBJ_VAL(closure));

                for(UInt i = 0; i < closure->upvalc; i++) {
                    Byte local = READ_BYTE();
                    UInt idx   = READ_BYTEL();

                    if(local) {
                        closure->upvals[i] = VM_capture_upval(vm, frame->sp + idx);
                    } else {
                        closure->upvals[i] = FRAME_CLOSURE(frame)->upvals[idx];
                    }
                }
                BREAK;
            }
            CASE(OP_GET_UPVALUE)
            {
                UInt idx = READ_BYTEL();
                VM_push(vm, *FRAME_CLOSURE(frame)->upvals[idx]->location);
                BREAK;
            }
            CASE(OP_SET_UPVALUE)
            {
                UInt idx = READ_BYTEL();
                //
                *FRAME_CLOSURE(frame)->upvals[idx]->location = stack_peek(0);
                BREAK;
            }
            CASE(OP_CLOSE_UPVAL)
            {
                VM_close_upval(vm, vm->sp - 1);
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_CLOSE_UPVALN)
            {
                UInt last = READ_BYTEL();
                VM_close_upval(vm, vm->sp - last);
                VM_popn(vm, last);
                BREAK;
            }
            CASE(OP_CLASS)
            {
                VM_push(vm, OBJ_VAL(ObjClass_new(vm, READ_STRING())));
                BREAK;
            }
            CASE(OP_CLASSL)
            {
                VM_push(vm, OBJ_VAL(ObjClass_new(vm, READ_STRINGL())));
                BREAK;
            }
            CASE(OP_INDEX)
            {
                Value receiver = stack_peek(1);
                Value key      = stack_peek(0);

                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    RUNTIME_INDEX_RECEIVER_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(IS_NIL(key))) {
                    frame->ip = ip;
                    RUNTIME_INDEX_NIL_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }

                Value        value;
                ObjInstance* instance = AS_INSTANCE(receiver);

                if(HashTable_get(&instance->fields, key, &value)) {
                    VM_popn(vm, 2);     // Pop key and receiver
                    VM_push(vm, value); // Push the field value
                    BREAK;
                }

                frame->ip = ip;

                ObjBoundMethod* bound = VM_bind_method(vm, instance->cclass, key, receiver);
                if(unlikely(bound == NULL)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                VM_popn(vm, 2);              // Pop key and receiver
                VM_push(vm, OBJ_VAL(bound)); // Push bound method
                BREAK;
            }
            CASE(OP_SET_INDEX)
            {
                Value receiver = stack_peek(2);
                Value property = stack_peek(1);
                Value field    = stack_peek(0);

                if(unlikely(!IS_INSTANCE(receiver))) {
                    frame->ip = ip;
                    RUNTIME_INDEX_RECEIVER_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(IS_NIL(property))) {
                    frame->ip = ip;
                    RUNTIME_INDEX_NIL_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }

                HashTable_insert(vm, &AS_INSTANCE(receiver)->fields, property, field);
                VM_popn(vm, 3);
                VM_push(vm, field);
                BREAK;
            }
            CASE(OP_INVOKE_INDEX)
            {
                Int argc  = READ_BYTEL();
                frame->ip = ip;
                if(unlikely(!VM_invoke_index(vm, stack_peek(argc), argc + 1))) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_OVERLOAD)
            {
                // Note: Do not pop anything, next instruction is OP_METHOD
                ObjClass* cclass = AS_CLASS(stack_peek(1));
                // Right now the only thing that can be overloaded
                // is class initializer, so this is not useful,
                // but if the operator overloading gets implemented
                // this will actually be index into the array of
                // overload-able methods/operators.
                Byte opn = READ_BYTE();
                UNUSED(opn);

                cclass->overloaded = AS_OBJ(stack_peek(0));
                BREAK;
            }
            CASE(OP_INHERIT)
            {
                // Safety: It is safe to cast 'subclass' as ObjClass,
                // compiler emits OP_INHERIT only when compiling a class.
                ObjClass* subclass   = AS_CLASS(stack_peek(0));
                Value     superclass = stack_peek(1);

                if(unlikely(!IS_CLASS(superclass))) {
                    frame->ip = ip;
                    RUNTIME_INHERIT_ERR(
                        vm,
                        Obj_to_str(vm, (Obj*)subclass)->storage, // no need to VM_push
                                                                 // classes already have names
                        Value_to_str(vm, superclass)->storage);
                    return INTERPRET_RUNTIME_ERROR;
                }

                // Impl all the methods
                HashTable_into(vm, &AS_CLASS(superclass)->methods, &subclass->methods);
                // Update the overloaded methods/operators cache
                // (only initializer method overload-able for now)
                subclass->overloaded = AS_CLASS(superclass)->overloaded;
                VM_pop(vm);
                BREAK;
            }
            {
                Value retval;
                CASE(OP_TOPRET) // Called loadscript, return true instead of nil
                {
                    retval = VM_pop(vm);
                    if(retval == NIL_VAL) {
                        retval = TRUE_VAL;
                    }
                    HashTable_insert(vm, &vm->loaded, OBJ_VAL(FRAME_FN(frame)->name), retval);
                    goto ret_fin;
                }
                CASE(OP_RET)
                {
                    retval = VM_pop(vm);
                    goto ret_fin;
                }
            ret_fin:;
                {
                    VM_close_upval(vm, frame->sp);
                    vm->fc--;
                    if(vm->fc == 0) {
                        VM_pop(vm);
                        return INTERPRET_OK;
                    }
                    vm->sp = frame->sp;
                    VM_push(vm, retval);
                    frame = &vm->frames[vm->fc - 1];
                    ip    = frame->ip;
                    BREAK;
                }
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
#undef VM_CONCAT_OR_ADD
}

InterpretResult VM_interpret(VM* vm, const char* source, const char* filename)
{
    Value        name = OBJ_VAL(ObjString_from(vm, filename, strlen(filename)));
    ObjFunction* fn   = compile(vm, source, name);

    if(fn == NULL) {
        return INTERPRET_COMPILE_ERROR;
    }

    VM_call_fn(vm, (Obj*)fn, 0, false, NULL);
    return VM_run(vm);
}

void VM_free(VM* vm)
{
    HashTable_free(vm, &vm->loaded);
    HashTable_free(vm, &vm->globids);
    GARRAY_FREE(vm);
    GSARRAY_FREE(vm);
    HashTable_free(vm, &vm->strings);

    Obj* next;
    for(Obj* head = vm->objects; head != NULL; head = next) {
        next = Obj_next(head);
        Obj_free(vm, head);
    }

    vm->config.reallocate(vm, 0, vm->config.userdata);
}

void _cleanup_vm(VM* vm)
{
    _cleanup_compiler(vm, vm->compiler);
    VM_free(vm);
}
