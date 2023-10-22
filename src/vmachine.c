#include "array.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "err.h"
#include "hash.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"
#include "vmachine.h"
#ifdef DEBUG
    #include "debug.h"
#endif

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define stack_peek(top) (*((vm)->sp - (top + 1)))
#define stack_reset(vm) (vm)->sp = (vm)->stack
#define stack_size(vm)  ((vm)->sp - (vm)->stack)

#define OBJSTR(vm, obj) Obj_to_str(vm, NULL, (Obj*)obj)
#define VALSTR(vm, val) Value_to_str(vm, NULL, val)

Int runtime = 0;

void VM_error(VM* vm, const char* errfmt, ...)
{
    va_list ap;
    va_start(ap, errfmt);
    vfprintf(stderr, errfmt, ap);
    va_end(ap);
    fputs("\n", stderr);

    for(Int i = vm->fc - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        Chunk*     chunk = &frame->fn->chunk;
        UInt       line  = Chunk_getline(chunk, frame->ip - chunk->code.data - 1);

        fprintf(stderr, "[line: %u] in ", line);

        if(frame->fn->name != NULL) {
            fprintf(stderr, "%s()\n", frame->fn->name->storage);
        } else {
            fprintf(stderr, "script\n");
        }
    }

    stack_reset(vm);
}


void VM_push(VM* vm, Value val)
{
    if(likely(vm->sp - vm->stack < VM_STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        RUNTIME_INTERNAL_STACK_OVERFLOW_ERR(vm, VM_STACK_MAX);
        exit(EXIT_FAILURE);
    }
}

Value VM_pop(VM* vm)
{
    return *--vm->sp;
}

SK_INTERNAL(force_inline void) VM_popn(VM* vm, UInt n)
{
    vm->sp -= n;
}

SK_INTERNAL(force_inline bool) isfalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

SK_INTERNAL(force_inline Byte) bool_to_str(char** str, bool boolean)
{
    if(boolean) {
        *str = "true";
        return sizeof("true") - 1;
    }
    *str = "false";
    return sizeof("false") - 1;
}

SK_INTERNAL(force_inline Byte) nil_to_str(char** str)
{
    *str = "nil";
    return sizeof("nil") - 1;
}

/* Current number is stored in static memory to prevent the need
 * for allocating before creating the 'ObjString', keep in mind this
 * function is only safe for usage inside a concatenate function.
 * Additionally concatenate function guarantees there is only one possible number Value
 * (because number + number is addition and not concatenation),
 * therefore there is no need for two static buffers inside of it. */
SK_INTERNAL(force_inline Byte) double_to_str(char** str, double dbl)
{
    static char buffer[30] = {0};
    Byte        len;

    if(floor(dbl) != dbl) {
        len = snprintf(buffer, sizeof(buffer), "%f", dbl);
    } else {
        /* Quick return */
        len = snprintf(buffer, sizeof(buffer), "%ld", (ssize_t)dbl);
        goto end;
    }

    int c;
    /* Trim excess zeroes */
    for(Byte i = len - 1; i > 0; i--) {
        switch((c = buffer[i])) {
            case '0':
                len--;
                break;
            default:
                goto end;
        }
    }
end:
    *str = buffer;
    return len;
}

static ObjString* concatenate(VM* vm, Value a, Value b)
{
    Value  value[2] = {a, b};
    size_t len[2]   = {0};
    char*  str[2]   = {0};

    for(int i = 0; i < 2; i++) {
#ifdef THREADED_CODE
        static const void* jump_table[] = {
            // Keep this in the same order as in the ValueType enum
            &&vbool,   /* VAL_BOOL */
            &&vnumber, /* VAL_NUMBER */
            &&vnil,    /* VAL_NIL */
            &&vobj,    /* VAL_OBJ */
            NULL,      /* VAL_EMPTY */
            NULL,      /* VAL_DECLARED */
        };

        goto* jump_table[value[i].type];

    vbool:
        len[i] = bool_to_str(&str[i], AS_BOOL(value[i]));
        continue;
    vnumber:
        len[i] = double_to_str(&str[i], AS_NUMBER(value[i]));
        continue;
    vnil:
        len[i] = nil_to_str(&str[i]);
        continue;
    vobj:
        str[i] = AS_CSTRING(value[i]);
        len[i] = AS_STRING(value[i])->len;
        continue;
#else
        switch(value[i].type) {
            case VAL_BOOL:
                len[i] = bool_to_str(&str[i], AS_BOOL(value[i]));
                break;
            case VAL_NUMBER:
                len[i] = double_to_str(&str[i], AS_NUMBER(value[i]));
                break;
            case VAL_NIL:
                len[i] = nil_to_str(&str[i]);
                break;
            case VAL_OBJ:
                str[i] = AS_CSTRING(value[i]);
                len[i] = AS_STRING(value[i])->len;
                VM_push(vm, value[i]);
                break;
            case VAL_EMPTY:
            default:
                unreachable;
        }
#endif
    }

    size_t length = len[0] + len[1];
    char   buffer[length + 1];
    memcpy(buffer, str[0], len[0]);
    memcpy(buffer + len[0], str[1], len[1]);
    buffer[length] = '\0';

    ObjString* string = ObjString_from(vm, NULL, buffer, length);

    VM_pop(vm); // GC
    VM_pop(vm); // GC

    return string;
}

SK_INTERNAL(force_inline ObjBoundMethod*)
VM_bind_method(VM* vm, ObjInstance* instance, Value name)
{
    Value method;
    if(unlikely(!HashTable_get(&instance->cclass->methods, name, &method))) {
        RUNTIME_INSTANCE_PROPERTY_ERR(vm, OBJSTR(vm, instance), AS_CSTRING(name));
        return NULL;
    }

    ObjBoundMethod* bound_method =
        ObjBoundMethod_new(vm, NULL, OBJ_VAL(instance), AS_OBJ(method)); // GC

    return bound_method;
}

SK_INTERNAL(force_inline void)
VM_define_native(VM* vm, const char* name, NativeFn native, UInt arity)
{
    VM_push(vm, OBJ_VAL(ObjString_from(vm, NULL, name, strlen(name))));
    VM_push(
        vm,
        OBJ_VAL(ObjNative_new(vm, NULL, AS_STRING(stack_peek(0)), native, arity)));

    UInt idx = GARRAY_PUSH(vm, NULL, ((Global){vm->stack[1], false}));               // GC
    HashTable_insert(vm, NULL, &vm->globids, vm->stack[0], NUMBER_VAL((double)idx)); // GC

    VM_pop(vm);
    VM_pop(vm);
}

//-------------------------NATIVE FUNCTIONS-------------------------//
/**
 * Determines processor time.
 * @ret - approximation of processor time used by the program in seconds.
 * @err - if the processor time used is not available or its value cannot
 *        be represented.
 **/
SK_INTERNAL(force_inline bool) native_clock(VM* vm, Value* argv)
{
    clock_t time = clock();

    if(likely(time != -1)) {
        argv[-1] = NUMBER_VAL((double)time / CLOCKS_PER_SEC);
        return true;
    }

    argv[-1] = OBJ_VAL(ERR_NEW(vm, CLOCK_ERR));
    return false;
}

#define NATIVE_FIELD_ERR(argv, name)                                                     \
    do {                                                                                 \
        ObjString* err = NULL;                                                           \
        if(unlikely(!IS_INSTANCE(argv[0]))) {                                            \
            err = ERR_NEW(vm, name##_INSTANCE_ERR);                                      \
        } else if(unlikely(!IS_STRING(argv[1]))) {                                       \
            err = ERR_NEW(vm, name##_FIELD_ERR);                                         \
        }                                                                                \
        argv[-1] = OBJ_VAL(err);                                                         \
    } while(false)

/**
 * Checks if ObjInstance contains field.
 * @ret - bool, true if it contains, false otherwise
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
SK_INTERNAL(force_inline bool) native_isfield(VM* vm, Value* argv)
{
    if(likely(IS_INSTANCE(argv[0]) && IS_STRING(argv[1]))) {
        ObjInstance* instance = AS_INSTANCE(argv[0]);
        Value        _dummy;
        argv[-1] = BOOL_VAL(HashTable_get(&instance->fields, argv[1], &_dummy));
        return true;
    }

    NATIVE_FIELD_ERR(argv, ISFIELD);
    return false;
}

/**
 * Deletes the field from ObjInstance.
 * @ret - bool, true if field was removed false otherwise
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
SK_INTERNAL(force_inline bool) native_delfield(VM* vm, Value* argv)
{
    if(likely(IS_INSTANCE(argv[0]) && IS_STRING(argv[1]))) {
        ObjInstance* instance = AS_INSTANCE(argv[0]);
        argv[-1]              = BOOL_VAL(HashTable_remove(&instance->fields, argv[1]));
        return true;
    }

    NATIVE_FIELD_ERR(argv, DELFIELD);
    return false;
}

/**
 * Creates or sets the value the field of ObjInstance.
 * @ret - bool, true if field was created otherwise false (field value changed)
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
SK_INTERNAL(force_inline bool) native_setfield(VM* vm, Value* argv)
{
    if(likely(IS_INSTANCE(argv[0]) && IS_STRING(argv[1]))) {
        ObjInstance* instance = AS_INSTANCE(argv[0]);
        argv[-1] =
            BOOL_VAL(HashTable_insert(vm, NULL, &instance->fields, argv[1], argv[2]));
        return true;
    }

    NATIVE_FIELD_ERR(argv, SETFIELD);
    return false;
}

/**
 * Prints a string and a newline.
 **/
SK_INTERNAL(force_inline bool) native_printl(unused VM* _, Value* argv)
{
    Value_print(argv[0]);
    printf("\n");
    argv[-1] = BOOL_VAL(true);
    return true;
}


/*
 *
 */
/*======================= VM core functions ==========================*/

// @TODO: After implementing classes, define garbage collector
// class with its own interface of native functions
void VM_init(VM* vm)
{
    vm->fc           = 0;
    vm->objects      = NULL;
    vm->open_upvals  = NULL;
    vm->gc_allocated = 0;
    vm->gc_next      = MIB(1);
    vm->gc_flags     = 0;
    stack_reset(vm);

    HashTable_init(&vm->globids); // Global variable identifiers (GC)
    GARRAY_INIT(vm);              // Global values array (GC)

    HashTable_init(&vm->strings); // Interned strings table (Weak_refs)

    Array_ObjRef_init(&vm->gray_stack); // Gray stack (NO GC)

    // Class initializer string
    vm->initstr = NULL; // GC stuff
    vm->initstr = ObjString_from(vm, NULL, "__init__", sizeof("__init__") - 1);

    // Native function definitions
    VM_define_native(vm, "clock", native_clock, 0);       // GC
    VM_define_native(vm, "isfield", native_isfield, 2);   // GC
    VM_define_native(vm, "delfield", native_delfield, 2); // GC
    VM_define_native(vm, "setfield", native_setfield, 3); // GC
    VM_define_native(vm, "printl", native_printl, 1);     // GC
}

#define CALL_FN_OR_CLOSURE(vm, obj, argc)                                                \
    ({                                                                                   \
        bool ret;                                                                        \
        if(Obj_type(obj) == OBJ_CLOSURE) {                                               \
            ObjClosure* closure = (ObjClosure*)(obj);                                    \
            ret                 = VM_call_fn(vm, closure, closure->fn, argc);            \
        } else {                                                                         \
            ret = VM_call_fn(vm, NULL, (ObjFunction*)(obj), argc);                       \
        }                                                                                \
        ret;                                                                             \
    })

SK_INTERNAL(bool) VM_call_fn(VM* vm, ObjClosure* closure, ObjFunction* fn, Int argc)
{
    if(unlikely((Int)fn->arity != argc)) {
        RUNTIME_ARGC_ERR(vm, OBJSTR(vm, fn), fn->arity, argc);
        return false;
    }

    if(unlikely(vm->fc == VM_FRAMES_MAX)) {
        RUNTIME_INTERNAL_FRAME_LIMIT_ERR(vm, VM_FRAMES_MAX);
    }

    CallFrame* frame = &vm->frames[vm->fc++];
    frame->closure   = closure;
    frame->fn        = fn;
    frame->ip        = fn->chunk.code.data;
    frame->sp        = vm->sp - argc - 1;

    return true;
}

SK_INTERNAL(force_inline bool) VM_call_native(VM* vm, ObjNative* native, Int argc)
{
    if(unlikely(native->arity != argc)) {
        RUNTIME_ARGC_ERR(vm, OBJSTR(vm, native), native->arity, argc);
        return false;
    }

    if(likely(native->fn(vm, vm->sp - argc))) {
        vm->sp -= argc;
        return true;
    } else {
        VM_error(vm, AS_CSTRING(vm->sp[-argc - 1]));
        return false;
    }
}

SK_INTERNAL(force_inline bool) VM_call_instance(VM* vm, ObjClass* cclass, Int argc)
{
    vm->sp[-argc - 1] = OBJ_VAL(ObjInstance_new(vm, NULL, cclass));
    Value init;
    if(HashTable_get(&cclass->methods, OBJ_VAL(vm->initstr), &init)) {
        return CALL_FN_OR_CLOSURE(vm, AS_OBJ(init), argc);
    } else if(unlikely(argc != 0)) {
        RUNTIME_INITIALIZER_ARGC_ERR(vm, OBJSTR(vm, cclass), vm->initstr->storage, argc);
        return false;
    }
    return true;
}

SK_INTERNAL(force_inline bool) VM_call_val(VM* vm, Value fnval, Int argc)
{
    if(IS_OBJ(fnval)) {
        switch(OBJ_TYPE(fnval)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(fnval);
                vm->sp[-argc - 1]     = bound->receiver; // class instance (self)
                return CALL_FN_OR_CLOSURE(vm, bound->method, argc);
            }
            case OBJ_FUNCTION:
                return VM_call_fn(vm, NULL, AS_FUNCTION(fnval), argc);
            case OBJ_CLOSURE: {
                ObjClosure* closure = AS_CLOSURE(fnval);
                return VM_call_fn(vm, closure, closure->fn, argc);
            }
            case OBJ_CLASS:
                return VM_call_instance(vm, AS_CLASS(fnval), argc);
            case OBJ_NATIVE:
                return VM_call_native(vm, AS_NATIVE(fnval), argc);
            default:
                break;
        }
    }


    RUNTIME_NONCALLABLE_ERR(vm, VALSTR(vm, fnval));
    return false;
}

SK_INTERNAL(force_inline bool)
VM_invoke_from_class(VM* vm, ObjClass* cclass, Value method_name, Int argc)
{
    Value method;
    if(unlikely(!HashTable_get(&cclass->methods, method_name, &method))) {
        RUNTIME_INSTANCE_PROPERTY_ERR(vm, OBJSTR(vm, cclass), AS_CSTRING(method_name));
        return false;
    }
    return VM_call_val(vm, method, argc);
}

SK_INTERNAL(force_inline bool) VM_invoke(VM* vm, Value name, Int argc)
{
    Value receiver = stack_peek(argc);
    if(unlikely(!IS_INSTANCE(receiver))) {
        RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, receiver));
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
// Think about it in a way that 'pp' holds the memory location of 'UpValue->next'
// excluding the first iteration where it holds list head.
// So the 'next' field is what we are holding onto, then when we dereference
// the 'pp' we automatically dereference the 'next' field of the previous 'UpValue'.
// This basically inserts new ObjUpvalue into the vm->open_upvals singly linked list
// (in reverse stack order head:high -> tail:low) or it returns already inserted/existing
// Upvalue.
SK_INTERNAL(force_inline ObjUpvalue*) VM_capture_upval(VM* vm, Value* var_ref)
{
    ObjUpvalue** pp = &vm->open_upvals;

    while(*pp != NULL && (*pp)->location > var_ref) {
        pp = &(*pp)->next;
    }

    // If pointers are the same we already captured
    if(*pp != NULL && (*pp)->location == var_ref) {
        return *pp;
    }

    ObjUpvalue* upval = ObjUpvalue_new(vm, NULL, var_ref);
    upval->next       = *pp;
    *pp               = upval;

    return upval;
}

SK_INTERNAL(force_inline void) VM_close_upval(VM* vm, Value* last)
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

SK_INTERNAL(InterpretResult) VM_run(VM* vm)
{
#define READ_BYTE()      (*ip++)
#define READ_BYTEL()     (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT()  frame->fn->chunk.constants[READ_BYTE()]
#define READ_CONSTANTL() frame->fn->chunk.constants[READ_BYTEL()]
#define READ_STRING()    AS_STRING(READ_CONSTANT())
#define READ_STRINGL()   AS_STRING(READ_CONSTANTL())
#define DISPATCH(x)      switch(READ_BYTE())
#define CASE(label)      case label:
#define BREAK            break
#define BINARY_OP(value_type, op)                                                        \
    do {                                                                                 \
        if(unlikely(!IS_NUMBER(stack_peek(0)) || !IS_NUMBER(stack_peek(1)))) {           \
            frame->ip = ip;                                                              \
            VM_error(vm, "Operands must be numbers (operator '" #op "').");              \
            return INTERPRET_RUNTIME_ERROR;                                              \
        }                                                                                \
        double b = AS_NUMBER(VM_pop(vm));                                                \
        double a = AS_NUMBER(VM_pop(vm));                                                \
        VM_push(vm, value_type(a op b));                                                 \
    } while(false)

#define CONCAT_OR_ADD()                                                                  \
    do {                                                                                 \
        if(IS_NUMBER(stack_peek(0)) && IS_NUMBER(stack_peek(1))) {                       \
            double b = AS_NUMBER(VM_pop(vm));                                            \
            double a = AS_NUMBER(VM_pop(vm));                                            \
            VM_push(vm, NUMBER_VAL((a + b)));                                            \
        } else {                                                                         \
            Value b = stack_peek(0);                                                     \
            Value a = stack_peek(1);                                                     \
            VM_push(vm, OBJ_VAL(concatenate(vm, a, b)));                                 \
        }                                                                                \
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
#ifdef THREADED_CODE
    #define OP_TABLE
    #include "jmptable.h"
    #undef OP_TABLE
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
        Instruction_debug(&frame->fn->chunk, (UInt)(ip - frame->fn->chunk.code.data));
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
                if(unlikely(!IS_NUMBER(stack_peek(0)))) {
                    RUNTIME_UNARY_NEGATION_ERR(vm, VALSTR(vm, stack_peek(0)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                AS_NUMBER_REF(vm->sp - 1) = -AS_NUMBER(stack_peek(0));
                BREAK;
            }
            CASE(OP_ADD)
            {
                CONCAT_OR_ADD();
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
                *(vm->sp - 1) = BOOL_VAL(isfalsey(stack_peek(0)));
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
            CASE(OP_PRINT)
            {
                Value_print(VM_pop(vm));
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
            CASE(OP_CONST)
            {
                VM_push(vm, READ_CONSTANT());
                BREAK;
            }
            CASE(OP_CONSTL)
            {
                VM_push(vm, READ_CONSTANTL());
                BREAK;
            }
            CASE(OP_DEFINE_GLOBAL)
            {
                Byte idx = READ_BYTE();
                //
                vm->globvals[idx].value = stack_peek(0);
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_DEFINE_GLOBALL)
            {
                UInt idx = READ_BYTEL();
                //
                vm->globvals[idx].value = stack_peek(0);
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_GET_GLOBAL)
            {
                Global* global = &vm->globvals[READ_BYTE()];

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    RUNTIME_GLOBAL_UNDEFINED_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }

                VM_push(vm, global->value);
                BREAK;
            }
            CASE(OP_GET_GLOBALL)
            {
                Global* global = &vm->globvals[READ_BYTEL()];

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    RUNTIME_GLOBAL_UNDEFINED_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }

                VM_push(vm, global->value);
                BREAK;
            }
            CASE(OP_SET_GLOBAL)
            {
                Global* global = &vm->globvals[READ_BYTE()];

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    RUNTIME_GLOBAL_UNDEFINED_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(GLOB_CHECK(global, GLOB_FIXED_BIT))) {
                    frame->ip = ip;
                    RUNTIME_GLOBAL_FIXED_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }

                global->value = stack_peek(0);
                BREAK;
            }
            CASE(OP_SET_GLOBALL)
            {
                Global* global = &vm->globvals[READ_BYTEL()];

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    RUNTIME_GLOBAL_UNDEFINED_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(GLOB_CHECK(global, GLOB_FIXED_BIT))) {
                    frame->ip = ip;
                    RUNTIME_GLOBAL_FIXED_ERR(vm);
                    return INTERPRET_RUNTIME_ERROR;
                }

                global->value = stack_peek(0);
                BREAK;
            }
            CASE(OP_GET_LOCAL)
            {
                Byte slot = READ_BYTE();
                VM_push(vm, frame->sp[slot]);
                BREAK;
            }
            CASE(OP_GET_LOCALL)
            {
                UInt slot = READ_BYTEL();
                VM_push(vm, frame->sp[slot]);
                BREAK;
            }
            CASE(OP_SET_LOCAL)
            {
                Byte slot       = READ_BYTE();
                frame->sp[slot] = stack_peek(0);
                BREAK;
            }
            CASE(OP_SET_LOCALL)
            {
                UInt slot       = READ_BYTEL();
                frame->sp[slot] = stack_peek(0);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE)
            {
                UInt skip_offset = READ_BYTEL();
                //
                ip += ((Byte)isfalsey(stack_peek(0)) * skip_offset);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_OR_POP)
            {
                UInt skip_offset = READ_BYTEL();
                if(isfalsey(stack_peek(0))) {
                    ip += skip_offset;
                } else {
                    VM_pop(vm);
                }
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE_AND_POP)
            {
                UInt skip_offset = READ_BYTEL();
                //
                ip += ((Byte)isfalsey(stack_peek(0)) * skip_offset);
                VM_pop(vm);
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
            CASE(OP_CALL)
            {
                Int argc  = READ_BYTE();
                frame->ip = ip;
                if(unlikely(!VM_call_val(vm, stack_peek(argc), argc))) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_CALLL)
            {
                Int argc  = READ_BYTEL();
                frame->ip = ip;
                if(unlikely(!VM_call_val(vm, stack_peek(argc), argc))) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_CLOSURE)
            {
                ObjFunction* fn      = AS_FUNCTION(READ_CONSTANTL());
                ObjClosure*  closure = ObjClosure_new(vm, NULL, fn);
                VM_push(vm, OBJ_VAL(closure));

                for(UInt i = 0; i < closure->upvalc; i++) {
                    Byte local = READ_BYTE();
                    UInt idx   = READ_BYTEL();

                    if(local) {
                        closure->upvals[i] = VM_capture_upval(vm, frame->sp + idx);
                    } else {
                        closure->upvals[i] = frame->closure->upvals[idx];
                    }
                }
                BREAK;
            }
            CASE(OP_GET_UPVALUE)
            {
                UInt idx = READ_BYTEL();
                VM_push(vm, *frame->closure->upvals[idx]->location);
                BREAK;
            }
            CASE(OP_SET_UPVALUE)
            {
                UInt idx = READ_BYTEL();
                //
                *frame->closure->upvals[idx]->location = stack_peek(0);
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
                VM_push(vm, OBJ_VAL(ObjClass_new(vm, NULL, READ_STRING())));
                BREAK;
            }
            CASE(OP_CLASSL)
            {
                VM_push(vm, OBJ_VAL(ObjClass_new(vm, NULL, READ_STRINGL())));
                BREAK;
            }
            CASE(OP_SET_PROPERTY)
            {
                if(unlikely(!IS_INSTANCE(stack_peek(1)))) {
                    frame->ip = ip;
                    RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, stack_peek(1)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(stack_peek(1));
                HashTable_insert(
                    vm,
                    NULL,
                    &instance->fields,
                    OBJ_VAL(READ_STRING()),
                    stack_peek(0));
                Value val = VM_pop(vm); // Get assigned value
                VM_pop(vm);             // Pop instance
                VM_push(vm, val);       // Push back the assigned value
                BREAK;
            }
            CASE(OP_SET_PROPERTYL)
            {
                if(unlikely(!IS_INSTANCE(stack_peek(1)))) {
                    frame->ip = ip;
                    RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, stack_peek(1)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(stack_peek(1));
                HashTable_insert(
                    vm,
                    NULL,
                    &instance->fields,
                    OBJ_VAL(READ_STRINGL()),
                    stack_peek(0));
                Value val = VM_pop(vm); // Get assigned value
                VM_pop(vm);             // Pop instance
                VM_push(vm, val);       // Push back the assigned value
                BREAK;
            }
            CASE(OP_GET_PROPERTY)
            {
                if(unlikely(!IS_INSTANCE(stack_peek(0)))) {
                    frame->ip = ip;
                    RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, stack_peek(0)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(stack_peek(0));
                Value        name     = OBJ_VAL(READ_STRING());
                Value        value;
                if(HashTable_get(&instance->fields, name, &value)) {
                    VM_pop(vm);
                    VM_push(vm, value);
                    BREAK;
                }
                frame->ip             = ip;
                ObjBoundMethod* bound = VM_bind_method(vm, instance, name);
                if(unlikely(bound == NULL)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                VM_pop(vm);                  // pop instance
                VM_push(vm, OBJ_VAL(bound)); // Push bound method
                BREAK;
            }
            CASE(OP_GET_PROPERTYL)
            {
                if(unlikely(!IS_INSTANCE(stack_peek(0)))) {
                    frame->ip = ip;
                    RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, stack_peek(0)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(stack_peek(0));
                Value        name     = OBJ_VAL(READ_STRINGL());
                Value        value;
                if(HashTable_get(&instance->fields, name, &value)) {
                    VM_pop(vm);
                    VM_push(vm, value);
                    BREAK;
                }
                frame->ip             = ip;
                ObjBoundMethod* bound = VM_bind_method(vm, instance, name);
                if(unlikely(bound == NULL)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                VM_pop(vm);                  // pop instance
                VM_push(vm, OBJ_VAL(bound)); // Push bound method
                BREAK;
            }
            CASE(OP_SET_DYNPROPERTY)
            {
                if(unlikely(!IS_INSTANCE(stack_peek(2)))) {
                    frame->ip = ip;
                    RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, stack_peek(2)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(stack_peek(2));
                HashTable_insert(
                    vm,
                    NULL,
                    &instance->fields,
                    stack_peek(1),
                    stack_peek(0));
                Value val = VM_pop(vm); // Get assigned value
                VM_popn(vm, 2);         // Pop instance and name
                VM_push(vm, val);       // Push back the assigned value
                BREAK;
            }
            CASE(OP_GET_DYNPROPERTY)
            {
                if(unlikely(!IS_INSTANCE(stack_peek(1)))) {
                    frame->ip = ip;
                    RUNTIME_INSTANCE_ERR(vm, VALSTR(vm, stack_peek(1)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(stack_peek(1));
                Value        name     = stack_peek(0);
                Value        value;
                if(HashTable_get(&instance->fields, name, &value)) {
                    VM_popn(vm, 2);
                    VM_push(vm, value);
                    BREAK;
                }
                frame->ip             = ip;
                ObjBoundMethod* bound = VM_bind_method(vm, instance, name);
                if(unlikely(bound == NULL)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                VM_popn(vm, 2);
                VM_push(vm, OBJ_VAL(bound));
                BREAK;
            }
            CASE(OP_METHOD)
            {
                Value     method = stack_peek(0); // Function or closure
                ObjClass* cclass = AS_CLASS(stack_peek(1));
                HashTable_insert(
                    vm,
                    NULL,
                    &cclass->methods,
                    OBJ_VAL(READ_STRING()),
                    method); // GC
                VM_pop(vm);  // pop the method (function/closure)
                BREAK;
            }
            CASE(OP_METHODL)
            {
                Value     method = stack_peek(0); // Function or closure
                ObjClass* cclass = AS_CLASS(stack_peek(1));
                HashTable_insert(
                    vm,
                    NULL,
                    &cclass->methods,
                    OBJ_VAL(READ_STRINGL()),
                    method); // GC
                VM_pop(vm);  // pop the method (function/closure)
                BREAK;
            }
            CASE(OP_INVOKE)
            {
                // Avoid allocating ObjBoundMethod and do immediate method call
                ObjString* method_name = READ_STRING();
                Value      argc        = READ_CONSTANTL();
                frame->ip              = ip;
                if(!VM_invoke(vm, OBJ_VAL(method_name), (Int)AS_NUMBER(argc))) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_INVOKEL)
            {
                // Avoid allocating ObjBoundMethod and do immediate method call long
                ObjString* method_name = READ_STRINGL();
                Value      argc        = READ_CONSTANTL();
                frame->ip              = ip;
                if(!VM_invoke(vm, OBJ_VAL(method_name), (Int)AS_NUMBER(argc))) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
                BREAK;
            }
            CASE(OP_RET)
            {
                Value retval = VM_pop(vm);
                VM_close_upval(vm, frame->sp);
                vm->fc--;
                if(vm->fc == 0) {
                    // @FIX: REPL not working, maybe (sp - stack) == -1 (underflow)
                    VM_pop(vm);
                    return INTERPRET_OK;
                }
                vm->sp = vm->frames[vm->fc].sp;
                VM_push(vm, retval);
                frame = &vm->frames[vm->fc - 1];
                ip    = frame->ip;
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
#undef VM_CONCAT_OR_ADD
}

InterpretResult VM_interpret(VM* vm, const char* source)
{
    ObjFunction* fn = compile(vm, source);

    if(fn == NULL) {
        return INTERPRET_COMPILE_ERROR;
    }

    VM_push(vm, OBJ_VAL(fn));
    VM_call_fn(vm, NULL, fn, 0);

    return VM_run(vm);
}

void VM_free(VM* vm)
{
    HashTable_free(vm, NULL, &vm->globids);
    GARRAY_FREE(vm);
    HashTable_free(vm, NULL, &vm->strings);
    Array_ObjRef_free(&vm->gray_stack, NULL);
    vm->initstr = NULL;

    Obj* next;
    for(Obj* head = vm->objects; head != NULL; head = next) {
        next = Obj_next(head);
        Obj_free(vm, NULL, head);
    }

    FREE(vm);
}
