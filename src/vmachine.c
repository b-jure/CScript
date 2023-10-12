#include "array.h"
#include "common.h"
#include "compiler.h"
#include "err.h"
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

#define stack_peek(top) ((Value) * ((vm)->sp - (top + 1)))
#define stack_reset(vm) (vm)->sp = (vm)->stack
#define stack_size(vm)  ((vm)->sp - (vm)->stack)

Int runtime = 0;

void VM_push(VM* vm, Value val)
{
    if(likely(vm->sp - vm->stack < VM_STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(
            stderr,
            "Internal error: vm stack overflow, stack size limit reached [%d].\n",
            VM_STACK_MAX);
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

SK_INTERNAL(force_inline void)
VM_define_native(VM* vm, const char* name, NativeFn native, UInt arity)
{
    VM_push(vm, OBJ_VAL(ObjString_from(&(Roots){NULL, vm}, name, strlen(name)))); // GC
    VM_push(vm, OBJ_VAL(ObjNative_new(&(Roots){NULL, vm}, native, arity)));       // GC

    UInt idx = Array_Global_push(&vm->global_vals, (Global){vm->stack[1], false});
    HashTable_insert(&vm->global_ids, vm->stack[0], NUMBER_VAL((double)idx));

    VM_pop(vm); // GC
    VM_pop(vm); // GC
}

//-------------------------NATIVE FUNCTIONS-------------------------//
SK_INTERNAL(force_inline bool) native_clock(VM* vm, unused Int _, unused Value* argv)
{
    clock_t time = clock();

    if(unlikely(time < 0)) {
        Roots roots = {NULL, vm};
        argv[-1]    = OBJ_VAL(ERR_NEW(&roots, CLOCK_ERR));
        return false;
    }

    argv[-1] = NUMBER_VAL((double)time / CLOCKS_PER_SEC);
    return true;
}
// END OF NATIVE FUNCTIONS~

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

    ObjString* string = ObjString_from(&(Roots){NULL, vm}, buffer, length);

    VM_pop(vm); // GC
    VM_pop(vm); // GC

    return string;
}

/*
 *
 */
/*======================= VM core functions ==========================*/

void VM_init(VM* vm, void* roots)
{
    vm->fc           = 0;
    vm->objects      = NULL;
    vm->open_upvals  = NULL;
    vm->gc_allocated = 0;
    vm->gc_next      = MIB(1);
    stack_reset(vm);

    // @TODO: Make HashTable internally contain roots in order
    // to use gc_reallocate instead of non-gc reallocate.
    HashTable_init(&vm->global_ids);
    Array_Global_init(&vm->global_vals, roots, gc_reallocate);
    HashTable_init(&vm->strings);
    Array_ObjRef_init(&vm->gray_stack, NULL, arr_reallocate);

    // Native function definitions
    VM_define_native(vm, "clock", native_clock, 0);
}

SK_INTERNAL(bool) VM_call_fn(VM* vm, ObjClosure* closure, ObjFunction* fn, UInt argc)
{
    if(fn->arity != argc) {
        VM_error(vm, "Expected %u arguments, but got %u instead.", fn->arity, argc);
        return false;
    }

    if(vm->fc == VM_FRAMES_MAX) {
        VM_error(
            vm,
            "Internal error: vm stack overflow, recursion depth limit reached [%u].\n",
            VM_FRAMES_MAX);
        return false;
    }

    CallFrame* frame = &vm->frames[vm->fc++];
    frame->closure   = closure;
    frame->fn        = fn;
    frame->ip        = fn->chunk.code.data;
    frame->sp        = vm->sp - argc - 1;

    return true;
}

SK_INTERNAL(force_inline bool) VM_call_native(VM* vm, ObjNative* native, UInt argc)
{
    if(unlikely(native->arity != argc)) {
        VM_error(vm, "Expected %u arguments, but got %u instead.", native->arity, argc);
        return false;
    }

    if(native->fn(vm, argc, vm->sp - argc)) {
        vm->sp -= (argc + 1);
        return true;
    } else {
        VM_error(vm, AS_CSTRING(vm->sp[-argc - 1]));
        return false;
    }
}

SK_INTERNAL(force_inline bool) VM_call_val(VM* vm, Value fnval, UInt argc)
{
    if(IS_OBJ(fnval)) {
        switch(OBJ_TYPE(fnval)) {
            case OBJ_FUNCTION:
                return VM_call_fn(vm, NULL, AS_FUNCTION(fnval), argc);
            case OBJ_CLOSURE: {
                ObjClosure* closure = AS_CLOSURE(fnval);
                return VM_call_fn(vm, closure, closure->fn, argc);
            }
            case OBJ_NATIVE: {
                return VM_call_native(vm, AS_NATIVE(fnval), argc);
            }
            default:
                break;
        }
    }

    VM_error(
        vm,
        "Tried calling non-callable object, only functions and classes can be called.");
    return false;
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

    ObjUpvalue* upval = ObjUpvalue_new(vm->global_vals.roots, var_ref);
    upval->next       = *pp;
    *pp               = upval;

    return upval;
}

SK_INTERNAL(force_inline void) VM_close_upval(VM* vm, Value* last)
{
    while(vm->open_upvals != NULL && vm->open_upvals->location >= last) {
        // This is where closing happens, stack values
        // get new 'location' on the heap (this 'Obj').
        ObjUpvalue* upval = vm->open_upvals;
        upval->closed     = *upval->location;
        upval->location   = &upval->closed;
        vm->open_upvals   = upval->next;
    }
}

// @TODO: Encode type of instruction inside of OpCode but avoid branching.
// This would result in having no 'L' (long) OpCodes but would require runtime check
// that needs to avoid branching and figure out the instruction length arithmetically.
SK_INTERNAL(InterpretResult) VM_run(VM* vm)
{
    runtime = 1;
#define READ_BYTE()      (*ip++)
#define READ_BYTEL()     (ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT()  frame->fn->chunk.constants.data[READ_BYTE()]
#define READ_CONSTANTL() frame->fn->chunk.constants.data[READ_BYTEL()]
#define READ_STRING()    AS_STRING(READ_CONSTANT())
#define READ_STRINGL()   AS_STRING(READ_CONSTANTL())
#define DISPATCH(x)      switch(x)
#define CASE(label)      case label:
#define BREAK            break
#define BINARY_OP(value_type, op)                                                        \
    do {                                                                                 \
        if(!IS_NUMBER(stack_peek(0)) || !IS_NUMBER(stack_peek(1))) {                     \
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

    // First frame is 'global' frame aka implicit
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
    #include "jmptable.h"
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
                    VM_error(vm, "Operand must be a number (unary negation '-').");
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
                printf("\n");
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
                Byte idx   = READ_BYTE();
                Byte flags = Array_Global_index(&vm->global_vals, idx)->flags;
                *Array_Global_index(&vm->global_vals, idx) =
                    (Global){stack_peek(0), flags};
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_DEFINE_GLOBALL)
            {
                UInt idx   = READ_BYTEL();
                Byte flags = Array_Global_index(&vm->global_vals, idx)->flags;
                *Array_Global_index(&vm->global_vals, idx) =
                    (Global){stack_peek(0), flags};
                VM_pop(vm);
                BREAK;
            }
            CASE(OP_GET_GLOBAL)
            {
                Global* global = Array_Global_index(&vm->global_vals, READ_BYTE());

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                VM_push(vm, global->value);
                BREAK;
            }
            CASE(OP_GET_GLOBALL)
            {
                Global* global = Array_Global_index(&vm->global_vals, READ_BYTEL());

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                VM_push(vm, global->value);
                BREAK;
            }
            CASE(OP_SET_GLOBAL)
            {
                Global* global = Array_Global_index(&vm->global_vals, READ_BYTE());

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(GLOB_CHECK(global, GLOB_FIXED_BIT))) {
                    frame->ip = ip;
                    VM_error(vm, "Can't assign to 'fixed' variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                global->value = stack_peek(0);
                BREAK;
            }
            CASE(OP_SET_GLOBALL)
            {
                Global* global = Array_Global_index(&vm->global_vals, READ_BYTEL());

                if(unlikely(IS_UNDEFINED(global->value))) {
                    frame->ip = ip;
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                } else if(unlikely(GLOB_CHECK(global, GLOB_FIXED_BIT))) {
                    frame->ip = ip;
                    VM_error(vm, "Can't assign to 'fixed' variable.");
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
                UInt argc = READ_BYTE();
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
                UInt argc = READ_BYTEL();
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
                ObjClosure*  closure = ObjClosure_new(vm->global_vals.roots, fn);
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
            CASE(OP_RET)
            {
                Value retval = VM_pop(vm);
                VM_close_upval(vm, frame->sp);
                vm->fc--;
                if(vm->fc == 0) {
                    // @FIX: REPL not working, maybe sp - stack is -1
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
    HashTable_free(&vm->global_ids);
    Array_Global_free(&vm->global_vals);
    HashTable_free(&vm->strings);
    Array_ObjRef_free(&vm->gray_stack);

    Obj* next;
    for(Obj* head = vm->objects; head != NULL; head = next) {
        next = head->next;
        Obj_free(&(Roots){NULL, vm}, head);
    }

    FREE(vm);
}
