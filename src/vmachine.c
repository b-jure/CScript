#include "array.h"
#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "object.h"
#include "vmachine.h"
#include <assert.h>

#ifdef DEBUG_TRACE_EXECUTION
    #include "debug.h"
#endif

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

DEFINE_ARRAY(Global);

#define stack_peek(top) ((Value) * ((vm)->sp - (top + 1)))
#define stack_reset(vm) (vm)->sp = (vm)->stack
#define stack_size(vm)  ((vm)->sp - (vm)->stack)

SK_INTERNAL(force_inline void) VM_push(VM* vm, Value val)
{
    if(likely(vm->sp - vm->stack < VM_STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(stderr, "Skooma: stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

SK_INTERNAL(force_inline Value) VM_pop(VM* vm)
{
    return *--vm->sp;
}

void VM_error(VM* vm, const char* errfmt, ...)
{
    va_list ap;
    va_start(ap, errfmt);
    vfprintf(stderr, errfmt, ap);
    va_end(ap);
    fputs("\n", stderr);

    UInt line = Chunk_getline(vm->chunk, vm->ip - vm->chunk->code.data - 1);
    fprintf(stderr, "[line: %u] in script\n", line);
    stack_reset(vm);
}

/* Returns true if 'value' is boolean 'false' or is 'nil', otherwise return false.
 * DEV_NOTE: bool type must be 1 if true and 0 if false,
 * otherwise this would break some logic, for example
 * the case where the VM is executing OP_JMP_IF_FALSE instruction. */
SK_INTERNAL(force_inline bool) isfalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

SK_INTERNAL(force_inline uint8_t) bool_to_str(char** str, bool boolean)
{
    if(boolean) {
        *str = "true";
        return sizeof("true") - 1;
    }
    *str = "false";
    return sizeof("false") - 1;
}

SK_INTERNAL(force_inline uint8_t) nil_to_str(char** str)
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
SK_INTERNAL(force_inline uint8_t) double_to_str(char** str, double dbl)
{
    static char buffer[30] = {0};
    uint8_t     len;

    if(floor(dbl) != dbl) {
        len = snprintf(buffer, sizeof(buffer), "%f", dbl);
    } else {
        /* Quick return */
        len = snprintf(buffer, sizeof(buffer), "%ld", (int64_t)dbl);
        goto end;
    }

    int c;
    /* Trim excess zeroes */
    for(uint8_t i = len - 1; i > 0; i--) {
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
        // NOTE: In case ValueType enum gets modified update this table
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

    // @GC - allocated
    return ObjString_from(vm, buffer, length);
}

/*
 *
 */
/*======================= VM core functions ==========================*/

VM* VM_new(void)
{
    VM* vm = NULL;
    vm     = MALLOC(sizeof(VM));
    VM_init(vm);
    return vm;
}

void VM_init(VM* vm)
{
    vm->chunk   = NULL;
    vm->ip      = NULL;
    vm->objects = NULL;
    stack_reset(vm);
    HashTable_init(&vm->global_ids);
    GlobalArray_init(&vm->global_vals);
    HashTable_init(&vm->strings);
}

static InterpretResult VM_run(VM* vm)
{
#define BINARY_OP(value_type, op)                                                        \
    do {                                                                                 \
        if(!IS_NUMBER(stack_peek(0)) || !IS_NUMBER(stack_peek(1))) {                     \
            VM_error(vm, "Operands must be numbers (binary operation '" #op "').");      \
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
            Value b = VM_pop(vm);                                                        \
            Value a = VM_pop(vm);                                                        \
            VM_push(vm, OBJ_VAL(concatenate(vm, a, b)));                                 \
        }                                                                                \
    } while(false)

#define READ_BYTE()      (*vm->ip++)
#define READ_BYTEL()     (vm->ip += 3, GET_BYTES3(vm->ip - 3))
#define READ_CONSTANT()  vm->chunk->constants.data[READ_BYTE()]
#define READ_CONSTANTL() vm->chunk->constants.data[READ_BYTEL()]
#define READ_STRING()    AS_STRING(READ_CONSTANT())
#define READ_STRINGL()   AS_STRING(READ_CONSTANTL())
#define DISPATCH(x)      switch(x)
#define CASE(label)      case label:
#define BREAK            break

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
        Instruction_debug(vm->chunk, (UInt)(vm->ip - vm->chunk->code.data), vm);
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
                if(!IS_NUMBER(stack_peek(0))) {
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
                Value a = VM_pop(vm);
                Value b = VM_pop(vm);
                VM_push(vm, BOOL_VAL(Value_eq(b, a)));
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
                UInt n = READ_BYTEL();
                while(n--) {
                    VM_pop(vm);
                }
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
                uint8_t idx               = READ_BYTE();
                bool    fixed             = vm->global_vals.data[idx].fixed;
                vm->global_vals.data[idx] = (Global){VM_pop(vm), fixed};
                BREAK;
            }
            CASE(OP_DEFINE_GLOBALL)
            {
                UInt idx                  = READ_BYTEL();
                bool fixed                = vm->global_vals.data[idx].fixed;
                vm->global_vals.data[idx] = (Global){VM_pop(vm), fixed};
                BREAK;
            }
            CASE(OP_GET_GLOBAL)
            {
                Global global = vm->global_vals.data[READ_BYTE()];
                if(IS_UNDEFINED(global.value)) {
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                VM_push(vm, global.value);
                BREAK;
            }
            CASE(OP_GET_GLOBALL)
            {
                Global global = vm->global_vals.data[READ_BYTEL()];
                if(IS_UNDEFINED(global.value)) {
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                VM_push(vm, global.value);
                BREAK;
            }
            CASE(OP_SET_GLOBAL)
            {
                uint8_t idx    = READ_BYTE();
                Global* global = &vm->global_vals.data[idx];
                if(IS_UNDEFINED(global->value)) {
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                } else if(global->fixed) {
                    VM_error(vm, "Can't assign to 'fixed' variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                global->value = stack_peek(0);
                BREAK;
            }
            CASE(OP_SET_GLOBALL)
            {
                UInt    idx    = READ_BYTEL();
                Global* global = &vm->global_vals.data[idx];
                if(IS_UNDEFINED(global->value)) {
                    VM_error(vm, "Undefined variable.");
                    return INTERPRET_RUNTIME_ERROR;
                } else if(global->fixed) {
                    VM_error(vm, "Can't assign to 'fixed' variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                global->value = stack_peek(0);
                BREAK;
            }
            CASE(OP_GET_LOCAL)
            {
                uint8_t slot = READ_BYTE();
                VM_push(vm, vm->stack[slot]);
                BREAK;
            }
            CASE(OP_GET_LOCALL)
            {
                UInt slot = READ_BYTEL();
                VM_push(vm, vm->stack[slot]);
                BREAK;
            }
            CASE(OP_SET_LOCAL)
            {
                uint8_t slot    = READ_BYTE();
                vm->stack[slot] = stack_peek(0);
                BREAK;
            }
            CASE(OP_SET_LOCALL)
            {
                UInt slot       = READ_BYTEL();
                vm->stack[slot] = stack_peek(0);
                BREAK;
            }
            CASE(OP_JMP_IF_FALSE)
            {
                UInt skip_offset = READ_BYTEL();
                //
                vm->ip += (uint8_t)isfalsey(stack_peek(0)) * skip_offset;
                BREAK;
            }
            CASE(OP_JMP)
            {
                UInt skip_offset = READ_BYTEL();
                //
                vm->ip += skip_offset;
                BREAK;
            }
            CASE(OP_RET)
            {
                return INTERPRET_OK;
            }
        }
    }

    unreachable;

#undef VM_BINARY_OP
#undef VM_CONCAT_OR_ADD
#undef READ_BYTE
#undef READ_BYTEL
#undef READ_CONSTANT
#undef READ_CONSTANTL
#undef READ_STRING
#undef READ_STRINGL
#undef DISPATCH
#undef CASE
#undef BREAK
}

InterpretResult VM_interpret(VM* vm, const char* source)
{
    Chunk chunk;
    Chunk_init(&chunk);

    if(!compile(vm, source, &chunk)) {
        Chunk_free(&chunk);
        stack_reset(vm);
        return INTERPRET_COMPILE_ERROR;
    }

    vm->chunk = &chunk;
    vm->ip    = vm->chunk->code.data;

    InterpretResult result = VM_run(vm);

    Chunk_free(&chunk);
    return result;
}

void VM_free(VM* vm)
{
    if(likely(vm->chunk != NULL)) { // @TODO: Remove this?
        Chunk_free(vm->chunk);
    }
    HashTable_free(&vm->global_ids);
    GlobalArray_free(&vm->global_vals);
    HashTable_free(&vm->strings);
    MFREE_LIST(vm->objects, Obj_free);
    MFREE(vm, sizeof(VM));
}
