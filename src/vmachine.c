#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "object.h"
#include "vmachine.h"

#ifdef DEBUG_TRACE_EXECUTION
    #include "debug.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// @TODO: Make some errors into warnings, for example emit warning when negating boolean
// values by converting them implicitly during compilation into numbers.

#define stack_peek(vm, top) ((Value) * ((vm)->sp - (top + 1)))
#define stack_reset(vm)     (vm)->sp = (vm)->stack
#define stack_size(vm)      ((vm)->sp - (vm)->stack)

/* This doesn't get inlined with gcc having -O3 optimizations enabled
 * this is the reason behind the force_inline attribute  */
static _force_inline void VM_push(VM* vm, Value val)
{
    if(_likely(vm->sp - vm->stack < STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(stderr, "Skooma: stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

static _force_inline Value VM_pop(VM* vm)
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

/* NIL and FALSE are always falsey */
static _force_inline bool isfalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static _force_inline uint8_t bool_to_str(char** str, bool boolean)
{
    if(boolean) {
        *str = "true";
        return sizeof("true") - 1;
    }
    *str = "false";
    return sizeof("false") - 1;
}

static _force_inline uint8_t nil_to_str(char** str)
{
    *str = "nil";
    return sizeof("nil") - 1;
}

/* Current number is stored in static memory to prevent the need
 * for allocating before creating the 'ObjString', keep in mind this
 * function is only safe for usage inside a concatenate function.
 * Additionally concatenate function guarantees there is only one possible number Value,
 * therefore there is no need for two static buffers inside of it. */
static _force_inline uint8_t double_to_str(char** str, double dbl)
{
    static char buffer[30] = {0};
    uint8_t     len;
    len = snprintf(buffer, 30, "%f", dbl);
    /* Trim excess zeroes and the floating point '.' if no fractional part */
    for(uint8_t i = len - 1; i > 0; i--) {
        if(buffer[i] == '0' || buffer[i] == '.') {
            len--;
        }
    }
    *str = buffer;
    return len;
}

static ObjString* concatenate(VM* vm, Value a, Value b)
{
    Value  value[2] = {a, b};
    size_t len[2] = {0};
    char*  str[2] = {0};

    for(int i = 0; i < 2; i++) {
#ifdef THREADED_CODE
        // NOTE: In case ValueType enum gets modified update this table
        static const void* jump_table[] = {
            // Keep this in the same order as in the ValueType enum
            &&vbool,   /* VAL_BOOL */
            &&vnumber, /* VAL_NUMBER */
            &&vnil,    /* VAL_NIL */
            &&vobj,    /* VAL_OBJ */
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
            default:
                _unreachable;
        }
#endif
    }
    return ObjString_from_concat(vm, str[0], len[0], str[1], len[1]);
}

#define VM_BINARY_OP(vm, value_type, op)                                                 \
    do {                                                                                 \
        if(!IS_NUMBER(stack_peek(vm, 0)) || !IS_NUMBER(stack_peek(vm, 1))) {             \
            VM_error(vm, "Operands must be numbers (binary operation '" #op "').");      \
            return INTERPRET_RUNTIME_ERROR;                                              \
        }                                                                                \
        double b = AS_NUMBER(VM_pop(vm));                                                \
        double a = AS_NUMBER(VM_pop(vm));                                                \
        VM_push(vm, value_type(a op b));                                                 \
    } while(false)

#define VM_CONCAT_OR_ADD(vm)                                                             \
    do {                                                                                 \
        if(IS_NUMBER(stack_peek(vm, 0)) && IS_NUMBER(stack_peek(vm, 1))) {               \
            double b = AS_NUMBER(VM_pop(vm));                                            \
            double a = AS_NUMBER(VM_pop(vm));                                            \
            VM_push(vm, NUMBER_VAL((a + b)));                                            \
        } else {                                                                         \
            Value b = VM_pop(vm);                                                        \
            Value a = VM_pop(vm);                                                        \
            VM_push(vm, OBJ_VAL(concatenate(vm, a, b)));                                 \
        }                                                                                \
    } while(false)

/*
 *
 */
/*======================= VM core functions ==========================*/

VM* VM_new(void)
{
    VM* vm = NULL;
    vm = MALLOC(sizeof(VM));
    VM_init(vm);
    return vm;
}

void VM_init(VM* vm)
{
    vm->chunk = NULL;
    vm->ip = NULL;
    vm->objects = NULL;
    stack_reset(vm);
}

static InterpretResult VM_run(VM* vm)
{
    while(true) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("           ");
        for(Value* ptr = vm->stack; ptr < vm->sp; ptr++) {
            printf("[");
            Value_print(*ptr);
            printf("]");
        }
        printf("\n");
        Instruction_debug(vm->chunk, (UInt)(vm->ip - vm->chunk->code.data));
#endif
#ifdef THREADED_CODE
        // IMPORTANT: update accordingly if OpCode enum is changed!
        static const void* jmp_table[] = {
            // Make sure order is same as in OpCode enum
            &&op_const,         // OP_CONST
            &&op_constl,        // OP_CONSTL
            &&op_true,          // OP_TRUE
            &&op_false,         // OP_FALSE
            &&op_nil,           // OP_NIL
            &&op_neg,           // OP_NEG
            &&op_add,           // OP_ADD
            &&op_sub,           // OP_SUB
            &&op_mul,           // OP_MUL
            &&op_div,           // OP_DIV
            &&op_not,           // OP_NOT
            &&op_not_equal,     // OP_NOT_EQUAL
            &&op_equal,         // OP_EQUAL
            &&op_greater,       // OP_GREATER
            &&op_greater_equal, // OP_GREATER_EQUAL
            &&op_less,          // OP_LESS
            &&op_less_equal,    // OP_LESS_EQUAL
            &&op_ret,           // OP_RET
        };

        goto* jmp_table[*vm->ip++];

    op_const:
        VM_push(vm, vm->chunk->constants.data[*vm->ip++]);
        continue;
    op_constl:
        VM_push(vm, vm->chunk->constants.data[GET_BYTES3(vm->ip)]);
        vm->ip += 3;
        continue;
    op_true:
        VM_push(vm, BOOL_VAL(true));
        continue;
    op_false:
        VM_push(vm, BOOL_VAL(false));
        continue;
    op_nil:
        VM_push(vm, NIL_VAL);
        continue;
    op_neg:
        if(!IS_NUMBER(stack_peek(vm, 0))) {
            VM_error(vm, "Operand must be a number (unary negation '-').");
            return INTERPRET_RUNTIME_ERROR;
        }
        AS_NUMBER_REF(vm->sp - 1) = -AS_NUMBER(stack_peek(vm, 0));
        continue;
    op_add:
        VM_CONCAT_OR_ADD(vm);
        continue;
    op_sub:
        VM_BINARY_OP(vm, NUMBER_VAL, -);
        continue;
    op_mul:
        VM_BINARY_OP(vm, NUMBER_VAL, *);
        continue;
    op_div:
        VM_BINARY_OP(vm, NUMBER_VAL, /);
        continue;
    op_not:
        *(vm->sp - 1) = BOOL_VAL(isfalsey(stack_peek(vm, 0)));
        continue;
    op_not_equal : {
        Value a = VM_pop(vm);
        Value b = VM_pop(vm);
        VM_push(vm, BOOL_VAL(!Value_eq(b, a)));
        continue;
    }
    op_equal : {
        Value a = VM_pop(vm);
        Value b = VM_pop(vm);
        VM_push(vm, BOOL_VAL(Value_eq(b, a)));
        continue;
    }
    op_greater:
        VM_BINARY_OP(vm, BOOL_VAL, >);
        continue;
    op_greater_equal:
        VM_BINARY_OP(vm, BOOL_VAL, >=);
        continue;
    op_less:
        VM_BINARY_OP(vm, BOOL_VAL, <);
        continue;
    op_less_equal:
        VM_BINARY_OP(vm, BOOL_VAL, <=);
        continue;
    op_ret:
        Value_print(VM_pop(vm));
        printf("\n");
        return INTERPRET_OK;
    }

    _unreachable;
#else
        switch(*vm->ip++) {
            case OP_CONST:
                VM_push(vm, vm->chunk->constants.data[*vm->ip++]);
                break;
            case OP_CONSTL:
                VM_push(vm, vm->chunk->constants.data[GET_BYTES3(vm->ip)]);
                vm->ip += 3;
                break;
            case OP_TRUE:
                VM_push(vm, BOOL_VAL(true));
                break;
            case OP_FALSE:
                VM_push(vm, BOOL_VAL(false));
                break;
            case OP_NIL:
                VM_push(vm, NIL_VAL);
                break;
            case OP_NEG:
                if(!IS_NUMBER(stack_peek(vm, 0))) {
                    VM_error(vm, "Operand must be a number (unary negation '-').");
                    return INTERPRET_RUNTIME_ERROR;
                }
                AS_NUMBER_REF(vm->sp - 1) = -AS_NUMBER(stack_peek(vm, 0));
                break;
            case OP_ADD:
                VM_BINARY_OP(NUMBER_VAL, +);
                break;
            case OP_SUB:
                VM_BINARY_OP(NUMBER_VAL, -);
                break;
            case OP_MUL:
                VM_BINARY_OP(NUMBER_VAL, *);
                break;
            case OP_DIV:
                VM_BINARY_OP(NUMBER_VAL, /);
                break;
            case OP_NOT:
                *(vm->sp - 1) = BOOL_VAL(isfalsey(stack_peek(vm, 0)));
                break;
            case OP_NOT_EQUAL:
                Value a = VM_pop(vm);
                Value b = VM_pop(vm);
                VM_push(BOOL_VAL(!Value_eq(b, a)));
                break;
            case OP_EQUAL:
                Value a = VM_pop(vm);
                Value b = VM_pop(vm);
                VM_push(BOOL_VAL(Value_eq(b, a)));
                break;
            case OP_GREATER_EQUAL:
                VM_BINARY_OP(BOOL_VAL, >=);
                break;
            case OP_GREATER:
                VM_BINARY_OP(BOOL_VAL, >);
                break;
            case OP_LESS_EQUAL:
                VM_BINARY_OP(BOOL_VAL, <=);
                break;
            case OP_LESS:
                VM_BINARY_OP(BOOL_VAL, <);
                break;
            case OP_RET:
                Value_print(VM_pop(vm));
                printf("\n");
                return INTERPRET_OK;
        }
#endif
}

InterpretResult VM_interpret(VM* vm, const char* source)
{
    Chunk chunk;
    Chunk_init(&chunk);

    if(!compile(vm, source, &chunk)) {
        Chunk_free(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm->chunk = &chunk;
    vm->ip = vm->chunk->code.data;

    InterpretResult result = VM_run(vm);

    Chunk_free(&chunk);
    return result;
}

void VM_free(VM* vm)
{
    if(_likely(vm->chunk != NULL)) {
        Chunk_free(vm->chunk);
    }
    MFREE_LIST(vm->objects, Obj_free);
    MFREE(vm, sizeof(VM));
}
