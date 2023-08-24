#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "vmachine.h"

#ifdef DEBUG_TRACE_EXECUTION
    #include "debug.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// @TODO: Make some errors into warnings, for example emit warning when negating boolean
// values by converting them implicitly during compilation into numbers.

#define peek_stack(vm, top) ((Value) * ((vm)->sp - (top + 1)))
#define reset_stack(vm)     (vm)->sp = (vm)->stack

static void VM_push(VM* vm, Value val)
{
    if(_likely(vm->sp - vm->stack < STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(stderr, "Skooma: stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

static Value VM_pop(VM* vm)
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
    reset_stack(vm);
}

/* nil, boolean false - are falsey */
static bool isfalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

#define VM_BINARY_OP(value_type, op)                                                     \
    do {                                                                                 \
        if(!IS_NUMBER(peek_stack(vm, 0)) || !IS_NUMBER(peek_stack(vm, 1))) {             \
            VM_error(vm, "Operands must be numbers (binary operation '" #op "').");      \
            return INTERPRET_RUNTIME_ERROR;                                              \
        }                                                                                \
        double b = AS_NUMBER(VM_pop(vm));                                                \
        double a = AS_NUMBER(VM_pop(vm));                                                \
        VM_push(vm, value_type(a op b));                                                 \
    } while(false)

/*
 *
 */
/*======================= VM core functions ==========================*/

VM* VM_new(void)
{
    VM* vm = NULL;
    vm     = MALLOC(vm, sizeof(VM));
    VM_init(vm);
    return vm;
}

void VM_init(VM* vm)
{
    vm->chunk = NULL;
    vm->ip    = NULL;
    reset_stack(vm);
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
            &&op_const,
            &&op_constl,
            &&op_true,
            &&op_false,
            &&op_nil,
            &&op_neg,
            &&op_add,
            &&op_sub,
            &&op_mul,
            &&op_div,
            &&op_not,
            &&op_not_equal,
            &&op_equal,
            &&op_greater,
            &&op_greater_equal,
            &&op_less,
            &&op_less_equal,
            &&op_ret,
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
        if(!IS_NUMBER(peek_stack(vm, 0))) {
            VM_error(vm, "Operand must be a number (unary negation '-').");
            return INTERPRET_RUNTIME_ERROR;
        }
        AS_NUMBER_REF(vm->sp - 1) = -AS_NUMBER(peek_stack(vm, 0));
        continue;
    op_add:
        VM_BINARY_OP(NUMBER_VAL, +);
        continue;
    op_sub:
        VM_BINARY_OP(NUMBER_VAL, -);
        continue;
    op_mul:
        VM_BINARY_OP(NUMBER_VAL, *);
        continue;
    op_div:
        VM_BINARY_OP(NUMBER_VAL, /);
        continue;
    op_not:
        *(vm->sp - 1) = BOOL_VAL(isfalsey(peek_stack(vm, 0)));
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
        VM_BINARY_OP(BOOL_VAL, >);
        continue;
    op_greater_equal:
        VM_BINARY_OP(BOOL_VAL, >=);
        continue;
    op_less:
        VM_BINARY_OP(BOOL_VAL, <);
        continue;
    op_less_equal:
        VM_BINARY_OP(BOOL_VAL, <=);
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
                if(!IS_NUMBER(peek_stack(vm, 0))) {
                    VM_error(vm, "Operand must be a number (unary negation '-').");
                    return INTERPRET_RUNTIME_ERROR;
                }
                AS_NUMBER_REF(vm->sp - 1) = -AS_NUMBER(peek_stack(vm, 0));
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
                *(vm->sp - 1) = BOOL_VAL(isfalsey(peek_stack(vm, 0)));
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

    if(!compile(source, &chunk)) {
        Chunk_free(&chunk);
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
    if(_likely(vm->chunk != NULL)) {
        Chunk_free(vm->chunk);
    }
    MFREE(vm, sizeof(VM));
}
