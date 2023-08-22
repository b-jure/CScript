#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "mem.h"
#include "vmachine.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/* Check if we can use labels as values for out computed goto/jmp table */
#if defined(__GNUC__) && __GNUC__ >= 2
    #define THREADED_CODE
#endif

static InterpretResult VM_run(VM* vm);
static Value           VM_pop(VM* vm);
static void            VM_push(VM* vm, Value val);
static void            VM_op_const(VM* vm);
static void            VM_op_constl(VM* vm);
static void            VM_op_neg(VM* vm);
static void            VM_op_add(VM* vm);
static void            VM_op_sub(VM* vm);
static void            VM_op_div(VM* vm);
static void            VM_op_mul(VM* vm);

static void VM_op_const(VM* vm)
{
    VM_push(vm, vm->chunk->constants.data[*vm->ip++]);
}

static void VM_op_constl(VM* vm)
{
    VM_push(vm, vm->chunk->constants.data[GET_BYTES3(vm->ip)]);
    vm->ip += 3;
}

static void VM_op_neg(VM* vm)
{
    *(vm->sp - 1) = -*(vm->sp - 1);
}

#define VM_BINARY_OP(op)                                                                 \
    do {                                                                                 \
        Value b = VM_pop(vm);                                                            \
        Value a = VM_pop(vm);                                                            \
        VM_push(vm, a op b);                                                             \
    } while(false)

static void VM_op_add(VM* vm)
{
    VM_BINARY_OP(+);
}

static void VM_op_sub(VM* vm)
{
    VM_BINARY_OP(-);
}

static void VM_op_mul(VM* vm)
{
    VM_BINARY_OP(*);
}

static void VM_op_div(VM* vm)
{
    VM_BINARY_OP(/);
}

#undef VM_BINARY_OP

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
    vm->sp    = vm->stack;
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
        // NOTE: This is a GCC extension that might get removed in the future,
        //       https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
        static const void* jmp_table[] = {
            &&op_const,
            &&op_constl,
            &&op_neg,
            &&op_add,
            &&op_sub,
            &&op_mul,
            &&op_div,
            &&op_ret,
        };

        goto* jmp_table[*vm->ip++];

    op_const:
        VM_op_const(vm);
        continue;
    op_constl:
        VM_op_constl(vm);
        continue;
    op_neg:
        VM_op_neg(vm);
        continue;
    op_add:
        VM_op_add(vm);
        continue;
    op_sub:
        VM_op_sub(vm);
        continue;
    op_mul:
        VM_op_mul(vm);
        continue;
    op_div:
        VM_op_div(vm);
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
                VM_op_const(vm);
                break;
            case OP_CONSTL:
                VM_op_constl(vm);
                break;
            case OP_NEG:
                VM_op_neg(vm);
                break;
            case OP_ADD:
                VM_op_add(vm);
                break;
            case OP_SUB:
                VM_op_sub(vm);
                break;
            case OP_MUL:
                VM_op_mul(vm);
                break;
            case OP_DIV:
                VM_op_div(vm);
                break;
            case OP_RET:
                Value_print(VM_pop(vm));
                printf("\n");
                return INTERPRET_OK;
        }
#endif
}

void VM_free(VM* vm)
{
    if(_likely(vm->chunk != NULL)) {
        Chunk_free(vm->chunk);
    }
    MFREE(vm, sizeof(VM));
}

/*
 *
 */
/*======================================= STACK =======================================*/

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
