#include "common.h"
#include "debug.h"
#include "mem.h"
#include "vmachine.h"

#include <stdio.h>
#include <stdlib.h>

#define FREE_VM(vm) MFREE((vm), sizeof(VM))

static InterpretResult        VM_run(VM* vm);
static __FORCE_INLINE__ Value VM_pop(VM* vm);
static __FORCE_INLINE__ void  VM_push(VM* vm, Value val);

/*
 *
 */
/*============================ JMP TABLE ============================*/

typedef void (*Handler)(VM* vm);

static __FORCE_INLINE__ void VM_op_const(VM* vm);
static __FORCE_INLINE__ void VM_op_constl(VM* vm);
static __FORCE_INLINE__ void VM_op_neg(VM* vm);
static __FORCE_INLINE__ void VM_op_add(VM* vm);
static __FORCE_INLINE__ void VM_op_sub(VM* vm);
static __FORCE_INLINE__ void VM_op_div(VM* vm);
static __FORCE_INLINE__ void VM_op_mul(VM* vm);

static Handler JmpTable[] = {
    VM_op_const,
    VM_op_constl,
    VM_op_neg,
    VM_op_add,
    VM_op_sub,
    VM_op_mul,
    VM_op_div,
};

static __FORCE_INLINE__ void VM_op_const(VM* vm)
{
    VM_push(vm, vm->chunk->constants.data[*vm->ip++]);
}

static __FORCE_INLINE__ void VM_op_constl(VM* vm)
{
    VM_push(vm, vm->chunk->constants.data[GET_BYTES3(vm->ip)]);
    vm->ip += 3;
}

static __FORCE_INLINE__ void VM_op_neg(VM* vm)
{
    *(vm->sp - 1) = -*(vm->sp - 1);
}

#define VM_BINARY_OP(op)                                                                 \
    do {                                                                                 \
        Value b = VM_pop(vm);                                                            \
        Value a = VM_pop(vm);                                                            \
        VM_push(vm, a op b);                                                             \
    } while (false)

static __FORCE_INLINE__ void VM_op_add(VM* vm)
{
    VM_BINARY_OP(+);
}

static __FORCE_INLINE__ void VM_op_sub(VM* vm)
{
    VM_BINARY_OP(-);
}

static __FORCE_INLINE__ void VM_op_mul(VM* vm)
{
    VM_BINARY_OP(*);
}

static __FORCE_INLINE__ void VM_op_div(VM* vm)
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

InterpretResult VM_interpret(VM* vm, Chunk* chunk)
{
    vm->chunk = chunk;
    vm->ip    = chunk->code.data;
    return VM_run(vm);
}

static InterpretResult VM_run(VM* vm)
{
    while (true) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("           ");
        for (Value* ptr = vm->stack; ptr < vm->sp; ptr++) {
            printf("[");
            Value_print(*ptr);
            printf("]");
        }
        printf("\n");
        Instruction_debug(vm->chunk, (UInt)(vm->ip - vm->chunk->code.data));
#endif

        Byte instruction = *vm->ip++;

        if (instruction != OP_RET) {
            JmpTable[instruction](vm);
        } else {
            Value_print(VM_pop(vm));
            printf("\n");
            return INTERPRET_OK;
        }
    }
}

void VM_free(VM* vm)
{
    if (LIKELY(vm->chunk != NULL)) {
        Chunk_free(vm->chunk);
    }
    FREE_VM(vm);
}

/*
 *
 */
/*======================================= STACK =======================================*/

static __FORCE_INLINE__ void VM_push(VM* vm, Value val)
{
    if (LIKELY(vm->sp - vm->stack < STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(stderr, "Skooma: stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

static __FORCE_INLINE__ Value VM_pop(VM* vm)
{
    return *--vm->sp;
}
