#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "chunk.h"
#include "value.h"

#define STACK_MAX (512000 / sizeof(Value))

typedef struct {
  Chunk *chunk;           /* Chunk being interpreted */
  Byte *ip;               /* Instruction pointer */
  Value stack[STACK_MAX]; /* Stack */
  Value *sp;              /* Stack pointer */
  Obj *objects;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;

VM *VM_new(void);
void VM_init(VM *vm);
InterpretResult VM_interpret(VM *vm, const char *source_code);
void VM_free(VM *vm);

#endif
