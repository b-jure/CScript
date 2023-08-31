#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "chunk.h"
#include "hashtable.h"
#include "value.h"

/* Mebibytes */
#define MIB(x) (x << 20)
/* Stack size */
#define STACK_MAX (MIB(1) / sizeof(Value))

/* @TODO: Make stack size modifiable inside interpreter */

typedef struct {
  Chunk *chunk;           /* Chunk being interpreted */
  Byte *ip;               /* Instruction pointer */
  Value stack[STACK_MAX]; /* Stack */
  Value *sp;              /* Stack pointer */
  HashTable globals;      /* Global variables */
  HashTable strings;      /* Strings (interning) */
  Obj *objects;           /* List of allocated object */
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
