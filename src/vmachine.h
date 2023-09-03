#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "chunk.h"
#include "hashtable.h"
#include "value.h"

/* Mebibytes */
#define MIB(x) (x << 20)
#define STACK_MAX(type) (MIB(1) / sizeof(type))

/* VM Stack size */
#define VM_STACK_MAX STACK_MAX(Value)

/* @TODO: Make stack size modifiable inside interpreter */

typedef struct {
  Chunk *chunk;              /* Chunk being interpreted */
  Byte *ip;                  /* Instruction pointer */
  Value stack[VM_STACK_MAX]; /* Stack */
  Value *sp;                 /* Stack pointer */
  HashTable global_ids;      /* Global variable names */
  ValueArray global_vals;    /* Global variable values */
  HashTable strings;         /* Strings (interning) */
  Obj *objects;              /* List of allocated object */
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
