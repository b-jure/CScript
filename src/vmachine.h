#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "array.h"
#include "chunk.h"
#include "hashtable.h"
#include "value.h"

/* Mebibytes */
#define MIB(x) (x << 20)
#define STACK_MAX(type) (MIB(1) / sizeof(type))

/* VM Stack size */
#define VM_STACK_MAX STACK_MAX(Value)

/* @TODO: Make stack size modifiable (arguments to interpreter executable) */

typedef struct {
  Value value;
  bool fixed;
} Global;

DECLARE_ARRAY(Global);

typedef struct {
  Chunk *chunk;              /* Chunk being interpreted */
  Byte *ip;                  /* Instruction pointer */
  Value stack[VM_STACK_MAX]; /* Stack */
  Value *sp;                 /* Stack pointer */
  HashTable global_ids;      /* Global variable names */
  GlobalArray global_vals;   /* Global variable values */
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
