#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "array.h"
#include "chunk.h"
#include "hashtable.h"
#include "skconf.h"
#include "value.h"

// Max depth of CallFrames
#define VM_FRAMES_MAX SK_CALLFRAMES_MAX

// Max stack size
#define VM_STACK_MAX (SK_STACK_MAX / sizeof(Value))

typedef struct {
  ObjFunction *fn; /* Function of this CallFrame */
  Byte *ip;
  Value *sp; /* Relative stack pointer */
} CallFrame;

typedef struct {
  Value value;
  bool fixed;
} Global;

DECLARE_ARRAY(Global);

typedef struct {
  CallFrame frames[VM_FRAMES_MAX]; /* Call frames */
  Int fc;                          /* Frame count */
  Value stack[VM_STACK_MAX];       /* Stack */
  Value *sp;                       /* Stack pointer */
  HashTable global_ids;            /* Global variable names */
  GlobalArray global_vals;         /* Global variable values */
  HashTable strings;               /* Strings (interning) */
  Obj *objects;                    /* List of allocated object */
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
