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
#define VM_STACK_MAX ((uint32_t)(SK_STACK_MAX / sizeof(Value)))

typedef struct {
  ObjFunction *fn; /* Function of this CallFrame */
  Byte *ip;        /* Top of the CallFrame */
  Value *sp;       /* Relative stack pointer */
} CallFrame;

typedef struct {
  Value value; /* Global value */
  bool fixed;  /* @TODO: Make this into a byte that holds flag bits */
} Global;

DECLARE_ARRAY(Global)

typedef struct {
  CallFrame frames[VM_FRAMES_MAX]; /* Call frames */
  Int fc;                          /* Frame count */
  Value stack[VM_STACK_MAX];       /* Stack */
  Value *sp;                       /* Stack pointer */
  HashTable global_ids;            /* Global variable names */
  GlobalArray global_vals;         /* Global variable values */
  HashTable strings;               /* Strings (interning) */
  Obj *objects;                    /* List of allocated object (GC) */
} VM;

typedef enum {
  INTERPRET_OK,            /* No error */
  INTERPRET_COMPILE_ERROR, /* Compile time error */
  INTERPRET_RUNTIME_ERROR, /* VM runtime error */
} InterpretResult;

void VM_init(VM *vm);
InterpretResult VM_interpret(VM *vm, const char *source_code);
void VM_free(VM *vm);

#endif
