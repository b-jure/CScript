#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "array.h"

typedef struct VM VM; // For chunk.h

#include "chunk.h"
#include "hashtable.h"
#include "skconf.h"
#include "value.h"

// Max depth of CallFrames
#define VM_FRAMES_MAX SK_CALLFRAMES_MAX

// Max stack size
#define VM_STACK_MAX ((uint32_t)(SK_STACK_MAX / sizeof(Value)))

typedef struct {
    ObjClosure*  closure;
    ObjFunction* fn;
    Byte*        ip; /* Top of the CallFrame */
    Value*       sp; /* Relative stack pointer */
} CallFrame;

#define GLOB_FIXED_BIT (1)
#define GLOB_MARKED_BIT (2)

#define GLOB_SET(glob, bit) BIT_SET((glob)->flags, bit)
#define GLOB_CLEAR(glob, bit) BIT_CLEAR((glob)->flags, bit)
#define GLOB_CHECK(glob, bit) BIT_CHECK((glob)->flags, bit)
#define GLOB_FLAGS(glob) ((glob)->flags)

typedef struct {
    Value value; /* Global value */
    /*
     * 1 - fixed
     * 2 - marked
     * 3 - unused
     * ...
     * 8 - unused
     */
    Byte  flags;
} Global;

ARRAY_NEW(Array_Global, Global);
ARRAY_NEW(Array_ObjRef, Obj*);

struct VM {
    CallFrame    frames[VM_FRAMES_MAX]; /* Call frames */
    Int          fc;                    /* Frame count */
    Value        stack[VM_STACK_MAX];   /* Stack */
    Value*       sp;                    /* Stack pointer */
    HashTable    global_ids;            /* Global variable names */
    Array_Global global_vals;           /* Global variable values */
    HashTable    strings;               /* Strings (interning) */
    ObjUpvalue*  open_upvals;           /* List of heap allocated Upvalues */
    Obj*         objects;               /* List of allocated object (GC) */
    Array_ObjRef gray_stack;
    size_t gc_allocated;
    size_t gc_next;
};

typedef enum {
    INTERPRET_OK,            /* No error */
    INTERPRET_COMPILE_ERROR, /* Compile time error */
    INTERPRET_RUNTIME_ERROR, /* VM runtime error */
} InterpretResult;

void            VM_init(VM* vm, void* roots);
InterpretResult VM_interpret(VM* vm, const char* source_code);
void            VM_set_roots(VM* vm, void* roots);
void            VM_push(VM* vm, Value val);
Value           VM_pop(VM* vm);
void            VM_free(VM* vm);

#endif
