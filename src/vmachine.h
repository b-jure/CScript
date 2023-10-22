#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "array.h"

typedef struct VM VM;

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

#define GLOB_SET(glob, bit)   BIT_SET((glob)->flags, bit)
#define GLOB_CLEAR(glob, bit) BIT_CLEAR((glob)->flags, bit)
#define GLOB_CHECK(glob, bit) BIT_CHECK((glob)->flags, bit)
#define GLOB_FLAGS(glob)      ((glob)->flags)

typedef struct {
    Value value; /* Global value */
    /*
     * 1 - fixed
     * 2 - marked
     * 3 - unused
     * ...
     * 8 - unused
     */
    Byte flags;
} Global;

ARRAY_NEW(Array_Global, Global);
ARRAY_NEW(Array_ObjRef, Obj*);

/* GC flags */
#define GC_MANUAL_BIT (1)

#define GC_SET(vm, bit)   BIT_SET((vm)->gc_flags, bit)
#define GC_CLEAR(vm, bit) BIT_CLEAR((vm)->gc_flags, bit)
#define GC_CHECK(vm, bit) BIT_CHECK((vm)->gc_flags, bit)

struct VM {
    // Function CallFrame-s
    CallFrame frames[VM_FRAMES_MAX]; /* Call frames (GC) */
    Int       fc;                    /* Frame count */

    // Stack storage
    Value  stack[VM_STACK_MAX]; /* Stack (GC) */
    Value* sp;                  /* Stack pointer */

    // Global names and values storage
    HashTable globids;  /* Global names (GC) */
    Global*   globvals; /* Global values (GC) */
    UInt      globlen;  /* Global array length */
    UInt      globcap;  /* Global array capacity */

    // Weak references
    HashTable strings; /* Interned strings (GC) */

    // Closure values
    ObjUpvalue* open_upvals; // List of open upvalues (NO GC)

    // Class init method name
    ObjString* initstr;

    // Garbage collection
    Obj*         objects;      /* List of allocated object (GC) */
    Array_ObjRef gray_stack;   /* marked objects stack (NO GC) */
    size_t       gc_allocated; /* count of allocated bytes */
    double       gc_next;      /* next byte threshold on which GC triggers */
    Byte         gc_flags;     /* GC flags */
};

typedef enum {
    INTERPRET_OK,            /* No error */
    INTERPRET_COMPILE_ERROR, /* Compile time error */
    INTERPRET_RUNTIME_ERROR, /* VM runtime error */
} InterpretResult;

void            VM_init(VM* vm);
InterpretResult VM_interpret(VM* vm, const char* source_code);
void            VM_push(VM* vm, Value val);
Value           VM_pop(VM* vm);
void            VM_free(VM* vm);

#endif
