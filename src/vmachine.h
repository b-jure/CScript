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

#define GC_SET(vm, bit)        BIT_SET((vm)->gc_flags, bit)
#define GC_CLEAR(vm, bit)      BIT_CLEAR((vm)->gc_flags, bit)
#define GC_TOGGLE(vm, bit, on) BIT_TOGGLE((vm)->gc_flags, bit, on)
#define GC_CHECK(vm, bit)      BIT_CHECK((vm)->gc_flags, bit)

typedef struct {
    const char*   name;
    const uint8_t len;
} InternedString;

#define sizeofstr(str) (sizeof(str) - 1)

/* Function names */
#define SS_INIT 0
#define SS_ADD  1
#define SS_SUB  2
#define SS_MUL  3
#define SS_DIV  4
#define SS_REM  5
#define SS_NEG  6
#define SS_NOT  7
#define OPSN    (SS_NOT + 1) /* Number of overloadable methods */
/* Value types */
#define SS_STR  8
#define SS_NUM  9
#define SS_INS  10
#define SS_BOOL 11
#define SS_NIL  12
/* Native function params */
#define SS_MANU 13
#define SS_AUTO 14
/* Total size */
#define SS_SIZE (sizeof(static_str) / sizeof(static_str[0]))

static const InternedString static_str[] = {
  /* Reserved class function names for overloading */
    {"__init__", sizeofstr("__init__")},
    {"__add__",  sizeofstr("__add__") }, // Overloading not implemented
    {"__sub__",  sizeofstr("__sub__") }, // Overloading not implemented
    {"__mul__",  sizeofstr("__mul__") }, // Overloading not implemented
    {"__div__",  sizeofstr("__div__") }, // Overloading not implemented
    {"__rem__",  sizeofstr("__rem__") }, // Overloading not implemented
    {"__neg__",  sizeofstr("__neg__") }, // Overloading not implemented
    {"__not__",  sizeofstr("__not__") }, // Overloading not implemented
  /* (user) Value types */
    {"string",   sizeofstr("string")  },
    {"number",   sizeofstr("number")  },
    {"instance", sizeofstr("instance")},
    {"bool",     sizeofstr("bool")    },
    {"nil",      sizeofstr("nil")     },
 /* Native function arguments */
    {"manual",   sizeofstr("manual")  },
    {"auto",     sizeofstr("auto")    },
};

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

    // static strings but allocated as ObjString's
    ObjString* statics[SS_SIZE];

    // Garbage collection
    Obj*         objects;      /* List of allocated object (GC) */
    Array_ObjRef gray_stack;   /* marked objects stack (NO GC) */
    size_t       gc_allocated; /* count of allocated bytes */
    size_t       gc_next;      /* next byte threshold on which GC triggers */
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
