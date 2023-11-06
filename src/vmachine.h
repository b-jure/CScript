#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "array.h"

#include "chunk.h"
#include "core.h"
#include "hashtable.h"
#include "skconf.h"
#include "value.h"

// Max depth of CallFrames
#define VM_FRAMES_MAX S_CALLFRAMES_MAX

// Max stack size
#define VM_STACK_MAX (S_STACK_MAX / sizeof(Value))




// Get frame function
#define FRAME_FN(frame) GET_FN((frame)->fn)
// Get frame closure
#define FRAME_CLOSURE(frame) ((ObjClosure*)(frame->fn))
// Get function from obj
#define GET_FN(obj)                                                                                \
    (Obj_type(obj) == OBJ_CLOSURE ? ((ObjClosure*)(obj))->fn : ((ObjFunction*)(obj)))

typedef struct {
    Obj*   fn;
    Byte*  ip; /* Top of the CallFrame */
    Value* sp; /* Relative stack pointer */
} CallFrame;




#define VAR_FIXED_BIT     (1)
#define ISFIXED(variable) VAR_CHECK(variable, VAR_FIXED_BIT)

#define VAR_SET(variable, bit)   BIT_SET((variable)->flags, bit)
#define VAR_CLEAR(variable, bit) BIT_CLEAR((variable)->flags, bit)
#define VAR_CHECK(variable, bit) BIT_CHECK((variable)->flags, bit)
#define VAR_FLAGS(variable)      ((variable)->flags)

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
} Variable;




/* GC flags */
#define GC_MANUAL_BIT (1)

#define GC_SET(vm, bit)        BIT_SET((vm)->gc_flags, bit)
#define GC_CLEAR(vm, bit)      BIT_CLEAR((vm)->gc_flags, bit)
#define GC_TOGGLE(vm, bit, on) BIT_TOGGLE((vm)->gc_flags, bit, on)
#define GC_CHECK(vm, bit)      BIT_CHECK((vm)->gc_flags, bit)



ARRAY_NEW(Array_ObjRef, Obj*);

struct VM {
    // Temporary values (loaded functions)
    Value temp[S_TEMP_MAX];
    UInt  tempc;

    // VM configuration
    Config config;

    // Loaded scripts (filenames)
    HashTable loaded;

    // Currently compiled script (name)
    Value script;

    // Track innermost compiler for gc.
    Compiler* compiler;

    // Function CallFrame-s
    CallFrame frames[VM_FRAMES_MAX]; /* Call frames (GC) */
    Int       fc;                    /* Frame count */

    // Stack storage
    Value  stack[VM_STACK_MAX]; /* Stack (GC) */
    Value* sp;                  /* Stack pointer */

    // Global names and values storage
    HashTable globids;  /* Global names (GC) */
    Variable* globvals; /* Global values (GC) */
    UInt      globlen;  /* Global array length */
    UInt      globcap;  /* Global array capacity */

    // Weak references
    HashTable strings; /* Interned strings (GC) */

    // Closure values
    ObjUpvalue* open_upvals; // List of open upvalues (NO GC)

    // Static strings but allocated as ObjString's.
    // Do not store them in global table, some
    // of these are reserved for native functions in
    // order to do object comparison or avoid runtime
    // allocation.
    ObjString* statics[SS_SIZE];

    // Garbage collection
    Obj*   objects; /* List of allocated object (GC) */
    Obj**  gray_stack;
    UInt   gslen;
    UInt   gscap;
    size_t gc_allocated; /* count of allocated bytes */
    size_t gc_next;      /* next byte threshold on which GC triggers */
    Byte   gc_flags;     /* GC flags */
};




typedef enum {
    INTERPRET_OK,            /* No error */
    INTERPRET_COMPILE_ERROR, /* Compile time error */
    INTERPRET_RUNTIME_ERROR, /* VM runtime error */
} InterpretResult;




InterpretResult VM_interpret(VM* vm, const char* source, const char* filename);
bool            VM_call_fn(VM* vm, Obj* callee, Int argc, bool init, ObjClass* debug);
void            VM_push(VM* vm, Value val);
Value           VM_pop(VM* vm);
void            VM_push_temp(VM* vm, Value temp);
Value           VM_pop_temp(VM* vm);
void            VM_free(VM* vm);
void            _cleanup_vm(VM* vm);
VM*             VM_new(Config* config);

#endif
