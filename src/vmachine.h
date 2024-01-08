#ifndef SKOOMA_VMACHINE_H
#define SKOOMA_VMACHINE_H

#include "array.h"
#include "chunk.h"
#include "common.h"
#include "hashtable.h"
#include "value.h"

#include <setjmp.h>

/* Wrapper around 'jmp_buf' with some additional
 * information, such as status code of the function that
 * was called and a previous sk_longjmp in order to handle
 * nested protected calls.
 * Additionally 'status' gets updated from the callee in
 * case of runtime error, this is the reason why
 * status is marked as volatile to assure the automatic
 * variable is updated and not only put in a register.
 * The whole structure lives on a stack right before calling
 * the C function.
 * Check 'protectedcall' in 'vmachine.c' for a reference. */
struct sk_longjmp {
    struct sk_longjmp* prev;
    jmp_buf buf;
    volatile int status;
};


typedef enum {
    CFI_FRESH = 1,
} CFInfo;

typedef struct {
    OClosure* closure;
    Byte* ip; /* Instruction pointer (closure chunk) */
    Value* callee; /* Pointer to the callee on the stack */
    Int retcnt; /* Expected value return count */
    Int vacnt; /* Count of extra arguments in vararg functions */
    Int status; /* In case of call errors */
    Byte info; /* Additional context */
} CallFrame;

/* Fetch the frame closure (skooma function) */
#define FFN(frame) (frame->closure->fn)




/* Variable flags */
#define VAR_FIXED_BIT 1
/* Check if variable is 'fixed' */
#define ISFIXED(variable) btest((variable)->flags, VAR_FIXED_BIT)

/* Variable, it has a 'Value' and flags (modifiers),
 * only globals and upvalues are 'Variable' type,
 * locals do not require this wrapper as they are
 * resolved during compilation. */
typedef struct {
    Value value; /* Global value */
    /* 1 - fixed
     * 2 - captured
     * 3 - unused
     * ...
     * 8 - unused */
    Byte flags;
} Variable;


/* Garbage collector flags */
typedef enum {
    GC_MANUAL = 1,
} GCFlags;


/* Configuration file, configurable by user (C API) */
typedef struct {
    AllocFn reallocate; // allocator
    void* userdata; // userdata for allocator
    CFunction panic; // panic handler
    size_t gc_heapinit; // initial heap threshold
    size_t gc_heapmin; // minimum heap size
    double gc_growfactor; // garbage collector grow factor
} Config;


/* Generic Arrays for VM */
ARRAY_NEW(Array_ORef, O*);
ARRAY_NEW(Array_VRef, Value*);
ARRAY_NEW(Array_Variable, Variable);


/* Skooma Virtual Machine */
struct VM {
    Config config; // user configuration
    unsigned long seed; // randomized seed for hashing
    struct sk_longjmp* errjmp;
    HashTable loaded; // loaded scripts
    Value script; // current script name
    Function* F; // function state
    CallFrame frames[VM_CALLSTACK_LIMIT];
    Int fc; // frame count
    Value stack[VM_STACK_LIMIT];
    Value* sp; // stack pointer
    Array_VRef callstart;
    Array_VRef retstart;
    HashTable globids; // global variable names
    Array_Variable globvars; // global variable values
    Array_Value temp; // temporary return values
    HashTable strings; // interned strings (weak refs)
    OUpvalue* open_upvals; // closure values
    OString* statics[SS_SIZE]; // static strings
    O* objects; // list of all allocated objects
    O** gray_stack; // tricolor gc (stores marked objects)
    uint64_t gslen; // gray stack length
    uint64_t gscap; // gray stack capacity
    size_t gc_allocated; // count of allocated bytes in use
    size_t gc_next; // next threshold where gc triggers
    Byte gc_flags; // gc flags (sk API)
};

/* Fetch the last call frame (current) */
#define last_frame(vm) ((vm)->frames[(vm)->fc - 1])


/* STACK */
#define restore_stack(vm, n) cast(Value*, (cast_charp((vm)->stack) + (n)))
#define save_stack(vm, ptr)  (cast_charp(ptr) - cast_charp((vm)->stack))
#define stackpeek(top)       ((vm)->sp - ((top) + 1))


/* PUSH/POP */
#define pop(vm)     (*--(vm)->sp)
#define popn(vm, n) ((vm)->sp -= n)
void push(VM* vm, Value val);
#define pushn(vm, n, val)                                                                \
    do {                                                                                 \
        int cnt = n;                                                                     \
        while(cnt-- > 0)                                                                 \
            push(vm, val);                                                               \
    } while(0)


/* Check if value is correct type and set the pointer.
 * Return 1 if pointer was set, 0 if not. */
#define tonumber(val, np)   (IS_NUMBER(val) ? (*(np) = AS_NUMBER(val), 1) : 0)
#define tostring(val, strp) (IS_STRING(val) ? (*(strp) = AS_CSTRING(val), 1) : 0)
#define tobool(val, bp)     (IS_BOOL(val) ? (*(bp) = AS_BOOL(val), 1) : 0)


/* ================== VM interface ================== */

/* Compile and run the chunk generated from 'source'. */
void interpret(VM* vm, const char* source, const char* filename);

/* Run interpreter. */
void run(VM* vm);

/* Normal call. */
int ncall(VM* vm, Value* retstart, Value callee, Int retcnt);

/* Protected call. */
int pcall(VM* vm, ProtectedFn fn, void* userdata, ptrdiff_t oldtop);

/* Close any open upvalues up to 'last' (stack position). */
void closeupval(VM* vm, Value* last);

/* Initialize config as per default settings. */
void Config_init(Config* config);


#endif
