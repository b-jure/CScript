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


/* Protected function signature */
typedef void (*ProtectedFn)(VM* vm, void* userdata);


typedef enum {
    CFI_FRESH = 1,
    CFI_CCALL = 2,
} CFInfo;

struct CallFrame {
    OClosure* closure;
    uint8_t* ip; /* Instruction pointer (closure chunk) */
    Value* callee; /* Pointer to the callee on the stack */
    int32_t retcnt; /* Expected value return count */
    int32_t vacnt; /* Count of extra arguments in vararg functions */
    // @TODO[debug] uint32_t firsttransfer; /* debug info */
    // @TODO[debug] uint32_t ntransferrs; /* debug info */
    uint8_t cfinfo; /* Additional context */
};

/* Fetch the frame closure (skooma function) */
#define FFN(frame) (frame->closure->fn)




/* Variable flags */
#define VAR_FIXED_BIT 1
/* Check if variable is 'fixed' */
#define ISFIXED(variable) btest((variable)->flags, VAR_FIXED_BIT)

/* Variable has a 'Value' and flags (modifiers),
 * only globals are 'Variable' type, locals do
 * not require this wrapper as they are resolved
 * during compilation. */
typedef struct {
    Value value; /* Global value */
    /* 1 - fixed
     * 2 - captured
     * 3 - unused
     * ...
     * 8 - unused */
    Byte flags;
} Variable;


typedef struct {
    AllocFn reallocate; // allocator
    void* userdata; // for allocator
    ReadFn reader; // for reading skooma scripts
    PanicFn panic; // panic handler
} Hooks; // Configurable hooks


typedef struct {
    size_t gc_heapmin; // minimum GC threshold
    size_t gc_nextgc; // next byte threshold when GC triggers
    size_t gc_allocated; // number of allocated bytes
    double gc_growfactor; // GC grow factor
    uint8_t gc_stopped : 1; // this flag is set if GC was stopped
} GC; // Configurable incremental garbage collection parameters


/* Generic Arrays for VM */
ARRAY_NEW(Array_ORef, O*);
ARRAY_NEW(Array_VRef, Value*);
ARRAY_NEW(Array_Variable, Variable);
ARRAY_NEW(Array_OSRef, OString*);


/* Skooma Virtual Machine */
struct VM {
    Hooks hooks;
    GC gc;
    unsigned long seed; // randomized seed for hashing
    struct sk_longjmp* errjmp; // error longjmp
    Status status; // status code
    HashTable loaded; // loaded scripts
    Function* F; // function state
    CallFrame frames[VM_CALLSTACK_LIMIT]; // call stack
    CallFrame* firstframe; // ptr to the first frame @remove ?
    Int fc; // call stack length
    Value stack[VM_STACK_LIMIT]; // values stack
    Value* sp; // stack pointer
    Array_VRef callstart; // start of call arguments
    Array_VRef retstart; // start of return values
    HashTable globids; // global variable names
    Array_Variable globvars; // global variable values
    Array_Value temp; // temporary return values
    OUpvalue* open_upvals; // closure values
    HashTable weakrefs; // interned strings (weak refs/not marked)
    Array_OSRef interned; // interned strings (marked)
    OString* faststatic[SS_SIZE]; // static strings with fast access
    OString* memerror; // preallocated object string for memory errors
    O* objects; // list of all allocated objects
    O** gray_stack; // tricolor gc (stores marked objects)
    size_t gslen; // gray stack length
    size_t gscap; // gray stack capacity
};

// Load script
#define loadscript(vm, sname, sclosure) HashTable_insert(vm, &(vm)->loaded, sname, sclosure)
// Check if script is loaded
#define isloaded(vm, sname, sclosure) (HashTable_get(&(vm)->loaded, sname, &sclosure))

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
#define pushn(vm, n, val)                                                                          \
    do {                                                                                           \
        int cnt = n;                                                                               \
        while(cnt-- > 0)                                                                           \
            push(vm, val);                                                                         \
    } while(0)


/* Check if value is correct type and set the pointer.
 * Return 1 if pointer was set, 0 if not. */
#define tonumber(val, np)   (IS_NUMBER(val) ? (*(np) = AS_NUMBER(val), 1) : 0)
#define tostring(val, strp) (IS_STRING(val) ? (*(strp) = AS_CSTRING(val), 1) : 0)
#define tobool(val, bp)     (IS_BOOL(val) ? (*(bp) = AS_BOOL(val), 1) : 0)



void VM_init(VM* vm);

void interpret(VM* vm, const char* source, const char* filename);

void run(VM* vm);

int ncall(VM* vm, Value* retstart, Value callee, Int retcnt);

int pcall(VM* vm, ProtectedFn fn, void* userdata, ptrdiff_t oldtop);

void closeupval(VM* vm, Value* last);

uint8_t bindmethod(VM* vm, OClass* oclass, Value name, Value receiver);

void resetvm(VM* vm, Status status);

#endif
