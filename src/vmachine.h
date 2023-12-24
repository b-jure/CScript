#ifndef SKOOMA_VMACHINE_H
#define SKOOMA_VMACHINE_H

#include "array.h"

#include "chunk.h"
#include "hashtable.h"
#include "skconf.h"
#include "skooma.h"
#include "value.h"

// Max depth of CallFrames
#define VM_FRAMES_MAX S_CALLFRAMES_MAX

// Max stack size
#define VM_STACK_MAX ((int)(S_STACK_MAX / sizeof(Value)))





typedef struct CallFrame CallFrame;

struct CallFrame {
    OClosure* closure;
    Byte*     ip; /* Instruction pointer (closure chunk) */
    Value*    callee; /* Pointer to the callee on the stack */
    Int       retcnt; /* Expected value return count */
    Int       vacnt; /* Count of extra arguments in vararg functions */
};

#define FFN(frame) (frame->closure->fn)





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



typedef enum {
    INTERPRET_OK, /* No error */
    INTERPRET_COMPILE_ERROR, /* Compile time error */
    INTERPRET_RUNTIME_ERROR, /* VM runtime error */
} InterpretResult;


#define tonumber(val, np)   (IS_NUMBER(val) ? (*(np) = AS_NUMBER(val), 1) : 0)
#define tostring(val, strp) (IS_STRING(val) ? (*(strp) = AS_CSTRING(val), 1) : 0)
#define tobool(val, bval)   (IS_BOOL(val) ? (*(bval) = AS_BOOL(val), 1) : 0)


void push(VM* vm, Value val);
#define pushn(vm, n, val)                                                                \
    do {                                                                                 \
        int cnt = n;                                                                     \
        while(cnt-- > 0)                                                                 \
            push(vm, val);                                                               \
    } while(0)

#define pop(vm)     (*--(vm)->sp)
#define popn(vm, n) (vm->sp -= n)

InterpretResult interpret(VM* vm, const char* source, const char* filename);
void            runerror(VM* vm, const char* errfmt, ...);
bool            callv(VM* vm, Value callee, Int argc, Int retcnt);

ARRAY_NEW(Array_ORef, O*);
ARRAY_NEW(Array_VRef, Value*);

struct VM {
    Config        config; // user configuration
    unsigned long seed; // randomized seed for hashing
    HashTable     loaded; // loaded scripts
    Value         script; // current script name
    Function*     F; // function state
    CallFrame     frames[VM_FRAMES_MAX];
    Int           fc; // frame count
    Value         stack[VM_STACK_MAX];
    Value*        sp; // stack pointer
    Array_VRef    callstart;
    Array_VRef    retstart;
    HashTable     globids; // global variable names
    Variable*     globvals; // global variable values
    UInt          globlen; // global variable count
    UInt          globcap; // global variable array size
    Array_Value   temp; // temporary return values
    HashTable     strings; // interned strings (weak refs)
    OUpvalue*     open_upvals; // closure values
    OString*      statics[SS_SIZE]; // static strings
    O*            objects; // list of all allocated objects
    O**           gray_stack; // tricolor gc (stores marked objects)
    UInt          gslen; // gray stack length
    UInt          gscap; // gray stack capacity
    size_t        gc_allocated; // count of allocated bytes in use
    size_t        gc_next; // next threshold where gc triggers
    Byte          gc_flags; // gc flags (sk API)
};

#define stackpeek(top) ((vm)->sp - ((top) + 1))

#endif
