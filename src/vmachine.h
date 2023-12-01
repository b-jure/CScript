#ifndef __SKOOMA_VMACHINE_H__
#define __SKOOMA_VMACHINE_H__

#include "array.h"

#include "chunk.h"
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
#define FRAME_CLOSURE(frame) ((OClosure*)((frame)->fn))
// Get function from obj
#define GET_FN(obj)                                                             \
    (otype(obj) == OBJ_CLOSURE ? ((OClosure*)(obj))->fn : ((OFunction*)(obj)))

typedef struct {
    O*     fn;
    Byte*  ip; /* Top of the CallFrame */
    Value* sp; /* Relative stack pointer */
    Int    retcnt; /* Expected value return count */
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



ARRAY_NEW(Array_ORef, O*);

struct VM {
    Config    config; // user configuration
    HashTable loaded; // loaded scripts
    Value     script; // current script name
    Function* F; // function state
    CallFrame frames[VM_FRAMES_MAX];
    Int       fc; // frame count
    Value     stack[VM_STACK_MAX];
    Value*    sp; // stack pointer
    Value*    callstart; // start of call args
    Value*    retstart; // start of return values
    HashTable globids; // global variable names
    Variable* globvals; // global variable values
    UInt      globlen; // global variable count
    UInt      globcap; // global variable array size
    HashTable strings; // interned strings (weak refs)
    OUpvalue* open_upvals; // closure values
    OString*  statics[SS_SIZE]; // static strings
    O*        objects; // list of all allocated objects
    O**       gray_stack; // tricolor gc (stores marked objects)
    UInt      gslen; // gray stack length
    UInt      gscap; // gray stack capacity
    size_t    gc_allocated; // count of allocated bytes in use
    size_t    gc_next; // next threshold where gc triggers
    Byte      gc_flags; // gc flags (sk API)
};






typedef enum {
    INTERPRET_OK, /* No error */
    INTERPRET_COMPILE_ERROR, /* Compile time error */
    INTERPRET_RUNTIME_ERROR, /* VM runtime error */
} InterpretResult;






VM*  VM_new(Config* config);
void VM_free(VM* vm);
void push(VM* vm, Value val);
#define pop(vm) *--vm->sp
InterpretResult interpret(VM* vm, const char* source, const char* filename);
bool            fncall(VM* vm, O* callee, Int argc, Int retcnt);
void            _cleanupvm(VM* vm);

#endif
