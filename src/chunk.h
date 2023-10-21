#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "hashtable.h"
#include "value.h"

#ifndef __SKOOMA_VMACHINE_H__
typedef struct VM VM;
#endif

#ifndef __SKOOMA_COMPILER_H__
typedef struct Compiler Compiler;
#endif

#define OPCODE_N ((uint32_t)(OP_RET + 1))

typedef enum {
    OP_TRUE = 0,  /* Push true literal on the stack */
    OP_FALSE,     /* Push false literal on the stack */
    OP_NIL,       /* Push nil literal on the stack */
    OP_NEG,       /* Negate the value on top of the stack */
    OP_ADD,       /* [Pop two values of the stack and] add them [pushing the result] */
    OP_SUB,       /* -||- subtract them -||- */
    OP_MUL,       /* -||- multiply them -||- */
    OP_DIV,       /* -||- divide them -||- */
    OP_NOT,       /* Apply logical negation on the value on top of the stack. */
    OP_NOT_EQUAL, /* [Pop two values of the stack and] check for inequality */
    OP_EQUAL,     /* -||- check for equality */
    OP_EQ,        /* Check two values for equality, pop only value on top of the stack */
    OP_GREATER,   /* [Pop two values of the stack and] check if left greater than
                     right */
    OP_GREATER_EQUAL,        /* -||- check if left greater or equal than right */
    OP_LESS,                 /* -||- check if left is less than right */
    OP_LESS_EQUAL,           /* -||- check if left is less or equal than right */
    OP_PRINT,                /* Pop and print the value on the stack. */
    OP_POP,                  /* Pop the value of the stack */
    OP_POPN,                 /* Pop 'n' values of the stack */
    OP_CONST,                /* Push constant on the stack */
    OP_CONSTL,               /* Push constant on the stack long */
    OP_DEFINE_GLOBAL,        /* Pop global value off the stack (8-bit idx) and store it
                                in chunk table for globals. */
    OP_DEFINE_GLOBALL,       /* Pop global value off the stack (24-bit idx) and store it
                                in chunk table for globals */
    OP_GET_GLOBAL,           /* Push global on the stack */
    OP_GET_GLOBALL,          /* Push global on the stack long */
    OP_SET_GLOBAL,           /* Set global variable */
    OP_SET_GLOBALL,          /* Set global variable long */
    OP_GET_LOCAL,            /* Push local variable on the stack */
    OP_GET_LOCALL,           /* Push local variable on the stack long */
    OP_SET_LOCAL,            /* Set local variable */
    OP_SET_LOCALL,           /* Set local variable long */
    OP_JMP_IF_FALSE,         /* Conditional jump to instruction */
    OP_JMP_IF_FALSE_OR_POP,  /* Conditional jump to instruction or pop stack value
                              */
    OP_JMP_IF_FALSE_AND_POP, /* Pop value off the stack and jump if false */
    OP_JMP,                  /* Jump to instruction */
    OP_JMP_AND_POP,          /* Jump to instruction and pop the value of the stack */
    OP_LOOP,                 /* Jump backwards unconditionally */
    OP_CALL,                 /* Call instruction */
    OP_CALLL,                /* Call long instruction */
    OP_CLOSURE,              /* Create a closure */
    OP_GET_UPVALUE,          /* Push the upvalue on the stack */
    OP_SET_UPVALUE,          /* Set upvalue */
    OP_CLOSE_UPVAL,          /* Close upvalue */
    OP_CLOSE_UPVALN,         /* Close 'n' upvalues */
    OP_CLASS,                /* Push class constant on the stack */
    OP_CLASSL,               /* Push class constant on the stack long */
    OP_SET_PROPERTY,         /* Set class instance property */
    OP_SET_PROPERTYL,        /* Set class instance property long */
    OP_GET_PROPERTY,         /* Get class instance property */
    OP_GET_PROPERTYL,        /* Get class instance property long */
    OP_SET_DYNPROPERTY,      /* Set class instance property created at runtime */
    OP_GET_DYNPROPERTY,      /* Get class instance property created at runtime */
    OP_METHOD,               /* Get class method name */
    OP_METHODL,              /* Get class method name long */
    OP_RET,                  /* Return from function, pop the CallFrame */
} OpCode;

ARRAY_NEW(Array_UInt, UInt);
ARRAY_NEW(Array_Byte, Byte);

typedef struct {
    Value* constants; // Constants array
    UInt   clen;      // Constants array length
    UInt   ccap;      // Constants array capacity

    Array_UInt lines; // Lines array (in case of compile time errors or debug)
    Array_Byte code;  // Bytecode array
} Chunk;

void Chunk_init(Chunk* chunk);
void Chunk_write(Chunk* chunk, uint8_t byte, UInt line);
void Chunk_write_codewparam(Chunk* chunk, OpCode code, UInt idx, UInt line);
UInt Chunk_getline(Chunk* chunk, UInt index);
void Chunk_free(Chunk* chunk, VM* vm, Compiler* C);
UInt Chunk_make_constant(VM* vm, Compiler* C, Chunk* chunk, Value value);

#endif
