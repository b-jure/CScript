#ifndef SKOOMA_CHUNK_H
#define SKOOMA_CHUNK_H

#include "array.h"
#include "common.h"
#include "hashtable.h"
#include "value.h"

ARRAY_NEW(Array_Byte, Byte);

#define OPCODE_N ((uint32_t)(OP_RET + 1))

typedef enum {
    OP_TRUE = 0, /* Push true literal on the stack */
    OP_FALSE, /* Push false literal on the stack */
    OP_NIL, /* Push nil literal on the stack */
    OP_NILN,
    OP_NEG, /* Negate the value on top of the stack */
    OP_ADD, /* [Pop two values of the stack and] add them [pushing the result] */
    OP_SUB, /* -||- subtract them -||- */
    OP_MUL, /* -||- multiply them -||- */
    OP_DIV, /* -||- divide them -||- */
    OP_MOD, /* -||- modulo them -||- */
    OP_POW, /* Binary exponentiation operation (power of 2) */
    OP_NOT, /* Apply logical negation on the value on top of the stack. */
    OP_VALIST, /* '...' literal, variable argument list */
    OP_NOT_EQUAL, /* [Pop two values of the stack and] check for inequality */
    OP_EQUAL, /* -||- check for equality */
    OP_EQ, /* Check two values for equality, pop only value on top of the stack
            */
    OP_GREATER, /* [Pop two values of the stack and] check if left greater than
                   right */
    OP_GREATER_EQUAL, /* -||- check if left greater or equal than right */
    OP_LESS, /* -||- check if left is less than right */
    OP_LESS_EQUAL, /* -||- check if left is less or equal than right */
    OP_POP, /* Pop the value of the stack */
    OP_POPN, /* Pop 'n' values of the stack */
    OP_CONST, /* Push constant on the stack */
    OP_DEFINE_GLOBAL, /* Pop global value off the stack (8-bit idx) and store it
                         in chunk table for globals. */
    OP_DEFINE_GLOBALL, /* Pop global value off the stack (24-bit idx) and store
                          it in chunk table for globals */
    OP_GET_GLOBAL, /* Push global on the stack */
    OP_GET_GLOBALL, /* Push global on the stack long */
    OP_SET_GLOBAL, /* Set global variable */
    OP_SET_GLOBALL, /* Set global variable long */
    OP_GET_LOCAL, /* Push local variable on the stack */
    OP_GET_LOCALL, /* Push local variable on the stack long */
    OP_SET_LOCAL, /* Set local variable */
    OP_SET_LOCALL, /* Set local variable long */
    OP_JMP_IF_FALSE, /* Conditional jump to instruction */
    OP_JMP_IF_FALSE_POP, /* Jump if false and pop unconditionally */
    OP_JMP_IF_FALSE_OR_POP, /* Conditional jump to instruction or pop stack value
                             */
    OP_JMP_IF_FALSE_AND_POP, /* Conditional jump to instruction and pop stack
                                value */
    OP_JMP, /* Jump to instruction */
    OP_JMP_AND_POP, /* Jump to instruction and pop the value of the stack */
    OP_LOOP, /* Jump backwards unconditionally */
    OP_CALL, /* Call instruction */
    OP_CLOSURE, /* Create a closure */
    OP_GET_UPVALUE, /* Push the upvalue on the stack */
    OP_SET_UPVALUE, /* Set upvalue */
    OP_CLOSE_UPVAL, /* Close upvalue */
    OP_CLOSE_UPVALN, /* Close 'n' upvalues */
    OP_CLASS, /* Push class constant on the stack */
    OP_SET_PROPERTY, /* Set class instance property */
    OP_GET_PROPERTY, /* Get class instance property */
    OP_INDEX, /* Index operator */
    OP_SET_INDEX, /* Set class instance property dynamically */
    OP_INVOKE_INDEX, /* Invoke class method name resolved dynamically */
    OP_METHOD, /* Create class method */
    OP_INVOKE, /* Invoke class method */
    OP_OVERLOAD, /* Overload operator or initializer for a class */
    OP_INHERIT, /* Inherit class properties. */
    OP_GET_SUPER, /* Fetch superclass method */
    OP_INVOKE_SUPER, /* Invoke superclass method call */
    OP_CALLSTART, /* Start of call instruction arguments */
    OP_RETSTART, /* Start of return instruction values */
    OP_FOREACH,
    OP_FOREACH_END,
    OP_TOPRET, /* Return from top-level function */
    OP_RET, /* Return from function, pop the CallFrame */
} OpCode;

ARRAY_NEW(Array_UInt, UInt);

typedef struct {
    Array_Value constants; // Constant values
    Array_UInt  lines; // Lines array (in case of compile time errors or debug)
    Array_Byte  code; // Bytecode array
} Chunk;

void Chunk_init(Chunk* chunk, VM* vm);
UInt Chunk_write(Chunk* chunk, uint8_t byte, UInt line);
UInt Chunk_write_codewparam(Chunk* chunk, OpCode code, UInt idx, UInt line);
void Chunk_free(Chunk* chunk);
UInt Chunk_make_constant(VM* vm, Chunk* chunk, Value value);

#endif
