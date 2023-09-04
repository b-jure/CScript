#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "hashtable.h"
#include "value.h"

#define OPCODE_N ((uint32_t)(OP_RET + 1))

typedef enum {
  OP_TRUE = 0,      /* Pop true literal of the stack */
  OP_FALSE,         /* Pop false literal of the stack */
  OP_NIL,           /* Pop nil literal of the stack */
  OP_NEG,           /* Pop the value off the stack and negate it */
  OP_ADD,           /* [Pop two values of the stack and] add them */
  OP_SUB,           /* -||- subtract them */
  OP_MUL,           /* -||- multiply them */
  OP_DIV,           /* -||- divide them */
  OP_NOT,           /* Pop the value of the stack and apply logical negation */
  OP_NOT_EQUAL,     /* [Pop two values of the stack and] check for inequality */
  OP_EQUAL,         /* -||- check for equality */
  OP_GREATER,       /* -||- check if left greater than right */
  OP_GREATER_EQUAL, /* -||- check if left greater or equal than right */
  OP_LESS,          /* -||- check if left is less than right */
  OP_LESS_EQUAL,    /* -||- check if left is less or equal than right */
  OP_PRINT,         /* Pop the value off the stack and print it */
  OP_POP,           /* Pop the value of the stack */
  OP_POPN,          /* Pop 'n' values of the stack */
  OP_CONST,         /* Pop constant off the stack (8-bit idx)*/
  OP_CONSTL,        /* Pop constant off the stack (24-bit idx) */
  OP_DEFINE_GLOBAL, /* Pop global value off the stack (8-bit idx) and store it
                       in chunk table for globals */
  OP_DEFINE_GLOBALL, /* Pop global value off the stack (24-bit idx) and store it
                        in chunk table for globals */
  OP_GET_GLOBAL,     /* Push the global on the stack */
  OP_GET_GLOBALL,    /* Push global on the stack long */
  OP_SET_GLOBAL,     /* Set global variable */
  OP_SET_GLOBALL,    /* Set global variable long */
  OP_GET_LOCAL,      /* Push local variable on the stack */
  OP_GET_LOCALL,     /* Push local variable on the stack long */
  OP_SET_LOCAL,      /* Set local variable */
  OP_SET_LOCALL,     /* Set local variable long */
  OP_RET,            /* Stop interpreting ? */
} OpCode;

DECLARE_ARRAY(UInt);
DECLARE_ARRAY(Byte);

typedef UIntArray LineArray;

typedef struct {
  LineArray lines;
  ValueArray constants;
  ByteArray code;
} Chunk;

void Chunk_init(Chunk *chunk);
void Chunk_free(Chunk *chunk);
void Chunk_write(Chunk *chunk, uint8_t byte, UInt line);
void Chunk_write_codewparam(Chunk *chunk, OpCode code, UInt idx, UInt line);
UInt Chunk_getline(Chunk *chunk, UInt index);
void Chunk_free(Chunk *chunk);

force_inline UInt Chunk_make_constant(Chunk *chunk, Value value) {
  return ValueArray_push(&chunk->constants, value);
}

#endif
