#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
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
  OP_CONST,         /* Pop constant off the stack (8-bit idx)*/
  OP_CONSTL,        /* Pop constant off the stack (24-bit idx) */
  OP_DEFINE_GLOBAL, /* Pop global value off the stack (8-bit idx) and store it
                       in chunk table for globals */
  OP_DEFINE_GLOBALL, /* Pop global value off the stack (24-bit idx) and store it
                        in chunk table for globals */
  OP_GET_GLOBAL,
  OP_GET_GLOBALL,
  OP_SET_GLOBAL,
  OP_SET_GLOBALL,
  OP_RET, /* Stop interpreting ? */
} OpCode;

typedef UIntArray LineArray;

typedef struct {
  LineArray lines;
  ValueArray constants;
  ByteArray code;
} Chunk;

void Chunk_init(Chunk *chunk);
void Chunk_free(Chunk *chunk);
void Chunk_write(Chunk *chunk, uint8_t byte, UInt line);
UInt Chunk_make_constant(Chunk *chunk, Value value);
void Chunk_write_codewidx(Chunk *chunk, OpCode code, UInt idx, UInt line);
UInt Chunk_getline(Chunk *chunk, UInt index);
void Chunk_free(Chunk *chunk);

#endif
