#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "value.h"

typedef enum {
  OP_CONST = 0,  /* Store 8-bit Value index instruction */
  OP_CONSTL = 1, /* Store 24-bit Value index instruction */
  OP_NEG = 2,    /* Unary negation instruction */
  OP_ADD = 3,    /* Binary addition instruction */
  OP_SUB = 4,    /* Binary subtraction instruction */
  OP_MUL = 5,    /* Binary multiplication instruction */
  OP_DIV = 6,    /* Binary division instruction */
  OP_RET = 7,    /* Return.. */
} OpCode;

typedef UIntArray LineArray;

typedef struct {
  LineArray lines;
  ValueArray constants;
  ByteArray code;
} Chunk;

void Chunk_init(Chunk *chunk);
void Chunk_write(Chunk *chunk, Byte byte, UInt line);
void Chunk_write_constant(Chunk *chunk, Value constant, UInt line);
UInt Chunk_getline(Chunk *chunk, UInt index);
void Chunk_free(Chunk *chunk);

#endif
