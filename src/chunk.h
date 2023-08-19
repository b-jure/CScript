#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "value.h"

typedef enum {
  OP_CONST = 0, /* Store 8-bit Value index instruction */
  OP_CONSTL,    /* Store 24-bit Value index instruction */
  OP_NEG,       /* Unary negation instruction */
  OP_ADD,       /* Binary addition instruction */
  OP_SUB,       /* Binary subtraction instruction */
  OP_MUL,       /* Binary multiplication instruction */
  OP_DIV,       /* Binary division instruction */
  OP_RET,       /* Return.. */
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
