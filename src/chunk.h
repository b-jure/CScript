#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "value.h"

typedef enum {
  OP_RETURN,
  OP_CONSTANT,      /* Stores index up to 8 bits */
  OP_CONSTANT_LONG, /* Stores index up to 24 bits */
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
