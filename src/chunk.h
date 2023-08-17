#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "value.h"

typedef enum {
  OP_RETURN,
  OP_CONSTANT,
} OpCode;

typedef UIntArray LineArray;

typedef struct {
  LineArray lines;
  ValueArray constants;
  ByteArray code;
} Chunk;

void Chunk_init(Chunk *chunk);
void Chunk_write(Chunk *chunk, uint8_t byte, UInt line);
UInt Chunk_getline(Chunk *chunk, UInt index);
UInt Chunk_add_constant(Chunk *chunk, Value constant);
void Chunk_free(Chunk *chunk);

#endif
