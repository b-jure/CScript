#include "chunk.h"
#include "debug.h"

#include <stdio.h>

static int
Instruction_constant(const char* name, Chunk* chunk, uint32_t offset);
static int
Instruction_simple(const char* name, uint32_t offset);

void
Chunk_debug(Chunk* chunk, const char* name)
{
  printf("=== %s ===\n", name);

  for (uint32_t offset = 0; offset < ByteArray_len(&chunk->code);) {
    offset = Instruction_debug(chunk, offset);
  }
}

int
Instruction_debug(Chunk* chunk, int offset)
{
  printf("%04d ", offset);

  UInt line = Chunk_getline(chunk, offset);
  if (offset > 0 && line == Chunk_getline(chunk, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", line);
  }

  uint8_t instruction = chunk->code.data[offset];
  switch (instruction) {
    case OP_RETURN:
      return Instruction_simple("OP_RETURN", offset);
    case OP_CONSTANT:
      return Instruction_constant("OP_CONSTANT", chunk, offset);
    default:
      printf("Unknown opcode: %d\n", instruction);
      return offset + 1;
  }
}

static int
Instruction_simple(const char* name, uint32_t offset)
{
  printf("%s\n", name);
  return offset + 1;
}

static int
Instruction_constant(const char* name, Chunk* chunk, uint32_t offset)
{
  uint8_t constant_index = chunk->code.data[offset + 1];
  printf("%-16s %4d '", name, constant_index);
  Value_print(chunk->constants.data[constant_index]);
  printf("'\n");
  return offset + 2;
}
