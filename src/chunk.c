#include "chunk.h"
#include "memory.h"

#include <stdio.h>

static void
LineArray_write(LineArray* lines, UInt line, UInt index);

void
Chunk_init(Chunk* chunk)
{
  ByteArray_init(&chunk->code);
  ValueArray_init(&chunk->constants);
  UIntArray_init(&chunk->lines);
}

void
Chunk_write(Chunk* chunk, uint8_t byte, UInt line)
{
  ByteArray_push(&chunk->code, byte);
  LineArray_write(&chunk->lines, line, ByteArray_len(&chunk->code) - 1);
}

void
Chunk_free(Chunk* chunk)
{
  ByteArray_free(&chunk->code);
  ValueArray_free(&chunk->constants);
  UIntArray_free(&chunk->lines);
  // Here chunk is at the init state
}

UInt
Chunk_add_constant(Chunk* chunk, Value constant)
{
  return ValueArray_push(&chunk->constants, constant);
}

UInt
Chunk_getline(Chunk* chunk, UInt index)
{
  // No need to check if Line array has elements,
  // if we are passing in a chunk or calling this function
  // it means there was a runtime error, this implies
  // there was at least one instruction, which implies
  // there are at least 2 elements in the lines array
  // ========[instruction_index, line_number]==========

  UInt idx = UIntArray_len(&chunk->lines) - 1;
  UInt instruction_idx = UIntArray_index(&chunk->lines, --idx);

  while (instruction_idx > index) {
    idx -= 2;
    instruction_idx = UIntArray_index(&chunk->lines, idx);
  }

  return UIntArray_index(&chunk->lines, instruction_idx + 1);
}

// This gets called only when writing to Chunk
/**
 * STRUCTURE RLA-ENCODED:
 * LOW ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ HIGH
 * [instruction_index, line_number, instruction_index, line_number, ...]
 */
static void
LineArray_write(LineArray* lines, UInt line, UInt index)
{
  if (UIntArray_len(lines) == 0 || UIntArray_last(lines) != line) {
    UIntArray_push(lines, index);
    UIntArray_push(lines, line);
  }
}
