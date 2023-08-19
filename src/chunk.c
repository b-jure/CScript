#include "chunk.h"
#include "mem.h"

#include <stdio.h>

static void LineArray_write(LineArray* lines, UInt line, UInt index);

void Chunk_init(Chunk* chunk)
{
    ByteArray_init(&chunk->code);
    ValueArray_init(&chunk->constants);
    UIntArray_init(&chunk->lines);
}

void Chunk_write(Chunk* chunk, uint8_t byte, UInt line)
{
    UInt idx = ByteArray_push(&chunk->code, byte);
    LineArray_write(&chunk->lines, line, idx);
}

void Chunk_free(Chunk* chunk)
{
    ByteArray_free(&chunk->code);
    ValueArray_free(&chunk->constants);
    UIntArray_free(&chunk->lines);
    // Here chunk is at the init state
}

void Chunk_write_constant(Chunk* chunk, Value constant, UInt line)
{
    UInt idx = ValueArray_push(&chunk->constants, constant);

    if (idx <= UINT8_MAX) {
        Chunk_write(chunk, OP_CONST, line);
        Chunk_write(chunk, idx, line);
    } else {
        Chunk_write(chunk, OP_CONSTL, line);
        Chunk_write(chunk, BYTE(idx, 0), line);
        Chunk_write(chunk, BYTE(idx, 1), line);
        Chunk_write(chunk, BYTE(idx, 2), line);
    }
}

UInt Chunk_getline(Chunk* chunk, UInt index)
{
    UIntArray* line_array = &chunk->lines;
    UInt idx = UIntArray_len(line_array) - 1;
    UInt instruction_idx = UIntArray_index(line_array, --idx);

    while (instruction_idx > index) {
        idx -= 2;
        instruction_idx = UIntArray_index(line_array, idx);
    }

    return UIntArray_index(line_array, idx + 1);
}

// This gets called only when writing to Chunk
/**
 * STRUCTURE RLA-ENCODED:
 * LOW ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ HIGH
 * [instruction_index, line_number, instruction_index, line_number, ...]
 */
static void LineArray_write(LineArray* lines, UInt line, UInt index)
{
    if (UIntArray_len(lines) <= 0 || UIntArray_last(lines) < line) {
        UIntArray_push(lines, index);
        UIntArray_push(lines, line);
    }
}
