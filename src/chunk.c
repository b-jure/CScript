#include "chunk.h"
#include "common.h"
#include "mem.h"

#include <stdio.h>

SK_STATIC_INLINE(void) LineArray_write(LineArray* lines, UInt line, UInt index);

/* Initializes the Chunk */
void Chunk_init(Chunk* chunk)
{
    ByteArray_init(&chunk->code);
    ValueArray_init(&chunk->constants);
    UIntArray_init(&chunk->lines);
}

/* Writes OpCode or constants info into chunk while also
 * storing line information for the current byte being written. */
void Chunk_write(Chunk* chunk, uint8_t byte, UInt line)
{
    UInt idx = ByteArray_push(&chunk->code, byte);
    LineArray_write(&chunk->lines, line, idx);
}

/* Frees the chunk memory (arrays) */
void Chunk_free(Chunk* chunk)
{
    ByteArray_free(&chunk->code);
    ValueArray_free(&chunk->constants);
    UIntArray_free(&chunk->lines);
    // Here chunk is at the init state
}

/* Write long index (24-bit) */
SK_STATIC_INLINE(void) Chunk_write_index24(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, BYTE(idx, 0), line);
    Chunk_write(chunk, BYTE(idx, 1), line);
    Chunk_write(chunk, BYTE(idx, 2), line);
}

void Chunk_write_constant(Chunk* chunk, UInt idx, UInt line)
{
    if(idx <= UINT8_MAX) {
        Chunk_write(chunk, OP_CONST, line);
        Chunk_write(chunk, idx, line);
    } else {
        Chunk_write(chunk, OP_CONSTL, line);
        Chunk_write_index24(chunk, idx, line);
    }
}

UInt Chunk_make_constant(Chunk* chunk, Value value)
{
    return ValueArray_push(&chunk->constants, value);
}

void Chunk_write_global(Chunk* chunk, UInt idx, UInt line)
{
    if(idx <= UINT8_MAX) {
        Chunk_write(chunk, OP_DEFINE_GLOBAL, line);
        Chunk_write(chunk, idx, line);
    } else {
        Chunk_write(chunk, OP_DEFINE_GLOBALL, line);
        Chunk_write_index24(chunk, idx, line);
    }
}

void Chunk_write_opcode(Chunk* chunk, UInt idx, UInt line)
{
    // @TODO: Implement
}

void Chunk_write_global_get(Chunk* chunk, UInt idx, UInt line)
{
    if(idx <= UINT8_MAX) {
        Chunk_write(chunk, OP_GET_GLOBAL, line);
        Chunk_write(chunk, idx, line);
    } else {
        Chunk_write(chunk, OP_GET_GLOBALL, line);
        Chunk_write_index24(chunk, idx, line);
    }
}

/* Returns the line of the current instruction (DEBUG ONLY) */
UInt Chunk_getline(Chunk* chunk, UInt index)
{
    UIntArray* line_array      = &chunk->lines;
    UInt       idx             = UIntArray_len(line_array) - 1;
    UInt       instruction_idx = UIntArray_index(line_array, --idx);

    while(instruction_idx > index) {
        idx             -= 2;
        instruction_idx = UIntArray_index(line_array, idx);
    }

    return UIntArray_index(line_array, idx + 1);
}

/**
 * RLA-ENCODED:
 * LOW ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ HIGH
 * [instruction_index, line_number, instruction_index, line_number, ...]
 *
 * Writes the current line into chunk line array only if the line is bigger than the last
 * one. Additionally stores the index of the instruction in order to retrieve it if
 * 'Chunk_getline' gets called; gets called only during debug or run-time errors.
 */
SK_STATIC_INLINE(void) LineArray_write(LineArray* lines, UInt line, UInt index)
{
    if(UIntArray_len(lines) <= 0 || UIntArray_last(lines) < line) {
        UIntArray_push(lines, index);
        UIntArray_push(lines, line);
    }
}
