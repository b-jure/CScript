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

/* Writes OpCodes that require no parameters */
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

/* Write OP_CONST (8-bit index) */
SK_STATIC_INLINE(void) Chunk_write_constant(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_CONST, line);
    Chunk_write(chunk, idx, line);
}

/* Write OP_CONSTL (24-bit index) */
SK_STATIC_INLINE(void) Chunk_write_constantl(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_CONSTL, line);
    Chunk_write_index24(chunk, idx, line);
}

/* Write OP_DEFINE_GLOBAL (8-bit index) */
SK_STATIC_INLINE(void) Chunk_write_global(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_DEFINE_GLOBAL, line);
    Chunk_write(chunk, idx, line);
}

/* Write OP_DEFINE_GLOBALL (24-bit index) */
SK_STATIC_INLINE(void) Chunk_write_globall(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_DEFINE_GLOBALL, line);
    Chunk_write_index24(chunk, idx, line);
}

/* Write OP_GET_GLOBAL (8-bit index) */
SK_STATIC_INLINE(void) Chunk_write_get_global(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_GET_GLOBAL, line);
    Chunk_write(chunk, idx, line);
}

/* Write OP_GET_GLOBALL (24-bit index) */
SK_STATIC_INLINE(void) Chunk_write_get_globall(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_GET_GLOBALL, line);
    Chunk_write_index24(chunk, idx, line);
}

/* Write OP_SET_GLOBAL (8-bit index) */
SK_STATIC_INLINE(void) Chunk_write_set_global(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_SET_GLOBAL, line);
    Chunk_write(chunk, idx, line);
}

/* Write OP_SET_GLOBALL (24-bit index) */
SK_STATIC_INLINE(void) Chunk_write_set_globall(Chunk* chunk, UInt idx, UInt line)
{
    Chunk_write(chunk, OP_SET_GLOBALL, line);
    Chunk_write_index24(chunk, idx, line);
}

/* Write generic OpCode-s with parameters. */
void Chunk_write_codewidx(Chunk* chunk, OpCode code, UInt idx, UInt line)
{
#ifdef THREADED_CODE
    #include "jmptable.h"
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
#endif
#undef BREAK
#define BREAK return

    DISPATCH(code)
    {
        CASE(OP_TRUE)
        CASE(OP_FALSE)
        CASE(OP_NIL)
        CASE(OP_NEG)
        CASE(OP_ADD)
        CASE(OP_SUB)
        CASE(OP_MUL)
        CASE(OP_DIV)
        CASE(OP_NOT)
        CASE(OP_NOT_EQUAL)
        CASE(OP_EQUAL)
        CASE(OP_GREATER)
        CASE(OP_GREATER_EQUAL)
        CASE(OP_LESS)
        CASE(OP_LESS_EQUAL)
        CASE(OP_PRINT)
        CASE(OP_POP)
        {
            _unreachable;
            BREAK;
        }
        CASE(OP_CONST)
        {
            Chunk_write_constant(chunk, idx, line);
            BREAK;
        }
        CASE(OP_CONSTL)
        {
            Chunk_write_constantl(chunk, idx, line);
            BREAK;
        }
        CASE(OP_DEFINE_GLOBAL)
        {
            Chunk_write_global(chunk, idx, line);
            BREAK;
        }
        CASE(OP_DEFINE_GLOBALL)
        {
            Chunk_write_globall(chunk, idx, line);
            BREAK;
        }
        CASE(OP_GET_GLOBAL)
        {
            Chunk_write_get_global(chunk, idx, line);
            BREAK;
        }
        CASE(OP_GET_GLOBALL)
        {
            Chunk_write_get_globall(chunk, idx, line);
            BREAK;
        }
        CASE(OP_SET_GLOBAL)
        {
            Chunk_write_set_global(chunk, idx, line);
            BREAK;
        }
        CASE(OP_SET_GLOBALL)
        {
            Chunk_write_set_globall(chunk, idx, line);
            BREAK;
        }
        CASE(OP_RET)
        {
            _unreachable;
            BREAK;
        }
    }
#undef DISPATCH
#undef CASE
#undef BREAK
}

UInt Chunk_make_constant(Chunk* chunk, Value value)
{
    return ValueArray_push(&chunk->constants, value);
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
