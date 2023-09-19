#include "chunk.h"
#include "common.h"
#include "mem.h"

#include <stdio.h>
#include <stdlib.h>

SK_INTERNAL(void) LineArray_write(LineArray* lines, UInt line, UInt index);

DEFINE_ARRAY(Byte);
DEFINE_ARRAY(UInt);

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

/* Write long param (24-bit) */
SK_INTERNAL(force_inline void) Chunk_write_param24(Chunk* chunk, UInt param, UInt line)
{
    Chunk_write(chunk, BYTE(param, 0), line);
    Chunk_write(chunk, BYTE(param, 1), line);
    Chunk_write(chunk, BYTE(param, 2), line);
}

SK_INTERNAL(force_inline void)
Chunk_write_op(Chunk* chunk, OpCode code, bool islong, UInt idx, UInt line)
{
    Chunk_write(chunk, code, line);
    if(!islong) {
        Chunk_write(chunk, idx, line);
    } else {
        Chunk_write_param24(chunk, idx, line);
    }
}

/* Write generic OpCode-s with parameters. */
void Chunk_write_codewparam(Chunk* chunk, OpCode code, UInt param, UInt line)
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
        CASE(OP_EQ)
        CASE(OP_GREATER)
        CASE(OP_GREATER_EQUAL)
        CASE(OP_LESS)
        CASE(OP_LESS_EQUAL)
        CASE(OP_PRINT)
        CASE(OP_POP)
        CASE(OP_LOOP)
        {
            unreachable;
        }
        CASE(OP_POPN)
        CASE(OP_CONSTL)
        CASE(OP_DEFINE_GLOBALL)
        CASE(OP_GET_GLOBALL)
        CASE(OP_SET_GLOBALL)
        CASE(OP_GET_LOCALL)
        CASE(OP_SET_LOCALL)
        CASE(OP_JMP_IF_FALSE)
        CASE(OP_JMP_IF_FALSE_OR_POP)
        CASE(OP_JMP_IF_FALSE_AND_POP)
        CASE(OP_JMP)
        CASE(OP_JMP_AND_POP)
        CASE(OP_CALLL)
        {
            Chunk_write_op(chunk, code, true, param, line);
            BREAK;
        }
        CASE(OP_CONST)
        CASE(OP_DEFINE_GLOBAL)
        CASE(OP_GET_GLOBAL)
        CASE(OP_SET_GLOBAL)
        CASE(OP_GET_LOCAL)
        CASE(OP_SET_LOCAL)
        CASE(OP_CALL)
        {
            Chunk_write_op(chunk, code, false, param, line);
            BREAK;
        }
        CASE(OP_RET)
        {
            unreachable;
        }
    }
#undef DISPATCH
#undef CASE
#undef BREAK
}

/* Returns the line of the current instruction (DEBUG ONLY) */
UInt Chunk_getline(Chunk* chunk, UInt index)
{
    LineArray* line_array      = &chunk->lines;
    UInt       idx             = UIntArray_len(line_array) - 1;
    UInt       instruction_idx = *UIntArray_index(line_array, --idx);

    while(instruction_idx > index) {
        idx             -= 2;
        instruction_idx = *UIntArray_index(line_array, idx);
    }

    return *UIntArray_index(line_array, idx + 1);
}

/**
 * RLA-ENCODED:
 * LOW ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ HIGH
 * [instruction_index, line_number, instruction_index, line_number, ...]
 *
 * Writes the current line into the chunk line array only if the line is bigger than the
 * last one. Additionally stores the index of the instruction in order to retrieve it if
 * 'Chunk_getline' gets called; it gets called only during debug or run-time errors.
 */
SK_INTERNAL(void) LineArray_write(LineArray* lines, UInt line, UInt index)
{
    if(UIntArray_len(lines) <= 0 || *UIntArray_last(lines) < line) {
        UIntArray_push(lines, index);
        UIntArray_push(lines, line);
    }
}
