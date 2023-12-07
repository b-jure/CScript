#include "array.h"
#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "mem.h"
#include "vmachine.h"

#include <stdio.h>
#include <stdlib.h>

/**
 * RLA-ENCODED:
 * LOW ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ HIGH
 * [instruction_index, line_number, instruction_index, line_number, ...]
 *
 * Writes the current line into the chunk line array only if the line is bigger
 * than the last one. Additionally stores the index of the instruction in order
 * to retrieve it if 'Chunk_getline' gets called; it gets called only during
 * debug or run-time errors.
 */
sstatic void LineArray_write(Array_UInt* lines, UInt line, UInt index)
{
    if(Array_UInt_len(lines) <= 0 || *Array_UInt_last(lines) < line) {
        Array_UInt_push(lines, index);
        Array_UInt_push(lines, line);
    }
}

UInt Chunk_make_constant(VM* vm, Chunk* chunk, Value value)
{
    push(vm, value);
    UInt idx = Array_Value_push(&chunk->constants, value);
    pop(vm);
    return idx;
}

/* Initializes the Chunk */
void Chunk_init(Chunk* chunk, VM* vm)
{
    Array_Byte_init(&chunk->code, vm);
    Array_Value_init(&chunk->constants, vm);
    Array_UInt_init(&chunk->lines, vm);
}

/* Writes OpCodes that require no parameters */
UInt Chunk_write(Chunk* chunk, uint8_t byte, UInt line)
{
    UInt idx = Array_Byte_push(&chunk->code, byte);
    LineArray_write(&chunk->lines, line, idx);
    return idx;
}

/* Frees the chunk memory. */
void Chunk_free(Chunk* chunk)
{
    Array_Value_free(&chunk->constants, NULL);
    Array_UInt_free(&chunk->lines, NULL);
    Array_Byte_free(&chunk->code, NULL);
    // Here chunk is at the init state
}

/* Write long param (24-bit) */
sstatic force_inline void
Chunk_write_param24(Chunk* chunk, UInt param, UInt line)
{
    Chunk_write(chunk, BYTE(param, 0), line);
    Chunk_write(chunk, BYTE(param, 1), line);
    Chunk_write(chunk, BYTE(param, 2), line);
    ASSERT(
        GET_BYTES3(Array_Byte_last(&chunk->code) - 2) == param,
        "Invalid write to chunk bytecode array.");
}

sstatic force_inline UInt
Chunk_write_op(Chunk* chunk, OpCode code, bool islong, UInt idx, UInt line)
{
    UInt start = Chunk_write(chunk, code, line);
    if(!islong) Chunk_write(chunk, idx, line);
    else Chunk_write_param24(chunk, idx, line);
    return start;
}

/* Write generic OpCode-s with parameters. */
UInt Chunk_write_codewparam(Chunk* chunk, OpCode code, UInt param, UInt line)
{
#ifdef S_PRECOMPUTED_GOTO
    #define OP_TABLE
    #include "jmptable.h"
    #undef OP_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
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
        CASE(OP_POP)
        CASE(OP_LOOP)
        CASE(OP_CLOSE_UPVAL)
        CASE(OP_INHERIT)
        CASE(OP_INDEX)
        CASE(OP_SET_INDEX)
        CASE(OP_CALLSTART)
        CASE(OP_RETSTART)
        CASE(OP_MOD)
        CASE(OP_POW)
        {
            unreachable;
        }
        CASE(OP_RET)
        CASE(OP_TOPRET)
        CASE(OP_POPN)
        CASE(OP_DEFINE_GLOBALL)
        CASE(OP_GET_GLOBALL)
        CASE(OP_SET_GLOBALL)
        CASE(OP_GET_LOCALL)
        CASE(OP_SET_LOCALL)
        CASE(OP_JMP_IF_FALSE)
        CASE(OP_JMP_IF_FALSE_POP)
        CASE(OP_JMP_IF_FALSE_OR_POP)
        CASE(OP_JMP_IF_FALSE_AND_POP)
        CASE(OP_JMP)
        CASE(OP_JMP_AND_POP)
        CASE(OP_GET_UPVALUE)
        CASE(OP_SET_UPVALUE)
        CASE(OP_CLOSURE)
        CASE(OP_CLOSE_UPVALN)
        CASE(OP_CLASS)
        CASE(OP_SET_PROPERTY)
        CASE(OP_GET_PROPERTY)
        CASE(OP_METHOD)
        CASE(OP_INVOKE)
        CASE(OP_VALIST)
        CASE(OP_GET_SUPER)
        CASE(OP_INVOKE_SUPER)
        CASE(OP_INVOKE_INDEX)
        CASE(OP_CONST)
        CASE(OP_NILN)
        CASE(OP_CALL)
        CASE(OP_FOREACH);
        CASE(OP_FOREACH_END);
        {
            return Chunk_write_op(chunk, code, true, param, line);
        }
        CASE(OP_DEFINE_GLOBAL)
        CASE(OP_GET_GLOBAL)
        CASE(OP_SET_GLOBAL)
        CASE(OP_GET_LOCAL)
        CASE(OP_SET_LOCAL)
        CASE(OP_OVERLOAD)
        {
            return Chunk_write_op(chunk, code, false, param, line);
        }
    }

#undef DISPATCH
#undef CASE
#undef BREAK

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

// @TODO: Implement binary search
/* Returns the line of the current instruction. */
UInt Chunk_getline(Chunk* chunk, UInt index)
{
    Array_UInt* line_array      = &chunk->lines;
    UInt        idx             = Array_UInt_len(line_array) - 1;
    UInt        instruction_idx = *Array_UInt_index(line_array, --idx);
    while(instruction_idx > index) {
        idx             -= 2;
        instruction_idx  = *Array_UInt_index(line_array, idx);
    }
    return *Array_UInt_index(line_array, idx + 1);
}
