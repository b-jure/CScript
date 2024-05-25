/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "skchunk.h"
#include "skcommon.h"
#include "skdebug.h"
#include "skmem.h"
#include "skvm.h"

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
static void LineArray_write(Array_uint* lines, cr_uint line, cr_uint index)
{
    if(lines->len <= 0 || *Array_uint_last(lines) < line) {
        Array_uint_push(lines, index);
        Array_uint_push(lines, line);
    }
}

cr_uint writechunk_constant(VM* vm, Chunk* chunk, Value value)
{
    push(vm, value);
    cr_uint idx = Array_Value_push(&chunk->constants, value);
    pop(vm);
    return idx;
}

/* Initializes the Chunk */
void initchunk(Chunk* chunk, VM* vm)
{
    Array_ubyte_init(&chunk->code, vm);
    Array_Value_init(&chunk->constants, vm);
    Array_uint_init(&chunk->lines, vm);
}

/* Writes OpCodes that require no parameters */
cr_uint writechunk(Chunk* chunk, cr_ubyte byte, cr_uint line)
{
    cr_uint idx = Array_ubyte_push(&chunk->code, byte);
    LineArray_write(&chunk->lines, line, idx);
    return idx;
}

/* Frees the chunk memory. */
void freechunk(Chunk* chunk)
{
    Array_Value_free(&chunk->constants, NULL);
    Array_uint_free(&chunk->lines, NULL);
    Array_ubyte_free(&chunk->code, NULL);
    // Here chunk is at the init state
}

/* Write long param (24-bit) */
static force_inline void writechunk_param24(Chunk* chunk, cr_uint param, cr_uint line)
{
    writechunk(chunk, BYTE(param, 0), line);
    writechunk(chunk, BYTE(param, 1), line);
    writechunk(chunk, BYTE(param, 2), line);
    ASSERT(
        GET_BYTES3(Array_ubyte_last(&chunk->code) - 2) == param,
        "Invalid write to chunk bytecode array.");
}

static force_inline cr_uint
writechunk_op(Chunk* chunk, OpCode code, cr_ubyte islong, cr_uint idx, cr_uint line)
{
    cr_uint start = writechunk(chunk, code, line);
    if(!islong) writechunk(chunk, idx, line);
    else writechunk_param24(chunk, idx, line);
    return start;
}

/* Write generic OpCode-s with parameters. */
cr_uint writechunk_codewparam(Chunk* chunk, OpCode code, cr_uint param, cr_uint line)
{
#ifdef CR_PRECOMPUTED_GOTO
#define OP_TABLE
#include "skjmptable.h"
#undef OP_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK break
#endif
#undef BREAK
#define BREAK return

    DISPATCH(code)
    {
        CASE(OP_TOPRET)
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
        CASE(OP_RET0)
        CASE(OP_RET1)
        CASE(OP_RET)
        {
            unreachable;
        }
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
        CASE(OP_INVOKE0)
        CASE(OP_INVOKE1)
        CASE(OP_INVOKE)
        CASE(OP_VALIST)
        CASE(OP_GET_SUPER)
        CASE(OP_INVOKE_SUPER0)
        CASE(OP_INVOKE_SUPER1)
        CASE(OP_INVOKE_SUPER)
        CASE(OP_INVOKE_INDEX)
        CASE(OP_CONST)
        CASE(OP_NILN)
        CASE(OP_CALL0)
        CASE(OP_CALL1)
        CASE(OP_CALL)
        CASE(OP_FOREACH);
        CASE(OP_FOREACH_PREP);
        {
            return writechunk_op(chunk, code, 1, param, line);
        }
        CASE(OP_DEFINE_GLOBAL)
        CASE(OP_GET_GLOBAL)
        CASE(OP_SET_GLOBAL)
        CASE(OP_GET_LOCAL)
        CASE(OP_SET_LOCAL)
        CASE(OP_OVERLOAD)
        {
            return writechunk_op(chunk, code, 0, param, line);
        }
    }

#undef DISPATCH
#undef CASE
#undef BREAK

#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}

// @TODO: Implement binary search
/* Returns the line of the current instruction. */
cr_uint Chunk_getline(Chunk* chunk, cr_uint index)
{
    Array_uint* line_array = &chunk->lines;
    cr_uint idx = line_array->len - 1;
    cr_uint instruction_idx = *Array_uint_index(line_array, --idx);
    while(instruction_idx > index) {
        idx -= 2;
        instruction_idx = *Array_uint_index(line_array, idx);
    }
    return *Array_uint_index(line_array, idx + 1);
}
