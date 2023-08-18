#include "chunk.h"
#include "debug.h"

#include <assert.h>
#include <stdio.h>

static int Instruction_constant(const char* name, Chunk* chunk, UInt offset);
static int Instruction_simple(const char* name, UInt offset);
static int Instruction_constant_long(const char* name, Chunk* chunk, UInt offset);

void Chunk_debug(Chunk* chunk, const char* name)
{
    printf("=== %s ===\n", name);

    for (UInt offset = 0; offset < ByteArray_len(&chunk->code);) {
        offset = Instruction_debug(chunk, offset);
    }
}

UInt Instruction_debug(Chunk* chunk, UInt offset)
{
    printf("%05d ", offset);

    UInt line = Chunk_getline(chunk, offset);

    if (offset > 0 && line == Chunk_getline(chunk, offset - 1)) {
        printf("    | ");
    } else {
        printf("%5d ", line);
    }

    Byte instruction = chunk->code.data[offset];
    switch (instruction) {
        case OP_RETURN:
            return Instruction_simple("OP_RETURN", offset);
        case OP_CONSTANT:
            return Instruction_constant("OP_CONSTANT", chunk, offset);
        case OP_CONSTANT_LONG:
            return Instruction_constant_long("OP_CONSTANT_LONG", chunk, offset);
        default:
            printf("Unknown opcode: %d\n", instruction);
            return offset + 1;
    }
}

static int Instruction_simple(const char* name, UInt offset)
{
    printf("%s\n", name);
    return offset + 1; /* OpCode */
}

static int Instruction_constant(const char* name, Chunk* chunk, UInt offset)
{
    Byte constant_index = ByteArray_index(&chunk->code, offset + 1);
    printf("%-16s %5u '", name, constant_index);
    Value_print(ValueArray_index(&chunk->constants, constant_index));
    printf("'\n");
    return offset + 2; /* OpCode + 8-bit/1-byte index */
}

static int Instruction_constant_long(const char* name, Chunk* chunk, UInt offset)
{
    UInt constant_index = *(UInt*)&chunk->code.data[offset + 1];
    printf("%-16s %5u '", name, constant_index);
    Value_print(ValueArray_index(&chunk->constants, constant_index));
    printf("'\n");
    return offset + 4; /* OpCode + 24-bit/3-byte index */
}
