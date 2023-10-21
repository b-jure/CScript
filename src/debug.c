#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"

#include <assert.h>
#include <stdio.h>

SK_INTERNAL(int)
Instruction_short(const char* name, Chunk* chunk, OpCode code, UInt offset);
SK_INTERNAL(int)
Instruction_long(const char* name, Chunk* chunk, OpCode code, UInt offset);
SK_INTERNAL(int) Instruction_simple(const char* name, UInt offset);
SK_INTERNAL(int)
Instruction_jump(const char* name, int sign, Chunk* chunk, UInt offset);

void Chunk_debug(Chunk* chunk, const char* name)
{
    printf("=== %s ===\n", name);

    for(UInt offset = 0; offset < chunk->code.len;) {
        offset = Instruction_debug(chunk, offset);
    }
}

UInt Instruction_debug(Chunk* chunk, UInt offset)
{
    printf("%04d ", offset);

    UInt line = Chunk_getline(chunk, offset);

    if(offset > 0 && line == Chunk_getline(chunk, offset - 1)) {
        printf("    | ");
    } else {
        printf("%5d ", line);
    }

    Byte instruction = chunk->code.data[offset];
    switch(instruction) {
        case OP_RET:
            return Instruction_simple("OP_RET", offset);
        case OP_TRUE:
            return Instruction_simple("OP_TRUE", offset);
        case OP_FALSE:
            return Instruction_simple("OP_FALSE", offset);
        case OP_NIL:
            return Instruction_simple("OP_NIL", offset);
        case OP_NEG:
            return Instruction_simple("OP_NEG", offset);
        case OP_ADD:
            return Instruction_simple("OP_ADD", offset);
        case OP_SUB:
            return Instruction_simple("OP_SUB", offset);
        case OP_MUL:
            return Instruction_simple("OP_MUL", offset);
        case OP_DIV:
            return Instruction_simple("OP_DIV", offset);
        case OP_NOT:
            return Instruction_simple("OP_NOT", offset);
        case OP_NOT_EQUAL:
            return Instruction_simple("OP_NOT_EQUAL", offset);
        case OP_EQUAL:
            return Instruction_simple("OP_EQUAL", offset);
        case OP_EQ:
            return Instruction_simple("OP_EQ", offset);
        case OP_GREATER_EQUAL:
            return Instruction_simple("OP_GREATER_EQUAL", offset);
        case OP_GREATER:
            return Instruction_simple("OP_GREATER", offset);
        case OP_LESS:
            return Instruction_simple("OP_LESS", offset);
        case OP_LESS_EQUAL:
            return Instruction_simple("OP_LESS_EQUAL", offset);
        case OP_PRINT:
            return Instruction_simple("OP_PRINT", offset);
        case OP_POP:
            return Instruction_simple("OP_POP", offset);
        case OP_POPN:
            return Instruction_long("OP_POPN", chunk, OP_POPN, offset);
        case OP_CONST:
            return Instruction_short("OP_CONST", chunk, OP_CONST, offset);
        case OP_CONSTL:
            return Instruction_long("OP_CONSTL", chunk, OP_CONSTL, offset);
        case OP_DEFINE_GLOBAL:
            return Instruction_short("OP_DEFINE_GLOBAL", chunk, OP_DEFINE_GLOBAL, offset);
        case OP_DEFINE_GLOBALL:
            return Instruction_long("OP_DEFINE_GLOBAL", chunk, OP_DEFINE_GLOBALL, offset);
        case OP_GET_GLOBAL:
            return Instruction_short("OP_GET_GLOBAL", chunk, OP_GET_GLOBAL, offset);
        case OP_GET_GLOBALL:
            return Instruction_long("OP_GET_GLOBALL", chunk, OP_GET_GLOBALL, offset);
        case OP_SET_GLOBAL:
            return Instruction_short("OP_SET_GLOBAL", chunk, OP_SET_GLOBAL, offset);
        case OP_SET_GLOBALL:
            return Instruction_long("OP_SET_GLOBALL", chunk, OP_SET_GLOBALL, offset);
        case OP_GET_LOCAL:
            return Instruction_short("OP_GET_LOCAL", chunk, OP_GET_LOCAL, offset);
        case OP_GET_LOCALL:
            return Instruction_long("OP_GET_LOCALL", chunk, OP_GET_LOCALL, offset);
        case OP_SET_LOCAL:
            return Instruction_short("OP_SET_LOCAL", chunk, OP_SET_LOCAL, offset);
        case OP_SET_LOCALL:
            return Instruction_long("OP_SET_LOCALL", chunk, OP_SET_LOCALL, offset);
        case OP_JMP_IF_FALSE:
            return Instruction_jump("OP_JMP_IF_FALSE", 1, chunk, offset);
        case OP_JMP_IF_FALSE_OR_POP:
            return Instruction_jump("OP_JMP_IF_FALSE_OR_POP", 1, chunk, offset);
        case OP_JMP_IF_FALSE_AND_POP:
            return Instruction_jump("OP_JMP_IF_FALSE_AND_POP", 1, chunk, offset);
        case OP_JMP:
            return Instruction_jump("OP_JMP", 1, chunk, offset);
        case OP_JMP_AND_POP:
            return Instruction_jump("OP_JMP_AND_POP", 1, chunk, offset);
        case OP_LOOP:
            return Instruction_jump("OP_LOOP", -1, chunk, offset);
        case OP_CALL:
            return Instruction_short("OP_CALL", chunk, OP_CALL, offset);
        case OP_CALLL:
            return Instruction_long("OP_CALLL", chunk, OP_CALLL, offset);
        case OP_CLOSURE:
            return Instruction_long("OP_CLOSURE", chunk, OP_CLOSURE, offset);
        case OP_GET_UPVALUE:
            return Instruction_long("OP_GET_UPVALUE", chunk, OP_GET_UPVALUE, offset);
        case OP_SET_UPVALUE:
            return Instruction_long("OP_SET_UPVALUE", chunk, OP_SET_UPVALUE, offset);
        case OP_CLOSE_UPVAL:
            return Instruction_simple("OP_CLOSE_UPVAL", offset);
        case OP_CLOSE_UPVALN:
            return Instruction_long("OP_CLOSE_UPVALN", chunk, OP_CLOSE_UPVALN, offset);
        case OP_CLASS:
            return Instruction_short("OP_CLASS", chunk, OP_CLASS, offset);
        case OP_CLASSL:
            return Instruction_long("OP_CLASSL", chunk, OP_CLASSL, offset);
        case OP_SET_PROPERTY:
            return Instruction_short("OP_SET_PROPERTY", chunk, OP_SET_PROPERTY, offset);
        case OP_SET_PROPERTYL:
            return Instruction_long("OP_SET_PROPERTYL", chunk, OP_SET_PROPERTYL, offset);
        case OP_GET_PROPERTY:
            return Instruction_short("OP_GET_PROPERTY", chunk, OP_GET_PROPERTY, offset);
        case OP_GET_PROPERTYL:
            return Instruction_long("OP_GET_PROPERTYL", chunk, OP_GET_PROPERTYL, offset);
        case OP_SET_DYNPROPERTY:
            return Instruction_simple("OP_SET_DYNPROPERTY", offset);
        case OP_GET_DYNPROPERTY:
            return Instruction_simple("OP_GET_DYNPROPERTY", offset);
        case OP_METHOD:
            return Instruction_short("OP_METHOD", chunk, OP_METHOD, offset);
        case OP_METHODL:
            return Instruction_long("OP_METHODL", chunk, OP_METHODL, offset);
        default:
            printf("Unknown opcode: %d\n", instruction);
            return offset + 1;
    }
}

SK_INTERNAL(int) Instruction_simple(const char* name, UInt offset)
{
    printf("%s\n", name);
    return offset + 1; /* OpCode */
}

SK_INTERNAL(int)
Instruction_jump(const char* name, int sign, Chunk* chunk, UInt offset)
{
    UInt jmp = GET_BYTES3(&chunk->code.data[offset + 1]);
    printf("%-25s %5u -> %u\n", name, offset, offset + 4 + (sign * jmp));
    return offset + 4;
}

SK_INTERNAL(void) disassemble_const(Chunk* chunk, UInt param)
{
    printf("'");
    Value_print(chunk->constants[param]);
    printf("'");
}

SK_INTERNAL(UInt) disassemble_closure(Chunk* chunk, UInt param, UInt offset)
{
    Value value = chunk->constants[param];
    Value_print(value);
    printf("\n");

    ObjFunction* fn = AS_FUNCTION(value);

    for(UInt i = 0; i < fn->upvalc; i++) {
        bool local = chunk->code.data[offset++];
        UInt idx   = GET_BYTES3(&chunk->code.data[offset]);
        printf(
            "%04d     |                                 %s %d\n",
            offset,
            local ? "local" : "upvalue",
            idx);
        offset += 3;
    }

    return offset;
}

SK_INTERNAL(int)
Instruction_short(const char* name, Chunk* chunk, OpCode code, UInt offset)
{
    Byte param = *Array_Byte_index(&chunk->code, offset + 1);
    printf("%-25s %5u ", name, param);
    switch(code) {
        case OP_CONST:
        case OP_CLASS:
        case OP_SET_PROPERTY:
        case OP_GET_PROPERTY:
        case OP_METHOD:
            disassemble_const(chunk, param);
            break;
        default:
            // do nothing
            break;
    }
    printf("\n");
    return offset + 2; /* OpCode + 8-bit/1-byte index */
}

SK_INTERNAL(int)
Instruction_long(const char* name, Chunk* chunk, OpCode code, UInt offset)
{
    UInt param = GET_BYTES3(&chunk->code.data[offset + 1]);
    printf("%-25s %5u ", name, param);

    switch(code) {
        case OP_CLOSURE:
            return disassemble_closure(chunk, param, offset + 4);
        case OP_CONSTL:
        case OP_CLASSL:
        case OP_SET_PROPERTYL:
        case OP_GET_PROPERTYL:
        case OP_METHODL:
            disassemble_const(chunk, param);
            break;
        default:
            // do nothing
            break;
    }
    printf("\n");
    return offset + 4; /* OpCode + 24-bit/3-byte index */
}
