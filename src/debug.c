#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"

#include <assert.h>
#include <stdio.h>

sdebug void Chunk_debug(Chunk* chunk, const char* name)
{
    printf("=== %s ===\n", name);
    for(UInt offset = 0; offset < chunk->code.len;)
        offset = Instruction_debug(chunk, offset);
}

sstatic Int simpleins(const char* name, UInt offset)
{
    printf("%s\n", name);
    return offset + 1; /* OpCode */
}

sstatic Int jmpins(const char* name, Int sign, Chunk* chunk, UInt offset)
{
    UInt jmp = GET_BYTES3(&chunk->code.data[offset + 1]);
    printf("%-25s %5u -> %u\n", name, offset, offset + 4 + (sign * jmp));
    return offset + 4;
}

sstatic void constant(Chunk* chunk, UInt param)
{
    printf("'");
    vprint(*Array_Value_index(&chunk->constants, param));
    printf("'");
}

sstatic UInt closure(Chunk* chunk, UInt param, UInt offset)
{
    Value value = *Array_Value_index(&chunk->constants, param);
    vprint(value);
    printf("\n");
    OFunction* fn = AS_FUNCTION(value);
    for(UInt i = 0; i < fn->upvalc; i++) {
        bool local = chunk->code.data[offset++];
        offset++; // flags
        UInt idx = GET_BYTES3(&chunk->code.data[offset]);
        printf(
            "%04d     |                                 %s %d\n",
            offset,
            local ? "local" : "upvalue",
            idx);
        offset += 3;
    }
    return offset;
}

sstatic Int shorinst(const char* name, Chunk* chunk, OpCode code, UInt offset)
{
    Byte param = *Array_Byte_index(&chunk->code, offset + 1);
    printf("%-25s %5u ", name, param);
    switch(code) {
        case OP_CONST:
        case OP_CLASS:
        case OP_SET_PROPERTY:
        case OP_GET_PROPERTY:
        case OP_METHOD:
        case OP_GET_SUPER:
            constant(chunk, param);
            break;
        case OP_OVERLOAD:
            printf("'%s'", static_str[param].name);
            break;
        default:
            // do nothing
            break;
    }
    printf("\n");
    return offset + 2; /* OpCode + param(8-bit/1-byte) */
}

sstatic Int longins(const char* name, Chunk* chunk, OpCode code, UInt offset)
{
    UInt param = GET_BYTES3(&chunk->code.data[offset + 1]);
    printf("%-25s %5u ", name, param);
    switch(code) {
        case OP_CLOSURE:
            return closure(chunk, param, offset + 4);
        case OP_CONST:
        case OP_CLASS:
        case OP_SET_PROPERTY:
        case OP_GET_PROPERTY:
        case OP_METHOD:
        case OP_GET_SUPER:
            constant(chunk, param);
            break;
        default:
            // do nothing
            break;
    }
    printf("\n");
    return offset + 4; /* OpCode(8-bit/1-byte) + param(24-bit/3-bytes) */
}

sstatic Int invoke(const char* name, Chunk* chunk, Int offset)
{
    UInt param  = GET_BYTES3(&chunk->code.data[offset + 1]);
    offset     += 4;
    Int retcnt  = GET_BYTES3(&chunk->code.data[offset]);
    printf("%-25s (retcnt %d) %5d ", name, retcnt, param);
    constant(chunk, param);
    printf("\n");
    return offset + 3;
}

sdebug UInt Instruction_debug(Chunk* chunk, UInt offset)
{
    printf("%04d ", offset);
    UInt line = Chunk_getline(chunk, offset);
    if(offset > 0 && line == Chunk_getline(chunk, offset - 1)) printf("    | ");
    else printf("%5d ", line);
    Byte instruction = chunk->code.data[offset];
    switch(instruction) {
        case OP_RET:
            return simpleins("OP_RET", offset);
        case OP_TOPRET:
            return simpleins("OP_TOPRET", offset);
        case OP_TRUE:
            return simpleins("OP_TRUE", offset);
        case OP_FALSE:
            return simpleins("OP_FALSE", offset);
        case OP_NIL:
            return simpleins("OP_NIL", offset);
        case OP_NILN:
            return longins("OP_NILN", chunk, OP_NILN, offset);
        case OP_VALIST:
            return longins("OP_VALIST", chunk, OP_VALIST, offset);
        case OP_NEG:
            return simpleins("OP_NEG", offset);
        case OP_ADD:
            return simpleins("OP_ADD", offset);
        case OP_SUB:
            return simpleins("OP_SUB", offset);
        case OP_MUL:
            return simpleins("OP_MUL", offset);
        case OP_MOD:
            return simpleins("OP_MOD", offset);
        case OP_POW:
            return simpleins("OP_POW", offset);
        case OP_DIV:
            return simpleins("OP_DIV", offset);
        case OP_NOT:
            return simpleins("OP_NOT", offset);
        case OP_NOT_EQUAL:
            return simpleins("OP_NOT_EQUAL", offset);
        case OP_EQUAL:
            return simpleins("OP_EQUAL", offset);
        case OP_EQ:
            return simpleins("OP_EQ", offset);
        case OP_GREATER_EQUAL:
            return simpleins("OP_GREATER_EQUAL", offset);
        case OP_GREATER:
            return simpleins("OP_GREATER", offset);
        case OP_LESS:
            return simpleins("OP_LESS", offset);
        case OP_LESS_EQUAL:
            return simpleins("OP_LESS_EQUAL", offset);
        case OP_POP:
            return simpleins("OP_POP", offset);
        case OP_POPN:
            return longins("OP_POPN", chunk, OP_POPN, offset);
        case OP_CONST:
            return longins("OP_CONST", chunk, OP_CONST, offset);
        case OP_DEFINE_GLOBAL:
            return shorinst("OP_DEFINE_GLOBAL", chunk, OP_DEFINE_GLOBAL, offset);
        case OP_DEFINE_GLOBALL:
            return longins(
                "OP_DEFINE_GLOBALL",
                chunk,
                OP_DEFINE_GLOBALL,
                offset);
        case OP_GET_GLOBAL:
            return shorinst("OP_GET_GLOBAL", chunk, OP_GET_GLOBAL, offset);
        case OP_GET_GLOBALL:
            return longins("OP_GET_GLOBALL", chunk, OP_GET_GLOBALL, offset);
        case OP_SET_GLOBAL:
            return shorinst("OP_SET_GLOBAL", chunk, OP_SET_GLOBAL, offset);
        case OP_SET_GLOBALL:
            return longins("OP_SET_GLOBALL", chunk, OP_SET_GLOBALL, offset);
        case OP_GET_LOCAL:
            return shorinst("OP_GET_LOCAL", chunk, OP_GET_LOCAL, offset);
        case OP_GET_LOCALL:
            return longins("OP_GET_LOCALL", chunk, OP_GET_LOCALL, offset);
        case OP_SET_LOCAL:
            return shorinst("OP_SET_LOCAL", chunk, OP_SET_LOCAL, offset);
        case OP_SET_LOCALL:
            return longins("OP_SET_LOCALL", chunk, OP_SET_LOCALL, offset);
        case OP_JMP_IF_FALSE:
            return jmpins("OP_JMP_IF_FALSE", 1, chunk, offset);
        case OP_JMP_IF_FALSE_POP:
            return jmpins("OP_JMP_IF_FALSE_POP", 1, chunk, offset);
        case OP_JMP_IF_FALSE_OR_POP:
            return jmpins("OP_JMP_IF_FALSE_OR_POP", 1, chunk, offset);
        case OP_JMP_IF_FALSE_AND_POP:
            return jmpins("OP_JMP_IF_FALSE_AND_POP", 1, chunk, offset);
        case OP_JMP:
            return jmpins("OP_JMP", 1, chunk, offset);
        case OP_JMP_AND_POP:
            return jmpins("OP_JMP_AND_POP", 1, chunk, offset);
        case OP_LOOP:
            return jmpins("OP_LOOP", -1, chunk, offset);
        case OP_CALL:
            return longins("OP_CALL", chunk, OP_CALL, offset);
        case OP_CLOSURE:
            return longins("OP_CLOSURE", chunk, OP_CLOSURE, offset);
        case OP_GET_UPVALUE:
            return longins("OP_GET_UPVALUE", chunk, OP_GET_UPVALUE, offset);
        case OP_SET_UPVALUE:
            return longins("OP_SET_UPVALUE", chunk, OP_SET_UPVALUE, offset);
        case OP_CLOSE_UPVAL:
            return simpleins("OP_CLOSE_UPVAL", offset);
        case OP_CLOSE_UPVALN:
            return longins("OP_CLOSE_UPVALN", chunk, OP_CLOSE_UPVALN, offset);
        case OP_CLASS:
            return longins("OP_CLASS", chunk, OP_CLASS, offset);
        case OP_SET_PROPERTY:
            return longins("OP_SET_PROPERTY", chunk, OP_SET_PROPERTY, offset);
        case OP_GET_PROPERTY:
            return longins("OP_GET_PROPERTY", chunk, OP_GET_PROPERTY, offset);
        case OP_INDEX:
            return simpleins("OP_INDEX", offset);
        case OP_SET_INDEX:
            return simpleins("OP_SET_INDEX", offset);
        case OP_INVOKE_INDEX:
            return longins("OP_INVOKE_INDEX", chunk, OP_INVOKE_INDEX, offset);
        case OP_METHOD:
            return longins("OP_METHOD", chunk, OP_METHOD, offset);
        case OP_INVOKE:
            return invoke("OP_INVOKE", chunk, offset);
        case OP_OVERLOAD:
            return shorinst("OP_OVERLOAD", chunk, OP_OVERLOAD, offset);
        case OP_INHERIT:
            return simpleins("OP_INHERIT", offset);
        case OP_GET_SUPER:
            return longins("OP_GET_SUPER", chunk, OP_GET_SUPER, offset);
        case OP_INVOKE_SUPER:
            return invoke("OP_INVOKE_SUPER", chunk, offset);
        case OP_CALLSTART:
            return simpleins("OP_CALLSTART", offset);
        case OP_RETSTART:
            return simpleins("OP_RETSTART", offset);
        case OP_FOREACH:
            return longins("OP_FOREACH", chunk, OP_FOREACH, offset);
        case OP_FOREACH_PREP:
            return longins("OP_FOREACH_PREP", chunk, OP_FOREACH_PREP, offset);
        default:
            printf("Unknown opcode: %d\n", instruction);
            return offset + 1;
    }
}
