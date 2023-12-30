#ifndef SKOOMA_DEBUG_H
#define SKOOMA_DEBUG_H

#include "chunk.h"
#include "vmachine.h"

sdebug void Chunk_debug(Chunk* chunk, const char* name);
sdebug UInt Instruction_debug(Chunk* chunk, UInt offset);
sdebug UInt Chunk_getline(Chunk* chunk, UInt index);
void dumpstack(VM* vm, CallFrame* frame, Byte* ip);
sk_noret runerror(VM* vm, Int status);
sk_noret ordererror(VM* vm, Value* a, Value* b);

#ifdef DEBUG_ASSERTIONS
#include <stdio.h>
#include <stdlib.h>
#define ASSERT(expr, fmt, ...)                                                           \
    do {                                                                                 \
        if(!(expr)) {                                                                    \
            fprintf(                                                                     \
                stderr,                                                                  \
                "Assertion failed at %d:%s\n\t'" #expr "'\n\t" fmt "\n",                 \
                __LINE__,                                                                \
                __FILE__ __VA_OPT__(, ) __VA_ARGS__);                                    \
            abort();                                                                     \
        }                                                                                \
    } while(false)
#else
#define ASSERT(expr, fmt, ...)
#endif

#define TODO(info)  ASSERT(false, "TODO: " info)
#define PANIC(info) ASSERT(false, "PANIC: " info)

#endif
