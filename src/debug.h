#ifndef SKOOMA_DEBUG_H
#define SKOOMA_DEBUG_H

#include "chunk.h"
#include "vmachine.h"

sdebug void Chunk_debug(VM* vm, Chunk* chunk, const char* name);
sdebug uint32_t Instruction_debug(VM* vm, Chunk* chunk, uint32_t offset);
sdebug uint32_t Chunk_getline(Chunk* chunk, uint32_t index);
void dumpstack(VM* vm, CallFrame* frame, Byte* ip);

// Internal assertions
#ifdef DEBUG_ASSERTIONS
#include <stdio.h>
#include <stdlib.h>
#define ASSERT(expr, fmt, ...)                                                                     \
    do {                                                                                           \
        if(!(expr)) {                                                                              \
            fprintf(                                                                               \
                stderr,                                                                            \
                "Assertion failed at %d:%s\n\t'" #expr "'\n\t" fmt "\n",                           \
                __LINE__,                                                                          \
                __FILE__ __VA_OPT__(, ) __VA_ARGS__);                                              \
            abort();                                                                               \
        }                                                                                          \
    } while(false)
#else
#define ASSERT(expr, fmt, ...)
#endif

#define TODO(info)  ASSERT(false, "TODO: " info)
#define PANIC(info) ASSERT(false, "PANIC: " info)

#endif
