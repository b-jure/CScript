#ifndef __SKOOMA_DEBUG_H__
#define __SKOOMA_DEBUG_H__

#include "chunk.h"
#include "vmachine.h"

#include <assert.h>

void Chunk_debug(Chunk *chunk, const char *name, VM *vm);
UInt Instruction_debug(Chunk *chunk, UInt offset, VM *vm);

#ifdef DEBUG_ASSERTIONS
#define sk_assert(expr) assert(expr)
#else
#define sk_assert(expr)
#endif

#endif
