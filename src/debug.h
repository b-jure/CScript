#ifndef __SKOOMA_DEBUG_H__
#define __SKOOMA_DEBUG_H__

#include "chunk.h"

void Chunk_debug(Chunk *chunk, const char *name);
UInt Instruction_debug(Chunk *chunk, UInt offset);

#ifdef DEBUG_ASSERTIONS
#define sk_assert(expr) assert(expr)
#define sk_assertfn(fn) assert(fn)
#else
#define sk_assert(expr)
#define sk_assertfn(fn) fn
#endif

#endif
