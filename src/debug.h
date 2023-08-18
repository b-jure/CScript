#ifndef __SKOOMA_DEBUG_H__
#define __SKOOMA_DEBUG_H__

#include "chunk.h"

void Chunk_debug(Chunk *chunk, const char *name);
UInt Instruction_debug(Chunk *chunk, UInt offset);
#endif
