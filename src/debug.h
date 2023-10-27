#ifndef __SKOOMA_DEBUG_H__
#define __SKOOMA_DEBUG_H__

#include "chunk.h"
#include "vmachine.h"

#include <assert.h>

void Chunk_debug(Chunk* chunk, const char* name);
UInt Instruction_debug(Chunk* chunk, UInt offset);

#ifdef DEBUG

    /* Debug flag for debugging chunks. */
    #define DEBUG_PRINT_CODE

    /* Debug flag for printing VM stack. */
    #define DEBUG_TRACE_EXECUTION

    /* Debug flag for assertions */
    //#define DEBUG_ASSERTIONS

    /* Debug flag for garbage collection stress test */
    //#define DEBUG_STRESS_GC

    /* Debug flag for logging garbage collection */
    //#define DEBUG_LOG_GC

#endif

#endif
