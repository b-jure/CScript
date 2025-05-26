/*
** ctrace.h
** Functions for low-level bytecode debugging and tracing
** See Copyright Notice in cscript.h
*/

#ifndef ctrace_h
#define ctrace_h

#include "cobject.h"

CSI_FUNC void csTR_tracepc(cs_State *C, SPtr sp, const Proto *fn,
                           const Instruction *pc, int tolevel);
CSI_FUNC void csTR_disassemble(cs_State *C, const Proto *fn);
CSI_FUNC void csTR_dumpstack(cs_State *C, int level, const char *fmt, ...);

#endif
