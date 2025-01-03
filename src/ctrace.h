/*
** ctrace.h
** Low-level bytecode tracing
** See Copyright Notice in cscript.h
*/

#ifndef CTRACE_H
#define CTRACE_H

#include "cobject.h"

CSI_FUNC void csTR_tracepc(cs_State *ts, const Proto *fn, const Instruction *pc);
CSI_FUNC void csTR_disassemble(cs_State *ts, const Proto *fn);
CSI_FUNC void csTR_dumpstack(cs_State *ts, int level, const char *fmt, ...);

#endif
