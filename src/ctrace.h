/*
** ctrace.h
** Low-level bytecode tracing
** See Copyright Notice in cscript.h
*/

#ifndef CTRACE_H
#define CTRACE_H

#include "cobject.h"

CSI_FUNC Instruction csTR_tracepc(const Function *fn, const Instruction *pc);
CSI_FUNC void csTR_disassemble(cs_State *ts, const Function *fn);

#endif
