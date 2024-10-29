/*
** ctrace.h
** Low-level bytecode tracing
** See Copyright Notice in cscript.h
*/

#ifndef CTRACE_H
#define CTRACE_H

#include "cobject.h"

CRI_FUNC Instruction crTR_tracepc(const Function *fn, const Instruction *pc);
CRI_FUNC void crTR_disassemble(cr_State *ts, const Function *fn);

#endif
