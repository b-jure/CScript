/*
** cfunction.h
** Functions for CScript functions and closures
** See Copyright Notice in cscript.h
*/

#ifndef CRFUNCTION_H
#define CRFUNCTION_H

#include "ccode.h"
#include "cobject.h"
#include "cstate.h"


/* get upvalue stack level */
#define uvlevel(u)	cast(SPtr, (u)->v.p)

/* 
** Maximum amount of upvalues in a closure (both C and CScript).
** Value must fit in 'MAXLONGARGSIZE' subtracted by 'CS_GINSTANCEINDEX'.
** This is because of the nature C API functions 'index' argument works.
*/
#define MAXUPVAL        (MAXLONGARGSIZE + CS_GINSTANCEINDEX)


/* special status to close upvalues preserving the top of the stack */
#define CLOSEKTOP       (-1)


CSI_FUNC Function *csF_newfunction(cs_State *ts);
CSI_FUNC CrClosure *csF_newCrClosure(cs_State *ts, int nup);
CSI_FUNC CClosure *csF_newCClosure(cs_State *ts, int nup);
CSI_FUNC void csF_adjustvarargs(cs_State *ts, int arity, CallFrame *cf,
                                const Function *fn);
CSI_FUNC void csF_getvarargs(cs_State *ts, CallFrame *cf, int wanted);
CSI_FUNC void csF_initupvals(cs_State *ts, CrClosure *cl);
CSI_FUNC UpVal *csF_findupval(cs_State *ts, SPtr sval);
CSI_FUNC const char *csF_getlocalname(const Function *fn, int lnum, int pc);
CSI_FUNC void csF_newtbcvar(cs_State *ts, SPtr level);
CSI_FUNC void csF_closeupval(cs_State *ts, SPtr level);
CSI_FUNC SPtr csF_close(cs_State *ts, SPtr level, int status);
CSI_FUNC void csF_freeupval(cs_State *ts, UpVal *upval);
CSI_FUNC void csF_free(cs_State *ts, Function *fn);

#endif
