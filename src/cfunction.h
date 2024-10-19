#ifndef CRFUNCTION_H
#define CRFUNCTION_H

#include "cobject.h"
#include "cstate.h"


#define uvlevel(u)		cast(SPtr, (u)->v.p)

/* special status to close upvalues preserving the top of the stack */
#define CLOSEKTOP       (-1)


CRI_FUNC Function *crF_new(cr_State *ts);
CRI_FUNC CrClosure *crF_newCrClosure(cr_State *ts, int nup);
CRI_FUNC CClosure *crF_newCClosure(cr_State *ts, cr_CFunction, int nup);
CRI_FUNC void crF_adjustvarargs(cr_State *ts, int arity, CallFrame *cf,
                                const Function *fn);
CRI_FUNC void crF_getvarargs(cr_State *ts, CallFrame *cf, int wanted);
CRI_FUNC void crF_initupvals(cr_State *ts, CrClosure *cl);
CRI_FUNC UpVal *crF_findupval(cr_State *ts, SPtr sval);
CRI_FUNC const char *crF_getlocalname(const Function *fn, int lnum, int pc);
CRI_FUNC void crF_newtbcvar(cr_State *ts, SPtr level);
CRI_FUNC void crF_closeupval(cr_State *ts, SPtr level);
CRI_FUNC SPtr crF_close(cr_State *ts, SPtr level, int status);
CRI_FUNC void crF_freeupval(cr_State *ts, UpVal *upval);
CRI_FUNC void crF_free(cr_State *ts, Function *fn);

#endif