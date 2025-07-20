/*
** tfunction.h
** Functionts for Tokudae functions and closures
** See Copyright Notice in tokudae.h
*/

#ifndef tfunction_h
#define tfunction_h

#include "tcode.h"
#include "tobject.h"
#include "tstate.h"


/* test if 'cl' is Tokudae closure */
#define isTclosure(cl)     ((cl) != NULL && (cl)->csc.tt_ == TOKU_VCSCL)


#define sizeofTcl(nup) \
        (offsetof(TClosure, upvals) + ((nup) * cast_int(sizeof(UpVal*))))


#define sizeofCcl(nup) \
        (offsetof(CClosure, upvals) + ((nup) * cast_int(sizeof(TValue))))


/* check if thread is in 'twups' (Threads with open UPvalueS) list */
#define isintwups(C)        ((C)->twups != (C))


/* maximum amount of upvalues in a Tokudae closure */
#define MAXUPVAL        MAX_ARG_L


#define uvisopen(uv)    ((uv)->v.p != &(uv)->u.value)


#define uvlevel(uv)	check_exp(uvisopen(uv), cast(SPtr, (uv)->v.p))


/* special status to close upvalues preserving the top of the stack */
#define CLOSEKTOP       (-1)


TOKUI_FUNC Proto *tokuF_newproto(toku_State *T);
TOKUI_FUNC TClosure *tokuF_newTclosure(toku_State *T, int nup);
TOKUI_FUNC CClosure *tokuF_newCclosure(toku_State *T, int nup);
TOKUI_FUNC void tokuF_adjustvarargs(toku_State *T, int arity, CallFrame *cf,
                                    SPtr *tp, const Proto *fn);
TOKUI_FUNC void tokuF_getvarargs(toku_State *T, CallFrame *cf, SPtr *sp,
                                 int wanted);
TOKUI_FUNC void tokuF_initupvals(toku_State *T, TClosure *cl);
TOKUI_FUNC UpVal *tokuF_findupval(toku_State *T, SPtr level);
TOKUI_FUNC void tokuF_unlinkupval(UpVal *upval);
TOKUI_FUNC const char *tokuF_getlocalname(const Proto *fn, int lnum, int pc);
TOKUI_FUNC void tokuF_newtbcvar(toku_State *T, SPtr level);
TOKUI_FUNC void tokuF_closeupval(toku_State *T, SPtr level);
TOKUI_FUNC SPtr tokuF_close(toku_State *T, SPtr level, int status);
TOKUI_FUNC void tokuF_freeupval(toku_State *T, UpVal *upval);
TOKUI_FUNC void tokuF_free(toku_State *T, Proto *fn);

#endif
