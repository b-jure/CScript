/*
** cfunction.h
** Functions for CScript functions and closures
** See Copyright Notice in cscript.h
*/

#ifndef CFUNCTION_H
#define CFUNCTION_H

#include "ccode.h"
#include "cobject.h"
#include "cstate.h"


/* get upvalue stack level */
#define uvlevel(u)	cast(SPtr, (u)->v.p)


/* test if upvalue is open */
#define uvisopen(uv)    ((uv)->v.p != &(uv)->u.value)


/* test if 'cl' is CScript closure */
#define isCSclosure(cl)     ((cl) != NULL && (cl)->csc.tt_ == CS_VCSCL)


/* size of 'CSClosure' */
#define sizeofCScl(nup) \
        (offsetof(CSClosure, upvals) + ((nup) * sizeof(UpVal*)))


/* size of 'CClosure' */
#define sizeofCcl(nup) \
        (offsetof(CClosure, upvals) + ((nup) * sizeof(TValue)))


/* 
** Maximum amount of upvalues in a closure (both C and CScript).
*/
#define MAXUPVAL    MAX_LARG


/* special status to close upvalues preserving the top of the stack */
#define CLOSEKTOP       (-1)


CSI_FUNC Proto *csF_newproto(cs_State *ts);
CSI_FUNC CSClosure *csF_newCSClosure(cs_State *ts, int nup);
CSI_FUNC CClosure *csF_newCClosure(cs_State *ts, int nup);
CSI_FUNC void csF_adjustvarargs(cs_State *ts, int arity, CallFrame *cf,
                                const Proto *fn);
CSI_FUNC void csF_getvarargs(cs_State *ts, CallFrame *cf, int wanted);
CSI_FUNC void csF_initupvals(cs_State *ts, CSClosure *cl);
CSI_FUNC UpVal *csF_findupval(cs_State *ts, SPtr sval);
CSI_FUNC void csF_unlinkupval(UpVal *upval);
CSI_FUNC const char *csF_getlocalname(const Proto *fn, int lnum, int pc);
CSI_FUNC void csF_newtbcvar(cs_State *ts, SPtr level);
CSI_FUNC void csF_closeupval(cs_State *ts, SPtr level);
CSI_FUNC SPtr csF_close(cs_State *ts, SPtr level, int status);
CSI_FUNC void csF_freeupval(cs_State *ts, UpVal *upval);
CSI_FUNC void csF_free(cs_State *ts, Proto *fn);

#endif
