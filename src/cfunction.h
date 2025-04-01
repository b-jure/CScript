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


/* test if 'cl' is CScript closure */
#define isCSclosure(cl)     ((cl) != NULL && (cl)->csc.tt_ == CS_VCSCL)


#define sizeofCScl(nup) \
        (offsetof(CSClosure, upvals) + ((nup) * cast_int(sizeof(UpVal*))))


#define sizeofCcl(nup) \
        (offsetof(CClosure, upvals) + ((nup) * cast_int(sizeof(TValue))))


/* check if thread is in 'twups' (Threads with open UPvalueS) list */
#define isintwups(C)        ((C)->twups != (C))


/* maximum amount of upvalues in a closure (both C and CScript) */
#define MAXUPVAL    MAX_ARG_L


#define uvisopen(uv)    ((uv)->v.p != &(uv)->u.value)


#define uvlevel(uv)	check_exp(uvisopen(uv), cast(SPtr, (uv)->v.p))



/* special status to close upvalues preserving the top of the stack */
#define CLOSEKTOP       (-1)


CSI_FUNC Proto *csF_newproto(cs_State *C);
CSI_FUNC CSClosure *csF_newCSClosure(cs_State *C, int nup);
CSI_FUNC CClosure *csF_newCClosure(cs_State *C, int nup);
CSI_FUNC void csF_adjustvarargs(cs_State *C, int arity, CallFrame *cf,
                                const Proto *fn);
CSI_FUNC void csF_getvarargs(cs_State *C, CallFrame *cf, int wanted);
CSI_FUNC void csF_initupvals(cs_State *C, CSClosure *cl);
CSI_FUNC UpVal *csF_findupval(cs_State *C, SPtr sval);
CSI_FUNC void csF_unlinkupval(UpVal *upval);
CSI_FUNC const char *csF_getlocalname(const Proto *fn, int lnum, int pc);
CSI_FUNC void csF_newtbcvar(cs_State *C, SPtr level);
CSI_FUNC void csF_closeupval(cs_State *C, SPtr level);
CSI_FUNC SPtr csF_close(cs_State *C, SPtr level, int status);
CSI_FUNC void csF_freeupval(cs_State *C, UpVal *upval);
CSI_FUNC void csF_free(cs_State *C, Proto *fn);

#endif
