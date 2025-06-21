/*
** cvm.h
** CScript Virtual Machine
** See Copyright Notice in cscript.h
*/

#ifndef cvm_h
#define cvm_h

#include "cobject.h"
#include "cstate.h"


/* generic loop private variable offsets */
#define VAR_ITER    0  /* iterator offset */
#define VAR_STATE   1  /* invariant state offset */
#define VAR_CNTL    2  /* control variable offset */
#define VAR_TBC     3  /* to-be-closed variable offset */
#define VAR_N       4


#define csV_raweq(v1_,v2_)    csV_ordereq(NULL, v1_, v2_)


#define csV_setlist(C,l,key,val,f) \
    { f(C, l, key, val); csG_barrierback(C, obj2gco(l), val); }

#define csV_settable(C,t,key,val,f) \
    { f(C, t, key, val); csG_barrierback(C, obj2gco(t), val); }


CSI_FUNC void csV_inherit(cs_State *C, OClass *cls, OClass *scl);
CSI_FUNC void csV_call(cs_State *C, SPtr fn, int nreturns);
CSI_FUNC void csV_concat(cs_State *C, int n);
CSI_FUNC cs_Integer csV_divi(cs_State *C, cs_Integer x, cs_Integer y);
CSI_FUNC cs_Integer csV_modi(cs_State *C, cs_Integer x, cs_Integer y);
CSI_FUNC cs_Number csV_modf(cs_State *C, cs_Number x, cs_Number y);
CSI_FUNC void csV_binarithm(cs_State *C, const TValue *a, const TValue *b,
                            SPtr res, int op);
CSI_FUNC void csV_unarithm(cs_State *C, const TValue *v, SPtr res, int op);
CSI_FUNC int csV_ordereq(cs_State *C, const TValue *v1, const TValue *v2);
CSI_FUNC int csV_orderlt(cs_State *C, const TValue *v1, const TValue *v2);
CSI_FUNC int csV_orderle(cs_State *C, const TValue *v1, const TValue *v2);
CSI_FUNC void csV_execute(cs_State *C, CallFrame *cf);
CSI_FUNC void csV_rawsetstr(cs_State *C, const TValue *o, const TValue *k,
                            const TValue *val);
CSI_FUNC void csV_rawset(cs_State *C, const TValue *o, const TValue *k,
                         const TValue *val);
CSI_FUNC void csV_set(cs_State *C, const TValue *o, const TValue *k,
                      const TValue *val);
CSI_FUNC void csV_rawgetstr(cs_State *C, const TValue *o, const TValue *k,
                            SPtr res);
CSI_FUNC void csV_rawget(cs_State *C, const TValue *o, const TValue *k,
                         SPtr res);
CSI_FUNC void csV_get(cs_State *C, const TValue *o, const TValue *k,
                      SPtr res);

#endif
