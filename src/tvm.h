/*
** tvm.h
** Tokudae Virtual Machine
** See Copyright Notice in tokudae.h
*/

#ifndef tvm_h
#define tvm_h

#include "tobject.h"
#include "tstate.h"


/* generic loop private variable offsets */
#define VAR_ITER    0  /* iterator offset */
#define VAR_STATE   1  /* invariant state offset */
#define VAR_CNTL    2  /* control variable offset */
#define VAR_TBC     3  /* to-be-closed variable offset */
#define VAR_N       4


#define tokuV_raweq(v1_,v2_)    tokuV_ordereq(NULL, v1_, v2_)


#define tokuV_setlist(C,l,key,val,f) \
    { f(C, l, key, val); tokuG_barrierback(C, obj2gco(l), val); }

#define tokuV_settable(C,t,key,val,f) \
    { f(C, t, key, val); tokuG_barrierback(C, obj2gco(t), val); }


TOKUI_FUNC void tokuV_inherit(toku_State *T, OClass *cls, OClass *scl);
TOKUI_FUNC void tokuV_call(toku_State *T, SPtr fn, int nreturns);
TOKUI_FUNC void tokuV_concat(toku_State *T, int n);
TOKUI_FUNC toku_Integer tokuV_divi(toku_State *T, toku_Integer x,
                                                  toku_Integer y);
TOKUI_FUNC toku_Integer tokuV_modi(toku_State *T, toku_Integer x,
                                                  toku_Integer y);
TOKUI_FUNC toku_Number tokuV_modf(toku_State *T, toku_Number x, toku_Number y);
TOKUI_FUNC void tokuV_binarithm(toku_State *T, const TValue *a,
                                const TValue *b, SPtr res, int op);
TOKUI_FUNC void tokuV_unarithm(toku_State *T, const TValue *v, SPtr res, int op);
TOKUI_FUNC int tokuV_ordereq(toku_State *T, const TValue *v1, const TValue *v2);
TOKUI_FUNC int tokuV_orderlt(toku_State *T, const TValue *v1, const TValue *v2);
TOKUI_FUNC int tokuV_orderle(toku_State *T, const TValue *v1, const TValue *v2);
TOKUI_FUNC void tokuV_execute(toku_State *T, CallFrame *cf);
TOKUI_FUNC void tokuV_rawsetstr(toku_State *T, const TValue *o, const TValue *k,
                                const TValue *val);
TOKUI_FUNC void tokuV_rawset(toku_State *T, const TValue *o, const TValue *k,
                             const TValue *val);
TOKUI_FUNC void tokuV_set(toku_State *T, const TValue *o, const TValue *k,
                          const TValue *val);
TOKUI_FUNC void tokuV_rawgetstr(toku_State *T, const TValue *o, const TValue *k,
                                SPtr res);
TOKUI_FUNC void tokuV_rawget(toku_State *T, const TValue *o, const TValue *k,
                             SPtr res);
TOKUI_FUNC void tokuV_get(toku_State *T, const TValue *o, const TValue *k,
                          SPtr res);

#endif
