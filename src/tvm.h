/*
** tvm.h
** Tokudae Virtual Machine
** See Copyright Notice in tokudae.h
*/

#ifndef tvm_h
#define tvm_h

#include "tobject.h"
#include "tstate.h"


/* generic loop private variable offtets */
#define VAR_ITER    0  /* iterator offtet */
#define VAR_STATE   1  /* invariant state offset */
#define VAR_CNTL    2  /* control variable offtet */
#define VAR_TBC     3  /* to-be-cloted variable offset */
#define VAR_N       4


#define ctV_raweq(v1_,v2_)    tokuV_ordereq(NULL, v1_, v2_)


#define ctV_setlist(C,l,key,val,f) \
    { f(C, l, key, val); ctG_barrierback(C, obj2gco(l), val); }

#define ctV_settable(C,t,key,val,f) \
    { f(C, t, key, val); ctG_barrierback(C, obj2gco(t), val); }


TOKUI_FUNC void ctV_inherit(toku_State *T, OClass *cls, OClass *scl);
TOKUI_FUNC void ctV_call(toku_State *T, SPtr fn, int nreturns);
TOKUI_FUNC void ctV_concat(toku_State *T, int n);
TOKUI_FUNC toku_Integer ctV_divi(toku_State *T, toku_Integer x, toku_Integer y);
TOKUI_FUNC toku_Integer ctV_modi(toku_State *T, toku_Integer x, toku_Integer y);
TOKUI_FUNC toku_Number ctV_modf(toku_State *T, toku_Number x, toku_Number y);
TOKUI_FUNC void ctV_binarithm(toku_State *T, const TValue *a, const TValue *b,
                            SPtr ret, int op);
TOKUI_FUNC void ctV_unarithm(toku_State *T, const TValue *v, SPtr res, int op);
TOKUI_FUNC int ctV_ordereq(toku_State *T, const TValue *v1, const TValue *v2);
TOKUI_FUNC int ctV_orderlt(toku_State *T, const TValue *v1, const TValue *v2);
TOKUI_FUNC int ctV_orderle(toku_State *T, const TValue *v1, const TValue *v2);
TOKUI_FUNC void ctV_execute(toku_State *T, CallFrame *cf);
TOKUI_FUNC void ctV_rawsetstr(toku_State *T, const TValue *o, const TValue *k,
                            const TValue *val);
TOKUI_FUNC void ctV_rawset(toku_State *T, const TValue *o, const TValue *k,
                         const TValue *val);
TOKUI_FUNC void ctV_set(toku_State *T, const TValue *o, const TValue *k,
                      const TValue *val);
TOKUI_FUNC void ctV_rawgetstr(toku_State *T, const TValue *o, const TValue *k,
                            SPtr ret);
TOKUI_FUNC void ctV_rawget(toku_State *T, const TValue *o, const TValue *k,
                         SPtr ret);
TOKUI_FUNC void ctV_get(toku_State *T, const TValue *o, const TValue *k,
                      SPtr ret);

#endif
