/*
** cvm.h
** CScript Virtual Machine
** See Copyright Notice in cscript.h
*/

#ifndef CRVM_H
#define CRVM_H

#include "cobject.h"
#include "cstate.h"



/* forloop private variable offsets */
#define FORITERATOR     0 /* iterator offset */
#define FORINVSTATE     1 /* invariant state offset */
#define FORCNTLVAR      2 /* control variable offset */
#define FORTBCVAR       3 /* to-be-closed variable offset */

/* number of state variables for generic forloop */
#define NSTATEVARS      4


#define csV_raweq(v1,v2)    csV_ordereq(NULL, v1, v2)


CSI_FUNC void csV_call(cs_State *ts, SPtr fn, int nreturns);
CSI_FUNC void csV_concat(cs_State *ts, int n);
CSI_FUNC cs_Integer csV_div(cs_State *ts, cs_Integer x, cs_Integer y);
CSI_FUNC cs_Integer csV_modint(cs_State *ts, cs_Integer x, cs_Integer y);
CSI_FUNC cs_Number csV_modnum(cs_State *ts, cs_Number x, cs_Number y);
CSI_FUNC void csV_binarithm(cs_State *ts, const TValue *a, const TValue *b,
                            SPtr res, int op);
CSI_FUNC void csV_unarithm(cs_State *ts, const TValue *v, SPtr res, int op);
CSI_FUNC int csV_ordereq(cs_State *ts, const TValue *v1, const TValue *v2);
CSI_FUNC int csV_orderlt(cs_State *ts, const TValue *v1, const TValue *v2);
CSI_FUNC int csV_orderle(cs_State *ts, const TValue *v1, const TValue *v2);
CSI_FUNC void csV_execute(cs_State *ts, CallFrame *cf);
CSI_FUNC void csV_set(cs_State *ts, const TValue *obj, const TValue *key,
                      const TValue *val);
CSI_FUNC void csV_get(cs_State *ts, const TValue *obj, const TValue *key,
                      SPtr res);

#endif
