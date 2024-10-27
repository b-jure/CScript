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


#define crV_raweq(v1,v2)    crV_ordereq(NULL, v1, v2)


CRI_FUNC void crV_call(cr_State *ts, SPtr fn, int nreturns);
CRI_FUNC void crV_concat(cr_State *ts, int n);
CRI_FUNC cr_Integer crV_div(cr_State *ts, cr_Integer x, cr_Integer y);
CRI_FUNC cr_Integer crV_modint(cr_State *ts, cr_Integer x, cr_Integer y);
CRI_FUNC cr_Number crV_modnum(cr_State *ts, cr_Number x, cr_Number y);
CRI_FUNC void crV_binarithm(cr_State *ts, const TValue *a, const TValue *b,
                            SPtr res, int op);
CRI_FUNC void crV_unarithm(cr_State *ts, const TValue *v, SPtr res, int op);
CRI_FUNC int crV_ordereq(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC int crV_orderlt(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC int crV_orderle(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC void crV_execute(cr_State *ts, CallFrame *cf);
CRI_FUNC void crV_set(cr_State *ts, TValue *obj, const TValue *key,
                      const TValue *val);
CRI_FUNC void crV_get(cr_State *ts, const TValue *obj, const TValue *key,
                      SPtr res);

#endif
