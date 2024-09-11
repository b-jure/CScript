#ifndef CRFUNCTION_H
#define CRFUNCTION_H

#include "crobject.h"


#define upvaltostk(u)		cast(SPtr, (u)->v.location)


CRI_FUNC Function *crF_new(cr_State *ts);
CRI_FUNC CrClosure *crF_newcrclosure(cr_State *ts, int nup);
CRI_FUNC CClosure *crF_newcclosure(cr_State *ts, cr_CFunction, int nup);
CRI_FUNC void crF_initupvals(cr_State *ts, CrClosure *cl);
CRI_FUNC UpVal *crF_findupval(cr_State *ts, SPtr sval);
CRI_FUNC void crF_freeupval(cr_State *ts, UpVal *upval);
CRI_FUNC void crF_free(cr_State *ts, Function *fn);

#endif
