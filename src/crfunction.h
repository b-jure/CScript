#ifndef CRFUNCTION_H
#define CRFUNCTION_H

#include "crobject.h"


#define upvaltostk(u)		cast(SPtr, (u)->v.location)


CRI_FUNC Function *cr_function_new(cr_State *ts);
CRI_FUNC CrClosure *cr_function_newcrclosure(cr_State *ts, int nup);
CRI_FUNC CClosure *cr_function_newcclosure(cr_State *ts, cr_cfunc, int nup);
CRI_FUNC void cr_function_initupvals(cr_State *ts, CrClosure *cl);
CRI_FUNC UpVal *cr_function_findupval(cr_State *ts, SPtr sval);
CRI_FUNC void cr_function_freeupval(cr_State *ts, UpVal *upval);
CRI_FUNC void cr_function_free(cr_State *ts, Function *fn);

#endif
