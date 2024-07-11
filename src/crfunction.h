#ifndef CRFUNCTION_H
#define CRFUNCTION_H

#include "crobject.h"


#define upvaltostk(u)		cast(SPtr, (u)->v.location)


Function *cr_function_new(cr_State *ts);
CrClosure *cr_function_newcrclosure(cr_State *ts, Function *fn, int nup);
CClosure *cr_function_newcclosure(cr_State *ts, cr_cfunc, int nup);
UpVal *cr_function_findupval(cr_State *ts, SPtr sval);

#endif
