/*
** capi.h
** Auxiliary functions for CScript API
** See Copyright Notice in cscript.h
*/

#ifndef capi_h
#define capi_h

#include "climits.h"
#include "cstate.h"

/*
** If a call returns too many multiple returns, the callee may not have
** stack space to accommodate all results. In this case, this macro
** increases its stack space ('C->cf->top.p').
*/
#define adjustresults(C,nres) \
    { if ((nres) <= CS_MULRET && (C)->cf->top.p < (C)->sp.p) \
	(C)->cf->top.p = (C)->sp.p; }


/* ensure the stack has at least 'n' elements */
#define api_checknelems(C, n) \
        api_check(C, (n) < ((C)->sp.p - (C)->cf->func.p), \
                    "not enough elements in the stack")


/* increments 'C->sp.p', checking for stack overflow */
#define api_inctop(C) \
    { (C)->sp.p++; \
      api_check(C, (C)->sp.p <= (C)->cf->top.p, "stack overflow"); }


/* checks if list index 'i' is in bounds */
#define api_checklistidx(C,i) \
        api_check(C, 0 <= (i) && (i) <= MAXLISTINDEX, \
                     "list index out of bounds");


#define hastocloseCfunc(n)	((n) < CS_MULRET)

#define codeNresults(n)		(-(n) - 3)
#define decodeNresults(n)	(-(n) - 3)

#endif
