/*
** tapi.h
** Auxiliary functions for Tokudae API
** See Copyright Notice in tokudae.h
*/

#ifndef tapi_h
#define tapi_h

#include "tokudaelimits.h"
#include "tstate.h"

/*
** If a call returns too many multiple returns, the callee may not have
** stack space to accommodate all results. In this case, this macro
** increases its stack space ('T->cf->top.p').
*/
#define adjustresults(T,nres) \
    { if ((nres) <= TOKU_MULTRET && (T)->cf->top.p < (T)->sp.p) \
	(T)->cf->top.p = (T)->sp.p; }


/* ensure the stack has at least 'n' elements */
#define api_checknelems(T, n) \
        api_check(T, (n) < ((T)->sp.p - (T)->cf->func.p), \
                    "not enough elements in the stack")


/* increments 'T->sp.p', checking for stack overflow */
#define api_inctop(T) \
    { (T)->sp.p++; \
      api_check(T, (T)->sp.p <= (T)->cf->top.p, "stack overflow"); }


/* checks if list index 'i' is in bounds */
#define api_checklistidx(T,i) \
        api_check(T, 0 <= (i) && (i) <= TOKU_MAXLISTINDEX, \
                     "list index out of bounds");


#define hastocloseCfunc(n)      ((n) < TOKU_MULTRET)

#define codeNresults(n)		(-(n) - 3)
#define decodeNresults(n)	(-(n) - 3)

#endif
