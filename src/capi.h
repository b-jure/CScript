/*
** capi.h
** Auxiliary functions for CScript API
** See Copyright Notice in cscript.h
*/

#ifndef CSAPI_H
#define CSAPI_H


/*
** If a call returns too many multiple returns, the callee may not have
** stack space to accommodate all results. In this case, this macro
** increases its stack space ('ts->cf->top.p').
*/
#define adjustresults(ts,nres) \
    { if ((nres) <= CS_MULRET && (ts)->cf->top.p < (ts)->sp.p) \
	(ts)->cf->top.p = (ts)->sp.p; }


/* Ensure the stack has at least 'n' elements. */
#define api_checknelems(ts, n) \
    api_check(ts, (n) < (ts)->sp.p - (ts)->cf->func.p, \
                 "not enough elements in the stack")


/* increments 'ts->sp.p', checking for stack overflow */
#define api_inctop(ts) \
    { (ts)->sp.p++; \
      api_check(ts, (ts)->sp.p <= (ts)->cf->top.p, "stack overflow"); }


#define hastocloseCfunc(n)	((n) < CS_MULRET)

#define codeNresults(n)		(-(n) - 3)
#define decodeNresults(n)	(-(n) - 3)

#endif
