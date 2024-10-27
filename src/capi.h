/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Cript.
 * Cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef CRIPTAPI_H
#define CRIPTAPI_H


/*
** If a call returns too many multiple returns, the callee may not have
** stack space to accommodate all results. In this case, this macro
** increases its stack space ('ts->cf->top.p').
*/
#define adjustresults(ts,nres) \
    { if ((nres) <= CR_MULRET && (ts)->cf->top.p < (ts)->sp.p) \
	(ts)->cf->top.p = (ts)->sp.p; }


/* Ensure the stack has at least 'n' elements. */
#define api_checknelems(ts, n) \
    api_check(ts, (n) < (ts)->sp.p - (ts)->cf->func.p, \
                 "not enough elements in the stack")


/* increments 'ts->sp.p', checking for stack overflow */
#define api_inctop(ts) \
    { (ts)->sp.p++; \
      api_check(ts, (ts)->sp.p <= (ts)->cf->top.p, "stack overflow"); }


#define hastocloseCfunc(n)	((n) < CR_MULRET)

#define codeNresults(n)		(-(n) - 3)
#define decodeNresults(n)	(-(n) - 3)

#endif
