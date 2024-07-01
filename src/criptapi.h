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


#include "crconf.h"



/* check if function 'nreturns' overflow stack */
#define checkapi_nreturns(ts, n, nr) \
	checkapi(ts, (nr) == (CR_MULRET) || \
			(((ts)->sp - (ts)->stack) + (n) + (nr)) < (CR_MAXSTACK), \
			"function return values overflow the stack.")


/* check if stack contains enough values */
#define checkapi_values(ts, n) \
	checkapi(ts, ((ts)->sp - last_frame(ts).callee) > (n), \
			"not enough elements in the stack.")


/* check if status code is valid */
#define checkapi_status(ts, code) \
	checkapi(ts, (code) >= 0 && (code) <= (CR_SN), "invalid errcode")


/* check if 'vtable' index is valid */
#define checkapi_vtabindex(ts, vti) \
	checkapi(ts, (vti) >= 0 && (vti) < (CR_MN), "invalid vtable index")


/* check if arithmetic operation is valid */
#define checkapi_arithop(ts, op) \
	checkapi(ts, (op) >= 0 && (op) < (CR_OPUMIN), "invalid arithmetic operation")


/* check if pointer is valid (non-NULL) */
#define checkapi_ptr(ts, ptr) \
	criptapi_check(ts, (ptr) != NULL, "NULL pointer")


/* check if stack has enough space */
#define checkapi_stack(ts, n) \
	checkapi(ts, ((ts)->sp - (ts)->stack) + cast(ptrdiff_t, n) <= CR_MAXSTACK, \
			"not enough stack space for #n element/s")


/* check if comparison operation is valid */
#define checkapi_cmpop(ts, op) \
	checkapi(ts, ((op) >= 0 && (op) < (CR_OPGE)), "invalid comparison operation")


/* increment stack pointer */
#define api_incsp(ts) \
	{ (ts)->stacktop.p++; \
	  checkapi(ts, ts->stacktop.p <= ts->aframe->top.p, \
			  "stack overflow"); }


/* decrement stack pointer */
#define api_decsp(ts) \
	{ (ts)->stacktop.p--; \
	  checkapi(ts, ts->stacktop.p >= ts->aframe->callee.p, \
			  "stack underflow"); }


#endif
