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
#define checkapi_nreturns(vm, n, nr) \
	checkapi(vm, (nr) == (CR_MULRET) || \
			(((vm)->sp - (vm)->stack) + (n) + (nr)) < (CR_MAXSTACK), \
			"function return values overflow the stack.")


/* check if stack contains enough values */
#define checkapi_values(vm, n) \
	checkapi(vm, ((vm)->sp - last_frame(vm).callee) > (n), \
			"not enough elements in the stack.")


/* check if status code is valid */
#define checkapi_status(vm, code) \
	checkapi(vm, (code) >= 0 && (code) <= (CR_SN), "invalid errcode")


/* check if 'vtable' index is valid */
#define checkapi_vtabindex(vm, vti) \
	checkapi(vm, (vti) >= 0 && (vti) < (CR_MN), "invalid vtable index")


/* check if arithmetic operation is valid */
#define checkapi_arithop(vm, op) \
	checkapi(vm, (op) >= 0 && (op) < (CR_OPUMIN), "invalid arithmetic operation")


/* check if pointer is valid (non-NULL) */
#define checkapi_ptr(vm, ptr) \
	criptapi_check(vm, (ptr) != NULL, "NULL pointer")


/* check if stack has enough space */
#define checkapi_stack(vm, n) \
	checkapi(vm, ((vm)->sp - (vm)->stack) + cast(ptrdiff_t, n) <= CR_MAXSTACK, \
			"not enough stack space for #n element/s")


/* check if comparison operation is valid */
#define checkapi_cmpop(vm, op) \
	checkapi(vm, ((op) >= 0 && (op) < (CR_OPGE)), "invalid comparison operation")


/* increment stack pointer */
#define api_incsp(vm) \
	{ (vm)->sp++; checkapi(vm, vm->sp - vm->stack <= CR_MAXSTACK, "stack overflow."); }


#endif
