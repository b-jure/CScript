/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef CRERR_H
#define CRERR_H

#include "cript.h"
#include "crvalue.h"


/* ==================== runtime errors ====================== */

/* 
 * Lower-level error invocation, skips the part where
 * we check if the function is protected and instead
 * prints the stack trace and invokes panic handler. 
 */
cr_noret printandpanic(TState* ts);

/* Generic runtime error */
cr_noret runerror(TState* ts, int status);

/* Memory allocation error */
cr_noret memerror(TState* ts);

/* Bytecode limit exceeded error */
cr_noret limiterror(TState *ts, cr_umem limit, const char *extra, ...);

/* 'Vec' size limit exceeded error */
#define veclimiterror(ts, v)	limiterror(ts, VECSIZE_LIMIT, #v " size")

/* gray stack size limit exceeded error */
#define gslimiterror(ts)	limiterror(ts, ts_GRAYSTACK_LIMIT, "gray stack size")

/* Ordering error */
cr_noret ordererror(TState* ts, Value a, Value b);

/* Binary/Unary arithmetic operation error */
#define operror(ts, l, r, op)                                                                      \
    (arisbin(op) ? binoperror(ts, l, r, cast(cr_om, op)) : unoperror(ts, l, cast(cr_om, op)))

/* Binary arithmetic operation error */
cr_noret binoperror(TState* ts, Value a, Value b, cr_om op);

/* Unary arithmetic operation error */
cr_noret unoperror(TState* ts, Value a, cr_om op);

/* Overload-able method didn't return valid type (what) error */
cr_noret omreterror(TState* ts, const char* what, cr_om tag);

/* Object string format error */
cr_noret ofmterror(TState* ts, cr_ubyte c, Value callee);

/* Stack overflow error */
cr_noret sovferror(TState* ts);

/* Undefined property error */
cr_noret udperror(TState* ts, Value property, Value oclass);

/* Return count stack overflow */
cr_noret retovferror(TState* ts, const char* fn);

/* Function invalid argument count error */
cr_noret arityerror(TState* ts, int expected, int got);

/* Call stack overflow (frame count) */
cr_noret fcovferror(TState* ts);

/* Called non-callable value */
cr_noret callerror(TState* ts, Value callee);

/* Invalid property access error */
cr_noret ipaerror(TState* ts, Value notinstance);

/* Global variable redefinition error */
cr_noret redefgerror(TState* ts, const char* gname);

/* Undefined global variable error */
cr_noret udgerror(TState* ts, const char* gname);

/* Assigning to variable defined as 'fixed' error */
cr_noret fixederror(TState* ts, const char* var);

/* 'nil' index error */
cr_noret nilidxerror(TState* ts);

/* Inheritance error */
cr_noret inheriterror(TState* ts, Value notclass);

/* ---------------------------------------------------------- */ // runtime errors


#endif
