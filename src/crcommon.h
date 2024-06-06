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

#ifndef CRCOMMON_H
#define CRCOMMON_H

#include "cript.h"
#include "crbits.h"
#include "crlimits.h"


/* 
 * garbage collection flag (check mem.c -> incgc())
 * 0 - compiling source code
 * 1 - VM is running 
 */
extern volatile cr_ubyte runtime; // in 'vmachine.c'


/* cleanup 'VM' */
void cleanvm(VM **vmp);

/* indices into static strings array */
typedef enum {
	/* Value types */
	SS_NIL = 0, SS_NUM, SS_STR, SS_BOOL, SS_CLASS,
	SS_INS, SS_FUNC, SS_CLS, SS_NAT, SS_UPVAL, SS_METHOD,
	/* Boolean strings */
	SS_TRUE, SS_FALSE,
	/* Class overload-able method names. */
	SS_INIT, SS_TOSTRING, SS_GETIDX, SS_SETIDX, SS_HASH,
	SS_FREE, SS_ADD, SS_SUB, SS_MUL, SS_DIV, SS_MOD, SS_POW,
	SS_NOT, SS_UMIN, SS_NE, SS_EQ, SS_LT, SS_LE, SS_GT, SS_GE,
	/* Class special field names. */
	SS_DBG,
	/* Operator strings */
	SS_OPADD, SS_OPSUB, SS_OPMUL, SS_OPDIV, SS_OPMOD, SS_OPEXP,
	SS_OPNOT, SS_OPNEG, SS_OPNE, SS_OPEQ, SS_OPLT, SS_OPLE,
	SS_OPGT, SS_OPGE, SS_OPAND, SS_OPOR,
	/* Other statics */
	SS_UNKNOWN, SS_CSRC, SS_N,
} cr_ss;

/* string literal + compile time lenght == 'StaticString' */
typedef struct {
	const char *str;
	const cr_mem len;
} StaticString;

/* this array holds all static (preallocated) strings */
extern const StaticString static_strings[SS_N];

#define SS static_strings

#endif
