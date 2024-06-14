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


#ifndef CRSTATICS_H
#define CRSTATICS_H


#include "crlimits.h"


typedef struct {
	const char *str;
	const cr_mem len;
} StaticString;


/* indices into static strings array */
typedef enum {
	/* keywords */
	SS_AND = 0, SS_BREAK, SS_CASE, SS_CONTINUE, SS_CLASS,
	SS_DEFAULT, SS_ELSE, SS_FALSE, SS_FOR, SS_FOREACH,
	SS_FN, SS_IF, SS_IN, SS_INHERITS, SS_NIL, SS_OR,
	/* Value types */
	SS_NUM, SS_STR, SS_BOOL, SS_INS,
	SS_FUNC, SS_CLS, SS_NAT, SS_UPVAL, SS_METHOD,
	/* Class overload-able method names. */
	SS_INIT, SS_TOSTRING, SS_GETIDX, SS_SETIDX, SS_GC, 
	SS_DEFER, SS_ADD, SS_SUB, SS_MUL, SS_DIV, SS_MOD, SS_POW,
	SS_NOT, SS_UMIN, SS_NE, SS_EQ, SS_LT, SS_LE, SS_GT, SS_GE,
	/* Operator strings */
	SS_OPADD, SS_OPSUB, SS_OPMUL, SS_OPDIV, SS_OPMOD, SS_OPEXP,
	SS_OPNOT, SS_OPNEG, SS_OPNE, SS_OPEQ, SS_OPLT, SS_OPLE,
	SS_OPGT, SS_OPGE, SS_OPAND, SS_OPOR, 
} sstag;


#define CR_SSNUM	(SS_OPOR + 1)


#define SS	ssstorage

/* storage (in 'crvm.h') */
CRI_DEC(const StaticString SS[CR_SSNUM]);


#endif
