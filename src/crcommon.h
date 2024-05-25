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

#include "crbits.h"
#include "crlimits.h"
#include "cript.h"

#include <sys/types.h>


/* avoid warnings for unused variables */
#ifndef UNUSED
#define UNUSED(x) ((void)(x))
#endif

/* scope guard for use in macros */
#ifndef SGUARD
#define SGUARD(b) {b}
#endif


/* 
 * garbage collection flag (check mem.c -> incgc())
 * 0 - compiling source code
 * 1 - VM is running 
 */
extern volatile cr_ubyte runtime; // in 'vmachine.c'


/* print to stream 's' */
#define cr_print(s, str)       fprintf(s, str)
#define cr_printf(s, fmt, ...) fprintf(s, fmt, __VA_ARGS__)


/* cleanup 'VM' */
void cleanvm(VM **vmp);


/* Return MAX */
#if defined(__GNUC__)
#define MAX(a, b)                       \
	({                              \
		__typeof__(a) _a = (a); \
		__typeof__(b) _b = (b); \
		_a > _b ? _a : _b;      \
	})

/* Return MIN */
#define MIN(a, b)                       \
	({                              \
		__typeof__(a) _a = (a); \
		__typeof__(b) _b = (b); \
		_a > _b ? _b : _a;      \
	})
#else
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif


/* 
 * @t - data type
 * @e - expression
 * Cast @e as @t.
 */
#define cast(t, e) ((t)(e))


/* string literal length */
#define SLL(sl) (sizeof(sl) - 1)


/* indices into static strings array */
typedef enum {
	/* Value types */
	SS_NIL = 0,
	SS_NUM,
	SS_STR,
	SS_BOOL,
	SS_CLASS,
	SS_INS,
	SS_FUNC,
	SS_CLS,
	SS_NAT,
	SS_UPVAL,
	SS_METHOD,
	/* Boolean strings */
	SS_TRUE,
	SS_FALSE,
	/* Class overload-able method names. */
	SS_INIT,
	SS_TOSTRING,
	SS_GETIDX,
	SS_SETIDX,
	SS_HASH,
	SS_FREE,
	SS_ADD,
	SS_SUB,
	SS_MUL,
	SS_DIV,
	SS_MOD,
	SS_POW,
	SS_NOT,
	SS_UMIN,
	SS_NE,
	SS_EQ,
	SS_LT,
	SS_LE,
	SS_GT,
	SS_GE,
	/* Class special field names. */
	SS_DBG,
	/* Operator strings */
	SS_OPADD,
	SS_OPSUB,
	SS_OPMUL,
	SS_OPDIV,
	SS_OPMOD,
	SS_OPEXP,
	SS_OPNOT,
	SS_OPNEG,
	SS_OPNE,
	SS_OPEQ,
	SS_OPLT,
	SS_OPLE,
	SS_OPGT,
	SS_OPGE,
	SS_OPAND,
	SS_OPOR,
	/* Other statics */
	SS_UNKNOWN,
	SS_CSRC,
	SS_N,
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
