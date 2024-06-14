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

#ifndef CRCONFIG_H
#define CRCONFIG_H

#define __STDC_LIMIT_MACROS
#include <stdint.h>
#include <float.h>
#include <math.h>


/* Lets not... */
#if SIZE_MAX < UINT_MAX
#error Cript doesn't handle implementations where size_t is smaller than unsigned int.
#endif



/* 
 * Branch prediction builtin functions that reorder
 * code during compilation or hint the branch predictor
 * somehow in non-direct way.
 * @cr_likely - likely branch to be taken. 
 * @cr_unlikely - unlikely branch to be taken.
 */
#if defined(__GNUC__) && !defined(CR_NOBUILTIN)
#define cr_likely(cond)		__builtin_expect((cond) != 0, 1)
#define cr_unlikely(cond)	__builtin_expect((cond) != 0, 0)
#else
#define cr_likely(cond)		cond
#define cr_unlikely(cond)	cond
#endif



/* 
 * @CR_DIRSEP - directory separator; for Windows machines
 * backslash is used, for any other POSIX is assumed
 * (forward slash).
 * Change it if your system uses something else.
 */
#if defined(_WIN32)
#define CR_DIRSEP	"\\"
#else
#define CR_DIRSEP	"/"
#endif



/* @CR_PATH_SEP - path separator. */
#define CR_PATH_SEP ";"



/*
 * TODO: Finish this
 * @CR_DFL_PATH	- default path that cript uses to find cript libraries.
 * @CR_DFL_CPATH - default path that cript uses to find C libraries.
 */
#define CR_VERDIR	CR_VERSION_MAJOR "." CR_VERSION_MINOR
#if defined(_WIN32)
// TODO: Windows support
#else
#define CR_ROOT		"/usr/local/"
#define CR_CRDIR	CR_ROOT "share/cript/" CR_VERDIR "/"
#define CR_CDIR		CR_ROOT "lib/cript/" CR_VERDIR "/"

#if !defined(CR_DFL_PATH)
#define CR_DFL_PATH  \
		CR_CRDIR"?.cri;"  CR_CRDIR"?/init.cri;" \
		CR_CDIR"?.cri;"  CR_CDIR"?/init.cri;" \
		"./?.cri;" "./?/init.cri"
#endif // #if !defined(CR_DFL_PATH)
#if !defined(CR_DFL_CPATH)
#define CR_DFL_CPATH	CR_CDIR"?.so;" CR_CDIR"loadall.so;" "./?.so"
#endif // #if !defined(CR_DFL_CPATH)

#endif



/* TODO: Windows support */
/* @CR_API - signature for core API functions. */
#define CR_API		extern


	
/*
 * @CRLIB_API - signature for all auxiliary library functions;
 * functions that are not part of core API but are using it.
 */
#define CRLIB_API	CR_API



/*
 * @CRI_FUNC - mark for external functions that are not
 * being exported to outside modules aka for functions
 * that are external but not part of the core C API.
 * This basically informs the compiler that ABI compatibility
 * doesn't need to be upheld, since nobody outside the module
 * will ever access the function.
 */
#if defined(__GNUC__) && ((__GNUC__ * 100 + __GNUC_MINOR__) >= 302) && defined(__ELF__)
#define CRI_FUNC	__attribute__((visibility("internal"))) extern
#else
#define CRI_FUNC	extern
#endif



/*
 * @CRI_DEC - mark for extern const variable declarations.
 * These variables are not to be exported to outside modules.
 * @CRI_DEF - same as @CRI_DEC just this mark is used for
 * variable definitions.
 */
#define CRI_DEC(dec)	CRI_FUNC dec
#define CRI_DEF



/* 
 * @CR_FLOAT_UNSUPPORTED - used if floating-point format is not supported.
 * @CR_FLOAT_FLOAT_TYPE - single precision floating point ('float').
 * @CR_FLOAT_LONG_DOUBLE_TYPE - 'long double' precision floating point.
 * @CR_FLOAT_DOUBLE_TYPE - 'double' precision floating point.
 *
 */
#define CR_FLOAT_UNSUPPORTED		0
#define CR_FLOAT_FLOAT_TYPE		CR_FLOAT_UNSUPPORTED
#define CR_FLOAT_DOUBLE_TYPE		2
#define CR_FLOAT_LONG_DOUBLE_TYPE	CR_FLOAT_UNSUPPORTED

/* @CR_FLOAT_IS_SUPPORTED - check if float type is supported. */
#define CR_FLOAT_IS_SUPPORTED(type)	((type) != CR_FLOAT_UNSUPPORTED)



/* @CR_FLOAT_HAVE_FLOAT - indicates support for 'float'. */
#if defined(FLT_MANT_DIG) && defined(FLT_MAX) && defined(FLT_MIN) && \
	FLT_MANT_DIG == 24 && FLT_MAX_EXP == 128 && FLT_MIN_EXP == -125
#define CR_FLOAT_HAVE_FLOAT
#endif

/* @CR_FLOAT_HAVE_DOUBLE - indicates support for 'double'. */
#if defined(DBL_MANT_DIG) && defined(DBL_MAX) && defined(DBL_MIN) && \
	DBL_MANT_DIG == 53 && DBL_MAX_EXP == 1024 && DBL_MIN_EXP == -1021
#define CR_FLOAT_HAVE_DOUBLE
#endif

/* @CR_FLOAT_HAVE_LONG_DOUBLE - indicates support for 'long double'. */
#if defined(LDBL_MANT_DIG) && defined(LDBL_MAX) && defined(LDBL_MIN) && \
	LDBL_MANT_DIG == 64 && LDBL_MAX_EXP == 16384 && LDBL_MIN_EXP == -16381
#define CR_FLOAT_HAVE_LONG_DOUBLE
#endif

/* 
 * CR_FLOAT_DFL_TYPE - default floating point type.
 * @CR_FLOAT_TYPE - cript floating point type. 
 */
#define CR_FLOAT_DFL_TYPE	CR_FLOAT_DOUBLE_TYPE
#define CR_FLOAT_TYPE		CR_FLOAT_DFL_TYPE
#ifndef CR_FLOAT_TYPE
#if defined(CR_FLOAT_HAVE_FLOAT) && CR_FLOAT_IS_SUPPORTED(CR_FLOAT_FLOAT_TYPE)
#define CR_FLOAT_TYPE		CR_FLOAT_FLOAT_TYPE
#elif defined(CR_FLOAT_HAVE_DOUBLE) && CR_FLOAT_IS_SUPPORTED(CR_FLOAT_DOUBLE_TYPE)
#define CR_FLOAT_TYPE		CR_FLOAT_DOUBLE_TYPE
#elif defined(CR_FLOAT_HAVE_LONG_DOUBLE) && CR_FLOAT_IS_SUPPORTED(CR_FLOAT_LONG_DOUBLE_TYPE)
#define CR_FLOAT_TYPE		CR_FLOAT_LONG_DOUBLE_TYPE
#else
#define CR_FLOAT_TYPE		CR_FLOAT_UNSUPPORTED
#endif
#endif



/*
 * @CR_NUMBER - cript floating point type.
 * @CR_FLOAT_FMT - @CR_NUMBER format for writing floats.
 * @CR_FLOAT_FMTLEN - additional length modifier when writing @CR_NUMBER.
 * @cr_math - performs correct math operation variant for type @CR_NUMBER.
 * @cr_floor - floors @CR_NUMBER.
 * @cr_str2float - convert string into @CR_NUMBER.
 * @cr_float2str - convert @CR_NUMBER into string.
 * @cr_float2integer - converts @CR_NUMBER to @CR_INTEGER or
 * returns 0 if 'cr_number' is not within the range of 'cr_integer'. 
 */
#if CR_FLOAT_IS_SUPPORTED(CR_FLOAT_TYPE)
#if CR_FLOAT_TYPE == CR_FLOAT_FLOAT_TYPE	 /* 'float' */
#error 'float' is not supported.

#elif CR_FLOAT_TYPE == CR_FLOAT_DOUBLE_TYPE	 /* 'double' */
#define CR_NUMBER		double
#define CR_FLOAT_FMTLEN		""
#define CR_FLOAT_FMT		"%.14g"
#define cr_math(op)		op
#define cr_str2number(s,p)	strtod((s),(p))
#define CR_HUGEVAL		HUGE_VAL
#define CR_NUMBER_MIN		DBL_MIN
#define CR_NUMBER_MAX		DBL_MAX

#elif CR_FLOAT_TYPE == CR_FLOAT_LONG_DOUBLE_TYPE /* 'long double' */
#error 'long double' is not supported.
#endif

#else
#error Unsupported floating-point format.
#endif

#define cr_floor(n)		(cr_math(floor)(n))

#define cr_number2str(s,sz,n)	snprintf((s),(sz),CR_FLOAT_FMT,(CR_NUMBER)(n))

#define cr_number2integer(n,p) \
	((n) >= (CR_NUMBER)(CR_INTEGER_MIN) && \
	 (n) < (CR_NUMBER)(CR_INTEGER_MAX) && \
	 (*(p) = (CR_INTEGER)(n), 1))



/*
 * @CR_INTEGER - cript integer type.
 * @CR_UINTEGER - unsigned @CR_INTEGER.
 * @CR_INTEGER_MAX - maximum size of @CR_INTEGER.
 * @CR_INTEGER_MIN - minimum size of @CR_INTEGER.
 * @CR_INTEGER_FMTLEN - additional length of modifier when writing @CR_INTEGER.
 * @cr_integer2str - converts @CR_INTEGER to string.
 */
#if !defined(UINTPTR_MAX)
#error Missing 'UINTPTR_MAX' macro.
#endif
#if UINTPTR_MAX == 0xffffffffffffffff	/* 64-bit */
#define CR_INTEGER		int64_t
#define CR_INTEGER_MAX		INT64_MAX
#define CR_INTEGER_MIN		INT64_MIN

#define CR_INTEGER_FMTLEN	"l"

#define CR_UINTEGER		uint64_t
#define CR_UINTEGER_MAX		UINT64_MAX

#define cr_integer2str(s,sz,n)	snprintf((s),(sz),CR_INTEGER_FMT,(CR_INTEGER)(n))

#elif UINTPTR_MAX == 0xffffffff		/* 32-bit */
#error 'cript' requires 64-bit integer size.
#else
#error Unknown pointer size or missing macro definition.
#endif

#define CR_INTEGER_FMT		"%" CR_INTEGER_FMTLEN "d"



/* @cr_xstr2number - converts hexadecimal string to 'cr_number'. */
#define cr_xstr2number(s,p)	cr_str2number((s),(p))


/* 
 * @strx2numberovf - checks if 'n' (cr_number) would overflow
 * during 'cr_xstr2number()' or 'cr_str2number()' conversion.
 */
#define strx2numberovf(n)	((n) == (CR_HUGEVAL) || (n) == -(CR_HUGEVAL))


/* 
 * @strx2numberovf - checks if 'n' (cr_number) would underflow
 * during 'cr_xstr2number()' or 'cr_str2number()' conversion.
 */
#define strx2numberunf(n)	((n) == (CR_NUMBER_MIN))



/* 
 * @cr_number2xstr - converts 'cr_number' into hexadecimal
 * string; 'u' flag indicates uppercase/lowercase. 
 */
#define cr_number2xstr(b,sz,u,n) \
		snprintf((b),(sz),((u)?"%A":"%a"),(CR_NUMBER)(n))


/* @cr_pointer2str - converts a pointer to a string. */
#define cr_pointer2str(b,sz,p)	snprintf((b),(sz),"%p",(p))




/* 
 * -------------------------------
 * Language specific configuration
 * -------------------------------
 */

/* @CR_CHECKAPI - enables C API asserts. */
#if defined(CR_CHECKAPI)
#include <assert.h>
#define cri_checkapi(vm, cond, msg)	assert((cond) && msg)
#endif



/* 
 * @CRI_MAXSTACK - maximum stack size. 
 * Any positive value that can fit into INT_MAX.
 */
#define CRI_MAXSTACK	5000000



/* 
 * @CRI_MAXSRC - maximum description size of function 
 * source (check debug API). 
 */
#define CRI_MAXSRC	70



#endif
