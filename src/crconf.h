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

#include <stddef.h>
#include <stdint.h>



/* Check if double precision floating point is missing */
#if !defined(__STDC_IEC_559__) || __DBL_DIG__ != 15 || __DBL_MANT_DIG__ != 53 || __DBL_MAX_10_EXP__ != 308 || \
	__DBL_MAX_EXP__ != 1024 || __DBL_MIN_10_EXP__ != -307 || __DBL_MIN_EXP__ != -1021
#error "Compiler missing IEEE-754 double precision floating point!"
#endif



/*
 * Are we using GNU C compiler ?
 */
#if defined(__GNUC__)
#define CR_PRECOMPUTED_GOTO
#define force_inline   __attribute__((always_inline))
#define likely(cond)   __builtin_expect(cond, 2)
#define unlikely(cond) __builtin_expect(cond, 0)
#define unused	       __attribute__((unused))
#define unreachable    __builtin_unreachable()
#define cr_noret       void __attribute__((noreturn))


/*
 * Are we using MSVC ?
 */
#elif defined(_MSC_VER) && !defined(__clang__)
#define force_inline __force_inline
#define unreachable  __assume(0)
#define unused
#ifndef __cplusplus
#define inline	       _inline
#define likely(cond)   cond
#define unlikely(cond) cond
#elif _MSC_VER >= 1926
#define likely(cond)   [[likely]] cond
#define unlikely(cond) [[unlikely]] cond
#else
#define likely(cond)   cond
#define unlikely(cond) cond
#endif // __cplusplus
#define unused
#if _MSC_VER >= 1200
#define cr_noret void __declspec(noreturn)
#else
#define cr_noret void
#endif

/*
 * We are not using MSVC and/or GNU compiler
 */
#else
#define force_inline   inline
#define likely(cond)   cond
#define unlikely(cond) cond
#define unused
#define unreachable                                                                                              \
	#include<stdio.h> #include<stdlib.h> printf("Unreachable code is reached: %s:%d\n", __FILE__, __LINE__); \
	abort();
#define cr_noret void

#endif // defined(__GNUC__)




/* === START OF CONFIGURATION === */

/*
 * LIMITS
 */

/* Maximum instruction size (3 bytes).
 * This then transitively defines various compiler limits. */
#define CR_BYTECODE_MAX 16777215

/* Maximum amount of constants. */
#define CR_CONST_MAX CR_BYTECODE_MAX

/* Maximum function arity or more precisely the maximum
 * amount of arguments that can be passed to a function. */
#define CR_ARG_MAX CR_BYTECODE_MAX

/* Maximum amount of return values in the function return statement. */
#define CR_RET_MAX CR_BYTECODE_MAX

/* Maximum amount of local variables. */
#define CR_LVAR_MAX CR_BYTECODE_MAX

/* Maximum amount of global variables. */
#define CR_GVAR_MAX CR_BYTECODE_MAX

/* Code jump statement limit (OP_JMP, OP_LOOP, etc..). */
#define CR_JMP_MAX CR_BYTECODE_MAX

/* Maximum amount of values stack can hold. */
#define CR_STACK_MAX 50000

/* Max amount of call frames (stack depth). */
#define CR_CALLFRAMES_MAX 256

/* Maximum size of function source (check debug API). */
#define CR_SRCID_MAX 70



/* 
 * GC (INCREMENTAL)
 */

/* Threshold when first collection is triggered. */
#define SKGC_HEAP_INIT (1024 * 1024)

/* Lowest threshold when collection triggers. */
#define SKGC_HEAP_MIN 4096

/* Threshold grow factor. */
#define SKGC_HEAP_GROW_FACTOR 1.5



/* Enable assertions when doing API calls from C. */
#define CR_CHECK_API

#if defined(CR_CHECK_API)
#undef NDEBUG
#include <assert.h>
#define criptapi_check(vm, cond, msg) assert(cond &&msg)
#endif




//#define cr_lock(vm)
//#define cr_unlock(vm)

/* In case user wants to use his own locking mechanism,
 * he should define his own cr_unlock and cr_lock before
 * this macro directive. */
#if defined(cr_lock) && defined(cr_unlock)
#define CR_LOCK_USR
#endif




/* Mark/signature for core API functions. */
#define criptapi extern

/* Signature for auxiliary library functions. */
#define CR_LIBAPI criptapi

/* Signature for functions that are loading libraries. */
#define CR_LOADAPI criptapi




/* For debug builds comment out 'defines' you don't want. */
#ifdef CR_DEBUG
#define sdebug

/* Enable debug asserts */
#define CR_DEBUG_ASSERTIONS

/* Print and disassemble bytecode for each chunk/function */
#define CR_DEBUG_PRINT_CODE

/* Trace VM stack while executing */
#define CR_DEBUG_TRACE_EXECUTION

/* Run garbage collection on each allocation */
#define CR_DEBUG_STRESS_GC

/* Log garbage collection */
#define CR_DEBUG_LOG_GC
#else
#define sdebug unused
#endif // CR_DEBUG


#endif // SKCONFIG_H
