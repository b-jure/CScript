/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef SKCONFIG_H
#define SKCONFIG_H

#include <stddef.h>
#include <stdint.h>



/* Check if double precision floating point is missing */
#if !defined(__STDC_IEC_559__) || __DBL_DIG__ != 15 || __DBL_MANT_DIG__ != 53 ||                   \
    __DBL_MAX_10_EXP__ != 308 || __DBL_MAX_EXP__ != 1024 || __DBL_MIN_10_EXP__ != -307 ||          \
    __DBL_MIN_EXP__ != -1021
#error "Compiler missing IEEE-754 double precision floating point!"
#endif



/*
 * Are we using GNU C compiler ?
 */
#if defined(__GNUC__)
#define SK_PRECOMPUTED_GOTO
#define force_inline __attribute__((always_inline))
#define likely(cond) __builtin_expect(cond, 2)
#define unlikely(cond) __builtin_expect(cond, 0)
#define unused __attribute__((unused))
#define unreachable __builtin_unreachable()
#define sk_noret void __attribute__((noreturn))


/*
 * Are we using MSVC ?
 */
#elif defined(_MSC_VER) && !defined(__clang__)
#define force_inline __force_inline
#define unreachable __assume(0)
#define unused
#ifndef __cplusplus
#define inline _inline
#define likely(cond) cond
#define unlikely(cond) cond
#elif _MSC_VER >= 1926
#define likely(cond) [[likely]] cond
#define unlikely(cond) [[unlikely]] cond
#else
#define likely(cond) cond
#define unlikely(cond) cond
#endif // __cplusplus
#define unused
#if _MSC_VER >= 1200
#define sk_noret void __declspec(noreturn)
#else
#define sk_noret void
#endif

/*
 * We are not using MSVC and/or GNU compiler
 */
#else
#define force_inline inline
#define likely(cond) cond
#define unlikely(cond) cond
#define unused
#define unreachable                                                                                \
    #include<stdio.h> #include<stdlib.h> printf(                                                   \
        "Unreachable code is reached: %s:%d\n",                                                    \
        __FILE__,                                                                                  \
        __LINE__);                                                                                 \
    abort();
#define sk_noret void

#endif // defined(__GNUC__)





/* === START OF CONFIGURATION === */


/* This is the limit of almost everything inside the
 * virtual machine, it is the maximum amount of variables,
 * constants, upvalues, local or global values you can
 * define.
 * It is also the limit of arguments a function
 * can take or maximum amount of values a function can
 * return.
 * Additionally it is the limit of OP_JMP family of
 * instructions, meaning you can't jump over more than
 * this amount of bytecode.
 *
 * The reason behind this is the size of the bytecode;
 * the longest instruction in Skooma is 3 bytes.
 *
 * Changing this to a lower value will enforce the new
 * limit on everything that was mentioned above.
 *
 * WARNING: setting this to a higher value than the
 * default one is not recommended, because in case
 * you reach this limit in any of the areas mentioned
 * above, it will probably break the VM. */
#define SK_BYTECODE_MAX 16777215

/* Maximum amount of constants.
 * By default it is set to 'SK_BYTECODE_MAX', can be changed
 * to a lower value if desired. */
#define SK_CONST_MAX SK_BYTECODE_MAX

/* Maximum amount of arguments a function can take.
 * By default it is set to 'SK_BYTECODE_MAX', can be changed
 * to a lower value if desired. */
#define SK_ARG_MAX SK_BYTECODE_MAX

/* Maximum amount of return values in the function return
 * statement.
 * By default it is set to 'SK_BYTECODE_MAX', can be changed
 * to a lower value if desired. */
#define SK_RET_MAX SK_BYTECODE_MAX

/* Maximum amount of local variables.
 * By default it is set to 'SK_BYTECODE_MAX', can be changed
 * to a lower value if desired. */
#define SK_LVAR_MAX SK_BYTECODE_MAX

/* Maximum amount of global variables.
 * By default it is set to 'SK_BYTECODE_MAX', can be changed
 * to a lower value if desired. */
#define SK_GVAR_MAX SK_BYTECODE_MAX

/* Code jump limit, used in conditional jumps or loops.
 * By default it is set to 'SK_BYTECODE_MAX', can be changed
 * to a lower value if desired. */
#define SK_JMP_MAX SK_BYTECODE_MAX




/* Max stack size in bytes, default set to 524 KiB.
 * In case the value (amount of memory) is set too low
 * in which case the stack wouldn't be able to store
 * a single Skooma value, then the size of the stack
 * is automatically set to '_FALLBACK_STACK' defined
 * in [sklimits.h].
 * In case the value (amount of memory) is set too
 * high, where the stack size is bigger than
 * 'SK_BYTECODE_MAX' limit, then the stack size will
 * be set to 'SK_BYTECODE_MAX' instead. */
#define SK_STACK_MAX 536574



/* Max function call frames.
 * This is basically size of a call stack (total
 * count of all the functions that are currently
 * being executed). */
#define SK_CALLFRAMES_MAX 256




/* Maximum size of function source (check debug API). */
#define SK_SRCID_MAX 70




/* Allow NaN boxing of values by default.
 * If this define is removed 'Value' will be
 * represented as a tagged union. */
#define SK_NAN_BOX




/* INCREMENTAL GC
 *
 * Default GC threshold when collection is triggered */
#define GC_HEAP_INIT (1024 * 1024)

/* Default lowest GC threshold */
#define GC_HEAP_MIN 4096

/* Default GC threshold grow factor */
#define GC_HEAP_GROW_FACTOR 1.5





/* Enables assertions when doing API calls from C.
 * Enabled by default, to disable remove this define
 * or comment it out. */
#define SK_CHECK_API

#if defined(SK_CHECK_API)
#undef NDEBUG
#include <assert.h>
#define skapi_check(vm, cond, msg) assert(cond&& msg)
#endif




/* In case user wants to use his own locking mechanism,
 * he should define his own sk_unlock and sk_lock. */
#if defined(sk_lock) && defined(sk_unlock)
#define SK_LOCK_USR
#endif




/* Mark/signature for core API functions. */
#define SK_API extern

/* Signature for auxiliary library functions. */
#define SK_LIBAPI SK_API

/* Signature for functions that are loading libraries. */
#define SK_LOADAPI SK_API




/* For debug builds comment out 'defines' you dont want. */
#ifdef SK_DEBUG
#define sdebug

/* Enable debug asserts */
#define SK_DEBUG_ASSERTIONS

/* Print and disassemble bytecode for each chunk/function */
#define SK_DEBUG_PRINT_CODE

/* Trace VM stack while executing */
#define SK_DEBUG_TRACE_EXECUTION

/* Run garbage collection on each allocation */
#define SK_DEBUG_STRESS_GC

/* Log garbage collection */
#define SK_DEBUG_LOG_GC
#else
#define sdebug unused
#endif



/* Allow operator overloading by default */
#define SK_OVERLOAD_OPS



#endif
