#ifndef __SKOOMA_CONFIG_H__
#define __SKOOMA_CONFIG_H__

#define __STDC_LIMIT_MACROS

#include <limits.h>
#include <assert.h>

#ifdef __STDC_VERSION__
    #if __STDC_VERSION__ < 201112L
        #error "Minimum required C standard must be C11 (201112L)."
    #endif
#elif defined __cplusplus
    #if __cplusplus < 201103L
        #error "Minimum required C++ standard must be C++11 (201103L)."
    #endif
#else
    #error "Compiler not supported (no __STDC_VERSION__ or __cplusplus defined)."
#endif

#if !defined(__STDC_IEC_559__) || __DBL_DIG__ != 15 || __DBL_MANT_DIG__ != 53 ||         \
    __DBL_MAX_10_EXP__ != 308 || __DBL_MAX_EXP__ != 1024 ||                              \
    __DBL_MIN_10_EXP__ != -307 || __DBL_MIN_EXP__ != -1021

    #error "Compiler missing IEEE-754 double precision floating point!"
#endif

static_assert(sizeof(void*) == 8, "Size of 'void*' is not 8 bytes!");


/**
 *
 * Note: Assuming GCC, MSVC or clang are newer versions
 * because they are supporting C11 or C++11!
 *
 * GCC      >= 4.6
 * Clang    >= 3.1
 * MSVC     >= 16.8
 *
 **/


#if defined(__GNUC__)

    #define SK_PRECOMPUTED_GOTO

    #define force_inline   __always_inline
    #define likely(cond)   __glibc_likely(cond)
    #define unlikely(cond) __glibc_unlikely(cond)
    #define unused         __attribute__((unused))
    #define unreachable    __builtin_unreachable()

#elif defined(_MSC_VER) && !defined(__clang__)

    #define force_inline __force_inline
    #define unreachable  __assume(0)
    #define unused

    #ifndef __cplusplus
        #define inline         _inline
        #define likely(cond)   cond
        #define unlikely(cond) cond
    #elif _MSC_VER >= 1926
        #define likely(cond)   [[likely]] cond
        #define unlikely(cond) [[unlikely]] cond
    #else
        #define likely(cond)   cond
        #define unlikely(cond) cond
    #endif

    #define unused

#elif defined(__clang__)

    #define SK_PRECOMPUTED_GOTO

    #if __has_attribute(always_inline)
        #define force_inline __attribute__((always_inline))
    #else
        #define force_inline inline
    #endif

    #if __has_attribute(unused)
        #define unused __attribute__((unused))
    #else
        #define unused
    #endif

    #if __has_builtin(__builtin_expect)
        #define likely(cond)   __builtin_expect(cond, 1)
        #define unlikely(cond) __builtin_expect(cond, 0)
    #else
        #define likely(cond)   cond
        #define unlikely(cond) cond
    #endif

    #if __has_builtin(__builtin_unreachable)
        #define unreachable __builtin_unreachable()
    #else
        #define unreachable
    #endif

#else

    #define force_inline
    #define likely(cond)   cond
    #define unlikely(cond) cond
    #define unused
    #define unreachable                                                                  \
        #include<stdio.h> #include<stdlib.h> printf(                                     \
            "Unreachable code: %s:%d\n",                                                 \
            __FILE__,                                                                    \
            __LINE__);                                                                   \
        abort();

#endif

#define SK_INTERNAL(ret) static ret

/* Max stack size in bytes, default 1 MiB */
#define SK_STACK_MAX (1 << 20)

/* Max function call frames. */
#define SK_CALLFRAMES_MAX 1024

/* Allow NaN boxing of values. */
#define SK_NAN_BOX

/* For debug builds comment out 'defines' you dont want. */
#ifdef DEBUG

    /* Enable asserts */
    #define DEBUG_ASSERTIONS

    /* Print and disassemble bytecode */
    #define DEBUG_PRINT_CODE

    /* Trace VM stack */
    #define DEBUG_TRACE_EXECUTION

    /* Run garbage collection on each allocation */
    // #define DEBUG_STRESS_GC

    /* Log garbage collection */
    #define DEBUG_LOG_GC

#endif

#endif
