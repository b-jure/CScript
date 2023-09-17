#ifndef __SKOOMA_CONFIG_H__
#define __SKOOMA_CONFIG_H__

#include <limits.h>

#define MIB(x) (x << 20)

/* Max stack size in bytes, default 1 MIB = ~ 1 MB. */
#define SK_STACK_MAX MIB(1)

/* Max function call frames. */
#define SK_CALLFRAMES_MAX 1024

/* Check if we can use labels as values for precomputed goto/jmp table */
#if defined(__GNUC__) && __GNUC__ >= 3
#define THREADED_CODE
#endif

/* Compiler builtins (attributes)------------------------------------------- */

#if defined(__GNUC__) && __GNUC__ >= 3
#define force_inline __always_inline
#define likely(cond) __glibc_likely(cond)
#define unlikely(cond) __glibc_unlikely(cond)
#define unused __attribute__((unused))
#define unreachable __builtin_unreachable()
#elif defined(__clang__)
#define force_inline __always_inline
#define likely(cond) [[likely]] cond
#define unlikely(cond) [[unlikely]] cond
#define unused [[maybe_unused]]
#define unreachable __builtin_unreachable()
#else
#define force_inline inline
#define likely(cond) cond
#define unlikely(cond) cond
#define unused
#define unreachable
#endif

#define SK_INTERNAL(ret) static ret
/* ------------------------------------------------------------------------- */

#endif
