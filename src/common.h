#ifndef __SKOOMA_COMMON_H__
#define __SKOOMA_COMMON_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* Bit manipulation--------------------------------------------------------- */

/* Return bit at 'bit' (0 or 1) from 'x'. */
#define BIT_CHECK(x, bit) ((size_t)(x) & ((size_t)1 << (bit - 1)))
// Set 'bit' from 'x'
#define BIT_SET(x, bit) ((x) |= ((size_t)1 << (bit - 1)))
// Clear 'bit' from 'x'
#define BIT_CLEAR(x, bit) ((x) &= ~((size_t)1 << (bit - 1)))
// Generate uint with 'bits' all set to 1
#define MAXBITS(bits) (~((size_t)0) >> ((sizeof(size_t) * 8) - ((size_t)bits)))
// Wrapper around MAXBITS, uses 'bytes' instead
#define MAXBYTES(bytes) MAXBITS((bytes)*8)

#define UINT24_MAX MAXBYTES(3)
/* ------------------------------------------------------------------------- */

/* Math--------------------------------------------------------------------- */

/* Check if double is positive/negative infinity */
#define IS_INFINITY(dbl)                                                       \
  ((*(long *)(&dbl) & 0x7FFFFFFFFFFFFFFF) == 0x7FF0000000000000)
/* Check if double is NaN */
#define IS_NAN(dbl)                                                            \
  ((*(uint64_t *)(&dbl) & 0x7FFFFFFFFFFFFFFFL) > 0x7FF0000000000000L)
/* Return MAX */
#if defined(__GNUC__) || defined(__clang__)
#define MAX(a, b)                                                              \
  ({                                                                           \
    __typeof__(a) _a = (a);                                                    \
    __typeof__(b) _b = (b);                                                    \
    _a > _b ? _a : _b;                                                         \
  })
/* Return MIN */
#define MIN(a, b)                                                              \
  ({                                                                           \
    __typeof__(a) _a = (a);                                                    \
    __typeof__(b) _b = (b);                                                    \
    _a > _b ? _b : _a;                                                         \
  })
#else
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif

/* ------------------------------------------------------------------------- */

/* Debug flag for debugging chunks. */
#define DEBUG_PRINT_CODE
/* Debug flag for printing VM stack. */
#define DEBUG_TRACE_EXECUTION
/* Debug flag for assertions */
#define DEBUG_ASSERTIONS

/* Check if we can use labels as values for precomputed goto/jmp table */
#if defined(__GNUC__) && __GNUC__ >= 2
#define THREADED_CODE
#endif

typedef uint8_t Byte;
typedef uint32_t UInt;
typedef int32_t Int;

/* Compiler builtins (attributes)------------------------------------------- */

#if defined(__GLIBC__)
#define _force_inline __always_inline
#define _likely(cond) __glibc_likely(cond)
#define _unlikely(cond) __glibc_unlikely(cond)
#define _unused __attribute__((unused))
#define _unreachable __builtin_unreachable()
#elif defined(__clang__)
#define _force_inline __always_inline
#define _likely(cond) [[likely]] cond
#define _unlikely(cond) [[unlikely]] cond
#define _unused [[maybe_unused]]
#define _unreachable
#else
#define _force_inline inline
#define _likely(cond) cond
#define _unlikely(cond) cond
#define _unused
#define _unreachable
#endif

#define force_inline _force_inline
#define likely(cond) _likely(cond)
#define unlikely(cond) _unlikely(cond)
#define unused _unused
#define unreachable _unreachable

#define SK_INTERNAL(ret) static ret
/* ------------------------------------------------------------------------- */

#endif
