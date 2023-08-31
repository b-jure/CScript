#ifndef __SKOOMA_COMMON_H__
#define __SKOOMA_COMMON_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* Bit manipulation--------------------------------------------------------- */

/* Return bit at 'bit' (0 or 1) from 'x'. */
#define BIT_CHECK(x, bit) ((size_t)(x) & ((size_t)1 << (bit)))
// Set 'bit' from 'x'
#define BIT_SET(x, bit) ((x) |= ((size_t)1 << (bit)))
// Clear 'bit' from 'x'
#define BIT_CLEAR(x, bit) ((x) &= ~((size_t)1 << (bit)))
// Generate uint with 'bits' all set to 1
#define MAXBITS(bits) (~((size_t)0) >> ((sizeof(size_t) * 8) - 1 - (bits)))
// Wrapper around MAXBITS, uses 'bytes' instead
#define MAXBYTES(bytes) MAXBITS((bytes)*8)
/* ------------------------------------------------------------------------- */

/* Math--------------------------------------------------------------------- */

/* Check if double is positive/negative infinity */
#define IS_INFINITY(dbl)                                                       \
  ((*(long *)(&dbl) & 0x7FFFFFFFFFFFFFFF) == 0x7FF0000000000000)
/* Check if double is NaN */
#define IS_NAN(dbl)                                                            \
  ((*(uint64_t *)(&dbl) & 0x7FFFFFFFFFFFFFFFL) > 0x7FF0000000000000L)
/* ------------------------------------------------------------------------- */

/* Debug flag for debugging chunks. */
#define DEBUG_PRINT_CODE
/* Debug flag for printing VM stack. */
#define DEBUG_TRACE_EXECUTION

/* Check if we can use labels as values for precomputed goto/jmp table */
#if defined(__GNUC__) && __GNUC__ >= 2
#define THREADED_CODE
#endif

typedef uint8_t Byte;
typedef uint32_t UInt;

/* Compiler builtins (attributes)------------------------------------------- */

#if defined(__GLIBC__)
#define __force_inline __always_inline
#define __likely(cond) __glibc_likely(cond)
#define __unlikely(cond) __glibc_unlikely(cond)
#define __unused __attribute__((unused))
#define __unreachable __builtin_unreachable()
#elif defined(__clang__)
#define __force_inline __always_inline
#define __likely(cond) [[likely]] cond
#define __unlikely(cond) [[unlikely]] cond
#define __unused [[maybe_unused]]
#define __unreachable
#else
#define __force_inline inline
#define __likely(cond) cond
#define __unlikely(cond) cond
#define __unused
#define __unreachable
#endif

#define _force_inline __force_inline
#define _likely(cond) __likely(cond)
#define _unlikely(cond) __unlikely(cond)
#define _unused __unused
#define _unreachable __unreachable

#define SK_STATIC_INLINE(ret) static _force_inline ret
/* ------------------------------------------------------------------------- */

#endif
