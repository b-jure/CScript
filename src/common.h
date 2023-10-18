#ifndef __SKOOMA_COMMON_H__
#define __SKOOMA_COMMON_H__

#include "skconf.h"

#include <assert.h>
#include <memory.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint8_t  Byte;
typedef uint32_t UInt;
typedef int32_t  Int;

/*
 * 0 - compiling source code
 * 1 - VM is running
 */
extern Int runtime;

/* Bit manipulation--------------------------------------------------------- */
SK_INTERNAL(force_inline size_t) bit_mask(uint8_t x)
{
    return (x >= sizeof(size_t) * CHAR_BIT) ? 0xffffffffffffffff : (1UL << (x)) - 1;
}
// Convert bit into unsigned long integer
#define btoul(bit) (~((size_t)0) & (1UL << ((bit)-1)))
// Return bit at 'bit' (0 or 1) from 'x'.
#define BIT_CHECK(x, bit) ((size_t)(x) & ((size_t)1 << ((bit)-1)))
// Toggle 'bit' from 'x'
#define BIT_TOGGLE(x, bit, toggle) ((x) &= (toggle * 1) << ((bit)-1))
// Set 'bit' from 'x'
#define BIT_SET(x, bit) ((x) |= ((size_t)1 << ((bit)-1)))
// Clear 'bit' from 'x'
#define BIT_CLEAR(x, bit) ((x) &= ~((size_t)1 << ((bit)-1)))
// Generate uint with 'bits' all set to 1
#define MAXBITS(bits) (~((size_t)0) >> ((sizeof(size_t) * 8) - (size_t)(bits)))
// Wrapper around MAXBITS, uses 'bytes' instead
#define MAXBYTES(bytes) MAXBITS((bytes) * 8)
// Max unsigned 24-bit value
#define UINT24_MAX ((uint32_t)MAXBYTES(3))
/* ------------------------------------------------------------------------- */
//
//
//
/* Math--------------------------------------------------------------------- */

// Check if compiler supports IEEE 754 floating point standard
#if !defined(__STDC_IEC_559__) || __DBL_DIG__ != 15 || __DBL_MANT_DIG__ != 53 ||         \
    __DBL_MAX_10_EXP__ != 308 || __DBL_MAX_EXP__ != 1024 ||                              \
    __DBL_MIN_10_EXP__ != -307 || __DBL_MIN_EXP__ != -1021
    #error "Compiler missing IEEE 754 floating point!"
#endif

// Check if size of double and long match (C11)
static_assert(sizeof(double) == sizeof(long), "Size of 'double' and 'long' don't match!");

/* Check if double is positive/negative infinity */
SK_INTERNAL(force_inline bool) is_infinity(double dbl)
{
    long integer;
    memcpy(&integer, &dbl, sizeof(long));
    return (integer & 0x7FFFFFFFFFFFFFFF) == 0x7FF0000000000000;
}

/* Check if double is NaN */
SK_INTERNAL(force_inline bool) is_nan(double dbl)
{
    long integer;
    memcpy(&integer, &dbl, sizeof(long));
    return (integer & 0x7FFFFFFFFFFFFFFFL) > 0x7FF0000000000000L;
}

/* Return MAX */
#if defined(__GNUC__) || defined(__clang__)
    #define MAX(a, b)                                                                    \
        ({                                                                               \
            __typeof__(a) _a = (a);                                                      \
            __typeof__(b) _b = (b);                                                      \
            _a > _b ? _a : _b;                                                           \
        })

    /* Return MIN */
    #define MIN(a, b)                                                                    \
        ({                                                                               \
            __typeof__(a) _a = (a);                                                      \
            __typeof__(b) _b = (b);                                                      \
            _a > _b ? _b : _a;                                                           \
        })
#else
    #define MAX(a, b) ((a) > (b) ? (a) : (b))
    #define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif

/* ------------------------------------------------------------------------- */

#endif
