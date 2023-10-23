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
 * garbage collection flag (check mem.c -> gc)
 * 0 - compiling source code
 * 1 - VM is running
 */
extern Int runtime;

/* Bits -------------------------------------------------------------------- */
SK_INTERNAL(force_inline size_t) bit_mask(uint8_t x)
{
    return (x >= sizeof(size_t) * CHAR_BIT) ? 0xffffffffffffffff : (1UL << (x)) - 1;
}
// Convert bit into unsigned long integer
#define btoul(bit) (~((size_t)0) & (1UL << ((bit)-1)))
// Return bit at 'bit' (0 or 1) from 'x'.
#define BIT_CHECK(x, bit) ((size_t)(x) & ((size_t)1 << ((bit)-1)))
// Toggle 'bit' from 'x'
#define BIT_TOGGLE(x, bit, toggle) ((x) |= (toggle * 1) << ((bit)-1))
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

// Check if size of double and long match
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

/* ----------------------------------------------------------------------- */



/* ----------- OPERATOR/CLASS INITIALIZER OVERLOADING ----------- */
typedef struct {
    const char* name;
    const Byte  len;
} Op;

#define OPS_INIT 0
#define OPS_ADD  1
#define OPS_SUB  2
#define OPS_MUL  3
#define OPS_DIV  4
#define OPS_REM  5
#define OPS_NEG  6
#define OPS_NOT  7

// @TODO: Implement operator overloading!
//        Hint: track overloaded classes during
//        compiling (their names) and emit new overloaded
//        instructions such as OP_OL_ADD, OP_OL_SUB, etc...
//        This way we avoid checking if we have an instance
//        and if it has overloaded operator during runtime.
//        Do not mark these for gc, but keep them as weak refs,
//        additionally remove them if they are not marked before sweeping.
//        Much more stuff I need to figure out before trying this...

#define OPSN (sizeof(ops) / sizeof(ops[0]))
static const Op ops[] = {
    {"__init__", sizeof("__init__") - 1},
    {"__add__",  sizeof("__add__") - 1 },
    {"__sub__",  sizeof("__sub__") - 1 },
    {"__mul__",  sizeof("__mul__") - 1 },
    {"__div__",  sizeof("__div__") - 1 },
    {"__rem__",  sizeof("__rem__") - 1 },
    {"__neg__",  sizeof("__neg__") - 1 },
    {"__not__",  sizeof("__not__") - 1 },
};

static_assert(OPSN == (OPS_NOT + 1), "ops table broken");
/* -------------------------------------------------------------- */


#endif
