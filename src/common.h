#ifndef __SKOOMA_COMMON_H__
#define __SKOOMA_COMMON_H__

#include "skconf.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint8_t  Byte;
typedef uint32_t UInt;
typedef int32_t  Int;

// Enable NaN boxing
#define NAN_BOXING

#define UNUSED(x) (void)(x)

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
#define BIT_CHECK(x, bit) ((x >> ((bit)-1)) & ((size_t)1))
// Toggle 'bit' from 'x'
#define BIT_TOGGLE(x, bit, toggle) (x) ^= (-(toggle) ^ (x)) & ((size_t)1 << (bit - 1))
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


/* Check if double is positive/negative infinity */
static force_inline bool is_infinity(double dbl)
{
    union {
        uint64_t integer;
        double   dbl;
    } bitcast;
    bitcast.dbl = dbl;
    return (bitcast.integer & 0x7fffffffffffffff) == 0x7fffffffffffffff;
}

/* Check if double is NaN */
static force_inline bool is_nan(double dbl)
{
    union {
        uint64_t integer;
        double   dbl;
    } bitcast;
    bitcast.dbl = dbl;
    return (bitcast.integer & 0x7fffffffffffffffL) > 0x7ff0000000000000L;
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

#endif
