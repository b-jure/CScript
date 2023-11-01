#ifndef __SKOOMA_MATH_H__
#define __SKOOMA_MATH_H__

#include "common.h"

#include <math.h>
#include <stdio.h>

typedef struct {
    double   fract;
    uint64_t integer;
} Bitcast;

#define skfloor(dbl) floor(dbl)

#define skceil(dbl) ceil(dbl)

/* Check if double is positive/negative infinity */
static force_inline bool is_infinity(double dbl)
{
    union {
        uint64_t integer;
        double   dbl;
    } bcast;
    bcast.dbl = dbl;
    return (bcast.integer & 0x7fffffffffffffff) == 0x7fffffffffffffff;
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


#endif
