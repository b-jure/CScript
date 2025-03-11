
#define CS_LIB

#include <float.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include <stdlib.h>

#include "cscript.h"

#include "cauxlib.h"
#include "cslib.h"
#include "climits.h"


#if !defined(PI)
#define PI          (c_mathop(3.141592653589793238462643383279502884))
#endif


static cs_Unsigned int_abs(cs_Integer v) {
    cs_Integer const mask = v >> (sizeof(cs_Integer)*CHAR_BIT - 1);
    return c_castS2U((v + mask) ^ mask);
}


static int m_abs(cs_State *C) {
    if (cs_is_integer(C, 0)) {
        cs_Integer n = int_abs(cs_to_integer(C, 0));
        cs_push_integer(C, n);
    } else
        cs_push_number(C, c_mathop(fabs)(csL_check_number(C, 0)));
    return 1;
}


static int m_sin(cs_State *C) {
    cs_push_number(C, c_mathop(sin)(csL_check_number(C, 0)));
    return 1;
}


static int m_cos(cs_State *C) {
    cs_push_number(C, c_mathop(cos)(csL_check_number(C, 0)));
    return 1;
}


static int m_tan(cs_State *C) {
    cs_push_number(C, c_mathop(tan)(csL_check_number(C, 0)));
    return 1;
}


static int m_asin(cs_State *C) {
    cs_push_number(C, c_mathop(asin)(csL_check_number(C, 0)));
    return 1;
}


static int m_acos(cs_State *C) {
    cs_push_number(C, c_mathop(acos)(csL_check_number(C, 0)));
    return 1;
}


static int m_atan(cs_State *C) {
    cs_Number y = csL_check_number(C, 0);
    cs_Number x = csL_opt_number(C, 1, 1);
    cs_push_number(C, c_mathop(atan2)(y, x));
    return 1;
}


/* convert top to integer */
static int m_toint(cs_State *C) {
    int valid;
    cs_Integer n = cs_to_integerx(C, 0, &valid);
    if (c_likely(valid))
        cs_push_integer(C, n);
    else {
        csL_check_any(C, 0);
        csL_push_fail(C); /* value is not convertible to integer */
    }
    return 1;
}


static void push_num_or_int(cs_State *C, cs_Number d) {
    cs_Integer n;
    if (cs_number2integer(d, &n)) /* does 'd' fit in an integer? */
        cs_push_integer(C, n); /* result is integer */
    else
        cs_push_number(C, d); /* result is float */
}


/* round down */
static int m_floor(cs_State *C) {
    if (cs_is_integer(C, 0))
        cs_settop(C, 1); /* integer is its own floor */
    else {
        cs_Number d = c_mathop(floor)(csL_check_number(C, 0));
        push_num_or_int(C, d);
    }
    return 1;
}


/* round up */
static int m_ceil (cs_State *C) {
    if (cs_is_integer(C, 0))
        cs_settop(C, 1); /* integer is its own ceiling */
    else {
        cs_Number d = c_mathop(ceil)(csL_check_number(C, 0));
        push_num_or_int(C, d);
    }
    return 1;
}


static int m_fmod(cs_State *C) {
    if (cs_is_integer(C, 0) && cs_is_integer(C, 1)) {
        cs_Integer d = cs_to_integer(C, 1); /* denominator */
        if ((cs_Unsigned)d + 1u <= 1u) { /* special cases: -1 or 0 */
            csL_check_arg(C, d != 0, 1, "zero");
            cs_push_integer(C, 0); /* avoid overflow with 0x80000... / -1 */
        } else
            cs_push_integer(C, cs_to_integer(C, 0) % d);
    } else
        cs_push_number(C, c_mathop(fmod)(csL_check_number(C, 0),
                                         csL_check_number(C, 1)));
    return 1; /* return remainder */
}


/*
** Next function does not use 'modf', avoiding problems with 'double*'
** (which is not compatible with 'float*') when cs_Number is not
** 'double'.
*/
static int m_modf(cs_State *C) {
    if (cs_is_integer(C ,0)) {
        cs_settop(C, 1); /* number is its own integer part */
        cs_push_number(C, 0); /* no fractional part */
    } else {
        cs_Number n = csL_check_number(C, 0);
        /* integer part (rounds toward zero) */
        cs_Number ip = (n < 0) ? c_mathop(ceil)(n) : c_mathop(floor)(n);
        push_num_or_int(C, ip);
        /* fractional part (test needed for inf/-inf) */
        cs_push_number(C, (n == ip) ? c_mathop(0.0) : (n - ip));
    }
    return 2; /* return integer part and fractional part */
}


static int m_sqrt(cs_State *C) {
    cs_push_number(C, c_mathop(sqrt)(csL_check_number(C, 0)));
    return 1;
}


/* unsigned "less than" */
static int m_ult(cs_State *C) {
    cs_Integer a = csL_check_integer(C, 0);
    cs_Integer b = csL_check_integer(C, 1);
    cs_push_bool(C, c_castS2U(a) < c_castS2U(b));
    return 1;
}


static int m_log(cs_State *C) {
    cs_Number x = csL_check_number(C, 0);
    cs_Number res;
    if (cs_is_noneornil(C, 1))
        res = c_mathop(log)(x); /* natural log */
    else {
        cs_Number base = csL_check_number(C, 1);
        if (base == c_mathop(2.0))
            res = c_mathop(log2)(x); /* base-2 log */
        else {
            if (base == c_mathop(10.0))
                res = c_mathop(log10)(x); /* base-10 log */
            else /* otherwise use "logarithms change of base rule" */
                res = c_mathop(log)(x)/c_mathop(log)(base);
        }
    }
    cs_push_number(C, res);
    return 1;
}


/* base-e exponentiation */
static int m_exp(cs_State *C) {
    cs_push_number(C, c_mathop(exp)(csL_check_number(C, 0)));
    return 1;
}


/* angle from radians to degrees */
static int m_deg(cs_State *C) {
    cs_push_number(C, csL_check_number(C, 0) * (c_mathop(180.0) / PI));
    return 1;
}


/* angle from degrees to radians */
static int m_rad (cs_State *C) {
    cs_push_number(C, csL_check_number(C, 0) * (PI / c_mathop(180.0)));
    return 1;
}


static int m_min(cs_State *C) {
    int n = cs_getntop(C); /* number of arguments */
    int imin = 0; /* index of current minimum value */
    csL_check_arg(C, n >= 0, 0, "value expected");
    for (int i = 1; i < n; i++) {
        if (cs_compare(C, i, imin, CS_OPLT))
            imin = i;
    }
    cs_push(C, imin); /* push value at 'imin' index */
    return 1; /* return min */
}


static int m_max(cs_State *C) {
    int n = cs_getntop(C); /* number of arguments */
    int imax = 0; /* index of current maximum value */
    csL_check_arg(C, n >= 0, 0, "value expected");
    for (int i = 1; i < n; i++) {
        if (cs_compare(C, imax, i, CS_OPLT))
            imax = i;
    }
    cs_push(C, imax); /* push value at 'imax' index */
    return 1; /* return max */
}


static int m_type(cs_State *C) {
    if (cs_type(C, 0) == CS_TNUMBER)
        cs_push_string(C, (cs_is_integer(C, 0)) ? "integer" : "float");
    else {
        csL_check_any(C, 0);
        csL_push_fail(C);
    }
    return 1;
}



/*
** {==================================================================
** Pseudo-Random Number Generator (based on MT19937-64)
** ===================================================================
*/


/*
** Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
** All rights reserved.                          
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
**
**   1. Redistributions of source code must retain the above copyright
**      notice, this list of conditions and the following disclaimer.
**
**   2. Redistributions in binary form must reproduce the above copyright
**      notice, this list of conditions and the following disclaimer in the
**      documentation and/or other materials provided with the distribution.
**
**   3. The names of its contributors may not be used to endorse or promote 
**      products derived from this software without specific prior written 
**      permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER
** OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
** EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
** PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
** PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
** LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
** NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#undef Rand64
#undef SRand64
#undef RandF


#if ((ULONG_MAX >> 31) >> 31) >= 3

/* 'long' has at least 64 bits */
#define Rand64	        unsigned long
#define SRand64		long

#elif defined(LLONG_MAX)

/* there is a 'long long' type (which must have at least 64 bits) */
#define Rand64		unsigned long long
#define SRand64		long long

#elif ((CS_UNSIGNED_MAX >> 31) >> 31) >= 3

/* 'cs_Unsigned' has at least 64 bits */
#define Rand64		cs_Unsigned
#define SRand64		cs_Integer

#endif


#if defined(DBL_MIN)

/* 'double' has 64 bits */
#define RandF   double

#elif (sizeof(cs_Number) * CHAR_BIT) >= 64

/* 'cs_Number' has at least 64 bits */
#define RandF   cs_Number

#endif


#if !defined(Rand64)
#error Mersenne Twister implementation is missing 64-bit integer type
#endif

#if !defined(RandF)
#error Mersenne Twister implementation is missing 64-bit float type
#endif


/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */


static Rand64 mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */


/*
** Initializes mt[N] with a seed.
*/
static void init_genrand(Rand64 s) {
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] = (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti); 
        mt[mti] &= 0xffffffffUL;
    }
}


/*
** Initialize by an array with array-length.
** 'init_key' is the array for initializing keys.
** 'key_length' is its length.
*/
static void init_by_array(Rand64 init_key[], int key_length) {
    int i, j, k;
    init_genrand(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL;
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL;
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }
    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}


/* 
** Generates a random number on [0,0xffffffff] interval.
*/
static Rand64 genrand_int32(void) {
    static Rand64 mag01[2]={0x0UL, MATRIX_A};
    Rand64 y;
    /* mag01[x] = x * MATRIX_A  for x=0,1 */
    if (mti >= N) { /* generate N words at one time */
        int kk;
        if (mti == N+1) /* init_genrand() has not been called? */
            init_genrand(5489UL); /* use default initial seed */
        for (kk = 0; kk < N-M; kk++) {
            y = (mt[kk]&UPPER_MASK) | (mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (; kk < N-1; kk++) {
            y = (mt[kk]&UPPER_MASK) | (mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK) | (mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];
        mti = 0;
    }
    y = mt[mti++];
    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);
    return y;
}


/*
** Generates a random number on [0,0x7fffffff] interval.
*/
static SRand64 genrand_int31(void) {
    return (SRand64)(genrand_int32()>>1);
}


/*
** Generates a random number on [0,1] real-interval.
*/
static RandF genrand_float32_full(void) {
    return (RandF)(genrand_int32()*(1.0/4294967295.0/*(1.0/(2^32-1))*/)); 
}


/*
** Generates a random number on [0,1) real-interval.
*/
static RandF genrand_float32_notone(void) {
    return (RandF)(genrand_int32()*(1.0/4294967296.0/*(1.0/(2^32))*/));
}


static int m_rand(cs_State *C) {
    cs_push_integer(C, (cs_Integer)genrand_int32());
    return 1;
}


static int m_rands(cs_State *C) {
    cs_push_integer(C, (cs_Integer)genrand_int31());
    return 1;
}


static int m_randff(cs_State *C) {
    cs_push_number(C, (cs_Number)genrand_float32_full());
    return 1;
}


static int m_randf(cs_State *C) {
    cs_push_number(C, (cs_Number)genrand_float32_notone());
    return 1;
}


static const cs_Entry randfuncs[] = {
    {"rand", m_rand},
    {"rands", m_rands},
    {"randff", m_randff},
    {"randf", m_randf},
    {NULL, NULL}
};


/*
** Register the random functions and initialize their state.
*/
static void set_rand_funcs(cs_State *C) {
    init_genrand(5489UL); /* initialize with default seed */
    csL_setfuncs(C, randfuncs, 0);
}


/* }================================================================== */


const cs_Entry mathlib[] = {
    {"abs", m_abs},
    {"acos", m_acos},
    {"asin", m_asin},
    {"atan", m_atan},
    {"ceil", m_ceil},
    {"cos", m_cos},
    {"deg", m_deg},
    {"exp", m_exp},
    {"to_integer", m_toint},
    {"floor", m_floor},
    {"fmod", m_fmod},
    {"ult", m_ult},
    {"log", m_log},
    {"max", m_max},
    {"min", m_min},
    {"modf", m_modf},
    {"rad", m_rad},
    {"sin", m_sin},
    {"sqrt", m_sqrt},
    {"tan", m_tan},
    {"type", m_type},
    /* placeholders */
    {"random", NULL},
    {"random_seed", NULL},
    {"pi", NULL},
    {"huge", NULL},
    {"maxint", NULL},
    {"minint", NULL},
    {NULL, NULL}
};


CSMOD_API int csopen_math(cs_State *C) {
    csL_newlib(C, mathlib);
    cs_push_number(C, PI);
    cs_set_fieldstr(C, -2, "pi");
    cs_push_number(C, CS_HUGE_VAL);
    cs_set_fieldstr(C, -2, "huge");
    cs_push_integer(C, CS_INTEGER_MAX);
    cs_set_fieldstr(C, -2, "maxint");
    cs_push_integer(C, CS_INTEGER_MIN);
    cs_set_fieldstr(C, -2, "minint");
    set_rand_funcs(C);
    return 1;
}
