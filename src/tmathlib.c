/*
** tmathlib.c
** Standard mathematical library
** See Copyright Notice in tokudae.h
*/

#define tmathlib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <float.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include <stdlib.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"
#include "tokudaelimits.h"


#if !defined(PI)
#define PI          (t_mathop(3.141592653589793238462643383279502884))
#endif


#define toku_seed


static int m_abs(toku_State *T) {
    if (toku_is_integer(T, 0)) {
        toku_Integer n = toku_to_integer(T, 0);
        if (n < 0) n = t_castU2S(0u - t_castS2U(n));
        toku_push_integer(T, n);
    } else
        toku_push_number(T, t_mathop(fabs)(tokuL_check_number(T, 0)));
    return 1;
}


static int m_acos(toku_State *T) {
    toku_push_number(T, t_mathop(acos)(tokuL_check_number(T, 0)));
    return 1;
}


static int m_asin(toku_State *T) {
    toku_push_number(T, t_mathop(asin)(tokuL_check_number(T, 0)));
    return 1;
}


static int m_atan(toku_State *T) {
    toku_Number y = tokuL_check_number(T, 0);
    toku_Number x = tokuL_opt_number(T, 1, 1);
    toku_push_number(T, t_mathop(atan2)(y, x));
    return 1;
}


static void push_num_or_int(toku_State *T, toku_Number d) {
    toku_Integer n;
    if (toku_number2integer(d, &n)) /* does 'd' fit in an integer? */
        toku_push_integer(T, n); /* result is integer */
    else
        toku_push_number(T, d); /* result is float */
}


/* round up */
static int m_ceil (toku_State *T) {
    if (toku_is_integer(T, 0))
        toku_setntop(T, 1); /* integer is its own ceiling */
    else {
        toku_Number d = t_mathop(ceil)(tokuL_check_number(T, 0));
        push_num_or_int(T, d);
    }
    return 1;
}


static int m_cos(toku_State *T) {
    toku_push_number(T, t_mathop(cos)(tokuL_check_number(T, 0)));
    return 1;
}


/* angle from radians to degrees */
static int m_deg(toku_State *T) {
    toku_push_number(T, tokuL_check_number(T, 0) * (t_mathop(180.0) / PI));
    return 1;
}


/* base-e exponentiation */
static int m_exp(toku_State *T) {
    toku_push_number(T, t_mathop(exp)(tokuL_check_number(T, 0)));
    return 1;
}


/* round down */
static int m_floor(toku_State *T) {
    if (toku_is_integer(T, 0))
        toku_setntop(T, 1); /* integer is its own floor */
    else {
        toku_Number d = t_mathop(floor)(tokuL_check_number(T, 0));
        push_num_or_int(T, d);
    }
    return 1;
}


static int m_fmod(toku_State *T) {
    if (toku_is_integer(T, 0) && toku_is_integer(T, 1)) {
        toku_Integer d = toku_to_integer(T, 1); /* denominator */
        if ((toku_Unsigned)d + 1u <= 1u) { /* special cases: -1 or 0 */
            tokuL_check_arg(T, d != 0, 1, "zero");
            toku_push_integer(T, 0); /* avoid overflow with 0x80000... / -1 */
        } else
            toku_push_integer(T, toku_to_integer(T, 0) % d);
    } else
        toku_push_number(T, t_mathop(fmod)(tokuL_check_number(T, 0),
                                         tokuL_check_number(T, 1)));
    return 1; /* return remainder */
}


static int m_sin(toku_State *T) {
    toku_push_number(T, t_mathop(sin)(tokuL_check_number(T, 0)));
    return 1;
}


static int m_tan(toku_State *T) {
    toku_push_number(T, t_mathop(tan)(tokuL_check_number(T, 0)));
    return 1;
}


/* convert top to integer */
static int m_toint(toku_State *T) {
    int valid;
    toku_Integer n = toku_to_integerx(T, 0, &valid);
    if (t_likely(valid))
        toku_push_integer(T, n);
    else {
        tokuL_check_any(T, 0);
        tokuL_push_fail(T); /* value is not convertible to integer */
    }
    return 1;
}


static int m_sqrt(toku_State *T) {
    toku_push_number(T, t_mathop(sqrt)(tokuL_check_number(T, 0)));
    return 1;
}


/* unsigned "less than" */
static int m_ult(toku_State *T) {
    toku_Integer a = tokuL_check_integer(T, 0);
    toku_Integer b = tokuL_check_integer(T, 1);
    toku_push_bool(T, t_castS2U(a) < t_castS2U(b));
    return 1;
}


static int m_log(toku_State *T) {
    toku_Number x = tokuL_check_number(T, 0);
    toku_Number res;
    if (toku_is_noneornil(T, 1))
        res = t_mathop(log)(x); /* natural log */
    else {
        toku_Number base = tokuL_check_number(T, 1);
        if (base == t_mathop(2.0))
            res = t_mathop(log2)(x); /* base-2 log */
        else {
            if (base == t_mathop(10.0))
                res = t_mathop(log10)(x); /* base-10 log */
            else /* otherwise use "logarithms change of base rule" */
                res = t_mathop(log)(x)/t_mathop(log)(base);
        }
    }
    toku_push_number(T, res);
    return 1;
}


static int m_max(toku_State *T) {
    int n = toku_getntop(T); /* number of arguments */
    int imax = 0; /* index of current maximum value */
    tokuL_check_arg(T, n > 0, 0, "value expected");
    for (int i = 1; i < n; i++) {
        if (toku_compare(T, imax, i, TOKU_ORD_LT))
            imax = i;
    }
    toku_push(T, imax); /* push value at 'imax' index */
    return 1; /* return max */
}


static int m_min(toku_State *T) {
    int n = toku_getntop(T); /* number of arguments */
    int imin = 0; /* index of current minimum value */
    tokuL_check_arg(T, n > 0, 0, "value expected");
    for (int i = 1; i < n; i++) {
        if (toku_compare(T, i, imin, TOKU_ORD_LT))
            imin = i;
    }
    toku_push(T, imin); /* push value at 'imin' index */
    return 1; /* return min */
}


/*
** Next function does not use 'modf', avoiding problems with 'double*'
** (which is not compatible with 'float*') when toku_Number is not
** 'double'.
*/
static int m_modf(toku_State *T) {
    if (toku_is_integer(T ,0)) {
        toku_setntop(T, 1); /* number is its own integer part */
        toku_push_number(T, 0); /* no fractional part */
    } else {
        toku_Number n = tokuL_check_number(T, 0);
        /* integer part (rounds toward zero) */
        toku_Number ip = (n < 0) ? t_mathop(ceil)(n) : t_mathop(floor)(n);
        push_num_or_int(T, ip);
        /* fractional part (test needed for inf/-inf) */
        toku_push_number(T, (n == ip) ? t_mathop(0.0) : (n - ip));
    }
    return 2; /* return integer part and fractional part */
}


/* angle from degrees to radians */
static int m_rad (toku_State *T) {
    toku_push_number(T, tokuL_check_number(T, 0) * (PI / t_mathop(180.0)));
    return 1;
}


static int m_type(toku_State *T) {
    if (toku_type(T, 0) == TOKU_T_NUMBER)
        toku_push_string(T, (toku_is_integer(T, 0)) ? "integer" : "float");
    else {
        tokuL_check_any(T, 0);
        tokuL_push_fail(T);
    }
    return 1;
}



/*
** {==================================================================
** Pseudo-Random Number Generator (based on MT19937-64)
** ===================================================================
*/


/*
** This is a 64-bit version of Mersenne Twister pseudorandom number
** generator.
**
** Copyright (T) 2004, Makoto Matsumoto and Takuji Nishimura,
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

#elif ((TOKU_UNSIGNED_MAX >> 31) >> 31) >= 3

/* 'toku_Unsigned' has at least 64 bits */
#define Rand64		toku_Unsigned
#define SRand64		toku_Integer

#endif


#if !defined(Rand64)
#error Mersenne Twister implementation is missing 64-bit integer type
#endif


/* convert 'Rand64' to 'toku_Unsigned' */
#define R2U(x)      cast(toku_Unsigned, (x) & 0xffffffffffffffffu)

/* convert a 'toku_Unsigned' to a 'Rand64' */
#define U2R(x)	    cast(Rand64, (x))

/* convert unsigned constant to a 'Rand64' */
#define UK2R(k)     U2R(t_intatt(k))


#if defined(DBL_MIN)
#define RandF       double
#endif


#if !defined(RandF)
#error Mersenne Twister implementation is missing double precision float type
#endif


/* convert 'RandF' to 'toku_Number' */
#define Rf2N(x)     cast(toku_Number, x)



#define NN          312
#define MM          156
#define MATRIX_A    UK2R(0xB5026F5AA96619E9U)
#define UM          UK2R(0xFFFFFFFF80000000U) /* most significant 33 bits */
#define LM          UK2R(0x7FFFFFFFU) /* least significant 31 bits */


/* Mersenne Twister algo-state */
typedef struct MT19937 {
    Rand64 mt[NN]; /* the array for the state vector */
    int mti; /* mti == NN+1 means mt[NN] is not initialized */
} MT19937;


/* initializes context with a seed */
static void init_ctx_seed(MT19937 *ctx, Rand64 seed) {
    ctx->mt[0] = seed;
    for (ctx->mti=1; ctx->mti<NN; ctx->mti++) 
        ctx->mt[ctx->mti] =
            UK2R(6364136223846793005U) *
            (ctx->mt[ctx->mti-1] ^ (ctx->mt[ctx->mti-1] >> 62))
            + U2R(cast_uint(ctx->mti));
}


/*
** Initialize context by an array.
** 'key' is the array for initializing keys and 'klen' is its length.
*/
static void init_ctx_array(MT19937 *ctx, Rand64 key[], Rand64 klen) {
    Rand64 i = 1, j = 0;
    Rand64 k = (NN>klen ? NN : klen);
    init_ctx_seed(ctx, UK2R(19650218U));
    for (; k; k--) {
        ctx->mt[i] = (ctx->mt[i] ^ ((ctx->mt[i-1] ^ (ctx->mt[i-1] >> 62))
                     * UK2R(3935559000370003845U))) + key[j] + j; /* non linear */
        i++; j++;
        if (i >= NN) { ctx->mt[0] = ctx->mt[NN-1]; i = 1; }
        if (j >= klen) j = 0;
    }
    for (k=NN-1; k; k--) {
        ctx->mt[i] = (ctx->mt[i] ^ ((ctx->mt[i-1] ^ (ctx->mt[i-1] >> 62))
                     * UK2R(2862933555777941757U))) - i; /* non linear */
        i++;
        if (i >= NN) { ctx->mt[0] = ctx->mt[NN-1]; i = 1; }
    }
    ctx->mt[0] = UK2R(1U) << 63; /* MSB is 1; assuring non-zero initial array */ 
}


/* default initialization */
static void init_ctx_default(toku_State *T, MT19937 *ctx) {
    init_ctx_seed(ctx, tokuL_makeseed(T));
}


/* generates a random number on [0, 2^64-1] interval */
static Rand64 genrand_integer(toku_State *T, MT19937 *ctx) {
    static Rand64 mag01[2]={0ULL, MATRIX_A};
    Rand64 x;
    if (ctx->mti >= NN) { /* generate NN words at one time */
        int i;
        if (ctx->mti == NN+1)  /* 'init_ctx_seed' has not been called? */
            init_ctx_default(T, ctx); /* use default initialization */
        for (i = 0; i < NN-MM; i++) {
            x = (ctx->mt[i]&UM) | (ctx->mt[i+1]&LM);
            ctx->mt[i] = ctx->mt[i+MM] ^ (x>>1) ^ mag01[cast_int(x&UK2R(1U))];
        }
        for (; i < NN-1; i++) {
            x = (ctx->mt[i]&UM) | (ctx->mt[i+1]&LM);
            ctx->mt[i] = ctx->mt[i+(MM-NN)] ^ (x>>1) ^ mag01[cast_int(x&UK2R(1U))];
        }
        x = (ctx->mt[NN-1]&UM) | (ctx->mt[0]&LM);
        ctx->mt[NN-1] = ctx->mt[MM-1] ^ (x>>1) ^ mag01[cast_int(x&UK2R(1U))];
        ctx->mti = 0;
    }
    x = ctx->mt[ctx->mti++];
    x ^= (x >> 29) & UK2R(0x5555555555555555U);
    x ^= (x << 17) & UK2R(0x71D67FFFEDA60000U);
    x ^= (x << 37) & UK2R(0xFFF7EEE000000000U);
    x ^= (x >> 43);
    return x;
}


/* generates a random number on (0,1) real-interval */
static RandF genrand_float(toku_State *T, MT19937 *ctx) {
    return (cast(RandF, (genrand_integer(T, ctx)>>12)) + 0.5)
            * (1.0 / 4503599627370496.0);
}


/* maximum number of elements in the seed array */
#define SEEDELEMS       1000

typedef struct SeedArray {
    Rand64 seed[SEEDELEMS]; /* seed elements */
    t_ushort i; /* next element position in 'seed' */
    t_ushort n; /* total number of elements in 'seed' */
} SeedArray;


/* adds seed element to 'SeedArray' */
static void add_seed_elem(toku_State *T, SeedArray *sa) {
    toku_Unsigned seed = pointer2uint(toku_to_pointer(T, -1));
    if (seed == 0) { /* no pointer? */
        toku_Unsigned ub = tokuL_makeseed(T);
        toku_Unsigned lb = tokuL_makeseed(T);
        seed = (ub << 31)|lb;
    }
    if (sa->i >= sizeof(sa->seed)/sizeof(sa->seed[0]))
        sa->i = 0; /* wrap */
    else
        sa->n++; /* new seed element */
    sa->seed[sa->i++] = U2R(seed);
    toku_pop(T, 1); /* remove seed value */
}


static int m_srand(toku_State *T) {
    SeedArray sa = {0};
    MT19937 *ctx = cast(MT19937 *, toku_to_userdata(T, toku_upvalueindex(0)));
    int t = toku_type(T, 0);
    if (t != TOKU_T_NONE) { /* have at least one argument? */
        if (t == TOKU_T_NUMBER) { /* seed with integer? */
            toku_Integer n = tokuL_check_integer(T, 0);
            sa.seed[0] = U2R(t_castS2U(n));
            sa.n = 1;
        } else if (t == TOKU_T_LIST) { /* seed with array values? */
            int len = cast_int(toku_len(T, 0));
            int i = toku_find_index(T, 0, 0, 0, len);
            while (i >= 0) {
                toku_get_index(T, 0, i);
                add_seed_elem(T, &sa);
                i = toku_find_index(T, 0, 0,  i+1, len);
            }
        } else if (t == TOKU_T_TABLE) { /* seed with table values */
            toku_push_nil(T);
            while (toku_nextfield(T, 0))
                add_seed_elem(T, &sa);
        } else /* invalid argument type */
            tokuL_error_type(T, 0, "number, list or a table");
    }
    if (sa.n == 0) /* no seed values? */
        init_ctx_default(T, ctx); /* default initialization */
    else if (sa.n == 1) /* single seed value? */
        init_ctx_seed(ctx, sa.seed[0]); /* use it as seed */
    else /* multiple seed values */
        init_ctx_array(ctx, sa.seed, sa.n); /* use all of them to seed */
    return 0;
}


/* project random number into the [0..n] interval */
static toku_Unsigned project(toku_State *T, MT19937 *ctx, toku_Unsigned ran,
                                                      toku_Unsigned n) {
    if ((n & (n-1)) == 0) /* 'n' is power of 2? */
        return ran & n;
    else {
        toku_Unsigned lim = n;
        /* Computes the smallest (2^b - 1) not smaller than 'n'.
        ** It works by copying the highest bit set in 'n' to
        ** all of the lower bits. */
        lim |= (lim >> 1);
        lim |= (lim >> 2);
        lim |= (lim >> 4);
        lim |= (lim >> 8);
        lim |= (lim >> 16);
#if (TOKU_UNSIGNED_MAX >> 31) >= 3
        lim |= (lim >> 32); /* type of 'lim' has more than 32 bits */
#endif
        toku_assert((lim & (lim+1)) == 0 /* 'lim + 1' is a power of 2, */
                && lim >= n /* not smaller than 'n', */
                && (lim >> 1) < n); /* and it is the smallest one */
        while ((ran &= lim) > n) /* project 'ran' into [0..lim] */
            ran = R2U(genrand_integer(T, ctx)); /* not in [0..n]? try again */
        return ran;
    }
}


static int m_rand(toku_State *T) {
    MT19937 *ctx = toku_to_userdata(T, toku_upvalueindex(0));
    Rand64 ran = genrand_integer(T, ctx);
    toku_Integer low, up;
    toku_Unsigned p;
    switch (toku_getntop(T)) {
        case 0: {
            toku_push_integer(T, t_castU2S(R2U(ran)));
            return 1;
        }
        case 1: { /* upper limit */
            low = 1;
            up = tokuL_check_integer(T, 0);
            break;
        }
        case 2: { /* lower and upper limit */
            low = tokuL_check_integer(T, 0);
            up = tokuL_check_integer(T, 1);
            break;
        }
        default: {
            return tokuL_error(T, "invalid number of arguments");
        }
    }
    tokuL_check_arg(T, low <= up, 0, "interval is empty");
    /* project random integer into the interval [low, up] */
    p = project(T, ctx, ran, t_castS2U(up - low));
    toku_push_integer(T, t_castU2S(p) + low);
    return 1;
}


static int m_randf(toku_State *T) {
    MT19937 *ctx = toku_to_userdata(T, toku_upvalueindex(0));
    toku_push_number(T, Rf2N(genrand_float(T, ctx)));
    return 1;
}


static const tokuL_Entry randfuncs[] = {
    {"srand", m_srand},
    {"rand", m_rand},
    {"randf", m_randf},
    {NULL, NULL}
};


/*
** Register the random functions and initialize their state.
*/
static void set_rand_funcs(toku_State *T) {
    MT19937 *ctx = toku_push_userdata(T, sizeof(*ctx), 0);
    init_ctx_default(T, ctx);
    tokuL_set_funcs(T, randfuncs, 1);
}

/* }================================================================== */


const tokuL_Entry mathlib[] = {
    {"abs", m_abs},
    {"acos", m_acos},
    {"asin", m_asin},
    {"atan", m_atan},
    {"ceil", m_ceil},
    {"cos", m_cos},
    {"deg", m_deg},
    {"exp", m_exp},
    {"toint", m_toint},
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
    {"srand", NULL},
    {"rand", NULL},
    {"randf", NULL},
    {"pi", NULL},
    {"huge", NULL},
    {"maxint", NULL},
    {"minint", NULL},
    {NULL, NULL}
};


TOKUMOD_API int tokuopen_math(toku_State *T) {
    tokuL_push_lib(T, mathlib);
    toku_push_number(T, PI);
    toku_set_field_str(T, -2, "pi");
    toku_push_number(T, TOKU_HUGE_VAL);
    toku_set_field_str(T, -2, "huge");
    toku_push_integer(T, TOKU_INTEGER_MAX);
    toku_set_field_str(T, -2, "maxint");
    toku_push_integer(T, TOKU_INTEGER_MIN);
    toku_set_field_str(T, -2, "minint");
    set_rand_funcs(T);
    return 1;
}
