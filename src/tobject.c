/*
** tobject.c
** Generic functions over Tokudae objects
** See Copyright Notice in tokudae.h
*/

#define tobject_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tokudaelimits.h"
#include "tobject.h"
#include "tvm.h"


int tokuO_ceillog2 (t_uint x) {
    static const t_ubyte log_2[256] = {  /* log_2[i] = ceil(log2(i - 1)) */
        0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    };
    int l = 0;
    x--;
    while (x >= 256) { l += 8; x >>= 8; }
    return l + log_2[x];
}


/* number of bits in 'toku_Integer' */
#define INTBITS         cast_int((sizeof(toku_Integer) * CHAR_BIT))


/* shift 'x', 'y' times, to the left; in case of overflow return 0 */
toku_Integer tokuO_shiftl(toku_Integer x, toku_Integer y) {
    if (y < 0) { /* shift right? */
        if (y <= -INTBITS) return 0; /* overflow */
        return t_castU2S(t_castS2U(x) >> t_castS2U(-y));
    } else { /* shift left */
        if (y >= INTBITS) return 0; /* overflow */
        return t_castU2S(t_castS2U(x) << t_castS2U(y));
    }
}


static toku_Number numarithm(toku_State *T, toku_Number x, toku_Number y, int op) {
    switch(op) {
        case TOKU_OP_ADD: return t_numadd(C, x, y);
        case TOKU_OP_SUB: return t_numsub(C, x, y);
        case TOKU_OP_MUL: return t_nummul(C, x, y);
        case TOKU_OP_DIV: return t_numdiv(C, x, y);
        case TOKU_OP_IDIV: return t_numidiv(C, x, y);
        case TOKU_OP_MOD: return tokuV_modf(C, x, y);
        case TOKU_OP_POW: return t_numpow(C, x, y);
        case TOKU_OP_UNM: return t_numunm(C, x);
        default: toku_assert(0); return 0.0;
    }
}


static toku_Integer intarithm(toku_State *T, toku_Integer x, toku_Integer y, int op) {
    switch(op) {
        case TOKU_OP_ADD: return intop(+, x, y);
        case TOKU_OP_SUB: return intop(-, x, y);
        case TOKU_OP_MUL: return intop(*, x, y);
        case TOKU_OP_IDIV: return tokuV_divi(C, x, y);
        case TOKU_OP_MOD: return tokuV_modi(C, x, y);
        case TOKU_OP_UNM: return intop(-, 0, x);
        case TOKU_OP_BSHL: return tokuO_shiftl(x, y);
        case TOKU_OP_BSHR: return tokuO_shiftr(x, y);
        case TOKU_OP_BNOT: return intop(^, ~t_castS2U(0), x);
        case TOKU_OP_BAND: return intop(&, x, y);
        case TOKU_OP_BOR: return intop(|, x, y);
        case TOKU_OP_BXOR: return intop(^, x, y);
        default: toku_assert(0); return 0;
    }
}


/* convert number 'n' to integer according to 'mode' */
int tokuO_n2i(toku_Number n, toku_Integer *i, N2IMode mode) {
    toku_Number floored = t_floor(n);
    if (floored != n) {
        if (mode == N2IEQ) return 0;
        else if (mode == N2ICEIL) floored++;
    }
    return toku_number2integer(n, i);
}


/* try to convert value to 'toku_Integer' */
int tokuO_tointeger(const TValue *o, toku_Integer *i, int mode) {
    if (ttisflt(o))
        return tokuO_n2i(fval(o), i, mode);
    else if (ttisint(o)) {
        *i = ival(o);
        return 1;
    }
    return 0;
}


/*
** Perform raw arithmetic operations on numbers, what this means
** is that no vtable methods will be invoked and the operation
** itself can't invoke runtime error, if the operation can't be
** done then return 0.
*/
int tokuO_arithmraw(toku_State *T, const TValue *a, const TValue *b,
                  TValue *res, int op) {
    switch (op) {
        case TOKU_OP_BNOT: case TOKU_OP_BXOR: case TOKU_OP_BSHL:
        case TOKU_OP_BSHR: case TOKU_OP_BOR: case TOKU_OP_BAND: { /* for integers */
            toku_Integer i1, i2;
            if (tointeger(a, &i1) && tointeger(b, &i2)) {
                setival(res, intarithm(C, i1, i2, op));
                return 1;
            }
            return 0; /* fail */
        }
        case TOKU_OP_DIV: case TOKU_OP_POW: { /* for floats */
            toku_Number n1, n2;
            if (tonumber(a, n1) && tonumber(b, n2)) {
                setfval(res, numarithm(C, n1, n2, op));
                return 1;
            }
            return 0; /* fail */
        }
        default: { /* other operations */
            toku_Number n1, n2;
            if (ttisint(a) && ttisint(b)) {
                setival(res, intarithm(C, ival(a), ival(b), op));
                return 1;
            } else if (tonumber(a, n1) && tonumber(b, n2)) {
                setfval(res, numarithm(C, n1, n2, op));
                return 1;
            } else
                return 0; /* fail */
        }
    }
}
