/*
** cobject.c
** Generic functions over CScript objects
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "climits.h"
#include "cobject.h"
#include "cvm.h"


int csO_ceillog2 (uint x) {
    static const cs_ubyte log_2[256] = {  /* log_2[i] = ceil(log2(i - 1)) */
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


/* number of bits in 'cs_Integer' */
#define INTBITS         cast_int((sizeof(cs_Integer)*8))


/* shift 'x', 'y' times, in case of overflow return 0 */
cs_Integer csO_shiftr(cs_Integer x, cs_Integer y) {
    if (y < 0) {
        if (y <= -INTBITS) return 0;
        return (x << y);
    } else {
        if (y >= INTBITS) return 0;
        return (x >> y);
    }
}


static cs_Number numarithm(cs_State *ts, cs_Number x, cs_Number y, int op) {
    switch(op) {
        case CS_OPADD: return csi_numadd(ts, x, y);
        case CS_OPSUB: return csi_numsub(ts, x, y);
        case CS_OPMUL: return csi_nummul(ts, x, y);
        case CS_OPDIV: return csi_numdiv(ts, x, y);
        case CS_OPMOD: return csV_modnum(ts, x, y);
        case CS_OPPOW: return csi_numpow(ts, x, y);
        case CS_OPUNM: return csi_nummul(ts, x, y);
        default: cs_assert(0); return 0.0;
    }
}


static cs_Integer intarithm(cs_State *ts, cs_Integer x, cs_Integer y, int op) {
    switch(op) {
        case CS_OPADD: return csi_intop(+, x, y);
        case CS_OPSUB: return csi_intop(-, x, y);
        case CS_OPMUL: return csi_intop(*, x, y);
        case CS_OPDIV: return csV_div(ts, x, y);
        case CS_OPMOD: return csV_modint(ts, x, y);
        case CS_OPPOW: return csi_intop(^, x, y);
        case CS_OPUNM: return csi_intop(-, 0, x);
        case CS_OPBSHL: return csO_shiftl(x, y);
        case CS_OPBSHR: return csO_shiftr(x, y);
        case CS_OPBNOT: return csi_intop(^, ~csi_castS2U(0), x);
        case CS_OPBAND: return csi_intop(&, x, y);
        case CS_OPBOR: return csi_intop(|, x, y);
        case CS_OPBXOR: return csi_intop(^, x, y);
        default: cs_assert(0); return 0;
    }
}


/* convert number 'n' to integer according to 'mode' */
int csO_n2i(cs_Number n, cs_Integer *i, N2IMode mode) {
    cs_Number floored = cs_floor(n);
    if (floored != n) {
        if (mode == N2IEXACT) return 0;
        else if (mode == N2ICEIL) floored++;
    }
    return cs_number2integer(n, i);
}


/* try to convert value to 'cs_Integer' */
int csO_tointeger(const TValue *o, cs_Integer *i, int mode) {
    if (ttisnum(o)) {
        return csO_n2i(fval(o), i, mode);
    } else if (ttisint(o)) {
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
int csO_arithmraw(cs_State *ts, const TValue *a, const TValue *b,
                  TValue *res, int op) {
    cs_Number n1, n2;
    switch (op) {
    case CS_OPBNOT: case CS_OPBXOR: case CS_OPBSHL:
    case CS_OPBSHR: case CS_OPBOR: case CS_OPBAND: {
        cs_Integer i1, i2;
        if (tointeger(a, &i1) && tointeger(b, &i2)) {
            setival(res, intarithm(ts, i1, i2, op));
            return 1;
        }
        return 0;
    }
    case CS_OPDIV: case CS_OPMOD: {
        if (tonumber(a, &n1) && tonumber(b, &n2)) {
            setfval(res, numarithm(ts, n1, n2, op));
            return 1;
        }
        return 0;
    }
    case CS_OPADD: case CS_OPSUB:
    case CS_OPMUL: case CS_OPUNM: {
        if (tonumber(a, &n1) && tonumber(b, &n2)) {
            setfval(res, numarithm(ts, n1, n2, op));
            return 1;
        }
        return 0;
    }
    default:
        if (ttisint(a) && ttisint(b)) {
            setival(res, intarithm(ts, ival(a), ival(b), op));
            return 1;
        } else if (tonumber(a, &n1) && tonumber(b, &n2)) {
            setfval(res, numarithm(ts, n1, n2, op));
            return 1;
        } else {
            return 0;
        }
    }
}
