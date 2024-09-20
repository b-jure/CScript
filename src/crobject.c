/*
** $Name: crobject.c
** $Description: Generic functions over Cript objects that do not invoke errors.
** $Copyright: Copyright Notice in cript.h.
*/

#define CR_CORE

#include "crlimits.h"
#include "crobject.h"
#include "crvm.h"


static const char udataname[] = "userdata";

CRI_DEF const char *const crO_typenames[CRI_TOTALTYPES] = {
    "no value", "boolean", "number", udataname, "string",
    "function", "class", "instance", udataname, "nil",
    "thread", "upvalue"
};


/* hash 'cr_Number' */
uint crO_hashnum(cr_Number n) {
    cr_Integer ni;
    int exp;
    n = cr_mathop(frexp(n, &exp)) * -cast_num(INT_MIN);
    if (cr_likely(cr_number2integer(n, &ni))) {
        uint ui = cast_uint(exp) + cast_uint(ni);
        return (ui <= cast_uint(INT_MAX) ? ui : cast_int(~ui));
    }
    cr_assert(cr_numisnan(n) || cr_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
}


/* https://www.lua.org/source/5.4/lobject.c.html (~ line 35) */
int crO_ceillog2 (uint x) {
    static const cr_ubyte log_2[256] = {  /* log_2[i] = ceil(log2(i - 1)) */
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


/* number of bits in 'cr_Integer' */
#define INTBITS         cast_int((sizeof(cr_Integer)*8))


/* shift 'x', 'y' times, in case of overflow return 0 */
cr_Integer crO_shiftr(cr_Integer x, cr_Integer y) {
    if (y < 0) {
        if (y <= -INTBITS) return 0;
        return (x << y);
    } else {
        if (y >= INTBITS) return 0;
        return (x >> y);
    }
}


static cr_Number numarithm(cr_State *ts, cr_Number x, cr_Number y, int op) {
    switch(op) {
    case CR_OPADD: return cri_numadd(ts, x, y);
    case CR_OPSUB: return cri_numsub(ts, x, y);
    case CR_OPMUL: return cri_nummul(ts, x, y);
    case CR_OPDIV: return cri_numdiv(ts, x, y);
    case CR_OPMOD: return crV_modnum(ts, x, y);
    case CR_OPPOW: return cri_numpow(ts, x, y);
    case CR_OPNOT: return cri_nummul(ts, x, y);
    case CR_OPUMIN: return cri_nummul(ts, x, y);
    default: cr_unreachable(); return 0.0;
    }
}


static cr_Integer intarithm(cr_State *ts, cr_Integer x, cr_Integer y, int op) {
    switch(op) {
    case CR_OPADD: return cri_intop(+, x, y);
    case CR_OPSUB: return cri_intop(-, x, y);
    case CR_OPMUL: return cri_intop(*, x, y);
    case CR_OPDIV: return crV_div(ts, x, y);
    case CR_OPMOD: return crV_modint(ts, x, y);
    case CR_OPPOW: return cri_intop(^, x, y);
    case CR_OPNOT: return cri_numnot(ts, x);
    case CR_OPUMIN: return cri_intop(-, 0, x);
    case CR_OPBSHL: return crO_shiftl(x, y);
    case CR_OPBSHR: return crO_shiftr(x, y);
    case CR_OPBNOT: return cri_intop(^, ~cri_castS2U(0), x);
    case CR_OPBAND: return cri_intop(&, x, y);
    case CR_OPBOR: return cri_intop(|, x, y);
    case CR_OPBXOR: return cri_intop(^, x, y);
    default: cr_unreachable(); return 0;
    }
}


/* convert number 'n' to integer according to 'mode' */
int crO_n2i(cr_Number n, cr_Integer *i, N2IMode mode) {
    cr_Number floored = cr_floor(n);
    if (floored != n) {
        if (mode == CR_N2IEXACT) return 0;
        else if (mode == CR_N2ICEIL) floored++;
    }
    return cr_number2integer(n, i);
}


/* try to convert value to 'cr_Integer' */
int crO_tointeger(const TValue *v, cr_Integer *i, int mode) {
    if (ttisnum(v)) {
        return crO_n2i(fval(v), i, mode);
    } else if (ttisint(v)) {
        *i = ival(v);
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
int crO_arithmraw(cr_State *ts, const TValue *a, const TValue *b,
                  TValue *res, int op)
{
    cr_Number n1, n2;
    switch (op) {
    case CR_OPBNOT: case CR_OPBXOR: case CR_OPBSHL:
    case CR_OPBSHR: case CR_OPBOR: case CR_OPBAND: {
        cr_Integer i1, i2;
        if (tointeger(a, &i1) && tointeger(b, &i2)) {
            setival(res, intarithm(ts, i1, i2, op));
            return 1;
        }
        return 0;
    }
    case CR_OPDIV: case CR_OPMOD: {
        if (tonumber(a, &n1) && tonumber(b, &n2)) {
            setfval(res, numarithm(ts, n1, n2, op));
            return 1;
        }
        return 0;
    }
    case CR_OPADD: case CR_OPSUB: case CR_OPMUL:
    case CR_OPNOT: case CR_OPUMIN: {
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
