/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crlimits.h"
#include "crdebug.h"
#include "crmeta.h"
#include "crstring.h"
#include "crvalue.h"
#include "crstate.h"



static const char udataname[] = "userdata";

CRI_DEF const char *const cr_value_typenames[CR_TOTALTYPES] = {
	"no value", "boolean", "number", udataname, "string",
	"function", "class", "instance", udataname, "nil",
	"thread", "upvalue"
};


/* hash 'cr_number' */
unsigned int cr_hash_number(cr_number n)
{
	unsigned int ui;
	cr_integer ni;
	int exp;

	n = cr_mathop(frexp(n, &exp)) * -cast_num(INT_MIN);
	if (cr_likely(cr_number2integer(n,&ni))) {
		ui = cast_uint(exp) + cast_uint(ni);
		return (ui <= cast_uint(INT_MAX) ? ui : cast_int(~ui));
	}
	cr_assert(cr_numisnan(n) || cr_mathop(fabs)(n) == cast_num(HUGE_VAL));
	return 0;
}


/* https://www.lua.org/source/5.4/lobject.c.html (~ line 35) */
int cr_ve_ceillog2 (unsigned int x)
{
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
	while (x >= 256) {
		l += 8;
		x >>= 8;
	}
	return l + log_2[x];
}


/*
 * Integer division; handles division by 0 and possible
 * overflow if 'y' == '-1' and 'x' == CR_INTEGER_MIN.
 */
cr_integer cr_value_div(cr_State *ts, cr_integer x, cr_integer y)
{
	if (cr_unlikely(cri_castS2U(y) + 1 <= 1)) { /* y == '0' or '-1' */
		if (y == 0)
			cr_debug_runerror(ts, "division by 0");
		return cri_intop(-, 0, x);
	}
	return (x / y);
}


/*
 * Integer modulus; handles modulo by 0 and overflow
 * as explained in 'cr_value_div()'.
 */
cr_integer cr_value_modint(cr_State *ts, cr_integer x, cr_integer y)
{
	cr_integer r;

	if (cr_unlikely(cri_castS2U(y) + 1 <= 1)) {
		if (y == 0)
			cr_debug_runerror(ts, "attempt to x%%0");
		return 0;
	}
	cri_nummod(ts, x, y, r);
	return r;
}


/* floating point modulus */
cr_number cr_value_modnum(cr_State *ts, cr_number x, cr_number y)
{
	cr_number r;
	cri_nummod(ts, x, y, r);
	return r;
}



/* number of bits in 'cr_integer' */
#define INTBITS		(sizeof(cr_integer)*8)


/* shift 'x', 'y' times, in case of overflow return 0 */
cr_integer cr_value_shiftr(cr_integer x, cr_integer y)
{
	if (y < 0) {
		if (y <= -INTBITS) return 0;
		return (x << y);
	} else {
		if (y >= INTBITS) return 0;
		return (x >> y);
	}
}


static cr_number numarithm(cr_State *ts, cr_number x, cr_number y, int op)
{
	switch(op) {
	case CR_OPADD: return cri_numadd(ts, x, y);
	case CR_OPSUB: return cri_numsub(ts, x, y);
	case CR_OPMUL: return cri_nummul(ts, x, y);
	case CR_OPDIV: return cri_numdiv(ts, x, y);
	case CR_OPMOD: return cr_value_modnum(ts, x, y);
	case CR_OPPOW: return cri_numpow(ts, x, y);
	case CR_OPNOT: return cri_nummul(ts, x, y);
	case CR_OPUMIN: return cri_nummul(ts, x, y);
	default: cr_unreachable(); return 0.0;
	}
}


static cr_integer intarithm(cr_State *ts, cr_integer x, cr_integer y, int op)
{
	cr_integer i;

	switch(op) {
	case CR_OPADD: return cri_intop(+, x, y);
	case CR_OPSUB: return cri_intop(-, x, y);
	case CR_OPMUL: return cri_intop(*, x, y);
	case CR_OPDIV: return cr_value_div(ts, x, y);
	case CR_OPMOD: return cr_value_modint(ts, x, y);
	case CR_OPPOW: return cri_intop(^, x, y);
	case CR_OPNOT: return cri_numnot(ts, x);
	case CR_OPUMIN: return cri_intop(-, 0, x);
	case CR_OPBSHL: return cr_value_shiftl(x, y);
	case CR_OPBSHR: return cr_value_shiftr(x, y);
	case CR_OPBNOT: return cri_intop(^, ~cri_castS2U(0), x);
	case CR_OPBAND: return cri_intop(&, x, y);
	case CR_OPBOR: return cri_intop(|, x, y);
	case CR_OPBXOR: return cri_intop(^, x, y);
	default: cr_unreachable(); return 0.0;
	}
}


/* convert number 'n' to integer according to 'mode' */
int cr_value_n2i(cr_number n, cr_integer *i, N2IMode mode)
{
	cr_number floored;

	floored = cr_floor(n);
	if (floored != n) {
		if (mode == CR_N2IEXACT) return 0;
		else if (mode == CR_N2ICEIL) floored++;
	}
	return cr_number2integer(n, i);
}


/* try to convert value to 'cr_integer' */
int cr_value_tointeger(const TValue *v, cr_integer *i, int mode)
{
	if (ttisnum(v)) {
		return cr_value_n2i(fval(v), i, mode);
	} else if (ttisint(v)) {
		*i = ival(v);
		return 1;
	}
	return 0;
}


/*
 * Perform raw arithmetic operations on numbers, what this means
 * is that no vtable methods will be invoked and the operation
 * itself can't invoke runtime error, if the operation can't be
 * done then return 0.
 */
int cr_value_arithmraw(cr_State *ts, const TValue *a, const TValue *b,
		       TValue *res, int op)
{
	cr_number n1, n2;

	switch (op) {
	case CR_OPBNOT: case CR_OPBXOR:
	case CR_OPBSHL: case CR_OPBSHR:
	case CR_OPBOR: case CR_OPBAND:
		if (ttisint(a) && ttisint(b)) {
			setival(res, intarithm(ts, ival(a), ival(b), op));
			return 1;
		}
		return 0;
	case CR_OPDIV: case CR_OPMOD:
		if (tonumber(a, &n1) && tonumber(b, &n2)) {
			setfval(res, numarithm(ts, n1, n2, op));
			return 1;
		}
		return 0;
	case CR_OPADD: case CR_OPSUB:
	case CR_OPMUL: case CR_OPNOT:
	case CR_OPUMIN:
		if (tonumber(a, &n1) && tonumber(b, &n2)) {
			setfval(res, numarithm(ts, n1, n2, op));
			return 1;
		}
		/* FALLTHRU */
	default: return 0;
	}
}


/*
 * Perform arithmetic operations on values, this function is free
 * to call overloaded methods such as '__add__', '__umin__', etc...,
 * in case raw arithmetic fails.
 */
void cr_value_arithm(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res, int op)
{
	if (!cr_value_arithmraw(ts, v1, v2, s2v(res), op))
		cr_meta_arithm(ts, v1, v2, res, (op - CR_OPADD) + CR_MADD);
}


/*
 * According to C99 6.3.1.8 page 45:
 * "...if the corresponding real type of either operand is double, the other
 * operand is converted, without change of type domain, to a type whose
 * corresponding real type is double."
 */
cr_sinline int intLEnum(cr_State *ts, const TValue *v1, const TValue *v2)
{
	return cri_numle(cast_num(ival(v1)), fval(v2));
}


/* check comment on function above ('intLEnum') */
cr_sinline int numLEint(cr_State *ts, const TValue *v1, const TValue *v2)
{
	return cri_numle(fval(v1), cast_num(ival(v2)));
}


/* less equal ordering on numbers */
cr_sinline int numLE(cr_State *ts, const TValue *v1, const TValue *v2)
{
	cr_number n1;
	cr_integer i1;

	cr_assert(ttisnum(v1) && ttisnum(v2));
	if (ttisint(v1)) {
		i1 = ival(v1);
		if (ttisint(v2)) return (i1 <= ival(v2));
		else return intLEnum(ts, v1, v2);
	} else {
		n1 = fval(v1);
		if (ttisint(v2)) return numLEint(ts, v1, v2);
		else return cri_numlt(n1, fval(v2));
	}
}


/* less equal ordering on non-number values */
cr_sinline int otherLE(cr_State *ts, const TValue *v1, const TValue *v2)
{
	if (ttisstr(v1) && ttisstr(v2))
		return (cr_string_cmp(strval(v1), strval(v2)) <= 0);
	else
		return cr_meta_order(ts, v1, v2, ts->stacktop.p, CR_MLE);
}


/* less or equal ordering '<=' */
int cr_value_orderLE(cr_State *ts, const TValue *v1, const TValue *v2)
{
	if (ttisnum(v1) && ttisnum(v2))
		return numLE(ts, v1, v2);
	return otherLE(ts, v1, v2);
}


/* check 'intLEnum' for conversion explanation */
cr_sinline int intLTnum(cr_State *ts, const TValue *v1, const TValue *v2)
{
	return cri_numlt(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' for conversion explanation */
cr_sinline int numLTint(cr_State *ts, const TValue *v1, const TValue *v2)
{
	return cri_numlt(fval(v1), cast_num(ival(v2)));
}


/* less than ordering on number values */
cr_sinline int numLT(cr_State *ts, const TValue *v1, const TValue *v2)
{
	cr_number n1;
	cr_integer i1;

	cr_assert(ttisnum(v1) && ttisnum(v2));
	if (ttisint(v1)) {
		i1 = ival(v1);
		if (ttisint(v2)) return (i1 <= ival(v2));
		else return intLTnum(ts, v1, v2);
	} else {
		n1 = fval(v1);
		if (ttisint(v2)) return numLTint(ts, v1, v2);
		else return cri_numlt(n1, fval(v2));
	}
}


/* less than ordering on non-number values */
cr_sinline int otherLT(cr_State *ts, const TValue *v1, const TValue *v2)
{
	if (ttisstr(v1) && ttisstr(v2))
		return (cr_string_cmp(strval(v1), strval(v2)) < 0);
	else
		return cr_meta_order(ts, v1, v2, ts->stacktop.p, CR_MLT);
}


/* less than ordering '<' */
int cr_value_orderLT(cr_State *ts, const TValue *v1, const TValue *v2)
{
	if (ttisnum(v1) && ttisnum(v2))
		return numLT(ts, v1, v2);
	return otherLT(ts, v1, v2);
}


/* equality ordering '==' */
int cr_value_orderEQ(cr_State *ts, const TValue *v1, const TValue *v2)
{
	cr_integer i1, i2;
	const TValue *method;
	const TValue *selfarg;

	if (vtt(v1) != vtt(v2)) {
		if (tt(v1) != tt(v2) || tt(v1) != CR_TNUMBER)
			return 0;
		return (cr_value_tointeger(v1, &i1, CR_N2IEXACT) &&
			cr_value_tointeger(v2, &i2, CR_N2IEXACT) && i1 == i2);
	}
	switch (vtt(v1)) {
	case CR_VNIL: case CR_VFALSE: case CR_VTRUE: return 1;
	case CR_VNUMINT: return (ival(v1) == ival(v2));
	case CR_VNUMFLT: return cri_numeq(fval(v1), fval(v2));
	case CR_VLUDATA: return (pval(v1) == pval(v2));
	case CR_VSTRING: return cr_string_eq(strval(v1), strval(v2));
	case CR_VUDATA:
		if (udval(v1) == udval(v2)) return 1;
		selfarg = v1;
		method = cr_meta_getvtable(ts, obj2gco(v1), CR_MEQ);
		if (!method) {
			selfarg = v2;
			method = cr_meta_getvtable(ts, obj2gco(v2), CR_MEQ);
		}
		break;
	case CR_VINSTANCE:
		if (insval(v1) == insval(v2)) return 1;
		selfarg = v1;
		method = cr_meta_getvtable(ts, obj2gco(v1), CR_MEQ);
		if (!method) {
			selfarg = v2;
			method = cr_meta_getvtable(ts, obj2gco(v2), CR_MEQ);
		}
		break;
	default:
		return (oval(v1) == oval(v2));
	}
	if (!method) return 0;
	cr_meta_callres(ts, selfarg, method, v1, v2, ts->stacktop.p);
	return !ttisfalsey(s2v(ts->stacktop.p - 1));
}
