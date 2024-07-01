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
#include "crobject.h"
#include "crvalue.h"



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


static cr_number numarithmetic(TState *ts, cr_number x, cr_number y, int op)
{
	cr_number m;

	switch(op) {
	case CR_OPADD: return cri_numadd(ts, x, y);
	case CR_OPSUB: return cri_numsub(ts, x, y);
	case CR_OPMUL: return cri_nummul(ts, x, y);
	case CR_OPDIV: return cri_numdiv(ts, x, y);
	case CR_OPMOD: { 
		cri_nummod(ts, x, y, m);
		return m;
	}
	case CR_OPPOW: return cri_nummul(ts, x, y);
	case CR_OPNOT: return cri_nummul(ts, x, y);
	case CR_OPUMIN: return cri_nummul(ts, x, y);
	default: cr_unreachable(); return 0.0;
	}
}


static cr_number fltarithm(TState *ts, cr_floating a, cr_floating b, cr_ar op)
{
	switch (op) {
	case CR_OPADD:
		return cr_numadd(ts, a, b);
	case CR_OPSUB:
		return cr_numsub(ts, a, b);
	case CR_OPMUL:
		return cr_nummul(ts, a, b);
	case CR_OPDIV:
		return cr_numdiv(ts, a, b);
	case CR_OPMOD:;
		cr_nummod(ts, a, b, a);
		return a;
	case CR_OPPOW:
		return cr_numpow(ts, a, b);
	case CR_OPUMIN:
		return cr_numunm(ts, a);
	case CR_OPNOT:
		return cast(cr_floating, 0); // never 'falsey'
	default:
		cr_unreachable;
		return 0;
	}
}


/* Perform arithmetic operation 'op' on cript values.
 * If arithmetic operation was executed successfully then this
 * returns 1, otherwise 0. */
int varith(TState *ts, Value a, Value b, cr_ar op, Value *res)
{
	if (arisbin(op)) { // binary operation
		if (IS_NUMBER(a) && IS_NUMBER(b))
			goto l_unarynum;
		else if (isstring(a) && isstring(b))
			*res = OBJ_VAL(concatenate(ts, a, b));
		else
			return 0;
	} else { // unary operation
		if (IS_NUMBER(a)) {
l_unarynum:;
			cr_floating ret = narith(ts, AS_NUMBER(a), AS_NUMBER(b), op);
			*res = (op != SKAR_NOT ? NUMBER_VAL(ret) : FALSE_VAL);
		} else if (op == SKAR_NOT) {
			if (IS_BOOL(a))
				*res = !AS_BOOL(a);
			else if (isstring(a))
				*res = FALSE_VAL;
			else
				return 0;
		} else
			return 0;
	}
	return 1;
}

/* Perform binary/unary operation on values. */
void arith(TState *ts, Value a, Value b, cr_ar op, Value *res)
{
	if (!varith(ts, a, b, op, res)) {
#if defined(CR_OVERLOAD_OPS)
		otryop(ts, a, b, (op - AR_ADD) + OM_ADD, res);
#else
		operror(ts, a, b, op);
#endif
	}
}


/* Get value type */
cr_tt val2type(Value value)
{
#if defined(val2tbmask)

	static const int typetable[] = {
		TT_NIL, TT_NUMBER, TT_STRING, TT_BOOL, TT_CLASS, TT_INSTANCE, TT_FUNCTION,
	};
	cr_ubyte bitidx = cr_ctz(val2tbmask(value));
	return typetable[bitidx];

#elif defined(CR_NAN_BOX)

	if (IS_NIL(value))
		return TT_NIL;
	else if (IS_NUMBER(value))
		return TT_NUMBER;
	else if (isstring(value))
		return TT_STRING;
	else if (IS_BOOL(value))
		return TT_BOOL;
	else if (isclassobj(value))
		return TT_CLASS;
	else if (isinstance(value))
		return TT_INSTANCE;
	else if (isfunction(value) || isclosureobj(value) || iscfunction(value) || isboundmethod(value))
		return TT_FUNCTION;
	cr_unreachable;

#else

#if defined(CR_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#else
#define DISPATCH(x) switch (x)
#define CASE(l)	    case l:
#endif
	DISPATCH(value.type)
	{
		CASE(VAL_NIL)
		{
			return TT_NIL;
		}
		CASE(VAL_NUMBER)
		{
			return TT_NUMBER;
		}
		CASE(SKVAL_BOOL)
		{
			return TT_BOOL;
		}
		CASE(VAL_OBJ)
		{
			switch (otype(asobj(value))) {
			case OBJ_STRING:
				return TT_STRING;
			case OBJ_CLASS:
				return TT_CLASS;
			case OBJ_CFUNCTION:
			case OBJ_FUNCTION:
			case OBJ_CLOSURE:
			case OBJ_BOUND_METHOD:
				return TT_FUNCTION;
			case OBJ_INSTANCE:
				return TT_INSTANCE;
			default:
				cr_unreachable; // upvalue
			}
		}
	}

#endif
}




/* ================= ordering ================= */


/* Special equality ordering that preserves left operand ('switch' statement) */
void eq_preserveL(TState *ts, Value l, Value r)
{
	push(ts, r);
	*stkpeek(1) = l;
	veq(ts, l, r);
}

/* Raw equality.
 * No overloaded method will be invoked, instead the result is directly
 * returned from the function. */
cr_ubyte raweq(Value l, Value r)
{
#if defined(CR_NAN_BOX)
	// NaN != NaN
	if (IS_NUMBER(l) && IS_NUMBER(r))
		return AS_NUMBER(l) == AS_NUMBER(r);
	else
		return l == r;
#else
	if (l.type != r.type)
		return 0;
#if defined(CR_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#else
#define DISPATCH(x) switch (x)
#define CASE(label) case label:
#endif
	DISPATCH(l.type)
	{
		CASE(SKVAL_BOOL)
		{
			return AS_BOOL(l) == AS_BOOL(r);
		}
		CASE(VAL_NUMBER)
		{
			return AS_NUMBER(l) == AS_NUMBER(r);
		}
		CASE(VAL_NIL)
		{
			return 1;
		}
		CASE(VAL_OBJ)
		{
			return l == r;
		}
	}
#endif
}



/* != */
void vne(TState *ts, Value l, Value r)
{
#if defined(CR_NAN_BOX)
	if (IS_NUMBER(l) && IS_NUMBER(r))
		push(ts, BOOL_VAL(AS_NUMBER(l) != AS_NUMBER(r)));
	else
		one(ts, l, r);
#else
	if (l.type != r.type)
		push(ts, TRUE_VAL);
#if defined(CR_PRECOMPUTED_GOTO)
#define VAL_TABLE
#include "jmptable.h"
#undef VAL_TABLE
#undef BREAK
#define BREAK return
#else
#define DISPATCH(x) switch (x)
#define CASE(label) case label:
#define BREAK	    break
#endif
	DISPATCH(l.type)
	{
		CASE(SKVAL_BOOL)
		{
			push(ts, BOOL_VAL(AS_BOOL(l) != AS_BOOL(r)));
			BREAK;
		}
		CASE(VAL_NUMBER)
		{
			push(ts, BOOL_VAL(AS_NUMBER(l) != AS_NUMBER(r)));
			BREAK;
		}
		CASE(VAL_NIL)
		{
			push(ts, FALSE_VAL);
			BREAK;
		}
		CASE(VAL_OBJ)
		{
			one(ts, l, r);
			BREAK;
		}
	}
#endif
}


/* == */
cr_ubyte veq(TState *ts, Value l, Value r)
{
	cr_ubyte res;

	if (l.type != r.type) {
		push(ts, FALSE_VAL);
		return 0;
	}

	switch(VT(l)) {
	case VTBOOL:
		res = BOOL_VAL(AS_BOOL(l) == AS_BOOL(r));
		break;
	case VTINTEGER:
		res = BOOL_VAL(cr_flteq(asint(l), asint(r)));
		break;
	case VTNUMBER:
		res = BOOL_VAL(asnum(l) == asnum(r));
		break;
	case VTLUDATA:
		break;
	case VTCFUNC:
		break;
	case VTNIL:
		break;
	case VTOBJ:
		break;
	case VTEMPTY:
		break;
	}
	DISPATCH(l.type)
	{
		CASE(SKVAL_BOOL)
		{
			push(ts, BOOL_VAL(AS_BOOL(l) == AS_BOOL(r)));
			BREAK;
		}
		CASE(VAL_NUMBER)
		{
			push(ts, BOOL_VAL(AS_NUMBER(l) == AS_NUMBER(r)));
			BREAK;
		}
		CASE(VAL_NIL)
		{
			push(ts, TRUE_VAL);
			BREAK;
		}
		CASE(VAL_OBJ)
		{
			oeq(ts, l, r);
			BREAK;
		}
	}
#endif
}

/* Value less than */
void vlt(TState *ts, Value l, Value r)
{
	if (IS_NUMBER(l) && IS_NUMBER(r))
		push(ts, BOOL_VAL(AS_NUMBER(l) < AS_NUMBER(r)));
	else
		olt(ts, l, r);
}


/* greater than */
void vgt(TState *ts, Value l, Value r)
{
	if (IS_NUMBER(l) && IS_NUMBER(r))
		push(ts, BOOL_VAL(AS_NUMBER(l) > AS_NUMBER(r)));
	else
		ogt(ts, l, r);
}


/* less equal */
void vle(TState *ts, Value l, Value r)
{
	if (IS_NUMBER(l) && IS_NUMBER(r))
		push(ts, BOOL_VAL(AS_NUMBER(l) <= AS_NUMBER(r)));
	else
		ole(ts, l, r);
}


/* greater equal */
void vge(TState *ts, Value l, Value r)
{
	if (IS_NUMBER(l) && IS_NUMBER(r))
		push(ts, BOOL_VAL(AS_NUMBER(l) >= AS_NUMBER(r)));
	else
		oge(ts, l, r);
}



/* 
 * ==========================
 * Value to string conversion  
 * ==========================
 */

/* convert 'cr_integer' to string */
Value tostr_integer(TState *ts, cr_integer n)
{
	static char buff[MAXINT2STR];
	CRString *str;
	cr_ubyte len;

	len = snprintf(buff, sizeof(buff), INTEGERFMT, n);
	return OBJ_VAL(OString_new(ts, buff, len));
}

/* convert 'cr_floating' to string */
Value tostr_floating(TState *ts, cr_floating n)
{
	static char buff[MAXFLT2STR];
	CRString *str;
	cr_ubyte len;

	len = snprintf(buff, sizeof(buff), FLTFMT, n);
	return OBJ_VAL(OString_new(ts, buff, len));
}

/* convert void pointer to string */
Value tostr_ptr(TState *ts, void *ptr)
{
	static char buff[MAXVOIDP2STR];
	CRString *str;
	cr_ubyte len;

	len = snprintf(buff, sizeof(buff), PTRFMT, ptr);
	return OBJ_VAL(OString_new(ts, buff, len));
}

/*
 * Convert 'Value' to string value.
 * This will either create new 'OString' object or return
 * the existing one depending on the 'v' type.
 * If 'raw' is set then the raw conversion is performed.
 */
Value vtostr(TState *ts, Value v, cr_ubyte raw)
{
	switch (VT(v)) {
	case VTBOOL:
		return ssvbool(ts, AS_BOOL(v));
	case VTINTEGER:
		return tostr_integer(ts, asint(v));
	case VTNUMBER:
		return tostr_flt(ts, asnum(v));
	case VTNIL:
		return ssv(ts, SS_NIL);
	case VTLUDATA:
		return tostr_ptr(ts, AS_LUDATA(v));
	case VTCFUNC:
		return tostr_ptr(ts, cast(void *, AS_CFUNC(v)));
	case VTOBJ:
		return otostr(ts, asobj(v), raw);
	default:
		cr_unreachable;
	}
}
