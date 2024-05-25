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

#ifndef CRVALUE_H
#define CRVALUE_H

#include "crcommon.h"
#include "crhash.h"

#include <stdio.h>
#include <string.h>



/* types of values (tags) */
typedef enum {
	VT_BOOL = 0,
	VT_INTEGER,
	VT_FLOAT,
	VT_LUDATA,
	VT_CFUNC,
	VT_NIL,
	VT_OBJ,
	VT_EMPTY,
} ValueType;


/* tagged union */
typedef struct {
	ValueType type;
	union {
		cr_ubyte boolean; /* boolean */
		cr_integer integer; /* integer */
		cr_floating flt; /* float */
		cr_lud lud; /* light userdata */
		cr_cfunc cfn; /* C function */
		struct O *object; /* GC object */
	} as;
} Value;


/* value type */
#define VT(v) ((v).type)

/* check if value type matches 't' */
#define IS_VT(v, t) (VT(v) == (t))


/* pointer to 'Value' on the stack */
typedef Value *StkValue;


/* 
 * Relative 'StkValue'.
 * When reallocating stack all stack pointers get changed
 * to offsets before stack reallocation occurs.
 */
typedef union {
	StkValue sp; /* stack pointer */
	cr_ptrdiff offset; /* stack offset */
} RelStkValue;



/* 
 * -------
 * Boolean
 * -------
 */

/* get boolean from value 'v' */
#define AS_BOOL(v)	((v).as.boolean)

/* create boolean value */
#define BOOL_VAL(v)	((Value){ .type = VT_BOOL, { .boolean = v } })

/* check if value 'v' is boolean value */
#define IS_BOOL(v)	IS_VT(v, VT_BOOL)

/* set 'bp' to boolean if 'v' is boolean value */
#define tobool(v, bp)	  	(IS_BOOL(v) ? (*(bp) = AS_BOOL(v), 1) : 0)



/* 
 * ------
 * Number
 * ------
 */

/* check if 'ar' is binary arithmetic operation */
#define arisbin(ar)	((ar) >= CR_AR_ADD && (ar) <= CR_AR_POW)

/* check if 'ar' is unary arithmetic operation */
#define arisun(ar)	((ar) >= CR_AR_NOT && (ar) <= CR_AR_UMIN)

/* get 'cr_integer' from value 'v' */
#define AS_INT(v)	((v).as.integer)
#define AS_INTREF(v) 	((v)->as.integer)

/* get 'cr_floating' from value 'v' */
#define AS_FLOAT(v)	((v).as.flt)

/* create 'cr_integer' value */
#define INT_VAL(v)	((Value){ .type = VT_INTEGER, { .integer = v } })

/* create 'cr_floating' value */
#define FLOAT_VAL(v)	((Value){ .type = VT_FLOAT, { .flt = v } })

/* check if value 'v' is 'cr_integer' value */
#define IS_INT(v)	IS_VT(v, VT_INTEGER)

/* check if value 'v' is 'cr_floating' value */
#define IS_FLOAT(v)	IS_VT(v, VT_FLOAT)

/* check if value 'v' is 'cr_floating' or 'cr_integer' value */
#define IS_NUM(v)	(IS_FLOAT(v) | IS_INT(v))

/* set 'np' to 'cr_integer' value if 'v' is integer value */
#define tointeger(v, np)	(IS_INT(v) ? (*(np) = AS_INT(v), 1) : 0)

/* set 'np' to 'cr_floating' value if 'v' is floating value */
#define tofloating(v, np)	(IS_FLOAT(v) ? (*(np) = AS_FLOAT(v), 1) : 0)


/* modulo 'a - floor(a/b)*b' */
#define cr_nummod(vm, a, b, m) { \
	(m) = fmod(a, b); \
	if (((m) > 0) ? (b) < 0 : ((m) < 0 && (b) > 0)) \
		(m) += (b); }

/* division */
#define cr_numdiv(vm, a, b)	((a)/(b))

/* integer division */
#define cr_numidiv(vm, a, b)	(floor(cr_numdiv(a, b))

/* exponentiation, addition, subtraction, multiplication, negation */
#define cr_numpow(vm, a, b)	((b) == 2 ? (a) * (a) : pow(a, b))
#define cr_numadd(vm, a, b) 	((a) + (b))
#define cr_numsub(vm, a, b) 	((a) - (b))
#define cr_nummul(vm, a, b) 	((a) * (b))
#define cr_numunm(vm, a)	(-(a))

/* ordering */
#define cr_numeq(a, b)		((a) == (b))
#define cr_numne(a, b) 		(!cr_flteq(a, b))
#define cr_numlt(a, b) 		((a) < (b))
#define cr_numle(a, b) 		((a) <= (b))
#define cr_numgt(a, b) 		((a) > (b))
#define cr_numge(a, b) 		((a) >= (b))

/* check if number is NaN */
#define cr_numisnan(a)		(!cr_numeq((a), (a)))

void varith(VM *vm, Value a, Value b, cr_ar op, Value *res);



/* 
 * --------------
 * Light userdata
 * --------------
 */

/* get light userdata from value 'v' */
#define AS_LUDATA(v) 		((v).as.lud)

/* create light userdata value */
#define LUDATA_VAL(v)		((Value){ .type = VT_LUDATA, { .lud = v } })

/* check if value 'v' is light userdata value */
#define IS_LUDATA(v)		IS_VT(v, VT_LUDATA)



/* 
 * ----------
 * C function
 * ----------
 */

/* get 'cr_cfunc' from value 'v' */
#define AS_CFUNC(v)  		((v).as.cfn)

/* create 'cr_cfunc' value */
#define CFUNC_VAL(v)  		((Value){ .type = VT_CFUNC, { .cfn = v } })

/* check if value 'v' is 'cr_cfunc' value */
#define IS_CFUNC(v)		IS_VT(v, VT_CFUNC)



/* 
 * ------
 * Object
 * ------
 */

/* get object from value 'v' */
#define AS_OBJ(v)		((v).as.object)

/* create object value */
#define OBJ_VAL(v)    		((Value){ .type = VT_OBJ, { .object = (O *)v } })

/* check if value 'v' is object value */
#define IS_OBJ(v)		IS_VT(v, VT_OBJ)


/* 
 * ---
 * Nil
 * ---
 */

/* create nil value */
#define NIL_VAL	      		((Value){ .type = VT_NIL, { 0 } })

/* check if value 'v' is nil value */
#define IS_NIL(v)		IS_VT(v, VT_NIL)



/* 
 * ---------------
 * Empty (private)
 * ---------------
 */

/* create empty value */
#define EMPTY_VAL     		((Value){ .type = VT_EMPTY, { 0 } })

/* check if value 'v' is empty value */
#define IS_EMPTY(v)		IS_VT(v, VT_EMPTY)

/* marker for undefined values (global ids) */
#define UNDEFINED_VAL 		EMPTY_VAL

/* check if value 'v' is undefined */
#define IS_UNDEFINED(v) 	IS_EMPTY(v)



/*
 * ------------------------
 * Generic functions/macros
 * ------------------------
 */

cr_tt v2t(Value value);
Value vtostr(VM *vm, Value value, cr_ubyte raw);
cr_hash vhash(VM *vm, Value value, cr_ubyte raw);

#endif
