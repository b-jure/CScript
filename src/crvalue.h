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
#include "crmem.h"

#include <stdio.h>
#include <string.h>



/* types of values (tags) */
typedef enum {
	VT_BOOL = 0,
	VT_INTEGER,
	VT_NUMBER,
	VT_LUDATA,
	VT_CFUNC,
	VT_NIL,
	VT_OBJ,
	VT_EMPTY,
} ValueType;


/* tagged union */
typedef struct {
	union {
		cr_ubyte boolean; /* boolean */
		cr_integer integer; /* integer */
		cr_number number; /* float */
		void *lud; /* light userdata */
		cr_cfunc cfn; /* C function */
		struct GCObject *o; /* collectable value */
	} as;
	ValueType type;
} Value;


Vec(ValueVec, Value);


/* value type */
#define vtt(v)		((v)->type)

/* check if value type matches 't' */
#define isvtt(v,t)	(vtt(v) == (t))


#define s2v(sv)		(



/*
 * Represents value on the stack.
 * It contains 'tbc' fields which represents
 * offset from the current stack value to the
 * next value on the stack that needs to-be-closed.
 * 'tbc' being 0 indicates that the value
 * doesn't fit in 'unsigned short' and it is
 * assumed that the actual value is USHRT_MAX.
 * This way we can represent larger distances
 * without using larger data type.
 */
typedef union {
	Value sv;
	unsigned short tbc;
} SValue;



/* stack pointer */
typedef SValue *SPtr;



/* 
 * Value that acts as index into the stack.
 * Before reallocation occurs 'offset' is filled
 * accordingly in case 'p' becomes invalid,
 * and then after reallocation 'p' is restored.
 */
typedef struct {
	SPtr p; /* pointer to the value on the stack */
	ptrdiff_t offset; /* used when stack is being reallocated */
} SIndex;



/* 
 * ---------------------------------------------------------------------------
 * Boolean
 * ---------------------------------------------------------------------------
 */

/* get boolean from value 'v' */
#define asboolean(v)		((v)->as.boolean)

/* create boolean value */
#define booleanval(v)		((Value){ .type = VT_BOOL, { .boolean = v } })

/* check if value 'v' is boolean value */
#define isboolean(v)		isvtt(v, VT_BOOL)

/* set 'bp' to boolean if 'v' is boolean value */
#define toboolean(v, bp)	(isboolval(v) ? (*(bp) = asbool(v), 1) : 0)



/* 
 * ---------------------------------------------------------------------------
 * Numbers
 * ---------------------------------------------------------------------------
 */

/* check if 'ar' is binary arithmetic operation */
#define arisbin(ar)	((ar) >= CR_AR_ADD && (ar) <= CR_AR_POW)

/* check if 'ar' is unary arithmetic operation */
#define arisun(ar)	((ar) >= CR_AR_NOT && (ar) <= CR_AR_UMIN)

/* get 'cr_integer' from value 'v' */
#define asinteger(v)	((v)->as.integer)

/* get 'cr_number' from value 'v' */
#define asnumber(v)	((v)->as.number)

/* create 'cr_integer' value */
#define integerval(v)	((Value){ .type = VT_INTEGER, { .integer = v } })

/* create 'cr_number' value */
#define numberval(v)	((Value){ .type = VT_NUMBER, { .number = v } })

/* check if value 'v' is 'cr_integer' value */
#define isinteger(v)	isvtt(v, VT_INTEGER)

/* check if value 'v' is 'cr_number' value */
#define isnumber(v)	isvtt(v, VT_NUMBER)

/* set 'np' to 'cr_integer' value if 'v' is integer value */
#define tointeger(v, np)	(isintval(v) ? (*(np) = asint(v), 1) : 0)

/* set 'np' to 'cr_number' value if 'v' is floating value */
#define tonumber(v, np)		(isnumval(v) ? (*(np) = asnum(v), 1) : 0)


void varith(VM *vm, Value a, Value b, int op, Value *res);



/* 
 * ---------------------------------------------------------------------------
 * Light userdata
 * ---------------------------------------------------------------------------
 */

/* get light userdata from value 'v' */
#define asludata(v) 		((v)->as.lud)

/* create light userdata value */
#define ludataval(v)		((Value){ .type = VT_LUDATA, { .lud = v } })

/* check if value 'v' is light userdata value */
#define isludata(v)		isvtt(v, VT_LUDATA)



/* 
 * ---------------------------------------------------------------------------
 * C function
 * ---------------------------------------------------------------------------
 */

/* get 'cr_cfunc' from value 'v' */
#define ascfunction(v)  	((v)->as.cfn)

/* create 'cr_cfunc' value */
#define cfunctionval(v) 	((Value){ .type = VT_CFUNC, { .cfn = v } })

/* check if value 'v' is 'cr_cfunc' value */
#define iscfunction(v)		isvtt(v, VT_CFUNC)



/* 
 * ---------------------------------------------------------------------------
 * Object
 * ---------------------------------------------------------------------------
 */

/* get object from value 'v' */
#define asobj(v)		((v)->as.o)

/* create object value */
#define objval(v)    		((Value){ .type = VT_OBJ, { .o = (GCObject *)v } })

/* check if value 'v' is object value */
#define isobj(v)		isvtt(v, VT_OBJ)


/* 
 * ---------------------------------------------------------------------------
 * Nil
 * ---------------------------------------------------------------------------
 */

/* create nil value */
#define nilval()	      	((Value){ .type = VT_NIL, { 0 } })

/* check if value 'v' is nil value */
#define isnil(v)		isvtt(v, VT_NIL)



/* 
 * ---------------------------------------------------------------------------
 * Empty (private)
 * ---------------------------------------------------------------------------
 */

/* create empty value */
#define empytval()     		((Value){ .type = VT_EMPTY, { 0 } })

/* check if value 'v' is empty value */
#define isempty(v)		isvtt(v, VT_EMPTY)

/* marker for undefined values (global ids) */
#define undefval() 		empytval()

/* check if value 'v' is undefined */
#define isundef(v)		isemptyval(v)



/*
 * ---------------------------------------------------------------------------
 * Generic functions/macros
 * ---------------------------------------------------------------------------
 */

int v2t(Value value);
Value vtostr(VM *vm, Value value, cr_ubyte raw);
unsigned int vhash(VM *vm, Value value, cr_ubyte raw);

#endif
