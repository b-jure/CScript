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


/* additional types that are used only as markers internally */
#define CR_TUVALUE	CR_NTYPES	/* upvalue */
#define CR_TOBJECT	(CR_NTYPES + 1)	/* for marking object values */
#define CR_TTOMBSTONE	(CR_NTYPES + 2)	/* for marking 'dead' keys in hashtable */


/* 
 * Number of all types ('CR_T*') but excluding marker types
 * 'CR_TOBJECT' and 'CR_TTOMBSTONE', but including 'CR_TNONE'.
 */
#define CR_TOTALTYPES	(CR_TUVALUE + 2)


/*
 * Tagged value types.
 * Bits 0-4 are for value types (CR_T*).
 * Bits 5-7 are for variant types (CR_V*).
 */

/* set variant bytes for type 't' */
#define makevariant(t, v)	((t) | ((b) << 5))


/* 'mod' bits */
#define CRMconst	0 /* value is constant */


/* tagged union */
typedef struct {
	union {
		int boolean; /* boolean */
		cr_integer integer; /* integer */
		cr_number number; /* float */
		void *lud; /* light userdata */
		cr_cfunc cfn; /* C function */
		struct GCObject *o; /* collectable value */
	} as;
	unsigned char tt; /* type tag */
	unsigned char mod; /* modifiers */
} Value;


Vec(ValueVec, Value);


#define vtt(v)		((v)->tt)
#define isvtt(v,t)	(vtt(v) == (t))
#define setvtt(v,t)	(vtt(v) = (t))


#define vmod(v)		((v)->mod)
#define ismod(v,m)	testbit(vmod(v), (m))
#define isconst(v)	ismod(v, CRMconst)




/*
 * Represents value on the stack.
 * It contains 'tbc' fields which represents
 * offset from the current stack value to the
 * next value on the stack that needs to-be-closed.
 * 'tbc' being 0 indicates that the distance value
 * doesn't fit in 'unsigned short' and then it is
 * assumed that the actual value is USHRT_MAX.
 * This way we can represent larger distances
 * without using larger data type.
 * On 8-byte alignment 'SValue' is 16 bytes,
 * while on 4-byte alignement 'SValue' is 8 bytes.
 */
typedef union {
	Value sv;
	unsigned short tbc;
} SValue;


/* stack value to value */
#define s2v(s)		(&(s)->sv)



/* pointer to the value on the stack */
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

#define CR_VFALSE	makevariant(CR_TBOOL, 0)
#define CR_VTRUE	makevariant(CR_TBOOL, 1)

#define bvalue(v)	((v)->as.boolean)

#define ttisboolean(v)		isvtt(v, CR_TBOOL)
#define ttistrue(v)		isvtt(v, CR_VTRUE)
#define ttisfalse(v)		isvtt(v, CR_VFALSE)

#define setbfvalue(v)		setvtt(v, CR_VFALSE)
#define setbtvalue(v)		setvtt(v, CR_VTRUE)

#define ttisfalsey(v)	(ttisfalse(v) || ttisnil(v))

#define booleanval(v) \
	((Value){ .tt = makevariant(CR_TBOOL, (v)), { .boolean = (v) } })



/* 
 * ---------------------------------------------------------------------------
 * Numbers
 * ---------------------------------------------------------------------------
 */

#define CR_VNUMINT	makevariant(CR_NTYPESUMBER, 0)
#define CR_VNUMFLT	makevariant(CR_NTYPESUMBER, 1)

#define ivalue(v)	((v)->as.integer)
#define fvalue(v)	((v)->as.number)
#define nvalue(v)	(isvtt(CR_VNUMINT) ? cast_num(ivalue(v)) : fvalue(v))

#define ttisflt(v)	isvtt(v, CR_VNUMFLT)
#define ttisint(v)	isvtt(v, CR_VNUMINT)
#define ttisnum(v)	isvtt(v, CR_NTYPESUMBER)

#define newfvalue(v)	((Value){.tt = CR_VNUMFLT, {.number = (v)}})
#define newivalue(v)	((Value){.tt = CR_VNUMINT, {.integer = (v)}})



/* 
 * ---------------------------------------------------------------------------
 * Light userdata
 * ---------------------------------------------------------------------------
 */

#define CR_VLUDATA	makevariant(CR_TLUDATA, 0)

#define pvalue(v)	((v)->as.lud)

#define ttislud(v)	isvtt(v, CR_VLUDATA)

#define newpvalue(v)	((Value){.tt = CR_VLUDATA, {.lud = (v)}})



/* 
 * ---------------------------------------------------------------------------
 * C function
 * ---------------------------------------------------------------------------
 */

#define CR_VCFUNCTION	makevariant(CR_TFUNCTION, 0)

#define cfvalue(v)	((v)->as.cfn)

#define ttiscfn(v)	isvtt(v, CR_VCFUNCTION)

#define newcfnvalue(v)	((Value){.type = CR_VCFUNCTION, {.cfn = (v)}})



/* 
 * ---------------------------------------------------------------------------
 * Object
 * ---------------------------------------------------------------------------
 */

/*
 * All collectable objects create variant from this type.
 *
 */
#define ovalue(v)	((v)->as.o)

#define ttiso(v)	isvtt(v, CR_TOBJECT)

#define newovalue(v)	((Value){.tt = CR_TOBJECT, {.o = (GCObject *)(v)}})



/* 
 * ---------------------------------------------------------------------------
 * Nil
 * ---------------------------------------------------------------------------
 */

#define CR_VNIL		makevariant(CR_NTYPESIL, 0)
#define CR_VEMPTY	makevariant(CR_NTYPESIL, 1)

#define ttisnil(v)	isvtt((v), CR_VNIL)
#define ttisempty(v)	isvtt((v), CR_VEMPTY)

#define newnilvalue()	((Value){.type = CR_VNIL, {0}})
#define newemptyvalue() ((Value){.type = CR_VEMPTY, {0}})

/* --------------------------------------------------------------------------- */



int v2t(Value value);
Value vtostr(VM *vm, Value value, cr_ubyte raw);
void varith(VM *vm, Value a, Value b, int op, Value *res);


#endif
