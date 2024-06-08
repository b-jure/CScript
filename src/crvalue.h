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


/* Cript values */
typedef union Value {
	int boolean; /* boolean */
	cr_integer i; /* integer */
	cr_number n; /* float */
	void *lud; /* light userdata */
	cr_cfunc cfn; /* C function */
	struct GCObject *o; /* collectable value */
} Value;



/*
 * Tagged value types.
 * Bits 0-4 are for value types (CR_T*).
 * Bits 5-7 are for variant types (CR_V*).
 */

/* set variant bytes for type 't' */
#define makevariant(t, v)	((t) | ((v) << 5))



/* macros for 'tt' */
#define vtt(v)		((v)->tt)
#define isvtt(v,t)	(vtt(v) == (t))
#define setvtt(v,t)	(vtt(v) = (t))



/* 'mod' bits */
#define CRMconst	0 /* value is constant */
#define CRMcaptured	1 /* only for local variables */


/* macros for 'mod' */
#define vmod(v)		((v)->mod)
#define ismod(v,m)	testbit(vmod(v), (m))
#define isconst(v)	ismod((v), CRMconst)
#define iscaptured(v)	ismod((v), CRMcaptured)



/* 'TValue' fields, defined for reuse (alignment) */
#define TValueFields	Value val; unsigned char tt; unsigned char mod;



/* 
 * 'Value' with type and modifiers. 
 * 'mod' might be unused but this memory would
 * be padded by compiler anyway (hopefully).
 */
typedef struct TValue {
	TValueFields;
} TValue;



Vec(TValueVec, TValue);




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
	TValue val_;
	struct {
		TValueFields;
		unsigned short tbc;
	} tbc;
} SValue;


/* stack value to value */
#define s2v(s)		(&(s)->val_)



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

#define bvalue(v)	((v)->val.boolean)

#define ttisboolean(v)		isvtt(v, CR_TBOOL)
#define ttistrue(v)		isvtt(v, CR_VTRUE)
#define ttisfalse(v)		isvtt(v, CR_VFALSE)

#define setbfvalue(v)	setvtt(v, CR_VFALSE)
#define setbtvalue(v)	setvtt(v, CR_VTRUE)

#define ttisfalsey(v)	(ttisfalse(v) || ttisnil(v))

#define newbvalue(v) \
	((TValue){.val = {.boolean = (v)}, .tt = makevariant(CR_TBOOL, (v)), .mod=0})



/* 
 * ---------------------------------------------------------------------------
 * Numbers
 * ---------------------------------------------------------------------------
 */

#define CR_VNUMINT	makevariant(CR_TNUMBER, 0)
#define CR_VNUMFLT	makevariant(CR_TNUMBER, 1)

#define ivalue(v)	((v)->val.i)
#define fvalue(v)	((v)->val.n)
#define nvalue(v)	(isvtt(CR_VNUMINT) ? cast_num(ivalue(v)) : fvalue(v))

#define ttisflt(v)	isvtt((v), CR_VNUMFLT)
#define ttisint(v)	isvtt((v), CR_VNUMINT)
#define ttisnum(v)	isvtt((v), CR_TNUMBER)

#define newfvalue(v)	((TValue){.val = {.n = (v)}, .tt = CR_VNUMFLT, .mod=0})
#define newivalue(v)	((TValue){.val = {.i = (v)}, .tt = CR_VNUMINT, .mod=0})



/* 
 * ---------------------------------------------------------------------------
 * Light userdata
 * ---------------------------------------------------------------------------
 */

#define CR_VLUDATA	makevariant(CR_TLUDATA, 0)

#define pvalue(v)	((v)->val.lud)

#define ttislud(v)	isvtt(v, CR_VLUDATA)

#define newpvalue(v)	((TValue){.val = {.lud = (v)}, .tt = CR_VLUDATA, .mod=0})



/* 
 * ---------------------------------------------------------------------------
 * C function
 * ---------------------------------------------------------------------------
 */

#define CR_VCFUNCTION	makevariant(CR_TFUNCTION, 0)

#define cfvalue(v)	((v)->val.cfn)

#define ttiscfn(v)	isvtt(v, CR_VCFUNCTION)

#define newcfnvalue(v)	((TValue){.val = {.cfn = (v)}, .type = CR_VCFUNCTION, .mod=0})



/* 
 * ---------------------------------------------------------------------------
 * Object
 * ---------------------------------------------------------------------------
 */

/*
 * All collectable objects create variant from this type.
 *
 */
#define ovalue(v)	((v)->val.o)

#define ttiso(v)	isvtt(v, CR_TOBJECT)

#define newovalue(v)	((TValue){.val = {.o = (GCObject*)(v)}, .tt = CR_TOBJECT, .mod = 0})



/* 
 * ---------------------------------------------------------------------------
 * Nil
 * ---------------------------------------------------------------------------
 */

#define CR_VNIL		makevariant(CR_NTYPESIL, 0)
#define CR_VEMPTY	makevariant(CR_NTYPESIL, 1)

#define ttisnil(v)	isvtt((v), CR_VNIL)
#define ttisempty(v)	isvtt((v), CR_VEMPTY)

#define newnilvalue()	((TValue){.val = {0}, .type = CR_VNIL, .mod=0})
#define newemptyvalue() ((TValue){.val = {0}, .type = CR_VEMPTY, .mod=0})

/* --------------------------------------------------------------------------- */



int v2t(TValue value);
TValue vtostr(VM *vm, TValue value, cr_ubyte raw);
void varith(VM *vm, TValue a, TValue b, int op, TValue *res);


#endif
