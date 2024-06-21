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
#define CR_TUVALUE	CR_NTYPES	 /* upvalue */
#define CR_TOBJECT	(CR_NTYPES + 1)	 /* for marking object values */


/* 
 * Number of all types ('CR_T*') but excluding marker type
 * 'CR_TOBJECT', but including 'CR_TNONE'.
 */
#define CR_TOTALTYPES	(CR_TUVALUE + 2)


/* Cript values */
typedef union Value {
	int b; /* boolean */
	cr_integer i; /* integer */
	cr_number n; /* float */
	void *lud; /* light userdata */
	cr_cfunc cfn; /* C function */
	struct GCObject *o; /* collectable value */
} Value;


/* get raw union values */
#define rawbvalue(v)	((v).b)
#define rawivalue(v)	((v).i)
#define rawfvalue(v)	((v).n)
#define rawpvalue(v)	((v).lud)
#define rawcfvalue(v)	((v).cfn)
#define rawovalue(v)	((v).o)


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
	  

/* mod bit/s */
#define MODconst	1 /* value is 'const' */

/* macros for 'mod' */
#define vmod(v)		((v)->mod)
#define ismod(v,m)	testbit(vmod(v), (m))
#define isconst(v)	ismod((v), MODconst)


/* macros for 'val' */
#define vval(v)		((v)->val)


/* copy values from 'v2' to 'v1' ('TValue') */
#define setv(vm,v1,v2) \
	{ TValue *v1_ = (v1); const TValue *v2_ = (v2); \
	  setvtt(v1_, vtt(v2_)); vmod(v1_) = vmod(v2_); \
	  v1_->val = v2_->val; }



/* 'TValue' fields, defined for reuse and alignment purposes */
#define TValueFields	Value val; cr_ubyte tt; cr_ubyte mod



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

/* set stack value 'sv' to value 'v' */
#define setsv(vm,sv,v)		setv(vm, s2v(sv), v)



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

/* set boolean false value */
#define setbfvalue(v) \
	{ TValue *v_=(v); bvalue(v_)=0; setvtt(v_, CR_VFALSE); }

/* set boolean true value */
#define setbtvalue(v) \
	{ TValue *v_=(v); bvalue(v_)=1; setvtt(v_, CR_VTRUE); }

#define ttisboolean(v)		isvtt(v, CR_TBOOL)
#define ttistrue(v)		isvtt(v, CR_VTRUE)
#define ttisfalse(v)		isvtt(v, CR_VFALSE)

#define ttisfalsey(v)		(ttisfalse(v) || ttisnil(v))

#define newbvalue(v) \
	((TValue){.val = {.boolean = (v)}, .tt = makevariant(CR_TBOOL, (v)), .mod=0})



/* 
 * ---------------------------------------------------------------------------
 * Numbers
 * ---------------------------------------------------------------------------
 */

#define CR_VNUMINT	makevariant(CR_TNUMBER, 0)
#define CR_VNUMFLT	makevariant(CR_TNUMBER, 1)

#define ivalue(v)	(rawivalue((v)->val))
#define fvalue(v)	(rawfvalue((v)->val))
#define nvalue(v)	(isvtt(CR_VNUMINT) ? cast_num(ivalue(v)) : fvalue(v))

/* set integer value */
#define setivalue(v,i) \
	{ TValue *v_=(v); ivalue(v_)=(i); setvtt(v_, CR_VNUMINT); }

/* set float value */
#define setfvalue(v,f) \
	{ TValue *v_=(v); fvalue(v_)=(f); setvtt(v_, CR_VNUMFLT); }

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

/* set pointer value */
#define setpvalue(v,p) \
	{ TValue *v_=(v); pvalue(v_)=(p); setvtt(v_, CR_VLUDATA); }

#define ttislud(v)	isvtt(v, CR_VLUDATA)

#define newpvalue(v)	((TValue){.val = {.lud = (v)}, .tt = CR_VLUDATA, .mod=0})



/* 
 * ---------------------------------------------------------------------------
 * C function
 * ---------------------------------------------------------------------------
 */

#define CR_VCFUNCTION	makevariant(CR_TFUNCTION, 0)

#define cfvalue(v)	((v)->val.cfn)

/* set C function value */
#define setcfvalue(v,cf) \
	{ TValue *v_=(v); cfvalue(v_)=(cf); setvtt(v_, CR_VCFUNCTION); }

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

/* set object value */
#define setovalue(v,o) \
	{ TValue *v_=(v); ovalue(v_)=(o); setvtt(v_, CR_TOBJECT); }

#define ttiso(v)	isvtt(v, CR_TOBJECT)

#define newovalue(v)	((TValue){.val = {.o = (GCObject*)(v)}, .tt = CR_TOBJECT, .mod = 0})



/* 
 * ---------------------------------------------------------------------------
 * Nil
 * ---------------------------------------------------------------------------
 */

#define CR_VNIL		makevariant(CR_TNIL, 0)
#define CR_VEMPTY	makevariant(CR_TNIL, 1)
#define CR_VTOMB	makevariant(CR_TNIL, 2)

/* set nil value */
#define setnilvalue(v) \
	{ TValue *v_=(v); setvtt(v_, CR_VNIL); }

/* set nil value */
#define setemptyvalue(v) \
	{ TValue *v_=(v); setvtt(v_, CR_VEMPTY); }

#define ttisnil(v)	isvtt((v), CR_VNIL)
#define ttisempty(v)	isvtt((v), CR_VEMPTY)

#define newnilvalue()	((TValue){.val = {0}, .tt = CR_VNIL, .mod=0})
#define newemptyvalue() ((TValue){.val = {0}, .tt = CR_VEMPTY, .mod=0})

/* --------------------------------------------------------------------------- */



int cr_ve_ceillog2 (unsigned int x);
int v2t(TValue value);
TValue vtostr(VM *vm, TValue value, cr_ubyte raw);
void varith(VM *vm, TValue a, TValue b, int op, TValue *res);


#endif
