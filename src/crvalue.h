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


#include <stdio.h>
#include <string.h>

#include "cript.h"
#include "crlimits.h"
#include "crmem.h"



/*
 * Additional types that are used only internally
 * or as markers.
 */
#define CR_TUVALUE      CR_NUMTYPES             /* upvalue */
#define CR_THTABLE      (CR_NUMTYPES + 1)       /* hashtable */


/* number of all types ('CR_T*') including 'CR_TNONE' */
#define CR_TOTALTYPES   (CR_THTABLE + 2)


CRI_DEC(const char *const cr_value_typenames[CR_TOTALTYPES]);

#define typename(t)     cr_value_typenames[(t) + 1]


/* Cript values */
typedef union Value {
    int b; /* boolean */
    cr_integer i; /* integer */
    cr_number n; /* float */
    void *p; /* light userdata */
    cr_cfunc cfn; /* C function */
    struct GCObject *o; /* collectable value */
} Value;


/* get raw union values */
#define rawbval(v)      ((v).b)
#define rawival(v)      ((v).i)
#define rawfval(v)      ((v).n)
#define rawpval(v)      ((v).p)
#define rawcfval(v)     ((v).cfn)
#define rawoval(v)      ((v).o)



/*
 * Tagged value types.
 * Bits 0-3 are for value types (CR_T*).
 * Bits 4-6 are for variant types (CR_V*).
 */

/* set variant bytes for type 't' */
#define makevariant(t, v)       ((t) | ((v) << 4))


/* tag bits (0-3) + variant bits (4-6) */
#define withvariant(t)  ((t) & 0x3F)

/* tag bits only (0-3) */
#define novariant(t)    ((t) & 0x0F)


/* macros for 'tt' */
#define rawtt(v)        ((v)->tt)
#define tt(v)           novariant(rawtt(v))
#define vtt(v)          withvariant(rawtt(v))
#define isvtt(v,t)      (vtt(v) == (t))



/* 'mod' bits */
#define MODnone         0 /* no modifiers */
#define MODconst        1 /* value is 'const' */

/* macros for 'mod' */
#define vmod(v)         ((v)->mod)
#define ismod(v,m)      testbit(vmod(v), (m))
#define isconst(v)      ismod((v), MODconst)



/* macro for 'val' */
#define vval(v)         ((v)->val)


/* copy values from 'v2' to 'v1' ('TValue') */
#define setval(ts,v1,v2) \
{ TValue *v1_ = (v1); const TValue *v2_ = (v2); \
    rawtt(v1_) = rawtt(v2_); vmod(v1_) = vmod(v2_); \
    v1_->val = v2_->val; }



/* 'TValue' fields, defined for reuse and alignment purposes */
#define TValueFields    Value val; cr_ubyte tt; cr_ubyte mod



/*
 * 'Value' with type and modifiers.
 * 'mod' might be unused but this memory would
 * be padded by compiler anyway (hopefully).
 */
typedef struct TValue {
    TValueFields;
} TValue;




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
#define s2v(s)          (&(s)->val_)

/* set stack value 'sv' to value 'v' */
#define setsval(ts,sv,v)                setval(ts, s2v(sv), v)



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

#define CR_VFALSE       makevariant(CR_TBOOL, 0)
#define CR_VTRUE        makevariant(CR_TBOOL, 1)

#define bval(v)         rawbval(vval(v))

/* set boolean false value */
#define setbfval(v) \
{ TValue *v_=(v); bval(v_)=0; rawtt(v_) = CR_VFALSE; }

/* set boolean true value */
#define setbtval(v) \
{ TValue *v_=(v); bval(v_)=1; rawtt(v_) = CR_VTRUE; }

#define ttisbool(v)             isvtt(v, CR_TBOOL)
#define ttistrue(v)             isvtt(v, CR_VTRUE)
#define ttisfalse(v)            isvtt(v, CR_VFALSE)

#define ttisfalsey(v)           (ttisfalse(v) || ttisnil(v))



/*
 * ---------------------------------------------------------------------------
 * Numbers
 * ---------------------------------------------------------------------------
 */

#define CR_VNUMINT              makevariant(CR_TNUMBER, 0)
#define CR_VNUMFLT              makevariant(CR_TNUMBER, 1)

#define ival(v)         rawival(vval(v))
#define fval(v)         rawfval(vval(v))
#define nval(v)         (isvtt(v, CR_VNUMINT) ? cast_num(ival(v)) : fval(v))

/* set integer value */
#define setival(v,i) \
{ TValue *v_=(v); ival(v_)=(i); rawtt(v_) = CR_VNUMINT; }

/* set float value */
#define setfval(v,f) \
{ TValue *v_=(v); fval(v_)=(f); rawtt(v_) = CR_VNUMFLT; }

#define ttisflt(v)              isvtt((v), CR_VNUMFLT)
#define ttisint(v)              isvtt((v), CR_VNUMINT)
#define ttisnum(v)              isvtt((v), CR_TNUMBER)



/*
 * ---------------------------------------------------------------------------
 * Light userdata
 * ---------------------------------------------------------------------------
 */

#define CR_VLUDATA              makevariant(CR_TLUDATA, 0)

#define pval(v)                 rawpval(vval(v))

/* set pointer value */
#define setpval(v,p) \
{ TValue *v_=(v); pval(v_)=(p); rawtt(v_) = CR_VLUDATA; }

#define ttislud(v)              isvtt(v, CR_VLUDATA)



/*
 * ---------------------------------------------------------------------------
 * C function
 * ---------------------------------------------------------------------------
 */

#define CR_VCFUNCTION           makevariant(CR_TFUNCTION, 0)

#define cfval(v)                rawcfval(vval(v))

/* set C function value */
#define setcfval(v,cf) \
{ TValue *v_=(v); cfval(v_)=(cf); rawtt(v_) = CR_VCFUNCTION; }

#define ttiscfn(v)              isvtt(v, CR_VCFUNCTION)



/*
 * ---------------------------------------------------------------------------
 * Object
 * ---------------------------------------------------------------------------
 */

#define oval(v)         rawoval(vval(v))

/* set object value */
#define setoval(v,o) \
{ TValue *v_=(v); oval(v_)=(o); rawtt(v_) = OBJECTTAG; }

/* check if value is collectable object */
#define ttiso(v)        (rawtt(v) == OBJECTTAG)



/*
 * ---------------------------------------------------------------------------
 * Nil
 * ---------------------------------------------------------------------------
 */

#define CR_VNIL         makevariant(CR_TNIL, 0)
#define CR_VEMPTY       makevariant(CR_TNIL, 1)
#define CR_VTOMB        makevariant(CR_TNIL, 2)

/* set nil value */
#define setnilval(v) \
{ TValue *v_=(v); rawtt(v_) = CR_VNIL; }

/* set nil value */
#define setemptyval(v) \
{ TValue *v_=(v); rawtt(v_) = CR_VEMPTY; }

#define ttisnil(v)      isvtt((v), CR_VNIL)
#define ttisempty(v)    isvtt((v), CR_VEMPTY)

/* --------------------------------------------------------------------------- */


/*
 * Conversion modes when converting 'cr_integer'
 * into 'cr_number'.
 */
typedef enum N2IMode {
    CR_N2IFLOOR,
    CR_N2ICEIL,
    CR_N2IEXACT,
} N2IMode;


/* convert value to 'cr_integer' */
#define tointeger(v, i) \
    (cr_likely(ttisint(v)) \
     ? (*(i) = ival(v), 1) \
     : cr_value_tointeger(v, i, CR_N2IFLOOR))


/* convert value to 'cr_number' */
#define tonumber(v, n) \
    (cr_likely(ttisflt(v)) \
     ? (*(n) = fval(v), 1) \
     : cr_likely(ttisint(v)) ? (*(n) = ival(v), 1) : 0)


/* same as right shift but indicate left by making 'y' negative */
#define cr_value_shiftl(x,y)    cr_value_shiftr(x, -y)


/* hash primitives */
#define cr_value_hashint(i)     cast_uint(cri_castS2U((i)))
#define cr_value_hashbool(b)    cast_uint((b) != 0)
#define cr_value_hashp(p)       pointer2uint((p))


CRI_FUNC uint cr_value_hashnum(cr_number n);
CRI_FUNC int cr_value_ceillog2(uint x);
CRI_FUNC int cr_value_n2i(cr_number n, cr_integer *i, N2IMode mode);
CRI_FUNC int cr_value_tointeger(const TValue *v, cr_integer *i, int mode);

CRI_FUNC cr_integer cr_value_div(cr_State *ts, cr_integer x, cr_integer y);
CRI_FUNC cr_integer cr_value_modint(cr_State *ts, cr_integer x, cr_integer y);
CRI_FUNC cr_number cr_value_modnum(cr_State *ts, cr_number x, cr_number y);
CRI_FUNC cr_integer cr_value_shiftr(cr_integer x, cr_integer y);

CRI_FUNC void cr_value_arithm(cr_State *ts, const TValue *a, const TValue *b,
                              SPtr res, int op);
CRI_FUNC int cr_value_arithmraw(cr_State *ts, const TValue *a, const TValue *b,
                                TValue *res, int op);

CRI_FUNC int cr_value_orderEQ(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC int cr_value_orderLT(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC int cr_value_orderLE(cr_State *ts, const TValue *v1, const TValue *v2);

#endif
