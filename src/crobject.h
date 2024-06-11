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

#ifndef CROBJECT_H
#define CROBJECT_H


#include "crhash.h"
#include "crhashtable.h"
#include "crmem.h"
#include "cript.h"
#include "crvalue.h"


/* object types */
typedef enum {
	OBJ_STRING = 0,
	OBJ_FUNCTION,
	OBJ_CLOSURE,
	OBJ_CRLOSURE,
	OBJ_CCLOSURE,
	OBJ_UVAL,
	OBJ_CLASS,
	OBJ_INSTANCE,
	OBJ_BOUND_METHOD,
} OType;



/* common header for objects */
#define ObjectHeader	struct GCObject* next; cr_ubyte ott; cr_ubyte mark


/* common type for collectable objects */
typedef struct GCObject {
	ObjectHeader;
} GCObject;


#define rawott(o)	((o)->ott)
#define rawomark(o)	((o)->mark)

#define ott(v)		(rawott(ovalue(v)))
#define omark(v)	(rawomark(ovalue(v)))

#define setott(v,t)	(ott(v) = (t))
#define isott(v,t)	(ott(v) == (t))


/* set value to GC object */
#define setv2o(vm,v,o,t) \
	{ TValue *v_=(v); t *o_=o; ovalue(v_) = cast(GCObject*,o_); }


/* set stack value to GC object */
#define setsv2o(vm,sv,o,t)	setv2o(vm,s2v(sv),o,t)



/* 
 * ---------------------------------------------------------------------------
 * OString 
 * ---------------------------------------------------------------------------
 */

typedef struct OString {
	ObjectHeader;
	int len; /* excluding null terminator */
	cr_ubyte hashash;
	unsigned int hash;
	char bytes[];
} OString;


#define CR_VSTRING	makevariant(CR_TSTRING, 0)

#define ttisstr(v)	isott((v), CR_VSTRING)
#define strvalue(v)	((OString*)ovalue(v))
#define cstrvalue(v)	(strvalue(v)->bytes)


/* set value to string */
#define setv2s(vm,v,s)		setv2o(vm,v,s,OString)

/* set stack value to string */
#define setsv2s(vm,sv,s)	setv2s(vm,s2v(sv),s)


/* string is equal to string literal */
#define streqlit(s,lit,l,h) \
	((s)->len == (l) && (s)->hash == (h) && \
	 memcmp((s)->bytes, (lit), (l)) == 0)


/* size of string */
#define sizes(s)	(sizeof(OString) + (s)->len + 1)



/* 
 * ---------------------------------------------------------------------------
 * UValue 
 * ---------------------------------------------------------------------------
 */

typedef struct UValue {
	ObjectHeader;
	union {
		TValue *location; /* stack or 'closed' */
		ptrdiff_t offset; /* when reallocating stack */
	} v;
	struct UValue *nextuv; /* chain */
	TValue closed; /* value T */
} UValue;


#define CR_VUVALUE	makevariant(CR_TUVALUE, 0)

#define ttisuval(o)	isott((v), CR_VUVALUE)
#define uvvalue(v)	((UValue *)ovalue(v))


/* set value to upvalue */
#define setv2uv(vm,v,uv)	setv2o(vm,v,uv,UValue)

/* set stack value to upvalue */
#define setsv2uv(vm,sv,uv)	setv2uv(vm,s2v(sv),uv)


/* size of upvalue */
#define sizeuv()	sizeof(UValue)



/*
 * ---------------------------------------------------------------------------
 * Function
 * ---------------------------------------------------------------------------
 */

/* line information and associated instruction */
typedef struct LineInfo {
	int pc;
	int line;
} LineInfo;


Vec(LineInfoVec, LineInfo);


/* 'code' array */
typedef ubyteVec InstructionVec;


/* Cript chunk */
typedef struct Function {
	ObjectHeader;
	OString *name; /* function name */
	OString *source; /* source name */
	TValueVec constants;
	LineInfoVec lineinfo;
	InstructionVec code;
	int arity; /* number of arguments */
	int defline; /* function definition line */
	int deflastline; /* function definition end line */
	cr_ubyte isvararg; /* set if contains '...' */
} Function;


#define CR_VFUNCTION	makevariant(CR_TFUNCTION, 0)

#define ttisfn(v)	isott((v), CR_VFUNCTION)
#define fnvalue(v)	((Function *)asobj(v))


/* set value to function */
#define setv2fn(vm,v,fn)	setv2o(vm,v,fn,Function)

/* set stack value to upvalue */
#define setsv2fn(vm,sv,fn)	setv2fn(vm,s2v(sv),fn)

/* size of function */
#define sizefn()	sizeof(Function)



/* 
 * ---------------------------------------------------------------------------
 * Closures
 * ---------------------------------------------------------------------------
 */


#define CR_VCRCL	makevariant(CR_TFUNCTION, 1) /* 'CriptClosure' */
#define CR_VCCL		makevariant(CR_TFUNCTION, 2) /* 'CClosure' */



/* common closure header */
#define ClosureHeader	ObjectHeader; int nupvalues;


typedef struct CriptClosure {
	ClosureHeader;
	Function *fn;
	UValue *upvalue[1];
} CriptClosure;

#define ttiscrcl(v)		isott((v), CR_VCRCL)
#define crclvalue(v)		((CriptClosure*)ovalue(v))

/* set value to cript closure */
#define setv2crcl(vm,v,crcl)		setv2o(vm,v,crcl,CriptClosure)

/* set stack value to cript closure */
#define setsv2crcl(vm,sv,crcl)		setv2crcl(vm,s2v(sv),crcl)

/* size of cript closure */
#define sizecrcl(crcl) \
	(sizeof(CriptClosure) + (crcl)->nupvalues * sizeof(UValue*))



typedef struct {
	ClosureHeader;
	cr_cfunc fn;
	TValue upvalue[1];
} CClosure;

#define ttisccl(v)		isott((v), CR_VCCL)
#define cclvalue(v)		((CClosure*)ovalue(v))

/* set value to C closure */
#define setv2ccl(vm,v,ccl)	setv2o(vm,v,ccl,CClosure)

/* set stack value to C closure */
#define setsv2ccl(vm,sv,ccl)	setv2ccl(vm,s2v(sv),ccl)

/* size of C closure */
#define sizeccl(ccl) \
	(sizeof(CClosure) + (ccl)->nupvalues * sizeof(TValue))

/* 'cl' is not a 'CriptClosure' */
#define noCriptclosure(cl)	((cl) == NULL || (cl)->cc.ott != CR_VCRCL)



typedef union Closure {
	CClosure cc;
	CriptClosure crc;
} Closure;

/* set value to closure */
#define setv2cl(vm,v,cl)	setv2o(vm,v,cl,Closure)

/* set stack value to closure */
#define setsv2cl(vm,sv,cl)	setv2cl(vm,s2v(sv),cl)

#define ttiscl(v)	(ttisccl(v) || ttiscrcl(v))
#define clvalue(v)	((Closure*)ovalue(v))




/* 
 * ---------------------------------------------------------------------------
 * OClass 
 * ---------------------------------------------------------------------------
 */

typedef struct OClass {
	ObjectHeader;
	OString *name; /* class name */
	HTable mtab; /* method table */
	GCObject *vtable[CR_MNUM]; /* overloadable methods */
} OClass;


#define CR_VCLASS	makevariant(CR_TCLASS, 0)

#define ttiscls(v)	isott((v), CR_VCLASS)
#define clsvalue(v)	((OClass*)ovalue(v))


/* set value to class */
#define setv2cls(vm,v,cls)	setv2o(vm,v,cls,OClass)

/* set stack value to class */
#define setsv2cls(vm,sv,cls)	setv2cls(vm,s2v(sv),cls)

/* size of class */
#define sizecls()	sizeof(OClass)



/*
 * ---------------------------------------------------------------------------
 *  Instance
 * ---------------------------------------------------------------------------
 */


/* 'OClass' instance */
typedef struct Instance {
	ObjectHeader;
	OClass *oclass; /* pointer to class */
	HTable fields; /* instance fields */
} Instance;


#define CR_VINSTANCE	makevariant(CR_TINSTANCE, 0)

#define ttisins(v)	isott((v), CR_VINSTANCE)
#define insvalue(v)	((Instance*)ovalue(v))


/* set value to instance */
#define setv2ins(vm,v,ins)	setv2o(vm,v,ins,Instance)

/* set stack value to instance */
#define setsv2ins(vm,sv,ins)	setv2ins(vm,s2v(sv),ins)

/* size of instance */
#define sizeins()	sizeof(Instance)



/*
 * ---------------------------------------------------------------------------
 *  InstanceMethod
 * ---------------------------------------------------------------------------
 */

/* method bound to 'receiver' (Instance) */
typedef struct InstanceMethod {
	ObjectHeader;
	Instance *receiver;
	GCObject *method;
} InstanceMethod;

#define CR_VMETHOD	makevariant(3, CR_TFUNCTION)

#define ttisim(v)	isott((v), CR_VMETHOD)
#define imvalue(v)	((InstanceMethod*)ovalue(v))


/* set value to instance method */
#define setv2im(vm,v,im)		setv2o(vm,v,im,InstanceMethod)

/* set stack value to instance method */
#define setsv2im(vm,sv,im)		setv2im(vm,s2v(sv),im)

/* size of instance method */
#define sizeim()	sizeof(InstanceMethod)

/* --------------------------------------------------------------------------- */


/* get vtable method info */
#define vtmi(mt)	(&vtmethodinfo[(mt)])

typedef struct Tuple {
	int arity;
	int nreturns;
} Tuple;

/* array of tuples for vtable method */
extern const Tuple vtmethodinfo[CR_MNUM];


#define cr_ot_newstringlit(vm, lit)	OString_new((vm), (lit), SLL(lit))

void cr_ot_sourceid(char *adest, const char *src, size_t len);
const char *cr_ot_pushfstring(VM *vm, const char *fmt, ...);
int cr_ot_eqstring(OString *s1, OString *s2);
int32_t cr_ot_id2mtag(VM *vm, OString *id);
OString *cr_ot_newstring(VM *vm, const char *chars, size_t len);
OString *cr_ot_newvstringf(VM *vm, const char *fmt, va_list argp);
OString *cr_ot_newstringf(VM *vm, const char *fmt, ...);
OString *cr_ot_concatenate(VM *vm, GCObject* a, GCObject* b);
InstanceMethod *cr_ot_newinstancemethod(VM *vm, Instance *receiver, CriptClosure *method);
Instance *cr_ot_newinstance(VM *vm, OClass *cclass);
OClass *cr_ot_newclass(VM *vm, OString *name);
UValue *cr_ot_newuvalue(VM *vm, TValue *vp);
CriptClosure *cr_ot_newcrclosure(VM *vm, Function *fn, int nupvalues);
CClosure *cr_ot_newcclosure(VM *vm, cr_cfunc fn, int nupvalues);
Function *cr_ot_newfunction(VM *vm);
cr_ubyte cr_ot_vtcall(VM *vm, TValue instance, int tag);
void cr_ot_free(VM *vm, GCObject *o);
void cr_ot_tryop(VM *vm, TValue a, TValue b, int op, TValue *res);
void oprint(VM *vm, TValue value, cr_ubyte raw, FILE *stream);
void oeq(VM *vm, TValue l, TValue r);
void one(VM *vm, TValue l, TValue r);
void olt(VM *vm, TValue l, TValue r);
void ogt(VM *vm, TValue l, TValue r);
void ole(VM *vm, TValue l, TValue r);
void oge(VM *vm, TValue l, TValue r);
cr_ubyte cr_ot_rawindex(VM *vm, TValue instance, cr_ubyte get);

#endif
