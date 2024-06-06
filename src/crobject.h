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


#include "crchunk.h"
#include "crcommon.h"
#include "crhash.h"
#include "crmem.h"
#include "cript.h"
#include "crvalue.h"


/* object types */
typedef enum {
	OBJ_STRING = 0,
	OBJ_FUNCTION,
	OBJ_CLOSURE,	/* 'Closure' */
	OBJ_CRLOSURE,	/* 'CriptClosure' */
	OBJ_CCLOSURE,	/* 'CClosure' */
	OBJ_UVAL,
	OBJ_CLASS,
	OBJ_INSTANCE,
	OBJ_BOUND_METHOD,
} OType;



/* common header for objects */
#define ObjectHeader	struct GCObject* next; cr_ubyte otype; cr_ubyte marked


/* common type for collectable objects */
typedef struct GCObject {
	ObjectHeader;
} GCObject;



/* set/check object type */
#define osett(o,t)	((o)->otype = (t))
#define oist(o,t)	((o)->otype == (t))

/* mark object (GC) */
#define omark(o)	((o)->omark = 1)

/* get object type from value */
#define otype(v)	(asobj(v)->otype)


/* check if value is object and it's object type matches @t */
#define isot(v, t)	(isobj(v) && oist(asobj(v), t))


/* check if value is 'falsey' */
#define isfalsey(v)	(isnil(v) || (isboolval(v) && !asboolean(v)))



/* 
 * ---------------------------------------------------------------------------
 * OString 
 * ---------------------------------------------------------------------------
 */

#define isstring(v)	isot(v, OBJ_STRING)
#define asstring(v)   	((OString *)asobj(v))
#define ascstring(v)   	(asstring(v)->bytes)

/* set 'strp' to cstring if 'v' is string object value */
#define tostring(v, strp) 	(isstring(v) ? (*(strp) = ascstring(v), 1) : 0)


typedef struct {
	ObjectHeader;
	int len; /* excluding null terminator */
	unsigned int hash;
	char bytes[];
} OString;



/* 
 * ---------------------------------------------------------------------------
 * UValue 
 * ---------------------------------------------------------------------------
 */

#define isupvalue(v)	isot(v, OBJ_UPVAL)
#define asupvalue(v)	((UValue *)asobj(v))


typedef struct UValue {
	ObjectHeader;
	Value closed; /* value */
	Value *location; /* stack or 'closed' */
	UValue *nextuv; /* chain */
} UValue;



/*
 * ---------------------------------------------------------------------------
 * Function
 * ---------------------------------------------------------------------------
 */

#define isfunction(v)  	isot(v, OBJ_FUNCTION)
#define asfunction(v)	((Function *)asobj(v))


/* line information and associated instruction */
typedef struct {
	int pc;
	int line;
} LineInfo;


Vec(LineInfoVec, LineInfo);


/* 'code' array */
typedef ubyteVec InstructionVec;


typedef struct {
	ObjectHeader;
	OString *name; /* function name */
	OString *source; /* source name */
	ValueVec constants;
	LineInfoVec lineinfo;
	InstructionVec code;
	int arity; /* number of arguments */
	int defline; /* function definition line */
	int deflastline; /* function definition end line */
	cr_ubyte isvararg; /* set if contains '...' */
} Function;



/* 
 * ---------------------------------------------------------------------------
 * Closures
 * ---------------------------------------------------------------------------
 */

/* common closure header */
#define ClosureHeader	ObjectHeader; int nupvalues;


#define iscrclosure(v)   	isot(v, OBJ_CRLOSURE)
#define ascrclosure(v)   	((CriptClosure *)asobj(v))

typedef struct {
	ClosureHeader;
	Function *fn;
	UValue *upvalue[1]; /* upvalues */
} CriptClosure;



#define iscclosure(v)  	isot(v, OBJ_CCLOSURE)
#define ascclosure(v)   ((CClosure *)asobj(v))

typedef struct {
	ClosureHeader;
	cr_cfunc fn; /* C function */
	Value upvalue[1]; /* upvalues */
} CClosure;



/* check if 'Closure' is 'CClosure' */
#define isccl(cl)	((cl) != NULL && (cl)->cc.otype == OBJ_CCLOSURE)

#define isclosure(v)  	isot(v, OBJ_CLOSURE)
#define asclosure(v)   ((Closure *)asobj(v))

typedef union {
	CClosure cc;
	CriptClosure crc;
} Closure;



/* 
 * ---------------------------------------------------------------------------
 * OClass 
 * ---------------------------------------------------------------------------
 */

#define isclass(v)	isot(v, OBJ_CLASS)
#define asclass(v) 	((OClass *)asobj(v))


typedef struct {
	ObjectHeader;
	OString *name; /* class name */
	HashTable mtab; /* method table */
	GCObject *vtable[CR_MN]; /* overloadable methods */
} OClass;



/*
 * ---------------------------------------------------------------------------
 *  Instance
 * ---------------------------------------------------------------------------
 */

#define isinstance(v)  	(isot(v, OBJ_INSTANCE))
#define asinstance(v)  	((OInstance *)asobj(v))


/* 'OClass' instance */
typedef struct {
	ObjectHeader;
	OClass *oclass; /* pointer to class */
	HashTable fields; /* instance fields */
} Instance;



/*
 * ---------------------------------------------------------------------------
 *  InstanceMethod
 * ---------------------------------------------------------------------------
 */

#define ismethod(v)	isot(v, OBJ_BOUND_METHOD)
#define asmethod(v) 	((InstanceMethod *)asobj(v))


/* method bound to 'receiver' (Instance) */
typedef struct {
	ObjectHeader;
	Instance *receiver;
	GCObject *method;
} InstanceMethod;




int32_t cr_ob_id2mtag(VM *vm, OString *id);


OString *cr_ob_newstring(VM *vm, const char *chars, size_t len);
OString *cr_ob_newvstringf(VM *vm, const char *fmt, va_list argp);
OString *cr_ob_newstringf(VM *vm, const char *fmt, ...);
OString *cr_ob_concatenate(VM *vm, GCObject* a, GCObject* b);

#define cr_ob_newstringlit(vm, lit)	OString_new((vm), (lit), SLL(lit))

InstanceMethod *cr_ob_newinstancemethod(VM *vm, Value receiver, CriptClosure *method);
Instance *cr_ob_newinstance(VM *vm, OClass *cclass);
OClass *cr_ob_newclass(VM *vm, OString *name);
UValue *cr_ob_newuvalue(VM *vm, Value *var_ref);
CriptClosure *cr_ob_newcrclosure(VM *vm, Function *fn);
CClosure *cr_ob_newcclosure(VM *vm, OString *name, cr_cfunc fn, int32_t arity, cr_ubyte isvararg, int upvals);
Function *cr_ob_newfunction(VM *vm);


/* call ('()') overload-able method */
cr_ubyte cr_ob_vtcall(VM *vm, Value instance, int tag);


/* get 'OInstance' tables */
#define getfieldtable(ins)  (&(ins)->fields)
#define getmethodtable(ins) (&(ins)->methods)


/* raw index ('[]') access */
cr_ubyte cr_ob_rawindex(VM *vm, Value instance, cr_ubyte get);


/* debug only, prints object type name */
void otypeprint(OType type);

/* Convert object to string object. */
OString *cr_ob_tostr(VM *vm, GCObject *o, cr_ubyte raw);


/* Tries calling binary or unary operator overload method. */
void cr_ob_tryop(VM *vm, Value a, Value b, int op, Value *res);


/* Prints the object value; can call __display__ if 'raw' is 0. */
void oprint(VM *vm, Value value, cr_ubyte raw, FILE *stream);


/* ordering */
#define ordop(name) void name(VM *vm, Value l, Value r)
ordop(oeq);
ordop(one);
ordop(olt);
ordop(ogt);
ordop(ole);
ordop(oge);


/* Free object memory */
void cr_ob_free(VM *vm, GCObject *object);

/* ------------------------------------------------------ */ // object functions




typedef struct {
	int32_t arity;
	int32_t retcnt;
} Tuple;

/* Array holding return count and arity for each overload-able method. */
extern const Tuple ominfo[CR_MN];


#endif
