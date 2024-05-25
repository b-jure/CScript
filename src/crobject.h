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
	OBJ_CLOSURE,
	OBJ_NATIVE,
	OBJ_UPVAL,
	OBJ_CLASS,
	OBJ_INSTANCE,
	OBJ_BOUND_METHOD,
} OType;


#define ObjectHeader	struct O* next; cr_ubyte otype; cr_ubyte marked


/* every object type contains this type */
typedef struct O {
	ObjectHeader;
} O;



/* object manipulation macros */
#define otypeset(o, t)	(o)->header = ((o)->header & 0x00ffffffffffffff) | (cast(t, cr_uintptr) << 56)
#define otype(o)       	((OType)(((o)->header >> 56) & 0xff))
#define oismarked(o)   	cast(cr_ubyte, ((o)->header >> 48) & 0x01)
#define osetmark(o, m) 	(((o)->header & 0xff00ffffffffffff) | (cast(cr_uintptr, (m)) << 48))
#define onext(o)       	cast(O *, (o)->header & 0x0000ffffffffffff)
#define osetnext(o, n) 	(o)->header = ((o)->header & 0xffff000000000000) | cast(cr_uintptr, next)
#define oistype(o, t)  	(otype(o) == (t))


/* get object type of value */
#define OBJ_TYPE(v)	(otype(AS_OBJ(v)))


/* check if value is object and it's object type matches @t */
#define isot(v, t)	(IS_OBJ(v) && oistype(AS_OBJ(v), t))


/* miscellaneous object macros */
#define IS_STRING(v)		isot(v, OBJ_STRING)
#define AS_STRING(v)	   	((OString *)AS_OBJ(v))
#define AS_CSTRING(v)	   	(((OString *)AS_OBJ(v))->bytes)
#define IS_FUNCTION(v)	   	isot(v, OBJ_FUNCTION)
#define AS_FUNCTION(v)	   	((OFunction *)AS_OBJ(v))
#define IS_NATIVE(v)	   	isot(v, OBJ_NATIVE)
#define AS_NATIVE(v)	   	((ONative *)AS_OBJ(v))
#define IS_CLOSURE(v)	   	isot(v, OBJ_CLOSURE)
#define AS_CLOSURE(v)	   	((OClosure *)AS_OBJ(v))
#define IS_UPVAL(v)	   	isot(v, OBJ_UPVAL)
#define AS_UPVAL(v)	   	((OUpvalue *)AS_OBJ(v))
#define IS_CLASS(v)	   	isot(v, OBJ_CLASS)
#define AS_CLASS(v)	   	((OClass *)AS_OBJ(v))
#define IS_INSTANCE(v)	   	(isot(v, OBJ_INSTANCE))
#define AS_INSTANCE(v)	   	((OInstance *)AS_OBJ(v))
#define IS_BOUND_METHOD(v) 	isot(v, OBJ_BOUND_METHOD)
#define AS_BOUND_METHOD(v) 	((OBoundMethod *)AS_OBJ(v))


/* check if value is 'falsey' */
#define ISFALSEY(v)	(IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v)))


/* set 'strp' to cstring if 'v' is string object value */
#define tostring(v, strp) 	(IS_STRING(v) ? (*(strp) = AS_CSTRING(v), 1) : 0)


/*
 * Heap allocated bytes that are null-terminated.
 * Additionally 'OString' contains 'hash' for various
 * reasons such as string interning, table lookup and comparisons.
 */
typedef struct {
	O obj;
	cr_uint len; /* excluding null terminator */
	cr_hash hash;
	char bytes[];
} OString;


/*
 * Captured local variable.
 * 'location' contains pointer to the local variable.
 * That local variable can be captured - in that case
 * the variable is copied over to the 'closed' field -
 * or it can live on stack - meaning the local variable
 * won't be stored in 'closed' - so the 'location'
 * points to the stack instead of 'closed'.
 * 'OUpvalue' is also an implicit linked list, it stores
 * the pointer to the 'next' 'OUpvalue'.
 */
typedef struct OUpvalue OUpvalue;
struct OUpvalue {
	O obj;
	Value closed;
	Value *location;
	OUpvalue *next; /* list */
};


/*
 * Both 'OFunction' (cript function) and 'ONative' (C function)
 * contain this structure.
 * It contains both debug information and information that is
 * checked at runtime to ensure function signature is upheld.
 */
typedef struct {
	OString *name; /* function name (declaration or userdata) */
	OString *source; /* source name (script name or 'name') */
	int32_t defline; /* function definition start or -1 */
	int32_t deflastline; /* function definition end or -1 */
	int32_t arity;
	cr_uint upvalcnt;
	cr_ubyte isvararg; /* '...' */
} FnInfo;

/*
 * cript function which always gets wrapped in 'OClosure'.
 * It contains it's own 'Chunk' which contains the compiled bytecode
 * together with the constants contained in that 'Chunk'.
 */
typedef struct {
	O o;
	FnInfo p;
	Chunk chunk;
	cr_ubyte gotret : 1; // TODO: maybe_remove
} OFunction;

/*
 * Wrapper around C function.
 * Because the body of this function is pure C code
 * it doesn't require it's own 'Chunk' where it would
 * store the constants and its instructions.
 * Additionally because of the lack of the 'Chunk' any
 * upvalues get stored inside the 'upvalue' array.
 * The lenght of that array is stored inside the 'FnInfo'
 * structure.
 */
typedef struct {
	O obj;
	FnInfo p;
	cr_cfunc fn; /* C function */
	Value upvalue[1]; /* upvalue storage */
} ONative;

/*
 * This is a function closure, a wrapper around the 'OFunction'.
 * What this means is that each 'OFunction' that accesses a
 * local variable outside of its scope - this would mean that
 * the local variable being accessed is located in the scope
 * beyond (before) current function scope - must keep it's own
 * copy/reference of that variable.
 *
 * The reason why is because 'OFunction' is an object 'Value'.
 * This means that all the stuff you do with other 'Value's
 * can also be done on 'OFunction's.
 * If you would like to use a local variable defined before
 * the current function scope and later return the function,
 * you would need to copy over (capture) that local variable
 * because the variable might get out of scope.
 * This is what the 'upvalue' array is used for, it stores
 * references to the 'OUpvalues's, which are captured local
 * variables.
 */
typedef struct {
	O obj;
	OFunction *fn;
	OUpvalue **upvalue; /* array of 'OUpvalue' references */
} OClosure;

/*
 * Class objects are nothing more than named objects
 * with their own methods (functions).
 * Each method is made of 'OString' (name) and the
 * 'OClosure' (function).
 * Additionally because cript allows overloading
 * operators, class objects also contain 'omethods' field.
 * That field is a storage for all the overloadable methods.
 */
typedef struct {
	O obj;
	OString *name; /* class name */
	HashTable methods; /* name/function pairs ('OString'/'OClosure') */
	O *omethods[CR_OM_CNT]; /* overloaded methods ('ONative' or 'OClosure') */
} OClass;

/*
 * This is instance of 'OClass'.
 * It additionally contains its own fields and pointer
 * to the class that it was created from.
 * These fields are private to the instance object and
 * are not to be confused with methods defined in 'OClass'.
 * Pointer to the class is kept for inheritance purposes.
 */
typedef struct {
	O obj;
	OClass *oclass; /* pointer to class */
	HashTable fields; /* instance fields */
} OInstance;

/*
 * This is wrapper around a method of a particular 'OInstance'.
 * The wrapping happens in case the method of the instance gets
 * bound to a variable.
 * Then the method must have some kind of way to find it's
 * 'OInstance' to properly push that instance on stack
 * before the calls to 'self'/'super'.
 */
typedef struct {
	O obj;
	Value receiver; /* pointer to OInstance */
	OClosure *method; /* actual method */
} OBoundMethod;

/* --------------------------------------------------------- */ // objects




/*
 * ================== Object functions ==================
 */

int32_t id2omtag(VM *vm, OString *id);


// clang-format off
/* object constructors */
OString *OString_new(VM *vm, const char *chars, size_t len);
#define OString_newlit(vm, lit) OString_new(vm, lit, SLL(lit))
OString *OString_fmt_from(VM *vm, const char *fmt, va_list argp);
OString *OString_fmt(VM *vm, const char *fmt, ...);
OString *concatenate(VM *vm, O* a, O* b);
OString *unescape(VM *vm, OString *string);
OBoundMethod *OBoundMethod_new(VM *vm, Value receiver, OClosure *method);
OInstance *OInstance_new(VM *vm, OClass *cclass);
OClass *OClass_new(VM *vm, OString *name);
OUpvalue *OUpvalue_new(VM *vm, Value *var_ref);
OClosure *OClosure_new(VM *vm, OFunction *fn);
ONative *ONative_new(VM *vm, OString *name, cr_cfunc fn, int32_t arity, cr_ubyte isvararg, cr_uint upvals);
OFunction *OFunction_new(VM *vm);
// clang-format on


/* call ('()') overload-able method */
cr_ubyte calloverload(VM *vm, Value instance, cr_om tag);


/* get 'OInstance' tables */
#define getfieldtable(ins)  (&(ins)->fields)
#define getmethodtable(ins) (&(ins)->methods)


/* raw index ('[]') access */
cr_ubyte rawindex(VM *vm, Value instance, cr_ubyte get);


/* debug only, prints object type name */
void otypeprint(OType type);

/* Convert object to string object. */
OString *otostr(VM *vm, O *o, cr_ubyte raw);


/* Tries calling binary or unary operator overload method. */
void otryop(VM *vm, Value a, Value b, cr_om op, Value *res);


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


/* Hashes the object value, can call __hash__ if 'raw' is 0. */
cr_hash ohash(VM *vm, Value value, cr_ubyte raw);


/* Free object memory */
void ofree(VM *vm, O *object);

/* ------------------------------------------------------ */ // object functions




typedef struct {
	int32_t arity;
	int32_t retcnt;
} Tuple;

/* Array holding return count and arity for each overload-able method. */
extern const Tuple ominfo[CR_OM_CNT];


#endif
