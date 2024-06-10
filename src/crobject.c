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

#include "crconf.h"
#include "crhashtable.h"
#include "crmem.h"
#include "crobject.h"
#include "crvalue.h"
#include "crvm.h"
#include "stdarg.h"

#include <stdio.h>
#include <stdlib.h>




/* allocate 'GCObject' */
#define allocgco(vm,s,tt,t) \
	((t *)(allocobject((vm), (s) + sizeof(t), (tt))))



// @TODO: Update 'calloverload()'
struct Tuple {
	int arity;
	int nreturns;
} ominfo[] = {
	/* {arity, nreturns} */
	{ 0, 1 }, /* __init__		{ args: self             - return: instance }  */
	{ 0, 1 }, /* __tostring__ 	{ args: self             - return: string   }  */
	{ 1, 1 }, /* __getidx__   	{ args: self, idx        - return: value    }  */
	{ 2, 0 }, /* __setidx__   	{ args: self, idx, value - return: none     }  */
	{ 0, 1 }, /* __hash__     	{ args: self             - return: number   }  */
	{ 0, 0 }, /* __free__     	{ args: self             - return: none     }  */
	{ 2, 1 }, /* __add__		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __sub__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __mul__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __div__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __mod__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __pow__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 1, 1 }, /* __not__  		{ args: self, lhs        - return: value    }  */
	{ 1, 1 }, /* __umin__ 		{ args: self, lhs        - return: value    }  */
	{ 2, 1 }, /* __ne__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __eq__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __lt__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __le__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __gt__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __ge__   		{ args: self, lhs, rhs   - return: value    }  */
};


void cr_ot_sourceid(char *restrict dest, const char *src, size_t len)
{
	size_t bufflen;

	bufflen = CR_MAXSRC - 1;
	if (bufflen < len) {
		memcpy(dest, src, bufflen - SLL("..."));
		memcpy(dest, "...", SLL("..."));
	} else {
		memcpy(dest, src, bufflen);
	}
	dest[bufflen] = '\0';
}

const char *cr_ot_pushfstring(VM *vm, const char *fmt, ...)
{
}


/*
 * Convert string into 'cr_vtable' index.
 * Returns '-1' in case string is not the name of any methods
 * inside the 'cr_vtable'.
 */
int cr_ot_strtomt(VM *vm, OString *id)
{
	uintptr_t ptr, start, end;

	ptr = cast(uintptr_t, id);
	start = cast(uintptr_t, vm->faststatic[SS_INIT]);
	end = cast(uintptr_t, vm->faststatic[SS_GE + 1]);
	return (ptr < start || ptr >= end ? -1 : ptr - start);
}


/* Create new unmarked 'GCObject' (object) and append it to GC list. */
cr_sinline GCObject *allocobject(VM *vm, size_t size, cr_ubyte ott)
{
	GCObject *o;

	o = cr_mm_malloc(vm, size);
	o->ott = ott;
	unmarkgco(o);
	o->next = vm->gc.list;
	vm->gc.list = o;
#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu\n", (void *)object, size);
#endif
	return o;
}


/* auxiliary function for 'OString' constructors */
cr_sinline OString *allocstring(VM *vm, uint32_t len)
{
	OString *string;

	string = allocgco(vm, len + 1, CR_VSTRING, OString);
	string->len = len;
	return string;
}

/*
 * Create new string object.
 * Allocation is skipped in case string is already interned.
 */
OString *cr_ot_newstring(VM *vm, const char *chars, size_t len)
{
	HTable *wtab;
	OString *interned;
	OString *string;
	unsigned int hash;
	TValue key;

	wtab = &vm->weakrefs;
	hash = cr_hh_string(chars, len, vm->seed);
	interned = cr_ht_getinterned(wtab, chars, len, hash);
	if (interned)
		return interned;
	string = allocstring(vm, len);
	if (len != 0)
		memcpy(string->bytes, chars, len);
	string->bytes[len] = '\0';
	string->hash = hash;
	string->hashash = 1;
	lmarkgco(string);
	key = newovalue(string);
	cr_ht_set(vm, wtab, &key, &vm->nil);
	lunmarkgco(string);
	return string;
}

/*
 * Maximum conversion length of a number to a string.
 * 'long double' (not supported currently) can be 33 digits
 * + sign + decimal point + exponent sign + 5 exponent digits
 * + null terminator (43 total).
 * All other types require less space.
 */
#define MAXNUM2STR	44


/*
 * Initial size of buffer used in 'cr_ot_newvstringf'
 * to prevent allocations, instead the function
 * will directly work on the buffer and will push
 * strings on stack in case buffer exceeds this limit.
 * This is all done because 'cr_ot_newvstringf' often
 * gets called by 'cr_dg_getinfo'; the size should be
 * at least 'CR_MAXSRC' + 'MAXNUM2STR' + size for message.
 */
#define BUFFVSFSIZ	(CR_MAXSRC + MAXNUM2STR + 100)

/* buffer for 'cr_ot_newvstringf' */
typedef struct BuffVSF {
	VM *vm;
	int pushed; /* true if 'space' was pushed on the stack */
	int len; /* string length in 'space' */
	char space[BUFFVSFSIZ];
} BuffVSF;


/* 
 * Pushes 'str' to the stack and concatenates it with
 * the previous pushed string (depending on buffer 'pushed').
 */
static void pushstring(BuffVSF *buff, const char *str, size_t len)
{

}


/* pushes buffer 'space' on the stack. */
static void pushbuff(BuffVSF *buff, const char *str, size_t len)
{
}


static void ensurebuff(BuffVSF *buff, const char *str, size_t len)
{
	// TODO
}


/* Create new string object from format 'fmt' and args in 'argp'. */
OString *cr_ot_newvstringf(VM *vm, const char *fmt, va_list argp)
{
	BuffVSF buff;
	unsigned char c;
	cr_number f;
	cr_integer i;
	const char *end;
	const void *p;
	OString *s;

	while ((end = strchr(fmt, '%')) != NULL) {
		Array_char_push_multiple(&buff, fmt, end - fmt);

		switch (*(end + 1)) {
		case 'c': /* 'char' */
			c = va_arg(argp, int);
			Array_char_push(&buff, c);
			break;
		case 'n': /* 'cr_integer' */
			integer = va_arg(argp, cr_ssize);
			a_arr_char_push_number(buffer, n);
			break;
		case 'f': /* 'cr_floating' */
			f = va_arg(argp, double);
			a_arr_char_push_double(buffer, f);
			break;
		case 's': /* 'string' */
			s = va_arg(argp, const char *);
			a_arr_char_push_str(buffer, s, strlen(s));
			break;
		case 'p': /* 'ptr' */
			p = va_arg(argp, const void *);
			a_arr_char_push_ptr(buffer, p);
			break;
		case '%':
			a_arr_char_push(buffer, '%');
			break;
		default:
			c = *(end + 1);
			ashe_panicf("invalid format specifier '%%%c'", c);
			/* UNREACHED */
		}
		fmt = end + 2; /* % + specifier */
	}

	a_arr_char_push_str(buffer, fmt, strlen(fmt));
}
	Array_char_init(&buff, vm);
	for (;;) {
		while ((c = *fmt++) != '%')
			Array_char_push(&buff, c);
		c = *fmt++;
		switch (c) {
		case '%': {
			Array_char_push(&buff, '%');
			break;
		}
		case 'd': /* cr_lint */
		{
			cr_lint n = va_arg(argp, cr_lint);
			Array_char_ensure(&buff, CR_NDIGITS);
			buff.len += snprintf((char *)&buff.data[buff.len], CR_NDIGITS, "%ld", n);
			break;
		}
		case 'n': /* cr_double (as double) */
		{
			fmttype = "%g";
			goto l_sknum;
		}
		case 'f': /* cr_double (as float) */
		{
			fmttype = "%f";
			goto l_sknum;
		}
l_sknum: {
	cr_double n = va_arg(argp, cr_double);
	Array_char_ensure(&buff, CR_NDIGITS);
	char *s = cast_charp(&buff.data[buff.len]);
	buff.len += snprintf(s, CR_NDIGITS, fmttype, n);
	break;
}
		case 's': { /* string */
			const char *str = va_arg(argp, char *);
			if (str == NULL)
				str = "(null)";
			size_t len = strlen(str);
			Array_char_ensure(&buff, len);
			memcpy(&buff.data[buff.len], str, len);
			buff.len += len;
			break;
		}
		case 'c': /* char */
		{
			int8_t c = va_arg(argp, int);
			Array_char_push(&buff, c);
			break;
		}
		case 'p': /* pointer */
		{
			Array_char_ensure(&buff, 11);
			void *ptr = va_arg(argp, void *);
			snprintf((char *)&buff.data[buff.len], 11, "%p", ptr);
			break;
		}
		default: /* invalid format specifier */
		{
			Array_char_free(&buff, NULL);
			ofmterror(vm, c, *last_frame(vm).callee);
		}
		}
	}
	fstr = OString_new(vm, (char *)buff.data, buff.len);
	Array_char_free(&buff, NULL);
	return fstr;
}

OString *OString_fmt(VM *vm, const char *fmt, ...)
{
	va_list argp;
	va_start(argp, fmt);
	OString *str = OString_fmt_from(vm, fmt, argp);
	va_end(argp);
	return str;
}

cr_sinline void OString_free(VM *vm, OString *string)
{
	GC_FREE(vm, string, sizeof(OString) + string->len + 1);
}


OString *concatenate(VM *vm, Value l, Value r)
{
	Buffer buf;
	OString *ls, *rs, *str;
	cr_mem length;

	cr_assert(vm, (isstring(l) && isstring(r)), "expect strings");
	ls = asstring(l);
	rs = asstring(r);
	length = ls->len + rs->len;
	buf_init_cap(&buf, vm, length);
	buf_push_str(&buf, ls->bytes, ls->len);
	buf_push_str(&buf, rs->bytes, rs->len);
	str = OString_new(vm, buffer, length);
	buf_free(&buf);
	return str;
}


ONative *ONative_new(VM *vm, OString *name, cr_cfunc fn, int arity, cr_ubyte isvararg,
		     uint32_t upvalc)
{
	ONative *native = ALLOC_NATIVE(vm, upvalc);
	native->fn = fn;
	FnInfo *p = &native->p;
	p->name = (name ? name : vm->faststatic[SS_UNKNOWN]);
	p->source = vm->faststatic[SS_CSRC];
	p->arity = arity;
	p->isvararg = isvararg;
	p->upvalc = upvalc;
	p->defline = -1;
	p->deflastline = -1;
	for (int i = 0; i < upvalc; i++)
		native->upvalue[i] = NIL_VAL;
	return native;
}

cr_sinline void ONative_free(VM *vm, ONative *native)
{
	GC_FREE(vm, native, sizeof(ONative) + (native->p.upvalc * sizeof(Value)));
}




OFunction *OFunction_new(VM *vm)
{
	OFunction *fn = ALLOC_OBJ(vm, OFunction, OBJ_FUNCTION);
	FnInfo *p = &fn->p;
	memset(p, 0, sizeof(FnInfo));
	initchunk(&fn->chunk, vm);
	return fn;
}

cr_sinline void ObjFunction_free(VM *vm, OFunction *fn)
{
	freechunk(&fn->chunk);
	GC_FREE(vm, fn, sizeof(OFunction));
}

cr_sinline void fnprint(OFunction *fn, FILE *stream)
{
	if (cr_unlikely(fn->p.name == NULL))
		fprintf(stream, "<script-fn %s>: %p", fn->p.source->bytes, fn);
	else
		fprintf(stream, "<fn %s>: %p", fn->p.name->bytes, fn);
}




OClosure *OClosure_new(VM *vm, OFunction *fn)
{
	FnInfo *p = &fn->p;
	OUpvalue **upvals = GC_MALLOC(vm, sizeof(OUpvalue *) * p->upvalc);
	for (uint32_t i = 0; i < p->upvalc; i++)
		upvals[i] = NULL;
	OClosure *closure = ALLOC_OBJ(vm, OClosure, OBJ_CLOSURE);
	closure->fn = fn;
	closure->upvalue = upvals;
	return closure;
}

cr_sinline void OClosure_free(VM *vm, OClosure *closure)
{
	GC_FREE(vm, closure->upvalue, closure->fn->p.upvalc * sizeof(OUpvalue *));
	GC_FREE(vm, closure, sizeof(OClosure));
}




OUpvalue *OUpvalue_new(VM *vm, Value *valp)
{
	OUpvalue *upval = ALLOC_OBJ(vm, OUpvalue, OBJ_UPVAL);
	upval->closed = EMPTY_VAL;
	upval->location = valp;
	upval->next = NULL;
	return upval;
}

cr_sinline void OUpvalue_free(VM *vm, OUpvalue *upval)
{
	GC_FREE(vm, upval, sizeof(OUpvalue));
}




OClass *OClass_new(VM *vm, OString *name)
{
	OClass *oclass = ALLOC_OBJ(vm, OClass, OBJ_CLASS);
	oclass->name = name;
	HTable_init(&oclass->methods);
	memset(oclass->omethods, 0, sizeof(oclass->omethods) / sizeof(oclass->omethods[0]));
	return oclass;
}

cr_sinline void OClass_free(VM *vm, OClass *oclass)
{
	HTable_free(vm, &oclass->methods);
	GC_FREE(vm, oclass, sizeof(OClass));
}




OInstance *OInstance_new(VM *vm, OClass *oclass)
{
	OInstance *instance = ALLOC_OBJ(vm, OInstance, OBJ_INSTANCE);
	instance->oclass = oclass;
	HTable_init(&instance->fields);
	return instance;
}

/* Return overloaded method or NULL if value
 * is not an instance value or method is not overloaded. */
cr_sinline GCObject *getomethod(VM *vm, Value val, cr_om om)
{
	if (isinstance(val))
		return asinstance(val)->oclass->omethods[om];
	return NULL;
}


cr_sinline void OInstance_free(VM *vm, OInstance *instance)
{
	HTable_free(vm, &instance->fields);
	GC_FREE(vm, instance, sizeof(OInstance));
}




OBoundMethod *OBoundMethod_new(VM *vm, Value receiver, OClosure *method)
{
	OBoundMethod *bound_method = ALLOC_OBJ(vm, OBoundMethod, OBJ_BOUND_METHOD);
	bound_method->receiver = receiver;
	bound_method->method = method;
	return bound_method;
}

cr_sinline void OBoundMethod_free(VM *vm, OBoundMethod *bound_method)
{
	GC_FREE(vm, bound_method, sizeof(OBoundMethod));
}



/* Free object memory, invokes '__free__' if defined. */
void ofree(VM *vm, GCObject *object)
{
#ifdef DEBUG_LOG_GC
	printf("%p free type ", (void *)object);
	otypeprint(otype(object));
	printf("\n");
#endif
#ifdef CR_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "skjmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch (x)
#define CASE(label) case label:
#define BREAK	    return
#endif
	DISPATCH(otype(object))
	{
		CASE(OBJ_STRING)
		{
			OString_free(vm, cast(OString *, object));
			BREAK;
		}
		CASE(OBJ_FUNCTION)
		{
			ObjFunction_free(vm, cast(OFunction *, object));
			BREAK;
		}
		CASE(OBJ_CLOSURE)
		{
			OClosure_free(vm, cast(OClosure *, object));
			BREAK;
		}
		CASE(OBJ_CFUNCTION)
		{
			ONative_free(vm, cast(ONative *, object));
			BREAK;
		}
		CASE(OBJ_UPVAL)
		{
			OUpvalue_free(vm, cast(OUpvalue *, object));
			BREAK;
		}
		CASE(OBJ_CLASS)
		{
			OClass_free(vm, cast(OClass *, object));
			BREAK;
		}
		CASE(OBJ_INSTANCE)
		{
			OInstance *instance = cast(OInstance *, object);
			calloverload(vm, OBJ_VAL(instance), OM_FREE); // try call '__free__'
			OInstance_free(vm, instance);
			BREAK;
		}
		CASE(OBJ_BOUND_METHOD)
		{
			OBoundMethod_free(vm, cast(OBoundMethod *, object));
			BREAK;
		}
	}
	cr_unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}




/* =============== raw access =============== */

cr_sinline cr_ubyte rawgetproperty(VM *vm, OInstance *instance, Value key, Value *out,
					    cr_ubyte what)
{
	HTable *table = rawgettable(vm, instance, what);
	return rawget(vm, table, key, out);
}

cr_sinline cr_ubyte rawsetproperty(VM *vm, OInstance *instance, Value key, Value value,
					    cr_ubyte what)
{
	HTable *table = rawgettable(vm, instance, what);
	return rawset(vm, table, key, value);
}


/* 
 * Perform raw index access on instance object.
 * 'set' determines if we are getting or setting the indexed value.
 * Indexing with 'nil' value is runtime error.
 */
cr_ubyte rawindex(VM *vm, Value value, cr_ubyte get)
{
	OInstance *instance;
	Value idx, out, rhs;
	Value *idxstk;
	cr_ubyte res;

	res = 0;
	instance = asinstance(value);
	if (!get) { /* set ? */
		rhs = *stkpeek(0);
		idx = *stkpeek(1);
		if (cr_unlikely(IS_NIL(idx)))
			nilidxerror(vm);
		rawsetproperty(vm, instance, idx, rhs, 0);
		popn(vm, 2); // pop [index] and [rhs]
		res = 1;
	} else { /* otherwise get */
		idxstk = stkpeek(0);
		if (cr_unlikely(IS_NIL(idx)))
			nilidxerror(vm);
		if (rawgetproperty(vm, instance, idx, &out, 0) ||
		    rawgetproperty(vm, instance, idx, &out, 1)) {
			*idxstk = out; // replace [index] with property
			res = 1;
		}
	}
	return res;
}

/* ---------------------------------------------------- */




/* =============== call overload-able methods =============== */

cr_ubyte calloverload(VM *vm, Value instance, cr_om tag)
{
	GCObject *const fn;
	Value *const retstart;
	int retcnt, arity, i;

	if (!(fn = getomethod(vm, instance, tag)))
		return 0;
	retstart = vm->sp;
	retcnt = ominfo[tag].retcnt;
	arity = ominfo[tag].arity;
	push(vm, instance); // push 'self'
	for (i = 0; i < arity; i++) // push args
		push(vm, *stkpeek(arity - 1));
	ncall(vm, retstart, OBJ_VAL(fn), retcnt);
	return 1;
}

/* ---------------------------------------------------- */




#if defined(CR_OVERLOAD_OPS)


/* =============== operator overloading =============== */

/* Tries to call class overloaded unary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
cr_sinline int callunop(VM *vm, Value lhs, cr_om op, Value *res)
{
	GCObject *om = getomethod(vm, lhs, op);
	if (om == NULL)
		return 0;
	Value *retstart = vm->sp;
	push(vm, lhs); // 'self'
	push(vm, lhs);
	ncall(vm, retstart, OBJ_VAL(om), ominfo[op].retcnt);
	*res = pop(vm); // assign and pop the method result
	return 1;
}


/* Tries to call class overloaded binary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
cr_sinline int callbinop(VM *vm, Value lhs, Value rhs, cr_om op, Value *res)
{
	Value instance;
	GCObject *om = getomethod(vm, lhs, op);
	if (om == NULL) {
		om = getomethod(vm, rhs, op);
		if (om == NULL)
			return 0;
		instance = rhs;
	} else
		instance = lhs;
	Value *retstart = vm->sp;
	push(vm, instance); // 'self'
	push(vm, lhs);
	push(vm, rhs);
	ncall(vm, retstart, OBJ_VAL(om), ominfo[op].retcnt);
	*res = pop(vm); // assign and pop the method result
	return 1;
}


/* Tries calling binary or unary overloaded operator method, errors on failure. */
void otryop(VM *vm, Value lhs, Value rhs, cr_om op, Value *res)
{
	if (!omisunop(op)) {
		if (cr_unlikely(!callbinop(vm, lhs, rhs, op, res)))
			binoperror(vm, lhs, rhs, op - OM_ADD);
	} else if (cr_unlikely(!callunop(vm, lhs, op, res)))
		unoperror(vm, lhs, op - OM_ADD);
}

/* ---------------------------------------------------- */




/* =============== ordering =============== */

cr_sinline int omcallorder(VM *vm, Value lhs, Value rhs, cr_om ordop)
{
	cr_assert(vm, ordop >= OM_NE && ordop <= OM_GE, "invalid cr_om for order");
	if (callbinop(vm, lhs, rhs, ordop, stkpeek(1))) { // try overload
		pop(vm); // remove second operand
		return 1;
	}
	// Instances (and cript objects) can always have equality comparison.
	// If their pointers are the same then the underlying objects are equal;
	// otherwise they are not equal.
	if (cr_unlikely(ordop != OM_EQ && ordop != OM_NE))
		ordererror(vm, lhs, rhs);
	return 0;
}

/* != */
void one(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(lhs != rhs));
	else if (!omcallorder(vm, lhs, rhs, OM_NE))
		push(vm, BOOL_VAL(lhs != rhs));
}

/* == */
void oeq(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(lhs == rhs));
	else if (!omcallorder(vm, lhs, rhs, OM_EQ))
		push(vm, BOOL_VAL(lhs == rhs));
}

/* < */
void olt(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) < 0));
	else
		omcallorder(vm, lhs, rhs, OM_LT);
}

/* > */
void ogt(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) > 0));
	else
		omcallorder(vm, lhs, rhs, OM_GT);
}

/* <= */
void ole(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) <= 0));
	else
		omcallorder(vm, lhs, rhs, OM_LE);
}

/* >= */
void oge(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) >= 0));
	else
		omcallorder(vm, lhs, rhs, OM_GE);
}

/* ---------------------------------------------------- */

#endif // if defined(CR_OVERLOAD_OPS)




/* 
 * Get 'OString' from object 'o'. 
 * 'raw' indicates raw access in case the object type is 'OBJ_INSTANCE'.
 */
OString *otostr(VM *vm, GCObject *o, cr_ubyte raw)
{
	OInstance *ins;
	Value debug, key, result;

	switch (otype(o)) {
	case OBJ_STRING:
		return cast(OString *, o);
	case OBJ_FUNCTION:
		return cast(OFunction *, o)->p.name;
	case OBJ_CLOSURE:
		return cast(OClosure *, o)->fn->p.name;
	case OBJ_CFUNCTION:
		return cast(ONative *, o)->p->name;
	case OBJ_UPVAL:
		return vtostr(vm, *cast(OUpvalue *, o)->location, raw);
	case OBJ_CLASS:
		return cast(OClass *, o)->name;
	case OBJ_INSTANCE:
		if (!raw && calloverload(vm, OBJ_VAL(o), SKOM_TOSTRING)) {
			result = *stkpeek(0);
			if (!isstring(result))
				omreterror(vm, "string", OM_TOSTRING);
			*o = pop(vm);
			return asstring(result);
		} else {
			ins = asinstance(oval);
			key = OBJ_VAL(vm->faststatic[SS_DBG]);
			if (rawget(vm, &ins->fields, key, &debug) && isstring(debug)) {
				*o = debug;
				return asstring(debug); // have debug name
			}
			return asstring(*o = OBJ_VAL(ins->oclass->name));
		}
	case OBJ_BOUND_METHOD:
		return cast(OBoundMethod *, o)->method->fn->p.name;
	}
}


/* Print the object value */
void oprint(VM *vm, Value value, cr_ubyte raw, FILE *stream)
{
#ifdef CR_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "skjmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch (x)
#define CASE(label) case label:
#define BREAK	    return
#endif
	DISPATCH(OBJ_TYPE(value))
	{
		CASE(OBJ_STRING)
		{
			fprintf(stream, "%s", ascstring(value));
			BREAK;
		}
		CASE(OBJ_FUNCTION)
		{
			fnprint(asfn(value), stream);
			BREAK;
		}
		CASE(OBJ_CLOSURE)
		{
			fnprint(asclosure(value)->fn, stream);
			BREAK;
		}
		CASE(OBJ_CFUNCTION)
		{
			fprintf(stream, "<native-fn %s>", ascfn(value)->p.name->bytes);
			BREAK;
		}
		CASE(OBJ_UPVAL)
		{ // recursion
			OUpvalue *upval = AS_UPVAL(value);
			vprint(vm, *upval->location, raw, stream);
			BREAK;
		}
		CASE(OBJ_CLASS)
		{
			fprintf(stream, "%p-%s", asobj(value), asclass(value)->name->bytes);
			BREAK;
		}
		CASE(OBJ_INSTANCE)
		{
			if (!raw && calloverload(vm, value, OM_TOSTRING)) { // have '__tostring__' ?
				Value result = *stkpeek(0);
				if (!isstring(result))
					omreterror(vm, "string", OM_TOSTRING);
				fprintf(stream, "%s", ascstring(*stkpeek(0)));
				pop(vm); // pop result
			} else { // no overload
				OInstance *instance = asinstance(value);
				Value debug;
				Value key = OBJ_VAL(vm->faststatic[SS_DBG]);
				if (rawget(vm, &instance->fields, key, &debug) &&
				    isstring(debug)) {
					fprintf(stream, "%s", ascstring(debug)); // __debug field
				} else { // default print
					fprintf(stream, "%p-%s instance", asobj(value),
						asinstance(value)->oclass->name->bytes);
				}
			}
			BREAK;
		}
		CASE(OBJ_BOUND_METHOD)
		{
			fnprint(asboundmethod(value)->method->fn, stream);
			BREAK;
		}
	}
	cr_unreachable;
}



