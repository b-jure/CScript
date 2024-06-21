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
#include "crlimits.h"
#include "crmem.h"
#include "crobject.h"
#include "crvalue.h"
#include "crvm.h"
#include "stdarg.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>



/* allocate new gc object */
#define otnewgco(vm,e,tt,t)	((t*)otallocate(vm, (e) + sizeof(t), tt))


const struct Tuple vtmethodinfo[CR_MNUM] = {
	{ 0, 1 }, /* __init__		{ args: self             - return: instance }  */
	{ 0, 1 }, /* __tostring__ 	{ args: self             - return: string   }  */
	{ 1, 1 }, /* __getidx__   	{ args: self, idx        - return: value    }  */
	{ 2, 0 }, /* __setidx__   	{ args: self, idx, value - return: none     }  */
	{ 1, 0 }, /* __gc__		{ args: self		 - return: none     }  */
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

	bufflen = CRI_MAXSRC - 1;
	if (bufflen < len) {
		memcpy(dest, src, bufflen - SLL("..."));
		memcpy(dest, "...", SLL("..."));
	} else {
		memcpy(dest, src, bufflen);
	}
	dest[bufflen] = '\0';
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
static GCObject *otallocate(VM *vm, size_t size, cr_ubyte ott)
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


/* auxiliary function for allocating string memory */
cr_sinline OString *allocstring(VM *vm, uint32_t len)
{
	OString *s;

	s = otnewgco(vm, len + 1, CR_VSTRING, OString);
	s->len = len;
	return s;
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
	string->extra = 0;
	string->bits = STRhashash;
	lmarkgco(string);
	setv2s(vm, &key, string);
	cr_ht_set(vm, wtab, &key, &vm->nil);
	lunmarkgco(string);
	return string;
}



/* --------------------------------------------------------------------------
 * String conversion
 * -------------------------------------------------------------------------- */

/* maximum value for last integer digit */
#define MAXINTLASTDIG		(CR_INTEGER_MAX % 10)

/* 
 * Check if integer 'i' overflows limit 'l' or in case
 * 'i' is equal to 'l' check if digit 'd' would overflow.
 */
#define ioverflow(i,d,l) \
	((i) >= (l) && ((i) > (l) || (d) > MAXINTLASTDIG))


/* decimal overflow */
#define decoverflow(i,d)	ioverflow(i, d, (CR_INTEGER_MAX / 10))

/* octal overflow */
#define octoverflow(i,d)	ioverflow(i, d, (CR_INTEGER_MAX / 8))

/* hexadecimal overflow */
#define hexoverflow(i,d)	ioverflow(i, d, (CR_INTEGER_MAX / 16))



/* convert hex character into digit */
int cr_ot_hexvalue(int c)
{
	cr_assert(isxdigit(c));
	if (isdigit(c)) return c - '0';
	else return (tolower(c) - 'a') + 10;
}


/*
 * Convert string to Cript integer.
 * This function can convert hexadecimal, octal
 * and decimal strings to 'cr_integer'.
 */
static const char *otstr2int(const char *s, cr_integer *i, int *overflow)
{
	cr_uinteger u;
	int novalue;
	int digit;
	int sign;

	sign = novalue = 1;
	while (isspace(*s)) s++; /* skip leading spaces */
	if (*s == '-' || *s == '+') {
		sign -= 2 * (*s == '-'); 
		s++;
	}
	if (*s == '0' && (*s == 'x' || *s == 'X')) { /* hex ? */
		s+=2; /* skip hex prefix */
		for (; isxdigit(*s); s++) {
			digit = cr_ot_hexvalue(*s);
			if (hexoverflow(u, digit)) {
				if (overflow)
					*overflow = 1;
				return NULL;
			}
			u = u * 16 + digit;
			novalue = 0;
		}
	} else if (*s == '0' && isodigit(s[1])) { /* octal ? */
		s++; /* skip '0' */
		do {
			digit = *s - '0';
			if (octoverflow(u, digit)) {
				if (overflow)
					*overflow = 1;
				return NULL;
			}
			u = u * 8 + digit;
			novalue = 0;
		} while (isodigit(*++s));
	} else { /* decimal */
		for (; isdigit(*s); s++) {
			digit = *s - '0';
			if (decoverflow(u, digit)) {
				if (overflow)
					*overflow = 1;
				return NULL;
			}
			u = u * 10 + digit;
			novalue = 0;
		}
	}
	while (isspace(*s)) s++; /* skip trailing spaces */
	if (novalue || *s != '\0') return NULL;
	*i = cri_castU2S(u*sign);
	return s;
}


static const char *otstr2flt(const char *s, cr_number *n, int *of)
{
	char *eptr;

	*of = 0;
	if (*s == '0'  && (s[1] == 'x' || s[1] == 'X'))
		*n = cr_xstr2number(s, &eptr);
	else
		*n = cr_str2number(s, &eptr);
	if (of) { /* set underflow flag */
		if (strx2numberovf(*n)) *n = 1;
		else if (strx2numberunf(*n)) *n = -1;
	}
	if (eptr == s) return NULL;
	while (isspace(*eptr)) eptr++;
	return (*eptr == '\0' ? eptr : NULL);
}


/* convert string to 'cr_number' or 'cr_integer' */
size_t cr_ot_strtonum(const char *s, TValue *o, int *of)
{
	cr_integer i;
	cr_number n;
	const char *e;
	int iof;

	if (of) *of = iof = 0;
	if ((e = otstr2int(s, &i, &iof)) != NULL) {
		setivalue(o, i);
	} else if ((e = otstr2flt(s, &n, of)) != NULL) {
		setfvalue(o, n);
	} else { /* both conversions failed */
		if (of && !*of) *of = iof;
		return 0;
	}
	return (e - s) + 1;
}


/*
 * Maximum conversion length of a number to a string.
 * 'long double' (not supported currently) can be 33 digits
 * + sign + decimal point + exponent sign + 5 exponent digits
 * + null terminator (43 total).
 * All other types require less space.
 */
#define MAXNUM2STR	44

static int otnum2buff(const TValue *nv, char *buff)
{
	int len;

	cr_assert(ttisnum(nv));
	if (ttisint(nv)) {
		len = cr_integer2str(buff, MAXNUM2STR, ivalue(nv));
	} else {
		len = cr_number2str(buff, MAXNUM2STR, fvalue(nv));
		/* if it looks like integer append '.0' */
		if (strspn(buff, "-0123456789") == len) {
			buff[len++] = *localeconv()->decimal_point;
			buff[len++] = '0';
		}
	}
	return len;
}


void cr_ot_numtostring(VM *vm, TValue *v)
{
	char buff[MAXNUM2STR];
	int len;

	len = otnum2buff(v, buff);
	setv2s(vm, v, cr_ot_newstring(vm, buff, len));
}



/* --------------------------------------------------------------------------
 * String format
 * -------------------------------------------------------------------------- */

/*
 * Initial size of buffer used in 'cr_ot_newvstringf'
 * to prevent allocations, instead the function
 * will directly work on the buffer and will push
 * strings on stack in case buffer exceeds this limit.
 * This is all done because 'cr_ot_newvstringf' often
 * gets called by 'cr_dg_getinfo'; the size should be
 * at least 'CR_MAXSRC' + 'MAXNUM2STR' + size for message.
 */
#define BUFFVFSSIZ	(CRI_MAXSRC + MAXNUM2STR + 100)

/* buffer for 'cr_ot_newvstringf' */
typedef struct BuffVSF {
	VM *vm;
	int pushed; /* true if 'space' was pushed on the stack */
	int len; /* string length in 'space' */
	char space[BUFFVFSSIZ];
} BuffVFS;


/* 
 * Pushes 'str' to the stack and concatenates it with
 * other string on the stack if 'pushed' is set.
 */
static void pushstr(BuffVFS *buff, const char *str, size_t len)
{
	VM *vm;
	OString *s;

	vm = buff->vm;
	s = cr_ot_newstring(vm, str, len);
	setsv2s(vm, vm->stacktop.p, s); 
	vm->stacktop.p++;
	if (buff->pushed)
		cr_vm_concat(vm, 2);
	else
		buff->pushed = 1;
}


/* pushes buffer 'space' on the stack */
static void pushbuff(BuffVFS *buff)
{
	pushstr(buff, buff->space, buff->len);
	buff->len = 0;
}


/* ensure up to buffer space (up to 'BUFFVSFSIZ') */
static char *getbuff(BuffVFS *buff, int n)
{
	cr_assert(n <= BUFFVSFSIZ);
	if (n > BUFFVFSSIZ - buff->len)
		pushbuff(buff);
	return buff->space + buff->len;
}


/* add string to buffer */
static void buffaddstring(BuffVFS *buff, const char *str, size_t len)
{
	char *p;

	if (len < BUFFVFSSIZ) {
		p = getbuff(buff, len);
		memcpy(p, str, len);
		buff->len += cast_int(len);
	} else {
		pushbuff(buff);
		pushstr(buff, str, len);
	}
}


/* add number to buffer */
static void buffaddnum(BuffVFS *buff, const TValue *nv)
{
	buff->len += otnum2buff(nv, getbuff(buff, MAXNUM2STR));
}


/* add pointer to buffer */
static void buffaddptr(BuffVFS *buff, const void *p)
{
	const int psize = 3 * sizeof(void*) + 8;
	buff->len += cr_pointer2str(getbuff(buff, psize), psize, p);
}


/* Create new string object from format 'fmt' and args in 'argp'. */
const char *cr_ot_pushvfstring(VM *vm, const char *fmt, va_list argp)
{
	const char *end;
	const char *str;
	BuffVFS buff;
	TValue nv;
	char c;

	while ((end = strchr(fmt, '%')) != NULL) {
		buffaddstring(&buff, fmt, end - fmt);
		switch (*(end + 1)) {
			case 'c': /* 'char' */
				c = cast(unsigned char, va_arg(argp, int));
				buffaddstring(&buff, &c, sizeof(c));
				break;
			case 'd': /* 'int' */
				setivalue(&nv, va_arg(argp, int));
				buffaddnum(&buff, &nv);
				break;
			case 'I': /* 'cr_integer' */
				setivalue(&nv, va_arg(argp, cr_integer));
				buffaddnum(&buff, &nv);
				break;
			case 'N': /* 'cr_number' */
				setivalue(&nv, va_arg(argp, cr_number));
				buffaddnum(&buff, &nv);
				break;
			case 's': /* 'string' */
				str = va_arg(argp, const char *);
				if (str == NULL) str = "(null)";
				buffaddstring(&buff, str, strlen(str));
				break;
			case 'p': /* 'ptr' */
				buffaddptr(&buff, va_arg(argp, const void *));
				break;
			case '%':
				buffaddstring(&buff, "%", 1);
				break;
			default:
				c = cast(unsigned char, *(end + 1));
				cr_assert(0 && "invalid format specifier '%%%c'");
		}
		fmt = end + 2; /* '%' + specifier */
	}
	buffaddstring(&buff, fmt, strlen(fmt));
	pushbuff(&buff);
	return cstrvalue(s2v(vm->stacktop.p));
}


const char *cr_ot_pushfstring(VM *vm, const char *fmt, ...)
{
	const char *str;
	va_list argp;

	va_start(argp, fmt);
	str = cr_ot_pushvfstring(vm, fmt, argp);
	va_end(argp);
	return str;
}



/* --------------------------------------------------------------------------
 * Other object constructors
 * -------------------------------------------------------------------------- */

CClosure *cr_ot_newcclosure(VM *vm, cr_cfunc fn, int nupvalues)
{
	CClosure *ccl;
	int i;

	ccl = otnewgco(vm, nupvalues * sizeof(TValue), CR_VCCL, CClosure);
	ccl->nupvalues = nupvalues;
	ccl->fn = fn;
	for (i = 0; i < nupvalues; i++)
		ccl->upvalue[i] = newnilvalue();
	return ccl;
}


Function *cr_ot_newfunction(VM *vm)
{
	Function *fn;

	fn = otnewgco(vm, 0, CR_VFUNCTION, Function);
	cr_mm_createvec(vm, &fn->constants, CRI_MAXCODE, "constants");
	cr_mm_createvec(vm, &fn->code, INT_MAX, "code");
	cr_mm_createvec(vm, &fn->lineinfo, INT_MAX, "lines");
	cr_mm_createvec(vm, &fn->lvars, CRI_MAXCODE, "local variables");
	return fn;
}


CriptClosure *cr_ot_newcrclosure(VM *vm, Function *fn, int nupvalues)
{
	CriptClosure *crcl;
	int i;

	crcl = otnewgco(vm, sizeof(UValue*) * nupvalues, CR_VCRCL, CriptClosure);
	crcl->nupvalues = nupvalues;
	crcl->fn = fn;
	memset(crcl->upvalue, 0, nupvalues * sizeof(UValue*));
	return crcl;
}


UValue *cr_ot_newuvalue(VM *vm, TValue *vp)
{
	UValue *uv;

	uv = otnewgco(vm, 0, CR_VUVALUE, UValue);
	uv->closed = newemptyvalue();
	uv->v.location = vp;
	uv->nextuv = NULL;
	return uv;
}


OClass *cr_ot_newclass(VM *vm, OString *id)
{
	OClass *cls;

	cls = otnewgco(vm, 0, CR_VCLASS, OClass);
	cls->name = id;
	cr_ht_init(&cls->mtab);
	memset(cls->vtable, 0, sizeof(cls->vtable) / sizeof(cls->vtable[0]));
	return cls;
}


Instance *cr_ot_newinstance(VM *vm, OClass *cls)
{
	Instance *ins;

	ins = otnewgco(vm, 0, CR_VINSTANCE, Instance);
	ins->oclass = cls;
	cr_ht_init(&ins->fields);
	return ins;
}


InstanceMethod *cr_ot_newinstancemethod(VM *vm, Instance *receiver, CriptClosure *method)
{
	InstanceMethod *im;

	im = otnewgco(vm, 0, CR_VMETHOD, InstanceMethod);
	im->receiver = receiver;
	im->method = cast(GCObject*, method);
	return im;
}



/* --------------------------------------------------------------------------
 * Free objects
 * -------------------------------------------------------------------------- */

/*
 * Performs raw deallocation of object memory it does
 * not try to call '__free__'.
 */
void cr_ot_free(VM *vm, GCObject *o)
{
	Function *fn; OClass *cls; Instance *ins;
#ifdef DEBUG_LOG_GC
	printf("%p freed\n", (void *)o);
#endif
	switch (rawott(o)) {
		case CR_VSTRING:
			cr_mm_free(vm, o, sizes((OString*)o));
			break;
		case CR_VFUNCTION:
			fn = cast(Function*, o);
			cr_mm_freevec(vm, &fn->constants);
			cr_mm_freevec(vm, &fn->code);
			cr_mm_freevec(vm, &fn->lineinfo);
			cr_mm_freevec(vm, &fn->lvars);
			cr_mm_free(vm, o, sizefn());
			break;
		case CR_VUVALUE:
			cr_mm_free(vm, o, sizeuv());
			break;
		case CR_VCRCL:
			cr_mm_free(vm, o, sizecrcl(cast(CriptClosure*, o)));
			break;
		case CR_VCCL:
			cr_mm_free(vm, o, sizeccl(cast(CClosure*, o)));
			break;
		case CR_VCLASS:
			cls = cast(OClass*, o);
			cr_ht_free(vm, &cls->mtab);
			cr_mm_free(vm, o, sizecls());
			break;
		case CR_VINSTANCE:
			ins = cast(Instance*, o);
			cr_ht_free(vm, &ins->fields);
			cr_mm_free(vm, o, sizeins());
			break;
		case CR_VMETHOD:
			cr_mm_free(vm, o, sizeim());
			break;
		default:
			cr_unreachable();
	}
}
