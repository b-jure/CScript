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



void cr_object_sourceid(char *restrict dest, const char *src, size_t len)
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
int cr_object_strtomt(cr_State *ts, OString *id)
{
	OString **names;
	uintptr_t ptr, start;

	names = GS(ts)->vtmnames;
	ptr = cast(uintptr_t, id);
	start = cast(uintptr_t, names);
	return (ptr < start || ptr > start + CR_NUMM ? -1 : ptr - start);
}


/* create new unmarked 'GCObject' (object) and append it to GC list */
GCObject *cr_object_new(cr_State *ts, size_t size, cr_ubyte ott)
{
	GCObject *o;
	GState *gs;

	o = cr_mem_malloc(ts, size);
	o->ott = ott;
	o->mark = 0;
	gs = GS(ts);
	o->next = gs->gc.objects;
	gs->gc.objects = o;
	return o;
}


/*
 * Create new string object.
 * Allocation is skipped in case string is already interned.
 */
OString *cr_object_newstring(cr_State *ts, const char *chars, size_t len)
{
	HTable *strtab;
	OString *weakref;
	OString *string;
	unsigned int hash;
	TValue key;

	strtab = &GS(ts)->strings;
	hash = cr_hh_string(chars, len, GS(ts)->seed);
	weakref = cr_htable_getraw(strtab, chars, len, hash);
	if (weakref)
		return weakref;
	string = newgco(ts, len + 1, CR_VSTRING, OString);
	string->len = len;
	if (len != 0)
		memcpy(string->bytes, chars, len);
	string->bytes[len] = '\0';
	string->hash = hash;
	string->extra = 0;
	string->bits = STRHASHASH;
	setv2s(ts, &key, string);
	setsv2s(ts, ts->stacktop.p++, string);
	cr_htable_set(ts, strtab, &key, &GS(ts)->nil);
	ts->stacktop.p--;
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
int cr_object_hexvalue(int c)
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
			digit = cr_object_hexvalue(*s);
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
size_t cr_object_strtonum(const char *s, TValue *o, int *of)
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


void cr_object_numtostring(cr_State *ts, TValue *v)
{
	char buff[MAXNUM2STR];
	int len;

	len = otnum2buff(v, buff);
	setv2s(ts, v, cr_object_newstring(ts, buff, len));
}



/* --------------------------------------------------------------------------
 * String format
 * -------------------------------------------------------------------------- */

/*
 * Initial size of buffer used in 'cr_object_newvstringf'
 * to prevent allocations, instead the function
 * will directly work on the buffer and will push
 * strings on stack in case buffer exceeds this limit.
 * This is all done because 'cr_object_newvstringf' often
 * gets called by 'cr_dg_getinfo'; the size should be
 * at least 'CR_MAXSRC' + 'MAXNUM2STR' + size for message.
 */
#define BUFFVFSSIZ	(CRI_MAXSRC + MAXNUM2STR + 100)

/* buffer for 'cr_object_newvstringf' */
typedef struct BuffVSF {
	cr_State *ts;
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
	cr_State *ts;
	OString *s;

	ts = buff->ts;
	s = cr_object_newstring(ts, str, len);
	setsv2s(ts, ts->stacktop.p, s); 
	ts->stacktop.p++;
	if (buff->pushed)
		cr_vm_concat(ts, 2);
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
const char *cr_object_pushvfstring(cr_State *ts, const char *fmt, va_list argp)
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
	return cstrvalue(s2v(ts->stacktop.p));
}


const char *cr_object_pushfstring(cr_State *ts, const char *fmt, ...)
{
	const char *str;
	va_list argp;

	va_start(argp, fmt);
	str = cr_object_pushvfstring(ts, fmt, argp);
	va_end(argp);
	return str;
}



/* --------------------------------------------------------------------------
 * Other object constructors
 * -------------------------------------------------------------------------- */

CClosure *cr_object_newcclosure(cr_State *ts, cr_cfunc fn, int nupvalues)
{
	CClosure *ccl;
	int i;

	ccl = newgco(ts, nupvalues * sizeof(TValue), CR_VCCL, CClosure);
	ccl->nupvalues = nupvalues;
	ccl->fn = fn;
	for (i = 0; i < nupvalues; i++)
		setnilvalue(&ccl->upvalue[i]);
	return ccl;
}


Function *cr_object_newfunction(cr_State *ts)
{
	Function *fn;

	fn = newgco(ts, 0, CR_VFUNCTION, Function);
	cr_mem_createvec(ts, &fn->constants, CRI_MAXCODE, "constants");
	cr_mem_createvec(ts, &fn->code, INT_MAX, "code");
	cr_mem_createvec(ts, &fn->lineinfo, INT_MAX, "lines");
	cr_mem_createvec(ts, &fn->lvars, CRI_MAXCODE, "local variables");
	return fn;
}


CriptClosure *cr_object_newcrclosure(cr_State *ts, Function *fn, int nupvalues)
{
	CriptClosure *crcl;
	int i;

	crcl = newgco(ts, sizeof(UValue*) * nupvalues, CR_VCRCL, CriptClosure);
	crcl->nupvalues = nupvalues;
	crcl->fn = fn;
	memset(crcl->upvalue, 0, nupvalues * sizeof(UValue*));
	return crcl;
}


UValue *cr_object_newuvalue(cr_State *ts, TValue *vp)
{
	UValue *uv;

	uv = newgco(ts, 0, CR_VUVALUE, UValue);
	uv->v.location = vp;
	return uv;
}


OClass *cr_object_newclass(cr_State *ts, OString *id)
{
	OClass *cls;

	cls = newgco(ts, 0, CR_VCLASS, OClass);
	cls->methods = NULL;
	cls->name = id;
	memset(cls->vtable, 0, sizeof(cls->vtable) / sizeof(cls->vtable[0]));
	setsv2cls(ts, ts->stacktop.p++, cls);
	cls->methods = cr_htable_new(ts);
	ts->stacktop.p--;
	return cls;
}


Instance *cr_object_newinstance(cr_State *ts, OClass *cls)
{
	Instance *ins;

	ins = newgco(ts, 0, CR_VINSTANCE, Instance);
	ins->oclass = cls;
	ins->fields = NULL;
	setsv2ins(ts, ts->stacktop.p++, ins);
	ins->fields = cr_htable_new(ts);
	ts->stacktop.p--;
	return ins;
}


InstanceMethod *cr_object_newinstancemethod(cr_State *ts, Instance *receiver,
						CriptClosure *method)
{
	InstanceMethod *im;

	im = newgco(ts, 0, CR_VMETHOD, InstanceMethod);
	im->receiver = receiver;
	im->method = cast(GCObject*, method);
	return im;
}
