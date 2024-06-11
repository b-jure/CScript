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
 * ---------------------------------------------------------------------------------------------- */


/*
 * Basically reimplementation of 'Lua' core API.
 * (check end of the file for copyright)
 */
#ifndef CR_H
#define CR_H

#include <stddef.h>
#include <stdarg.h>

#include "crconf.h"




/* version info and copyright */
#define CR_VERSION_MAJOR	"1"
#define CR_VERSION_MINOR	"0"
#define CR_VERSION_RELEASE	"0"

#define CR_VERSION_NUMBER	100
#define CR_VERSION_RELEASE_NUM	(CR_VERSION_NUMBER * 100);

#define CR_VERSION	"cript " CR_VERSION_MAJOR "." CR_VERSION_MINOR
#define CR_RELEASE   	CR_VERSION "." CR_VERSION_RELEASE
#define CR_COPYRIGHT 	CR_RELEASE " Copyright (C) 2023-2024 Jure Bagić"
#define CR_AUTHORS   	"Jure Bagić"



/* multiple return count option for 'cr_call' and 'cr_pcall' */
#define CR_MULRET	(-1)



/* minimum stack space available to a C function */
#define CR_MINSTACK	20



/* Cript thread */
typedef struct VM VM;



/* types of values */
#define CR_TNONE	(-1)

#define CR_TBOOL	0 /* boolean */
#define CR_TNUMBER	1 /* 'cr_number' */
#define CR_TLUDATA	2 /* light userdata */
#define CR_TSTRING	3 /* string */
#define CR_TFUNCTION	4 /* function */
#define CR_TCLASS	5 /* class */
#define CR_TINSTANCE	6 /* instance */
#define CR_TUDATA	7 /* userdata */
#define CR_TNIL		8 /* nil */

#define CR_NTYPES	9



/* type for integers */
typedef CR_INTEGER cr_integer;

/* type for unsigned integers */
typedef CR_UINTEGER cr_uinteger;

/* type for floating point numbers (Cript numbers) */
typedef CR_NUMBER cr_number;


/* type for registered (with Cript) C function */
typedef int (*cr_cfunc)(VM *vm);

/* type for memory allocators */
typedef void *(*cr_alloc)(void *ptr, size_t newsize, void *userdata);

/* type for functions that read blocks when loading Cript chunks */
typedef const char *(*cr_reader)(VM *vm, void *userdata, size_t *szread);

/* type for class interface */
typedef struct cr_vtable cr_vtable;

/* type for debugging */
typedef struct cr_debuginfo cr_debuginfo;



/* 
 * state manipulation 
 */
CR_API VM              *cr_create(cr_alloc allocator, void *ud);
CR_API void		cr_destroy(VM *vm);
CR_API cr_number	cr_version(VM *vm);



/* 
 * stack manipulation 
 */
CR_API void		cr_settop(VM *vm, int idx);
CR_API int		cr_gettop(const VM *vm);
CR_API int		cr_absidx(VM *vm, int idx);
CR_API void		cr_rotate(VM *vm, int idx, int n);
CR_API void		cr_copy(VM *vm, int src, int dest);
CR_API int		cr_checkstack(VM *vm, int n);
CR_API const char      *cr_tostring(VM *vm, int idx, size_t *len);
CR_API void 		cr_push(VM *vm, int idx);



/* 
 * access functions, stack -> C
 */
CR_API int 		cr_isnumber(VM *vm, int idx);
CR_API int		cr_isinteger(VM *vm, int idx); // TODO
CR_API int 		cr_isstring(VM *vm, int idx);
CR_API int		cr_iscfunc(VM *vm, int idx);
CR_API int		cr_isuserdata(VM *vm, int idx);
CR_API int 		cr_type(VM *vm, int idx);// TODO
CR_API const char      *cr_typename(VM *vm, int type);// TODO

CR_API cr_number	cr_getnumber(VM *vm, int idx, int *isnum);
CR_API cr_integer	cr_getinteger(VM *vm, int idx, int *isnum);// TODO
CR_API int		cr_getbool(VM *vm, int idx);
CR_API const char      *cr_getstring(VM *vm, int idx);
CR_API cr_uinteger	cr_strlen(VM *vm, int idx);
CR_API cr_cfunc		cr_getcfunction(VM *vm, int idx);
CR_API void            *cr_getuserdata(VM *vm, int idx);// TODO
CR_API const void      *cr_getpointer(VM *vm, int idx); // TODO



/* 
 * ordering and arithmetic functions 
 */
#define CR_OPADD	0
#define CR_OPSUB	1
#define CR_OPMUL	2
#define CR_OPDIV	3
#define CR_OPMOD	4
#define CR_OPPOW	5
#define CR_OPNOT	6
#define CR_OPUMIN	7

#define CR_ARN		8

CR_API void	cr_arith(VM *vm, int op);

#define CR_OPEQ		0
#define CR_OPNE		1
#define CR_OPLT		2
#define CR_OPGT		3
#define CR_OPLE		4
#define CR_OPGE		5

#define CR_CMPN		6

CR_API int 	cr_rawequal(VM *vm, int idx1, int idx2);
CR_API int	cr_compare(VM *vm, int idx1, int idx2, int op);



/* 
 * push functions, C -> stack 
 */
CR_API void		cr_pushnil(VM *vm);
CR_API void 		cr_pushnumber(VM *vm, cr_number n); // TODO
CR_API void 		cr_pushinteger(VM *vm, cr_integer n); // TODO
CR_API void 		cr_pushstring(VM *vm, const char *str, size_t len);
CR_API void 		cr_pushcstring(VM *vm, const char *str);
CR_API const char      *cr_pushvfstring(VM *vm, const char *fmt, va_list argp);
CR_API const char      *cr_pushfstring(VM *vm, const char *fmt, ...);
CR_API void		cr_pushcclosure(VM *vm, cr_cfunc fn, int upvals); // TODO
CR_API void		cr_pushbool(VM *vm, int b);
CR_API void		cr_pushlightuserdata(VM *vm, void *p);



/* 
 * get functions, Cript -> stack 
 */
CR_API int cr_getglobal(VM *vm, const char *name);
CR_API int cr_getfield(VM *vm, int idx, const char *field);
CR_API int cr_getmethod(VM *vm, int idx, const char *method);
CR_API int cr_getindex(VM *vm, int idx);
CR_API int cr_rawget(VM *vm, int idx);
CR_API int cr_rawgeti(VM *vm, int idx, cr_integer n);
CR_API int cr_rawgetp(VM *vm, int idx, const void *p);

CR_API int cr_createarray(VM *vm, int nelems); // TODO
CR_API int cr_createuserdata(VM *vm, size_t sz, int nuvalues); // TODO
CR_API int cr_getuservalue(VM *vm, int idx, int n);



/* 
 * class interface 
 */

/* types of methods for 'cr_vtable' */
#define CR_MTCFUNCTION		0
#define CR_MTINDEX		1

/* 'cr_vtable' methods */
#define CR_MINIT		0
#define CR_MDISPLAY		1
#define CR_MTOSTRING		2
#define CR_MGETIDX		3
#define CR_MSETIDX		4
#define CR_MGC			5
#define CR_MDEFER		6
#define CR_MADD			7
#define CR_MSUB			8
#define CR_MMUL			9
#define CR_MDIV			10
#define CR_MMOD			11
#define CR_MPOW			12
#define CR_MNOT			13
#define CR_MUMIN		14
#define CR_MNE			15
#define CR_MEQ			16
#define CR_MLT			17
#define CR_MLE			18
#define CR_MGT			19
#define CR_MGE			20

#define CR_MNUM			21


/* type for class interface */
struct cr_vtable {
	struct {
		union {
			cr_cfunc cfunction; /* C function */
			int stkidx; /* value on stack */
		} method;
		int mtt; /* method type tag */
	} methods[CR_MNUM];
}; // TODO


/* 
 * Helpers for setting 'cr_vtable'. 
 * @vt - pointer to 'cr_vtable'
 * @m - method ('CR_M*')
 * @mt - type of method ('CR_MT*')
 * @v - method value ('cr_cfunc' or 'int')
 */

/* set index value for method 'm' */
#define cr_vtablesetidx(vt,m,idx) \
	{ (vt)->methods[(m)].mtt = CR_MTINDEX; \
	  (vt)->methods[(m)].method.stkidx = (idx); }

/* set function value for method 'm' */
#define cr_vtablesetfunc(vt,m,fn) \
	{ (vt)->methods[(m)].mtt = CR_MTCFUNCTION; \
	  (vt)->methods[(m)].method.cfunction = (fn); }

/* set value 'v' for method 'm' (bit slower but generic) */
#define cr_vtableset(vt,m,mt,v) { \
	(vt)->methods[(m)].mtt = (mt); \
	switch ((mt)) { \
	case CR_VTECFUNCTION: (vt).methods[(m)].method.cfunction = (v); break; \
	case CR_VTEINDEX: (vt).methods[(m)].method.stkidx = (v); break; \
	case CR_VTNONE: case default: break; }}


CR_API void cr_createclass(VM *vm, cr_vtable *vt); // TODO



/*
 * set functions stack -> Cript 
 */
CR_API int	cr_setglobal(VM *vm, const char *name, int isconst);
CR_API int 	cr_setfield(VM *vm, int idx, const char *field);
CR_API int 	cr_setindex(VM *vm, int idx);
CR_API int	cr_seti(VM *vm, int idx, cr_integer n); // TODO
CR_API int	cr_rawset(VM *vm, int idx); // TODO
CR_API int	cr_rawseti(VM *vm, int idx, cr_integer n); // TODO
CR_API int	cr_rawsetp(VM *vm, int idx, void *p); // TODO
CR_API int	cr_setuservalue(VM *vm, int idx, int n); // TODO



/* 
 * error reporting 
 */

/* thread status codes */
#define CR_OK			0  /* ok */
#define CR_ERRRUNTIME		1  /* runtime error */
#define CR_ERRSYNTAX		3  /* syntax error (compiler) */
#define CR_ERRMEM		4  /* memory related error (oom) */
#define CR_ERRERROR		5  /* error while handling error */

CR_API int cr_getstatus(VM *vm);
CR_API int cr_error(VM *vm);



/* 
 * call/load cript code 
 */
CR_API int cr_pcall(VM *vm, int argc, int retcnt);
CR_API void cr_call(VM *vm, int argc, int retcnt);
CR_API int cr_load(VM *vm, cr_reader reader, void *userdata, const char *source);



/* 
 * garbage-collection 
 */

/* GC options */
#define CR_GCSTOP		(1<<0) /* stop GC */
#define CR_GCRESTART		(1<<1) /* restart GC (start if stopped) */
#define CR_GCCOLLECT		(1<<2) /* perform full GC cycle */
#define CR_GCSTEP		(1<<3) /* perform single gc step */
#define CR_GCCOUNT		(1<<4) /* get number of bytes allocated */
#define CR_GCISRUNNING		(1<<5) /* check whether GC is stopped */
#define CR_GCNEXTGC		(1<<6) /* set bytes amount when the next GC will trigger */

CR_API int cr_gc(VM *vm, int optmask, ...);



/* 
 * miscellaneous functions/macros 
 */
CR_API const char      *cr_stringify(VM *vm, int idx); // TODO ?

CR_API int		cr_getupvalue(VM *vm, int fidx, int idx);
CR_API int		cr_setupvalue(VM *vm, int fidx, int idx);

CR_API const char      *cr_concat(VM *vm);

CR_API int		cr_nextproperty(VM *vm, int idx, int nextfield);

CR_API cr_cfunc		cr_setpanic(VM *vm, cr_cfunc panicfn);
CR_API cr_cfunc		cr_getpanic(VM *vm);
CR_API cr_cfunc		cr_setalloc(VM *vm, cr_alloc allocfn, void *ud);
CR_API cr_alloc		cr_getalloc(VM *vm, void **ud);

#define cr_nextfield(vm,idx)		cr_nextproperty((vm),(idx),0)
#define cr_nextmethod(vm,idx)		cr_nextproperty((vm),(idx),1)

#define cr_pushcfunction(vm,fn)		cr_pushcclosure((vm),(fn),0)

#define cr_registerfunction(vm,fn,name) \
	(cr_pushcfunction((vm),(fn)), cr_setglobal((vm),(name),1))

#define cr_pop(vm,n)		cr_settop((vm),-(n)-1)
#define cr_replace(vm,idx)	(cr_copy((vm),-1,(idx)), cr_pop((vm),1))
#define cr_remove(vm,idx)	(cr_rotate((vm),(idx),-1), cr_pop((vm),1))
#define cr_insert(vm,idx)	cr_rotate((vm),(idx),1)



/* 
 * Debug API 
 */

CR_API int cr_getstack(VM *vm, int level, cr_debuginfo *di);

/* bits for debug mask */
#define CR_DBGFNGET 	 (1<<0) /* load the function on top of the stack (processed first) */
#define CR_DBGLINE 	 (1<<1) /* fill 'line' */
#define CR_DBGFNINFO 	 (1<<2) /* fill all function info in 'cr_debuginfo' */
#define CR_DBGFNSRC 	 (1<<3) /* fill function source information */
#define CR_DBGFNPUSH 	 (1<<4) /* push current function on the stack (processed last) */

CR_API int cr_getinfo(VM *vm, int dbgmask, cr_debuginfo *di);

struct cr_debuginfo {
	const char *name; /* function name (declaration name in cript script) */
	const char *type; /* function type ('cript', 'main' or 'C') */
	const char *source; /* function source */
	size_t srclen; /* length of 'source' */
	int line; /* current line in cript script */
	int nups; /* number of function upvalues */
	int nparams; /* number of function parameters */
	char isvararg; /* is function vararg ('...') */
	int defline; /* line number where the function definition starts */
	int deflastline; /* line number where the function definition ends */
	char shortsrc[CRI_MAXSRC];
	/* private */
	struct CallFrame *frame; /* active function frame */
};



/* 
 * !!!
 * Because the Cript C API is almost identical to Lua,
 * we include the below copyright (thank you Lua developers). 
 * !!!
 */

/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 1994-2023 Lua.org, PUC-Rio.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ---------------------------------------------------------------------------------------------- */
#endif
