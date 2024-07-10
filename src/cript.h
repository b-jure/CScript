/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Cript.
 * Cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ---------------------------------------------------------------------------------------------- */


/*
 * Basically reimplementation of 'Lua' core API.
 * (check end of the file for copyright)
 */
#ifndef CRIPT_H
#define CRIPT_H

#include <stddef.h>
#include <stdarg.h>

#include "crconf.h"




/* version info and copyright */
#define CR_VERSION_MAJOR	"1"
#define CR_VERSION_MINOR	"0"
#define CR_VERSION_RELEASE	"0"

#define CR_VERSION_NUMBER		100
#define CR_VERSION_RELEASE_NUM		(CR_VERSION_NUMBER * 100);

#define CR_VERSION	"Cript " CR_VERSION_MAJOR "." CR_VERSION_MINOR
#define CR_RELEASE   	CR_VERSION "." CR_VERSION_RELEASE
#define CR_COPYRIGHT 	CR_RELEASE " Copyright (C) 2023-2024 Jure Bagić"
#define CR_AUTHORS   	"Jure Bagić"



/* multiple return count option for 'cr_call' and 'cr_pcall' */
#define CR_MULRET	(-1)



/* minimum stack space available to a C function */
#define CR_MINSTACK	20



/* Cript thread state */
typedef struct cr_State cr_State;



/* types of values */
#define CR_TNONE	(-1)

#define CR_TBOOL	0 /* boolean */
#define CR_TNUMBER	1 /* number */
#define CR_TLUDATA	2 /* light userdata */
#define CR_TSTRING	3 /* string */
#define CR_TFUNCTION	4 /* function */
#define CR_TCLASS	5 /* class */
#define CR_TINSTANCE	6 /* instance */
#define CR_TUDATA	7 /* userdata */
#define CR_TNIL		8 /* nil */
#define CR_TTHREAD	9 /* thread */

#define CR_NUMTYPES	10	



/* type for integers */
typedef CR_INTEGER cr_integer;

/* type for unsigned integers */
typedef CR_UINTEGER cr_uinteger;

/* type for floating point numbers (Cript numbers) */
typedef CR_NUMBER cr_number;


/* type for registered (with Cript) C function */
typedef int (*cr_cfunc)(cr_State *ts);

/* type for memory allocators */
typedef void *(*cr_alloc)(void *ptr, size_t newsize, void *userdata);

/* type for functions that read blocks when loading Cript chunks */
typedef const char *(*cr_reader)(cr_State *ts, void *userdata, size_t *szread);

/* type for class interface */
typedef struct cr_vtable cr_vtable;

/* type for debugging */
typedef struct cr_debuginfo cr_debuginfo;



/* -------------------------------------------------------------------------
 * State manipulation
 * ------------------------------------------------------------------------- */
CR_API cr_State        *cr_newstate(cr_alloc allocator, void *ud);
CR_API cr_State	       *cr_newthread(cr_State *ts);
CR_API void		cr_freethread(cr_State *ts);
CR_API cr_number	cr_version(cr_State *ts);



/* -------------------------------------------------------------------------
 * Stack manipulation
 * ------------------------------------------------------------------------- */
CR_API void		cr_settop(cr_State *ts, int idx);
CR_API int		cr_gettop(const cr_State *ts);
CR_API int		cr_absidx(cr_State *ts, int idx);
CR_API void		cr_rotate(cr_State *ts, int idx, int n);
CR_API void		cr_copy(cr_State *ts, int src, int dest);
CR_API int		cr_checkstack(cr_State *ts, int n);
CR_API const char      *cr_tostring(cr_State *ts, int idx, size_t *len);
CR_API void 		cr_push(cr_State *ts, int idx);



/* -------------------------------------------------------------------------
 * Access functions (Stack -> C)
 * ------------------------------------------------------------------------- */
CR_API int 		cr_isnumber(cr_State *ts, int idx);
CR_API int		cr_isinteger(cr_State *ts, int idx); // TODO
CR_API int 		cr_isstring(cr_State *ts, int idx);
CR_API int		cr_iscfunc(cr_State *ts, int idx);
CR_API int		cr_isuserdata(cr_State *ts, int idx);
CR_API int 		cr_type(cr_State *ts, int idx);// TODO
CR_API const char      *cr_typename(cr_State *ts, int type);// TODO

CR_API cr_number	cr_getnumber(cr_State *ts, int idx, int *isnum);
CR_API cr_integer	cr_getinteger(cr_State *ts, int idx, int *isnum);// TODO
CR_API int		cr_getbool(cr_State *ts, int idx);
CR_API const char      *cr_getstring(cr_State *ts, int idx);
CR_API cr_uinteger	cr_strlen(cr_State *ts, int idx);
CR_API cr_cfunc		cr_getcfunction(cr_State *ts, int idx);
CR_API void            *cr_getuserdata(cr_State *ts, int idx);// TODO
CR_API const void      *cr_getpointer(cr_State *ts, int idx); // TODO



/* -------------------------------------------------------------------------
 * Ordering & Arithmetic functions
 * ------------------------------------------------------------------------- */
#define CR_OPADD	0
#define CR_OPSUB	1
#define CR_OPMUL	2
#define CR_OPDIV	3
#define CR_OPMOD	4
#define CR_OPPOW	5
#define CR_OPNOT	6
#define CR_OPUMIN	7

#define CR_NUMARITH	8

CR_API void	cr_arith(cr_State *ts, int op);


#define CR_OPEQ		0
#define CR_OPNE		1
#define CR_OPLT		2
#define CR_OPGT		3
#define CR_OPLE		4
#define CR_OPGE		5

#define CR_NUMCMP	6

CR_API int 	cr_rawequal(cr_State *ts, int idx1, int idx2);
CR_API int	cr_compare(cr_State *ts, int idx1, int idx2, int op);



/* -------------------------------------------------------------------------
 * Push functions (C -> stack)
 * ------------------------------------------------------------------------- */
CR_API void		cr_pushnil(cr_State *ts);
CR_API void 		cr_pushnumber(cr_State *ts, cr_number n); // TODO
CR_API void 		cr_pushinteger(cr_State *ts, cr_integer n); // TODO
CR_API void 		cr_pushstring(cr_State *ts, const char *str, size_t len);
CR_API void 		cr_pushcstring(cr_State *ts, const char *str);
CR_API const char      *cr_pushvfstring(cr_State *ts, const char *fmt, va_list argp);
CR_API const char      *cr_pushfstring(cr_State *ts, const char *fmt, ...);
CR_API void		cr_pushcclosure(cr_State *ts, cr_cfunc fn, int upvals); // TODO
CR_API void		cr_pushbool(cr_State *ts, int b);
CR_API void		cr_pushlightuserdata(cr_State *ts, void *p);



/* -------------------------------------------------------------------------
 * Get functions (Cript -> stack)
 * ------------------------------------------------------------------------- */
CR_API int cr_getglobal(cr_State *ts, const char *name);
CR_API int cr_getfield(cr_State *ts, int idx, const char *field);
CR_API int cr_getmethod(cr_State *ts, int idx, const char *method);
CR_API int cr_getindex(cr_State *ts, int idx);
CR_API int cr_rawget(cr_State *ts, int idx);
CR_API int cr_rawgeti(cr_State *ts, int idx, cr_integer n);
CR_API int cr_rawgetp(cr_State *ts, int idx, const void *p);

CR_API int cr_createarray(cr_State *ts, int nelems); // TODO
CR_API int cr_createuserdata(cr_State *ts, size_t sz, int nuvalues); // TODO
CR_API int cr_getuservalue(cr_State *ts, int idx, int n);



/* -------------------------------------------------------------------------
 * Class interface (type system)
 * ------------------------------------------------------------------------- */
/* types of methods for 'cr_vtable' */
#define CR_MT_CFUNCTION		0
#define CR_MT_INDEX		1

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

#define CR_NUMM			21


/* type for class interface */
struct cr_vtable {
	struct {
		union {
			cr_cfunc cfunction; /* C function */
			int stkidx; /* value on stack */
		} method;
		int mtt; /* method type tag */
	} methods[CR_NUMM];
};


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


CR_API void cr_createclass(cr_State *ts, cr_vtable *vt);



/* -------------------------------------------------------------------------
 * Set functions (stack -> Cript)
 * ------------------------------------------------------------------------- */
CR_API int	cr_setglobal(cr_State *ts, const char *name, int isconst);
CR_API int 	cr_setfield(cr_State *ts, int idx, const char *field);
CR_API int 	cr_setindex(cr_State *ts, int idx);
CR_API int	cr_seti(cr_State *ts, int idx, cr_integer n); // TODO
CR_API int	cr_rawset(cr_State *ts, int idx); // TODO
CR_API int	cr_rawseti(cr_State *ts, int idx, cr_integer n); // TODO
CR_API int	cr_rawsetp(cr_State *ts, int idx, void *p); // TODO
CR_API int	cr_setuservalue(cr_State *ts, int idx, int n); // TODO



/* -------------------------------------------------------------------------
 * Error reporting
 * ------------------------------------------------------------------------- */

/* thread status codes */
#define CR_OK			0  /* ok */
#define CR_ERRRUNTIME		1  /* runtime error */
#define CR_ERRSYNTAX		3  /* syntax error (compiler) */
#define CR_ERRMEM		4  /* memory related error (oom) */
#define CR_ERRERROR		5  /* error while handling error */

CR_API int cr_getstatus(cr_State *ts);
CR_API int cr_error(cr_State *ts);



/* -------------------------------------------------------------------------
 * Call/Load Cript code
 * ------------------------------------------------------------------------- */
CR_API int cr_pcall(cr_State *ts, int argc, int retcnt);
CR_API void cr_call(cr_State *ts, int argc, int retcnt);
CR_API int cr_load(cr_State *ts, cr_reader reader, void *userdata, const char *source);



/* -------------------------------------------------------------------------
 * Garbage collector
 * ------------------------------------------------------------------------- */

/* GC options */
#define CR_GCSTOP		(1<<0) /* stop GC */
#define CR_GCRESTART		(1<<1) /* restart GC (start if stopped) */
#define CR_GCCOLLECT		(1<<2) /* perform full GC cycle */
#define CR_GCSTEP		(1<<3) /* perform single gc step */
#define CR_GCCOUNT		(1<<4) /* get number of bytes allocated */
#define CR_GCISRUNNING		(1<<5) /* check whether GC is stopped */
#define CR_GCNEXTGC		(1<<6) /* set bytes amount when the next GC will trigger */

CR_API int cr_gc(cr_State *ts, int optmask, ...);



/* -------------------------------------------------------------------------
 * Miscellaneous functions/macros
 * ------------------------------------------------------------------------- */
CR_API const char      *cr_stringify(cr_State *ts, int idx); // TODO ?
CR_API int		cr_getupvalue(cr_State *ts, int fidx, int idx);
CR_API int		cr_setupvalue(cr_State *ts, int fidx, int idx);
CR_API const char      *cr_concat(cr_State *ts);
CR_API int		cr_nextproperty(cr_State *ts, int idx, int nextfield);
CR_API cr_cfunc		cr_setpanic(cr_State *ts, cr_cfunc panicfn);
CR_API cr_cfunc		cr_getpanic(cr_State *ts);
CR_API cr_cfunc		cr_setalloc(cr_State *ts, cr_alloc allocfn, void *ud);
CR_API cr_alloc		cr_getalloc(cr_State *ts, void **ud);

#define cr_nextfield(ts,idx)		cr_nextproperty((ts),(idx),0)
#define cr_nextmethod(ts,idx)		cr_nextproperty((ts),(idx),1)

#define cr_pushcfunction(ts,fn)		cr_pushcclosure((ts),(fn),0)

#define cr_registerfunction(ts,fn,name) \
	(cr_pushcfunction((ts),(fn)), cr_setglobal((ts),(name),1))

#define cr_pop(ts,n)		cr_settop((ts),-(n)-1)
#define cr_replace(ts,idx)	(cr_copy((ts),-1,(idx)), cr_pop((ts),1))
#define cr_remove(ts,idx)	(cr_rotate((ts),(idx),-1), cr_pop((ts),1))
#define cr_insert(ts,idx)	cr_rotate((ts),(idx),1)



/* -------------------------------------------------------------------------
 * Debug interface
 * ------------------------------------------------------------------------- */

/* bits for 'dbgmask' */
#define CR_DBGFNGET 	 (1<<0) /* load the function on top of the stack (processed first) */
#define CR_DBGLINE 	 (1<<1) /* fill 'line' */
#define CR_DBGFNINFO 	 (1<<2) /* fill all function info in 'cr_debuginfo' */
#define CR_DBGFNSRC 	 (1<<3) /* fill function source information */
#define CR_DBGFNPUSH 	 (1<<4) /* push current function on the stack (processed last) */

CR_API int cr_getstack(cr_State *ts, int level, cr_debuginfo *di);
CR_API int cr_getinfo(cr_State *ts, int dbgmask, cr_debuginfo *di);

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
	struct CallFrame *cf; /* active function frame */
};



/*************************************************************
 * Because Cript C API is almost identical to Lua,
 * we include the below copyright (THANK YOU LUA DEVELOPERS). 
 *************************************************************/

/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 1994-2024 Lua.org, PUC-Rio.
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
