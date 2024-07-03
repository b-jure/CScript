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

#ifndef CRMEM_H
#define CRMEM_H


#include "crlimits.h"


#define cr_mem_rawmalloc(gs, s)		(gs)->realloc(NULL, s, (gs)->udrealloc)
#define cr_mem_rawrealloc(gs, p, s)   	(gs)->realloc(p, s, (gs)->udrealloc)
#define cr_mem_rawfree(gs, p)	   	(gs)->realloc(p, 0, (gs)->udrealloc)


#define cr_mem_newarray(ts,s,t)		cr_mem_malloc(ts, (s) * sizeof(t))

#define cr_mem_reallocarray(ts,p,os,ns) \
	cr_mem_realloc(ts, (p), (os)*sizeof(*p), (ns)*sizeof(ns))

#define cr_mem_freearray(ts,p,n)	\
	cr_mem_free((ts), (p), cast_umem(n)*sizeof(*(p)))



/* declare 'name' Vec */
#define Vec(name, type) \
	typedef struct { \
		type *ptr; \
		int len; \
		int size; \
		cr_umem limit; \
		const char *what; \
	} name;


/* should be called only once */
#define cr_mem_createvec(ts,v,l,w) \
	({ (void)(ts); cr_mem_initvec(ts,v); (v)->limit = (l); (v)->what = (w); })


#define cr_mem_initvec(ts,v) \
	({ (void)(ts); (v)->ptr = NULL; (v)->len = (v)->size = 0; })


#define cr_mem_ensurevec(ts,v,n) \
	((v)->ptr = cr_mem_growarr((ts), (v)->ptr, (v)->len, &(v)->size, \
		sizeof(*(v)->ptr), (n), (v)->limit, (v)->what))


#define cr_mem_growvec(ts,v)	cr_mem_ensurevec((ts), (v), 0)


#define cr_mem_reallocvec(ts,v,ns) \
	((v)->ptr = cr_mem_realloc((ts), (v)->ptr, \
		cast_umem((v)->size*sizeof(*(v)->ptr)), (ns)))


#define cr_mem_freevec(ts,v) \
	cr_mem_freearray((ts), (v)->ptr, (v)->size*sizeof(*(v)->ptr))



/* commonly used 'Vec's */
Vec(ubyteVec, cr_ubyte);
Vec(intVec, int);
Vec(uintVec, int);



CRI_FUNC void *cr_mem_realloc(cr_State *ts, void *ptr, cr_umem osize, cr_umem nsize);

CRI_FUNC void *cr_mem_malloc(cr_State *ts, cr_umem size);
CRI_FUNC void *cr_mem_saferealloc(cr_State *ts, void *ptr, cr_umem osize, cr_umem nsize);
CRI_FUNC void cr_mem_free(cr_State *ts, void *ptr, cr_umem osize);

CRI_FUNC void *cr_mem_growarr(cr_State *ts, void *ptr, int len, int *sizep, int elemsize,
		int ensure, int limit, const char *what);

CRI_FUNC int cr_mem_reallocstack(cr_State *ts, int n);
CRI_FUNC int cr_mem_growstack(cr_State *ts, int n);

#endif
