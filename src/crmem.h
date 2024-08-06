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


#define cr_mem_newarray(ts,s,t)     cr_mem_malloc(ts, (s) * sizeof(t))

#define cr_mem_reallocarray(ts,p,os,ns) \
    ((p) = cr_mem_realloc(ts, (p), (os)*sizeof(*p), (ns)*sizeof(ns)))

#define cr_mem_freearray(ts,p,n) \
    cr_mem_free((ts), (p), cast_umem(n)*sizeof(*(p)))



#define cr_mem_ensurevec(ts,p,s,n,e,l,w) \
    ((p) = cr_mem_growarr_(ts, p, n, &(s), sizeof(*(p)), e, l, w))

#define cr_mem_growvec(ts,p,s,n,l,w) \
    ((p) = cr_mem_ensurevec((ts), p, s, n, 0, l, w))

#define cr_mem_shrinkvec(ts,p,s,f) \
    ((p) = cr_mem_shrinkarr_(ts, p, &(s), f, sizeof(*(p))))



CRI_FUNC void *cr_mem_malloc(cr_State *ts, cr_umem size);
CRI_FUNC void *cr_mem_realloc(cr_State *ts, void *ptr, cr_umem osize,
                              cr_umem nsize);
CRI_FUNC void *cr_mem_saferealloc(cr_State *ts, void *ptr, cr_umem osize,
                                  cr_umem nsize);
CRI_FUNC void cr_mem_free(cr_State *ts, void *ptr, cr_umem osize);
CRI_FUNC void *cr_mem_growarr_(cr_State *ts, void *ptr, int len, int *sizep,
                               int elemsize, int ensure, int limit,
                               const char *what);
CRI_FUNC void *cr_mem_shrinkarr_(cr_State *ts, void *ptr, int *sizep, int final,
                                 int elemsize);
CRI_FUNC int cr_mem_reallocstack(cr_State *ts, int n);
CRI_FUNC int cr_mem_growstack(cr_State *ts, int n);

#endif
