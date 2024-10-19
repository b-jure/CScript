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


#include "climits.h"


#define crM_newarray(ts,s,t)     crM_malloc(ts, (s) * sizeof(t))

#define crM_reallocarray(ts,p,os,ns,t) \
    ((p) = crM_realloc(ts, (p), (os)*sizeof(t), (ns)*sizeof(t)))

#define crM_freearray(ts,p,n,t) \
    crM_free((ts), (p), cast_umem(n)*sizeof(*(p)))



#define crM_ensurevec(ts,p,s,n,e,l,w,t) \
    ((p) = crM_growarr_(ts, p, n, &(s), sizeof(t), e, l, w))

#define crM_growvec(ts,p,s,n,l,w,t) \
    ((p) = crM_ensurevec((ts), p, s, n, 0, l, w, t))

#define crM_shrinkvec(ts,p,s,f,t) \
    ((p) = crM_shrinkarr_(ts, p, &(s), f, sizeof(t)))



CRI_FUNC void *crM_malloc(cr_State *ts, cr_umem size);
CRI_FUNC void *crM_realloc(cr_State *ts, void *ptr, cr_umem osize,
                           cr_umem nsize);
CRI_FUNC void *crM_saferealloc(cr_State *ts, void *ptr, cr_umem osize,
                               cr_umem nsize);
CRI_FUNC void crM_free(cr_State *ts, void *ptr, cr_umem osize);
CRI_FUNC void *crM_growarr_(cr_State *ts, void *ptr, int len, int *sizep,
                            int elemsize, int ensure, int limit,
                               const char *what);
CRI_FUNC void *crM_shrinkarr_(cr_State *ts, void *ptr, int *sizep, int final,
                              int elemsize);
CRI_FUNC int crM_reallocstack(cr_State *ts, int n);
CRI_FUNC int crM_growstack(cr_State *ts, int n);

#endif
