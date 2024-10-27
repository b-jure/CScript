/*
** cmem.h
** Functions for memory management
** See Copyright Notice in cscript.h
*/

#ifndef CRMEM_H
#define CRMEM_H


#include "climits.h"


/* memory error */
#define crM_error(ts)   crPR_throw(ts, CR_ERRMEM);


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
