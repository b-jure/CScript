/*
** cmem.h
** Functions for memory management
** See Copyright Notice in cscript.h
*/

#ifndef CRMEM_H
#define CRMEM_H


#include "climits.h"


/* memory error */
#define crM_error(ts)   crPR_throw(ts, CS_ERRMEM);


#define crM_newarray(ts,s,t)     crM_malloc(ts, (s) * sizeof(t))

#define crM_reallocarray(ts,p,os,ns,t) \
    ((p) = crM_realloc_(ts, (p), (os)*sizeof(t), (ns)*sizeof(t)))

#define crM_freearray(ts,p,n,t) \
    crM_free((ts), (p), cast_umem(n)*sizeof(*(p)))


#define crM_ensurevec(ts,p,s,n,e,l,w,t) \
    ((p) = crM_growarr(ts, p, cast(int *, &(s)), n, sizeof(t), e, l, w))

#define crM_growvec(ts,p,s,n,l,w,t) \
    ((p) = crM_ensurevec((ts), p, s, n, 0, l, w, t))

#define crM_shrinkvec(ts,p,s,f,t) \
    ((p) = crM_shrinkarr(ts, p, cast(int *, &(s)), f, sizeof(t)))



CSI_FUNC void *crM_malloc(cs_State *ts, cs_umem size);
CSI_FUNC void *crM_realloc_(cs_State *ts, void *ptr, cs_umem osize,
                            cs_umem nsize);
CSI_FUNC void *crM_saferealloc(cs_State *ts, void *ptr, cs_umem osize,
                               cs_umem nsize);
CSI_FUNC void crM_free(cs_State *ts, void *ptr, cs_umem osize);
CSI_FUNC void *crM_growarr(cs_State *ts, void *ptr, int *sizep, int len,
                           int elemsize, int ensure, int limit,
                               const char *what);
CSI_FUNC void *crM_shrinkarr(cs_State *ts, void *ptr, int *sizep, int final,
                              int elemsize);
CSI_FUNC int crM_reallocstack(cs_State *ts, int n);
CSI_FUNC int crM_growstack(cs_State *ts, int n);

#endif
