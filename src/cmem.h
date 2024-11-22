/*
** cmem.h
** Functions for memory management
** See Copyright Notice in cscript.h
*/

#ifndef CMEM_H
#define CMEM_H


#include "climits.h"


/* memory error */
#define csM_error(ts)   csPR_throw(ts, CS_ERRMEM);


#define csM_newarray(ts,s,t)     csM_malloc(ts, (s) * sizeof(t))

#define csM_reallocarray(ts,p,os,ns,t) \
        ((p) = csM_realloc_(ts, (p), (os)*sizeof(t), (ns)*sizeof(t)))

#define csM_freearray(ts,p,n,t) \
        csM_free_((ts), (p), cast_umem(n)*sizeof(*(p)))


#define csM_ensurevec(ts,p,s,n,e,l,w,t) \
        ((p) = csM_growarr(ts, p, cast(int *, &(s)), n, sizeof(t), e, l, w))

#define csM_growvec(ts,p,s,n,l,w,t) \
        csM_ensurevec((ts), p, s, n, 1, l, w, t)

#define csM_shrinkvec(ts,p,s,f,t) \
        ((p) = csM_shrinkarr(ts, p, cast(int *, &(s)), f, sizeof(t)))


#define csM_free(ts,p)          csM_free_(ts, p, sizeof(*(p)))
#define csM_freemem(ts,p,sz)    csM_free_((ts), (p), (sz))


CSI_FUNC void *csM_malloc(cs_State *ts, cs_umem size);
CSI_FUNC void *csM_realloc_(cs_State *ts, void *ptr, cs_umem osize,
                            cs_umem nsize);
CSI_FUNC void *csM_saferealloc(cs_State *ts, void *ptr, cs_umem osize,
                               cs_umem nsize);
CSI_FUNC cs_noret csM_toobig(cs_State *ts);
CSI_FUNC void csM_free_(cs_State *ts, void *ptr, cs_umem osize);
CSI_FUNC void *csM_growarr(cs_State *ts, void *ptr, int *sizep, int len,
                           int elemsz, int ensure, int lim, const char *what);
CSI_FUNC void *csM_shrinkarr(cs_State *ts, void *ptr, int *sizep, int final,
                             int elemsz);
CSI_FUNC int csM_reallocstack(cs_State *ts, int n);
CSI_FUNC int csM_growstack(cs_State *ts, int n);

#endif
