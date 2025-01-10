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


#define csM_new(ts,t)           csM_malloc_(ts, sizeof(t), 0)
#define csM_newarray(ts,s,t)    csM_malloc_(ts, (s)*sizeof(t), 0)
#define csM_newobj(ts,tag,sz)   csM_malloc_(ts, (sz), tag)

#define csM_free(ts,p)          csM_free_(ts, p, sizeof(*(p)))
#define csM_freemem(ts,p,sz)    csM_free_((ts), (p), (sz))
#define csM_freearray(ts,p,n)   csM_free_((ts), (p), (n)*sizeof(*(p)))

#define csM_reallocarray(ts,p,os,ns) \
        ((p) = csM_realloc_(ts, p, (os)*sizeof(*(p)), (ns)*sizeof(*(p))))

#define csM_ensurearray(ts,p,s,n,e,l,w,t) \
        ((p) = csM_growarr_(ts, p, cast(int *,&(s)), n, sizeof(t), e, l, w))

#define csM_growarray(ts,p,s,n,l,w,t) \
        csM_ensurearray((ts), (p), (s), (n), 1, (l), (w), t)

#define csM_shrinkarray(ts,p,s,f,t) \
        ((p) = csM_shrinkarr_(ts, p, cast(int *, &(s)), f, sizeof(t)))


CSI_FUNC void *csM_malloc_(cs_State *ts, c_mem size, int tag);
CSI_FUNC void *csM_realloc_(cs_State *ts, void *ptr, c_mem osize,
                            c_mem nsize);
CSI_FUNC void *csM_saferealloc(cs_State *ts, void *ptr, c_mem osize,
                               c_mem nsize);
CSI_FUNC c_noret csM_toobig(cs_State *ts);
CSI_FUNC void csM_free_(cs_State *ts, void *ptr, c_mem osize);
CSI_FUNC void *csM_growarr_(cs_State *ts, void *ptr, int *sizep, int len,
                           int elemsz, int ensure, int lim, const char *what);
CSI_FUNC void *csM_shrinkarr_(cs_State *ts, void *ptr, int *sizep, int final,
                             int elemsz);
CSI_FUNC int csM_growstack(cs_State *ts, int n);

#endif
