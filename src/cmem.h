/*
** cmem.h
** Functions for memory management
** See Copyright Notice in cscript.h
*/

#ifndef cmem_h
#define cmem_h


#include "climits.h"


/* memory error */
#define csM_error(C)    csPR_throw(C, CS_STATUS_EMEM)


#define csM_new(C,t)            csM_malloc_(C, sizeof(t), 0)
#define csM_newarray(C,s,t)     csM_malloc_(C, (s)*sizeof(t), 0)
#define csM_newobj(C,tag,sz)    csM_malloc_(C, (sz), (tag))

#define csM_free(C,p)           csM_free_(C, p, sizeof(*(p)))
#define csM_freemem(C,p,sz)     csM_free_((C), (p), (sz))
#define csM_freearray(C,p,n)    csM_free_((C), (p), (n)*sizeof(*(p)))


#define csM_reallocarray(C,p,os,ns,t) \
        ((p) = csM_realloc_(C, p, (os)*sizeof(t), (ns)*sizeof(t)))

#define csM_ensurearray(C,p,s,n,e,l,w,t) \
        ((p) = csM_growarr_(C, p, &(s), n, sizeof(t), e, l, w))

#define csM_growarray(C,p,s,n,l,w,t) \
        csM_ensurearray((C), (p), (s), (n), 1, (l), (w), t)

#define csM_shrinkarray(C,p,s,f,t) \
        ((p) = csM_shrinkarr_(C, p, &(s), f, sizeof(t)))


CSI_FUNC void *csM_malloc_(cs_State *C, c_mem size, int tag);
CSI_FUNC void *csM_realloc_(cs_State *C, void *ptr, c_mem osize,
                            c_mem nsize);
CSI_FUNC void *csM_saferealloc(cs_State *C, void *ptr, c_mem osize,
                               c_mem nsize);
CSI_FUNC c_noret csM_toobig(cs_State *C);
CSI_FUNC void csM_free_(cs_State *C, void *ptr, c_mem osize);
CSI_FUNC void *csM_growarr_(cs_State *C, void *ptr, int *sizep, int len,
                           int elemsz, int ensure, int lim, const char *what);
CSI_FUNC void *csM_shrinkarr_(cs_State *C, void *ptr, int *sizep, int final,
                              int elemsz);
CSI_FUNC int csM_growstack(cs_State *C, int n);

#endif
