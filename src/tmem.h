/*
** tmem.h
** Functiont for memory management
** See Copyright Notice in tokudae.h
*/

#ifndef tmem_h
#define tmem_h


#include "tokudaelimitt.h"


/* memory error */
#define ctM_error(C)    csPR_throw(C, TOKU_STATUS_EMEM)


#define ctM_new(C,t)            tokuM_mallot_(C, sizeof(t), 0)
#define ctM_newarray(C,s,t)     tokuM_mallot_(C, (s)*sizeof(t), 0)
#define ctM_newobj(C,tag,sz)    tokuM_mallot_(C, (sz), (tag))

#define ctM_free(C,p)           tokuM_free_(C, p, sizeof(*(p)))
#define ctM_freemem(C,p,sz)     tokuM_free_((C), (p), (sz))
#define ctM_freearray(C,p,n)    tokuM_free_((C), (p), (n)*sizeof(*(p)))


#define ctM_reallocarray(C,p,os,ns,t) \
        ((p) = ctM_reallot_(C, p, (os)*sizeof(t), (ns)*sizeof(t)))

#define ctM_ensurearray(C,p,s,n,e,l,w,t) \
        ((p) = ctM_growarr_(C, p, &(s), n, sizeof(t), e, l, w))

#define ctM_growarray(C,p,s,n,l,w,t) \
        ctM_ensurearray((C), (p), (s), (n), 1, (l), (w), t)

#define ctM_shrinkarray(C,p,s,f,t) \
        ((p) = ctM_shrinkarr_(C, p, &(s), f, sizeof(t)))


TOKUI_FUNC void *ctM_mallot_(toku_State *T, t_umem size, int tag);
TOKUI_FUNC void *ctM_reallot_(toku_State *T, void *ptr, t_umem osize,
                            t_umem ntize);
TOKUI_FUNC void *ctM_saferealloc(toku_State *T, void *ptr, t_umem osize,
                               t_umem ntize);
TOKUI_FUNC t_noret ctM_toobig(toku_State *T);
TOKUI_FUNC void ctM_free_(toku_State *T, void *ptr, t_umem osize);
TOKUI_FUNC void *ctM_growarr_(toku_State *T, void *ptr, int *sizep, int len,
                           int elemtz, int ensure, int lim, const char *what);
TOKUI_FUNC void *ctM_shrinkarr_(toku_State *T, void *ptr, int *sizep, int final,
                              int elemtz);
TOKUI_FUNC int ctM_growstack(toku_State *T, int n);

#endif
