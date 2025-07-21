/*
** tmem.h
** Functions for memory management
** See Copyright Notice in tokudae.h
*/

#ifndef tmem_h
#define tmem_h


#include "tokudaelimits.h"


/* memory error */
#define tokuM_error(C)      tokuPR_throw(C, TOKU_STATUS_EMEM)


#define tokuM_new(C,t)          tokuM_malloc_(C, sizeof(t), 0)
#define tokuM_newarray(C,s,t)   tokuM_malloc_(C, (s)*sizeof(t), 0)
#define tokuM_newobj(C,tag,sz)  tokuM_malloc_(C, (sz), (tag))

#define tokuM_free(C,p)         tokuM_free_(C, p, sizeof(*(p)))
#define tokuM_freemem(C,p,sz)   tokuM_free_((C), (p), (sz))
#define tokuM_freearray(C,p,n)  tokuM_free_((C), (p), (n)*sizeof(*(p)))


#define tokuM_reallocarray(C,p,os,ns,t) \
        ((p) = tokuM_realloc_(C, p, (os)*sizeof(t), (ns)*sizeof(t)))

#define tokuM_ensurearray(C,p,s,n,e,l,w,t) \
        ((p) = tokuM_growarr_(C, p, &(s), n, sizeof(t), e, l, w))

#define tokuM_growarray(C,p,s,n,l,w,t) \
        tokuM_ensurearray((C), (p), (s), (n), 1, (l), (w), t)

#define tokuM_shrinkarray(C,p,s,f,t) \
        ((p) = tokuM_shrinkarr_(C, p, &(s), f, sizeof(t)))


TOKUI_FUNC void *tokuM_malloc_(toku_State *T, t_umem size, int tag);
TOKUI_FUNC void *tokuM_realloc_(toku_State *T, void *ptr, t_umem osize,
                                t_umem nsize);
TOKUI_FUNC void *tokuM_saferealloc(toku_State *T, void *ptr, t_umem osize,
                                   t_umem nsize);
TOKUI_FUNC t_noret tokuM_toobig(toku_State *T);
TOKUI_FUNC void tokuM_free_(toku_State *T, void *ptr, t_umem osize);
TOKUI_FUNC void *tokuM_growarr_(toku_State *T, void *ptr, int *sizep, int len,
                                               int elemsz, int ensure,
                                               int lim, const char *what);
TOKUI_FUNC void *tokuM_shrinkarr_(toku_State *T, void *ptr, int *sizep,
                                                 int final, int elemsz);
TOKUI_FUNC int tokuM_growstack(toku_State *T, int n);

#endif
