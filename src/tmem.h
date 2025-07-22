/*
** tmem.h
** Functions for memory management
** See Copyright Notice in tokudae.h
*/

#ifndef tmem_h
#define tmem_h


#include "tokudaelimits.h"


/* memory error */
#define tokuM_error(T)      tokuPR_throw(T, TOKU_STATUS_EMEM)


/*
** Computes the minimum between 'n' and 'TOKU_MAXSIZET/sizeof(t)', so that
** the result is not larger than 'n' and cannot overflow a 'size_t'
** when multiplied by the size of type 't'. (Assumes that 'n' is an
** 'int' and that 'int' is not larger than 'size_t'.)
*/
#define tokuM_limitN(n,t)  \
    ((cast_sizet(n) <= TOKU_MAXSIZET/sizeof(t)) ? (n) :  \
        cast_int((TOKU_MAXSIZET/sizeof(t))))


#define tokuM_new(T,t)          tokuM_malloc_(T, sizeof(t), 0)
#define tokuM_newarray(T,s,t)   tokuM_malloc_(T, (s)*sizeof(t), 0)
#define tokuM_newobj(T,tag,sz)  tokuM_malloc_(T, (sz), (tag))

#define tokuM_free(T,p)         tokuM_free_(T, p, sizeof(*(p)))
#define tokuM_freemem(T,p,sz)   tokuM_free_((T), (p), (sz))
#define tokuM_freearray(T,p,n)  tokuM_free_((T), (p), (n)*sizeof(*(p)))


#define tokuM_reallocarray(T,p,os,ns,t) \
        cast(t *, tokuM_realloc_(T, p, cast_sizet(os)*sizeof(t), \
                                       cast_sizet(ns)*sizeof(t)))

#define tokuM_ensurearray(T,p,size,nelems,space,limit,err,t) \
        ((p) = cast(t *, tokuM_growarr_(T, p, &(size), nelems, sizeof(t), \
                                        space, tokuM_limitN(limit,t), err)))

#define tokuM_growarray(T,p,size,nelems,limit,err,t) \
        tokuM_ensurearray(T, p, size, nelems, 1, limit, err, t)

#define tokuM_shrinkarray(T,p,size,f,t) \
        ((p) = cast(t *, tokuM_shrinkarr_(T, p, &(size), f, sizeof(t))))


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
