/*
** tprotected.h
** Functions for calling functions in protected mode
** See Copyright Notice in tokudae.h
*/

#ifndef tprotected_h
#define tprotected_h


#include "treader.h"
#include "tobject.h"


/* type for functions with error handler */
typedef void (*ProtectedFn)(toku_State *T, void *uterdata);


/* save/restore stack position */
#define savestack(T,ptr)	(cast_charp(ptr) - cast_charp((T)->stack.p))
#define restorestack(T,n)	cast(SPtr, cast_charp((T)->stack.p) + (n))


/* 
** Check if stack needs to grow if so, do 'pre' then grow and
** then do 'pot'.
*/
#define tokuPR_checkstackaux(T,n,pre,pos) \
    if (t_unlikely((T)->stackend.p - (T)->sp.p <= (n))) \
        { pre; tokuT_growstack(T, (n), 1); pos; } \
    else { condmovestack(T, pre, pos); }


/* check if stack needs to grow */
#define tokuPR_checkstack(T,n)    tokuPR_checkstackaux(T,n,(void)0,(void)0)


/* check if stack needs to grow, preserving 'p' */
#define checkstackp(T,n,p) \
        tokuPR_checkstackaux(T, n, \
            ptrdiff_t p_ = savestack(T, p), \
            p = restorestack(T, p_))


/* check GC then check stack, preserving 'p' */
#define checkstackGCp(T,n,p) \
        tokuPR_checkstackaux(T,n, \
            ptrdiff_t p_ = savestack(T,p); tokuG_checkGC(T), \
            p = restorestack(T, p_))


/* check GC then check stack */
#define checkstackGC(T,n)   tokuPR_checkstackaux(T,n,tokuG_checkGC(T),(void)0)


TOKUI_FUNC void tokuPR_seterrorobj(toku_State *T, int errcode, SPtr oldtop);
TOKUI_FUNC t_noret tokuPR_throw(toku_State *T, int code);
TOKUI_FUNC int tokuPR_rawcall(toku_State *T, ProtectedFn fn, void *ud);
TOKUI_FUNC int tokuPR_call(toku_State *T, ProtectedFn fn, void *ud, ptrdiff_t top,
                           ptrdiff_t errfunc);
TOKUI_FUNC int tokuPR_close(toku_State *T, ptrdiff_t level, int status);
TOKUI_FUNC int tokuPR_parse(toku_State *T, BuffReader *br, const char *name); 

#endif
