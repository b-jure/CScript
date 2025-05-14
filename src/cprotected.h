/*
** cprotected.h
** Functions for calling functions in protected mode
** See Copyright Notice in cscript.h
*/

#ifndef cprotected_h
#define cprotected_h


#include "creader.h"
#include "cobject.h"


/* type for functions with error handler */
typedef void (*ProtectedFn)(cs_State *C, void *userdata);


/* save/restore stack position */
#define savestack(C,ptr)	(cast_charp(ptr) - cast_charp((C)->stack.p))
#define restorestack(C,n)	cast(SPtr, cast_charp((C)->stack.p) + (n))


/* 
** Check if stack nees to grow if so, do 'pre' then grow and
** then do 'pos'.
*/
#define csPR_checkstackaux(C,n,pre,pos) \
    if (csi_unlikely((C)->stackend.p - (C)->sp.p <= (n))) \
        { pre; csT_growstack(C, (n), 1); pos; } \
    else { condmovestack(C, pre, pos); }


/* check if stack needs to grow */
#define csPR_checkstack(C,n)    csPR_checkstackaux(C,n,(void)0,(void)0)


/* check if stack needs to grow, preserving 'p' */
#define checkstackp(C,n,p) \
        csPR_checkstackaux(C, n, \
            ptrdiff_t p_ = savestack(C, p), \
            p = restorestack(C, p_))


/* check GC then check stack, preserving 'p' */
#define checkstackGCp(C,n,p) \
        csPR_checkstackaux(C,n, \
            ptrdiff_t p_ = savestack(C,p); csG_checkGC(C), \
            p = restorestack(C, p_))


/* check GC then check stack */
#define checkstackGC(C,n)   csPR_checkstackaux(C,n,csG_checkGC(C),(void)0)


CSI_FUNC void csPR_seterrorobj(cs_State *C, int errcode, SPtr oldtop);
CSI_FUNC c_noret csPR_throw(cs_State *C, int code);
CSI_FUNC int csPR_rawcall(cs_State *C, ProtectedFn fn, void *ud);
CSI_FUNC int csPR_call(cs_State *C, ProtectedFn fn, void *ud, ptrdiff_t top,
                       ptrdiff_t errfunc);
CSI_FUNC int csPR_close(cs_State *C, ptrdiff_t level, int status);
CSI_FUNC int csPR_parse(cs_State *C, BuffReader *br, const char *name); 

#endif
