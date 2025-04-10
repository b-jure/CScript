/*
** cprotected.c
** Functions for calling functions in protected mode
** See Copyright Notice in cscript.h
*/


#define CS_CORE

#include "cprefix.h"

#include <setjmp.h>
#include <stdlib.h>

#include "cprotected.h"
#include "cmem.h"
#include "cparser.h"
#include "cobject.h"
#include "cfunction.h"
#include "creader.h"
#include "cstate.h"
#include "cstring.h"
#include "cgc.h"



/*
** {======================================================
** Error-recovery functions
** =======================================================
*/

/*
** CSI_THROW/CSI_TRY define how CScript does exception handling. By
** default, CScript handles errors with exceptions when compiling as
** C++ code, with _longjmp/_setjmp when asked to use them, and with
** longjmp/setjmp otherwise.
*/
#if !defined(CSI_THROW)				        /* { */

#if defined(__cplusplus) && !defined(CS_USE_LONGJMP)	/* { */

/* C++ exceptions */
#define CSI_THROW(C,c)		throw(c)
#define CSI_TRY(C,c,a) \
	try { a } catch(...) { if ((c)->status == 0) (c)->status = -1; }
#define csi_jmpbuf	        int  /* dummy variable */

#elif defined(CS_USE_POSIX)				/* }{ */

/*
** In POSIX, try _longjmp/_setjmp
** (more efficient, does not manipulate signal mask).
*/
#define CSI_THROW(C,c)		_longjmp((c)->buf, 1)
#define CSI_TRY(C,c,a)		if (_setjmp((c)->buf) == 0) { a }
#define csi_jmpbuf		jmp_buf

#else						        /* }{ */

/* ISO C handling with long jumps */
#define CSI_THROW(C,c)		longjmp((c)->buf, 1)
#define CSI_TRY(C,c,a)		if (setjmp((c)->buf) == 0) { a }
#define csi_jmpbuf		jmp_buf

#endif							/* } */

#endif							/* } */



/* chain list of long jump buffers */
typedef struct cs_longjmp {
    struct cs_longjmp *prev;
    csi_jmpbuf buf;
    volatile int status;
} cs_longjmp;



void csPR_seterrorobj(cs_State *C, int errcode, SPtr oldtop) {
    switch (errcode) {
        case CS_ERRMEM: { /* memory error? */
            setstrval2s(C, oldtop, G(C)->memerror);
            break;
        }
        case CS_ERRERROR: { /* error while handling error? */
            setstrval2s(C, oldtop, csS_newlit(C, "error in error handling"));
            break;
        }
        case CS_OK: { /* closing upvalue? */
            setnilval(s2v(oldtop)); /* no error message */
            break;
        }
        default: {
            cs_assert(errcode > CS_OK); /* real error */
            setobjs2s(C, oldtop, C->sp.p - 1); /* error msg on current top */
            break;
        }
    }
    C->sp.p = oldtop + 1;
}


/*
** Throw error to the current thread error handler, mainthread
** error handler or invoke panic if hook for it is present.
** In case none of the above occurs, program is aborted.
*/
c_noret csPR_throw(cs_State *C, int errcode) {
    if (C->errjmp) { /* thread has error handler? */
        C->errjmp->status = errcode; /* set status */
        CSI_THROW(C, C->errjmp); /* jump to it */
    } else { /* thread has no error handler */
        GState *gs = G(C);
        csT_resetthread(C, errcode); /* close all */
        if (gs->mainthread->errjmp) { /* mainthread has error handler? */
            /* copy over error object */
            setobj2s(C, gs->mainthread->sp.p++, s2v(C->sp.p));
            csPR_throw(C, errcode); /* re-throw in main thread */
        } else { /* no error handlers, abort */
            if (gs->fpanic) { /* state has panic handler? */
                cs_unlock(C); /* release the lock... */
                gs->fpanic(C); /* ...and call it */
            }
            abort();
        }
    }
}


int csPR_rawcall(cs_State *C, ProtectedFn fn, void *ud) {
    c_uint32 old_nCcalls = C->nCcalls;
    cs_longjmp lj;
    lj.status = CS_OK;
    lj.prev = C->errjmp;
    C->errjmp = &lj;
    CSI_TRY(C, &lj, 
        fn(C, ud);
    );
    C->errjmp = lj.prev;
    C->nCcalls = old_nCcalls;
    return lj.status;
}

/* }====================================================== */


int csPR_call(cs_State *C, ProtectedFn fn, void *ud, ptrdiff_t old_top,
              ptrdiff_t errfunc) {
    int status;
    CallFrame *old_cf = C->cf;
    ptrdiff_t old_errfunc = C->errfunc;
    C->errfunc = errfunc;
    status = csPR_rawcall(C, fn, ud);
    if (c_unlikely(status != CS_OK)) {
        C->cf = old_cf;
        status = csPR_close(C, old_top, status);
        csPR_seterrorobj(C, status, restorestack(C, old_top));
        csT_shrinkstack(C); /* restore stack (overflow might of happened) */
    }
    C->errfunc = old_errfunc;
    return status;
}


/* auxiliary structure to call 'csF_close' in protected mode */
struct PCloseData {
    SPtr level;
    int status;
};


/* auxiliary function to call 'csF_close' in protected mode */
static void closepaux(cs_State *C, void *ud) {
    struct PCloseData *pcd = (struct PCloseData*)ud;
    csF_close(C, pcd->level, pcd->status);
}


/* call 'csF_close' in protected mode */
int csPR_close(cs_State *C, ptrdiff_t level, int status) {
    CallFrame *old_cf = C->cf;
    for (;;) { /* keep closing upvalues until no more errors */
        struct PCloseData pcd;
        pcd.level = restorestack(C, level);
        pcd.status = status;
        status = csPR_rawcall(C, closepaux, &pcd);
        if (c_likely(status == CS_OK))
            return pcd.status;
        else /* error occurred; restore saved state and repeat */
            C->cf = old_cf;
    }
}


/* auxiliary structure to call 'csP_parse' in protected mode */
struct PParseData {
    BuffReader *br;
    Buffer buff;
    ParserState ps;
    const char *source;
};


/* auxiliary function to call 'csP_pparse' in protected mode */
static void parsepaux(cs_State *C, void *userdata) {
    struct PParseData *ppd = cast(struct PParseData *, userdata);
    CSClosure *cl = csP_parse(C, ppd->br, &ppd->buff, &ppd->ps, ppd->source);
    csF_initupvals(C, cl);
}


/* call 'csP_parse' in protected mode */
int csPR_parse(cs_State *C, BuffReader *br, const char *name) {
    struct PParseData pd;
    int status;
    incnnyc(C);
    pd.br = br;
    csR_buffinit(&pd.buff);
    pd.ps.actlocals.len = pd.ps.actlocals.size = 0; pd.ps.actlocals.arr = NULL;
    pd.ps.patches.len = pd.ps.patches.size = 0; pd.ps.patches.arr = NULL;
    pd.ps.literals.len = pd.ps.literals.size = 0; pd.ps.literals.arr = NULL;
    pd.ps.cs = NULL;
    pd.source = name;
    status = csPR_call(C, parsepaux, &pd, savestack(C, C->sp.p), C->errfunc);
    csR_freebuffer(C, &pd.buff);
    csM_freearray(C, pd.ps.actlocals.arr, pd.ps.actlocals.size);
    for (int i = 0; i < pd.ps.patches.len; i++) {
        PatchList *l = &pd.ps.patches.arr[i];
        csM_freearray(C, l->arr, l->size);
    }
    csM_freearray(C, pd.ps.patches.arr, pd.ps.patches.size);
    csM_freearray(C, pd.ps.literals.arr, pd.ps.literals.size);
    decnnyc(C);
    return status;
}
