/*
** tprotected.c
** Functions for calling functions in protected mode
** See Copyright Notice in tokudae.h
*/

#define tprotected_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include <setjmp.h>
#include <stdlib.h>

#include "tprotected.h"
#include "tmem.h"
#include "tparser.h"
#include "tobject.h"
#include "tfunction.h"
#include "treader.h"
#include "tstate.h"
#include "tstring.h"
#include "tgc.h"



/*
** {======================================================
** Error-recovery functions
** =======================================================
*/

/*
** TOKUI_THROW/TOKUI_TRY define how Tokudae does exception handling. By
** default, Tokudae handles errors with exceptions when compiling as
** C++ code, with _longjmp/_setjmp when asked to use them, and with
** longjmp/setjmp otherwise.
*/
#if !defined(TOKUI_THROW)				        /* { */

#if defined(__cplusplus) && !defined(TOKU_USE_LONGJMP)	/* { */

/* C++ exceptions */
#define TOKUI_THROW(C,c)		throw(c)
#define TOKUI_TRY(C,c,a) \
	try { a } catch(...) { if ((c)->status == 0) (c)->status = -1; }
#define csi_jmpbuf	        int  /* dummy variable */

#elif defined(TOKU_USE_POSIX)				/* }{ */

/*
** In POSIX, try _longjmp/_setjmp
** (more efficient, does not manipulate signal mask).
*/
#define TOKUI_THROW(C,c)		_longjmp((c)->buf, 1)
#define TOKUI_TRY(C,c,a)		if (_setjmp((c)->buf) == 0) { a }
#define csi_jmpbuf		jmp_buf

#else						        /* }{ */

/* ISO C handling with long jumps */
#define TOKUI_THROW(C,c)		longjmp((c)->buf, 1)
#define TOKUI_TRY(C,c,a)		if (setjmp((c)->buf) == 0) { a }
#define csi_jmpbuf		jmp_buf

#endif							/* } */

#endif							/* } */



/* chain list of long jump buffers */
typedef struct toku_longjmp {
    struct toku_longjmp *prev;
    csi_jmpbuf buf;
    volatile int status;
} toku_longjmp;



void csPR_seterrorobj(toku_State *T, int errcode, SPtr oldtop) {
    switch (errcode) {
        case TOKU_STATUS_EMEM: { /* memory error? */
            setstrval2s(C, oldtop, G(C)->memerror);
            break;
        }
        case TOKU_STATUS_EERROR: { /* error while handling error? */
            setstrval2s(C, oldtop, csS_newlit(C, "error in error handling"));
            break;
        }
        case TOKU_STATUS_OK: { /* closing upvalue? */
            setnilval(s2v(oldtop)); /* no error message */
            break;
        }
        default: {
            toku_assert(errcode > toku_STATUS_OK); /* real error */
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
t_noret csPR_throw(toku_State *T, int errcode) {
    if (C->errjmp) { /* thread has error handler? */
        C->errjmp->status = errcode; /* set status */
        TOKUI_THROW(C, C->errjmp); /* jump to it */
    } else { /* thread has no error handler */
        GState *gs = G(C);
        csT_resetthread(C, errcode); /* close all */
        if (gs->mainthread->errjmp) { /* mainthread has error handler? */
            /* copy over error object */
            setobj2s(C, gs->mainthread->sp.p++, s2v(C->sp.p));
            csPR_throw(C, errcode); /* re-throw in main thread */
        } else { /* no error handlers, abort */
            if (gs->fpanic) { /* state has panic handler? */
                toku_unlock(C); /* release the lock... */
                gs->fpanic(C); /* ...and call it */
            }
            abort();
        }
    }
}


int csPR_rawcall(toku_State *T, ProtectedFn fn, void *ud) {
    t_uint32 old_nCcalls = C->nCcalls;
    toku_longjmp lj;
    lj.status = TOKU_STATUS_OK;
    lj.prev = C->errjmp;
    C->errjmp = &lj;
    TOKUI_TRY(C, &lj, 
        fn(C, ud);
    );
    C->errjmp = lj.prev;
    C->nCcalls = old_nCcalls;
    return lj.status;
}

/* }====================================================== */


int csPR_call(toku_State *T, ProtectedFn fn, void *ud,
              ptrdiff_t old_top, ptrdiff_t ef) {
    int status;
    CallFrame *old_cf = C->cf;
    t_ubyte old_allowhook = C->allowhook;
    ptrdiff_t old_errfunc = C->errfunc;
    C->errfunc = ef;
    status = csPR_rawcall(C, fn, ud);
    if (t_unlikely(status != TOKU_STATUS_OK)) {
        C->cf = old_cf;
        C->allowhook = old_allowhook;
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
static void closep(toku_State *T, void *ud) {
    struct PCloseData *pcd = (struct PCloseData*)ud;
    csF_close(C, pcd->level, pcd->status);
}


/* call 'csF_close' in protected mode */
int csPR_close(toku_State *T, ptrdiff_t level, int status) {
    CallFrame *old_cf = C->cf;
    t_ubyte old_allowhook = C->allowhook;
    for (;;) { /* keep closing upvalues until no more errors */
        struct PCloseData pcd = {
            .level = restorestack(C, level),
            .status = status };
        status = csPR_rawcall(C, closep, &pcd);
        if (t_likely(status == TOKU_STATUS_OK))
            return pcd.status;
        else { /* error occurred; restore saved state and repeat */
            C->cf = old_cf;
            C->allowhook = old_allowhook;
        }
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
static void pparse(toku_State *T, void *userdata) {
    struct PParseData *ppd = cast(struct PParseData *, userdata);
    CSClosure *cl = csP_parse(C, ppd->br, &ppd->buff, &ppd->ps, ppd->source);
    toku_assert(cl->nupvalues == cl->p->sizeupvals);
    csF_initupvals(C, cl);
}


/* call 'csP_parse' in protected mode */
int csPR_parse(toku_State *T, BuffReader *br, const char *name) {
    int status;
    struct PParseData pd = { .br = br, .source = name };
    incnnyc(C);
    status = csPR_call(C, pparse, &pd, savestack(C, C->sp.p), C->errfunc);
    csR_freebuffer(C, &pd.buff);
    csM_freearray(C, pd.ps.actlocals.arr, pd.ps.actlocals.size);
    csM_freearray(C, pd.ps.literals.arr, pd.ps.literals.size);
    csM_freearray(C, pd.ps.gt.arr, pd.ps.gt.size);
    decnnyc(C);
    return status;
}
