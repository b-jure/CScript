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
** T++ code, with _longjmp/_setjmp when asked to use them, and with
** longjmp/setjmp otherwise.
*/
#if !defined(TOKUI_THROW)				/* { */

#if defined(__cplusplus) && !defined(TOKU_USE_LONGJMP)	/* { */

/* T++ exceptions */
#define TOKUI_THROW(T,c)	throw(c)
#define TOKUI_TRY(T,c,a) \
	try { a } catch(...) { if ((c)->status == 0) (c)->status = -1; }
#define tokui_jmpbuf	        int  /* dummy variable */

#elif defined(TOKU_USE_POSIX)				/* }{ */

/*
** In POSIX, try _longjmp/_setjmp
** (more efficient, does not manipulate signal mask).
*/
#define TOKUI_THROW(T,c)	_longjmp((c)->buf, 1)
#define TOKUI_TRY(T,c,a)	if (_setjmp((c)->buf) == 0) { a }
#define tokui_jmpbuf		jmp_buf

#else						        /* }{ */

/* ISO C handling with long jumps */
#define TOKUI_THROW(T,c)	longjmp((c)->buf, 1)
#define TOKUI_TRY(T,c,a)	if (setjmp((c)->buf) == 0) { a }
#define tokui_jmpbuf		jmp_buf

#endif							/* } */

#endif							/* } */



/* chain list of long jump buffers */
typedef struct toku_longjmp {
    struct toku_longjmp *prev;
    tokui_jmpbuf buf;
    volatile int status;
} toku_longjmp;



void tokuPR_seterrorobj(toku_State *T, int errcode, SPtr oldtop) {
    switch (errcode) {
        case TOKU_STATUS_EMEM: { /* memory error? */
            setstrval2s(T, oldtop, G(T)->memerror);
            break;
        }
        case TOKU_STATUS_EERROR: { /* error while handling error? */
            setstrval2s(T, oldtop, tokuS_newlit(T, "error in error handling"));
            break;
        }
        case TOKU_STATUS_OK: { /* closing upvalue? */
            setnilval(s2v(oldtop)); /* no error message */
            break;
        }
        default: {
            toku_assert(errcode > TOKU_STATUS_OK); /* real error */
            setobjs2s(T, oldtop, T->sp.p - 1); /* error msg on current top */
            break;
        }
    }
    T->sp.p = oldtop + 1;
}


/*
** Throw error to the current thread error handler, mainthread
** error handler or invoke panic if hook for it is present.
** In case none of the above occurs, program is aborted.
*/
t_noret tokuPR_throw(toku_State *T, int errcode) {
    if (T->errjmp) { /* thread has error handler? */
        T->errjmp->status = errcode; /* set status */
        TOKUI_THROW(T, T->errjmp); /* jump to it */
    } else { /* thread has no error handler */
        GState *gs = G(T);
        tokuT_resetthread(T, errcode); /* close all */
        if (gs->mainthread->errjmp) { /* mainthread has error handler? */
            /* copy over error object */
            setobjs2s(T, gs->mainthread->sp.p++, T->sp.p - 1);
            tokuPR_throw(gs->mainthread, errcode); /* re-throw in main th. */
        } else { /* no error handlers, abort */
            if (gs->fpanic) { /* state has panic handler? */
                toku_unlock(T); /* release the lock... */
                gs->fpanic(T); /* ...and call it */
            }
            abort();
        }
    }
}


int tokuPR_rawcall(toku_State *T, ProtectedFn fn, void *ud) {
    t_uint32 old_nCcalls = T->nCcalls;
    toku_longjmp lj;
    lj.status = TOKU_STATUS_OK;
    lj.prev = T->errjmp;
    T->errjmp = &lj;
    TOKUI_TRY(T, &lj, 
        fn(T, ud);
    );
    T->errjmp = lj.prev;
    T->nCcalls = old_nCcalls;
    return lj.status;
}

/* }====================================================== */


int tokuPR_call(toku_State *T, ProtectedFn fn, void *ud,
              ptrdiff_t old_top, ptrdiff_t ef) {
    int status;
    CallFrame *old_cf = T->cf;
    t_ubyte old_allowhook = T->allowhook;
    ptrdiff_t old_errfunc = T->errfunc;
    T->errfunc = ef;
    status = tokuPR_rawcall(T, fn, ud);
    if (t_unlikely(status != TOKU_STATUS_OK)) {
        T->cf = old_cf;
        T->allowhook = old_allowhook;
        status = tokuPR_close(T, old_top, status);
        tokuPR_seterrorobj(T, status, restorestack(T, old_top));
        tokuT_shrinkstack(T); /* restore stack (overflow might of happened) */
    }
    T->errfunc = old_errfunc;
    return status;
}


/* auxiliary structure to call 'tokuF_close' in protected mode */
struct PCloseData {
    SPtr level;
    int status;
};


/* auxiliary function to call 'tokuF_close' in protected mode */
static void closep(toku_State *T, void *ud) {
    struct PCloseData *pcd = (struct PCloseData*)ud;
    tokuF_close(T, pcd->level, pcd->status);
}


/* call 'tokuF_close' in protected mode */
int tokuPR_close(toku_State *T, ptrdiff_t level, int status) {
    CallFrame *old_cf = T->cf;
    t_ubyte old_allowhook = T->allowhook;
    for (;;) { /* keep closing upvalues until no more errors */
        struct PCloseData pcd = {
            .level = restorestack(T, level),
            .status = status };
        status = tokuPR_rawcall(T, closep, &pcd);
        if (t_likely(status == TOKU_STATUS_OK))
            return pcd.status;
        else { /* error occurred; restore saved state and repeat */
            T->cf = old_cf;
            T->allowhook = old_allowhook;
        }
    }
}


/* auxiliary structure to call 'tokuP_parse' in protected mode */
struct PParseData {
    BuffReader *br;
    Buffer buff;
    ParserState ps;
    const char *source;
};


/* auxiliary function to call 'tokuP_pparse' in protected mode */
static void pparse(toku_State *T, void *userdata) {
    struct PParseData *ppd = cast(struct PParseData *, userdata);
    TClosure *cl = tokuP_parse(T, ppd->br, &ppd->buff, &ppd->ps, ppd->source);
    toku_assert(cl->nupvalues == cl->p->sizeupvals);
    tokuF_initupvals(T, cl);
}


/* call 'tokuP_parse' in protected mode */
int tokuPR_parse(toku_State *T, BuffReader *br, const char *name) {
    int status;
    struct PParseData pd = { .br = br, .source = name };
    incnnyc(T);
    status = tokuPR_call(T, pparse, &pd, savestack(T, T->sp.p), T->errfunc);
    tokuR_freebuffer(T, &pd.buff);
    tokuM_freearray(T, pd.ps.actlocals.arr, pd.ps.actlocals.size);
    tokuM_freearray(T, pd.ps.literals.arr, pd.ps.literals.size);
    tokuM_freearray(T, pd.ps.gt.arr, pd.ps.gt.size);
    decnnyc(T);
    return status;
}
