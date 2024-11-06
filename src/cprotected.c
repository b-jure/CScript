/*
** cprotected.c
** Functions for calling functions in protected mode
** See Copyright Notice in cscript.h
*/

#include <stdlib.h> /* for 'abort()' */

#include "cprotected.h"
#include "cmem.h"
#include "cparser.h"
#include "cobject.h"
#include "cfunction.h"
#include "creader.h"
#include "cstate.h"


/*
** Throw error to the current thread error handler, mainthread
** error handler or invoke panic if hook for it is present.
** In case none of the above occurs, program is aborted.
*/
cs_noret crPR_throw(cs_State *ts, int errcode) {
    if (ts->errjmp) { /* thread has error handler? */
        ts->errjmp->status = errcode; /* set status */
        CSI_THROW(ts, ts->errjmp); /* jump to it */
    } else { /* thread has no error handler */
        GState *gs = G_(ts);
        crT_resetthread(ts, errcode); /* close all */
        if (gs->mainthread->errjmp) { /* mainthread has error handler? */
            /* copy over error object */
            setobj2s(ts, gs->mainthread->sp.p++, s2v(ts->sp.p));
            crPR_throw(ts, errcode); /* re-throw in main thread */
        } else { /* no error handlers, abort */
            if (gs->fpanic) { /* state has panic handler? */
                cs_unlock(ts); /* release the lock... */
                gs->fpanic(ts); /* ...and call it */
            }
            abort();
        }
    }
}



/* -------------------------------------------------------------------------
 * Protected call
 * ------------------------------------------------------------------------- */

int crPR_rawcall(cs_State *ts, ProtectedFn fn, void *ud) {
    cs_uint32 old_nCcalls = ts->nCcalls;
    struct cs_ljmp lj;
    lj.status = CS_OK;
    lj.prev = ts->errjmp;
    ts->errjmp = &lj;
    CSI_TRY(ts, &lj, 
        fn(ts, ud);
    );
    ts->errjmp = lj.prev;
    ts->nCcalls = old_nCcalls;
    return lj.status;
}


int crPR_call(cs_State *ts, ProtectedFn fn, void *ud, ptrdiff_t old_top,
              ptrdiff_t errfunc) {
    int status;
    CallFrame *old_cf = ts->cf;
    ptrdiff_t old_errfunc = errfunc;
    ts->errfunc = errfunc;
    status = crPR_rawcall(ts, fn, ud);
    if (cs_unlikely(status != CS_OK)) {
        ts->cf = old_cf;
        status = crPR_close(ts, old_top, status);
        crT_seterrorobj(ts, status, restorestack(ts, old_top));
        crT_shrinkstack(ts); /* restore stack (overflow might of happened) */
    }
    ts->errfunc = old_errfunc;
    return status;
}


/* auxiliary structure to call 'crF_close' in protected mode */
struct PCloseData {
    SPtr level;
    int status;
};


/* auxiliary function to call 'crF_close' in protected mode */
static void closepaux(cs_State *ts, void *ud) {
    struct PCloseData *pcd = (struct PCloseData*)ud;
    crF_close(ts, pcd->level, pcd->status);
}


/* call 'crF_close' in protected mode */
int crPR_close(cs_State *ts, ptrdiff_t level, int status) {
    CallFrame *oldcf = ts->cf;
    for (;;) {
        struct PCloseData pcd;
        pcd.level = restorestack(ts, level); pcd.status = status;
        status = crPR_rawcall(ts, closepaux, &pcd);
        if (cs_likely(status == CS_OK))
            return  pcd.status;
        else
            ts->cf = oldcf;
    }
}


/* auxiliary structure to call 'crP_parse' in protected mode */
struct PParseData {
    BuffReader *br;
    Buffer buff;
    ParserState ps;
    const char *source;
};


/* auxiliary function to call 'crP_pparse' in protected mode */
static void parsepaux(cs_State *ts, void *userdata) {
    struct PParseData *ppd = cast(struct PParseData *, userdata);
    CrClosure *cl = crP_parse(ts, ppd->br, &ppd->buff, &ppd->ps, ppd->source);
    crF_initupvals(ts, cl);
}


/* call 'crP_parse' in protected mode */
int crPR_parse(cs_State *ts, BuffReader *br, const char *name) {
    struct PParseData ppd;
    int status;
    ppd.br = br;
    crR_buffinit(&ppd.buff); /* 'buff' */
    ppd.ps.lvars.len = ppd.ps.lvars.size = 0; ppd.ps.lvars.arr = NULL;
    ppd.ps.cs = NULL; /* 'cs' */
    ppd.source = name; /* 'source' */
    status = crPR_call(ts, parsepaux, &ppd, savestack(ts, ts->sp.p), ts->errfunc);
    crR_freebuffer(ts, &ppd.buff);
    crM_freearray(ts, ppd.ps.lvars.arr, ppd.ps.lvars.size, LVar);
    decnnyc(ts);
    return status;
}
