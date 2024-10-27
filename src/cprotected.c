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
cr_noret crPR_throw(cr_State *ts, int code) {
    if (ts->errjmp) /* thread has error recovery jump ? */
        CRI_THROW(ts, ts->errjmp);
    GState *gs = G_(ts);
    if (gs->mainthread->errjmp) {
        /* copy over error object */
        setobj2s(ts, gs->mainthread->sp.p++, s2v(ts->sp.p));
        crPR_throw(ts, code);
    } else if (gs->panic) {
        cr_unlock(ts);
        gs->panic(ts);
    }
    abort();
}



/* -------------------------------------------------------------------------
 * Protected call
 * ------------------------------------------------------------------------- */

int crPR_rawcall(cr_State *ts, ProtectedFn fn, void *ud) {
    cr_uint32 old_nCcalls = ts->nCcalls;
    struct cr_ljmp lj;
    lj.status = CR_OK;
    lj.prev = ts->errjmp;
    ts->errjmp = &lj;
    CRI_TRY(ts, &lj, 
        fn(ts, ud);
    );
    ts->errjmp = lj.prev;
    ts->nCcalls = old_nCcalls;
    return lj.status;
}


int crPR_call(cr_State *ts, ProtectedFn fn, void *ud, ptrdiff_t old_top) {
    int status;
    CallFrame *old_cf = ts->cf;
    status = crPR_rawcall(ts, fn, ud);
    if (cr_unlikely(status != CR_OK)) {
        ts->cf = old_cf;
        status = crPR_close(ts, old_top, status);
        crT_seterrorobj(ts, status, restorestack(ts, old_top));
        crT_shrinkstack(ts);
    }
    return status;
}


/* auxiliary structure to call 'crF_close' in protected mode */
struct PCloseData {
    SPtr level;
    int status;
};


/* auxiliary function to call 'crF_close' in protected mode */
static void closepaux(cr_State *ts, void *ud) {
    struct PCloseData *pcd = (struct PCloseData*)ud;
    crF_close(ts, pcd->level, pcd->status);
}


/* call 'crF_close' in protected mode */
int crPR_close(cr_State *ts, ptrdiff_t level, int status) {
    CallFrame *oldcf = ts->cf;
    for (;;) {
        struct PCloseData pcd;
        pcd.level = restorestack(ts, level); pcd.status = status;
        status = crPR_rawcall(ts, closepaux, &pcd);
        if (cr_likely(status == CR_OK))
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
static void parsepaux(cr_State *ts, void *userdata) {
    CrClosure *cl;
    struct PParseData *ppd = cast(struct PParseData *, userdata);
    cl = crP_parse(ts, ppd->br, &ppd->buff, &ppd->ps, ppd->source);
    crF_initupvals(ts, cl);
}


/* call 'crP_parse' in protected mode */
int crPR_parse(cr_State *ts, BuffReader *br, const char *name) {
    struct PParseData ppd;
    int status;
    ppd.br = br;
    crR_buffinit(&ppd.buff); /* 'buff' */
    ppd.ps.lvars.len = ppd.ps.lvars.size = 0; ppd.ps.lvars.arr = NULL;
    ppd.ps.cs = NULL; /* 'cs' */
    ppd.source = name; /* 'source' */
    status = crPR_call(ts, parsepaux, &ppd, savestack(ts, ts->sp.p));
    crR_freebuffer(ts, &ppd.buff);
    crM_freearray(ts, ppd.ps.lvars.arr, ppd.ps.lvars.size, LVar);
    decnnyc(ts);
    return status;
}
