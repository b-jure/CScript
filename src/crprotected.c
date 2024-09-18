#include <stdlib.h>

#include "crprotected.h"
#include "crparser.h"
#include "crobject.h"
#include "crfunction.h"
#include "crstate.h"


/*
** Throw error to the current thread error handler, mainthread
** error handler or invoke panic if hook for it is present.
** In case none of the above occurs, program is aborted.
*/
cr_noret crPr_throw(cr_State *ts, int code) {
    if (ts->errjmp) /* thread has error recovery jump ? */
        CRI_THROW(ts, ts->errjmp);
    GState *gs = G_(ts);
    if (gs->mainthread->errjmp) {
        /* copy over error object */
        setobj2s(ts, gs->mainthread->sp.p++, s2v(ts->sp.p));
        crPr_throw(ts, code);
    } else if (gs->panic) {
        cr_unlock(ts);
        gs->panic(ts);
    }
    abort();
}



/* -------------------------------------------------------------------------
 * Protected call
 * ------------------------------------------------------------------------- */

int crPr_rawcall(cr_State *ts, ProtectedFn fn, void *ud) {
    cr_uint32 oldnCC = ts->nCC;
    struct cr_ljmp lj;
    lj.status = CR_OK;
    lj.prev = ts->errjmp;
    ts->errjmp = &lj;
    CRI_TRY(ts, &lj, 
        fn(ts, ud);
    );
    ts->errjmp = lj.prev;
    ts->nCC = oldnCC;
    return lj.status;
}


int crPr_call(cr_State *ts, ProtectedFn fn, void *ud, ptrdiff_t top) {
    int status = crPr_rawcall(ts, fn, ud);
    if (cr_unlikely(status != CR_OK)) {
        ts->sp.p = restorestack(ts, top);
    }
    return status;
}



/* -------------------------------------------------------------------------
 * Protected parser
 * ------------------------------------------------------------------------- */

/* data for 'pparse' */
typedef struct PPData {
    BuffReader br;
    Buffer buff;
    ParserState ps;
    const char *source;
} PPData;


/* protected 'parse' */
static void pparse(cr_State *ts, void *userdata)
{
    PPData *parsedata = cast(PPData *, userdata);
    CrClosure *cl = crP_parse(ts, &parsedata->br, &parsedata->buff, &parsedata->ps,
                          parsedata->source);
    cr_assert(cl->nupvals <= cl->sizeupvals);
    crF_initupvals(ts, cl);
}


/* external interface for 'pparse' */
void crP_pparse(cr_State *ts, cr_fReader freader, void *userdata,
                      const char *name)
{
    PPData parsedata;
    ParserState *ps = &parsedata.ps;
    crR_init(ts, &parsedata.br, freader, userdata); /* 'br' */
    crR_buffinit(&parsedata.buff); /* 'buff' */
    { ps->lvars.len = ps->lvars.size = 0; ps->lvars.arr = NULL; } /* 'lvars' */
    ps->cs = NULL; /* 'cs' */
    parsedata.source = name; /* 'source' */
    crPr_call(ts, pparse, &parsedata, savestack(ts, ts->sp.p));
}
