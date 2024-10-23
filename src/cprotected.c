#include <stdlib.h>

#include "cprotected.h"
#include "cparser.h"
#include "cobject.h"
#include "cfunction.h"
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


int crPR_call(cr_State *ts, ProtectedFn fn, void *ud, ptrdiff_t top) {
    int status = crPR_rawcall(ts, fn, ud);
    if (cr_unlikely(status != CR_OK)) {
        ts->sp.p = restorestack(ts, top);
    }
    return status;
}


/* auxiliary structure to call 'crF_close' in protected mode */
struct CloseData {
    SPtr level;
    int status;
};


/* auxiliary function to call 'crF_close' in protected mode */
static void closepaux(cr_State *ts, void *ud) {
    struct CloseData *cd = (struct CloseData*)ud;
    crF_close(ts, cd->level, cd->status);
}


/* call 'crF_close' in protected mode */
int crPR_close(cr_State *ts, ptrdiff_t level, int status) {
    CallFrame *oldcf = ts->cf;
    for (;;) {
        struct CloseData cd;
        cd.level = restorestack(ts, level); cd.status = status;
        status = crPR_rawcall(ts, closepaux, &cd);
        if (cr_likely(status == CR_OK))
            return  cd.status;
        else
            ts->cf = oldcf;
    }
}



/* auxiliary structure to call 'crP_parse' in protected mode */
typedef struct PPData {
    BuffReader br;
    Buffer buff;
    ParserState ps;
    const char *source;
} PPData;


/* auxiliary function to call 'crP_pparse' in protected mode */
static void parsepaux(cr_State *ts, void *userdata)
{
    PPData *pd = cast(PPData *, userdata);
    CrClosure *cl = crP_parse(ts, &pd->br, &pd->buff, &pd->ps, pd->source);
    crF_initupvals(ts, cl);
}


/* call 'crP_parse' in protected mode */
void crPR_parse(cr_State *ts, cr_fReader freader, void *userdata,
                      const char *name)
{
    PPData parsedata;
    ParserState *ps = &parsedata.ps;
    crR_init(ts, &parsedata.br, freader, userdata); /* 'br' */
    crR_buffinit(&parsedata.buff); /* 'buff' */
    { ps->lvars.len = ps->lvars.size = 0; ps->lvars.arr = NULL; } /* 'lvars' */
    ps->cs = NULL; /* 'cs' */
    parsedata.source = name; /* 'source' */
    crPR_call(ts, parsepaux, &parsedata, savestack(ts, ts->sp.p));
}
