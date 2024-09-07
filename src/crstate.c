#include "crstate.h"
#include "crdebug.h"
#include "crgc.h"


#define CRI_THROW(ts, b) 	longjmp((b)->buf, 1)


static void preinitstate(cr_State *ts)
{
    // TODO
}


CR_API cr_State *cr_newstate(cr_alloc allocator, void *ud)
{
    GState *gs;
    cr_State *ts;
    // TODO
    gs->realloc = allocator;
    cr_gc_init(&gs->gc);
    preinitstate(ts);
    return ts;
}


cr_noret cr_state_throw(cr_State *ts, int code)
{
    if (ts->errjmp) /* thread has error recovery jump ? */
        CRI_THROW(ts, ts->errjmp);
    GState *gs = GS(ts);
    if (gs->mainthread->errjmp) {
        /* copy over error object */
        setsval(ts, gs->mainthread->stacktop.p++, s2v(ts->stacktop.p));
        cr_state_throw(ts, code);
    } else if (gs->panic) {
        cr_unlock(ts);
        gs->panic(ts);
    }
    abort();
}


void cr_state_inccalls(cr_State *ts)
{
    ts->ncalls++;
    if (ts->ncalls >= CRI_MAXCCALLS) {
        if (ts->ncalls == CRI_MAXCCALLS) /* not handling erorr ? */
            crD_runerror(ts, "C stack overflow");
        else if (ts->ncalls >= CRI_MAXCCALLS / 10 * 11)
            cr_state_throw(ts, CR_ERRERROR);
    }
}


