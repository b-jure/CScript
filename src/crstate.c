#include <stdlib.h>

#include "crstate.h"
#include "crdebug.h"
#include "crgc.h"
#include "crmem.h"





static void preinitstate(cr_State *ts) {
    // TODO
}


CR_API cr_State *cr_newstate(cr_fAlloc falloc, void *ud) {
    GState *gs;
    cr_State *ts;
    // TODO
    gs->falloc = falloc;
    cr_gc_init(&gs->gc);
    preinitstate(ts);
    return ts;
}


/*
 * Stack size to grow the stack to when stack
 * overflow occurs for error handling.
 */
#define OVERFLOWSTACKSIZE       (CRI_MAXSTACK + 200)


CallFrame *crT_newcf(cr_State *ts) {
    CallFrame *cf = crM_malloc(ts, sizeof(*cf));
    cr_assert(ts->frame->next == NULL);
    ts->cf->next = cf;
    cf->prev = ts->cf;
    cf->next = NULL;
    ts->ncf++;
    return cf;
}


/* convert stack pointers into stack offsets */
static void savestackptrs(cr_State *ts) {
    ts->sp.offset = savestack(ts, ts->sp.p);
    for (CallFrame *cf = ts->cf; cf != NULL; cf = cf->prev) {
        cf->callee.offset = savestack(ts, cf->callee.p);
        cf->top.offset = savestack(ts, cf->top.p);
    }
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.offset = savestack(ts, uv->v.location);
    ts->tbclist.offset = savestack(ts, ts->tbclist.p);
}


/* convert stack offsets back to stack pointers */
static void restorestackptrs(cr_State *ts) {
    ts->sp.p = restorestack(ts, ts->sp.offset);
    for (CallFrame *cf = ts->cf; cf != NULL; cf = cf->prev) {
        cf->callee.p = restorestack(ts, cf->callee.offset);
        cf->top.p = restorestack(ts, cf->top.offset);
    }
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.location = s2v(restorestack(ts, uv->v.offset));
    ts->tbclist.p = restorestack(ts, ts->tbclist.offset);
}


/* reallocate stack to new size */
int crT_reallocstack(cr_State *ts, int size, int raiseerr) {
    cr_assert(nsize <= CRI_MAXSTACK || nsize == OVERFLOWSTACKSIZE);
    GState *gs = G_(ts);
    int oldstopem = gs->gc.stopem;
    int osize = stacksize(ts);
    savestackptrs(ts);
    gs->gc.stopem = 1; /* no emergency collection when reallocating stack */
    SPtr newstack = crM_reallocarray(ts, ts->stack.p,
            osize + EXTRA_STACK, size + EXTRA_STACK);
    gs->gc.stopem = oldstopem;
    if (cr_unlikely(newstack == NULL)) {
        restorestackptrs(ts);
        if (raiseerr)
            crT_throw(ts, CR_ERRMEM);
        return 0;
    }
    restorestackptrs(ts);
    ts->stack.p = newstack;
    ts->stackend.p = newstack + size;
    for (int i = osize + EXTRA_STACK; i < size + EXTRA_STACK; i++)
        setnilval(s2v(newstack + i));
    return 1;
}


/* grow stack to accommodate 'n' values */
int crT_growstack(cr_State *ts, int n, int raiseerr) {
    int size = stacksize(ts);
    if (cr_unlikely(size > CRI_MAXSTACK)) { /* overflowed already ? */
        cr_assert(size == OVERFLOWSTACKSIZE);
        if (raiseerr)
            crT_throw(ts, CR_ERRERROR);
        return 0;
    }
    if (cr_unlikely(n > CRI_MAXSTACK)) {
        int nsize = size << 1;
        int needed = topoffset(ts) + n;
        if (nsize > CRI_MAXSTACK)
            nsize = CRI_MAXSTACK;
        if (nsize < needed)
            nsize = needed;
        if (cr_likely(nsize <= CRI_MAXSTACK))
            return crT_reallocstack(ts, nsize, raiseerr);
    }
    crT_reallocstack(ts, OVERFLOWSTACKSIZE, raiseerr);
    if (raiseerr)
        crD_runerror(ts, "stack overflow");
    return 0;
}


static int stackinuse(cr_State *ts) {
    SPtr maxtop = ts->cf->top.p;
    for (CallFrame *cf = ts->cf->prev; cf != NULL; cf = cf->prev) {
        if (maxtop < cf->top.p)
            maxtop = cf->top.p;
    }
    cr_assert(maxtop <= ts->stackend.p + EXTRA_STACK);
    int n = savestack(ts, maxtop);
    if (n < CR_MINSTACK)
        n = CR_MINSTACK;
    return n;
}


/*
 * Shrink stack if the current stack size is more
 * than 3 times the current use.
 * This also rolls back the stack to its original maximum
 * size 'CRI_MAXSTACK' in case the stack was previously
 * handling stack overflow.
 */
void crT_shrinkstack(cr_State *ts) {
    int inuse = stackinuse(ts);
    int limit = (inuse >= CRI_MAXSTACK / 3 ? CRI_MAXSTACK : inuse * 3);
    if (inuse <= CRI_MAXSTACK && stacksize(ts) > limit) {
        int nsize = (inuse < (CRI_MAXSTACK / 2) ? (inuse * 2) : CRI_MAXSTACK);
        crT_reallocstack(ts, nsize, 0); /* this can fail */
    }
}


/* increment stack pointer */
void crT_incsp(cr_State *ts) {
    checkstack(ts, 1);
    ts->sp.p++;
}

cr_noret crT_throw(cr_State *ts, int code)
{
    if (ts->errjmp) /* thread has error recovery jump ? */
        CRI_THROW(ts, ts->errjmp);
    GState *gs = G_(ts);
    if (gs->mainthread->errjmp) {
        /* copy over error object */
        setsval(ts, gs->mainthread->sp.p++, s2v(ts->sp.p));
        crT_throw(ts, code);
    } else if (gs->panic) {
        cr_unlock(ts);
        gs->panic(ts);
    }
    abort();
}


/*
** Called when 'getnC_' is >= CRI_MAXCCALLS.
** If equal to CRI_MAXCCALLS then overflow error is invoked.
** Otherwise it is ignored in order to resolve the current
** overflow error, unless the number of calls is significantly
** higher than CRI_MAXCCALLS.
*/
void crT_checkCstack(cr_State *ts) {
    if (getnC_(ts) == CRI_MAXCCALLS) /* not handling erorr ? */
        crD_runerror(ts, "C stack overflow");
    else if (getnC_(ts) >= (CRI_MAXCCALLS / 10 * 11))
        crT_throw(ts, CR_ERRERROR);
}


/* Increment number of nested C calls and check for overflow. */
void crT_incC_(cr_State *ts) {
    ts->nCC++;
    if (getnC_(ts) >= CRI_MAXCCALLS)
        crT_checkCstack(ts);
}
