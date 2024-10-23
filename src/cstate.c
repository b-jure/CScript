#include "cstate.h"
#include "cdebug.h"
#include "cgc.h"
#include "cmem.h"
#include "cobject.h"
#include "cprotected.h"
#include "cstring.h"


void crT_seterrorobj(cr_State *ts, int errcode, SPtr oldtop) {
    switch (errcode) {
        case CR_ERRMEM: { /* memory error? */
            setstrval2s(ts, oldtop, G_(ts)->memerror);
            break;
        }
        case CR_ERRERROR: { /* error while handling error? */
            setstrval2s(ts, oldtop, crS_newlit(ts, "error in error handling"));
            break;
        }
        case CR_OK: { /* closing upvalue? */
            setnilval(s2v(oldtop)); /* no error message */
            break;
        }
        default: { /* real error? */
            cr_assert(errcode > CR_OK);
            setobjs2s(L, oldtop, ts->sp.p - 1); /* error message on current top */
            break;
        }
    }
    ts->sp.p = oldtop + 1;
}


/*
 * Stack size to grow the stack to when stack
 * overflow occurs for error handling.
 */
#define OVERFLOWSTACKSIZE       (CRI_MAXSTACK + 200)


CallFrame *crT_newcf(cr_State *ts) {
    CallFrame *cf;
    cr_assert(ts->cf->next == NULL);
    cf = crM_malloc(ts, sizeof(*cf));
    cr_assert(ts->cf->next == NULL);
    ts->cf->next = cf;
    cf->prev = ts->cf;
    cf->next = NULL;
    ts->ncf++;
    return cf;
}


/* convert stack pointers into relative stack offsets */
static void stackptrs2offsets(cr_State *ts) {
    ts->sp.offset = savestack(ts, ts->sp.p);
    for (CallFrame *cf = ts->cf; cf != NULL; cf = cf->prev) {
        cf->func.offset = savestack(ts, cf->func.p);
        cf->top.offset = savestack(ts, cf->top.p);
    }
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.offset = savestack(ts, uv->v.p);
    ts->tbclist.offset = savestack(ts, ts->tbclist.p);
}


/* convert relative stack offsets into stack pointers */
static void offsets2stackptrs(cr_State *ts) {
    ts->sp.p = restorestack(ts, ts->sp.offset);
    for (CallFrame *cf = ts->cf; cf != NULL; cf = cf->prev) {
        cf->func.p = restorestack(ts, cf->func.offset);
        cf->top.p = restorestack(ts, cf->top.offset);
    }
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.p = s2v(restorestack(ts, uv->v.offset));
    ts->tbclist.p = restorestack(ts, ts->tbclist.offset);
}


/* reallocate stack to new size */
int crT_reallocstack(cr_State *ts, int size, int raiseerr) {
    GState *gs = G_(ts);
    int oldstopem = gs->gc.stopem;
    int osz = stacksize(ts);
    cr_assert(size <= CRI_MAXSTACK || size == OVERFLOWSTACKSIZE);
    stackptrs2offsets(ts);
    gs->gc.stopem = 1; /* no emergency collection when reallocating stack */
    SPtr newstack = crM_reallocarray(ts, ts->stack.p, osz + EXTRA_STACK,
                                     size + EXTRA_STACK, SValue);
    gs->gc.stopem = oldstopem;
    if (cr_unlikely(newstack == NULL)) {
        offsets2stackptrs(ts);
        if (raiseerr)
            crPR_throw(ts, CR_ERRMEM);
        return 0;
    }
    offsets2stackptrs(ts);
    ts->stack.p = newstack;
    ts->stackend.p = newstack + size;
    for (int i = osz + EXTRA_STACK; i < size + EXTRA_STACK; i++)
        setnilval(s2v(newstack + i));
    return 1;
}


/* grow stack to accommodate 'n' values */
int crT_growstack(cr_State *ts, int n, int raiseerr) {
    int size = stacksize(ts);
    if (cr_unlikely(size > CRI_MAXSTACK)) { /* overflowed already ? */
        cr_assert(size == OVERFLOWSTACKSIZE);
        if (raiseerr)
            crPR_throw(ts, CR_ERRERROR);
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
    crT_checkstack(ts, 1);
    ts->sp.p++;
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
        crPR_throw(ts, CR_ERRERROR);
}


/* Increment number of nested C calls and check for overflow. */
void crT_incC_(cr_State *ts) {
    ts->nCC++;
    if (getnC_(ts) >= CRI_MAXCCALLS)
        crT_checkCstack(ts);
}


void crT_free(cr_State *ts, cr_State *thread) {
    
}
