#include <string.h>

#include "crstate.h"
#include "crdebug.h"
#include "crgc.h"
#include "crlexer.h"
#include "crmem.h"
#include "crmeta.h"
#include "crprotected.h"
#include "crstring.h"
#include "crhashtable.h"



/* thread state + CR_EXTRASPACE */
typedef struct XS {
    cr_ubyte extra_[CR_EXTRASPACE];
    cr_State ts;
} XS;


/* Main thread + global state */
typedef struct SG {
    XS xs;
    GState gs;
} SG;


/*
** -- Lua 5.4.7 [lstate.c]:58
** Macro for creating "random" seed when a state is created;
** seed is used for randomizing string hashes.
*/
#if !defined(cri_makeseed)
#include <time.h>

#define buffadd(b,p,e) \
    { size_t t = cast_sizet(e); \
      memcpy((b) + (p), &t, sizeof(t)); (p) += sizeof(t); }

static uint cri_makeseed(cr_State *ts) {
    char str[3 * sizeof(size_t)];
    uint seed = time(NULL); /* seed with current time */
    int n = 0;
    buffadd(str, n, ts); /* heap variable */
    buffadd(str, n, &seed); /* local variable */
    buffadd(str, n, &cr_newstate); /* public function */
    cr_assert(n == sizeof(str));
    return crS_hash(str, n, seed);
}
#endif


/*
** Preinitialize all thread fields to avoid collector
** errors.
*/
static void preinitstate(cr_State *ts, GState *gs) {
    ts->ncf = 0;
    ts->status = CR_OK;
    ts->nCC = 0;
    ts->gclist = NULL;
    ts->thwouv = ts; /* if ('ts->thwouv' == 'ts') then no upvalues */
    G_(ts) = gs;
    ts->errjmp = NULL;
    ts->cf = NULL;
    ts->openupval = NULL;
}


/*
** Initialize stack and base call frame for 'newts'.
** 'mts' is main thread state; 'newts' == 'mts' only when
** creating new state.
*/
static void init_stack(cr_State *newts, cr_State *mts) {
    newts->stack.p = crM_newarray(mts, INIT_STACKSIZE + EXTRA_STACK, SValue);
    newts->tbclist.p = newts->stack.p;
    for (int i = 0; i < INIT_STACKSIZE + EXTRA_STACK; i++)
        setnilval(s2v(newts->stack.p + i));
    newts->sp.p = newts->stack.p;
    newts->stackend.p = newts->stack.p + INIT_STACKSIZE;
    CallFrame *cf = &newts->basecf;
    cf->next = cf->prev = NULL;
    cf->callee.p = newts->sp.p;
    cf->top.p = mts->stack.p + CR_MINSTACK;
    cf->pc = NULL;
    cf->nvarargs = 0;
    cf->nreturns = 0;
    cf->cfstatus = CFST_C;
    setnilval(s2v(mts->sp.p)); /* 'cf' entry function */
    mts->sp.p++;
    mts->cf = cf;
}


/*
** Initialize parts of state that may cause memory
** allocation errors.
*/
static void fnewstate(cr_State *ts, void *ud) {
    GState *gs = G_(ts);
    UNUSED(ud);
    init_stack(ts, ts); /* initialize 'ts' stack */
    gs->strings = crH_new(ts); /* new weak strings table */
    gs->globals = crH_new(ts); /* new global table */
    gs->memerror = crS_newlit(ts, MEMERRMSG);
    crG_fix(ts, obj2gco(gs->memerror));
    crMM_init(ts);
    crL_init(ts);
    gs->gc.stopped = 0;
    setnilval(&gs->nil); /* signal that state is fully built */
    cri_userstatecreated(ts);
}


/*
** Allocate new thread and global state with 'falloc' and
** userdata 'ud', from here on 'falloc' will be the allocator.
** The returned thread state is mainthread.
** In case of errors NULL is returned.
*/
CR_API cr_State *cr_newstate(cr_fAlloc falloc, void *ud) {
    GState *gs;
    cr_State *ts;
    SG *sg = falloc(NULL, 0, sizeof(SG), ud);
    if (cr_unlikely(sg == NULL))
        return NULL;
    gs = &sg->gs;
    ts = &sg->xs.ts;
    ts->next = NULL;
    ts->tt_ = CR_VTHREAD;
    crG_init(&gs->gc, ts, sizeof(SG)); /* initialize collector */
    ts->mark = crG_white(&gs->gc);
    preinitstate(ts, gs);
    gs->falloc = falloc;
    gs->udalloc = ud;
    gs->panic = NULL; /* no panic handler by default */
    gs->seed = cri_makeseed(ts); /* initial seed for hashing */
    setival(&gs->nil, 0); /* signals that state is not yet fully initialized */
    gs->mainthread = ts;
    gs->thwouv = NULL;
    for (int i = 0; i < CR_NUM_TYPES; i++)
        gs->vmt[i] = NULL;
    if (crPR_rawcall(ts, fnewstate, NULL) != CR_OK) {
        cr_freestate(ts);
        ts = NULL;
    }
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


/* convert stack pointers into relative stack offsets */
static void stackptrs2offsets(cr_State *ts) {
    ts->sp.offset = savestack(ts, ts->sp.p);
    for (CallFrame *cf = ts->cf; cf != NULL; cf = cf->prev) {
        cf->callee.offset = savestack(ts, cf->callee.p);
        cf->top.offset = savestack(ts, cf->top.p);
    }
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.offset = savestack(ts, uv->v.location);
    ts->tbclist.offset = savestack(ts, ts->tbclist.p);
}


/* convert relative stack offsets into stack pointers */
static void offsets2stackptrs(cr_State *ts) {
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
    stackptrs2offsets(ts);
    gs->gc.stopem = 1; /* no emergency collection when reallocating stack */
    SPtr newstack = crM_reallocarray(ts, ts->stack.p,
            osize + EXTRA_STACK, size + EXTRA_STACK);
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
