#include "cstate.h"
#include "capi.h"
#include "cdebug.h"
#include "cfunction.h"
#include "cgc.h"
#include "chashtable.h"
#include "cmem.h"
#include "cmeta.h"
#include "cobject.h"
#include "cprotected.h"
#include "cstring.h"


/*
** -- Lua 5.4.7 [lstate.c]:58
** Macro for creating "random" seed when a state is created;
** seed is used for randomizing string hashes.
*/
#if !defined(cri_makeseed)
#include <time.h>
#include <string.h>

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
static void preinit_thread(cr_State *ts, GState *gs) {
    ts->ncf = 0;
    ts->status = CR_OK;
    ts->nCcalls = 0;
    ts->gclist = NULL;
    ts->thwouv = ts; /* if ('ts->thwouv' == 'ts') then no upvalues */
    G_(ts) = gs;
    ts->errjmp = NULL;
    ts->stack.p = NULL;
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
    cf->func.p = newts->sp.p;
    cf->top.p = mts->stack.p + CR_MINSTACK;
    cf->pc = NULL;
    cf->nvarargs = 0;
    cf->nresults = 0;
    cf->status = CFST_CCALL;
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
    sethtval(ts, &gs->globals, crH_new(ts));
    gs->memerror = crS_newlit(ts, MEMERRMSG);
    crG_fix(ts, obj2gco(gs->memerror));
    crMM_init(ts);
    crL_init(ts);
    gs->gc.stopped = 0;
    setnilval(&gs->nil); /* signal that state is fully built */
    cri_userstatecreated(ts);
}


/* free all 'CallFrame' structures NOT in use by thread */
static void freecallframes(cr_State *ts) {
    CallFrame *cf = ts->cf;
    CallFrame *next = cf->next;
    cf->next = NULL;
    while ((cf = next) != NULL) {
        next = cf->next;
        crM_free(ts, cf, sizeof(cf));
        ts->ncf--;
    }
}


/* free thread stack and call frames */
static void freestack(cr_State *ts) {
    if (ts->stack.p == NULL)
        return;
    ts->cf = &ts->basecf;
    freecallframes(ts);
    cr_assert(ts->ncf == 0);
    crM_freearray(ts, s2v(ts->stack.p), stacksize(ts), TValue);
}


/* free global and mainthread state */
static void freestate(cr_State *mt) {
    GState *gs = G_(mt);
    cr_assert(mt == G_(mt)->mainthread);
    if (!gsinitialized(gs)) { /* partially built state? */
        crG_freeallobjects(mt);
    } else {
        mt->cf = &mt->basecf; /* undwind call frame list */
        crPR_close(mt, 1, CR_OK);
        crG_freeallobjects(mt);
        cri_userstatefree(mt);
    }
    freestack(mt);
    cr_assert(totalbytes(&gs->gc) == sizeof(XS));
    gs->falloc(fromstate(mt), sizeof(XS), 0, gs->udalloc);
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
    preinit_thread(ts, gs);
    setnilval(&gs->globals);
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
        freestate(ts);
        ts = NULL;
    }
    return ts;
}


/* free state (global state + mainthread) */
CR_API void cr_freestate(cr_State *mts) {
    cr_lock(ts);
    cr_State *mt = G_(mts)->mainthread;
    freestate(mt);
}


/*
** Create new thread state.
** Argument 'mts' is the main thread created by 'cr_newstate'.
*/
CR_API cr_State *cr_newthread(cr_State *mts) {
    GState *gs = G_(mts);
    cr_State *newts;
    GCObject *o;
    cr_lock(mts);
    o = crG_newoff(mts, sizeof(XS), CR_VTHREAD, offsetof(XS, ts));
    newts = gco2th(o);
    setsv2th(mt, mts->sp.p, newts);
    api_inctop(mts);
    preinit_thread(newts, gs);
    init_stack(newts, mts);
    memcpy(cr_getextraspace(newts), cr_getextraspace(gs->mainthread),
           CR_EXTRASPACE);
    cri_userstatethread(mts, newts);
    cr_unlock(mts);
    return newts;
}


/*
** Reset thread state 'ts' by unwinding `CallFrame` list,
** closing all upvalues (and to-be-closed variables) and
** reseting the stack.
** In case of errors, error object is placed on top of the
** stack and the function returns relevant status code.
** If no errors occured `CR_OK` status is returned.
*/
CR_API int cr_resetthread(cr_State *ts) {
    CallFrame *cf = ts->cf = &ts->basecf;
    int status = ts->status;
    cr_lock(ts);
    ts->status = CR_OK; /* so we can run '__close' */
    status = crPR_close(ts, 1, status);
    if (status != CR_OK) /* error? */
        crT_seterrorobj(ts, status, ts->stack.p + 1);
    else
        ts->sp.p = ts->stack.p + 1;
    cf->top.p = ts->sp.p + CR_MINSTACK;
    crT_reallocstack(ts, cf->top.p - ts->sp.p, 0);
    cr_unlock(ts);
    return status;
}


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
            setobjs2s(ts, oldtop, ts->sp.p - 1); /* error message on current top */
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
** Called when 'getCcalls' is >= CRI_MAXCCALLS.
** If equal to CRI_MAXCCALLS then overflow error is invoked.
** Otherwise it is ignored in order to resolve the current
** overflow error, unless the number of calls is significantly
** higher than CRI_MAXCCALLS.
*/
void crT_checkCstack(cr_State *ts) {
    if (getCcalls(ts) == CRI_MAXCCALLS) /* not handling error ? */
        crD_runerror(ts, "C stack overflow");
    else if (getCcalls(ts) >= (CRI_MAXCCALLS / 10 * 11))
        crPR_throw(ts, CR_ERRERROR);
}


/* Increment number of C calls and check for overflow. */
void crT_incCstack(cr_State *ts) {
    ts->nCcalls++;
    if (getCcalls(ts) >= CRI_MAXCCALLS)
        crT_checkCstack(ts);
}


void crT_free(cr_State *ts, cr_State *thread) {
  XS *xs = fromstate(thread);
  crF_closeupval(thread, thread->stack.p);  /* close all upvalues */
  cr_assert(thread->openupval == NULL);
  cri_userthreadfree(ts, thread);
  freestack(thread);
  crM_free(ts, xs, sizeof(*xs));
}
