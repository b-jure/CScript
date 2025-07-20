/*
** tstate.c
** Global and Thread state
** See Copyright Notice in tokudae.h
*/

#define tstate_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include <string.h>

#include "ttable.h"
#include "tlist.h"
#include "tstate.h"
#include "tapi.h"
#include "tdebug.h"
#include "tfunction.h"
#include "tgc.h"
#include "tmem.h"
#include "tmeta.h"
#include "tobject.h"
#include "tprotected.h"
#include "tokudae.h"
#include "tstring.h"



/*
** Preinitialize all thread fields to avoid collector
** errors.
*/
static void preinit_thread(toku_State *T, GState *gs) {
    T->ncf = 0;
    T->status = TOKU_STATUS_OK;
    T->errfunc = 0;
    T->nCcalls = 0;
    T->gclist = NULL;
    T->twups = T; /* if ('T->twups' == 'T') then no upvalues */
    G(T) = gs;
    T->errjmp = NULL;
    T->hook = NULL;
    T->hookmask = 0;
    T->oldpc = 0;
    T->basehookcount = 0;
    T->allowhook = 1;
    resethookcount(T);
    T->stack.p = T->sp.p = T->stackend.p = NULL;
    T->cf = NULL;
    T->openupval = NULL;
    T->tbclist.p = NULL;
    T->basecf.prev = T->basecf.next = NULL;
    T->transferinfo.ftransfer = T->transferinfo.ntransfer = 0;
}


static void resetCF(toku_State *T) {
    CallFrame *cf = T->cf = &T->basecf;
    cf->func.p = T->stack.p;
    setnilval(s2v(cf->func.p)); /* 'function' entry for basic 'cf' */
    cf->top.p = cf->func.p + 1 + TOKU_MINSTACK; /* +1 for 'function' entry */
    cf->status = CFST_CCALL;
    cf->t.pc = cf->t.pcret = NULL;
    cf->t.trap = 0;
    cf->t.nvarargs = 0;
    cf->nresults = 0;
    T->status = TOKU_STATUS_OK;
    T->errfunc = 0; /* stack unwind can "throw away" the error function */
}


/*
** Initialize stack and base call frame for 'T'.
** 'T' is a main thread state ('T1' == 'T' only when creating new
** state).
*/
static void stackinit(toku_State *T1, toku_State *T) {
    toku_assert(!statefullybuilt(G(T1)) == (T1 == T));
    T1->stack.p = tokuM_newarray(T, INIT_STACKSIZE + EXTRA_STACK, SValue);
    T1->tbclist.p = T1->stack.p;
    for (int i = 0; i < INIT_STACKSIZE + EXTRA_STACK; i++)
        setnilval(s2v(T1->stack.p + i));
    T1->stackend.p = T1->stack.p + INIT_STACKSIZE;
    resetCF(T1);
    T1->sp.p = T1->stack.p + 1; /* +1 for function */
}


static void init_cstorage(toku_State *T, GState *gs) {
    List *clist = tokuA_newl(T, TOKU_CLIST_LAST + 1); 
    /* gs->c_list = list */
    setlistval(T, &gs->c_list, clist);
    /* clist[TOKU_CLIST_MAINTHREAD] = T (mainthread) */
    setthval(T, &clist->arr[TOKU_CLIST_MAINTHREAD], T);
    /* clist[TOKU_CLIST_GLOBALS] = global table */
    settval(T, &clist->arr[TOKU_CLIST_GLOBALS], tokuH_new(T));
    /* gs->c_table = table */
    settval(T, &gs->c_table, tokuH_new(T));
}


/*
** Initializes parts of state that may cause memory allocation
** errors.
*/
static void f_newstate(toku_State *T, void *ud) {
    GState *gs = G(T);
    UNUSED(ud);
    stackinit(T, T);
    init_cstorage(T, gs);
    tokuS_init(T); /* keep this init first */
    tokuY_init(T);
    tokuMM_init(T);
    tokuA_init(T);
    gs->gcstop = 0;
    setnilval(&gs->nil); /* signal that state is fully built */
    tokui_userstateopen(T);
}


/*
** Free all 'CallFrame' structures NOT in use by a thread.
*/
static void freeCF(toku_State *T) {
    CallFrame *cf = T->cf;
    CallFrame *next = cf->next;
    cf->next = NULL;
    while ((cf = next) != NULL) {
        next = cf->next;
        tokuM_free(T, cf);
        T->ncf--;
    }
}


/*
** Free thread stack and the call frames.
*/
static void freestack(toku_State *T) {
    if (T->stack.p != NULL) { /* stack fully built? */
        T->cf = &T->basecf; /* free the entire 'cf' list */
        freeCF(T);
        toku_assert(T->ncf == 0 && T->basecf.next == NULL);
        tokuM_freearray(T, T->stack.p, cast_sizet(stacksize(T) + EXTRA_STACK));
    }
}


static void freestate(toku_State *T) {
    GState *gs = G(T);
    toku_assert(T == G(T)->mainthread);
    if (!statefullybuilt(gs)) /* closing partially built state? */
        tokuG_freeallobjects(T); /* collect only objects */
    else { /* otherwise closing a fully built state */
        resetCF(T); /* undwind call stack */
        tokuPR_close(T, 1, TOKU_STATUS_OK); /* close all upvalues */
        T->sp.p = T->stack.p + 1; /* empty the stack to run finalizers */
        tokuG_freeallobjects(T); /* collect all objects */
        tokui_userstateclose(T);
    }
    tokuM_freearray(T, G(T)->strtab.hash, cast_sizet(G(T)->strtab.size));
    freestack(T);
    toku_assert(gettotalbytes(gs) == sizeof(XSG));
    gs->falloc(fromstate(T), sizeof(XSG), 0, gs->ud_alloc); /* free state */
}


/*
** Initialize garbage collection parameters.
*/
static void initGCparams(GState *gs) {
    setgcparam(gs->gcparams[TOKU_GCP_PAUSE], TOKUI_GCP_PAUSE);
    setgcparam(gs->gcparams[TOKU_GCP_STEPMUL], TOKUI_GCP_STEPMUL);
    gs->gcparams[TOKU_GCP_STEPSIZE] = TOKUI_GCP_STEPSIZE;
}


/*
** Allocate new thread and global state with 'falloc' and
** userdata 'ud', from here on 'falloc' will be the allocator.
** The returned thread state is mainthread.
** In case of errors NULL is returned.
*/
TOKU_API toku_State *toku_newstate(toku_Alloc falloc, void *ud, unsigned seed) {
    GState *gs;
    toku_State *T;
    XSG *xsg = falloc(NULL, 0, sizeof(XSG), ud);
    if (t_unlikely(xsg == NULL)) return NULL;
    gs = &xsg->gs;
    T = &xsg->xs.t;
    T->tt_ = TOKU_VTHREAD;
    gs->whitebit = bitmask(WHITEBIT0);
    T->mark = tokuG_white(gs);
    preinit_thread(T, gs);
    T->next = NULL;
    incnnyc(T);
    gs->objects = obj2gco(T);
    gs->totalbytes = sizeof(XSG);
    gs->seed = seed; /* initial seed for hashing */
    gs->strtab.hash = NULL;
    gs->strtab.nuse = gs->strtab.size = 0;
    gs->gcdebt = 0;
    gs->gcstate = GCSpause;
    gs->gcstopem = 0;
    initGCparams(gs);
    gs->gcstop = GCSTP; /* no GC while creating state */
    gs->gcemergency = 0;
    gs->gccheck = 0;
    gs->sweeppos = NULL;
    gs->fixed = gs->fin = gs->tobefin = NULL;
    gs->graylist = gs->grayagain = NULL;
    gs->weak = NULL;
    setnilval(&gs->c_list);
    setnilval(&gs->c_table);
    gs->falloc = falloc;
    gs->ud_alloc = ud;
    gs->fpanic = NULL; /* no panic handler by default */
    setival(&gs->nil, 0); /* signals that state is not yet fully initialized */
    gs->mainthread = T;
    gs->twups = NULL;
    gs->fwarn = NULL; gs->ud_warn = NULL;
    toku_assert(gs->totalbytes == sizeof(XSG) && gs->gcdebt == 0);
    if (tokuPR_rawcall(T, f_newstate, NULL) != TOKU_STATUS_OK) {
        freestate(T);
        T = NULL;
    }
    return T;
}


/* free state (global state + mainthread) */
TOKU_API void toku_close(toku_State *T) {
    toku_lock(T);
    toku_State *mt = G(T)->mainthread;
    freestate(mt);
}


/*
** Create new thread state.
*/
TOKU_API toku_State *toku_newthread(toku_State *T) {
    GState *gs = G(T);
    GCObject *o;
    toku_State *T1;
    toku_lock(T);
    tokuG_checkGC(T);
    o = tokuG_newoff(T, sizeof(XS), TOKU_VTHREAD, offsetof(XS, t));
    T1 = gco2th(o);
    setthval2s(T, T->sp.p, T1);
    api_inctop(T);
    preinit_thread(T1, gs);
    T1->hookmask = T->hookmask;
    T1->basehookcount = T->basehookcount;
    T1->hook = T->hook;
    resethookcount(T1);
    memcpy(toku_getextraspace(T1), toku_getextraspace(gs->mainthread),
           TOKU_EXTRASPACE);
    tokui_userstatethread(T, T1);
    stackinit(T1, T);
    toku_unlock(T);
    return T1;
}


int tokuT_resetthread(toku_State *T, int status) {
    CallFrame *cf = T->cf = &T->basecf;
    setnilval(s2v(T->stack.p)); /* 'basecf' func */
    cf->func.p = T->stack.p;
    cf->status = CFST_CCALL;
    T->status = TOKU_STATUS_OK; /* so we can run '__close' */
    status = tokuPR_close(T, 1, status);
    if (status != TOKU_STATUS_OK) /* error? */
        tokuPR_seterrorobj(T, status, T->stack.p + 1);
    else
        T->sp.p = T->stack.p + 1;
    cf->top.p = T->sp.p + TOKU_MINSTACK;
    tokuT_reallocstack(T, cf->top.p - T->sp.p, 0);
    return status;
}


/*
** Reset thread state 'T' by unwinding `CallFrame` list,
** closing all upvalues (and to-be-closed variables) and
** reseting the stack.
** In case of errors, error object is placed on top of the
** stack and the function returns relevant status code.
** If no errors occured `TOKU_STATUS_OK` status is returned.
*/
TOKU_API int toku_resetthread(toku_State *T) {
    int status;
    toku_lock(T);
    status = tokuT_resetthread(T, T->status);
    toku_unlock(T);
    return status;
}


/* some space for error handling */
#define ERRORSTACKSIZE      (TOKUI_MAXSTACK + 200)


CallFrame *tokuT_newcf(toku_State *T) {
    CallFrame *cf;
    toku_assert(T->cf->next == NULL);
    cf = tokuM_new(T, CallFrame);
    toku_assert(T->cf->next == NULL);
    T->cf->next = cf;
    cf->prev = T->cf;
    cf->next = NULL;
    cf->t.trap = 0;
    T->ncf++;
    return cf;
}


/* convert stack pointers into relative stack offsets */
static void relstack(toku_State *T) {
    T->sp.offset = savestack(T, T->sp.p);
    T->tbclist.offset = savestack(T, T->tbclist.p);
    for (UpVal *uv = T->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.offset = savestack(T, uvlevel(uv));
    for (CallFrame *cf = T->cf; cf != NULL; cf = cf->prev) {
        cf->func.offset = savestack(T, cf->func.p);
        cf->top.offset = savestack(T, cf->top.p);
    }
}


/* convert relative stack offsets into stack pointers */
static void correctstack(toku_State *T) {
    T->sp.p = restorestack(T, T->sp.offset);
    T->tbclist.p = restorestack(T, T->tbclist.offset);
    for (UpVal *uv = T->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.p = s2v(restorestack(T, uv->v.offset));
    for (CallFrame *cf = T->cf; cf != NULL; cf = cf->prev) {
        cf->func.p = restorestack(T, cf->func.offset);
        cf->top.p = restorestack(T, cf->top.offset);
        if (isTokudae(cf))
            cf->t.trap = 1; /* signal to update 'trap' in 'tokuV_execute' */
    }
}


/* reallocate stack to new size */
int tokuT_reallocstack(toku_State *T, int newsize, int raiseerr) {
    int osz = stacksize(T);
    int old_stopem = G(T)->gcstopem;
    SPtr newstack;
    toku_assert(newsize <= TOKUI_MAXSTACK || newsize == ERRORSTACKSIZE);
    relstack(T); /* change pointers to offsets */
    G(T)->gcstopem = 1; /* no emergency collection when reallocating stack */
    newstack = tokuM_reallocarray(T, T->stack.p, osz + EXTRA_STACK,
                                               newsize + EXTRA_STACK, SValue);
    G(T)->gcstopem = old_stopem;
    if (t_unlikely(newstack == NULL)) {
        correctstack(T); /* change offsets back to pointers */
        if (raiseerr)
            tokuM_error(T);
        else return 0;
    }
    T->stack.p = newstack;
    correctstack(T); /* change offsets back to pointers */
    T->stackend.p = T->stack.p + newsize;
    for (int i = osz + EXTRA_STACK; i < newsize + EXTRA_STACK; i++)
        setnilval(s2v(newstack + i)); /* erase new segment */
    return 1;
}


/* 
** Grow stack to accommodate 'n' values. When 'raiseerr' is true,
** raises any error; otherwise, return 0 in case of errors.
*/
int tokuT_growstack(toku_State *T, int n, int raiseerr) {
    int size = stacksize(T);
    if (t_unlikely(size > TOKUI_MAXSTACK)) { /* overflowed already ? */
        /* if stack is larger than maximum, thread is already using the
        ** extra space reserved for errors, that is, thread is handling
        ** a stack error; cannot grow further than that. */
        toku_assert(size == ERRORSTACKSIZE);
        if (raiseerr)
            tokuPR_throw(T, TOKU_STATUS_EERROR);
        return 0;
    } else if (n < TOKUI_MAXSTACK) { /* avoid arithmetic overflow */
        int nsize = size * 2;
        int needed = cast_int((T)->sp.p - (T)->stack.p) + n;
        if (nsize > TOKUI_MAXSTACK)
            nsize = TOKUI_MAXSTACK; /* clamp it */
        if (nsize < needed) /* size still too small? */
            nsize = needed; /* grow to needed size */
        if (t_likely(nsize <= TOKUI_MAXSTACK)) /* new size is ok? */
            return tokuT_reallocstack(T, nsize, raiseerr);
    }
    /* else stack overflow */
    /* add extra size to be able to handle the error message */
    tokuT_reallocstack(T, ERRORSTACKSIZE, raiseerr);
    if (raiseerr)
        tokuD_runerror(T, "stack overflow");
    return 0;
}


static int stackinuse(toku_State *T) {
    int res;
    SPtr lim = T->sp.p;
    for (CallFrame *cf = T->cf; cf != NULL; cf = cf->prev) {
        if (lim < cf->top.p)
            lim = cf->top.p;
    }
    toku_assert(lim <= T->stackend.p + EXTRA_STACK);
    res = cast_int(lim - T->stack.p) + 1; /* part of stack in use */
    if (res < TOKU_MINSTACK)
        res = TOKU_MINSTACK; /* ensure a minimum size */
    return res;
}


/*
** Shrink stack if the current stack size is more than 3 times the
** current use. This also rolls back the stack to its original maximum
** size 'TOKUI_MAXSTACK' in case the stack was previously handling stack
** overflow.
*/
void tokuT_shrinkstack(toku_State *T) {
    int inuse = stackinuse(T);
    int limit = (inuse >= TOKUI_MAXSTACK / 3 ? TOKUI_MAXSTACK : inuse * 3);
    if (inuse <= TOKUI_MAXSTACK && stacksize(T) > limit) {
        int nsize = (inuse < (TOKUI_MAXSTACK / 2) ? (inuse * 2) : TOKUI_MAXSTACK);
        tokuT_reallocstack(T, nsize, 0); /* this can fail */
    }
}


/* increment stack pointer */
void tokuT_incsp(toku_State *T) {
    tokuPR_checkstack(T, 1);
    T->sp.p++;
}


/*
** Called when 'getCcalls' is >= TOKUI_MAXCCALLS.
** If equal to TOKUI_MAXCCALLS then overflow error is invoked.
** Otherwise it is ignored in order to resolve the current
** overflow error, unless the number of calls is significantly
** higher than TOKUI_MAXCCALLS.
*/
void tokuT_checkCstack(toku_State *T) {
    if (getCcalls(T) == TOKUI_MAXCCALLS) /* not handling error ? */
        tokuD_runerror(T, "T stack overflow");
    else if (getCcalls(T) >= (TOKUI_MAXCCALLS / 10 * 11))
        tokuPR_throw(T, TOKU_STATUS_EERROR);
}


/* Increment number of T calls and check for overflow. */
void tokuT_incCstack(toku_State *T) {
    T->nCcalls++;
    if (getCcalls(T) >= TOKUI_MAXCCALLS)
        tokuT_checkCstack(T);
}


void tokuT_warning(toku_State *T, const char *msg, int cont) {
    toku_WarnFunction fwarn = G(T)->fwarn;
    if (fwarn)
        fwarn(G(T)->ud_warn, msg, cont);
}


/* generate a warning from an error message */
void tokuT_warnerror(toku_State *T, const char *where) {
    TValue *errobj = s2v(T->sp.p - 1);
    const char *msg = (ttisstring(errobj))
                      ? getstr(strval(errobj))
                      : "error object is not a string";
    tokuT_warning(T, "error in ", 1);
    tokuT_warning(T, where, 1);
    tokuT_warning(T, " (", 1);
    tokuT_warning(T, msg, 1);
    tokuT_warning(T, ")", 0);
}


void tokuT_free(toku_State *T, toku_State *T1) {
    XS *xs = fromstate(T1);
    tokuF_closeupval(T1, T1->stack.p);  /* close all upvalues */
    toku_assert(T1->openupval == NULL);
    tokui_userstatefree(T, T1);
    freestack(T1);
    tokuM_free(T, xs);
}
