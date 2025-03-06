/*
** cstate.c
** Global and Thread state
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "ctable.h"
#include "carray.h"
#include "cstate.h"
#include "capi.h"
#include "cdebug.h"
#include "cfunction.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cobject.h"
#include "cprotected.h"
#include "cscript.h"
#include "cstring.h"


#if !defined(csi_makeseed)
#include <time.h>
#include <string.h>

#define buffadd(b,p,e) \
    { size_t t = cast_sizet(e); \
      memcpy((b) + (p), &t, sizeof(t)); (p) += sizeof(t); }

static uint csi_makeseed(cs_State *C) {
    char str[3 * sizeof(size_t)];
    uint seed = time(NULL); /* seed with current time */
    int n = 0;
    buffadd(str, n, C); /* heap variable */
    buffadd(str, n, &seed); /* local variable */
    buffadd(str, n, &cs_newstate); /* public function */
    cs_assert(n == sizeof(str));
    return csS_hash(str, n, seed);
}
#endif


/*
** Preinitialize all thread fields to avoid collector
** errors.
*/
static void preinit_thread(cs_State *C, GState *gs) {
    C->ncf = 0;
    C->status = CS_OK;
    C->errfunc = 0;
    C->nCcalls = 0;
    C->gclist = NULL;
    C->thwouv = C; /* if ('C->thwouv' == 'C') then no upvalues */
    G(C) = gs;
    C->errjmp = NULL;
    C->stack.p = C->sp.p = C->stackend.p = NULL;
    C->cf = NULL;
    C->openupval = NULL;
    C->tbclist.p = NULL;
}


/*
** Initialize stack and base call frame for 'C'.
** 'C' is a main thread state ('C1' == 'C' only when creating new
** state).
*/
static void init_stack(cs_State *C1, cs_State *C) {
    CallFrame *cf;
    cs_assert(!statefullybuilt(G(C1)) == (C1 == C));
    C1->stack.p = csM_newarray(C, INIT_STACKSIZE + EXTRA_STACK, SValue);
    C1->tbclist.p = C1->stack.p;
    for (int i = 0; i < INIT_STACKSIZE + EXTRA_STACK; i++)
        setnilval(s2v(C1->stack.p + i));
    C1->sp.p = C1->stack.p;
    C1->stackend.p = C1->stack.p + INIT_STACKSIZE;
    cf = &C1->basecf;
    cf->next = cf->prev = NULL;
    cf->func.p = C1->sp.p;
    cf->pc = NULL;
    cf->nvarargs = 0;
    cf->nresults = 0;
    cf->status = CFST_CCALL;
    setnilval(s2v(C->sp.p)); /* 'cf' entry function */
    C->sp.p++;
    cf->top.p = C->stack.p + CS_MINSTACK;
    C->cf = cf;
}


static void init_registry(cs_State *C, GState *gs) {
    Array *registry = csA_new(C); 
    setarrval(C, &gs->c_registry, registry);
    csA_ensureindex(C, registry, CS_RINDEX_LAST);
    /* registry[CS_RINDEX_MAINTHREAD] = C (mainthread) */
    setthval(C, &registry->b[CS_RINDEX_MAINTHREAD], C);
    /* registry[CS_RINDEX_GLOBALS] = new table (for global variables) */
    settval(C, &registry->b[CS_RINDEX_GLOBALS], csH_new(C));
}


/*
** Initializes parts of state that may cause memory allocation
** errors.
*/
static void f_newstate(cs_State *C, void *ud) {
    GState *gs = G(C);
    UNUSED(ud);
    init_stack(C, C);
    init_registry(C, gs);
    csS_init(C);
    csMM_init(C);
    csY_init(C);
    gs->gcstop = 0;
    setnilval(&gs->nil); /* signal that state is fully built */
    csi_userstateopen(C);
}


/* free all 'CallFrame' structures NOT in use by thread */
static void free_frames(cs_State *C) {
    CallFrame *cf = C->cf;
    CallFrame *next = cf->next;
    cf->next = NULL;
    while ((cf = next) != NULL) {
        next = cf->next;
        csM_free(C, cf);
        C->ncf--;
    }
}


/* free thread stack and call frames */
static void free_stack(cs_State *C) {
    if (C->stack.p != NULL) { /* stack fully built? */
        C->cf = &C->basecf; /* free all of the call frames */
        free_frames(C);
        cs_assert(C->ncf == 0 && C->basecf.next == NULL);
        csM_freearray(C, C->stack.p, stacksize(C) + EXTRA_STACK);
    }
}


/* free global state virtual method tables */
static void free_vmt(cs_State *C) {
    GState *gs = G(C);
    cs_assert(C == gs->mainthread);
    for (int i = 0; i < CS_NUM_TYPES; i++)
        if (gs->vmt[i])
            csM_freearray(C, gs->vmt[i], CS_MM_N);
}


static void freestate(cs_State *C) {
    GState *gs = G(C);
    cs_assert(C == G(C)->mainthread);
    if (!statefullybuilt(gs)) { /* partially built state? */
        csG_freeallobjects(C);
    } else { /* freeing fully built state */
        C->cf = &C->basecf; /* undwind call frames */
        csPR_close(C, 1, CS_OK);
        csG_freeallobjects(C);
        csi_userstateclose(C);
    }
    csM_freearray(C, gs->strtab.hash, gs->strtab.size);
    free_stack(C);
    free_vmt(C);
    cs_assert(gettotalbytes(gs) == sizeof(XSG));
    gs->falloc(fromstate(C), sizeof(XSG), 0, gs->ud_alloc); /* free state */
}


/*
** Allocate new thread and global state with 'falloc' and
** userdata 'ud', from here on 'falloc' will be the allocator.
** The returned thread state is mainthread.
** In case of errors NULL is returned.
*/
CS_API cs_State *cs_newstate(cs_Alloc falloc, void *ud) {
    GState *gs;
    cs_State *C;
    XSG *xsg = falloc(NULL, 0, sizeof(XSG), ud);
    if (c_unlikely(xsg == NULL)) return NULL;
    gs = &xsg->gs;
    C = &xsg->xs.C;
    C->tt_ = CS_VTHREAD;
    gs->whitebit = bitmask(WHITEBIT0);
    C->mark = csG_white(gs);
    preinit_thread(C, gs);
    C->next = NULL;
    incnnyc(C);
    gs->objects = obj2gco(C);
    gs->totalbytes = sizeof(XSG);
    gs->seed = csi_makeseed(C); /* initial seed for hashing */
    gs->strtab.hash = NULL;
    gs->strtab.nuse = gs->strtab.size = 0;
    gs->gcdebt = 0;
    gs->gcstate = GCSpause;
    gs->gcstopem = 0;
    gs->gcstop = GCSTP; /* no GC while creating state */
    gs->gcemergency = 0;
    setgcparam(gs->gcpause, CSI_GCPAUSE);
    setgcparam(gs->gcstepmul, CSI_GCSTEPMUL);
    gs->gcstepsize = CSI_GCSTEPSIZE;
    gs->sweeppos = NULL;
    gs->fixed = gs->fin = gs->tobefin = NULL;
    gs->graylist = gs->grayagain = NULL;
    gs->weak = NULL;
    setnilval(&gs->c_registry);
    gs->falloc = falloc;
    gs->ud_alloc = ud;
    gs->fpanic = NULL; /* no panic handler by default */
    setival(&gs->nil, 0); /* signals that state is not yet fully initialized */
    gs->mainthread = C;
    gs->thwouv = NULL;
    gs->fwarn = NULL; gs->ud_warn = NULL;
    for (int i = 0; i < CS_NUM_TYPES; i++) gs->vmt[i] = NULL;
    cs_assert(gs->totalbytes == sizeof(XSG) && gs->gcdebt == 0);
    if (csPR_rawcall(C, f_newstate, NULL) != CS_OK) {
        freestate(C);
        C = NULL;
    }
    return C;
}


/* free state (global state + mainthread) */
CS_API void cs_close(cs_State *C) {
    cs_lock(C);
    cs_State *mt = G(C)->mainthread;
    freestate(mt);
    /* user shall handle unlocking he defined himself (if any) */
}


/*
** Create new thread state.
*/
CS_API cs_State *cs_newthread(cs_State *C) {
    GState *gs = G(C);
    cs_State *C1;
    GCObject *o;
    cs_lock(C);
    o = csG_newoff(C, sizeof(XS), CS_VTHREAD, offsetof(XS, C));
    C1 = gco2th(o);
    setthval2s(C, C->sp.p, C1);
    api_inctop(C);
    preinit_thread(C1, gs);
    init_stack(C1, C);
    memcpy(cs_getextraspace(C1), cs_getextraspace(gs->mainthread),
           CS_EXTRASPACE);
    csi_userstate(C, C1);
    cs_unlock(C);
    return C1;
}


int csT_resetthread(cs_State *C, int status) {
    CallFrame *cf = C->cf = &C->basecf;
    setnilval(s2v(C->stack.p)); /* 'basecf' func */
    cf->func.p = C->stack.p;
    cf->status = CFST_CCALL;
    C->status = CS_OK; /* so we can run '__close' */
    status = csPR_close(C, 1, status);
    if (status != CS_OK) /* error? */
        csT_seterrorobj(C, status, C->stack.p + 1);
    else
        C->sp.p = C->stack.p + 1;
    cf->top.p = C->sp.p + CS_MINSTACK;
    csT_reallocstack(C, cf->top.p - C->sp.p, 0);
    return status;
}


/*
** Reset thread state 'C' by unwinding `CallFrame` list,
** closing all upvalues (and to-be-closed variables) and
** reseting the stack.
** In case of errors, error object is placed on top of the
** stack and the function returns relevant status code.
** If no errors occured `CS_OK` status is returned.
*/
CS_API int cs_resetthread(cs_State *C) {
    int status;
    cs_lock(C);
    status = csT_resetthread(C, C->status);
    cs_unlock(C);
    return status;
}


void csT_seterrorobj(cs_State *C, int errcode, SPtr oldtop) {
    switch (errcode) {
        case CS_ERRMEM: { /* memory error? */
            setstrval2s(C, oldtop, G(C)->memerror);
            break;
        }
        case CS_ERRERROR: { /* error while handling error? */
            setstrval2s(C, oldtop, csS_newlit(C, "error in error handling"));
            break;
        }
        case CS_OK: { /* closing upvalue? */
            setnilval(s2v(oldtop)); /* no error message */
            break;
        }
        default: { /* real error */
            cs_assert(errcode > CS_OK);
            setobjs2s(C, oldtop, C->sp.p - 1); /* error msg on current top */
            break;
        }
    }
    C->sp.p = oldtop + 1;
}


/*
** Stack size to grow the stack to when stack overflow occurs
** for error handling.
*/
#define OVERFLOWSTACKSIZE       (CSI_MAXSTACK + 200)


CallFrame *csT_newcf(cs_State *C) {
    CallFrame *cf;
    cs_assert(C->cf->next == NULL);
    cf = csM_new(C, CallFrame);
    cs_assert(C->cf->next == NULL);
    C->cf->next = cf;
    cf->prev = C->cf;
    cf->next = NULL;
    C->ncf++;
    return cf;
}


/* convert stack pointers into relative stack offsets */
static void sptr2rel(cs_State *C) {
    C->sp.offset = savestack(C, C->sp.p);
    for (CallFrame *cf = C->cf; cf != NULL; cf = cf->prev) {
        cf->func.offset = savestack(C, cf->func.p);
        cf->top.offset = savestack(C, cf->top.p);
    }
    for (UpVal *uv = C->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.offset = savestack(C, uv->v.p);
    C->tbclist.offset = savestack(C, C->tbclist.p);
}


/* convert relative stack offsets into stack pointers */
static void rel2sptr(cs_State *C) {
    C->sp.p = restorestack(C, C->sp.offset);
    for (CallFrame *cf = C->cf; cf != NULL; cf = cf->prev) {
        cf->func.p = restorestack(C, cf->func.offset);
        cf->top.p = restorestack(C, cf->top.offset);
    }
    for (UpVal *uv = C->openupval; uv != NULL; uv = uv->u.open.next)
        uv->v.p = s2v(restorestack(C, uv->v.offset));
    C->tbclist.p = restorestack(C, C->tbclist.offset);
}


/* reallocate stack to new size */
int csT_reallocstack(cs_State *C, int size, int raiseerr) {
    SPtr newstack;
    GState *gs = G(C);
    int old_stopem = gs->gcstopem;
    int osz = stacksize(C);
    cs_assert(size <= CSI_MAXSTACK || size == OVERFLOWSTACKSIZE);
    sptr2rel(C);
    gs->gcstopem = 1; /* no emergency collection when reallocating stack */
    newstack = csM_reallocarray(C, C->stack.p, osz + EXTRA_STACK,
                                size + EXTRA_STACK, SValue);
    gs->gcstopem = old_stopem;
    if (c_unlikely(newstack == NULL)) {
        rel2sptr(C);
        if (raiseerr)
            csPR_throw(C, CS_ERRMEM);
        return 0;
    }
    rel2sptr(C);
    C->stack.p = newstack;
    C->stackend.p = newstack + size;
    for (int i = osz + EXTRA_STACK; i < size + EXTRA_STACK; i++)
        setnilval(s2v(newstack + i));
    return 1;
}


/* grow stack to accommodate 'n' values */
int csT_growstack(cs_State *C, int n, int raiseerr) {
    int size = stacksize(C);
    if (c_unlikely(size > CSI_MAXSTACK)) { /* overflowed already ? */
        cs_assert(size == OVERFLOWSTACKSIZE);
        if (raiseerr)
            csPR_throw(C, CS_ERRERROR);
        return 0;
    }
    if (c_unlikely(n > CSI_MAXSTACK)) {
        int nsize = size * 2;
        int needed = cast_int((C)->sp.p - (C)->stack.p) + n;
        if (nsize > CSI_MAXSTACK)
            nsize = CSI_MAXSTACK;
        if (nsize < needed)
            nsize = needed;
        if (c_likely(nsize <= CSI_MAXSTACK))
            return csT_reallocstack(C, nsize, raiseerr);
    }
    csT_reallocstack(C, OVERFLOWSTACKSIZE, raiseerr);
    if (raiseerr)
        csD_runerror(C, "stack overflow");
    return 0;
}


static int stackinuse(cs_State *C) {
    SPtr maxtop = C->cf->top.p;
    for (CallFrame *cf = C->cf->prev; cf != NULL; cf = cf->prev) {
        if (maxtop < cf->top.p)
            maxtop = cf->top.p;
    }
    cs_assert(maxtop <= C->stackend.p + EXTRA_STACK);
    int n = savestack(C, maxtop);
    if (n < CS_MINSTACK)
        n = CS_MINSTACK;
    return n;
}


/*
** Shrink stack if the current stack size is more
** than 3 times the current use.
** This also rolls back the stack to its original maximum
** size 'CSI_MAXSTACK' in case the stack was previously
** handling stack overflow.
*/
void csT_shrinkstack(cs_State *C) {
    int inuse = stackinuse(C);
    int limit = (inuse >= CSI_MAXSTACK / 3 ? CSI_MAXSTACK : inuse * 3);
    if (inuse <= CSI_MAXSTACK && stacksize(C) > limit) {
        int nsize = (inuse < (CSI_MAXSTACK / 2) ? (inuse * 2) : CSI_MAXSTACK);
        csT_reallocstack(C, nsize, 0); /* this can fail */
    }
}


/* increment stack pointer */
void csT_incsp(cs_State *C) {
    csT_checkstack(C, 1);
    C->sp.p++;
}


/*
** Called when 'getCcalls' is >= CSI_MAXCCALLS.
** If equal to CSI_MAXCCALLS then overflow error is invoked.
** Otherwise it is ignored in order to resolve the current
** overflow error, unless the number of calls is significantly
** higher than CSI_MAXCCALLS.
*/
void csT_checkCstack(cs_State *C) {
    if (getCcalls(C) == CSI_MAXCCALLS) /* not handling error ? */
        csD_runerror(C, "C stack overflow");
    else if (getCcalls(C) >= (CSI_MAXCCALLS / 10 * 11))
        csPR_throw(C, CS_ERRERROR);
}


/* Increment number of C calls and check for overflow. */
void csT_incCstack(cs_State *C) {
    C->nCcalls++;
    if (getCcalls(C) >= CSI_MAXCCALLS)
        csT_checkCstack(C);
}


void csT_warning(cs_State *C, const char *msg, int cont) {
    cs_WarnFunction fwarn = G(C)->fwarn;
    if (fwarn)
        fwarn(G(C)->ud_warn, msg, cont);
}


/* generate a warning from an error message */
void csT_warnerror(cs_State *C, const char *where) {
    TValue *errobj = s2v(C->sp.p - 1);
    const char *msg = (ttisstring(errobj))
                      ? getstr(strval(errobj))
                      : "error object is not a string";
    csT_warning(C, "error in ", 1);
    csT_warning(C, where, 1);
    csT_warning(C, " (", 1);
    csT_warning(C, msg, 1);
    csT_warning(C, ")", 0);
}


void csT_free(cs_State *C, cs_State *thread) {
    XS *xs = fromstate(thread);
    csF_closeupval(thread, thread->stack.p);  /* close all upvalues */
    cs_assert(thread->openupval == NULL);
    csi_userstatefree(C, thread);
    free_stack(thread);
    csM_free(C, xs);
}
