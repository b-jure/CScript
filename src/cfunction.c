/*
** cfunction.c
** Functions for CScript functions and closures
** See Copyright Notice in cscript.h
*/

#include "cfunction.h"
#include "cdebug.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cobject.h"
#include "cstate.h"
#include "cvm.h"


Function *crF_newfunction(cr_State *ts) {
    Function *fn = crG_new(ts, sizeof(Function), CR_VFUNCTION, Function);
    fn->isvararg = 0;
    fn->gclist = NULL;
    fn->source = NULL;
    { fn->funcs = NULL; fn->sizefn = 0; } /* functions */
    { fn->k = NULL; fn->sizek = 0; } /* constants */
    { fn->private = NULL; fn->sizeprivate = 0; } /* privates */
    { fn->code = NULL; fn->sizecode = 0; } /* code */
    { fn->linfo = NULL; fn->sizelinfo = 0; } /* line information */
    { fn->locals = NULL; fn->sizelocals = 0; } /* locals */
    { fn->upvals = NULL; fn->sizeupvals = 0; } /* upvalues */
    fn->maxstack = 0;
    fn->arity = 0;
    fn->defline = 0;
    fn->deflastline = 0;
    return fn;
}


CrClosure *crF_newCrClosure(cr_State *ts, int nup) {
    CrClosure *crcl = crG_new(ts, sizeofcrcl(nup), CR_VCRCL, CrClosure);
    crcl->nupvalues = nup;
    crcl->fn = NULL;
    for (int i = 0; i < nup; i++)
        crcl->upvals[i] = NULL;
    return crcl;
}


CClosure *crF_newCClosure(cr_State *ts, int nupvalues) {
    CClosure *ccl = crG_new(ts, nupvalues * sizeof(TValue), CR_VCCL, CClosure);
    ccl->nupvalues = nupvalues;
    return ccl;
}


/*
** Adjusts function varargs by moving the named parameters
** and the function in front of the varargs.
** Additionally adjust new top for 'cf' and invalidates
** old named parameters (after they get moved).
*/
void crF_adjustvarargs(cr_State *ts, int arity, CallFrame *cf,
                       const Function *fn) {
    int i;
    int actual = cast_int(ts->sp.p - cf->func.p) - 1;
    int extra = actual - arity; /* number of varargs */
    cf->nvarargs = extra;
    crT_checkstack(ts, fn->maxstack + 1);
    setobjs2s(ts, ts->sp.p++, cf->func.p); /* move function */
    for (i = 0; i < arity; i++) {
        setobjs2s(ts, ts->sp.p++, cf->func.p + i); /* move param */
        setnilval(s2v(cf->func.p + i)); /* invalidate old */
    }
    cf->func.p += actual + 1;
    cf->top.p += actual + 1;
    cr_assert(ts->sp.p <= cf->top.p && cf->top.p <= ts->stackend.p);
}


/* Get 'wanted' varargs starting at the current stack pointer. */
void crF_getvarargs(cr_State *ts, CallFrame *cf, int wanted) {
    int have = cf->nvarargs;
    if (wanted < 0) { /* CR_MULRET ? */
        wanted = have;
        checkstackGC(ts, wanted); /* check stack, maybe 'wanted > have' */
    }
    for (int i = 0; wanted-- && i < have; i++)
        setobjs2s(ts, ts->sp.p++, cf->func.p - have + i);
    while (wanted--)
        setnilval(s2v(ts->sp.p++));
}


/* Create and initialize all the upvalues in 'cl'. */
void crF_initupvals(cr_State *ts, CrClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = crG_new(ts, sizeof(UpVal), CR_VUVALUE, UpVal);
        uv->v.p = &uv->u.value; /* close it */
        setnilval(uv->v.p);
        cl->upvals[i] = uv;
        crG_objbarrier(ts, cl, uv);
    }
}


/*
** Create a new upvalue and link it into 'openupval' list
** right after 'prev'.
*/
static UpVal *newupval(cr_State *ts, SPtr val, UpVal **prev) {
    UpVal *uv = crG_new(ts, sizeof(*uv), CR_VUVALUE, UpVal);
    UpVal *previous = *prev;
    uv->v.p = s2v(val);
    uv->u.open.next = previous;
    if (previous) /* have previous upval ? */
        previous->u.open.prev = &uv->u.open.next;
    *prev = uv; /* adjust list head or 'previous.u.open.next' */
    cr_assert(prev == &ts->openupval);
    if (!isinthwouv(ts)) {
        GState *gs = G_(ts);
        ts->thwouv = gs->thwouv;
        gs->thwouv = ts;
    }
    return uv;
}


/*
** Find and return already existing upvalue or create
** and return a new one.
*/
UpVal *crF_findupval(cr_State *ts, SPtr sval) {
    UpVal *upval;
    SPtr sp;
    cr_assert(isinthwouv(ts) || ts->openupval == NULL);
    UpVal **pp = &ts->openupval; /* good ol' pp */
    while ((upval = *pp) != NULL && (sp = uvlevel(upval)) > sval) {
        cr_assert(!isdead(&G_(ts)->gc, upval));
        if (sp == sval)
            return upval;
        pp = &upval->u.open.next;
    }
    return newupval(ts, sval, pp);
}


/*
** Find local variable name that must be alive 'endpc > pc'
** and must be at index 'lnum'.
** This function takes into consideration that 'fn' might be
** missing debug information (stripped).
** Note: current version (1.0.0) always contains debug information.
*/
const char *crF_getlocalname(const Function *fn, int lnum, int pc) {
    cr_assert(lnum > 0);
    for (int i = 0; i < fn->sizelocals && fn->locals->startpc <= pc; i++)
        if ((lnum -= (pc < fn->locals[i].endpc)) == 0)
            return getstrbytes(fn->locals[i].name);
    return NULL;
}


/* 
** Check if object at stack 'level' has a '__close' method,
** raise error if not.
*/
static void checkclosem(cr_State *ts, SPtr level) {
    const TValue *fn = crMM_get(ts, s2v(level), CR_MM_CLOSE);
    if (cr_unlikely(ttisnil(fn))) { /* missing '__close' method ? */
        int vidx = level - ts->cf->func.p;
        const char *name = crD_findlocal(ts, ts->cf, vidx, NULL);
        if (name == NULL) name = "?";
        crD_runerror(ts, "variable %s got a non-closeable value", name);
    }
}


/* 
** Maximum value for 'delta', dependant on the data type
** of 'tbc.delta'.
*/
#define MAXDELTA \
    ((256UL << ((sizeof(ts->stack.p->tbc.delta) - 1) * 8)) - 1)


/* insert variable into the list of to-be-closed variables */
void crF_newtbcvar(cr_State *ts, SPtr level) {
    if (cri_isfalse(s2v(level)))
        return;
    checkclosem(ts, level);
    while (cast_uint(level - ts->tbclist.p) > MAXDELTA) {
        ts->tbclist.p += MAXDELTA;
        ts->tbclist.p->tbc.delta = 0;
    }
    level->tbc.delta = level - ts->tbclist.p;
    ts->tbclist.p = level;
}


/* unlinks upvalue from the list */
static void unlinkupval(UpVal *upval) {
    *upval->u.open.prev = upval->u.open.next;
    if (upval->u.open.next)
        upval->u.open.next->u.open.prev = upval->u.open.prev;
}


/* close any open upvalues up to the 'level' */
void crF_closeupval(cr_State *ts, SPtr level) {
    UpVal *uv = ts->openupval;
    while (uv != NULL && uvlevel(uv) <= level) {
        TValue *slot = &uv->u.value;
        unlinkupval(uv);
        setobj(ts, slot, uv->v.p);
        uv->v.p = slot;
        if (!iswhite(uv)) { /* not white but maybe gray ? */
            notw2black(uv); /* closed upvalue can't be gray */
            crG_barrier(ts, uv, slot); /* noop, 'slot' (uv) is black */
        }
    }
}


/* remove first element from 'tbclist' */
static void poptbclist(cr_State *ts) {
    SPtr tbc = ts->tbclist.p;
    cr_assert(tbc->tbc.delta > 0);
    tbc -= tbc->tbc.delta;
    while (tbc > ts->stack.p && tbc->tbc.delta == 0)
        tbc -= tbc->tbc.delta;
    ts->tbclist.p = tbc;
}


/* 
** Call '__close' method on 'obj' with error object 'errobj'.
** This function assumes 'EXTRA_STACK'.
*/
static void callclosemethod(cr_State *ts, TValue *obj, TValue *errobj) {
    SPtr top = ts->sp.p;
    const TValue *method = crMM_get(ts, obj, CR_MM_CLOSE);
    cr_assert(!ttisnil(method));
    setobj2s(ts, top + 1, method);
    setobj2s(ts, top + 2, obj);
    setobj2s(ts, top + 3, errobj);
    ts->sp.p = top + 3;
    crV_call(ts, top, 0);
}


/*
** Prepare and call '__close' method.
** If status is CLOSEKTOP, the call to the closing method will be pushed
** at the top of the stack. Otherwise, values can be pushed right after
** the 'level' of the upvalue being closed, as everything after that
** won't be used again.
*/
static void prepcallclosem(cr_State *ts, SPtr level, int status) {
    TValue *v = s2v(level); /* value being closed */
    TValue *errobj;
    if (status == CLOSEKTOP) {
        errobj = &G_(ts)->nil;
    } else { /* top will be set to 'level' + 2 */
        errobj = s2v(level + 1); /* error object goes after 'v' */
        crT_seterrorobj(ts, status, level + 1);
    }
    callclosemethod(ts, v, errobj);
}


/*
** Close all up-values and to-be-closed variables up to (stack) 'level'.
** Returns (potentially restored stack) 'level'.
*/
SPtr crF_close(cr_State *ts, SPtr level, int status) {
    ptrdiff_t relativelevel = savestack(ts, level);
    crF_closeupval(ts, level);
    while (ts->tbclist.p >= level) {
        SPtr tbc = ts->tbclist.p;
        poptbclist(ts);
        prepcallclosem(ts, tbc, status);
        level = restorestack(ts, relativelevel);
    }
    return level;
}


/* free 'UpVal' */
void crF_freeupval(cr_State *ts, UpVal *upval) {
    if (uvisopen(upval))
        unlinkupval(upval);
    crM_free(ts, upval, sizeof(UpVal));
}


/* free 'Function' */
void crF_free(cr_State *ts, Function *fn) {
    crM_freearray(ts, fn->funcs, fn->sizefn, Function);
    crM_freearray(ts, fn->k, fn->sizek, TValue);
    crM_freearray(ts, fn->code, fn->sizecode, Instruction);
    crM_freearray(ts, fn->linfo, fn->sizelinfo, LineInfo);
    crM_freearray(ts, fn->locals, fn->sizelocals, LVarInfo);
    crM_freearray(ts, fn->upvals, fn->sizeupvals, UpValInfo);
    crM_free(ts, fn, sizeof(Function));
}
