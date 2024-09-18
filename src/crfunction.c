#include "crfunction.h"
#include "crgc.h"
#include "crmem.h"

Function *crF_newfunction(cr_State *ts)
{
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


CrClosure *crF_newCrClosure(cr_State *ts, int nup)
{
    CrClosure *crcl = crG_new(ts, sizeofcrcl(nup), CR_VCRCL, CrClosure);
    crcl->nupvalues = nup;
    crcl->fn = NULL;
    for (int i = 0; i < nup; i++)
        crcl->upvals[i] = NULL;
    return crcl;
}


CClosure *crF_newCClosure(cr_State *ts, cr_CFunction fn, int nupvalues)
{
    CClosure *ccl = crG_new(ts, nupvalues * sizeof(TValue), CR_VCCL, CClosure);
    ccl->nupvalues = nupvalues;
    ccl->fn = fn;
    for (int i = 0; i < nupvalues; i++)
        setnilval(&ccl->upvals[i]);
    return ccl;
}


/*
** Adjusts function varargs by moving the named parameters
** and the function in front of the varargs.
** Additionally adjust new top for 'cf' and invalidates
** old named parameters (after they get moved).
*/
void crF_adjustvarargs(cr_State *ts, int arity, CallFrame *cf,
                       const Function *fn)
{
    int i;
    int actual = cast_int(ts->sp.p - cf->callee.p) - 1;
    int extra = actual - arity; /* number of varargs */
    cf->nvarargs = extra;
    crT_checkstack(ts, fn->maxstack + 1);
    setobjs2s(ts, ts->sp.p++, cf->callee.p); /* move function */
    for (i = 0; i < arity; i++) {
        setobjs2s(ts, ts->sp.p++, cf->callee.p + i); /* move param */
        setnilval(s2v(cf->callee.p + i)); /* invalidate old */
    }
    cf->callee.p += actual + 1;
    cf->top.p += actual + 1;
    cr_assert(cf->sp.p <= cf->top.p && cf->top.p <= ts->stackend.p);
}


/* Get 'wanted' varargs starting at the current stack pointer. */
void crF_getvarargs(cr_State *ts, CallFrame *cf, int wanted) {
    int have = cf->nvarargs;
    if (wanted < 0) { /* CR_MULRET ? */
        wanted = have;
        checkstackGC(ts, wanted); /* check stack, maybe 'wanted > have' */
    }
    for (int i = 0; wanted-- && i < have; i++)
        setobjs2s(ts, ts->sp.p++, cf->callee.p - have + i);
    while (wanted--)
        setnilval(s2v(ts->sp.p++));
}


/* Create and initialize all the upvalues in 'cl'. */
void crF_initupvals(cr_State *ts, CrClosure *cl)
{
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = crG_new(ts, sizeof(UpVal), CR_VUVALUE, UpVal);
        uv->v.location = &uv->u.value; /* close it */
        setnilval(uv->v.location);
        cl->upvals[i] = uv;
        crG_objbarrier(ts, cl, uv);
    }
}


/*
** Create a new upvalue and link it into 'openupval' list
** right after 'prev'.
*/
static UpVal *newupval(cr_State *ts, SPtr val, UpVal **prev)
{
    UpVal *uv = crG_new(ts, sizeof(*uv), CR_VUVALUE, UpVal);
    UpVal *previous = *prev;
    uv->v.location = s2v(val);
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
UpVal *crF_findupval(cr_State *ts, SPtr sval)
{
    UpVal *upval;
    SPtr sp;
    cr_assert(isinthwouv(ts) || ts->openupval == NULL);
    UpVal **pp = &ts->openupval;
    while ((upval = *pp) != NULL && (sp = upvaltostk(upval)) > sval) {
        cr_assert(!isdead(&G_(ts)->gc, upval));
        if (sp == sval)
            return upval;
        pp = &upval->u.open.next;
    }
    return newupval(ts, sval, pp);
}


/* unlinks upvalue from the list */
static void unlinkupval(UpVal *upval)
{
    *upval->u.open.prev = upval->u.open.next;
    if (upval->u.open.next)
        upval->u.open.next->u.open.prev = upval->u.open.prev;
}


/* free 'UpVal' */
void crF_freeupval(cr_State *ts, UpVal *upval)
{
    if (uvisopen(upval))
        unlinkupval(upval);
    crM_free(ts, upval, sizeof(UpVal));
}


/* free 'Function' */
void crF_free(cr_State *ts, Function *fn)
{
    crM_freearray(ts, fn->funcs, fn->sizefn);
    crM_freearray(ts, fn->k, fn->sizek);
    crM_freearray(ts, fn->code, fn->sizecode);
    crM_freearray(ts, fn->linfo, fn->sizelinfo);
    crM_freearray(ts, fn->locals, fn->sizelocals);
    crM_freearray(ts, fn->upvals, fn->sizeupvals);
    crM_free(ts, fn, sizeof(Function));
}
