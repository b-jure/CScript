/*
** cfunction.c
** Functions for CScript functions and closures
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cfunction.h"
#include "cdebug.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cobject.h"
#include "cstate.h"
#include "cvm.h"

#include <stdio.h>


Proto *csF_newproto(cs_State *ts) {
    GCObject *o = csG_new(ts, sizeof(Proto), CS_VPROTO); 
    Proto *p = gco2proto(o);
    p->isvararg = 0;
    p->gclist = NULL;
    p->source = NULL;
    { p->p = NULL; p->sizep = 0; } /* function prototypes */
    { p->k = NULL; p->sizek = 0; } /* constants */
    { p->code = NULL; p->sizecode = 0; } /* code */
    { p->linfo = NULL; p->sizelinfo = 0; } /* line information */
    { p->locals = NULL; p->sizelocals = 0; } /* locals */
    { p->upvals = NULL; p->sizeupvals = 0; } /* upvalues */
    p->maxstack = 0;
    p->arity = 0;
    p->defline = 0;
    p->deflastline = 0;
    return p;
}


CSClosure *csF_newCSClosure(cs_State *ts, int nup) {
    GCObject *o = csG_new(ts, sizeofCScl(nup), CS_VCSCL);
    CSClosure *cl = gco2clcs(o);
    cl->p = NULL;
    cl->nupvalues = nup;
    while (nup--) cl->upvals[nup] = NULL;
    return cl;
}


CClosure *csF_newCClosure(cs_State *ts, int nupvalues) {
    GCObject *o = csG_new(ts, sizeofCcl(nupvalues), CS_VCCL);
    CClosure *cl = gco2clc(o);
    cl->nupvalues = nupvalues;
    return cl;
}


/*
** Adjusts function varargs by moving the named parameters
** and the function in front of the varargs.
** Additionally adjust new top for 'cf' and invalidates
** old named parameters (after they get moved).
*/
void csF_adjustvarargs(cs_State *ts, int arity, CallFrame *cf,
                       const Proto *fn) {
    int i;
    int actual = cast_int(ts->sp.p - cf->func.p) - 1;
    int extra = actual - arity; /* number of varargs */
    cf->nvarargs = extra;
    csT_checkstack(ts, fn->maxstack + 1);
    setobjs2s(ts, ts->sp.p++, cf->func.p); /* move function */
    for (i = 0; i < arity; i++) {
        setobjs2s(ts, ts->sp.p++, cf->func.p + i); /* move param */
        setnilval(s2v(cf->func.p + i)); /* invalidate old */
    }
    cf->func.p += actual + 1;
    cf->top.p += actual + 1;
    cs_assert(ts->sp.p <= cf->top.p && cf->top.p <= ts->stackend.p);
}


/* Get 'wanted' varargs starting at the current stack pointer. */
void csF_getvarargs(cs_State *ts, CallFrame *cf, int wanted) {
    int have = cf->nvarargs;
    if (wanted < 0) { /* CS_MULRET? */
        wanted = have;
        checkstackGC(ts, wanted); /* check stack, maybe 'wanted > have' */
    }
    for (int i = 0; wanted > 0 && i < have; i++, wanted--)
        setobjs2s(ts, ts->sp.p++, cf->func.p - have + i);
    while (wanted--)
        setnilval(s2v(ts->sp.p++));
}


/* Create and initialize all the upvalues in 'cl'. */
void csF_initupvals(cs_State *ts, CSClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        GCObject *o = csG_new(ts, sizeof(UpVal), CS_VUPVALUE);
        UpVal *uv = gco2uv(o);
        uv->v.p = &uv->u.value; /* close it */
        setnilval(uv->v.p);
        cl->upvals[i] = uv;
        csG_objbarrier(ts, cl, uv);
    }
}


/*
** Create a new upvalue and link it into 'openupval' list
** right after 'prev'.
*/
static UpVal *newupval(cs_State *ts, SPtr val, UpVal **prev) {
    GCObject *o = csG_new(ts, sizeof(UpVal), CS_VUPVALUE);
    UpVal *uv = gco2uv(o);;
    UpVal *previous = *prev;
    uv->v.p = s2v(val);
    uv->u.open.next = previous;
    if (previous) /* have previous upval ? */
        previous->u.open.prev = &uv->u.open.next;
    *prev = uv; /* adjust list head or 'previous.u.open.next' */
    cs_assert(prev == &ts->openupval);
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
UpVal *csF_findupval(cs_State *ts, SPtr sval) {
    UpVal *upval;
    SPtr sp;
    cs_assert(isinthwouv(ts) || ts->openupval == NULL);
    UpVal **pp = &ts->openupval; /* good ol' pp */
    while ((upval = *pp) != NULL && (sp = uvlevel(upval)) > sval) {
        cs_assert(!isdead(G_(ts), upval));
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
const char *csF_getlocalname(const Proto *fn, int lnum, int pc) {
    cs_assert(lnum > 0);
    for (int i = 0; i < fn->sizelocals && fn->locals->startpc <= pc; i++)
        if ((lnum -= (pc < fn->locals[i].endpc)) == 0)
            return getstr(fn->locals[i].name);
    return NULL;
}


/* 
** Check if object at stack 'level' has a '__close' method,
** raise error if not.
*/
static void checkclosem(cs_State *ts, SPtr level) {
    const TValue *fn = csMM_get(ts, s2v(level), CS_MM_CLOSE);
    if (c_unlikely(ttisnil(fn))) { /* missing '__close' method ? */
        int vidx = level - ts->cf->func.p;
        const char *name = csD_findlocal(ts, ts->cf, vidx, NULL);
        if (name == NULL) name = "?";
        csD_runerror(ts, "variable %s got a non-closeable value", name);
    }
}


/* 
** Maximum value for 'delta', dependant on the data type
** of 'tbc.delta'.
*/
#define MAXDELTA \
    ((256UL << ((sizeof(ts->stack.p->tbc.delta) - 1) * 8)) - 1)


/* insert variable into the list of to-be-closed variables */
void csF_newtbcvar(cs_State *ts, SPtr level) {
    if (c_isfalse(s2v(level)))
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
void csF_closeupval(cs_State *ts, SPtr level) {
    UpVal *uv;
    while ((uv = ts->openupval) != NULL && uvlevel(uv) >= level) {
        TValue *slot = &uv->u.value; /* new position for value */
        cs_assert(uvlevel(uv) < ts->sp.p);
        unlinkupval(uv); /* remove it from 'openupval' list */
        setobj(ts, slot, uv->v.p); /* move value to the upvalue slot */
        uv->v.p = slot; /* adjust its pointer */
        if (!iswhite(uv)) { /* neither white nor dead? */
            notw2black(uv); /* closed upvalues cannot be gray */
            csG_barrier(ts, uv, slot);
        }
    }
}


/* remove first element from 'tbclist' */
static void poptbclist(cs_State *ts) {
    SPtr tbc = ts->tbclist.p;
    cs_assert(tbc->tbc.delta > 0);
    tbc -= tbc->tbc.delta;
    while (tbc > ts->stack.p && tbc->tbc.delta == 0)
        tbc -= MAXDELTA; /* remove dummy nodes */
    ts->tbclist.p = tbc;
}


/* 
** Call '__close' method on 'obj' with error object 'errobj'.
** This function assumes 'EXTRA_STACK'.
*/
static void callclosemethod(cs_State *ts, TValue *obj, TValue *errobj) {
    SPtr top = ts->sp.p;
    const TValue *method = csMM_get(ts, obj, CS_MM_CLOSE);
    cs_assert(!ttisnil(method));
    setobj2s(ts, top + 1, method);
    setobj2s(ts, top + 2, obj);
    setobj2s(ts, top + 3, errobj);
    ts->sp.p = top + 3;
    csV_call(ts, top, 0);
}


/*
** Prepare and call '__close' method.
** If status is CLOSEKTOP, the call to the closing method will be pushed
** at the top of the stack. Otherwise, values can be pushed right after
** the 'level' of the upvalue being closed, as everything after that
** won't be used again.
*/
static void prepcallclosem(cs_State *ts, SPtr level, int status) {
    TValue *v = s2v(level); /* value being closed */
    TValue *errobj;
    if (status == CLOSEKTOP) {
        errobj = &G_(ts)->nil; /* error object is nil */
    } else { /* top will be set to 'level' + 2 */
        errobj = s2v(level + 1); /* error object goes after 'v' */
        csT_seterrorobj(ts, status, level + 1); /* set error object */
    }
    callclosemethod(ts, v, errobj);
}


/*
** Close all up-values and to-be-closed variables up to (stack) 'level'.
** Returns (potentially restored stack) 'level'.
*/
SPtr csF_close(cs_State *ts, SPtr level, int status) {
    ptrdiff_t rellevel = savestack(ts, level);
    csF_closeupval(ts, level);
    while (ts->tbclist.p >= level) {
        SPtr tbc = ts->tbclist.p;
        poptbclist(ts);
        prepcallclosem(ts, tbc, status);
        level = restorestack(ts, rellevel);
    }
    return level;
}


/* free 'UpVal' */
void csF_freeupval(cs_State *ts, UpVal *upval) {
    if (uvisopen(upval))
        unlinkupval(upval);
    csM_free(ts, upval);
}


/* free function prototype */
void csF_free(cs_State *ts, Proto *fn) {
    csM_freearray(ts, fn->p, fn->sizep);
    csM_freearray(ts, fn->k, fn->sizek);
    csM_freearray(ts, fn->code, fn->sizecode);
    csM_freearray(ts, fn->linfo, fn->sizelinfo);
    csM_freearray(ts, fn->locals, fn->sizelocals);
    csM_freearray(ts, fn->upvals, fn->sizeupvals);
    csM_free(ts, fn);
}
