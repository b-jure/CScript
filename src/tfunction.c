/*
** tfunction.c
** Functions for Tokudae functions and closures
** See Copyright Notice in tokudae.h
*/

#define tfunction_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tfunction.h"
#include "tdebug.h"
#include "tgc.h"
#include "tmem.h"
#include "tmeta.h"
#include "tobject.h"
#include "tstate.h"
#include "tvm.h"
#include "tprotected.h"


Proto *csF_newproto(toku_State *T) {
    GCObject *o = csG_new(C, sizeof(Proto), TOKU_VPROTO); 
    Proto *p = gco2proto(o);
    p->isvararg = 0;
    p->gclist = NULL;
    p->source = NULL;
    { p->p = NULL; p->sizep = 0; } /* function prototypes */
    { p->k = NULL; p->sizek = 0; } /* constants */
    { p->code = NULL; p->sizecode = 0; } /* code */
    { p->lineinfo = NULL; p->sizelineinfo = 0; } /* rel. line info */
    { p->abslineinfo = NULL; p->sizeabslineinfo = 0; } /* abs. line info */
    { p->instpc = NULL; p->sizeinstpc = 0; } /* instruction pc's */
    { p->locals = NULL; p->sizelocals = 0; } /* locals */
    { p->upvals = NULL; p->sizeupvals = 0; } /* upvalues */
    p->maxstack = 0;
    p->arity = 0;
    p->defline = 0;
    p->deflastline = 0;
    return p;
}


CSClosure *csF_newCSClosure(toku_State *T, int nup) {
    GCObject *o = csG_new(C, sizeofCScl(nup), TOKU_VCSCL);
    CSClosure *cl = gco2clcs(o);
    cl->p = NULL;
    cl->nupvalues = nup;
    while (nup--) cl->upvals[nup] = NULL;
    return cl;
}


CClosure *csF_newCClosure(toku_State *T, int nupvalues) {
    GCObject *o = csG_new(C, sizeofCcl(nupvalues), TOKU_VCCL);
    CClosure *cl = gco2clc(o);
    cl->nupvalues = nupvalues;
    return cl;
}


/*
** Adjusts function varargs by moving the named parameters and the
** function in front of the varargs. Additionally adjust new top for
** 'cf' and invalidates old named parameters (after they get moved).
*/
void csF_adjustvarargs(toku_State *T, int arity, CallFrame *cf,
                       SPtr *sp, const Proto *fn) {
    int actual = cast_int(C->sp.p - cf->func.p) - 1;
    int extra = actual - arity; /* number of varargs */
    cf->cs.nvarargs = extra;
    checkstackp(C, fn->maxstack + 1, *sp);
    setobjs2s(C, C->sp.p++, cf->func.p); /* move function to the top */
    for (int i = 1; i <= arity; i++) { /* move params to the top */
        setobjs2s(C, C->sp.p++, cf->func.p + i);
        setnilval(s2v(cf->func.p + i)); /* erase original (for GC) */
    }
    cf->func.p += actual + 1;
    cf->top.p += actual + 1;
    toku_assert(C->sp.p <= cf->top.p && cf->top.p <= C->stackend.p);
    *sp = C->sp.p;
}


void csF_getvarargs(toku_State *T, CallFrame *cf, SPtr *sp, int wanted) {
    int have = cf->cs.nvarargs;
    if (wanted < 0) { /* TOKU_MULTRET? */
        wanted = have;
        checkstackGCp(C, wanted, *sp); /* check stack, maybe wanted>have */
    }
    for (int i = 0; wanted > 0 && i < have; i++, wanted--)
        setobjs2s(C, C->sp.p++, cf->func.p - have + i);
    while (wanted-- > 0)
        setnilval(s2v(C->sp.p++));
    *sp = C->sp.p;
}


/* Create and initialize all the upvalues in 'cl'. */
void csF_initupvals(toku_State *T, CSClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        GCObject *o = csG_new(C, sizeof(UpVal), TOKU_VUPVALUE);
        UpVal *uv = gco2uv(o);
        uv->v.p = &uv->u.value; /* close it */
        setnilval(uv->v.p);
        cl->upvals[i] = uv;
        csG_objbarrier(C, cl, uv);
    }
}


/*
** Create a new upvalue at the given level, and link it to the list of
** open upvalues of 'l' after entry 'prev'.
*/
static UpVal *newupval(toku_State *T, SPtr level, UpVal **prev) {
    GCObject *o = csG_new(C, sizeof(UpVal), TOKU_VUPVALUE);
    UpVal *uv = gco2uv(o);
    UpVal *next = *prev;
    uv->v.p = s2v(level); /* current value lives on the stack */
    uv->u.open.next = next; /* link it to the list of open upvalues */
    uv->u.open.prev = prev;
    if (next)
        next->u.open.prev = &uv->u.open.next;
    *prev = uv;
    if (!isintwups(C)) { /* thread not in list of threads with upvalues? */
        C->twups = G(C)->twups; /* link it to the list */
        G(C)->twups = C;
    }
    return uv;
}


/*
** Find and reuse, or create if it does not exist, an upvalue
** at the given level.
*/
UpVal *csF_findupval(toku_State *T, SPtr level) {
    UpVal **pp = &C->openupval; /* good ol' pp */
    UpVal *p;
    toku_assert(isintwups(C) || C->openupval == NULL);
    while ((p = *pp) != NULL && uvlevel(p) >= level) {
        toku_assert(!isdead(G(C), p));
        if (uvlevel(p) == level) /* corresponding upvalue? */
            return p; /* return it */
        pp = &p->u.open.next; /* get next in the list */
    }
    /* not found: create a new upvalue after 'pp' */
    return newupval(C, level, pp);
}


/*
** Find local variable name that must be alive for the given 'pc',
** and at the position 'lnum', meaning there are 'lnum' locals before it.
*/
const char *csF_getlocalname(const Proto *fn, int lnum, int pc) {
    for (int i = 0; i < fn->sizelocals && fn->locals[i].startpc <= pc; i++) {
        if (pc < fn->locals[i].endpc) { /* variable is active? */
            if (--lnum == 0)
                return getstr(fn->locals[i].name);
        }
    }
    return NULL; /* not found */
}


/* 
** Check if object at stack 'level' has a '__close' method,
** raise error if not.
*/
static void checkclosem(toku_State *T, SPtr level) {
    const TValue *fmm = csMM_get(C, s2v(level), TOKU_MT_CLOSE);
    if (t_unlikely(ttisnil(fmm))) { /* missing __close metamethod? */
        int vidx = cast_int(level - C->cf->func.p);
        const char *name = csD_findlocal(C, C->cf, vidx, NULL);
        if (name == NULL) name = "?";
        csD_runerror(C, "local variable %s got a non-closeable value", name);
    }
}


/* 
** Maximum value for 'delta', dependant on the data type
** of 'tbc.delta'.
*/
#define MAXDELTA \
        ((256UL << ((sizeof(C->stack.p->tbc.delta) - 1) * 8)) - 1)


/*
** Insert value at the given stack level into the to-be-closed list.
*/
void csF_newtbcvar(toku_State *T, SPtr level) {
    toku_assert(level > C->tbclist.p);
    if (t_isfalse(s2v(level)))
        return; /* false doesn't need to be closed */
    checkclosem(C, level);
    while (cast_uint(level - C->tbclist.p) > MAXDELTA) {
        C->tbclist.p += MAXDELTA; /* create a dummy node at maximum delta */
        C->tbclist.p->tbc.delta = 0;
    }
    level->tbc.delta = cast(t_ushort, level - C->tbclist.p);
    C->tbclist.p = level;
}


/*
** Unlink upvalue from the list of open upvalues.
*/
void csF_unlinkupval(UpVal *uv) {
    toku_assert(uvisopen(uv));
    *uv->u.open.prev = uv->u.open.next;
    if (uv->u.open.next)
        uv->u.open.next->u.open.prev = uv->u.open.prev;
}


/*
** Close any open upvalues up to the given stack level.
*/
void csF_closeupval(toku_State *T, SPtr level) {
    UpVal *uv;
    while ((uv = C->openupval) != NULL && uvlevel(uv) >= level) {
        TValue *slot = &uv->u.value; /* new position for value */
        toku_assert(uvlevel(uv) < C->sp.p);
        csF_unlinkupval(uv); /* remove it from 'openupval' list */
        setobj(C, slot, uv->v.p); /* move value to the upvalue slot */
        uv->v.p = slot; /* adjust its pointer */
        if (!iswhite(uv)) { /* neither white nor dead? */
            notw2black(uv); /* closed upvalues cannot be gray */
            csG_barrier(C, uv, slot);
        }
    }
}


/*
** Remove first value from 'tbclist'.
*/
static void poptbclist(toku_State *T) {
    SPtr tbc = C->tbclist.p;
    toku_assert(tbc->tbc.delta > 0);
    tbc -= tbc->tbc.delta;
    while (tbc > C->stack.p && tbc->tbc.delta == 0)
        tbc -= MAXDELTA; /* remove dummy nodes */
    C->tbclist.p = tbc;
}


/* 
** Call '__close' method on 'obj' with error object 'errobj'.
** This function assumes 'EXTRA_STACK'.
*/
static void callclosemm(toku_State *T, TValue *obj, TValue *errobj) {
    SPtr top = C->sp.p;
    const TValue *method = csMM_get(C, obj, TOKU_MT_CLOSE);
    toku_assert(!ttisnil(method));
    setobj2s(C, top, method);
    setobj2s(C, top + 1, obj);
    setobj2s(C, top + 2, errobj);
    C->sp.p = top + 3;
    csV_call(C, top, 0);
}


/*
** Prepare and call '__close' method.
** If status is CLOSEKTOP, the call to the closing method will be pushed
** at the top of the stack. Otherwise, values can be pushed right after
** the 'level' of the upvalue being closed, as everything after that
** won't be used again.
*/
static void prepcallclose(toku_State *T, SPtr level, int status) {
    TValue *uv = s2v(level); /* value being closed */
    TValue *errobj;
    if (status == CLOSEKTOP)
        errobj = &G(C)->nil; /* error object is nil */
    else { /* 'csPR_seterrorobj' will set top to level + 2 */
        errobj = s2v(level + 1); /* error object goes after 'uv' */
        csPR_seterrorobj(C, status, level + 1); /* set error object */
    }
    callclosemm(C, uv, errobj);
}


/*
** Close all up-values and to-be-closed variables up to (stack) 'level'.
** Returns (restored) level.
*/
SPtr csF_close(toku_State *T, SPtr level, int status) {
    ptrdiff_t levelrel = savestack(C, level);
    csF_closeupval(C, level);
    while (C->tbclist.p >= level) {
        SPtr tbc = C->tbclist.p;
        poptbclist(C);
        prepcallclose(C, tbc, status);
        level = restorestack(C, levelrel);
    }
    return level;
}


/* free function prototype */
void csF_free(toku_State *T, Proto *p) {
    csM_freearray(C, p->p, p->sizep);
    csM_freearray(C, p->k, p->sizek);
    csM_freearray(C, p->code, p->sizecode);
    csM_freearray(C, p->lineinfo, p->sizelineinfo);
    csM_freearray(C, p->abslineinfo, p->sizeabslineinfo);
    csM_freearray(C, p->instpc, p->sizeinstpc);
    csM_freearray(C, p->locals, p->sizelocals);
    csM_freearray(C, p->upvals, p->sizeupvals);
    csM_free(C, p);
}
