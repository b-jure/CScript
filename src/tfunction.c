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


Proto *tokuF_newproto(toku_State *T) {
    GCObject *o = tokuG_new(T, sizeof(Proto), TOKU_VPROTO); 
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


TClosure *tokuF_newTclosure(toku_State *T, int nup) {
    GCObject *o = tokuG_new(T, sizeofTcl(nup), TOKU_VCSCL);
    TClosure *cl = gco2clt(o);
    cl->p = NULL;
    cl->nupvalues = nup;
    while (nup--) cl->upvals[nup] = NULL;
    return cl;
}


CClosure *tokuF_newCclosure(toku_State *T, int nupvalues) {
    GCObject *o = tokuG_new(T, sizeofCcl(nupvalues), TOKU_VCCL);
    CClosure *cl = gco2clc(o);
    cl->nupvalues = nupvalues;
    return cl;
}


/*
** Adjusts function varargs by moving the named parameters and the
** function in front of the varargs. Additionally adjust new top for
** 'cf' and invalidates old named parameters (after they get moved).
*/
void tokuF_adjustvarargs(toku_State *T, int arity, CallFrame *cf,
                       SPtr *sp, const Proto *fn) {
    int actual = cast_int(T->sp.p - cf->func.p) - 1;
    int extra = actual - arity; /* number of varargs */
    cf->t.nvarargs = extra;
    checkstackp(T, fn->maxstack + 1, *sp);
    setobjs2s(T, T->sp.p++, cf->func.p); /* move function to the top */
    for (int i = 1; i <= arity; i++) { /* move params to the top */
        setobjs2s(T, T->sp.p++, cf->func.p + i);
        setnilval(s2v(cf->func.p + i)); /* erase original (for GC) */
    }
    cf->func.p += actual + 1;
    cf->top.p += actual + 1;
    toku_assert(T->sp.p <= cf->top.p && cf->top.p <= T->stackend.p);
    *sp = T->sp.p;
}


void tokuF_getvarargs(toku_State *T, CallFrame *cf, SPtr *sp, int wanted) {
    int have = cf->t.nvarargs;
    if (wanted < 0) { /* TOKU_MULTRET? */
        wanted = have;
        checkstackGCp(T, wanted, *sp); /* check stack, maybe wanted>have */
    }
    for (int i = 0; wanted > 0 && i < have; i++, wanted--)
        setobjs2s(T, T->sp.p++, cf->func.p - have + i);
    while (wanted-- > 0)
        setnilval(s2v(T->sp.p++));
    *sp = T->sp.p;
}


/* Create and initialize all the upvalues in 'cl'. */
void tokuF_initupvals(toku_State *T, TClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        GCObject *o = tokuG_new(T, sizeof(UpVal), TOKU_VUPVALUE);
        UpVal *uv = gco2uv(o);
        uv->v.p = &uv->u.value; /* close it */
        setnilval(uv->v.p);
        cl->upvals[i] = uv;
        tokuG_objbarrier(T, cl, uv);
    }
}


/*
** Create a new upvalue at the given level, and link it to the list of
** open upvalues of 'l' after entry 'prev'.
*/
static UpVal *newupval(toku_State *T, SPtr level, UpVal **prev) {
    GCObject *o = tokuG_new(T, sizeof(UpVal), TOKU_VUPVALUE);
    UpVal *uv = gco2uv(o);
    UpVal *next = *prev;
    uv->v.p = s2v(level); /* current value lives on the stack */
    uv->u.open.next = next; /* link it to the list of open upvalues */
    uv->u.open.prev = prev;
    if (next)
        next->u.open.prev = &uv->u.open.next;
    *prev = uv;
    if (!isintwups(T)) { /* thread not in list of threads with upvalues? */
        T->twups = G(T)->twups; /* link it to the list */
        G(T)->twups = T;
    }
    return uv;
}


/*
** Find and reuse, or create if it does not exist, an upvalue
** at the given level.
*/
UpVal *tokuF_findupval(toku_State *T, SPtr level) {
    UpVal **pp = &T->openupval; /* good ol' pp */
    UpVal *p;
    toku_assert(isintwups(T) || T->openupval == NULL);
    while ((p = *pp) != NULL && uvlevel(p) >= level) {
        toku_assert(!isdead(G(T), p));
        if (uvlevel(p) == level) /* corresponding upvalue? */
            return p; /* return it */
        pp = &p->u.open.next; /* get next in the list */
    }
    /* not found: create a new upvalue after 'pp' */
    return newupval(T, level, pp);
}


/*
** Find local variable name that must be alive for the given 'pc',
** and at the position 'lnum', meaning there are 'lnum' locals before it.
*/
const char *tokuF_getlocalname(const Proto *fn, int lnum, int pc) {
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
    const TValue *fmm = tokuTM_objget(T, s2v(level), TM_CLOSE);
    if (t_unlikely(ttisnil(fmm))) { /* missing __close metamethod? */
        int vidx = cast_int(level - T->cf->func.p);
        const char *name = tokuD_findlocal(T, T->cf, vidx, NULL);
        if (name == NULL) name = "?";
        tokuD_runerror(T, "local variable %s got a non-closeable value", name);
    }
}


/* 
** Maximum value for 'delta', dependant on the data type
** of 'tbc.delta'.
*/
#define MAXDELTA \
        ((256UL << ((sizeof(T->stack.p->tbc.delta) - 1) * 8)) - 1)


/*
** Insert value at the given stack level into the to-be-closed list.
*/
void tokuF_newtbcvar(toku_State *T, SPtr level) {
    toku_assert(level > T->tbclist.p);
    if (t_isfalse(s2v(level)))
        return; /* false doesn't need to be closed */
    checkclosem(T, level);
    while (cast_uint(level - T->tbclist.p) > MAXDELTA) {
        T->tbclist.p += MAXDELTA; /* create a dummy node at maximum delta */
        T->tbclist.p->tbc.delta = 0;
    }
    level->tbc.delta = cast(t_ushort, level - T->tbclist.p);
    T->tbclist.p = level;
}


/*
** Unlink upvalue from the list of open upvalues.
*/
void tokuF_unlinkupval(UpVal *uv) {
    toku_assert(uvisopen(uv));
    *uv->u.open.prev = uv->u.open.next;
    if (uv->u.open.next)
        uv->u.open.next->u.open.prev = uv->u.open.prev;
}


/*
** Close any open upvalues up to the given stack level.
*/
void tokuF_closeupval(toku_State *T, SPtr level) {
    UpVal *uv;
    while ((uv = T->openupval) != NULL && uvlevel(uv) >= level) {
        TValue *slot = &uv->u.value; /* new position for value */
        toku_assert(uvlevel(uv) < T->sp.p);
        tokuF_unlinkupval(uv); /* remove it from 'openupval' list */
        setobj(T, slot, uv->v.p); /* move value to the upvalue slot */
        uv->v.p = slot; /* adjust its pointer */
        if (!iswhite(uv)) { /* neither white nor dead? */
            notw2black(uv); /* closed upvalues cannot be gray */
            tokuG_barrier(T, uv, slot);
        }
    }
}


/*
** Remove first value from 'tbclist'.
*/
static void poptbclist(toku_State *T) {
    SPtr tbc = T->tbclist.p;
    toku_assert(tbc->tbc.delta > 0);
    tbc -= tbc->tbc.delta;
    while (tbc > T->stack.p && tbc->tbc.delta == 0)
        tbc -= MAXDELTA; /* remove dummy nodes */
    T->tbclist.p = tbc;
}


/* 
** Call '__close' method on 'obj' with error object 'errobj'.
** This function assumes 'EXTRA_STACK'.
*/
static void callclosemm(toku_State *T, TValue *obj, TValue *errobj) {
    SPtr top = T->sp.p;
    const TValue *method = tokuTM_objget(T, obj, TM_CLOSE);
    toku_assert(!ttisnil(method));
    setobj2s(T, top, method);
    setobj2s(T, top + 1, obj);
    setobj2s(T, top + 2, errobj);
    T->sp.p = top + 3;
    tokuV_call(T, top, 0);
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
        errobj = &G(T)->nil; /* error object is nil */
    else { /* 'tokuPR_seterrorobj' will set top to level + 2 */
        errobj = s2v(level + 1); /* error object goes after 'uv' */
        tokuPR_seterrorobj(T, status, level + 1); /* set error object */
    }
    callclosemm(T, uv, errobj);
}


/*
** Close all up-values and to-be-closed variables up to (stack) 'level'.
** Returns (restored) level.
*/
SPtr tokuF_close(toku_State *T, SPtr level, int status) {
    ptrdiff_t levelrel = savestack(T, level);
    tokuF_closeupval(T, level);
    while (T->tbclist.p >= level) {
        SPtr tbc = T->tbclist.p;
        poptbclist(T);
        prepcallclose(T, tbc, status);
        level = restorestack(T, levelrel);
    }
    return level;
}


/* free function prototype */
void tokuF_free(toku_State *T, Proto *p) {
    tokuM_freearray(T, p->p, cast_uint(p->sizep));
    tokuM_freearray(T, p->k, cast_uint(p->sizek));
    tokuM_freearray(T, p->code, cast_uint(p->sizecode));
    tokuM_freearray(T, p->lineinfo, cast_uint(p->sizelineinfo));
    tokuM_freearray(T, p->abslineinfo, cast_uint(p->sizeabslineinfo));
    tokuM_freearray(T, p->instpc, cast_uint(p->sizeinstpc));
    tokuM_freearray(T, p->locals, cast_uint(p->sizelocals));
    tokuM_freearray(T, p->upvals, cast_uint(p->sizeupvals));
    tokuM_free(T, p);
}
