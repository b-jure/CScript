/*
** cfunction.c
** Functions for CScript functions and closures
** See Copyright Notice in cscript.h
*/

#define cfunction_c
#define CS_CORE

#include "cprefix.h"

#include "cfunction.h"
#include "cdebug.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cobject.h"
#include "cstate.h"
#include "cvm.h"
#include "cprotected.h"


Proto *csF_newproto(cs_State *C) {
    GCObject *o = csG_new(C, sizeof(Proto), CS_VPROTO); 
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
    { p->upvals = NULL; p->sizeupvalues = 0; } /* upvalues */
    p->maxstack = 0;
    p->arity = 0;
    p->defline = 0;
    p->deflastline = 0;
    return p;
}


CSClosure *csF_newCSClosure(cs_State *C, int nup) {
    GCObject *o = csG_new(C, sizeofCScl(nup), CS_VCSCL);
    CSClosure *cl = gco2clcs(o);
    cl->p = NULL;
    cl->nupvalues = nup;
    while (nup--) cl->upvals[nup] = NULL;
    return cl;
}


CClosure *csF_newCClosure(cs_State *C, int nupvalues) {
    GCObject *o = csG_new(C, sizeofCcl(nupvalues), CS_VCCL);
    CClosure *cl = gco2clc(o);
    cl->nupvalues = nupvalues;
    return cl;
}


/*
** Adjusts function varargs by moving the named parameters and the
** function in front of the varargs. Additionally adjust new top for
** 'cf' and invalidates old named parameters (after they get moved).
*/
void csF_adjustvarargs(cs_State *C, int arity, CallFrame *cf,
                       const Proto *fn) {
    int actual = cast_int(C->sp.p - cf->func.p) - 1;
    int extra = actual - arity; /* number of varargs */
    cf->cs.nvarargs = extra;
    csPR_checkstack(C, fn->maxstack + 1);
    setobjs2s(C, C->sp.p++, cf->func.p); /* move function */
    for (int i = 1; i <= arity; i++) {
        setobjs2s(C, C->sp.p++, cf->func.p + i); /* move param */
        setnilval(s2v(cf->func.p + i)); /* invalidate old */
    }
    cf->func.p += actual + 1;
    cf->top.p += actual + 1;
    cs_assert(C->sp.p <= cf->top.p && cf->top.p <= C->stackend.p);
}


void csF_getvarargs(cs_State *C, CallFrame *cf, int wanted) {
    int have = cf->cs.nvarargs;
    if (wanted < 0) { /* CS_MULRET? */
        wanted = have;
        checkstackGC(C, wanted); /* check stack, maybe wanted>have */
    }
    for (int i = 0; wanted > 0 && i < have; i++, wanted--)
        setobjs2s(C, C->sp.p++, cf->func.p - have + i);
    while (wanted-- > 0)
        setnilval(s2v(C->sp.p++));
}


/* Create and initialize all the upvalues in 'cl'. */
void csF_initupvals(cs_State *C, CSClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        GCObject *o = csG_new(C, sizeof(UpVal), CS_VUPVALUE);
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
static UpVal *newupval(cs_State *C, SPtr level, UpVal **prev) {
    GCObject *o = csG_new(C, sizeof(UpVal), CS_VUPVALUE);
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
UpVal *csF_findupval(cs_State *C, SPtr level) {
    UpVal **pp = &C->openupval; /* good ol' pp */
    UpVal *p;
    cs_assert(isintwups(C) || C->openupval == NULL);
    while ((p = *pp) != NULL && uvlevel(p) >= level) {
        cs_assert(!isdead(G(C), p));
        if (uvlevel(p) == level) /* corresponding upvalue? */
            return p; /* return it */
        pp = &p->u.open.next; /* get next in the list */
    }
    /* not found: create a new upvalue after 'pp' */
    return newupval(C, level, pp);
}


/*
** Find local variable name that must be alive 'endpc > pc'
** and must be at index 'lnum' in the corresponding scope.
*/
const char *csF_getlocalname(const Proto *fn, int lnum, int pc) {
    cs_assert(lnum > 0);
    for (int i = 0; i < fn->sizelocals && fn->locals->startpc <= pc; i++) {
        if (pc < fn->locals[i].endpc) { /* variable is active? */
            lnum--;
            if (lnum == 0)
                return getstr(fn->locals[i].name);
        }
    }
    return NULL; /* not found */
}


/* 
** Check if object at stack 'level' has a '__close' method,
** raise error if not.
*/
static void checkclosem(cs_State *C, SPtr level) {
    const TValue *fmm = csMM_get(C, s2v(level), CS_MM_CLOSE);
    if (c_unlikely(ttisnil(fmm))) { /* missing __close metamethod? */
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
void csF_newtbcvar(cs_State *C, SPtr level) {
    cs_assert(level > C->tbclist.p);
    if (c_isfalse(s2v(level)))
        return; /* false doesn't need to be closed */
    checkclosem(C, level);
    while (cast_uint(level - C->tbclist.p) > MAXDELTA) {
        C->tbclist.p += MAXDELTA; /* create a dummy node at maximum delta */
        C->tbclist.p->tbc.delta = 0;
    }
    level->tbc.delta = cast(ushort, level - C->tbclist.p);
    C->tbclist.p = level;
}


/*
** Unlink upvalue from the list of open upvalues.
*/
void csF_unlinkupval(UpVal *uv) {
    cs_assert(uvisopen(uv));
    *uv->u.open.prev = uv->u.open.next;
    if (uv->u.open.next)
        uv->u.open.next->u.open.prev = uv->u.open.prev;
}


/*
** Close any open upvalues up to the given stack level.
*/
void csF_closeupval(cs_State *C, SPtr level) {
    UpVal *uv;
    while ((uv = C->openupval) != NULL && uvlevel(uv) >= level) {
        TValue *slot = &uv->u.value; /* new position for value */
        cs_assert(uvlevel(uv) < C->sp.p);
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
static void poptbclist(cs_State *C) {
    SPtr tbc = C->tbclist.p;
    cs_assert(tbc->tbc.delta > 0);
    tbc -= tbc->tbc.delta;
    while (tbc > C->stack.p && tbc->tbc.delta == 0)
        tbc -= MAXDELTA; /* remove dummy nodes */
    C->tbclist.p = tbc;
}


/* 
** Call '__close' method on 'obj' with error object 'errobj'.
** This function assumes 'EXTRA_STACK'.
*/
static void callclosemm(cs_State *C, TValue *obj, TValue *errobj) {
    SPtr top = C->sp.p;
    const TValue *method = csMM_get(C, obj, CS_MM_CLOSE);
    cs_assert(!ttisnil(method));
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
static void prepcallclose(cs_State *C, SPtr level, int status) {
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
** Returns (potentially restored stack) 'level'.
*/
SPtr csF_close(cs_State *C, SPtr level, int status) {
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
void csF_free(cs_State *C, Proto *p) {
    csM_freearray(C, p->p, p->sizep);
    csM_freearray(C, p->k, p->sizek);
    csM_freearray(C, p->code, p->sizecode);
    csM_freearray(C, p->lineinfo, p->sizelineinfo);
    csM_freearray(C, p->abslineinfo, p->sizeabslineinfo);
    csM_freearray(C, p->instpc, p->sizeinstpc);
    csM_freearray(C, p->locals, p->sizelocals);
    csM_freearray(C, p->upvals, p->sizeupvalues);
    csM_free(C, p);
}
