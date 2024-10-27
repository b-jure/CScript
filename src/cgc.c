/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "cgc.h"
#include "cconf.h"
#include "cdebug.h"
#include "cfunction.h"
#include "climits.h"
#include "cmeta.h"
#include "cobject.h"
#include "cstate.h"
#include "chashtable.h"
#include "cmem.h"
#include "cstring.h"
#include "cvm.h"
#include "cprotected.h"


/* mark object as current white */
#define markwhite(gc,o) \
    (gcomark_(o) = (gcomark_(o) & ~COLORBITS) | crG_white(gc))

/* mark object as black */
#define markblack(o) \
    (gcomark_(o) = (gcomark_(o) & ~COLORBITS) | BLACKBIT)

/* mark object as gray */
#define markgray(o)	resetbits(gcomark_(o), COLORBITS)



/* check if 'TValue' is object and white */
#define valiswhite(v)		(iscollectable(v) && iswhite(gcoval(v)))

/* check if 'HTable' key is object and white */
#define keyiswhite(n)		(keyisobj(n) && iswhite(keygcoval(n)))



/* 'markobject_' but only if 'v' is object and white */
#define markvalue(gc,v) \
    (valiswhite(v) ? markobject_(gc, gcoval(v)) : (void)0)

/* 'markobject_' but only if 'o' is non-NULL */
#define markobjectcheck(gc,o)	((o) ? markobject_(gc, obj2gco(o)) : (void)0)

/* 'markobject_' but only if object is white */
#define markobject(gc,o) \
    (iswhite(o) ? markobject_(gc, obj2gco(o)) : (void)0)

/* 'markobject' but only if key value is object and white */
#define markkey(gc, n) \
    (keyiswhite(n) ? markobject_(gc, keygcoval(n)) : (void)0)



/* link objects 'gclist' into the list 'l' */
#define linkgclist(o,l)		linkgclist_(obj2gco(o), &(o)->gclist, &(l))

/* simmilar to 'linkgclist' but generic */
#define linkobjgclist(o,l)	linkgclist_(obj2gco(o), getgclist(o), &(l))


/* maximum amount of objects to sweep in a single 'sweepstep' */
#define GCSWEEPMAX	100


/* maximum number of finalizers to call in each 'singlestep' */
#define GCFINMAX	10


/* cost of calling one finalizer */
#define GCFINCOST	50


/*
 * Action of visiting a slot or sweeping an object converted
 * into bytes.
 */
#define WORK2MEM	sizeof(TValue)


/* adjust 'pause' (same as in Lua, percentage of the 'pause') */
#define PAUSEADJ	100




/* forward declare */
static void markobject_(GC *gc, GCObject *o);



void crG_init(GC *gc, cr_State *ts, size_t LGsize) {
    gc->next = 0;
    gc->allocated = 0;
    gc->debt = 0;
    gc->total = LGsize;
    /* 'estimate' set on each cycle */
    gc->objects = obj2gco(ts); /* mainthread */
    gc->sweeppos = NULL;
    gc->graylist = gc->grayagain = gc->weak = NULL;
    gc->fixed = gc->fin = gc->tobefin = NULL;
    setgcparam(gc->pause, CRI_GCPAUSE);
    setgcparam(gc->stepmul, CRI_GCSTEPMUL);
    gc->stepsize = CRI_GCSTEPSIZE;
    gc->state = GCSpause;
    gc->stopem = 0;
    gc->stopped = GCSTP; /* stop while initializing */
    gc->whitebit = bitmask(WHITEBIT0);
    gc->isem = 0;
    gc->stopem = 0;
}


/* create new object and append it to 'objects' */
GCObject *crG_newoff(cr_State *ts, size_t sz, int tt_, size_t offset) {
    GC *gc = &G_(ts)->gc;
    char *p = cast_charp(crM_malloc(ts, sz));
    GCObject *o = cast(GCObject*, p + offset);
    o->mark = crG_white(gc);
    o->tt_ = tt_;
    o->next = gc->objects;
    gc->objects = o;
    return o;
}


GCObject *crG_new_(cr_State *ts, size_t size, int tt_) {
    return crG_newoff(ts, size, tt_, 0);
}


void crG_fix(cr_State *ts, GCObject *o) {
    GC *gc = &G_(ts)->gc;
    cr_assert(o == gc->objects); /* first in the list */
    markgray(o);
    gc->objects = o->next;
    o->next = gc->fixed;
    gc->fixed = o;
}


/* set collector debt */
void crG_setdebt(GC *gc, cr_mem debt) {
    cr_mem total = totalbytes(gc);
    if (debt < total - CRMEM_MAX) /* 'total' will underflow ? */
        debt = total - CRMEM_MAX;
    gc->total = total - debt;
    gc->debt = debt;
}


static void linkgclist_(GCObject *o, GCObject **gclist, GCObject **list) {
    cr_assert(!isgray(o));
    *gclist = *list;
    *list = o;
    markgray(o);
}


static GCObject **getgclist(GCObject *o) {
    switch (o->tt_) {
    case CR_VFUNCTION: return &gco2fn(o)->gclist;
    case CR_VCRCL: return &gco2crcl(o)->gclist;
    case CR_VCCL: return &gco2ccl(o)->gclist;
    case CR_VCLASS: return &gco2cls(o)->gclist;
    case CR_VHTABLE: return &gco2ht(o)->gclist;
    case CR_VTHREAD: return &gco2th(o)->gclist;
    case CR_VUDATA: {
         UserData *ud = gco2ud(o);
         UNUSED(ud);
         cr_assert(ud->nuv > 0 || ud->vmt);
         return &gco2ud(o)->gclist;
    }
    default: cr_unreachable();
    }
}


/*
 * Write barrier that marks white object 'o' pointed to by black
 * object 'r' (as in root), effectively moving the collector forward.
 * This is to ensure that the garbage collector doesn't miss any objects
 * that have become reachable since the last collection cycle and
 * to maintain the invariant that no black object points to a white object.
 * In case we are in sweep phase, then just mark 'r' as white
 * to prevent further write barriers for performance purposes and because
 * the invariant state in sweep phases might be violated.
 * Not to worry, the white bits after atomic phase are switched
 * around, so marking 'r' white won't make it collectable until
 * the next cycle.
 */
void crG_barrier_(cr_State *ts, GCObject *r, GCObject *o) {
    GC *gc = &G_(ts)->gc;
    if (invariantstate(gc)) { /* invariant holds ? */
        cr_assert(isblack(r) && iswhite(o));
        markobject_(gc, o);
    } else { /* in sweep phase */
        cr_assert(sweepstate(gc));
        markwhite(gc, r);
    }
}


/*
 * Write barrier that marks the black object 'r' that is
 * pointing to a white object gray again, effectively
 * moving the collector backwards.
 */
void crG_barrierback_(cr_State *ts, GCObject *r) {
    GC *gc = &G_(ts)->gc;
    cr_assert(isblack(r));
    linkobjgclist(r, gc->grayagain);
}



/* ------------------------------------------------------------------------
 * Mark functions
 * ------------------------------------------------------------------------- */

/*
 * Marks white object 'o'.
 * Some objects are directly marked as black, these
 * objects do not point to other objects, or their references
 * can be resolved by a single recursive call to this function,
 * meaning the actual 'work' they do is 1 (look at other marking
 * functions for more information).
 * Other objects are marked gray, more precisely they are
 * first moved into 'gray' list and then marked as gray.
 */
static void markobject_(GC *gc, GCObject *o) {
    cr_assert(iswhite(o));
    switch(o->tt_) {
    case CR_VSTRING: {
        notw2black(o);
        break;
    }
    case CR_VUVALUE: {
        UpVal *uv = gco2uv(o);
        if (uvisopen(uv)) 
            markgray(uv);
        else 
            notw2black(uv);
        markvalue(gc, uv->v.p);
        break;
    }
    case CR_VINSTANCE: {
        Instance *ins = gco2ins(o);
        notw2black(ins);
        markobject(gc, ins->oclass);
        break;
    }
    case CR_VMETHOD: {
        IMethod *im = gco2im(o);
        notw2black(im);
        markobject(gc, im->receiver);
        markvalue(gc, &im->method);
        break;
    }
    case CR_VUDATA: {
        UserData *ud = gco2ud(o);
        if (ud->nuv == 0 && !ud->vmt) {
            notw2black(ud);
            break;
        }
    } /* FALLTHRU */
    case CR_VHTABLE: case CR_VFUNCTION: case CR_VCRCL:
    case CR_VCCL: case CR_VCLASS: case CR_VTHREAD: {
        linkobjgclist(o, gc->graylist);
        break;
    }
    default: cr_unreachable();
    }
}


/* mark 'VMT' */
cr_sinline cr_mem markvmt(GC *gc, TValue *vmt) {
    cr_assert(vmt != NULL);
    for (int i = 0; i < CR_NUM_MM; i++)
        if (!ttisnil(&vmt[i]))
            markvalue(gc, &vmt[i]);
    return CR_NUM_MM;
}


/* mark 'HTable' slots */
static cr_mem markhtable(GC *gc, HTable *ht) {
    if (ht->isweak) { /* weak table ? */
        linkgclist(ht, gc->weak);
    } else { /* otherwise strong */
        Node *last = htnodelast(ht);
        for (Node *n = htnode(ht, 0); n < last; n++) {
            if (!keyisempty(n)) {
                markkey(gc, n);
                markvalue(gc, nodeval(n));
            }
        }
    }
    return 1 + (htsize(ht) << 1);
}


/* mark 'Function' */
static cr_mem markfunction(GC *gs, Function *fn) {
    int i;
    markobjectcheck(gs, fn->source);
    for (i = 0; i < fn->sizefn; i++)
        markobject(gs, fn->funcs[i]);
    for (i = 0; i < fn->sizek; i++)
        markvalue(gs, &fn->k[i]);
    for (i = 0; i < fn->sizeprivate; i++)
        markobjectcheck(gs, fn->private[i].s.name);
    for (i = 0; i < fn->sizelocals; i++)
        markobjectcheck(gs, fn->locals[i].name);
    for (i = 0; i < fn->sizeupvals; i++)
        markobjectcheck(gs, fn->upvals[i].name);
    return 1 + fn->sizefn + fn->sizek + fn->sizelocals + fn->sizeupvals;
}


/* mark 'CClosure' */
static cr_mem markcclosure(GC *gc, CClosure *ccl) {
    for (int i = 0; i < ccl->nupvalues; i++)
        markvalue(gc, &ccl->upvals[i]);
    return 1 + ccl->nupvalues;
}


/* mark 'CrClosure' */
static cr_mem markcriptclosure(GC *gc, CrClosure *crcl) {
    markobjectcheck(gc, crcl->fn);
    for (int i = 0; i < crcl->nupvalues; i++)
        markobjectcheck(gc, &crcl->upvals[i]);
    return 1 + crcl->nupvalues;
}


/* mark 'OClass' */
static cr_mem markclass(GC *gc, OClass *cls) {
    markobjectcheck(gc, cls->methods);
    return 1 + (cls->vmt ? markvmt(gc, cls->vmt) : 0);
}


/* mark 'UserData' */
static cr_mem markuserdata(GC *gc, UserData *ud) {
    cr_mem extra = 0;
    if (ud->vmt)
        extra = markvmt(gc, ud->vmt);
    for (int i = 0; i < ud->nuv; i++)
        markobjectcheck(gc, &ud->uv[i]);
    return 1 + ud->nuv + extra;
}


/*
 * Marks thread (per-thread-state).
 * Threads do not use write barriers, because using
 * a write barrier correctly on each thread modification
 * would introduce a lot of complexity.
 * And the way we deal with properly remarking the
 * thread is by linking it into the 'grayagain', a list
 * which is again traversed in 'GCSatomic' state.
 * Marking (traversing) the thread black only occurs in
 * either 'GCSpropagate' or 'GCSatomic' state and between
 * those two states only in 'GCSpropagate' can the objects get modified.
 * So if we are in 'GCSpropagate' we link the object into
 * 'grayagain' and 'GCSatomic' state remarks our thread,
 * restoring the invariant state (in cases where the thread
 * really did get modified after we marked it black) without
 * using write barriers.
 */
static cr_mem markthread(GState *gs, cr_State *ts) {
    GC *gc = &gs->gc;
    cr_assert(gc->state & (GCSpropagate | GCSatomic));
    SPtr sp = ts->stack.p;
    if (gc->state == GCSpropagate)
        linkgclist(ts, gc->grayagain);
    if (sp == NULL) /* thread not fully initialized ? */
        return 1;
    for (; sp < ts->sp.p; sp++)
        markvalue(gc, s2v(sp));
    for (UpVal *uv = ts->openupval; uv; uv = ts->openupval->u.open.next)
        markobject(gc, uv);
    /* 'markopenupvalues' might of removed thread from 'thwouv' list */
    if (gc->state == GCSatomic && !isinthwouv(ts) && ts->openupval != NULL) {
        ts->thwouv = gs->thwouv;
        gs->thwouv = ts;
    }
    if (!gc->isem)
        crT_shrinkstack(ts);
    return 1 + topoffset(ts);
}


/*
 * Remarks open upvalues in 'thwouv'.
 * Basically acts as a barrier for values in already
 * visited open upvalues. It keeps those values alive
 * as long as its upvalue is marked.
 * These upvalues won't get marked if thread is already
 * marked and upvalue itself is not marked (or if
 * thread doesn't contain any open upvalues).
 */
static cr_mem markopenupvalues(GState *gs) {
    int work = 0;
    cr_State *th;
    cr_State **pp = &gs->thwouv;
    while ((th = *pp) != NULL) {
        work++;
        if (iswhite(th) || th->openupval == NULL) {
            *pp = th->thwouv;
            th->thwouv = th;
            for (UpVal *uv = th->openupval; uv; uv = uv->u.open.next) {
                work++;
                /* if visited then keep values alive */
                if (!iswhite(uv)) {
                    cr_assert(uvisopen(uv) && isgray(uv));
                    markvalue(&gs->gc, uv->v.p);
                }
            }
        } else {
            pp = &th->thwouv;
        }
    }
    return work;
}


/* traverse a single gray object turning it black */
static cr_mem propagate(GState *gs) {
    GCObject *o = gs->gc.graylist;
    markblack(o);
    gs->gc.graylist = *getgclist(o);
    switch(o->tt_) {
    case CR_VUDATA: return markuserdata(&gs->gc, gco2ud(o));
    case CR_VHTABLE: return markhtable(&gs->gc, gco2ht(o));
    case CR_VFUNCTION: return markfunction(&gs->gc, gco2fn(o));
    case CR_VCRCL: return markcriptclosure(&gs->gc, gco2crcl(o));
    case CR_VCCL: return markcclosure(&gs->gc, gco2ccl(o));
    case CR_VCLASS: return markclass(&gs->gc, gco2cls(o));
    case CR_VTHREAD: return markthread(gs, gco2th(o));
    default: cr_unreachable(); return 0;
    }
}


/* propagates all gray objects */
static cr_mem propagateall(GState *gs) {
    cr_mem work;
    for (work = 0; gs->gc.graylist; work += propagate(gs));
    return work;
}



/* --------------------------------------------------------------------------
 * Free objects
 * -------------------------------------------------------------------------- */

/* free objectc 'o' */
static void freeobject(cr_State *ts, GCObject *o) {
    switch (o->tt_) {
        case CR_VSTRING: crS_free(ts, gco2str(o)); break;
        case CR_VFUNCTION: crF_free(ts, gco2fn(o)); break;
        case CR_VUVALUE: crF_freeupval(ts, gco2uv(o)); break;
        case CR_VCRCL: crM_free(ts, o, sizeofcrcl(gco2crcl(o)->nupvalues)); break;
        case CR_VCCL: crM_free(ts, o, sizeofccl(gco2ccl(o)->nupvalues)); break;
        case CR_VCLASS: crMM_freeclass(ts, gco2cls(o)); break;
        case CR_VINSTANCE: crMM_freeinstance(ts, gco2ins(o)); break;
        case CR_VMETHOD: crM_free(ts, o, sizeof(*gco2im(o))); break;
        case CR_VTHREAD: crT_free(ts, gco2th(o));
        case CR_VUDATA: crMM_freeuserdata(ts, gco2ud(o)); break;
        default: cr_unreachable();
    }
}



/* ------------------------------------------------------------------------
 * Sweep functions
 * ------------------------------------------------------------------------- */

static GCObject **sweeplist(cr_State *ts, GCObject **l, int nobjects, 
                            int *nsweeped)
{
    cr_assert(nobjects > 0);
    GState *gs = G_(ts);
    int white = crG_white(&gs->gc); /* current white */
    int whitexor = whitexor(&gs->gc);
    int i;
    for (i = 0; i < nobjects && *l; i++) {
        GCObject *o = *l;
        int mark = gcomark_(o);
        if (whitexor & mark) { /* collect ? */
            *l = o->next;
            freeobject(ts, o);
        } else { /* mark white */
            gcomark_(o) = cast_ubyte((mark & ~COLORBITS) | white);
            l = &o->next;
        }
    }
    if (nsweeped) 
        *nsweeped = i;
    return (*l ? l : NULL);
}


/* do a single sweep step limited by 'GCSWEEPMAX' */
static int sweepstep(cr_State *ts, GCObject **nextlist, int nextstate) {
    GC *gc = &G_(ts)->gc;
    if (gc->sweeppos) {
        int cnt;
        gc->sweeppos = sweeplist(ts, gc->sweeppos, GCSWEEPMAX, &cnt);
        gc->estimate += gc->debt;
        return cnt;
    } else {
        gc->sweeppos = nextlist;
        gc->state = nextstate;
        return 0;
    }
}


/*
 * Sweep objects in 'list' until alive (marked) object
 * or the end of the list.
 */
static GCObject **sweepuntilalive(cr_State *ts, GCObject **list) {
    GCObject **pp = list;
    do {
        list = sweeplist(ts, list, 1, NULL);
    } while (pp == list);
    return list;
}


static void entersweep(cr_State *ts) {
    GC *gc = &G_(ts)->gc;
    gc->state = GCSsweepall;
    cr_assert(gc->sweeppos == NULL);
    gc->sweeppos = sweepuntilalive(ts, &gc->objects);
}



/* -------------------------------------------------------------------------
 * Finalization (__gc__)
 * ------------------------------------------------------------------------- */

/*
 * Get object from 'tobefin' list and link it back
 * to the 'objects' list.
 */
static GCObject *gettobefin(GC *gc) {
    GCObject *o = gc->tobefin;
    cr_assert(o && isfin(o));
    gc->tobefin = o->next;
    o->next = gc->objects;
    gc->objects = o;
    if (sweepstate(gc))
        markwhite(gc, o);
    return o;
}


/* protected finalizer */
static void protectedfinalizer(cr_State *ts, void *userdata) {
    UNUSED(userdata);
    crV_call(ts, ts->sp.p - 2, 0);
}


/* call a finalizer */
static void callfin(cr_State *ts) {
    TValue v;
    const TValue *m;
    GC *gc = &G_(ts)->gc;
    setgcoval(ts, &v, gettobefin(gc));
    if (!ttisnil(m = crMM_get(ts, &v, CR_MM_GC))) { /* have __gc ? */
        int oldstopped = gc->stopped;
        gc->stopped = GCSTP; /* prevent recursive GC calls */
        setobj2s(ts, ts->sp.p++, m);
        setobj2s(ts, ts->sp.p++, &v);
        ptrdiff_t oldtop = savestack(ts, ts->sp.p - 2);
        ts->cf->status |= CFST_FIN; /* running a finalizer */
        int status = crPR_call(ts, protectedfinalizer, NULL, oldtop);
        ts->cf->status &= ~CFST_FIN; /* finalizer returned */
        gc->stopped = oldstopped;
        if (cr_unlikely(status != CR_OK)) {
            crD_warnerror(ts, "__gc__");
            ts->sp.p--; /* pop err object */
        }
    }
}


/* call objects with finalizer in 'tobefin' */
static int callNfinalizers(cr_State *ts, int n) {
    GC *gc = &G_(ts)->gc;
    int i;
    for (i = 0; i < n && gc->tobefin; i++)
        callfin(ts);
    return i;
}


/*
 * Check if object has a finalizer and move it into 'fin'
 * list but only if it wasn't moved already indicated by
 * 'FINBIT' being set, additionally don't move it in case
 * state is closing.
 */
void crG_checkfin(cr_State *ts, GCObject *o, TValue *vmt) {
    GCObject **pp;
    GC *gc = &G_(ts)->gc;
    if (isfin(o) || ttisnil(&vmt[CR_MM_GC]) || (gc->stopped & GCSTPCLS))
        return;
    if (sweepstate(gc)) {
        markwhite(gc, o);
        if (gc->sweeppos == &o->next)
            gc->sweeppos = sweepuntilalive(ts, gc->sweeppos);
    }
    for (pp = &gc->objects; *pp != o; pp = &(*pp)->next);
    *pp = o->next;
    o->next = gc->fin;
    gc->fin = o;
    setbit(o->mark, FINBIT);
}



/* -------------------------------------------------------------------------
 * GC control
 * ------------------------------------------------------------------------- */


/* clear all unmarked keys in global strings table */
static void refclear(GState *gs) {
    HTable *ht = gs->strings;
    Node *limit = htnodelast(ht);
    for (Node *n = htnode(ht, 0); n < limit; n++) {
        if (!ttisempty(nodeval(n))) { /* (key: string, value: string) */
            setemptykey(n);
            setemptyval(nodeval(n));
        }
    }
}


/*
 * Get the last 'next' object in list 'l'
 * Useful used when trying to link objects
 * at the end of the list.
 */
cr_sinline GCObject **getlastnext(GCObject **l) {
    while (*l)
        l = &(*l)->next;
    return l;
}


/*
 * Separate all unreachable objects with a finalizer
 * in 'fin' list into the 'tobefin' list.
 * In case 'force' is true then every object in the
 * 'fin' list will moved.
 */
static void separatetobefin(GState *gs, int force) {
    GC *gc = &gs->gc;
    GCObject **finlp = &gc->fin;
    GCObject **lastnext = getlastnext(&gc->tobefin);
    for (GCObject *curr = *finlp; curr != NULL; curr = *finlp) {
        if (!(iswhite(curr) || force)) { /* marked and force is false ? */
            finlp = &curr->next;
        } else { /* unreachable, move it into 'tobefin' */
            *finlp = curr->next; /* unlink 'curr' from 'fin' */
            curr->next = *lastnext;
            *lastnext = curr; /* link 'curr' into 'tobefin' */
            lastnext = &curr->next; /* advance 'lastnext' */
        }
    }
}


static cr_mem marktobefin(GState *gs) {
    cr_mem cnt = 0;
    for (GCObject *o = gs->gc.tobefin; o != NULL; o = o->next) {
        markobject(&gs->gc, o);
        cnt++;
    }
    return cnt;
}


static cr_mem atomic(cr_State *ts) {
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    cr_mem work = 0;
    GCObject *grayagain = gc->grayagain;
    gc->grayagain = NULL;
    cr_assert(gc->weak == NULL);
    gc->state = GCSatomic;
    markobject(gc, ts); /* mark running thread */
    markvalue(gc, &gs->globals); /* mark global variables table */
    work += propagateall(gs);
    /* mark open upvalues */
    work += markopenupvalues(gs);
    work += propagateall(gs);
    cr_assert(gc->graylist == NULL);
    gc->graylist = grayagain;
    work += propagateall(gs);
    /* all accessible objects are marked,
     * safely clear weak tables */
    refclear(gs);
    /* Note: as of version 1.0.0 'gc->weak' is equal
     * to strings table because weak tables are not
     * accessible via core API. */
    cr_assert(gc->weak == obj2gco(gs->strings));
    /* separate and 'resurrect' unreachable
     * objects with the finalizer */
    separatetobefin(gs, 0);
    work += marktobefin(gs);
    work += propagateall(gs);
    /* all 'resurrected' objects are marked,
     * so clear weak tables safely again */
    refclear(gs);
    gc->whitebit = whitexor(gc); /* flip white bit */
    cr_assert(gc->graylist == NULL); /* all must be propagated */
    return work;
}


/* 
** Set collector pause; called after end of each full GC cycle.
** The new threshold is calculated as 'estimate' / 'pause'.
** 'PAUSEADJ' is there to provide more precise control over
** when collection occurs (the value is chosen by testing from
** the side of Lua developers). 
** One could think of 'gc->pause' to be the percentage as
** it is divided by 'PAUSEADJ' which is 100.
*/
static void setpause(GC *gc) {
    int pause = getgcparam(gc->pause);
    cr_mem estimate = gc->estimate / PAUSEADJ;
    cr_assert(estimate > 0); /* CScript state memory >= PAUSEADJ */
    cr_mem threshold = (pause < (CRMEM_MAX / estimate)) /* can fit ? */
                        ? (estimate * pause) /* yes */
                        : CRMEM_MAX; /* no; use maximum */
    /* debt = totalbytes - ((estimate/100)*pause) */
    cr_mem debt = totalbytes(gc) - threshold;
    if (debt > 0) debt = 0;
    crG_setdebt(gc, debt);
}


/* restart GC, mark roots and leftover 'tobefin' objects */
static void restartgc(GState *gs) {
    GC *gc = &gs->gc;
    gc->graylist = gc->grayagain = gc->weak = NULL;
    markobject(gc, gs->mainthread);
    markvalue(gc, &gs->globals);
    markopenupvalues(gs);
    /* there could be leftover unreachable objects
     * with a finalizer from the previous cycle in
     * case of emergency collection, so mark them */
    marktobefin(gs);
}


/*
 * Garbage collector state machine.
 * GCSpause marks all the roots.
 * GCSpropagate propagates gray objects into black
 * or links them into 'grayagain' for atomic phase.
 * GCSenteratomic enters the atomic state and
 * marks main thread, globals, etc... and propagates
 * all of them. Finally it clears the strings table
 * (dead weak references) and changes white bit.
 * GCSsweepall sweeps all the objects in 'objects'.
 * GCSsweepfin sweeps all the objects in 'fin'.
 * GCSsweeptofin sweeps all the objects in 'tobefin'.
 * GCSsweepend (as of this version) does nothing
 * but provide clarity that sweep phase is over.
 * GCScallfin calls finalizers of all the objects
 * in 'tobefin' and puts them back into 'objects'
 * list after the call.
 */
static cr_mem singlestep(cr_State *ts) {
    cr_mem work;
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    gc->stopem = 1; /* prevent emergency collections */
    switch (gc->state) {
        case GCSpause: { /* mark roots */
            restartgc(gs);
            gc->state = GCSpropagate;
            work = 1;
            break;
        }
        case GCSpropagate: { /* gray -> black */
            if (gc->graylist) {
                work = propagate(gs);
            } else {
                gc->state = GCSenteratomic;
                work = 0;
            }
            break;
        }
        case GCSenteratomic: { /* remark */
            work = atomic(ts);
            entersweep(ts);
            break;
        }
        case GCSsweepall: {
            work = sweepstep(ts, &gc->fin, GCSsweepfin);
            break;
        }
        case GCSsweepfin: {
            work = sweepstep(ts, &gc->tobefin, GCSsweeptofin);
            break;
        }
        case GCSsweeptofin: {
            work = sweepstep(ts, NULL, GCSsweepend);
            break;
        }
        case GCSsweepend: {
            /* state not used for anything but clarity */
            gc->state = GCScallfin;
            work = 0;
            break;
        }
        case GCScallfin: { /* call finalizers */
            if (gc->tobefin && !gc->isem) {
                gc->stopem = 0; /* can collect in finalizer */
                work = callNfinalizers(ts, GCFINMAX) * GCFINCOST;
            } else {
                gc->state = GCSpause;
                work = 0;
            }
            break;
        }
        default: cr_unreachable();
    }
    gc->stopem = 0;
    return work;
}


/* free list 'l' objects until 'limit' */
cr_sinline void freelist(cr_State *ts, GCObject *l, GCObject *limit) {
    while (l != limit) {
        GCObject *next = l->next;
        freeobject(ts, l);
        l = next;
    }
}


static void runallfinalizers(cr_State *ts) {
    GC *gc = &G_(ts)->gc;
    while (gc->tobefin)
        callfin(ts);
}


/*
 * Free all objects except main thread, additionally
 * call all finalizers.
 */
void crG_freeallobjects(cr_State *ts) {
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    gc->stopped = GCSTPCLS; /* paused by state closing */
    separatetobefin(gs, 1);
    cr_assert(gc->fin == NULL);
    runallfinalizers(ts);
    freelist(ts, gc->objects, obj2gco(gs->mainthread));
    cr_assert(gc->fin == NULL);
    freelist(ts, gc->fixed, NULL);
}


/* run GC steps until 'state' is in any of the states of 'statemask' */
void crG_rununtilstate(cr_State *ts, int statemask) {
    GC *gc = &G_(ts)->gc;
    while (!testbits(gc->state, statemask))
        singlestep(ts);
}


/*
 * Run collector until debt is less than a stepsize
 * or the full cycle was done (GC state is GCSpause).
 * Both the debt and stepsize are converted to 'work',
 */
static void step(cr_State *ts, GState *gs) {
    GC *gc = &gs->gc;
    int stepmul = getgcparam(gc->stepmul) | 1;
    cr_mem debt = (gc->debt / WORK2MEM) * stepmul;
    cr_mem stepsize = (gc->stepsize <= sizeof(cr_mem) * 8 - 2 /* fits ? */
            ? (cast_mem(1) << gc->stepsize) / WORK2MEM /* yes */
            : CRMEM_MAX); /* no; return maximum possible value */
    do {
        debt -= singlestep(ts);
    } while (debt > -stepsize && gc->state != GCSpause);
    if (gc->state == GCSpause) {
        setpause(gc);
    } else {
        debt = (debt / stepmul) * WORK2MEM; /* convert back to bytes */
        crG_setdebt(gc, debt);
    }
}


void crG_step(cr_State *ts) {
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    if (!gcrunning(gc)) /* stopped ? */
        crG_setdebt(gc, -2000);
    else
        step(ts, gs);
}


static void fullcycle(cr_State *ts) {
    GC *gc = &G_(ts)->gc;
    if (invariantstate(gc)) /* already have black objects ? */
        entersweep(ts); /* if so sweep them first */
    crG_rununtilstate(ts, bitmask(GCSpause)); /* restart collector */
    crG_rununtilstate(ts, bitmask(GCScallfin)); /* run until finalizers */
    cr_assert(gc->estimate == totalbytes(gc)); /* end of cycle, check estimate */
    crG_rununtilstate(ts, bitmask(GCSpause)); /* finish collection */
    setpause(gc);
}


void crG_full(cr_State *ts, int isemergency) {
    GC *gc = &G_(ts)->gc;
    cr_assert(!gc->isem);
    gc->isem = isemergency;
    fullcycle(ts);
    gc->isem = 0;
}
