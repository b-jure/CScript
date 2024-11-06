/*
** cgc.c
** Garbage Collector
** See Copyright Notice in cscript.h
*/

#include "cgc.h"
#include "carray.h"
#include "cconf.h"
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
    (gcomark_(o) = (gcomark_(o) & ~maskcolorbits) | csG_white(gc))

/* mark object as black */
#define markblack(o) \
    (gcomark_(o) = (gcomark_(o) & ~maskwhitebits) | bitmask(BLACKBIT))

/* mark object as gray */
#define markgray(o)	resetbits(gcomark_(o), maskcolorbits)



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



void csG_init(GC *gc, cs_State *ts, size_t LGsize) {
    gc->next = 0;
    gc->allocated = 0;
    gc->debt = 0;
    gc->total = LGsize;
    /* 'estimate' set on each cycle */
    gc->objects = obj2gco(ts); /* mainthread */
    gc->sweeppos = NULL;
    gc->graylist = gc->grayagain = gc->weak = NULL;
    gc->fixed = gc->fin = gc->tobefin = NULL;
    setgcparam(gc->pause, CSI_GCPAUSE);
    setgcparam(gc->stepmul, CSI_GCSTEPMUL);
    gc->stepsize = CSI_GCSTEPSIZE;
    gc->state = GCSpause;
    gc->stopem = 0;
    gc->stopped = GCSTP; /* stop while initializing */
    gc->whitebit = bitmask(WHITEBIT0);
    gc->isem = 0;
    gc->stopem = 0;
}


/* create new object and append it to 'objects' */
GCObject *csG_newoff(cs_State *ts, size_t sz, int tt_, size_t offset) {
    GC *gc = &G_(ts)->gc;
    char *p = cast_charp(csM_malloc(ts, sz));
    GCObject *o = cast(GCObject*, p + offset);
    o->mark = csG_white(gc);
    o->tt_ = tt_;
    o->next = gc->objects;
    gc->objects = o;
    return o;
}


GCObject *csG_new_(cs_State *ts, size_t size, int tt_) {
    return csG_newoff(ts, size, tt_, 0);
}


void csG_fix(cs_State *ts, GCObject *o) {
    GC *gc = &G_(ts)->gc;
    cs_assert(o == gc->objects); /* first in the list */
    markgray(o);
    gc->objects = o->next;
    o->next = gc->fixed;
    gc->fixed = o;
}


/* set collector debt */
void csG_setdebt(GC *gc, cs_mem debt) {
    cs_mem total = totalbytes(gc);
    if (debt < total - CRMEM_MAX) /* 'total' will underflow ? */
        debt = total - CRMEM_MAX;
    gc->total = total - debt;
    gc->debt = debt;
}


static void linkgclist_(GCObject *o, GCObject **gclist, GCObject **list) {
    cs_assert(!isgray(o));
    *gclist = *list;
    *list = o;
    markgray(o);
}


static GCObject **getgclist(GCObject *o) {
    switch (o->tt_) {
    case CS_VFUNCTION: return &gco2fn(o)->gclist;
    case CS_VCRCL: return &gco2crcl(o)->gclist;
    case CS_VCCL: return &gco2ccl(o)->gclist;
    case CS_VCLASS: return &gco2cls(o)->gclist;
    case CS_VARRAY: return &gco2arr(o)->gclist;
    case CS_VHTABLE: return &gco2ht(o)->gclist;
    case CS_VTHREAD: return &gco2th(o)->gclist;
    case CS_VUDATA: {
         UserData *ud = gco2ud(o);
         UNUSED(ud);
         cs_assert(ud->nuv > 0 || ud->vmt);
         return &gco2ud(o)->gclist;
    }
    default: cs_unreachable();
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
void csG_barrier_(cs_State *ts, GCObject *r, GCObject *o) {
    GC *gc = &G_(ts)->gc;
    if (invariantstate(gc)) { /* invariant holds ? */
        cs_assert(isblack(r) && iswhite(o));
        markobject_(gc, o);
    } else { /* in sweep phase */
        cs_assert(sweepstate(gc));
        markwhite(gc, r);
    }
}


/*
 * Write barrier that marks the black object 'r' that is
 * pointing to a white object gray again, effectively
 * moving the collector backwards.
 */
void csG_barrierback_(cs_State *ts, GCObject *r) {
    GC *gc = &G_(ts)->gc;
    cs_assert(isblack(r));
    linkobjgclist(r, gc->grayagain);
}



/* ------------------------------------------------------------------------
 * Mark functions
 * ------------------------------------------------------------------------- */

/*
** Marks white object 'o'.
** Some objects are directly marked as black, these
** objects do not point to other objects, or their references
** can be resolved by up to a single recursive call to this function.
** Other objects are marked gray, more precisely they are
** first moved into 'gray' list and then marked as gray.
** The 'gclist' pointer is the way we link them into graylist, while
** preserving their link in the 'objects'.
*/
static void markobject_(GC *gc, GCObject *o) {
    cs_assert(iswhite(o));
    switch (o->tt_) {
        case CS_VSTRING: {
            markblack(o);
            break;
        }
        case CS_VUVALUE: {
            UpVal *uv = gco2uv(o);
            if (uvisopen(uv)) 
                markgray(uv);
            else 
                markblack(uv);
            markvalue(gc, uv->v.p);
            break;
        }
        case CS_VMETHOD: {
            IMethod *im = gco2im(o);
            markblack(im);
            markobject(gc, im->receiver);
            markvalue(gc, &im->method);
            break;
        }
        case CS_VARRAY: {
            Array *arr = gco2arr(o);
            if (arr->n == 0) { /* no elements? */
                markblack(arr);
                break;
            }
            goto linklist;
        }
        case CS_VUDATA: {
            UserData *ud = gco2ud(o);
            if (ud->nuv == 0 && !ud->vmt) { /* no user values and VMT? */
                markblack(ud);
                break;
            }
        } /* FALLTHRU */
    linklist:
        case CS_VHTABLE: case CS_VFUNCTION: case CS_VCRCL: case CS_VCCL:
        case CS_VCLASS: case CS_VTHREAD: case CS_VINSTANCE: {
            linkobjgclist(o, gc->graylist);
            break;
        }
        default: {
            cs_unreachable();
            cs_assert(0);
        }
    }
}


/* mark 'VMT' */
cs_sinline cs_mem markvmt(GC *gc, TValue *vmt) {
    cs_assert(vmt != NULL);
    for (int i = 0; i < CS_NUM_MM; i++)
        markvalue(gc, &vmt[i]);
    return CS_NUM_MM; /* size of VMT array */
}


/* mark 'HTable' slots */
static cs_mem markhtable(GC *gc, HTable *ht) {
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
    return 1 + (htsize(ht) * 2); /* key/value pairs + table itself */
}


/* mark 'Function' */
static cs_mem markfunction(GC *gs, Function *fn) {
    int i;
    markobjectcheck(gs, fn->source);
    for (i = 0; i < fn->sizefn; i++)
        markobject(gs, fn->funcs[i]);
    for (i = 0; i < fn->sizek; i++)
        markvalue(gs, &fn->k[i]);
    for (i = 0; i < fn->sizeprivate; i++) {
        markobjectcheck(gs, fn->private[i].s.name);
        markvalue(gs, &fn->private[i].val);
    }
    for (i = 0; i < fn->sizelocals; i++)
        markobjectcheck(gs, fn->locals[i].name);
    for (i = 0; i < fn->sizeupvals; i++)
        markobjectcheck(gs, fn->upvals[i].name);
    /* fn + funcs + constants + locals + upvalues + privates */
    return 1 + fn->sizefn + fn->sizek + fn->sizelocals + fn->sizeupvals +
               (fn->sizeprivate * 2);
}


/* mark C closure */
static cs_mem markcclosure(GC *gc, CClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        TValue *uv = &cl->upvals[i];
        markvalue(gc, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


/* mark CScript closure */
static cs_mem markcstclosure(GC *gc, CrClosure *cl) {
    markobjectcheck(gc, cl->fn);
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = cl->upvals[i];
        markobjectcheck(gc, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


/* mark 'OClass' */
static cs_mem markclass(GC *gc, OClass *cls) {
    markobjectcheck(gc, cls->methods);
    return 1 + (cls->vmt ? markvmt(gc, cls->vmt) : 0); /* class + VMT */
}


/* mark 'UserData' */
static cs_mem markuserdata(GC *gc, UserData *ud) {
    cs_mem extra = 0;
    if (ud->vmt) /* have VMT? */
        extra = markvmt(gc, ud->vmt);
    for (int i = 0; i < ud->nuv; i++)
        markobjectcheck(gc, &ud->uv[i]);
    return 1 + ud->nuv + extra; /* user values + extra (VMT) + ud */
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
static cs_mem markthread(GState *gs, cs_State *ts) {
    GC *gc = &gs->gc;
    SPtr sp = ts->stack.p;
    cs_assert(gc->state & (GCSpropagate | GCSatomic));
    if (gc->state == GCSpropagate)
        linkgclist(ts, gc->grayagain);
    if (sp == NULL) /* stack not fully built? */
        return 1;
    /* either in atomic, or no open upvalues or 'ts' is in correct list */
    cs_assert(gc->state==GCSatomic || ts->openupval==NULL || isinthwouv(ts));
    for (; sp < ts->sp.p; sp++)
        markvalue(gc, s2v(sp));
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        markobject(gc, uv);
    if (gc->state == GCSatomic) { /* final traversal? */
        if (!gc->isem) /* not an emergency collection? */
            csT_shrinkstack(ts); /* shrink stack if possible */
        for (sp = ts->sp.p; sp < ts->stackend.p + EXTRA_STACK; sp++)
          setnilval(s2v(sp)); /* clear dead stack slice */
        /* 'markopenupvalues' might of removed thread from 'thwouv' list */
        if (gc->state==GCSatomic && !isinthwouv(ts) && ts->openupval!=NULL) {
            ts->thwouv = gs->thwouv; /* link it back */
            gs->thwouv = ts;
        }
    }
    return 1 + stacksize(ts); /* thread + stack size */
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
static cs_mem markopenupvalues(GState *gs) {
    int work = 0;
    cs_State *th;
    cs_State **pp = &gs->thwouv;
    while ((th = *pp) != NULL) {
        work++;
        if (iswhite(th) || th->openupval == NULL) {
            *pp = th->thwouv;
            th->thwouv = th;
            for (UpVal *uv = th->openupval; uv; uv = uv->u.open.next) {
                work++;
                /* if visited then keep values alive */
                if (!iswhite(uv)) {
                    cs_assert(uvisopen(uv) && isgray(uv));
                    markvalue(&gs->gc, uv->v.p);
                }
            }
        } else {
            pp = &th->thwouv;
        }
    }
    return work;
}


static cs_mem markinstance(GC *gc, Instance *ins) {
    markobjectcheck(gc, ins->oclass);
    return markhtable(gc, &ins->fields); /* instance + fields */
}


static cs_mem markarray(GC *gc, Array *arr) {
    cs_assert(arr->n > 0);
    for (uint i = 0; i < arr->n; i++)
        markvalue(gc, &arr->b[i]);
    return 1 + arr->n; /* array + elements */
}


/* traverse a single gray object turning it black */
static cs_mem propagate(GState *gs) {
    GCObject *o = gs->gc.graylist;
    cs_assert(!iswhite(o)); /* 'o' must be gray */
    notw2black(o);
    gs->gc.graylist = *getgclist(o);
    switch(o->tt_) {
    case CS_VUDATA: return markuserdata(&gs->gc, gco2ud(o));
    case CS_VHTABLE: return markhtable(&gs->gc, gco2ht(o));
    case CS_VFUNCTION: return markfunction(&gs->gc, gco2fn(o));
    case CS_VCRCL: return markcstclosure(&gs->gc, gco2crcl(o));
    case CS_VCCL: return markcclosure(&gs->gc, gco2ccl(o));
    case CS_VCLASS: return markclass(&gs->gc, gco2cls(o));
    case CS_VINSTANCE: return markinstance(&gs->gc, gco2ins(o));
    case CS_VARRAY: return markarray(&gs->gc, gco2arr(o));
    case CS_VTHREAD: return markthread(gs, gco2th(o));
    default: cs_unreachable(); cs_assert(0); return 0;
    }
}


/* propagates all gray objects */
static cs_mem propagateall(GState *gs) {
    cs_mem work;
    for (work = 0; gs->gc.graylist; work += propagate(gs));
    return work;
}



/* --------------------------------------------------------------------------
 * Free objects
 * -------------------------------------------------------------------------- */

/* free objectc 'o' */
static void freeobject(cs_State *ts, GCObject *o) {
    switch (o->tt_) {
        case CS_VSTRING: csS_free(ts, gco2str(o)); break;
        case CS_VFUNCTION: csF_free(ts, gco2fn(o)); break;
        case CS_VUVALUE: csF_freeupval(ts, gco2uv(o)); break;
        case CS_VCRCL: csM_free(ts, o, sizeofcrcl(gco2crcl(o)->nupvalues)); break;
        case CS_VCCL: csM_free(ts, o, sizeofccl(gco2ccl(o)->nupvalues)); break;
        case CS_VCLASS: csMM_freeclass(ts, gco2cls(o)); break;
        case CS_VARRAY: csA_free(ts, gco2arr(o)); break;
        case CS_VINSTANCE: csMM_freeinstance(ts, gco2ins(o)); break;
        case CS_VMETHOD: csM_free(ts, o, sizeof(*gco2im(o))); break;
        case CS_VTHREAD: csT_free(ts, gco2th(o));
        case CS_VUDATA: csMM_freeuserdata(ts, gco2ud(o)); break;
        default: cs_unreachable();
    }
}



/* ------------------------------------------------------------------------
 * Sweep functions
 * ------------------------------------------------------------------------- */

static GCObject **sweeplist(cs_State *ts, GCObject **l, int nobjects, 
                            int *nsweeped)
{
    GState *gs = G_(ts);
    int white = csG_white(&gs->gc); /* current white */
    int whitexor = whitexor(&gs->gc);
    int i;
    cs_assert(nobjects > 0);
    for (i = 0; i < nobjects && *l; i++) {
        GCObject *o = *l;
        int mark = gcomark_(o);
        if (whitexor & mark) { /* collect ? */
            *l = o->next;
            freeobject(ts, o);
        } else { /* mark white */
            gcomark_(o) = cast_ubyte((mark & ~maskcolorbits) | white);
            l = &o->next;
        }
    }
    if (nsweeped) 
        *nsweeped = i;
    return (*l ? l : NULL);
}


/* do a single sweep step limited by 'GCSWEEPMAX' */
static int sweepstep(cs_State *ts, GCObject **nextlist, int nextstate) {
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
static GCObject **sweepuntilalive(cs_State *ts, GCObject **list) {
    GCObject **pp = list;
    do {
        list = sweeplist(ts, list, 1, NULL);
    } while (pp == list);
    return list;
}


static void entersweep(cs_State *ts) {
    GC *gc = &G_(ts)->gc;
    gc->state = GCSsweepall;
    cs_assert(gc->sweeppos == NULL);
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
    cs_assert(o && isfin(o));
    gc->tobefin = o->next;
    o->next = gc->objects;
    gc->objects = o;
    if (sweepstate(gc))
        markwhite(gc, o);
    return o;
}


/* protected finalizer */
static void protectedfinalizer(cs_State *ts, void *userdata) {
    UNUSED(userdata);
    csV_call(ts, ts->sp.p - 2, 0);
}


/* call a finalizer "__gc" */
static void callfin(cs_State *ts) {
    TValue v;
    const TValue *m;
    GC *gc = &G_(ts)->gc;
    setgcoval(ts, &v, gettobefin(gc));
    if (ttisnil(m = csMM_get(ts, &v, CS_MM_GC))) /* __gc is nil ? */
        return; /* done; no finalizer */
    int oldstopped = gc->stopped;
    gc->stopped = GCSTP; /* prevent recursive GC calls */
    setobj2s(ts, ts->sp.p++, m);
    setobj2s(ts, ts->sp.p++, &v);
    ptrdiff_t oldtop = savestack(ts, ts->sp.p - 2);
    ts->cf->status |= CFST_FIN; /* running a finalizer */
    int status = csPRcall(ts, protectedfinalizer, NULL, oldtop, ts->errfunc);
    ts->cf->status &= ~CFST_FIN; /* finalizer returned */
    gc->stopped = oldstopped;
    if (cs_unlikely(status != CS_OK)) {
        csT_warnerror(ts, "__gc");
        ts->sp.p--; /* pop err object */
    }
}


/* call objects with finalizer in 'tobefin' */
static int callNfinalizers(cs_State *ts, int n) {
    int i;
    GC *gc = &G_(ts)->gc;
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
void csG_checkfin(cs_State *ts, GCObject *o, TValue *vmt) {
    GCObject **pp;
    GC *gc = &G_(ts)->gc;
    if (isfin(o) || ttisnil(&vmt[CS_MM_GC]) || (gc->stopped & GCSTPCLS))
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
cs_sinline GCObject **getlastnext(GCObject **l) {
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


static cs_mem marktobefin(GState *gs) {
    cs_mem cnt = 0;
    for (GCObject *o = gs->gc.tobefin; o != NULL; o = o->next) {
        markobject(&gs->gc, o);
        cnt++;
    }
    return cnt;
}


static void markvmts(GState *gs) {
    for (int i = 0; i < CS_NUM_TYPES; i++)
        if (gs->vmt[i])
            markvalue(&gs->gc, gs->vmt[i]);
}


static cs_mem atomic(cs_State *ts) {
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    cs_mem work = 0;
    GCObject *grayagain = gc->grayagain;
    gc->grayagain = NULL;
    cs_assert(gc->weak == NULL);
    cs_assert(!iswhite(gs->mainthread));
    gc->state = GCSatomic;
    markobject(gc, ts); /* mark running thread */
    markvalue(gc, &gs->ginstance); /* mark global instance */
    markvmts(gs); /* mark global VMTs */
    work += propagateall(gs); /* traverse all gray objects */
    work += markopenupvalues(gs); /* mark open upvalues */
    work += propagateall(gs); /* propagate changes */
    cs_assert(gc->graylist = NULL); /* all must be propagated */
    gc->graylist = grayagain; /* set 'grayagain' as the graylist */
    work += propagateall(gs); /* propagate gray objects from 'grayagain' */
    refclear(gs); /* all accessible objects are marked, clear weak tables */
    /* Note: as of version 1.0.0 'gc->weak' is equal to strings table
     * because weak tables are not accessible via CScript API. */
    cs_assert(gc->weak == obj2gco(gs->strings));
    /* separate and 'resurrect' unreachable objects with the finalizer */
    separatetobefin(gs, 0);
    work += marktobefin(gs); /* and mark them */
    work += propagateall(gs); /* propagate changes */
    refclear(gs); /* all 'resurrected' objects are marked, clear weak tables */
    gc->whitebit = whitexor(gc); /* flip white bit */
    cs_assert(gc->graylist == NULL); /* all must be propagated */
    return work; /* estimated number of marked slots during 'atomic' */
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
    cs_mem estimate = gc->estimate / PAUSEADJ;
    cs_assert(estimate > 0); /* CScript state memory >= PAUSEADJ */
    cs_mem threshold = (pause < (CRMEM_MAX / estimate)) /* can fit ? */
                        ? (estimate * pause) /* yes */
                        : CRMEM_MAX; /* no; use maximum */
    /* debt = totalbytes - ((estimate/100)*pause) */
    cs_mem debt = totalbytes(gc) - threshold;
    if (debt > 0) debt = 0;
    csG_setdebt(gc, debt);
}


/* restart GC, mark roots and leftover 'tobefin' objects */
static void restartgc(GState *gs) {
    GC *gc = &gs->gc;
    gc->graylist = gc->grayagain = gc->weak = NULL;
    markobject(gc, gs->mainthread);
    markvalue(gc, &gs->ginstance);
    markvmts(gs);
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
static cs_mem singlestep(cs_State *ts) {
    cs_mem work;
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    gc->stopem = 1; /* prevent emergency collections */
    switch (gc->state) {
        case GCSpause: { /* mark roots */
            restartgc(gs);
            gc->state = GCSpropagate;
            work = 1; /* mainthread */
            break;
        }
        case GCSpropagate: { /* gray -> black */
            if (gc->graylist) {
                work = propagate(gs);
            } else { /* no more gray objects */
                gc->state = GCSenteratomic;
                work = 0;
            }
            break;
        }
        case GCSenteratomic: { /* remark */
            work = atomic(ts);
            entersweep(ts);
            gc->estimate = totalbytes(gc);
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
        default: {
            cs_unreachable();
            cs_assert(0);
            return 0;
        }
    }
    gc->stopem = 0;
    return work;
}


/* free list 'l' objects until 'limit' */
cs_sinline void freelist(cs_State *ts, GCObject *l, GCObject *limit) {
    while (l != limit) {
        GCObject *next = l->next;
        freeobject(ts, l);
        l = next;
    }
}


static void runallfinalizers(cs_State *ts) {
    GC *gc = &G_(ts)->gc;
    while (gc->tobefin)
        callfin(ts);
}


/*
 * Free all objects except main thread, additionally
 * call all finalizers.
 */
void csG_freeallobjects(cs_State *ts) {
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    gc->stopped = GCSTPCLS; /* paused by state closing */
    separatetobefin(gs, 1);
    cs_assert(gc->fin == NULL);
    runallfinalizers(ts);
    freelist(ts, gc->objects, obj2gco(gs->mainthread));
    cs_assert(gc->fin == NULL);
    freelist(ts, gc->fixed, NULL);
}


/* run GC steps until 'state' is in any of the states of 'statemask' */
void csG_rununtilstate(cs_State *ts, int statemask) {
    GC *gc = &G_(ts)->gc;
    while (!testbits(gc->state, statemask))
        singlestep(ts);
}


/*
 * Run collector until debt is less than a stepsize
 * or the full cycle was done (GC state is GCSpause).
 * Both the debt and stepsize are converted to 'work',
 */
static void step(cs_State *ts, GState *gs) {
    GC *gc = &gs->gc;
    int stepmul = getgcparam(gc->stepmul) | 1;
    cs_mem debt = (gc->debt / WORK2MEM) * stepmul;
    cs_mem stepsize = (gc->stepsize <= sizeof(cs_mem) * 8 - 2 /* fits ? */
            ? (cast_mem(1) << gc->stepsize) / WORK2MEM /* yes */
            : CRMEM_MAX); /* no; return maximum possible value */
    do {
        debt -= singlestep(ts);
    } while (debt > -stepsize && gc->state != GCSpause);
    if (gc->state == GCSpause) {
        setpause(gc);
    } else {
        debt = (debt / stepmul) * WORK2MEM; /* convert back to bytes */
        csG_setdebt(gc, debt);
    }
}


void csG_step(cs_State *ts) {
    GState *gs = G_(ts);
    GC *gc = &gs->gc;
    if (!gcrunning(gc)) /* stopped ? */
        csG_setdebt(gc, -2000);
    else
        step(ts, gs);
}


static void fullcycle(cs_State *ts) {
    GC *gc = &G_(ts)->gc;
    if (invariantstate(gc)) /* already have black objects ? */
        entersweep(ts); /* if so sweep them first */
    csG_rununtilstate(ts, bitmask(GCSpause)); /* restart collector */
    csG_rununtilstate(ts, bitmask(GCScallfin)); /* run until finalizers */
    cs_assert(gc->estimate == totalbytes(gc)); /* end of cycle, check estimate */
    csG_rununtilstate(ts, bitmask(GCSpause)); /* finish collection */
    setpause(gc);
}


void csG_full(cs_State *ts, int isemergency) {
    GC *gc = &G_(ts)->gc;
    cs_assert(!gc->isem);
    gc->isem = isemergency;
    fullcycle(ts);
    gc->isem = 0;
}


/* traverse a list making all its elements white */
static void whitelist (GC *gc, GCObject *l) {
    int white = csG_white(gc);
    for (; l != NULL; l = l->next)
        l->mark = cast_byte((l->mark & ~maskgcbits) | white);
}


/*
** Enter incremental mode. Turn all objects white, make all
** intermediate lists point to NULL (to avoid invalid pointers),
** and go to the pause state.
*/
static void enterinc (GC *gc) {
    whitelist(gc, gc->objects);
    whitelist(gc, gc->fin);
    whitelist(gc, gc->tobefin);
    gc->state = GCSpause;
}


/* enter incremental mode */
void csG_incmode(cs_State *ts) {
    GC *gc = &G_(ts)->gc;
    enterinc(gc);
}
