/*
** cgc.c
** Garbage Collector
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cgc.h"
#include "carray.h"
#include "csconf.h"
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
#define markwhite(gs,o) \
    (gcomark_(o) = (gcomark_(o) & ~maskcolorbits) | csG_white(gs))

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
#define markvalue(gs,v) \
    (valiswhite(v) ? markobject_(gs, gcoval(v)) : (void)0)

/* 'markobject_' but only if 'o' is non-NULL */
#define markobjectcheck(gs,o)	((o) ? markobject_(gs, obj2gco(o)) : (void)0)

/* 'markobject_' but only if object is white */
#define markobject(gs,o) \
    (iswhite(o) ? markobject_(gs, obj2gco(o)) : (void)0)

/* 'markobject' but only if key value is object and white */
#define markkey(gs, n) \
    (keyiswhite(n) ? markobject_(gs, keygcoval(n)) : (void)0)



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
static void markobject_(GState *gs, GCObject *o);


static void cleargraylists(GState *gs) {
    gs->graylist = gs->grayagain = NULL;
    gs->weak = NULL;
}


/* create new object and append it to 'objects' */
GCObject *csG_newoff(cs_State *ts, size_t sz, int tt_, size_t offset) {
    GState *gs = G_(ts);
    char *p = cast_charp(csM_malloc(ts, sz));
    GCObject *o = cast(GCObject*, p + offset);
    o->mark = csG_white(gs);
    o->tt_ = tt_;
    o->next = gs->objects;
    gs->objects = o;
    return o;
}


GCObject *csG_new_(cs_State *ts, size_t size, int tt_) {
    return csG_newoff(ts, size, tt_, 0);
}


void csG_fix(cs_State *ts, GCObject *o) {
    GState *gs = G_(ts);
    cs_assert(o == gs->objects); /* first in the list */
    markgray(o);
    gs->objects = o->next;
    o->next = gs->fixed;
    gs->fixed = o;
}


/* set collector gcdebt */
void csG_setgcdebt(GState *gs, cs_mem gcdebt) {
    cs_mem total = gettotalbytes(gs);
    if (gcdebt < total - CRMEM_MAX) /* 'total' will underflow ? */
        gcdebt = total - CRMEM_MAX;
    gs->totalbytes = total - gcdebt;
    gs->gcdebt = gcdebt;
}



/* link objects 'gclist' into the list 'l' */
#define linkgclist(o,l)		linkgclist_(obj2gco(o), &(o)->gclist, &(l))

static void linkgclist_(GCObject *o, GCObject **gclist, GCObject **list) {
    cs_assert(!isgray(o));
    *gclist = *list;
    *list = o;
    markgray(o);
}

/* simmilar to 'linkgclist' but generic */
#define linkobjgclist(o,l)	linkgclist_(obj2gco(o), getgclist(o), &(l))


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
        default: cs_assert(0); cs_unreachable();
    }
}


#include <stdio.h>
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
    GState *gs = G_(ts);
    printf("barrier -> R[%p], O[%p]\n", (void*)r, (void*)o);
    if (invariantstate(gs)) { /* invariant holds ? */
        cs_assert(isblack(r) && iswhite(o));
        markobject_(gs, o);
        printf("barrier done, invariant holds\n");
    } else { /* in sweep phase */
        cs_assert(sweepstate(gs));
        markwhite(gs, r);
        printf("barrier done, sweep state\n");
    }
}


/*
 * Write barrier that marks the black object 'r' that is
 * pointing to a white object gray again, effectively
 * moving the collector backwards.
 */
void csG_barrierback_(cs_State *ts, GCObject *r) {
    GState *gs = G_(ts);
    cs_assert(isblack(r) && !isdead(gs, r));
    linkobjgclist(r, gs->grayagain);
    printf("barrierback -> R[%p]\n", (void*)r);
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
static void markobject_(GState *gs, GCObject *o) {
    printf("Marking object: %p\n", (void*)o);
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
            markvalue(gs, uv->v.p);
            break;
        }
        case CS_VMETHOD: {
            IMethod *im = gco2im(o);
            markblack(im);
            markobject(gs, im->receiver);
            markvalue(gs, &im->method);
            break;
        }
        case CS_VINSTANCE: {
            Instance *ins = gco2ins(o);
            markblack(ins);
            markobjectcheck(gs, ins->oclass);
            markobjectcheck(gs, ins->fields);
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
        case CS_VCLASS: case CS_VTHREAD: {
            linkobjgclist(o, gs->graylist);
            printf("Marked by linking into graylist\n");
            break;
        }
        default: {
            cs_unreachable();
            cs_assert(0);
        }
    }
}


/* mark 'VMT' */
cs_sinline cs_mem markvmt(GState *gs, TValue *vmt) {
    cs_assert(vmt != NULL);
    for (int i = 0; i < CS_NUM_MM; i++)
        markvalue(gs, &vmt[i]);
    return CS_NUM_MM; /* size of VMT array */
}


/* mark 'HTable' slots */
static cs_mem markhtable(GState *gs, HTable *ht) {
    Node *last = htnodelast(ht);
    for (Node *n = htnode(ht, 0); n < last; n++) {
        if (!isempty(nodeval(n))) {
            TValue *val = nodeval(n);
            GCObject *obj = gcoval(val);
            markkey(gs, n);
            cs_assert(val != NULL);
            cs_assert(ttypetag(val) != CS_VEMPTY);
            if (iscollectable(val)
                    && (gcomark_(obj)
                    & maskwhitebits)) {
                markobject_(gs, obj);
            }
            // markvalue(gs, nodeval(n));
        }
    }
    return 1 + (htsize(ht) * 2); /* key/value pairs + table itself */
}


/* mark 'Function' */
static cs_mem markfunction(GState *gs, Function *fn) {
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
static cs_mem markcclosure(GState *gs, CClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        TValue *uv = &cl->upvals[i];
        markvalue(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


/* mark CScript closure */
static cs_mem markcstclosure(GState *gs, CrClosure *cl) {
    markobjectcheck(gs, cl->fn);
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = cl->upvals[i];
        markobjectcheck(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


/* mark 'OClass' */
static cs_mem markclass(GState *gs, OClass *cls) {
    markobjectcheck(gs, cls->methods);
    return 1 + (cls->vmt ? markvmt(gs, cls->vmt) : 0); /* class + VMT */
}


/* mark 'UserData' */
static cs_mem markuserdata(GState *gs, UserData *ud) {
    cs_mem extra = 0;
    if (ud->vmt) /* have VMT? */
        extra = markvmt(gs, ud->vmt);
    for (int i = 0; i < ud->nuv; i++)
        markobjectcheck(gs, &ud->uv[i]);
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
    SPtr sp = ts->stack.p;
    cs_assert(gs->gcstate & (GCSpropagate | GCSatomic));
    if (gs->gcstate == GCSpropagate)
        linkgclist(ts, gs->grayagain);
    if (sp == NULL) /* stack not fully built? */
        return 1;
    /* either in atomic, or no open upvalues or 'ts' is in correct list */
    cs_assert(gs->gcstate==GCSatomic || ts->openupval==NULL || isinthwouv(ts));
    for (; sp < ts->sp.p; sp++)
        markvalue(gs, s2v(sp));
    for (UpVal *uv = ts->openupval; uv != NULL; uv = uv->u.open.next)
        markobject(gs, uv);
    if (gs->gcstate == GCSatomic) { /* final traversal? */
        if (!gs->gcemergency) /* not an emergency collection? */
            csT_shrinkstack(ts); /* shrink stack if possible */
        for (sp = ts->sp.p; sp < ts->stackend.p + EXTRA_STACK; sp++)
          setnilval(s2v(sp)); /* clear dead stack slice */
        /* 'markopenupvalues' might of removed thread from 'thwouv' list */
        if (gs->gcstate==GCSatomic && !isinthwouv(ts) && ts->openupval!=NULL) {
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
                    markvalue(gs, uv->v.p);
                }
            }
        } else {
            pp = &th->thwouv;
        }
    }
    return work;
}


static cs_mem markarray(GState *gs, Array *arr) {
    cs_assert(arr->n > 0);
    for (uint i = 0; i < arr->n; i++)
        markvalue(gs, &arr->b[i]);
    return 1 + arr->n; /* array + elements */
}


/* traverse a single gray object turning it black */
static cs_mem propagate(GState *gs) {
    GCObject *o = gs->graylist;
    cs_assert(!iswhite(o)); /* 'o' must be gray */
    notw2black(o);
    gs->graylist = *getgclist(o);
    printf("propagate <%s: %p>\n", typename(o->tt_), (void*)o);
    switch(o->tt_) {
        case CS_VUDATA: return markuserdata(gs, gco2ud(o));
        case CS_VHTABLE: return markhtable(gs, gco2ht(o));
        case CS_VFUNCTION: return markfunction(gs, gco2fn(o));
        case CS_VCRCL: return markcstclosure(gs, gco2crcl(o));
        case CS_VCCL: return markcclosure(gs, gco2ccl(o));
        case CS_VCLASS: return markclass(gs, gco2cls(o));
        case CS_VARRAY: return markarray(gs, gco2arr(o));
        case CS_VTHREAD: return markthread(gs, gco2th(o));
        default: cs_unreachable(); cs_assert(0); return 0;
    }
    printf("propagate done\n");
}


/* propagates all gray objects */
static cs_mem propagateall(GState *gs) {
    cs_mem work;
    for (work = 0; gs->graylist; work += propagate(gs));
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
        case CS_VCRCL: csM_free_(ts, o, sizeofcrcl(gco2crcl(o)->nupvalues)); break;
        case CS_VCCL: csM_free_(ts, o, sizeofccl(gco2ccl(o)->nupvalues)); break;
        case CS_VCLASS: csMM_freeclass(ts, gco2cls(o)); break;
        case CS_VARRAY: csA_free(ts, gco2arr(o)); break;
        case CS_VINSTANCE: csM_free(ts, gco2ins(o)); break;
        case CS_VMETHOD: csM_free(ts, gco2im(o)); break;
        case CS_VTHREAD: csT_free(ts, gco2th(o)); break;
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
    int white = csG_white(gs); /* current white */
    int whitexor = whitexor(gs);
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
    GState *gs = G_(ts);
    if (gs->sweeppos) {
        cs_mem old_gcdebt = gs->gcdebt;
        int cnt;
        gs->sweeppos = sweeplist(ts, gs->sweeppos, GCSWEEPMAX, &cnt);
        gs->gcestimate += gs->gcdebt - old_gcdebt;
        return cnt;
    } else {
        gs->sweeppos = nextlist;
        gs->gcstate = nextstate;
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
    GState *gs = G_(ts);
    gs->gcstate = GCSsweepall;
    cs_assert(gs->sweeppos == NULL);
    gs->sweeppos = sweepuntilalive(ts, &gs->objects);
}



/* -------------------------------------------------------------------------
 * Finalization (__gc__)
 * ------------------------------------------------------------------------- */

/*
 * Get object from 'tobefin' list and link it back
 * to the 'objects' list.
 */
static GCObject *gettobefin(GState *gs) {
    GCObject *o = gs->tobefin;
    cs_assert(o && isfin(o));
    gs->tobefin = o->next;
    o->next = gs->objects;
    gs->objects = o;
    if (sweepstate(gs))
        markwhite(gs, o);
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
    GState *gs = G_(ts);
    setgcoval(ts, &v, gettobefin(gs));
    if (ttisnil(m = csMM_get(ts, &v, CS_MM_GC))) /* __gc is nil ? */
        return; /* done; no finalizer */
    int old_stop = gs->gcstop;
    gs->gcstop = GCSTP; /* prevent recursive GState calls */
    setobj2s(ts, ts->sp.p++, m);
    setobj2s(ts, ts->sp.p++, &v);
    ptrdiff_t oldtop = savestack(ts, ts->sp.p - 2);
    ts->cf->status |= CFST_FIN; /* running a finalizer */
    int status = csPR_call(ts, protectedfinalizer, NULL, oldtop, ts->errfunc);
    ts->cf->status &= ~CFST_FIN; /* finalizer returned */
    gs->gcstop = old_stop;
    if (c_unlikely(status != CS_OK)) {
        csT_warnerror(ts, "__gc");
        ts->sp.p--; /* pop err object */
    }
}


/* call objects with finalizer in 'tobefin' */
static int callNfinalizers(cs_State *ts, int n) {
    int i;
    GState *gs = G_(ts);
    for (i = 0; i < n && gs->tobefin; i++)
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
    GState *gs = G_(ts);
    if (isfin(o) || ttisnil(&vmt[CS_MM_GC]) || (gs->gcstop & GCSTPCLS))
        return;
    if (sweepstate(gs)) {
        markwhite(gs, o);
        if (gs->sweeppos == &o->next)
            gs->sweeppos = sweepuntilalive(ts, gs->sweeppos);
    }
    for (pp = &gs->objects; *pp != o; pp = &(*pp)->next);
    *pp = o->next;
    o->next = gs->fin;
    gs->fin = o;
    setbit(o->mark, FINBIT);
}



/* -------------------------------------------------------------------------
 * GState control
 * ------------------------------------------------------------------------- */


/* clear all unmarked keys in global strings table */
static void refclear(GState *gs) {
    HTable *ht = gs->strings;
    Node *limit = htnodelast(ht);
    for (Node *n = htnode(ht, 0); n < limit; n++) {
        cs_assert(keytt(n) == CS_VSTRING);
        if (iswhite(keygcoval(n))) { /* (key: string, value: string) */
            setnilkey(n);
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
    GCObject **finlp = &gs->fin;
    GCObject **lastnext = getlastnext(&gs->tobefin);
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
    for (GCObject *o = gs->tobefin; o != NULL; o = o->next) {
        markobject(gs, o);
        cnt++;
    }
    return cnt;
}


static void markvmts(GState *gs) {
    for (int i = 0; i < CS_NUM_TYPES; i++)
        if (gs->vmt[i])
            markvmt(gs, gs->vmt[i]);
}


static cs_mem atomic(cs_State *ts) {
    GState *gs = G_(ts);
    cs_mem work = 0;
    GCObject *grayagain = gs->grayagain;
    gs->grayagain = NULL;
    cs_assert(gs->weak == NULL);
    cs_assert(!iswhite(gs->mainthread));
    gs->gcstate = GCSatomic;
    markobject(gs, ts); /* mark running thread */
    markvalue(gs, &gs->ginstance); /* mark global instance */
    markvmts(gs); /* mark global VMTs */
    work += propagateall(gs); /* traverse all gray objects */
    work += markopenupvalues(gs); /* mark open upvalues */
    work += propagateall(gs); /* propagate changes */
    cs_assert(gs->graylist = NULL); /* all must be propagated */
    gs->graylist = grayagain; /* set 'grayagain' as the graylist */
    work += propagateall(gs); /* propagate gray objects from 'grayagain' */
    refclear(gs); /* all accessible objects are marked, clear references */
    /* separate and 'resurrect' unreachable objects with the finalizer */
    separatetobefin(gs, 0);
    work += marktobefin(gs); /* and mark them */
    work += propagateall(gs); /* propagate changes */
    refclear(gs); /* all 'resurrected' objects are marked, clear references */
    gs->whitebit = whitexor(gs); /* flip white bit */
    cs_assert(gs->graylist == NULL); /* all must be propagated */
    return work; /* gcestimated number of marked slots during 'atomic' */
}


/* 
** Set collector pause; called after end of each full GState cycle.
** The new threshold is calculated as 'gcestimate' / 'pause'.
** 'PAUSEADJ' is there to provide more precise control over
** when collection occurs (the value is chosen by testing from
** the side of Lua developers). 
** One could think of 'gs->gcpause' to be the percentage as
** it is divided by 'PAUSEADJ' which is 100.
*/
static void setpause(GState *gs) {
    int pause = getgcparam(gs->gcpause);
    cs_mem gcestimate = gs->gcestimate / PAUSEADJ;
    cs_assert(gcestimate > 0); /* CScript state memory >= PAUSEADJ */
    cs_mem threshold = (pause < (CRMEM_MAX / gcestimate)) /* can fit ? */
                        ? (gcestimate * pause) /* yes */
                        : CRMEM_MAX; /* no; use maximum */
    /* gcdebt = totalbytes - ((gcestimate/100)*pause) */
    cs_mem gcdebt = gettotalbytes(gs) - threshold;
    if (gcdebt > 0) gcdebt = 0;
    csG_setgcdebt(gs, gcdebt);
}


/* restart GState, mark roots and leftover 'tobefin' objects */
static void restartgc(GState *gs) {
    cleargraylists(gs);
    markobject(gs, gs->mainthread); /* mark mainthread */
    markvalue(gs, &gs->ginstance); /* mark global instance */
    markvmts(gs);
    /* there could be leftover unreachable objects
     * with a finalizer from the previous cycle in
     * case of emergency collection, so mark them */
    marktobefin(gs);
}


/*
** Garbage collector state machine.
** GCSpause marks all the roots.
** GCSpropagate propagates gray objects into black
** or links them into 'grayagain' for atomic phase.
** GCSenteratomic enters the atomic state and
** marks main thread, globals, etc... and propagates
** all of them. Finally it clears the strings table
** and changes white bit.
** GCSsweepall sweeps all the objects in 'objects'.
** GCSsweepfin sweeps all the objects in 'fin'.
** GCSsweeptofin sweeps all the objects in 'tobefin'.
** GCSsweepend (in current version) does nothing
** but provide clarity that sweep phase is over.
** GCScallfin calls finalizers of all the objects
** in 'tobefin' and puts them back into 'objects'
** list after the call.
*/
static cs_mem singlestep(cs_State *ts) {
    cs_mem work;
    GState *gs = G_(ts);
    gs->gcstopem = 1; /* prevent emergency collections */
    switch (gs->gcstate) {
        case GCSpause: { /* mark roots */
            printf(">>>GCSpause<<<\n");
            restartgc(gs);
            gs->gcstate = GCSpropagate;
            work = 1; /* mainthread */
            break;
        }
        case GCSpropagate: { /* gray -> black */
            printf(">>>GCSpropagate<<<\n");
            if (gs->graylist) {
                work = propagate(gs);
            } else { /* no more gray objects */
                gs->gcstate = GCSenteratomic;
                work = 0;
            }
            break;
        }
        case GCSenteratomic: { /* remark */
            printf(">>>GCSenteratomic<<<\n");
            work = atomic(ts);
            entersweep(ts);
            gs->gcestimate = gettotalbytes(gs);
            break;
        }
        case GCSsweepall: {
            printf(">>>GCSsweepall<<<\n");
            work = sweepstep(ts, &gs->fin, GCSsweepfin);
            break;
        }
        case GCSsweepfin: {
            printf(">>>GCSsweepfin<<<\n");
            work = sweepstep(ts, &gs->tobefin, GCSsweeptofin);
            break;
        }
        case GCSsweeptofin: {
            printf(">>>GCSsweeptofin<<<\n");
            work = sweepstep(ts, NULL, GCSsweepend);
            break;
        }
        case GCSsweepend: {
            printf(">>>GCSsweepend<<<\n");
            /* state not used for anything but clarity */
            gs->gcstate = GCScallfin;
            work = 0;
            break;
        }
        case GCScallfin: { /* call finalizers */
            printf(">>>GCScallfin<<<\n");
            if (gs->tobefin && !gs->gcemergency) {
                gs->gcstopem = 0; /* can collect in finalizer */
                work = callNfinalizers(ts, GCFINMAX) * GCFINCOST;
            } else {
                gs->gcstate = GCSpause;
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
    gs->gcstopem = 0;
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
    GState *gs = G_(ts);
    while (gs->tobefin)
        callfin(ts);
}


/*
 * Free all objects except main thread, additionally
 * call all finalizers.
 */
void csG_freeallobjects(cs_State *ts) {
    GState *gs = G_(ts);
    gs->gcstop = GCSTPCLS; /* paused by state closing */
    separatetobefin(gs, 1);
    cs_assert(gs->fin == NULL);
    runallfinalizers(ts);
    freelist(ts, gs->objects, obj2gco(gs->mainthread));
    cs_assert(gs->fin == NULL);
    freelist(ts, gs->fixed, NULL);
}


/* run GState steps until 'state' is in any of the states of 'statemask' */
void csG_rununtilstate(cs_State *ts, int statemask) {
    GState *gs = G_(ts);
    while (!testbits(gs->gcstate, statemask))
        singlestep(ts);
}


/*
 * Run collector until gcdebt is less than a stepsize
 * or the full cycle was done (GState state is GCSpause).
 * Both the gcdebt and stepsize are converted to 'work',
 */
static void step(cs_State *ts, GState *gs) {
    int stepmul = getgcparam(gs->gcstepmul) | 1;
    cs_mem gcdebt = (gs->gcdebt / WORK2MEM) * stepmul;
    cs_mem stepsize = (gs->gcstepsize <= sizeof(cs_mem) * 8 - 2 /* fits ? */
            ? (cast_mem(1) << gs->gcstepsize) / WORK2MEM /* yes */
            : CRMEM_MAX); /* no; return maximum possible value */
    do {
        gcdebt -= singlestep(ts);
    } while (gcdebt > -stepsize && gs->gcstate != GCSpause);
    if (gs->gcstate == GCSpause) {
        setpause(gs);
    } else {
        gcdebt = (gcdebt / stepmul) * WORK2MEM; /* convert back to bytes */
        csG_setgcdebt(gs, gcdebt);
    }
}


void csG_step(cs_State *ts) {
    GState *gs = G_(ts);
    if (!gcrunning(gs)) /* stopped ? */
        csG_setgcdebt(gs, -2000);
    else
        step(ts, gs);
}


static void fullcycle(cs_State *ts) {
    GState *gs = G_(ts);
    if (invariantstate(gs)) /* already have black objects ? */
        entersweep(ts); /* if so sweep them first */
    csG_rununtilstate(ts, bitmask(GCSpause)); /* restart collector */
    csG_rununtilstate(ts, bitmask(GCScallfin)); /* run until finalizers */
    cs_assert(gs->gcestimate == gettotalbytes(gs)); /* end of cycle, check gcestimate */
    csG_rununtilstate(ts, bitmask(GCSpause)); /* finish collection */
    setpause(gs);
}


void csG_full(cs_State *ts, int isemergency) {
    GState *gs = G_(ts);
    cs_assert(!gs->gcemergency);
    gs->gcemergency = isemergency;
    fullcycle(ts);
    gs->gcemergency = 0;
}


/* traverse a list making all its elements white */
static void whitelist (GState *gs, GCObject *l) {
    int white = csG_white(gs);
    for (; l != NULL; l = l->next)
        l->mark = cast_byte((l->mark & ~maskgcbits) | white);
}


/*
** Enter incremental mode. Turn all objects white, make all
** intermediate lists point to NULL (to avoid invalid pointers),
** and go to the pause state.
*/
static void enterinc (GState *gs) {
    whitelist(gs, gs->objects);
    whitelist(gs, gs->fin);
    whitelist(gs, gs->tobefin);
    gs->gcstate = GCSpause;
}


/* enter incremental mode */
void csG_incmode(cs_State *ts) {
    enterinc(G_(ts));
}
