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
        ((o)->mark = ((o)->mark & ~maskcolorbits) | csG_white(gs))

/* mark object as black */
#define markblack(o) \
        ((o)->mark = ((o)->mark & ~maskwhitebits) | bitmask(BLACKBIT))

/* mark object as gray */
#define markgray(o)	resetbits((o)->mark, maskcolorbits)



/* check if 'TValue' is object and white */
#define valiswhite(v)		(iscollectable(v) && iswhite(gcoval(v)))

/* check if 'HTable' key is object and white */
#define keyiswhite(n)		(keyiscollectable(n) && iswhite(keygcoval(n)))



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
        case CS_VPROTO: return &gco2proto(o)->gclist;
        case CS_VCSCL: return &gco2clcs(o)->gclist;
        case CS_VCCL: return &gco2clc(o)->gclist;
        case CS_VCLASS: return &gco2cls(o)->gclist;
        case CS_VARRAY: return &gco2arr(o)->gclist;
        case CS_VHTABLE: return &gco2ht(o)->gclist;
        case CS_VTHREAD: return &gco2th(o)->gclist;
        case CS_VUSERDATA: return &gco2u(o)->gclist;
        default: cs_assert(0); return NULL;
    }
}


/*
** Write barrier that marks white object 'o' pointed to by black
** object 'r' (as in root), effectively moving the collector forward.
** This is to ensure that the garbage collector doesn't miss any objects
** that have become reachable since the last collection cycle and
** to maintain the invariant that no black object points to a white object.
** In case we are in sweep phase, then just mark 'r' as white to keep the
** invariant and prevent further write barriers.
** Not to worry, the white bits after atomic phase are switched
** around, so marking 'r' white won't make it collectable until
** the next cycle.
*/
void csG_barrier_(cs_State *ts, GCObject *r, GCObject *o) {
    GState *gs = G_(ts);
    if (invariantstate(gs)) { /* invariant holds ? */
        cs_assert(isblack(r) && iswhite(o));
        markobject_(gs, o);
    } else { /* in sweep phase */
        cs_assert(sweepstate(gs));
        markwhite(gs, r);
    }
}


/*
** Write barrier that marks the black object 'r' that is
** pointing to a white object gray again, effectively
** moving the collector backwards.
*/
void csG_barrierback_(cs_State *ts, GCObject *r) {
    GState *gs = G_(ts);
    cs_assert(isblack(r) && !isdead(gs, r));
    linkobjgclist(r, gs->grayagain);
}



/* ------------------------------------------------------------------------
** Mark functions
** ------------------------------------------------------------------------ */

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
    cs_assert(iswhite(o));
    switch (o->tt_) {
        case CS_VSHRSTR: case CS_VLNGSTR: {
            markblack(o);
            break;
        }
        case CS_UPVALUE: {
            UpVal *uv = gco2uv(o);
            if (uvisopen(uv)) 
                markgray(uv);
            else 
                markblack(uv);
            markvalue(gs, uv->v.p);
            break;
        }
        case CS_VIMETHOD: {
            IMethod *im = gco2im(o);
            markblack(im);
            markobject(gs, im->ins);
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
        case CS_VUSERDATA: {
            UserData *ud = gco2u(o);
            if (ud->nuv == 0) { /* no user values? */
                markblack(ud);
                break;
            } /* else fall through */
        } /* else fall through */
    linklist:
        case CS_VHTABLE: case CS_VPROTO: case CS_VCSCL: case CS_VCCL:
        case CS_VCLASS: case CS_VTHREAD: {
            linkobjgclist(o, gs->graylist);
            break;
        }
        default: cs_assert(0); break;
    }
}


/* mark 'VMT' */
cs_sinline cs_umem markvmt(GState *gs, TValue *vmt) {
    cs_assert(vmt != NULL);
    for (int i = 0; i < CS_MM_N; i++)
        markvalue(gs, &vmt[i]);
    return CS_MM_N; /* size of VMT array */
}


/*
** Clear keys for empty entries in tables. If entry is empty, mark its
** entry as dead. This allows the collection of the key, but keeps its
** entry in the table: its removal could break a chain and could break
** a table traversal. Other places never manipulate dead keys, because
** its associated empty value is enough to signal that the entry is
** logically empty.
*/
static void clearkey (Node *n) {
  cs_assert(isempty(nodeval(n)));
  if (keyiscollectable(n))
    setdeadkey(n); /* unused key; remove it */
}


/* mark 'HTable' slots */
static cs_umem markhtable(GState *gs, HTable *ht) {
    Node *last = htnodelast(ht);
    for (Node *n = htnode(ht, 0); n < last; n++) {
        if (!isempty(nodeval(n))) { /* entry is not empty? */
            cs_assert(!keyisnil(n));
            markkey(gs, n);
            markvalue(gs, nodeval(n));
        } else
            clearkey(n);
    }
    return 1 + (htsize(ht) * 2); /* key/value pairs + table itself */
}


/* mark 'Function' */
static cs_umem markfunction(GState *gs, Proto *p) {
    int i;
    markobjectcheck(gs, p->source);
    for (i = 0; i < p->sizep; i++)
        markobject(gs, p->p[i]);
    for (i = 0; i < p->sizek; i++)
        markvalue(gs, &p->k[i]);
    for (i = 0; i < p->sizelocals; i++)
        markobjectcheck(gs, p->locals[i].name);
    for (i = 0; i < p->sizeupvals; i++)
        markobjectcheck(gs, p->upvals[i].name);
    /* p + prototypes + constants + locals + upvalues */
    return 1 + p->sizep + p->sizek + p->sizelocals + p->sizeupvals;
}


/* mark C closure */
static cs_umem markcclosure(GState *gs, CClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        TValue *uv = &cl->upvals[i];
        markvalue(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


/* mark CScript closure */
static cs_umem markcstclosure(GState *gs, CSClosure *cl) {
    markobjectcheck(gs, cl->p);
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = cl->upvals[i];
        markobjectcheck(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


/* mark 'OClass' */
static cs_umem markclass(GState *gs, OClass *cls) {
    markobjectcheck(gs, cls->methods);
    return 1 + (cls->vmt ? markvmt(gs, cls->vmt) : 0); /* class + VMT */
}


/* mark 'UserData' */
static cs_umem markuserdata(GState *gs, UserData *ud) {
    /* no need to mark VMT, all functions in there are light C functions */
    for (int i = 0; i < ud->nuv; i++)
        markobjectcheck(gs, &ud->uv[i]);
    return 1 + ud->nuv; /* user values + userdata */
}


/*
** Marks thread (per-thread-state).
** Threads do not use write barriers, because using
** a write barrier correctly on each thread modification
** would introduce a lot of complexity.
** And the way we deal with properly remarking the
** thread is by linking it into the 'grayagain', a list
** which is again traversed in 'GCSatomic' state.
** Marking (traversing) the thread black only occurs in
** either 'GCSpropagate' or 'GCSatomic' state and between
** those two states only in 'GCSpropagate' can the objects get modified.
** So if we are in 'GCSpropagate' we link the object into
** 'grayagain' and 'GCSatomic' state remarks our thread,
** restoring the invariant state (in cases where the thread
** really did get modified after we marked it black) without
** using write barriers.
*/
static cs_umem markthread(GState *gs, cs_State *ts) {
    SPtr sp = ts->stack.p;
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
    return 1 + stacksize(ts); /* thread + stack slots */
}


/*
** Remarks open upvalues in 'thwouv'.
** Basically acts as a barrier for values in already
** visited open upvalues. It keeps those values alive
** as long as its upvalue is marked.
** These upvalues won't get marked if thread is already
** marked and upvalue itself is not marked (or if
** thread doesn't contain any open upvalues).
*/
static cs_umem markopenupvalues(GState *gs) {
    cs_umem work = 0;
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


static cs_umem markarray(GState *gs, Array *arr) {
    cs_assert(arr->n > 0);
    for (uint i = 0; i < arr->n; i++)
        markvalue(gs, &arr->b[i]);
    return 1 + arr->n; /* array + elements */
}


/* traverse a single gray object turning it black */
static cs_umem propagate(GState *gs) {
    GCObject *o = gs->graylist;
    cs_assert(!iswhite(o)); /* 'o' must be gray */
    notw2black(o); /* mark gray object as black */
    gs->graylist = *getgclist(o); /* remove from gray list */
    switch(o->tt_) {
        case CS_VUSERDATA: return markuserdata(gs, gco2u(o));
        case CS_VHTABLE: return markhtable(gs, gco2ht(o));
        case CS_VPROTO: return markfunction(gs, gco2proto(o));
        case CS_VCSCL: return markcstclosure(gs, gco2clcs(o));
        case CS_VCCL: return markcclosure(gs, gco2clc(o));
        case CS_VCLASS: return markclass(gs, gco2cls(o));
        case CS_VARRAY: return markarray(gs, gco2arr(o));
        case CS_VTHREAD: return markthread(gs, gco2th(o));
        default: cs_assert(0); return 0;
    }
}


/* propagates all gray objects */
static cs_umem propagateall(GState *gs) {
    cs_umem work;
    for (work = 0; gs->graylist != NULL; work += propagate(gs));
    return work;
}



/* -----------------------------------------------------------------------
** Free objects
** ----------------------------------------------------------------------- */

/* free objectc 'o' */
static void freeobject(cs_State *ts, GCObject *o) {
    switch (o->tt_) {
        case CS_VSHRSTR: {
            OString *s = gco2str(o);
            csS_remove(ts, s); /* remove the weak reference */
            csM_freemem(ts, s, sizeofstring(s->shrlen));
            break;
        }
        case CS_VLNGSTR: {
            OString *s = gco2str(o);
            csM_freemem(ts, s, sizeofstring(s->u.lnglen));
            break;
        }
        case CS_VPROTO: csF_free(ts, gco2proto(o)); break;
        case CS_UPVALUE: csF_freeupval(ts, gco2uv(o)); break;
        case CS_VCSCL: csM_free_(ts, o, sizeofCScl(gco2clcs(o)->nupvalues)); break;
        case CS_VCCL: csM_free_(ts, o, sizeofCcl(gco2clc(o)->nupvalues)); break;
        case CS_VCLASS: csMM_freeclass(ts, gco2cls(o)); break;
        case CS_VARRAY: csA_free(ts, gco2arr(o)); break;
        case CS_VHTABLE: csH_free(ts, gco2ht(o)); break;
        case CS_VINSTANCE: csM_free(ts, gco2ins(o)); break;
        case CS_VIMETHOD: csM_free(ts, gco2im(o)); break;
        case CS_VTHREAD: csT_free(ts, gco2th(o)); break;
        case CS_VUSERDATA: csMM_freeuserdata(ts, gco2u(o)); break;
        default: cs_assert(0); break;
    }
}



/* -----------------------------------------------------------------------
** Sweep functions
** ----------------------------------------------------------------------- */

static GCObject **sweeplist(cs_State *ts, GCObject **l, int nobjects, 
                            int *nsweeped) {
    GState *gs = G_(ts);
    int white = csG_white(gs); /* current white */
    int whitexor = whitexor(gs);
    int i;
    cs_assert(nobjects > 0);
    for (i = 0; i < nobjects && *l; i++) {
        GCObject *o = *l;
        int mark = o->mark;
        if (whitexor & mark) { /* collect ? */
            *l = o->next;
            freeobject(ts, o);
        } else { /* mark white */
            o->mark = cast_ubyte((mark & ~maskcolorbits) | white);
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
** Sweep objects in 'list' until alive (marked) object
** or the end of the list.
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



/* -----------------------------------------------------------------------
** Finalization (__gc)
** ----------------------------------------------------------------------- */

/*
** Get object from 'tobefin' list and link it back
** to the 'objects' list.
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
static void callGCMM(cs_State *ts) {
    TValue v;
    const TValue *m;
    GState *gs = G_(ts);
    setgcoval(ts, &v, gettobefin(gs));
    if (ttisnil(m = csMM_get(ts, &v, CS_MM_GC))) /* __gc is nil? */
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
        callGCMM(ts);
    return i;
}


/*
** Check if object has a finalizer and move it into 'fin'
** list but only if it wasn't moved already indicated by
** 'FINBIT' being set, additionally don't move it in case
** state is closing.
*/
void csG_checkfin(cs_State *ts, GCObject *o, TValue vmt[CS_MM_N]) {
    GCObject **pp;
    GState *gs = G_(ts);
    if (isfin(o) ||             /* object is already marked... */
        ttisnil(&vmt[CS_MM_GC])     /* or has no finalizer... */
        || (gs->gcstop & GCSTPCLS))     /* ...or closing state ? */
        return; /* nothing to be done */
    /* otherwise move 'o' to 'fin' list */
    if (sweepstate(gs)) {
        markwhite(gs, o); /* sweep object 'o' */
        if (gs->sweeppos == &o->next) /* should sweep more? */
            gs->sweeppos = sweepuntilalive(ts, gs->sweeppos);
    }
    /* serch for pointer in 'objects' pointing to 'o' */
    for (pp = &gs->objects; *pp != o; pp = &(*pp)->next) {/* empty */}
    *pp = o->next; /* remove 'o' from 'objects' */
    o->next = gs->fin; /* link it in 'fin' list */
    gs->fin = o; /* adjust 'fin' head */
    setbit(o->mark, FINBIT); /* mark it */
}



/* -------------------------------------------------------------------------
 * GState control
 * ------------------------------------------------------------------------- */


/* get the last 'next' object in list 'l' */
cs_sinline GCObject **findlastnext(GCObject **l) {
    while (*l)
        l = &(*l)->next;
    return l;
}


/*
** Separate all unreachable objects with a finalizer
** in 'fin' list into the 'tobefin' list.
** In case 'force' is true then every object in the
** 'fin' list will moved.
*/
static void separatetobefin(GState *gs, int force) {
    GCObject **finp = &gs->fin;
    GCObject **lastnext = findlastnext(&gs->tobefin);
    for (GCObject *curr = *finp; curr != NULL; curr = *finp) {
        if (!(iswhite(curr) || force)) { /* marked and force is false ? */
            finp = &curr->next;
        } else { /* unreachable, move it into 'tobefin' */
            *finp = curr->next; /* unlink 'curr' from 'fin' */
            curr->next = *lastnext;
            *lastnext = curr; /* link 'curr' into 'tobefin' */
            lastnext = &curr->next; /* advance 'lastnext' */
        }
    }
}


static cs_umem marktobefin(GState *gs) {
    cs_umem cnt = 0;
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


static cs_umem atomic(cs_State *ts) {
    GState *gs = G_(ts);
    cs_mem work = 0;
    GCObject *grayagain = gs->grayagain;
    gs->grayagain = NULL;
    cs_assert(gs->weak == NULL); /* 'weak' unused */
    cs_assert(!iswhite(gs->mainthread)); /* mainthread must be marked */
    gs->gcstate = GCSatomic;
    markobject(gs, ts); /* mark running thread */
    markvalue(gs, &gs->c_registry); /* mark registry */
    markvmts(gs); /* mark global VMTs */
    work += propagateall(gs); /* traverse all gray objects */
    work += markopenupvalues(gs); /* mark open upvalues */
    work += propagateall(gs); /* propagate changes */
    cs_assert(gs->graylist == NULL); /* all must be propagated */
    gs->graylist = grayagain; /* set 'grayagain' as the graylist */
    work += propagateall(gs); /* propagate gray objects from 'grayagain' */
    /* separate and 'resurrect' unreachable objects with the finalizer */
    separatetobefin(gs, 0);
    work += marktobefin(gs); /* and mark them */
    work += propagateall(gs); /* propagate changes */
    gs->whitebit = whitexor(gs); /* flip current white bit */
    cs_assert(gs->graylist == NULL); /* all must be propagated */
    cs_assert(gs->weak == NULL); /* 'weak' unused */
    return work; /* estimate number of slots marked by 'atomic' */
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
    markvalue(gs, &gs->c_registry); /* mark registry */
    markvmts(gs);
    marktobefin(gs); /* mark any finalizing object left from previous cycle */
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
static cs_umem singlestep(cs_State *ts) {
    cs_umem work;
    GState *gs = G_(ts);
    gs->gcstopem = 1; /* prevent emergency collections */
    switch (gs->gcstate) {
        case GCSpause: { /* mark roots */
            restartgc(gs);
            gs->gcstate = GCSpropagate;
            work = 1; /* mainthread */
            break;
        }
        case GCSpropagate: { /* gray -> black */
            if (gs->graylist == NULL) { /* no more gray objects? */
                gs->gcstate = GCSenteratomic;
                work = 0;
            } else { /* otherwise propagate them */
                work = propagate(gs);
            }
            break;
        }
        case GCSenteratomic: { /* remark */
            work = atomic(ts);
            entersweep(ts);
            gs->gcestimate = gettotalbytes(gs);
            break;
        }
        case GCSsweepall: {
            work = sweepstep(ts, &gs->fin, GCSsweepfin);
            break;
        }
        case GCSsweepfin: {
            work = sweepstep(ts, &gs->tobefin, GCSsweeptofin);
            break;
        }
        case GCSsweeptofin: {
            work = sweepstep(ts, NULL, GCSsweepend);
            break;
        }
        case GCSsweepend: {
            /* state not used for anything but clarity */
            gs->gcstate = GCScallfin;
            work = 0;
            break;
        }
        case GCScallfin: { /* call finalizers */
            if (gs->tobefin && !gs->gcemergency) {
                gs->gcstopem = 0; /* can collect in finalizer */
                work = callNfinalizers(ts, GCFINMAX) * GCFINCOST;
            } else {
                gs->gcstate = GCSpause;
                work = 0;
            }
            break;
        }
        default: cs_assert(0); return 0;
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
        callGCMM(ts);
}


/*
** Free all objects except main thread, additionally
** call all finalizers.
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
** Run collector until gcdebt is less than a stepsize
** or the full cycle was done (GState state is GCSpause).
** Both the gcdebt and stepsize are converted to 'work',
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
