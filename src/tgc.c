/*
** tgc.c
** Garbage Collector (more or less port of Lua incremental GC)
** See Copyright Notice in tokudae.h
*/

#define tgt_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tgc.h"
#include "tlist.h"
#include "tokudaeconf.h"
#include "tfunction.h"
#include "tokudaelimits.h"
#include "tmeta.h"
#include "tobject.h"
#include "tstate.h"
#include "ttable.h"
#include "tmem.h"
#include "tstring.h"
#include "tvm.h"
#include "tprotected.h"


/* mark object as current white */
#define markwhite(gs,o) \
    ((o)->mark = cast_ubyte(((o)->mark & ~maskcolorbits) | tokuG_white(gs)))

/* mark object as black */
#define markblack(o) \
    ((o)->mark = cast_ubyte(((o)->mark & ~maskwhitebits) | bitmask(BLACKBIT)))

/* mark object as gray */
#define markgray(o)	resetbits((o)->mark, maskcolorbits)



/* check if 'TValue' is object and white */
#define valiswhite(v)	    (iscollectable(v) && iswhite(gcoval(v)))

/* check if 'Table' key is object and white */
#define keyiswhite(n)	    (keyiscollectable(n) && iswhite(keygcoval(n)))


/* 'markobject_' but only if object is white */
#define markobject(gs,o) \
        (iswhite(o) ? markobject_(gs, obj2gco(o)) : (void)0)

/* 'markobject' but only if 'o' is non-NULL */
#define markobjectN(gs,o)       ((o) ? markobject(gs, o) : (void)0)

/* 'markobject_' but only if 'v' is object and white */
#define markvalue(gs,v) \
        (valiswhite(v) ? markobject_(gs, gcoval(v)) : (void)0)

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
** Action of visiting a slot or sweeping an object converted
** into bytes.
*/
#define WORK2MEM	cast_int(sizeof(TValue))


/* adjust 'pause' (same as in Lua, percentage of the 'pause') */
#define PAUSEADJ	100




/* forward declare */
static void markobject_(GState *gs, GCObject *o);


static void cleargraylists(GState *gs) {
    gs->graylist = gs->grayagain = NULL;
    gs->weak = NULL;
}


/* create new object and append it to 'objects' */
GCObject *tokuG_newoff(toku_State *T, size_t sz, int tt_, size_t offset) {
    GState *gs = G(T);
    char *p = tokuM_newobj(T, novariant(tt_), sz);
    GCObject *o = cast(GCObject*, p + offset);
    o->mark = tokuG_white(gs); /* mark as white */
    o->tt_ = cast_ubyte(tt_);
    o->next = gs->objects; /* chain it */
    gs->objects = o;
    return o;
}


GCObject *tokuG_new(toku_State *T, size_t size, int tt_) {
    return tokuG_newoff(T, size, tt_, 0);
}


void tokuG_fix(toku_State *T, GCObject *o) {
    GState *gs = G(T);
    toku_assert(o == gs->objects); /* first in the list */
    markgray(o);
    gs->objects = o->next;
    o->next = gs->fixed;
    gs->fixed = o;
}


/*
** Set gcdebt to a new value keeping the value (totalbytes + gcdebt)
** invariant (and avoiding underflows in 'totalbytes').
*/
void tokuG_setgcdebt(GState *gs, t_mem debt) {
    t_mem total = gettotalbytes(gs);
    toku_assert(total > 0);
    if (debt < total - TOKU_MAXMEM) /* 'totalbytes' would underflow ? */
        debt = total - TOKU_MAXMEM; /* set maximum relative debt possible */
    gs->totalbytes = total - debt;
    gs->gcdebt = debt;
}



/* link objects 'gclist' into the list 'l' */
#define linkgclist(o,l)		linkgclist_(obj2gco(o), &(o)->gclist, &(l))

static void linkgclist_(GCObject *o, GCObject **gclist, GCObject **list) {
    toku_assert(!isgray(o));
    *gclist = *list;
    *list = o;
    markgray(o);
}

/* simmilar to 'linkgclist' but generic */
#define linkobjgclist(o,l)	linkgclist_(obj2gco(o), getgclist(o), &(l))


static GCObject **getgclist(GCObject *o) {
    switch (o->tt_) {
        case TOKU_VPROTO: return &gco2proto(o)->gclist;
        case TOKU_VCSCL: return &gco2clt(o)->gclist;
        case TOKU_VCCL: return &gco2clc(o)->gclist;
        case TOKU_VLIST: return &gco2list(o)->gclist;
        case TOKU_VTABLE: return &gco2ht(o)->gclist;
        case TOKU_VTHREAD: return &gco2th(o)->gclist;
        case TOKU_VUSERDATA: return &gco2u(o)->gclist;
        default: toku_assert(0); return NULL;
    }
}


/*
** Write barrier that marks white object 'o' pointed to by black
** object 'r' (as in root), effectively moving the collector forward.
** If called in the sweep phase, it clears the black object to white
** (sweeps it) to avoid other barrier calls for this same object.
** NOTE: there is a difference between the dead and white object.
** Object is considered dead if it was white prior to sweep phase in
** the current GC cycle, so clearing (sweeping) the black object to white
** by calling this function in the sweep phase, will not result in the
** object being collected in the current GC cycle.
*/
void tokuG_barrier_(toku_State *T, GCObject *r, GCObject *o) {
    GState *gs = G(T);
    toku_assert(isblack(r) && iswhite(o) && !isdead(gs, r) && !isdead(gs, o));
    if (keepinvariant(gs)) /* must keep invariant? */
        markobject_(gs, o); /* restore invariant */
    else { /* sweep phase */
        toku_assert(issweepstate(gs));
        markwhite(gs, r); /* sweep the black object */
    }
}


/*
** Write barrier that marks the black object 'r' that is
** pointing to a white object gray again, effectively
** moving the collector backwards.
*/
void tokuG_barrierback_(toku_State *T, GCObject *r) {
    GState *gs = G(T);
    toku_assert(isblack(r) && !isdead(gs, r));
    linkobjgclist(r, gs->grayagain);
}



/* {======================================================================
** Mark objects
** ======================================================================= */

/*
** Marks white object 'o'.
** Some objects are directly marked as black, these
** objects do not point to other objects, or their references
** can be resolved by up to a single recursive call to this function.
** Other objects are marked gray, more precisely they are
** first moved into 'gray' list and then marked as gray.
** The 'gclist' pointer is the way we link them into graylist, while
** preserving their link in the list of all objects ('object').
*/
static void markobject_(GState *gs, GCObject *o) {
    toku_assert(iswhite(o));
    switch (o->tt_) {
        case TOKU_VSHRSTR: case TOKU_VLNGSTR: {
            markblack(o); /* nothing to visit */
            break;
        }
        case TOKU_VUPVALUE: {
            UpVal *uv = gco2uv(o);
            if (uvisopen(uv)) 
                markgray(uv); /* open upvalues are kept gray */
            else 
                markblack(uv); /* closed upvalues are visited here */
            markvalue(gs, uv->v.p); /* mark its contents */
            break;
        }
        case TOKU_VIMETHOD: {
            IMethod *im = gco2im(o);
            markvalue(gs, &im->method);
            markobject(gs, im->ins);
            markblack(im); /* nothing else to mark */
            break;
        }
        case TOKU_VUMETHOD: {
            UMethod *um = gco2um(o);
            markvalue(gs, &um->method);
            markobject(gs, um->ud);
            markblack(um); /* nothing else to mark */
            break;
        }
        case TOKU_VINSTANCE: {
            Instance *ins = gco2ins(o);
            markobject(gs, ins->oclass);
            markobjectN(gs, ins->fields);
            markblack(ins); /* nothing else to mark */
            break;
        }
        case TOKU_VLIST: {
            List *l = gco2list(o);
            if (l->len == 0) { /* no elements? */
                markblack(l); /* nothing to visit */
                break;
            }
            /* else... */
            goto linklist; /* link to gray list */
        }
        case TOKU_VCLASS: {
            OClass *cls = gco2cls(o);
            markobjectN(gs, cls->sclass);
            markobjectN(gs, cls->metatable);
            markobjectN(gs, cls->methods);
            markblack(cls); /* nothing else to mark */
            break;
        }
        case TOKU_VUSERDATA: {
            UserData *ud = gco2u(o);
            if (ud->nuv == 0) { /* no user values? */
                markobjectN(gs, ud->metatable);
                markblack(ud); /* nothing else to mark */
                break;
            }
            /* else ... */
        } /* fall through */
    linklist:
        case TOKU_VTABLE: case TOKU_VPROTO: case TOKU_VCSCL:
        case TOKU_VCCL: case TOKU_VTHREAD: {
            linkobjgclist(o, gs->graylist);
            break;
        }
        default: toku_assert(0); /* invalid object tag */
    }
}


/*
** Clear keys for empty entries in tables. If entry is empty, mark its
** entry as dead. This allows the collection of the key, but keeps its
** entry in the table: its removal could break a chain and could break
** a table traversal. Other places never manipulate dead keys, because
** its associated empty value is enough to signal that the entry is
** logically empty.
*/
static void clearkey(Node *n) {
    toku_assert(isempty(nodeval(n)));
    if (keyiscollectable(n))
        setdeadkey(n); /* unused key; remove it */
}


static t_mem marktable(GState *gs, Table *t) {
    Node *last = htnodelast(t);
    for (Node *n = htnode(t, 0); n < last; n++) {
        if (!isempty(nodeval(n))) { /* entry is not empty? */
            toku_assert(!keyisnil(n));
            markkey(gs, n);
            markvalue(gs, nodeval(n));
        } else
            clearkey(n);
    }
    return 1 + cast_mem(htsize(t) * 2); /* table + key/value pairs */
}


static t_mem markproto(GState *gs, Proto *p) {
    int i;
    markobjectN(gs, p->source);
    for (i = 0; i < p->sizep; i++)
        markobjectN(gs, p->p[i]);
    for (i = 0; i < p->sizek; i++)
        markvalue(gs, &p->k[i]);
    for (i = 0; i < p->sizelocals; i++)
        markobjectN(gs, p->locals[i].name);
    for (i = 0; i < p->sizeupvals; i++)
        markobjectN(gs, p->upvals[i].name);
    /* p + prototypes + constants + locals + upvalues */
    return 1 + p->sizep + p->sizek + p->sizelocals + p->sizeupvals;
}


static t_mem markcclosure(GState *gs, CClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        TValue *uv = &cl->upvals[i];
        markvalue(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


static t_mem markcsclosure(GState *gs, TClosure *cl) {
    markobjectN(gs, cl->p);
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = cl->upvals[i];
        markobjectN(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


static t_mem markuserdata(GState *gs, UserData *ud) {
    markobjectN(gs, ud->metatable);
    for (t_ushort i = 0; i < ud->nuv; i++)
        markvalue(gs, &ud->uv[i].val);
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
static t_mem markthread(GState *gs, toku_State *T) {
    SPtr sp = T->stack.p;
    if (gs->gcstate == GCSpropagate)
        linkgclist(T, gs->grayagain); /* traverse 'T' again in 'atomic' */
    if (sp == NULL) /* stack not fully built? */
        return 1;
    toku_assert(gs->gcstate == GCSatomic || /* either in atomic phase... */
              T->openupval == NULL || /* or no open upvalues... */
              isintwups(T)); /* ...or 'T' is in correct list */
    for (; sp < T->sp.p; sp++) /* mark live stack elements */
        markvalue(gs, s2v(sp));
    for (UpVal *uv = T->openupval; uv != NULL; uv = uv->u.open.next)
        markobject(gs, uv); /* open upvalues cannot be collected */
    if (gs->gcstate == GCSatomic) { /* final traversal? */
        if (!gs->gcemergency) /* not an emergency collection? */
            tokuT_shrinkstack(T); /* shrink stack if possible */
        for (sp = T->sp.p; sp < T->stackend.p + EXTRA_STACK; sp++)
          setnilval(s2v(sp)); /* clear dead stack slice */
        /* 'markopenupvalues' might of removed thread from 'twups' list */
        if (!isintwups(T) && T->openupval != NULL) {
            T->twups = gs->twups; /* link it back */
            gs->twups = T;
        }
    }
    return 1 + stacksize(T); /* thread + stack slots */
}


/*
** Remarks open upvalues in 'twups'.
** Basically acts as a barrier for values in already
** visited open upvalues. It keeps those values alive
** as long as its upvalue is marked.
** These upvalues won't get marked if thread is already
** marked and upvalue itself is not marked (or if
** thread doesn't contain any open upvalues).
*/
static t_mem markopenupvalues(GState *gs) {
    t_mem work = 0; /* work estimate */
    toku_State *th;
    toku_State **pp = &gs->twups;
    while ((th = *pp) != NULL) {
        work++;
        if (iswhite(th) || th->openupval == NULL) {
            toku_assert(th->openupval == NULL);
            *pp = th->twups; /* remove thread from the list... */
            th->twups = th; /* ...and mark it as such */
            for (UpVal *uv = th->openupval; uv; uv = uv->u.open.next) {
                work++;
                /* if visited then keep values alive */
                if (!iswhite(uv)) {
                    toku_assert(uvisopen(uv) && isgray(uv));
                    markvalue(gs, uv->v.p);
                }
            }
        } else /* thread is marked and has upvalues */
            pp = &th->twups; /* keep it in the list */
    }
    return work;
}


static t_mem marklist(GState *gs, List *l) {
    toku_assert(l->len > 0);
    for (int i = 0; i < l->len; i++)
        markvalue(gs, &l->arr[i]);
    return 1 + l->len; /* list + elements */
}


/* 
** Traverse a single gray object turning it to black.
*/
static t_mem propagate(GState *gs) {
    GCObject *o = gs->graylist;
    toku_assert(!iswhite(o)); /* 'o' must be gray */
    notw2black(o); /* mark gray object as black */
    gs->graylist = *getgclist(o); /* remove from gray list */
    switch(o->tt_) {
        case TOKU_VUSERDATA: return markuserdata(gs, gco2u(o));
        case TOKU_VTABLE: return marktable(gs, gco2ht(o));
        case TOKU_VPROTO: return markproto(gs, gco2proto(o));
        case TOKU_VCSCL: return markcsclosure(gs, gco2clt(o));
        case TOKU_VCCL: return markcclosure(gs, gco2clc(o));
        case TOKU_VLIST: return marklist(gs, gco2list(o));
        case TOKU_VTHREAD: return markthread(gs, gco2th(o));
        default: toku_assert(0); return 0;
    }
}


/* propagates all gray objects */
static t_mem propagateall(GState *gs) {
    t_mem work = 0;
    while (gs->graylist)
        work += propagate(gs);
    return work;
}

/* }===================================================================== */


/* {=====================================================================
** Free objects
** ====================================================================== */

static void freeupval(toku_State *T, UpVal *uv) {
    if (uvisopen(uv))
        tokuF_unlinkupval(uv);
    tokuM_free(T, uv);
}


static void freeobject(toku_State *T, GCObject *o) {
    switch (o->tt_) {
        case TOKU_VPROTO: tokuF_free(T, gco2proto(o)); break;
        case TOKU_VUPVALUE: freeupval(T, gco2uv(o)); break;
        case TOKU_VLIST: tokuA_free(T, gco2list(o)); break;
        case TOKU_VTABLE: tokuH_free(T, gco2ht(o)); break;
        case TOKU_VINSTANCE: tokuM_free(T, gco2ins(o)); break;
        case TOKU_VIMETHOD: tokuM_free(T, gco2im(o)); break;
        case TOKU_VUMETHOD: tokuM_free(T, gco2um(o)); break;
        case TOKU_VTHREAD: tokuT_free(T, gco2th(o)); break;
        case TOKU_VCLASS: tokuM_free(T, gco2cls(o)); break;
        case TOKU_VSHRSTR: {
            OString *s = gco2str(o);
            tokuS_remove(T, s); /* remove it from the string table */
            tokuM_freemem(T, s, sizeofstring(s->shrlen));
            break;
        }
        case TOKU_VLNGSTR: {
            OString *s = gco2str(o);
            tokuM_freemem(T, s, sizeofstring(s->u.lnglen));
            break;
        }
        case TOKU_VCSCL: {
            TClosure *cl = gco2clt(o);
            tokuM_freemem(T, cl, sizeofTcl(cl->nupvalues));
            break;
        }
        case TOKU_VCCL: {
            CClosure *cl = gco2clc(o);
            tokuM_freemem(T, cl, sizeofCcl(cl->nupvalues));
            break;
        }
        case TOKU_VUSERDATA: {
            UserData *u = gco2u(o);
            tokuM_freemem(T, u, sizeofuserdata(u->nuv, u->size));
            break;
        }
        default: toku_assert(0); /* invalid object tag */
    }
}

/* }===================================================================== */


/* {======================================================================
** Sweep objects
** ======================================================================= */

static GCObject **sweeplist(toku_State *T, GCObject **l, t_uint nobjects, 
                            t_uint *nsweeped) {
    GState *gs = G(T);
    int white = tokuG_white(gs); /* current white */
    int whitexor = whitexor(gs); /* dead white */
    t_uint i;
    toku_assert(nobjects > 0);
    for (i = 0; *l != NULL && i < nobjects; i++) {
        GCObject *curr = *l;
        int mark = curr->mark;
        if (whitexor & mark) { /* is 'curr' dead? */
            *l = curr->next; /* remove 'curr' from list */
            freeobject(T, curr); /* and collect it */
        } else { /* otherwise change mark to 'white' */
            curr->mark = cast_ubyte((mark & ~maskcolorbits) | white);
            l = &curr->next; /* go to next element */
        }
    }
    if (nsweeped)
        *nsweeped = i; /* number of elements traversed */
    return (*l == NULL ? NULL : l);
}


/* do a single sweep step limited by 'GCSWEEPMAX' */
static t_uint sweepstep(toku_State *T, GCObject **nextlist, t_ubyte nextstate) {
    GState *gs = G(T);
    if (gs->sweeppos) {
        t_mem old_gcdebt = gs->gcdebt;
        t_uint count;
        gs->gccheck = 1; /* set check flag */
        gs->sweeppos = sweeplist(T, gs->sweeppos, GCSWEEPMAX, &count);
        gs->gcestimate += cast_umem(gs->gcdebt - old_gcdebt); /* update estimate */
        return count;
    } else { /* enter next state */
        gs->sweeppos = nextlist;
        gs->gcstate = nextstate;
        return 0; /* no work done */
    }
}


/*
** Sweep objects in 'list' until alive (marked) object
** or the end of the list.
*/
static GCObject **sweepuntilalive(toku_State *T, GCObject **l) {
    GCObject **old = l;
    do {
        l = sweeplist(T, l, 1, NULL);
    } while (old == l);
    return l;
}


static void entersweep(toku_State *T) {
    GState *gs = G(T);
    gs->gcstate = GCSsweepall;
    toku_assert(gs->sweeppos == NULL);
    gs->sweeppos = sweepuntilalive(T, &gs->objects);
}

/* }===================================================================== */


/* {=====================================================================
** Finalization (__gc)
** ====================================================================== */

/*
** If possible, shrink string table.
*/
static void checksizes(toku_State *T, GState *gs) {
    if (!gs->gcemergency) {
        if (gs->strtab.nuse < gs->strtab.size / 4) { /* strtab too big? */
            t_mem old_gcdebt = gs->gcdebt;
            tokuS_resize(T, gs->strtab.size / 2);
            gs->gcestimate += cast_umem(gs->gcdebt - old_gcdebt);
        }
    }
}


/*
** Get object from 'tobefin' list and link it back
** to the 'objects' list.
*/
static GCObject *gettobefin(GState *gs) {
    GCObject *o = gs->tobefin; /* get first element */
    toku_assert(o && isfin(o));
    gs->tobefin = o->next; /* remove it from 'tobefin' list */
    o->next = gs->objects; /* return it to 'objects' list */
    gs->objects = o;
    resetbit(o->mark, FINBIT); /* object is "normal" again */
    if (issweepstate(gs))
        markwhite(gs, o); /* "sweep" object */
    return o;
}


/* protected finalizer */
static void pgc(toku_State *T, void *userdata) {
    UNUSED(userdata);
    tokuV_call(T, T->sp.p - 2, 0);
}


/* call a finalizer "__gc" */
static void callgc(toku_State *T) {
    TValue v;
    const TValue *tm;
    GState *gs = G(T);
    toku_assert(!gs->gcemergency);
    setgcoval(T, &v, gettobefin(gs));
    tm = tokuTM_objget(T, &v, TM_GC);
    if (!notm(tm)) { /* is there a finalizer? */
        int status;
        t_ubyte old_allowhook = T->allowhook;
        t_ubyte old_gcstop = gs->gcstop;
        gs->gcstop |= GCSTP; /* avoid GC steps */
        T->allowhook = 0; /* stop debug hooks during GC metamethod */
        setobj2s(T, T->sp.p++, tm); /* push finalizer... */
        setobj2s(T, T->sp.p++, &v); /* ...and its argument */
        T->cf->status |= CFST_FIN; /* will run a finalizer */
        status = tokuPR_call(T, pgc, NULL, savestack(T,T->sp.p-2), T->errfunc);
        T->cf->status &= cast_ubyte(~CFST_FIN); /* not running a finalizer */
        T->allowhook = old_allowhook; /* restore hooks */
        gs->gcstop = old_gcstop; /* restore state */
        if (t_unlikely(status != TOKU_STATUS_OK)) { /* error in __gc? */
            tokuT_warnerror(T, "__gc");
            T->sp.p--; /* pop error object */
        }
    }
}


/* call objects with finalizer in 'tobefin' */
static int runNfinalizers(toku_State *T, int n) {
    int i;
    GState *gs = G(T);
    for (i = 0; i < n && gs->tobefin; i++)
        callgc(T);
    return i;
}


/*
** Check if object has a finalizer and move it into 'fin' list but
** only if it wasn't moved already indicated by 'FINBIT' being set,
** additionally don't move it in case state is closing.
*/
void tokuG_checkfin(toku_State *T, GCObject *o, Table *mt) {
    GCObject **pp;
    GState *gs = G(T);
    if (isfin(o) ||                     /* or object is already marked... */
        gfasttm(gs, mt, TM_GC) == NULL ||   /* or it has no finalizer... */
        (gs->gcstop & GCSTPCLS))                /* ...or state is closing? */
        return; /* nothing to be done */
    /* otherwise move 'o' to 'fin' list */
    if (issweepstate(gs)) {
        markwhite(gs, o); /* sweep object 'o' */
        if (gs->sweeppos == &o->next) /* should sweep more? */
            gs->sweeppos = sweepuntilalive(T, gs->sweeppos);
    }
    /* search for pointer in 'objects' pointing to 'o' */
    for (pp = &gs->objects; *pp != o; pp = &(*pp)->next) {/* empty */}
    *pp = o->next; /* remove 'o' from 'objects' */
    o->next = gs->fin; /* link it in 'fin' list */
    gs->fin = o; /* adjust 'fin' head */
    setbit(o->mark, FINBIT); /* mark it */
}

/* }===================================================================== */


/*
** Get the last 'next' object in list 'l'.
*/
t_sinline GCObject **findlastnext(GCObject **l) {
    while (*l)
        l = &(*l)->next;
    return l;
}


/*
** Separate all unreachable objects with a finalizer in 'fin' list
** into the 'tobefin' list. In case 'force' is true then every
** object in the 'fin' list will moved regardless if its 'mark'.
*/
static void separatetobefin(GState *gs, int force) {
    GCObject *curr;
    GCObject **finp = &gs->fin;
    GCObject **lastnext = findlastnext(&gs->tobefin);
    while ((curr = *finp) != NULL) {
        toku_assert(isfin(curr));
        if (!(iswhite(curr) || force)) /* not being collected? */
            finp = &curr->next; /* ignore it and advance the 'fin' list */
        else { /* otherwise move it into 'tobefin' */
            *finp = curr->next; /* remove 'curr' from 'fin' */
            curr->next = *lastnext; /* link is at the end of 'tobefin' list */
            *lastnext = curr; /* link 'curr' into 'tobefin' */
            lastnext = &curr->next; /* advance 'lastnext' */
        }
    }
}


static t_mem marktobefin(GState *gs) {
    t_mem count = 0;
    for (GCObject *o = gs->tobefin; o != NULL; o = o->next) {
        markobject(gs, o);
        count++;
    }
    return count;
}


static t_mem atomic(toku_State *T) {
    GState *gs = G(T);
    GCObject *grayagain = gs->grayagain;
    t_mem work = 0;
    gs->grayagain = NULL;
    toku_assert(gs->weak == NULL); /* 'weak' unused */
    toku_assert(!iswhite(gs->mainthread)); /* mainthread must be marked */
    gs->gcstate = GCSatomic;
    markobject(gs, T); /* mark running thread */
    markvalue(gs, &gs->c_list); /* mark clist */
    markvalue(gs, &gs->c_table); /* mark ctable */
    work += propagateall(gs); /* traverse all gray objects */
    work += markopenupvalues(gs); /* mark open upvalues */
    work += propagateall(gs); /* propagate changes */
    toku_assert(gs->graylist == NULL); /* all must be propagated */
    gs->graylist = grayagain; /* set 'grayagain' as the graylist */
    work += propagateall(gs); /* propagate gray objects from 'grayagain' */
    /* separate and 'resurrect' unreachable objects with the finalizer... */
    separatetobefin(gs, 0);
    work += marktobefin(gs); /* ...and mark them */
    work += propagateall(gs); /* propagate changes */
    tokuS_clearcache(gs);
    gs->whitebit = whitexor(gs); /* flip current white bit */
    toku_assert(gs->graylist == NULL); /* all must be propagated */
    toku_assert(gs->weak == NULL); /* 'weak' unused */
    return work; /* estimate number of values marked by 'atomic' */
}


/*
** Set the "time" to wait before starting a new GC cycle; cycle will
** start when memory use hits the threshold of ('estimate' * pause /
** PAUSEADJ). (Division by 'estimate' should be OK: it cannot be zero,
** because Tokudae cannot even start with less than PAUSEADJ bytes).
*/
static void setpause(GState *gs) {
    t_mem debt;
    t_mem threshold;
    int pause = cast_int(getgcparam(gs->gcparams[TOKU_GCP_PAUSE]));
    t_mem estimate = cast_mem(gs->gcestimate / PAUSEADJ); /* adjust estimate */
    toku_assert(estimate > 0);
    threshold = (pause < TOKU_MAXMEM / estimate) /* can fit ? */
              ? estimate * pause /* yes */
              : TOKU_MAXMEM; /* overflow; truncate to maximum */
    /* debt = totalbytes - ((gcestimate/100)*pause) */
    debt = gettotalbytes(gs) - threshold;
    if (debt > 0) debt = 0;
    tokuG_setgcdebt(gs, debt);
}


/* restart GState, mark roots and leftover 'tobefin' objects */
static void restartgc(GState *gs) {
    cleargraylists(gs);
    markobject(gs, gs->mainthread); /* mark mainthread */
    markvalue(gs, &gs->c_list); /* mark clist */
    markvalue(gs, &gs->c_table); /* mark ctable */
    marktobefin(gs); /* mark any finalizing object left from previous cycle */
}


/*
** Garbage collector state machine. GCSpause marks all the roots.
** GCSpropagate propagates gray objects into black or links them into
** 'grayagain' for atomic phase. GCSenteratomic enters the atomic state
** and marks main thread, globals, etc... and propagates all of them.
** Finally it clears the strings table and changes white bit. GCSsweepall
** sweeps all the objects in 'objects'. GCSsweepfin sweeps all the objects
** in 'fin'. GCSsweeptofin sweeps all the objects in 'tobefin'. GCSsweepend
** indicates end of the sweep phase. GCScallfin calls finalizers of all the
** objects in 'tobefin' and puts them back into 'objects' list, before the
** call to finalizer.
*/
static t_mem singlestep(toku_State *T) {
    t_mem work;
    GState *gs = G(T);
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
            } else /* otherwise propagate them */
                work = propagate(gs); /* traverse gray objects */
            break;
        }
        case GCSenteratomic: { /* re-mark all reachable objects */
            work = atomic(T);
            entersweep(T); /* set 'objects' as first list to sweep */
            gs->gcestimate = cast_umem(gettotalbytes(gs)); /* first estimate */
            break;
        }
        case GCSsweepall: { /* sweep objects in */
            work = sweepstep(T, &gs->fin, GCSsweepfin);
            break;
        }
        case GCSsweepfin: { /* sweep objects with finalizers */
            work = sweepstep(T, &gs->tobefin, GCSsweeptofin);
            break;
        }
        case GCSsweeptofin: { /* sweep objects to be finalized */
            work = sweepstep(T, NULL, GCSsweepend);
            break;
        }
        case GCSsweepend: { /* finish sweeps */
            checksizes(T, gs);
            gs->gcstate = GCScallfin;
            work = 0;
            break;
        }
        case GCScallfin: { /* call remaining finalizers */
            if (gs->tobefin && !gs->gcemergency) {
                gs->gcstopem = 0; /* enable collections during finalizers */
                work = cast_uint(runNfinalizers(T, GCFINMAX)) * GCFINCOST;
            } else { /* emergency or no more finalizers */
                gs->gcstate = GCSpause;
                work = 0;
            }
            break;
        }
        default: toku_assert(0); return 0;
    }
    gs->gcstopem = 0;
    return work;
}


/* free list 'l' objects until 'limit' */
t_sinline void freelist(toku_State *T, GCObject *l, GCObject *limit) {
    while (l != limit) {
        GCObject *next = l->next;
        freeobject(T, l);
        l = next;
    }
}


static void callpendingfinalizers(toku_State *T) {
    GState *gs = G(T);
    while (gs->tobefin)
        callgc(T);
}


/*
** Free all objects except main thread, additionally
** call all finalizers.
*/
void tokuG_freeallobjects(toku_State *T) {
    GState *gs = G(T);
    gs->gcstop = GCSTPCLS; /* paused by state closing */
    separatetobefin(gs, 1); /* seperate all objects with a finalizer */
    toku_assert(gs->fin == NULL);
    callpendingfinalizers(T);
    freelist(T, gs->objects, obj2gco(gs->mainthread));
    toku_assert(gs->fin == NULL); /* no new finalizers */
    freelist(T, gs->fixed, NULL); /* collect fixed objects */
    toku_assert(gs->strtab.nuse == 0);
}


/*
** Advances the garbage collector until it reaches a state
** allowed by 'statemask'.
*/
void tokuG_rununtilstate(toku_State *T, int statemask) {
    GState *gs = G(T);
    while (!testbit(statemask, gs->gcstate))
        singlestep(T);
}


/*
** Run collector until gcdebt is less than a stepsize
** or the full cycle was done (GState state is GCSpause).
** Both the gcdebt and stepsize are converted to 'work',
*/
static void step(toku_State *T, GState *gs) {
    int stepmul = (getgcparam(gs->gcparams[TOKU_GCP_STEPMUL])|1);
    t_ubyte nbits = gs->gcparams[TOKU_GCP_STEPSIZE];
    t_mem debt = (gs->gcdebt / WORK2MEM) * stepmul;
    t_mem stepsize = (nbits <= sizeof(t_mem) * 8 - 2) /* fits ? */
                    ? (((cast_mem(1) << nbits) / WORK2MEM) * stepmul)
                    : TOKU_MAXMEM; /* overflows; keep maximum value */
    do { /* do until pause or enough negative debt */
        t_mem work = singlestep(T); /* perform one single step */
        debt -= work;
    } while (debt > -stepsize && gs->gcstate != GCSpause);
    if (gs->gcstate == GCSpause) /* pause? */
        setpause(gs); /* pause until next cycle */
    else { /* otherwise enough debt collected */
        debt = (debt / stepmul) * WORK2MEM; /* convert 'work' to bytes */
        tokuG_setgcdebt(gs, debt);
    }
}


void tokuG_step(toku_State *T) {
    GState *gs = G(T);
    if (!gcrunning(gs)) /* stopped ? */
        tokuG_setgcdebt(gs, -2000);
    else
        step(T, gs);
}


static void fullinc(toku_State *T, GState *gs) {
    if (keepinvariant(gs)) /* have black objects? */
        entersweep(T); /* sweep all tto turn them back to white */
    /* finish any pending sweep phase to start a new cycle */
    tokuG_rununtilstate(T, bitmask(GCSpause));
    tokuG_rununtilstate(T, bitmask(GCScallfin)); /* run up to finalizers */
    /* estimate must be correct after full GC cycle */
    toku_assert(gs->gcestimate == cast_umem(gettotalbytes(gs)));
    tokuG_rununtilstate(T, bitmask(GCSpause)); /* finish collection */
    setpause(gs);
}


void tokuG_fullinc(toku_State *T, int isemergency) {
    GState *gs = G(T);
    toku_assert(!gs->gcemergency);
    gs->gcemergency = cast_ubyte(isemergency);
    fullinc(T, G(T));
    gs->gcemergency = 0u;
}


/* traverse a list making all its elements white */
static void whitelist(GState *gs, GCObject *l) {
    int white = tokuG_white(gs);
    for (; l != NULL; l = l->next)
        l->mark = cast_ubyte((l->mark & ~maskgcbits) | white);
}


/*
** Enter incremental mode. Turn all objects white, make all
** intermediate lists point to NULL (to avoid invalid pointers),
** and go to the pause state.
*/
static void enterinc(GState *gs) {
    whitelist(gs, gs->objects);
    whitelist(gs, gs->fin);
    whitelist(gs, gs->tobefin);
    gs->gcstate = GCSpause;
}


/* enter incremental mode */
void tokuG_incmode(toku_State *T) {
    enterinc(G(T));
}
