/*
** cgc.c
** Garbage Collector
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cgc.h"
#include "clist.h"
#include "csconf.h"
#include "cfunction.h"
#include "climits.h"
#include "cmeta.h"
#include "cobject.h"
#include "cstate.h"
#include "ctable.h"
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

/* check if 'Table' key is object and white */
#define keyiswhite(n)		(keyiscollectable(n) && iswhite(keygcoval(n)))


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
GCObject *csG_newoff(cs_State *C, size_t sz, int tt_, size_t offset) {
    GState *gs = G(C);
    char *p = csM_newobj(C, novariant(tt_), sz);
    GCObject *o = cast(GCObject*, p + offset);
    o->mark = csG_white(gs); /* mark as white */
    o->tt_ = tt_;
    o->next = gs->objects; /* chain it */
    gs->objects = o;
    return o;
}


GCObject *csG_new(cs_State *C, size_t size, int tt_) {
    return csG_newoff(C, size, tt_, 0);
}


void csG_fix(cs_State *C, GCObject *o) {
    GState *gs = G(C);
    cs_assert(o == gs->objects); /* first in the list */
    markgray(o);
    gs->objects = o->next;
    o->next = gs->fixed;
    gs->fixed = o;
}


/* set collector gcdebt */
void csG_setgcdebt(GState *gs, c_smem debt) {
    c_smem total = gettotalbytes(gs);
    cs_assert(total > 0);
    if (debt < total - MAXSMEM) /* 'totalbytes' would underflow ? */
        debt = total - MAXSMEM; /* set maximum relative debt possible */
    gs->totalbytes = total - debt;
    gs->gcdebt = debt;
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
        case CS_VLIST: return &gco2list(o)->gclist;
        case CS_VTABLE: return &gco2ht(o)->gclist;
        case CS_VTHREAD: return &gco2th(o)->gclist;
        case CS_VUSERDATA: return &gco2u(o)->gclist;
        default: cs_assert(0); return NULL;
    }
}


/*
** Write barrier that marks white object 'o' pointed to by black
** object 'r' (as in root), effectively moving the collector forward.
** If called in the sweep phase, it clears the black object to white
** (sweeps it) to avoid other barrier calls for this same object.
** NOTE that there is a difference between the dead and white object.
** Object is considered dead if it was white prior to sweep phase in
** the current GC cycle, so clearing (sweeping) the black object to white
** by calling this function in the sweep phase, will not result in the
** object being collected.
*/
void csG_barrier_(cs_State *C, GCObject *r, GCObject *o) {
    GState *gs = G(C);
    cs_assert(isblack(r) && iswhite(o) && !isdead(gs, r) && !isdead(gs, o));
    if (keepinvariant(gs)) /* must keep invariant? */
        markobject_(gs, o); /* restore invariant */
    else { /* sweep phase */
        cs_assert(issweepstate(gs));
        markwhite(gs, r); /* sweep the black object */
    }
}


/*
** Write barrier that marks the black object 'r' that is
** pointing to a white object gray again, effectively
** moving the collector backwards.
*/
void csG_barrierback_(cs_State *C, GCObject *r) {
    GState *gs = G(C);
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
** preserving their link in the list of all objects ('object').
*/
static void markobject_(GState *gs, GCObject *o) {
    cs_assert(iswhite(o));
    switch (o->tt_) {
        case CS_VSHRSTR: case CS_VLNGSTR: {
            markblack(o); /* nothing to visit */
            break;
        }
        case CS_VUPVALUE: {
            UpVal *uv = gco2uv(o);
            if (uvisopen(uv)) 
                markgray(uv); /* open upvalues are kept gray */
            else 
                markblack(uv); /* closed upvalues are visited here */
            markvalue(gs, uv->v.p); /* mark its contents */
            break;
        }
        case CS_VIMETHOD: {
            IMethod *im = gco2im(o);
            markobject(gs, im->ins);
            markvalue(gs, &im->method);
            markblack(im); /* nothing else to mark */
            break;
        }
        case CS_VINSTANCE: {
            Instance *ins = gco2ins(o);
            markobject(gs, ins->oclass);
            markobjectN(gs, ins->fields);
            markblack(ins); /* nothing else to mark */
            break;
        }
        case CS_VLIST: {
            List *arr = gco2list(o);
            if (arr->n == 0) { /* no elements? */
                markblack(arr); /* nothing to visit */
                break;
            }
            /* else... */
            goto linklist; /* link to gray list */
        }
        case CS_VCLASS: {
            OClass *cls = gco2cls(o);
            markobjectN(gs, cls->metalist);
            markobjectN(gs, cls->methods);
            markobjectN(gs, cls->sclass);
            markblack(cls); /* nothing else to mark */
            break;
        }
        case CS_VUSERDATA: {
            UserData *ud = gco2u(o);
            if (ud->nuv == 0) { /* no user values? */
                markobjectN(gs, ud->metalist); /* mark its metalist */
                markblack(ud); /* nothing else to mark */
                break;
            }
            /* else ... */
        } /* fall through */
    linklist:
        case CS_VTABLE: case CS_VPROTO: case CS_VCSCL:
        case CS_VCCL: case CS_VTHREAD: {
            linkobjgclist(o, gs->graylist);
            break;
        }
        default: cs_assert(0); /* invalid object tag */
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
static void clearkey (Node *n) {
    cs_assert(isempty(nodeval(n)));
    if (keyiscollectable(n))
        setdeadkey(n); /* unused key; remove it */
}


static c_mem marktable(GState *gs, Table *t) {
    Node *last = htnodelast(t);
    for (Node *n = htnode(t, 0); n < last; n++) {
        if (!isempty(nodeval(n))) { /* entry is not empty? */
            cs_assert(!keyisnil(n));
            markkey(gs, n);
            markvalue(gs, nodeval(n));
        } else
            clearkey(n);
    }
    return 1 + htsize(t) * 2; /* table + key/value pairs */
}


static c_mem markproto(GState *gs, Proto *p) {
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


static c_mem markcclosure(GState *gs, CClosure *cl) {
    for (int i = 0; i < cl->nupvalues; i++) {
        TValue *uv = &cl->upvals[i];
        markvalue(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


static c_mem markcsclosure(GState *gs, CSClosure *cl) {
    markobjectN(gs, cl->p);
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = cl->upvals[i];
        markobjectN(gs, uv);
    }
    return 1 + cl->nupvalues; /* closure + upvalues */
}


static c_mem markuserdata(GState *gs, UserData *ud) {
    markobjectN(gs, ud->metalist);
    for (int i = 0; i < ud->nuv; i++)
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
static c_mem markthread(GState *gs, cs_State *C) {
    SPtr sp = C->stack.p;
    if (gs->gcstate == GCSpropagate)
        linkgclist(C, gs->grayagain); /* traverse 'C' again in 'atomic' */
    if (sp == NULL) /* stack not fully built? */
        return 1;
    cs_assert(gs->gcstate == GCSatomic || /* either in atomic phase... */
              C->openupval == NULL || /* or no open upvalues... */
              isinthwouv(C)); /* ...or 'C' is in correct list */
    for (; sp < C->sp.p; sp++) /* mark live stack elements */
        markvalue(gs, s2v(sp));
    for (UpVal *uv = C->openupval; uv != NULL; uv = uv->u.open.next)
        markobject(gs, uv); /* open upvalues cannot be collected */
    if (gs->gcstate == GCSatomic) { /* final traversal? */
        if (!gs->gcemergency) /* not an emergency collection? */
            csT_shrinkstack(C); /* shrink stack if possible */
        for (sp = C->sp.p; sp < C->stackend.p + EXTRA_STACK; sp++)
          setnilval(s2v(sp)); /* clear dead stack slice */
        /* 'markopenupvalues' might of removed thread from 'thwouv' list */
        if (!isinthwouv(C) && C->openupval != NULL) {
            C->thwouv = gs->thwouv; /* link it back */
            gs->thwouv = C;
        }
    }
    return 1 + stacksize(C); /* thread + stack slots */
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
static int markopenupvalues(GState *gs) {
    int work = 0; /* work estimate */
    cs_State *th;
    cs_State **pp = &gs->thwouv;
    while ((th = *pp) != NULL) {
        work++;
        if (iswhite(th) || th->openupval == NULL) {
            cs_assert(th->openupval == NULL);
            *pp = th->thwouv; /* remove thread from the list... */
            th->thwouv = th; /* ...and mark it as such */
            for (UpVal *uv = th->openupval; uv; uv = uv->u.open.next) {
                work++;
                /* if visited then keep values alive */
                if (!iswhite(uv)) {
                    cs_assert(uvisopen(uv) && isgray(uv));
                    markvalue(gs, uv->v.p);
                }
            }
        } else { /* thread is marked and has upvalues */
            pp = &th->thwouv; /* keep it in the list */
        }
    }
    return work;
}


static c_mem marklist(GState *gs, List *l) {
    cs_assert(l->n > 0);
    for (uint i = 0; i < l->n; i++)
        markvalue(gs, &l->b[i]);
    return 1 + l->n; /* list + elements */
}


/* 
** Traverse a single gray object turning it to black.
*/
static c_mem propagate(GState *gs) {
    GCObject *o = gs->graylist;
    cs_assert(!iswhite(o)); /* 'o' must be gray */
    notw2black(o); /* mark gray object as black */
    gs->graylist = *getgclist(o); /* remove from gray list */
    switch(o->tt_) {
        case CS_VUSERDATA: return markuserdata(gs, gco2u(o));
        case CS_VTABLE: return marktable(gs, gco2ht(o));
        case CS_VPROTO: return markproto(gs, gco2proto(o));
        case CS_VCSCL: return markcsclosure(gs, gco2clcs(o));
        case CS_VCCL: return markcclosure(gs, gco2clc(o));
        case CS_VLIST: return marklist(gs, gco2list(o));
        case CS_VTHREAD: return markthread(gs, gco2th(o));
        default: cs_assert(0); return 0;
    }
}


/* propagates all gray objects */
static c_mem propagateall(GState *gs) {
    c_mem work = 0;
    while (gs->graylist)
        work += propagate(gs);
    return work;
}



/* -----------------------------------------------------------------------
** Free objects
** ----------------------------------------------------------------------- */

static void freeupval(cs_State *C, UpVal *uv) {
    if (uvisopen(uv))
        csF_unlinkupval(uv);
    csM_free(C, uv);
}


static void freeobject(cs_State *C, GCObject *o) {
    switch (o->tt_) {
        case CS_VPROTO: csF_free(C, gco2proto(o)); break;
        case CS_VUPVALUE: freeupval(C, gco2uv(o)); break;
        case CS_VLIST: csA_free(C, gco2list(o)); break;
        case CS_VTABLE: csH_free(C, gco2ht(o)); break;
        case CS_VINSTANCE: csM_free(C, gco2ins(o)); break;
        case CS_VIMETHOD: csM_free(C, gco2im(o)); break;
        case CS_VTHREAD: csT_free(C, gco2th(o)); break;
        case CS_VSHRSTR: {
            OString *s = gco2str(o);
            csS_remove(C, s); /* remove the weak reference */
            csM_freemem(C, s, sizeofstring(s->shrlen));
            break;
        }
        case CS_VLNGSTR: {
            OString *s = gco2str(o);
            csM_freemem(C, s, sizeofstring(s->u.lnglen));
            break;
        }
        case CS_VCSCL: {
            CSClosure *cl = gco2clcs(o);
            csM_freemem(C, cl, sizeofCScl(cl->nupvalues));
            break;
        }
        case CS_VCCL: {
            CClosure *cl = gco2clc(o);
            csM_freemem(C, cl, sizeofCcl(cl->nupvalues));
            break;
        }
        case CS_VCLASS: {
            OClass *cls = gco2cls(o);
            csM_free(C, cls);
            break;
        }
        case CS_VUSERDATA: {
            UserData *u = gco2u(o);
            csM_freemem(C, u, sizeofuserdata(u->nuv, u->size));
            break;
        }
        default: cs_assert(0); break; /* invalid object */
    }
}



/* -----------------------------------------------------------------------
** Sweep functions
** ----------------------------------------------------------------------- */

static GCObject **sweeplist(cs_State *C, GCObject **l, int nobjects, 
                            int *nsweeped) {
    GState *gs = G(C);
    int white = csG_white(gs); /* current white */
    int whitexor = whitexor(gs); /* dead white */
    int i;
    cs_assert(nobjects > 0);
    for (i = 0; *l != NULL && i < nobjects; i++) {
        GCObject *curr = *l;
        int mark = curr->mark;
        if (whitexor & mark) { /* is 'curr' dead? */
            *l = curr->next; /* remove 'curr' from list */
            freeobject(C, curr); /* and collect it */
        } else { /* otherwise change mark to 'white' */
            curr->mark = cast_byte((mark & ~maskcolorbits) | white);
            l = &curr->next; /* go to next element */
        }
    }
    if (nsweeped)
        *nsweeped = i; /* number of elements traversed */
    return (*l == NULL ? NULL : l);
}


/* do a single sweep step limited by 'GCSWEEPMAX' */
static int sweepstep(cs_State *C, GCObject **nextlist, int nextstate) {
    GState *gs = G(C);
    if (gs->sweeppos) {
        c_smem old_gcdebt = gs->gcdebt;
        int count;
        gs->sweeppos = sweeplist(C, gs->sweeppos, GCSWEEPMAX, &count);
        gs->gcestimate += gs->gcdebt - old_gcdebt; /* update estimate */
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
static GCObject **sweepuntilalive(cs_State *C, GCObject **l) {
    GCObject **old = l;
    do {
        l = sweeplist(C, l, 1, NULL);
    } while (old == l);
    return l;
}


static void entersweep(cs_State *C) {
    GState *gs = G(C);
    gs->gcstate = GCSsweepall;
    cs_assert(gs->sweeppos == NULL);
    gs->sweeppos = sweepuntilalive(C, &gs->objects);
}



/* -----------------------------------------------------------------------
** Finalization (__gc)
** ----------------------------------------------------------------------- */

/*
** If possible, shrink string table.
*/
static void checksizes(cs_State *C, GState *gs) {
    if (!gs->gcemergency) {
        if (gs->strtab.nuse < gs->strtab.size / 4) { /* strtab too big? */
            c_smem old_gcdebt = gs->gcdebt;
            csS_resize(C, gs->strtab.size / 2);
            gs->gcestimate += gs->gcdebt - old_gcdebt; /* correct estimate */
        }
    }
}


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
    if (issweepstate(gs))
        markwhite(gs, o);
    return o;
}


/* protected finalizer */
static void pgc(cs_State *C, void *userdata) {
    UNUSED(userdata);
    csV_call(C, C->sp.p - 2, 0);
}


/* call a finalizer "__gc" */
static void callgc(cs_State *C) {
    TValue v;
    const TValue *m;
    GState *gs = G(C);
    cs_assert(!gs->gcemergency);
    setgcoval(C, &v, gettobefin(gs));
    if (!ttisnil(m = csMM_get(C, &v, CS_MM_GC))) { /* have __gc? */
        int status;
        int old_gcstop = gs->gcstop;
        gs->gcstop = GCSTP; /* avoid GC steps */
        setobj2s(C, C->sp.p++, m); /* push finalizer... */
        setobj2s(C, C->sp.p++, &v); /* ...and its argument */
        C->cf->status |= CFST_FIN; /* will run a finalizer */
        status = csPR_call(C, pgc, NULL, savestack(C,C->sp.p-2), C->errfunc);
        C->cf->status &= ~CFST_FIN; /* not running a finalizer anymore */
        gs->gcstop = old_gcstop; /* restore state */
        if (c_unlikely(status != CS_OK)) { /* error while running __gc? */
            csT_warnerror(C, "__gc");
            C->sp.p--; /* pop error object */
        }
    }
}


/* call objects with finalizer in 'tobefin' */
static int runNfinalizers(cs_State *C, int n) {
    int i;
    GState *gs = G(C);
    for (i = 0; i < n && gs->tobefin; i++)
        callgc(C);
    return i;
}


/*
** Check if object has a finalizer and move it into 'fin'
** list but only if it wasn't moved already indicated by
** 'FINBIT' being set, additionally don't move it in case
** state is closing.
*/
void csG_checkfin(cs_State *C, GCObject *o, List *ml) {
    GCObject **pp;
    GState *gs = G(C);
    if (!ml ||                          /* no metalist... */
        isfin(o) ||                     /* or object is already marked... */
        ttisnil(&ml->b[CS_MM_GC]) ||    /* or it has no finalizer... */
        (gs->gcstop & GCSTPCLS))        /* ...or state is closing? */
        return; /* nothing to be done */
    /* otherwise move 'o' to 'fin' list */
    if (issweepstate(gs)) {
        markwhite(gs, o); /* sweep object 'o' */
        if (gs->sweeppos == &o->next) /* should sweep more? */
            gs->sweeppos = sweepuntilalive(C, gs->sweeppos);
    }
    /* search for pointer in 'objects' pointing to 'o' */
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
c_sinline GCObject **findlastnext(GCObject **l) {
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
        cs_assert(isfin(curr));
        if (!(iswhite(curr) || force)) { /* not being collected? */
            finp = &curr->next; /* ignore it and advance the 'fin' list */
        } else { /* otherwise move it into 'tobefin' */
            *finp = curr->next; /* remove 'curr' from 'fin' */
            curr->next = *lastnext; /* link is at the end of 'tobefin' list */
            *lastnext = curr; /* link 'curr' into 'tobefin' */
            lastnext = &curr->next; /* advance 'lastnext' */
        }
    }
}


static c_mem marktobefin(GState *gs) {
    c_mem count = 0;
    for (GCObject *o = gs->tobefin; o != NULL; o = o->next) {
        markobject(gs, o);
        count++;
    }
    return count;
}


static c_mem atomic(cs_State *C) {
    GState *gs = G(C);
    c_smem work = 0;
    GCObject *grayagain = gs->grayagain;
    gs->grayagain = NULL;
    cs_assert(gs->weak == NULL); /* 'weak' unused */
    cs_assert(!iswhite(gs->mainthread)); /* mainthread must be marked */
    gs->gcstate = GCSatomic;
    markobject(gs, C); /* mark running thread */
    markvalue(gs, &gs->c_registry); /* mark registry */
    work += propagateall(gs); /* traverse all gray objects */
    work += markopenupvalues(gs); /* mark open upvalues */
    work += propagateall(gs); /* propagate changes */
    cs_assert(gs->graylist == NULL); /* all must be propagated */
    gs->graylist = grayagain; /* set 'grayagain' as the graylist */
    work += propagateall(gs); /* propagate gray objects from 'grayagain' */
    /* separate and 'resurrect' unreachable objects with the finalizer... */
    separatetobefin(gs, 0);
    work += marktobefin(gs); /* ...and mark them */
    work += propagateall(gs); /* propagate changes */
    csS_clearcache(gs);
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
    c_smem threshold, debt;
    int pause = getgcparam(gs->gcpause);
    c_smem estimate = gs->gcestimate / PAUSEADJ; /* adjust estimate */
    cs_assert(estimate > 0);
    threshold = (pause < MAXSMEM / estimate) /* can fit ? */
              ? estimate * pause /* yes */
              : MAXSMEM; /* no, overflows; truncate to maximum */
    /* debt = totalbytes - ((gcestimate/100)*pause) */
    debt = gettotalbytes(gs) - threshold;
    if (debt > 0) debt = 0;
    csG_setgcdebt(gs, debt);
}


/* restart GState, mark roots and leftover 'tobefin' objects */
static void restartgc(GState *gs) {
    cleargraylists(gs);
    markobject(gs, gs->mainthread); /* mark mainthread */
    markvalue(gs, &gs->c_registry); /* mark registry */
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
** GCSsweepend indicates end of the sweep phase.
** GCScallfin calls finalizers of all the objects
** in 'tobefin' and puts them back into 'objects'
** list after the call.
*/
static c_mem singlestep(cs_State *C) {
    c_mem work;
    GState *gs = G(C);
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
                work = propagate(gs); /* traverse gray objects */
            }
            break;
        }
        case GCSenteratomic: { /* remark */
            work = atomic(C);
            entersweep(C);
            gs->gcestimate = gettotalbytes(gs); /* first estimate */
            break;
        }
        case GCSsweepall: { /* sweep objects */
            work = sweepstep(C, &gs->fin, GCSsweepfin);
            break;
        }
        case GCSsweepfin: { /* sweep objects with finalizers */
            work = sweepstep(C, &gs->tobefin, GCSsweeptofin);
            break;
        }
        case GCSsweeptofin: { /* sweep objects to be finalized */
            work = sweepstep(C, NULL, GCSsweepend);
            break;
        }
        case GCSsweepend: { /* finish sweeps */
            checksizes(C, gs);
            gs->gcstate = GCScallfin;
            work = 0;
            break;
        }
        case GCScallfin: { /* call remaining finalizers */
            if (gs->tobefin && !gs->gcemergency) {
                gs->gcstopem = 0; /* enable collections during finalizers */
                work = runNfinalizers(C, GCFINMAX) * GCFINCOST;
            } else { /* emergency or no more finalizers */
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
c_sinline void freelist(cs_State *C, GCObject *l, GCObject *limit) {
    while (l != limit) {
        GCObject *next = l->next;
        freeobject(C, l);
        l = next;
    }
}


static void runallfinalizers(cs_State *C) {
    GState *gs = G(C);
    while (gs->tobefin)
        callgc(C);
}


/*
** Free all objects except main thread, additionally
** call all finalizers.
*/
void csG_freeallobjects(cs_State *C) {
    GState *gs = G(C);
    gs->gcstop = GCSTPCLS; /* paused by state closing */
    separatetobefin(gs, 1); /* seperate all objects with a finalizer... */
    cs_assert(gs->fin == NULL);
    runallfinalizers(C); /* ...and run them */
    freelist(C, gs->objects, obj2gco(gs->mainthread));
    cs_assert(gs->fin == NULL); /* no new finalizers */
    freelist(C, gs->fixed, NULL); /* collect fixed objects */
    cs_assert(gs->strtab.nuse == 0);
}


/*
** Advances the garbage collector until it reaches a state
** allowed by 'statemask'.
*/
void csG_rununtilstate(cs_State *C, int statemask) {
    GState *gs = G(C);
    while (!testbit(statemask, gs->gcstate))
        singlestep(C);
}


/*
** Run collector until gcdebt is less than a stepsize
** or the full cycle was done (GState state is GCSpause).
** Both the gcdebt and stepsize are converted to 'work',
*/
static void step(cs_State *C, GState *gs) {
    int stepmul = (getgcparam(gs->gcstepmul) | 1); /* avoid division by 0 */
    c_smem debt = (gs->gcdebt / WORK2MEM) * stepmul;
    c_smem stepsize = (gs->gcstepsize <= sizeof(c_smem) * 8 - 2 /* fits ? */
                    ? ((cast_mem(1) << gs->gcstepsize) / WORK2MEM) * stepmul
                    : MAXSMEM); /* overflows; keep maximum value */
    do { /* do until pause or enough negative debt */
        c_mem work = singlestep(C); /* perform one step */
        debt -= work;
    } while (debt > -stepsize && gs->gcstate != GCSpause);
    if (gs->gcstate == GCSpause) { /* pause? */
        setpause(gs); /* pause until next cycle */
    } else { /* otherwise enough debt collected */
        debt = (debt / stepmul) * WORK2MEM; /* convert 'work' to bytes... */
        csG_setgcdebt(gs, debt); /* ...and set the new debt */
    }
}


void csG_step(cs_State *C) {
    GState *gs = G(C);
    if (!gcrunning(gs)) /* stopped ? */
        csG_setgcdebt(gs, -2000);
    else
        step(C, gs);
}


static void fullcycle(cs_State *C, GState *gs) {
    if (keepinvariant(gs)) /* already have black objects? */
        entersweep(C); /* if so sweep them first to turn them back to white */
    /* finish any pending sweep phase to start a new cycle */
    csG_rununtilstate(C, bitmask(GCSpause));
    csG_rununtilstate(C, bitmask(GCSpropagate)); /* start a new cycle */
    gs->gcstate = GCSenteratomic; /* go straight to atomic phase */
    csG_rununtilstate(C, bitmask(GCScallfin)); /* run up to finalizers */
    /* estimate must be correct after full GC cycle */
    cs_assert(gs->gcestimate == gettotalbytes(gs));
    csG_rununtilstate(C, bitmask(GCSpause)); /* finish collection */
    setpause(gs);
}


void csG_full(cs_State *C, int isemergency) {
    GState *gs = G(C);
    cs_assert(!gs->gcemergency);
    gs->gcemergency = isemergency;
    fullcycle(C, G(C));
    gs->gcemergency = 0;
}


/* traverse a list making all its elements white */
static void whitelist (GState *gs, GCObject *l) {
    int white = csG_white(gs);
    for (; l != NULL; l = l->next)
        l->mark = cast_sbyte((l->mark & ~maskgcbits) | white);
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
void csG_incmode(cs_State *C) {
    enterinc(G(C));
}
