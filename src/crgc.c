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

#include "crgc.h"
#include "crconf.h"
#include "crdebug.h"
#include "crlimits.h"
#include "crobject.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crvm.h"


/* mark object as current white */
#define markwhite(gc,o) \
	(rawomark(o) = (rawomark(o) & ~COLORBITS) | cr_gc_white(gc))

/* mark object as black */
#define markblack(o) \
	(rawomark(o) = (rawomark(o) & ~COLORBITS) | BLACKBIT)

/* mark object as gray */
#define markgray(o)	resetbits(rawomark(o), COLORBITS)



/* check if 'TValue' is object and white */
#define valiswhite(v)		(ttiso(v) && iswhite(ovalue(v)))

/* check if 'HTable' key is object and white */
#define keyiswhite(n)		(keyisobj(n) && iswhite(keyovalue(n)))



/* 'markobject_' but only if 'v' is object and white */
#define markvalue(gc,v) \
	(valiswhite(v) ? markobject_(gc, ovalue(v)) : (void)0)

/* 'markobject_' but only if 'o' is non-NULL */
#define markobjectcheck(gc,o)	((o) ? markobject_(gc, obj2gco(o)) : (void)0)

/* 'markobject_' but only if object is white */
#define markobject(gc,o)	(iswhite(o) ? markobject_(gc, obj2gco(o)) : (void)0)

/* 'markobject' but only if key value is object and white */
#define markkey(gc, n) \
	(keyiswhite(n) ? markobject_(gc, keyovalue(n)) : (void)0)



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


/* adjust 'pause' (same as in Lua) */
#define PAUSEADJ	100




/* forward declare */
static void markobject_(GC *gc, GCObject *o);



void cr_gc_init(GC *gc)
{
	gc->next = 0;
	gc->allocated = 0;
	gc->debt = 0;
	/* 'total' is set when creating state */
	/* 'estimate' set on each cycle */
	gc->objects = NULL;
	gc->sweeppos = NULL;
	gc->graylist = gc->grayagain = gc->weak = NULL;
	gc->fixed = gc->fin = gc->tobefin = NULL;
	setgcparam(gc->pause, CRI_GCPAUSE);
	setgcparam(gc->stepmul, CRI_GCSTEPMUL);
	setgcparam(gc->stepsize, CRI_GCSTEPSIZE);
	gc->state = GCSpause;
	gc->stopem = 0;
	gc->stopped = GCSTP; /* disable while initializing */
	gc->whitebit = bitmask(WHITEBIT0);
	gc->isem = 0;
	gc->stopem = 0;
}


void cr_gc_fix(cr_State *ts, GCObject *o)
{
	GC *gc;

	gc = &GS(ts)->gc;
	cr_assert(o == gc->objects); /* first in the list */
	markgray(o);
	gc->objects = o->next;
	o->next = gc->fixed;
	gc->fixed = o;
}


/* set collector debt */
void cr_gc_setdebt(GC *gc, cr_mem debt)
{
	cr_mem total;

	total = totalbytes(gc);
	if (debt < total - CRMEM_MAX) /* 'total' will underflow ? */
		debt = total - CRMEM_MAX;
	gc->total = total - debt;
	gc->debt = debt;
}


static void linkgclist_(GCObject *o, GCObject **gclist, GCObject **list)
{
	cr_assert(!isgray(o));
	*gclist = *list;
	*list = o;
	markgray(o);
}


static GCObject **getgclist(GCObject *o)
{
	UserData *ud;

	switch (rawott(o)) {
	case CR_VFUNCTION: return &gco2fn(o)->gclist;
	case CR_VCRCL: return &gco2crcl(o)->gclist;
	case CR_VCCL: return &gco2ccl(o)->gclist;
	case CR_VCLASS: return &gco2cls(o)->gclist;
	case CR_VHTABLE: return &gco2ht(o)->gclist;
	case CR_VTHREAD: return &gco2th(o)->gclist;
	case CR_VUDATA: 
		ud = gco2ud(o);
		cr_assert(ud->nuv > 0 || !ud->vtempty);
		return &gco2ud(o)->gclist;
	default: cr_unreachable(); break;
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
void cr_gc_barrierforward_(cr_State *ts, GCObject *r, GCObject *o)
{
	GC *gc;

	gc = &GS(ts)->gc;
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
void cr_gc_barrierback_(cr_State *ts, GCObject *r)
{
	GC *gc;

	gc = &GS(ts)->gc;
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
static void markobject_(GC *gc, GCObject *o)
{
	UValue *uv;
	UserData *ud;
	Instance *ins;
	InstanceMethod *im;

	cr_assert(iswhite(o));
	switch(rawott(o)) {
	case CR_VSTRING:
		markblack(o);
		break;
	case CR_VUVALUE:
		uv = gco2uv(o);
		if (uvisopen(uv)) markgray(uv);
		else markblack(uv);
		markvalue(gc, uv->v.location);
		break;
	case CR_VINSTANCE:
		ins = gco2ins(o);
		markblack(ins);
		markobject(gc, ins->oclass);
		markobjectcheck(gc, ins->fields);
		break;
	case CR_VMETHOD:
		im = gco2im(o);
		markblack(im);
		markobject(gc, im->receiver);
		markobject(gc, im->method);
		break;
	case CR_VUDATA: 
		ud = gco2ud(o);
		if (ud->nuv == 0 && ud->vmtempty) {
			markblack(ud);
			break;
		} /* FALLTHRU */
	case CR_VHTABLE: case CR_VFUNCTION: case CR_VCRCL:
	case CR_VCCL: case CR_VCLASS: case CR_VTHREAD:
		linkobjgclist(o, gc->graylist);
		break;
	default: cr_unreachable(); break;
	}
}


/* mark 'VMT' */
cr_sinline cr_mem markvmt(GC *gc, VMT vmt)
{
	for (int i = 0; i < CR_NUMM; i++)
		if (!ttisnil(&vmt[i])) 
			markvalue(gc, &vmt[i]);
	return CR_NUMM;
}


/* mark 'HTable' slots */
static cr_mem markhtable(GC *gc, HTable *ht)
{
	Node *n;
	Node *last;

	if (ht->isweak) { /* weak table ? */
		linkgclist(ht, gc->weak);
	} else { /* otherwise strong */
		last = htlastnode(ht);
		for (n = htfirstnode(ht); n <= last; n++) {
			if (!keyisempty(n)) {
				markkey(gc, n);
				markvalue(gc, htnodevalue(n));
			}
		}
	}
	return 1 + (htsize(ht) << 1);
}


/* mark 'Function' */
static cr_mem markfunction(GC *gs, Function *fn)
{
	int i;

	markobjectcheck(gs, fn->name);
	markobjectcheck(gs, fn->source);
	for (i = 0; i < fn->fns.len; i++)
		markobject(gs, &fn->fns.ptr[i]);
	for (i = 0; i < fn->constants.len; i++)
		markvalue(gs, &fn->constants.ptr[i]);
	for (i = 0; i < fn->lvars.len; i++)
		markobjectcheck(gs, fn->lvars.ptr[i].name);
	for (i = 0; i < fn->upvalues.len; i++)
		markobjectcheck(gs, fn->upvalues.ptr[i].name);
	return 1 + fn->fns.len + fn->constants.len + fn->lvars.len +
			fn->upvalues.len;
}


/* mark 'CClosure' */
static cr_mem markcclosure(GC *gc, CClosure *ccl)
{
	int i;

	for (i = 0; i < ccl->nupvalues; i++)
		markvalue(gc, &ccl->upvalue[i]);
	return 1 + ccl->nupvalues;
}


/* mark 'CriptClosure' */
static cr_mem markcriptclosure(GC *gc, CriptClosure *crcl)
{
	int i;

	markobjectcheck(gc, crcl->fn);
	for (i = 0; i < crcl->nupvalues; i++)
		markobjectcheck(gc, &crcl->upvalue[i]);
	return 1 + crcl->nupvalues;
}


/* mark 'OClass' */
static cr_mem markclass(GC *gc, OClass *cls)
{
	markobjectcheck(gc, cls->name);
	markobjectcheck(gc, cls->methods);
	return 1 + markvmt(gc, cls->vtable);
}


/* mark 'UserData' */
static cr_mem markuserdata(GC *gc, UserData *ud)
{
	int i;
	cr_mem extra;

	extra = 0;
	if (!ud->vmtempty)
		extra = markvmt(gc, ud->vtable);
	for (i = 0; i < ud->nuv; i++)
		markobjectcheck(gc, &ud->uv[i]);
	return 1 + ud->nuv + extra;
}


/*
 * Marks thread (per-thread-state).
 * Threads do not use write barriers, because using
 * a write barrier correctly on each thread modification
 * would introduce a lot of overhead and complexity.
 * Using no write barriers for such a huge object also 
 * improves robustness and consistency.
 * And the way we deal with properly remarking the
 * thread is by linking it into the 'grayagain', a list
 * which is again traversed in 'GCSatomic' state.
 * Marking (traversing) the thread black only occurs in
 * either 'GCSpropagate' or 'GCSatomic' state and between
 * those two states only in 'GCSpropagate' can the objects
 * get modified.
 * So if we are in 'GCSpropagate' we link the object into
 * 'grayagain' and 'GCSatomic' state remarks our thread,
 * restoring the invariant state (in cases where the thread
 * really did get modified after we marked it black) without
 * using write barriers.
 */
static cr_mem markthread(GState *gs, cr_State *ts)
{
	UValue *uv;
	SPtr sp;
	GC *gc;
	int i;

	gc = &gs->gc;
	cr_assert(gc->state & (GCSpropagate | GCSatomic));
	sp = ts->stack.p;
	if (gc->state == GCSpropagate)
		linkgclist(ts, gc->grayagain);
	if (sp == NULL) /* thread not fully initialized ? */
		return 1;
	for (; sp < ts->stacktop.p; sp++)
		markvalue(gc, s2v(sp));
	for (uv = ts->openuv; uv; uv = ts->openuv->u.open.next)
		markobject(gc, uv);
	/* 'markopenupvalues' might of removed thread from 'thwouv' list */
	if (gc->state == GCSatomic && !isinthwouv(ts) && ts->openuv != NULL) {
		ts->thwouv = gs->thwouv;
		gs->thwouv = ts;
	}
	if (!gc->isem)
		cr_vm_shrinkstack(ts);
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
static cr_mem markopenupvalues(GState *gs)
{
	cr_State *th;
	cr_State **pp;
	UValue *uv;
	int work;

	work = 0;
	pp = &gs->thwouv;
	while ((th = *pp) != NULL) {
		work++;
		if (iswhite(th) || th->openuv == NULL) {
			*pp = th->thwouv;
			th->thwouv = th;
			uv = th->openuv;
			for (; uv; uv = uv->u.open.next) {
				work++;
				/* if visited then keep values alive */
				if (!iswhite(uv)) {
					cr_assert(uvisopen(uv) && isgray(uv));
					markvalue(&gs->gc, uv->v.location);
				}
			}
		} else {
			pp = &th->thwouv;
		}
	}
	return work;
}


/* traverse a single gray object turning it black */
static cr_mem propagate(GState *gs)
{
	GCObject *o;

	o = gs->gc.graylist;
	markblack(o);
	gs->gc.graylist = *getgclist(o);
	switch(rawott(o)) {
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
static cr_mem propagateall(GState *gs)
{
	cr_mem work;

	for (work = 0; gs->gc.graylist; work += propagate(gs));
	return work;
}



/* --------------------------------------------------------------------------
 * Free objects
 * -------------------------------------------------------------------------- */

/*
 * Performs raw deallocation of object memory it does
 * not try to call '__free__'.
 */
static void freeobject(cr_State *ts, GCObject *o)
{
	Instance *ins;
	Function *fn;
	OClass *cls;
	UValue *upval;

	switch (rawott(o)) {
		case CR_VSTRING:
			cr_mem_free(ts, o, sizes((OString*)o));
			break;
		case CR_VFUNCTION:
			fn = gco2fn(o);
			cr_mem_freevec(ts, &fn->fns);
			cr_mem_freevec(ts, &fn->constants);
			cr_mem_freevec(ts, &fn->code);
			cr_mem_freevec(ts, &fn->lineinfo);
			cr_mem_freevec(ts, &fn->lvars);
			cr_mem_freevec(ts, &fn->upvalues);
			cr_mem_free(ts, o, sizeof(Function));
			break;
		case CR_VUVALUE:
			upval = gco2uv(o);
			if (uvisopen(upval))
				cr_vm_unlinkupval(upval);
			cr_mem_free(ts, o, sizeof(UValue));
			break;
		case CR_VCRCL:
			cr_mem_free(ts, o, sizecrcl(gco2crcl(o)));
			break;
		case CR_VCCL:
			cr_mem_free(ts, o, sizeccl(gco2ccl(o)));
			break;
		case CR_VCLASS:
			cls = cast(OClass*, o);
			cr_htable_free(ts, cls->methods);
			cr_mem_free(ts, o, sizeof(OClass));
			break;
		case CR_VINSTANCE:
			ins = cast(Instance*, o);
			cr_htable_free(ts, ins->fields);
			cr_mem_free(ts, o, sizeof(Instance));
			break;
		case CR_VMETHOD:
			cr_mem_free(ts, o, sizeof(InstanceMethod));
			break;
		default:
			cr_unreachable();
	}
}



/* ------------------------------------------------------------------------
 * Sweep functions
 * ------------------------------------------------------------------------- */


static GCObject **sweeplist(cr_State *ts, GCObject **l, int nobjects, int *nsweeped)
{
	GCObject *o;
	GState *gs;
	int whitexor;
	int white; /* current white */
	int i;
	cr_ubyte mark;

	cr_assert(nobjects > 0);
	gs = GS(ts);
	white = cr_gc_white(&gs->gc);
	whitexor = whitexor(&gs->gc);
	for (i = 0; i < nobjects && *l; i++) {
		o = *l;
		mark = rawomark(o);
		if (whitexor & mark) { /* collect ? */
			*l = o->next;
			freeobject(ts, o);
		} else { /* mark white */
			rawomark(o) = cast_ubyte((mark & ~COLORBITS) | white);
			l = &o->next;
		}
	}
	if (nsweeped) *nsweeped = i;
	return (*l ? l : NULL);
}


/* do a single sweep step limited by 'GCSWEEPMAX' */
static int sweepstep(cr_State *ts, GCObject **nextlist, int nextstate)
{
	GC *gc;
	int cnt;

	gc = &GS(ts)->gc;
	if (gc->sweeppos) {
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
static GCObject **sweepuntilalive(cr_State *ts, GCObject **list)
{
	GCObject **pp;

	pp = list;
	do {
		list = sweeplist(ts, list, 1, NULL);
	} while (pp == list);
	return list;
}


static void entersweep(cr_State *ts)
{
	GC *gc;

	gc = &GS(ts)->gc;
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
static GCObject *gettobefin(GC *gc)
{
	GCObject *o;

	o = gc->tobefin;
	cr_assert(o && isfin(o));
	gc->tobefin = o->next;
	o->next = gc->objects;
	gc->objects = o;
	if (sweepstate(gc))
		markwhite(gc, o);
	return o;
}


/* protected finalizer */
static void protectedfinalizer(cr_State *ts, void *userdata)
{
	UNUSED(userdata);
	cr_vm_call(ts, ts->stacktop.p - 2, 0);
}


/* call a finalizer */
static void callfin(cr_State *ts)
{
	GC *gc;
	TValue v;
	const TValue *m;
	ptrdiff_t oldtop;
	int status;
	cr_ubyte oldstopped;

	cr_assert(amount > 0);
	gc = &GS(ts)->gc;
	setv2gco(ts, &v, gettobefin(gc));
	if ((m = cr_vtable_get(ts, &v, CR_MGC))) {
		oldstopped = gc->stopped;
		gc->stopped = GCSTP; /* prevent recursive GC calls */
		setsv(ts, ts->stacktop.p++, m);
		setsv(ts, ts->stacktop.p++, &v);
		oldtop = savestack(ts, ts->stacktop.p - 2);
		ts->aframe->cfstatus |= CFST_FIN;
		status = cr_vm_pcall(ts, protectedfinalizer, NULL, oldtop);
		ts->aframe->cfstatus &= ~CFST_FIN;
		gc->stopped = oldstopped;
		if (cr_unlikely(status != CR_OK)) {
			cr_debug_warnerror(ts, "__gc__");
			ts->stacktop.p--; /* pop err object */
		}
	}
}


/* call objects with finalizer in 'tobefin' */
static int callNfinalizers(cr_State *ts, int n)
{
	GC *gc;
	int i;
	
	gc = &GS(ts)->gc;
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
void cr_gc_checkfin(cr_State *ts, GCObject *o, VMT vtable) 
{
	GC *gc;
	GCObject **pp;

	gc = &GS(ts)->gc;
	if (isfin(o) || ttisnil(&vtable[CR_MGC]) || (gc->stopped & GCSTPCLS))
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


/* auxiliary to 'clearkeys' */
cr_sinline int keyisunmarked(GState *gs, GCObject *o)
{
	if (o == NULL) return 0;
	return iswhite(o);
}


/* clear all unmarked keys in weak table list 'l' */
static void clearkeys(GState *gs, GCObject *l)
{
	HTable *ht;
	Node *n;
	Node *limit;

	for (; l != NULL; l = gco2ht(l)->gclist) {
		ht = gco2ht(l);
		limit = htlastnode(ht);
		for (n = htfirstnode(ht); n <= limit; n++) {
			if (keyisunmarked(gs, keyobjN(n)))
				cr_htable_removedirect(ht, n);
		}
	}
}


/* 
 * Get the last 'next' object in list 'l' 
 * Useful used when trying to link objects
 * at the end of the list.
 */
cr_sinline GCObject **getlastnext(GCObject **l)
{
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
static void separatetobefin(GState *gs, int force)
{
	GC *gc;
	GCObject *curr;
	GCObject **finlp;
	GCObject **lastnext;

	gc = &gs->gc;
	finlp = &gc->fin;
	lastnext = getlastnext(&gc->tobefin);
	while ((curr = *finlp) != NULL) {
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


static cr_mem marktobefin(GState *gs)
{
	GCObject *o;
	cr_mem cnt;

	for (o = gs->gc.tobefin; o != NULL; o = o->next) {
		markobject(&gs->gc, o);
		cnt++;
	}
	return cnt;
}


static cr_mem atomic(cr_State *ts)
{
	GState *gs;
	GC *gc;
	GCObject *grayagain;
	cr_mem work;
	int i;
	
	gs = GS(ts);
	gc = &gs->gc;
	work = 0;
	grayagain = gc->grayagain;
	gc->grayagain = NULL;
	cr_assert(gc->weakhtab == NULL);
	gc->state = GCSatomic;
	markobject(gc, ts); /* mark running thread */
	markobject(gc, gs->gids); /* mark global id table */
	for (i = 0; i < gs->gvars.len; i++) /* mark global values */
		markvalue(gc, &gs->gvars.ptr[i]);
	work += i;
	work += propagateall(gs);
	/* mark open upvalues */
	work += markopenupvalues(gs);
	work += propagateall(gs);
	cr_assert(gc->graylist == NULL);
	gc->graylist = grayagain;
	work += propagateall(gs);
	/* all accessible objects are marked,
	 * safely clear weak tables */
	clearkeys(gs, gc->weak);
	/* separate and 'resurrect' unreachable
	 * objects with the finalizer */
	separatetobefin(gs, 0);
	work += marktobefin(gs);
	work += propagateall(gs);
	/* all 'resurrected' objects are marked,
	 * so clear weak tables safely again */
	clearkeys(gs, gc->weak); 
	gc->whitebit = whitexor(gc); /* flip white bit */
	cr_assert(gc->gray == NULL); /* all must be propagated */
	return work;
}


/* Set collector pause; basically called after
 * end of each full GC cycle. The new threshold
 * is calculated as 'estimate' / 'pause'.
 * 'PAUSEADJ' is there to provide more precise
 * control over when collection occurs, the value
 * is chosen by testing (Lua developers effort). */
static void setpause(GC *gc)
{
	cr_mem threshold;
	cr_mem estimate;
	cr_mem debt;
	int pause;

	pause = getgcparam(gc->pause);
	estimate = gc->estimate / PAUSEADJ;
	cr_assert(estimate > 0);
	threshold = (pause < CRMEM_MAX / estimate)  /* overflow ? */
			? estimate * pause  /* no overflow */
			: CRMEM_MAX;  /* overflow; use maximum */
	debt = totalbytes(gc) - threshold;
	if (debt > 0) debt = 0;
	cr_gc_setdebt(gc, debt);
}


/* restart GC, mark roots and leftover 'tobefin' objects */
static void restartgc(GState *gs)
{
	GC *gc;
	int i;

	gc = &gs->gc;
	gc->graylist = gc->grayagain = gc->weak = NULL;
	markobject(gc, gs->mainthread);
	markobject(gc, gs->gids);
	for (i = 0; i < gs->gvars.len; i++)
		markvalue(gc, &gs->gvars.ptr[i]);
	markopenupvalues(gs);
	/* there could be leftover unreachable objects
	 * with a finalizer from the previous cycle, in
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
static cr_mem singlestep(cr_State *ts)
{
	GState *gs;
	GC *gc;
	cr_mem work;

	gs = GS(ts);
	gc = &gs->gc;
	gc->stopem = 1; /* prevent emergency collections */
	switch (gc->state) {
	case GCSpause: /* mark roots */
		restartgc(gs);
		gc->state = GCSpropagate;
		work = 1;
		break;
	case GCSpropagate: /* gray -> black */
		if (gc->graylist) {
			work = propagate(gs);
		} else {
			gc->state = GCSenteratomic;
			work = 0;
		}
		break;
	case GCSenteratomic: /* remark */
		work = atomic(ts);
		entersweep(ts);
		break;
	case GCSsweepall:
		work = sweepstep(ts, &gc->fin, GCSsweepfin);
		break;
	case GCSsweepfin:
		work = sweepstep(ts, &gc->tobefin, GCSsweeptofin);
		break;
	case GCSsweeptofin:
		work = sweepstep(ts, NULL, GCSsweepend);
		break;
	case GCSsweepend:
		/* state not used for anything but clarity */
		gc->state = GCScallfin;
		work = 0;
		break;
	case GCScallfin: /* call finalizers */
		if (gc->tobefin && !gc->isem) {
			gc->stopem = 0; /* can collect in finalizer */
			work = callNfinalizers(ts, GCFINMAX) * GCFINCOST;
		} else {
			gc->state = GCSpause;
			work = 0;
		}
		break;
	default: 
		cr_unreachable(); 
		break;
	}
	gc->stopem = 0;
	return work;
}


/* free list 'l' objects until 'limit' */
cr_sinline void freelist(cr_State *ts, GCObject *l, GCObject *limit)
{
	GCObject *next;

	while (l != limit) {
		next = l->next;
		freeobject(ts, l);
		l = next;
	}
}


static void runallfinalizers(cr_State *ts)
{
	GC *gc;

	gc = &GS(ts)->gc;
	while (gc->tobefin)
		callfin(ts);
}


/* 
 * Free all objects except main thread, additionally
 * call all finalizers.
 */
void cr_gc_freeallobjects(cr_State *ts)
{
	GState *gs;
	GC *gc;

	gs = GS(ts);
	gc = &gs->gc;
	gc->stopped = GCSTPCLS; /* paused by state closing */
	separatetobefin(gs, 1);
	cr_assert(gc->fin == NULL);
	runallfinalizers(ts);
	freelist(ts, gc->objects, obj2gco(gs->mainthread));
	cr_assert(gc->fin == NULL);
	freelist(ts, gc->fixed, NULL);
}


/* run GC steps until 'state' is in any of the states of 'statemask' */
void cr_gc_rununtilstate(cr_State *ts, int statemask)
{
	GC *gc;

	gc = &GS(ts)->gc;
	while (!testbits(gc->state, statemask))
		singlestep(ts);
}


/*
 * Run collector until debt is less than a stepsize
 * or the full cycle was done (GC state is GCSpause).
 * Both the debt and stepsize are converted to 'work',
 */
static void step(cr_State *ts, GState *gs)
{
	GC *gc;
	cr_mem debt;
	cr_mem stepsize;
	int stepmul;

	gc = &gs->gc;
	stepmul = getgcparam(gc->stepmul) | 1;
	debt = (gc->debt / WORK2MEM) * stepmul;
	stepsize = (gc->stepsize <= sizeof(cr_mem) * 8 - 2 /* can fit in 'cr_mem' ? */
			? (cast_mem(1) << gc->stepsize) / WORK2MEM /* it can fit */
			: CRMEM_MAX); /* overflowed; return maximum possible value */
	do {
		debt -= singlestep(ts);
	} while (debt > -stepsize && gc->state != GCSpause);
	if (gc->state == GCSpause) {
		setpause(gc);
	} else {
		debt = (debt / stepmul) * WORK2MEM; /* convert back to bytes */
		cr_gc_setdebt(gc, debt);
	}
}


void cr_gc_step(cr_State *ts)
{
	GState *gs;
	GC *gc;

	gs = GS(ts);
	gc = &gs->gc;
	if (!gcrunning(gc)) /* stopped ? */
		cr_gc_setdebt(gc, -2000);
	else
		step(ts, gs);
}


static void fullcycle(cr_State *ts)
{
	GC *gc;

	gc = &GS(ts)->gc;
	if (invariantstate(gc)) /* already have black objects ? */
		entersweep(ts); /* if so sweep them first */
	cr_gc_rununtilstate(ts, bitmask(GCSpause)); /* restart collector */
	cr_gc_rununtilstate(ts, bitmask(GCScallfin)); /* run until finalizers */
	cr_assert(gc->estimate == totalbytes(gc)); /* end of cycle, check estimate */
	cr_gc_rununtilstate(ts, bitmask(GCSpause)); /* finish collection */
	setpause(gc);
}


void cr_gc_full(cr_State *ts, int isemergency)
{
	GC *gc;

	gc = &GS(ts)->gc;
	cr_assert(!gc->isem);
	gc->isem = isemergency;
	fullcycle(ts);
	gc->isem = 0;
}
