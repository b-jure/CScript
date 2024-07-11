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

#ifndef CRGC_H
#define CRGC_H

#include "crbits.h"
#include "crobject.h"



/* -------------------------------------------------------------------------
 * Tri-color marking
 * ------------------------------------------------------------------------- */

/* object 'mark' bits (GC colors) */
#define WHITEBIT0	0 /* object is white v0 */
#define WHITEBIT1	1 /* object is white v1 */
#define BLACKBIT	2 /* object is black */
#define FINBIT		3 /* object has finalizer */

/* white bits */
#define WHITEBITS	bit2mask(WHITEBIT0, WHITEBIT1)

/* bits used for coloring */
#define COLORBITS	bit2mask(WHITEBITS, BLACKBIT)

/* test 'mark' bits */
#define iswhite(o)	testbits(omark_(o), WHITEBITS)
#define isgray(o)	(!testbits(omark_(o), COLORBITS))
#define isblack(o)	testbit(omark_(o), BLACKBIT)
#define isfin(o)	testbit(omark_(o), FINBIT)

/* get the current white bit */
#define cr_gc_white(gc)		((gc)->whitebit & WHITEBITS)

/* get the other white bit (not the current one) */
#define whitexor(gc)		((gc)->whitebit ^ WHITEBITS)

/* mark object to be finalized */
#define markfin(o)		setbit(omark_(o), FINBIT)


/* object is dead if xor (flipped) white bit is set */
#define isdead(gc, o)		testbits(whitexor(gc), omark_(o))



/* -------------------------------------------------------------------------
 * GC states and other parameters
 * ------------------------------------------------------------------------- */

/* GC 'state' */
#define GCSpropagate		0 /* propagating gray object to black */
#define GCSenteratomic		1 /* enters atomic state and then moves to sweep phase */
#define GCSatomic		2 /* propagates and remarks necessary objects */
#define GCSsweepall		3 /* sweep all regular objects */
#define GCSsweepfin		4 /* sweep all objects in 'fin' */
#define GCSsweeptofin		5 /* sweep all objects in 'tobefin' */
#define GCSsweepend		6 /* state after sweeping */
#define GCScallfin		7 /* call objects in 'tobefin' */
#define GCSpause		8 /* starting state (marking roots) */


/* 
 * Check if GC is in a state that holds the invariant 
 * that white objects cannot point to black objects.
 * States that break this invariant are sweep states.
 */
#define invariantstate(gc)	((gc)->state <= GCSatomic)

/* check if GC is in a sweep state */
#define sweepstate(gc)		(GCSsweepall <= (gc)->state && (gc)->state <= GCSsweepend)


/* GC 'stopped' bits */
#define GCSTP			(1<<0) /* GC stopped by itself */
#define GCSTPUSR		(1<<1) /* GC stopped by user */
#define GCSTPCLS		(1<<2) /* GC stopped while closing 'cr_State' */
#define gcrunning(gc)		((gc)->stopped == 0)


/* default GC parameters */
#define CRI_GCSTEPMUL		100 /* 'stepmul' */
#define CRI_GCSTEPSIZE		14  /* 'stepsize' (log2) */
#define CRI_GCPAUSE		200 /* after memory doubles begin cycle */



/* -------------------------------------------------------------------------
 * Check GC debt
 * ------------------------------------------------------------------------- */

/*
 * Performs a single step of collection if collector
 * debt is positive.
 */
#define checkgc(ts,pre,pos) \
	{ pre; if ((ts)->gc.debt > 0) { cr_gc_step(ts); pos; } \
	  gcmemchange(ts,pre,pos); }


/* 'checkgc' but without 'pre' and 'pos' */
#define cr_gc_check(ts)		checkgc(ts,(void)0,(void)0)



/* get total bytes allocated (by accounting for 'debt') */
#define totalbytes(gc)		cast(cr_umem, ((gc)->total - (gc)->debt))



/* -------------------------------------------------------------------------
 * Write barriers
 * ------------------------------------------------------------------------- */

/* 
 * Same as 'cr_gc_barrierforward_' but ensures that it is only
 * called when 'r' (root) is a black object and 'o' is white.
 */
#define cr_gc_objbarrierforward(ts,r,o) \
	(isblack(r) && iswhite(o) ? \
	cr_gc_barrierforward_(ts,obj2gco(r),obj2gco(o)) : (void)(0))

/*
 * Wrapper around 'cr_gc_objbarrierforward' that ensures
 * 'v' (pointed to value) is object.
 */
#define cr_gc_barrierforward(ts,r,v) \
	(ttiso(v) ? cr_gc_objbarrierforward(ts,r,oval(v)) : (void)(0))

/*
 * Same as 'cr_gc_barrierback_' but ensures that it is only
 * called when 'r' (root) is a black object and 'o' is white.
 */
#define cr_gc_objbarrierback(ts,r,o) \
	(isblack(r) && iswhite(o) ? \
	cr_gc_barrierback_(ts,objtogco(r)) : (void)(0))

/*
 * Wrapper around 'cr_gc_objbarrierback' that ensures
 * 'v' (pointed to value) is object.
 */
#define cr_gc_barrierback(ts,r,v) \
	(ttiso(v) ? cr_gc_objbarrierback(ts,r,oval(v)) : (void)(0))



/* to allow a maximum value of up to 1023 in a 'cr_ubyte' */
#define getgcparam(p)		((p) << 2)
#define setgcparam(p,v)		((p) = (v) >> 2)


/* allocate new GC object */
#define cr_gc_new(ts,s,tt,t)	cast(t *, cr_gc_new_(ts, s, tt))



/* garbage collector parameters and state */
typedef struct GC {
	cr_mem next; /* next byte threshold when GC triggers */
	cr_mem allocated; /* number of allocated bytes ? REMOVE */
	cr_mem debt; /* memory unaccounted by collector */
	cr_mem total; /* total memory in use in bytes - 'debt' */
	cr_umem estimate; /* estimate of non-garbage memory in use */
	GCObject *objects; /* list of all GC objects */
	GCObject **sweeppos; /* current position of sweep in list */
	GCObject *graylist; /* list of gray objects */
	GCObject *grayagain; /* list of objects to be traversed atomically */
	GCObject *weak; /* list of all weak hashtables (key & value) */
	GCObject *fixed; /* list of fixed objects (not to be collected) */
	GCObject *fin; /* list of objects that have finalizer */
	GCObject *tobefin; /* list of objects to be finalized (pending) */
	cr_ubyte pause; /* how long to wait until next cycle */
	cr_ubyte stepmul; /* GC heap grow speed */
	cr_ubyte stepsize; /* step size in bytes (log2) */
	cr_ubyte state; /* GC state bits */
	cr_ubyte stopped; /* collector is stopped bits */
	cr_ubyte whitebit; /* current white bit (WHITEBIT0 or WHITEBIT1) */
	cr_ubyte isem; /* true if this is emergency collection */
	cr_ubyte stopem; /* stop emergency collection */
} GC;


void cr_gc_init(GC *gc);
GCObject *cr_gc_new_(cr_State *ts, size_t size, cr_ubyte ott);
void cf_gc_step(cr_State *ts);
void cr_gc_full(cr_State *ts, int isemergency);
void cr_gc_rununtilstate(cr_State *ts, int statemask);
void cr_gc_freeallobjects(cr_State *ts);
void cr_gc_checkfin(cr_State *ts, GCObject *o, VMT vtable);
void cr_gc_fix(cr_State *ts, GCObject *o);
void cr_gc_barrier_(cr_State *ts, GCObject *r, GCObject *o);
void cr_gc_barrierback_(cr_State *ts, GCObject *r);
void cr_gc_setdebt(GC *gc, cr_mem debt);

#endif
