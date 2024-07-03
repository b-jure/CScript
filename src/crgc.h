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


/* object 'mark' bits (GC colors) */
#define WHITEBIT	0
#define BLACKBIT	1
#define FIXEDBIT	2
#define FINBIT		3

/* set 'mark' bits */
#define markwhite(o)	setbit(rawomark(o), WHITEBIT)
#define markgray(o)	resetbits(rawomark(o), (WHITEBIT | BLACKBIT))
#define markblack(o)	setbit(rawomark(o), BLACKBIT)
#define markfixed(o)	setbit(rawomark(o), FIXEDBIT)
#define markfin(o)	setbit(rawomark(o), FINBIT)

/* test 'mark' bits */
#define iswhite(o)	testbit(rawomark(o), WHITEBIT)
#define isgray(o)	testbits(rawomark(o), ~(WHITEBIT | BLACKBIT))
#define isblack(o)	testbit(rawomark(o), BLACKBIT)
#define isfixed(o)	testbit(rawomark(o), FIXEDBIT)
#define isfin(o)	testbit(rawomark(o), FINBIT)



/* GC 'stopped' bits */
#define GCSTP			(1<<0) /* GC stopped by itself */
#define GCSTPUSR		(1<<1) /* GC stopped by user */
#define GCSTPCLS		(1<<2) /* GC stopped while closing 'cr_State' */
#define gcrunning(gc)		((gc)->stopped == 0)



/* default GC parameters */
#define GCSTEPMUL		100 /* 'stepmul' */
#define GCSTEPSIZE		14  /* 'stepsize' (log2) */



/* GC states */
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
#define invariantstate(gc)	((gc).state <= GCSatomic)

/* check if GC is in a sweep state */
#define sweepstate(gc)		(GCSsweepall <= (gc).state && (gc).state <= GCSsweepend)



/*
 * Performs a single step of collection if collector
 * debt is positive.
 */
#define checkgc(ts,pre,pos) \
	{ pre; if ((ts)->gc.debt > 0) { cr_gc_step(ts); pos; } \
	  gcmemchange(ts, pre, pos); }


/* 'checkgc' but without 'pre' and 'pos' */
#define cr_gc_check(ts)		checkgc(ts, (void)0, (void)0)


/* 
 * 'cr_gc_barrier_' but ensures that it is only called
 * when 'r' (root) is a black object and 'o' is white.
 */
#define cr_gc_barrier(ts,r,o) \
	(isblack(r) && iswhite(o) ? \
	cr_gc_barrier(ts, objtogco(r), objtogco(o)) : (void)(0))



/* garbage collector parameters and state */
typedef struct GC {
	cr_mem sizegs; /* size of 'graystack' */
	cr_mem ngs; /* number of elements in 'graystack' */
	cr_mem next; /* next byte threshold when GC triggers */
	cr_mem allocated; /* number of allocated bytes ? REMOVE */
	cr_mem debt; /* memory unaccounted by collector */
	cr_mem total; /* total memory in use in bytes - 'debt' */
	cr_umem estimate; /* estimate of non-garbage memory in use */
	GCObject *objects; /* list of all GC objects */
	GCObject **sweeppos; /* current position of sweep in list */
	GCObject **graystack; /* stack of reachable objects */
	GCObject *fixed; /* list of fixed objects (not to be collected) */
	GCObject *fin; /* list of objects that have finalizer */
	GCObject *tobefin; /* list of objects to be finalized (pending) */
	cr_ubyte isem; /* true if this is emergency collection */
	cr_ubyte stopem; /* stop emergency collection */
	cr_ubyte stopped; /* collector is stopped bits */
	cr_ubyte state; /* GC state bits */
	cr_ubyte stepmul; /* GC heap grow speed */
	cr_ubyte stepsize; /* step size in bytes (log2) */
} GC;



void cr_gc_init(GC *gc);
size_t cr_gc_full(cr_State *ts, int isemergency);
size_t cf_gc_step(cr_State *ts);
void cr_gc_fix(cr_State *ts, GCObject *o);
void cr_gc_mark(cr_State *ts, Value *v);
void cr_gc_barrier_(cr_State *ts, GCObject *r, GCObject *o);

#endif
