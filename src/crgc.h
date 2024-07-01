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

#include "crobject.h"


/* GCObject 'marked' bits */
#define BLACK		0 /* object and its children are reachable */
#define FIXED		1 /* object is always reachable */
#define FIN		2 /* object is to be finalized */


/* bit mask of 'marked' bits */
#define MMARKED		bit2mask(FIXED, bit2mask(BLACK, FIN))


/* set 'marked' bits */
#define markwhite(o)		resetbits(rawomark(o), (BLACK | FIXED))
#define markblack(o)		setbit(rawomark(o), BLACK)
#define markfixed(o)		setbit(rawomark(o), FIXED)


/* 'stopped' bits */
#define GCSTP		(1<<0) /* GC stopped by itself */
#define GCSTPUSR	(1<<1) /* GC stopped by user */
#define GCSTPCLS	(1<<2) /* GC stopped while closing 'TState' */
#define gcrunning(gc)	((gc)->stopped == 0)


/* default GC parameters */
#define GCSTEPMUL		100	/* 'stepmul' */
#define GCSTEPSIZE		14	/* 'stepsize' (log2) */


/* GC states */
#define GCSpropagate		0 /* propagating gray object to black */
#define GCSenteratomic		1 /* prepare for atomic state (propagate all) */
#define GCSatomic		2 /* everything is propagated and ready for sweep */
#define GCSsweepall		3 /* sweep all regular objects */
#define GCSsweepfin		4 /* sweep all objects with '__defer__' */
#define GCSsweeptofin		5 /* sweep all objects with finalizer */
#define GCSsweepend		6 /* end of sweep phase */
#define GCScallfin		7 /* call objects with finalizer */
#define GCSpause		8 /* starting state (marking roots) */


/*
 * Performs a single step of collection if collector
 * debt is positive.
 */
#define checkgc(ts,pre,pos) \
	{ pre; if ((ts)->gc.debt > 0) { cr_gc_step(ts); pos; } \
	  gcmemchange(ts, pre, pos); }


#define cr_gc_check(ts)		checkgc(ts, (void)0, (void)0)



/* garbage collector parameters and state */
typedef struct GC {
	cr_mem sizegs; /* size of 'graystack' */
	cr_mem ngs; /* number of elements in 'graystack' */
	cr_mem next; /* next byte threshold when GC triggers */
	cr_mem allocated; /* number of allocated bytes */
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


void cr_gc_init(GC *gcs);
size_t cr_gc_full(TState *ts);
size_t cf_gc_step(TState *ts);
void cr_gc_fix(TState *ts, GCObject *o);
void cr_gc_mark(TState *ts, Value *v);

#endif
