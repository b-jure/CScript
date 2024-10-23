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

#include "cbits.h"
#include "cobject.h"



/* -------------------------------------------------------------------------
 * Tri-color marking
 * ------------------------------------------------------------------------- */

/* object 'mark' bits (GC colors) */
#define WHITEBIT0       0 /* object is white v0 */
#define WHITEBIT1       1 /* object is white v1 */
#define BLACKBIT        2 /* object is black */
#define FINBIT          3 /* object has finalizer */

/* white bits */
#define WHITEBITS       bit2mask(WHITEBIT0, WHITEBIT1)

/* bits used for coloring */
#define COLORBITS       bit2mask(WHITEBITS, BLACKBIT)

/* test 'mark' bits */
#define iswhite(o)      testbits(gcomark_(o), WHITEBITS)
#define isgray(o)       (!testbits(gcomark_(o), COLORBITS))
#define isblack(o)      testbit(gcomark_(o), BLACKBIT)
#define isfin(o)        testbit(gcomark_(o), FINBIT)

/* get the current white bit */
#define crG_white(gc)           ((gc)->whitebit & WHITEBITS)

/* get the other white bit */
#define whitexor(gc)            ((gc)->whitebit ^ WHITEBITS)

/* mark object to be finalized */
#define markfin(o)              setbit(gcomark_(o), FINBIT)

/* mark non-white object as black */
#define notw2black(o)           setbit(gcomark_(o), BLACKBIT)


/* object is dead if xor (flipped) white bit is set */
#define isdead(gc, o)           testbits(whitexor(gc), gcomark_(o))

/* flip object white bit */
#define changewhite(o)          ((o)->mark ^= WHITEBITS)



/* -------------------------------------------------------------------------
 * GC states and other parameters
 * ------------------------------------------------------------------------- */

/* GC 'state' */
#define GCSpropagate            0 /* propagating gray object to black */
#define GCSenteratomic          1 /* enters atomic and then sweep state */
#define GCSatomic               2 /* propagates and re-marks objects */
#define GCSsweepall             3 /* sweep all regular objects */
#define GCSsweepfin             4 /* sweep all objects in 'fin' */
#define GCSsweeptofin           5 /* sweep all objects in 'tobefin' */
#define GCSsweepend             6 /* state after sweeping (unused) */
#define GCScallfin              7 /* call objects in 'tobefin' */
#define GCSpause                8 /* starting state (marking roots) */


/*
 * Check if GC is in a state that holds the invariant
 * that white objects cannot point to black objects.
 * States that break this invariant are sweep states.
 */
#define invariantstate(gc)      ((gc)->state <= GCSatomic)

/* check if GC is in a sweep state */
#define sweepstate(gc) \
    (GCSsweepall <= (gc)->state && (gc)->state <= GCSsweepend)


/* GC 'stopped' bits */
#define GCSTP                   (1<<0) /* GC stopped by itself */
#define GCSTPUSR                (1<<1) /* GC stopped by user */
#define GCSTPCLS                (1<<2) /* GC stopped while freeing 'cr_State' */
#define gcrunning(gc)           ((gc)->stopped == 0)


/* default GC parameters */
#define CRI_GCSTEPMUL           100 /* 'stepmul' */
#define CRI_GCSTEPSIZE          14  /* 'stepsize' (log2) */
#define CRI_GCPAUSE             200 /* after memory doubles begin cycle */



/* -------------------------------------------------------------------------
 * Check GC debt
 * ------------------------------------------------------------------------- */

/*
 ** Performs a single step of collection if collector
 ** debt is positive.
 */
#define checkgc(ts,pre,pos) \
    { pre; if (G_(ts)->gc.debt > 0) { crG_step(ts); pos; } \
      gcmemchange(ts,pre,pos); }


/* 'checkgc' but without 'pre' and 'pos' */
#define crG_check(ts)       checkgc(ts,(void)0,(void)0)



/* get total bytes allocated (by accounting for 'debt') */
#define totalbytes(gc)      cast_umem((gc)->total - (gc)->debt)



/* -------------------------------------------------------------------------
 * Write barriers
 * ------------------------------------------------------------------------- */

/*
 ** Same as 'crG_barrierforward_' but ensures that it is only
 ** called when 'r' (root) is a black object and 'o' is white.
 */
#define crG_objbarrier(ts,r,o) \
    (isblack(r) && iswhite(o) \
     ? crG_barrier_(ts, obj2gco(r), obj2gco(o)) \
     : (void)(0))

/* wrapper around 'crG_objbarrier' that check if ** 'v' is object */
#define crG_barrier(ts,r,v) \
    (iscollectable(v) ? crG_objbarrier(ts, r, gcoval(v)) : (void)(0))

/*
** Same as 'crG_barrierback_' but ensures that it is only
** called when 'r' (root) is a black object and 'o' is white.
*/
#define crG_objbarrierback(ts,r,o) \
    (isblack(r) && iswhite(o) \
     ? crG_barrierback_(ts, r) \
     : (void)(0))

/* wrapper around 'crG_objbarrierback' that checks if 'v' is object */
#define crG_barrierback(ts,r,v) \
    (iscollectable(v) ? crG_objbarrierback(ts,r,gcoval(v)) : (void)(0))


/* to allow a maximum value of up to 1023 in a 'cr_ubyte' */
#define getgcparam(p)           ((p) * 4)
#define setgcparam(p,v)         ((p) = (v) / 4)


/* allocate new GC object */
#define crG_new(ts,s,tt,t)      cast(t *, crG_new_(ts, s, tt))


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


CRI_FUNC void crG_init(GC *gc, cr_State *ts, size_t LGsize);
CRI_FUNC GCObject *crG_new_(cr_State *ts, size_t size, int tt_);
CRI_FUNC GCObject *crG_newoff(cr_State *ts, size_t sz, int tt_, size_t offset);
CRI_FUNC void crG_step(cr_State *ts);
CRI_FUNC void crG_full(cr_State *ts, int isemergency);
CRI_FUNC void crG_rununtilstate(cr_State *ts, int statemask);
CRI_FUNC void crG_freeallobjects(cr_State *ts);
CRI_FUNC void crG_checkfin(cr_State *ts, GCObject *o, TValue *vmt);
CRI_FUNC void crG_fix(cr_State *ts, GCObject *o);
CRI_FUNC void crG_barrier_(cr_State *ts, GCObject *r, GCObject *o);
CRI_FUNC void crG_barrierback_(cr_State *ts, GCObject *r);
CRI_FUNC void crG_setdebt(GC *gc, cr_mem debt);

#endif
