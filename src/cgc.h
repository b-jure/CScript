/*
** cgc.h
** Garbage Collector
** See Copyright Notice in cscript.h
*/

#ifndef CRGC_H
#define CRGC_H


#include "cbits.h"
#include "cobject.h"
#include "cstate.h"



/* -------------------------------------------------------------------------
 * Tri-color marking
 * ------------------------------------------------------------------------- */

/* object 'mark' bits (GC colors) */
#define WHITEBIT0       0 /* object is white v0 */
#define WHITEBIT1       1 /* object is white v1 */
#define BLACKBIT        2 /* object is black */
#define FINBIT          3 /* object has finalizer */


/* mask of white bits */
#define maskwhitebits   bit2mask(WHITEBIT0, WHITEBIT1)

/* mask of bits used for coloring */
#define maskcolorbits   (maskwhitebits | bitmask(BLACKBIT))

/* mask of all GC bits */
#define maskgcbits      (maskcolorbits | maskwhitebits)


/* test 'mark' bits */
#define iswhite(o)      testbits((o)->mark, maskwhitebits)
#define isblack(o)      testbit((o)->mark, BLACKBIT)
#define isgray(o) /* neither white nor black */ \
        (!testbits((o)->mark, maskcolorbits))
#define isfin(o)        testbit((o)->mark, FINBIT)


/* get the current white bit */
#define csG_white(gs)           ((gs)->whitebit & maskwhitebits)

/* get the other white bit */
#define whitexor(gs)            ((gs)->whitebit ^ maskwhitebits)

/* mark object to be finalized */
#define markfin(o)              setbit((o)->mark, FINBIT)

/* mark non-white object as black */
#define notw2black(o) \
        check_exp(!iswhite(o), setbit((o)->mark, BLACKBIT))

/* object is dead if xor (flipped) white bit is set */
#define isdead(gs, o)           testbits(whitexor(gs), (o)->mark)

/* flip object white bit */
#define changewhite(o)          ((o)->mark ^= maskwhitebits)



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
#define invariantstate(gs)      ((gs)->gcstate <= GCSatomic)


/* check if GC is in a sweep state */
#define sweepstate(gs) \
        (GCSsweepall <= (gs)->gcstate && (gs)->gcstate <= GCSsweepend)


/* GC 'stopped' bits */
#define GCSTP                   (1<<0) /* GC stopped by itself */
#define GCSTPUSR                (1<<1) /* GC stopped by user */
#define GCSTPCLS                (1<<2) /* GC stopped while freeing 'cs_State' */
#define gcrunning(gs)           ((gs)->gcstop == 0)


/* default GC parameters */
#define CSI_GCSTEPMUL           100 /* 'gcstepmul' */
#define CSI_GCSTEPSIZE          13  /* 'gcstepsize' (log2) */
#define CSI_GCPAUSE             200 /* 'gcpause' after memory 2x do cycle */



/* -------------------------------------------------------------------------
 * Check GC gcdebt
 * ------------------------------------------------------------------------- */

/*
 ** Performs a single step of collection if collector
 ** gcdebt is positive.
 */
#define checkgc(ts,pre,pos) \
    { pre; if (G_(ts)->gcdebt > 0) { csG_step(ts); pos; } \
      gcmemchange(ts,pre,pos); }


/* 'checkgc' but without 'pre' and 'pos' */
#define csG_check(ts) \
    { printf("GC check %s\n", __func__); checkgc(ts,(void)0,(void)0); }



/* get total bytes allocated (by accounting for 'gcdebt') */
#define gettotalbytes(gs)      cast_umem((gs)->totalbytes + (gs)->gcdebt)



/* -------------------------------------------------------------------------
 * Write barriers
 * ------------------------------------------------------------------------- */

/*
 ** Same as 'csG_barrier_' but ensures that it is only
 ** called when 'r' (root) is a black object and 'o' is white.
 */
#define csG_objbarrier(ts,r,o) \
        (isblack(r) && iswhite(o) ? csG_barrier_(ts, obj2gco(r), obj2gco(o)) \
                                  : (void)(0))

/* wrapper around 'csG_objbarrier' that check if 'v' is object */
#define csG_barrier(ts,r,v) \
        (iscollectable(v) ? csG_objbarrier(ts, r, gcoval(v)) : (void)(0))

/*
** Same as 'csG_barrierback_' but ensures that it is only
** called when 'r' (root) is a black object and 'o' is white.
*/
#define csG_objbarrierback(ts,r,o) \
        (isblack(r) && iswhite(o) ? csG_barrierback_(ts, r) : (void)(0))

/* wrapper around 'csG_objbarrierback' that checks if 'v' is object */
#define csG_barrierback(ts,r,v) \
        (iscollectable(v) ? csG_objbarrierback(ts,r,gcoval(v)) : (void)(0))


/* 
** Some GC parameters are stored divided by 4 to allow a
** maximum value of up to 1023 in a 'cs_ubyte'.
*/
#define getgcparam(p)           ((p) * 4)
#define setgcparam(p,v)         ((p) = (v) / 4)


/* allocate new GC object */
#define csG_new(ts,s,tt,t)      cast(t *, csG_new_(ts, s, tt))


CSI_FUNC GCObject *csG_new_(cs_State *ts, size_t size, int tt_);
CSI_FUNC GCObject *csG_newoff(cs_State *ts, size_t sz, int tt_, size_t offset);
CSI_FUNC void csG_step(cs_State *ts);
CSI_FUNC void csG_full(cs_State *ts, int isemergency);
CSI_FUNC void csG_rununtilstate(cs_State *ts, int statemask);
CSI_FUNC void csG_freeallobjects(cs_State *ts);
CSI_FUNC void csG_checkfin(cs_State *ts, GCObject *o, TValue vmt[CS_MM_N]);
CSI_FUNC void csG_fix(cs_State *ts, GCObject *o);
CSI_FUNC void csG_barrier_(cs_State *ts, GCObject *r, GCObject *o);
CSI_FUNC void csG_barrierback_(cs_State *ts, GCObject *r);
CSI_FUNC void csG_setgcdebt(GState *gs, cs_mem gcdebt);
CSI_FUNC void csG_incmode(cs_State *ts);

#endif
