/*
** cgc.h
** Garbage Collector
** See Copyright Notice in cscript.h
*/

#ifndef CGC_H
#define CGC_H


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
** Macro to tell when main invariant (white objects cannot point to black
** objects) must be kept. During a collection, the sweep phase may break
** the invariant, as objects turned white may point to still-black
** objects. The invariant is restored when sweep ends and all objects
** are white again.
*/
#define keepinvariant(gs)       ((gs)->gcstate <= GCSatomic)


/* check if GC is in a sweep state */
#define issweepstate(gs) \
        (GCSsweepall <= (gs)->gcstate && (gs)->gcstate <= GCSsweepend)


/* GC 'stopped' bits */
#define GCSTP                   1 /* GC stopped by itself */
#define GCSTPUSR                2 /* GC stopped by user */
#define GCSTPCLS                4 /* GC stopped while freeing 'cs_State' */
#define gcrunning(gs)           ((gs)->gcstop == 0)


/* default GC parameters */
#define CSI_GCSTEPMUL           100 /* 'gcstepmul' */
#define CSI_GCSTEPSIZE          13  /* 'gcstepsize' (log2; 8KB) */
#define CSI_GCPAUSE             200 /* 'gcpause' after memory 2x do cycle */



/* -----------------------------------------------------------------------
** Check GC gcdebt
** ----------------------------------------------------------------------- */

/*
** Performs a single step of collection if collector
** gcdebt is positive.
*/
#define csG_condGC(C,pre,pos) \
    { pre; if (G(C)->gcdebt > 0) { csG_step(C); pos; } \
      condchangemem(C,pre,pos); }


/* 'csG_condGC' but without 'pre' and 'pos' */
#define csG_checkGC(C)         csG_condGC(C,(void)0,(void)0)


/* get total bytes allocated (by accounting for 'gcdebt') */
#define gettotalbytes(gs)       cast_mem((gs)->totalbytes + (gs)->gcdebt)



/* -----------------------------------------------------------------------
** Write barriers
** ----------------------------------------------------------------------- */

/*
** Same as 'csG_barrier_' but ensures that it is only called when 'r'
** (root) is a black object and 'o' is white.
*/
#define csG_objbarrier(C,r,o) \
        (isblack(r) && iswhite(o) ? csG_barrier_(C, obj2gco(r), obj2gco(o)) \
                                  : (void)(0))

/* wrapper around 'csG_objbarrier' that check if 'v' is object */
#define csG_barrier(C,r,v) \
        (iscollectable(v) ? csG_objbarrier(C, r, gcoval(v)) : (void)(0))

/*
** Same as 'csG_barrierback_' but ensures that it is only
** called when 'r' (root) is a black object and 'o' is white.
*/
#define csG_objbarrierback(C,r,o) \
        (isblack(r) && iswhite(o) ? csG_barrierback_(C, r) : (void)(0))

/* wrapper around 'csG_objbarrierback' that checks if 'v' is object */
#define csG_barrierback(C,r,v) \
        (iscollectable(v) ? csG_objbarrierback(C,r,gcoval(v)) : (void)(0))


/* 
** Some GC parameters are stored divided by 4 to allow a
** maximum value of up to 1023 in a 'c_byte'.
*/
#define getgcparam(p)           ((p) * 4)
#define setgcparam(p,v)         ((p) = (v) / 4)


CSI_FUNC GCObject *csG_new(cs_State *C, size_t size, int tt_);
CSI_FUNC GCObject *csG_newoff(cs_State *C, size_t sz, int tt_, size_t offset);
CSI_FUNC void csG_step(cs_State *C);
CSI_FUNC void csG_full(cs_State *C, int isemergency);
CSI_FUNC void csG_rununtilstate(cs_State *C, int statemask);
CSI_FUNC void csG_freeallobjects(cs_State *C);
CSI_FUNC void csG_checkfin(cs_State *C, GCObject *o, TValue vmt[CS_MM_N]);
CSI_FUNC void csG_fix(cs_State *C, GCObject *o);
CSI_FUNC void csG_barrier_(cs_State *C, GCObject *r, GCObject *o);
CSI_FUNC void csG_barrierback_(cs_State *C, GCObject *r);
CSI_FUNC void csG_setgcdebt(GState *gs, c_smem gcdebt);
CSI_FUNC void csG_incmode(cs_State *C);

#endif
