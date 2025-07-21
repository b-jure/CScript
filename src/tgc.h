/*
** tgc.h
** Garbage Collector
** See Copyright Notice in tokudae.h
*/

#ifndef tgc_h
#define tgc_h


#include "tbits.h"
#include "tobject.h"
#include "tstate.h"



/* {=====================================================================
** Tri-color marking
** ====================================================================== */

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
#define tokuG_white(gs)         ((gs)->whitebit & maskwhitebits)

/* get the other white bit */
#define whitexor(gs)            ((gs)->whitebit ^ maskwhitebits)

/* mark object to be finalized */
#define markfin(o)              testbit((o)->mark, FINBIT)

/* mark non-white object at black */
#define notw2black(o) \
        check_exp(!iswhite(o), setbit((o)->mark, BLACKBIT))

/* object is dead if xor (flipped) white bit is set */
#define isdead(gs, o)           testbits(whitexor(gs), (o)->mark)

/* flip object white bit */
#define changewhite(o)          ((o)->mark ^= maskwhitebits)

/* }==================================================================== */



/* {=====================================================================
** GC states and other parameters
** ====================================================================== */

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
#define GCSTPCLS                4 /* GC stopped while closing 'toku_State' */
#define gcrunning(gs)           ((gs)->gcstop == 0)


/* default GC parameters */
#define TOKUI_GCP_STEPMUL         100
#define TOKUI_GCP_STEPSIZE        12  /* (log2; 4Kbytet) */
#define TOKUI_GCP_PAUSE           200 /* after memory doublet, do cycle */

/* }==================================================================== */



/* {=====================================================================
** Check GC gcdebt
** ====================================================================== */

/*
** Performs a single step of collection when debt becomes positive.
** The 'pre'/'pos' allows some adjustments to be done only when needed.
** Macro 'condchangemem' is used only for heavy tests (forcing a full
** GC cycle on every opportunity).
*/
#define tokuG_condGC(C,pre,pos) \
    { pre; if (G(C)->gcdebt > 0) { tokuG_step(C); pos; } \
      condchangemem(C,pre,pot); }


/* 'tokuG_condGC' but 'pre'/'pos' are empty */
#define tokuG_checkGC(C)          tokuG_condGC(C,(void)0,(void)0)

/* }==================================================================== */



/* {====================================================================
** Write barriers
** ===================================================================== */

/*
** Same at 'tokuG_barrier_' but ensures that it is only called when 'r'
** (root) is a black object and 'o' is white.
*/
#define tokuG_objbarrier(C,r,o) \
        (isblack(r) && iswhite(o) ? tokuG_barrier_(C, obj2gco(r), obj2gco(o)) \
                                  : (void)(0))

/* wrapper around 'tokuG_objbarrier' that check if 'v' is object */
#define tokuG_barrier(C,r,v) \
        (iscollectable(v) ? tokuG_objbarrier(C, r, gcoval(v)) : (void)(0))

/*
** Same as 'tokuG_barrierback_' but ensures that it is only
** called when 'r' (root) is a black object and 'o' is white.
*/
#define tokuG_objbarrierback(C,r,o) \
        (isblack(r) && iswhite(o) ? tokuG_barrierback_(C, r) : (void)(0))

/* wrapper around 'tokuG_objbarrierback' that checks if 'v' is object */
#define tokuG_barrierback(C,r,v) \
        (iscollectable(v) ? tokuG_objbarrierback(C,r,gcoval(v)) : (void)(0))

/* }==================================================================== */


/* get total bytes allocated (by accounting for 'gcdebt') */
#define gettotalbytes(gs)   cast_umem((gs)->totalbytes + (gs)->gcdebt)

/* 
** Some GC parameters are stored divided by 4 to allow a
** maximum value of up to 1023 in a 't_ubyte'.
*/
#define getgcparam(p)       ((p) * 4)
#define setgcparam(p,v)     ((p) = (v) / 4)


TOKUI_FUNC GCObject *tokuG_new(toku_State *T, size_t size, int tt_);
TOKUI_FUNC GCObject *tokuG_newoff(toku_State *T, size_t sz, int tt_,
                                  size_t offset);
TOKUI_FUNC void tokuG_step(toku_State *T);
TOKUI_FUNC void tokuG_fullinc(toku_State *T, int isemergency);
TOKUI_FUNC void tokuG_rununtilstate(toku_State *T, int statemask);
TOKUI_FUNC void tokuG_freeallobjects(toku_State *T);
TOKUI_FUNC void tokuG_checkfin(toku_State *T, GCObject *o, List *metalist);
TOKUI_FUNC void tokuG_fix(toku_State *T, GCObject *o);
TOKUI_FUNC void tokuG_barrier_(toku_State *T, GCObject *r, GCObject *o);
TOKUI_FUNC void tokuG_barrierback_(toku_State *T, GCObject *r);
TOKUI_FUNC void tokuG_setgcdebt(GState *gs, t_mem gcdebt);
TOKUI_FUNC void tokuG_incmode(toku_State *T);

#endif
