/*
** cstate.h
** Global and Thread state
** See Copyright Notice in cscript.h
*/

#ifndef CRSTATE_H
#define CRSTATE_H


#include "cobject.h"
#include "cobject.h"

#include <setjmp.h>



/*
 * Extra stack space that is used mostly when calling vtable
 * methods. Helps in avoiding stack checks (branching).
 */
#define EXTRA_STACK	5


/* initial stack size */
#define INIT_STACKSIZE		(CS_MINSTACK * 4)


/* stack size */
#define stacksize(ts)		cast_int((ts)->stackend.p - (ts)->stack.p)

/* current stack top offset */
#define topoffset(ts)		cast_int((ts)->sp.p - (ts)->stack.p)


/* save/restore stack position */
#define savestack(ts,ptr)	(cast_charp(ptr) - cast_charp((ts)->stack.p))
#define restorestack(ts,o)	cast(SPtr, cast_charp((ts)->stack.p) + (o))


/* 
** Check if stack nees to grow if so, do 'pre' then grow and
** then do 'pos'.
*/
#define csT_checkstackaux(ts,n,pre,pos) \
    if (csi_unlikely((ts)->stackend.p - (ts)->sp.p <= (n))) \
        { pre; csT_growstack(ts, (n), 1); pos; }


/* check if stack needs to grow */
#define csT_checkstack(ts,n)    csT_checkstackaux(ts,n,(void)0,(void)0)


/* check if stack needs to grow and preserve 'p' */
#define checkstackp(ts,n,p) \
    csT_checkstackaux(ts, n, \
            ptrdiff_t p_ = savestack(ts, p), \
            p = restorestack(ts, p_))


/* check GC then check stack preserving 'p' */
#define checkstackGCp(ts,n,p) \
    csT_checkstackaux(ts,n, \
            ptrdiff_t p_ = savestack(ts,p); csG_check(ts), \
            p = restorestack(ts, p_))


/* check GC then check stack */
#define checkstackGC(ts,n) \
    csT_checkstackaux(ts,n,csG_check(ts),(void)0)
    


/* 
** Increment number of non-yieldable calls.
** The counter is located in the upper 2 bytes of 'nCcalls'.
*/
#define incnnyc(ts)       ((ts)->nCcalls += 0x10000)

/* Decrement number of nested non-yieldable calls. */
#define decnnyc(ts)       ((ts)->nCcalls -= 0x10000)


/*
** Get total number of C calls.
** This counter is located in the lower 2 bytes of 'nCcalls'.
*/
#define getCcalls(ts)       ((ts)->nCcalls & 0xffff)


/* non-yieldable and C calls increment */
#define nyci        (0x10000 | 1)



/* -------------------------------------------------------------------------
 * Long jump (for protected calls)
 * ------------------------------------------------------------------------- */

#define csi_jmpbuf          jmp_buf
#define CSI_THROW(ts,b)     longjmp((b)->buf, 1)
#define CSI_TRY(ts,b,fn)    if (setjmp((b)->buf) == 0) { fn }

/* jmpbuf for jumping out of protected function */
typedef struct cs_ljmp {
    struct cs_ljmp *prev;
    csi_jmpbuf buf;
    volatile int status;
} cs_ljmp;



/* -------------------------------------------------------------------------
 * CallFrame (function frame/stack)
 * ------------------------------------------------------------------------- */

/* get CSript 'Function' */
#define cfFunction(cf)      (crclval(s2v((cf)->func.p))->fn)


/* 'cfstatus' bits */
#define CFST_FRESH          (1<<0) /* fresh execute of Cript functon */
#define CFST_CCALL          (1<<1) /* call is running C function */
#define CFST_FIN            (1<<2) /* function called finalizer */


/* 'CallFrame' function is CSript function */
#define cfisCScript(cf)     (!((cf)->status & CFST_CCALL))


/* call information */
typedef struct CallFrame {
    SIndex func; /* function stack index */
    SIndex top; /* top for this function */
    struct CallFrame *prev, *next; /* call link */
    const Instruction *pc; /* (only for Cript function) */
    int nvarargs; /* number of varargs (only for Cript function) */
    int nresults; /* number of expected results from this function */
    cs_ubyte status; /* call status */
} CallFrame;



/* -------------------------------------------------------------------------
 * Global state
 * ------------------------------------------------------------------------- */

typedef struct GState {
    cs_Alloc falloc; /* allocator */
    void *ud_alloc; /* userdata for 'falloc' */
    cs_mem totalbytes; /* number of bytes allocated - gcgcdebt */
    cs_mem gcdebt; /* number of bbytes not yet compensated by collector */
    cs_umem gcestimate; /* gcestimate of non-garbage memory in use */
    HTable *strings; /* interned strings and weak refs */
    TValue ginstance; /* global instance */
    TValue nil; /* nil value (init flag) */
    uint seed; /* initial seed for hashing */
    cs_ubyte whitebit; /* current white bit (WHITEBIT0 or WHITEBIT1) */
    cs_ubyte gcstate; /* GC state bits */
    cs_ubyte gcstopem; /* stops emergency collections */
    cs_ubyte gcstop; /* control wheter GC is running */
    cs_ubyte gcemergency; /* true if this is emergency collection */
    cs_ubyte gcpause; /* how long to wait until next cycle */
    cs_ubyte gcstepmul; /* GC "speed" (heap size grow speed) */
    cs_ubyte gcstepsize; /* log2 of GC granularity */
    GCObject *objects; /* list of all collectable objects */
    GCObject **sweeppos; /* current position of sweep in list */
    GCObject *fin; /* list of objects that have finalizer */
    GCObject *graylist; /* list of gray objects */
    GCObject *grayagain; /* list of objects to be traversed atomically */
    GCObject *weak; /* list of all weak hashtables (key & value) */
    GCObject *tobefin; /* list of objects to be finalized (pending) */
    GCObject *fixed; /* list of fixed objects (not to be collected) */
    struct cs_State *thwouv; /* list of threads with open upvalues */
    cs_CFunction fpanic; /* panic handler (unprotected calls) */
    struct cs_State *mainthread; /* thread that also created global state */
    OString *memerror; /* preallocated message for memory errors */
    OString *mmnames[CS_NUM_MM]; /* array with metamethod names */
    TValue *vmt[CS_NUM_TYPES]; /* vmt's for basic types */
    cs_WarnFunction fwarn; /* warning function */
    void *ud_warn; /* userdata for 'fwarn' */
} GState;



/* -------------------------------------------------------------------------
 * Thread (per-thread-state)
 * ------------------------------------------------------------------------- */

/* Cript thread state */
struct cs_State {
    ObjectHeader;
    ushrt ncf; /* number of call frames in 'cf' list */
    int status; /* status code */
    ptrdiff_t errfunc; /* error handling function (on stack) */
    cs_uint32 nCcalls; /* number of C calls */
    GCObject *gclist;
    struct cs_State *thwouv; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    cs_ljmp *errjmp; /* error recovery */
    SIndex stack; /* stack base */
    SIndex sp; /* first free slot in the 'stack' */
    SIndex stackend; /* end of 'stack' + 1 */
    CallFrame basecf; /* base frame, C's entry point to Cript */
    CallFrame *cf; /* active frame */
    UpVal *openupval; /* list of open upvalues */
    SIndex tbclist; /* list of to-be-closed variables */
};


/* thread global state */
#define G_(ts)      (ts)->gstate

/* get global instance */
#define GI(ts)      insval(&G_(ts)->ginstance)

/* check if global state is initialized */
#define gsinitialized(gs)       ttisnil(&(gs)->nil)

/* check if thread is in 'thwouv' list */
#define isinthwouv(ts)          ((ts)->thwouv != (ts))



/* thread state + CS_EXTRASPACE */
typedef struct XS {
    cs_ubyte extra_[CS_EXTRASPACE];
    cs_State ts;
} XS;


/* Main thread + global state */
typedef struct XSG {
    XS xs;
    GState gs;
} XSG;


/* cast 'cs_State' back to start of 'XS' */
#define fromstate(th)   cast(XS *, cast(cs_ubyte *, th) - offsetof(XS, ts))



/* union for conversions (casting) */
union GCUnion {
    struct GCObject gc; /* object header */
    struct HTable ht;
    struct Array arr;
    struct OString str;
    struct UpVal uv;
    struct Function fn;
    union Closure cl;
    struct OClass cls;
    struct Instance ins;
    struct IMethod im;
    struct UserData ud;
    struct cs_State th;
};

#define cast_gcu(o)     cast(union GCUnion *, (o))

#define gco2ht(o)       (&(cast_gcu(o)->ht))
#define gco2arr(o)      (&(cast_gcu(o)->arr))
#define gco2str(o)      (&(cast_gcu(o)->str))
#define gco2uv(o)       (&(cast_gcu(o)->uv))
#define gco2fn(o)       (&(cast_gcu(o)->fn))
#define gco2ccl(o)      (&((cast_gcu(o)->cl).cc))
#define gco2crcl(o)     (&((cast_gcu(o)->cl).crc))
#define gco2cl(o)       (&(cast_gcu(o)->cl))
#define gco2cls(o)      (&(cast_gcu(o)->cls))
#define gco2ins(o)      (&(cast_gcu(o)->ins))
#define gco2im(o)       (&(cast_gcu(o)->im))
#define gco2ud(o)       (&(cast_gcu(o)->ud))
#define gco2th(o)       (&(cast_gcu(o)->th))

#define obj2gco(o)      (&(cast_gcu(o)->gc))


CSI_FUNC CallFrame *csT_newcf(cs_State *ts);
CSI_FUNC void csT_seterrorobj(cs_State *ts, int errcode, SPtr oldtop);
CSI_FUNC int csT_reallocstack(cs_State *ts, int size, int raiseerr);
CSI_FUNC int csT_growstack(cs_State *ts, int n, int raiseerr);
CSI_FUNC void csT_shrinkstack(cs_State *ts);
CSI_FUNC void csT_incsp(cs_State *ts);
CSI_FUNC void csT_incCstack(cs_State *ts);
CSI_FUNC void csT_checkCstack(cs_State *ts);
CSI_FUNC int csT_resetthread(cs_State *ts, int status);
CSI_FUNC void csT_warning(cs_State *ts, const char *msg, int cont);
CSI_FUNC void csT_warnerror(cs_State *ts, const char *where);
CSI_FUNC void csT_free(cs_State *ts, cs_State *thread);

#endif
