/*
** cstate.h
** Global and Thread state
** See Copyright Notice in cscript.h
*/

#ifndef CSTATE_H
#define CSTATE_H


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
#define stacksize(C)		cast_int((C)->stackend.p - (C)->stack.p)


/* save/restore stack position */
#define savestack(C,ptr)	(cast_charp(ptr) - cast_charp((C)->stack.p))
#define restorestack(C,n)	cast(SPtr, cast_charp((C)->stack.p) + (n))


/* 
** Check if stack nees to grow if so, do 'pre' then grow and
** then do 'pos'.
*/
#define csT_checkstackaux(C,n,pre,pos) \
    if (csi_unlikely((C)->stackend.p - (C)->sp.p <= (n))) \
        { pre; csT_growstack(C, (n), 1); pos; } \
    else { condmovestack(C, pre, pos); }


/* check if stack needs to grow */
#define csT_checkstack(C,n)    csT_checkstackaux(C,n,(void)0,(void)0)


/* check if stack needs to grow and preserve 'p' */
#define checkstackp(C,n,p) \
        csT_checkstackaux(C, n, \
            ptrdiff_t p_ = savestack(C, p), \
            p = restorestack(C, p_))


/* check GC then check stack preserving 'p' */
#define checkstackGCp(C,n,p) \
        csT_checkstackaux(C,n, \
            ptrdiff_t p_ = savestack(C,p); csG_checkGC(C), \
            p = restorestack(C, p_))


/* check GC then check stack */
#define checkstackGC(C,n) \
        csT_checkstackaux(C,n,csG_checkGC(C),(void)0)
    


/* 
** Increment number of nested non-yieldable calls.
** The counter is located in the upper 2 bytes of 'nCcalls'.
*/
#define incnnyc(C)       ((C)->nCcalls += 0x10000)

/* Decrement number of nested non-yieldable calls. */
#define decnnyc(C)       ((C)->nCcalls -= 0x10000)


/*
** Get total number of C calls.
** This counter is located in the lower 2 bytes of 'nCcalls'.
*/
#define getCcalls(C)       ((C)->nCcalls & 0xffff)


/* non-yieldable and C calls increment */
#define nyci        (0x10000 | 1)



/* -------------------------------------------------------------------------
 * Long jump (for protected calls)
 * ------------------------------------------------------------------------- */

#define csi_jmpbuf          jmp_buf
#define CSI_THROW(C,b)      longjmp((b)->buf, 1)
#define CSI_TRY(C,b,fn)     if (setjmp((b)->buf) == 0) { fn }

/* jmpbuf for jumping out of protected function */
typedef struct c_ljmp {
    struct c_ljmp *prev;
    csi_jmpbuf buf;
    volatile int status;
} c_ljmp;



/* -------------------------------------------------------------------------
 * CallFrame (function frame/stack)
 * ------------------------------------------------------------------------- */

/* get CSript function prototype */
#define cfProto(cf)         (clCSval(s2v((cf)->func.p))->p)


/* 'cfstatus' bits */
#define CFST_FRESH          (1<<0) /* fresh execute of Cript functon */
#define CFST_CCALL          (1<<1) /* call is running C function */
#define CFST_FIN            (1<<2) /* function called finalizer */


/* 'CallFrame' function is CSript closure */
#define isCScript(cf)       (!((cf)->status & CFST_CCALL))


/* call information */
typedef struct CallFrame {
    SIndex func; /* function stack index */
    SIndex top; /* top for this function */
    struct CallFrame *prev, *next; /* call link */
    const Instruction *pc; /* (only for CScript function) */
    const Instruction *realpc; /* (only for CScript function) */
    int nvarargs; /* number of varargs (only for CScript function) */
    int nresults; /* number of expected results from this function */
    c_byte status; /* call status */
} CallFrame;



/* -------------------------------------------------------------------------
 * Global state
 * ------------------------------------------------------------------------- */


/* 
** Table for interned strings.
** Collision resolution is resolved by chain.
*/
typedef struct StringTable {
    OString **hash;
    int nuse;
    int size;
} StringTable;


typedef struct GState {
    cs_Alloc falloc; /* allocator */
    void *ud_alloc; /* userdata for 'falloc' */
    c_smem totalbytes; /* number of bytes allocated - gcgcdebt */
    c_smem gcdebt; /* number of bbytes not yet compensated by collector */
    c_mem gcestimate; /* gcestimate of non-garbage memory in use */
    StringTable strtab; /* interned strings (weak refs) */
    TValue c_registry; /* global registry (array) */
    TValue nil; /* nil value (init flag) */
    uint seed; /* initial seed for hashing */
    c_byte whitebit; /* current white bit (WHITEBIT0 or WHITEBIT1) */
    c_byte gcstate; /* GC state bits */
    c_byte gcstopem; /* stops emergency collections */
    c_byte gcstop; /* control wheter GC is running */
    c_byte gcemergency; /* true if this is emergency collection */
    c_byte gcpause; /* how long to wait until next cycle */
    c_byte gcstepmul; /* GC "speed" (heap size grow speed) */
    c_byte gcstepsize; /* log2 of GC granularity */
    GCObject *objects; /* list of all collectable objects */
    GCObject **sweeppos; /* current position of sweep in list */
    GCObject *fin; /* list of objects that have finalizer */
    GCObject *graylist; /* list of gray objects */
    GCObject *grayagain; /* list of objects to be traversed atomically */
    GCObject *weak; /* list of all weak hashtables (key & value) */
    GCObject *tobefin; /* list of objects to be finalized (pending) */
    GCObject *fixed; /* list of fixed objects (not to be collected) */
    struct cs_State *thwouv; /* list of threads with open upvalues */
    cs_CFunction fpanic; /* panic handler (runs in unprotected calls) */
    struct cs_State *mainthread; /* thread that also created global state */
    OString *memerror; /* preallocated message for memory errors */
    OString *mmnames[CS_MM_N]; /* array with metamethod names */
    TValue *vmt[CS_NUM_TYPES]; /* vmt's for basic types */
    OString *strcache[STRCACHE_N][STRCACHE_M]; /* cache for strings in API */
    cs_WarnFunction fwarn; /* warning function */
    void *ud_warn; /* userdata for 'fwarn' */
} GState;



/* -------------------------------------------------------------------------
 * Thread (per-thread-state)
 * ------------------------------------------------------------------------- */

/* CScript thread state */
struct cs_State {
    ObjectHeader;
    ushort ncf; /* number of call frames in 'cf' list */
    int status; /* status code */
    ptrdiff_t errfunc; /* error handling function (on stack) */
    c_uint32 nCcalls; /* number of C calls */
    GCObject *gclist;
    struct cs_State *thwouv; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    c_ljmp *errjmp; /* error recovery */
    SIndex stack; /* stack base */
    SIndex sp; /* first free slot in the 'stack' */
    SIndex stackend; /* end of 'stack' + 1 */
    CallFrame basecf; /* base frame, C's entry point to CScript */
    CallFrame *cf; /* active frame */
    UpVal *openupval; /* list of open upvalues */
    SIndex tbclist; /* list of to-be-closed variables */
};


/* thread global state */
#define G(C)        ((C)->gstate)

/* check if global state is fully built */
#define statefullybuilt(gs)     ttisnil(&(gs)->nil)

/* check if thread is in 'thwouv' (Threads-With-Open-UpValues) list */
#define isinthwouv(C)           ((C)->thwouv != (C))


/*
** Get the global table in the registry. Since all predefined
** indices in the registry were inserted right when the registry
** was created and never removed, they must always be in the array
** part of the registry.
*/
#define getGtable(C) \
	(&arrval(&G(C)->c_registry)->b[CS_RINDEX_GLOBALS])



/* extra space(X) + main thread state(S) */
typedef struct XS {
    c_byte extra_[CS_EXTRASPACE];
    cs_State C;
} XS;


/* extra space(X) + main thread state(S) + global state(G) */
typedef struct XSG {
    XS xs;
    GState gs;
} XSG;


/* cast 'cs_State' back to start of 'XS' */
#define fromstate(th)   cast(XS *, cast(c_byte *, (th)) - offsetof(XS, C))



/* union for conversions (casting) */
union GCUnion {
    struct GCObject gc; /* object header */
    struct Table ht;
    struct Array arr;
    struct OString str;
    struct UpVal uv;
    struct Proto p;
    union Closure cl;
    struct OClass cls;
    struct Instance ins;
    struct IMethod im;
    struct UserData u;
    struct cs_State th;
};

#define cast_gcu(o)     cast(union GCUnion *, (o))

#define gco2ht(o)       (&(cast_gcu(o)->ht))
#define gco2arr(o)      (&(cast_gcu(o)->arr))
#define gco2str(o)      (&(cast_gcu(o)->str))
#define gco2uv(o)       (&(cast_gcu(o)->uv))
#define gco2proto(o)    (&(cast_gcu(o)->p))
#define gco2clc(o)      (&((cast_gcu(o)->cl).c))
#define gco2clcs(o)     (&((cast_gcu(o)->cl).cs))
#define gco2cl(o)       (&(cast_gcu(o)->cl))
#define gco2cls(o)      (&(cast_gcu(o)->cls))
#define gco2ins(o)      (&(cast_gcu(o)->ins))
#define gco2im(o)       (&(cast_gcu(o)->im))
#define gco2u(o)        (&(cast_gcu(o)->u))
#define gco2th(o)       (&(cast_gcu(o)->th))

#define obj2gco(o)      (&(cast_gcu(o)->gc))


CSI_FUNC CallFrame *csT_newcf(cs_State *C);
CSI_FUNC void csT_seterrorobj(cs_State *C, int errcode, SPtr oldtop);
CSI_FUNC int csT_reallocstack(cs_State *C, int size, int raiseerr);
CSI_FUNC int csT_growstack(cs_State *C, int n, int raiseerr);
CSI_FUNC void csT_shrinkstack(cs_State *C);
CSI_FUNC void csT_incsp(cs_State *C);
CSI_FUNC void csT_incCstack(cs_State *C);
CSI_FUNC void csT_checkCstack(cs_State *C);
CSI_FUNC int csT_resetthread(cs_State *C, int status);
CSI_FUNC void csT_warning(cs_State *C, const char *msg, int cont);
CSI_FUNC void csT_warnerror(cs_State *C, const char *where);
CSI_FUNC void csT_free(cs_State *C, cs_State *thread);

#endif
