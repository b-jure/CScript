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
** Increment number of nested non-yieldable calls.
** The counter is located in the upper 2 bytes of 'nCcalls'.
*/
#define incnnyc(C)      ((C)->nCcalls += 0x10000)

/* Decrement number of nested non-yieldable calls. */
#define decnnyc(C)      ((C)->nCcalls -= 0x10000)


/*
** Get total number of C calls.
** This counter is located in the lower 2 bytes of 'nCcalls'.
*/
#define getCcalls(C)    ((C)->nCcalls & 0xffff)


/* non-yieldable and C calls increment */
#define nyci        (0x10000 | 1)


typedef struct cs_longjmp cs_longjmp; /* defined in 'cprotected.c' */


/* atomic type */
#if !defined(c_signal)
#include <signal.h>
#define c_signal        sig_atomic_t
#endif


/*
** Extra stack space that is used mostly when calling metamethods.
** Helps in avoiding stack checks (branching).
*/
#define EXTRA_STACK	5


#define INIT_STACKSIZE		(CS_MINSTACK * 4)

#define stacksize(th)	    cast_int((th)->stackend.p - (th)->stack.p)



/* {======================================================================
** CallFrame
** ======================================================================= */

/* 'status' bits */
#define CFST_FRESH          (1<<0) /* fresh execute of Cript functon */
#define CFST_CCALL          (1<<1) /* call is running C function */
#define CFST_FIN            (1<<2) /* function "called" a finalizer */


/* 'CallFrame' function is CSript closure */
#define isCScript(cf)       (!((cf)->status & CFST_CCALL))


typedef struct CallFrame {
    SIndex func; /* function stack index */
    SIndex top; /* top for this function */
    struct CallFrame *prev, *next; /* dynamic call link */
    const Instruction *pc; /* (only for CScript func.) */
    const Instruction *realpc; /* (only for CScript func.) */
    volatile c_signal trap; /* (only for CScript func.) */
    int nvarargs; /* number of optional arguments (only for CScript func.) */
    int nresults; /* number of expected results from this function */
    c_byte status; /* call status (CFST_*) */
} CallFrame;

/* }====================================================================== */



/* {======================================================================
** Global state
** ======================================================================= */

/* 
** Table for interned strings.
** Collision resolution is resolved by chain.
*/
typedef struct stringtable {
    OString **hash;
    int nuse;
    int size;
} stringtable;


typedef struct GState {
    cs_Alloc falloc; /* allocator */
    void *ud_alloc; /* userdata for 'falloc' */
    c_smem totalbytes; /* number of bytes allocated - gcgcdebt */
    c_smem gcdebt; /* number of bbytes not yet compensated by collector */
    c_mem gcestimate; /* gcestimate of non-garbage memory in use */
    stringtable strtab; /* interned strings (weak refs) */
    TValue c_registry; /* global registry */
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
    struct cs_State *twups; /* list of threads with open upvalues */
    cs_CFunction fpanic; /* panic handler (runs in unprotected calls) */
    struct cs_State *mainthread; /* thread that also created global state */
    OString *memerror; /* preallocated message for memory errors */
    OString *mmnames[CS_MM_N]; /* array with metamethod names */
    OString *strcache[STRCACHE_N][STRCACHE_M]; /* cache for strings in API */
    cs_WarnFunction fwarn; /* warning function */
    void *ud_warn; /* userdata for 'fwarn' */
} GState;

/* }====================================================================== */


/* {======================================================================
** Thread (per-thread-state)
** ======================================================================= */

/* CScript thread state */
struct cs_State {
    ObjectHeader;
    ushort ncf; /* number of call frames in 'cf' list */
    int status; /* status code */
    ptrdiff_t errfunc; /* error handling function (on stack) */
    c_uint32 nCcalls; /* number of C calls */
    GCObject *gclist;
    struct cs_State *twups; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    cs_longjmp *errjmp; /* error recovery */
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


/*
** Get the global table in the registry. Since all predefined
** indices in the registry were inserted right when the registry
** was created and never removed, they must always be present.
*/
#define GT(C) \
	(&listval(&G(C)->c_registry)->b[CS_RINDEX_GLOBALS])

/* get the registry table */
#define RT(C) \
        (&listval(&G(C)->c_registry)->b[CS_RINDEX_REGTABLE])



/* extra space(X) + main thread state(S) */
typedef struct XS {
    c_byte extra_[CS_EXTRASPACE];
    cs_State c;
} XS;


/* extra space(X) + main thread state(S) + global state(G) */
typedef struct XSG {
    XS xs;
    GState gs;
} XSG;


/* cast 'cs_State' back to start of 'XS' */
#define fromstate(C)    cast(XS *, cast(c_byte *, (C)) - offsetof(XS, c))

/* }====================================================================== */



/* union for conversions (casting) */
union GCUnion {
    struct GCObject gc; /* object header */
    struct Table ht;
    struct List l;
    struct OString str;
    struct UpVal uv;
    struct Proto p;
    union Closure cl;
    struct OClass cls;
    struct Instance ins;
    struct IMethod im;
    struct UMethod um;
    struct UserData u;
    struct cs_State C;
};

#define cast_gcu(o)     cast(union GCUnion *, (o))

#define gco2ht(o)       (&(cast_gcu(o)->ht))
#define gco2list(o)     (&(cast_gcu(o)->l))
#define gco2str(o)      (&(cast_gcu(o)->str))
#define gco2uv(o)       (&(cast_gcu(o)->uv))
#define gco2proto(o)    (&(cast_gcu(o)->p))
#define gco2clc(o)      (&((cast_gcu(o)->cl).c))
#define gco2clcs(o)     (&((cast_gcu(o)->cl).cs))
#define gco2cl(o)       (&(cast_gcu(o)->cl))
#define gco2cls(o)      (&(cast_gcu(o)->cls))
#define gco2ins(o)      (&(cast_gcu(o)->ins))
#define gco2im(o)       (&(cast_gcu(o)->im))
#define gco2um(o)       (&(cast_gcu(o)->um))
#define gco2u(o)        (&(cast_gcu(o)->u))
#define gco2th(o)       (&(cast_gcu(o)->C))

#define obj2gco(o)      (&(cast_gcu(o)->gc))


CSI_FUNC CallFrame *csT_newcf(cs_State *C);
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
