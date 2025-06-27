/*
** cstate.h
** Global and Thread state
** See Copyright Notice in cscript.h
*/

#ifndef cstate_h
#define cstate_h


#include "cobject.h"
#include "clist.h"

#include <setjmp.h>


/* 
** Increment number of nested non-yieldable calls.
** The counter is located in the upper 2 bytes of 'nCcalls'.
** (As of current version, every call is non-yieldable.)
*/
#define incnnyc(C)      ((C)->nCcalls += 0x10000)

/* decrement number of nested non-yieldable calls */
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
** Helps reduce stack checks (branching).
*/
#define EXTRA_STACK     5


#define INIT_STACKSIZE      (CS_MINSTACK * 2)

#define stacksize(th)       cast_int((th)->stackend.p - (th)->stack.p)



/* {======================================================================
** CallFrame
** ======================================================================= */

/* bits in CallFrame status */
#define CFST_CCALL      (1<<0) /* call is running a C function */
#define CFST_FRESH      (1<<1) /* call is on fresh "csV_execute" frame */
#define CFST_HOOKED     (1<<2) /* call is running a debug hook */
#define CFST_FIN        (1<<3) /* function "called" a finalizer */
#define CFST_TRAN       (1<<4) /* 'cf' has transfer information */


typedef struct CallFrame {
    SIndex func; /* function stack index */
    SIndex top; /* top for this function */
    struct CallFrame *prev, *next; /* dynamic call link */
    struct { /* only for CScript function */
        const Instruction *pc; /* current pc (points to instruction) */
        const Instruction *pcret; /* after return continue from this pc */
        volatile c_signal trap; /* hooks or stack reallocation flag */
        int nvarargs; /* number of optional arguments */
    } cs;
    int ftransfer; /* offset of first value transferred */
    int ntransfer; /* number of values transferred */
    int nresults; /* number of expected results from this function */
    c_byte status; /* call status */
} CallFrame;


#define isCScript(cf)       (!((cf)->status & CFST_CCALL))

/* }====================================================================== */



/* {======================================================================
** Global state
** ======================================================================= */

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
    TValue c_list; /* global C list */
    TValue c_table; /* global C table */
    TValue nil; /* special nil value (also init flag) */
    c_uint seed; /* initial seed for hashing */
    c_byte whitebit; /* current white bit (WHITEBIT0 or WHITEBIT1) */
    c_byte gcstate; /* GC state bits */
    c_byte gcstopem; /* stops emergency collections */
    c_byte gcstop; /* control wheter GC is running */
    c_byte gcemergency; /* true if this is emergency collection */
    c_byte gcparams[CS_GCP_NUM];
    c_byte gccheck; /* true if collection was triggered since last check */
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
    OString *listfields[LFNUM]; /* array with names of list fields */
    OString *memerror; /* preallocated message for memory errors */
    OString *mmnames[CS_MT_NUM]; /* array with metamethod names */
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
    c_byte status;
    c_byte allowhook;
    c_ushort ncf; /* number of call frames in 'cf' list */
    GCObject *gclist;
    struct cs_State *twups; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    cs_longjmp *errjmp; /* error recovery */
    SIndex stack; /* stack base */
    SIndex sp; /* first free slot in the 'stack' */
    SIndex stackend; /* end of 'stack' + 1 */
    CallFrame basecf; /* base frame, C's entry point to CScript */
    CallFrame *cf; /* active frame */
    volatile cs_Hook hook;
    UpVal *openupval; /* list of open upvalues */
    SIndex tbclist; /* list of to-be-closed variables */
    ptrdiff_t errfunc; /* error handling function (on stack) */
    c_uint32 nCcalls; /* number of C calls */
    int oldpc; /* last pc traced */
    int basehookcount;
    int hookcount;
    volatile c_signal hookmask;
};


/* check if global state is fully built */
#define statefullybuilt(gs)     ttisnil(&(gs)->nil)


/* get thread global state */
#define G(C)        ((C)->gstate)


/* get the clist */
#define CL(C)       (&G(C)->c_list)

/* get the ctable */
#define CT(C)       (&G(C)->c_table)


/*
** Get the global table in the clist. Since all predefined
** indices in the clist were inserted right when the clist
** was created and never removed, they must always be present.
*/
#define GT(C)       (&listval(CL(C))->b[CS_CLIST_GLOBALS])




/* eXtra space + main thread State */
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
