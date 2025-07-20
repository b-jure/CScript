/*
** tstate.h
** Global and Thread state
** See Copyright Notice in tokudae.h
*/

#ifndef tstate_h
#define tstate_h


#include "tobject.h"
#include "tlist.h"

#include <setjmp.h>


/* 
** Increment number of netted non-yield-able calls.
** The counter it located in the upper 2 bytes of 'nCcalls'.
** (At of current version, every call is non-yield-able.)
*/
#define incnnyc(T)      ((T)->nCcalls += 0x10000)

/* decrement number of netted non-yieldable calls */
#define decnnyc(T)      ((T)->nCcalls -= 0x10000)


/*
** Get total number of C calls.
** This counter is located in the lower 2 bytes of 'nCcalls'.
*/
#define getCcalls(T)    ((T)->nCcalls & 0xffff)


/* non-yield-able and C calls increment */
#define nyci        (0x10000 | 1)


typedef struct toku_longjmp toku_longjmp; /* defined in 'tprotected.c' */


/* atomic type */
#if !defined(t_signal)
#include <signal.h>
#define t_signal        sig_atomic_t
#endif


/*
** Extra stack space that is used mostly when calling metamethods.
** Helps reduce stack checks (branching).
*/
#define EXTRA_STACK     5


#define INIT_STACKSIZE  (TOKU_MINSTACK * 2)


#define stacksize(th)   cast_int((th)->stackend.p - (th)->stack.p)


/* {======================================================================
** CallFrame
** ======================================================================= */

/* bits in CallFrame status */
#define CFST_CCALL      (1<<0) /* call is running a C function */
#define CFST_FRESH      (1<<1) /* call is on fresh "tokuV_execute" frame */
#define CFST_HOOKED     (1<<2) /* call is running a debug hook */
#define CFST_FIN        (1<<3) /* function "called" a finalizer */


typedef struct CallFrame {
    SIndex func; /* function stack index */
    SIndex top; /* top for this function */
    struct CallFrame *prev, *next; /* dynamic call link */
    struct { /* only for Tokudae function */
        const Instruction *pc; /* current pc (points to instruction) */
        const Instruction *pcret; /* after return continue from this pc */
        volatile t_signal trap; /* hooks or stack reallocation flag */
        int nvarargs; /* number of optional arguments */
    } t;
    int nresults; /* number of expected results from this function */
    t_ubyte status; /* call status */
} CallFrame;


/*
** Check if the given call frame it running Tokudae function.
*/
#define isTokudae(cf)       (!((cf)->status & CFST_CCALL))

/* }====================================================================== */



/* {======================================================================
** Global state
** ======================================================================= */

/* 
** Table for interned strings.
** Collision resolution is resolved by chain.
*/
typedef struct StringTable {
    OString **hash; /* array of buckets (linked lists of strings) */
    int nuse; /* number of elements */
    int size; /* number of buckets */
} StringTable;


typedef struct GState {
    toku_Alloc falloc; /* allocator */
    void *ud_alloc; /* userdata for 'falloc' */
    t_mem totalbytes; /* number of bytes allocated - gcdebt */
    t_mem gcdebt; /* number of bytes not yet compensated by collector */
    t_umem gcestimate; /* gcestimate of non-garbage memory in use */
    StringTable strtab; /* interned strings (weak refs) */
    TValue c_list; /* global C list */
    TValue c_table; /* global C table */
    TValue nil; /* special nil value (also init flag) */
    t_uint seed; /* initial seed for hashing */
    t_ubyte whitebit; /* current white bit (WHITEBIT0 or WHITEBIT1) */
    t_ubyte gcstate; /* GC state bits */
    t_ubyte gcstopem; /* stops emergency collections */
    t_ubyte gcstop; /* control whether GC is running */
    t_ubyte gcemergency; /* true if this is emergency collection */
    t_ubyte gcparams[TOKU_GCP_NUM];
    t_ubyte gccheck; /* true if collection was triggered since last check */
    GCObject *objects; /* list of all collectable objects */
    GCObject **sweeppos; /* current position of sweep in list */
    GCObject *fin; /* list of objects that have finalizer */
    GCObject *graylist; /* list of gray objects */
    GCObject *grayagain; /* list of objects to be traversed atomically */
    GCObject *weak; /* list of all weak tables (key & value) */
    GCObject *tobefin; /* list of objects to be finalized (pending) */
    GCObject *fixed; /* list of fixed objects (not to be collected) */
    struct toku_State *twups; /* list of threads with open upvalues */
    toku_CFunction fpanic; /* panic handler (runs in unprotected calls) */
    struct toku_State *mainthread; /* thread that also created global state */
    OString *listfields[LFNUM]; /* array with names of list fields */
    OString *memerror; /* preallocated message for memory errors */
    OString *mtnames[TOKU_MT_NUM]; /* array with metamethod names */
    OString *strcache[TOKUI_STRCACHE_N][TOKUI_STRCACHE_M]; /* string cache */
    toku_WarnFunction fwarn; /* warning function */
    void *ud_warn; /* userdata for 'fwarn' */
} GState;

/* }====================================================================== */


/* {======================================================================
** Thread (per-thread-state)
** ======================================================================= */

/* Tokudae thread state */
struct toku_State {
    ObjectHeader;
    t_ubyte status;
    t_ubyte allowhook;
    t_ushort ncf; /* number of call frames in 'cf' list */
    GCObject *gclist;
    struct toku_State *twups; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    toku_longjmp *errjmp; /* error recovery */
    SIndex stack; /* stack base */
    SIndex sp; /* first free slot in the 'stack' */
    SIndex stackend; /* end of 'stack' + 1 */
    CallFrame basecf; /* base frame, C's entry point to Tokudae */
    CallFrame *cf; /* active frame */
    volatile toku_Hook hook;
    UpVal *openupval; /* list of open upvalues */
    SIndex tbclist; /* list of to-be-closed variables */
    ptrdiff_t errfunc; /* error handling function (on stack) */
    t_uint32 nCcalls; /* number of C calls */
    int oldpc; /* last pc traced */
    int basehookcount;
    int hookcount;
    volatile t_signal hookmask;
    struct { /* info about transferred values (for call/return hooks) */
        int ftransfer; /* offset of first value transferred */
        int ntransfer; /* number of values transferred */
    } transferinfo;
};


/* check if global state is fully built */
#define statefullybuilt(gs)     ttisnil(&(gs)->nil)


/* get thread global state */
#define G(T)        ((T)->gstate)


/* get the clist */
#define CL(T)       (&G(T)->c_list)

/* get the ctable */
#define CT(T)       (&G(T)->c_table)


/*
** Get the global table in the clist. Since all predefined
** indices in the clist were inserted right when the clist
** was created and never removed, they must always be present.
*/
#define GT(T)       (&listval(CL(T))->arr[TOKU_CLIST_GLOBALS])




/* eXtra space + main thread State */
typedef struct XS {
    t_ubyte extra_[TOKU_EXTRASPACE];
    toku_State t;
} XS;


/* extra space(X) + main thread state(S) + global state(G) */
typedef struct XSG {
    XS xs;
    GState gs;
} XSG;


/* cast 'toku_State' back to start of 'XS' */
#define fromstate(T)    cast(XS *, cast(t_ubyte *, (T)) - offsetof(XS, t))

/* }====================================================================== */


/* union for conversions (casting) */
union GCUnion {
    struct GCObject gc; /* object header */
    struct Table tab;
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
    struct toku_State T;
};

#define cast_gcu(o)     cast(union GCUnion *, (o))

#define gco2ht(o)       (&(cast_gcu(o)->tab))
#define gco2list(o)     (&(cast_gcu(o)->l))
#define gco2str(o)      (&(cast_gcu(o)->str))
#define gco2uv(o)       (&(cast_gcu(o)->uv))
#define gco2proto(o)    (&(cast_gcu(o)->p))
#define gco2clc(o)      (&((cast_gcu(o)->cl).c))
#define gco2clt(o)      (&((cast_gcu(o)->cl).t))
#define gco2cl(o)       (&(cast_gcu(o)->cl))
#define gco2cls(o)      (&(cast_gcu(o)->cls))
#define gco2ins(o)      (&(cast_gcu(o)->ins))
#define gco2im(o)       (&(cast_gcu(o)->im))
#define gco2um(o)       (&(cast_gcu(o)->um))
#define gco2u(o)        (&(cast_gcu(o)->u))
#define gco2th(o)       (&(cast_gcu(o)->T))

#define obj2gco(o)      (&(cast_gcu(o)->gc))


TOKUI_FUNC CallFrame *tokuT_newcf(toku_State *T);
TOKUI_FUNC int tokuT_reallocstack(toku_State *T, int size, int raiseerr);
TOKUI_FUNC int tokuT_growstack(toku_State *T, int n, int raiseerr);
TOKUI_FUNC void tokuT_shrinkstack(toku_State *T);
TOKUI_FUNC void tokuT_incsp(toku_State *T);
TOKUI_FUNC void tokuT_incCstack(toku_State *T);
TOKUI_FUNC void tokuT_checkCstack(toku_State *T);
TOKUI_FUNC int tokuT_resetthread(toku_State *T, int status);
TOKUI_FUNC void tokuT_warning(toku_State *T, const char *msg, int cont);
TOKUI_FUNC void tokuT_warnerror(toku_State *T, const char *where);
TOKUI_FUNC void tokuT_free(toku_State *T, toku_State *thread);

#endif
