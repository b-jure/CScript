#ifndef CRSTATE_H
#define CRSTATE_H


#include "crgc.h"
#include "crobject.h"
#include "crvalue.h"

#include <setjmp.h>



/*
 * Extra stack space that is used mostly when calling vtable
 * methods. Helps in avoiding stack checks (branching).
 */
#define EXTRA_STACK	5


/* initial stack size */
#define STACKSIZE_INIT		(CR_MINSTACK * 4)


/* stack size */
#define stacksize(ts)		cast_int((ts)->stackend.p - (ts)->stack.p)

/* current stack top offset */
#define topoffset(ts)		cast_int((ts)->sp.p - (ts)->stack.p)


/* save/restore stack position */
#define savestack(ts,ptr)	(cast_charp(ptr) - cast_charp((ts)->stack.p))
#define restorestack(ts,o)	cast(SPtr, cast_charp((ts)->stack.p) + (o))


/* check if stack needs to grow */
#define checkstack(ts,n) \
    if (cr_unlikely((ts)->stackend.p - (ts)->sp.p <= (n))) \
            crT_growstack(ts, (n), 1);


/* 
** Increment number of nested Cript calls.
** The counter is located in the upper 2 bytes of 'nCC'.
*/
#define incn_C(ts)       ((ts)->nCC += 0x10000)

/* Decrement number of nested Cript calls. */
#define decn_C(ts)       ((ts)->nCC -= 0x10000)


/*
** Get number of nested C calls.
** This counter is located in the lower 2 bytes of 'nCC'.
*/
#define getnC_(ts)       ((ts)->nCC & 0xffff)


/* Increment value for both the nested C and Cript calls. */
#define nCCi    (0x10000 | 1)



/* -------------------------------------------------------------------------
 * Long jump (for protected calls)
 * ------------------------------------------------------------------------- */

#define CRI_THROW(ts,b)     longjmp((b)->buf, 1)
#define CRI_TRY(ts,b,fn)    if (setjmp((b)->buf) == 0) { fn }

#define cri_jmpbuf          jmp_buf

/* jmpbuf for jumping out of protected function */
typedef struct cr_ljmp {
    struct cr_ljmp *prev;
    cri_jmpbuf buf;
    volatile int status;
} cr_ljmp;



/* -------------------------------------------------------------------------
 * CallFrame (function frame/stack)
 * ------------------------------------------------------------------------- */

/* get Cript 'Function' */
#define cffn(cf)        (crclval(s2v((cf)->callee.p)).fn)


/* 'cfstatus' bits */
#define CFST_FRESH          (1<<0) /* fresh execute of Cript functon */
#define CFST_CCALL          (1<<1) /* in C call */
#define CFST_FIN            (1<<2) /* in finalizer */


/* 'CallFrame' function is Cript function */
#define cfiscript(cf)   (!((cf)->cfstatus & CFST_CCALL))


/* call information */
typedef struct CallFrame {
    struct CallFrame *prev, *next; /* linked-list */
    SIndex callee; /* function */
    SIndex top; /* top for this call */
    const Instruction *pc; /* only for non-C callee */
    int nvarargs; /* only for non-C callee */
    int nreturns; /* number of return values */
    cr_ubyte cfstatus; /* call status */
} CallFrame;



/* -------------------------------------------------------------------------
 * Global state
 * ------------------------------------------------------------------------- */

typedef struct GState {
    cr_fAlloc falloc; /* allocator */
    void *udrealloc; /* userdata for 'realloc' */
    cr_CFunction panic; /* panic handler (unprotected calls) */
    uint seed; /* initial seed for hashing */
    TValue nil; /* nil value (init flag) */
    GC gc; /* garbage collection managed values and parameters */
    HTable strings; /* weak 'HTable' (weak references) */
    HTable *globals; /* global variables */
    struct cr_State *mainthread; /* thread that also created global state */
    struct cr_State *thwouv; /* thread with open upvalues */
    OString *memerror; /* preallocated message for memory errors */
    OString *vmtnames[CR_NUM_META]; /* method names */
} GState;



/* -------------------------------------------------------------------------
 * Thread (per-thread-state)
 * ------------------------------------------------------------------------- */

/* Cript thread state */
struct cr_State {
    ObjectHeader;
    ushrt ncf; /* number of call frames in 'cf' list */
    int status; /* status code */
    cr_uint32 nCC; /* number of calls (C | Cript) */
    GCObject *gclist;
    struct cr_State *thwouv; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    cr_ljmp *errjmp; /* error recovery */
    SIndex stack; /* stack base */
    SIndex sp; /* first free slot in the 'stack' */
    SIndex stackend; /* end of 'stack' + 1 */
    CallFrame *cf; /* active frame */
    UpVal *openupval; /* list of open upvalues */
    SIndex tbclist; /* list of to-be-closed variables */
};


/* thread global state */
#define G_(ts)          (ts)->gstate

/* check if thread is initialized */
#define tsinitialized(ts)       (ttisnil(&(ts)->nil))

/* check if thread is in 'thwouv' list */
#define isinthwouv(ts)          ((ts)->thwouv != (ts))



/* union for conversions (casting) */
union GCUnion {
    struct GCObject gc; /* object header */
    struct HTable ht;
    struct OString str;
    struct UpVal uv;
    struct Function fn;
    union Closure cl;
    struct OClass cls;
    struct Instance ins;
    struct InstanceMethod im;
    struct UserData ud;
    struct cr_State th;
};

#define cast_gcu(o)     cast(union GCUnion *, (o))

#define gco2ht(o)       (&(cast_gcu(o)->ht))
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


CRI_FUNC CallFrame *crT_newcf(cr_State *ts);
CRI_FUNC int crT_reallocstack(cr_State *ts, int size, int raiseerr);
CRI_FUNC int crT_growstack(cr_State *ts, int n, int raiseerr);
CRI_FUNC void crT_shrinkstack(cr_State *ts);
CRI_FUNC void crT_incsp(cr_State *ts);
CRI_FUNC void crT_incC_(cr_State *ts);
CRI_FUNC void crT_checkCstack(cr_State *ts);
CRI_FUNC cr_noret crT_throw(cr_State *ts, int code);

#endif
