#ifndef CRSTATE_H
#define CRSTATE_H


#include "crgc.h"
#include "crobject.h"
#include "crvalue.h"

#include <setjmp.h>



/* -------------------------------------------------------------------------
 * Long jump (for protected calls)
 * ------------------------------------------------------------------------- */

#define cr_jmpbuf       jmp_buf


/* jmpbuf for jumping out of protected function */
typedef struct cr_ljmp {
    struct cr_ljmp *prev;
    cr_jmpbuf buf;
    volatile int status;
} cr_ljmp;



/* -------------------------------------------------------------------------
 * CallFrame (function frame/stack)
 * ------------------------------------------------------------------------- */

/* get Cript 'Function' */
#define cffn(cf)        (crclval(s2v((cf)->callee.p)).fn)


/* 'cfstatus' bits */
#define CFST_FRESH          (1<<0) /* in top-level Cript function */
#define CFST_CCALL          (1<<1) /* in C call */
#define CFST_FIN            (1<<2) /* in finalizer */


/* 'CallFrame' function is Cript function */
#define cfiscript(cf)   (!((cf)->cfstatus & CFST_CCALL))


/* call information */
typedef struct CallFrame {
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
    cr_alloc realloc; /* allocator */
    void *udrealloc; /* userdata for 'realloc' */
    cr_cfunc panic; /* panic handler (unprotected calls) */
    uint seed; /* initial seed for hashing */
    TValue nil; /* nil value (init flag) */
    GC gc; /* garbage collection managed values and parameters */
    HTable strings; /* weak 'HTable' (weak references) */
    HTable *globals; /* global variables */
    struct cr_State *mainthread; /* thread that also created global state */
    struct cr_State *thwouv; /* thread with open upvalues */
    OString *memerror; /* error message for memory errors */
    OString *vtmnames[CR_NUM_META]; /* vtable method names */
} GState;



/* -------------------------------------------------------------------------
 * Thread (per-thread-state)
 * ------------------------------------------------------------------------- */

/* Cript thread state */
struct cr_State {
    ObjectHeader;
    GCObject *gclist;
    struct cr_State *thwouv; /* next thread with open upvalues */
    GState *gstate; /* shared global state */
    int status; /* status code */
    cr_ljmp *errjmp; /* error recovery */
    SIndex stacktop; /* first free slot in the stack */
    SIndex stackend; /* end of stack */
    SIndex stack; /* stack base */
    CallFrame *aframe; /* currently active frame in 'frames' */
    CallFrame *frames; /* call stack */
    UpVal *openupval; /* list of open upvalues */
    TValue *deferlist;
    SIndex tbclist; /* list of to-be-closed variables */
    uint ncalls; /* number of nested calls */
    int nframes; /* number of elements in 'frames' */
    int sizeframes; /* size of 'frames' */
};


/* thread global state */
#define GS(ts)          (ts)->gstate

/* check if thread is initialized */
#define tsinitialized(ts) (ttisnil(&(ts)->nil))

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


CRI_FUNC cr_noret cr_state_throw(cr_State *ts, int code);
CRI_FUNC void cr_state_inccalls(cr_State *ts);

#endif
