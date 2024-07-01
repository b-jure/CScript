#ifndef CRSTATE_H
#define CRSTATE_H


#include "crgc.h"
#include "crobject.h"
#include "crvalue.h"

#include <setjmp.h>



/* -------------------------------------------------------------------------
 * Long jump (for protected calls)
 * ------------------------------------------------------------------------- */

#define cr_jmpbuf	jmp_buf


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
#define cffn(cf)	(crclvalue(s2v((cf)->callee.p))->fn)


/* 'cfstatus' bits */
#define CFST_FRESH		(1<<0) /* in top-level Cript function */
#define CFST_CCALL		(1<<1) /* in C call */


/* 'CallFrame' function is Cript function */
#define cfiscript(cf)	(!((cf)->cfstatus & CFccall))


/* call information */
typedef struct CallFrame {
	SIndex callee; /* function */
	SIndex stacktop; /* stack top for this call */
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
	unsigned int seed; /* initial seed for hashing */
	TValue nil; /* nil value (init flag) */
	GC gc; /* garbage collector */
	HTable strings; /* strings table (weak refs) */
	struct TState *mainthread; /* thread that also created global state */
	struct TState **tsopenuv; /* threads with open upvalues */
	OString *memerror; /* error message for memory errors */
	OString *vtmnames[CR_NUMM]; /* vtable method names */
} GState;



/* -------------------------------------------------------------------------
 * TState (per-thread state)
 * ------------------------------------------------------------------------- */
Vec(GCObjectVec, GCObject*);
Vec(SIndexVec, SIndex);
Vec(OStringVec, OString*);
Vec(CallFrameVec, CallFrame);


/* thread global state */
#define GS(ts)		(ts)->gstate


/* Cript thread state */
typedef struct TState {
	ObjectHeader;
	GState *gstate; /* shared global state */
	int status; /* status code */
	cr_ljmp *errjmp; /* error recovery */
	unsigned int ncalls; /* number of nested calls */
	SIndex stacktop; /* first free slot in the stack */
	SIndex stackend; /* end of stack */
	SIndex stack; /* stack base */
	CallFrame *aframe; /* currently active frame in 'frames' */
	CallFrameVec frames; /* call stack */
	SIndexVec callstart; /* start of call values */
	SIndexVec retstart; /* start of return values */
	HTable gids; /* global variable names + index into 'gvars' */
	TValueVec gvars; /* global variable values */
	UValue *openuv; /* open upvalues */
	SIndex tbclist; /* list of to-be-closed variables */
} TState;



#endif
