/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef CRVM_H
#define CRVM_H

#include "crgc.h"
#include "crchunk.h"
#include "crcommon.h"
#include "crhashtable.h"
#include "crvalue.h"
#include "crvec.h"

#include <setjmp.h>



/*
 * --------------
 * Error recovery
 * --------------
 */

/* protected function */
typedef void (*ProtectedFn)(VM *vm, void *userdata);


/* Wrapper around 'jmp_buf' with some additional
 * information, such as status code of the function that
 * was called and a previous cr_longjmp in order to handle
 * nested protected calls.
 * Additionally 'status' gets updated from the callee in
 * case of runtime error, this is the reason why
 * status is marked as volatile to assure the automatic
 * variable is updated and not only put in a register.
 * The whole structure lives on a stack right before calling
 * the C function.
 * Check 'protectedcall' in 'crvm.c' for a reference. */
struct cr_longjmp {
	struct cr_longjmp *prev;
	jmp_buf buf;
	volatile int status;
};



/*
 * ---------
 * CallFrame
 * ---------
 */

/* CallFrame info */
typedef enum {
	CFI_FRESH = 1, /* fresh vm loop execution */
	CFI_CCALL = 2, /* call is in 'cr_cfunc' */
} CFInfo;


/* size of instruction */
typedef cr_ubyte Instruction;


/* common call frame fields */
#define CommonFrameFields \
	Value *callee; cr_int retcnt; cr_int vacnt; cr_ubyte cfinfo


/* script frame */
typedef struct {
	CommonFrameFields;
	OClosure *closure;
	Instruction *ip;
} Frame;


/* 'cr_cfunc' frame */
typedef struct {
	CommonFrameFields;
} CFrame;


/* function CallFrame */
typedef union CallFrame {
	Frame frame; /* script */
	CFrame cframe; /* 'cr_cfunc' */
} CallFrame;


/* common frame fields */
#define cffirst		cast(cr_ubyte*, cf->callee)
#define cfcallee	cast(Value*, cf->callee)
#define cfretcnt	cast(cr_int, cffirst + sizeof(Value*))
#define cfvacnt		cast(cr_int, cffirst + sizeof(Value*) + sizeof(cr_int))
#define cfinfo		cast(cr_ubyte, cffirst + sizeof(Value*) + 2 * sizeof(cr_int))


/* 'Frame' function */
#define FFN(cf)		((cf)->frame->closure->fn)



/*
 * ---------
 * GlobalVar
 * ---------
 */

/* local/global variable bits */
#define VARconst	0 /* variable is const */
#define VARcaptured	1 /* local variable is captured */

#define isconst(var)	testbit((var)->flags, VARconst)


/* 
 * This is a wrapper around 'Value' with it's modifiers (flags).
 * Local variables do not require this wrapper as they are resolved
 * during compilation meaning they do not need to store 'flags'. 
 */
typedef struct {
	Value value;
	cr_ubyte flags;
} GlobalVar;



/*
 * -----
 * Hooks
 * -----
 */

typedef struct {
	cr_alloc reallocate; /* allocator */
	void *userdata; /* userdata for allocator */
	cr_reader reader; /* source file reader */
	cr_panic panic; /* panic handler */
} Hooks; /* configurable hooks */



/*
 * ---------
 * VM 'Vec's
 * ---------
 */

VecGeneric(OPtrVec, O*);
VecGeneric(RelStkValueVec, RelStkValue);
VecGeneric(VariableVec, GlobalVar);
VecGeneric(StrRefVec, OString*);



/*
 * ---------------
 * Virtual Machine
 * ---------------
 */


/* 
 * Virtual Machine (thread state).
 * Note: As of version 1.0.0 there is no separation
 * of global and thread state.
 * This means that cript does not have internal support
 * for asynchronous code or concurrency.
 */
struct VM {
	cr_uint seed; /* initial seed for hashing */
	cr_status status; /* status code */
	cr_int fc; /* call frame count */
	CallFrame frames[VM_CALLSTACK_LIMIT]; /* call stack */
	Hooks hooks; /* hooks to extension code */
	GC gc; /* garbage collector */
	ValueVec stack; /* stack */
	RelStkValue *sp; /* stack pointer */
	RelStkValue *stackend; /* end of the stack */
	RelStkValueVec callstart; /* start of call values */
	RelStkValueVec retstart; /* start of return values */
	HashTable gids; /* global variable names */
	VariableVec gvars; /* global variable values */
	ValueVec temp; /* temporary storage for return values */
	struct cr_longjmp *errjmp; /* error recovery */
	HashTable weakrefs; /* interned strings (unmarked) */
	StrRefVec interned; /* interned strings (marked) */
	OUpvalue *open_upvals; /* closure values */
	OString *faststatic[SS_N]; /* preallocated static strings */
	OString *memerror; /* preallocated string object for memory errors */
	Value init; /* init flag */
	Value *gs; /* gray stack */
	cr_umem gslen; /* gs length */
	cr_umem gscap; /* gs capacity */
};


/* boolean static string value */
#define ssvbool(vm, b) \
	OBJ_VAL((b) ? (vm)->faststatic[SS_TRUE] : (vm)->faststatic[SS_FALSE])


/* get static string value with tag 't' */
#ifndef ssv
#define ssv(vm, t) OBJ_VAL((vm)->faststatic[(t)])
#endif


/* 'init' member is set to nil 'Value' if VM is fully initialized */
#define vminitialized(vm) (IS_NIL((vm)->init))


/* Fetch the last call frame (current) */
#define last_frame(vm) ((vm)->frames[(vm)->fc - 1])


/* ensure stack space for 'n' values */
#define ensurestack(vm, n) ValueVec_ensure(&(vm)->stack, n)

/* ensure 'EXTRASTACK' stack space */
#define ensureminstack(vm) (ensurestack(vm, EXTRASTACK))


/* TODO: rewrite */
#define restore_stack(vm, n) cast(Value *, (cast_charp((vm)->stack) + (n)))
#define save_stack(vm, ptr)  (cast_charp(ptr) - cast_charp((vm)->stack))
#define stkpeek(top)	     ((vm)->sp - ((top) + 1))


/* decrement stack pointer */
#define decsp(vm)	((vm)->sp--)
#define pop(vm)		(*--(vm)->sp)
#define popn(vm, n) 	((vm)->sp -= n)

/* increment stack pointer */
#define incsp(vm)	((vm)->sp++)

/* push value/s on the stack */
void push(VM *vm, Value val);
#define pushn(vm, n, val)  { cr_int cnt = (n); while (cnt-- > 0) push(vm, val); }



/* concatenate on stack */
#define vmconcatstk(vm) \
	{ *stkpeek(1) = OBJ_VAL(vmconcat(vm, *stkpeek(1), *stkpeek(0))); decsp(vm); }



Value vmconcat(VM *vm, Value l, Value r);

/* ordering */
void equalop(VM *vm, StkValue l, StkValue r); /* OP_EQUAL */
cr_ubyte eqop_raw(StkValue l, StkValue r); /* OP_EQ raw */
void opeq(VM *vm, StkValue l, StkValue r); /* OP_EQ */
void opne(VM *vm, StkValue l, StkValue r); /* OP_NE */
void oplt(VM *vm, StkValue l, StkValue r); /* OP_LT */
void opgt(VM *vm, StkValue l, StkValue r); /* OP_GT */
void ople(VM *vm, StkValue l, StkValue r); /* OP_LE */
void opge(VM *vm, StkValue l, StkValue r); /* OP_GE */


void VM_init(VM *vm);
void interpret(VM *vm, const char *source, const char *filename);
OString *globalname(VM *vm, uint32_t idx);
void run(VM *vm);
void ncall(VM *vm, Value *retstart, Value fn, int32_t retcnt);
int pcall(VM *vm, ProtectedFn fn, void *userdata, ptrdiff_t oldtop);
cr_ubyte pcompile(VM* vm, void* userdata, const char* name, cr_ubyte isingscope);
void closeupval(VM *vm, Value *last);
cr_ubyte bindmethod(VM *vm, OClass *oclass, Value name, Value receiver);
void resetvm(VM *vm, cr_status status);

#endif
