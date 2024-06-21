/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Cript.
 * Cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/


#ifndef CRVM_H
#define CRVM_H

#include "crgc.h"
#include "crstatics.h"
#include "crhashtable.h"
#include "crvalue.h"

#include <setjmp.h>



/* jmpbuf for jumping out of protected function */
typedef struct cr_longjmp {
	struct cr_longjmp *prev;
	jmp_buf buf;
	volatile int status;
} cr_longjmp;



/* type for functions with error recovery */
typedef void (*ProtectedFn)(VM *vm, void *userdata);



/* call information */
typedef struct CallFrame {
	SIndex callee; /* function */
	SIndex stacktop; /* stack top for this call */
	const Instruction *pc; /* only for non-C callee */
	int nvarargs; /* only for non-C callee */
	int nreturns; /* number of return values */
	cr_ubyte cfstatus; /* call status */
} CallFrame;


/* get Cript 'Function' */
#define cffn(cf)	(crclvalue(s2v((cf)->callee.p))->fn)

/* 'cfstatus' bits */
#define CFfresh		0 /* in top-level Cript function */
#define CFccall		1 /* in C call */

/* 'CallFrame' function is Cript function */
#define cfiscript(cf)	(!((cf)->cfstatus & CFccall))




typedef struct {
	cr_alloc reallocate; /* allocator */
	void *userdata; /* userdata for allocator */
	cr_reader reader; /* script reader */
	cr_cfunc panic; /* panic handler */
} Hooks; 




Vec(GCObjectVec, GCObject*);
Vec(SIndexVec, SIndex);
Vec(OStringVec, OString*);
Vec(CallFrameVec, CallFrame);



/*
 * Extra stack space used mostly when calling vtable
 * methods. Useful in avoiding stack checks (branching).
 */
#define EXTRA_STACK	5


/* initial stack size */
#define STACKSIZE_INIT	(CR_MINSTACK*4)


/* stack size */
#define stacksize(vm)		cast_int((vm)->stackend.p - (vm)->stack.p)

/* current stack top offset */
#define topoffset(vm)		cast_int((vm)->stacktop.p - (vm)->stack.p)


/* save/restore stack position */
#define savestack(vm,ptr)	(cast(char*, (ptr)) - cast(char*, (vm)->stack.p))
#define restorestack(vm,o)	cast(SPtr, cast(char*, (vm)->stack.p) + (o))


/* grow stack if needed */
#define checkstack(vm,n) \
	if (cr_unlikely((vm)->stackend.p - (vm)->stacktop.p <= (n))) \
		cr_vm_growstack(vm, (n), 1);


/* get static string */
#define fstatic(vm,i)		(vm)->faststatic[(i)]


#define vminitialized(vm) (ttisnil(&(vm)->nil))


/* 
 * Virtual Machine (thread state).
 * Note: As of version 1.0.0 there is no separation
 * of global and thread state.
 * This means that Cript does not yet have internal
 * support for asynchronous or concurrent code execution.
 */
struct VM {
	int seed; /* initial seed for hashing */
	int status; /* status code */
	unsigned int nccalls; /* number of nested C calls */
	SIndex stacktop; /* first free slot in the stack */
	SIndex stackend; /* end of stack */
	SIndex stack; /* stack base */
	CallFrame *aframe; /* currently active frame in 'frames' */
	CallFrameVec frames; /* nested function call frames */
	SIndexVec callstart; /* start of call values */
	SIndexVec retstart; /* start of return values */
	HTable gids; /* global variable names */
	TValueVec gvars; /* global variable values */
	cr_longjmp *errjmp; /* error recovery */
	HTable weakrefs; /* string weak references */
	OString *mnames[CR_MNUM]; /* vtable method names */
	OStringVec interned; /* user interned strings */
	Hooks hooks; /* hooks to external code */
	GC gc; /* garbage collector */
	UValue *openuv; /* unclosed closure values */
	SIndex deferlist; /* TODO: list of variables to '__defer__' */
	OString *faststatic[CR_SSNUM]; /* preallocated static strings */
	OString *memerror; /* preallocated string object for memory errors */
	TValue nil; /* nil value (avoid copying and used as init flag) */
};


void cr_vm_init(VM *vm);
void cr_vm_inctop(VM *vm);
int cr_vm_growstack(VM *vm, int n, int raiseerr);
int cr_vm_reallocstack(VM *vm, int size, int raiseerr);
void cr_vm_ncall(VM *vm, SPtr callee, int nreturns);
cr_number cr_vm_modnum(VM *vm, cr_number x, cr_number y);
void cr_vm_incccalls(VM *vm);

void resetvm(VM *vm, int status);
void cr_vm_concat(VM *vm, int n);

void vminterpret(VM *vm, const char *source, const char *filename);
void vmrun(VM *vm);
void vmcall(VM *vm, SIndex *retstart, SIndex fn, int nreturns);
void vmpcall(VM *vm, ProtectedFn fn, void *userdata, ptrdiff_t oldtop);
cr_ubyte vmcompile(VM *vm, void *userdata, const char *name, int gscope);

void vmcloseupval(VM *vm, SIndex *last);

cr_ubyte vmbindmethod(VM *vm, OClass *oclass, SIndex name, SIndex receiver);

Value vmconcat(VM *vm, SIndex l, SIndex r);

cr_ubyte vmequal(VM *vm, SIndex l, SIndex r);
cr_ubyte vmeqraw(SIndex l, SIndex r);
cr_ubyte vmeq(VM *vm, SIndex l, SIndex r);
cr_ubyte vmne(VM *vm, SIndex l, SIndex r);
cr_ubyte vmlt(VM *vm, SIndex l, SIndex r);
cr_ubyte vmgt(VM *vm, SIndex l, SIndex r);
cr_ubyte vmle(VM *vm, SIndex l, SIndex r);
cr_ubyte vmge(VM *vm, SIndex l, SIndex r);

#endif
