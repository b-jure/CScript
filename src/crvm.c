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

#include "crconf.h"
#include "criptapi.h"
#include "crdebug.h"
#include "crlimits.h"
#include "crmem.h"
#include "crobject.h"
#include "crparser.h"
#include "crvalue.h"
#include "crvm.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>



/* get instance 'i' property table */
#define proptab(i,f)	((f) != 0 ? &(i)->fields : &(i)->oclass->mtab)


/*
 * Stack size to grow the stack to when stack
 * overflow occurs for error handling.
 */
#define OVERFLOWSTACKSIZE	(CRI_MAXSTACK + 200)



/* convert stack pointers into stack offsets */
static void savestackptrs(VM *vm)
{
	SIndex *si;
	UValue *uv;
	CallFrame *cf;
	int i;

	vm->stacktop.offset = savestack(vm, vm->stacktop.p);
	for (i = 0; i < vm->frames.len; i++) {
		cf = &vm->frames.ptr[i];
		cf->callee.offset = savestack(vm, cf->callee.p);
		cf->stacktop.offset = savestack(vm, cf->stacktop.p);
	}
	for (i = 0; i < vm->callstart.len; i++) {
		si = &vm->callstart.ptr[i];
		si->offset = savestack(vm, si->p);
	}
	for (i = 0; i < vm->retstart.len; i++) {
		si = &vm->retstart.ptr[i];
		si->offset = savestack(vm, si->p);
	}
	for (uv = vm->openuv; uv != NULL; uv = uv->nextuv)
		uv->v.offset = savestack(vm, uv->v.location);
	vm->deferlist.offset = savestack(vm, vm->deferlist.p);
}


/* convert stack offsets back to stack pointers */
static void restorestackptrs(VM *vm)
{
	SIndex *si;
	UValue *uv;
	CallFrame *cf;
	int i;

	vm->stacktop.p = restorestack(vm, vm->stacktop.offset);
	for (i = 0; i < vm->frames.len; i++) {
		cf = &vm->frames.ptr[i];
		cf->callee.p = restorestack(vm, cf->callee.offset);
		cf->stacktop.p = restorestack(vm, cf->stacktop.offset);
	}
	for (i = 0; i < vm->callstart.len; i++) {
		si = &vm->callstart.ptr[i];
		si->p = restorestack(vm, si->offset);
	}
	for (i = 0; i < vm->retstart.len; i++) {
		si = &vm->retstart.ptr[i];
		si->p = restorestack(vm, si->offset);
	}
	for (uv = vm->openuv; uv != NULL; uv = uv->nextuv)
		uv->v.location = s2v(restorestack(vm, uv->v.offset));
	vm->deferlist.p = restorestack(vm, vm->deferlist.offset);
}


/* reallocate stack to new size */
int cr_vm_reallocstack(VM *vm, int size, int raiseerr)
{
	int oldstopem;
	int osize;
	SPtr newstack;
	int i;

	cr_assert(nsize <= CRI_MAXSTACK || nsize == OVERFLOWSTACKSIZE);
	oldstopem = vm->gc.stopem;
	osize = stacksize(vm);
	savestackptrs(vm);
	vm->gc.stopem = 1; /* no emergency collection when reallocating stack */
	newstack = cr_mm_reallocarray(vm, vm->stack.p,
			osize + EXTRA_STACK, size + EXTRA_STACK);
	vm->gc.stopem = oldstopem;
	if (cr_unlikely(newstack == NULL)) {
		restorestackptrs(vm);
		if (raiseerr)
			cr_assert("memory error");
		return 0;
	}
	restorestackptrs(vm);
	vm->stack.p = newstack;
	vm->stackend.p = newstack + size;
	for (i = osize + EXTRA_STACK; i < size + EXTRA_STACK; i++)
		setnilvalue(s2v(newstack + i));
	return 1;
}


/* grow stack to accommodate 'n' values */
int cr_vm_growstack(VM *vm, int n, int raiseerr)
{
	int size;
	int nsize;
	int needed;

	size = stacksize(vm);
	if (cr_unlikely(size > CRI_MAXSTACK)) { /* overflowed already ? */
		cr_assert(size == OVERFLOWSTACKSIZE);
		if (raiseerr)
			cr_assert(0 && "errerror");
		return 0;
	}
	if (cr_unlikely(n > CRI_MAXSTACK)) {
		nsize = size << 1;
		needed = topoffset(vm) + n;
		if (nsize > CRI_MAXSTACK)
			nsize = CRI_MAXSTACK;
		if (nsize < needed)
			nsize = needed;
		if (cr_likely(nsize <= CRI_MAXSTACK))
			return cr_vm_reallocstack(vm, nsize, raiseerr);
	}
	cr_vm_reallocstack(vm, OVERFLOWSTACKSIZE, raiseerr);
	if (raiseerr)
		cr_assert(0 && "stack overflow");
	return 0;
}


/* increment stack top */
void cr_vm_inctop(VM *vm)
{
	checkstack(vm, 1);
	vm->stacktop.p++;
}


/* raw property get */
cr_sinline int getproperty(VM *vm, Instance *ins, TValue *k, TValue *v, int field)
{
	return cr_ht_get(vm, proptab(ins, field), k, v);
}


/* raw property set (instance fields) */
cr_sinline int setproperty(VM *vm, Instance *ins, TValue *k, TValue *v, int field)
{
	return cr_ht_set(vm, &ins->fields, k, v);
}


/* try get vtable method 'm' */
cr_sinline GCObject *vtmethod(VM *vm, const TValue *v, int m)
{
	cr_assert(m >= 0 && m < CR_MNUM && "invalid method tag");
	return (ttisins(v) ? insvalue(v)->oclass->vtable[m] : NULL);
}


/* sets up stack for vtable method before actual call */
static void setvtmethodstack(VM *vm, SPtr callee, const TValue *ins,
				const GCObject *m, int mt)
{
	const TValue *arg;
	int arity;
	int i;

	arity = vtmi(mt)->arity;
	cr_assert(arity <= 2);
	setsv(vm, callee, &newovalue(m)); /* method */
	setsv(vm, callee + 1, ins); /* 'self' */
	for (i = 0; i < arity; i++) { /* args */
		arg = s2v(callee - arity + i);
		setsv(vm, callee + 2 + i, arg);
	}
	vm->stacktop.p = callee + arity + 2; /* assume 'EXTRA_STACK' */
}


/* try call vtable method 'm' */
int callvtmethod(VM *vm, const TValue *ins, int mt)
{
	const TValue *arg;
	const GCObject *m;
	SPtr callee;

	if ((m = vtmethod(vm, ins, mt)) == NULL)
		return 0;
	callee = vm->stacktop.p;
	setvtmethodstack(vm, callee, ins, m, mt);
	cr_vm_ncall(vm, callee, vtmi(mt)->nreturns);
	return 1;
}


/* try calling unary vtable method */
static int callunop(VM *vm, TValue *v, int mt, Value *res)
{
	GCObject *m;

	if ((m = vtmethod(vm, v, mt)) == NULL)
		return 0;
	Value *retstart = vm->sp;
	push(vm, lhs); // 'self'
	push(vm, lhs);
	ncall(vm, retstart, OBJ_VAL(om), ominfo[op].retcnt);
	*res = pop(vm); // assign and pop the method result
	return 1;
}


/* Tries to call class overloaded binary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
cr_sinline int callbinop(VM *vm, Value lhs, Value rhs, cr_om op, Value *res)
{
	Value instance;
	GCObject *om = getomethod(vm, lhs, op);
	if (om == NULL) {
		om = getomethod(vm, rhs, op);
		if (om == NULL)
			return 0;
		instance = rhs;
	} else
		instance = lhs;
	Value *retstart = vm->sp;
	push(vm, instance); // 'self'
	push(vm, lhs);
	push(vm, rhs);
	ncall(vm, retstart, OBJ_VAL(om), ominfo[op].retcnt);
	*res = pop(vm); // assign and pop the method result
	return 1;
}


/* Tries calling binary or unary overloaded operator method, errors on failure. */
void otryop(VM *vm, Value lhs, Value rhs, cr_om op, Value *res)
{
	if (!omisunop(op)) {
		if (cr_unlikely(!callbinop(vm, lhs, rhs, op, res)))
			binoperror(vm, lhs, rhs, op - OM_ADD);
	} else if (cr_unlikely(!callunop(vm, lhs, op, res)))
		unoperror(vm, lhs, op - OM_ADD);
}

cr_sinline int omcallorder(VM *vm, Value lhs, Value rhs, cr_om ordop)
{
	cr_assert(vm, ordop >= OM_NE && ordop <= OM_GE, "invalid cr_om for order");
	if (callbinop(vm, lhs, rhs, ordop, stkpeek(1))) { // try overload
		pop(vm); // remove second operand
		return 1;
	}
	// Instances (and cript objects) can always have equality comparison.
	// If their pointers are the same then the underlying objects are equal;
	// otherwise they are not equal.
	if (cr_unlikely(ordop != OM_EQ && ordop != OM_NE))
		ordererror(vm, lhs, rhs);
	return 0;
}

/* != */
void one(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(lhs != rhs));
	else if (!omcallorder(vm, lhs, rhs, OM_NE))
		push(vm, BOOL_VAL(lhs != rhs));
}

/* == */
void oeq(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(lhs == rhs));
	else if (!omcallorder(vm, lhs, rhs, OM_EQ))
		push(vm, BOOL_VAL(lhs == rhs));
}

/* < */
void olt(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) < 0));
	else
		omcallorder(vm, lhs, rhs, OM_LT);
}

/* > */
void ogt(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) > 0));
	else
		omcallorder(vm, lhs, rhs, OM_GT);
}

/* <= */
void ole(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) <= 0));
	else
		omcallorder(vm, lhs, rhs, OM_LE);
}

/* >= */
void oge(VM *vm, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(vm, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) >= 0));
	else
		omcallorder(vm, lhs, rhs, OM_GE);
}


/* Bind class method in order to preserve the receiver.
 * By doing so the interpreter can then push the receiver on the
 * stack before running the function.
 * This is needed because class methods expect the receiver to be
 * the first argument ('self' automatic var). */
cr_ubyte bindmethod(VM *vm, OClass *oclass, Value name, Value receiver)
{
	Value method;
	if (!tableget(vm, &oclass->mtab, name, &method))
		return 0;
	*stkpeek(0) = OBJ_VAL(OBoundMethod_new(vm, receiver, asclosure(method)));
	return 1;
}


/* Adjust return values after native call finishes. */
static cr_inline void moveresults(VM *vm, Value *fn, int32_t got, int32_t expect)
{
	Value *retstart = vm->sp - got; // start of return values
	if (expect == 0)
		expect = got; // all results (MULRET)
	if (got > expect)
		got = expect; // remove extra results
	memcpy(fn, retstart, got); // Safety: 'retstart' >= 'nativefn'
	for (int32_t i = got; i < expect; i++) // replace missing values with nil
		fn[i] = NIL_VAL;
	vm->sp = fn + expect;
}


/* Call native function. */
static cr_inline int32_t callnative(VM *vm, Value fn)
{
	cr_unlock(vm);
	int32_t n = ascfn(fn)->fn(vm);
	cr_lock(vm);
	criptapi_checkelems(vm, n);
	CallFrame *f = &last_frame(vm);
	moveresults(vm, f->callee, n, f->retcnt);
	vm->fc--; // pop frame
	return n;
}



/* Calls have lots of checks and there are two main reasons why.
 * cript does not allow extra arguments if the function does not
 * accept variable number of arguments and you are not allowed to
 * provide less arguments than the function arity.
 * In case 'callee' is a cript closure then just return the new 'CallFrame',
 * otherwise run the C closure and return NULL. */
static CallFrame *precall(VM *vm, Value callee, int32_t argc, int32_t retcnt)
{
	FnInfo *p = NULL;
	CallFrame *frame = &vm->frames[vm->fc];
	if (!iscfunction(callee)) {
		CriptClosure *closure = asclosure(callee);
		Function *fn = closure->fn;
		p = &fn->p;
		frame->closure = closure;
		frame->ip = fn->chunk.code.data;
		frame->cfinfo = 0;
		retcnt = (retcnt == CR_MULRET ? 0 : retcnt); // adjust return count
	} else {
		CClosure *native = ascfn(callee);
		p = &native->p;
		frame->closure = NULL;
		frame->ip = NULL;
		frame->cfinfo = CFI_CCALL;
	}
#if defined(callbitmask)
	const static void *jmptable[] = {
		&&l_stack_overflow,
		&&l_invalid_argc,
		&&l_callstack_overflow,
		&&l_ok,
	};
	uint32_t bitmask = callbitmask(vm, p->isvararg, p->arity, argc, retcnt);
	cr_ubyte idx = cr_ctz(bitmask);
	goto *jmptable[idx];
l_stack_overflow:
	retovferror(vm, p->name->storage);
l_invalid_argc:
	arityerror(vm, p->arity, argc);
l_callstack_overflow:
	fcovferror(vm);
#else
	if (cr_unlikely(!cr_ensurestack(vm, retcnt))) {
		retovferror(vm, p->name->storage);
	} else if (cr_unlikely((p->isvararg && p->arity > argc) || (!p->isvararg && p->arity != argc))) {
		arityerror(vm, p->arity, argc);
	} else if (cr_unlikely(vm->fc == VM_CALLSTACK_LIMIT)) {
		fcovferror(vm);
	} else
		goto ok;
#endif
l_ok:
	frame->vacnt = argc - p->arity;
	frame->retcnt = retcnt;
	frame->callee = vm->sp - argc - 1;
	vm->fc++;
	if (frame->cfinfo & CFI_CCALL) {
		callnative(vm, callee);
		return NULL;
	} else
		return &last_frame(vm);
}

/* Call '()' a value (closure, method, class). */
static CallFrame *call(VM *vm, Value callee, int32_t argc, int32_t retcnt)
{
	if (cr_unlikely(!IS_OBJ(callee)))
		callerror(vm, callee);
	switch (OBJ_TYPE(callee)) {
	case OBJ_BOUND_METHOD: {
		InstanceMethod *bound = asboundmethod(callee);
		vm->sp[-argc - 1] = bound->receiver; // class instance (self)
		return precall(vm, OBJ_VAL(bound->method), argc, retcnt);
	}
	case OBJ_CLASS: {
		OClass *oclass = asclass(callee);
		Value instance = OBJ_VAL(Instance_new(vm, oclass));
		if (!calloverload(vm, instance, OM_INIT)) { // not overloaded ?
			*stkpeek(argc) = instance; // 'self'
			int32_t arity = ominfo[OM_INIT].arity; // default arity
			if (cr_unlikely(argc != arity))
				arityerror(vm, arity, argc);
		}
		return NULL;
	}
	case OBJ_CLOSURE:
	case OBJ_FUNCTION:
	case OBJ_CFUNCTION:
		return precall(vm, callee, argc, retcnt);
	default:
		cr_unreachable;
	}
}

/* call value (unprotected). */
void cr_vm_ncall(VM *vm, SPtr callee, int nreturns)
{
	int argc = vm->stacktop.p - callee - 1;
	if (call(vm, fn, argc, retcnt) != NULL)
		run(vm);
}


/* Protected call with longjmp.
 * Performs a protected call, calling the wrapper 'ProtectedFn' around
 * a cript function or native C function.
 * Returns status of the called function, this status is modified
 * by function that errors and performs the long jump or it
 * stays unchanged and the wrapper function just returns and
 * execution continues. */
static cr_inline int32_t protectedcall(VM *vm, ProtectedFn fn, void *userdata)
{
	int32_t oldfc = vm->fc;
	struct cr_longjmp lj;
	lj.status = S_OK;
	lj.prev = vm->errjmp;
	vm->errjmp = &lj;
	if (setjmp(lj.buf) == 0) // setter ?
		(*fn)(vm, userdata); // perform the call
	vm->errjmp = lj.prev;
	vm->fc = oldfc;
	return lj.status;
}

/* Public interface to 'protectedcall'.
 * In case of errors it performs a recovery by closing all
 * open upvalues (values to be closed) and restoring the
 * old stack pointer (oldtop). */
int32_t pcall(VM *vm, ProtectedFn fn, void *userdata, ptrdiff_t oldtop)
{
	int8_t status = protectedcall(vm, fn, userdata);
	if (cr_unlikely(status != S_OK)) {
		closeupval(vm, vm->sp);
		Value *oldsp = restore_stack(vm, oldtop);
		*oldsp = vm->sp[-1];
		vm->sp = oldsp + 1;
	}
	return status;
}


/* Private to the interpreter.
 * Tries to call the superclass method 'name'. */
static cr_inline void invokefrom(VM *vm, OClass *oclass, Value name, int32_t argc, int32_t retcnt)
{
	Value method;
	if (cr_unlikely(!rawget(vm, &oclass->mtab, name, &method)))
		udperror(vm, name, oclass);
	call(vm, method, argc, retcnt);
}


/* Check if receiver and key are valid for indexing. */
static cr_inline void checkindex(VM *vm, Value receiver, Value key)
{
	if (cr_unlikely(!isinstance(receiver)))
		ipaerror(vm, receiver);
	else if (cr_unlikely(IS_NIL(key)))
		nilidxerror(vm);
}


/* Private to interpreter.
 * Invokes the field/method of the instance class directly in a single
 * instruction.
 * First it tries to find the field with the 'name', if it was not
 * found it calls the method of its own class or errors if the method
 * was not found. */
static cr_inline void invoke(VM *vm, Value name, int32_t argc, int32_t retcnt)
{
	Value receiver = *stkpeek(argc);
	if (cr_unlikely(!isinstance(receiver)))
		ipaerror(vm, receiver);
	Instance *instance = asinstance(receiver);
	Value field;
	if (rawget(vm, &instance->fields, name, &field)) {
		*stkpeek(argc) = field; // swap receiver with field
		call(vm, field, argc, retcnt);
	}
	invokefrom(vm, instance->oclass, name, argc, retcnt);
}

/* Private to interpreter.
 * Used when creating a cript closure. */
static cr_inline OUpvalue *captureupval(VM *vm, Value *valp)
{
	OUpvalue **upvalpp = &vm->open_upvals;
	while (*upvalpp != NULL && (*upvalpp)->location > valp)
		upvalpp = &(*upvalpp)->next;
	if (*upvalpp != NULL && (*upvalpp)->location == valp)
		return *upvalpp;
	OUpvalue *upvalp = OUpvalue_new(vm, valp);
	upvalp->next = *upvalpp;
	*upvalpp = upvalp;
	return upvalp;
}

/* Closes all of the captured variables moving
 * them from the stack onto the heap (open_upvals array),
 * making them reachable for gc. */
void closeupval(VM *vm, Value *last)
{
	while (vm->openuvals != NULL && vm->openuvals->location >= last) {
		OUpvalue *upvalp = vm->open_upvals;
		upvalp->closed = *upvalp->location;
		upvalp->location = &upvalp->closed;
		vm->open_upvals = upvalp->next;
	}
}

/**
 * Searches the entire table for the matching index in order to
 * provide more descriptive runtime error.
 **/
CRString *globalname(VM *vm, uint32_t idx)
{
	for (uint32_t i = 0; i < vm->globids.cap; i++) {
		Entry *entry = &vm->globids.entries[i];
		if (!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
			return (OString *)asobj(entry->key);
	}
	cr_unreachable;
}


#define BINARY_OP(vm, op)                \
	do {                             \
		savepc();                \
		Value *l = stkpeek(1);   \
		Value r = *stkpeek(0);   \
		arith(vm, *l, r, op, l); \
	} while (0)
#define UNARY_OP(vm, op)                       \
	do {                                   \
		savepc();                      \
		Value *l = stkpeek(0);         \
		arith(vm, *l, NIL_VAL, op, l); \
	} while (0)
#define ORDER_OP(vm, fnop)             \
	do {                           \
		savepc();              \
		Value l = *stkpeek(1); \
		Value r = *stkpeek(0); \
		fnop(vm, l, r);        \
	} while (0)





/* -------------------------------------------------------------------------
 * Interpreter loop
 * -------------------------------------------------------------------------- */


/* save program counter */
#define savepc()	((cf)->pc = pc)

/* save program counter and stack top */
#define savestate()	(savepc(), (vm)->stacktop.p = (cf)->stacktop.p)

/* protect code that can raise errors or change the stack */
#define protect(e)	(savestate(), (e))


/* fetch an instruction */
#define fetch()		(*(pc)++)

/* fetch short instruction parameter */
#define shortparam()	fetch()

/* fetch long instruction parameter */
#define longparam()	((ip) += 3, get3bytes((ip) - 3))


/* get constant */
#define GETK()		(cffn(cf)->constants.ptr[longparam(pc)])

/* get string constant */
#define GETSTRK()	(strvalue(GETK(cf, pc)))


#define DISPATCH(x)	switch(x)
#define CASE(l)		case l:
#define BREAK		break


void run(VM *vm)
{
	register CallFrame *cf;
	register const Instruction *pc;
	int codeparam1;
	int codeparam2;
	int codeparam3;

	cf = vm->aframe;
	pc = frame->pc;
	for (;;) {
#ifdef PRECOMPUTED_GOTO
#include "crjmptable.h"
#endif
		DISPATCH(fetch(pc)) {
			CASE(OP_TRUE) {
				setbtvalue(s2v(vm->stacktop.p++));
				BREAK;
			}
			CASE(OP_FALSE) {
				setbfvalue(s2v(vm->stacktop.p++));
				BREAK;
			}
			CASE(OP_NIL) {
				setnilvalue(s2v(vm->stacktop.p++));
				BREAK;
			}
			CASE(OP_NILN) {
				codeparam1 = longparam();
				while (codeparam1--)
					setnilvalue(s2v(vm->stacktop.p++));
				BREAK;
			} CASE(OP_ADD) {
				BINARY_OP(vm, AR_ADD);
				BREAK;
			}
			CASE(OP_SUB) {
				BINARY_OP(vm, AR_SUB);
				BREAK;
			}
			CASE(OP_MUL) {
				BINARY_OP(vm, AR_MUL);
				BREAK;
			}
			CASE(OP_MOD) {
				BINARY_OP(vm, AR_MOD);
				BREAK;
			}
			CASE(OP_POW) {
				BINARY_OP(vm, AR_POW);
				BREAK;
			}
			CASE(OP_DIV) {
				BINARY_OP(vm, AR_DIV);
				BREAK;
			}
			CASE(OP_NEG) {
				UNARY_OP(vm, AR_UMIN);
				BREAK;
			}
			CASE(OP_NOT) {
				UNARY_OP(vm, AR_NOT);
				BREAK;
			}
			CASE(OP_VARARG)
			{
				savepc();
				Function *fn = FFN(frame);
				uint32_t vacnt = READ_BYTEL();
				if (vacnt == 0)
					vacnt = frame->vacnt;
				for (uint32_t i = 1; i <= vacnt; i++) {
					Value *next = frame->callee + fn->p.arity + i;
					push(vm, *next);
				}
				BREAK;
			}
			CASE(OP_NOT_EQUAL)
			{
				ORDER_OP(vm, vne);
				BREAK;
			}
			CASE(OP_EQUAL)
			{
				ORDER_OP(vm, veq);
				BREAK;
			}
			CASE(OP_EQ) // same as OP_EQUAL except we don't pop the first operand
			{
				ORDER_OP(vm, eq_preserveL);
				BREAK;
			}
			CASE(OP_GREATER)
			{
				ORDER_OP(vm, vgt);
				BREAK;
			}
			CASE(OP_GREATER_EQUAL)
			{
				ORDER_OP(vm, vge);
				BREAK;
			}
			CASE(OP_LESS)
			{
				ORDER_OP(vm, vle);
				BREAK;
			}
			CASE(OP_LESS_EQUAL)
			{
				ORDER_OP(vm, vle);
				BREAK;
			}
			CASE(OP_POP)
			{
				pop(vm);
				BREAK;
			}
			CASE(OP_POPN)
			{
				popn(vm, READ_BYTEL());
				BREAK;
			}
			CASE(OP_CONST)
			{
				savepc();
				push(vm, READ_CONSTANT());
				BREAK;
			}
			{
				int32_t argc; // shared
				CASE(OP_CALL0)
				{
					argc = 0;
					goto l_call;
				}
				CASE(OP_CALL1)
				{
					argc = 1;
					goto l_call;
				}
				CASE(OP_CALL)
				{
					argc = cast(int32_t, vm->sp - Array_VRef_pop(&vm->callstart));
l_call:;
					{
						int32_t retcnt = READ_BYTEL();
						Value callee = *stkpeek(argc);
						savepc();
						call(vm, callee, argc, retcnt);
						updatestate();
						BREAK;
					}
				}
			}
			CASE(OP_METHOD)
			{
				Value methodname = READ_CONSTANT();
				Value method = *stkpeek(0); // OClosure or ONative
				OClass *oclass = asclass(*stkpeek(1));
				rawset(vm, &oclass->mtab, methodname, method);
				pop(vm); // pop method
				BREAK;
			}
			{
				int32_t argc; // shared
				CASE(OP_INVOKE0)
				{
					argc = 0;
					goto l_invoke;
				}
				CASE(OP_INVOKE1)
				{
					argc = 1;
					goto l_invoke;
				}
				CASE(OP_INVOKE)
				{
					argc = cast(int32_t, vm->sp - Array_VRef_pop(&vm->callstart));
l_invoke:;
					{
						Value methodname = READ_CONSTANT();
						int32_t retcnt = READ_BYTEL();
						savepc();
						invoke(vm, methodname, argc, retcnt);
						updatestate();
						BREAK;
					}
				}
			}
			CASE(OP_GET_SUPER)
			{
				cr_assert(vm, isclassobj(*stkpeek(0)), "Expect OClass.");
				Value methodname = READ_CONSTANT();
				OClass *superclass = asclass(pop(vm));
				savepc();
				if (cr_unlikely(!bindmethod(vm, superclass, methodname, *stkpeek(0))))
					udperror(vm, methodname, superclass);
				BREAK;
			}
			{
				int32_t argc; // shared
				CASE(OP_INVOKE_SUPER0)
				{
					argc = 0;
					goto l_invoke_super;
				}
				CASE(OP_INVOKE_SUPER1)
				{
					argc = 1;
					goto l_invoke_super;
				}
				CASE(OP_INVOKE_SUPER)
				{
					argc = cast(int32_t, vm->sp - Array_VRef_pop(&vm->callstart));
l_invoke_super:;
					{
						Value methodname = READ_CONSTANT();
						int32_t retcnt = READ_BYTEL();
						cr_assert(vm, isclassobj(*stkpeek(0)), "superclass must be class.");
						OClass *superclass = asclass(pop(vm));
						savepc();
						invokefrom(vm, superclass, methodname, argc, retcnt);
						updatestate();
						BREAK;
					}
				}
			}
			CASE(OP_SET_PROPERTY) // 'instance.property_name = property'
			{
				Value property_name = READ_CONSTANT();
				Value property = *stkpeek(0);
				Value receiver = *stkpeek(1);
				if (cr_unlikely(!isinstance(receiver))) {
					savepc();
					ipaerror(vm, receiver);
				}
				Instance *instance = asinstance(receiver);
				rawset(vm, &instance->fields, property_name, property);
				popn(vm, 2); // instance + property
				BREAK;
			}
			CASE(OP_GET_PROPERTY) // 'instance.property_name'
			{
				Value property_name = READ_CONSTANT();
				Value receiver = *stkpeek(0);
				if (cr_unlikely(!isinstance(receiver))) {
					savepc();
					ipaerror(vm, receiver);
				}
				Instance *instance = asinstance(receiver);
				Value property;
				if (rawget(vm, &instance->fields, property_name, &property)) {
					*stkpeek(0) = property;
					BREAK;
				}
				savepc();
				if (cr_unlikely(!bindmethod(vm, instance->oclass, property_name, receiver)))
					udperror(vm, property_name, instance->oclass);
				BREAK;
			}
			{
				int32_t bytecode_param; // shared
				CASE(OP_DEFINE_GLOBAL)
				{
					bytecode_param = READ_BYTE();
					goto l_define_global;
				}
				CASE(OP_DEFINE_GLOBALL)
				{
					bytecode_param = READ_BYTEL();
					goto l_define_global;
				}
l_define_global:;
				{
					Value *gvalue = &vm->globvars.data[bytecode_param].value;
					if (cr_unlikely(*gvalue != EMPTY_VAL)) {
						savepc();
						redefgerror(vm, globalname(vm, bytecode_param)->storage);
					}
					*gvalue = pop(vm);
					BREAK;
				}
				CASE(OP_GET_GLOBAL)
				{
					bytecode_param = READ_BYTE();
					goto l_get_global;
				}
				CASE(OP_GET_GLOBALL)
				{
					bytecode_param = READ_BYTEL();
					goto l_get_global;
				}
l_get_global:;
				{
					Value *gvalue = &vm->globvars.data[bytecode_param].value;
					if (cr_unlikely(*gvalue == EMPTY_VAL)) {
						savepc();
						udgerror(vm, globalname(vm, bytecode_param)->storage);
					}
					push(vm, *gvalue);
					BREAK;
				}
				CASE(OP_SET_GLOBAL)
				{
					bytecode_param = READ_BYTE();
					goto l_set_global;
				}
				CASE(OP_SET_GLOBALL)
				{
					bytecode_param = READ_BYTEL();
					goto l_set_global;
				}
l_set_global:;
				{
					Variable *gvar = &vm->globvars.data[bytecode_param];
					if (cr_unlikely(gvar->value == EMPTY_VAL)) {
						savepc();
						udgerror(vm, globalname(vm, bytecode_param)->storage);
					} else if (cr_unlikely(VISCONST(gvar))) {
						savepc();
						fixederror(vm, globalname(vm, bytecode_param)->storage);
					}
					gvar->value = pop(vm);
					BREAK;
				}
				CASE(OP_GET_LOCAL)
				{
					bytecode_param = READ_BYTE();
					goto l_get_local;
				}
				CASE(OP_GET_LOCALL)
				{
					bytecode_param = READ_BYTEL();
					goto l_get_local;
				}
l_get_local:;
				{
					push(vm, frame->callee[bytecode_param]);
					BREAK;
				}
				CASE(OP_SET_LOCAL)
				{
					bytecode_param = READ_BYTE();
					goto l_set_local;
				}
				CASE(OP_SET_LOCALL)
				{
					bytecode_param = READ_BYTEL();
					goto l_set_local;
				}
l_set_local:;
				{
					frame->callee[bytecode_param] = pop(vm);
					BREAK;
				}
			}
			CASE(OP_JMP_IF_FALSE) // unused, using 'optimized' versions with pop
			{
				cr_unreachable;
				uint32_t skip_offset = READ_BYTEL();
				ip += ISFALSE(*stkpeek(0)) * skip_offset;
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP_IF_FALSE_POP)
			{
				UNARY_OP(vm, AR_NOT);
				uint32_t skip_offset = READ_BYTEL();
				ip += ISFALSE(*stkpeek(0)) * skip_offset;
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				pop(vm);
				BREAK;
			}
			CASE(OP_JMP_IF_FALSE_OR_POP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += (ISFALSE(*stkpeek(0)) ? skip_offset : (pop(vm), 0));
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP_IF_FALSE_AND_POP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += (ISFALSE(*stkpeek(0)) ? (pop(vm), skip_offset) : 0);
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += skip_offset;
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP_AND_POP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += skip_offset;
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				pop(vm);
				BREAK;
			}
			CASE(OP_LOOP)
			{
				uint32_t offset = READ_BYTEL();
				ip -= offset;
				cr_assert(vm, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_CLOSURE)
			{
				Function *fn = asfn(READ_CONSTANT());
				CriptClosure *closure = OClosure_new(vm, fn);
				push(vm, OBJ_VAL(closure));
				for (uint32_t i = 0; i < closure->fn->p.upvalc; i++) {
					cr_ubyte local = READ_BYTE();
					uint32_t idx = READ_BYTEL();
					if (local)
						closure->upvalue[i] = captureupval(vm, frame->callee + idx);
					else
						closure->upvalue[i] = frame->closure->upvalue[idx];
				}
				BREAK;
			}
			CASE(OP_GET_UPVALUE)
			{
				uint32_t idx = READ_BYTEL();
				push(vm, *frame->closure->upvalue[idx]->location);
				BREAK;
			}
			CASE(OP_SET_UPVALUE)
			{
				uint32_t idx = READ_BYTEL();
				*frame->closure->upvalue[idx]->location = pop(vm);
				BREAK;
			}
			CASE(OP_CLOSE_UPVAL)
			{
				closeupval(vm, vm->sp - 1);
				pop(vm);
				BREAK;
			}
			CASE(OP_CLOSE_UPVALN)
			{
				uint32_t last = READ_BYTEL();
				closeupval(vm, vm->sp - last);
				popn(vm, last);
				BREAK;
			}
			CASE(OP_CLASS)
			{
				push(vm, OBJ_VAL(OClass_new(vm, READ_STRING())));
				BREAK;
			}
			CASE(OP_INDEX) // 'instance[key]'
			{
				Value receiver = *stkpeek(1);
				Value key = *stkpeek(0);
				savepc();
				checkindex(vm, receiver, key);
				if (!calloverload(vm, receiver, OM_GETIDX)) { // not overloaded ?
					Instance *instance = asinstance(receiver);
					Value property;
					if (tableget(vm, &instance->fields, key, &property)) {
						*stkpeek(1) = property; // replace receiver with field value
						pop(vm); // pop key
						BREAK;
					} // else try get method
					if (cr_unlikely(!bindmethod(vm, instance->oclass, key, receiver)))
						udperror(vm, key, instance->oclass);
					*stkpeek(1) = pop(vm); // replace receiver with popped method
				} else
					updatestate();
				BREAK;
			}
			CASE(OP_SET_INDEX) // 'instance[key] = value;'
			{
				Value receiver = *stkpeek(2);
				Value key = *stkpeek(1);
				Value value = *stkpeek(0);
				savepc();
				checkindex(vm, receiver, key);
				Instance *instance = asinstance(receiver);
				if (!calloverload(vm, receiver, OM_SETIDX)) { // not overloaded ?
					tableset(vm, &instance->fields, key, value);
					popn(vm, 3); // pop instance, key and value
				} else
					updatestate();
				BREAK;
			}
			CASE(OP_OVERLOAD) // 'fn __omethod__ { ... }'
			{
				cr_om tag = READ_BYTE();
				OClass *oclass = asclass(*stkpeek(1));
				oclass->omethods[tag] = asobj(pop(vm));
				cr_assert(vm, *ip == OP_METHOD, "Expected 'OP_METHOD'.");
				BREAK;
			}
			CASE(OP_INHERIT) // 'class A impl B { ... }'
			{
				cr_assert(vm, isclassobj(*stkpeek(0)), "subclass must be class.");
				OClass *subclass = asclass(*stkpeek(0));
				Value superclass = *stkpeek(1);
				if (cr_unlikely(!isclassobj(superclass))) {
					savepc();
					inheriterror(vm, superclass);
				}
				HTable_into(vm, &asclass(superclass)->mtab, &subclass->mtab, 0);
				memcpy(subclass->vtable, asclass(superclass)->vtable, sizeof(subclass->vtable));
				pop(vm); // pop subclass
				BREAK;
			}
			CASE(OP_FOREACH_PREP)
			{
				int32_t vars = READ_BYTEL();
				memcpy(vm->sp, stkpeek(2), 3 * sizeof(Value));
				vm->sp += 3;
				savepc();
				Value fn = *stkpeek(2);
				call(vm, fn, 2, vars);
				updatestate();
				BREAK;
			}
			CASE(OP_FOREACH)
			{
				int32_t vars = READ_BYTEL();
				Value *cntlvar = stkpeek(vars);
				*cntlvar = *stkpeek(vars - 1);
				cr_assert(vm, *ip == OP_JMP, "Expect 'OP_JMP'.");
				if (!IS_NIL(*cntlvar))
					ip += 4;
				BREAK;
			}
			CASE(OP_CALLSTART)
			{
				Array_VRef_push(&vm->callstart, vm->sp);
				BREAK;
			}
			CASE(OP_RETSTART)
			{
				Array_VRef_push(&vm->retstart, vm->sp);
				BREAK;
			}
			CASE(OP_RET0) // should not return anything
			{
				// When returning from overload-able methods that do
				// not have return value (such as '__setidx__').
				frame->retcnt = 0;
				goto l_ret;
			}
			CASE(OP_RET1) // single value return
			{
				// Stack already contains only a single return value;
				// this instruction comes in handy when returning from
				// overloaded method.
				// Because grammar and parser ensure there can only be
				// a single return value from __init__, __getidx__, __add__,
				// __sub__, __eq__, etc...
				// So instead of shuffling and checking if stack has enough
				// or lack of values, we skip the check instead.
				cr_assert(vm, frame->retcnt == 1, "invalid retcnt");
				goto l_ret;
			}
			CASE(OP_RET) // function return
			{
				int32_t retvalcnt, unaccounted;
				retvalcnt = cast(int32_t, vm->sp - Array_VRef_pop(&vm->retstart));
				if (frame->retcnt == 0) { // multiple return values ?
					unaccounted = 0;
					frame->retcnt = retvalcnt;
				} else
					unaccounted = frame->retcnt - retvalcnt;
				if (unaccounted < 0)
					popn(vm, -unaccounted);
				else
					pushn(vm, unaccounted, NIL_VAL);
l_ret:
				savepc();
				cr_assert(vm, vm->temp.len == 0, "Temporary array not empty.");
				for (int32_t returns = frame->retcnt; returns--;)
					Array_Value_push(&vm->temp, *--vm->sp);
				closeupval(vm, frame->callee); // close any open upvalues
				vm->fc--; // pop the frame
				vm->sp = frame->callee; // adjust the stack pointer
				while (vm->temp.len > 0) // push return values
					push(vm, Array_Value_pop(&vm->temp));
				cr_assert(vm, vm->temp.len == 0, "Temporary array not empty.");
				if (last_frame(vm).cfinfo & CFI_FRESH)
					return;
				cr_assert(vm, vm->fc > 0, "Invalid cfinfo.");
				updatestate();
				BREAK;
			}
		}
	}

	cr_unreachable;

#undef READ_BYTE
#undef READ_BYTEL
#undef READ_CONSTANT
#undef READ_CONSTANTL
#undef READ_STRING
#undef READ_STRINGL
#undef DISPATCH
#undef CASE
#undef BREAK
#undef VM_BINARY_OP
}


/* cr_interprets (compiles and runs) the 'source'. */
void interpret(VM *vm, const char *source, const char *path)
{
	TODO("Refactor")
	Value name = OBJ_VAL(OString_new(vm, path, strlen(path)));
	CriptClosure *closure = NULL; // TODO: compile(vm, source, name, true);
	if (closure == NULL)
		printandpanic(vm);
	cr_pcall(vm, 0, 0);
}


/* Initialize the allocated VM */
void VM_init(VM *vm)
{
	srand(time(0));
	vm->seed = rand();
	vm->fc = 0;
	vm->objects = NULL;
	vm->openuvals = NULL;
	vm->gc.heapmin = GC_HEAP_MIN;
	vm->gc.nextgc = GC_HEAP_INIT; // 1 MiB
	vm->gc.allocated = 0;
	vm->gc.growfactor = GC_HEAP_GROW_FACTOR;
	vm->gc.stopped = 0;
	vm->sp = vm->stack;
	vm->gs = NULL;
	vm->gslen = 0;
	vm->gscap = 0;
	HTable_init(&vm->loaded); // Loaded scripts and their functions
	HTable_init(&vm->globids); // Global variable identifiers
	Array_Variable_init(&vm->globvars, vm);
	Array_Value_init(&vm->temp, vm); // Temp values storage (return values)
	Array_VRef_init(&vm->callstart, vm);
	Array_VRef_init(&vm->retstart, vm);
	Array_OSRef_init(&vm->interned, vm);
	HTable_init(&vm->weakrefs); // cr_interned strings table (Weak_refs)
	memset(vm->faststatic, 0, sizeof(vm->faststatic));
	for (cr_ubyte i = 0; i < SS_SIZE; i++)
		vm->faststatic[i] = OString_new(vm, static_strings[i].name, static_strings[i].len);
}



/*
 * Reset virtual machine call stack and close all upvalues.
 * Additionally set the error object on top of the stack
 * if the 'status' is error code.
 */
void resetvm(VM *vm, cr_status status)
{
	Value *top = vm->stack;
	closeupval(vm, top + 1); // close all open upvalues
	vm->fc = 0; // reset call stack
	if (status != S_OK) {
		if (status == S_EMEM)
			*top = OBJ_VAL(vm->memerror);
		else
			*top = *stkpeek(0); // err obj on top
		vm->sp = top + 1;
	} else
		vm->sp = top;
}


/*
 * Frees the VM and nulls out its pointer.
 */
void cleanvm(VM **vmp)
{
	_cleanup_function((*vmp)->F);
	cr_destroy(vmp);
}


/* statics (check 'crstatics.h') */
CRI_DEF const StaticString SS[CR_SSNUM] = {
	/* method names */
	{ "__init__", SLL("__init__") },
	{ "__tostring__", SLL("__tostring__") },
	{ "__getidx__", SLL("__getidx__") },
	{ "__setidx__", SLL("__setidx__") },
	{ "__gc__", SLL("__gc__") },
	{ "__defer__", SLL("__defer__") },
	{ "__hash__", SLL("__hash__") },
	{ "__add__", SLL("__add__") },
	{ "__sub__", SLL("__sub__") },
	{ "__mul__", SLL("__mul__") },
	{ "__div__", SLL("__div__") },
	{ "__mod__", SLL("__mod__") },
	{ "__pow__", SLL("__pow__") },
	{ "__not__", SLL("__not__") },
	{ "__umin__", SLL("__umin__") },
	{ "__ne__", SLL("__ne__") },
	{ "__eq__", SLL("__eq__") },
	{ "__lt__", SLL("__lt__") },
	{ "__le__", SLL("__le__") },
	{ "__gt__", SLL("__gt__") },
	{ "__ge__", SLL("__ge__") },
};
