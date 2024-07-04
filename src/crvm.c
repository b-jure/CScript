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
#include "cript.h"
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
static void savestackptrs(cr_State *ts)
{
	SIndex *si;
	UValue *uv;
	CallFrame *cf;
	int i;

	ts->stacktop.offset = savestack(ts, ts->stacktop.p);
	for (i = 0; i < ts->frames.len; i++) {
		cf = &ts->frames.ptr[i];
		cf->callee.offset = savestack(ts, cf->callee.p);
		cf->top.offset = savestack(ts, cf->top.p);
	}
	for (i = 0; i < ts->callstart.len; i++) {
		si = &ts->callstart.ptr[i];
		si->offset = savestack(ts, si->p);
	}
	for (i = 0; i < ts->retstart.len; i++) {
		si = &ts->retstart.ptr[i];
		si->offset = savestack(ts, si->p);
	}
	for (uv = ts->openuv; uv != NULL; uv = uv->u.open.nextuv)
		uv->v.offset = savestack(ts, uv->v.location);
	ts->tbclist.offset = savestack(ts, ts->tbclist.p);
}


/* convert stack offsets back to stack pointers */
static void restorestackptrs(cr_State *ts)
{
	SIndex *si;
	UValue *uv;
	CallFrame *cf;
	int i;

	ts->stacktop.p = restorestack(ts, ts->stacktop.offset);
	for (i = 0; i < ts->frames.len; i++) {
		cf = &ts->frames.ptr[i];
		cf->callee.p = restorestack(ts, cf->callee.offset);
		cf->top.p = restorestack(ts, cf->top.offset);
	}
	for (i = 0; i < ts->callstart.len; i++) {
		si = &ts->callstart.ptr[i];
		si->p = restorestack(ts, si->offset);
	}
	for (i = 0; i < ts->retstart.len; i++) {
		si = &ts->retstart.ptr[i];
		si->p = restorestack(ts, si->offset);
	}
	for (uv = ts->openuv; uv != NULL; uv = uv->u.open.nextuv)
		uv->v.location = s2v(restorestack(ts, uv->v.offset));
	ts->tbclist.p = restorestack(ts, ts->tbclist.offset);
}


/* reallocate stack to new size */
int cr_vm_reallocstack(cr_State *ts, int size, int raiseerr)
{
	GState *gs;
	int oldstopem;
	int osize;
	SPtr newstack;
	int i;

	cr_assert(nsize <= CRI_MAXSTACK || nsize == OVERFLOWSTACKSIZE);
	gs = GS(ts);
	oldstopem = gs->gc.stopem;
	osize = stacksize(ts);
	savestackptrs(ts);
	gs->gc.stopem = 1; /* no emergency collection when reallocating stack */
	newstack = cr_mem_reallocarray(ts, ts->stack.p,
			osize + EXTRA_STACK, size + EXTRA_STACK);
	gs->gc.stopem = oldstopem;
	if (cr_unlikely(newstack == NULL)) {
		restorestackptrs(ts);
		if (raiseerr)
			cr_assert("memory error");
		return 0;
	}
	restorestackptrs(ts);
	ts->stack.p = newstack;
	ts->stackend.p = newstack + size;
	for (i = osize + EXTRA_STACK; i < size + EXTRA_STACK; i++)
		setnilvalue(s2v(newstack + i));
	return 1;
}


/* grow stack to accommodate 'n' values */
int cr_ts_growstack(cr_State *ts, int n, int raiseerr)
{
	int size;
	int nsize;
	int needed;

	size = stacksize(ts);
	if (cr_unlikely(size > CRI_MAXSTACK)) { /* overflowed already ? */
		cr_assert(size == OVERFLOWSTACKSIZE);
		if (raiseerr)
			cr_assert(0 && "errerror");
		return 0;
	}
	if (cr_unlikely(n > CRI_MAXSTACK)) {
		nsize = size << 1;
		needed = topoffset(ts) + n;
		if (nsize > CRI_MAXSTACK)
			nsize = CRI_MAXSTACK;
		if (nsize < needed)
			nsize = needed;
		if (cr_likely(nsize <= CRI_MAXSTACK))
			return cr_vm_reallocstack(ts, nsize, raiseerr);
	}
	cr_vm_reallocstack(ts, OVERFLOWSTACKSIZE, raiseerr);
	if (raiseerr)
		cr_assert(0 && "stack overflow");
	return 0;
}


static int stackinuse(cr_State *ts)
{
	CallFrame *cf;
	SPtr maxtop;
	int n;
	int i;

	maxtop = ts->aframe->top.p;
	for (i = 0; i < ts->frames.len; i++) {
		cf = &ts->frames.ptr[i];
		if (maxtop < cf->top.p)
			maxtop = cf->top.p;
	}
	cr_assert(maxtop <= ts->stackend.p + EXTRA_STACK);
	n = savestack(ts, maxtop);
	if (n < CR_MINSTACK)
		n = CR_MINSTACK;
	return n;
}

/*
 * Shrink stack if the current stack size is more
 * than 3 times the current use.
 * This also rolls back the stack to its original maximum
 * size 'CRI_MAXSTACK' in case the stack was previously
 * handling stack overflow.
 */
void cr_vm_shrinkstack(cr_State *ts)
{
	int inuse;
	int limit;
	int nsize;

	inuse = stackinuse(ts);
	limit = (inuse >= CRI_MAXSTACK / 3 ? CRI_MAXSTACK : inuse * 3);
	if (inuse <= CRI_MAXSTACK && stacksize(ts) > limit) {
		nsize = (inuse < (CRI_MAXSTACK>>1) ? (inuse<<1) : CRI_MAXSTACK);
		cr_vm_reallocstack(ts, nsize, 0); /* fine if it fails */
	}
}


/* increment stack top */
void cr_ts_inctop(cr_State *ts)
{
	checkstack(ts, 1);
	ts->stacktop.p++;
}


/* 
 * Integer division; handles division by 0 and possible
 * overflow if 'y' == '-1' and 'x' == CR_INTEGER_MIN.
 */
cr_integer cr_ts_div(cr_State *ts, cr_integer x, cr_integer y)
{
	if (cr_unlikely(cri_castS2U(y) + 1 <= 1)) { /* y == '0' or '-1' */
		if (y == 0)
			cr_dg_runerror(ts, "division by 0");
		return cr_intop(-, 0, x);
	}
	return (x / y);
}


/*
 * Integer modulus; handles modulo by 0 and overflow
 * as explained in 'cr_vm_div()'.
 */
cr_integer cr_ts_mod(cr_State *ts, cr_integer x, cr_integer y) 
{
	cr_integer r;

	if (cr_unlikely(cri_castS2U(y) + 1 <= 1)) {
		if (y == 0)
			cr_dg_runerror(ts, "attempt to x%%0");
		return 0;
	}
	cri_nummod(ts, x, y, r);
	return r;
}


/* floating point modulus */
cr_number cr_ts_modnum(cr_State *ts, cr_number x, cr_number y)
{
	cr_number r;
	cri_nummod(ts, x, y, r);
	return r;
}




/* raw property get */
cr_sinline int getproperty(cr_State *ts, Instance *ins, TValue *k, TValue *v, int field)
{
	return cr_htable_get(proptab(ins, field), k, v);
}


/* raw property set (instance fields) */
cr_sinline int setproperty(cr_State *ts, Instance *ins, TValue *k, TValue *v, int field)
{
	return cr_htable_set(ts, &ins->fields, k, v);
}


/* try get vtable method 'm' */
cr_sinline GCObject *vtmethod(cr_State *ts, const TValue *v, int m)
{
	cr_assert(m >= 0 && m < CR_MNUM && "invalid method tag");
	return (ttisins(v) ? insvalue(v)->oclass->vtable[m] : NULL);
}


/* sets up stack for vtable method before actual call */
static void setvtmethodstack(cr_State *ts, SPtr callee, const TValue *ins,
				const GCObject *m, int mt)
{
	const TValue *arg;
	int arity;
	int i;

	arity = vtmi(mt)->arity;
	cr_assert(arity <= 2);
	setsv(ts, callee, &newovalue(m)); /* method */
	setsv(ts, callee + 1, ins); /* 'self' */
	for (i = 0; i < arity; i++) { /* args */
		arg = s2v(callee - arity + i);
		setsv(ts, callee + 2 + i, arg);
	}
	ts->stacktop.p = callee + arity + 2; /* assume 'EXTRA_STACK' */
}


/* try call vtable method 'm' */
int callvtmethod(cr_State *ts, const TValue *ins, int mt)
{
	const TValue *arg;
	const GCObject *m;
	SPtr callee;

	if ((m = vtmethod(ts, ins, mt)) == NULL)
		return 0;
	callee = ts->stacktop.p;
	setvtmethodstack(ts, callee, ins, m, mt);
	cr_ts_ncall(ts, callee, vtmi(mt)->nreturns);
	return 1;
}


/* try calling unary vtable method */
static int callunop(cr_State *ts, TValue *v, int mt, Value *res)
{
	GCObject *m;

	if ((m = vtmethod(ts, v, mt)) == NULL)
		return 0;
	Value *retstart = ts->sp;
	push(ts, lhs); // 'self'
	push(ts, lhs);
	ncall(ts, retstart, OBJ_VAL(om), ominfo[op].retcnt);
	*res = pop(ts); // assign and pop the method result
	return 1;
}


/* Tries to call class overloaded binary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
cr_sinline int callbinop(cr_State *ts, Value lhs, Value rhs, cr_om op, Value *res)
{
	Value instance;
	GCObject *om = getomethod(ts, lhs, op);
	if (om == NULL) {
		om = getomethod(ts, rhs, op);
		if (om == NULL)
			return 0;
		instance = rhs;
	} else
		instance = lhs;
	Value *retstart = ts->sp;
	push(ts, instance); // 'self'
	push(ts, lhs);
	push(ts, rhs);
	ncall(ts, retstart, OBJ_VAL(om), ominfo[op].retcnt);
	*res = pop(ts); // assign and pop the method result
	return 1;
}


/* Tries calling binary or unary overloaded operator method, errors on failure. */
void otryop(cr_State *ts, Value lhs, Value rhs, cr_om op, Value *res)
{
	if (!omisunop(op)) {
		if (cr_unlikely(!callbinop(ts, lhs, rhs, op, res)))
			binoperror(ts, lhs, rhs, op - OM_ADD);
	} else if (cr_unlikely(!callunop(ts, lhs, op, res)))
		unoperror(ts, lhs, op - OM_ADD);
}

cr_sinline int omcallorder(cr_State *ts, Value lhs, Value rhs, cr_om ordop)
{
	cr_assert(ts, ordop >= OM_NE && ordop <= OM_GE, "invalid cr_om for order");
	if (callbinop(ts, lhs, rhs, ordop, stkpeek(1))) { // try overload
		pop(ts); // remove second operand
		return 1;
	}
	// Instances (and cript objects) can always have equality comparison.
	// If their pointers are the same then the underlying objects are equal;
	// otherwise they are not equal.
	if (cr_unlikely(ordop != OM_EQ && ordop != OM_NE))
		ordererror(ts, lhs, rhs);
	return 0;
}

/* != */
void one(cr_State *ts, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(ts, BOOL_VAL(lhs != rhs));
	else if (!omcallorder(ts, lhs, rhs, OM_NE))
		push(ts, BOOL_VAL(lhs != rhs));
}

/* == */
void oeq(cr_State *ts, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(ts, BOOL_VAL(lhs == rhs));
	else if (!omcallorder(ts, lhs, rhs, OM_EQ))
		push(ts, BOOL_VAL(lhs == rhs));
}

/* < */
void olt(cr_State *ts, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(ts, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) < 0));
	else
		omcallorder(ts, lhs, rhs, OM_LT);
}

/* > */
void ogt(cr_State *ts, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(ts, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) > 0));
	else
		omcallorder(ts, lhs, rhs, OM_GT);
}

/* <= */
void ole(cr_State *ts, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(ts, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) <= 0));
	else
		omcallorder(ts, lhs, rhs, OM_LE);
}

/* >= */
void oge(cr_State *ts, Value lhs, Value rhs)
{
	if (isstring(lhs) && isstring(rhs))
		push(ts, BOOL_VAL(strcmp(ascstring(lhs), ascstring(rhs)) >= 0));
	else
		omcallorder(ts, lhs, rhs, OM_GE);
}


/* Bind class method in order to preserve the receiver.
 * By doing so the interpreter can then push the receiver on the
 * stack before running the function.
 * This is needed because class methods expect the receiver to be
 * the first argument ('self' automatic var). */
cr_ubyte bindmethod(cr_State *ts, OClass *oclass, Value name, Value receiver)
{
	Value method;
	if (!tableget(ts, &oclass->methods, name, &method))
		return 0;
	*stkpeek(0) = OBJ_VAL(OBoundMethod_new(ts, receiver, asclosure(method)));
	return 1;
}


/* Adjust return values after native call finishes. */
static cr_inline void moveresults(cr_State *ts, Value *fn, int32_t got, int32_t expect)
{
	Value *retstart = ts->sp - got; // start of return values
	if (expect == 0)
		expect = got; // all results (MULRET)
	if (got > expect)
		got = expect; // remove extra results
	memcpy(fn, retstart, got); // Safety: 'retstart' >= 'nativefn'
	for (int32_t i = got; i < expect; i++) // replace missing values with nil
		fn[i] = NIL_VAL;
	ts->sp = fn + expect;
}


/* Call native function. */
static cr_inline int32_t callnative(cr_State *ts, Value fn)
{
	cr_unlock(ts);
	int32_t n = ascfn(fn)->fn(ts);
	cr_lock(ts);
	criptapi_checkelems(ts, n);
	CallFrame *f = &last_frame(ts);
	moveresults(ts, f->callee, n, f->retcnt);
	ts->fc--; // pop frame
	return n;
}



/* Calls have lots of checks and there are two main reasons why.
 * cript does not allow extra arguments if the function does not
 * accept variable number of arguments and you are not allowed to
 * provide less arguments than the function arity.
 * In case 'callee' is a cript closure then just return the new 'CallFrame',
 * otherwise run the C closure and return NULL. */
static CallFrame *precall(cr_State *ts, Value callee, int32_t argc, int32_t retcnt)
{
	FnInfo *p = NULL;
	CallFrame *frame = &ts->frames[ts->fc];
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
	uint32_t bitmask = callbitmask(ts, p->isvararg, p->arity, argc, retcnt);
	cr_ubyte idx = cr_ctz(bitmask);
	goto *jmptable[idx];
l_stack_overflow:
	retovferror(ts, p->name->storage);
l_invalid_argc:
	arityerror(ts, p->arity, argc);
l_callstack_overflow:
	fcovferror(ts);
#else
	if (cr_unlikely(!cr_ensurestack(ts, retcnt))) {
		retovferror(ts, p->name->storage);
	} else if (cr_unlikely((p->isvararg && p->arity > argc) || (!p->isvararg && p->arity != argc))) {
		arityerror(ts, p->arity, argc);
	} else if (cr_unlikely(ts->fc == ts_CALLSTACK_LIMIT)) {
		fcovferror(ts);
	} else
		goto ok;
#endif
l_ok:
	frame->vacnt = argc - p->arity;
	frame->retcnt = retcnt;
	frame->callee = ts->sp - argc - 1;
	ts->fc++;
	if (frame->cfinfo & CFI_CCALL) {
		callnative(ts, callee);
		return NULL;
	} else
		return &last_frame(ts);
}

/* Call '()' a value (closure, method, class). */
static CallFrame *call(cr_State *ts, Value callee, int32_t argc, int32_t retcnt)
{
	if (cr_unlikely(!IS_OBJ(callee)))
		callerror(ts, callee);
	switch (OBJ_TYPE(callee)) {
	case OBJ_BOUND_METHOD: {
		InstanceMethod *bound = asboundmethod(callee);
		ts->sp[-argc - 1] = bound->receiver; // class instance (self)
		return precall(ts, OBJ_VAL(bound->method), argc, retcnt);
	}
	case OBJ_CLASS: {
		OClass *oclass = asclass(callee);
		Value instance = OBJ_VAL(Instance_new(ts, oclass));
		if (!calloverload(ts, instance, OM_INIT)) { // not overloaded ?
			*stkpeek(argc) = instance; // 'self'
			int32_t arity = ominfo[OM_INIT].arity; // default arity
			if (cr_unlikely(argc != arity))
				arityerror(ts, arity, argc);
		}
		return NULL;
	}
	case OBJ_CLOSURE:
	case OBJ_FUNCTION:
	case OBJ_CFUNCTION:
		return precall(ts, callee, argc, retcnt);
	default:
		cr_unreachable;
	}
}


/* call value (unprotected). */
void cr_ts_ncall(cr_State *ts, SPtr callee, int nreturns)
{
	int argc = ts->stacktop.p - callee - 1;
	if (call(ts, fn, argc, retcnt) != NULL)
		run(ts);
}


/* Protected call with longjmp.
 * Performs a protected call, calling the wrapper 'ProtectedFn' around
 * a cript function or native C function.
 * Returns status of the called function, this status is modified
 * by function that errors and performs the long jump or it
 * stays unchanged and the wrapper function just returns and
 * execution continues. */
static cr_inline int32_t protectedcall(cr_State *ts, ProtectedFn fn, void *userdata)
{
	int32_t oldfc = ts->fc;
	struct cr_longjmp lj;
	lj.status = S_OK;
	lj.prev = ts->errjmp;
	ts->errjmp = &lj;
	if (setjmp(lj.buf) == 0) // setter ?
		(*fn)(ts, userdata); // perform the call
	ts->errjmp = lj.prev;
	ts->fc = oldfc;
	return lj.status;
}


/* Public interface to 'protectedcall'.
 * In case of errors it performs a recovery by closing all
 * open upvalues (values to be closed) and restoring the
 * old stack pointer (oldtop). */
int32_t pcall(cr_State *ts, ProtectedFn fn, void *userdata, ptrdiff_t oldtop)
{
	int8_t status = protectedcall(ts, fn, userdata);
	if (cr_unlikely(status != S_OK)) {
		closeupval(ts, ts->sp);
		Value *oldsp = restore_stack(ts, oldtop);
		*oldsp = ts->sp[-1];
		ts->sp = oldsp + 1;
	}
	return status;
}


/* Check if receiver and key are valid for indexing. */
static cr_inline void checkindex(cr_State *ts, Value receiver, Value key)
{
	if (cr_unlikely(!isinstance(receiver)))
		ipaerror(ts, receiver);
	else if (cr_unlikely(IS_NIL(key)))
		nilidxerror(ts);
}


/* Private to interpreter.
 * Used when creating a cript closure. */
static cr_inline OUpvalue *captureupval(cr_State *ts, Value *valp)
{
	OUpvalue **upvalpp = &ts->open_upvals;
	while (*upvalpp != NULL && (*upvalpp)->location > valp)
		upvalpp = &(*upvalpp)->next;
	if (*upvalpp != NULL && (*upvalpp)->location == valp)
		return *upvalpp;
	OUpvalue *upvalp = OUpvalue_new(ts, valp);
	upvalp->next = *upvalpp;
	*upvalpp = upvalp;
	return upvalp;
}

/* Closes all of the captured variables moving
 * them from the stack onto the heap (open_upvals array),
 * making them reachable for gc. */
void closeupval(cr_State *ts, Value *last)
{
	while (ts->openuvals != NULL && ts->openuvals->location >= last) {
		OUpvalue *upvalp = ts->open_upvals;
		upvalp->closed = *upvalp->location;
		upvalp->location = &upvalp->closed;
		ts->open_upvals = upvalp->next;
	}
}

/**
 * Searches the entire table for the matching index in order to
 * provide more descriptive runtime error.
 **/
CRString *globalname(cr_State *ts, uint32_t idx)
{
	for (uint32_t i = 0; i < ts->globids.cap; i++) {
		Entry *entry = &ts->globids.entries[i];
		if (!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
			return (OString *)asobj(entry->key);
	}
	cr_unreachable;
}



/* -------------------------------------------------------------------------
 * Interpreter loop
 * -------------------------------------------------------------------------- */


/* save program counter */
#define savepc()	((cf)->pc = pc)

/* save program counter and stack top */
#define savestate()	(savepc(), (ts)->stacktop.p = (cf)->stacktop.p)

/* protect code that can raise errors or change the stack */
#define protect(e)	(savestate(), (e))


/* fetch an instruction */
#define fetch()		(pc += INSTRSIZE, pc[-INSTRSIZE])

/* fetch short instruction parameter */
#define fetchshort() \
	(pc += SPARAMSIZE, GETSPARAMV(&pc[-SPARAMSIZE-INSTRSIZE], 0))

/* fetch long instruction parameter */
#define fetchlong() \
	(pc += LPARAMSIZE, GETLPARAMV(&pc[-LPARAMSIZE-INSTRSIZE], 0))


/* get constant */
#define fetchconstant()		(cffn(cf)->constants.ptr[fetchlong(pc)])

/* get string constant */
#define fetchstring()		(strvalue(fetchconstant()))


#define DISPATCH(x)	switch(x)
#define CASE(l)		case l:
#define BREAK		break


void run(cr_State *ts)
{
	register CallFrame *cf;
	register const Instruction *pc;
	int codeparam1;
	int codeparam2;
	int codeparam3;

	cf = ts->aframe;
	pc = frame->pc;
	for (;;) {
#ifdef PRECOMPUTED_GOTO
#include "crjmptable.h"
#endif
		DISPATCH(fetch(pc)) {
			CASE(OP_TRUE) {
				setbtvalue(s2v(ts->stacktop.p++));
				BREAK;
			}
			CASE(OP_FALSE) {
				setbfvalue(s2v(ts->stacktop.p++));
				BREAK;
			}
			CASE(OP_NIL) {
				setnilvalue(s2v(ts->stacktop.p++));
				BREAK;
			}
			CASE(OP_NILN) {
				codeparam1 = longparam();
				while (codeparam1--)
					setnilvalue(s2v(ts->stacktop.p++));
				BREAK;
			} CASE(OP_ADD) {
				BINARY_OP(ts, AR_ADD);
				BREAK;
			}
			CASE(OP_SUB) {
				BINARY_OP(ts, AR_SUB);
				BREAK;
			}
			CASE(OP_MUL) {
				BINARY_OP(ts, AR_MUL);
				BREAK;
			}
			CASE(OP_MOD) {
				BINARY_OP(ts, AR_MOD);
				BREAK;
			}
			CASE(OP_POW) {
				BINARY_OP(ts, AR_POW);
				BREAK;
			}
			CASE(OP_DIV) {
				BINARY_OP(ts, AR_DIV);
				BREAK;
			}
			CASE(OP_NEG) {
				UNARY_OP(ts, AR_UMIN);
				BREAK;
			}
			CASE(OP_NOT) {
				UNARY_OP(ts, AR_NOT);
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
					push(ts, *next);
				}
				BREAK;
			}
			CASE(OP_NOT_EQUAL)
			{
				ORDER_OP(ts, vne);
				BREAK;
			}
			CASE(OP_EQUAL)
			{
				ORDER_OP(ts, veq);
				BREAK;
			}
			CASE(OP_EQ) // same as OP_EQUAL except we don't pop the first operand
			{
				ORDER_OP(ts, eq_preserveL);
				BREAK;
			}
			CASE(OP_GREATER)
			{
				ORDER_OP(ts, vgt);
				BREAK;
			}
			CASE(OP_GREATER_EQUAL)
			{
				ORDER_OP(ts, vge);
				BREAK;
			}
			CASE(OP_LESS)
			{
				ORDER_OP(ts, vle);
				BREAK;
			}
			CASE(OP_LESS_EQUAL)
			{
				ORDER_OP(ts, vle);
				BREAK;
			}
			CASE(OP_POP)
			{
				pop(ts);
				BREAK;
			}
			CASE(OP_POPN)
			{
				popn(ts, READ_BYTEL());
				BREAK;
			}
			CASE(OP_CONST)
			{
				savepc();
				push(ts, READ_CONSTANT());
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
					argc = cast(int32_t, ts->sp - Array_VRef_pop(&ts->callstart));
l_call:;
					{
						int32_t retcnt = READ_BYTEL();
						Value callee = *stkpeek(argc);
						savepc();
						call(ts, callee, argc, retcnt);
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
				rawset(ts, &oclass->methods, methodname, method);
				pop(ts); // pop method
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
					argc = cast(int32_t, ts->sp - Array_VRef_pop(&ts->callstart));
l_invoke:;
					{
						Value methodname = READ_CONSTANT();
						int32_t retcnt = READ_BYTEL();
						savepc();
						invoke(ts, methodname, argc, retcnt);
						updatestate();
						BREAK;
					}
				}
			}
			CASE(OP_GET_SUPER)
			{
				cr_assert(ts, isclassobj(*stkpeek(0)), "Expect OClass.");
				Value methodname = READ_CONSTANT();
				OClass *superclass = asclass(pop(ts));
				savepc();
				if (cr_unlikely(!bindmethod(ts, superclass, methodname, *stkpeek(0))))
					udperror(ts, methodname, superclass);
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
					argc = cast(int32_t, ts->sp - Array_VRef_pop(&ts->callstart));
l_invoke_super:;
					{
						Value methodname = READ_CONSTANT();
						int32_t retcnt = READ_BYTEL();
						cr_assert(ts, isclassobj(*stkpeek(0)), "superclass must be class.");
						OClass *superclass = asclass(pop(ts));
						savepc();
						invokefrom(ts, superclass, methodname, argc, retcnt);
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
					ipaerror(ts, receiver);
				}
				Instance *instance = asinstance(receiver);
				rawset(ts, &instance->fields, property_name, property);
				popn(ts, 2); // instance + property
				BREAK;
			}
			CASE(OP_GET_PROPERTY) // 'instance.property_name'
			{
				Value property_name = READ_CONSTANT();
				Value receiver = *stkpeek(0);
				if (cr_unlikely(!isinstance(receiver))) {
					savepc();
					ipaerror(ts, receiver);
				}
				Instance *instance = asinstance(receiver);
				Value property;
				if (rawget(ts, &instance->fields, property_name, &property)) {
					*stkpeek(0) = property;
					BREAK;
				}
				savepc();
				if (cr_unlikely(!bindmethod(ts, instance->oclass, property_name, receiver)))
					udperror(ts, property_name, instance->oclass);
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
					Value *gvalue = &ts->globvars.data[bytecode_param].value;
					if (cr_unlikely(*gvalue != EMPTY_VAL)) {
						savepc();
						redefgerror(ts, globalname(ts, bytecode_param)->storage);
					}
					*gvalue = pop(ts);
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
					Value *gvalue = &ts->globvars.data[bytecode_param].value;
					if (cr_unlikely(*gvalue == EMPTY_VAL)) {
						savepc();
						udgerror(ts, globalname(ts, bytecode_param)->storage);
					}
					push(ts, *gvalue);
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
					Variable *gvar = &ts->globvars.data[bytecode_param];
					if (cr_unlikely(gvar->value == EMPTY_VAL)) {
						savepc();
						udgerror(ts, globalname(ts, bytecode_param)->storage);
					} else if (cr_unlikely(VISCONST(gvar))) {
						savepc();
						fixederror(ts, globalname(ts, bytecode_param)->storage);
					}
					gvar->value = pop(ts);
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
					push(ts, frame->callee[bytecode_param]);
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
					frame->callee[bytecode_param] = pop(ts);
					BREAK;
				}
			}
			CASE(OP_JMP_IF_FALSE) // unused, using 'optimized' versions with pop
			{
				cr_unreachable;
				uint32_t skip_offset = READ_BYTEL();
				ip += ISFALSE(*stkpeek(0)) * skip_offset;
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP_IF_FALSE_POP)
			{
				UNARY_OP(ts, AR_NOT);
				uint32_t skip_offset = READ_BYTEL();
				ip += ISFALSE(*stkpeek(0)) * skip_offset;
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				pop(ts);
				BREAK;
			}
			CASE(OP_JMP_IF_FALSE_OR_POP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += (ISFALSE(*stkpeek(0)) ? skip_offset : (pop(ts), 0));
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP_IF_FALSE_AND_POP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += (ISFALSE(*stkpeek(0)) ? (pop(ts), skip_offset) : 0);
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += skip_offset;
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_JMP_AND_POP)
			{
				uint32_t skip_offset = READ_BYTEL();
				ip += skip_offset;
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				pop(ts);
				BREAK;
			}
			CASE(OP_LOOP)
			{
				uint32_t offset = READ_BYTEL();
				ip -= offset;
				cr_assert(ts, ipinbounds(), "Invalid jump.");
				BREAK;
			}
			CASE(OP_CLOSURE)
			{
				Function *fn = asfn(READ_CONSTANT());
				CriptClosure *closure = OClosure_new(ts, fn);
				push(ts, OBJ_VAL(closure));
				for (uint32_t i = 0; i < closure->fn->p.upvalc; i++) {
					cr_ubyte local = READ_BYTE();
					uint32_t idx = READ_BYTEL();
					if (local)
						closure->upvalue[i] = captureupval(ts, frame->callee + idx);
					else
						closure->upvalue[i] = frame->closure->upvalue[idx];
				}
				BREAK;
			}
			CASE(OP_GET_UPVALUE)
			{
				uint32_t idx = READ_BYTEL();
				push(ts, *frame->closure->upvalue[idx]->location);
				BREAK;
			}
			CASE(OP_SET_UPVALUE)
			{
				uint32_t idx = READ_BYTEL();
				*frame->closure->upvalue[idx]->location = pop(ts);
				BREAK;
			}
			CASE(OP_CLOSE_UPVAL)
			{
				closeupval(ts, ts->sp - 1);
				pop(ts);
				BREAK;
			}
			CASE(OP_CLOSE_UPVALN)
			{
				uint32_t last = READ_BYTEL();
				closeupval(ts, ts->sp - last);
				popn(ts, last);
				BREAK;
			}
			CASE(OP_CLASS)
			{
				push(ts, OBJ_VAL(OClass_new(ts, READ_STRING())));
				BREAK;
			}
			CASE(OP_INDEX) // 'instance[key]'
			{
				Value receiver = *stkpeek(1);
				Value key = *stkpeek(0);
				savepc();
				checkindex(ts, receiver, key);
				if (!calloverload(ts, receiver, OM_GETIDX)) { // not overloaded ?
					Instance *instance = asinstance(receiver);
					Value property;
					if (tableget(ts, &instance->fields, key, &property)) {
						*stkpeek(1) = property; // replace receiver with field value
						pop(ts); // pop key
						BREAK;
					} // else try get method
					if (cr_unlikely(!bindmethod(ts, instance->oclass, key, receiver)))
						udperror(ts, key, instance->oclass);
					*stkpeek(1) = pop(ts); // replace receiver with popped method
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
				checkindex(ts, receiver, key);
				Instance *instance = asinstance(receiver);
				if (!calloverload(ts, receiver, OM_SETIDX)) { // not overloaded ?
					tableset(ts, &instance->fields, key, value);
					popn(ts, 3); // pop instance, key and value
				} else
					updatestate();
				BREAK;
			}
			CASE(OP_OVERLOAD) // 'fn __omethod__ { ... }'
			{
				cr_om tag = READ_BYTE();
				OClass *oclass = asclass(*stkpeek(1));
				oclass->omethods[tag] = asobj(pop(ts));
				cr_assert(ts, *ip == OP_METHOD, "Expected 'OP_METHOD'.");
				BREAK;
			}
			CASE(OP_INHERIT) // 'class A impl B { ... }'
			{
				cr_assert(ts, isclassobj(*stkpeek(0)), "subclass must be class.");
				OClass *subclass = asclass(*stkpeek(0));
				Value superclass = *stkpeek(1);
				if (cr_unlikely(!isclassobj(superclass))) {
					savepc();
					inheriterror(ts, superclass);
				}
				HTable_into(ts, &asclass(superclass)->mtab, &subclass->methods, 0);
				memcpy(subclass->vtable, asclass(superclass)->vtable, sizeof(subclass->vtable));
				pop(ts); // pop subclass
				BREAK;
			}
			CASE(OP_FOREACH_PREP)
			{
				int32_t vars = READ_BYTEL();
				memcpy(ts->sp, stkpeek(2), 3 * sizeof(Value));
				ts->sp += 3;
				savepc();
				Value fn = *stkpeek(2);
				call(ts, fn, 2, vars);
				updatestate();
				BREAK;
			}
			CASE(OP_FOREACH)
			{
				int32_t vars = READ_BYTEL();
				Value *cntlvar = stkpeek(vars);
				*cntlvar = *stkpeek(vars - 1);
				cr_assert(ts, *ip == OP_JMP, "Expect 'OP_JMP'.");
				if (!IS_NIL(*cntlvar))
					ip += 4;
				BREAK;
			}
			CASE(OP_CALLSTART)
			{
				Array_VRef_push(&ts->callstart, ts->sp);
				BREAK;
			}
			CASE(OP_RETSTART)
			{
				Array_VRef_push(&ts->retstart, ts->sp);
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
				cr_assert(ts, frame->retcnt == 1, "invalid retcnt");
				goto l_ret;
			}
			CASE(OP_RET) // function return
			{
				int32_t retvalcnt, unaccounted;
				retvalcnt = cast(int32_t, ts->sp - Array_VRef_pop(&ts->retstart));
				if (frame->retcnt == 0) { // multiple return values ?
					unaccounted = 0;
					frame->retcnt = retvalcnt;
				} else
					unaccounted = frame->retcnt - retvalcnt;
				if (unaccounted < 0)
					popn(ts, -unaccounted);
				else
					pushn(ts, unaccounted, NIL_VAL);
l_ret:
				savepc();
				cr_assert(ts, ts->temp.len == 0, "Temporary array not empty.");
				for (int32_t returns = frame->retcnt; returns--;)
					Array_Value_push(&ts->temp, *--ts->sp);
				closeupval(ts, frame->callee); // close any open upvalues
				ts->fc--; // pop the frame
				ts->sp = frame->callee; // adjust the stack pointer
				while (ts->temp.len > 0) // push return values
					push(ts, Array_Value_pop(&ts->temp));
				cr_assert(ts, ts->temp.len == 0, "Temporary array not empty.");
				if (last_frame(ts).cfinfo & CFI_FRESH)
					return;
				cr_assert(ts, ts->fc > 0, "Invalid cfinfo.");
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
void interpret(cr_State *ts, const char *source, const char *path)
{
	TODO("Refactor")
	Value name = OBJ_VAL(OString_new(ts, path, strlen(path)));
	CriptClosure *closure = NULL; // TODO: compile(ts, source, name, true);
	if (closure == NULL)
		printandpanic(ts);
	cr_pcall(ts, 0, 0);
}


/* Initialize the allocated cr_State */
void cr_State_init(cr_State *ts)
{
	srand(time(0));
	ts->seed = rand();
	ts->fc = 0;
	ts->objects = NULL;
	ts->openuvals = NULL;
	ts->gc.heapmin = GC_HEAP_MIN;
	ts->gc.nextgc = GC_HEAP_INIT; // 1 MiB
	ts->gc.allocated = 0;
	ts->gc.growfactor = GC_HEAP_GROW_FACTOR;
	ts->gc.stopped = 0;
	ts->sp = ts->stack;
	ts->gs = NULL;
	ts->gslen = 0;
	ts->gscap = 0;
	HTable_init(&ts->loaded); // Loaded scripts and their functions
	HTable_init(&ts->globids); // Global variable identifiers
	Array_Variable_init(&ts->globvars, ts);
	Array_Value_init(&ts->temp, ts); // Temp values storage (return values)
	Array_VRef_init(&ts->callstart, ts);
	Array_VRef_init(&ts->retstart, ts);
	Array_OSRef_init(&ts->interned, ts);
	HTable_init(&ts->weakrefs); // cr_interned strings table (Weak_refs)
	memset(ts->faststatic, 0, sizeof(ts->faststatic));
	for (cr_ubyte i = 0; i < SS_SIZE; i++)
		ts->faststatic[i] = OString_new(ts, static_strings[i].name, static_strings[i].len);
}



/*
 * Reset virtual machine call stack and close all upvalues.
 * Additionally set the error object on top of the stack
 * if the 'status' is error code.
 */
void resetts(cr_State *ts, cr_status status)
{
	Value *top = ts->stack;
	closeupval(ts, top + 1); // close all open upvalues
	ts->fc = 0; // reset call stack
	if (status != S_OK) {
		if (status == S_EMEM)
			*top = OBJ_VAL(ts->memerror);
		else
			*top = *stkpeek(0); // err obj on top
		ts->sp = top + 1;
	} else
		ts->sp = top;
}


/*
 * Frees the cr_State and nulls out its pointer.
 */
void cleanvm(cr_State **vmp)
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
