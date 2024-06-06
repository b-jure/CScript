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

#include "criptapi.h"
#include "crchunk.h"
#include "crcommon.h"
#include "crdebug.h"
#include "crerr.h"
#include "crlimits.h"
#include "crmem.h"
#include "crobject.h"
#include "crparser.h"
#include "crvalue.h"
#include "crvm.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>



volatile cr_ubyte runtime = 0; // VM is running?



/* Push the value on the stack */
void push(VM *vm, Value val)
{
	if (cr_likely(cast_int(vm->sp - vm->stack) < VM_STACK_LIMIT))
		*vm->sp++ = val;
	else
		sovferror(vm);
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
		Value instance = OBJ_VAL(OInstance_new(vm, oclass));
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

/* Public interface for normal call (unprotected). */
void ncall(VM *vm, Value *retstart, Value fn, int32_t retcnt)
{
	int32_t argc = vm->sp - retstart - 1;
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


void run(VM *vm)
{
#define saveip()	(frame->ip = ip)
#define updatestate()	(frame = &last_frame(vm), ip = frame->ip)
#define throwerr(vm)	runerror(vm, vtostr(vm, *stkpeek(0))->storage)
#define READ_BYTE()	(*ip++)
#define READ_BYTEL()	(ip += 3, GET_BYTES3(ip - 3))
#define READ_CONSTANT() FFN(frame)->chunk.constants.data[READ_BYTEL()]
#define READ_STRING()	asstring(READ_CONSTANT())
#define bcstart()	(FFN(frame).chunk.code.data)
#define ipinbounds()	(ip - bcstart() < VM_STACK_LIMIT && ip >= bcstart())
#define BINARY_OP(vm, op)                \
	do {                             \
		saveip();                \
		Value *l = stkpeek(1);   \
		Value r = *stkpeek(0);   \
		arith(vm, *l, r, op, l); \
	} while (0)
#define UNARY_OP(vm, op)                       \
	do {                                   \
		saveip();                      \
		Value *l = stkpeek(0);         \
		arith(vm, *l, NIL_VAL, op, l); \
	} while (0)
#define ORDER_OP(vm, fnop)             \
	do {                           \
		saveip();              \
		Value l = *stkpeek(1); \
		Value r = *stkpeek(0); \
		fnop(vm, l, r);        \
	} while (0)


	last_frame(vm).cfinfo = CFI_FRESH; // mark as fresh execute of cript script
	runtime = 1;
	register CallFrame *frame = &vm->frames[vm->fc - 1];
	register cr_ubyte *ip = frame->ip;
#ifdef DEBUG_TRACE_EXECUTION
	printf("\n=== VM - execution ===\n");
#endif
	for (;;) {
#ifdef CR_PRECOMPUTED_GOTO
#define OP_TABLE
#include "skjmptable.h"
#undef OP_TABLE
#ifdef DEBUG_TRACE_EXECUTION
#undef BREAK
#define BREAK                     \
	dumpstack(vm, frame, ip); \
	DISPATCH(READ_BYTE())
#endif
#else
#define DISPATCH(x) switch (x)
#define CASE(label) case label:
#ifdef DEBUG_TRACE_EXECUTION
#define BREAK                     \
	dumpstack(vm, frame, ip); \
	break
#else
#define BREAK break
#endif
#endif
		DISPATCH(READ_BYTE())
		{
			CASE(OP_TRUE)
			{
				saveip();
				push(vm, BOOL_VAL(1));
				BREAK;
			}
			CASE(OP_FALSE)
			{
				saveip();
				push(vm, BOOL_VAL(0));
				BREAK;
			}
			CASE(OP_NIL)
			{
				saveip();
				push(vm, NIL_VAL);
				BREAK;
			}
			CASE(OP_NILN)
			{
				saveip();
				pushn(vm, READ_BYTEL(), NIL_VAL);
				BREAK;
			}
			CASE(OP_ADD)
			{
				BINARY_OP(vm, AR_ADD);
				BREAK;
			}
			CASE(OP_SUB)
			{
				BINARY_OP(vm, AR_SUB);
				BREAK;
			}
			CASE(OP_MUL)
			{
				BINARY_OP(vm, AR_MUL);
				BREAK;
			}
			CASE(OP_MOD)
			{
				BINARY_OP(vm, AR_MOD);
				BREAK;
			}
			CASE(OP_POW)
			{
				BINARY_OP(vm, AR_POW);
				BREAK;
			}
			CASE(OP_DIV)
			{
				BINARY_OP(vm, AR_DIV);
				BREAK;
			}
			CASE(OP_NEG)
			{
				UNARY_OP(vm, AR_UMIN);
				BREAK;
			}
			CASE(OP_NOT)
			{
				UNARY_OP(vm, AR_NOT);
				BREAK;
			}
			CASE(OP_VARARG)
			{
				saveip();
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
				saveip();
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
						saveip();
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
						saveip();
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
				saveip();
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
						saveip();
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
					saveip();
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
					saveip();
					ipaerror(vm, receiver);
				}
				Instance *instance = asinstance(receiver);
				Value property;
				if (rawget(vm, &instance->fields, property_name, &property)) {
					*stkpeek(0) = property;
					BREAK;
				}
				saveip();
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
						saveip();
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
						saveip();
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
						saveip();
						udgerror(vm, globalname(vm, bytecode_param)->storage);
					} else if (cr_unlikely(VISCONST(gvar))) {
						saveip();
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
				saveip();
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
				saveip();
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
					saveip();
					inheriterror(vm, superclass);
				}
				HashTable_into(vm, &asclass(superclass)->mtab, &subclass->mtab, 0);
				memcpy(subclass->vtable, asclass(superclass)->vtable, sizeof(subclass->vtable));
				pop(vm); // pop subclass
				BREAK;
			}
			CASE(OP_FOREACH_PREP)
			{
				int32_t vars = READ_BYTEL();
				memcpy(vm->sp, stkpeek(2), 3 * sizeof(Value));
				vm->sp += 3;
				saveip();
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
				saveip();
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
	HashTable_init(&vm->loaded); // Loaded scripts and their functions
	HashTable_init(&vm->globids); // Global variable identifiers
	Array_Variable_init(&vm->globvars, vm);
	Array_Value_init(&vm->temp, vm); // Temp values storage (return values)
	Array_VRef_init(&vm->callstart, vm);
	Array_VRef_init(&vm->retstart, vm);
	Array_OSRef_init(&vm->interned, vm);
	HashTable_init(&vm->weakrefs); // cr_interned strings table (Weak_refs)
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
