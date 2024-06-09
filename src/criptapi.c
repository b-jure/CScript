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
#include "crconf.h"
#include "crerr.h"
#include "crhashtable.h"
#include "crobject.h"
#include "cript.h"
#include "crparser.h"
#include "crreader.h"
#include "crvalue.h"
#include "crvm.h"
#include "stdarg.h"


/* Get stack value at 'idx'. */
static cr_inline TValue *i2val(const VM *vm, int idx)
{
	TValue *fn = last_frame(vm).callee;
	if (idx >= 0) {
		cr_checkapi(vm, idx < vm->sp - 1 - fn, "index too big.");
		return (fn + 1 + idx);
	} else { // idx is negative
		cr_checkapi(vm, -idx <= (vm->sp - fn), "Invalid index.");
		return (vm->sp + idx);
	}
}



/* Ensure the stack has enough space. */
CR_API cr_ubyte cr_checkstack(VM *vm, int n)
{
	cr_checkapi(vm, n >= 0, "negative 'n'.");
	return (((vm->sp - vm->stack) + n) <= VM_STACK_LIMIT);
}



/* 
 * Create and allocate VM by providing your own 'allocator'.
 * In case the NULL pointer is provided as 'allocator' and/or
 * allocation fails NULL is returned. 
 */
CR_API VM *cr_create(cr_alloc allocator, void *ud)
{
	VM *vm;

	if (allocator == NULL || cr_unlikely((vm = allocator(NULL, sizeof(VM), ud)) == NULL))
		return NULL;
	memset(&vm->hooks, 0, sizeof(Hooks));
	vm->hooks.reallocate = allocator;
	vm->hooks.userdata = ud;
	vm->gc.gc_stopped = 1; // wait until 'vm' is initialized
	VM_init(vm);
	vm->gc.gc_stopped = 0;
	return vm;
}


/* Resets 'VM' clearing its call stack and closing all
 * to be closed variables. */
CR_API void cr_resetvm(VM *vm)
{
	resetvm(vm, vm->status);
}


/* Free the VM allocation, the pointer to VM will be nulled out. */
CR_API void cr_destroy(VM **vmp)
{
	VM *vm;
	GCObject *head, *next;

	if (cr_likely(vmp != NULL)) { // non-null pointer ?
		cr_lock(*vmp);
		if (*vmp == NULL)
			return;
		vm = *vmp;
		HashTable_free(vm, &vm->loaded);
		HashTable_free(vm, &vm->globids);
		GSARRAY_FREE(vm);
		Array_Variable_free(&vm->globvars, NULL);
		Array_Value_free(&vm->temp, NULL);
		Array_VRef_free(&vm->callstart, NULL);
		Array_VRef_free(&vm->retstart, NULL);
		Array_OSRef_free(&vm->interned, NULL);
		HashTable_free(vm, &vm->weakrefs);
		for (head = vm->objects; head != NULL; head = next) {
			next = onext(head);
			ofree(vm, head);
		}
		FREE(vm, vm);
		*vmp = NULL;
	}
}


/* Set panic handler and return old one */
CR_API cr_panic cr_setpanic(VM *vm, cr_panic panicfn)
{
	cr_panic old_panic;

	cr_lock(vm);
	old_panic = vm->hooks.panic;
	vm->hooks.panic = panicfn;
	cr_unlock(vm);
	return old_panic;
}


/* Return current version. */
CR_API cr_umem cr_version(VM *vm)
{
	UNUSED(vm);
	return cast(cr_umem, CR_VERSION_NUMBER);
}


/* 
 * Apply ordering on 2 values on the stack.
 * First both values are pushed on top of the stack (idx1 then idx2)
 * and ordering is applied.
 * This functions is free to call overloaded operator methods.
 * Result in placed in place of first operand and the second operand
 * is popped off.
 * Returned value of 1 means ordering applied is true, otherwise 0 is returned.
 */
CR_API cr_ubyte cr_compare(VM *vm, int idx1, int idx2, cr_ord ord)
{
	static void (*ordfuncs[])(VM *, Value, Value) = { veq, vne, vlt, vgt, vle, vge };
	Value l, r;
	cr_ubyte res;

	cr_lock(vm);
	criptapi_checkordop(vm, ord);
	criptapi_checkstack(vm, 2);
	l = *i2val(vm, idx1);
	r = *i2val(vm, idx2);
	*vm->sp++ = l; // push left operand
	*vm->sp++ = r; // push right operand
	ordfuncs[ord](vm, l, r);
	res = !ISFALSE(*stkpeek(0));
	cr_unlock(vm);
	return res;
}


/* 
 * Perform equality ordering on values at stack index 'idx1' and 'idx2'.
 * This function will not call overload-able operator methods (__eq__).
 * Result is returned directly without storing it on the stack.
 * Returned value of 1 means values are equal, otherwise 0 is returned. 
 */
CR_API cr_ubyte cr_rawequal(VM *vm, int idx1, int idx2)
{
	Value l, r;
	cr_ubyte res;

	cr_lock(vm);
	l = *i2val(vm, idx1);
	r = *i2val(vm, idx2);
	res = raweq(l, r);
	cr_unlock(vm);
	return res;
}


/* 
 * Perform arithmetic 'op' on values on
 * top of the stack.
 * If 'op' is unary operation then the value on top
 * of the stack is considered as operand.
 * If 'op' is binary operation then the 2 values
 * on top of the stack are considered as operands.
 * This function is free to call overload-able operator methods.
 * Result is pushed on top of the stack in place of the
 * first operand and second operand is popped of. 
 */
CR_API void cr_arith(VM *vm, cr_ar op)
{
	Value *res;
	int adjust;

	cr_lock(vm);
	criptapi_checkarop(vm, op);
	adjust = 0;
	if (arisbin(op)) {
		criptapi_checkelems(vm, 2);
		adjust = 1;
	} else {
		criptapi_checkelems(vm, 1);
	}
	res = stkpeek(1);
	arith(vm, *res, *stkpeek(0), op, res);
	vm->sp -= adjust; // result is where the first operand was
	cr_unlock(vm);
}


/* Push nil on the stack */
CR_API void cr_pushnil(VM *vm)
{
	cr_lock(vm);
	criptapi_pushnil(vm);
	cr_unlock(vm);
}


/* Push number on the stack */
CR_API void cr_pushinteger(VM *vm, cr_lint number)
{
	cr_lock(vm);
	criptapi_pushinteger(vm, number);
	cr_unlock(vm);
}


/* Push number on the stack */
CR_API void cr_pushfloat(VM *vm, cr_double number)
{
	cr_lock(vm);
	criptapi_pushfloat(vm, number);
	cr_unlock(vm);
}


/* Push string on the stack */
CR_API void cr_pushstring(VM *vm, const char *str, cr_umem len)
{
	cr_lock(vm);
	criptapi_pushstr(vm, str, len);
	cr_unlock(vm);
}


/* Push cstring on the stack */
CR_API void cr_pushcstring(VM *vm, const char *str)
{
	cr_lock(vm);
	criptapi_pushstr(vm, str, strlen(str));
	cr_unlock(vm);
}


/* Push formatted cstring on the stack, format arguments
 * start from 'argp'. */
CR_API const char *cr_pushvfstring(VM *vm, const char *fmt, va_list argp)
{
	const char *str = NULL;
	cr_lock(vm);
	criptapi_pushfstr(vm, fmt, argp);
	str = ascstring(*stkpeek(0));
	cr_unlock(vm);
	return str;
}


/* Push formatted cstring on the stack */
CR_API const char *cr_pushfstring(VM *vm, const char *fmt, ...)
{
	const char *str = NULL;
	va_list argp;
	cr_lock(vm);
	va_start(argp, fmt);
	criptapi_pushfstr(vm, fmt, argp);
	va_end(argp);
	str = ascstring(*stkpeek(0));
	cr_unlock(vm);
	return str;
}


/* Push boolean on the stack */
CR_API void cr_pushbool(VM *vm, int boolean)
{
	cr_lock(vm);
	cr_checkapi(vm, boolean == 0 || boolean == 1, "invalid boolean.");
	criptapi_pushbool(vm, boolean);
	cr_unlock(vm);
}


/* Auxiliary to 'cr_pushcclosure' and 'cr_pushclass' */
static CClosure *auxpushcclosure(VM *vm, cr_cfunc fn, int args, cr_ubyte isvararg,
				int upvals)
{
	CRString *name = asstring(*stkpeek(0));
	CClosure *native = ONative_new(vm, name, fn, args, isvararg, upvals);
	pop(vm); // name
	vm->sp -= upvals;
	while (upvals--)
		native->upvalue[upvals] = *(vm->sp + upvals);
	return native;
}


/* Push C closure on to the stack.
 * The 'args' is how many arguments this function expects (minimum),
 * 'isvararg' is a boolean value indicating if this function takes in
 * variable amount of arguments, 'upvals' is the number of
 * upvalues this C closure has.
 * These upvalues are stored directly in this function and can
 * be accessed with the provided API in this header file.
 * This function will remove 'upvals' amount of values from the stack
 * and store them in C closure. */
CR_API void cr_pushcclosure(VM *vm, const char *name, cr_cfunc fn, int args, cr_ubyte isvararg,
			    int upvals)
{
	cr_lock(vm);
	criptapi_checkelems(vm, upvals);
	criptapi_checkptr(vm, fn);
	CRString *fname = vm->faststatic[SS_CSRC];
	if (name)
		fname = OString_new(vm, name, strlen(name));
	criptapi_pusho(vm, fname);
	CClosure *native = auxpushcclosure(vm, fn, args, isvararg, upvals);
	criptapi_pushonative(vm, native);
	cr_unlock(vm);
}


/* Push value from the stack located at 'idx', on top of the stack */
CR_API void cr_push(VM *vm, int idx)
{
	cr_lock(vm);
	Value *val = i2val(vm, idx);
	criptapi_pushval(vm, *val);
	cr_unlock(vm);
}



/* Push class on the stack.
 * Stack will contain 'nup' upvalues that the 'cr_cfunc' located in
 * array of 'cr_entry' will have.
 * Top of the stack shall contain the name of the class.
 * Each 'cr_entry' contains the 'name' of the C function,
 * its argument count (arity) and if the function accepts variable
 * number of arguments (isvararg).
 * This is all what the C function needs to become a C closure inside
 * of cript, with the exception of 'nup'.
 * The reason why each entry does not contain 'nup' is because
 * all of the functions in entries array share the same upvalues.
 * This is in order to simplify the implementation.
 * In case the function name is overload-able method (such as __init__),
 * then 'args' and 'isvararg' in that 'cr_entry' are ignored and
 * the appropriate values are used.
 * After the function returns upvalues will be popped together with
 * the class name; top of the stack will contain newly created class. */
CR_API void cr_pushclass(VM *vm, cr_entry entries[], int nup)
{
	cr_lock(vm);
	criptapi_checkelems(vm, nup + 1); // upvalues + class name
	Value classname = *stkpeek(0);
	cr_checkapi(vm, isstring(classname), "Expect string");
	OClass *oclass = OClass_new(vm, asstring(classname));
	pop(vm); // class name
	criptapi_pusho(vm, oclass);
	cr_entry *entry = entries;
	while (entry->name && entry->fn) { // while valid entry
		for (int i = 0; i < nup; i++)
			criptapi_pushval(vm, *stkpeek(nup - 1));
		CRString *name = OString_new(vm, entry->name, strlen(entry->name));
		criptapi_pusho(vm, name);
		int tag = id2omtag(vm, name);
		if (tag == -1) { // not overload ?
			CClosure *native =
				auxpushcclosure(vm, entry->fn, entry->args, entry->isvararg, nup);
			*stkpeek(0) = OBJ_VAL(native);
			rawset(vm, &oclass->mtab, OBJ_VAL(name), OBJ_VAL(native));
			pop(vm); // native
		} else { // overloaded, override 'arity' and 'isvararg'
			CClosure *native = auxpushcclosure(vm, entry->fn, ominfo[tag].arity, 0, nup);
			oclass->vtable[tag] = cast(GCObject *, native);
		}
		entry = ++entries;
	}
	*(vm->sp - nup) = pop(vm); // move class to first upvalue
	popn(vm, nup - 1); // pop the rest of the upvalues
	cr_unlock(vm);
}




/* 
 * Convert 'acceptable' stack index into an absolute index.
 * For example: if there are 5 values on the stack after the
 * callee then -1 would be index 4. 
 */
CR_API int cr_absidx(VM *vm, int idx)
{
	return (idx >= 0) ? idx : cast_int(vm->sp - last_frame(vm).callee - 1) + idx;
}



/* Return type of the value on the stack at 'idx'. */
CR_API cr_tt cr_type(const VM *vm, int idx)
{
	Value *value = i2val(vm, idx);
	return val2type(*value);
}



/* Return type name of the value on the stack at 'idx'.
 * This returned pointer is 'const' indicating the
 * memory it points to should not be modified. */
CR_API const char *cr_typename(const VM *vm, int idx)
{
	Value *value = i2val(vm, idx);
	int type = val2type(*value);
	return vm->faststatic[type]->bytes;
}



/* Convert type tag into name */
CR_API const char *cr_tagname(const VM *vm, cr_tt type)
{
	return vm->faststatic[type]->bytes;
}



/* Check if the value on the stack at 'idx' is nil. */
CR_API cr_ubyte cr_isnil(const VM *vm, int idx)
{
	return IS_NIL(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is number. */
CR_API cr_ubyte cr_isnumber(const VM *vm, int idx)
{
	return IS_NUMBER(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is string. */
CR_API cr_ubyte cr_isstring(const VM *vm, int idx)
{
	return isstring(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is bool. */
CR_API cr_ubyte cr_isbool(const VM *vm, int idx)
{
	return IS_BOOL(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is class. */
CR_API cr_ubyte cr_isclass(const VM *vm, int idx)
{
	return isclassobj(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is instance. */
CR_API cr_ubyte cr_isinstance(const VM *vm, int idx)
{
	return isinstance(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is native C function. */
CR_API cr_ubyte cr_isnative(const VM *vm, int idx)
{
	return iscfunction(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is bound method (instance method). */
CR_API cr_ubyte cr_ismethod(const VM *vm, int idx)
{
	return isboundmethod(*i2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is cript closure. */
CR_API cr_ubyte cr_isclosure(const VM *vm, int idx)
{
	return isclosureobj(*i2val(vm, idx));
}




/* Concatenate 2 strings on top of the stack.
 * Pops the string on top of the stack and replaces the first
 * one with the concatenated string.
 * They are concatenated in the order they were pushed on the stack. */
CR_API const char *cr_concat(VM *vm)
{
	cr_lock(vm);
	criptapi_checkelems(vm, 2);
	Value right = *stkpeek(0);
	Value left = *stkpeek(1);
	cr_checkapi(vm, isstring(right) && isstring(left), "expect strings");
	concatonstack(vm);
	const char *concated = ascstring(*stkpeek(0));
	cr_unlock(vm);
	return concated;
}


/* Push class method of an instance at idx on top of the stack.
 * If method doesn't exist this function returns 0 otherwise 1.
 * Note: Class instance methods are all cript closures. */
CR_API cr_ubyte cr_getmethod(VM *vm, int idx, const char *method)
{
	cr_lock(vm);
	criptapi_checkptr(vm, method);
	Value val = *i2val(vm, idx);
	if (!isinstance(val))
		return 0;
	criptapi_pushstr(vm, method, strlen(method));
	cr_ubyte haveit = bindmethod(vm, asinstance(val)->oclass, *stkpeek(0), val);
	cr_unlock(vm);
	return haveit;
}



/* Pushes the field value of the class instance at 'idx' on top
 * of the stack.
 * If field value was not found or the value at 'idx' is not
 * class instance return 0, otherwise 1. */
CR_API cr_ubyte cr_getfield(VM *vm, int idx, const char *field)
{
	cr_ubyte res = 0;
	cr_lock(vm);
	criptapi_checkptr(vm, field);
	Value insval = *i2val(vm, idx);
	if (isinstance(insval)) {
		Instance *instance = asinstance(insval);
		Value key = OBJ_VAL(OString_new(vm, field, strlen(field)));
		Value fieldval;
		if ((res = rawget(vm, &instance->fields, key, &fieldval)))
			criptapi_pushval(vm, fieldval);
	}
	cr_unlock(vm);
	return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__getidx__) on it using the
 * value on top of the stack as index value.
 * If the value is not an instance or the
 * '__getidx__' is not overloaded, this function
 * returns 0, otherwise 1 and the value will
 * be on top of the stack. */
CR_API cr_ubyte cr_getindex(VM *vm, int idx)
{
	cr_ubyte res = 0;
	cr_lock(vm);
	criptapi_checkelems(vm, 1); // [index]
	Value *index = stkpeek(0);
	Value value = *i2val(vm, idx);
	res = calloverload(vm, value, OM_GETIDX);
	*index = pop(vm); // replace [index] with result
	cr_unlock(vm);
	return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__setidx__) on it, index value
 * is located one place before the top of the stack
 * while the value getting assigned is on top of the stack.
 * If the value is not an instance or '__setidx__' is not
 * overloaded, this function returns 0, otherwise 1. */
CR_API cr_ubyte cr_setindex(VM *vm, int idx)
{
	cr_ubyte res = 0;
	cr_lock(vm);
	criptapi_checkelems(vm, 2); // [index][expr]
	Value value = *i2val(vm, idx);
	res = calloverload(vm, value, OM_SETIDX);
	popn(vm, 2); // pop [index] and [expr]
	cr_unlock(vm);
	return res;
}



/* Performs 'raw' index operation, meaning it doesn't invoke
 * overloaded methods when getting/setting the instance property.
 * 'what' parameter if it is a zero means we are setting and
 * non-zero 'what' means we are getting the value at that index.
 * In case we are setting the indexed value then the 'value' we are
 * assigning will be on top of the stack and the 'index' value right
 * below it; if we are getting the value then the 'key' will be on top
 * of the stack.
 * If the operation was successful then 1 is returned; otherwise 0.
 * @ERR: if value we are indexing with is 'nil'. */
CR_API cr_ubyte cr_rawindex(VM *vm, int idx, cr_ubyte what)
{
	cr_ubyte res = 0;
	cr_lock(vm);
	criptapi_checkelems(vm, what == CR_RAWSET ? 2 : 1);
	Value value = *i2val(vm, idx);
	if (!isinstance(value))
		return res;
	res = rawindex(vm, value, what);
	cr_unlock(v);
	return res;
}



/* Push global value on top of the stack.
 * In case global value was found, it will be on top of the stack,
 * and this function will return 1, otherwise nothing will be pushed
 * on the stack and the function will return 0. */
CR_API cr_ubyte cr_getglobal(VM *vm, const char *name)
{
	cr_lock(vm);
	criptapi_checkptr(vm, name);
	int res = 0;
	Value gval;
	criptapi_checkptr(vm, name);
	CRString *str = OString_new(vm, name, strlen(name));
	if (rawget(vm, &vm->globids, OBJ_VAL(str), &gval)) {
		int idx = (int)AS_NUMBER(gval);
		criptapi_pushval(vm, vm->globvars.data[idx].value);
		res = 1;
	}
	cr_unlock(vm);
	return res;
}



/* Get panic handler */
CR_API cr_panic cr_getpanic(VM *vm)
{
	cr_lock(vm);
	cr_panic panic_handler = vm->hooks.panic;
	cr_unlock(vm);
	return panic_handler;
}



/* Get allocator function */
CR_API cr_alloc cr_getalloc(VM *vm, void **ud)
{
	cr_lock(vm);
	cr_alloc alloc = vm->hooks.reallocate;
	if (ud)
		*ud = vm->hooks.userdata;
	cr_unlock(vm);
	return alloc;
}



/* Get boolean value (int 1/0) from the stack at 'idx'.
 * If the value at 'idx' is not a boolean, then the flag
 * if provided 'isbool' is set as 0, otherwise flag is set to 1. */
CR_API cr_ubyte cr_getbool(const VM *vm, int idx, cr_ubyte *isbool)
{
	cr_ubyte bval;
	Value val = *i2val(vm, idx);
	cr_ubyte is = tobool(val, &bval);
	if (isbool)
		*isbool = is;
	return bval;
}



/* Get number value (cr_double) from the stack at 'idx'.
 * If the value at 'idx' is not a number, then the flag
 * if provided 'isnum' is set as 0, otherwise flag is set to 1. */
CR_API cr_double cr_getnumber(const VM *vm, int idx, cr_ubyte *isnum)
{
	cr_double nval = 0.0;
	Value val = *i2val(vm, idx);
	cr_ubyte is = tonumber(val, &nval);
	if (isnum)
		*isnum = is;
	return nval;
}



/* Get string value from the stack at 'idx'.
 * Returns NULL (0) if the value is not a string.
 * Otherwise it returns pointer to the start of the string.
 * Returned pointer is 'const' indicating that user should not
 * modify the contents the pointer points to. */
CR_API const char *cr_getstring(const VM *vm, int idx)
{
	Value val = *i2val(vm, idx);
	return isstring(val) ? ascstring(val) : NULL;
}



/* Get native C function from the stack at 'idx'.
 * Return NULL if the value is not a 'cr_cfunc'. */
CR_API cr_cfunc cr_getcfunction(const VM *vm, int idx)
{
	Value val = *i2val(vm, idx);
	return iscfunction(val) ? ascfn(val)->fn : NULL;
}



/* Return the number of values currently on the stack
 * relative to the current function */
CR_API int cr_gettop(const VM *vm)
{
	return cast_int(vm->sp - (last_frame(vm).callee + 1));
}



/* Copy value on the stack located at index 'src'
 * to value on the stack located at index 'dest'. */
CR_API void cr_copy(VM *vm, int src, int dest)
{
	cr_lock(vm);
	Value *from = i2val(vm, src);
	Value *to = i2val(vm, dest);
	*to = *from;
	cr_unlock(vm);
}



/* Auxiliary to 'cr_rotate', reverses values from 'from' until 'to'. */
static cr_inline void reverse(Value *from, Value *to)
{
	for (; from < to; from++, to--) {
		*from ^= *to;
		*to ^= *from;
		*from ^= *to;
	}
}



/*
 * This is basically a stack-array rotation between the
 * top of the stack and the index 'idx' for 'n' elements.
 * Negative '-n' indicates left-rotation, while positive
 * 'n' right-rotation.
 * The absolute value of 'n' must not be greater
 * than the array slice we are rotating.
 *
 * Example right-rotation:
 * - Before rotation:
 * [callee][0][1][2][3][4][sp]
 * - Do the rotation:
 * cr_rotate(vm, 2, 2);
 * - After right-rotation:
 * [callee][0][1][3][4][2][sp]
 *
 *
 * Example left-rotation:
 * - Before rotation:
 * [callee][0][1][2][3][4][sp]
 * - Do the rotation:
 * cr_rotate(vm, 2, -2);
 * -After left-rotation:
 * [callee][0][1][4][3][2][sp]
 */
CR_API void cr_rotate(VM *vm, int idx, int n)
{
	cr_lock(vm);
	Value *end = stkpeek(0);
	Value *start = i2val(vm, idx);
	cr_checkapi(vm, (n >= 0 ? n : -n) <= end - start + 1, "invalid 'n'");
	Value *pivot = (n >= 0 ? end - n : end - n - 1);
	reverse(pivot, start);
	reverse(pivot + 1, end);
	reverse(start, end);
	cr_unlock(vm);
}




/* Call the value on the stack with 'argc' arguments. */
CR_API void cr_call(VM *vm, int argc, int retcnt)
{
	cr_lock(vm);
	cr_checkapi(vm, retcnt >= CR_MULRET, "invalid return count");
	criptapi_checkelems(vm, argc + 1);
	criptapi_checkresults(vm, argc, retcnt);
	Value *fn = vm->sp - (argc + 1);
	ncall(vm, fn, *fn, retcnt);
	cr_unlock(vm);
}



/* Data used for 'fcall' */
struct CallData {
	Value *callee;
	int retcnt;
};


/* Wrapper function */
static void fcall(VM *vm, void *userdata)
{
	struct CallData *cd = cast(struct CallData *, userdata);
	ncall(vm, cd->callee, *cd->callee, cd->retcnt);
}


/* Protected call.
 * Same as cr_call except this runs the function in protected
 * mode, meaning that in case the function errors it won't print
 * invoke panic handler.
 * Instead it restores the old call frame and pushes the error object
 * on top of the stack.
 * This function returns 'cr_status' [defined @cript.h] code. */
CR_API cr_status cr_pcall(VM *vm, int argc, int retcnt)
{
	cr_lock(vm);
	cr_checkapi(vm, retcnt >= CR_MULRET, "invalid return count");
	criptapi_checkelems(vm, argc + 1);
	criptapi_checkresults(vm, argc, retcnt);
	struct CallData cd;
	cd.retcnt = retcnt;
	cd.callee = vm->sp - (argc + 1);
	int status = pcall(vm, fcall, &cd, save_stack(vm, cd.callee));
	cr_unlock(vm);
	return status;
}



/*
 * Loads (compiles) cript script using provided 'reader'.
 * Returns 'cr_status' [defined @cript.h] code.
 * If the script compiled without any errors then the compiled
 * function (cript closure) gets pushed on top of the stack.
 * In case there were any compile errors, then the error object
 * gets pushed on top of the stack (error message).
 *
 * 'reader' - user provided 'cr_reader' responsible for reading
 *            the '.sk' source file.
 *            Refer to 'cr_reader' in [@cript.h] for more
 *            information on how this reader should 'behave'.
 * 'userdata' - user provided data for 'reader'.
 * 'source' - name of the cript script you are loading.
 */
CR_API cr_status cr_load(VM *vm, cr_reader reader, void *userdata, const char *source)
{
	BuffReader br;
	cr_lock(vm);
	BuffReader_init(vm, &br, reader, userdata);
	cr_status status = pcompile(vm, &br, source, 0);
	cr_unlock(vm);
	return status;
}



/* Garbage collection API.
 * Refer to the @cript.h and 'cr_gco' enum defined in the same header. */
CR_API cr_umem cr_incgc(VM *vm, cr_incgco option, ...)
{
	va_list argp;
	cr_umem res = 0;
	cr_lock(vm);
	va_start(argp, option);
	switch (option) {
	case GCO_STOP:
		res = vm->gc.gc_stopped;
		vm->gc.gc_stopped = 1;
		break;
	case GCO_RESTART:
		res = vm->gc.gc_stopped;
		vm->gc.gc_stopped = 0;
		break;
	case GCO_COLLECT:
		res = incgc(vm);
		break;
	case GCO_COUNT:
		res = vm->gc.gc_allocated;
		break;
	case GCO_ISRUNNING:
		res = (vm->gc.gc_stopped == 0);
		break;
	case GCO_NEXTGC:
		res = vm->gc.gc_nextgc;
		break;
	}
	va_end(argp);
	cr_unlock(vm);
	return res;
}




// TODO: Implement
CR_API void cr_dumpstack(VM *vm)
{
	(void)(0);
}



/* Converts value on the stack at the 'idx' into string.
 * Additionally if the 'len' and/or 'hash' are non-NULL then
 * it also fills them.
 * This can call overload-able method '__tostring__'. */
CR_API const char *cr_tostring(VM *vm, int idx, cr_umem *len, cr_hash *hash)
{
	const char *str = NULL;
	cr_lock(vm);
	Value *v = i2val(vm, idx);
	CRString *ostr = vtostr(vm, v, *v, 0);
	str = ostr->bytes;
	if (len)
		*len = ostr->len;
	if (hash)
		*hash = ostr->hash;
	cr_unlock(vm);
	return str;
}



/* Sets the new stack top relative to the current function */
CR_API void cr_settop(VM *vm, int idx)
{
	cr_lock(vm);
	Value *newtop;
	Value *fn = vm->frames[vm->fc - 1].callee;
	ptrdiff_t diff;
	if (idx >= 0) {
		cr_checkapi(vm, idx < ((Value *)stklast(vm) - fn), "index too big.");
		diff = ((fn + 1) + idx) - vm->sp;
		for (; diff > 0; diff--)
			*vm->sp++ = NIL_VAL;
	} else { // index negative
		cr_checkapi(vm, -idx <= (vm->sp - fn), "invalid index.");
		diff = idx + 1;
	}
	vm->sp += diff; // set new top
	if (diff < 0)
		closeupval(vm, vm->sp);
	cr_unlock(vm);
}



/* Set global value 'name' to the value on top of the stack.
 * In case the global variable 'name' does not exist, then
 * the new one is declared and 'isconst' modifier is considered
 * when creating it.
 * Otherwise 'isconst' modifier is ignored and the global
 * variable is set to the new value UNLESS the variable is
 * set as 'fixed'; in that case runtime error is invoked. */
CR_API cr_ubyte cr_setglobal(VM *vm, const char *name, int isconst)
{
	cr_lock(vm);
	criptapi_checkelems(vm, 1); // value must be present
	criptapi_checkptr(vm, name);
	Value newval = *stkpeek(0);
	Value key = OBJ_VAL(OString_new(vm, name, strlen(name)));
	cr_ubyte isnew = 0;
	Value gidx;
	if ((isnew = !rawget(vm, &vm->globids, key, &gidx))) {
		criptapi_pushval(vm, key);
		Variable gvar = { newval, 0x01 & isconst };
		Value idx = NUMBER_VAL(Array_Variable_push(&vm->globvars, gvar));
		rawset(vm, &vm->globids, key, idx);
		popn(vm, 2); // value and key
	} else {
		Variable *gvar = Array_Variable_index(&vm->globvars, AS_NUMBER(gidx));
		if (cr_unlikely(VISCONST(gvar))) {
			fixederror(vm, globalname(vm, AS_NUMBER(gidx))->bytes);
		}
		gvar->value = newval;
		pop(vm); // value
	}
	cr_unlock(vm);
	return isnew;
}



/* Set the field of the class instance to the value on top of the stack.
 * Class should be located at 'idx' and the name of the field to be set is 'field'.
 * This sets the field to that value and pops it off the top of the stack.
 * Returns 1 if the field didn't exist before or false if the value
 * of the field got overwritten. */
CR_API cr_ubyte cr_setfield(VM *vm, int idx, const char *field)
{
	cr_lock(vm);
	criptapi_checkelems(vm, 1);
	criptapi_checkptr(vm, field);
	Value insval = *i2val(vm, idx);
	cr_checkapi(vm, isinstance(insval), "expect class instance");
	Instance *instance = asinstance(insval);
	criptapi_pushcstr(vm, field);
	cr_ubyte res = rawset(vm, &instance->fields, *stkpeek(0), *stkpeek(1));
	vm->sp -= 2; // pop value and key
	cr_unlock(vm);
	return res;
}



/* Auxiliary function for 'cr_getupval' and 'cr_setupval'.
 * Returns pointer to the upvalue. */
static cr_inline Value *getupval(Value fn, int n)
{
	if (isclosureobj(fn)) { // cript closure ?
		CriptClosure *closure = asclosure(fn);
		if (cast_uint(n) > closure->fn->p.upvalc - 1)
			return NULL;
		return closure->upvalue[n]->location;
	} else if (iscfunction(fn)) { // native C function ?
		CClosure *native = ascfn(fn);
		if (cast_uint(n) > native->p.upvalc - 1)
			return NULL;
		return &native->upvalue[n];
	} else
		return NULL;
}



/* Get upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pushes the upvalue on top of the stack and returns 1.
 * If the upvalue was not found or the function at 'fidx' is not
 * a cript or C closure then nothing will be pushed on the stack and
 * 0 is returned. */
CR_API cr_ubyte cr_getupvalue(VM *vm, int fidx, int idx)
{
	cr_lock(vm);
	cr_ubyte ret = 0;
	Value fn = *i2val(vm, fidx);
	Value *upval = getupval(fn, idx);
	if (upval) {
		criptapi_pushval(vm, *upval);
		ret = 1;
	}
	cr_unlock(vm);
	return ret;
}



/* Sets the upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pops the top value on the stack and sets
 * it as the new value of the upvalue.
 * If the upvalue doesn't exist and/or 'fidx' is not a
 * closure or native C function, then the function returns 0
 * indicating the upvalue was not set, otherwise it returns 1. */
CR_API int cr_setupvalue(VM *vm, int fidx, int idx)
{
	cr_lock(vm);
	criptapi_checkelems(vm, 1);
	int changed = 0;
	Value fn = *i2val(vm, fidx);
	Value *upval = getupval(fn, idx);
	if (upval) {
		*upval = *stkpeek(0);
		vm->sp--;
		changed = 1;
	}
	cr_unlock(vm);
	return changed;
}


static Instance *getinstance(VM *vm, int idx)
{
	TValue *v;
	v = i2val(vm, idx);
	checkapi(vm, ttisins(v), "expect instance");
	return insvalue(v);
}


/* 
 * Get the next property of the instance located at 'idx' on the stack.
 * The 'key' value used for lookup is on top of the stack.
 * 'what' if set to 0 fetches next field, otherwise it
 * gets method.
 * This function returns 1 if there is next property and
 * the value on top of the stack (key) is replaced with the
 * next key; additionally value associated with that key is
 * also pushed on top of the stack.
 * If there is no next property 0 is returned and stack
 * remains unchanged. 
 * In case user provided 'key' that the instance method/field
 * table does not contain then runtime error is invoked.
 */
CR_API cr_ubyte cr_nextproperty(VM *vm, int idx, cr_ubyte what)
{
	cr_ubyte hasnext;
	Instance *instance;
	HashTable *tab;
	TValue *key;

	cr_lock(vm);
	hasnext = 0;
	checkapi_values(vm, 2); /* key + instance */
	checkapi_stack(vm, 1); /* value */
	instance = getinstance(vm, idx);
	key = speek(0);
	if (what == 0)
		tab = &instance->fields;
	else
		tab = &instance->oclass->mtab;
	hasnext = cr_ht_next(vm, tab, key);
	if (hasnext)
		api_incsp(vm); /* push value */
	else
		api_decsp(vm); /* pop key */
	cr_unlock(vm);
	return hasnext;
}



/* Return the length of the string at 'idx'.
 * If the value is not a string then return 0. */
CR_API cr_umem cr_strlen(const VM *vm, int idx)
{
	Value val = *i2val(vm, idx);
	return (isstring(val) ? asstring(val)->len : 0);
}


/* Return 'VM' 'cr_status' code. */
CR_API cr_status cr_getstatus(VM *vm)
{
	UNUSED(vm);
	return vm->status;
}


/* Invoke a runetime error with errcode */
CR_API int cr_error(VM *vm, cr_status errcode)
{
	cr_lock(vm);
	Value *errobj = stkpeek(0);
	criptapi_checkelems(vm, 1);
	criptapi_checkerrcode(vm, errcode);
	runerror(vm, errcode); // cr_unlock in here
	return 0; // to avoid compiler warnings
}
