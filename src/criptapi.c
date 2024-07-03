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
#include "crts.h"
#include "stdarg.h"


/* Get stack value at 'idx'. */
static cr_inline TValue *i2val(const cr_State *ts, int idx)
{
	TValue *fn = last_frame(ts).callee;
	if (idx >= 0) {
		cr_checkapi(ts, idx < ts->sp - 1 - fn, "index too big.");
		return (fn + 1 + idx);
	} else { // idx is negative
		cr_checkapi(ts, -idx <= (ts->sp - fn), "Invalid index.");
		return (ts->sp + idx);
	}
}



/* Ensure the stack has enough space. */
CR_API cr_ubyte cr_checkstack(cr_State *ts, int n)
{
	cr_checkapi(ts, n >= 0, "negative 'n'.");
	return (((ts->sp - ts->stack) + n) <= ts_STACK_LIMIT);
}



/* 
 * Create and allocate cr_State by providing your own 'allocator'.
 * In case the NULL pointer is provided as 'allocator' and/or
 * allocation fails NULL is returned. 
 */
CR_API cr_State *cr_create(cr_alloc allocator, void *ud)
{
	cr_State *ts;

	if (allocator == NULL || cr_unlikely((ts = allocator(NULL, sizeof(cr_State), ud)) == NULL))
		return NULL;
	memset(&ts->hooks, 0, sizeof(Hooks));
	ts->hooks.reallocate = allocator;
	ts->hooks.userdata = ud;
	ts->gc.gc_stopped = 1; // wait until 'ts' is initialized
	ts_init(ts);
	ts->gc.gc_stopped = 0;
	return ts;
}


/* Resets 'cr_State' clearing its call stack and closing all
 * to be closed variables. */
CR_API void cr_resetts(cr_State *ts)
{
	resetts(ts, ts->status);
}


/* Free the cr_State allocation, the pointer to cr_State will be nulled out. */
CR_API void cr_destroy(cr_State **tsp)
{
	cr_State *ts;
	GCObject *head, *next;

	if (cr_likely(tsp != NULL)) { // non-null pointer ?
		cr_lock(*tsp);
		if (*tsp == NULL)
			return;
		ts = *tsp;
		HTable_free(ts, &ts->loaded);
		HTable_free(ts, &ts->globids);
		GSARRAY_FREE(ts);
		Array_Variable_free(&ts->globvars, NULL);
		Array_Value_free(&ts->temp, NULL);
		Array_VRef_free(&ts->callstart, NULL);
		Array_VRef_free(&ts->retstart, NULL);
		Array_OSRef_free(&ts->interned, NULL);
		HTable_free(ts, &ts->weakrefs);
		for (head = ts->objects; head != NULL; head = next) {
			next = onext(head);
			ofree(ts, head);
		}
		FREE(ts, ts);
		*tsp = NULL;
	}
}


/* Set panic handler and return old one */
CR_API cr_panic cr_setpanic(cr_State *ts, cr_panic panicfn)
{
	cr_panic old_panic;

	cr_lock(ts);
	old_panic = ts->hooks.panic;
	ts->hooks.panic = panicfn;
	cr_unlock(ts);
	return old_panic;
}


/* Return current version. */
CR_API cr_umem cr_version(cr_State *ts)
{
	UNUSED(ts);
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
CR_API cr_ubyte cr_compare(cr_State *ts, int idx1, int idx2, cr_ord ord)
{
	static void (*ordfuncs[])(cr_State *, Value, Value) = { veq, vne, vlt, vgt, vle, vge };
	Value l, r;
	cr_ubyte res;

	cr_lock(ts);
	criptapi_checkordop(ts, ord);
	criptapi_checkstack(ts, 2);
	l = *i2val(ts, idx1);
	r = *i2val(ts, idx2);
	*ts->sp++ = l; // push left operand
	*ts->sp++ = r; // push right operand
	ordfuncs[ord](ts, l, r);
	res = !ISFALSE(*stkpeek(0));
	cr_unlock(ts);
	return res;
}


/* 
 * Perform equality ordering on values at stack index 'idx1' and 'idx2'.
 * This function will not call overload-able operator methods (__eq__).
 * Result is returned directly without storing it on the stack.
 * Returned value of 1 means values are equal, otherwise 0 is returned. 
 */
CR_API cr_ubyte cr_rawequal(cr_State *ts, int idx1, int idx2)
{
	Value l, r;
	cr_ubyte res;

	cr_lock(ts);
	l = *i2val(ts, idx1);
	r = *i2val(ts, idx2);
	res = raweq(l, r);
	cr_unlock(ts);
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
CR_API void cr_arith(cr_State *ts, cr_ar op)
{
	Value *res;
	int adjust;

	cr_lock(ts);
	criptapi_checkarop(ts, op);
	adjust = 0;
	if (arisbin(op)) {
		criptapi_checkelems(ts, 2);
		adjust = 1;
	} else {
		criptapi_checkelems(ts, 1);
	}
	res = stkpeek(1);
	arith(ts, *res, *stkpeek(0), op, res);
	ts->sp -= adjust; // result is where the first operand was
	cr_unlock(ts);
}


/* Push nil on the stack */
CR_API void cr_pushnil(cr_State *ts)
{
	cr_lock(ts);
	criptapi_pushnil(ts);
	cr_unlock(ts);
}


/* Push number on the stack */
CR_API void cr_pushinteger(cr_State *ts, cr_lint number)
{
	cr_lock(ts);
	criptapi_pushinteger(ts, number);
	cr_unlock(ts);
}


/* Push number on the stack */
CR_API void cr_pushfloat(cr_State *ts, cr_double number)
{
	cr_lock(ts);
	criptapi_pushfloat(ts, number);
	cr_unlock(ts);
}


/* Push string on the stack */
CR_API void cr_pushstring(cr_State *ts, const char *str, cr_umem len)
{
	cr_lock(ts);
	criptapi_pushstr(ts, str, len);
	cr_unlock(ts);
}


/* Push cstring on the stack */
CR_API void cr_pushcstring(cr_State *ts, const char *str)
{
	cr_lock(ts);
	criptapi_pushstr(ts, str, strlen(str));
	cr_unlock(ts);
}


/* Push formatted cstring on the stack, format arguments
 * start from 'argp'. */
CR_API const char *cr_pushvfstring(cr_State *ts, const char *fmt, va_list argp)
{
	const char *str = NULL;
	cr_lock(ts);
	criptapi_pushfstr(ts, fmt, argp);
	str = ascstring(*stkpeek(0));
	cr_unlock(ts);
	return str;
}


/* Push formatted cstring on the stack */
CR_API const char *cr_pushfstring(cr_State *ts, const char *fmt, ...)
{
	const char *str = NULL;
	va_list argp;
	cr_lock(ts);
	va_start(argp, fmt);
	criptapi_pushfstr(ts, fmt, argp);
	va_end(argp);
	str = ascstring(*stkpeek(0));
	cr_unlock(ts);
	return str;
}


/* Push boolean on the stack */
CR_API void cr_pushbool(cr_State *ts, int boolean)
{
	cr_lock(ts);
	cr_checkapi(ts, boolean == 0 || boolean == 1, "invalid boolean.");
	criptapi_pushbool(ts, boolean);
	cr_unlock(ts);
}


/* Auxiliary to 'cr_pushcclosure' and 'cr_pushclass' */
static CClosure *auxpushcclosure(cr_State *ts, cr_cfunc fn, int args, cr_ubyte isvararg,
				int upvals)
{
	CRString *name = asstring(*stkpeek(0));
	CClosure *native = ONative_new(ts, name, fn, args, isvararg, upvals);
	pop(ts); // name
	ts->sp -= upvals;
	while (upvals--)
		native->upvalue[upvals] = *(ts->sp + upvals);
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
CR_API void cr_pushcclosure(cr_State *ts, const char *name, cr_cfunc fn, int args, cr_ubyte isvararg,
			    int upvals)
{
	cr_lock(ts);
	criptapi_checkelems(ts, upvals);
	criptapi_checkptr(ts, fn);
	CRString *fname = ts->faststatic[SS_CSRC];
	if (name)
		fname = OString_new(ts, name, strlen(name));
	criptapi_pusho(ts, fname);
	CClosure *native = auxpushcclosure(ts, fn, args, isvararg, upvals);
	criptapi_pushonative(ts, native);
	cr_unlock(ts);
}


/* Push value from the stack located at 'idx', on top of the stack */
CR_API void cr_push(cr_State *ts, int idx)
{
	cr_lock(ts);
	Value *val = i2val(ts, idx);
	criptapi_pushval(ts, *val);
	cr_unlock(ts);
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
CR_API void cr_pushclass(cr_State *ts, cr_entry entries[], int nup)
{
	cr_lock(ts);
	criptapi_checkelems(ts, nup + 1); // upvalues + class name
	Value classname = *stkpeek(0);
	cr_checkapi(ts, isstring(classname), "Expect string");
	OClass *oclass = OClass_new(ts, asstring(classname));
	pop(ts); // class name
	criptapi_pusho(ts, oclass);
	cr_entry *entry = entries;
	while (entry->name && entry->fn) { // while valid entry
		for (int i = 0; i < nup; i++)
			criptapi_pushval(ts, *stkpeek(nup - 1));
		CRString *name = OString_new(ts, entry->name, strlen(entry->name));
		criptapi_pusho(ts, name);
		int tag = id2omtag(ts, name);
		if (tag == -1) { // not overload ?
			CClosure *native =
				auxpushcclosure(ts, entry->fn, entry->args, entry->isvararg, nup);
			*stkpeek(0) = OBJ_VAL(native);
			rawset(ts, &oclass->mtab, OBJ_VAL(name), OBJ_VAL(native));
			pop(ts); // native
		} else { // overloaded, override 'arity' and 'isvararg'
			CClosure *native = auxpushcclosure(ts, entry->fn, ominfo[tag].arity, 0, nup);
			oclass->vtable[tag] = cast(GCObject *, native);
		}
		entry = ++entries;
	}
	*(ts->sp - nup) = pop(ts); // move class to first upvalue
	popn(ts, nup - 1); // pop the rest of the upvalues
	cr_unlock(ts);
}




/* 
 * Convert 'acceptable' stack index into an absolute index.
 * For example: if there are 5 values on the stack after the
 * callee then -1 would be index 4. 
 */
CR_API int cr_absidx(cr_State *ts, int idx)
{
	return (idx >= 0) ? idx : cast_int(ts->sp - last_frame(ts).callee - 1) + idx;
}



/* Return type of the value on the stack at 'idx'. */
CR_API cr_tt cr_type(const cr_State *ts, int idx)
{
	Value *value = i2val(ts, idx);
	return val2type(*value);
}



/* Return type name of the value on the stack at 'idx'.
 * This returned pointer is 'const' indicating the
 * memory it points to should not be modified. */
CR_API const char *cr_typename(const cr_State *ts, int idx)
{
	Value *value = i2val(ts, idx);
	int type = val2type(*value);
	return ts->faststatic[type]->bytes;
}



/* Convert type tag into name */
CR_API const char *cr_tagname(const cr_State *ts, cr_tt type)
{
	return ts->faststatic[type]->bytes;
}



/* Check if the value on the stack at 'idx' is nil. */
CR_API cr_ubyte cr_isnil(const cr_State *ts, int idx)
{
	return IS_NIL(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is number. */
CR_API cr_ubyte cr_isnumber(const cr_State *ts, int idx)
{
	return IS_NUMBER(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is string. */
CR_API cr_ubyte cr_isstring(const cr_State *ts, int idx)
{
	return isstring(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is bool. */
CR_API cr_ubyte cr_isbool(const cr_State *ts, int idx)
{
	return IS_BOOL(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is class. */
CR_API cr_ubyte cr_isclass(const cr_State *ts, int idx)
{
	return isclassobj(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is instance. */
CR_API cr_ubyte cr_isinstance(const cr_State *ts, int idx)
{
	return isinstance(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is native C function. */
CR_API cr_ubyte cr_isnative(const cr_State *ts, int idx)
{
	return iscfunction(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is bound method (instance method). */
CR_API cr_ubyte cr_ismethod(const cr_State *ts, int idx)
{
	return isboundmethod(*i2val(ts, idx));
}



/* Check if the value on the stack at 'idx' is cript closure. */
CR_API cr_ubyte cr_isclosure(const cr_State *ts, int idx)
{
	return isclosureobj(*i2val(ts, idx));
}




/* Concatenate 2 strings on top of the stack.
 * Pops the string on top of the stack and replaces the first
 * one with the concatenated string.
 * They are concatenated in the order they were pushed on the stack. */
CR_API const char *cr_concat(cr_State *ts)
{
	cr_lock(ts);
	criptapi_checkelems(ts, 2);
	Value right = *stkpeek(0);
	Value left = *stkpeek(1);
	cr_checkapi(ts, isstring(right) && isstring(left), "expect strings");
	concatonstack(ts);
	const char *concated = ascstring(*stkpeek(0));
	cr_unlock(ts);
	return concated;
}


/* Push class method of an instance at idx on top of the stack.
 * If method doesn't exist this function returns 0 otherwise 1.
 * Note: Class instance methods are all cript closures. */
CR_API cr_ubyte cr_getmethod(cr_State *ts, int idx, const char *method)
{
	cr_lock(ts);
	criptapi_checkptr(ts, method);
	Value val = *i2val(ts, idx);
	if (!isinstance(val))
		return 0;
	criptapi_pushstr(ts, method, strlen(method));
	cr_ubyte haveit = bindmethod(ts, asinstance(val)->oclass, *stkpeek(0), val);
	cr_unlock(ts);
	return haveit;
}



/* Pushes the field value of the class instance at 'idx' on top
 * of the stack.
 * If field value was not found or the value at 'idx' is not
 * class instance return 0, otherwise 1. */
CR_API cr_ubyte cr_getfield(cr_State *ts, int idx, const char *field)
{
	cr_ubyte res = 0;
	cr_lock(ts);
	criptapi_checkptr(ts, field);
	Value insval = *i2val(ts, idx);
	if (isinstance(insval)) {
		Instance *instance = asinstance(insval);
		Value key = OBJ_VAL(OString_new(ts, field, strlen(field)));
		Value fieldval;
		if ((res = rawget(ts, &instance->fields, key, &fieldval)))
			criptapi_pushval(ts, fieldval);
	}
	cr_unlock(ts);
	return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__getidx__) on it using the
 * value on top of the stack as index value.
 * If the value is not an instance or the
 * '__getidx__' is not overloaded, this function
 * returns 0, otherwise 1 and the value will
 * be on top of the stack. */
CR_API cr_ubyte cr_getindex(cr_State *ts, int idx)
{
	cr_ubyte res = 0;
	cr_lock(ts);
	criptapi_checkelems(ts, 1); // [index]
	Value *index = stkpeek(0);
	Value value = *i2val(ts, idx);
	res = calloverload(ts, value, OM_GETIDX);
	*index = pop(ts); // replace [index] with result
	cr_unlock(ts);
	return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__setidx__) on it, index value
 * is located one place before the top of the stack
 * while the value getting assigned is on top of the stack.
 * If the value is not an instance or '__setidx__' is not
 * overloaded, this function returns 0, otherwise 1. */
CR_API cr_ubyte cr_setindex(cr_State *ts, int idx)
{
	cr_ubyte res = 0;
	cr_lock(ts);
	criptapi_checkelems(ts, 2); // [index][expr]
	Value value = *i2val(ts, idx);
	res = calloverload(ts, value, OM_SETIDX);
	popn(ts, 2); // pop [index] and [expr]
	cr_unlock(ts);
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
CR_API cr_ubyte cr_rawindex(cr_State *ts, int idx, cr_ubyte what)
{
	cr_ubyte res = 0;
	cr_lock(ts);
	criptapi_checkelems(ts, what == CR_RAWSET ? 2 : 1);
	Value value = *i2val(ts, idx);
	if (!isinstance(value))
		return res;
	res = rawindex(ts, value, what);
	cr_unlock(v);
	return res;
}



/* Push global value on top of the stack.
 * In case global value was found, it will be on top of the stack,
 * and this function will return 1, otherwise nothing will be pushed
 * on the stack and the function will return 0. */
CR_API cr_ubyte cr_getglobal(cr_State *ts, const char *name)
{
	cr_lock(ts);
	criptapi_checkptr(ts, name);
	int res = 0;
	Value gval;
	criptapi_checkptr(ts, name);
	CRString *str = OString_new(ts, name, strlen(name));
	if (rawget(ts, &ts->globids, OBJ_VAL(str), &gval)) {
		int idx = (int)AS_NUMBER(gval);
		criptapi_pushval(ts, ts->globvars.data[idx].value);
		res = 1;
	}
	cr_unlock(ts);
	return res;
}



/* Get panic handler */
CR_API cr_panic cr_getpanic(cr_State *ts)
{
	cr_lock(ts);
	cr_panic panic_handler = ts->hooks.panic;
	cr_unlock(ts);
	return panic_handler;
}



/* Get allocator function */
CR_API cr_alloc cr_getalloc(cr_State *ts, void **ud)
{
	cr_lock(ts);
	cr_alloc alloc = ts->hooks.reallocate;
	if (ud)
		*ud = ts->hooks.userdata;
	cr_unlock(ts);
	return alloc;
}



/* Get boolean value (int 1/0) from the stack at 'idx'.
 * If the value at 'idx' is not a boolean, then the flag
 * if provided 'isbool' is set as 0, otherwise flag is set to 1. */
CR_API cr_ubyte cr_getbool(const cr_State *ts, int idx, cr_ubyte *isbool)
{
	cr_ubyte bval;
	Value val = *i2val(ts, idx);
	cr_ubyte is = tobool(val, &bval);
	if (isbool)
		*isbool = is;
	return bval;
}



/* Get number value (cr_double) from the stack at 'idx'.
 * If the value at 'idx' is not a number, then the flag
 * if provided 'isnum' is set as 0, otherwise flag is set to 1. */
CR_API cr_double cr_getnumber(const cr_State *ts, int idx, cr_ubyte *isnum)
{
	cr_double nval = 0.0;
	Value val = *i2val(ts, idx);
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
CR_API const char *cr_getstring(const cr_State *ts, int idx)
{
	Value val = *i2val(ts, idx);
	return isstring(val) ? ascstring(val) : NULL;
}



/* Get native C function from the stack at 'idx'.
 * Return NULL if the value is not a 'cr_cfunc'. */
CR_API cr_cfunc cr_getcfunction(const cr_State *ts, int idx)
{
	Value val = *i2val(ts, idx);
	return iscfunction(val) ? ascfn(val)->fn : NULL;
}



/* Return the number of values currently on the stack
 * relative to the current function */
CR_API int cr_gettop(const cr_State *ts)
{
	return cast_int(ts->sp - (last_frame(ts).callee + 1));
}



/* Copy value on the stack located at index 'src'
 * to value on the stack located at index 'dest'. */
CR_API void cr_copy(cr_State *ts, int src, int dest)
{
	cr_lock(ts);
	Value *from = i2val(ts, src);
	Value *to = i2val(ts, dest);
	*to = *from;
	cr_unlock(ts);
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
 * cr_rotate(ts, 2, 2);
 * - After right-rotation:
 * [callee][0][1][3][4][2][sp]
 *
 *
 * Example left-rotation:
 * - Before rotation:
 * [callee][0][1][2][3][4][sp]
 * - Do the rotation:
 * cr_rotate(ts, 2, -2);
 * -After left-rotation:
 * [callee][0][1][4][3][2][sp]
 */
CR_API void cr_rotate(cr_State *ts, int idx, int n)
{
	cr_lock(ts);
	Value *end = stkpeek(0);
	Value *start = i2val(ts, idx);
	cr_checkapi(ts, (n >= 0 ? n : -n) <= end - start + 1, "invalid 'n'");
	Value *pivot = (n >= 0 ? end - n : end - n - 1);
	reverse(pivot, start);
	reverse(pivot + 1, end);
	reverse(start, end);
	cr_unlock(ts);
}




/* Call the value on the stack with 'argc' arguments. */
CR_API void cr_call(cr_State *ts, int argc, int retcnt)
{
	cr_lock(ts);
	cr_checkapi(ts, retcnt >= CR_MULRET, "invalid return count");
	criptapi_checkelems(ts, argc + 1);
	criptapi_checkresults(ts, argc, retcnt);
	Value *fn = ts->sp - (argc + 1);
	ncall(ts, fn, *fn, retcnt);
	cr_unlock(ts);
}



/* Data used for 'fcall' */
struct CallData {
	Value *callee;
	int retcnt;
};


/* Wrapper function */
static void fcall(cr_State *ts, void *userdata)
{
	struct CallData *cd = cast(struct CallData *, userdata);
	ncall(ts, cd->callee, *cd->callee, cd->retcnt);
}


/* Protected call.
 * Same as cr_call except this runs the function in protected
 * mode, meaning that in case the function errors it won't print
 * invoke panic handler.
 * Instead it restores the old call frame and pushes the error object
 * on top of the stack.
 * This function returns 'cr_status' [defined @cript.h] code. */
CR_API cr_status cr_pcall(cr_State *ts, int argc, int retcnt)
{
	cr_lock(ts);
	cr_checkapi(ts, retcnt >= CR_MULRET, "invalid return count");
	criptapi_checkelems(ts, argc + 1);
	criptapi_checkresults(ts, argc, retcnt);
	struct CallData cd;
	cd.retcnt = retcnt;
	cd.callee = ts->sp - (argc + 1);
	int status = pcall(ts, fcall, &cd, save_stack(ts, cd.callee));
	cr_unlock(ts);
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
CR_API cr_status cr_load(cr_State *ts, cr_reader reader, void *userdata, const char *source)
{
	BuffReader br;
	cr_lock(ts);
	BuffReader_init(ts, &br, reader, userdata);
	cr_status status = pcompile(ts, &br, source, 0);
	cr_unlock(ts);
	return status;
}



/* Garbage collection API.
 * Refer to the @cript.h and 'cr_gco' enum defined in the same header. */
CR_API cr_umem cr_incgc(cr_State *ts, cr_incgco option, ...)
{
	va_list argp;
	cr_umem res = 0;
	cr_lock(ts);
	va_start(argp, option);
	switch (option) {
	case GCO_STOP:
		res = ts->gc.gc_stopped;
		ts->gc.gc_stopped = 1;
		break;
	case GCO_RESTART:
		res = ts->gc.gc_stopped;
		ts->gc.gc_stopped = 0;
		break;
	case GCO_COLLECT:
		res = incgc(ts);
		break;
	case GCO_COUNT:
		res = ts->gc.gc_allocated;
		break;
	case GCO_ISRUNNING:
		res = (ts->gc.gc_stopped == 0);
		break;
	case GCO_NEXTGC:
		res = ts->gc.gc_nextgc;
		break;
	}
	va_end(argp);
	cr_unlock(ts);
	return res;
}




// TODO: Implement
CR_API void cr_dumpstack(cr_State *ts)
{
	(void)(0);
}



/* Converts value on the stack at the 'idx' into string.
 * Additionally if the 'len' and/or 'hash' are non-NULL then
 * it also fills them.
 * This can call overload-able method '__tostring__'. */
CR_API const char *cr_tostring(cr_State *ts, int idx, cr_umem *len, cr_hash *hash)
{
	const char *str = NULL;
	cr_lock(ts);
	Value *v = i2val(ts, idx);
	CRString *ostr = vtostr(ts, v, *v, 0);
	str = ostr->bytes;
	if (len)
		*len = ostr->len;
	if (hash)
		*hash = ostr->hash;
	cr_unlock(ts);
	return str;
}



/* Sets the new stack top relative to the current function */
CR_API void cr_settop(cr_State *ts, int idx)
{
	cr_lock(ts);
	Value *newtop;
	Value *fn = ts->frames[ts->fc - 1].callee;
	ptrdiff_t diff;
	if (idx >= 0) {
		cr_checkapi(ts, idx < ((Value *)stklast(ts) - fn), "index too big.");
		diff = ((fn + 1) + idx) - ts->sp;
		for (; diff > 0; diff--)
			*ts->sp++ = NIL_VAL;
	} else { // index negative
		cr_checkapi(ts, -idx <= (ts->sp - fn), "invalid index.");
		diff = idx + 1;
	}
	ts->sp += diff; // set new top
	if (diff < 0)
		closeupval(ts, ts->sp);
	cr_unlock(ts);
}



/* Set global value 'name' to the value on top of the stack.
 * In case the global variable 'name' does not exist, then
 * the new one is declared and 'isconst' modifier is considered
 * when creating it.
 * Otherwise 'isconst' modifier is ignored and the global
 * variable is set to the new value UNLESS the variable is
 * set as 'fixed'; in that case runtime error is invoked. */
CR_API cr_ubyte cr_setglobal(cr_State *ts, const char *name, int isconst)
{
	cr_lock(ts);
	criptapi_checkelems(ts, 1); // value must be present
	criptapi_checkptr(ts, name);
	Value newval = *stkpeek(0);
	Value key = OBJ_VAL(OString_new(ts, name, strlen(name)));
	cr_ubyte isnew = 0;
	Value gidx;
	if ((isnew = !rawget(ts, &ts->globids, key, &gidx))) {
		criptapi_pushval(ts, key);
		Variable gvar = { newval, 0x01 & isconst };
		Value idx = NUMBER_VAL(Array_Variable_push(&ts->globvars, gvar));
		rawset(ts, &ts->globids, key, idx);
		popn(ts, 2); // value and key
	} else {
		Variable *gvar = Array_Variable_index(&ts->globvars, AS_NUMBER(gidx));
		if (cr_unlikely(VISCONST(gvar))) {
			fixederror(ts, globalname(ts, AS_NUMBER(gidx))->bytes);
		}
		gvar->value = newval;
		pop(ts); // value
	}
	cr_unlock(ts);
	return isnew;
}



/* Set the field of the class instance to the value on top of the stack.
 * Class should be located at 'idx' and the name of the field to be set is 'field'.
 * This sets the field to that value and pops it off the top of the stack.
 * Returns 1 if the field didn't exist before or false if the value
 * of the field got overwritten. */
CR_API cr_ubyte cr_setfield(cr_State *ts, int idx, const char *field)
{
	cr_lock(ts);
	criptapi_checkelems(ts, 1);
	criptapi_checkptr(ts, field);
	Value insval = *i2val(ts, idx);
	cr_checkapi(ts, isinstance(insval), "expect class instance");
	Instance *instance = asinstance(insval);
	criptapi_pushcstr(ts, field);
	cr_ubyte res = rawset(ts, &instance->fields, *stkpeek(0), *stkpeek(1));
	ts->sp -= 2; // pop value and key
	cr_unlock(ts);
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
CR_API cr_ubyte cr_getupvalue(cr_State *ts, int fidx, int idx)
{
	cr_lock(ts);
	cr_ubyte ret = 0;
	Value fn = *i2val(ts, fidx);
	Value *upval = getupval(fn, idx);
	if (upval) {
		criptapi_pushval(ts, *upval);
		ret = 1;
	}
	cr_unlock(ts);
	return ret;
}



/* Sets the upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pops the top value on the stack and sets
 * it as the new value of the upvalue.
 * If the upvalue doesn't exist and/or 'fidx' is not a
 * closure or native C function, then the function returns 0
 * indicating the upvalue was not set, otherwise it returns 1. */
CR_API int cr_setupvalue(cr_State *ts, int fidx, int idx)
{
	cr_lock(ts);
	criptapi_checkelems(ts, 1);
	int changed = 0;
	Value fn = *i2val(ts, fidx);
	Value *upval = getupval(fn, idx);
	if (upval) {
		*upval = *stkpeek(0);
		ts->sp--;
		changed = 1;
	}
	cr_unlock(ts);
	return changed;
}


static Instance *getinstance(cr_State *ts, int idx)
{
	TValue *v;
	v = i2val(ts, idx);
	checkapi(ts, ttisins(v), "expect instance");
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
CR_API cr_ubyte cr_nextproperty(cr_State *ts, int idx, cr_ubyte what)
{
	cr_ubyte hasnext;
	Instance *instance;
	HTable *tab;
	TValue *key;

	cr_lock(ts);
	hasnext = 0;
	checkapi_values(ts, 2); /* key + instance */
	checkapi_stack(ts, 1); /* value */
	instance = getinstance(ts, idx);
	key = speek(0);
	if (what == 0)
		tab = &instance->fields;
	else
		tab = &instance->oclass->methods;
	hasnext = cr_htable_next(ts, tab, key);
	if (hasnext)
		api_incsp(ts); /* push value */
	else
		api_decsp(ts); /* pop key */
	cr_unlock(ts);
	return hasnext;
}



/* Return the length of the string at 'idx'.
 * If the value is not a string then return 0. */
CR_API cr_umem cr_strlen(const cr_State *ts, int idx)
{
	Value val = *i2val(ts, idx);
	return (isstring(val) ? asstring(val)->len : 0);
}


/* Return 'cr_State' 'cr_status' code. */
CR_API cr_status cr_getstatus(cr_State *ts)
{
	UNUSED(ts);
	return ts->status;
}


/* Invoke a runetime error with errcode */
CR_API int cr_error(cr_State *ts, cr_status errcode)
{
	cr_lock(ts);
	Value *errobj = stkpeek(0);
	criptapi_checkelems(ts, 1);
	criptapi_checkerrcode(ts, errcode);
	runerror(ts, errcode); // cr_unlock in here
	return 0; // to avoid compiler warnings
}
