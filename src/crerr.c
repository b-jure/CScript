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

#include "crerr.h"
#include "crconf.h"
#include "crlimits.h"
#include "crobject.h"
#include "crvalue.h"
#include "crvm.h"


/* ==================== runtime errors ====================== */


/* Performs long jump if there is one otherwise prints
 * the runtime error and invokes either a panic handler or aborts.
 * Error message is on top of the stack, or whatever value
 * was passed to cr_error. */
cr_noret runerror(cr_State *ts, int status)
{
	struct cr_longjmp *errjmp;

	ts->status = status;
	errjmp = ts->errjmp;
	if (errjmp) { // protected call ?
		errjmp->status = status;
		longjmp(errjmp->buf, 1);
	} else if (ts->hooks.panic) { // panic handler ?
		cr_unlock(ts);
		ts->hooks.panic(ts);
	} else {
		abort(); // gg
	}
	cr_unreachable;
}


cr_noret memerror(cr_State *ts)
{
	push(ts, OBJ_VAL(ts->memerror));
	runerror(ts, S_EMEM);
}

cr_noret bclimiterror(cr_State *ts, const char *extra, ...)
{
	va_list ap;

	va_start(ap, extra);
	cr_pushvfstring(ts, "bytecode limit exceeded (%s)", ap);
	va_end(ap);
	runerror(ts, S_EBCLIMIT);
}

cr_noret gslimiterror(cr_State *ts)
{
	cr_pushfstring(ts, "gray stack limit reached %zu", ts_GRAYSTACK_LIMIT >> 1);
	runerror(ts, S_GSLIMIT);
}

cr_noret ordererror(cr_State *ts, Value a, Value b)
{
	const char *t1 = ts->faststatic[val2type(a)]->bytes;
	const char *t2 = ts->faststatic[val2type(a)]->bytes;

	if (strcmp(t1, t2) == 0)
		cr_pushfstring(ts, "Attempt to compare two %s values.", t1);
	else
		cr_pushfstring(ts, "Attempt to compare %s and %s.", t1, t2);
	runerror(ts, S_ECMP);
}


cr_noret binoperror(cr_State *ts, Value a, Value b, cr_om op)
{
	static const char *fmt = "Attempt to perform binary %s on %s (left) and %s (right).";
	push(ts, OBJ_VAL(vtostr(ts, a, 0)));
	push(ts, OBJ_VAL(vtostr(ts, b, 0)));
	const char *operation = ts->faststatic[op + SS_OPADD]->storage;
	const char *left = ascstring(*stkpeek(1));
	const char *right = ascstring(*stkpeek(0));
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, operation, left, right)));
	runerror(ts, S_EARBIN);
}


cr_noret unoperror(cr_State *ts, Value a, cr_om op)
{
	static const char *fmt = "Attempt to perform unary '%s' on %s.";
	push(ts, OBJ_VAL(vtostr(ts, a, 0)));
	const char *operation = ts->faststatic[op + SS_OPADD]->storage;
	const char *operand = ascstring(*stkpeek(0));
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, operation, operand)));
	runerror(ts, S_EARUN);
}


cr_noret omreterror(cr_State *ts, const char *what, cr_om tag)
{
	static const char *fmt = "%s method must return value of type %s.";
	const char *method = ts->faststatic[tag + SS_INIT]->storage;
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, what, method)));
	runerror(ts, S_EOMRET);
}


cr_noret ofmterror(cr_State *ts, int8_t c, Value callee)
{
	static const char *fmt = "Invalid format specifier '%%%c' for '%s'";
	push(ts, OBJ_VAL(vtostr(ts, callee, 0)));
	const char *fn = ascstring(*stkpeek(0));
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, c, fn)));
	runerror(ts, S_ESTRFMT);
}


cr_noret sovferror(cr_State *ts)
{
	static const char *fmt = "Stack overflow, limit overflown -> %d.";
	ts->sp--; // make some space
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, ts_STACK_LIMIT)));
	runerror(ts, S_ESOVERFLOW);
}


cr_noret udproperror(cr_State *ts, Value property, OClass *oclass)
{
	static const char *fmt = "Property '%s' is not defined for <class '%s'>.";
	const char *pname = ascstring(property);
	const char *classname = oclass->name->storage;
	push(ts, OBJ_VAL(OString_fmt(ts, pname, classname)));
	runerror(ts, S_EUDPROPERTY);
}


cr_noret retovferror(cr_State *ts, const char *fn)
{
	static const char *fmt = "Called function '%s' return count overflows the stack.";
	if (ts->sp - ts->stack >= ts_STACK_LIMIT)
		ts->sp--;
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, fn)));
	runerror(ts, S_ESOVERFLOW);
}


cr_noret arityerror(cr_State *ts, int expected, int got)
{
	static const char *fmt = "Expected %d arguments instead got %d.";
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, expected, got)));
	runerror(ts, S_EARITY);
}


cr_noret fcovferror(cr_State *ts)
{
	static const char *fmt = "Callstack overflow, limit overflown -> %lu.";
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, ts_CALLSTACK_LIMIT)));
	runerror(ts, S_EFOVERFLOW);
}


cr_noret callerror(cr_State *ts, Value callee)
{
	static const char *fmt = "Tried calling non-callable value '%s'.";
	push(ts, OBJ_VAL(vtostr(ts, callee, 0)));
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, ascstring(*stkpeek(0)))));
	runerror(ts, S_ECALL);
}


cr_noret ipaerror(cr_State *ts, Value notinstance)
{
	static const char *fmt = "Invalid property access, tried accessing property on %s";
	push(ts, OBJ_VAL(vtostr(ts, notinstance, 1)));
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, ascstring(*stkpeek(0)))));
	runerror(ts, S_EPACCESS);
}


cr_noret redefgerror(cr_State *ts, const char *gname)
{
	static const char *fmt = "Redefinition of global variable '%s'.";
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, gname)));
	runerror(ts, S_EGLOBALREDEF);
}


cr_noret udgerror(cr_State *ts, const char *gname)
{
	static const char *fmt = "Undefined global variable '%s'.";
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, gname)));
	runerror(ts, S_EUDGLOBAL);
}


cr_noret fixederror(cr_State *ts, const char *var)
{
	static const char *fmt = "Can't assign to variable '%s', it is declared as 'fixed'.";
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, var)));
	runerror(ts, S_EFIXEDASSIGN);
}


cr_noret nilidxerror(cr_State *ts)
{
	push(ts, OBJ_VAL(OString_newlit(ts, "Can't index with 'nil'.")));
	runerror(ts, S_ENILIDX);
}


cr_noret inheriterror(cr_State *ts, Value notclass)
{
	const char *fmt = "Can't inherit from '%s', value must be class object.";

	push(ts, OBJ_VAL(vtostr(ts, notclass, 1)));
	push(ts, OBJ_VAL(OString_fmt(ts, fmt, ascstring(*stkpeek(0)))));
	runerror(ts, S_EINHERIT);
}

/* --------------------------------------------------------- */ // runtime errors
