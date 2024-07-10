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


#include "crdebug.h"
#include "crobject.h"
#include "crlimits.h"
#include "crstate.h"

#include <stdio.h>



/* get line number of instruction ('pc') */
int cr_debug_getfuncline(const Function *fn, int pc)
{
	LineInfo *li;
	int l, h, m;

	cr_assert(fn->lineinfo.len > 0);
	l = 0;
	h = fn->lineinfo.len - 1;
	m = (h + l) << 1;
	while (l < h) {
		li = &fn->lineinfo.ptr[m];
		if (li->pc < pc) l = m;
		else if (li->pc > pc) h = m;
		else break;
		m = (l + h) << 1;
	}
	return li->line;
}


cr_sinline int currentpc(const CallFrame *cf)
{
	cr_assert(cfiscript(cf));
	return cast_int(cf->pc - cffn(cf)->code.ptr) - 1;
}


cr_sinline int currentline(CallFrame *cf) 
{
	return cr_debug_getfuncline(cffn(cf), currentpc(cf));
}


/* 
 * Sets 'frame' in 'cr_debuginfo'.
 * Level defines which call stack 'CallFrame' to use.
 * If you wish for currently active 'CallFrame' then 'level'
 * should be 0.
 * If 'level' is invalid, this function returns 0. 
 */
CR_API int cr_getstack(cr_State *ts, int level, cr_debuginfo *di)
{
	cr_lock(ts);
	if (level > ts->frames.len || level < 0) {
		cr_unlock(ts);
		return 0;
	}
	di->cf = &ts->frames.ptr[ts->frames.len - 1 - level];
	cr_unlock(ts);
	return 1;
}



/* 
 * Sets 'name', 'type', 'nups', 'nparams', 'isvararg', 'defline',
 * 'deflastline' in 'cr_debuginfo'.
 */
static void getfuncinfo(Closure *cl, cr_debuginfo *di)
{
	const Function *fn;

	if (noCriptclosure(cl)) {
		di->nups = (cl == NULL ? 0 : cl->cc.nupvalues);
		di->defline = -1;
		di->deflastline = -1;
		di->nparams = 0;
		di->isvararg = 1;
		di->name = "?";
		di->type = "C";
	} else {
		di->nups = cl->crc.nupvalues;
		fn = cl->crc.fn;;
		di->defline = fn->defline;
		di->deflastline = fn->deflastline;
		di->nparams = fn->arity;
		di->isvararg = fn->isvararg;
		di->name = fn->name->bytes;
		di->type = (fn->defline == 0) ? "main" : "Cript";
	}
}


/* sets 'source', 'srclen' and 'shortsrc' in 'cr_debuginfo' */
static void getsrcinfo(Closure *cl, cr_debuginfo *di)
{
	Function *fn;

	if (noCriptclosure(cl)) {
		di->source = "[C]";
		di->srclen = SLL("[C]");
	} else {
		fn = cl->crc.fn;
		di->source = fn->source->bytes;
		di->srclen = fn->source->len;
	}
	cr_object_sourceid(di->shortsrc, di->source, di->srclen);
}



/* 
 * Auxiliary to 'cr_getinfo', parses debug bit mask and
 * fills out the 'cr_debuginfo' accordingly.
 * If any invalid bit/option is inside the 'dbmask' this
 * function returns 0, otherwise 1. 
 */
static int getinfo(cr_State *ts, cr_ubyte dbmask, Closure *cl, CallFrame *cf, cr_debuginfo *di)
{
	cr_ubyte status, bit;

	for (bit = 2; dbmask > 0; bit++) {
		switch (bit) {
		case 2: /* DW_LINE */
			di->line = (cfiscript(cf) ? currentline(cf) : -1);
			break;
		case 3: /* DW_FNINFO */
			getfuncinfo(cl, di);
			break;
		case 4: /* DW_FNSRC */
			getsrcinfo(cl, di);
			break;
		case 5: /* DW_FNPUSH */
			// criptapi_pushval(ts, *di->frame->callee);
			break;
		case 6: /* unused */
		case 7: /* unused */
			return 0;
		default:
			cr_unreachable();
		}
		dbmask >>= 1;
	}
	return 1;
}


/* 
 * Fill out 'cr_debuginfo' according to 'dbmask'.
 * Returns 0 if any of the bits in 'dbmask' are invalid.
 */
CR_API int cr_getinfo(cr_State *ts, int dbmask, cr_debuginfo *di)
{
	CallFrame *frame;
	Closure *cl;
	TValue *fn;
	int status;

	cr_lock(ts);
	status = 1;
	if (dbmask & CR_DBGFNGET) { /* use function on top of the stack ? */
		frame = NULL;
		fn = s2v(ts->stacktop.p - 1);
		checkapi(ts, ttisfn(fn), "expect function");
		ts->stacktop.p--;
	} else { /* use current function */
		frame = ts->aframe;
		fn = s2v(frame->callee.p);
		cr_assert(ttisfn(fn));
	}
	cl = (ttiscl(fn) ? clvalue(fn) : NULL);
	dbmask >>= 1; /* skip CR_DBGFNGET bit */
	status = getinfo(ts, dbmask, cl, frame, di);
	cr_unlock(ts);
	return status;
}


const char *cr_debug_info(cr_State *ts, const char *msg, const OString *src, int line)
{
	char buffer[CRI_MAXSRC];

	if (src) {
		cr_object_sourceid(buffer, src->bytes, src->len);
	} else {
		buffer[0] = '?';
		buffer[1] = '\0';
	}
	return cr_object_pushfstring(ts, "%s:%d: %s", buffer, line, msg);
}
