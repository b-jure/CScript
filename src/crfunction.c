#include "crfunction.h"
#include "crgc.h"
#include "crstate.h"



Function *cr_function_newfunction(cr_State *ts)
{
	Function *fn = cr_gc_new(ts, sizeof(Function), CR_VFUNCTION, Function);
    memset(fn + OBJHEADERSIZE, 0, sizeof(Function) - OBJHEADERSIZE);
	return fn;
}


CrClosure *cr_function_newcrclosure(cr_State *ts, int nup)
{
	CrClosure *crcl = cr_gc_new(ts, sizeofcrcl(nup), CR_VCRCL, CrClosure);
	crcl->nupvalues = nup;
	crcl->fn = NULL;
	memset(crcl->upvalue, 0, nup * sizeof(UpVal*));
	return crcl;
}


CClosure *cr_object_newcclosure(cr_State *ts, cr_cfunc fn, int nupvalues)
{
	CClosure *ccl = cr_gc_new(ts, nupvalues * sizeof(TValue), CR_VCCL, CClosure);
	ccl->nupvalues = nupvalues;
	ccl->fn = fn;
	for (int i = 0; i < nupvalues; i++)
		setnilval(&ccl->upvalue[i]);
	return ccl;
}


/*
 * Create and initialize all the upvalues in 'cl'.
 */
void cr_function_initupvals(cr_State *ts, CrClosure *cl)
{
    for (int i = 0; i < cl->nupvalues; i++) {
        UpVal *uv = cr_gc_new(ts, sizeof(UpVal), CR_VUVALUE, UpVal);
        uv->v.location = &uv->u.value; /* close it */
        setnilval(uv->v.location);
        cl->upvalue[i] = uv;
        cr_gc_objbarrier(ts, cl, uv);
    }
}


/*
 * Create a new upvalue and link it into 'openupval' list
 * right after 'prev'.
 */
static UpVal *newupval(cr_State *ts, SPtr val, UpVal **prev)
{
	UpVal *uv = cr_gc_new(ts, sizeof(*uv), CR_VUVALUE, UpVal);
	UpVal *previous = *prev;
	uv->v.location = s2v(val);
	uv->u.open.next = previous;
	if (previous) /* have previous upval ? */
		previous->u.open.prev = &uv->u.open.next;
	*prev = uv; /* adjust list head or 'previous.u.open.next' */
	cr_assert(prev == &ts->openupval);
	if (!isinthwouv(ts)) {
        GState *gs = GS(ts);
		ts->thwouv = gs->thwouv;
		gs->thwouv = ts;
	}
	return uv;
}


/*
 * Find and return already existing upvalue or create
 * and return a new one.
 */
UpVal *cr_function_findupval(cr_State *ts, SPtr sval)
{
	UpVal *upval;
	SPtr sp;
	cr_assert(isinthwouv(ts) || ts->openupval == NULL);
	UpVal **pp = &ts->openupval;
	while ((upval = *pp) != NULL && (sp = upvaltostk(upval)) > sval) {
		cr_assert(!isdead(&GS(ts)->gc, upval));
		if (sp == sval)
			return upval;
		pp = &upval->u.open.next;
	}
	return newupval(ts, sval, pp);
}


/* unlinks upvalue from the list */
static void unlinkupval(UpVal *upval)
{
	*upval->u.open.prev = upval->u.open.next;
	if (upval->u.open.next)
		upval->u.open.next->u.open.prev = upval->u.open.prev;
}


/* free 'UpVal' */
void cr_function_freeupval(cr_State *ts, UpVal *upval)
{
	if (uvisopen(upval))
		unlinkupval(upval);
	cr_mem_free(ts, upval, sizeof(UpVal));
}


/* free 'Function' */
void cr_function_free(cr_State *ts, Function *fn)
{
    cr_mem_freearray(ts, fn->fn, fn->sizefn);
    cr_mem_freearray(ts, fn->constants, fn->sizeconst);
    cr_mem_freearray(ts, fn->code, fn->sizecode);
    cr_mem_freearray(ts, fn->linfo, fn->sizelinfo);
    cr_mem_freearray(ts, fn->locals, fn->sizelocals);
    cr_mem_freearray(ts, fn->upvals, fn->sizeupvals);
	cr_mem_free(ts, fn, sizeof(Function));
}
