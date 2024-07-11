#include "crmeta.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crgc.h"


OClass *cr_meta_newclass(cr_State *ts, OString *id)
{
	OClass *cls;

	cls = cr_gc_new(ts, sizeof(OClass), CR_VCLASS, OClass);
	cls->methods = NULL;
	cls->name = id;
	memset(cls->vtable, 0, sizeof(cls->vtable) / sizeof(cls->vtable[0]));
	setsv2cls(ts, ts->stacktop.p++, cls);
	cls->methods = cr_htable_new(ts);
	ts->stacktop.p--;
	return cls;
}


Instance *cr_meta_newinstance(cr_State *ts, OClass *cls)
{
	Instance *ins;

	ins = cr_gc_new(ts, sizeof(Instance), CR_VINSTANCE, Instance);
	ins->oclass = cls;
	ins->fields = NULL;
	setsv2ins(ts, ts->stacktop.p++, ins);
	ins->fields = cr_htable_new(ts);
	ts->stacktop.p--;
	return ins;
}


InstanceMethod *cr_meta_newinstancemethod(cr_State *ts, Instance *receiver,
						CrClosure *method)
{
	InstanceMethod *im;

	im = cr_gc_new(ts, sizeof(InstanceMethod), CR_VMETHOD, InstanceMethod);
	im->receiver = receiver;
	im->method = obj2gco(method);
	return im;
}


UserData *cr_meta_newuserdata(cr_State *ts, size_t size, int nuv)
{
	UserData *ud;
	int i;

	ud = cr_gc_new(ts, sizeofud(nuv, size), CR_VUDATA, UserData);
	ud->nuv = nuv;
	ud->size = size;
	for (i = 0; i < CR_NUMM; i++)
		setnilval(&ud->vtable[i]);
	return ud;
}
