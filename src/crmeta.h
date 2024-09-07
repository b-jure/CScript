#ifndef CRMETA_H
#define CRMETA_H


#include "crobject.h"


#define crMM_vget(ts,v,mt) \
	(ttiso(v) ? crMM_get(ts, oval(v), mt) : NULL)


CRI_FUNC void crMM_init(cr_State *ts);
CRI_FUNC const TValue *crMM_get(cr_State *ts, const GCObject *o, int mt);
CRI_FUNC OClass *crMM_newclass(cr_State *ts, OString *id);
CRI_FUNC Instance *crMM_newinstance(cr_State *ts, OClass *cls);
CRI_FUNC UserData *crMM_newuserdata(cr_State *ts, size_t size, int nuv);
CRI_FUNC InstanceMethod *crMM_newinstancemethod(cr_State *ts, Instance *receiver,
						CrClosure *method);

CRI_FUNC void crMM_callres(cr_State *ts, const TValue *selfarg, const TValue *fn,
		     	      const TValue *v1, const TValue *v2, SPtr res);
CRI_FUNC int crMM_order(cr_State *ts, const TValue *v1, const TValue *v2,
		  	SPtr res, int mt);
CRI_FUNC void crMM_arithm(cr_State *ts, const TValue *v1, const TValue *v2,
		    	  SPtr res, int mt);

CRI_FUNC void crMM_freeclass(cr_State *ts, OClass *cls);
CRI_FUNC void crMM_freeinstance(cr_State *ts, Instance *ins);

#endif
