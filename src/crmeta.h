#ifndef CRMETA_H
#define CRMETA_H

#include "cript.h"
#include "crobject.h"


CRI_FUNC void crMm_init(cr_State *ts);
CRI_FUNC const TValue *crMm_get(cr_State *ts, const TValue *v, int mt);
CRI_FUNC OClass *crMm_newclass(cr_State *ts, OString *id);
CRI_FUNC Instance *crMm_newinstance(cr_State *ts, OClass *cls);
CRI_FUNC UserData *crMm_newuserdata(cr_State *ts, size_t size, int nuv);
CRI_FUNC InstanceMethod *crMm_newinstancemethod(cr_State *ts, 
                                                Instance *receiver,
						CrClosure *method);
CRI_FUNC void crMm_callres(cr_State *ts, const TValue *selfarg,
                           const TValue *fn, const TValue *v1,
                           const TValue *v2, SPtr res);
CRI_FUNC int crMm_order(cr_State *ts, const TValue *v1, const TValue *v2,
		  	SPtr res, int mt);
CRI_FUNC void crMm_arithm(cr_State *ts, const TValue *v1, const TValue *v2,
		    	  SPtr res, int mt);
CRI_FUNC void crMm_freeclass(cr_State *ts, OClass *cls);
CRI_FUNC void crMm_freeinstance(cr_State *ts, Instance *ins);

#endif
