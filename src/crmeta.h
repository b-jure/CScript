#ifndef CRMETA_H
#define CRMETA_H


#include "crobject.h"


#define cr_meta_vget(ts,v,mt) \
	(ttiso(v) ? cr_meta_get(ts, oval(v), mt) : NULL)


CRI_FUNC void cr_meta_init(cr_State *ts);
CRI_FUNC const TValue *cr_meta_get(cr_State *ts, const GCObject *o, int mt);
CRI_FUNC OClass *cr_meta_newclass(cr_State *ts, OString *id);
CRI_FUNC Instance *cr_meta_newinstance(cr_State *ts, OClass *cls);
CRI_FUNC UserData *cr_meta_newuserdata(cr_State *ts, size_t size, int nuv);
CRI_FUNC InstanceMethod *cr_meta_newinstancemethod(cr_State *ts, Instance *receiver,
						   CrClosure *method);

CRI_FUNC void cr_meta_callres(cr_State *ts, const TValue *selfarg, const TValue *fn,
		     	      const TValue *v1, const TValue *v2, SPtr res);
CRI_FUNC int cr_meta_order(cr_State *ts, const TValue *v1, const TValue *v2,
		  	   SPtr res, int mt);
CRI_FUNC void cr_meta_arithm(cr_State *ts, const TValue *v1, const TValue *v2,
		    	     SPtr res, int mt);

CRI_FUNC void cr_meta_freeclass(cr_State *ts, OClass *cls);
CRI_FUNC void cr_meta_freeinstance(cr_State *ts, Instance *ins);

#endif
