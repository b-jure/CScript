#ifndef CRMETA_H
#define CRMETA_H

#include "crconf.h"
#include "cript.h"
#include "crobject.h"


CRI_DEC(const char *mmnames[CR_NUM_MM]);


CRI_FUNC void crMM_init(cr_State *ts);
CRI_FUNC const TValue *crMM_get(cr_State *ts, const TValue *v, cr_MM mm);
CRI_FUNC OClass *crMM_newclass(cr_State *ts);
CRI_FUNC Instance *crMM_newinstance(cr_State *ts, OClass *cls);
CRI_FUNC UserData *crMM_newuserdata(cr_State *ts, size_t size, int nuv);
CRI_FUNC IMethod *crMM_newinsmethod(cr_State *ts, Instance *receiver,
				    const TValue *method);
CRI_FUNC void crMM_callhtm(cr_State *ts, const TValue *fn, const TValue *p1,
                           const TValue *p2, const TValue *p3);
CRI_FUNC void crMM_callhtmres(cr_State *ts, const TValue *fn, const TValue *p1,
                              const TValue *p2, SPtr res);
CRI_FUNC void crMM_callbinres(cr_State *ts, const TValue *fn, const TValue *v1,
                              const TValue *v2, SPtr res);
CRI_FUNC void crMM_callunaryres(cr_State *ts, const TValue *fn,
                                const TValue *v, SPtr res);
CRI_FUNC int crMM_order(cr_State *ts, const TValue *v1, const TValue *v2,
		  	cr_MM mm);
CRI_FUNC int crMM_orderI(cr_State *ts, const TValue *v1, int v2, int flip,
                         int isflt, cr_MM mm);
CRI_FUNC void crMM_trybin(cr_State *ts, const TValue *v1, const TValue *v2,
		    	  SPtr res, cr_MM mm);
CRI_FUNC void crMM_tryunary(cr_State *ts, const TValue *v, SPtr res,
                             cr_MM mm);
CRI_FUNC void crMM_freeclass(cr_State *ts, OClass *cls);
CRI_FUNC void crMM_freeinstance(cr_State *ts, Instance *ins);
CRI_FUNC void crMM_freeuserdata(cr_State *ts, UserData *ud);

#endif
