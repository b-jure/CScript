/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in cscript.h
*/

#ifndef CRMETA_H
#define CRMETA_H

#include "csconf.h"
#include "cscript.h"
#include "cobject.h"


/* number of elements in VMT */
#define SIZEVMT         (sizeof(TValue) * CS_MM_N)


CSI_DEC(const char *const csO_typenames[CSI_TOTALTYPES]);

#define typename(t)     csO_typenames[(t) + 1]


CSI_FUNC void csMM_init(cs_State *ts);
CSI_FUNC TValue *csMM_newvmt(cs_State *ts);
CSI_FUNC const TValue *csMM_get(cs_State *ts, const TValue *v, cs_MM mm);
CSI_FUNC OClass *csMM_newclass(cs_State *ts);
CSI_FUNC Instance *csMM_newinstance(cs_State *ts, OClass *cls);
CSI_FUNC UserData *csMM_newuserdata(cs_State *ts, size_t size, int nuv);
CSI_FUNC IMethod *csMM_newinsmethod(cs_State *ts, Instance *receiver,
				    const TValue *method);
CSI_FUNC void csMM_callhtm(cs_State *ts, const TValue *fn, const TValue *p1,
                           const TValue *p2, const TValue *p3);
CSI_FUNC void csMM_callhtmres(cs_State *ts, const TValue *fn, const TValue *p1,
                              const TValue *p2, SPtr res);
CSI_FUNC void csMM_callbinres(cs_State *ts, const TValue *fn, const TValue *v1,
                              const TValue *v2, SPtr res);
CSI_FUNC void csMM_callunaryres(cs_State *ts, const TValue *fn,
                                const TValue *v, SPtr res);
CSI_FUNC int csMM_order(cs_State *ts, const TValue *v1, const TValue *v2,
		  	cs_MM mm);
CSI_FUNC int csMM_orderI(cs_State *ts, const TValue *v1, int v2, int flip,
                         int isflt, cs_MM mm);
CSI_FUNC void csMM_trybin(cs_State *ts, const TValue *v1, const TValue *v2,
		    	  SPtr res, cs_MM mm);
CSI_FUNC void csMM_tryunary(cs_State *ts, const TValue *v, SPtr res,
                             cs_MM mm);
CSI_FUNC void csMM_tryconcat(cs_State *ts);
CSI_FUNC void csMM_freeclass(cs_State *ts, OClass *cls);
CSI_FUNC void csMM_freeinstance(cs_State *ts, Instance *ins);
CSI_FUNC void csMM_freeuserdata(cs_State *ts, UserData *ud);

#endif
