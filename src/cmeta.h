/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in cscript.h
*/

#ifndef CMETA_H
#define CMETA_H

#include "csconf.h"
#include "cscript.h"
#include "cobject.h"


CSI_DEC(const char *const csO_typenames[CSI_TOTALTYPES]);

#define typename(t)     csO_typenames[(t) + 1]


CSI_FUNC void csMM_init(cs_State *C);
CSI_FUNC const TValue *csMM_get(cs_State *C, const TValue *v, cs_MM mm);
CSI_FUNC OClass *csMM_newclass(cs_State *C);
CSI_FUNC Instance *csMM_newinstance(cs_State *C, OClass *cls);
CSI_FUNC UserData *csMM_newuserdata(cs_State *C, size_t size, int nuv);
CSI_FUNC IMethod *csMM_newinsmethod(cs_State *C, Instance *receiver,
				    const TValue *method);
CSI_FUNC void csMM_callset(cs_State *C, const TValue *fn, const TValue *p1,
                           const TValue *p2, const TValue *p3);
CSI_FUNC void csMM_callgetres(cs_State *C, const TValue *fn, const TValue *p1,
                              const TValue *p2, SPtr res);
CSI_FUNC void csMM_callbinres(cs_State *C, const TValue *fn, const TValue *v1,
                              const TValue *v2, SPtr res);
CSI_FUNC void csMM_callunaryres(cs_State *C, const TValue *fn, const TValue *v);
CSI_FUNC int csMM_order(cs_State *C, const TValue *v1, const TValue *v2,
		  	cs_MM mm);
CSI_FUNC int csMM_orderI(cs_State *C, const TValue *v1, int v2, int flip,
                         int isflt, cs_MM mm);
CSI_FUNC void csMM_trybin(cs_State *C, const TValue *v1, const TValue *v2,
		    	  SPtr res, cs_MM mm);
CSI_FUNC void csMM_tryunary(cs_State *C, const TValue *v, cs_MM mm);
CSI_FUNC void csMM_tryconcat(cs_State *C);

#endif
