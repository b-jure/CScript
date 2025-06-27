/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in cscript.h
*/

#ifndef cmeta_h
#define cmeta_h

#include "cscriptconf.h"
#include "cscript.h"
#include "cobject.h"


CSI_DEC(const char *const csO_typenames[CSI_TOTALTYPES]);

#define typename(t)     csO_typenames[(t) + 1]


#define metaname(C,mm)      (getstr(G(C)->mmnames[mm]) + 2)


CSI_FUNC void csMM_init(cs_State *C);
CSI_FUNC const TValue *csMM_get(cs_State *C, const TValue *v, int mm);
CSI_FUNC OClass *csMM_newclass(cs_State *C);
CSI_FUNC Instance *csMM_newinstance(cs_State *C, OClass *cls);
CSI_FUNC UserData *csMM_newuserdata(cs_State *C, size_t size, int nuv);
CSI_FUNC IMethod *csMM_newinsmethod(cs_State *C,
                                    Instance *receiver,
				    const TValue *method);
CSI_FUNC int csMM_eqimethod(const IMethod *v1, const IMethod *v2);
CSI_FUNC UMethod *csMM_newudmethod(cs_State *C,
                                   UserData *ud,
                                   const TValue *method);
CSI_FUNC int csMM_equmethod(const UMethod *v1, const UMethod *v2);
CSI_FUNC const char *csMM_objtypename(cs_State *C, const TValue *o);
CSI_FUNC void csMM_callset(cs_State *C,
                           const TValue *fn,
                           const TValue *o,
                           const TValue *k,
                           const TValue *v);
CSI_FUNC void csMM_callgetres(cs_State *C,
                              const TValue *fn,
                              const TValue *o,
                              const TValue *k,
                              SPtr res);
CSI_FUNC void csMM_callbinres(cs_State *C,
                              const TValue *fn,
                              const TValue *v1,
                              const TValue *v2,
                              SPtr res);
CSI_FUNC void csMM_callunaryres(cs_State *C,
                                const TValue *fn,
                                const TValue *o,
                                SPtr res);
CSI_FUNC int csMM_order(cs_State *C,
                        const TValue *v1,
                        const TValue *v2,
		  	int mm);
CSI_FUNC int csMM_orderI(cs_State *C,
                         const TValue *v1,
                         int v2, int flip,
                         int isflt, int mm);
CSI_FUNC void csMM_trybin(cs_State *C,
                          const TValue *v1,
                          const TValue *v2,
		    	  SPtr res, int mm);
CSI_FUNC void csMM_tryunary(cs_State *C, const TValue *o, SPtr res, int mm);
CSI_FUNC void csMM_tryconcat(cs_State *C);

#endif
