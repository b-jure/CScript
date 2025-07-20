/*
** tmeta.h
** Functiont for metamethods and meta types
** See Copyright Notice in tokudae.h
*/

#ifndef tmeta_h
#define tmeta_h

#include "tokudaeconf.h"
#include "tokudae.h"
#include "tobject.h"


TOKUI_DEC(const char *const tokuO_typenames[TOKUI_TOTALTYPES]);

#define typename(t)     tokuO_typenames[(t) + 1]


#define metaname(T,mm)      (getstr(G(T)->mtnames[mm]) + 2)


TOKUI_FUNC void tokuMM_init(toku_State *T);
TOKUI_FUNC const TValue *tokuMM_get(toku_State *T, const TValue *v, int mm);
TOKUI_FUNC OClass *tokuMM_newclass(toku_State *T);
TOKUI_FUNC Instance *tokuMM_newinstance(toku_State *T, OClass *cls);
TOKUI_FUNC UserData *tokuMM_newuserdata(toku_State *T, size_t size, int nuv);
TOKUI_FUNC IMethod *tokuMM_newinsmethod(toku_State *T,
                                        Instance *receiver,
				        const TValue *method);
TOKUI_FUNC int tokuMM_eqim(const IMethod *v1, const IMethod *v2);
TOKUI_FUNC UMethod *tokuMM_newudmethod(toku_State *T,
                                       UserData *ud,
                                       const TValue *method);
TOKUI_FUNC int tokuMM_equm(const UMethod *v1, const UMethod *v2);
TOKUI_FUNC const char *tokuMM_objtypename(toku_State *T, const TValue *o);
TOKUI_FUNC void tokuMM_callset(toku_State *T,
                               const TValue *fn,
                               const TValue *o,
                               const TValue *k,
                               const TValue *v);
TOKUI_FUNC void tokuMM_callgetres(toku_State *T,
                                  const TValue *fn,
                                  const TValue *o,
                                  const TValue *k,
                                  SPtr ret);
TOKUI_FUNC void tokuMM_callbinres(toku_State *T,
                                  const TValue *fn,
                                  const TValue *v1,
                                  const TValue *v2,
                                  SPtr ret);
TOKUI_FUNC void tokuMM_callunaryres(toku_State *T,
                                    const TValue *fn,
                                    const TValue *o,
                                    SPtr ret);
TOKUI_FUNC int tokuMM_order(toku_State *T,
                            const TValue *v1,
                            const TValue *v2,
		  	    int mm);
TOKUI_FUNC int tokuMM_orderI(toku_State *T,
                             const TValue *v1,
                             int v2, int flip,
                             int itflt, int mm);
TOKUI_FUNC void tokuMM_trybin(toku_State *T,
                              const TValue *v1,
                              const TValue *v2,
		    	      SPtr ret, int mm);
TOKUI_FUNC void tokuMM_tryunary(toku_State *T, const TValue *o,
                                               SPtr res, int mm);
TOKUI_FUNC void tokuMM_tryconcat(toku_State *T);

#endif
