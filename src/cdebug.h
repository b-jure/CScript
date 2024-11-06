/*
** cdebug.h
** Debug and error reporting functions
** See Copyright Notice in cscript.h
*/

#ifndef CRDEBUG_H
#define CRDEBUG_H


#include "cobject.h"
#include "cstate.h"


#define crD_aritherror(ts,v1,v2) \
    crD_operror(ts, v1, v2, "perform arithmetic operation on")

#define crD_bitwerror(ts,v1,v2) \
    crD_operror(ts, v1, v2, "perform bitwise operation on")


#define pcrel(pc,fn)    ((pc) - (fn)->code)


CSI_FUNC int crD_getfuncline(const Function *fn, int pc);
CSI_FUNC const char *crD_findlocal(cs_State *ts, CallFrame *cf, int n,
                                   SPtr *pos);
CSI_FUNC const char *crD_addinfo(cs_State *ts, const char *msg, OString *src,
                                 int line);
CSI_FUNC cs_noret crD_runerror(cs_State *ts, const char *fmt, ...);
CSI_FUNC cs_noret crD_globalerror(cs_State *ts, const char *err, OString *name);
CSI_FUNC cs_noret crD_typeerror(cs_State *ts, const TValue *v,
                                const char *op);
CSI_FUNC cs_noret crD_ordererror(cs_State *ts, const TValue *v1,
                                 const TValue *v2);
CSI_FUNC cs_noret crD_typeerrormeta(cs_State *ts, const TValue *v1,
                                    const TValue *v2, const char * mop);
CSI_FUNC cs_noret crD_operror(cs_State *ts, const TValue *v1,
                              const TValue *v2, const char *op);
CSI_FUNC cs_noret crD_callerror(cs_State *ts, const TValue *obj);
CSI_FUNC cs_noret crD_concaterror(cs_State *ts, const TValue *v1,
                                  const TValue *v2);
CSI_FUNC cs_noret crD_indexerror(cs_State *ts, cs_Integer index,
                                 const char *what);
CSI_FUNC cs_noret crD_indextypeerror(cs_State *ts, const TValue *index);
CSI_FUNC cs_noret crD_errormsg(cs_State *ts);

#endif
