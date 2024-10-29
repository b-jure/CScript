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


CRI_FUNC int crD_getfuncline(const Function *fn, int pc);
CRI_FUNC const char *crD_findlocal(cr_State *ts, CallFrame *cf, int n,
                                   SPtr *pos);
CRI_FUNC const char *crD_info(cr_State *ts, const char *msg,
                              const OString *src, int line);
CRI_FUNC cr_noret crD_runerror(cr_State *ts, const char *fmt, ...);
CRI_FUNC cr_noret crD_globalerror(cr_State *ts, const char *err, OString *name);
CRI_FUNC cr_noret crD_typeerror(cr_State *ts, const TValue *v,
                                const char *op);
CRI_FUNC cr_noret crD_ordererror(cr_State *ts, const TValue *v1,
                                 const TValue *v2);
CRI_FUNC cr_noret crD_typeerrormeta(cr_State *ts, const TValue *v1,
                                    const TValue *v2, const char * mop);
CRI_FUNC cr_noret crD_operror(cr_State *ts, const TValue *v1,
                              const TValue *v2, const char *op);
CRI_FUNC cr_noret crD_callerror(cr_State *ts, const TValue *obj);
CRI_FUNC cr_noret crD_concaterror(cr_State *ts, const TValue *v1,
                                  const TValue *v2);

#endif
