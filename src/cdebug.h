/*
** cdebug.h
** Debug and error reporting functions
** See Copyright Notice in cscript.h
*/

#ifndef cdebug_h
#define cdebug_h


#include "cobject.h"
#include "cstate.h"


#define csD_aritherror(C,v1,v2) \
        csD_operror(C, v1, v2, "perform arithmetic operation on")

#define csD_bitwerror(C,v1,v2) \
        csD_operror(C, v1, v2, "perform bitwise operation on")

#define csD_tointerror(C) \
        csD_runerror(C, "number has no integer representation")


/*
** Mark for entries in 'lineinfo' array that has absolute information in
** 'abslineinfo' array.
*/
#define ABSLINEINFO     (-0x80)


/*
** Mark for entries in 'lineinfo' array that containt instruction
** arguments.
*/
#define ARGLINEINFO     (ABSLINEINFO + 1)


/*
** MAXimum number of successive Instructions WiTHout ABSolute line
** information. (A power of two allows fast divisions.)
*/
#if !defined(MAXIWTHABS)
#define MAXIWTHABS     128
#endif


/* active CScript function (given call frame) */
#define cf_func(cf)     (clCSval(s2v((cf)->func.p)))


CSI_FUNC int csD_getfuncline(const Proto *fn, int pc);
CSI_FUNC const char *csD_findlocal(cs_State *C, CallFrame *cf, int n,
                                   SPtr *pos);
CSI_FUNC const char *csD_addinfo(cs_State *C, const char *msg, OString *src,
                                 int line);
CSI_FUNC c_noret csD_runerror(cs_State *C, const char *fmt, ...);
CSI_FUNC c_noret csD_globalerror(cs_State *C, const char *err, OString *name);
CSI_FUNC c_noret csD_typeerror(cs_State *C, const TValue *v,
                                const char *op);
CSI_FUNC c_noret csD_ordererror(cs_State *C, const TValue *v1,
                                 const TValue *v2);
CSI_FUNC c_noret csD_typeerrormeta(cs_State *C, const TValue *v1,
                                    const TValue *v2, const char * mop);
CSI_FUNC c_noret csD_operror(cs_State *C, const TValue *v1,
                              const TValue *v2, const char *op);
CSI_FUNC c_noret csD_callerror(cs_State *C, const TValue *obj);
CSI_FUNC c_noret csD_concaterror(cs_State *C, const TValue *v1,
                                  const TValue *v2);
CSI_FUNC c_noret csD_indexerror(cs_State *C, cs_Integer index,
                                 const char *what);
CSI_FUNC c_noret csD_indextypeerror(cs_State *C, const TValue *index);
CSI_FUNC c_noret csD_errormsg(cs_State *C);
CSI_FUNC int csD_traceexec(cs_State *C, const Instruction *pc);

#endif
