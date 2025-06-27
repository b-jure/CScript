/*
** cdebug.h
** Debug and error reporting functions
** See Copyright Notice in cscript.h
*/

#ifndef cdebug_h
#define cdebug_h


#include "cobject.h"
#include "cstate.h"


#define relpc(pc, p)	(cast_int((pc) - (p)->code) - SIZE_INSTR)


/* active CScript function (given call frame) */
#define cf_func(cf)     (clCSval(s2v((cf)->func.p)))


#define resethookcount(C)       (C->hookcount = C->basehookcount)


/*
** Mark for entries in 'lineinfo' array that has absolute information in
** 'abslineinfo' array, or for instruction arguments.
*/
#define ABSLINEINFO     (-0x80)


/*
** MAXimum number of successive Instructions WiTHout ABSolute line
** information. (A power of two allows fast divisions.)
*/
#if !defined(MAXIWTHABS)
#define MAXIWTHABS     128
#endif


#define csD_aritherror(C,v1,v2) \
        csD_opinterror(C, v1, v2, "perform arithmetic on")

#define csD_bitwerror(C,v1,v2) \
        csD_opinterror(C, v1, v2, "perform bitwise operation on")

CSI_FUNC int csD_getfuncline(const Proto *fn, int pc);
CSI_FUNC const char *csD_findlocal(cs_State *C,
                                   CallFrame *cf,
                                   int n, SPtr *pos);
CSI_FUNC const char *csD_addinfo(cs_State *C,
                                 const char *msg,
                                 OString *src,
                                 int line);
CSI_FUNC c_noret csD_runerror(cs_State *C, const char *fmt, ...);
CSI_FUNC c_noret csD_typeerror(cs_State *C, const TValue *o, const char *op);
CSI_FUNC c_noret csD_binoperror(cs_State *C,
                                const TValue *v1,
                                const TValue *v2,
                                int mm);
CSI_FUNC c_noret csD_ordererror(cs_State *C,
                                const TValue *v1,
                                const TValue *v2);
CSI_FUNC c_noret csD_opinterror(cs_State *C,
                                const TValue *v1,
                                const TValue *v2,
                                const char *msg);
CSI_FUNC c_noret csD_tointerror(cs_State *C,
                                const TValue *v1,
                                const TValue *v2);
CSI_FUNC c_noret csD_callerror(cs_State *C, const TValue *obj);
CSI_FUNC c_noret csD_concaterror(cs_State *C,
                                 const TValue *v1,
                                 const TValue *v2);
CSI_FUNC c_noret csD_listerror(cs_State *C,
                               const TValue *o,
                               const char *what,
                               const char *msg);
CSI_FUNC c_noret csD_errormsg(cs_State *C);
CSI_FUNC void csD_hook(cs_State *C,
                       int event, int line,
                       int ftransfer, int ntransfer);
CSI_FUNC void csD_hookcall(cs_State *C, CallFrame *cf, int delta);
CSI_FUNC int csD_tracecall(cs_State *C, int delta);
CSI_FUNC int csD_traceexec(cs_State *C, const Instruction *pc, ptrdiff_t stks);

#endif
