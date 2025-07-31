/*
** tdebug.h
** Debug and error reporting functiont
** See Copyright Notice in tokudae.h
*/

#ifndef tdebug_h
#define tdebug_h


#include "tobject.h"
#include "tstate.h"


#define relpc(pc, p)	cast_int((pc) - (p)->code - cast_int(SIZE_INSTR))


/* active Tokudae function (given call frame) */
#define cf_func(cf)     (clTval(s2v((cf)->func.p)))


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


#define tokuD_aritherror(C,v1,v2) \
        tokuD_opinterror(C, v1, v2, "perform arithmetic on")

#define tokuD_bitwerror(C,v1,v2) \
        tokuD_opinterror(C, v1, v2, "perform bitwise operation on")

TOKUI_FUNC int tokuD_getfuncline(const Proto *fn, int pc);
TOKUI_FUNC const char *tokuD_findlocal(toku_State *T, CallFrame *cf, int n,
                                       SPtr *pos);
TOKUI_FUNC const char *tokuD_addinfo(toku_State *T, const char *msg,
                                     OString *src, int line);
TOKUI_FUNC t_noret tokuD_runerror(toku_State *T, const char *fmt, ...);
TOKUI_FUNC t_noret tokuD_typeerror(toku_State *T, const TValue *o,
                                   const char *op);
TOKUI_FUNC t_noret tokuD_binoperror(toku_State *T, const TValue *v1,
                                    const TValue *v2, TM event);
TOKUI_FUNC t_noret tokuD_ordererror(toku_State *T, const TValue *v1,
                                    const TValue *v2);
TOKUI_FUNC t_noret tokuD_opinterror(toku_State *T, const TValue *v1,
                                    const TValue *v2, const char *msg);
TOKUI_FUNC t_noret tokuD_tointerror(toku_State *T, const TValue *v1,
                                    const TValue *v2);
TOKUI_FUNC t_noret tokuD_callerror(toku_State *T, const TValue *obj);
TOKUI_FUNC t_noret tokuD_concaterror(toku_State *T, const TValue *v1,
                                     const TValue *v2);
TOKUI_FUNC t_noret tokuD_listerror(toku_State *T, const TValue *o,
                                   const char *what, const char *msg);
TOKUI_FUNC t_noret tokuD_listfielderror(toku_State *T, const TValue *field);
TOKUI_FUNC t_noret tokuD_errormsg(toku_State *T);
TOKUI_FUNC void tokuD_hook(toku_State *T, int event, int line,
                                          int ftransfer, int ntransfer);
TOKUI_FUNC void tokuD_hookcall(toku_State *T, CallFrame *cf, int delta);
TOKUI_FUNC int tokuD_tracecall(toku_State *T, int delta);
TOKUI_FUNC int tokuD_traceexec(toku_State *T, const Instruction *pc,
                                              ptrdiff_t stacksize);

#endif
