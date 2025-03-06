/*
** cvm.h
** CScript Virtual Machine
** See Copyright Notice in cscript.h
*/

#ifndef CVM_H
#define CVM_H

#include "cobject.h"
#include "cstate.h"


/* generic loop private variable offsets */
#define FORITERATOR     0  /* iterator offset */
#define FORINVSTATE     1  /* invariant state offset */
#define FORCNTLVAR      2  /* control variable offset */
#define FORTBCVAR       3  /* to-be-closed variable offset */
#define NSTATEVARS      4


#define csV_raweq(v1_,v2_)    csV_ordereq(NULL, v1_, v2_)


CSI_FUNC void csV_call(cs_State *C, SPtr fn, int nreturns);
CSI_FUNC void csV_concat(cs_State *C, int n);
CSI_FUNC cs_Integer csV_divi(cs_State *C, cs_Integer x, cs_Integer y);
CSI_FUNC cs_Integer csV_modi(cs_State *C, cs_Integer x, cs_Integer y);
CSI_FUNC cs_Number csV_modf(cs_State *C, cs_Number x, cs_Number y);
CSI_FUNC void csV_binarithm(cs_State *C, const TValue *a, const TValue *b,
                            SPtr res, int op);
CSI_FUNC void csV_unarithm(cs_State *C, const TValue *v, int op);
CSI_FUNC int csV_ordereq(cs_State *C, const TValue *v1, const TValue *v2);
CSI_FUNC int csV_orderlt(cs_State *C, const TValue *v1, const TValue *v2);
CSI_FUNC int csV_orderle(cs_State *C, const TValue *v1, const TValue *v2);
CSI_FUNC void csV_execute(cs_State *C, CallFrame *cf);
CSI_FUNC void csV_rawset(cs_State *C, const TValue *obj, const TValue *key,
                         const TValue *val);
CSI_FUNC void csV_set(cs_State *C, const TValue *obj, const TValue *key,
                      const TValue *val);
CSI_FUNC void csV_rawget(cs_State *C, const TValue *obj, const TValue *key,
                         SPtr res);
CSI_FUNC void csV_get(cs_State *C, const TValue *obj, const TValue *key,
                      SPtr res);

#endif
