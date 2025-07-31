/*
** tmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in tokudae.h
*/

#ifndef tmeta_h
#define tmeta_h

#include "tokudaeconf.h"
#include "tokudae.h"
#include "tobject.h"


/*
** Mask with 1 in all fast-access methods. A 1 in any of these bits
** in the flag of a (meta)table means the metatable does not have the
** corresponding metamethod field. (Bit 7 of the flag indicates that
** the table is using the dummy node.)
*/
#define maskflags   cast_ubyte(~(~0u << (TM_INIT + 1)))


#define notm(tm)    ttisnil(tm)


#define checknoTM(mt,e)	    ((mt) == NULL || (mt)->flags & (1u<<(e)))

#define gfasttm(g,mt,e) \
        (checknoTM(mt, e) ? NULL : tokuTM_get(mt, e, (g)->tmnames[e]))

#define fasttm(T,mt,e)	    gfasttm(G(T), mt, e)


#define typename(t)     tokuO_typenames[(t) + 1]

TOKUI_DEC(const char *const tokuO_typenames[TOKUI_TOTALTYPES]);


#define eventstring(T,tm)   (G(T)->tmnames[tm])

#define eventname(T,tm)     getstr(eventstring(T, tm))


/*
** Tag method events.
** WARNING: If you change the order of this enumeration,
** grep "ORDER TM" and "ORDER OP".
*/
typedef enum {
    TM_GETIDX,
    TM_SETIDX,
    TM_GC,
    TM_CALL,
    TM_EQ,
    TM_NAME,
    TM_INIT, /* last tag method with fast access */
    TM_ADD,
    TM_SUB,
    TM_MUL,
    TM_DIV,
    TM_IDIV,
    TM_MOD,
    TM_POW,
    TM_BSHL,
    TM_BSHR,
    TM_BAND,
    TM_BOR,
    TM_BXOR,
    TM_CONCAT,
    TM_UNM,
    TM_BNOT,
    TM_LT,
    TM_LE,
    TM_CLOSE,
    TM_NUM, /* number of events */
} TM;


TOKUI_FUNC void tokuTM_init(toku_State *T);
TOKUI_FUNC const TValue *tokuTM_objget(toku_State *T, const TValue *o,
                                                      TM event);
TOKUI_FUNC const TValue *tokuTM_get(Table *events, TM event, OString *ename);
TOKUI_FUNC OClass *tokuTM_newclass(toku_State *T);
TOKUI_FUNC Instance *tokuTM_newinstance(toku_State *T, OClass *cls);
TOKUI_FUNC UserData *tokuTM_newuserdata(toku_State *T, size_t size, t_ushort nuv);
TOKUI_FUNC IMethod *tokuTM_newinsmethod(toku_State *T, Instance *receiver,
				        const TValue *method);
TOKUI_FUNC int tokuTM_eqim(const IMethod *v1, const IMethod *v2);
TOKUI_FUNC UMethod *tokuTM_newudmethod(toku_State *T, UserData *ud,
                                       const TValue *method);
TOKUI_FUNC int tokuTM_equm(const UMethod *v1, const UMethod *v2);
TOKUI_FUNC const char *tokuTM_objtypename(toku_State *T, const TValue *o);
TOKUI_FUNC void tokuTM_callset(toku_State *T, const TValue *fn,
                               const TValue *o, const TValue *k,
                               const TValue *v);
TOKUI_FUNC void tokuTM_callgetres(toku_State *T, const TValue *fn,
                                  const TValue *o, const TValue *k,
                                  SPtr res);
TOKUI_FUNC void tokuTM_callbinres(toku_State *T, const TValue *fn,
                                  const TValue *v1, const TValue *v2,
                                  SPtr res);
TOKUI_FUNC void tokuTM_callunaryres(toku_State *T, const TValue *fn,
                                    const TValue *o, SPtr res);
TOKUI_FUNC int tokuTM_order(toku_State *T, const TValue *v1, const TValue *v2,
		  	    TM event);
TOKUI_FUNC void tokuTM_trybin(toku_State *T, const TValue *v1,
                              const TValue *v2, SPtr res, TM event);
TOKUI_FUNC void tokuTM_tryunary(toku_State *T, const TValue *o,
                                SPtr res, TM event);
TOKUI_FUNC void tokuTM_tryconcat(toku_State *T);

#endif
