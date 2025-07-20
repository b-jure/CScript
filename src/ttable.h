/*
** ttable.h
** Hash Table
** See Copyright Notice in tokudae.h
*/

#ifndef ttable_h
#define ttable_h


#include "tobject.h"
#include "tobject.h"
#include "tbits.h"


/* node value */
#define nodeval(n)          (&(n)->i_val)

/* get offset to next node in the chain, 0 if there is no next node */
#define nodenext(n)         ((n)->s.next)

/* get table slot */
#define htnode(t,i)	    (&(t)->node[(i)])

/* one after last node */
#define htnodelast(t)       htnode(t, htsize(t))

/* get table size */
#define htsize(t)	    (twoto((t)->size))



TOKUI_FUNC Table *tokuH_newsz(toku_State *T, int size);
TOKUI_FUNC Table *tokuH_new(toku_State *T);
TOKUI_FUNC int tokuH_next(toku_State *T, Table *tab, SPtr key);
TOKUI_FUNC void tokuH_copykeys(toku_State *T, Table *stab, Table *dtab);
TOKUI_FUNC void tokuH_resize(toku_State *T, Table *t, t_uint newsize);
TOKUI_FUNC void tokuH_newkey(toku_State *T, Table *t, const TValue *key,
                             const TValue *val);
TOKUI_FUNC const TValue *tokuH_getshortstr(Table *t, OString *key);
TOKUI_FUNC const TValue *tokuH_getstr(Table *t, OString *key);
TOKUI_FUNC const TValue *tokuH_getint(Table *t, toku_Integer key);
TOKUI_FUNC const TValue *tokuH_get(Table *tab, const TValue *key);
TOKUI_FUNC void tokuH_finishset(toku_State *T, Table *t, const TValue *slot,
                                const TValue *key, const TValue *val);
TOKUI_FUNC void tokuH_set(toku_State *T, Table *tab, const TValue *key,
                          const TValue *val);
TOKUI_FUNC void tokuH_setstr(toku_State *T, Table *tab, OString *key,
                                                        const TValue *val);
TOKUI_FUNC void tokuH_setint(toku_State *T, Table *t, toku_Integer key,
                                                      const TValue *val);
TOKUI_FUNC void tokuH_free(toku_State *T, Table *t);
TOKUI_FUNC int tokuH_len(const Table *t);

#endif
