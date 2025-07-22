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

/*
** Clear all bits of fast-access metamethods, which means that the table
** may have any of these metamethods. (First access that fails after the
** clearing will set the bit again.)
*/
#define invalidateTMcache(t)    ((t)->flags &= ~maskflags)


#define nodeval(n)          (&(n)->i_val)
#define nodenext(n)         ((n)->s.next)
#define htnode(t,i)	    (&(t)->node[(i)])
#define htnodelast(t)       htnode(t, htsize(t))

#define htsize(t)	    (twoto((t)->size))


/*
** Bit BITDUMMY set in 'flags' means the table is using the dummy node
** for its hash.
*/

#define BITDUMMY	    (1 << 7)
#define NOTBITDUMMY	    cast_byte(~BITDUMMY)
#define isdummy(t)	    ((t)->flags & BITDUMMY)

#define setnodummy(t)	    ((t)->flags &= NOTBITDUMMY)
#define setdummy(t)	    ((t)->flags |= BITDUMMY)


/* allocated size for hash nodes */
#define allocsizenode(t)    (isdummy(t) ? 0 : htsize(t))


TOKUI_FUNC Table *tokuH_new(toku_State *T);
TOKUI_FUNC int tokuH_next(toku_State *T, Table *t, SPtr key);
TOKUI_FUNC void tokuH_copy(toku_State *T, Table *stab, Table *dtab);
TOKUI_FUNC void tokuH_resize(toku_State *T, Table *t, t_uint newsize);
TOKUI_FUNC void tokuH_newkey(toku_State *T, Table *t, const TValue *key,
                                                      const TValue *value);
TOKUI_FUNC const TValue *tokuH_getshortstr(Table *t, OString *key);
TOKUI_FUNC t_ubyte tokuH_getstr(Table *t, OString *key, TValue *res);
TOKUI_FUNC t_ubyte tokuH_getint(Table *t, toku_Integer key, TValue *res);
TOKUI_FUNC t_ubyte tokuH_get(Table *t, const TValue *key, TValue *res);
TOKUI_FUNC void tokuH_finishset(toku_State *T, Table *t, const TValue *slot,
                                const TValue *key, const TValue *value);
TOKUI_FUNC void tokuH_set(toku_State *T, Table *t, const TValue *key,
                                                   const TValue *value);
TOKUI_FUNC void tokuH_setstr(toku_State *T, Table *t, OString *key,
                                                      const TValue *value);
TOKUI_FUNC void tokuH_setint(toku_State *T, Table *t, toku_Integer key,
                                                      const TValue *value);
TOKUI_FUNC void tokuH_free(toku_State *T, Table *t);
TOKUI_FUNC int tokuH_len(const Table *t);

#endif
