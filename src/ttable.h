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
#define invalidateTMcache(t)    ((t)->flags &= cast_ubyte(~maskflags))


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
#define NOTBITDUMMY	    cast_ubyte(~BITDUMMY)
#define isdummy(t)	    ((t)->flags & BITDUMMY)

#define setnodummy(t)	    ((t)->flags &= NOTBITDUMMY)
#define setdummy(t)	    ((t)->flags |= BITDUMMY)


/* allocated size for hash nodes */
#define allocsizenode(t)    (isdummy(t) ? 0 : htsize(t))


/* results from pset */
#define HOK		0
#define HNOTFOUND	1
#define HFIRSTNODE	2


/*
** 'tokuH_get*' operations set 'res', unless the value is absent, and
** return the tag of the result.
** The 'tokuH_pset*' (pre-set) operations set the given value and return
** HOK, unless the original value is absent. In that case, if the key
** is really absent, they return HNOTFOUND. Otherwise, if there is a
** slot with that key but with no value, 'tokuH_pset*' return an encoding
** of where the key is (usually called 'hres').
** The encoding is (HFIRSTNODE + hash index);
** The size of the hash is limited by the maximum power of two that fits
** in a signed integer; that is (INT_MAX+1)/2. So, it is safe to add
** HFIRSTNODE to any index there.)
*/


TOKUI_FUNC t_ubyte tokuH_get(Table *t, const TValue *key, TValue *res);
TOKUI_FUNC t_ubyte tokuH_getshortstr(Table *t, OString *key, TValue *res);
TOKUI_FUNC t_ubyte tokuH_getstr(Table *t, OString *key, TValue *res);
TOKUI_FUNC t_ubyte tokuH_getint(Table *t, toku_Integer key, TValue *res);

/* special get for metamethods */
TOKUI_FUNC const TValue *tokuH_Hgetshortstr(Table *t, OString *key);

TOKUI_FUNC int tokuH_psetint(Table *t, toku_Integer key, const TValue *val);
TOKUI_FUNC int tokuH_psetshortstr(Table *t, OString *key, const TValue *val);
TOKUI_FUNC int tokuH_psetstr(Table *t, OString *key, const TValue *val);
TOKUI_FUNC int tokuH_pset(Table *t, const TValue *key, const TValue *val);

TOKUI_FUNC void tokuH_setstr(toku_State *T, Table *t, OString *key,
                                                      const TValue *value);
TOKUI_FUNC void tokuH_setint(toku_State *T, Table *t, toku_Integer key,
                                                      const TValue *value);
TOKUI_FUNC void tokuH_set(toku_State *T, Table *t, const TValue *key,
                                                   const TValue *value);

TOKUI_FUNC void tokuH_finishset(toku_State *T, Table *t, const TValue *key,
                                const TValue *value, int hres);

TOKUI_FUNC Table *tokuH_new(toku_State *T);
TOKUI_FUNC void tokuH_resize(toku_State *T, Table *t, t_uint newsize);
TOKUI_FUNC void tokuH_copy(toku_State *T, Table *dest, Table *src);
TOKUI_FUNC void tokuH_free(toku_State *T, Table *t);
TOKUI_FUNC int tokuH_len(Table *t);
TOKUI_FUNC int tokuH_next(toku_State *T, Table *t, SPtr key);

#endif
