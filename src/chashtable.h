/*
** chashtable.h
** Hash Table
** See Copyright Notice in cscript.h
*/

#ifndef SKHASHTABLE_H
#define SKHASHTABLE_H


#include "cobject.h"
#include "cobject.h"
#include "cbits.h"


#define keyisempty(n)	    (keytt(n) == CR_VEMPTY)

/* node value */
#define nodeval(n)          (&(n)->i_val)

/* get offset to next node in the chain, 0 if there is no next node */
#define nodenext(n)         ((n)->s.next)

/* get table slot */
#define htnode(ht,i)	    (&(ht)->node[(i)])

/* one after last node */
#define htnodelast(ht)      htnode(ht, htsize(ht))

/* get table size */
#define htsize(ht)	    (twoto((ht)->size))


CRI_FUNC HTable *crH_new(cr_State *ts);
CRI_FUNC HTable *crH_newsize(cr_State *ts, uint size);
CRI_FUNC int crH_next(cr_State *ts, HTable *tab, SPtr key);
CRI_FUNC void crH_copykeys(cr_State *ts, HTable *stab, HTable *dtab);
CRI_FUNC int crH_intern(cr_State *ts, const char *string);
CRI_FUNC void crH_newkey(cr_State *ts, HTable *ht, const TValue *key,
                         const TValue *val);
CRI_FUNC const TValue *crH_getstr(HTable *ht, OString *key);
CRI_FUNC const TValue *crH_getint(HTable *ht, cr_Integer key);
CRI_FUNC const TValue *crH_get(HTable *tab, const TValue *key);
CRI_FUNC void crH_finishset(cr_State *ts, HTable *ht, const TValue *slot,
                            const TValue *key, const TValue *val);
CRI_FUNC void crH_set(cr_State *ts, HTable *tab, const TValue *key,
                      const TValue *val);
CRI_FUNC void crH_free(cr_State *ts, HTable *ht);
CRI_FUNC int crH_len(const HTable *ht);
CRI_FUNC OString *crH_getinterned(cr_State *ts, HTable *tab, const char *str,
                                  size_t len, uint hash);

#endif
