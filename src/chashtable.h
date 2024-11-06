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


#define keyisempty(n)	    (keytt(n) == CS_VEMPTY)

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


CSI_FUNC HTable *crH_new(cs_State *ts);
CSI_FUNC HTable *crH_newsize(cs_State *ts, uint size);
CSI_FUNC int crH_next(cs_State *ts, HTable *tab, SPtr key);
CSI_FUNC void crH_copykeys(cs_State *ts, HTable *stab, HTable *dtab);
CSI_FUNC int crH_intern(cs_State *ts, const char *string);
CSI_FUNC void crH_newkey(cs_State *ts, HTable *ht, const TValue *key,
                         const TValue *val);
CSI_FUNC const TValue *crH_getstr(HTable *ht, OString *key);
CSI_FUNC const TValue *crH_getint(HTable *ht, cs_Integer key);
CSI_FUNC const TValue *crH_get(HTable *tab, const TValue *key);
CSI_FUNC void crH_finishset(cs_State *ts, HTable *ht, const TValue *slot,
                            const TValue *key, const TValue *val);
CSI_FUNC void crH_set(cs_State *ts, HTable *tab, const TValue *key,
                      const TValue *val);
CSI_FUNC void crH_free(cs_State *ts, HTable *ht);
CSI_FUNC int crH_len(const HTable *ht);
CSI_FUNC OString *crH_getinterned(cs_State *ts, HTable *tab, const char *str,
                                  size_t len, uint hash);

#endif
