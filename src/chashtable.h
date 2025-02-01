/*
** chashtable.h
** Hash Table
** See Copyright Notice in cscript.h
*/

#ifndef CHASHTABLE_H
#define CHASHTABLE_H


#include "cobject.h"
#include "cobject.h"
#include "cbits.h"


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



CSI_FUNC HTable *csH_newsz(cs_State *C, int size);
CSI_FUNC HTable *csH_new(cs_State *C);
CSI_FUNC int csH_next(cs_State *C, HTable *tab, SPtr key);
CSI_FUNC void csH_copykeys(cs_State *C, HTable *stab, HTable *dtab);
CSI_FUNC void csH_resize(cs_State *C, HTable *ht, uint newsize);
CSI_FUNC void csH_newkey(cs_State *C, HTable *ht, const TValue *key,
                         const TValue *val);
CSI_FUNC const TValue *csH_getshortstr(HTable *ht, OString *key);
CSI_FUNC const TValue *csH_getstr(HTable *ht, OString *key);
CSI_FUNC const TValue *csH_getint(HTable *ht, cs_Integer key);
CSI_FUNC const TValue *csH_get(HTable *tab, const TValue *key);
CSI_FUNC void csH_finishset(cs_State *C, HTable *ht, const TValue *slot,
                            const TValue *key, const TValue *val);
CSI_FUNC void csH_set(cs_State *C, HTable *tab, const TValue *key,
                      const TValue *val);
CSI_FUNC void csH_free(cs_State *C, HTable *ht);
CSI_FUNC int csH_len(const HTable *ht);

#endif
