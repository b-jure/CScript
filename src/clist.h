/*
** clist.h
** List manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef clist_h
#define clist_h

#include "cobject.h"


#define csA_fastset(C,l,i,v) \
    { setobj(C, &(l)->b[(i)], v); csG_barrierback(C, obj2gco(l), (v)); }

#define csA_fastget(C,l,i,o)      setobj(C, o, &(l)->b[(i)])


/* indices into 'C->gs->listfields' */
#define LFLEN       0  /* "len" */
#define LFLAST      1  /* "last" */
#define LFX         2  /* "x" */
#define LFY         3  /* "y" */
#define LFZ         4  /* "z" */
#define LFNUM       5  /* number of fields */


CSI_FUNC List *csA_new(cs_State *C);
CSI_FUNC List *csA_newl(cs_State *C, int n);
CSI_FUNC void csA_init(cs_State *C);
CSI_FUNC void csA_shrink(cs_State *C, List *l);
CSI_FUNC int csA_ensure(cs_State *C, List *l, int n);
CSI_FUNC void csA_ensureindex(cs_State *C, List *l, int index);
CSI_FUNC void csA_set(cs_State *C, List *l, const TValue *val,
                                            const TValue *index);
CSI_FUNC void csA_get(cs_State *C, List *l, const TValue *index, TValue *out);
CSI_FUNC const TValue *csA_getival(cs_State *C, List *l, int i);
CSI_FUNC void csA_geti(cs_State *C, List *l, int i, TValue *res);
CSI_FUNC int csA_findindex(List *l, int rev, int nn, int s, int e);
CSI_FUNC void csA_free(cs_State *C, List *l);

#endif
