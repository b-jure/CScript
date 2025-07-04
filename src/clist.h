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


/* value of 'extra' for first list field name */
#define FIRSTLF     (NUM_KEYWORDS + CS_MT_NUM + 1)


/* indices into 'C->gs->listfields' */
#define LFLEN       0
#define LFLAST      1
#define LFX         2
#define LFY         3
#define LFZ         4
#define LFNUM       5


/* test whether a string is a valid list field */
#define islistfield(s) \
        ((s)->tt_ == CS_VSHRSTR && FIRSTLF <= (s)->extra && \
         (s)->extra < FIRSTLF + LFNUM)


/*
** For passing both the stack value and integer to 'csA_set(get)int',
** without breaking generic macros in 'cvm.c'.
*/
typedef struct FatValue {
    const TValue *v;
    cs_Integer i;
} FatValue;


CSI_FUNC List *csA_new(cs_State *C);
CSI_FUNC List *csA_newl(cs_State *C, int n);
CSI_FUNC void csA_init(cs_State *C);
CSI_FUNC void csA_shrink(cs_State *C, List *l);
CSI_FUNC int csA_ensure(cs_State *C, List *l, int n);
CSI_FUNC void csA_ensureindex(cs_State *C, List *l, int index);
CSI_FUNC void csA_set(cs_State *C, List *l, const TValue *i, const TValue *v);
CSI_FUNC void csA_setstr(cs_State *C, List *l, const TValue *k, const TValue *v);
CSI_FUNC void csA_setint(cs_State *C, List *l, const FatValue *k, const TValue *v);
CSI_FUNC void csA_get(cs_State *C, List *l, const TValue *k, TValue *out);
CSI_FUNC void csA_getstr(cs_State *C, List *l, const TValue *k, TValue *out);
CSI_FUNC void csA_getint(cs_State *C, List *l, const FatValue *k, TValue *out);
CSI_FUNC const TValue *csA_getival(cs_State *C, List *l, int i);
CSI_FUNC void csA_geti(cs_State *C, List *l, int i, TValue *res);
CSI_FUNC int csA_findindex(List *l, int rev, int nn, int s, int e);
CSI_FUNC void csA_free(cs_State *C, List *l);

#endif
