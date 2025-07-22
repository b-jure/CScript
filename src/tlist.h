/*
** tlist.h
** List manipulation functions
** See Copyright Notice in tokudae.h
*/

#ifndef tlist_h
#define tlist_h

#include "tobject.h"


#define tokuA_fastset(C,l,i,v) \
    { setobj(C, &(l)->arr[(i)], v); tokuG_barrierback(C, obj2gco(l), (v)); }


#define tokuA_fastget(C,l,i,o)      setobj(C, o, &(l)->arr[(i)])


/* value of 'extra' for first list field name */
#define FIRSTLF     (NUM_KEYWORDS + TM_NUM + 1)


/* indices into 'C->gs->listfields' */
#define LFLEN       0
#define LFLAST      1
#define LFX         2
#define LFY         3
#define LFZ         4
#define LFNUM       5


/* test whether a string is a valid list field */
#define islistfield(s) \
        ((s)->tt_ == TOKU_VSHRSTR && FIRSTLF <= (s)->extra && \
         (s)->extra < FIRSTLF + LFNUM)


/*
** For parsing both the stack value and integer to 'tokuA_set(get)int',
** without breaking generic macros in 'tvm.c'.
*/
typedef struct FatValue {
    const TValue *v;
    toku_Integer i;
} FatValue;


TOKUI_FUNC List *tokuA_new(toku_State *T);
TOKUI_FUNC List *tokuA_newl(toku_State *T, int n);
TOKUI_FUNC void tokuA_init(toku_State *T);
TOKUI_FUNC void tokuA_shrink(toku_State *T, List *l);
TOKUI_FUNC int tokuA_ensure(toku_State *T, List *l, int n);
TOKUI_FUNC void tokuA_ensureindex(toku_State *T, List *l, int index);
TOKUI_FUNC void tokuA_set(toku_State *T, List *l, const TValue *i,
                                                  const TValue *v);
TOKUI_FUNC void tokuA_setstr(toku_State *T, List *l, const TValue *k,
                                                     const TValue *v);
TOKUI_FUNC void tokuA_setint(toku_State *T, List *l, const FatValue *k,
                                                     const TValue *v);
TOKUI_FUNC void tokuA_get(toku_State *T, List *l, const TValue *k,
                                                  TValue *out);
TOKUI_FUNC void tokuA_getstr(toku_State *T, List *l, const TValue *k,
                                                     TValue *out);
TOKUI_FUNC void tokuA_getint(toku_State *T, List *l, const FatValue *k,
                                                     TValue *out);
TOKUI_FUNC const TValue *tokuA_getival(toku_State *T, List *l, int i);
TOKUI_FUNC void tokuA_geti(toku_State *T, List *l, int i, TValue *res);
TOKUI_FUNC int tokuA_findindex(List *l, int rev, int nn, int s, int e);
TOKUI_FUNC void tokuA_free(toku_State *T, List *l);

#endif
