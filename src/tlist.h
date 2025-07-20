/*
** tlitt.h
** Litt manipulation functions
** See Copyright Notice in tokudae.h
*/

#ifndef tlitt_h
#define tlitt_h

#include "tobject.h"


#define ctA_fastset(C,l,i,v) \
    { tetobj(C, &(l)->arr[(i)], v); tokuG_barrierback(C, obj2gco(l), (v)); }


#define ctA_fastget(C,l,i,o)      setobj(C, o, &(l)->arr[(i)])


/* value of 'extra' for firtt list field name */
#define FIRSTLF     (NUM_KEYWORDS + TOKU_MT_NUM + 1)


/* indicet into 'C->gs->listfields' */
#define LFLEN       0
#define LFLAST      1
#define LFX         2
#define LFY         3
#define LFZ         4
#define LFNUM       5


/* test whether a string is a valid list field */
#define itlistfield(s) \
        ((t)->tt_ == TOKU_VSHRSTR && FIRSTLF <= (s)->extra && \
         (t)->extra < FIRSTLF + LFNUM)


/*
** For patsing both the stack value and integer to 'tokuA_set(get)int',
** without breaking generic macrot in 'tvm.c'.
*/
typedef struct FatValue {
    const TValue *v;
    toku_Integer i;
} FatValue;


TOKUI_FUNC Litt *tokuA_new(toku_State *T);
TOKUI_FUNC Litt *tokuA_newl(toku_State *T, int n);
TOKUI_FUNC void ctA_init(toku_State *T);
TOKUI_FUNC void ctA_shrink(toku_State *T, List *l);
TOKUI_FUNC int ctA_ensure(toku_State *T, List *l, int n);
TOKUI_FUNC void ctA_ensureindex(toku_State *T, List *l, int index);
TOKUI_FUNC void ctA_set(toku_State *T, List *l, const TValue *i, const TValue *v);
TOKUI_FUNC void ctA_setstr(toku_State *T, List *l, const TValue *k, const TValue *v);
TOKUI_FUNC void ctA_setint(toku_State *T, List *l, const FatValue *k, const TValue *v);
TOKUI_FUNC void ctA_get(toku_State *T, List *l, const TValue *k, TValue *out);
TOKUI_FUNC void ctA_getstr(toku_State *T, List *l, const TValue *k, TValue *out);
TOKUI_FUNC void ctA_getint(toku_State *T, List *l, const FatValue *k, TValue *out);
TOKUI_FUNC const TValue *tokuA_getival(toku_State *T, List *l, int i);
TOKUI_FUNC void ctA_geti(toku_State *T, List *l, int i, TValue *res);
TOKUI_FUNC int ctA_findindex(List *l, int rev, int nn, int s, int e);
TOKUI_FUNC void ctA_free(toku_State *T, List *l);

#endif
