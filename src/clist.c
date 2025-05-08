/*
** clist.c
** List manipulation functions
** See Copyright Notice in cscript.h
*/

#define clist_c
#define CS_CORE

#include "cprefix.h"

#include "clist.h"
#include "cgc.h"
#include "climits.h"
#include "cmem.h"
#include "cobject.h"


List *csA_new(cs_State *C) {
    GCObject *o = csG_new(C, sizeof(List), CS_VLIST);
    List *l = gco2list(o);
    l->sz = l->n = 0;
    l->b = NULL;
    return l;
}


void csA_shrink(cs_State *C, List *l) {
    if (l->b && l->sz > l->n)
        csM_shrinkarray(C, l->b, l->sz, l->n, TValue);
}


int csA_ensure(cs_State *C, List *l, int space) {
    if (space < l->n) /* in bound? */
        return 0; /* done */
    else {
        csM_ensurearray(C, l->b, l->sz, l->n, (space - l->n) + 1, MAXLISTINDEX,
                        "list elements", TValue);
        for (int i = l->n; i < space; i++)
            setnilval(&l->b[i]); /* clear new part */
        return 1;
    }
}


void csA_ensureindex(cs_State *C, List *l, int index) {
    if (csA_ensure(C, l, index + 1)) /* length changed? */
        l->n = index + 1;
}


List *csA_newl(cs_State *C, int n) {
    List *l = csA_new(C);
    cs_assert(n > 0);
    setlistval2s(C, C->sp.p++, l); /* anchor it */
    csA_ensureindex(C, l, n - 1);
    C->sp.p--; /* remove list */
    return l;
}


void csA_free(cs_State *C, List *l) {
    csM_freearray(C, l->b, l->sz);
    csM_free(C, l);
}
