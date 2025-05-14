/*
** clist.c
** List manipulation functions
** See Copyright Notice in cscript.h
*/

#define clist_c
#define CS_CORE

#include "cprefix.h"

#include "clist.h"
#include "climits.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cdebug.h"


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


/*
** Warning: when using this function the caller probably needs to
** check a GC barrier.
*/
void csA_set(cs_State *C, List *l, const TValue *val, const TValue *index) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* non-negative index? */
            if (c_unlikely(MAXLISTINDEX < i)) /* 'index' too large? */
                csD_indexerror(C, i, "too large"); /* error */
            else { /* ok */
                csA_ensureindex(C, l, i);
                setobj(C, &l->b[i], val);
            }
        } else /* error; negative index */
            csD_indexerror(C, i, "negative");
    } else /* error; invalid index type */
        csD_indexterror(C, index);
}


const TValue *csA_get(cs_State *C, List *l, const TValue *index) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* positive index? */
            if (i < l->n) /* index in bounds? */
                return &l->b[i];
            else /* otherwise index out of bounds */
                return &G(C)->nil;
        } else /* error; negative index */
            csD_indexerror(C, i, "negative");
    } else /* error; invalid index type */
        csD_indexterror(C, index);
}


const TValue *csA_getival(cs_State *C, List *l, int i) {
    cs_assert(i >= 0);
    if (i < l->n)
        return &l->b[i];
    else
        return &G(C)->nil;
}


void csA_geti(cs_State *C, List *l, int i, TValue *res) {
    cs_assert(i >= 0);
    if (i < l->n) {
        csA_fastget(C, l, i, res);
    } else
        setnilval(res);
}


int csA_findindex(List *l, int rev, int nn, int s, int e) {
    cs_assert(s >= 0 && e < l->n);
    if (!rev) { /* search from start */
        for (; s <= e; s++)
            if (!ttisnil(&l->b[s]) == nn) return s;
    } else { /* search from end (reverse) */
        for (; s <= e; e--)
            if (!ttisnil(&l->b[e]) == nn) return e;
    }
    return -1; /* not found */
}


void csA_free(cs_State *C, List *l) {
    csM_freearray(C, l->b, l->sz);
    csM_free(C, l);
}
