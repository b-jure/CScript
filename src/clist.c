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
#include "cstring.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cdebug.h"
#include "clexer.h"


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
    cs_assert(index >= -1);
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


void csA_init(cs_State *C) {
    static const char *fields[LFNUM] = { "len", "last", "x", "y", "z" };
    const int first = NUM_KEYWORDS + CS_MM_NUM + 1;
    cs_assert(NUM_KEYWORDS + CS_MM_NUM + LFNUM <= MAXBYTE);
    for (int i = 0; i < LFNUM; i++) {
        OString *s = csS_new(C, fields[i]);
        s->extra = cast_byte(i + first);
        G(C)->listfields[i] = s;
        csG_fix(C, obj2gco(G(C)->listfields[i]));
    }
}


c_sinline void setfield(cs_State *C, List *l, int lf, const TValue *val) {
    switch (lf) {
        case LFLEN: { /* set list length */
            cs_Integer i;
            if (c_likely(tointeger(val, &i))) {
                if (c_likely(i >= 0)) {
                    i = (i <= MAXINT) ? i - 1 : MAXINT - 1;
                    csA_ensureindex(C, l, i);
                    csA_fastset(C, l, i, val);
                } else /* otherwise negative length */
                    csD_llenerror(C, "expected positive integer or 0");
            } else
                csD_llenerror(C, "expected integer");
            break;
        }
        case LFLAST: { /* set the last element */
            if (l->n > 0)
                csA_fastset(C, l, l->n - 1, val);
            /* else, do nothing */
            break;
        }
        case LFX: case LFY: case LFZ: { /* set 1st, 2nd or 3rd element */
            int i = lf - LFX;
            csA_ensureindex(C, l, i);
            csA_fastset(C, l, i, val);
            break;
        }
        default: cs_assert(0); /* unreachable */
    }
}


c_sinline void getfield(cs_State *C, List *l, int lf, TValue *out) {
    switch (lf) {
        case LFLEN: { /* get list length */
            setival(out, l->n);
            break;
        }
        case LFLAST: { /* get the last element */
            if (l->n > 0) {
                setobj(C, out, &l->b[l->n - 1]);
            } else
                setnilval(out);
            break;
        }
        case LFX: case LFY: case LFZ: { /* get 1st, 2nd or 3rd element */
            int i = lf - LFX;
            if (i < l->n) {
                setobj(C, out, &l->b[i]);
            } else
                setnilval(out);
            break;
        }
        default: cs_assert(0); /* unreachable */
    }
}


c_sinline void trylistfield(cs_State *C, List *l, const TValue *idx,
                           const TValue *val, TValue *out) {
    int lf; /* index of list field (LF*) */
    switch (ttypetag(idx)) {
        case CS_VSHRSTR: {
            OString *s = strval(idx);
            for (int i = 0; i < LFNUM; i++) {
                if (eqshrstr(s, G(C)->listfields[i])) {
                    lf = i;
                    if (!out) goto set;
                    else goto get;
                }
            }
            break;
        }
        case CS_VLNGSTR: {
            OString *s = strval(idx);
            for (int i = 0; i < LFNUM; i++) {
                if (csS_eqlngstr(s, G(C)->listfields[i])) {
                    lf = i;
                    if (!out) goto set;
                    else goto get;
                }
            }
            break;
        }
        default: csD_indexterror(C, idx);
    }
    csD_runerror(C, "invalid list field ('%s')", getstr(strval(idx)));
set:
    cs_assert(out == NULL);
    setfield(C, l, lf, val);
    return;
get:
    cs_assert(val == NULL);
    getfield(C, l, lf, out);
}


/*
** Warning: when using this function the caller probably needs to
** check a GC barrier.
*/
void csA_set(cs_State *C, List *l, const TValue *val, const TValue *idx) {
    cs_Integer i;
    if (c_likely(tointeger(idx, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* non-negative index? */
            if (c_unlikely(MAXLISTINDEX < i)) /* 'index' too large? */
                csD_indexerror(C, i, "too large"); /* error */
            else { /* ok */
                csA_ensureindex(C, l, i);
                setobj(C, &l->b[i], val);
            }
        } else /* error; negative index */
            csD_indexerror(C, i, "negative");
    } else /* index is not an integer */
        trylistfield(C, l, idx, val, NULL);
}


void csA_get(cs_State *C, List *l, const TValue *idx, TValue *out) {
    cs_Integer i;
    if (c_likely(tointeger(idx, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* positive index? */
            if (i < l->n) { /* index in bounds? */
                setobj(C, out, &l->b[i]);
            } else /* otherwise index out of bounds */
                setnilval(out);
        } else /* error; negative index */
            csD_indexerror(C, i, "negative");
    } else /* index is not an integer */
        trylistfield(C, l, idx, NULL, out);
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
