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
    cs_assert(FIRSTLF + LFNUM <= MAXBYTE);
    for (int i = 0; i < LFNUM; i++) {
        OString *s = csS_new(C, fields[i]);
        s->extra = cast_byte(i + FIRSTLF);
        G(C)->listfields[i] = s;
        csG_fix(C, obj2gco(G(C)->listfields[i]));
    }
}


c_sinline void setfield(cs_State *C, List *l, int lf, const TValue *v) {
    switch (lf) {
        case LFLEN: { /* set list length */
            cs_Integer i;
            if (c_likely(tointeger(v, &i))) {
                if (c_likely(i >= 0)) {
                    i = (i <= MAXINT) ? i - 1 : MAXINT - 1;
                    csA_ensureindex(C, l, i);
                    setobj(C, &l->b[i], v);
                } else /* otherwise negative length */
                    csD_llenerror(C, "expected positive integer or 0");
            } else
                csD_llenerror(C, "expected integer");
            break;
        }
        case LFLAST: { /* set the last element */
            if (l->n > 0)
                setobj(C, &l->b[l->n - 1], v);
            /* else, do nothing */
            break;
        }
        case LFX: case LFY: case LFZ: { /* set 1st, 2nd or 3rd element */
            int i = lf - LFX;
            csA_ensureindex(C, l, i);
            setobj(C, &l->b[i], v);
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
                csA_fastget(C, l, l->n - 1, out);
            } else
                setnilval(out);
            break;
        }
        case LFX: case LFY: case LFZ: { /* get 1st, 2nd or 3rd element */
            int i = lf - LFX;
            if (i < l->n) {
                csA_fastget(C, l, i, out);
            } else
                setnilval(out);
            break;
        }
        default: cs_assert(0); /* unreachable */
    }
}


c_sinline void trylistfield(cs_State *C, List *l, OString *idx,
                            const TValue *v, TValue *out) {
    if (c_likely(islistfield(idx))) { /* valid list field? */
        int lf = idx->extra - FIRSTLF;
        assert(lf >= 0 && lf < LFNUM);
        if (!out) { /* set list field? */
            assert(v);
            setfield(C, l, lf, v);
        } else { /* otherwise get list field */
            assert(!v);
            getfield(C, l, lf, out);
        }
    } else /* otherwise invalid list field */
        csD_runerror(C, "invalid list field ('%s')", getstr(idx));
}


/*
** Warning: when using this function the caller probably needs to
** check a GC barrier.
*/
void csA_setstr(cs_State *C, List *l, OString *i, const TValue *v) {
    trylistfield(C, l, i, v, NULL);
}


/*
** Ditto for GC barrier.
*/
void csA_setint(cs_State *C, List *l, cs_Integer i, const TValue *v) {
    if (c_likely(0 <= i)) { /* index is 0 or positive? */
        if (c_unlikely(MAXLISTINDEX < i)) /* 'index' too large? */
            csD_indexerror(C, i, "too large");
        else { /* ok */
            csA_ensureindex(C, l, i);
            setobj(C, &l->b[i], v);
        }
    } else /* otherwise negative index */
        csD_indexerror(C, i, "negative");
}


/*
** Ditto for GC barrier.
*/
void csA_set(cs_State *C, List *l, const TValue *idx, const TValue *v) {
    cs_Integer i;
    if (c_likely(tointeger(idx, &i))) /* index is integer? */
        csA_setint(C, l, i, v);
    else if (ttisstring(idx)) /* index is string? */
        csA_setstr(C, l, strval(idx), v);
    else /* otherwise invalid index value */
        csD_indexterror(C, idx);
}


void csA_getstr(cs_State *C, List *l, OString *i, TValue *out) {
    trylistfield(C, l, i, NULL, out);
}


void csA_getint(cs_State *C, List *l, cs_Integer i, TValue *out) {
    if (c_likely(0 <= i)) { /* positive index? */
        if (i < l->n) { /* index in bounds? */
            setobj(C, out, &l->b[i]);
        } else /* otherwise index out of bounds */
            setnilval(out);
    } else /* error; negative index */
        csD_indexerror(C, i, "negative");
}


void csA_get(cs_State *C, List *l, const TValue *idx, TValue *out) {
    cs_Integer i;
    if (c_likely(tointeger(idx, &i))) /* index is integer? */
        csA_getint(C, l, i, out);
    else if (ttisstring(idx)) /* index is a string? */
        csA_getstr(C, l, strval(idx), out);
    else /* otherwise invalid index value */
        csD_indexterror(C, idx);
}


/* returns reference to the value at index 'i' */
const TValue *csA_getival(cs_State *C, List *l, int i) {
    cs_assert(i >= 0);
    if (i < l->n)
        return &l->b[i];
    else
        return &G(C)->nil;
}


/* similar to above function, except this returns the value into 'res' */
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
