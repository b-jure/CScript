/*
** tlist.c
** List manipulation functions
** See Copyright Notice in tokudae.h
*/

#define tlist_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tlist.h"
#include "tokudaelimits.h"
#include "tstring.h"
#include "tgc.h"
#include "tmem.h"
#include "tmeta.h"
#include "tdebug.h"
#include "tlexer.h"


static const char *strneg = "negative";
static const char *stroit = "of invalid type";
static const char *stridx = "index";
static const char *strlng = "length";


List *tokuA_new(toku_State *T) {
    GCObject *o = tokuG_new(C, sizeof(List), TOKU_VLIST);
    List *l = gco2list(o);
    l->size = l->len = 0;
    l->arr = NULL;
    return l;
}


void tokuA_shrink(toku_State *T, List *l) {
    if (l->arr && l->len < l->size)
        tokuM_shrinkarray(C, l->arr, l->size, l->len, TValue);
}


int tokuA_ensure(toku_State *T, List *l, int space) {
    if (space <= l->len) /* in bound? */
        return check_exp(0 <= space, 0); /* done */
    else {
        tokuM_ensurearray(C, l->arr, l->size, l->len, space - l->len,
                           TOKU_MAXINT, "list elements", TValue);
        for (int i = l->len; i < space; i++)
            setnilval(&l->arr[i]); /* clear new part */
        return 1;
    }
}


void tokuA_ensureindex(toku_State *T, List *l, int index) {
    toku_assert(index >= -1);
    if (tokuA_ensure(C, l, index + 1)) /* length changed? */
        l->len = index + 1;
}


List *tokuA_newl(toku_State *T, int n) {
    List *l = tokuA_new(C);
    toku_assert(n > 0);
    setlistval2s(C, C->sp.p++, l); /* anchor it */
    tokuA_ensureindex(C, l, n - 1);
    C->sp.p--; /* remove list */
    return l;
}


void tokuA_init(toku_State *T) {
    static const char *fields[LFNUM] = { "len", "last", "x", "y", "z" };
    toku_assert(FIRSTLF + LFNUM <= TOKU_MAXUBYTE);
    for (int i = 0; i < LFNUM; i++) {
        OString *s = tokuS_new(C, fields[i]);
        s->extra = cast_ubyte(i + FIRSTLF);
        G(C)->listfields[i] = s;
        tokuG_fix(C, obj2gco(G(C)->listfields[i]));
    }
}


t_sinline void setfield(toku_State *T, List *l, int lf, const TValue *v) {
    switch (lf) {
        case LFLEN: { /* set list length */
            toku_Integer i;
            if (t_likely(tointeger(v, &i))) {
                if (t_likely(i >= 0)) {
                    i = (i <= TOKU_MAXINT) ? i : TOKU_MAXINT;
                    tokuA_ensure(C, l, i);
                    l->len = i;
                } else /* otherwise negative length */
                    tokuD_listerror(C, v, strlng, strneg);
            } else
                tokuD_listerror(C, v, strlng, stroit);
            break;
        }
        case LFLAST: { /* set the last element */
            if (l->len > 0)
                setobj(C, &l->arr[l->len - 1], v);
            /* else, do nothing */
            break;
        }
        case LFX: case LFY: case LFZ: { /* set 1st, 2nd or 3rd element */
            int i = lf - LFX;
            tokuA_ensureindex(C, l, i);
            setobj(C, &l->arr[i], v);
            break;
        }
        default: toku_assert(0); /* unreachable */
    }
}


t_sinline void getfield(toku_State *T, List *l, int lf, TValue *out) {
    switch (lf) {
        case LFLEN: { /* get list length */
            setival(out, l->len);
            break;
        }
        case LFLAST: { /* get the last element */
            if (l->len > 0) {
                tokuA_fastget(C, l, l->len - 1, out);
            } else
                setnilval(out);
            break;
        }
        case LFX: case LFY: case LFZ: { /* get 1st, 2nd or 3rd element */
            int i = lf - LFX;
            if (i < l->len) {
                tokuA_fastget(C, l, i, out);
            } else
                setnilval(out);
            break;
        }
        default: toku_assert(0); /* unreachable */
    }
}


t_sinline void trylistfield(toku_State *T, List *l, const TValue *k,
                                         const TValue *v, TValue *out) {
    OString *idx = strval(k);
    if (t_likely(islistfield(idx))) { /* valid list field? */
        int lf = idx->extra - FIRSTLF;
        toku_assert(lf >= 0 && lf < LFNUM);
        if (!out) { /* set list field? */
            toku_assert(v);
            setfield(C, l, lf, v);
        } else { /* otherwise get list field */
            toku_assert(!v);
            getfield(C, l, lf, out);
        }
    } else /* otherwise invalid list field */
        tokuD_listerror(C, k, stridx, "unknown field");
}


/*
** WARNING: when using this function the caller probably needs to
** check a GC barrier.
*/
void tokuA_setstr(toku_State *T, List *l, const TValue *k, const TValue *v) {
    trylistfield(C, l, k, v, NULL);
}


/*
** Ditto for GC barrier.
*/
void tokuA_setint(toku_State *T, List *l, const FatValue *k, const TValue *v) {
    if (t_likely(0 <= k->i)) { /* index is 0 or positive? */
        if (t_unlikely(TOKU_MAXLISTINDEX < k->i)) /* 'index' too large? */
            tokuD_listerror(C, k->v, stridx, "too large");
        else { /* ok */
            tokuA_ensureindex(C, l, k->i);
            setobj(C, &l->arr[k->i], v);
        }
    } else /* XXX: remove this branch (wrap as unsigned) */
        tokuD_listerror(C, k->v, stridx, strneg);
}


/*
** Ditto for GC barrier.
*/
void tokuA_set(toku_State *T, List *l, const TValue *k, const TValue *v) {
    FatValue fv;
    if (t_likely(tointeger(k, &fv.i))) { /* index is integer? */
        fv.v = k;
        tokuA_setint(C, l, &fv, v);
    } else if (ttisstring(k)) /* index is string? */
        tokuA_setstr(C, l, k, v);
    else /* otherwise invalid index value */
        tokuD_listerror(C, k, stridx, stroit);
}


void tokuA_getstr(toku_State *T, List *l, const TValue *k, TValue *out) {
    trylistfield(C, l, k, NULL, out);
}


void tokuA_getint(toku_State *T, List *l, const FatValue *k, TValue *out) {
    if (t_likely(0 <= k->i)) { /* positive index? */
        if (k->i < l->len) { /* index in bounds? */
            setobj(C, out, &l->arr[k->i]);
        } else /* otherwise index out of bounds */
            setnilval(out);
    } else /* XXX: remove this branch */
        tokuD_listerror(C, k->v, stridx, strneg);
}


void tokuA_get(toku_State *T, List *l, const TValue *k, TValue *out) {
    FatValue fv;
    if (t_likely(tointeger(k, &fv.i))) { /* index is integer? */
        fv.v = k;
        tokuA_getint(C, l, &fv, out);
    } else if (ttisstring(k)) /* index is a string? */
        tokuA_getstr(C, l, k, out);
    else /* otherwise invalid index value */
        tokuD_listerror(C, k, stridx, stroit);
}


/* returns reference to the value at index 'i' */
const TValue *tokuA_getival(toku_State *T, List *l, int i) {
    toku_assert(i >= 0);
    if (i < l->len)
        return &l->arr[i];
    else
        return &G(C)->nil;
}


/* similar to above function, except this returns the value into 'res' */
void tokuA_geti(toku_State *T, List *l, int i, TValue *res) {
    toku_assert(i >= 0);
    if (i < l->len) {
        tokuA_fastget(C, l, i, res);
    } else
        setnilval(res);
}


int tokuA_findindex(List *l, int rev, int nn, int s, int e) {
    toku_assert(0 <= s && e < l->len);
    if (!rev) { /* search from start */
        for (; s <= e; s++)
            if (!ttisnil(&l->arr[s]) == nn) return s;
    } else { /* search from end (reverse) */
        for (; s <= e; e--)
            if (!ttisnil(&l->arr[e]) == nn) return e;
    }
    return -1; /* not found */
}


void tokuA_free(toku_State *T, List *l) {
    tokuM_freearray(C, l->arr, l->size);
    tokuM_free(C, l);
}
