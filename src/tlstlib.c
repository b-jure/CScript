/*
** tlstlib.c
** Standard library for list manipulation
** See Copyright Notice in tokudae.h
*/

#define tlstlib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"
#include "tokudaelimits.h"


#define lbcheck(idx)        (0 <= (toku_Integer)(idx))
#define ubcheck(idx)        ((toku_Integer)(idx) < TOKU_MAXINT)

/* check if 'idx' is in bounds */
#define bcheck(idx)         (lbcheck(idx) && ubcheck(idx))


#define checklist(C,n,p,b) \
        (csL_check_type(C, n, TOKU_T_LIST), auxlen(C,n,p,b))


static int auxlen(toku_State *T, int index, int push, int border) {
    int i;
    switch (border) {
        case 1: { /* respect border */
            i = csL_find_index(C, index, TOKU_FI_NIL);
            if (i >= 0) break;
            /* else... */
        } /* fall through */
        default: { /* get regular length */
            toku_assert(border == 0 || i < 0);
            i = toku_len(C, index);
        }
    }
    if (push) toku_push_integer(C, i);
    return i;
}


static int lst_len(toku_State *T) {
    int border = csL_opt(C, toku_to_bool, 1, 1);
    checklist(C, 0, 1, border);
    return 1;
}


static int enumerateaux(toku_State *T) {
    toku_Integer size = checklist(C, 0, 0, csL_opt(C, toku_to_bool, 2, 1));
    toku_Integer i = csL_check_integer(C, 1);
    i = csL_intop(+, i, 1);
    toku_push_integer(C, i);
    if (i < size) {
        toku_get_index(C, 0, i);
        return 2;
    }
    /* otherwise only return nil (next index might be non-nil) */
    toku_push_nil(C);
    return 1;
}


static int lst_enumerate(toku_State *T) {
    csL_check_type(C, 0, TOKU_T_LIST);
    toku_push_cfunction(C, enumerateaux); /* iteration function */
    toku_push(C, 0); /* state */
    toku_push_integer(C, -1); /* initial value */
    return 3;
}


static int lst_insert(toku_State *T) {
    toku_Integer size = checklist(C, 0, 0, csL_opt(C, toku_to_bool, 3, 1));
    toku_Integer pos = size;
    switch (toku_getntop(C)) {
        case 2: break; /* position is already set */
        case 4: {
            toku_setntop(C, 3); /* remove optional bool (border flag) */
        } /* fall through */
        case 3: { /* have position */
            pos = csL_check_integer(C, 1);
            csL_check_arg(C, 0 <= pos && pos <= size, 1,
                             "position out of bounds");
            /* memmove(&l[pos+1], &l[pos], size-pos) */
            for (int i=size-1; pos <= i; i--) {
                toku_get_index(C, 0, i);
                toku_set_index(C, 0, i+1);
            }
            break;
        }
        default: csL_error(C, "wrong number of arguments to 'insert'");
    }
    toku_set_index(C, 0, pos);
    return 0;
}


static int lst_remove(toku_State *T) {
    toku_Integer size = checklist(C, 0, 0, csL_opt(C, toku_to_bool, 2, 1));
    toku_Integer pos = csL_opt_integer(C, 1, size-(size>0));
    csL_check_arg(C, 0 <= pos && pos < size+(size==0), 1,
                     "position out of bounds");
    toku_get_index(C, 0, pos); /* result = l[pos]; */
    if (size != 0) { /* the list is not empty? */
        /* memcpy(&l[pos], &l[pos]+1, size-pos-1) */
        for (; pos < size-1; pos++) {
            toku_get_index(C, 0, pos+1);
            toku_set_index(C, 0, pos); /* l[pos] = l[pos+1]; */
        }
        toku_push_nil(C);
        toku_set_index(C, 0, pos); /* remove slot l[pos] */
    }
    return 1;
}


/* check list start/end indices */
static void check_bounds(toku_State *T, int idx1, toku_Integer i,
                                      int idx2, toku_Integer e) {
    csL_check_arg(C, bcheck(i), idx1, "start index out of bounds");
    csL_check_arg(C, bcheck(e), idx2, "end index out of bounds");
}


/*
** Copy elements (0[f], ..., 0[e]) into (dl[d], dl[d+1], ...).
*/
static int lst_move(toku_State *T) {
    toku_Integer f = csL_check_integer(C, 1); /* from */
    toku_Integer e = csL_check_integer(C, 2); /* end */
    toku_Integer d = csL_check_integer(C, 3); /* destination */
    int dl = !toku_is_noneornil(C, 4) ? 4 : 0; /* destination list */
    csL_check_type(C, 0, TOKU_T_LIST);
    csL_check_type(C, dl, TOKU_T_LIST);
    check_bounds(C, 1, f, 2, e);
    if (e >= f) { /* otherwise, nothing to move */
        int n;
        csL_check_arg(C, bcheck(d), 3, "destination index out of bounds");
        n = cast_int(e) - cast_int(f) + 1; /* number of elements to move */
        if (d > e || d <= f || (dl != 0 && !toku_compare(C, 0, dl, TOKU_ORD_EQ))) {
            for (int i = 0; i < n; i++) { /* lists are not overlapping */
                toku_get_index(C, 0, f + i);
                toku_set_index(C, dl, d + i);
            }
        } else { /* list are overlapping */
            for (int i = n - 1; i >= 0; i--) {
                toku_get_index(C, 0, f + i);
                toku_set_index(C, dl, d + i);
            }
        }
    }
    toku_push(C, dl); /* return destination list */
    return 1;
}


static int lst_new(toku_State *T) {
    toku_Unsigned size = (toku_Unsigned)csL_check_integer(C, 0);
    csL_check_arg(C, size <= cast_uint(TOKU_MAXINT), 0, "out of range");
    toku_push_list(C, cast_int(size));
    return 1;
}


static int lst_flatten(toku_State *T) {
    toku_Unsigned n;
    toku_Integer l = checklist(C, 0, 0, 0);
    toku_Integer i = csL_opt_integer(C, 1, 0);
    toku_Integer e = csL_opt(C, csL_check_integer, 2, l-(l>0));
    check_bounds(C, 1, i, 2, e);
    e = (e >= l) ? l-1 : e; /* clamp */
    if (e < i) return 0; /* empty range or empty list */
    n = t_castS2U(e) - t_castS2U(i); /* number of elements minus 1 */
    if (t_unlikely(!toku_checkstack(C, cast_int(++n))))
        return csL_error(C, "too many results");
    while (i < e) /* push l[i..e - 1] (to avoid overflows in 'i') */
        toku_get_index(C, 0, i++);
    toku_get_index(C, 0, e); /* push last element */
    return n;
}


static void addvalue(toku_State *T, csL_Buffer *b, int i) {
    int t;
    toku_get_index(C, 0, i);
    t = toku_type(C, -1);
    if (t == TOKU_T_NUMBER) {
        char numbuff[TOKU_N2SBUFFSZ];
        toku_numbertocstring(C, -1, numbuff); /* convert it to string */
        toku_push_string(C, numbuff); /* push it on stack */
        toku_replace(C, -2); /* and replace original value */
    } else if (t != TOKU_T_STRING) /* value is not a string? */
        csL_error(C, "cannot concat value (%s) at index %d",
                      toku_typename(C, t), i);
    csL_buff_push_stack(b);
}


#define getnexti(C,i,e,sn) \
        (!(sn) ? cast_int(i) + 1 \
               : toku_find_index(C, 0, 0, cast_int(i), cast_int(e)))


static int lst_concat(toku_State *T) {
    size_t lsep;
    toku_Integer l = checklist(C, 0, 0, 0);
    const char *sep = csL_opt_lstring(C, 1, "", &lsep);
    toku_Integer i = csL_opt_integer(C, 2, 0);
    toku_Integer e = csL_opt_integer(C, 3, l - (l>0));
    int sn = csL_opt(C, toku_to_bool, 4, 1);
    check_bounds(C, 2, i, 3, e);
    if (0 < l && i <= e) { /* list has elements and range is non-empty? */
        const int x = !sn; /* if using 'toku_find_index' go one index behind */
        int next = getnexti(C, i-x, e, sn);
        csL_Buffer b;
        csL_buff_init(C, &b);
        while (0 <= next && next < e) { /* l[next .. (next < e)] */
            addvalue(C, &b, next);
            csL_buff_push_lstring(&b, sep, lsep);
            next = getnexti(C, next+1-x, e, sn);
        }
        if (next == e && (!sn || 0 <= toku_find_index(C, 0, 0, next, e)))
            addvalue(C, &b, cast_int(e)); /* add last value */
        csL_buff_end(&b);
    } else /* otherwise nothing to concatenate */
        toku_push_literal(C, "");
    return 1;
}


/* {===================================================================
** Quicksort implementation (based on Lua's implementation of
** 'Algorithms in MODULA-3', Robert Sedgewick, Addison-Wesley, 1993.)
** ==================================================================== */

typedef unsigned int Idx;


#define geti(C,idl,idx)     toku_get_index(C, idl, t_castU2S(idx))
#define seti(C,idl,idx)     toku_set_index(C, idl, t_castU2S(idx))


/*
** Lists larger than 'RANLIMIT' may use randomized pivots.
*/
#define RANLIMIT    100u


/*
** Error for invalid sorting function.
*/
#define FNSORTERR   "invalid order function for sorting"


static void get2(toku_State *T, Idx idx1, Idx idx2) {
    geti(C, 0, idx1);
    geti(C, 0, idx2);
}


static void set2(toku_State *T, Idx idx1, Idx idx2) {
    seti(C, 0, idx1);
    seti(C, 0, idx2);
}


static int sort_cmp(toku_State *T, Idx a, Idx b) {
    if (toku_is_nil(C, 1)) /* no compare function? */
        return toku_compare(C, a, b, TOKU_ORD_LT);
    else { /* otherwise call compare function */
        int res;
        toku_push(C, 1);          /* push function */
        toku_push(C, a-1);        /* -1 to compensate for function */
        toku_push(C, b-2);        /* -2 to compensate for function and 'a' */
        toku_call(C, 2, 1);       /* call function */
        res = toku_to_bool(C, -1); /* get result */
        toku_pop(C, 1);           /* pop result */
        return res;
    }
}


/*
** Does the partition: pivot P is at the top of the stack.
** precondition: l[lo] <= P == l[hi-1] <= l[hi],
** so it only needs to do the partition from lo + 1 to hi - 2.
** Pos-condition: l[lo .. i - 1] <= l[i] == P <= l[i + 1 .. hi]
** returns 'i'.
*/
static Idx partition(toku_State *T, Idx lo, Idx hi) {
    Idx i = lo; /* will be incremented before first use */
    Idx j = hi - 1; /* will be decremented before first use */
    /* loop invariant: l[lo .. i] <= P <= l[j .. hi] */
    for (;;) {
        /* next loop: repeat ++i while l[i] < P */
        while ((void)geti(C, 0, ++i), sort_cmp(C, -1, -2)) {
            if (t_unlikely(i == hi - 1)) { /* l[hi - 1] < P == l[hi - 1] */
                /* (pivot element can't be less than itself...) */
                csL_error(C, FNSORTERR);
            }
            toku_pop(C, 1); /* remove l[i] */
        }
        /* after the loop, l[i] >= P and l[lo .. i - 1] < P  (l) */
        /* next loop: repeat --j while P < l[j] */
        while ((void)geti(C, 0, --j), sort_cmp(C, -3, -1)) {
            if (t_unlikely(j < i)) { /* j <= i-1 and l[j] > P, contradicts (l) */
                /* (can't have P < l[j] <= l[i-1] and P < l[i-1] */
                csL_error(C, FNSORTERR);
            }
            toku_pop(C, 1); /* remove l[j] */
        }
        /* after the loop, l[j] <= P and l[j + 1 .. hi] >= P */
        if (j < i) { /* no elements out of place? */
            /* l[lo .. i - 1] <= P <= l[j + 1 .. i .. hi] */
            toku_pop(C, 1); /* pop l[j] */
            /* swap pivot l[hi - 1] with l[i] to satisfy pos-condition */
            set2(C, hi - 1, i);
            return i;
        }
        /* otherwise, swap l[i] with l[j] to restore invariant and repeat */
        set2(C, i, j);
    }
}


/*
** Choose an element in the middle (2nd-3th quarters) of [lo,hi]
** "randomized" by 'rnd'.
*/
static Idx chose_pivot(Idx lo, Idx hi, unsigned int rnd) {
    Idx r4 = (hi - lo) / 4; /* range/4 */
    Idx p = (rnd ^ lo ^ hi) % (r4 * 2) + (lo + r4);
    toku_assert(lo + r4 <= p && p <= hi - r4);
    return p;
}


static void auxsort(toku_State *T, Idx lo, Idx hi, unsigned rnd) {
    while (lo < hi) {
        Idx n;
        Idx p; /* pivot index */
        get2(C, lo, hi);
        if (sort_cmp(C, -1, -2)) /* l[hi] < l[lo]? */
            set2(C, lo, hi); /* swap l[hi] with l[lo] */
        else
            toku_pop(C, 2);
        if (hi - lo == 1) /* only 2 elements? */
            return; /* done */
        if (hi - lo < RANLIMIT || !rnd) /* small interval or no randomize */
            p = lo + ((hi - lo) / 2); /* use middle element as pivot */
        else
            p = chose_pivot(lo, hi, rnd);
        get2(C, p, lo);
        if (sort_cmp(C, -2, -1)) /* l[p] < l[lo]? */
            set2(C, p, lo); /* swap l[p] with l[lo] */
        else {
            toku_pop(C, 1); /* remove l[lo] */
            geti(C, 0, hi);
            if (sort_cmp(C, -1, -2)) /* l[hi] < l[p] */ 
                set2(C, p, hi); /* swap l[hi] with l[p] */
            else
                toku_pop(C, 2);
        }
        if (hi - lo == 2) /* only 3 elements? */
            return; /* done */
        geti(C, 0, p); /* get pivot */
        toku_push(C, -1); /* push pivot */
        geti(C, 0, hi - 1); /* push l[hi - 1] */
        set2(C, p, hi - 1); /* swap pivot l[p] with l[hi - 1] */
        p = partition(C, lo, hi);
        /* l[lo .. p - 1] <= l[p] == P <= l[p + 1 .. hi] */
        if (p - lo < hi - p) { /* lower interval is smaller? */
            auxsort(C, lo, p-1, rnd); /* call recursively for lower interval */
            n = p - lo; /* size of smaller interval */
            lo = p + 1; /* tail call for [p + 1 .. hi] (upper interval) */
        } else {
            auxsort(C, p+1, hi, rnd); /* call recursively for upper interval */
            n = hi - p; /* size of smaller interval */
            hi = p - 1; /* tail call for [lo .. p - 1]  (lower interval) */
        }
        if ((hi - lo) / 128 > n) /* partition too imbalanced? */
            rnd = csL_makeseed(C); /* try a new randomization */
    } /* tail call auxsort(C, lo, hi, rnd) */
}


static int lst_sort(toku_State *T) {
    toku_Integer size = checklist(C, 0, 0, csL_opt(C, toku_to_bool, 2, 1));
    if (size > 1) { /* non trivial? */
        csL_check_arg(C, size <= TOKU_MAXINT, 0, "list too big");
        if (!toku_is_noneornil(C, 1)) /* is there a 2nd argument? */
            csL_check_type(C, 1, TOKU_T_FUNCTION); /* it must be a function */
        toku_setntop(C, 2); /* make sure there are two arguments */
        auxsort(C, 0, (Idx)size-1u, 0);
    }
    return 0;
}

/* }=================================================================== */


static int lst_shrink(toku_State *T) {
    csL_check_type(C, 0, TOKU_T_LIST);
    toku_shrinklist(C, 0);
    return 0;
}


static int lst_isordered(toku_State *T) {
    int sorted = 1;
    toku_Integer i, e;
    toku_Integer size = checklist(C, 0, 0, csL_opt(C, toku_to_bool, 4, 1));
    if (!toku_is_noneornil(C, 1)) /* is there a 2nd argument? */
        csL_check_type(C, 1, TOKU_T_FUNCTION); /* must be a function */
    i = csL_opt_integer(C, 2, 0); /* start index */
    e = csL_opt_integer(C, 3, size-(size>0)); /* end index */
    check_bounds(C, 2, i, 3, e);
    if (e > i) {
        toku_setntop(C, 2); /* make sure there are two arguments */
        while (sorted && i < e) {
            toku_get_index(C, 0, cast_int(i));
            toku_get_index(C, 0, cast_int(i)+1);
            sorted = sort_cmp(C, -2, -1);
            toku_pop(C, 2);
            i++;
        }
    }
    toku_push_bool(C, sorted);
    if (!sorted) {
        toku_push_integer(C, i-1);
        return 2; /* return false and first index which breaks ordering */
    }
    return 1; /* return true */
}


/// TODO: add docs
static csL_Entry lstlib[] = {
    {"len", lst_len},
    {"enumerate", lst_enumerate},
    {"insert", lst_insert},
    {"remove", lst_remove},
    {"move", lst_move},
    {"new", lst_new},
    {"flatten", lst_flatten},
    {"concat", lst_concat},
    {"sort", lst_sort},
    {"shrink", lst_shrink},
    {"isordered", lst_isordered},
    {"maxindex", NULL},
    {NULL, NULL}
};


CSMOD_API int tokuopen_list(toku_State *T) {
    csL_push_lib(C, lstlib);
    toku_push_integer(C, TOKU_MAXLISTINDEX);
    toku_set_fieldstr(C, -2, "maxindex");
    return 1;
}
