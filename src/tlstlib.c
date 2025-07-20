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


#define checklist(T,n,p,b) \
        (tokuL_check_type(T, n, TOKU_T_LIST), auxlen(T,n,p,b))


static int auxlen(toku_State *T, int index, int push, int border) {
    int i;
    switch (border) {
        case 1: { /* respect border */
            i = tokuL_find_index(T, index, TOKU_FI_NIL);
            if (i >= 0) break;
            /* else... */
        } /* fall through */
        default: { /* get regular length */
            toku_assert(border == 0 || i < 0);
            i = toku_len(T, index);
        }
    }
    if (push) toku_push_integer(T, i);
    return i;
}


static int lst_len(toku_State *T) {
    int border = tokuL_opt(T, toku_to_bool, 1, 1);
    checklist(T, 0, 1, border);
    return 1;
}


static int enumerateaux(toku_State *T) {
    toku_Integer size = checklist(T, 0, 0, tokuL_opt(T, toku_to_bool, 2, 1));
    toku_Integer i = tokuL_check_integer(T, 1);
    i = tokuL_intop(+, i, 1);
    toku_push_integer(T, i);
    if (i < size) {
        toku_get_index(T, 0, i);
        return 2;
    }
    /* otherwise only return nil (next index might be non-nil) */
    toku_push_nil(T);
    return 1;
}


static int lst_enumerate(toku_State *T) {
    tokuL_check_type(T, 0, TOKU_T_LIST);
    toku_push_cfunction(T, enumerateaux); /* iteration function */
    toku_push(T, 0); /* state */
    toku_push_integer(T, -1); /* initial value */
    return 3;
}


static int lst_insert(toku_State *T) {
    toku_Integer size = checklist(T, 0, 0, tokuL_opt(T, toku_to_bool, 3, 1));
    toku_Integer pos = size;
    switch (toku_getntop(T)) {
        case 2: break; /* position is already set */
        case 4: {
            toku_setntop(T, 3); /* remove optional bool (border flag) */
        } /* fall through */
        case 3: { /* have position */
            pos = tokuL_check_integer(T, 1);
            tokuL_check_arg(T, 0 <= pos && pos <= size, 1,
                             "position out of bounds");
            /* memmove(&l[pos+1], &l[pos], size-pos) */
            for (int i=size-1; pos <= i; i--) {
                toku_get_index(T, 0, i);
                toku_set_index(T, 0, i+1);
            }
            break;
        }
        default: tokuL_error(T, "wrong number of arguments to 'insert'");
    }
    toku_set_index(T, 0, pos);
    return 0;
}


static int lst_remove(toku_State *T) {
    toku_Integer size = checklist(T, 0, 0, tokuL_opt(T, toku_to_bool, 2, 1));
    toku_Integer pos = tokuL_opt_integer(T, 1, size-(size>0));
    tokuL_check_arg(T, 0 <= pos && pos < size+(size==0), 1,
                     "position out of bounds");
    toku_get_index(T, 0, pos); /* result = l[pos]; */
    if (size != 0) { /* the list is not empty? */
        /* memcpy(&l[pos], &l[pos]+1, size-pos-1) */
        for (; pos < size-1; pos++) {
            toku_get_index(T, 0, pos+1);
            toku_set_index(T, 0, pos); /* l[pos] = l[pos+1]; */
        }
        toku_push_nil(T);
        toku_set_index(T, 0, pos); /* remove slot l[pos] */
    }
    return 1;
}


/* check list start/end indices */
static void check_bounds(toku_State *T, int idx1, toku_Integer i,
                                      int idx2, toku_Integer e) {
    tokuL_check_arg(T, bcheck(i), idx1, "start index out of bounds");
    tokuL_check_arg(T, bcheck(e), idx2, "end index out of bounds");
}


/*
** Copy elements (0[f], ..., 0[e]) into (dl[d], dl[d+1], ...).
*/
static int lst_move(toku_State *T) {
    toku_Integer f = tokuL_check_integer(T, 1); /* from */
    toku_Integer e = tokuL_check_integer(T, 2); /* end */
    toku_Integer d = tokuL_check_integer(T, 3); /* destination */
    int dl = !toku_is_noneornil(T, 4) ? 4 : 0; /* destination list */
    tokuL_check_type(T, 0, TOKU_T_LIST);
    tokuL_check_type(T, dl, TOKU_T_LIST);
    check_bounds(T, 1, f, 2, e);
    if (e >= f) { /* otherwise, nothing to move */
        int n;
        tokuL_check_arg(T, bcheck(d), 3, "destination index out of bounds");
        n = cast_int(e) - cast_int(f) + 1; /* number of elements to move */
        if (d > e || d <= f || (dl != 0 && !toku_compare(T, 0, dl, TOKU_ORD_EQ))) {
            for (int i = 0; i < n; i++) { /* lists are not overlapping */
                toku_get_index(T, 0, f + i);
                toku_set_index(T, dl, d + i);
            }
        } else { /* list are overlapping */
            for (int i = n - 1; i >= 0; i--) {
                toku_get_index(T, 0, f + i);
                toku_set_index(T, dl, d + i);
            }
        }
    }
    toku_push(T, dl); /* return destination list */
    return 1;
}


static int lst_new(toku_State *T) {
    toku_Unsigned size = (toku_Unsigned)tokuL_check_integer(T, 0);
    tokuL_check_arg(T, size <= cast_uint(TOKU_MAXINT), 0, "out of range");
    toku_push_list(T, cast_int(size));
    return 1;
}


static int lst_flatten(toku_State *T) {
    toku_Unsigned n;
    toku_Integer l = checklist(T, 0, 0, 0);
    toku_Integer i = tokuL_opt_integer(T, 1, 0);
    toku_Integer e = tokuL_opt(T, tokuL_check_integer, 2, l-(l>0));
    check_bounds(T, 1, i, 2, e);
    e = (e >= l) ? l-1 : e; /* clamp */
    if (e < i) return 0; /* empty range or empty list */
    n = t_castS2U(e) - t_castS2U(i); /* number of elements minus 1 */
    if (t_unlikely(!toku_checkstack(T, cast_int(++n))))
        return tokuL_error(T, "too many results");
    while (i < e) /* push l[i..e - 1] (to avoid overflows in 'i') */
        toku_get_index(T, 0, i++);
    toku_get_index(T, 0, e); /* push last element */
    return n;
}


static void addvalue(toku_State *T, tokuL_Buffer *b, int i) {
    int t;
    toku_get_index(T, 0, i);
    t = toku_type(T, -1);
    if (t == TOKU_T_NUMBER) {
        char numbuff[TOKU_N2SBUFFSZ];
        toku_numbertocstring(T, -1, numbuff); /* convert it to string */
        toku_push_string(T, numbuff); /* push it on stack */
        toku_replace(T, -2); /* and replace original value */
    } else if (t != TOKU_T_STRING) /* value is not a string? */
        tokuL_error(T, "cannot concat value (%s) at index %d",
                      toku_typename(T, t), i);
    tokuL_buff_push_stack(b);
}


#define getnexti(T,i,e,sn) \
        (!(sn) ? cast_int(i) + 1 \
               : toku_find_index(T, 0, 0, cast_int(i), cast_int(e)))


static int lst_concat(toku_State *T) {
    size_t lsep;
    toku_Integer l = checklist(T, 0, 0, 0);
    const char *sep = tokuL_opt_lstring(T, 1, "", &lsep);
    toku_Integer i = tokuL_opt_integer(T, 2, 0);
    toku_Integer e = tokuL_opt_integer(T, 3, l - (l>0));
    int sn = tokuL_opt(T, toku_to_bool, 4, 1);
    check_bounds(T, 2, i, 3, e);
    if (0 < l && i <= e) { /* list has elements and range is non-empty? */
        const int x = !sn; /* if using 'toku_find_index' go one index behind */
        int next = getnexti(T, i-x, e, sn);
        tokuL_Buffer b;
        tokuL_buff_init(T, &b);
        while (0 <= next && next < e) { /* l[next .. (next < e)] */
            addvalue(T, &b, next);
            tokuL_buff_push_lstring(&b, sep, lsep);
            next = getnexti(T, next+1-x, e, sn);
        }
        if (next == e && (!sn || 0 <= toku_find_index(T, 0, 0, next, e)))
            addvalue(T, &b, cast_int(e)); /* add last value */
        tokuL_buff_end(&b);
    } else /* otherwise nothing to concatenate */
        toku_push_literal(T, "");
    return 1;
}


/* {===================================================================
** Quicksort implementation (based on Lua's implementation of
** 'Algorithms in MODULA-3', Robert Sedgewick, Addison-Wesley, 1993.)
** ==================================================================== */

typedef unsigned int Idx;


#define geti(T,idl,idx)     toku_get_index(T, idl, t_castU2S(idx))
#define seti(T,idl,idx)     toku_set_index(T, idl, t_castU2S(idx))


/*
** Lists larger than 'RANLIMIT' may use randomized pivots.
*/
#define RANLIMIT    100u


/*
** Error for invalid sorting function.
*/
#define FNSORTERR   "invalid order function for sorting"


static void get2(toku_State *T, Idx idx1, Idx idx2) {
    geti(T, 0, idx1);
    geti(T, 0, idx2);
}


static void set2(toku_State *T, Idx idx1, Idx idx2) {
    seti(T, 0, idx1);
    seti(T, 0, idx2);
}


static int sort_cmp(toku_State *T, Idx a, Idx b) {
    if (toku_is_nil(T, 1)) /* no compare function? */
        return toku_compare(T, a, b, TOKU_ORD_LT);
    else { /* otherwise call compare function */
        int res;
        toku_push(T, 1);          /* push function */
        toku_push(T, a-1);        /* -1 to compensate for function */
        toku_push(T, b-2);        /* -2 to compensate for function and 'a' */
        toku_call(T, 2, 1);       /* call function */
        res = toku_to_bool(T, -1); /* get result */
        toku_pop(T, 1);           /* pop result */
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
        while ((void)geti(T, 0, ++i), sort_cmp(T, -1, -2)) {
            if (t_unlikely(i == hi - 1)) { /* l[hi - 1] < P == l[hi - 1] */
                /* (pivot element can't be less than itself...) */
                tokuL_error(T, FNSORTERR);
            }
            toku_pop(T, 1); /* remove l[i] */
        }
        /* after the loop, l[i] >= P and l[lo .. i - 1] < P  (l) */
        /* next loop: repeat --j while P < l[j] */
        while ((void)geti(T, 0, --j), sort_cmp(T, -3, -1)) {
            if (t_unlikely(j < i)) { /* j <= i-1 and l[j] > P, contradicts (l) */
                /* (can't have P < l[j] <= l[i-1] and P < l[i-1] */
                tokuL_error(T, FNSORTERR);
            }
            toku_pop(T, 1); /* remove l[j] */
        }
        /* after the loop, l[j] <= P and l[j + 1 .. hi] >= P */
        if (j < i) { /* no elements out of place? */
            /* l[lo .. i - 1] <= P <= l[j + 1 .. i .. hi] */
            toku_pop(T, 1); /* pop l[j] */
            /* swap pivot l[hi - 1] with l[i] to satisfy pos-condition */
            set2(T, hi - 1, i);
            return i;
        }
        /* otherwise, swap l[i] with l[j] to restore invariant and repeat */
        set2(T, i, j);
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
        get2(T, lo, hi);
        if (sort_cmp(T, -1, -2)) /* l[hi] < l[lo]? */
            set2(T, lo, hi); /* swap l[hi] with l[lo] */
        else
            toku_pop(T, 2);
        if (hi - lo == 1) /* only 2 elements? */
            return; /* done */
        if (hi - lo < RANLIMIT || !rnd) /* small interval or no randomize */
            p = lo + ((hi - lo) / 2); /* use middle element as pivot */
        else
            p = chose_pivot(lo, hi, rnd);
        get2(T, p, lo);
        if (sort_cmp(T, -2, -1)) /* l[p] < l[lo]? */
            set2(T, p, lo); /* swap l[p] with l[lo] */
        else {
            toku_pop(T, 1); /* remove l[lo] */
            geti(T, 0, hi);
            if (sort_cmp(T, -1, -2)) /* l[hi] < l[p] */ 
                set2(T, p, hi); /* swap l[hi] with l[p] */
            else
                toku_pop(T, 2);
        }
        if (hi - lo == 2) /* only 3 elements? */
            return; /* done */
        geti(T, 0, p); /* get pivot */
        toku_push(T, -1); /* push pivot */
        geti(T, 0, hi - 1); /* push l[hi - 1] */
        set2(T, p, hi - 1); /* swap pivot l[p] with l[hi - 1] */
        p = partition(T, lo, hi);
        /* l[lo .. p - 1] <= l[p] == P <= l[p + 1 .. hi] */
        if (p - lo < hi - p) { /* lower interval is smaller? */
            auxsort(T, lo, p-1, rnd); /* call recursively for lower interval */
            n = p - lo; /* size of smaller interval */
            lo = p + 1; /* tail call for [p + 1 .. hi] (upper interval) */
        } else {
            auxsort(T, p+1, hi, rnd); /* call recursively for upper interval */
            n = hi - p; /* size of smaller interval */
            hi = p - 1; /* tail call for [lo .. p - 1]  (lower interval) */
        }
        if ((hi - lo) / 128 > n) /* partition too imbalanced? */
            rnd = tokuL_makeseed(T); /* try a new randomization */
    } /* tail call auxsort(T, lo, hi, rnd) */
}


static int lst_sort(toku_State *T) {
    toku_Integer size = checklist(T, 0, 0, tokuL_opt(T, toku_to_bool, 2, 1));
    if (size > 1) { /* non trivial? */
        tokuL_check_arg(T, size <= TOKU_MAXINT, 0, "list too big");
        if (!toku_is_noneornil(T, 1)) /* is there a 2nd argument? */
            tokuL_check_type(T, 1, TOKU_T_FUNCTION); /* it must be a function */
        toku_setntop(T, 2); /* make sure there are two arguments */
        auxsort(T, 0, (Idx)size-1u, 0);
    }
    return 0;
}

/* }=================================================================== */


static int lst_shrink(toku_State *T) {
    tokuL_check_type(T, 0, TOKU_T_LIST);
    toku_shrinklist(T, 0);
    return 0;
}


static int lst_isordered(toku_State *T) {
    int sorted = 1;
    toku_Integer i, e;
    toku_Integer size = checklist(T, 0, 0, tokuL_opt(T, toku_to_bool, 4, 1));
    if (!toku_is_noneornil(T, 1)) /* is there a 2nd argument? */
        tokuL_check_type(T, 1, TOKU_T_FUNCTION); /* must be a function */
    i = tokuL_opt_integer(T, 2, 0); /* start index */
    e = tokuL_opt_integer(T, 3, size-(size>0)); /* end index */
    check_bounds(T, 2, i, 3, e);
    if (e > i) {
        toku_setntop(T, 2); /* make sure there are two arguments */
        while (sorted && i < e) {
            toku_get_index(T, 0, cast_int(i));
            toku_get_index(T, 0, cast_int(i)+1);
            sorted = sort_cmp(T, -2, -1);
            toku_pop(T, 2);
            i++;
        }
    }
    toku_push_bool(T, sorted);
    if (!sorted) {
        toku_push_integer(T, i-1);
        return 2; /* return false and first index which breaks ordering */
    }
    return 1; /* return true */
}


/// TODO: add docs
static tokuL_Entry lstlib[] = {
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


TOKUMOD_API int tokuopen_list(toku_State *T) {
    tokuL_push_lib(T, lstlib);
    toku_push_integer(T, TOKU_MAXLISTINDEX);
    toku_set_fieldstr(T, -2, "maxindex");
    return 1;
}
