/*
** clstlib.c
** Standard library for list manipulation
** See Copyright Notice in cscript.h
*/

#define clstlib_c
#define CS_LIB

#include "cprefix.h"

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "climits.h"


#define lbcheck(idx)        (0 <= (cs_Integer)(idx))
#define ubcheck(idx)        ((cs_Integer)(idx) < INT_MAX)

/* check if 'idx' is in bounds */
#define bcheck(idx)         (lbcheck(idx) && ubcheck(idx))


#define checklist(C,n,p,b) \
        (csL_check_type(C, n, CS_T_LIST), auxlen(C,n,p,b))



/* list Length TO Index */
#define ltoi(l)         ((l) - ((l) > 0))


static int auxlen(cs_State *C, int index, int push, int border) {
    int i;
    switch (border) {
        case 1: { /* respect border */
            i = csL_find_index(C, index, CS_FI_NIL);
            if (i >= 0) break;
            /* else... */
        } /* fall through */
        default: { /* get regular length */
            cs_assert(border == 0 || i < 0);
            i = cs_len(C, index);
        }
    }
    if (push) cs_push_integer(C, i);
    return i;
}


static int lst_len(cs_State *C) {
    checklist(C, 0, 1, csL_opt_bool(C, 1, 1));
    return 1;
}


static int lst_insert(cs_State *C) {
    cs_Integer size = checklist(C, 0, 0, csL_opt_bool(C, 3, 1));
    cs_Integer pos = size;
    switch (cs_getntop(C)) {
        case 2: break; /* position is already set */
        case 4: {
            cs_setntop(C, 3); /* remove optional bool (border flag) */
        } /* fall through */
        case 3: { /* have position */
            pos = csL_check_integer(C, 1);
            csL_check_arg(C, 0 <= pos && pos <= size, 1,
                             "position out of bounds");
            /* memmove(&l[pos+1], &l[pos], size-pos) */
            for (int i=size-1; pos <= i; i--) {
                cs_get_index(C, 0, i);
                cs_set_index(C, 0, i+1);
            }
            break;
        }
        default: csL_error(C, "wrong number of arguments to 'insert'");
    }
    cs_set_index(C, 0, pos);
    return 0;
}


static int lst_remove(cs_State *C) {
    cs_Integer size = checklist(C, 0, 0, csL_opt_bool(C, 2, 1));
    cs_Integer pos = csL_opt_integer(C, 1, size-(size>0));
    csL_check_arg(C, 0 <= pos && pos < size+(size==0), 1, "position out of bounds");
    cs_get_index(C, 0, pos); /* result = l[pos]; */
    if (size != 0) { /* the list is not empty? */
        /* memcpy(&l[pos], &l[pos]+1, size-pos-1) */
        for (; pos < size-1; pos++) {
            cs_get_index(C, 0, pos+1);
            cs_set_index(C, 0, pos); /* l[pos] = l[pos+1]; */
        }
        cs_push_nil(C);
        cs_set_index(C, 0, pos); /* remove slot l[pos] */
    }
    return 1;
}


/* check list start/end indices */
static void check_bounds(cs_State *C, int idx1, cs_Integer i,
                                      int idx2, cs_Integer e) {
    csL_check_arg(C, bcheck(i), idx1, "start index out of bounds");
    csL_check_arg(C, bcheck(e), idx2, "end index out of bounds");
}


/*
** Copy elements (0[f], ..., 0[e]) into (dl[d], dl[d+1], ...).
*/
static int lst_move(cs_State *C) {
    cs_Integer f = csL_check_integer(C, 1); /* from */
    cs_Integer e = csL_check_integer(C, 2); /* end */
    cs_Integer d = csL_check_integer(C, 3); /* destination */
    int dl = !cs_is_noneornil(C, 4) ? 4 : 0; /* destination list */
    csL_check_type(C, 0, CS_T_LIST);
    csL_check_type(C, dl, CS_T_LIST);
    check_bounds(C, 1, f, 2, e);
    if (e >= f) { /* otherwise, nothing to move */
        int n;
        csL_check_arg(C, bcheck(d), 3, "destination index out of bounds");
        n = (int)e - (int)f + 1; /* number of elements to move */
        if (d > e || d <= f || (dl != 0 && !cs_compare(C, 0, dl, CS_ORD_EQ))) {
            for (int i = 0; i < n; i++) { /* lists are not overlapping */
                cs_get_index(C, 0, f + i);
                cs_set_index(C, dl, d + i);
            }
        } else { /* list are overlapping */
            for (int i = n - 1; i >= 0; i--) {
                cs_get_index(C, 0, f + i);
                cs_set_index(C, dl, d + i);
            }
        }
    }
    cs_push(C, dl); /* return destination list */
    return 1;
}


static int lst_new(cs_State *C) {
    cs_Unsigned size = (cs_Unsigned)csL_check_integer(C, 0);
    csL_check_arg(C, size <= cast_uint(INT_MAX), 0, "out of range");
    cs_push_list(C, cast_int(size));
    return 1;
}


static int lst_flatten(cs_State *C) {
    cs_Unsigned n;
    cs_Integer e, i, last;
    last = ltoi(checklist(C, 0, 0, 0));
    i = csL_opt_integer(C, 1, 0);
    e = csL_opt(C, csL_check_integer, 2, last);
    check_bounds(C, 1, i, 2, e);
    e = (e > last) ? last : e; /* flatten up to last index */
    if (i > e) return 0; /* empty range */
    n = c_castS2U(e) - c_castS2U(i); /* number of elements minus 1 */
    cs_assert(n < cast_uint(INT_MAX)); /* bounds are already checked */
    if (c_unlikely(!cs_checkstack(C, (int)(++n))))
        return csL_error(C, "too many results");
    for (; i < e; i++) /* push l[i..e - 1] (to avoid overflows in 'i') */
        cs_get_index(C, 0, i);
    cs_get_index(C, 0, e); /* push last element */
    return n;
}


static void addvalue(cs_State *C, csL_Buffer *b, int i) {
    cs_get_index(C, 0, i);
    if (cs_is_number(C, -1)) { /* value is number? */
        char numbuff[CS_N2SBUFFSZ];
        cs_numbertocstring(C, -1, numbuff); /* convert it to string */
        cs_push_string(C, numbuff); /* push it on stack */
        cs_replace(C, -2); /* and replace original value */
    } else if (c_unlikely(!cs_is_string(C, -1))) /* value is not a string? */
        csL_error(C, "cannot concat value (%s) at index %d",
                     csL_typename(C, -1), i);
    csL_buff_push_stack(b);
}


#define getnexti(C,i,e,sn) \
        (!(sn) ? cast_int(i) + 1 \
               : cs_find_index(C, 0, 0, cast_int(i), cast_int(e)))


static int lst_concat(cs_State *C) {
    int next;
    size_t lsep;
    csL_Buffer b;
    cs_Integer l = checklist(C, 0, 0, 0);
    const char *sep = csL_opt_lstring(C, 1, "", &lsep);
    cs_Integer i = csL_opt_integer(C, 2, 0);
    cs_Integer e = csL_opt_integer(C, 3, ltoi(l));
    int skipnil = csL_opt_bool(C, 4, 1);
    check_bounds(C, 2, i, 3, e);
    if (e >= i) { /* otherwise, nothing to concat */
        csL_buff_init(C, &b);
        next = getnexti(C, (i - !skipnil), e, skipnil);
        while (next >= 0 && next < e) { /* l[next .. (next < e)] */
            addvalue(C, &b, next);
            csL_buff_push_lstring(&b, sep, lsep);
            next = getnexti(C, next+1, e, skipnil);
        }
        if (next == e && (!skipnil || cs_find_index(C, 0, 0, next, e) != -1))
            addvalue(C, &b, cast_int(e)); /* add last value */
        csL_buff_end(&b);
    }
    return 1;
}


/* {===================================================================
** Quicksort implementation (based on Lua's implementation of
** 'Algorithms in MODULA-3', Robert Sedgewick, Addison-Wesley, 1993.)
** ==================================================================== */

typedef unsigned int Idx;


#define geti(C,idl,idx)     cs_get_index(C, idl, c_castU2S(idx))
#define seti(C,idl,idx)     cs_set_index(C, idl, c_castU2S(idx))


/*
** Lists larger than 'RANLIMIT' may use randomized pivots.
*/
#define RANLIMIT    100u


/*
** Error for invalid sorting function.
*/
#define FNSORTERR   "invalid order function for sorting"


static void get2(cs_State *C, Idx idx1, Idx idx2) {
    geti(C, 0, idx1);
    geti(C, 0, idx2);
}


static void set2(cs_State *C, Idx idx1, Idx idx2) {
    seti(C, 0, idx1);
    seti(C, 0, idx2);
}


static int sort_cmp(cs_State *C, Idx a, Idx b) {
    if (cs_is_nil(C, 1)) /* no compare function? */
        return cs_compare(C, a, b, CS_ORD_LT);
    else { /* otherwise call compare function */
        int res;
        cs_push(C, 1);          /* push function */
        cs_push(C, a-1);        /* -1 to compensate for function */
        cs_push(C, b-2);        /* -2 to compensate for function and 'a' */
        cs_call(C, 2, 1);       /* call function */
        res = cs_to_bool(C, -1); /* get result */
        cs_pop(C, 1);           /* pop result */
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
static Idx partition(cs_State *C, Idx lo, Idx hi) {
    Idx i = lo; /* will be incremented before first use */
    Idx j = hi - 1; /* will be decremented before first use */
    /* loop invariant: l[lo .. i] <= P <= l[j .. hi] */
    for (;;) {
        /* next loop: repeat ++i while l[i] < P */
        while ((void)geti(C, 0, ++i), sort_cmp(C, -1, -2)) {
            if (c_unlikely(i == hi - 1)) { /* l[hi - 1] < P == l[hi - 1] */
                /* (pivot element can't be less than itself...) */
                csL_error(C, FNSORTERR);
            }
            cs_pop(C, 1); /* remove l[i] */
        }
        /* after the loop, l[i] >= P and l[lo .. i - 1] < P  (l) */
        /* next loop: repeat --j while P < l[j] */
        while ((void)geti(C, 0, --j), sort_cmp(C, -3, -1)) {
            if (c_unlikely(j < i)) { /* j <= i-1 and l[j] > P, contradicts (l) */
                /* (can't have P < l[j] <= l[i-1] and P < l[i-1] */
                csL_error(C, FNSORTERR);
            }
            cs_pop(C, 1); /* remove l[j] */
        }
        /* after the loop, l[j] <= P and l[j + 1 .. hi] >= P */
        if (j < i) { /* no elements out of place? */
            /* l[lo .. i - 1] <= P <= l[j + 1 .. i .. hi] */
            cs_pop(C, 1); /* pop l[j] */
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
    cs_assert(lo + r4 <= p && p <= hi - r4);
    return p;
}


static void auxsort(cs_State *C, Idx lo, Idx hi, unsigned rnd) {
    while (lo < hi) {
        Idx n;
        Idx p; /* pivot index */
        get2(C, lo, hi);
        if (sort_cmp(C, -1, -2)) /* l[hi] < l[lo]? */
            set2(C, lo, hi); /* swap l[hi] with l[lo] */
        else
            cs_pop(C, 2);
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
            cs_pop(C, 1); /* remove l[lo] */
            geti(C, 0, hi);
            if (sort_cmp(C, -1, -2)) /* l[hi] < l[p] */ 
                set2(C, p, hi); /* swap l[hi] with l[p] */
            else
                cs_pop(C, 2);
        }
        if (hi - lo == 2) /* only 3 elements? */
            return; /* done */
        geti(C, 0, p); /* get pivot */
        cs_push(C, -1); /* push pivot */
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


static int lst_sort(cs_State *C) {
    cs_Integer size = checklist(C, 0, 0, csL_opt_bool(C, 2, 1));
    if (size > 1) { /* non trivial? */
        csL_check_arg(C, size <= INT_MAX, 0, "list too big");
        if (!cs_is_noneornil(C, 1)) /* is there a 2nd argument? */
            csL_check_type(C, 1, CS_T_FUNCTION); /* it must be a function */
        cs_setntop(C, 2); /* make sure there are two arguments */
        auxsort(C, 0, (Idx)size-1u, 0);
    }
    return 0;
}

/* }=================================================================== */


static int lst_isordered(cs_State *C) {
    int sorted = 1;
    cs_Integer i, e;
    cs_Integer size = checklist(C, 0, 0, csL_opt_bool(C, 4, 1));
    if (!cs_is_noneornil(C, 1)) /* is there a 2nd argument? */
        csL_check_type(C, 1, CS_T_FUNCTION); /* must be a function */
    i = csL_opt_integer(C, 2, 0); /* start index */
    e = csL_opt_integer(C, 3, size-(size>0)); /* end index */
    check_bounds(C, 2, i, 3, e);
    if (e > i) {
        cs_setntop(C, 2); /* make sure there are two arguments */
        while (sorted && i < e) {
            cs_get_index(C, 0, (int)i);
            cs_get_index(C, 0, (int)i+1);
            sorted = sort_cmp(C, -2, -1);
            cs_pop(C, 2);
            i++;
        }
    }
    cs_push_bool(C, sorted);
    if (!sorted) {
        cs_push_integer(C, i-1);
        return 2; /* return false and first index which breaks ordering */
    }
    return 1; /* return true */
}


/// TODO: add tests and docs
static cs_Entry lstlib[] = {
    {"len", lst_len},
    {"insert", lst_insert},
    {"remove", lst_remove},
    {"move", lst_move},
    {"new", lst_new},
    {"flatten", lst_flatten},
    {"concat", lst_concat},
    {"sort", lst_sort},
    {"isordered", lst_isordered},
    {"maxindex", NULL},
    {NULL, NULL}
};


CSMOD_API int csopen_list(cs_State *C) {
    csL_push_lib(C, lstlib);
    cs_push_integer(C, MAXLISTINDEX);
    cs_set_fieldstr(C, -2, "maxindex");
    return 1;
}
