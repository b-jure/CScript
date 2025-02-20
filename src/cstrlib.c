/*
** cstrlib.c
** Standard library for string operations
** See Copyright Notice in cscript.h
*/

#include <ctype.h>
#define cstrlib_c
#define CS_LIB


#include <float.h>
#include <limits.h>
#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscript.h"

#include "cauxlib.h"
#include "cslib.h"


#define MAX_SIZET   ((size_t)(~(size_t)0))

#define MAXSIZE \
        (sizeof(size_t) < sizeof(int) ? (size_t)INT_MAX : MAX_SIZET)


#define cast_uchar(c)   ((unsigned char)(c))


static const char *find(const char *s, size_t ls, const char *p, size_t lp) {
    const char *aux;
    lp--; /* 'strchr' checks the first char */
    ls -= lp; /* 'p' cannot be found after that */
    while (ls > 0 && (aux = strchr(s, *p)) != NULL) {
        aux++; /* skip first char (already checked) */
        if (memcmp(aux, p+1, lp) == 0)
            return aux-1;
        else {
            ls -= aux-s;
            s = aux;
        }
    }
    return NULL; /* not found */
}


static const void *memrchr(const void *s, int c, size_t n, size_t nmin) {
    const unsigned char *p = s;
    const unsigned char *const e = p+n;
    c = cast_uchar(c);
    n++; /* compensate for loop */
    while (n--)
        if (p[n] == c)
            return ((size_t)(e-(p+n)) >= nmin) ? p+n : NULL;
    return NULL;
}


static const char *rfind(const char *s, size_t ls, const char *p, size_t lp) {
    const char *end = s + ls;
    const char *aux;
    size_t rl = ls; /* real length of 's' */
    lp--; /* 'strrchr' checks the first char */
    ls -= lp; /* 'p' cannot be found after that */
    while (ls > 0 && (aux = memrchr(s, *p, rl, lp+1)) != NULL) {
        if (memcmp(aux+1, p+1, lp) == 0)
            return aux;
        else {
            rl = aux-s;
            ls -= end-aux;
            end = aux;
        }
    }
    return NULL; /* not found */
}


static const char *findstr(const char *s, size_t ls,
                           const char *p, size_t lp, int rev) {
    if (c_unlikely(lp == 0)) return s; /* empty strings match everything */
    else if (ls < lp) return NULL; /* avoid negative 'ls' */
    else if (!rev) return find(s, ls, p, lp); /* regular find */
    else return rfind(s, ls, p, lp); /* reverse find */
}


static int splitintoarray(cs_State *C, int rev) {
    size_t ls, lp;
    const char *s = csL_check_lstring(C, 0, &ls); /* string */
    const char *p = csL_check_lstring(C, 1, &lp); /* pattern */
    cs_Integer n = csL_opt_integer(C, 2, CS_INTEGER_MAX); /* maxsplit */
    const char *aux;
    cs_push_array(C, (n > 0 ? n : 1));
    if (c_unlikely(n <= 0 || lp == 0)) { /* no splits or pattern is '""'? */
        cs_push(C, 0);
        cs_set_index(C, 3, 0);
    } else {
        int i = 0;
        const char *e = s+lp;
        while (n > 0 && (aux = findstr(s, ls, p, lp, rev)) != NULL) {
            if (!rev) { /* regular find? */
                cs_push_lstring(C, s, s-aux);
                ls -= (aux+lp)-s;
                s = aux+lp;
            } else { /* reverse find */
                cs_push_lstring(C, aux+lp, e-(aux+lp));
                e = aux;
                ls = aux-s;
            }
            cs_set_index(C, 3, i);
            n--; i++;
        }
        if (n > 0) { /* maxsplit not reached? */
            cs_push_lstring(C, s, ls); /* push last piece */
            cs_set_index(C, 3, i);
        }
    }
    return 1; /* return array */
}


static int s_split(cs_State *C) {
    return splitintoarray(C, 0);
}


static int s_rsplit(cs_State *C) {
    return splitintoarray(C, 1);
}


/*
** Translate relative string index to absolute.
*/
static size_t r2a(cs_Integer pos, size_t len) {
    if (pos >= 0) /* already absolute? */
        return pos;
    else if (pos < -(cs_Integer)len) /* negative out-of-bounds 'pos'? */
        return 0; /* clip it */
    else /* otherwise negative in-range 'pos' */
        return len + (size_t)pos;
}


static int s_startswith(cs_State *C) {
    size_t l1, l2;
    const char *s1 = csL_check_lstring(C, 0, &l1);
    const char *s2 = csL_check_lstring(C, 1, &l2);
    size_t i = r2a(csL_opt_integer(C, 2, 0), l1);
    size_t j = r2a(csL_opt_integer(C, 3, -1), l1);
    if (i < l1) {
        size_t k = 0;
        while (i <= j && k < l2 && s1[i] == s2[k]) { i++; k++; }
        if (k+1 == l2) {
            cs_push_integer(C, i);
            return 1; /* return index after 's2' in 's1' */
        } /* else fall through */
    } /* else fall through */
    csL_push_fail(C);
    return 1; /* return fail */
}


static int s_reverse(cs_State *C) {
    size_t l, i;
    csL_Buffer b;
    const char *s = csL_check_lstring(C, 0, &l);
    char *p = csL_buff_initsz(C, &b, l);
    for (i = 0; i < l; i++)
        p[i] = s[l - i - 1];
    csL_buff_endsz(&b, l);
    return 1; /* return string */
}


static int s_repeat(cs_State *C) {
    size_t l, lsep;
    const char *s = csL_check_lstring(C, 0, &l);
    cs_Integer n = csL_check_integer(C, 1);
    const char *sep = csL_opt_lstring(C, 2, "", &lsep);
    if (c_unlikely(n <= 0))
        cs_push_literal(C, "");
    else if (l+lsep < l || l+lsep > MAXSIZE/n)
        csL_error(C, "resulting string too large");
    else {
        size_t totalsize = (l+lsep) * n;
        csL_Buffer b;
        char *p = csL_buff_initsz(C, &b, totalsize);
        while (n-- > 1) {
            memcpy(p, s, l*sizeof(char)); p += l;
            if (lsep > 0) { /* branch costs less than empty 'memcpy' copium */
                memcpy(p, sep, lsep*sizeof(char));
                p += lsep;
            }
        }
        memcpy(p, s, l*sizeof(char)); /* last copy without separator */
        csL_buff_endsz(&b, totalsize);
    }
    return 1; /* return final string */
}


static void auxjoinstr(cs_State *C, csL_Buffer *b,
                       const char *sep, size_t lsep) {
    size_t l;
    const char *s = cs_to_lstring(C, -1, &l);
    if (s && l > 0) { /* value is a non empty string? */
        csL_buff_push_lstring(b, s, l);
        if (lsep > 0) /* non empty separator? */
            csL_buff_push_lstring(b, sep, lsep);
    }
    cs_pop(C, 1); /* remove the value */
}


static void joinfromtable(cs_State *C, csL_Buffer *b,
                          const char *sep, size_t lsep) {
    cs_push_nil(C);
    while (cs_next(C, 1) != 0)
        auxjoinstr(C, b, sep, lsep);
}


static void joinfromarray(cs_State *C, csL_Buffer *b,
                          const char *sep, size_t lsep, int len) {
    int i = cs_get_nnilindex(C, 1, 0, --len);
    while (i >= 0) {
        cs_get_index(C, 1, i);
        auxjoinstr(C, b, sep, lsep);
        i = cs_get_nnilindex(C, 1, ++i, len);
    }
}


static int s_join(cs_State *C) {
    size_t lsep;
    const char *sep = csL_check_lstring(C, 0, &lsep);
    int t = cs_type(C, 1);
    csL_expect_arg(C, (t == CS_TARRAY || t == CS_TTABLE), 0, "array or table");
    csL_Buffer b;
    csL_buff_init(C, &b);
    if (t == CS_TARRAY) {
        int len = cs_len(C, 1);
        if (len > 0)
            joinfromarray(C, &b, sep, lsep, len);
    } else
        joinfromtable(C, &b, sep, lsep);
    if (csL_bufflen(&b) > 0 && lsep > 0) /* buffer has not empty separator? */
        csL_buffsub(&b, lsep); /* remove it */
    csL_buff_end(&b);
    return 1; /* return final string */
}


// string.format(string_fmt, ...)
static int s_format(cs_State *C) {
    return 1;
}


static int auxtocase(cs_State *C, int (*f)(int c)) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    size_t i = r2a(csL_opt_integer(C, 1, 0), l);
    size_t j = r2a(csL_opt_integer(C, 2, -1), l);
    if (l == 0 || i >= l || i > j)
        cs_push_literal(C, "");
    else {
        csL_Buffer b;
        size_t sz = (j-i)+1;
        char *p = csL_buff_initsz(C, &b, sz);
        if (j >= l) j = l-1; /* clip */
        for (; i <= j; i++)
            p[i] = f(s[i]);
        csL_buff_endsz(&b, sz);
    }
    return 1; /* return final string */
}


static int s_toupper(cs_State *C) {
    return auxtocase(C, toupper);
}


static int s_tolower(cs_State *C) {
    return auxtocase(C, tolower);
}


static int s_count(cs_State *C) {
    size_t l, lpat;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *pat = csL_check_lstring(C, 1, &lpat);
    size_t i = r2a(csL_opt_integer(C, 2, 0), l);
    size_t j = r2a(csL_opt_integer(C, 3, -1), l);
    if (l == 0 || lpat > l || i >= l || i > j)
        cs_push_integer(C, 0);
    else {
        size_t count = 0;
        const char *aux;
        s = s+i;
        l = (j-i) + 1;
        while ((aux = findstr(s, l, pat, lpat, 0)) != NULL) {
            aux++;
            s = aux;
            l -= aux-s;
            count++;
        }
        cs_push_integer(C, count);
    }
    return 1; /* return the count */
}


// find(string, string_pattern, [, start, end])
static int s_find(cs_State *C) {
    size_t l, lpat;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *pat = csL_check_lstring(C, 1, &lpat);
    size_t i = r2a(csL_opt_integer(C, 2, 0), l);
    size_t j = r2a(csL_opt_integer(C, 3, -1), l);

    return 1;
}


static int s_rfind(cs_State *C) {
    return 1;
}


static int s_replace(cs_State *C) {
    return 1;
}


static int s_substr(cs_State *C) {
    return 1;
}


static int s_swapcase(cs_State *C) {
    return 1;
}


static int s_swapupper(cs_State *C) {
    return 1;
}


static int s_swaplower(cs_State *C) {
    return 1;
}



static const cs_Entry strlib[] = {
    {"split", s_split},
    {"rsplit", s_rsplit},
    {"startswith", s_startswith},
    {"reverse", s_reverse},
    {"repeat", s_repeat},
    {"join", s_join},
    {"format", s_format},
    {"toupper", s_toupper},
    {"tolower", s_tolower},
    {"count", s_count},
    {"find", s_find},
    {"rfind", s_rfind},
    {"replace", s_replace},
    {"substr", s_substr},
    {"swapcase", s_swapcase},
    {"swapupper", s_swapupper},
    {"swaplower", s_swaplower},
    {NULL, NULL}
};
/*
   string.rfind(string, string_pattern [, start, end]) // default: start = 0, end = -1
   string.replace(string, string_pattern, value [, n_times]) // default: n_times = nil
   string.substr(string, start [, end]) // default: end = -1
   string.swapcase(string)
   string.swapupper(string)
   string.swaplower(string)
*/


CSMOD_API int luaopen_string (cs_State *C) {
    csL_newlib(C, strlib);
    return 1;
}
