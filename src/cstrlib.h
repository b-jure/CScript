/*
** cstrlib.h
** Common header for string and pattern-matching library.
** See Copyright Notice in cscript.h
*/

#ifndef cstrlib_h
#define cstrlib_h


#if !defined(cstrlib_c) && !defined(creglib_c)
#error Only string or pattern-matching library can include this header file.
#endif


#include <stddef.h>
#include <string.h>

#include "cscript.h"



/* macro to 'unsign' a character */
#define uchar(c)	((unsigned char)(c))


/*
** Some sizes are better limited to fit in 'int', but must also fit in
** 'size_t'. (We assume that 'cs_Integer' cannot be smaller than 'int'.)
*/
#define MAX_SIZET	((size_t)(~(size_t)0))

#define STR_MAXSIZE \
	(sizeof(size_t) < sizeof(int) ? MAX_SIZET : (size_t)(INT_MAX))




/* Translate relative starting position to absolute slice index. */
static size_t posrelStart(cs_Integer pos, size_t len) {
    if (pos >= 0) /* already absolute? */
        return (size_t)pos;
    else if (pos < -(cs_Integer)len) /* negative out-of-bounds 'pos'? */
        return 0; /* clip to 0 */
    else /* otherwise negative in-range 'pos' */
        return len + (size_t)pos;
}


static const char *sfind(const char *s, size_t l, const char *p, size_t lp) {
    const char *aux;
    lp--; /* 'memchr' checks the first char */
    l -= lp; /* 'p' cannot be found after that */
    while (l > 0 && (aux = (const char *)memchr(s, *p, l)) != NULL) {
        aux++; /* skip first char (already checked) */
        if (memcmp(aux, p+1, lp) == 0)
            return aux-1; /* found */
        else {
            l -= aux-s;
            s = aux;
        }
    }
    return NULL; /* not found */
}


static const char *rsfind(const char *s, size_t l, const char *p, size_t lp) {
    const char *start = (s + l) - lp;
    lp--; /* first char is checked */
    while (start >= s) {
        if (*start == *p && memcmp(start + 1, p + 1, lp) == 0)
            return start;
        start--;
    }
    return NULL; /* not found */
}


/* find pattern 'pat' in 's' */
static const char *findstr(const char *s, size_t l,
                           const char *pat, size_t lpat, int rev) {
    if (lpat == 0) return s; /* empty strings match everything */
    else if (l < lpat) return NULL; /* avoid negative 'l' */
    else return (!rev) ? sfind(s, l, pat, lpat) : rsfind(s, l, pat, lpat);
}


#endif
