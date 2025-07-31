/*
** tstrlib.h
** Common header for string and pattern-matching library.
** See Copyright Notice in tokudae.h
*/

#ifndef tstrlib_h
#define tstrlib_h


#if !defined(tstrlib_c) && !defined(treglib_c)
#error Only string and pattern-matching library can include this header file.
#endif


#include <stddef.h>
#include <string.h>

#include "tokudae.h"
#include "tokudaelimits.h"


#define uchar(c)    cast_ubyte(c)


/*
** Translate relative starting position to absolute slice index.
*/
static size_t posrelStart(toku_Integer pos, size_t len) {
    if (pos >= 0) /* already absolute? */
        return cast_sizet(pos);
    else if (pos < -(toku_Integer)len) /* negative out-of-bounds 'pos'? */
        return 0; /* clip to 0 */
    else /* otherwise negative in-range 'pos' */
        return len + cast_sizet(pos);
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
            l -= cast_diff2sz(aux-s);
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
