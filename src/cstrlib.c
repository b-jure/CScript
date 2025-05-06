/*
** cstrlib.c
** Standard library for string operations
** See Copyright Notice in cscript.h
*/

#define cstrlib_c
#define CS_LIB

#include "cprefix.h"

#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscript.h"

#include "cstrlib.h"
#include "cscriptaux.h"
#include "cscriptlib.h"
#include "climits.h"


#if !defined(CS_BYTES)

#define CS_BYTES

#define CS_BYTES_UPPERCASE      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define CS_BYTES_LOWERCASE      "abcdefghijklmnopqrstuvwxyz"
#define CS_BYTES_LETTERS        CS_BYTES_UPPERCASE CS_BYTES_LOWERCASE

#define CS_BYTES_OCTDIGITS      "01234567"
#define CS_BYTES_DIGITS         CS_BYTES_OCTDIGITS "89"
#define CS_BYTES_HEXDIGITS      CS_BYTES_DIGITS "abcdef"

#define CS_BYTES_PUNCTUATION    "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

#define CS_BYTES_WHITESPACE     " \t\n\r\v\f"

#define CS_BYTES_PRINTABLE      CS_BYTES_DIGITS CS_BYTES_LETTERS \
                                CS_BYTES_PUNCTUATION CS_BYTES_WHITESPACE

#endif


/* Translate relative end position to absolute slice index. */
static size_t posrelEnd(cs_Integer pos, size_t len) {
    if (pos >= (cs_Integer)len) /* absolute that overflows? */
        return (size_t)(len - 1);
    else if (pos >= 0) /* absolute in-range 'pos'? */
        return pos;
    else if (pos < -(cs_Integer)len) /* negative out-of-bounds 'pos'? */
        return 0; /* clip it */
    else /* otherwise negative in-range 'pos' */
        return len + (size_t)pos;
}


static const char *skipws(const char *s, size_t *pl, int rev) {
    const unsigned char *p;
    size_t l = *pl;
    if (l == 0) return NULL;
    else if (!rev) { /* from 's'? */
        p = (const unsigned char *)s;
        while (l > 0 && isspace(*p)) { p++; l--; }
    } else { /* reverse */
        p = ((const unsigned char *)s + l) - 1;
        while (l > 0 && isspace(*p)) { p--; l--; }
    }
    *pl = l;
    return (l > 0 ? (const char *)p : NULL);
}


static int split_into_list(cs_State *C, int rev) {
    size_t ls;
    const char *s = csL_check_lstring(C, 0, &ls); /* string */
    cs_Integer n = csL_opt_integer(C, 2, CS_INTEGER_MAX-1); /* maxsplit */
    int arr = cs_getntop(C);
    int i = 0;
    const char *aux;
    if (cs_is_noneornil(C, 1)) { /* split by whitespace? */
        cs_push_list(C, 1);
        if (!rev)
            while (ls > 0 && isspace(uchar(*s))) { s++; ls--; }
        else {
            aux = (s + ls) - 1;
            while (ls > 0 && isspace(uchar(*aux))) { aux--; ls--; }
        }
        while (n > 0 && (aux = skipws(s, &ls, rev)) != NULL) {
            size_t lw = 0;
            const char *p = aux;
            if (!rev)
                while (ls > 0 && !isspace(uchar(*aux)))
                { ls--; lw++; aux++; }
            else { /* reverse */
                while (ls > 0 && !isspace(uchar(*aux)))
                { ls--; lw++; aux--; }
                p = aux+1;
            }
            cs_assert(lw > 0);
            cs_push_lstring(C, p, lw);
            cs_set_index(C, arr, i);
            if (!rev) s = aux;
            n--; i++;
        }
        if (!rev)
            while (ls > 0 && isspace(uchar(*s))) { s++; ls--; }
        else {
            aux = (s + ls) - 1;
            while (ls > 0 && isspace(uchar(*aux))) { aux--; ls--; }
        }
        if (ls == 0) return 1; /* done */
    } else { /* else split by pattern */
        size_t lpat;
        const char *pat = csL_check_lstring(C, 1, &lpat);
        const char *e = s+ls;
        cs_push_list(C, 1);
        if (n < 1 || lpat == 0) goto pushs;
        while (n > 0 && (aux = findstr(s, ls, pat, lpat, rev)) != NULL) {
            if (!rev) { /* find from start? */
                cs_push_lstring(C, s, aux-s);
                ls -= (aux+lpat)-s;
                s = aux+lpat;
            } else { /* reverse find */
                cs_push_lstring(C, aux+lpat, e-(aux+lpat));
                e = aux;
                ls = aux-s;
            }
            cs_set_index(C, arr, i);
            n--; i++;
        }
    }
pushs:
    cs_push_lstring(C, s, ls); /* push last piece */
    cs_set_index(C, arr, i);
    return 1; /* return list */
}


static int s_split(cs_State *C) {
    return split_into_list(C, 0);
}


static int s_rsplit(cs_State *C) {
    return split_into_list(C, 1);
}


static int s_startswith(cs_State *C) {
    size_t l, l1;
    const char *s1 = csL_check_lstring(C, 0, &l);
    const char *s2 = csL_check_lstring(C, 1, &l1);
    size_t i = posrelStart(csL_opt_integer(C, 2, 0), l);
    size_t j = posrelEnd(csL_opt_integer(C, 3, -1), l);
    if (i <= j && l1 <= (j-i) + 1) {
        size_t k = 0;
        while (i <= j && k < l1 && s1[i] == s2[k]) { i++; k++; }
        cs_push_integer(C, i);
        if (k == l1) {
            return 1; /* return index after 's2' in 's1' */
        } else {
            csL_push_fail(C);
            cs_insert(C, -2);
        }
    } else {
        csL_push_fail(C);
        cs_push_integer(C, -1); /* invalid range */
    }
    return 2; /* return fail and invalid or non-matching index */
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
    else if (l+lsep < l || l+lsep > STR_MAXSIZE/n)
        csL_error(C, "resulting string too large");
    else {
        size_t totalsize = (size_t)n*l + (size_t)(n-1)*lsep;
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


static void auxjoinstr(csL_Buffer *b, const char *s, size_t l,
                                      const char *sep, size_t lsep) {
    csL_buff_push_lstring(b, s, l);
    if (lsep > 0) /* non empty separator? */
        csL_buff_push_lstring(b, sep, lsep);
}


static void joinfromtable(cs_State *C, csL_Buffer *b,
                          const char *sep, size_t lsep) {
    cs_push_nil(C);
    while (cs_next(C, 1) != 0) {
        size_t l;
        const char *s = cs_to_lstring(C, -1, &l);
        int pop = 1; /* value */
        if (s && l > 0) {
            cs_push(C, -3); /* push buffer */
            auxjoinstr(b, s, l, sep, lsep);
            pop++; /* buffer */
        }
        cs_pop(C, pop);
    }
}


static void joinfromlist(cs_State *C, csL_Buffer *b,
                          const char *sep, size_t lsep, int len) {
    int i = cs_find_nnilindex(C, 1, 0, --len);
    while (i >= 0) {
        size_t l;
        const char *s;
        cs_get_index(C, 1, i);
        s = cs_to_lstring(C, -1, &l);
        cs_pop(C, 1);
        if (s && l > 0)
            auxjoinstr(b, s, l, sep, lsep);
        i = cs_find_nnilindex(C, 1, ++i, len);
    }
}


static int s_join(cs_State *C) {
    size_t lsep;
    const char *sep = csL_check_lstring(C, 0, &lsep);
    int t = cs_type(C, 1);
    csL_Buffer b;
    csL_expect_arg(C, (t == CS_TLIST || t == CS_TTABLE), 1, "list/table");
    csL_buff_init(C, &b);
    if (t == CS_TLIST) {
        int len = cs_len(C, 1);
        if (len > 0)
            joinfromlist(C, &b, sep, lsep, len);
    } else
        joinfromtable(C, &b, sep, lsep);
    if (csL_bufflen(&b) > 0 && lsep > 0) /* buffer has separator? */
        csL_buffsub(&b, lsep); /* remove it */
    csL_buff_end(&b);
    return 1; /* return final string */
}


/*
** {======================================================
** STRING FORMAT
** =======================================================
*/


/*
** Maximum size for items formatted with '%f'. This size is produced
** by format('%.99f', -maxfloat), and is equal to 99 + 3 ('-', '.',
** and '\0') + number of decimal digits to represent maxfloat (which
** is maximum exponent + 1). (99+3+1, adding some extra, 110)
*/
#define MAX_ITEMF	(110 + c_floatatt(MAX_10_EXP))


/*
** All formats except '%f' do not need that large limit.  The other
** float formats use exponents, so that they fit in the 99 limit for
** significant digits; 's' for large strings and 'q' add items directly
** to the buffer; all integer formats also fit in the 99 limit.  The
** worst case are floats: they may need 99 significant digits, plus
** '0x', '-', '.', 'e+XXXX', and '\0'. Adding some extra, 120.
*/
#define MAX_ITEM    120


#if !defined(C_FMTC)

/* conversion specification introducer */
#define C_FMTC          '%'

/* valid flags for a, A, e, E, f, F, g, and G conversions */
#define C_FMTFLAGSF     "-+#0 "

/* valid flags for o, x, and X conversions */
#define C_FMTFLAGSX	"-#0"

/* valid flags for d and i conversions */
#define C_FMTFLAGSI	"-+0 "

/* valid flags for u conversions */
#define C_FMTFLAGSU	"-0"

/* valid flags for c, p, and s conversions */
#define C_FMTFLAGSC	"-"

#endif


/*
** Maximum size of each format specification (such as "%-099.99d"):
** Initial '%', flags (up to 5), width (2), period, precision (2),
** length modifier (8), conversion specifier, and final '\0', plus some
** extra.
*/
#define MAX_FORMAT	32



static void addquoted(csL_Buffer *b, const char *s, size_t len) {
    csL_buff_push(b, '"');
    while (len--) {
        if (*s == '"' || *s == '\\' || *s == '\n') {
            csL_buff_push(b, '\\');
            csL_buff_push(b, *s);
        }
        else if (iscntrl(uchar(*s))) {
            char buff[10];
            if (!isdigit(uchar(*(s+1))))
                c_snprintf(buff, sizeof(buff), "\\%d", (int)uchar(*s));
            else
                c_snprintf(buff, sizeof(buff), "\\%03d", (int)uchar(*s));
            csL_buff_push_string(b, buff);
        }
        else
            csL_buff_push(b, *s);
        s++;
    }
    csL_buff_push(b, '"');
}


/*
** Serialize a floating-point number in such a way that it can be
** scanned back by Lua. Use hexadecimal format for "common" numbers
** (to preserve precision); inf, -inf, and NaN are handled separately.
** (NaN cannot be expressed as a numeral, so we write '(0/0)' for it.)
*/
static int quotefloat(cs_State *C, char *buff, cs_Number n) {
    const char *s; /* for the fixed representations */
    if (n == (cs_Number)HUGE_VAL) /* inf? */
        s = "1e9999";
    else if (n == -(cs_Number)HUGE_VAL) /* -inf? */
        s = "-1e9999";
    else if (n != n) /* NaN? */
        s = "(0/0)";
    else { /* format number as hexadecimal */
        int  nb = cs_number2strx(C, buff, MAX_ITEM, "%"CS_NUMBER_FMTLEN"a", n);
        /* ensures that 'buff' string uses a dot as the radix character */
        if (memchr(buff, '.', nb) == NULL) { /* no dot? */
            char point = cs_getlocaledecpoint(); /* try locale point */
            char *ppoint = (char *)memchr(buff, point, nb);
            if (ppoint) *ppoint = '.'; /* change it to a dot */
        }
        return nb;
    }
    /* for the fixed representations */
    return c_snprintf(buff, MAX_ITEM, "%s", s);
}


static void addliteral(cs_State *C, csL_Buffer *b, int arg) {
    switch (cs_type(C, arg)) {
        case CS_TSTRING: {
            size_t len;
            const char *s = cs_to_lstring(C, arg, &len);
            addquoted(b, s, len);
            break;
        }
        case CS_TNUMBER: {
            char *buff = csL_buff_ensure(b, MAX_ITEM);
            int nb;
            if (!cs_is_integer(C, arg)) /* float? */
                nb = quotefloat(C, buff, cs_to_number(C, arg));
            else { /* integers */
                cs_Integer n = cs_to_integer(C, arg);
                const char *format = (n == CS_INTEGER_MIN) /* corner case? */
                    ? "0x%" CS_INTEGER_FMTLEN "x" /* use hex */
                    : CS_INTEGER_FMT; /* else use default format */
                nb = c_snprintf(buff, MAX_ITEM, format, (cs_Integer)n);
            }
            csL_buffadd(b, nb);
            break;
        }
        case CS_TNIL: case CS_TBOOL: {
            csL_to_lstring(C, arg, NULL);
            csL_buff_push_stack(b);
            break;
        }
        default:
            csL_error_arg(C, arg, "value has no literal form");
    }
}


static const char *get2digits (const char *s) {
    if (isdigit(uchar(*s))) {
        s++;
        if (isdigit(uchar(*s))) s++; /* (2 digits at most) */
    }
    return s;
}


/*
** Check whether a conversion specification is valid. When called,
** first character in 'form' must be '%' and last character must
** be a valid conversion specifier. 'flags' are the accepted flags;
** 'precision' signals whether to accept a precision.
*/
static void checkformat(cs_State *C, const char *form, const char *flags,
                        int precision) {
    const char *spec = form + 1; /* skip '%' */
    spec += strspn(spec, flags); /* skip flags */
    if (*spec != '0') { /* a width cannot start with '0' */
        spec = get2digits(spec); /* skip width */
        if (*spec == '.' && precision) {
            spec++;
            spec = get2digits(spec); /* skip precision */
        }
    }
    if (!isalpha(uchar(*spec))) /* did not go to the end? */
        csL_error(C, "invalid conversion specification: '%s'", form);
}


/*
** Get a conversion specification and copy it to 'form'.
** Return the address of its last character.
*/
static const char *getformat(cs_State *C, const char *strfrmt, char *form) {
    /* spans flags, width, and precision ('0' is included as a flag) */
    size_t len = strspn(strfrmt, C_FMTFLAGSF "123456789.");
    len++;  /* adds following character (should be the specifier) */
    /* still needs space for '%', '\0', plus a length modifier */
    if (len >= MAX_FORMAT - 10)
        csL_error(C, "invalid format (too long)");
    *(form++) = '%';
    memcpy(form, strfrmt, len*sizeof(char));
    *(form + len) = '\0';
    return strfrmt + len - 1;
}


/*
** Add length modifier into formats.
*/
static void addlenmod(char *form, const char *lenmod) {
    size_t l = strlen(form);
    size_t lm = strlen(lenmod);
    char spec = form[l - 1];
    strcpy(form + l - 1, lenmod);
    form[l + lm - 1] = spec;
    form[l + lm] = '\0';
}


static int formatstr(cs_State *C, const char *fmt, size_t lfmt) {
    int top = cs_gettop(C);
    int arg = 0;
    const char *efmt = fmt + lfmt;
    const char *flags;
    csL_Buffer b;
    csL_buff_init(C, &b);
    while (fmt < efmt) {
        if (*fmt != C_FMTC) { /* not % */
            csL_buff_push(&b, *fmt++);
            continue;
        } else if (*++fmt == C_FMTC) { /* %% */
            csL_buff_push(&b, *fmt++);
            continue;
        } /* else '%' */
        char form[MAX_FORMAT]; /* to store the format ('%...') */
        int maxitem = MAX_ITEM; /* maximum length for the result */
        char *buff = csL_buff_ensure(&b, maxitem); /* to put result */
        int nb = 0; /* number of bytes in result */
        if (++arg > top) /* too many format specifiers? */
            return csL_error_arg(C, arg, "missing format value");
        fmt = getformat(C, fmt, form);
        switch (*fmt++) {
            case 'c': {
                checkformat(C, form, C_FMTFLAGSC, 0);
                nb = c_snprintf(buff, maxitem, form, (int)csL_check_integer(C, arg));
                break;
            }
            case 'd': case 'i':
                flags = C_FMTFLAGSI;
                goto intcase;
            case 'u':
                flags = C_FMTFLAGSU;
                goto intcase;
            case 'o': case 'x': case 'X':
                flags = C_FMTFLAGSX;
            intcase: {
                cs_Integer n = csL_check_integer(C, arg);
                checkformat(C, form, flags, 1);
                addlenmod(form, CS_INTEGER_FMTLEN);
                nb = c_snprintf(buff, maxitem, form, (CS_INTEGER)n);
                break;
            }
            case 'a': case 'A':
                checkformat(C, form, C_FMTFLAGSF, 1);
                addlenmod(form, CS_NUMBER_FMTLEN);
                nb = cs_number2strx(C, buff, maxitem, form, csL_check_number(C, arg));
                break;
            case 'f':
                     maxitem = MAX_ITEMF; /* extra space for '%f' */
                     buff = csL_buff_ensure(&b, maxitem);
                     /* fall through */
            case 'e': case 'E': case 'g': case 'G': {
                cs_Number n = csL_check_number(C, arg);
                checkformat(C, form, C_FMTFLAGSF, 1);
                addlenmod(form, CS_NUMBER_FMTLEN);
                nb = c_snprintf(buff, maxitem, form, (CS_NUMBER)n);
                break;
            }
            case 'p': {
                const void *p = cs_to_pointer(C, arg);
                checkformat(C, form, C_FMTFLAGSC, 0);
                if (p == NULL) { /* avoid calling 'printf' with NULL */
                    p = "(null)"; /* result */
                    form[strlen(form) - 1] = 's'; /* format it as a string */
                }
                nb = c_snprintf(buff, maxitem, form, p);
                break;
            }
            case 'q': {
                if (form[2] != '\0') /* modifiers? */
                    return csL_error(C, "specifier '%%q' cannot have modifiers");
                addliteral(C, &b, arg);
                break;
            }
            case 's': {
                size_t l;
                const char *s = csL_to_lstring(C, arg, &l);
                if (form[2] == '\0') /* no modifiers? */
                    csL_buff_push_stack(&b); /* keep entire string */
                else {
                    csL_check_arg(C, l == strlen(s), arg, "string contains zeros");
                    checkformat(C, form, C_FMTFLAGSC, 1);
                    if (strchr(form, '.') == NULL && l >= 100) {
                        /* no precision and string is too long to be formatted */
                        csL_buff_push_stack(&b); /* keep entire string */
                    }
                    else { /* format the string into 'buff' */
                        nb = c_snprintf(buff, maxitem, form, s);
                        cs_pop(C, 1); /* remove result from 'csL_tolstring' */
                    }
                }
                break;
            }
            default: { /* also treat cases 'pnLlh' */
                return csL_error(C, "invalid conversion '%s' to 'format'", form);
            }
        }
        cs_assert(nb < maxitem);
        csL_buffadd(&b, nb);
    }
    csL_buff_end(&b);
    return 1; /* return formatted string */
}


static int s_fmt(cs_State *C) {
    size_t lfmt;
    const char *fmt = csL_check_lstring(C, 0, &lfmt);
    if (lfmt == 0) {
        cs_push_literal(C, "");
        return 1;
    }
    return formatstr(C, fmt, lfmt);
}

/* }====================================================== */


static int auxtocase(cs_State *C, int (*f)(int c)) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    size_t i = posrelStart(csL_opt_integer(C, 1, 0), l);
    size_t j = posrelEnd(csL_opt_integer(C, 2, -1), l);
    if (l == 0 || i > j)
        cs_push(C, 0);
    else {
        csL_Buffer b;
        char *p = csL_buff_initsz(C, &b, l);
        memcpy(p, s, i);
        for (; i <= j; i++)
            p[i] = f(s[i]);
        j++;
        memcpy(p+j, s+j, l-j);
        csL_buff_endsz(&b, l);
    }
    return 1; /* return final string */
}


static int s_toupper(cs_State *C) {
    return auxtocase(C, toupper);
}


static int s_tolower(cs_State *C) {
    return auxtocase(C, tolower);
}


static int auxfind(cs_State *C, int rev) {
    size_t l, lpat;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *pat = csL_check_lstring(C, 1, &lpat);
    size_t i = posrelStart(csL_opt_integer(C, 2, 0), l);
    size_t j = posrelEnd(csL_opt_integer(C, 3, -1), l);
    const char *p;
    if (i <= j && (p = findstr(s+i, (j-i) + 1, pat, lpat, rev)))
        cs_push_integer(C, p-s); /* start index */
    else
        csL_push_fail(C); /* nothing was found */
    return 1;
}


static int s_find(cs_State *C) {
    return auxfind(C, 0);
}


static int s_rfind(cs_State *C) {
    return auxfind(C, 1);
}


static int aux_span(cs_State *C, int complement) {
    size_t l, lb;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *b = csL_check_lstring(C, 1, &lb);
    size_t i = posrelStart(csL_opt_integer(C, 2, 0), l);
    size_t j = posrelEnd(csL_opt_integer(C, 3, -1), l);
    size_t starti = i;
    if (i > j) { /* 'i' too large? */
        csL_push_fail(C);
        return 1;
    } else if (complement) { /* strcspn */
        if (lb == 0) { /* 'b' is empty string? */
            i = l - i; /* whole segment is valid */
            goto pushlen;
        }
        while (i <= j) {
            for (uint k = 0; k < lb; k++)
                if (s[i] == b[k]) goto pushlen;
            i++;
        }
    } else { /* strspn */
        if (lb == 0) { /* 'b' is empty string? */
            i = starti; /* 0 */
            goto pushlen;
        }
        while (i <= j) {
            for (uint k = 0; k < lb; k++)
                if (s[i] == b[k]) goto nextc;
            break; /* push segment len */
        nextc:
            i++;
        }
    }
pushlen:
    /* return computed segment length (span) */
    cs_push_integer(C, (i - starti));
    return 1;
}


static int s_span(cs_State *C) {
    return aux_span(C, 0);
}


static int s_cspan(cs_State *C) {
    return aux_span(C, 1);
}


static int s_replace(cs_State *C) {
    size_t l, lpat, lv;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *pat = csL_check_lstring(C, 1, &lpat);
    const char *v = csL_check_lstring(C, 2, &lv);
    cs_Integer n = csL_opt_integer(C, 3, CS_INTEGER_MAX);
    if (n <= 0) /* no replacements? */
        cs_push(C, 0); /* return original string */
    else if (lpat == 0) /* pattern is empty string? */
        cs_push_lstring(C, v, lv); /* return replacement string */
    else {
        csL_Buffer b;
        const char *p;
        csL_buff_init(C, &b);
        while (n > 0 && (p = findstr(s, l, pat, lpat, 0))) {
            size_t sz = p - s;
            csL_buff_push_lstring(&b, s, sz); /* push prefix */
            csL_buff_push_lstring(&b, v, lv); /* push replacement text */
            l -= sz + lpat; /* subtract prefix and pattern length */
            s = p + lpat; /* go after the pattern */
            n--; /* one less replacement to do */
        }
        csL_buff_push_lstring(&b, s, l); /* push remaining string */
        csL_buff_end(&b);
    }
    return 1; /* return final string */
}


static int s_substr(cs_State *C) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    size_t i = posrelStart(csL_check_integer(C, 1), l);
    size_t j = posrelEnd(csL_opt_integer(C, 2, -1), l);
    if (i <= j)
        cs_push_lstring(C, s + i, (j - i) + 1);
    else
        cs_push_literal(C, "");
    return 1; /* return final substring */
}


static void auxswapcase(char *d, const char *s, size_t i, size_t j) {
    for (; i <= j; i++) {
        unsigned char c = s[i];
        d[i] = isupper(c) ? tolower(c) : toupper(c);
    }
}


static void auxuppercase(char *d, const char *s, size_t i, size_t j) {
    for (; i <= j; i++) {
        unsigned char c = s[i];
        d[i] = isupper(c) ? tolower(c) : c;
    }
}


static void auxlowercase(char *d, const char *s, size_t i, size_t j) {
    for (; i <= j; i++) {
        unsigned char c = s[i];
        d[i] = islower(c) ? toupper(c) : c;
    }
}


static int auxcase(cs_State *C, void (*f)(char*,const char*,size_t,size_t)) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    size_t i = posrelStart(csL_opt_integer(C, 1, 0), l);
    size_t j = posrelEnd(csL_opt_integer(C, 2, -1), l);
    char *p;
    csL_Buffer b;
    if (i > j || l == 0) {
        cs_push(C, 0);
        return 1;
    }
    p = csL_buff_initsz(C, &b, l);
    memcpy(p, s, i);
    f(p, s, i, j);
    j++; /* go past the last index that was swapped */
    memcpy(p+j, s+j, l-j);
    csL_buff_endsz(&b, l);
    return 1;
}


static int s_swapcase(cs_State *C) {
    return auxcase(C, auxswapcase);
}


static int s_swapupper(cs_State *C) {
    return auxcase(C, auxuppercase);
}


static int s_swaplower(cs_State *C) {
    return auxcase(C, auxlowercase);
}


static int getbytes_list(cs_State *C, const char *s, size_t i, size_t j) {
    int n = (int)(j - i) + 1;
    cs_push_list(C, n);
    for (int k = 0; k < n; k++) {
        cs_push_integer(C, uchar(s[i + cast_uint(k)]));
        cs_set_index(C, -2, k);
    }
    return 1; /* return list */
}


static int getbytes_bytes(cs_State *C, const char *s, size_t i, size_t j) {
    int n = (int)(j - i) + 1;
    csL_check_stack(C, n, "string slice too long");
    for (int k = 0; k < n; k++)
        cs_push_integer(C, uchar(s[i + cast_uint(k)]));
    return n; /* return 'n' bytes */
}


static int auxgetbytes(cs_State *C, int pack) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    size_t i = posrelStart(csL_opt_integer(C, 1, 0), l);
    size_t j = posrelEnd(csL_opt_integer(C, 2, (pack) ? -1 : (cs_Integer)i), l);
    if (i > j || l == 0) /* empty interval? */
        return 0; /* return no values */
    else if (c_unlikely((j-i)+1 <= (j-i) || (j-i)+1 >= (size_t)INT_MAX))
        return csL_error(C, "string slice too long");
    else if (pack) /* pack into list? */
        return getbytes_list(C, s, i, j);
    else /* get them individually */
        return getbytes_bytes(C, s, i, j);
}


static int s_byte(cs_State *C) {
    return auxgetbytes(C, 0);
}


static int s_bytes(cs_State *C) {
    return auxgetbytes(C, 1);
}


static void addvalue(cs_State *C, csL_Buffer *b, int i) {
    cs_get_index(C, 0, i);
    if (c_unlikely(!cs_is_string(C, -1)))
        csL_error(C, "invalid value (%s) at index %I in list for 'concat'",
                     csL_typename(C, -1), i);
    csL_buff_push_stack(b);
}


#define getnexti(C,begin,end,skip) \
        ((!skip) ? (int)begin+1 : cs_find_nnilindex(C,0,(uint)begin,(int)end))


// TODO: add docs and tests
static int s_concat(cs_State *C) {
    csL_Buffer b;
    size_t lsep;
    cs_Integer l = (csL_check_type(C, 0, CS_TLIST), cs_len(C, 0));
    const char *sep = csL_opt_lstring(C, 1, "", &lsep);
    cs_Integer i = posrelStart(csL_opt_integer(C, 2, 0), l);
    cs_Integer j = posrelEnd(csL_opt_integer(C, 3, l - 1), l);
    int skipnil = csL_opt_bool(C, 4, 0);
    int il = getnexti(C, (i - !skipnil), j, skipnil);
    csL_buff_init(C, &b);
    while (il != -1 && il < j) {
        addvalue(C, &b, (int)il);
        csL_buff_push_lstring(&b, sep, lsep);
        il = getnexti(C, il, j, skipnil);
    }
    if (il == j && (!skipnil || cs_find_nnilindex(C, 0, il, j) != -1))
        addvalue(C, &b, (int)j); /* add last value */
    csL_buff_end(&b);
    return 1;
}


// TODO: add docs and tests
static int s_cmp(cs_State *C) {
    int diff;
    size_t l1, l2, i;
    const char *s1 = csL_check_lstring(C, 0, &l1);
    const char *s2 = csL_check_lstring(C, 1, &l2);
    for (i = 0; l1 && l2 && *s1 == *s2; i++) {
        l1--; l2--;
        s1++; s2++;
    }
    diff = *(unsigned char *)s1 - *(unsigned char *)s2;
    cs_push_integer(C, diff);
    if (diff) /* strings are not equal? */
        cs_push_integer(C, i); /* push that index */
    else /* strings are equal */
        cs_push_nil(C); /* that index does not exist */
    return 2;
}


static const cs_Entry strlib[] = {
    {"split", s_split},
    {"rsplit", s_rsplit},
    {"startswith", s_startswith},
    {"reverse", s_reverse},
    {"repeat", s_repeat},
    {"join", s_join},
    {"fmt", s_fmt},
    {"toupper", s_toupper},
    {"tolower", s_tolower},
    {"find", s_find},
    {"rfind", s_rfind},
    {"span", s_span},
    {"cspan", s_cspan},
    {"replace", s_replace},
    {"substr", s_substr},
    {"swapcase", s_swapcase},
    {"swapupper", s_swapupper},
    {"swaplower", s_swaplower},
    {"byte", s_byte},
    {"bytes", s_bytes},
    {"concat", s_concat},
    {"cmp", s_cmp},
    {"ascii_uppercase", NULL},
    {"ascii_lowercase", NULL},
    {"ascii_letters", NULL},
    {"digits", NULL},
    {"hexdigits", NULL},
    {"octdigits", NULL},
    {"punctuation", NULL},
    {"printable", NULL},
    {"printable", NULL},
    {NULL, NULL}
};


// TODO: add docs
static void set_string_bytes(cs_State *C) {
    /* letter bytes */
    cs_push_string(C, CS_BYTES_UPPERCASE);
    cs_set_fieldstr(C, -2, "ascii_uppercase");
    cs_push_string(C, CS_BYTES_LOWERCASE);
    cs_set_fieldstr(C, -2, "ascii_lowercase");
    cs_push_string(C, CS_BYTES_LETTERS);
    cs_set_fieldstr(C, -2, "ascii_letters");
    /* digit bytes */
    cs_push_string(C, CS_BYTES_OCTDIGITS);
    cs_set_fieldstr(C, -2, "octdigits");
    cs_push_string(C, CS_BYTES_DIGITS);
    cs_set_fieldstr(C, -2, "digits");
    cs_push_string(C, CS_BYTES_HEXDIGITS);
    cs_set_fieldstr(C, -2, "hexdigits");
    /* punctuation bytes */
    cs_push_string(C, CS_BYTES_PUNCTUATION);
    cs_set_fieldstr(C, -2, "punctuation");
    /* whitespace bytes */
    cs_push_string(C, CS_BYTES_WHITESPACE);
    cs_set_fieldstr(C, -2, "whitespace");
    /* printable bytes */
    cs_push_string(C, CS_BYTES_PRINTABLE);
    cs_set_fieldstr(C, -2, "printable");
}


CSMOD_API int csopen_string(cs_State *C) {
    csL_push_lib(C, strlib);
    set_string_bytes(C);
    return 1;
}
