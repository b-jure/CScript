/*
** cstrlib.c
** Standard library for string operations
** See Copyright Notice in cscript.h
*/

#define cstrlib_c
#define CS_LIB

#include "cscriptprefix.h"

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
#include "cscriptlimits.h"


#if !defined(CS_BYTES)

#define CS_BYTES


/* uppercase ASCII letters */
#define CS_BYTES_UPPERCASE      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

/* lowercase ASCII letters */
#define CS_BYTES_LOWERCASE      "abcdefghijklmnopqrstuvwxyz"

/* (uppercase/lowercase) */
#define CS_BYTES_LETTERS        CS_BYTES_UPPERCASE CS_BYTES_LOWERCASE


/* octal digits */
#define CS_BYTES_OCTDIGITS      "01234567"

/* decimal digits */
#define CS_BYTES_DIGITS         CS_BYTES_OCTDIGITS "89"

/* hexadecimal digits */
#define CS_BYTES_HEXDIGITS      CS_BYTES_DIGITS "abcdef"


/* punctuation chars */
#define CS_BYTES_PUNCTUATION    "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

/* whitespace chars */
#define CS_BYTES_WHITESPACE     " \t\n\r\v\f"

/* graphical chars */
#define CS_BYTES_PRINTABLE      CS_BYTES_DIGITS CS_BYTES_LETTERS \
                                CS_BYTES_PUNCTUATION CS_BYTES_WHITESPACE

#endif


/* common error message */
static const char *strtoolong = "string slice too long";


/*
** Translate relative end position to absolute slice index.
*/
static size_t posrelEnd(cs_Integer pos, size_t len) {
    cs_assert(pos >= -(cs_Integer)len); /* should be handled already */
    if (pos >= (cs_Integer)len) /* absolute that overflows? */
        return cast_sizet(len - (len>0)); /* clip to last index */
    else if (pos >= 0) /* absolute in-range 'pos'? */
        return cast_sizet(pos);
    else /* otherwise negative 'pos' */
        return len + pos;
}


static const char *skipws(const char *s, size_t *pl, int rev) {
    const unsigned char *p;
    size_t l = *pl;
    if (l == 0)
        return NULL;
    else if (!rev) { /* from 's'? */
        p = (const c_ubyte *)s;
        while (l > 0 && isspace(*p))
            p++, l--;
    } else { /* reverse */
        p = ((const c_ubyte *)s + l) - 1;
        while (l > 0 && isspace(*p))
            p--, l--;
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
            while (ls > 0 && isspace(uchar(*s)))
                s++, ls--;
        else {
            aux = (s + ls) - 1;
            while (ls > 0 && isspace(uchar(*aux)))
                aux--, ls--;
        }
        while (n > 0 && (aux = skipws(s, &ls, rev)) != NULL) {
            size_t lw = 0;
            const char *p = aux;
            if (!rev)
                while (ls > 0 && !isspace(uchar(*aux)))
                    ls--, lw++, aux++;
            else { /* reverse */
                while (ls > 0 && !isspace(uchar(*aux)))
                    ls--, lw++, aux--;
                p = aux+1;
            }
            cs_assert(lw > 0);
            cs_push_lstring(C, p, lw);
            cs_set_index(C, arr, i);
            if (!rev) s = aux;
            n--; i++;
        }
        if (!rev)
            while (ls > 0 && isspace(uchar(*s)))
                s++, ls--;
        else {
            aux = (s + ls) - 1;
            while (ls > 0 && isspace(uchar(*aux)))
                aux--, ls--;
        }
        if (ls == 0)
            return 1; /* done */
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
    size_t l1, l2, posi, posj;
    const char *s1 = csL_check_lstring(C, 0, &l1);
    const char *s2 = csL_check_lstring(C, 1, &l2);
    cs_Integer i = csL_opt_integer(C, 2, 0);
    cs_Integer j = csL_opt_integer(C, 3, -1);
    if (j < -(cs_Integer)l1) /* 'j' would be less than 0? */
        goto l_fail; /* empty interval */
    else { /* convert to positions */
        posi = posrelStart(i, l1);
        posj = posrelEnd(j, l1);
    }
    if (posi <= posj && l2 <= (posj-posi) + 1) {
        size_t k = 0;
        while (posi <= posj && k < l2 && s1[posi] == s2[k])
            posi++, k++;
        cs_push_integer(C, posi);
        if (k == l2)
            return 1; /* return index after 's2' in 's1' */
        else {
            csL_push_fail(C);
            cs_insert(C, -2);
        }
    } else {
    l_fail:
        csL_push_fail(C);
        cs_push_integer(C, -1); /* invalid range */
    }
    return 2; /* return fail and invalid or non-matching index */
}


static int s_reverse(cs_State *C) {
    size_t l, i;
    csL_Buffer b;
    const char *s = csL_check_lstring(C, 0, &l);
    if (l > 1) { /* non trivial string? */
        char *p = csL_buff_initsz(C, &b, l);
        char *end = p + l - 1;
        for (i = 0; p < end; i++) {
            *p++ = s[l-i-1];
            *end-- = s[i];
        }
        csL_buff_endsz(&b, l);
    }
    return 1; /* return string */
}


static int s_repeat(cs_State *C) {
    size_t l, lsep;
    const char *s = csL_check_lstring(C, 0, &l);
    cs_Integer n = csL_check_integer(C, 1);
    const char *sep = csL_opt_lstring(C, 2, "", &lsep);
    if (c_unlikely(n <= 0))
        cs_push_literal(C, "");
    else if (l+lsep < l || l+lsep > STR_CS_MAXSIZE/n)
        csL_error(C, "resulting string too large");
    else {
        csL_Buffer b;
        size_t totalsize = (size_t)n*l + (size_t)(n-1)*lsep;
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
    while (cs_nextfield(C, 1) != 0) {
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
    int i = cs_find_index(C, 1, 0, 0, --len);
    while (i >= 0) {
        size_t l;
        const char *s;
        cs_get_index(C, 1, i);
        s = cs_to_lstring(C, -1, &l);
        cs_pop(C, 1);
        if (s && l > 0)
            auxjoinstr(b, s, l, sep, lsep);
        i = cs_find_index(C, 1, 0, ++i, len);
    }
}


static int s_join(cs_State *C) {
    csL_Buffer b;
    size_t lsep;
    const char *sep = csL_check_lstring(C, 0, &lsep);
    int t = cs_type(C, 1);
    csL_expect_arg(C, (t == CS_T_LIST || t == CS_T_TABLE), 1, "list/table");
    csL_buff_init(C, &b);
    if (t == CS_T_LIST) {
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


/* {======================================================
** STRING FORMAT
** ======================================================= */

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
** scanned back by CScript. Use hexadecimal format for "common" numbers
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
        case CS_T_STRING: {
            size_t len;
            const char *s = cs_to_lstring(C, arg, &len);
            addquoted(b, s, len);
            break;
        }
        case CS_T_NUMBER: {
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
        case CS_T_NIL: case CS_T_BOOL: {
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
    size_t l, posi, posj, endpos;
    const char *s = csL_check_lstring(C, 0, &l);
    cs_Integer i = csL_opt_integer(C, 1, 0);
    cs_Integer j = csL_opt_integer(C, 2, -1);
    if (j < -(cs_Integer)l) /* 'j' would be less than 0? */
        goto l_done; /* empty interval */
    else { /* convert to positions */
        posi = posrelStart(i, l);
        posj = posrelEnd(j, l);
        endpos = posj + 1; /* save end position */
    }
    if (l == 0 || posj < posi) {
        l_done: cs_push(C, 0);
    } else {
        csL_Buffer b;
        char *p = csL_buff_initsz(C, &b, l);
        memcpy(p, s, posi);
        while (posi < posj) {
            p[posi] = f(s[posi]); posi++;
            p[posj] = f(s[posj]); posj--;
        }
        p[posi] = f(s[posi]);
        memcpy(p+endpos, s+endpos, l-endpos);
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
    size_t l, lpat, posi, posj;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *pat = csL_check_lstring(C, 1, &lpat);
    cs_Integer i = csL_opt_integer(C, 2, 0);
    cs_Integer j = csL_opt_integer(C, 3, -1);
    if (j < -(cs_Integer)l) /* 'j' would be less than 0? */
        goto l_fail; /* empty interval */
    else { /* convert to positions */
        posi = posrelStart(i, l);
        posj = posrelEnd(j, l);
    }
    const char *p;
    if (posi <= posj && (p = findstr(s+posi, (posj-posi)+1, pat, lpat, rev)))
        cs_push_integer(C, p-s); /* start index */
    else {
        l_fail: csL_push_fail(C); /* nothing was found */
    }
    return 1;
}


static int s_find(cs_State *C) {
    return auxfind(C, 0);
}


static int s_rfind(cs_State *C) {
    return auxfind(C, 1);
}


static int aux_span(cs_State *C, int complement) {
    size_t l, lb, posi, posj, startpos;
    const char *s = csL_check_lstring(C, 0, &l);
    const char *b = csL_check_lstring(C, 1, &lb);
    cs_Integer i = csL_opt_integer(C, 2, 0);
    cs_Integer j = csL_opt_integer(C, 3, -1);
    if (j < -(cs_Integer)l) /* 'j' would be less than 0? */
        goto l_fail; /* empty interval */
    else { /* convert to positions */
        posi = posrelStart(i, l);
        posj = posrelEnd(j, l);
        startpos = posi; /* save starting position */
    }
    if (posj < posi) { /* empty interval? */
    l_fail:
        csL_push_fail(C);
        return 1;
    } else if (complement) { /* strcspn */
        if (lb == 0) { /* 'b' is empty string? */
            posi = l - posi; /* whole segment is valid */
            goto pushlen;
        }
        while (posi <= posj) {
            for (c_uint k = 0; k < lb; k++)
                if (s[posi] == b[k]) goto pushlen;
            posi++;
        }
    } else { /* strspn */
        if (lb == 0) { /* 'b' is empty string? */
            posi = startpos; /* 0 */
            goto pushlen;
        }
        while (posi <= posj) {
            for (c_uint k = 0; k < lb; k++)
                if (s[posi] == b[k]) goto nextc;
            break; /* push segment len */
        nextc:
            posi++;
        }
    }
pushlen:
    /* return computed segment length (span) */
    cs_push_integer(C, (posi - startpos));
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


// TODO: update docs
static int s_substr(cs_State *C) {
    size_t l, posi, posj;
    const char *s = csL_check_lstring(C, 0, &l);
    cs_Integer i = csL_opt_integer(C, 1, 0);
    cs_Integer j = csL_opt_integer(C, 2, -1);
    if (cs_to_bool(C, 3)) { /* positions must be absolute? */
        if (!(i < 0 || j < 0 || j < i))
            posi = i, posj = j;
        else /* otherwise force to push empty string */
            posi = 1, posj = 0;
    } else if (-(cs_Integer)l <= j) {
        posi = posrelStart(i, l),
        posj = posrelEnd(j, l);
    } else goto pushempty;
    if (posi <= posj) {
        cs_push_lstring(C, s + posi, (posj-posi)+1);
        return 1;
    }
pushempty:
    cs_push_literal(C, "");
    return 1;
}


#define swapcase(c)     (isalpha(c) ? (c)^32 : (c))
#define swaptolower(c)  (isupper(c) ? tolower(c) : (c))
#define swaptoupper(c)  (islower(c) ? toupper(c) : (c))

static void auxswapcase(char *d, const char *s, size_t posi, size_t posj) {
    c_ubyte c;
    while (posi < posj) {
        c = s[posi], d[posi++] = swapcase(c);
        c = s[posj], d[posj--] = swapcase(c);
    }
    if (posi == posj)
        c = s[posi], d[posi] = swapcase(c);
}


static void auxuppercase(char *d, const char *s, size_t posi, size_t posj) {
    c_ubyte c;
    while (posi < posj) {
        c = s[posi], d[posi++] = swaptolower(c);
        c = s[posj], d[posj--] = swaptolower(c);
    }
    c = s[posi], d[posi] = swaptolower(c);
}


static void auxlowercase(char *d, const char *s, size_t posi, size_t posj) {
    c_ubyte c;
    while (posi < posj) {
        c = s[posi], d[posi++] = swaptoupper(c);
        c = s[posj], d[posj--] = swaptoupper(c);
    }
    c = s[posi], d[posi] = swaptoupper(c);
}


static int auxcase(cs_State *C, void (*f)(char*,const char*,size_t,size_t)) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    cs_Integer i = csL_opt_integer(C, 1, 0);
    cs_Integer j = csL_opt_integer(C, 2, -1);
    if (j >= -(cs_Integer)l) { /* 'j' would be greater than 0? */
        size_t posi = posrelStart(i, l);
        size_t posj = posrelEnd(j, l);
        if (posj < posi || l == 0) /* empty interval or empty string */
            goto l_empty;
        else { /* otherwise build the new string */
            csL_Buffer b;
            char *p = csL_buff_initsz(C, &b, l);
            memcpy(p, s, posi);
            f(p, s, posi, posj);
            posj++; /* go past the last index that was swapped */
            memcpy(p+posj, s+posj, l-posj);
            csL_buff_endsz(&b, l);
        }
    } else { /* otherwise 'j' would be less than 0 */
        l_empty: cs_push(C, 0); /* get original string */
    }
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


#define pushbyte(C,s,i,k)   cs_push_integer(C, uchar((s)[(i) + cast_uint(k)]))

static int getbytes_list(cs_State *C, const char *s, size_t posi, size_t posj) {
    int n = cast_int(posj - posi) + 1;
    cs_push_list(C, n);
    for (int k = 0; k < n; k++) {
        pushbyte(C, s, posi, k);
        cs_set_index(C, -2, k);
    }
    return 1; /* return list */
}


static int getbytes_bytes(cs_State *C, const char *s, size_t posi, size_t posj) {
    int n = cast_int(posj - posi) + 1;
    csL_check_stack(C, n, strtoolong);
    for (int k = 0; k < n; k++) pushbyte(C, s, posi, k);
    return n; /* return 'n' bytes */
}


static int auxgetbytes(cs_State *C, int pack) {
    size_t l;
    const char *s = csL_check_lstring(C, 0, &l);
    cs_Integer i = csL_opt_integer(C, 1, 0);
    cs_Integer j;
    if (!cs_is_noneornil(C, 2)) /* have ending position? */
        j = csL_check_integer(C, 2); /* get it */
    else { /* no ending position, set default one */
        if (!pack) { /* not packing bytes into list? */
            if (i < -(cs_Integer)l) /* 'i' would be less than 0? */
                j = 0; /* clip end position to first index */
            else /* otherwise 'j' will be equal or above 0 */
                j = i;
        } else /* otherwise pack bytes into a list */
            j = -1; /* end position will be the last index */
        goto l_skipjcheck;
    }
    if (j >= -(cs_Integer)l) { /* end position will be >=0? */
    l_skipjcheck: {
        if (l > 0) {
            size_t posi = posrelStart(i, l);
            size_t posj = posrelEnd(j, l);
            if (posj >= posi) { /* non-empty interval? */
                if (c_unlikely((posj-posi)+1 <= (posj-posi) ||
                            cast_sizet(CS_MAXINT) <= (posj-posi)+1))
                    return csL_error(C, strtoolong);
                else if (pack) /* pack bytes into a list? */
                    return getbytes_list(C, s, posi, posj);
                else /* otherwise get bytes by pushing them on stack */
                    return getbytes_bytes(C, s, posi, posj);
            }
        }
    }}
    return 0; /* nothing to return */
}


static int s_byte(cs_State *C) {
    return auxgetbytes(C, 0);
}


static int s_bytes(cs_State *C) {
    return auxgetbytes(C, 1);
}


// TODO: add docs
static int s_char(cs_State *C) {
    int n = cs_getntop(C); /* number of arguments */
    if (n > 0) { /* have at least 1 argument? */
        csL_Buffer b;
        char *p = csL_buff_initsz(C, &b, cast_uint(n));
        for (int i=0; i<n; i++) {
            cs_Unsigned c = (cs_Unsigned)csL_check_integer(C, i);
            csL_check_arg(C, c <= (cs_Unsigned)UCHAR_MAX, i, "value out of range");
            p[i] = cast_char(uchar(c));
        }
        csL_buff_endsz(&b, cast_uint(n));
    } else /* otherwise no arguments were provided */
        cs_push_literal(C, ""); /* push empty string (a bit faster) */
    return 1;
}


// TODO: add docs
static int s_cmp(cs_State *C) {
    int res;
    size_t l1, l2;
    const char *s1 = csL_check_lstring(C, 0, &l1);
    const char *s2 = csL_check_lstring(C, 1, &l2);
    size_t i = 0;
    if (l2 > l1) l1 = l2;
    while (l1-- && uchar(s1[i]) == uchar(s2[i])) i++;
    res = s1[i] - s2[i];
    cs_push_integer(C, res);
    if (res) /* strings are not equal? */
        cs_push_integer(C, i); /* push "that" position */
    else /* otherwise strings are equal */
        cs_push_nil(C); /* "that" position doesn't exist */
    return 2; /* return comparison result and position */
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
    {"char", s_char},
    {"cmp", s_cmp},
    {"ascii_uppercase", NULL},
    {"ascii_lowercase", NULL},
    {"ascii_letters", NULL},
    {"digits", NULL},
    {"hexdigits", NULL},
    {"octdigits", NULL},
    {"punctuation", NULL},
    {"whitespace", NULL},
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
