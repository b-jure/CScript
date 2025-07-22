/*
** tutf8lib.c
** Standard library for UTF-8 manipulation
** See Copyright Notice in tokudae.h
*/

#define tutf8lib_c
#define TOKU_LIB

#include "tokudaeprefix.h"


#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"
#include "tokudaelimits.h"


#define MAXUNICODE	0x10FFFFu

#define MAXUTF		0x7FFFFFFFu


#define iscont(c)	(((c) & 0xC0) == 0x80)
#define iscontp(p)	iscont(*(p))


#define uchar(c)        ((t_ubyte)(c))


/* common error messages */
static const char *stroob = "out of bounds";
static const char *strtoolong = "string slice too long";
static const char *strinvalid = "invalid UTF-8 code";


/*
** Translate a relative string position: negative means from end.
*/
static toku_Integer posrel(toku_Integer pos, size_t len) {
    if (pos >= 0) return pos;
    else if (0u - cast_sizet(pos) > len) return -1;
    else return (toku_Integer)len + pos;
}


/*
** Decode one UTF-8 sequence, returning NULL if byte sequence is
** invalid.  The array 'limits' stores the minimum value for each
** sequence length, to check for overlong representations. Its first
** entry forces an error for non-ascii bytes with no continuation
** bytes (count == 0).
*/
static const char *utf8decode(const char *s, t_uint32 *val, int strict) {
    static const t_uint32 limits[] =
    {~(t_uint32)0, 0x80, 0x800, 0x10000u, 0x200000u, 0x4000000u};
    t_uint c = uchar(s[0]);
    t_uint32 res = 0; /* final result */
    if (c < 0x80) /* ascii? */
        res = c;
    else {
        int count = 0; /* to count number of continuation bytes */
        for (; c & 0x40; c <<= 1) { /* while it needs continuation bytes... */
            t_uint cc = uchar(s[++count]); /* read next byte */
            if (!iscont(cc)) /* not a continuation byte? */
                return NULL; /* invalid byte sequence */
            res = (res << 6) | (cc & 0x3F); /* add lower 6 bits from cont. byte */
        }
        res |= ((t_uint32)(c & 0x7F) << (count * 5)); /* add first byte */
        if (count > 5 || res > MAXUTF || res < limits[count])
            return NULL; /* invalid byte sequence */
        s += count; /* skip continuation bytes read */
    }
    if (strict) { /* comply with RFC-3629? */
        /* check for invalid code points; too large or surrogates */
        if (res > MAXUNICODE || (0xD800u <= res && res <= 0xDFFFu))
            return NULL;
    }
    if (val) *val = res;
    return s + 1; /* +1 to include first byte */
}


/*
** utf8len(s [, i [, j [, lax]]]) --> number of characters that
** start in the range [i,j], or nil + current position if 's' is not
** well formed in that interval.
*/
static int utf8_len(toku_State *T) {
    toku_Integer n = 0; /* counter for the number of characters */
    size_t len; /* string length in bytes */
    const char *s = tokuL_check_lstring(T, 0, &len);
    toku_Integer posi = posrel(tokuL_opt_integer(T, 1, 0), len);
    toku_Integer posj = posrel(tokuL_opt_integer(T, 2, -1), len);
    int lax = toku_to_bool(T, 3);
    tokuL_check_arg(T, 0 <= posi && posi <= (toku_Integer)len, 1,
            "initial position out of bounds");
    tokuL_check_arg(T, posj < (toku_Integer)len, 2,
            "final position out of bounds");
    while (posi <= posj) {
        const char *s1 = utf8decode(s + posi, NULL, !lax);
        if (s1 == NULL) { /* conversion error? */
            tokuL_push_fail(T); /* return fail ... */
            toku_push_integer(T, posi); /* ... and current position */
            return 2;
        }
        posi = (toku_Integer)(s1 - s);
        n++;
    }
    toku_push_integer(T, n);
    return 1;
}


/*
** utf8_codepoint(s, [i, [j [, lax]]]) -> returns codepoints for all
** characters that start in the range [i,j]
*/
static int utf8_codepoint(toku_State *T) {
    size_t len;
    const char *s = tokuL_check_lstring(T, 0, &len);
    toku_Integer posi = posrel(tokuL_opt_integer(T, 1, 0), len);
    toku_Integer posj = posrel(tokuL_opt_integer(T, 2, posi), len);
    int lax = toku_to_bool(T, 3);
    int n;
    const char *se;
    tokuL_check_arg(T, 0 <= posi && posi < (toku_Integer)len+!len, 1, stroob);
    tokuL_check_arg(T, posj < (toku_Integer)len+!len, 2, stroob);
    if (posj < posi || len == 0) return 0; /* empty interval or empty string */
    if (t_unlikely(cast_sizet(posj-posi) + 1u <= cast_sizet(posj-posi) ||
                   TOKU_MAXINT <= cast_sizet(posj-posi) + 1u)) /* overflow? */
        return tokuL_error(T, strtoolong);
    n = cast_int(posj-posi) + 1; /* upper bound for number of returns */
    tokuL_check_stack(T, n, strtoolong);
    n = 0; /* count the number of returns */
    se = s + posj + 1; /* string end */
    for (s += posi; s < se;) {
        t_uint32 code;
        s = utf8decode(s, &code, !lax);
        if (s == NULL)
            return tokuL_error(T, strinvalid);
        toku_push_integer(T, t_castU2S(code));
        n++;
    }
    return n;
}


static void push_utf8char(toku_State *T, int arg) {
    toku_Unsigned code = (toku_Unsigned)tokuL_check_integer(T, arg);
    tokuL_check_arg(T, code <= MAXUTF, arg, "value out of range");
    toku_push_fstring(T, "%U", (long)code);
}


/*
** utf8_char(n1, n2, ...)  -> char(n1)..char(n2)...
*/
static int utf8_char(toku_State *T) {
    int n = toku_getntop(T); /* number of arguments */
    if (n == 1) /* optimize common case of single char */
        push_utf8char(T, 0);
    else {
        tokuL_Buffer b;
        tokuL_buff_init(T, &b);
        for (int i = 0; i < n; i++) {
            push_utf8char(T, i);
            tokuL_buff_push_stack(&b);
        }
        tokuL_buff_end(&b);
    }
    return 1;
}


/*
** offset(s, n, [i])  -> indices where n-th character counting from
** position 'i' starts and ends; 0 means character at 'i'.
*/
static int utf8_byteoffset(toku_State *T) {
    size_t len;
    const char *s = tokuL_check_lstring(T, 0, &len);
    toku_Integer n  = tokuL_check_integer(T, 1);
    toku_Integer posi = (n >= 0) ? 0 : (toku_Integer)len;
    posi = posrel(tokuL_opt_integer(T, 2, posi), len);
    tokuL_check_arg(T, 0 <= posi && posi <= (toku_Integer)len, 2,
                     "position out of bounds");
    if (n == 0) { /* special case? */
        /* find beginning of current byte sequence */
        while (posi > 0 && iscontp(s + posi)) posi--;
    } else {
        if (iscontp(s + posi))
            return tokuL_error(T, "initial position is a continuation byte");
        if (n < 0) { /* find back from 'posi'? */
            while (n < 0 && 0 < posi) { /* move back */
                do {
                    posi--; /* find beginning of previous character */
                } while (0 < posi && iscontp(s + posi));
                n++;
            }
        } else { /* otherwise find forward from 'posi' */
            n--; /* do not move for 1st character */
            while (0 < n && posi < (toku_Integer)len) {
                do {
                    posi++; /* find beginning of next character */
                } while (iscontp(s + posi)); /* cannot pass final '\0' */
                n--;
            }
        }
    }
    if (n != 0) { /* did not find given character */
        tokuL_push_fail(T);
        return 1;
    } else { /* otherwise push start and final position of utf8 char */
        toku_push_integer(T, posi); /* initial position */
        if ((s[posi] & 0x80) != 0) { /* multi-byte character? */
            while (iscontp(s + posi + 1)) /* skip to final byte */
                posi++;
        }
        /* else one-byte character: final position is the initial one */
        toku_push_integer(T, posi); /* 'posi' now is the final position */
        return 2;
    }
}


static int iter_aux(toku_State *T, int strict) {
    size_t len;
    const char *s = tokuL_check_lstring(T, 0, &len);
    toku_Unsigned n = t_castS2U(toku_to_integer(T, 1) + 1);
    if (n < len) {
        while (iscontp(s + n)) n++; /* go to next character */
    }
    if (n >= len) /* (also handles original 'n' being less than -1) */
        return 0; /* no more codepoints */
    else {
        t_uint32 code;
        const char *next = utf8decode(s + n, &code, strict);
        if (next == NULL || iscontp(next))
            return tokuL_error(T, strinvalid);
        toku_push_integer(T, t_castU2S(n));
        toku_push_integer(T, t_castU2S(code));
        return 2;
    }
}


static int iter_auxstrict(toku_State *T) {
    return iter_aux(T, 1);
}

static int iter_auxlax(toku_State *T) {
    return iter_aux(T, 0);
}


static int utf8_itercodes(toku_State *T) {
    int lax = toku_to_bool(T, 1);
    const char *s = tokuL_check_string(T, 0);
    tokuL_check_arg(T, !iscontp(s), 0, strinvalid);
    toku_push_cfunction(T, lax ? iter_auxlax : iter_auxstrict);
    toku_push(T, 0);
    toku_push_integer(T, -1);
    return 3;
}


/* pattern to match a single UTF-8 character */
#define UTF8PATT	"[\0-\x7F\xC2-\xFD][\x80-\xBF]*"


static const tokuL_Entry funcs[] = {
    {"offset", utf8_byteoffset},
    {"codepoint", utf8_codepoint},
    {"char", utf8_char},
    {"len", utf8_len},
    {"codes", utf8_itercodes},
    /* placeholders */
    {"charpattern", NULL},
    {NULL, NULL}
};


TOKUMOD_API int tokuopen_utf8(toku_State *T) {
    tokuL_push_lib(T, funcs);
    toku_push_lstring(T, UTF8PATT, sizeof(UTF8PATT)/sizeof(char) - 1);
    toku_set_field_str(T, -2, "charpattern");
    return 1;
}

