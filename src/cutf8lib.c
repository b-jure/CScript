/*
** cutf8lib.c
** Standard library for UTF-8 manipulation
** See Copyright Notice in cscript.h
*/

#define cutf8lib_c
#define CS_LIB

#include "cscriptprefix.h"


#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "cscriptlimits.h"


#define MAXUNICODE	0x10FFFFu

#define MAXUTF		0x7FFFFFFFu


#define iscont(c)	(((c) & 0xC0) == 0x80)
#define iscontp(p)	iscont(*(p))


#define uchar(c)        ((c_ubyte)(c))


/* common error messages */
static const char *stroob = "out of bounds";
static const char *strtoolong = "string slice too long";
static const char *strinvalid = "invalid UTF-8 code";


/*
** Translate a relative string position: negative means from end.
*/
static cs_Integer posrel(cs_Integer pos, size_t len) {
    if (pos >= 0) return pos;
    else if (0u - cast_sizet(pos) > len) return -1;
    else return (cs_Integer)len + pos;
}


/*
** Decode one UTF-8 sequence, returning NULL if byte sequence is
** invalid.  The array 'limits' stores the minimum value for each
** sequence length, to check for overlong representations. Its first
** entry forces an error for non-ascii bytes with no continuation
** bytes (count == 0).
*/
static const char *utf8decode(const char *s, c_uint32 *val, int strict) {
    static const c_uint32 limits[] =
    {~(c_uint32)0, 0x80, 0x800, 0x10000u, 0x200000u, 0x4000000u};
    c_uint c = uchar(s[0]);
    c_uint32 res = 0; /* final result */
    if (c < 0x80) /* ascii? */
        res = c;
    else {
        int count = 0; /* to count number of continuation bytes */
        for (; c & 0x40; c <<= 1) { /* while it needs continuation bytes... */
            c_uint cc = uchar(s[++count]); /* read next byte */
            if (!iscont(cc)) /* not a continuation byte? */
                return NULL; /* invalid byte sequence */
            res = (res << 6) | (cc & 0x3F); /* add lower 6 bits from cont. byte */
        }
        res |= ((c_uint32)(c & 0x7F) << (count * 5)); /* add first byte */
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
static int utf8_len(cs_State *C) {
    cs_Integer n = 0; /* counter for the number of characters */
    size_t len; /* string length in bytes */
    const char *s = csL_check_lstring(C, 0, &len);
    cs_Integer posi = posrel(csL_opt_integer(C, 1, 0), len);
    cs_Integer posj = posrel(csL_opt_integer(C, 2, -1), len);
    int lax = cs_to_bool(C, 3);
    csL_check_arg(C, 0 <= posi && posi <= (cs_Integer)len, 1,
            "initial position out of bounds");
    csL_check_arg(C, posj < (cs_Integer)len, 2,
            "final position out of bounds");
    while (posi <= posj) {
        const char *s1 = utf8decode(s + posi, NULL, !lax);
        if (s1 == NULL) { /* conversion error? */
            csL_push_fail(C); /* return fail ... */
            cs_push_integer(C, posi); /* ... and current position */
            return 2;
        }
        posi = (cs_Integer)(s1 - s);
        n++;
    }
    cs_push_integer(C, n);
    return 1;
}


/*
** utf8_codepoint(s, [i, [j [, lax]]]) -> returns codepoints for all
** characters that start in the range [i,j]
*/
static int utf8_codepoint(cs_State *C) {
    size_t len;
    const char *s = csL_check_lstring(C, 0, &len);
    cs_Integer posi = posrel(csL_opt_integer(C, 1, 0), len);
    cs_Integer posj = posrel(csL_opt_integer(C, 2, posi), len);
    int lax = cs_to_bool(C, 3);
    int n;
    const char *se;
    csL_check_arg(C, 0 <= posi && posi < (cs_Integer)len+!len, 1, stroob);
    csL_check_arg(C, posj < (cs_Integer)len+!len, 2, stroob);
    if (posj < posi || len == 0) return 0; /* empty interval or empty string */
    if (c_unlikely(cast_sizet(posj-posi) + 1u <= cast_sizet(posj-posi) ||
                   CS_MAXINT <= cast_sizet(posj-posi) + 1u)) /* overflow? */
        return csL_error(C, strtoolong);
    n = cast_int(posj-posi) + 1; /* upper bound for number of returns */
    csL_check_stack(C, n, strtoolong);
    n = 0; /* count the number of returns */
    se = s + posj + 1; /* string end */
    for (s += posi; s < se;) {
        c_uint32 code;
        s = utf8decode(s, &code, !lax);
        if (s == NULL)
            return csL_error(C, strinvalid);
        cs_push_integer(C, c_castU2S(code));
        n++;
    }
    return n;
}


static void push_utf8char(cs_State *C, int arg) {
    cs_Unsigned code = (cs_Unsigned)csL_check_integer(C, arg);
    csL_check_arg(C, code <= MAXUTF, arg, "value out of range");
    cs_push_fstring(C, "%U", (long)code);
}


/*
** utf8_char(n1, n2, ...)  -> char(n1)..char(n2)...
*/
static int utf8_char(cs_State *C) {
    int n = cs_getntop(C); /* number of arguments */
    if (n == 1) /* optimize common case of single char */
        push_utf8char(C, 0);
    else {
        csL_Buffer b;
        csL_buff_init(C, &b);
        for (int i = 0; i < n; i++) {
            push_utf8char(C, i);
            csL_buff_push_stack(&b);
        }
        csL_buff_end(&b);
    }
    return 1;
}


/*
** offset(s, n, [i])  -> indices where n-th character counting from
** position 'i' starts and ends; 0 means character at 'i'.
*/
static int utf8_byteoffset(cs_State *C) {
    size_t len;
    const char *s = csL_check_lstring(C, 0, &len);
    cs_Integer n  = csL_check_integer(C, 1);
    cs_Integer posi = (n >= 0) ? 0 : (cs_Integer)len;
    posi = posrel(csL_opt_integer(C, 2, posi), len);
    csL_check_arg(C, 0 <= posi && posi <= (cs_Integer)len, 2,
                     "position out of bounds");
    if (n == 0) { /* special case? */
        /* find beginning of current byte sequence */
        while (posi > 0 && iscontp(s + posi)) posi--;
    } else {
        if (iscontp(s + posi))
            return csL_error(C, "initial position is a continuation byte");
        if (n < 0) { /* find back from 'posi'? */
            while (n < 0 && 0 < posi) { /* move back */
                do {
                    posi--; /* find beginning of previous character */
                } while (0 < posi && iscontp(s + posi));
                n++;
            }
        } else { /* otherwise find forward from 'posi' */
            n--; /* do not move for 1st character */
            while (0 < n && posi < (cs_Integer)len) {
                do {
                    posi++; /* find beginning of next character */
                } while (iscontp(s + posi)); /* cannot pass final '\0' */
                n--;
            }
        }
    }
    if (n != 0) { /* did not find given character */
        csL_push_fail(C);
        return 1;
    } else { /* otherwise push start and final position of utf8 char */
        cs_push_integer(C, posi); /* initial position */
        if ((s[posi] & 0x80) != 0) { /* multi-byte character? */
            do {
                posi++;
            } while (iscontp(s + posi + 1)); /* skip to final byte */
        }
        /* else one-byte character: final position is the initial one */
        cs_push_integer(C, posi); /* 'posi' now is the final position */
        return 2;
    }
}


static int iter_aux(cs_State *C, int strict) {
    size_t len;
    const char *s = csL_check_lstring(C, 0, &len);
    cs_Unsigned n = c_castS2U(cs_to_integer(C, 1) + 1);
    if (n < len) {
        while (iscontp(s + n)) n++; /* go to next character */
    }
    if (n >= len) /* (also handles original 'n' being less than -1) */
        return 0; /* no more codepoints */
    else {
        c_uint32 code;
        const char *next = utf8decode(s + n, &code, strict);
        if (next == NULL || iscontp(next))
            return csL_error(C, strinvalid);
        cs_push_integer(C, c_castU2S(n));
        cs_push_integer(C, c_castU2S(code));
        return 2;
    }
}


static int iter_auxstrict(cs_State *C) {
    return iter_aux(C, 1);
}

static int iter_auxlax(cs_State *C) {
    return iter_aux(C, 0);
}


static int utf8_itercodes(cs_State *C) {
    int lax = cs_to_bool(C, 1);
    const char *s = csL_check_string(C, 0);
    csL_check_arg(C, !iscontp(s), 0, strinvalid);
    cs_push_cfunction(C, lax ? iter_auxlax : iter_auxstrict);
    cs_push(C, 0);
    cs_push_integer(C, -1);
    return 3;
}


/* pattern to match a single UTF-8 character */
#define UTF8PATT	"[\0-\x7F\xC2-\xFD][\x80-\xBF]*"


static const csL_Entry funcs[] = {
    {"offset", utf8_byteoffset},
    {"codepoint", utf8_codepoint},
    {"char", utf8_char},
    {"len", utf8_len},
    {"codes", utf8_itercodes},
    /* placeholders */
    {"charpattern", NULL},
    {NULL, NULL}
};


CSMOD_API int csopen_utf8(cs_State *C) {
    csL_push_lib(C, funcs);
    cs_push_lstring(C, UTF8PATT, sizeof(UTF8PATT)/sizeof(char) - 1);
    cs_set_fieldstr(C, -2, "charpattern");
    return 1;
}

