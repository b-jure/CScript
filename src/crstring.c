#include "cript.h"
#include "crstring.h"
#include "crobject.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crgc.h"
#include "crdebug.h"
#include "crmem.h"
#include "crvm.h"

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>



/*
** Hash string.
** One-byte-at-a-time hash based on Murmur's mix
** Source: https://github.com/aappleby/smhasher/blob/master/src/Hashes.cpp
*/
uint crS_hash(const char *str, size_t len, unsigned int seed) {
    const cr_ubyte *data = cast(const cr_ubyte *, str);
    uint h = seed;
    for (uint i = 0; i < len; i++) {
        h ^= data[i];
        h *= 0x5bd1e995;
        h ^= h >> 15;
    }
    return h;
}


/*
** Create new string object of size 'len'.
** Allocation is skipped in case string is already interned.
*/
OString *crS_newl(cr_State *ts, const char *chars, size_t len) {
    TValue key;
    HTable *strtab = G_(ts)->strings;
    uint hash = crS_hash(chars, len, G_(ts)->seed);
    OString *weakref = crH_getstring(strtab, chars, len, hash);
    if (weakref)
        return weakref;
    OString *string = crG_new(ts, sizeofstring(len), CR_VSTRING, OString);
    string->len = len;
    if (cr_likely(len != 0))
        memcpy(string->bytes, chars, len);
    string->bytes[len] = '\0';
    string->hash = hash;
    string->extra = 0;
    setbit(string->bits, STRHASHBIT);
    setstrval(ts, &key, string);
    setstrval2s(ts, ts->sp.p++, string);
    crH_set(ts, strtab, &key, &key);
    ts->sp.p--;
    return string;
}


/* create new string object */
OString *crS_new(cr_State *ts, const char *chars) {
    return crS_newl(ts, chars, strlen(chars));
}


/* free string object */
void crS_free(cr_State *ts, OString *s) {
    crM_free(ts, s, sizeofstring(s->len));
}


/*
** Comparison similar to 'strcmp' but this works on
** strings that might have null terminator in between
** of their contents.
*/
int crS_cmp(const OString *s1, const OString *s2) {
    const char *p1 = s1->bytes;
    size_t s1l = s1->len;
    const char *p2 = s2->bytes;
    size_t s2l = s2->len;
    for (;;) {
        int res = strcoll(p1, p2);
        if (res != 0)
            return res;
        size_t len = strlen(p1);
        if (len == s2l)
            return !(s1l == s2l);
        else if (len == s1l)
            return -1;
        len++; /* skip '\0' */
        p1 += len; s1l -= len;
        p2 += len; s2l -= len;
    }
}


int crS_eq(const OString *s1, const OString *s2) {
    return ((s1 == s2) || (s1->hash == s2->hash /* pointers match or hash */
                && s1->len == s2->len /* and length */
                && memcmp(s1->bytes, s2->bytes, s1->len))); /* and contents */
}


void crS_sourceid(char *restrict dest, const char *src, size_t len) {
    size_t bufflen = CRI_MAXSRC - 1;
    if (bufflen < len) {
        memcpy(dest, src, bufflen - SLL("..."));
        memcpy(dest, "...", SLL("..."));
    } else {
        memcpy(dest, src, bufflen);
    }
    dest[bufflen] = '\0';
}


/* -------------------------------------------------------------------------
 * String conversion
 * ------------------------------------------------------------------------- */

/* maximum value for last integer digit */
#define MAXINTLASTDIG		(CR_INTEGER_MAX % 10)

/*
 * Check if integer 'i' overflows limit 'l' or in case
 * 'i' is equal to 'l' check if digit 'd' would overflow.
 */
#define ioverflow(i,d,l) \
    ((i) >= (l) && ((i) > (l) || (d) > MAXINTLASTDIG))


/* decimal overflow */
#define decoverflow(i,d)	ioverflow(i, d, (CR_INTEGER_MAX / 10))

/* octal overflow */
#define octoverflow(i,d)	ioverflow(i, d, (CR_INTEGER_MAX / 8))

/* hexadecimal overflow */
#define hexoverflow(i,d)	ioverflow(i, d, (CR_INTEGER_MAX / 16))



/* convert hex character into digit */
int crS_hexvalue(int c) {
    cr_assert(isxdigit(c));
    if (isdigit(c)) 
        return c - '0';
    else 
        return (tolower(c) - 'a') + 10;
}


/*
 * Convert string to Cript integer.
 * This function can convert hexadecimal, octal
 * and decimal strings to 'cr_Integer'.
 */
static const char *otstr2int(const char *s, cr_Integer *i, int *overflow) {
    cr_uinteger u = 0;
    int ngcoval, digit, sign;
    sign = ngcoval = 1;
    while (isspace(*s)) s++; /* skip leading spaces */
    if (*s == '-' || *s == '+') {
        sign -= 2 * (*s == '-');
        s++;
    }
    if (*s == '0' && (*s == 'x' || *s == 'X')) { /* hex ? */
        s+=2; /* skip hex prefix */
        for (; isxdigit(*s); s++) {
            digit = crS_hexvalue(*s);
            if (hexoverflow(u, digit)) {
                if (overflow)
                    *overflow = 1;
                return NULL;
            }
            u = u * 16 + digit;
            ngcoval = 0;
        }
    } else if (*s == '0' && isodigit(s[1])) { /* octal ? */
        s++; /* skip '0' */
        do {
            digit = *s - '0';
            if (octoverflow(u, digit)) {
                if (overflow)
                    *overflow = 1;
                return NULL;
            }
            u = u * 8 + digit;
            ngcoval = 0;
        } while (isodigit(*++s));
    } else { /* decimal */
        for (; isdigit(*s); s++) {
            digit = *s - '0';
            if (decoverflow(u, digit)) {
                if (overflow)
                    *overflow = 1;
                return NULL;
            }
            u = u * 10 + digit;
            ngcoval = 0;
        }
    }
    while (isspace(*s)) s++; /* skip trailing spaces */
    if (ngcoval || *s != '\0') return NULL;
    *i = cri_castU2S(u*sign);
    return s;
}


static const char *otstr2flt(const char *s, cr_Number *n, int *of) {
    char *eptr;

    *of = 0;
    if (*s == '0'  && (s[1] == 'x' || s[1] == 'X'))
        *n = cr_xstr2number(s, &eptr);
    else
        *n = cr_str2number(s, &eptr);
    if (of) { /* set underflow flag */
        if (strx2numberovf(*n)) 
            *n = 1;
        else if (strx2numberunf(*n)) 
            *n = -1;
    }
    if (eptr == s) 
        return NULL;
    while (isspace(*eptr)) 
        eptr++;
    return (*eptr == '\0' ? eptr : NULL);
}


/* convert string to 'cr_Number' or 'cr_Integer' */
size_t crS_tonum(const char *s, TValue *o, int *of) {
    cr_Integer i;
    cr_Number n;
    const char *e;
    int iof;

    if (of) *of = iof = 0;
    if ((e = otstr2int(s, &i, &iof)) != NULL) {
        setival(o, i);
    } else if ((e = otstr2flt(s, &n, of)) != NULL) {
        setfval(o, n);
    } else { /* both conversions failed */
        if (of && !*of) *of = iof;
        return 0;
    }
    return (e - s) + 1;
}


/*
 * Maximum conversion length of a number to a string.
 * 'long double' (not supported currently) can be 33 digits
 * + sign + decimal point + exponent sign + 5 exponent digits
 * + null terminator (43 total).
 * All other types require less space.
 */
#define MAXNUM2STR	44

static int otnum2buff(const TValue *nv, char *buff) {
    size_t len;
    cr_assert(ttisnum(nv));
    if (ttisint(nv)) {
        len = cr_Integer2str(buff, MAXNUM2STR, ival(nv));
    } else {
        len = cr_number2str(buff, MAXNUM2STR, fval(nv));
        /* if it looks like integer append '.0' */
        if (strspn(buff, "-0123456789") == len) {
            buff[len++] = *localeconv()->decimal_point;
            buff[len++] = '0';
        }
    }
    return len;
}


void crS_numtostring(cr_State *ts, TValue *v) {
    char buff[MAXNUM2STR];
    int len = otnum2buff(v, buff);
    setstrval(ts, v, crS_newl(ts, buff, len));
}



/* --------------------------------------------------------------------------
 * String format
 * -------------------------------------------------------------------------- */

/*
 * Initial size of buffer used in 'crS_newvstringf'
 * to prevent allocations, instead the function
 * will directly work on the buffer and will push
 * strings on stack in case buffer exceeds this limit.
 * This is all done because 'crS_newvstringf' often
 * gets called by 'crD_getinfo'; the size should be
 * at least 'CR_MAXSRC' + 'MAXNUM2STR' + size for message.
 */
#define BUFFVFSSIZ	(CRI_MAXSRC + MAXNUM2STR + 100)

/* buffer for 'crS_newvstringf' */
typedef struct BuffVSF {
    cr_State *ts;
    int pushed; /* true if 'space' was pushed on the stack */
    int len; /* string length in 'space' */
    char space[BUFFVFSSIZ];
} BuffVFS;


/*
 * Pushes 'str' to the stack and concatenates it with
 * other string on the stack if 'pushed' is set.
 */
static void pushstr(BuffVFS *buff, const char *str, size_t len) {
    cr_State *ts = buff->ts;
    OString *s = crS_newl(ts, str, len);
    setstrval2s(ts, ts->sp.p, s);
    ts->sp.p++;
    if (buff->pushed)
        crV_concat(ts, 2);
    else
        buff->pushed = 1;
}


/* pushes buffer 'space' on the stack */
static void pushbuff(BuffVFS *buff) {
    pushstr(buff, buff->space, buff->len);
    buff->len = 0;
}


/* ensure up to buffer space (up to 'BUFFVSFSIZ') */
static char *getbuff(BuffVFS *buff, int n) {
    cr_assert(n <= BUFFVSFSIZ);
    if (n > BUFFVFSSIZ - buff->len)
        pushbuff(buff);
    return buff->space + buff->len;
}


/* add string to buffer */
static void buffaddstring(BuffVFS *buff, const char *str, size_t len) {
    if (len < BUFFVFSSIZ) {
        char *p = getbuff(buff, len);
        memcpy(p, str, len);
        buff->len += cast_int(len);
    } else {
        pushbuff(buff);
        pushstr(buff, str, len);
    }
}


/* add number to buffer */
static void buffaddnum(BuffVFS *buff, const TValue *nv) {
    buff->len += otnum2buff(nv, getbuff(buff, MAXNUM2STR));
}


/* add pointer to buffer */
static void buffaddptr(BuffVFS *buff, const void *p) {
    const int psize = 3 * sizeof(void*) + 8;
    buff->len += cr_pointer2str(getbuff(buff, psize), psize, p);
}


/* Create new string object from format 'fmt' and args in 'argp'. */
const char *crS_pushvfstring(cr_State *ts, const char *fmt, va_list argp) {
    const char *end;
    TValue nv;
    BuffVFS buff;
    while ((end = strchr(fmt, '%')) != NULL) {
        buffaddstring(&buff, fmt, end - fmt);
        switch (*(end + 1)) {
        case 'c': { /* 'char' */
            char c = cast(unsigned char, va_arg(argp, int));
            buffaddstring(&buff, &c, sizeof(c));
            break;
        }
        case 'd': { /* 'int' */
            setival(&nv, va_arg(argp, int));
            buffaddnum(&buff, &nv);
            break;
        }
        case 'I': { /* 'cr_Integer' */
            setival(&nv, va_arg(argp, cr_Integer));
            buffaddnum(&buff, &nv);
            break;
        }
        case 'N': { /* 'cr_Number' */
            setival(&nv, va_arg(argp, cr_Number));
            buffaddnum(&buff, &nv);
            break;
        }
        case 's': { /* 'string' */
            const char *str = va_arg(argp, const char *);
            if (str == NULL) str = "(null)";
            buffaddstring(&buff, str, strlen(str));
            break;
        }
        case 'p': { /* 'ptr' */
            buffaddptr(&buff, va_arg(argp, const void *));
            break;
        }
        case '%': {
            buffaddstring(&buff, "%", 1);
            break;
        }
        default:;
            cr_ubyte c = cast(unsigned char, *(end + 1));
            crD_runerror(ts, "invalid format specifier '%%%c'", c);
            /* UNREACHED */
            return NULL;
        }
        fmt = end + 2; /* '%' + specifier */
    }
    buffaddstring(&buff, fmt, strlen(fmt));
    pushbuff(&buff);
    return cstrval(s2v(ts->sp.p));
}


const char *crS_pushfstring(cr_State *ts, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    const char *str = crS_pushvfstring(ts, fmt, argp);
    va_end(argp);
    return str;
}
