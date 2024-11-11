/*
** cstring.c
** Functions for CScript string objects
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cscript.h"
#include "cstring.h"
#include "cobject.h"
#include "cstate.h"
#include "chashtable.h"
#include "cgc.h"
#include "cdebug.h"
#include "cmem.h"
#include "cvm.h"

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
uint csS_hash(const char *str, size_t len, unsigned int seed) {
    const cs_ubyte *data = cast(const cs_ubyte *, str);
    uint h = seed;
    for (uint i = 0; i < len; i++) {
        h ^= data[i];
        h *= 0x5bd1e995;
        h ^= h >> 15;
    }
    return h;
}


void csS_init(cs_State *ts) {
    GState *gs = G_(ts);
    /* first create weak strings table */
    gs->strings = csH_newsize(ts, CSI_MINSTRHTABSIZE);
    csG_fix(ts, obj2gco(gs->strings));
    cs_assert(obj2gco(gs->strings) == gs->fixed);
    /* then we allocate the memory error msg */
    gs->memerror = csS_newlit(ts, MEMERRMSG);
    csG_fix(ts, obj2gco(gs->memerror));
    cs_assert(obj2gco(gs->memerror) == gs->fixed);
}


static OString *createstrobj(cs_State *ts, size_t l, uint h) {
    OString *s = csG_new(ts, sizeofstring(l), CS_VSTRING, OString);
    s->len = l;
    s->hash = h;
    s->extra = 0;
    getstrbytes(s)[l] = '\0';
    return s;
}


/*
** Create new string object of size 'len'.
** Allocation is skipped in case string is already interned.
*/
OString *csS_newl(cs_State *ts, const char *chars, size_t len) {
    TValue key;
    HTable *strtab = G_(ts)->strings;
    uint hash = csS_hash(chars, len, G_(ts)->seed);
    OString *str = csH_getinterned(ts, strtab, chars, len, hash);
    if (str) { /* is interned or weak reference ? */
        return str;
    } else {
        str = createstrobj(ts, len, hash);
        if (c_likely(len != 0))
            memcpy(str->bytes, chars, len);
        setbit(str->bits, STRHASHBIT);
        setstrval(ts, &key, str);
        setstrval2s(ts, ts->sp.p++, str);
        csH_set(ts, strtab, &key, &key);
        ts->sp.p--;
        return str;
    }
}


/* create new string object from null terminated bytes */
OString *csS_new(cs_State *ts, const char *chars) {
    return csS_newl(ts, chars, strlen(chars));
}


OString *csS_newlobj(cs_State *ts, size_t len) {
    return createstrobj(ts, len, G_(ts)->seed);
}


/* free string object */
void csS_free(cs_State *ts, OString *s) {
    csM_free_(ts, s, sizeofstring(s->len));
}


/*
** Comparison similar to 'strcmp' but this works on
** strings that might have null terminator in between
** of their contents.
*/
int csS_cmp(const OString *s1, const OString *s2) {
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


/* string equality */
int csS_eq(const OString *s1, const OString *s2) {
    return ((s1 == s2) || (s1->hash == s2->hash /* pointers match or hash */
                && s1->len == s2->len /* and length */
                && memcmp(s1->bytes, s2->bytes, s1->len))); /* and contents */
}


void csS_strlimit(char *dest, const char *src, size_t len, size_t limit) {
    limit--;
    if (limit < len) {
        memcpy(dest, src, limit - SLL("..."));
        memcpy(dest, "...", SLL("..."));
        len = limit;
    } else {
        memcpy(dest, src, len);
    }
    dest[len] = '\0';
}


void csS_sourceid(char *restrict dest, const char *src, size_t len) {
    csS_strlimit(dest, src, len, CSI_MAXSRC - 1);
}


/* -------------------------------------------------------------------------
 * String conversion
 * ------------------------------------------------------------------------- */

/* maximum value for last integer digit */
#define MAXINTLASTDIG		(CS_INTEGER_MAX % 10)

/*
 * Check if integer 'i' overflows limit 'l' or in case
 * 'i' is equal to 'l' check if digit 'd' would overflow.
 */
#define ioverflow(i,d,l) \
    ((i) >= (l) && ((i) > (l) || (d) > MAXINTLASTDIG))


/* decimal overflow */
#define decoverflow(i,d)	ioverflow(i, d, (CS_INTEGER_MAX / 10))

/* octal overflow */
#define octoverflow(i,d)	ioverflow(i, d, (CS_INTEGER_MAX / 8))

/* hexadecimal overflow */
#define hexoverflow(i,d)	ioverflow(i, d, (CS_INTEGER_MAX / 16))



/* convert hex character into digit */
int csS_hexvalue(int c) {
    cs_assert(isxdigit(c));
    if (isdigit(c)) 
        return c - '0';
    else 
        return (tolower(c) - 'a') + 10;
}



#define TOLOWERBUFFSZ   200

/* 
** Convert all characters in 's' to lower case.
** This function is not reentrant and 's' must be null terminated.
** Upon each call to this function static buffer is overwritten.
** Up to 'TOLOWERBUFFSZ' characters in 's' will be converted.
*/
const char *csS_tolowerall(const char *s) {
    static char buff[TOLOWERBUFFSZ];
    int c;
    for (int i = 0; (c = *s++) && i < TOLOWERBUFFSZ; i++)
        buff[i] = tolower(c);
    return buff;
}


/*
** Convert string to 'cs_Integer'.
** This function can convert hexadecimal, octal and decimal strings
** to 'cs_Integer'.
*/
static const char *str2int(const char *s, cs_Integer *i, int *overflow) {
    cs_Unsigned u = 0;
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
            digit = csS_hexvalue(*s);
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
    *i = csi_castU2S(u*sign);
    return s;
}


static const char *str2flt(const char *s, cs_Number *n, int *of) {
    char *eptr;
    *of = 0;
    if (*s == '0'  && (s[1] == 'x' || s[1] == 'X'))
        *n = cs_strx2number(s, &eptr);
    else
        *n = cs_str2number(s, &eptr);
    if (of) { /* set underflow flag */
        if (cs_numoverflow(*n)) 
            *n = 1;
        else if (cs_numunderflow(*n)) 
            *n = -1;
    }
    if (eptr == s) 
        return NULL;
    while (isspace(*eptr)) 
        eptr++;
    return (*eptr == '\0' ? eptr : NULL);
}


/* convert string to 'cs_Number' or 'cs_Integer' */
size_t csS_tonum(const char *s, TValue *o, int *of) {
    cs_Integer i;
    cs_Number n;
    const char *e;
    int iof;

    if (of) *of = iof = 0;
    if ((e = str2int(s, &i, &iof)) != NULL) {
        setival(o, i);
    } else if ((e = str2flt(s, &n, of)) != NULL) {
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

static int num2buff(const TValue *nv, char *buff) {
    size_t len;
    cs_assert(ttisnum(nv));
    if (ttisint(nv)) {
        len = cs_integer2str(buff, MAXNUM2STR, ival(nv));
    } else {
        len = cs_number2str(buff, MAXNUM2STR, fval(nv));
        /* if it looks like integer append '.0' */
        if (strspn(buff, "-0123456789") == len) {
            buff[len++] = *localeconv()->decimal_point;
            buff[len++] = '0';
        }
    }
    return len;
}


const char *csS_numtostr(const TValue *v, size_t *plen) {
    static char buff[MAXNUM2STR];
    size_t len = num2buff(v, buff);
    if (plen)
        *plen = len;
    return buff;
}



/* --------------------------------------------------------------------------
 * String format
 * -------------------------------------------------------------------------- */

/*
 * Initial size of buffer used in 'csS_newvstringf'
 * to prevent allocations, instead the function
 * will directly work on the buffer and will push
 * strings on stack in case buffer exceeds this limit.
 * This is all done because 'csS_newvstringf' often
 * gets called by 'csD_getinfo'; the size should be
 * at least 'CS_MAXSRC' + 'MAXNUM2STR' + size for message.
 */
#define BUFFVFSSIZ	(CSI_MAXSRC + MAXNUM2STR + 100)

/* buffer for 'csS_newvstringf' */
typedef struct BuffVFS {
    cs_State *ts;
    int pushed; /* true if 'space' was pushed on the stack */
    int len; /* string length in 'space' */
    char space[BUFFVFSSIZ];
} BuffVFS;


static void initvfs(cs_State *ts, BuffVFS *vfs) {
    vfs->len = vfs->pushed = 0;
    vfs->ts = ts;
}


/*
 * Pushes 'str' to the stack and concatenates it with
 * other string on the stack if 'pushed' is set.
 */
static void pushstr(BuffVFS *buff, const char *str, size_t len) {
    cs_State *ts = buff->ts;
    OString *s = csS_newl(ts, str, len);
    setstrval2s(ts, ts->sp.p, s);
    ts->sp.p++;
    if (buff->pushed)
        csV_concat(ts, 2);
    else
        buff->pushed = 1;
}


/* pushes buffer 'space' on the stack */
static void pushbuff(BuffVFS *buff) {
    pushstr(buff, buff->space, buff->len);
    buff->len = 0;
}


/* ensure up to buffer space (up to 'BUFFVFSSIZ') */
static char *getbuff(BuffVFS *buff, int n) {
    cs_assert(n <= BUFFVFSSIZ);
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
    buff->len += num2buff(nv, getbuff(buff, MAXNUM2STR));
}


/* add pointer to buffer */
static void buffaddptr(BuffVFS *buff, const void *p) {
    const int psize = 3 * sizeof(void*) + 8;
    buff->len += cs_pointer2str(getbuff(buff, psize), psize, p);
}


/* Create new string object from format 'fmt' and args in 'argp'. */
const char *csS_pushvfstring(cs_State *ts, const char *fmt, va_list argp) {
    const char *end;
    TValue nv;
    BuffVFS buff;
    initvfs(ts, &buff);
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
        case 'I': { /* 'cs_Integer' */
            setival(&nv, va_arg(argp, cs_Integer));
            buffaddnum(&buff, &nv);
            break;
        }
        case 'N': { /* 'cs_Number' */
            setival(&nv, va_arg(argp, cs_Number));
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
            cs_ubyte c = cast(unsigned char, *(end + 1));
            csD_runerror(ts, "invalid format specifier '%%%c'", c);
            /* UNREACHED */
            return NULL;
        }
        fmt = end + 2; /* '%' + specifier */
    }
    buffaddstring(&buff, fmt, strlen(fmt));
    printf("pushing buffer\n");
    pushbuff(&buff);
    return cstrval(s2v(ts->sp.p));
}


const char *csS_pushfstring(cs_State *ts, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    const char *str = csS_pushvfstring(ts, fmt, argp);
    va_end(argp);
    return str;
}
