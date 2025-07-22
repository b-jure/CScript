/*
** tstring.c
** Functions for Tokudae string objects
** See Copyright Notice in tokudae.h
*/

#define tstring_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tstate.h"
#include "tokudae.h"
#include "tstring.h"
#include "tobject.h"
#include "tgc.h"
#include "ttypes.h"
#include "tdebug.h"
#include "tmem.h"
#include "tvm.h"
#include "tokudaelimits.h"
#include "tprotected.h"

#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>


/* maximum size for string table */
#define MAXSTRTABLE     TOKU_MAXINT


/* string equality */
int tokuS_eqlngstr(const OString *s1, const OString *s2) {
    size_t len = s1->u.lnglen;
    return (s1 == s2) || /* same instance or... */
        ((len == s2->u.lnglen) && /* equal length and... */
        (memcmp(getlngstr(s1), getlngstr(s2), len) == 0)); /* equal contents */
}


/*
** Clear API string cache. (Entries cannot be empty, so fill them with
** a non-collectable string.)
*/
void tokuS_clearcache(GState *gs) {
    for (int i = 0; i < TOKUI_STRCACHE_N; i++) {
        for (int j = 0; j < TOKUI_STRCACHE_M; j++) {
            if (iswhite(gs->strcache[i][j])) /* will entry be collected? */
                gs->strcache[i][j] = gs->memerror;
        }
    }
}


t_uint tokuS_hash(const char *str, size_t l, unsigned int seed) {
    t_uint h = seed ^ cast_uint(l);
    for (; l > 0; l--)
        h ^= ((h<<5) + (h>>2) + cast_ubyte(str[l - 1]));
    return h;
}


t_uint tokuS_hashlngstr(OString *s) {
    toku_assert(s->tt_ == TOKU_VLNGSTR);
    if (s->extra == 0) { /* no hash? */
        size_t len = s->u.lnglen;
        s->hash = tokuS_hash(getlngstr(s), len, s->hash);
        s->extra = 1; /* indicate that it has hash */
    }
    return s->hash;
}


static void rehashtable(OString **arr, int osz, int nsz) {
    int i;
    for (i = osz; i < nsz; i++) /* clear new part */
        arr[i] = NULL;
    for (i = 0; i < osz; i++) { /* rehash old part */
        OString *s = arr[i]; /* get the string at slot (if any) */
        arr[i] = NULL; /* clear the slot */
        while (s) { /* for each string in the chain */
            OString *next = s->u.next; /* save 'next' */
            t_uint h = hashmod(s->hash, nsz); /* get hash position */
            s->u.next = arr[h]; /* chain it into array */
            arr[h] = s;
            s = next;
        }
    }
}


/* 
** Resize string table. If allocation fails, keep the current size.
*/
void tokuS_resize(toku_State *T, int nsz) {
    StringTable *tab = &G(T)->strtab;
    int osz = tab->size;
    OString **newarr;
    toku_assert(nsz <= MAXSTRTABLE);
    if (nsz < osz) /* shrinking ? */
        rehashtable(tab->hash, osz, nsz); /* depopulate shrinking part */
    newarr = tokuM_reallocarray(T, tab->hash, osz, nsz, OString*);
    if (t_unlikely(newarr == NULL)) { /* reallocation failed? */
        if (nsz < osz) /* was it shrinking table? */
            rehashtable(tab->hash, nsz, osz); /* restore to original size */
        /* leave table as it was */
    } else { /* allocation succeeded */
        tab->hash = newarr;
        tab->size = nsz;
        if (nsz > osz) /* expanded? */
            rehashtable(newarr, osz, nsz); /* rehash for new size */
    }
}


void tokuS_init(toku_State *T) {
    GState *gs = G(T);
    StringTable *tab = &gs->strtab;
    /* first initialize string table... */
    tab->hash = tokuM_newarray(T, TOKUI_MINSTRTABSIZE, OString*);
    rehashtable(tab->hash, 0, TOKUI_MINSTRTABSIZE); /* clear array */
    tab->size = TOKUI_MINSTRTABSIZE;
    toku_assert(tab->nuse == 0);
    /* allocate the memory-error message */
    gs->memerror = tokuS_newlit(T, MEMERRMSG);
    tokuG_fix(T, obj2gco(gs->memerror)); /* fix it */
    for (int i = 0; i < TOKUI_STRCACHE_N; i++) /* fill cache with valid strings */
        for (int j = 0; j < TOKUI_STRCACHE_M; j++)
            gs->strcache[i][j] = gs->memerror;
}


static OString *newstrobj(toku_State *T, size_t l, int tag, t_uint h) {
    GCObject *o = tokuG_new(T, sizeofstring(l), tag);
    OString *s = gco2str(o);
    s->hash = h;
    s->extra = 0;
    getstr(s)[l] = '\0'; /* null-terminate */
    return s;
}


OString *tokuS_newlngstrobj(toku_State *T, size_t len) {
    OString *s = newstrobj(T, len, TOKU_VLNGSTR, G(T)->seed);
    s->u.lnglen = len;
    s->shrlen = 0xFF;
    return s;
}


void tokuS_remove(toku_State *T, OString *s) {
    StringTable *tab = &G(T)->strtab;
    OString **pp = &tab->hash[hashmod(s->hash, tab->size)];
    while (*pp != s) /* find previous element */
        pp = &(*pp)->u.next;
    *pp = (*pp)->u.next; /* remove it from list */
    tab->nuse--;
}


/* grow string table */
static void growtable(toku_State *T, StringTable *tab) {
    if (t_unlikely(tab->nuse == TOKU_MAXINT)) {
        tokuG_fullinc(T, 1); /* try to reclaim memory */
        if (tab->nuse == TOKU_MAXINT) /* still too many strings? */
            tokuM_error(T);
    }
    if (tab->size <= MAXSTRTABLE / 2) /* can grow string table? */
        tokuS_resize(T, tab->size * 2);
}


static OString *internshrstr(toku_State *T, const char *str, size_t l) {
    OString *s;
    GState *gs = G(T);
    StringTable *tab = &gs->strtab;
    t_uint h = tokuS_hash(str, l, gs->seed);
    OString **list = &tab->hash[hashmod(h, tab->size)];
    toku_assert(str != NULL); /* otherwise 'memcmp'/'memcpy' are undefined */
    for (s = *list; s != NULL; s = s->u.next) { /* probe chain */
        if (s->shrlen==l && (memcmp(str, getshrstr(s), l*sizeof(char))==0)) {
            if (isdead(gs, s)) /* dead (but not yet collected)? */
                changewhite(s); /* ressurect it */
            return s;
        }
    }
    /* else must create a new string */
    if (tab->nuse >= tab->size) { /* need to grow the table? */
        growtable(T, tab);
        list = &tab->hash[hashmod(h, tab->size)]; /* rehash with new size */
    }
    s = newstrobj(T, l, TOKU_VSHRSTR, h);
    s->shrlen = cast_ubyte(l);
    memcpy(getshrstr(s), str, l*sizeof(char));
    s->u.next = *list;
    *list = s;
    tab->nuse++;
    return s;
}


/* create new string with explicit length */
OString *tokuS_newl(toku_State *T, const char *str, size_t l) {
    if (l <= TOKUI_MAXSHORTLEN) { /* short string? */
        return internshrstr(T, str, l);
    } else { /* otherwise long string */
        OString *s;
        if (t_unlikely(l*sizeof(char) >= (TOKU_MAXSIZE-sizeof(OString))))
            tokuM_toobig(T);
        s = tokuS_newlngstrobj(T, l);
        memcpy(getlngstr(s), str, l*sizeof(char));
        return s;
    }
}


/*
** Create or ruse a zero-terminated string, first checking the
** cache (using the string address as key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp'.
*/
OString *tokuS_new(toku_State *T, const char *str) {
    t_uint i = pointer2uint(str) % TOKUI_STRCACHE_N; /* hash */
    OString **p = G(T)->strcache[i]; /* address as key */
    int j;
    for (j = 0; j < TOKUI_STRCACHE_M; j++) {
        if (strcmp(str, getstr(p[j])) == 0) /* hit? */
            return p[j]; /* done */
    }
    /* normal route */
    for (j = TOKUI_STRCACHE_M - 1; j > 0; j--) /* make space for new string */
        p[j] = p[j - 1]; /* move out last element */
    /* new string is first in the list */
    p[0] = tokuS_newl(T, str, strlen(str));
    return p[0];
}


/*
** Comparison similar to 'strcmp' but this works on strings that
** might have null terminator before their end.
*/
int tokuS_cmp(const OString *s1, const OString *s2) {
    const char *p1 = s1->bytes;
    size_t lreal1 = getstrlen(s1);
    const char *p2 = s2->bytes;
    size_t lreal2 = getstrlen(s2);
    for (;;) { /* for each segment */
        int temp = strcoll(p1, p2);
        if (temp != 0) { /* not equal? */
            return temp; /* done */
        } else { /* strings are equal up to '\0' */
            size_t lseg1 = strlen(p1); /* index of first '\0' in 'p1' */
            size_t lseg2 = strlen(p2); /* index of first '\0' in 'p2' */
            if (lseg2 == lreal2) /* 'p2' finished? */
                return !(lseg1 == lreal2);
            else if (lseg1 == lreal1) /* 'p1' finished? */
                return -1; /* 'p1' is less than 'p2' ('p2' is not finihsed) */
            /* both strings longher than the segments; compare after '\0' */
            lseg1++; lseg2++; /* skip '\0' */
            p1 += lseg1; lreal1 -= lseg1; p2 += lseg1; lreal2 -= lseg1;
        }
    }
}


/* {=====================================================================
** String conversion
** ====================================================================== */

/* convert hex character into digit */
t_sinline int hexvalue(int c) {
    if (c > '9') /* hex digit? */ 
        return (ttolower(c) - 'a') + 10;
    else  /* decimal digit */
        return c - '0';
}

int tokuS_hexvalue(int c) {
    toku_assert(tisxdigit(c));
    return hexvalue(c);
}


/* Lookup table for digit values. -1==255>=36 -> invalid */
static const unsigned char table[] = { -1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
-1,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
25,26,27,28,29,30,31,32,33,34,35,-1,-1,-1,-1,-1,
-1,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
25,26,27,28,29,30,31,32,33,34,35,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
};

static const char *str2int(const char *s, toku_Integer *i) {
    const t_ubyte *val = table + 1;
    toku_Unsigned lim = TOKU_INTEGER_MIN;
    int sign = 1;
    uint32_t x;
    toku_Unsigned y = 0;
    int base, c, empty;
    empty = 1;
    c = *s++;
    while (tisspace(c)) c = *s++; /* skip leading spaces */
    if (c == '-' || c == '+') { /* have sign? */
        sign -= 2*(c == '-'); /* adjust sign value */
        c = *s++;
    }
    /* handle prefix to get base (if any) */
    if (c == '0') {
        if (ttolower(*s) == 'x') { /* hexadecimal? */
            s++; /* skip x|X */
            base = 16;
        } else if (ttolower(*s) == 'b') { /* binary? */
            s++; /* skip b|B */
            base = 2;
        } else { /* otherwise octal */
            base = 8;
            empty = 0; /* have '0' */
        }
        c = *s++; /* get first digit */
    } else /* otherwise it must be decimal */
        base = 10; /* c already has first digit */
    empty = !(val[c] < base) && empty;
    /* now do the conversion */
    if (base == 10) {
        if (!empty) {  
            for (x=0; val[c]<base && x <= UINT_MAX/10-1; c=*s++)
                x = x * 10 + val[c];
            for (y=x; val[c]<base && y <= TOKU_UNSIGNED_MAX/10 &&
                      10*y <= TOKU_UNSIGNED_MAX-val[c]; c=*s++)
                y = y * 10 + val[c];
        }
    } else if (!(base & (base-1))) { /* base is power of 2? (up to base 32) */
        if (!empty) {
            int bs = "\0\1\2\4\7\3\6\5"[(0x17*base)>>5&7];
            for (x=0; val[c] < base && x <= UINT_MAX/32; c=*s++)
                x = x<<bs | val[c];
            for (y=x; val[c]<base && y <= TOKU_UNSIGNED_MAX>>bs; c=*s++)
                y = y<<bs | val[c];
        }
    } else { /* other bases (up to base 36) */
        if (!empty) {
            for (x=0; val[c]<base && x <= UINT_MAX/36-1; c=*s++)
                x = x * base + val[c];
            for (y=x; val[c] < base && y <= TOKU_UNSIGNED_MAX/base &&
                      base*y <= TOKU_UNSIGNED_MAX-val[c]; c=*s++)
                y = y * base + val[c];
        }
    }
    if (empty || /* empty numeral... */
        val[c] < base || /* ...or 'TOKU_UNSIGNED_MAX' overflown, */
        (y >= lim && /* ...or numeral is bigger or equal than 'lim', */
         ((sign > 0 && base != 16 && base != 2) || /* ...and positive x|b, */
          (y > lim && /* ...or the 'lim' is overflown' */
          /* ...and is not hex or binary and not out of bounds */
          !((base == 16 || base == 2) && y <= TOKU_UNSIGNED_MAX))))) {
        return NULL; /* over(under)flow (do not accept it as integer) */
    } else {
        while (tisspace(c)) c = *s++; /* skip trailing spaces */
        if (empty || c != '\0') return NULL; /* conversion failed? */
        *i = t_castU2S(sign * y);
        return s - 1;
    }
}


/* maximum length of a numeral to be converted to a number */
#if !defined (T_MAXNUMERAL)
#define T_MAXNUMERAL	200
#endif


static const char *loc_str2flt(const char *s, toku_Number *res, int *pf) {
    char *eptr = NULL; /* to avoid warnings */
    toku_assert(pf != NULL);
    *pf = 0;
    errno = 0;
    *res = toku_str2number(s, &eptr);
    if (eptr == s)
        return NULL; /* nothing was converted? */
    else if (t_unlikely(errno == ERANGE)) {
        if (*res == TOKU_HUGE_VAL || *res == -TOKU_HUGE_VAL) {
            *pf = 1; /* overflow (negative/positive infinity) */
            /* explicit 'inf|infinity' does not set errno */
        } else {
            toku_assert(*res <= TOKU_NUMBER_MIN);
            *pf = -1; /* underflow (very large negative exponent) */
        }
    }
    while (tisspace(*eptr)) eptr++; /* skip trailing spaces */
    return (*eptr == '\0') ? eptr : NULL;
}


static const char *str2flt(const char *s, toku_Number *res, int *pf) {
    const char *endptr = loc_str2flt(s, res, pf);
    if (endptr == NULL) { /* failed? may be a different locale */
        char buff[T_MAXNUMERAL + 1];
        const char *pdot = strchr(s, '.');
        if (pdot == NULL || strlen(s) > T_MAXNUMERAL)
            return NULL; /* no dot or string too long; fail */
        strcpy(buff, s);
        buff[pdot - s] = toku_getlocaledecpoint(); /* correct decimal point */
        endptr = loc_str2flt(buff, res, pf);
        if (endptr != NULL)
            endptr = s + (endptr - buff); /* make relative to 's' */
    }
    return endptr;
}


size_t tokuS_tonum(const char *s, TValue *o, int *pf) {
    const char *e;
    toku_Integer i;
    toku_Number n;
    int f = 0; /* flag for float overflow */
    if ((e = str2int(s, &i)) != NULL) {
        setival(o, i);
    } else if ((e = str2flt(s, &n, &f)) != NULL) {
        setfval(o, cast_num(n));
    } else /* both conversions failed */
        return 0;
    if (pf) *pf = f;
    return (e - s) + 1; /* success; return string size */
}


unsigned tokuS_tostringbuff(const TValue *obj, char *buff) {
    size_t len;
    toku_assert(ttisnum(obj));
    if (ttisint(obj)) {
        len = toku_integer2str(buff, TOKU_N2SBUFFSZ, ival(obj));
    } else {
        len = toku_number2str(buff, TOKU_N2SBUFFSZ, fval(obj));
        /* if it looks like integer append '.0' */
        if (buff[strspn(buff, "-0123456789")] == '\0') {
            buff[len++] = toku_getlocaledecpoint();
            buff[len++] = '0'; /* adds ".0" to result */
        }
    }
    toku_assert(len < TOKU_N2SBUFFSZ);
    return cast_uint(len);
}


void tokuS_tostring(toku_State *T, TValue *obj) {
    char buff[TOKU_N2SBUFFSZ];
    t_uint len = tokuS_tostringbuff(obj, buff);
    setstrval(T, obj, tokuS_newl(T, buff, len));
}


int tokuS_utf8esc(char *buff, t_ulong n) {
    int x = 1; /* number of bytes put in buffer (backwards) */
    toku_assert(n <= 0x7FFFFFFFu);
    if (n < 0x80) /* ascii? */
        buff[UTF8BUFFSZ - 1] = cast_char(n);
    else { /* need continuation bytes */
        t_uint mfb = 0x3f; /* maximum that fits in first byte */
        do { /* add continuation bytes */
            buff[UTF8BUFFSZ - (x++)] = cast_char(0x80 | (n & 0x3f));
            n >>= 6; /* remove added bits */
            mfb >>= 1; /* now there is one less bit available in first byte */
        } while (n > mfb); /* still needs continuation byte? */
        buff[UTF8BUFFSZ - x] = cast_char((~mfb << 1) | n); /* add first byte */
    }
    return x;
}



/* ------------------------------------------------------------------------
** String format
** ------------------------------------------------------------------------ */

/*
** Initial size of buffer used in 'tokuS_newvstringf'
** to prevent allocations, instead the function
** will directly work on the buffer and will push
** strings on stack in case buffer exceeds this limit.
** This is all done because 'tokuS_newvstringf' often
** gets called by 'tokuD_getinfo'; the size should be
** at least 'TOKU_IDSIZE' + 'MAXNUM2STR' + size for message.
*/
#define BUFFVFSSIZ	(TOKU_IDSIZE + TOKU_N2SBUFFSZ + 100)

/* buffer for 'tokuS_newvstringf' */
typedef struct BuffVFS {
    toku_State *T;
    int pushed; /* true if 'space' was pushed on the stack */
    int len; /* string length in 'space' */
    char space[BUFFVFSSIZ];
} BuffVFS;


static void initvfs(toku_State *T, BuffVFS *vfs) {
    vfs->len = vfs->pushed = 0;
    vfs->T = T;
}


/*
** Pushes 'str' to the stack and concatenates it with
** other string on the stack if 'pushed' is set.
*/
static void pushstr(BuffVFS *buff, const char *str, size_t len) {
    toku_State *T = buff->T;
    OString *s = tokuS_newl(T, str, len);
    setstrval2s(T, T->sp.p, s);
    T->sp.p++;
    if (buff->pushed)
        tokuV_concat(T, 2);
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
    toku_assert(n <= BUFFVFSSIZ);
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
    buff->len += tokuS_tostringbuff(nv, getbuff(buff, TOKU_N2SBUFFSZ));
}


/* add pointer to buffer */
static void buffaddptr(BuffVFS *buff, const void *p) {
    const int psize = 3 * sizeof(void*) + 8;
    buff->len += toku_pointer2str(getbuff(buff, psize), psize, p);
}


/* Create new string object from format 'fmt' and args in 'argp'. */
const char *tokuS_pushvfstring(toku_State *T, const char *fmt, va_list argp) {
    const char *end;
    TValue nv;
    BuffVFS buff;
    initvfs(T, &buff);
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
            case 'I': { /* 'toku_Integer' */
                setival(&nv, va_arg(argp, toku_Integer));
                buffaddnum(&buff, &nv);
                break;
            }
            case 'f': { /* 'toku_Number' */
                setfval(&nv, va_arg(argp, toku_Number));
                buffaddnum(&buff, &nv);
                break;
            }
            case 'U': {  /* a 'long' as a UTF-8 sequence */
                char bf[UTF8BUFFSZ];
                int len = tokuS_utf8esc(bf, va_arg(argp, long));
                buffaddstring(&buff, bf + UTF8BUFFSZ - len, len);
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
            default: {
                t_ubyte c = cast(unsigned char, *(end + 1));
                tokuD_runerror(T, "invalid format specifier '%%%c'", c);
                /* UNREACHED */
                return NULL;
            }
        }
        fmt = end + 2; /* '%' + specifier */
    }
    buffaddstring(&buff, fmt, strlen(fmt));
    pushbuff(&buff);
    return getstr(strval(s2v(T->sp.p - 1)));
}


const char *tokuS_pushfstring(toku_State *T, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    const char *str = tokuS_pushvfstring(T, fmt, argp);
    va_end(argp);
    return str;
}

/* }===================================================================== */


#define DOTS	"..."
#define PRE	"[string \""
#define POS	"\"]"

#define addstr(a,b,l)	(memcpy(a,b,(l) * sizeof(char)), a += (l))


void tokuS_trimstr(char *restrict out, size_t lout, const char *s, size_t l) {
    const char *nl = strchr(s, '\n'); /* find first new line */
    lout -= LL(DOTS) + 1; /* save space for '...' and '\0' */
    if (l < lout && nl == NULL) /* no newlines? */
        addstr(out, s, l); /* keep it */
    else {
        if (nl != NULL)
            l = cast_sizet(nl - s); /* stop at first newline */
        if (l > lout) l = lout;
        addstr(out, s, l);
        addstr(out, DOTS, LL(DOTS));
    }
    *out = '\0'; /* terminate */
}


// TODO: update load docs (when loading strings, functions, etc...)
void tokuS_chunkid(char *restrict out, const char *source, size_t srclen) {
    size_t bufflen = TOKU_IDSIZE; /* free space in buffer */
    if (*source == '=') { /* 'literal' source */
        if (srclen <= bufflen) /* small enough? (excluding '=') */
            memcpy(out, source + 1, srclen * sizeof(char));
        else { /* too large, truncate it */
            addstr(out, source + 1, bufflen - 1);
            *out = '\0';
        }
    } else if (*source == '@') { /* file name */
        if (srclen <= bufflen) /* small enough? (excluding '@') */
            memcpy(out, source + 1, srclen * sizeof(char));
        else { /* too large, add '...' before rest of name */
            addstr(out, DOTS, LL(DOTS));
            bufflen -= LL(DOTS);
            memcpy(out, source + 1 + srclen - bufflen, bufflen * sizeof(char));
        }
    } else { /* string; format as [string "source"] */
        const char *nl = strchr(source, '\n'); /* find first new line */
        addstr(out, PRE, LL(PRE)); /* add prefix */
        bufflen -= LL(PRE DOTS POS) + 1; /* save space for prefix+suffix+'\0' */
        if (srclen < bufflen && nl == NULL) /* small one-line source? */
            addstr(out, source, srclen); /* keep it */
        else {
            if (nl != NULL)
                srclen = (size_t)(nl - source); /* stop at first newline */
            if (srclen > bufflen) srclen = bufflen;
            addstr(out, source, srclen);
            addstr(out, DOTS, LL(DOTS));
        }
        memcpy(out, POS, (LL(POS) + 1) * sizeof(char));
    }
}
