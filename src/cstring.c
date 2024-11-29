/*
** cstring.c
** Functions for CScript string objects
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cstate.h"
#include "cscript.h"
#include "cstring.h"
#include "cobject.h"
#include "cgc.h"
#include "ctypes.h"
#include "cdebug.h"
#include "cmem.h"
#include "cvm.h"
#include "climits.h"
#include "cprotected.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>


/* maximum size for string table */
#define MAXSTRTABLE     MAX_INT


/* string equality */
int csS_eqlngstr(const OString *s1, const OString *s2) {
    size_t len = s1->u.lnglen;
    return (s1 == s2) || /* same instance or... */
            ((len == s2->u.lnglen) && /* equal length and... */
             (memcmp(getlngstr(s1), getlngstr(s2), len) == 0)); /* contents */
}


/*
** Clear API string cache. (Entries cannot be empty, so fill them with
** a non-collectable string.)
*/
void csS_clearcache(GState *gs) {
    for (int i = 0; i < STRCACHE_N; i++) {
        for (int j = 0; j < STRCACHE_M; j++) {
            if (iswhite(gs->strcache[i][j])) /* will entry be collected? */
                gs->strcache[i][j] = gs->memerror;
        }
    }
}


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


uint csS_hashlngstr(OString *s) {
    cs_assert(s->tt_ == CS_VLNGSTR);
    if (s->extra == 0) { /* no hash? */
        size_t len = s->u.lnglen;
        s->hash = csS_hash(getlngstr(s), len, s->hash);
        s->extra = 1; /* indicate that it has hash */
    }
    return s->hash;
}


static void rehashtable(OString **arr, int osz, int nsz) {
    int i;
    for (i = osz; i < nsz; i++) /* clear new part */
        arr[i] = NULL;
    for (i = 0; i < osz; i++) { /* rehash old part */
        OString *s = arr[i]; /* get the slot if any */
        arr[i] = NULL; /* clear the slot */
        while (s) { /* for each string in the list/chain */
            OString *next = s->u.next; /* save 'next' */
            uint h = hashmod(s->hash, nsz); /* get hash position */
            s->u.next = arr[h]; /* chain the existing if any */
            arr[h] = s; /* set as head in the array */
            s = next; /* insert/chain 'next' if any */
        }
    }
}


/* 
** Resize string table. If allocation fails, keep the current size.
*/
void csS_resize(cs_State *ts, int nsz) {
    StringTable *tab = &G_(ts)->strtab;
    int osz = tab->size;
    OString **newarr;
    cs_assert(nsz <= MAXSTRTABLE);
    if (nsz < osz) /* shrinking ? */
        rehashtable(tab->hash, osz, nsz); /* depopulate shrinking part */
    newarr = csM_reallocarray(ts, tab->hash, osz, nsz);
    if (c_unlikely(newarr == NULL)) { /* reallocation failed? */
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


void csS_init(cs_State *ts) {
    GState *gs = G_(ts);
    StringTable *tab = &gs->strtab;
    /* first initialize string table... */
    tab->hash = csM_newarray(ts, MINSTRTABSIZE, OString*);
    rehashtable(tab->hash, 0, MINSTRTABSIZE); /* clear array */
    tab->size = MINSTRTABSIZE;
    cs_assert(tab->nuse == 0);
    /* allocate the memory-error message... */
    gs->memerror = csS_newlit(ts, MEMERRMSG);
    csG_fix(ts, obj2gco(gs->memerror)); /* ...and fix it */
    for (int i = 0; i < STRCACHE_N; i++) /* fill cache with valid strings */
        for (int j = 0; j < STRCACHE_M; j++)
            gs->strcache[i][j] = gs->memerror;
}


static OString *newstrobj(cs_State *ts, size_t l, int tt_, uint h) {
    GCObject *o = csG_new(ts, sizeofstring(l), tt_);
    OString *s = gco2str(o);
    s->hash = h;
    s->extra = 0;
    getstr(s)[l] = '\0'; /* null-terminate */
    return s;
}


OString *csS_newlngstrobj(cs_State *ts, size_t len) {
    OString *s = newstrobj(ts, len, CS_VLNGSTR, G_(ts)->seed);
    s->u.lnglen = len;
    s->shrlen = 0xFF;
    return s;
}


void csS_remove(cs_State *ts, OString *s) {
    StringTable *tab = &G_(ts)->strtab;
    OString **pp = &tab->hash[hashmod(s->hash, tab->size)];
    while (*pp != s) /* find previous element */
        pp = &(*pp)->u.next;
    *pp = (*pp)->u.next; /* remove it from list */
    tab->nuse--;
}


/* grow string table */
static void growtable(cs_State *ts, StringTable *tab) {
    if (c_unlikely(tab->nuse == MAX_INT)) {
        csG_full(ts, 1); /* try to reclaim memory */
        if (tab->nuse == MAX_INT)
            csM_error(ts);
    }
    if (tab->size <= MAXSTRTABLE / 2)
        csS_resize(ts, tab->size * 2);
}


static OString *internshrstr(cs_State *ts, const char *str, size_t l) {
    OString *s;
    GState *gs = G_(ts);
    StringTable *tab = &gs->strtab;
    uint h = csS_hash(str, l, gs->seed);
    OString **list = &tab->hash[hashmod(h, tab->size)];
    cs_assert(str != NULL); /* otherwise 'memcmp'/'memcpy' are undefined */
    for (s = *list; s != NULL; s = s->u.next) { /* probe chain */
        if (s->shrlen == l && (memcmp(str, getstr(s), l*sizeof(char)) == 0)) {
            if (isdead(gs, s)) /* "dead"? */
                changewhite(s); /* "ressurect" it */
            return s;
        }
    }
    /* otherwise create new string */
    if (tab->nuse >= tab->size) { /* need to grow the table? */
        growtable(ts, tab);
        list = &tab->hash[hashmod(h, tab->size)];
    }
    s = newstrobj(ts, l, CS_VSHRSTR, h);
    memcpy(getshrstr(s), str, l*sizeof(char));
    s->shrlen = cast_ubyte(l);
    s->u.next = *list;
    *list = s;
    tab->nuse++;
    return s;
}


/* create new string with explicit length */
OString *csS_newl(cs_State *ts, const char *str, size_t len) {
    if (len <= CSI_MAXSHORTLEN) { /* short string? */
        return internshrstr(ts, str, len);
    } else { /* otherwise long string */
        OString *s;
        if (c_unlikely(len * sizeof(char) >= (MAXSIZE - sizeof(OString))))
            csM_toobig(ts);
        s = csS_newlngstrobj(ts, len);
        memcpy(getlngstr(s), str, len);
        return s;
    }
}


/*
** Create or ruse a zero-terminated string, first checking the
** cache (using the string address as key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp'.
*/
OString *csS_new(cs_State *ts, const char *str) {
    int j;
    uint i = pointer2uint(str) % STRCACHE_N;
    OString **p = G_(ts)->strcache[i]; /* address as key */
    for (j = 0; j < STRCACHE_M; j++)
        if (strcmp(getstr(p[j]), str) == 0)
            return p[j];
    /* regular route */
    for (j = STRCACHE_M - 1; j > 0; j--) /* make space for new string */
        p[j] = p[j - 1]; /* move out last element */
    /* new string is first in the cache line 'i' */
    p[0] = csS_newl(ts, str, strlen(str));
    return p[0];
}


/*
** Comparison similar to 'strcmp' but this works on strings that
** might have null terminator before their end.
*/
int csS_cmp(const OString *s1, const OString *s2) {
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


/* -----------------------------------------------------------------------
** String conversion
** ----------------------------------------------------------------------- */

/* convert hex character into digit */
int csS_hexvalue(int c) {
    cs_assert(cisxdigit(c));
    if (cisdigit(c)) 
        return c - '0';
    else 
        return (ctolower(c) - 'a') + 10;
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

/*
** Convert string to 'cs_Integer'.
** This function can convert hexadecimal, octal and decimal numerals
** to 'cs_Integer'. Additional conversions are possible for bases up to
** 36, but this function is used for scanner, so it is not required.
*/
static const char *str2int(const char *s, cs_Integer *i, int *oflow) {
    const cs_ubyte *val = table + 1;
    cs_Unsigned lim = CS_INTEGER_MIN;
    int neg = 0;
    uint x;
    cs_Unsigned y;
    int base, c, empty;
    cs_assert(oflow != NULL);
    c = *s++;
    while (cisspace(c)) c = *s++; /* skip leading spaces */
    if (c == '-' || c == '+') { /* have sign? */
        neg = -(c == '-'); /* adjust sign value */
        c = *s++;
    }
    /* handle prefix to get base (if any) */
    if (c == '0' && ctolower(*s) == 'x') { /* hexadecimal? */
        s++; /* skip x | X */
        base = 16;
        c = *s++; /* get first digit */
    } else if (c == '0' && cisodigit(*s)) { /* octal? */
        base = 8;
        c = *s++; /* get first digit */
    } else /* otherwise it must be decimal */
        base = 10; /* c already has first digit */
    /* now do the conversion */
    if (base == 10) {
        empty = !cisdigit(c);
        for (x = 0; cisdigit(c) && x <= UINT_MAX/10-1; c = *s++)
            x = x * 10 + ctodigit(c);
        for (y = x; cisdigit(c) && y <= CS_UNSIGNED_MAX/10 &&
                    10*y <= CS_UNSIGNED_MAX - ctodigit(c); c = *s++)
            y = y * 10 + ctodigit(c);
        if (!cisdigit(c)) goto done;
    } else if (!(base & (base-1))) { /* base is power of 2? (up to base 32) */
        int bs = "\0\1\2\4\7\3\6\5"[(0x17*base)>>5&7];
        empty = val[c] >= base;
        for (x = 0; val[c] < base && x <= UINT_MAX/32; c = *s++)
            x = x<<bs | val[c];
        for (y = x; val[c] < base && y <= CS_UNSIGNED_MAX>>bs; c = *s++)
            y = y<<bs | val[c];
    } else { /* other bases (up to base 36) */
        empty = val[c] >= base;
        for (x = 0; val[c] < base && x <= UINT_MAX/36-1; c = *s++)
            x = x * base + val[c];
        for (y = x; val[c] < base && y <= CS_UNSIGNED_MAX/base &&
                    base*y <= CS_UNSIGNED_MAX-val[c]; c = *s++)
            y = y * base + val[c];
    }
    if (val[c] < base) { /* numeral value too large? */
        for (; val[c] < base; c = *s++); /* skip rest of the digits... */
        y = CS_INTEGER_MIN; /* ...and indicate numeral is too large */
    }
done:
    if (y >= lim) { /* potential overflow? */
        if (!neg) { /* positive value overflows? */
            *oflow = 1; /* propagate overflow */
            *i = csi_castU2S(lim - 1); /* *i = CS_INTEGER_MAX */
        } else if (y > lim) { /* negative value underflows? */
            *oflow = -1; /* propagate underflow */
            *i = csi_castU2S(lim); /* *i = CS_INTEGER_MIN */
        } else /* otherwise y is negative value equal to lim */
            cs_assert(neg && y == lim);
    }
    while (cisspace(c)) c = *s++; /* skip trailing spaces */
    if (empty || c != '\0') return NULL; /* conversion failed? */
    *i = csi_castU2S((y ^ neg) - neg); /* two's complement hack */
    return s - 1;
}


#define converr(eptr)    (*(eptr) != '\0')

static const char *str2flt(const char *s, cs_Number *n, int *oflow) {
    char *eptr;
    cs_assert(oflow != NULL);
    *oflow = 0;
    *n = cs_str2number(s, &eptr);
    if (eptr == s) { /* nothing was converted? */
        return NULL;
    } else if (c_unlikely(oflow && converr(eptr))) {
        /* have overflow flag, converted some characters and error occurred */
        if (*n == CS_HUGEVAL || *n == -CS_HUGEVAL) *oflow = 1; /* overflow */
        else if (*n == CS_NUMBER_MIN) *oflow = -1; /* underflow */
        /* otherwise no (under)overflow, but still have conversion error */
    }
    while (cisspace(*eptr)) eptr++; /* skip trailing spaces */
    return (!converr(eptr) ? eptr : NULL);
}


/* convert string to 'cs_Number' or 'cs_Integer' */
size_t csS_tonum(const char *s, TValue *o, int *poflow) {
    cs_Integer i;
    cs_Number n;
    const char *e;
    int oflow = 0;
    if ((e = str2int(s, &i, &oflow)) != NULL) {
        setival(o, i);
    } else if ((e = str2flt(s, &n, &oflow)) != NULL) {
        setfval(o, n);
    } else /* both conversions failed */
        return 0;
    if (poflow) *poflow = oflow; /* propagate overflow */
    return (e - s) + 1; /* return size */
}


/*
** Maximum conversion length of a number to a string.
** 'long double' (not supported currently) can be 33 digits
** + sign + decimal point + exponent sign + 5 exponent digits
** + null terminator (43 total).
** All other types require less space.
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
            buff[len++] = cs_getlocaledecpoint();
            buff[len++] = '0';
        }
    }
    return len;
}


const char *csS_numtostr(const TValue *v, size_t *plen) {
    static char buff[MAXNUM2STR];
    size_t len = num2buff(v, buff);
    if (plen) *plen = len;
    return buff;
}


int csS_utf8esc(char *buff, ulong n) {
    int x = 1; /* number of bytes put in buffer (backwards) */
    cs_assert(n <= 0x7FFFFFFFu);
    if (n < 0x80) /* ascii? */
        buff[UTF8BUFFSZ - 1] = cast_char(n);
    else { /* need continuation bytes */
        uint mfb = 0x3f; /* maximum that fits in first byte */
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
** Initial size of buffer used in 'csS_newvstringf'
** to prevent allocations, instead the function
** will directly work on the buffer and will push
** strings on stack in case buffer exceeds this limit.
** This is all done because 'csS_newvstringf' often
** gets called by 'csD_getinfo'; the size should be
** at least 'CS_MAXSRC' + 'MAXNUM2STR' + size for message.
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
** Pushes 'str' to the stack and concatenates it with
** other string on the stack if 'pushed' is set.
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
            setfval(&nv, va_arg(argp, cs_Number));
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
    pushbuff(&buff);
    return getstr(strval(s2v(ts->sp.p - 1)));
}


const char *csS_pushfstring(cs_State *ts, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    const char *str = csS_pushvfstring(ts, fmt, argp);
    va_end(argp);
    return str;
}
