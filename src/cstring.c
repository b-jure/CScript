/*
** cstring.c
** Functions for CScript string objects
** See Copyright Notice in cscript.h
*/


#define cstring_c
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
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>


/* maximum size for string table */
#define MAXSTRTABLE     MAXINT


/* string equality */
int csS_eqlngstr(const OString *s1, const OString *s2) {
    size_t len = s1->u.lnglen;
    return (s1 == s2) || /* same instance or... */
        ((len == s2->u.lnglen) && /* equal length and... */
        (memcmp(getlngstr(s1), getlngstr(s2), len) == 0)); /* equal contents */
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


uint csS_hash(const char *str, size_t l, unsigned int seed) {
    uint h = seed ^ cast_uint(l);
    for (; l > 0; l--)
        h ^= ((h<<5) + (h>>2) + cast_byte(str[l - 1]));
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
        OString *s = arr[i]; /* get the string at slot (if any) */
        arr[i] = NULL; /* clear the slot */
        while (s) { /* for each string in the chain */
            OString *next = s->u.next; /* save 'next' */
            uint h = hashmod(s->hash, nsz); /* get hash position */
            s->u.next = arr[h]; /* chain it into array */
            arr[h] = s;
            s = next;
        }
    }
}


/* 
** Resize string table. If allocation fails, keep the current size.
*/
void csS_resize(cs_State *C, int nsz) {
    stringtable *tab = &G(C)->strtab;
    int osz = tab->size;
    OString **newarr;
    cs_assert(nsz <= MAXSTRTABLE);
    if (nsz < osz) /* shrinking ? */
        rehashtable(tab->hash, osz, nsz); /* depopulate shrinking part */
    newarr = csM_reallocarray(C, tab->hash, osz, nsz, OString*);
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


void csS_init(cs_State *C) {
    GState *gs = G(C);
    stringtable *tab = &gs->strtab;
    /* first initialize string table... */
    tab->hash = csM_newarray(C, MINSTRTABSIZE, OString*);
    rehashtable(tab->hash, 0, MINSTRTABSIZE); /* clear array */
    tab->size = MINSTRTABSIZE;
    cs_assert(tab->nuse == 0);
    /* allocate the memory-error message */
    gs->memerror = csS_newlit(C, MEMERRMSG);
    csG_fix(C, obj2gco(gs->memerror)); /* fix it */
    for (int i = 0; i < STRCACHE_N; i++) /* fill cache with valid strings */
        for (int j = 0; j < STRCACHE_M; j++)
            gs->strcache[i][j] = gs->memerror;
}


static OString *newstrobj(cs_State *C, size_t l, int tag, uint h) {
    GCObject *o = csG_new(C, sizeofstring(l), tag);
    OString *s = gco2str(o);
    s->hash = h;
    s->extra = 0;
    getstr(s)[l] = '\0'; /* null-terminate */
    return s;
}


OString *csS_newlngstrobj(cs_State *C, size_t len) {
    OString *s = newstrobj(C, len, CS_VLNGSTR, G(C)->seed);
    s->u.lnglen = len;
    s->shrlen = 0xFF;
    return s;
}


void csS_remove(cs_State *C, OString *s) {
    stringtable *tab = &G(C)->strtab;
    OString **pp = &tab->hash[hashmod(s->hash, tab->size)];
    while (*pp != s) /* find previous element */
        pp = &(*pp)->u.next;
    *pp = (*pp)->u.next; /* remove it from list */
    tab->nuse--;
}


/* grow string table */
static void growtable(cs_State *C, stringtable *tab) {
    if (c_unlikely(tab->nuse == MAXINT)) {
        csG_full(C, 1); /* try to reclaim memory */
        if (tab->nuse == MAXINT) /* still too many strings? */
            csM_error(C);
    }
    if (tab->size <= MAXSTRTABLE / 2) /* can grow string table? */
        csS_resize(C, tab->size * 2);
}


static OString *internshrstr(cs_State *C, const char *str, size_t l) {
    OString *s;
    GState *gs = G(C);
    stringtable *tab = &gs->strtab;
    uint h = csS_hash(str, l, gs->seed);
    OString **list = &tab->hash[hashmod(h, tab->size)];
    cs_assert(str != NULL); /* otherwise 'memcmp'/'memcpy' are undefined */
    for (s = *list; s != NULL; s = s->u.next) { /* probe chain */
        if (s->shrlen==l && (memcmp(str, getshrstr(s), l*sizeof(char))==0)) {
            if (isdead(gs, s)) /* dead (but not yet collected)? */
                changewhite(s); /* ressurect it */
            return s;
        }
    }
    /* else must create a new string */
    if (tab->nuse >= tab->size) { /* need to grow the table? */
        growtable(C, tab);
        list = &tab->hash[hashmod(h, tab->size)]; /* rehash with new size */
    }
    s = newstrobj(C, l, CS_VSHRSTR, h);
    s->shrlen = cast_byte(l);
    memcpy(getshrstr(s), str, l*sizeof(char));
    s->u.next = *list;
    *list = s;
    tab->nuse++;
    return s;
}


/* create new string with explicit length */
OString *csS_newl(cs_State *C, const char *str, size_t l) {
    if (l <= CSI_MAXSHORTLEN) { /* short string? */
        return internshrstr(C, str, l);
    } else { /* otherwise long string */
        OString *s;
        if (c_unlikely(l*sizeof(char) >= (MAXSIZE-sizeof(OString))))
            csM_toobig(C);
        s = csS_newlngstrobj(C, l);
        memcpy(getlngstr(s), str, l*sizeof(char));
        return s;
    }
}


/*
** Create or ruse a zero-terminated string, first checking the
** cache (using the string address as key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp'.
*/
OString *csS_new(cs_State *C, const char *str) {
    uint i = pointer2uint(str) % STRCACHE_N; /* hash */
    OString **p = G(C)->strcache[i]; /* address as key */
    int j;
    for (j = 0; j < STRCACHE_M; j++) {
        if (strcmp(str, getstr(p[j])) == 0) /* hit? */
            return p[j]; /* done */
    }
    /* normal route */
    for (j = STRCACHE_M - 1; j > 0; j--) /* make space for new string */
        p[j] = p[j - 1]; /* move out last element */
    /* new string is first in the list */
    p[0] = csS_newl(C, str, strlen(str));
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
        size_t n = limit - SLL("...");
        memcpy(dest, src, n);
        memcpy(&dest[n], "...", SLL("..."));
        len = limit;
    } else {
        memcpy(dest, src, len);
    }
    dest[len] = '\0';
}


void csS_sourceid(char *restrict dest, const char *src, size_t len) {
    csS_strlimit(dest, src, len, CS_MAXSRC - 1);
}


/* -----------------------------------------------------------------------
** String conversion
** ----------------------------------------------------------------------- */

/* convert hex character into digit */
c_sinline int hexvalue(int c) {
    if (c > '9') /* hex digit? */ 
        return (ctolower(c) - 'a') + 10;
    else  /* decimal digit */
        return c - '0';
}

int csS_hexvalue(int c) {
    cs_assert(cisxdigit(c));
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

static const char *str2int(const char *s, cs_Integer *i) {
    const c_byte *val = table + 1;
    cs_Unsigned lim = CS_INTEGER_MIN;
    int sign = 1;
    uint32_t x;
    cs_Unsigned y;
    int base, c, empty;
    c = *s++;
    while (cisspace(c)) c = *s++; /* skip leading spaces */
    if (c == '-' || c == '+') { /* have sign? */
        sign -= 2*(c == '-'); /* adjust sign value */
        c = *s++;
    }
    /* handle prefix to get base (if any) */
    if (c == '0' && ctolower(*s) == 'x') { /* hexadecimal? */
        s++; /* skip x|X */
        base = 16;
        c = *s++; /* get first digit */
    } else if (c == '0') { /* octal? */
        base = 8;
        c = *s++; /* get first digit */
        empty = 0; /* have '0' */
    } else /* otherwise it must be decimal */
        base = 10; /* c already has first digit */
    empty = !(val[c] < base) && empty;
    /* now do the conversion */
    if (base == 10) {
        if (!empty) {  
            for (x=0; val[c]<base && x <= UINT_MAX/10-1; c=*s++)
                x = x * 10 + val[c];
            for (y=x; val[c]<base && y <= CS_UNSIGNED_MAX/10 &&
                      10*y <= CS_UNSIGNED_MAX-val[c]; c=*s++)
                y = y * 10 + val[c];
        }
    } else if (!(base & (base-1))) { /* base is power of 2? (up to base 32) */
        if (!empty) {
            int bs = "\0\1\2\4\7\3\6\5"[(0x17*base)>>5&7];
            for (x=0; val[c] < base && x <= UINT_MAX/32; c=*s++)
                x = x<<bs | val[c];
            for (y=x; val[c]<base && y <= CS_UNSIGNED_MAX>>bs; c=*s++)
                y = y<<bs | val[c];
        }
    } else { /* other bases (up to base 36) */
        if (!empty) {
            for (x=0; val[c]<base && x <= UINT_MAX/36-1; c=*s++)
                x = x * base + val[c];
            for (y=x; val[c] < base && y <= CS_UNSIGNED_MAX/base &&
                      base*y <= CS_UNSIGNED_MAX-val[c]; c=*s++)
                y = y * base + val[c];
        }
    }
    if (val[c] < base || /* 'CS_UNSIGNED_MAX' overflown, */
        (y >= lim && /* or numeral is bigger or equal than 'lim', */
         ((sign > 0 && base != 16) || /* and is positive and not hex, */
          (y > lim && /* or the 'lim' is overflown' */
          /* and is not hex and less than max unsigned int */
          !(base == 16 && y <= CS_UNSIGNED_MAX))))) {
        return NULL; /* over(under)flow (do not accept it as integer) */
    } else {
        while (cisspace(c)) c = *s++; /* skip trailing spaces */
        if (empty || c != '\0') return NULL; /* conversion failed? */
        *i = c_castU2S(sign * y);
        return s - 1;
    }
}


/* maximum length of a numeral to be converted to a number */
#if !defined (C_MAXNUMERAL)
#define C_MAXNUMERAL	200
#endif


static const char *loc_str2flt(const char *s, cs_Number *res, int *pf) {
    char *eptr = NULL; /* to avoid warnings */
    cs_assert(pf != NULL);
    *pf = 0;
    errno = 0;
    *res = cs_str2number(s, &eptr);
    if (eptr == s)
        return NULL; /* nothing was converted? */
    else if (c_unlikely(errno == ERANGE)) {
        if (*res == CS_HUGE_VAL || *res == -CS_HUGE_VAL) {
            *pf = 1; /* overflow (negative/positive infinity) */
            /* explicit 'inf|infinity' does not set errno */
        } else {
            cs_assert(*res <= CS_NUMBER_MIN);
            *pf = -1; /* underflow (very large negative exponent) */
        }
    }
    while (cisspace(*eptr)) eptr++; /* skip trailing spaces */
    return (*eptr == '\0') ? eptr : NULL;
}


static const char *str2flt(const char *s, cs_Number *res, int *pf) {
    const char *endptr = loc_str2flt(s, res, pf);
    if (endptr == NULL) { /* failed? may be a different locale */
        char buff[C_MAXNUMERAL + 1];
        const char *pdot = strchr(s, '.');
        if (pdot == NULL || strlen(s) > C_MAXNUMERAL)
            return NULL; /* no dot or string too long; fail */
        strcpy(buff, s);
        buff[pdot - s] = cs_getlocaledecpoint(); /* correct decimal point */
        endptr = loc_str2flt(s, res, pf);
        if (endptr != NULL)
            endptr = s + (endptr - buff); /* make relative to 's' */
    }
    return endptr;
}


size_t csS_tonum(const char *s, TValue *o, int *pf) {
    const char *e;
    cs_Integer i;
    cs_Number n;
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


unsigned csS_tostringbuff(const TValue *obj, char *buff) {
    size_t len;
    cs_assert(ttisnum(obj));
    if (ttisint(obj)) {
        len = cs_integer2str(buff, CS_N2BUFFSZ, ival(obj));
    } else {
        len = cs_number2str(buff, CS_N2BUFFSZ, fval(obj));
        /* if it looks like integer append '.0' */
        if (buff[strspn(buff, "-0123456789")] == '\0') {
            buff[len++] = cs_getlocaledecpoint();
            buff[len++] = '0'; /* adds ".0" to result */
        }
    }
    cs_assert(len < CS_N2BUFFSZ);
    return cast_uint(len);
}


void csS_tostring(cs_State *C, TValue *obj) {
    char buff[CS_N2BUFFSZ];
    uint len = csS_tostringbuff(obj, buff);
    setstrval(C, obj, csS_newl(C, buff, len));
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
#define BUFFVFSSIZ	(CS_MAXSRC + CS_N2BUFFSZ + 100)

/* buffer for 'csS_newvstringf' */
typedef struct BuffVFS {
    cs_State *C;
    int pushed; /* true if 'space' was pushed on the stack */
    int len; /* string length in 'space' */
    char space[BUFFVFSSIZ];
} BuffVFS;


static void initvfs(cs_State *C, BuffVFS *vfs) {
    vfs->len = vfs->pushed = 0;
    vfs->C = C;
}


/*
** Pushes 'str' to the stack and concatenates it with
** other string on the stack if 'pushed' is set.
*/
static void pushstr(BuffVFS *buff, const char *str, size_t len) {
    cs_State *C = buff->C;
    OString *s = csS_newl(C, str, len);
    setstrval2s(C, C->sp.p, s);
    C->sp.p++;
    if (buff->pushed)
        csV_concat(C, 2);
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
    buff->len += csS_tostringbuff(nv, getbuff(buff, CS_N2BUFFSZ));
}


/* add pointer to buffer */
static void buffaddptr(BuffVFS *buff, const void *p) {
    const int psize = 3 * sizeof(void*) + 8;
    buff->len += cs_pointer2str(getbuff(buff, psize), psize, p);
}


/* Create new string object from format 'fmt' and args in 'argp'. */
const char *csS_pushvfstring(cs_State *C, const char *fmt, va_list argp) {
    const char *end;
    TValue nv;
    BuffVFS buff;
    initvfs(C, &buff);
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
            case 'f': { /* 'cs_Number' */
                setfval(&nv, va_arg(argp, cs_Number));
                buffaddnum(&buff, &nv);
                break;
            }
            case 'U': {  /* a 'long' as a UTF-8 sequence */
                char bf[UTF8BUFFSZ];
                int len = csS_utf8esc(bf, va_arg(argp, long));
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
                c_byte c = cast(unsigned char, *(end + 1));
                csD_runerror(C, "invalid format specifier '%%%c'", c);
                /* UNREACHED */
                return NULL;
            }
        }
        fmt = end + 2; /* '%' + specifier */
    }
    buffaddstring(&buff, fmt, strlen(fmt));
    pushbuff(&buff);
    return getstr(strval(s2v(C->sp.p - 1)));
}


const char *csS_pushfstring(cs_State *C, const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    const char *str = csS_pushvfstring(C, fmt, argp);
    va_end(argp);
    return str;
}
