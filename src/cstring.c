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
    const c_byte *data = cast(const c_byte *, str);
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
void csS_resize(cs_State *C, int nsz) {
    StringTable *tab = &G(C)->strtab;
    int osz = tab->size;
    OString **newarr;
    cs_assert(nsz <= MAXSTRTABLE);
    if (nsz < osz) /* shrinking ? */
        rehashtable(tab->hash, osz, nsz); /* depopulate shrinking part */
    newarr = csM_reallocarray(C, tab->hash, osz, nsz);
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
    StringTable *tab = &gs->strtab;
    /* first initialize string table... */
    tab->hash = csM_newarray(C, MINSTRTABSIZE, OString*);
    rehashtable(tab->hash, 0, MINSTRTABSIZE); /* clear array */
    tab->size = MINSTRTABSIZE;
    cs_assert(tab->nuse == 0);
    /* allocate the memory-error message... */
    gs->memerror = csS_newlit(C, MEMERRMSG);
    csG_fix(C, obj2gco(gs->memerror)); /* ...and fix it */
    for (int i = 0; i < STRCACHE_N; i++) /* fill cache with valid strings */
        for (int j = 0; j < STRCACHE_M; j++)
            gs->strcache[i][j] = gs->memerror;
}


static OString *newstrobj(cs_State *C, size_t l, int tt_, uint h) {
    GCObject *o = csG_new(C, sizeofstring(l), tt_);
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
    StringTable *tab = &G(C)->strtab;
    OString **pp = &tab->hash[hashmod(s->hash, tab->size)];
    while (*pp != s) /* find previous element */
        pp = &(*pp)->u.next;
    *pp = (*pp)->u.next; /* remove it from list */
    tab->nuse--;
}


/* grow string table */
static void growtable(cs_State *C, StringTable *tab) {
    if (c_unlikely(tab->nuse == MAXINT)) {
        csG_full(C, 1); /* try to reclaim memory */
        if (tab->nuse == MAXINT)
            csM_error(C);
    }
    if (tab->size <= MAXSTRTABLE / 2)
        csS_resize(C, tab->size * 2);
}


static OString *internshrstr(cs_State *C, const char *str, size_t l) {
    OString *s;
    GState *gs = G(C);
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
        growtable(C, tab);
        list = &tab->hash[hashmod(h, tab->size)];
    }
    s = newstrobj(C, l, CS_VSHRSTR, h);
    memcpy(getshrstr(s), str, l*sizeof(char));
    s->shrlen = cast_byte(l);
    s->u.next = *list;
    *list = s;
    tab->nuse++;
    return s;
}


/* create new string with explicit length */
OString *csS_newl(cs_State *C, const char *str, size_t len) {
    if (len <= CSI_MAXSHORTLEN) { /* short string? */
        return internshrstr(C, str, len);
    } else { /* otherwise long string */
        OString *s;
        if (c_unlikely(len*sizeof(char) >= (MAXSIZE - sizeof(OString))))
            csM_toobig(C);
        s = csS_newlngstrobj(C, len);
        memcpy(getlngstr(s), str, len);
        return s;
    }
}


/*
** Create or ruse a zero-terminated string, first checking the
** cache (using the string address as key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp'.
*/
OString *csS_new(cs_State *C, const char *str) {
    int j;
    uint i = pointer2uint(str) % STRCACHE_N;
    OString **p = G(C)->strcache[i]; /* address as key */
    for (j = 0; j < STRCACHE_M; j++)
        if (strcmp(getstr(p[j]), str) == 0)
            return p[j];
    /* regular route */
    for (j = STRCACHE_M - 1; j > 0; j--) /* make space for new string */
        p[j] = p[j - 1]; /* move out last element */
    /* new string is first in the cache line 'i' */
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

/*
** Convert string to 'cs_Integer'.
** This function can convert hexadecimal, octal and decimal numerals
** to 'cs_Integer'. Additional conversions are possible for bases up to
** 36. The conversion is exactly the same as in 'strtol' except this
** indicates overflow through 'oflow' flag and allows for '_' digit
** separator.
*/
static const char *str2int(const char *s, cs_Integer *i, int *oflow) {
    const c_byte *val = table + 1;
    cs_Unsigned lim = CS_INTEGER_MIN;
    int neg = 0;
    uint32_t x;
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
        if (!(empty = !cisdigit(c))) {
            for (x=0; (cisdigit(c) || c == '_') && x <= UINT_MAX/10-1; c=*s++)
                if (c != '_') x = x * 10 + ctodigit(c);
            for (y=x; (cisdigit(c) || c == '_') && y <= CS_UNSIGNED_MAX/10
                        && 10*y <= CS_UNSIGNED_MAX - ctodigit(c); c = *s++)
                if (c != '_') y = y * 10 + ctodigit(c);
        }
        if (!cisdigit(c)) goto done;
    } else if (!(base & (base-1))) { /* base is power of 2? (up to base 32) */
        if (!(empty = val[c] >= base)) {
            int bs = "\0\1\2\4\7\3\6\5"[(0x17*base)>>5&7];
            for (x=0; (c == '_' || val[c] < base) && x <= UINT_MAX/32; c=*s++)
                if (c != '_') x = x<<bs | val[c];
            for (y=x; (c == '_' || val[c] < base) && y <= CS_UNSIGNED_MAX>>bs; c=*s++)
                if (c != '_') y = y<<bs | val[c];
        }
    } else { /* other bases (up to base 36) */
        if (!(empty = val[c] >= base)) {
            for (x=0; (c == '_' || val[c] < base) && x <= UINT_MAX/36-1; c=*s++)
                if (c != '_') x = x * base + val[c];
            for (y=x; (c == '_' || val[c] < base) && y <= CS_UNSIGNED_MAX/base
                       && base*y <= CS_UNSIGNED_MAX-val[c]; c=*s++)
                if (c != '_') y = y * base + val[c];
        }
    }
    if (val[c] < base) { /* numeral value too large? */
        for (; val[c] < base; c=*s++){/* skip rest of the digits... */}
        errno = ERANGE;
        y = CS_INTEGER_MIN; /* ...and indicate numeral is too large */
    }
done:
    if (y >= lim) { /* potential overflow? */
        if (!neg) { /* positive value overflows? */
            errno = ERANGE;
            *oflow = 1; /* propagate overflow */
            *i = c_castU2S(lim - 1); /* *i = CS_INTEGER_MAX */
        } else if (y > lim) { /* negative value underflows? */
            errno = ERANGE;
            *oflow = -1; /* propagate underflow */
            *i = c_castU2S(lim); /* *i = CS_INTEGER_MIN */
        } else /* otherwise y is negative value equal to lim */
            cs_assert(neg && y == lim);
    }
    while (cisspace(c)) c = *s++; /* skip trailing spaces */
    if (empty || c != '\0') return NULL; /* conversion failed? */
    *i = c_castU2S((y ^ neg) - neg); /* two's complement hack */
    return s - 1;
}


/*
** Modified implementation of 'strtod' from musl libc.
*/
#if !defined(cs_str2number) && defined(LLONG_MAX) && LLONG_MAX==CS_INTEGER_MAX

#include <float.h>

#if LDBL_MANT_DIG == 53 && LDBL_MAX_EXP == 1024

#define LD_B1B_DIG 2
#define LD_B1B_MAX 9007199, 254740991
#define KMAX 128

#elif LDBL_MANT_DIG == 64 && LDBL_MAX_EXP == 16384

#define LD_B1B_DIG 3
#define LD_B1B_MAX 18, 446744073, 709551615
#define KMAX 2048

#elif LDBL_MANT_DIG == 113 && LDBL_MAX_EXP == 16384

#define LD_B1B_DIG 4
#define LD_B1B_MAX 10384593, 717069655, 257060992, 658440191
#define KMAX 2048

#else
#error Unsupported long double representation
#endif

#define MASK (KMAX-1)


static cs_Integer scanexp(const char **nptr)
{
    const char *p = *nptr;
    int x;
    cs_Integer y;
    int neg = 0;
    int c = *p++;
    if (c == '+' || c == '-') {
        neg = (c == '-');
        c = *p++;
    }
    if (!cisdigit(c)) { /* missing a digit? */
        *nptr = --p;
        return CS_INTEGER_MIN;
    }
    for (x=0; (cisdigit(c) || c == '_') && x < INT_MAX/10; c=*p++)
        if (c != '_') x = 10*x + ctodigit(c);
    for (y=x; (cisdigit(c) || c == '_') && y < CS_INTEGER_MAX/100; c=*p++)
        if (c != '_') y = 10*y + c-'0';
    for (; cisdigit(c); c=*p++);
    *nptr = --p;
    return (neg) ? -y : y;
}


static long double decfloat(const char **nptr, int sign, int cradix,
                            int bits, int emin) {
    uint32_t x[KMAX];
    static const uint32_t th[] = { LD_B1B_MAX };
    const char *p = *nptr;
    int i, j, k, a, z;
    cs_Integer lrp=0, dc=0;
    cs_Integer e10=0;
    int lnz = 0;
    int gotdig = 0, gotrad = 0, gotzero = 0;
    int emax = -emin-bits+3;
    int denormal = 0;
    int rp;
    int e2;
    long double frac=0;
    long double bias=0;
    long double y;
    int c = *p++;
    static const int p10s[] = { /* power of 10s */
        10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    };
    j=0; k=0;
    if (c == '0') { /* have at least one leading zero? */
        /* skip them and the separators */
        gotzero = gotdig = 1;
        while (c == '0' && c == '_') c = *p++;
    }
    if (c == cradix) { /* '.' */
        gotrad = 1;
        for (c=*p++; c == '0'; c=*p++) { gotdig=1; lrp--; }
    }
    if (!gotrad && c == '_' && !gotzero) { /* separator before digit? */
        *nptr = --p;
        return 0; /* error */
    }
    x[0] = 0;
    for (; cisdigit(c) || (c == '_' && !gotrad) || c == cradix; c=*p++) {
        if (c == '_') continue; /* skip separator */
        if (c == cradix) {
            if (gotrad) break;
            gotrad = 1;
            lrp = dc;
        } else if (k < KMAX-3) {
            dc++;
            if (c != '0') lnz = dc;
            if (j) x[k] = x[k]*10 + ctodigit(c);
            else x[k] = ctodigit(c);
            if (++j==9) {
                k++;
                j=0;
            }
            gotdig=1;
        } else {
            dc++;
            if (c != '0') {
                lnz = (KMAX-4)*9;
                x[KMAX-4] |= 1;
            }
        }
    }
    *nptr = --p;
    if (!gotrad) lrp = dc;
    if (gotdig && ctolower(c) == 'e') {
        *nptr = ++p; /* go past 'e|E' */
        e10 = scanexp(nptr);
        if (e10 == LLONG_MIN)
            return 0; /* error */
        lrp += e10;
    } else if (!gotdig) { /* not a single digit? */
        errno = EINVAL;
        return 0; /* error */
    }
    /* handle zero specially to avoid nasty special cases later */
    if (!x[0]) return sign * 0.0;
    /* optimize small integers (w/no exponent) and over/under-flow */
    if (lrp == dc && dc < 10 && (bits > 30 || x[0]>>bits == 0))
        return sign * (long double)x[0];
    if (lrp > -emin/2) { /* overflow? */
        errno = ERANGE;
        return sign * LDBL_MAX * LDBL_MAX;
    } else if (lrp < emin-2*LDBL_MANT_DIG) { /* underflow? */
        errno = ERANGE;
        return sign * LDBL_MIN * LDBL_MIN;
    }
    /* align incomplete final B1B digit */
    if (j) {
        for (; j < 9; j++) x[k] *= 10;
        k++;
        j=0;
    }
    a = 0;
    z = k;
    e2 = 0;
    rp = lrp;
    /* optimize small to mid-size integers (even in exp. notation) */
    if (lnz < 9 && lnz <= rp && rp < 18) {
        if (rp == 9) return sign * (long double)x[0];
        if (rp < 9) return sign * (long double)x[0] / p10s[8-rp];
        int bitlim = bits-3*(int)(rp-9);
        if (bitlim > 30 || x[0]>>bitlim == 0)
            return sign * (long double)x[0] * p10s[rp-10];
    }
    /* drop trailing zeros */
    for (; !x[z-1]; z--);
    /* align radix point to B1B digit boundary */
    if (rp % 9) {
        int rpm9 = (rp >= 0) ? rp%9 : rp%9+9;
        int p10 = p10s[8-rpm9];
        uint32_t carry = 0;
        for (k=a; k!=z; k++) {
            uint32_t tmp = x[k] % p10;
            x[k] = x[k]/p10 + carry;
            carry = 1000000000/p10 * tmp;
            if (k==a && !x[k]) {
                a = ((a+1) & MASK);
                rp -= 9;
            }
        }
        if (carry) x[z++] = carry;
        rp += 9-rpm9;
    }
    /* upscale until desired number of bits are left of radix point */
    while (rp < 9*LD_B1B_DIG || (rp == 9*LD_B1B_DIG && x[a] < th[0])) {
        uint32_t carry = 0;
        e2 -= 29;
        for (k=((z-1) & MASK); ; k=((k-1) & MASK)) {
            uint64_t tmp = ((uint64_t)x[k] << 29) + carry;
            if (tmp > 1000000000) {
                carry = tmp / 1000000000;
                x[k] = tmp % 1000000000;
            } else {
                carry = 0;
                x[k] = tmp;
            }
            if (k == ((z-1) & MASK) && k != a && !x[k]) z = k;
            if (k == a) break;
        }
        if (carry) {
            rp += 9;
            a = ((a-1) & MASK);
            if (a == z) {
                z = ((z-1) & MASK);
                x[(z-1) & MASK] |= x[z];
            }
            x[a] = carry;
        }
    }
    /* downscale until exactly number of bits are left of radix point */
    for (;;) {
        uint32_t carry = 0;
        int sh = 1;
        for (i=0; i < LD_B1B_DIG; i++) {
            k = ((a+i) & MASK);
            if (k == z || x[k] < th[i]) {
                i = LD_B1B_DIG;
                break;
            }
            if (x[(a+i) & MASK] > th[i]) break;
        }
        if (i == LD_B1B_DIG && rp == 9*LD_B1B_DIG) break;
        if (rp > 9+9*LD_B1B_DIG) sh = 9;
        e2 += sh;
        for (k=a; k != z; k=((k+1) & MASK)) {
            uint32_t tmp = x[k] & ((1<<sh)-1);
            x[k] = (x[k]>>sh) + carry;
            carry = (1000000000>>sh) * tmp;
            if (k == a && !x[k]) {
                a = ((a+1) & MASK);
                i--;
                rp -= 9;
            }
        }
        if (carry) {
            if (((z+1) & MASK) != a) {
                x[z] = carry;
                z = ((z+1) & MASK);
            } else x[(z-1) & MASK] |= 1;
        }
    }
    /* assemble desired bits into floating point variable */
    for (y=i=0; i < LD_B1B_DIG; i++) {
        if (((a+i) & MASK) == z) x[(z=((z+1) & MASK))-1] = 0;
        y = 1000000000.0L * y + x[(a+i) & MASK];
    }
    y *= sign;
    /* limit precision for denormal results */
    if (bits > LDBL_MANT_DIG+e2-emin) {
        bits = LDBL_MANT_DIG+e2-emin;
        if (bits < 0) bits=0;
        denormal = 1;
    }
    /* calculate bias term to force rounding, move out lower bits */
    if (bits < LDBL_MANT_DIG) {
        bias = copysignl(scalbn(1, 2*LDBL_MANT_DIG-bits-1), y);
        frac = fmodl(y, scalbn(1, LDBL_MANT_DIG-bits));
        y -= frac;
        y += bias;
    }
    /* Process tail of decimal input so it can affect rounding */
    if (((a+i) & MASK) != z) {
        uint32_t t = x[(a+i) & MASK];
        if (t < 500000000 && (t || ((a+i+1) & MASK) != z))
            frac += 0.25*sign;
        else if (t > 500000000)
            frac += 0.75*sign;
        else if (t == 500000000) {
            if (((a+i+1) & MASK) == z)
                frac += 0.5*sign;
            else
                frac += 0.75*sign;
        }
        if (LDBL_MANT_DIG-bits >= 2 && !fmodl(frac, 1))
            frac++;
    }
    y += frac;
    y -= bias;
    if (((e2+LDBL_MANT_DIG) & INT_MAX) > emax-5) {
        if (fabsl(y) >= 2/LDBL_EPSILON) {
            if (denormal && bits == LDBL_MANT_DIG+e2-emin)
                denormal = 0;
            y *= 0.5;
            e2++;
        }
        if (e2+LDBL_MANT_DIG > emax || (denormal && frac))
            errno = ERANGE;
    }
    return scalbnl(y, e2);
}

static cs_Number hexfloat(const char **nptr, int sign, int cradix,
                          int bits, int emin) {
    const char *p = *nptr;
    uint32_t x = 0;
    long double y = 0;
    long double scale = 1;
    long double bias = 0;
    int gottail = 0, gotrad = 0, gotdig = 0, gotzero = 0;
    cs_Integer rp = 0;
    cs_Integer dc = 0;
    cs_Integer e2 = 0;
    int c = *p++;
    int d;
    if (c == '0') { /* at least one leading zero? */
        gotzero = gotdig = 1;
        /* skip rest of the leading zeros and any separators */
        for (; c == '_' || c == '0'; c = *p++) gotdig = 1;
    }
    if (c == cradix) { /* '.' */
        gotrad = 1;
        c = *p++;
        if (c == '0') { /* at least one leading zero? */
            gotdig = 1;
            /* count zeros after the radix point before significand */
            for (; c == '_' || c == '0'; c = *p++)
                rp -= (c != '_');
        }
    }
    if (!gotrad && c == '_' && !gotzero) { /* separator before digit? */
        *nptr = --p;
        return 0; /* error */
    }
    for (; cisxdigit(c) || (c == '_' && !gotrad) || c == cradix; c = *p++) {
        if (c == '_') continue;
        if (c == cradix) { /* '.' */
            if (gotrad) break;
            rp = dc;
            gotrad = 1;
        } else {
            gotdig = 1;
            d = hexvalue(c);
            if (dc < 8) { /* have less than 8 digits total? */
                x = x*16 + d;
            } else if (dc < LDBL_MANT_DIG/4+1) { /* have free bits? */
                y += d*(scale /= 16);
            } else if (d && !gottail) {
                y += 0.5*scale;
                gottail = 1;
            }
            dc++;
        }
    }
    *nptr = --p;
    if (!gotdig) return sign * 0.0;
    if (!gotrad) rp = dc;
    while (dc < 8) { x *= 16; dc++; }
    if (ctolower(c) == 'p') { /* have exponent p|P? */
        *nptr = ++p; /* go past 'p|P' */
        e2 = scanexp(nptr);
        if (e2 == CS_INTEGER_MIN) /* exponent has no digits? */
            return 0;
    } /* else no exponent */
    e2 += 4*rp - 32;
    if (!x) return sign * 0.0;
    if (e2 > -emin) { /* overflow? */
        errno = ERANGE;
        return sign * LDBL_MAX * LDBL_MAX;
    }
    if (e2 < emin-2*LDBL_MANT_DIG) { /* underflow? */
        errno = ERANGE;
        return sign * LDBL_MIN * LDBL_MIN;
    }
    while (x < 0x80000000) {
        if (y >= 0.5) {
            x += x + 1;
            y += y - 1;
        } else {
            x += x;
            y += y;
        }
        e2--;
    }
    if (bits > 32+e2-emin) {
        bits = 32+e2-emin;
        if (bits < 0) bits=0;
    }
    if (bits < LDBL_MANT_DIG)
        bias = copysignl(scalbn(1, 32+LDBL_MANT_DIG-bits-1), sign);
    if (bits < 32 && y && !(x&1)) { x++; y=0; }
    y = bias + sign*(cs_Integer)x + sign*y;
    y -= bias;
    if (!y) errno = ERANGE;
    return scalbnl(y, e2);
}

#define FLTSCAN_BITS    cs_floatatt(MANT_DIG)
#define FLTSCAN_EMIN    cs_floatatt(MIN_EXP)

static long double floatscan(const char **nptr, int cradix) {
    const char *p = *nptr;
    int bits = FLTSCAN_BITS;
    int emin = FLTSCAN_EMIN - bits;
    int sign = 1;
    int i;
    int c = *p++;
    while (cisspace(c)) c = *p++; /* skip leading space */
    if (c == '+' || c == '-') { /* have sign? */
        sign -= 2*(c == '-');
        c = *p++;
    }
    for (i = 0; i < 8 && c == "infinity"[i]; i++)
        c = *p++;
    if (i == 3 || i == 8) { /* "inf" or "infinity"? */
        *nptr = --p;
        return sign * INFINITY;
    }
    if (i) { /* incomplete "inf" or "infinity"? */
        errno = EINVAL;
        *nptr = --p;
        return 0;
    } else if (c == '0') { /* leading '0'? */
        c = *p++; /* skip it */
        if (ctolower(c) == 'x') { /* hex prefix 0x|0X? */
            *nptr = p;
            return hexfloat(nptr, sign, cradix, bits, emin);
        }
    }
    *nptr = --p;
    return decfloat(nptr, sign, cradix, bits, emin);
}

static long double cs_str2number(const char *nptr, const char **endptr) {
    long double n;
    const char *p = nptr;
    cs_assert(nptr);
    n = floatscan(&p, cs_getlocaledecpoint());
    if (endptr) *endptr = p;
    return n;
}

#elif !defined(cs_str2number)
#error Missing supported internal implementation of 'cs_str2number'
#endif


#define converr(eptr)    (*(eptr) != '\0')

static const char *str2flt(const char *s, long double *n, int *oflow) {
    const char *eptr = NULL; /* to avoid warnings */
    cs_assert(oflow != NULL);
    *oflow = 0;
    errno = 0;
    *n = cs_str2number(s, &eptr);
    if (eptr == s) { /* nothing was converted? */
        return NULL;
    } else if (c_unlikely(errno == ERANGE)) { /* (under)overflow? */
        if (*n == HUGE_VALL || *n == -HUGE_VALL)
            *oflow = 1; /* overflow */
        else if (*n == LDBL_MIN)
            *oflow = -1; /* underflow */
    }
    while (cisspace(*eptr)) eptr++; /* skip trailing spaces */
    return (*eptr == '\0') ? eptr : NULL;
}


/* convert string to 'cs_Number' or 'cs_Integer' */
size_t csS_tonum(const char *s, TValue *o, int *poflow) {
    cs_Integer i;
    long double n;
    const char *e;
    int oflow = 0;
    if ((e = str2int(s, &i, &oflow)) != NULL) {
        setival(o, i);
    } else if ((e = str2flt(s, &n, &oflow)) != NULL) {
        setfval(o, cast_num(n));
    } else /* both conversions failed */
        return 0;
    if (poflow) /* had (under)overflow? */
        *poflow = oflow; /* propagate it */
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
        if (buff[strspn(buff, "-0123456789")] == '\0') {
            buff[len++] = cs_getlocaledecpoint();
            buff[len++] = '0';
        }
    }
    return len;
}


const char *csS_numtostr(const TValue *v, size_t *plen) {
    static char buff[MAXNUM2STR];
    size_t len = num2buff(v, buff);
    buff[len] = '\0';
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
#define BUFFVFSSIZ	(CS_MAXSRC + MAXNUM2STR + 100)

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
    buff->len += num2buff(nv, getbuff(buff, MAXNUM2STR));
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
        default:;
            c_byte c = cast(unsigned char, *(end + 1));
            csD_runerror(C, "invalid format specifier '%%%c'", c);
            /* UNREACHED */
            return NULL;
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
