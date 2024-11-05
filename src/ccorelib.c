/*
** ccorelib.c
** Core library
** See Copyright Notice in cscript.h
*/

#include <ctype.h>
#include <string.h>

#include "cauxlib.h"
#include "cscript.h"


static int csCore_error(cr_State *ts) {
    int level = crL_opt_integer(ts, 1, 0);
    cr_setntop(ts, 1); /* leave only message on top */
    if (cr_type(ts, 0) == CR_TSTRING && level >= 0) {
        crL_where(ts, level); /* add extra information */
        cr_push(ts, 0); /* push original error message... */
        cr_concat(ts, 2); /* ...and concatenate it with extra information */
        /* error message with extra information is now on top */
    }
    return cr_error(ts);
}


static int csCore_assert(cr_State *ts) {
    if (cr_likely(cr_to_bool(ts, -1))) { /* true? */
        return cr_nvalues(ts); /* get all arguments */
    } else { /* failed assert (error) */
        crL_check_any(ts, 0); /* must have a condition */
        cr_remove(ts, 0); /* remove condition */
        cr_push_literal(ts, "assertion failed"); /* push default err message */
        cr_setntop(ts, 1); /* leave only one message on top */
        return csCore_error(ts);
    }
}


/* check if 'optnum' for 'cr_gc' was valid */
#define checkres(res)   { if (res == -1) break; }

static int csCore_gc(cr_State *ts) {
    static const char *const opts[] = {"stop", "restart", "collect", "count",
        "step", "setpause", "setstepmul", "isrunning", NULL};
    static const int numopts[] = {CR_GCSTOP, CR_GCRESTART, CR_GCCOLLECT,
        CR_GCCOUNT, CR_GCSTEP, CR_GCSETPAUSE, CR_GCSETSTEPMUL, CR_GCISRUNNING};
    int optnum = numopts[crL_check_option(ts, 0, "collect", opts)];
    switch (optnum) {
        case CR_GCCOUNT: {
            int kb = cr_gc(ts, optnum); /* kibibytes */
            int b = cr_gc(ts, CR_GCCOUNTBYTES); /* leftover bytes */
            checkres(kb);
            cr_push_number(ts, (cr_Number)kb + ((cr_Number)b/1024));
            return 1;
        }
        case CR_GCSTEP: {
            int nstep = crL_opt_integer(ts, 1, 0);
            int completecycle = cr_gc(ts, optnum, nstep);
            checkres(completecycle);
            cr_push_bool(ts, completecycle);
            return 1;
        }
        case CR_GCSETPAUSE: case CR_GCSETSTEPMUL: {
            int arg = crL_opt_integer(ts, 1, 0);
            int prev = cr_gc(ts, optnum, arg);
            checkres(prev);
            cr_push_integer(ts, prev);
            return 1;
        }
        case CR_GCISRUNNING: {
            int running = cr_gc(ts, optnum);
            checkres(running);
            cr_push_bool(ts, running);
            return 1;
        }
        default: {
            int res = cr_gc(ts, optnum);
            checkres(res);
            cr_push_integer(ts, res);
            return 1;
        }
    }
    crL_push_fail(ts);
    return 0;
}


/*
** Reserved slot, above all arguments, to hold a copy of the returned
** string to avoid it being collected while parsed. 'load' has two
** optional arguments (chunk and source name).
*/
#define RESERVEDSLOT  2


static const char *loadreader(cr_State *ts, void *ud, size_t *sz) {
    (void)ud; /* unused */
    crL_check_stack(ts, 2, "too many nested functions");
    cr_push(ts, 0); /* push func... */
    cr_call(ts, 0, 1); /* ...and call it */
    if (cr_is_nil(ts, -1)) { /* nothing else to read? */
        cr_pop(ts, 1); /* pop result (nil) */
        *sz = 0;
        return NULL;
    } else if (cr_unlikely(!cr_is_string(ts, -1))) { /* top is not a string? */
        crL_error(ts, "reader function must return a string");
    }
    cr_replace(ts, RESERVEDSLOT); /* move string into reserved slot */
    return crL_to_lstring(ts, RESERVEDSLOT, sz);
}


static int auxload(cr_State *ts, int status) {
    if (cr_unlikely(status != CR_OK)) {
        crL_push_fail(ts); /* push fail */
        cr_insert(ts, -2); /* and put it in front of error message */
        return 2; /* nil + error message */
    }
    return 1; /* compiled function */
}


static int csCore_load(cr_State *ts) {
    int status;
    size_t sz;
    const char *chunkname;
    const char *chunk = cr_to_lstring(ts, 0, &sz);
    if (chunk != NULL) { /* 'chunk' is a string? */
        chunkname = crL_opt_string(ts, 1, chunk);
        status = crL_loadbuffer(ts, chunk, sz, chunkname);
    } else { /* 'chunk' is not a string */
        chunkname = crL_opt_string(ts, 1, "(load)");
        crL_check_type(ts, 0, CR_TFUNCTION); /* 'chunk' must be a function */
        status = cr_load(ts, loadreader, NULL, chunkname);
    }
    return auxload(ts, status);
}


static int csCore_loadfile(cr_State *ts) {
    const char *filename = crL_opt_string(ts, 0, NULL);
    int status = crL_loadfile(ts, filename);
    return auxload(ts, status);
}


static int csCore_runfile(cr_State *ts) {
    const char *filename = crL_opt_string(ts, -1, NULL);
    cr_setntop(ts, 1);
    if (cr_unlikely(crL_loadfile(ts, filename) != CR_OK))
        return cr_error(ts);
    cr_call(ts, 0, CR_MULRET);
    return cr_nvalues(ts) - 1; /* all not including 'filename' */
}


static int csCore_getmetamethod(cr_State *ts) {
    static const char * const opts[] = {"__init", "__getidx", "__setidx",
        "__gc", "__add", "__sub", "__mul", "__div", "__mod", "__pow", "__not",
        "__bnot", "__shl", "__shr", "__band", "__bor", "__xor", "__eq",
        "__lt", "__le", NULL};
    static const cr_MM mmnum[] = {CR_MM_INIT, CR_MM_GETIDX, CR_MM_SETIDX,
        CR_MM_GC, CR_MM_CLOSE, CR_MM_CALL, CR_MM_CONCAT, CR_MM_ADD, CR_MM_SUB,
        CR_MM_MUL, CR_MM_DIV, CR_MM_MOD, CR_MM_POW, CR_MM_BSHL, CR_MM_BSHR,
        CR_MM_BAND, CR_MM_BOR, CR_MM_BXOR, CR_MM_UNM, CR_MM_BNOT, CR_MM_EQ,
        CR_MM_LT, CR_MM_LE};
    cr_MM mm;
    crL_check_any(ts, 0); /* object with metamethods */
    mm = mmnum[crL_check_option(ts, 1, NULL, opts)];
    if (!cr_hasvmt(ts, 0) || (cr_get_metamethod(ts, 0, mm) == CR_TNONE))
        cr_push_nil(ts);
    return 1;
}


static int csCore_next(cr_State *ts) {
    crL_check_type(ts, 0, CR_TINSTANCE);
    cr_setntop(ts, 2); /* if 2nd argument is missing create it */
    if (cr_next(ts, 0)) { /* found field? */
        return 2; /* key (index) + value */
    } else {
        cr_push_nil(ts);
        return 1;
    }
}


static int finishpcall(cr_State *ts, int status, int extra) {
    if (cr_unlikely(status != CR_OK)) {
        cr_push_bool(ts, 0); /* false */
        cr_push(ts, -2); /* error message */
        return 2; /* return false, message */
    } else {
        return cr_nvalues(ts) - extra; /* return all */
    }
}


static int csCore_pcall(cr_State *ts) {
    int status;
    crL_check_any(ts, 0);
    cr_push_bool(ts, 1); /* first result if no errors */
    cr_insert(ts, 0); /* insert it before the object being called */
    status = cr_pcall(ts, cr_nvalues(ts) - 2, CR_MULRET, 0);
    return finishpcall(ts, status, 0);
}


static int csCore_xpcall(cr_State *ts) {
    int status;
    int nargs = cr_nvalues(ts) - 2;
    crL_check_type(ts, 1, CR_TFUNCTION);
    cr_push_bool(ts, 1); /* first result if no errors */
    cr_push(ts, 0); /* function */
    cr_rotate(ts, 2, 2); /* move them below the function's arguments */
    status = cr_pcall(ts, nargs, CR_MULRET, 1);
    return finishpcall(ts, status, 1);
}


static int csCore_print(cr_State *ts) {
    int n = cr_nvalues(ts);
    for (int i = 0; i < n; i++) {
        size_t len;
        const char *str = crL_to_lstring(ts, i, &len);
        if (i > 0)
            cst_writelen(stdout, "\t", 1);
        cst_writelen(stdout, str, len);
        cr_pop(ts, 1); /* pop result from 'crL_to_string' */
    }
    cst_writeline(stdout);
    return 0;
}


static int csCore_warn(cr_State *ts) {
    int n = cr_nvalues(ts);
    int i;
    crL_check_string(ts, 0); /* at least one string */
    for (i = 1; i < n; i++)
        crL_check_string(ts, i);
    for (i = 0; i < n - 1; i++)
        cr_warning(ts, cr_to_string(ts, i), 1);
    cr_warning(ts, cr_to_string(ts, n - 1), 0);
    return 0;
}


static int csCore_rawequal(cr_State *ts) {
    crL_check_any(ts, 0); /* lhs */
    crL_check_any(ts, 1); /* rhs */
    cr_push_bool(ts, cr_rawequal(ts, 0, 1));
    return 1;
}


static int csCore_rawget(cr_State *ts) {
    crL_check_type(ts, 0, CR_TINSTANCE);
    crL_check_any(ts, 1); /* index */
    cr_setntop(ts, 2);
    crL_get_property(ts, 0); /* this pops index */
    return 1; /* return property */
}


static int csCore_rawset(cr_State *ts) {
    crL_check_type(ts, 0, CR_TINSTANCE);
    crL_check_any(ts, 1); /* index */
    crL_check_any(ts, 2); /* value */
    cr_setntop(ts, 3);
    cr_set_field(ts, 1); /* this pops index and value */
    return 1; /* return instance */
}


static int csCore_getargs(cr_State *ts) {
    int n = cr_nvalues(ts);
    if (cr_type(ts, 0) == CR_TSTRING) {
        const char *what = cr_to_string(ts, 0);
        if (strcmp(what, "array") == 0) {
            cr_push_array(ts);
            cr_replace(ts, 0);
            while (--n)
                cr_set_index(ts, 0, n);
        } else if (strcmp(what, "set") == 0) {
            crL_push_hashtable(ts);
            cr_replace(ts, 0);
            while (--n) {
                cr_push_bool(ts, 1);
                cr_set_field(ts, 0);
            }
        } else if (strcmp(what, "len") == 0) {
            cr_push_integer(ts, n - 1);
        } else {
            crL_arg_error(ts, 0,
            "invalid string value, expected \"array\", \"set\" or \"len\"");
        }
        return 1;
    } else {
        cr_Integer i = crL_check_integer(ts, 0);
        if (i < 0) i = n + i;
        else if (++i > n) i = n - 1;
        crL_check_arg(ts, 0 <= i, 0, "index out of range");
        return n - (int)i;
    }
}


/* lookup table for digit values; -1==255>=36 -> invalid */
static const unsigned char numeraltable[] = { -1,
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

/* space characters to skip */
#define SPACECHARS	" \f\n\r\t\v"

/*
** Converts string to 'cr_Integer', skips leading and trailing whitespace,
** checks for overflows and underflows and checks if 's' is valid numeral
** string. Conversion works for bases 2-36 and hexadecimal and octal literal
** strings.
*/
static const char *strtoint(const char *s, int base, cr_Integer *pn, int *of) {
    const unsigned char *val = numeraltable + 1;
    const cr_Unsigned lowlim = CR_INTEGER_MIN;
    const cr_Unsigned lim = CR_UNSIGNED_MAX;
    cr_Unsigned n = 0;
    int c, neg = 0;
    *of = 0; /* reset overflow flag */
    s += strspn(s, SPACECHARS); /* skip leading whitespace */
    c = *s;
    if (c == '+' || c == '-') { /* have sign? */
        neg = -(c == '-');
        c = *s++;
    }
    if ((base == 8 || base == 16) && c == '0') { /* hexadecimal or octal? */
        c = *s++;
        /* (c | 32) => tolower */
        if ((c | 32) == 'x') { /* X or x ? */
            c = *s++;
            if (val[c] >= 16) return NULL; /* missing first digit */
            base = 16; /* set hexadecimal base */
        } else if (base == 0) { /* must be octal */
            base = 8; /* set octal base */
        }
    } else if (val[c] >= base) {
        return NULL;
    }
    if (base == 10) { /* decimal base? */
        for (;isdigit(c) && n <= lim/10 && 10*n <= lim-(c-'0'); c = *s++)
            n = n * 10 + (c - '0');
        if (!isdigit(c)) goto done;
    } else if (!(base & base-1)) { /* base is power of 2? */
        /* get the number of bit shifts depending on the value of base */
        int bs = "\0\1\2\4\7\3\6\5"[(0x17*base)>>5&7];
        for (;isalnum(c) && val[c] < base && n <= lim>>bs; c = *s++)
            n = n<<bs | val[c];
    } else {
        for (;isalnum(c) && val[c]<base && n <= lim/base &&
                base * n <= lim - val[c]; c = *s++)
            n = n * base + val[c];
    }
    if (isalnum(c) && val[c] < base) { /* overflow? */
        *of = 1; /* signal it */
        do {c = *s++;} while(isalnum(c) && val[c] < base); /* skip numerals */
        n = lowlim;
    }
done:
    s--; s += strspn(s, SPACECHARS); /* skip trailing whitespace */
    if (n >= lowlim) { /* potential overflow? */
        if (!neg) { /* overflow */
            *of = 1;
            *pn = lim;
            return s;
        } else if (n > lowlim) { /* underflow? */
            *of = -1;
            *pn = lowlim;
            return s;
        }
    }
    *pn = (cr_Integer)((n^neg) - neg); /* resolve sign and store the result */
    return s;
}


static int csCore_tonumber(cr_State *ts) {
    int overflow = 0;
    if (cr_is_noneornil(ts, 1)) { /* no base? */
        if (cr_type(ts, 0) == CR_TNUMBER) { /* number ? */
            cr_setntop(ts, 1); /* set it as top */
            return 1; /* return it */
        } else { /* must be string */
            const char *s = cr_to_string(ts, 0);
            if (s != NULL && cr_stringtonumber(ts, s, &overflow)) {
                cr_push_bool(ts, overflow);
                return 1;
            }
            crL_check_any(ts, 0);
        }
    } else { /* have base */
        size_t l;
        const char *s;
        cr_Integer n;
        cr_Integer i = crL_check_integer(ts, 1); /* base */
        crL_check_type(ts, 0, CR_TSTRING); /* string to convert */
        s = cr_to_lstring(ts, 0, &l);
        crL_check_arg(ts, 2 <= i && i <= 32, 1, "base out of range");
        if (strtoint(s, i, &n, &overflow) == s + l) { /* conversion ok? */
            cr_push_integer(ts, n); /* push the conversion number */
            cr_push_bool(ts, overflow); /* push overflow boolean */
            return 2;
        }
    }
    crL_push_fail(ts); /* conversion failed */
    return 1; /* return fail */
}


static int csCore_tostring(cr_State *ts) {
    crL_check_number(ts, 0);
    crL_to_lstring(ts, 0, NULL);
    return 1;
}


static int csCore_typeof(cr_State *ts) {
    int tt = cr_type(ts, 0);
    crL_check_arg(ts, tt != CR_TNONE, 0, "value expected");
    cr_push_string(ts, cr_typename(ts, 0));
    return 1;
}


static const cr_Entry core_funcs[] = {
    {"error", csCore_error},
    {"assert", csCore_assert},
    {"gc", csCore_gc},
    {"load", csCore_load},
    {"loadfile", csCore_loadfile},
    {"runfile", csCore_runfile},
    {"getmetamethod", csCore_getmetamethod},
    {"next", csCore_next},
    {"pcall", csCore_pcall},
    {"xpcall", csCore_xpcall},
    {"print", csCore_print},
    {"warn", csCore_warn},
    {"rawequal", csCore_rawequal},
    {"rawget", csCore_rawget},
    {"rawset", csCore_rawset},
    {"getargs", csCore_getargs},
    {"tonumber", csCore_tonumber},
    {"tostring", csCore_tostring},
    {"typeof", csCore_typeof},
    {NULL, NULL},
};
