/*
** cbaselib.c
** Basic library
** See Copyright Notice in cscript.h
*/

#define cbaselib_c
#define CS_LIB

#include "cprefix.h"

#include <string.h>

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "climits.h"


static int b_error(cs_State *C) {
    int level = csL_opt_integer(C, 1, 1);
    cs_setntop(C, 1); /* leave only message on top */
    if (cs_type(C, 0) == CS_T_STRING && level >= 0) {
        csL_where(C, level); /* push extra information */
        cs_push(C, 0); /* push original error message... */
        cs_concat(C, 2); /* ...and concatenate it with extra information */
        /* error message with extra information is now on top */
    }
    return cs_error(C);
}


static int b_assert(cs_State *C) {
    if (c_likely(cs_to_bool(C, 0))) { /* true? */
        return cs_getntop(C); /* get all arguments */
    } else { /* failed assert (error) */
        csL_check_any(C, 0); /* must have a condition */
        cs_remove(C, 0); /* remove condition */
        cs_push_literal(C, "assertion failed"); /* push default error msg */
        cs_setntop(C, 1); /* leave only one message on top */
        return b_error(C);
    }
}


/*
** Auxiliary macro for 'b_gc' that checks if GC option 'optnum' is valid.
*/
#define checkres(v)   { if (v == -1) break; }


// TODO: update docs
static int b_gc(cs_State *C) {
    static const char *const opts[] = {"stop", "restart", "collect",
        "check", "count", "step", "param", "isrunning", "incremental", NULL};
    static const int numopts[] = {CS_GC_STOP, CS_GC_RESTART, CS_GC_COLLECT,
        CS_GC_CHECK, CS_GC_COUNT,  CS_GC_STEP, CS_GC_PARAM, CS_GC_ISRUNNING,
        CS_GC_INC};
    int opt = numopts[csL_check_option(C, 0, "collect", opts)];
    switch (opt) {
        case CS_GC_CHECK: {
            int hadcollection = cs_gc(C, opt);
            checkres(hadcollection);
            cs_push_bool(C, hadcollection);
            return 1;
        }
        case CS_GC_COUNT: {
            int kb = cs_gc(C, opt); /* Kbytes */
            int b = cs_gc(C, CS_GC_COUNTBYTES); /* leftover bytes */
            checkres(kb);
            cs_push_number(C, (cs_Number)kb + ((cs_Number)b/1024));
            return 1;
        }
        case CS_GC_STEP: {
            int nstep = (int)csL_opt_integer(C, 1, 0);
            int completecycle = cs_gc(C, opt, nstep);
            checkres(completecycle);
            cs_push_bool(C, completecycle);
            return 1;
        }
        case CS_GC_PARAM: {
            static const char *const params[] = {
                "pause", "stepmul", "stepsize", NULL};
            int param = csL_check_option(C, 1, NULL, params);
            int value = (int)csL_opt_integer(C, 2, -1);
            cs_push_integer(C, cs_gc(C, opt, param, value));
            return 1;
        }
        case CS_GC_ISRUNNING: {
            int running = cs_gc(C, opt);
            checkres(running);
            cs_push_bool(C, running);
            return 1;
        }
        case CS_GC_INC: {
            int pause = (int)csL_opt_integer(C, 1, -1);
            int stepmul = (int)csL_opt_integer(C, 2, -1);
            int stepsize = (int)csL_opt_integer(C, 3, -1);
            int oldmode = cs_gc(C, opt, pause, stepmul, stepsize);
            checkres(oldmode);
            cs_assert(oldmode == CS_GC_INC);
            cs_push_string(C, "incremental");
            return 1;
        }
        default: {
            int res = cs_gc(C, opt);
            checkres(res);
            cs_push_integer(C, res);
            return 1;
        }
    }
    csL_push_fail(C); /* invalid call (inside a finalizer) */
    return 1;
}


/*
** Reserved slot, above all arguments, to hold a copy of the returned
** string to avoid it being collected while parsed. 'load' has 3
** optional arguments (chunk, chunkname and environment).
*/
#define RESERVEDSLOT    3


static const char *genericreader(cs_State *C, void *ud, size_t *sz) {
    (void)ud; /* unused */
    csL_check_stack(C, 2, "too many nested functions");
    cs_push(C, 0); /* push func... */
    cs_call(C, 0, 1); /* ...and call it */
    if (cs_is_nil(C, -1)) { /* nothing else to read? */
        cs_pop(C, 1); /* pop result */
        *sz = 0;
        return NULL;
    } else if (c_unlikely(!cs_is_string(C, -1))) /* top is not a string? */
        csL_error(C, "reader function must return a string");
    cs_replace(C, RESERVEDSLOT); /* move string into reserved slot */
    return cs_to_lstring(C, RESERVEDSLOT, sz);
}


static int auxload(cs_State *C, int status, int envidx) {
    if (c_likely(status == CS_STATUS_OK)) {
        if (envidx != 0) { /* 'env' parameter? */
            cs_push(C, envidx); /* environment for loaded function */
            if (!cs_setupvalue(C, -2, 0)) /* set it as 1st upvalue */
                cs_pop(C, 1); /* remove 'env' if not used by previous call */
        }
        return 1; /* compiled function */
    } else { /* error (message is on top of the stack) */
        csL_push_fail(C); /* push fail */
        cs_insert(C, -2); /* and put it before error message */
        return 2; /* return fail + error message */
    }
}


// TODO: update docs and tests
static int b_load(cs_State *C) {
    int status;
    size_t l;
    const char *s = cs_to_lstring(C, 0, &l);
    int env = (!cs_is_none(C, 2) ? 2 : 0);
    if (s != NULL) { /* loading a string? */
        const char *chunkname = csL_opt_string(C, 1, s);
        status = csL_loadbuffer(C, s, l, chunkname);
    } else { /* loading from a reader function */
        const char *chunkname = csL_opt_string(C, 1, "(load)");
        csL_check_type(C, 0, CS_T_FUNCTION); /* 'chunk' must be a function */
        cs_setntop(C, RESERVEDSLOT+1); /* create reserved slot */
        status = cs_load(C, genericreader, NULL, chunkname);
    }
    return auxload(C, status, env);
}


// TODO: update docs and tests
static int b_loadfile(cs_State *C) {
    const char *filename = csL_opt_string(C, 0, NULL);
    int env = (!cs_is_none(C, 1) ? 1 : 0);
    int status = csL_loadfile(C, filename);
    return auxload(C, status, env);
}


static int b_runfile(cs_State *C) {
    const char *filename = csL_opt_string(C, 0, NULL);
    cs_setntop(C, 1);
    if (c_unlikely(csL_loadfile(C, filename) != CS_STATUS_OK))
        return cs_error(C);
    cs_call(C, 0, CS_MULRET);
    return cs_gettop(C); /* all except the 'filename' */
}


static int b_getmetalist(cs_State *C) {
    csL_check_any(C, 0);
    if (!cs_get_metalist(C, 0))
        cs_push_nil(C);
    return 1; /* return 'nil' or metalist */
}


static int b_setmetalist(cs_State *C) {
    int t = cs_type(C, 1);
    csL_check_type(C, 0, CS_T_CLASS);
    csL_expect_arg(C, t == CS_T_NIL || t == CS_T_LIST, 1, "nil/list");
    cs_setntop(C, 2);
    cs_set_metalist(C, 0);
    return 1;
}


static int b_nextfield(cs_State *C) {
    int t = cs_type(C, 0);
    csL_expect_arg(C, (t == CS_T_INSTANCE || t == CS_T_TABLE), 0,
                       "instance/table");
    cs_setntop(C, 2); /* if 2nd argument is missing create it */
    if (cs_nextfield(C, 0)) { /* found field? */
        return 2; /* key (index) + value */
    } else {
        cs_push_nil(C);
        return 1;
    }
}


static int b_pairs(cs_State *C) {
    csL_check_any(C, 0);
    cs_push_cfunction(C, b_nextfield);  /* will return generator, */
    cs_push(C, 0);                      /* state, */
    cs_push_nil(C);                     /* and initial value */
    return 3;
}


static int ipairsaux(cs_State *C) {
    cs_Integer i;
    csL_check_type(C, 0, CS_T_LIST);
    i = csL_check_integer(C, 1);
    i = csL_intop(+, i, 1);
    cs_push_integer(C, i);
    cs_get_index(C, 0, i);
    return (cs_len(C, 0) <= i) ? 1 : 2;
}


static int b_ipairs(cs_State *C) {
    csL_check_type(C, 0, CS_T_LIST);
    cs_push_cfunction(C, ipairsaux); /* iteration function */
    cs_push(C, 0); /* state */
    cs_push_integer(C, -1); /* initial value */
    return 3;
}


static int finishpcall(cs_State *C, int status, int extra) {
    if (c_unlikely(status != CS_STATUS_OK)) {
        cs_push_bool(C, 0);     /* false */
        cs_push(C, -2);         /* error message */
        return 2;               /* return false, message */
    } else
        return cs_getntop(C) - extra; /* return all */
}


static int b_pcall(cs_State *C) {
    int status;
    csL_check_any(C, 0);
    cs_push_bool(C, 1); /* first result if no errors */
    cs_insert(C, 0); /* insert it before the object being called */
    status = cs_pcall(C, cs_getntop(C) - 2, CS_MULRET, -1);
    return finishpcall(C, status, 0);
}


static int b_xpcall(cs_State *C) {
    int status;
    int nargs = cs_getntop(C) - 2;
    csL_check_type(C, 1, CS_T_FUNCTION); /* check error function */
    cs_push_bool(C, 1); /* first result */
    cs_push(C, 0); /* function */
    cs_rotate(C, 2, 2); /* move them below function's arguments */
    status = cs_pcall(C, nargs, CS_MULRET, 1);
    return finishpcall(C, status, 1);
}


static int b_print(cs_State *C) {
    int n = cs_getntop(C);
    for (int i = 0; i < n; i++) {
        size_t len;
        const char *str = csL_to_lstring(C, i, &len);
        if (i > 0)
            cs_writelen(stdout, "\t", 1);
        cs_writelen(stdout, str, len);
        cs_pop(C, 1); /* pop result from 'csL_to_string' */
    }
    cs_writeline(stdout);
    return 0;
}


static int b_warn(cs_State *C) {
    int n = cs_getntop(C);
    int i;
    csL_check_string(C, 0); /* at least one string */
    for (i = 1; i < n; i++)
        csL_check_string(C, i);
    for (i = 0; i < n - 1; i++)
        cs_warning(C, cs_to_string(C, i), 1);
    cs_warning(C, cs_to_string(C, n - 1), 0);
    return 0;
}


static int b_len(cs_State *C) {
    int t = cs_type(C, 0);
    csL_expect_arg(C, t == CS_T_LIST || t == CS_T_TABLE || t == CS_T_INSTANCE
                      || t == CS_T_CLASS || t == CS_T_STRING, 0,
                      "list/table/class/instance/string");
    cs_push_integer(C, cs_len(C, 0));
    return 1;
}


static int b_rawequal(cs_State *C) {
    csL_check_any(C, 0); /* lhs */
    csL_check_any(C, 1); /* rhs */
    cs_push_bool(C, cs_rawequal(C, 0, 1));
    return 1;
}


static int b_rawget(cs_State *C) {
    csL_check_type(C, 0, CS_T_INSTANCE);
    csL_check_any(C, 1); /* index */
    cs_setntop(C, 2);
    cs_get_raw(C, 0); /* this pops index */
    return 1;
}


static int b_rawset(cs_State *C) {
    csL_check_type(C, 0, CS_T_INSTANCE);
    csL_check_any(C, 1); /* index */
    csL_check_any(C, 2); /* value */
    cs_setntop(C, 3);
    cs_set_raw(C, 0); /* this pops index and value */
    return 1; /* return object */
}


static int b_getargs(cs_State *C) {
    int nres = cs_getntop(C) - 1;
    if (cs_type(C, 0) == CS_T_STRING) {
        const char *what = cs_to_string(C, 0);
        if (strcmp(what, "list") == 0) { /* list? */
            cs_push_list(C, nres); /* push the list */
            cs_replace(C, 0); /* move in place of string */
            while (nres--) /* set the list indices */
                cs_set_index(C, 0, nres);
        } else if (strcmp(what, "table") == 0) { /* hashset? */
            cs_push_table(C, nres); /* push the table (hashset) */
            cs_replace(C, 0); /* move in place of string */
            while (nres--) { /* set the table fields */
                cs_push_bool(C, 1);
                cs_set_field(C, 0);
            }
        } else if (strcmp(what, "len") == 0) /* len? */
            cs_push_integer(C, nres);
        else
            csL_error_arg(C, 0,
            "invalid string value, expected \"list\", \"table\" or \"len\"");
        return 1; /* return (list|table|len) */
    } else {
        cs_Integer i = csL_check_integer(C, 0);
        if (i < 0) i = nres + i;
        else if (i > nres) i = nres - 1;
        csL_check_arg(C, 0 <= i, 0, "index out of range");
        return nres - (int)i; /* return 'nres-i' results */
    }
}


/* space characters to skip */
#define SPACECHARS      " \f\n\r\t\v"

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
** Converts string to 'cs_Integer', skips leading and trailing whitespace,
** checks for overflows and underflows and checks if 's' is valid numeral
** string. Conversion works for bases [2,36] and hexadecimal and octal
** literal strings.
*/
static const char *strtoint(const char *s, int base, cs_Integer *pn, int *of) {
    const unsigned char *val = table+1;
    cs_Unsigned lim = CS_INTEGER_MIN;
    cs_Unsigned n = 0;
    int sign = 1;
    int c;
    s += strspn(s, SPACECHARS); /* skip leading spaces */
    c = *s++;
    if (c == '-' || c == '+') { /* handle sign */
        sign -= 2*(c == '-');
        c = *s++;
    }
    if (c == '0' && (base == 8 || base == 16)) {
        c = *s++; /* skip 0 */
        if ((c|32) == 'x') { /* hexadecimal prefix? */
            if (base != 16) return NULL; /* octal numeral with x|X */
            c = *s++; /* skip x|X */
        }
    }
    if (!(val[c] < base)) /* no digit? */
        return NULL;
    do {
        if (val[c] >= base) return NULL; /* invalid numeral */
        n = n * base + val[c];
        c = *s++;
    } while (val[c] < base && n <= CS_UNSIGNED_MAX/base &&
             base*n <= CS_UNSIGNED_MAX-val[c]);
    if (val[c] < base) {
        while (val[c] < base) c=*s++;
        if (sign > 0)
            n = lim-1;
        else
            n = lim;
        *of = sign;
    } else if (n >= lim) {
        if (sign > 0) {
            *of = 1;
            n = lim-1;
        } else if (n > lim) {
            *of = -1;
            n = lim;
        }
    }
    s--;
    s += strspn(s, SPACECHARS); /* skip trailing spaces */
    *pn = sign * n;
    return s;
}


static int b_tonum(cs_State *C) {
    int overflow = 0;
    if (cs_is_noneornil(C, 1)) { /* no base? */
        if (cs_type(C, 0) == CS_T_NUMBER) { /* number ? */
            cs_setntop(C, 1); /* set it as top */
            return 1; /* return it */
        } else { /* must be string */
            size_t l;
            const char *s = cs_to_lstring(C, 0, &l);
            if (s != NULL && cs_stringtonumber(C, s, &overflow) == l + 1)
                goto done;
            /* else not a number */
            csL_check_any(C, 0); /* (but there must be some parameter) */
        }
    } else { /* have base */
        size_t l;
        cs_Integer i = csL_check_integer(C, 1); /* base */
        const char *s = csL_check_lstring(C, 0, &l); /* numeral to convert */
        cs_Integer n;
        csL_check_arg(C, 2 <= i && i <= 36, 1, "base out of range [2,36]");
        if (strtoint(s, i, &n, &overflow) == s + l) { /* conversion ok? */
            cs_push_integer(C, n); /* push the conversion number */
done:
            if (overflow) {
                cs_push_integer(C, overflow);
                return 2; /* return number + over(under)flow flag (-1|1) */
            }
            return 1; /* return only number */ 
        }
    }
    csL_push_fail(C); /* conversion failed */
    return 1;
}


static int b_tostr(cs_State *C) {
    csL_check_any(C, 0);
    csL_to_lstring(C, 0, NULL);
    return 1;
}


static int b_typeof(cs_State *C) {
    csL_check_any(C, 0);
    cs_push_string(C, csL_typename(C, 0));
    return 1;
}


static int b_getclass(cs_State *C) {
    csL_check_any(C, 0);
    if (cs_type(C, 0) == CS_T_INSTANCE) /* argument is instance? */
        cs_get_class(C, 0);
    else /* argument is not an instance */
        csL_push_fail(C);
    return 1;
}


static int b_getsuper(cs_State *C) {
    int t = cs_type(C, 0);
    cs_setntop(C, 2);
    csL_expect_arg(C, t == CS_T_CLASS || t == CS_T_INSTANCE, 0,
                      "class/instance");
    if (!cs_get_superclass(C, 0))
        cs_push_nil(C); /* value has no superclass */
    else if (!cs_is_noneornil(C, 1)) { /* get superclass method? */
        cs_pop(C, 1); /* remove superclass */
        csL_check_type(C, 0, CS_T_INSTANCE);
        cs_get_supermethod(C, 0);
    }
    return 1;
}


/* {=====================================================================
** RANGE
** ====================================================================== */

#define RANGE_VALUES(C) \
    cs_Integer start = cs_to_integer(C, cs_upvalueindex(0)); \
    cs_Integer stop = cs_to_integer(C, cs_upvalueindex(1)); \
    cs_Integer step = cs_to_integer(C, cs_upvalueindex(2));


static void pushrangeres(cs_State *C, cs_Integer start, cs_Integer next) {
    cs_push_integer(C, start); /* current range value */
    cs_push_integer(C, next); /* next range value */
    cs_replace(C, cs_upvalueindex(0)); /* update upvalue */
}


static int aux_revrange(cs_State *C) {
    RANGE_VALUES(C);
    cs_assert(step < 0);
    if (start > stop) {
        cs_Integer next = (start >= CS_INTEGER_MIN-step) ? start+step : stop;
        pushrangeres(C, start, next);
    } else
        cs_push_nil(C);
    return 1; /* return 'next' or nil */
}


static int aux_range(cs_State *C) {
    RANGE_VALUES(C);
    cs_assert(step > 0);
    if (start < stop) {
        cs_Integer next = (start <= CS_INTEGER_MAX-step) ? start+step : stop;
        pushrangeres(C, start, next);
    } else
        cs_push_nil(C);
    return 1; /* return 'next' or nil */
}


// TODO: add docs and tests
static int b_range(cs_State *C) {
    cs_Integer stop, step;
    cs_Integer start = csL_check_integer(C, 0);
    if (cs_is_noneornil(C, 1)) { /* no stop? */
        stop = start; /* range stops at start... */
        start = 0; /* ...and starts at 0 */
    } else /* have stop value */
        stop = csL_check_integer(C, 1);
    step = csL_opt_integer(C, 2, 1);
    if (c_unlikely(step == 0)) /* invalid range? */
        return csL_error(C, "invalid 'step' (must be positive or negative)");
    else { /* push iterator */
        cs_push_integer(C, start);
        cs_push_integer(C, stop);
        cs_push_integer(C, step);
        cs_push_cclosure(C, (step < 0) ? aux_revrange : aux_range, 3);
        return 1; /* return iterator */
    }
}

/* ====================================================================} */


static const cs_Entry basic_funcs[] = {
    {"error", b_error},
    {"assert", b_assert},
    {"gc", b_gc},
    {"load", b_load},
    {"loadfile", b_loadfile},
    {"runfile", b_runfile},
    {"getmetalist", b_getmetalist},
    {"setmetalist", b_setmetalist},
    {"nextfield", b_nextfield},
    {"pairs", b_pairs},
    {"ipairs", b_ipairs},
    {"pcall", b_pcall},
    {"xpcall", b_xpcall},
    {"print", b_print},
    {"warn", b_warn},
    {"len", b_len},
    {"rawequal", b_rawequal},
    {"rawget", b_rawget},
    {"rawset", b_rawset},
    {"getargs", b_getargs},
    {"tonum", b_tonum},
    {"tostr", b_tostr},
    {"typeof", b_typeof},
    {"getclass", b_getclass},
    {"getsuper", b_getsuper},
    {"range", b_range},
    /* placeholders */
    {CS_GNAME, NULL},
    {"__VERSION", NULL},
    /* compatibility flags */
    {"__POSIX", NULL},
    {"__WINDOWS", NULL},
    /* metalist indices */
    {"__getidx", NULL},
    {"__setidx", NULL},
    {"__gc", NULL},
    {"__close", NULL},
    {"__call", NULL},
    {"__init", NULL},
    {"__concat", NULL},
    {"__add", NULL},
    {"__sub", NULL},
    {"__mul", NULL},
    {"__div", NULL},
    {"__idiv", NULL},
    {"__mod", NULL},
    {"__pow", NULL},
    {"__shl", NULL},
    {"__shr", NULL},
    {"__band", NULL},
    {"__bor", NULL},
    {"__bxor", NULL},
    {"__unm", NULL},
    {"__bnot", NULL},
    {"__eq", NULL},
    {"__lt", NULL},
    {"__le", NULL},
    {"__tostring", NULL},
    {"__N", NULL},
    {NULL, NULL},
};


static void set_compat(cs_State *C, const char *have, const char *missing) {
    if (have) cs_set_fieldstr(C, -2, have);
    cs_push_nil(C);
    cs_set_fieldstr(C, -2, missing);
}


static void set_compat_flags(cs_State *C) {
    #if (defined(CS_USE_POSIX)) /* POSIX */
        cs_push_integer(C, CS_POSIX_REV);
        set_compat(C, "__POSIX", "__WINDOWS");
    #elif (defined(CS_USE_WINDOWS)) /* WINDOWS */
        cs_push_integer(C, _MSC_VER);
        set_compat(C, "__WINDOWS", "__POSIX");
    #else /* ISO C */
        set_compat(C, NULL, "__POSIX");
        set_compat(C, NULL, "__WINDOWS");
    #endif
}


static void set_metalist_indices(cs_State *C) {
    const char *mm[CS_MM_NUM] = {
        "__getidx", "__setidx", "__gc", "__close", "__call", "__init",
        "__concat", "__add", "__sub", "__mul", "__div", "__idiv", "__mod",
        "__pow", "__shl", "__shr", "__band", "__bor", "__bxor", "__unm",
        "__bnot", "__eq", "__lt", "__le"
    };
    for (int i = 0; i < CS_MM_NUM; i++) {
        cs_push_integer(C, i);
        cs_set_fieldstr(C, -2, mm[i]);
    }
    /* set __tostring metamethod index */
    cs_push_integer(C, CS_MM_NUM);
    cs_set_fieldstr(C, -2, "__tostring");
    /* set total number of metamethods */
    cs_push_integer(C, CS_MM_NUM + 1);
    cs_set_fieldstr(C, -2, "__N");
}


CSMOD_API int csopen_basic(cs_State *C) {
    /* open lib into global instance */
    cs_push_globaltable(C);
    csL_set_funcs(C, basic_funcs, 0);
    /* set global __G */
    cs_push(C, -1); /* copy of global table */
    cs_set_fieldstr(C, -2, CS_GNAME);
    /* set global __VERSION */
    cs_push_literal(C, CS_VERSION);
    cs_set_fieldstr(C, -2, "__VERSION");
    /* set compatibility flags */
    set_compat_flags(C);
    /* set global metalist indices */
    set_metalist_indices(C);
    return 1;
}
