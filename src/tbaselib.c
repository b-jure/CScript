/*
** tbaselib.c
** Basic library
** See Copyright Notice in tokudae.h
*/

#define tbaselib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <string.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"
#include "tokudaelimits.h"


/* {=====================================================================
** Clone
** ====================================================================== */

/*
** Index where cache table is stored.
*/
#define CACHEINDEX  1


/* repeated error message when cloning objects (for 'csL_check_stack') */
static const char *errclone = "too many values to clone";


/*
** Create a mapping 'ct[originalobject] = clonedobject' in the cache table.
** The value at 'index' is the original object, while the value on stack
** top is the clone of that value. This creates a graph of references,
** such that, each time we try to clone a new value, we first check if
** the value we are cloning is "repeated". If we have already "seen" this
** value, we will find it in cache table, and the value is the already
** cloned object of the original one. This way we just re-use the cloned
** object, emulating the way original objects were stored.
*/
static void cacheobject(toku_State *T, int index) {
    toku_push(C, index); /* key */
    toku_push(C, -2); /* value */
    toku_set_field(C, CACHEINDEX);
}


/*
** Check if the original value at index has the mapping in the cache table.
** If so, re-use the already existing clone.
*/
static int checkcache(toku_State *T, int index) {
    int res = 1;
    toku_assert(index == -1 || CACHEINDEX < index);
    toku_push(C, index); /* copy of original value */
    if (toku_get_field(C, CACHEINDEX) == TOKU_T_NIL) { /* no cache hit? */
        toku_pop(C, 1); /* remove nil */
        res = 0; /* false; not found */
    } /* otherwise, the clone is on stack top */
    return res;
}


/*
** More often than not, the value to query in cache table is
** on stack top.
*/
#define checkcachetop(C)   checkcache(C, -1)


/* recursive clone functions */
static void cloneclass(toku_State *T, int index);
static void auxclone(toku_State *T, int t, int index);


static void clonelist(toku_State *T, int index) {
    int absi; /* absolute index of list value */
    toku_Integer l;
    toku_assert(CACHEINDEX < index);
    csL_check_stack(C, 4, errclone); /* list+value+cache */
    l = toku_len(C, index);
    toku_push_list(C, cast_int(l));
    absi = toku_getntop(C);
    for (int i = 0; i < l; i++) {
        auxclone(C, toku_get_index(C, index, i), absi);
        toku_set_index(C, -2, i);
    }
    cacheobject(C, index);
}


static void clonetable(toku_State *T, int index) {
    int absi; /* absolute index for table key */
    toku_assert(CACHEINDEX < index);
    csL_check_stack(C, 6, errclone); /* table+2xkey+value+cache */
    toku_push_table(C, cast_int(toku_len(C, index)));
    toku_push_nil(C); /* initial key for 'toku_nextfield' */
    absi = toku_gettop(C);
    while (toku_nextfield(C, index)) {
        toku_push(C, absi); /* key copy for 'toku_nextfield' */
        auxclone(C, toku_type(C, absi), absi);
        auxclone(C, toku_type(C, absi+1), absi+1);
        toku_push(C, absi);
        toku_push(C, absi+1);
        toku_set_raw(C, absi-1);
        toku_pop(C, 2); /* key/value */
    }
    cacheobject(C, index);
}


/* auxiliary clone function for meta objects */
static void clonemetalist(toku_State *T, int index) {
    toku_assert(CACHEINDEX < index);
    if (toku_get_metalist(C, index)) {
        if (!checkcachetop(C))
            clonelist(C, toku_gettop(C));
        toku_replace(C, -2); /* replace original */
        toku_set_metalist(C, -2);
    }
}

/* auxiliary clone function for meta objects */
static void clonemethods(toku_State *T, int index) {
    toku_assert(CACHEINDEX < index);
    if (toku_get_methods(C, index)) {
        if (!checkcachetop(C))
            clonetable(C, toku_gettop(C));
        toku_replace(C, -2); /* replace original */
        toku_set_methods(C, -2);
    }
}

/* auxiliary clone function for meta objects */
static void clonesuperclass(toku_State *T, int index) {
    toku_assert(CACHEINDEX < index);
    if (toku_get_superclass(C, index)) {
        if (!checkcachetop(C))
            cloneclass(C, toku_gettop(C));
        toku_replace(C, -2); /* replace original */
        toku_set_superclass(C, -2);
    }
}


static void cloneclass(toku_State *T, int index) {
    toku_assert(CACHEINDEX < index);
    csL_check_stack(C, 4, errclone); /* class+(list/table/superclass)+cache */
    toku_push_class(C);
    clonemetalist(C, index);
    clonemethods(C, index);
    clonesuperclass(C, index);
    cacheobject(C, index);
}


static void cloneuservalues(toku_State *T, int index, t_uint nuv) {
    toku_assert(CACHEINDEX < index);
    for (t_uint i = 0; i < nuv; i++) {
        toku_get_uservalue(C, index, i);
        auxclone(C, toku_type(C, -1), toku_absindex(C, -1));
        toku_set_uservalue(C, -2, i);
    }
}


static void cloneuserdata(toku_State *T, int index) {
    void *p;
    t_uint nuv = toku_numuservalues(C, check_exp(CACHEINDEX < index, index));
    size_t size = toku_lenudata(C, index);
    csL_check_stack(C, 5, errclone); /* udata+(mlist/methods/2xudval)+cache */
    p = toku_push_userdata(C, size, nuv);
    memcpy(p, toku_to_userdata(C, index), size);
    clonemethods(C, index);
    if (toku_get_metalist(C, index))
        toku_set_metalist(C, -2); /* keep metalist the same */
    cloneuservalues(C, index, nuv);
    cacheobject(C, index);
}


static void cloneinstance(toku_State *T, int index) {
    toku_assert(CACHEINDEX < index);
    csL_check_stack(C, 4, errclone); /* ins+class+cache */
    toku_get_class(C, index);
    if (!checkcachetop(C))
        cloneclass(C, toku_gettop(C));
    toku_replace(C, -2); /* replace original */
    toku_push_instance(C, -1);
    toku_remove(C, -2); /* remove class */
    toku_get_fields(C, index);
    if (!checkcachetop(C))
        clonetable(C, toku_gettop(C));
    toku_replace(C, -2); /* replace original */
    toku_set_fields(C, -2);
    cacheobject(C, index);
}


static void clonebmethod(toku_State *T, int index) {
    int type;
    toku_assert(CACHEINDEX < index);
    csL_check_stack(C, 5, errclone); /* bmethod+self+method+cache */
    type = toku_get_self(C, index);
    if (!checkcachetop(C)) {
        if (type == TOKU_T_USERDATA) /* self is userdata? */
            cloneuserdata(C, toku_gettop(C));
        else /* otherwise self is instance */
            cloneinstance(C, toku_gettop(C));
    }
    toku_replace(C, -2); /* replace original */
    toku_get_method(C, index);
    if (!checkcachetop(C)) /* not in cache table? */
        auxclone(C, toku_type(C, -1), toku_gettop(C));
    else /* otherwise cached value is on stack top */
        toku_replace(C, -2); /* replace original */
    toku_push_method(C, -2);
    toku_remove(C, -2); /* remove self */
    cacheobject(C, index);
}


static int trycopy(toku_State *T, int t, int index) {
    switch (t) {
        case TOKU_T_NIL: case TOKU_T_BOOL: case TOKU_T_NUMBER:
        case TOKU_T_LIGHTUSERDATA: case TOKU_T_FUNCTION:
        case TOKU_T_STRING: case TOKU_T_THREAD: {
            toku_push(C, index);
            return 1;
        }
        default: return 0;
    }
}


static void auxclone(toku_State *T, int t, int index) {
    toku_assert(CACHEINDEX < index); /* index is absolute */
    if (!trycopy(C, t, index) && !checkcache(C, index)) {
        /* value is object not in cache */
        switch (t) { /* clone it */
            case TOKU_T_LIST: clonelist(C, index); break;
            case TOKU_T_TABLE: clonetable(C, index); break;
            case TOKU_T_CLASS: cloneclass(C, index); break;
            case TOKU_T_INSTANCE: cloneinstance(C, index); break;
            case TOKU_T_USERDATA: cloneuserdata(C, index); break;
            case TOKU_T_BMETHOD: clonebmethod(C, index); break;
            default: toku_assert(0); /* unreachable */
        }
    }
    toku_replace(C, index); /* replace original */
}


// TODO: add docs
static int b_clone(toku_State *T) {
    csL_check_any(C, 0);
    toku_setntop(C, 1); /* leave only object to copy on top */
    toku_push_table(C, 0); /* push cache table */
    toku_push(C, 0); /* copy of value to clone */
    auxclone(C, toku_type(C, 2), 2);
    return 1;
}

/* }===================================================================== */


static int b_error(toku_State *T) {
    int level = csL_opt_integer(C, 1, 1);
    toku_setntop(C, 1); /* leave only message on top */
    if (toku_type(C, 0) == TOKU_T_STRING && level >= 0) {
        csL_where(C, level); /* push extra information */
        toku_push(C, 0); /* push original error message... */
        toku_concat(C, 2); /* ...and concatenate it with extra information */
        /* error message with extra information is now on top */
    }
    return toku_error(C);
}


static int b_assert(toku_State *T) {
    if (t_likely(toku_to_bool(C, 0))) { /* true? */
        return toku_getntop(C); /* get all arguments */
    } else { /* failed assert (error) */
        csL_check_any(C, 0); /* must have a condition */
        toku_remove(C, 0); /* remove condition */
        toku_push_literal(C, "assertion failed!"); /* push default error msg */
        toku_setntop(C, 1); /* leave only one message on top */
        return b_error(C);
    }
}


/*
** Auxiliary macro for 'b_gc' that checks if GC option 'optnum' is valid.
*/
#define checkres(v)   { if (v == -1) break; }


// TODO: update docs
static int b_gc(toku_State *T) {
    static const char *const opts[] = {"stop", "restart", "collect",
        "check", "count", "step", "param", "isrunning", "incremental", NULL};
    static const int numopts[] = {TOKU_GC_STOP, TOKU_GC_RESTART, TOKU_GC_COLLECT,
        TOKU_GC_CHECK, TOKU_GC_COUNT,  TOKU_GC_STEP, TOKU_GC_PARAM, TOKU_GC_ISRUNNING,
        TOKU_GC_INC};
    int opt = numopts[csL_check_option(C, 0, "collect", opts)];
    switch (opt) {
        case TOKU_GC_CHECK: {
            int hadcollection = toku_gc(C, opt);
            checkres(hadcollection);
            toku_push_bool(C, hadcollection);
            return 1;
        }
        case TOKU_GC_COUNT: {
            int kb = toku_gc(C, opt); /* Kbytes */
            int b = toku_gc(C, TOKU_GC_COUNTBYTES); /* leftover bytes */
            checkres(kb);
            toku_push_number(C, cast_num(kb) + (cast_num(b)/1024));
            return 1;
        }
        case TOKU_GC_STEP: {
            int nstep = cast_int(csL_opt_integer(C, 1, 0));
            int completecycle = toku_gc(C, opt, nstep);
            checkres(completecycle);
            toku_push_bool(C, completecycle);
            return 1;
        }
        case TOKU_GC_PARAM: {
            static const char *const params[] = {
                "pause", "stepmul", "stepsize", NULL};
            int param = csL_check_option(C, 1, NULL, params);
            int value = cast_int(csL_opt_integer(C, 2, -1));
            toku_push_integer(C, toku_gc(C, opt, param, value));
            return 1;
        }
        case TOKU_GC_ISRUNNING: {
            int running = toku_gc(C, opt);
            checkres(running);
            toku_push_bool(C, running);
            return 1;
        }
        case TOKU_GC_INC: {
            int pause = cast_int(csL_opt_integer(C, 1, -1));
            int stepmul = cast_int(csL_opt_integer(C, 2, -1));
            int stepsize = cast_int(csL_opt_integer(C, 3, -1));
            int oldmode = toku_gc(C, opt, pause, stepmul, stepsize);
            checkres(oldmode);
            toku_assert(oldmode == TOKU_GC_INC);
            toku_push_string(C, "incremental");
            return 1;
        }
        default: {
            int res = toku_gc(C, opt);
            checkres(res);
            toku_push_integer(C, res);
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


static const char *genericreader(toku_State *T, void *ud, size_t *sz) {
    (void)ud; /* unused */
    csL_check_stack(C, 2, "too many nested functions");
    toku_push(C, 0); /* push func... */
    toku_call(C, 0, 1); /* ...and call it */
    if (toku_is_nil(C, -1)) { /* nothing else to read? */
        toku_pop(C, 1); /* pop result */
        *sz = 0;
        return NULL;
    } else if (t_unlikely(!toku_is_string(C, -1))) /* top is not a string? */
        csL_error(C, "reader function must return a string");
    toku_replace(C, RESERVEDSLOT); /* move string into reserved slot */
    return toku_to_lstring(C, RESERVEDSLOT, sz);
}


static int auxload(toku_State *T, int status, int envidx) {
    if (t_likely(status == TOKU_STATUS_OK)) {
        if (envidx != 0) { /* 'env' parameter? */
            toku_push(C, envidx); /* environment for loaded function */
            if (!toku_setupvalue(C, -2, 0)) /* set it as 1st upvalue */
                toku_pop(C, 1); /* remove 'env' if not used by previous call */
        }
        return 1; /* compiled function */
    } else { /* error (message is on top of the stack) */
        csL_push_fail(C); /* push fail */
        toku_insert(C, -2); /* and put it before error message */
        return 2; /* return fail + error message */
    }
}


// TODO: update docs
static int b_load(toku_State *T) {
    int status;
    size_t l;
    const char *s = toku_to_lstring(C, 0, &l);
    int env = (!toku_is_none(C, 2) ? 2 : 0);
    if (s != NULL) { /* loading a string? */
        const char *chunkname = csL_opt_string(C, 1, s);
        status = csL_loadbuffer(C, s, l, chunkname);
    } else { /* loading from a reader function */
        const char *chunkname = csL_opt_string(C, 1, "=(load)");
        csL_check_type(C, 0, TOKU_T_FUNCTION); /* 'chunk' must be a function */
        toku_setntop(C, RESERVEDSLOT+1); /* create reserved slot */
        status = toku_load(C, genericreader, NULL, chunkname);
    }
    return auxload(C, status, env);
}


// TODO: update docs
static int b_loadfile(toku_State *T) {
    const char *filename = csL_opt_string(C, 0, NULL);
    int env = (!toku_is_none(C, 1) ? 1 : 0);
    int status = csL_loadfile(C, filename);
    return auxload(C, status, env);
}


static int b_runfile(toku_State *T) {
    const char *filename = csL_opt_string(C, 0, NULL);
    toku_setntop(C, 1);
    if (t_unlikely(csL_loadfile(C, filename) != TOKU_STATUS_OK))
        return toku_error(C);
    toku_call(C, 0, TOKU_MULTRET);
    return toku_gettop(C); /* all except the 'filename' */
}


// TODO: update docs
static int b_getmetalist(toku_State *T) {
    csL_check_any(C, 0);
    if (!toku_get_metalist(C, 0)) {
        toku_push_nil(C);
        return 1; /* no metalist */
    }
    csL_get_metaindex(C, 0, TOKU_MT_METALIST);
    return 1; /* return either metalist tag value (if present) or metalist */
}


#define expectmeta(C,t,index) \
        csL_expect_arg(C, t == TOKU_T_CLASS || t == TOKU_T_INSTANCE, index, \
                          "class/instance")


// TODO: update docs
static int b_setmetalist(toku_State *T) {
    int t = toku_type(C, 0);
    expectmeta(C, t, 0);
    t = toku_type(C, 1);
    csL_expect_arg(C, t == TOKU_T_NIL || t == TOKU_T_LIST, 1, "nil/list");
    if (t_unlikely(csL_get_metaindex(C, 0, TOKU_MT_METALIST) != TOKU_T_NONE))
        return csL_error(C, "cannot change a protected metalist");
    toku_setntop(C, 2);
    toku_set_metalist(C, 0);
    return 1;
}


// TODO: add docs
static int b_unwrapmethod(toku_State *T) {
    csL_check_type(C, 0, TOKU_T_BMETHOD);
    toku_get_self(C, 0);
    toku_get_method(C, 0);
    return 2;
}


// TODO: add docs
static int b_getmethods(toku_State *T) {
    int t = toku_type(C, 0);
    expectmeta(C, t, 0);
    toku_setntop(C, 1);
    if (!toku_get_methods(C, 0))
        toku_push_nil(C);
    return 1;
}


// TODO: add docs
static int b_setmethods(toku_State *T) {
    int t = toku_type(C, 0);
    expectmeta(C, t, 0);
    if (toku_is_none(C, 1))
        toku_push_nil(C);
    else if (!toku_is_nil(C, 1))
        csL_check_type(C, 1, TOKU_T_TABLE);
    toku_setntop(C, 2);
    toku_set_methods(C, 0);
    return 1;
}


static int b_nextfield(toku_State *T) {
    int t = toku_type(C, 0);
    csL_expect_arg(C, (t == TOKU_T_INSTANCE || t == TOKU_T_TABLE), 0,
                       "instance/table");
    toku_setntop(C, 2); /* if 2nd argument is missing create it */
    if (toku_nextfield(C, 0)) /* found field? */
        return 2; /* key (index) + value */
    else {
        toku_push_nil(C);
        return 1;
    }
}


static int b_pairs(toku_State *T) {
    csL_check_any(C, 0);
    toku_push_cfunction(C, b_nextfield);  /* will return generator, */
    toku_push(C, 0);                      /* state, */
    toku_push_nil(C);                     /* and initial value */
    return 3;
}


static int ipairsaux(toku_State *T) {
    toku_Integer i;
    csL_check_type(C, 0, TOKU_T_LIST);
    i = csL_check_integer(C, 1);
    i = csL_intop(+, i, 1);
    toku_push_integer(C, i);
    toku_get_index(C, 0, i);
    return (toku_len(C, 0) <= i) ? 1 : 2;
}


static int b_ipairs(toku_State *T) {
    csL_check_type(C, 0, TOKU_T_LIST);
    toku_push_cfunction(C, ipairsaux); /* iteration function */
    toku_push(C, 0); /* state */
    toku_push_integer(C, -1); /* initial value */
    return 3;
}


static int finishpcall(toku_State *T, int status, int extra) {
    if (t_unlikely(status != TOKU_STATUS_OK)) {
        toku_push_bool(C, 0);     /* false */
        toku_push(C, -2);         /* error message */
        return 2;               /* return false, message */
    } else
        return toku_getntop(C) - extra; /* return all */
}


static int b_pcall(toku_State *T) {
    int status;
    csL_check_any(C, 0);
    toku_push_bool(C, 1); /* first result if no errors */
    toku_insert(C, 0); /* insert it before the object being called */
    status = toku_pcall(C, toku_getntop(C) - 2, TOKU_MULTRET, -1);
    return finishpcall(C, status, 0);
}


static int b_xpcall(toku_State *T) {
    int status;
    int nargs = toku_getntop(C) - 2;
    csL_check_type(C, 1, TOKU_T_FUNCTION); /* check error function */
    toku_push_bool(C, 1); /* first result */
    toku_push(C, 0); /* function */
    toku_rotate(C, 2, 2); /* move them below function's arguments */
    status = toku_pcall(C, nargs, TOKU_MULTRET, 1);
    return finishpcall(C, status, 2);
}


static int b_print(toku_State *T) {
    int n = toku_getntop(C);
    for (int i = 0; i < n; i++) {
        size_t len;
        const char *str = csL_to_lstring(C, i, &len);
        if (i > 0)
            toku_writelen(stdout, "\t", 1);
        toku_writelen(stdout, str, len);
        toku_pop(C, 1); /* pop result from 'csL_to_string' */
    }
    toku_writeline(stdout);
    return 0;
}


static int b_printf(toku_State *T) {
    size_t lfmt;
    const char *fmt = csL_check_lstring(C, 0, &lfmt);
    const char *efmt = fmt + lfmt;
    int top = toku_gettop(C);
    int arg = 0;
    csL_Buffer b;
    csL_buff_initsz(C, &b, lfmt);
    while (fmt < efmt) {
        if (*fmt != '%') {
            csL_buff_push(&b, *fmt++);
            continue;
        } else if (*++fmt == '%') {
            csL_buff_push(&b, *fmt++);
            continue;
        } /* else '%' */
        if (++arg > top) /* too many format specifiers? */
            return csL_error_arg(C, arg, "missing format value");
        csL_to_lstring(C, arg, NULL);
        csL_buff_push_stack(&b);
    }
    csL_buff_end(&b);
    fmt = toku_to_lstring(C, -1, &lfmt);
    toku_writelen(stdout, fmt, lfmt);
    toku_writeline(stdout);
    return 0;
}


static int b_warn(toku_State *T) {
    int n = toku_getntop(C);
    int i;
    csL_check_string(C, 0); /* at least one string */
    for (i = 1; i < n; i++)
        csL_check_string(C, i);
    for (i = 0; i < n - 1; i++)
        toku_warning(C, toku_to_string(C, i), 1);
    toku_warning(C, toku_to_string(C, n - 1), 0);
    return 0;
}


static int b_len(toku_State *T) {
    int t = toku_type(C, 0);
    csL_expect_arg(C, t == TOKU_T_LIST || t == TOKU_T_TABLE || t == TOKU_T_INSTANCE
                      || t == TOKU_T_CLASS || t == TOKU_T_STRING, 0,
                      "list/table/class/instance/string");
    toku_push_integer(C, toku_len(C, 0));
    return 1;
}


static int b_rawequal(toku_State *T) {
    csL_check_any(C, 0); /* lhs */
    csL_check_any(C, 1); /* rhs */
    toku_push_bool(C, toku_rawequal(C, 0, 1));
    return 1;
}


static int auxrawget(toku_State *T, int field) {
    int t = toku_type(C, 0);
    csL_expect_arg(C, t == TOKU_T_INSTANCE || t == TOKU_T_TABLE, 0, "instance/table");
    csL_check_any(C, 1); /* key */
    toku_setntop(C, 2);
    if (field) /* only field? */
        toku_get_field(C, 0);
    else /* otherwise get either field or method */
        toku_get_raw(C, 0);
    return 1;
}


// TODO: update docs (now accepts table)
static int b_rawget(toku_State *T) {
    return auxrawget(C, 0);
}


// TODO: add docs
static int b_getfield(toku_State *T) {
    return auxrawget(C, 1);
}


static int b_rawset(toku_State *T) {
    csL_check_type(C, 0, TOKU_T_INSTANCE);
    csL_check_any(C, 1); /* key */
    csL_check_any(C, 2); /* value */
    toku_setntop(C, 3);
    toku_set_raw(C, 0); /* this pops index and value */
    return 1; /* return object */
}


static int b_getargs(toku_State *T) {
    toku_Integer i;
    int nres = toku_getntop(C) - 1;
    if (toku_type(C, 0) == TOKU_T_STRING) {
        const char *what = toku_to_string(C, 0);
        if (strcmp(what, "list") == 0) { /* list? */
            toku_push_list(C, nres); /* push the list */
            toku_replace(C, 0); /* move in place of string */
            while (nres--) /* set the list indices */
                toku_set_index(C, 0, nres);
        } else if (strcmp(what, "table") == 0) { /* hashset? */
            toku_push_table(C, nres); /* push the table (hashset) */
            toku_replace(C, 0); /* move in place of string */
            while (nres--) { /* set the table fields */
                toku_push_bool(C, 1);
                toku_set_field(C, 0);
            }
        } else if (strcmp(what, "last") == 0) { /* last? */
            i = (nres > 0) ? nres-1 : 0; /* get last argument */
            goto l_getargs;
        } else if (strcmp(what, "len") == 0) /* len? */
            toku_push_integer(C, nres); 
        else
            csL_error_arg(C, 0,
            "invalid mode, expected \"list\"/\"table\"/\"len\"/\"last\"");
        return 1; /* return (list|table|len) */
    } else {
        i = csL_check_integer(C, 0);
    l_getargs:
        if (i < 0) i = nres + i;
        else if (i > nres) i = nres - 1;
        csL_check_arg(C, 0 <= i, 0, "index out of range");
        return nres - cast_int(i); /* return 'nres-i' results */
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
** Converts string to 'toku_Integer', skips leading and trailing whitespace,
** checks for overflows and underflows and checks if 's' is valid numeral
** string. Conversion works for bases [2,36] and hexadecimal and octal
** literal strings.
*/
static const char *strtoint(const char *s, int base, toku_Integer *pn, int *of) {
    const unsigned char *val = table+1;
    toku_Unsigned lim = TOKU_INTEGER_MIN;
    toku_Unsigned n = 0;
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
    } while (val[c] < base && n <= TOKU_UNSIGNED_MAX/base &&
             base*n <= TOKU_UNSIGNED_MAX-val[c]);
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


static int b_tonum(toku_State *T) {
    int overflow = 0;
    if (toku_is_noneornil(C, 1)) { /* no base? */
        if (toku_type(C, 0) == TOKU_T_NUMBER) { /* number ? */
            toku_setntop(C, 1); /* set it as top */
            return 1; /* return it */
        } else { /* must be string */
            size_t l;
            const char *s = toku_to_lstring(C, 0, &l);
            if (s != NULL && toku_stringtonumber(C, s, &overflow) == l + 1)
                goto done;
            /* else not a number */
            csL_check_any(C, 0); /* (but there must be some parameter) */
        }
    } else { /* have base */
        size_t l;
        toku_Integer i = csL_check_integer(C, 1); /* base */
        const char *s = csL_check_lstring(C, 0, &l); /* numeral to convert */
        toku_Integer n;
        csL_check_arg(C, 2 <= i && i <= 36, 1, "base out of range [2,36]");
        if (strtoint(s, i, &n, &overflow) == s + l) { /* conversion ok? */
            toku_push_integer(C, n); /* push the conversion number */
done:
            if (overflow) {
                toku_push_integer(C, overflow);
                return 2; /* return number + over(under)flow flag (-1|1) */
            }
            return 1; /* return only number */ 
        }
    }
    csL_push_fail(C); /* conversion failed */
    return 1;
}


static int b_tostr(toku_State *T) {
    csL_check_any(C, 0);
    csL_to_lstring(C, 0, NULL);
    return 1;
}


static int b_typeof(toku_State *T) {
    csL_check_any(C, 0);
    toku_push_string(C, csL_typename(C, 0));
    return 1;
}


static int b_getclass(toku_State *T) {
    toku_setntop(C, 2);
    csL_check_any(C, 0);
    if (toku_type(C, 0) == TOKU_T_INSTANCE) /* argument is instance? */
        if (!toku_is_noneornil(C, 1)) /* have key? */
            toku_get_method(C, 0); /* get method */
        else /* otherwise only have instance */
            toku_get_class(C, 0); /* get it's class */
    else /* argument is not an instance */
        csL_push_fail(C);
    return 1;
}


static int b_getsuper(toku_State *T) {
    int t = toku_type(C, 0);
    toku_setntop(C, 2);
    csL_expect_arg(C, t == TOKU_T_CLASS || t == TOKU_T_INSTANCE, 0,
                      "class/instance");
    if (!toku_get_superclass(C, 0))
        toku_push_nil(C); /* value has no superclass */
    else if (!toku_is_noneornil(C, 1)) { /* get superclass method? */
        toku_pop(C, 1); /* remove superclass */
        csL_check_type(C, 0, TOKU_T_INSTANCE);
        toku_get_supermethod(C, 0);
    }
    return 1;
}


/* {=====================================================================
** RANGE
** ====================================================================== */

#define RANGE_VALUES(C) \
    toku_Integer start = toku_to_integer(C, toku_upvalueindex(0)); \
    toku_Integer stop = toku_to_integer(C, toku_upvalueindex(1)); \
    toku_Integer step = toku_to_integer(C, toku_upvalueindex(2));


static void pushrangeres(toku_State *T, toku_Integer start, toku_Integer next) {
    toku_push_integer(C, start); /* current range value */
    toku_push_integer(C, next); /* next range value */
    toku_replace(C, toku_upvalueindex(0)); /* update upvalue */
}


/* get next range value */
#define getnext(start,step,stop,op,lim) \
        (((start) op (lim)-(step)) ? (start)+(step) : (stop))


static int aux_revrange(toku_State *T) {
    RANGE_VALUES(C);
    toku_assert(step < 0);
    if (start > stop) {
        toku_Integer next = getnext(start, step, stop, >=, TOKU_INTEGER_MIN);
        pushrangeres(C, start, next);
    } else
        toku_push_nil(C);
    return 1; /* return 'next' or nil */
}


static int aux_range(toku_State *T) {
    RANGE_VALUES(C);
    toku_assert(step > 0);
    if (start < stop) {
        toku_Integer next = getnext(start, step, stop, <=, TOKU_INTEGER_MAX);
        pushrangeres(C, start, next);
    } else
        toku_push_nil(C);
    return 1; /* return 'next' or nil */
}


// TODO: add docs
static int b_range(toku_State *T) {
    toku_Integer stop, step;
    toku_Integer start = csL_check_integer(C, 0);
    if (toku_is_noneornil(C, 1)) { /* no stop? */
        stop = start; /* range stops at start... */
        start = 0; /* ...and starts at 0 */
    } else /* have stop value */
        stop = csL_check_integer(C, 1);
    step = csL_opt_integer(C, 2, 1);
    if (t_unlikely(step == 0)) /* invalid range? */
        return csL_error(C, "invalid 'step', expected non-zero integer");
    else { /* push iterator */
        toku_push_integer(C, start);
        toku_push_integer(C, stop);
        toku_push_integer(C, step);
        toku_push_cclosure(C, (step < 0) ? aux_revrange : aux_range, 3);
        return 1; /* return iterator */
    }
}

/* ====================================================================} */


static const csL_Entry basic_funcs[] = {
    {"clone", b_clone},
    {"error", b_error},
    {"assert", b_assert},
    {"gc", b_gc},
    {"load", b_load},
    {"loadfile", b_loadfile},
    {"runfile", b_runfile},
    {"getmetalist", b_getmetalist},
    {"setmetalist", b_setmetalist},
    {"unwrapmethod", b_unwrapmethod},
    {"getmethods", b_getmethods},
    {"setmethods", b_setmethods},
    {"nextfield", b_nextfield},
    {"pairs", b_pairs},
    {"ipairs", b_ipairs},
    {"pcall", b_pcall},
    {"xpcall", b_xpcall},
    {"print", b_print},
    {"printf", b_printf},
    {"warn", b_warn},
    {"len", b_len},
    {"rawequal", b_rawequal},
    {"rawget", b_rawget},
    {"getfield", b_getfield},
    {"rawset", b_rawset},
    {"getargs", b_getargs},
    {"tonum", b_tonum},
    {"tostr", b_tostr},
    {"typeof", b_typeof},
    {"getclass", b_getclass},
    {"getsuper", b_getsuper},
    {"range", b_range},
    /* placeholders */
    {TOKU_GNAME, NULL},
    {"__VERSION", NULL},
    /* compatibility flags */
    {"__POSIX", NULL},
    {"__WINDOWS", NULL},
    /* table for meta indices */
    {"__MT", NULL},
    {NULL, NULL},
};


static void set_compat(toku_State *T, const char *have, const char *missing) {
    if (have) toku_set_fieldstr(C, -2, have);
    toku_push_nil(C);
    toku_set_fieldstr(C, -2, missing);
}


static void set_compat_flags(toku_State *T) {
    #if (defined(TOKU_USE_POSIX)) /* POSIX */
        toku_push_integer(C, TOKU_POSIX_REV);
        set_compat(C, "__POSIX", "__WINDOWS");
    #elif (defined(TOKU_USE_WINDOWS)) /* WINDOWS */
        toku_push_integer(C, _MSC_VER);
        set_compat(C, "__WINDOWS", "__POSIX");
    #else /* ISO C */
        set_compat(C, NULL, "__POSIX");
        set_compat(C, NULL, "__WINDOWS");
    #endif
}


/* create __MT table that holds meta tags */
static void create_meta(toku_State *T) {
    const char *mm[TOKU_MT_NUM] = { /* ORDER MT */
        "getidx", "setidx", "gc", "close", "call", "init", "concat", "add",
        "sub", "mul", "div", "idiv", "mod", "pow", "shl", "shr", "band",
        "bor", "bxor", "unm", "bnot", "eq", "lt", "le", "name", "metalist"
    };
    toku_push_table(C, TOKU_MT_NUM + 1);
    for (int i = 0; i < TOKU_MT_NUM; i++) {
        toku_push_integer(C, i);
        toku_set_fieldstr(C, -2, mm[i]);
    }
    toku_push_integer(C, TOKU_MT_NUM);
    toku_set_fieldstr(C, -2, "tostring");
    toku_push_integer(C, TOKU_MT_NUM + 1);
    toku_set_fieldstr(C, -2, "n");
    toku_set_fieldstr(C, -2, "__MT");
}


CSMOD_API int tokuopen_basic(toku_State *T) {
    /* open lib into global instance */
    toku_push_globaltable(C);
    csL_set_funcs(C, basic_funcs, 0);
    /* set global __G */
    toku_push(C, -1); /* copy of global table */
    toku_set_fieldstr(C, -2, TOKU_GNAME);
    /* set global __VERSION */
    toku_push_literal(C, TOKU_VERSION);
    toku_set_fieldstr(C, -2, "__VERSION");
    /* set compatibility flags */
    set_compat_flags(C);
    /* set global metalist indices */
    create_meta(C);
    return 1;
}
