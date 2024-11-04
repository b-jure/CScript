/*
** ccorelib.c
** Core library
** See Copyright Notice in cscript.h
*/

#include "cauxlib.h"
#include "cconf.h"
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
    {NULL, NULL},
};
