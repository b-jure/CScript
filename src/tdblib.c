/*
** tdblib.c
** Interface from Tokudae to its debug API
** See Copyright Notice in tokudae.h
*/

#define tdblib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <stdio.h>
#include <string.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"
#include "tokudaelimits.h"


/*
** The hook table at ctable[HOOKKEY] maps threads to their current
** hook function.
*/
static const char *const HOOKKEY = "__HOOKKEY";


/*
** If C1 != C, C1 can be in any state, and therefore there are no
** guarantees about its stack space; any push in C1 must be
** checked.
*/
static void checkstack(toku_State *T, toku_State *T1, int n) {
    if (t_unlikely(C != C1 && !toku_checkstack(C1, n)))
        tokuL_error(C, "stack overflow");
}


static int db_getctable(toku_State *T) {
    toku_push(C, TOKU_CTABLE_INDEX);
    return 1;
}


static int db_getclist(toku_State *T) {
    toku_push(C, TOKU_CLIST_INDEX);
    return 1;
}


static int db_getuservalue(toku_State *T) {
    int n = (int)tokuL_opt_integer(C, 1, 0);
    if (toku_type(C, 0) != TOKU_T_USERDATA)
        tokuL_push_fail(C);
    else if (toku_get_uservalue(C, 0, n) != TOKU_T_NONE) {
        toku_push_bool(C, 1);
        return 2;
    }
    return 1;
}


static int db_setuservalue(toku_State *T) {
    int n = (int)tokuL_opt_integer(C, 2, 0);
    tokuL_check_type(C, 0, TOKU_T_USERDATA);
    tokuL_check_any(C, 1);
    toku_setntop(C, 2);
    if (!toku_set_uservalue(C, 0, n))
        tokuL_push_fail(C);
    return 1;
}


/*
** Auxiliary function used by several library functions: check for
** an optional thread as function's first argument and set 'arg' with
** 0 if this argument is present (so that functions can skip it to
** access their other arguments)
*/
static toku_State *getthread(toku_State *T, int *arg) {
    if (toku_is_thread(C, 0)) {
        *arg = 0;
        return toku_to_thread(C, 0);
    } else {
        *arg = -1;
        return C; /* function will operate over current thread */
    }
}


/*
** Variations of 'toku_set_field', used by 'db_getinfo' to put results
** from 'toku_getinfo' into result table. Key is always a string;
** value can be a string, an int, or a bool.
*/
static void settabss(toku_State *T, const char *k, const char *v) {
    toku_push_string(C, v);
    toku_set_fieldstr(C, -2, k);
}

static void settabsi(toku_State *T, const char *k, int v) {
    toku_push_integer(C, v);
    toku_set_fieldstr(C, -2, k);
}

static void settabsb(toku_State *T, const char *k, int v) {
    toku_push_bool(C, v);
    toku_set_fieldstr(C, -2, k);
}


/*
** In function 'db_getinfo', the call to 'toku_getinfo' may push
** results on the stack; later it creates the result table to put
** these objects. Function 'treatstackoption' puts the result from
** 'toku_getinfo' on top of the result table so that it can call
** 'toku_set_fieldstr'.
*/
static void treatstackoption(toku_State *T, toku_State *T1, const char *fname) {
    if (C == C1)
        toku_rotate(C, -2, 1); /* exchange object and table */
    else
        toku_xmove(C1, C, 1); /* move object to the "main" stack */
    toku_set_fieldstr(C, -2, fname); /* put object into table */
}


/*
** Calls 'toku_getinfo' and collects all results in a new table.
** C1 needs stack space for an optional input (function) plus
** two optional outputs (function and line table) from function
** 'toku_getinfo'.
*/
static int db_getinfo(toku_State *T) {
    int arg;
    toku_Debug ar;
    toku_State *T1 = getthread(C, &arg);
    const char *options = tokuL_opt_string(C, arg + 2, "flnsru");
    checkstack(C, C1, 3);
    tokuL_check_arg(C, options[0] != '>', arg + 2, "invalid option '>'");
    if (toku_is_function(C, arg + 1)) { /* info about a function? */
        options = toku_push_fstring(C, ">%s", options); /* add '>' to 'options' */
        toku_push(C, arg+1); /* move function to 'C1' stack */
        toku_xmove(C, C1, 1);
    } else { /* stack level */
        if (!toku_getstack(C1, (int)tokuL_check_integer(C, arg + 1), &ar)) {
            tokuL_push_fail(C); /* level out of range */
            return 1;
        }
    }
    if (!toku_getinfo(C1, options, &ar))
        return tokuL_error_arg(C, arg+2, "invalid option");
    toku_push_table(C, 0); /* table to collect results */
    if (strchr(options, 's')) {
        toku_push_lstring(C, ar.source, ar.srclen);
        toku_set_fieldstr(C, -2, "source");
        settabss(C, "shortsrc", ar.shortsrc);
        settabsi(C, "defline", ar.defline);
        settabsi(C, "lastdefline", ar.lastdefline);
        settabss(C, "what", ar.what);
    }
    if (strchr(options, 'l'))
        settabsi(C, "currline", ar.currline);
    if (strchr(options, 'u')) {
        settabsi(C, "nupvals", ar.nupvals);
        settabsi(C, "nparams", ar.nparams);
        settabsb(C, "isvararg", ar.isvararg);
    }
    if (strchr(options, 'n')) {
        settabss(C, "name", ar.name);
        settabss(C, "namewhat", ar.namewhat);
    }
    if (strchr(options, 'r')) {
        settabsi(C, "ftransfer", ar.ftransfer);
        settabsi(C, "ntransfer", ar.ntransfer);
    }
    if (strchr(options, 'L'))
        treatstackoption(C, C1, "activelines");
    if (strchr(options, 'f'))
        treatstackoption(C, C1, "func");
    return 1; /* return table */
}


static int db_getlocal(toku_State *T) {
    int arg;
    toku_State *T1 = getthread(C, &arg);
    int nvar = (int)tokuL_check_integer(C, arg + 2); /* local-variable index */
    if (toku_is_function(C, arg + 1)) { /* function argument? */
        toku_push(C, arg + 1); /* push function */
        toku_push_string(C, toku_getlocal(C, NULL, nvar)); /* push local name */
        return 1; /* return only name (there is no value) */
    } else { /* stack-level argument */
        toku_Debug ar;
        const char *name;
        int level = (int)tokuL_check_integer(C, arg + 1);
        if (t_unlikely(!toku_getstack(C1, level, &ar)))  /* out of range? */
            return tokuL_error_arg(C, arg+1, "level out of range");
        checkstack(C, C1, 1);
        name = toku_getlocal(C1, &ar, nvar);
        if (name) {
            toku_xmove(C1, C, 1); /* move local value */
            toku_push_string(C, name); /* push name */
            toku_rotate(C, -2, 1); /* re-order */
            return 2;
        } else {
            tokuL_push_fail(C); /* no name (nor value) */
            return 1;
        }
    }
}


static int db_setlocal(toku_State *T) {
    int arg;
    toku_Debug ar;
    const char *name;
    toku_State *T1 = getthread(C, &arg);
    int level = (int)tokuL_check_integer(C, arg + 1);
    int nvar = (int)tokuL_check_integer(C, arg + 2);
    if (t_unlikely(!toku_getstack(C1, level, &ar)))  /* out of range? */
        return tokuL_error_arg(C, arg+1, "level out of range");
    tokuL_check_any(C, arg+3);
    toku_setntop(C, arg+4);
    checkstack(C, C1, 1); /* ensure space for value */
    toku_xmove(C, C1, 1); /* move value (4th or 3rd parameter) */
    name = toku_setlocal(C1, &ar, nvar);
    if (name == NULL) /* no local was found? */
        toku_pop(C1, 1); /* pop value (if not popped by 'toku_setlocal') */
    toku_push_string(C, name);
    return 1;
}


/*
** get (if 'get' is true) or set an upvalue from a closure
*/
static int auxupvalue(toku_State *T, int get) {
    const char *name;
    int n = (int)tokuL_check_integer(C, 1); /* upvalue index */
    tokuL_check_type(C, 0, TOKU_T_FUNCTION); /* closure */
    name = get ? toku_getupvalue(C, 0, n) : toku_setupvalue(C, 0, n);
    if (name == NULL) return 0;
    toku_push_string(C, name);
    toku_insert(C, -(get+1)); /* no-op if 'get' is false */
    return get + 1;
}


static int db_getupvalue(toku_State *T) {
    return auxupvalue(C, 1);
}


static int db_setupvalue(toku_State *T) {
    tokuL_check_any(C, 2);
    return auxupvalue(C, 0);
}


/*
** Checks whether a given upvalue from a given closure exists and
** returns its index.
*/
static void *checkupval(toku_State *T, int argf, int argnup, int *pnup) {
    void *id;
    int nup = (int)tokuL_check_integer(C, argnup); /* upvalue index */
    tokuL_check_type(C, argf, TOKU_T_FUNCTION); /* closure */
    id = toku_upvalueid(C, argf, nup);
    if (pnup) {
        tokuL_check_arg(C, id != NULL, argnup, "invalid upvalue index");
        *pnup = nup;
    }
    return id;
}


static int db_upvalueid(toku_State *T) {
    void *id = checkupval(C, 0, 1, NULL);
    if (id != NULL)
        toku_push_lightuserdata(C, id);
    else
        tokuL_push_fail(C);
    return 1;
}


static int db_upvaluejoin(toku_State *T) {
    int n1, n2;
    checkupval(C, 0, 1, &n1);
    checkupval(C, 2, 3, &n2);
    tokuL_check_arg(C, !toku_is_cfunction(C, 0), 0, "Tokudae function expected");
    tokuL_check_arg(C, !toku_is_cfunction(C, 2), 2, "Tokudae function expected");
    toku_upvaluejoin(C, 0, n1, 2, n2);
    return 0;
}


/*
** Call hook function registered at hook table for the current
** thread (if there is one).
*/
static void hookf(toku_State *T, toku_Debug *ar) {
    static const char *const hooknames[] = {"call","return","line","count"};
    toku_get_cfieldstr(C, HOOKKEY);
    toku_push_thread(C);
    if (toku_get_raw(C, -2) == TOKU_T_FUNCTION) { /* is there a hook function? */
        toku_push_string(C, hooknames[ar->event]); /* push event name */
        if (ar->currline >= 0)
            toku_push_integer(C, ar->currline); /* push current line */
        else toku_push_nil(C);
        toku_assert(toku_getinfo(C, "ls", ar));
        toku_call(C, 2, 0); /* call hook function */
    }
}


/*
** Convert a string mask (for 'sethook') into a bit mask
*/
static int makemask(const char *smask, int count) {
    int mask = 0;
    if (strchr(smask, 'c')) mask |= TOKU_MASK_CALL;
    if (strchr(smask, 'r')) mask |= TOKU_MASK_RET;
    if (strchr(smask, 'l')) mask |= TOKU_MASK_LINE;
    if (count > 0) mask |= TOKU_MASK_COUNT;
    return mask;
}


/*
** Convert a bit mask (for 'gethook') into a string mask
*/
static char *unmakemask(int mask, char *smask) {
    int i = 0;
    if (mask & TOKU_MASK_CALL) smask[i++] = 'c';
    if (mask & TOKU_MASK_RET) smask[i++] = 'r';
    if (mask & TOKU_MASK_LINE) smask[i++] = 'l';
    smask[i] = '\0';
    return smask;
}


static int db_sethook(toku_State *T) {
    int arg, mask, count;
    toku_Hook func;
    toku_State *T1 = getthread(C, &arg);
    if (toku_is_noneornil(C, arg+1)) { /* no hook? */
        toku_setntop(C, arg+2);
        func = NULL; mask = 0; count = 0; /* turn off hooks */
    } else {
        const char *smask = tokuL_check_string(C, arg+2);
        tokuL_check_type(C, arg+1, TOKU_T_FUNCTION);
        count = (int)tokuL_opt_integer(C, arg + 3, 0);
        func = hookf; mask = makemask(smask, count);
    }
    tokuL_get_subtable(C, TOKU_CTABLE_INDEX, HOOKKEY);
    checkstack(C, C1, 1);
    toku_push_thread(C1); toku_xmove(C1, C, 1); /* key (thread) */
    toku_push(C, arg + 1); /* value (hook function) */
    toku_set_raw(C, -3); /* hooktable[C1] = new Tokudae hook */
    toku_sethook(C1, func, mask, count);
    return 0;
}


static int db_gethook(toku_State *T) {
    int arg;
    char buff[5];
    toku_State *T1 = getthread(C, &arg);
    int mask = toku_gethookmask(C1);
    toku_Hook hook = toku_gethook(C1);
    if (hook == NULL) { /* no hook? */
        tokuL_push_fail(C);
        return 1;
    } else if (hook != hookf) /* external hook? */
        toku_push_literal(C, "external hook");
    else { /* hook table must exist */
        toku_get_cfieldstr(C, HOOKKEY);
        checkstack(C, C1, 1);
        toku_push_thread(C1); toku_xmove(C1, C, 1);
        toku_get_raw(C, -2); /* 1st result = hooktable[C1] */
        toku_remove(C, -2); /* remove hook table */
    }
    toku_push_string(C, unmakemask(mask, buff)); /* 2nd result = mask */
    toku_push_integer(C, toku_gethookcount(C1)); /* 3rd result = count */
    return 3;
}


/*
** Maximum size of input, when in interactive mode after calling
** 'db_debug'.
*/
#if !defined(T_MAXDBLINE)
#define T_MAXDBLINE     250
#endif

static int db_debug(toku_State *T) {
    for (;;) {
        char buffer[T_MAXDBLINE];
        toku_writefmt(stderr, "%s", "tokudae_debug> ");
        if (fgets(buffer, sizeof(buffer), stdin) == NULL ||
                strcmp(buffer, "cont\n") == 0)
            return 0;
        if (tokuL_loadbuffer(C, buffer, strlen(buffer), "(debug command)") ||
                toku_pcall(C, 0, 0, -1))
            toku_writefmt(stderr, "%s\n", tokuL_to_lstring(C, -1, NULL));
        toku_setntop(C, 0); /* remove eventual returns */
    }
}


static int db_traceback(toku_State *T) {
    int arg;
    toku_State *T1 = getthread(C, &arg);
    const char *msg = toku_to_string(C, arg + 1);
    if (msg == NULL && !toku_is_noneornil(C, arg + 1)) /* non-string 'msg'? */
        toku_push(C, arg + 1); /* return it untouched */
    else {
        int level = (int)tokuL_opt_integer(C, arg + 2, (C == C1) ? 1 : 0);
        tokuL_traceback(C, C1, level, msg);
    }
    return 1;
}


static int db_stackinuse(toku_State *T) {
    int res = toku_stackinuse(getthread(C, &res));
    toku_push_integer(C, res);
    return 1;
}


static const tokuL_Entry dblib[] = {
    {"debug", db_debug},
    {"getuservalue", db_getuservalue},
    {"gethook", db_gethook},
    {"getinfo", db_getinfo},
    {"getlocal", db_getlocal},
    {"getctable", db_getctable},
    {"getclist", db_getclist},
    {"getupvalue", db_getupvalue},
    {"upvaluejoin", db_upvaluejoin},
    {"upvalueid", db_upvalueid},
    {"setuservalue", db_setuservalue},
    {"sethook", db_sethook},
    {"setlocal", db_setlocal},
    {"setupvalue", db_setupvalue},
    {"traceback", db_traceback},
    {"stackinuse", db_stackinuse},
    {"cstacklimit", NULL},
    {"maxstack", NULL},
    {NULL, NULL}
};


CSMOD_API int tokuopen_debug(toku_State *T) {
    tokuL_push_lib(C, dblib);
    toku_push_integer(C, TOKUI_MAXCCALLS);
    toku_set_fieldstr(C, -2, "cstacklimit");
    toku_push_integer(C, TOKUI_MAXSTACK);
    toku_set_fieldstr(C, -2, "maxstack");
    return 1;
}
