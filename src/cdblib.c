/*
** cdblib.c
** Interface from CScript to its debug API
** See Copyright Notice in cscript.h
*/

#define cdblib_c
#define CS_LIB

#include "cprefix.h"

#include <stdio.h>
#include <string.h>

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "climits.h"

#include "ctrace.h"


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
static void checkstack(cs_State *C, cs_State *C1, int n) {
    if (c_unlikely(C != C1 && !cs_checkstack(C1, n)))
        csL_error(C, "stack overflow");
}


static int db_getctable(cs_State *C) {
    cs_push(C, CS_CTABLE_INDEX);
    return 1;
}


static int db_getclist(cs_State *C) {
    cs_push(C, CS_CLIST_INDEX);
    return 1;
}


static int db_getuservalue(cs_State *C) {
    int n = (int)csL_opt_integer(C, 1, 0);
    if (cs_type(C, 0) != CS_T_USERDATA)
        csL_push_fail(C);
    else if (cs_get_uservalue(C, 0, n) != CS_T_NONE) {
        cs_push_bool(C, 1);
        return 2;
    }
    return 1;
}


static int db_setuservalue(cs_State *C) {
    int n = (int)csL_opt_integer(C, 2, 0);
    csL_check_type(C, 0, CS_T_USERDATA);
    csL_check_any(C, 1);
    cs_setntop(C, 2);
    if (!cs_set_uservalue(C, 0, n))
        csL_push_fail(C);
    return 1;
}


/*
** Auxiliary function used by several library functions: check for
** an optional thread as function's first argument and set 'arg' with
** 0 if this argument is present (so that functions can skip it to
** access their other arguments)
*/
static cs_State *getthread(cs_State *C, int *arg) {
    if (cs_is_thread(C, 0)) {
        *arg = 0;
        return cs_to_thread(C, 0);
    } else {
        *arg = -1;
        return C; /* function will operate over current thread */
    }
}


/*
** Variations of 'cs_set_field', used by 'db_getinfo' to put results
** from 'cs_getinfo' into result table. Key is always a string;
** value can be a string, an int, or a bool.
*/
static void settabss(cs_State *C, const char *k, const char *v) {
    cs_push_string(C, v);
    cs_set_fieldstr(C, -2, k);
}

static void settabsi(cs_State *C, const char *k, int v) {
    cs_push_integer(C, v);
    cs_set_fieldstr(C, -2, k);
}

static void settabsb(cs_State *C, const char *k, int v) {
    cs_push_bool(C, v);
    cs_set_fieldstr(C, -2, k);
}


/*
** In function 'db_getinfo', the call to 'cs_getinfo' may push
** results on the stack; later it creates the result table to put
** these objects. Function 'treatstackoption' puts the result from
** 'cs_getinfo' on top of the result table so that it can call
** 'cs_set_fieldstr'.
*/
static void treatstackoption(cs_State *C, cs_State *C1, const char *fname) {
    if (C == C1)
        cs_rotate(C, -2, 1); /* exchange object and table */
    else
        cs_xmove(C1, C, 1); /* move object to the "main" stack */
    cs_set_fieldstr(C, -2, fname); /* put object into table */
}


/*
** Calls 'cs_getinfo' and collects all results in a new table.
** C1 needs stack space for an optional input (function) plus
** two optional outputs (function and line table) from function
** 'cs_getinfo'.
*/
static int db_getinfo(cs_State *C) {
    int arg;
    cs_Debug ar;
    cs_State *C1 = getthread(C, &arg);
    const char *options = csL_opt_string(C, arg + 2, "flnsru");
    checkstack(C, C1, 3);
    csL_check_arg(C, options[0] != '>', arg + 2, "invalid option '>'");
    if (cs_is_function(C, arg + 1)) { /* info about a function? */
        options = cs_push_fstring(C, ">%s", options); /* add '>' to 'options' */
        cs_push(C, arg+1); /* move function to 'C1' stack */
        cs_xmove(C, C1, 1);
    } else { /* stack level */
        if (!cs_getstack(C1, (int)csL_check_integer(C, arg + 1), &ar)) {
            csL_push_fail(C); /* level out of range */
            return 1;
        }
    }
    if (!cs_getinfo(C1, options, &ar))
        return csL_error_arg(C, arg+2, "invalid option");
    cs_push_table(C, 0); /* table to collect results */
    if (strchr(options, 's')) {
        cs_push_lstring(C, ar.source, ar.srclen);
        cs_set_fieldstr(C, -2, "source");
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


static int db_getlocal(cs_State *C) {
    int arg;
    cs_State *C1 = getthread(C, &arg);
    int nvar = (int)csL_check_integer(C, arg + 2); /* local-variable index */
    if (cs_is_function(C, arg + 1)) { /* function argument? */
        cs_push(C, arg + 1); /* push function */
        cs_push_string(C, cs_getlocal(C, NULL, nvar)); /* push local name */
        return 1; /* return only name (there is no value) */
    } else { /* stack-level argument */
        cs_Debug ar;
        const char *name;
        int level = (int)csL_check_integer(C, arg + 1);
        if (c_unlikely(!cs_getstack(C1, level, &ar)))  /* out of range? */
            return csL_error_arg(C, arg+1, "level out of range");
        checkstack(C, C1, 1);
        name = cs_getlocal(C1, &ar, nvar);
        if (name) {
            cs_xmove(C1, C, 1); /* move local value */
            cs_push_string(C, name); /* push name */
            cs_rotate(C, -2, 1); /* re-order */
            return 2;
        } else {
            csL_push_fail(C); /* no name (nor value) */
            return 1;
        }
    }
}


static int db_setlocal(cs_State *C) {
    int arg;
    cs_Debug ar;
    const char *name;
    cs_State *C1 = getthread(C, &arg);
    int level = (int)csL_check_integer(C, arg + 1);
    int nvar = (int)csL_check_integer(C, arg + 2);
    if (c_unlikely(!cs_getstack(C1, level, &ar)))  /* out of range? */
        return csL_error_arg(C, arg+1, "level out of range");
    csL_check_any(C, arg+3);
    cs_setntop(C, arg+4);
    checkstack(C, C1, 1); /* ensure space for value */
    cs_xmove(C, C1, 1); /* move value (4th or 3rd parameter) */
    name = cs_setlocal(C1, &ar, nvar);
    if (name == NULL) /* no local was found? */
        cs_pop(C1, 1); /* pop value (if not popped by 'cs_setlocal') */
    cs_push_string(C, name);
    return 1;
}


/*
** get (if 'get' is true) or set an upvalue from a closure
*/
static int auxupvalue(cs_State *C, int get) {
    const char *name;
    int n = (int)csL_check_integer(C, 1); /* upvalue index */
    csL_check_type(C, 0, CS_T_FUNCTION); /* closure */
    name = get ? cs_getupvalue(C, 0, n) : cs_setupvalue(C, 0, n);
    if (name == NULL) return 0;
    cs_push_string(C, name);
    cs_insert(C, -(get+1)); /* no-op if 'get' is false */
    return get + 1;
}


static int db_getupvalue(cs_State *C) {
    return auxupvalue(C, 1);
}


static int db_setupvalue(cs_State *C) {
    csL_check_any(C, 2);
    return auxupvalue(C, 0);
}


/*
** Check whether a given upvalue from a given closure exists and
** returns its index.
*/
static void *checkupval(cs_State *C, int argf, int argnup, int *pnup) {
    void *id;
    int nup = (int)csL_check_integer(C, argnup); /* upvalue index */
    csL_check_type(C, argf, CS_T_FUNCTION); /* closure */
    id = cs_upvalueid(C, argf, nup);
    if (pnup) {
        csL_check_arg(C, id != NULL, argnup, "invalid upvalue index");
        *pnup = nup;
    }
    return id;
}


static int db_upvalueid(cs_State *C) {
    void *id = checkupval(C, 0, 1, NULL);
    if (id != NULL)
        cs_push_lightuserdata(C, id);
    else
        csL_push_fail(C);
    return 1;
}


static int db_upvaluejoin(cs_State *C) {
    int n1, n2;
    checkupval(C, 0, 1, &n1);
    checkupval(C, 2, 3, &n2);
    csL_check_arg(C, !cs_is_cfunction(C, 0), 0, "CScript function expected");
    csL_check_arg(C, !cs_is_cfunction(C, 2), 2, "CScript function expected");
    cs_upvaluejoin(C, 0, n1, 2, n2);
    return 0;
}


#include <stdio.h>
/*
** Call hook function registered at hook table for the current
** thread (if there is one).
*/
static void hookf(cs_State *C, cs_Debug *ar) {
    static const char *const hooknames[] = {"call","return","line","count"};
    cs_get_cfieldstr(C, HOOKKEY);
    csTR_dumpstack(C, 2, "hookf HOOKKEY");
    cs_push_thread(C);
    if (cs_get_raw(C, -2) == CS_T_FUNCTION) { /* is there a hook function? */
        cs_push_string(C, hooknames[ar->event]); /* push event name */
        if (ar->currline >= 0)
            cs_push_integer(C, ar->currline); /* push current line */
        else cs_push_nil(C);
        csTR_dumpstack(C, 2, "hookf");
        printf("ASSERTING LS\n");
        cs_assert(cs_getinfo(C, "ls", ar));
        printf("CALLING HOOK FUNC\n"); fflush(stdout);
        cs_call(C, 2, 0); /* call hook function */
    }
}


/*
** Convert a string mask (for 'sethook') into a bit mask
*/
static int makemask(const char *smask, int count) {
    int mask = 0;
    if (strchr(smask, 'c')) mask |= CS_MASK_CALL;
    if (strchr(smask, 'r')) mask |= CS_MASK_RET;
    if (strchr(smask, 'l')) mask |= CS_MASK_LINE;
    if (count > 0) mask |= CS_MASK_COUNT;
    return mask;
}


/*
** Convert a bit mask (for 'gethook') into a string mask
*/
static char *unmakemask(int mask, char *smask) {
    int i = 0;
    if (mask & CS_MASK_CALL) smask[i++] = 'c';
    if (mask & CS_MASK_RET) smask[i++] = 'r';
    if (mask & CS_MASK_LINE) smask[i++] = 'l';
    smask[i] = '\0';
    return smask;
}


static int db_sethook(cs_State *C) {
    int arg, mask, count;
    cs_Hook func;
    cs_State *C1 = getthread(C, &arg);
    if (cs_is_noneornil(C, arg+1)) { /* no hook? */
        cs_setntop(C, arg+2);
        func = NULL; mask = 0; count = 0; /* turn off hooks */
    } else {
        const char *smask = csL_check_string(C, arg+2);
        csL_check_type(C, arg+1, CS_T_FUNCTION);
        count = (int)csL_opt_integer(C, arg + 3, 0);
        func = hookf; mask = makemask(smask, count);
    }
    csL_get_subtable(C, CS_CTABLE_INDEX, HOOKKEY);
    checkstack(C, C1, 1);
    cs_push_thread(C1); cs_xmove(C1, C, 1); /* key (thread) */
    cs_push(C, arg + 1); /* value (hook function) */
    cs_set_raw(C, -3); /* hooktable[C1] = new CScript hook */
    cs_sethook(C1, func, mask, count);
    return 0;
}


static int db_gethook(cs_State *C) {
    int arg;
    cs_State *C1 = getthread(C, &arg);
    char buff[5];
    int mask = cs_gethookmask(C1);
    cs_Hook hook = cs_gethook(C1);
    if (hook == NULL) { /* no hook? */
        csL_push_fail(C);
        return 1;
    } else if (hook != hookf) /* external hook? */
        cs_push_literal(C, "external hook");
    else { /* hook table must exist */
        cs_get_cfieldstr(C, HOOKKEY);
        checkstack(C, C1, 1);
        cs_push_thread(C1); cs_xmove(C1, C, 1);
        cs_get_raw(C, -2); /* 1st result = hooktable[C1] */
        cs_remove(C, -2); /* remove hook table */
    }
    cs_push_string(C, unmakemask(mask, buff)); /* 2nd result = mask */
    cs_push_integer(C, cs_gethookcount(C1)); /* 3rd result = count */
    return 3;
}


/*
** Maximum size of input, when in interactive mode after calling
** 'db_debug'.
*/
#if !defined(C_MAXDBLINE)
#define C_MAXDBLINE     250
#endif

static int db_debug(cs_State *C) {
    for (;;) {
        char buffer[C_MAXDBLINE];
        cs_writefmt(stderr, "%s", "cscript_debug> ");
        if (fgets(buffer, sizeof(buffer), stdin) == NULL ||
                strcmp(buffer, "cont\n") == 0)
            return 0;
        if (csL_loadbuffer(C, buffer, strlen(buffer), "(debug command)") ||
                cs_pcall(C, 0, 0, -1))
            cs_writefmt(stderr, "%s\n", csL_to_lstring(C, -1, NULL));
        cs_setntop(C, 0); /* remove eventual returns */
    }
}


static int db_traceback(cs_State *C) {
    int arg;
    cs_State *C1 = getthread(C, &arg);
    const char *msg = cs_to_string(C, arg + 1);
    if (msg == NULL && !cs_is_noneornil(C, arg + 1)) /* non-string 'msg'? */
        cs_push(C, arg + 1); /* return it untouched */
    else {
        int level = (int)csL_opt_integer(C, arg + 2, (C == C1) ? 1 : 0);
        csL_traceback(C, C1, level, msg);
    }
    return 1;
}


static int db_stackinuse(cs_State *C) {
    int res = cs_stackinuse(getthread(C, &res));
    cs_push_integer(C, res);
    return 1;
}


static const cs_Entry dblib[] = {
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


CSMOD_API int csopen_debug(cs_State *C) {
    csL_push_lib(C, dblib);
    cs_push_integer(C, CSI_MAXCCALLS);
    cs_set_fieldstr(C, -2, "cstacklimit");
    cs_push_integer(C, CSI_MAXSTACK);
    cs_set_fieldstr(C, -2, "maxstack");
    return 1;
}
