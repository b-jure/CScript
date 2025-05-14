/*
** cdblib.c
** Interface from CScript to its debug API
** See Copyright Notice in cscript.h
*/

#define cdblib_c
#define CS_LIB

#include "cprefix.h"

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "climits.h"


///*
//** The hook table at registry[HOOKKEY] maps threads to their current
//** hook function.
//*/
//static const char *const HOOKKEY = "__HOOKKEY";
//
//
///*
//** If C1 != C, C1 can be in any state, and therefore there are no
//** guarantees about its stack space; any push in C1 must be
//** checked.
//*/
//static void checkstack(cs_State *C, cs_State *C1, int n) {
//    if (c_unlikely(C != C1 && !cs_check_stack(C1, n)))
//        csL_error(C, "stack overflow");
//}
//
//
//static int db_getregtable(cs_State *C) {
//    cs_push(C, CS_REGISTRYINDEX);
//    return 1;
//}
//
//
//static int db_getreglist(cs_State *C) {
//    cs_pushvalue(C, CS_REGISTRYINDEX);
//    return 1;
//}
//
//
//static int db_getmetatable (cs_State *C) {
//  csL_checkany(C, 1);
//  if (!cs_getmetatable(C, 1)) {
//    cs_pushnil(C);  /* no metatable */
//  }
//  return 1;
//}
//
//
//static int db_setmetatable (cs_State *C) {
//  int t = cs_type(C, 2);
//  csL_argexpected(C, t == CS_T_NIL || t == CS_T_TABLE, 2, "nil or table");
//  cs_setntop(C, 2);
//  cs_setmetatable(C, 1);
//  return 1;  /* return 1st argument */
//}
//
//
//static int db_getuservalue (cs_State *C) {
//  int n = (int)csL_optinteger(C, 2, 1);
//  if (cs_type(C, 1) != CS_T_USERDATA)
//    csL_pushfail(C);
//  else if (cs_getiuservalue(C, 1, n) != CS_T_NONE) {
//    cs_pushboolean(C, 1);
//    return 2;
//  }
//  return 1;
//}
//
//
//static int db_setuservalue (cs_State *C) {
//  int n = (int)csL_optinteger(C, 3, 1);
//  csL_checktype(C, 1, CS_T_USERDATA);
//  csL_checkany(C, 2);
//  cs_setntop(C, 2);
//  if (!cs_setiuservalue(C, 1, n))
//    csL_pushfail(C);
//  return 1;
//}
//
//
///*
//** Auxiliary function used by several library functions: check for
//** an optional thread as function's first argument and set 'arg' with
//** 1 if this argument is present (so that functions can skip it to
//** access their other arguments)
//*/
//static cs_State *getthread (cs_State *C, int *arg) {
//  if (cs_isthread(C, 1)) {
//    *arg = 1;
//    return cs_tothread(C, 1);
//  }
//  else {
//    *arg = 0;
//    return C;  /* function will operate over current thread */
//  }
//}
//
//
///*
//** Variations of 'cs_settable', used by 'db_getinfo' to put results
//** from 'cs_getinfo' into result table. Key is always a string;
//** value can be a string, an int, or a boolean.
//*/
//static void settabss (cs_State *C, const char *k, const char *v) {
//  cs_pushstring(C, v);
//  cs_setfield(C, -2, k);
//}
//
//static void settabsi (cs_State *C, const char *k, int v) {
//  cs_pushinteger(C, v);
//  cs_setfield(C, -2, k);
//}
//
//static void settabsb (cs_State *C, const char *k, int v) {
//  cs_pushboolean(C, v);
//  cs_setfield(C, -2, k);
//}
//
//
///*
//** In function 'db_getinfo', the call to 'cs_getinfo' may push
//** results on the stack; later it creates the result table to put
//** these objects. Function 'treatstackoption' puts the result from
//** 'cs_getinfo' on top of the result table so that it can call
//** 'cs_setfield'.
//*/
//static void treatstackoption (cs_State *C, cs_State *C1, const char *fname) {
//  if (C == C1)
//    cs_rotate(C, -2, 1);  /* exchange object and table */
//  else
//    cs_xmove(C1, C, 1);  /* move object to the "main" stack */
//  cs_setfield(C, -2, fname);  /* put object into table */
//}
//
//
///*
//** Calls 'cs_getinfo' and collects all results in a new table.
//** C1 needs stack space for an optional input (function) plus
//** two optional outputs (function and line table) from function
//** 'cs_getinfo'.
//*/
//static int db_getinfo (cs_State *C) {
//  cs_Debug ar;
//  int arg;
//  cs_State *C1 = getthread(C, &arg);
//  const char *options = csL_optstring(C, arg+2, "flnSrtu");
//  checkstack(C, C1, 3);
//  csL_argcheck(C, options[0] != '>', arg + 2, "invalid option '>'");
//  if (cs_isfunction(C, arg + 1)) {  /* info about a function? */
//    options = cs_pushfstring(C, ">%s", options);  /* add '>' to 'options' */
//    cs_pushvalue(C, arg + 1);  /* move function to 'C1' stack */
//    cs_xmove(C, C1, 1);
//  }
//  else {  /* stack level */
//    if (!cs_getstack(C1, (int)csL_checkinteger(C, arg + 1), &ar)) {
//      csL_pushfail(C);  /* level out of range */
//      return 1;
//    }
//  }
//  if (!cs_getinfo(C1, options, &ar))
//    return csL_argerror(C, arg+2, "invalid option");
//  cs_newtable(C);  /* table to collect results */
//  if (strchr(options, 'S')) {
//    cs_pushlstring(C, ar.source, ar.srclen);
//    cs_setfield(C, -2, "source");
//    settabss(C, "short_src", ar.short_src);
//    settabsi(C, "linedefined", ar.linedefined);
//    settabsi(C, "lastlinedefined", ar.lastlinedefined);
//    settabss(C, "what", ar.what);
//  }
//  if (strchr(options, 'l'))
//    settabsi(C, "currentline", ar.currentline);
//  if (strchr(options, 'u')) {
//    settabsi(C, "nups", ar.nups);
//    settabsi(C, "nparams", ar.nparams);
//    settabsb(C, "isvararg", ar.isvararg);
//  }
//  if (strchr(options, 'n')) {
//    settabss(C, "name", ar.name);
//    settabss(C, "namewhat", ar.namewhat);
//  }
//  if (strchr(options, 'r')) {
//    settabsi(C, "ftransfer", ar.ftransfer);
//    settabsi(C, "ntransfer", ar.ntransfer);
//  }
//  if (strchr(options, 't'))
//    settabsb(C, "istailcall", ar.istailcall);
//  if (strchr(options, 'C'))
//    treatstackoption(C, C1, "activelines");
//  if (strchr(options, 'f'))
//    treatstackoption(C, C1, "func");
//  return 1;  /* return table */
//}
//
//
//static int db_getlocal (cs_State *C) {
//  int arg;
//  cs_State *C1 = getthread(C, &arg);
//  int nvar = (int)csL_checkinteger(C, arg + 2);  /* local-variable index */
//  if (cs_isfunction(C, arg + 1)) {  /* function argument? */
//    cs_pushvalue(C, arg + 1);  /* push function */
//    cs_pushstring(C, cs_getlocal(C, NULL, nvar));  /* push local name */
//    return 1;  /* return only name (there is no value) */
//  }
//  else {  /* stack-level argument */
//    cs_Debug ar;
//    const char *name;
//    int level = (int)csL_checkinteger(C, arg + 1);
//    if (c_unlikely(!cs_getstack(C1, level, &ar)))  /* out of range? */
//      return csL_argerror(C, arg+1, "level out of range");
//    checkstack(C, C1, 1);
//    name = cs_getlocal(C1, &ar, nvar);
//    if (name) {
//      cs_xmove(C1, C, 1);  /* move local value */
//      cs_pushstring(C, name);  /* push name */
//      cs_rotate(C, -2, 1);  /* re-order */
//      return 2;
//    }
//    else {
//      csL_pushfail(C);  /* no name (nor value) */
//      return 1;
//    }
//  }
//}
//
//
//static int db_setlocal (cs_State *C) {
//  int arg;
//  const char *name;
//  cs_State *C1 = getthread(C, &arg);
//  cs_Debug ar;
//  int level = (int)csL_checkinteger(C, arg + 1);
//  int nvar = (int)csL_checkinteger(C, arg + 2);
//  if (c_unlikely(!cs_getstack(C1, level, &ar)))  /* out of range? */
//    return csL_argerror(C, arg+1, "level out of range");
//  csL_checkany(C, arg+3);
//  cs_setntop(C, arg+3);
//  checkstack(C, C1, 1);
//  cs_xmove(C, C1, 1);
//  name = cs_setlocal(C1, &ar, nvar);
//  if (name == NULL)
//    cs_pop(C1, 1);  /* pop value (if not popped by 'cs_setlocal') */
//  cs_pushstring(C, name);
//  return 1;
//}
//
//
///*
//** get (if 'get' is true) or set an upvalue from a closure
//*/
//static int auxupvalue (cs_State *C, int get) {
//  const char *name;
//  int n = (int)csL_checkinteger(C, 2);  /* upvalue index */
//  csL_checktype(C, 1, cs_TFUNCTION);  /* closure */
//  name = get ? cs_getupvalue(C, 1, n) : cs_setupvalue(C, 1, n);
//  if (name == NULL) return 0;
//  cs_pushstring(C, name);
//  cs_insert(C, -(get+1));  /* no-op if get is false */
//  return get + 1;
//}
//
//
//static int db_getupvalue (cs_State *C) {
//  return auxupvalue(C, 1);
//}
//
//
//static int db_setupvalue (cs_State *C) {
//  csL_checkany(C, 3);
//  return auxupvalue(C, 0);
//}
//
//
///*
//** Check whether a given upvalue from a given closure exists and
//** returns its index
//*/
//static void *checkupval (cs_State *C, int argf, int argnup, int *pnup) {
//  void *id;
//  int nup = (int)csL_checkinteger(C, argnup);  /* upvalue index */
//  csL_checktype(C, argf, cs_TFUNCTION);  /* closure */
//  id = cs_upvalueid(C, argf, nup);
//  if (pnup) {
//    csL_argcheck(C, id != NULL, argnup, "invalid upvalue index");
//    *pnup = nup;
//  }
//  return id;
//}
//
//
//static int db_upvalueid (cs_State *C) {
//  void *id = checkupval(C, 1, 2, NULL);
//  if (id != NULL)
//    cs_pushlightuserdata(C, id);
//  else
//    csL_pushfail(C);
//  return 1;
//}
//
//
//static int db_upvaluejoin (cs_State *C) {
//  int n1, n2;
//  checkupval(C, 1, 2, &n1);
//  checkupval(C, 3, 4, &n2);
//  csL_argcheck(C, !cs_iscfunction(C, 1), 1, "Lua function expected");
//  csL_argcheck(C, !cs_iscfunction(C, 3), 3, "Lua function expected");
//  cs_upvaluejoin(C, 1, n1, 3, n2);
//  return 0;
//}
//
//
///*
//** Call hook function registered at hook table for the current
//** thread (if there is one)
//*/
//static void hookf (cs_State *C, cs_Debug *ar) {
//  static const char *const hooknames[] =
//    {"call", "return", "line", "count", "tail call"};
//  cs_getfield(C, CS_REGISTRYINDEX, HOOKKEY);
//  cs_pushthread(C);
//  if (cs_rawget(C, -2) == CS_T_FUNCTION) {  /* is there a hook function? */
//    cs_pushstring(C, hooknames[(int)ar->event]);  /* push event name */
//    if (ar->currentline >= 0)
//      cs_pushinteger(C, ar->currentline);  /* push current line */
//    else cs_pushnil(C);
//    cs_assert(cs_getinfo(C, "lS", ar));
//    cs_call(C, 2, 0);  /* call hook function */
//  }
//}
//
//
///*
//** Convert a string mask (for 'sethook') into a bit mask
//*/
//static int makemask (const char *smask, int count) {
//  int mask = 0;
//  if (strchr(smask, 'c')) mask |= CS_MASKCALL;
//  if (strchr(smask, 'r')) mask |= CS_MASKRET;
//  if (strchr(smask, 'l')) mask |= CS_MASKLINE;
//  if (count > 0) mask |= CS_MASKCOUNT;
//  return mask;
//}
//
//
///*
//** Convert a bit mask (for 'gethook') into a string mask
//*/
//static char *unmakemask (int mask, char *smask) {
//  int i = 0;
//  if (mask & CS_MASKCALL) smask[i++] = 'c';
//  if (mask & CS_MASKRET) smask[i++] = 'r';
//  if (mask & CS_MASKLINE) smask[i++] = 'l';
//  smask[i] = '\0';
//  return smask;
//}
//
//
//static int db_sethook (cs_State *C) {
//  int arg, mask, count;
//  cs_Hook func;
//  cs_State *C1 = getthread(C, &arg);
//  if (cs_isnoneornil(C, arg+1)) {  /* no hook? */
//    cs_setntop(C, arg+1);
//    func = NULL; mask = 0; count = 0;  /* turn off hooks */
//  }
//  else {
//    const char *smask = csL_checkstring(C, arg+2);
//    csL_checktype(C, arg+1, CS_T_FUNCTION);
//    count = (int)csL_optinteger(C, arg + 3, 0);
//    func = hookf; mask = makemask(smask, count);
//  }
//  if (!csL_getsubtable(C, CS_REGISTRYINDEX, HOOKKEY)) {
//    /* table just created; initialize it */
//    cs_pushliteral(C, "k");
//    cs_setfield(C, -2, "__mode");  /** hooktable.__mode = "k" */
//    cs_pushvalue(C, -1);
//    cs_setmetatable(C, -2);  /* metatable(hooktable) = hooktable */
//  }
//  checkstack(C, C1, 1);
//  cs_pushthread(C1); cs_xmove(C1, C, 1);  /* key (thread) */
//  cs_pushvalue(C, arg + 1);  /* value (hook function) */
//  cs_rawset(C, -3);  /* hooktable[C1] = new Lua hook */
//  cs_sethook(C1, func, mask, count);
//  return 0;
//}
//
//
//static int db_gethook (cs_State *C) {
//  int arg;
//  cs_State *C1 = getthread(C, &arg);
//  char buff[5];
//  int mask = cs_gethookmask(C1);
//  cs_Hook hook = cs_gethook(C1);
//  if (hook == NULL) {  /* no hook? */
//    csL_pushfail(C);
//    return 1;
//  }
//  else if (hook != hookf)  /* external hook? */
//    cs_pushliteral(C, "external hook");
//  else {  /* hook table must exist */
//    cs_getfield(C, CS_REGISTRYINDEX, HOOKKEY);
//    checkstack(C, C1, 1);
//    cs_pushthread(C1); cs_xmove(C1, C, 1);
//    cs_rawget(C, -2);   /* 1st result = hooktable[C1] */
//    cs_remove(C, -2);  /* remove hook table */
//  }
//  cs_pushstring(C, unmakemask(mask, buff));  /* 2nd result = mask */
//  cs_pushinteger(C, cs_gethookcount(C1));  /* 3rd result = count */
//  return 3;
//}
//
//
//static int db_debug (cs_State *C) {
//  for (;;) {
//    char buffer[250];
//    cs_writestringerror("%s", "cs_debug> ");
//    if (fgets(buffer, sizeof(buffer), stdin) == NULL ||
//        strcmp(buffer, "cont\n") == 0)
//      return 0;
//    if (csL_loadbuffer(C, buffer, strlen(buffer), "=(debug command)") ||
//        cs_pcall(C, 0, 0, 0))
//      cs_writestringerror("%s\n", csL_tolstring(C, -1, NULL));
//    cs_setntop(C, 0);  /* remove eventual returns */
//  }
//}
//
//
//static int db_traceback (cs_State *C) {
//  int arg;
//  cs_State *C1 = getthread(C, &arg);
//  const char *msg = cs_tostring(C, arg + 1);
//  if (msg == NULL && !cs_isnoneornil(C, arg + 1))  /* non-string 'msg'? */
//    cs_pushvalue(C, arg + 1);  /* return it untouched */
//  else {
//    int level = (int)csL_optinteger(C, arg + 2, (C == C1) ? 1 : 0);
//    csL_traceback(C, C1, msg, level);
//  }
//  return 1;
//}
//
//
//static int db_setcstacklimit (cs_State *C) {
//  int limit = (int)csL_checkinteger(C, 1);
//  int res = cs_setcstacklimit(C, limit);
//  cs_pushinteger(C, res);
//  return 1;
//}
//
//
static const cs_Entry dblib[] = {
    //{"debug", db_debug},
    //{"getuservalue", db_getuservalue},
    //{"gethook", db_gethook},
    //{"getinfo", db_getinfo},
    //{"getlocal", db_getlocal},
    //{"getregtable", db_getregtable},
    //{"getreglist", db_getreglist},
    //{"getmetatable", db_getmetatable},
    //{"getupvalue", db_getupvalue},
    //{"upvaluejoin", db_upvaluejoin},
    //{"upvalueid", db_upvalueid},
    //{"setuservalue", db_setuservalue},
    //{"sethook", db_sethook},
    //{"setlocal", db_setlocal},
    //{"setmetatable", db_setmetatable},
    //{"setupvalue", db_setupvalue},
    //{"traceback", db_traceback},
    //{"setcstacklimit", db_setcstacklimit},
    {NULL, NULL}
};


CSMOD_API int csopen_debug(cs_State *C) {
    csL_push_lib(C, dblib);
    return 1;
}
