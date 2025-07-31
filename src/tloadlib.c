/*
** tloadlib.c
** Dynamic library loader for Tokudae
** See Copyright Notice in tokudae.h
*/

#define tloadlib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <string.h>
#include <stdlib.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"


/* prefix for open functions in C libraries */
#define TOKU_POF	    "tokuopen_"

/* separator for open functions in C libraries */
#define TOKU_OFSEP    "_"


/* prefix in errors */
#define PREFIX      "\n        "
#define PREFIX_LEN  (sizeof(PREFIX) - 1)


/*
** Key for full userdata in the Ctable that keeps handles
** for all loaded C libraries.
*/
static const char *const CLIBS = "__CLIBS";

#define LIB_FAIL    "open"


#define setprogdir(T)       ((void)0)


/*
** Unload library 'lib' and return 0.
** In case of error, returns non-zero plus an error string
** in the stack.
*/
static int csys_unloadlib(toku_State *T, void *lib);

/*
** Load C library in file 'path'. If 'global', load with all names
** in the library global.
** Returns the library; in case of error, returns NULL plus an error
** string in the stack.
*/
static void *csys_load(toku_State *T, const char *path, int global);

/*
** Try to find a function named 'sym' in library 'lib'.
** Returns the function; in case of error, returns NULL plus an
** error string in the stack.
*/
static toku_CFunction csys_symbolf(toku_State *T, void *lib, const char *sym);


#if defined(TOKU_USE_DLOPEN)  /* { */

#include <dlfcn.h>

/*
** Macro to convert pointer-to-void* to pointer-to-function. This cast
** is undefined according to ISO C, but POSIX assumes that it works.
** (The '__extension__' in gnu compilers is only to avoid warnings.)
*/
#if defined(__GNUC__)
#define cast_func(p) (__extension__ (toku_CFunction)(p))
#else
#define cast_func(p) ((toku_CFunction)(p))
#endif


static int csys_unloadlib(toku_State *T, void *lib) {
    int res = dlclose(lib);
    if (t_unlikely(res != 0))
        toku_push_fstring(T, dlerror());
    return res;
}


static void *csys_load(toku_State *T, const char *path, int global) {
    void *lib = dlopen(path, RTLD_LAZY | (global ? RTLD_GLOBAL : RTLD_LOCAL));
    if (t_unlikely(lib == NULL))
        toku_push_fstring(T, dlerror());
    return lib;
}


static toku_CFunction csys_symbolf(toku_State *T, void *lib, const char *sym) {
    toku_CFunction f;
    const char *msg;
    (void)dlerror(); /* clear any old error conds. before calling 'dlsym' */
    f = cast_func(dlsym(lib, sym));
    if (t_unlikely(f == NULL && (msg = dlerror()) != NULL))
        toku_push_fstring(T, msg);
    return f;
}


#elif defined(TOKU_DL_DLL)    /* }{ */

#include <windows.h>


/*
** optional flags for LoadLibraryEx
*/
#if !defined(TOKU_LLE_FLAGS)
#define TOKU_LLE_FLAGS	0
#endif


#undef setprogdir


/*
** Replace in the path (on the top of the stack) any occurrence
** of TOKU_EXEC_DIR with the executable's path.
*/
static void setprogdir(toku_State *T) {
    char buff[MAX_PATH + 1];
    char *lb;
    DWORD nsize = sizeof(buff)/sizeof(char);
    DWORD n = GetModuleFileNameA(NULL, buff, nsize); /* get exec. name */
    if (n == 0 || n == nsize || (lb = strrchr(buff, '\\')) == NULL)
        tokuL_error(T, "unable to get ModuleFileName");
    else {
        *lb = '\0'; /* cut name on the last '\\' to get the path */
        tokuL_gsub(T, toku_to_string(T, -1), TOKU_EXEC_DIR, buff);
        toku_remove(T, -2); /* remove original string */
    }
}


static void pusherror(toku_State *T) {
    int error = GetLastError();
    char buffer[128];
    if (FormatMessageA(FORMAT_MESSAGE_IGNORE_INSERTS|FORMAT_MESSAGE_FROM_SYSTEM,
                NULL, error, 0, buffer, sizeof(buffer)/sizeof(char), NULL))
        toku_push_string(T, buffer);
    else
        toku_push_fstring(T, "system error %d\n", error);
}


static int csys_unloadlib(toku_State *T, void *lib) {
    int res = FreeLibrary((HMODULE)lib);
    if (t_unlikely(res == 0)) pusherror(T);
    return res;
}


static void *csys_load(toku_State *T, const char *path, int global) {
    HMODULE lib = LoadLibraryExA(path, NULL, TOKU_LLE_FLAGS);
    (void)(global); /* not used: symbols are 'global' by default */
    if (lib == NULL) pusherror(T);
    return lib;
}


static toku_CFunction csys_symbolf(toku_State *T, void *lib, const char *sym) {
    toku_CFunction f = (toku_CFunction)(voidf)GetProcAddress((HMODULE)lib, sym);
    if (f == NULL) pusherror(T);
    return f;
}

#else                       /* }{ */

#undef LIB_FAIL
#define LIB_FAIL    "absent"

#define DLMSG "dynamic libraries not enabled; check your Tokudae installation"

static int csys_unloadlib(toku_State *T, void *lib) {
    (void)(lib); /* unused */
    toku_push_literal(T, DLMSG);
    return 1;
}


static void *csys_load(toku_State *T, const char *path, int global) {
    (void)(path); (void)(global); /* unused */
    toku_push_literal(T, DLMSG);
    return NULL;
}


static toku_CFunction csys_symbolf(toku_State *T, void *lib, const char *sym) {
    (void)(lib); (void)(sym); /* unused */
    toku_push_literal(T, DLMSG);
    return NULL;
}

#endif                      /* } */


static int searcher_preload(toku_State *T) {
    const char *name = tokuL_check_string(T, 0);
    toku_get_cfield_str(T, TOKU_PRELOAD_TABLE); /* get ctable[__PRELOAD] */
    if (toku_get_field_str(T, -1, name) == TOKU_T_NIL) { /* 'name' not found? */
        toku_push_fstring(T, "no field package.preload['%s']", name);
        return 1;
    } else {
        toku_push_literal(T, ":preload:");
        return 2; /* return value and literal string */
    }
}


/*
** Return package C library.
*/
static void *check_clib(toku_State *T, const char *path) {
    void *plib;
    toku_get_cfield_str(T, CLIBS); /* get clibs userdata */
    toku_get_uservalue(T, -1, 0); /* get list uservalue */
    toku_get_index(T, -1, 0); /* get list query table */
    toku_get_field_str(T, -1, path); /* get t[path] */
    plib = toku_to_userdata(T, -1); /* plib = t[path] */
    toku_pop(T, 4); /* clibs, list, query table and plib */
    return plib;
}


/*
** Adds 'plib' (a library handle) to clibs userdata.
*/
static void add_libhandle_to_clibs(toku_State *T, const char *path, void *plib) {
    toku_get_cfield_str(T, CLIBS); /* get clibs userdata */
    toku_get_uservalue(T, -1, 0); /* get list uservalue */
    toku_get_index(T, -1, 0); /* get l[0] (query table) */
    toku_push_lightuserdata(T, plib); /* push lib handle */
    toku_push(T, -1); /* copy of lib handle */
    toku_set_index(T, -4, cast_int(toku_len(T, -4))); /* l[len(l)] = plib */
    toku_set_field_str(T, -2, path); /* t[path] = plib */
    toku_pop(T, 3); /* clibs, list and query table */
}


/* error codes for 'look_for_func' */
#define ERRLIB		1 /* unable to load library */
#define ERRFUNC		2 /* unable to find function */

/*
** Look for a C function named 'sym' in a dynamically loaded library 'path'.
** First, check whether the library is already loaded; if not, try to load it.
** Then, if 'sym' is '*', return true (as library has been loaded).
** Otherwise, look for symbol 'sym' in the library and push a C function
** with that symbol.
** Return 0 and 'true' or a function in the stack; in case of
** errors, return an error code and an error message in the stack.
*/
static int look_for_func(toku_State *T, const char *path, const char *sym) {
    void *reg = check_clib(T, path); /* check loaded C libraries */
    if (reg == NULL) { /* must load library? */
        reg = csys_load(T, path, *sym == '*'); /* global symbols if 'sym'=='*' */
        if (reg == NULL) return ERRLIB; /* unable to load library */
        add_libhandle_to_clibs(T, path, reg);
    }
    if (*sym == '*') /* loading only library (no function)? */
        toku_push_bool(T, 1); /* return 'true' */
    else {
        toku_CFunction f = csys_symbolf(T, reg, sym);
        if (f == NULL) return ERRFUNC; /* unable to find function */
        toku_push_cfunction(T, f); /* else create new function */
    }
    return 0; /* no errors */
}


static int pkg_loadlib(toku_State *T) {
    const char *path = tokuL_check_string(T, 0);
    const char *init = tokuL_check_string(T, 1);
    int res = look_for_func(T, path, init);
    if (t_likely(res == 0)) /* no errors? */
        return 1; /* return the loaded function */
    else { /* error; error message is on top of the stack */
        tokuL_push_fail(T);
        toku_insert(T, -2);
        toku_push_string(T, (res == ERRLIB) ? LIB_FAIL : "init");
        return 3;  /* return fail, error message, and where */
    }
}


/*
** Get the next name in '*path' = 'name1;name2;name3;...', changing
** the ending ';' to '\0' to create a zero-terminated string. Return
** NULL when list ends.
*/
static const char *get_next_filename (char **path, char *end) {
    char *sep;
    char *name = *path;
    if (name == end)
        return NULL; /* no more names */
    else if (*name == '\0') { /* from previous iteration? */
        *name = *TOKU_PATH_SEP; /* restore separator */
        name++;  /* skip it */
    }
    sep = strchr(name, *TOKU_PATH_SEP); /* find next separator */
    if (sep == NULL) /* separator not found? */
        sep = end; /* name goes until the end */
    *sep = '\0'; /* finish file name */
    *path = sep; /* will start next search from here */
    return name;
}


static int readable (const char *filename) {
    FILE *f = fopen(filename, "r"); /* try to open file */
    if (f == NULL) return 0; /* open failed */
    fclose(f);
    return 1; /* ok (can be read) */
}


/*
** Given a path such as "blabla.so;blublu.so", pushes the string
**
** no file 'blabla.so'
**         no file 'blublu.so'
*/
static void push_notfound_error(toku_State *T, const char *path) {
    tokuL_Buffer b;
    tokuL_buff_init(T, &b);
    tokuL_buff_push_string(&b, "no file '");
    tokuL_buff_push_gsub(&b, path, TOKU_PATH_SEP, "'" PREFIX "no file '");
    tokuL_buff_push_string(&b, "'");
    tokuL_buff_end(&b);
}


static const char *search_path(toku_State *T, const char *name, const char *path,
                               const char *sep, const char *dirsep) {
    tokuL_Buffer buff;
    char *pathname; /* path with name inserted */
    char *endpathname; /* its end */
    const char *filename;
    /* separator is non-empty and appears in 'name'? */
    if (*sep != '\0' && strchr(name, *sep) != NULL)
        name = tokuL_gsub(T, name, sep, dirsep); /* replace it by 'dirsep' */
    tokuL_buff_init(T, &buff);
    /* add path to the buffer, replacing marks ('?') with the file name */
    tokuL_buff_push_gsub(&buff, path, TOKU_PATH_MARK, name);
    tokuL_buff_push(&buff, '\0');
    pathname = tokuL_buffptr(&buff); /* writable list of file names */
    endpathname = pathname + tokuL_bufflen(&buff) - 1;
    while ((filename = get_next_filename(&pathname, endpathname)) != NULL) {
        if (readable(filename)) /* does file exist and is readable? */
            return toku_push_string(T, filename); /* save and return name */
    }
    tokuL_buff_end(&buff); /* push path to create error message */
    push_notfound_error(T, toku_to_string(T, -1)); /* create error message */
    return NULL; /* not found */
}


static int pkg_searchpath(toku_State *T) {
    const char *fname = search_path(T, tokuL_check_string(T, 0),
                                       tokuL_check_string(T, 1),
                                       tokuL_opt_string(T, 2, "."),
                                       tokuL_opt_string(T, 3, TOKU_DIRSEP));
    if (fname != NULL) {
        return 1;
    } else { /* error message is on top of the stack */
        tokuL_push_fail(T);
        toku_insert(T, -2);
        return 2; /* return fail + error message */
    }
}


static const char *find_file(toku_State *T, const char *name,
                             const char *pname, const char *dirsep) {
    const char *path;
    toku_get_field_str(T, toku_upvalueindex(0), pname); /* get 'package[pname]' */
    path = toku_to_string(T, -1);
    if (t_unlikely(path == NULL)) /* path template is not a string? */
        tokuL_error(T, "'package.%s' must be a string", pname);
    /* otherwise, search for the path in the 'path' template */
    return search_path(T, name, path, ".", dirsep);
}


static int check_load(toku_State *T, int res, const char *filename) {
    if (t_likely(res)) { /* module loaded successfully? */
        toku_push_string(T, filename); /* will be 2nd argument to module */
        return 2; /* return open function and file name */
    } else
        return tokuL_error(T, "error loading module '%s' from file '%s':"
                            PREFIX "%s", toku_to_string(T, 1), filename,
                            toku_to_string(T, -1));
}


static int searcher_Tokudae(toku_State *T) {
    const char *filename;
    const char *name = tokuL_check_string(T, 0);
    filename = find_file(T, name, "path", TOKU_DIRSEP);
    if (filename == NULL) return 1; /* module not found in this path */
    return check_load(T, (tokuL_loadfile(T, filename) == TOKU_STATUS_OK), filename);
}


static const tokuL_Entry pkg_funcs[] = {
    {"loadlib", pkg_loadlib},
    {"searchpath", pkg_searchpath},
    /* placeholders */
    {"preload", NULL},
    {"cpath", NULL},
    {"path", NULL},
    {"searchers", NULL},
    {"loaded", NULL},
    {NULL, NULL}
};


static void find_loader(toku_State *T, const char *name) {
    int i;
    tokuL_Buffer msg; /* to build error message */
    /* push 'package.searchers' list to index 2 in the stack */
    if (toku_get_field_str(T, toku_upvalueindex(0), "searchers") != TOKU_T_LIST)
        tokuL_error(T, "'package.searchers' must be list");
    tokuL_buff_init(T, &msg);
    for (i = 0; ; i++) { /* iter over available searchers to find a loader */
        tokuL_buff_push_string(&msg, PREFIX); /* error-message prefix */
        if (t_unlikely(toku_get_index(T, 2, i) == TOKU_T_NIL)) { /* no more? */
            toku_pop(T, 1); /* remove nil */
            tokuL_buffsub(&msg, PREFIX_LEN); /* remove prefix */
            tokuL_buff_end(&msg); /* create error message */
            tokuL_error(T, "module '%s' not found:%s", name, toku_to_string(T,-1));
        }
        toku_push_string(T, name);
        toku_call(T, 1, 2); /* call it */
        if (toku_is_function(T, -2)) { /* did it find a loader? */
            return; /* module loader found */
        } else if (toku_is_string(T, -2)) { /* searcher returned error msg? */
            toku_pop(T, 1); /* remove extra return */
            tokuL_buff_push_stack(&msg); /* concatenate error message */
        } else { /* no error message */
            toku_pop(T, 2); /* remove both returns */
            tokuL_buffsub(&msg, PREFIX_LEN); /* remove prefix */
        }
    }
}


static int l_import(toku_State *T) {
    const char *name = tokuL_check_string(T, 0);
    toku_setntop(T, 1); /* __LOADED table will be at index 1 */
    toku_get_cfield_str(T, TOKU_LOADED_TABLE); /* get __LOADED table */
    toku_get_field_str(T, 1, name); /* get __LOADED[name] */
    if (toku_to_bool(T, -1)) /* is it there? */
        return 1; /* package is already loaded */
    /* else must load package */
    toku_pop(T, 1); /* remove result */
    find_loader(T, name);
    toku_rotate(T, -2, 1); /* loader function <-> loader data */
    toku_push(T, 0); /* name is 1st argument to module loader */
    toku_push(T, -3); /* loader data is 2nd argument */
    /* stack: ...; loader data; loader function; mod. name; loader data */
    toku_call(T, 2, 1); /* run loader to load module */
    /* stack: ...; loader data; result from loader */
    if (!toku_is_nil(T, -1)) /* non-nil return? */
        toku_set_field_str(T, 1, name); /* __LOADED[name] = result from loader */
    else
        toku_pop(T, 1); /* remove nil */
    if (toku_get_field_str(T, 1, name) == TOKU_T_NIL) { /* module set no value? */
        toku_push_bool(T, 1); /* use true as result */
        toku_copy(T, -1, -2); /* replace loader result */
        toku_set_field_str(T, 1, name); /* __LOADED[name] = true */
    }
    toku_rotate(T, -2, 1); /* loader data <-> module result  */
    return 2; /* return module result and loader data (in that order) */
}


static const tokuL_Entry load_funcs[] = {
    {"import", l_import},
    {NULL, NULL}
};


/*
** Finalizer for clibs: calls 'csys_unloadlib' for all lib
** handles in clibs list upvalue in reverse order.
*/
static int gcmm(toku_State *T) {
    int i;
    toku_get_uservalue(T, -1, 0); /* get list upvalue */
    /* get initial lib handle index (reverse) */
    i = toku_find_index(T, -1, TOKU_FI_REV, 1, cast_int(toku_len(T, -1)));
    while (0 < i) { /* for each handle (in reverse order) */
        toku_get_index(T, -1, i); /* get handle */
        if (t_unlikely(csys_unloadlib(T, toku_to_userdata(T, -1)) != 0))
            toku_error(T); /* unloading failed; error string is on top */
        toku_pop(T, 1); /* pop handle */
        i = toku_find_index(T, -1, TOKU_FI_REV, 1, i);
    }
    return 0;
}


/*
** CLIBS is full userdata with one user value.
** The upvalues is a list holding table at index 0 used for
** querying loaded libraries, while the rest of indices hold
** loaded C libraries. When created, it is set under key "__CLIBS"
** in the ctable.
*/
static void create_clibs_userdata(toku_State *T) {
    if (toku_get_cfield_str(T, CLIBS) != TOKU_T_USERDATA) {
        toku_pop(T, 1); /* remove value */
        toku_push_userdata(T, 0, 1); /* create clibs userdata */
        toku_push_list(T, 1); /* create the user value */
        toku_push_table(T, 0); /* create query table */
        toku_set_index(T, -2, 0); /* set query table into the list */
        toku_set_uservalue(T, -2, 0); /* set list as first usr val of clibs */
        toku_push(T, -1); /* copy of clibs */
        toku_set_cfield_str(T, CLIBS); /* ctable[CLIBS] = userdata */
    }
    toku_push_table(T, 1); /* push metatable */
    toku_push_cfunction(T, gcmm); /* push finalizer */
    toku_set_field_str(T, -2, "__gc"); /* metatable.__gc = gcmm*/
    toku_set_metatable(T, -2); /* set clibs metatable */
    toku_pop(T, 1); /* pop clibs */
}


/*
** Try to find a load function for module 'modname' at file 'filename'.
** First, change '.' to '_' in 'modname'; then, if 'modname' has
** the form X-Y (that is, it has an "ignore mark"), build a function
** name "tokuopen_X" and look for it.
** If there is no ignore mark, look for a function named "tokuopen_modname".
*/
static int load_func(toku_State *T, const char *filename, const char *modname) {
    const char *openfunc;
    const char *mark;
    modname = tokuL_gsub(T, modname, ".", TOKU_OFSEP);
    mark = strchr(modname, *TOKU_IGMARK);
    if (mark) { /* have '-' (ignore mark)? */
        openfunc = toku_push_lstring(T, modname, cast_diff2sz(mark - modname));
        openfunc = toku_push_fstring(T, TOKU_POF"%s", openfunc);
    } else /* no ignore mark (will try "tokuopen_modname") */
        openfunc = toku_push_fstring(T, TOKU_POF"%s", modname);
    return look_for_func(T, filename, openfunc);
}


static int searcher_C(toku_State *T) {
    const char *name = tokuL_check_string(T, 0);
    const char *filename = find_file(T, name, "cpath", TOKU_DIRSEP);
    if (filename == NULL) return 1;  /* module not found in this path */
    return check_load(T, (load_func(T, filename, name) == 0), filename);
}


static int searcher_Croot(toku_State *T) {
    const char *filename;
    const char *name = tokuL_check_string(T, 0);
    const char *p = strchr(name, '.');
    int res;
    if (p == NULL) return 0; /* is root */
    toku_push_lstring(T, name, cast_diff2sz(p - name));
    filename = find_file(T, toku_to_string(T, -1), "cpath", TOKU_DIRSEP);
    if (filename == NULL) return 1; /* root not found */
    if ((res = load_func(T, filename, name)) != 0) { /* error? */
        if (res != ERRFUNC)
            return check_load(T, 0, filename); /* real error */
        else { /* open function not found */
            toku_push_fstring(T, "no module '%s' in file '%s'", name, filename);
            return 1;
        }
    }
    toku_push_string(T, filename); /* will be 2nd argument to module */
    return 2; /* return open function and filename */
}


static void create_searchers_array(toku_State *T) {
    static const toku_CFunction searchers[] = {
        searcher_preload,
        searcher_Tokudae,
        searcher_C,
        searcher_Croot,
        NULL
    };
    /* create 'searchers' list ('package' table is on stack top) */
    toku_push_list(T, sizeof(searchers)/sizeof(searchers[0]) - 1);
    /* fill it with predefined searchers */
    for (int i = 0; searchers[i] != NULL; i++) {
        toku_push(T, -2); /* set 'package' as upvalue for all searchers */
        toku_push_cclosure(T, searchers[i], 1);
        toku_set_index(T, -2, i);
    }
    toku_set_field_str(T, -2, "searchers"); /* package.searchers = list */
}


/*
** TOKU_PATH_VAR and TOKU_CPATH_VAR are the names of the environment
** variables that Tokudae checks to set its paths.
*/
#if !defined(TOKU_PATH_VAR)
#define TOKU_PATH_VAR     "TOKU_PATH"
#endif

#if !defined(TOKU_CPATH_VAR)
#define TOKU_CPATH_VAR    "TOKU_CPATH"
#endif


/*
** Return __G["TOKU_NOENV"] as a boolean.
*/
static int noenv(toku_State *T) {
    int b;
    toku_get_cfield_str(T, "TOKU_NOENV");
    b = toku_to_bool(T, -1);
    toku_pop(T, 1); /* remove value */
    return b;
}


/* set a path */
static void setpath(toku_State *T, const char *fieldname, const char *envname,
                     const char *dflt) {
    const char *dfltmark;
    const char *nver = toku_push_fstring(T, "%s%s", envname, TOKU_VERSUFFIX);
    const char *path = getenv(nver); /* try versioned name */
    if (path == NULL) /* no versioned environment variable? */
        path = getenv(envname); /* try unversioned name */
    if (path == NULL || noenv(T)) /* no environment variable? */
        toku_push_string(T, dflt); /* use default */
    else if ((dfltmark = strstr(path, TOKU_PATH_SEP TOKU_PATH_SEP)) == NULL)
        toku_push_string(T, path); /* nothing to change */
    else { /* path contains a ";;": insert default path in its place */
        size_t len = strlen(path);
        tokuL_Buffer b;
        tokuL_buff_init(T, &b);
        if (path < dfltmark) { /* is there a prefix before ';;'? */
            tokuL_buff_push_lstring(&b, path, cast_diff2sz(dfltmark - path));
            tokuL_buff_push(&b, *TOKU_PATH_SEP);
        }
        tokuL_buff_push_string(&b, dflt); /* add default */
        if (dfltmark < path + len - 2) { /* is there a suffix after ';;'? */
            tokuL_buff_push(&b, *TOKU_PATH_SEP);
            tokuL_buff_push_lstring(&b, dfltmark + 2,
                                        cast_diff2sz((path+len-2) - dfltmark));
        }
        tokuL_buff_end(&b);
    }
    setprogdir(T);
    toku_set_field_str(T, -3, fieldname); /* package[fieldname] = path value */
    toku_pop(T, 1); /* pop versioned variable name ('nver') */
}


TOKUMOD_API int tokuopen_package(toku_State *T) {
    create_clibs_userdata(T); /* create clibs userdata */
    tokuL_push_lib(T, pkg_funcs); /* create 'package' table */
    create_searchers_array(T); /* set 'package.searchers' */
    setpath(T, "path", TOKU_PATH_VAR, TOKU_PATH_DEFAULT); /* 'package.path' */
    setpath(T, "cpath", TOKU_CPATH_VAR, TOKU_CPATH_DEFAULT); /* 'package.cpath' */
    /* set 'package.config' */
    toku_push_literal(T, TOKU_DIRSEP "\n" TOKU_PATH_SEP "\n" TOKU_PATH_MARK "\n"
                        TOKU_EXEC_DIR "\n" TOKU_IGMARK "\n");
    toku_set_field_str(T, -2, "config");
    /* ctable[__LOADED] = table */
    tokuL_get_subtable(T, TOKU_CTABLE_INDEX, TOKU_LOADED_TABLE);
    toku_set_field_str(T, -2, "loaded"); /* 'package.loaded' = __LOADED */
    /* ctable[__PRELOAD] = table */
    tokuL_get_subtable(T, TOKU_CTABLE_INDEX, TOKU_PRELOAD_TABLE);
    toku_set_field_str(T, -2, "preload"); /* 'package.preload' = __PRELOAD */
    toku_push_globaltable(T); /* open library into global table */
    toku_push(T, -2); /* set 'package' as upvalue for next lib */
    tokuL_set_funcs(T, load_funcs, 1);
    toku_pop(T, 1); /* pop global table */
    return 1; /* return 'package' table */
}
