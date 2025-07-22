/*
** tokudaeaux.c
** Auxiliary library
** See Copyright Notice in tokudae.h
*/

#define tokudaeaux_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelimits.h"


/* {=======================================================================
** Errors
** ======================================================================== */

TOKULIB_API int tokuL_error(toku_State *T, const char *fmt, ...) {
    va_list ap;
    tokuL_where(T, 1);
    va_start(ap, fmt);
    toku_push_vfstring(T, fmt, ap);
    va_end(ap);
    toku_concat(T, 2);
    va_end(ap);
    return toku_error(T);
}


/*
** Find field in table at idx -1. If field value is found
** meaning the object at 'idx' is equal to the field value, then
** this function returns 1. Only string keys are considered and
** limit is the limit of recursive calls in case table field
** contains another table value.
*/
static int findfield(toku_State *T, int idx, int limit) {
    if (limit == 0 || !toku_is_table(T, -1))
        return 0; /* not found */
    toku_push_nil(T); /* start 'next' loop (from beginning) */
    while (toku_nextfield(T, -2)) { /* for each pair in the table */
        if (toku_type(T, -2) == TOKU_T_STRING) { /* ignore non-string keys */
            if (toku_rawequal(T, idx, -1)) { /* found object (function)? */
                toku_pop(T, 1); /* remove value (but keep name) */
                return 1;
            } else if (findfield(T, idx, limit - 1)) { /* try recursively */
                /* stack: lib_name, lib_table, field_name (top) */
                toku_push_literal(T, "."); /* place '.' between the names */
                toku_replace(T, -3); /* (in the slot occupied by table) */
                toku_concat(T, 3); /* lib_name.field_name */
                return 1;
            }
        }
        toku_pop(T, 1); /* remove value */
    }
    return 0; /* not found */
}


/*
** Try and push name of the currently active function in 'ar'.
** If function is global function, then the its name is pushed
** on the top of the stack.
*/
static int push_glbfunc_name(toku_State *T, toku_Debug *ar) {
    int top = toku_gettop(T); /* index of value on top of the stack */
    int func = top + 1;
    toku_getinfo(T, "f", ar); /* push function (top + 1) */
    toku_get_cfield_str(T, TOKU_LOADED_TABLE);
    tokuL_check_stack(T, 6, "not enough stack space"); /* for 'findfield' */
    if (findfield(T, func, 2)) { /* found? */
        const char *name = toku_to_string(T, -1);
        if (strncmp(name, TOKU_GNAME ".", 4) == 0) { /* starts with '__G.'? */
            toku_push_string(T, name + 4); /* push name without prefix */
            toku_remove(T, -2); /* remove original name */
        }
        toku_copy(T, -1, func); /* copy name to proper place */
        toku_setntop(T, func + 1); /* remove "loaded" table and name copy */
        return 1;
    } else { /* not a global */
        toku_setntop(T, func); /* remove func and Lib */
        return 0;
    }
}


TOKULIB_API int tokuL_error_arg(toku_State *T, int arg, const char *extra) {
    toku_Debug ar;
    if (!toku_getstack(T, 0, &ar)) /* no stack frame? */
        return tokuL_error(T, "bad argument #%d (%s)", arg+1, extra);
    toku_getinfo(T, "n", &ar);
    if (strcmp(ar.namewhat, "metamethod") == 0) {
        /* NOTE: currently, this is unreachable! */
        toku_assert(0);
        arg--; /* ignore 'self' */
        if (arg == -1) /* 'self' is the invalid argument? */
            tokuL_error(T, "calling '%s' on a bad 'self' (%s)", ar.name, extra);
    }
    if (ar.name == NULL)
        ar.name = (push_glbfunc_name(T, &ar)) ? toku_to_string(T, -1) : "?";
    return tokuL_error(T, "bad argument #%d to '%s' (%s)", arg+1, ar.name, extra);
}


TOKULIB_API int tokuL_error_type(toku_State *T, int arg, const char *tname) {
    const char *msg, *type;
    if (tokuL_get_metafield(T, arg, "__name") == TOKU_T_STRING)
        type = toku_to_string(T, -1);
    else
        type = tokuL_typename(T, arg);
    msg = toku_push_fstring(T, "%s expected, instead got %s", tname, type);
    return tokuL_error_arg(T, arg, msg);
}


static void terror(toku_State *T, int arg, int t) {
    tokuL_error_type(T, arg, toku_typename(T, t));
}

/* }======================================================================= */


/* {=======================================================================
** Required argument/option and other checks
** ======================================================================== */

TOKULIB_API toku_Number tokuL_check_number(toku_State *T, int idx) {
    int isnum;
    toku_Number n = toku_to_numberx(T, idx, &isnum);
    if (t_unlikely(!isnum))
        terror(T, idx, TOKU_T_NUMBER);
    return n;
}


static void interror(toku_State *T, int argindex) {
    if (toku_is_number(T, argindex))
        tokuL_error_arg(T, argindex, "number has no integer representation");
    else
        terror(T, argindex, TOKU_T_NUMBER);
}


TOKULIB_API toku_Integer tokuL_check_integer(toku_State *T, int idx) {
    int isint;
    toku_Integer i = toku_to_integerx(T, idx, &isint);
    if (t_unlikely(!isint))
        interror(T, idx);
    return i;
}


TOKULIB_API const char *tokuL_check_lstring(toku_State *T, int idx,
                                                           size_t *len) {
    const char *str = toku_to_lstring(T, idx, len);
    if (t_unlikely(str == NULL))
        terror(T, idx, TOKU_T_STRING);
    return str;
}


TOKULIB_API void *tokuL_check_userdata(toku_State *T, int idx, const char *n) {
    void *p = tokuL_test_userdata(T, idx, n);
    if (t_unlikely(p == NULL))
        tokuL_error_type(T, idx, n);
    return p;
}


TOKULIB_API void tokuL_check_stack(toku_State *T, int sz, const char *msg) {
    if (t_unlikely(!toku_checkstack(T, sz))) {
        if (msg)
            tokuL_error(T, "stack overflow (%s)", msg);
        else
            tokuL_error(T, "stack overflow");
    }
}


TOKULIB_API void tokuL_check_type(toku_State *T, int arg, int t) {
    if (t_unlikely(toku_type(T, arg) != t))
        terror(T, arg, t);
}


TOKULIB_API void tokuL_check_any(toku_State *T, int arg) {
    if (t_unlikely(toku_type(T, arg) == TOKU_T_NONE))
        tokuL_error_arg(T, arg, "value expected");
}


TOKULIB_API int tokuL_check_option(toku_State *T, int arg, const char *dfl,
                                   const char *const opts[]) {
    const char *str = dfl ? tokuL_opt_string(T, arg, dfl)
                          : tokuL_check_string(T, arg);
    for (int i = 0; opts[i]; i++)
        if (strcmp(str, opts[i]) == 0)
            return i;
    return tokuL_error_arg(T, arg,
                         toku_push_fstring(T, "invalid option '%s'", str));
}

/* }======================================================================= */


/* {=======================================================================
** Optional argument
** ======================================================================== */

TOKULIB_API toku_Number tokuL_opt_number(toku_State *T, int idx,
                                                        toku_Number dfl) {
    return tokuL_opt(T, tokuL_check_number, idx, dfl);
}


TOKULIB_API toku_Integer tokuL_opt_integer(toku_State *T, int idx,
                                                          toku_Integer dfl) {
    return tokuL_opt(T, tokuL_check_integer, idx, dfl);
}


TOKULIB_API const char *tokuL_opt_lstring(toku_State *T, int idx,
                                          const char *dfl, size_t *plen) {
    if (toku_is_noneornil(T, idx)) {
        if (plen)
            *plen = (dfl ? strlen(dfl) : 0);
        return dfl;
    }
    return tokuL_check_lstring(T, idx, plen);
}

/* }======================================================================= */


/* {=======================================================================
** Chunk loading
** ======================================================================== */

typedef struct LoadFile {
    int n; /* number of pre-read characters */
    FILE *fp; /* file being read */
    char buffer[BUFSIZ];
} LoadFile;


static const char *filereader(toku_State *T, void *data, size_t *szread) {
    LoadFile *fr = (LoadFile *)data;
    (void)T; /* unused */
    if (fr->n > 0) { /* have pre-read characters ? */
        *szread = fr->n;
        fr->n = 0;
    } else { /* read from a file */
        if (feof(fr->fp)) return NULL;
        *szread = fread(fr->buffer, 1, sizeof(fr->buffer), fr->fp);
    }
    return fr->buffer;
}


static int errorfile(toku_State *T, const char *what, int fidx) {
    int err = errno;
    const char *fname = toku_to_string(T, fidx);
    if (err != 0)
        toku_push_fstring(T, "cannot %s %s: %s", what, fname, strerror(err)); 
    else
        toku_push_fstring(T, "cannot %s %s", what, fname);
    toku_remove(T, fidx);
    return TOKU_STATUS_EFILE;
}


TOKULIB_API int tokuL_loadfile(toku_State *T, const char *fname) {
    LoadFile lf = {0};
    int status, readstatus;
    int filename_index = toku_getntop(T);
    if (fname == NULL) { /* stdin? */
        toku_push_string(T, "=stdin");
        lf.fp = stdin;
    } else { /* otherwise real file */
        toku_push_fstring(T, "@%s", fname);
        errno = 0;
        lf.fp = fopen(fname, "r");
        if (lf.fp == NULL)
            return errorfile(T, "open", filename_index);
    }
    status = toku_load(T, filereader, &lf, toku_to_string(T, -1));
    readstatus = ferror(lf.fp);
    if (fname) /* real file ? */
        fclose(lf.fp); /* close it */
    if (readstatus) { /* error while reading */
        toku_setntop(T, filename_index); /* remove any results */
        return errorfile(T, "read", filename_index);
    }
    toku_remove(T, filename_index);
    return status;
}


typedef struct LoadString {
    const char *str;
    size_t sz;
} LoadString;


static const char *stringreader(toku_State *T, void *data, size_t *szread) {
    LoadString *ls = (LoadString *)data;
    (void)T; /* unused */
    if (ls->sz == 0) return NULL;
    *szread = ls->sz;
    ls->sz = 0;
    return ls->str;
}


TOKULIB_API int tokuL_loadstring(toku_State *T, const char *s) {
    return tokuL_loadbuffer(T, s, strlen(s), s);
}


TOKULIB_API int tokuL_loadbuffer(toku_State *T, const char *buff, size_t sz,
                             const char *name) {
    LoadString ls;
    ls.sz = sz;
    ls.str = buff;
    return toku_load(T, stringreader, &ls, name);
}

/* }======================================================================= */


/* {=======================================================================
** Userdata and metatable functions
** ======================================================================== */

TOKULIB_API int tokuL_new_metatable(toku_State *T, const char *tname) {
    if (tokuL_get_metatable(T, tname) != TOKU_T_NIL) /* name already in use? */
        return 0; /* false; metatable already exists */
    toku_pop(T, 1); /* remove nil */
    toku_push_table(T, 2); /* create metatable */
    toku_push_string(T, tname);
    toku_set_field_str(T, -2, "__name"); /* metatable.__name = tname */
    toku_push(T, -1); /* metatable copy */
    toku_set_cfield_str(T, tname); /* ctable.tname = metatable */
    return 1; /* true; created new metatable */
}


// TODO: update docs
TOKULIB_API void tokuL_set_metatable(toku_State *T, const char *tname) {
    tokuL_get_metatable(T, tname);
    toku_set_metatable(T, -2);
}


TOKULIB_API int tokuL_get_metafield(toku_State *T, int idx, const char *ev) {
    if (!toku_get_metatable(T, idx))
        return TOKU_T_NONE;
    else {
        int t = toku_get_field_str(T, -1, ev);
        if (t == TOKU_T_NIL) {
            toku_pop(T, 2); /* remove metatable and nil */
            t = TOKU_T_NONE;
        } else
            toku_remove(T, -2); /* remove only metatable */
        return t; /* return value type */
    }
}


TOKULIB_API int tokuL_callmeta(toku_State *T, int idx, const char *event) {
    int t = toku_type(T, idx);
    idx = toku_absindex(T, idx);
    if ((t != TOKU_T_INSTANCE && t != TOKU_T_USERDATA) ||
            tokuL_get_metafield(T, idx, event) == TOKU_T_NONE)
        return 0;
    toku_push(T, idx);
    toku_call(T, 1, 1);
    return 1;
}


TOKULIB_API void *tokuL_test_userdata(toku_State *T, int idx, const char *tn) {
    void *p = tokuL_to_fulluserdata(T, idx);
    if (p != NULL) { /* 'idx' is full userdata? */
        if (toku_get_metatable(T, 0)) { /* it has a metatable? */
            tokuL_get_metatable(T, tn); /* get correct metatable */
            if (!toku_rawequal(T, -1, -2)) /* not the same? */
                p = NULL;
            toku_pop(T, 2); /* remove both metatables */
            return p;
        }
    }
    return NULL; /* value is not a userdata with a metatable */
}

/* }======================================================================= */


/* {=======================================================================
** File/Exec result process functions
** ======================================================================== */

TOKULIB_API int tokuL_fileresult(toku_State *T, int ok, const char *fname) {
    int err = errno;
    if (ok) { /* ok? */
        toku_push_bool(T, 1);
        return 1; /* return true */
    } else {
        const char *msg = (err != 0) ? strerror(err) : "(no extra info)";
        tokuL_push_fail(T);
        if (fname) /* have file name? */
            toku_push_fstring(T, "%s: %s", fname, msg);
        else
            toku_push_string(T, msg);
        toku_push_integer(T, err);
        return 3; /* return fail, msg, and error code */
    }
}


#if !defined(t_inspectstat)	/* { */

#if defined(TOKU_USE_POSIX)

#include <sys/wait.h>

/* use appropriate macros to interpret 'pclose' return status */
#define t_inspectstat(stat,what)  \
    if (WIFEXITED(stat)) { stat = WEXITSTATUS(stat); } \
    else if (WIFSIGNALED(stat)) { stat = WTERMSIG(stat); what = "signal"; }

#else

#define t_inspectstat(stat,what)  /* no op */

#endif

#endif				/* } */


TOKULIB_API int tokuL_execresult(toku_State *T, int stat) {
    if (stat != 0 && errno != 0) /* error with an 'errno'? */
        return tokuL_fileresult(T, 0, NULL);
    else {
        const char *what = "exit"; /* type of termination */
        t_inspectstat(stat, what); /* interpret result */
        if (*what == 'e' && stat == 0) /* successful termination? */
            toku_push_bool(T, 1);
        else
            tokuL_push_fail(T);
        toku_push_string(T, what);
        toku_push_integer(T, stat);
        return 3; /* return true/fail, what and code */
    }
}

/* }======================================================================= */


/* {=======================================================================
** Miscellaneous functions
** ======================================================================== */

TOKULIB_API const char *tokuL_to_lstring(toku_State *T, int idx,
                                                        size_t *plen) {
    idx = toku_absindex(T, idx);
    if (tokuL_callmeta(T, idx, "__tostring")) {
        if (!toku_is_string(T, -1))
            tokuL_error(T, "'__tostring' must return a string");
    } else {
        switch (toku_type(T, idx)) {
            case TOKU_T_NIL: {
                toku_push_literal(T, "nil");
                break;
            }
            case TOKU_T_BOOL: {
                toku_push_string(T, (toku_to_bool(T, idx) ? "true" : "false"));
                break;
            }
            case TOKU_T_NUMBER: {
                if (toku_is_integer(T, idx))
                    toku_push_fstring(T, "%I", toku_to_integer(T, idx));
                else
                    toku_push_fstring(T, "%f", toku_to_number(T, idx));
                break;
            }
            case TOKU_T_STRING: {
                toku_push(T, idx);
                break;
            }
            default: {
                /* get metatable entry '__name' */
                int tt = tokuL_get_metafield(T, idx, "__name");
                const char *kind = (tt == TOKU_T_STRING) /* is it a string? */
                                 ? toku_to_string(T, -1) /* use it */
                                 : tokuL_typename(T, idx); /* fallback name */
                toku_push_fstring(T, "%s: %p", kind, toku_to_pointer(T, idx));
                if (tt != TOKU_T_NONE)
                    toku_remove(T, -2); /* remove '__name' */
                break;
            }
        }
    }
    return toku_to_lstring(T, -1, plen);
}


TOKULIB_API void *tokuL_to_fulluserdata(toku_State *T, int idx) {
    return toku_is_fulluserdata(T, idx) ? toku_to_userdata(T, idx) : NULL;
}


TOKULIB_API void *tokuL_to_lightuserdata(toku_State *T, int idx) {
    return toku_is_lightuserdata(T, idx) ? toku_to_userdata(T, idx) : NULL;
}


TOKULIB_API void tokuL_where(toku_State *T, int level) {
    toku_Debug ar;
    if (toku_getstack(T, level, &ar)) {
        toku_getinfo(T, "sl", &ar);
        if (ar.currline > 0) { /* have info? */
            toku_push_fstring(T, "%s:%d: ", ar.shortsrc, ar.currline);
            return;
        }
    }
    toku_push_literal(T, "");
}


static void *allocator(void *ptr, size_t osz, size_t nsz, void *ud) {
    (void)osz; (void)ud; /* unused */
    if (nsz == 0) {
        free(ptr);
        return NULL;
    } else
        return realloc(ptr, nsz);
}


static int panic(toku_State *T) {
    const char *msg = (toku_type(T, -1) == TOKU_T_STRING
                       ? toku_to_string(T, -1)
                       : "error object is not a string");
    toku_writefmt(stderr, "PANIC: unprotected error in call to "
                          "Tokudae API: %s\n", msg);
    return 0; /* return to abort */
}


static void fwarnon(void *ud, const char *msg, int tocont);
static void fwarnoff(void *ud, const char *msg, int tocont);


static int warning_checkmessage(toku_State *T, const char *msg, int tocont) {
    if (tocont || *msg++ != '@') /* not a control message? */
        return 0;
    else {
        if (strcmp(msg, "on") == 0) /* turn warnings on */
            toku_setwarnf(T, fwarnon, T);
        else if (strcmp(msg, "off") == 0) /* turn warnings off */
            toku_setwarnf(T, fwarnoff, T);
        return 1; /* it was a control message */
    }
}


static void fwarncont(void *ud, const char *msg, int tocont) {
    toku_State *T = (toku_State *)ud;
    toku_writefmt(stderr, "%s", msg);
    if (tocont) /* to be continued? */
        toku_setwarnf(T, fwarncont, ud);
    else { /* this is the end of the warning */
        toku_writefmt(stderr, "%s", "\n");
        toku_setwarnf(T, fwarnon, T);
    }
}


static void fwarnon(void *ud, const char *msg, int tocont) {
    if (warning_checkmessage((toku_State *)ud, msg, tocont))
        return; /* it was a control message */
    toku_writefmt(stderr, "%s", "Tokudae warning: ");
    fwarncont(ud, msg, tocont);
}


static void fwarnoff(void *ud, const char *msg, int tocont) {
    warning_checkmessage((toku_State *)ud, msg, tocont);
}


#if !defined(tokui_makeseed)

#include <time.h>


/* Size for the buffer, in bytes */
#define BUFSEEDB	(sizeof(void*) + sizeof(time_t))

/* Size for the buffer in int's, rounded up */
#define BUFSEED		((BUFSEEDB + sizeof(int) - 1) / sizeof(int))

/*
** Copy the contents of variable 'v' into the buffer pointed by 'b'.
** (The '&b[0]' disguises 'b' to fix an absurd warning from clang.)
*/
#define addbuff(b,v)	(memcpy(&b[0], &(v), sizeof(v)), b += sizeof(v))


static unsigned int tokui_makeseed(void) {
    unsigned int buff[BUFSEED];
    unsigned int res;
    unsigned int i;
    time_t t = time(NULL);
    char *b = (char*)buff;
    addbuff(b, b);  /* local variable's address */
    addbuff(b, t);  /* time */
    /* fill (rare but possible) remain of the buffer with zeros */
    memset(b, 0, sizeof(buff) - BUFSEEDB);
    res = buff[0];
    for (i = 1; i < BUFSEED; i++)
        res ^= (res >> 3) + (res << 7) + buff[i];
    return res;
}

#endif


TOKULIB_API toku_State *tokuL_newstate(void) {
    toku_State *T = toku_newstate(allocator, NULL, tokui_makeseed());
    if (t_likely(T)) {
        toku_atpanic(T, panic);
        toku_setwarnf(T, fwarnoff, T); /* warnings off by default */
    }
    return T;
}


TOKULIB_API int tokuL_get_subtable(toku_State *T, int idx, const char *k) {
    if (toku_get_field_str(T, idx, k) == TOKU_T_TABLE) {
        return 1; /* true, already have table */
    } else {
        toku_pop(T, 1); /* pop previous result */
        idx = toku_absindex(T, idx);
        toku_push_table(T, 0);
        toku_push(T, -1); /* copy will be left on the top */
        toku_set_field_str(T, idx, k); /* table[k] = newtable */
        return 0; /* false, no table was found */
    }
}


TOKULIB_API void tokuL_importf(toku_State *T, const char *modname,
                               toku_CFunction openf, int global) {
    tokuL_get_subtable(T, TOKU_CTABLE_INDEX, TOKU_LOADED_TABLE);
    toku_get_field_str(T, -1, modname); /* get __LOADED[modname] */
    if (!toku_to_bool(T, -1)) { /* package not already loaded? */
        toku_pop(T, 1); /* remove field */
        toku_push_cfunction(T, openf); /* push func that opens the module */
        toku_push_string(T, modname); /* argument to 'openf' */
        toku_call(T, 1, 1); /* call 'openf' */
        toku_push(T, -1);  /* make copy of the module (call result) */
        toku_set_field_str(T, -3, modname); /* __LOADED[modname] = module */
    }
    toku_remove(T, -2); /* remove __LOADED table */
    if (global) { /* set the module as global? */
        toku_push(T, -1); /* copy of module */
        toku_set_global(T, modname); /* __G[modname] = module */
    }
}


/* find and return last call frame level */
static int lastlevel(toku_State *T) {
    toku_Debug ar;
    int low = 0, high = 0;
    /* get upper bound, and store last known valid level in 'low' */
    while (toku_getstack(T, high, &ar)) {
        low = high;
        high += (high == 0); /* avoid multiplying by 0 */
        high *= 2;
    }
    /* binary search between 'low' and 'high' levels */
    while (low < high) {
        int mid = low + ((high - low)/2);
        if (toku_getstack(T, mid, &ar))
            low = mid + 1;
        else
            high = mid;
    }
    return low - 1;
}


static void push_func_name(toku_State *T, toku_Debug *ar) {
    if (*ar->namewhat != '\0') /* name from code? */
        toku_push_fstring(T, "%s '%s'", ar->namewhat, ar->name);
    else if (*ar->what == 'm') /* main? */
        toku_push_literal(T, "main chunk");
    else if (push_glbfunc_name(T, ar)) { /* try global name */
        toku_push_fstring(T, "function '%s'", toku_to_string(T, -1));
        toku_remove(T, -2); /* remove name */
    } else if (*ar->what != 'C') /* for Tokudae functions, use <file:line> */
        toku_push_fstring(T, "function <%s:%d>", ar->shortsrc, ar->defline);
    else /* unknown */
        toku_push_literal(T, "?");
}


#define STACKLEVELS     10

TOKULIB_API void tokuL_traceback(toku_State *T, toku_State *T1,
                                 int level, const char *msg) {
    tokuL_Buffer B;
    toku_Debug ar;
    int last = lastlevel(T1);
    int limit2show = (last - level > (STACKLEVELS * 2) ? STACKLEVELS : -1);
    tokuL_buff_init(T, &B);
    if (msg) {
        tokuL_buff_push_string(&B, msg);
        tokuL_buff_push(&B, '\n');
    }
    tokuL_buff_push_string(&B, "stack traceback:");
    while (toku_getstack(T1, level++, &ar)) { /* tracing back... */
        if (limit2show-- == 0) { /* too many levels? */
            int n = last - level - STACKLEVELS + 1; /* levels to skip */
            toku_push_fstring(T, "\n\t...\t(skipping %d levels)", n);
            tokuL_buff_push_stack(&B);
            level += n; /* skip to last levels */
        } else {
            toku_getinfo(T1, "snl", &ar); /* source, name, line info */
            if (ar.currline <= 0)
                toku_push_fstring(T, "\n\t%s in ", ar.shortsrc);
            else
                toku_push_fstring(T, "\n\t%s:%d: in ", ar.shortsrc, ar.currline);
            tokuL_buff_push_stack(&B);
            push_func_name(T, &ar);
            tokuL_buff_push_stack(&B);
        }
    }
    tokuL_buff_end(&B);
}


TOKULIB_API void tokuL_set_funcs(toku_State *T, const tokuL_Entry *l, int nup){
    tokuL_check_stack(T, nup, "too many upvalues");
    for (; l->name != NULL; l++) {
        if (l->func == NULL) { /* placeholder? */
            toku_push_bool(T, 0);
        } else { /* otherwise a function */
            for (int i = 0; i < nup; i++) /* copy upvalues */
                toku_push(T, -nup);
            toku_push_cclosure(T, l->func, nup); /* create closure */
        }
        toku_set_field_str(T, -(nup + 2), l->name);
    }
    toku_pop(T, nup); /* remove upvalues */
}


TOKULIB_API void tokuL_check_version_(toku_State *T, toku_Number ver) {
    toku_Number v = toku_version(T);
    if (v != ver)
        tokuL_error(T,
            "version mismatch: application needs %f, Tokudae core provides %f",
             ver, v);
}


TOKULIB_API unsigned tokuL_makeseed(toku_State *T) {
    (void)(T); /* unused */
    return tokui_makeseed();
}

/* }======================================================================= */


/* {=======================================================================
** Reference system
** ======================================================================== */

/* index of free-list header (after the predefined values) */
#define freelist    (TOKU_CLIST_LAST + 1)

TOKULIB_API int tokuL_ref(toku_State *T, int a) {
    int ref;
    if (toku_is_nil(T, -1)) { /* value on top is 'nil'? */
        toku_pop(T, 1); /* remove it from the stack */
        return TOKU_REFNIL;
    }
    a = toku_absindex(T, a);
    if (toku_get_index(T, a, freelist) == TOKU_T_NIL) { /* first access? */
        ref = 0; /* list is empty */
        toku_push_integer(T, 0); /* initialize empty list */
        toku_set_index(T, a, freelist); /* ref = a[freelist] = 0 */
    } else { /* already initialized */
        toku_assert(toku_is_integer(T, -1));
        ref = (int)toku_to_integer(T, -1); /* ref = a[freelist] */
    }
    toku_pop(T, 1); /* remove element */
    if (ref != 0) { /* any free element? */
        toku_get_index(T, a, ref); /* remove it from list */
        toku_set_index(T, a, freelist); /* (a[freelist] = a[ref]) */
    } else { /* no free elements */
        /* get a new ref */
        ref = (int)toku_find_index(T, a, TOKU_FI_NIL, freelist+1, 0);
    }
    toku_set_index(T, a, ref);
    return ref;
}


TOKULIB_API void tokuL_unref(toku_State *T, int a, int ref) {
    if (ref >= 0) {
        a = toku_absindex(T, a);
        toku_get_index(T, a, freelist);
        toku_assert(toku_is_integer(T, -1));
        toku_set_index(T, a, ref); /* a[ref] = a[freelist] */
        toku_push_integer(T, ref);
        toku_set_index(T, a, freelist); /* a[freelist] = ref */
    }
}

/* }======================================================================= */


/* {=======================================================================
** Buffer manipulation
** ======================================================================== */

typedef struct UserBox {
    void *p; /* data */
    size_t sz; /* size of 'p' (data) */
} UserBox;


static void *resizebox(toku_State *T, int idx, size_t newsz) {
    void *ud;
    toku_Alloc falloc = toku_getallocf(T, &ud);
    UserBox *box = (UserBox *)toku_to_userdata(T, idx);
    void *newblock = falloc(box->p, box->sz, newsz, ud);
    if (t_unlikely(newblock == NULL && newsz > 0)) {
        toku_push_literal(T, "out of memory");
        toku_error(T);
    }
    box->p = newblock;
    box->sz = newsz;
    return newblock;
}


static int boxgc(toku_State *T) {
    resizebox(T, 0, 0);
    return 0;
}


static void newbox(toku_State *T) {
    UserBox *box = toku_push_userdata(T, sizeof(*box), 0);
    box->p = NULL;
    box->sz = 0;
    toku_push_table(T, 2); /* push metatable */
    toku_push_cfunction(T, boxgc);
    toku_set_field_str(T, -2, "__gc"); /* metatable.__gc = boxgc */
    toku_push_cfunction(T, boxgc);
    toku_set_field_str(T, -2, "__close"); /* metatable.__close = boxgc */
    toku_set_metatable(T, -2);
}


/*
** Initializes the buffer 'B' and pushes it's placeholder onto
** the top of the stack as light userdata.
*/
TOKULIB_API void tokuL_buff_init(toku_State *T, tokuL_Buffer *B) {
    B->T = T;
    B->n = 0;
    B->b = B->init.b;
    B->sz = TOKUL_BUFFERSIZE;
    toku_push_lightuserdata(T, B);
}


/* 
** Test whether the buffer is using a temporary arena on stack.
*/
#define buffonstack(B)      ((B)->b != (B)->init.b)


/*
** Whenever buffer is accessed, slot 'idx' must be either a box,
** meaning the buffer is stored as 'UserBox' (which cannot be NULL)
** or it is a placeholder for the buffer (meaning that buffer is
** still using 'init.b[TOKUL_BUFFERSIZE]').
*/
#define checkbufflevel(B, idx) \
        toku_assert(buffonstack(B) \
                ? toku_to_userdata((B)->T, idx) != NULL \
                : toku_to_userdata((B)->T, idx) == (void*)(B))


/* calculate new buffer size */
static size_t newbuffsize(tokuL_Buffer *B, size_t sz) {
    size_t newsize = (B->sz / 2) * 3; /* 1.5x size */
    if (t_unlikely(SIZE_MAX - sz < B->n)) /* would overflow? */
        return tokuL_error(B->T, "buffer too large");
    if (newsize < B->n + sz)
        newsize = B->n + sz;
    return newsize;
}


/*
** Ensure that buffer 'B' can fit 'sz' bytes.
** This also creates 'UserBox' if internal buffer is not big enough.
*/
static char *buffensure(tokuL_Buffer *B, size_t sz, int boxindex) {
    checkbufflevel(B, boxindex);
    if (B->sz - B->n >= sz) { /* have enough space? */
        return B->b + B->n;
    } else { /* otherwise expand */
        char *newb;
        toku_State *T = B->T;
        size_t newsize = newbuffsize(B, sz);
        if (buffonstack(B)) { /* already have 'UserBox'? */
            newb = resizebox(T, boxindex, newsize); /* resize it and done */
        } else {
            toku_remove(T, boxindex); /* remove placeholder */
            newbox(T); /* create new user box on top */
            toku_insert(T, boxindex); /* insert the new box into 'boxindex' */
            toku_toclose(T, boxindex); /* mark box to-be-closed */
            newb = resizebox(T, boxindex, newsize); /* resize it */
            memcpy(newb, B->b, B->n * sizeof(char)); /* copy 'B->init.b' */
        }
        B->b = newb;
        B->sz = newsize;
        return newb + B->n;
    }
}


/*
** Initializes buffer 'B' to the size 'sz' bytes and returns the pointer
** to that memory block.
*/
TOKULIB_API char *tokuL_buff_initsz(toku_State *T, tokuL_Buffer *B,
                                                   size_t sz) {
    tokuL_buff_init(T, B);
    return buffensure(B, sz, -1);
}


/*
** Return the pointer to the free memory block of at least 'sz' bytes.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
TOKULIB_API char *tokuL_buff_ensure(tokuL_Buffer *B, size_t sz) {
    return buffensure(B, sz, -1);
}


/*
** Push sized string into the buffer.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
TOKULIB_API void tokuL_buff_push_lstring(tokuL_Buffer *B, const char *s,
                                                          size_t l) {
    if (l > 0) {
        char *p = buffensure(B, l, -1);
        memcpy(p, s, l*sizeof(char));
        tokuL_buffadd(B, l);
    }
}


/*
** Similar to 'tokuL_buff_push_lstring', the only difference is that this
** function measures the length of 's'.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
TOKULIB_API void tokuL_buff_push_string(tokuL_Buffer *B, const char *s) {
    tokuL_buff_push_lstring(B, s, strlen(s));
}


/*
** Pushes the string value on top of the stack into the buffer.
** This function expects buffer placeholder or its 'UserBox' to be on
** the stack below the string value being pushed, which is on top of
** the stack.
*/
TOKULIB_API void tokuL_buff_push_stack(tokuL_Buffer *B) {
    size_t len;
    const char *str = toku_to_lstring(B->T, -1, &len);
    char *p = buffensure(B, len, -2);
    memcpy(p, str, len);
    tokuL_buffadd(B, len);
    toku_pop(B->T, 1); /* remove string */
}


TOKULIB_API void tokuL_buff_push_gsub(tokuL_Buffer *B, const char *s,
                                      const char *p, const char *r) {
    const char *wild;
    size_t l = strlen(p);
    while ((wild = strstr(s, p)) != NULL) { /* find 'p' (pattern) */
        tokuL_buff_push_lstring(B, s, wild - s); /* push prefix */
        tokuL_buff_push_string(B, r); /* push 'r' in place of pattern */
        s = wild + l; /* continue after 'p' */
    }
    tokuL_buff_push_string(B, s); /* push last suffix */
}


TOKULIB_API const char *tokuL_gsub(toku_State *T, const char *s,
                                   const char *p, const char *r) {
    tokuL_Buffer B;
    tokuL_buff_init(T, &B);
    tokuL_buff_push_gsub(&B, s, p, r);
    tokuL_buff_end(&B);
    return toku_to_string(T, -1);
}


/*
** Finish the use of buffer 'B' leaving the final string on top of
** the stack.
*/
TOKULIB_API void tokuL_buff_end(tokuL_Buffer *B) {
    toku_State *T = B->T;
    checkbufflevel(B, -1);
    toku_push_lstring(T, B->b, B->n);
    if (buffonstack(B)) /* have 'UserBox'? */
        toku_closeslot(T, -2); /* close it -> boxgc */
    toku_remove(T, -2);
}


TOKULIB_API void tokuL_buff_endsz(tokuL_Buffer *B, size_t sz) {
    tokuL_buffadd(B, sz);
    tokuL_buff_end(B);
}

/* }======================================================================= */
