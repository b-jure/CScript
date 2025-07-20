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



CSLIB_API int csL_error(toku_State *T, const char *fmt, ...) {
    va_list ap;
    csL_where(C, 1);
    va_start(ap, fmt);
    toku_push_vfstring(C, fmt, ap);
    va_end(ap);
    toku_concat(C, 2);
    va_end(ap);
    return toku_error(C);
}


/*
** Find field in table at index -1. If field value is found
** meaning the object at 'index' is equal to the field value, then
** this function returns 1. Only string keys are considered and
** limit is the limit of recursive calls in case table field
** contains another table value.
*/
static int findfield(toku_State *T, int index, int limit) {
    if (limit == 0 || !toku_is_table(C, -1))
        return 0; /* not found */
    toku_push_nil(C); /* start 'next' loop (from beginning) */
    while (toku_nextfield(C, -2)) { /* for each pair in the table */
        if (toku_type(C, -2) == TOKU_T_STRING) { /* ignore non-string keys */
            if (toku_rawequal(C, index, -1)) { /* found object (function)? */
                toku_pop(C, 1); /* remove value (but keep name) */
                return 1;
            } else if (findfield(C, index, limit - 1)) { /* try recursively */
                /* stack: lib_name, lib_table, field_name (top) */
                toku_push_literal(C, "."); /* place '.' between the two names */
                toku_replace(C, -3); /* (in the slot occupied by table) */
                toku_concat(C, 3); /* lib_name.field_name */
                return 1;
            }
        }
        toku_pop(C, 1); /* remove value */
    }
    return 0; /* not found */
}


/*
** Try and push name of the currently active function in 'ar'.
** If function is global function, then the its name is pushed
** on the top of the stack.
*/
static int push_glbfunt_name(toku_State *T, toku_Debug *ar) {
    int top = toku_gettop(C); /* index of value on top of the stack */
    int func = top + 1;
    toku_getinfo(C, "f", ar); /* push function (top + 1) */
    toku_get_cfieldstr(C, TOKU_LOADED_TABLE);
    csL_check_stack(C, 6, "not enough stack space"); /* for 'findfield' */
    if (findfield(C, func, 2)) { /* found? */
        const char *name = toku_to_string(C, -1);
        if (strncmp(name, TOKU_GNAME ".", 4) == 0) { /* starts with '__G.'? */
            toku_push_string(C, name + 4); /* push name without prefix */
            toku_remove(C, -2); /* remove original name */
        }
        toku_copy(C, -1, func); /* copy name to proper place */
        toku_setntop(C, func + 1); /* remove "loaded" table and name copy */
        return 1;
    } else { /* not a global */
        toku_setntop(C, func); /* remove func and Lib */
        return 0;
    }
}


CSLIB_API int csL_error_arg(toku_State *T, int arg, const char *extra) {
    toku_Debug ar;
    if (!toku_getstack(C, 0, &ar)) /* no stack frame? */
        return csL_error(C, "bad argument #%d (%s)", arg+1, extra);
    toku_getinfo(C, "n", &ar);
    if (strcmp(ar.namewhat, "metamethod") == 0) {
        /* NOTE: currently, this is unreachable! */
        arg--; /* ignore 'self' */
        if (arg == -1) /* 'self' is the invalid argument? */
            csL_error(C, "calling '%s' on a bad 'self' (%s)", ar.name, extra);
    }
    if (ar.name == NULL)
        ar.name = (push_glbfunt_name(C, &ar)) ? toku_to_string(C, -1) : "?";
    return csL_error(C, "bad argument #%d to '%s' (%s)", arg+1, ar.name, extra);
}


CSLIB_API int csL_error_type(toku_State *T, int arg, const char *tname) {
    const char *msg, *type;
    if (csL_get_metaindex(C, arg, TOKU_MT_NAME) == TOKU_T_STRING)
        type = toku_to_string(C, -1);
    else
        type = csL_typename(C, arg);
    msg = toku_push_fstring(C, "%s expected, instead got %s", tname, type);
    return csL_error_arg(C, arg, msg);
}


static void terror(toku_State *T, int arg, int t) {
    csL_error_type(C, arg, toku_typename(C, t));
}


CSLIB_API toku_Number csL_check_number(toku_State *T, int index) {
    int isnum;
    toku_Number n = toku_to_numberx(C, index, &isnum);
    if (t_unlikely(!isnum))
        terror(C, index, TOKU_T_NUMBER);
    return n;
}


static void interror(toku_State *T, int argindex) {
    if (toku_is_number(C, argindex))
        csL_error_arg(C, argindex, "number has no integer representation");
    else
        terror(C, argindex, TOKU_T_NUMBER);
}


CSLIB_API toku_Integer csL_check_integer(toku_State *T, int index) {
    int isint;
    toku_Integer i = toku_to_integerx(C, index, &isint);
    if (t_unlikely(!isint))
        interror(C, index);
    return i;
}


CSLIB_API const char *csL_check_lstring(toku_State *T, int index, size_t *len) {
    const char *str = toku_to_lstring(C, index, len);
    if (t_unlikely(str == NULL))
        terror(C, index, TOKU_T_STRING);
    return str;
}


CSLIB_API void *csL_check_userdata(toku_State *T, int index, const char *name) {
    void *p = csL_test_userdata(C, index, name);
    if (t_unlikely(p == NULL))
        csL_error_type(C, index, name);
    return p;
}


CSLIB_API void csL_check_stack(toku_State *T, int sz, const char *msg) {
    if (t_unlikely(!toku_checkstack(C, sz))) {
        if (msg)
            csL_error(C, "stack overflow (%s)", msg);
        else
            csL_error(C, "stack overflow");
    }
}


CSLIB_API void csL_check_type(toku_State *T, int arg, int t) {
    if (t_unlikely(toku_type(C, arg) != t))
        terror(C, arg, t);
}


CSLIB_API void csL_check_any(toku_State *T, int arg) {
    if (t_unlikely(toku_type(C, arg) == TOKU_T_NONE))
        csL_error_arg(C, arg, "value expected");
}


CSLIB_API int csL_check_option(toku_State *T, int arg, const char *dfl,
                               const char *const opts[]) {
    const char *str = dfl ? csL_opt_string(C, arg, dfl)
                          : csL_check_string(C, arg);
    for (int i=0; opts[i]; i++)
        if (strcmp(str, opts[i]) == 0)
            return i;
    return csL_error_arg(C, arg,
                         toku_push_fstring(C, "invalid option '%s'", str));
}


CSLIB_API toku_Number csL_opt_number(toku_State *T, int index, toku_Number dfl) {
    return csL_opt(C, csL_check_number, index, dfl);
}


CSLIB_API toku_Integer csL_opt_integer(toku_State *T, int index, toku_Integer dfl) {
    return csL_opt(C, csL_check_integer, index, dfl);
}


CSLIB_API const char *csL_opt_lstring(toku_State *T, int index, const char *dfl,
                                      size_t *plen) {
    if (toku_is_noneornil(C, index)) {
        if (plen)
            *plen = (dfl ? strlen(dfl) : 0);
        return dfl;
    }
    return csL_check_lstring(C, index, plen);
}



typedef struct LoadFile {
    int n; /* number of pre-read characters */
    FILE *fp; /* file being read */
    char buffer[BUFSIZ];
} LoadFile;


static const char *filereader(toku_State *T, void *data, size_t *szread) {
    LoadFile *fr = (LoadFile *)data;
    (void)C; /* unused */
    if (fr->n > 0) { /* have pre-read characters ? */
        *szread = fr->n;
        fr->n = 0;
    } else { /* read from a file */
        if (feof(fr->fp)) return NULL;
        *szread = fread(fr->buffer, 1, sizeof(fr->buffer), fr->fp);
    }
    return fr->buffer;
}


static int errorfile(toku_State *T, const char *what, int filename_index) {
    int err = errno;
    const char *filename = toku_to_string(C, filename_index);
    if (err != 0)
        toku_push_fstring(C, "cannot %s %s: %s", what, filename, strerror(err)); 
    else
        toku_push_fstring(C, "cannot %s %s", what, filename);
    toku_remove(C, filename_index);
    return TOKU_STATUS_EFILE;
}


CSLIB_API int csL_loadfile(toku_State *T, const char *filename) {
    LoadFile lf = {0};
    int status, readstatus;
    int filename_index = toku_getntop(C);
    if (filename == NULL) { /* stdin? */
        toku_push_string(C, "=stdin");
        lf.fp = stdin;
    } else { /* otherwise real file */
        toku_push_fstring(C, "@%s", filename);
        errno = 0;
        lf.fp = fopen(filename, "r");
        if (lf.fp == NULL)
            return errorfile(C, "open", filename_index);
    }
    status = toku_load(C, filereader, &lf, toku_to_string(C, -1));
    readstatus = ferror(lf.fp);
    if (filename) /* real file ? */
        fclose(lf.fp); /* close it */
    if (readstatus) { /* error while reading */
        toku_setntop(C, filename_index); /* remove any results */
        return errorfile(C, "read", filename_index);
    }
    toku_remove(C, filename_index);
    return status;
}


typedef struct LoadString {
    const char *str;
    size_t sz;
} LoadString;


static const char *stringreader(toku_State *T, void *data, size_t *szread) {
    LoadString *ls = (LoadString *)data;
    (void)C; /* unused */
    if (ls->sz == 0) return NULL;
    *szread = ls->sz;
    ls->sz = 0;
    return ls->str;
}


CSLIB_API int csL_loadstring(toku_State *T, const char *s) {
    return csL_loadbuffer(C, s, strlen(s), s);
}


CSLIB_API int csL_loadbuffer(toku_State *T, const char *buff, size_t sz,
                             const char *name) {
    LoadString ls;
    ls.sz = sz;
    ls.str = buff;
    return toku_load(C, stringreader, &ls, name);
}


CSLIB_API int csL_new_metalist(toku_State *T, const char *lname) {
    if (csL_get_metalist(C, lname) != TOKU_T_NIL) /* name already in use? */
        return 0; /* false; metalist already exists */
    toku_pop(C, 1); /* remove nil */
    toku_push_metalist(C); /* create metalist */
    toku_push(C, -1); /* push metalist copy */
    toku_set_cfieldstr(C, lname); /* ctable.lname = metalist */
    return 1; /* true; created new metalist */
}


// TODO: update docs
CSLIB_API void csL_set_metalist(toku_State *T, const char *lname) {
    csL_get_metalist(C, lname);
    toku_set_metalist(C, -2);
}


CSLIB_API int csL_get_metaindex(toku_State *T, int index, int mm) {
    if (!toku_get_metalist(C, index))
        return TOKU_T_NONE;
    else {
        int t = toku_get_index(C, -1, mm);
        if (t == TOKU_T_NIL) {
            toku_pop(C, 2); /* remove metalist and nil */
            t = TOKU_T_NONE;
        } else
            toku_remove(C, -2); /* remove only metalist */
        return t; /* return metalist index value type */
    }
}


CSLIB_API int csL_callmeta(toku_State *T, int index, int mm) {
    int t = toku_type(C, index);
    index = toku_absindex(C, index);
    if ((t != TOKU_T_INSTANCE && t != TOKU_T_USERDATA) ||
            csL_get_metaindex(C, index, mm) == TOKU_T_NONE)
        return 0;
    toku_push(C, index);
    toku_call(C, 1, 1);
    return 1;
}


CSLIB_API int csL_new_usermethods(toku_State *T, const char *tname, int sz) {
    if (csL_get_methods(C, tname) != TOKU_T_NIL) /* name already in use? */
        return 0; /* false; methods table already exists */
    toku_pop(C, 1); /* remove nil */
    toku_push_table(C, sz); /* create methods table */
    toku_push(C, -1); /* push table copy */
    toku_set_cfieldstr(C, tname); /* ctable.tname = table */
    return 1; /* true; created new methods table */
}


CSLIB_API void csL_set_usermethods(toku_State *T, const char *tname) {
    csL_get_methods(C, tname);
    toku_set_methods(C, -2);
}


CSLIB_API void *csL_test_userdata(toku_State *T, int index, const char *lname) {
    void *p = csL_to_fulluserdata(C, index);
    if (p != NULL) { /* 'index' is full userdata? */
        if (toku_get_metalist(C, 0)) { /* it has a metalist? */
            csL_get_metalist(C, lname); /* get correct metalist */
            if (!toku_rawequal(C, -1, -2)) /* not the same? */
                p = NULL;
            toku_pop(C, 2); /* remove both metalists */
            return p;
        }
    }
    return NULL; /* value is not a userdata with a metalist */
}


CSLIB_API void csL_set_metafuncs(toku_State *T, const csL_MetaEntry *l,
                                 int nup) {
    csL_check_stack(C, nup, "too many upvalues");
    for (; l->mm >= 0; l++) { /* for each metamethod */
        if (l->metaf == NULL) /* placeholder? */
            toku_push_bool(C, 0);
        else {
            for (int i = 0; i < nup; i++) /* copy upvalues */
                toku_push(C, -nup);
            toku_push_cclosure(C, l->metaf, nup); /* create closure */
        }
        toku_set_index(C, -(nup + 2), l->mm);
    }
    toku_pop(C, nup);
}


CSLIB_API int csL_fileresult(toku_State *T, int ok, const char *fname) {
    int err = errno;
    if (ok) { /* ok? */
        toku_push_bool(C, 1);
        return 1; /* return true */
    } else {
        const char *msg = (err != 0) ? strerror(err) : "(no extra info)";
        csL_push_fail(C);
        if (fname) /* have file name? */
            toku_push_fstring(C, "%s: %s", fname, msg);
        else
            toku_push_string(C, msg);
        toku_push_integer(C, err);
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


CSLIB_API int csL_execresult(toku_State *T, int stat) {
    if (stat != 0 && errno != 0) /* error with an 'errno'? */
        return csL_fileresult(C, 0, NULL);
    else {
        const char *what = "exit"; /* type of termination */
        t_inspectstat(stat, what); /* interpret result */
        if (*what == 'e' && stat == 0) /* successful termination? */
            toku_push_bool(C, 1);
        else
            csL_push_fail(C);
        toku_push_string(C, what);
        toku_push_integer(C, stat);
        return 3; /* return true/fail, what and code */
    }
}


CSLIB_API const char *csL_to_lstring(toku_State *T, int index, size_t *plen) {
    index = toku_absindex(C, index);
    if (csL_callmeta(C, index, TOKU_MT_TOSTRING)) {
        if (!toku_is_string(C, -1))
            csL_error(C, "'__tostring' must return a string");
    } else {
        switch (toku_type(C, index)) {
            case TOKU_T_NIL: {
                toku_push_literal(C, "nil");
                break;
            }
            case TOKU_T_BOOL: {
                toku_push_string(C, (toku_to_bool(C, index) ? "true" : "false"));
                break;
            }
            case TOKU_T_NUMBER: {
                if (toku_is_integer(C, index))
                    toku_push_fstring(C, "%I", toku_to_integer(C, index));
                else
                    toku_push_fstring(C, "%f", toku_to_number(C, index));
                break;
            }
            case TOKU_T_STRING: {
                toku_push(C, index);
                break;
            }
            default: {
                /* get metalist entry '__name' */
                int tt = csL_get_metaindex(C, index, TOKU_MT_NAME);
                const char *kind = (tt == TOKU_T_STRING) /* is it a string? */
                                 ? toku_to_string(C, -1) /* use it */
                                 : csL_typename(C, index); /* fallback name */
                toku_push_fstring(C, "%s: %p", kind, toku_to_pointer(C, index));
                if (tt != TOKU_T_NONE) toku_remove(C, -2); /* remove '__name' */
                break;
            }
        }
    }
    return toku_to_lstring(C, -1, plen);
}


CSLIB_API void *csL_to_fulluserdata(toku_State *T, int index) {
    return toku_is_fulluserdata(C, index) ? toku_to_userdata(C, index) : NULL;
}


CSLIB_API void *csL_to_lightuserdata(toku_State *T, int index) {
    return toku_is_lightuserdata(C, index) ? toku_to_userdata(C, index) : NULL;
}


CSLIB_API void csL_where(toku_State *T, int level) {
    toku_Debug ar;
    if (toku_getstack(C, level, &ar)) {
        toku_getinfo(C, "sl", &ar);
        if (ar.currline > 0) { /* have info? */
            toku_push_fstring(C, "%s:%d: ", ar.shortsrc, ar.currline);
            return;
        }
    }
    toku_push_literal(C, "");
}


// TODO: add docs
CSLIB_API int csL_get_fieldstr(toku_State *T, int index, const char *field) {
    int t = toku_type(C, index);
    if (t == TOKU_T_INSTANCE || t == TOKU_T_TABLE)
        return toku_get_fieldstr(C, index, field);
    return TOKU_T_NONE;
}


// TODO: update docs?
CSLIB_API int csL_get_property(toku_State *T, int index) {
    if (toku_get_field(C, index) == TOKU_T_NIL) {
        toku_pop(C, 1); /* remove nil */
        toku_get_method(C, index);
    }
    return toku_type(C, -1);
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
    const char *msg = (toku_type(C, -1) == TOKU_T_STRING
                       ? toku_to_string(C, -1)
                       : "error object is not a string");
    toku_writefmt(stderr, "PANIC: unprotected error in call to Tokudae API: %s\n",
                         msg);
    return 0; /* return to abort */
}


static void fwarnon(void *ud, const char *msg, int tocont);
static void fwarnoff(void *ud, const char *msg, int tocont);


static int warning_checkmessage(toku_State *T, const char *msg, int tocont) {
    if (tocont || *msg++ != '@') /* not a control message? */
        return 0;
    else {
        if (strcmp(msg, "on") == 0) /* turn warnings on */
            toku_setwarnf(C, fwarnon, C);
        else if (strcmp(msg, "off") == 0) /* turn warnings off */
            toku_setwarnf(C, fwarnoff, C);
        return 1; /* it was a control message */
    }
}


static void fwarncont(void *ud, const char *msg, int tocont) {
    toku_State *T = (toku_State *)ud;
    toku_writefmt(stderr, "%s", msg);
    if (tocont) /* to be continued? */
        toku_setwarnf(C, fwarncont, ud);
    else { /* this is the end of the warning */
        toku_writefmt(stderr, "%s", "\n");
        toku_setwarnf(C, fwarnon, C);
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


#if !defined(csi_makeseed)

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


static unsigned int csi_makeseed(void) {
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


CSLIB_API toku_State *csL_newstate(void) {
    toku_State *T = toku_newstate(allocator, NULL, csi_makeseed());
    if (t_likely(C)) {
        toku_atpanic(C, panic);
        toku_setwarnf(C, fwarnoff, C); /* warnings off by default */
    }
    return C;
}


CSLIB_API int csL_get_subtable(toku_State *T, int index, const char *field) {
    if (toku_get_fieldstr(C, index, field) == TOKU_T_TABLE) {
        return 1; /* true, already have table */
    } else {
        toku_pop(C, 1); /* pop previous result */
        index = toku_absindex(C, index);
        toku_push_table(C, 0);
        toku_push(C, -1); /* copy will be left on the top */
        toku_set_fieldstr(C, index, field); /* table[field] = newtable */
        return 0; /* false, no table was found */
    }
}


CSLIB_API void csL_importf(toku_State *T, const char *modname,
                           toku_CFunction openf, int global) {
    csL_get_subtable(C, TOKU_CTABLE_INDEX, TOKU_LOADED_TABLE);
    toku_get_fieldstr(C, -1, modname); /* get __LOADED[modname] */
    if (!toku_to_bool(C, -1)) { /* package not already loaded? */
        toku_pop(C, 1); /* remove field */
        toku_push_cfunction(C, openf); /* push func that opens the module */
        toku_push_string(C, modname); /* argument to 'openf' */
        toku_call(C, 1, 1); /* call 'openf' */
        toku_push(C, -1);  /* make copy of the module (call result) */
        toku_set_fieldstr(C, -3, modname); /* __LOADED[modname] = module */
    }
    toku_remove(C, -2); /* remove __LOADED table */
    if (global) { /* set the module as global? */
        toku_push(C, -1); /* copy of module */
        toku_set_global(C, modname); /* __G[modname] = module */
    }
}


/* find and return last call frame level */
static int lastlevel(toku_State *T) {
    toku_Debug ar;
    int low = 0, high = 0;
    /* get upper bound, and store last known valid level in 'low' */
    while (toku_getstack(C, high, &ar)) {
        low = high;
        high += (high == 0); /* avoid multiplying by 0 */
        high *= 2;
    }
    /* binary search between 'low' and 'high' levels */
    while (low < high) {
        int mid = low + ((high - low)/2);
        if (toku_getstack(C, mid, &ar))
            low = mid + 1;
        else
            high = mid;
    }
    return low - 1;
}


static void push_funt_name(toku_State *T, toku_Debug *ar) {
    if (*ar->namewhat != '\0') /* name from code? */
        toku_push_fstring(C, "%s '%s'", ar->namewhat, ar->name);
    else if (*ar->what == 'm') /* main? */
        toku_push_literal(C, "main chunk");
    else if (push_glbfunt_name(C, ar)) { /* try global name */
        toku_push_fstring(C, "function '%s'", toku_to_string(C, -1));
        toku_remove(C, -2); /* remove name */
    } else if (*ar->what != 'C') /* for Tokudae functions, use <file:line> */
        toku_push_fstring(C, "function <%s:%d>", ar->shortsrc, ar->defline);
    else /* unknown */
        toku_push_literal(C, "?");
}


#define STACKLEVELS     10

CSLIB_API void csL_traceback(toku_State *T, toku_State *T1,
                             int level, const char *msg) {
    csL_Buffer B;
    toku_Debug ar;
    int last = lastlevel(C1);
    int limit2show = (last - level > (STACKLEVELS * 2) ? STACKLEVELS : -1);
    csL_buff_init(C, &B);
    if (msg) {
        csL_buff_push_string(&B, msg);
        csL_buff_push(&B, '\n');
    }
    csL_buff_push_string(&B, "stack traceback:");
    while (toku_getstack(C1, level++, &ar)) { /* tracing back... */
        if (limit2show-- == 0) { /* too many levels? */
            int n = last - level - STACKLEVELS + 1; /* levels to skip */
            toku_push_fstring(C, "\n\t...\t(skipping %d levels)", n);
            csL_buff_push_stack(&B);
            level += n; /* skip to last levels */
        } else {
            toku_getinfo(C1, "snl", &ar); /* source, name, line info */
            if (ar.currline <= 0)
                toku_push_fstring(C, "\n\t%s in ", ar.shortsrc);
            else
                toku_push_fstring(C, "\n\t%s:%d: in ", ar.shortsrc, ar.currline);
            csL_buff_push_stack(&B);
            push_funt_name(C, &ar);
            csL_buff_push_stack(&B);
        }
    }
    csL_buff_end(&B);
}


CSLIB_API void csL_set_funcs(toku_State *T, const csL_Entry *l, int nup) {
    csL_check_stack(C, nup, "too many upvalues");
    for (; l->name != NULL; l++) {
        if (l->func == NULL) { /* placeholder? */
            toku_push_bool(C, 0);
        } else { /* otherwise a function */
            for (int i = 0; i < nup; i++) /* copy upvalues */
                toku_push(C, -nup);
            toku_push_cclosure(C, l->func, nup); /* create closure */
        }
        toku_set_fieldstr(C, -(nup + 2), l->name);
    }
    toku_pop(C, nup); /* remove upvalues */
}


CSLIB_API void csL_check_version_(toku_State *T, toku_Number ver) {
    toku_Number v = toku_version(C);
    if (v != ver)
        csL_error(C,
            "version mismatch: application needs %f, Tokudae core provides %f",
            ver, v);
}


CSLIB_API unsigned csL_makeseed(toku_State *T) {
    (void)(C); /* unused */
    return csi_makeseed();
}


/* ------------------------------------------------------------------------
** Reference System
** ------------------------------------------------------------------------ */

/* index of free-list header (after the predefined values) */
#define freelist    (TOKU_CLIST_LAST + 1)

CSLIB_API int csL_ref(toku_State *T, int a) {
    int ref;
    if (toku_is_nil(C, -1)) { /* value on top is 'nil'? */
        toku_pop(C, 1); /* remove it from the stack */
        return TOKU_REFNIL;
    }
    a = toku_absindex(C, a);
    if (toku_get_index(C, a, freelist) == TOKU_T_NIL) { /* first access? */
        ref = 0; /* list is empty */
        toku_push_integer(C, 0); /* initialize empty list */
        toku_set_index(C, a, freelist); /* ref = a[freelist] = 0 */
    } else { /* already initialized */
        toku_assert(toku_is_integer(C, -1));
        ref = (int)toku_to_integer(C, -1); /* ref = a[freelist] */
    }
    toku_pop(C, 1); /* remove element */
    if (ref != 0) { /* any free element? */
        toku_get_index(C, a, ref); /* remove it from list */
        toku_set_index(C, a, freelist); /* (a[freelist] = a[ref]) */
    } else { /* no free elements */
        /* get a new ref */
        ref = (int)toku_find_index(C, a, TOKU_FI_NIL, freelist+1, 0);
    }
    toku_set_index(C, a, ref);
    return ref;
}


CSLIB_API void csL_unref(toku_State *T, int a, int ref) {
    if (ref >= 0) {
        a = toku_absindex(C, a);
        toku_get_index(C, a, freelist);
        toku_assert(toku_is_integer(C, -1));
        toku_set_index(C, a, ref); /* a[ref] = a[freelist] */
        toku_push_integer(C, ref);
        toku_set_index(C, a, freelist); /* a[freelist] = ref */
    }
}


/* ------------------------------------------------------------------------
** Buffering
** ------------------------------------------------------------------------ */

typedef struct UserBox {
    void *p; /* data */
    size_t sz; /* size of 'p' (data) */
} UserBox;


static void *resizebox(toku_State *T, int index, size_t newsz) {
    void *ud;
    toku_Alloc falloc = toku_getallocf(C, &ud);
    UserBox *box = (UserBox *)toku_to_userdata(C, index);
    void *newblock = falloc(box->p, box->sz, newsz, ud);
    if (t_unlikely(newblock == NULL && newsz > 0)) {
        toku_push_literal(C, "out of memory");
        toku_error(C);
    }
    box->p = newblock;
    box->sz = newsz;
    return newblock;
}


static int boxgc(toku_State *T) {
    resizebox(C, 0, 0);
    return 0;
}


static void newbox(toku_State *T) {
    UserBox *box = toku_push_userdata(C, sizeof(*box), 0);
    box->p = NULL;
    box->sz = 0;
    toku_push_list(C, TOKU_MT_NUM);
    toku_push_cfunction(C, boxgc);
    toku_set_index(C, -2, TOKU_MT_GC);
    toku_push_cfunction(C, boxgc);
    toku_set_index(C, -2, TOKU_MT_CLOSE);
    toku_set_metalist(C, -2);
}


/*
** Initializes the buffer 'B' and pushes it's placeholder onto
** the top of the stack as light userdata.
*/
CSLIB_API void csL_buff_init(toku_State *T, csL_Buffer *B) {
    B->C = C;
    B->n = 0;
    B->b = B->init.b;
    B->sz = CSL_BUFFERSIZE;
    toku_push_lightuserdata(C, B);
}


/* 
** Test whether the buffer is using a temporary arena on stack.
*/
#define buffonstack(B)      ((B)->b != (B)->init.b)


/*
** Whenever buffer is accessed, slot 'index' must be either a box,
** meaning the buffer is stored as 'UserBox' (which cannot be NULL)
** or it is a placeholder for the buffer (meaning that buffer is
** still using 'init.b[CSL_BUFFERSIZE]').
*/
#define checkbufflevel(B, index) \
        toku_assert(buffonstack(B) ? toku_to_userdata((B)->C, index) != NULL \
                                 : toku_to_userdata((B)->C, index) == (void*)(B))


/* calculate new buffer size */
static size_t newbuffsize(csL_Buffer *B, size_t sz) {
    size_t newsize = (B->sz / 2) * 3; /* 1.5x size */
    if (t_unlikely(SIZE_MAX - sz < B->n)) /* would overflow? */
        return csL_error(B->C, "buffer too large");
    if (newsize < B->n + sz)
        newsize = B->n + sz;
    return newsize;
}


/*
** Ensure that buffer 'B' can fit 'sz' bytes.
** This also creates 'UserBox' if internal buffer is not big enough.
*/
static char *buffensure(csL_Buffer *B, size_t sz, int boxindex) {
    checkbufflevel(B, boxindex);
    if (B->sz - B->n >= sz) { /* have enough space? */
        return B->b + B->n;
    } else { /* otherwise expand */
        char *newb;
        toku_State *T = B->C;
        size_t newsize = newbuffsize(B, sz);
        if (buffonstack(B)) { /* already have 'UserBox'? */
            newb = resizebox(C, boxindex, newsize); /* resize it and done */
        } else {
            toku_remove(C, boxindex); /* remove placeholder */
            newbox(C); /* create new user box on top */
            toku_insert(C, boxindex); /* insert the new box into 'boxindex' */
            toku_toclose(C, boxindex); /* mark box to-be-closed */
            newb = resizebox(C, boxindex, newsize); /* resize it */
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
CSLIB_API char *csL_buff_initsz(toku_State *T, csL_Buffer *B, size_t sz) {
    csL_buff_init(C, B);
    return buffensure(B, sz, -1);
}


/*
** Return the pointer to the free memory block of at least 'sz' bytes.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
CSLIB_API char *csL_buff_ensure(csL_Buffer *B, size_t sz) {
    return buffensure(B, sz, -1);
}


/*
** Push sized string into the buffer.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
CSLIB_API void csL_buff_push_lstring(csL_Buffer *B, const char *s, size_t l) {
    if (l > 0) {
        char *p = buffensure(B, l, -1);
        memcpy(p, s, l*sizeof(char));
        csL_buffadd(B, l);
    }
}


/*
** Similar to 'csL_buff_push_lstring', the only difference is that this
** function measures the length of 's'.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
CSLIB_API void csL_buff_push_string(csL_Buffer *B, const char *s) {
    csL_buff_push_lstring(B, s, strlen(s));
}


/*
** Pushes the string value on top of the stack into the buffer.
** This function expects buffer placeholder or its 'UserBox' to be on
** the stack below the string value being pushed, which is on top of the stack.
*/
CSLIB_API void csL_buff_push_stack(csL_Buffer *B) {
    size_t len;
    const char *str = toku_to_lstring(B->C, -1, &len);
    char *p = buffensure(B, len, -2);
    memcpy(p, str, len);
    csL_buffadd(B, len);
    toku_pop(B->C, 1); /* remove string */
}


CSLIB_API void csL_buff_push_gsub(csL_Buffer *B, const char *s, const char *p,
                                  const char *r) {
    const char *wild;
    size_t l = strlen(p);
    while ((wild = strstr(s, p)) != NULL) {
        csL_buff_push_lstring(B, s, wild - s); /* push prefix */
        csL_buff_push_string(B, r); /* push replacement in place of pattern */
        s = wild + l; /* continue after 'p' */
    }
    csL_buff_push_string(B, s); /* push last suffix */
}


CSLIB_API const char *csL_gsub(toku_State *T, const char *s, const char *p,
                               const char *r) {
    csL_Buffer B;
    csL_buff_init(C, &B);
    csL_buff_push_gsub(&B, s, p, r);
    csL_buff_end(&B);
    return toku_to_string(C, -1);
}


/*
** Finish the use of buffer 'B' leaving the final string on top of
** the stack.
*/
CSLIB_API void csL_buff_end(csL_Buffer *B) {
    toku_State *T = B->C;
    checkbufflevel(B, -1);
    toku_push_lstring(C, B->b, B->n);
    if (buffonstack(B)) /* have 'UserBox'? */
        toku_closeslot(C, -2); /* close it -> boxgc */
    toku_remove(C, -2);
}


CSLIB_API void csL_buff_endsz(csL_Buffer *B, size_t sz) {
    csL_buffadd(B, sz);
    csL_buff_end(B);
}
