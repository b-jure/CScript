/*
** cscriptaux.c
** Auxiliary library
** See Copyright Notice in cscript.h
*/

#define cscriptaux_c
#define CS_LIB

#include "cprefix.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "cscript.h"

#include "cscriptaux.h"
#include "climits.h"



CSLIB_API int csL_error(cs_State *C, const char *fmt, ...) {
    va_list ap;
    csL_where(C, 1);
    va_start(ap, fmt);
    cs_push_vfstring(C, fmt, ap);
    va_end(ap);
    cs_concat(C, 2);
    va_end(ap);
    return cs_error(C);
}


/*
** Find field in table at index -1. If field value is found
** meaning the object at `index` is equal to the field value, then
** this function returns 1. Only string keys are considered and
** limit is the limit of recursive calls in case table field
** contains another table value.
*/
static int findfield(cs_State *C, int index, int limit) {
    if (limit == 0 || !cs_is_table(C, -1))
        return 0; /* not found */
    cs_push_nil(C); /* start `next` loop (from beginning) */
    while (cs_next(C, -2)) { /* for each pair in the table */
        if (cs_type(C, -2) == CS_TSTRING) { /* ignore non-string keys */
            if (cs_rawequal(C, index, -1)) { /* found object (function)? */
                cs_pop(C, 1); /* remove value (but keep name) */
                return 1;
            } else if (findfield(C, index, limit - 1)) { /* try recursively */
                /* stack: lib_name, lib_table, field_name (top) */
                cs_push_literal(C, "."); /* place '.' between the two names */
                cs_replace(C, -3); /* (in the slot occupied by table) */
                cs_concat(C, 3); /* lib_name.field_name */
                return 1;
            }
        }
        cs_pop(C, 1); /* remove value */
    }
    return 0; /* not found */
}


/*
** Try and push name of the currently active function in `ar`.
** If function is global function, then the its name is pushed
** on the top of the stack.
*/
static int push_glbfunc_name(cs_State *C, cs_Debug *ar) {
    int top = cs_gettop(C); /* index of value on top of the stack */
    int func = top + 1;
    cs_getinfo(C, "f", ar); /* push function (top + 1) */
    cs_get_rtable(C, CS_LOADED_TABLE);
    csL_check_stack(C, 6, "not enough stack space"); /* for `findfield` */
    if (findfield(C, func, 2)) { /* found? */
        const char *name = cs_to_string(C, -1);
        if (strncmp(name, CS_GNAME ".", 4) == 0) { /* starts with '__G.'? */
            cs_push_string(C, name + 4); /* push name without prefix */
            cs_remove(C, -2); /* remove original name */
        }
        cs_copy(C, -1, func); /* copy name to proper place */
        cs_settop(C, func + 1); /* remove "loaded" table and name copy */
        return 1;
    } else { /* not a global */
        cs_settop(C, func); /* remove func and Lib */
        return 0;
    }
}


CSLIB_API int csL_error_arg(cs_State *C, int arg, const char *extra) {
    cs_Debug ar;
    if (!cs_getstack(C, 0, &ar)) /* no stack frame? */
        return csL_error(C, "bad argument #%d (%s)", arg, extra);
    cs_getinfo(C, "n", &ar);
    if (strcmp(ar.namewhat, "method") == 0) {
        arg--; /* ignore 'self' */
        if (arg == -1) /* 'self' is the invalid argument? */
            csL_error(C, "calling '%s' on a bad 'self' (%s)", ar.name, extra);
    }
    if (ar.name == NULL)
        ar.name = (push_glbfunc_name(C, &ar)) ? cs_to_string(C, -1) : "?";
    return csL_error(C, "bad argument #%d to '%s' (%s)", arg, ar.name, extra);
}


CSLIB_API int csL_error_type(cs_State *C, int arg, const char *tname) {
    const char *msg, *type;
    if (cs_type(C, arg) == CS_TLIGHTUSERDATA)
        type = "light userdata";
    else
        type = csL_typename(C, arg);
    msg = cs_push_fstring(C, "%s expected, instead got %s", tname, type);
    return csL_error_arg(C, arg, msg);
}


static void terror(cs_State *C, int arg, int t) {
    csL_error_type(C, arg, cs_typename(C, t));
}


CSLIB_API cs_Number csL_check_number(cs_State *C, int index) {
    int isnum;
    cs_Number n = cs_to_numberx(C, index, &isnum);
    if (c_unlikely(!isnum))
        terror(C, index, CS_TNUMBER);
    return n;
}


static void interror(cs_State *C, int argindex) {
    if (cs_is_number(C, argindex))
        csL_error_arg(C, argindex, "number has no integer representation");
    else
        terror(C, argindex, CS_TNUMBER);
}


CSLIB_API cs_Integer csL_check_integer(cs_State *C, int index) {
    int isint;
    cs_Integer i = cs_to_integerx(C, index, &isint);
    if (c_unlikely(!isint))
        interror(C, index);
    return i;
}


// TODO: add docs
CSLIB_API int csL_check_bool(cs_State *C, int index) {
    int isbool;
    int i = cs_to_boolx(C, index, &isbool);
    if (c_unlikely(!isbool))
        terror(C, index, CS_TBOOL);
    return i;
}


CSLIB_API const char *csL_check_lstring(cs_State *C, int index, size_t *len) {
    const char *str = cs_to_lstring(C, index, len);
    if (c_unlikely(str == NULL))
        terror(C, index, CS_TSTRING);
    return str;
}


CSLIB_API void *csL_check_userdata(cs_State *C, int index, const char *name) {
    void *p = csL_test_userdata(C, index, name);
    if (c_unlikely(p == NULL))
        csL_error_type(C, index, name);
    return p;
}


CSLIB_API void csL_check_stack(cs_State *C, int sz, const char *msg) {
    if (c_unlikely(!cs_checkstack(C, sz))) {
        if (msg)
            csL_error(C, "stack overflow (%s)", msg);
        else
            csL_error(C, "stack overflow");
    }
}


CSLIB_API void csL_check_type(cs_State *C, int arg, int t) {
    if (c_unlikely(cs_type(C, arg) != t))
        terror(C, arg, t);
}


CSLIB_API void csL_check_any(cs_State *C, int arg) {
    if (c_unlikely(cs_type(C, arg) == CS_TNONE))
        csL_error_arg(C, arg, "value expected");
}


CSLIB_API int csL_check_option(cs_State *C, int arg, const char *dfl,
                               const char *const opts[]) {
    const char *str = dfl ? csL_opt_string(C, arg, dfl)
                          : csL_check_string(C, arg);
    for (int i=0; opts[i]; i++)
        if (strcmp(str, opts[i]) == 0)
            return i;
    return csL_error_arg(C, arg,
                         cs_push_fstring(C, "invalid option `%s`", str));
}


CSLIB_API cs_Number csL_opt_number(cs_State *C, int index, cs_Number dfl) {
    return csL_opt(C, csL_check_number, index, dfl);
}


CSLIB_API cs_Integer csL_opt_integer(cs_State *C, int index, cs_Integer dfl) {
    return csL_opt(C, csL_check_integer, index, dfl);
}


// TODO: add docs
CSLIB_API int csL_opt_bool(cs_State *C, int index, int dfl) {
    return csL_opt(C, csL_check_bool, index, dfl);
}


CSLIB_API const char *csL_opt_lstring(cs_State *C, int index, const char *dfl,
                                      size_t *plen) {
    if (cs_is_noneornil(C, index)) {
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


static const char *filereader(cs_State *C, void *data, size_t *szread) {
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


static int errorfile(cs_State *C, const char *what, int filename_index) {
    int err = errno;
    const char *filename = cs_to_string(C, filename_index);
    if (err != 0)
        cs_push_fstring(C, "cannot %s %s: %s", what, filename, strerror(err)); 
    else
        cs_push_fstring(C, "cannot %s %s", what, filename);
    cs_remove(C, filename_index);
    return CS_ERRFILE;
}


CSLIB_API int csL_loadfile(cs_State *C, const char *filename) {
    LoadFile lf = {0};
    int status, readstatus;
    int filename_index = cs_getntop(C);
    errno = 0;
    if (filename == NULL) { /* stdin? */
        cs_push_string(C, "stdin");
        lf.fp = stdin;
    } else { /* otherwise real file */
        cs_push_string(C, filename);
        lf.fp = fopen(filename, "r");
        if (lf.fp == NULL)
            return errorfile(C, "open", filename_index);
    }
    status = cs_load(C, filereader, &lf, cs_to_string(C, -1));
    readstatus = ferror(lf.fp);
    if (filename) /* real file ? */
        fclose(lf.fp); /* close it */
    if (readstatus) { /* error while reading */
        cs_settop(C, filename_index); /* remove any results */
        return errorfile(C, "read", filename_index);
    }
    cs_remove(C, filename_index);
    return status;
}


typedef struct LoadString {
    const char *str;
    size_t sz;
} LoadString;


static const char *stringreader(cs_State *C, void *data, size_t *szread) {
    LoadString *ls = (LoadString *)data;
    (void)C; /* unused */
    if (ls->sz == 0) return NULL;
    *szread = ls->sz;
    ls->sz = 0;
    return ls->str;
}


CSLIB_API int csL_loadstring(cs_State *C, const char *s) {
    return csL_loadbuffer(C, s, strlen(s), s);
}


CSLIB_API int csL_loadbuffer(cs_State *C, const char *buff, size_t sz,
                             const char *name) {
    LoadString ls;
    ls.sz = sz;
    ls.str = buff;
    return cs_load(C, stringreader, &ls, name);
}


CSLIB_API int csL_new_metalist(cs_State *C, const char *lname) {
    if (csL_get_metalist(C, lname) != CS_TNIL) /* name already in use? */
        return 0; /* false; metalist already exists */
    cs_pop(C, 1); /* remove nil */
    cs_push_metalist(C); /* create metalist */
    cs_push(C, -1); /* push metalist copy */
    cs_set_rtable(C, lname); /* registrytable.lname = metalist */
    return 1; /* true; created new metalist */
}


CSLIB_API int csL_set_metalist(cs_State *C, const char *lname) {
    csL_get_metalist(C, lname);
    return cs_set_metalist(C, -2);
}


CSLIB_API int csL_get_metaindex(cs_State *C, int index, int mm) {
    if (!cs_get_metalist(C, index)) {
        return CS_TNONE;
    } else {
        int t = cs_get_index(C, -1, mm);
        if (t == CS_TNIL) {
            cs_pop(C, 2); /* remove metalist and nil */
            t = CS_TNONE;
        } else
            cs_remove(C, -2); /* remove only metalist */
        return t; /* return metalist index value type */
    }
}


CSLIB_API int csL_callmeta(cs_State *C, int index, int mm) {
    index = cs_absindex(C, index);
    if (csL_get_metaindex(C, index, mm) == CS_TNONE)
        return 0;
    cs_push(C, index);
    cs_call(C, 1, 1);
    return 1;
}


CSLIB_API int csL_new_usermethods(cs_State *C, const char *tname, int sz) {
    if (csL_get_methods(C, tname) != CS_TNIL) /* name already in use? */
        return 0; /* false; methods table already exists */
    cs_pop(C, 1); /* remove nil */
    cs_push_table(C, sz); /* create methods table */
    cs_push(C, -1); /* push table copy */
    cs_set_rtable(C, tname); /* registrytable.tname = table */
    return 1; /* true; created new methods table */
}


CSLIB_API void csL_set_usermethods(cs_State *C, const char *tname) {
    csL_get_methods(C, tname);
    cs_set_usermethods(C, -2);
}


#include "ctrace.h"
CSLIB_API void *csL_test_userdata(cs_State *C, int index, const char *lname) {
    void *p = csL_to_fulluserdata(C, index);
    if (p != NULL) { /* `index` is full userdata? */
        if (cs_get_metalist(C, 0)) { /* it has a metalist? */
            csL_get_metalist(C, lname); /* get correct metalist */
            if (!cs_rawequal(C, -1, -2)) /* not the same? */
                p = NULL;
            cs_pop(C, 2); /* remove both metalists */
            return p;
        }
    }
    return NULL; /* value is not a userdata with a metalist */
}


CSLIB_API void csL_set_metafuncs(cs_State *C, const csL_MetaEntry *l,
                                 int nup) {
    csL_check_stack(C, nup, "too many upvalues");
    for (; l->mm >= 0; l++) { /* for each metamethod */
        if (l->metaf != NULL) { /* have function? */
            for (int i = 0; i < nup; i++) /* copy upvalues */
                cs_push(C, -nup);
            cs_push_cclosure(C, l->metaf, nup); /* create closure */
        } else /* otherwise set as nil */
            cs_push_nil(C);
        cs_set_index(C, -(nup + 2), l->mm);
    }
    cs_pop(C, nup);
}


CSLIB_API int csL_fileresult(cs_State *C, int ok, const char *fname) {
    int err = errno;
    if (ok) { /* ok? */
        cs_push_bool(C, 1);
        return 1; /* return true */
    } else {
        const char *msg = (err != 0) ? strerror(err) : "(no extra info)";
        csL_push_fail(C);
        if (fname) /* have file name? */
            cs_push_fstring(C, "%s: %s", fname, msg);
        else
            cs_push_string(C, msg);
        cs_push_integer(C, err);
        return 3; /* return fail, msg, and error code */
    }
}


#if !defined(c_inspectstat)	/* { */

#if defined(CS_USE_POSIX)

#include <sys/wait.h>

/* use appropriate macros to interpret 'pclose' return status */
#define c_inspectstat(stat,what)  \
    if (WIFEXITED(stat)) { stat = WEXITSTATUS(stat); } \
    else if (WIFSIGNALED(stat)) { stat = WTERMSIG(stat); what = "signal"; }

#else

#define c_inspectstat(stat,what)  /* no op */

#endif

#endif				/* } */


CSLIB_API int csL_execresult(cs_State *C, int stat) {
    if (stat != 0 && errno != 0) /* error with an 'errno'? */
        return csL_fileresult(C, 0, NULL);
    else {
        const char *what = "exit"; /* type of termination */
        c_inspectstat(stat, what); /* interpret result */
        if (*what == 'e' && stat == 0) /* successful termination? */
            cs_push_bool(C, 1);
        else
            csL_push_fail(C);
        cs_push_string(C, what);
        cs_push_integer(C, stat);
        return 3; /* return true/fail, what and code */
    }
}


CSLIB_API const char *csL_to_lstring(cs_State *C, int index, size_t *plen) {
    index = cs_absindex(C, index);
    if (csL_callmeta(C, index, CS_MM_TOSTRING)) {
        if (!cs_is_string(C, -1))
            csL_error(C, "'__tostring' must return a string");
    } else {
        switch (cs_type(C, index)) {
            case CS_TNIL: {
                cs_push_literal(C, "nil");
                break;
            }
            case CS_TBOOL: {
                cs_push_string(C, (cs_to_bool(C, index) ? "true" : "false"));
                break;
            }
            case CS_TNUMBER: {
                if (cs_is_integer(C, index))
                    cs_push_fstring(C, "%I", cs_to_integer(C, index));
                else
                    cs_push_fstring(C, "%f", cs_to_number(C, index));
                break;
            }
            case CS_TSTRING: {
                cs_push(C, index);
                break;
            }
            default: {
                const char *kind = csL_typename(C, index);
                cs_push_fstring(C, "%s: %p", kind, cs_to_pointer(C, index));
                break;
            }
        }
    }
    return cs_to_lstring(C, -1, plen);
}


CSLIB_API void *csL_to_fulluserdata(cs_State *C, int index) {
    return cs_is_fulluserdata(C, index) ? cs_to_userdata(C, index) : NULL;
}


CSLIB_API void *csL_to_lightuserdata(cs_State *C, int index) {
    return cs_is_lightuserdata(C, index) ? cs_to_userdata(C, index) : NULL;
}


CSLIB_API void csL_where(cs_State *C, int level) {
    cs_Debug ar;
    if (cs_getstack(C, level, &ar)) {
        cs_getinfo(C, "sl", &ar);
        if (ar.currline > 0) { /* have info? */
            cs_push_fstring(C, "%s:%d: ", ar.shortsrc, ar.currline);
            return;
        }
    }
    cs_push_literal(C, "");
}


CSLIB_API int csL_get_property(cs_State *C, int index) {
    if (cs_get_field(C, index) == CS_TNIL) {
        cs_pop(C, 1); /* remove field */
        cs_get_class(C, index);
        cs_get_method(C, index);
    }
    return cs_type(C, -1);
}


CSLIB_API void csL_set_index(cs_State *C, int index, int i) {
    if (c_unlikely(i < 0 || MAXLISTINDEX < i))
        csL_error(C, "list index out of bounds (%d)", i);
    cs_set_index(C, index, i);
}


static void *allocator(void *ptr, size_t osz, size_t nsz, void *ud) {
    (void)osz; (void)ud; /* unused */
    if (nsz == 0) {
        free(ptr);
        return NULL;
    } else
        return realloc(ptr, nsz);
}


static int panic(cs_State *C) {
    const char *msg = (cs_type(C, -1) == CS_TSTRING
                       ? cs_to_string(C, -1)
                       : "error object is not a string");
    cs_writefmt(stderr, "PANIC: unprotected error in call to CScript API: %s\n",
                         msg);
    return 0; /* return to abort */
}


static void fwarnon(void *ud, const char *msg, int tocont);
static void fwarnoff(void *ud, const char *msg, int tocont);


static int warning_checkmessage(cs_State *C, const char *msg, int tocont) {
    if (tocont || *msg++ != '@') /* not a control message? */
        return 0;
    else {
        if (strcmp(msg, "on") == 0) /* turn warnings on */
            cs_setwarnf(C, fwarnon, C);
        else if (strcmp(msg, "off") == 0) /* turn warnings off */
            cs_setwarnf(C, fwarnoff, C);
        return 1; /* it was a control message */
    }
}


static void fwarncont(void *ud, const char *msg, int tocont) {
    cs_State *C = (cs_State *)ud;
    cs_writefmt(stderr, "%s", msg);
    if (tocont) /* to be continued? */
        cs_setwarnf(C, fwarncont, ud);
    else { /* this is the end of the warning */
        cs_writefmt(stderr, "%s", "\n");
        cs_setwarnf(C, fwarnon, C);
    }
}


static void fwarnon(void *ud, const char *msg, int tocont) {
    if (warning_checkmessage((cs_State *)ud, msg, tocont))
        return; /* it was a control message */
    cs_writefmt(stderr, "%s", "CScript warning: ");
    fwarncont(ud, msg, tocont);
}


static void fwarnoff(void *ud, const char *msg, int tocont) {
    warning_checkmessage((cs_State *)ud, msg, tocont);
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


CSLIB_API cs_State *csL_newstate(void) {
    cs_State *C = cs_newstate(allocator, NULL, csi_makeseed());
    if (c_likely(C)) {
        cs_atpanic(C, panic);
        cs_setwarnf(C, fwarnoff, C); /* warnings off by default */
    }
    return C;
}


CSLIB_API int csL_get_subtable(cs_State *C, int index, const char *field) {
    if (cs_get_fieldstr(C, index, field) == CS_TTABLE) {
        return 1; /* true, already have table */
    } else {
        cs_pop(C, 1); /* pop previous result */
        index = cs_absindex(C, index);
        cs_push_table(C, 0);
        cs_push(C, -1); /* copy will be left on the top */
        cs_set_fieldstr(C, index, field); /* table[field] = newtable */
        return 0; /* false, no table was found */
    }
}


CSLIB_API void csL_importf(cs_State *C, const char *modname,
                           cs_CFunction openf, int global) {
    csL_get_rsubtable(C, CS_LOADED_TABLE);
    cs_get_fieldstr(C, -1, modname); /* get __LOADED[modname] */
    if (!cs_to_bool(C, -1)) { /* package not already loaded? */
        cs_pop(C, 1); /* remove field */
        cs_push_cfunction(C, openf); /* push func that opens the module */
        cs_push_string(C, modname); /* argument to `openf` */
        cs_call(C, 1, 1); /* call `openf` */
        cs_push(C, -1);  /* make copy of the module (call result) */
        cs_set_fieldstr(C, -3, modname); /* __LOADED[modname] = module */
    }
    cs_remove(C, -2); /* remove __LOADED table */
    if (global) { /* set the module as global? */
        cs_push(C, -1); /* copy of module */
        cs_set_global(C, modname); /* __G[modname] = module */
    }
}


/* find and return last call frame level */
static int lastlevel(cs_State *C) {
    cs_Debug ar;
    int low = 0, high = 0;
    /* get upper bound, and store last known valid level in `low` */
    while (cs_getstack(C, high, &ar)) {
        low = high;
        high += (high == 0); /* avoid multiplying by 0 */
        high *= 2;
    }
    /* binary search between `low` and `high` levels */
    while (low < high) {
        int mid = low + ((high - low)/2);
        if (cs_getstack(C, mid, &ar))
            low = mid + 1;
        else
            high = mid;
    }
    return low - 1;
}


static void push_func_name(cs_State *C, cs_Debug *ar) {
    if (*ar->namewhat != '\0') { /* name from code? */
        cs_push_fstring(C, "%s `%s`", ar->namewhat, ar->name);
    } else if (*ar->what == 'm') { /* main? */
        cs_push_literal(C, "main chunk");
    } else if (*ar->what != 'C') { /* CScript function? */
        cs_push_fstring(C, "function <%s:%d>", ar->shortsrc, ar->defline);
    } else if (push_glbfunc_name(C, ar)) { /* try global name */
        cs_push_fstring(C, "function `%s`", cs_to_string(C, -1));
        cs_remove(C, -2); /* remove name */
    } else /* unknown */
        cs_push_literal(C, "?");
}


/* `0` is a valid stack level (top-level) */
#define tostacklevel(x)     ((x)-1)

#define STACKLEVELS         tostacklevel(11)

CSLIB_API void csL_traceback(cs_State *C, cs_State *C1, int level,
                             const char *msg) {
    csL_Buffer B;
    cs_Debug ar;
    int last = lastlevel(C1);
    int limit2show = (last - level > (STACKLEVELS * 2) ? STACKLEVELS : -1);
    csL_buff_init(C, &B);
    if (msg) {
        csL_buff_push_string(&B, msg);
        csL_buff_push(&B, '\n');
    }
    csL_buff_push_string(&B, "stack traceback:");
    while (cs_getstack(C1, level++, &ar)) { /* tracing back... */
        if (limit2show-- == 0) { /* too many levels? */
            int n = last - level - STACKLEVELS + 1; /* levels to skip */
            cs_push_fstring(C, "\n\t(skipping %d levels)", n);
            csL_buff_push_stack(&B);
            level += n; /* skip to last levels */
        } else {
            cs_getinfo(C1, "snl", &ar); /* source, name, line info */
            if (ar.currline <= 0)
                cs_push_fstring(C, "\n\t%s in ", ar.shortsrc);
            else
                cs_push_fstring(C, "\n\t%s:%d: in ", ar.shortsrc, ar.currline);
            csL_buff_push_stack(&B);
            push_func_name(C, &ar);
            csL_buff_push_stack(&B);
        }
    }
    csL_buff_end(&B);
}


CSLIB_API void csL_set_funcs(cs_State *C, const cs_Entry *l, int nup) {
    csL_check_stack(C, nup, "too many upvalues");
    for (; l->name != NULL; l++) {
        if (l->func == NULL) { /* placeholder? */
            cs_push_bool(C, 0);
        } else { /* otherwise a function */
            for (int i = 0; i < nup; i++) /* copy upvalues */
                cs_push(C, -nup);
            cs_push_cclosure(C, l->func, nup); /* create closure */
        }
        cs_set_fieldstr(C, -(nup + 2), l->name);
    }
    cs_pop(C, nup); /* remove upvalues */
}


CSLIB_API void csL_check_version_(cs_State *C, cs_Number ver) {
    cs_Number v = cs_version(C);
    if (v != ver)
        csL_error(C,
            "version mismatch: application needs %f, CScript core provides %f",
            ver, v);
}


CSLIB_API unsigned csL_makeseed(cs_State *C) {
    (void)(C); /* unused */
    return csi_makeseed();
}


/* ------------------------------------------------------------------------
** Reference System
** ------------------------------------------------------------------------ */

/* index of free-list header (after the predefined values) */
#define freelist    (CS_RINDEX_LAST + 1)

CSLIB_API int csL_ref(cs_State *C, int a) {
    int ref;
    if (cs_is_nil(C, -1)) { /* value on top is 'nil'? */
        cs_pop(C, 1); /* remove it from the stack */
        return CS_REFNIL;
    }
    a = cs_absindex(C, a);
    if (cs_get_index(C, a, freelist) == CS_TNIL) { /* first access? */
        ref = 0; /* list is empty */
        cs_push_integer(C, 0); /* initialize empty list */
        cs_set_index(C, a, freelist); /* ref = a[freelist] = 0 */
    } else { /* already initialized */
        cs_assert(cs_is_integer(C, -1));
        ref = (int)cs_to_integer(C, -1); /* ref = a[freelist] */
    }
    cs_pop(C, 1); /* remove element */
    if (ref != 0) { /* any free element? */
        cs_get_index(C, a, ref); /* remove it from list */
        cs_set_index(C, a, freelist); /* (a[freelist] = a[ref]) */
    } else /* no free elements */
        ref = (int)cs_find_nilindex(C, a, freelist+1, 0); /* get a new ref */
    cs_set_index(C, a, ref);
    return ref;
}


CSLIB_API void csL_unref(cs_State *C, int a, int ref) {
    if (ref >= 0) {
        a = cs_absindex(C, a);
        cs_get_index(C, a, freelist);
        cs_assert(cs_is_integer(C, -1));
        cs_set_index(C, a, ref); /* a[ref] = a[freelist] */
        cs_push_integer(C, ref);
        cs_set_index(C, a, freelist); /* a[freelist] = ref */
    }
}


/* ------------------------------------------------------------------------
** Buffering
** ------------------------------------------------------------------------ */

typedef struct UserBox {
    void *p; /* data */
    size_t sz; /* size of `p` (data) */
} UserBox;


static void *resizebox(cs_State *C, int index, size_t newsz) {
    void *ud;
    cs_Alloc falloc = cs_getallocf(C, &ud);
    UserBox *box = (UserBox *)cs_to_userdata(C, index);
    void *newblock = falloc(box->p, box->sz, newsz, ud);
    if (c_unlikely(newblock == NULL && newsz > 0)) {
        cs_push_literal(C, "out of memory");
        cs_error(C);
    }
    box->p = newblock;
    box->sz = newsz;
    return newblock;
}


static int boxgc(cs_State *C) {
    resizebox(C, 0, 0);
    return 0;
}


static void newbox(cs_State *C) {
    UserBox *box = cs_push_userdata(C, sizeof(*box), 0);
    box->p = NULL;
    box->sz = 0;
    cs_push_list(C, CS_MM_N);
    cs_push_cfunction(C, boxgc);
    cs_set_index(C, -2, CS_MM_GC);
    cs_push_cfunction(C, boxgc);
    cs_set_index(C, -2, CS_MM_CLOSE);
    cs_set_metalist(C, -2);
}


/*
** Initializes the buffer `B` and pushes it's placeholder onto
** the top of the stack as light userdata.
*/
CSLIB_API void csL_buff_init(cs_State *C, csL_Buffer *B) {
    B->C = C;
    B->n = 0;
    B->b = B->init.b;
    B->sz = CSL_BUFFERSIZE;
    cs_push_lightuserdata(C, B);
}


/* 
** Test whether the buffer is using a temporary arena on stack.
*/
#define buffonstack(B)      ((B)->b != (B)->init.b)


/*
** Whenever buffer is accessed, slot `index` must be either a box,
** meaning the buffer is stored as `UserBox` (which cannot be NULL)
** or it is a placeholder for the buffer (meaning that buffer is
** still using `init.b[CSL_BUFFERSIZE]`).
*/
#define checkbufflevel(B, index) \
        cs_assert(buffonstack(B) ? cs_to_userdata((B)->C, index) != NULL \
                                 : cs_to_userdata((B)->C, index) == (void*)(B))


/* calculate new buffer size */
static size_t newbuffsize(csL_Buffer *B, size_t sz) {
    size_t newsize = (B->sz / 2) * 3; /* 1.5x size */
    if (c_unlikely(SIZE_MAX - sz < B->n)) /* would overflow? */
        return csL_error(B->C, "buffer too large");
    if (newsize < B->n + sz)
        newsize = B->n + sz;
    return newsize;
}


/*
** Ensure that buffer `B` can fit `sz` bytes.
** This also creates `UserBox` if internal buffer is not big enough.
*/
static char *buffensure(csL_Buffer *B, size_t sz, int boxindex) {
    checkbufflevel(B, boxindex);
    if (B->sz - B->n >= sz) { /* have enough space? */
        return B->b + B->n;
    } else { /* otherwise expand */
        char *newb;
        cs_State *C = B->C;
        size_t newsize = newbuffsize(B, sz);
        if (buffonstack(B)) { /* already have `UserBox`? */
            newb = resizebox(C, boxindex, newsize); /* resize it and done */
        } else {
            cs_remove(C, boxindex); /* remove placeholder */
            newbox(C); /* create new user box on top */
            cs_insert(C, boxindex); /* insert the new box into `boxindex` */
            cs_toclose(C, boxindex); /* mark box to-be-closed */
            newb = resizebox(C, boxindex, newsize); /* resize it */
            memcpy(newb, B->b, B->n * sizeof(char)); /* copy `B->init.b` */
        }
        B->b = newb;
        B->sz = newsize;
        return newb + B->n;
    }
}


/*
** Initializes buffer `B` to the size `sz` bytes and returns the pointer
** to that memory block.
*/
CSLIB_API char *csL_buff_initsz(cs_State *C, csL_Buffer *B, size_t sz) {
    csL_buff_init(C, B);
    return buffensure(B, sz, -1);
}


/*
** Return the pointer to the free memory block of at least `sz` bytes.
** This function expects buffer placeholder or its `UserBox` to be on
** top of the stack.
*/
CSLIB_API char *csL_buff_ensure(csL_Buffer *B, size_t sz) {
    return buffensure(B, sz, -1);
}


/*
** Push sized string into the buffer.
** This function expects buffer placeholder or its `UserBox` to be on
** top of the stack.
*/
CSLIB_API void csL_buff_push_lstring(csL_Buffer *B, const char *s, size_t l) {
    if (l > 0) {
        char *b = buffensure(B, l, -1);
        memcpy(b, s, l*sizeof(char));
        csL_buffadd(B, l);
    }
}


/*
** Similar to `csL_buff_push_lstring`, the only difference is that this
** function measures the length of `s`.
** This function expects buffer placeholder or its `UserBox` to be on
** top of the stack.
*/
CSLIB_API void csL_buff_push_string(csL_Buffer *B, const char *s) {
    csL_buff_push_lstring(B, s, strlen(s));
}


/*
** Pushes the string value on top of the stack into the buffer.
** This function expects buffer placeholder or its `UserBox` to be on
** the stack below the string value being pushed, which is on top of the stack.
*/
CSLIB_API void csL_buff_push_stack(csL_Buffer *B) {
    size_t len;
    const char *str = cs_to_lstring(B->C, -1, &len);
    char *p = buffensure(B, len, -2);
    memcpy(p, str, len);
    csL_buffadd(B, len);
    cs_pop(B->C, 1); /* remove string */
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


CSLIB_API const char *csL_gsub(cs_State *C, const char *s, const char *p,
                               const char *r) {
    csL_Buffer B;
    csL_buff_init(C, &B);
    csL_buff_push_gsub(&B, s, p, r);
    csL_buff_end(&B);
    return cs_to_string(C, -1);
}


/*
** Finish the use of buffer `B` leaving the final string on top of
** the stack.
*/
CSLIB_API void csL_buff_end(csL_Buffer *B) {
    cs_State *C = B->C;
    checkbufflevel(B, -1);
    cs_push_lstring(C, B->b, B->n);
    if (buffonstack(B)) /* have `UserBox`? */
        cs_closeslot(C, -2); /* close it -> boxgc */
    cs_remove(C, -2);
}


CSLIB_API void csL_buff_endsz(csL_Buffer *B, size_t sz) {
    csL_buffadd(B, sz);
    csL_buff_end(B);
}
