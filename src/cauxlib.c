/*
** cauxlib.c
** Auxiliary library
** See Copyright Notice in cscript.h
*/


#define CS_LIB


#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "cauxlib.h"
#include "csconf.h"
#include "cscript.h"



CSLIB_API int csL_error(cs_State *ts, const char *fmt, ...) {
    va_list ap;
    csL_where(ts, 0);
    va_start(ap, fmt);
    cs_push_vfstring(ts, fmt, ap);
    va_end(ap);
    cs_concat(ts, 2);
    va_end(ap);
    return cs_error(ts);
}


CSLIB_API int csL_arg_error(cs_State *ts, int argindex, const char *extra) {
    cs_DebugInfo di;
    if (!cs_getstack(ts, 0, &di)) /* no stack frame? */
        return csL_error(ts, "bad argument @%d (%s)", argindex, extra);
    cs_getinfo(ts, "n", &di);
    if (strcmp(di.name, "method") == 0) {
        argindex--; /* ignore 'self' */
        if (argindex == 0) /* self is the invalid argument? */
            csL_error(ts, "calling '%s' on a bad 'self' (%s)", di.name, extra);
    }
    if (di.name == NULL)
        di.name = "?";
    return csL_error(ts, "bad argument @%d to '%s' (%s)",
                         argindex, di.name, extra);
}


CSLIB_API int csL_type_error(cs_State *ts, int argindex, const char *tname) {
    const char *msg;
    const char *argtype;
    if (cs_type(ts, argindex) == CS_TLIGHTUSERDATA)
        argtype = "light userdata";
    else
        argtype = csL_typename(ts, argindex);
    msg = cs_push_fstring(ts, "%s expected, got %s", tname, argtype);
    return csL_arg_error(ts, argindex, msg);
}


static void tterror(cs_State *ts, int argindex, int tt) {
    csL_type_error(ts, argindex, cs_typename(ts, tt));
}


CSLIB_API cs_Number csL_check_number(cs_State *ts, int index) {
    int isnum;
    cs_Number n = cs_to_numberx(ts, index, &isnum);
    if (csi_unlikely(!isnum))
        tterror(ts, index, CS_TNUMBER);
    return n;
}


static void interror(cs_State *ts, int argindex) {
    if (cs_is_number(ts, argindex))
        csL_arg_error(ts, argindex, "number has no integer representation");
    else
        tterror(ts, argindex, CS_TNUMBER);
}


CSLIB_API cs_Integer csL_check_integer(cs_State *ts, int index) {
    int isint;
    cs_Integer i = cs_to_integerx(ts, index, &isint);
    if (csi_unlikely(!isint))
        interror(ts, index);
    return i;
}


CSLIB_API const char *csL_check_lstring(cs_State *ts, int index, size_t *len) {
    const char *str = cs_to_lstring(ts, index, len);
    if (csi_unlikely(str == NULL))
        tterror(ts, index, CS_TSTRING);
    return str;
}


CSLIB_API void *csL_check_userdata(cs_State *ts, int index, const char *name) {
    void *p = csL_test_userdata(ts, index, name);
    if (csi_unlikely(p == NULL))
        csL_type_error(ts, index, name);
    return p;
}


CSLIB_API void csL_check_stack(cs_State *ts, int space, const char *msg) {
    if (csi_unlikely(!cs_checkstack(ts, space))) {
        if (msg)
            csL_error(ts, "stack overflow (%s)", msg);
        else
            csL_error(ts, "stack overflow");
    }
}


CSLIB_API void csL_check_type(cs_State *ts, int index, int tt) {
    if (csi_unlikely(cs_type(ts, index) != tt))
        tterror(ts, index, tt);
}


CSLIB_API void csL_check_any(cs_State *ts, int index) {
    if (csi_unlikely(cs_type(ts, index) == CS_TNONE))
        csL_arg_error(ts, index, "value expected");
}


CSLIB_API int csL_check_option(cs_State *ts, int index, const char *dfl,
                               const char *const opts[]) {
    const char *str = (dfl ? csL_opt_string(ts, index, dfl) :
                             csL_check_string(ts, index));
    int i;
    for (i=0; opts[i]; i++)
        if (strcmp(str, opts[i]) == 0)
            return i;
    return csL_arg_error(ts, index,
                         cs_push_fstring(ts, "invalid option '%s'", str));
}


CSLIB_API cs_Number csL_opt_number(cs_State *ts, int index, cs_Number dfl) {
    return csL_opt(ts, csL_check_number, index, dfl);
}


CSLIB_API cs_Integer csL_opt_integer(cs_State *ts, int index, cs_Integer dfl) {
    return csL_opt(ts, csL_check_integer, index, dfl);
}


CSLIB_API const char *csL_opt_lstring(cs_State *ts, int index, const char *dfl,
                                      size_t *plen) {
    if (cs_is_noneornil(ts, index)) {
        if (plen)
            *plen = (dfl ? strlen(dfl) : 0);
        return dfl;
    }
    return csL_check_lstring(ts, index, plen);
}



typedef struct LoadFile {
    int n; /* number of pre-read characters */
    FILE *fp; /* file being read */
    char buffer[BUFSIZ];
} LoadFile;


static const char *filereader(cs_State *ts, void *data, size_t *szread) {
    LoadFile *fr = (LoadFile *)data;
    (void)ts; /* unused */
    if (fr->n > 0) { /* have pre-read characters ? */
        *szread = fr->n;
        fr->n = 0;
    } else { /* read from a file */
        if (feof(fr->fp)) return NULL;
        *szread = fread(fr->buffer, 1, sizeof(fr->buffer), fr->fp);
    }
    return fr->buffer;
}


static int errorfile(cs_State *ts, const char *what, int filename_index) {
    int err = errno;
    const char *filename = cs_to_string(ts, filename_index);
    if (err != 0)
        cs_push_fstring(ts, "cannot %s %s: %s", what, filename, strerror(err)); 
    else
        cs_push_fstring(ts, "cannot %s %s", what, filename);
    cs_remove(ts, filename_index);
    return CS_ERRFILE;
}


CSLIB_API int csL_loadfile(cs_State *ts, const char *filename) {
    LoadFile lf;
    int status, readstatus;
    int filename_index = cs_gettop(ts) + 1;
    errno = 0;
    if (filename == NULL) { /* stdin? */
        cs_push_string(ts, "stdin");
        lf.fp = stdin;
    } else { /* otherwise real file */
        cs_push_string(ts, filename);
        lf.fp = fopen(filename, "r");
        if (lf.fp == NULL)
            return errorfile(ts, "open", filename_index);
    }
    status = cs_load(ts, filereader, &lf, cs_to_string(ts, -1));
    readstatus = ferror(lf.fp);
    if (filename) /* real file ? */
        fclose(lf.fp); /* close it */
    if (readstatus) { /* error while reading */
        cs_setntop(ts, filename_index); /* remove any results */
        return errorfile(ts, "read", filename_index);
    }
    cs_remove(ts, filename_index);
    return status;
}


typedef struct LoadString {
    const char *str;
    size_t sz;
} LoadString;


static const char *stringreader(cs_State *ts, void *data, size_t *szread) {
    LoadString *ls = (LoadString *)data;
    (void)ts; /* unused */
    if (ls->sz == 0)
        return NULL;
    *szread = ls->sz;
    ls->sz = 0;
    return ls->str;
}


CSLIB_API int csL_loadbuffer(cs_State *ts, const char *buff, size_t sz,
                             const char *name) {
    LoadString ls;
    ls.sz = sz;
    ls.str = buff;
    return cs_load(ts, stringreader, &ls, name);
}


CSLIB_API int csL_loadstring(cs_State *ts, const char *str) {
    return csL_loadbuffer(ts, str, strlen(str), str);
}


CSLIB_API const char *csL_to_lstring(cs_State *ts, int index, size_t *plen) {
    int tt;
    index = cs_absindex(ts, index);
    tt = cs_type(ts, index);
    switch (tt) {
        case CS_TNIL: {
            cs_push_literal(ts, "nil");
            break;
        }
        case CS_TBOOL: {
            cs_push_string(ts, (cs_to_bool(ts, index) ? "true" : "false"));
            break;
        }
        case CS_TNUMBER: {
            if (cs_is_integer(ts, index))
                cs_push_fstring(ts, CS_INTEGER_FMT, cs_to_integer(ts, index));
            else
                cs_push_fstring(ts, CS_FLOAT_FMT, cs_to_number(ts, index));
            break;
        }
        case CS_TSTRING: {
            cs_push(ts, index);
            break;
        }
        default: {
            const char *kind = cs_typename(ts, tt);
            cs_push_fstring(ts, "%s: %p", kind, cs_to_pointer(ts, index));
            break;
        }
    }
    return cs_to_lstring(ts, -1, plen);
}


CSLIB_API void csL_where(cs_State *ts, int level) {
    cs_DebugInfo di;
    if (cs_getstack(ts, level, &di)) {
        cs_getinfo(ts, "sl", &di);
        if (di.defline > 0) { /* have info? */
            cs_push_fstring(ts, "%s:%d", di.shortsrc, di.currline);
            return;
        }
    }
    cs_push_literal(ts, "");
}


CSLIB_API int csL_fileresult(cs_State *ts, int ok, const char *fname) {
    int err = errno;
    if (ok) { /* ok? */
        cs_push_bool(ts, 1);
        return CS_OK;
    } else {
        const char *msg;
        csL_push_fail(ts);
        msg = (err != 0 ? strerror(err) : "(no extra info)");
        if (fname) /* have file name? */
            cs_push_fstring(ts, "%s: %s", fname, msg);
        else
            cs_push_string(ts, msg);
        cs_push_integer(ts, err);
        return CS_ERRFILE;
    }
}


CSLIB_API int csL_get_property(cs_State *ts, int insobj) {
    if (cs_get_field(ts, insobj) == CS_TNIL) {
        cs_pop(ts, 1); /* remove nil */
        cs_get_class(ts, insobj);
        cs_get_method(ts, insobj);
    }
    return cs_type(ts, -1);
}


CSLIB_API void csL_set_cindex(cs_State *ts, int arrobj, cs_Integer i) {
    if (csi_unlikely(i < 0))
        csL_error(ts, "array index is negative (%I)", i);
    cs_set_index(ts, arrobj, i);
}


static void *allocator(void *ptr, size_t osz, size_t nsz, void *ud) {
    (void)osz; (void)ud; /* unused */
    if (nsz == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, nsz);
}


static int panic(cs_State *ts) {
    const char *msg = (cs_type(ts, -1) == CS_TSTRING
                       ? cs_to_string(ts, -1)
                       : "error object is not a string");
    cs_writefmt(stderr, "PANIC: unprotected error in call to CScript API: %s\n",
                         msg);
    return 0; /* return to abort */
}


static void fwarnon(void *ud, const char *msg, int tocont);
static void fwarnoff(void *ud, const char *msg, int tocont);


static int warning_checkmessage(cs_State *ts, const char *msg, int tocont) {
    if (tocont || *msg++ != '@') /* not a control message? */
        return 0;
    if (strcmp(msg, "on") == 0)
        cs_setwarnf(ts, fwarnon, ts);
    else if (strcmp(msg, "off"))
        cs_setwarnf(ts, fwarnoff, ts);
    return 1;
}


static void fwarncont(void *ud, const char *msg, int tocont) {
    cs_State *ts = (cs_State *)ud;
    cs_writefmt(stderr, "%s", msg);
    if (tocont) { /* to be continued? */
        cs_setwarnf(ts, fwarncont, ud);
    } else { /* this is the end of the warning */
        cs_writefmt(stderr, "%s", "\n");
        cs_setwarnf(ts, fwarnon, ts);
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


CSLIB_API cs_State *csL_newstate(void) {
    cs_State *ts = cs_newstate(allocator, NULL);
    if (csi_likely(ts)) {
        cs_atpanic(ts, panic);
        cs_setwarnf(ts, fwarnoff, ts); /* warnings off by default */
    }
    return ts;
}


CSLIB_API int csL_get_subtable(cs_State *ts, int htobj, const char *field) {
    if (cs_get_fieldstr(ts, htobj, field) == CS_THTABLE) {
        return 1; /* true, already have table */
    } else {
        cs_pop(ts, 1); /* pop previous result */
        htobj = cs_absindex(ts, htobj);
        cs_push_table(ts, 0);
        cs_push(ts, -1); /* copy will be left on the top */
        cs_set_fieldstr(ts, htobj, field); /* table[field] = newtable */
        return 0; /* false, no table was found */
    }
}


CSLIB_API void csL_include(cs_State *ts, const char *libname,
                           cs_CFunction openf, int global) {
    csL_get_subtable(ts, CS_REGISTRYINDEX, CS_LOADED_TABLE);
    cs_get_fieldstr(ts, -1, libname); /* get lib[libname] */
    if (!cs_to_bool(ts, -1)) { /* library not already loaded? */
        cs_pop(ts, 1); /* remove field */
        cs_push_cfunction(ts, openf); /* push func that opens the library */
        cs_push_string(ts, libname); /* argument to 'openf' */
        cs_call(ts, 1, 1); /* call 'openf' */
        cs_push(ts, -1); /* copy the library (call result) */
        cs_set_fieldstr(ts, -3, libname); /* lib[libname] = library */
    }
    cs_remove(ts, -2); /* remove 'lib' */
    if (global) { /* set the library as global? */
        cs_push(ts, -1); /* copy of library */
        cs_set_global(ts, libname); /* set it as global variable */
    }
}


CSLIB_API void *csL_test_userdata(cs_State *ts, int index, const char *name) {
    void *p = cs_to_userdata(ts, index);
    if (p != NULL) { /* 'index' is userdata? */
        cs_push(ts, index);
        cs_get_global(ts, name);
        if (!cs_rawequal(ts, -2, -1)) /* not the same as global? */
            p = NULL; /* value is a userdata but not a global 'name' */
        cs_pop(ts, 2); /* remove both values */
        return p;
    }
    return NULL; /* value is not a userdata */
}


/* find and return last call frame level */
static int lastlevel(cs_State *ts) {
    cs_DebugInfo di;
    int high = 0, low = 0;
    /* get top limit, and store last known valid in 'low' */
    while (cs_getstack(ts, high, &di)) { low = high; high *= 2; }
    /* binary search between 'low' and 'high' levels */
    while (low < high) {
        int mid = low + ((high - low)/2);
        if (cs_getstack(ts, mid, &di))
            low = mid + 1;
        else
            high = mid;
    }
    return low - 1;
}


/*
** Find field in table at index -1. If field value is found
** meaning the object at 'index' is equal to the field value, then
** this function returns 1. Only string keys are considered and
** limit is the limit of recursive calls in case table field
** contains another table value.
*/
static int findfield(cs_State *ts, int index, int limit) {
    if (limit == 0 || !cs_is_hashtable(ts, -1))
        return 0; /* not found */
    cs_push_nil(ts); /* start 'next' loop */
    while (cs_next(ts, -2)) { /* for each field in table */
        if (cs_type(ts, -2) == CS_TSTRING) { /* ignore non-string keys */
            if (cs_rawequal(ts, index, -1)) { /* found object? */
                cs_pop(ts, 1); /* remove value (but keep name) */
                return 1;
            } else if (findfield(ts, index, limit - 1)) { /* try recursively */
                /* stack: lib_name, lib_table, field_name (top) */
                cs_push_literal(ts, "."); /* place '.' between the two names */
                cs_replace(ts, -3); /* (in the slot occupied by table) */
                cs_concat(ts, 3); /* lib_name.field_name */
                return 1;
            }
        }
        cs_pop(ts, 1); /* remove value */
    }
    return 0; /* not found */
}


/*
** Try and push name of the currently active func in 'di'.
** If func is global function, then the its name is pushed on the top of
** the stack.
*/
static int pushglobalfuncname(cs_State *ts, cs_DebugInfo *di) {
    int top = cs_gettop(ts);
    cs_getinfo(ts, "f", di); /* push func */
    cs_get_global(ts, CS_LOADED_TABLE); /* get Lib */
    csL_check_stack(ts, 6, "not enough stack space"); /* for 'findfield' */
    if (findfield(ts, top + 1, 2)) { /* found? */
        cs_copy(ts, -1, top + 1); /* copy name to proper place */
        cs_setntop(ts, top + 1); /* remove Lib and copy of name */
        return 1;
    } else { /* not a global */
        cs_setntop(ts, top); /* remove func and Lib */
        return 0;
    }
}


static void pushfuncname(cs_State *ts, cs_DebugInfo *di) {
    if (pushglobalfuncname(ts, di)) { /* try first a global name */
        cs_push_fstring(ts, "function '%s'", cs_to_string(ts, -1));
        cs_remove(ts, -2); /* remove name */
    }
    else if (*di->namewhat != '\0') /* name from code? */
        cs_push_fstring(ts, "%s '%s'", di->namewhat, di->name);
    else if (*di->what == 'm') /* main? */
        cs_push_literal(ts, "main chunk");
    else if (*di->what != 'C') /* CScript functions? */
        cs_push_fstring(ts, "function <%s:%d>", di->shortsrc, di->defline);
    else /* unknown */
        cs_push_literal(ts, "?");
}


/* '0' is a valid stack level (top-level) */
#define tostacklevel(x)     ((x)-1)

#define STACKLEVELS         tostacklevel(11)

CSLIB_API void csL_traceback(cs_State *ts, cs_State *at, int level,
                             const char *msg) {
    csL_Buffer B;
    cs_DebugInfo di;
    int last = lastlevel(at);
    int levelsleft = (last - level > (STACKLEVELS*2) ? STACKLEVELS : -1);
    csL_buff_init(ts, &B);
    if (msg) {
        csL_buff_push_string(&B, msg);
        csL_buff_push(&B, '\n');
    }
    csL_buff_push_string(&B, "stack traceback:");
    while (cs_getstack(at, level++, &di)) { /* tracing back... */
        if (levelsleft-- == 0) { /* too many levels? */
            int n = last - level - STACKLEVELS + 1;
            cs_push_fstring(ts, "\n\t(skipping %d levels)", n);
            csL_buff_push_value(&B);
            level += n;
        } else {
            /* source, name, line info */
            cs_getinfo(at, "snl", &di);
            if (di.defline <= 0)
                cs_push_fstring(ts, "\n\t%s in ", di.shortsrc);
            else
                cs_push_fstring(ts, "\n\t%s:%d in ", di.shortsrc, di.currline);
            csL_buff_push_value(&B);
            pushfuncname(ts, &di);
            csL_buff_push_value(&B);
        }
    }
    csL_buff_end(&B);
}


CSLIB_API void csL_set_funcs(cs_State *ts, const cs_Entry *l, int nup) {
    csL_check_stack(ts, nup, "too many upvalues");
    for (; l->name != NULL; l++) {
        if (l->func == NULL) { /* placeholder? */
            cs_push_bool(ts, 0);
        } else {
            for (int i = 0; i < nup; i++) /* copy upvalues */
                cs_push(ts, -nup);
            cs_push_cclosure(ts, l->func, nup); /* create closure */
        }
        cs_set_fieldstr(ts, -(nup + 2), l->name);
    }
    cs_pop(ts, nup); /* remove upvalues */
}



/* ------------------------------------------------------------------------
** Buffering
** ------------------------------------------------------------------------ */

typedef struct UserBox {
    void *p; /* data */
    size_t sz; /* size of 'p' (data) */
} UserBox;


static void *resizebox(cs_State *ts, int index, size_t newsz) {
    void *ud;
    cs_Alloc falloc = cs_getallocf(ts, &ud);
    UserBox *box = (UserBox *)cs_to_userdata(ts, index);
    void *newblock = falloc(box->p, box->sz, newsz, ud);
    if (csi_unlikely(newblock == NULL && newsz > 0)) {
        cs_push_literal(ts, "out of memory");
        cs_error(ts);
    }
    box->p = newblock;
    box->sz = newsz;
    return newblock;
}


static int boxgc(cs_State *ts) {
    resizebox(ts, 0, 0);
    return 0;
}


static const cs_VMT boxvmt = {
    .func[CS_MM_GC] = boxgc,
    .func[CS_MM_CLOSE] = boxgc,
};


static void newbox(cs_State *ts) {
    UserBox *box = cs_newuserdata(ts, sizeof(*box), 0);
    box->p = NULL;
    box->sz = 0;
    cs_set_uservmt(ts, -1, &boxvmt);
}


/*
** Initializes the buffer 'B' and pushes it's placeholder onto
** the top of the stack as light userdata.
*/
CSLIB_API void csL_buff_init(cs_State *ts, csL_Buffer *B) {
    B->ts = ts;
    B->n = 0;
    B->b = B->init.b;
    B->sz = CSL_BUFFERSIZE;
    cs_push_lightuserdata(ts, B);
}


/* 
** Test whether the buffer is using a temporary arena on stack.
*/
#define buffonstack(B)      ((B)->b != (B)->init.b)


/*
** Whenever buffer is accessed, slot 'index' must be either a box,
** meaning the buffer is using a temporary arena on stack (which
** cannot be NULL) or it is a placeholder for the buffer (meaning
** that buffer is still using 'init.b[CSL_BUFFERSIZE]').
*/
#define checkbufflevel(B, index) \
    cs_assert(buffonstack(B) ? cs_to_userdata((B)->ts, index) != NULL \
                             : cs_to_userdata((B)->ts, index) == (void*)(B))


/* calculate new buffer size */
static size_t newbuffsize(csL_Buffer *B, size_t sz) {
    size_t newsize = (B->sz / 2) * 3; /* 1.5x size */
    if (csi_unlikely(SIZE_MAX - sz < B->n)) /* would overflow? */
        return csL_error(B->ts, "buffer too large");
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
        cs_State *ts = B->ts;
        size_t newsize = newbuffsize(B, sz);
        if (buffonstack(B)) { /* already have 'UserBox'? */
            newb = resizebox(ts, boxindex, newsize); /* resize it and done */
        } else {
            cs_remove(ts, boxindex); /* remove placeholder */
            newbox(ts); /* create new user box on top */
            cs_insert(ts, boxindex); /* insert the new box into 'boxindex' */
            cs_toclose(ts, boxindex); /* mark box to-be-closed */
            newb = resizebox(ts, boxindex, newsize); /* resize it */
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
CSLIB_API char *csL_buff_initsz(cs_State *ts, csL_Buffer *B, size_t sz) {
    csL_buff_init(ts, B);
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
        char *b = buffensure(B, l, -1);
        memcpy(b, s, l * sizeof(char));
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
CSLIB_API void csL_buff_push_value(csL_Buffer *B) {
    size_t len;
    const char *str = cs_to_lstring(B->ts, -1, &len);
    char *p = buffensure(B, len, -2);
    memcpy(p, str, len);
    csL_buffadd(B, len);
    cs_pop(B->ts, 1); /* remove string */
}


/*
** Finish the use of buffer 'B' leaving the final string on top of
** the stack.
*/
CSLIB_API void csL_buff_end(csL_Buffer *B) {
    cs_State *ts = B->ts;
    checkbufflevel(B, -1);
    cs_push_lstring(ts, B->b, B->n);
    if (buffonstack(B)) /* have 'UserBox'? */
        cs_closeslot(ts, -2); /* close it -> boxgc */
    cs_remove(ts, -2); /* remove buffer placeholder or box */
}
