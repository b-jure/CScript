/*
** cauxlib.c
** Auxiliary library
** See Copyright Notice in cscript.h
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "cauxlib.h"
#include "cconf.h"
#include "cscript.h"



CRLIB_API int crL_error(cr_State *ts, const char *fmt, ...) {
    va_list ap;
    crL_where(ts, 0);
    va_start(ap, fmt);
    cr_push_vfstring(ts, fmt, ap);
    va_end(ap);
    cr_concat(ts, 2);
    va_end(ap);
    return cr_error(ts);
}


CRLIB_API int crL_arg_error(cr_State *ts, int argindex, const char *extra) {
    cr_DebugInfo di;
    if (!cr_getstack(ts, 0, &di)) /* no stack frame? */
        return crL_error(ts, "bad argument @%d (%s)", argindex, extra);
    cr_getinfo(ts, "n", &di);
    if (strcmp(di.name, "method") == 0) {
        argindex--; /* ignore 'self' */
        if (argindex == 0) /* self is the invalid argument? */
            crL_error(ts, "calling '%s' on a bad 'self' (%s)", di.name, extra);
    }
    if (di.name == NULL)
        di.name = "?";
    return crL_error(ts, "bad argument @%d to '%s' (%s)",
                         argindex, di.name, extra);
}


CRLIB_API int crL_type_error(cr_State *ts, int argindex, const char *tname) {
    const char *msg;
    const char *argtype;
    if (cr_type(ts, argindex) == CR_TLUDATA)
        argtype = "light userdata";
    else
        argtype = crL_typename(ts, argindex);
    msg = cr_push_fstring(ts, "%s expected, got %s", tname, argtype);
    return crL_arg_error(ts, argindex, msg);
}


static void tterror(cr_State *ts, int argindex, int tt) {
    crL_type_error(ts, argindex, cr_typename(ts, tt));
}


CRLIB_API cr_Number crL_check_number(cr_State *ts, int index) {
    int isnum;
    cr_Number n = cr_to_numberx(ts, index, &isnum);
    if (cr_unlikely(!isnum))
        tterror(ts, index, CR_TNUMBER);
    return n;
}


static void interror(cr_State *ts, int argindex) {
    if (cr_is_number(ts, argindex))
        crL_arg_error(ts, argindex, "number has no integer representation");
    else
        tterror(ts, argindex, CR_TNUMBER);
}


CRLIB_API cr_Integer crL_check_integer(cr_State *ts, int index) {
    int isint;
    cr_Integer i = cr_to_integerx(ts, index, &isint);
    if (cr_unlikely(!isint))
        interror(ts, index);
    return i;
}


CRLIB_API const char *crL_check_lstring(cr_State *ts, int index, size_t *len) {
    const char *str = cr_to_lstring(ts, index, len);
    if (cr_unlikely(str == NULL))
        tterror(ts, index, CR_TSTRING);
    return str;
}


CRLIB_API void *crL_check_userdata(cr_State *ts, int index, const char *name) {
    void *p = crL_test_userdata(ts, index, name);
    if (cr_unlikely(p == NULL))
        crL_type_error(ts, index, name);
    return p;
}


CRLIB_API void crL_check_stack(cr_State *ts, int space, const char *msg) {
    if (cr_unlikely(!cr_checkstack(ts, space))) {
        if (msg)
            crL_error(ts, "stack overflow (%s)", msg);
        else
            crL_error(ts, "stack overflow");
    }
}


CRLIB_API void crL_check_type(cr_State *ts, int index, int tt) {
    if (cr_unlikely(cr_type(ts, index) != tt))
        tterror(ts, index, tt);
}


CRLIB_API void crL_check_any(cr_State *ts, int index) {
    if (cr_unlikely(cr_type(ts, index) == CR_TNONE))
        crL_arg_error(ts, index, "value expected");
}


CRLIB_API int crL_check_option(cr_State *ts, int index, const char *dfl,
                               const char *const opts[]) {
    const char *str = (dfl ? crL_opt_string(ts, index, dfl) :
                             crL_check_string(ts, index));
    int i;
    for (i=0; opts[i]; i++)
        if (strcmp(str, opts[i]) == 0)
            return i;
    return crL_arg_error(ts, index,
                         cr_push_fstring(ts, "invalid option '%s'", str));
}


CRLIB_API cr_Number crL_opt_number(cr_State *ts, int index, cr_Number dfl) {
    return crL_opt(ts, crL_check_number, index, dfl);
}


CRLIB_API cr_Integer crL_opt_integer(cr_State *ts, int index, cr_Integer dfl) {
    return crL_opt(ts, crL_check_integer, index, dfl);
}


CRLIB_API const char *crL_opt_lstring(cr_State *ts, int index, const char *dfl,
                                      size_t *plen) {
    if (cr_is_noneornil(ts, index)) {
        if (plen)
            *plen = (dfl ? strlen(dfl) : 0);
        return dfl;
    }
    return crL_check_lstring(ts, index, plen);
}



typedef struct LoadFile {
    int n; /* number of pre-read characters */
    FILE *fp; /* file being read */
    char buffer[BUFSIZ];
} LoadFile;


static const char *filereader(cr_State *ts, void *data, size_t *szread) {
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


static int errorfile(cr_State *ts, const char *what, int filename_index) {
    int err = errno;
    const char *filename = cr_to_string(ts, filename_index);
    if (err != 0)
        cr_push_fstring(ts, "cannot %s %s: %s", what, filename, strerror(err)); 
    else
        cr_push_fstring(ts, "cannot %s %s", what, filename);
    cr_remove(ts, filename_index);
    return CR_ERRFILE;
}


CRLIB_API int crL_loadfile(cr_State *ts, const char *filename) {
    LoadFile lf;
    int status, readstatus;
    int filename_index = cr_gettop(ts) + 1;
    errno = 0;
    if (filename == NULL) { /* stdin? */
        cr_push_string(ts, "stdin");
        lf.fp = stdin;
    } else { /* otherwise real file */
        cr_push_string(ts, filename);
        lf.fp = fopen(filename, "r");
        if (lf.fp == NULL)
            return errorfile(ts, "open", filename_index);
    }
    status = cr_load(ts, filereader, &lf, cr_to_string(ts, -1));
    readstatus = ferror(lf.fp);
    if (filename) /* real file ? */
        fclose(lf.fp); /* close it */
    if (readstatus) { /* error while reading */
        cr_settop(ts, filename_index); /* remove any results */
        return errorfile(ts, "read", filename_index);
    }
    cr_remove(ts, filename_index);
    return status;
}


typedef struct LoadString {
    const char *str;
    size_t sz;
} LoadString;


static const char *stringreader(cr_State *ts, void *data, size_t *szread) {
    LoadString *ls = (LoadString *)data;
    (void)ts; /* unused */
    if (ls->sz == 0)
        return NULL;
    *szread = ls->sz;
    ls->sz = 0;
    return ls->str;
}


CRLIB_API int crL_loadbuffer(cr_State *ts, const char *buff, size_t sz,
                             const char *name) {
    LoadString ls;
    ls.sz = sz;
    ls.str = buff;
    return cr_load(ts, stringreader, &ls, name);
}


CRLIB_API int crL_loadstring(cr_State *ts, const char *str) {
    return crL_loadbuffer(ts, str, strlen(str), str);
}


CRLIB_API const char *crL_to_lstring(cr_State *ts, int index, size_t *plen) {
    int tt;
    index = cr_absindex(ts, index);
    tt = cr_type(ts, index);
    switch (tt) {
        case CR_TNIL: {
            cr_push_literal(ts, "nil");
            break;
        }
        case CR_TBOOL: {
            cr_push_string(ts, (cr_to_bool(ts, index) ? "true" : "false"));
            break;
        }
        case CR_TNUMBER: {
            if (cr_is_integer(ts, index))
                cr_push_fstring(ts, CR_INTEGER_FMT, cr_to_integer(ts, index));
            else
                cr_push_fstring(ts, CR_FLOAT_FMT, cr_to_number(ts, index));
            break;
        }
        case CR_TSTRING: {
            cr_push(ts, index);
            break;
        }
        default: {
            const char *kind = cr_typename(ts, tt);
            cr_push_fstring(ts, "%s: %p", kind, cr_to_pointer(ts, index));
            break;
        }
    }
    return cr_to_lstring(ts, -1, plen);
}


CRLIB_API void crL_where(cr_State *ts, int level) {
    cr_DebugInfo di;
    if (cr_getstack(ts, level, &di)) {
        cr_getinfo(ts, "sl", &di);
        if (di.defline > 0) { /* have info? */
            cr_push_fstring(ts, "%s:%d", di.shortsrc, di.currline);
            return;
        }
    }
    cr_push_literal(ts, "");
}


CRLIB_API int crL_fileresult(cr_State *ts, int ok, const char *fname) {
    int err = errno;
    if (ok) { /* ok? */
        cr_push_bool(ts, 1);
        return CR_OK;
    } else {
        const char *msg;
        crL_push_fail(ts);
        msg = (err != 0 ? strerror(err) : "(no extra info)");
        if (fname) /* have file name? */
            cr_push_fstring(ts, "%s: %s", fname, msg);
        else
            cr_push_string(ts, msg);
        cr_push_integer(ts, err);
        return CR_ERRFILE;
    }
}


static void *allocator(void *ptr, size_t osz, size_t nsz, void *ud) {
    (void)osz; (void)ud; /* unused */
    if (nsz == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, nsz);
}


static int panic(cr_State *ts) {
    const char *msg = (cr_type(ts, -1) == CR_TSTRING
                       ? cr_to_string(ts, -1)
                       : "error object is not a string");
    cst_writeferror("PANIC: unprotected error in call to CScript API: %s\n",
                     msg);
    return 0; /* return to abort */
}


static void fwarnon(void *ud, const char *msg, int tocont);
static void fwarnoff(void *ud, const char *msg, int tocont);


static int warning_checkmessage(cr_State *ts, const char *msg, int tocont) {
    if (tocont || *msg++ != '@') /* not a control message? */
        return 0;
    if (strcmp(msg, "on") == 0)
        cr_setwarnf(ts, fwarnon, ts);
    else if (strcmp(msg, "off"))
        cr_setwarnf(ts, fwarnoff, ts);
    return 1;
}


static void fwarncont(void *ud, const char *msg, int tocont) {
    cr_State *ts = (cr_State *)ud;
    cst_writeferror("%s", msg);
    if (tocont) { /* to be continued? */
        cr_setwarnf(ts, fwarncont, ud);
    } else { /* this is the end of the warning */
        cst_writeerror("\n");
        cr_setwarnf(ts, fwarnon, ts);
    }
}


static void fwarnon(void *ud, const char *msg, int tocont) {
    if (warning_checkmessage((cr_State *)ud, msg, tocont))
        return; /* it was a control message */
    cst_writeerror("CScript warning: ");
    fwarncont(ud, msg, tocont);
}


static void fwarnoff(void *ud, const char *msg, int tocont) {
    warning_checkmessage((cr_State *)ud, msg, tocont);
}


CRLIB_API cr_State *crL_newstate(void) {
    cr_State *ts = cr_newstate(allocator, NULL);
    if (cr_likely(ts)) {
        cr_atpanic(ts, panic);
        cr_setwarnf(ts, fwarnoff, ts); /* warnings off by default */
    }
    return ts;
}


CRLIB_API int crL_callmeta(cr_State *ts, int index, cr_MM mm) {
    index = cr_absindex(ts, index);
    if (cr_get_metamethod(ts, index, mm) == CR_TNONE)
        return 0;
    cr_push(ts, index); /* push 'self' */
    cr_call(ts, 1, 1);
    return 1;
}


CRLIB_API void *crL_test_userdata(cr_State *ts, int index, const char *name) {
    void *p = cr_to_userdata(ts, index);
    if (p != NULL) { /* 'index' is userdata? */
        cr_push(ts, index);
        cr_get_global(ts, name);
        if (!cr_rawequal(ts, -2, -1)) /* not the same as global? */
            p = NULL; /* value is a userdata but not a global 'name' */
        cr_pop(ts, 2); /* remove both values */
        return p;
    }
    return NULL; /* value is not a userdata */
}


/* --------- */
/* Buffering */
/* --------- */

typedef struct UserBox {
    void *p; /* data */
    size_t sz; /* size of 'p' (data) */
} UserBox;


static void *resizebox(cr_State *ts, int index, size_t newsz) {
    void *ud;
    cr_Alloc falloc = cr_getallocf(ts, &ud);
    UserBox *box = (UserBox *)cr_to_userdata(ts, index);
    void *newblock = falloc(box->p, box->sz, newsz, ud);
    if (cr_unlikely(newblock == NULL && newsz > 0)) {
        cr_push_literal(ts, "out of memory");
        cr_error(ts);
    }
    box->p = newblock;
    box->sz = newsz;
    return newblock;
}


static int boxgc(cr_State *ts) {
    resizebox(ts, 0, 0);
    return 0;
}


static const cr_VMT boxvmt = {
    .func[CR_MM_GC] = boxgc,
    .func[CR_MM_CLOSE] = boxgc,
};


static void newbox(cr_State *ts) {
    UserBox *box = cr_newuserdata(ts, sizeof(*box), 0);
    box->p = NULL;
    box->sz = 0;
    cr_setuserdatavmt(ts, -1, &boxvmt);
}


/*
** Initializes the buffer 'B' and pushes it's placeholder onto
** the top of the stack as light userdata.
*/
CRLIB_API void crL_buff_init(cr_State *ts, crL_Buffer *B) {
    B->ts = ts;
    B->n = 0;
    B->b = B->init.b;
    B->sz = CRL_BUFFERSIZE;
    cr_push_lightuserdata(ts, B);
}


/* 
** Test whether the buffer is using a temporary arena on stack.
*/
#define buffonstack(B)      ((B)->b != (B)->init.b)


/*
** Whenever buffer is accessed, slot 'index' must be either a box,
** meaning the buffer is using a temporary arena on stack (which
** cannot be NULL) or it is a placeholder for the buffer (meaning
** that buffer is still using 'init.b[CRL_BUFFERSIZE]').
*/
#define checkbufflevel(B, index) \
    cr_assert(buffonstack(B) ? cr_to_userdata((B)->ts, index) != NULL \
                             : cr_to_userdata((B)->ts, index) == (void*)(B))


/* calculate new buffer size */
static size_t newbuffsize(crL_Buffer *B, size_t sz) {
    size_t newsize = (B->sz / 2) * 3; /* 1.5x size */
    if (cr_unlikely(SIZE_MAX - sz < B->n)) /* would overflow? */
        return crL_error(B->ts, "buffer too large");
    if (newsize < B->n + sz)
        newsize = B->n + sz;
    return newsize;
}


/*
** Ensure that buffer 'B' can fit 'sz' bytes.
** This also creates 'UserBox' if internal buffer is not big enough.
*/
static char *buffensure(crL_Buffer *B, size_t sz, int boxindex) {
    checkbufflevel(B, boxindex);
    if (B->sz - B->n >= sz) { /* have enough space? */
        return B->b + B->n;
    } else { /* otherwise expand */
        char *newb;
        cr_State *ts = B->ts;
        size_t newsize = newbuffsize(B, sz);
        if (buffonstack(B)) { /* already have 'UserBox'? */
            newb = resizebox(ts, boxindex, newsize); /* resize it and done */
        } else {
            cr_remove(ts, boxindex); /* remove placeholder */
            newbox(ts); /* create new user box on top */
            cr_insert(ts, boxindex); /* insert the new box into 'boxindex' */
            cr_toclose(ts, boxindex); /* mark box to-be-closed */
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
CRLIB_API char *crL_buff_initsz(cr_State *ts, crL_Buffer *B, size_t sz) {
    crL_buff_init(ts, B);
    return buffensure(B, sz, -1);
}


/*
** Return the pointer to the free memory block of at least 'sz' bytes.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
CRLIB_API char *crL_buff_ensure(crL_Buffer *B, size_t sz) {
    return buffensure(B, sz, -1);
}


/*
** Push sized string into the buffer.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
CRLIB_API void crL_buff_push_lstring(crL_Buffer *B, const char *s, size_t l) {
    if (l > 0) {
        char *b = buffensure(B, l, -1);
        memcpy(b, s, l * sizeof(char));
        crL_buffadd(B, l);
    }
}


/*
** Similar to 'crL_buff_push_lstring', the only difference is that this
** function measures the length of 's'.
** This function expects buffer placeholder or its 'UserBox' to be on
** top of the stack.
*/
CRLIB_API void crL_buff_push_string(crL_Buffer *B, const char *s) {
    crL_buff_push_lstring(B, s, strlen(s));
}


/*
** Pushes the string value on top of the stack into the buffer.
** This function expects buffer placeholder or its 'UserBox' to be on
** the stack below the string value being pushed, which is on top of the stack.
*/
CRLIB_API void crL_buff_push_value(crL_Buffer *B) {
    size_t len;
    const char *str = cr_to_lstring(B->ts, -1, &len);
    char *p = buffensure(B, len, -2);
    memcpy(p, str, len);
    crL_buffadd(B, len);
    cr_pop(B->ts, 1); /* remove string */
}


/*
** Finish the use of buffer 'B' leaving the final string on top of
** the stack.
*/
CRLIB_API void crL_buff_end(crL_Buffer *B) {
    cr_State *ts = B->ts;
    checkbufflevel(B, -1);
    cr_push_lstring(ts, B->b, B->n);
    if (buffonstack(B)) /* have 'UserBox'? */
        cr_closeslot(ts, -2); /* close it -> boxgc */
    cr_remove(ts, -2); /* remove buffer placeholder or box */
}


/* find and return last call frame level */
static int lastlevel(cr_State *ts) {
    cr_DebugInfo di;
    int high = 0, low = 0;
    /* get top limit, and store last known valid in 'low' */
    while (cr_getstack(ts, high, &di)) {
        low = high;
        high *= 2;
    }
    /* binary search between 'low' and 'high' levels */
    while (low < high) {
        int mid = low + ((high - low)/2);
        if (cr_getstack(ts, mid, &di))
            low = mid + 1;
        else
            high = mid;
    }
    return low - 1;
}


/*
** Find field in instance at index -1. If field value is found
** meaning the object at 'index' is equal to the field value, then
** this function returns 1. Only string keys are considered and
** limit is the limit of recursive calls in case instance field
** contains instance value.
*/
static int findfield(cr_State *ts, int index, int limit) {
    if (limit == 0 || !cr_is_instance(ts, -1))
        return 0; /* not found */
    cr_push_nil(ts); /* start 'next' loop */
    while (cr_next(ts, -2)) { /* for each field in instance */
        if (cr_type(ts, -2) == CR_TSTRING) { /* ignore non-string keys */
            if (cr_rawequal(ts, index, -1)) { /* found object? */
                cr_pop(ts, 1); /* remove value (but keep name) */
                return 1;
            }
            else if (findfield(ts, index, limit - 1)) { /* try recursively */
                /* stack: lib_name, lib_instance, field_name (top) */
                cr_push_literal(ts, "."); /* place '.' between the two names */
                cr_replace(ts, -3); /* (in the slot occupied by instance) */
                cr_concat(ts, 3); /* lib_name.field_name */
                return 1;
            }
        }
        cr_pop(ts, 1); /* remove value */
    }
    return 0; /* not found */
}


/*
** Try and push name of the currently active func in 'di'.
** If func is global function, then the its name is pushed on the top of
** the stack.
*/
static int pushglobalfuncname(cr_State *ts, cr_DebugInfo *di) {
    int top = cr_gettop(ts);
    cr_getinfo(ts, "f", di); /* push func */
    cr_get_global(ts, CR_LOADED_LIBS); /* get global lib instance */
    crL_check_stack(ts, 6, "not enough stack space"); /* for 'findfield' */
    if (findfield(ts, top + 1, 2)) { /* found? */
        cr_copy(ts, -1, top + 1); /* copy name to proper place */
        cr_settop(ts, top + 1); /* remove lib instance and name copy */
        return 1;
    } else { /* not a global */
        cr_settop(ts, top); /* remove func and lib instance */
        return 0;
    }
}


static void pushfuncname(cr_State *ts, cr_DebugInfo *di) {
    if (pushglobalfuncname(ts, di)) { /* try first a global name */
        cr_push_fstring(ts, "function '%s'", cr_to_string(ts, -1));
        cr_remove(ts, -2); /* remove name */
    }
    else if (*di->namewhat != '\0') /* name from code? */
        cr_push_fstring(ts, "%s '%s'", di->namewhat, di->name);
    else if (*di->what == 'm') /* main? */
        cr_push_literal(ts, "main chunk");
    else if (*di->what != 'C') /* CScript functions? */
        cr_push_fstring(ts, "function <%s:%d>", di->shortsrc, di->defline);
    else /* unknown */
        cr_push_literal(ts, "?");
}


/* '0' is a valid stack level (top-level) */
#define tostacklevel(x)     ((x)-1)

#define STACKLEVELS         tostacklevel(11)

CRLIB_API void crL_traceback(cr_State *ts, cr_State *at, int level,
                             const char *msg) {
    crL_Buffer B;
    cr_DebugInfo di;
    int last = lastlevel(at);
    int levelsleft = (last - level > (STACKLEVELS*2) ? STACKLEVELS : -1);
    crL_buff_init(ts, &B);
    if (msg) {
        crL_buff_push_string(&B, msg);
        crL_buff_push(&B, '\n');
    }
    crL_buff_push_string(&B, "stack traceback:");
    while (cr_getstack(at, level++, &di)) { /* tracing back... */
        if (levelsleft-- == 0) { /* too many levels? */
            int n = last - level - STACKLEVELS + 1;
            cr_push_fstring(ts, "\n\t(skipping %d levels)", n);
            crL_buff_push_value(&B);
            level += n;
        } else {
            /* source, name, line info */
            cr_getinfo(at, "snl", &di);
            if (di.defline <= 0)
                cr_push_fstring(ts, "\n\t%s in ", di.shortsrc);
            else
                cr_push_fstring(ts, "\n\t%s:%d in ", di.shortsrc, di.currline);
            crL_buff_push_value(&B);
            pushfuncname(ts, &di);
            crL_buff_push_value(&B);
        }
    }
    crL_buff_end(&B);
}
