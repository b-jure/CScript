/*
** ciolib.c
** Standard I/O (and system) library
** See Copyright Notice in cscript.h
*/

#define CS_LIB

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <locale.h>

#include "cscript.h"

#include "cauxlib.h"
#include "cslib.h"
#include "climits.h"


#if !defined(c_checkmode)

static int checkmode(const char *mode) {
    /* check if it starts with 'r', 'w' or 'a' */
    if (*mode != '\0' && strchr("rwa", *(mode++)) != NULL) {
        if (*mode == '+') mode++; /* skip '+' */
        return (strspn(mode, "b") == strlen(mode)); /* check 'b' extension */
    }
    return 0; /* invalid mode */
}

#endif

/* 
** {=================================================================
** c_popen spawns a new process connected to the current one through
** the file streams.
** ==================================================================
*/

#if !defined(c_popen)           /* { */

#if defined(CS_USE_POSIX)       /* { */

#define c_popen(C,f,m)      (fflush(NULL), popen(f,m))
#define c_pclose(C,file)    (pclose(file))

#elif defined(CS_USE_WINDOWS)   /* }{ */

#define c_popen(C,f,m)      (_popen(f,m))
#define c_pclose(C,file)    (_pclose(file))

#if !defined(c_checkmodep)
/* Windows accepts "[rw][bt]?" as valid modes */
#define c_checkmodep(m) ((m[0] == 'r' || m[0] == 'w') && \
        (m[1] == '\0' || ((m[1] == 'b' || m[1] == 't') && m[2] == '\0')))
#endif

#else                           /* }{ */

/* ISO C definition */
#define c_popen(C,f,m) \
        ((void)f, (void)m, csL_error(C, "'popen' not supported"), (FILE*)0)
#define c_pclose(C,file)    ((void)C, (void)file, -1)

#endif                          /* } */

#endif                          /* } */


#if !defined(c_checkmodep)
/* By default, CScript accepts only "r" or "w" as valid modes */
#define c_checkmodep(m)     ((m[0] == 'r' || m[0] == 'w') && m[1] == '\0')
#endif

/* }=============================================================== */


#if !defined(c_getc)		/* { */

#if defined(CS_USE_POSIX)
#define c_getc(f)	        getc_unlocked(f)
#define c_lockfile(f)		flockfile(f)
#define c_unlockfile(f)		funlockfile(f)
#else
#define c_getc(f)		getc(f)
#define c_lockfile(f)		((void)0)
#define c_unlockfile(f)		((void)0)
#endif

#endif				/* } */


/*
** {======================================================
** c_fseek: configuration for longer offsets
** =======================================================
*/

#if !defined(c_fseek)		/* { */

#if defined(CS_USE_POSIX)	/* { */

#include <sys/types.h>

#define c_fseek(f,o,w)		fseeko(f,o,w)
#define c_ftell(f)		ftello(f)
#define c_seeknum		off_t

#elif defined(CS_USE_WINDOWS) && !defined(_CRTIMP_TYPEINFO) \
   && defined(_MSC_VER) && (_MSC_VER >= 1400)	/* }{ */

/* Windows (but not DDK) and Visual C++ 2005 or higher */
#define c_fseek(f,o,w)		_fseeki64(f,o,w)
#define c_ftell(f)		_ftelli64(f)
#define c_seeknum		__int64

#else				/* }{ */

/* ISO C definitions */
#define c_fseek(f,o,w)		fseek(f,o,w)
#define c_ftell(f)		ftell(f)
#define c_seeknum		long

#endif				/* } */

#endif				/* } */

/* }====================================================== */



#define IO_PREFIX	"__IO_"
#define IOPREF_LEN	(sizeof(IO_PREFIX)/sizeof(char) - 1)
#define IO_INPUT	(IO_PREFIX "STDIN")
#define IO_OUTPUT	(IO_PREFIX "STDOUT")


typedef csL_Stream CStream;


#define tocstream(C)    ((CStream *)csL_check_userdata(C, 0, CS_FILEHANDLE))

#define isclosed(p)     ((p)->closef == NULL)
#define markclosed(p)   ((p)->closef = NULL)



/* get open file handle */
static FILE *tofile(cs_State *C) {
    CStream *p = tocstream(C);
    if (c_unlikely(isclosed(p)))
        csL_error(C, "attempt to use a closed file");
    cs_assert(p->f);
    return p->f;
}


/*
** Create new CScript stream userdata with CS_FILEHANDLE metalist and
** CS_FILEHANDLE_TABLE methods table.
** Additionally 'closef' is set as NULL as the stream is considered
** "closed".
*/
static CStream *new_cstream(cs_State *C) {
    CStream *p = (CStream *)cs_push_userdata(C, sizeof(CStream), 0);
    p->closef = NULL; /* mark as closed */
    csL_set_usermethods(C, CS_FILEHANDLE_TABLE);
    csL_set_metalist(C, CS_FILEHANDLE);
    return p;
}


static int aux_close(cs_State *C) {
    CStream *p = tocstream(C);
    cs_CFunction f = p->closef;
    markclosed(p);
    return (*f)(C); /* close it */
}


static int f_gc(cs_State *C) {
    CStream *p = tocstream(C);
    if (!isclosed(p) && p->f != NULL)
        aux_close(C); /* ignore closed and incompletely open files */
    return 0;
}


static int closef(cs_State *C) {
    CStream *p = tocstream(C);
    errno = 0; /* reset errno */
    return csL_fileresult(C, (fclose(p->f) == 0), NULL);
}


/* create new incomplete file stream */
static CStream *new_file(cs_State *C) {
    CStream *p = new_cstream(C);
    p->f = NULL;
    p->closef = &closef;
    return p;
}


static void open_and_check(cs_State *C, const char *fname, const char *mode) {
    CStream *p = new_file(C);
    p->f = fopen(fname, mode);
    if (c_unlikely(p->f == NULL))
        csL_error(C, "cannot open file '%s' (%s)", fname, strerror(errno));
}


static int io_open(cs_State *C) {
    const char *fname = csL_check_string(C, 0);
    const char *mode = csL_opt_string(C, 1, "r");
    CStream *p = new_file(C);
    csL_check_arg(C, checkmode(mode), 1, "invalid mode");
    errno = 0;
    p->f = fopen(fname, mode);
    return (p->f == NULL) ? csL_fileresult(C, 0, fname) : 1;
}


/* forward declare */
static int f_close(cs_State *C);


static int io_close(cs_State *C) {
    if (cs_is_none(C, 0)) /* no arguments? */
        cs_get_rtable(C, IO_OUTPUT); /* use default output */
    return f_close(C);
}


static FILE *getiofile(cs_State *C, const char *fname) {
    CStream *p;
    cs_get_rtable(C, fname);
    p = (CStream *)cs_to_userdata(C, -1);
    if (c_unlikely(isclosed(p)))
        csL_error(C, "default %s file is closed", fname + IOPREF_LEN);
    return p->f;
}


static int io_flush(cs_State *C) {
    FILE *f = getiofile(C, IO_OUTPUT);
    errno = 0;
    return csL_fileresult(C, fflush(f) == 0, NULL);
}


static int open_or_set_iofile(cs_State *C, const char *f, const char *mode) {
    if (!cs_is_noneornil(C, 0)) { /* have an argument? */
        const char *fname = cs_to_string(C, 0);
        if (fname) /* have a filename? */
            open_and_check(C, fname, mode); /* open it */
        else { /* otherwise it is a file handle */
            tofile(C); /* check that it's a valid file handle */
            cs_push(C, 0); /* push on top */
        }
        cs_set_rtable(C, f); /* set new file handle */
    }
    /* return current value */
    cs_get_rtable(C, f);
    return 1;
}


static int io_input(cs_State *C) {
    return open_or_set_iofile(C, IO_INPUT, "r");
}


static int io_output(cs_State *C) {
    return open_or_set_iofile(C, IO_OUTPUT, "w");
}


/* function to close 'popen' files */
static int io_pclose(cs_State *C) {
    CStream *p = tocstream(C);
    errno = 0;
    return csL_execresult(C, c_pclose(C, p->f));
}


static int io_popen(cs_State *C) {
    const char *fname = csL_check_string(C, 0);
    const char *mode = csL_opt_string(C, 1, "r");
    CStream *p = new_cstream(C);
    csL_check_arg(C, c_checkmodep(mode), 1, "invalid mode");
    errno = 0;
    p->f = c_popen(C, fname, mode);
    p->closef = &io_pclose;
    return (p->f == NULL) ? csL_fileresult(C, 0, fname) : 1;
}


static int io_tmpfile(cs_State *C) {
    CStream *p = new_file(C);
    errno = 0;
    p->f = tmpfile();
    return (p->f == NULL) ? csL_fileresult(C, 0, NULL) : 1;
}


static int io_type(cs_State *C) {
    CStream *p;
    csL_check_any(C, 0);
    p = (CStream *)csL_test_userdata(C, 0, CS_FILEHANDLE);
    if (p == NULL) /* not a file? */
        csL_push_fail(C);
    else if (isclosed(p)) /* closed file? */
        cs_push_literal(C, "closed file");
    else /* open file */
        cs_push_literal(C, "file");
    return 1;
}


/* forward declare */
static int iter_readline(cs_State *C);


/*
** maximum number of arguments to 'f:lines'/'io.lines' (it + 3 must fit
** in the limit for upvalues of a closure)
*/
#define MAXARGLINE      (USHRT_MAX - 5)


/*
** Auxiliary function to create the iteration function for 'lines'.
** The iteration function is a closure over 'iter_readline', with
** the following upvalues:
** 1) The file being read (first value in the stack)
** 2) the number of arguments to read
** 3) a boolean, true iff file has to be closed when finished ('toclose')
** *) a variable number of format arguments (rest of the stack)
*/
static void aux_lines(cs_State *C, int toclose) {
    int n = cs_getntop(C) - 1;
    csL_check_arg(C, n <= MAXARGLINE, MAXARGLINE + 1, "too many arguments");
    cs_push(C, 0); /* file */
    cs_push_integer(C, n); /* number of arguments to read */
    cs_push_bool(C, toclose); /* to (not)close file when finished */
    cs_rotate(C, 1, 3); /* move the three values to their positions */
    cs_push_cclosure(C, iter_readline, 3 + n);
}


/*
** Return an iteration function for 'io.lines'. If file has to be
** closed, also returns the file itself as a second result (to be
** closed as the state at the exit of a foreach loop).
*/
static int io_lines(cs_State *C) {
    int toclose;
    if (cs_is_none(C, 0)) cs_push_nil(C); /* at least one argument */
    if (cs_is_nil(C, 0)) { /* no file name? */
        cs_get_rtable(C, IO_INPUT); /* get default input */
        cs_replace(C, 0); /* put it at index 0 */
        tofile(C); /* check that it's a valid file handle */
        toclose = 0; /* do not close it after iteration */
    } else { /* open a new file */
        const char *fname = csL_check_string(C, 0);
        open_and_check(C, fname, "r");
        cs_replace(C, 0); /* put file at index 0 */
        toclose = 1; /* close it after iteration */
    }
    aux_lines(C, toclose); /* push iteration function */
    if (toclose) { /* file is not a default input? */
        cs_push_nil(C); /* state (unused in the iterator function) */
        cs_push_nil(C); /* control (unused in the iterator function) */
        cs_push(C, 0); /* file is the to-be-closed variable (4th result) */
        return 4; /* return iter. function, state, control var and file */
    } else
        return 1; /* return only iter. function */
}


/* {======================================================
** READ
** ======================================================= */

/* valid formats for 'aux_read' */
#define READFORMATS \
    "\"n\" read number, \"l\" read line without end of line, " \
    "\"L\" read line inclusive, or \"a\" read all file contents"

/* errors for 'aux_read' */
static const char *read_format_err[] = {
    "invalid format, expected " READFORMATS,
    "format too long, expected " READFORMATS, 
};

/* errors for 'aux_read' as macros */
#define EREAD_FMT         read_format_err[0]
#define EREAD_FMTLEN      read_format_err[1]


/* maximum length of a numeral */
#if !defined(C_MAXNUMERAL)
#define C_MAXNUMERAL    200
#endif


/* auxiliary structure used by 'read_number' */
typedef struct NumBuff {
    FILE *f;
    int c;
    int n;
    char buff[C_MAXNUMERAL + 1];
} NumBuff;


/* add current char to buffer (if not out of space) and read next one */
static int nextchar(NumBuff *nb) {
    if (c_unlikely(nb->n >= C_MAXNUMERAL)) { /* buffer overflow? */
        nb->buff[0] = '\0'; /* invalidate result */
        return 0; /* fail */
    } else {
        nb->buff[nb->n++] = nb->c; /* save current char */
        nb->c = c_getc(nb->f); /* read next char */
        return 1;
    }
}


/* accept current char if it is in 'set' (of size 2) */
static int test2(NumBuff *nb, const char *set) {
    if (nb->c == set[0] || nb->c == set[1])
        return nextchar(nb);
    else return 0;
}


/* read sequence of (hex)digits */
static int read_digits(NumBuff *nb, int hex) {
    int count = 0;
    while ((hex ? isxdigit(nb->c) : isdigit(nb->c)) && nextchar(nb))
        count++;
    return count;
}


/*
** Read a number; first reads a valid prefix of a numeral into a buffer.
** Then it calls 'cs_stringtonumber' to check wheter the format is
** correct and to convert it to a CScript number.
*/
static int read_number(cs_State *C, FILE *f) {
    NumBuff nb;
    int count = 0;
    int hex = 0;
    char decp[2];
    nb.f = f; nb.n = 0;
    decp[0] = cs_getlocaledecpoint();
    decp[1] = '.';
    c_lockfile(nb.f);
    do { nb.c = c_getc(nb.f); } while (isspace(nb.c)); /* skip leading space */
    test2(&nb, "+-"); /* optional sign */
    if (test2(&nb, "00")) {
        if (test2(&nb, "xX")) hex = 1; /* numeral as hexadecimal */
        else count = 1; /* count initial '0' as valid digit */
    }
    count += read_digits(&nb, hex); /* integral part */
    if (test2(&nb, decp)) /* decimal point? */
        count += read_digits(&nb, hex); /* read fractional part */
    if (count > 0 && test2(&nb, (hex ? "pP" : "eE"))) { /* exponent mark? */
        test2(&nb, "+-"); /* exponent sign */
        read_digits(&nb, 0); /* exponent digits */
    }
    ungetc(nb.c, nb.f); /* unread look-ahead char */
    c_unlockfile(nb.f);
    nb.buff[nb.n] = '\0'; /* null terminate */
    if (c_likely(cs_stringtonumber(C, nb.buff, NULL)))
        return 1; /* ok, it is a valid number */
    else { /* invalid format */
        cs_push_nil(C); /* "result to be removed */
        return 0; /* read fails */
    }
}


static int test_eof(cs_State *C, FILE *f) {
    int c = getc(f);
    ungetc(c, f); /* no-op when c == EOF */
    cs_push_literal(C, "");
    return (c != EOF);
}


static int read_line(cs_State *C, FILE *f, int chop) {
    csL_Buffer b;
    int c;
    csL_buff_init(C, &b);
    do { /* may need to read several chunks to get whole line */
        char *buff = csL_buff_prep(&b); /* preallocate buffer space */
        int i = 0;
        c_lockfile(f); /* no memory errors can happen inside the lock */
        while (i < CSL_BUFFERSIZE && (c = c_getc(f)) != EOF && c != '\n')
            buff[i++] = c;/* read up to end of line or buffer limit */
        c_unlockfile(f);
        csL_buffadd(&b, i);
    } while (c != EOF && c != '\n'); /* repeat until end of line */
    if (!chop && c == '\n') /* want a newline and have one? */
        csL_buff_push(&b, c); /* add ending newline to result */
    csL_buff_end(&b); /* close buffer */
    /* return ok if read something (either a newline or something else) */
    return (c == '\n' || cs_len(C, -1) > 0);
}


static void read_all(cs_State *C, FILE *f) {
    size_t nr;
    csL_Buffer b;
    csL_buff_init(C, &b);
    do { /* read file in chunks of CSL_BUFFERSIZE bytes */
        char *p = csL_buff_prep(&b);
        nr = fread(p, sizeof(char), CSL_BUFFERSIZE, f);
        csL_buffadd(&b, nr);
    } while (nr == CSL_BUFFERSIZE);
    csL_buff_end(&b); /* close buffer */
}


static int read_chars(cs_State *C, FILE *f, size_t n) {
    size_t nr; /* number of chars actually read */
    char *p;
    csL_Buffer b;
    csL_buff_init(C, &b);
    p = csL_buff_ensure(&b, n); /* prepare buffer to read whole block */
    nr = fread(p, sizeof(char), n, f); /* try to read 'n' chars */
    csL_buffadd(&b, nr);
    csL_buff_end(&b); /* close buffer */
    return (nr > 0); /* true if read something */
}


static int aux_read(cs_State *C, FILE *f, int first) {
    int nargs = cs_getntop(C) - 1;
    int n, success;
    clearerr(f);
    errno = 0;
    if (nargs == 0) { /* no arguments? */
        success = read_line(C, f, 0);
        n = first + 1; /* return 1 result */
    } else {
        /* ensure stack space for all results and for auxlib's buffer */
        csL_check_stack(C, nargs + CS_MINSTACK, "too many arguments");
        success = 1;
        for (n = first; nargs-- && success; n++) {
            if (cs_type(C, n) == CS_TNUMBER) {
                size_t l = csL_check_integer(C, n);
                success = (l == 0) ? test_eof(C, f) : read_chars(C, f, l);
            } else {
                size_t lp;
                const char *p = csL_check_lstring(C, n, &lp);
                if (c_unlikely(lp > 1))
                    return csL_error_arg(C, n, EREAD_FMTLEN);
                else {
                    switch (*p) {
                        case 'n': success = read_number(C, f); break;
                        case 'l': success = read_line(C, f, 1); break;
                        case 'L': success = read_line(C, f, 0); break;
                        case 'a': read_all(C, f); success = 1; break;
                        default: return csL_error_arg(C, n, EREAD_FMT);
                    }
                }
            }
        }
    }
    if (ferror(f))
        return csL_fileresult(C, 0, NULL);
    if (!success) {
        cs_pop(C, 1); /* remove last result */
        csL_push_fail(C); /* push nil instead */
    }
    return n - first;
}


static int io_read(cs_State *C) {
    FILE *f = getiofile(C, IO_INPUT);
    return aux_read(C, f, 0);
}


/* iterator function for 'lines' */
static int iter_readline(cs_State *C) {
    CStream *p = (CStream *)cs_to_userdata(C, cs_upvalueindex(1));
    int n = cs_to_integer(C, cs_upvalueindex(2));
    int i;
    if (isclosed(p)) /* file is already closed? */
        return csL_error(C, "file is already closed");
    cs_settop(C, 1);
    csL_check_stack(C, n, "too many arguments");
    for (i = 1; i <= n; i++) /* push arguments to 'aux_read' */
        cs_push(C, cs_upvalueindex(3 + i));
    n = aux_read(C, p->f, 1); /* 'n' is number of results */
    cs_assert(n > 0); /* should return at least a nil */
    if (cs_to_bool(C, -n)) /* read at least one value? */
        return n; /* return them */
    else { /* first result is false: EOF or error */
        if (n > 1) { /* is there error information? */
            /* 2nd result is error message */
            return csL_error(C, "%s", cs_to_string(C, -n + 1));
        }
        if (cs_to_bool(C, cs_upvalueindex(3))) { /* generator created file? */
            cs_settop(C, 0); /* clear stack */
            cs_push(C, cs_upvalueindex(1)); /* push file */
            aux_close(C); /* close it */
        }
        return 0;
    }
}

/* }====================================================== */


static int aux_write(cs_State *C, FILE *f, int arg) {
    int nargs = cs_gettop(C) - arg;
    int status = 1;
    errno = 0;
    for (; nargs--; arg++) {
        if (cs_type(C, arg) == CS_TNUMBER) {
            int len = cs_is_integer(C, arg)
                    ? fprintf(f, CS_INTEGER_FMT, cs_to_integer(C, arg))
                    : fprintf(f, CS_NUMBER_FMT, cs_to_number(C, arg));
            status = status && (len > 0);
        } else { /* string */
            size_t l;
            const char *s = csL_check_lstring(C, arg, &l);
            status = status && (fwrite(s, sizeof(char), l, f) == l);
        }
    }
    if (c_likely(status))
        return 1; /* file handle already on stack top */
    else
        return csL_fileresult(C, status, NULL);
}


static int io_write(cs_State *C) {
    FILE *f = getiofile(C, IO_OUTPUT);
    return aux_write(C, f, 0);
}


/* function for 'io' library */
// TODO: add docs
static const cs_Entry iolib[] = {
    {"open", io_open},
    {"close", io_close},
    {"flush", io_flush},
    {"input", io_input},
    {"output", io_output},
    {"popen", io_popen},
    {"tmpfile", io_tmpfile},
    {"type", io_type},
    {"lines", io_lines},
    {"read", io_read},
    {"write", io_write},
    {NULL, NULL},
};


static int f_read(cs_State *C) {
    FILE *f = tofile(C);
    return aux_read(C, f, 1);
}


static int f_write(cs_State *C) {
    FILE *f = tofile(C);
    cs_push(C, 0); /* push file at the stack top (to be returned) */
    return aux_write(C, f, 1);
}


static int f_lines(cs_State *C) {
    tofile(C);
    aux_lines(C, 0);
    return 0;
}


static int f_flush(cs_State *C) {
    FILE *f = tofile(C);
    errno = 0;
    return csL_fileresult(C, fflush(f) == 0, NULL);
}


static int f_seek(cs_State *C) {
    static const int whence[] = { SEEK_SET, SEEK_CUR, SEEK_END };
    static const char *whence_names[] = { "set", "cur", "end", NULL };
    FILE *f = tofile(C);
    int opt = csL_check_option(C, 1, NULL, whence_names);
    c_seeknum offset = (c_seeknum)csL_opt_integer(C, 2, 0);
    int res = fseek(f, offset, whence[opt]);
    if (c_unlikely(res))
        return csL_fileresult(C, 0, NULL); /* error */
    else {
        /* 'c_ftell' shouldn't fail as 'fseek' was successful */
        cs_push_integer(C, (cs_Integer)c_ftell(f));
        return 1;
    }
}


static int f_close(cs_State *C) {
    tofile(C); /* make sure argument is open stream */
    return aux_close(C);
}


static int f_setvbuf(cs_State *C) {
    static const int modes[] = { _IONBF, _IOLBF, _IOFBF };
    static const char *mode_names[] = { "no", "line", "full", NULL };
    FILE *f = tofile(C);
    int opt = csL_check_option(C, 1, NULL, mode_names);
    cs_Integer sz = csL_opt_integer(C, 2, CSL_BUFFERSIZE);
    int res = setvbuf(f, NULL, modes[opt], (size_t)sz);
    return csL_fileresult(C, (res == 0), NULL);
}


/* methods for file handles */
// TODO: add docs
static const cs_Entry f_methods[] = {
    {"read", f_read},
    {"write", f_write},
    {"lines", f_lines},
    {"flush", f_flush},
    {"seek", f_seek},
    {"close", f_close},
    {"setvbuf", f_setvbuf},
    {NULL, NULL},
};


static void create_filehandle_methods(cs_State *C) {
    csL_newlibtable(C, f_methods);
    csL_set_funcs(C, f_methods, 0);
    csL_new_usermethods(C, CS_FILEHANDLE_TABLE,
                        sizeof(f_methods)/sizeof(f_methods[0]) - 1);
    cs_pop(C, 1); /* remove methods table */
}


static int f_getidx(cs_State *C) {
    tocstream(C);
    cs_get_method(C, -2);
    return 1;
}


static int f_tostring(cs_State *C) {
    CStream *p = tocstream(C);
    if (isclosed(p))
        cs_push_literal(C, "file (closed)");
    else
        cs_push_fstring(C, "file (%p)", (void *)p->f);
    return 1;
}


static const csL_MetaEntry f_mm[] = {
    {CS_MM_GETIDX, f_getidx},
    {CS_MM_GC, f_gc},
    {CS_MM_CLOSE, f_close},
    {CS_MM_TOSTRING, f_tostring},
    {-1, NULL},
};


static void create_filehandle_metalist(cs_State *C) {
    csL_new_metalist(C, CS_FILEHANDLE); /* metalist for file handles */
    csL_set_metafuncs(C, f_mm, 0); /* set its metamethods */
    cs_pop(C, 1); /* remove metalist */
}


/*
** function to (not) close the standard files stdin, stdout, and stderr
*/
static int io_noclose(cs_State *C) {
    CStream *p = tocstream(C);
    p->closef = &io_noclose; /* keep file opened */
    csL_push_fail(C);
    cs_push_literal(C, "cannot close standard file");
    return 2;
}


static void create_stdfile(cs_State *C, FILE *f, const char *k,
                           const char *fname) {
    CStream *p = new_cstream(C);
    p->f = f;
    p->closef = &io_noclose;
    if (k != NULL) {
        cs_push(C, -1);
        cs_set_rtable(C, k); /* add file to registry table */
    }
    cs_set_fieldstr(C, -2, fname); /* add file to module */
}


CSMOD_API int csopen_io(cs_State *C) {
    csL_newlib(C, iolib); /* 'io' table */
    create_filehandle_methods(C);
    create_filehandle_metalist(C);
    /* create (and set) default files */
    create_stdfile(C, stdin, IO_INPUT, "stdin");
    create_stdfile(C, stdout, IO_OUTPUT, "stdout");
    create_stdfile(C, stderr, NULL, "stderr");
    return 1;
}
