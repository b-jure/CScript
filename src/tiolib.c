/*
** tiolib.c
** Standard I/O (and system) library
** See Copyright Notice in tokudae.h
*/

#define tiolib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"



#if !defined(t_checkmode)

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
** t_popen spawns a new process connected to the current one through
** the file streams.
** ==================================================================
*/

#if !defined(t_popen)           /* { */

#if defined(TOKU_USE_POSIX)       /* { */

#define t_popen(T,c,m)      (fflush(NULL), popen(c,m))
#define t_pclose(T,file)    (pclose(file))

#elif defined(TOKU_USE_WINDOWS)   /* }{ */

#define t_popen(T,c,m)      (_popen(c,m))
#define t_pclose(T,file)    (_pclose(file))

#if !defined(t_checkmodep)
/* Windows accepts "[rw][bt]?" as valid modes */
#define t_checkmodep(m) ((m[0] == 'r' || m[0] == 'w') && \
        (m[1] == '\0' || ((m[1] == 'b' || m[1] == 't') && m[2] == '\0')))
#endif

#else                           /* }{ */

/* ISO C definition */
#define t_popen(T,c,m) \
        ((void)c, (void)m, tokuL_error(T, "'popen' not supported"), (FILE*)0)
#define t_pclose(T,file)    ((void)T, (void)file, -1)

#endif                          /* } */

#endif                          /* } */


#if !defined(t_checkmodep)
/* By default, Tokudae accepts only "r" or "w" as valid modes */
#define t_checkmodep(m)     ((m[0] == 'r' || m[0] == 'w') && m[1] == '\0')
#endif

/* }=============================================================== */


#if !defined(t_getc)		/* { */

#if defined(TOKU_USE_POSIX)
#define t_getc(f)	        getc_unlocked(f)
#define t_lockfile(f)		flockfile(f)
#define t_unlockfile(f)		funlockfile(f)
#else
#define t_getc(f)		getc(f)
#define t_lockfile(f)		((void)0)
#define t_unlockfile(f)		((void)0)
#endif

#endif				/* } */


/*
** {======================================================
** t_fseek: configuration for longer offsets
** =======================================================
*/

#if !defined(t_fseek)		/* { */

#if defined(TOKU_USE_POSIX)	/* { */

#include <sys/types.h>

#define t_fseek(f,o,w)		fseeko(f,o,w)
#define t_ftell(f)		ftello(f)
#define t_seeknum		off_t

#elif defined(TOKU_USE_WINDOWS) && !defined(_CRTIMP_TYPEINFO) \
   && defined(_MST_VER) && (_MST_VER >= 1400)	/* }{ */

/* Windows (but not DDK) and Visual T++ 2005 or higher */
#define t_fseek(f,o,w)		_fseeki64(f,o,w)
#define t_ftell(f)		_ftelli64(f)
#define t_seeknum		__int64

#else				/* }{ */

/* ISO C definitions */
#define t_fseek(f,o,w)		fseek(f,o,w)
#define t_ftell(f)		ftell(f)
#define t_seeknum		long

#endif				/* } */

#endif				/* } */

/* }====================================================== */



#define IO_PREFIX	"__IO_"
#define IOPREF_LEN	(sizeof(IO_PREFIX)/sizeof(char) - 1)
#define IO_INPUT	(IO_PREFIX "stdin")
#define IO_OUTPUT	(IO_PREFIX "stdout")


typedef tokuL_Stream CStream;


#define tocstream(T) \
        ((CStream *)tokuL_check_userdata(T, 0, TOKU_FILEHANDLE))

#define isclosed(p)     ((p)->closef == NULL)
#define markclosed(p)   ((p)->closef = NULL)



/* get open file handle */
static FILE *tofile(toku_State *T) {
    CStream *p = tocstream(T);
    if (t_unlikely(isclosed(p)))
        tokuL_error(T, "attempt to use a closed file");
    toku_assert(p->f);
    return p->f;
}


/*
** Create new Tokudae stream userdata with TOKU_FILEHANDLE metatable.
** Additionally 'closef' is set as NULL as the stream is considered
** "closed".
*/
static CStream *new_cstream(toku_State *T) {
    CStream *p = (CStream *)toku_push_userdata(T, sizeof(CStream), 0);
    p->closef = NULL; /* mark as closed */
    tokuL_set_metatable(T, TOKU_FILEHANDLE);
    return p;
}


static int aux_close(toku_State *T) {
    CStream *p = tocstream(T);
    toku_CFunction f = p->closef;
    markclosed(p);
    return (*f)(T); /* close it */
}


static int f_gc(toku_State *T) {
    CStream *p = tocstream(T);
    if (!isclosed(p) && p->f != NULL)
        aux_close(T); /* ignore closed and incompletely open files */
    return 0;
}


static int closef(toku_State *T) {
    CStream *p = tocstream(T);
    errno = 0; /* reset errno */
    return tokuL_fileresult(T, (fclose(p->f) == 0), NULL);
}


/* create new incomplete file stream */
static CStream *new_file(toku_State *T) {
    CStream *p = new_cstream(T);
    p->f = NULL;
    p->closef = &closef;
    return p;
}


static void open_and_check(toku_State *T, const char *fname, const char *mode) {
    CStream *p = new_file(T);
    p->f = fopen(fname, mode);
    if (t_unlikely(p->f == NULL))
        tokuL_error(T, "cannot open file '%s' (%s)", fname, strerror(errno));
}


static int io_open(toku_State *T) {
    const char *fname = tokuL_check_string(T, 0);
    const char *mode = tokuL_opt_string(T, 1, "r");
    CStream *p = new_file(T);
    tokuL_check_arg(T, checkmode(mode), 1, "invalid mode");
    errno = 0;
    p->f = fopen(fname, mode);
    return (p->f == NULL) ? tokuL_fileresult(T, 0, fname) : 1;
}


/* forward declare */
static int f_close(toku_State *T);


static int io_close(toku_State *T) {
    if (toku_is_none(T, 0)) /* no arguments? */
        toku_get_cfield_str(T, IO_OUTPUT); /* use default output */
    return f_close(T);
}


static FILE *getiofile(toku_State *T, const char *fname) {
    CStream *p;
    toku_get_cfield_str(T, fname);
    p = (CStream *)toku_to_userdata(T, -1);
    if (t_unlikely(isclosed(p)))
        tokuL_error(T, "default %s file is closed", fname + IOPREF_LEN);
    return p->f;
}


static int io_flush(toku_State *T) {
    FILE *f = getiofile(T, IO_OUTPUT);
    errno = 0;
    return tokuL_fileresult(T, fflush(f) == 0, NULL);
}


static int open_or_set_iofile(toku_State *T, const char *f, const char *mode) {
    if (!toku_is_noneornil(T, 0)) { /* have an argument? */
        const char *fname = toku_to_string(T, 0);
        if (fname) /* have a filename? */
            open_and_check(T, fname, mode); /* open it */
        else { /* otherwise it is a file handle */
            tofile(T); /* check that it's a valid file handle */
            toku_push(T, 0); /* push on top */
        }
        toku_set_cfield_str(T, f); /* set new file handle */
    }
    /* return current value */
    toku_get_cfield_str(T, f);
    return 1;
}


static int io_input(toku_State *T) {
    return open_or_set_iofile(T, IO_INPUT, "r");
}


static int io_output(toku_State *T) {
    return open_or_set_iofile(T, IO_OUTPUT, "w");
}


/* function to close 'popen' files */
static int io_pclose(toku_State *T) {
    CStream *p = tocstream(T);
    errno = 0;
    return tokuL_execresult(T, t_pclose(T, p->f));
}


static int io_popen(toku_State *T) {
    const char *cmd = tokuL_check_string(T, 0);
    const char *mode = tokuL_opt_string(T, 1, "r");
    CStream *p = new_cstream(T);
    tokuL_check_arg(T, t_checkmodep(mode), 1, "invalid mode");
    errno = 0;
    p->f = t_popen(T, cmd, mode);
    p->closef = &io_pclose;
    return (p->f == NULL) ? tokuL_fileresult(T, 0, cmd) : 1;
}


static int io_tmpfile(toku_State *T) {
    CStream *p = new_file(T);
    errno = 0;
    p->f = tmpfile();
    return (p->f == NULL) ? tokuL_fileresult(T, 0, NULL) : 1;
}


static int io_type(toku_State *T) {
    CStream *p;
    tokuL_check_any(T, 0);
    p = (CStream *)tokuL_test_userdata(T, 0, TOKU_FILEHANDLE);
    if (p == NULL) /* not a file? */
        tokuL_push_fail(T);
    else if (isclosed(p)) /* closed file? */
        toku_push_literal(T, "closed file");
    else /* open file */
        toku_push_literal(T, "file");
    return 1;
}


/* forward declare */
static int iter_readline(toku_State *T);


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
static void aux_lines(toku_State *T, int toclose) {
    int n = toku_getntop(T) - 1;
    tokuL_check_arg(T, n <= MAXARGLINE, MAXARGLINE + 1, "too many arguments");
    toku_push(T, 0); /* file */
    toku_push_integer(T, n); /* number of arguments to read */
    toku_push_bool(T, toclose); /* to (not)close file when finished */
    toku_rotate(T, 1, 3); /* move the three values to their positions */
    toku_push_cclosure(T, iter_readline, 3 + n);
}


/*
** Return an iteration function for 'io.lines'. If file has to be
** closed, also returns the file itself as a second result (to be
** closed as the state at the exit of a foreach loop).
*/
static int io_lines(toku_State *T) {
    int toclose;
    if (toku_is_none(T, 0)) toku_push_nil(T); /* at least one argument */
    if (toku_is_nil(T, 0)) { /* no file name? */
        toku_get_cfield_str(T, IO_INPUT); /* get default input */
        toku_replace(T, 0); /* put it at index 0 */
        tofile(T); /* check that it's a valid file handle */
        toclose = 0; /* do not close it after iteration */
    } else { /* open a new file */
        const char *fname = tokuL_check_string(T, 0);
        open_and_check(T, fname, "r");
        toku_replace(T, 0); /* put file at index 0 */
        toclose = 1; /* close it after iteration */
    }
    aux_lines(T, toclose); /* push iteration function */
    if (toclose) { /* file is not a default input? */
        toku_push_nil(T); /* state (unused in the iterator function) */
        toku_push_nil(T); /* control (unused in the iterator function) */
        toku_push(T, 0); /* file is the to-be-closed variable (4th result) */
        return 4; /* return func, nil, nil, file */
    } else
        return 1; /* return only iter. function */
}


/* {======================================================
** READ
** ======================================================= */

/* maximum length of a numeral */
#if !defined(T_MAXNUMERAL)
#define T_MAXNUMERAL    200
#endif


typedef enum { DTDEC, DTHEX, DTBIN } DigitType;


/* auxiliary structure used by 'read_number' */
typedef struct NumBuff {
    FILE *f;
    int c;
    int n;
    char buff[T_MAXNUMERAL + 1];
} NumBuff;


/* add current char to buffer (if not out of space) and read next one */
static int nextchar(NumBuff *nb) {
    if (t_unlikely(nb->n >= T_MAXNUMERAL)) { /* buffer overflow? */
        nb->buff[0] = '\0'; /* invalidate result */
        return 0; /* fail */
    } else {
        nb->buff[nb->n++] = nb->c; /* save current char */
        nb->c = t_getc(nb->f); /* read next char */
        return 1;
    }
}


/* skip current char */
static int skipchar(NumBuff *nb) {
    if (t_unlikely(nb->n >= T_MAXNUMERAL))
        return 0;
    nb->c = t_getc(nb->f);
    return 1;
}


/* accept current char if it is in 'set' (of size 2) */
static int test2(NumBuff *nb, const char *set) {
    if (nb->c == set[0] || nb->c == set[1])
        return nextchar(nb);
    else return 0;
}


/* read sequence of (hex)digits */
static int read_digits(NumBuff *nb, DigitType dt, int rad) {
    int count = 0;
    for (;;) {
        if (count > 0 && !rad && nb->c == '_') {
            if (!skipchar(nb))
                break; /* buffer overflow */
        } else {
            switch (dt) {
                case DTDEC: {
                    if (!isdigit(nb->c))
                        return count;
                    break;
                }
                case DTHEX: {
                    if (!isxdigit(nb->c))
                        return count;
                    break;
                }
                case DTBIN: {
                    if (nb->c != '0' && nb->c != '1')
                        return count;
                    break;
                }
                default: toku_assert(0); /* invalid digit type */
            }
            if (!nextchar(nb))
                break; /* buffer overflow */
            count++;
        }
    }
    return count;
}


/*
** Read a number; first reads a valid prefix of a numeral into a buffer.
** Then it calls 'toku_stringtonumber' to check wheter the format is
** correct and to convert it to a Tokudae number.
*/
static int read_number(toku_State *T, FILE *f) {
    NumBuff nb;
    int count = 0;
    DigitType dt = DTDEC;
    char decp[2];
    nb.f = f; nb.n = 0;
    decp[0] = toku_getlocaledecpoint();
    decp[1] = '.';
    t_lockfile(nb.f);
    do { nb.c = t_getc(nb.f); } while (isspace(nb.c)); /* skip leading space */
    test2(&nb, "+-"); /* optional sign */
    if (test2(&nb, "00")) { /* leading zero? */
        if (test2(&nb, "xX")) /* have hex prefix? */
            dt = DTHEX; /* numeral as hexadecimal */
        else if (test2(&nb, "bB")) /* have binary prefix? */
            dt = DTBIN;
        else { /* decimal (or octal) */
            dt = DTDEC;
            count = 1; /* count initial '0' as valid digit */
        }
        if (count == 0 && nb.c != '_') /* separator is first? */
            nextchar(&nb); /* force error */
    }
    count += read_digits(&nb, dt, 0); /* integral part */
    if (test2(&nb, decp)) /* decimal point? */
        count += read_digits(&nb, dt, 1); /* read fractional part */
    if (count > 0 && dt != DTBIN) { /* can have an exponent? */
        if (test2(&nb, (dt == DTHEX) ? "pP" : "eE")) { /* exponent? */
            test2(&nb, "+-"); /* exponent sign */
            read_digits(&nb, DTDEC, 0); /* exponent digits */
        }
    }
    ungetc(nb.c, nb.f); /* unread look-ahead char */
    t_unlockfile(nb.f);
    nb.buff[nb.n] = '\0'; /* null terminate */
    if (t_likely(toku_stringtonumber(T, nb.buff, NULL)))
        return 1; /* ok, it is a valid number */
    else { /* invalid format */
        toku_push_nil(T); /* "result to be removed */
        return 0; /* read fails */
    }
}


static int test_eof(toku_State *T, FILE *f) {
    int c = getc(f);
    ungetc(c, f); /* no-op when c == EOF */
    toku_push_literal(T, "");
    return (c != EOF);
}


static int read_line(toku_State *T, FILE *f, int chop) {
    tokuL_Buffer b;
    int c;
    tokuL_buff_init(T, &b);
    do { /* may need to read several chunks to get whole line */
        char *buff = tokuL_buff_prep(&b); /* preallocate buffer space */
        int i = 0;
        t_lockfile(f); /* no memory errors can happen inside the lock */
        while (i < TOKUL_BUFFERSIZE && (c = t_getc(f)) != EOF && c != '\n')
            buff[i++] = c;/* read up to end of line or buffer limit */
        t_unlockfile(f);
        tokuL_buffadd(&b, i);
    } while (c != EOF && c != '\n'); /* repeat until end of line */
    if (!chop && c == '\n') /* want a newline and have one? */
        tokuL_buff_push(&b, c); /* add ending newline to result */
    tokuL_buff_end(&b); /* close buffer */
    /* return ok if read something (either a newline or something else) */
    return (c == '\n' || toku_len(T, -1) > 0);
}


static void read_all(toku_State *T, FILE *f) {
    size_t nr;
    tokuL_Buffer b;
    tokuL_buff_init(T, &b);
    do { /* read file in chunks of TOKUL_BUFFERSIZE bytes */
        char *p = tokuL_buff_prep(&b);
        nr = fread(p, sizeof(char), TOKUL_BUFFERSIZE, f);
        tokuL_buffadd(&b, nr);
    } while (nr == TOKUL_BUFFERSIZE);
    tokuL_buff_end(&b); /* close buffer */
}


static int read_chars(toku_State *T, FILE *f, size_t n) {
    size_t nr; /* number of chars actually read */
    char *p;
    tokuL_Buffer b;
    tokuL_buff_init(T, &b);
    p = tokuL_buff_ensure(&b, n); /* prepare buffer to read whole block */
    nr = fread(p, sizeof(char), n, f); /* try to read 'n' chars */
    tokuL_buffadd(&b, nr);
    tokuL_buff_end(&b); /* close buffer */
    return (nr > 0); /* true if read something */
}


static int aux_read(toku_State *T, FILE *f, int first) {
    int nargs = toku_getntop(T) - 1;
    int n, success;
    clearerr(f);
    errno = 0;
    if (nargs == 0) { /* no arguments? */
        success = read_line(T, f, 1);
        n = first + 1; /* return 1 result */
    } else {
        /* ensure stack space for all results and for auxlib's buffer */
        tokuL_check_stack(T, nargs + TOKU_MINSTACK, "too many arguments");
        success = 1;
        for (n = first; nargs-- && success; n++) {
            if (toku_type(T, n) == TOKU_T_NUMBER) {
                size_t l = tokuL_check_integer(T, n);
                success = (l == 0) ? test_eof(T, f) : read_chars(T, f, l);
            } else {
                size_t lp;
                const char *p = tokuL_check_lstring(T, n, &lp);
                if (t_unlikely(lp > 1))
                    return tokuL_error_arg(T, n, "format string too long");
                else {
                    switch (*p) {
                        case 'n': success = read_number(T, f); break;
                        case 'l': success = read_line(T, f, 1); break;
                        case 'L': success = read_line(T, f, 0); break;
                        case 'a': read_all(T, f); success = 1; break;
                        default: return tokuL_error_arg(T, n, "invalid format");
                    }
                }
            }
        }
    }
    if (ferror(f))
        return tokuL_fileresult(T, 0, NULL);
    if (!success) {
        toku_pop(T, 1); /* remove last result */
        tokuL_push_fail(T); /* push nil instead */
    }
    return n - first;
}


static int io_read(toku_State *T) {
    FILE *f = getiofile(T, IO_INPUT);
    return aux_read(T, f, 0);
}


/* iterator function for 'lines' */
static int iter_readline(toku_State *T) {
    CStream *p = (CStream *)toku_to_userdata(T, toku_upvalueindex(0));
    int n = toku_to_integer(T, toku_upvalueindex(1));
    int i;
    if (isclosed(p)) /* file is already closed? */
        return tokuL_error(T, "file is already closed");
    toku_setntop(T, 1);
    tokuL_check_stack(T, n, "too many arguments");
    for (i = 1; i <= n; i++) /* push arguments to 'aux_read' */
        toku_push(T, toku_upvalueindex(2 + i));
    n = aux_read(T, p->f, 1); /* 'n' is number of results */
    toku_assert(n > 0); /* should return at least a nil */
    if (toku_to_bool(T, -n)) /* read at least one value? */
        return n; /* return them */
    else { /* first result is false: EOF or error */
        if (n > 1) { /* is there error information? */
            /* 2nd result is error message */
            return tokuL_error(T, "%s", toku_to_string(T, -n + 1));
        }
        if (toku_to_bool(T, toku_upvalueindex(2))) { /* generator created file? */
            toku_setntop(T, 0); /* clear stack */
            toku_push(T, toku_upvalueindex(0)); /* push file */
            aux_close(T); /* close it */
        }
        return 0;
    }
}

/* }====================================================== */


static int aux_write(toku_State *T, FILE *f, int arg) {
    int nargs = toku_gettop(T) - arg;
    int status = 1;
    errno = 0;
    for (; nargs--; arg++) {
        if (toku_type(T, arg) == TOKU_T_NUMBER) {
            int len = toku_is_integer(T, arg)
                    ? fprintf(f, TOKU_INTEGER_FMT, toku_to_integer(T, arg))
                    : fprintf(f, TOKU_NUMBER_FMT, toku_to_number(T, arg));
            status = status && (len > 0);
        } else { /* string */
            size_t l;
            const char *s = tokuL_check_lstring(T, arg, &l);
            status = status && (fwrite(s, sizeof(char), l, f) == l);
        }
    }
    if (t_likely(status))
        return 1; /* file handle already on stack top */
    else
        return tokuL_fileresult(T, status, NULL);
}


static int io_write(toku_State *T) {
    FILE *f = getiofile(T, IO_OUTPUT);
    return aux_write(T, f, 0);
}


/* function for 'io' library */
static const tokuL_Entry iolib[] = {
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


static int f_read(toku_State *T) {
    FILE *f = tofile(T);
    return aux_read(T, f, 1);
}


static int f_write(toku_State *T) {
    FILE *f = tofile(T);
    toku_push(T, 0); /* push file at the stack top (to be returned) */
    return aux_write(T, f, 1);
}


static int f_lines(toku_State *T) {
    tofile(T);
    aux_lines(T, 0);
    return 1;
}


static int f_flush(toku_State *T) {
    FILE *f = tofile(T);
    errno = 0;
    return tokuL_fileresult(T, fflush(f) == 0, NULL);
}


static int f_seek(toku_State *T) {
    static const int whence[] = { SEEK_SET, SEEK_CUR, SEEK_END };
    static const char *whence_names[] = { "set", "cur", "end", NULL };
    FILE *f = tofile(T);
    int opt = tokuL_check_option(T, 1, "cur", whence_names);
    t_seeknum offset = (t_seeknum)tokuL_opt_integer(T, 2, 0);
    int res = fseek(f, offset, whence[opt]);
    if (t_unlikely(res))
        return tokuL_fileresult(T, 0, NULL); /* error */
    else {
        /* 't_ftell' shouldn't fail as 'fseek' was successful */
        toku_push_integer(T, (toku_Integer)t_ftell(f));
        return 1;
    }
}


static int f_close(toku_State *T) {
    tofile(T); /* make sure argument is open stream */
    return aux_close(T);
}


static int f_setvbuf(toku_State *T) {
    static const int modes[] = { _IONBF, _IOLBF, _IOFBF };
    static const char *mode_names[] = { "no", "line", "full", NULL };
    FILE *f = tofile(T);
    int opt = tokuL_check_option(T, 1, NULL, mode_names);
    toku_Integer sz = tokuL_opt_integer(T, 2, TOKUL_BUFFERSIZE);
    int res = setvbuf(f, NULL, modes[opt], (size_t)sz);
    return tokuL_fileresult(T, (res == 0), NULL);
}


/* methods for file handles */
static const tokuL_Entry f_methods[] = {
    {"read", f_read},
    {"write", f_write},
    {"lines", f_lines},
    {"flush", f_flush},
    {"seek", f_seek},
    {"close", f_close},
    {"setvbuf", f_setvbuf},
    {NULL, NULL},
};


static int f_getidx(toku_State *T) {
    tocstream(T);
    tokuL_get_metafield(T, 0, "__methods");
    if (toku_get_field(T, -1) == TOKU_T_NIL)
        tokuL_error_arg(T, 1, "unknown file method");
    toku_push_method(T, 0);
    return 1;
}


static int f_tostring(toku_State *T) {
    CStream *p = tocstream(T);
    if (isclosed(p))
        toku_push_literal(T, "file (closed)");
    else
        toku_push_fstring(T, "file (%p)", (void *)p->f);
    return 1;
}


static const tokuL_Entry f_meta[] = {
    {"__getidx", f_getidx},
    {"__gc", f_gc},
    {"__close", f_gc},
    {"__tostring", f_tostring},
    {"__name", NULL},
    {NULL, NULL},
};


static void create_filehandle_metatable(toku_State *T) {
    tokuL_new_metatable(T, TOKU_FILEHANDLE); /* metatable for file handles */
    tokuL_set_funcs(T, f_meta, 0); /* add metamethods to metatable */
    tokuL_push_libtable(T, f_methods); /* create methods table */
    tokuL_set_funcs(T, f_methods, 0); /* add file methods to methods table */
    toku_set_field_str(T, -2, "__methods"); /* metatable.__methods = m. tab. */
    toku_pop(T, 1); /* remove metatable */
}


/*
** function to (not) close the standard files stdin, stdout, and stderr
*/
static int io_noclose(toku_State *T) {
    CStream *p = tocstream(T);
    p->closef = &io_noclose; /* keep file opened */
    tokuL_push_fail(T);
    toku_push_literal(T, "cannot close standard file");
    return 2;
}


static void create_stdfile(toku_State *T, FILE *f, const char *k,
                           const char *fname) {
    CStream *p = new_cstream(T);
    p->f = f;
    p->closef = &io_noclose;
    if (k != NULL) {
        toku_push(T, -1);
        toku_set_cfield_str(T, k); /* add file to ctable */
    }
    toku_set_field_str(T, -2, fname); /* add file to module */
}


TOKUMOD_API int tokuopen_io(toku_State *T) {
    tokuL_push_lib(T, iolib); /* 'io' table */
    create_filehandle_metatable(T);
    /* create (and set) default files */
    create_stdfile(T, stdin, IO_INPUT, "stdin");
    create_stdfile(T, stdout, IO_OUTPUT, "stdout");
    create_stdfile(T, stderr, NULL, "stderr");
    return 1;
}
