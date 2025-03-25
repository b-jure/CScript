/*
** ciolib.c
** Standard I/O (and system) library
** See Copyright Notice in lua.h
*/

#define CS_LIB

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

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
    csL_set_metalist(C, CS_FILEHANDLE);
    csL_set_usermethods(C, CS_FILEHANDLE_TABLE);
    return p;
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


static int aux_close(cs_State *C) {
    CStream *p = tocstream(C);
    cs_CFunction f = p->closef;
    markclosed(p);
    return (*f)(C); /* close it */
}


static int f_close(cs_State *C) {
    tofile(C); /* make sure argument is open stream */
    return aux_close(cs_State *C);
}


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
            open_and_check(f, mode); /* open 'fname' and check for errors */
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
    open_or_set_iofile(C, IO_INPUT, "r");
}


static int io_output(cs_State *C) {
    open_or_set_iofile(C, IO_OUTPUT, "w");
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
        lua_pushliteral(C, "closed file");
    else /* open file */
        lua_pushliteral(C, "file");
    return 1;
}


/* forward declare */
static int io_readline(cs_State *C);


static int io_lines(cs_State *C) {
    // TODO
    return 0;
}


static int aux_read(cs_State *C, FILE *f, int first) {
    // TODO
}


static int io_read(cs_State *C) {
    // TODO
}


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
    aux_write(C, f, 0);
}


static int f_write(cs_State *C) {
    FILE *f = tofile(C);
    cs_push(C, 0); /* push file at the stack top (to be returned) */
    aux_write(C, f, 1);
}


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


static int f_flush(cs_State *C) {
    FILE *f = tofile(C);
    errno = 0;
    return csL_fileresult(C, fflush(f) == 0, NULL);
}


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
}


static int f_getidx(cs_State *C) {
    (void)tocstream(C);
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
