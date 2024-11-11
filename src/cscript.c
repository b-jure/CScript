/*
** cscript.c
** CScript Interpreter
** See Copyright Notice in cscript.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscript.h"

#include "cauxlib.h"
#include "cslib.h"
#include "ctrace.h"


#define CS_PROGNAME     "CScript"


#define ewritefmt(fmt,...)      cs_writefmt(stderr, fmt, __VA_ARGS__)


/* assume stdin is a tty */
#define cs_stdin_is_tty()   1


static const char *progname = CS_PROGNAME;


static void printusage(const char *badopt) {
    FILE *fp;
    if (badopt) {
        fp = stderr;
        if (badopt[1] == 's')
            ewritefmt("option '%s' needs argument\n", badopt);
        else
            ewritefmt("unknown option '%s'\n", badopt);
    } else {
        fp = stdout;
    }
    cs_writefmt(fp,
    "usage: %s [options] [script [args]]\n"
    "Available options are:\n"
    "   -s str      execute string 'str'\n"
    "   -i          enter interactive mode after executing 'script'\n"
    "   -v          show version information\n"
    "   -w          turn warnings on\n"
    "   -h          show help (this)\n"
    "   --          stop handling options\n"
    "   -           stop handling options and execute stdin\n",
    progname);
}


static void printversion(void) {
    cs_writelen(stdout, CS_COPYRIGHT, strlen(CS_COPYRIGHT));
    cs_writeline(stdout);
}


/* write 'msg' to 'stderr' */
static void emessage(const char *prog, const char *msg) {
    if (prog)
        ewritefmt("%s: ", prog);
    ewritefmt("%s\n", msg);
}


/* report 'status' if it is error status */
static int report(cs_State *ts, int status) {
    if (status != CS_OK) { /* have errors? */
        const char *msg = cs_to_string(ts, -1);
        if (msg == NULL)
            msg = "(error object not a string)";
        emessage(progname, msg);
        cs_pop(ts, 1); /* error object */
    }
    return status;
}


/* bits for 'parseargs' */
#define arg_error	1   /* bad option */
#define arg_s		2   /* -s */
#define arg_v		4   /* -v */
#define arg_w		8   /* -w */
#define arg_h		16  /* -h */
#define arg_i           32  /* -i */


/* collects arg in 'parseargs' */
#define collectarg(arg,endi) \
    { if (argv[i][(endi)] != '\0') return arg_error; \
      args |= (arg); *first = i + 1; }


static int parseargs(char **argv, int *first) {
    int args = 0;
    if (*argv) {
        if (argv[0][0])
            progname = *argv;
    } else { /* missing program name */
        *first = -1;
        return 0;
    }
    for (int i = 1; argv[i] != NULL; i++) {
        *first = i;
        if (argv[i][0] != '-') /* no more args? */
            return args;
        switch (argv[i][1]) {
            case '\0': return args; /* '-' */
            case '-': collectarg(0, 2); return args; /* -- */
            case 'i': args |= arg_i; /* ('i' implies 'v') */
            /* fall through */
            case 'v': collectarg(arg_v, 2); break; /* -v */
            case 'w': collectarg(arg_w, 2); break; /* -w */
            case 'h': collectarg(arg_h, 2); break; /* -h */
            case 's': { /* -s */
                args |= arg_s;
                if (argv[i][2] == '\0') { /* no concatenated argument? */
                    i++; /* try next 'argv' */
                    if (argv[i] == NULL || argv[i][0] == '-')
                        return arg_error;
                }
                break; /* next iteration returns 'args' */
            }
            default: return arg_error;
        }
    }
    *first = 0; /* no script name */
    return args;
}


/* errfunc for protected calls */
static int errfunc(cs_State *ts) {
    const char *msg = cs_to_string(ts, 1);
    if (msg == NULL) { /* error object is not a string? */
        msg = cs_push_fstring(ts, "(error object is a %s value)",
                                  csL_typename(ts, 0));
    }
    csL_traceback(ts, ts, 1, msg); /* append traceback */
    return 1;
}


static int execs_cript(cs_State *ts, int nargs, int nres) {
    int status;
    int base = cs_gettop(ts) - nargs; /* function index */
    cs_assert(base >= 0);
    cs_push_cfunction(ts, errfunc); /* push 'errfunc' on top */
    cs_insert(ts, base); /* insert 'errfunc' below the function */
    status = cs_pcall(ts, nargs, nres, base);
    cs_remove(ts, base); /* remove 'errfunc' */
    return status;
}


static int runchunk(cs_State *ts, int status) {
    if (status == CS_OK)
        execs_cript(ts, 0, 0);
    return report(ts, status);
}


static int runfile(cs_State *ts, const char *filename) {
    return runchunk(ts, csL_loadfile(ts, filename));
}


static int runstring(cs_State *ts, const char *str, const char *name) {
    return runchunk(ts, csL_loadbuffer(ts, str, strlen(str), name));
}


/* 
** Run options 's' which run CScript code and 'w' options which
** also affect the state.
*/
static int runargs(cs_State *ts, char **argv, int n)  {
    for (int i = 1; i < n; i++) {
        int option = argv[i][1];
        cs_assert(argv[i][0] == '-');
        switch (option) {
            case 's': {
                int status;
                char *extra = argv[i] + 2;
                if (*extra == '\0')
                    extra = argv[++i];
                cs_assert(extra != NULL);
                status = runstring(ts, extra, "(command line)");
                if (status != CS_OK) /* have error? */
                    return 0;
                break;
            }
            case 'w': {
                cs_warning(ts, "@on", 0); /* warnings on */
                break;
            }
            default: break;
        }
    }
    return 1; /* no errors */
}


static int pushargs(cs_State *ts) {
    int i, nargs;
    if (cs_get_global(ts, "arg") != CS_TARRAY)
        csL_error(ts, "'arg' is not an array");
    nargs = cs_len(ts, -1);
    csL_check_stack(ts, nargs + 3, "too many arguments to script");
    for (i = 1; i <= nargs; i++) /* push all args */
        cs_get_index(ts, -i, i - 1);
    cs_remove(ts, -i); /* remove array from the stack */
    return nargs;
}


static int runscript(cs_State *ts, char **argv) {
    int status;
    const char *filename = argv[0];
    if (strcmp(filename, "-") == 0 && strcmp(argv[-1], "--") != 0)
        filename = NULL; /* stdin */
    status = csL_loadfile(ts, filename);
    if (status == CS_OK) {
        int nargs = pushargs(ts);
        status = execs_cript(ts, nargs, CS_MULRET);
    }
    return report(ts, status);
}


/* ------------------------------------------------------------------------
** REPL (read-eval-print loop) {
** ------------------------------------------------------------------------ */

#define PROMPT1     "> "
#define PROMPT2     ">> "

#define CST_MAXLINE     512

#if !defined(cs_readline)
#define cs_readline(ts, buff, prompt) \
    ((void)ts, fputs(prompt, stdout), fflush(stdout), \
       fgets(buff, CST_MAXLINE, stdin) != NULL)
#endif


static inline const char *getprompt(int firstline) {
    return (firstline ? PROMPT1 : PROMPT2);
}


static int addreturn(cs_State *ts) {
    const char *line = cs_to_string(ts, -1);
    const char *retline = cs_push_fstring(ts, "return %s;", line);
    int status = csL_loadbuffer(ts, retline, strlen(retline), "stdin");
    /* stack: [line][retline][result] */
    if (status == CS_OK)
        cs_remove(ts, -2); /* remove 'retline' */
    else
        cs_pop(ts, 2); /* pop result from 'csL_loadbuffer' and 'retline' */
    return status;
}


static int pushline(cs_State *ts, int firstline) {
    char buffer[CST_MAXLINE];
    char *b = buffer;
    size_t len;
    const char *pr = getprompt(firstline);
    if (cs_readline(ts, b, pr) == 0)
        return 0;
    len = strlen(b);
    if (len > 0 && b[len - 1] == '\n')
        b[--len] = '\0';
    cs_push_lstring(ts, b, len);
    return 1;
}


#define EOFTEXT     "<eof>"
#define EOFLEN      (sizeof(EOFTEXT)/sizeof(char) - 1)

/*
** If the status is syntax error, then this message checks the
** error string generated by parser, and if the error message signals
** that error occurred at the end of file, then the expression/statement
** is considered as incomplete.
*/
static int incomplete(cs_State *ts, int status) {
    if (status == CS_ERRSYNTAX) {
        size_t len;
        const char *msg = cs_to_lstring(ts, -1, &len);
        if (len >= EOFLEN && strcmp(msg + len - EOFLEN, EOFTEXT) == 0) {
            cs_pop(ts, 1); /* pop 'msg' */
            return 1;
        }
    }
    return 0;
}


static int multiline(cs_State *ts) {
    for (;;) {
        size_t len;
        const char *line = cs_to_lstring(ts, 0, &len);
        int status = csL_loadbuffer(ts, line, len, "stdin");
        if (!incomplete(ts, status) || !pushline(ts, 0))
            return status;
        cs_push_literal(ts, "\n");
        cs_insert(ts, -2); /* insert newline in between the lines */
        cs_concat(ts, 3);
    }
}


static int loadline(cs_State *ts) {
    int status;
    cs_setntop(ts, 0); /* remove all values */
    if (!pushline(ts, 1))
        return -1;
    if ((status = addreturn(ts)) != CS_OK)
        status = multiline(ts);
    cs_remove(ts, 0); /* remove line */
    cs_assert(cs_gettop(ts) == 0); /* 'csL_loadbuffer' result on top */
    return status;
}


static void printresults(cs_State *ts) {
    int n = cs_gettop(ts);
    if (n > 0) { /* have result to print? */
        csL_check_stack(ts, CS_MINSTACK, "too many results to print");
        cs_get_global(ts, "print");
        cs_insert(ts, 1);
        if (cs_pcall(ts, n, 0, 0) != CS_OK)
            emessage(progname, cs_push_fstring(ts, "error calling 'print' (%s)",
                        cs_to_string(ts, -1)));
    }
}


static void runREPL(cs_State *ts) {
    int status;
    const char *old_progname = progname;
    progname = NULL;
    while ((status = loadline(ts)) != -1) { /* while no empty EOF line */
        if (status == CS_OK) /* line loaded with no errors? */
            status = execs_cript(ts, 0, CS_MULRET);
        if (status == CS_OK) /* script returned without errors? */
            printresults(ts);
        else
            report(ts, status);
    }
    cs_setntop(ts, 0); /* remove all values */
    cs_writeline(stdout);
    progname = old_progname;
}

/* -----------------------------------------------------------------------
** }
** ----------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Interpreter entry (main) {
** ----------------------------------------------------------------------- */

/* create global array 'arg' that holds command line arguments */
static void createargarray(cs_State *ts, char **argv, int argc) {
    cs_push_array(ts);
    for (int i = 0; i < argc; i++) {
        cs_push_string(ts, argv[i]);
        cs_set_index(ts, -2, i);
    }
    cs_set_global(ts, "arg");
}


/*
** Main body of interpereter (called in protected mode).
** Reads all options and handles them all.
*/
static int pmain(cs_State *ts) {
    int argc = cs_to_integer(ts, -2);
    char **argv = cs_to_userdata(ts, -1);
    int script;
    int args = parseargs(argv, &script);
    int optlimit = (script > 0 ? script : argc);
    if (args == arg_error) {
        printusage(argv[script]);
        return 0;
    }
    if (args & arg_h)
        printusage(NULL);
    if (args & arg_v)
        printversion();
    csTR_dumpstack(ts, "before standard libraries are open");
    csL_open_libs(ts); /* open standard libraries */
    createargarray(ts, argv, argc); /* create 'arg' array */
    cs_gc(ts, CS_GCRESTART);
    cs_gc(ts, CS_GCINC, 0, 0, 0);
    if (!runargs(ts, argv, optlimit)) /* have error? */
        return 0;
    if (script > 0) { /* have script file? */
        if (runscript(ts, argv + script) != CS_OK)
            return 0;
    }
    if (args & arg_i) {
        runREPL(ts);
    } else if (script <= 0  && !(args & (arg_s | arg_v | arg_h))) {
        if (cs_stdin_is_tty()) {
            printversion();
            runREPL(ts);
        } else {
            runfile(ts, NULL); /* execute stdin as a file */
        }
    }
    cs_push_bool(ts, 1);
    return 1;
}


int main(int argc, char* argv[]) {
    int status, res;
    cs_State *ts = csL_newstate();
    if (ts == NULL) {
        emessage(progname, "cannot create state: out of memory");
        return EXIT_FAILURE;
    }
    cs_gc(ts, CS_GCSTOP); /* stop until all args are parsed */
    cs_test_hashtable(ts);
    cs_push_cfunction(ts, pmain);
    cs_push_integer(ts, argc);
    cs_push_lightuserdata(ts, argv);
    status = cs_pcall(ts, 2, 1, 0);
    res = cs_to_bool(ts, -1);
    report(ts, status);
    cs_freestate(ts);
    return (res && status == CS_OK ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* -----------------------------------------------------------------------
** END }
** ----------------------------------------------------------------------- */
