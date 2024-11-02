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


#define CR_PROGNAME     "CScript"


#define ewritefmt(fmt,...)      cst_writefmt(stderr, fmt, __VA_ARGS__)


/* assume stdin is a tty */
#define cr_stdin_is_tty()   1


static const char *progname = CR_PROGNAME;


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
    cst_writefmt(fp,
    "usage: %s [options] [script [args]]\n"
    "Available options are:\n"
    "   -s str      execute string 'str'\n"
    "   -v          show version information\n"
    "   -w          turn warnings on\n"
    "   -h, --help  show help (this)\n"
    "   --          stop handling options\n",
    progname);
}


static void printversion(void) {
    cst_writelen(stdout, CR_COPYRIGHT, strlen(CR_COPYRIGHT));
    cst_writeline(stdout);
}


/* write 'msg' to 'stderr' */
static void emessage(const char *prog, const char *msg) {
    if (prog)
        ewritefmt("%s: ", prog);
    ewritefmt("%s\n", msg);
}


/* report 'status' if it is error status */
static int report(cr_State *ts, int status) {
    if (status != CR_OK) { /* have errors? */
        const char *msg = cr_to_string(ts, -1);
        if (msg == NULL)
            msg = "(error object not a string)";
        emessage(progname, msg);
        cr_pop(ts, 1); /* error object */
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
      args |= (arg); *first_arg = i + 1; }


static int parseargs(char **argv, int *first_arg) {
    int args = 0;
    if (*argv) {
        if (argv[0][0])
            progname = *argv;
    } else { /* missing program name */
        *first_arg = -1;
        return 0;
    }
    for (int i = 1; argv[i] != NULL; i++) {
        *first_arg = i;
        if (argv[i][0] != '-') /* no more args? */
            return args;
        switch (argv[i][1]) {
            case '\0': return args; /* '-' */
            case '-': collectarg(0, 2); return args; /* -- */
            case 'i': args |= arg_i; /* FALLTHRU ('i' implies 'v') */
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
    *first_arg = 0; /* no script name */
    return args;
}


/* errfunc for protected calls */
static int errfunc(cr_State *ts) {
    const char *msg = cr_to_string(ts, 1);
    if (msg == NULL) { /* error object is not a string? */
        msg = cr_push_fstring(ts, "(error object is a %s value)",
                                  crL_typename(ts, 0));
    }
    crL_traceback(ts, ts, 1, msg); /* append traceback */
    return 1;
}


static int callcscript(cr_State *ts, int nargs, int nres) {
    int status;
    int base = cr_gettop(ts) - nargs; /* function index */
    cr_assert(base >= 0);
    cr_push_cfunction(ts, errfunc); /* push 'errfunc' on top */
    cr_insert(ts, base); /* insert 'errfunc' below the function */
    status = cr_pcall(ts, nargs, nres, base);
    cr_remove(ts, base); /* remove 'errfunc' */
    return status;
}


static int runchunk(cr_State *ts, int status) {
    if (status == CR_OK)
        callcscript(ts, 0, 0);
    return report(ts, status);
}


static int runfile(cr_State *ts, const char *filename) {
    return runchunk(ts, crL_loadfile(ts, filename));
}


static int runstring(cr_State *ts, const char *str, const char *name) {
    return runchunk(ts, crL_loadbuffer(ts, str, strlen(str), name));
}


/* 
** Run options 's' which run CScript code and 'w' options which
** also affect the state.
*/
static int runargs(cr_State *ts, char **argv, int n)  {
    for (int i = 0; i < n; i++) {
        int option = argv[i][1];
        cr_assert(argv[i][0] == '-');
        switch (option) {
            case 's': {
                int status;
                char *extra = argv[i] + 2;
                if (*extra == '\0')
                    extra = argv[++i];
                cr_assert(extra != NULL);
                status = runstring(ts, extra, "(command line)");
                if (status != CR_OK) /* have error? */
                    return 0;
                break;
            }
            case 'w': {
                cr_warning(ts, "@on", 0); /* warnings on */
                break;
            }
            default: break;
        }
    }
    return 1; /* no errors */
}


static int runscript(cr_State *ts, char **argv) {
    int status;
    const char *filename = argv[0];
    if (strcmp(filename, "-") == 0 && strcmp(argv[-1], "--") != 0)
        filename = NULL; /* stdin */
    status = crL_loadfile(ts, filename);
    if (status == CR_OK) {
        /* TODO: push args after implementing args array */
        status = callcscript(ts, 0, CR_MULRET);
    }
    return report(ts, status);
}


/* ------------------------------------------------------------------------
** REPL (read-eval-print loop) {
** ------------------------------------------------------------------------ */

#define PROMPT1     ">"
#define PROMPT2     ">>"

#define CST_MAXLINE     512

#if !defined(cst_readline)
#define cst_readline(ts, buff, prompt) \
    ((void)ts, fputs(prompt, stdout), fflush(stdout), \
       fgets(buff, CST_MAXLINE, stdin) != NULL)
#endif


static inline const char *getprompt(int firstline) {
    return (firstline ? PROMPT1 : PROMPT2);
}


static int addreturn(cr_State *ts) {
    const char *line = cr_to_string(ts, -1);
    const char *retline = cr_push_fstring(ts, "return %s;", line);
    int status = crL_loadbuffer(ts, retline, strlen(retline), "stdin");
    /* stack: [line][retline][result] */
    if (status == CR_OK)
        cr_remove(ts, -2); /* remove 'retline' */
    else
        cr_pop(ts, 2); /* pop result from 'crL_loadbuffer' and 'retline' */
    return status;
}


static int pushline(cr_State *ts, int firstline) {
    char buffer[CST_MAXLINE];
    char *b = buffer;
    size_t len;
    const char *pr = getprompt(firstline);
    if (cst_readline(ts, b, pr) == 0)
        return 0;
    len = strlen(b);
    if (len > 0 && b[len - 1] == '\n')
        b[--len] = '\0';
    cr_push_lstring(ts, b, len);
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
static int incomplete(cr_State *ts, int status) {
    if (status == CR_ERRSYNTAX) {
        size_t len;
        const char *msg = cr_to_lstring(ts, -1, &len);
        if (len >= EOFLEN && strcmp(msg + len - EOFLEN, EOFTEXT) == 0) {
            cr_pop(ts, 1); /* pop 'msg' */
            return 1;
        }
    }
    return 0;
}


static int multiline(cr_State *ts) {
    for (;;) {
        size_t len;
        const char *line = cr_to_lstring(ts, 0, &len);
        int status = crL_loadbuffer(ts, line, len, "stdin");
        if (!incomplete(ts, status) || !pushline(ts, 0))
            return status;
        cr_push_literal(ts, "\n");
        cr_insert(ts, -2); /* insert newline in between the lines */
        cr_concat(ts, 3);
    }
}


static int loadline(cr_State *ts) {
    int status;
    cr_settop(ts, 0);
    if (!pushline(ts, 1))
        return -1;
    if ((status = addreturn(ts)) != CR_OK)
        status = multiline(ts);
    cr_remove(ts, 0); /* remove line */
    cr_assert(cr_gettop(ts) == 0); /* 'crL_loadbuffer' result on top */
    return status;
}


static void printresults(cr_State *ts) {
    int n = cr_gettop(ts);
    if (n > 0) { /* have result to print? */
        crL_check_stack(ts, CR_MINSTACK, "too many results to print");
        cr_get_global(ts, "print");
        /* TODO: implement print global (in 'core' lib) */
        cr_insert(ts, 1);
        if (cr_pcall(ts, n, 0, 0) != CR_OK)
            emessage(progname, cr_push_fstring(ts, "error calling 'print' (%s)",
                        cr_to_string(ts, -1)));
    }
}


static void runREPL(cr_State *ts) {
    int status;
    const char *old_progname = progname;
    progname = NULL;
    while ((status = loadline(ts)) != -1) { /* while no empty EOF line */
        if (status == CR_OK) /* line loaded with no errors? */
            status = callcscript(ts, 0, CR_MULRET);
        if (status == CR_OK) /* script returned without errors? */
            printresults(ts);
        else
            report(ts, status);
    }
    cr_settop(ts, 0);
    cst_writeline(stdout);
    progname = old_progname;
}

/* -----------------------------------------------------------------------
** }
** ----------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Interpreter entry (main) {
** ----------------------------------------------------------------------- */

/*
** Main body of interpereter (called in protected mode).
** Reads all options and handles them all.
*/
static int pmain(cr_State *ts) {
    int argc = cr_to_integer(ts, -1);
    char **argv = cr_to_userdata(ts, -2);
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
    /* TODO: load libs here ('lib' class and instance) */
    /* TODO: args instance here ('args' class and instance) */
    cr_gc(ts, CR_GCRESTART);
    cr_gc(ts, CR_GCINC, 0, 0, 0);
    if (!runargs(ts, argv, optlimit)) /* have error? */
        return 0;
    if (script > 0) { /* have script file? */
        if (runscript(ts, argv + script) != CR_OK)
            return 0;
    }
    if (args & arg_i) {
        runREPL(ts);
    } else if (script <= 0  && !(args & (arg_s | arg_v | arg_h))) {
        if (cr_stdin_is_tty()) {
            printversion();
            runREPL(ts);
        } else {
            runfile(ts, NULL); /* execute stdin as a file */
        }
    }
    cr_push_bool(ts, 1);
    return 1;
}


int main(int argc, char* argv[]) {
    int status, res;
    cr_State *ts = crL_newstate();
    if (ts == NULL) {
        emessage(progname, "cannot create state: out of memory");
        return EXIT_FAILURE;
    }
    cr_gc(ts, CR_GCSTOP); /* stop until all args are parsed */
    cr_push_cfunction(ts, pmain);
    cr_push_integer(ts, argc);
    cr_push_lightuserdata(ts, argv);
    status = cr_pcall(ts, 2, 1, 0);
    res = cr_to_bool(ts, -1);
    report(ts, status);
    cr_freestate(ts);
    return (res && status == CR_OK ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* -----------------------------------------------------------------------
** }
** ----------------------------------------------------------------------- */
