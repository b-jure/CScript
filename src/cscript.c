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


static int callcscript(cr_State *ts, int nargs, int nres) {
    // int base = cr_gettop(ts) - nargs;  /* function index */
    /* TODO: add error func both in core and here */
    return cr_pcall(ts, nargs, nres);
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
** REPL (read-eval-print loop)
** ------------------------------------------------------------------------ */

#define PROMPT1     ">"
#define PROMPT2     ">>"

#define MAXLINE     512

#if !defined(cst_readline)

#define cst_readline(ts, buff, prompt) \
    ((void)ts, fputs(prompt, stdout), fflush(stdout), \
       fgets(buff, MAXLINE, stdin) != NULL)

#endif


static const char *getprompt(int firstline) {
    return (firstline ? PROMPT1 : PROMPT2);
}


static void loadline(cr_State *ts) {
    cr_settop(ts, 0);
}


static void runrepl(cr_State *ts) {
}


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
        runrepl(ts);
    } else if (script <= 0  && !(args & (arg_s | arg_v | arg_h))) {
        if (cr_stdin_is_tty()) {
            printversion();
            runrepl(ts);
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
    status = cr_pcall(ts, 2, 1);
    res = cr_to_bool(ts, -1);
    report(ts, status);
    cr_freestate(ts);
    return (res && status == CR_OK ? EXIT_SUCCESS : EXIT_FAILURE);
}
