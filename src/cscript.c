/*
** cscript.c
** CScript Interpreter
** See Copyright Notice in cscript.h
*/

#define cscript_c

#include "cprefix.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "climits.h"


#if !defined(CS_PROGNAME)
#define CS_PROGNAME         "cscript"
#endif


#define ewritefmt(fmt,...)      cs_writefmt(stderr, fmt, __VA_ARGS__)


static const char *progname = CS_PROGNAME;


static void print_usage(const char *badopt) {
    FILE *fp;
    if (badopt) {
        fp = stderr;
        if (badopt[1] == 's' || badopt[1] == 'l')
            ewritefmt("option '%s' needs argument\n", badopt);
        else
            ewritefmt("unknown option '%s'\n", badopt);
    } else fp = stdout;
    cs_writefmt(fp,
    "usage: %s [options] [script [args]]\n"
    "Available options are:\n"
    "   -s str      execute string 'str'\n"
    "   -i          enter interactive mode after executing 'script'\n"
    "   -l mod      require library 'mod' into global 'mod'\n"
    "   -l g=mod    require library 'mod' into global 'g'\n"
    "   -v          show version information\n"
    "   -w          turn warnings on\n"
    "   -h          show help (this)\n"
    "   --          stop handling options\n"
    "   -           stop handling options and execute stdin\n",
    progname);
}


static void print_version(void) {
    cs_writelen(stdout, CS_COPYRIGHT, sizeof(CS_COPYRIGHT)-1);
    cs_writeline(stdout);
}


/* write 'msg' to 'stderr' */
static void errmsg(const char *prog, const char *msg) {
    if (prog) ewritefmt("%s: ", prog);
    ewritefmt("%s\n", msg);
}


/* report 'status' if it is error status */
static int report(cs_State *C, int status) {
    if (status != CS_STATUS_OK) { /* have errors? */
        const char *msg = cs_to_string(C, -1);
        if (msg == NULL) msg = "(error object not a string)";
        errmsg(progname, msg);
        cs_pop(C, 1); /* error object */
    }
    return status;
}


/* command line args */
#define arg_error	1   /* bad option */
#define arg_s		2   /* -s */
#define arg_l		4   /* -l */
#define arg_v		8   /* -v */
#define arg_w		16  /* -w */
#define arg_h		32  /* -h */
#define arg_i           64  /* -i */

/* collects arg in 'colectargs' */
#define collectarg(arg,endi) \
    { if (argv[i][(endi)] != '\0') return arg_error; \
      args |= (arg); *first = i + 1; }

/*
** Traverses all arguments from 'argv', returning a mask with those
** needed before running any CScript code or an error code if it finds
** any invalid argument. In case of error, 'first' is the index of the
** bad argument. Otherwise 'first' is -1 if there is no program name,
** 0 if there is no script name, or the index of the script name.
*/
static int collect_args(char **argv, int *first) {
    int args = 0;
    if (*argv) { /* is there a program name? */
        if (argv[0][0]) /* not empty? */
            progname = *argv; /* save it */
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
            case 'l': args |= arg_l; goto l_s; /* -l */
            case 's': args |= arg_s; /* -s */
            l_s: { /* both options need an argument */
                if (argv[i][2] == '\0') { /* no concatenated argument? */
                    i++; /* try next 'argv' */
                    if (argv[i] == NULL || argv[i][0] == '-') {
                        /* no next argument or it is another options */
                        return arg_error; 
                    }
                }
                break;
            }
            default: /* invalid option */
                return arg_error;
        }
    }
    *first = 0; /* no script name */
    return args;
}


/*
** Message handler for protected calls.
*/
static int msghandler(cs_State *C) {
    const char *msg = cs_to_string(C, -1);
    if (msg == NULL) { /* error object is not a string? */
        if (csL_callmeta(C, -1, CS_MM_TOSTRING) && /* it has a metamethod, */
            cs_type(C, -1) == CS_T_STRING) /* that produces a string? */
            return 1; /* that is the message */
        else
            msg = cs_push_fstring(C, "(error object is a %s value)",
                                     csL_typename(C, -1));
    }
    csL_traceback(C, C, 1, msg); /* append traceback */
    return 1; /* return the traceback */
}


static int docall(cs_State *C, int nargs, int nres) {
    int status;
    int base = cs_gettop(C) - nargs; /* function index */
    cs_assert(base >= 0);
    cs_push_cfunction(C, msghandler); /* push 'msghandler' on top */
    cs_insert(C, base); /* insert 'msghandler' below the function */
    status = cs_pcall(C, nargs, nres, base);
    cs_remove(C, base); /* remove 'msghandler' */
    return status;
}


static int run_chunk(cs_State *C, int status) {
    if (status == CS_STATUS_OK) status = docall(C, 0, 0);
    return report(C, status);
}


static int run_file(cs_State *C, const char *filename) {
    return run_chunk(C, csL_loadfile(C, filename));
}


static int run_string(cs_State *C, const char *str, const char *name) {
    return run_chunk(C, csL_loadbuffer(C, str, strlen(str), name));
}


/*
** Receives 'globname[=modname]' and runs 'globname = import(modname)'.
** If there is no explicit modname and globname contains a '-', cut
** the suffix after '-' (the "version") to make the global name.
** The '-' might be something different, depends on the value of CS_IGMARK.
*/
static int run_library(cs_State *C, char *globname) {
    int status;
    char *suffix = NULL;
    char *modname = strchr(globname, '=');
    if (modname == NULL) { /* no explicit name? */
        modname = globname; /* module name is equal to global name */
        suffix = strchr(modname, *CS_IGMARK); /* look for a suffix mark */
    } else {
        *modname = '\0'; /* global name ends here */
        modname++; /* module name starts after the '=' */
    }
    cs_get_global(C, "import");
    cs_push_string(C, modname);
    status = docall(C, 1, 1); /* call 'import(modname)' */
    if (status == CS_STATUS_OK) {
        if (suffix != NULL) /* is there a suffix mark? */
            *suffix = '\0'; /* remove suffix from global name */
        cs_set_global(C, globname); /* globname = require(modname) */
    }
    return report(C, status);
}


/* 
** Run options 's' and 'l', which run CScript code, and 'w' option which,
** also affects the state.
** Returns 0 if some code raises an error.
*/
static int run_args(cs_State *C, char **argv, int n)  {
    for (int i = 1; i < n; i++) {
        int option = argv[i][1];
        cs_assert(argv[i][0] == '-');
        switch (option) {
            case 's': case 'l': {
                int status;
                char *extra = argv[i] + 2; /* both options need an argument */
                if (*extra == '\0') extra = argv[++i];
                cs_assert(extra != NULL);
                status = (option == 's')
                       ? run_string(C, extra, "=(command line)")
                       : run_library(C, extra);
                if (status != CS_STATUS_OK) return 0;
                break;
            }
            case 'w': {
                cs_warning(C, "@on", 0); /* warnings on */
                break;
            }
            default: break;
        }
    }
    return 1; /* no errors */
}


static int pushargs(cs_State *C) {
    int i, nargs;
    if (cs_get_global(C, "args") != CS_T_LIST)
        csL_error(C, "'args' is not an array");
    nargs = cs_len(C, -1);
    csL_check_stack(C, nargs + 3, "too many arguments to script");
    for (i = 1; i <= nargs; i++) /* push all args */
        cs_get_index(C, -i, i - 1);
    cs_remove(C, -i); /* remove array from the stack */
    return nargs;
}


static int run_script(cs_State *C, char **argv) {
    int status;
    const char *filename = argv[0];
    if (strcmp(filename, "-") == 0 && strcmp(argv[-1], "--") != 0)
        filename = NULL; /* stdin */
    status = csL_loadfile(C, filename);
    if (status == CS_STATUS_OK) {
        int nargs = pushargs(C);
        status = docall(C, nargs, CS_MULRET);
    }
    return report(C, status);
}


/* {=====================================================================
** REPL (read-eval-print loop)
** ====================================================================== */

#if !defined(CS_PROMPT)
#define PROMPT1     "> "
#define PROMPT2     ">> "
#endif


#if !defined(CS_MAXINPUT)
#define CS_MAXINPUT     512
#endif


#if !defined(cs_stdin_is_tty)       /* { */

#if defined(CS_USE_POSIX)           /* { */

#include <unistd.h>
#define cs_stdin_is_tty()   isatty(STDIN_FILENO)

#elif defined(CS_USE_WINDOWS)       /* }{ */

#include <io.h>
#include <windows.h>

#define cs_stdin_is_tty()   _isatty(_fileno(stdin))

#else                               /* }{ */

/* ISO C definition */
#define cs_stdin_is_tty()   1  /* assume stdin is a tty */

#endif                              /* } */

#endif                              /* } */


/*
** cs_readline defines how to show a prompt and then read a line from
** the standard input.
** cs_saveline defines how to "save" a read line in a "history".
** cs_freeline defines how to free a line read by cs_readline.
*/
#if !defined(cs_readline)	/* { */

#if defined(CS_USE_READLINE)	/* { */

#include <readline/readline.h>
#include <readline/history.h>
#define cs_initreadline(C)	((void)C, rl_readline_name="cscript")
#define cs_readline(C,b,p)	((void)C, ((b)=readline(p)) != NULL)
#define cs_saveline(C,line)	((void)C, add_history(line))
#define cs_freeline(C,b)	((void)C, free(b))

#else				/* }{ */

#define cs_initreadline(C)  ((void)C)
#define cs_readline(C, buff, prompt) \
        ((void)C, fputs(prompt, stdout), fflush(stdout), /* show prompt */ \
        fgets(buff, CS_MAXINPUT, stdin) != NULL) /* get line */
#define cs_saveline(C,line)	{ (void)C; (void)line; }
#define cs_freeline(C,b)	{ (void)C; (void)b; }

#endif				/* } */

#endif				/* } */

#if !defined(cs_readline)
#endif


static const char *getprompt(int firstline) {
    return (firstline ? PROMPT1 : PROMPT2);
}


/*
** Prompt the user, read a line, and push it into the CScript stack.
*/
static int pushline(cs_State *C, int firstline) {
    char buffer[CS_MAXINPUT];
    char *b = buffer;
    size_t len;
    const char *pr = getprompt(firstline);
    if (cs_readline(C, b, pr) == 0)
        return 0; /* no input */
    len = strlen(b);
    if (len > 0 && b[len - 1] == '\n') /* line ends with newline? */
        b[--len] = '\0'; /* remove it */
    cs_push_lstring(C, b, len);
    cs_freeline(C, b);
    return 1;
}


/*
** Try to compile line on the stack as 'return <line>;'; on return, stack
** has either compiled chunk or original line (if compilation failed).
*/
static int addreturn(cs_State *C) {
    const char *line = cs_to_string(C, -1); /* original line */
    const char *retline = cs_push_fstring(C, "return %s;", line);
    int status = csL_loadbuffer(C, retline, strlen(retline), "=stdin");
    /* stack: [line][retline][result] */
    if (status == CS_STATUS_OK) {
        cs_remove(C, -2); /* remove modified line ('retline') */
        if (line[0] != '\0') /* not empty? */
            cs_saveline(C, line); /* keep history */
    } else
        cs_pop(C, 2); /* pop result from 'csL_loadbuffer' and 'retline' */
    return status;
}


#define EOFTEXT     "<eof>"
#define EOFLEN      (sizeof(EOFTEXT)/sizeof(char) - 1)

/*
** If the status is syntax error, then this message checks the
** error string generated by parser, and if the error message signals
** that error occurred at the end of file, then the expression/statement
** is considered as incomplete.
*/
static int incomplete(cs_State *C, int status) {
    if (status == CS_STATUS_ESYNTAX) {
        size_t len;
        const char *msg = cs_to_lstring(C, -1, &len);
        if (len >= EOFLEN && strcmp(msg + len - EOFLEN, EOFTEXT) == 0) {
            cs_pop(C, 1); /* pop 'msg' */
            return 1;
        }
    }
    return 0; /* else... */
}


/*
** Read multiple lines until a complete CScript declaration/statement.
*/
static int multi_line(cs_State *C) {
    for (;;) { /* repeat until complete declaration */
        size_t len;
        const char *line = cs_to_lstring(C, 0, &len);
        int status = csL_loadbuffer(C, line, len, "=stdin");
        if (!incomplete(C, status) || !pushline(C, 0)) {
            cs_saveline(C, line); /* keep history */
            return status; /* cannot or should not try to add continuation */
        }
        cs_push_literal(C, "\n"); /* add newline... */
        cs_insert(C, -2); /* ...between the two lines */
        cs_concat(C, 3); /* join them */
    }
}


/*
** Read a line and try to load (compile) it first as an expression (by
** adding "return " in front of it) and second as a statement. Return
** the final status of load/call with the resulting function (if any)
** in the top of the stack.
*/
static int load_line(cs_State *C) {
    int status;
    cs_setntop(C, 0); /* remove all values */
    if (!pushline(C, 1))
        return -1; /* no input */
    if ((status = addreturn(C)) != CS_STATUS_OK) /* 'return ...;' did not work? */
        status = multi_line(C); /* try as command, maybe with continuation lines */
    cs_remove(C, 0); /* remove line from the stack */
    cs_assert(cs_getntop(C) == 1); /* 'csL_loadbuffer' result on top */
    return status;
}


/*
** Prints (calling the CScript 'print' function) any values on the stack.
*/
static void print_result(cs_State *C) {
    int n = cs_getntop(C);
    if (n > 0) { /* have result to print? */
        csL_check_stack(C, CS_MINSTACK, "too many results to print");
        cs_get_global(C, "print");
        cs_insert(C, 0);
        if (cs_pcall(C, n, 0, -1) != CS_STATUS_OK)
            errmsg(progname, cs_push_fstring(C, "error calling 'print' (%s)",
                             cs_to_string(C, -1)));
    }
}


/*
** Run the REPL: repeatedly read (load) a line, evaluate (call) it, and
** print any results.
*/
static void run_repl(cs_State *C) {
    int status;
    const char *old_progname = progname;
    progname = NULL;
    while ((status = load_line(C)) != -1) { /* while no empty EOF line */
        if (status == CS_STATUS_OK) /* line loaded with no errors? */
            status = docall(C, 0, CS_MULRET);
        if (status == CS_STATUS_OK) /* script returned without errors? */
            print_result(C);
        else
            report(C, status);
    }
    cs_setntop(C, 0); /* remove all values */
    cs_writeline(stdout);
    progname = old_progname;
}

/* }===================================================================== */



/* {=====================================================================
** Interpreter entry (main)
** ====================================================================== */

/* create global array 'args' that holds command line arguments */
static void create_args_array(cs_State *C, char **argv, int argc) {
    cs_push_list(C, argc);
    for (int i = 0; i < argc; i++) {
        cs_push_string(C, argv[i]);
        cs_set_index(C, -2, i);
    }
    cs_set_global(C, "args");
}


/*
** Main body of interpereter (called in protected mode).
** Reads all options and handles them all.
*/
static int pmain(cs_State *C) {
    int argc = cs_to_integer(C, -2);
    char **argv = cs_to_userdata(C, -1);
    int script;
    int args = collect_args(argv, &script);
    int optlimit = (script > 0 ? script : argc);
    csL_check_version(C); /* check that the interpreter has correct version */
    if (args == arg_error) { /* bad arg? */
        print_usage(argv[script]); /* 'script' has index of bad arg. */
        return 0;
    }
    if (args & arg_h) { /* option '-h'? */
        print_usage(NULL); /* print usage (help) */
        goto end; /* and return */
    }
    if (args & arg_v) /* option '-v'? */
        print_version(); /* print version with copyright */
    csL_openlibs(C); /* open standard libraries */
    create_args_array(C, argv, argc); /* create 'args' array */
    cs_gc(C, CS_GC_RESTART); /* start GC... */
    cs_gc(C, CS_GC_INC, 0, 0, 0); /* ...in incremental mode */
    if (!run_args(C, argv, optlimit)) /* execute arguments -s and -l */
        return 0; /* something failed */
    if (script > 0) { /* execute main script (if there is one) */
        if (run_script(C, argv + script) != CS_STATUS_OK)
            return 0; /* interrupt in case of error */
    }
    if (args & arg_i) /* '-i' option? */
        run_repl(C);
    else if (script < 1 && !(args & (arg_s | arg_l | arg_v))) {
        /* no active option */
        if (cs_stdin_is_tty()) { /* running in interactive mode? */
            print_version();
            run_repl(C); /* run read-eval-print loop */
        } else
            run_file(C, NULL); /* execute stdin as a file */
    }
end:
    cs_push_bool(C, 1); /* signal no errors */
    return 1;
}


int main(int argc, char* argv[]) {
    int status, res;
    cs_State *C = csL_newstate();
    if (C == NULL) {
        errmsg(progname, "cannot create state: out of memory");
        return EXIT_FAILURE;
    }
    cs_gc(C, CS_GC_STOP); /* stop GC while building state */
    cs_push_cfunction(C, &pmain); /* to call 'pmain' in protected mode */
    cs_push_integer(C, argc); /* 1st argument */
    cs_push_lightuserdata(C, argv); /* 2nd argument */
    status = cs_pcall(C, 2, 1, -1); /* do the call */
    res = cs_to_bool(C, -1); /* get result */
    report(C, status);
    cs_close(C);
    return (res && status == CS_STATUS_OK ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* }===================================================================== */
