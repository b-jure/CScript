/*
** cscript.c
** CScript Interpreter
** See Copyright Notice in cscript.h
*/

#define cscript_c

#include "cscriptprefix.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <signal.h>

#include "cscript.h"

#include "cscriptaux.h"
#include "cscriptlib.h"
#include "cscriptlimits.h"


#if !defined(CS_PROGNAME)
#define CS_PROGNAME         "cscript"
#endif


#if !defined(CS_INIT_VAR)
#define CS_INIT_VAR		"CS_INIT"
#endif

#define CS_INITVARVERSION	CS_INIT_VAR CS_VERSUFFIX


#define ewritefmt(fmt,...)      cs_writefmt(stderr, fmt, __VA_ARGS__)


static cs_State *globalC = NULL;

static const char *progname = CS_PROGNAME;


#if defined(CS_USE_POSIX)       /* { */

/*
** Use 'sigaction' when available.
*/
static void setsignal(int sig, void (*handler)(int)) {
    struct sigaction sa;
    sa.sa_handler = handler;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);  /* do not mask any signal */
    sigaction(sig, &sa, NULL);
}

#else                           /* }{ */

#define setsignal       signal

#endif                          /* } */


/*
** Hook set by signal function to stop the interpreter.
*/
static void cstop(cs_State *C, cs_Debug *ar) {
    (void)ar;  /* unused arg. */
    cs_sethook(C, NULL, 0, 0);  /* reset hook */
    csL_error(C, "interrupted!");
}


/*
** Function to be called at a C signal. Because a C signal cannot
** just change a CScript state (as there is no proper synchronization),
** this function only sets a hook that, when called, will stop the
** interpreter.
*/
static void caction(int i) {
    int flag = CS_MASK_CALL | CS_MASK_RET | CS_MASK_LINE | CS_MASK_COUNT;
    setsignal(i, SIG_DFL); /* if another SIGINT happens, terminate process */
    cs_sethook(globalC, cstop, flag, 1);
}


static void print_usage(const char *badopt) {
    FILE *fp;
    if (badopt) {
        fp = stderr;
        if (badopt[1] == 'e' || badopt[1] == 'l')
            ewritefmt("option '%s' needs argument\n", badopt);
        else
            ewritefmt("unknown option '%s'\n", badopt);
    } else fp = stdout;
    cs_writefmt(fp,
    "usage: %s [options] [script [args]]\n"
    "Available options are:\n"
    "   -e stat     execute string 'stat'\n"
    "   -i          enter interactive mode after executing 'script'\n"
    "   -l mod      import library 'mod' into global 'mod'\n"
    "   -l g=mod    import library 'mod' into global 'g'\n"
    "   -v          show version information\n"
    "   -W          turn warnings on\n"
    "   -E          ignore environment variables\n"
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
#define arg_error	(1<<0) /* bad option */
#define arg_e		(1<<1) /* -e (execute stat) */
#define arg_v		(1<<2) /* -v (show version) */
#define arg_h		(1<<3) /* -h (show help) */
#define arg_i           (1<<4) /* -i (interactive mode after script) */
#define arg_E           (1<<5) /* -E (ignore env. vars) */

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
            case '\0': /* '-' */
                return args;
            case '-': /* '--' */
                if (argv[i][2] != '\0')
                    return arg_error;
                i++;
                *first = argv[i] ? i : 0;
                return args;
            case 'E': /* -E */
                if (argv[i][2] != '\0')
                    return arg_error;
                args |= arg_E;
                break;
            case 'i': /* -i */
                args |= arg_i; /* (-i implies -v) */
                /* fall through */
            case 'v': /* -v */
                if (argv[i][2] != '\0')
                    return arg_error;
                args |= arg_v;
                break;
            case 'W': /* '-W' */
                if (argv[i][2] != '\0')
                    return arg_error;
                break;
            case 'h': /* '-h' */
                if (argv[i][2] != '\0')
                    return arg_error;
                args |= arg_h;
                break;
            case 'e': /* '-e' */
                args |= arg_e;
                /* fall through */
            case 'l': /* '-l' */
                /* both options need an argument */
                if (argv[i][2] == '\0') { /* no concatenated argument? */
                    i++; /* try next 'argv' */
                    if (argv[i] == NULL || argv[i][0] == '-')
                        return arg_error; /* no next arg or it is another opt */
                }
                break;
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
        if (csL_callmeta(C, -1, CS_MT_TOSTRING) && /* it has a metamethod, */
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
    globalC = C; /* to be available to 'caction' */
    setsignal(SIGINT, caction); /* set C-signal handler */
    status = cs_pcall(C, nargs, nres, base);
    setsignal(SIGINT, SIG_DFL); /* reset C-signal handler */
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
        cs_set_global(C, globname); /* globname = import(modname) */
    }
    return report(C, status);
}


/* 
** Run options 'e' and 'l', which run CScript code, and 'w' option which,
** also affects the state.
** Returns 0 if some code raises an error.
*/
static int run_args(cs_State *C, char **argv, int n)  {
    for (int i = 1; i < n; i++) {
        int option = argv[i][1];
        cs_assert(argv[i][0] == '-');
        switch (option) {
            case 'e': case 'l': {
                int status;
                char *extra = argv[i] + 2; /* both options need an argument */
                if (*extra == '\0') extra = argv[++i];
                cs_assert(extra != NULL);
                status = (option == 'e')
                       ? run_string(C, extra, "=(command line)")
                       : run_library(C, extra);
                if (status != CS_STATUS_OK) return 0;
                break;
            }
            case 'W': {
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
    if (cs_get_global(C, "cliargs") != CS_T_LIST)
        csL_error(C, "'cliargs' is not a list");
    cs_pop(C, 1); /* remove 'cliargs' */
    if (cs_get_global(C, "args") != CS_T_LIST)
        csL_error(C, "'args' is not a list");
    nargs = cs_len(C, -1);
    csL_check_stack(C, nargs + 3, "too many arguments to script");
    for (i = 1; i <= nargs; i++) /* push all args */
        cs_get_index(C, -i, i - 1);
    cs_remove(C, -i); /* remove list from the stack */
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


static int handle_csinit(cs_State *C) {
    const char *name = "=" CS_INITVARVERSION;
    const char *init = getenv(name + 1);
    if (init == NULL) {
        name = "=" CS_INIT_VAR;
        init = getenv(name + 1); /* try alternative name */
    }
    if (init == NULL) return CS_STATUS_OK;
    else if (init[0] == '@')
        return run_file(C, init+1);
    else
        return run_string(C, init, name);
}


/* {=====================================================================
** REPL (read-eval-print loop)
** ====================================================================== */

#if !defined(CS_PROMPT)
#define CS_PROMPT1      "> "
#define CS_PROMPT2      ">> "
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
#if !defined(cs_readline)   /* { */

/* Code to use the readline library, either statically or dynamically linked */

/* pointer to 'readline' function (if any) */
typedef char *(*c_readlineT)(const char *prompt);
static c_readlineT c_readline = NULL;

/* pointer to 'add_history' function (if any) */
typedef void (*c_addhistT)(const char *string);
static c_addhistT c_addhist = NULL;


static char *cs_readline(char *buff, const char *prompt) {
    if (c_readline != NULL) /* is there a 'readline'? */
        return (*c_readline)(prompt); /* use it */
    else { /* emulate 'readline' over 'buff' */
        fputs(prompt, stdout);
        fflush(stdout); /* show prompt */
        return fgets(buff, CS_MAXINPUT, stdin); /* read line */
    }
}


static void cs_saveline(const char *line) {
    if (c_addhist != NULL) /* is there an 'add_history'? */
        (*c_addhist)(line); /* use it */
    /* else nothing to be done */
}


static void cs_freeline (char *line) {
    if (c_readline != NULL) /* is there a 'readline'? */
        free(line); /* free line created by it */
    /* else 'cs_readline' used an automatic buffer; nothing to free */
}


#if defined(CS_USE_READLINE)	/* { */

/* assume CScript will be linked with '-lreadline' */
#include <readline/readline.h>
#include <readline/history.h>

static void cs_initreadline(cs_State *C) {
    UNUSED(C);
    rl_readline_name = "cscript";
    c_readline = readline;
    c_addhist = add_history;
}

#elif defined(CS_USE_DLOPEN) && defined(CS_READLINELIB)   /* }{ */

/* try to load 'readline' dynamically */
#include <dlfcn.h>

static void cs_initreadline(cs_State *C) {
    void *lib = dlopen(CS_READLINELIB, RTLD_NOW | RTLD_LOCAL);
    if (lib == NULL)
        cs_warning(C, "library '" CS_READLINELIB "' not found", 0);
    else {
        const char **name = cast(const char**, dlsym(lib, "rl_readline_name"));
        if (name != NULL)
            *name = "cscript";
        c_readline = cast(c_readlineT, cast_func(dlsym(lib, "readline")));
        c_addhist = cast(c_addhistT, cast_func(dlsym(lib, "add_history")));
    }
}

#else				/* }{ */

/* no readline; leave function pointers as NULL */
#define cs_initreadline(L)	cast(void, L)

#endif				/* } */

#endif			    /* } */


static const char *getprompt(cs_State *C, int firstline) {
    if (cs_get_global(C, firstline ? "__PROMPT" : "__PROMPT2") == CS_T_NIL)
        return (firstline ? CS_PROMPT1 : CS_PROMPT2); /* use the default */
    else { /* apply 'to_string' over the value */
        const char *p = csL_to_lstring(C, -1, NULL);
        cs_remove(C, -2);  /* remove original value */
        return p;
    }
}


/*
** Prompt the user, read a line, and push it into the CScript stack.
*/
static int pushline(cs_State *C, int firstline) {
    char buffer[CS_MAXINPUT];
    size_t l;
    const char *pr = getprompt(C, firstline);
    char *b = cs_readline(buffer, pr);
    cs_pop(C, 1); /* remove prompt */
    if (b == NULL)
        return 0; /* no input */
    l = strlen(b);
    if (l > 0 && b[l - 1] == '\n') /* line ends with newline? */
        b[--l] = '\0'; /* remove it */
    cs_push_lstring(C, b, l);
    cs_freeline(b);
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
    if (status == CS_STATUS_OK)
        cs_remove(C, -2); /* remove modified line ('retline') */
    else
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
        if (len >= EOFLEN && strcmp(msg + len - EOFLEN, EOFTEXT) == 0)
            return 1;
    }
    return 0; /* else... */
}


static void checklocal(const char *line) {
    static const size_t szloc = sizeof("local") - 1;
    static const char space[] = " \t";
    line += strspn(line, space); /* skip spaces */
    if (strncmp(line, "local", szloc) == 0 && /* "local"? */
            strchr(space, *(line + szloc)) != NULL) { /* followed by a space? */
        ewritefmt("%s\n",
            "warning: locals do not survive across lines in interactive mode");
    }
}


/*
** Read multiple lines until a complete CScript declaration/statement.
*/
static int multiline(cs_State *C) {
    size_t len;
    const char *line = cs_to_lstring(C, 0, &len); /* get first line */
    checklocal(line);
    for (;;) { /* repeat until complete declaration/statement */
        int status = csL_loadbuffer(C, line, len, "=stdin");
        if (!incomplete(C, status) || !pushline(C, 0))
            return status; /* cannot or should not try to add continuation */
        cs_remove(C, -2); /* remove error message (from incomplete line) */
        cs_push_literal(C, "\n"); /* add newline... */
        cs_insert(C, -2); /* ...between the two lines */
        cs_concat(C, 3); /* join them */
        line = cs_to_lstring(C, 0, &len);
    }
}


/*
** Read a line and try to load (compile) it first as an expression (by
** adding "return " in front of it) and second as a statement. Return
** the final status of load/call with the resulting function (if any)
** in the top of the stack.
*/
static int loadline(cs_State *C) {
    const char *line;
    int status;
    cs_setntop(C, 0); /* remove all values */
    if (!pushline(C, 1))
        return -1; /* no input */
    if ((status = addreturn(C)) != CS_STATUS_OK) /* 'return ...;' did not work? */
        status = multiline(C); /* try as command, maybe with continuation lines */
    line = cs_to_string(C, 0);
    if (*line != '\0') /* non empty line? */
        cs_saveline(line); /* keep history */
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
    cs_initreadline(C);
    while ((status = loadline(C)) != -1) { /* while no empty EOF line */
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

/*
** Create two global lists that hold command line arguments.
** 'cliargs' holds interpreter arguments, such as the name of the
** interpreter executable, flags arguments, up to the script name.
** 'args' holds script name, and all the arguments after it.
*/
static void create_arg_lists(cs_State *C, char **argv, int argc, int script) {
    int i;
    int nargs; /* number of arguments for 'args' list */
    int ncliargs; /* number of arguments for 'cliargs' list */
    if (script == 0) /* no script name? */
        script = argc; /* make it so that 'nargs' is 0 */
    nargs = argc - script; 
    ncliargs = argc - nargs;
    cs_push_list(C, ncliargs); /* make 'cliargs' */
    for (i = 0; i < ncliargs; i++) {
        cs_push_string(C, argv[i]);
        cs_set_index(C, -2, i);
    }
    cs_set_global(C, "cliargs");
    cs_push_list(C, nargs); /* make 'args' */
    for (int j = 0; i < argc; i++) {
        cs_push_string(C, argv[i]);
        cs_set_index(C, -2, j++);
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
    if (args == arg_error) { /* bad argument? */
        print_usage(argv[script]); /* 'script' is index of bad argument */
        return 0;
    }
    if (args & arg_h) { /* option '-h'? */
        print_usage(NULL); /* print usage (help) */
        goto end; /* and return */
    }
    if (args & arg_v) /* option '-v'? */
        print_version(); /* print version with copyright */
    if (args & arg_E) { /* option '-E'? */
        cs_push_bool(C, 1); /* signal for libraries to ignore env. vars. */
        cs_set_cfieldstr(C, "CS_NOENV");
    }
    csL_openlibs(C); /* open standard libraries */
    create_arg_lists(C, argv, argc, script); /* create 'cliargs' and 'args' */
    cs_gc(C, CS_GC_RESTART);        /* start GC... */
    cs_gc(C, CS_GC_INC, 0, 0, 0);   /* ...in incremental mode */
    if (!(args & arg_E)) { /* no option '-E'? */
        if (handle_csinit(C) != CS_STATUS_OK) /* run CS_INIT */
            return 0; /* error running CS_INIT */
    }
    if (!run_args(C, argv, optlimit)) /* execute arguments -e and -l */
        return 0; /* something failed */
    if (script > 0) { /* execute main script (if there is one) */
        if (run_script(C, argv + script) != CS_STATUS_OK)
            return 0; /* interrupt in case of error */
    }
    if (args & arg_i) /* '-i' option? */
        run_repl(C); /* run read-eval-print loop */
    else if (script < 1 && !(args & (arg_e | arg_v))) {
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
