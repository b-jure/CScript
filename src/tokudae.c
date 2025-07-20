/*
** tokudae.c
** Tokudae Interpreter
** See Copyright Notice in tokudae.h
*/

#define tokudae_c

#include "tokudaeprefix.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <signal.h>

#include "tokudae.h"

#include "tokudaeaux.h"
#include "tokudaelib.h"
#include "tokudaelimits.h"


#if !defined(TOKU_PROGNAME)
#define TOKU_PROGNAME         "tokudae"
#endif


#if !defined(TOKU_INIT_VAR)
#define TOKU_INIT_VAR		"TOKU_INIT"
#endif

#define TOKU_INITVARVERSION	TOKU_INIT_VAR TOKU_VERSUFFIX


#define ewritefmt(fmt,...)      toku_writefmt(stderr, fmt, __VA_ARGS__)


static toku_State *globalT = NULL;

static const char *progname = TOKU_PROGNAME;


#if defined(TOKU_USE_POSIX)       /* { */

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
static void cstop(toku_State *T, toku_Debug *ar) {
    (void)ar;  /* unused arg. */
    toku_sethook(T, NULL, 0, 0);  /* reset hook */
    tokuL_error(T, "interrupted!");
}


/*
** Function to be called at a T signal. Because a T signal cannot
** just change a Tokudae state (as there is no proper synchronization),
** this function only sets a hook that, when called, will stop the
** interpreter.
*/
static void caction(int i) {
    int flag = TOKU_MASK_CALL | TOKU_MASK_RET | TOKU_MASK_LINE | TOKU_MASK_COUNT;
    setsignal(i, SIG_DFL); /* if another SIGINT happens, terminate process */
    toku_sethook(globalT, cstop, flag, 1);
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
    toku_writefmt(fp,
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
    toku_writelen(stdout, TOKU_COPYRIGHT, sizeof(TOKU_COPYRIGHT)-1);
    toku_writeline(stdout);
}


/* write 'msg' to 'stderr' */
static void errmsg(const char *prog, const char *msg) {
    if (prog) ewritefmt("%s: ", prog);
    ewritefmt("%s\n", msg);
}


/* report 'status' if it is error status */
static int report(toku_State *T, int status) {
    if (status != TOKU_STATUS_OK) { /* have errors? */
        const char *msg = toku_to_string(T, -1);
        if (msg == NULL) msg = "(error object not a string)";
        errmsg(progname, msg);
        toku_pop(T, 1); /* error object */
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
** needed before running any Tokudae code or an error code if it finds
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
static int msghandler(toku_State *T) {
    const char *msg = toku_to_string(T, -1);
    if (msg == NULL) { /* error object is not a string? */
        if (tokuL_callmeta(T, -1, TOKU_MT_TOSTRING) && /* it has a metamethod, */
            toku_type(T, -1) == TOKU_T_STRING) /* that produces a string? */
            return 1; /* that is the message */
        else
            msg = toku_push_fstring(T, "(error object is a %s value)",
                                     tokuL_typename(T, -1));
    }
    tokuL_traceback(T, T, 1, msg); /* append traceback */
    return 1; /* return the traceback */
}


static int docall(toku_State *T, int nargs, int nres) {
    int status;
    int base = toku_gettop(T) - nargs; /* function index */
    toku_assert(base >= 0);
    toku_push_cfunction(T, msghandler); /* push 'msghandler' on top */
    toku_insert(T, base); /* insert 'msghandler' below the function */
    globalT = T; /* to be available to 'caction' */
    setsignal(SIGINT, caction); /* set T-signal handler */
    status = toku_pcall(T, nargs, nres, base);
    setsignal(SIGINT, SIG_DFL); /* reset T-signal handler */
    toku_remove(T, base); /* remove 'msghandler' */
    return status;
}


static int run_chunk(toku_State *T, int status) {
    if (status == TOKU_STATUS_OK) status = docall(T, 0, 0);
    return report(T, status);
}


static int run_file(toku_State *T, const char *filename) {
    return run_chunk(T, tokuL_loadfile(T, filename));
}


static int run_string(toku_State *T, const char *str, const char *name) {
    return run_chunk(T, tokuL_loadbuffer(T, str, strlen(str), name));
}


/*
** Receives 'globname[=modname]' and runs 'globname = import(modname)'.
** If there is no explicit modname and globname contains a '-', cut
** the suffix after '-' (the "version") to make the global name.
** The '-' might be something different, depends on the value of TOKU_IGMARK.
*/
static int run_library(toku_State *T, char *globname) {
    int status;
    char *suffix = NULL;
    char *modname = strchr(globname, '=');
    if (modname == NULL) { /* no explicit name? */
        modname = globname; /* module name is equal to global name */
        suffix = strchr(modname, *TOKU_IGMARK); /* look for a suffix mark */
    } else {
        *modname = '\0'; /* global name ends here */
        modname++; /* module name starts after the '=' */
    }
    toku_get_global(T, "import");
    toku_push_string(T, modname);
    status = docall(T, 1, 1); /* call 'import(modname)' */
    if (status == TOKU_STATUS_OK) {
        if (suffix != NULL) /* is there a suffix mark? */
            *suffix = '\0'; /* remove suffix from global name */
        toku_set_global(T, globname); /* globname = import(modname) */
    }
    return report(T, status);
}


/* 
** Run options 'e' and 'l', which run Tokudae code, and 'w' option which,
** also affects the state.
** Returns 0 if some code raises an error.
*/
static int run_args(toku_State *T, char **argv, int n)  {
    for (int i = 1; i < n; i++) {
        int option = argv[i][1];
        toku_assert(argv[i][0] == '-');
        switch (option) {
            case 'e': case 'l': {
                int status;
                char *extra = argv[i] + 2; /* both options need an argument */
                if (*extra == '\0') extra = argv[++i];
                toku_assert(extra != NULL);
                status = (option == 'e')
                       ? run_string(T, extra, "=(command line)")
                       : run_library(T, extra);
                if (status != TOKU_STATUS_OK) return 0;
                break;
            }
            case 'W': {
                toku_warning(T, "@on", 0); /* warnings on */
                break;
            }
            default: break;
        }
    }
    return 1; /* no errors */
}


static int pushargs(toku_State *T) {
    int i, nargs;
    if (toku_get_global(T, "cliargs") != TOKU_T_LIST)
        tokuL_error(T, "'cliargs' is not a list");
    toku_pop(T, 1); /* remove 'cliargs' */
    if (toku_get_global(T, "args") != TOKU_T_LIST)
        tokuL_error(T, "'args' is not a list");
    nargs = toku_len(T, -1);
    tokuL_check_stack(T, nargs + 3, "too many arguments to script");
    for (i = 1; i <= nargs; i++) /* push all args */
        toku_get_index(T, -i, i - 1);
    toku_remove(T, -i); /* remove list from the stack */
    return nargs;
}


static int run_script(toku_State *T, char **argv) {
    int status;
    const char *filename = argv[0];
    if (strcmp(filename, "-") == 0 && strcmp(argv[-1], "--") != 0)
        filename = NULL; /* stdin */
    status = tokuL_loadfile(T, filename);
    if (status == TOKU_STATUS_OK) {
        int nargs = pushargs(T);
        status = docall(T, nargs, TOKU_MULTRET);
    }
    return report(T, status);
}


static int handle_csinit(toku_State *T) {
    const char *name = "=" TOKU_INITVARVERSION;
    const char *init = getenv(name + 1);
    if (init == NULL) {
        name = "=" TOKU_INIT_VAR;
        init = getenv(name + 1); /* try alternative name */
    }
    if (init == NULL) return TOKU_STATUS_OK;
    else if (init[0] == '@')
        return run_file(T, init+1);
    else
        return run_string(T, init, name);
}


/* {=====================================================================
** REPL (read-eval-print loop)
** ====================================================================== */

#if !defined(TOKU_PROMPT)
#define TOKU_PROMPT1      "> "
#define TOKU_PROMPT2      ">> "
#endif


#if !defined(TOKU_MAXINPUT)
#define TOKU_MAXINPUT     512
#endif


#if !defined(toku_stdin_is_tty)       /* { */

#if defined(TOKU_USE_POSIX)           /* { */

#include <unistd.h>
#define toku_stdin_is_tty()   isatty(STDIN_FILENO)

#elif defined(TOKU_USE_WINDOWS)       /* }{ */

#include <io.h>
#include <windows.h>

#define toku_stdin_is_tty()   _isatty(_fileno(stdin))

#else                               /* }{ */

/* ISO C definition */
#define toku_stdin_is_tty()   1  /* assume stdin is a tty */

#endif                              /* } */

#endif                              /* } */


/*
** toku_readline defines how to show a prompt and then read a line from
** the standard input.
** toku_saveline defines how to "save" a read line in a "history".
** toku_freeline defines how to free a line read by toku_readline.
*/
#if !defined(toku_readline)   /* { */

/* Code to use the readline library, either statically or dynamically linked */

/* pointer to 'readline' function (if any) */
typedef char *(*t_readlineT)(const char *prompt);
static t_readlineT t_readline = NULL;

/* pointer to 'add_history' function (if any) */
typedef void (*t_addhistT)(const char *string);
static t_addhistT t_addhist = NULL;


static char *toku_readline(char *buff, const char *prompt) {
    if (t_readline != NULL) /* is there a 'readline'? */
        return (*t_readline)(prompt); /* use it */
    else { /* emulate 'readline' over 'buff' */
        fputs(prompt, stdout);
        fflush(stdout); /* show prompt */
        return fgets(buff, TOKU_MAXINPUT, stdin); /* read line */
    }
}


static void toku_saveline(const char *line) {
    if (t_addhist != NULL) /* is there an 'add_history'? */
        (*t_addhist)(line); /* use it */
    /* else nothing to be done */
}


static void toku_freeline (char *line) {
    if (t_readline != NULL) /* is there a 'readline'? */
        free(line); /* free line created by it */
    /* else 'toku_readline' used an automatic buffer; nothing to free */
}


#if defined(TOKU_USE_READLINE)	/* { */

/* assume Tokudae will be linked with '-lreadline' */
#include <readline/readline.h>
#include <readline/history.h>

static void toku_initreadline(toku_State *T) {
    UNUSED(T);
    rl_readline_name = "tokudae";
    t_readline = readline;
    t_addhist = add_history;
}

#elif defined(TOKU_USE_DLOPEN) && defined(TOKU_READLINELIB)   /* }{ */

/* try to load 'readline' dynamically */
#include <dlfcn.h>

static void toku_initreadline(toku_State *T) {
    void *lib = dlopen(TOKU_READLINELIB, RTLD_NOW | RTLD_LOCAL);
    if (lib == NULL)
        toku_warning(T, "library '" TOKU_READLINELIB "' not found", 0);
    else {
        const char **name = cast(const char**, dlsym(lib, "rl_readline_name"));
        if (name != NULL)
            *name = "tokudae";
        t_readline = cast(t_readlineT, cast_func(dlsym(lib, "readline")));
        t_addhist = cast(t_addhistT, cast_func(dlsym(lib, "add_history")));
    }
}

#else				/* }{ */

/* no readline; leave function pointers as NULL */
#define toku_initreadline(L)	cast(void, L)

#endif				/* } */

#endif			    /* } */


static const char *getprompt(toku_State *T, int firstline) {
    if (toku_get_global(T, firstline ? "__PROMPT" : "__PROMPT2") == TOKU_T_NIL)
        return (firstline ? TOKU_PROMPT1 : TOKU_PROMPT2); /* use the default */
    else { /* apply 'to_string' over the value */
        const char *p = tokuL_to_lstring(T, -1, NULL);
        toku_remove(T, -2);  /* remove original value */
        return p;
    }
}


/*
** Prompt the user, read a line, and push it into the Tokudae stack.
*/
static int pushline(toku_State *T, int firstline) {
    char buffer[TOKU_MAXINPUT];
    size_t l;
    const char *pr = getprompt(T, firstline);
    char *b = toku_readline(buffer, pr);
    toku_pop(T, 1); /* remove prompt */
    if (b == NULL)
        return 0; /* no input */
    l = strlen(b);
    if (l > 0 && b[l - 1] == '\n') /* line ends with newline? */
        b[--l] = '\0'; /* remove it */
    toku_push_lstring(T, b, l);
    toku_freeline(b);
    return 1;
}


/*
** Try to compile line on the stack as 'return <line>;'; on return, stack
** has either compiled chunk or original line (if compilation failed).
*/
static int addreturn(toku_State *T) {
    const char *line = toku_to_string(T, -1); /* original line */
    const char *retline = toku_push_fstring(T, "return %s;", line);
    int status = tokuL_loadbuffer(T, retline, strlen(retline), "=stdin");
    /* stack: [line][retline][result] */
    if (status == TOKU_STATUS_OK)
        toku_remove(T, -2); /* remove modified line ('retline') */
    else
        toku_pop(T, 2); /* pop result from 'tokuL_loadbuffer' and 'retline' */
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
static int incomplete(toku_State *T, int status) {
    if (status == TOKU_STATUS_ESYNTAX) {
        size_t len;
        const char *msg = toku_to_lstring(T, -1, &len);
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
** Read multiple lines until a complete Tokudae declaration/statement.
*/
static int multiline(toku_State *T) {
    size_t len;
    const char *line = toku_to_lstring(T, 0, &len); /* get first line */
    checklocal(line);
    for (;;) { /* repeat until complete declaration/statement */
        int status = tokuL_loadbuffer(T, line, len, "=stdin");
        if (!incomplete(T, status) || !pushline(T, 0))
            return status; /* cannot or should not try to add continuation */
        toku_remove(T, -2); /* remove error message (from incomplete line) */
        toku_push_literal(T, "\n"); /* add newline... */
        toku_insert(T, -2); /* ...between the two lines */
        toku_concat(T, 3); /* join them */
        line = toku_to_lstring(T, 0, &len);
    }
}


/*
** Read a line and try to load (compile) it first as an expression (by
** adding "return " in front of it) and second as a statement. Return
** the final status of load/call with the resulting function (if any)
** in the top of the stack.
*/
static int loadline(toku_State *T) {
    const char *line;
    int status;
    toku_setntop(T, 0); /* remove all values */
    if (!pushline(T, 1))
        return -1; /* no input */
    if ((status = addreturn(T)) != TOKU_STATUS_OK) /* 'return ...;' did not work? */
        status = multiline(T); /* try as command, maybe with continuation lines */
    line = toku_to_string(T, 0);
    if (*line != '\0') /* non empty line? */
        toku_saveline(line); /* keep history */
    toku_remove(T, 0); /* remove line from the stack */
    toku_assert(toku_getntop(T) == 1); /* 'tokuL_loadbuffer' result on top */
    return status;
}


/*
** Prints (calling the Tokudae 'print' function) any values on the stack.
*/
static void print_result(toku_State *T) {
    int n = toku_getntop(T);
    if (n > 0) { /* have result to print? */
        tokuL_check_stack(T, TOKU_MINSTACK, "too many results to print");
        toku_get_global(T, "print");
        toku_insert(T, 0);
        if (toku_pcall(T, n, 0, -1) != TOKU_STATUS_OK)
            errmsg(progname, toku_push_fstring(T, "error calling 'print' (%s)",
                             toku_to_string(T, -1)));
    }
}


/*
** Run the REPL: repeatedly read (load) a line, evaluate (call) it, and
** print any results.
*/
static void run_repl(toku_State *T) {
    int status;
    const char *old_progname = progname;
    progname = NULL;
    toku_initreadline(T);
    while ((status = loadline(T)) != -1) { /* while no empty EOF line */
        if (status == TOKU_STATUS_OK) /* line loaded with no errors? */
            status = docall(T, 0, TOKU_MULTRET);
        if (status == TOKU_STATUS_OK) /* script returned without errors? */
            print_result(T);
        else
            report(T, status);
    }
    toku_setntop(T, 0); /* remove all values */
    toku_writeline(stdout);
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
static void create_arg_lists(toku_State *T, char **argv, int argc, int script) {
    int i;
    int nargs; /* number of arguments for 'args' list */
    int ncliargs; /* number of arguments for 'cliargs' list */
    if (script == 0) /* no script name? */
        script = argc; /* make it so that 'nargs' is 0 */
    nargs = argc - script; 
    ncliargs = argc - nargs;
    toku_push_list(T, ncliargs); /* make 'cliargs' */
    for (i = 0; i < ncliargs; i++) {
        toku_push_string(T, argv[i]);
        toku_set_index(T, -2, i);
    }
    toku_set_global(T, "cliargs");
    toku_push_list(T, nargs); /* make 'args' */
    for (int j = 0; i < argc; i++) {
        toku_push_string(T, argv[i]);
        toku_set_index(T, -2, j++);
    }
    toku_set_global(T, "args");
}


/*
** Main body of interpereter (called in protected mode).
** Reads all options and handles them all.
*/
static int pmain(toku_State *T) {
    int argc = toku_to_integer(T, -2);
    char **argv = toku_to_userdata(T, -1);
    int script;
    int args = collect_args(argv, &script);
    int optlimit = (script > 0 ? script : argc);
    tokuL_check_version(T); /* check that the interpreter has correct version */
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
        toku_push_bool(T, 1); /* signal for libraries to ignore env. vars. */
        toku_set_cfieldstr(T, "TOKU_NOENV");
    }
    tokuL_openlibs(T); /* open standard libraries */
    create_arg_lists(T, argv, argc, script); /* create 'cliargs' and 'args' */
    toku_gc(T, TOKU_GC_RESTART);        /* start GC... */
    toku_gc(T, TOKU_GC_INC, 0, 0, 0);   /* ...in incremental mode */
    if (!(args & arg_E)) { /* no option '-E'? */
        if (handle_csinit(T) != TOKU_STATUS_OK) /* run TOKU_INIT */
            return 0; /* error running TOKU_INIT */
    }
    if (!run_args(T, argv, optlimit)) /* execute arguments -e and -l */
        return 0; /* something failed */
    if (script > 0) { /* execute main script (if there is one) */
        if (run_script(T, argv + script) != TOKU_STATUS_OK)
            return 0; /* interrupt in case of error */
    }
    if (args & arg_i) /* '-i' option? */
        run_repl(T); /* run read-eval-print loop */
    else if (script < 1 && !(args & (arg_e | arg_v))) {
        /* no active option */
        if (toku_stdin_is_tty()) { /* running in interactive mode? */
            print_version();
            run_repl(T); /* run read-eval-print loop */
        } else
            run_file(T, NULL); /* execute stdin as a file */
    }
end:
    toku_push_bool(T, 1); /* signal no errors */
    return 1;
}


int main(int argc, char* argv[]) {
    int status, res;
    toku_State *T = tokuL_newstate();
    if (T == NULL) {
        errmsg(progname, "cannot create state: out of memory");
        return EXIT_FAILURE;
    }
    toku_gc(T, TOKU_GC_STOP); /* stop GC while building state */
    toku_push_cfunction(T, &pmain); /* to call 'pmain' in protected mode */
    toku_push_integer(T, argc); /* 1st argument */
    toku_push_lightuserdata(T, argv); /* 2nd argument */
    status = toku_pcall(T, 2, 1, -1); /* do the call */
    res = toku_to_bool(T, -1); /* get result */
    report(T, status);
    toku_close(T);
    return (res && status == TOKU_STATUS_OK ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* }===================================================================== */
