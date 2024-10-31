/*
** cscript.c
** CScript Interpreter
** See Copyright Notice in cscript.h
*/

#include <stdio.h>
#include <stdlib.h>

#include "cscript.h"
#include "cauxlib.h"


#define CR_PROGNAME     "CScript"


static cr_State *gstate = NULL;

static const char *progname = CR_PROGNAME;



static void printusage(void) {
    cst_writeferror(
    "usage: %s [options] [script [args]]\n"
    "Available options are:\n"
    "   -s str      execute string 'str'\n"
    "   -v          show version information\n"
    "   -w          turn warnings on\n"
    "   -h, --help  show help (this)\n"
    "   --          stop handling options\n",
    progname);
}


static void printmsg(const char *prog, const char *msg) {
    if (prog)
        cst_writeferror("%s: %s", prog, msg);
    else
        cst_writeerror(msg);
}


static int report(cr_State *ts, int status) {
    if (status != CR_OK) { /* have errors? */
        const char *msg = cr_to_string(ts, -1);
        if (msg == NULL)
            msg = "(error object not a string)";
        printmsg(progname, msg);
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


static int parseargs(char **argv, int *first) {
    if (*argv) {
        if (argv[0][0])
            progname = *argv;
    } else { /* missing program name */
        *first = -1;
        return 0;
    }
    for (int i = 0; argv[i] != NULL; i++) {
        switch (*argv[i]) {
        }
    }
}


static void pmain(cr_State *ts) {
    int args;
    parseargs(ts, &args);
}


int main(int argc, char* argv[]) {
    int status, res;
    cr_State *ts = crL_newstate();
    if (ts == NULL) {
        printmsg(progname, "cannot create state: out of memory");
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
