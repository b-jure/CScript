/*
** cscript.c
** CScript Interpreter
** See Copyright Notice in cscript.h
*/

#include <stdio.h>

#include "cscript.h"


#define CR_PROGNAME     "CScript"


static cr_State *gstate = NULL;

static const char *progname = CR_PROGNAME;


#if !defined(cst_writestring)
#define cst_writestring(s, l)   fwrite((s), sizeof(char), (l), stdout)
#endif

#if !defined(cst_writeline)
#define cst_writeline()         (cst_writestring("\n", 1), fflush(stdout))
#endif

#if !defined(cst_writeerror)
#define cst_writeerror(msg)     (fprintf(stderr, msg), fflush(stderr))
#endif

#if !defined(cst_writeferror)
#define cst_writeferror(msg, ...) \
    (fprintf(stderr, msg, __VA_ARGS__), fflush(stderr))
#endif


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


int main(int argc, char* argv[])
{
}
