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


static void fwarn(void *ud, const char *msg, int tocont) {
}


int main(int argc, char* argv[]) {
}
