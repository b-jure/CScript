/*
** cscriptlib.c
** CScript standard libraries
** See Copyright Notice in cscript.h
*/

#define cscriptlib_c
#define CS_LIB

#include "cprefix.h"

#include "cscriptlib.h"
#include "cscriptaux.h"


static const cs_Entry loadedlibs[] = {
    {CS_GNAME, csopen_basic},
    {CS_LOADLIBNAME, csopen_package},
    {CS_STRLIBNAME, csopen_string},
    {CS_MATHLIBNAME, csopen_math},
    {CS_IOLIBNAME, csopen_io},
    {CS_OSLIBNAME, csopen_os},
    {CS_REGLIBNAME, csopen_reg},
    {CS_DBLIBNAME, csopen_debug},
    {CS_LISTLIBNAME, csopen_list},
    {NULL, NULL}
};


CSLIB_API void csL_openlibs(cs_State *C) {
    for (const cs_Entry *lib = loadedlibs; lib->func != NULL; lib++) {
        csL_importf(C, lib->name, lib->func, 1);
        cs_pop(C, 1); /* remove module */
    }
}
