/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/


#define CS_LIB


#include "cslib.h"
#include "cauxlib.h"


static const cs_Entry loadedlibs[] = {
    {CS_GNAME, csopen_basic},
    {CS_LOADLIBNAME, csopen_package},
    {CS_STRLIBNAME, csopen_string},
    {NULL, NULL}
};


CSLIB_API void csL_openlibs(cs_State *C) {
    for (const cs_Entry *lib = loadedlibs; lib->func != NULL; lib++) {
        csL_importf(C, lib->name, lib->func, 1);
        cs_pop(C, 1); /* remove module */
    }
}
