/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/


#define CS_LIB


#include "cslib.h"
#include "cauxlib.h"


static const cs_Entry loadedlibs[] = {
    {CS_GNAME, csL_open_basic},
    {NULL, NULL}
};


CSLIB_API void csL_open_libs(cs_State *C) {
    for (const cs_Entry *lib = loadedlibs; lib->func; lib++) {
        csL_include(C, lib->name, lib->func, 1);
        cs_pop(C, 1); /* remove module */
    }
}
