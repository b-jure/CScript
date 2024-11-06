/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/


#include "cslib.h"
#include "cauxlib.h"


static const cs_Entry loadedlibs[] = {
    {CS_CORELIBNAME, csL_open_core},
    {NULL, NULL}
};


CRLIB_API void crL_open_libs(cs_State *ts) {
    for (const cs_Entry *lib = loadedlibs; lib->func; lib++) {
        crL_include(ts, lib->name, lib->func, 1);
        cs_pop(ts, 1); /* remove lib */
    }
}
