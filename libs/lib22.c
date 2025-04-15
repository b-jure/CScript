#include "cscript.h"
#include "cscriptaux.h"
#include <stdio.h>

static int id(cs_State *C) {
    cs_push_bool(C, 1);
    cs_insert(C, 0);
    return cs_getntop(C);
}


static const struct cs_Entry funcs[] = {
    {"id", id},
    {NULL, NULL}
};


CSMOD_API int csopen_lib2(cs_State *C) {
    cs_settop(C, 2);
    cs_set_global(C, "y"); /* y gets 2nd parameter */
    cs_set_global(C, "x"); /* x gets 1st parameter */
    csL_push_lib(C, funcs);
    return 1;
}
