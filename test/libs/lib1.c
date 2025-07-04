#include "cscript.h"
#include "cscriptaux.h"

static int id(cs_State *C) {
    return cs_getntop(C);
}


static const struct cs_Entry funcs[] = {
    {"id", id},
    {NULL, NULL}
};


/* function used by lib11.c */
CSMOD_API int lib1_export(cs_State *C) {
    cs_push_string(C, "exported");
    return 1;
}


CSMOD_API int onefunction(cs_State *C) {
    csL_check_version(C);
    cs_setntop(C, 2);
    cs_push(C, 0);
    return 2;
}


CSMOD_API int anotherfunc(cs_State *C) {
    csL_check_version(C);
    cs_push_fstring(C, "%d%%%d\n", (int)cs_to_integer(C, 0),
                                   (int)cs_to_integer(C, 1));
    return 1;
} 


CSMOD_API int csopen_lib1_sub(cs_State *C) {
    cs_set_global(C, "y"); /* 2nd arg: extra value (file name) */
    cs_set_global(C, "x"); /* 1st arg: module name */
    csL_push_lib(C, funcs);
    return 1;
}
