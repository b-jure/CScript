#include "cscript.h"

/* function from lib1.c */
int lib1_export (cs_State *C);

CSMOD_API int csopen_lib11(cs_State *C) {
    return lib1_export(C);
}
