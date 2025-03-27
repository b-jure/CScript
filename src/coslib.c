#define CS_LIB

#include "cscript.h"
#include "cauxlib.h"

#include "cslib.h"


static int os_remove(cs_State *C) {
    const char *fname = cs_check_string(C, 0);
    errno = 0;
    return csL_fileresult(C, (remove(fname) != -1), fname);
}


static int os_rename(cs_State *C) {
    const char *old_name = cs_check_string(C, 0);
    const char *new_name = cs_check_string(C, 1);
    errno = 0;
    return csL_fileresult(C, rename(old_name, new_name) == 0, NULL);
}


static const cs_Entry syslib[] = {
    //{"clock",     os_clock},
    //{"date",      os_date},
    //{"difftime",  os_difftime},
    //{"execute",   os_execute},
    //{"exit",      os_exit},
    //{"getenv",    os_getenv},
    {"remove",    os_remove},
    {"rename",    os_rename},
    //{"setlocale", os_setlocale},
    //{"time",      os_time},
    //{"tmpname",   os_tmpname},
    {NULL, NULL}
};


CSMOD_API int csopen_os(cs_State *C) {
    csL_newlib(C, syslib);
    return 1;
}
