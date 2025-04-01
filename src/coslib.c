#define CS_LIB

#include <errno.h>

#include "cscript.h"
#include "cauxlib.h"

#include "cslib.h"


/*
** {======================================================
** 'tmpnam' configuration
** =======================================================
*/

#if !defined(c_tmpnam)      /* { */

#if defined(CS_USE_POSIX)   /* { */

#define C_TMPNAMBUFSZ       32

#if !defined(C_TEMPLATENAME)
#define C_TEMPLATENAME      "/tmp/cs_XXXXXX"
#endif

#define c_tmpnam(b, e) \
    { strcpy(buff, C_TEMPLATENAME); \
      e = mkstemp(C_TEMPLATENAME); \
      if (e != -1) close(e); \
      e = (e == -1); }

#else                       /* } */

#define C_TMPNAMBUFSZ       L_tmpnam
#define c_tmpnam(b, e)      { e = (tmpnam(b) == NULL); }

#endif                      /* } */

#endif                      /* } */

/* }====================================================== */


static int os_remove(cs_State *C) {
    const char *fname = csL_check_string(C, 0);
    errno = 0;
    return csL_fileresult(C, (remove(fname) != -1), fname);
}


static int os_rename(cs_State *C) {
    const char *old_name = csL_check_string(C, 0);
    const char *new_name = csL_check_string(C, 1);
    errno = 0;
    return csL_fileresult(C, rename(old_name, new_name) == 0, NULL);
}


static int os_tmpname(cs_State *C) {
    char buff[C_TMPNAMBUFSZ];
    int err;
    c_tmpnam(buff, err);
    if (c_unlikely(err))
        csL_error(C, "unable to generate a unique filename");
    cs_push_string(C, buff);
    return 1;
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
    {"tmpname",   os_tmpname},
    {NULL, NULL}
};


CSMOD_API int csopen_os(cs_State *C) {
    csL_push_lib(C, syslib);
    return 1;
}
