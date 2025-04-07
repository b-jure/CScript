#define CS_LIB

#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <locale.h>

#include "cscript.h"
#include "cauxlib.h"

#include "cslib.h"


/*
** {=====================================================================
** List of valid conversion specifiers for the 'strftime' function;
** options are grouped by length; group of length 2 start with '||'.
** ======================================================================
*/
#if !defined(CS_STRFTIMEOPTIONS)	/* { */

#if defined(CS_USE_WINDOWS)
#define CS_STRFTIMEOPTIONS  "aAbBcdHIjmMpSUwWxXyYzZ%" \
    "||" "#c#x#d#H#I#j#m#M#S#U#w#W#y#Y"  /* two-char options */
#else /* C99 specification */
#define CS_STRFTIMEOPTIONS  "aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%" \
    "||" "EcECExEXEyEY" "OdOeOHOIOmOMOSOuOUOVOwOWOy" /* two-char options */
#endif

#endif					/* } */
/* }===================================================================== */


/*
** {=====================================================================
** Configuration for time-related stuff
** ======================================================================
*/

/* type to represent time_t in CScript */
#if !defined(CS_NUMTIME)	/* { */

#define c_time			cs_Integer
#define c_push_time(C,t)	cs_push_integer(C,(cs_Integer)(t))
#define c_to_time(C,index)      csL_check_integer(C, index)

#else				/* }{ */

#define c_time			cs_Number
#define c_push_time(C,t)	cs_push_number(C,(cs_Number)(t))
#define c_to_time(C,index)	csL_check_number(C, index)

#endif				/* } */


#if !defined(c_gmtime)		/* { */

/*
** By default, CScript uses gmtime/localtime, except when POSIX is available,
** where it uses gmtime_r/localtime_r
*/

#if defined(CS_USE_POSIX)	/* { */

#define c_gmtime(t,r)		gmtime_r(t,r)
#define c_localtime(t,r)	localtime_r(t,r)

#else				/* }{ */

/* ISO C definitions */
#define c_gmtime(t,r)		((void)(r)->tm_sec, gmtime(t))
#define c_localtime(t,r)	((void)(r)->tm_sec, localtime(t))

#endif				/* } */

#endif				/* } */

/* }===================================================================== */


/*
** {=====================================================================
** Configuration for 'tmpnam';
** By default, CScript uses tmpnam except when POSIX is available, where
** it uses mkstemp.
** ======================================================================
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

#else                       /* }{ */

#define C_TMPNAMBUFSZ       L_tmpnam
#define c_tmpnam(b, e)      { e = (tmpnam(b) == NULL); }

#endif                      /* } */

#endif                      /* } */

/* }===================================================================== */


/*
** {=====================================================================
** Configuration for 'setenv';
** When POSIX is available, CScript uses setenv, when on Windows, it uses
** _putenv, otherwise this always returns error result.
** ======================================================================
*/

#if !defined(c_setenv)              /* { */

#if defined(CS_USE_WINDOWS)         /* { */

static int c_setenv(cs_State *C, const char *name, const char *value) {
    size_t ln = strlen(name); 
    size_t lv = strlen(value);
    csL_Buffer b;
    char *p;
    if (c_unlikely(lv >= lv + ln + 1))
        csL_error(C, "\"{name}={value}\" string for 'setenv' is too large");
    p = csL_buff_initsz(&b, lv+ln+1);
    /* make "name=value" */
    memcpy(p, name, sizeof(char)*ln);
    p[ln] = '=';
    memcpy(p+ln+1, value, sizeof(char)*lv);
    csL_buff_endsz(&b, ln+1+lv);
    /* set the variable */
    return _putenv(cs_to_string(C, -1));
}

#elif defined(CS_USE_POSIX)         /* }{ */

#define c_setenv(C,name,value)      setenv(name, value, 1)

#else                               /* }{ */

/* ISO C definition */
#define c_setenv(C,name,value)      ((void)(name), (void)(value), -1)

#endif                              /* } */

#endif                              /* } */

/* }===================================================================== */


#if !defined(c_system)
#if defined(CS_USE_IOS)
/* iOS does not implement 'system' */
#define c_system(cmd)       ((cmd) == NULL ? 0 : -1)
#else
#define c_system(cmd)       system(cmd)
#endif
#endif


static int os_execute(cs_State *C) {
    const char *cmd = csL_opt_string(C, 0, NULL);
    int stat;
    errno = 0;
    stat = c_system(cmd);
    if (cmd != NULL)
        return csL_execresult(C, stat);
    else {
        cs_push_bool(C, stat); /* true if there is a shell */
        return 1;
    }
}


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


static int os_getenv(cs_State *C) {
    cs_push_string(C, getenv(csL_check_string(C, 0))); /* if NULL push nil */
    return 1;
}


static int os_setenv(cs_State *C) {
    const char *name = csL_check_string(C, 0);
    const char *value = csL_opt_string(C, 1, "");
    if (c_setenv(C, name, value) == 0)
        cs_push_bool(C, 1); /* ok */
    else
        csL_push_fail(C);
    return 1; /* return nil (fail) or true */
}


static int os_clock(cs_State *C) {
    cs_push_number(C, ((cs_Number)clock()/(cs_Number)CLOCKS_PER_SEC));
    return 1;
}


/*
** {======================================================
** Time/Date operations
** { year=%Y, month=%m, day=%d, hour=%H, min=%M, sec=%S,
**   wday=%w+1, yday=%j, isdst=? }
** 
** ISO C "broken-down time" structure.
**
** struct tm {
**   int tm_sec;    Seconds         [0-60] (1 leap second)
**   int tm_min;    Minutes         [0-59]
**   int tm_hour;   Hours           [0-23]
**   int tm_mday;   Day	            [1-31]
**   int tm_mon;    Month           [0-11]
**   int tm_year;   Year            -1900
**   int tm_wday;   Day of week	    [0-6]
**   int tm_yday;   Days in year    [0-365]
**   int tm_isdst;  DST		    [-1/0/1]
** }
** =======================================================
*/


/*
** About the overflow check: an overflow cannot occur when time
** is represented by a cs_Integer, because either cs_Integer is
** large enough to represent all int fields or it is not large enough
** to represent a time that cause a field to overflow.  However, if
** times are represented as doubles and cs_Integer is int, then the
** time 0x1.e1853b0d184f6p+55 would cause an overflow when adding 1900
** to compute the year.
*/
static void set_field(cs_State *C, const char *key, int value, int delta) {
    #if (defined(CS_NUMTIME) && CS_INTEGER_MAX <= INT_MAX)
        if (c_unlikely(value > CS_INTEGER_MAX - delta))
            csL_error(C, "field '%s' is out-of-bound", key);
    #endif
    cs_push_integer(C, (cs_Integer)value + delta);
    cs_set_fieldstr(C, -2, key);
}


static void set_bool_field(cs_State *C, const char *key, int value) {
    if (value < 0) /* undefined? */
        return; /* does not set field */
    cs_push_bool(C, value);
    cs_set_fieldstr(C, -2, key);
}


/*
** Set all fields from structure 'tm' in the table on top of the stack.
*/
static void set_all_fields(cs_State *C, struct tm *stm) {
    set_field(C, "year", stm->tm_year, 1900);
    set_field(C, "month", stm->tm_mon, 1);
    set_field(C, "day", stm->tm_mday, 0);
    set_field(C, "hour", stm->tm_hour, 0);
    set_field(C, "min", stm->tm_min, 0);
    set_field(C, "sec", stm->tm_sec, 0);
    set_field(C, "yday", stm->tm_yday, 1);
    set_field(C, "wday", stm->tm_wday, 1);
    set_bool_field(C, "isdst", stm->tm_isdst);
}


static int get_bool_field(cs_State *C, const char *key) {
    int res = (cs_get_fieldstr(C, -1, key) == CS_TNIL)
            ? -1
            : cs_to_bool(C, -1);
    cs_pop(C, 1);
    return res;
}


static int get_field(cs_State *C, const char *key, int dfl, int delta) {
    int isnum;
    int t = cs_get_fieldstr(C, -1, key); /* get field and its type */
    cs_Integer res = cs_to_integerx(C, -1, &isnum);
    if (!isnum) { /* field is not an integer? */
        if (c_unlikely(t != CS_TNIL)) /* some other value? */
            return csL_error(C, "field '%s' is not an integer", key);
        else if (c_unlikely(dfl < 0)) /* absent field; no default? */
            return csL_error(C, "field '%s' missing in date table", key);
        res = dfl;
    } else { /* final field integer must not overflow 'int' */
        if (!(res >= 0 ? res - delta <= INT_MAX : INT_MIN + delta <= res))
            return csL_error(C, "field '%s' is out-of-bound", key);
        res -= delta;
    }
    cs_pop(C, 1);
    return (int)res;
}


static const char *check_option(cs_State *C, const char *conv,
                                ptrdiff_t convlen, char *buff) {
    const char *option = CS_STRFTIMEOPTIONS;
    int oplen = 1; /* length of options being checked */
    for (; *option && oplen <= convlen; option += oplen) {
        if (*option == '|')  /* next block? */
            oplen++; /* will check options with next length (+1) */
        else if (memcmp(conv, option, oplen) == 0) { /* match? */
            memcpy(buff, conv, oplen); /* copy valid option to buffer */
            buff[oplen] = '\0';
            return conv + oplen; /* return next item */
        }
    }
    csL_error_arg(C, 0,
            cs_push_fstring(C, "invalid conversion specifier '%%%s'", conv));
    return conv; /* to avoid warnings */
}


static time_t c_checktime (cs_State *C, int index) {
    c_time t = c_to_time(C, index);
    csL_check_arg(C, (time_t)t == t, index, "time out-of-bounds");
    return (time_t)t;
}


/* maximum size for an individual 'strftime' item */
#define SIZETIMEFMT     250


static int os_date(cs_State *C) {
    size_t slen;
    const char *s = csL_opt_lstring(C, 0, "%c", &slen);
    time_t t = csL_opt(C, c_checktime, 1, time(NULL));
    const char *send = s + slen; /* 's' end */
    struct tm tmr, *stm;
    if (*s == '!') { /* UTC? */
        stm = c_gmtime(&t, &tmr);
        s++; /* skip '!' */
    } else
        stm = c_localtime(&t, &tmr);
    if (stm == NULL) /* invalid date? */
        return csL_error(C,
                "date result cannot be represented in this installation");
    if (strcmp(s, "t") == 0) {
        cs_push_table(C, 9); /* 9 = number of fields */
        set_all_fields(C, stm);
    } else {
        char cc[4]; /* buffer for individual conversion specifiers */
        csL_Buffer b;
        cc[0] = '%';
        csL_buff_init(C, &b);
        while (s < send) {
            if (*s != '%')  /* not a conversion specifier? */
                csL_buff_push(&b, *s++);
            else {
                size_t reslen;
                char *buff = csL_buff_ensure(&b, SIZETIMEFMT);
                s++; /* skip '%' */
                /* copy specifier to 'cc' */
                s = check_option(C, s, send - s, cc + 1);
                reslen = strftime(buff, SIZETIMEFMT, cc, stm);
                csL_buffadd(&b, reslen);
            }
        }
        csL_buff_end(&b);
    }
    return 1;
}


static int os_time(cs_State *C) {
    time_t t;
    if (cs_is_noneornil(C, 0)) /* called without args? */
        t = time(NULL); /* get current time */
    else {
        struct tm ts;
        csL_check_type(C, 0, CS_TTABLE);
        cs_settop(C, 1); /* make sure table is at the top */
        ts.tm_year = get_field(C, "year", -1, 1900);
        ts.tm_mon = get_field(C, "month", -1, 1);
        ts.tm_mday = get_field(C, "day", -1, 0);
        ts.tm_hour = get_field(C, "hour", 12, 0);
        ts.tm_min = get_field(C, "min", 0, 0);
        ts.tm_sec = get_field(C, "sec", 0, 0);
        ts.tm_isdst = get_bool_field(C, "isdst");
        t = mktime(&ts);
        set_all_fields(C, &ts); /* update fields with normalized values */
    }
    if (t != (time_t)(c_time)t || t == (time_t)(-1))
        return csL_error(C,
                "time result cannot be represented in this installation");
    c_push_time(C, t);
    return 1;
}


static int os_difftime (cs_State *C) {
    time_t t1 = c_checktime(C, 0);
    time_t t2 = c_checktime(C, 1);
    cs_push_number(C, (cs_Number)difftime(t1, t2));
    return 1;
}

/* }====================================================== */


static int os_exit(cs_State *C) {
    int status;
    if (cs_is_bool(C, 0))
        status = (cs_to_bool(C, 0) ? EXIT_SUCCESS : EXIT_FAILURE);
    else
        status = (int)csL_opt_integer(C, 0, EXIT_SUCCESS);
    if (cs_to_bool(C, 1))
        cs_close(C); /* close the state before exiting */
    if (C) exit(status); /* 'if' to avoid warnings for unreachable 'return' */
    return 0;
}


static int os_setlocale (cs_State *C) {
    static const int cat[] = {
        LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY, LC_NUMERIC, LC_TIME };
    static const char *const catnames[] = {
        "all", "collate", "ctype", "monetary", "numeric", "time", NULL };
    const char *l = csL_opt_string(C, 0, NULL);
    int opt = csL_check_option(C, 1, "all", catnames);
    cs_push_string(C, setlocale(cat[opt], l));
    return 1;
}


// TODO: add documentation, tests are not really viable as many of the
// behvaiour depends on the implementation and or current time...
static const cs_Entry syslib[] = {
    {"clock",     os_clock},
    {"date",      os_date},
    {"difftime",  os_difftime},
    {"execute",   os_execute},
    {"exit",      os_exit},
    {"getenv",    os_getenv},
    {"setenv",    os_setenv},
    {"remove",    os_remove},
    {"rename",    os_rename},
    {"setlocale", os_setlocale},
    {"time",      os_time},
    {"tmpname",   os_tmpname},
    {NULL, NULL}
};


CSMOD_API int csopen_os(cs_State *C) {
    csL_push_lib(C, syslib);
    return 1;
}
