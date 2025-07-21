/*
** coslib.c
** Standard Operating System library
** See Copyright Notice in tokudae.h
*/

#define toslib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <locale.h>

#include "tokudae.h"
#include "tokudaeaux.h"

#include "tokudaelib.h"


/*
** {=====================================================================
** List of valid conversion specifiers for the 'strftime' function;
** options are grouped by length; group of length 2 start with '||'.
** ======================================================================
*/
#if !defined(TOKU_STRFTIMEOPTIONS)	/* { */

#if defined(TOKU_USE_WINDOWS)
#define TOKU_STRFTIMEOPTIONS  "aAbBcdHIjmMpSUwWxXyYzZ%" \
    "||" "#c#x#d#H#I#j#m#M#S#U#w#W#y#Y"  /* two-char options */
#else /* C99 specification */
#define TOKU_STRFTIMEOPTIONS  "aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%" \
    "||" "EcECExEXEyEY" "OdOeOHOIOmOMOSOuOUOVOwOWOy" /* two-char options */
#endif

#endif					/* } */
/* }===================================================================== */


/*
** {=====================================================================
** Configuration for time-related stuff
** ======================================================================
*/

/* type to represent time_t in Tokudae */
#if !defined(TOKU_NUMTIME)	/* { */

#define t_time			toku_Integer
#define t_push_time(T,t)	toku_push_integer(T,(toku_Integer)(t))
#define t_to_time(T,index)      tokuL_check_integer(T, index)

#else				/* }{ */

#define t_time			toku_Number
#define t_push_time(T,t)	toku_push_number(T,(toku_Number)(t))
#define t_to_time(T,index)	tokuL_check_number(T, index)

#endif				/* } */


#if !defined(t_gmtime)		/* { */

/*
** By default, Tokudae uses gmtime/localtime, except when POSIX is available,
** where it uses gmtime_r/localtime_r
*/

#if defined(TOKU_USE_POSIX)	/* { */

#define t_gmtime(t,r)		gmtime_r(t,r)
#define t_localtime(t,r)	localtime_r(t,r)

#else				/* }{ */

/* ISO C definitions */
#define t_gmtime(t,r)		((void)(r)->tm_sec, gmtime(t))
#define t_localtime(t,r)	((void)(r)->tm_sec, localtime(t))

#endif				/* } */

#endif				/* } */

/* }===================================================================== */


/*
** {=====================================================================
** Configuration for 'tmpnam';
** By default, Tokudae uses tmpnam except when POSIX is available, where
** it uses mkstemp.
** ======================================================================
*/

#if !defined(t_tmpnam)      /* { */

#if defined(TOKU_USE_POSIX)   /* { */

#define T_TMPNAMBUFSZ       32

#if !defined(T_TEMPLATENAME)
#define T_TEMPLATENAME      "/tmp/tokudae_XXXXXX"
#endif

#define t_tmpnam(b, e) \
    { strcpy(b, T_TEMPLATENAME); \
      e = mkstemp(b); \
      if (e != -1) close(e); \
      e = (e == -1); }

#else                       /* }{ */

#define T_TMPNAMBUFSZ       L_tmpnam
#define t_tmpnam(b, e)      { e = (tmpnam(b) == NULL); }

#endif                      /* } */

#endif                      /* } */

/* }===================================================================== */


/*
** {=====================================================================
** Configuration for 'setenv';
** When POSIX is available, Tokudae uses setenv, when on Windows, it uses
** _putenv, otherwise this always returns error result.
** ======================================================================
*/

#if !defined(t_setenv)              /* { */

#if defined(TOKU_USE_WINDOWS)         /* { */

static int t_setenv(toku_State *T, const char *name, const char *value) {
    size_t ln = strlen(name); 
    size_t lv = strlen(value);
    tokuL_Buffer b;
    char *p;
    if (t_unlikely(lv >= lv + ln + 1))
        tokuL_error(T, "\"{name}={value}\" string for 'setenv' is too large");
    p = tokuL_buff_initsz(&b, lv+ln+1);
    /* make "name=value" */
    memcpy(p, name, sizeof(char)*ln);
    p[ln] = '=';
    memcpy(p+ln+1, value, sizeof(char)*lv);
    tokuL_buff_endsz(&b, ln+1+lv);
    /* set the variable */
    return _putenv(toku_to_string(T, -1));
}

#elif defined(TOKU_USE_POSIX)         /* }{ */

static int t_setenv(toku_State *T, const char *name, const char *value) {
    int res;
    (void)(T); /* unused */
    if (*value == '\0' && strlen(value) == 0)
        res = unsetenv(name);
    else
        res = setenv(name, value, 1);
    return res;
}

#else                               /* }{ */

/* ISO C definition */
#define t_setenv(T,name,value) \
        ((void)(name), (void)(value), \
         tokuL_error(T, "cannot set environment in this installation"), -1)

#endif                              /* } */

#endif                              /* } */

/* }===================================================================== */


#if !defined(t_system)
#if defined(TOKU_USE_IOS)
/* iOS does not implement 'system' */
#define t_system(cmd)       ((cmd) == NULL ? 0 : -1)
#else
#define t_system(cmd)       system(cmd)
#endif
#endif


static int os_execute(toku_State *T) {
    const char *cmd = tokuL_opt_string(T, 0, NULL);
    int stat;
    errno = 0;
    stat = t_system(cmd);
    if (cmd != NULL)
        return tokuL_execresult(T, stat);
    else {
        toku_push_bool(T, stat); /* true if there is a shell */
        return 1;
    }
}


static int os_remove(toku_State *T) {
    const char *fname = tokuL_check_string(T, 0);
    errno = 0;
    return tokuL_fileresult(T, (remove(fname) != -1), fname);
}


static int os_rename(toku_State *T) {
    const char *old_name = tokuL_check_string(T, 0);
    const char *new_name = tokuL_check_string(T, 1);
    errno = 0;
    return tokuL_fileresult(T, rename(old_name, new_name) == 0, NULL);
}


static int os_tmpname(toku_State *T) {
    char buff[T_TMPNAMBUFSZ];
    int err;
    t_tmpnam(buff, err);
    if (t_unlikely(err))
        tokuL_error(T, "unable to generate a unique filename");
    toku_push_string(T, buff);
    return 1;
}


static int os_getenv(toku_State *T) {
    toku_push_string(T, getenv(tokuL_check_string(T, 0))); /* if NULL push nil */
    return 1;
}


static int os_setenv(toku_State *T) {
    const char *name = tokuL_check_string(T, 0);
    const char *value = tokuL_opt_string(T, 1, "");
    if (t_setenv(T, name, value) == 0)
        toku_push_bool(T, 1); /* ok */
    else
        tokuL_push_fail(T);
    return 1; /* return nil (fail) or true */
}


static int os_clock(toku_State *T) {
    toku_push_number(T, ((toku_Number)clock()/(toku_Number)CLOCKS_PER_SEC));
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
** is represented by a toku_Integer, because either toku_Integer is
** large enough to represent all int fields or it is not large enough
** to represent a time that cause a field to overflow.  However, if
** times are represented as doubles and toku_Integer is int, then the
** time 0x1.e1853b0d184f6p+55 would cause an overflow when adding 1900
** to compute the year.
*/
static void set_field(toku_State *T, const char *key, int value, int delta) {
    #if (defined(TOKU_NUMTIME) && TOKU_INTEGER_MAX <= INT_MAX)
        if (t_unlikely(value > TOKU_INTEGER_MAX - delta))
            tokuL_error(T, "field '%s' is out-of-bound", key);
    #endif
    toku_push_integer(T, (toku_Integer)value + delta);
    toku_set_fieldstr(T, -2, key);
}


static void set_bool_field(toku_State *T, const char *key, int value) {
    if (value < 0) /* undefined? */
        return; /* does not set field */
    toku_push_bool(T, value);
    toku_set_fieldstr(T, -2, key);
}


/*
** Set all fields from structure 'tm' in the table on top of the stack.
*/
static void set_all_fields(toku_State *T, struct tm *stm) {
    set_field(T, "year", stm->tm_year, 1900);
    set_field(T, "month", stm->tm_mon, 1);
    set_field(T, "day", stm->tm_mday, 0);
    set_field(T, "hour", stm->tm_hour, 0);
    set_field(T, "min", stm->tm_min, 0);
    set_field(T, "sec", stm->tm_sec, 0);
    set_field(T, "yday", stm->tm_yday, 1);
    set_field(T, "wday", stm->tm_wday, 1);
    set_bool_field(T, "isdst", stm->tm_isdst);
}


static int get_bool_field(toku_State *T, const char *key) {
    int res = (toku_get_fieldstr(T, -1, key) == TOKU_T_NIL)
            ? -1
            : toku_to_bool(T, -1);
    toku_pop(T, 1);
    return res;
}


static int get_field(toku_State *T, const char *key, int dfl, int delta) {
    int isnum;
    int t = toku_get_fieldstr(T, -1, key); /* get field and its type */
    toku_Integer res = toku_to_integerx(T, -1, &isnum);
    if (!isnum) { /* field is not an integer? */
        if (t_unlikely(t != TOKU_T_NIL)) /* some other value? */
            return tokuL_error(T, "field '%s' is not an integer", key);
        else if (t_unlikely(dfl < 0)) /* absent field; no default? */
            return tokuL_error(T, "field '%s' missing in date table", key);
        res = dfl;
    } else { /* final field integer must not overflow 'int' */
        if (!(res >= 0 ? res - delta <= INT_MAX : INT_MIN + delta <= res))
            return tokuL_error(T, "field '%s' is out-of-bound", key);
        res -= delta;
    }
    toku_pop(T, 1);
    return (int)res;
}


static const char *check_option(toku_State *T, const char *conv,
                                size_t convlen, char *buff) {
    const char *option = TOKU_STRFTIMEOPTIONS;
    t_uint oplen = 1; /* length of options being checked */
    for (; *option && oplen <= convlen; option += oplen) {
        if (*option == '|')  /* next block? */
            oplen++; /* will check options with next length (+1) */
        else if (memcmp(conv, option, oplen) == 0) { /* match? */
            memcpy(buff, conv, oplen); /* copy valid option to buffer */
            buff[oplen] = '\0';
            return conv + oplen; /* return next item */
        }
    }
    tokuL_error_arg(T, 0,
            toku_push_fstring(T, "invalid conversion specifier '%%%s'", conv));
    return conv; /* to avoid warnings */
}


static time_t t_checktime (toku_State *T, int index) {
    t_time t = t_to_time(T, index);
    tokuL_check_arg(T, (time_t)t == t, index, "time out-of-bounds");
    return (time_t)t;
}


/* maximum size for an individual 'strftime' item */
#define SIZETIMEFMT     250


static int os_date(toku_State *T) {
    size_t slen;
    const char *s = tokuL_opt_lstring(T, 0, "%c", &slen);
    time_t t = tokuL_opt(T, t_checktime, 1, time(NULL));
    const char *send = s + slen; /* 's' end */
    struct tm tmr, *stm;
    if (*s == '!') { /* UTC? */
        stm = t_gmtime(&t, &tmr);
        s++; /* skip '!' */
    } else
        stm = t_localtime(&t, &tmr);
    if (stm == NULL) /* invalid date? */
        return tokuL_error(T,
                "date result cannot be represented in this installation");
    if (s[0] == 't' && s[1] == '\0') {
        toku_push_table(T, 9); /* 9 = number of fields */
        set_all_fields(T, stm);
    } else {
        char cc[4]; /* buffer for individual conversion specifiers */
        tokuL_Buffer b;
        cc[0] = '%';
        tokuL_buff_init(T, &b);
        while (s < send) {
            if (*s != '%')  /* not a conversion specifier? */
                tokuL_buff_push(&b, *s++);
            else {
                size_t reslen;
                char *buff = tokuL_buff_ensure(&b, SIZETIMEFMT);
                s++; /* skip '%' */
                /* copy specifier to 'cc' */
                s = check_option(T, s, cast_sizet(send - s), cc + 1);
                reslen = strftime(buff, SIZETIMEFMT, cc, stm);
                tokuL_buffadd(&b, reslen);
            }
        }
        tokuL_buff_end(&b);
    }
    return 1;
}


static int os_time(toku_State *T) {
    time_t t;
    if (toku_is_noneornil(T, 0)) /* called without args? */
        t = time(NULL); /* get current time */
    else {
        struct tm ts;
        tokuL_check_type(T, 0, TOKU_T_TABLE);
        toku_setntop(T, 1); /* make sure table is at the top */
        ts.tm_year = get_field(T, "year", -1, 1900);
        ts.tm_mon = get_field(T, "month", -1, 1);
        ts.tm_mday = get_field(T, "day", -1, 0);
        ts.tm_hour = get_field(T, "hour", 12, 0);
        ts.tm_min = get_field(T, "min", 0, 0);
        ts.tm_sec = get_field(T, "sec", 0, 0);
        ts.tm_isdst = get_bool_field(T, "isdst");
        t = mktime(&ts);
        set_all_fields(T, &ts); /* update fields with normalized values */
    }
    if (t != (time_t)(t_time)t || t == (time_t)(-1))
        return tokuL_error(T,
                "time result cannot be represented in this installation");
    t_push_time(T, t);
    return 1;
}


static int os_difftime(toku_State *T) {
    time_t t1 = t_checktime(T, 0);
    time_t t2 = t_checktime(T, 1);
    toku_push_number(T, (toku_Number)difftime(t1, t2));
    return 1;
}

/* }====================================================== */


static int os_exit(toku_State *T) {
    int status;
    if (toku_is_bool(T, 0))
        status = (toku_to_bool(T, 0) ? EXIT_SUCCESS : EXIT_FAILURE);
    else
        status = (int)tokuL_opt_integer(T, 0, EXIT_SUCCESS);
    if (toku_to_bool(T, 1))
        toku_close(T); /* close the state before exiting */
    if (T) exit(status); /* 'if' to avoid warnings for unreachable 'return' */
    return 0;
}


static int os_setlocale (toku_State *T) {
    static const int cat[] = {
        LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY, LC_NUMERIC, LC_TIME };
    static const char *const catnames[] = {
        "all", "collate", "ctype", "monetary", "numeric", "time", NULL };
    const char *l = tokuL_opt_string(T, 0, NULL);
    int opt = tokuL_check_option(T, 1, "all", catnames);
    toku_push_string(T, setlocale(cat[opt], l));
    return 1;
}


static const tokuL_Entry syslib[] = {
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


TOKUMOD_API int tokuopen_os(toku_State *T) {
    tokuL_push_lib(T, syslib);
    return 1;
}
