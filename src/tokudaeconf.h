/*
** tokudaeconf.h
** Tokudae configuration
** See Copyright Notice in tokudae.h
*/


#ifndef tokudaeconfig_h
#define tokudaeconfig_h


#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <float.h>
#include <math.h>


/* {====================================================================== 
** Some hard limits to current Tokudae implementation
** ======================================================================= */

#if SIZE_MAX < UINT_MAX
#error SIZE_MAX must be greater or equal UINT_MAX
#endif

#if ((UINT_MAX >> 30) < 3)
#error 'int' has to have at least 32 bits
#endif

/* }===================================================================== */


/* {====================================================================== 
**                      Configuration file for Tokudae.
**                (Tries its best to mimic Lua configuration)
** ======================================================================= */

#if defined(_WIN32) && !defined(_WIN32_WCE)
#define TOKU_USE_WINDOWS    /* enable goodies for regular Windows */
#endif


#if defined(TOKU_USE_WINDOWS)
#define TOKU_DL_DLL         /* enable support for DLL */
#endif


#if defined(TOKU_USE_LINUX)
#define TOKU_USE_POSIX
#define TOKU_USE_DLOPEN
#define TOKU_READLINELIB	    "libreadline.so"
#endif


#if defined(TOKU_USE_MACOSX)
#define TOKU_USE_POSIX
#define TOKU_USE_DLOPEN
#define TOKU_READLINELIB	    "libedit.dylib"
#endif


#if defined(TOKU_USE_IOS)
#define TOKU_USE_POSIX
#define TOKU_USE_DLOPEN
#endif


/* {======================================================================
** Configuration for number types.
** ======================================================================= */

/* 
** @TOKU_INT_TYPE defines the type for Tokudae integers.
** @TOKU_FLOAT_TYPE defines the type for Tokudae floats.
*/

/* predefined options for TOKU_INT_TYPE */
#define TOKU_INT_INT                1
#define TOKU_INT_LONG               2
#define TOKU_INT_LONGLONG           3

/* predefined options for TOKU_FLOAT_TYPE */
#define TOKU_FLOAT_FLOAT            1
#define TOKU_FLOAT_DOUBLE           2
#define TOKU_FLOAT_LONGDOUBLE       3


/* default configuration ('long long' and 'double', for 64-bit) */
#define TOKU_INT_DEFAULT        TOKU_INT_LONGLONG
#define TOKU_FLOAT_DEFAULT      TOKU_FLOAT_DOUBLE


/* types for integers and floats */
#define TOKU_INT_TYPE           TOKU_INT_DEFAULT
#define TOKU_FLOAT_TYPE         TOKU_FLOAT_DEFAULT

/* }===================================================================== */



/* {======================================================================
** Configuration for paths
** ======================================================================= */

/*
** @TOKU_PATH_SEP - is the character that separates templates in a path.
** @TOKU_PATH_MARK - is the string that marks the substitution points in a
** template.
** @TOKU_EXEC_DIR - in a Windows path is replaced by the executable's
** directory.
*/
#define TOKU_PATH_SEP       ";"
#define TOKU_PATH_MARK      "?"
#define TOKU_EXEC_DIR       "!"


/*
** @TOKU_PATH_DEFAULT - is the default path that Tokudae uses to look for
** Tokudae libraries.
** @TOKU_CPATH_DEFAULT - is the default path that Tokudae uses to look for
** C libraries.
** CHANGE them if your machine has a non-conventional directory
** hierarchy or if you want to install your libraries in
** non-conventional directories.
*/

#define TOKU_VDIR       TOKU_VERSION_MAJOR "." TOKU_VERSION_MINOR
#if defined(_WIN32)     /* { */
/*
** In Windows, any exclamation mark ('!') in the path is replaced by the
** path of the directory of the executable file of the current process.
*/
#define TOKU_TDIR       "!\\tokudae\\"
#define TOKU_CDIR       "!\\"
#define TOKU_SHRDIR     "!\\..\\share\\tokudae\\" TOKU_VDIR "\\"

#if !defined(TOKU_PATH_DEFAULT)
#define TOKU_PATH_DEFAULT \
        TOKU_TDIR"?.toku;"  TOKU_TDIR"?\\init.toku;" \
        TOKU_CDIR"?.toku;"  TOKU_CDIR"?\\init.toku;" \
        TOKU_SHRDIR"?.toku;" TOKU_SHRDIR"?\\init.toku;" \
        ".\\?.toku;" ".\\?\\init.toku"
#endif

#if !defined(TOKU_CPATH_DEFAULT)
#define TOKU_CPATH_DEFAULT \
        TOKU_CDIR"?.dll;" \
        TOKU_CDIR"..\\lib\\tokudae\\" TOKU_VDIR "\\?.dll;" \
        TOKU_CDIR"loadall.dll;" ".\\?.dll"
#endif

#else                   /* }{ */

#define TOKU_ROOT       "/usr/local/"
#define TOKU_TDIR      TOKU_ROOT "share/tokudae/" TOKU_VDIR "/"
#define TOKU_CDIR       TOKU_ROOT "lib/tokudae/" TOKU_VDIR "/"

#if !defined(TOKU_PATH_DEFAULT)
#define TOKU_PATH_DEFAULT \
        TOKU_TDIR"?.toku;"  TOKU_TDIR"?/init.toku;" \
        TOKU_CDIR"?.toku;"  TOKU_CDIR"?/init.toku;" \
        "./?.toku;" "./?/init.toku"
#endif

#if !defined(TOKU_CPATH_DEFAULT)
#define TOKU_CPATH_DEFAULT \
        TOKU_CDIR"?.so;" TOKU_CDIR"loadall.so;" "./?.so"
#endif

#endif                  /* } */


/*
** @TOKU_DIRSEP - is the directory separator (for submodules).
** CHANGE it if your machine does not use "/" as the directory separator
** and is not Windows. (On Windows Tokudae automatically uses "\".)
*/
#if !defined(TOKU_DIRSEP)

#if defined(_WIN32)
#define TOKU_DIRSEP     "\\"
#else
#define TOKU_DIRSEP     "/"
#endif

#endif


/*
** @TOKU_IGMARK - is a mark to ignore all after it when building the
** module name (e.g., used to build the tokuopen_ function name).
** Typically, the suffix after the mark is the module version,
** as in "mod-v1.2.so".
*/
#define TOKU_IGMARK     "-"

/* }===================================================================== */



/* {======================================================================
** Marks for exported symbols in the C code
** ======================================================================= */

/*
** @TOKU_API - is a mark for all core API functions.
** @TOKULIB_API - is a mark for all auxiliary library functions.
** @TOKUMOD_API - is a mark for all standard library opening functions.
** CHANGE them if you need to define those functions in some special way.
** For instance, if you want to create one Windows DLL with the core and
** the libraries, you may want to use the following definition (define
** TOKU_BUILD_AS_DLL to get it).
*/
#if defined(TOKU_BUILD_AS_DLL)  /* { */
#if defined(TOKU_CORE) || defined(TOKU_LIB)     /* { */
#define TOKU_API    __declspec(dllexport)
#else                                           /* }{ */
#define TOKU_API    __declspec(dllimport)
#endif                                          /* } */
#else                           /* }{ */
#define TOKU_API    extern
#endif                          /* } */


#define TOKULIB_API     TOKU_API
#define TOKUMOD_API       TOKU_API


/*
** @TOKUI_FUNC - mark for all external functions that are not being exported
** to outside modules.
** @TOKUI_DDEF and @TOKUI_DDEC - are marks for all extern (const) variables,
** none of which to be exported to outside modules (TOKUI_DDEF for
** definitions and TOKUI_DDEC for declarations).
*/
#if defined(__GNUC__) && ((__GNUC__ * 100 + __GNUC_MINOR__) >= 302) \
    && defined(__ELF__)             /* { */
#define TOKUI_FUNC          __attribute__((visibility("internal"))) extern
#else                               /* }{ */
#define TOKUI_FUNC          extern
#endif                              /* } */

#define TOKUI_DEC(dec)      TOKUI_FUNC dec
#define TOKUI_DEF           /* empty */

/* }===================================================================== */



/* {======================================================================
** Configuration for numbers
** ======================================================================= */

/*
** @TOKU_NUMBER - is the Tokudae floating point type.
** @TOKU_NUMBER_FMT - is the format for writing floats.
** @TOKU_NUMBER_FMTLEN - is the additional length modifier when writing floats.
** @t_mathop - allows the addition of an 'l' or 'f' to all math operations.
** @t_floor - floor division.
** @toku_number2str - convert float into string.
** @toku_str2number - convert numeral into float
** @toku_number2integer - converts float to integer or returns 0 if float is
** not withing the range of integer.
*/


#define t_floor(n)      (t_mathop(floor)(n))

#define toku_number2str(s,sz,n) \
        t_snprintf((s), (sz), TOKU_NUMBER_FMT, (TOKU_NUMBER)(n))

#define toku_number2integer(n,p) \
    ((n) >= (TOKU_NUMBER)(TOKU_INTEGER_MIN) && \
     (n) < (TOKU_NUMBER)(TOKU_INTEGER_MAX) && \
     (*(p) = (TOKU_INTEGER)(n), 1))


#if TOKU_FLOAT_TYPE == TOKU_FLOAT_FLOAT             /* { single precision */

#error 'float' as 'TOKU_NUMBER' is not supported.

#elif TOKU_FLOAT_TYPE == TOKU_FLOAT_DOUBLE          /* }{ double precision */

#define TOKU_NUMBER             double

#define TOKU_NUMBER_FMTLEN      ""
#define TOKU_NUMBER_FMT         "%.15g"

#define t_floatatt(n)           (DBL_##n)

#define TOKU_NUMBER_MIN         t_floatatt(MIN)
#define TOKU_NUMBER_MAX         t_floatatt(MAX)

#define TOKU_HUGE_VAL           ((toku_Number)HUGE_VAL)

#define t_mathop(op)            op

#define toku_str2number(s,p)    strtod((s), (p))

#elif TOKU_FLOAT_TYPE == TOKU_FLOAT_LONG_DOUBLE_TYPE

#error 'long double' as 'TOKU_NUMBER' is not supported.

#else                                           /* }{ */

#error Unrecognized or undefined float type.

#endif                                          /* } */


#if !defined(toku_str2number)
#endif


/*
** @TOKU_INTEGER - integer type.
** @TOKU_UNSIGNED - unsigned integer.
** @TOKU_INTEGER_MAX - maximum integer size.
** @TOKU_INTEGER_MIN - minimum integer size.
** @TOKU_UNSIGNED_MAX - maximum unsigned integer size.
** @TOKU_INTEGER_FMTLEN - additional length of modifier when writing integers.
** @toku_integer2str - converts an integer to string.
*/


#define TOKU_UNSIGNED         unsigned TOKU_INTEGER

#define TOKU_INTEGER_FMT      "%" TOKU_INTEGER_FMTLEN "d"

#define toku_integer2str(s,sz,n) \
        t_snprintf((s),(sz),TOKU_INTEGER_FMT,(TOKU_INTEGER)(n))


#if TOKU_INT_TYPE == TOKU_INT_INT               /* { int */

#error 'int' as 'TOKU_INTEGER' is not supported.

#elif TOKU_INT_TYPE == TOKU_INT_LONG            /* }{ long */

#error 'long' as 'TOKU_INTEGER' is not supported.

#elif TOKU_INT_TYPE == TOKU_INT_LONGLONG        /* }{ long long */

#if defined(LLONG_MAX)          /* { */

#define TOKU_INTEGER              long long
#define TOKU_INTEGER_MAX          LLONG_MAX
#define TOKU_INTEGER_MIN          LLONG_MIN

#define TOKU_UNSIGNED_MAX         ULLONG_MAX

#define TOKU_INTEGER_FMTLEN       "ll"

#elif defined(TOKU_USE_WINDOWS)   /* }{ */

#define TOKU_INTEGER              __int64
#define TOKU_INTEGER_MAX          _I64_MAX
#define TOKU_INTEGER_MIN          _I64_MIN

#define TOKU_UNSIGNED_MAX         _UI64_MAX

#define TOKU_INTEGER_FMTLEN       "I64"

#else                           /* }{ */

#error Implementation does not support 'long long'.

#endif                          /* } */

#else

#error Unrecognized or undefined integer type.

#endif                                      /* } */

/* }===================================================================== */



/* {======================================================================
** Dependencies with C99
** ======================================================================= */

/*
** @t_sprintf - is equivalent to 'snprintf'.
*/
#define t_snprintf(s,sz,fmt,...)        snprintf(s, sz, fmt, __VA_ARGS__)


/* 
** @toku_pointer2str - converts a pointer to a string.
*/
#define toku_pointer2str(buff,sz,p)     t_snprintf(buff,sz,"%p",p)


/*
** @toku_number2strx - converts float to a hexadecimal numeral.
*/
#define toku_number2strx(C,b,sz,f,n)  \
        ((void)C, t_snprintf(b,sz,f,(TOKU_NUMBER)(n)))


/*
** @toku_getlocaledecpoint - gets the locale "radix character" (decimal point).
** Change that if you do not want to use C locales. (Code using this
** macro must include the header 'locale.h'.)
*/
#if !defined(toku_getlocaledecpoint)
#define toku_getlocaledecpoint()    (localeconv()->decimal_point[0])
#endif


/*
** @tokui_likely - likely branch to be taken.
** @tokui_unlikely - unlikely branch to be taken.
** Jump prediction macros.
*/
#if !defined(tokui_likely)

#if defined(__GNUC__) && !defined(TOKU_NOBUILTIN)
#define tokui_likely(cond)      __builtin_expect((cond) != 0, 1)
#define tokui_unlikely(cond)    __builtin_expect((cond) != 0, 0)
#else
#define tokui_likely(cond)      cond
#define tokui_unlikely(cond)    cond
#endif

#endif


#if defined(TOKU_CORE) || defined(TOKU_LIB)
/* shorter names for internal use */
#define t_likely(cond)      tokui_likely(cond)
#define t_unlikely(cond)    tokui_unlikely(cond)
#endif

/* }===================================================================== */



/* {======================================================================
** Macros that affect the API and must be stable (that is, must be the
** same when you compile Tokudae and when you compile code that links to
** Tokudae).
** ======================================================================= */

/*
** @TOKUI_MAXSTACK - stack size limit.
** CHANGE it if you need a different limit. This limit is arbitrary;
** its only purpose is to stop Tokudae from consuming unlimited stack
** space (and to reserve some numbers for pseudo-indices).
** (It must fit into max(size_t)/32 and max(int)/2.)
*/
#define TOKUI_MAXSTACK      (1 << 23)


/*
** @TOKU_EXTRASPACE - defines the size of a raw memory associated with
** the Tokudae state with very fast access (memory chunk before state).
** CHANGE if you need a different size.
*/
#define TOKU_EXTRASPACE     sizeof(void *)


/*
** @TOKU_IDSIZE - the maximum size for the description of the source
** of a function in debug information.
** CHANGE it if you want a different size.
*/
#define TOKU_IDSIZE         60


/*
** @TOKUL_BUFFERSIZE is the initial buffer size used by the 'tokudaeaux.h'
** buffer system.
*/
#define TOKUL_BUFFERSIZE    1024


/*
** @TOKUI_MAXALIGN - defines fields that, when used in a union, ensure maximum
** alignment for the other items in that union.
*/
#define TOKUI_MAXALIGN \
        long l; toku_Integer i; double d; toku_Number n; void *p


/* 
** @TOKU_USE_APICHECK turns on several consistency checks on the C API.
** Define it as a help when debugging C code.
*/
#if defined(TOKU_USE_APICHECK)
#include <assert.h>
#define tokui_checkapi(C,e)       assert(e)
#endif

/* }====================================================================== */




/* }====================================================================== */

/*
** Local configuration. You can use this space to add your redefinitions
** without modifying the main part of the file.
*/




#endif
