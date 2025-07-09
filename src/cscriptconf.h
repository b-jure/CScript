/*
** cscriptconf.h
** CScript configuration
** See Copyright Notice in cscript.h
*/


#ifndef cscriptconfig_h
#define cscriptconfig_h


#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <float.h>
#include <math.h>


#if SIZE_MAX < UCS_MAXINT
#error SIZE_MAX must be greater or equal UCS_MAXINT
#endif

#if ((UINT_MAX >> 30) < 3)
#error 'int' has to have at least 32 bits
#endif


/* {====================================================================== 
**                      Configuration file for CScript.
**                (Tries its best to mimic Lua configuration)
** ======================================================================= */

#if defined(_WIN32) && !defined(_WIN32_WCE)
#define CS_USE_WINDOWS      /* enable goodies for regular Windows */
#endif


#if defined(CS_USE_WINDOWS)
#define CS_DL_DLL           /* enable support for DLL */
#endif


#if defined(CS_USE_LINUX)
#define CS_USE_POSIX
#define CS_USE_DLOPEN
#define CS_READLINELIB	        "libreadline.so"
#endif


#if defined(CS_USE_MACOSX)
#define CS_USE_POSIX
#define CS_USE_DLOPEN
#define CS_READLINELIB	        "libedit.dylib"
#endif


#if defined(CS_USE_IOS)
#define CS_USE_POSIX
#define CS_USE_DLOPEN
#endif


/* {======================================================================
** Configuration for number types.
** ======================================================================= */

/* 
** @CS_INT_TYPE defines the type for CScript integers.
** @CS_FLOAT_TYPE defines the type for CScript floats.
*/

/* predefined options for CS_INT_TYPE */
#define CS_INT_INT                  1
#define CS_INT_LONG                 2
#define CS_INT_LONGLONG             3

/* predefined options for CS_FLOAT_TYPE */
#define CS_FLOAT_FLOAT              1
#define CS_FLOAT_DOUBLE             2
#define CS_FLOAT_LONGDOUBLE         3


/* default configuration ('long long' and 'double', for 64-bit) */
#define CS_INT_DEFAULT              CS_INT_LONGLONG
#define CS_FLOAT_DEFAULT            CS_FLOAT_DOUBLE


/* types for integers and floats */
#define CS_INT_TYPE                 CS_INT_DEFAULT
#define CS_FLOAT_TYPE               CS_FLOAT_DEFAULT

/* }===================================================================== */



/* {======================================================================
** Configuration for paths
** ======================================================================= */

/*
** @CS_PATH_SEP - is the character that separates templates in a path.
** @CS_PATH_MARK - is the string that marks the substitution points in a
** template.
** @CS_EXEC_DIR - in a Windows path is replaced by the executable's
** directory.
*/
#define CS_PATH_SEP             ";"
#define CS_PATH_MARK            "?"
#define CS_EXEC_DIR             "!"


/*
** @CS_PATH_DEFAULT - is the default path that CScript uses to look for
** CScript libraries.
** @CS_CPATH_DEFAULT - is the default path that CScript uses to look for
** C libraries.
** CHANGE them if your machine has a non-conventional directory
** hierarchy or if you want to install your libraries in
** non-conventional directories.
*/

#define CS_VDIR         CS_VERSION_MAJOR "." CS_VERSION_MINOR
#if defined(_WIN32)     /* { */
/*
** In Windows, any exclamation mark ('!') in the path is replaced by the
** path of the directory of the executable file of the current process.
*/
#define CS_CSDIR    "!\\cscript\\"
#define CS_CDIR     "!\\"
#define CS_SHRDIR   "!\\..\\share\\cscript\\" CS_VDIR "\\"

#if !defined(CS_PATH_DEFAULT)
#define CS_PATH_DEFAULT \
        CS_CSDIR"?.cscript;"  CS_CSDIR"?\\init.cscript;" \
        CS_CDIR"?.cscript;"  CS_CDIR"?\\init.cscript;" \
        CS_SHRDIR"?.cscript;" CS_SHRDIR"?\\init.cscript;" \
        ".\\?.cscript;" ".\\?\\init.cscript"
#endif

#if !defined(CS_CPATH_DEFAULT)
#define CS_CPATH_DEFAULT \
        CS_CDIR"?.dll;" \
        CS_CDIR"..\\lib\\cscript\\" CS_VDIR "\\?.dll;" \
        CS_CDIR"loadall.dll;" ".\\?.dll"
#endif

#else                   /* }{ */

#define CS_ROOT     "/usr/local/"
#define CS_CSDIR    CS_ROOT "share/cscript/" CS_VDIR "/"
#define CS_CDIR     CS_ROOT "lib/cscript/" CS_VDIR "/"

#if !defined(CS_PATH_DEFAULT)
#define CS_PATH_DEFAULT \
        CS_CSDIR"?.cscript;"  CS_CSDIR"?/init.cscript;" \
        CS_CDIR"?.cscript;"  CS_CDIR"?/init.cscript;" \
        "./?.cscript;" "./?/init.cscript"
#endif

#if !defined(CS_CPATH_DEFAULT)
#define CS_CPATH_DEFAULT \
        CS_CDIR"?.so;" CS_CDIR"loadall.so;" "./?.so"
#endif

#endif                  /* } */


/*
** @CS_DIRSEP - is the directory separator (for submodules).
** CHANGE it if your machine does not use "/" as the directory separator
** and is not Windows. (On Windows CScript automatically uses "\".)
*/
#if !defined(CS_DIRSEP)

#if defined(_WIN32)
#define CS_DIRSEP       "\\"
#else
#define CS_DIRSEP       "/"
#endif

#endif


/*
** @CS_IGMARK - is a mark to ignore all after it when building the
** module name (e.g., used to build the csopen_ function name).
** Typically, the suffix after the mark is the module version,
** as in "mod-v1.2.so".
*/
#define CS_IGMARK               "-"

/* }===================================================================== */



/* {======================================================================
** Marks for exported symbols in the C code
** ======================================================================= */

/*
** @CS_API - is a mark for all core API functions.
** @CSLIB_API - is a mark for all auxiliary library functions.
** @CSMOD_API - is a mark for all standard library opening functions.
** CHANGE them if you need to define those functions in some special way.
** For instance, if you want to create one Windows DLL with the core and
** the libraries, you may want to use the following definition (define
** CS_BUILD_AS_DLL to get it).
*/
#if defined(CS_BUILD_AS_DLL)    /* { */
#if defined(CS_CORE) || defined(CS_LIB)         /* { */
#define CS_API      __declspec(dllexport)
#else                                           /* }{ */
#define CS_API      __declspec(dllimport)
#endif                                          /* } */
#else                           /* }{ */
#define CS_API      extern
#endif                          /* } */


#define CSLIB_API       CS_API
#define CSMOD_API       CS_API


/*
** @CSI_FUNC - mark for all external functions that are not being exported
** to outside modules.
** @CSI_DDEF and @CSI_DDEC - are marks for all extern (const) variables,
** none of which to be exported to outside modules (CSI_DDEF for
** definitions and CSI_DDEC for declarations).
*/
#if defined(__GNUC__) && ((__GNUC__ * 100 + __GNUC_MINOR__) >= 302) \
    && defined(__ELF__)             /* { */
#define CSI_FUNC        __attribute__((visibility("internal"))) extern
#else                               /* }{ */
#define CSI_FUNC        extern
#endif                              /* } */

#define CSI_DEC(dec)    CSI_FUNC dec
#define CSI_DEF         /* empty */

/* }===================================================================== */



/* {======================================================================
** Configuration for numbers
** ======================================================================= */

/*
** @CS_NUMBER - is the CScript floating point type.
** @CS_NUMBER_FMT - is the format for writing floats.
** @CS_NUMBER_FMTLEN - is the additional length modifier when writing floats.
** @c_mathop - allows the addition of an 'l' or 'f' to all math operations.
** @c_floor - floor division.
** @cs_number2str - convert float into string.
** @cs_str2number - convert numeral into float
** @cs_number2integer - converts float to integer or returns 0 if float is
** not withing the range of integer.
*/


#define c_floor(n)          (c_mathop(floor)(n))

#define cs_number2str(s,sz,n) \
        c_snprintf((s), (sz), CS_NUMBER_FMT, (CS_NUMBER)(n))

#define cs_number2integer(n,p) \
    ((n) >= (CS_NUMBER)(CS_INTEGER_MIN) && \
     (n) < (CS_NUMBER)(CS_INTEGER_MAX) && \
     (*(p) = (CS_INTEGER)(n), 1))


#if CS_FLOAT_TYPE == CS_FLOAT_FLOAT                 /* { single precision */

#error 'float' as 'CS_NUMBER' is not supported.

#elif CS_FLOAT_TYPE == CS_FLOAT_DOUBLE              /* }{ double precision */

#define CS_NUMBER               double

#define CS_NUMBER_FMTLEN        ""
#define CS_NUMBER_FMT           "%.15g"

#define c_floatatt(n)           (DBL_##n)

#define CS_NUMBER_MIN           c_floatatt(MIN)
#define CS_NUMBER_MAX           c_floatatt(MAX)

#define CS_HUGE_VAL             ((cs_Number)HUGE_VAL)

#define c_mathop(op)            op

#define cs_str2number(s,p)      strtod((s), (p))

#elif CS_FLOAT_TYPE == CS_FLOAT_LONG_DOUBLE_TYPE

#error 'long double' as 'CS_NUMBER' is not supported.

#else                                               /* }{ */

#error Unrecognized or undefined float type.

#endif                                              /* } */


#if !defined(cs_str2number)
#endif


/*
** @CS_INTEGER - integer type.
** @CS_UNSIGNED - unsigned integer.
** @CS_INTEGER_MAX - maximum integer size.
** @CS_INTEGER_MIN - minimum integer size.
** @CS_UNSIGNED_MAX - maximum unsigned integer size.
** @CS_INTEGER_FMTLEN - additional length of modifier when writing integers.
** @cs_integer2str - converts an integer to string.
*/


#define CS_UNSIGNED         unsigned CS_INTEGER

#define CS_INTEGER_FMT      "%" CS_INTEGER_FMTLEN "d"

#define cs_integer2str(s,sz,n) \
        c_snprintf((s),(sz),CS_INTEGER_FMT,(CS_INTEGER)(n))


#if CS_INT_TYPE == CS_INT_INT               /* { int */

#error 'int' as 'CS_INTEGER' is not supported.

#elif CS_INT_TYPE == CS_INT_LONG            /* }{ long */

#error 'long' as 'CS_INTEGER' is not supported.

#elif CS_INT_TYPE == CS_INT_LONGLONG        /* }{ long long */

#if defined(LLONG_MAX)          /* { */

#define CS_INTEGER              long long
#define CS_INTEGER_MAX          LLONG_MAX
#define CS_INTEGER_MIN          LLONG_MIN

#define CS_UNSIGNED_MAX         ULLONG_MAX

#define CS_INTEGER_FMTLEN       "ll"

#elif defined(CS_USE_WINDOWS)   /* }{ */

#define CS_INTEGER              __int64
#define CS_INTEGER_MAX          _I64_MAX
#define CS_INTEGER_MIN          _I64_MIN

#define CS_UNSIGNED_MAX         _UI64_MAX

#define CS_INTEGER_FMTLEN       "I64"

#else                           /* }{ */

#error Compiler does not support 'long long'.

#endif                          /* } */

#else

#error Unrecognized or undefined integer type.

#endif                                      /* } */

/* }===================================================================== */



/* {======================================================================
** Dependencies with C99
** ======================================================================= */

/*
** @c_sprintf - is equivalent to 'snprintf'.
*/
#define c_snprintf(s,sz,fmt,...)        snprintf(s, sz, fmt, __VA_ARGS__)


/* 
** @cs_pointer2str - converts a pointer to a string.
*/
#define cs_pointer2str(buff,sz,p)       c_snprintf(buff,sz,"%p",p)


/*
** @cs_number2strx - converts float to a hexadecimal numeral.
*/
#define cs_number2strx(C,b,sz,f,n)  \
        ((void)C, c_snprintf(b,sz,f,(CS_NUMBER)(n)))


/*
** @cs_getlocaledecpoint - gets the locale "radix character" (decimal point).
** Change that if you do not want to use C locales. (Code using this
** macro must include the header 'locale.h'.)
*/
#if !defined(cs_getlocaledecpoint)
#define cs_getlocaledecpoint()      (localeconv()->decimal_point[0])
#endif


/*
** @csi_likely - likely branch to be taken.
** @csi_unlikely - unlikely branch to be taken.
** Jump prediction macros.
*/
#if !defined(csi_likely)

#if defined(__GNUC__) && !defined(CS_NOBUILTIN)
#define csi_likely(cond)        __builtin_expect((cond) != 0, 1)
#define csi_unlikely(cond)      __builtin_expect((cond) != 0, 0)
#else
#define csi_likely(cond)        cond
#define csi_unlikely(cond)      cond
#endif

#endif


#if defined(CS_CORE) || defined(CS_LIB)
/* shorter names for internal use */
#define c_likely(cond)      csi_likely(cond)
#define c_unlikely(cond)    csi_unlikely(cond)
#endif

/* }===================================================================== */



/* {======================================================================
** Macros that affect the API and must be stable (that is, must be the
** same when you compile CScript and when you compile code that links to
** CScript).
** ======================================================================= */

/*
** @CSI_MAXSTACK - stack size limit.
** CHANGE it if you need a different limit. This limit is arbitrary;
** its only purpose is to stop CScript from consuming unlimited stack
** space (and to reserve some numbers for pseudo-indices).
** (It must fit into max(size_t)/32 and max(int)/2.)
*/
#define CSI_MAXSTACK        (1 << 23)


/*
** @CS_EXTRASPACE - defines the size of a raw memory associated with
** the CScript state with very fast access (memory chunk before state).
** CHANGE if you need a different size.
*/
#define CS_EXTRASPACE       sizeof(void *)


/*
** @CS_IDSIZE - the maximum size for the description of the source
** of a function in debug information.
** CHANGE it if you want a different size.
*/
#define CS_IDSIZE           60


/*
** @CSL_BUFFERSIZE is the initial buffer size used by the cscriptaux
** buffer system.
*/
#define CSL_BUFFERSIZE      1024


/*
** @CSI_MAXALIGN - defines fields that, when used in a union, ensure maximum
** alignment for the other items in that union.
*/
#define CSI_MAXALIGN    long l; cs_Integer i; double d; cs_Number n; void *p


/* 
** @CS_USE_APICHECK turns on several consistency checks on the C API.
** Define it as a help when debugging C code.
*/
#if defined(CS_USE_APICHECK)
#include <assert.h>
#define csi_checkapi(C,e)       assert(e)
#endif

/* }====================================================================== */




/* }====================================================================== */

/*
** Local configuration. You can use this space to add your redefinitions
** without modifying the main part of the file.
*/




#endif
