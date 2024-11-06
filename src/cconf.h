/*
** cconf.h
** CScript configuration
** See Copyright Notice in cscript.h
*/

#ifndef CRCONFIG_H  /* { */
#define CRCONFIG_H

#define __STDC_LIMIT_MACROS
#include <limits.h>
#include <stdint.h>
#include <float.h>
#include <math.h>


/* Lets not... */
#if SIZE_MAX < UINT_MAX     /* { */
#error CScript doesn't handle implementations where 'size_t' is smaller than \
    'unsigned int'.
#endif                      /* } */


/*
 * Branch prediction builtin functions that reorder
 * code during compilation or hint the branch predictor
 * somehow in non-direct way.
 * @cs_likely - likely branch to be taken.
 * @cs_unlikely - unlikely branch to be taken.
 */
#if defined(__GNUC__) && !defined(CS_NOBUILTIN)     /* { */
#define cs_likely(cond)         __builtin_expect((cond) != 0, 1)
#define cs_unlikely(cond)       __builtin_expect((cond) != 0, 0)
#else                                               /* }{ */
#define cs_likely(cond)         cond
#define cs_unlikely(cond)       cond
#endif                                              /* } */



/*
 * @CS_DIRSEP - directory separator; for Windows machines
 * backslash is used, for any other POSIX is assumed
 * (forward slash).
 * Change it if your system uses something else.
 */
#if defined(_WIN32)             /* { */
#define CS_DIRSEP       "\\"
#else                           /* }{ */
#define CS_DIRSEP       "/"
#endif                          /* } */


/* @CS_PATH_SEP - path separator. */
#define CS_PATH_SEP ";"


/*
 * TODO: Finish this
 * @CS_DFL_PATH - default path that CScript uses to find CScript libraries.
 * @CS_DFL_CPATH - default path that CScript uses to find C libraries.
 */
#define CS_VERDIR       CS_VERSION_MAJOR "." CS_VERSION_MINOR
#if defined(_WIN32)
// TODO: Windows support
#else
#define CS_ROOT         "/usr/local/"
#define CS_CRDIR        CS_ROOT "share/cscript/" CS_VERDIR "/"
#define CS_CDIR         CS_ROOT "lib/cscript/" CS_VERDIR "/"

#if !defined(CS_DFL_PATH)
#define CS_DFL_PATH  \
    CS_CRDIR"?.cri;"  CS_CRDIR"?/init.cri;" \
    CS_CDIR"?.cri;"  CS_CDIR"?/init.cri;" \
    "./?.cri;" "./?/init.cri"
#endif
#if !defined(CS_DFL_CPATH)
#define CS_DFL_CPATH    CS_CDIR"?.so;" CS_CDIR"loadall.so;" "./?.so"
#endif

#endif


/* 
** @CS_API - signature for core API functions.
** @CRLIB_API - signature for all auxiliary library functions.
** @CRMOD_API - signature for all standard library opening functions.
*/
#if defined(CS_BUILD_AS_DLL)    /* { */
#if defined(CS_CORE) || defined(CS_LIB)     /* { */
#define CS_API      __declspec(dllexport)
#else                                       /* }{ */
#define CS_API      __declspec(dllimport)
#endif                                      /* } */
#else
#define CS_API      extern
#endif                          /* } */

#define CRLIB_API       CS_API
#define CRMOD_API       CS_API


/*
 * @CSI_FUNC - mark for external functions that are not
 * being exported to outside modules aka for functions
 * that are external but not part of the core C API.
 * This basically informs the compiler that ABI compatibility
 * doesn't need to be upheld, since nobody outside the module
 * will ever access the function.
 */
#if defined(__GNUC__) && ((__GNUC__ * 100 + __GNUC_MINOR__) >= 302) \
    && defined(__ELF__)             /* { */
#define CSI_FUNC        __attribute__((visibility("internal"))) extern
#else                               /* }{ */
#define CSI_FUNC        extern
#endif                              /* } */


/*
 * @CSI_DEC - mark for extern const variable declarations.
 * These variables are not to be exported to outside modules.
 * @CSI_DEF - same as @CSI_DEC just this mark is used for
 * variable definitions.
 */
#define CSI_DEC(dec)    CSI_FUNC dec
#define CSI_DEF         /* empty */


/*
 * @CS_FLOAT_FLOAT_TYPE - single precision floating point.
 * @CS_FLOAT_DOUBLE_TYPE - double precision floating point.
 * @CS_FLOAT_LONG_DOUBLE_TYPE - 'long double'.
 */
#define CS_FLOAT_FLOAT_TYPE             0
#define CS_FLOAT_DOUBLE_TYPE            1
#define CS_FLOAT_LONG_DOUBLE_TYPE       2


/* @CS_FLOAT_TYPE - CScript floating point type. */
#define CS_FLOAT_TYPE           CS_FLOAT_DOUBLE_TYPE



/*
 * @CS_NUMBER - CScript floating point type.
 * @CS_FLOAT_FMT - @CS_NUMBER format for writing floats.
 * @CS_FLOAT_FMTLEN - additional length modifier when writing @CS_NUMBER.
 * @cs_mathop - performs correct math operation variant for type @CS_NUMBER.
 * @cs_floor - floors @CS_NUMBER.
 * @cs_str2float - convert string into @CS_NUMBER.
 * @cs_float2str - convert @CS_NUMBER into string.
 * @cs_float2integer - converts @CS_NUMBER to @CS_INTEGER or
 * returns 0 if 'cs_Number' is not within the range of 'cs_Integer'.
 */
#if CS_FLOAT_TYPE == CS_FLOAT_FLOAT_TYPE        /* { */
#error 'float' as 'cs_Number' is not supported.
#elif CS_FLOAT_TYPE == CS_FLOAT_DOUBLE_TYPE     /* }{ */
#define CS_NUMBER               double
#define CS_FLOAT_FMTLEN         ""
#define CS_FLOAT_FMT            "%.14g"
#define cs_mathop(op)           op
#define cs_str2number(s,p)      strtod((s),(p))
#define CS_HUGEVAL              HUGE_VAL
#define CS_NUMBER_MIN           DBL_MIN
#define CS_NUMBER_MAX           DBL_MAX
#elif CS_FLOAT_TYPE == CS_FLOAT_LONG_DOUBLE_TYPE
#error 'long double' as 'cs_Number' is not supported.
#else                                           /* }{ */
#error Unsupported floating-point format.
#endif                                          /* } */

#define cs_floor(n)             (cs_mathop(floor)(n))

#define cs_number2str(s,sz,n)   snprintf((s),(sz),CS_FLOAT_FMT,(cs_Number)(n))

#define cs_number2integer(n,p) \
    ((n) >= (CS_NUMBER)(CS_INTEGER_MIN) && \
     (n) < (CS_NUMBER)(CS_INTEGER_MAX) && \
     (*(p) = (CS_INTEGER)(n), 1))



/*
 * @CS_INTEGER - CScript integer type.
 * @CS_UNSIGNED - unsigned @CS_INTEGER.
 * @CS_INTEGER_MAX - maximum size of @CS_INTEGER.
 * @CS_INTEGER_MIN - minimum size of @CS_INTEGER.
 * @CS_INTEGER_FMTLEN - additional length of modifier when writing @CS_INTEGER.
 * @cs_Integer2str - converts @cs_Integer to string.
 */
#if !defined(UINTPTR_MAX)       /* { */
#error Missing 'UINTPTR_MAX' macro.
#endif                          /* } */
#if UINTPTR_MAX == 0xffffffffffffffff       /* { */
#define CS_INTEGER              int64_t
#define CS_INTEGER_MAX          INT64_MAX
#define CS_INTEGER_MIN          INT64_MIN

#define CS_INTEGER_FMTLEN       "l"

#define CS_UNSIGNED             uint64_t
#define CS_UNSIGNED_MAX         UINT64_MAX

#define cs_integer2str(s,sz,n) \
    snprintf((s),(sz),CS_INTEGER_FMT,(CS_INTEGER)(n))

#define cri_intop(op,x,y) \
    cri_castU2S(cri_castS2U(x) op cri_castS2U(y))

#elif UINTPTR_MAX == 0xffffffff             /* }{ */
#error 'CScript' requires 64-bit integer size.
#else                                       /* }{ */
#error Unknown pointer size or missing macro definition.
#endif                                      /* } */

#define CS_INTEGER_FMT          "%" CS_INTEGER_FMTLEN "d"



/* @cs_xstr2number - converts hexadecimal string to 'cs_Number'. */
#define cs_xstr2number(s,p)     cs_str2number((s),(p))


/*
 * @strx2numberovf - checks if 'n' (cs_Number) would overflow
 * during 'cs_xstr2number()' or 'cs_str2number()' conversion.
 */
#define strx2numberovf(n)       ((n) == (CS_HUGEVAL) || (n) == -(CS_HUGEVAL))


/*
 * @strx2numberovf - checks if 'n' (cs_Number) would underflow
 * during 'cs_xstr2number()' or 'cs_str2number()' conversion.
 */
#define strx2numberunf(n)       ((n) == (CS_NUMBER_MIN))



/*
 * @cs_Number2xstr - converts 'cs_Number' into hexadecimal
 * string; 'u' flag indicates uppercase/lowercase.
 */
#define cs_number2xstr(b,sz,u,n) \
    snprintf((b),(sz),((u)?"%A":"%a"),(CS_NUMBER)(n))


/* @cs_pointer2str - converts a pointer to a string. */
#define cs_pointer2str(b,sz,p)  snprintf((b),(sz),"%p",(p))



/*
** @CRL_BUFFERSIZE is the initial buffer size used by the cauxlib
** buffer system.
*/
#define CRL_BUFFERSIZE      1024


/*
 * @CSI_MAXALIGN - values that ensure maximum alignment of
 * other values when used inside of union.
 */
#define CSI_MAXALIGN    long l; cs_Integer i; double d; cs_Number n; void *p


/*
 ** @CS_EXTRASPACE - defines the size of a raw memory associated with
 ** a CScript state with very fast access (memory chunk before state).
 */
#define CS_EXTRASPACE       (sizeof(void *))


/* @CS_CHECKAPI - enables C API asserts. */
#if defined(CS_CHECKAPI)    /* { */
#include <assert.h>
#define cri_checkapi(ts,e)      assert(e)
#endif                      /* } */


/*
 * @CSI_MAXSTACK - maximum stack size.
 * Any positive value that can fit into INT_MAX/2.
 */
#define CSI_MAXSTACK    5000000


/*
 * @CSI_MAXSRC - maximum description size of function
 * source (check debug API).
 */
#define CSI_MAXSRC      70


#endif  /* } */ 
