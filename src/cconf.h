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
 * @cr_likely - likely branch to be taken.
 * @cr_unlikely - unlikely branch to be taken.
 */
#if defined(__GNUC__) && !defined(CR_NOBUILTIN)     /* { */
#define cr_likely(cond)         __builtin_expect((cond) != 0, 1)
#define cr_unlikely(cond)       __builtin_expect((cond) != 0, 0)
#else                                               /* }{ */
#define cr_likely(cond)         cond
#define cr_unlikely(cond)       cond
#endif                                              /* } */



/*
 * @CR_DIRSEP - directory separator; for Windows machines
 * backslash is used, for any other POSIX is assumed
 * (forward slash).
 * Change it if your system uses something else.
 */
#if defined(_WIN32)             /* { */
#define CR_DIRSEP       "\\"
#else                           /* }{ */
#define CR_DIRSEP       "/"
#endif                          /* } */


/* @CR_PATH_SEP - path separator. */
#define CR_PATH_SEP ";"


/*
 * TODO: Finish this
 * @CR_DFL_PATH - default path that CScript uses to find CScript libraries.
 * @CR_DFL_CPATH - default path that CScript uses to find C libraries.
 */
#define CR_VERDIR       CR_VERSION_MAJOR "." CR_VERSION_MINOR
#if defined(_WIN32)
// TODO: Windows support
#else
#define CR_ROOT         "/usr/local/"
#define CR_CRDIR        CR_ROOT "share/cscript/" CR_VERDIR "/"
#define CR_CDIR         CR_ROOT "lib/cscript/" CR_VERDIR "/"

#if !defined(CR_DFL_PATH)
#define CR_DFL_PATH  \
    CR_CRDIR"?.cri;"  CR_CRDIR"?/init.cri;" \
    CR_CDIR"?.cri;"  CR_CDIR"?/init.cri;" \
    "./?.cri;" "./?/init.cri"
#endif
#if !defined(CR_DFL_CPATH)
#define CR_DFL_CPATH    CR_CDIR"?.so;" CR_CDIR"loadall.so;" "./?.so"
#endif

#endif


/* @CR_API - signature for core API functions. */
#if defined(CR_BUILD_AS_DLL)    /* { */
#if defined(CR_CORE) || defined(CR_LIB)     /* { */
#define CR_API      __declspec(dllexport)
#else                                       /* }{ */
#define CR_API      __declspec(dllimport)
#endif                                      /* } */
#else
#define CR_API      extern
#endif                          /* } */


/*
 * @CRLIB_API - signature for all auxiliary library functions;
 * functions that are not part of core API but are using it.
 */
#define CRLIB_API       CR_API


/*
 * @CRI_FUNC - mark for external functions that are not
 * being exported to outside modules aka for functions
 * that are external but not part of the core C API.
 * This basically informs the compiler that ABI compatibility
 * doesn't need to be upheld, since nobody outside the module
 * will ever access the function.
 */
#if defined(__GNUC__) && ((__GNUC__ * 100 + __GNUC_MINOR__) >= 302) \
                                                               && defined(__ELF__)         /* { */
#define CRI_FUNC        __attribute__((visibility("internal"))) extern
#else                               /* }{ */
#define CRI_FUNC        extern
#endif                              /* } */


/*
 * @CRI_DEC - mark for extern const variable declarations.
 * These variables are not to be exported to outside modules.
 * @CRI_DEF - same as @CRI_DEC just this mark is used for
 * variable definitions.
 */
#define CRI_DEC(dec)    CRI_FUNC dec
#define CRI_DEF         /* empty */


/*
 * @CR_FLOAT_FLOAT_TYPE - single precision floating point.
 * @CR_FLOAT_DOUBLE_TYPE - double precision floating point.
 * @CR_FLOAT_LONG_DOUBLE_TYPE - 'long double'.
 */
#define CR_FLOAT_FLOAT_TYPE             0
#define CR_FLOAT_DOUBLE_TYPE            1
#define CR_FLOAT_LONG_DOUBLE_TYPE       2


/* @CR_FLOAT_TYPE - CScript floating point type. */
#define CR_FLOAT_TYPE           CR_FLOAT_DOUBLE_TYPE



/*
 * @CR_NUMBER - CScript floating point type.
 * @CR_FLOAT_FMT - @CR_NUMBER format for writing floats.
 * @CR_FLOAT_FMTLEN - additional length modifier when writing @CR_NUMBER.
 * @cr_mathop - performs correct math operation variant for type @CR_NUMBER.
 * @cr_floor - floors @CR_NUMBER.
 * @cr_str2float - convert string into @CR_NUMBER.
 * @cr_float2str - convert @CR_NUMBER into string.
 * @cr_float2integer - converts @CR_NUMBER to @CR_INTEGER or
 * returns 0 if 'cr_Number' is not within the range of 'cr_Integer'.
 */
#if CR_FLOAT_TYPE == CR_FLOAT_FLOAT_TYPE        /* { */
#error 'float' as 'cr_Number' is not supported.
#elif CR_FLOAT_TYPE == CR_FLOAT_DOUBLE_TYPE     /* }{ */
#define CR_NUMBER               double
#define CR_FLOAT_FMTLEN         ""
#define CR_FLOAT_FMT            "%.14g"
#define cr_mathop(op)           op
#define cr_str2number(s,p)      strtod((s),(p))
#define CR_HUGEVAL              HUGE_VAL
#define CR_NUMBER_MIN           DBL_MIN
#define CR_NUMBER_MAX           DBL_MAX
#elif CR_FLOAT_TYPE == CR_FLOAT_LONG_DOUBLE_TYPE
#error 'long double' as 'cr_Number' is not supported.
#else                                           /* }{ */
#error Unsupported floating-point format.
#endif                                          /* } */

#define cr_floor(n)             (cr_mathop(floor)(n))

#define cr_number2str(s,sz,n)   snprintf((s),(sz),CR_FLOAT_FMT,(cr_Number)(n))

#define cr_number2integer(n,p) \
    ((n) >= (CR_NUMBER)(CR_INTEGER_MIN) && \
     (n) < (CR_NUMBER)(CR_INTEGER_MAX) && \
     (*(p) = (CR_INTEGER)(n), 1))



/*
 * @CR_INTEGER - CScript integer type.
 * @CR_UNSIGNED - unsigned @CR_INTEGER.
 * @CR_INTEGER_MAX - maximum size of @CR_INTEGER.
 * @CR_INTEGER_MIN - minimum size of @CR_INTEGER.
 * @CR_INTEGER_FMTLEN - additional length of modifier when writing @CR_INTEGER.
 * @cr_Integer2str - converts @cr_Integer to string.
 */
#if !defined(UINTPTR_MAX)       /* { */
#error Missing 'UINTPTR_MAX' macro.
#endif                          /* } */
#if UINTPTR_MAX == 0xffffffffffffffff       /* { */
#define CR_INTEGER              int64_t
#define CR_INTEGER_MAX          INT64_MAX
#define CR_INTEGER_MIN          INT64_MIN

#define CR_INTEGER_FMTLEN       "l"

#define CR_UNSIGNED             uint64_t
#define CR_UNSIGNED_MAX         UINT64_MAX

#define cr_integer2str(s,sz,n) \
    snprintf((s),(sz),CR_INTEGER_FMT,(CR_INTEGER)(n))

#define cri_intop(op,x,y) \
    cri_castU2S(cri_castS2U(x) op cri_castS2U(y))

#elif UINTPTR_MAX == 0xffffffff             /* }{ */
#error 'CScript' requires 64-bit integer size.
#else                                       /* }{ */
#error Unknown pointer size or missing macro definition.
#endif                                      /* } */

#define CR_INTEGER_FMT          "%" CR_INTEGER_FMTLEN "d"



/* @cr_xstr2number - converts hexadecimal string to 'cr_Number'. */
#define cr_xstr2number(s,p)     cr_str2number((s),(p))


/*
 * @strx2numberovf - checks if 'n' (cr_Number) would overflow
 * during 'cr_xstr2number()' or 'cr_str2number()' conversion.
 */
#define strx2numberovf(n)       ((n) == (CR_HUGEVAL) || (n) == -(CR_HUGEVAL))


/*
 * @strx2numberovf - checks if 'n' (cr_Number) would underflow
 * during 'cr_xstr2number()' or 'cr_str2number()' conversion.
 */
#define strx2numberunf(n)       ((n) == (CR_NUMBER_MIN))



/*
 * @cr_Number2xstr - converts 'cr_Number' into hexadecimal
 * string; 'u' flag indicates uppercase/lowercase.
 */
#define cr_number2xstr(b,sz,u,n) \
    snprintf((b),(sz),((u)?"%A":"%a"),(CR_NUMBER)(n))


/* @cr_pointer2str - converts a pointer to a string. */
#define cr_pointer2str(b,sz,p)  snprintf((b),(sz),"%p",(p))



/*
** @CRL_BUFFERSIZE is the initial buffer size used by the cauxlib
** buffer system.
*/
#define CRL_BUFFERSIZE      1024


/*
 * @CRI_MAXALIGN - values that ensure maximum alignment of
 * other values when used inside of union.
 */
#define CRI_MAXALIGN    long l; cr_Integer i; double d; cr_Number n; void *p


/*
 ** @CR_EXTRASPACE - defines the size of a raw memory associated with
 ** a CScript state with very fast access (memory chunk before state).
 */
#define CR_EXTRASPACE       (sizeof(void *))


/* @CR_CHECKAPI - enables C API asserts. */
#if defined(CR_CHECKAPI)    /* { */
#include <assert.h>
#define cri_checkapi(ts,e)      assert(e)
#endif                      /* } */


/*
 * @CRI_MAXSTACK - maximum stack size.
 * Any positive value that can fit into INT_MAX/2.
 */
#define CRI_MAXSTACK    5000000


/*
 * @CRI_MAXSRC - maximum description size of function
 * source (check debug API).
 */
#define CRI_MAXSRC      70


#endif  /* } */ 
