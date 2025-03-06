/*
** climits.h
** Limits, basic types and some other definitions
** See Copyright Notice in cscript.h
*/


#ifndef CLIMITS_H
#define CLIMITS_H

#include "cscript.h"


typedef size_t          c_mem;
#define MAXMEM          ((c_mem)(~(c_mem)(0)))
typedef ptrdiff_t       c_smem;
#define MAXSMEM         ((c_smem)(MAXMEM >> 1))


typedef unsigned char   c_byte;
#define MAXBYTE         ((c_byte)(~(c_byte)(0)))
typedef signed char     c_sbyte;
#define MAXSBYTE        ((c_byte)(MAXBYTE >> 1))


/* 
** Unsigned 32-bit integer; for 'nCcalls'.
** This value must have 16 bits for counting nested
** CSript function calls and 16 bits for nested C calls.
*/
typedef uint32_t        c_uint32;


/* nice to have */
typedef unsigned int    uint;
typedef unsigned short  ushort;
typedef unsigned long   ulong;


/* maximum value that fits in 'int' type */
#define MAXINT      INT_MAX


/*
** Maximum size visible for CScript.
** It must be less than what is representable by 'cs_Integer'.
*/
#define MAXSIZE \
        (sizeof(size_t) < sizeof(cs_Integer) \
            ? (SIZE_MAX) \
            : (size_t)(CS_INTEGER_MAX))



/* convert pointer 'p' to 'unsigned int' */
#if defined(UINTPTR_MAX)
#define C_P2I   uintptr_t
#else
#define C_P2I   uintmax_t
#endif

#define pointer2uint(p)     ((unsigned int)((C_P2I)(p) & UINT_MAX))


/* internal assertions for debugging */
#if defined(CSI_ASSERT)
#undef NDEBUG
#include <assert.h>
#define cs_assert(e)            assert(e)
#endif

#if defined(cs_assert)
#define check_exp(c,e)          (cs_assert(c),(e))
#else
#define cs_assert(e)            ((void)0)
#define check_exp(c,e)          (e)
#endif

/* C API assertions */
#if !defined(csi_checkapi)
#define csi_checkapi(C,e)       ((void)C, cs_assert(e))
#endif

#define api_check(C,e,err)      csi_checkapi(C,(e) && err)



/*
** Allow threaded code by default on GNU C compilers.
** What this allows is usage of jump table, meaning the use of
** local labels inside arrays, making instruction dispatch O(1)
** inside the interpreter loop.
*/
#if defined(__GNUC__)
#define PRECOMPUTED_GOTO
#endif



/* inline functions */
#if defined(__GNUC__)
#define c_inline        __inline__
#else
#define c_inline        inline
#endif

/* static inline */
#define c_sinline       static c_inline



/* non-return type */
#if defined(__GNUC__)
#define c_noret         void __attribute__((noreturn))
#elif defined(_MSC_VER) && _MSC_VER >= 1200
#define c_noret         void __declspec(noreturn)
#else
#define c_noret         void
#endif



/*
** Type for virtual-machine instructions.
** Instructions (opcodes) are 1-byte in size not including
** the arguments; arguments vary in size (short/long) and
** more on that in 'ccode.h'.
*/
typedef c_byte Instruction;



/*
** Maximum length for short strings, that is, strings that are
** internalized. (Cannot be smaller than reserved words or keys for
** metamethods, as these strings must be internalized;
** strlen("continue") = 8, strlen("__getidx") = 8.)
*/
#if !defined(CSI_MAXSHORTLEN)
#define CSI_MAXSHORTLEN	    40
#endif



/*
** Initial size for the string table (must be power of 2).
** The CScript core alone registers ~50 strings (reserved words +
** metamethod keys + a few others). Libraries would typically add
** a few dozens more.
*/
#if !defined(MINSTRTABSIZE)
#define MINSTRTABSIZE	    128
#endif



/*
** Size of cache for strings in the API. 'N' is the number of
** sets (better be a prime) and "M" is the size of each set (M == 1
** makes a direct cache.)
*/
#if !defined(STRCACHE_N)
#define STRCACHE_N	    53  /* cache lines */
#define STRCACHE_M	    2   /* cache line size * sizeof(OString*) */
#endif



/*
** Minimum size for string buffer during lexing, this buffer memory
** will be freed after compilation.
*/
#if !defined(CSI_MINBUFFER)
#define CSI_MINBUFFER       32
#endif



/*
** Maximum call depth for nested C calls including the
** parser limit for syntactically nested non-terminals and
** other features implemented through recursion in C.
** Any value will suffice as long as it fits in 'unsigned short'.
*/
#if !defined(CSI_MAXCCALLS)
#define CSI_MAXCCALLS       4096
#endif



/*
** Runs each time program enters ('cs_lock') and leaves ('cs_unlock')
** CSript core (C API).
*/
#if !defined(cs_lock)
#define cs_lock(C)          ((void)0)
#define cs_unlock(C)        ((void)0)
#endif



/*
** These macros allow user-defined action to be taken each time
** thread is created/deleted.
*/
#if !defined(csi_userstateopen)
#define csi_userstateopen(C)            ((void)(C))
#endif

#if !defined(csi_userstateclose)
#define csi_userstateclose(C)           ((void)(C))
#endif

#if !defined(csi_userstate)
#define csi_userstate(C,thread)         ((void)(C))
#endif

#if !defined(csi_userstatefree)
#define csi_userstatefree(C,thread)     ((void)(C))
#endif



/*
** @UNUSED - marks variable unused to avoid compiler
** warnings.
*/
#ifndef UNUSED
#define UNUSED(x)   ((void)(x))
#endif



/* @cast - cast expression 'e' as type 't'. */
#define cast(t, e)          ((t)(e))

#define cast_node(e)        cast(Node*,(e))
#define cast_byte(e)        cast(c_byte,(e))
#define cast_bytep(e)       cast(c_byte*,(e))
#define cast_sbyte(e)       cast(c_sbyte,(e))
#define cast_num(e)         cast(cs_Number,(e))
#define cast_int(e)         cast(int,(e))
#define cast_uint(e)        cast(uint,(e))
#define cast_mem(e)         cast(c_mem,(e))
#define cast_charp(e)       cast(char *,(e))
#define cast_char(e)        cast(char,(e))
#define cast_sizet(e)       cast(size_t,(e))
#define cast_voidp(e)       cast(void *,(e))
#define cast_void(e)        cast(void,(e))

/* cast 'cs_Integer' to 'cs_Unsigned' */
#define c_castS2U(i)        ((cs_Unsigned)(i))

/* cast 'cs_Unsigned' to 'cs_Integer' */
#define c_castU2S(i)        ((cs_Integer)(i))


#define c_intop(op,x,y)     c_castU2S(c_castS2U(x) op c_castS2U(y))


/* string literal length */
#define SLL(sl) (sizeof(sl) - 1)



/* @c_nummod - modulo 'a - floor(a/b)*b'. */
#define c_nummod(C,a,b,m) \
        { (void)(C); (m) = cs_mathop(fmod)(a, b); \
          if (((m) > 0) ? (b) < 0 : ((m) < 0 && (b) > 0)) (m) += (b); }

/* @c_numdiv - float division. */
#ifndef c_numdiv
#define c_numdiv(C, a, b)       ((void)(C), (a)/(b))
#endif

/* @c_numidiv - float floor division. */
#ifndef c_numidiv
#define c_numidiv(C, a, b)      ((void)(C), c_floor(c_numdiv(C,a,b)))
#endif

/* @c_numpow - exponentiation. */
#ifndef c_numpow
#define c_numpow(C, a, b) \
        ((void)(C), (b) == 2 ? (a)*(a) : cs_mathop(pow)(a, b))
#endif

/*
** @c_numadd - addition.
** @c_numsub - subtraction.
** @c_nummul - multiplication.
** @c_numunm - negation.
*/
#ifndef c_numadd
#define c_numadd(C, a, b)       ((void)(C), (a) + (b))
#define c_numsub(C, a, b)       ((void)(C), (a) - (b))
#define c_nummul(C, a, b)       (void)(C), ((a) * (b))
#define c_numunm(C, a)          ((void)(C), -(a))
#endif

/*
** @c_numeq - ordering equal.
** @c_numne - ordering not equal.
** @c_numlt - ordering less than.
** @c_numle - ordering less equal.
** @c_numgt - ordering greater than.
** @c_numge - ordering greater equal.
*/
#ifndef c_numeq
#define c_numeq(a, b)       ((a) == (b))
#define c_numne(a, b)       (!c_numeq(a, b))
#define c_numlt(a, b)       ((a) < (b))
#define c_numle(a, b)       ((a) <= (b))
#define c_numgt(a, b)       ((a) > (b))
#define c_numge(a, b)       ((a) >= (b))
#endif

/* @c_numisnan - check if number is 'NaN'. */
#ifndef c_numisnan
#define c_numisnan(a)       (!c_numeq(a, a))
#endif


/*
** Macro to control inclusion of some hard tests on stack reallocation.
*/
#if !defined(HARDSTACKTESTS)
#define condmovestack(C,pre,pos)	((void)0)
#else
/* realloc stack keeping its size */
#define condmovestack(C,pre,pos)  \
    { int sz_ = stacksize(C); pre; csT_reallocstack((C), sz_, 0); pos; }
#endif

#if !defined(HARDMEMTESTS)
#define condchangemem(C,pre,pos)	((void)0)
#else
#define condchangemem(C,pre,pos)  \
    { if (gcrunning(G(C))) { pre; csG_full(C, 0); pos; } }
#endif

#endif
