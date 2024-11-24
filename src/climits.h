/*
** climits.h
** Limits, basic types and some other definitions
** See Copyright Notice in cscript.h
*/


#ifndef CLIMITS_H
#define CLIMITS_H

#include "cscript.h"



/*
** Signed and unsigned types that represent count
** in bytes of total memory used by cript.
*/
typedef size_t cs_umem;
typedef ptrdiff_t cs_mem;

#define CRUMEM_MAX      ((cs_umem)(~(cs_umem)(0)))
#define CRMEM_MAX       ((cs_mem)(CRUMEM_MAX >> 1))


/*
** Used for representing small signed/unsigned
** numbers instead of declaring them 'char'.
*/
typedef unsigned char cs_ubyte;
typedef signed char cs_byte;

#define CRUBYTE_MAX     ((cs_ubyte)(~(cs_ubyte)(0)))
#define CRBYTE_MAX      ((cs_ubyte)(cs_ubyte_MAX >> 1))


/* 
** Unsigned 32-bit integer; for 'nCcalls'.
** This value must have 16 bits for counting nested
** Cript function calls and 16 bits for nested C calls.
*/
typedef uint32_t cs_uint32;


/* nice to have */
typedef unsigned int uint;
typedef unsigned short ushrt;
typedef unsigned long ulong;


/*
** Maximum size visible for CSript.
** It must be less than what is representable by 'cs_Integer'.
*/
#define MAXSIZE \
        (sizeof(size_t) < sizeof(cs_Integer) \
        ? (SIZE_MAX) : (size_t)(CS_INTEGER_MAX))



/* convert pointer 'p' to 'unsigned int' */
#define pointer2uint(p)         ((uint)((uintptr_t)(p)&(UINT_MAX)))



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
#define csi_checkapi(ts,e)      ((void)ts, cs_assert(e))
#endif

#define api_check(ts,e,err)     csi_checkapi(ts,(e) && err)



/*
** Allow threaded code by default on GNU C compilers.
** What this allows is usage of jump table aka using
** local labels inside arrays making O(1) jumps to
** instructions inside interpreter loop.
*/
#if defined(__GNUC__)
#define PRECOMPUTED_GOTO
#endif



/* inline functions */
#if defined(__GNUC__)
#define cs_inline       __inline__
#else
#define cs_inline       inline
#endif

/* static inline */
#define cs_sinline      static cs_inline



/* non-return type */
#if defined(__GNUC__)
#define cs_noret        void __attribute__((noreturn))
#elif defined(_MSC_VER) && _MSC_VER >= 1200
#define cs_noret        void __declspec(noreturn)
#else
#define cs_noret        void
#endif



/*
** Type for virtual-machine instructions
** Instructions (opcodes) are 1 byte in size not including
** the arguments; arguments vary in size (short/long) and
** more on that in 'copcode.h'.
*/
typedef cs_ubyte Instruction;



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
** Minimum initial size for hashtables.
** Size is power of 2 => 2^CSI_MINHTBITS.
** Must be less than MAXHBITS (check chashtable.c).
*/
#if !defined(CSI_MINHTABSIZE)
#define CSI_MINHTBITS           3
#endif



/*
** Minimum size for string buffer during lexing, this buffer memory
** will be freed after compilation.
*/
#if !defined(CS_MINBUFFER)
#define CS_MINBUFFER           32
#endif



/*
** Minimum internal array size.
** This should be 2^n='CS_MINARRSIZE'.
** Make sure this value fits in 'INT_MAX' and is >= 4.
*/
#if !defined(CSI_MINARRSIZE)
#define CSI_MINARRSIZE          8
#endif



/*
** Maximum call depth for nested C calls including the
** parser limit for syntactically nested non-terminals and
** other features implemented through recursion in C.
** Any value will suffice as long as it fits in 'unsigned short'.
*/
#define CSI_MAXCCALLS       4096



/*
** Runs each time program enters ('cs_lock') and
** leaves ('cs_unlock') CSript core (C API).
 */
#if !defined(cs_lock)

#if 1
#define cs_lock(ts)         ((void)0)
#define cs_unlock(ts)       csTR_dumpstack(ts, "stack after -> %s", __func__)
#else
#define cs_lock(ts)         ((void)0)
#define cs_unlock(ts)       ((void)0)
#endif

#endif



/*
** These macros allow user-defined action to be taken each
** time cs_State (thread) is created or deleted.
*/
#if !defined(csi_userstatecreated)
#define csi_userstatecreated(ts)            ((void)(ts))
#endif

#if !defined(csi_userstatethread)
#define csi_userstatethread(ts,thread)      ((void)ts)
#endif

#if !defined(csi_userthreadfree)
#define csi_userthreadfree(ts,thread)       ((void)ts)
#endif

#if !defined(csi_userstatefree)
#define csi_userstatefree(ts)               ((void)(ts))
#endif



/*
** @MAX - return maximum value.
** @MIN - return minimum value.
*/
#if defined(__GNUC__)
#define MAX(a, b) \
        __extension__ \
        ({ __typeof__(a) _a = (a); \
           __typeof__(b) _b = (b); \
           _a > _b ? _a : _b; })

#define MIN(a, b) \
        __extension__\
        ({ __typeof__(a) _a = (a); \
           __typeof__(b) _b = (b); \
           _a > _b ? _b : _a; })
#else
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif



/* @csi_abs - get absolute 'x' value. */
#ifndef csi_abs
#define csi_abs(x)      ((x) < 0 ? -(x) : (x))
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
#define cast_ubyte(e)       cast(cs_ubyte,(e))
#define cast_ubytep(e)      cast(cs_ubyte*,(e))
#define cast_byte(e)        cast(cs_byte,(e))
#define cast_num(e)         cast(cs_Number,(e))
#define cast_int(e)         cast(int,(e))
#define cast_uint(e)        cast(uint,(e))
#define cast_umem(e)        cast(cs_umem,(e))
#define cast_mem(e)         cast(cs_mem,(e))
#define cast_charp(e)       cast(char *,(e))
#define cast_char(e)        cast(char,(e))
#define cast_sizet(e)       cast(size_t,(e))
#define cast_voidp(e)       cast(void *,(e))
#define cast_void(e)        cast(void,(e))

/* cast 'cs_Integer' to 'cs_Unsigned' */
#define csi_castS2U(i)      ((cs_Unsigned)(i))

/* cast 'cs_Unsigned' to 'cs_Integer' */
#define csi_castU2S(i)      ((cs_Integer)(i))



/* string literal length */
#define SLL(sl) (sizeof(sl) - 1)



/* @csi_nummod - modulo 'a - floor(a/b)*b'. */
#define csi_nummod(ts,a,b,m) \
        { (void)(ts); (m) = cs_mathop(fmod)(a, b); \
          if (((m) > 0) ? (b)<0 : ((m) < 0 && (b) > 0)) (m) += (b); }

/* @csi_numdiv - float division. */
#ifndef csi_numdiv
#define csi_numdiv(ts, a, b)    ((void)(ts), (a)/(b))
#endif

/* @csi_numidiv - floor division (or division between integers). */
#ifndef csi_numidiv
#define csi_numidiv(ts, a, b)   ((void)(ts), cs_mathop(floor)(csi_numdiv(a, b)))
#endif

/* @csi_numpow - exponentiation. */
#ifndef csi_numpow
#define csi_numpow(ts, a, b) \
    ((void)(ts), (b) == 2 ? (a)*(a) : cs_mathop(pow)(a, b))
#endif

/*
** @csi_numadd - addition.
** @csi_numsub - subtraction.
** @csi_nummul - multiplication.
** @csi_numunm - negation.
*/
#ifndef csi_numadd
#define csi_numadd(ts, a, b)    ((void)(ts), (a) + (b))
#define csi_numsub(ts, a, b)    ((void)(ts), (a) - (b))
#define csi_nummul(ts, a, b)    (void)(ts), ((a) * (b))
#define csi_numunm(ts, a)       ((void)(ts), -(a))
#endif

/*
** @csi_numeq - ordering equal.
** @csi_numne - ordering not equal.
** @csi_numlt - ordering less than.
** @csi_numle - ordering less equal.
** @csi_numgt - ordering greater than.
** @csi_numge - ordering greater equal.
*/
#ifndef csi_numeq
#define csi_numeq(a, b)         ((a) == (b))
#define csi_numne(a, b)         (!csi_numeq(a, b))
#define csi_numlt(a, b)         ((a) < (b))
#define csi_numle(a, b)         ((a) <= (b))
#define csi_numgt(a, b)         ((a) > (b))
#define csi_numge(a, b)         ((a) >= (b))
#endif

/* @csi_numisnan - check if number is 'NaN'. */
#ifndef csi_numisnan
#define csi_numisnan(a)         (!csi_numeq(a, a))
#endif


/*
** @CS_STRESS_GC - enables stress test for garbage collector, on each
** tracked memory change it performs full garbage collection.
*/
#if defined(CS_STRESS_GC)
#define gcmemchange(ts,pre,pos) \
    { if (gcrunning(G_(ts)->gc)) { pre; csG_full(ts); pos; } }
#else
#define gcmemchange(ts,pre,pos)         ((void)0)
#endif

#endif
