/*
** climits.h
** Limits, basic types and some other definitions
** See Copyright Notice in cscript.h
*/

#ifndef CRLIMITS_H
#define CRLIMITS_H

#include "cscript.h"



/*
 * Signed and unsigned types that represent count
 * in bytes of total memory used by cript.
 */
typedef size_t cs_umem;
typedef ptrdiff_t cs_mem;

#define CRUMEM_MAX      ((cs_umem)(~(cs_umem)(0)))
#define CRMEM_MAX       ((cs_mem)(CRUMEM_MAX >> 1))


/*
 * Used for representing small signed/unsigned
 * numbers instead of declaring them 'char'.
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


/*
 * Maximum size visible for CSript.
 * It must be less than what is representable by 'cs_Integer'.
 */
#define CRMAXSIZE \
    (sizeof(size_t) < sizeof(cs_Integer) ? \
        (SIZE_MAX) : (size_t)(CS_INTEGER_MAX))



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
#if !defined(cri_checkapi)
#define cri_checkapi(ts,e)      ((void)ts, cs_assert(e))
#endif

#define api_check(ts,e,err)     cri_checkapi(ts,(e) && err)



/*
 * Allow threaded code by default on GNU C compilers.
 * What this allows is usage of jump table aka using
 * local labels inside arrays making O(1) jumps to
 * instructions inside interpreter loop.
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



/* unreachable code (optimization) */
#if defined(__GNUC__)
#define cs_unreachable()        __builtin_unreachable()
#elif defined(_MSC_VER) && _MSC_VER >= 1200
#define cs_unreachable()        __assume(0)
#else
#define cs_unreachable()        cs_assert(0 && "unreachable")
#endif



/*
 * Type for virtual-machine instructions
 * Instructions (opcodes) are 1 byte in size not including
 * the arguments; arguments vary in size (short/long) and
 * more on that in 'copcode.h'.
 */
typedef cs_ubyte Instruction;



/*
 * Initial size for the weak hash table that stores
 * interned strings.
 * It has to be power of 2, because of the hash table
 * implementation.
 */
#if !defined(CSI_MINSTRHTABSIZE)
#define CSI_MINSTRHTABSIZE      64
#endif



/*
 * Initial size for hash tables excluding weak
 * strings hash table.
 * It has to be power of 2, because of the hash table
 * implementation.
 */
#if !defined(CSI_MINHTABSIZE)
#define CSI_MINHTABSIZE         8
#endif



/*
 * Minimum size for string buffer during
 * lexing, this buffer memory will be freed
 * after compilation.
 */
#if !defined(CSI_MINBUFFER)
#define CSI_MINBUFFER           32
#endif



/*
 * Maximum table load factor.
 * v1.0.0 is using linear probing so
 * keep this load factor <= 0.70 to
 * avoid excess collisions.
 */
#if !defined(CSI_MAXTABLOAD)
#define CSI_MAXHTABLOAD         0.70
#endif



/*
 * Maximum size for 'HTable'.
 * Make sure the value fits in 'INT_MAX'.
 */
#if !defined(CSI_MAXHTABSIZE)
#define CSI_MAXHTABSIZE         INT_MAX
#endif



/*
 * Minimum internal array siz.
 * This should be 2^n='CS_MINARRSIZE'.
 * Make sure this value fits in 'INT_MAX'
 * and is >= 4.
 */
#if !defined(CSI_MINARRSIZE)
#define CSI_MINARRSIZE          8
#endif



/*
 * Maximum call depth for nested C calls including the
 * parser limit for syntactically nested non-terminals and
 * other features implemented through recursion in C.
 * Any value will suffice as long as it fits in 'unsigned short'.
 */
#define CSI_MAXCCALLS       4096



/*
 * Runs each time program enters ('cs_lock') and
 * leaves ('cs_unlock') CSript core (C API).
 */
#if !defined(cs_lock)
#define cs_lock(ts)         ((void)0)
#define cs_unlock(ts)       ((void)0)
#endif



/*
 * These macros allow user-defined action to be taken each
 * time cs_State (thread) is created or deleted.
 */
#if !defined(cri_userstatecreated)
#define cri_userstatecreated(ts)            ((void)(ts))
#endif

#if !defined(cri_userstatethread)
#define cri_userstatethread(ts,thread)      ((void)ts)
#endif

#if !defined(cri_userthreadfree)
#define cri_userthreadfree(ts,thread)       ((void)ts)
#endif

#if !defined(cri_userstatefree)
#define cri_userstatefree(ts)               ((void)(ts))
#endif



/*
 * @MAX - return maximum value.
 * @MIN - return minimum value.
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



/* @cri_abs - get absolute 'x' value. */
#ifndef cri_abs
#define cri_abs(x)      ((x) < 0 ? -(x) : (x))
#endif



/*
 * @UNUSED - marks variable unused to avoid compiler
 * warnings.
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
#define cast_sizet(e)       cast(size_t,(e))

/* cast 'cs_Integer' to 'cs_Unsigned' */
#define cri_castS2U(i)      ((cs_Unsigned)(i))

/* cast 'cs_Unsigned' to 'cs_Integer' */
#define cri_castU2S(i)      ((cs_Integer)(i))



/* string literal length */
#define SLL(sl) (sizeof(sl) - 1)



/* @cri_nummod - modulo 'a - floor(a/b)*b'. */
#define cri_nummod(ts,a,b,m) \
        { (void)(ts); (m) = cs_mathop(fmod)(a, b); \
          if (((m) > 0) ? (b)<0 : ((m) < 0 && (b) > 0)) (m) += (b); }

/* @cri_numdiv - float division. */
#ifndef cri_numdiv
#define cri_numdiv(ts, a, b)    ((void)(ts), (a)/(b))
#endif

/* @cri_numidiv - floor division (or division between integers). */
#ifndef cri_numidiv
#define cri_numidiv(ts, a, b)   ((void)(ts), cs_mathop(floor)(cri_numdiv(a, b)))
#endif

/* @cri_numpow - exponentiation. */
#ifndef cri_numpow
#define cri_numpow(ts, a, b) \
    ((void)(ts), (b) == 2 ? (a)*(a) : cs_mathop(pow)(a, b))
#endif

/*
 * @cri_numadd - addition.
 * @cri_numsub - subtraction.
 * @cri_nummul - multiplication.
 * @cri_numunm - negation.
 */
#ifndef cri_numadd
#define cri_numadd(ts, a, b)    ((void)(ts), (a) + (b))
#define cri_numsub(ts, a, b)    ((void)(ts), (a) - (b))
#define cri_nummul(ts, a, b)    (void)(ts), ((a) * (b))
#define cri_numunm(ts, a)       ((void)(ts), -(a))
#endif

/*
 * @cri_numeq - ordering equal.
 * @cri_numne - ordering not equal.
 * @cri_numlt - ordering less than.
 * @cri_numle - ordering less equal.
 * @cri_numgt - ordering greater than.
 * @cri_numge - ordering greater equal.
 */
#ifndef cri_numeq
#define cri_numeq(a, b)         ((a) == (b))
#define cri_numne(a, b)         (!cri_numeq(a, b))
#define cri_numlt(a, b)         ((a) < (b))
#define cri_numle(a, b)         ((a) <= (b))
#define cri_numgt(a, b)         ((a) > (b))
#define cri_numge(a, b)         ((a) >= (b))
#endif

/* @cri_numisnan - check if number is 'NaN'. */
#ifndef cri_numisnan
#define cri_numisnan(a)         (!cri_numeq(a, a))
#endif


/*
** @CS_STRESS_GC - enables stress test for garbage
** collector, on each tracked memory change it performs
** full garbage collection.
*/
#if defined(CS_STRESS_GC)
#define gcmemchange(ts,pre,pos) \
    { if (gcrunning(G_(ts)->gc)) { pre; crG_full(ts); pos; } }
#else
#define gcmemchange(ts,pre,pos)         ((void)0)
#endif

#endif
