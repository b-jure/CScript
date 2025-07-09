/*
** cscriptlimits.h
** Limits, basic types and some other definitions
** See Copyright Notice in cscript.h
*/


#ifndef cscriptlimits_h
#define cscriptlimits_h

#include "cscript.h"


typedef size_t          c_umem;
#define CS_MAXUMEM      ((c_umem)(~(c_umem)(0)))
typedef ptrdiff_t       c_mem;
#define CS_MAXMEM       ((c_mem)(CS_MAXUMEM >> 1))


typedef unsigned char   c_ubyte;
#define CS_MAXUBYTE     ((c_ubyte)(~(c_ubyte)(0)))
typedef signed char     c_byte;
#define MAXBYTE         ((c_ubyte)(CS_MAXUBYTE >> 1))


/* 
** Unsigned size of (at least) 4 bytes.
*/
typedef uint32_t        c_uint32;


/* nice to have */
typedef unsigned short  c_ushort;
typedef unsigned int    c_uint;
typedef unsigned long   c_ulong;


/* maximum value that fits in 'int' type */
#define CS_MAXINT       INT_MAX


/*
** Maximum size visible for CScript.
** It must be less than what is representable by 'cs_Integer'.
*/
#define CS_MAXSIZE \
        (sizeof(size_t) < sizeof(cs_Integer) \
            ? (SIZE_MAX) \
            : (size_t)(CS_INTEGER_MAX))


/* convert pointer 'p' to 'unsigned int' */
#if defined(UINTPTR_MAX)
#define C_P2I   uintptr_t
#else
#define C_P2I   uintmax_t
#endif

#define pointer2uint(p)     cast_uint((C_P2I)(p) & UINT_MAX)


/* internal assertions for debugging */
#if defined(CSI_ASSERT)
#undef NDEBUG
#include <assert.h>
#define cs_assert(e)        assert(e)
#endif

#if defined(cs_assert)
#define check_exp(c,e)      (cs_assert(c),(e))
#else
#define cs_assert(e)        ((void)0)
#define check_exp(c,e)      (e)
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
** Type for vm instructions.
** Instructions (opcodes) are 1-byte in size not including the instruction
** arguments.
*/
typedef c_ubyte Instruction;



#if !defined(CS_MAXLISTINDEX)
#define CS_MAXLISTINDEX         (CS_MAXINT - 1)
#endif



/*
** Maximum length for short strings, that is, strings that are
** internalized. (Cannot be smaller than reserved words or keys for
** metamethods, as these strings must be internalized;
** strlen("continue") = 8, strlen("__getidx") = 8.)
*/
#if !defined(CSI_MAXSHORTLEN)
#define CSI_MAXSHORTLEN	        40
#endif



/*
** Initial size for the string table (must be power of 2).
** The CScript core alone registers ~50 strings (reserved words +
** metamethod keys + a few others). Libraries would typically add
** a few dozens more.
*/
#if !defined(CSI_MINSTRTABSIZE)
#define CSI_MINSTRTABSIZE	128
#endif



/*
** Size of cache for strings in the API. 'N' is the number of
** sets (better be a prime) and "M" is the size of each set (M == 1
** makes a direct cache.)
*/
#if !defined(CSI_STRCACHE_N)
#define CSI_STRCACHE_N	        53  /* cache lines */
#define CSI_STRCACHE_M	        2   /* cache line size * sizeof(OString*) */
#endif



/*
** Minimum size for string buffer during lexing, this buffer memory
** will be freed after compilation.
*/
#if !defined(CSI_MINBUFFER)
#define CSI_MINBUFFER       32
#endif



/*
** Maximum depth for nested C calls, syntactical nested non-terminals,
** and other features implemented through recursion in C. (Value must
** fit in a 16-bit unsigned integer. It must also be compatible with
** the size of the C stack.)
*/
#if !defined(CSI_MAXCCALLS)
#define CSI_MAXCCALLS       200
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
** thread is created/deleted and/or state is opened/closed.
*/
#if !defined(csi_userstateopen)
#define csi_userstateopen(C)            ((void)(C))
#endif

#if !defined(csi_userstateclose)
#define csi_userstateclose(C)           ((void)(C))
#endif

#if !defined(csi_userstatethread)
#define csi_userstatethread(C,C1)       ((void)(C))
#endif

#if !defined(csi_userstatefree)
#define csi_userstatefree(C,C1)         ((void)(C))
#endif



/*
** @UNUSED - marks variable unused to avoid compiler
** warnings.
*/
#if !defined(UNUSED)
#define UNUSED(x)   ((void)(x))
#endif



#if !defined(cast)

#define cast(t, e)          ((t)(e))

#define cast_void(e)        cast(void,(e))
#define cast_voidp(e)       cast(void *,(e))
#define cast_num(e)         cast(cs_Number,(e))
#define cast_byte(e)        cast(c_ubyte,(e))
#define cast_bytep(e)       cast(c_ubyte*,(e))
#define cast_sbyte(e)       cast(c_byte,(e))
#define cast_int(e)         cast(int,(e))
#define cast_uint(e)        cast(c_uint,(e))
#define cast_umem(e)        cast(c_umem,(e))
#define cast_mem(e)         cast(c_mem,(e))
#define cast_charp(e)       cast(char *,(e))
#define cast_char(e)        cast(char,(e))
#define cast_uchar(e)       cast(unsigned char,(e))
#define cast_sizet(e)       cast(size_t,(e))

#define c_castS2U(i)        ((cs_Unsigned)(i))

#define c_castU2S(i)        ((cs_Integer)(i))

#endif


/* literal length */
#if !defined(LL)
#define LL(sl)      (sizeof(sl) - 1)
#endif


#if !defined(c_nummod)

/* modulo 'a - floor(a/b)*b' */
#define c_nummod(C,a,b,m) \
        { (void)(C); (m) = c_mathop(fmod)(a, b); \
          if (((m) > 0) ? (b) < 0 : ((m) < 0 && (b) > 0)) (m) += (b); }

#define c_numdiv(C, a, b)       ((void)(C), (a)/(b))

#define c_numidiv(C, a, b)      ((void)(C), c_floor(c_numdiv(C,a,b)))

#define c_numpow(C, a, b) \
        ((void)(C), (b) == 2 ? (a)*(a) : c_mathop(pow)(a, b))

#endif


#if !defined(c_numadd)
#define c_numadd(C, a, b)       ((void)(C), (a) + (b))
#define c_numsub(C, a, b)       ((void)(C), (a) - (b))
#define c_nummul(C, a, b)       (void)(C), ((a) * (b))
#define c_numunm(C, a)          ((void)(C), -(a))
#endif


#if !defined(c_numeq)
#define c_numeq(a, b)       ((a) == (b))
#define c_numne(a, b)       (!c_numeq(a, b))
#define c_numlt(a, b)       ((a) < (b))
#define c_numle(a, b)       ((a) <= (b))
#define c_numgt(a, b)       ((a) > (b))
#define c_numge(a, b)       ((a) >= (b))
#endif


#if !defined(c_numisnan)
#define c_numisnan(a)       (!c_numeq(a, a))
#endif


/*
** Macro to control inclusion of some hard tests on stack reallocation.
*/
#if !defined(CSI_HARDSTACKTESTS)
#define condmovestack(C,pre,pos)    ((void)0)
#else
/* realloc stack keeping its size */
#define condmovestack(C,pre,pos)  \
    { int sz_ = stacksize(C); pre; csT_reallocstack((C), sz_, 0); pos; }
#endif


#if !defined(CSI_HARDMEMTESTS)
#define condchangemem(C,pre,pos)    ((void)0)
#else
#define condchangemem(C,pre,pos)  \
    { if (gcrunning(G(C))) { pre; csG_fullinc(C, 0); pos; } }
#endif


/* write a message to 'fp' stream */
#if !defined(cs_writelen)
#define cs_writelen(fp,s,l)         fwrite((s), sizeof(char), (l), fp)
#define cs_writeline(fp)            (cs_writelen(fp, "\n", 1), fflush(fp))
#define cs_writefmt(fp, msg, ...)   (fprintf(fp, msg, __VA_ARGS__), fflush(fp))
#define cs_writevfmt(fp,msg,ap)     (vfprintf(fp, msg, ap), fflush(fp))
#endif


#endif
