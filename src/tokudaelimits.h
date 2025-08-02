/*
** tokudaelimits.h
** Limits, basic types and some other definitions
** See Copyright Notice in tokudae.h
*/


#ifndef tokudaelimits_h
#define tokudaelimits_h

#include "tokudae.h"


typedef size_t          t_umem;
#define TOKU_MAXUMEM    ((t_umem)(~(t_umem)(0)))
typedef ptrdiff_t       t_mem;
#define TOKU_MAXMEM     ((t_mem)(TOKU_MAXUMEM >> 1))


typedef unsigned char   t_ubyte;
#define TOKU_MAXUBYTE   ((t_ubyte)(~(t_ubyte)(0)))
typedef signed char     t_byte;
#define TOKU_MAXBYTE    ((t_ubyte)(TOKU_MAXUBYTE >> 1))


/* 
** Unsigned size of (at least) 4 bytes.
*/
typedef uint32_t        t_uint32;


/* nice to have */
typedef unsigned short  t_ushort;
typedef unsigned int    t_uint;
typedef unsigned long   t_ulong;


/* maximum value that fits in 'int' type */
#define TOKU_MAXINT     INT_MAX


/* maximum value for 'size_t' */
#define TOKU_MAXSIZET	((size_t)(~(size_t)0))


/*
** Maximum size for strings and userdata visible for Tokudae;
** should be representable as a toku_Integer and as a size_t.
*/
#define TOKU_MAXSIZE \
        (sizeof(size_t) < sizeof(toku_Integer) \
            ? TOKU_MAXSIZET : cast_sizet(TOKU_INTEGER_MAX))


/* convert pointer 'p' to 'unsigned int' */
#if defined(UINTPTR_MAX)
#define T_P2I   uintptr_t
#else
#define T_P2I   uintmax_t
#endif

#define pointer2uint(p)     cast_uint((T_P2I)(p) & UINT_MAX)


/* internal assertions for debugging */
#if defined(TOKUI_ASSERT)
#undef NDEBUG
#include <assert.h>
#define toku_assert(e)        assert(e)
#endif

#if defined(toku_assert)
#define check_exp(c,e)      (toku_assert(c),(e))
#else
#define toku_assert(e)      ((void)0)
#define check_exp(c,e)      (e)
#endif

/* C API assertions */
#if !defined(tokui_checkapi)
#define tokui_checkapi(T,e)       ((void)T, toku_assert(e))
#endif

#define api_check(T,e,err)      tokui_checkapi(T,(e) && err)



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
#define t_inline        __inline__
#else
#define t_inline        inline
#endif

/* static inline */
#define t_sinline       static t_inline



/* non-return type */
#if defined(__GNUC__)
#define t_noret         void __attribute__((noreturn))
#elif defined(_MST_VER) && _MST_VER >= 1200
#define t_noret         void __declspec(noreturn)
#else
#define t_noret         void
#endif



/*
** Type for vm instructions.
** Instructions (opcodes) are 1-byte in size not including the instruction
** arguments.
*/
typedef t_ubyte Instruction;



#if !defined(TOKU_MAXLISTINDEX)
#define TOKU_MAXLISTINDEX       (TOKU_MAXINT - 1)
#endif



/*
** Maximum length for short strings, that is, strings that are
** internalized. (Cannot be smaller than reserved words or keys for
** metamethods, as these strings must be internalized;
** strlen("continue") = 8, strlen("__getidx") = 8.)
*/
#if !defined(TOKUI_MAXSHORTLEN)
#define TOKUI_MAXSHORTLEN       40
#endif



/*
** Size of cache for strings in the API. 'N' is the number of
** sets (better be a prime) and "M" is the size of each set (M == 1
** makes a direct cache.)
*/
#if !defined(TOKUI_STRCACHE_N)
#define TOKUI_STRCACHE_N        53  /* cache lines */
#define TOKUI_STRCACHE_M        2   /* cache line size * sizeof(OString*) */
#endif



/*
** Minimum size for string buffer during lexing, this buffer memory
** will be freed after compilation.
*/
#if !defined(TOKUI_MINBUFFER)
#define TOKUI_MINBUFFER         32
#endif



/*
** Maximum depth for nested C calls, syntactical nested non-terminals,
** and other features implemented through recursion in C. (Value must
** fit in a 16-bit unsigned integer. It must also be compatible with
** the size of the C stack.)
*/
#if !defined(TOKUI_MAXCCALLS)
#define TOKUI_MAXCCALLS         200
#endif



/*
** Runs each time program enters ('toku_lock') and leaves ('toku_unlock')
** Tokudae core (C API).
*/
#if !defined(toku_lock)
#define toku_lock(T)            ((void)0)
#define toku_unlock(T)          ((void)0)
#endif



/*
** These macros allow user-defined action to be taken each time
** thread is created/deleted and/or state is opened/closed.
*/
#if !defined(tokui_userstateopen)
#define tokui_userstateopen(T)        ((void)(T))
#endif

#if !defined(tokui_userstateclose)
#define tokui_userstateclose(T)       ((void)(T))
#endif

#if !defined(tokui_userstatethread)
#define tokui_userstatethread(T,C1)   ((void)(T))
#endif

#if !defined(tokui_userstatefree)
#define tokui_userstatefree(T,C1)     ((void)(T))
#endif



/*
** @UNUSED - marks variable unused to avoid compiler
** warnings.
*/
#if !defined(UNUSED)
#define UNUSED(x)       ((void)(x))
#endif


/* type casts (a macro highlights casts in the code) */
#define cast(t, e)          ((t)(e))

#define cast_void(e)        cast(void,(e))
#define cast_voidp(e)       cast(void *,(e))
#define cast_num(e)         cast(toku_Number,(e))
#define cast_Integer(e)     cast(toku_Integer,(e))
#define cast_ubyte(e)       cast(t_ubyte,(e))
#define cast_ubytep(e)      cast(t_ubyte *,(e))
#define cast_byte(e)        cast(t_byte,(e))
#define cast_ushort(e)      cast(t_ushort,(e))
#define cast_int(e)         cast(int,(e))
#define cast_uint(e)        cast(t_uint,(e))
#define cast_umem(e)        cast(t_umem,(e))
#define cast_mem(e)         cast(t_mem,(e))
#define cast_charp(e)       cast(char *,(e))
#define cast_char(e)        cast(char,(e))
#define cast_sizet(e)       cast(size_t,(e))


/* cast a signed toku_Integer to toku_Unsigned */
#if !defined(t_castS2U)
#define t_castS2U(i)        ((toku_Unsigned)(i))
#endif

/*
** Cast a toku_Unsigned to a signed toku_Integer; this cast is
** not strict ISO C, but two-complement architectures should
** work fine.
*/
#if !defined(t_castU2S)
#define t_castU2S(i)        ((toku_Integer)(i))
#endif

/* 
** Cast a size_t to toku_Integer: These casts are always valid for
** sizes of Tokudae objects (see TOKU_MAXSIZE).
*/
#define cast_sz2S(sz)       ((toku_Integer)(sz))

/* 
** Casts a ptrdiff_t to size_t when it is known that the minuend
** comes from the subtrahend (the base).
*/
#define cast_diff2sz(df)    ((size_t)(df))

/* cast a ptrdiff_t to toku_Integer */
#define cast_diff2S(df)     cast_sz2S(cast_diff2sz(df))


/* literal length */
#if !defined(LL)
#define LL(sl)      (sizeof(sl) - 1)
#endif


#if !defined(t_nummod)

/* modulo 'a - floor(a/b)*b' */
#define t_nummod(T,a,b,m) \
        { (void)(T); (m) = t_mathop(fmod)(a, b); \
          if (((m) > 0) ? (b) < 0 : ((m) < 0 && (b) > 0)) (m) += (b); }

#define t_numdiv(T, a, b)       ((void)(T), (a)/(b))

#define t_numidiv(T, a, b)      ((void)(T), t_floor(t_numdiv(T,a,b)))

#define t_numpow(T, a, b) \
        ((void)(T), (b) == 2 ? (a)*(a) : t_mathop(pow)(a, b))

#endif


#if !defined(t_numadd)
#define t_numadd(T, a, b)       ((void)(T), (a) + (b))
#define t_numsub(T, a, b)       ((void)(T), (a) - (b))
#define t_nummul(T, a, b)       ((void)(T), ((a) * (b)))
#define t_numunm(T, a)          ((void)(T), -(a))
#endif


#if !defined(t_numeq)
#define t_numeq(a, b)       ((a) == (b))
#define t_numne(a, b)       (!t_numeq(a, b))
#define t_numlt(a, b)       ((a) < (b))
#define t_numle(a, b)       ((a) <= (b))
#define t_numgt(a, b)       ((a) > (b))
#define t_numge(a, b)       ((a) >= (b))
#endif


#if !defined(t_numisnan)
#define t_numisnan(a)       (!t_numeq(a, a))
#endif


/*
** Macro to control inclusion of some hard tests on stack reallocation.
*/
#if !defined(TOKUI_HARDSTACKTESTS)
#define condmovestack(T,pre,pos)    ((void)0)
#else
/* realloc stack keeping its size */
#define condmovestack(T,pre,pos)  \
    { int sz_ = stacksize(T); pre; tokuT_reallocstack((T), sz_, 0); pos; }
#endif


/*
** Does one step of collection when debt becomes zero. 'pre'/'pos'
** allows some adjustments to be done only when needed. macro
** 'condchangemem' is used only for heavy tests (forcing a full
** GC cycle on every opportunity)
*/

#if !defined(TOKUI_HARDMEMTESTS)
#define condchangemem(T,pre,pos,emg)	((void)0)
#else
#define condchangemem(T,pre,pos,emg)  \
	{ if (gcrunning(G(T))) { pre; tokuG_fullinc(T, emg); pos; } }
#endif


/* write a message to 'fp' stream */
#if !defined(toku_writelen)
#define toku_writelen(fp,s,l)         fwrite((s), sizeof(char), (l), fp)
#define toku_writeline(fp)            (toku_writelen(fp, "\n", 1), fflush(fp))
#define toku_writefmt(fp, msg, ...)   (fprintf(fp, msg, __VA_ARGS__), fflush(fp))
#define toku_writevfmt(fp,msg,ap)     (vfprintf(fp, msg, ap), fflush(fp))
#endif


#endif
