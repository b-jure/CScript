/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

/* internal macros, limits and types */
#ifndef CRLIMITS_H
#define CRLIMITS_H

#include "cript.h"

#include <limits.h>



/*
 * Signed and unsigned types that represent count 
 * in bytes of total memory used by cript.
 */
typedef size_t cr_umem;
typedef ptrdiff_t cr_mem;

#define CRUMEM_MAX	((cr_umem)(~(cr_umem)(0)))
#define CRMEM_MAX	((cr_mem)(CR_UMEM_MAX >> 1))


/*
 * Used for representing small signed/unsigned
 * numbers instead of declaring them 'char'.
 */
typedef unsigned char cr_ubyte;
typedef signed char cr_byte;

#define CRUBYTE_MAX	((cr_ubyte)(~(cr_ubyte)(0)))
#define CRBYTE_MAX	((cr_ubyte)(CR_UBYTE_MAX >> 1))



/* 
 * Maximum size visible for cript.
 * It must be less than what is representable by 'cr_integer'. 
 */
#define MAXSIZE		(sizeof(size_t) < sizeof(cr_integer) ? \
				(SIZE_MAX) : (size_t)(CR_INTEGER_MAX))



/* convert pointer 'p' to 'unsigned int' */
#define pointer2uint(p)		((unsigned int)((uintptr_t)(p)&(UINT_MAX)))



/* internal assertions for debugging */
#if defined(CRI_DEBUG_ASSERT)
#undef NDEBUG
#include <assert.h>
#define cr_assert(e)		assert(e)
#endif

#if defined(cr_assert)
#define check_exp(c,e)		(cr_assert(c),(e))
#else
#define cr_assert(e)		((void)0)
#define check_exp(c,e)		(e)
#endif

/* C API assertions */
#if !defined(cri_checkapi)
#define cri_checkapi(vm,e)	((void)vm, cr_assert(e))
#endif

#define checkapi(vm,e,err)	cri_checkapi(vm,(e) && err)



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
#define cr_inline	__inline__
#else
#define cr_inline	inline
#endif

/* static inline */
#define cr_sinline	static cr_inline



/* non-return type */
#if defined(__GNUC__)
#define cr_noret	void __attribute__((noreturn))
#elif defined(_MSC_VEC) && _MSC_VEC >= 1200
#define cr_noret	void __declspec(noreturn)
#else
#define cr_noret	void
#endif



/* mark unreachable code (optimization) */
#if defined(__GNUC__)
#define cr_unreachable	__builtin_unreachable()
#else
#define cr_unreachable \
	{ #include<stdlib.h> \
	  cr_assert(0 && "unreachable"); \
	  abort(); }

#endif



/* 
 * Type for virtual-machine instructions 
 * Instructions (opcodes) are 1 byte in size not including
 * the arguments; arguments vary in size (short/long) and
 * more on that in 'cropcode.h'.
 */
typedef cr_ubyte Instruction;



/* 
 * Maximum instruction parameter size.
 * This is the maximum unsigned value that fits in 3 bytes.
 * Transitively this defines various compiler limits. 
 */
#define CR_MAXCODE		16777215



/*
 * Initial size for the interned strings table.
 * It has to be power of 2, because of the implementation
 * of the table itself.
 */
#if !defined(CR_MINSTRTABSIZE)
#define CR_MINSTRTABSIZE	64
#endif



/* minimum size for string buffer */
#if !defined(CR_MINBUFFER)
#define CR_MINBUFFER	32
#endif



/* maximum table load factor */
#if !defined(CR_MAXTABLOAD)
#define CR_MAXTABLOAD	0.70
#endif



/* 
 * Maximum call depth for nested C calls including the
 * parser limit for syntactically nested non-terminals and
 * other features implemented through recursion in C.
 * Any value will suffice as long as it fits in 'unsigned short'.
 * By design smaller type (unsigned short) is chosen than the stack
 * size so you can't mess up.
 */
#define CR_MAXCCALLS	4096



/* 
 * Runs each time program enters ('cr_lock') and
 * leaves ('cr_unlock') cript core (C API).
 */
#if !defined(cr_lock)
#define	cr_lock(vm)	((void)0)
#define	cr_unlock(vm)	((void)0)
#endif



/*
 * These allow user-defined action to be taken each
 * time VM (thread) is created or deleted.
 */
#if !defined(cri_threadcreated)
#define cri_threadcreated(vm)	((void)(vm))
#endif

#if !defined(cri_threaddelete)
#define cri_threaddelete(vm)	((void)(vm))
#endif



/* 
 * @MAX - return maximum value.
 * @MIN - return minimum value.
 */
#if defined(__GNUC__)
#define MAX(a, b) \
	({ __typeof__(a) _a = (a); \
	   __typeof__(b) _b = (b); \
	   _a > _b ? _a : _b; })

#define MIN(a, b) \
	({ __typeof__(a) _a = (a); \
	   __typeof__(b) _b = (b); \
	   _a > _b ? _b : _a; })
#else
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif



/* 
 * @UNUSED - marks variable unused to avoid compiler
 * warnings. 
 */
#ifndef UNUSED
#define UNUSED(x) ((void)(x))
#endif



/* @cast - cast expression 'e' as type 't'. */
#define cast(t, e)	((t)(e))

#define cast_ubyte(e)	cast(cr_ubyte,(e))
#define cast_byte(e)	cast(cr_byte,(e))
#define cast_num(e)	cast(cr_number,(e))
#define cast_int(e)	cast(int,(e))
#define cast_uint(e)	cast(unsigned int,(e))


/* @cast_umem - cast expression 'e' as 'cr_umem'. */
#define cast_umem(e)	cast(cr_umem, (e))

/* cast 'cr_integer' to 'cr_uinteger' */
#define cr_castS2U(i)	((cr_uinteger)(i))

/* cast 'cr_uinteger' to 'cr_integer' */
#define cr_castU2S(i)	((cr_integer)(i))



/* string literal length */
#define SLL(sl) (sizeof(sl) - 1)



/* @cr_nummod - modulo 'a - floor(a/b)*b'. */
#define cr_nummod(vm,a,b,m) { \
	(m)=fmod((a),(b)); \
	if (((m) > 0) ? (b)<0 : ((m)<0 && (b)>0)) (m)+=(b); }

/* @cr_numdiv - float division. */
#ifndef cr_numdiv
#define cr_numdiv(vm, a, b)	((a)/(b))
#endif

/* @cr_numidiv - floor division (or division between integers). */
#ifndef cr_numidiv
#define cr_numidiv(vm, a, b)	(floor(cr_numdiv(a, b))
#endif

/* @cr_numpow - exponentiation. */
#ifndef cr_numpow
#define cr_numpow(vm, a, b)	((b)==2 ? (a)*(a) : pow((a),(b)))
#endif

/* 
 * @cr_numadd - addition.
 * @cr_numsub - subtraction.
 * @cr_nummul - multiplication.
 * @cr_numunm - negation.
 */
#ifndef cr_numadd
#define cr_numadd(vm, a, b) 	((a)+(b))
#define cr_numsub(vm, a, b) 	((a)-(b))
#define cr_nummul(vm, a, b) 	((a)*(b))
#define cr_numunm(vm, a)	(-(a))
#endif

/* 
 * @cr_numeq - ordering equal.
 * @cr_numne - ordering not equal.
 * @cr_numlt - ordering less than.
 * @cr_numle - ordering less equal.
 * @cr_numgt - ordering greater than.
 * @cr_numge - ordering greater equal.
 */
#ifndef cr_numeq
#define cr_numeq(a, b)		((a)==(b))
#define cr_numne(a, b) 		(!cr_numeq(a,b))
#define cr_numlt(a, b) 		((a)<(b))
#define cr_numle(a, b) 		((a)<=(b))
#define cr_numgt(a, b) 		((a)>(b))
#define cr_numge(a, b) 		((a)>=(b))
#endif

/* @cr_numisnan - check if number is 'NaN'. */
#ifndef cr_numisnan
#define cr_numisnan(a)		(!cr_numeq(a,a))
#endif


#endif
