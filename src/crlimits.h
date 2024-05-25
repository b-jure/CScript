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

#ifndef CRLIMITS_H
#define CRLIMITS_H

#include "cript.h"

#define __STDC_LIMIT_MACROS
#include <limits.h>


/* ====================== Parser limits ====================== */
/* Limit of constants created in a function. */
#define PARSER_CONST_LIMIT	CR_CONST_MAX
/* Limit of function arguments provided. */
#define PARSER_ARG_LIMIT	CR_ARG_MAX
/* Limit of return values. */
#define PARSER_RET_LIMIT	CR_ARG_MAX
/* Limit of local variable defines. */
#define PARSER_LVAR_LIMIT	CR_LVAR_MAX
/* Limit of global variable defines. */
#define PARSER_GVAR_LIMIT	CR_GVAR_MAX
/* Limit of code jump size. */
#define PARSER_JMP_LIMIT	CR_JMP_MAX
/* -------------------------------------------------------- */


/* ====================== VM limits ====================== */
/* Virtual Machine stack limit size. */
#define VM_STACK_LIMIT		CR_STACK_MAX
/* Virtual Machine functions called limit. */
#define VM_CALLSTACK_LIMIT	CR_CALLFRAMES_MAX
/* Virtual Machine garbage collector gray-stack size limit. */
#define VM_GRAYSTACK_LIMIT	UINT64_MAX
/* -------------------------------------------------------- */


/* ==================== Lexer limits ==================== */
/* Token size limit in bytes. */
#define LEX_TOKEN_LEN_LIMIT 2000
/* ------------------------------------------------------ */


/* ====================== Conversion limits ====================== */
/* limit for 'cr_integer' is 20 digits/bytes + 1 sign byte + null term */
#define MAXINT2STR 22

/* limit for 'cr_floating' */
#define MAXFLT2STR 42

/* limit for pointer to 'void' is 8 digits/bytes + 2 hex bytes + null term */
#define MAXVOIDP2STR 11
/* -------------------------------------------------------- */


/* ====================== Format specifiers ====================== */
/* format specifer for 'cr_integer' */
#define INTEGERFMT "%ld"

/* format specifer for 'cr_floating' */
#define FLTFMT "%g"

/* format specifier for pointer */
#define PTRFMT "%p"
/* -------------------------------------------------------- */


/* ====================== Integer type limits ====================== */
/* maximum */
#define CR_UBYTE_MAX		bmax(cr_ubyte)
#define CR_BYTE_MAX    		((cr_byte)(CR_UBYTE_MAX >> 1))
#define CR_UINT_MAX    		bmax(cr_uint)
#define CR_INT_MAX     		((cr_int)(CR_UINT_MAX >> 1))
#define CR_ULINT_MAX   		bmax(cr_ulint)
#define CR_LINT_MAX    		((cr_lint)(CR_ULINT_MAX >> 1))
#define CR_UMEM_MAX    		bmax(cr_umem)
#define CR_MEM_MAX     		((cr_mem)(UMEM_MAX >> 1))
#define CR_HASH_MAX    		CR_ULINT_MAX
#define CR_UINTPTR_MAX 		bmax(cr_uintptr)
#define CR_INTPTR_MAX  		bmax(cr_intptr)
#define CR_INTEGER_MAX 		CR_LINT_MAX
/* minimum */
#define CR_UBYTE_MIN		bmax(cr_ubyte)
#define CR_BYTE_MIN    		bmax(cr_byte)
#define CR_INT_MIN     		bmax(cr_int)
#define CR_UINT_MIN    		bmax(cr_uint)
#define CR_LINT_MIN    		bmax(cr_lint)
#define CR_ULINT_MIN   		bmax(cr_ulint)
#define CR_UMEM_MIN    		bmax(cr_umem)
#define CR_MEM_MIN     		((cr_mem)(UMEM_MAX >> 1))
#define CR_HASH_MIN    		CR_ULINT_MIN
#define CR_UINTPTR_MIN 		bmax(cr_uintptr)
#define CR_INTPTR_MIN  		bmax(cr_intptr)
#define CR_INTEGER_MIN 		CR_LINT_MAX
/* -------------------------------------------------------- */


/* ====================== Other limits ====================== */
/* minimum ensured stack space */
#define EXTRASTACK 5

/* 'Vec' size limit */
#define VECSIZE_LIMIT	CR_BYTECODE_MAX
/* -------------------------------------------------------- */

#endif
