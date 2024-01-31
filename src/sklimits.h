/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef SKLIMITS_H
#define SKLIMITS_H

#include "skooma.h"

#define __STDC_LIMIT_MACROS
#include <limits.h>


/* ====================== Parser limits ====================== */

/* Limit of constants created in a function. */
#define PARSER_CONST_LIMIT SK_CONST_MAX
/* Limit of function arguments provided. */
#define PARSER_ARG_LIMIT SK_ARG_MAX
/* Limit of return values. */
#define PARSER_RET_LIMIT SK_ARG_MAX
/* Limit of local variable defines. */
#define PARSER_LVAR_LIMIT SK_LVAR_MAX
/* Limit of global variable defines. */
#define PARSER_GVAR_LIMIT SK_GVAR_MAX
/* Limit of code jump size. */
#define PARSER_JMP_LIMIT SK_JMP_MAX

/* -------------------------------------------------------- */




/* ====================== VM limits ====================== */

#define _FALLBACK_STACK 20000
#define _STACK_CNT cast_uint(SK_STACK_MAX / sizeof(Value))
#define _STACK_SIZE                                                                                \
    (_STACK_CNT == 0 ? _FALLBACK_STACK                                                             \
                     : (_STACK_CNT > SK_BYTECODE_MAX ? SK_BYTECODE_MAX : _STACK_CNT))

/* Virtual Machine stack limit size. */
#define VM_STACK_LIMIT _STACK_SIZE
/* Virtual Machine functions called limit. */
#define VM_CALLSTACK_LIMIT SK_CALLFRAMES_MAX
/* Virtual Machine garbage collector gray-stack size limit. */
#define VM_GRAYSTACK_LIMIT UINT64_MAX

/* -------------------------------------------------------- */




/* ==================== Lexer limits ==================== */

/* Token size limit in bytes. */
#define LEX_TOKEN_LEN_LIMIT 2000

/* ------------------------------------------------------ */



/* ====================== Conversion limits ====================== */

/* 'sk_number' byte limit when being converted to string. */
#define SK_NDIGITS 30

/* -------------------------------------------------------- */


#endif
