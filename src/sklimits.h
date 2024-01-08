#ifndef SKLIMITS_H
#define SKLIMITS_H

#include "skooma.h"

#define __STDC_LIMIT_MACROS
#include <limits.h>


/* ====================== Language limits ====================== */

/* Limit of constants created in a function. */
#define VM_CONST_LIMIT SK_CONST_MAX
/* Limit of function arguments provided. */
#define VM_ARG_LIMIT SK_ARG_MAX
/* Limit of return values. */
#define VM_RET_LIMIT SK_ARG_MAX
/* Limit of local variable defines. */
#define VM_LVAR_LIMIT SK_LVAR_MAX
/* Limit of global variable defines. */
#define VM_GVAR_LIMIT SK_GVAR_MAX
/* Limit of code jump size. */
#define VM_JMP_LIMIT SK_JMP_MAX

/* -------------------------------------------------------- */


/* ====================== VM limits ====================== */

#define _FALLBACK_STACK 20000
#define _STACK_CNT      cast_uint(SK_STACK_MAX / sizeof(Value))
#define _STACK_SIZE                                                                      \
    (_STACK_CNT == 0 ? _FALLBACK_STACK                                                   \
                     : (_STACK_CNT > SK_BYTECODE_MAX ? SK_BYTECODE_MAX : _STACK_CNT))

/* Virtual Machine stack limit size. */
#define VM_STACK_LIMIT _STACK_SIZE
/* Virtual Machine functions called limit. */
#define VM_CALLSTACK_LIMIT SK_CALLFRAMES_MAX
/* Virtual Machine garbage collector gray-stack size limit. */
#define VM_GRAYSTACK_LIMIT UINT64_MAX

/* -------------------------------------------------------- */


/* ====================== Type limits ====================== */

/* How many digits/chars/bytes sk_number has when converted into string. */
#define SK_NDIGITS 30

/* -------------------------------------------------------- */


#endif
