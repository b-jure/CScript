#ifndef SKLIMITS_H
#define SKLIMITS_H

#include "skooma.h"

#define __STDC_LIMIT_MACROS
#include <limits.h>


/* ====================== Language limits ====================== */

#define VM_ARG_LIMIT  SK_ARG_MAX
#define VM_RET_LIMIT  SK_ARG_MAX
#define VM_LVAR_LIMIT SK_LVAR_MAX
#define VM_GVAR_LIMIT SK_GVAR_MAX
#define VM_JMP_LIMIT  SK_JMP_MAX

/* -------------------------------------------------------- */




/* ====================== VM limits ====================== */

#define _FALLBACK_STACK 20000
#define _STACK_CNT      cast_uint(SK_STACK_MAX / sizeof(Value))
#define _STACK_SIZE                                                                      \
    (_STACK_CNT == 0 ? _FALLBACK_STACK                                                   \
                     : (_STACK_CNT > SK_BYTECODE_MAX ? SK_BYTECODE_MAX : _STACK_CNT))

#define VM_STACK_LIMIT     _STACK_SIZE
#define VM_CALLSTACK_LIMIT SK_CALLFRAMES_MAX
#define VM_GRAYSTACK_LIMIT UINT64_MAX

/* -------------------------------------------------------- */


#endif
