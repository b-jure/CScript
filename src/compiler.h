#ifndef __SKOOMA_COMPILER_H__
#define __SKOOMA_COMPILER_H__

#include "common.h"
#include "object.h"
#include "vmachine.h"

ObjFunction *compile(VM *vm, const char *source);

/* Precedence from LOW-est to HIGH-est */
typedef enum {
  PREC_NONE = 0,
  PREC_ASSIGNMENT,
  PREC_TERNARY,
  PREC_OR,
  PREC_AND,
  PREC_EQUALITY,
  PREC_COMPARISON,
  PREC_TERM,
  PREC_FACTOR,
  PREC_UNARY,
  PREC_CALL,
  PREC_PRIMARY
} Precedence;

#endif
