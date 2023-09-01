#ifndef __SKOOMA_COMPILER_H__
#define __SKOOMA_COMPILER_H__

#include "chunk.h"
#include "common.h"
#include "scanner.h"
#include "vmachine.h"

bool compile(VM* vm, const char *source, Chunk *chunk);

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

typedef void (*ParseFn)(VM *, Scanner *, bool);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

#endif
