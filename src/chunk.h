#ifndef __SKOOMA_CHUNK_H__
#define __SKOOMA_CHUNK_H__

#include "array.h"
#include "common.h"
#include "value.h"

typedef enum {
  OP_CONST = 0,     /* Store 8-bit Value index */
  OP_CONSTL,        /* Store 24-bit Value index */
  OP_TRUE,          /* Store true (bool) literal */
  OP_FALSE,         /* Store false (bool) literal */
  OP_NIL,           /* Store nil (NULL) literal */
  OP_NEG,           /* Unary negation */
  OP_ADD,           /* Binary addition */
  OP_SUB,           /* Binary subtraction */
  OP_MUL,           /* Binary multiplication */
  OP_DIV,           /* Binary division */
  OP_NOT,           /* Unary not */
  OP_NOT_EQUAL,     /* Binary 'not equal' comparison */
  OP_EQUAL,         /* Binary 'equality' comparison */
  OP_GREATER,       /* Binary 'greater than' comparison */
  OP_GREATER_EQUAL, /* Binary 'greater than or equal to' comparison */
  OP_LESS,          /* Binary 'less than' comparison */
  OP_LESS_EQUAL,    /* BInary 'less than or equal to' comparison */
  OP_RET,           /* Return instruction */
} OpCode;

typedef UIntArray LineArray;

typedef struct {
  LineArray lines;
  ValueArray constants;
  ByteArray code;
} Chunk;

void Chunk_init(Chunk *chunk);
void Chunk_write(Chunk *chunk, Byte byte, UInt line);
void Chunk_write_constant(Chunk *chunk, Value constant, UInt line);
UInt Chunk_getline(Chunk *chunk, UInt index);
void Chunk_free(Chunk *chunk);

#endif
