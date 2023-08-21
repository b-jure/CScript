#ifndef __SKOOMA_SCANNER_H__
#define __SKOOMA_SCANNER_H__

#include "common.h"

typedef enum {
  // Single character tokens.
  TOK_LPAREN = 0,
  TOK_RPAREN,
  TOK_LBRACE,
  TOK_RBRACE,
  TOK_DOT,
  TOK_COMMA,
  TOK_MINUS,
  TOK_PLUS,
  TOK_SEMICOLON,
  TOK_SLASH,
  TOK_STAR,
  // One or two character tokens.
  TOK_BANG,
  TOK_BANG_EQUAL,
  TOK_EQUAL,
  TOK_EQUAL_EQUAL,
  TOK_GREATER,
  TOK_GREATER_EQUAL,
  TOK_LESS,
  TOK_LESS_EQUAL,
  // Literals.
  TOK_IDENTIFIER,
  TOK_STRING,
  TOK_NUMBER,
  // Keywords.
  TOK_AND,
  TOK_CLASS,
  TOK_ELSE,
  TOK_FALSE,
  TOK_FOR,
  TOK_FN,
  TOK_IF,
  TOK_IMPL,
  TOK_DUST, /* Nil, NULL, ... */
  TOK_OR,
  TOK_PRINT,
  TOK_RETURN,
  TOK_SUPER,
  TOK_SELF,
  TOK_TRUE,
  TOK_VAR,
  TOK_WHILE,

  TOK_ERROR,
  TOK_EOF
} TokenType;

typedef struct {
  TokenType type;
  const char *start;
  UInt len;
  UInt line;
} Token;

void Scanner_init(const char *source);
Token Scanner_scan(void);

#endif
