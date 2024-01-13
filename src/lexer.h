#ifndef SKOOMA_LEXER_H
#define SKOOMA_LEXER_H

#include "common.h"
#include "mem.h"
#include "reader.h"
#include "value.h"

#include <stdarg.h>

typedef enum {
    // Single character tokens.
    TOK_LBRACK = 0,
    TOK_RBRACK,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_DOT,
    TOK_DOT_DOT_DOT,
    TOK_COMMA,
    TOK_MINUS,
    TOK_PLUS,
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_SLASH,
    TOK_STAR,
    TOK_PERCENT,
    TOK_CARET,
    TOK_QMARK,
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
    TOK_BREAK,
    TOK_CASE,
    TOK_CONTINUE,
    TOK_CLASS,
    TOK_DEFAULT,
    TOK_ELSE,
    TOK_FALSE,
    TOK_FOR,
    TOK_FOREACH,
    TOK_FN,
    TOK_IF,
    TOK_IN,
    TOK_IMPL,
    TOK_NIL,
    TOK_OR,
    TOK_RETURN,
    TOK_SUPER,
    TOK_SELF,
    TOK_SWITCH,
    TOK_TRUE,
    TOK_VAR,
    TOK_WHILE,
    TOK_LOOP,
    TOK_FIXED,

    TOK_ERROR,
    TOK_EOF
} TokenType;

typedef struct {
    TokenType type;
    const char* start; // slice start
    uint8_t len; // slice length (LEX_TOKEN_LEN_LIMIT)
    uint32_t line; // source file line
    Value value; // constant value
} Token;

typedef struct {
    VM* vm; // virtual machine
    BuffReader* br; // buffered reader
    int32_t c; // current char
    Array_Byte buffer; // for tokens
    Token previous;
    Token current;
    uint32_t line; // source file line
    bool skip; // skip current token (LEX_TOKEN_LIMIT reached)
    bool panic; // sync flag
    bool error; // parse error flag
} Lexer; // Lexer



void L_init(Lexer* L, VM* vm, BuffReader* br);
void L_free(Lexer* L);
Token scan(Lexer* lexer);
Token syntoken(const char* name);
void regcomperror(Lexer* lexer, const char* err, va_list args);

#endif
