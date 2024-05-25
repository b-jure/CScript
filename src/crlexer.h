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


#ifndef CRLEXER_H
#define CRLEXER_H

#include "crvm.h"
#include "crcommon.h"
#include "crmem.h"
#include "crreader.h"
#include "crvalue.h"
#include "crvec.h"

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
    cr_ubyte len; // slice length (LEX_TOKEN_LEN_LIMIT)
    cr_uint line; // source file line
    Value value; // constant value
} Token;


typedef struct {
    VM* vm; // virtual machine
    BuffReader* br; // buffered reader
    cr_ubyte c; // current char
    ubyteVec buffer; // for tokens
    Token previous;
    Token current;
    Value src; // current source name
    cr_uint line; // source file line
    cr_ubyte skip; // skip current token (LEX_TOKEN_LIMIT reached)
    cr_ubyte panic; // sync flag
    cr_ubyte error; // parse error flag
} Lexer;


void initlexer(Lexer* L, VM* vm, BuffReader* br, Value source);
void freelexer(Lexer* L);
Token scan(Lexer* lexer);
Token syntoken(const char* name);
void regcomperror(Lexer* lexer, const char* err, va_list args);

#endif
