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

#include "crmem.h"
#include "crreader.h"
#include "crvalue.h"
#include "crobject.h"

#include <stdarg.h>


typedef enum {
	/* single/double character tokens */
	TOK_LBRACK = 0, TOK_RBRACK, TOK_LPAREN, TOK_RPAREN, TOK_LBRACE,
	TOK_RBRACE, TOK_DOT, TOK_DOT_DOT_DOT, TOK_COMMA, TOK_MINUS,
	TOK_PLUS, TOK_COLON, TOK_SEMICOLON, TOK_SLASH, TOK_STAR,
	TOK_PERCENT, TOK_CARET, TOK_QMARK, TOK_BANG, TOK_BANG_EQUAL,
	TOK_EQUAL, TOK_EQUAL_EQUAL, TOK_GREATER, TOK_GREATER_EQUAL,
	TOK_LESS, TOK_LESS_EQUAL,
	/* literals */
	TOK_IDENTIFIER, TOK_STRING, TOK_NUMBER,
	/* keywords */
	TOK_AND, TOK_BREAK, TOK_CASE, TOK_CONTINUE, TOK_CLASS,
	TOK_DEFAULT, TOK_ELSE, TOK_FALSE, TOK_FOR, TOK_FOREACH,
	TOK_FN, TOK_IF, TOK_IN, TOK_IMPL, TOK_NIL, TOK_OR, TOK_RETURN,
	TOK_SUPER, TOK_SELF, TOK_SWITCH, TOK_TRUE, TOK_VAR, TOK_WHILE,
	TOK_LOOP, TOK_FIXED,
	/* special */
	TOK_ERROR, TOK_EOF
} TType;



/* constant values for tokens */
typedef union {
	cr_integer i;
	cr_number n;
	OString *str;
} KValue;



typedef struct {
	TType tt;
	KValue k;
} Token;


/* buffer for strings */
Vec(Buffer, char);


typedef struct Lexer {
	struct VM *vm;
	struct FunctionState *fs;
	BuffReader *br; /* buffered reader */
	Buffer buf; /* buffer for tokens */
	Token previous;
	Token current;
	OString *src; /* current source name */
	int c; /* current char */
	int currline; /* 'current' token line */
	int prevline; /* 'previous' token line */
	cr_ubyte skip; /* skip current token */
} Lexer;


void cr_lr_init(VM *vm, Lexer *lx, BuffReader *br, OString *source);
void cr_lr_free(Lexer *lx);
Token cr_lr_scan(Lexer *lx);
Token cr_lr_syntoken(const char *name);
void cr_lr_syntaxerror(Lexer *lx, const char *err, va_list args);

#endif
