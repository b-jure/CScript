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


/* multi-char tokens start at this numeric value */
#define FIRSTTK		(UCHAR_MAX + 1)

/* number of Cript keywords */
#define NUM_KEYWORDS	((TK_CONST - (FIRSTTK)) + 1)



enum TK {
	TK_AND = FIRSTTK, TK_BREAK, TK_CASE, TK_CONTINUE,
	TK_CLASS, TK_DEFAULT, TK_ELSE, TK_FALSE, TK_FOR,
	TK_FOREACH, TK_FN, TK_IF, TK_IN, TK_INHERITS, TK_NIL,
	TK_OR, TK_RETURN, TK_SUPER, TK_SELF, TK_SWITCH, TK_TRUE,
	TK_LET, TK_WHILE, TK_LOOP, TK_CONST,
	TK_NE, TK_EQ, TK_GE, TK_LE, TK_SHL, TK_SHR,
	TK_DOTS, TK_EOS,
	TK_FLT, TK_INT, TK_STRING, TK_IDENTIFIER,
};



/* storage for literals */
typedef union {
	cr_integer i;
	cr_number n;
	OString *str;
} Literal;


typedef struct {
	int tk;
	Literal k;
} Token;


/* buffer for strings */
Vec(Buffer, char);


typedef struct Lexer {
	struct VM *vm;
	struct FunctionState *fs;
	HTable tab; /* prevent string literal collection */
	BuffReader *br; /* buffered reader */
	Buffer buff; /* buffer for tokens */
	Token t; /* current token */
	Token tahead; /* lookahead token */
	OString *src; /* current source name */
	int c; /* current char */
	int lastline; /* line of previous token */
	int line; /* current line number */
} Lexer;



void cr_lr_init(VM *vm, Lexer *lx, BuffReader *br, OString *src);
const char *cr_lx_tok2str(Lexer *lx, int t);
void cr_lr_syntaxerror(Lexer *lx, const char *err);
Token cr_lr_syntoken(const char *name);
void cr_lr_scan(Lexer *lx);
int cr_lr_scanahead(Lexer *lx);

#endif
