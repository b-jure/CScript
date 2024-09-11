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

#include "crreader.h"
#include "crobject.h"



/* multi-char tokens start at this numeric value */
#define FIRSTTK		(UCHAR_MAX + 1)

/* number of Cript keywords */
#define NUM_KEYWORDS	((TK_FINAL - (FIRSTTK)) + 1)



enum TK {
    /* keyword tokens */
    TK_AND = FIRSTTK, TK_BREAK, TK_CASE, TK_CONTINUE,
    TK_CLASS, TK_DEFAULT, TK_ELSE, TK_FALSE, TK_FOR,
    TK_EACH, TK_FN, TK_IF, TK_IN, TK_INHERITS, TK_NIL,
    TK_OR, TK_RETURN, TK_SUPER, TK_SELF, TK_SWITCH, TK_TRUE,
    TK_LET, TK_WHILE, TK_LOOP, TK_FINAL, TK_PRIVATE,
    /* other multi-char tokens */
    TK_NE, TK_EQ, TK_GE, TK_LE, TK_SHL, TK_SHR,
    TK_POW, TK_RANGE, TK_DOTS, TK_EOS,
    /* literal tokens */
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
    Literal lit;
} Token;


typedef struct Lexer {
    int c; /* current char */
    int lastline; /* line of previous token */
    int line; /* current line number */
    Token t; /* current token */
    Token tahead; /* lookahead token */
    HTable *tab; /* scanner table */
    struct cr_State *ts;
    struct FunctionState *fs;
    BuffReader *br; /* buffered reader */
    Buffer *buff; /* string buffer */
    struct ParserState *ps; /* dynamic data used by parser */
    OString *src; /* current source name */
    OString *env; /* name of environment variable */
} Lexer;


CRI_FUNC void crL_setsource(cr_State *ts, Lexer *lx, BuffReader *br,
                            OString *source);
CRI_FUNC void crL_init(cr_State *ts);
CRI_FUNC const char *crL_tok2str(Lexer *lx, int t);
CRI_FUNC OString *crL_newstring(Lexer *lx, const char *str, size_t len);
CRI_FUNC cr_noret crL_syntaxerror(Lexer *lx, const char *err);
CRI_FUNC void crL_scan(Lexer *lx);
CRI_FUNC int crL_scanahead(Lexer *lx);

#endif
