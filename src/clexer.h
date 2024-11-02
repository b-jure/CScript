/*
** clexer.h
** Lexical Analyzer
** See Copyright Notice in cscript.h
*/

#ifndef CRLEXER_H
#define CRLEXER_H

#include "creader.h"
#include "cobject.h"



/* multi-char tokens start at this numeric value */
#define FIRSTTK		(UCHAR_MAX + 1)

/* number of Cript keywords */
#define NUM_KEYWORDS	((TK_FINAL - (FIRSTTK)) + 1)



enum TK {
    /* keyword tokens */
    TK_AND = FIRSTTK, TK_BREAK, TK_CASE, TK_CONTINUE,
    TK_CLASS, TK_DEFAULT, TK_ELSE, TK_FALSE, TK_FOR,
    TK_EACH, TK_FN, TK_IF, TK_IN, TK_INHERITS, TK_NIL, TK_NOT,
    TK_OR, TK_RETURN, TK_SUPER, TK_SELF, TK_SWITCH, TK_TRUE,
    TK_LET, TK_WHILE, TK_LOOP, TK_FINAL, TK_PRIVATE,
    /* other multi-char tokens */
    TK_NE, TK_EQ, TK_GE, TK_LE, TK_SHL, TK_SHR,
    TK_POW, TK_CONCAT, TK_DOTS, TK_EOS,
    /* literal tokens */
    TK_FLT, TK_INT, TK_STRING, TK_IDENTIFIER,
};



/* storage for literals */
typedef union {
    cr_Integer i;
    cr_Number n;
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


CRI_FUNC void crY_setsource(cr_State *ts, Lexer *lx, BuffReader *br,
                            OString *source);
CRI_FUNC void crY_init(cr_State *ts);
CRI_FUNC const char *crY_tok2str(Lexer *lx, int t);
CRI_FUNC OString *crY_newstring(Lexer *lx, const char *str, size_t len);
CRI_FUNC cr_noret crY_syntaxerror(Lexer *lx, const char *err);
CRI_FUNC void crY_scan(Lexer *lx);
CRI_FUNC int crY_scanahead(Lexer *lx);

#endif
