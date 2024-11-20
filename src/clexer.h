/*
** clexer.h
** Scanner
** See Copyright Notice in cscript.h
*/

#ifndef CRLEXER_H
#define CRLEXER_H

#include "creader.h"
#include "cobject.h"



/* multi-char tokens start at this numeric value */
#define FIRSTTK		(UCHAR_MAX + 1)


/*
** Top-level ("main") CScript closures run in their own sandboxed environment,
** meaning any declared global variable inside of a CScript closure is
** actually getting set as the field in "__ENV" hashtable which itself is
** the first and only upvalue in the top-level ("main") CScript closure.
*/
#if !defined(CS_ENV)
#define CS_ENV	        "__ENV"
#endif


/*
** WARNING: if you change the order of this enumeration, grep
** "ORDER TK".
*/
enum TK {
    /* keyword tokens */
    TK_AND = FIRSTTK, TK_BREAK, TK_CASE, TK_CONTINUE, TK_CLASS,
    TK_DEFAULT, TK_ELSE, TK_FALSE, TK_FOR, TK_EACH, TK_FN, TK_IF,
    TK_IN, TK_INHERITS, TK_NIL, TK_NOT, TK_OR, TK_RETURN, TK_SUPER,
    TK_SWITCH, TK_TRUE, TK_WHILE, TK_LOOP, TK_FINAL, TK_LOCAL,
    /* other multi-char tokens */
    TK_NE, TK_EQ, TK_GE, TK_LE, TK_SHL, TK_SHR, TK_POW, TK_CONCAT,
    TK_DOTS, TK_EOS,
    /* literal tokens */
    TK_FLT, TK_INT, TK_STRING, TK_NAME,
};

/* number of reserved keywords */
#define NUM_KEYWORDS	((TK_FINAL - (FIRSTTK)) + 1)



/* scanner literals */
typedef union {
    cs_Integer i;
    cs_Number n;
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
    struct cs_State *ts;
    struct FunctionState *fs;
    BuffReader *br; /* buffered reader */
    Buffer *buff; /* string buffer */
    struct ParserState *ps; /* dynamic data used by parser */
    OString *src; /* current source name */
    OString *envname; /* name of environment variable */
} Lexer;


CSI_FUNC void csY_setsource(cs_State *ts, Lexer *lx, BuffReader *br,
                            OString *source);
CSI_FUNC void csY_init(cs_State *ts);
CSI_FUNC const char *csY_tok2str(Lexer *lx, int t);
CSI_FUNC OString *csY_newstring(Lexer *lx, const char *str, size_t len);
CSI_FUNC cs_noret csY_syntaxerror(Lexer *lx, const char *err);
CSI_FUNC void csY_scan(Lexer *lx);
CSI_FUNC int csY_scanahead(Lexer *lx);

#endif
