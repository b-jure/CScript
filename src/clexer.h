/*
** clexer.h
** Scanner
** See Copyright Notice in cscript.h
*/

#ifndef clexer_h
#define clexer_h

#include "creader.h"
#include "cobject.h"



/* multi-char tokens start at this numeric value */
#define FIRSTTK		(UCHAR_MAX + 1)


#if !defined(CS_ENV)
#define CS_ENV      "__ENV"
#endif


/*
** WARNING: if you change the order of this enumeration, grep
** "ORDER TK".
*/
enum TK {
    /* keyword tokens */
    TK_AND = FIRSTTK, TK_BREAK, TK_CASE, TK_CONTINUE, TK_CLASS,
    TK_DEFAULT, TK_ELSE, TK_FALSE, TK_FOR, TK_FOREACH, TK_FN, TK_IF,
    TK_IN, TK_INHERITS, TK_NIL, TK_OR, TK_RETURN, TK_SUPER,
    TK_SWITCH, TK_TRUE, TK_WHILE, TK_LOOP, TK_LOCAL, TK_INF, TK_INFINITY,
    /* other multi-char tokens */
    TK_IDIV, TK_NE, TK_EQ, TK_GE, TK_LE, TK_SHL, TK_SHR, TK_POW, TK_CONCAT,
    TK_DOTS, TK_EOS,
    /* literal tokens */
    TK_FLT, TK_INT, TK_STRING, TK_NAME,
};

/* number of reserved keywords */
#define NUM_KEYWORDS	((TK_INFINITY - (FIRSTTK)) + 1)



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
    Table *tab; /* scanner table */
    struct cs_State *C;
    struct FunctionState *fs;
    BuffReader *br; /* buffered reader */
    Buffer *buff; /* string buffer */
    struct ParserState *ps; /* dynamic data used by parser */
    OString *src; /* current source name */
    OString *envn; /* environment variable */
} Lexer;


#define csY_newliteral(lx, l)   csY_newstring(lx, "" (l), LL(l))

CSI_FUNC void csY_setinput(cs_State *C, Lexer *lx, BuffReader *br,
                           OString *source);
CSI_FUNC void csY_init(cs_State *C);
CSI_FUNC const char *csY_tok2str(Lexer *lx, int t);
CSI_FUNC OString *csY_newstring(Lexer *lx, const char *str, size_t len);
CSI_FUNC c_noret csY_syntaxerror(Lexer *lx, const char *err);
CSI_FUNC void csY_scan(Lexer *lx);
CSI_FUNC int csY_scanahead(Lexer *lx);

#endif
