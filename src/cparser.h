/*
** cparser.h
** CScript Parser
** See Copyright Notice in cscript.h
*/

#ifndef CPARSER_H
#define CPARSER_H


#include "cobject.h"
#include "clexer.h"
#include "cobject.h"
#include "cbits.h"


/* maximum number of local variables per function */
#define MAXVARS     MAX_LARG


/* 
** Because all strings are unified by the scanner, the parser
** can use pointer equality for string equality
*/
#define eqstr(a,b)	((a) == (b))


/* check expression type */
#define eisvar(e)           ((e)->et >= EXP_GLOBAL && (e)->et <= EXP_DOTSUPER)
#define eisconstant(e)      ((e)->et >= EXP_NIL && (e)->et <= EXP_K)
#define eismulret(e)        ((e)->et == EXP_CALL || (e)->et == EXP_VARARG)
#define eistrue(e)          ((e)->et >= EXP_TRUE && (e)->et <= EXP_K)
#define eisindexed(e)       ((e)->et >= EXP_INDEXED && (e)->et <= EXP_DOTSUPER)


/* expression types */
typedef enum expt {
    /* no expression */
    EXP_VOID,
    /* expression is nil constant */
    EXP_NIL,
    /* expression is false constant */
    EXP_FALSE,
    /* expression is true constant */
    EXP_TRUE,
    /* string constant;
     * 'str' = string value; */
    EXP_STRING,
    /* integer constant;
     * 'i' = integer value; */
    EXP_INT,
    /* floating constant;
     * 'n' = floating value; */
    EXP_FLT,
    /* registered constant value;
     * 'info' = index in 'constants'; */
    EXP_K,
    /* global variable;
     * 'str' = global name */
    EXP_GLOBAL,
    /* upvalue variable;
     * 'info' = index of upvalue in 'upvals'; */
    EXP_UVAL,
    /* local variable;
     * 'info' = stack index; */
    EXP_LOCAL,
    /* indexed variable; */
    EXP_INDEXED,
    /* variable indexed with literal string;
     * 'info' = index in 'constants'; */
    EXP_INDEXSTR,
    /* variable indexed with constant integer;
     * 'info' = index in 'constants'; */
    EXP_INDEXINT,
    /* indexed 'super'; */
    EXP_INDEXSUPER,
    /* indexed 'super' with literal string;
     * 'info' = index in 'constants'; */
    EXP_INDEXSUPERSTR,
    /* indexed variable with '.';
     * 'info' = index in 'constants'; */
    EXP_DOT,
    /* indexed 'super' with '.'; */
    EXP_DOTSUPER,
    /* function call;
     * 'info' = pc; */
    EXP_CALL,
    /* vararg expression '...';
     * 'info' = pc; */
    EXP_VARARG,
    /* expression is a jump test;
     * 'info' = pc; */
    EXP_JMP,
    /* finalized expression */
    EXP_FINEXPR,
} expt;


/*
 * Expression information.
 * Parser builds up the expression information and feeds it into
 * functions that generate bytecode.
 * Then those functions also fill the 'ExpInfo' accordingly.
 * So the codegen functions are essentially consumers of 'ExpInfo'.
 */
typedef struct ExpInfo {
    expt et;
    union {
        cs_Number n; /* floating constant */
        cs_Integer i; /* integer constant  */
        OString *str; /* string literal */
        int info; /* pc or some other generic information */
    } u;
    int t; /* jmp to patch if true */
    int f; /* jmp to patch if false */
} ExpInfo;



/* variable kind (stored in 'mod') */
#define VARREG          0 /* regular */
#define VARFINAL        1 /* final (immutable) */
#define VARTBC          2 /* to-be-closed */

/* bit mask of all valid modifiers in 'mod' */
#define maskvarkind     (bit2mask(VARFINAL, VARPRIVATE) | bitmask(VARTBC))


/* active local variable compiler information */
typedef union LVar {
    struct {
        TValueFields;
        cs_ubyte kind;
        int idx; /* index in 'locals' */
        OString *name;
    } s;
    TValue val; /* constant value */
} LVar;


typedef struct BreakJmp {
    int jmp;
    int hasclose;
} BreakJmp;


/* list of jump instructions to patch */
typedef struct PatchList {
    int len;
    int size;
    BreakJmp *arr;
} PatchList;


/* class declaration information */
typedef struct ClassState {
    struct ClassState *prev;
    cs_ubyte super; /* true if class has superclass */
} ClassState;



/*
** Dynamic data used by parser.
** It is stored inside 'Lexer' because each 'FunctionState' shares
** the same 'Lexer'.
*/
typedef struct ParserState {
    struct {
        int len;
        int size;
        LVar *arr;
    } actlocals; /* active local vars */
    struct ClassState *cs;
} ParserState;


/* dynamic data context (for optimizations) */
typedef struct DynCtx {
    int loopstart;
    int sp;
    int np;
    int nk;
    int pc;
    int nlinfo;
    int nlocals;
    int nupvals;
    int nbrks;
    int needclose;
    int lastwasret;
} DynCtx;


/* state for currently compiled function prototype */
typedef struct FunctionState {
    Proto *p; /* current function prototype */
    struct FunctionState *prev; /* implicit linked-list */
    struct Lexer *lx; /* lexer */
    struct Scope *scope; /* scope information */
    struct Scope *loopscope; /* innermost loop scope */
    struct Scope *switchscope; /* innermost switch scope */
    int loopstart; /* innermost loop start offset */
    int sp; /* first free compiler stack index */
    int nactlocals; /* number of active local variables */
    int firstlocal; /* index of first local in 'lvars' */
    int np; /* number of elements in 'p' */
    int nk; /* number of elements in 'k' */
    int pc; /* number of elements in 'code' (equialent to 'ncode') */
    int nlinfo; /* number of elements in 'linfo' */
    int nlocals; /* number of elements in 'locals' */
    int nupvals; /* number of elements in 'upvals' */
    DynCtx deadcode; /* context before "dead" (unreachable) code */
    struct {
        int len; /* number of elements in 'list' */
        int size; /* size of 'list' */
        PatchList *list; /* list of patch lists */
    } patches; /* 2Dvec */
    cs_ubyte needclose; /* true if needs to close upvalues before returning */
    cs_ubyte lastwasret; /* last statement is 'return' */
    cs_ubyte pclastop; /* last OpCode pc */
} FunctionState;


CSI_FUNC cs_noret csP_semerror(Lexer *lx, const char *err);
CSI_FUNC CSClosure *csP_parse(cs_State *ts, BuffReader *br, Buffer *buff,
                              ParserState *ps, const char *source);

#endif
