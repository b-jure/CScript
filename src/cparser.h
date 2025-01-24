/*
** cparser.h
** CScript Parser
** See Copyright Notice in cscript.h
*/

#ifndef CPARSER_H
#define CPARSER_H


#include "clexer.h"
#include "cobject.h"


/* maximum number of local variables per function */
#define MAXVARS         MAX_CODE


/*
** Because all strings are unified by the scanner, the parser
** can use pointer equality for string equality.
*/
#define eqstr(a, b)     ((a) == (b))


/* check expression type */
#define eisvar(e)       ((e)->et >= EXP_GLOBAL && (e)->et <= EXP_DOTSUPER)
#define eisconstant(e)  ((e)->et >= EXP_NIL && (e)->et <= EXP_K)
#define eismulret(e)    ((e)->et == EXP_CALL || (e)->et == EXP_VARARG)
#define eistrue(e)      ((e)->et >= EXP_TRUE && (e)->et <= EXP_K)
#define eisindexed(e)   ((e)->et >= EXP_INDEXED && (e)->et <= EXP_DOTSUPER)


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
    /* finalized expression */
    EXP_FINEXPR,
} expt;


/*
** Expression information.
** Parser builds up the expression information and feeds it into
** functions that generate bytecode (codegen).
** Then those functions also fill the 'ExpInfo' accordingly.
*/
typedef struct ExpInfo {
    expt et;
    union {
        cs_Number n;  /* floating constant */
        cs_Integer i; /* integer constant  */
        OString *str; /* string literal */
        int info;     /* pc or some other generic information */
    } u;
    int t; /* jmp to patch if true */
    int f; /* jmp to patch if false */
} ExpInfo;


/* variable kind */
#define VARREG      0   /* regular */
#define VARFINAL    1   /* final (immutable) */
#define VARTBC      2   /* to-be-closed */


/* active local variable compiler information */
typedef union LVar {
    struct {
        TValueFields;
        c_byte kind;
        int sidx; /* stack slot holding the value */
        int pidx; /* index of local variable in Proto's 'locals' array */
        OString *name;
    } s;
    TValue val; /* constant value */
} LVar;


typedef struct Jump {
    int jmp;
    int nactlocals;
    union { /* extra information */
        int hasclose;   /* true if has to close open upvalues */
        int iscontinue; /* true if this is continue jump (generic loop) */
    } e;
} Jump;


/* list of jump instructions to patch */
typedef struct PatchList {
    int len;
    int size;
    Jump *arr;
} PatchList;


/* class declaration information */
typedef struct ClassState {
    struct ClassState *prev; /* chain of nested declarations */
    c_byte super; /* true if class has superclass */
} ClassState;


/*
** Dynamic data used by parser.
** It is stored inside 'Lexer' because each 'FunctionState' shares
** the same 'Lexer'.
*/
typedef struct ParserState {
    struct { /* list of all active local variables */
        int len; int size;
        LVar *arr;
    } actlocals;
    struct { /* list of pending break jumps */
        int len; int size;
        PatchList *arr;
    } patches;
    struct ClassState *cs;
} ParserState;


/* 
** Function state context (for optimizations); snapshot of state fields.
*/
typedef struct FuncContext {
    int ninstpc;
    int loopstart;
    int prevpc;
    int prevline;
    int sp;
    int nactlocals;
    int np;
    int nk;
    int pc;
    int nabslineinfo;
    int nlocals;
    int nupvals;
    int npatches;
    int njumps;
    c_byte iwthabs;
    c_byte needclose;
    c_byte lastwasret;
} FuncContext;


/* state for currently compiled function prototype */
typedef struct FunctionState {
    Proto *p;                   /* current function prototype */
    struct FunctionState *prev; /* implicit linked-list */
    struct Lexer *lx;           /* lexer */
    struct Scope *scope;        /* scope information */
    struct Scope *loopscope;    /* innermost loop scope */
    struct Scope *switchscope;  /* innermost switch scope */
    int firstlocal;     /* index of first local in 'lvars' */
    int loopstart;      /* innermost loop start offset */
    int prevpc;         /* previous instruction pc */
    int prevline;       /* previous instruction line */
    int sp;             /* first free compiler stack index */
    int nactlocals;     /* number of active local variables */
    int np;             /* number of elements in 'p' */
    int nk;             /* number of elements in 'k' */
    int pc;             /* number of elements in 'code' (equialent to 'ncode') */
    int nabslineinfo;   /* number of elements in 'abslineinfo' */
    int ninstpc;        /* number of elements in 'instpc' */
    int nlocals;        /* number of elements in 'locals' */
    int nupvals;        /* number of elements in 'upvals' */
    c_byte iwthabs;     /* instructions issued since last absolute line info */
    c_byte needclose;   /* true if needs to close upvalues before returning */
    c_byte lastwasret;  /* last statement is 'return' */
    c_byte lastisend;   /* true if last statement ends control flow */
} FunctionState;


CSI_FUNC c_noret csP_semerror(Lexer *lx, const char *err);
CSI_FUNC CSClosure *csP_parse(cs_State *ts, BuffReader *br, Buffer *buff,
                              ParserState *ps, const char *source);


#endif
