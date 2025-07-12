/*
** cparser.h
** CScript Parser
** See Copyright Notice in cscript.h
*/

#ifndef cparser_h
#define cparser_h


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
#define eisvar(e)       ((e)->et >= EXP_UVAL && (e)->et <= EXP_DOTSUPER)
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
    /* upvalue variable;
     * 'info' = index of upvalue in 'upvals'; */
    EXP_UVAL,
    /* local variable;
     * 'v.sidx' = stack index;
     * 'v.vidx' = compiler index; */
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
        struct {
            int vidx; /* compiler index */
            int sidx; /* stack slot index */
        } var; /* local var */
        int info; /* pc or some other generic information */
    } u;
    int t; /* jmp to patch if true */
    int f; /* jmp to patch if false */
} ExpInfo;


#define onstack(e)      ((e)->et == EXP_FINEXPR)


/* variable kind */
#define VARREG      0   /* regular */
#define VARFINAL    1   /* final (immutable) */
#define VARTBC      2   /* to-be-closed */


/* active local variable compiler information */
typedef union LVar {
    struct {
        TValueFields;
        c_ubyte kind;
        int sidx; /* stack slot index holding the variable value */
        int pidx; /* index of variable in Proto's 'locals' array */
        OString *name;
    } s;
    TValue val; /* constant value */
} LVar;


/* switch statement constant description */
typedef struct LiteralInfo {
    Literal lit; /* constant */
    int tt; /* type tag */
} LiteralInfo;


/*
** Description of pending goto jumps (break/continue).
** CScript does not support explicit 'goto' statements and labels,
** instead this structure refers to the 'break' and 'continue' jumps.
*/
typedef struct Goto {
    int pc; /* position in the code */
    int nactlocals; /* number of active local variables in that position */
    c_ubyte close; /* true if goto jump escapes upvalues */
    c_ubyte bk; /* true if goto is break (otherwise continue in gen. loop) */
} Goto;


/* list of goto jumps */
typedef struct GotoList {
    int len; /* number of labels in use */
    int size; /* size of 'arr' */
    Goto *arr; /* array of pending goto jumps */
} GotoList;


/*
** Dynamic data used by parser.
*/
typedef struct ParserState {
    struct { /* list of all active local variables */
        int len; /* number of locals in use */
        int size; /* size of 'arr' */
        LVar *arr; /* array of compiler local variables */
    } actlocals;
    struct { /* list of all switch constants */
        int len; /* number of constants in use */
        int size; /* size of 'arr' */
        struct LiteralInfo *arr; /* array of switch constants */
    } literals;
    GotoList gt; /* idem */
} ParserState;


struct LoopState; /* defined in cparser.c */
struct ClassState; /* defined in cparser.c */
struct Scope; /* defined in cparser.c */


/* state for currently compiled function prototype */
typedef struct FunctionState {
    Proto *p;                   /* current function prototype */
    struct ClassState *cs;      /* chain, class definition */
    struct LoopState *ls;       /* chain, loop specific state */
    struct FunctionState *prev; /* chain, enclosing function */
    struct Lexer *lx;           /* lexical state */
    struct Scope *scope;        /* chain, current scope */
    struct Scope *loopscope;    /* chain, innermost loop scope */
    struct Scope *switchscope;  /* chain, innermost switch scope */
    int firstlocal;     /* index of first local in 'lvars' */
    int loopstart;      /* innermost loop start offset */
    int prevpc;         /* previous instruction pc */
    int prevline;       /* previous instruction line */
    int sp;             /* first free compiler stack index */
    int nactlocals;     /* number of active local variables */
    int np;             /* number of elements in 'p' */
    int nk;             /* number of elements in 'k' */
    int pc;             /* number of elements in 'code' (aka 'ncode') */
    int nabslineinfo;   /* number of elements in 'abslineinfo' */
    int ninstpc;        /* number of elements in 'instpc' */
    int nlocals;        /* number of elements in 'locals' */
    int nupvals;        /* number of elements in 'upvals' */
    int pcswtest;       /* 'pc' of the last test instruction in 'switchstm' */
    int lasttarget;     /* latest 'pc' that is jump target */
    c_ubyte iwthabs;    /* instructions issued since last abs. line info */
    c_ubyte needclose;  /* true if needs to close upvalues before returning */
    c_ubyte opbarrier;  /* true if op merge is prohibited 1=nil/2=pop/3=both */ 
    c_ubyte lastisend;  /* true if last statement ends control flow
                           (1==return, 2==break, 3==continue)*/
} FunctionState;


CSI_FUNC c_noret csP_semerror(Lexer *lx, const char *err);
CSI_FUNC void csP_checklimit(FunctionState *fs, int n,
                             int limit, const char *what);
CSI_FUNC CSClosure *csP_parse(cs_State *C, BuffReader *br, Buffer *buff,
                              ParserState *ps, const char *source);


#endif
