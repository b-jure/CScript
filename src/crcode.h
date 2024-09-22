#ifndef CRCODE_H
#define CRCODE_H


#include "crparser.h"



/* get current pc */
#define currentPC(fs)		((fs)->pc)


/* get constant of 'ExpInfo' */
#define getconstant(fs,e)	(&(fs)->fn->k[(e)->u.info])


/* get pointer to instruction of 'ExpInfo' */
#define getinstruction(fs,e)	(&(fs)->fn->code[(e)->u.info])


/* instruction and argument sizes (bytes) */
#define SIZEINSTR	    1
#define SIZEARGS	    SIZEINSTR
#define SIZEARGL	    3

/* instruction and args width */
#define WIDTHINSTR          (SIZEINSTR << 3)
#define WIDTHARGS           (SIZEARGS << 3)
#define WIDTHARGL           (SIZEARGL << 3)

/* maximum instruction and arg sizes */
#define MAXSIZEINSTR        ((1 << WIDTHINSTR) - 1)
#define MAXSHRTARGSIZE      MAXSIZEINSTR
#define MAXLONGARGSIZE      ((1 << WIDTHARGL) - 1)
#define MAXCODESIZE         MAXLONGARGSIZE


/* gets first arg pc */
#define GETARG(ip)		((ip) + SIZEINSTR)

/* get short/long argument pc */
#define GETPC_S(ip,o)            (GETARG(ip) + ((o)*SIZEARGS))
#define GETPC_L(ip,o)            (GETARG(ip) + ((o)*SIZEARGL))


/* get/set short parameter */
#define GETARG_S(ip,o)		cast_ubyte(*GETPC_S(ip,o))
#define SETARG_S(ip,o,v)	setbyte(GETPC_S(ip,0), o, v);


/* get/set long arg */
#define GETARG_L(ip,o)		get3bytes(GETARG(ip) + ((o)*SIZEARGL))
#define SETARG_L(ip,o,v)	set3bytes(GETPC_L(ip,o), v)


/* size of instruction jump argument in bytes */
#define JMPARGSIZE      SIZEARGL

/* max code jump offset value */
#define MAXJMP          MAXLONGARGSIZE

/* value indicating there is no jump */
#define NOJMP           (-1)


/* binary operators */
typedef enum {
	/* arithmetic operators */
	OPR_ADD, OPR_SUB, OPR_MUL,
	OPR_DIV, OPR_MOD, OPR_POW,
	/* bitwise operators */
	OPR_SHL, OPR_SHR, OPR_BAND,
	OPR_BOR, OPR_BXOR,
        /* range operator */
        OPR_RANGE,
	/* comparison operators */
	OPR_NE, OPR_EQ, OPR_LT,
	OPR_LE, OPR_GT, OPR_GE,
	/* logical operators */
	OPR_AND, OPR_OR,
	OPR_NOBINOPR,
} Binopr;


/* binary operator 'op' is foldable */
#define boprisfoldable(op)      ((op) <= OPR_BXOR)


/* only for checking in asserts */
#define opriscommutative(opr) \
    (opr == OPR_ADD || opr == OPR_MUL || (opr >= OPR_BAND && opr <= OPR_BXOR))



/* unary operators */
typedef enum {
	OPR_NOT = OPR_NOBINOPR, OPR_UMIN, OPR_BNOT, OPR_NOUNOPR,
} Unopr;



typedef enum {
/* ------------------------------------------------------------------------
** S - short arg (8-bit)
** L - long arg (24-bit)
** V - stack value
** V{x} - stack value at index 'x'
** K{x} - constant at index 'x'
** I(x) - 'x' is immediate operand
** U{x} - upvalue at index 'x'
** OU{x} - open upvalue at index 'x'
** P{x} - private variable in 'fn->private[x]'
** G(x) - global variable with 'get 'x' from 'gs->globals' htable
** L{x} - local variable in 'fn->locals[x]'
**
** operation     args           description
** ------------------------------------------------------------------------ */
OP_TRUE = 0,    /*            'load true constant' */
OP_FALSE,       /*            'load false constant' */
OP_NIL,         /*            'load nil constant' */
OP_NILN,        /* L          'load L nils' */
OP_CONST,       /* S          'load K{S}' */
OP_CONSTL,      /* L          'load K{L}' */
OP_CONSTI,      /* L S        'load integer L (S signedness)' */
OP_CONSTF,      /* L S        'load integer L as float (S signedness)' */
OP_VARARGPREP,  /* L          'adjust function varargs (L function arity)' */
OP_VARARG,      /* L          'load L-1 varargs' */
OP_CLOSURE,     /* L          'load closure(Enclosing->fns[L])' */
OP_CLASS,       /*            'create and load new class' */
OP_METHOD,   /* L V1 V2    'define method V2 for class V1 under key K{L}' */
OP_SETMM,       /* S V1 V2    'V1->vmt[S] = V2' (see notes) */
OP_POP,         /*            'pop value off the stack' */
OP_POPN,        /* L          'pop L values off the stack' */

OP_MBIN,        /* V1 V2 S    'V1 S V2'  (S is binop) */

OP_ADDK,         /* V L S   'V + K{L}:number' */
OP_SUBK,         /* V L S   'V - K{L}:number' */
OP_MULK,         /* V L S   'V * K{L}:number' */
OP_DIVK,         /* V L S   'V / K{L}:number' */
OP_MODK,         /* V L S   'V % K{L}:number' */
OP_POWK,         /* V L S   'V ** K{L}:number' */
OP_BSHLK,        /* V L S   'V << K{L}:number' */
OP_BSHRK,        /* V L S   'V >> K{L}:number' */
OP_BANDK,        /* V L S   'V & K{L}:number' */
OP_BORK,         /* V L S   'V | K{L}:number' */
OP_BXORK,        /* V L S   'V ^ K{L}:number' */

OP_ADDI,         /* V L S   'V + ((S - 1) * I(L))' */
OP_SUBI,         /* V L S   'V - ((S - 1) * I(L))' */
OP_MULI,         /* V L S   'V * ((S - 1) * I(L))' */
OP_DIVI,         /* V L S   'V / ((S - 1) * I(L))' */
OP_MODI,         /* V L S   'V % ((S - 1) * I(L))' */
OP_POWI,         /* V L S   'V ** ((S - 1) * I(L))' */
OP_BSHLI,        /* V L S   'V << ((S - 1) * I(L))' */
OP_BSHRI,        /* V L S   'V >> ((S - 1) * I(L))' */
OP_BANDI,        /* V L S   'V & ((S - 1) * I(L))' */
OP_BORI,         /* V L S   'V | ((S - 1) * I(L))' */
OP_BXORI,        /* V L S   'V ^ ((S - 1) * I(L))' */

OP_ADD,          /* V1 V2   'V1 + V2' */
OP_SUB,          /* V1 V2   'V1 - V2' */
OP_MUL,          /* V1 V2   'V1 * V2' */
OP_DIV,          /* V1 V2   'V1 / V2' */
OP_MOD,          /* V1 V2   'V1 % V2' */
OP_POW,          /* V1 V2   'V1 ** V2' */
OP_BSHL,         /* V1 V2   'V1 << V2' */
OP_BSHR,         /* V1 V2   'V1 >> V2' */
OP_BAND,         /* V1 V2   'V1 & V2' */
OP_BOR,          /* V1 V2   'V1 | V2' */
OP_BXOR,         /* V1 V2   'V1 ^ V2' */

OP_RANGE,        /* V V1    'V..V1' */

OP_EQK,          /* V L S   '(V == K{L}) == S' */

OP_EQI,       /* V L S1 S2      '(V == I(L) * (S1 - 1)) == S2' */
OP_LTI,       /* V L S1         'V < (S1 - 1) * I(L)' */
OP_LEI,       /* V L S1         'V <= (S1 - 1) * I(L)' */
OP_GTI,       /* V L S1         'V > (S1 - 1) * I(L)' */
OP_GEI,       /* V L S1         'V >= (S1 - 1) * I(L)' */ 

OP_EQ,           /* V1 V2 S     '(V1 == V2) == S' */
OP_LT,           /* V1 V2       '(V1 < V2)' */
OP_LE,           /* V1 V2       '(V1 <= V2)' */

OP_EQPRESERVE,   /* V1 V2   'V1 == V2 (preserves V1 operand)' */

OP_NOT,          /* V       'not V' */
OP_UNM,          /* V       '-V' */
OP_BNOT,         /* V       '~V' */

OP_JMP,          /* L       'pc += L' */
OP_JMPS,         /* L       'pc -= L' */

OP_TEST,        /* V L S   'if (!cri_isfalse(V) == S) pc += L' */
OP_TESTORPOP,   /* V L S   'if (!cri_isfalse(V) == S) pc += L; else pop V;' */
OP_TESTANDPOP,  /* V L S   'if (!cri_isfalse(V) == S) { pc += L; pop V; }' */
OP_TESTPOP,     /* V L S   'if (!cri_isfalse(V) == S) { pc += L; } pop V;' */

OP_CALL,   /* L1 L2 L3  'V{L1},...,V{L1+L3-2} = V{L1}(V{L1+1},...,V{L1+L2-1})'
            (check info) */

OP_CLOSE,        /* L           'close all upvalues >= OU{L} */
OP_TBC,          /* L           'mark L{L} as to-be-closed' */

OP_GETLOCAL,     /* L           'L{L}' */
OP_SETLOCAL,     /* V L         'L{L} = V' */

OP_GETPRIVATE,   /* L           'P{L}' */
OP_SETPRIVATE,   /* V L         'P{L} = V' */

OP_GETUVAL,      /* L           'U{L}' */
OP_SETUVAL,      /* V L         'U{L} = V'*/

OP_DEFGLOBAL,    /* V L         'G(L) = V' (check notes) */
OP_GETGLOBAL,    /* L           'G(L)' */
OP_SETGLOBAL,    /* V L         'G(L) = V' */

OP_SETPROPERTY,  /* V1 V2 L     'V1.K{L} = V2' */
OP_GETPROPERTY,  /* V  L        'V.K{L}' */

OP_GETINDEX,     /* V1 V2       'V1[V2]' */
OP_SETINDEX,     /* V1 V2 V3    'V1[V2] = V3' */

OP_GETINDEXSTR,  /* V L         'V[K{L}:string]' */
OP_SETINDEXSTR,  /* V1 V2 L     'V1[K{L}:string] = V2' */

OP_GETINDEXINT,  /* V L         'V[I(L):integer]' */
OP_SETINDEXINT,  /* V1 V2 L     'V[I(L):integer] = V2' */

OP_GETSUP,       /* V L         'V:super.K{L}:string' */
OP_GETSUPIDX,    /* V1 V2       'V1:super[V2]' */
OP_GETSUPIDXSTR, /* V L         'V:super[K{L}:string]' */

OP_INHERIT,    /* V1 V2  'V2 inherits from superclass V1' */
OP_FORPREP,    /* L1 L2  'create upvalue V{L1+3}; pc += L2' */
OP_FORCALL,    /* L1 L2  'V{L1+4},...,V{L1+3+L2} = V{L1}(V{L1+1}, V{L1+2});' */
OP_FORLOOP,    /* L1 L2  'if V{L1+2} != nil { V{L1} = V{L1+2}; pc -= L2 }' */

OP_RET0,        /* L1 L2 L3 S  'return;' (L1 L2 L3 S are unused) */
OP_RET1,        /* L1 L2 L3 S  'return V{L1};' (L2 L3 S are unused) */
OP_RET,         /* L1 L2 L3 S  'return V{L1}, ... ,V{L1+L2-2}' (check notes) */
} OpCode;


/*
** Notes:
** [OP_SETMM]
** Sets virtual method table entry value at index S.
**
** [OP_EQI]
** 
**
** [OP_CALL]
** L1 is the base stack offset where the value being called is located.
** L2 is the number of arguments biased with +1. L3 is the number of
** expected returns also biased with +1. If (L2-1 == 0) then, L2 = top - L1 
** (the actual number of arguments on stack). If (L3-1 == 0), then top is set
** to last return_result+1.
** 
** [OP_DEFGLOBAL]
** This instruction reffers to global variable declaration but behaves
** exactly as a simple assignment. It does some additional work to
** ensure global variable is semantically defined.
**
** [OP_RET]
** L2 is biased with +1, in order to represent multiple returns when the
** number of results is only known during runtime. For example (L2 == 0)
** represents CR_MULRET, in this case we would return all values up to the
** top; additionally check 'crC_ret' and you will see that 'nreturns' is
** positive biased with +1 (in generic return case). In case L3 > 0 it
** indicates this function is vararg function and represents number of
** vararg parameters. S indicates if current function needs to close upvalues
** or tbc variables before returning.
*/


/* number of 'OpCode's */
#define NUM_OPCODES	(OP_RET + 1)


/* OpCode format */
enum OpFormat { 
    FormatI,
    FormatIS,
    FormatISS,
    FormatIL,
    FormatILS,
    FormatILSS,
    FormatILL,
    FormatILLL,
    FormatN,
};


/*
** bits 0-2: format ('OpFormat')
** bit 3: instruction is a test (TProp)
** bit 4: instruction is a jump (JProp)
** bit 5: instruction is metamethod call (MProp)
** bit 6-7: unused
*/
CRI_DEC(const cr_ubyte crC_opProp[NUM_OPCODES];)

#define getOpFormat(p)      (crC_opProp[p] & 0x07)
#define testTProp(p)        (crC_opProp[p] & (1 << 3))
#define testJProp(p)        (crC_opProp[p] & (1 << 4))
#define testMProp(p)        (crC_opProp[p] & (1 << 5))

#define opProp(mm,j,t,f)    (((mm) << 5) | ((j) << 4) | ((t) << 3) | (f))



/* Instruction format sizes in bytes (aka as bytecode) */
CRI_DEC(const cr_ubyte crC_opSize[FormatN];)

#define getOpSize(p)        crC_opSize[getOpFormat(p)]



/* OpCode names table */ 
CRI_DEC(const char *crC_opName[NUM_OPCODES];)

#define getOpName(p)        crC_opName[p]



/* number of symbols in 'crC_opBinsym' */
#define NUM_BINSYM      (OP_BXOR - OP_ADD + 1)

/* OpCode binary op symbols */
CRI_DEC(const char *crC_opBinsym[NUM_BINSYM];)

#define getOpBinsym(p)       crC_opBinsym[(p) - OP_ADD]



CRI_FUNC int crC_emitI(FunctionState *fs, Instruction i);
CRI_FUNC int crC_emitIS(FunctionState *fs, Instruction i, int a);
CRI_FUNC int crC_emitIL(FunctionState *fs, Instruction i, int a);
CRI_FUNC int crC_emitILL(FunctionState *fs, Instruction i, int a, int b);
CRI_FUNC void crC_checkstack(FunctionState *fs, int n);
CRI_FUNC void crC_reserveslots(FunctionState *fs, int n);
CRI_FUNC void crC_setoneret(FunctionState *fs, ExpInfo *e);
CRI_FUNC void crC_setreturns(FunctionState *fs, ExpInfo *e, int nreturns);
CRI_FUNC int crC_nil(FunctionState *fs, int n);
CRI_FUNC int crC_pop(FunctionState *fs, int n);
CRI_FUNC int crC_ret(FunctionState *fs, int base, int nreturns);
CRI_FUNC int crC_call(FunctionState *fs, int base, int nparams); 
CRI_FUNC void crC_method(FunctionState *fs, ExpInfo *e);
CRI_FUNC void crC_storevar(FunctionState *fs, ExpInfo *var);
CRI_FUNC void crC_defineglobal(FunctionState *fs, ExpInfo *e);
CRI_FUNC void crC_varexp2stack(FunctionState *fs, ExpInfo *e);
CRI_FUNC void crC_exp2stack(FunctionState *fs, ExpInfo *e);
CRI_FUNC void crC_getproperty(FunctionState *fs, ExpInfo *var,
                              ExpInfo *keystr, int super);
CRI_FUNC void crC_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key,
                          int super);
CRI_FUNC void crC_unary(FunctionState *fs, ExpInfo *e, Unopr opr);
CRI_FUNC int crC_jmp(FunctionState *fs, OpCode jop);
CRI_FUNC void crC_concatjmp(FunctionState *fs, int *l1, int l2);
CRI_FUNC void crC_patch(FunctionState *fs, int pc, int target);
CRI_FUNC void crC_patchtohere(FunctionState *fs, int pc);
CRI_FUNC int crC_test(FunctionState *fs, OpCode testop, int cond);
CRI_FUNC void crC_prebinary(FunctionState *fs, ExpInfo *e, Binopr op);
CRI_FUNC void crC_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                         Binopr opr);

#endif
