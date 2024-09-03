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
#define INSTSIZE	    1
#define ARGSSIZE	    INSTSIZE
#define ARGLSIZE	    3

/* instruction and arg sizes (bits) */
#define INSTBSIZE           (INSTSIZE << 3)
#define ARGSBSIZE           (ARGSSIZE << 3)
#define ARGLBSIZE           (ARGLSIZE << 3)

/* maximum instruction and arg sizes */
#define MAXINSTSIZE         ((1 << INSTBSIZE) - 1)
#define MAXSHRTARGSIZE      ((1 << ARGSBSIZE) - 1)
#define MAXLONGARGSIZE      ((1 << ARGLBSIZE) - 1)
#define MAXCODESIZE         MAXLONGARGSIZE


/* gets first arg pc */
#define GETARG(ip)		((ip) + INSTSIZE)

/* get short/long argument pc */
#define GETPC_S(ip,o)            (GETARG(ip) + ((o)*ARGSSIZE))
#define GETPC_L(ip,o)            (GETARG(ip) + ((o)*ARGLSIZE))


/* get/set short parameter */
#define GETARG_S(ip,o)		cast_ubyte(*GETPC_S(ip,o))
#define SETARG_S(ip,o,v)	setbyte(GETPC_S(ip,0), o, v);


/* get/set long arg */
#define GETARG_L(ip,o)		get3bytes(GETARG(ip) + ((o)*ARGLSIZE))
#define SETARG_L(ip,o,v)	set3bytes(GETPC_L(ip,o), v)


/* value indicating there is no jump */
#define NOJMP       (-1)

/* max code jump offset */
#define MAXJMP      MAXLONGARGSIZE


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



/* unary operators */
typedef enum {
	OPR_NOT = OPR_NOBINOPR, OPR_UMIN, OPR_BNOT, OPR_NOUNOPR,
} Unopr;



typedef enum {
/* -------------------------------------------------------------------------
 * S - short arg (8-bit)
 * L - long arg (24-bit)
 * V - stack value
 * K(x) - constant at index 'x'
 *
 * operation     args           description
 * ------------------------------------------------------------------------- */
OP_TRUE = 0,    /*             'load true constant' */
OP_FALSE,       /*             'load false constant' */
OP_NIL,         /*             'load nil constant' */
OP_NILN,        /* L           'load L nils' */
OP_CONST,       /* S           'load K(S)' */
OP_CONSTL,      /* L           'load K(L)' */
OP_CONSTI,      /* L S         'load integer L (S signedness) */
OP_CONSTF,      /* L S         'load integer L as float (S signedness) */
OP_SETVARARG,   /* L           'adjust function varargs (L function arity)' */
OP_VARARG,      /* L           'load L-1 varargs' */
OP_CLOSURE,     /*             'create and load new closure */
OP_CLASS,       /*             'create and load new class */
OP_METHOD,      /* L V1 V2     'define method V2 for class V1 under key K(L)' */
OP_SETVMT,      /* S V1 V2     'define Mmethod V2 for obj V1 at index S' */
OP_POP,         /*             'pop value off the stack' */
OP_POPN,        /* L           'pop L values off the stack' */

OP_MBIN,        /* V1 V2 S    'V1 S V2 (S is binop)' */
OP_MBINI,       /* V L S1 S2  'V mbinop I(L) (S is signedness, S1 is flip)' */
OP_MBINK,       /* V L S      'V mbinop K(L) (S is flip)' */

OP_ADDK,         /* V L S     'V + (S * K(L))' */
OP_SUBK,         /* V L S     'V - (S * K(L))' */
OP_MULK,         /* V L S     'V * (S * K(L))' */
OP_DIVK,         /* V L S     'V / (S * K(L))' */
OP_MODK,         /* V L S     'V % (S * K(L))' */
OP_POWK,         /* V L S     'V ** (S * K(L))' */
OP_BSHLK,        /* V L S     'V << (S * K(L))' */
OP_BSHRK,        /* V L S     'V >> (S * K(L))' */
OP_BANDK,        /* V L S     'V & (S * K(L))' */
OP_BORK,         /* V L S     'V | (S * K(L))' */
OP_BXORK,        /* V L S     'V ^ (S * K(L))' */

OP_ADDI,         /* V L S     'V + (S * I(L))' */
OP_SUBI,         /* V L S     'V - (S * I(L))' */
OP_MULI,         /* V L S     'V * (S * I(L))' */
OP_DIVI,         /* V L S     'V / (S * I(L))' */
OP_MODI,         /* V L S     'V % (S * I(L))' */
OP_POWI,         /* V L S     'V ** (S * I(L))' */
OP_BSHLI,        /* V L S     'V << (S * I(L))' */
OP_BSHRI,        /* V L S     'V >> (S * I(L))' */
OP_BANDI,        /* V L S     'V & (S * I(L))' */
OP_BORI,         /* V L S     'V | (S * I(L))' */
OP_BXORI,        /* V L S     'V ^ (S * I(L))' */

OP_IRANGEI,     /* V L     'V..I(L); V is also integer' */

OP_ADD,          /* V V1    'V + V1' */
OP_SUB,          /* V V1    'V - V1' */
OP_MUL,          /* V V1    'V * V1' */
OP_DIV,          /* V V1    'V / V1' */
OP_MOD,          /* V V1    'V % V1' */
OP_POW,          /* V V1    'V ** V1' */
OP_BSHL,         /* V V1    'V << V1' */
OP_BSHR,         /* V V1    'V >> V1' */
OP_BAND,         /* V V1    'V & V1' */
OP_BOR,          /* V V1    'V | V1' */
OP_BXOR,         /* V V1    'V ^ V1' */

OP_RANGE,        /* V V1    'V..V1' */

OP_EQK,          /* V L S   '(V == K(L)) == S' */

OP_EQI,          /* V L S S1   '(V == I(L)) == S1 (S true if I(L) is float)' */
OP_LTI,          
OP_LEI,
OP_GTI,
OP_GEI,

OP_EQ,           /* V V1 S   '(V == V1) == S' */
OP_LT,
OP_LE,

OP_NOT,          /* V       '!V' */
OP_UNM,          /* V       '-V' */
OP_BNOT,         /* V       '~V' */

OP_EQPRESERVE,   /* equality (preserve left operand) */

OP_RANGEINT,     /* integer range; TODO */

OP_JMP,          /* L        'pc += L' */
OP_JMPS,         /* L        'pc -= L' */
OP_JF,           /* V L      'if (cr_isfalse(V)) pc += L' */
OP_JFANDPOP,     /* V L      'if (cr_isfalse(V)) { pc += L; pop V }' */
OP_JFORPOP,      /* V L      'if (cr_isfalse(V)) pc += L; else pop V' */
OP_JFPOP,        /* V L      'if (cr_isfalse(V)) pc += L; pop V' */
OP_JT,           /* V L      'if (!cr_isfalse(V)) pc += L' */
OP_JTORPOP,      /* V L      'if (!cr_isfalse(V)) pc += L; else pop V' */
OP_JTPOP,        /* V L      'if (!cr_isfalse(V)) pc += L; pop V' */

OP_CALL0,        /* call value with no arguments */
OP_CALL1,        /* call value with a single argument */
OP_CALL,         /* call value with 2 or more arguments */

OP_GETUVAL,      /* get upvalue */
OP_SETUVAL,      /* set upvalue */

OP_CLOSE,        /* L        'close all upvalues >= L (stack ptr) */
OP_TBC,          /* V        'create new to-be-closed upvalue from variable V */

OP_DEFGLOBAL,    /* V1 V2    'define global variable V1 with value V2' */
OP_GETGLOBAL,    /* V        'get global variable of name V' */
OP_SETGLOBAL,    /* V1 V2    'set global variable V1 with value V2' */

OP_GETLOCAL,     /* get local variable */
OP_SETLOCAL,     /* set local variable */

OP_GETPRIVATE,   /* get private variable */
OP_SETPRIVATE,   /* set private variable */

OP_SETPROPERTY,  /* set property ('v.str') */
OP_GETPROPERTY,  /* get property ('v.str = ..') */

OP_GETINDEX,     /* get index ('v[k]') */
OP_SETINDEX,     /* set index ('v[k] = ..') */

OP_GETINDEXSTR,  /* get string index ('v[str]') */
OP_SETINDEXSTR,  /* set string index ('v[str] = ..') */

OP_GETINDEXINT,  /* get integer index ('v[int]') */
OP_SETINDEXINT,  /* set integer index ('v[int] = ..') */

OP_GETSUP,       /* get super class method ('super.k') */
OP_GETSUPIDX,    /* get super class method ('super[k | str]') */
OP_GETSUPIDXSTR, /* get super class method ('super[k | str]') */

OP_SETVTABLE,    /* set vtable method */
OP_INHERIT,      /* V1 V2         'V2 inherits from superclass V1' */
OP_CALLSTART,    /* mark start of call values */
OP_RETSTART,     /* mark start of return values */
OP_FORPREP,      /* prepare foreach loop */
OP_FORCALL,      /* V1 V2 V3    'call V1 with V2 and V3 as args' */
OP_FORLOOP,      /* run foreach loop */

OP_RET0,         /* return with no values */
OP_RET1,         /* return with a single value */
OP_RET,          /* L */
} OpCode;


/* number of 'OpCode's */
#define CR_NUMOPS	(OP_RET + 1)


CRI_FUNC int cr_code(FunctionState *fs, Instruction i);
CRI_FUNC int cr_code_S(FunctionState *fs, Instruction i, int a);
CRI_FUNC int cr_code_L(FunctionState *fs, Instruction i, int a);
CRI_FUNC int cr_code_LL(FunctionState *fs, Instruction i, int a, int b);
CRI_FUNC void cr_code_checkstack(FunctionState *fs, int n);
CRI_FUNC void cr_code_reserveslots(FunctionState *fs, int n);
CRI_FUNC void cr_code_setoneret(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_setreturns(FunctionState *fs, ExpInfo *e, int nreturns);
CRI_FUNC int cr_code_nil(FunctionState *fs, int n);
CRI_FUNC int cr_code_pop(FunctionState *fs, int n);
CRI_FUNC int cr_code_ret(FunctionState *fs, int base, int nreturns);
CRI_FUNC int cr_code_call(FunctionState *fs, int base, int nparams, 
                          int nreturns);
CRI_FUNC void cr_code_method(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_storevar(FunctionState *fs, ExpInfo *var);
CRI_FUNC void cr_code_defineglobal(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_varexp2stack(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_exp2stack(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_getproperty(FunctionState *fs, ExpInfo *var,
                                  ExpInfo *keystr, int super);
CRI_FUNC void cr_code_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key,
                              int super);
CRI_FUNC void cr_code_unary(FunctionState *fs, ExpInfo *e, Unopr opr);
CRI_FUNC int cr_code_jmp(FunctionState *fs, OpCode jop);
CRI_FUNC int cr_code_jmpf(FunctionState *fs, OpCode jfop);
CRI_FUNC void cr_code_concatjmp(FunctionState *fs, int *l1, int l2);
CRI_FUNC void cr_code_patch(FunctionState *fs, int pc, int target);
CRI_FUNC void cr_code_patchtohere(FunctionState *fs, int pc);
CRI_FUNC void cr_code_jmpiffalse(FunctionState *fs, ExpInfo *e, OpCode jmpop);
CRI_FUNC void cr_code_jmpiftrue(FunctionState *fs, ExpInfo *e, OpCode jmpop);
CRI_FUNC void cr_code_prebinary(FunctionState *fs, ExpInfo *e, Binopr op);
CRI_FUNC void cr_code_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                             Binopr opr);

#endif
