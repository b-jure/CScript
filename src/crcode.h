#ifndef CRCODE_H
#define CRCODE_H


#include "cript.h"
#include "crparser.h"



/* get current pc */
#define codeoffset(fs)		((fs)->fn->ncode)


/* get constant of 'ExpInfo' */
#define getconstant(fs,e)	(&(fs)->fn->constants[(e)->u.idx])


/* get pointer to instruction of 'ExpInfo' */
#define getinstruction(fs,e)	(&(fs)->fn->code[(e)->u.info])


/* instruction and arg sizes (bytes) */
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


/* gets first parameter */
#define getarg(ip)		((ip) + INSTSIZE)


/* get/set short parameter */
#define getsarg(ip,o)		(getarg(ip) + ((o)*ARGSSIZE))
#define setsarg(ip,o,v)		setbytes(getsarg(ip,o), v, ARGSSIZE);
#define getsarg0(ip)            getsarg(ip,0)
#define setsarg0(ip,v)          setsarg(ip,0,v)
#define getsarg1(ip)            getsarg(ip,1)
#define setsarg1(ip,v)          setsarg(ip,1,v)
#define getsarg2(ip)            getsarg(ip,2)
#define setsarg2(ip,v)          setsarg(ip,2,v)


/* get/set long arg */
#define getlarg(ip,o)		(getarg(ip) + ((o)*ARGLSIZE))
#define setlarg(ip,o,v)		setbytes(getlarg(ip,o), v, ARGLSIZE)
#define getlarg0(ip)            getlarg(ip,0)
#define setlarg0(ip,v)          setlarg(ip,0,v)
#define getlarg1(ip)            getlarg(ip,1)
#define setlarg1(ip,v)          setlarg(ip,1,v)
#define getlarg2(ip)            getlarg(ip,2)
#define setlarg2(ip,v)          setlarg(ip,2,v)


/* value indicating there is no jmp label */
#define NOJMP       (-1)


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



/* 
 * OpCode legend:
 * S - short instruction arg (8-bit)
 * L - long instruction arg (24-bit)
 * K(i) - constant at index 'i'
 * I(v) - format that indicates v is immediate operand
 * '...' - short description on what the instruction does
 */
typedef enum {
OP_TRUE = 0,    /*      'load true constant' */
OP_FALSE,       /*      'load false constant' */
OP_NIL,         /*      'load nil constant' */
OP_NILN,        /* L    'load L nils' */
OP_CONST,       /* S    'load constant at index S' */
OP_CONSTL,      /* L    'load constant at index L' */
OP_CONSTI,      /* L S  'load integer L (S signedness) */
OP_CONSTF,      /* L S  'load integer L as float (S signedness) */
OP_SETVARARG,   /* L    'adjust function varargs (L function arity)' */
OP_VARARG,      /* L    'load L-1 varargs' */
OP_CLOSURE,     /* TODO */
OP_CLASS,       /* TODO */
OP_METHOD,      /* TODO */
OP_POP,         /*      'pop value off the stack' */
OP_POPN,        /* L    'pop L values off the stack' */

OP_MBIN,        /* V V1 S    'V S V1 (S is binop)' */
OP_MBINI,       /* V L S S1  'V mbinop I(L) (S is signedness, S1 is flip)' */
OP_MBINK,       /* V L S     'V mbinop K(L) (S is flip)' */

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

OP_EQL,          /* equality (preserve left operand) */

OP_RANGEINT,     /* integer range; TODO */

OP_DEFGVAR,      /* define global variable */
OP_GETGVAR,      /* get global variable */
OP_SETGVAR,      /* set global variable */

OP_GETLVAR,      /* get local variable */
OP_SETLVAR,      /* set local variable */

OP_JZ,           /* jump if false */
OP_JZPOP,        /* jump if false and pop unconditionally */
OP_JZORPOP,      /* jump if false or pop */
OP_JZANDPOP,     /* jump if false and pop */
OP_JMP,          /* jump to specified location */
OP_JMPORPOP,
OP_JMPANDPOP,    /* jump to specified location and pop */
OP_LOOP,         /* jump back to specified location */

OP_CALL0,        /* call value with no arguments */
OP_CALL1,        /* call value with a single argument */
OP_CALL,         /* call value with 2 or more arguments */

OP_GETUVAL,      /* get upvalue */
OP_SETUVAL,      /* set upvalue */
OP_CLOSEUVAL,    /* close upvalue */
OP_CLOSEUVALN,   /* close 'n' upvalues */

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
OP_INHERIT,      /* inherit from class */
OP_CALLSTART,    /* mark start of call values */
OP_RETSTART,     /* mark start of return values */
OP_FOREACH_PREP, /* prepare foreach loop */
OP_FOREACH,      /* run foreach loop */

OP_RET0,         /* return with no values */
OP_RET1,         /* return with a single value */
OP_RET,          /* return with 2 or more values */
} OpCode;


/* number of 'OpCode's */
#define CR_NUMOPS	(OP_RET + 1)


CRI_FUNC int cr_code_SA(FunctionState *fs, Instruction i, int a);
CRI_FUNC int cr_code_LA(FunctionState *fs, Instruction i, int a);
CRI_FUNC void cr_code_reserveslots(FunctionState *fs, int n);
CRI_FUNC void cr_code_checkstack(FunctionState *fs, int n);
CRI_FUNC void cr_code_setoneret(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_setreturns(FunctionState *fs, ExpInfo *e, int nreturns);
CRI_FUNC int cr_code_nil(FunctionState *fs, int n);
CRI_FUNC int cr_code_ret(FunctionState *fs, int base, int nreturns);
CRI_FUNC int cr_code_call(FunctionState *fs, int base, int args, int nreturns);
CRI_FUNC void cr_code_varexp2stack(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_exp2stack(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key,
                              int super);
CRI_FUNC void cr_code_getproperty(FunctionState *fs, ExpInfo *var,
                                  ExpInfo *keystr, int super);
CRI_FUNC int cr_code_call(FunctionState *fs, int base, int args, int nreturns);
CRI_FUNC void cr_code_unary(FunctionState *fs, ExpInfo *e, Unopr op);
CRI_FUNC void cr_code_prebinary(FunctionState *fs, ExpInfo *e, Binopr op);
CRI_FUNC void cr_code_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, 
                             Binopr op);

CRI_FUNC void cr_code_storevar(FunctionState *fs, ExpInfo *var, ExpInfo *exp);
CRI_FUNC void cr_code_rmlastins(FunctionState *fs, ExpInfo *e);

#endif
