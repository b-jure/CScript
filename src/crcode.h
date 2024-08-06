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
 * sarg - short arg (8-bit)
 * larg - long arg (24-bit)
 * C - 'constants' array;
 * U - 'upvals' array;
 * S - stack;
 */
typedef enum {
	OP_TRUE = 0,
	OP_FALSE,
	OP_NIL,
	OP_NILN,         /* larg = n */
	OP_CONST,        /* sarg = 8-bit C index */
        OP_CONSTINT,     /* sarg = 8-bit integer */
        OP_CONSTFLT,     /* sarg = 8-bit float */
	OP_CONSTL,       /* larg = 24-bit C index */
        OP_CONSTINTL,    /* larg = 24-bit integer */
        OP_CONSTFLTL,    /* larg = 24-bit float */
	OP_SETVARARG,    /* adjust function varargs; larg = function arity */
	OP_VARARG,       /* push all function vararg arguments on S */
	OP_CLOSURE,      /* create and push closure on S */
	OP_CLASS,        /* create and push class on S */
	OP_METHOD,       /* create and push method on S */
	OP_POP,          /* pop value off S; no args */
	OP_POPN,         /* pop 'n' values off S; larg = n */

	OP_ADD,          /* add */
	OP_SUB,          /* subtract */
	OP_MUL,          /* multiply */
	OP_DIV,          /* divide */
	OP_MOD,          /* modulo */
	OP_POW,          /* raise value 'x' to power of 'y' */
        OP_BSHL,         /* arithmetic left shift */
        OP_BSHR,         /* arithmetic right shift */
        OP_BAND,         /* binary and */
        OP_BOR,          /* binary or */
        OP_BXOR,         /* binary xor */
        OP_RANGE,        /* binary range operator */
	OP_NOT,          /* negate (boolean) */
	OP_UNM,          /* unary minus, negate (arithmetic) */
        OP_BNOT,         /* bitwise complement */

	OP_NEQ,          /* !(equality) */
	OP_EQ,           /* equality */
	OP_EQUAL,        /* equality (preserve left operand) */
	OP_GT,           /* greater than */
	OP_GE,           /* greater or equal */
	OP_LT,           /* less than */
	OP_LE,           /* less or equal */

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
CRI_FUNC int cr_code_dischargevars(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_dischargetostack(FunctionState *fs, ExpInfo *e);
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
