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


/* instruction and parameter sizes in bytes */
#define INSTSIZE	1
#define ARGSSIZE	INSTSIZE
#define ARGLSIZE	3


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
 * Instructions/operations (bytecode).
 * All instructions are size of 1 byte.
 * Some but not all instructions have single/multiple
 * parameters that vary in size.
 * There are two sizes that Cript uses for the instruction
 * parameters, 3 byte parameters and single byte size parameters.
 * Some instructions that have 3 byte size parameter also have
 * extra 'L' in order to differentiate them from other single byte
 * parameter instructions that do the same thing.
 */
typedef enum {
	OP_TRUE = 0,     /* push 'true' literal */
	OP_FALSE,        /* push 'false' literal */
	OP_NIL,          /* push 'nil' literal */
	OP_NILN,         /* push 'n' 'nil' literals; larg = quantity */
	OP_CONST,        /* push constant; larg = index into 'constants' array */
	OP_SETVARARG,    /* set function varargs; larg = function arity */
	OP_VARARG,       /* push all extra arguments */
	OP_CLOSURE,      /* create and push closure */
	OP_CLASS,        /* create and push class */
	OP_METHOD,       /* create and push method */
	OP_POP,          /* pop single value */
	OP_POPN,         /* pop 'n' values */

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

	OP_DEFGVAR,      /* define global variable */
	OP_DEFGVARL,     /* define global variable long */
	OP_GETGVAR,      /* get global variable */
	OP_GETGVARL,     /* get global variable long */
	OP_SETGVAR,      /* set global variable */
	OP_SETGVARL,     /* set global variable long */

	OP_GETLVAR,      /* get local variable */
	OP_GETLVARL,     /* get local variable long */
	OP_SETLVAR,      /* set local variable */
	OP_SETLVARL,     /* set local variable long */

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
	OP_GETPROPERTY,  /* get property ('v.str') */
	OP_GETINDEX,     /* get index ('v[k]') */
	OP_SETINDEX,     /* set index ('v[k]') */
	OP_GETINDEXK,    /* get index ('v[kk]') */
	OP_SETINDEXK,    /* set index ('v[kk]') */
	OP_GETSUP,       /* get super class method ('super.k') */
	OP_GETSUPIDX,    /* get super class method ('super[k or str]') */

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



CRI_FUNC int cr_code_code(FunctionState *fs, Instruction i);
CRI_FUNC void cr_code_unary(FunctionState *fs, ExpInfo *e, Unopr op);
CRI_FUNC void cr_code_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, 
                             Binopr op);
CRI_FUNC int cr_code_codesarg(FunctionState *fs, Instruction i, int arg);
CRI_FUNC int cr_code_codelarg(FunctionState *fs, Instruction i, int arg);
CRI_FUNC int cr_code_codearg(FunctionState *fs, Instruction i, int arg);
CRI_FUNC int cr_code_flt(FunctionState *fs, cr_number n);
CRI_FUNC int cr_code_int(FunctionState *fs, cr_integer i);
CRI_FUNC int cr_code_string(FunctionState *fs, OString *str);
CRI_FUNC void cr_code_reservestack(FunctionState *fs, int n);
CRI_FUNC void cr_code_checkstack(FunctionState *fs, int n);
CRI_FUNC void cr_code_setoneret(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_setreturns(FunctionState *fs, ExpInfo *e, int nreturns);
CRI_FUNC int cr_code_ret(FunctionState *fs, int base, int nreturns);
CRI_FUNC int cr_code_call(FunctionState *fs, int base, int args, int nreturns);
CRI_FUNC void cr_code_dischargevar(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_storevar(FunctionState *fs, ExpInfo *e);
CRI_FUNC void cr_code_rmlastins(FunctionState *fs, ExpInfo *e);


#endif
