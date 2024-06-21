#ifndef CRCODE_H
#define CRCODE_H


#include "cript.h"
#include "crparser.h"



/* get current pc */
#define codeoffset(fs)		((fs)->fn->code.len)



/* get constant of 'ExpInfo' */
#define getconstant(fs,e)	(&(fs)->fn->constants.ptr[(e)->u.idx])



/* get pointer to instruction of 'ExpInfo' */
#define getinstruction(fs,e)	(&(fs)->fn->code.ptr[(e)->u.info])


/* instruction and parameter sizes in bytes */
#define INSTRSIZE	1
#define SPARAMSIZE	INSTRSIZE
#define LPARAMSIZE	3


/* gets first parameter */
#define GETPARAM(ip)		((ip) + INSTRSIZE)


/* get/set short parameter */
#define GETSPARAM(ip,o)		(GETPARAM(ip) + ((o)*SPARAMSIZE))
#define GETSPARAMV(ip,o)	(*(GETSPARAM(ip,o)))
#define SETSPARAM(ip,v)		setbytes(GETSPARAM(ip,0), v, SPARAMSIZE);


/* get/set long parameter */
#define GETLPARAM(ip,o)		(GETPARAM(ip) + ((o)*LPARAMSIZE))
#define GETLPARAMV(ip,o)	get3bytes(GETLPARAM(ip, o))
#define SETLPARAM(ip,v)		setbytes(GETLPARAM(ip,0), v, LPARAMSIZE)




/* unary operators */
typedef enum {
	OPR_MINUS, OPR_BNOT, OPR_NOT,
	OPR_NOUNOPR,
} Unopr;


/* binary operators */
typedef enum {
	/* arithmetic operators */
	OPR_ADD, OPR_SUB, OPR_MUL,
	OPR_DIV, OPR_MOD, OPR_POW,
	/* bitwise operators */
	OPR_SHR, OPR_SHL, OPR_BAND,
	OPR_BOR, OPR_BXOR,
	/* comparison operators */
	OPR_NE, OPR_EQ, OPR_LT,
	OPR_LE, OPR_GT, OPR_GE,
	/* logical operators */
	OPR_AND, OPR_OR,
	OPR_NOBINOPR,
} Binopr;


/* binary operator 'op' is foldable */
#define boprisfoldable(op) ((op) <= OPR_POW)


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
	/* pop/push instructions */
	OP_TRUE = 0, /* push 'true' literal */
	OP_FALSE, /* push 'false' literal */
	OP_NIL, /* push 'nil' literal */
	OP_NILN, /* push 'n' 'nil' literals */
	OP_CONST, /* push constant */
	OP_VARARG, /* push all variable arguments */
	OP_CLOSURE, /* create and push closure */
	OP_CLASS, /* create and push class */
	OP_METHOD, /* create and push method */
	OP_POP, /* pop single value */
	OP_POPN, /* pop 'n' values */
	/* arithmetic instructions */
	OP_NEG, /* negate (arithmetic) */
	OP_NOT, /* negate (boolean) */
	OP_ADD, /* add */
	OP_SUB, /* subtract */
	OP_MUL, /* multiply */
	OP_DIV, /* divide */
	OP_MOD, /* modulo */
	OP_POW, /* raise value 'x' to power of 'y' */
	/* ordering instructions */
	OP_NEQ, /* !(equality) */
	OP_EQ, /* equality */
	OP_EQUAL, /* equality (preserve left operand) */
	OP_GT, /* greater than */
	OP_GE, /* greater or equal */
	OP_LT, /* less than */
	OP_LE, /* less or equal */
	/* global variable instructions */
	OP_DEFGVAR, /* define global variable */
	OP_DEFGVARL, /* define global variable long */
	OP_GETGVAR, /* get global variable */
	OP_GETGVARL, /* get global variable long */
	OP_SETGVAR, /* set global variable */
	OP_SETGVARL, /* set global variable long */
	/* local variable instructions */
	OP_GETLVAR, /* get local variable */
	OP_GETLVARL, /* get local variable long */
	OP_SETLVAR, /* set local variable */
	OP_SETLVARL, /* set local variable long */
	/* jump instructions */
	OP_JZ, /* jump if false */
	OP_JZPOP, /* jump if false and pop unconditionally */
	OP_JZORPOP, /* jump if false or pop */
	OP_JZANDPOP, /* jump if false and pop */
	OP_JMP, /* jump to specified location */
	OP_JMPANDPOP, /* jump to specified location and pop */
	OP_LOOP, /* jump back to specified location */
	/* call instructions */
	OP_CALL0, /* call value with no arguments */
	OP_CALL1, /* call value with a single argument */
	OP_CALL, /* call value with 2 or more arguments */
	/* upvalue instructions */
	OP_GETUVAL, /* get upvalue */
	OP_SETUVAL, /* set upvalue */
	OP_CLOSEUVAL, /* close upvalue */
	OP_CLOSEUVALN, /* close 'n' upvalues */
	/* property access instructions */
	OP_SETPROPERTY, /* set property ('v.str') */
	OP_GETPROPERTY, /* get property ('v.str') */
	OP_GETINDEX, /* get index ('v[k]') */
	OP_SETINDEX, /* set index ('v[k]') */
	OP_GETINDEXK, /* get index ('v[kk]') */
	OP_SETINDEXK, /* set index ('v[kk]') */
	OP_GETSUP, /* get super class method ('super.k') */
	OP_GETSUPIDX, /* get super class method ('super[k or str]') */
	/* other specific instructions */
	OP_SETVTABLE, /* set vtable method */
	OP_INHERIT, /* inherit from class */
	OP_CALLSTART, /* mark start of call values */
	OP_RETSTART, /* mark start of return values */
	OP_FOREACH_PREP, /* prepare foreach loop */
	OP_FOREACH, /* run foreach loop */
	/* return instructions */
	OP_RET0, /* return with no values */
	OP_RET1, /* return with a single value */
	OP_RET, /* return with 2 or more values */
} OpCode;


/* number of 'OpCode's */
#define CR_NUMOPS	(OP_RET + 1)



int cr_ce_code(FunctionState *fs, Instruction i);
int cr_ce_flt(FunctionState *fs, cr_number n);
int cr_ce_int(FunctionState *fs, cr_integer i);
int cr_ce_string(FunctionState *fs, OString *str);
void cr_ce_reservestack(FunctionState *fs, int n);
void cr_ce_checkstack(FunctionState *fs, int n);
void cr_ce_setoneret(FunctionState *fs, ExpInfo *e);
void cr_ce_dischargevar(FunctionState *fs, ExpInfo *e);
void cr_ce_storevar(FunctionState *fs, ExpInfo *e);


#endif
