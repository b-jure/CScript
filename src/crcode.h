#ifndef CRCODE_H
#define CRCODE_H


#include "cript.h"
#include "crparser.h"



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
	OP_CALLPROP0, /* call property with no arguments */
	OP_CALLPROP1, /* call property with a single argument */
	OP_CALLPROP, /* call property with 2 or more arguments */
	OP_CALLSUPER0, /* call superclass method with no arguments */
	OP_CALLSUPER1, /* call superclass method with a single argument */
	OP_CALLSUPER, /* call superclass method with 2 or more arguments */
	/* upvalue instructions */
	OP_GETUVAL, /* get upvalue */
	OP_SETUVAL, /* set upvalue */
	OP_CLOSEUVAL, /* close upvalue */
	OP_CLOSEUVALN, /* close 'n' upvalues */
	/* property access instructions */
	OP_SETPROP, /* set property ('.') */
	OP_GETPROP, /* get property ('.') */
	OP_GETPROPIDX, /* get property ('[k]') */
	OP_SETPROPIDX, /* set property ('[k]') */
	OP_GETSUP, /* get super class method ('.') */
	OP_GETSUPIDX, /* get super class method ('[k]') */
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



/* get instruction 'op' size */
#define opsize(i,op,e) \
	(((e)->ins.l = (i) > CR_SHRTCODE) ? (op##L) : (op))


/* get constant */
#define constant(f,e)		(&(f)->fn->constants[(e)->info])


/* get code current */
#define codeoffset(f)		((f)->fn->code.len)



int cr_ce_code(FunctionState *fs, Instruction i);
int cr_ce_codewparam(FunctionState *fs, Instruction i, int idx);
void cr_ce_fltconstant(FunctionState *fs, cr_number n);
void cr_ce_intconstant(FunctionState *fs, cr_integer i);
void cr_ce_strconstant(FunctionState *fs, OString *str);


#endif
