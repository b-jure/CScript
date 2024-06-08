#ifndef CRCODE_H
#define CRCODE_H


#include "cript.h"
#include "crparser.h"



/* get instruction 'op' size */
#define opsize(i,op,e) \
	(((e)->ins.l = (i) > CR_SHRTCODE) ? (op##L) : (op))


/* get constant */
#define constant(f,e)		(&(f)->fn->constants[(e)->info])


/* get code current */
#define codeoffset(f)		((f)->fn->code.len)


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
	/* push literals */
	OP_TRUE = 0, OP_FALSE, OP_NIL, OP_NILN,
	/* perform unary operation */
	OP_NEG, OP_NOT,
	/* perform binary operation */
	OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_POW,
	/* push all varargs */
	OP_VARARG,
	/* perform comparison */
	OP_NEQ, OP_EQ, OP_EQUAL, OP_GT, OP_GE, OP_LT, OP_LE,
	/* pop value/s */
	OP_POP, OP_POPN,
	/* push constant */
	OP_CONST,
	/* global variable ops */
	OP_DEFINE_GLOBAL, OP_DEFINE_GLOBALL, OP_GET_GLOBAL, OP_GET_GLOBALL,
	OP_SET_GLOBAL, OP_SET_GLOBALL,
	/* local variable ops */
	OP_GET_LOCAL, OP_GET_LOCALL, OP_SET_LOCAL, OP_SET_LOCALL,
	/* code jump ops */
	OP_JMP_IF_FALSE, OP_JMP_IF_FALSE_POP, OP_JMP_IF_FALSE_OR_POP,
	OP_JMP_IF_FALSE_AND_POP, OP_JMP, OP_JMP_AND_POP, OP_LOOP,
	/* call '()' ops */
	OP_CALL0, OP_CALL1, OP_CALL,
	/* push closure */
	OP_CLOSURE,
	/* upvalue ops */
	OP_GET_UPVALUE, OP_SET_UPVALUE, OP_CLOSE_UPVAL, OP_CLOSE_UPVALN,
	/* push class */
	OP_CLASS,
	/* dot '.' property ops */
	OP_SET_PROPERTY, OP_GET_PROPERTY,
	/* index '[]' property ops */
	OP_INDEX, OP_SET_INDEX,
	/* push method */
	OP_METHOD,
	/* combined property access + call ops */
	OP_INVOKE0, OP_INVOKE1, OP_INVOKE,
	/* overload vtable method */
	OP_OVERLOAD,
	/* inherit from class */
	OP_INHERIT,
	/* 'super'(class) ops */
	OP_GET_SUPER, OP_INVOKE_SUPER0, OP_INVOKE_SUPER1, OP_INVOKE_SUPER,
	/* set start/end of params */
	OP_CALLSTART, OP_RETSTART,
	/* generic 'foreach' loop ops */
	OP_FOREACH, OP_FOREACH_PREP,
	/* return value/s ops */
	OP_RET0, OP_RET1, OP_RET,
} OpCode;


/* number of 'OpCode's */
#define CR_NOPC		(OP_RET + 1)



int cr_ce_code(FunctionState *fs, Instruction i);
int cr_ce_codewparam(FunctionState *fs, Instruction i, int idx);
void cr_ce_fltconstant(FunctionState *fs, cr_number n);
void cr_ce_intconstant(FunctionState *fs, cr_integer i);
void cr_ce_strconstant(FunctionState *fs, OString *str);


#endif
