#ifndef CRCODE_H
#define CRCODE_H

/* get instruction with correct length */
#define getoptype(i, op, e) \
	((i) <= VM_SHORTINS_MAX ? ((e)->ins.l = 0, op) : ((e)->ins.l = 1, op##L))


/* get chunk */
#define getchunk(f)	(&(f)->fn->chunk)

/* get expression constant value */
#define getconstant(f, e)	ValueVec_at(&getchunk(f)->constants, (e)->u.info)

/* get current code length */
#define codeoffset(f)	(getchunk(f)->code.len)

#define constant(f, e)	ValueVec_at(&getchunk(f)->constants, (e)->info)



#define OP_N	(OP_RET + 1)

/*
 * Instructions/operations (bytecode).
 *
 * All op codes are size of 1 byte.
 * Some operations are 'long' operations and they are
 * indicated by extra 'L' at the end of their name.
 *
 * Long instructions have total size of up to 4 bytes
 * (this is including their arguments).
 * 1 byte instruction + 3 bytes argument = long instruction
 *
 * Short instructions have total size of up to 2 bytes
 * (this is including their arguments).
 * 1 byte instruction + 1 byte argument = short instruction
 */
typedef enum {
	/* push true/false */
	OP_TRUE = 0, OP_FALSE,
	/* push nil */
	OP_NIL, OP_NILN,
	/* unary ops */
	OP_NEG, OP_NOT,
	/* binary ops */
	OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_POW,
	/* push varargs */
	OP_VARARG,
	/* compare */
	OP_NEQ, OP_EQ, OP_EQUAL, OP_GT, OP_GE, OP_LT, OP_LE,
	/* pop */
	OP_POP, OP_POPN,
	/* load constant */
	OP_CONST,
	/* global var ops */
	OP_DEFINE_GLOBAL, OP_DEFINE_GLOBALL, OP_GET_GLOBAL, OP_GET_GLOBALL,
	OP_SET_GLOBAL, OP_SET_GLOBALL,
	/* local var ops */
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
	/* dot '.' ops */
	OP_SET_PROPERTY, OP_GET_PROPERTY,
	/* index '[]' ops */
	OP_INDEX, OP_SET_INDEX,
	/* push method */
	OP_METHOD,
	/* optimized call ops */
	OP_INVOKE0, OP_INVOKE1, OP_INVOKE,
	/* overload vtable */
	OP_OVERLOAD,
	/* inherit from class */
	OP_INHERIT,
	/* 'super'(class) ops */
	OP_GET_SUPER, OP_INVOKE_SUPER0, OP_INVOKE_SUPER1, OP_INVOKE_SUPER,
	/* indicate start/end of args */
	OP_CALLSTART, OP_RETSTART,
	/* generic for-loop ops */
	OP_FOREACH, OP_FOREACH_PREP,
	/* return ops */
	OP_RET0, OP_RET1, OP_RET,
} OpCode;

void initchunk(Chunk *chunk, VM *vm);
int writechunk(Chunk *chunk, cr_ubyte byte, int line);
int writechunk_codewparam(Chunk *chunk, OpCode code, int idx, int line);
int writechunk_constant(VM *vm, Chunk *chunk, Value value);
void freechunk(Chunk *chunk);


#endif
