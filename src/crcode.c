#include "crcode.h"
#include "crlexer.h"
#include "crbits.h"
#include "crlimits.h"
#include "crparser.h"
#include "crstate.h"



/* long op */
#define OPL(op)		op##L


/* check if 'ExpInfo' has jumps */
#define hasjumps(e)	((e)->t != (e)->f)



/*
 * Add line and pc information, skip adding 'LineInfo' if previous
 * entry contained the same line.
 */
static void addlineinfo(FunctionState *fs, Function *f, int line)
{
	int len = f->nlinfo;
	if (len <= 0 || f->linfo[len - 1].line < line) {
		cr_mem_growvec(fs->lx->ts, f->linfo, f->sizelinfo, f->nlinfo, INT_MAX, "lines");
		f->linfo[len].pc = f->ncode - 1;
		f->linfo[f->nlinfo++].line = line;
	}
}


/* write instruction */
int cr_code_code(FunctionState *fs, Instruction i)
{
	Function *f = fs->fn;
    cr_mem_growvec(fs->lx->ts, f->code, f->sizecode, f->ncode, INT_MAX, "code");
	f->code[f->ncode++] = i;
	addlineinfo(fs, f, fs->lx->line);
	return f->ncode - 1;
}


/* write short instruction parameter */
static int shortparam(FunctionState *fs, Function *f, Instruction i, cr_ubyte arg)
{
    cr_mem_growvec(fs->lx->ts, f->code, f->sizecode, f->ncode, INT_MAX, "code");
	f->code[f->ncode++] = cast_ubyte(arg & 0xff);
	return f->ncode - 1;
}


/* write instruction 'i' and short parameter */
int cr_code_codesarg(FunctionState *fs, Instruction i, int arg)
{
	Function *f = fs->fn;
	int offset = cr_code_code(fs, i);
	shortparam(fs, f, i, arg);
	return offset;
}


/* write long instruction parameter */
static int longparam(FunctionState *fs, Function *f, Instruction i, int idx)
{
    cr_mem_ensurevec(fs->lx->ts, f->code, f->sizecode, f->ncode, 3, INT_MAX, "code");
	f->code[f->ncode++] = i;
	setbytes(f->code, idx, 3);
	f->ncode += 3;
	return f->ncode - 3;
}


/* write instruction 'i' and long parameter */
int cr_code_codelarg(FunctionState *fs, Instruction i, int arg)
{
	Function *f = fs->fn;
	int offset = cr_code_code(fs, i);
	longparam(fs, f, i, arg);
	return offset;
}


/* write instruction and its parameter depending on the parameter size */
int cr_code_codearg(FunctionState *fs, Instruction i, int arg)
{
	Function *f = fs->fn;
	int offset = cr_code_code(fs, i);
	cr_assert(arg >= 0);
	if (arg <= CRI_SHRTPARAM)
		shortparam(fs, f, i, cast_ubyte(arg));
	else
		longparam(fs, f, i, arg);
	return offset;
}


/* add constant value to the function */
static int addconstant(FunctionState *fs, TValue *constant)
{
	Function *f = fs->fn;
    cr_mem_growvec(fs->lx->ts, f->constants, f->sizeconst, f->nconst, INT_MAX,
                   "constants");
	f->constants[f->nconst++] = *constant;
	return f->nconst - 1;
}


/* write OP_CONST instruction with float parameter */
int cr_code_flt(FunctionState *fs, cr_number n)
{
	TValue vn;
	setfval(&vn, n);
	int idx = addconstant(fs, &vn);
	cr_code_codelarg(fs, OP_CONST, idx);
	return idx;
}


/* write OP_CONST instruction with integer parameter */
int cr_code_int(FunctionState *fs, cr_integer i)
{
	TValue vi;
	setival(&vi, i);
	int idx = addconstant(fs, &vi);
	cr_code_codelarg(fs, OP_CONST, idx);
	return idx;
}


/* write OP_CONST instruction with string parameter */
int cr_code_string(FunctionState *fs, OString *str)
{
	TValue vs;
	setoval(&vs, obj2gco(str));
	int idx = addconstant(fs, &vs);
	cr_code_codelarg(fs, OP_CONST, idx);
	return idx;
}


/* free stack space */
cr_sinline void freestack(FunctionState *fs, int n)
{
	cr_assert(fs->sp - n >= 0);
	fs->sp -= n;
}


void cr_code_checkstack(FunctionState *fs, int n)
{
	int newstack = fs->sp + n;
	if (fs->fn->maxstack > newstack) {
		if (cr_unlikely(newstack >= CRI_LONGPARAM))
			cr_lex_syntaxerror(fs->lx,
				"function requires too much stack space");
		fs->fn->maxstack = newstack;
	}
}


void cr_code_reservestack(FunctionState *fs, int n)
{
	cr_code_checkstack(fs, n);
	fs->sp += n;
}


static int getvar(FunctionState *fs, OpCode op, ExpInfo *e)
{
	cr_assert(op == OP_GETLVAR || op == OP_GETGVAR);
	if (e->u.idx <= CRI_SHRTPARAM)
		return cr_code_codesarg(fs, op, e->u.idx);
	cr_assert(op + 1 == OP_GETLVARL || op + 1 == OP_GETGVARL);
	return cr_code_codelarg(fs, ++op, e->u.idx);
}


/* set single return, for call and vararg expressions */
void cr_code_setoneret(FunctionState *fs, ExpInfo *e)
{
	if (e->et == EXP_CALL) {
		/* already returns a single value */
		cr_assert(*getlongparam(getinstruction(fs, e), 0) == 1);
		e->et = EXP_FINEXPR;
		e->u.info = *getlongparam(getinstruction(fs, e), 0);
	} else if (e->et == EXP_VARARG) {
		setlongparam(getinstruction(fs, e), 1);
		e->et = EXP_FINEXPR;
	}
}


/* emit 'OP_SET' family of instructions */
void cr_code_storevar(FunctionState *fs, ExpInfo *e)
{
	switch (e->et) {
	case EXP_LOCAL:
		e->u.info = getvar(fs, OP_SETLVAR, e);
		e->et = EXP_FINEXPR;
		break;
	case EXP_UVAL:
		e->u.info = cr_code_codelarg(fs, OP_SETUVAL, e->u.info);
		e->et = EXP_FINEXPR;
		break;
	case EXP_GLOBAL:
		e->u.info = getvar(fs, OP_SETGVAR, e);
		e->et = EXP_FINEXPR;
		break;
	case EXP_INDEXK:
		e->u.info = cr_code_codelarg(fs, OP_SETINDEXK, e->u.idx);
		break;
	case EXP_INDEXRAW:
		e->u.info = cr_code_codelarg(fs, OP_SETPROPERTY, e->u.idx);
		e->et = EXP_FINEXPR;
		break;
	case EXP_INDEXED:
		freestack(fs, 1);
		e->u.info = cr_code_code(fs, OP_SETINDEX);
		e->et = EXP_FINEXPR;
		break;
	default:
		cr_unreachable();
		break;
	}
	freestack(fs, 1);
}


/* emit 'OP_GET' family of instructions */
void cr_code_dischargevar(FunctionState *fs, ExpInfo *e)
{
	switch (e->et) {
	case EXP_LOCAL:
		e->u.info = getvar(fs, OP_GETLVAR, e);
		e->et = EXP_FINEXPR;
		break;
	case EXP_UVAL:
		e->u.info = cr_code_codelarg(fs, OP_GETUVAL, e->u.info);
		e->et = EXP_FINEXPR;
		break;
	case EXP_GLOBAL:
		e->u.info = getvar(fs, OP_GETGVAR, e);
		e->et = EXP_FINEXPR;
		break;
	case EXP_INDEXK:
		freestack(fs, 1);
		e->u.info = cr_code_codelarg(fs, OP_GETINDEXK, e->u.idx);
		break;
	case EXP_INDEXRAW:
		freestack(fs, 1);
		e->u.info = cr_code_codelarg(fs, OP_GETPROPERTY, e->u.idx);
		e->et = EXP_FINEXPR;
		break;
	case EXP_INDEXRAWSUP:
		freestack(fs, 1);
		e->u.info = cr_code_codelarg(fs, OP_GETSUP, e->u.idx);
		e->et = EXP_FINEXPR;
		break;
	case EXP_INDEXSUP:
		freestack(fs, 1);
		e->u.info = cr_code_codelarg(fs, OP_GETSUPIDX, e->u.idx);
		e->et = EXP_FINEXPR;
		break;
	case EXP_INDEXED:
		freestack(fs, 2);
		e->u.info = cr_code_code(fs, OP_GETINDEX);
		e->et = EXP_FINEXPR;
		break;
	case EXP_CALL: case EXP_VARARG:
		cr_code_setoneret(fs, e);
		break;
	default:
		cr_unreachable();
		break;
	}
}
