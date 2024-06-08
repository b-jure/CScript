#include "crcode.h"
#include "crlexer.h"
#include "crbits.h"



/* long op */
#define OPL(op)		op##L



/* 
 * Add line and pc information, skip adding 'LineInfo' if previous
 * entry contained the same line.
 */
static void addlineinfo(FunctionState *fs, Function *f, int line)
{
	int len;

	len = f->lineinfo.len;
	if (len <= 0 || f->lineinfo.ptr[len - 1].line < line) {
		cr_mm_growvec(fs->lexer->vm, &f->lineinfo);
		f->lineinfo.ptr[len].pc = f->code.len - 1;
		f->lineinfo.ptr[f->lineinfo.len++].line = line;
		fs->lexer->prevline = line; // Maybe (prevline = currline) ?
	}
}


/* write instruction */
int cr_ce_code(FunctionState *fs, Instruction i)
{
	Function *f = fs->fn;

	cr_mm_growvec(fs->lexer->vm, &f->code);
	f->code.ptr[f->code.len++] = i;
	addlineinfo(fs, f, fs->lexer->currline);
	return f->code.len - 1;
}


/* write short instruction parameter */
static int shortparam(FunctionState *fs, Function *f, Instruction i, cr_ubyte idx)
{
	cr_mm_growvec(fs->lexer->vm, &f->code);
	f->code.ptr[f->code.len++] = cast_ubyte(idx & 0xff);
	return f->code.len - 1;
}


/* write instruction 'i' and short parameter */
static int shortcode(FunctionState *fs, Instruction i, int idx)
{
	Function *f = fs->fn;
	int offset;

	offset = cr_ce_code(fs, i);
	shortparam(fs, f, i, idx);
	return offset;
}


/* write long instruction parameter */
static int longparam(FunctionState *fs, Function *f, Instruction i, int idx)
{
	cr_mm_ensurevec(fs->lexer->vm, &f->code, 3);
	f->code.ptr[f->code.len++] = i;
	setbytes(f->code.ptr, &idx, 3);
	f->code.len += 3;
	return f->code.len - 3;
}


/* write instruction 'i' and long parameter */
static int longcode(FunctionState *fs, Instruction i, int idx)
{
	Function *f = fs->fn;
	int offset;

	offset = cr_ce_code(fs, i);
	longparam(fs, f, i, idx);
	return offset;
}


/* write instruction and its parameter depending on the parameter size */
int cr_ce_codewparam(FunctionState *fs, Instruction i, int idx)
{
	Function *f = fs->fn;
	int offset;

	offset = cr_ce_code(fs, i);
	cr_assert(idx >= 0);
	if (idx <= CR_SHRTPARAM) shortparam(fs, f, i, cast_ubyte(idx));
	else longparam(fs, f, i, idx);
	return offset;
}


/* add constant value to the function */
static int addconstant(FunctionState *fs, TValue *constant)
{
	Function *f;

	f = fs->fn;
	if (ttiso(constant)) {
		cr_assert(ttisstr(constant));
		markgco(ovalue(constant));
	}
	cr_mm_growvec(fs->lexer->vm, &f->constants);
	f->constants.ptr[f->constants.len++] = *constant;
	if (ttiso(constant))
		unmarkgco(ovalue(constant));
	return f->constants.len - 1;
}


/* write OP_CONST instruction with float parameter */
void cr_ce_fltconstant(FunctionState *fs, cr_number n)
{
	TValue vn = newfvalue(n);
	longcode(fs, OP_CONST, addconstant(fs, &vn));
}


/* write OP_CONST instruction with integer parameter */
void cr_ce_intconstant(FunctionState *fs, cr_integer i)
{
	TValue vi = newivalue(i);
	longcode(fs, OP_CONST, addconstant(fs, &vi));
}


/* write OP_CONST instruction with string parameter */
void cr_ce_strconstant(FunctionState *fs, OString *str)
{
	TValue vs = newovalue(str);
	longcode(fs, OP_CONST, addconstant(fs, &vs));
}
