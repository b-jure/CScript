/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crcode.h"
#include "crconf.h"
#include "crlexer.h"
#include "crlimits.h"
#include "crobject.h"
#include "crparser.h"
#include "crvalue.h"
#include "crvm.h"


#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>



#define entercstack(lx)		(cr_vm_inccstack((lx)->vm))

#define leavecstack(lx)		((lx)->vm->nccalls--)


/* check condition */
#define expect_cond(lx, cond, err) \
	{ if (!(cond)) cr_lr_syntaxerror(lx, err); }


/* Scope 'bits' */
#define SCloop		(1 << 0)
#define SCforeach	(1 << 1)
#define SCswitch	(1 << 2)

/* lexical scope information */
typedef struct Scope {
	struct Scope *prev; /* chain */
	int depth; /* scope depth (index) */
	int nlocals; /* number of locals outside of this scope */
	cr_ubyte upval; /* set if scope contains upvalue variable */
	cr_ubyte bits;
} Scope;



/* class declaration information */
typedef struct ClassState {
	struct ClassState *enclosing;
	cr_ubyte superclass;
} ClassState;



/* forward declare (recursive) */
static void expr(FunctionState *fs, ExpInfo *E);



static void initexp(ExpInfo *e, expt et, int info)
{
	e->t = e->f = -1;
	e->et = et;
	e->u.info = info;
}


static void initvar(FunctionState *fs, ExpInfo *e, int idx)
{
	e->t = e->f = -1;
	e->et = EXP_LOCAL;
	e->u.info = idx;
}


/*
 * Returns 1 if 'S' is in loop, 0 if inside of a switch
 * and -1 if outside.
 */
static cr_inline int inwhat(Scope *s, Scope **target)
{
	Scope *head;
	int which;

	which = -1;
	head = s;
	for (; head != NULL; head = head->prev) {
		if (head->isswitch) {
			which = 0;
			break;
		}
		if (head->isloop) {
			which = 1;
			break;
		}
	}
	*target = head;
	return which;
}

#define inswitch(S) (inwhat(S) == 0)
#define inloop(S)   (inwhat(S) == 1)




static cr_inline Scope *getscope(FunctionState *fs, int depth)
{
	Scope *scope = fs->S;
	while (scope != NULL) {
		if (scope->depth == depth)
			return scope;
		scope = scope->prev;
	}
	return NULL;
}


typedef struct {
	int codeoffset;
	int constlen;
	int localc;
	int upvalc;
} Context;

static cr_inline void savecontext(FunctionState *fs, Context *C)
{
	C->codeoffset = codeoffset(fs);
	C->constlen = CHUNK(fs)->constants.len;
	C->localc = fs->locals.len;
	C->upvalc = fs->upvalues->len;
}

// Trim/set length of code and/or constant array
static cr_inline void concatcode(FunctionState *fs, int codeoffset, int constoffset)
{
	CHUNK(fs)->bcode.len = codeoffset;
	CHUNK(fs)->constants.len = constoffset;
}

static cr_inline void restorecontext(FunctionState *fs, Context *C)
{
	concatcode(fs, C->codeoffset, C->constlen);
	fs->locals.len = C->localc;
	fs->upvalues->len = C->upvalc;
}




// Forward declare
static void dec(FunctionState *fs);
static void expr(FunctionState *fs, ExpInfo *E);
static void suffixedexp(FunctionState *fs, ExpInfo *E);
static void stm(FunctionState *fs);




/*========================== ERROR =========================*/

// Compile-time error
static void error(FunctionState *fs, const char *error, ...)
{
	Token *token = &PREVT(fs);
	va_list args;
	cr_assert(vm, token->type != TOK_ERROR, "Only lexer can register error tokens.");
	// If panic bit is on, then sync the lexer before registering any new errors.
	if (fs->lexer->panic)
		return;
	va_start(args, error);
	regcomperror(fs->lexer, error, args);
	va_end(args);
}




//======================= CODE =======================//

// Multi-byte instruction (up to 4 bytes)
#define CODEOP(fs, code, param) writechunk_codewparam(CHUNK(fs), code, param, PREVT(fs).line)

// Single byte instruction
#define CODE(fs, byte)                                       \
	({                                                  \
		(fs)->fn->gotret = ((byte) == OP_RET);       \
		writechunk(CHUNK(fs), byte, PREVT(fs).line); \
	})

// 3 byte parameter
#define CODEL(fs, bytes)                  \
	do {                             \
		CODE(fs, BYTE(bytes, 0)); \
		CODE(fs, BYTE(bytes, 1)); \
		CODE(fs, BYTE(bytes, 2)); \
	} while (0)

// Emit jump instruction
#define CODEJMP(fs, jmp)            \
	({                         \
		CODEOP(fs, jmp, 0); \
		codeoffset(fs) - 3; \
	})

// Emit pop instruction
#define CODEPOP(fs, n)                                  \
	do {                                           \
		if (n > 0) {                           \
			if ((n) > 1)                   \
				CODEOP(fs, OP_POPN, n); \
			else                           \
				CODE(fs, OP_POP);       \
		}                                      \
	} while (0)

// Emit unary instruction
#define CODEUN(fs, opr) CODE(fs, unopr2op(opr))

// Emit binary instruction
#define CODEBIN(fs, opr) CODE(fs, binopr2op(opr))

// Implicit return
#define implicitreturn(fs) (CODE(fs, OP_NIL), CODE(fs, OP_RET1))

// Class initializer return
#define initreturn(fs) (CODEOP(fs, OP_GET_LOCAL, 0), CODE(fs, OP_RET1))

// Emit loop instruction
static cr_inline void codeloop(FunctionState *fs, int start)
{
	CODE(fs, OP_LOOP);
	int offset = codeoffset(fs) - start + 3;
	if (cr_unlikely(offset >= PARSER_JMP_LIMIT))
		error(fs, comperrors[CE_JMPLIMIT], PARSER_JMP_LIMIT);
	CODEL(fs, offset);
}

// Initialize global variable
#define INIT_GLOBAL(fs, idx, vflags, E)                                 \
	do {                                                           \
		(fs)->vm->globvars.data[idx].flags = vflags;            \
		CODEOP(fs, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, E), idx); \
	} while (0)


// Get local variable
// helper [rmlastins]
static cr_inline void popvarins(FunctionState *fs, ExpInfo *E)
{
	switch (E->et) {
	case EXP_UPVAL:
	case EXP_LOCAL:
	case EXP_GLOBAL:
		if (E->ins.l)
			LINSTRUCTION_POP(fs);
		else
			INSTRUCTION_POP(fs);
		break;
	case EXP_INDEXED:
		// @?: setters are not reachable ?
		switch (*INSTRUCTION(fs, E)) {
		case OP_INDEX:
			SINSTRUCTION_POP(fs);
			break;
		case OP_GET_PROPERTY:
		case OP_GET_SUPER:
			LINSTRUCTION_POP(fs);
			break;
		default:
			cr_unreachable;
		}
		break;
	default:
		cr_unreachable;
	}
}

// helper [rmlastins]
static cr_inline void popcallins(FunctionState *fs, ExpInfo *E)
{
	switch (E->et) {
	case EXP_CALL:
		LINSTRUCTION_POP(fs);
		break;
	case EXP_INVOKE:
		LINSTRUCTION_POP(fs);
		LPARAM_POP(fs);
		break;
	default:
		cr_unreachable;
	}
}

static void rmlastins(FunctionState *fs, ExpInfo *E)
{
	exptt type = E->et;
	if (etisliteral(type))
		SINSTRUCTION_POP(fs);
	else if (etisconst(type))
		LINSTRUCTION_POP(fs);
	else if (etisvar(type))
		popvarins(fs, E);
	else if (etiscall(type))
		popcallins(fs, E);
	else
		switch (type) {
		case EXP_JMP:
			goto panic;
		case EXP_EXPR:
			if (E->ins.binop) {
				LINSTRUCTION_POP(fs);
				break;
			} else {
panic: // FALLTHRU
				PANIC("Tried removing 'and'/'or' expression.");
				cr_unreachable;
			}
		default:
			cr_unreachable;
		}
}




//======================= SCOPE/C-FLOW =======================//

// Start new 'Scope'
static cr_inline void startscope(FunctionState *fs, Scope *S, cr_ubyte isloop, cr_ubyte isswitch)
{
	if (cr_unlikely(fs->S->depth >= CR_BYTECODE_MAX))
		error(fs, comperrors[CE_SCOPE], CR_BYTECODE_MAX);
	S->nlocals = fs->locals.len;
	S->isloop = isloop;
	S->isswitch = isswitch;
	S->depth = fs->S->depth + 1;
	S->prev = fs->S;
	fs->S = S;
}

// End scope and pop locals and/or close captured locals
static void endscope(FunctionState *fs)
{
#define LOCAL_IS_CAPTURED(local) (btest((local)->flags, VCAPTURED_BIT))

	fs->fn->gotret = 0;
	int pop = 0;
	Scope *current = fs->S;
	fs->S = current->prev;
	while (fs->locals.len > 0 && Array_Local_last(&fs->locals)->depth > fs->S->depth) {
		if (LOCAL_IS_CAPTURED(Array_Local_last(&fs->locals))) {
			int capture = 1;
			fs->locals.len--;
			CODEPOP(fs, pop);
			pop = 0; // Reset pop count
			do {
				if (!LOCAL_IS_CAPTURED(Array_Local_last(&fs->locals))) {
					if (capture == 1)
						CODE(fs, OP_CLOSE_UPVAL);
					else
						CODEOP(fs, OP_CLOSE_UPVALN, capture);
					break;
				}
				capture++;
				fs->locals.len--;
			} while (fs->locals.len > 0 && Array_Local_last(&fs->locals)->depth > fs->S->depth);
		} else {
			pop++;
			fs->locals.len--;
		}
	}
	CODEPOP(fs, pop);

#undef LOCAL_IS_CAPTURED
}


static void ControlFlow_init(VM *vm, CFlow *cflow)
{
	cflow->innerlstart = -1;
	cflow->innerldepth = 0;
	cflow->innersdepth = 0;
	Array_Array_cr_int_init(&cflow->breaks, vm);
}

static void ControlFlow_free(CFlow *context)
{
	Array_Array_cr_int_free(&context->breaks, NULL);
	context->innerlstart = -1;
	context->innerldepth = 0;
	context->innersdepth = 0;
}




/*========================== FUNCTION STATE =========================*/

static void F_init(VM *vm, FunctionState *fs, Scope *globscope, Class *cclass, FunctionType fn_type, FunctionState *enclosing)
{
	// Initialize FunctionState state
	fs->vm = vm;
	fs->S = globscope;
	fs->enclosing = enclosing;
	fs->cclass = cclass;
	vm->fs = fs;
	fs->fn = NULL; // Initialize to NULL so gc does not get confused
	fs->fn = OFunction_new(vm);
	fs->fn_type = fn_type;
	fs->vflags = 0;
	ControlFlow_init(vm, &fs->cflow);
	// Setup upvalues storage
	if (enclosing == NULL) {
		fs->tag = -1;
		fs->upvalues = MALLOC(vm, sizeof(Array_Upvalue));
		Array_Upvalue_init(fs->upvalues, vm);
	} else {
		fs->tag = enclosing->tag;
		fs->upvalues = enclosing->upvalues;
	}
	// Setup local variables storage
	Array_Local_init(&fs->locals, vm);
	Array_Local_init_cap(&fs->locals, SHORT_STACK_SIZE);
	fs->locals.len++;
	LocalVar *local = fs->locals.data;
	local->depth = 0;
	local->flags = 0;
	if (fn_type == FN_METHOD) {
		local->name.start = "self";
		local->name.len = 4;
	} else {
		local->name.start = "";
		local->name.len = 0;
	}
	if (fn_type != FN_SCRIPT) {
		fs->fn->p.defline = PREVT(fs).line;
		fs->fn->p.name = OString_new(vm, PREVT(fs).start, PREVT(fs).len);
	}
}

void F_free(FunctionState *fs)
{
	VM *vm = fs->vm;
	ControlFlow_free(&fs->cflow);
	if (fs->enclosing == NULL) {
		cr_assert(fs->vm, fs->fn_type == FN_SCRIPT, "FunctionState is top-level but the type is not 'FN_SCRIPT'.");
		Array_Upvalue_free(fs->upvalues, NULL);
		FREE(vm, fs->upvalues); // free the memory holding the pointer
	}
	Array_Local_free(&fs->locals, NULL);
	vm->fs = fs->enclosing;
	FREE(vm, fs);
}

// Cleanup the function stack in case of internal errors
void _cleanup_function(FunctionState *fs)
{
	if (fs != NULL) {
		L_free(fs->lexer);
		for (FunctionState *fn = fs; fn != NULL; fn = fn->enclosing)
			F_free(fs);
	}
}

void mark_function_roots(VM *vm)
{
	// Mark all functions and token values
	for (FunctionState *current = vm->fs; current != NULL; current = current->enclosing) {
		vmark(vm, PREVT(current).value);
		vmark(vm, CURRT(current).value);
		omark(vm, (GCObject *)current->fn);
	}
}




/* check if 'tk' matches the current token */
static void expect(Lexer *lx, int tk)
{
	const char *err;

	if (lx->t.tk == tk)
		return;
	err = cr_ot_pushfstring(lx->vm, "expected %s", cr_lr_tok2str(lx, tk));
	cr_lr_syntaxerror(lx, err);
}


/* same as 'expect', scan for next token if no error */
static void expectnext(Lexer *lx, int tk)
{
	expect(lx, tk);
	cr_lr_scan(lx);
}


/* same as 'expectnext' but do not invoke syntax error */
static int match(Lexer *lx, int tk)
{
	if (lx->t.tk == tk) {
		cr_lr_scan(lx);
		return 1;
	}
	return 0;
}


static OString *expect_id(Lexer *lx)
{
	OString *s;

	expect(lx, TK_IDENTIFIER);
	s = lx->t.lit.str;
	cr_lr_scan(lx);
	return s;
}


/* End compilation of the function and emit return instruction */
static Function *parse_end(FunctionState *fs)
{
	if (!fs->fn->gotret) { // last statement wasn't 'return' ?
		if (isom(fs) && tagisnoret(fs->tag)) { // om that can't return values with 'return' ?
			if (fs->tag == SS_INIT) { // return 'self'
				CODEOP(fs, OP_GET_LOCAL, 0);
				CODE(fs, OP_RET1);
			} else
				CODE(fs, OP_RET0); // no return
		} else
			implicitreturn(fs); // otherwise implicit 'nil' return
	}
	fs->fn->p.deflastline = PREVT(fs).line;
#ifdef CR_DEBUG_PRINT_CODE
	if (!fs->lexer->error)
		Chunk_debug(fs->vm, CHUNK(fs), fs->fn->name->storage);
#endif
	return fs->fn;
}


// Compiles source code, 'isingscope' determines if the script getting
// compiled is in global or local scope.
// Additionally parser only registers compile-time errors but it does not
// call the usual 'runerror' [@err.h], this ensures that all of the
// compile-time errors will be reported (exhaustive parser).
CriptClosure *parse(VM *vm, BuffReader *br, const char *name, int isingscope)
{
	Scope gscope, lscope;
	FunctionState fs;
	Lexer L;

	if (name == NULL)
		name = "?";
	F_init(vm, &fs, &globalscope, NULL, FN_SCRIPT, vm->fs);
	fs.fn->p.source = OString_new(vm, name, strlen(name));
	L_init(&L, vm, br, fs.fn->p.source);
	startscope(&fs, &gscope, 0, 0);
	if (!isingscope) // script not in global scope?
		startscope(&fs, &lscope, 0, 0);
	advance(&fs); // fetch first token
	while (!match(&fs, TOK_EOF))
		dec(&fs);
	Function *fn = parse_end(&fs);
	cr_ubyte comperr = fs.lexer->error;
	L_free(&L);
	F_free(&fs);
	if (cr_likely(!comperr)) {
		push(vm, OBJ_VAL(fn)); // prevent 'fn' from getting collected
		CriptClosure *closure = OClosure_new(vm, fn);
		pop(vm); // 'fn'
		push(vm, OBJ_VAL(closure));
		return closure;
	}
	return NULL;
}

// Create new local variable
static cr_inline void local_new(FunctionState *fs, Token name)
{
	if (cr_unlikely(cast_int(fs->locals.len) >= PARSER_LVAR_LIMIT)) {
		error(fs, comperrors[CE_LVARLIMIT], PARSER_LVAR_LIMIT);
		return;
	}
	Array_Local_push(&fs->locals, (LocalVar){ name, -1, fs->vflags });
}

// Make local variable but check for redefinitions in local scope
static void make_local(FunctionState *fs, Token *name)
{
	for (int i = fs->locals.len - 1; i >= 0; i--) {
		LocalVar *local = Array_Local_index(&fs->locals, i);
		if (local->depth != -1 && local->depth < fs->S->depth)
			break;
		if (cr_unlikely(nameeq(name, &local->name)))
			error(fs, comperrors[CE_LREDEF], name->len, name->start);
	}
	local_new(fs, *name);
}

// Patch jump instruction
static cr_inline void patchjmp(FunctionState *fs, int jmp_offset)
{
	int offset = codeoffset(fs) - jmp_offset - 3;
	if (cr_unlikely(offset >= PARSER_JMP_LIMIT))
		error(fs, comperrors[CE_JMPLIMIT], PARSER_JMP_LIMIT);
	PUT_BYTES3(&CHUNK(fs)->bcode.data[jmp_offset], offset);
}

static cr_inline void startbreaklist(FunctionState *fs)
{
	Array_cr_int patches;
	Array_cr_int_init(&patches, fs->vm);
	Array_Array_cr_int_push(&fs->cflow.breaks, patches);
}

static cr_inline void patchbreaklist(FunctionState *fs)
{
	Array_cr_int *patches = Array_Array_cr_int_last(&fs->cflow.breaks);
	for (int i = 0; i < cast_int(patches->len); i++)
		patchjmp(fs, patches->data[i]);
}

static cr_inline void endbreaklist(FunctionState *fs)
{
	Array_cr_int last = Array_Array_cr_int_pop(&fs->cflow.breaks);
	Array_cr_int_free(&last, NULL);
}


static cr_inline int make_constant(FunctionState *fs, TValue constant)
{
	if (cr_unlikely(cast_int(CHUNK(fs)->constants.len) > PARSER_CONST_LIMIT))
		error(fs, "Too many constants created in this chunk, limit is '%d'.\n", PARSER_CONST_LIMIT);
	return writechunk_constant(fs->vm, CHUNK(fs), constant);
}


/* 
 * exprstm ::= functioncall
 *           | varlist '=' explist
 */
static void exprstm(Lexer *lx, int lastforclause)
{
	ExpInfo E;
	E.ins.set = 0;
	suffixedexp(fs, &E);
	TType next = CURRT(fs).type;
	if (next == TOK_EQUAL || next == TOK_COMMA) {
		E.ins.set = 1;
		Array_Exp Earr;
		Array_Exp_init(&Earr, fs->vm);
		expect_cond(fs, etisvar(E.et), expectstr("Expect variable."));
		rmlastins(fs, &E); // remove 'OP_GET..'
		Array_Exp_push(&Earr, E);
		int vars = 1;
		while (match(fs, TOK_COMMA)) {
			if (cr_unlikely(vars >= CR_BYTECODE_MAX))
				error(fs, comperrors[CE_VARLIST], CR_BYTECODE_MAX);
			vars++;
			suffixedexp(fs, &E);
			expect_cond(fs, etisvar(E.et), expectstr("Expect variable."));
			Array_Exp_push(&Earr, E);
		}
		expect(fs, TOK_EQUAL, expectstr("Expect '='."));
		E.ins.set = 0;
		int expc = explist(fs, vars, &E);
		if (vars != expc)
			adjustassign(fs, &E, vars, expc);
		codesetall(fs, &Earr);
		Array_Exp_free(&Earr, NULL);
	} else if (etiscall(E.et))
		CODE(fs, OP_POP);
	else
		error(fs, comperrors[CE_EXPSTM]);
	if (!lastclause)
		expect(fs, TOK_SEMICOLON, expectstr("Expect ';'."));
}


cr_sinline LVarInfo *getlocal(FunctionState *fs, int idx)
{
	cr_assert(fs->firstlocal + idx < fs->l->ps->locals.len);
	return &fs->l->ps->locals.ptr[fs->firstlocal + idx];
}


/* add local debug information into Function 'lvars' */
static int registerlocal(Lexer *lx, FunctionState *fs, OString *name)
{
	Function *fn;
	LVar *local;

	fn = fs->fn;
	cr_mm_growvec(lx->vm, &fn->lvars);
	local = fn->lvars.ptr[fn->lvars.len++];
	local->name = name;
	local->alivepc = fn->code.len;
	return fn->lvars.len - 1;
}


/* 
 * Adjust locals by increment 'nlocals' and registering them
 * inside the 'lvars'.
 */
static void adjustlocals(Lexer *lx, int nvars)
{
	FunctionState *fs;
	LVarInfo *lvinfo;
	int idx;
	int i;

	fs = lx->fs;
	for (i = 0; i < nvars; nvars--) {
		idx = fs->nlocals++;
		lvinfo = getlocal(fs, idx);
		lvinfo->s.idx = registerlocal(lx, fs, lvinfo->s.name);
	}
}


/* sets 'upval' in scope that contains variable at 'vidx' */
static void markscope(FunctionState *fs, int vidx)
{
	Scope *s;

	s = fs->s;
	while(s->nlocals - 1 > vidx)
		s = s->prev;
	s->upval = 1;
	fs->close = 1;
}


/* same as 'markscope' but it marks current scope */
static void markcurrscope(FunctionState *fs)
{
	Scope *s;

	s = fs->s;
	s->upval = 1;
	fs->close = 1;
}


/* adds local variable to the 'locals' */
static int addlocal(Lexer *lx, OString *name)
{
	FunctionState *fs = lx->fs;
	LVarVec *locals;
	LVarInfo *lvinfo;

	locals = &fs->l->ps->locals;
	cr_mm_growvec(fs->l->vm, locals);
	lvinfo = &locals->ptr[locals->len++];
	lvinfo->s.name = name;
	vmod(lvinfo->val) = VARREGULAR;
	return locals->len - 1 - fs->firstlocal;
}


/* same as 'addlocal' but use string literal as name */
#define addlocallit(lx,lit) \
	addlocal(lx, cr_lr_newstring(lx, "" lit, SLL(lit)))


/* 
 * Searches for local variable 'name' taking in account name
 * collisions in the same scope.
 */
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e)
{
	int i;
	LVarInfo *lvinfo;
	Lexer *lx;

	lx = fs->l;
	for (i = fs->nlocals - 1; i >= 0; i--) {
		lvinfo = getlocal(fs, i);
		if (name == lvinfo->s.name) {
			if (cr_unlikely(vmod(lvinfo->val) & VARUNINIT)) {
				cr_lr_syntaxerror(lx, cr_ot_pushfstring(lx->vm, 
					"can't read local variable '%s' in its "
					"own initializer", name));
			}
			initvar(fs, e, i);
			return e->et;
		}
	}
	return -1;
}


/* get upvalue 'idx' from 'upvalues' */
cr_sinline UVInfo *getupvalue(FunctionState *fs, int idx)
{
	return &fs->fn->upvalues.ptr[idx];
}


/* create new upvalue in 'upvalues' */
static UVInfo *newupvalue(FunctionState *fs)
{
	UVInfoVec *uvals;

	uvals = &fs->fn->upvalues;
	cr_mm_growvec(fs->l->vm, uvals);
	return &uvals->ptr[uvals->len++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *e)
{
	UVInfo *uv;
	FunctionState *enclosing;

	uv = newupvalue(fs);
	uv->name = name;
	if (e->et == EXP_LOCAL) { /* local var ? */
		uv->onstack = 1;
		uv->idx = e->u.idx;
		uv->mod = vmod(getlocal(fs, e->u.idx)->val);
		cr_assert(name == getlocal(fs, e->u.idx)->s.name);
	} else { /* must be another upvalue */
		cr_assert(e->et == EXP_UVAL);
		enclosing = fs->enclosing;
		uv->onstack = 0;
		uv->idx = e->u.info;
		uv->mod = enclosing->fn->upvalues.ptr[e->u.info].mod;
		cr_assert(name == enclosing->fn->upvalues.ptr[e->u.info].name);
	}
	return fs->fn->upvalues.len - 1;
}


/* searches for upvalue 'name' */
static int searchupvalue(FunctionState *fs, OString *name)
{
	UVInfoVec *upvals;
	int i;

	upvals = &fs->fn->upvalues;
	for (i = 0; i < upvals->len; i++)
		if (upvals->ptr[i]->name == name) return i;
	return -1;
}


/*
 * Search for variable; if 'name' is not local variable
 * try finding the upvalue.
 */
static void searchvar(FunctionState *fs, OString *name, ExpInfo *e, int base)
{
	int ret;

	if (fs == NULL) { /* global ? */
		initexp(e, EXP_VOID, 0);
	} else {
		ret = searchlocal(fs, name, e);
		if (ret >= 0) { /* local ? */
			if (ret == EXP_LOCAL && !base)
				markscope(fs, e->u.vidx);
		} else { /* try upvalue */
			ret = searchupvalue(fs, name);
			if (ret < 0) {
				searchvar(fs->enclosing, name, e, 0);
				if (e->et == EXP_LOCAL || e->et == EXP_UVAL)
					ret = addupvalue(fs, name);
				else return;
			}
			initexp(e, EXP_UVAL, ret);
		}
	}
}


/* get global value 'idx' from 'gvars' */
cr_sinline TValue *getglobal(FunctionState *fs, int idx)
{
	return &fs->l->vm->gvars.ptr[idx];
}


/* create new global in 'gvars' */
static TValue *newglobal(VM *vm)
{
	cr_mm_growvec(vm, &vm->gvars);
	return &vm->gvars.ptr[vm->gvars.len++];
}


/* 
 * Add new global value into 'gvars' and store its
 * name into 'gids'. 
 */
static int addglobal(VM *vm, OString *name)
{
	TValue idx;
	TValue k;

	setemptyvalue(newglobal(vm, name));
	setivalue(&idx, vm->gvars.len - 1);
	setv2s(vm, &k, name);
	cr_ht_set(vm, &vm->gids, &k, &idx);
	return ivalue(&idx);
}


/* get global variable 'name' or create undefined global */
static void globalvar(VM *vm, OString *name, ExpInfo *e)
{
	TValue k;
	TValue o;

	setv2s(vm, &k, name);
	if (!cr_ht_get(vm, &k, &o))
		e->u.vidx = addglobal(vm, name);
	else
		e->u.vidx = ivalue(o);
	e->et = EXP_GLOBAL;
}


/* find variable 'name' */
static void varname(Lexer *lx, OString *name, ExpInfo *e)
{
	searchvar(lx->fs, name, e, 1);
	if (e->et == EXP_VOID)
		globalvar(lx->vm, name, e)
}


#define var(lx,e)	varname(lx, expect_id(lx), e)


#define varlit(lx,l,e) \
	varname(lx, cr_lr_newstring(lx, "" l, SLL(l), e)


/* 
 * self ::= 'self' 
 */
static void self_(Lexer *lx, ExpInfo *e)
{
	if (lx->fs->cs != NULL) {
		varlit(lx, "self", e);
		cr_lr_scan(lx);
		return;
	}
	cr_lr_syntaxerror(lx, "can't use 'self' outside of class declaration");
}


static int explist(Lexer *lx, ExpInfo *e)
{
	int n;

	n = 1;
	expr(lx->fs, e);
	while (match(lx, ',')) {
		expr(lx->fs, e);
	}
	return n;
}


static void dotaccess(Lexer *lx, ExpInfo *e)
{
	cr_lr_scan(lx); /* skip '.' */
	cr_ce_string(lx->fs, expect_id(lx));
	e->et = EXP_INDEXRAW;
}


static void indexaccess(Lexer *lx, ExpInfo *e)
{
	expr(lx->fs, e);
	expect(lx, ']');
	if (eisliteral(e)) {
		if (e->et == EXP_NIL)
			cr_lr_syntaxerror(lx, "can't index with 'nil'");
		e->et = EXP_INDEXK;
	} else {
		e->et = EXP_INDEXED;
	}
}


/* auxiliary function to 'super_' */
static void supdotaccess(Lexer *lx, ExpInfo *e)
{
	cr_lr_scan(lx); /* skip '.' */
	cr_ce_string(lx->fs, expect_id(lx));
	e->et = EXP_INDEXRAWSUP;
}


/* auxiliary function to 'super_' */
static void supidxaccess(Lexer *lx, ExpInfo *e)
{
}


static void super_(Lexer *lx, ExpInfo *e)
{
	FunctionState *fs;
	ExpInfo e2;

	fs = lx->fs;
	if (fs->cs == NULL)
		cr_lr_syntaxerror(lx, "can't use 'super' outside of class decl");
	else if (!fs->cs->superclass)
		cr_lr_syntaxerror(lx, "class has no superclass");
	varlit(lx, "self", e);
	cr_assert(e->et == EXP_LOCAL);
	cr_ce_reservestack(fs, 1); /* instance */
	cr_ce_dischargevar(fs, e);
	cr_lr_scan(lx);
	if (match(lx, '[')) supidxaccess(lx, e);
	else if (match(lx, '.')) supdotaccess(lx, e);
	else cr_lr_syntaxerror(lx, "missing method access after 'super'");
	varlit(lx, "super", &e2);
	cr_assert(e2->et == EXP_UVAL);
	cr_ce_dischargevar(fs, &e2);
	cr_ce_reservestack(fs, 1); /* superclass */
	cr_ce_dischargevar(fs, e);
}


/* 
 * primary_exp ::= '(' exp ')'
 *               | identifier
 *               | 'self'
 */
static void primaryexp(Lexer *lx, ExpInfo *e)
{
	switch (lx->t.tk) {
	case '(':
		cr_lr_scan(lx);
		expr(lx, e);
		expect(lx, ')');
		break;
	case TK_IDENTIFIER:
		var(lx, e);
		break;
	case TK_SELF:
		self_(lx, e);
		break;
	case TK_SUPER:
		super_(lx, e);
		break;
	default:
		cr_lr_syntaxerror(lx, "unexpected symbol");
		break;
	}
}


/* 
 * suffixedexp ::= primaryexp
 *               | primaryexp [dot|call|indexed...] 
 */
static void suffixedexp(Lexer *lx, ExpInfo *e)
{
	primaryexp(lx, e);
	for (;;) {
		switch (lx->t.tk) {
		case '.':
			dotaccess(lx, e);
			break;
		case '(':
			if (etisconst(E->type))
				error(fs, comperrors[CE_CALLCONST]);
			advance(fs);
			codecall(fs, e);
			break;
		case '[':
			advance(fs);
			indexed(fs, e);
			break;
		default:
			return;
		}
	}
}


/* 
 * simpleexp ::= int
 *	       | flt
 *             | string
 *             | 'nil'
 *             | 'true'
 *             | 'false'
 *             | '...'
 *             | suffixedexp 
 */
static void simpleexp(Lexer *lx, ExpInfo *e)
{
	switch (lx->t.tk) {
	case TK_INT:
		initexp(e, EXP_INT, 0);
		e->u.i = lx->t.lit.i;
		break;
	case TK_FLT:
		initexp(e, EXP_FLT, 0);
		e->u.n = lx->t.lit.n;
		break;
	case TK_STRING:
		initexp(e, EXP_STRING, 0);
		e->u.str = lx->t.lit.str;
		break;
	case TK_NIL:
		initexp(e, EXP_NIL, 0);
		break;
	case TK_TRUE:
		initexp(e, EXP_TRUE, 0);
		break;
	case TK_FALSE:
		initexp(e, EXP_FALSE, 0);
		break;
	case TK_DOTS:
		expect_cond(lx, lx->fs->fn.isvararg, 
				"cannot use '...' outside of vararg function");
		initexp(e, EXP_VARARG, cr_ce_vararg(lx->fs, 1));
		break;
	default:
		suffixedexp(lx, e);
		return;
	}
	cr_lr_scan(lx);
}


static Unopr getunopr(int token)
{
	switch (token) {
	case '-': return OPR_MINUS;
	case '~': return OPR_BNOT;
	case '!': return OPR_NOT;
	default: return OPR_NOUNOPR;
	}
}

static Binopr getbinopr(int token)
{
	switch (token) {
	case '+': return OPR_ADD;
	case '-': return OPR_SUB;
	case '*': return OPR_MUL;
	case '/': return OPR_DIV;
	case '%': return OPR_MOD;
	case TK_POW: return OPR_POW;
	case TK_SHR: return OPR_SHR;
	case TK_SHL: return OPR_SHL;
	case TK_BAND: return OPR_BAND;
	case TK_BOR: return OPR_BOR;
	case TK_NE: return OPR_NE;
	case TK_EQ: return OPR_EQ;
	case '<': return OPR_LT;
	case TK_LE: return OPR_LE;
	case '>': return OPR_GT;
	case TK_GE: return OPR_GE;
	case TK_AND: return OPR_AND;
	case TK_OR: return OPR_OR;
	default: return OPR_NOBINOPR;
	}
}


/*
 * Operators with higher 'left' priority are
 * left associative while operators with higher
 * 'right' priority are right associative.
 */
static const struct {
	cr_ubyte left; /* left priority */
	cr_ubyte right; /* right priority */
} priority[] = {
	/* unary operators priority */
	[OPR_MINUS]	= { 12, 12 }, /* '-' */
	[OPR_BNOT]	= { 12, 12 }, /* '~' */
	[OPR_NOT]	= { 12, 12 }, /* '!' */
	/* binary operators priority */
	[OPR_POW]	= { 14, 13 }, /* '**' */
	[OPR_MUL] 	= { 11, 11 }, /* '*' */
	[OPR_DIV] 	= { 11, 11 }, /* '/' */
	[OPR_MOD] 	= { 11, 11 }, /* '%' */
	[OPR_ADD] 	= { 10, 10 },   /* '+' */
	[OPR_SUB] 	= { 10, 10 },   /* '-' */
	[OPR_SHR] 	= { 9, 9 },   /* '>>' */
	[OPR_SHL]	= { 9, 9 },   /* '<<' */
	[OPR_LT]	= { 8, 8 },   /* '<' */
	[OPR_LE]	= { 8, 8 },   /* '<=' */
	[OPR_GT] 	= { 8, 8 },   /* '>' */
	[OPR_GE] 	= { 8, 8 },   /* '>=' */
	[OPR_EQ] 	= { 7, 7 },   /* '==' */
	[OPR_NE] 	= { 7, 7 },   /* '!=' */
	[OPR_BAND]	= { 5, 5 },   /* '&' */
	[OPR_BXOR] 	= { 4, 4 },   /* '^' */
	[OPR_BOR]	= { 3, 3 },   /* '|' */
	[OPR_AND] 	= { 2, 2 },   /* 'and' */
	[OPR_OR]	= { 1, 1 },   /* 'or' */
};


/* 
 * subexpr ::= simpleexp
 *           | '-' simpleexp
 *           | '!' simpleexp
 *           | '~' simpleexp
 *           | simpleexp '+' subexpr
 *           | simpleexp '-' subexpr
 *           | simpleexp '*' subexpr
 *           | simpleexp '/' subexpr
 *           | simpleexp '%' subexpr
 *           | simpleexp '**' subexpr
 *           | simpleexp '>>' subexpr
 *           | simpleexp '<<' subexpr
 *           | simpleexp '==' subexpr
 *           | simpleexp '<' subexpr
 *           | simpleexp '<=' subexpr
 *           | simpleexp '>' subexpr
 *           | simpleexp '>=' subexpr
 *           | simpleexp '&' subexpr
 *           | simpleexp '^' subexpr
 *           | simpleexp '|' subexpr
 *           | simpleexp 'and' subexpr
 *           | simpleexp 'or' subexpr
 */
static Binopr subexp(Lexer *lx, ExpInfo *e, int limit)
{
	Binopr op;
	Unopr uop;
	int line;

	entercstack(lx);
	uop = getunopr(lx->t.tk);
	if (uop != OPR_NOUNOPR) {
		line = lx->line; /* save line */
		cr_lr_scan(lx);
		subexp(lx, e, priority[uop].right);
		cr_ce_unary(lx->fs, uop, e, line);
	} else {
		simpleexp(lx, e);
	}
	op = getbinaryopr(CURRT(fs).type);
	while (op != OPR_NOBINOPR && priority[op].left > limit) {
		ExpInfo E2;
		E2.ins.set = 0;
		advance(fs); // skip binary operator
		shortcircuit(fs, op, E1);
		op nextop = subexp(fs, &E2, priority[op].right);
		postfix(fs, op, E1, &E2);
		op = nextop;
	}
	leavecstack(lx);
	return op;
}


/* expr ::= subexpr */
static void expr(Lexer *lx, ExpInfo *e)
{
	subexp(lx, e, 0);
}



/* -------------------------------------------------------------------------
 * Statements & Declarations
 * ------------------------------------------------------------------------- */

/* vararg ::= '...' */
static int vararg(FunctionState *fs)
{
	if (!fs->fn->isvararg)
		error(fs, comperrors[CE_VARARG]);
	return CODEOP(fs, OP_VARARG, 1);
}


static void setmulret(FunctionState *fs, ExpInfo *E)
{
	switch (E->et) {
	case EXP_CALL:
	case EXP_VARARG:
		SET_RETCNT(fs, E, MULRET);
		break;
	case EXP_INVOKE:
		SET_RETCNTL(fs, E, MULRET);
		break;
	default:
		cr_unreachable;
	}
}

// Adjust assign expressions in case last expression is a function call
static void adjustassign(FunctionState *fs, ExpInfo *E, int left, int right)
{
	int leftover = left - right; // Safety: left < right is a compile error
	switch (E->et) {
	case EXP_CALL:
		SET_RETCNT(fs, E, leftover + 1);
		break;
	case EXP_INVOKE:
		SET_RETCNTL(fs, E, leftover + 1);
		break;
	default:
		if (leftover > 1)
			CODEOP(fs, OP_NILN, leftover);
		else if (leftover == 1)
			CODE(fs, OP_NIL);
		break;
	}
}

// explist ::= expr
//           | expr ',' explist
static int explist(FunctionState *fs, int limit, ExpInfo *E)
{
	int left = limit;
	int got = 0;
	do {
		left--;
		got++;
		if (left < 0)
			error(fs, comperrors[CE_EXPLIST], limit);
		expr(fs, E);
	} while (match(fs, TOK_COMMA));
	return got;
}

static int name(FunctionState *fs, const char *errmsg)
{
	expect(fs, TOK_IDENTIFIER, errmsg);
	Token *name = &PREVT(fs);
	if (fs->S->depth > 0) { // If local scope make local variable
		make_local(fs, name);
		return -1;
	} // Otherwise make global variable
	return MAKE_GLOBAL(fs, name);
}

static LocalVar *upvalvar(FunctionState *fs, int idx)
{
	UpValue *upval = &fs->upvalues->data[idx];
	if (!upval->local)
		upvalvar(fs->enclosing, idx);
	return &fs->locals.data[upval->idx];
}

// helper [exprstm]
static void codeset(FunctionState *fs, ExpInfo *E)
{
	static const char *errfmt = "Can't assign to variable '%.*s', it is declared as 'fixed'.";
	switch (E->et) {
	case EXP_UPVAL: {
		LocalVar *local = upvalvar(fs, E->value);
		if (cr_unlikely(btest(local->flags, VFIXED_BIT)))
			error(fs, errfmt, local->name.len, local->name.start);
		CODEOP(fs, OP_SET_UPVALUE, E->value);
		break;
	}
	case EXP_LOCAL: {
		LocalVar *local = &fs->locals.data[E->value];
		if (cr_unlikely(btest(local->flags, VFIXED_BIT)))
			error(fs, errfmt, local->name.len, local->name.start);
		CODEOP(fs, GET_OP_TYPE(E->value, OP_SET_LOCAL, E), E->value);
		break;
	}
	case EXP_GLOBAL:
		CODEOP(fs, GET_OP_TYPE(E->value, OP_SET_GLOBAL, E), E->value);
		break;
	case EXP_INDEXED:
		if (E->value == NO_VAL)
			CODE(fs, OP_SET_INDEX);
		else
			CODEOP(fs, OP_SET_PROPERTY, E->value);
		break;
	default:
		return;
	}
}

ARRAY_NEW(Array_Exp, Exp);
// helper [exprstm]
static void codesetall(FunctionState *fs, Array_Exp *Earr)
{
	for (int i = 0; i < cast_int(Earr->len); i++) {
		ExpInfo *E = Array_Exp_index(Earr, i);
		codeset(fs, E);
	}
}

static void block(FunctionState *fs)
{
	while (!check(fs, TOK_RBRACE) && !check(fs, TOK_EOF))
		dec(fs);
	expect(fs, TOK_RBRACE, expectstr("Expect '}' after block."));
}

static cr_inline void blockstm(FunctionState *fs)
{
	Scope S;
	startscope(fs, &S, 0, 0);
	block(fs);
	endscope(fs);
}

// arglist ::= name
//           | '...'
//           | name ',' arglist
static void arglist(FunctionState *fs)
{
	do {
		if (match(fs, TOK_DOT_DOT_DOT)) {
			fs->fn->p.isvararg = 1;
			break;
		}
		fs->fn->p.arity++;
		name(fs, expectstr("Expect parameter name."));
		initlocal(fs, 0);
	} while (match(fs, TOK_COMMA));
}

// namelist ::= name
//            | name ',' namelist
static int namelist(FunctionState *fs, Array_cr_int *nameidx)
{
	int names = 0;
	do {
		if (names >= CR_BYTECODE_MAX)
			error(fs, comperrors[CE_NAMELIST], CR_BYTECODE_MAX);
		names++;
		int idx = name(fs, expectstr("Expect name.")); // initialize later
		if (fs->S->depth == 0)
			Array_cr_int_push(nameidx, idx);
	} while (match(fs, TOK_COMMA));
	return names;
}

static void codeassign(FunctionState *fs, int names, Array_cr_int *nameidx)
{
	if (fs->S->depth > 0) {
		for (int i = 0; i < names; i++)
			initlocal(fs, i);
		return;
	}
	cr_assert(fs->vm, names == cast_int(nameidx->len), "name count != indexes array len.");
	while (nameidx->len > 0) {
		int idx = Array_cr_int_pop(nameidx);
		ExpInfo _; // dummy
		INIT_GLOBAL(fs, idx, fs->vflags, &_);
	}
}

// vardec ::= 'var' name ';'
//          | 'var' namelist ';'
//          | 'var' name '=' explist ';'
//          | 'var' namelist '=' explist ';'
//          | 'fixed' 'var' name ';'
//          | 'fixed' 'var' namelist ';'
//          | 'fixed' 'var' name '=' explist ';'
//          | 'fixed' 'var' namelist '=' explist ';'
static void vardec(FunctionState *fs)
{
	if (match(fs, TOK_FIXED)) {
		if (FIS(fs, FFIXED))
			error(fs, expectstr("Expect variable name."));
		FSET(fs, FFIXED);
	}
	Array_cr_int nameidx;
	Array_cr_int_init(&nameidx, fs->vm);
	int names = namelist(fs, &nameidx);
	int expc = 0;
	ExpInfo E;
	E.ins.set = 0;
	if (match(fs, TOK_EQUAL))
		expc = explist(fs, names, &E);
	if (names != expc)
		adjustassign(fs, &E, names, expc);
	codeassign(fs, names, &nameidx);
	Array_cr_int_free(&nameidx, NULL);
	expect(fs, TOK_SEMICOLON, expectstr("Expect ';'."));
}

// fvardec ::= 'fixed' vardec
static cr_inline void fvardec(FunctionState *fs)
{
	FSET(fs, FFIXED);
	advance(fs);
	vardec(fs);
}


// Create and parse a new FunctionState
static void fn(FunctionState *fs, FunctionType type)
{
	FunctionState Fnew;
	Scope globscope, S;
	F_init(fs->vm, &Fnew, &globscope, fs->cclass, type, fs);
	Fnew.fn->p.source = fs->fn->p.source; // source is same
	startscope(&Fnew, &S, 0, 0); // no need to end this scope
	expect(&Fnew, TOK_LPAREN, expectstr("Expect '(' after function name."));
	if (!check(&Fnew, TOK_RPAREN))
		arglist(&Fnew);
	if (Fnew.fn->p.isvararg)
		expect(&Fnew, TOK_RPAREN, expectstr("Expect ')' after '...'."));
	else
		expect(&Fnew, TOK_RPAREN, expectstr("Expect ')' after parameters."));
	int arity = Fnew.fn->p.arity;
	int expected = ominfo[fs->tag].arity;
	if (cr_unlikely(isom(fs) && expected != arity)) {
		error(fs, comperrors[CE_OMSIG], fs->vm->faststatic[fs->tag]->storage, expected, arity);
		Fnew.lexer->panic = 0; // clear panic flag do not sync yet
	}
	expect(&Fnew, TOK_LBRACE, expectstr("Expect '{' before function body."));
	block(&Fnew); // body
	Function *fn = parse_end(&Fnew);
	CODEOP(fs, OP_CLOSURE, make_constant(fs, OBJ_VAL(fn)));
	for (int i = 0; i < fn->p.upvalc; i++) {
		UpValue *upval = Array_Upvalue_index(Fnew.upvalues, i);
		CODE(fs, upval->local ? 1 : 0);
		CODEL(fs, upval->idx);
	}
	F_free(&Fnew);
}

// fndec ::= 'fn' name '(' arglist ')' '{' block '}'
static void fndec(FunctionState *fs)
{
	int idx = name(fs, expectstr("Expect function name."));
	if (fs->S->depth > 0)
		initlocal(fs, 0); // initialize to allow recursion
	fn(fs, FN_FUNCTION);
	ExpInfo _; // dummy
	if (fs->S->depth == 0)
		INIT_GLOBAL(fs, idx, 0, &_);
}

static void method(FunctionState *fs)
{
	TValue identifier;
	int idx;

	expect(fs, TOK_FN, expectstr("Expect 'fn'."));
	expect(fs, TOK_IDENTIFIER, expectstr("Expect method name."));
	identifier = tokintostr(fs->vm, &PREVT(fs));
	idx = make_constant(fs, identifier);
	fs->tag = id2omtag(fs->vm, asstring(identifier));
	fn(fs, FN_METHOD);
	if (fs->tag != -1)
		CODEOP(fs, OP_OVERLOAD, fs->tag - SS_INIT);
	else
		CODEOP(fs, OP_METHOD, idx);
}

static void classdec(FunctionState *fs)
{
	expect(fs, TOK_IDENTIFIER, expectstr("Expect class name."));
	Token class_name = PREVT(fs);
	TValue identifier = tokintostr(fs->vm, &class_name);
	int idx = make_constant(fs, identifier);
	ExpInfo _; // dummy
	CODEOP(fs, OP_CLASS, idx);
	if (fs->S->depth > 0) {
		make_local(fs, &class_name);
		initlocal(fs, 0);
	} else
		INIT_GLOBAL(fs, MAKE_GLOBAL(fs, &class_name), 0, &_);
	Class cclass;
	cclass.enclosing = fs->cclass;
	cclass.superclass = 0;
	fs->cclass = &cclass;
	_.ins.set = 0;
	Scope S;
	if (match(fs, TOK_IMPL)) { // have superclass ?
		expect(fs, TOK_IDENTIFIER, expectstr("Expect superclass name."));
		codevarprev(fs, &_); // get superclass
		if (nameeq(&PREVT(fs), &class_name))
			error(fs, comperrors[CE_INHERIT], ascstring(identifier));
		startscope(fs, &S, 0, 0);
		local_new(fs, syntoken("super"));
		initlocal(fs, 0);
		var(fs, class_name, &_);
		CODE(fs, OP_INHERIT);
		cclass.superclass = 1;
	}
	var(fs, class_name, &_);
	expect(fs, TOK_LBRACE, expectstr("Expect '{' before class body."));
	while (!check(fs, TOK_RBRACE) && !check(fs, TOK_EOF))
		method(fs);
	expect(fs, TOK_RBRACE, expectstr("Expect '}' after class body."));
	CODE(fs, OP_POP); // Pop the class
	if (cclass.superclass)
		endscope(fs);
	fs->cclass = cclass.enclosing;
}

/// call ::= '(' ')'
///        | '(' explist ')'
static OpCode call(FunctionState *fs, ExpInfo *E)
{
	OpCode op;
	if (!check(fs, TOK_RPAREN)) {
		int index = CODE(fs, OP_CALLSTART);
		int argc = explist(fs, PARSER_ARG_LIMIT, E);
		if (argc == 1 && !ethasmulret(E->et)) {
			Array_ubyte_remove(&CHUNK(fs)->bcode, index); // remove 'OP_CALLSTART'
			op = OP_CALL1;
		} else {
			setmulret(fs, E);
			op = OP_CALL;
		}
	} else {
		E->et = EXP_NONE;
		op = OP_CALL0;
	}
	expect(fs, TOK_RPAREN, expectstr("Expect ')'."));
	return op;
}

static void codecall(FunctionState *fs, ExpInfo *E)
{
	OpCode callop = call(fs, E);
	E->et = EXP_CALL;
	if (callop == OP_CALL0 || callop == OP_CALL1)
		E->ins.code = CODE(fs, callop);
	else
		E->ins.code = CODEOP(fs, callop, 1);
}

static void codeinvoke(FunctionState *fs, ExpInfo *E, int idx)
{
	OpCode callop = call(fs, E);
	E->et = EXP_INVOKE;
	OpCode invokeop;
	switch (callop) {
	case OP_CALL0:;
		invokeop = OP_INVOKE0;
		break;
	case OP_CALL1:
		invokeop = OP_INVOKE1;
		break;
	case OP_CALL:
		invokeop = OP_INVOKE;
		break;
	default:
		cr_unreachable;
	}
	E->ins.code = CODEOP(fs, invokeop, idx);
	CODEL(fs, 1); // retcnt
}

static void dec(FunctionState *fs)
{
	fs->vflags = 0;
	if (match(fs, TOK_VAR))
		vardec(fs);
	else if (match(fs, TOK_FIXED))
		fvardec(fs);
	else if (match(fs, TOK_FN))
		fndec(fs);
	else if (match(fs, TOK_CLASS))
		classdec(fs);
	else
		stm(fs);
	if (fs->lexer->panic)
		sync(fs);
}


// 'switch' statement state.
typedef struct {
	int patch; // Jump to patch if case expression does not match
	enum {
		CS_MATCH, // Case is constant expression match
		CS_DFLT, // Case is 'default'
		CS_NONE, // Did not parse any cases yet
		CS_CASE, // Case is 'case'
	} casestate;
	Array_Value constants; // all case constant expressions
	cr_ubyte dflt : 1; // if switch has 'default' case
	cr_ubyte havenil : 1; // if switch has 'nil' case
	cr_ubyte havetrue : 1; // if switch has '1' case
	cr_ubyte havefalse : 1; // if switch has '0' case
} SwitchState;

static cr_inline void SwitchState_init(FunctionState *fs, SwitchState *state)
{
	state->patch = -1;
	state->casestate = CS_NONE;
	state->dflt = 0;
	state->havenil = 0;
	state->havetrue = 0;
	state->havefalse = 0;
	Array_Value_init(&state->constants, fs->vm);
}

#define SwitchState_free(state) Array_Value_free(&(state)->constants, NULL);

/*
 * Updates 'switch' constants and checks if the constant
 * already exists, but only if 'e' is constant expression.
 * TODO: Refactor a bit, logic can be compressed
 */
static cr_inline void switchconstants(FunctionState *fs, SwitchState *state, ExpInfo *E)
{
	if (!etisconst(E->type))
		return;
	switch (E->et) {
	case EXP_FALSE:
		if (cr_unlikely(state->havefalse))
			error(fs, comperrors[CE_SWDUP], "0");
		state->havefalse = 1;
		break;
	case EXP_TRUE:
		if (cr_unlikely(state->havetrue))
			error(fs, comperrors[CE_SWDUP], "1");
		state->havetrue = 1;
		break;
	case EXP_NIL:
		if (cr_unlikely(state->havenil))
			error(fs, comperrors[CE_SWDUP], "nil");
		state->havenil = 1;
		break;
	case EXP_STRING:
	case EXP_NUMBER:;
		TValue caseval = *CONSTANT(fs, E);
		cr_ubyte _; // dummy
		for (int i = 0; i < cast_int(state->constants.len); i++) {
			if (cr_unlikely(raweq(state->constants.data[i], caseval))) {
				const char *casename = NULL;
				if (E->et == EXP_STRING)
					casename = vtostr(fs->vm, caseval, 1)->storage;
				else
					casename = dtostr(AS_NUMBER(caseval), &_);
				error(fs, comperrors[CE_SWDUP], casename);
				return;
			}
		}
		Array_Value_push(&state->constants, caseval);
		break;
	default:
		cr_unreachable;
	}
}

static void switchstm(FunctionState *fs)
{
	Context C;
	Scope S;
	SwitchState swstate;
	cr_ubyte inswitch = fs->S->isswitch;
	ExpInfo E1;
	Array_cr_int fts;
	int sdepth;
	savecontext(fs, &C);
	SwitchState_init(fs, &swstate);
	Array_cr_int_init(&fts, fs->vm);
	startscope(fs, &S, 0, 1); // implicit scope
	startbreaklist(fs);
	expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'switch'."));
	E1.ins.set = 0;
	expr(fs, &E1);
	expect(fs, TOK_RPAREN, expectstr("Expect ')' after condition."));
	expect(fs, TOK_LBRACE, expectstr("Expect '{' after ')'."));
	sdepth = fs->cflow.innersdepth;
	fs->cflow.innersdepth = fs->S->depth;
	// Switch must contain case or default before any statements
	if (!check(fs, TOK_RBRACE) && !check(fs, TOK_EOF) && !check(fs, TOK_CASE) && !check(fs, TOK_DEFAULT))
		error(fs, comperrors[CE_SWNOC]);
	// 'switch' body
	while (!match(fs, TOK_RBRACE) && !check(fs, TOK_EOF)) {
		if (match(fs, TOK_CASE) || match(fs, TOK_DEFAULT)) {
			fs->fn->gotret = 0;
			if (swstate.casestate != CS_NONE) {
				Array_cr_int_push(&fts, CODEJMP(fs, OP_JMP));
				if (swstate.casestate != CS_DFLT && swstate.casestate != CS_MATCH)
					patchjmp(fs, swstate.patch);
			}
			swstate.casestate = CS_DFLT;
			if (PREVT(fs).type == TOK_CASE) {
				ExpInfo E2;
				E2.ins.set = 0;
				expr(fs, &E2);
				expect(fs, TOK_COLON, expectstr("Expect ':' after 'case'."));
				switchconstants(fs, &swstate, &E2);
				if (eareconstandeq(fs, &E1, &E2)) {
					restorecontext(fs, &C);
					swstate.casestate = CS_MATCH;
				} else {
					CODE(fs, OP_EQ);
					swstate.casestate = CS_CASE;
					swstate.patch = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
				}
			} else if (!swstate.dflt) {
				swstate.dflt = 1;
				swstate.casestate = CS_DFLT;
				expect(fs, TOK_COLON, expectstr("Expect ':' after 'default'."));
			} else
				error(fs, comperrors[CE_SWDEF]);
			if (fts.len > 0)
				patchjmp(fs, Array_cr_int_pop(&fts));
		} else {
			stm(fs);
			if (swstate.casestate == CS_MATCH && fs->fn->gotret) {
				// @TODO: Implement optimizations.
				// Also check if last 'stm' was 'breakstm' (same effect)
			}
		}
	}
	if (PREVT(fs).type == TOK_EOF)
		error(fs, comperrors[CE_SWRBR]);
	Array_cr_int_free(&fts, NULL);
	SwitchState_free(&swstate);
	endscope(fs);
	CODE(fs, OP_POP); // pop switch expression
	patchbreaklist(fs);
	endbreaklist(fs);
	fs->cflow.innersdepth = sdepth;
	fs->S->isswitch = inswitch;
}

static void ifstm(FunctionState *fs)
{
	ExpInfo E;
	Context C;
	int jmptoelse, jmptoend = -1;
	savecontext(fs, &C);
	expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'if'."));
	E.ins.set = 0;
	expr(fs, &E); // condition
	expect(fs, TOK_RPAREN, expectstr("Expect ')' after condition."));
	cr_ubyte remove = 0;
	cr_ubyte istrue = 0;
	if (etisconst(E.type)) {
		rmlastins(fs, &E);
		if (etisfalse(E.et))
			remove = 1;
		else
			istrue = 1;
	} else
		jmptoelse = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
	stm(fs);
	if (!remove) {
		jmptoend = CODEJMP(fs, OP_JMP);
		if (!istrue)
			patchjmp(fs, jmptoelse);
		else if (fs->fn->gotret) { // condition is 1 and 'stm' was a 'returnstm'
			// @TODO: Implement optimization.
		}
	} else
		restorecontext(fs, &C);
	fs->fn->gotret = 0;
	if (match(fs, TOK_ELSE)) { // there is 'else' branch?
		stm(fs);
		if (!remove)
			patchjmp(fs, jmptoend);
	}
}

static cr_inline void startloop(FunctionState *fs, int *lstart, int *ldepth)
{
	*lstart = (fs)->cflow.innerlstart;
	*ldepth = (fs)->cflow.innerldepth;
	(fs)->cflow.innerlstart = codeoffset(fs);
	(fs)->cflow.innerldepth = fs->S->depth;
}

static cr_inline void endloop(FunctionState *fs, int lstart, int ldepth)
{
	(fs)->cflow.innerlstart = lstart;
	(fs)->cflow.innerldepth = ldepth;
}

static void whilestm(FunctionState *fs)
{
	Scope S;
	Context C;
	ExpInfo E;
	int lstart, ldepth;
	int jmptoend = -1;
	cr_ubyte infinite = 0;
	cr_ubyte remove = 0;
	savecontext(fs, &C);
	startscope(fs, &S, 1, 0);
	startloop(fs, &lstart, &ldepth);
	startbreaklist(fs);
	expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'while'."));
	E.ins.set = 0;
	expr(fs, &E); // conditional
	if (etisconst(E.type)) {
		rmlastins(fs, &E);
		if (etisfalse(E.et))
			remove = 1;
		else
			infinite = 1;
	} else
		jmptoend = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
	expect(fs, TOK_RPAREN, expectstr("Expect ')' after condition."));
	stm(fs); // body
	cr_ubyte gotret = fs->fn->gotret;
	endscope(fs);
	if (!remove) {
		codeloop(fs, (fs)->cflow.innerlstart);
		if (!infinite) {
			cr_assert(fs->vm, jmptoend != -1, "end jmp invalid but flag is 0.");
			patchjmp(fs, jmptoend);
		} else if (gotret) { // cond 1 and 'stm' was 'returnstm'
			// @TODO: Implement optimizations
		}
		patchbreaklist(fs);
	} else
		restorecontext(fs, &C);
	endloop(fs, lstart, ldepth);
	endbreaklist(fs);
}

static void forstm(FunctionState *fs)
{
	Scope S;
	Context C;
	ExpInfo E;
	int lstart, ldepth;
	int jmptoend = -1;
	cr_ubyte remove = 0;
	cr_ubyte infinite = 0;
	startscope(fs, &S, 1, 0);
	startbreaklist(fs);
	expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'for'."));
	if (match(fs, TOK_SEMICOLON)) // Initializer for-clause
		; // no initializer
	else if (match(fs, TOK_VAR))
		vardec(fs);
	else if (match(fs, TOK_FIXED))
		fvardec(fs);
	else
		exprstm(fs, 0);
	savecontext(fs, &C);
	startloop(fs, &lstart, &ldepth);
	if (!match(fs, TOK_SEMICOLON)) { // conditional
		E.ins.set = 0;
		expr(fs, &E);
		if (etisconst(E.type)) {
			rmlastins(fs, &E);
			if (etistrue(E.type))
				infinite = 1;
			else
				remove = 1;
		} else
			jmptoend = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
		expect(fs, TOK_SEMICOLON, expectstr("Expect ';' after for-loop condition clause."));
	} else
		infinite = 1;
	if (!match(fs, TOK_RPAREN)) { // last for-clause
		int jmptobody = -1;
		int jmptoincr = -1;
		if (!infinite && !remove)
			jmptobody = CODEJMP(fs, OP_JMP);
		if (!remove)
			jmptoincr = codeoffset(fs);
		exprstm(fs, 1);
		if (!infinite && !remove) {
			codeloop(fs, (fs)->cflow.innerlstart);
			patchjmp(fs, jmptobody);
			(fs)->cflow.innerlstart = jmptoincr;
		}
		expect(fs, TOK_RPAREN, expectstr("Expect ')' after last for-loop clause."));
	}
	stm(fs); // Loop body
	if (!remove) {
		codeloop(fs, (fs)->cflow.innerlstart);
		if (!infinite)
			patchjmp(fs, jmptoend);
		else if (fs->fn->gotret) { // 'stm' was 'returnstm' and conditional is 1
			// @TODO: Implement optimizations
		}
		patchbreaklist(fs);
	} else
		restorecontext(fs, &C);
	endscope(fs);
	endloop(fs, lstart, ldepth);
	endbreaklist(fs);
}

static int foreachvars(FunctionState *fs)
{
	int vars = 0;
	do {
		if (vars >= CR_BYTECODE_MAX)
			error(fs, comperrors[CE_VARLIST], CR_BYTECODE_MAX);
		vars++;
		expect(fs, TOK_IDENTIFIER, expectstr("Expect varname."));
		make_local(fs, &PREVT(fs));
		initlocal(fs, 0);
	} while (match(fs, TOK_COMMA));
	return vars;
}

static void newlocalliteral(FunctionState *fs, const char *name)
{
	Token syntok = syntoken(name);
	local_new(fs, syntok);
	initlocal(fs, 0);
}

static void foreachstm(FunctionState *fs)
{
	Scope S;
	int lstart, ldepth, vars, expc, endjmp;
	ExpInfo E;
	startscope(fs, &S, 1, 0);
	S.isgloop = 1; // set as generic loop
	startbreaklist(fs);
	newlocalliteral(fs, "(for iterator)"); // iterator function
	newlocalliteral(fs, "(for invstate)"); // invariant state
	newlocalliteral(fs, "(for cntlvar)"); // control variable
	vars = foreachvars(fs); // declared vars
	expect(fs, TOK_IN, expectstr("Expect 'in'."));
	E.ins.set = 0;
	expr(fs, &E); // iterator factory
	expect_cond(fs, !etisconst(E.type), "Can't call constant expression.");
	expc = 1;
	if (match(fs, TOK_COMMA))
		expc += explist(fs, 2, &E);
	adjustassign(fs, &E, 3, expc);
	startloop(fs, &lstart, &ldepth);
	CODEOP(fs, OP_FOREACH_PREP, vars);
	CODEOP(fs, OP_FOREACH, vars);
	endjmp = CODEJMP(fs, OP_JMP);
	stm(fs);
	CODEPOP(fs, vars);
	codeloop(fs, (fs)->cflow.innerlstart);
	patchjmp(fs, endjmp);
	endscope(fs);
	patchbreaklist(fs);
	endbreaklist(fs);
	endloop(fs, lstart, ldepth);
}

static void loopstm(FunctionState *fs)
{
	Scope S;
	int lstart, ldepth;
	startscope(fs, &S, 1, 0);
	startbreaklist(fs);
	startloop(fs, &lstart, &ldepth);
	stm(fs);
	if (fs->fn->gotret) {
		// @TODO: Implement optimizations
	}
	codeloop(fs, (fs)->cflow.innerlstart);
	endscope(fs);
	patchbreaklist(fs);
	endbreaklist(fs);
	endloop(fs, lstart, ldepth);
}

static cr_inline Scope *loopscope(FunctionState *fs)
{
	Scope *S = fs->S;
	while (S != NULL) {
		if (S->isloop)
			return S;
		S = S->prev;
	}
	return NULL;
}

// Return count of switch statements until the first loop 'Scope'
// or the top-level code counting from the current 'Scope' in the 'FunctionState'.
static cr_inline int switchcnt(FunctionState *fs)
{
	Scope *S = fs->S;
	int count = 0;
	while (S != NULL && S->depth > fs->cflow.innerldepth) {
		if (S->isswitch)
			count++;
		S = S->prev;
	}
	return count;
}

static void continuestm(FunctionState *fs)
{
	expect(fs, TOK_SEMICOLON, expectstr("Expect ';' after 'continue'."));
	if (cr_unlikely(fs->cflow.innerlstart == -1))
		error(fs, comperrors[CE_CONT]);
	else {
		Scope *S = loopscope(fs);
		cr_assert(fs->vm, S != NULL, "Loop scope not found but cflow offset is set.");
		int popn = fs->locals.len - (S->isgloop * 3) - S->nlocals + switchcnt(fs);
		CODEPOP(fs, popn);
		codeloop(fs, fs->cflow.innerlstart);
	}
}

static void breakstm(FunctionState *fs)
{
	expect(fs, TOK_SEMICOLON, expectstr("Expect ';' after 'break'."));
	Array_Array_cr_int *arr = &fs->cflow.breaks;
	int popn = 0;
	Scope *scope = NULL;
	switch (inwhat(fs->S, &scope)) {
	case 0: // in switch
		popn++; // switch expression
	case 1: // FALLTHRU (in loop)
		break;
	case -1: // outside
		error(fs, comperrors[CE_BREAK]);
		return;
	default:
		cr_unreachable;
	}
	popn += fs->locals.len - scope->nlocals;
	CODEPOP(fs, popn);
	Array_cr_int *last = Array_Array_cr_int_last(arr);
	Array_cr_int_push(last, CODEJMP(fs, OP_JMP));
}

/// return ::= 'return' ';'
///          | 'return' explist ';'
static void returnstm(FunctionState *fs)
{
	/* @TODO: Optimize even further by removing all of the cr_unreachable code. */
	static const char *expectstr = expectstr("Expect ';' after return statement values.");
	cr_ubyte gotret = fs->fn->gotret;
	Context C;
	savecontext(fs, &C);
	if (fs->tag != -1) { // overload-able method ?
		const char *method = fs->vm->faststatic[fs->tag]->storage;
		if (tagisnoret(fs->tag)) {
			if (fs->tag == SS_INIT) { // init is a function call in cript syntax
				CODEOP(fs, OP_GET_LOCAL, 0);
				CODE(fs, OP_RET1);
			} else
				CODE(fs, OP_RET0);
			if (!match(fs, TOK_SEMICOLON))
				error(fs, comperrors[CE_NORET], method);
		} else { // has return values
			cr_om tag = fs->tag - SS_INIT;
			switch (tag) {
			case OM_DISPLAY:
			case OM_GETIDX:
			case OM_ADD:
			case OM_SUB:
			case OM_MUL:
			case OM_DIV:
			case OM_MOD:
			case OM_POW:
			case OM_NOT:
			case OM_UMIN:
			case OM_NE:
			case OM_EQ:
			case OM_LT:
			case OM_LE:
			case OM_GT:
			case OM_GE:
			{
				ExpInfo _; // dummy
				_.ins.set = 0;
				explist(fs, 1, &_);
				expect(fs, TOK_SEMICOLON, expectstr);
				CODE(fs, OP_RET1);
				break;
			}
			default:
				cr_unreachable;
			}
		}
	} else if (match(fs, TOK_SEMICOLON)) { // implicit 'nil' return
		implicitreturn(fs);
	} else { // generic return
		int idx = CODE(fs, OP_RETSTART);
		ExpInfo E;
		E.ins.set = 0;
		int retcnt = explist(fs, PARSER_RET_LIMIT, &E);
		expect(fs, TOK_SEMICOLON, expectstr);
		cr_ubyte singleret = (retcnt == 1);
		if (ethasmulret(E.et)) {
			singleret = 0;
			setmulret(fs, &E);
		}
		if (singleret) { // single return value ?
			Array_ubyte_remove(&CHUNK(fs)->bcode, idx); // remove 'OP_RETSTART'
			CODE(fs, OP_RET1);
		} else
			CODE(fs, OP_RET);
	}
	if (gotret) { // last statement was 'return' ?
		fs->fn->gotret = 1; // this is also a 'return' statement
		restorecontext(fs, &C);
	}
}


/// dot ::= '.' name
///       | '.' name call
static void dot(FunctionState *fs, ExpInfo *E)
{
	expect(fs, TOK_IDENTIFIER, expectstr("Expect property name after '.'."));
	TValue identifier = tokintostr(fs->vm, &PREVT(fs));
	int idx = make_constant(fs, identifier);
	if (match(fs, TOK_LPAREN))
		codeinvoke(fs, E, idx);
	else {
		E->et = EXP_INDEXED;
		E->value = idx;
		if (!E->ins.set)
			E->ins.code = CODEOP(fs, OP_GET_PROPERTY, idx);
	}
}

static void stm(FunctionState *fs)
{
	if (match(fs, TOK_WHILE))
		whilestm(fs);
	else if (match(fs, TOK_FOR))
		forstm(fs);
	else if (match(fs, TOK_FOREACH))
		foreachstm(fs);
	else if (match(fs, TOK_IF))
		ifstm(fs);
	else if (match(fs, TOK_SWITCH))
		switchstm(fs);
	else if (match(fs, TOK_LBRACE))
		blockstm(fs);
	else if (match(fs, TOK_CONTINUE))
		continuestm(fs);
	else if (match(fs, TOK_BREAK))
		breakstm(fs);
	else if (match(fs, TOK_RETURN))
		returnstm(fs);
	else if (match(fs, TOK_LOOP))
		loopstm(fs);
	else if (match(fs, TOK_SEMICOLON))
		; // empty statement
	else
		exprstm(fs, 0);
}

// indexed ::= '[' expr ']'
static cr_inline void indexed(FunctionState *fs, ExpInfo *E)
{
	ExpInfo E2;
	E2.ins.set = 0;
	expr(fs, &E2);
	expect(fs, TOK_RBRACK, expectstr("Expect ']'."));
	E->et = EXP_INDEXED;
	E->value = NO_VAL;
	if (!E->ins.set)
		E->ins.code = CODE(fs, OP_INDEX);
}




/*========================== EXPRESSION =========================*/


// Try folding unary operation.
// Example: OP_CONST (1), OP_NEG => OP_CONST (-1)
static cr_ubyte foldunary(FunctionState *fs, unop opr, ExpInfo *E)
{
	if (E->type == EXP_NUMBER && opr == OPR_NEGATE) {
		double val = AS_NUMBER(*CONSTANT(fs, E));
		if (cr_isnan(val) || val == 0.0)
			return 0;
		*CONSTANT(fs, E) = NUMBER_VAL(-val);
		return 1;
	}
	return 0;
}

// Fold constant number expressions
static void calcnum(FunctionState *fs, binop opr, const ExpInfo *E1, const ExpInfo *E2, TValue *result)
{
#define BINOP(op, n1, n2) NUMBER_VAL((n1)op(n2))

	double n1 = AS_NUMBER(*CONSTANT(fs, E1));
	double n2 = AS_NUMBER(*CONSTANT(fs, E2));
	switch (opr) {
	case OPR_ADD:
		*result = BINOP(+, n1, n2);
		break;
	case OPR_SUB:
		*result = BINOP(-, n1, n2);
		break;
	case OPR_MUL:
		*result = BINOP(*, n1, n2);
		break;
	case OPR_DIV:
		*result = BINOP(/, n1, n2);
		break;
	case OPR_MOD:
		*result = BINOP(%, (long int)n1, (long int)n2);
		break;
	case OPR_POW:
		*result = NUMBER_VAL((cr_powl(n1, n2)));
		break;
	default:
		cr_unreachable;
	}

#undef BINOP
}

// Check if the binary operation is valid
static cr_ubyte validop(FunctionState *fs, binop opr, const ExpInfo *E1, const ExpInfo *E2)
{
	double n1 = AS_NUMBER(*CONSTANT(fs, E1));
	double n2 = AS_NUMBER(*CONSTANT(fs, E2));
	return !(opr == OPR_MOD && (cr_floor(n1) != n1 || cr_floor(n2) != n2));
}

// Try folding binary operation
// Example: OP_CONST (1), OP_CONST (2), OP_ADD => OP_CONST (3)
static cr_ubyte foldbinary(FunctionState *fs, binop opr, ExpInfo *E1, const ExpInfo *E2)
{
	if (E1->type != E2->type || E1->type != EXP_NUMBER || !validop(fs, opr, E1, E2))
		return 0;
	TValue result;
	calcnum(fs, opr, E1, E2, &result);
	if (cr_isnan(AS_NUMBER(result)))
		return 0;
	CONSTANT_POP(fs); // Pop constant (E2)
	*CONSTANT(fs, E1) = result; // Set new constant value (E1)
	LINSTRUCTION_POP(fs); // Pop off the last OP_CONST instruction
	return 1;
}

// Emit optimized 'and' instruction
static void codeand(FunctionState *fs, ExpInfo *E)
{
	switch (E->et) {
	case EXP_TRUE:
		INSTRUCTION_POP(fs);
		goto fin;
	case EXP_STRING:
	case EXP_NUMBER:
		LINSTRUCTION_POP(fs);
		CONSTANT_POP(fs);
fin:
		E->jmp.f = NO_JMP;
		break;
	default:
		E->jmp.f = CODEJMP(fs, OP_JMP_IF_FALSE_OR_POP);
		E->ins.code = codeoffset(fs) - 4; // Index of jump instruction
		break;
	}
	E->jmp.t = NO_JMP;
	E->et = EXP_JMP;
}

// Emit optimized 'or' instruction
static void codeor(FunctionState *fs, ExpInfo *E)
{
	switch (E->et) {
	case EXP_NIL:
	case EXP_FALSE:
		INSTRUCTION_POP(fs);
		E->jmp.t = NO_JMP;
		break;
	case EXP_STRING:
	case EXP_NUMBER:
	case EXP_TRUE:
		E->jmp.t = codeoffset(fs);
		E->jmp.f = CHUNK(fs)->constants.len;
		return;
	default:
		E->jmp.f = CODEJMP(fs, OP_JMP_IF_FALSE_AND_POP);
		E->jmp.t = CODEJMP(fs, OP_JMP);
		patchjmp(fs, E->jmp.f);
		break;
	}
	E->jmp.f = NO_JMP;
	E->jmp.t = EXP_JMP;
}

// Emit binary instruction
static void postfix(FunctionState *fs, binop opr, ExpInfo *E1, ExpInfo *E2)
{
	if (FOLDABLE(opr) && foldbinary(fs, opr, E1, E2))
		return;
	switch (opr) {
	case OPR_ADD:
	case OPR_SUB:
	case OPR_MUL:
	case OPR_DIV:
	case OPR_MOD:
	case OPR_POW:
	case OPR_NE:
	case OPR_EQ:
	case OPR_LT:
	case OPR_LE:
	case OPR_GT:
	case OPR_GE:
		E1->ins.code = CODEBIN(fs, opr);
		E1->et = EXP_EXPR;
		E1->ins.binop = 1;
		break;
	case OPR_OR:
		if (E1->et != EXP_JMP) {
			concatcode(fs, E1->jmp.t + 3, E1->jmp.f);
			*E1 = *E2;
		} else if (E1->jmp.t != NO_JMP) {
			patchjmp(fs, E1->jmp.t);
			*E1 = *E2;
		} else
			E1->et = EXP_EXPR;
		break;
	case OPR_AND:
		if (E1->jmp.f == NO_JMP)
			*E1 = *E2;
		else {
			patchjmp(fs, E1->jmp.f);
			E1->et = EXP_EXPR;
		}
		break;
	default:
		cr_unreachable;
	}
}

// cr_intermediate step that tries to optimize/process 'and' and 'or'
// instructions before the second expression gets parsed.
static void shortcircuit(FunctionState *fs, binop opr, ExpInfo *E)
{
	switch (opr) {
	case OPR_AND:
		codeand(fs, E);
		break;
	case OPR_OR:
		codeor(fs, E);
		break;
	default:
		return;
	}
}

// Emit prefix instruction (only if folding didn't work)
static cr_inline void prefix(FunctionState *fs, unop opr, ExpInfo *E)
{
	if (!foldunary(fs, opr, E))
		CODE(fs, unopr2op(opr));
}


/* -------------------------------------------------------------------------
 * Protected parser
 * ------------------------------------------------------------------------- */

/* data for 'protectedcompile' */
typedef struct PParseData {
	BuffReader *br;
	const char *name;
	cr_ubyte gscope;
} PParseData;


/* protected parser entry */
static void pparse(VM *vm, void *userdata)
{
	PParseData *pcompile;

	pcompile = cast(PParseData *, userdata);
	if (cr_unlikely(compile(vm, pcompile->br, pcompile->name, pcompile->isingscope) == NULL))
		runerror(vm, S_ECOMP);
}


/* external interface for 'protectedparser' */
int cr_pr_pparse(VM *vm, void *userdata, const char *name, cr_ubyte gscope)
{
	BuffReader br;
	PParseData ppdata;

	cr_br_init(vm, &br, (vm)->hooks.reader, userdata);
	ppdata = { &br, name, gscope };
	return pcall(vm, pparse, &ppdata, save_stack(vm, vm->sp));
}
