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
#include "crgc.h"
#include "crlexer.h"
#include "crlimits.h"
#include "crobject.h"
#include "crparser.h"
#include "crstate.h"
#include "crvalue.h"
#include "crstring.h"
#include "crfunction.h"
#include "crvm.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>



#define entercstack(lx)		(cr_ts_inccstack((lx)->ts))

#define leavecstack(lx)		((lx)->ts->nccalls--)


#define expect_cond(lx, cond, err) \
	{ if (!(cond)) cr_lex_syntaxerror(lx, err); }



/* Scope 'cflow' bits */
#define CFLOOP	    1
#define CFGLOOP	    2
#define CFSWITCH	4

/* test 'cflow' bits */
#define scopeisloop(s)      testbit((s)->cflow, CFLOOP)
#define scopeisgloop(s)     testbit((s)->cflow, CFGLOOP)
#define scopeisswitch(s)    testbit((s)->cflow, CFSWITCH)

/* lexical scope information */
typedef struct Scope {
	struct Scope *prev; /* implicit linked-list */
	int nlocals; /* number of locals outside of this scope */
    cr_ubyte cflow; /* control flow context */
	cr_ubyte haveupval; /* set if scope contains upvalue variable */
	cr_ubyte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;



/* forward declare recursive non-terminals */
static void stm(FunctionState *fs);
static void expr(FunctionState *fs, ExpInfo *e);



static cr_noret expecterror(Lexer *lx, int tk)
{
	const char *err = cr_string_pushfstring(lx->ts, "expected %s",
                                            cr_lex_tok2str(lx, tk));
	cr_lex_syntaxerror(lx, err);
}


static cr_noret limiterror(FunctionState *fs, const char *what, int limit)
{
    cr_State *ts = fs->lx->ts;
    const char *err;
    const char *where;
    int line = fs->fn->defline;
    if (line == 0)
        where = "main function";
    else
        where = cr_string_pushfstring(ts, "function at line %d", line);
    err = cr_string_pushfstring(ts, "too many %s (limit is %d) in %s",
                                what, limit, where);
    cr_lex_syntaxerror(fs->lx, err);
}


static void checklimit(FunctionState *fs, int n, int limit, const char *what)
{
    if (n >= limit)
        limiterror(fs, what, limit);
}


/* check if 'tk' matches the current token */
static void expect(Lexer *lx, int tk)
{
	if (lx->t.tk == tk)
		return;
    expecterror(lx, tk);
}


/* same as 'expect', scan for next token if no error */
static void expectnext(Lexer *lx, int tk)
{
	expect(lx, tk);
	cr_lex_scan(lx);
}


/* same as 'expectnext' but do not invoke syntax error */
static int match(Lexer *lx, int tk)
{
	if (lx->t.tk == tk) {
		cr_lex_scan(lx);
		return 1;
	}
	return 0;
}


static OString *expect_id(Lexer *lx)
{
	OString *s;

	expect(lx, TK_IDENTIFIER);
	s = lx->t.lit.str;
	cr_lex_scan(lx);
	return s;
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
	Array_cr_int_init(&patches, fs->ts);
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



/* -------------------------------------------------------------------------
 * Statements
 * ------------------------------------------------------------------------- */

/*
 * Used to chain variables on the left side of
 * the assignment.
 */
typedef struct LHS {
	struct LHS *prev;
	ExpInfo *e;
} LHS;


static void assignmore(Lexer *lx, LHS *lhs, ExpInfo *e)
{
}


/*
 * exprstm ::= functioncall
 *           | varlist '=' explist
 */
static void exprstm(Lexer *lx, int lastforclause)
{
	ExpInfo e;
	e.ins.set = 0;
	suffixedexp(fs, &E);
	TType next = CURRT(fs).type;
	if (next == TOK_EQUAL || next == TOK_COMMA) {
		e.ins.set = 1;
		Array_Exp Earr;
		Array_Exp_init(&Earr, fs->ts);
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
		e.ins.set = 0;
		int expc = explist(fs, vars, &E);
		if (vars != expc)
			adjustassign(fs, &E, vars, expc);
		codesetall(fs, &Earr);
		Array_Exp_free(&Earr, NULL);
	} else if (etiscall(e.et))
		CODE(fs, OP_POP);
	else
		error(fs, comperrors[CE_EXPSTM]);
	if (!lastclause)
		expect(fs, TOK_SEMICOLON, expectstr("Expect ';'."));
}


cr_sinline LocalVar *getlocal(FunctionState *fs, int idx)
{
	cr_assert(fs->firstlocal + idx < fs->l->ps->locals.len);
	return &fs->lx->ps->locals.ptr[fs->firstlocal + idx];
}


/* add local debug information into Function 'lvars' */
static int registerlocal(Lexer *lx, FunctionState *fs, OString *name)
{
	Function *fn;
	LVar *local;

	fn = fs->fn;
	cr_mem_growvec(lx->ts, &fn->autovars);
	local = fn->lvars.ptr[fn->lvars.len++];
	local->name = name;
	local->alivepc = fn->code.len;
	return fn->autovars.len - 1;
}


/*
 * Adjust locals by increment 'nlocals' and registering them
 * inside the 'lvars'.
 */
static void adjustlocals(Lexer *lx, int nvars)
{
	FunctionState *fs;
	LocalVar *lvinfo;
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

	s = fs->scope;
	while(s->nlocals - 1 > vidx)
		s = s->prev;
	s->intbc = 1;
	fs->close = 1;
}


/* same as 'markscope' but it marks current scope */
static void markcurrscope(FunctionState *fs)
{
	Scope *s;

	s = fs->scope;
	s->intbc = 1;
	fs->close = 1;
}


/* adds local variable to the 'locals' */
static int addlocal(Lexer *lx, OString *name)
{
	FunctionState *fs = lx->fs;
	AutoVarVec *locals;
	LocalVar *lvinfo;

	locals = &fs->lx->ps->locals;
	cr_mem_growvec(fs->lx->ts, locals);
	lvinfo = &locals->ptr[locals->len++];
	lvinfo->s.name = name;
	vmod(lvinfo->val) = VARREGULAR;
	return locals->len - 1 - fs->firstlocal;
}


/* same as 'addlocal' but use string literal as name */
#define addlocallit(lx,lit) \
	addlocal(lx, cr_lex_newstring(lx, "" lit, SLL(lit)))


/*
 * Searches for local variable 'name' taking in account name
 * collisions in the same scope.
 */
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e)
{
	int i;
	LocalVar *lvinfo;
	Lexer *lx;

	lx = fs->lx;
	for (i = fs->nlocals - 1; i >= 0; i--) {
		lvinfo = getlocal(fs, i);
		if (name == lvinfo->s.name) {
			if (cr_unlikely(vmod(lvinfo->val) & VARUNINIT)) {
				cr_lex_syntaxerror(lx, cr_object_pushfstring(lx->ts,
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
cr_sinline UpValInfo *getupvalue(FunctionState *fs, int idx)
{
	return &fs->fn->upvals.ptr[idx];
}


/* create new upvalue in 'upvalues' */
static UpValInfo *newupvalue(FunctionState *fs)
{
	UVInfoVec *uvals;

	uvals = &fs->fn->upvals;
	cr_mem_growvec(fs->lx->ts, uvals);
	return &uvals->ptr[uvals->len++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *e)
{
	UpValInfo *uv;
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
		enclosing = fs->prev;
		uv->onstack = 0;
		uv->idx = e->u.info;
		uv->mod = enclosing->fn->upvals.ptr[e->u.info].mod;
		cr_assert(name == enclosing->fn->upvalues.ptr[e->u.info].name);
	}
	return fs->fn->upvals.len - 1;
}


/* searches for upvalue 'name' */
static int searchupvalue(FunctionState *fs, OString *name)
{
	UVInfoVec *upvals;
	int i;

	upvals = &fs->fn->upvals;
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
				searchvar(fs->prev, name, e, 0);
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
	return &fs->lx->ts->gvars.ptr[idx];
}


/* create new global in 'gvars' */
static TValue *newglobal(cr_State *ts)
{
	cr_mem_growvec(ts, &ts->gvars);
	return &ts->gvars.ptr[ts->gvars.len++];
}


/*
 * Add new global value into 'gvars' and store its
 * name into 'gids'.
 */
static int addglobal(cr_State *ts, OString *name)
{
	TValue idx;
	TValue k;

	setemptyvalue(newglobal(ts, name));
	setivalue(&idx, ts->gvars.len - 1);
	setv2s(ts, &k, name);
	cr_htable_set(ts, &ts->gids, &k, &idx);
	return ivalue(&idx);
}


/* get global variable 'name' or create undefined global */
static void globalvar(cr_State *ts, OString *name, ExpInfo *e)
{
	TValue k;
	TValue o;

	setv2s(ts, &k, name);
	if (!cr_htable_get(ts, &k, &o))
		e->u.idx = addglobal(ts, name);
	else
		e->u.idx = ivalue(o);
	e->et = EXP_GLOBAL;
}


/* find variable 'name' */
static void varname(Lexer *lx, OString *name, ExpInfo *e)
{
	searchvar(lx->fs, name, e, 1);
	if (e->et == EXP_VOID)
		globalvar(lx->ts, name, e)
}


#define var(lx,e) \
	{ varname(lx, expect_id(lx), e); \
	  cr_code_reservestack((lx)->fs, 1); }


#define varlit(lx,l,e) \
	{ varname(lx, cr_lex_newstring(lx, "" l, SLL(l), e); \
	  cr_code_reservestack((lx)->fs, 1); }


/*
 * self ::= 'self'
 */
static void self_(Lexer *lx, ExpInfo *e)
{
	if (lx->fs->cs != NULL) {
		varlit(lx, "self", e);
		cr_lex_scan(lx);
		return;
	}
	cr_lex_syntaxerror(lx, "can't use 'self' outside of class declaration");
}


static int explist(Lexer *lx, ExpInfo *e)
{
	int n;

	n = 1;
	expr(lx->fs, e);
	while (match(lx, ','))
		expr(lx->fs, e);
	return n;
}


static void dotaccess(Lexer *lx, ExpInfo *e)
{
	cr_lex_scan(lx); /* skip '.' */
	e->u.idx = cr_code_string(lx->fs, expect_id(lx));
	e->et = EXP_INDEXRAW;
}


static void indexaccess(Lexer *lx, ExpInfo *e)
{
	expr(lx->fs, e);
	expect(lx, ']');
	if (eisliteral(e)) {
		if (e->et == EXP_NIL)
			cr_lex_syntaxerror(lx, "can't index with 'nil'");
		e->et = EXP_INDEXK;
	} else {
		e->et = EXP_INDEXED;
	}
}


/* auxiliary function to 'super_' */
static void supdotaccess(Lexer *lx, ExpInfo *e)
{
	cr_lex_scan(lx); /* skip '.' */
	e->u.idx = cr_code_string(lx->fs, expect_id(lx));
	e->et = EXP_INDEXRAWSUP;
}


/* auxiliary function to 'super_' */
static void supidxaccess(Lexer *lx, ExpInfo *e)
{
	OString *id;

	id = lx->t.lit.str;
	if (match(lx, TK_IDENTIFIER)) e->et = EXP_INDEXSUP;
	else if (match(lx, TK_STRING)) e->et = EXP_INDEXRAWSUP;
	else cr_lex_syntaxerror(lx, "invalid index value for 'super'");
	e->u.idx = cr_code_string(lx->fs, id);
	expect(lx, ']');
}


static void super_(Lexer *lx, ExpInfo *e)
{
	FunctionState *fs;
	ExpInfo e2;

	fs = lx->fs;
	if (fs->cs == NULL)
		cr_lex_syntaxerror(lx, "can't use 'super' outside of class decl");
	else if (!fs->cs->superclass)
		cr_lex_syntaxerror(lx, "class has no superclass");
	varlit(lx, "self", e);
	cr_assert(e->et == EXP_LOCAL);
	cr_code_dischargevar(fs, e);
	cr_lex_scan(lx);
	if (match(lx, '[')) supidxaccess(lx, e);
	else if (match(lx, '.')) supdotaccess(lx, e);
	else cr_lex_syntaxerror(lx, "missing method access after 'super'");
	varlit(lx, "super", &e2);
	cr_assert(e2->et == EXP_UVAL);
	cr_code_dischargevar(fs, &e2);
	cr_code_dischargevar(fs, e);
}


/*
 * primary_exp ::= '(' exp ')'
 *               | identifier
 *               | 'self'
 *               | 'super' '.' id
 *               | 'super' '[' id ']'
 *               | 'super' '[' string ']'
 */
static void primaryexp(Lexer *lx, ExpInfo *e)
{
	switch (lx->t.tk) {
	case '(':
		cr_lex_scan(lx);
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
		cr_lex_syntaxerror(lx, "unexpected symbol");
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
		initexp(e, EXP_VARARG, cr_code_vararg(lx->fs, 1));
		break;
	default:
		suffixedexp(lx, e);
		return;
	}
	cr_lex_scan(lx);
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
		cr_lex_scan(lx);
		subexp(lx, e, priority[uop].right);
		cr_code_unary(lx->fs, uop, e, line);
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
		upvalvar(fs->prev, idx);
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

static void block(Lexer *lx)
{
	while (!check(fs, TOK_RBRACE) && !check(fs, TOK_EOF))
		stm(lx);
	expect(fs, TOK_RBRACE, expectstr("Expect '}' after block."));
}

static cr_inline void blockstm(FunctionState *fs)
{
	Scope S;
	startscope(fs, &S, 0, 0);
	block(fs);
	leavescope(fs);
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
	cr_assert(fs->ts, names == cast_int(nameidx->len), "name count != indexes array len.");
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
static void vardec(Lexer *lx, int haveconst)
{
	if (match(fs, TOK_FIXED)) {
		if (FIS(fs, FFIXED))
			error(fs, expectstr("Expect variable name."));
		FSET(fs, FFIXED);
	}
	Array_cr_int nameidx;
	Array_cr_int_init(&nameidx, fs->ts);
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


// Create and parse a new FunctionState
static void fn(FunctionState *fs, FunctionType type)
{
	FunctionState Fnew;
	Scope globscope, S;
	startfs(fs->ts, &Fnew, &globscope, fs->cclass, type, fs);
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
		error(fs, comperrors[CE_OMSIG], fs->ts->faststatic[fs->tag]->storage, expected, arity);
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
	endfs(&Fnew);
}

// fndec ::= 'fn' name '(' arglist ')' '{' block '}'
static void fndec(Lexer *lx)
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
	identifier = tokintostr(fs->ts, &PREVT(fs));
	idx = make_constant(fs, identifier);
	fs->tag = id2omtag(fs->ts, asstring(identifier));
	fn(fs, FN_METHOD);
	if (fs->tag != -1)
		CODEOP(fs, OP_OVERLOAD, fs->tag - SS_INIT);
	else
		CODEOP(fs, OP_METHOD, idx);
}

static void classdec(Lexer *lx)
{
	expect(fs, TOK_IDENTIFIER, expectstr("Expect class name."));
	Token class_name = PREVT(fs);
	TValue identifier = tokintostr(fs->ts, &class_name);
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
		leavescope(fs);
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
	Array_Value_init(&state->constants, fs->ts);
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
					casename = vtostr(fs->ts, caseval, 1)->storage;
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
	Array_cr_int_init(&fts, fs->ts);
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
	leavescope(fs);
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

static void whilestm(Lexer *lx)
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
	leavescope(fs);
	if (!remove) {
		codeloop(fs, (fs)->cflow.innerlstart);
		if (!infinite) {
			cr_assert(fs->ts, jmptoend != -1, "end jmp invalid but flag is 0.");
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

static void forstm(Lexer *lx)
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
		constvardec(fs);
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
	leavescope(fs);
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
	leavescope(fs);
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
	leavescope(fs);
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
		cr_assert(fs->ts, S != NULL, "Loop scope not found but cflow offset is set.");
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
		const char *method = fs->ts->faststatic[fs->tag]->storage;
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
	TValue identifier = tokintostr(fs->ts, &PREVT(fs));
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

static void stm(Lexer *lx)
{
    switch (lx->t.tk) {
    case TK_WHILE:
        cr_lex_scan(lx);
		whilestm(lx);
        break;
	case TK_FOR:
        cr_lex_scan(lx);
		forstm(lx);
        break;
	case TK_FOREACH:
        cr_lex_scan(lx);
		foreachstm(lx);
        break;
	case TK_IF:
        cr_lex_scan(lx);
		ifstm(lx);
        break;
	case TK_SWITCH:
        cr_lex_scan(lx);
		switchstm(lx);
        break;
	case '{':
        cr_lex_scan(lx);
		blockstm(lx);
        break;
	case TK_CONTINUE:
        cr_lex_scan(lx);
		continuestm(lx);
        break;
	case TK_BREAK:
        cr_lex_scan(lx);
		breakstm(lx);
        break;
	case TK_RETURN:
        cr_lex_scan(lx);
		returnstm(lx);
        break;
	case TK_LOOP:
        cr_lex_scan(lx);
		loopstm(lx);
        break;
	case ';': /* empty statement */
        break;
    default:
        cr_lex_scan(lx);
		exprstm(lx, 0);
        break;
    }
}


static void dec(Lexer *lx)
{
    int isconst = 0;

    switch (lx->t.tk) {
	case TK_CONST:
        isconst = 1;
        /* FALLTHRU */
    case TK_LET:
        cr_lex_scan(lx);
		vardec(lx, isconst);
        break;
	case TK_FN:
        cr_lex_scan(lx);
		fndec(lx);
        break;
	case TK_CLASS:
        cr_lex_scan(lx);
		classdec(lx);
        break;
    default:
        stm(lx);
        break;
    }
}


static inline void parseuntileos(Lexer *lx) {
    while (!lx->t.tk != TK_EOS)
        dec(lx);
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


static Scope *getscope(FunctionState *fs, int idx)
{
	Scope *scope = fs->scope;

	while (scope != NULL) {
		if (scope->nlocals >= idx)
			break;
		scope = scope->prev;
	}
	return scope;
}



static void enterscope(FunctionState *fs, Scope *s, int cflow)
{
	s->nlocals = fs->nlocals;
	s->cflow = cflow;
    s->haveupval = 0;
	s->havetbcvar = (fs->scope != NULL && fs->scope->havetbcvar);
	s->prev = fs->scope;
	fs->scope = s;
}


static void leavescope(FunctionState *fs)
{
    Scope *s = fs->scope;
    fs->scope = s->prev;
    /* TODO */
}


static void startfs(FunctionState *fs, Lexer *lx, Scope *s)
{
    cr_assert(fs->fn != NULL);
	fs->prev = lx->fs;
	fs->lx = lx;
	lx->fs = fs;
	fs->scope = NULL;
	fs->sp = 0;
	fs->nlocals = 0;
	fs->firstlocal = lx->ps->lvars.len;
    fs->firstbreak = lx->ps->breaks.len;
    fs->innerloopstart = -1;
    fs->innerloopdepth = 0;
    fs->innerswitchdepth = 0;
	fs->close = 0;
	fs->fn->source = lx->src;
	cr_gc_objbarrier(lx->ts, fs->fn, fs->fn->source);
    fs->fn->maxstack = 1; /* for the function itself */
	enterscope(fs, s, 0);
}

static void endfs(FunctionState *fs)
{
	cr_State *ts = fs->lx->ts;
    Function *fn = fs->fn;
    Lexer *lx = fs->lx;
    cr_code_ret(fs, fs->nlocals - 1, 0);
    leavescope(fs);
    cr_assert(fs->scope == NULL); /* last scope */
    cr_mem_shrinkvec(ts, fn->fn, fn->sizefn, fs->nfn);
    cr_mem_shrinkvec(ts, fn->constants, fn->sizeconst, fn->nconst);
    cr_mem_shrinkvec(ts, fn->code, fn->sizecode, fn->ncode);
    cr_mem_shrinkvec(ts, fn->linfo, fn->sizelinfo, fn->nlinfo);
    cr_mem_shrinkvec(ts, fn->locals, fn->sizelocals, fn->nlocals);
    cr_mem_shrinkvec(ts, fn->upvals, fn->sizeupvals, fn->nupvals);
    lx->fs = fs->prev;
    cr_gc_check(ts);
}


static Function *addfunction(Lexer *lx)
{
    Function *new;
    cr_State *ts = lx->ts;
    FunctionState *fs = lx->fs;
    Function *fn = fs->fn;
    checklimit(fs, fn->nfn + 1, INT_MAX, "function");
    cr_mem_growvec(ts, fn->fn, fn->sizefn, fn->nfn, INT_MAX, "functions");
    fn->fn[fn->nfn++] = new = cr_function_new(ts);
    cr_gc_objbarrier(ts, fn, new);
    return new;
}


/* set current function as vararg */
static void setvararg(FunctionState *fs, int arity) {
    fs->fn->isvararg = 1;
    cr_code_codelarg(fs, OP_SETVARARG, arity);
}


/* allocate space for new 'UpValInfo' */
static UpValInfo *newupvalinfo(FunctionState *fs)
{
    Function *fn = fs->fn;
    cr_State *ts = fs->lx->ts;
    checklimit(fs, fn->nupvals + 1, INT_MAX, "upvalues");
    cr_mem_growvec(ts, fn->upvals, fn->sizeupvals, fn->nupvals, INT_MAX,
                   "upvalues");
    return &fn->upvals[fn->nupvals++];
}


/* compile main function */
static void criptmain(FunctionState *fs, Lexer *lx)
{
    Scope gscope;
    startfs(fs, lx, &gscope);
    setvararg(fs, 0); /* main is always vararg */
    cr_lex_scan(lx); /* scan first token */
    parseuntileos(lx);
    cr_assert(lx->t.tk == TK_EOS);
    endfs(fs);
}


CrClosure *parse(cr_State *ts, BuffReader *br, Buffer *buff, ParserState *ps,
                 const char *source)
{
    Lexer lx;
	FunctionState fs;
    CrClosure *cl = cr_function_newcrclosure(ts, 0);
    setsv2crcl(ts, ts->stacktop.p, cl); /* anchor main function closure */
    cr_vm_inctop(ts);
    lx.tab = cr_htable_new(ts);
    setsv2ht(ts, ts->stacktop.p, lx.tab); /* anchor scanner htable */
    cr_vm_inctop(ts);
    fs.fn = cl->fn = cr_function_new(ts);
    cr_gc_objbarrier(ts, cl, cl->fn);
    fs.fn->source = cr_string_new(ts, source);
    cr_gc_objbarrier(ts, fs.fn, fs.fn->source);
    lx.ps = ps;
    lx.buff = buff;
    cr_lex_setsource(ts, &lx, br, fs.fn->source);
    criptmain(&fs, &lx); /* Cript main function */
    ts->stacktop.p--; /* pop scanner htable */
    return cl;
}



/* -------------------------------------------------------------------------
 * Protected parsing
 * ------------------------------------------------------------------------- */

/* data for 'pparse' */
typedef struct PPData {
	BuffReader *br;
    Buffer buff;
    ParserState ps;
	const char *source;
} PPData;


/* protected 'parse' */
static void pparse(cr_State *ts, void *userdata)
{
	PPData *pcompile = cast(PPData *, userdata);
    CrClosure *cl = parse(ts, pcompile->br, &pcompile->buff, &pcompile->ps,
                          pcompile->source);
    cr_assert(cl->nupvals <= cl->sizeupvals);
    cr_function_initupvals(ts, cl);
}


/* external interface for 'pparse' */
void cr_parser_pparse(cr_State *ts, BuffReader *br, const char *source)
{
	PPData ppdata;

    ppdata.br = br;
    cr_reader_buffinit(&ppdata.buff);
    memset(&ppdata.ps, 0, sizeof(ppdata.ps));
    ppdata.source = source;
	cr_vm_pcall(ts, pparse, &ppdata, savestack(ts, ts->stacktop.p));
}
