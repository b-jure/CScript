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

#include "crparser.h"
#include "crconf.h"
#include "crlexer.h"
#include "crlimits.h"
#include "crobject.h"
#include "crvalue.h"


#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>



/* If 'cr_om' does not return values */
#define omnoret(omtag) ((omtag) == OM_INIT || (omtag) == OM_SETIDX)

/* If current function is overload-able 'method' and does not return values */
#define tagisnoret(tag) ((tag) != -1 && omnoret((tag)-SS_INIT))



/* lexical scope information */
typedef struct Scope {
	struct Scope *prev; /* list */
	int localc; /* count of locals up until this scope */
	int depth; /* scope depth (index) */
	cr_ubyte isloop : 1; /* loop */
	cr_ubyte isgloop : 1; /* generic loop */
	cr_ubyte isswitch : 1; /* switch */
} Scope;



/* class declaration information */
typedef struct ClassDecl {
	struct ClassDecl *enclosing;
	cr_ubyte superclass;
} ClassDecl;



/* compile time error indices */
typedef enum {
	CE_JMPLIMIT, CE_LREAD, CE_GVARLIMIT, CE_LVARLIMIT, CE_LREDEF,
	CE_SWDEF, CE_SWNOC, CE_SWRBR, CE_SWDUP, CE_CONT, CE_BREAK,
	CE_EXPLIST, CE_VARLIST, CE_SCOPE, CE_UVALLIMIT, CE_INHERIT,
	CE_VARARG, CE_NAMELIST, CE_CALLCONST, CE_SELF, CE_SUPER,
	CE_NOSUPER, CE_OMSIG, CE_EXPSTM, CE_NORET, CE_CNT,
} CompErr;

/* compile time errors */
const char *comperrors[CE_CNT] = {
	"\tToo much code to jump over, limit is '%d'.\n",
	"\tCan't read local variable %.*s in its own initializer.\n",
	"\tToo many global values defined in script, limit is '%d'.\n",
	"\tToo many local values defined in script, limit is '%d'.\n",
	"\tRedefinition of local variable '%.*s'.\n",
	"\tMultiple 'default' labels.\n",
	"\tCan't have statements before first case.\n",
	"\tExpect '}' at the end of 'switch'.\n",
	"\tAlready have case with constant '%s'.\n",
	"\t'continue' statement not in loop statement.\n",
	"\t'break' statement not in loop or switch statement.\n",
	"\tToo many expressions in explist, expected at most '%d'.\n",
	"\tToo many variables in varlist, limit is '%d'.\n",
	"\tScope nesting limit reached, limit is '%d'.\n",
	"\tToo many upvalues in '%s', limit is '%d'.\n",
	"\t<class '%s'> can't 'impl' itself.\n",
	"\t'...' can only be used inside functions that accept variable number of arguments.\n",
	"\tToo many names in namelist, limit is '%d'\n",
	"\tAttempted to call a constant value.\n",
	"\tCan't use 'self' outside of a class.\n",
	"\tCan't use 'super' outside of a class.\n",
	"\tCan't use 'super', class does not have a superclass.\n",
	"\tOverload-able method %s expects %d parameters, instead defined %d.\n",
	"\tInvalid statement, expect function call or assignment.\n",
	"\tExpect ';' after explicit 'return' in '%s' (no return values).\n",
};



/* initialize expression */
static cr_inline void initexp(Exp *E, ExpType type, int32_t code, int32_t info)
{
	E->type = type;
	E->t = -1;
	E->f = -1;
	E->ins.code = code;
	E->ins.binop = 0;
	E->u.info = info;
}

// Get constant
#define CONSTANT(F, E) Array_Value_index(&CHUNK(F)->constants, (E)->value)
// Get instruction
#define INSTRUCTION(F, E) byteptr(Array_ubyte_index(&CHUNK(F)->code, (E)->ins.code))

// Expressions are constants and their Values are equal
#define eareconstandeq(F, E1, E2) \
	(etisconst((E1)->type) && etisconst((E2)->type) && raweq(*CONSTANT(F, E1), *CONSTANT(F, E2)))


// Set first long parameter
#define SET_LPARAM(F, E, val) PUT_BYTES3(INSTRUCTION(F, E) + 1, val)
// Set second long parameter
#define SET_LLPARAM(F, E, val) PUT_BYTES3(INSTRUCTION(F, E) + 4, val)
// Set first short parameter
#define SET_SPARAM(F, E, val) PUT_BYTE(INSTRUCTION(F, E) + 1, val)
// Set instruction return count (first param)
#define SET_RETCNT(F, E, cnt) SET_LPARAM(F, E, cnt)
// Set instruction return count (second param)
#define SET_RETCNTL(F, E, cnt) SET_LLPARAM(F, E, cnt)

// Pop last instruction parameter (1 byte)
#define PARAM_POP(F)                                                               \
	do {                                                                       \
		cr_assert((F)->vm, CHUNK(F)->code.len >= 1, "Invalid PARAM_POP."); \
		CHUNK(F)->code.len--;                                              \
	} while (0)
// Pop last instruction long parameter (3 bytes)
#define LPARAM_POP(F)                                                               \
	do {                                                                        \
		cr_assert((F)->vm, CHUNK(F)->code.len >= 3, "Invalid LPARAM_POP."); \
		CHUNK(F)->code.len -= 3;                                            \
	} while (0)
// Pop last short/simple instruction (1 byte)
#define SINSTRUCTION_POP(F)                                                               \
	do {                                                                              \
		cr_assert((F)->vm, CHUNK(F)->code.len >= 1, "Invalid SINSTRUCTION_POP."); \
		CHUNK(F)->code.len--;                                                     \
	} while (0)
// Pop last instruction with parameter (2 bytes)
#define INSTRUCTION_POP(F)                                                               \
	do {                                                                             \
		cr_assert((F)->vm, CHUNK(F)->code.len >= 2, "Invalid INSTRUCTION_POP."); \
		CHUNK(F)->code.len -= 2;                                                 \
	} while (0)
// Pop last instruction with long parameter (4 bytes)
#define LINSTRUCTION_POP(F)                                                               \
	do {                                                                              \
		cr_assert((F)->vm, CHUNK(F)->code.len >= 4, "Invalid LINSTRUCTION_POP."); \
		CHUNK(F)->code.len -= 4;                                                  \
	} while (0)

// Pop last constant
#define CONSTANT_POP(F)                                                                   \
	do {                                                                              \
		cr_assert((F)->vm, CHUNK(F)->constants.len > 0, "Invalid CONSTANT_POP."); \
		Array_Value_pop(&CHUNK(F)->constants);                                    \
	} while (0)



/*
 * Returns 1 if 'S' is in loop, 0 if inside of a switch
 * and -1 if outside.
 */
static cr_inline int32_t inwhat(Scope *S, Scope **target)
{
	int8_t which = -1;
	Scope *head = S;
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




static cr_inline Scope *getscope(FunctionState *F, int32_t depth)
{
	Scope *scope = F->S;
	while (scope != NULL) {
		if (scope->depth == depth)
			return scope;
		scope = scope->prev;
	}
	return NULL;
}


/* default 'FunctionState' stack size */
#define SHORT_STACK_SIZE UINT8_MAX

/* tokens (previous and current) */
#define PREVT(F) (F)->lexer->previous
#define CURRT(F) (F)->lexer->current

typedef struct {
	int32_t codeoffset;
	int32_t constlen;
	int32_t localc;
	int32_t upvalc;
} Context;

static cr_inline void savecontext(FunctionState *F, Context *C)
{
	C->codeoffset = codeoffset(F);
	C->constlen = CHUNK(F)->constants.len;
	C->localc = F->locals.len;
	C->upvalc = F->upvalues->len;
}

// Trim/set length of code and/or constant array
static cr_inline void concatcode(FunctionState *F, int32_t codeoffset, int32_t constoffset)
{
	CHUNK(F)->bcode.len = codeoffset;
	CHUNK(F)->constants.len = constoffset;
}

static cr_inline void restorecontext(FunctionState *F, Context *C)
{
	concatcode(F, C->codeoffset, C->constlen);
	F->locals.len = C->localc;
	F->upvalues->len = C->upvalc;
}




// Forward declare
static void dec(FunctionState *F);
static void expr(FunctionState *F, Exp *E);
static void suffixedexp(FunctionState *F, Exp *E);
static void stm(FunctionState *F);




/*========================== ERROR =========================*/

// Compile-time error
static void error(FunctionState *F, const char *error, ...)
{
	Token *token = &PREVT(F);
	va_list args;
	cr_assert(vm, token->type != TOK_ERROR, "Only lexer can register error tokens.");
	// If panic bit is on, then sync the lexer before registering any new errors.
	if (F->lexer->panic)
		return;
	va_start(args, error);
	regcomperror(F->lexer, error, args);
	va_end(args);
}




//======================= CODE =======================//

// Multi-byte instruction (up to 4 bytes)
#define CODEOP(F, code, param) writechunk_codewparam(CHUNK(F), code, param, PREVT(F).line)

// Single byte instruction
#define CODE(F, byte)                                       \
	({                                                  \
		(F)->fn->gotret = ((byte) == OP_RET);       \
		writechunk(CHUNK(F), byte, PREVT(F).line); \
	})

// 3 byte parameter
#define CODEL(F, bytes)                  \
	do {                             \
		CODE(F, BYTE(bytes, 0)); \
		CODE(F, BYTE(bytes, 1)); \
		CODE(F, BYTE(bytes, 2)); \
	} while (0)

// Emit jump instruction
#define CODEJMP(F, jmp)            \
	({                         \
		CODEOP(F, jmp, 0); \
		codeoffset(F) - 3; \
	})

// Emit pop instruction
#define CODEPOP(F, n)                                  \
	do {                                           \
		if (n > 0) {                           \
			if ((n) > 1)                   \
				CODEOP(F, OP_POPN, n); \
			else                           \
				CODE(F, OP_POP);       \
		}                                      \
	} while (0)

// Emit unary instruction
#define CODEUN(F, opr) CODE(F, unopr2op(opr))

// Emit binary instruction
#define CODEBIN(F, opr) CODE(F, binopr2op(opr))

// Implicit return
#define implicitreturn(F) (CODE(F, OP_NIL), CODE(F, OP_RET1))

// Class initializer return
#define initreturn(F) (CODEOP(F, OP_GET_LOCAL, 0), CODE(F, OP_RET1))

// Emit loop instruction
static cr_inline void codeloop(FunctionState *F, int start)
{
	CODE(F, OP_LOOP);
	int offset = codeoffset(F) - start + 3;
	if (cr_unlikely(offset >= PARSER_JMP_LIMIT))
		error(F, comperrors[CE_JMPLIMIT], PARSER_JMP_LIMIT);
	CODEL(F, offset);
}

// Initialize global variable
#define INIT_GLOBAL(F, idx, vflags, E)                                 \
	do {                                                           \
		(F)->vm->globvars.data[idx].flags = vflags;            \
		CODEOP(F, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, E), idx); \
	} while (0)


/* Check if token names are equal */
#define nameeq(l, r) (((l)->len == (r)->len) && (memcmp((l)->start, (r)->start, (l)->len) == 0))


// Get local variable
static cr_inline int32_t get_local(FunctionState *F, Token *name)
{
	for (int32_t i = F->locals.len - 1; i >= 0; i--) {
		LocalVar *local = Array_Local_index(&F->locals, i);
		if (nameeq(name, &local->name)) {
			if (cr_unlikely(local->depth == -1))
				error(F, comperrors[CE_LREAD], name->len, name->start);
			return i;
		}
	}
	return -1;
}

static int add_upval(FunctionState *F, int idx, cr_ubyte flags, cr_ubyte local)
{
	int32_t upvalc = F->fn->p.upvalc;
	cr_assert(F->vm, upvalc < cast_int(F->upvalues->len + 1), "Invalid upvalc.");
	for (int32_t i = 0; i < upvalc; i++) {
		Upvalue *upvalue = Array_Upvalue_index(F->upvalues, i);
		if (upvalue->idx == idx && upvalue->local == local)
			return i; // already exists
	}
	if (cr_unlikely(upvalc >= VM_STACK_LIMIT)) {
		error(F, comperrors[CE_UVALLIMIT], VM_STACK_LIMIT, F->fn->p.name->storage);
		return 0;
	}
	Upvalue upval = { idx, flags, local };
	if (upvalc == cast_int(F->upvalues->len))
		Array_Upvalue_push(F->upvalues, upval);
	else
		*Array_Upvalue_index(F->upvalues, upvalc) = upval;
	return F->fn->p.upvalc++;
}

static int32_t get_upval(FunctionState *F, Token *name)
{
	if (F->enclosing == NULL)
		return -1;
	int32_t idx = get_local(F->enclosing, name);
	if (idx != -1) {
		LocalVar *l = Array_Local_index(&F->enclosing->locals, idx);
		bset(l->flags, VCAPTURED_BIT);
		return add_upval(F, cast_uint(idx), l->flags, 1);
	}
	idx = get_upval(F->enclosing, name);
	if (idx != -1) {
		LocalVar *l = Array_Local_index(&F->enclosing->locals, idx);
		return add_upval(F, cast_uint(idx), l->flags, 0);
	}
	return -1;
}

static int globalvar(FunctionState *F, Value identifier)
{
	Value idx;
	Variable glob = { undefval(), F->vflags };
	VM *vm = F->vm;
	if (!rawget(vm, &vm->globids, identifier, &idx)) {
		if (cr_unlikely((vm)->globvars.len + 1 > PARSER_GVAR_LIMIT))
			error(F, comperrors[CE_GVARLIMIT], PARSER_GVAR_LIMIT);
		push(vm, identifier);
		idx = NUMBER_VAL(Array_Variable_push(&vm->globvars, glob));
		rawset(vm, &vm->globids, identifier, idx);
		pop(vm);
	}
	return cast_uint(AS_NUMBER(idx));
}

#define tokintostr(vm, name) OBJ_VAL(OString_new(vm, (name)->start, (name)->len))

// Make global variable
#define MAKE_GLOBAL(F, name)                                  \
	({                                                    \
		Value identifier = tokintostr((F)->vm, name); \
		globalvar(F, identifier);                     \
	})

static int32_t codevar(FunctionState *F, Token name, Exp *E)
{
	OpCode getop;
	int32_t idx = get_local(F, &name);
	if (idx != -1) {
		E->type = EXP_LOCAL;
		getop = GET_OP_TYPE(idx, OP_GET_LOCAL, E);
	} else if ((idx = get_upval(F, &name)) != -1) {
		E->type = EXP_UPVAL;
		E->ins.l = 1;
		getop = OP_GET_UPVALUE;
	} else {
		E->type = EXP_GLOBAL;
		idx = MAKE_GLOBAL(F, &name);
		getop = GET_OP_TYPE(idx, OP_GET_GLOBAL, E);
	}
	E->value = idx;
	if (!E->ins.set)
		return (E->ins.code = CODEOP(F, getop, idx));
	else
		return (E->ins.code = -1); // this is assignment
}

static cr_inline int codevarprev(FunctionState *F, Exp *E)
{
	return codevar(F, PREVT(F), E);
}

// helper [rmlastins]
static cr_inline void popvarins(FunctionState *F, Exp *E)
{
	switch (E->type) {
	case EXP_UPVAL:
	case EXP_LOCAL:
	case EXP_GLOBAL:
		if (E->ins.l)
			LINSTRUCTION_POP(F);
		else
			INSTRUCTION_POP(F);
		break;
	case EXP_INDEXED:
		// @?: setters are not reachable ?
		switch (*INSTRUCTION(F, E)) {
		case OP_INDEX:
			SINSTRUCTION_POP(F);
			break;
		case OP_GET_PROPERTY:
		case OP_GET_SUPER:
			LINSTRUCTION_POP(F);
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
static cr_inline void popcallins(FunctionState *F, Exp *E)
{
	switch (E->type) {
	case EXP_CALL:
		LINSTRUCTION_POP(F);
		break;
	case EXP_INVOKE:
		LINSTRUCTION_POP(F);
		LPARAM_POP(F);
		break;
	default:
		cr_unreachable;
	}
}

static void rmlastins(FunctionState *F, Exp *E)
{
	ExpType type = E->type;
	if (etisliteral(type))
		SINSTRUCTION_POP(F);
	else if (etisconst(type))
		LINSTRUCTION_POP(F);
	else if (etisvar(type))
		popvarins(F, E);
	else if (etiscall(type))
		popcallins(F, E);
	else
		switch (type) {
		case EXP_JMP:
			goto panic;
		case EXP_EXPR:
			if (E->ins.binop) {
				LINSTRUCTION_POP(F);
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
static cr_inline void startscope(FunctionState *F, Scope *S, cr_ubyte isloop, cr_ubyte isswitch)
{
	if (cr_unlikely(F->S->depth >= CR_BYTECODE_MAX))
		error(F, comperrors[CE_SCOPE], CR_BYTECODE_MAX);
	S->localc = F->locals.len;
	S->isloop = isloop;
	S->isswitch = isswitch;
	S->depth = F->S->depth + 1;
	S->prev = F->S;
	F->S = S;
}

// End scope and pop locals and/or close captured locals
static void endscope(FunctionState *F)
{
#define LOCAL_IS_CAPTURED(local) (btest((local)->flags, VCAPTURED_BIT))

	F->fn->gotret = 0;
	int32_t pop = 0;
	Scope *current = F->S;
	F->S = current->prev;
	while (F->locals.len > 0 && Array_Local_last(&F->locals)->depth > F->S->depth) {
		if (LOCAL_IS_CAPTURED(Array_Local_last(&F->locals))) {
			int32_t capture = 1;
			F->locals.len--;
			CODEPOP(F, pop);
			pop = 0; // Reset pop count
			do {
				if (!LOCAL_IS_CAPTURED(Array_Local_last(&F->locals))) {
					if (capture == 1)
						CODE(F, OP_CLOSE_UPVAL);
					else
						CODEOP(F, OP_CLOSE_UPVALN, capture);
					break;
				}
				capture++;
				F->locals.len--;
			} while (F->locals.len > 0 && Array_Local_last(&F->locals)->depth > F->S->depth);
		} else {
			pop++;
			F->locals.len--;
		}
	}
	CODEPOP(F, pop);

#undef LOCAL_IS_CAPTURED
}


static void ControlFlow_init(VM *vm, ControlFlow *cflow)
{
	cflow->innerlstart = -1;
	cflow->innerldepth = 0;
	cflow->innersdepth = 0;
	Array_Array_cr_int_init(&cflow->breaks, vm);
}

static void ControlFlow_free(ControlFlow *context)
{
	Array_Array_cr_int_free(&context->breaks, NULL);
	context->innerlstart = -1;
	context->innerldepth = 0;
	context->innersdepth = 0;
}




/*========================== FUNCTION STATE =========================*/

static void F_init(VM *vm, FunctionState *F, Scope *globscope, Class *cclass, FunctionType fn_type, FunctionState *enclosing)
{
	// Initialize FunctionState state
	F->vm = vm;
	F->S = globscope;
	F->enclosing = enclosing;
	F->cclass = cclass;
	vm->F = F;
	F->fn = NULL; // Initialize to NULL so gc does not get confused
	F->fn = OFunction_new(vm);
	F->fn_type = fn_type;
	F->vflags = 0;
	ControlFlow_init(vm, &F->cflow);
	// Setup upvalues storage
	if (enclosing == NULL) {
		F->tag = -1;
		F->upvalues = MALLOC(vm, sizeof(Array_Upvalue));
		Array_Upvalue_init(F->upvalues, vm);
	} else {
		F->tag = enclosing->tag;
		F->upvalues = enclosing->upvalues;
	}
	// Setup local variables storage
	Array_Local_init(&F->locals, vm);
	Array_Local_init_cap(&F->locals, SHORT_STACK_SIZE);
	F->locals.len++;
	LocalVar *local = F->locals.data;
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
		F->fn->p.defline = PREVT(F).line;
		F->fn->p.name = OString_new(vm, PREVT(F).start, PREVT(F).len);
	}
}

void F_free(FunctionState *F)
{
	VM *vm = F->vm;
	ControlFlow_free(&F->cflow);
	if (F->enclosing == NULL) {
		cr_assert(F->vm, F->fn_type == FN_SCRIPT, "FunctionState is top-level but the type is not 'FN_SCRIPT'.");
		Array_Upvalue_free(F->upvalues, NULL);
		FREE(vm, F->upvalues); // free the memory holding the pointer
	}
	Array_Local_free(&F->locals, NULL);
	vm->F = F->enclosing;
	FREE(vm, F);
}

// Cleanup the function stack in case of internal errors
void _cleanup_function(FunctionState *F)
{
	if (F != NULL) {
		L_free(F->lexer);
		for (FunctionState *fn = F; fn != NULL; fn = fn->enclosing)
			F_free(F);
	}
}

void mark_function_roots(VM *vm)
{
	// Mark all functions and token values
	for (FunctionState *current = vm->F; current != NULL; current = current->enclosing) {
		vmark(vm, PREVT(current).value);
		vmark(vm, CURRT(current).value);
		omark(vm, (GCObject *)current->fn);
	}
}




/*========================== PARSING =========================*/

// Advance to the next token
static void advance(FunctionState *F)
{
	PREVT(F) = CURRT(F);
	while (1) {
		CURRT(F) = scan(F->lexer);
		if (CURRT(F).type != TOK_ERROR)
			break;
	}
}

// Advance and return 1 if 'type' matches the current token type
static cr_inline cr_ubyte match(FunctionState *F, TType type)
{
	if (CURRT(F).type != type)
		return 0;
	advance(F);
	return 1;
}

// Sync lexer to the next statement
static void sync(FunctionState *F)
{
	F->lexer->panic = 0;
	while (CURRT(F).type != TOK_EOF) {
		if (PREVT(F).type == TOK_SEMICOLON)
			return;
		switch (CURRT(F).type) {
		case TOK_FOR:
		case TOK_FN:
		case TOK_VAR:
		case TOK_CLASS:
		case TOK_IF:
		case TOK_RETURN:
		case TOK_CONTINUE:
		case TOK_BREAK:
		case TOK_WHILE:
		case TOK_FOREACH:
		case TOK_LOOP:
		case TOK_SWITCH:
			return;
		default:
			advance(F);
			break;
		}
	}
}

// Check if the token type matches the current token type
#define check(F, toktype) (CURRT(F).type == (toktype))

// Invoke compile-time error if 'cond' is 0
#define expect_cond(F, cond, err) \
	if (!cond)                \
		error(F, err);

// Invoke compile-time error if 'type' does not match the current token type
static cr_inline void expect(FunctionState *F, TType type, const char *err)
{
	if (CURRT(F).type == type) {
		advance(F);
		return;
	}
	error(F, err);
}

// String literal as argument to 'expect()'
#define expectstr(str) "\t" str "\n"

// End compilation of the function and emit return instruction
static cr_inline Function *compile_end(FunctionState *F)
{
	if (!F->fn->gotret) { // last statement wasn't 'return' ?
		if (isom(F) && tagisnoret(F->tag)) { // om that can't return values with 'return' ?
			if (F->tag == SS_INIT) { // return 'self'
				CODEOP(F, OP_GET_LOCAL, 0);
				CODE(F, OP_RET1);
			} else
				CODE(F, OP_RET0); // no return
		} else
			implicitreturn(F); // otherwise implicit 'nil' return
	}
	F->fn->p.deflastline = PREVT(F).line;
#ifdef CR_DEBUG_PRINT_CODE
	if (!F->lexer->error)
		Chunk_debug(F->vm, CHUNK(F), F->fn->name->storage);
#endif
	return F->fn;
}


// Compiles source code, 'isingscope' determines if the script getting
// compiled is in global or local scope.
// Additionally parser only registers compile-time errors but it does not
// call the usual 'runerror' [@err.h], this ensures that all of the
// compile-time errors will be reported (exhaustive parser).
CriptClosure *compile(VM *vm, BuffReader *br, const char *name, cr_ubyte isingscope)
{
	Scope globalscope, local;
	FunctionState F;
	Lexer L;
	if (name == NULL)
		name = "?";
	F_init(vm, &F, &globalscope, NULL, FN_SCRIPT, vm->F);
	F.fn->p.source = OString_new(vm, name, strlen(name));
	L_init(&L, vm, br, F.fn->p.source);
	startscope(&F, &globalscope, 0, 0);
	if (!isingscope) // script not in global scope?
		startscope(&F, &local, 0, 0);
	advance(&F); // fetch first token
	while (!match(&F, TOK_EOF))
		dec(&F);
	Function *fn = compile_end(&F);
	cr_ubyte comperr = F.lexer->error;
	L_free(&L);
	F_free(&F);
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
static cr_inline void local_new(FunctionState *F, Token name)
{
	if (cr_unlikely(cast_int(F->locals.len) >= PARSER_LVAR_LIMIT)) {
		error(F, comperrors[CE_LVARLIMIT], PARSER_LVAR_LIMIT);
		return;
	}
	Array_Local_push(&F->locals, (LocalVar){ name, -1, F->vflags });
}

// Make local variable but check for redefinitions in local scope
static void make_local(FunctionState *F, Token *name)
{
	for (int32_t i = F->locals.len - 1; i >= 0; i--) {
		LocalVar *local = Array_Local_index(&F->locals, i);
		if (local->depth != -1 && local->depth < F->S->depth)
			break;
		if (cr_unlikely(nameeq(name, &local->name)))
			error(F, comperrors[CE_LREDEF], name->len, name->start);
	}
	local_new(F, *name);
}

// Patch jump instruction
static cr_inline void patchjmp(FunctionState *F, int32_t jmp_offset)
{
	int32_t offset = codeoffset(F) - jmp_offset - 3;
	if (cr_unlikely(offset >= PARSER_JMP_LIMIT))
		error(F, comperrors[CE_JMPLIMIT], PARSER_JMP_LIMIT);
	PUT_BYTES3(&CHUNK(F)->bcode.data[jmp_offset], offset);
}

static cr_inline void startbreaklist(FunctionState *F)
{
	Array_cr_int patches;
	Array_cr_int_init(&patches, F->vm);
	Array_Array_cr_int_push(&F->cflow.breaks, patches);
}

static cr_inline void patchbreaklist(FunctionState *F)
{
	Array_cr_int *patches = Array_Array_cr_int_last(&F->cflow.breaks);
	for (int32_t i = 0; i < cast_int(patches->len); i++)
		patchjmp(F, patches->data[i]);
}

static cr_inline void endbreaklist(FunctionState *F)
{
	Array_cr_int last = Array_Array_cr_int_pop(&F->cflow.breaks);
	Array_cr_int_free(&last, NULL);
}


static cr_inline int make_constant(FunctionState *F, Value constant)
{
	if (cr_unlikely(cast_int(CHUNK(F)->constants.len) > PARSER_CONST_LIMIT))
		error(F, "Too many constants created in this chunk, limit is '%d'.\n", PARSER_CONST_LIMIT);
	return writechunk_constant(F->vm, CHUNK(F), constant);
}




/* =================== protected 'compile' =================== */

// Data for 'protectedcompile'
typedef struct {
	BuffReader *br;
	const char *name;
	cr_ubyte isingscope;
} PCompile;

// Wrapper around 'compile' to match 'ProtectedFn' signature
static void protectedcompile(VM *vm, void *userdata)
{
	PCompile *pcompile = cast(PCompile *, userdata);
	if (cr_unlikely(compile(vm, pcompile->br, pcompile->name, pcompile->isingscope) == NULL))
		runerror(vm, S_ECOMP);
}

// External interface for 'protectedcompile'.
// Runs compiler in protected mode and returns runtime 'status'.
cr_ubyte pcompile(VM *vm, void *userdata, const char *name, cr_ubyte isingscope)
{
	BuffReader br;
	BuffReader_init(vm, &br, (vm)->hooks.reader, userdata);
	PCompile pcompile = { &br, name, isingscope };
	return pcall(vm, protectedcompile, &pcompile, save_stack(vm, vm->sp));
}

/* -------------------------------------------------------- */ // protected 'compile'




/*========================== OPERATIONS/PRIORITY =========================*/

typedef enum {
	OPR_NOT = 0,
	OPR_NEGATE,
	OPR_NOUNARYOPR,
} UnaryOpr;

typedef enum {
	OPR_ADD = 0,
	OPR_SUB,
	OPR_MUL,
	OPR_DIV,
	OPR_MOD,
	OPR_POW,
	OPR_NE,
	OPR_EQ,
	OPR_LT,
	OPR_LE,
	OPR_GT,
	OPR_GE,
	OPR_AND,
	OPR_OR,
	OPR_NOBINOPR,
} BinaryOpr;

#define FOLDABLE(opr) ((opr) <= OPR_POW)

// Fetch unary operation
static UnaryOpr getunaryopr(TType type)
{
	switch (type) {
	case TOK_BANG:
		return OPR_NOT;
	case TOK_MINUS:
		return OPR_NEGATE;
	default:
		return OPR_NOUNARYOPR;
	}
}

// Fetch binary operation
static BinaryOpr getbinaryopr(TType type)
{
	switch (type) {
	case TOK_PLUS:
		return OPR_ADD;
	case TOK_MINUS:
		return OPR_SUB;
	case TOK_STAR:
		return OPR_MUL;
	case TOK_SLASH:
		return OPR_DIV;
	case TOK_PERCENT:
		return OPR_MOD;
	case TOK_CARET:
		return OPR_POW;
	case TOK_BANG_EQUAL:
		return OPR_NE;
	case TOK_EQUAL_EQUAL:
		return OPR_EQ;
	case TOK_LESS:
		return OPR_LT;
	case TOK_LESS_EQUAL:
		return OPR_LE;
	case TOK_GREATER:
		return OPR_GT;
	case TOK_GREATER_EQUAL:
		return OPR_GE;
	case TOK_AND:
		return OPR_AND;
	case TOK_OR:
		return OPR_OR;
	default:
		return OPR_NOBINOPR;
	}
}

// Fetch unary instruction
static OpCode unopr2op(UnaryOpr opr)
{
	switch (opr) {
	case OPR_NOT:
		return OP_NOT;
	case OPR_NEGATE:
		return OP_NEG;
	default:
		cr_unreachable;
	}
}

// Fetch binary instruction
static OpCode binopr2op(BinaryOpr opr)
{
	switch (opr) {
	case OPR_ADD:
		return OP_ADD;
	case OPR_SUB:
		return OP_SUB;
	case OPR_MUL:
		return OP_MUL;
	case OPR_DIV:
		return OP_DIV;
	case OPR_MOD:
		return OP_MOD;
	case OPR_POW:
		return OP_POW;
	case OPR_NE:
		return OP_NOT_EQUAL;
	case OPR_EQ:
		return OP_EQUAL;
	case OPR_LT:
		return OP_LESS;
	case OPR_LE:
		return OP_LESS_EQUAL;
	case OPR_GT:
		return OP_GREATER;
	case OPR_GE:
		return OP_GREATER_EQUAL;
	default:
		/// OPR_AND and OPR_OR do not emit direct bytecode,
		/// they are instead sets of OP_JMP and OP_JMP_IF_FALSE instructions
		cr_unreachable;
	}
}

static const struct {
	cr_ubyte left; // Left priority
	cr_ubyte right; // Right priority
} priority[] = {
	[OPR_ADD] = { 4, 4 }, /* '+' */
	[OPR_SUB] = { 4, 4 }, /* '-' */
	[OPR_MUL] = { 5, 5 }, /* '*' */
	[OPR_DIV] = { 5, 5 }, /* '/' */
	[OPR_MOD] = { 5, 5 }, /* '%' */
	[OPR_POW] = { 8, 7 }, /* '^' (right associative) */
	[OPR_NE] = { 3, 3 }, /* '!=' */
	[OPR_EQ] = { 3, 3 }, /* '==' */
	[OPR_LT] = { 3, 3 }, /* '<' */
	[OPR_LE] = { 3, 3 }, /* '<=' */
	[OPR_GT] = { 3, 3 }, /* '>' */
	[OPR_GE] = { 3, 3 }, /* '>=' */
	[OPR_AND] = { 2, 2 }, /* 'and' */
	[OPR_OR] = { 1, 1 }, /* 'or' */
};

#define UNARY_PRIORITY 6




/*========================== STATEMENT/DECLARATION =========================*/

// vararg ::= '...'
static cr_inline int vararg(FunctionState *F)
{
	if (!F->fn->p.isvararg)
		error(F, comperrors[CE_VARARG]);
	return CODEOP(F, OP_VARARG, 1);
}

static void setmulret(FunctionState *F, Exp *E)
{
	switch (E->type) {
	case EXP_CALL:
	case EXP_VARARG:
		SET_RETCNT(F, E, MULRET);
		break;
	case EXP_INVOKE:
		SET_RETCNTL(F, E, MULRET);
		break;
	default:
		cr_unreachable;
	}
}

// Adjust assign expressions in case last expression is a function call
static void adjustassign(FunctionState *F, Exp *E, int32_t left, int32_t right)
{
	int32_t leftover = left - right; // Safety: left < right is a compile error
	switch (E->type) {
	case EXP_CALL:
		SET_RETCNT(F, E, leftover + 1);
		break;
	case EXP_INVOKE:
		SET_RETCNTL(F, E, leftover + 1);
		break;
	default:
		if (leftover > 1)
			CODEOP(F, OP_NILN, leftover);
		else if (leftover == 1)
			CODE(F, OP_NIL);
		break;
	}
}

// explist ::= expr
//           | expr ',' explist
static int32_t explist(FunctionState *F, int32_t limit, Exp *E)
{
	int32_t left = limit;
	int32_t got = 0;
	do {
		left--;
		got++;
		if (left < 0)
			error(F, comperrors[CE_EXPLIST], limit);
		expr(F, E);
	} while (match(F, TOK_COMMA));
	return got;
}

static int32_t name(FunctionState *F, const char *errmsg)
{
	expect(F, TOK_IDENTIFIER, errmsg);
	Token *name = &PREVT(F);
	if (F->S->depth > 0) { // If local scope make local variable
		make_local(F, name);
		return -1;
	} // Otherwise make global variable
	return MAKE_GLOBAL(F, name);
}

static LocalVar *upvalvar(FunctionState *F, int32_t idx)
{
	Upvalue *upval = &F->upvalues->data[idx];
	if (!upval->local)
		upvalvar(F->enclosing, idx);
	return &F->locals.data[upval->idx];
}

// helper [exprstm]
static void codeset(FunctionState *F, Exp *E)
{
	static const char *errfmt = "Can't assign to variable '%.*s', it is declared as 'fixed'.";
	switch (E->type) {
	case EXP_UPVAL: {
		LocalVar *local = upvalvar(F, E->value);
		if (cr_unlikely(btest(local->flags, VFIXED_BIT)))
			error(F, errfmt, local->name.len, local->name.start);
		CODEOP(F, OP_SET_UPVALUE, E->value);
		break;
	}
	case EXP_LOCAL: {
		LocalVar *local = &F->locals.data[E->value];
		if (cr_unlikely(btest(local->flags, VFIXED_BIT)))
			error(F, errfmt, local->name.len, local->name.start);
		CODEOP(F, GET_OP_TYPE(E->value, OP_SET_LOCAL, E), E->value);
		break;
	}
	case EXP_GLOBAL:
		CODEOP(F, GET_OP_TYPE(E->value, OP_SET_GLOBAL, E), E->value);
		break;
	case EXP_INDEXED:
		if (E->value == NO_VAL)
			CODE(F, OP_SET_INDEX);
		else
			CODEOP(F, OP_SET_PROPERTY, E->value);
		break;
	default:
		return;
	}
}

ARRAY_NEW(Array_Exp, Exp);
// helper [exprstm]
static void codesetall(FunctionState *F, Array_Exp *Earr)
{
	for (int32_t i = 0; i < cast_int(Earr->len); i++) {
		Exp *E = Array_Exp_index(Earr, i);
		codeset(F, E);
	}
}

/// exprstm ::= functioncall
///           | varlist '=' explist
static void exprstm(FunctionState *F, cr_ubyte lastclause)
{
	Exp E;
	E.ins.set = 0;
	suffixedexp(F, &E);
	TType next = CURRT(F).type;
	if (next == TOK_EQUAL || next == TOK_COMMA) {
		E.ins.set = 1;
		Array_Exp Earr;
		Array_Exp_init(&Earr, F->vm);
		expect_cond(F, etisvar(E.type), expectstr("Expect variable."));
		rmlastins(F, &E); // remove 'OP_GET..'
		Array_Exp_push(&Earr, E);
		int32_t vars = 1;
		while (match(F, TOK_COMMA)) {
			if (cr_unlikely(vars >= CR_BYTECODE_MAX))
				error(F, comperrors[CE_VARLIST], CR_BYTECODE_MAX);
			vars++;
			suffixedexp(F, &E);
			expect_cond(F, etisvar(E.type), expectstr("Expect variable."));
			Array_Exp_push(&Earr, E);
		}
		expect(F, TOK_EQUAL, expectstr("Expect '='."));
		E.ins.set = 0;
		int32_t expc = explist(F, vars, &E);
		if (vars != expc)
			adjustassign(F, &E, vars, expc);
		codesetall(F, &Earr);
		Array_Exp_free(&Earr, NULL);
	} else if (etiscall(E.type))
		CODE(F, OP_POP);
	else
		error(F, comperrors[CE_EXPSTM]);
	if (!lastclause)
		expect(F, TOK_SEMICOLON, expectstr("Expect ';'."));
}

static void block(FunctionState *F)
{
	while (!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
		dec(F);
	expect(F, TOK_RBRACE, expectstr("Expect '}' after block."));
}

static cr_inline void blockstm(FunctionState *F)
{
	Scope S;
	startscope(F, &S, 0, 0);
	block(F);
	endscope(F);
}

// arglist ::= name
//           | '...'
//           | name ',' arglist
static void arglist(FunctionState *F)
{
	do {
		if (match(F, TOK_DOT_DOT_DOT)) {
			F->fn->p.isvararg = 1;
			break;
		}
		F->fn->p.arity++;
		name(F, expectstr("Expect parameter name."));
		initlocal(F, 0);
	} while (match(F, TOK_COMMA));
}

// namelist ::= name
//            | name ',' namelist
static int namelist(FunctionState *F, Array_cr_int *nameidx)
{
	int32_t names = 0;
	do {
		if (names >= CR_BYTECODE_MAX)
			error(F, comperrors[CE_NAMELIST], CR_BYTECODE_MAX);
		names++;
		int32_t idx = name(F, expectstr("Expect name.")); // initialize later
		if (F->S->depth == 0)
			Array_cr_int_push(nameidx, idx);
	} while (match(F, TOK_COMMA));
	return names;
}

static void codeassign(FunctionState *F, int32_t names, Array_cr_int *nameidx)
{
	if (F->S->depth > 0) {
		for (int32_t i = 0; i < names; i++)
			initlocal(F, i);
		return;
	}
	cr_assert(F->vm, names == cast_int(nameidx->len), "name count != indexes array len.");
	while (nameidx->len > 0) {
		int32_t idx = Array_cr_int_pop(nameidx);
		Exp _; // dummy
		INIT_GLOBAL(F, idx, F->vflags, &_);
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
static void vardec(FunctionState *F)
{
	if (match(F, TOK_FIXED)) {
		if (FIS(F, FFIXED))
			error(F, expectstr("Expect variable name."));
		FSET(F, FFIXED);
	}
	Array_cr_int nameidx;
	Array_cr_int_init(&nameidx, F->vm);
	int32_t names = namelist(F, &nameidx);
	int32_t expc = 0;
	Exp E;
	E.ins.set = 0;
	if (match(F, TOK_EQUAL))
		expc = explist(F, names, &E);
	if (names != expc)
		adjustassign(F, &E, names, expc);
	codeassign(F, names, &nameidx);
	Array_cr_int_free(&nameidx, NULL);
	expect(F, TOK_SEMICOLON, expectstr("Expect ';'."));
}

// fvardec ::= 'fixed' vardec
static cr_inline void fvardec(FunctionState *F)
{
	FSET(F, FFIXED);
	advance(F);
	vardec(F);
}


// Create and parse a new FunctionState
static void fn(FunctionState *F, FunctionType type)
{
	FunctionState Fnew;
	Scope globscope, S;
	F_init(F->vm, &Fnew, &globscope, F->cclass, type, F);
	Fnew.fn->p.source = F->fn->p.source; // source is same
	startscope(&Fnew, &S, 0, 0); // no need to end this scope
	expect(&Fnew, TOK_LPAREN, expectstr("Expect '(' after function name."));
	if (!check(&Fnew, TOK_RPAREN))
		arglist(&Fnew);
	if (Fnew.fn->p.isvararg)
		expect(&Fnew, TOK_RPAREN, expectstr("Expect ')' after '...'."));
	else
		expect(&Fnew, TOK_RPAREN, expectstr("Expect ')' after parameters."));
	int32_t arity = Fnew.fn->p.arity;
	int32_t expected = ominfo[F->tag].arity;
	if (cr_unlikely(isom(F) && expected != arity)) {
		error(F, comperrors[CE_OMSIG], F->vm->faststatic[F->tag]->storage, expected, arity);
		Fnew.lexer->panic = 0; // clear panic flag do not sync yet
	}
	expect(&Fnew, TOK_LBRACE, expectstr("Expect '{' before function body."));
	block(&Fnew); // body
	Function *fn = compile_end(&Fnew);
	CODEOP(F, OP_CLOSURE, make_constant(F, OBJ_VAL(fn)));
	for (int i = 0; i < fn->p.upvalc; i++) {
		Upvalue *upval = Array_Upvalue_index(Fnew.upvalues, i);
		CODE(F, upval->local ? 1 : 0);
		CODEL(F, upval->idx);
	}
	F_free(&Fnew);
}

// fndec ::= 'fn' name '(' arglist ')' '{' block '}'
static void fndec(FunctionState *F)
{
	int idx = name(F, expectstr("Expect function name."));
	if (F->S->depth > 0)
		initlocal(F, 0); // initialize to allow recursion
	fn(F, FN_FUNCTION);
	Exp _; // dummy
	if (F->S->depth == 0)
		INIT_GLOBAL(F, idx, 0, &_);
}

static void method(FunctionState *F)
{
	Value identifier;
	int idx;

	expect(F, TOK_FN, expectstr("Expect 'fn'."));
	expect(F, TOK_IDENTIFIER, expectstr("Expect method name."));
	identifier = tokintostr(F->vm, &PREVT(F));
	idx = make_constant(F, identifier);
	F->tag = id2omtag(F->vm, asstring(identifier));
	fn(F, FN_METHOD);
	if (F->tag != -1)
		CODEOP(F, OP_OVERLOAD, F->tag - SS_INIT);
	else
		CODEOP(F, OP_METHOD, idx);
}

static void classdec(FunctionState *F)
{
	expect(F, TOK_IDENTIFIER, expectstr("Expect class name."));
	Token class_name = PREVT(F);
	Value identifier = tokintostr(F->vm, &class_name);
	int idx = make_constant(F, identifier);
	Exp _; // dummy
	CODEOP(F, OP_CLASS, idx);
	if (F->S->depth > 0) {
		make_local(F, &class_name);
		initlocal(F, 0);
	} else
		INIT_GLOBAL(F, MAKE_GLOBAL(F, &class_name), 0, &_);
	Class cclass;
	cclass.enclosing = F->cclass;
	cclass.superclass = 0;
	F->cclass = &cclass;
	_.ins.set = 0;
	Scope S;
	if (match(F, TOK_IMPL)) { // have superclass ?
		expect(F, TOK_IDENTIFIER, expectstr("Expect superclass name."));
		codevarprev(F, &_); // get superclass
		if (nameeq(&PREVT(F), &class_name))
			error(F, comperrors[CE_INHERIT], ascstring(identifier));
		startscope(F, &S, 0, 0);
		local_new(F, syntoken("super"));
		initlocal(F, 0);
		codevar(F, class_name, &_);
		CODE(F, OP_INHERIT);
		cclass.superclass = 1;
	}
	codevar(F, class_name, &_);
	expect(F, TOK_LBRACE, expectstr("Expect '{' before class body."));
	while (!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
		method(F);
	expect(F, TOK_RBRACE, expectstr("Expect '}' after class body."));
	CODE(F, OP_POP); // Pop the class
	if (cclass.superclass)
		endscope(F);
	F->cclass = cclass.enclosing;
}

/// call ::= '(' ')'
///        | '(' explist ')'
static OpCode call(FunctionState *F, Exp *E)
{
	OpCode op;
	if (!check(F, TOK_RPAREN)) {
		int32_t index = CODE(F, OP_CALLSTART);
		int32_t argc = explist(F, PARSER_ARG_LIMIT, E);
		if (argc == 1 && !ethasmulret(E->type)) {
			Array_ubyte_remove(&CHUNK(F)->bcode, index); // remove 'OP_CALLSTART'
			op = OP_CALL1;
		} else {
			setmulret(F, E);
			op = OP_CALL;
		}
	} else {
		E->type = EXP_NONE;
		op = OP_CALL0;
	}
	expect(F, TOK_RPAREN, expectstr("Expect ')'."));
	return op;
}

static void codecall(FunctionState *F, Exp *E)
{
	OpCode callop = call(F, E);
	E->type = EXP_CALL;
	if (callop == OP_CALL0 || callop == OP_CALL1)
		E->ins.code = CODE(F, callop);
	else
		E->ins.code = CODEOP(F, callop, 1);
}

static void codeinvoke(FunctionState *F, Exp *E, int32_t idx)
{
	OpCode callop = call(F, E);
	E->type = EXP_INVOKE;
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
	E->ins.code = CODEOP(F, invokeop, idx);
	CODEL(F, 1); // retcnt
}

static void dec(FunctionState *F)
{
	F->vflags = 0;
	if (match(F, TOK_VAR))
		vardec(F);
	else if (match(F, TOK_FIXED))
		fvardec(F);
	else if (match(F, TOK_FN))
		fndec(F);
	else if (match(F, TOK_CLASS))
		classdec(F);
	else
		stm(F);
	if (F->lexer->panic)
		sync(F);
}


// 'switch' statement state.
typedef struct {
	int32_t patch; // Jump to patch if case expression does not match
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

static cr_inline void SwitchState_init(FunctionState *F, SwitchState *state)
{
	state->patch = -1;
	state->casestate = CS_NONE;
	state->dflt = 0;
	state->havenil = 0;
	state->havetrue = 0;
	state->havefalse = 0;
	Array_Value_init(&state->constants, F->vm);
}

#define SwitchState_free(state) Array_Value_free(&(state)->constants, NULL);

/*
 * Updates 'switch' constants and checks if the constant
 * already exists, but only if 'e' is constant expression.
 * TODO: Refactor a bit, logic can be compressed
 */
static cr_inline void switchconstants(FunctionState *F, SwitchState *state, Exp *E)
{
	if (!etisconst(E->type))
		return;
	switch (E->type) {
	case EXP_FALSE:
		if (cr_unlikely(state->havefalse))
			error(F, comperrors[CE_SWDUP], "0");
		state->havefalse = 1;
		break;
	case EXP_TRUE:
		if (cr_unlikely(state->havetrue))
			error(F, comperrors[CE_SWDUP], "1");
		state->havetrue = 1;
		break;
	case EXP_NIL:
		if (cr_unlikely(state->havenil))
			error(F, comperrors[CE_SWDUP], "nil");
		state->havenil = 1;
		break;
	case EXP_STRING:
	case EXP_NUMBER:;
		Value caseval = *CONSTANT(F, E);
		cr_ubyte _; // dummy
		for (int32_t i = 0; i < cast_int(state->constants.len); i++) {
			if (cr_unlikely(raweq(state->constants.data[i], caseval))) {
				const char *casename = NULL;
				if (E->type == EXP_STRING)
					casename = vtostr(F->vm, caseval, 1)->storage;
				else
					casename = dtostr(AS_NUMBER(caseval), &_);
				error(F, comperrors[CE_SWDUP], casename);
				return;
			}
		}
		Array_Value_push(&state->constants, caseval);
		break;
	default:
		cr_unreachable;
	}
}

static void switchstm(FunctionState *F)
{
	Context C;
	Scope S;
	SwitchState swstate;
	cr_ubyte inswitch = F->S->isswitch;
	Exp E1;
	Array_cr_int fts;
	int32_t sdepth;
	savecontext(F, &C);
	SwitchState_init(F, &swstate);
	Array_cr_int_init(&fts, F->vm);
	startscope(F, &S, 0, 1); // implicit scope
	startbreaklist(F);
	expect(F, TOK_LPAREN, expectstr("Expect '(' after 'switch'."));
	E1.ins.set = 0;
	expr(F, &E1);
	expect(F, TOK_RPAREN, expectstr("Expect ')' after condition."));
	expect(F, TOK_LBRACE, expectstr("Expect '{' after ')'."));
	sdepth = F->cflow.innersdepth;
	F->cflow.innersdepth = F->S->depth;
	// Switch must contain case or default before any statements
	if (!check(F, TOK_RBRACE) && !check(F, TOK_EOF) && !check(F, TOK_CASE) && !check(F, TOK_DEFAULT))
		error(F, comperrors[CE_SWNOC]);
	// 'switch' body
	while (!match(F, TOK_RBRACE) && !check(F, TOK_EOF)) {
		if (match(F, TOK_CASE) || match(F, TOK_DEFAULT)) {
			F->fn->gotret = 0;
			if (swstate.casestate != CS_NONE) {
				Array_cr_int_push(&fts, CODEJMP(F, OP_JMP));
				if (swstate.casestate != CS_DFLT && swstate.casestate != CS_MATCH)
					patchjmp(F, swstate.patch);
			}
			swstate.casestate = CS_DFLT;
			if (PREVT(F).type == TOK_CASE) {
				Exp E2;
				E2.ins.set = 0;
				expr(F, &E2);
				expect(F, TOK_COLON, expectstr("Expect ':' after 'case'."));
				switchconstants(F, &swstate, &E2);
				if (eareconstandeq(F, &E1, &E2)) {
					restorecontext(F, &C);
					swstate.casestate = CS_MATCH;
				} else {
					CODE(F, OP_EQ);
					swstate.casestate = CS_CASE;
					swstate.patch = CODEJMP(F, OP_JMP_IF_FALSE_POP);
				}
			} else if (!swstate.dflt) {
				swstate.dflt = 1;
				swstate.casestate = CS_DFLT;
				expect(F, TOK_COLON, expectstr("Expect ':' after 'default'."));
			} else
				error(F, comperrors[CE_SWDEF]);
			if (fts.len > 0)
				patchjmp(F, Array_cr_int_pop(&fts));
		} else {
			stm(F);
			if (swstate.casestate == CS_MATCH && F->fn->gotret) {
				// @TODO: Implement optimizations.
				// Also check if last 'stm' was 'breakstm' (same effect)
			}
		}
	}
	if (PREVT(F).type == TOK_EOF)
		error(F, comperrors[CE_SWRBR]);
	Array_cr_int_free(&fts, NULL);
	SwitchState_free(&swstate);
	endscope(F);
	CODE(F, OP_POP); // pop switch expression
	patchbreaklist(F);
	endbreaklist(F);
	F->cflow.innersdepth = sdepth;
	F->S->isswitch = inswitch;
}

static void ifstm(FunctionState *F)
{
	Exp E;
	Context C;
	int32_t jmptoelse, jmptoend = -1;
	savecontext(F, &C);
	expect(F, TOK_LPAREN, expectstr("Expect '(' after 'if'."));
	E.ins.set = 0;
	expr(F, &E); // condition
	expect(F, TOK_RPAREN, expectstr("Expect ')' after condition."));
	cr_ubyte remove = 0;
	cr_ubyte istrue = 0;
	if (etisconst(E.type)) {
		rmlastins(F, &E);
		if (etisfalse(E.type))
			remove = 1;
		else
			istrue = 1;
	} else
		jmptoelse = CODEJMP(F, OP_JMP_IF_FALSE_POP);
	stm(F);
	if (!remove) {
		jmptoend = CODEJMP(F, OP_JMP);
		if (!istrue)
			patchjmp(F, jmptoelse);
		else if (F->fn->gotret) { // condition is 1 and 'stm' was a 'returnstm'
			// @TODO: Implement optimization.
		}
	} else
		restorecontext(F, &C);
	F->fn->gotret = 0;
	if (match(F, TOK_ELSE)) { // there is 'else' branch?
		stm(F);
		if (!remove)
			patchjmp(F, jmptoend);
	}
}

static cr_inline void startloop(FunctionState *F, int32_t *lstart, int32_t *ldepth)
{
	*lstart = (F)->cflow.innerlstart;
	*ldepth = (F)->cflow.innerldepth;
	(F)->cflow.innerlstart = codeoffset(F);
	(F)->cflow.innerldepth = F->S->depth;
}

static cr_inline void endloop(FunctionState *F, int32_t lstart, int32_t ldepth)
{
	(F)->cflow.innerlstart = lstart;
	(F)->cflow.innerldepth = ldepth;
}

static void whilestm(FunctionState *F)
{
	Scope S;
	Context C;
	Exp E;
	int32_t lstart, ldepth;
	int32_t jmptoend = -1;
	cr_ubyte infinite = 0;
	cr_ubyte remove = 0;
	savecontext(F, &C);
	startscope(F, &S, 1, 0);
	startloop(F, &lstart, &ldepth);
	startbreaklist(F);
	expect(F, TOK_LPAREN, expectstr("Expect '(' after 'while'."));
	E.ins.set = 0;
	expr(F, &E); // conditional
	if (etisconst(E.type)) {
		rmlastins(F, &E);
		if (etisfalse(E.type))
			remove = 1;
		else
			infinite = 1;
	} else
		jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
	expect(F, TOK_RPAREN, expectstr("Expect ')' after condition."));
	stm(F); // body
	cr_ubyte gotret = F->fn->gotret;
	endscope(F);
	if (!remove) {
		codeloop(F, (F)->cflow.innerlstart);
		if (!infinite) {
			cr_assert(F->vm, jmptoend != -1, "end jmp invalid but flag is 0.");
			patchjmp(F, jmptoend);
		} else if (gotret) { // cond 1 and 'stm' was 'returnstm'
			// @TODO: Implement optimizations
		}
		patchbreaklist(F);
	} else
		restorecontext(F, &C);
	endloop(F, lstart, ldepth);
	endbreaklist(F);
}

static void forstm(FunctionState *F)
{
	Scope S;
	Context C;
	Exp E;
	int32_t lstart, ldepth;
	int32_t jmptoend = -1;
	cr_ubyte remove = 0;
	cr_ubyte infinite = 0;
	startscope(F, &S, 1, 0);
	startbreaklist(F);
	expect(F, TOK_LPAREN, expectstr("Expect '(' after 'for'."));
	if (match(F, TOK_SEMICOLON)) // Initializer for-clause
		; // no initializer
	else if (match(F, TOK_VAR))
		vardec(F);
	else if (match(F, TOK_FIXED))
		fvardec(F);
	else
		exprstm(F, 0);
	savecontext(F, &C);
	startloop(F, &lstart, &ldepth);
	if (!match(F, TOK_SEMICOLON)) { // conditional
		E.ins.set = 0;
		expr(F, &E);
		if (etisconst(E.type)) {
			rmlastins(F, &E);
			if (etistrue(E.type))
				infinite = 1;
			else
				remove = 1;
		} else
			jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
		expect(F, TOK_SEMICOLON, expectstr("Expect ';' after for-loop condition clause."));
	} else
		infinite = 1;
	if (!match(F, TOK_RPAREN)) { // last for-clause
		int32_t jmptobody = -1;
		int32_t jmptoincr = -1;
		if (!infinite && !remove)
			jmptobody = CODEJMP(F, OP_JMP);
		if (!remove)
			jmptoincr = codeoffset(F);
		exprstm(F, 1);
		if (!infinite && !remove) {
			codeloop(F, (F)->cflow.innerlstart);
			patchjmp(F, jmptobody);
			(F)->cflow.innerlstart = jmptoincr;
		}
		expect(F, TOK_RPAREN, expectstr("Expect ')' after last for-loop clause."));
	}
	stm(F); // Loop body
	if (!remove) {
		codeloop(F, (F)->cflow.innerlstart);
		if (!infinite)
			patchjmp(F, jmptoend);
		else if (F->fn->gotret) { // 'stm' was 'returnstm' and conditional is 1
			// @TODO: Implement optimizations
		}
		patchbreaklist(F);
	} else
		restorecontext(F, &C);
	endscope(F);
	endloop(F, lstart, ldepth);
	endbreaklist(F);
}

static int32_t foreachvars(FunctionState *F)
{
	int32_t vars = 0;
	do {
		if (vars >= CR_BYTECODE_MAX)
			error(F, comperrors[CE_VARLIST], CR_BYTECODE_MAX);
		vars++;
		expect(F, TOK_IDENTIFIER, expectstr("Expect varname."));
		make_local(F, &PREVT(F));
		initlocal(F, 0);
	} while (match(F, TOK_COMMA));
	return vars;
}

static void newlocalliteral(FunctionState *F, const char *name)
{
	Token syntok = syntoken(name);
	local_new(F, syntok);
	initlocal(F, 0);
}

static void foreachstm(FunctionState *F)
{
	Scope S;
	int32_t lstart, ldepth, vars, expc, endjmp;
	Exp E;
	startscope(F, &S, 1, 0);
	S.isgloop = 1; // set as generic loop
	startbreaklist(F);
	newlocalliteral(F, "(for iterator)"); // iterator function
	newlocalliteral(F, "(for invstate)"); // invariant state
	newlocalliteral(F, "(for cntlvar)"); // control variable
	vars = foreachvars(F); // declared vars
	expect(F, TOK_IN, expectstr("Expect 'in'."));
	E.ins.set = 0;
	expr(F, &E); // iterator factory
	expect_cond(F, !etisconst(E.type), "Can't call constant expression.");
	expc = 1;
	if (match(F, TOK_COMMA))
		expc += explist(F, 2, &E);
	adjustassign(F, &E, 3, expc);
	startloop(F, &lstart, &ldepth);
	CODEOP(F, OP_FOREACH_PREP, vars);
	CODEOP(F, OP_FOREACH, vars);
	endjmp = CODEJMP(F, OP_JMP);
	stm(F);
	CODEPOP(F, vars);
	codeloop(F, (F)->cflow.innerlstart);
	patchjmp(F, endjmp);
	endscope(F);
	patchbreaklist(F);
	endbreaklist(F);
	endloop(F, lstart, ldepth);
}

static void loopstm(FunctionState *F)
{
	Scope S;
	int32_t lstart, ldepth;
	startscope(F, &S, 1, 0);
	startbreaklist(F);
	startloop(F, &lstart, &ldepth);
	stm(F);
	if (F->fn->gotret) {
		// @TODO: Implement optimizations
	}
	codeloop(F, (F)->cflow.innerlstart);
	endscope(F);
	patchbreaklist(F);
	endbreaklist(F);
	endloop(F, lstart, ldepth);
}

static cr_inline Scope *loopscope(FunctionState *F)
{
	Scope *S = F->S;
	while (S != NULL) {
		if (S->isloop)
			return S;
		S = S->prev;
	}
	return NULL;
}

// Return count of switch statements until the first loop 'Scope'
// or the top-level code counting from the current 'Scope' in the 'FunctionState'.
static cr_inline int32_t switchcnt(FunctionState *F)
{
	Scope *S = F->S;
	int32_t count = 0;
	while (S != NULL && S->depth > F->cflow.innerldepth) {
		if (S->isswitch)
			count++;
		S = S->prev;
	}
	return count;
}

static void continuestm(FunctionState *F)
{
	expect(F, TOK_SEMICOLON, expectstr("Expect ';' after 'continue'."));
	if (cr_unlikely(F->cflow.innerlstart == -1))
		error(F, comperrors[CE_CONT]);
	else {
		Scope *S = loopscope(F);
		cr_assert(F->vm, S != NULL, "Loop scope not found but cflow offset is set.");
		int32_t popn = F->locals.len - (S->isgloop * 3) - S->localc + switchcnt(F);
		CODEPOP(F, popn);
		codeloop(F, F->cflow.innerlstart);
	}
}

static void breakstm(FunctionState *F)
{
	expect(F, TOK_SEMICOLON, expectstr("Expect ';' after 'break'."));
	Array_Array_cr_int *arr = &F->cflow.breaks;
	int32_t popn = 0;
	Scope *scope = NULL;
	switch (inwhat(F->S, &scope)) {
	case 0: // in switch
		popn++; // switch expression
	case 1: // FALLTHRU (in loop)
		break;
	case -1: // outside
		error(F, comperrors[CE_BREAK]);
		return;
	default:
		cr_unreachable;
	}
	popn += F->locals.len - scope->localc;
	CODEPOP(F, popn);
	Array_cr_int *last = Array_Array_cr_int_last(arr);
	Array_cr_int_push(last, CODEJMP(F, OP_JMP));
}

/// return ::= 'return' ';'
///          | 'return' explist ';'
static void returnstm(FunctionState *F)
{
	/* @TODO: Optimize even further by removing all of the cr_unreachable code. */
	static const char *expectstr = expectstr("Expect ';' after return statement values.");
	cr_ubyte gotret = F->fn->gotret;
	Context C;
	savecontext(F, &C);
	if (F->tag != -1) { // overload-able method ?
		const char *method = F->vm->faststatic[F->tag]->storage;
		if (tagisnoret(F->tag)) {
			if (F->tag == SS_INIT) { // init is a function call in cript syntax
				CODEOP(F, OP_GET_LOCAL, 0);
				CODE(F, OP_RET1);
			} else
				CODE(F, OP_RET0);
			if (!match(F, TOK_SEMICOLON))
				error(F, comperrors[CE_NORET], method);
		} else { // has return values
			cr_om tag = F->tag - SS_INIT;
			switch (tag) {
			case OM_DISPLAY:
			case OM_GETIDX:
#if defined(CR_OVERLOAD_OPS)
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
#endif
			{
				Exp _; // dummy
				_.ins.set = 0;
				explist(F, 1, &_);
				expect(F, TOK_SEMICOLON, expectstr);
				CODE(F, OP_RET1);
				break;
			}
			default:
				cr_unreachable;
			}
		}
	} else if (match(F, TOK_SEMICOLON)) { // implicit 'nil' return
		implicitreturn(F);
	} else { // generic return
		int idx = CODE(F, OP_RETSTART);
		Exp E;
		E.ins.set = 0;
		int32_t retcnt = explist(F, PARSER_RET_LIMIT, &E);
		expect(F, TOK_SEMICOLON, expectstr);
		cr_ubyte singleret = (retcnt == 1);
		if (ethasmulret(E.type)) {
			singleret = 0;
			setmulret(F, &E);
		}
		if (singleret) { // single return value ?
			Array_ubyte_remove(&CHUNK(F)->bcode, idx); // remove 'OP_RETSTART'
			CODE(F, OP_RET1);
		} else
			CODE(F, OP_RET);
	}
	if (gotret) { // last statement was 'return' ?
		F->fn->gotret = 1; // this is also a 'return' statement
		restorecontext(F, &C);
	}
}


/// dot ::= '.' name
///       | '.' name call
static void dot(FunctionState *F, Exp *E)
{
	expect(F, TOK_IDENTIFIER, expectstr("Expect property name after '.'."));
	Value identifier = tokintostr(F->vm, &PREVT(F));
	int idx = make_constant(F, identifier);
	if (match(F, TOK_LPAREN))
		codeinvoke(F, E, idx);
	else {
		E->type = EXP_INDEXED;
		E->value = idx;
		if (!E->ins.set)
			E->ins.code = CODEOP(F, OP_GET_PROPERTY, idx);
	}
}

static void stm(FunctionState *F)
{
	if (match(F, TOK_WHILE))
		whilestm(F);
	else if (match(F, TOK_FOR))
		forstm(F);
	else if (match(F, TOK_FOREACH))
		foreachstm(F);
	else if (match(F, TOK_IF))
		ifstm(F);
	else if (match(F, TOK_SWITCH))
		switchstm(F);
	else if (match(F, TOK_LBRACE))
		blockstm(F);
	else if (match(F, TOK_CONTINUE))
		continuestm(F);
	else if (match(F, TOK_BREAK))
		breakstm(F);
	else if (match(F, TOK_RETURN))
		returnstm(F);
	else if (match(F, TOK_LOOP))
		loopstm(F);
	else if (match(F, TOK_SEMICOLON))
		; // empty statement
	else
		exprstm(F, 0);
}

// indexed ::= '[' expr ']'
static cr_inline void indexed(FunctionState *F, Exp *E)
{
	Exp E2;
	E2.ins.set = 0;
	expr(F, &E2);
	expect(F, TOK_RBRACK, expectstr("Expect ']'."));
	E->type = EXP_INDEXED;
	E->value = NO_VAL;
	if (!E->ins.set)
		E->ins.code = CODE(F, OP_INDEX);
}




/*========================== EXPRESSION =========================*/

static void _self(FunctionState *F, Exp *E)
{
	if (F->cclass == NULL) {
		error(F, comperrors[CE_SELF]);
		return;
	}
	advance(F);
	codevarprev(F, E);
}

static void _super(FunctionState *F, Exp *E)
{
	if (!F->cclass) {
		error(F, comperrors[CE_SUPER]);
		return;
	}
	if (!F->cclass->superclass) {
		error(F, comperrors[CE_NOSUPER]);
		return;
	}
	advance(F);
	expect(F, TOK_DOT, expectstr("Expect '.' after 'super'."));
	expect(F, TOK_IDENTIFIER, expectstr("Expect superclass method name after '.'."));
	Value methodname = tokintostr(F->vm, &PREVT(F));
	int32_t idx = make_constant(F, methodname);
	Exp _; // dummy
	_.ins.set = 0;
	codevar(F, syntoken("self"), &_);
	if (match(F, TOK_LPAREN)) {
		OpCode opcall = call(F, E);
		codevar(F, syntoken("super"), &_);
		OpCode invokesuperop;
		switch (opcall) {
		case OP_CALL0:
			invokesuperop = OP_INVOKE_SUPER0;
			break;
		case OP_CALL1:
			invokesuperop = OP_INVOKE_SUPER1;
			break;
		case OP_CALL:
			invokesuperop = OP_INVOKE_SUPER;
			break;
		default:
			cr_unreachable;
		}
		E->ins.code = CODEOP(F, invokesuperop, idx);
		CODEL(F, 1); // retcnt
		E->type = EXP_INVOKE;
	} else {
		codevar(F, syntoken("super"), &_);
		if (E->ins.set)
			error(F, "Can't assign to methods from superclass.\n");
		E->ins.code = CODEOP(F, OP_GET_SUPER, idx);
		E->type = EXP_INDEXED; // superclass method
	}
}

// primary_exp ::= '(' exp ')'
//               | name
//               | 'self'
static void primaryexp(FunctionState *F, Exp *E)
{
	switch (CURRT(F).type) {
	case TOK_LPAREN:
		advance(F);
		expr(F, E);
		expect(F, TOK_RPAREN, "Expect ')'.\n");
		break;
	case TOK_IDENTIFIER:
		advance(F);
		codevarprev(F, E);
		break;
	case TOK_SELF:
		_self(F, E);
		break;
	case TOK_SUPER:
		_super(F, E);
		break;
	default:
		error(F, "Unexpected symbol.\n");
		return;
	}
}

// suffixedexp ::= primaryexp
//               | primaryexp [dot|call|indexed...]
static void suffixedexp(FunctionState *F, Exp *E)
{
	primaryexp(F, E);
	while (1) {
		switch (CURRT(F).type) {
		case TOK_DOT:
			advance(F);
			dot(F, E);
			break;
		case TOK_LPAREN:
			if (etisconst(E->type))
				error(F, comperrors[CE_CALLCONST]);
			advance(F);
			codecall(F, E);
			break;
		case TOK_LBRACK:
			advance(F);
			indexed(F, E);
			break;
		default:
			return;
		}
	}
}

// simpleexp ::= number
//             | string
//             | 'nil'
//             | '1'
//             | '0'
//             | '...'
//             | suffixedexp
static void simpleexp(FunctionState *F, Exp *E)
{
	int constidx;
	ExpType type;
	switch (CURRT(F).type) {
	case TOK_NUMBER:
		type = EXP_NUMBER;
		cr_assert(F->vm, IS_NUMBER(CURRT(F).value), "Expect number.");
		goto constfin;
	case TOK_STRING:
		type = EXP_STRING;
		cr_assert(F->vm, isstring(CURRT(F).value), "Expect string.");
constfin:
		constidx = make_constant(F, CURRT(F).value);
		initexp(E, type, CODEOP(F, OP_CONST, constidx), constidx);
		break;
	case TOK_NIL:
		initexp(E, EXP_NIL, CODE(F, OP_NIL), 0);
		break;
	case TOK_TRUE:
		initexp(E, EXP_TRUE, CODE(F, OP_TRUE), 0);
		break;
	case TOK_FALSE:
		initexp(E, EXP_FALSE, CODE(F, OP_FALSE), 0);
		break;
	case TOK_DOT_DOT_DOT:
		initexp(E, EXP_VARARG, vararg(F), 0);
		break;
	default:
		suffixedexp(F, E);
		return;
	}
	advance(F);
}


// Try folding unary operation.
// Example: OP_CONST (1), OP_NEG => OP_CONST (-1)
static cr_ubyte foldunary(FunctionState *F, UnaryOpr opr, Exp *E)
{
	if (E->type == EXP_NUMBER && opr == OPR_NEGATE) {
		double val = AS_NUMBER(*CONSTANT(F, E));
		if (cr_isnan(val) || val == 0.0)
			return 0;
		*CONSTANT(F, E) = NUMBER_VAL(-val);
		return 1;
	}
	return 0;
}

// Fold constant number expressions
static void calcnum(FunctionState *F, BinaryOpr opr, const Exp *E1, const Exp *E2, Value *result)
{
#define BINOP(op, n1, n2) NUMBER_VAL((n1)op(n2))

	double n1 = AS_NUMBER(*CONSTANT(F, E1));
	double n2 = AS_NUMBER(*CONSTANT(F, E2));
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
static cr_ubyte validop(FunctionState *F, BinaryOpr opr, const Exp *E1, const Exp *E2)
{
	double n1 = AS_NUMBER(*CONSTANT(F, E1));
	double n2 = AS_NUMBER(*CONSTANT(F, E2));
	return !(opr == OPR_MOD && (cr_floor(n1) != n1 || cr_floor(n2) != n2));
}

// Try folding binary operation
// Example: OP_CONST (1), OP_CONST (2), OP_ADD => OP_CONST (3)
static cr_ubyte foldbinary(FunctionState *F, BinaryOpr opr, Exp *E1, const Exp *E2)
{
	if (E1->type != E2->type || E1->type != EXP_NUMBER || !validop(F, opr, E1, E2))
		return 0;
	Value result;
	calcnum(F, opr, E1, E2, &result);
	if (cr_isnan(AS_NUMBER(result)))
		return 0;
	CONSTANT_POP(F); // Pop constant (E2)
	*CONSTANT(F, E1) = result; // Set new constant value (E1)
	LINSTRUCTION_POP(F); // Pop off the last OP_CONST instruction
	return 1;
}

// Emit optimized 'and' instruction
static void codeand(FunctionState *F, Exp *E)
{
	switch (E->type) {
	case EXP_TRUE:
		INSTRUCTION_POP(F);
		goto fin;
	case EXP_STRING:
	case EXP_NUMBER:
		LINSTRUCTION_POP(F);
		CONSTANT_POP(F);
fin:
		E->jmp.f = NO_JMP;
		break;
	default:
		E->jmp.f = CODEJMP(F, OP_JMP_IF_FALSE_OR_POP);
		E->ins.code = codeoffset(F) - 4; // Index of jump instruction
		break;
	}
	E->jmp.t = NO_JMP;
	E->type = EXP_JMP;
}

// Emit optimized 'or' instruction
static void codeor(FunctionState *F, Exp *E)
{
	switch (E->type) {
	case EXP_NIL:
	case EXP_FALSE:
		INSTRUCTION_POP(F);
		E->jmp.t = NO_JMP;
		break;
	case EXP_STRING:
	case EXP_NUMBER:
	case EXP_TRUE:
		E->jmp.t = codeoffset(F);
		E->jmp.f = CHUNK(F)->constants.len;
		return;
	default:
		E->jmp.f = CODEJMP(F, OP_JMP_IF_FALSE_AND_POP);
		E->jmp.t = CODEJMP(F, OP_JMP);
		patchjmp(F, E->jmp.f);
		break;
	}
	E->jmp.f = NO_JMP;
	E->jmp.t = EXP_JMP;
}

// Emit binary instruction
static void postfix(FunctionState *F, BinaryOpr opr, Exp *E1, Exp *E2)
{
	if (FOLDABLE(opr) && foldbinary(F, opr, E1, E2))
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
		E1->ins.code = CODEBIN(F, opr);
		E1->type = EXP_EXPR;
		E1->ins.binop = 1;
		break;
	case OPR_OR:
		if (E1->type != EXP_JMP) {
			concatcode(F, E1->jmp.t + 3, E1->jmp.f);
			*E1 = *E2;
		} else if (E1->jmp.t != NO_JMP) {
			patchjmp(F, E1->jmp.t);
			*E1 = *E2;
		} else
			E1->type = EXP_EXPR;
		break;
	case OPR_AND:
		if (E1->jmp.f == NO_JMP)
			*E1 = *E2;
		else {
			patchjmp(F, E1->jmp.f);
			E1->type = EXP_EXPR;
		}
		break;
	default:
		cr_unreachable;
	}
}

// cr_intermediate step that tries to optimize/process 'and' and 'or'
// instructions before the second expression gets parsed.
static void shortcircuit(FunctionState *F, BinaryOpr opr, Exp *E)
{
	switch (opr) {
	case OPR_AND:
		codeand(F, E);
		break;
	case OPR_OR:
		codeor(F, E);
		break;
	default:
		return;
	}
}

// Emit prefix instruction (only if folding didn't work)
static cr_inline void prefix(FunctionState *F, UnaryOpr opr, Exp *E)
{
	if (!foldunary(F, opr, E))
		CODE(F, unopr2op(opr));
}

// subexpr ::= simpleexp
//           | '-' simpleexp
//           | '!' simpleexp
//           | simpleexp '+' subexpr
//           | simpleexp '-' subexpr
//           | simpleexp '*' subexpr
//           | simpleexp '/' subexpr
//           | simpleexp '^' subexpr
//           | simpleexp '%' subexpr
//           | simpleexp '!=' subexpr
//           | simpleexp '==' subexpr
//           | simpleexp '<' subexpr
//           | simpleexp '<=' subexpr
//           | simpleexp '>' subexpr
//           | simpleexp '>=' subexpr
//           | simpleexp 'and' subexpr
//           | simpleexp 'or' subexpr
static BinaryOpr subexp(FunctionState *F, Exp *E1, int32_t limit)
{
	UnaryOpr unaryop = getunaryopr(CURRT(F).type);
	if (unaryop != OPR_NOUNARYOPR) {
		advance(F);
		subexp(F, E1, UNARY_PRIORITY);
		prefix(F, unaryop, E1);
	} else
		simpleexp(F, E1);
	BinaryOpr binop = getbinaryopr(CURRT(F).type);
	while (binop != OPR_NOBINOPR && priority[binop].left > limit) {
		Exp E2;
		E2.ins.set = 0;
		advance(F); // skip binary operator
		shortcircuit(F, binop, E1);
		BinaryOpr nextop = subexp(F, &E2, priority[binop].right);
		postfix(F, binop, E1, &E2);
		binop = nextop;
	}
	return binop;
}

// expr ::= subexpr
static void expr(FunctionState *F, Exp *E)
{
	subexp(F, E, 0);
}
