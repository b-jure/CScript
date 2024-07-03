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

#ifndef CRPARSER_H
#define CRPARSER_H


#include "crobject.h"
#include "crreader.h"
#include "crvalue.h"




/* variable kind (stored in 'mod') */
#define VARREGULAR	0 /* regular */
#define VARCONST	1 /* constant */
#define VARTBC		2 /* to-be-closed */
#define VARCTC		3 /* compile-time constant */
#define VARUNINIT	4 /* uninitialized */

/* active local variable compiler information */
typedef union LVarInfo {
	struct {
		TValueFields;
		int idx; /* index into Function 'lvars' */
		OString *name;
	} s;
	TValue val; /* constant value */
} LVarInfo;



Vec(LVarInfoVec, LVarInfo);

/* 
 * Dynamic data used by parser.
 * It is stored inside 'Lexer' because there is
 * only one lexer for every 'FunctionState'.
 */
typedef struct ParserState {
	struct ClassState *cs; /* linked-list */
	LVarInfoVec locals; /* compiler locals stack */
} ParserState;



/* --------------------------------------------------------------------------
 * Expression info
 * -------------------------------------------------------------------------- */

/* check expression type */
#define eisvar(e)		((e)->et >= EXP_UVAL && (e)->et <= EXP_INDEXED)
#define eisliteral(e)		((e)->et >= EXP_NIL && (e)->et <= EXP_FLT)
#define eiscall(e)		((e)->et >= EXP_CALL && (e)->et <= EXP_INVOKE)
#define eismulret(e)		(eiscall(e) && (e)->et <= EXP_VARARG)
#define eissuper(e)		((e)->et == EXP_INDEXRAWSUP || (e)->et == EXP_INDEXSUP)


/* expression types */
typedef enum expt {
	 /* no expression; */
	EXP_VOID,
	 /* 'nil' constant; */
	EXP_NIL,
	 /* 'false' constant; */
	EXP_FALSE,
	 /* 'true' constant; */
	EXP_TRUE,
	/* string constant;
	 * 'str' = string value; */
	EXP_STRING, 	
	/* integer constant;
	 * 'i' = integer value; */
	EXP_INT,
	/* floating constant;
	 * 'n' = floating value; */
	EXP_FLT,
	/* upvalue variable;
	 * 'info' = index of upvalue in 'upvalues' */
	EXP_UVAL, 
	/* local variable;
	 * 'idx' = relative index of variable in 'locals'; */
	EXP_LOCAL, 
	/* global variable; 
	 * 'idx' = index in 'gvars' (cr_State) */
	EXP_GLOBAL, 
	/* constant indexed variable ('[kk]');
	 * 'idx' = index in 'constants'; */
	EXP_INDEXK,
	/* indexed variable ('.k');
	 * 'idx' = index of constant string in 'constants'; */
	EXP_INDEXRAW,
	/* raw indexed 'super' variable ('super.k');
	 * 'idx' = index of constant string in 'constants'; */
	EXP_INDEXRAWSUP,
	/* indexed 'super' variable ('super[k]');
	 * 'idx' = index of constant string in 'constants'; */
	EXP_INDEXSUP,
	/* indexed variable ('[k=expr]'); */
	EXP_INDEXED,
	/* function call; 
	 * 'info' = pc (in 'code'); */
	EXP_CALL, 
	/* vararg expression ('...'); 
	 * 'info' = pc (in 'code'); */
	EXP_VARARG, 
	/* expression is a test/comparison; 
	 * 'info' = pc (in 'code') */
	EXP_JMP,
	 /* finalized expression */
	EXP_FINEXPR,
} expt;


/* 
 * Expression information.
 * Parser builds up the expression information and feeds it into
 * functions that generate bytecode.
 * Then those functions also fill the 'ExpInfo' accordingly.
 * So the codegen functions are essentially consumers of 'ExpInfo'.
 */
typedef struct ExpInfo {
	expt et;
	union {
		cr_number n; /* floating constant */
		cr_integer i; /* integer constant  */
		OString *str; /* string literal */
		int info; /* pc or some other generic information */
		int idx; /* index in 'locals' or 'constants' */
	} u;
	int t; /* jmp to patch if true */
	int f; /* jmp to patch if false */
	cr_ubyte set; /* true if setting expression */
} ExpInfo;



/* --------------------------------------------------------------------------
 * Function state
 * -------------------------------------------------------------------------- */

/* 
 * Represents all the information the parser has
 * on the currently parsed function.
 * So 'FunctionState' is a state for currently compiled 'Function'.
 * Keep in mind that most of the informations are filled in
 * during compilation and not only at the end, this
 * is because Cript compiler is a single pass compiler.
 */
typedef struct FunctionState {
	Function *fn; /* currently parsed function */
	struct FunctionState *prev; /* implicit linked-list */
	struct Lexer *lx; /* lexer */
	struct Scope *s; /* scope information */
	struct CFInfo *cflow; /* control flow information */
	int stkidx; /* first free stack index */
	int nlocals; /* number of active local variables */
	int firstlocal; /* index of first local in 'ParserState' */
	cr_ubyte close; /* true if needs to close upvalues before returning */
	cr_ubyte isvtm; /* true if this is vtable method */
} FunctionState;



int cr_pr_nstackvars(FunctionState *fs);
void _cleanup_function(FunctionState* F);
void F_free(FunctionState* F);
void mark_function_roots(FunctionState *F);

#endif
