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



/* automatic (local) variable */
typedef struct {
	OString *name; /* variable name */
	int depth; /* definition scope (-1 if uninitialized) */
	cr_ubyte flags; /* variable flags */
} LocalVar;



/*
 * ----------------------
 * Control flow structure
 * ----------------------
 */

Vec(intVecVec, intVec);

typedef struct {
	intVecVec breaks; /* break statement offsets */
	int32_t innerlstart; /* innermost loop start offset */
	int32_t innerldepth; /* innermost loop scope depth */
	int32_t innersdepth; /* innermost switch scope depth */
} ControlFlow;



/*
 * UpValue is a local variable that is defined inside of the enclosing function.
 * Contains stack index of that variable inside of the function that encloses,
 * or the index of that upvalue in the enclosing function if the variable to be
 * captured is located outside of the enclosing function (either global scope or
 * another function).
 */
typedef struct {
	int idx; /* stack index */
	cr_ubyte flags; /* variable flags */
	cr_ubyte local; /* captured in enclosing function */
} Upvalue;



/* --------------------------------------------------------------------------
 * Expression info
 * -------------------------------------------------------------------------- */

/* check expression type */
#define etisconst(exptype)	((exptype) >= EXP_FALSE && (exptype) <= EXP_NUMBER)
#define etisfalse(exptype)   	((exptype) >= EXP_FALSE && (exptype) <= EXP_NIL)
#define etistrue(exptype)    	((exptype) >= EXP_TRUE && (exptype) <= EXP_NUMBER)
#define etisvar(exptype)     	((exptype) >= EXP_UPVAL && (exptype) <= EXP_INDEXED)
#define etiscall(exptype)    	((exptype) >= EXP_CALL || (exptype) <= EXP_INVOKE)
#define ethasmulret(exptype) 	((exptype) >= EXP_CALL && (exptype) <= EXP_VARARG)
#define etisliteral(exptype) 	((exptype) >= EXP_FALSE && (exptype) <= EXP_TRUE)

/* expression types */
typedef enum ExpType {
	EXP_VOID = 0,  /* no expression */
	EXP_NIL, /* 'nil' literal */
	EXP_FALSE, /* 'false' literal */
	EXP_TRUE, /* 'true' literal */
	EXP_STRING,
	EXP_INT,
	EXP_FLT,
	EXP_UVAL,
	EXP_LOCAL,
	EXP_GLOBAL,
	EXP_INDEXED,
	EXP_CALL,
	EXP_INVOKE,
	EXP_VARARG,
	EXP_EXPR,
	EXP_JMP,
} ExpType;


/* expression information */
typedef struct {
	ExpType et;
	union {
		cr_number n;
		cr_integer i;
		OString *str;
		int info; /* expression value or index */
	} u;
	struct {
		int code; /* instruction index */
		cr_ubyte l; /* is this long instruction */
		cr_ubyte set; /* setter (or getter) */
		cr_ubyte binop; /* is this simple binary operator */
	} ins; /* instruction details */
	int t; /* jmp to patch if true */
	int f; /* jmp to patch if false */
} ExpInfo;




/* --------------------------------------------------------------------------
 * Function state
 * -------------------------------------------------------------------------- */

#define FNFUNCTION	0
#define FNMETHOD	1
#define FNSCRIPT	2

/* 'Vec's for 'FunctionState' */
Vec(LocalVec, LocalVar);
Vec(UpvalueVec, Upvalue);

/* currently parsed function state */
typedef struct FunctionState {
	Function *fn; /* currently parsed function */
	struct FunctionState *enclosing; /* list */
	struct Lexer *l;
	struct Scope *s;
	// struct ClassDecl *cldecl;
	int lastline; /* last saved line in array ('fn') */
	int nlineinfo; /* length of 'line' array ('fn') */
	ControlFlow cflow;
	UpvalueVec *upvalues; /* captured variables */
	LocalVec locals; /* local variables stack */
	cr_byte tag; /* tag for overload-able methods */
	cr_ubyte vflags; /* variable flags */
	cr_ubyte fntype;
} FunctionState;


/* check if current function is overloadable method */
#define isom(F) ((F)->tag != -1)


void _cleanup_function(FunctionState* F);
void F_free(FunctionState* F);
void mark_function_roots(FunctionState *F);

#endif
