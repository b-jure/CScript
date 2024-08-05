/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Cript.
 * Cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef CRJMPTABLE_H
#define CRJMPTABLE_H


#undef DISPATCH
#undef CASE
#undef BREAK

#define DISPATCH(x)	goto *optable[x];
#define CASE(label) 	L_##label:
#define BREAK	    	DISPATCH(fetch(pc))


/* Make sure the order is the same as in the OpCode enum */
static const void *const optable[CR_NUMOPS] = {
	&&L_OP_TRUE,
	&&L_OP_FALSE,
	&&L_OP_NIL,
	&&L_OP_NILN,
	&&L_OP_CONST,
	&&L_OP_SETVARARG,
	&&L_OP_VARARG,
	&&L_OP_CLOSURE,
	&&L_OP_CLASS,
	&&L_OP_METHOD,
	&&L_OP_POP,
	&&L_OP_POPN,
	&&L_OP_NEG,
	&&L_OP_NOT,
	&&L_OP_ADD,
	&&L_OP_SUB,
	&&L_OP_MUL,
	&&L_OP_DIV,
	&&L_OP_MOD,
	&&L_OP_POW,
	&&L_OP_NEQ,
	&&L_OP_EQ,
	&&L_OP_EQUAL,
	&&L_OP_GT,
	&&L_OP_GE,
	&&L_OP_LT,
	&&L_OP_LE,
	&&L_OP_DEFGVAR,
	&&L_OP_DEFGVARL,
	&&L_OP_GETGVAR,
	&&L_OP_GETGVARL,
	&&L_OP_SETGVAR,
	&&L_OP_SETGVARL,
	&&L_OP_GETLVAR,
	&&L_OP_GETLVARL,
	&&L_OP_SETLVAR,
	&&L_OP_SETLVARL,
	&&L_OP_JZ,
	&&L_OP_JZPOP,
	&&L_OP_JZORPOP,
	&&L_OP_JZANDPOP,
	&&L_OP_JMP,
	&&L_OP_JMPANDPOP,
	&&L_OP_LOOP,
	&&L_OP_CALL0,
	&&L_OP_CALL1,
	&&L_OP_CALL,
	&&L_OP_CALLPROP0,
	&&L_OP_CALLPROP1,
	&&L_OP_CALLPROP,
	&&L_OP_CALLSUPER0,
	&&L_OP_CALLSUPER1,
	&&L_OP_CALLSUPER,
	&&L_OP_GETUVAL,
	&&L_OP_SETUVAL,
	&&L_OP_CLOSEUVAL,
	&&L_OP_CLOSEUVALN,
	&&L_OP_SETPROP,
	&&L_OP_GETPROP,
	&&L_OP_GETPROPIDX,
	&&L_OP_SETPROPIDX,
	&&L_OP_GETSUP,
	&&L_OP_GETSUPIDX,
	&&L_OP_SETVTABLE,
	&&L_OP_INHERIT,
	&&L_OP_CALLSTART,
	&&L_OP_RETSTART,
	&&L_OP_FOREACH_PREP,
	&&L_OP_FOREACH,
	&&L_OP_RET0,
	&&L_OP_RET1,
	&&L_OP_RET,
};

#endif
