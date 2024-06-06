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

#ifndef SKJMPTABLE_H
#define SKJMPTABLE_H

#undef DISPATCH
#undef CASE
#undef BREAK

/* Redefine dispatch from switch into goto */
#define DISPATCH(x) goto *optable[x];

/* Redefine case into label */
#define CASE(label) L_##label:

/* Redefine break into another goto/dispatch */
#define BREAK DISPATCH(READ_BYTE())

/* Make sure the order is the same as in the OpCode enum */
static const void *const optable[OPCODE_N] = {
	&&L_OP_TRUE,
	&&L_OP_FALSE,
	&&L_OP_NIL,
	&&L_OP_NILN,
	&&L_OP_NEG,
	&&L_OP_ADD,
	&&L_OP_SUB,
	&&L_OP_MUL,
	&&L_OP_DIV,
	&&L_OP_MOD,
	&&L_OP_POW,
	&&L_OP_NOT,
	&&L_OP_VARARG,
	&&L_OP_NOT_EQUAL,
	&&L_OP_EQUAL,
	&&L_OP_EQ,
	&&L_OP_GREATER,
	&&L_OP_GREATER_EQUAL,
	&&L_OP_LESS,
	&&L_OP_LESS_EQUAL,
	&&L_OP_POP,
	&&L_OP_POPN,
	&&L_OP_CONST,
	&&L_OP_DEFINE_GLOBAL,
	&&L_OP_DEFINE_GLOBALL,
	&&L_OP_GET_GLOBAL,
	&&L_OP_GET_GLOBALL,
	&&L_OP_SET_GLOBAL,
	&&L_OP_SET_GLOBALL,
	&&L_OP_GET_LOCAL,
	&&L_OP_GET_LOCALL,
	&&L_OP_SET_LOCAL,
	&&L_OP_SET_LOCALL,
	&&L_OP_JMP_IF_FALSE,
	&&L_OP_JMP_IF_FALSE_POP,
	&&L_OP_JMP_IF_FALSE_OR_POP,
	&&L_OP_JMP_IF_FALSE_AND_POP,
	&&L_OP_JMP,
	&&L_OP_JMP_AND_POP,
	&&L_OP_LOOP,
	&&L_OP_CALL0,
	&&L_OP_CALL1,
	&&L_OP_CALL,
	&&L_OP_CLOSURE,
	&&L_OP_GET_UPVALUE,
	&&L_OP_SET_UPVALUE,
	&&L_OP_CLOSE_UPVAL,
	&&L_OP_CLOSE_UPVALN,
	&&L_OP_CLASS,
	&&L_OP_SET_PROPERTY,
	&&L_OP_GET_PROPERTY,
	&&L_OP_INDEX,
	&&L_OP_SET_INDEX,
	&&L_OP_METHOD,
	&&L_OP_INVOKE0,
	&&L_OP_INVOKE1,
	&&L_OP_INVOKE,
	&&L_OP_OVERLOAD,
	&&L_OP_INHERIT,
	&&L_OP_GET_SUPER,
	&&L_OP_INVOKE_SUPER0,
	&&L_OP_INVOKE_SUPER1,
	&&L_OP_INVOKE_SUPER,
	&&L_OP_CALLSTART,
	&&L_OP_RETSTART,
	&&L_OP_FOREACH,
	&&L_OP_FOREACH_PREP,
	&&L_OP_RET0,
	&&L_OP_RET1,
	&&L_OP_RET,
};

#endif
