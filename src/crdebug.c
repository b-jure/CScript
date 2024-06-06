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


#include "crchunk.h"
#include "crcommon.h"
#include "crdebug.h"
#include "crmem.h"
#include "crobject.h"
#include "criptapi.h"
#include "crvm.h"

#include <stdio.h>


// Load current CallFrame into the 'private' part of 'cr_debuginfo'.
// Level 0 means the current function, level 1 is the previous call
// and so on...
// If 'level' is invalid, this function returns 0 otherwise 1.
CR_API int cr_getstack(VM *vm, int level, cr_debuginfo *di)
{
	CallFrame *cf;

	cr_lock(vm);
	if (level > vm->frames.len || level < 0) {
		cr_unlock(vm);
		return 0;
	}
	cf = &vm->frames.ptr[vm->frames.len - 1 - level];
	di->frame = cf;
	cr_unlock(vm);
	return 1;
}



/* 
 * Sets 'name', 'type', 'nups', 'nparams', 'isvararg', 'defline',
 * 'deflastline' in 'cr_debuginfo'.
 */
static void getfuncinfo(VM *vm, Closure *cl, cr_debuginfo *di)
{
	const Function *fn;

	if (isccl(cl)) {
		di->nups = cl->cc.nupvalues;
		di->defline = -1;
		di->deflastline = -1;
		di->nparams = -1;
		di->isvararg = 0;
		di->name = "?";
		di->type = "C";
	} else {
		di->nups = cl->crc.nupvalues;
		fn = cl->crc.fn;;
		di->defline = fn->defline;
		di->deflastline = fn->deflastline;
		di->nparams = fn->arity;
		di->isvararg = fn->isvararg;
		di->name = fn->name->bytes;
		di->type = (fn->defline == 0) ? "main" : "Cript";
	}
}


int cr_dg_getfuncline(const Function *fn, int pc)
{
	// TODO
	cr_assert(fn->lineinfo.ptr != NULL);
	return 0;
}


cr_sinline int getcurrentpc(const CallFrame *cf)
{
	cr_assert(cfiscript(cf));
	return cast_int(cf->f.pc - cffn(cf)->code.ptr) - 1;
}


cr_sinline int getcurrentline(CallFrame *cf) 
{
	return cr_dg_getfuncline(cf->f.cl->fn, getcurrentpc(cf));
}


/* sets 'source', 'srclen' and 'shortsrc' in 'cr_debuginfo' */
static void getsrcinfo(Closure *cl, cr_debuginfo *di)
{
	if (isccl(cl)) {
	}
	di->source = p->source->storage;
	di->srclen = p->source->len;
	size_t bufflen = CR_SRCID_MAX - 1;
	if (bufflen < di->srclen) {
		memcpy(di->shortsrc, di->source, bufflen - SLL("..."));
		memcpy(di->shortsrc, "...", SLL("..."));
	} else
		memcpy(di->shortsrc, di->source, bufflen);
	di->shortsrc[bufflen] = '\0';
}



// Auxiliary to 'cr_getinfo', parses debug bit mask and
// fills out the 'cr_debuginfo' accordingly.
// If any invalid bit/option is inside the 'dbmask' this
// function returns 0, otherwise 1.
static cr_ubyte auxgetinfo(VM *vm, cr_ubyte dbmask, Value *cl, CallFrame *cf, cr_debuginfo *di)
{
	cr_ubyte status = 1;
	for (cr_ubyte bit = 2; dbmask > 0; bit++) {
		switch (bit) {
		case 2:; // DW_LINE
			di->line = (cfiscript(cf) ? getcurrentline(cf) : -1);
			break;
		case 3: // DW_FNINFO
			getfuncinfo(vm, *cl, di);
			break;
		case 4: // DW_FNSRC
			getsrcinfo(asclosure(cl), di);
			break;
		case 5: // DW_FNPUSH
			criptapi_pushval(vm, *di->frame->callee);
			break;
		case 6: // unused
		case 7: // unused
			status = 0;
			break;
		default:
			cr_unreachable;
		}
		dbmask >>= 1;
	}
	return status;
}

// Fill out 'cr_debuginfo' according to 'dbmask'.
// Return 0 if any of the bits in 'dbmask' is invalid,
// otherwise 1.
CR_API int cr_getinfo(VM *vm, int dbmask, cr_debuginfo *di)
{
	Value *fn;
	CallFrame *frame;
	int status;

	cr_lock(vm);
	status = 1;
	if (dbmask & CR_DBGFNGET) { /* use function on top of the stack ? */
		fn = stkpeek(0);
		criptapi_checktype(vm, *fn, TT_FUNCTION);
	} else { // use function stored in current frame
		frame = &last_frame(vm);
		fn = last_frame(vm).callee;
		cr_assert(vm, val2type(*fn) == TT_FUNCTION, "expect function");
	}
	dbmask >>= 1; // skip DW_FNGET
	status = auxgetinfo(vm, dbmask, fn, frame, di);
	cr_unlock(vm);
	return status;
}

void dumpstack(VM *vm, CallFrame *frame, Byte *ip)
{
	printf("           ");
	for (Value *ptr = vm->stack; ptr < vm->sp; ptr++) {
		printf("[");
		vprint(vm, *ptr, stderr);
		printf("]");
	}
	printf("\n");
	Instruction_debug(vm, &FFN(frame)->chunk, (uint32_t)(ip - FFN(frame)->chunk.code.data));
}


void Chunk_debug(VM *vm, Chunk *chunk, const char *name)
{
	printf("=== %s ===\n", name);
	for (uint32_t offset = 0; offset < chunk->code.len;)
		offset = Instruction_debug(vm, chunk, offset);
}

static int32_t simpleins(const char *name, uint32_t offset)
{
	printf("%s\n", name);
	return offset + 1; /* OpCode */
}

static int32_t jmpins(const char *name, int8_t sign, Chunk *chunk, uint32_t offset)
{
	int32_t jmp = GET_BYTES3(&chunk->code.data[offset + 1]);
	printf("%-25s %5u -> %d\n", name, offset, offset + 4 + (sign * jmp));
	return offset + 4;
}

static void constant(VM *vm, Chunk *chunk, uint32_t param)
{
	printf("'");
	vprint(vm, *Array_Value_index(&chunk->constants, param), stderr);
	printf("'");
}

static uint32_t closure(VM *vm, Chunk *chunk, uint32_t param, uint32_t offset)
{
	Value value = *Array_Value_index(&chunk->constants, param);
	vprint(vm, value, stderr);
	printf("\n");
	OFunction *fn = asfn(value);
	for (uint32_t i = 0; i < fn->p.upvalc; i++) {
		bool local = chunk->code.data[offset++];
		offset++; // flags
		uint32_t idx = GET_BYTES3(&chunk->code.data[offset]);
		printf("%04d     |                                 %s %d\n", offset, local ? "local" : "upvalue", idx);
		offset += 3;
	}
	return offset;
}

static int32_t shorinst(VM *vm, const char *name, Chunk *chunk, OpCode code, uint32_t offset)
{
	Byte param = *Array_ubyte_index(&chunk->code, offset + 1);
	printf("%-25s %5u ", name, param);
	switch (code) {
	case OP_CONST:
	case OP_CLASS:
	case OP_SET_PROPERTY:
	case OP_GET_PROPERTY:
	case OP_METHOD:
	case OP_GET_SUPER:
		constant(vm, chunk, param);
		break;
	case OP_OVERLOAD:
		printf("'%s'", static_strings[param].name);
		break;
	default:
		// do nothing
		break;
	}
	printf("\n");
	return offset + 2; /* OpCode + param(8-bit/1-byte) */
}

static int32_t longins(VM *vm, const char *name, Chunk *chunk, OpCode code, uint32_t offset)
{
	uint32_t param = GET_BYTES3(&chunk->code.data[offset + 1]);
	printf("%-25s %5u ", name, param);
	switch (code) {
	case OP_CLOSURE:
		return closure(vm, chunk, param, offset + 4);
	case OP_CONST:
	case OP_CLASS:
	case OP_SET_PROPERTY:
	case OP_GET_PROPERTY:
	case OP_METHOD:
	case OP_GET_SUPER:
		constant(vm, chunk, param);
		break;
	default:
		// do nothing
		break;
	}
	printf("\n");
	return offset + 4; /* OpCode(8-bit/1-byte) + param(24-bit/3-bytes) */
}

static int32_t invoke(VM *vm, const char *name, Chunk *chunk, int32_t offset)
{
	uint32_t key = GET_BYTES3(&chunk->code.data[offset + 1]);
	offset += 4;
	int32_t retcnt = GET_BYTES3(&chunk->code.data[offset]);
	printf("%-25s (retcnt %d) %5d ", name, retcnt, key);
	constant(vm, chunk, key);
	printf("\n");
	return offset + 3;
}

uint32_t Instruction_debug(VM *vm, Chunk *chunk, uint32_t offset)
{
	printf("%04d ", offset);
	uint32_t line = Chunk_getline(chunk, offset);
	if (offset > 0 && line == Chunk_getline(chunk, offset - 1))
		printf("    | ");
	else
		printf("%5d ", line);
	Byte instruction = chunk->code.data[offset];
	switch (instruction) {
	case OP_TRUE:
		return simpleins("OP_TRUE", offset);
	case OP_FALSE:
		return simpleins("OP_FALSE", offset);
	case OP_NIL:
		return simpleins("OP_NIL", offset);
	case OP_NILN:
		return longins(vm, "OP_NILN", chunk, OP_NILN, offset);
	case OP_VARARG:
		return longins(vm, "OP_VARARG", chunk, OP_VARARG, offset);
	case OP_NEG:
		return simpleins("OP_NEG", offset);
	case OP_ADD:
		return simpleins("OP_ADD", offset);
	case OP_SUB:
		return simpleins("OP_SUB", offset);
	case OP_MUL:
		return simpleins("OP_MUL", offset);
	case OP_MOD:
		return simpleins("OP_MOD", offset);
	case OP_POW:
		return simpleins("OP_POW", offset);
	case OP_DIV:
		return simpleins("OP_DIV", offset);
	case OP_NOT:
		return simpleins("OP_NOT", offset);
	case OP_NOT_EQUAL:
		return simpleins("OP_NOT_EQUAL", offset);
	case OP_EQUAL:
		return simpleins("OP_EQUAL", offset);
	case OP_EQ:
		return simpleins("OP_EQ", offset);
	case OP_GREATER_EQUAL:
		return simpleins("OP_GREATER_EQUAL", offset);
	case OP_GREATER:
		return simpleins("OP_GREATER", offset);
	case OP_LESS:
		return simpleins("OP_LESS", offset);
	case OP_LESS_EQUAL:
		return simpleins("OP_LESS_EQUAL", offset);
	case OP_POP:
		return simpleins("OP_POP", offset);
	case OP_POPN:
		return longins(vm, "OP_POPN", chunk, OP_POPN, offset);
	case OP_CONST:
		return longins(vm, "OP_CONST", chunk, OP_CONST, offset);
	case OP_DEFINE_GLOBAL:
		return shorinst(vm, "OP_DEFINE_GLOBAL", chunk, OP_DEFINE_GLOBAL, offset);
	case OP_DEFINE_GLOBALL:
		return longins(vm, "OP_DEFINE_GLOBALL", chunk, OP_DEFINE_GLOBALL, offset);
	case OP_GET_GLOBAL:
		return shorinst(vm, "OP_GET_GLOBAL", chunk, OP_GET_GLOBAL, offset);
	case OP_GET_GLOBALL:
		return longins(vm, "OP_GET_GLOBALL", chunk, OP_GET_GLOBALL, offset);
	case OP_SET_GLOBAL:
		return shorinst(vm, "OP_SET_GLOBAL", chunk, OP_SET_GLOBAL, offset);
	case OP_SET_GLOBALL:
		return longins(vm, "OP_SET_GLOBALL", chunk, OP_SET_GLOBALL, offset);
	case OP_GET_LOCAL:
		return shorinst(vm, "OP_GET_LOCAL", chunk, OP_GET_LOCAL, offset);
	case OP_GET_LOCALL:
		return longins(vm, "OP_GET_LOCALL", chunk, OP_GET_LOCALL, offset);
	case OP_SET_LOCAL:
		return shorinst(vm, "OP_SET_LOCAL", chunk, OP_SET_LOCAL, offset);
	case OP_SET_LOCALL:
		return longins(vm, "OP_SET_LOCALL", chunk, OP_SET_LOCALL, offset);
	case OP_JMP_IF_FALSE:
		return jmpins("OP_JMP_IF_FALSE", 1, chunk, offset);
	case OP_JMP_IF_FALSE_POP:
		return jmpins("OP_JMP_IF_FALSE_POP", 1, chunk, offset);
	case OP_JMP_IF_FALSE_OR_POP:
		return jmpins("OP_JMP_IF_FALSE_OR_POP", 1, chunk, offset);
	case OP_JMP_IF_FALSE_AND_POP:
		return jmpins("OP_JMP_IF_FALSE_AND_POP", 1, chunk, offset);
	case OP_JMP:
		return jmpins("OP_JMP", 1, chunk, offset);
	case OP_JMP_AND_POP:
		return jmpins("OP_JMP_AND_POP", 1, chunk, offset);
	case OP_LOOP:
		return jmpins("OP_LOOP", -1, chunk, offset);
	case OP_CALL0:
		return simpleins("OP_CALL0", offset);
	case OP_CALL1:
		return simpleins("OP_CALL1", offset);
	case OP_CALL:
		return longins(vm, "OP_CALL", chunk, OP_CALL, offset);
	case OP_CLOSURE:
		return longins(vm, "OP_CLOSURE", chunk, OP_CLOSURE, offset);
	case OP_GET_UPVALUE:
		return longins(vm, "OP_GET_UPVALUE", chunk, OP_GET_UPVALUE, offset);
	case OP_SET_UPVALUE:
		return longins(vm, "OP_SET_UPVALUE", chunk, OP_SET_UPVALUE, offset);
	case OP_CLOSE_UPVAL:
		return simpleins("OP_CLOSE_UPVAL", offset);
	case OP_CLOSE_UPVALN:
		return longins(vm, "OP_CLOSE_UPVALN", chunk, OP_CLOSE_UPVALN, offset);
	case OP_CLASS:
		return longins(vm, "OP_CLASS", chunk, OP_CLASS, offset);
	case OP_SET_PROPERTY:
		return longins(vm, "OP_SET_PROPERTY", chunk, OP_SET_PROPERTY, offset);
	case OP_GET_PROPERTY:
		return longins(vm, "OP_GET_PROPERTY", chunk, OP_GET_PROPERTY, offset);
	case OP_INDEX:
		return simpleins("OP_INDEX", offset);
	case OP_SET_INDEX:
		return simpleins("OP_SET_INDEX", offset);
	case OP_METHOD:
		return longins(vm, "OP_METHOD", chunk, OP_METHOD, offset);
	case OP_INVOKE0:
		return invoke(vm, "OP_INVOKE0", chunk, offset);
	case OP_INVOKE1:
		return invoke(vm, "OP_INVOKE1", chunk, offset);
	case OP_INVOKE:
		return invoke(vm, "OP_INVOKE", chunk, offset);
	case OP_OVERLOAD:
		return shorinst(vm, "OP_OVERLOAD", chunk, OP_OVERLOAD, offset);
	case OP_INHERIT:
		return simpleins("OP_INHERIT", offset);
	case OP_GET_SUPER:
		return longins(vm, "OP_GET_SUPER", chunk, OP_GET_SUPER, offset);
	case OP_INVOKE_SUPER0:
		return invoke(vm, "OP_INVOKE_SUPER0", chunk, offset);
	case OP_INVOKE_SUPER1:
		return invoke(vm, "OP_INVOKE_SUPER1", chunk, offset);
	case OP_INVOKE_SUPER:
		return invoke(vm, "OP_INVOKE_SUPER", chunk, offset);
	case OP_CALLSTART:
		return simpleins("OP_CALLSTART", offset);
	case OP_RETSTART:
		return simpleins("OP_RETSTART", offset);
	case OP_FOREACH:
		return longins(vm, "OP_FOREACH", chunk, OP_FOREACH, offset);
	case OP_FOREACH_PREP:
		return longins(vm, "OP_FOREACH_PREP", chunk, OP_FOREACH_PREP, offset);
	case OP_RET0:
		return simpleins("OP_RET0", offset);
	case OP_RET1:
		return simpleins("OP_RET1", offset);
	case OP_RET:
		return simpleins("OP_RET", offset);
	default:
		printf("Unknown opcode: %d\n", instruction);
		return offset + 1;
	}
}
