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

#ifndef CRDEBUG_H
#define CRDEBUG_H


#include "crchunk.h"
#include "crvm.h"


int cr_dg_getfuncline(const Function *fn, int pc);
void Chunk_debug(VM* vm, Chunk* chunk, const char* name);
uint32_t Instruction_debug(VM* vm, Chunk* chunk, uint32_t offset);
uint32_t Chunk_getline(Chunk* chunk, uint32_t index);
void dumpstack(VM* vm, CallFrame* frame, cr_ubyte* ip);


#endif
