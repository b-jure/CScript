/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef SKDEBUG_H
#define SKDEBUG_H

#include "chunk.h"
#include "vmachine.h"

sdebug void Chunk_debug(VM* vm, Chunk* chunk, const char* name);
sdebug uint32_t Instruction_debug(VM* vm, Chunk* chunk, uint32_t offset);
sdebug uint32_t Chunk_getline(Chunk* chunk, uint32_t index);
void dumpstack(VM* vm, CallFrame* frame, Byte* ip);

// Internal assertions
#ifdef DEBUG_ASSERTIONS
#include <stdio.h>
#include <stdlib.h>
#define ASSERT(expr, fmt, ...)                                                                     \
    do {                                                                                           \
        if(!(expr)) {                                                                              \
            fprintf(                                                                               \
                stderr,                                                                            \
                "Assertion failed at %d:%s\n\t'" #expr "'\n\t" fmt "\n",                           \
                __LINE__,                                                                          \
                __FILE__ __VA_OPT__(, ) __VA_ARGS__);                                              \
            abort();                                                                               \
        }                                                                                          \
    } while(false)
#else
#define ASSERT(expr, fmt, ...)
#endif

#define TODO(info)  ASSERT(false, "TODO: " info)
#define PANIC(info) ASSERT(false, "PANIC: " info)

#endif
