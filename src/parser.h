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

#ifndef SKPARSER_H
#define SKPARSER_H

#include "array.h"
#include "common.h"
#include "reader.h"
#include "value.h"
#include "vmachine.h"

uint8_t pcompile(VM* vm, void* userdata, const char* name, uint8_t isingscope);
void _cleanup_function(Function* F);
void F_free(Function* F);
void mark_function_roots(VM* vm);

#endif
