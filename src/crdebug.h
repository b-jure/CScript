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


#include "crobject.h"


cr_noret cr_dg_throw(cr_State *ts, int code);
cr_noret cr_dg_runerror(cr_State *ts, const char *fmt, ...);
int cr_dg_getfuncline(const Function *fn, int pc);
const char *cr_dg_info(cr_State *ts, const char *msg, const OString *src, int line);


#endif
