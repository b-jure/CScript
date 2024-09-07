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


CRI_FUNC int crD_getfuncline(const Function *fn, int pc);
CRI_FUNC const char *crD_info(cr_State *ts, const char *msg,
                                   const OString *src, int line);
CRI_FUNC void crD_warnerror(cr_State *ts, const char *str);
CRI_FUNC cr_noret crD_runerror(cr_State *ts, const char *fmt, ...);
CRI_FUNC cr_noret crD_typeerror(cr_State *ts, const TValue *v,
                                     const char *op);
CRI_FUNC cr_noret crD_ordererror(cr_State *ts, const TValue *v1,
                                      const TValue *v2);
CRI_FUNC cr_noret crD_operror(cr_State *ts, const TValue *v1,
                                   const TValue *v2, const char *op);

#endif
