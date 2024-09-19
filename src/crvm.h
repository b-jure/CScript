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


#ifndef CRVM_H
#define CRVM_H

#include "crobject.h"
#include "crstate.h"


#define crV_rawEQ(v1,v2)    crV_orderEQ(NULL, v1, v2)


CRI_FUNC void crV_call(cr_State *ts, SPtr fn, int nreturns);
CRI_FUNC void crV_concat(cr_State *ts, int n);
CRI_FUNC cr_Integer crV_div(cr_State *ts, cr_Integer x, cr_Integer y);
CRI_FUNC cr_Integer crV_modint(cr_State *ts, cr_Integer x, cr_Integer y);
CRI_FUNC cr_Number crV_modnum(cr_State *ts, cr_Number x, cr_Number y);
CRI_FUNC void crV_arithm(cr_State *ts, const TValue *a, const TValue *b,
                         SPtr res, int op);
CRI_FUNC int crV_orderEQ(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC int crV_orderLT(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC int crV_orderLE(cr_State *ts, const TValue *v1, const TValue *v2);
CRI_FUNC void crV_execute(cr_State *ts, CallFrame *cf);

#endif
