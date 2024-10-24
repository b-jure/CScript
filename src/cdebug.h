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


#include "cobject.h"
#include "cstate.h"


#define crD_aritherror(ts,v1,v2) \
    crD_operror(ts, v1, v2, "perform arithmetic operation on")

#define crD_bitwerror(ts,v1,v2) \
    crD_operror(ts, v1, v2, "perform bitwise operation on")


CRI_FUNC int crD_getfuncline(const Function *fn, int pc);
CRI_FUNC const char *crD_findlocal(cr_State *ts, CallFrame *cf, int n,
                                   SPtr *pos);
CRI_FUNC const char *crD_info(cr_State *ts, const char *msg,
                              const OString *src, int line);
CRI_FUNC void crD_warnerror(cr_State *ts, const char *str);
CRI_FUNC cr_noret crD_runerror(cr_State *ts, const char *fmt, ...);
CRI_FUNC cr_noret crD_globalerror(cr_State *ts, const char *err, OString *name);
CRI_FUNC cr_noret crD_typeerror(cr_State *ts, const TValue *v,
                                const char *op);
CRI_FUNC cr_noret crD_ordererror(cr_State *ts, const TValue *v1,
                                 const TValue *v2);
CRI_FUNC cr_noret crD_typeerrormeta(cr_State *ts, const TValue *v1,
                                    const TValue *v2, const char * mop);
CRI_FUNC cr_noret crD_operror(cr_State *ts, const TValue *v1,
                              const TValue *v2, const char *op);
CRI_FUNC cr_noret crD_callerror(cr_State *ts, const TValue *obj);

#endif
