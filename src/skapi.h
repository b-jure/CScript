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

#ifndef SKAPI_H
#define SKAPI_H

#include "skconf.h"

/* ==================== check C API  ==================== */

#define skapi_checkresults(vm, n, nr)                                                              \
    skapi_check(                                                                                   \
        vm,                                                                                        \
        (nr) == SK_MULRET || (((vm)->sp - (vm)->stack) + (n) + (nr)) < VM_STACK_LIMIT,             \
        "function results overflow the stack.")

#define skapi_checkelems(vm, n)                                                                    \
    skapi_check(vm, ((vm)->sp - last_frame(vm).callee) > (n), "not enough elements in the stack.")

#define skapi_checkerrcode(vm, errcode)                                                            \
    skapi_check(vm, (errcode) >= S_EARG && (errcode) <= S_EGLOBALREDEF, "invalid errcode")

#define skapi_checkomtag(vm, omtag)                                                                \
    skapi_check(vm, (omtag) >= 0 && (omtag) < OM_CNT, "invalid OMTag")

#define skapi_checksftag(vm, sftag)                                                                \
    skapi_check(vm, (sftag) >= 0 && (sftag) < SF_CNT, "invalid SFTag")

#define skapi_checkarop(vm, op)                                                                    \
    skapi_check(vm, (op) >= 0 && (op) < AR_CNT, "invalid arithmetic operation")

#define skapi_checkptr(vm, ptr) skapi_check(vm, (ptr) != NULL, "#ptr can't be (null) pointer")

#define skapi_checkstack(vm, n)                                                                    \
    skapi_check(                                                                                   \
        vm,                                                                                        \
        ((vm)->sp - (vm)->stack) + cast(ptrdiff_t, n) <= VM_STACK_LIMIT,                           \
        "not enough stack space for #n element/s")

#define skapi_checkordop(vm, ord)                                                                  \
    skapi_check(vm, ((ord) >= 0 && (ord) < ORD_CNT), "invalid Ord operation")

// Make sure 'typetag' is valid 'TypeTag'!
#define skapi_checktype(vm, value, typetag)                                                        \
    skapi_check(vm, val2type(value) == (typetag), "expect #typetag")

/* ------------------------------------------------------ */






/* ==================== change stack pointer with checks ==================== */

#define stklast(vm) cast_intptr(vm->stack + VM_STACK_LIMIT - 1)

/* Increment stack pointer */
#define skapi_incsp(vm)                                                                            \
    do {                                                                                           \
        (vm)->sp++;                                                                                \
        skapi_check(vm, vm->sp - vm->stack <= VM_STACK_LIMIT, "stack overflow.");                  \
    } while(0)

/* Decrement stack pointer */
#define skapi_decsp(vm)                                                                            \
    do {                                                                                           \
        (vm)->sp--;                                                                                \
        skapi_check(vm, vm->stack <= (vm)->sp, "stack underflow.");                                \
    } while(0)

/* ------------------------------------------------------ */






/* ==================== push values with checks ==================== */

/* Push value on the stack */
#define skapi_pushval(vm, val)                                                                     \
    do {                                                                                           \
        *(vm)->sp = val;                                                                           \
        skapi_incsp(vm);                                                                           \
    } while(0)

/* Push object on the stack */
#define skapi_pusho(vm, o) skapi_pushval(vm, OBJ_VAL(o))

/* Push string object */
#define skapi_pushostring(vm, string) skapi_pusho(vm, (O*)(string))

/* Push closure object */
#define skapi_pushoclosure(vm, closure) skapi_pusho(vm, (O*)(closure))

/* Push class */
#define skapi_pushoclass(vm, class) skapi_pusho(vm, (O*)(class))

/* Push instance */
#define skapi_pushoinst(vm, inst) skapi_pusho(vm, (O*)(inst))

/* Push native (C closure) */
#define skapi_pushonative(vm, native) skapi_pusho(vm, (O*)(native))

/* Push nil literal */
#define skapi_pushnil(vm) skapi_pushval(vm, NIL_VAL)

#define skapi_pushfstrva(vm, fmt, ...)                                                             \
    if(fmt) skapi_pushostring(vm, OString_fmt(vm, fmt __VA_OPT__(, ) __VA_ARGS__));                \
    else skapi_pushnil(vm);

/* Push formatted cstring */
#define skapi_pushfstr(vm, fmt, argp)                                                              \
    if(fmt) skapi_pushostring(vm, OString_fmt_from(vm, fmt, argp));                                \
    else skapi_pushnil(vm);

/* Push string */
#define skapi_pushstr(vm, ptr, len)                                                                \
    if(ptr) skapi_pushostring(vm, OString_new(vm, ptr, len));                                      \
    else skapi_pushnil(vm);

/* Push cstring */
#define skapi_pushcstr(vm, ptr) skapi_pushstr(vm, ptr, strlen(ptr))

/* Push true literal */
#define skapi_pushtrue(vm) skapi_pushval(vm, TRUE_VAL)

/* Push false literal */
#define skapi_pushfalse(vm) skapi_pushval(vm, FALSE_VAL)

/* Push bool */
#define skapi_pushbool(vm, b) skapi_pushval(vm, BOOL_VAL(b))

/* Push number */
#define skapi_pushnum(vm, n) skapi_pushval(vm, NUMBER_VAL(n))

/* ------------------------------------------------------ */



#endif
