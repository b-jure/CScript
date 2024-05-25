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

#ifndef CRIPTAPI_H
#define CRIPTAPI_H

#include "crconf.h"

/* ==================== check C API  ==================== */

#define criptapi_checkresults(vm, n, nr)                                                              \
    criptapi_check(                                                                                   \
        vm,                                                                                        \
        (nr) == CR_MULRET || (((vm)->sp - (vm)->stack) + (n) + (nr)) < VM_STACK_LIMIT,             \
        "function results overflow the stack.")

#define criptapi_checkelems(vm, n)                                                                    \
    criptapi_check(vm, ((vm)->sp - last_frame(vm).callee) > (n), "not enough elements in the stack.")

#define criptapi_checkerrcode(vm, errcode)                                                            \
    criptapi_check(vm, (errcode) >= S_EARG && (errcode) <= S_EGLOBALREDEF, "invalid errcode")

#define criptapi_checkomtag(vm, omtag)                                                                \
    criptapi_check(vm, (omtag) >= 0 && (omtag) < SKOM_CNT, "invalid OMTag")

#define criptapi_checksftag(vm, sftag)                                                                \
    criptapi_check(vm, (sftag) >= 0 && (sftag) < SKSF_CNT, "invalid SFTag")

#define criptapi_checkarop(vm, op)                                                                    \
    criptapi_check(vm, (op) >= 0 && (op) < SKAR_CNT, "invalid arithmetic operation")

#define criptapi_checkptr(vm, ptr) criptapi_check(vm, (ptr) != NULL, "#ptr can't be (null) pointer")

#define criptapi_checkstack(vm, n)                                                                    \
    criptapi_check(                                                                                   \
        vm,                                                                                        \
        ((vm)->sp - (vm)->stack) + cast(ptrdiff_t, n) <= VM_STACK_LIMIT,                           \
        "not enough stack space for #n element/s")

#define criptapi_checkordop(vm, ord)                                                                  \
    criptapi_check(vm, ((ord) >= 0 && (ord) < SKORD_CNT), "invalid Ord operation")

// Make sure 'typetag' is valid 'TypeTag'!
#define criptapi_checktype(vm, value, typetag)                                                        \
    criptapi_check(vm, val2type(value) == (typetag), "expect #typetag")

/* ------------------------------------------------------ */






/* ==================== change stack pointer with checks ==================== */

#define stklast(vm) cast_intptr(vm->stack + VM_STACK_LIMIT - 1)

/* Increment stack pointer */
#define criptapi_incsp(vm)                                                                            \
    do {                                                                                           \
        (vm)->sp++;                                                                                \
        criptapi_check(vm, vm->sp - vm->stack <= VM_STACK_LIMIT, "stack overflow.");                  \
    } while(0)

/* Decrement stack pointer */
#define criptapi_decsp(vm)                                                                            \
    do {                                                                                           \
        (vm)->sp--;                                                                                \
        criptapi_check(vm, vm->stack <= (vm)->sp, "stack underflow.");                                \
    } while(0)

/* ------------------------------------------------------ */






/* ==================== push values with checks ==================== */

/* Push value on the stack */
#define criptapi_pushval(vm, val)                                                                     \
    do {                                                                                           \
        *(vm)->sp = val;                                                                           \
        criptapi_incsp(vm);                                                                           \
    } while(0)

/* Push object on the stack */
#define criptapi_pusho(vm, o) criptapi_pushval(vm, OBJ_VAL(o))

/* Push string object */
#define criptapi_pushostring(vm, string) criptapi_pusho(vm, (O*)(string))

/* Push closure object */
#define criptapi_pushoclosure(vm, closure) criptapi_pusho(vm, (O*)(closure))

/* Push class */
#define criptapi_pushoclass(vm, class) criptapi_pusho(vm, (O*)(class))

/* Push instance */
#define criptapi_pushoinst(vm, inst) criptapi_pusho(vm, (O*)(inst))

/* Push native (C closure) */
#define criptapi_pushonative(vm, native) criptapi_pusho(vm, (O*)(native))

/* Push nil literal */
#define criptapi_pushnil(vm) criptapi_pushval(vm, NIL_VAL)

#define criptapi_pushfstrva(vm, fmt, ...)                                                             \
    if(fmt) criptapi_pushostring(vm, OString_fmt(vm, fmt __VA_OPT__(, ) __VA_ARGS__));                \
    else criptapi_pushnil(vm);

/* Push formatted cstring */
#define criptapi_pushfstr(vm, fmt, argp)                                                              \
    if(fmt) criptapi_pushostring(vm, OString_fmt_from(vm, fmt, argp));                                \
    else criptapi_pushnil(vm);

/* Push string */
#define criptapi_pushstr(vm, ptr, len)                                                                \
    if(ptr) criptapi_pushostring(vm, OString_new(vm, ptr, len));                                      \
    else criptapi_pushnil(vm);

/* Push cstring */
#define criptapi_pushcstr(vm, ptr) criptapi_pushstr(vm, ptr, strlen(ptr))

/* Push true literal */
#define criptapi_pushtrue(vm) criptapi_pushval(vm, TRUE_VAL)

/* Push false literal */
#define criptapi_pushfalse(vm) criptapi_pushval(vm, FALSE_VAL)

/* Push bool */
#define criptapi_pushbool(vm, b) criptapi_pushval(vm, BOOL_VAL(b))

/* Push float */
#define criptapi_pushfloat(vm, n) criptapi_pushval(vm, FLOAT_VAL(n))

/* Push integer */
#define criptapi_pushinteger(vm, n) criptapi_pushval(vm, INTEGER_VAL(n))

/* ------------------------------------------------------ */

#endif
