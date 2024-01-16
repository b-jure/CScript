#ifndef SKAPI_H
#define SKAPI_H

#include "skconf.h"

/* ==================== check C API  ==================== */

#define skapi_checkresults(vm, n, nr)                                                              \
    sk_checkapi(                                                                                   \
        vm,                                                                                        \
        (nr) == SK_MULRET || (((vm)->sp - (vm)->stack) + (n) + (nr)) < VM_STACK_LIMIT,             \
        "function results overflow the stack.")

#define skapi_checkelems(vm, n)                                                                    \
    sk_checkapi(vm, ((vm)->sp - last_frame(vm).callee) > (n), "not enough elements in the stack.")

#define skapi_checkerrcode(vm, errcode)                                                            \
    sk_checkapi(vm, (errcode) >= S_EARG && (errcode) <= S_EGLOBALREDEF, "invalid errcode")

#define skapi_checkomtag(vm, omtag)                                                                \
    sk_checkapi(vm, (omtag) >= 0 && (omtag) < OM_CNT, "invalid OMTag")

#define skapi_checksftag(vm, sftag)                                                                \
    sk_checkapi(vm, (sftag) >= 0 && (sftag) < SF_CNT, "invalid SFTag")

#define skapi_checkarop(vm, op)                                                                    \
    sk_checkapi(vm, (op) >= 0 && (op) < AR_CNT, "invalid arithmetic operation")

#define skapi_checkptr(vm, ptr) sk_checkapi(vm, (ptr) != NULL, "#ptr can't be (null) pointer")

#define skapi_checkstack(vm, n)                                                                    \
    sk_checkapi(                                                                                   \
        vm,                                                                                        \
        ((vm)->sp - (vm)->stack) + cast(ptrdiff_t, n) <= VM_STACK_LIMIT,                           \
        "not enough stack space for #n elements")

#define skapi_checkordop(vm, ord)                                                                  \
    sk_checkapi(vm, ((ord) >= 0 && (ord) < ORD_CNT), "invalid Ord operation")

// Make sure 'typetag' is valid 'TypeTag'!
#define skapi_checktype(vm, value, typetag)                                                        \
    sk_checkapi(vm, val2type(value) == (typetag), "expect #typetag")

/* ------------------------------------------------------ */






/* ==================== change stack pointer with checks ==================== */

#define stklast(vm) cast_intptr(vm->stack + VM_STACK_LIMIT - 1)

/* Increment stack pointer */
#define skapi_incsp(vm)                                                                            \
    do {                                                                                           \
        (vm)->sp++;                                                                                \
        sk_checkapi(vm, vm->sp - vm->stack <= VM_STACK_LIMIT, "stack overflow.");                  \
    } while(0)

/* Decrement stack pointer */
#define skapi_decsp(vm)                                                                            \
    do {                                                                                           \
        (vm)->sp--;                                                                                \
        sk_checkapi(vm, vm->stack <= (vm)->sp, "stack underflow.");                                \
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
