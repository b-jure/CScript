/*
** capi.c
** CScript API
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "carray.h"
#include "cdebug.h"
#include "cfunction.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cprotected.h"
#include "cscript.h"
#include "csconf.h"
#include "climits.h"
#include "ctable.h"
#include "cobject.h"
#include "cscript.h"
#include "creader.h"
#include "cobject.h"
#include "cstate.h"
#include "cstring.h"
#include "cvm.h"
#include "stdarg.h"
#include "capi.h"
#include "ctrace.h"


/* test for pseudo index */
#define ispseudo(i)		((i) <= CS_REGISTRYINDEX)

/* test for upvalue */
#define isupvalue(i)		((i) < CS_REGISTRYINDEX)

/* test for valid index */
#define isvalid(C,o)           (!ttisnil(o) || (o) != &G(C)->nil)


/* 
** Convert index to a pointer to its value.
** Invalid indices (using upvalue index for CScript functions) return
** special nil value `&G(C)->nil`.
*/
static TValue *index2value(const cs_State *C, int index) {
    CallFrame *cf = C->cf;
    if (index >= 0) { /* absolute index? */
        SPtr o = (cf->func.p + 1) + index;
        api_check(C, index < cf->top.p - (cf->func.p + 1), "index too large");
        if (o >= C->sp.p) return &G(C)->nil;
        else return s2v(o);
    } else if (!ispseudo(index)) { /* negative index? */
        api_check(C, -index <= C->sp.p - (cf->func.p + 1), "index too small");
        return s2v(C->sp.p + index);
    } else if (index == CS_REGISTRYINDEX) {
        return &G(C)->c_registry;
    } else { /* upvalues */
        index = CS_REGISTRYINDEX - index;
        api_check(C, index < MAXUPVAL, "upvalue index too large");
        if (c_likely(ttisCclosure(s2v(cf->func.p)))) { /* C closure? */
            CClosure *ccl = clCval(s2v(cf->func.p));
            return &ccl->upvals[index];
        } else { /* CScript function (invalid) */
            api_check(C, 0, "caller not a C closure");
            return &G(C)->nil; /* no upvalues */
        }
    }
}


static SPtr index2stack(const cs_State *C, int index) {
    CallFrame *cf = C->cf;
    if (index >= 0) {
        SPtr p = (cf->func.p + 1) + index;
        api_check(C, p < C->sp.p, "invalid index");
        return p;
    } else { /* negative index */
        api_check(C, -index <= (C->sp.p - (cf->func.p + 1)), "invalid index");
        api_check(C, !ispseudo(index), "invalid index");
        return C->sp.p + index; /* index is subtracted */
    }
}


CS_API cs_CFunction cs_atpanic(cs_State *C, cs_CFunction fpanic) {
    cs_CFunction old_panic;
    cs_lock(C);
    old_panic = G(C)->fpanic;
    G(C)->fpanic = fpanic;
    cs_unlock(C);
    return old_panic;
}


CS_API cs_Number cs_version(cs_State *C) {
    UNUSED(C);
    return CS_VERSION_NUMBER;
}


c_sinline void setntop(cs_State *C, int n) {
    CallFrame *cf;
    SPtr func, newtop;
    ptrdiff_t diff;
    cf = C->cf;
    func = cf->func.p;
    if (n >= 0) {
        api_check(C, n <= (cf->top.p - func + 1), "new top too large");
        diff = (func + 1 + n) - C->sp.p;
        for (; diff > 0; diff--)
            setnilval(s2v(C->sp.p++));
    } else { /* negative index */
        api_check(C, -(n+1) <= (C->sp.p-(func+1)), "new top underflow");
        diff = n + 1;
    }
    newtop = C->sp.p + diff;
    if (diff < 0 && C->tbclist.p >= newtop) {
        cs_assert(hastocloseCfunc(cf->nresults));
        csF_close(C, newtop, CLOSEKTOP);
    }
    C->sp.p = newtop; /* set new top */
}


/* 
** Sets the stack top to `n` - 1 (n being the number of values to be on top).
** If new top is greater than the previous one, new values are elements
** are filled with `nil`.
** If n is 0, then all stack elements are removed.
** This function can run `__close` if it removes an index from the stack
** marked as to-be-closed.
*/
CS_API void cs_setntop(cs_State *C, int n) {
    cs_lock(C);
    setntop(C, n);
    cs_unlock(C);
}


/* 
** Return the index of the top element on the stack.
** This result is equal to the number of elements on the stack - 1.
** Negative value (-1) means an empty stack.
*/
CS_API int cs_gettop(const cs_State *C) {
    return cast_int(C->sp.p - (C->cf->func.p + 1) - 1);
}


/* 
** Convert `acceptable` stack index into an absolute index.
** For example, if there are 5 values on the stack then passing
** -1 as the value of `index` would be return 4. 
*/
CS_API int cs_absindex(cs_State *C, int index)
{
    return (index >= 0 || ispseudo(index))
            ? index
            : cast_int(C->sp.p - C->cf->func.p - 1) + index;
}


/* 
** Auxiliary to `cs_rotate`, reverses stack values `from` until `to`.
*/
c_sinline void rev(cs_State *C, SPtr from, SPtr to) {
    while (from < to) {
        TValue temp;
        setobj(C, &temp, s2v(from));
        setobjs2s(C, from, to);
        setobj2s(C, to, &temp);
        from++, to--;
    }
}


/*
** Stack-array rotation between the top of the stack and the `index` for `n`
** times elements. Negative `-n` indicates left-rotation, while positive `n`
** right-rotation. The absolute value of `n` must not be greater than the size
** of the slice being rotated.
** Note that `index` must be in stack.
**
** Example right-rotation:
** [func][0][1][2][3][4]
** cs_rotate(C, 2, 2);
** [func][0][1][3][4][2]
**
** Example left-rotation:
** [func][0][1][2][3][4]
** cs_rotate(C, 2, -2);
** [func][0][1][4][3][2]
**
** Example right-rotation:
** [nil][func]
** cs_rotate(C, 0, 1);
** [func][nil]
*/
CS_API void cs_rotate(cs_State *C, int index, int n) {
    SPtr start, end, pivot;
    cs_lock(C);
    end = C->sp.p - 1; /* end of segment */
    start = index2stack(C, index); /* start of segment */
    api_check(C, (n >= 0 ? n : -n) <= (end - start + 1), "invalid `n`");
    pivot = (n >= 0 ? end - n : start - n - 1); /* end of prefix */
    rev(C, start, pivot);
    rev(C, pivot + 1, end);
    rev(C, start, end);
    cs_unlock(C);
}


/* 
** Copy value at index `src` to stack slot at index `dest`.
** Note that `dest` must be stack index.
*/
CS_API void cs_copy(cs_State *C, int src, int dest) {
    TValue *from, *to;
    cs_lock(C);
    from = index2value(C, src);
    to = index2value(C, dest);
    api_check(C, isvalid(C, to), "invalid index");
    setobj(C, to, from);
    if (isupvalue(dest)) /* closure upvalue? */
        csG_barrier(C, clCval(s2v(C->cf->func.p)), from);
    cs_unlock(C);
}


/*
** Check if stack has enough space for `n` elements,
** if not ensure it does.
*/
CS_API int cs_checkstack(cs_State *C, int n) {
    CallFrame *cf;
    int res;
    cs_lock(C);
    cf = C->cf;
    api_check(C, n >= 0, "negative `n`");
    if (cf->top.p - C->sp.p >= n) /* stack large enough? */
        res = 1;
    else /* need to grow the stack */
        res = csT_growstack(C, n, 0);
    if (res && cf->top.p < C->sp.p + n)
        cf->top.p = C->sp.p + n; /* adjust top */
    cs_unlock(C);
    return res;
}


/* push without lock */
#define pushvalue(C, index) \
    { setobj2s(C, C->sp.p, index2value(C, index)); api_inctop(C); }


/*
** Push value at the index on to the top of the stack.
*/
CS_API void cs_push(cs_State *C, int index) {
    cs_lock(C);
    pushvalue(C, index);
    cs_unlock(C);
}


/*
** Exchange values between two threads of the same state.
** This function pops `n` values from `src` and pushes them
** onto the stack of `dest`.
*/
CS_API void cs_xmove(cs_State *src, cs_State *dest, int n) {
    if (src == dest) return; /* same thread ? */
    cs_lock(dest);
    api_checknelems(src, n); /* have enough elements to move? */
    api_check(src, G(src) == G(dest), "moving between different states");
    api_check(src, dest->cf->top.p - dest->sp.p >= n, "dest stack overflow");
    src->sp.p -= n;
    for (int i = 0; i < n; i++) {
        setobjs2s(dest, dest->sp.p, src->sp.p + i);
        dest->sp.p++; /* already checked by `api_check` */
    }
    cs_unlock(dest);
}


/* Check if the value at index is a number. */
CS_API int cs_is_number(cs_State *C, int index) {
    cs_Number n;
    UNUSED(n);
    const TValue *o = index2value(C, index);
    return tonumber(o, n);
}


/* Check if the value at index is an integer. */
CS_API int cs_is_integer(cs_State *C, int index) {
    cs_Integer i;
    UNUSED(i);
    const TValue *o = index2value(C, index);
    return tointeger(o, &i);
}


/* Check if the value at index is a string. */
CS_API int cs_is_string(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return ttisstring(o);
}


/* Check if the value at index is a C function. */
CS_API int cs_is_cfunction(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return (ttislcf(o) || ttisCclosure(o));
}


/* Check if the value at index is a userdata. */
CS_API int cs_is_userdata(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return (ttislightuserdata(o) || ttisfulluserdata(o));
}


c_sinline TValue *getvvmt(cs_State *C, const TValue *o) {
    switch (ttype(o)) {
        case CS_VINSTANCE: return insval(o)->oclass->vmt;
        case CS_TCLASS: return classval(o)->vmt;
        case CS_TUSERDATA: return uval(o)->vmt;
        default: return G(C)->vmt[ttype(o)];
    }
}


c_sinline TValue *getvmt(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return getvvmt(C, o);
}


/* 
** Return the type of the value at valid index or CS_TNONE
** if index is invalid.
** The types returned are defined in `cscript.h` (CS_T*).
*/
CS_API int cs_type(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return (isvalid(C, o) ? ttype(o) : CS_TNONE);
}


/*
** Return text representation of the given type.
** The returned string has a static lifetime and should not be modified
** directly.
*/
CS_API const char *cs_typename(cs_State *C, int type) {
    UNUSED(C);
    api_check(C, 0 <= type && type < CS_NUM_TYPES, "invalid type");
    return typename(type);
}


/*
** Returns the number value of the value at index.
** The fact whether the value was a number is stored in `pisnum` if
** provided, the default value returned when index is not a number is 0.0.
*/
CS_API cs_Number cs_to_numberx(cs_State *C, int index, int *pisnum) {
    cs_Number n = 0.0;
    const TValue *o = index2value(C, index);
    int isnum = tonumber(o, n);
    if (pisnum)
        *pisnum = isnum;
    return n;
}


/*
** Returns the integer value of the value at index.
** The fact whether the value was an integer is stored in `pisint` if
** provided, the default value returned when index is not an integer is 0.
*/
CS_API cs_Integer cs_to_integerx(cs_State *C, int index, int *pisint) {
    cs_Integer i = 0;
    const TValue *o = index2value(C, index);
    int isint = tointeger(o, &i);
    if (pisint)
        *pisint = isint;
    return i;
}


/*
** Returns 0 or 1 whether the value at index is false or true respectively.
** All values in CScript are considered true except `nil` and `false`.
*/
CS_API int cs_to_bool(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return !c_isfalse(o);
}


/*
** Return string value from value at index and if `plen` is provided
** set it to match the length of the string.
** If the value is not a string return NULL (in this case if `plen` is
** provided it is ignored).
*/
CS_API const char *cs_to_lstring(cs_State *C, int index, size_t *plen) {
    const TValue *o = index2value(C, index);
    if (!ttisstring(o)) { /* not a string? */
        return NULL;
    } else if (plen != NULL)
        *plen = getstrlen(strval(o)); 
    return getstr(strval(o));
}


/*
** Return `cs_CFunction` from the value at index.
** If the value is not a C closure or light C function, then this returns NULL.
*/
CS_API cs_CFunction cs_to_cfunction(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    if (ttislcf(o)) 
        return lcfval(o);
    else if (ttisCclosure(o))
        return clCval(o)->fn;
    else
        return NULL;
}


c_sinline void *touserdata(const TValue *o) {
    switch (ttypetag(o)) {
        case CS_VLIGHTUSERDATA: return pval(o);
        case CS_VUSERDATA: return getuserdatamem(uval(o));
        default: return NULL;
    }
}


/*
** Return pointer to userdata memory from the value at index.
** If the value is not userdata return NULL.
*/
CS_API void *cs_to_userdata(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return touserdata(o);
}


/*
** Return the pointer to the object at index.
** If the object is not userdata, C function or collectable, then this
** returns NULL. Note that returned pointer shouldn't be modified.
*/
CS_API const void *cs_to_pointer(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case CS_VLCF: {
            return cast(void *, cast_sizet(lcfval(o)));
        }
        case CS_VUSERDATA: case CS_VLIGHTUSERDATA: {
            return touserdata(o);
        }
        default: {
            if (iscollectable(o))
                return gcoval(o);
            else
                return NULL;
        }
    }
}


/*
** Return the thread value at index.
** If the value is not a thread, then this returns NULL.
*/
CS_API cs_State *cs_to_thread(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    return (ttisthread(o) ? thval(o) : NULL);
}


/*
** Perform arithmetic operation `op`; the valid arithmetic operations are
** located in `cscript.h`.
** This functions is free to call overloadable methods.
*/
CS_API void cs_arith(cs_State *C, int op) {
    cs_lock(C);
    if (op != CS_OPUNM && op != CS_OPBNOT) { /* binary op? */
        api_checknelems(C, 2);
        csV_binarithm(C, s2v(C->sp.p-2), s2v(C->sp.p-1), C->sp.p-2, op);
        C->sp.p--; /* pop second operand */
    } else { /* unary op */
        api_checknelems(C, 1);
        csV_unarithm(C, s2v(C->sp.p-1), C->sp.p-1, op);
    }
    cs_unlock(C);
}


/*
** Perform raw equality between values at `index1` and `index2`.
** `raw` meaning this function won't call overloaded methods.
** In cases where either of the indexes is not valid this always returns 0.
*/
CS_API int cs_rawequal(cs_State *C, int index1, int index2) {
    const TValue *lhs = index2value(C, index1);
    const TValue *rhs = index2value(C, index2);
    return (isvalid(C, lhs) && isvalid(C, rhs) ? csV_raweq(lhs, rhs) : 0);
}


/*
** Compares values at `index1` and `index2`.
** This function is free to call overloded methods.
** In case either index is invalid this then always returns 0.
*/
CS_API int cs_compare(cs_State *C, int index1, int index2, int op) {
    const TValue *lhs;
    const TValue *rhs;
    int res = 0;
    cs_lock(C); /* might call overloaded method */
    lhs = index2value(C, index1);
    rhs = index2value(C, index2);
    if (isvalid(C, lhs) && isvalid(C, rhs)) {
        switch (op) {
            case CS_OPEQ: res = csV_ordereq(C, lhs, rhs); break;
            case CS_OPLT: res = csV_orderlt(C, lhs, rhs); break;
            case CS_OPLE: res = csV_orderle(C, lhs, rhs); break;
            default: api_check(C, 0, "invalid `op`"); break;
        }
    }
    cs_unlock(C);
    return res;
}


/* Push nil value on top of the stack. */
CS_API void cs_push_nil(cs_State *C) {
    cs_lock(C);
    setnilval(s2v(C->sp.p));
    api_inctop(C);
    cs_unlock(C);
}


/* Push `cs_Number` value on top of the stack. */
CS_API void cs_push_number(cs_State *C, cs_Number n) {
    cs_lock(C);
    setfval(s2v(C->sp.p), n);
    api_inctop(C);
    cs_unlock(C);
}


/* Push `cs_Integer` value on top of the stack. */
CS_API void cs_push_integer(cs_State *C, cs_Integer i) {
    cs_lock(C);
    setival(s2v(C->sp.p), i);
    api_inctop(C);
    cs_unlock(C);
}


/* Push string value of length `len` on top of the stack. */
CS_API const char *cs_push_lstring(cs_State *C, const char *str, size_t len) {
    OString *s;
    cs_lock(C);
    s = (len == 0 ? csS_new(C, "") : csS_newl(C, str, len));
    setstrval2s(C, C->sp.p, s);
    api_inctop(C);
    csG_checkGC(C);
    cs_unlock(C);
    return getstr(s);
}


/* Push null terminated string value on top of the stack. */
CS_API const char *cs_push_string(cs_State *C, const char *str) {
    cs_lock(C);
    if (str == NULL) {
        setnilval(s2v(C->sp.p));
    } else {
        OString *s = csS_new(C, str);
        setstrval2s(C, C->sp.p, s);
        str = getstr(s);
    }
    api_inctop(C);
    csG_checkGC(C);
    cs_unlock(C);
    return str;
}


/* 
** Push formatted string with format values in a `va_list` on top of the stack.
** Valid format values are c (char), d (int), I (cs_Integer), N (cs_Number),
** s (string), p (pointer) and % (`%`). Note that each format specifier is
** preceeded by `%` (so %I...).
*/
CS_API const char *cs_push_vfstring(cs_State *C, const char *fmt, va_list argp) {
    const char *str;
    cs_lock(C);
    str = csS_pushvfstring(C, fmt, argp);
    csG_checkGC(C);
    cs_unlock(C);
    return str;
}


/* 
** Push formatted string with variable amount of format values on top of the
** stack. Valid format specifiers are the same as in `cs_pushvfstring`.
*/
CS_API const char *cs_push_fstring(cs_State *C, const char *fmt, ...) {
    const char *str;
    va_list argp;
    cs_lock(C);
    va_start(argp, fmt);
    str = csS_pushvfstring(C, fmt, argp);
    va_end(argp);
    csG_checkGC(C);
    cs_unlock(C);
    return str;
}


/* pushes C closure without locking */
c_sinline void pushcclosure(cs_State *C, cs_CFunction fn, int nupvals) {
    if (nupvals == 0) {
        setcfval(C, s2v(C->sp.p), fn);
        api_inctop(C);
    } else {
        CClosure *cl;
        api_checknelems(C, nupvals);
        cl = csF_newCClosure(C, nupvals);
        cl->fn = fn;
        C->sp.p -= nupvals;
        while (nupvals--) {
            setobj(C, &cl->upvals[nupvals], s2v(C->sp.p + nupvals));
            cs_assert(iswhite(cl));
        }
        setclCval(C, s2v(C->sp.p), cl);
        api_inctop(C);
        csG_checkGC(C);
    }
}


/*
** Push C closure value on top of the stack.
** This closure will have `nupvals` upvalues, these values will be popped
** of the stack and inserted into the closure.
** If `nupvals` is 0 then this does not create a C closure, rather it only
** pushes the `cs_CFunction` on top of the stack.
*/
CS_API void cs_push_cclosure(cs_State *C, cs_CFunction fn, int nupvals) {
    cs_lock(C);
    pushcclosure(C, fn, nupvals);
    cs_unlock(C);
}


/*
** Push boolean value on top of the stack.
** If `b` is 0 then `false` is pushed, otherwise `true`.
*/
CS_API void cs_push_bool(cs_State *C, int b) {
    cs_lock(C);
    if (b)
        setbtval(s2v(C->sp.p));
    else
        setbfval(s2v(C->sp.p));
    api_inctop(C);
    cs_unlock(C);
}


/* Push light userdata on top of the stack. */
CS_API void cs_push_lightuserdata(cs_State *C, void *p) {
    cs_lock(C);
    setpval(s2v(C->sp.p), p);
    api_inctop(C);
    cs_unlock(C);
}


/* Push array on top of the stack. */
CS_API void cs_push_array(cs_State *C, int sz) {
    Array *arr;
    cs_lock(C);
    arr = csA_new(C);
    setarrval2s(C, C->sp.p, arr);
    api_inctop(C);
    if (sz > 0)
        csA_ensure(C, arr, sz - 1);
    csG_checkGC(C);
    cs_unlock(C);
}


/* Push table on top of the stack. */
CS_API void cs_push_table(cs_State *C, int sz) {
    Table *ht;
    cs_lock(C);
    ht = csH_new(C);
    settval2s(C, C->sp.p, ht);
    api_inctop(C);
    if (sz > 0)
        csH_resize(C, ht, sz);
    csG_checkGC(C);
    cs_unlock(C);
}


/*
** Push thread `C` on top of the stack.
** Returns non-zero if the pushed thread is main thread.
*/
CS_API int cs_push_thread(cs_State *C) {
    cs_lock(C);
    setthval2s(C, C->sp.p, C);
    api_inctop(C);
    cs_unlock(C);
    return (G(C)->mainthread == C);
}


CS_API void cs_push_instance(cs_State *C, int index) {
    const TValue *o;
    SPtr func;
    cs_lock(C);
    o = index2value(C, index);
    api_check(C, ttisclass(o), "expect class");
    func = C->sp.p;
    setclsval2s(C, func, classval(o));
    api_inctop(C);
    csV_call(C, func, 1);
    cs_assert(ttisinstance(s2v(C->sp.p))); /* result is the instance */
    csG_checkGC(C);
    cs_unlock(C);
}


c_sinline void auxsetvmt(TValue *dest, const cs_VMT *vmt) {
    for (int i = 0; i < CS_MM_N; i++) {
        if (vmt->func[i]) {
            setcfval(C, &dest[i], vmt->func[i]);
        } else
            setnilval(&dest[i]);
    }
}


#define fastget(C,ht,k,slot,f)     ((slot = f(ht, k)), !isempty(slot))

#define finishfastset(C,ht,slot,v) \
    { setobj(C, cast(TValue *, slot), v); \
      csG_barrierback(C, obj2gco(ht), v); }


c_sinline void auxrawsetstr(cs_State *C, Table *ht, const char *str,
                             const TValue *v) {
    const TValue *slot;
    OString *s = csS_new(C, str);
    if (fastget(C, ht, s, slot, csH_getstr)) {
        finishfastset(C, ht, slot, v);
        C->sp.p--; /* pop value */
    } else {
        setstrval2s(C, C->sp.p, s);
        api_inctop(C);
        csH_set(C, ht, s2v(C->sp.p - 1), v);
        csG_barrierback(C, obj2gco(ht), v);
        C->sp.p -= 2; /* pop string key and value */
    }
    cs_unlock(C);
}


c_sinline void auxsetentrylist(cs_State *C, OClass *cls, const cs_Entry *l,
                                int nup) {
    cs_assert(l != NULL);
    cs_checkstack(C, nup);
    if (l->name) { /* have at least one entry? */
        cs_assert(cls->methods == NULL);
        cls->methods = csH_new(C);
        csG_checkGC(C);
        do {
            api_check(C, l->func, "l->func is NULL");
            for (int i = 0; i < nup; i++) /* push upvalues to the top */
                pushvalue(C, -nup);
            pushcclosure(C, l->func, nup);
            auxrawsetstr(C, cls->methods, l->name, s2v(C->sp.p - 1));
        } while (l++, l->name);
    }
    setntop(C, -(nup - 1)); /* pop upvalues */
}


CS_API void cs_push_class(cs_State *C, const cs_VMT *vmt, int abscls,
                          int nup, const cs_Entry *l) {
    OClass *cls;
    cs_lock(C);
    cls = csMM_newclass(C);
    csG_checkGC(C);
    setclsval2s(C, C->sp.p, cls);
    api_inctop(C);
    if (abscls >= 0) { /* have superclass? */
        const TValue *osup = index2value(C, abscls);
        api_check(C, ttisclass(osup), "expect class");
        if (classval(osup)->methods) { /* have methods? */
            cls->methods = csH_new(C);
            csH_copykeys(C, classval(osup)->methods, cls->methods);
        }
    }
    if (vmt) { /* have virtual method table? */
        cls->vmt = csMM_newvmt(C);
        csG_checkGC(C);
        auxsetvmt(cls->vmt, vmt);
    }
    if (l) /* have methods? */
        auxsetentrylist(C, cls, l, nup);
    else /* otherwise there should be no upvalues */
        api_check(C, nup == 0, "nup non-zero but l is NULL");
    cs_unlock(C);
}



/* CScript -> stack */

c_sinline int finishrawgetfield(cs_State *C, const TValue *val) {
    if (isempty(val))
        setnilval(s2v(C->sp.p));
    else
        setobj2s(C, C->sp.p, val);
    api_inctop(C);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


c_sinline int auxrawgetfieldstr(cs_State *C, Table *ht, const char *k) {
    const TValue *slot;
    OString *str = csS_new(C, k);
    if (fastget(C, ht, str, slot, csH_getstr)) {
        setobj2s(C, C->sp.p, slot);
    } else
        setnilval(s2v(C->sp.p));
    api_inctop(C);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


/*
** Gets the global variable `name` value and pushes it on top of the stack.
** This function returns the value type.
*/
CS_API int cs_get_global(cs_State *C, const char *name) {
    TValue *gt;
    cs_lock(C);
    gt = getGtable(C);
    return auxrawgetfieldstr(C, tval(gt), name);
}


CS_API int cs_get(cs_State *C, int obj) {
    const TValue *o;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, obj);
    csV_get(C, o, s2v(C->sp.p - 1), C->sp.p - 1);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


CS_API int cs_get_raw(cs_State *C, int obj) {
    const TValue *o;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, obj);
    csV_rawget(C, o, s2v(C->sp.p - 1), C->sp.p - 1);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


c_sinline Array *getarray(cs_State *C, int arrobj) {
    const TValue *o = index2value(C, arrobj);
    api_check(C, ttisarr(o), "expect array");
    return arrval(o);
}


CS_API int cs_get_index(cs_State *C, int index, cs_Integer i) {
    Array *arr;
    cs_lock(C);
    api_check(C, i >= 0, "invalid `index`");
    arr = getarray(C, index);
    if (c_castS2U(i) < arr->n) {
        setobj2s(C, C->sp.p, &arr->b[i]);
    } else
        setnilval(s2v(C->sp.p));
    api_inctop(C);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


static int auxgetindex(cs_State *C, int index, int begin, int end, int nn) {
    Array *arr = getarray(C, index);
    uint len = (arr->n <= cast_uint(end) ? arr->n : cast_uint(end) + 1);
    api_check(C, begin >= 0, "invalid 'begin' index");
    for (uint i = begin; i < len; i++)
        if (!isempty(&arr->b[i]) == nn)
            return i;
    return -1;
}


CS_API int cs_get_nilindex(cs_State *C, int index, int begin, int end) {
    return auxgetindex(C, index, begin, end, 0);
}


CS_API int cs_get_nnilindex(cs_State *C, int index, int begin, int end) {
    return auxgetindex(C, index, begin, end, 1);
}


c_sinline Table *gettable(cs_State *C, int obj) {
    const TValue *o = index2value(C, obj);
    switch (ttypetag(o)) {
        case CS_VINSTANCE: return insval(o)->fields;
        case CS_VTABLE: return tval(o); 
        default:  {
            api_check(C, 0, "expect instance or table");
            return NULL;
        }
    }
}


CS_API int cs_get_field(cs_State *C, int obj) {
    Table *ht;
    const TValue *val;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    ht = gettable(C, obj);
    val = csH_get(ht, s2v(C->sp.p - 1));
    C->sp.p--; /* remove key */
    return finishrawgetfield(C, val);
}


CS_API int cs_get_fieldstr(cs_State *C, int obj, const char *field) {
    Table *ht;
    cs_lock(C);
    ht = gettable(C, obj);
    return auxrawgetfieldstr(C, ht, field);
}


CS_API int cs_get_fieldptr(cs_State *C, int obj, const void *field) {
    Table *ht;
    TValue aux;
    cs_lock(C);
    ht = gettable(C, obj);
    setpval(&aux, cast_voidp(field));
    return finishrawgetfield(C, csH_get(ht, &aux));
}


CS_API int cs_get_fieldint(cs_State *C, int obj, cs_Integer i) {
    Table *ht;
    cs_lock(C);
    ht = gettable(C, obj);
    return finishrawgetfield(C, csH_getint(ht, i));
}


CS_API int cs_get_fieldflt(cs_State *C, int obj, cs_Number n) {
    Table *ht;
    TValue aux;
    cs_lock(C);
    ht = gettable(C, obj);
    setfval(&aux, n);
    return finishrawgetfield(C, csH_get(ht, &aux));
}


CS_API int cs_get_class(cs_State *C, int insobj) {
    const TValue *o;
    int tt;
    cs_lock(C);
    o = index2value(C, insobj);
    if (ttisinstance(o)) {
        setclsval2s(C, C->sp.p, insval(o)->oclass);
        api_inctop(C);
        tt = CS_TCLASS;
    } else {
        tt = CS_TNONE;
    }
    cs_unlock(C);
    return tt;
}


c_sinline Instance *getinstance(cs_State *C, int insobj) {
    const TValue *o = index2value(C, insobj);
    api_check(C, ttisinstance(o), "expect instance");
    return insval(o);
}


CS_API int cs_get_method(cs_State *C, int insobj) {
    Instance *ins;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    ins = getinstance(C, insobj);
    if (ins->oclass->methods) { /* have methods ? */
        const TValue *slot = csH_get(ins->oclass->methods, s2v(C->sp.p - 1));
        if (!isempty(slot)) { /* found? */
            IMethod *im = csMM_newinsmethod(C, ins, slot);
            setimval2s(C, C->sp.p - 1, im);
            goto unlock; /* done */
        } /* else fallthrough */
    } /* else fall through */
    setnilval(s2v(C->sp.p - 1));
unlock:
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


CS_API int cs_get_metamethod(cs_State *C, int obj, cs_MM mm) {
    TValue *vmt;
    int tt;
    cs_lock(C);
    vmt = getvmt(C, obj);
    if (vmt) {
        setobj2s(C, C->sp.p, &vmt[mm]);
        api_inctop(C);
        tt = ttype(s2v(C->sp.p - 1)); 
    } else {
        tt = CS_TNONE;
    }
    cs_unlock(C);
    return tt;
}


CS_API void *cs_newuserdata(cs_State *C, size_t sz, int nuv) {
    UserData *ud;
    cs_lock(C);
    api_check(C, 0 <= nuv && nuv < USHRT_MAX, "invalid value");
    ud = csMM_newuserdata(C, sz, nuv);
    setuval2s(C, C->sp.p, ud);
    api_inctop(C);
    cs_unlock(C);
    return getuserdatamem(ud);
}


c_sinline UserData *getuserdata(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttisfulluserdata(o), "expect full userdata");
    return uval(o);
}


CS_API int cs_get_uservmt(cs_State *C, int udobj, cs_VMT *pvmt) {
    UserData *ud;
    int nmm = 0;
    cs_lock(C);
    api_check(C, pvmt != NULL, "`pvmt` is NULL");
    ud = getuserdata(C, udobj);
    if (!ud->vmt) {
        for (int i = 0; i < CS_MM_N; i++)
            pvmt->func[i] = NULL;
    } else {
        for (int i = 0; i < CS_MM_N; i++) {
            if (!isempty(&ud->vmt[i])) {
                cs_assert(ttislcf(&ud->vmt[i]));
                pvmt->func[i] = lcfval(&ud->vmt[i]);
                nmm++;
            }
        }
    }
    cs_unlock(C);
    return nmm;
}


CS_API int cs_get_uservalue(cs_State *C, int udobj, int n) {
    UserData *ud;
    int tt;
    cs_lock(C);
    ud = getuserdata(C, udobj);
    if (n <= 0 || ud->nuv < n) {
        setnilval(s2v(C->sp.p));
        tt = CS_TNONE;
    } else {
        setobj2s(C, C->sp.p, &ud->uv[n - 1].val);
        tt = ttype(s2v(C->sp.p - 1));
    }
    api_inctop(C);
    cs_unlock(C);
    return tt;
}


CS_API void cs_set_global(cs_State *C, const char *name) {
    TValue *gt;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    gt = getGtable(C);
    auxrawsetstr(C, tval(gt), name, s2v(C->sp.p - 1));
}


CS_API void cs_set(cs_State *C, int obj) {
    TValue *o;
    cs_lock(C);
    api_checknelems(C, 2); /* value and key */
    o = index2value(C, obj);
    csV_set(C, o, s2v(C->sp.p - 1), s2v(C->sp.p - 2));
    C->sp.p -= 2; /* remove value and key */
    cs_unlock(C);
}


CS_API void  cs_set_raw(cs_State *C, int obj) {
    TValue *o;
    cs_lock(C);
    api_checknelems(C, 2); /* value and key */
    o = index2value(C, obj);
    csV_rawset(C, o, s2v(C->sp.p - 1), s2v(C->sp.p - 2));
    C->sp.p -= 2; /* remove value and key */
    cs_unlock(C);
}


CS_API void cs_set_index(cs_State *C, int index, int i) {
    Array *arr;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    api_check(C, 0 <= i, "`index` out of bounds");
    arr = getarray(C, index);
    csA_ensure(C, arr, i);
    setobj(C, &arr->b[i], s2v(C->sp.p - 1));
    csG_barrierback(C, obj2gco(arr), s2v(C->sp.p - 1));
    C->sp.p--; /* remove value */
    cs_unlock(C);
}


c_sinline void auxrawsetfield(cs_State *C, int obj, TValue *key, int n) {
    Table *ht;
    cs_lock(C);
    api_checknelems(C, n);
    ht = gettable(C, obj);
    csH_set(C, ht, key, s2v(C->sp.p - 1));
    csG_barrierback(C, obj2gco(ht), s2v(C->sp.p - 1));
    C->sp.p -= n;
    cs_unlock(C);
}


CS_API void cs_set_field(cs_State *C, int obj) {
    auxrawsetfield(C, obj, s2v(C->sp.p - 2), 2);
}


CS_API void cs_set_fieldstr(cs_State *C, int index, const char *field) {
    Table *ht;
    cs_lock(C);
    api_checknelems(C, 1);
    ht = gettable(C, index);
    auxrawsetstr(C, ht, field, s2v(C->sp.p - 1));
}


CS_API void cs_set_fieldptr(cs_State *C, int obj, const void *field) {
    TValue key;
    setpval(&key, cast_voidp(field));
    auxrawsetfield(C, obj, &key, 1);
}


CS_API void  cs_set_fieldint(cs_State *C, int obj, cs_Integer field) {
    TValue key;
    setival(&key, field);
    auxrawsetfield(C, obj, &key, 1);
}


CS_API void  cs_set_fieldflt(cs_State *C, int obj, cs_Number field) {
    TValue key;
    setfval(&key, field);
    auxrawsetfield(C, obj, &key, 1);
}


CS_API void cs_set_uservmt(cs_State *C, int udobj, const cs_VMT *vmt) {
    UserData *ud;
    cs_lock(C);
    ud = getuserdata(C, udobj);
    if (!ud->vmt) {
        ud->vmt = csMM_newvmt(C);
        csG_checkGC(C);
        if (!vmt) goto unlock;
    }
    if (vmt) {
        auxsetvmt(ud->vmt, vmt);
        csG_checkfin(C, obj2gco(ud), ud->vmt);
    } else {
        for (int i = 0; i < CS_MM_N; i++)
            setnilval(&ud->vmt[i]);
    }
unlock:
    cs_unlock(C);

}


CS_API int cs_set_uservalue(cs_State *C, int index, int n) {
    UserData *ud;
    int res;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    ud = getuserdata(C, index);
    if (!(cast_uint(n) <= cast_uint(ud->nuv))) {
        res = 0; /* `n` not in [0, ud->nuv) */
    } else {
        setobj(C, &ud->uv[n - 1].val, s2v(C->sp.p - 1));
        csG_barrierback(C, obj2gco(ud), s2v(C->sp.p - 1));
        res = 1;
    }
    C->sp.p--; /* remove value */
    cs_unlock(C);
    return res;
}


CS_API void cs_set_usermm(cs_State *C, int index, cs_MM mm) {
    UserData *ud;
    cs_lock(C);
    api_checknelems(C, 1); /* metamethod func */
    ud = getuserdata(C, index);
    if (!ud->vmt) {
        ud->vmt = csMM_newvmt(C);
        csG_checkGC(C);
    }
    setobj(C, &ud->vmt[mm], s2v(C->sp.p - 1));
    csG_barrierback(C, obj2gco(ud), s2v(C->sp.p - 1));
    csG_checkfin(C, obj2gco(ud), ud->vmt);
    C->sp.p--;
    cs_unlock(C);
}



/* Error reporting */


CS_API int cs_status(cs_State *C) {
    return C->status;
}


CS_API int cs_error(cs_State *C) {
    TValue *errobj;
    cs_lock(C);
    api_checknelems(C, 1); /* errobj */
    errobj = s2v(C->sp.p - 1);
    if (ttisstring(errobj) && strval(errobj) == G(C)->memerror) {
        csM_error(C); /* raise a memory error */
    } else
        csD_errormsg(C);
    /* cs_unlock() is called when control leaves the core */
    cs_assert(0);
}



/* Call/Load CScript code */

#define checkresults(C,nargs,nres) \
     api_check(C, (nres) == CS_MULRET \
               || (C->cf->top.p - C->sp.p >= (nres) - (nargs)), \
	"results from function overflow current stack size")


CS_API void cs_call(cs_State *C, int nargs, int nresults) {
    SPtr func;
    cs_lock(C);
    api_checknelems(C, nargs + 1); /* args + func */
    api_check(C, C->status == CS_OK, "can't do calls on non-normal thread");
    checkresults(C, nargs, nresults);
    func = C->sp.p - nargs - 1;
    csV_call(C, func, nresults);
    adjustresults(C, nresults);
    cs_unlock(C);
}


struct PCallData {
    SPtr func;
    int nresults;
};


static void fcall(cs_State *C, void *ud) {
    struct PCallData *pcd = cast(struct PCallData*, ud);
    csV_call(C, pcd->func, pcd->nresults);
}


CS_API int cs_pcall(cs_State *C, int nargs, int nresults, int absmsgh) {
    struct PCallData pcd;
    int status;
    ptrdiff_t func;
    cs_lock(C);
    api_checknelems(C, nargs+1); /* args + func */
    api_check(C, C->status == CS_OK, "can't do calls on non-normal thread");
    checkresults(C, nargs, nresults);
    if (absmsgh < 0) {
        func = 0;
    } else {
        SPtr o = index2stack(C, absmsgh);
        api_check(C, ttisfunction(s2v(o)), "error handler must be a function");
        func = savestack(C, o);
    }
    pcd.func = C->sp.p - nargs - 1;
    pcd.nresults = nresults;
    status = csPR_call(C, fcall, &pcd, savestack(C, pcd.func), func);
    adjustresults(C, nresults);
    cs_unlock(C);
    return status;
}


CS_API int cs_load(cs_State *C, cs_Reader reader, void *userdata,
                    const char *source) {
    BuffReader br;
    int status;
    cs_lock(C);
    if (!source) source = "?";
    csR_init(C, &br, reader, userdata);
    status = csPR_parse(C, &br, source);
    cs_unlock(C);
    return status;
}


CS_API int cs_gc(cs_State *C, int option, ...) {
    va_list ap;
    int res = 0;
    GState *gs = G(C);
    cs_lock(C);
    va_start(ap, option);
    switch (option) {
	case CS_GCSTOP: { /* stop garbage collector */
            gs->gcstop = GCSTPUSR; /* stopped by user */
            break;
        }
	case CS_GCRESTART: { /* restart GC */
            csG_setgcdebt(gs, 0);
            gs->gcstop = 0;
            break;
        }
	case CS_GCCOLLECT: { /* start GC cycle */
            csG_full(C, 0);
            break;
        }
	case CS_GCCOUNT: { /* total GC memory count in Kibibytes */
            res = gettotalbytes(gs) >> 10;
            break;
        }
	case CS_GCCOUNTBYTES: { /* remainder bytes of total memory / 1024 */
            res = gettotalbytes(gs) & 0x3FF; /* all bits before 10th bit */
            break;
        }
	case CS_GCSTEP: { /* perform GC step */
            int data = va_arg(ap, int); /* kibibytes */
            c_smem gcdebt = 69; /* >0 to signal that it did an actual step */
            c_byte old_gcstop = gs->gcstop;
            if (data == 0) { /* do a regular step ? */
                csG_setgcdebt(gs, 0);
                csG_step(C);
            } else { /* add `data` to total gcdebt */
                /* convert `data` to bytes (data = bytes/2^10) */
                gcdebt = (data * 1024) + gs->gcdebt;
                csG_setgcdebt(gs, gcdebt);
                csG_checkGC(C);
            }
            gs->gcstop = old_gcstop;
            if (gcdebt > 0 && gs->gcstate == GCSpause) /* end of cycle? */
                res = 1; /* signal it */
            break;
        }
	case CS_GCISRUNNING: { /* check if GC is running */
            res = gcrunning(gs);
            break;
        }
        case CS_GCINC: {
            int pause = va_arg(ap, int);
            int stepmul = va_arg(ap, int);
            int stepsize = va_arg(ap, int);
            res = CS_GCINC;
            if (pause != 0)
                setgcparam(gs->gcpause, pause);
            if (stepmul != 0)
                setgcparam(gs->gcstepmul, stepmul);
            if (stepsize != 0)
                gs->gcstepsize = stepsize;
            csG_incmode(C);
            break;
        }
        default: res = -1; /* invalid option */
    }
    va_end(ap);
    cs_unlock(C);
    return res;
}


CS_API void cs_setwarnf(cs_State *C, cs_WarnFunction fwarn, void *ud) {
    cs_lock(C);
    G(C)->fwarn = fwarn;
    G(C)->ud_warn = ud;
    cs_unlock(C);
}


CS_API void cs_warning(cs_State *C, const char *msg, int cont) {
    cs_lock(C);
    csT_warning(C, msg, cont);
    cs_unlock(C);
}


/* Check if the value at the index has virtual method table. */
CS_API int cs_hasvmt(cs_State *C, int index) {
    return getvmt(C, index) != NULL;
}


/* Check if the value at the index has meta method. */
CS_API int cs_hasmetamethod(cs_State *C, int index, cs_MM mm) {
    TValue *vmt = getvmt(C, index);
    return (vmt ? !ttisnil(&vmt[mm]) : 0);
}


/*
** Return the length of the value at index.
** Length means different things depending on the type of the value at index.
** For strings, this is the string length; for classes, this is the number
** of methods; for instances, this is the number of fields; for userdata, this
** is the size of the block of memory allocated for userdata.
*/
CS_API cs_Unsigned cs_len(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case CS_VSHRSTR: return strval(o)->shrlen;
        case CS_VLNGSTR: return strval(o)->u.lnglen;
        case CS_VARRAY: return arrval(o)->n;
        case CS_VTABLE: return csH_len(tval(o));
        case CS_VCLASS: {
            Table *t = classval(o)->methods;
            return (t ? csH_len(classval(o)->methods) : 0);
        }
        case CS_VINSTANCE: return csH_len(insval(o)->fields);
        case CS_VUSERDATA: return uval(o)->size;
        default: return 0;
    }
}


CS_API int cs_next(cs_State *C, int obj) {
    Table *ht;
    int more;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    ht = gettable(C, obj);
    more = csH_next(C, ht, C->sp.p - 1);
    if (more) {
        api_inctop(C);
    } else
        C->sp.p--; /* remove key */
    cs_unlock(C);
    return more;
}


CS_API void cs_concat(cs_State *C, int n) {
    cs_lock(C);
    api_checknelems(C, n);
    if (n > 0)
        csV_concat(C, n);
    else { /* nothing to concatenate */
        setstrval2s(C, C->sp.p, csS_newl(C, "", 0));
        api_inctop(C);
    }
    csG_checkGC(C);
    cs_unlock(C);
}


CS_API size_t cs_stringtonumber(cs_State *C, const char *s, int *f) {
    size_t sz = csS_tonum(s, s2v(C->sp.p), f);
    if (sz != 0) /* no conversion errors? */
        api_inctop(C);
    return sz;
}


CS_API cs_Alloc cs_getallocf(cs_State *C, void **ud) {
    cs_Alloc falloc;
    cs_lock(C);
    if (ud) *ud = G(C)->ud_alloc;
    falloc = G(C)->falloc;
    cs_unlock(C);
    return falloc;
}


CS_API void cs_setallocf(cs_State *C, cs_Alloc falloc, void *ud) {
    cs_lock(C);
    G(C)->falloc = falloc;
    G(C)->ud_alloc = ud;
    cs_unlock(C);
}


CS_API void cs_toclose(cs_State *C, int index) {
    SPtr o;
    int nresults;
    cs_lock(C);
    o = index2stack(C, index);
    api_check(C, C->tbclist.p < o,
                  "given level below or equal to the last marked slot");
    csF_newtbcvar(C, o); /* create new to-be-closed upvalue */
    nresults = C->cf->nresults;
    if (!hastocloseCfunc(nresults)) /* function not yet marked? */
        C->cf->nresults = codeNresults(nresults); /* mark it */
    cs_assert(hastocloseCfunc(C->cf->nresults)); /* must be marked */
    cs_unlock(C);
}


CS_API void cs_closeslot(cs_State *C, int index) {
    SPtr level;
    cs_lock(C);
    level = index2stack(C, index);
    api_check(C, hastocloseCfunc(C->cf->nresults) && C->tbclist.p == level,
                  "no variable to close at the given level");
    csF_close(C, level, CLOSEKTOP);
    setnilval(s2v(level)); /* closed */
    cs_unlock(C);
}


/*
** Sets `frame` in `cs_Debug`; `level` is `CallFrame` level.
** To traverse the call stack backwards (up), then level should be
** greater than 0. For example if you wish for currently active `CallFrame`,
** then `level` should be 0, if `level` is 1 then the `CallFrame` of the
** function that called the current function is considered.
** If `level` is found, therefore `cf` is set, then this function returns 1,
** otherwise 0.
*/
CS_API int cs_getstack(cs_State *C, int level, cs_Debug *ar) {
    int status;
    CallFrame *cf;
    if (level < 0) return 0; /* invalid (negative) level */
    cs_lock(C);
    for (cf = C->cf; level > 0 && cf != &C->basecf; cf = cf->prev)
        level--;
    if (level == 0 && cf != &C->basecf) { /* level found ? */
        ar->cf = cf;
        status = 1;
    } else /* otherwise no such level */
        status = 0;
    cs_unlock(C);
    return status;
}


static const char *auxupvalue(TValue *func, int n, TValue **val,
                               GCObject **owner) {
    switch (ttypetag(func)) {
        case CS_VCCL: { /* C closure */
            CClosure *ccl = clCval(func);
            if (!(cast_uint(n) - 1u < cast_uint(ccl->nupvalues)))
                return NULL;  /* `n` not in [0, cl->nupvalues) */
            *val = &ccl->upvals[n-1];
            if (owner) *owner = obj2gco(ccl);
            return "";
        }
        case CS_VCSCL: { /* CScript closure */
            OString *name;
            CSClosure *cl = clCSval(func);
            Proto *p = cl->p;
            if (!(cast_uint(n) - 1u < cast_uint(p->sizeupvals)))
                return NULL;  /* `n` not in [0, fn->sizeupvals) */
            *val = cl->upvals[n-1]->v.p;
            if (owner) *owner = obj2gco(cl->upvals[n]);
            name = p->upvals[n-1].name;
            cs_assert(name != NULL); /* must have debug information */
            return getstr(name);
        }
        default: return NULL; /* not a closure */
    }
}


CS_API const char *cs_getupvalue(cs_State *C, int index, int n) {
    const char *name;
    TValue *upval = NULL;
    cs_lock(C);
    name = auxupvalue(index2value(C, index), n, &upval, NULL);
    if (name) { /* have upvalue ? */
        setobj2s(C, C->sp.p, upval);
        api_inctop(C);
    }
    cs_unlock(C);
    return name;
}


CS_API const char *cs_setupvalue(cs_State *C, int index, int n) {
    const char *name;
    TValue *upval = NULL;
    GCObject *owner = NULL;
    TValue *func;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    func = index2value(C, index);
    name = auxupvalue(func, n, &upval, &owner);
    if (name) { /* found upvalue ? */
        setobj(C, upval, s2v(--C->sp.p));
        csG_barrier(C, owner, upval);
    }
    cs_unlock(C);
    return name;
}
