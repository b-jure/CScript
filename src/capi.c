/*
** capi.c
** CScript API
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "clist.h"
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


/* test for pseudo index */
#define ispseudo(i)		((i) <= CS_REGISTRYINDEX)

/* test for upvalue */
#define isupvalue(i)		((i) < CS_REGISTRYINDEX)

/* test for valid index */
#define isvalid(C,o)           (!isempty(o) || (o) != &G(C)->nil)


/* 
** Convert index to a pointer to its value.
** Invalid indices (using upvalue index for CScript functions) return
** special nil value '&G(C)->nil'.
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
        index = (CS_REGISTRYINDEX - index) - 1;
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


c_sinline void settop(cs_State *C, int n) {
    CallFrame *cf;
    SPtr func, newtop;
    ptrdiff_t diff;
    cf = C->cf;
    func = cf->func.p;
    if (n >= 0) {
        api_check(C, n <= (cf->top.p - func + 1), "new top too large");
        diff = ((func + 1) + n) - C->sp.p;
        for (; diff > 0; diff--)
            setnilval(s2v(C->sp.p++));
    } else { /* negative index */
        api_check(C, -(n+1) <= (C->sp.p-(func+1)), "new top underflow");
        diff = n + 1;
    }
    api_check(C, C->tbclist.p < C->sp.p, "previous pop of an unclosed slot");
    newtop = C->sp.p + diff;
    if (diff < 0 && C->tbclist.p >= newtop) {
        cs_assert(hastocloseCfunc(cf->nresults));
        newtop = csF_close(C, newtop, CLOSEKTOP);
    }
    C->sp.p = newtop; /* set new top */
}


CS_API void cs_settop(cs_State *C, int n) {
    cs_lock(C);
    settop(C, n);
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
    api_check(C, n >= 0, "negative 'n'");
    if (C->stackend.p - C->sp.p >= n) /* stack large enough? */
        res = 1;
    else /* need to grow the stack */
        res = csT_growstack(C, n, 0);
    if (res && cf->top.p < C->sp.p + n)
        cf->top.p = C->sp.p + n; /* adjust frame top */
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
    const TValue *o = index2value(C, index);
    return ttisint(o);
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
    api_check(C, CS_TNONE <= type && type < CS_NUM_TYPES, "invalid type");
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
    if (!ttisstring(o)) /* not a string? */
        return NULL;
    else if (plen != NULL)
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
        csV_unarithm(C, s2v(C->sp.p-1), op);
        /* done */
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
    return (isvalid(C, lhs) && isvalid(C, rhs)) ? csV_raweq(lhs, rhs) : 0;
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


/* Push fulluserdata on top of the stack. */
CS_API void *cs_push_userdata(cs_State *C, size_t sz, int nuv) {
    UserData *ud;
    cs_lock(C);
    api_check(C, 0 <= nuv && nuv < USHRT_MAX, "invalid value");
    ud = csMM_newuserdata(C, sz, nuv);
    setuval2s(C, C->sp.p, ud);
    api_inctop(C);
    csG_checkGC(C);
    cs_unlock(C);
    return getuserdatamem(ud);
}


/* Push list on top of the stack. */
CS_API void cs_push_list(cs_State *C, int sz) {
    List *l;
    cs_lock(C);
    l = csA_new(C);
    setlistval2s(C, C->sp.p, l);
    api_inctop(C);
    csA_ensure(C, l, sz);
    csG_checkGC(C);
    cs_unlock(C);
}


/* Push table on top of the stack. */
CS_API void cs_push_table(cs_State *C, int sz) {
    Table *t;
    cs_lock(C);
    t = csH_new(C);
    settval2s(C, C->sp.p, t);
    api_inctop(C);
    if (sz > 0)
        csH_resize(C, t, sz);
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
    SPtr func = C->sp.p;
    cs_lock(C);
    o = index2value(C, index);
    api_check(C, ttisclass(o), "expect class");
    setclsval2s(C, func, classval(o));
    api_inctop(C);
    csV_call(C, func, 1);
    csG_checkGC(C);
    cs_unlock(C);
}


#define fastget(C,t,k,slot,f)     ((slot = f(t, k)), !isempty(slot))

#define finishfastset(C,t,slot,v) \
    { setobj(C, cast(TValue *, slot), v); \
      csG_barrierback(C, obj2gco(t), v); }


c_sinline void aux_rawsetstr(cs_State *C, Table *t, const char *str,
                            const TValue *v) {
    const TValue *slot;
    OString *s = csS_new(C, str);
    if (fastget(C, t, s, slot, csH_getstr)) {
        finishfastset(C, t, slot, v);
        C->sp.p--; /* pop value */
    } else {
        setstrval2s(C, C->sp.p, s);
        api_inctop(C);
        csV_settable(C, t, s2v(C->sp.p - 1), v);
        C->sp.p -= 2; /* pop string key and value */
    }
    cs_unlock(C);
}


c_sinline OClass *getclass(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttisclass(o), "expected class");
    return classval(o);
}


c_sinline List *getlist(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttislist(o), "expect list");
    return listval(o);
}


c_sinline void setmethods(cs_State *C, OClass *cls, const cs_Entry *l,
                          int nup) {
    if (l) { /* have methods? */
        cs_checkstack(C, nup);
        if (l->name) { /* have at least one entry? */
            cs_assert(cls->methods == NULL);
            cls->methods = csH_new(C);
            do {
                api_check(C, l->func, "l->func is NULL");
                for (int i = 0; i < nup; i++) /* push upvalues to the top */
                    pushvalue(C, -nup);
                pushcclosure(C, l->func, nup);
                aux_rawsetstr(C, cls->methods, l->name, s2v(C->sp.p - 1));
            } while (l++, l->name);
        }
        settop(C, -(nup - 1)); /* pop upvalues */
    } else
        api_check(C, nup <= 0, "'nup' greater than zero but 'l' is NULL");
}


/* 'dowhat' bits for 'aux_pushclass' */
#define DOWNOTHING          0
#define DOWINHERIT          1 /* inherit from superclass */
#define DOWMETALIST         2 /* set class metalist */

static void aux_pushclass(cs_State *C, int nup, const cs_Entry *l, int sci,
                          int mli, int dowhat) {
    OClass *cls;
    cs_lock(C);
    cls = csMM_newclass(C);
    setclsval2s(C, C->sp.p, cls);
    api_inctop(C);
    if (dowhat & DOWINHERIT) { /* inherit? */
        OClass *scl = getclass(C, sci);
        csV_inherit(C, cls, scl);
    }
    if (dowhat & DOWMETALIST) { /* set metalist? */
        List *ml = getlist(C, mli);
        cls->metalist = ml;
    }
    setmethods(C, cls, l, nup);
    csG_checkGC(C);
    cs_unlock(C);
}


CS_API void cs_push_class(cs_State *C, int nup, const cs_Entry *l) {
    aux_pushclass(C, nup, l, 0, 0, DOWNOTHING);
}


CS_API void cs_push_metaclass(cs_State *C, int ml, int nup,
                              const cs_Entry *l) {
    aux_pushclass(C, nup, l, 0, ml, DOWMETALIST);
}


CS_API void cs_push_subclass(cs_State *C, int sc, int nup, const cs_Entry *l) {
    aux_pushclass(C, nup, l, sc, 0, DOWINHERIT);
}


CS_API void cs_push_metasubclass(cs_State *C, int sc, int ml, int nup,
                                 const cs_Entry *l) {
    aux_pushclass(C, nup, l, sc, ml, (DOWINHERIT | DOWMETALIST));
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


c_sinline int aux_rawgetfieldstr(cs_State *C, Table *t, const char *k) {
    const TValue *slot;
    OString *str = csS_new(C, k);
    if (fastget(C, t, str, slot, csH_getstr)) {
        setobj2s(C, C->sp.p, slot);
    } else
        setnilval(s2v(C->sp.p));
    api_inctop(C);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


CS_API int cs_get_global(cs_State *C, const char *name) {
    TValue *gt;
    cs_lock(C);
    gt = GT(C);
    return aux_rawgetfieldstr(C, tval(gt), name);
}


CS_API int cs_get_rtable(cs_State *C, const char *field) {
    const TValue *t = RT(C);
    cs_lock(C);
    api_check(C, ttistable(t), "expect table");
    return aux_rawgetfieldstr(C, tval(t), field);
}


CS_API int cs_get(cs_State *C, int index) {
    const TValue *o;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, index);
    csV_get(C, o, s2v(C->sp.p - 1), C->sp.p - 1);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


CS_API int cs_get_raw(cs_State *C, int index) {
    const TValue *o;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, index);
    csV_rawget(C, o, s2v(C->sp.p - 1), C->sp.p - 1);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


CS_API int cs_get_index(cs_State *C, int index, int i) {
    List *l;
    cs_lock(C);
    api_check(C, i >= 0, "invalid `index`");
    l = getlist(C, index);
    if (c_castS2U(i) < l->n) {
        setobj2s(C, C->sp.p, &l->b[i]);
    } else
        setnilval(s2v(C->sp.p));
    api_inctop(C);
    cs_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


static int auxgetindex(cs_State *C, int index, int begin, int end, int nn) {
    List *l = getlist(C, index);
    if (end < 0 || end >= (int)l->n)
        end = cast_int(l->n - (l->n > 0));
    while (begin <= end) {
        if (!isempty(&l->b[begin]) == nn)
            return begin; /* found */
        begin++;
    }
    return -1; /* not found */
}


CS_API int cs_get_nilindex(cs_State *C, int index, uint begin, int end) {
    return auxgetindex(C, index, begin, end, 0);
}


CS_API int cs_get_nnilindex(cs_State *C, int index, uint begin, int end) {
    return auxgetindex(C, index, begin, end, 1);
}


static int auxgetrindex(cs_State *C, int index, int begin, int end, int nn) {
    List *l = getlist(C, index);
    if (begin < 0 || begin >= (int)l->n)
        begin = cast_int(l->n - (l->n > 0));
    while (begin <= end) {
        if (!isempty(&l->b[end]) == nn)
            return end; /* found */
        end--;
    }
    return -1; /* not found */
}


CS_API int cs_get_nilindex_rev(cs_State *C, int index, int begin, uint end) {
    return auxgetrindex(C, index, begin, end, 0);
}


CS_API int cs_get_nnilindex_rev(cs_State *C, int index, int begin, uint end) {
    return auxgetrindex(C, index, begin, end, 1);
}


c_sinline Table *gettable(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case CS_VINSTANCE: return insval(o)->fields;
        case CS_VTABLE: return tval(o); 
        default:  {
            api_check(C, 0, "expect instance or table");
            return NULL;
        }
    }
}


CS_API int cs_get_field(cs_State *C, int index) {
    Table *t;
    const TValue *val;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    t = gettable(C, index);
    val = csH_get(t, s2v(C->sp.p - 1));
    C->sp.p--; /* remove key */
    return finishrawgetfield(C, val);
}


CS_API int cs_get_fieldstr(cs_State *C, int index, const char *field) {
    Table *t;
    cs_lock(C);
    t = gettable(C, index);
    return aux_rawgetfieldstr(C, t, field);
}


CS_API int cs_get_fieldptr(cs_State *C, int index, const void *field) {
    Table *t;
    TValue aux;
    cs_lock(C);
    t = gettable(C, index);
    setpval(&aux, cast_voidp(field));
    return finishrawgetfield(C, csH_get(t, &aux));
}


CS_API int cs_get_fieldint(cs_State *C, int index, cs_Integer i) {
    Table *t;
    cs_lock(C);
    t = gettable(C, index);
    return finishrawgetfield(C, csH_getint(t, i));
}


CS_API int cs_get_fieldflt(cs_State *C, int index, cs_Number n) {
    Table *t;
    TValue aux;
    cs_lock(C);
    t = gettable(C, index);
    setfval(&aux, n);
    return finishrawgetfield(C, csH_get(t, &aux));
}


CS_API int cs_get_class(cs_State *C, int index) {
    const TValue *o;
    int t;
    cs_lock(C);
    o = index2value(C, index);
    if (ttisinstance(o)) {
        setclsval2s(C, C->sp.p, insval(o)->oclass);
        api_inctop(C);
        t = CS_TCLASS;
    } else
        t = CS_TNONE;
    cs_unlock(C);
    return t;
}


CS_API int cs_get_superclass(cs_State *C, int index) {
    OClass *scl = NULL;
    int res = 1;
    const TValue *o;
    cs_lock(C);
    o = index2value(C, index);
    if (ttisinstance(o))
        scl = insval(o)->oclass->sclass;
    else if (ttisclass(o))
        scl = classval(o)->sclass;
    if (scl) {
        setclsval2s(C, C->sp.p, scl);
        api_inctop(C);
    } else
        res = 0;
    cs_unlock(C);
    return res;
}


c_sinline Instance *getinstance(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttisinstance(o), "expect instance");
    return insval(o);
}


static int aux_getmethod(cs_State *C, const TValue *index, Table *t) {
    int tt;
    if (t) { /* have methods table? */
        const TValue *method = csH_get(t, s2v(C->sp.p - 1));
        if (!isempty(method)) { /* method found? */
            tt = ttype(method);
            if (ttisinstance(index)) { /* instance method? */
                IMethod *im = csMM_newinsmethod(C, insval(index), method);
                setimval2s(C, C->sp.p-1, im);
            } else { /* userdata method */
                UMethod *um = csMM_newudmethod(C, uval(index), method);
                setumval2s(C, C->sp.p-1, um);
            }
            csG_checkGC(C);
            goto unlock;
        }
    }
    tt = CS_TNIL;
    setnilval(s2v(C->sp.p - 1));
unlock:
    cs_unlock(C);
    return tt;
}


static const TValue *objwithmethods(cs_State *C, int index, Table **t) {
    const TValue *obj = index2value(C, index);
    assert(t != NULL);
    api_check(C, ttisinstance(obj) || ttisfulluserdata(obj),
                 "expect full userdata or instance");
    if (ttisinstance(obj))
        *t = insval(obj)->oclass->methods;
    else
        *t = uval(obj)->methods;
    return obj;
}


CS_API int cs_get_method(cs_State *C, int index) {
    const TValue *obj;
    Table *t;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    obj = objwithmethods(C, index, &t);
    return aux_getmethod(C, obj, t);
}


CS_API int cs_get_supermethod(cs_State *C, int index) {
    const TValue *obj;
    Table *t = NULL;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    obj = index2value(C, index);
    api_check(C, ttisinstance(obj), "expect instance");
    if (insval(obj)->oclass->sclass) /* have superclass? */
        t = insval(obj)->oclass->sclass->methods;
    return aux_getmethod(C, obj, t);
}


c_sinline UserData *getuserdata(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttisfulluserdata(o), "expect full userdata");
    return uval(o);
}


CS_API int cs_get_metalist(cs_State *C, int index) {
    const TValue *obj;
    List *ml;
    int res = 0;
    cs_lock(C);
    obj = index2value(C, index);
    switch (ttype(obj)) {
        case CS_TINSTANCE:
            ml = insval(obj)->oclass->metalist;
            break;
        case CS_TCLASS:
            ml = classval(obj)->metalist;
            break;
        case CS_TUSERDATA:
            ml = uval(obj)->metalist;
            break;
        default:
            ml = NULL;
            break;
    }
    if (ml != NULL) {
        setlistval2s(C, C->sp.p, ml);
        api_inctop(C);
        res = 1;
    }
    cs_unlock(C);
    return res;
}


CS_API int cs_get_uservalue(cs_State *C, int index, unsigned short n) {
    UserData *ud;
    int t;
    cs_lock(C);
    ud = getuserdata(C, index);
    if (n <= 0 || ud->nuv < n) {
        setnilval(s2v(C->sp.p));
        t = CS_TNONE;
    } else {
        setobj2s(C, C->sp.p, &ud->uv[n - 1].val);
        t = ttype(s2v(C->sp.p - 1));
    }
    api_inctop(C);
    cs_unlock(C);
    return t;
}


CS_API int cs_get_usermethods(cs_State *C, int index) {
    UserData *ud;
    int res = 0;
    cs_lock(C);
    ud = getuserdata(C, index);
    if (ud->methods) {
        settval2s(C, C->sp.p, ud->methods);
        api_inctop(C);
        res = 1;
    }
    cs_unlock(C);
    return res;
}


CS_API void cs_set_global(cs_State *C, const char *name) {
    TValue *gt;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    gt = GT(C);
    aux_rawsetstr(C, tval(gt), name, s2v(C->sp.p - 1));
}


CS_API void cs_set_rtable(cs_State *C, const char *field) {
    TValue *t = RT(C);
    cs_lock(C);
    api_checknelems(C, 1);
    api_check(C, ttistable(t), "expect table");
    aux_rawsetstr(C, tval(t), field, s2v(C->sp.p - 1));
}


CS_API void cs_set(cs_State *C, int obj) {
    TValue *o;
    cs_lock(C);
    api_checknelems(C, 2); /* value and key */
    o = index2value(C, obj);
    csV_set(C, o, s2v(C->sp.p - 2), s2v(C->sp.p - 1));
    C->sp.p -= 2; /* remove value and key */
    cs_unlock(C);
}


CS_API void  cs_set_raw(cs_State *C, int obj) {
    TValue *o;
    cs_lock(C);
    api_checknelems(C, 2); /* value and key */
    o = index2value(C, obj);
    csV_rawset(C, o, s2v(C->sp.p - 2), s2v(C->sp.p - 1));
    C->sp.p -= 2; /* remove value and key */
    cs_unlock(C);
}


CS_API void cs_set_index(cs_State *C, int index, int i) {
    List *l;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    api_check(C, 0 <= i && i <= CS_MAXLISTINDEX, "`index` out of bounds");
    l = getlist(C, index);
    csA_ensureindex(C, l, i);
    setobj(C, &l->b[i], s2v(C->sp.p - 1));
    csV_finishrawset(C, l, s2v(C->sp.p - 1));
    C->sp.p--; /* remove value */
    cs_unlock(C);
}


c_sinline void aux_rawsetfield(cs_State *C, int obj, TValue *key, int n) {
    Table *t;
    cs_lock(C);
    api_checknelems(C, n);
    t = gettable(C, obj);
    csV_settable(C, t, key, s2v(C->sp.p - 1));
    C->sp.p -= n;
    cs_unlock(C);
}


CS_API void cs_set_field(cs_State *C, int obj) {
    aux_rawsetfield(C, obj, s2v(C->sp.p - 2), 2);
}


CS_API void cs_set_fieldstr(cs_State *C, int index, const char *field) {
    Table *t;
    cs_lock(C);
    api_checknelems(C, 1);
    t = gettable(C, index);
    aux_rawsetstr(C, t, field, s2v(C->sp.p - 1));
}


CS_API void cs_set_fieldptr(cs_State *C, int obj, const void *field) {
    TValue key;
    setpval(&key, cast_voidp(field));
    aux_rawsetfield(C, obj, &key, 1);
}


CS_API void  cs_set_fieldint(cs_State *C, int obj, cs_Integer field) {
    TValue key;
    setival(&key, field);
    aux_rawsetfield(C, obj, &key, 1);
}


CS_API void  cs_set_fieldflt(cs_State *C, int obj, cs_Number field) {
    TValue key;
    setfval(&key, field);
    aux_rawsetfield(C, obj, &key, 1);
}


CS_API int cs_set_metalist(cs_State *C, int index) {
    const TValue *obj;
    List *ml;
    int res = 1;
    cs_lock(C);
    api_checknelems(C, 1); /* metalist */
    obj = index2value(C, index);
    if (ttisnil(s2v(C->sp.p - 1)))
        ml = NULL;
    else {
        api_check(C, ttislist(s2v(C->sp.p - 1)), "list expected");
        ml = listval(s2v(C->sp.p - 1));
    }
    switch (ttype(obj)) {
        case CS_TINSTANCE: {
            insval(obj)->oclass->metalist = ml;
            break;
        }
        case CS_TCLASS: {
            classval(obj)->metalist = ml;
            break;
        }
        case CS_TUSERDATA: {
            uval(obj)->metalist = ml;
            break;
        }
        default: {
            res = 0;
            goto done;
        }
    }
    if (ml) {
        csG_objbarrier(C, gcoval(obj), ml);
        csG_checkfin(C, gcoval(obj), ml);
        csA_ensureindex(C, ml, CS_MM_N - 1);
    }
done:
    C->sp.p--; /* remove metalist */
    cs_unlock(C);
    return res;
}


CS_API int cs_set_uservalue(cs_State *C, int index, unsigned short n) {
    UserData *ud;
    int res;
    cs_lock(C);
    api_checknelems(C, 1); /* value */
    ud = getuserdata(C, index);
    if (!(cast_uint(n) <= cast_uint(ud->nuv))) {
        res = 0; /* `n` not in [0, ud->nuv) */
    } else {
        setobj(C, &ud->uv[n - 1].val, s2v(C->sp.p - 1));
        csV_finishrawset(C, ud, s2v(C->sp.p - 1));
        res = 1;
    }
    C->sp.p--; /* remove value */
    cs_unlock(C);
    return res;
}


CS_API void cs_set_usermethods(cs_State *C, int index) {
    UserData *ud;
    cs_lock(C);
    api_checknelems(C, 1); /* table */
    ud = getuserdata(C, index);
    api_check(C, ttistable(s2v(C->sp.p - 1)), "expect table");
    ud->methods = tval(s2v(C->sp.p - 1));
    csG_barrierback(C, obj2gco(ud), s2v(C->sp.p - 1));
    C->sp.p--; /* remove table */
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
    /* cs_unlock() is called before control leaves the core */
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


CS_API int cs_pcall(cs_State *C, int nargs, int nresults, int abserrfunc) {
    struct PCallData pcd;
    int status;
    ptrdiff_t func;
    cs_lock(C);
    api_checknelems(C, nargs+1); /* args + func */
    api_check(C, C->status == CS_OK, "can't do calls on non-normal thread");
    checkresults(C, nargs, nresults);
    if (abserrfunc < 0) {
        func = 0;
    } else {
        SPtr o = index2stack(C, abserrfunc);
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


/*
** Return the length of the value at index.
** Length means different things depending on the type of the value at index.
** For strings, this is the string length; for classes, this is the number
** of methods; for instances, this is the number of fields; for userdata, this
** is the size of the block of memory allocated for userdata.
*/
CS_API cs_Integer cs_len(cs_State *C, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case CS_VSHRSTR: return strval(o)->shrlen;
        case CS_VLNGSTR: return strval(o)->u.lnglen;
        case CS_VLIST: return listval(o)->n;
        case CS_VTABLE: return csH_len(tval(o));
        case CS_VCLASS: {
            Table *t = classval(o)->methods;
            return (t ? csH_len(classval(o)->methods) : 0);
        }
        case CS_VINSTANCE: return csH_len(insval(o)->fields);
        default: return 0;
    }
}


CS_API size_t cs_lenudata(cs_State *C, int index) {
    return getuserdata(C, index)->size;
}


CS_API int cs_next(cs_State *C, int obj) {
    Table *t;
    int more;
    cs_lock(C);
    api_checknelems(C, 1); /* key */
    t = gettable(C, obj);
    more = csH_next(C, t, C->sp.p - 1);
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
    if (n > 0) {
        csV_concat(C, n);
        csG_checkGC(C);
    } else { /* nothing to concatenate */
        setstrval2s(C, C->sp.p, csS_newl(C, "", 0));
        api_inctop(C);
    }
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


static const char *aux_upvalue(TValue *func, int n, TValue **val,
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
    name = aux_upvalue(index2value(C, index), n, &upval, NULL);
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
    name = aux_upvalue(func, n, &upval, &owner);
    if (name) { /* found upvalue ? */
        setobj(C, upval, s2v(--C->sp.p));
        csG_barrier(C, owner, upval);
    }
    cs_unlock(C);
    return name;
}
