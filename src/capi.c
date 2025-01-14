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
#include "chashtable.h"
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
#define isvalid(ts,o)           (!ttisnil(o) || (o) != &G_(ts)->nil)


/* 
** Convert index to a pointer to its value.
** Invalid indices (using upvalue index for CScript functions) return
** special nil value `&G_(ts)->nil`.
*/
static TValue *index2value(const cs_State *ts, int index) {
    CallFrame *cf = ts->cf;
    if (index >= 0) { /* absolute index? */
        SPtr o = cf->func.p + index + 1;
        api_check(ts, index < ts->sp.p - (cf->func.p + 1), "index too large");
        if (o >= ts->sp.p) return &G_(ts)->nil;
        else return s2v(o);
    } else if (!ispseudo(index)) { /* negative index? */
        api_check(ts, -index <= ts->sp.p - cf->func.p, "index too small");
        return s2v(ts->sp.p + index);
    } else if (index == CS_REGISTRYINDEX) {
        return &G_(ts)->c_registry;
    } else { /* upvalues */
        index = CS_REGISTRYINDEX - index;
        api_check(ts, index < MAXUPVAL, "upvalue index too large");
        if (c_likely(ttisCclosure(s2v(cf->func.p)))) { /* C closure? */
            CClosure *ccl = clCval(s2v(cf->func.p));
            return &ccl->upvals[index];
        } else { /* CScript function (invalid) */
            api_check(ts, 0, "caller not a C closure");
            return &G_(ts)->nil; /* no upvalues */
        }
    }
}


static SPtr index2stack(const cs_State *ts, int index) {
    CallFrame *cf = ts->cf;
    if (index >= 0) {
        SPtr p = cf->func.p + index;
        api_check(ts, p < ts->sp.p, "invalid index");
        return p;
    } else { /* negative index */
        api_check(ts, -index <= (ts->sp.p-cf->func.p), "invalid index");
        api_check(ts, !ispseudo(index), "invalid index");
        return ts->sp.p + index; /* index is subtracted */
    }
}


CS_API cs_CFunction cs_atpanic(cs_State *ts, cs_CFunction fpanic) {
    cs_CFunction old_panic;
    cs_lock(ts);
    old_panic = G_(ts)->fpanic;
    G_(ts)->fpanic = fpanic;
    cs_unlock(ts);
    return old_panic;
}


CS_API cs_Number cs_version(cs_State *ts) {
    UNUSED(ts);
    return CS_VERSION_NUMBER;
}


/* auxiliary function that sets the stack top without locking */
c_sinline void auxsettop(cs_State *ts, int n) {
    CallFrame *cf;
    SPtr func, newtop;
    ptrdiff_t diff;
    cf = ts->cf;
    func = cf->func.p;
    if (n >= 0) {
        api_check(ts, n <= (cf->top.p - func + 1), "new top too large");
        diff = (func + 1 + n) - ts->sp.p;
        for (; diff > 0; diff--)
            setnilval(s2v(ts->sp.p++));
    } else { /* negative index */
        api_check(ts, -(n+1) <= (ts->sp.p-(func+1)), "new top underflow");
        diff = n + 1;
    }
    newtop = ts->sp.p + diff;
    if (diff < 0 && ts->tbclist.p >= newtop) {
        cs_assert(hastocloseCfunc(cf->nresults));
        csF_close(ts, newtop, CLOSEKTOP);
    }
    ts->sp.p = newtop; /* set new top */
}


/* 
** Sets the stack top to `n` - 1 (n being the number of values to be on top).
** If new top is greater than the previous one, new values are elements
** are filled with `nil`.
** If n is 0, then all stack elements are removed.
** This function can run `__close` if it removes an index from the stack
** marked as to-be-closed.
*/
CS_API void cs_setntop(cs_State *ts, int n) {
    cs_lock(ts);
    auxsettop(ts, n);
    cs_unlock(ts);
}


/* 
** Return the index of the top element on the stack.
** This result is equal to the number of elements on the stack - 1.
** Negative value (-1) means an empty stack.
*/
CS_API int cs_gettop(const cs_State *ts) {
    return cast_int(ts->sp.p - (ts->cf->func.p + 1) - 1);
}


/* 
** Convert `acceptable` stack index into an absolute index.
** For example, if there are 5 values on the stack then passing
** -1 as the value of `index` would be return 4. 
*/
CS_API int cs_absindex(cs_State *ts, int index)
{
    return (index >= 0 || ispseudo(index))
            ? index
            : cast_int(ts->sp.p - ts->cf->func.p - 1) + index;
}


/* 
** Auxiliary to `cs_rotate`, reverses stack values `from` until `to`.
** Note that this does not perform a deep copy.
*/
c_sinline void reverse(cs_State *ts, SPtr from, SPtr to) {
    while (from < to) {
        TValue aux;
        setobj(ts, &aux, s2v(from));
        setobjs2s(ts, from, to);
        setobj2s(ts, to, &aux);
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
** cs_rotate(ts, 2, 2);
** [func][0][1][3][4][2]
**
** Example left-rotation:
** [func][0][1][2][3][4]
** cs_rotate(ts, 2, -2);
** [func][0][1][4][3][2]
*/
CS_API void cs_rotate(cs_State *ts, int index, int n) {
    SPtr start, end, pivot;
    cs_lock(ts);
    end = ts->sp.p - 1; /* end of segment */
    start = index2stack(ts, index); /* start of segment */
    api_check(ts, (n >= 0 ? n : -n) <= (end - start + 1), "invalid `n`");
    pivot = (n >= 0 ? end - n : start - n - 1); /* end of prefix */
    reverse(ts, start, pivot);
    reverse(ts, pivot + 1, end);
    reverse(ts, start, end);
    cs_unlock(ts);
}


/* 
** Copy value at index `src` to stack slot at index `dest`.
** Note that `dest` must be stack index.
*/
CS_API void cs_copy(cs_State *ts, int src, int dest) {
    TValue *from, *to;
    cs_lock(ts);
    from = index2value(ts, src);
    to = index2value(ts, dest);
    api_check(ts, isvalid(ts, to), "invalid index");
    setobj(ts, to, from);
    if (isupvalue(dest)) /* closure upvalue? */
        csG_barrier(ts, clCval(s2v(ts->cf->func.p)), from);
    cs_unlock(ts);
}


/*
** Check if stack has enough space for `n` elements,
** if not ensure it does.
*/
CS_API int cs_checkstack(cs_State *ts, int n) {
    CallFrame *cf;
    int res;
    cs_lock(ts);
    cf = ts->cf;
    api_check(ts, n >= 0, "negative `n`");
    if (cf->top.p - ts->sp.p >= n) /* stack large enough? */
        res = 1;
    else /* need to grow the stack */
        res = csT_growstack(ts, n, 0);
    if (res && cf->top.p < ts->sp.p + n)
        cf->top.p = ts->sp.p + n; /* adjust top */
    cs_unlock(ts);
    return res;
}


/* push without lock */
#define pushvalue(ts, index) \
    { setobj2s(ts, ts->sp.p, index2value(ts, index)); api_inctop(ts); }


/*
** Push value at the index on to the top of the stack.
*/
CS_API void cs_push(cs_State *ts, int index) {
    cs_lock(ts);
    pushvalue(ts, index);
    cs_unlock(ts);
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
    api_check(src, G_(src) == G_(dest), "moving between different states");
    api_check(src, dest->cf->top.p - dest->sp.p >= n, "dest stack overflow");
    src->sp.p -= n;
    for (int i = 0; i < n; i++) {
        setobjs2s(dest, dest->sp.p, src->sp.p + i);
        dest->sp.p++; /* already checked by `api_check` */
    }
    cs_unlock(dest);
}


/* Check if the value at index is a number. */
CS_API int cs_is_number(cs_State *ts, int index) {
    cs_Number n;
    UNUSED(n);
    const TValue *o = index2value(ts, index);
    return tonumber(o, n);
}


/* Check if the value at index is an integer. */
CS_API int cs_is_integer(cs_State *ts, int index) {
    cs_Integer i;
    UNUSED(i);
    const TValue *o = index2value(ts, index);
    return tointeger(o, &i);
}


/* Check if the value at index is a string. */
CS_API int cs_is_string(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return ttisstring(o);
}


/* Check if the value at index is a C function. */
CS_API int cs_is_cfunction(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttislcf(o) || ttisCclosure(o));
}


/* Check if the value at index is a userdata. */
CS_API int cs_is_userdata(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttislightuserdata(o) || ttisfulluserdata(o));
}


c_sinline TValue *getvvmt(cs_State *ts, const TValue *o) {
    switch (ttype(o)) {
        case CS_VINSTANCE: return insval(o)->oclass->vmt;
        case CS_TCLASS: return classval(o)->vmt;
        case CS_TUSERDATA: return uval(o)->vmt;
        default: return G_(ts)->vmt[ttype(o)];
    }
}


c_sinline TValue *getvmt(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return getvvmt(ts, o);
}


/* 
** Return the type of the value at valid index or CS_TNONE
** if index is invalid.
** The types returned are defined in `cscript.h` (CS_T*).
*/
CS_API int cs_type(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (isvalid(ts, o) ? ttype(o) : CS_TNONE);
}


/*
** Return text representation of the given type.
** The returned string has a static lifetime and should not be modified
** directly.
*/
CS_API const char *cs_typename(cs_State *ts, int type) {
    UNUSED(ts);
    api_check(ts, 0 <= type && type < CS_NUM_TYPES, "invalid type");
    return typename(type);
}


/*
** Returns the number value of the value at index.
** The fact whether the value was a number is stored in `pisnum` if
** provided, the default value returned when index is not a number is 0.0.
*/
CS_API cs_Number cs_to_numberx(cs_State *ts, int index, int *pisnum) {
    cs_Number n = 0.0;
    const TValue *o = index2value(ts, index);
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
CS_API cs_Integer cs_to_integerx(cs_State *ts, int index, int *pisint) {
    cs_Integer i = 0;
    const TValue *o = index2value(ts, index);
    int isint = tointeger(o, &i);
    if (pisint)
        *pisint = isint;
    return i;
}


/*
** Returns 0 or 1 whether the value at index is false or true respectively.
** All values in CScript are considered true except `nil` and `false`.
*/
CS_API int cs_to_bool(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return !c_isfalse(o);
}


/*
** Return string value from value at index and if `plen` is provided
** set it to match the length of the string.
** If the value is not a string return NULL (in this case if `plen` is
** provided it is ignored).
*/
CS_API const char *cs_to_lstring(cs_State *ts, int index, size_t *plen) {
    const TValue *o;
    cs_lock(ts);
    o = index2value(ts, index);
    if (!ttisstring(o)) /* not a string? */
        return NULL;
    if (plen != NULL)
        *plen = getstrlen(strval(o)); 
    cs_unlock(ts);
    return getstr(strval(o));
}


/*
** Return `cs_CFunction` from the value at index.
** If the value is not a C closure or light C function, then this returns NULL.
*/
CS_API cs_CFunction cs_to_cfunction(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
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
CS_API void *cs_to_userdata(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return touserdata(o);
}


/*
** Return the pointer to the object at index.
** If the object is not userdata, C function or collectable, then this
** returns NULL. Note that returned pointer shouldn't be modified.
*/
CS_API const void *cs_to_pointer(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
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
CS_API cs_State *cs_to_thread(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttisthread(o) ? thval(o) : NULL);
}


/*
** Perform arithmetic operation `op`; the valid arithmetic operations are
** located in `cscript.h`.
** This functions is free to call overloadable methods.
*/
CS_API void cs_arith(cs_State *ts, int op) {
    cs_lock(ts);
    if (op != CS_OPUNM && op != CS_OPBNOT) { /* binary op? */
        api_checknelems(ts, 2);
        csV_binarithm(ts, s2v(ts->sp.p-2), s2v(ts->sp.p-1), ts->sp.p-2, op);
        ts->sp.p--; /* pop second operand */
    } else { /* unary op */
        api_checknelems(ts, 1);
        csV_unarithm(ts, s2v(ts->sp.p-1), ts->sp.p-1, op);
    }
    cs_unlock(ts);
}


/*
** Perform raw equality between values at `index1` and `index2`.
** `raw` meaning this function won't call overloaded methods.
** In cases where either of the indexes is not valid this always returns 0.
*/
CS_API int cs_rawequal(cs_State *ts, int index1, int index2) {
    const TValue *lhs = index2value(ts, index1);
    const TValue *rhs = index2value(ts, index2);
    return (isvalid(ts, lhs) && isvalid(ts, rhs) ? csV_raweq(lhs, rhs) : 0);
}


/*
** Compares values at `index1` and `index2`.
** This function is free to call overloded methods.
** In case either index is invalid this then always returns 0.
*/
CS_API int cs_compare(cs_State *ts, int index1, int index2, int op) {
    const TValue *lhs;
    const TValue *rhs;
    int res = 0;
    cs_lock(ts); /* might call overloaded method */
    lhs = index2value(ts, index1);
    rhs = index2value(ts, index2);
    if (isvalid(ts, lhs) && isvalid(ts, rhs)) {
        switch (op) {
            case CS_OPEQ: res = csV_ordereq(ts, lhs, rhs); break;
            case CS_OPLT: res = csV_orderlt(ts, lhs, rhs); break;
            case CS_OPLE: res = csV_orderle(ts, lhs, rhs); break;
            default: api_check(ts, 0, "invalid `op`"); break;
        }
    }
    cs_unlock(ts);
    return res;
}


/* Push nil value on top of the stack. */
CS_API void cs_push_nil(cs_State *ts) {
    cs_lock(ts);
    setnilval(s2v(ts->sp.p));
    api_inctop(ts);
    cs_unlock(ts);
}


/* Push `cs_Number` value on top of the stack. */
CS_API void cs_push_number(cs_State *ts, cs_Number n) {
    cs_lock(ts);
    setfval(s2v(ts->sp.p), n);
    api_inctop(ts);
    cs_unlock(ts);
}


/* Push `cs_Integer` value on top of the stack. */
CS_API void cs_push_integer(cs_State *ts, cs_Integer i) {
    cs_lock(ts);
    setival(s2v(ts->sp.p), i);
    api_inctop(ts);
    cs_unlock(ts);
}


/* Push string value of length `len` on top of the stack. */
CS_API const char *cs_push_lstring(cs_State *ts, const char *str, size_t len) {
    OString *s;
    cs_lock(ts);
    s = (len == 0 ? csS_new(ts, "") : csS_newl(ts, str, len));
    setstrval2s(ts, ts->sp.p, s);
    api_inctop(ts);
    csG_checkGC(ts);
    cs_unlock(ts);
    return getstr(s);
}


/* Push null terminated string value on top of the stack. */
CS_API const char *cs_push_string(cs_State *ts, const char *str) {
    cs_lock(ts);
    if (str == NULL) {
        setnilval(s2v(ts->sp.p));
    } else {
        OString *s = csS_new(ts, str);
        setstrval2s(ts, ts->sp.p, s);
        str = getstr(s);
    }
    api_inctop(ts);
    csG_checkGC(ts);
    cs_unlock(ts);
    return str;
}


/* 
** Push formatted string with format values in a `va_list` on top of the stack.
** Valid format values are c (char), d (int), I (cs_Integer), N (cs_Number),
** s (string), p (pointer) and % (`%`). Note that each format specifier is
** preceeded by `%` (so %I...).
*/
CS_API const char *cs_push_vfstring(cs_State *ts, const char *fmt, va_list argp) {
    const char *str;
    cs_lock(ts);
    str = csS_pushvfstring(ts, fmt, argp);
    csG_checkGC(ts);
    cs_unlock(ts);
    return str;
}


/* 
** Push formatted string with variable amount of format values on top of the
** stack. Valid format specifiers are the same as in `cs_pushvfstring`.
*/
CS_API const char *cs_push_fstring(cs_State *ts, const char *fmt, ...) {
    const char *str;
    va_list argp;
    cs_lock(ts);
    va_start(argp, fmt);
    str = csS_pushvfstring(ts, fmt, argp);
    va_end(argp);
    csG_checkGC(ts);
    cs_unlock(ts);
    return str;
}


/* auxiliary function, pushes C closure without locking */
c_sinline void auxpushcclosure(cs_State *ts, cs_CFunction fn, int nupvals) {
    if (nupvals == 0) {
        setcfval(ts, s2v(ts->sp.p), fn);
        api_inctop(ts);
    } else {
        CClosure *ccl;
        api_checknelems(ts, nupvals);
        ccl = csF_newCClosure(ts, nupvals);
        ccl->fn = fn;
        ts->sp.p -= nupvals;
        while (nupvals--) {
            setobj(ts, &ccl->upvals[nupvals], s2v(ts->sp.p + nupvals));
            cs_assert(iswhite(ccl));
        }
        setclCval2s(ts, ts->sp.p, ccl);
        api_inctop(ts);
        csG_checkGC(ts);
    }
}


/*
** Push C closure value on top of the stack.
** This closure will have `nupvals` upvalues, these values will be popped
** of the stack and inserted into the closure.
** If `nupvals` is 0 then this does not create a C closure, rather it only
** pushes the `cs_CFunction` on top of the stack.
*/
CS_API void cs_push_cclosure(cs_State *ts, cs_CFunction fn, int nupvals) {
    cs_lock(ts);
    auxpushcclosure(ts, fn, nupvals);
    cs_unlock(ts);
}


/*
** Push boolean value on top of the stack.
** If `b` is 0 then `false` is pushed, otherwise `true`.
*/
CS_API void cs_push_bool(cs_State *ts, int b) {
    cs_lock(ts);
    if (b)
        setbtval(s2v(ts->sp.p));
    else
        setbfval(s2v(ts->sp.p));
    api_inctop(ts);
    cs_unlock(ts);
}


/* Push light userdata on top of the stack. */
CS_API void cs_push_lightuserdata(cs_State *ts, void *p) {
    cs_lock(ts);
    setpval(s2v(ts->sp.p), p);
    api_inctop(ts);
    cs_unlock(ts);
}


/* Push array on top of the stack. */
CS_API void cs_push_array(cs_State *ts, int sz) {
    Array *arr;
    cs_lock(ts);
    arr = csA_new(ts);
    setarrval2s(ts, ts->sp.p, arr);
    api_inctop(ts);
    if (sz > 0)
        csA_ensure(ts, arr, sz - 1);
    csG_checkGC(ts);
    cs_unlock(ts);
}


/* Push hashtable on top of the stack. */
CS_API void cs_push_table(cs_State *ts, int sz) {
    HTable *ht;
    cs_lock(ts);
    ht = csH_new(ts);
    sethtval2s(ts, ts->sp.p, ht);
    api_inctop(ts);
    if (sz > 0)
        csH_resize(ts, ht, sz);
    csG_checkGC(ts);
    cs_unlock(ts);
}


/*
** Push thread `ts` on top of the stack.
** Returns non-zero if the pushed thread is main thread.
*/
CS_API int cs_push_thread(cs_State *ts) {
    cs_lock(ts);
    setthval2s(ts, ts->sp.p, ts);
    api_inctop(ts);
    cs_unlock(ts);
    return (G_(ts)->mainthread == ts);
}


CS_API void cs_push_instance(cs_State *ts, int clsobj) {
    const TValue *o;
    SPtr func;
    cs_lock(ts);
    o = index2value(ts, clsobj);
    api_check(ts, ttisclass(o), "expect class");
    func = ts->sp.p;
    setclsval2s(ts, func, classval(o));
    api_inctop(ts);
    csV_call(ts, func, 1);
    cs_assert(ttisinstance(s2v(ts->sp.p))); /* result is the instance */
    csG_checkGC(ts);
    cs_unlock(ts);
}


c_sinline void auxsetvmt(TValue *dest, const cs_VMT *vmt) {
    for (int i = 0; i < CS_MM_N; i++) {
        if (vmt->func[i]) {
            setcfval(ts, &dest[i], vmt->func[i]);
        } else
            setnilval(&dest[i]);
    }
}


#define fastget(ts,ht,k,slot,f)     ((slot = f(ht, k)), !isempty(slot))

#define finishfastset(ts,ht,slot,v) \
    { setobj(ts, cast(TValue *, slot), v); \
      csG_barrierback(ts, obj2gco(ht), v); }


c_sinline void auxrawsetstr(cs_State *ts, HTable *ht, const char *str,
                             const TValue *v) {
    const TValue *slot;
    OString *s = csS_new(ts, str);
    if (fastget(ts, ht, s, slot, csH_getstr)) {
        finishfastset(ts, ht, slot, v);
        ts->sp.p--; /* pop value */
    } else {
        setstrval2s(ts, ts->sp.p, s);
        api_inctop(ts);
        csH_set(ts, ht, s2v(ts->sp.p - 1), v);
        csG_barrierback(ts, obj2gco(ht), v);
        ts->sp.p -= 2; /* pop string key and value */
    }
    cs_unlock(ts);
}


c_sinline void auxsetentrylist(cs_State *ts, OClass *cls, const cs_Entry *l,
                                int nup) {
    cs_assert(l != NULL);
    cs_checkstack(ts, nup);
    if (l->name) { /* have at least one entry? */
        cs_assert(cls->methods == NULL);
        cls->methods = csH_new(ts);
        csG_checkGC(ts);
        do {
            api_check(ts, l->func, "l->func is NULL");
            for (int i = 0; i < nup; i++) /* push upvalues to the top */
                pushvalue(ts, -nup);
            auxpushcclosure(ts, l->func, nup);
            auxrawsetstr(ts, cls->methods, l->name, s2v(ts->sp.p - 1));
        } while (l++, l->name);
    }
    auxsettop(ts, -(nup - 1)); /* pop upvalues */
}


CS_API void cs_push_class(cs_State *ts, const cs_VMT *vmt, int clsobjabs,
                          int nup, const cs_Entry *l) {
    OClass *cls;
    cs_lock(ts);
    cls = csMM_newclass(ts);
    csG_checkGC(ts);
    setclsval2s(ts, ts->sp.p, cls);
    api_inctop(ts);
    if (clsobjabs >= 0) { /* have superclass? */
        const TValue *osup = index2value(ts, clsobjabs);
        api_check(ts, ttisclass(osup), "expect class");
        if (classval(osup)->methods) { /* have methods? */
            cls->methods = csH_new(ts);
            csH_copykeys(ts, classval(osup)->methods, cls->methods);
        }
    }
    if (vmt) { /* have virtual method table? */
        cls->vmt = csMM_newvmt(ts);
        csG_checkGC(ts);
        auxsetvmt(cls->vmt, vmt);
    }
    if (l) /* have methods? */
        auxsetentrylist(ts, cls, l, nup);
    else /* otherwise there should be no upvalues */
        api_check(ts, nup == 0, "nup non-zero but l is NULL");
    cs_unlock(ts);
}



/* CScript -> stack */

c_sinline int finishrawgetfield(cs_State *ts, const TValue *val) {
    if (isempty(val))
        setnilval(s2v(ts->sp.p));
    else
        setobj2s(ts, ts->sp.p, val);
    api_inctop(ts);
    cs_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


c_sinline int auxrawgetfieldstr(cs_State *ts, HTable *ht, const char *k) {
    const TValue *slot;
    OString *str = csS_new(ts, k);
    if (fastget(ts, ht, str, slot, csH_getstr)) {
        setobj2s(ts, ts->sp.p, slot);
    } else
        setnilval(s2v(ts->sp.p));
    api_inctop(ts);
    cs_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


/*
** Gets the global variable `name` value and pushes it on top of the stack.
** This function returns the value type.
*/
CS_API int cs_get_global(cs_State *ts, const char *name) {
    TValue *gt;
    cs_lock(ts);
    gt = getGtable(ts);
    return auxrawgetfieldstr(ts, htval(gt), name);
}


CS_API int cs_get(cs_State *ts, int obj) {
    const TValue *o;
    cs_lock(ts);
    api_checknelems(ts, 1); /* key */
    o = index2value(ts, obj);
    csV_get(ts, o, s2v(ts->sp.p - 1), ts->sp.p - 1);
    cs_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


CS_API int cs_get_raw(cs_State *ts, int obj) {
    const TValue *o;
    cs_lock(ts);
    api_checknelems(ts, 1); /* key */
    o = index2value(ts, obj);
    csV_rawget(ts, o, s2v(ts->sp.p - 1), ts->sp.p - 1);
    cs_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


c_sinline Array *getarray(cs_State *ts, int arrobj) {
    const TValue *o = index2value(ts, arrobj);
    api_check(ts, ttisarr(o), "expect array");
    return arrval(o);
}


CS_API int cs_get_index(cs_State *ts, int arrobj, cs_Integer index) {
    Array *arr;
    cs_lock(ts);
    api_check(ts, 0 <= index && index < ARRAYLIMIT, "invalid `index`");
    arr = getarray(ts, arrobj);
    if (c_castS2U(index) < arr->n) {
        setobj2s(ts, ts->sp.p, &arr->b[index]);
    } else
        setnilval(s2v(ts->sp.p));
    api_inctop(ts);
    cs_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


c_sinline HTable *gettable(cs_State *ts, int obj) {
    const TValue *o = index2value(ts, obj);
    switch (ttypetag(o)) {
        case CS_VINSTANCE: return insval(o)->fields;
        case CS_VHTABLE: return htval(o); 
        default:  {
            api_check(ts, 0, "expect instance or hashtable");
            return NULL;
        }
    }
}


CS_API int cs_get_field(cs_State *ts, int obj) {
    HTable *ht;
    const TValue *val;
    cs_lock(ts);
    api_checknelems(ts, 1); /* key */
    ht = gettable(ts, obj);
    val = csH_get(ht, s2v(ts->sp.p - 1));
    ts->sp.p--; /* remove key */
    return finishrawgetfield(ts, val);

}


CS_API int cs_get_fieldstr(cs_State *ts, int obj, const char *field) {
    HTable *ht;
    cs_lock(ts);
    ht = gettable(ts, obj);
    return auxrawgetfieldstr(ts, ht, field);
}


CS_API int cs_get_fieldptr(cs_State *ts, int obj, const void *field) {
    HTable *ht;
    TValue aux;
    cs_lock(ts);
    ht = gettable(ts, obj);
    setpval(&aux, cast_voidp(field));
    return finishrawgetfield(ts, csH_get(ht, &aux));
}


CS_API int cs_get_fieldint(cs_State *ts, int obj, cs_Integer i) {
    HTable *ht;
    cs_lock(ts);
    ht = gettable(ts, obj);
    return finishrawgetfield(ts, csH_getint(ht, i));
}


CS_API int cs_get_fieldflt(cs_State *ts, int obj, cs_Number n) {
    HTable *ht;
    TValue aux;
    cs_lock(ts);
    ht = gettable(ts, obj);
    setfval(&aux, n);
    return finishrawgetfield(ts, csH_get(ht, &aux));
}


CS_API int cs_get_class(cs_State *ts, int insobj) {
    const TValue *o;
    int tt;
    cs_lock(ts);
    o = index2value(ts, insobj);
    if (ttisinstance(o)) {
        setclsval2s(ts, ts->sp.p, insval(o)->oclass);
        api_inctop(ts);
        tt = CS_TCLASS;
    } else {
        tt = CS_TNONE;
    }
    cs_unlock(ts);
    return tt;
}


c_sinline Instance *getinstance(cs_State *ts, int insobj) {
    const TValue *o = index2value(ts, insobj);
    api_check(ts, ttisinstance(o), "expect instance");
    return insval(o);
}


CS_API int cs_get_method(cs_State *ts, int insobj) {
    Instance *ins;
    cs_lock(ts);
    api_checknelems(ts, 1); /* key */
    ins = getinstance(ts, insobj);
    if (ins->oclass->methods) { /* have methods ? */
        const TValue *slot = csH_get(ins->oclass->methods, s2v(ts->sp.p - 1));
        if (!isempty(slot)) { /* found? */
            IMethod *im = csMM_newinsmethod(ts, ins, slot);
            setimval2s(ts, ts->sp.p - 1, im);
            goto unlock; /* done */
        } /* else fallthrough */
    } /* else fall through */
    setnilval(s2v(ts->sp.p - 1));
unlock:
    cs_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


CS_API int cs_get_metamethod(cs_State *ts, int obj, cs_MM mm) {
    TValue *vmt;
    int tt;
    cs_lock(ts);
    vmt = getvmt(ts, obj);
    if (vmt) {
        setobj2s(ts, ts->sp.p, &vmt[mm]);
        api_inctop(ts);
        tt = ttype(s2v(ts->sp.p - 1)); 
    } else {
        tt = CS_TNONE;
    }
    cs_unlock(ts);
    return tt;
}


CS_API void *cs_newuserdata(cs_State *ts, size_t sz, int nuv) {
    UserData *ud;
    cs_lock(ts);
    api_checknelems(ts, nuv);
    ud = csMM_newuserdata(ts, sz, nuv);
    ts->sp.p -= nuv;
    while (nuv--)
        setobj(ts, &ud->uv[nuv].val, s2v(ts->sp.p + nuv));
    ud->nuv = nuv;
    setuval2s(ts, ts->sp.p, ud);
    api_inctop(ts);
    cs_unlock(ts);
    return getuserdatamem(ud);
}


c_sinline UserData *getuserdata(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    api_check(ts, ttisfulluserdata(o), "expect full userdata");
    return uval(o);
}


CS_API int cs_get_uservmt(cs_State *ts, int udobj, cs_VMT *pvmt) {
    UserData *ud;
    int nmm = 0;
    cs_lock(ts);
    api_check(ts, pvmt != NULL, "`pvmt` is NULL");
    ud = getuserdata(ts, udobj);
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
    cs_unlock(ts);
    return nmm;
}


CS_API int cs_get_uservalue(cs_State *ts, int udobj, int n) {
    UserData *ud;
    int tt;
    cs_lock(ts);
    ud = getuserdata(ts, udobj);
    if (0 < n || ud->nuv < n) {
        setnilval(s2v(ts->sp.p));
        tt = CS_TNONE;
    } else {
        setobj2s(ts, ts->sp.p, &ud->uv[n].val);
        tt = ttype(s2v(ts->sp.p - 1));
    }
    api_inctop(ts);
    cs_unlock(ts);
    return tt;
}


CS_API void cs_set_global(cs_State *ts, const char *name) {
    TValue *gt;
    cs_lock(ts);
    api_checknelems(ts, 1); /* value */
    gt = getGtable(ts);
    auxrawsetstr(ts, htval(gt), name, s2v(ts->sp.p - 1));
}


CS_API void cs_set(cs_State *ts, int obj) {
    TValue *o;
    cs_lock(ts);
    api_checknelems(ts, 2); /* value and key */
    o = index2value(ts, obj);
    csV_set(ts, o, s2v(ts->sp.p - 1), s2v(ts->sp.p - 2));
    ts->sp.p -= 2; /* remove value and key */
    cs_unlock(ts);
}


CS_API void  cs_set_raw(cs_State *ts, int obj) {
    TValue *o;
    cs_lock(ts);
    api_checknelems(ts, 2); /* value and key */
    o = index2value(ts, obj);
    csV_rawset(ts, o, s2v(ts->sp.p - 1), s2v(ts->sp.p - 2));
    ts->sp.p -= 2; /* remove value and key */
    cs_unlock(ts);
}


CS_API void cs_set_index(cs_State *ts, int arrobj, cs_Integer index) {
    Array *arr;
    cs_lock(ts);
    api_checknelems(ts, 1); /* value */
    api_check(ts, 0 <= index && index < ARRAYLIMIT, "`index` out of bounds");
    arr = getarray(ts, arrobj);
    csA_ensure(ts, arr, cast_int(index));
    setobj(ts, &arr->b[cast_int(index)], s2v(ts->sp.p - 1));
    csG_barrierback(ts, obj2gco(arr), s2v(ts->sp.p - 1));
    ts->sp.p--; /* remove value */
    cs_unlock(ts);
}


c_sinline void auxrawsetfield(cs_State *ts, int obj, TValue *key, int n) {
    HTable *ht;
    cs_lock(ts);
    api_checknelems(ts, n);
    ht = gettable(ts, obj);
    csH_set(ts, ht, key, s2v(ts->sp.p - 1));
    csG_barrierback(ts, obj2gco(ht), s2v(ts->sp.p - 1));
    ts->sp.p -= n;
    cs_unlock(ts);
}


CS_API void cs_set_field(cs_State *ts, int obj) {
    auxrawsetfield(ts, obj, s2v(ts->sp.p - 2), 2);
}


CS_API void cs_set_fieldstr(cs_State *ts, int obj, const char *field) {
    HTable *ht;
    cs_lock(ts);
    api_checknelems(ts, 1);
    ht = gettable(ts, obj);
    auxrawsetstr(ts, ht, field, s2v(ts->sp.p - 1));
}


CS_API void cs_set_fieldptr(cs_State *ts, int obj, const void *field) {
    TValue key;
    setpval(&key, cast_voidp(field));
    auxrawsetfield(ts, obj, &key, 1);
}


CS_API void  cs_set_fieldint(cs_State *ts, int obj, cs_Integer field) {
    TValue key;
    setival(&key, field);
    auxrawsetfield(ts, obj, &key, 1);
}


CS_API void  cs_set_fieldflt(cs_State *ts, int obj, cs_Number field) {
    TValue key;
    setfval(&key, field);
    auxrawsetfield(ts, obj, &key, 1);
}


CS_API void cs_set_uservmt(cs_State *ts, int udobj, const cs_VMT *vmt) {
    UserData *ud;
    cs_lock(ts);
    ud = getuserdata(ts, udobj);
    if (!ud->vmt) {
        ud->vmt = csMM_newvmt(ts);
        csG_checkGC(ts);
        if (!vmt) goto unlock;
    }
    if (vmt) {
        auxsetvmt(ud->vmt, vmt);
        csG_checkfin(ts, obj2gco(ud), ud->vmt);
    } else {
        for (int i = 0; i < CS_MM_N; i++)
            setnilval(&ud->vmt[i]);
    }
unlock:
    cs_unlock(ts);

}


CS_API int cs_set_uservalue(cs_State *ts, int index, int n) {
    UserData *ud;
    int res;
    cs_lock(ts);
    api_checknelems(ts, 1); /* value */
    ud = getuserdata(ts, index);
    if (!(cast_uint(n) <= cast_uint(ud->nuv))) {
        res = 0; /* `n` not in [0, ud->nuv) */
    } else {
        setobj(ts, &ud->uv[n].val, s2v(ts->sp.p - 1));
        csG_barrierback(ts, obj2gco(ud), s2v(ts->sp.p - 1));
        res = 1;
    }
    ts->sp.p--; /* remove value */
    cs_unlock(ts);
    return res;
}


CS_API void cs_set_usermm(cs_State *ts, int index, cs_MM mm) {
    UserData *ud;
    cs_lock(ts);
    api_checknelems(ts, 1); /* metamethod func */
    ud = getuserdata(ts, index);
    if (!ud->vmt) {
        ud->vmt = csMM_newvmt(ts);
        csG_checkGC(ts);
    }
    setobj(ts, &ud->vmt[mm], s2v(ts->sp.p - 1));
    csG_barrierback(ts, obj2gco(ud), s2v(ts->sp.p - 1));
    ts->sp.p--;
    cs_unlock(ts);
}



/* Error reporting */


CS_API int cs_status(cs_State *ts) {
    return ts->status;
}


CS_API int cs_error(cs_State *ts) {
    TValue *errobj;
    cs_lock(ts);
    api_checknelems(ts, 1); /* errobj */
    errobj = s2v(ts->sp.p - 1);
    if (ttisstring(errobj) && strval(errobj) == G_(ts)->memerror) {
        csM_error(ts); /* raise a memory error */
    } else
        csD_errormsg(ts);
    /* cs_unlock() is called when control leaves the core */
    cs_assert(0);
}



/* Call/Load CScript code */

#define checkresults(ts,nargs,nres) \
     api_check(ts, (nres) == CS_MULRET \
               || (ts->cf->top.p - ts->sp.p >= (nres) - (nargs)), \
	"results from function overflow current stack size")


CS_API void cs_call(cs_State *ts, int nargs, int nresults) {
    SPtr func;
    cs_lock(ts);
    api_checknelems(ts, nargs + 1); /* args + func */
    api_check(ts, ts->status == CS_OK, "can't do calls on non-normal thread");
    checkresults(ts, nargs, nresults);
    func = ts->sp.p - nargs - 1;
    csV_call(ts, func, nresults);
    adjustresults(ts, nresults);
    cs_unlock(ts);
}


struct PCallData {
    SPtr func;
    int nresults;
};


static void fcall(cs_State *ts, void *ud) {
    struct PCallData *pcd = cast(struct PCallData*, ud);
    csV_call(ts, pcd->func, pcd->nresults);
}


CS_API int cs_pcall(cs_State *ts, int nargs, int nresults, int errfunc) {
    struct PCallData pcd;
    int status;
    ptrdiff_t func;
    cs_lock(ts);
    api_checknelems(ts, nargs + 1); /* args + func */
    api_check(ts, ts->status == CS_OK, "can't do calls on non-normal thread");
    checkresults(ts, nargs, nresults);
    if (errfunc == 0) {
        func = 0;
    } else {
        SPtr o = index2stack(ts, errfunc);
        api_check(ts, ttisfunction(s2v(o)), "error handler must be a function");
        func = savestack(ts, o);
    }
    pcd.func = ts->sp.p - nargs - 1;
    pcd.nresults = nresults;
    status = csPR_call(ts, fcall, &pcd, savestack(ts, pcd.func), func);
    adjustresults(ts, nresults);
    cs_unlock(ts);
    return status;
}


CS_API int cs_load(cs_State *ts, cs_Reader reader, void *userdata,
                    const char *source) {
    BuffReader br;
    int status;
    cs_lock(ts);
    if (!source) source = "?";
    csR_init(ts, &br, reader, userdata);
    status = csPR_parse(ts, &br, source);
    cs_unlock(ts);
    return status;
}


CS_API int cs_gc(cs_State *ts, int option, ...) {
    va_list ap;
    int res = 0;
    GState *gs = G_(ts);
    cs_lock(ts);
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
            csG_full(ts, 0);
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
                csG_step(ts);
            } else { /* add `data` to total gcdebt */
                /* convert `data` to bytes (data = bytes/2^10) */
                gcdebt = (data * 1024) + gs->gcdebt;
                csG_setgcdebt(gs, gcdebt);
                csG_checkGC(ts);
            }
            gs->gcstop = old_gcstop;
            if (gcdebt > 0 && gs->gcstate == GCSpause) /* end of cycle? */
                res = 1; /* signal it */
            break;
        }
        case CS_GCSETPAUSE: { /* set GC pause */
            int data = va_arg(ap, int); /* percentage */
            api_check(ts, data <= CS_MAXPAUSE, "GC pause overflow");
            res = getgcparam(gs->gcpause);
            setgcparam(gs->gcpause, data);
            break;
        }
        case CS_GCSETSTEPMUL: { /* set GC step multiplier */
            int data = va_arg(ap, int); /* percentage */
            api_check(ts, data <= CS_MAXPAUSE, "GC step multiplier overflow");
            res = getgcparam(gs->gcstepmul);
            setgcparam(gs->gcstepmul, data);
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
            csG_incmode(ts);
            break;
        }
        default: res = -1; /* invalid option */
    }
    va_end(ap);
    cs_unlock(ts);
    return res;
}


CS_API void cs_setwarnf(cs_State *ts, cs_WarnFunction fwarn, void *ud) {
    cs_lock(ts);
    G_(ts)->fwarn = fwarn;
    G_(ts)->ud_warn = ud;
    cs_unlock(ts);
}


CS_API void cs_warning(cs_State *ts, const char *msg, int cont) {
    cs_lock(ts);
    csT_warning(ts, msg, cont);
    cs_unlock(ts);
}


/* Check if the value at the index has virtual method table. */
CS_API int cs_hasvmt(cs_State *ts, int index) {
    return getvmt(ts, index) != NULL;
}


/* Check if the value at the index has meta method. */
CS_API int cs_hasmetamethod(cs_State *ts, int index, cs_MM mm) {
    TValue *vmt = getvmt(ts, index);
    return (vmt ? !ttisnil(&vmt[mm]) : 0);
}


/*
** Return the length of the value at index.
** Length means different things depending on the type of the value at index.
** For strings, this is the string length; for classes, this is the number
** of methods; for instances, this is the number of fields; for userdata, this
** is the size of the block of memory allocated for userdata.
*/
CS_API cs_Unsigned cs_len(cs_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    switch (ttypetag(o)) {
        case CS_VSHRSTR: return strval(o)->shrlen;
        case CS_VLNGSTR: return strval(o)->u.lnglen;
        case CS_VCLASS: return csH_len(htval(o));
        case CS_VINSTANCE: return csH_len(insval(o)->fields);
        case CS_VHTABLE: return csH_len(htval(o));
        case CS_VARRAY: return arrval(o)->n;
        case CS_VUSERDATA: return uval(o)->size;
        default: return 0;
    }
}


CS_API int cs_next(cs_State *ts, int obj) {
    HTable *ht;
    int more;
    cs_lock(ts);
    api_checknelems(ts, 1); /* key */
    ht = gettable(ts, obj);
    more = csH_next(ts, ht, ts->sp.p - 1);
    if (more) {
        api_inctop(ts);
    } else
        ts->sp.p--; /* remove key */
    cs_unlock(ts);
    return more;
}


CS_API void cs_concat(cs_State *ts, int n) {
    cs_lock(ts);
    api_checknelems(ts, n);
    if (n > 0) {
        csV_concat(ts, n);
    } else { /* nothing to concatenate */
        setstrval2s(ts, ts->sp.p, csS_newl(ts, "", 0));
        api_inctop(ts);
    }
    csG_checkGC(ts);
    cs_unlock(ts);
}


CS_API size_t cs_stringtonumber(cs_State *ts, const char *s, int *f) {
    size_t sz = csS_tonum(s, s2v(ts->sp.p), f);
    if (sz != 0) /* no conversion errors? */
        api_inctop(ts);
    return sz;
}


CS_API cs_Alloc cs_getallocf(cs_State *ts, void **ud) {
    cs_Alloc falloc;
    cs_lock(ts);
    if (ud) *ud = G_(ts)->ud_alloc;
    falloc = G_(ts)->falloc;
    cs_unlock(ts);
    return falloc;
}


CS_API void cs_setallocf(cs_State *ts, cs_Alloc falloc, void *ud) {
    cs_lock(ts);
    G_(ts)->falloc = falloc;
    G_(ts)->ud_alloc = ud;
    cs_unlock(ts);
}


CS_API void cs_toclose(cs_State *ts, int index) {
    SPtr o;
    int nresults;
    cs_lock(ts);
    o = index2stack(ts, index);
    api_check(ts, ts->tbclist.p < o,
                  "given level below or equal to the last marked slot");
    csF_newtbcvar(ts, o); /* create new to-be-closed upvalue */
    nresults = ts->cf->nresults;
    if (!hastocloseCfunc(nresults)) /* function not yet marked? */
        ts->cf->nresults = codeNresults(nresults); /* mark it */
    cs_assert(hastocloseCfunc(ts->cf->nresults)); /* must be marked */
    cs_unlock(ts);
}


CS_API void cs_closeslot(cs_State *ts, int index) {
    SPtr level;
    cs_lock(ts);
    level = index2stack(ts, index);
    api_check(ts, hastocloseCfunc(ts->cf->nresults) && ts->tbclist.p == level,
                  "no variable to close at the given level");
    csF_close(ts, level, CLOSEKTOP);
    setnilval(s2v(level)); /* closed */
    cs_unlock(ts);
}


/*
** Sets `frame` in `cs_DebugInfo`; `level` is `CallFrame` level.
** To traverse the call stack backwards (up), then level should be
** greater than 0. For example if you wish for currently active `CallFrame`,
** then `level` should be 0, if `level` is 1 then the `CallFrame` of the
** function that called the current function is considered.
** If `level` is found, therefore `cf` is set, then this function returns 1,
** otherwise 0.
*/
CS_API int cs_getstack(cs_State *ts, int level, cs_DebugInfo *di) {
    int status;
    CallFrame *cf;
    if (level < 0) return 0; /* invalid (negative) level */
    cs_lock(ts);
    for (cf = ts->cf; level > 0 && cf != &ts->basecf; cf = cf->prev)
        level--;
    if (level == 0 && cf != &ts->basecf) { /* level found ? */
        di->cf = cf;
        status = 1;
    } else /* otherwise no such level */
        status = 0;
    cs_unlock(ts);
    return status;
}


static const char *auxupvalue(TValue *func, int n, TValue **val,
                               GCObject **owner) {
    switch (ttypetag(func)) {
        case CS_VCCL: { /* C closure */
            CClosure *ccl = clCval(func);
            if (!(cast_uint(n) < cast_uint(ccl->nupvalues)))
                return NULL;  /* `n` not in [0, cl->nupvalues) */
            *val = &ccl->upvals[n];
            if (owner)
                *owner = obj2gco(ccl);
            return "";
        }
        case CS_VCSCL: { /* CScript closure */
            OString *name;
            CSClosure *cl = clCSval(func);
            Proto *p = cl->p;
            if (!(cast_uint(n) < cast_uint(p->sizeupvals)))
                return NULL;  /* `n` not in [0, fn->sizeupvals) */
            *val = cl->upvals[n]->v.p;
            if (owner)
                *owner = obj2gco(cl->upvals[n]);
            name = p->upvals[n].name;
            cs_assert(name != NULL);
            return getstr(name);
        }
        default: return NULL; /* not a closure */
    }
}


CS_API const char *cs_getupvalue(cs_State *ts, int index, int n) {
    const char *name;
    TValue *upval = NULL;
    cs_lock(ts);
    name = auxupvalue(index2value(ts, index), n, &upval, NULL);
    if (name) { /* have upvalue ? */
        setobj2s(ts, ts->sp.p, upval);
        api_inctop(ts);
    }
    cs_unlock(ts);
    return name;
}


CS_API const char *cs_setupvalue(cs_State *ts, int index, int n) {
    const char *name;
    TValue *upval = NULL;
    GCObject *owner = NULL;
    TValue *func;
    cs_lock(ts);
    api_checknelems(ts, 1); /* value */
    func = index2value(ts, index);
    name = auxupvalue(func, n, &upval, &owner);
    if (name) { /* found upvalue ? */
        setobj(ts, upval, s2v(--ts->sp.p));
        csG_barrier(ts, owner, upval);
    }
    cs_unlock(ts);
    return name;
}
