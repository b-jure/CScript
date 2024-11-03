/*
** capi.c
** CScript API
** See Copyright Notice in cscript.h
*/

#include "carray.h"
#include "cfunction.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cprotected.h"
#include "cscript.h"
#include "cconf.h"
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


/* test for pseudo index */
#define ispseudo(i)		((i) <= CR_REGISTRYINDEX)

/* test for upvalue */
#define isupvalue(i)		((i) < CR_REGISTRYINDEX)

/* test for valid index */
#define isvalid(ts,o)           (!ttisnil(o) || (o) != &G_(ts)->nil)


/* 
** Convert index to a pointer to its value.
** Invalid indices (using upvalue index for CScript functions) return
** special nil value '&G_(ts)->nil'.
*/
static TValue *index2value(const cr_State *ts, int index) {
    CallFrame *cf = ts->cf;
    if (index >= 0) {
        api_check(ts, index < (ts->sp.p - cf->func.p - 1), "index too large");
        return s2v(cf->func.p + index);
    } else if (!ispseudo(index)) { /* negative index? */
        api_check(ts, -index <= ts->sp.p - cf->func.p, "index too small");
        return s2v(ts->sp.p + index);
    } else if (index == CR_REGISTRYINDEX) { /* global variables table? */
        return &G_(ts)->globals;
    } else { /* upvalues */
        index = CR_REGISTRYINDEX - index;
        api_check(ts, index < MAXUPVAL, "upvalue index too large");
        if (cr_likely(ttisccl(s2v(cf->func.p)))) { /* C closure? */
            CClosure *ccl = cclval(s2v(cf->func.p));
            return &ccl->upvals[index];
        } else { /* CScript function (invalid) */
            api_check(ts, 0, "caller not a C closure");
            return &G_(ts)->nil; /* no upvalues */
        }
    }
}


static SPtr index2stack(const cr_State *ts, int index) {
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


CR_API cr_CFunction cr_atpanic(cr_State *ts, cr_CFunction fpanic) {
    cr_CFunction old_panic;
    cr_lock(ts);
    old_panic = G_(ts)->fpanic;
    G_(ts)->fpanic = fpanic;
    cr_unlock(ts);
    return old_panic;
}


CR_API cr_Number cr_version(cr_State *ts) {
    UNUSED(ts);
    return CR_VERSION_NUMBER;
}


/* auxiliary function that sets the stack top without locking */
cr_inline void auxsettop(cr_State *ts, int index) {
    CallFrame *cf;
    SPtr func, newtop;
    ptrdiff_t diff;
    cf = ts->cf;
    func = cf->func.p;
    if (index >= 0) {
        api_check(ts, index <= (cf->top.p - func + 1), "new top too large");
        diff = (func + 1 + index) - ts->sp.p;
        for (; diff > 0; diff--)
            setnilval(s2v(ts->sp.p++));
    } else { /* negative index */
        api_check(ts, -(index+1) <= (ts->sp.p-(func+1)), "new top underflow");
        diff = index + 1;
    }
    newtop = ts->sp.p + diff;
    if (diff < 0 && ts->tbclist.p >= newtop) {
        cr_assert(hastocloseCfunc(cf->nresults));
        crF_close(ts, newtop, CLOSEKTOP);
    }
    ts->sp.p = newtop; /* set new top */
}


/* 
** Sets the stack top to 'index'.
** If new top is greater than the previous one, new values are elements
** are filled with 'nil'.
** If index is 0, then all stack elements are removed.
** This function can run '__close' if it removes an index from the stack
** marked as to-be-closed.
*/
CR_API void cr_settop(cr_State *ts, int index) {
    cr_lock(ts);
    auxsettop(ts, index);
    cr_unlock(ts);
}


/* 
** Return the index of the top element on the stack.
** This result is equal to the number of elements on the stack - 1.
** Negative value (-1) means an empty stack.
*/
CR_API int cr_gettop(const cr_State *ts) {
    return cast_int(ts->sp.p - (ts->cf->func.p + 1) - 1);
}


/* 
** Convert 'acceptable' stack index into an absolute index.
** For example, if there are 5 values on the stack then passing
** -1 as the value of 'index' would be return 4. 
*/
CR_API int cr_absindex(cr_State *ts, int index)
{
    return (index >= 0 || ispseudo(index))
            ? index
            : cast_int(ts->sp.p - ts->cf->func.p - 1) + index;
}


/* 
** Auxiliary to 'cr_rotate', reverses stack values 'from' until 'to'.
** Note that this does not perform a deep copy.
*/
cr_sinline void reverse(cr_State *ts, SPtr from, SPtr to) {
    while (from < to) {
        TValue aux;
        setobj(ts, &aux, s2v(from));
        setobjs2s(ts, from, to);
        setobj2s(ts, to, &aux);
        from++, to--;
    }
}


/*
** Stack-array rotation between the top of the stack and the 'index' for 'n'
** times elements. Negative '-n' indicates left-rotation, while positive 'n'
** right-rotation. The absolute value of 'n' must not be greater than the size
** of the slice being rotated.
** Note that 'index' must be in stack.
**
** Example right-rotation:
** [func][0][1][2][3][4]
** cr_rotate(ts, 2, 2);
** [func][0][1][3][4][2]
**
** Example left-rotation:
** [func][0][1][2][3][4]
** cr_rotate(ts, 2, -2);
** [func][0][1][4][3][2]
*/
CR_API void cr_rotate(cr_State *ts, int index, int n) {
    SPtr start, end, pivot;
    cr_lock(ts);
    end = ts->sp.p - 1; /* end of segment */
    start = index2stack(ts, index); /* start of segment */
    api_check(ts, (n >= 0 ? n : -n) <= (end - start + 1), "invalid 'n'");
    pivot = (n >= 0 ? end - n : start - n - 1); /* end of prefix */
    reverse(ts, start, pivot);
    reverse(ts, pivot + 1, end);
    reverse(ts, start, end);
    cr_unlock(ts);
}


/* 
** Copy value at index 'src' to stack slot at index 'dest'.
** Note that 'dest' must be stack index.
*/
CR_API void cr_copy(cr_State *ts, int src, int dest) {
    TValue *from, *to;
    cr_lock(ts);
    from = index2value(ts, src);
    to = index2value(ts, dest);
    api_check(ts, isvalid(ts, to), "invalid index");
    setobj(ts, to, from);
    if (isupvalue(dest)) /* C closure upvalue? */
        crG_barrier(ts, crclval(s2v(ts->cf->func.p)), from);
    cr_unlock(ts);
}


/*
** Check if stack has enough space for 'n' elements,
** if not ensure it does.
*/
CR_API int cr_checkstack(cr_State *ts, int n) {
    CallFrame *cf;
    int res;
    cr_lock(ts);
    cf = ts->cf;
    api_check(ts, n >= 0, "negative 'n'");
    if (cf->top.p - ts->sp.p >= n) /* stack large enough? */
        res = 1;
    else /* need to grow the stack */
        res = crT_growstack(ts, n, 0);
    if (res && cf->top.p < ts->sp.p + n)
        cf->top.p = ts->sp.p + n; /* adjust top */
    cr_unlock(ts);
    return res;
}


/* push without lock */
#define pushvalue(ts, index) \
    { setobj2s(ts, ts->sp.p, index2value(ts, index)); api_inctop(ts); }


/*
** Push value at the index on to the top of the stack.
*/
CR_API void cr_push(cr_State *ts, int index) {
    cr_lock(ts);
    pushvalue(ts, index);
    cr_unlock(ts);
}


/*
** Exchange values between two threads of the same state.
** This function pops 'n' values from 'src' and pushes them
** onto the stack of 'dest'.
*/
CR_API void cr_xmove(cr_State *src, cr_State *dest, int n) {
    if (src == dest) return; /* same thread ? */
    cr_lock(dest);
    api_checknelems(src, n); /* have enough elements to move? */
    api_check(src, G_(src) == G_(dest), "moving between different states");
    api_check(src, dest->cf->top.p - dest->sp.p >= n, "dest stack overflow");
    src->sp.p -= n;
    for (int i = 0; i < n; i++) {
        setobjs2s(dest, dest->sp.p, src->sp.p + i);
        dest->sp.p++; /* already checked by 'api_check' */
    }
    cr_unlock(dest);
}


/* Check if the value at index is a number. */
CR_API int cr_is_number(cr_State *ts, int index) {
    cr_Number n;
    const TValue *o = index2value(ts, index);
    return tonumber(o, &n);
}


/* Check if the value at index is an integer. */
CR_API int cr_is_integer(cr_State *ts, int index) {
    cr_Integer i;
    const TValue *o = index2value(ts, index);
    return tointeger(o, &i);
}


/* Check if the value at index is a string. */
CR_API int cr_is_string(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return ttisstr(o);
}


/* Check if the value at index is a C function. */
CR_API int cr_is_cfunction(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttiscfn(o) || ttisccl(o));
}


/* Check if the value at index is a userdata. */
CR_API int cr_is_userdata(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttislud(o) || ttisud(o));
}


cr_sinline TValue *getvvmt(cr_State *ts, const TValue *o) {
    switch (ttype(o)) {
        case CR_VINSTANCE: return insval(o)->oclass->vmt;
        case CR_TCLASS: return clsval(o)->vmt;
        case CR_TUDATA: return udval(o)->vmt;
        default: return G_(ts)->vmt[ttype(o)];
    }
}


cr_sinline TValue *getvmt(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return getvvmt(ts, o);
}


/* 
** Return the type of the value at valid index or CR_TNONE
** if index is invalid.
** The types returned are defined in 'cscript.h' (CR_T*).
*/
CR_API int cr_type(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (isvalid(ts, o) ? ttype(o) : CR_TNONE);
}


/*
** Return text representation of the given type.
** The returned string has a static lifetime and should not be modified
** directly.
*/
CR_API const char *cr_typename(cr_State *ts, int type) {
    UNUSED(ts);
    api_check(ts, 0 <= type && type < CR_NUM_TYPES, "invalid type");
    return typename(type);
}


/*
** Returns the number value of the value at index.
** The fact whether the value was a number is stored in 'pisnum' if
** provided, the default value returned when index is not a number is 0.0.
*/
CR_API cr_Number cr_to_numberx(cr_State *ts, int index, int *pisnum) {
    cr_Number n = 0.0;
    const TValue *o = index2value(ts, index);
    int isnum = tonumber(o, &n);
    if (pisnum)
        *pisnum = isnum;
    return n;
}


/*
** Returns the integer value of the value at index.
** The fact whether the value was an integer is stored in 'pisint' if
** provided, the default value returned when index is not an integer is 0.
*/
CR_API cr_Integer cr_to_integerx(cr_State *ts, int index, int *pisint) {
    cr_Integer i = 0;
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
CR_API int cr_to_bool(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return !cri_isfalse(o);
}


/*
** Return string value from value at index and if 'plen' is provided
** set it to match the length of the string.
** If the value is not a string return NULL (in this case if 'plen' is
** provided it is ignored).
*/
CR_API const char *cr_to_lstring(cr_State *ts, int index, size_t *plen) {
    const TValue *o;
    cr_lock(ts);
    o = index2value(ts, index);
    if (!ttisstr(o)) /* not a string? */
        return NULL;
    if (plen)
        *plen = lenstr(o); 
    cr_unlock(ts);
    return cstrval(o);
}


/*
** Return 'cr_CFunction' from the value at index.
** If the value is not a C closure or light C function, then this returns NULL.
*/
CR_API cr_CFunction cr_to_cfunction(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    if (ttiscfn(o)) 
        return cfval(o);
    else if (ttisccl(o))
        return cclval(o)->fn;
    else
        return NULL;
}


cr_sinline void *touserdata(const TValue *o) {
    switch (ttype(o)) {
        case CR_TLUDATA: return pval(o);
        case CR_TUDATA: return getudmem(udval(o));
        default: return NULL;
    }
}


/*
** Return pointer to userdata memory from the value at index.
** If the value is not userdata return NULL.
*/
CR_API void *cr_to_userdata(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return touserdata(o);
}


/*
** Return the pointer to the object at index.
** If the object is not userdata, C function or collectable, then this
** returns NULL. Note that returned pointer shouldn't be modified.
*/
CR_API const void *cr_to_pointer(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    switch (ttypetag(o)) {
        case CR_VCFUNCTION:
            return cast(void *, cast_sizet(cfval(o)));
        case CR_VUDATA: case CR_VLUDATA:
            return touserdata(o);
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
CR_API cr_State *cr_to_thread(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttisthread(o) ? thval(o) : NULL);
}


/*
** Perform arithmetic operation 'op'; the valid arithmetic operations are
** located in 'cscript.h'.
** This functions is free to call overloadable methods.
*/
CR_API void cr_arith(cr_State *ts, int op) {
    cr_lock(ts);
    if (op != CR_OPUNM && op != CR_OPBNOT) { /* binary op? */
        api_checknelems(ts, 2);
        crV_binarithm(ts, s2v(ts->sp.p-2), s2v(ts->sp.p-1), ts->sp.p-2, op);
        ts->sp.p--; /* pop second operand */
    } else { /* unary op */
        api_checknelems(ts, 1);
        crV_unarithm(ts, s2v(ts->sp.p-1), ts->sp.p-1, op);
    }
    cr_unlock(ts);
}


/*
** Perform raw equality between values at 'index1' and 'index2'.
** "Raw" meaning this function won't call overloaded methods.
** In cases where either of the indexes is not valid this always returns 0.
*/
CR_API int cr_rawequal(cr_State *ts, int index1, int index2) {
    const TValue *lhs = index2value(ts, index1);
    const TValue *rhs = index2value(ts, index2);
    return (isvalid(ts, lhs) && isvalid(ts, rhs) ? crV_raweq(lhs, rhs) : 0);
}


/*
** Compares values at 'index1' and 'index2'.
** This function is free to call overloded methods.
** In case either index is invalid this then always returns 0.
*/
CR_API int cr_compare(cr_State *ts, int index1, int index2, int op) {
    const TValue *lhs;
    const TValue *rhs;
    int res = 0;
    cr_lock(ts); /* might call overloaded method */
    lhs = index2value(ts, index1);
    rhs = index2value(ts, index2);
    if (isvalid(ts, lhs) && isvalid(ts, rhs)) {
        switch (op) {
            case CR_OPEQ: res = crV_ordereq(ts, lhs, rhs);
            case CR_OPLT: res = crV_orderlt(ts, lhs, rhs);
            case CR_OPLE: res = crV_orderle(ts, lhs, rhs);
            default: api_check(ts, 0, "invalid 'op'");
        }
    }
    cr_unlock(ts);
    return res;
}


/* Push nil value on top of the stack. */
CR_API void cr_push_nil(cr_State *ts) {
    cr_lock(ts);
    setnilval(s2v(ts->sp.p));
    api_inctop(ts);
    cr_unlock(ts);
}


/* Push 'cr_Number' value on top of the stack. */
CR_API void cr_push_number(cr_State *ts, cr_Number n) {
    cr_lock(ts);
    setfval(s2v(ts->sp.p), n);
    api_inctop(ts);
    cr_unlock(ts);
}


/* Push 'cr_Integer' value on top of the stack. */
CR_API void cr_push_integer(cr_State *ts, cr_Integer i) {
    cr_lock(ts);
    setival(s2v(ts->sp.p), i);
    api_inctop(ts);
    cr_unlock(ts);
}


/* Push string value of length 'len' on top of the stack. */
CR_API const char *cr_push_lstring(cr_State *ts, const char *str, size_t len) {
    OString *s;
    cr_lock(ts);
    s = (len == 0 ? crS_new(ts, "") : crS_newl(ts, str, len));
    setstrval2s(ts, ts->sp.p, s);
    api_inctop(ts);
    crG_check(ts);
    cr_unlock(ts);
    return getstrbytes(s);
}


/* Push null terminated string value on top of the stack. */
CR_API const char *cr_push_string(cr_State *ts, const char *str) {
    cr_lock(ts);
    if (str == NULL) {
        setnilval(s2v(ts->sp.p));
    } else {
        OString *s = crS_new(ts, str);
        setstrval2s(ts, ts->sp.p, s);
        str = getstrbytes(s);
    }
    api_inctop(ts);
    crG_check(ts);
    cr_unlock(ts);
    return str;
}


/* 
** Push formatted string with format values in a 'va_list' on top of the stack.
** Valid format values are c (char), d (int), I (cr_Integer), N (cr_Number),
** s (string), p (pointer) and % ('%'). Note that each format specifier is
** preceeded by '%' (so %I...).
*/
CR_API const char *cr_push_vfstring(cr_State *ts, const char *fmt, va_list argp) {
    const char *str;
    cr_lock(ts);
    str = crS_pushvfstring(ts, fmt, argp);
    crG_check(ts);
    cr_unlock(ts);
    return str;
}


/* 
** Push formatted string with variable amount of format values on top of the
** stack. Valid format specifiers are the same as in 'cr_pushvfstring'.
*/
CR_API const char *cr_push_fstring(cr_State *ts, const char *fmt, ...) {
    const char *str;
    va_list argp;
    cr_lock(ts);
    va_start(argp, fmt);
    str = crS_pushvfstring(ts, fmt, argp);
    va_end(argp);
    crG_check(ts);
    cr_unlock(ts);
    return str;
}


/* auxiliary function, pushes C closure without locking */
cr_sinline void auxpushcclosure(cr_State *ts, cr_CFunction fn, int nupvals) {
    if (nupvals == 0) {
        setcfval(s2v(ts->sp.p), fn);
        api_inctop(ts);
    } else {
        CClosure *ccl;
        api_checknelems(ts, nupvals);
        ccl = crF_newCClosure(ts, nupvals);
        ccl->fn = fn;
        ts->sp.p -= nupvals;
        while (nupvals--) {
            setobj(ts, &ccl->upvals[nupvals], s2v(ts->sp.p + nupvals));
            cr_assert(iswhite(ccl));
        }
        setccl2s(ts, ts->sp.p, ccl);
        api_inctop(ts);
        crG_check(ts);
    }
}


/*
** Push C closure value on top of the stack.
** This closure will have 'nupvals' upvalues, these values will be popped
** of the stack and inserted into the closure.
** If 'nupvals' is 0 then this does not create a C closure, rather it only
** pushes the 'cr_CFunction' on top of the stack.
*/
CR_API void cr_push_cclosure(cr_State *ts, cr_CFunction fn, int nupvals) {
    cr_lock(ts);
    auxpushcclosure(ts, fn, nupvals);
    cr_unlock(ts);
}


/*
** Push boolean value on top of the stack.
** If 'b' is 0 then `false` is pushed, otherwise `true`.
*/
CR_API void cr_push_bool(cr_State *ts, int b) {
    cr_lock(ts);
    if (b)
        setbtval(s2v(ts->sp.p));
    else
        setbfval(s2v(ts->sp.p));
    api_inctop(ts);
    cr_unlock(ts);
}


/* Push light userdata on top of the stack. */
CR_API void cr_push_lightuserdata(cr_State *ts, void *p) {
    cr_lock(ts);
    setpval(s2v(ts->sp.p), p);
    api_inctop(ts);
    cr_unlock(ts);
}


/* Push array on top of the stack. */
CR_API void cr_push_array(cr_State *ts) {
    Array *arr;
    cr_lock(ts);
    arr = crA_new(ts);
    setarr2s(ts, ts->sp.p, arr);
    api_inctop(ts);
    cr_unlock(ts);
}


/*
** Push thread 'ts' on top of the stack.
** Returns non-zero if the pushed thread is main thread.
*/
CR_API int cr_push_thread(cr_State *ts) {
    cr_lock(ts);
    setsv2th(ts, ts->sp.p, ts);
    api_inctop(ts);
    cr_unlock(ts);
    return (G_(ts)->mainthread == ts);
}


CR_API void cr_push_instance(cr_State *ts, int clsobj) {
    const TValue *o;
    SPtr func;
    cr_lock(ts);
    o = index2value(ts, clsobj);
    api_check(ts, ttiscls(o), "expect class");
    func = ts->sp.p;
    setcls2s(ts, func, clsval(o));
    api_inctop(ts);
    crV_call(ts, func, 1);
    cr_assert(ttisins(s2v(ts->sp.p))); /* result is the instance */
    cr_unlock(ts);
}


cr_sinline void auxsetvmt(TValue *dest, const cr_VMT *vmt) {
    for (int i = 0; i < CR_NUM_MM; i++)
        setcfval(&dest[i], vmt->func[i]);
}


cr_sinline void auxclearvmt(TValue *vmt) {
    for (int i = 0; i < CR_NUM_MM; i++)
        setnilval(&vmt[i]);
}


#define fastget(ts,ht,k,slot,f)     ((slot = f(ht, k)), !isabstkey(slot))

#define finishfastset(ts,ht,slot,v) \
    { setobj(ts, cast(TValue *, slot), v); \
      crG_barrierback(ts, obj2gco(ht), v); }


cr_sinline void auxrawsetstr(cr_State *ts, HTable *ht, const char *str,
                             const TValue *v) {
    const TValue *slot;
    OString *s = crS_new(ts, str);
    if (fastget(ts, ht, s, slot, crH_getstr)) {
        finishfastset(ts, ht, slot, s2v(ts->sp.p - 1));
        ts->sp.p--; /* pop value */
    } else {
        setstrval2s(ts, ts->sp.p, s);
        api_inctop(ts);
        crH_set(ts, ht, s2v(ts->sp.p - 1), v);
        ts->sp.p -= 2; /* pop string key and value */
    }
}


cr_sinline void auxsetentrylist(cr_State *ts, OClass *cls, cr_Entry *list,
                                int nup) {
    cr_checkstack(ts, nup);
    if (list->name && !cls->methods) { /* have entry and no method table? */
        cls->methods = crH_new(ts);
        crG_check(ts);
    }
    for (; list->name; list++) {
        api_check(ts, list->func, "entry 'func' is NULL");
        for (int i = 0; i < nup; i++) /* push upvalues to the top */
            pushvalue(ts, -nup);
        auxpushcclosure(ts, list->func, nup);
        auxrawsetstr(ts, cls->methods, list->name, s2v(ts->sp.p - 1));
    }
    auxsettop(ts, -(nup - 1)); /* pop upvalues */
}


CR_API void cr_push_class(cr_State *ts, cr_VMT *vmt, int clsobjabs, int nup,
                          cr_Entry *entries) {
    OClass *cls;
    cr_lock(ts);
    cls = crMM_newclass(ts);
    crG_check(ts);
    setcls2s(ts, ts->sp.p, cls);
    api_inctop(ts);
    if (clsobjabs >= 0) { /* have superclass? */
        const TValue *osup = index2value(ts, clsobjabs);
        api_check(ts, ttiscls(osup), "expect class");
        if (clsval(osup)->methods) { /* have methods? */
            cls->methods = crH_new(ts);
            crH_copykeys(ts, clsval(osup)->methods, cls->methods);
        }
    }
    if (vmt) { /* have Virtual Method Table? */
        cls->vmt = crMM_newvmt(ts);
        crG_check(ts);
        auxsetvmt(cls->vmt, vmt);
    }
    if (entries) /* have methods? */
        auxsetentrylist(ts, cls, entries, nup);
    else /* otherwise there should be no upvalues */
        api_check(ts, nup == 0, "'nup' non-zero but 'entries' NULL");
    cr_unlock(ts);
}



/* CScript -> stack */


#define setslot2stack(ts,slot,sptr) \
    { if (!isabstkey(slot)) { setobj2s(ts, sptr, slot); } \
      else { setnilval(s2v(sptr)); }}


#define setslot2stacktop(ts,slot)   setslot2stack(ts,slot,(ts)->sp.p - 1)


cr_sinline int finishrawget(cr_State *ts, const TValue *slot) {
    setslot2stack(ts, slot, ts->sp.p);
    api_inctop(ts);
    cr_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


/* auxiliary raw function for getting the value of the string 'k' */
cr_sinline int auxrawgetstr(cr_State *ts, HTable *ht, const char *k) {
    OString *key = crS_new(ts, k);
    return finishrawget(ts, crH_getstr(ht, key));
}


/*
** Gets the global variable 'name' value and pushes it on top of the stack.
** This function returns the value type.
*/
CR_API int cr_get_global(cr_State *ts, const char *name) {
    HTable *G;
    cr_lock(ts);
    G = htval(&G_(ts)->globals);
    return auxrawgetstr(ts, G, name);
}


CR_API int cr_get(cr_State *ts, int obj) {
    const TValue *o;
    cr_lock(ts);
    api_checknelems(ts, 1); /* key */
    o = index2value(ts, obj);
    crV_get(ts, o, s2v(ts->sp.p - 1), ts->sp.p - 1);
    cr_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


cr_sinline Array *getarray(cr_State *ts, int arrobj) {
    const TValue *o = index2value(ts, arrobj);
    api_check(ts, ttisarr(o), "expect array");
    return arrval(o);
}


CR_API int cr_get_index(cr_State *ts, int arrobj, cr_Integer index) {
    Array *arr;
    cr_lock(ts);
    api_check(ts, index >= 0 && index < ARRAYLIMIT, "invalid 'index'");
    arr = getarray(ts, arrobj);
    if (cri_castS2U(index) < arr->n) {
        setobj2s(ts, ts->sp.p, &arr->b[index]);
    } else
        setnilval(s2v(ts->sp.p));
    api_inctop(ts);
    cr_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


cr_sinline Instance *getinstance(cr_State *ts, int insobj) {
    const TValue *o = index2value(ts, insobj);
    api_check(ts, ttisins(o), "expect instance");
    return insval(o);
}


cr_sinline int auxrawget(cr_State *ts, HTable *ht, const TValue *key) {
    return finishrawget(ts, crH_get(ht, key));
}


CR_API int cr_get_field(cr_State *ts, int insobj) {
    Instance *ins;
    cr_lock(ts);
    api_checknelems(ts, 1); /* key */
    ins = getinstance(ts, insobj);
    return auxrawget(ts, &ins->fields, s2v(--ts->sp.p));

}


CR_API int cr_get_class(cr_State *ts, int insobj) {
    const TValue *o;
    int tt;
    cr_lock(ts);
    o = index2value(ts, insobj);
    if (ttisins(o)) {
        setcls2s(ts, ts->sp.p, insval(o)->oclass);
        api_inctop(ts);
        tt = CR_TCLASS;
    } else {
        tt = CR_TNONE;
    }
    cr_unlock(ts);
    return tt;
}


CR_API int cr_get_method(cr_State *ts, int insobj, const char *name) {
    Instance *ins;
    OString *key;
    cr_lock(ts);
    ins = getinstance(ts, insobj);
    key = crS_new(ts, name);
    setstrval2s(ts, ts->sp.p, key); /* anchor */
    api_inctop(ts);
    if (ins->oclass->methods) { /* have methods ? */
        const TValue *slot = crH_getstr(ins->oclass->methods, key);
        if (!isabstkey(slot)) { /* have 'name' ? */
            IMethod *im = crMM_newinsmethod(ts, ins, slot);
            setim2s(ts, ts->sp.p - 1, im);
            goto unlock;
        } /* else fallthrough */
    }
    setnilval(s2v(ts->sp.p - 1));
unlock:
    cr_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


CR_API int cr_get_metamethod(cr_State *ts, int obj, cr_MM mm) {
    TValue *vmt;
    int tt;
    cr_lock(ts);
    vmt = getvmt(ts, obj);
    if (vmt) {
        setobj2s(ts, ts->sp.p, &vmt[mm]);
        api_inctop(ts);
        tt = ttype(s2v(ts->sp.p - 1)); 
    } else {
        tt = CR_TNONE;
    }
    cr_unlock(ts);
    return tt;
}


CR_API void *cr_newuserdata(cr_State *ts, size_t sz, int nuv) {
    UserData *ud;
    cr_lock(ts);
    api_checknelems(ts, nuv);
    ud = crMM_newuserdata(ts, sz, nuv);
    ts->sp.p -= nuv;
    while (nuv--)
        setobj(ts, &ud->uv[nuv], s2v(ts->sp.p + nuv));
    ud->nuv = nuv;
    setud2s(ts, ts->sp.p, ud);
    api_inctop(ts);
    cr_unlock(ts);
    return getudmem(ud);
}


cr_sinline UserData *getuserdata(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    api_check(ts, ttisud(o), "expect userdata");
    return udval(o);
}


CR_API int cr_get_uservalue(cr_State *ts, int udobj, int n) {
    UserData *ud;
    int tt;
    cr_lock(ts);
    ud = getuserdata(ts, udobj);
    if (0 < n || ud->nuv < n) {
        setnilval(s2v(ts->sp.p));
        tt = CR_TNONE;
    } else {
        setobj2s(ts, ts->sp.p, &ud->uv[n]);
        tt = ttype(s2v(ts->sp.p - 1));
    }
    api_inctop(ts);
    cr_unlock(ts);
    return tt;
}


CR_API void cr_set_global(cr_State *ts, const char *name) {
    HTable *G;
    cr_lock(ts);
    api_checknelems(ts, 1); /* value */
    G = htval(&G_(ts)->globals);
    auxrawsetstr(ts, G, name, s2v(ts->sp.p - 1));
    cr_unlock(ts);
}


CR_API void cr_set(cr_State *ts, int obj) {
    TValue *o;
    cr_lock(ts);
    api_checknelems(ts, 2); /* value and key */
    o = index2value(ts, obj);
    crV_set(ts, o, s2v(ts->sp.p - 1), s2v(ts->sp.p - 2));
    ts->sp.p -= 2; /* remove value and key */
    cr_unlock(ts);
}


CR_API void cr_set_index(cr_State *ts, int arrobj, cr_Integer index) {
    Array *arr;
    cr_lock(ts);
    api_checknelems(ts, 1); /* value */
    api_check(ts, index >= 0 && index < ARRAYLIMIT, "invalid 'index'");
    arr = getarray(ts, arrobj);
    crA_ensure(ts, arr, index);
    setobj(ts, &arr->b[index], s2v(ts->sp.p - 1));
    ts->sp.p--; /* pop value */
    cr_unlock(ts);
}


CR_API void cr_set_field(cr_State *ts, int index, const char *field) {
    Instance *ins;
    cr_lock(ts);
    api_checknelems(ts, 1);
    ins = getinstance(ts, index);
    auxrawsetstr(ts, &ins->fields, field, s2v(ts->sp.p - 1));
    cr_unlock(ts);
}


CR_API void cr_set_userdatavmt(cr_State *ts, int index, const cr_VMT *vmt) {
    UserData *ud;
    cr_lock(ts);
    ud = getuserdata(ts, index);
    if (!ud->vmt) {
        ud->vmt = crMM_newvmt(ts);
        crG_check(ts);
    }
    if (vmt)
        auxsetvmt(ud->vmt, vmt);
    else
        auxclearvmt(ud->vmt);
    cr_unlock(ts);

}


CR_API int cr_set_uservalue(cr_State *ts, int index, int n) {
    UserData *ud;
    int res;
    cr_lock(ts);
    api_checknelems(ts, 1); /* value */
    ud = getuserdata(ts, index);
    if (!(cast_uint(n) <= cast_uint(ud->nuv))) {
        res = 0; /* 'n' not in [0, ud->nuv) */
    } else {
        setobj(ts, &ud->uv[n], s2v(ts->sp.p - 1));
        crG_barrierback(ts, obj2gco(ud), s2v(ts->sp.p - 1));
        res = 1;
    }
    ts->sp.p--; /* remove value */
    cr_unlock(ts);
    return res;
}


CR_API void cr_set_userdatamm(cr_State *ts, int index, cr_MM mm) {
    UserData *ud;
    cr_lock(ts);
    api_checknelems(ts, 1); /* metamethod func */
    ud = getuserdata(ts, index);
    if (!ud->vmt) {
        ud->vmt = crMM_newvmt(ts);
        crG_check(ts);
    }
    setobj(ts, &ud->vmt[mm], s2v(ts->sp.p - 1));
    crG_barrierback(ts, obj2gco(ud), s2v(ts->sp.p - 1));
    ts->sp.p--;
    cr_unlock(ts);
}



/* Error reporting */


CR_API int cr_status(cr_State *ts) {
    return ts->status;
}


CR_API int cr_error(cr_State *ts) {
    TValue *errobj;
    cr_lock(ts);
    api_checknelems(ts, 1); /* errobj */
    errobj = s2v(ts->sp.p - 1);
    if (ttisstr(errobj) && strval(errobj) == G_(ts)->memerror) {
        crM_error(ts); /* raise a memory error */
    } else
        crPR_throw(ts, CR_ERRRUNTIME); /* raise a regular runtime error */
    /* cr_unlock() is called when control leaves the core */
    cr_unreachable();
}



/* Call/Load CScript code */


#define checkresults(ts,nargs,nres) \
     api_check(ts, (nres) == CR_MULRET \
               || (ts->cf->top.p - ts->sp.p >= (nres) - (nargs)), \
	"results from function overflow current stack size")


CR_API void cr_call(cr_State *ts, int nargs, int nresults) {
    SPtr func;
    cr_lock(ts);
    api_checknelems(ts, nargs + 1); /* args + func */
    api_check(ts, ts->status == CR_OK, "can't do calls on non-normal thread");
    checkresults(ts, nargs, nresults);
    func = ts->sp.p - nargs - 1;
    crV_call(ts, func, nresults);
    adjustresults(ts, nresults);
    cr_unlock(ts);
}


struct PCallData {
    SPtr func;
    int nresults;
    ptrdiff_t errfunc;
};


static void fcall(cr_State *ts, void *ud) {
    struct PCallData *pcd = cast(struct PCallData*, ud);
    crV_call(ts, pcd->func, pcd->nresults);
}


CR_API int cr_pcall(cr_State *ts, int nargs, int nresults, int errfunc) {
    struct PCallData pcd;
    int status;
    cr_lock(ts);
    api_checknelems(ts, nargs + 1);
    api_check(ts, ts->status == CR_OK, "can't do calls on non-normal thread");
    checkresults(ts, nargs, nresults);
    pcd.func = ts->sp.p - nargs - 1;
    pcd.nresults = nresults;
    status = crPR_call(ts, fcall, &pcd, savestack(ts, pcd.func), errfunc);
    adjustresults(ts, nresults);
    cr_unlock(ts);
    return status;
}


CR_API int cr_load(cr_State *ts, cr_Reader reader, void *userdata,
                    const char *source) {
    BuffReader br;
    int status;
    cr_lock(ts);
    if (!source) source = "?";
    crR_init(ts, &br, reader, userdata);
    status = crPR_parse(ts, &br, source);
    cr_unlock(ts);
    return status;
}


CR_API int cr_gc(cr_State *ts, int option, ...) {
    va_list ap;
    int res = 0;
    GC *gc = &G_(ts)->gc;
    cr_lock(ts);
    va_start(ap, option);
    switch (option) {
	case CR_GCSTOP: { /* stop garbage collector */
            gc->stopped = GCSTPUSR; /* stopped by user */
            break;
        }
	case CR_GCRESTART: { /* restart GC */
            crG_setdebt(gc, 0);
            gc->stopped = 0;
            break;
        }
	case CR_GCCOLLECT: { /* start GC cycle */
            crG_full(ts, 0);
            break;
        }
	case CR_GCCOUNT: { /* total GC memory count in Kibibytes */
            res = totalbytes(gc) >> 10;
            break;
        }
	case CR_GCCOUNTBYTES: { /* remainder bytes of total memory / 1024 */
            res = totalbytes(gc) & 0x3ff; /* all bits before 10th bit */
            break;
        }
	case CR_GCSTEP: { /* perform GC step */
            int data = va_arg(ap, int); /* kibibytes */
            cr_mem debt = 69; /* >0 to signal that it did an actual step */
            cr_ubyte old_stopped = gc->stopped;
            if (data == 0) { /* do a regular step ? */
                crG_setdebt(gc, 0);
                crG_step(ts);
            } else { /* add 'data' to total debt */
                /* convert 'data' to bytes (data = bytes/2^10) */
                debt = (data * 1024) + gc->debt;
                crG_setdebt(gc, debt);
                crG_check(ts);
            }
            gc->stopped = old_stopped;
            if (debt > 0 && gc->state == GCSpause) /* end of cycle? */
                res = 1; /* signal it */
            break;
        }
        case CR_GCSETPAUSE: { /* set GC pause */
            int data = va_arg(ap, int); /* percentage */
            api_check(ts, data <= CR_MAXPAUSE, "GC pause overflow");
            res = getgcparam(gc->pause);
            setgcparam(gc->pause, data);
            break;
        }
        case CR_GCSETSTEPMUL: { /* set GC step multiplier */
            int data = va_arg(ap, int); /* percentage */
            api_check(ts, data <= CR_MAXPAUSE, "GC step multiplier overflow");
            res = getgcparam(gc->stepmul);
            setgcparam(gc->stepmul, data);
            break;
        }
	case CR_GCISRUNNING: { /* check if GC is running */
            res = gcrunning(gc);
            break;
        }
        case CR_GCINC: {
            int pause = va_arg(ap, int);
            int stepmul = va_arg(ap, int);
            int stepsize = va_arg(ap, int);
            res = CR_GCINC;
            if (pause != 0)
                setgcparam(gc->pause, pause);
            if (stepmul != 0)
                setgcparam(gc->stepmul, stepmul);
            if (stepsize != 0)
                gc->stepsize = stepsize;
            crG_incmode(ts);
            break;
        }
        default: res = -1; /* invalid option */
    }
    va_end(ap);
    cr_unlock(ts);
    return res;
}


CR_API void cr_setwarnf(cr_State *ts, cr_WarnFunction fwarn, void *ud) {
    cr_lock(ts);
    G_(ts)->fwarn = fwarn;
    G_(ts)->ud_warn = ud;
    cr_unlock(ts);
}


CR_API void cr_warning(cr_State *ts, const char *msg, int cont) {
    cr_lock(ts);
    crT_warning(ts, msg, cont);
    cr_unlock(ts);
}


/* Check if the value at the index has virtual method table. */
CR_API int cr_hasvmt(cr_State *ts, int index) {
    return getvmt(ts, index) != NULL;
}


/* Check if the value at the index has meta method. */
CR_API int cr_hasmetamethod(cr_State *ts, int index, cr_MM mm) {
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
CR_API cr_Unsigned cr_len(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    switch (ttypetag(o)) {
        case CR_VSTRING: return lenstr(o);
        case CR_VCLASS: return crH_len(htval(o));
        case CR_VINSTANCE: return crH_len(&insval(o)->fields);
        case CR_VARRAY: return arrval(o)->n;
        case CR_VUDATA: return udval(o)->size;
        default: return 0;
    }
}


cr_sinline HTable *getnextht(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    switch (ttype(o)) {
        case CR_TINSTANCE: return &insval(o)->fields;
        case CR_TCLASS: return clsval(o)->methods;
        default: {
            api_check(ts, 0, "expected instance or class");
            return NULL;
        }
    }
}


CR_API int cr_next(cr_State *ts, int index) {
    HTable *ht;
    int more;
    cr_lock(ts);
    api_checknelems(ts, 1); /* key */
    ht = getnextht(ts, index);
    if (ht) {
        more = crH_next(ts, ht, ts->sp.p - 1);
        if (more) {
            api_inctop(ts);
            goto unlock;
        }
    } else {
        more = 0;
    }
    ts->sp.p--; /* remove key */
unlock:
    cr_unlock(ts);
    return more;
}


CR_API void cr_concat(cr_State *ts, int n) {
    cr_lock(ts);
    api_checknelems(ts, n);
    if (n > 0)
        crV_concat(ts, n);
    else { /* nothing to concatenate */
        setstrval2s(ts, ts->sp.p, crS_newl(ts, "", 0));
        api_inctop(ts);
    }
    crG_check(ts);
    cr_unlock(ts);
}


CR_API size_t cr_stringtonumber(cr_State *ts, const char *s, int *povf) {
    size_t sz = crS_tonum(s, s2v(ts->sp.p), povf);
    if (sz != 0) /* no conversion errors? */
        api_inctop(ts);
    return sz;
}


CR_API cr_Alloc cr_getallocf(cr_State *ts, void **ud) {
    cr_Alloc falloc;
    cr_lock(ts);
    if (ud) *ud = G_(ts)->ud_alloc;
    falloc = G_(ts)->falloc;
    cr_unlock(ts);
    return falloc;
}


CR_API void cr_setallocf(cr_State *ts, cr_Alloc falloc, void *ud) {
    cr_lock(ts);
    G_(ts)->falloc = falloc;
    G_(ts)->ud_alloc = ud;
    cr_unlock(ts);
}


CR_API void cr_toclose(cr_State *ts, int index) {
    SPtr o;
    int nresults;
    cr_lock(ts);
    o = index2stack(ts, index);
    api_check(ts, ts->tbclist.p < o,
                  "given index below or equal to the last marked slot");
    crF_newtbcvar(ts, o); /* create new to-be-closed upvalue */
    nresults = ts->cf->nresults;
    if (!hastocloseCfunc(nresults)) /* function not yet marked? */
        ts->cf->nresults = codeNresults(nresults); /* mark it */
    cr_assert(hastocloseCfunc(ts->cf->nresults)); /* must be marked */
    cr_unlock(ts);
}


CR_API void cr_closeslot(cr_State *ts, int index) {
    SPtr level;
    cr_lock(ts);
    level = index2stack(ts, index);
    api_check(ts, hastocloseCfunc(ts->cf->nresults) && ts->tbclist.p == level,
                  "no variable to close at the given level");
    crF_close(ts, level, CLOSEKTOP);
    setnilval(s2v(level)); /* closed */
    cr_unlock(ts);
}


/*
** Sets 'frame' in 'cr_DebugInfo'; 'level' is 'CallFrame' level.
** To traverse the call stack backwards (up), then level should be
** greater than 0. For example if you wish for currently active 'CallFrame',
** then 'level' should be 0, if 'level' is 1 then the 'CallFrame' of the
** function that called the current function is considered.
** If 'level' is found, therefore 'cf' is set, then this function returns 1,
** otherwise 0.
*/
CR_API int cr_getstack(cr_State *ts, int level, cr_DebugInfo *di) {
    cr_lock(ts);
    int status = 0;
    CallFrame *cf;
    if (level > ts->ncf || level < 0) {
        cr_unlock(ts);
        return status;
    }
    for (cf = ts->cf; level > 0 && cf != &ts->basecf; cf = cf->prev)
        level--;
    if (level == 0) { /* found ? */
        di->cf = cf;
        status = 1;
    }
    cr_unlock(ts);
    return status;
}


static const char *auxupvalue(TValue *func, int n, TValue **val,
                               GCObject **owner) {
    switch (ttypetag(func)) {
        case CR_VCCL: { /* C closure */
            CClosure *ccl = cclval(func);
            if (!(cast_uint(n) < cast_uint(ccl->nupvalues)))
                return NULL;  /* 'n' not in [0, cl->nupvalues) */
            *val = &ccl->upvals[n];
            if (owner)
                *owner = obj2gco(ccl);
            return "";
        }
        case CR_VCRCL: { /* CScript closure */
            OString *name;
            CrClosure *crcl = crclval(func);
            Function *fn = crcl->fn;
            if (!(cast_uint(n) < cast_uint(fn->sizeupvals)))
                return NULL;  /* 'n' not in [0, fn->sizeupvals) */
            *val = crcl->upvals[n]->v.p;
            if (owner)
                *owner = obj2gco(crcl->upvals[n]);
            name = fn->upvals[n].name;
            cr_assert(name != NULL);
            return getstrbytes(name);
        }
        default: return NULL; /* not a closure */
    }
}


CR_API const char *cr_getupvalue(cr_State *ts, int index, int n) {
    const char *name;
    TValue *upval = NULL;
    cr_lock(ts);
    name = auxupvalue(index2value(ts, index), n, &upval, NULL);
    if (name) { /* have upvalue ? */
        setobj2s(ts, ts->sp.p, upval);
        api_inctop(ts);
    }
    cr_unlock(ts);
    return name;
}


CR_API const char *cr_setupvalue(cr_State *ts, int index, int n) {
    const char *name;
    TValue *upval = NULL;
    GCObject *owner = NULL;
    TValue *func;
    cr_lock(L);
    api_checknelems(ts, 1); /* value */
    func = index2value(ts, index);
    name = auxupvalue(func, n, &upval, &owner);
    if (name) { /* found upvalue ? */
        setobj(ts, upval, s2v(--ts->sp.p));
        crG_barrier(ts, owner, upval);
    }
    cr_unlock(L);
    return name;
}
