#include "cfunction.h"
#include "cgc.h"
#include "cmem.h"
#include "cmeta.h"
#include "cprotected.h"
#include "cscript.h"
#include "cconf.h"
#include "cdebug.h"
#include "capi.h"
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

#include <string.h>


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
** Index can be any value.
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
    return cast_int(ts->sp.p - (ts->cf->func.p + 1));
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
CR_API int cr_isnumber(cr_State *ts, int index) {
    cr_Number n;
    const TValue *o = index2value(ts, index);
    return tonumber(o, &n);
}


/* Check if the value at index is an integer. */
CR_API int cr_isinteger(cr_State *ts, int index) {
    cr_Integer i;
    const TValue *o = index2value(ts, index);
    return tointeger(o, &i);
}


/* Check if the value at index is a string. */
CR_API int cr_isstring(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return ttisstr(o);
}


/* Check if the value at index is a C function. */
CR_API int cr_iscfunction(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttiscfn(o) || ttisccl(o));
}


/* Check if the value at index is a userdata. */
CR_API int cr_isuserdata(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttislud(o) || ttisud(o));
}


cr_sinline TValue *getvvmt(cr_State *ts, const TValue *o) {
    switch (ttype(o)) {
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
CR_API cr_Number cr_tonumber(cr_State *ts, int index, int *pisnum) {
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
CR_API cr_Integer cr_tointeger(cr_State *ts, int index, int *pisint) {
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
CR_API int cr_tobool(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return !cri_isfalse(o);
}


/*
** Return string value from value at index and if 'plen' is provided
** set it to match the length of the string.
** If the value is not a string return NULL (in this case if 'plen' is
** provided it is ignored).
*/
CR_API const char *cr_tostring(cr_State *ts, int index, size_t *plen) {
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
CR_API cr_CFunction cr_tocfunction(cr_State *ts, int index) {
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
CR_API void *cr_touserdata(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return touserdata(o);
}


/*
** Return the pointer to the object at index.
** If the object is not userdata, C function or collectable, then this
** returns NULL. Note that returned pointer shouldn't be modified.
*/
CR_API const void *cr_topointer(cr_State *ts, int index) {
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
CR_API cr_State *cr_tothread(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    return (ttisthread(o) ? thval(o) : NULL);
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
        case CR_VUDATA: return udval(o)->size;
        default: return 0;
    }
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
CR_API const char *cr_push_string(cr_State *ts, const char *str, size_t len) {
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
CR_API const char *cr_push_cstring(cr_State *ts, const char *str) {
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


cr_sinline void auxsetvmt(TValue *dest, cr_VMT *vmt) {
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


cr_sinline void auxsetentrylist(cr_State *ts, OClass *cls, cr_ClassEntry *list,
                                int nup) {
    /* TODO: implement cr_checkstack */
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


CR_API void cr_push_class(cr_State *ts, cr_VMT *vmt, int sindex, int nup,
                         cr_ClassEntry *entries) {
    OClass *cls;
    cr_lock(ts);
    cls = crMM_newclass(ts);
    crG_check(ts);
    setcls2s(ts, ts->sp.p, cls);
    api_inctop(ts);
    if (sindex) { /* have superclass? */
        const TValue *osup = index2value(ts, sindex);
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


CR_API int cr_get(cr_State *ts, int index) {
    const TValue *o;
    cr_lock(ts);
    api_checknelems(ts, 1); /* key */
    o = index2value(ts, index);
    crV_get(ts, o, s2v(ts->sp.p - 1), ts->sp.p - 1);
    cr_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


cr_sinline Instance *getinstance(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    api_check(ts, ttisins(o), "expect instance");
    return insval(o);
}


cr_sinline int auxrawget(cr_State *ts, HTable *ht, const TValue *key) {
    return finishrawget(ts, crH_get(ht, key));
}


CR_API int cr_get_field(cr_State *ts, int index) {
    Instance *ins;
    cr_lock(ts);
    api_checknelems(ts, 1); /* key */
    ins = getinstance(ts, index);
    return auxrawget(ts, &ins->fields, s2v(--ts->sp.p));

}


cr_sinline OClass *getclass(cr_State *ts, int index) {
    const TValue *o = index2value(ts, index);
    api_check(ts, ttiscls(o), "expect class");
    return clsval(o);
}


CR_API int cr_get_class(cr_State *ts, int index) {
    const TValue *o;
    int tt;
    cr_lock(ts);
    o = index2value(ts, index);
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


CR_API int cr_get_method(cr_State *ts, int index, const char *name) {
    OClass *cls;
    OString *key;
    cr_lock(ts);
    cls = getclass(ts, index);
    key = crS_new(ts, name);
    setstrval2s(ts, ts->sp.p, key);
    api_inctop(ts);
    if (cls->methods) {
        const TValue *slot = crH_getstr(cls->methods, key);
        setslot2stacktop(ts, slot);
    } else {
        setnilval(s2v(ts->sp.p - 1));
    }
    cr_unlock(ts);
    return ttype(s2v(ts->sp.p - 1));
}


CR_API int cr_get_metamethod(cr_State *ts, int index, cr_MM mm) {
    TValue *vmt;
    int tt;
    cr_lock(ts);
    vmt = getvmt(ts, index);
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


CR_API int cr_get_uservalue(cr_State *ts, int index, int n) {
    UserData *ud;
    int tt;
    cr_lock(ts);
    ud = getuserdata(ts, index);
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


CR_API int cr_set(cr_State *ts, int index) {
    TValue *o;
    cr_lock(ts);
    api_checknelems(ts, 2); /* value and key */
    o = index2value(ts, index);
    crV_set(ts, o, s2v(ts->sp.p - 1), s2v(ts->sp.p - 2));
    ts->sp.p -= 2; /* remove value and key */
    cr_unlock(ts);
}


CR_API int cr_set_field(cr_State *ts, int index, const char *field) {
    Instance *ins;
    cr_lock(ts);
    api_checknelems(ts, 1);
    ins = getinstance(ts, index);
    auxrawsetstr(ts, &ins->fields, field, s2v(ts->sp.p - 1));
    cr_unlock(ts);
}


CR_API void cr_setuserdatavmt(cr_State *ts, int index, cr_VMT *vmt) {
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


CR_API int cr_setuservalue(cr_State *ts, int index, int n) {
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


/* Check if the value at the index has virtual method table. */
CR_API int cr_hasvmt(cr_State *ts, int index) {
    return getvmt(ts, index) != NULL;
}


/* Check if the value at the index has meta method. */
CR_API int cr_hasmetamethod(cr_State *ts, int index, cr_MM mm) {
    TValue *vmt = getvmt(ts, index);
    return (vmt ? !ttisnil(&vmt[mm]) : 0);
}


/* Set panic handler and return old one */
CR_API cr_panic cr_setpanic(cr_State *ts, cr_panic panicfn)
{
    cr_panic old_panic;

    cr_lock(ts);
    old_panic = ts->hooks.panic;
    ts->hooks.panic = panicfn;
    cr_unlock(ts);
    return old_panic;
}


/* Return current version. */
CR_API cr_umem cr_version(cr_State *ts)
{
    UNUSED(ts);
    return cast(cr_umem, CR_VERSION_NUMBER);
}


/* 
 * Apply ordering on 2 values on the stack.
 * First both values are pushed on top of the stack (idx1 then idx2)
 * and ordering is applied.
 * This functions is free to call overloaded operator methods.
 * Result in placed in place of first operand and the second operand
 * is popped off.
 * Returned value of 1 means ordering applied is true, otherwise 0 is returned.
 */
CR_API cr_ubyte cr_compare(cr_State *ts, int idx1, int idx2, cr_ord ord)
{
    static void (*ordfuncs[])(cr_State *, Value, Value) = { veq, vne, vlt, vgt, vle, vge };
    Value l, r;
    cr_ubyte res;

    cr_lock(ts);
    criptapi_checkordop(ts, ord);
    criptapi_checkstack(ts, 2);
    l = *index2value(ts, idx1);
    r = *index2value(ts, idx2);
    *ts->sp++ = l; // push left operand
    *ts->sp++ = r; // push right operand
    ordfuncs[ord](ts, l, r);
    res = !ISFALSE(*stkpeek(0));
    cr_unlock(ts);
    return res;
}


/* 
 * Perform equality ordering on values at stack index 'idx1' and 'idx2'.
 * This function will not call overload-able operator methods (__eq__).
 * Result is returned directly without storing it on the stack.
 * Returned value of 1 means values are equal, otherwise 0 is returned. 
 */
CR_API cr_ubyte cr_rawequal(cr_State *ts, int idx1, int idx2)
{
    Value l, r;
    cr_ubyte res;

    cr_lock(ts);
    l = *index2value(ts, idx1);
    r = *index2value(ts, idx2);
    res = raweq(l, r);
    cr_unlock(ts);
    return res;
}


/* 
 * Perform arithmetic 'op' on values on
 * top of the stack.
 * If 'op' is unary operation then the value on top
 * of the stack is considered as operand.
 * If 'op' is binary operation then the 2 values
 * on top of the stack are considered as operands.
 * This function is free to call overload-able operator methods.
 * Result is pushed on top of the stack in place of the
 * first operand and second operand is popped of. 
 */
CR_API void cr_arith(cr_State *ts, cr_ar op)
{
    Value *res;
    int adjust;

    cr_lock(ts);
    criptapi_checkarop(ts, op);
    adjust = 0;
    if (arisbin(op)) {
        criptapi_checkelems(ts, 2);
        adjust = 1;
    } else {
        criptapi_checkelems(ts, 1);
    }
    res = stkpeek(1);
    arith(ts, *res, *stkpeek(0), op, res);
    ts->sp -= adjust; // result is where the first operand was
    cr_unlock(ts);
}


/* Push nil on the stack */
CR_API void cr_pushnil(cr_State *ts)
{
    cr_lock(ts);
    criptapi_pushnil(ts);
    cr_unlock(ts);
}


/* Push number on the stack */
CR_API void cr_pushinteger(cr_State *ts, cr_lint number)
{
    cr_lock(ts);
    criptapi_pushinteger(ts, number);
    cr_unlock(ts);
}


/* Push number on the stack */
CR_API void cr_pushfloat(cr_State *ts, cr_double number)
{
    cr_lock(ts);
    criptapi_pushfloat(ts, number);
    cr_unlock(ts);
}


/* Push string on the stack */
CR_API void cr_pushstring(cr_State *ts, const char *str, cr_umem len)
{
    cr_lock(ts);
    criptapi_pushstr(ts, str, len);
    cr_unlock(ts);
}


/* Push cstring on the stack */
CR_API void cr_pushcstring(cr_State *ts, const char *str)
{
    cr_lock(ts);
    criptapi_pushstr(ts, str, strlen(str));
    cr_unlock(ts);
}


/* Push formatted cstring on the stack, format arguments
 * start from 'argp'. */
CR_API const char *cr_pushvfstring(cr_State *ts, const char *fmt, va_list argp)
{
    const char *str = NULL;
    cr_lock(ts);
    criptapi_pushfstr(ts, fmt, argp);
    str = ascstring(*stkpeek(0));
    cr_unlock(ts);
    return str;
}


/* Push formatted cstring on the stack */
CR_API const char *cr_pushfstring(cr_State *ts, const char *fmt, ...)
{
    const char *str = NULL;
    va_list argp;
    cr_lock(ts);
    va_start(argp, fmt);
    criptapi_pushfstr(ts, fmt, argp);
    va_end(argp);
    str = ascstring(*stkpeek(0));
    cr_unlock(ts);
    return str;
}


/* Push boolean on the stack */
CR_API void cr_pushbool(cr_State *ts, int boolean)
{
    cr_lock(ts);
    cr_checkapi(ts, boolean == 0 || boolean == 1, "invalid boolean.");
    criptapi_pushbool(ts, boolean);
    cr_unlock(ts);
}


/* Auxiliary to 'cr_pushcclosure' and 'cr_pushclass' */
static CClosure *auxpushcclosure(cr_State *ts, cr_CFunction fn, int args, cr_ubyte isvararg,
        int upvals)
{
    CRString *name = asstring(*stkpeek(0));
    CClosure *native = ONative_new(ts, name, fn, args, isvararg, upvals);
    pop(ts); // name
    ts->sp -= upvals;
    while (upvals--)
        native->upvalue[upvals] = *(ts->sp + upvals);
    return native;
}


/* Push C closure on to the stack.
 * The 'args' is how many arguments this function expects (minimum),
 * 'isvararg' is a boolean value indicating if this function takes in
 * variable amount of arguments, 'upvals' is the number of
 * upvalues this C closure has.
 * These upvalues are stored directly in this function and can
 * be accessed with the provided API in this header file.
 * This function will remove 'upvals' amount of values from the stack
 * and store them in C closure. */
CR_API void cr_pushcclosure(cr_State *ts, const char *name, cr_CFunction fn, int args, cr_ubyte isvararg,
        int upvals)
{
    cr_lock(ts);
    criptapi_checkelems(ts, upvals);
    criptapi_checkptr(ts, fn);
    CRString *fname = ts->faststatic[SS_CSRC];
    if (name)
        fname = OString_new(ts, name, strlen(name));
    criptapi_pusho(ts, fname);
    CClosure *native = auxpushcclosure(ts, fn, args, isvararg, upvals);
    criptapi_pushonative(ts, native);
    cr_unlock(ts);
}


/* Push value from the stack located at 'idx', on top of the stack */
CR_API void cr_push(cr_State *ts, int idx) {
    cr_lock(ts);
    Value *val = index2value(ts, idx);
    criptapi_pushval(ts, *val);
    cr_unlock(ts);
}



/* Push class on the stack.
 * Stack will contain 'nup' upvalues that the 'cr_CFunction' located in
 * array of 'cr_entry' will have.
 * Top of the stack shall contain the name of the class.
 * Each 'cr_entry' contains the 'name' of the C function,
 * its argument count (arity) and if the function accepts variable
 * number of arguments (isvararg).
 * This is all what the C function needs to become a C closure inside
 * of cript, with the exception of 'nup'.
 * The reason why each entry does not contain 'nup' is because
 * all of the functions in entries array share the same upvalues.
 * This is in order to simplify the implementation.
 * In case the function name is overload-able method (such as __init__),
 * then 'args' and 'isvararg' in that 'cr_entry' are ignored and
 * the appropriate values are used.
 * After the function returns upvalues will be popped together with
 * the class name; top of the stack will contain newly created class. */
CR_API void cr_pushclass(cr_State *ts, cr_entry entries[], int nup)
{
    cr_lock(ts);
    criptapi_checkelems(ts, nup + 1); // upvalues + class name
    Value classname = *stkpeek(0);
    cr_checkapi(ts, isstring(classname), "Expect string");
    OClass *oclass = OClass_new(ts, asstring(classname));
    pop(ts); // class name
    criptapi_pusho(ts, oclass);
    cr_entry *entry = entries;
    while (entry->name && entry->fn) { // while valid entry
        for (int i = 0; i < nup; i++)
            criptapi_pushval(ts, *stkpeek(nup - 1));
        CRString *name = OString_new(ts, entry->name, strlen(entry->name));
        criptapi_pusho(ts, name);
        int tag = id2omtag(ts, name);
        if (tag == -1) { // not overload ?
            CClosure *native =
                auxpushcclosure(ts, entry->fn, entry->args, entry->isvararg, nup);
            *stkpeek(0) = OBJ_VAL(native);
            rawset(ts, &oclass->mtab, OBJ_VAL(name), OBJ_VAL(native));
            pop(ts); // native
        } else { // overloaded, override 'arity' and 'isvararg'
            CClosure *native = auxpushcclosure(ts, entry->fn, ominfo[tag].arity, 0, nup);
            oclass->vtable[tag] = cast(GCObject *, native);
        }
        entry = ++entries;
    }
    *(ts->sp - nup) = pop(ts); // move class to first upvalue
    popn(ts, nup - 1); // pop the rest of the upvalues
    cr_unlock(ts);
}



/* Return type of the value on the stack at 'idx'. */
CR_API cr_tt cr_type(const cr_State *ts, int idx)
{
    Value *value = index2value(ts, idx);
    return val2type(*value);
}



/* Return type name of the value on the stack at 'idx'.
 * This returned pointer is 'const' indicating the
 * memory it points to should not be modified. */
CR_API const char *cr_typename(const cr_State *ts, int idx)
{
    Value *value = index2value(ts, idx);
    int type = val2type(*value);
    return ts->faststatic[type]->bytes;
}



/* Convert type tag into name */
CR_API const char *cr_tagname(const cr_State *ts, cr_tt type)
{
    return ts->faststatic[type]->bytes;
}



/* Check if the value on the stack at 'idx' is nil. */
CR_API cr_ubyte cr_isnil(const cr_State *ts, int idx)
{
    return IS_NIL(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is string. */
CR_API cr_ubyte cr_isstring(const cr_State *ts, int idx)
{
    return isstring(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is bool. */
CR_API cr_ubyte cr_isbool(const cr_State *ts, int idx)
{
    return IS_BOOL(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is class. */
CR_API cr_ubyte cr_isclass(const cr_State *ts, int idx)
{
    return isclassobj(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is instance. */
CR_API cr_ubyte cr_isinstance(const cr_State *ts, int idx)
{
    return isinstance(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is native C function. */
CR_API cr_ubyte cr_isnative(const cr_State *ts, int idx)
{
    return iscfunction(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is bound method (instance method). */
CR_API cr_ubyte cr_ismethod(const cr_State *ts, int idx)
{
    return isboundmethod(*index2value(ts, idx));
}



/* Check if the value on the stack at 'idx' is cript closure. */
CR_API cr_ubyte cr_isclosure(const cr_State *ts, int idx)
{
    return isclosureobj(*index2value(ts, idx));
}




/* Concatenate 2 strings on top of the stack.
 * Pops the string on top of the stack and replaces the first
 * one with the concatenated string.
 * They are concatenated in the order they were pushed on the stack. */
CR_API const char *cr_concat(cr_State *ts)
{
    cr_lock(ts);
    criptapi_checkelems(ts, 2);
    Value right = *stkpeek(0);
    Value left = *stkpeek(1);
    cr_checkapi(ts, isstring(right) && isstring(left), "expect strings");
    concatonstack(ts);
    const char *concated = ascstring(*stkpeek(0));
    cr_unlock(ts);
    return concated;
}


/* Push class method of an instance at idx on top of the stack.
 * If method doesn't exist this function returns 0 otherwise 1.
 * Note: Class instance methods are all cript closures. */
CR_API cr_ubyte cr_getmethod(cr_State *ts, int idx, const char *method)
{
    cr_lock(ts);
    criptapi_checkptr(ts, method);
    Value val = *index2value(ts, idx);
    if (!isinstance(val))
        return 0;
    criptapi_pushstr(ts, method, strlen(method));
    cr_ubyte haveit = bindmethod(ts, asinstance(val)->oclass, *stkpeek(0), val);
    cr_unlock(ts);
    return haveit;
}



/* Pushes the field value of the class instance at 'idx' on top
 * of the stack.
 * If field value was not found or the value at 'idx' is not
 * class instance return 0, otherwise 1. */
CR_API cr_ubyte cr_getproperty(cr_State *ts, int idx, const char *field)
{
    cr_ubyte res = 0;
    cr_lock(ts);
    criptapi_checkptr(ts, field);
    Value insval = *index2value(ts, idx);
    if (isinstance(insval)) {
        Instance *instance = asinstance(insval);
        Value key = OBJ_VAL(OString_new(ts, field, strlen(field)));
        Value fieldval;
        if ((res = rawget(ts, &instance->fields, key, &fieldval)))
            criptapi_pushval(ts, fieldval);
    }
    cr_unlock(ts);
    return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__getidx__) on it using the
 * value on top of the stack as index value.
 * If the value is not an instance or the
 * '__getidx__' is not overloaded, this function
 * returns 0, otherwise 1 and the value will
 * be on top of the stack. */
CR_API cr_ubyte cr_getindex(cr_State *ts, int idx)
{
    cr_ubyte res = 0;
    cr_lock(ts);
    criptapi_checkelems(ts, 1); // [index]
    Value *index = stkpeek(0);
    Value value = *index2value(ts, idx);
    res = calloverload(ts, value, OM_GETIDX);
    *index = pop(ts); // replace [index] with result
    cr_unlock(ts);
    return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__setidx__) on it, index value
 * is located one place before the top of the stack
 * while the value getting assigned is on top of the stack.
 * If the value is not an instance or '__setidx__' is not
 * overloaded, this function returns 0, otherwise 1. */
CR_API cr_ubyte cr_setindex(cr_State *ts, int idx)
{
    cr_ubyte res = 0;
    cr_lock(ts);
    criptapi_checkelems(ts, 2); // [index][expr]
    Value value = *index2value(ts, idx);
    res = calloverload(ts, value, OM_SETIDX);
    popn(ts, 2); // pop [index] and [expr]
    cr_unlock(ts);
    return res;
}



/* Performs 'raw' index operation, meaning it doesn't invoke
 * overloaded methods when getting/setting the instance property.
 * 'what' parameter if it is a zero means we are setting and
 * non-zero 'what' means we are getting the value at that index.
 * In case we are setting the indexed value then the 'value' we are
 * assigning will be on top of the stack and the 'index' value right
 * below it; if we are getting the value then the 'key' will be on top
 * of the stack.
 * If the operation was successful then 1 is returned; otherwise 0.
 * @ERR: if value we are indexing with is 'nil'. */
CR_API cr_ubyte cr_rawindex(cr_State *ts, int idx, cr_ubyte what)
{
    cr_ubyte res = 0;
    cr_lock(ts);
    criptapi_checkelems(ts, what == CR_RAWSET ? 2 : 1);
    Value value = *index2value(ts, idx);
    if (!isinstance(value))
        return res;
    res = rawindex(ts, value, what);
    cr_unlock(v);
    return res;
}



/* Push global value on top of the stack.
 * In case global value was found, it will be on top of the stack,
 * and this function will return 1, otherwise nothing will be pushed
 * on the stack and the function will return 0. */
CR_API cr_ubyte cr_get_global(cr_State *ts, const char *name)
{
    cr_lock(ts);
    criptapi_checkptr(ts, name);
    int res = 0;
    Value gval;
    criptapi_checkptr(ts, name);
    CRString *str = OString_new(ts, name, strlen(name));
    if (rawget(ts, &ts->globids, OBJ_VAL(str), &gval)) {
        int idx = (int)AS_NUMBER(gval);
        criptapi_pushval(ts, ts->globvars.data[idx].value);
        res = 1;
    }
    cr_unlock(ts);
    return res;
}



/* Get panic handler */
CR_API cr_panic cr_getpanic(cr_State *ts)
{
    cr_lock(ts);
    cr_panic panic_handler = ts->hooks.panic;
    cr_unlock(ts);
    return panic_handler;
}



/* Get allocator function */
CR_API cr_fAlloc cr_getalloc(cr_State *ts, void **ud)
{
    cr_lock(ts);
    cr_fAlloc alloc = ts->hooks.reallocate;
    if (ud)
        *ud = ts->hooks.userdata;
    cr_unlock(ts);
    return alloc;
}



/* Get boolean value (int 1/0) from the stack at 'idx'.
 * If the value at 'idx' is not a boolean, then the flag
 * if provided 'isbool' is set as 0, otherwise flag is set to 1. */
CR_API cr_ubyte cr_getbool(const cr_State *ts, int idx, cr_ubyte *isbool)
{
    cr_ubyte bval;
    Value val = *index2value(ts, idx);
    cr_ubyte is = tobool(val, &bval);
    if (isbool)
        *isbool = is;
    return bval;
}



/* Get number value (cr_double) from the stack at 'idx'.
 * If the value at 'idx' is not a number, then the flag
 * if provided 'isnum' is set as 0, otherwise flag is set to 1. */
CR_API cr_double cr_getnumber(const cr_State *ts, int idx, cr_ubyte *isnum)
{
    cr_double nval = 0.0;
    Value val = *index2value(ts, idx);
    cr_ubyte is = tonumber(val, &nval);
    if (isnum)
        *isnum = is;
    return nval;
}



/* Get string value from the stack at 'idx'.
 * Returns NULL (0) if the value is not a string.
 * Otherwise it returns pointer to the start of the string.
 * Returned pointer is 'const' indicating that user should not
 * modify the contents the pointer points to. */
CR_API const char *cr_getstring(const cr_State *ts, int idx)
{
    Value val = *index2value(ts, idx);
    return isstring(val) ? ascstring(val) : NULL;
}



/* Get native C function from the stack at 'idx'.
 * Return NULL if the value is not a 'cr_CFunction'. */
CR_API cr_CFunction cr_getcfunction(const cr_State *ts, int idx)
{
    Value val = *index2value(ts, idx);
    return iscfunction(val) ? ascfn(val)->fn : NULL;
}


/* Call the value on the stack with 'argc' arguments. */
CR_API void cr_call(cr_State *ts, int argc, int retcnt)
{
    cr_lock(ts);
    cr_checkapi(ts, retcnt >= CR_MULRET, "invalid return count");
    criptapi_checkelems(ts, argc + 1);
    criptapi_checkresults(ts, argc, retcnt);
    Value *fn = ts->sp - (argc + 1);
    ncall(ts, fn, *fn, retcnt);
    cr_unlock(ts);
}



/* Data used for 'fcall' */
struct CallData {
    Value *callee;
    int retcnt;
};


/* Wrapper function */
static void fcall(cr_State *ts, void *userdata)
{
    struct CallData *cd = cast(struct CallData *, userdata);
    ncall(ts, cd->callee, *cd->callee, cd->retcnt);
}


/* Protected call.
 * Same as cr_call except this runs the function in protected
 * mode, meaning that in case the function errors it won't print
 * invoke panic handler.
 * Instead it restores the old call frame and pushes the error object
 * on top of the stack.
 * This function returns 'c_status' [defined @cript.h] code. */
CR_API cr_status cr_pcall(cr_State *ts, int argc, int retcnt)
{
    cr_lock(ts);
    cr_checkapi(ts, retcnt >= CR_MULRET, "invalid return count");
    criptapi_checkelems(ts, argc + 1);
    criptapi_checkresults(ts, argc, retcnt);
    struct CallData cd;
    cd.retcnt = retcnt;
    cd.callee = ts->sp - (argc + 1);
    int status = pcall(ts, fcall, &cd, save_stack(ts, cd.callee));
    cr_unlock(ts);
    return status;
}



/*
 * Loads (compiles) cript script using provided 'reader'.
 * Returns 'c_status' [defined @cript.h] code.
 * If the script compiled without any errors then the compiled
 * function (cript closure) gets pushed on top of the stack.
 * In case there were any compile errors, then the error object
 * gets pushed on top of the stack (error message).
 *
 * 'reader' - user provided 'crR' responsible for reading
 *            the '.sk' source file.
 *            Refer to 'cR' in [@cript.h] for more
 *            information on how this reader should 'behave'.
 * 'userdata' - user provided data for 'reader'.
 * 'source' - name of the cript script you are loading.
 */
CR_API cr_status cr_load(cr_State *ts, crR reader, void *userdata, const char *source)
{
    BuffReader br;
    cr_lock(ts);
    BuffReader_init(ts, &br, reader, userdata);
    cr_status status = pcompile(ts, &br, source, 0);
    cr_unlock(ts);
    return status;
}



/* Garbage collection API.
 * Refer to the @cscript.h and 'cr_gco' enum defined in the same header. */
CR_API cr_umem cr_incgc(cr_State *ts, cr_incgco option, ...)
{
    va_list argp;
    cr_umem res = 0;
    cr_lock(ts);
    va_start(argp, option);
    switch (option) {
        case GCO_STOP:
            res = ts->gc.gc_stopped;
            ts->gc.gc_stopped = 1;
            break;
        case GCO_RESTART:
            res = ts->gc.gc_stopped;
            ts->gc.gc_stopped = 0;
            break;
        case GCO_COLLECT:
            res = incgc(ts);
            break;
        case GCO_COUNT:
            res = ts->gc.gc_allocated;
            break;
        case GCO_ISRUNNING:
            res = (ts->gc.gc_stopped == 0);
            break;
        case GCO_NEXTGC:
            res = ts->gc.gc_nextgc;
            break;
    }
    va_end(argp);
    cr_unlock(ts);
    return res;
}




// TODO: Implement
CR_API void cr_dumpstack(cr_State *ts)
{
    (void)(0);
}



/* Converts value on the stack at the 'idx' into string.
 * Additionally if the 'len' and/or 'hash' are non-NULL then
 * it also fills them.
 * This can call overload-able method '__tostring__'. */
CR_API const char *cr_tostring(cr_State *ts, int idx, cr_umem *len, cr_hash *hash)
{
    const char *str = NULL;
    cr_lock(ts);
    Value *v = index2value(ts, idx);
    CRString *ostr = vtostr(ts, v, *v, 0);
    str = ostr->bytes;
    if (len)
        *len = ostr->len;
    if (hash)
        *hash = ostr->hash;
    cr_unlock(ts);
    return str;
}



/* Set global value 'name' to the value on top of the stack.
 * In case the global variable 'name' does not exist, then
 * the new one is declared and 'isconst' modifier is considered
 * when creating it.
 * Otherwise 'isconst' modifier is ignored and the global
 * variable is set to the new value UNLESS the variable is
 * set as 'fixed'; in that case runtime error is invoked. */
CR_API cr_ubyte cr_setglobal(cr_State *ts, const char *name, int isconst)
{
    cr_lock(ts);
    criptapi_checkelems(ts, 1); // value must be present
    criptapi_checkptr(ts, name);
    Value newval = *stkpeek(0);
    Value key = OBJ_VAL(OString_new(ts, name, strlen(name)));
    cr_ubyte isnew = 0;
    Value gidx;
    if ((isnew = !rawget(ts, &ts->globids, key, &gidx))) {
        criptapi_pushval(ts, key);
        Variable gvar = { newval, 0x01 & isconst };
        Value idx = NUMBER_VAL(Array_Variable_push(&ts->globvars, gvar));
        rawset(ts, &ts->globids, key, idx);
        popn(ts, 2); // value and key
    } else {
        Variable *gvar = Array_Variable_index(&ts->globvars, AS_NUMBER(gidx));
        if (cr_unlikely(VISCONST(gvar))) {
            fixederror(ts, globalname(ts, AS_NUMBER(gidx))->bytes);
        }
        gvar->value = newval;
        pop(ts); // value
    }
    cr_unlock(ts);
    return isnew;
}



/* Set the field of the class instance to the value on top of the stack.
 * Class should be located at 'idx' and the name of the field to be set is 'field'.
 * This sets the field to that value and pops it off the top of the stack.
 * Returns 1 if the field didn't exist before or false if the value
 * of the field got overwritten. */
CR_API cr_ubyte cr_setfield(cr_State *ts, int idx, const char *field)
{
    cr_lock(ts);
    criptapi_checkelems(ts, 1);
    criptapi_checkptr(ts, field);
    Value insval = *index2value(ts, idx);
    cr_checkapi(ts, isinstance(insval), "expect class instance");
    Instance *instance = asinstance(insval);
    criptapi_pushcstr(ts, field);
    cr_ubyte res = rawset(ts, &instance->fields, *stkpeek(0), *stkpeek(1));
    ts->sp -= 2; // pop value and key
    cr_unlock(ts);
    return res;
}



/* Auxiliary function for 'cr_getupval' and 'cr_setupval'.
 * Returns pointer to the upvalue. */
static cr_inline Value *getupval(Value fn, int n)
{
    if (isclosureobj(fn)) { // cript closure ?
        CrClosure *closure = asclosure(fn);
        if (cast_uint(n) > closure->fn->p.upvalc - 1)
            return NULL;
        return closure->upvalue[n]->location;
    } else if (iscfunction(fn)) { // native C function ?
        CClosure *native = ascfn(fn);
        if (cast_uint(n) > native->p.upvalc - 1)
            return NULL;
        return &native->upvalue[n];
    } else
        return NULL;
}



/* Get upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pushes the upvalue on top of the stack and returns 1.
 * If the upvalue was not found or the function at 'fidx' is not
 * a cript or C closure then nothing will be pushed on the stack and
 * 0 is returned. */
CR_API cr_ubyte cr_getupvalue(cr_State *ts, int fidx, int idx)
{
    cr_lock(ts);
    cr_ubyte ret = 0;
    Value fn = *index2value(ts, fidx);
    Value *upval = getupval(fn, idx);
    if (upval) {
        criptapi_pushval(ts, *upval);
        ret = 1;
    }
    cr_unlock(ts);
    return ret;
}



/* Sets the upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pops the top value on the stack and sets
 * it as the new value of the upvalue.
 * If the upvalue doesn't exist and/or 'fidx' is not a
 * closure or native C function, then the function returns 0
 * indicating the upvalue was not set, otherwise it returns 1. */
CR_API int cr_setupvalue(cr_State *ts, int fidx, int idx)
{
    cr_lock(ts);
    criptapi_checkelems(ts, 1);
    int changed = 0;
    Value fn = *index2value(ts, fidx);
    Value *upval = getupval(fn, idx);
    if (upval) {
        *upval = *stkpeek(0);
        ts->sp--;
        changed = 1;
    }
    cr_unlock(ts);
    return changed;
}


static Instance *getinstance(cr_State *ts, int idx)
{
    TValue *v;
    v = index2value(ts, idx);
    checkapi(ts, ttisins(v), "expect instance");
    return insvalue(v);
}


/* 
 * Get the next property of the instance located at 'idx' on the stack.
 * The 'key' value used for lookup is on top of the stack.
 * 'what' if set to 0 fetches next field, otherwise it
 * gets method.
 * This function returns 1 if there is next property and
 * the value on top of the stack (key) is replaced with the
 * next key; additionally value associated with that key is
 * also pushed on top of the stack.
 * If there is no next property 0 is returned and stack
 * remains unchanged. 
 * In case user provided 'key' that the instance method/field
 * table does not contain then runtime error is invoked.
 */
CR_API cr_ubyte cr_nextproperty(cr_State *ts, int idx, cr_ubyte what)
{
    cr_ubyte hasnext;
    Instance *instance;
    HTable *tab;
    TValue *key;

    cr_lock(ts);
    hasnext = 0;
    checkapi_values(ts, 2); /* key + instance */
    checkapi_stack(ts, 1); /* value */
    instance = getinstance(ts, idx);
    key = speek(0);
    if (what == 0)
        tab = &instance->fields;
    else
        tab = &instance->oclass->methods;
    hasnext = crH_next(ts, tab, key);
    if (hasnext)
        api_incsp(ts); /* push value */
    else
        api_decsp(ts); /* pop key */
    cr_unlock(ts);
    return hasnext;
}



/* Return the length of the string at 'idx'.
 * If the value is not a string then return 0. */
CR_API cr_umem cr_strlen(const cr_State *ts, int idx)
{
    Value val = *index2value(ts, idx);
    return (isstring(val) ? asstring(val)->len : 0);
}


/* Return 'cr_State' 'cr_status' code. */
CR_API cr_status cr_getstatus(cr_State *ts)
{
    UNUSED(ts);
    return ts->status;
}


/* Invoke a runetime error with errcode */
CR_API int cr_error(cr_State *ts, cr_status errcode)
{
    cr_lock(ts);
    Value *errobj = stkpeek(0);
    criptapi_checkelems(ts, 1);
    criptapi_checkerrcode(ts, errcode);
    runerror(ts, errcode); // cr_unlock in here
    return 0; // to avoid compiler warnings
}
