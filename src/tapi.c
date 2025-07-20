/*
** tapi.c
** Toku API
** See Copyright Notice in tokudae.h
*/


#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tlist.h"
#include "tdebug.h"
#include "tfunction.h"
#include "tgc.h"
#include "tmem.h"
#include "tmeta.h"
#include "tprotected.h"
#include "tokudae.h"
#include "tokudaetonf.h"
#include "tokudaelimits.h"
#include "ttable.h"
#include "tobject.h"
#include "tokudae.h"
#include "treader.h"
#include "tobject.h"
#include "tstate.h"
#include "tstring.h"
#include "tvm.h"
#include "stdarg.h"
#include "ttrace.h"
#include "tapi.h"


/* first pseudo-index for upvalues */
#define UPVALINDEX      (TOKU_CTABLE_INDEX - 1)


/* test for pseudo index */
#define ispseudo(i)	    ((i) <= TOKU_CLIST_INDEX)

/* test for upvalue */
#define isupvalue(i)	    ((i) <= UPVALINDEX)

/* test for valid index */
#define isvalid(C,o)        (!isempty(o) || (o) != &G(C)->nil)


/* 
** Convert index to a pointer to its value.
** Invalid indices (using upvalue index for Toku functions) return
** special nil value '&G(C)->nil'.
*/
static TValue *index2value(const toku_State *T, int index) {
    CallFrame *cf = C->cf;
    if (index >= 0) { /* absolute index? */
        SPtr o = (cf->func.p + 1) + index;
        api_check(C, index < cf->top.p - (cf->func.p + 1), "index too large");
        if (o >= C->sp.p) return &G(C)->nil;
        else return s2v(o);
    } else if (!ispseudo(index)) { /* negative index? */
        api_check(C, -index <= C->sp.p - (cf->func.p + 1), "index too small");
        return s2v(C->sp.p + index);
    } else if (index == TOKU_CLIST_INDEX) /* C list index? */
        return &G(C)->t_list;
    else if (index == TOKU_CTABLE_INDEX) /* C table index? */
        return &G(C)->t_table;
    else { /* otherwise upvalue index */
        index = UPVALINDEX - index;
        api_check(C, index < USHRT_MAX, "upvalue index too large");
        if (t_likely(ttisCclosure(s2v(cf->func.p)))) { /* C closure? */
            CClosure *ccl = clCval(s2v(cf->func.p));
            return &ccl->upvals[index];
        } else { /* light C function or Toku function (through a hook)? */
            api_check(C, ttislcf(s2v(cf->func.p)), "caller not a C function");
            return &G(C)->nil; /* no upvalues */
        }
    }
}


/*
** Convert index to a stack slot.
*/
static SPtr index2stack(const toku_State *T, int index) {
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


/* {======================================================================
** State manipulation (other functions are defined in tstate.c)
** ======================================================================= */

TOKU_API toku_CFunction toku_atpanic(toku_State *T, toku_CFunction fpanic) {
    toku_CFunction old_panic;
    toku_lock(C);
    old_panic = G(C)->fpanic;
    G(C)->fpanic = fpanic;
    toku_unlock(C);
    return old_panic;
}


TOKU_API void toku_setallocf(toku_State *T, toku_Alloc falloc, void *ud) {
    toku_lock(C);
    G(C)->falloc = falloc;
    G(C)->ud_alloc = ud;
    toku_unlock(C);
}


TOKU_API toku_Alloc toku_getallocf(toku_State *T, void **ud) {
    toku_Alloc falloc;
    toku_lock(C);
    if (ud) *ud = G(C)->ud_alloc;
    falloc = G(C)->falloc;
    toku_unlock(C);
    return falloc;
}

/* }====================================================================== */


/* {======================================================================
** Stack manipulation
** ======================================================================= */

t_sinline void settop(toku_State *T, int n) {
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
        toku_assert(hastocloseCfunc(cf->nresults));
        newtop = csF_close(C, newtop, CLOSEKTOP);
    }
    C->sp.p = newtop; /* set new top */
}


TOKU_API void toku_setntop(toku_State *T, int n) {
    toku_lock(C);
    settop(C, n);
    toku_unlock(C);
}


TOKU_API int toku_gettop(const toku_State *T) {
    return cast_int(C->sp.p - (C->cf->func.p + 1) - 1);
}


TOKU_API int toku_absindex(toku_State *T, int index)
{
    return (index >= 0 || ispseudo(index))
            ? index
            : cast_int(C->sp.p - C->cf->func.p - 1) + index;
}


/* 
** Auxiliary to 'toku_rotate', reverses stack values starting at 'from'
** until 'to'.
*/
t_sinline void rev(toku_State *T, SPtr from, SPtr to) {
    while (from < to) {
        TValue temp;
        setobj(C, &temp, s2v(from));
        setobjs2s(C, from, to);
        setobj2s(C, to, &temp);
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
** toku_rotate(C, 2, 2);
** [func][0][1][3][4][2]
**
** Example left-rotation:
** [func][0][1][2][3][4]
** toku_rotate(C, 2, -2);
** [func][0][1][4][3][2]
*/
TOKU_API void toku_rotate(toku_State *T, int index, int n) {
    SPtr start, end, pivot;
    toku_lock(C);
    end = C->sp.p - 1; /* end of segment */
    start = index2stack(C, index); /* start of segment */
    api_check(C, (n >= 0 ? n : -n) <= (end - start + 1), "invalid 'n'");
    pivot = (n >= 0 ? end - n : start - n - 1); /* end of prefix */
    rev(C, start, pivot);
    rev(C, pivot + 1, end);
    rev(C, start, end);
    toku_unlock(C);
}


TOKU_API void toku_copy(toku_State *T, int src, int dest) {
    TValue *from, *to;
    toku_lock(C);
    from = index2value(C, src);
    to = index2value(C, dest);
    api_check(C, isvalid(C, to), "invalid index");
    setobj(C, to, from);
    if (isupvalue(dest)) /* closure upvalue? */
        csG_barrier(C, clCval(s2v(C->cf->func.p)), from);
    toku_unlock(C);
}


TOKU_API int toku_checkstack(toku_State *T, int n) {
    CallFrame *cf;
    int res;
    toku_lock(C);
    cf = C->cf;
    api_check(C, n >= 0, "negative 'n'");
    if (C->stackend.p - C->sp.p >= n) /* stack large enough? */
        res = 1;
    else /* need to grow the stack */
        res = csT_growstack(C, n, 0);
    if (res && cf->top.p < C->sp.p + n)
        cf->top.p = C->sp.p + n; /* adjust frame top */
    toku_unlock(C);
    return res;
}


/* push without lock */
#define pushvalue(C, index) \
    { setobj2s(C, C->sp.p, index2value(C, index)); api_inctop(C); }


TOKU_API void toku_push(toku_State *T, int index) {
    toku_lock(C);
    pushvalue(C, index);
    toku_unlock(C);
}


TOKU_API void toku_xmove(toku_State *src, toku_State *dest, int n) {
    if (src == dest) return; /* same thread ? */
    toku_lock(dest);
    api_checknelems(src, n); /* have enough elements to move? */
    api_check(src, G(src) == G(dest), "moving between different states");
    api_check(src, dest->cf->top.p - dest->sp.p >= n, "dest stack overflow");
    src->sp.p -= n;
    for (int i = 0; i < n; i++) {
        setobjs2s(dest, dest->sp.p, src->sp.p + i);
        dest->sp.p++; /* already checked by 'api_check' */
    }
    toku_unlock(dest);
}

/* }====================================================================== */


/* {======================================================================
** Access functions (Stack -> C)
** ======================================================================= */

TOKU_API int toku_is_number(toku_State *T, int index) {
    toku_Number n;
    UNUSED(n);
    const TValue *o = index2value(C, index);
    return tonumber(o, n);
}


TOKU_API int toku_is_integer(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return ttisint(o);
}


TOKU_API int toku_is_string(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return ttisstring(o);
}


TOKU_API int toku_is_cfunction(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return (ttislcf(o) || ttisCclosure(o));
}


TOKU_API int toku_is_udatamethod(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return ttisusermethod(o);
}


TOKU_API int toku_is_userdata(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return (ttislightuserdata(o) || ttisfulluserdata(o));
}


TOKU_API int toku_type(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return (isvalid(C, o) ? ttype(o) : TOKU_T_NONE);
}


TOKU_API const char *toku_typename(toku_State *T, int type) {
    UNUSED(C);
    api_check(C, TOKU_T_NONE <= type && type < TOKU_T_NUM, "invalid type");
    return typename(type);
}


TOKU_API toku_Number toku_to_numberx(toku_State *T, int index, int *pisnum) {
    toku_Number n = 0.0;
    const TValue *o = index2value(C, index);
    int isnum = tonumber(o, n);
    if (pisnum)
        *pisnum = isnum;
    return n;
}


TOKU_API toku_Integer toku_to_integerx(toku_State *T, int index, int *pisint) {
    toku_Integer i = 0;
    const TValue *o = index2value(C, index);
    int isint = tointeger(o, &i);
    if (pisint)
        *pisint = isint;
    return i;
}


TOKU_API int toku_to_bool(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return !t_isfalse(o);
}


TOKU_API const char *toku_to_lstring(toku_State *T, int index, size_t *plen) {
    const TValue *o = index2value(C, index);
    if (!ttisstring(o)) /* not a string? */
        return NULL;
    else if (plen != NULL)
        *plen = getstrlen(strval(o)); 
    return getstr(strval(o));
}


TOKU_API toku_CFunction toku_to_cfunction(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    if (ttislcf(o)) 
        return lcfval(o);
    else if (ttisCclosure(o))
        return clCval(o)->fn;
    else
        return NULL;
}


t_sinline void *touserdata(const TValue *o) {
    switch (ttypetag(o)) {
        case TOKU_VLIGHTUSERDATA: return pval(o);
        case TOKU_VUSERDATA: return getuserdatamem(udval(o));
        default: return NULL;
    }
}


/*
** Return pointer to userdata memory from the value at index.
** If the value is not userdata return NULL.
*/
TOKU_API void *toku_to_userdata(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return touserdata(o);
}


/*
** Returns a pointer to the internal representation of an object.
** Note that ANSI C does not allow the conversion of a pointer to
** function to a 'void*', so the conversion here goes through
** a 'size_t'. (As the returned pointer is only informative, this
** conversion should not be a problem.)
*/
TOKU_API const void *toku_to_pointer(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case TOKU_VLCF: return cast_voidp(cast_sizet(lcfval(o)));
        case TOKU_VUSERDATA: case TOKU_VLIGHTUSERDATA:
            return touserdata(o);
        default: {
            if (iscollectable(o))
                return gcoval(o);
            else
                return NULL;
        }
    }
}


TOKU_API toku_State *toku_to_thread(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    return (ttisthread(o) ? thval(o) : NULL);
}

/* }====================================================================== */


/* {======================================================================
** Ordering & Arithmetic functions
** ======================================================================= */

TOKU_API void toku_arith(toku_State *T, int op) {
    toku_lock(C);
    if (op != TOKU_OP_UNM && op != TOKU_OP_BNOT) { /* binary op? */
        api_checknelems(C, 2);
        csV_binarithm(C, s2v(C->sp.p-2), s2v(C->sp.p-1), C->sp.p-2, op);
        C->sp.p--; /* pop second operand */
    } else { /* unary op */
        api_checknelems(C, 1);
        csV_unarithm(C, s2v(C->sp.p-1), C->sp.p-1, op);
        /* done */
    }
    toku_unlock(C);
}


TOKU_API int toku_rawequal(toku_State *T, int index1, int index2) {
    const TValue *lhs = index2value(C, index1);
    const TValue *rhs = index2value(C, index2);
    return (isvalid(C, lhs) && isvalid(C, rhs)) ? csV_raweq(lhs, rhs) : 0;
}


TOKU_API int toku_compare(toku_State *T, int index1, int index2, int op) {
    const TValue *lhs;
    const TValue *rhs;
    int res = 0;
    toku_lock(C); /* might call overloaded method */
    lhs = index2value(C, index1);
    rhs = index2value(C, index2);
    if (isvalid(C, lhs) && isvalid(C, rhs)) {
        switch (op) {
            case TOKU_ORD_EQ: res = csV_ordereq(C, lhs, rhs); break;
            case TOKU_ORD_LT: res = csV_orderlt(C, lhs, rhs); break;
            case TOKU_ORD_LE: res = csV_orderle(C, lhs, rhs); break;
            default: api_check(C, 0, "invalid 'op'"); break;
        }
    }
    toku_unlock(C);
    return res;
}

/* }====================================================================== */


/* {======================================================================
** Push functions (C -> stack)
** ======================================================================= */

/* Push nil value on top of the stack. */
TOKU_API void toku_push_nil(toku_State *T) {
    toku_lock(C);
    setnilval(s2v(C->sp.p));
    api_inctop(C);
    toku_unlock(C);
}


/* Push 'toku_Number' value on top of the stack. */
TOKU_API void toku_push_number(toku_State *T, toku_Number n) {
    toku_lock(C);
    setfval(s2v(C->sp.p), n);
    api_inctop(C);
    toku_unlock(C);
}


/* Push 'toku_Integer' value on top of the stack. */
TOKU_API void toku_push_integer(toku_State *T, toku_Integer i) {
    toku_lock(C);
    setival(s2v(C->sp.p), i);
    api_inctop(C);
    toku_unlock(C);
}


/* Push string value of length 'len' on top of the stack. */
TOKU_API const char *toku_push_lstring(toku_State *T, const char *str, size_t len) {
    OString *s;
    toku_lock(C);
    s = (len == 0 ? csS_new(C, "") : csS_newl(C, str, len));
    setstrval2s(C, C->sp.p, s);
    api_inctop(C);
    csG_checkGC(C);
    toku_unlock(C);
    return getstr(s);
}


/* Push null terminated string value on top of the stack. */
TOKU_API const char *toku_push_string(toku_State *T, const char *str) {
    toku_lock(C);
    if (str == NULL) {
        setnilval(s2v(C->sp.p));
    } else {
        OString *s = csS_new(C, str);
        setstrval2s(C, C->sp.p, s);
        str = getstr(s);
    }
    api_inctop(C);
    csG_checkGC(C);
    toku_unlock(C);
    return str;
}


TOKU_API const char *toku_push_vfstring(toku_State *T, const char *fmt, va_list ap) {
    const char *str;
    toku_lock(C);
    str = csS_pushvfstring(C, fmt, ap);
    csG_checkGC(C);
    toku_unlock(C);
    return str;
}


TOKU_API const char *toku_push_fstring(toku_State *T, const char *fmt, ...) {
    const char *str;
    va_list ap;
    toku_lock(C);
    va_start(ap, fmt);
    str = csS_pushvfstring(C, fmt, ap);
    va_end(ap);
    csG_checkGC(C);
    toku_unlock(C);
    return str;
}


/* push C closure without locking */
t_sinline void pushcclosure(toku_State *T, toku_CFunction fn, int nupvals) {
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
            toku_assert(iswhite(cl));
        }
        setclCval(C, s2v(C->sp.p), cl);
        api_inctop(C);
        csG_checkGC(C);
    }
}


TOKU_API void toku_push_cclosure(toku_State *T, toku_CFunction fn, int nupvals) {
    toku_lock(C);
    pushcclosure(C, fn, nupvals);
    toku_unlock(C);
}


TOKU_API void toku_push_bool(toku_State *T, int b) {
    toku_lock(C);
    if (b)
        setbtval(s2v(C->sp.p));
    else
        setbfval(s2v(C->sp.p));
    api_inctop(C);
    toku_unlock(C);
}


TOKU_API void toku_push_lightuserdata(toku_State *T, void *p) {
    toku_lock(C);
    setpval(s2v(C->sp.p), p);
    api_inctop(C);
    toku_unlock(C);
}


TOKU_API void *toku_push_userdata(toku_State *T, size_t sz, int nuv) {
    UserData *ud;
    toku_lock(C);
    api_check(C, 0 <= nuv && nuv < USHRT_MAX, "invalid value");
    ud = csMM_newuserdata(C, sz, nuv);
    setudval2s(C, C->sp.p, ud);
    api_inctop(C);
    csG_checkGC(C);
    toku_unlock(C);
    return getuserdatamem(ud);
}


TOKU_API void toku_push_list(toku_State *T, int sz) {
    List *l;
    toku_lock(C);
    l = csA_new(C);
    setlistval2s(C, C->sp.p, l);
    api_inctop(C);
    csA_ensure(C, l, sz);
    csG_checkGC(C);
    toku_unlock(C);
}


TOKU_API void toku_push_table(toku_State *T, int sz) {
    Table *t;
    toku_lock(C);
    t = csH_new(C);
    settval2s(C, C->sp.p, t);
    api_inctop(C);
    if (sz > 0)
        csH_resize(C, t, sz);
    csG_checkGC(C);
    toku_unlock(C);
}


TOKU_API int toku_push_thread(toku_State *T) {
    toku_lock(C);
    setthval2s(C, C->sp.p, C);
    api_inctop(C);
    toku_unlock(C);
    return (G(C)->mainthread == C);
}


// TODO: update docs
TOKU_API void toku_push_class(toku_State *T) {
    OClass *cls;
    toku_lock(C);
    cls = csMM_newclass(C);
    setclsval2s(C, C->sp.p, cls);
    api_inctop(C);
    csG_checkGC(C);
    toku_unlock(C);
}


TOKU_API void toku_push_instance(toku_State *T, int index) {
    const TValue *o;
    SPtr func = C->sp.p;
    toku_lock(C);
    o = index2value(C, index);
    api_check(C, ttisclass(o), "expect class");
    setclsval2s(C, func, classval(o));
    api_inctop(C);
    csV_call(C, func, 1);
    csG_checkGC(C);
    toku_unlock(C);
}


// TODO: add docs
TOKU_API void toku_push_method(toku_State *T, int index) {
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1); /* method */
    o = index2value(C, index);
    switch (ttypetag(o)) {
        case TOKU_VINSTANCE: {
            IMethod *im = csMM_newinsmethod(C, insval(o), s2v(C->sp.p - 1));
            setimval2s(C, C->sp.p - 1, im);
            break;
        }
        case TOKU_VUSERDATA: {
            UMethod *um = csMM_newudmethod(C, udval(o), s2v(C->sp.p - 1));
            setumval2s(C, C->sp.p - 1, um);
            break;
        }
        default: api_check(C, 0, "fulluserdata/instance expected");
    }
    csG_checkGC(C);
    toku_unlock(C);
}

/* }====================================================================== */


/* {======================================================================
** Get functions (Toku -> stack)
** ======================================================================= */

TOKU_API int toku_get(toku_State *T, int index) {
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, index);
    csV_get(C, o, s2v(C->sp.p - 1), C->sp.p - 1);
    toku_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


TOKU_API int toku_get_raw(toku_State *T, int index) {
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, index);
    csV_rawget(C, o, s2v(C->sp.p - 1), C->sp.p - 1);
    toku_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


t_sinline int finishrawgetfield(toku_State *T, const TValue *val) {
    if (isempty(val))
        setnilval(s2v(C->sp.p));
    else
        setobj2s(C, C->sp.p, val);
    api_inctop(C);
    toku_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


#define fastget(C,t,k,slot,f)     ((slot = f(t, k)), !isempty(slot))

t_sinline void aux_rawsetstr(toku_State *T, Table *t, const char *str,
                             const TValue *v) {
    const TValue *slot;
    OString *s = csS_new(C, str);
    if (fastget(C, t, s, slot, csH_getstr)) {
        setobj(C, cast(TValue *, slot), v);
        csG_barrierback(C, obj2gco(t), v);
        C->sp.p--; /* pop value */
    } else {
        setstrval2s(C, C->sp.p, s);
        api_inctop(C);
        csV_settable(C, t, s, v, csH_setstr);
        C->sp.p -= 2; /* pop string key and value */
    }
    toku_unlock(C);
}


t_sinline int aux_rawgetfieldstr(toku_State *T, Table *t, const char *k) {
    const TValue *slot;
    OString *str = csS_new(C, k);
    if (fastget(C, t, str, slot, csH_getstr)) {
        setobj2s(C, C->sp.p, slot);
    } else
        setnilval(s2v(C->sp.p));
    api_inctop(C);
    toku_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


TOKU_API int toku_get_global(toku_State *T, const char *name) {
    toku_lock(C);
    return aux_rawgetfieldstr(C, tval(GT(C)), name);
}


static int aux_getindex(toku_State *T, List *l, int i) {
    csA_geti(C, l, i, s2v(C->sp.p));
    api_inctop(C);
    toku_unlock(C);
    return ttype(s2v(C->sp.p - 1));
}


t_sinline List *getlist(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttislist(o), "list expected");
    return listval(o);
}


TOKU_API int toku_get_index(toku_State *T, int index, int i) {
    toku_lock(C);
    api_checklistidx(C, i);
    return aux_getindex(C, getlist(C, index), i);
}


TOKU_API int toku_get_cindex(toku_State *T, int i) {
    toku_lock(C);
    api_checklistidx(C, i);
    return aux_getindex(C, listval(CL(C)), i);
}


TOKU_API int toku_get_cfieldstr(toku_State *T, const char *field) {
    toku_lock(C);
    return aux_rawgetfieldstr(C, tval(CT(C)), field);
}


t_sinline Table *gettable(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case TOKU_VINSTANCE: return insval(o)->fields;
        case TOKU_VTABLE: return tval(o); 
        default:  {
            api_check(C, 0, "expect instance or table");
            return NULL;
        }
    }
}


TOKU_API int toku_get_field(toku_State *T, int index) {
    Table *t;
    const TValue *val;
    toku_lock(C);
    api_checknelems(C, 1); /* key */
    t = gettable(C, index);
    val = csH_get(t, s2v(C->sp.p - 1));
    C->sp.p--; /* remove key */
    return finishrawgetfield(C, val);
}


TOKU_API int toku_get_fieldstr(toku_State *T, int index, const char *field) {
    Table *t;
    toku_lock(C);
    t = gettable(C, index);
    return aux_rawgetfieldstr(C, t, field);
}


TOKU_API int toku_get_fieldptr(toku_State *T, int index, const void *field) {
    Table *t;
    TValue aux;
    toku_lock(C);
    t = gettable(C, index);
    setpval(&aux, cast_voidp(field));
    return finishrawgetfield(C, csH_get(t, &aux));
}


TOKU_API int toku_get_fieldint(toku_State *T, int index, toku_Integer i) {
    Table *t;
    toku_lock(C);
    t = gettable(C, index);
    return finishrawgetfield(C, csH_getint(t, i));
}


TOKU_API int toku_get_fieldflt(toku_State *T, int index, toku_Number n) {
    Table *t;
    TValue aux;
    toku_lock(C);
    t = gettable(C, index);
    setfval(&aux, n);
    return finishrawgetfield(C, csH_get(t, &aux));
}


TOKU_API int toku_get_class(toku_State *T, int index) {
    int t;
    const TValue *o;
    toku_lock(C);
    o = index2value(C, index);
    if (ttisinstance(o)) {
        setclsval2s(C, C->sp.p, insval(o)->oclass);
        api_inctop(C);
        t = TOKU_T_CLASS;
    } else
        t = TOKU_T_NONE;
    toku_unlock(C);
    return t;
}


TOKU_API int toku_get_superclass(toku_State *T, int index) {
    OClass *scl = NULL;
    int res = 1;
    const TValue *o;
    toku_lock(C);
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
    toku_unlock(C);
    return res;
}


static int aux_getmethod(toku_State *T, const TValue *o, Table *t) {
    int tt;
    if (t) { /* have methods table? */
        const TValue *method = csH_get(t, s2v(C->sp.p - 1));
        if (!isempty(method)) { /* method found? */
            tt = ttype(method);
            if (ttisinstance(o)) { /* instance method? */
                IMethod *im = csMM_newinsmethod(C, insval(o), method);
                setimval2s(C, C->sp.p-1, im);
            } else { /* userdata method */
                UMethod *um = csMM_newudmethod(C, udval(o), method);
                setumval2s(C, C->sp.p-1, um);
            }
            csG_checkGC(C);
            goto unlock;
        }
    }
    tt = TOKU_T_NIL;
    setnilval(s2v(C->sp.p - 1));
unlock:
    toku_unlock(C);
    return tt;
}


// TODO: update docs
TOKU_API int toku_get_method(toku_State *T, int index) {
    Table *t = NULL; /* to prevent warnings */
    TValue *m;
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1); /* key */
    o = index2value(C, index);
    switch (ttypetag(o)) {
        case TOKU_VINSTANCE: {
            t = insval(o)->oclass->methods;
            break;
        }
        case TOKU_VUSERDATA: {
            t = udval(o)->methods;
            break;
        }
        case TOKU_VIMETHOD: {
            m = &imval(o)->method;
            goto unwrap;
        }
        case TOKU_VUMETHOD: {
            m = &umval(o)->method;
        unwrap:
            setobj2s(C, C->sp.p, m);
            api_inctop(C);
            toku_unlock(C);
            return ttype(s2v(C->sp.p - 1));
        }
        default: api_check(C, 0, "instance/fulluserdata/bmethod expected");
    }
    return aux_getmethod(C, o, t);
}


// TODO: add docs
TOKU_API int toku_get_self(toku_State *T, int index) {
    int tt;
    const TValue *o;
    toku_lock(C);
    o = index2value(C, index);
    switch (ttypetag(o)) {
        case TOKU_VUMETHOD: {
            tt = TOKU_T_USERDATA;
            setudval2s(C, C->sp.p, umval(o)->ud);
            break;
        }
        case TOKU_VIMETHOD: {
            tt = TOKU_T_INSTANCE;
            setinsval2s(C, C->sp.p, imval(o)->ins);
            break;
        }
        default: {
            tt = TOKU_T_NONE;
            goto nopush;
        }
    }
    api_inctop(C);
nopush:
    toku_unlock(C);
    return tt;
}


TOKU_API int toku_get_supermethod(toku_State *T, int index) {
    const TValue *obj;
    Table *t = NULL;
    toku_lock(C);
    api_checknelems(C, 1); /* key */
    obj = index2value(C, index);
    api_check(C, ttisinstance(obj), "instance expected");
    if (insval(obj)->oclass->sclass) /* have superclass? */
        t = insval(obj)->oclass->sclass->methods;
    return aux_getmethod(C, obj, t);
}


TOKU_API int toku_get_metalist(toku_State *T, int index) {
    const TValue *obj;
    List *ml;
    int res = 0;
    toku_lock(C);
    obj = index2value(C, index);
    switch (ttype(obj)) {
        case TOKU_T_INSTANCE:
            ml = insval(obj)->oclass->metalist;
            break;
        case TOKU_T_CLASS:
            ml = classval(obj)->metalist;
            break;
        case TOKU_T_USERDATA:
            ml = udval(obj)->metalist;
            break;
        default:
            ml = NULL;
            break;
    }
    if (ml) { /* have metalist? */
        setlistval2s(C, C->sp.p, ml);
        api_inctop(C);
        res = 1;
    }
    toku_unlock(C);
    return res;
}


t_sinline UserData *getuserdata(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    api_check(C, ttisfulluserdata(o), "fulluserdata expected");
    return udval(o);
}


TOKU_API int toku_get_uservalue(toku_State *T, int index, t_ushort n) {
    UserData *ud;
    int t;
    toku_lock(C);
    ud = getuserdata(C, index);
    if (ud->nuv <= n) {
        setnilval(s2v(C->sp.p));
        t = TOKU_T_NONE;
    } else {
        setobj2s(C, C->sp.p, &ud->uv[n].val);
        t = ttype(s2v(C->sp.p));
    }
    api_inctop(C);
    toku_unlock(C);
    return t;
}


// TODO: update docs
TOKU_API int toku_get_methods(toku_State *T, int index) {
    int res = 0;
    Table *t = NULL; /* to prevent warnings */
    const TValue *o;
    toku_lock(C);
    o = index2value(C, index);
    switch (ttype(o)) {
        case TOKU_VCLASS: {
            t = classval(o)->methods;
            break;
        }
        case TOKU_VINSTANCE: {
            t = insval(o)->oclass->methods;
            break;
        }
        case TOKU_VUSERDATA: {
            t = udval(o)->methods;
            break;
        }
        default: api_check(C, 0, "class/instance/fulluserdata expected");
    }
    if (t) {
        settval2s(C, C->sp.p, t);
        api_inctop(C);
        res = 1;
    }
    toku_unlock(C);
    return res;
}


// TODO: add docs
TOKU_API void toku_get_fields(toku_State *T, int index) {
    const TValue *o;
    toku_lock(C);
    o = index2value(C, index);
    api_check(C, ttisinstance(o), "instance expected");
    settval2s(C, C->sp.p, insval(o)->fields);
    api_inctop(C);
    toku_unlock(C);
}

/* }====================================================================== */


/* {======================================================================
** Set functions (stack -> Toku)
** ======================================================================= */

TOKU_API void toku_set(toku_State *T, int obj) {
    TValue *o;
    toku_lock(C);
    api_checknelems(C, 2); /* value and key */
    o = index2value(C, obj);
    csV_set(C, o, s2v(C->sp.p - 2), s2v(C->sp.p - 1));
    C->sp.p -= 2; /* remove value and key */
    toku_unlock(C);
}


TOKU_API void  toku_set_raw(toku_State *T, int obj) {
    TValue *o;
    toku_lock(C);
    api_checknelems(C, 2); /* key and value */
    o = index2value(C, obj);
    csV_rawset(C, o, s2v(C->sp.p - 2), s2v(C->sp.p - 1));
    C->sp.p -= 2; /* remove value and key */
    toku_unlock(C);
}


TOKU_API void toku_set_global(toku_State *T, const char *name) {
    toku_lock(C);
    api_checknelems(C, 1); /* value */
    aux_rawsetstr(C, tval(GT(C)), name, s2v(C->sp.p - 1));
}


static void aux_setindex(toku_State *T, List *l, int i) {
    csA_ensureindex(C, l, i);
    csA_fastset(C, l, i, s2v(C->sp.p - 1));
    C->sp.p--; /* remove value */
    toku_unlock(C);
}


TOKU_API void toku_set_index(toku_State *T, int index, int i) {
    toku_lock(C);
    api_checknelems(C, 1); /* value */
    api_checklistidx(C, i);
    aux_setindex(C, getlist(C, index), i);
}


TOKU_API void toku_set_cindex(toku_State *T, int i) {
    toku_lock(C);
    api_checknelems(C, 1); /* value */
    api_checklistidx(C, i);
    aux_setindex(C, listval(CL(C)), i);
}


t_sinline void aux_rawsetfield(toku_State *T, int obj, TValue *key, int n) {
    Table *t;
    toku_lock(C);
    api_checknelems(C, n);
    t = gettable(C, obj);
    csV_settable(C, t, key, s2v(C->sp.p - 1), csH_set);
    C->sp.p -= n;
    toku_unlock(C);
}


TOKU_API void toku_set_field(toku_State *T, int obj) {
    aux_rawsetfield(C, obj, s2v(C->sp.p - 2), 2);
}


TOKU_API void toku_set_fieldstr(toku_State *T, int index, const char *field) {
    Table *t;
    toku_lock(C);
    api_checknelems(C, 1);
    t = gettable(C, index);
    aux_rawsetstr(C, t, field, s2v(C->sp.p - 1));
}


TOKU_API void toku_set_fieldptr(toku_State *T, int obj, const void *field) {
    TValue key;
    setpval(&key, cast_voidp(field));
    aux_rawsetfield(C, obj, &key, 1);
}


TOKU_API void  toku_set_fieldint(toku_State *T, int obj, toku_Integer field) {
    TValue key;
    setival(&key, field);
    aux_rawsetfield(C, obj, &key, 1);
}


TOKU_API void  toku_set_fieldflt(toku_State *T, int obj, toku_Number field) {
    TValue key;
    setfval(&key, field);
    aux_rawsetfield(C, obj, &key, 1);
}


TOKU_API void toku_set_cfieldstr(toku_State *T, const char *field) {
    toku_lock(C);
    api_checknelems(C, 1); /* value */
    aux_rawsetstr(C, tval(CT(C)), field, s2v(C->sp.p - 1));
}


TOKU_API void toku_set_metalist(toku_State *T, int index) {
    List *ml;
    TValue temp;
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1); /* metalist */
    o = index2value(C, index);
    if (ttisnil(s2v(C->sp.p - 1)))
        ml = NULL;
    else {
        api_check(C, ttislist(s2v(C->sp.p - 1)), "list expected");
        ml = listval(s2v(C->sp.p - 1));
    }
    switch (ttype(o)) {
        case TOKU_T_INSTANCE: {
            insval(o)->oclass->metalist = ml;
            if (ml) {
                /* first look for finalizer */
                csG_checkfin(C, gcoval(o), ml);
                setclsval(C, &temp, insval(o)->oclass);
                o = &temp; /* for GC barrier */
                goto gcbarrier;
            }
            goto nobarrier;
        }
        case TOKU_T_CLASS: {
            classval(o)->metalist = ml;
            break;
        }
        case TOKU_T_USERDATA: {
            udval(o)->metalist = ml;
            if (ml) {
                csG_checkfin(C, gcoval(o), ml);
                goto gcbarrier;
            }
            goto nobarrier;
        }
        default: api_check(C, 0, "instance/class/fulluserdata expected");
    }
    if (ml) gcbarrier: csG_objbarrier(C, gcoval(o), ml);
nobarrier:
    C->sp.p--; /* remove metalist */
    toku_unlock(C);
}


TOKU_API int toku_set_uservalue(toku_State *T, int index, t_ushort n) {
    int res;
    UserData *ud;
    toku_lock(C);
    api_checknelems(C, 1); /* value */
    ud = getuserdata(C, index);
    if (ud->nuv <= n)
        res = 0; /* 'n' not in [0, ud->nuv) */
    else {
        setobj(C, &ud->uv[n].val, s2v(C->sp.p - 1));
        csG_barrierback(C, obj2gco(ud), s2v(C->sp.p - 1));
        res = 1;
    }
    C->sp.p--; /* remove value */
    toku_unlock(C);
    return res;
}


// TODO: update docs
TOKU_API void toku_set_methods(toku_State *T, int index) {
    Table *t;
    TValue temp;
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1); /* table or nil */
    o = index2value(C, index);
    if (ttisnil(s2v(C->sp.p - 1)))
        t = NULL;
    else {
        api_check(C, ttistable(s2v(C->sp.p - 1)), "table expected");
        t = tval(s2v(C->sp.p - 1));
    }
    switch (ttype(o)) {
        case TOKU_T_INSTANCE: {
            insval(o)->oclass->methods = t;
            if (t) {
                setclsval(C, &temp, insval(o)->oclass);
                o = &temp; /* for GC barrier */
                goto gcbarrier;
            }
            goto nobarrier;
        }
        case TOKU_T_CLASS: {
            classval(o)->methods = t;
            break;
        }
        case TOKU_T_USERDATA: {
            udval(o)->methods = t;
            break;
        }
        default: api_check(C, 0, "instance/class/fulluserdata expected");
    }
    if (t) gcbarrier: csG_objbarrier(C, gcoval(o), t);
nobarrier:
    C->sp.p--; /* remove table or nil */
    toku_unlock(C);
}


// TODO: add docs
TOKU_API void toku_set_fields(toku_State *T, int index) {
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1);
    o = index2value(C, index);
    api_check(C, ttisinstance(o), "instance expected");
    if (ttisnil(s2v(C->sp.p - 1)))
        insval(o)->fields = NULL;
    else {
        Table *t;
        api_check(C, ttistable(s2v(C->sp.p - 1)), "table expected");
        t = tval(s2v(C->sp.p - 1));
        insval(o)->fields = t;
        csG_objbarrier(C, gcoval(o), t);
    }
    C->sp.p--;
    toku_unlock(C);
}


// TODO: add docs
TOKU_API void toku_set_listlen(toku_State *T, int index, int len) {
    List *l;
    toku_lock(C);
    l = getlist(C, index);
    api_check(C, 0 <= len, "negative 'len'");
    csA_ensure(C, l, len);
    l->len = len;
    toku_unlock(C);
}


TOKU_API void toku_set_superclass(toku_State *T, int index) {
    OClass *sc;
    TValue temp;
    const TValue *o;
    toku_lock(C);
    api_checknelems(C, 1);
    o = index2value(C, index);
    if (ttisnil(s2v(C->sp.p - 1)))
        sc = NULL;
    else {
        api_check(C, ttisclass(s2v(C->sp.p - 1)), "class expected");
        sc = classval(s2v(C->sp.p - 1));
    }
    switch (ttype(o)) {
        case TOKU_T_INSTANCE: {
            insval(o)->oclass->sclass = sc;
            if (sc) {
                setclsval(C, &temp, insval(o)->oclass);
                o = &temp; /* for GC barrier */
                goto barrier;
            }
            goto nobarrier;
        }
        case TOKU_T_CLASS: {
            classval(o)->sclass = sc;
            break;
        }
        default: api_check(C, 0, "class/instance expected");
    }
    if (sc) barrier: csG_objbarrier(C, gcoval(o), sc);
nobarrier:
    C->sp.p--; /* remove class or nil */
    toku_unlock(C);
}

/* }====================================================================== */


/* {======================================================================
** Status and Error reporting
** ======================================================================= */

TOKU_API int toku_status(toku_State *T) {
    return C->status;
}


TOKU_API int toku_error(toku_State *T) {
    TValue *errobj;
    toku_lock(C);
    api_checknelems(C, 1); /* errobj */
    errobj = s2v(C->sp.p - 1);
    if (ttisshrstring(errobj) && eqshrstr(strval(errobj), G(C)->memerror)) {
        csM_error(C); /* raise a memory error */
    } else
        csD_errormsg(C);
    /* toku_unlock() is called after control leaves the core */
    toku_assert(0);
}

/* }====================================================================== */


/* {======================================================================
** Call/Load Toku chunks
** ======================================================================= */

#define checkresults(C,nargs,nres) \
     api_check(C, (nres) == TOKU_MULTRET \
               || (C->cf->top.p - C->sp.p >= (nres) - (nargs)), \
	"results from function overflow current stack size")


TOKU_API void toku_call(toku_State *T, int nargs, int nresults) {
    SPtr func;
    toku_lock(C);
    api_checknelems(C, nargs + 1); /* args + func */
    api_check(C, C->status == TOKU_STATUS_OK, "can't do calls on non-normal thread");
    checkresults(C, nargs, nresults);
    func = C->sp.p - nargs - 1;
    csV_call(C, func, nresults);
    adjustresults(C, nresults);
    toku_unlock(C);
}


struct PCallData {
    SPtr func;
    int nresults;
};


static void fcall(toku_State *T, void *ud) {
    struct PCallData *pcd = cast(struct PCallData*, ud);
    csV_call(C, pcd->func, pcd->nresults);
}


TOKU_API int toku_pcall(toku_State *T, int nargs, int nresults, int abserrf) {
    struct PCallData pcd;
    int status;
    ptrdiff_t func;
    toku_lock(C);
    api_checknelems(C, nargs+1); /* args + func */
    api_check(C, C->status == TOKU_STATUS_OK, "can't do calls on non-normal thread");
    checkresults(C, nargs, nresults);
    if (abserrf < 0)
        func = 0;
    else {
        SPtr o = index2stack(C, abserrf);
        api_check(C, ttisfunction(s2v(o)), "error handler must be a function");
        func = savestack(C, o);
    }
    pcd.func = C->sp.p - (nargs + 1); /* function to be called */
    pcd.nresults = nresults;
    status = csPR_call(C, fcall, &pcd, savestack(C, pcd.func), func);
    adjustresults(C, nresults);
    toku_unlock(C);
    return status;
}


// TODO: update docs
TOKU_API int toku_load(toku_State *T, toku_Reader reader, void *userdata,
                   const char *chunkname) {
    BuffReader br;
    int status;
    toku_lock(C);
    if (!chunkname) chunkname = "?";
    csR_init(C, &br, reader, userdata);
    status = csPR_parse(C, &br, chunkname);
    if (status == TOKU_STATUS_OK) {  /* no errors? */
        CSClosure *cl = clCSval(s2v(C->sp.p - 1)); /* get new function */
        if (cl->nupvalues >= 1) { /* does it have an upvalue? */
            const TValue *gt = GT(C); /* get global table from clist */
            /* set global table as 1st upvalue of 'cl' (may be TOKU_ENV) */
            setobj(C, cl->upvals[0]->v.p, gt);
            csG_barrier(C, cl->upvals[0], gt);
        }
    }
    toku_unlock(C);
    return status;
}

/* }====================================================================== */


/* {======================================================================
** Garbage collector
** ======================================================================= */

TOKU_API int toku_gc(toku_State *T, int option, ...) {
    va_list argp;
    int res = 0;
    GState *gs = G(C);
    if (gs->gcstop & (GCSTP | GCSTPCLS)) /* internal stop? */
        return -1; /* all options are invalid when stopped */
    toku_lock(C);
    va_start(argp, option);
    switch (option) {
	case TOKU_GC_STOP: { /* stop garbage collector */
            gs->gcstop = GCSTPUSR; /* stopped by user */
            break;
        }
	case TOKU_GC_RESTART: { /* restart GC */
            csG_setgcdebt(gs, 0);
            gs->gcstop = 0; /* clear stop */
            break;
        }
        case TOKU_GC_CHECK: { /* check and clear collection flag */
            res = gs->gccheck; /* get check flag */
            gs->gccheck = 0; /* clear check flag */
            break;
        }
	case TOKU_GC_COLLECT: { /* start GC cycle */
            csG_fullinc(C, 0);
            break;
        }
	case TOKU_GC_COUNT: { /* total GC memory count (in Kbytes) */
            res = cast_int(gettotalbytes(gs) >> 10);
            break;
        }
	case TOKU_GC_COUNTBYTES: { /* remainder bytes of totalbytes/1024 */
            res = cast_int(gettotalbytes(gs) & 0x3FF); /* all before bit 10 */
            break;
        }
	case TOKU_GC_STEP: { /* perform GC step */
            int data = va_arg(argp, int); /* Kbytes */
            t_mem gcdebt = 1; /* true if GC did work */
            t_ubyte old_gcstop = gs->gcstop;
            gs->gcstop = 0; /* allow GC to run */
            if (data == 0) {
                csG_setgcdebt(gs, 0); /* force to run one basic step */
                csG_step(C);
            } else { /* add 'data' to total gcdebt */
                /* convert 'data' to bytes (data = bytes/2^10) */
                gcdebt = cast(t_mem, data) * 1024 + gs->gcdebt;
                csG_setgcdebt(gs, gcdebt);
                csG_checkGC(C);
            }
            gs->gcstop = old_gcstop; /* restore previous state */
            if (gcdebt > 0 && gs->gcstate == GCSpause) /* end of cycle? */
                res = 1; /* signal it */
            break;
        }
        case TOKU_GC_PARAM: {
            int param = va_arg(argp, int);
            int value = va_arg(argp, int);
            api_check(C, 0 <= param && param < TOKU_GCP_NUM, "invalid parameter");
            res = cast_int(getgcparam(gs->gcparams[param]));
            if ((value) >= 0) {
                if (param == TOKU_GCP_STEPSIZE)
                    gs->gcparams[param] = value;
                else
                    setgcparam(gs->gcparams[param], value);
            }
            break;
        }
	case TOKU_GC_ISRUNNING: { /* check if GC is running */
            res = gcrunning(gs);
            break;
        }
        case TOKU_GC_INC: {
            int pause = va_arg(argp, int);
            int stepmul = va_arg(argp, int);
            int stepsize = va_arg(argp, int);
            res = TOKU_GC_INC;
            if (pause >= 0)
                setgcparam(gs->gcparams[TOKU_GCP_PAUSE], pause);
            if (stepmul >= 0)
                setgcparam(gs->gcparams[TOKU_GCP_STEPMUL], stepmul);
            if (stepsize >= 0)
                gs->gcparams[TOKU_GCP_STEPSIZE] = stepsize;
            csG_incmode(C);
            break;
        }
        default: res = -1; /* invalid option */
    }
    va_end(argp);
    toku_unlock(C);
    return res;
}

/* }====================================================================== */


/* {======================================================================
** Warning-related functions
** ======================================================================= */

TOKU_API void toku_setwarnf(toku_State *T, toku_WarnFunction fwarn, void *ud) {
    toku_lock(C);
    G(C)->fwarn = fwarn;
    G(C)->ud_warn = ud;
    toku_unlock(C);
}


TOKU_API void toku_warning(toku_State *T, const char *msg, int cont) {
    toku_lock(C);
    csT_warning(C, msg, cont);
    toku_unlock(C);
}

/* }====================================================================== */


/* {======================================================================
** Miscellaneous functions
** ====================================================================== */

TOKU_API int toku_find_index(toku_State *T, int index, int fi, int s, int e) {
    List *l;
    int res;
    toku_lock(C);
    api_check(C, !(fi & ~TOKU_FI_MASK), "invalid 'fi' mask");
    l = getlist(C, index);
    e = (e < l->len) ? e : l->len - 1;
    s = (0 <= s) ? s : 0;
    res = csA_findindex(l, (fi & TOKU_FI_REV), !(fi & TOKU_FI_NIL), s, e);
    toku_unlock(C);
    return res;
}


TOKU_API unsigned toku_numbertocstring(toku_State *T, int index, char *buff) {
    const TValue *o = index2value(C, index);
    if (ttisnum(o)) {
        t_uint len = csS_tostringbuff(o, buff);
        buff[len++] = '\0'; /* terminate */
        return len;
    } else
        return 0;
}


TOKU_API size_t toku_stringtonumber(toku_State *T, const char *s, int *f) {
    size_t sz = csS_tonum(s, s2v(C->sp.p), f);
    if (sz != 0) /* no conversion errors? */
        api_inctop(C);
    return sz;
}


TOKU_API toku_Number toku_version(toku_State *T) {
    UNUSED(C);
    return TOKU_VERSION_NUM;
}


TOKU_API toku_Integer toku_len(toku_State *T, int index) {
    const TValue *o = index2value(C, index);
    switch (ttypetag(o)) {
        case TOKU_VSHRSTR: return strval(o)->shrlen;
        case TOKU_VLNGSTR: return strval(o)->u.lnglen;
        case TOKU_VLIST: return listval(o)->len;
        case TOKU_VTABLE: return csH_len(tval(o));
        case TOKU_VCLASS: {
            Table *t = classval(o)->methods;
            return (t ? csH_len(classval(o)->methods) : 0);
        }
        case TOKU_VINSTANCE: return csH_len(insval(o)->fields);
        default: return 0;
    }
}


TOKU_API size_t toku_lenudata(toku_State *T, int index) {
    return getuserdata(C, index)->size;
}


TOKU_API int toku_nextfield(toku_State *T, int obj) {
    Table *t;
    int more;
    toku_lock(C);
    api_checknelems(C, 1); /* key */
    t = gettable(C, obj);
    more = csH_next(C, t, C->sp.p - 1);
    if (more) {
        api_inctop(C);
    } else
        C->sp.p--; /* remove key */
    toku_unlock(C);
    return more;
}


TOKU_API void toku_concat(toku_State *T, int n) {
    toku_lock(C);
    api_checknelems(C, n);
    if (n > 0) {
        csV_concat(C, n);
        csG_checkGC(C);
    } else { /* nothing to concatenate */
        setstrval2s(C, C->sp.p, csS_newl(C, "", 0));
        api_inctop(C);
    }
    toku_unlock(C);
}


TOKU_API void toku_toclose(toku_State *T, int index) {
    SPtr o;
    int nresults;
    toku_lock(C);
    o = index2stack(C, index);
    api_check(C, C->tbclist.p < o,
                  "given level below or equal to the last marked slot");
    csF_newtbcvar(C, o); /* create new to-be-closed upvalue */
    nresults = C->cf->nresults;
    if (!hastocloseCfunc(nresults)) /* function not yet marked? */
        C->cf->nresults = codeNresults(nresults); /* mark it */
    toku_assert(hastocloseCfunc(C->cf->nresults)); /* must be marked */
    toku_unlock(C);
}


TOKU_API void toku_closeslot(toku_State *T, int index) {
    SPtr level;
    toku_lock(C);
    level = index2stack(C, index);
    api_check(C, hastocloseCfunc(C->cf->nresults) && C->tbclist.p == level,
                 "no variable to close at the given level");
    level = csF_close(C, level, CLOSEKTOP);
    setnilval(s2v(level)); /* closed */
    toku_unlock(C);
}


// TODO: add docs
TOKU_API void toku_shrinklist(toku_State *T, int index) {
    toku_lock(C);
    csA_shrink(C, getlist(C, index));
    toku_unlock(C);
}


// TODO: add docs
TOKU_API unsigned toku_numuservalues(toku_State *T, int index) {
    return getuserdata(C, index)->nuv;
}

/* }====================================================================== */


/* {======================================================================
** Debug functions (other functions are defined in tdebug.c)
** ======================================================================= */

/*
** Sets 'frame' in 'toku_Debug'; 'level' is 'CallFrame' level.
** To traverse the call stack backwards (up), then level should be
** greater than 0. For example if you wish for currently active 'CallFrame',
** then 'level' should be 0, if 'level' is 1 then the 'CallFrame' of the
** function that called the current function is considered.
** If 'level' is found, therefore 'cf' is set, then this function returns 1,
** otherwise 0.
*/
TOKU_API int toku_getstack(toku_State *T, int level, toku_Debug *ar) {
    int status = 0;
    CallFrame *cf;
    if (level >= 0) { /* valid level? */
        toku_lock(C);
        for (cf = C->cf; level > 0 && cf != &C->basecf; cf = cf->prev)
            level--;
        if (level == 0 && cf != &C->basecf) { /* level found ? */
            ar->cf = cf;
            status = 1; /* signal it */
        }
        toku_unlock(C);
    }
    return status;
}


// TODO: add docs
TOKU_API int toku_stackinuse(toku_State *T) {
    toku_assert(TOKU_MAXINT >= savestack(C, C->sp.p));
    return cast_int(savestack(C, C->sp.p));
}


static const char *aux_upvalue(TValue *func, int n, TValue **val,
                               GCObject **owner) {
    switch (ttypetag(func)) {
        case TOKU_VCCL: { /* C closure */
            CClosure *f = clCval(func);
            if (!(cast_uint(n) < cast_uint(f->nupvalues)))
                return NULL;  /* 'n' not in [0, cl->nupvalues) */
            *val = &f->upvals[n];
            if (owner) *owner = obj2gco(f);
            return "";
        }
        case TOKU_VCSCL: { /* Toku closure */
            OString *name;
            CSClosure *f = clCSval(func);
            Proto *p = f->p;
            if (!(cast_uint(n) < cast_uint(p->sizeupvals)))
                return NULL; /* 'n' not in [0, fn->sizeupvals) */
            *val = f->upvals[n]->v.p;
            if (owner) *owner = obj2gco(f->upvals[n]);
            name = p->upvals[n].name;
            return check_exp(name != NULL, getstr(name));
        }
        default: return NULL; /* not a closure */
    }
}


/*
** If object is a bound method, this returns reference to the
** underlying function object (which again might be the bound method,
** but at that point we don't care, it's up to the user).
*/
static TValue *rawfunc(toku_State *T, int index) {
    TValue *o = index2value(C, index);
    if (ttisinstancemethod(o))
        o = &imval(o)->method;
    else if (ttisusermethod(o))
        o = &umval(o)->method;
    return o;
}


TOKU_API const char *toku_getupvalue(toku_State *T, int index, int n) {
    const char *name;
    TValue *upval = NULL; /* to avoid warnings */
    toku_lock(C);
    name = aux_upvalue(rawfunc(C, index), n, &upval, NULL);
    if (name) { /* have upvalue ? */
        setobj2s(C, C->sp.p, upval);
        api_inctop(C);
    }
    toku_unlock(C);
    return name;
}


TOKU_API const char *toku_setupvalue(toku_State *T, int index, int n) {
    const char *name;
    TValue *upval = NULL; /* to avoid warnings */
    GCObject *owner = NULL; /* to avoid warnings */
    toku_lock(C);
    api_checknelems(C, 1); /* value */
    name = aux_upvalue(rawfunc(C, index), n, &upval, &owner);
    if (name) { /* found upvalue ? */
        C->sp.p--;
        setobj(C, upval, s2v(C->sp.p));
        csG_barrier(C, owner, upval);
    }
    toku_unlock(C);
    return name;
}


static UpVal **getupvalref(toku_State *T, int index, int n, CSClosure **pf) {
    static const UpVal *const nullup = NULL;
    const TValue *fi = rawfunc(C, index);
    CSClosure *f;
    api_check(C, ttisCSclosure(fi), "Toku function expected");
    f = clCSval(fi);
    if (pf) *pf = f;
    if (0 <= n && n < f->p->sizeupvals)
        return &f->upvals[n]; /* get its upvalue pointer */
    else
        return (UpVal**)&nullup;
}


// TODO: add docs
TOKU_API void *toku_upvalueid(toku_State *T, int index, int n) {
    const TValue *fi = rawfunc(C, index);
    switch (ttypetag(fi)) {
        case TOKU_VCSCL: { /* Toku closure */
            return *getupvalref(C, index, n, NULL);
        }
        case TOKU_VCCL: { /* C closure */
            CClosure *f = clCval(fi);
            if (0 <= n && n < f->nupvalues)
                return &f->upvals[n];
            /* else */
        } /* fall through */
        case TOKU_VLCF:
            return NULL; /* light C functions have no upvalues */
        default: {
            api_check(C, 0, "function expected");
            return NULL;
        }
    }
}


// TODO: add docs
TOKU_API void toku_upvaluejoin(toku_State *T, int index1, int n1,
                                        int index2, int n2) {
    CSClosure *f1;
    UpVal **up1 = getupvalref(C, index1, n1, &f1);
    UpVal **up2 = getupvalref(C, index2, n2, NULL);
    api_check(C, *up1 != NULL && *up2 != NULL, "invalid upvalue index");
    *up1 = *up2;
    csG_objbarrier(C, f1, *up1);
}

/* }====================================================================== */
