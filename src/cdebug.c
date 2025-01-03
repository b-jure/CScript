/*
** cdebug.c
** Debug and error reporting functions
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include <string.h>

#include "cdebug.h"
#include "capi.h"
#include "ccode.h"
#include "cfunction.h"
#include "cstring.h"
#include "climits.h"
#include "cobject.h"
#include "cprotected.h"
#include "cmeta.h"
#include "cvm.h"
#include "cgc.h"


#define CScriptClosure(cl)      ((cl) != NULL && (cl)->c.tt_ == CS_VCSCL)


/* get line number of instruction ('pc') */
int csD_getfuncline(const Proto *p, int pc) {
    int low = 0;
    int high = p->sizelinfo - 1;
    LineInfo *li;
    while (low <= high) {
        int mid = low + ((high - low)/2);
        li = &p->linfo[mid];
        if (pc < li->pc)
            high = mid - 1;
        else if (li->pc < pc)
            low = mid + 1;
        else /* otherwise direct hit */
            break;
    }
    li += li->pc < pc;
    cs_assert(pc <= li->pc);
    return li->line;
}


cs_sinline int relpc(const CallFrame *cf) {
    cs_assert(isCScript(cf));
    return cast_int(cf->pc - cfProto(cf)->code) - 1;
}


/* current line number */
cs_sinline int currentline(CallFrame *cf) {
    return csD_getfuncline(cfProto(cf), relpc(cf));
}


static const char *findvararg(CallFrame *cf, SPtr *pos, int n) {
    if (cfProto(cf)->isvararg) {
        int nextra = cf->nvarargs;
        if (n >= -nextra) {
            *pos = cf->func.p - nextra - (n + 1);
            return "(vararg)";
        }
    }
    return NULL;
}


/*
** Find local variable at index 'n', store it in 'pos' and
** returns its name. If variable is not found return NULL.
*/
const char *csD_findlocal(cs_State *ts, CallFrame *cf, int n, SPtr *pos) {
    SPtr base = cf->func.p + 1;
    const char *name = NULL;
    if (isCScript(cf)) {
        if (n < 0) /* vararg ? */
            return findvararg(cf, pos, n);
        else /* otherwise local variable */
            name = csF_getlocalname(cfProto(cf), n, relpc(cf));
    }
    if (name == NULL) {
        SPtr limit = (cf == ts->cf) ? ts->sp.p : cf->next->func.p;
        if (limit - base >= n && n > 0) /* 'n' is in stack range ? */
            name = isCScript(cf) ? "(auto)" : "(C auto)";
        else
            return NULL;
    }
    if (pos)
        *pos = base + (n - 1);
    return name;
}


CS_API const char *cs_getlocal(cs_State *ts, const cs_DebugInfo *di, int n) {
    const char *name;
    cs_lock(ts);
    if (di == NULL) {
        if (!ttisCSclosure(s2v(ts->sp.p - 1)))
            name = NULL;
        else
            name = csF_getlocalname(clCSval(s2v(ts->sp.p - 1))->p, n, 0);
    }
    else {
        SPtr pos = NULL;
        name = csD_findlocal(ts, di->cf, n, &pos);
        if (name) { /* found ? */
            setobjs2s(ts, ts->sp.p, pos);
            api_inctop(ts);
        }
    }
    cs_unlock(ts);
    return name;
}


CS_API const char *cs_setlocal (cs_State *ts, const cs_DebugInfo *ar, int n) {
    SPtr pos = NULL;
    const char *name;
    cs_lock(ts);
    name = csD_findlocal(ts, ar->cf, n, &pos);
    if (name) { /* found ? */
        setobjs2s(ts, pos, ts->sp.p - 1); /* set the value */
        ts->sp.p--; /* remove value */
    }
    cs_unlock(ts);
    return name;
}


static void getfuncinfo(Closure *cl, cs_DebugInfo *di) {
    if (!CScriptClosure(cl)) {
        di->source = "[C]";
        di->srclen = SLL("[C]");
        di->defline = -1;
        di->lastdefline = -1;
        di->what = "C";
    } else {
        const Proto *p = cl->cs.p;
        if (p->source) { /* have source? */
          di->source = getstr(p->source);
          di->srclen = getstrlen(p->source);
        }
        else {
          di->source = "?";
          di->srclen = SLL("?");
        }
        di->defline = p->defline;
        di->lastdefline = p->deflastline;
        di->what = (di->lastdefline == 0) ? "main" : "CScript";
    }
    csS_sourceid(di->shortsrc, di->source, di->srclen);
}


static const char *funcnamefromcode(cs_State *ts, const Proto *p, int pc,
                                    const char **name) {
    cs_MM mm;
    Instruction *i = &p->code[pc];
    switch (*i) {
        /* TODO: symbolic execution for OP_CALL */
        case OP_CALL: return NULL;
        case OP_FORCALL: {
            *name = "for iterator";
            return "for iterator";
        }
        case OP_GETPROPERTY: case OP_GETINDEX:
        case OP_GETINDEXSTR: case OP_GETINDEXINT:
            mm = CS_MM_GETIDX;
            break;
        case OP_SETPROPERTY: case OP_SETINDEX:
        case OP_SETINDEXSTR: case OP_SETINDEXINT:
            mm = CS_MM_SETIDX;
            break;
        case OP_MBIN: {
            mm = GETARG_S(i, 0);
            break;
        }
        case OP_LT: case OP_LTI: case OP_GTI: mm = CS_MM_LT; break;
        case OP_LE: case OP_LEI: case OP_GEI: mm = CS_MM_LE; break;
        case OP_CLOSE: case OP_RET: mm = CS_MM_CLOSE; break;
        case OP_UNM: mm = CS_MM_UNM; break;
        case OP_BNOT: mm = CS_MM_BNOT; break;
        case OP_CONCAT: mm = CS_MM_CONCAT; break;
        case OP_EQ: mm = CS_MM_EQ; break;
        default: return NULL;
    }
    *name = getstr(G_(ts)->mmnames[mm]) + /* skip '__' */ 2;
    return "metamethod";
}


/* try to find a name for a function based on how it was called */
static const char *funcnamefromcall(cs_State *ts, CallFrame *cf,
                                    const char **name) {
    if (cf->status & CFST_FIN) { /* called as finalizer? */
        *name = "__gc";
        return "metamethod";
    }
    else if (isCScript(cf))
        return funcnamefromcode(ts, cfProto(cf), relpc(cf), name);
    else
        return NULL;
}


static const char *getfuncname(cs_State *ts, CallFrame *cf, const char **name) {
    if (cf != NULL)
        return funcnamefromcall(ts, cf->prev, name);
    else
        return NULL;
}


/*
** Auxiliary to 'cs_getinfo', parses 'options' and fills out the
** 'cs_DebugInfo' accordingly. If any invalid options is specified this
** returns 0.
*/
static int getinfo(cs_State *ts, const char *options, Closure *cl,
                   CallFrame *cf, cs_DebugInfo *di) {
    int status = 1;
    for (; *options; options++) {
        switch (*options) {
            case 'n': {
                di->namewhat = getfuncname(ts, cf, &di->name);
                if (di->namewhat == NULL) { /* not found ? */
                    di->namewhat = "";
                    di->name = NULL;
                }
                break;
            }
            case 's': {
                getfuncinfo(cl, di);
                break;
            }
            case 'l': {
                di->currline = (isCScript(cf) ? currentline(cf) : -1);
                break;
            }
            case 'u': {
                di->nupvals = (cl ? cl->c.nupvalues : 0);
                if (isCScript(cf)) {
                    di->nparams = cl->cs.p->arity;
                    di->isvararg = cl->cs.p->isvararg;
                } else {
                    di->nparams = 0;
                    di->isvararg = 1;
                }
                break;
            }
            case 'f': /* resolved in 'cs_getinfo' */
                break;
            default: { /* invalid option */
                status = 0;
                break;
            }
        }
    }
    return status;
}


/*
** Fill out 'cs_DebugInfo' according to 'options'.
**
** '>' - Pops the function on top of the stack and loads it into 'cf'.
** 'n' - Fills in the field `name` and `namewhat`.
** 's' - Fills in the fields `source`, `shortsrc`, `defline`,
**       `lastdefline`, and `what`.
** 'l' - Fills in the field `currentline`.
** 'u' - Fills in the field `nupvals`.
** 'f' - Pushes onto the stack the function that is running at the
**       given level.
**
** This function returns 0 on error (for instance if any option in 'options'
** is invalid).
*/
CS_API int cs_getinfo(cs_State *ts, const char *options, cs_DebugInfo *di) {
    CallFrame *cf;
    Closure *cl;
    TValue *func;
    int status = 1;
    cs_lock(ts);
    api_check(ts, options, "'options' is NULL");
    if (*options == '>') {
        cf = NULL; /* not currently running */
        func = s2v(ts->sp.p - 1);
        api_check(ts, ttisfunction(func), "expect function");
        options++; /* skip '>' */
        ts->sp.p--;
    } else {
        cf = ts->cf;
        func = s2v(cf->func.p);
        cs_assert(ttisfunction(func));
    }
    cl = (ttisCSclosure(func) ? clval(func) : NULL);
    status = getinfo(ts, options, cl, cf, di);
    if (strchr(options, 'f')) {
        setobj2s(ts, ts->sp.p, func);
        api_inctop(ts);
    }
    cs_unlock(ts);
    return status;
}


/* add usual debug information to 'msg' (source id and line) */
const char *csD_addinfo(cs_State *ts, const char *msg, OString *src,
                        int line) {
    char buffer[CSI_MAXSRC];
    if (src) {
        csS_sourceid(buffer, getstr(src), getstrlen(src));
    } else {
        buffer[0] = '?';
        buffer[1] = '\0';
    }
    return csS_pushfstring(ts, "%s:%d: %s", buffer, line, msg);
}


/* generic runtime error */
cs_noret csD_runerror(cs_State *ts, const char *fmt, ...) {
    va_list ap;
    const char *err;
    va_start(ap, fmt);
    err = csS_pushvfstring(ts, fmt, ap);
    va_end(ap);
    if (isCScript(ts->cf)) {
        csD_addinfo(ts, err, cfProto(ts->cf)->source, currentline(ts->cf));
        setobj2s(ts, ts->sp.p - 2, s2v(ts->sp.p - 1)); /* remove 'err' */
        ts->sp.p--;
    }
    csPR_throw(ts, CS_ERRRUNTIME);
}


/* global variable related error */
cs_noret csD_globalerror(cs_State *ts, const char *err, OString *name) {
    csD_runerror(ts, "%s global variable '%s'", err, getstr(name));
}


/* operation on invalid type error */
cs_noret csD_typeerror(cs_State *ts, const TValue *v, const char *op) {
    csD_runerror(ts, "tried to %s %s value", op, typename(ttype(v)));
}


cs_noret csD_typeerrormeta(cs_State *ts, const TValue *v1, const TValue *v2,
                           const char * mop) {
    csD_runerror(ts, "tried to %s %s and %s values",
                     mop, typename(ttype(v1)), typename(ttype(v2)));
}


/* arithmetic operation error */
cs_noret csD_operror(cs_State *ts, const TValue *v1, const TValue *v2,
                     const char *op) {
    if (ttisnum(v1))
        v1 = v2;  /* correct error value */
    csD_typeerror(ts, v1, op);
}


/* ordering error */
cs_noret csD_ordererror(cs_State *ts, const TValue *v1, const TValue *v2) {
    const char *name1 = typename(ttype(v1));
    const char *name2 = typename(ttype(v2));
    if (name1 == name2) /* point to same entry ? */
        csD_runerror(ts, "tried to compare two %s values", name1);
    else
        csD_runerror(ts, "tried to compare %s with %s", name1, name2);
}


cs_noret csD_concaterror(cs_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1)) v1 = v2;
    csD_typeerror(ts, v1, "concatenate");
}


cs_noret csD_callerror(cs_State *ts, const TValue *o) {
    csD_typeerror(ts, o, "call");
}


cs_noret csD_indexerror(cs_State *ts, cs_Integer index, const char *what) {
    csD_runerror(ts, "array index '%I' is %s", index, what);
}


cs_noret csD_indextypeerror(cs_State *ts, const TValue *index) {
    cs_assert(ttypetag(index) != CS_VNUMINT);
    csD_runerror(ts, "invalid array index type (%s), expected integer",
                     typename(ttype(index)));
}


cs_noret csD_errormsg(cs_State *ts) {
    if (ts->errfunc != 0) { /* have error func? */
        SPtr errfunc = restorestack(ts, ts->errfunc); /* get it */
        cs_assert(ttisfunction(s2v(errfunc))); /* must be a function */
        setobjs2s(ts, ts->sp.p, ts->sp.p - 1); /* move argument */
        setobjs2s(ts, ts->sp.p - 1, errfunc); /* push function */
        ts->sp.p++; /* assume EXTRA_STACK */
        csV_call(ts, ts->sp.p - 2, 1);
    }
    csPR_throw(ts, CS_ERRRUNTIME); /* raise a regular runtime error */
}
