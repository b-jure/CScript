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
#include "ctrace.h"
#include "cgc.h"


#define CScriptClosure(cl)      ((cl) != NULL && (cl)->c.tt_ == CS_VCSCL)


/*
** Get a "base line" to find the line corresponding to an instruction.
** Base lines are regularly placed at MAXIWTHABS intervals, so usually
** an integer division gets the right place. When the source file has
** large sequences of empty/comment lines, it may need extra entries,
** so the original estimate needs a correction.
** If the original estimate is -1, the initial 'if' ensures that the
** 'while' will run at least once.
** The assertion that the estimate is a lower bound for the correct base
** is valid as long as the debug info has been generated with the same
** value for MAXIWTHABS or smaller.
*/
static int getbaseline(const Proto *p, int pc, int *basepc) {
    if (p->sizeabslineinfo == 0 || pc < p->abslineinfo[0].pc) {
        *basepc = 0; /* start from the beginning */
        return p->defline; /* (from the line this function was defined) */
    } else {
        int i = cast_uint(pc) / MAXIWTHABS - 1; /* get an estimate */
        /* estimate must be a lower bound of the correct base */
        cs_assert(i < 0 || /* linedif was too large before MAXIWTHABS? */
                 (i < p->sizeabslineinfo && p->abslineinfo[i].pc <= pc));
        while (i + 1 < p->sizeabslineinfo && pc >= p->abslineinfo[i + 1].pc)
            i++; /* low estimate; adjust it */
        *basepc = p->abslineinfo[i].pc;
        return p->abslineinfo[i].line;
    }
}


/*
** Get the line corresponding to instruction 'pc' in function prototype 'p';
** first gets a base line and from there does the increments until the
** desired instruction.
*/
int csD_getfuncline(const Proto *p, int pc) {
    if (p->lineinfo == NULL) { /* no debug information? */
        cs_assert(0); /* (currently CScript always contains debug info) */
        return -1;
    } else {
        int basepc;
        int baseline = getbaseline(p, pc, &basepc);
        while (basepc < pc) { /* walk until given instruction */
            basepc += getOpSize(p->code[basepc]);
            cs_assert(p->lineinfo[basepc] != ABSLINEINFO);
            baseline += p->lineinfo[basepc]; /* correct line */
        }
        return baseline;
    }
}


c_sinline int relpc(const CallFrame *cf) {
    cs_assert(isCScript(cf));
    return cast_int(cf->pc - cfProto(cf)->code) - 1;
}


/* get current line number */
c_sinline int getcurrentline(CallFrame *cf) {
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
    } else if (isCScript(cf)) {
        return funcnamefromcode(ts, cfProto(cf), relpc(cf), name);
    } else
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
static int auxgetinfo(cs_State *ts, const char *options, Closure *cl,
                      CallFrame *cf, cs_DebugInfo *di) {
    int status = 1;
    for (unsigned char opt = *options++; opt != '\0'; opt = *options++) {
        switch (opt) {
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
                di->currline = (cf && isCScript(cf) ? getcurrentline(cf) : -1);
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
** `>` - Pops the function on top of the stack and loads it into 'cf'.
** `n` - Fills in the field `name` and `namewhat`.
** `s` - Fills in the fields `source`, `shortsrc`, `defline`,
**       `lastdefline`, and `what`.
** `l` - Fills in the field `currentline`.
** `u` - Fills in the field `nupvals`.
** `f` - Pushes onto the stack the function that is running at the
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
        cf = di->cf;
        func = s2v(cf->func.p);
        cs_assert(ttisfunction(func));
    }
    cl = (ttisCSclosure(func) ? clval(func) : NULL);
    status = auxgetinfo(ts, options, cl, cf, di);
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
c_noret csD_runerror(cs_State *ts, const char *fmt, ...) {
    va_list ap;
    const char *err;
    va_start(ap, fmt);
    err = csS_pushvfstring(ts, fmt, ap);
    va_end(ap);
    if (isCScript(ts->cf)) {
        csD_addinfo(ts, err, cfProto(ts->cf)->source, getcurrentline(ts->cf));
        setobj2s(ts, ts->sp.p - 2, s2v(ts->sp.p - 1)); /* remove 'err' */
        ts->sp.p--;
    }
    csPR_throw(ts, CS_ERRRUNTIME);
}


/* global variable related error */
c_noret csD_globalerror(cs_State *ts, const char *err, OString *name) {
    csD_runerror(ts, "%s global variable '%s'", err, getstr(name));
}


/* operation on invalid type error */
c_noret csD_typeerror(cs_State *ts, const TValue *v, const char *op) {
    csD_runerror(ts, "tried to %s %s value", op, typename(ttype(v)));
}


c_noret csD_typeerrormeta(cs_State *ts, const TValue *v1, const TValue *v2,
                           const char * mop) {
    csD_runerror(ts, "tried to %s %s and %s values",
                     mop, typename(ttype(v1)), typename(ttype(v2)));
}


/* arithmetic operation error */
c_noret csD_operror(cs_State *ts, const TValue *v1, const TValue *v2,
                     const char *op) {
    if (ttisnum(v1))
        v1 = v2;  /* correct error value */
    csD_typeerror(ts, v1, op);
}


/* ordering error */
c_noret csD_ordererror(cs_State *ts, const TValue *v1, const TValue *v2) {
    const char *name1 = typename(ttype(v1));
    const char *name2 = typename(ttype(v2));
    if (name1 == name2) /* point to same entry ? */
        csD_runerror(ts, "tried to compare two %s values", name1);
    else
        csD_runerror(ts, "tried to compare %s with %s", name1, name2);
}


c_noret csD_concaterror(cs_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1)) v1 = v2;
    csD_typeerror(ts, v1, "concatenate");
}


c_noret csD_callerror(cs_State *ts, const TValue *o) {
    csD_typeerror(ts, o, "call");
}


c_noret csD_indexerror(cs_State *ts, cs_Integer index, const char *what) {
    csD_runerror(ts, "array index '%I' is %s", index, what);
}


c_noret csD_indextypeerror(cs_State *ts, const TValue *index) {
    cs_assert(ttypetag(index) != CS_VNUMINT);
    csD_runerror(ts, "invalid array index type (%s), expected integer",
                     typename(ttype(index)));
}


c_noret csD_errormsg(cs_State *ts) {
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
