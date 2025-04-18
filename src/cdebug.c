/*
** cdebug.c
** Debug and error reporting functions
** See Copyright Notice in cscript.h
*/

#define cdebug_c
#define CS_CORE

#include "cprefix.h"

#include <string.h>

#include "cscript.h"
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


/*
** Gets the Number of instructions up to 'pc' in O(log(n)).
** As CSCript implementation is using stack-based architecture for its VM,
** we then also store additional pc for each instruction emitted for a
** faster lookup. This is required in order to properly calculate estimate
** when fetching base line.
*/
static int Ninstuptopc(const Proto *p, int pc) {
    int l = 0;
    int h = p->sizeinstpc - 1;
    int m, instpc;
    cs_assert(h >= 0);
    while (l <= h) {
        m = l + ((h - l) / 2); /* avoid potential overflow */
        instpc = p->instpc[m];
        if (pc < instpc)
            h = m - 1;
        else if (instpc < pc)
            l = m + 1;
        else
            return m;
    }
    cs_assert(0); /* 'pc' does not correspond to any instruction */
}


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
        return p->defline + p->lineinfo[0]; /* first instruction line */
    } else {
        int i = cast_uint(Ninstuptopc(p, pc)) / MAXIWTHABS - 1; /* get an estimate */
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
    int basepc;
    int baseline = getbaseline(p, pc, &basepc);
    while (basepc < pc) { /* walk until given instruction */
        basepc += getOpSize(p->code[basepc]); /* next instruction pc */
        cs_assert(p->lineinfo[basepc] != ABSLINEINFO);
        cs_assert(p->lineinfo[basepc] != ARGLINEINFO);
        baseline += p->lineinfo[basepc]; /* correct line */
    }
    cs_assert(pc == basepc);
    return baseline;
}


c_sinline int relpc(const CallFrame *cf) {
    cs_assert(isCScript(cf));
    return cast_int(cf->pc - cf_func(cf)->p->code) - 1;
}


/* get current line number */
c_sinline int getcurrentline(CallFrame *cf) {
    return csD_getfuncline(cf_func(cf)->p, relpc(cf));
}


static const char *findvararg(CallFrame *cf, SPtr *pos, int n) {
    if (cf_func(cf)->p->isvararg) {
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
const char *csD_findlocal(cs_State *C, CallFrame *cf, int n, SPtr *pos) {
    SPtr base = cf->func.p + 1;
    const char *name = NULL;
    if (isCScript(cf)) {
        if (n < 0) /* vararg ? */
            return findvararg(cf, pos, n);
        else /* otherwise local variable */
            name = csF_getlocalname(cf_func(cf)->p, n, relpc(cf));
    }
    if (name == NULL) {
        SPtr limit = (cf == C->cf) ? C->sp.p : cf->next->func.p;
        if (limit - base >= n && n > 0) /* 'n' is in stack range ? */
            name = isCScript(cf) ? "(auto)" : "(C auto)";
        else
            return NULL;
    }
    if (pos)
        *pos = base + (n - 1);
    return name;
}


CS_API const char *cs_getlocal(cs_State *C, const cs_Debug *ar, int n) {
    const char *name;
    cs_lock(C);
    if (ar == NULL) {
        if (!ttisCSclosure(s2v(C->sp.p - 1)))
            name = NULL;
        else
            name = csF_getlocalname(clCSval(s2v(C->sp.p - 1))->p, n, 0);
    } else {
        SPtr pos = NULL;
        name = csD_findlocal(C, ar->cf, n, &pos);
        if (name) { /* found ? */
            setobjs2s(C, C->sp.p, pos);
            api_inctop(C);
        }
    }
    cs_unlock(C);
    return name;
}


CS_API const char *cs_setlocal (cs_State *C, const cs_Debug *ar, int n) {
    SPtr pos = NULL;
    const char *name;
    cs_lock(C);
    name = csD_findlocal(C, ar->cf, n, &pos);
    if (name) { /* found ? */
        setobjs2s(C, pos, C->sp.p - 1); /* set the value */
        C->sp.p--; /* remove value */
    }
    cs_unlock(C);
    return name;
}


static void getfuncinfo(Closure *cl, cs_Debug *ar) {
    if (!CScriptClosure(cl)) {
        ar->source = "[C]";
        ar->srclen = SLL("[C]");
        ar->defline = -1;
        ar->lastdefline = -1;
        ar->what = "C";
    } else {
        const Proto *p = cl->cs.p;
        if (p->source) { /* have source? */
          ar->source = getstr(p->source);
          ar->srclen = getstrlen(p->source);
        } else {
          ar->source = "?";
          ar->srclen = SLL("?");
        }
        ar->defline = p->defline;
        ar->lastdefline = p->deflastline;
        ar->what = (ar->lastdefline == 0) ? "main" : "CScript";
    }
    csS_sourceid(ar->shortsrc, ar->source, ar->srclen);
}


static const char *funcnamefromcode(cs_State *C, const Proto *p, int pc,
                                    const char **name) {
    cs_MM mm;
    Instruction *i = &p->code[pc];
    switch (*i) {
        case OP_CALL: return NULL; /* TODO(symbolic execution) */
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
    *name = getstr(G(C)->mmnames[mm]) + 2; /* skip '__' */
    return "metamethod";
}


/* try to find a name for a function based on how it was called */
static const char *funcnamefromcall(cs_State *C, CallFrame *cf,
                                    const char **name) {
    if (cf->status & CFST_FIN) { /* called as finalizer? */
        *name = "__gc";
        return "metamethod";
    } else if (isCScript(cf)) {
        return funcnamefromcode(C, cf_func(cf)->p, relpc(cf), name);
    } else
        return NULL;
}


static const char *getfuncname(cs_State *C, CallFrame *cf, const char **name) {
    if (cf != NULL)
        return funcnamefromcall(C, cf->prev, name);
    else
        return NULL;
}


/*
** Auxiliary to 'cs_getinfo', parses 'options' and fills out the
** 'cs_Debug' accordingly. If any invalid options is specified this
** returns 0.
*/
static int auxgetinfo(cs_State *C, const char *options, Closure *cl,
                      CallFrame *cf, cs_Debug *ar) {
    int status = 1;
    for (; *options; options++) {
        switch (*options) {
            case 'n': {
                ar->namewhat = getfuncname(C, cf, &ar->name);
                if (ar->namewhat == NULL) { /* not found ? */
                    ar->namewhat = "";
                    ar->name = NULL;
                }
                break;
            }
            case 's': {
                getfuncinfo(cl, ar);
                break;
            }
            case 'l': {
                ar->currline = (cf && isCScript(cf) ? getcurrentline(cf) : -1);
                break;
            }
            case 'u': {
                ar->nupvals = (cl ? cl->c.nupvalues : 0);
                if (CScriptClosure(cl)) {
                    ar->nparams = cl->cs.p->arity;
                    ar->isvararg = cl->cs.p->isvararg;
                } else {
                    ar->nparams = 0;
                    ar->isvararg = 1;
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
** Fill out 'cs_Debug' according to 'options'.
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
CS_API int cs_getinfo(cs_State *C, const char *options, cs_Debug *ar) {
    CallFrame *cf;
    Closure *cl;
    TValue *func;
    int status = 1;
    cs_lock(C);
    api_check(C, options, "'options' is NULL");
    if (*options == '>') {
        cf = NULL; /* not currently running */
        func = s2v(C->sp.p - 1);
        api_check(C, ttisfunction(func), "expect function");
        options++; /* skip '>' */
        C->sp.p--;
    } else {
        cf = ar->cf;
        func = s2v(cf->func.p);
        cs_assert(ttisfunction(func));
    }
    cl = (ttisCSclosure(func) ? clval(func) : NULL);
    status = auxgetinfo(C, options, cl, cf, ar);
    if (strchr(options, 'f')) {
        setobj2s(C, C->sp.p, func);
        api_inctop(C);
    }
    cs_unlock(C);
    return status;
}


/* add usual debug information to 'msg' (source id and line) */
const char *csD_addinfo(cs_State *C, const char *msg, OString *src,
                        int line) {
    char buffer[CS_MAXSRC];
    if (src) {
        csS_sourceid(buffer, getstr(src), getstrlen(src));
    } else {
        buffer[0] = '?';
        buffer[1] = '\0';
    }
    return csS_pushfstring(C, "%s:%d: %s", buffer, line, msg);
}


c_noret csD_errormsg(cs_State *C) {
    if (C->errfunc != 0) { /* is there an error handling function? */
        SPtr errfunc = restorestack(C, C->errfunc);
        cs_assert(ttisfunction(s2v(errfunc)));
        setobjs2s(C, C->sp.p, C->sp.p - 1); /* move argument */
        setobjs2s(C, C->sp.p - 1, errfunc); /* push function */
        C->sp.p++; /* assume EXTRA_STACK */
        csV_call(C, C->sp.p - 2, 1); /* call it */
    }
    csPR_throw(C, CS_ERRRUNTIME);
}


/* generic runtime error */
c_noret csD_runerror(cs_State *C, const char *fmt, ...) {
    CallFrame *cf = C->cf;
    const char *err;
    va_list ap;
    csG_checkGC(C);
    va_start(ap, fmt);
    err = csS_pushvfstring(C, fmt, ap);
    va_end(ap);
    if (isCScript(cf)) { /* can add source information? */
        csD_addinfo(C, err, cf_func(cf)->p->source, getcurrentline(cf));
        setobj2s(C, C->sp.p - 2, s2v(C->sp.p - 1)); /* remove 'err' */
        C->sp.p--;
    }
    csD_errormsg(C);
}


/* global variable related error */
c_noret csD_globalerror(cs_State *C, const char *err, OString *name) {
    csD_runerror(C, "%s global variable '%s'", err, getstr(name));
}


/* operation on invalid type error */
c_noret csD_typeerror(cs_State *C, const TValue *v, const char *op) {
    csD_runerror(C, "tried to %s %s value", op, typename(ttype(v)));
}


c_noret csD_typeerrormeta(cs_State *C, const TValue *v1, const TValue *v2,
                           const char * mop) {
    csD_runerror(C, "tried to %s %s and %s values",
                     mop, typename(ttype(v1)), typename(ttype(v2)));
}


/* arithmetic operation error */
c_noret csD_operror(cs_State *C, const TValue *v1, const TValue *v2,
                     const char *op) {
    if (ttisnum(v1))
        v1 = v2;  /* correct error value */
    csD_typeerror(C, v1, op);
}


/* ordering error */
c_noret csD_ordererror(cs_State *C, const TValue *v1, const TValue *v2) {
    const char *name1 = typename(ttype(v1));
    const char *name2 = typename(ttype(v2));
    if (name1 == name2) /* point to same entry ? */
        csD_runerror(C, "tried to compare two %s values", name1);
    else
        csD_runerror(C, "tried to compare %s with %s", name1, name2);
}


c_noret csD_concaterror(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1)) v1 = v2;
    csD_typeerror(C, v1, "concatenate");
}


c_noret csD_callerror(cs_State *C, const TValue *o) {
    csD_typeerror(C, o, "call");
}


c_noret csD_indexerror(cs_State *C, cs_Integer index, const char *what) {
    csD_runerror(C, "list index '%I' is %s", index, what);
}


c_noret csD_indextypeerror(cs_State *C, const TValue *index) {
    cs_assert(ttypetag(index) != CS_VNUMINT);
    csD_runerror(C, "invalid list index type (%s), expected integer",
                     typename(ttype(index)));
}


/* TODO(v2.0): implement hooks */
int csD_traceexec(cs_State *C, const Instruction *pc) {
    UNUSED(pc); /* no hooks */
    C->cf->trap = 0; /* stack reallocation handled */
    return 0; /* turn off 'trap' */
}
