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
#include "ctable.h"

#include <stdio.h>


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
    return -1; /* prevent compiler warnings */
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
        /* get an estimate */
        int i = cast_uint(Ninstuptopc(p, pc)) / MAXIWTHABS - 1;
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
        basepc += getopSize(p->code[basepc]); /* next instruction pc */
        cs_assert(p->lineinfo[basepc] != ARGLINEINFO);
        baseline += p->lineinfo[basepc]; /* correct line */
    }
    cs_assert(pc == basepc);
    return baseline;
}


static TValue *unwrapfunc(TValue *func) {
    if (ttisinstancemethod(func))
        return &imval(func)->method; /* unwrap instance method */
    else if (ttisusermethod(func))
        return &umval(func)->method; /* unwrap userdata method */
    else /* otherwise return as is... */
        return func;
}


c_sinline int currentpc(const CallFrame *cf) {
    return relpc(cf->cs.pc, cf_func(cf)->p);
}


/* get current line number */
c_sinline int getcurrentline(CallFrame *cf) {
    cs_assert(isCScript(cf));
    return csD_getfuncline(cf_func(cf)->p, currentpc(cf));
}


static const char *findvararg(CallFrame *cf, SPtr *pos, int n) {
    if (cf_func(cf)->p->isvararg) {
        int nextra = cf->cs.nvarargs;
        if (n >= -nextra) {
            *pos = cf->func.p - nextra - (n + 1);
            return "(vararg)";
        }
    }
    return NULL;
}


const char *csD_findlocal(cs_State *C, CallFrame *cf, int n, SPtr *pos) {
    SPtr base = cf->func.p + 1;
    const char *name = NULL;
    if (isCScript(cf)) {
        if (n < 0) /* vararg ? */
            return findvararg(cf, pos, n);
        else /* otherwise local variable */
            name = csF_getlocalname(cf_func(cf)->p, n, currentpc(cf));
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
    if (ar == NULL) { /* information about non-active function? */
        const TValue *func = unwrapfunc(s2v(C->sp.p - 1));
        if (!ttisCSclosure(func)) /* not a CScript function? */
            name = NULL;
        else /* consider live variables at function start (parameters) */
            name = csF_getlocalname(clCSval(func)->p, n, 0);
    } else { /* active function; get information through 'ar' */
        SPtr pos = NULL; /* to avoid warnings */
        name = csD_findlocal(C, ar->cf, n, &pos);
        if (name) { /* found ? */
            /* push its value on top of the stack */
            setobjs2s(C, C->sp.p, pos);
            api_inctop(C);
        }
    }
    cs_unlock(C);
    return name;
}


CS_API const char *cs_setlocal(cs_State *C, const cs_Debug *ar, int n) {
    SPtr pos = NULL;
    const char *name;
    cs_lock(C);
    name = csD_findlocal(C, ar->cf, n, &pos);
    if (name) { /* found ? */
        setobjs2s(C, pos, C->sp.p - 1);
        C->sp.p--; /* pop value */
    }
    cs_unlock(C);
    return name;
}


static void getfuncinfo(Closure *cl, cs_Debug *ar) {
    if (!CScriptClosure(cl)) {
        ar->source = "=[C]";
        ar->srclen = LL("=[C]");
        ar->defline = -1;
        ar->lastdefline = -1;
        ar->what = "C";
    } else {
        const Proto *p = cl->cs.p;
        if (p->source) { /* have source? */
            ar->source = getstr(p->source);
            ar->srclen = getstrlen(p->source);
        } else {
            ar->source = "=?";
            ar->srclen = LL("=?");
        }
        ar->defline = p->defline;
        ar->lastdefline = p->deflastline;
        ar->what = (ar->lastdefline == 0) ? "main" : "CScript";
    }
    csS_chunkid(ar->shortsrc, ar->source, ar->srclen);
}


static int filterpc(int pc, int jmptarget, int ltop, int *top) {
    if (pc < jmptarget) /* is code conditional (inside a jump)? */
        return (*top = -1); /* cannot know who sets that stack slot */
    else
        return (*top = ltop, pc); /* current position sets that stack slot */
}


static Instruction symbexec(const Proto *p, int lastpc, int *top, int sp) {
    const Instruction *code = p->code;
    int pc = 0;
    int ltop = p->arity;
    int setsp = -1;
    int jmptarget = 0;
    if (code[0] == OP_VARARGPREP) /* vararg function? */
        pc += getopSize(OP_VARARGPREP); /* skip first opcode */
    for (; pc < lastpc; pc += opsize(code, pc)) {
        const Instruction *i = &code[pc];
        int change; /* true if current instruction changed 'sp' */
        printf("ltop = %d, p->maxstack = %d\n", ltop, p->maxstack);
        cs_assert(0 <= ltop && ltop <= p->maxstack);
        switch (*i) {
            case OP_RET: {
                int stk = GET_ARG_L(i, 0);
                cs_assert(stk <= ltop);
                change = (stk == ltop);
                ltop = stk;
                break;
            }
            case OP_CALL: {
                int stk = GET_ARG_L(i, 0);
                int nresults = GET_ARG_L(i, 1);
                if (nresults == CS_MULRET) nresults = 1;
                cs_assert(stk <= ltop);
                change = (sp >= stk);
                ltop = stk + nresults;
                break;
            }
            case OP_NILN: case OP_VARARG: {
                int n = GET_ARG_L(i, 0);
                if (*i == OP_VARARG) {
                    if (--n == CS_MULRET) n = 1;
                }
                change = (ltop <= sp && sp <= ltop + n);
                ltop += n;
                break;
            }
            case OP_POPN: {
                ltop -= GET_ARG_L(i, 0);
                change = 0;
                break;
            }
            case OP_CONCAT: {
                ltop -= GET_ARG_L(i, 0) - 1;
                change = (ltop == sp);
                break;
            }
            case OP_SETLIST: {
                ltop = GET_ARG_L(i, 0);
                change = 0;
                break;
            }
            case OP_JMP: case OP_JMPS: {
                int off = GET_ARG_L(i, 0) * (-1 + 2 * (*i == OP_JMP));
                int dest = (pc + getopSize(*i)) + off;
                /* jump does not skip 'lastpc' and is larger than current? */
                if (dest <= lastpc && jmptarget < dest)
                    jmptarget = dest; /* update 'jmptarget' */
                change = 0;
                break;
            }
            case OP_FORLOOP: {
                change = (GET_ARG_L(i, 0) == sp);
                ltop -= GET_ARG_L(i, 2);
                break;
            }
            default: {
                int npush = csC_opproperties[*i].push;
                int delta = getopDelta(*i);
                cs_assert(delta != VD); /* default case can't handle VD */
                if (npush > 0)
                    change = (ltop <= sp && sp <= ltop + npush);
                else change = 0;
                ltop += delta;
            }
        }
        if (change)
            setsp = filterpc(pc, jmptarget, ltop, top);
    }
    return setsp;
}


static const char *upvalname(const Proto *p, int uv) {
    OString *s = check_exp(uv < p->sizeupvals, p->upvals[uv].name);
    cs_assert(s != NULL); /* must have debug information */
    return getstr(s);
}


/*
** Find a "name" for the constant 'c'.
*/
static const char *kname(const Proto *p, int index, const char **name) {
    TValue *kval = &p->k[index];
    if (ttisstring(kval)) {
        *name = getstr(strval(kval));
        return "constant";
    } else {
        *name = "?";
        return NULL;
    }
}


static const char *basicgetobjname(const Proto *p, int *ppc, int sp,
                                   int *top, const char **name) {
    int pc = *ppc;
    *name = csF_getlocalname(p, sp + 1, pc);
    if (*name)  /* is a local? */
        return "local";
    /* else try symbolic execution */
    *ppc = pc = symbexec(p, pc, top, sp);
    if (pc != -1) { /* could find instruction? */
        Instruction *i = &p->code[pc];
        switch (*i) {
            case OP_GETLOCAL: {
                int stk = GET_ARG_L(i, 0);
                cs_assert(stk < sp);
                return basicgetobjname(p, ppc, stk, top, name);
            }
            case OP_GETUVAL: {
                *name = upvalname(p, GET_ARG_L(i, 0));
                return "upvalue";
            }
            case OP_CONST: return kname(p, GET_ARG_S(i, 0), name);
            case OP_CONSTL: return kname(p, GET_ARG_L(i, 0), name);
            default: break;
        }
    }
    return NULL; /* could not find reasonable name */
}


/*
** Find a "name" for the stack slot 'c'.
*/
static void sname(const Proto *p, int pc, int c, const char **name) {
    int dummy;
    const char *what = basicgetobjname(p, &pc, c, &dummy, name);
    if (!(what && *what == 'c')) /* did not find a constant name? */
        *name = "?";
}


/*
** Check whether table at stack slot 't' is the environment '__ENV'.
*/
static const char *isEnv(const Proto *p, int pc, int t, int isup) {
    int dummy;
    const char *name; /* name of indexed variable */
    if (isup) /* is 't' an upvalue? */
        name = upvalname(p, t);
    else /* 't' is a stack slot */
        basicgetobjname(p, &pc, t, &dummy, &name);
    return (name && strcmp(name, CS_ENV) == 0) ? "global" : "field";
}


/*
** Extend 'basicgetobjname' to handle table accesses.
*/
static const char *getobjname(const Proto *p, int lastpc, int sp,
                              const char **name) {
    int top;
    const char *kind = basicgetobjname(p, &lastpc, sp, &top, name);
    if (kind != NULL)
        return kind;
    else if (lastpc != -1) { /* could find instruction? */
        Instruction *i = &p->code[lastpc];
        switch (*i) {
            case OP_GETPROPERTY: case OP_GETINDEXSTR: {
                int k = GET_ARG_L(i, 0); /* key index */
                kname(p, k, name);
                return isEnv(p, lastpc, top-1, 0);
            }
            case OP_GETINDEX: {
                sname(p, lastpc, top-1, name);
                return isEnv(p, lastpc, top-2, 0);
            }
            case OP_GETINDEXINT: case OP_GETINDEXINTL: {
                *name = "integer index";
                return "field";
            }
            case OP_GETSUP: case OP_GETSUPIDXSTR: {
                int k = GET_ARG_L(i, 0);
                kname(p, k, name);
                return "superclass field";
            }
            case OP_GETSUPIDX: {
                sname(p, lastpc, top-1, name);
                return "superclass field";
            }
            default: break; /* go through to return NULL */
        }
    }
    return NULL; /* could not find reasonable name */
}


static const char *funcnamefromcode(cs_State *C, const Proto *p, int pc,
                                    const char **name) {
    int mm;
    Instruction *i = &p->code[pc];
    switch (*i) {
        case OP_CALL:
            return NULL;
            //return getobjname(p, pc, GET_ARG_L(i, 0), name);
        case OP_FORCALL: {
            *name = "for iterator";
            return "for iterator";
        }
        case OP_GETPROPERTY: case OP_GETINDEX:
        case OP_GETINDEXSTR: case OP_GETINDEXINT: {
            mm = CS_MM_GETIDX;
            break;
        }
        case OP_SETPROPERTY: case OP_SETINDEX:
        case OP_SETINDEXSTR: case OP_SETINDEXINT: {
            mm = CS_MM_SETIDX;
            break;
        }
        case OP_MBIN: {
            mm = GET_ARG_S(i, 0);
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
    if (cf->status & CFST_HOOKED) { /* was it called inside a hook? */
        *name = "?";
        return "hook";
    }
    if (cf->status & CFST_FIN) { /* was it called as finalizer? */
        *name = "__gc";
        return "metamethod";
    } else if (isCScript(cf))
        return funcnamefromcode(C, cf_func(cf)->p, currentpc(cf), name);
    else
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
                ar->currline = (cf && isCScript(cf)) ? getcurrentline(cf) : -1;
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
            case 'r': {
                if (cf == NULL || !(cf->status & CFST_TRAN))
                    ar->ftransfer = ar->ntransfer = 0;
                else {
                    ar->ftransfer = cf->ftransfer;
                    ar->ntransfer = cf->ntransfer;
                }
                break;
            }
            case 'f':
            case 'L': /* handled by 'cs_getinfo' */
                break;
            default: { /* invalid option */
                status = 0;
                break;
            }
        }
    }
    return status;
}


static int nextline (const Proto *p, int currline, int pc) {
    if (p->lineinfo[pc] != ABSLINEINFO)
        return currline + p->lineinfo[pc];
    else
        return csD_getfuncline(p, pc);
}


static void collectvalidlines(cs_State *C, Closure *f) {
    if (!CScriptClosure(f)) {
        setnilval(s2v(C->sp.p));
        api_inctop(C);
    } else {
        int i;
        TValue v;
        const Proto *p = f->cs.p;
        int currline = p->defline;
        Table *t = csH_new(C); /* new table to store active lines */
        settval2s(C, C->sp.p, t); /* push it on stack */
        api_inctop(C);
        cs_assert(p->lineinfo != NULL); /* must have debug information */
        setbtval(&v); /* bool 'true' to be the value of all indices */
        if (!p->isvararg) /* regular function? */
            i = 0; /* consider all instructions */
        else { /* vararg function */
            cs_assert(p->code[0] == OP_VARARGPREP);
            currline = nextline(p, currline, 0);
            i = getopSize(OP_VARARGPREP); /* skip first instruction */
        }
        while (i < p->sizelineinfo) { /* for each instruction */
            currline = nextline(p, currline, i); /* get its line */
            csH_setint(C, t, currline, &v); /* table[line] = true */
            i += getopSize(p->code[i]); /* get next instruction */
        }
    }
}


// TODO: update docs
CS_API int cs_getinfo(cs_State *C, const char *options, cs_Debug *ar) {
    CallFrame *cf;
    Closure *cl;
    TValue *func;
    int status = 1;
    cs_lock(C);
    api_check(C, options != NULL, "'options' can't be NULL");
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
    func = unwrapfunc(func);
    cl = (ttisCSclosure(func) ? clval(func) : NULL);
    status = auxgetinfo(C, options, cl, cf, ar);
    if (strchr(options, 'f')) {
        setobj2s(C, C->sp.p, func);
        api_inctop(C);
    }
    if (strchr(options, 'L'))
        collectvalidlines(C, cl);
    cs_unlock(C);
    return status;
}


#include <stdio.h>
/*
** Set 'trap' for all active CScript frames.
** This function can be called during a signal, under "reasonable"
** assumptions. A new 'cf' is completely linked in the list before it
** becomes part of the "active" list, and we assume that pointers are
** atomic; see comment in next function.
** (A compiler doing interprocedural optimizations could, theoretically,
** reorder memory writes in such a way that the list could be temporarily
** broken while inserting a new element. We simply assume it has no good
** reasons to do that.)
*/
static void settraps(CallFrame *cf) {
    for (; cf != NULL; cf = cf->prev)
        if (isCScript(cf))
            cf->cs.trap = 1;
}


/*
** This function can be called during a signal, under "reasonable"
** assumptions.
** Fields 'basehookcount' and 'hookcount' (set by 'resethookcount')
** are for debug only, and it is no problem if they get arbitrary
** values (causes at most one wrong hook call). 'hookmask' is an atomic
** value. We assume that pointers are atomic too (e.g., gcc ensures that
** for all platforms where it runs). Moreover, 'hook' is always checked
** before being called (see 'csD_hook').
*/
// TODO: add docs
CS_API void cs_sethook(cs_State *C, cs_Hook func, int mask, int count) {
    if (func == NULL || mask == 0) { /* turn off hooks? */
        mask = 0;
        func = NULL;
    }
    C->hook = func;
    C->basehookcount = count;
    resethookcount(C);
    C->hookmask = cast_byte(mask);
    if (mask)
        settraps(C->cf); /* to trace inside 'csV_execute' */
}


// TODO: add docs
CS_API cs_Hook cs_gethook(cs_State *C) {
    return C->hook;
}


// TODO: add docs
CS_API int cs_gethookmask(cs_State *C) {
    return C->hookmask;
}


// TODO: add docs
CS_API int cs_gethookcount(cs_State *C) {
    return C->basehookcount;
}


/* add usual debug information to 'msg' (source id and line) */
const char *csD_addinfo(cs_State *C, const char *msg, OString *src,
                        int line) {
    char buffer[CS_IDSIZE];
    if (src)
        csS_chunkid(buffer, getstr(src), getstrlen(src));
    else {
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
    csPR_throw(C, CS_STATUS_ERUNTIME);
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
    csD_runerror(C, "tried to '%s' %s and %s values",
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
    csD_runerror(C, "list index ('%I') is %s", index, what);
}


c_noret csD_indexterror(cs_State *C, const TValue *index) {
    cs_assert(ttypetag(index) != CS_VNUMINT);
    csD_runerror(C, "invalid list index type (%s)", typename(ttype(index)));
}


c_noret csD_llenerror(cs_State *C, const char *extra) {
    csD_runerror(C, "invalid list length (%s)", extra);
}


/*
** Check whether new instruction 'newpc' is in a different line from
** previous instruction 'oldpc'. More often than not, 'newpc' is only
** one or a few instructions after 'oldpc' (it must be after, see
** caller), so try to avoid calling 'csD_getfuncline'. If they are
** too far apart, there is a good chance of a ABSLINEINFO in the way,
** so it goes directly to 'csD_getfuncline'.
*/
static int changedline(const Proto *p, int oldpc, int newpc) {
    cs_assert(p->lineinfo != NULL);
    cs_assert(oldpc < newpc);
    if (newpc - oldpc < MAXIWTHABS / 2) { /* not too far apart? */
        int delta = 0; /* line difference */
        int pc = oldpc;
        for (;;) {
            int lineinfo;
            pc += getopSize(p->code[pc]);
            lineinfo = p->lineinfo[pc];
            if (lineinfo == ABSLINEINFO)
                break; /* cannot compute delta; fall through */
            delta += lineinfo;
            if (pc == newpc)
                return (delta != 0); /* delta computed successfully */
        }
    }
    /* either instructions are too far apart or there is an absolute line
       info in the way; compute line difference explicitly */
    return (csD_getfuncline(p, oldpc) != csD_getfuncline(p, newpc));
}


/*
** Call a hook for the given event. Make sure there is a hook to be
** called. (Both 'C->hook' and 'C->hookmask', which trigger this
** function, can be changed asynchronously by signals.)
*/
void csD_hook(cs_State *C, int event, int line, int ftransfer, int ntransfer) {
    cs_Hook hook = C->hook;
    if (hook && C->allowhook) { /* make sure there is a hook */
        int mask = CFST_HOOKED;
        CallFrame *cf = C->cf;
        ptrdiff_t sp = savestack(C, C->sp.p); /* preserve original 'sp' */
        ptrdiff_t cf_top = savestack(C, cf->top.p); /* idem for 'cf->top' */
        cs_Debug ar = { .event = event, .currline = line, .cf = cf };
        if (ntransfer != 0) {
            mask |= CFST_TRAN; /* 'cf' has transfer information */
            cf->ftransfer = ftransfer;
            cf->ntransfer = ntransfer;
        }
        csPR_checkstack(C, CS_MINSTACK); /* ensure minimum stack size */
        if (cf->top.p < C->sp.p + CS_MINSTACK)
            cf->top.p = C->sp.p + CS_MINSTACK;
        C->allowhook = 0; /* cannot call hooks inside a hook */
        cf->status |= mask;
        cs_unlock(C);
        (*hook)(C, &ar);
        cs_lock(C);
        cs_assert(!C->allowhook);
        C->allowhook = 1;
        cf->top.p = restorestack(C, cf_top);
        C->sp.p = restorestack(C, sp);
        cf->status &= ~mask;
    }
}


/*
** Executes a call hook for CScript functions. This function is called
** whenever 'hookmask' is not zero, so it checks whether call hooks are
** active.
*/
void csD_hookcall(cs_State *C, CallFrame *cf) {
    C->oldpc = 0; /* set 'oldpc' for new function */
    if (C->hookmask & CS_MASK_CALL) { /* is call hook on? */
        Proto *p = cf_func(cf)->p;
        int delta = getopSize(*cf->cs.pc);
        cf->cs.pc += delta; /* hooks assume 'pc' is already incremented */
        csD_hook(C, CS_HOOK_CALL, -1, 0, p->arity);
        cf->cs.pc -= delta; /* correct 'pc' */
    }
}


/*
** Traces CScript calls. If code is running the first instruction of
** a function, and function is not vararg, calls 'csD_hookcall'
** (Vararg functions will call 'csD_hookcall' after adjusting its
** variable arguments; otherwise, they could call a line/count hook
** before the call hook.)
*/
int csD_tracecall(cs_State *C) {
    CallFrame *cf = C->cf;
    Proto *p = cf_func(cf)->p;
    cf->cs.trap = 1; /* ensure hooks will be checked */
    if (cf->cs.pc == p->code) {
        if (p->isvararg)
            return 0; /* hooks will start at VARARGPREP instruction */
        else
            csD_hookcall(C, cf); /* check 'call' hook */
    }
    return 1; /* keep 'trap' on */
}


/*
** Traces the execution of a CScript function. Called before the execution
** of each opcode, when debug is on. 'C->oldpc' stores the last
** instruction traced, to detect line changes. When entering a new
** function, 'npci' will be zero and will test as a new line whatever
** the value of 'oldpc'. Some exceptional conditions may return to
** a function without setting 'oldpc'. In that case, 'oldpc' may be
** invalid; if so, use zero as a valid value. (A wrong but valid 'oldpc'
** at most causes an extra call to a line hook.)
** This function is not "Protected" when called, so it should correct
** 'C->sp.p' before calling anything that can run the GC.
*/
int csD_traceexec(cs_State *C, const Instruction *pc) {
    CallFrame *cf = C->cf;
    c_byte mask = C->hookmask;
    const Proto *p = cf_func(cf)->p;
    int counthook;
    if (!(mask & (CS_MASK_LINE | CS_MASK_COUNT))) { /* no hooks? */
        cf->cs.trap = 0; /* don't need to stop again */
        return 0; /* turn off 'trap' */
    }
    pc += getopSize(*pc); /* reference is always next instruction */
    cf->cs.pc = pc; /* save 'pc' */
    counthook = (mask & CS_MASK_COUNT) && (--C->hookcount == 0);
    if (counthook)
        resethookcount(C); /* reset count */
    else if (!(mask & CS_MASK_LINE))
        return 1; /* no line hook and count != 0; nothing to be done now */
    if (counthook)
        csD_hook(C, CS_HOOK_COUNT, -1, 0, 0); /* call count hook */
    if (mask & CS_MASK_LINE) {
        /* 'C->oldpc' may be invalid; use zero in this case */
        int oldpc = (C->oldpc < p->sizecode) ? C->oldpc : 0;
        int npci = relpc(pc, p);
        if (npci <= oldpc || /* call hook when jump back (loop), */
                changedline(p, oldpc, npci)) { /* or when enter new line */
            int newline = csD_getfuncline(p, npci);
            csD_hook(C, CS_HOOK_LINE, newline, 0, 0); /* call line hook */
        }
        C->oldpc = npci; /* 'pc' of last call to line hook */
    }
    return 1; /* keep 'trap' on */
}
