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
        basepc += getOpSize(p->code[basepc]); /* next instruction pc */
        cs_assert(p->lineinfo[basepc] != ARGLINEINFO);
        baseline += p->lineinfo[basepc]; /* correct line */
    }
    cs_assert(pc == basepc);
    return baseline;
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
        ar->srclen = LL("[C]");
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
          ar->srclen = LL("?");
        }
        ar->defline = p->defline;
        ar->lastdefline = p->deflastline;
        ar->what = (ar->lastdefline == 0) ? "main" : "CScript";
    }
    csS_sourceid(ar->shortsrc, ar->source, ar->srclen);
}


static const char *funcnamefromcode(cs_State *C, const Proto *p, int pc,
                                    const char **name) {
    int mm;
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


static TValue *unwrapfunc(TValue *func) {
    if (ttisinstancemethod(func))
        return &imval(func)->method; /* unwrap instance method */
    else if (ttisusermethod(func))
        return &umval(func)->method; /* unwrap userdata method */
    else /* otherwise return as is... */
        return func;
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
    cs_unlock(C);
    return status;
}


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
        settraps(C->cf);  /* to trace inside 'luaV_execute' */
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
    csD_runerror(C, "invalid list index type (%s), expected integer",
                    typename(ttype(index)));
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
            pc += getOpSize(p->code[pc]);
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
** Executes a call hook for Lua functions. This function is called
** whenever 'hookmask' is not zero, so it checks whether call hooks are
** active.
*/
void csD_hookcall(cs_State *C, CallFrame *cf) {
    C->oldpc = 0; /* set 'oldpc' for new function */
    if (C->hookmask & CS_MASK_CALL) { /* is call hook on? */
        Proto *p = cf_func(cf)->p;
        int delta = getOpSize(*cf->cs.pc);
        cf->cs.pc += delta; /* hooks assume 'pc' is already incremented */
        csD_hook(C, CS_HOOK_CALL, -1, 0, p->arity);
        cf->cs.pc -= delta; /* correct 'pc' */
    }
}


/*
** Traces CScript calls. If code is running the first instruction of
** a function, and function is not vararg, calls 'luaD_hookcall'.
** (Vararg functions will call 'luaD_hookcall' after adjusting its
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
    pc += getOpSize(*pc); /* reference is always next instruction */
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
