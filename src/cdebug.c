/*
** cdebug.c
** Debug and error reporting functions
** See Copyright Notice in cscript.h
*/

#include "cdebug.h"
#include "cfunction.h"
#include "cobject.h"
#include "cstring.h"
#include "climits.h"
#include "cstate.h"
#include "cobject.h"
#include "cprotected.h"


/* get line number of instruction ('pc') */
// TODO
int crD_getfuncline(const Function *fn, int pc) {
    LineInfo *li;
    int l = 0;
    int h = fn->sizelinfo - 1;
    int m = l + ((h - l) / 2);
    cr_assert(fn->sizelinfo > 0);
    while (l <= h) {
        li = &fn->linfo[m];
        if (li->pc < pc) 
            l = m + 1;
        else if (li->pc > pc) 
            h = m - 1;
        else 
            break;
        m = l + ((h - l) / 1);
    }
    return li->line;
}


/* current instruction in 'CrClosure' ('Function') */
cr_sinline int currentpc(const CallFrame *cf) {
    cr_assert(cfisCript(cf));
    return cast_int(cf->pc - cfFn(cf)->code) - 1;
}


/* current line number */
cr_sinline int currentline(CallFrame *cf) {
    return crD_getfuncline(cfFn(cf), currentpc(cf));
}


static const char *findvararg(CallFrame *cf, SPtr *pos, int n) {
    cr_assert(n < 0);
    if (crclval(s2v(cf->func.p))->fn->isvararg) {
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
const char *crD_findlocal(cr_State *ts, CallFrame *cf, int n, SPtr *pos) {
    SPtr base = cf->func.p + 1;
    const char *name = NULL;
    if (cfisCript(cf)) {
        if (n < 0) /* vararg ? */
            return findvararg(cf, pos, n);
        else /* otherwise local variable */
            name = crF_getlocalname(cfFn(cf), n, currentpc(cf));
    }
    if (name == NULL) {
        SPtr limit = (cf == ts->cf) ? ts->sp.p : cf->next->func.p;
        if (limit - base >= n && n > 0) /* 'n' is in stack range ? */
            name = cfisCript(cf) ? "(auto)" : "(C auto)";
        else
            return NULL;
    }
    if (pos)
        *pos = base + (n - 1);
    return name;
}


/*
 * Sets 'frame' in 'cr_DebugInfo'.
 * Level defines which call stack 'CallFrame' to use.
 * If you wish for currently active 'CallFrame' then 'level'
 * should be 0.
 * If 'level' is invalid, this function returns 0.
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



/*
 * Sets 'name', 'type', 'nups', 'nparams', 'isvararg', 'defline',
 * 'deflastline' in 'cr_DebugInfo'.
 */
static void getfuncinfo(Closure *cl, cr_DebugInfo *di) {
    if (noCriptclosure(cl)) {
        di->nups = (cl == NULL ? 0 : cl->cc.nupvalues);
        di->defline = -1;
        di->deflastline = -1;
        di->nparams = 0;
        di->isvararg = 1;
        di->type = "C";
    } else {
        di->nups = cl->crc.nupvalues;
        const Function *fn = cl->crc.fn;;
        di->defline = fn->defline;
        di->deflastline = fn->deflastline;
        di->nparams = fn->arity;
        di->isvararg = fn->isvararg;
        di->type = (fn->defline == 0) ? "main" : "Cscript";
    }
}


/* sets 'source', 'srclen' and 'shortsrc' in 'cr_DebugInfo' */
static void getsrcinfo(Closure *cl, cr_DebugInfo *di) {
    if (noCriptclosure(cl)) {
        di->source = "[C]";
        di->srclen = SLL("[C]");
    } else {
        const Function *fn = cl->crc.fn;
        di->source = fn->source->bytes;
        di->srclen = fn->source->len;
    }
    crS_sourceid(di->shortsrc, di->source, di->srclen);
}



/*
 * Auxiliary to 'cr_getinfo', parses debug bit mask and
 * fills out the 'cr_DebugInfo' accordingly.
 * If any invalid bit/option is inside the 'dbmask' this
 * function returns 0, otherwise 1.
 */
static int getinfo(cr_State *ts, int dbmask, Closure *cl, CallFrame *cf,
                   cr_DebugInfo *di) {
    UNUSED(ts); // TODO: remove this after DW_FNPUSH
    int bit;
    for (bit = 2; dbmask > 0; bit++) {
        switch (bit) {
            case 2: /* DW_LINE */
                di->line = (cfisCript(cf) ? currentline(cf) : -1);
                break;
            case 3: /* DW_FNINFO */
                getfuncinfo(cl, di);
                break;
            case 4: /* DW_FNSRC */
                getsrcinfo(cl, di);
                break;
            case 5: /* DW_FNPUSH */
                // criptapi_pushval(ts, *di->frame->func);
                break;
            case 6: /* unused */
            case 7: /* unused */
                return 0;
            default:
                cr_unreachable();
        }
        dbmask >>= 1;
    }
    return 1;
}


/*
 * Fill out 'cr_DebugInfo' according to 'dbmask'.
 * Returns 0 if any of the bits in 'dbmask' are invalid.
 */
CR_API int cr_getinfo(cr_State *ts, int dbmask, cr_DebugInfo *di) {
    CallFrame *cf;
    Closure *cl;
    TValue *fn;
    int status = 1;
    cr_lock(ts);
    if (dbmask & CR_DBG_FNGET) { /* use function on top of the stack ? */
        cf = NULL;
        fn = s2v(ts->sp.p - 1);
        api_check(ts, ttisfn(fn), "expect function");
        ts->sp.p--;
    } else { /* use current function */
        cf = ts->cf;
        fn = s2v(cf->func.p);
        cr_assert(ttisfn(fn));
    }
    cl = (ttiscl(fn) ? clval(fn) : NULL);
    dbmask >>= 1; /* skip CR_DBGFNGET bit */
    status = getinfo(ts, dbmask, cl, cf, di);
    cr_unlock(ts);
    return status;
}


/* add usual debug information to 'msg' (source id and line) */
const char *crD_addinfo(cr_State *ts, const char *msg, const OString *src,
                        int line) {
    char buffer[CRI_MAXSRC];
    if (src) {
        crS_sourceid(buffer, src->bytes, src->len);
    } else {
        buffer[0] = '?';
        buffer[1] = '\0';
    }
    return crS_pushfstring(ts, "%s:%d: %s", buffer, line, msg);
}


/* generic runtime error */
cr_noret crD_runerror(cr_State *ts, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    const char *err = crS_pushvfstring(ts, fmt, ap);
    va_end(ap);
    if (cfisCript(ts->cf)) { /* in Cript function (closure) ? */
        crD_addinfo(ts, err, cfFn(ts->cf)->source, currentline(ts->cf));
        setobj2s(ts, ts->sp.p - 2, s2v(ts->sp.p - 1));
        ts->sp.p--;
    }
    crPR_throw(ts, CR_ERRRUNTIME);
}


/* global variable related error */
cr_noret crD_globalerror(cr_State *ts, const char *err, OString *name) {
    crD_runerror(ts, "%s global variable '%s'", err, getstrbytes(name));
}


/* operation on invalid type error */
cr_noret crD_typeerror(cr_State *ts, const TValue *v, const char *op) {
    crD_runerror(ts, "tried to %s a %s value", op, typename(ttypetag(v)));
}


cr_noret crD_typeerrormeta(cr_State *ts, const TValue *v1, const TValue *v2,
                           const char * mop) {
    crD_runerror(ts, "tried to %s %s and %s values",
                     mop, typename(ttypetag(v1)), typename(ttypetag(v2)));
}


/* arithmetic operation error */
cr_noret crD_operror(cr_State *ts, const TValue *v1, const TValue *v2,
                     const char *op) {
    if (ttisnum(v1))
        v1 = v2;  /* correct error value */
    crD_typeerror(ts, v1, op);
}


/* ordering error */
cr_noret crD_ordererror(cr_State *ts, const TValue *v1, const TValue *v2) {
    const char *name1 = typename(ttype(v1));
    const char *name2 = typename(ttype(v2));
    if (name1 == name2) /* point to same entry ? */
        crD_runerror(ts, "tried to compare two %s values", name1);
    else
        crD_runerror(ts, "tried to compare %s with %s", name1, name2);
}


cr_noret crD_callerror(cr_State *ts, const TValue *o) {
    crD_typeerror(ts, o, "call");
}
