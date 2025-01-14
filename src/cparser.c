/*
** cparser.c
** CScript Parser
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "ccode.h"
#include "csconf.h"
#include "cgc.h"
#include "clexer.h"
#include "climits.h"
#include "cobject.h"
#include "cparser.h"
#include "cstate.h"
#include "cobject.h"
#include "cstring.h"
#include "ctrace.h"
#include "cvm.h"
#include "cfunction.h"
#include "chashtable.h"
#include "cmem.h"

#include <string.h>


/*
** By default, disable bytecode disassembly.
*/
#if !defined(DISASSEMBLE_BYTECODE)
#define DISASSEMBLE_BYTECODE    1
#endif


/* check if current token matches 'tk' */
#define check(lx, tok)      ((lx)->t.tk == (tok))


#define enterCstack(lx)     csT_incCstack((lx)->ts)
#define leaveCstack(lx)     ((lx)->ts->nCcalls--)


/* expect 'cond' to be true or invoke error */
#define expect_cond(lx, cond, err) \
    { if (!(cond)) csY_syntaxerror(lx, err); }


/* get the last patch list */
#define gplist(lx) \
        check_exp((lx)->ps->patches.len > 0, \
                  &(lx)->ps->patches.arr[(lx)->ps->patches.len - 1])



/*
** Break jump used to indicate end of the patch list.
*/
static const BJmp dummyjmp = { NOJMP, 0, 0 };


/* check if 'BJmp' is a dummy :-) */
#define isdummy(jmp)        ((jmp) == &dummyjmp)



/* lexical scope information */
typedef struct Scope {
    struct Scope *prev; /* implicit linked-list */
    int nactlocals; /* number of locals outside of this scope */
    int nswscope; /* number of switch scopes before this scope */
    int depth;
    c_byte cf; /* control flow */
    c_byte haveupval; /* set if scope contains upvalue variable */
    c_byte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;


/* control flow masks */
#define CFML            1 /* loop */
#define CFMGL           2 /* generic loop */
#define CFMS            4 /* switch */
#define CFMASK          (CFML | CFMGL | CFMS)

#define is_loop(s)          testbits((s)->cf, CFML)
#define is_genloop(s)       testbits((s)->cf, CFMGL)
#define is_switch(s)        testbits((s)->cf, CFMS)
#define haspatchlist(s)     testbits((s)->cf, CFMASK)



static c_noret expecterror(Lexer *lx, int tk) {
    const char *err = csS_pushfstring(lx->ts, "expected %s",
                                            csY_tok2str(lx, tk));
    csY_syntaxerror(lx, err);
}


static c_noret limiterror(FunctionState *fs, const char *what, int limit) {
    cs_State *ts = fs->lx->ts;
    int line = fs->p->defline;
    const char *where = (line == 0 ? "main function" :
                        csS_pushfstring(ts, "function at line %d", line));
    const char *err = csS_pushfstring(ts, "too many %s (limit is %d) in %s",
                                          what, limit, where);
    csY_syntaxerror(fs->lx, err);
}


/* semantic error; variant of syntax error without 'near <token>' */
c_noret csP_semerror(Lexer *lx, const char *err) {
    lx->t.tk = 0;
    csY_syntaxerror(lx, err);
}


static void checklimit(FunctionState *fs, int n, int limit, const char *what) {
    if (n >= limit)
        limiterror(fs, what, limit);
}


static void rmpatchlists(Lexer *lx, int limit) {
    ParserState *ps = lx->ps;
    cs_assert(0 <= limit && limit <= ps->patches.len);
    while (limit < ps->patches.len) {
        PatchList *l = &ps->patches.arr[--ps->patches.len];
        csM_freearray(lx->ts, l->arr, l->size);
    }
}


static void storecontext(FunctionState *fs, FuncContext *ctx) {
    ParserState *ps = fs->lx->ps;
    ctx->loopstart = fs->loopstart;
    ctx->prevpc = fs->prevpc;
    ctx->prevline = fs->prevline;
    ctx->sp = fs->sp;
    ctx->nactlocals = fs->nactlocals;
    ctx->np = fs->np;
    ctx->nk = fs->nk;
    ctx->pc = currPC;
    ctx->nabslineinfo = fs->nabslineinfo;
    ctx->nlocals = fs->nlocals;
    ctx->nupvals = fs->nupvals;
    ctx->npatches = ps->patches.len;
    if (ctx->npatches > 0) /* have patch list */
        ctx->nbjmp = gplist(fs->lx)->len;
    ctx->npcdif = ps->pcdif.len;
    ctx->iwthabs = fs->iwthabs;
    ctx->needclose = fs->needclose;
    ctx->lastwasret = fs->lastwasret;
}


static void loadcontext(FunctionState *fs, FuncContext *ctx) {
    ParserState *ps = fs->lx->ps;
    fs->loopstart = ctx->loopstart;
    fs->prevpc = ctx->prevpc;
    fs->prevline = ctx->prevline;
    fs->sp = ctx->sp;
    fs->nactlocals = ctx->nactlocals;
    fs->np = ctx->np;
    fs->nk = ctx->nk;
    currPC = ctx->pc;
    fs->nabslineinfo = ctx->nabslineinfo;
    fs->nlocals = ctx->nlocals;
    fs->nupvals = ctx->nupvals;
    rmpatchlists(fs->lx, ctx->npatches); /* remove extra patch lists */
    cs_assert(ps->patches.len == ctx->npatches);
    if (ctx->npatches > 0)
        gplist(fs->lx)->len = ctx->nbjmp;
    ps->pcdif.len = ctx->npcdif;
    fs->iwthabs = ctx->iwthabs;
    fs->needclose = ctx->needclose;
    fs->lastwasret = ctx->lastwasret;
}


/* get local variable */
static LVar *getlocalvar(FunctionState *fs, int idx) {
    return &fs->lx->ps->actlocals.arr[fs->firstlocal + idx];
}


/* get local variable debug information */
static LVarInfo *getlocalinfo(FunctionState *fs, int vidx) {
    LVar *lv = check_exp(vidx <= fs->nactlocals, getlocalvar(fs, vidx));
    cs_assert(lv->s.pidx >= 0 && lv->s.pidx < fs->nlocals);
    return &fs->p->locals[lv->s.pidx];
}


/*
** Convert 'nvar', a compiler index level, to its corresponding
** stack level.
*/
static int stacklevel(FunctionState *fs, int nvar) {
    if (nvar-- > 0) /* have at least one variable? */
        return getlocalvar(fs, nvar)->s.sidx + 1;
    return 0; /* no variables on stack */
}


/*
** Return number of variables on the stack for the given
** function.
*/
static int nvarstack(FunctionState *fs) {
    return stacklevel(fs, fs->nactlocals);
}


/* pop last patch list */
static PatchList *patchlistpop(Lexer *lx) {
    cs_assert(lx->ps->patches.len > 0);
    return &lx->ps->patches.arr[--lx->ps->patches.len];
}


/* pop last pending jump from patch list */
static const BJmp *popbreakjmp(PatchList *l) {
    if (l->len > 0)
        return &l->arr[--l->len];
    else
        return &dummyjmp;
}


/* add break jump to patch list (for backpatching) */
static void addbreakjmp(Lexer *lx, PatchList *l, int jmp, int hasclose) {
    FunctionState *fs = lx->fs;
    BJmp bjmp;
    bjmp.jmp = jmp;
    bjmp.nactlocals = fs->nactlocals;
    bjmp.hasclose = hasclose;
    csM_growarray(lx->ts, l->arr, l->size, l->len, MAXINT, "breaks", BJmp);
    l->arr[l->len++] = bjmp;
}


/* create new patch list */
static void addpatchlist(Lexer *lx) {
    ParserState *ps = lx->ps;
    PatchList pl;
    // TODO: memory leak (patch list not properly freed?)
    pl.len = pl.size = 0; pl.arr = NULL;
    csM_growarray(lx->ts, ps->patches.arr, ps->patches.size, ps->patches.len,
                  MAXINT, "control flows", PatchList);
    ps->patches.arr[ps->patches.len++] = pl;
}


static void continuepop(FunctionState *fs) {
    int ncntl = (check_exp(fs->loopscope, is_genloop(fs->loopscope))
                 ? NSTATEVARS /* keep 'foreach' local control variables */
                 : 0); /* otherwise pop all locals */
    csC_pop(fs, fs->nactlocals - fs->loopscope->nactlocals - ncntl);
    /* pop switch values up to the loop scope */
    csC_pop(fs, fs->scope->nswscope - fs->loopscope->nswscope); 
}


/* 
** Remove local variables up to 'tolevel'.
*/
static void removelocals(FunctionState *fs, int tolevel) {
    printf("Removing %d locals\n", fs->nactlocals - tolevel);
    fs->lx->ps->actlocals.len -= (fs->nactlocals - tolevel);
    cs_assert(fs->lx->ps->actlocals.len >= 0);
    while (fs->nactlocals > tolevel) /* set debug information */
        getlocalinfo(fs, --fs->nactlocals)->endpc = currPC;
}


static int finishpatchlist(FunctionState *fs, int level) {
    PatchList *l = patchlistpop(fs->lx);
    const BJmp *bjmp = popbreakjmp(l);
    int hasclose = 0;
    if (!isdummy(bjmp)) { /* have break jump? */
        if (bjmp->jmp == fs->prevpc) { /* no offset break jump? */
            cs_assert(fs->p->code[fs->prevpc] == OP_BJMP);
            currPC -= getOpSize(OP_BJMP); /* adjust pc */
            bjmp = popbreakjmp(l); /* get the next break (if any) */
        }
        while (!isdummy(bjmp)) { /* (maybe removed last and only break) */
            Instruction *jmp = &fs->p->code[bjmp->jmp];
            int popn = bjmp->nactlocals - level;
            hasclose |= bjmp->hasclose;
            cs_assert(bjmp->nactlocals >= level);
            csC_patch(fs, bjmp->jmp, currPC);   /* fix jump offset... */
            SETARG_L(jmp, 1, popn);             /* ...and pop count */
            bjmp = popbreakjmp(l);
        }
    }
    cs_assert(l->len == 0); /* at this point it must be empty */
    printf("Freeing patch list of size %zu\n", l->size * sizeof(*l->arr));
    csM_freearray(fs->lx->ts, l->arr, l->size); /* free patch list memory */
    if (hasclose) csC_emitIL(fs, OP_CLOSE, nvarstack(fs));
    return hasclose;
}


/* move constant expression to value 'v' */
static void movexp2v(FunctionState *fs, ExpInfo *e, TValue *v) {
    switch (e->et) {
        case EXP_NIL: setnilval(v); break;
        case EXP_FALSE: setbfval(v); break;
        case EXP_TRUE: setbtval(v); break;
        case EXP_STRING: setstrval(cast(cs_State *, NULL), v, e->u.str); break;
        case EXP_INT: setival(v, e->u.i); break;
        case EXP_FLT: setfval(v, e->u.n); break;
        case EXP_K: *v = *getconstant(fs, e); break;
        default: cs_assert(0);
    }
}


/* init expression with generic information */
static void initexp(ExpInfo *e, expt et, int info) {
    e->t = e->f = NOJMP;
    e->et = et;
    e->u.info = info;
}


#define voidexp(e)      initexp(e, EXP_VOID, 0)


static void initstring(ExpInfo *e, OString *s) {
    e->f = e->t = NOJMP;
    e->et = EXP_STRING;
    e->u.str = s;
}


/* add local debug information into 'locals' */
static int registerlocal(Lexer *lx, FunctionState *fs, OString *name) {
    Proto *p = fs->p;
    int osz = p->sizelocals;
    csM_growarray(lx->ts, p->locals, p->sizelocals, fs->nlocals, MAXVARS,
                  "locals", LVarInfo);
    while (osz < p->sizelocals)
        p->locals[osz++].name = NULL;
    p->locals[fs->nlocals].name = name;
    p->locals[fs->nlocals].startpc = currPC;
    csG_objbarrier(lx->ts, p, name);
    return fs->nlocals++;
}


/*
** Adjust locals by increment 'nactlocals' and registering them
** inside 'locals'.
*/
static void adjustlocals(Lexer *lx, int nvars) {
    FunctionState *fs = lx->fs;
    int stacklevel = nvarstack(fs);
    for (int i = 0; i < nvars; nvars--) {
        int vidx = fs->nactlocals++;
        LVar *lvar = getlocalvar(fs, vidx);
        lvar->s.sidx = stacklevel++;
        lvar->s.pidx = registerlocal(lx, fs, lvar->s.name);
    }
}


/* start lexical scope */
static void enterscope(FunctionState *fs, Scope *s, int cf) {
    if (testbits(cf, CFMASK)) /* needs a patch list? */
        addpatchlist(fs->lx); /* 'break' jumps storage */
    if (fs->scope) { /* not a global scope? */
        s->nswscope = fs->scope->nswscope + is_switch(fs->scope);
        s->depth = fs->scope->depth + 1;
        s->havetbcvar = fs->scope->havetbcvar;
    } else { /* global scope */
        s->nswscope = 0;
        s->depth = 0;
        s->havetbcvar = 0;
    }
    s->nactlocals = fs->nactlocals;
    s->cf = cf;
    s->haveupval = 0;
    s->prev = fs->scope;
    fs->scope = s;
}


/* end lexical scope */
static void leavescope(FunctionState *fs) {
    Scope *s = fs->scope;
    int stklevel = stacklevel(fs, s->nactlocals);
    int nactlocals = fs->nactlocals;
    int popn = nactlocals - s->nactlocals + is_switch(s);
    int hasclose = 0;
    printf("LEAVING SCOPE {\n");
    removelocals(fs, s->nactlocals); /* remove scope locals */
    cs_assert(s->nactlocals == fs->nactlocals);
    if (haspatchlist(s)) /* have to fix break jumps? */
        hasclose = finishpatchlist(fs, nactlocals);
    if (s->prev) { /* not main function scope? */
        if (!hasclose && s->haveupval) /* still need to close? */
            csC_emitIL(fs, OP_CLOSE, stklevel);
        csC_pop(fs, popn); /* pop locals and switch expression (if any) */
    } else if (!fs->lastwasret) /* last scope missing 'return'? */
        csC_ret(fs, 0, 0); /* 'return;' */
    printf("}\n");
    fs->sp = stklevel; /* free scope stack slots */
    fs->scope = s->prev; /* go back to the previous scope (if any) */
}


/* 
** Mark scope where variable at idx 'vidx' was defined
** in order to emit close instruction before the scope
** gets closed.
*/
static void scopemarkupval(FunctionState *fs, int vidx) {
    Scope *s = fs->scope;
    while(s->nactlocals - 1 > vidx)
        s = s->prev;
    s->haveupval = 1;
    fs->needclose = 1;
}


/* 
** Mark current scope as scope that has a to-be-closed
** variable.
*/
static void scopemarkclose(FunctionState *fs) {
    Scope *s = fs->scope;
    s->haveupval = 1;
    s->havetbcvar = 1;
    fs->needclose = 1;
}


static void open_func(Lexer *lx, FunctionState *fs, Scope *s) {
    Proto *p = fs->p;
    cs_assert(p != NULL);
    fs->prev = lx->fs;
    fs->lx = lx;
    lx->fs = fs;
    fs->scope = fs->loopscope = fs->switchscope = NULL;
    fs->loopstart = NOJMP;
    currPC = fs->prevpc = 0;
    fs->prevline = p->defline;
    fs->sp = 0;
    fs->nactlocals = 0;
    fs->firstlocal = lx->ps->actlocals.len;
    fs->np = 0;
    fs->nk = 0;
    fs->nabslineinfo = 0;
    fs->nlocals = 0;
    fs->nupvals = 0;
    lx->ps->patches.len = lx->ps->patches.size = 0;
    lx->ps->patches.arr = NULL;
    fs->iwthabs = fs->needclose = fs->lastwasret = 0;
    p->source = lx->src;
    csG_objbarrier(lx->ts, p, p->source);
    p->maxstack = 2; /* stack slots 0/1 are always valid */
    enterscope(fs, s, 0); /* start top-level scope */
}


static void close_func(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Proto *p = fs->p;
    cs_State *ts = lx->ts;
    cs_assert(fs->scope && fs->scope->prev == NULL);
    leavescope(fs); /* end final scope */
    cs_assert(fs->scope == NULL);
    cs_assert(fs->sp == 0);
    csC_finish(fs); /* final code adjustments */
    /* shrink unused memory */
    csM_shrinkarray(ts, p->p, p->sizep, fs->np, Proto);
    csM_shrinkarray(ts, p->k, p->sizek, fs->nk, TValue);
    csM_shrinkarray(ts, p->code, p->sizecode, currPC, Instruction);
    csM_shrinkarray(ts, p->lineinfo, p->sizelineinfo, currPC, c_sbyte);
    csM_shrinkarray(ts, p->abslineinfo, p->sizeabslineinfo, fs->nabslineinfo,
                        AbsLineInfo);
    csM_shrinkarray(ts, p->locals, p->sizelocals, fs->nlocals, LVarInfo);
    csM_shrinkarray(ts, p->upvals, p->sizeupvals, fs->nupvals, UpValInfo);
    lx->fs = fs->prev; /* go back to enclosing function (if any) */
    csG_checkGC(ts); /* try to collect garbage memory */
#if DISASSEMBLE_BYTECODE
    csTR_disassemble(fs->lx->ts, fs->p);
#endif
}


/* add function prototype */
static Proto *addproto(Lexer *lx) {
    cs_State *ts = lx->ts;
    FunctionState *fs = lx->fs;
    Proto *p = fs->p;
    Proto *clp; /* closure prototype */
    if (fs->np >= p->sizep) {
        int osz = p->sizep;
        csM_growarray(ts, p->p, p->sizep, fs->np, MAX_LARG, "functions",
                      Proto *);
        while (osz < p->sizep)
            p->p[osz++] = NULL;
    }
    p->p[fs->np++] = clp = csF_newproto(ts);
    csG_objbarrier(ts, p, clp);
    return clp;
}


/* set current function as vararg */
static void setvararg(FunctionState *fs, int arity) {
    fs->p->isvararg = 1;
    csC_emitIL(fs, OP_VARARGPREP, arity);
}



/* forward declare (can be both part of statement and expression) */
static void funcbody(Lexer *lx, ExpInfo *v, int linenum, int ismethod);

/* forward declare recursive non-terminals */
static void decl(Lexer *lx);
static void stm(Lexer *lx);
static void expr(Lexer *lx, ExpInfo *e);



/* 
** Advance scanner if 'tk' matches the current token,
** otherwise return 0. 
*/
static int match(Lexer *lx, int tk) {
    if (check(lx, tk)) {
        csY_scan(lx);
        return 1;
    }
    return 0;
}


/* check if 'tk' matches the current token if not invoke error */
static void expect(Lexer *lx, int tk) {
    if (c_unlikely(!check(lx, tk)))
        expecterror(lx, tk);
}


/* same as 'expect' but this also advances the scanner */
static void expectnext(Lexer *lx, int tk) {
    expect(lx, tk);
    csY_scan(lx);
}


/*
** Check that next token is 'what'. 
** Otherwise raise an error that the expected 'what' should 
** match a 'who' in line 'linenum'.
*/
static void expectmatch(Lexer *lx, int what, int who, int linenum) {
    if (c_unlikely(!match(lx, what))) {
        if (lx->line == linenum) /* same line? */
            expecterror(lx, what); /* emit usual error message */
        else /* otherwise spans across multiple lines */
            csY_syntaxerror(lx, csS_pushfstring(lx->ts,
                    "%s expected (to close %s at line %d)",
                    csY_tok2str(lx, what), csY_tok2str(lx, who), linenum));
    }
}


static OString *str_expectname(Lexer *lx) {
    OString *s;
    expect(lx, TK_NAME);
    s = lx->t.lit.str;
    csY_scan(lx);
    return s;
}


/* adds local variable to the 'actlocals' */
static int addlocal(Lexer *lx, OString *name) {
    FunctionState *fs = lx->fs;
    ParserState *ps = lx->ps;
    LVar *local;
    checklimit(fs, ps->actlocals.len + 1 - fs->firstlocal, MAXVARS, "locals");
    csM_growarray(lx->ts, ps->actlocals.arr, ps->actlocals.size,
                  ps->actlocals.len, MAXINT, "locals", LVar);
    local = &ps->actlocals.arr[ps->actlocals.len++];
    local->s.kind = VARREG;
    local->s.name = name;
    local->s.pidx = -1;
    return ps->actlocals.len - fs->firstlocal - 1;
}


#define addlocallit(lx,lit) \
        addlocal(lx, csY_newstring(lx, "" lit, SLL(lit)))


/*
** Searches for local variable 'name'.
*/
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e, int limit) {
    for (int i = fs->nactlocals - 1; 0 <= i && limit < i; i--) {
        LVar *lvar = getlocalvar(fs, i);
        if (eqstr(name, lvar->s.name)) { /* found? */
            if (c_unlikely(lvar->s.pidx == -1)) { /* uninitialized? */
                const char *msg = csS_pushfstring(fs->lx->ts, 
                    "can't read local variable '%s' in its own initializer",
                    getstr(name));
                csP_semerror(fs->lx, msg);
            } else {
                initexp(e, EXP_LOCAL, lvar->s.sidx);
                return e->et;
            }
        }
    }
    return -1; /* not found */
}


/* allocate space for new 'UpValInfo' */
static UpValInfo *newupvalue(FunctionState *fs) {
    Proto *p = fs->p;
    cs_State *ts = fs->lx->ts;
    int osz = p->sizeupvals;
    checklimit(fs, fs->nupvals + 1, MAXUPVAL, "upvalues");
    csM_growarray(ts, p->upvals, p->sizeupvals, fs->nupvals, MAXUPVAL,
                  "upvalues", UpValInfo);
    while (osz < p->sizeupvals)
        p->upvals[osz++].name = NULL;
    return &p->upvals[fs->nupvals++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *e) {
    UpValInfo *uv = newupvalue(fs);
    FunctionState *prev = fs->prev;
    if (e->et == EXP_LOCAL) { /* local? */
        uv->onstack = 1;
        uv->idx = e->u.info;
        uv->kind = getlocalvar(prev, e->u.info)->s.kind;
        cs_assert(eqstr(name, getlocalvar(prev, e->u.info)->s.name));
    } else { /* must be upvalue */
        cs_assert(e->et == EXP_UVAL);
        uv->onstack = 0;
        uv->idx = e->u.info;
        uv->kind = prev->p->upvals[e->u.info].kind;
        cs_assert(eqstr(name, prev->p->upvals[e->u.info].name));
    }
    uv->name = name;
    csG_objbarrier(fs->lx->ts, fs->p, name);
    return fs->nupvals - 1;
}


/* searches for upvalue 'name' */
static int searchupvalue(FunctionState *fs, OString *name) {
    Proto *fn = fs->p;
    for (int i = 0; i < fs->nupvals; i++)
        if (eqstr(fn->upvals[i].name, name)) 
            return i;
    return -1;
}


/*
** Find a variable with the given name. If it is upvalue add this upvalue
** into all intermediate functions. If it is not found, set 'var' as EXP_VOID.
*/
static void varaux(FunctionState *fs, OString *name, ExpInfo *var, int base) {
    if (fs == NULL) { /* last scope? */
        voidexp(var); /* not found */
    } else { /* otherwise search... */
        int ret = searchlocal(fs, name, var, -1); /* ...locals */
        if (ret >= 0) { /* found? */
            if (ret == EXP_LOCAL && !base) /* in recursive call to varaux? */
                scopemarkupval(fs, var->u.info); /* mark scope appropriately */
        } else { /* if not found try searching for upvalue */
            ret = searchupvalue(fs, name);
            if (ret < 0) { /* still not found? */
                varaux(fs->prev, name, var, 0); /* try enclosing 'fs' */
                if (var->et == EXP_LOCAL || var->et == EXP_UVAL) /* found? */
                    ret = addupvalue(fs, name, var); /* add upvalue to 'fs' */
                else /* otherwise not found (EXP_VOID) */
                    return;
            }
            initexp(var, EXP_UVAL, ret);
        }
    }
}


#include <stdio.h>
/* find variable 'name' */
static void var(Lexer *lx, OString *varname, ExpInfo *var) {
    FunctionState *fs = lx->fs;
    varaux(fs, varname, var, 1);
    if (var->et == EXP_VOID) { /* global name? */
        var->et = EXP_GLOBAL;
        var->u.str = varname;
    }
}


#define varlit(lx,l,e)      var(lx, csY_newstring(lx, "" l, SLL(l)), e)



/* -------------------------------------------------------------------------
 *                              EXPRESSIONS
 * ------------------------------------------------------------------------- */

static void expname(Lexer *lx, ExpInfo *e) {
    initstring(e, str_expectname(lx));
}


/*
** explist ::= expr
**           | expr ',' explist
*/
static int explist(Lexer *lx, ExpInfo *e) {
    int n = 1;
    expr(lx, e);
    while (match(lx, ',')) {
        csC_exp2stack(lx->fs, e);
        expr(lx, e);
        n++;
    }
    return n;
}


/* indexed ::= '[' expr ']' */
static void indexed(Lexer *lx, ExpInfo *var, int super) {
    ExpInfo key;
    voidexp(&key);
    csY_scan(lx); /* skip '[' */
    csC_exp2stack(lx->fs, var);
    expr(lx, &key);
    csC_indexed(lx->fs, var, &key, super);
    expectnext(lx, ']');
}


/* getfield ::= '.' name */
static void getfield(Lexer *lx, ExpInfo *var, int super) {
    ExpInfo key;
    voidexp(&key);
    csC_exp2stack(lx->fs, var);
    csY_scan(lx); /* skip '.' */
    expname(lx, &key);
    csC_getfield(lx->fs, var, &key, super);
}


/* 
** superkw ::= 'super' '.' name 
**           | 'super' '[' name ']'
**           | 'super' '[' string ']' 
*/
static void superkw(Lexer *lx, ExpInfo *e) {
    if (c_unlikely(lx->ps->cs == NULL))
        csP_semerror(lx, "usage of 'super' outside of method");
    else if (c_unlikely(!lx->ps->cs->super))
        csY_syntaxerror(lx, "use of 'super' but class does not inherit");
    else {
        FunctionState *fs = lx->fs;
        csY_scan(lx); /* skip 'super' */
        varlit(lx, "self", e); /* get class... */
        cs_assert(e->et == EXP_LOCAL); /* which must be local... */
        csC_exp2stack(fs, e); /* ...and put it on stack */
        varlit(lx, "super", e); /* get superclass */
        if (check(lx, '[')) /* index access? */
            indexed(lx, e, 1);
        else if (check(lx, '.')) /* field access? */
            getfield(lx, e, 1);
        else 
            csY_syntaxerror(lx, "'.' or '[' expected");
    }
}


/*
** primaryexp ::= '(' expr ')'
**              | name
**              | superkw
*/
static void primaryexp(Lexer *lx, ExpInfo *e) {
    switch (lx->t.tk) {
    case '(':
        csY_scan(lx); /* skip ')' */
        expr(lx, e);
        expectnext(lx, ')');
        csC_varexp2stack(lx->fs, e);
        break;
    case TK_NAME:
        var(lx, str_expectname(lx), e);
        break;
    case TK_SUPER:
        superkw(lx, e);
        break;
    default:
        csY_syntaxerror(lx, "unexpected symbol");
        break;
    }
}


/* 
** call ::= '(' ')'
**        | '(' explist ')' 
*/
static void call(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int line = lx->line;
    int base;
    csC_exp2stack(fs, e); /* put func on stack */
    base = fs->sp - 1; /* func */
    csY_scan(lx); /* skip '(' */
    if (!check(lx, ')')) { /* have args ? */
        explist(lx, e);
        if (eismulret(e))
            csC_setmulret(fs, e);
        else
            csC_exp2stack(fs, e);
    } else
        e->et = EXP_VOID;
    expectnext(lx, ')');
    initexp(e, EXP_CALL, csC_emitILL(fs, OP_CALL, base, 2));
    csC_fixline(fs, line);
    fs->sp = base + 1; /* call removes function and arguments and leaves
                          one result (unless changed later) */
}


/*
** suffixedexpr ::= primaryexp
**               | primaryexp dotaccess
**               | primaryexp call
**               | primaryexp indexed
*/
static void suffixedexp(Lexer *lx, ExpInfo *e) {
    primaryexp(lx, e);
    for (;;) {
        switch (lx->t.tk) {
            case '.': {
                getfield(lx, e, 0);
                break;
            }
            case '[': {
                indexed(lx, e, 0);
                break;
            }
            case '(': {
                call(lx, e);
                break;
            }
            default: return;
        }
    }
}


/* array and table constructor */
typedef struct Constructor {
    union {
        struct {
            ExpInfo v; /* last array item read */
            int na; /* number of array elements already stored */
            int tostore; /* number of array elements pending to be stored */
        } a; /* array */
        struct {
            ExpInfo *t; /* table descriptor */
            int nh; /* total number of table elements */
        } t; /* table */
    } u;
} Constructor;


/* arrfield ::= expr */
static void arrfield(Lexer *lx, Constructor *c) {
    expr(lx, &c->u.a.v);
    c->u.a.tostore++;
}


static void closearrfield(FunctionState *fs, Constructor *c) {
    if (c->u.a.v.et == EXP_VOID) return; /* there is no array item */
    csC_exp2stack(fs, &c->u.a.v); /* put the item on stack */
    c->u.a.v.et = EXP_VOID; /* now empty */
    if (c->u.a.tostore == ARRFIELDS_PER_FLUSH) { /* flush? */
        csC_setarray(fs, c->u.a.na, c->u.a.tostore);
        c->u.a.na += c->u.a.tostore; /* add to total */
        c->u.a.tostore = 0; /* no more pending items */
    }
}


static void lastarrfield(FunctionState *fs, Constructor *c) {
    if (c->u.a.tostore == 0) return;
    if (eismulret(&c->u.a.v)) { /* last item has multiple returns? */
        csC_setmulret(fs, &c->u.a.v);
        csC_setarray(fs, c->u.a.na, CS_MULRET);
        c->u.a.na--; /* do not count last expression (unknown num of elems) */
    } else {
        if (c->u.a.v.et != EXP_VOID) /* have item? */
            csC_exp2stack(fs, &c->u.a.v); /* ensure it is on stack */
        csC_setarray(fs, c->u.a.na, c->u.a.tostore);
    }
    c->u.a.na += c->u.a.tostore;
}


/*
** arrayexp ::= '[' [ arrfield [sep] ] ']'
** sep ::= ',' | ';'
*/
static void arrayexp(Lexer *lx, ExpInfo *a) {
    FunctionState *fs = lx->fs;
    int matchline = lx->line;
    int pc = csC_emitIS(fs, OP_NEWARRAY, 0);
    Constructor c;
    c.u.a.na = c.u.a.tostore = 0;
    initexp(a, EXP_FINEXPR, pc); /* finalize array expression */
    csC_reserveslots(fs, 1); /* space for array */
    voidexp(&c.u.a.v); /* no value (yet) */
    expectnext(lx, '[');
    do {
        cs_assert(c.u.a.v.et == EXP_VOID || c.u.a.tostore > 0);
        if (check(lx, ']')) break; /* delimiter; no more elements */
        closearrfield(fs, &c); /* try to close any pending array elements */
        arrfield(lx, &c); /* get array element */
    } while (match(lx, ',') || match(lx, ';'));
    expectmatch(lx, ']', '[', matchline);
    lastarrfield(fs, &c);
    csC_setarraysize(fs, pc, c.u.a.na);
}


/* tabindex ::= '[' expr ']' */
static void tabindex(Lexer *lx, ExpInfo *e) {
    expectnext(lx, '[');
    expr(lx, e);
    csC_varexp2stack(lx->fs, e);
    expectnext(lx, ']');
}


/*
** tabfield ::= name '=' expr
**            | tabindex '=' expr
*/
static void tabfield(Lexer *lx, Constructor *c) {
    FunctionState *fs = lx->fs;
    int sp = fs->sp;
    int extra;
    ExpInfo tab, key, val;
    UNUSED(sp); /* used only for assertion */
    if (check(lx, TK_NAME)) {
        checklimit(fs, c->u.t.nh, MAXINT, "records in a table constructor");
        expname(lx, &key);
    } else
        tabindex(lx, &key);
    c->u.t.nh++;
    expectnext(lx, '=');
    tab = *c->u.t.t;
    csC_indexed(fs, &tab, &key, 0); /* ensure key is in proper place... */
    expr(lx, &val); /* get key value... */
    csC_exp2stack(fs, &val); /* put it on stack... */
    extra = csC_store(fs, &tab); /* ...and do the store */
    csC_pop(fs, extra); /* pop potential key value */
    cs_assert(fs->sp == sp);
}


/*
** tableexp ::= '{' [ tabfield { sep tabfield } [sep] ] '}'
** sep ::= ',' | ';'
*/
static void tableexp(Lexer *lx, ExpInfo *t) {
    FunctionState *fs = lx->fs;
    int matchline = lx->line;
    int pc = csC_emitIS(fs, OP_NEWTABLE, 0);
    Constructor c;
    c.u.t.nh = 0;
    c.u.t.t = t;
    expectnext(lx, '{');
    initexp(t, EXP_FINEXPR, pc); /* finalize table expression */
    csC_reserveslots(fs, 1); /* space for table */
    do { /* while have table fields */
        if (check(lx, '}')) break; /* delimiter; no more field */
        tabfield(lx, &c);
    } while (match(lx, ',') || match(lx, ';'));
    expectmatch(lx, '}', '{', matchline);
    csC_settablesize(fs, pc, c.u.t.nh);
}


/*
** simpleexp ::= int
**              | flt
**              | string
**              | nil
**              | true
**              | false
**              | '...'
**              | arrayexp
**              | tableexp
**              | functionexp
**              | suffixedexp
*/
static void simpleexp(Lexer *lx, ExpInfo *e) {
    switch (lx->t.tk) {
        case TK_INT: {
            initexp(e, EXP_INT, 0);
            e->u.i = lx->t.lit.i;
            break;
        }
        case TK_FLT: {
            initexp(e, EXP_FLT, 0);
            e->u.n = lx->t.lit.n;
            break;
        }
        case TK_STRING: {
            initexp(e, EXP_STRING, 0);
            e->u.str = lx->t.lit.str;
            break;
        }
        case TK_NIL: {
            initexp(e, EXP_NIL, 0);
            break;
        }
        case TK_TRUE: {
            initexp(e, EXP_TRUE, 0);
            break;
        }
        case TK_FALSE: {
            initexp(e, EXP_FALSE, 0);
            break;
        }
        case TK_DOTS: {
            expect_cond(lx, lx->fs->p->isvararg,
                        "cannot use '...' outside of vararg function");
            initexp(e, EXP_VARARG, csC_emitIL(lx->fs, OP_VARARG, 2));
            break;
        }
        case '[': {
            arrayexp(lx, e);
            return;
        }
        case '{': {
            tableexp(lx, e);
            return;
        }
        case TK_FN: { /* functionexp */
            csY_scan(lx); /* skip 'fn' */
            funcbody(lx, e, lx->line, 0);
            return;
        }
        default: {
            suffixedexp(lx, e);
            return;
        }
    }
    csY_scan(lx);
}


/* get unary operation matching 'token' */
static Unopr getunopr(int token) {
    switch (token) {
        case '-': return OPR_UNM;
        case '~': return OPR_BNOT;
        case '!': return OPR_NOT;
        default: return OPR_NOUNOPR;
    }
}


/* get binary operation matching 'token' */
static Binopr getbinopr(int token) {
    switch (token) {
        case '+': return OPR_ADD;
        case '-': return OPR_SUB;
        case '*': return OPR_MUL;
        case '/': return OPR_DIV;
        case '%': return OPR_MOD;
        case TK_POW: return OPR_POW;
        case TK_SHR: return OPR_SHR;
        case TK_SHL: return OPR_SHL;
        case '&': return OPR_BAND;
        case '|': return OPR_BOR;
        case '^': return OPR_BXOR;
        case TK_CONCAT: return OPR_CONCAT;
        case TK_NE: return OPR_NE;
        case TK_EQ: return OPR_EQ;
        case '<': return OPR_LT;
        case TK_LE: return OPR_LE;
        case '>': return OPR_GT;
        case TK_GE: return OPR_GE;
        case TK_AND: return OPR_AND;
        case TK_OR: return OPR_OR;
        default: return OPR_NOBINOPR;
    }
}


/*
** If 'left' == 'right' then operator is associative;
** if 'left' < 'right' then operator is left associative;
** if 'left' > 'right' then operator is right associative.
*/
static const struct {
    c_byte left;
    c_byte right;
} priority[] = { /* ORDER OPR */
    /* binary operators priority */
    {12, 12}, {12, 12},             /* '+' '-' */
    {13, 13}, {13, 13}, {13, 13},   /* '*' '/' '%' */
    {16, 15},                       /* '**' (right associative) */
    {9, 9}, {9, 9},                 /* '<<' '>>' */
    {6, 6}, {4, 4}, {5, 5},         /* '&' '|' '^' */
    {11, 10},                       /* '..' (right associative) */
    {7, 7}, {7, 7},                 /* '==' '!=' */
    {8, 8}, {8, 8},                 /* '<' '<= */
    {8, 8}, {8, 8},                 /* '>' '>= */
    {3, 3}, {2, 2},                 /* 'and' 'or' */
    {1, 1}                          /* TODO: '?:' (ternary) */
};

#define UNARY_PRIORITY  14  /* priority for unary operators */


/*
** subexpr ::= simpleexp
**           | '-' simpleexp
**           | '!' simpleexp
**           | '~' simpleexp
**           | simpleexp '+' subexpr
**           | simpleexp '-' subexpr
**           | simpleexp '*' subexpr
**           | simpleexp '/' subexpr
**           | simpleexp '%' subexpr
**           | simpleexp '**' subexpr
**           | simpleexp '>>' subexpr
**           | simpleexp '<<' subexpr
**           | simpleexp '==' subexpr
**           | simpleexp '<' subexpr
**           | simpleexp '<=' subexpr
**           | simpleexp '>' subexpr
**           | simpleexp '>=' subexpr
**           | simpleexp '&' subexpr
**           | simpleexp '^' subexpr
**           | simpleexp '|' subexpr
**           | simpleexp 'and' subexpr
**           | simpleexp 'or' subexpr
**           | simpleexp '..' subexpr
*/
static Binopr subexpr(Lexer *lx, ExpInfo *e, int limit) {
    Binopr opr;
    Unopr uopr;
    enterCstack(lx);
    uopr = getunopr(lx->t.tk);
    if (uopr != OPR_NOUNOPR) {
        csY_scan(lx); /* skip operator */
        subexpr(lx, e, UNARY_PRIORITY);
        csC_unary(lx->fs, e, uopr);
    } else {
        simpleexp(lx, e);
    }
    opr = getbinopr(lx->t.tk);
    while (opr != OPR_NOBINOPR && priority[opr].left > limit) {
        ExpInfo e2;
        Binopr next;
        int line = lx->line;
        voidexp(&e2);
        csY_scan(lx); /* skip operator */
        csC_prebinary(lx->fs, e, opr);
        next = subexpr(lx, &e2, priority[opr].right);
        csC_binary(lx->fs, e, &e2, opr, line);
        opr = next;
    }
    leaveCstack(lx);
    return opr;
}


/* expr ::= subexpr */
static void expr(Lexer *lx, ExpInfo *e) {
    subexpr(lx, e, 0);
}



/* ----------------------------------------------------------------------
**                              STATEMENTS
** ---------------------------------------------------------------------- */

/* 
** decl_list ::= decl
**             | decl decl_list
**             | decl_list returnstm
**             | decl_list continuestm
**             | decl_list breakstm
*/
static void decl_list(Lexer *lx, int blocktk) {
    while (!check(lx, TK_EOS) && !(blocktk && check(lx, blocktk))) {
        if (check(lx, TK_RETURN) ||     /* if returnstm... */
            check(lx, TK_CONTINUE) ||   /* or continuestm... */
            check(lx, TK_BREAK)) {      /* ...or breakstm? */
            stm(lx);                    /* then it must be the last statement */
            return;                     /* done */
        } else                          /* otherwise it is a declaration */
            decl(lx);
    }
}


/* check if 'var' is 'final' (read-only) */
static void checkreadonly(Lexer *lx, ExpInfo *var) {
    FunctionState *fs = lx->fs;
    OString *varid = NULL;
    switch (var->et) {
    case EXP_UVAL: {
        UpValInfo *uv = &fs->p->upvals[var->u.info];
        if (uv->kind == VARFINAL)
            varid = uv->name;
        break;
    }
    case EXP_LOCAL: {
        LVar *lv = getlocalvar(fs, var->u.info);
        if (lv->s.kind == VARFINAL)
            varid = lv->s.name;
        break;
    }
    default: return; /* cannot be read-only */
    }
    if (varid) {
        const char *msg = csS_pushfstring(lx->ts,
            "attempt to assign to a read-only variable '%s'", getstr(varid));
        csP_semerror(lx, msg);
    }
}


/* adjust left and right side of an assignment */
static void adjustassign(Lexer *lx, int nvars, int nexps, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int need = nvars - nexps;
    if (eismulret(e)) {
        int extra = need + 1;
        if (extra < 0)
            extra = 0;
        csC_setreturns(fs, e, extra);
    } else {
        if (e->et != EXP_VOID)
            csC_exp2stack(fs, e);
        if (need > 0) { /* missing values? */
            csC_nil(fs, need);
            return; /* done */
        } /* else fall through */
    }
    if (need > 0) /* need to reserve stack slots? */
        csC_reserveslots(fs, need);
    else /* otherwise 'need' is negative or zero */
        csC_pop(fs, -need); /* pop extra values if any */
}


/*
** Structure to chain all variables on the left side of the
** assignment.
*/
struct LHS_assign {
    struct LHS_assign *prev;
    ExpInfo v;
};


/*
** assign ::= vars '=' explist
**          | 
** vars ::= var
**        | var ',' vars
*/
static int assign(Lexer *lx, struct LHS_assign *lhs, int nvars) {
    int left = 0; /* number of values left in the stack after assignment */
    expect_cond(lx, eisvar(&lhs->v), "expect variable");
    checkreadonly(lx, &lhs->v);
    if (match(lx, ',')) { /* more vars ? */
        struct LHS_assign var;
        var.prev = lhs; /* chain previous variable */
        suffixedexp(lx, &var.v); /* get the next variable in the list... */
        enterCstack(lx);
        left = assign(lx, &var, nvars + 1); /* ...and assign it */
        leaveCstack(lx);
    } else { /* right side of assignment '=' */
        ExpInfo e;
        int nexps;
        expectnext(lx, '=');
        nexps = explist(lx, &e);
        if (nexps != nvars)
            adjustassign(lx, nvars, nexps, &e);
        else
            csC_exp2stack(lx->fs, &e);
    }
    if (eisindexed(&lhs->v)) {
        int extra = csC_storevar(lx->fs, &lhs->v, left+nvars-1);
        left += 1 + extra; /* variable and extra (key value) */
    } else /* no leftover values */
        csC_store(lx->fs, &lhs->v);
    return left;
}


/*
** exprstm ::= call
**           | assign
*/
static void exprstm(Lexer *lx) {
    struct LHS_assign v;
    suffixedexp(lx, &v.v);
    if (check(lx, '=') || check(lx, ',')) { /* assignment? */
        int left;
        v.prev = NULL;
        left = assign(lx, &v, 1);
        csC_adjuststack(lx->fs, left);
    } else { /* otherwise must be call */
        FunctionState *fs = lx->fs;
        Instruction *inst;
        expect_cond(lx, v.v.et == EXP_CALL, "syntax error");
        inst = getinstruction(fs, &v.v);
        SETARG_L(inst, 1, 1); /* call statement uses no results... */
    }
}


static int getlocalattribute(Lexer *lx) {
    if (match(lx, '<')) {
        const char *attr = getstr(str_expectname(lx));
        expectnext(lx, '>');
        if (strcmp(attr, "final") == 0)
            return VARFINAL; /* read-only variable */
        else if (strcmp(attr, "close") == 0)
            return VARTBC; /* to-be-closed variable */
        else
            csP_semerror(lx,
                csS_pushfstring(lx->ts, "unknown attribute '%s'", attr));
    }
    return VARREG;
}


static int newlocalvar(Lexer *lx, OString *name) {
    int limit = lx->fs->scope->nactlocals - 1;
    ExpInfo dummy;
    if (c_unlikely(searchlocal(lx->fs, name, &dummy, limit) >= 0))
        csP_semerror(lx, csS_pushfstring(lx->ts,
                     "redefinition of local variable '%s'", getstr(name)));
    return addlocal(lx, name);
}


static void checkclose(FunctionState *fs, int level) {
    if (level != -1) {
        scopemarkclose(fs);
        csC_emitIL(fs, OP_TBC, level);
    }
}


/* 
** localstm ::= 'local' idlist ';'
**            | 'local' idlist '=' explist ';'
*/
static void localstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int toclose = -1;
    int nvars = 0;
    int kind, vidx;
    int nexps;
    ExpInfo e;
    voidexp(&e);
    do {
        vidx = newlocalvar(lx, str_expectname(lx)); /* create new local... */
        kind = getlocalattribute(lx); /* get its attribute... */
        getlocalvar(fs, vidx)->s.kind = kind; /* ...and set it */
        if (kind & VARTBC) { /* to-be-closed? */
            if (toclose != -1) /* one already present? */
                csP_semerror(fs->lx,
                        "multiple to-be-closed variables in a local list");
            toclose = fs->nactlocals + nvars;
        }
        nvars++;
    } while (match(lx, ','));
    if (match(lx, '='))
        nexps = explist(lx, &e);
    else
        nexps = 0;
    cs_assert((nexps == 0) == (e.et == EXP_VOID));
    adjustassign(lx, nvars, nexps, &e);
    adjustlocals(lx, nvars);
    checkclose(fs, toclose);
    expectnext(lx, ';');
}


/*
** localfn ::= 'local' 'fn' name funcbody
*/
static void localfn(Lexer *lx) {
    ExpInfo e;
    FunctionState *fs = lx->fs;
    int fvar = fs->nactlocals; /* function's variable index */
    newlocalvar(lx, str_expectname(lx)); /* create new local... */
    adjustlocals(lx, 1); /* ...and register it */
    funcbody(lx, &e, lx->line, 0);
    /* debug information will only see the variable after this point! */
    getlocalinfo(fs, fvar)->startpc = currPC;
}


/* inherit from superclass */
static void codeinherit(Lexer *lx, OString *name, Scope *s) {
    FunctionState *fs = lx->fs;
    OString *supname = str_expectname(lx);
    ExpInfo e;
    voidexp(&e);
    if (c_unlikely(eqstr(name, supname))) /* name collision? */
        csP_semerror(lx, csS_pushfstring(lx->ts,
                     "variable '%s' attempted to inherit itself", name));
    enterscope(fs, s, 0); /* start scope for superclass */
    addlocallit(lx, "super"); /* create 'super' local... */
    adjustlocals(lx, 1); /* ...and register it */
    var(lx, supname, &e); /* get superclass value... */
    csC_exp2stack(fs, &e); /* ...and put it on stack */
    var(lx, name, &e); /* get the class (to set its methods and inherit)... */
    csC_varexp2stack(fs, &e); /* ...and put it on stack */
    csC_emitI(fs, OP_INHERIT); /* do the inherit */
    lx->ps->cs->super = 1; /* indicate class inherited (scope was started) */
}


static void startcs(FunctionState *fs, ClassState *cs) {
    ParserState *ps = fs->lx->ps;
    cs->prev = ps->cs;
    cs->super = 0;
    ps->cs = cs;
}


/* end current 'ClassState' */
static void endcs(FunctionState *fs) {
    ParserState *ps = fs->lx->ps;
    cs_assert(ps->cs != NULL);
    if (ps->cs->super) { /* scope was created? */
        cs_assert(fs->scope->prev != NULL); /* at least 2 scopes */
        csC_pop(fs, 1); /* pop duplicate class value */
        leavescope(fs); /* end 'super' scope */
    }
    ps->cs = ps->cs->prev;
}


static int codemethod(FunctionState *fs, ExpInfo *var) {
    int ismethod = 1;
    cs_assert(var->et == EXP_STRING);
    if (ismetatag(var->u.str)) { /* metamethod? */
        csC_emitIS(fs, OP_SETMM, var->u.str->extra - NUM_KEYWORDS - 1);
        ismethod = 0; /* (this function goes in VMT) */
    } else /* otherwise have methods hashtable entry */
        csC_method(fs, var);
    fs->sp--; /* function is removed from stack */
    return ismethod; 
}


/* method ::= fn name funcbody */
static int method(Lexer *lx) {
    ExpInfo var, dummy;
    int defline = lx->line; /* method definition start line */
    expectnext(lx, TK_FN);
    expname(lx, &var);
    funcbody(lx, &dummy, defline, 1);
    return codemethod(lx->fs, &var);
}


/* 
** methods ::= method
**           | method methods
**           | empty
*/
static int methods(Lexer *lx) {
    int i = 0;
    while (!check(lx, '}') && !check(lx, TK_EOS))
        i += method(lx);
    return i;
}


/* 
** klass ::= '{' '}'
**         | '{' methods '}'
**         | 'inherits' name '{' '}'
**         | 'inherits' name '{' methods '}'
*/
static void klass(Lexer *lx, FunctionState *fs, OString *name) {
    ClassState cs;
    Scope s;
    int matchline, pc, nm;
    startcs(fs, &cs); /* start class state */
    pc = csC_emitIS(fs, OP_NEWCLASS, 0);
    csC_reserveslots(fs, 1); /* space for class */
    if (match(lx, TK_INHERITS)) /* class object inherits? */
        codeinherit(lx, name, &s);
    matchline = lx->line;
    expectnext(lx, '{');
    nm = methods(lx);
    if (nm > 0) {
        nm += (nm == 1); /* avoid 0 edge case in 'csO_ceillog' */
        SETARG_S(&fs->p->code[pc], 0, csO_ceillog2(nm));
    }
    expectmatch(lx, '}', '{', matchline);
    endcs(fs); /* end class state */
}


/*
** localclass ::= 'local' 'class' name klass
*/
static void localclass(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int cvar = fs->nactlocals; /* class variable index */
    OString *name;
    name = str_expectname(lx);
    newlocalvar(lx, name); /* create new local... */
    adjustlocals(lx, 1); /* ...and register it */
    klass(lx, fs, name);
    /* debug information will only see the variable after this point! */
    getlocalinfo(fs, cvar)->startpc = currPC;
}


/* blockstm ::= '{' stmlist '}' */
static void blockstm(Lexer *lx) {
    int matchline = lx->line;
    Scope s;
    csY_scan(lx); /* skip '{' */
    enterscope(lx->fs, &s, 0); /* explicit scope */
    decl_list(lx, '}'); /* body */
    expectmatch(lx, '}', '{', matchline);
    leavescope(lx->fs);
}


/* 
** paramlist ::= name
**             | '...'
**             | name ',' paramlist
*/
static void paramlist(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Proto *fn = fs->p;
    int nparams = 0;
    int isvararg = 0;
    if (!check(lx, ')')) { /* have at least one arg? */
        do {
            switch (lx->t.tk) {
                case TK_NAME: {
                    newlocalvar(lx, str_expectname(lx));
                    nparams++;
                    break;
                }
                case TK_DOTS: {
                    csY_scan(lx);
                    isvararg = 1;
                    break;
                }
                default: csY_syntaxerror(lx, "<name> or '...' expected");
            }
        } while (!isvararg && match(lx, ','));
    }
    adjustlocals(lx, nparams);
    fn->arity = fs->nactlocals;
    if (isvararg) setvararg(fs, fn->arity);
    csC_reserveslots(fs, fs->nactlocals);
}


/* emit closure instruction */
static void codeclosure(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs->prev;
    initexp(e, EXP_FINEXPR, csC_emitIL(fs, OP_CLOSURE, fs->np - 1));
    csC_reserveslots(fs, 1); /* space for closure */
}


/* funcbody ::= '(' paramlist ')' stmlist */
static void funcbody(Lexer *lx, ExpInfo *v, int linenum, int ismethod) {
    FunctionState newfs;
    Scope scope;
    int matchline;
    newfs.p = addproto(lx);
    newfs.p->defline = linenum;
    open_func(lx, &newfs, &scope);
    matchline = lx->line; /* line where '(' is located */
    expectnext(lx, '(');
    if (ismethod) { /* is this method ? */
        addlocallit(lx, "self"); /* create 'self' */
        adjustlocals(lx, 1); /* and register it */
        /* runtime ensures extra slot for 'self' and its value */
    }
    paramlist(lx);
    expectmatch(lx, ')', '(', matchline);
    matchline = lx->line; /* line where '{' is located */
    expectnext(lx, '{');
    decl_list(lx, '}');
    newfs.p->deflastline = lx->line;
    expectmatch(lx, '}', '{', matchline);
    codeclosure(lx, v);
    close_func(lx);
}


/* 
** stmname ::= name
**           | name '.' stmname
*/
static OString *stmname(Lexer *lx, ExpInfo *v, int *left) {
    OString *name = str_expectname(lx);
    cs_assert(left != NULL && *left == 0);
    var(lx, name, v);
    while (check(lx, '.')) {
        getfield(lx, v, 0);
        *left = 1;
    }
    return (*left ? strval(getconstant(lx->fs, v)) : name);
}


/* fnstm ::= 'fn' stmname funcbody */
static void fnstm(Lexer *lx, int linenum) {
    FunctionState *fs = lx->fs;
    int left = 0;
    ExpInfo var, e;
    csY_scan(lx); /* skip 'fn' */
    stmname(lx, &var, &left);
    funcbody(lx, &e, linenum, 0);
    checkreadonly(lx, &var);
    csC_store(fs, &var);
    csC_pop(fs, left); /* remove leftover (if any) */
}


/* classstm ::= 'class' stmname klass */
static void classstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int left = 0;
    OString *name;
    ExpInfo var;
    csY_scan(lx); /* skip 'class' */
    name = stmname(lx, &var, &left);
    klass(lx, fs, name);
    checkreadonly(lx, &var);
    csC_store(fs, &var);
    cs_assert(var.et == EXP_FINEXPR);
    csC_pop(fs, left); /* remove leftover (if any) */
}


/* literal value information */
typedef struct TLiteral {
    Literal lit; /* literal value */
    int tt; /* type tag */
    int line; /* source line */
} LiteralInfo;


/* 'switch' statement state. */
typedef struct {
    ExpInfo e; /* expression being matched */
    struct {
        LiteralInfo *arr; /* array of literals */
        int len; /* number of elements in 'literals' */
        int size; /* size of 'literals' */
    } literals; /* literal information */
    c_byte havedefault; /* if switch has 'default' case */
    c_byte havenil; /* if switch has 'nil' case */
    c_byte havetrue; /* if switch has '1' case */
    c_byte havefalse; /* if switch has '0' case */
    int jmp; /* code jmp to patch if 'label' expression is not 'CASEMATCH' */
    enum { LNONE, LDEFAULT, LCASE, LMATCH } label;
} SwitchState;


/* convert literal information into text */
static const char *literal2text(cs_State *ts, LiteralInfo *li) {
    switch (li->tt) {
        case CS_VNUMINT: return csS_pushfstring(ts, " (%I)", li->lit.i);
        case CS_VNUMFLT: return csS_pushfstring(ts, " (%N)", li->lit.n);
        case CS_VSHRSTR: case CS_VLNGSTR:
            return csS_pushfstring(ts, " (%s)", getstr(li->lit.str));
        default: cs_assert(0); return NULL;
    }
}


/* find literal info in 'literals' */
static int findliteral(SwitchState *ss, LiteralInfo *tlit) {
    for (int i = 0; i < ss->literals.len; i++) {
        LiteralInfo *curr = &ss->literals.arr[i];
        if (tlit->tt != curr->tt) continue; /* skip if types don't match */
        switch (tlit->tt) {
            case CS_VSHRSTR: case CS_VLNGSTR:
                if (eqstr(tlit->lit.str, curr->lit.str)) return i;
                break;
            case CS_VNUMINT:
                if (tlit->lit.i == curr->lit.i) return i;
                break;
            case CS_VNUMFLT:
                if (c_numeq(tlit->lit.n, curr->lit.n)) return i;
                break;
            default:cs_assert(0); break;
        }
    }
    return -1;
}


/* check for duplicate literal otherwise fill the relevant info */
static LiteralInfo checkduplicate(Lexer *lx, SwitchState *ss, ExpInfo *e) {
    const char *what = NULL;
    int extra = 0;
    LiteralInfo li;
    cs_assert(eisconstant(e));
    switch (e->et) {
        case EXP_FALSE: {
            if (c_unlikely(ss->havefalse)) what = "false";
            ss->havefalse = 1;
            break;
        }
        case EXP_TRUE: {
            if (c_unlikely(ss->havetrue)) what = "true";
            ss->havetrue = 1;
            break;
        }
        case EXP_NIL: {
            if (c_unlikely(ss->havenil)) what = "nil";
            ss->havenil = 1;
            break;
        }
        case EXP_STRING: {
            what = "string";
            li.lit.str = e->u.str;
            li.tt = e->u.str->tt_;
            goto findliteral;
        }
        case EXP_INT: {
            what = "integer";
            li.lit.i = e->u.i;
            li.tt = CS_VNUMINT;
            goto findliteral;
        }
        case EXP_FLT: {
            int idx;
            what = "number";
            li.lit.n = e->u.n;
            li.tt = CS_VNUMFLT;
        findliteral:
            idx = findliteral(ss, &li);
            if (c_likely(idx < 0)) 
                what = NULL;
            else
                extra = 1;
            break;
        }
        default: cs_assert(0); break;
    }
    if (c_unlikely(what))
        csP_semerror(lx, csS_pushfstring(lx->ts,
                    "duplicate %s literal%s in switch statement",
                    what, (extra ? literal2text(lx->ts, &li) : "")));
    return li;
}


/*
** Adds new literal information to 'literals' if 'e' is a constant
** expression.
*/
static int newlitinfo(Lexer *lx, SwitchState *ss, ExpInfo *caseexp) {
    FunctionState *fs = lx->fs;
    if (eisconstant(caseexp)) {
        LiteralInfo li = checkduplicate(lx, ss, caseexp);
        checklimit(fs, ss->literals.len, MAX_LARG, "literal switch cases");
        csM_growarray(lx->ts, ss->literals.arr, ss->literals.size,
                    ss->literals.len, MAX_LARG, "switch literals", LiteralInfo);
        ss->literals.arr[ss->literals.len++] = li;
        if (eisconstant(&ss->e)) { /* both are constant expressions ? */
            TValue v1, v2;
            movexp2v(fs, &ss->e, &v1);
            movexp2v(fs, caseexp, &v2);
            return csV_raweq(&v1, &v2); /* compare for match */
        } /* else fall through */
    } /* else fall through */
    return 0; /* no match */
}


/* 
** Tries to preserve expression 'e' after consuming it, in order
** to enable more optimizations.
*/
static int codepres_exp(FunctionState *fs, ExpInfo *e) {
    ExpInfo pres = *e;
    int isctc = eisconstant(e);
    csC_exp2stack(fs, e);
    if (isctc) *e = pres;
    return isctc;
}


/* 
** switchbody ::= 'case' ':' expr switchbody
**              | 'default' ':' switchbody
**              | stm switchbody
**              | empty
*/
static void switchbody(Lexer *lx, SwitchState *ss, FuncContext *ctxbefore) {
    FunctionState *fs = lx->fs;
    FuncContext ctxafter;
    int ftjmp = NOJMP; /* fall-through jump */
    ctxafter.pc = NOJMP;
    while (!check(lx, '}') && !check(lx, TK_EOS)) { /* while switch body... */
        if (check(lx, TK_CASE) || match(lx, TK_DEFAULT)) { /* has label?... */
            if (ss->label != LNONE && ss->label != LMATCH) {
                /* current label is not a compile-time-constant match */
                ftjmp = csC_jmp(fs, OP_JMP); /* create fall through jump */
                if (ss->label == LCASE) /* this is not a 'default' case? */
                    csC_patchtohere(fs, ss->jmp); /* patch test jump */
            }
            if (match(lx, TK_CASE)) { /* 'case' label? */
                ExpInfo cexp; /* case expression */
                voidexp(&cexp);
                expr(lx, &cexp); /* get the case expression... */
                codepres_exp(fs, &cexp); /* ...and put it on stack (preserve) */
                expectnext(lx, ':');
                if (newlitinfo(lx, ss, &cexp)) { /* ctc match? */
                    ss->label = LMATCH; /* mark the current label as such */
                    loadcontext(fs, ctxbefore); /* load context before switch */
                } else if (ss->label != LMATCH) { /* no match? */
                    ss->label = LCASE; /* mark the current label as such */
                    csC_emitI(fs, OP_EQPRESERVE); /* EQ but preserves lhs */
                    ss->jmp = csC_test(fs, OP_TESTPOP, 0); /* test jump */
                }
            } else if (!ss->havedefault) { /* first 'default' label? */
                ss->havedefault = 1; /* remember this fact... */
                ss->label = LDEFAULT; /* ...and mark the label as such */
                expectnext(lx, ':');
            } else /* multiple 'default' labels are not allowed */
                csP_semerror(lx, "multiple default labels in switch");
            if (ftjmp != NOJMP) { /* have jump to patch? */
                csC_patchtohere(fs, ftjmp); /* if so, patch it */
                ftjmp = NOJMP; /* reset */
            }
        } else if (ss->label != LNONE) { /* or is not empty?... */
            stm(lx);
            if (ss->label == LMATCH /* current label is a ctc match... */
                    && fs->lastwasret /* and last statement was 'return'... */
                    && ctxafter.pc == NOJMP) /* ...and context is free? */
                storecontext(fs, &ctxafter); /* set current context as end */
        } else /* ...otherwise error */
            csP_semerror(lx, "expected 'case' or 'default'");
    }
    if (ctxafter.pc != NOJMP) /* had a compile-time label match with 'return' */
        loadcontext(fs, &ctxafter); /* load it */
}


/* switchstm ::= 'switch' '(' expr ')' '{' switchbody '}' */
static void switchstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Scope *old_switchscope = fs->switchscope;
    int matchline;
    FuncContext ctxbefore;
    Scope s; /* switch scope */
    SwitchState ss;
    ss.literals.len = ss.literals.size = 0;
    ss.literals.arr = NULL;
    ss.jmp = NOJMP;
    ss.label = LNONE;
    ss.havedefault = 0;
    ss.havenil = 0;
    ss.havetrue = 0;
    ss.havefalse = 0;
    enterscope(fs, &s, CFMS);
    storecontext(fs, &ctxbefore);
    fs->switchscope = &s; /* set the innermost 'switch' scope */
    csY_scan(lx); /* skip 'switch' */
    expectnext(lx, '(');
    expr(lx, &ss.e); /* get the 'switch' expression... */
    expectnext(lx, ')');
    codepres_exp(fs, &ss.e);
    matchline = lx->line;
    expectnext(lx, '{');
    switchbody(lx, &ss, &ctxbefore);
    expectmatch(lx, '}', '{', matchline);
    leavescope(fs);
    fs->switchscope = old_switchscope;
}


/* condition statement body; for 'forstm', 'whilestm' and 'ifstm' */
static void condbody(Lexer *lx, FuncContext *ctxbefore, ExpInfo *cond, int isif,
                     OpCode opT, OpCode opJ, int condpc, int clausepc) {
    FunctionState *fs = lx->fs;
    int cisctc = eisconstant(cond);
    int bodypc = currPC;
    FuncContext ctxafter; /* context after the condition statement */
    int test, jump; /* jumps */
    int optaway, cistrue, ttarget;
    cs_assert(!isif == (opJ == OP_JMPS));
    test = jump = ctxafter.pc = NOJMP;
    cistrue = 0;
    optaway = (cisctc && !(cistrue = eistrue(cond)));
    if (!optaway) { /* statement will not be optimized away? */
        if (cistrue) { /* condition is true? */
            if (clausepc == NOJMP) { /* not a forloop? */
                loadcontext(fs, ctxbefore); /* remove condition */
                bodypc = currPC; /* update bodypc */
            } /* otherwise already optimized out condition */
        } else /* otherwise emit condition test jump */
            test = csC_test(fs, opT, 0); /* condition test */
    }
    stm(lx); /* loop/if body */
    if (optaway) { /* optimize away this statement? */
        loadcontext(fs, ctxbefore);
    } else if (cistrue && (fs->lastwasret || isif)) {
        storecontext(fs, &ctxafter);
    } else {
        jump = csC_jmp(fs, opJ); /* loop/if jump */
        if (!isif) { /* loop statement? */
            if (clausepc != NOJMP) { /* 'for' loop? */
                csC_patch(fs, jump, clausepc); /* jump to last clause */
            } else if (cistrue) { /* 'while' loop with true condition? */
                csC_patch(fs, jump, bodypc); /* jump to start of the body */
                fs->loopstart = bodypc; /* convert it to infinite loop */ 
            } else /* 'while' loop with non-constant condition */
                csC_patch(fs, jump, condpc); /* jump back to condition */
        }
    }
    ttarget = currPC; /* set test jump target */
    if (isif) { /* is if statement? */
        int pcjump = (jump != NOJMP ? fs->prevpc : ttarget);
        if (match(lx, TK_ELSE)) /* have else? */
            stm(lx); /* else body */
        if (ttarget == currPC) { /* no else branch? */
            currPC = pcjump; /* adjust pc (remove jump) */
            jump = NOJMP; /* no jump to patch */
            ttarget = currPC; /* adjust test pc */
        }
        if (jump != NOJMP) /* have jump? */
            csC_patchtohere(fs, jump); /* it jumps after else body */
    }
    if (test != NOJMP) /* have condition test? */
        csC_patch(fs, test, ttarget); /* patch it */
    if (ctxafter.pc != NOJMP) /* statement has "dead" code? */
        loadcontext(fs, &ctxafter); /* trim off dead code */
}


/* 
** ifstm ::= 'if' '(' expr ')' condbody
*/
static void ifstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    FuncContext ctxbefore;
    ExpInfo cond;
    int condpc;
    voidexp(&cond);
    csY_scan(lx); /* skip 'if' */
    storecontext(lx->fs, &ctxbefore);
    condpc = currPC;
    expectnext(lx, '(');
    expr(lx, &cond);
    expectnext(lx, ')');
    codepres_exp(fs, &cond);
    condbody(lx, &ctxbefore, &cond, 1, OP_TESTPOP, OP_JMP, condpc, NOJMP);
}


struct LoopState {
    struct LoopState *prev;
    Scope *loopscope;
    int loopstart;
};


/* handle loop state */
static void handlels(FunctionState *fs, struct LoopState *ls, int store) {
    static struct LoopState *currls = NULL;
    if (store) { /* store (chain) 'ls' */
        ls->prev = currls;
        currls = ls;
    } else { /* load (unlink) 'currls' */
        cs_assert(ls == NULL); /* please provide NULL for clarity */
        fs->loopscope = currls->loopscope;
        fs->loopstart = currls->loopstart;
        currls = currls->prev;
    }
}


/* start loop scope */
static void enterloop(FunctionState *fs, Scope *s, struct LoopState *ls,
                      int isgen) {
    enterscope(fs, s, isgen ? CFMGL : CFML);
    ls->prev = NULL;
    ls->loopscope = fs->loopscope;
    ls->loopstart = fs->loopstart;
    handlels(fs, ls, 1);
    fs->loopscope = fs->scope;
    fs->loopstart = currPC;
}


/* end loop scope */
static void leaveloop(FunctionState *fs) {
    cs_assert(is_loop(fs->scope) || is_genloop(fs->scope));
    leavescope(fs);
    handlels(fs, NULL, 0); /* load old loop ctx values */
}


/* whilestm ::= 'while' '(' expr ')' condbody */
static void whilestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    FuncContext ctxbefore;
    struct LoopState ls;
    Scope s; /* new 'loopscope' */
    ExpInfo cond;
    int pcexpr, matchline;
    voidexp(&cond);
    csY_scan(lx); /* skip 'while' */
    enterloop(fs, &s, &ls, 0);
    storecontext(fs, &ctxbefore);
    pcexpr = currPC;
    matchline = lx->line;
    expectnext(lx, '(');
    expr(lx, &cond);
    codepres_exp(fs, &cond);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &ctxbefore, &cond, 0, OP_TESTPOP, OP_JMPS, pcexpr, NOJMP);
    leaveloop(fs);
}


/* patch for loop jump */
static void patchforjmp(FunctionState *fs, int pc, int target, int back) {
    Instruction *jmp = &fs->p->code[pc];
    int offset = target - (pc + getOpSize(*jmp));
    if (back) offset = -offset;
    cs_assert(offset >= 0);
    if (c_unlikely(offset > MAXJMP))
        csY_syntaxerror(fs->lx, "control structure (for loop) too long");
    SETARG_L(jmp, 1, offset);
}


/* generic for loop expressions */
static int forexplist(Lexer *lx, ExpInfo *e, int limit) {
    int nexpr = 1;
    expr(lx, e);
    while (match(lx, ',')) {
        csC_exp2stack(lx->fs, e);
        expr(lx, e);
        nexpr++;
    }
    if (c_unlikely(nexpr > limit))
        limiterror(lx->fs, "foreach loop expressions", limit);
    return nexpr;
}


/* foreachstm ::= 'for' 'each' idlist 'in' forexplist '{' stmlist '}' */
static void foreachstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int nvars = 1; /* iter func result */
    int base = fs->sp;
    int line;
    struct LoopState ls;
    Scope s, svars;
    int forend, prep;
    ExpInfo e;
    voidexp(&e);
    enterloop(fs, &s, &ls, 1); /* enter loop (scope for control variables) */
    csY_scan(lx); /* skip 'foreach' */
    addlocallit(lx, "(foreach iter)");      /* iterator         (base)   */
    addlocallit(lx, "(foreach invariant)"); /* invariant state  (base+1) */
    addlocallit(lx, "(foreach cntlvar)");   /* control var      (base+2) */
    addlocallit(lx, "(foreach tbcvar)");    /* to-be-closed var (base+3) */
    /* create locals variables */
    newlocalvar(lx, str_expectname(lx)); /* at least one variable expected */
    while (match(lx, ',')) {
        newlocalvar(lx, str_expectname(lx));
        nvars++;
    }
    // TODO: locals not registered properly by the parser
    expectnext(lx, TK_IN);
    line = lx->line;
    adjustassign(lx, NSTATEVARS, forexplist(lx, &e, NSTATEVARS), &e);
    adjustlocals(lx, NSTATEVARS); /* register control variables */
    scopemarkclose(fs); /* last control variable might get closed */
    csC_checkstack(fs, 3); /* extra space to call generator */
    prep = csC_emitILL(fs, OP_FORPREP, base, 0);
    enterscope(fs, &svars, 0); /* scope for declared locals */
    adjustlocals(lx, nvars); /* register delcared locals */
    csC_reserveslots(fs, nvars); /* space for declared locals */
    stm(lx); /* body */
    fs->loopstart = currPC;
    /* TODO:
       - continue statement jump needs to be patched to here
       - this would probably require continue jumps to be included in patch list
         meaning that we should convert 'continue' into 'break' when in generic loop.
    */
    leavescope(fs); /* leave declared locals scope */
    patchforjmp(fs, prep, currPC, 0);
    csC_emitILL(fs, OP_FORCALL, base, nvars);
    csC_fixline(fs, line);
    forend = csC_emitILLL(fs, OP_FORLOOP, base, 0, nvars);
    patchforjmp(fs, forend, prep + getOpSize(OP_FORPREP), 1);
    csC_fixline(fs, line);
    leaveloop(fs); /* leave loop (pops control variables) */
}


/* 'for' loop initializer */
void forinitializer(Lexer *lx) {
    if (!match(lx, ';')) { /* have for loop initializer? */
        if (match(lx, TK_LOCAL)) { /* 'local' statement? */
            localstm(lx);
            /* statements end with ';' */
        } else { /* otherwise expression statement */
            exprstm(lx);
            expectnext(lx, ';');
        }
    }
}


/* 'for' loop condition */
int forcondition(Lexer *lx, ExpInfo *e) {
    int isctc;
    if (!match(lx, ';')) { /* have condition? */
        expr(lx, e);                        /* get it... */
        isctc = codepres_exp(lx->fs, e);    /* ...and put it on stack */
        expectnext(lx, ';');
    } else { /* otherwise no condition (infinite loop) */
        initexp(e, EXP_TRUE, 0);
        isctc = 1; /* true */
    }
    return isctc;
}


/* 'for' loop last clause */
void forlastclause(Lexer *lx, FuncContext *ctxbefore, ExpInfo *cond, int *clausepc) {
    FunctionState *fs = lx->fs;
    int bodyjmp, loopjmp;
    int inf = eistrue(cond);
    cs_assert(*clausepc == NOJMP);
    if (inf) /* infinite loop? */
        loadcontext(fs, ctxbefore); /* remove condition */
    if (check(lx, ')')) { /* no end clause? */
        return; /* done (converted to a 'while' loop) */
    } else {
        bodyjmp = csC_jmp(fs, OP_JMP); /* insert jump in-between */
        *clausepc = currPC; /* set end clause pc */
        exprstm(lx); /* get the end clause expression statement */
        if (!inf) { /* loop is not infinite? */
            loopjmp = csC_jmp(fs, OP_JMPS); /* emit jump back to cond... */
            csC_patch(fs, loopjmp, fs->loopstart); /* ...and patch it */
        }
        csC_patchtohere(fs, bodyjmp); /* patch jump from cond to body */
        fs->loopstart = *clausepc; /* loop starts at end clause pc */
    }
}


/* forstm ::= 'for' '(' forinit ';' forcond ';' forlastclause ')' condbody */
static void forstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int matchline = lx->line;
    int condpc, clausepc;
    FuncContext ctxbefore;
    struct LoopState ls;
    Scope s, init;
    ExpInfo cond;
    voidexp(&cond);
    csY_scan(lx); /* skip 'for' */
    enterscope(fs, &init, 0); /* enter initializer scope */
    expectnext(lx, '(');
    forinitializer(lx);
    enterloop(fs, &s, &ls, 0); /* enter loop scope */
    fs->loopstart = currPC; /* loop is after initializer clause */
    storecontext(fs, &ctxbefore); /* store context at start of for loop */
    condpc = currPC;
    forcondition(lx, &cond);
    clausepc = NOJMP;
    forlastclause(lx, &ctxbefore, &cond, &clausepc);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &ctxbefore, &cond, 0, OP_TESTPOP, OP_JMPS, condpc, clausepc);
    leaveloop(fs); /* leave loop scope */
    leavescope(fs); /* leave initializer scope */
}


/* loopstm ::= 'loop' stm */
static void loopstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopState ls;
    Scope s;
    int jmp, lstart;
    csY_scan(lx); /* skip 'loop' */
    lstart = currPC; /* store the pc where the loop starts */
    enterloop(fs, &s, &ls, 0);
    stm(lx);
    if (!fs->lastwasret) {
        jmp = csC_jmp(fs, OP_JMPS);
        csC_patch(fs, jmp, lstart);
    }
    leaveloop(fs);
}


/* continuestm ::= 'continue' ';' */
static void continuestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int old_sp = fs->sp;
    int jmp;
    csY_scan(lx); /* skip 'continue' */
    if (c_unlikely(fs->loopscope == NULL)) /* not in a loop? */
        csP_semerror(lx, "'continue' not in loop statement");
    cs_assert(fs->loopstart != NOJMP); /* must have loop start offset */
    continuepop(fs);
    fs->sp = old_sp; /* restore old 'sp', as parser is not jumping back */
    if (fs->scope->havetbcvar) { /* have to close upvalues? */
        int stklevel = stacklevel(fs, fs->loopscope->nactlocals);
        csC_emitIL(fs, OP_CLOSE, stklevel); /* close them */
    }
    jmp = csC_jmp(fs, OP_JMPS); /* jump backwards... */
    csC_patch(fs, jmp, fs->loopstart); /* ...to start of the loop */
    expectnext(lx, ';');
}


/* 
** Get the most recent control flow scope, or NULL if none
** present.
*/
static const Scope *getcfscope(const FunctionState *fs) {
    const Scope *s = NULL;
    if (fs->switchscope)
        s = fs->switchscope;
    if (fs->loopscope && (!s || s->depth < fs->loopscope->depth))
        s = fs->loopscope;
    return s;
}


/* breakstm ::= 'break' ';' */
static void breakstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    const Scope *cfs = getcfscope(fs); /* control flow scope */
    csY_scan(lx); /* skip 'break' */
    if (c_unlikely(cfs == NULL)) /* no control flow scope? */
        csP_semerror(lx, "'break' not in loop or switch statement");
    cs_assert(haspatchlist(cfs));
    addbreakjmp(lx, gplist(lx), csC_jmp(fs, OP_BJMP), fs->scope->haveupval);
    expectnext(lx, ';');
}


/* 
** returnstm ::= 'return' ';' 
**             | 'return' explist ';'
*/
static void returnstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int first = nvarstack(fs); /* first slot to be returned */
    int nret = 0;
    ExpInfo e;
    voidexp(&e);
    csY_scan(lx); /* skip 'return' */
    if (!check(lx, ';')) { /* have return values ? */
        nret = explist(lx, &e); /* get return values */
        if (eismulret(&e)) {
            csC_setmulret(fs, &e);
            nret = CS_MULRET; /* return all values */
        } else
            csC_exp2stack(fs, &e);
    }
    csC_ret(fs, first, nret);
    expectnext(lx, ';');
    fs->lastwasret = 1; /* indicate last statement was return */
}


/* 
** stm_ ::= fnstm
**        | classstm
**        | whilestm
**        | forstm
**        | foreachstm
**        | ifstm
**        | switchstm
**        | blockstm
**        | continuestm
**        | breakstm
**        | returnstm
**        | loopstm
**        | ';'
**        | exprstm
*/
static void stm_(Lexer *lx) {
    switch (lx->t.tk) {
        case TK_FN: {
            fnstm(lx, lx->line);
            break;
        }
        case TK_CLASS: {
            classstm(lx);
            break;
        }
        case TK_WHILE: {
            whilestm(lx);
            break;
        }
        case TK_FOR: {
            forstm(lx);
            break;
        }
        case TK_FOREACH: {
            foreachstm(lx);
            break;
        }
        case TK_IF: {
            ifstm(lx);
            break;
        }
        case TK_SWITCH: {
            switchstm(lx);
            break;
        }
        case '{': {
            blockstm(lx);
            break;
        }
        case TK_CONTINUE: {
            continuestm(lx);
            break;
        }
        case TK_BREAK: {
            breakstm(lx);
            break;
        }
        case TK_RETURN: {
            returnstm(lx);
            return; /* do not reset 'lastwasret' flag */
        }
        case TK_LOOP: {
            loopstm(lx);
            break;
        }
        case ';': {
            csY_scan(lx);
            break;
        }
        default: {
            exprstm(lx);
            expectnext(lx, ';');
            break;
        }
    }
    lx->fs->lastwasret = 0;
}


static void freestackslots(FunctionState *fs) {
    cs_assert(fs->p->maxstack >= fs->sp);
    cs_assert(fs->sp >= nvarstack(fs) + fs->scope->nswscope);
    /* leave only locals and switch statement expressions */
    fs->sp = nvarstack(fs) + fs->scope->nswscope;
}


/*
** decl ::= localstm
**        | localfn
**        | localclass
**        | stm
*/
static void decl(Lexer *lx) {
    enterCstack(lx);
    switch (lx->t.tk) {
        case TK_LOCAL: {
            csY_scan(lx); /* skip 'local' */
            if (match(lx, TK_FN))
                localfn(lx);
            else if (match(lx, TK_CLASS))
                localclass(lx);
            else
                localstm(lx);
            lx->fs->lastwasret = 0;
            break;
        }
        default: {
            stm_(lx);
            break;
        }
    }
    freestackslots(lx->fs);
    leaveCstack(lx);
}


static void stm(Lexer *lx) {
    enterCstack(lx);
    stm_(lx);
    freestackslots(lx->fs);
    leaveCstack(lx);
}


/* compile main function */
static void mainfunc(FunctionState *fs, Lexer *lx) {
    Scope s;
    open_func(lx, fs, &s);
    setvararg(fs, 0); /* main function is always vararg */
    csY_scan(lx); /* scan first token */
    decl_list(lx, 0);
    expect(lx, TK_EOS);
    close_func(lx);
}


/* parse source code */
CSClosure *csP_parse(cs_State *ts, BuffReader *br, Buffer *buff,
                     ParserState *ps, const char *source) {
    Lexer lx;
    FunctionState fs;
    CSClosure *cl = csF_newCSClosure(ts, 0);
    setclCSval2s(ts, ts->sp.p, cl); /* anchor main function closure */
    csT_incsp(ts);
    lx.tab = csH_new(ts);
    sethtval2s(ts, ts->sp.p, lx.tab); /* anchor scanner table */
    csT_incsp(ts);
    fs.p = cl->p = csF_newproto(ts);
    csG_objbarrier(ts, cl, cl->p);
    fs.p->source = csS_new(ts, source);
    csG_objbarrier(ts, fs.p, fs.p->source);
    lx.ps = ps;
    lx.buff = buff;
    csY_setinput(ts, &lx, br, fs.p->source);
    mainfunc(&fs, &lx);
    cs_assert(!fs.prev && fs.nupvals == 0 && !lx.fs);
    /* all scopes should be correctly finished */
    cs_assert(ps->actlocals.len == 0 && ps->patches.len == 0 && !ps->cs);
    ts->sp.p--; /* remove scanner table */
    return cl;
}
