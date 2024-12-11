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


/* check if current token matches 'tk' */
#define check(lx, tok)       ((lx)->t.tk == (tok))


#define enterCstack(lx)         csT_incCstack((lx)->ts)
#define leaveCstack(lx)         ((lx)->ts->nCcalls--)


/* expect 'cond' to be true or invoke error */
#define expect_cond(lx, cond, err) \
    { if (!(cond)) csY_syntaxerror(lx, err); }


/* 'cfbits' */
#define CFLOOP      0 /* 'for' loop */
#define CFSWITCH    1 /* 'switch' */

/* test 'cfbits' */
#define sisloop(s)      testbit((s)->cfbits, CFLOOP)
#define sisswitch(s)    testbit((s)->cfbits, CFSWITCH)


#define checklen(fs)      ((fs)->patches.len > 0)

#define lastpatchlist(fs) \
        check_exp(checklen(fs), &(fs)->patches.list[(fs)->patches.len - 1])

#define patchlistpop(fs) \
        check_exp(checklen(fs), &(fs)->patches.list[--(fs)->patches.len])


/*
** BreakJmp used to indicate end of the patch list.
*/
static const BreakJmp dummyjmp = { NOJMP, 0 };


/* lexical scope information */
typedef struct Scope {
    struct Scope *prev; /* implicit linked-list */
    int activelocals; /* number of locals outside of this scope */
    int nswscope; /* number of switch scopes before this scope */
    int depth;
    cs_ubyte cfbits; /* control flow bits */
    cs_ubyte haveupval; /* set if scope contains upvalue variable */
    cs_ubyte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;



static cs_noret expecterror(Lexer *lx, int tk) {
    const char *err = csS_pushfstring(lx->ts, "expected %s",
                                            csY_tok2str(lx, tk));
    csY_syntaxerror(lx, err);
}


static cs_noret limiterror(FunctionState *fs, const char *what, int limit) {
    cs_State *ts = fs->lx->ts;
    int line = fs->p->defline;
    const char *where = (line == 0 ? "main function" :
                        csS_pushfstring(ts, "function at line %d", line));
    const char *err = csS_pushfstring(ts, "too many %s (limit is %d) in %s",
                                          what, limit, where);
    csY_syntaxerror(fs->lx, err);
}


/* semantic error; variant of syntax error without 'near <token>' */
cs_noret csP_semerror(Lexer *lx, const char *err) {
    lx->t.tk = 0;
    csY_syntaxerror(lx, err);
}


static void checklimit(FunctionState *fs, int n, int limit, const char *what) {
    if (n >= limit)
        limiterror(fs, what, limit);
}


static void storectx(FunctionState *fs, DynCtx *ctx) {
    ctx->loopstart = fs->loopstart;
    ctx->sp = fs->sp;
    ctx->np = fs->np;
    ctx->nk = fs->nk;
    ctx->pc = fs->pc;
    ctx->nlinfo = fs->nlinfo;
    ctx->nlocals = fs->nlocals;
    ctx->nupvals = fs->nupvals;
    ctx->nbrks = fs->patches.len;
    ctx->needclose = fs->needclose;
    ctx->lastwasret = fs->lastwasret;
}


static void loadctx(FunctionState *fs, DynCtx *ctx) {
    fs->loopstart = ctx->loopstart;
    fs->sp = ctx->sp;
    fs->np = ctx->np;
    fs->nk = ctx->nk;
    fs->pc = ctx->pc;
    fs->nlinfo = ctx->nlinfo;
    fs->nlocals = ctx->nlocals;
    fs->nupvals = ctx->nupvals;
    fs->patches.len = ctx->nbrks;
    fs->needclose = ctx->needclose;
    fs->lastwasret = ctx->lastwasret;
    fs->p->linfo[fs->nlinfo - 1].pc = fs->pc;
}


/* 
** If 'deadcode' is not yet stored, store the current state
** into it.
*/
static void storereachablectx(FunctionState *fs) {
    if (fs->deadcode.pc == NOJMP) /* 'deadcode' empty ? */
        storectx(fs, &fs->deadcode);
}


/* load context right before dead code */
static void loadreachablectx(FunctionState *fs) {
    cs_assert(fs->deadcode.pc >= 0);
    loadctx(fs, &fs->deadcode);
}


/* get local variable */
static LVar *getlocalvar(FunctionState *fs, int idx) {
    return &fs->lx->ps->actlocals.arr[fs->firstlocal + idx];
}


/* get local variable debug information */
static LVarInfo *getlocalinfo(FunctionState *fs, int vidx) {
    LVar *lv;
    cs_assert(0 <= vidx && vidx <= fs->nactlocals);
    lv = getlocalvar(fs, vidx);
    return &fs->p->locals[lv->s.idx];
}


/*
** Convert 'nvar', a compiler index level, to its corresponding
** stack level.
*/
static int stacklevel(FunctionState *fs, int nvar) {
    if (nvar-- > 0) /* have at least one variable? */
        return getlocalvar(fs, nvar)->s.idx + 1;
    return 0; /* no variables on stack */
}


/*
** Return number of variables on the stack for the given
** function.
*/
static int nvarstack(FunctionState *fs) {
    return stacklevel(fs, fs->nactlocals);
}


/* pop last pending jump from currently active patch list */
static BreakJmp popbreakjmp(Lexer *lx) {
    FunctionState *fs = lx->fs;
    PatchList *l = lastpatchlist(fs);
    if (l->len > 0)
        return l->arr[--l->len];
    else
        return dummyjmp;
}


/* add jump to patch list (for backpatching) */
static void patchlistadd(Lexer *lx, int jmp, cs_ubyte hasclose) {
    FunctionState *fs = lx->fs;
    BreakJmp bj = { jmp, hasclose };
    PatchList *l = lastpatchlist(fs);
    checklimit(fs, l->size, MAX_INT, "code jumps");
    csM_growarray(lx->ts, l->arr, l->size, l->len, MAX_CODE, "code jumps",
                  BreakJmp);
    l->arr[l->len++] = bj;
}


/* reset current patch list length */
static void patchlistreset(Lexer *lx) {
    lastpatchlist(lx->fs)->len = 0;
}


/* create new patch list */
static void patchliststart(Lexer *lx) {
    FunctionState *fs = lx->fs;
    PatchList newpl = { 0, 0, NULL };
    csM_growarray(lx->ts, fs->patches.list, fs->patches.size, fs->patches.len,
                  MAX_INT, "control flows code jumps", PatchList);
    fs->patches.list[fs->patches.len++] = newpl;
}


/* end patch list by patching all pending jumps */
static int patchlistend(Lexer *lx, int target) {
    FunctionState *fs = lx->fs;
    PatchList *l = patchlistpop(fs);
    BreakJmp brj;
    int hasclose = 0;
    while ((brj = popbreakjmp(lx)).jmp != NOJMP) {
        hasclose |= brj.hasclose;
        csC_patch(fs, brj.jmp, target); /* back-patch the jump */
    }
    csM_freearray(lx->ts, l->arr, l->size);
    if (hasclose) csC_emitIL(fs, OP_CLOSE, nvarstack(fs));
    return hasclose;
}


static void poplocals(FunctionState *fs, int tolevel, int nswitch) {
    csC_pop(fs, fs->nactlocals - tolevel + nswitch);
}


/* 
** Remove local variables up to 'tolevel'.
*/
static void removelocals(FunctionState *fs, int tolevel) {
    fs->lx->ps->actlocals.len -= (fs->nactlocals - tolevel);
    cs_assert(fs->lx->ps->actlocals.len >= 0);
    while (fs->nactlocals > tolevel) /* set debug information */
        getlocalinfo(fs, --fs->nactlocals)->endpc = fs->pc;
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


static void initstring(ExpInfo *e, OString *s) {
    e->f = e->t = NOJMP;
    e->et = EXP_STRING;
    e->u.str = s;
}


/* add local debug information into 'locals' */
static int registerlocal(Lexer *lx, FunctionState *fs, OString *name) {
    Proto *p = fs->p;
    int osz = p->sizelocals;
    csM_growarray(lx->ts, p->locals, p->sizelocals, fs->nlocals,
                  MAX_LARG, "locals", LVarInfo);
    while (osz < p->sizelocals)
        p->locals[osz++].name = NULL;
    p->locals[fs->nlocals].name = name;
    p->locals[fs->nlocals].startpc = fs->pc;
    csG_objbarrier(lx->ts, p, name);
    return fs->nlocals++;
}


/*
** Adjust locals by increment 'nactlocals' and registering them
** inside 'locals'.
*/
static void adjustlocals(Lexer *lx, int nvars) {
    FunctionState *fs = lx->fs;
    for (int i = 0; i < nvars; nvars--) {
        int idx = fs->nactlocals++;
        LVar *local = getlocalvar(fs, idx);
        local->s.idx = registerlocal(lx, fs, local->s.name);
    }
}


/* start lexical scope */
static void open_scope(FunctionState *fs, Scope *s, int cfbits) {
    if (cfbits & (CFLOOP | CFSWITCH)) /* needs a patch list ? */
        patchliststart(fs->lx); /* 'break' statement jumps storage */
    if (fs->scope) {
        s->nswscope = fs->scope->nswscope + sisswitch(fs->scope);
        s->depth = fs->scope->depth + 1;
        s->havetbcvar = fs->scope->havetbcvar;
    } else {
        s->nswscope = 0;
        s->depth = 0;
        s->havetbcvar = 0;
    }
    s->activelocals = fs->nactlocals;
    s->cfbits = cfbits;
    s->haveupval = 0;
    s->prev = fs->scope;
    fs->scope = s;
}


/* end lexical scope */
static void close_scope(FunctionState *fs) {
    Scope *s = fs->scope;
    int stklevel = stacklevel(fs, s->activelocals);
    int popn = fs->nactlocals - s->activelocals + sisswitch(s);
    int hasclose;
    removelocals(fs, s->activelocals); /* remove scope locals */
    cs_assert(s->activelocals == fs->nactlocals);
    if (sisloop(s) || sisswitch(s)) /* have pending jumps to fix? */
        hasclose = patchlistend(fs->lx, currentpc(fs));
    if (s->prev) { /* not main function scope? */
        if (!hasclose && s->haveupval) /* still need to close? */
            csC_emitIS(fs, OP_CLOSE, stklevel);
        csC_pop(fs, popn); /* pop locals and switch expression (if any) */
    } else if (!fs->lastwasret) /* last scope missing 'return'? */
        csC_ret(fs, 0, 0); /* 'return;' */
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
    while(s->activelocals - 1 > vidx)
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
    fs->sp = 0;
    fs->nactlocals = 0;
    fs->firstlocal = lx->ps->actlocals.len;
    fs->np = 0;
    fs->nk = 0;
    fs->pc = 0;
    fs->nlinfo = 0;
    fs->nlocals = 0;
    fs->nupvals = 0;
    fs->deadcode.pc = NOJMP;
    fs->patches.len = fs->patches.size = 0; fs->patches.list = NULL;
    fs->needclose = fs->lastwasret = 0;
    fs->pclastop = -1;
    p->source = lx->src;
    csG_objbarrier(lx->ts, p, p->source);
    p->maxstack = 2; /* stacks slots 0/1 are always valid */
    open_scope(fs, s, 0); /* start top-level scope */
}


static void close_func(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Proto *p = fs->p;
    cs_State *ts = lx->ts;
    if (fs->deadcode.pc != NOJMP) /* have unreachable code? */
        loadreachablectx(fs); /* load reachable context */
    cs_assert(fs->scope && fs->scope->prev == NULL);
    close_scope(fs); /* end final scope */
    cs_assert(fs->scope == NULL);
    cs_assert(fs->sp == 0);
    csM_freearray(ts, fs->patches.list, fs->patches.size);
    csC_finish(fs); /* final code adjustments */
    /* shrink unused memory */
    csM_shrinkarray(ts, p->p, p->sizep, fs->np, Proto);
    csM_shrinkarray(ts, p->k, p->sizek, fs->nk, TValue);
    csM_shrinkarray(ts, p->code, p->sizecode, fs->pc, Instruction);
    csM_shrinkarray(ts, p->linfo, p->sizelinfo, fs->nlinfo, LineInfo);
    csM_shrinkarray(ts, p->locals, p->sizelocals, fs->nlocals, LVarInfo);
    csM_shrinkarray(ts, p->upvals, p->sizeupvals, fs->nupvals, UpValInfo);
    lx->fs = fs->prev; /* go back to enclosing function (if any) */
    csG_checkGC(ts); /* try to collect garbage memory */
#if 1 /* low-level bytecode disassembly */
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



/* forward declare, can be both part of statement and expression */
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
        if (lx->line == linenum) { /* same line ? */
            expecterror(lx, what); /* emit usual error message */
        } else {
            csY_syntaxerror(lx, csS_pushfstring(lx->ts,
                    "%s expected (to close %s at line %d)",
                    csY_tok2str(lx, what), csY_tok2str(lx, who), linenum));
        }
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
static int newlocal(Lexer *lx, OString *name) {
    FunctionState *fs = lx->fs;
    ParserState *ps = lx->ps;
    LVar *local;
    checklimit(fs, ps->actlocals.len + 1 - fs->firstlocal, MAXVARS, "locals");
    csM_growarray(lx->ts, ps->actlocals.arr, ps->actlocals.size,
                  ps->actlocals.len, MAXVARS, "locals", LVar);
    local = &ps->actlocals.arr[ps->actlocals.len++];
    local->s.kind = VARREG;
    local->s.name = name;
    local->s.idx = -1;
    return ps->actlocals.len - fs->firstlocal - 1;
}


#define newlocallit(lx,lit) \
        newlocal(lx, csY_newstring(lx, "" lit, SLL(lit)))


/*
** Searches for local variable 'name'.
*/
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e) {
    for (int i = fs->nactlocals - 1; 0 <= i; i--) {
        LVar *local = getlocalvar(fs, i);
        if (eqstr(name, local->s.name)) { /* found? */
            if (c_unlikely(local->s.idx == -1)) { /* uninitialized? */
                csP_semerror(fs->lx,
                    csS_pushfstring(fs->lx->ts, 
                        "can't read %s variable '%s' in its own initializer",
                        "local", getstr(name)));
            } else {
                initexp(e, EXP_LOCAL, i);
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
        initexp(var, EXP_VOID, 0); /* not found */
    } else { /* otherwise search... */
        int ret = searchlocal(fs, name, var); /* ...locals */
        if (ret >= 0) { /* found? */
            if (ret == EXP_LOCAL && !base) /* in recursive call to varaux? */
                scopemarkupval(fs, var->u.info); /* mark scope appropriately */
        } else { /* if not found try searching for upvalue */
            ret = searchupvalue(fs, name);
            if (ret < 0) { /* still not found? */
                varaux(fs->prev, name, var, 0); /* try enclosing 'fs' */
                if (var->et == EXP_LOCAL || var->et == EXP_UVAL) /* found? */
                    ret = addupvalue(fs, name, var); /* add upvalue to 'fs' */
                else /* otherwise var must be set as VOID */
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
    initexp(&key, EXP_VOID, 0);
    csY_scan(lx); /* skip '[' */
    csC_exp2stack(lx->fs, var);
    expr(lx, &key);
    csC_indexed(lx->fs, var, &key, super);
    expectnext(lx, ']');
}


/* getfield ::= '.' name */
static void getfield(Lexer *lx, ExpInfo *var, int super) {
    ExpInfo key;
    initexp(&key, EXP_VOID, 0);
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
    if (c_unlikely(lx->ps->cs == NULL)) {
        csP_semerror(lx, "usage of 'super' outside of method");
    } else if (c_unlikely(!lx->ps->cs->super)) {
        csY_syntaxerror(lx, "use of 'super' but class does not inherit");
    } else {
        FunctionState *fs = lx->fs;
        csY_scan(lx); /* skip 'super' */
        varlit(lx, "self", e); /* get class... */
        cs_assert(e->et == EXP_LOCAL); /* which must be local... */
        csC_exp2stack(fs, e); /* ...and put it on stack */
        varlit(lx, "super", e); /* get superclass... */
        cs_assert(e->et == EXP_UVAL); /* ...which must be upvalue */
        if (check(lx, '[')) /* index access? */
            indexed(lx, e, 1);
        else if (check(lx, '.')) /* field access? */
            getfield(lx, e, 1);
        else 
            csY_syntaxerror(lx, "'super' expects '.' or '['");
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
    initexp(&c.u.a.v, EXP_VOID, 0); /* no value (yet) */
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
        checklimit(fs, c->u.t.nh, MAX_INT, "records in a table constructor");
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
    initexp(t, EXP_FINEXPR, fs->sp); /* table will be at stack top */
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
    cs_ubyte left;
    cs_ubyte right;
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
    enterCstack(lx);
    Unopr uopr = getunopr(lx->t.tk);
    if (uopr != OPR_NOUNOPR) {
        csY_scan(lx); /* skip operator */
        subexpr(lx, e, UNARY_PRIORITY);
        csC_unary(lx->fs, e, uopr);
    } else {
        simpleexp(lx, e);
    }
    Binopr opr = getbinopr(lx->t.tk);
    while (opr != OPR_NOBINOPR && priority[opr].left > limit) {
        ExpInfo e2;
        initexp(&e2, EXP_VOID, 0);
        csY_scan(lx); /* skip operator */
        csC_prebinary(lx->fs, e, opr);
        Binopr next = subexpr(lx, &e2, priority[opr].right);
        csC_binary(lx->fs, e, &e2, opr);
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
** listdecl ::= decl
**           | decl listdecl
**           | listdecl returnstm
**           | listdecl continuestm
**           | listdecl breakstm
*/
static void listdecl(Lexer *lx, int blocktk) {
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
    ExpInfo dummy;
    if (c_unlikely(searchlocal(lx->fs, name, &dummy) >= 0))
        csP_semerror(lx, csS_pushfstring(lx->ts,
                     "redefinition of local variable '%s'", getstr(name)));
    return newlocal(lx, name);
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
    if (match(lx, '=')) {
        nexps = explist(lx, &e);
    } else {
        e.et = EXP_VOID;
        nexps = 0;
    }
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
    getlocalinfo(fs, fvar)->startpc = fs->pc;
}


/* inherit from superclass */
static void codeinherit(Lexer *lx, OString *name, Scope *s) {
    FunctionState *fs = lx->fs;
    OString *supname = str_expectname(lx);
    ExpInfo e;
    if (c_unlikely(eqstr(name, supname))) /* name collision ? */
        csP_semerror(lx, csS_pushfstring(lx->ts,
                    "class '%s' attempt to inherit itself", name));
    varaux(fs, supname, &e, 1); /* get the superclass... */
    csC_varexp2stack(fs, &e); /* and put it on the stack... */
    open_scope(fs, s, 0); /* start scope for super variable... */
    newlocallit(lx, "super"); /* create superclass variable... */
    adjustlocals(lx, 1); /* and register it... */
    varaux(fs, name, &e, 1); /* get the class... */
    csC_varexp2stack(fs, &e); /* put it on the stack... */
    csC_emitI(fs, OP_INHERIT); /* ...and finally inherit */
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
        csC_pop(fs, 1); /* pop dup. class */
        close_scope(fs); /* end 'super' scope */
    }
    ps->cs = ps->cs->prev;
}


static int codemethod(FunctionState *fs, ExpInfo *var) {
    cs_assert(var->et == EXP_STRING);
    if (ismetatag(var->u.str)) {
        csC_emitIS(fs, OP_SETMM, var->u.str->extra - NUM_KEYWORDS - 1);
        return 0; /* (this function goes in VMT) */
    } else {
        csC_method(fs, var);
        csC_reserveslots(fs, 1); /* space of method */
        return 1; /* have methods hashtable entry */
    }
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
    getlocalinfo(fs, cvar)->startpc = fs->pc;
}


/* blockstm ::= '{' stmlist '}' */
static void blockstm(Lexer *lx) {
    int matchline = lx->line;
    Scope s;
    csY_scan(lx); /* skip '{' */
    open_scope(lx->fs, &s, 0);
    listdecl(lx, '}');
    expectmatch(lx, '}', '{', matchline);
    close_scope(lx->fs);
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
    if (isvararg)
        setvararg(fs, fn->arity);
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
        newlocallit(lx, "self"); /* create 'self' */
        adjustlocals(lx, 1); /* and register it */
        /* runtime ensures extra slot for 'self' */
    }
    paramlist(lx);
    expectmatch(lx, ')', '(', matchline);
    matchline = lx->line; /* line where '{' is located */
    expectnext(lx, '{');
    listdecl(lx, '}');
    newfs.p->deflastline = lx->line;
    expectmatch(lx, '}', '{', matchline);
    codeclosure(lx, v);
    close_func(lx);
}


/* 
** stmname ::= name
**           | name '.' stmname
*/
static OString *stmname(Lexer *lx, ExpInfo *v) {
    OString *name = str_expectname(lx);
    var(lx, name, v);
    while (check(lx, '.'))
        getfield(lx, v, 0);
    return strval(getconstant(lx->fs, v));
}


/* fnstm ::= 'fn' stmname funcbody */
static void fnstm(Lexer *lx, int linenum) {
    FunctionState *fs = lx->fs;
    ExpInfo var, e;
    csY_scan(lx); /* skip 'fn' */
    stmname(lx, &var);
    funcbody(lx, &e, linenum, 0);
    checkreadonly(lx, &var);
    csC_store(fs, &var);
}


/* classstm ::= 'class' stmname klass */
static void classstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    OString *name;
    ExpInfo var;
    csY_scan(lx); /* skip 'class' */
    name = stmname(lx, &var);
    klass(lx, fs, name);
    checkreadonly(lx, &var);
    csC_store(fs, &var);
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
    cs_ubyte havedefault; /* if switch has 'default' case */
    cs_ubyte havenil; /* if switch has 'nil' case */
    cs_ubyte havetrue; /* if switch has '1' case */
    cs_ubyte havefalse; /* if switch has '0' case */
    int jmp; /* code jmp to patch if 'label' expression is not 'CASEMATCH' */
    enum { LNONE, LDEFAULT, LCASE, LMATCH } label;
} SwitchState;


static void initss(SwitchState *ss) {
    ss->literals.arr = NULL; ss->literals.len = ss->literals.size = 0;
    ss->jmp = NOJMP;
    ss->label = LNONE;
    ss->havedefault = 0;
    ss->havenil = 0;
    ss->havetrue = 0;
    ss->havefalse = 0;
}


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
                if (csi_numeq(tlit->lit.n, curr->lit.n)) return i;
                break;
            default:cs_assert(0); break;
        }
    }
    return -1;
}


/* check for duplicate literal otherwise fill the relevant info */
static LiteralInfo checkduplicate(Lexer *lx, SwitchState *ss, ExpInfo *e) {
    LiteralInfo li;
    int extra = 0;
    const char *what = NULL;
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


/* add new fall through case jump */
static void addfallthrough(Lexer *lx) {
    int jmp = csC_jmp(lx->fs, OP_JMP);
    patchlistadd(lx, jmp, 0);
}


/* 
** Tries to preserve expression 'e' after consuming it, in order
** to enable more optimizations.
*/
static int codepres_exp(FunctionState *fs, ExpInfo *e) {
    ExpInfo pres = *e;
    int isctc = eisconstant(e);
    csC_exp2stack(fs, e);
    if (isctc)
        *e = pres;
    return isctc;
}


/* 
** switchbody ::= 'case' ':' expr switchbody
**              | 'default' ':' switchbody
**              | stm switchbody
**              | empty
*/
static void switchbody(Lexer *lx, SwitchState *ss, DynCtx *ctx) {
    DynCtx endctx;
    FunctionState *fs = lx->fs;
    BreakJmp brj;
    endctx.pc = ctx->pc;
    while (!check(lx, '}') && !check(lx, TK_EOS)) { /* while switch body... */
        if (check(lx, TK_CASE) || match(lx, TK_DEFAULT)) { /* has label?... */
            if (ss->label != LNONE && ss->label != LMATCH) {
                /* have a label that is not a compile-time-constant match */
                addfallthrough(lx); /* create fall through jump */
                if (ss->label == LCASE) /* this is not a 'default' case? */
                    csC_patchtohere(fs, ss->jmp); /* patch test jump */
            }
            if (match(lx, TK_CASE)) { /* 'case'? */
                ExpInfo cexp; /* case expression */
                initexp(&cexp, EXP_VOID, 0);
                expr(lx, &cexp); /* get the case expression... */
                codepres_exp(fs, &cexp); /* ...and put it on stack (preserve) */
                expectnext(lx, ':');
                if (newlitinfo(lx, ss, &cexp)) { /* ctc match? */
                    ss->label = LMATCH; /* mark the current label as such */
                    loadctx(fs, ctx); /* load context ('switch' start) */
                    patchlistreset(lx); /* all previous jumps are removed */
                } else if (ss->label != LMATCH) { /* no match? */
                    ss->label = LCASE; /* mark the current label as such */
                    csC_emitI(fs, OP_EQPRESERVE); /* EQ but preserves lhs */
                    ss->jmp = csC_test(fs, OP_TESTPOP, 0); /* test jump */
                }
            } else if (!ss->havedefault) { /* first 'default' case? */
                ss->havedefault = 1; /* remember this fact... */
                ss->label = LDEFAULT; /* ...and mark the label as such */
                expectnext(lx, ':');
            } else /* multiple 'default' cases are not allowed */
                csP_semerror(lx, "multiple default cases in switch");
            if ((brj = popbreakjmp(lx)).jmp != NOJMP) /* have jump to patch? */
                csC_patchtohere(fs, brj.jmp); /* if so, patch it */
        } else if (ss->label != LNONE) { /* is not empty?... */
            stm(lx);
            if (ss->label == LMATCH && fs->lastwasret) {
                /* current label is ctc match and last statement is 'return' */
                storectx(fs, &endctx); /* store this as end context... */
                storereachablectx(fs); /* ...and as start of 'deadcode' */
            }
        } else /* ...otherwise error */
            csP_semerror(lx, "expected 'case' or 'default'");
    }
    if (ss->label == LMATCH) { /* had a ctc label match with 'return' */
        cs_assert(endctx.pc > ctx->pc); /* must have valid 'endctx' */
        loadctx(fs, &endctx); /* load it */
    }
}


/* switchstm ::= 'switch' '(' expr ')' '{' switchbody '}' */
static void switchstm(Lexer *lx) {
    SwitchState ss;
    FunctionState *fs = lx->fs;
    DynCtx ctx;
    Scope *old_switchscope = fs->switchscope;
    Scope s; /* switch scope */
    initss(&ss); /* initialize 'switch' state */
    open_scope(fs, &s, CFSWITCH); /* enter switch scope */
    fs->switchscope = &s; /* set the innermost 'switch' scope */
    csY_scan(lx); /* skip 'switch' */
    int matchline = lx->line;
    expectnext(lx, '(');
    expr(lx, &ss.e); /* get the 'switch' expression... */
    codepres_exp(fs, &ss.e); /* ...and put it on stack but preserve 'e' */
    storectx(fs, &ctx); /* store context at 'switch' start */
    expectmatch(lx, ')', '(', matchline);
    matchline = lx->line;
    expectnext(lx, '{');
    switchbody(lx, &ss, &ctx);
    expectmatch(lx, '}', '{', matchline);
    close_scope(fs); /* end 'switch' scope */
    fs->switchscope = old_switchscope;
}


/* condition statement body; for 'forloop', 'whilestm' & 'ifstm' */
static void condbody(Lexer *lx, DynCtx *startctx, ExpInfo *cond, OpCode testop,
                     OpCode jmpop, int condpc, int endclausepc) {
    FunctionState *fs = lx->fs;
    DynCtx endctx;
    int test, jmp;
    int optaway, condistrue;
    int condisctc = eisconstant(cond);
    int bodypc = currentpc(fs);
    int isloop = sisloop(fs->scope);
    cs_assert(isloop == (jmpop == OP_JMPS));
    optaway = condistrue = 0;
    endctx.pc = NOJMP;
    jmp = NOJMP; /* to avoid warnings */
    if (condisctc && !(condistrue = eistrue(cond)))
        optaway = 1; /* can optimize away the statement */
    else /* otherwise emit test instruction */
        test = csC_test(fs, testop, 0);
    stm(lx); /* condition statement (body) */
    if (optaway) { /* optimize away the whole statement? */
        loadctx(fs, startctx); /* load the starting context */
        patchlistreset(lx);
    } else if (condisctc && fs->lastwasret) {
        /* last stm was `return` and... */
        cs_assert(condistrue); /* ...condition is true */
        storectx(fs, &endctx); /* store current context as the end... */
        storereachablectx(fs); /* ...and mark the start of 'deadcode' */
    } else { /* emit jump */
        jmp = csC_jmp(fs, jmpop); /* <- */
        if (isloop) { /* loop body ? */
            if (endclausepc != NOJMP) { /* 'for' loop ? */
                csC_patch(fs, jmp, endclausepc);
            } else if (condistrue) { /* 'while' loop with true condition ? */
                csC_patch(fs, jmp, bodypc);
                fs->loopstart = bodypc;
            } else { /* else 'while' loop with unknown non-constant cond ? */
                csC_patch(fs, jmp, condpc);
            }
        } /* else nothing to back-patch */
    }
    if (!optaway) /* the statement wasn't optimized away ?*/
        csC_patchtohere(fs, test); /* patch the test jump */
    if (!isloop && match(lx, TK_ELSE)) { /* `if` statement with `else` ? */
        stm(lx); /* `else` expects statement */
        if (!optaway && !condisctc) {
            /* code is not optimized away and condition is not a ctc */
            cs_assert(jmp >= 0); /* must of emitted a jump instruction */
            csC_patchtohere(fs, jmp); /* patch the jump (grep "<-") */
        } /* else nothing to back-patch */
    }
    if (endctx.pc != NOJMP) /* have end context? */
        loadctx(fs, &endctx); /* load it */
}


/* 
** ifstm ::= 'if' '(' expr ')' condbody
*/
static void ifstm(Lexer *lx) {
    DynCtx ctx;
    ExpInfo e;
    initexp(&e, EXP_VOID, 0);
    csY_scan(lx); /* skip 'if' */
    storectx(lx->fs, &ctx);
    int matchline = lx->line;
    expectnext(lx, '(');
    expr(lx, &e);
    codepres_exp(lx->fs, &e);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &ctx, &e, OP_TESTPOP, OP_JMP, NOJMP, NOJMP);
}


struct LoopCtx { /* loop context */
    struct LoopCtx *prev;
    Scope *loopscope;
    int loopstart;
};


/* set loop scope context to current values */
static void initloopctx(FunctionState *fs, struct LoopCtx *ctx) {
    ctx->prev = NULL;
    ctx->loopscope = fs->loopscope;
    ctx->loopstart = fs->loopstart;
}


/* store 'ctx' or load 'currctx' */
static void handleloopctx(FunctionState *fs, struct LoopCtx *ctx, int store) {
    static struct LoopCtx *currctx = NULL;
    if (store) { /* store 'ctx' */
        ctx->prev = currctx;
        currctx = ctx;
    } else { /* load 'currctx' */
        cs_assert(ctx == NULL); /* please provide NULL for clarity */
        fs->loopscope = currctx->loopscope;
        fs->loopstart = currctx->loopstart;
        currctx = currctx->prev;
    }
}


/* start loop scope */
static void startloop(FunctionState *fs, Scope *s, struct LoopCtx *ctx,
                      int cfbits) {
    open_scope(fs, s, cfbits);
    initloopctx(fs, ctx);
    handleloopctx(fs, ctx, 1);
    fs->loopscope = fs->scope;
    fs->loopstart = currentpc(fs);
}


/* end loop scope */
static void endloop(FunctionState *fs) {
    cs_assert(sisloop(fs->scope));
    close_scope(fs);
    handleloopctx(fs, NULL, 0); /* load old loop ctx values */
}


/* whilestm ::= 'while' '(' expr ')' condbody */
static void whilestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    DynCtx startctx;
    struct LoopCtx lctx;
    Scope s; /* new 'loopscope' */
    ExpInfo cond;
    initexp(&cond, EXP_VOID, 0);
    csY_scan(lx); /* skip 'while' */
    storectx(fs, &startctx);
    startloop(fs, &s, &lctx, CFLOOP);
    int pcexpr = currentpc(fs);
    int matchline = lx->line;
    expectnext(lx, '(');
    expr(lx, &cond);
    codepres_exp(fs, &cond);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &startctx, &cond, OP_TESTPOP, OP_JMPS, pcexpr, NOJMP);
    endloop(fs);
}


/* patch for loop jump */
static void patchforjmp(FunctionState *fs, int pc, int target, int back) {
    Instruction *jmp = &fs->p->code[pc];
    int offset = target - pc;
    if (back)
        offset = -offset;
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
        limiterror(lx->fs, "generic for loop expressions", limit);
    return nexpr;
}


/* foreachloop ::= 'for' 'each' idlist 'in' forexprlist '{' stmlist '}' */
static void foreachloop(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int nvars = 1; /* iter func result */
    int base = fs->sp;
    DynCtx startctx;
    struct LoopCtx lctx;
    Scope s;
    int forend;
    ExpInfo e;
    initexp(&e, EXP_VOID, 0);
    storectx(fs, &startctx);
    newlocallit(lx, "(for state)"); /* iterator         (base)   */
    newlocallit(lx, "(for state)"); /* invariant state  (base+1) */
    newlocallit(lx, "(for state)"); /* control var      (base+2) */
    newlocallit(lx, "(for state)"); /* to-be-closed var (base+3) */
    /* create declared variables */
    newlocalvar(lx, str_expectname(lx));
    while (match(lx, ',')) {
        newlocalvar(lx, str_expectname(lx));
        nvars++;
    }
    expectnext(lx, TK_IN);
    adjustassign(lx, NSTATEVARS, forexplist(lx, &e, NSTATEVARS), &e);
    adjustlocals(lx, NSTATEVARS); /* register control variables */
    scopemarkclose(fs); /* last control variable must be closed */
    /* runtime space for call (iterator, invariant state and control var) */
    csC_checkstack(fs, 3);
    int prep = csC_emitILL(fs, OP_FORPREP, base, 0);
    startloop(fs, &s, &lctx, CFLOOP); /* scope for declared variables */
    adjustlocals(lx, nvars); /* register declared variables */
    csC_reserveslots(fs, nvars); /* space for declared variables */
    stm(lx); /* body */
    endloop(fs); /* end scope for declared vars */
    patchforjmp(fs, prep, currentpc(fs), 0);
    csC_emitILL(fs, OP_FORCALL, base, nvars);
    forend = csC_emitILL(fs, OP_FORLOOP, base, 0);
    patchforjmp(fs, forend, prep + getOpSize(OP_FORPREP), 1);
}


/* 'for' loop initializer */
void forinit(Lexer *lx) {
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
void forcond(Lexer *lx, ExpInfo *e) {
    if (!match(lx, ';')) { /* have condition? */
        expr(lx, e); /* get it... */
        codepres_exp(lx->fs, e); /* ...and put it on stack (preserve) */
        expectnext(lx, ';');
    } else { /* otherwise no condition (infinite loop) */
        e->et = EXP_VOID; /* indicate it */
    }
}


/* 'for' loop last clause */
void forendclause(Lexer *lx, ExpInfo *cond, int *clausepc) {
    FunctionState *fs = lx->fs;
    int bodyjmp, loopjmp;
    cs_assert(*clausepc == NOJMP);
    if (check(lx, ')')) { /* no end clause ? */
        return; /* done; will be converted to 'while' loop */
    } else {
        int inf = eistrue(cond) || cond->et == EXP_VOID;
        if (!inf) /* loop is not infinite? */
            bodyjmp = csC_jmp(fs, OP_JMP); /* insert jump in-between */
        *clausepc = currentpc(fs); /* update end clause pc */
        exprstm(lx); /* get the end clause expression statement */
        if (!inf) { /* loop is not infinite? */
            loopjmp = csC_jmp(fs, OP_JMPS); /* emit jump back to cond... */
            csC_patch(fs, loopjmp, fs->loopstart); /* ...and patch it */
            csC_patchtohere(fs, bodyjmp); /* patch jump from cond to body */
        }
        fs->loopstart = *clausepc; /* loop starts at end clause pc... */
        /* ...and then it jumps to cond */
    }
}


/* forloop ::= 'for' '(' forinit ';' forcond ';' forendclause ')' condbody */
static void forloop(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int matchline = lx->line;
    int condpc, endclausepc;
    DynCtx startctx;
    struct LoopCtx lctx;
    Scope s; /* new 'loopscope' */
    ExpInfo cond;
    initexp(&cond, EXP_VOID, 0);
    startloop(fs, &s, &lctx, CFLOOP); /* enter loop */
    expectnext(lx, '(');
    forinit(lx); /* get for loop initializer */
    storectx(fs, &startctx); /* store context at start of for loop */
    condpc = currentpc(fs);
    forcond(lx, &cond); /* get for loop condition expression */
    endclausepc = NOJMP;
    forendclause(lx, &cond, &endclausepc); /* get for last clause */
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &startctx, &cond, OP_TESTPOP, OP_JMPS, condpc, endclausepc);
    endloop(fs); /* end/leave loop */
}


/* 
** forstm ::= foreachloop
**          | forloop
*/
static void forstm(Lexer *lx) {
    csY_scan(lx); /* skip 'for' */
    if (match(lx, TK_EACH)) /* generic loop? */
        foreachloop(lx);
    else /* standard C-like loop */
        forloop(lx);
}


/* loopstm ::= 'loop' stm */
static void loopstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopCtx ctx;
    Scope s;
    int jmp, lstart;
    csY_scan(lx); /* skip 'loop' */
    lstart = currentpc(fs); /* store the pc where the loop starts */
    startloop(fs, &s, &ctx, CFLOOP);
    stm(lx);
    jmp = csC_jmp(fs, OP_JMPS);
    csC_patch(fs, jmp, lstart);
    endloop(fs);
}


/* continuestm ::= 'continue' ';' */
static void continuestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int old_sp = fs->sp;
    int extra, jmp;
    csY_scan(lx); /* skip 'continue' */
    if (c_unlikely(fs->loopstart == NOJMP)) { /* not in a loop? */
        cs_assert(fs->loopscope == NULL); /* then there is no loop scope */
        csP_semerror(lx, "'continue' not in loop statement");
    }
    cs_assert(fs->loopscope != NULL); /* must have loop scope */
    /* pop (but do not remove) locals and switch expressions */
    extra = fs->scope->nswscope - fs->loopscope->nswscope;
    poplocals(fs, fs->loopscope->activelocals, extra);
    fs->sp = old_sp; /* restore old 'sp', as parser is not jumping back */
    if (fs->scope->havetbcvar) { /* have to close upvalues? */
        int stklevel = stacklevel(fs, fs->loopscope->activelocals);
        csC_emitIS(fs, OP_CLOSE, stklevel); /* close them */
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
    Scope *s = fs->scope; /* current scope */
    const Scope *cfs = getcfscope(fs); /* control flow scope */
    csY_scan(lx); /* skip 'break' */
    if (c_unlikely(cfs == NULL)) /* no control flow scope? */
        csP_semerror(lx, "'break' not in loop or switch statement");
    patchlistadd(lx, csC_jmp(fs, OP_JMP), s->havetbcvar);
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
    initexp(&e, EXP_VOID, 0);
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


static void freevalueslots(FunctionState *fs) {
    cs_assert(fs->p->maxstack >= fs->sp);
    cs_assert(fs->sp >= nvarstack(fs));
    fs->sp = nvarstack(fs); /* leave only locals */
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
    freevalueslots(lx->fs);
    leaveCstack(lx);
}


static void stm(Lexer *lx) {
    enterCstack(lx);
    stm_(lx);
    freevalueslots(lx->fs);
    leaveCstack(lx);
}


/* compile main function */
static void mainfunc(FunctionState *fs, Lexer *lx) {
    Scope s;
    open_func(lx, fs, &s);
    setvararg(fs, 0); /* main function is always vararg */
    csY_scan(lx); /* scan first token */
    listdecl(lx, 0);
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
    cs_assert(ps->actlocals.len == 0); /* all scopes should be finished */
    ts->sp.p--; /* remove scanner table */
    return cl;
}
