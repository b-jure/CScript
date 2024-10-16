/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crcode.h"
#include "crconf.h"
#include "crgc.h"
#include "crlexer.h"
#include "crlimits.h"
#include "crobject.h"
#include "crparser.h"
#include "crstate.h"
#include "crobject.h"
#include "crstring.h"
#include "crvm.h"
#include "crfunction.h"
#include "crhashtable.h"
#include "crmem.h"

#include <string.h>


/* enter C function frame */
#define enterCstack(lx)         crT_incC_((lx)->ts)

/* pop C function frame */
#define leaveCstack(lx)         ((lx)->ts->nCC--)


/* compare 'OString' pointers for equality */
#define streq(s1,s2)        ((s1) == (s2))


/* expect 'cond' to be true or invoke error */
#define expect_cond(lx, cond, err) \
    { if (!(cond)) crL_syntaxerror(lx, err); }


/* 
 * Mask for marking undefined or not fully initialized
 * variables (private globals).
 */
#define UNDEFMODMASK     0x80 /* 0b10000000 */


/* 'cfbits' */
#define CFLOOP      0 /* 'for' loop */
#define CFSWITCH    1 /* 'switch' */

/* test 'cfbits' */
#define scopeisloop(s)      testbit((s)->cfbits, CFLOOP)
#define scopeisswitch(s)    testbit((s)->cfbits, CFSWITCH)


/* lexical scope information */
typedef struct Scope {
    struct Scope *prev; /* implicit linked-list */
    int activelocals; /* number of locals outside of this scope */
    int nswscope; /* number of switch scopes before this scope */
    int depth; /* NOTE(jure): might remove, only used for O(1) in 'breakstm'  */
    cr_ubyte cfbits; /* control flow bits */
    cr_ubyte haveupval; /* set if scope contains upvalue variable */
    cr_ubyte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;


static void storectx(FunctionState *fs, DynCtx *ctx) {
    ctx->loopstart = fs->loopstart;
    ctx->sp = fs->sp;
    ctx->nfuncs = fs->nfuncs;
    ctx->nk = fs->nk;
    ctx->nprivate = fs->nprivate;
    ctx->pc = fs->pc;
    ctx->nlinfo = fs->nlinfo;
    ctx->nlocals = fs->nlocals;
    ctx->nupvals = fs->nupvals;
    ctx->nbrks = fs->patches.len;
    ctx->needclose = fs->needclose;
}


static void loadctx(FunctionState *fs, DynCtx *ctx) {
    fs->loopstart = ctx->loopstart;
    fs->sp = ctx->sp;
    fs->nfuncs = ctx->nfuncs;
    fs->nk = ctx->nk;
    fs->nprivate = ctx->nprivate;
    fs->pc = ctx->pc;
    fs->nlinfo = ctx->nlinfo;
    fs->nlocals = ctx->nlocals;
    fs->nupvals = ctx->nupvals;
    fs->patches.len = ctx->nbrks;
    fs->needclose = ctx->needclose;
}


/* 
 * If 'deadcode' is not yet stored, store the current state
 * into it.
 */
static void storereachablectx(FunctionState *fs) {
    if (fs->deadcode.pc == NOJMP) /* 'deadcode' empty ? */
        storectx(fs, &fs->deadcode);
}


/* load context right before dead code */
static void loadreachablectx(FunctionState *fs) {
    cr_assert(fs->deadcode.pc >= 0);
    loadctx(fs, &fs->deadcode);
}


/* pop last pending jump from currently active patch list */
static int patchlistpop(FunctionState *fs) {
    cr_assert(fs->patches.len > 0);
    PatchList *list = &fs->patches.list[fs->patches.len - 1];
    if (list->len)
        return list->arr[--list->len];
    else
        return NOJMP;
}


/* add jump to patch list (for backpatching) */
static void patchlistaddjmp(FunctionState *fs, int jmp) {
    cr_assert(fs->patches.len > 0);
    PatchList *list = &fs->patches.list[fs->patches.len - 1];
    crM_growvec(fs->lx->ts, list->arr, list->size, list->len,
                INT_MAX, "code jumps", int);
    list->arr[list->len++] = jmp;
}


/* reset patch list length */
static void resetpatchlist(FunctionState *fs) {
    cr_assert(fs->patches.len > 0);
    fs->patches.list[fs->patches.len - 1].len = 0;
}


/* create new patch list */
static void patchliststart(FunctionState *fs) {
    crM_growvec(fs->lx->ts, fs->patches.list, fs->patches.size, fs->patches.len,
                INT_MAX, "patch lists", PatchList);
    fs->patches.list[fs->patches.len++] = (PatchList){0};
}


/* end patch list by patching all pending jumps */
static void patchlistend(FunctionState *fs) {
    cr_assert(fs->patches.len > 0);
    PatchList *list = &fs->patches.list[--fs->patches.len];
    int jmp; /* 'break' jmp */
    while ((jmp = patchlistpop(fs)) != NOJMP)
        crC_patchtohere(fs, jmp);
    cr_assert(list->len == 0);
    crM_freearray(fs->lx->ts, list->arr, list->size, int);
}


static cr_noret expecterror(Lexer *lx, int tk) {
    const char *err = crS_pushfstring(lx->ts, "expected %s",
                                            crL_tok2str(lx, tk));
    crL_syntaxerror(lx, err);
}


static cr_noret limiterror(FunctionState *fs, const char *what, int limit) {
    cr_State *ts = fs->lx->ts;
    int line = fs->fn->defline;
    const char *where = (line == 0 ? "main function" :
                        crS_pushfstring(ts, "function at line %d", line));
    const char *err = crS_pushfstring(ts, "too many %s (limit is %d) in %s",
                                          what, limit, where);
    crL_syntaxerror(fs->lx, err);
}


/* semantic error; variant of syntax error without 'near <token>' */
cr_noret crP_semerror(Lexer *lx, const char *err) {
    lx->t.tk = 0;
    crL_syntaxerror(lx, err);
}


static void checklimit(FunctionState *fs, int n, int limit, const char *what) {
    if (n >= limit)
        limiterror(fs, what, limit);
}


/* variable initialization error */
static cr_noret variniterror(Lexer *lx, const char *kind, const char *name) {
    crP_semerror(lx, crS_pushfstring(lx->ts,
                     "can't read %s variable '%s' in its own initializer",
                     kind, name));
}


/* check for variable collision */
static void checkvarcollision(FunctionState *fs, OString *name, const char *vk,
                              int (*fn)(FunctionState *, OString *, ExpInfo *))
{
    ExpInfo dummy;
    if (cr_unlikely(fn(fs, name, &dummy) >= 0))
        crP_semerror(fs->lx, crS_pushfstring(fs->lx->ts,
                             "redefinition of %s variable '%s'", vk, name));
}


/* get local variable */
static LVar *getlocal(FunctionState *fs, int idx) {
    cr_assert(fs->firstlocal + idx < fs->lx->ps->lvars.len);
    return &fs->lx->ps->lvars.arr[fs->firstlocal + idx];
}


/* get local variable debug information */
static LVarInfo *getlocalinfo(FunctionState *fs, int vidx) {
    cr_assert(0 <= vidx && vidx <= fs->activelocals);
    LVar *local = getlocal(fs, vidx);
    return &fs->fn->locals[local->s.idx];
}


/*
 * Convert 'nvar', a compiler index level, to its 
 * corresponding stack position.
 */
static int getstacklevel(FunctionState *fs, int nvar) {
    return getlocal(fs, --nvar)->s.idx + 1;
}


/* 
 * Pop local variables up to stack level 'tolevel'; 'extra'
 * values to pop are usually remaining switch statement
 * values or any other values left on stack besides
 * local variables.
 */
static void poplocals(FunctionState *fs, int tolevel, int extra) {
    int popn = fs->activelocals - tolevel;
    fs->lx->ps->lvars.len -= popn;
    popn += extra;
    crC_pop(fs, popn);
    fs->sp -= popn;
}


/* 
 * Remove and pop local variables up to 'tolevel';
 * 'popextra' is used as 'extra' in 'poplocals'.
 */
static void removelocals(FunctionState *fs, int tolevel, int popextra) {
    poplocals(fs, tolevel, popextra);
    while (fs->activelocals > tolevel) { /* set debug lifetime pc */
        LVarInfo *locinfo = getlocalinfo(fs, --fs->activelocals);
        locinfo->endpc = fs->pc;
    }
}


/* get private variable */
cr_sinline PrivateVar *getprivate(FunctionState *fs, int idx) {
    cr_assert(0 <= idx && idx < fs->nprivate);
    return &fs->fn->private[idx];
}


/* get upvalue variable */
cr_sinline UpValInfo *getupvalue(FunctionState *fs, int idx) {
    cr_assert(idx < fs->nupvals);
    return &fs->fn->upvals[idx];
}


/* move constant expression to value 'v' */
static void movexp2v(FunctionState *fs, ExpInfo *e, TValue *v) {
    switch (e->et) {
    case EXP_NIL: setnilval(v); break;
    case EXP_FALSE: setbfval(v); break;
    case EXP_TRUE: setbtval(v); break;
    case EXP_STRING: setstrval(NULL, v, e->u.str); break;
    case EXP_INT: setival(v, e->u.i); break;
    case EXP_FLT: setfval(v, e->u.n); break;
    case EXP_K: *v = *getconstant(fs, e); break;
    default: cr_unreachable();
    }
}


/* init expression with generic information */
static void initexp(ExpInfo *e, expt et, int info) {
    e->t = e->f = NOJMP;
    e->et = et;
    e->u.info = info;
}


/* init global variable expression */
static void initglobal(ExpInfo *e, OString *name) {
    e->t = e->f = NOJMP;
    e->et = EXP_GLOBAL;
    e->u.str = name;
}


static void initstring(ExpInfo *e, OString *s) {
    e->f = e->t = NOJMP;
    e->et = EXP_STRING;
    e->u.str = s;
}


/* add local debug information into 'locals' */
static int registerlocal(Lexer *lx, FunctionState *fs, OString *name) {
    Function *fn = fs->fn;
    checklimit(fs, fs->nlocals, MAXLONGARGSIZE, "locals");
    crM_growvec(lx->ts, fn->locals, fn->sizelocals, fs->nlocals,
                MAXLONGARGSIZE, "locals", LVarInfo);
    LVarInfo *lvarinfo = &fn->locals[fs->nlocals++];
    lvarinfo->name = name;
    lvarinfo->startpc = fs->pc;
    return fs->nlocals - 1;
}


/*
 * Adjust locals by increment 'nlocals' and registering them
 * inside 'locals'.
 */
static void adjustlocals(Lexer *lx, int nvars) {
    FunctionState *fs = lx->fs;
    for (int i = 0; i < nvars; nvars--) {
        int idx = fs->activelocals++;
        LVar *local = getlocal(fs, idx);
        local->s.idx = registerlocal(lx, fs, local->s.name);
    }
}


/* 
 * Define private variable by removing UNDEFMODMASK.
 * UNDEFMODMASK is not contained in VARBITMASK. 
 */ 
static void defineprivate(FunctionState *fs, int vidx) {
    PrivateVar *pv = getprivate(fs, vidx);
    pv->val.mod &= VARBITMASK;
}


/* define global variable */
static void defineglobal(FunctionState *fs, ExpInfo *var) {
    cr_assert(var->et == EXP_GLOBAL);
    crC_defineglobal(fs, var);
}


/* initialize non-global variable */
static void initvariable(FunctionState *fs, ExpInfo *var) {
    switch (var->et) {
    case EXP_LOCAL: {
        adjustlocals(fs->lx, 1);
        break;
    }
    case EXP_PRIVATE: {
        defineprivate(fs, var->u.info);
        break;
    }
    default: break;
    }
}


/* start lexical scope */
static void startscope(FunctionState *fs, Scope *s, int cfbits) {
    if (cfbits & (CFLOOP | CFSWITCH)) /* needs a patch list ? */
        patchliststart(fs); /* 'break' statement jumps storage */
    s->activelocals = fs->activelocals;
    s->nswscope = fs->scope->nswscope + scopeisswitch(fs->scope);
    s->depth = fs->scope->depth + 1;
    s->cfbits = cfbits;
    s->haveupval = 0;
    s->havetbcvar = (fs->scope != NULL && fs->scope->havetbcvar);
    s->prev = fs->scope;
    fs->scope = s;
}


/* end lexical scope */
static void endscope(FunctionState *fs) {
    Scope *s = fs->scope;
    int stklevel = getstacklevel(fs, s->activelocals);
    removelocals(fs, s->activelocals, scopeisswitch(s));
    cr_assert(s->activelocals == fs->activelocals);
    if (scopeisloop(s) || scopeisswitch(s)) /* has a patch list ? */
        patchlistend(fs);
    if (s->prev && s->haveupval) /* need to close upvalues ? */
        crC_emitIS(fs, OP_CLOSE, stklevel);
    fs->sp = stklevel; /* free stack slots */
    fs->scope = s->prev;
}


/* 
 * Mark scope where variable at idx 'vidx' was defined
 * in order to emit close instruction before the scope
 * gets closed.
 */
static void scopemarkupval(FunctionState *fs, int vidx) {
    Scope *s = fs->scope;
    while(s->activelocals - 1 > vidx)
        s = s->prev;
    s->haveupval = 1;
    fs->needclose = 1;
}


/* 
 * Mark current scope as scopee that has a to-be-closed
 * variable.
 */
static void scopemarktbc(FunctionState *fs) {
    Scope *s = fs->scope;
    s->haveupval = 1;
    s->havetbcvar = 1;
    fs->needclose = 1;
}


/* initialize function state */
static void startfs(FunctionState *fs, Lexer *lx, Scope *s) {
    cr_assert(fs->fn != NULL);
    fs->prev = lx->fs;
    fs->lx = lx;
    lx->fs = fs;
    fs->scope = fs->loopscope = fs->switchscope = NULL;
    fs->loopstart = NOJMP;
    fs->sp = 0;
    fs->activelocals = 0;
    fs->firstlocal = lx->ps->lvars.len;
    fs->nfuncs = 0;
    fs->nk = 0;
    fs->nprivate = 0;
    fs->pc = 0;
    fs->nlinfo = 0;
    fs->nlocals = 0;
    fs->nupvals = 0;
    fs->deadcode.pc = NOJMP;
    fs->patches.len = fs->patches.size = 0; fs->patches.list = NULL;
    fs->needclose = fs->lastwasret = 0;
    fs->fn->source = lx->src;
    crG_objbarrier(lx->ts, fs->fn, fs->fn->source);
    fs->fn->maxstack = 1; /* for 'self' */
    startscope(fs, s, 0); /* start global scope */
}


/* cleanup function state */
static void endfs(FunctionState *fs) {
    Lexer *lx = fs->lx;
    Function *fn = fs->fn;
    cr_State *ts = lx->ts;
    crC_ret(fs, fs->activelocals - 1, 0);
    cr_assert(fs->scope && !fs->scope->prev);
    endscope(fs); /* end global scope */
    cr_assert(!fs->scope);
    if (fs->deadcode.pc != NOJMP) /* have dead code ? */
        loadreachablectx(fs);
    /* preserve memory; shrink unused space */
    /* by using counters in 'fs' as final size */
    crM_shrinkvec(ts, fn->funcs, fn->sizefn, fs->nfuncs, Function);
    crM_shrinkvec(ts, fn->k, fn->sizek, fs->nk, TValue);
    crM_shrinkvec(ts, fn->private, fn->sizeprivate, fs->nprivate, PrivateVar);
    crM_shrinkvec(ts, fn->code, fn->sizecode, fs->pc, Instruction);
    crM_shrinkvec(ts, fn->linfo, fn->sizelinfo, fs->nlinfo, LineInfo);
    crM_shrinkvec(ts, fn->locals, fn->sizelocals, fs->nlocals, LVarInfo);
    crM_shrinkvec(ts, fn->upvals, fn->sizeupvals, fs->nupvals, UpValInfo);
    lx->fs = fs->prev;
    crG_check(ts);
}


/* add function */
static Function *addfunction(Lexer *lx) {
    Function *new;
    cr_State *ts = lx->ts;
    FunctionState *fs = lx->fs;
    Function *fn = fs->fn;
    checklimit(fs, fs->nfuncs + 1, MAXLONGARGSIZE, "functions");
    crM_growvec(ts, fn->funcs, fn->sizefn, fs->nfuncs, MAXLONGARGSIZE,
                "functions", Function);
    fn->funcs[fs->nfuncs++] = new = crF_new(ts);
    crG_objbarrier(ts, fn, new);
    return new;
}


/* set current function as vararg */
static void setvararg(FunctionState *fs, int arity) {
    fs->fn->isvararg = 1;
    crC_emitIL(fs, OP_VARARGPREP, arity);
}



/* forward declare, can be both part of stm and expr */
static void funcbody(Lexer *lx, ExpInfo *v, int linenum, int ismethod);

/* forward declare recursive non-terminals */
static void decl(Lexer *lx);
static void stm(Lexer *lx);
static void expr(Lexer *lx, ExpInfo *e);



/* check if current token matches 'tk' */
#define check(lx, tok)       ((lx)->t.tk == (tok))


/* 
 * Advance scanner if 'tk' matches the current token,
 * otherwise return 0. 
 */
static int match(Lexer *lx, int tk) {
    if (check(lx, tk)) {
        crL_scan(lx);
        return 1;
    }
    return 0;
}


/* check if 'tk' matches the current token */
static void expect(Lexer *lx, int tk) {
    if (check(lx, tk)) {
        crL_scan(lx);
        return;
    }
    expecterror(lx, tk);
}


/*
 * Check that next token is 'what'. 
 * Otherwise raise an error that the expected 'what' should 
 * match a 'who' in line 'linenum'.
 */
static void expectmatch(Lexer *lx, int what, int who, int linenum) {
    if (cr_unlikely(!match(lx, what))) {
        if (lx->line == linenum) { /* same line ? */
            expecterror(lx, what); /* emit usual error message */
        } else {
            crL_syntaxerror(lx, crS_pushfstring(lx->ts,
            "%s expected (to close %s at line %d)",
            crL_tok2str(lx, what), crL_tok2str(lx, who), linenum));
        }
    }
}


static OString *expect_id(Lexer *lx) {
    expect(lx, TK_IDENTIFIER);
    return lx->t.lit.str;
}


/* adds local variable to the 'lvars' */
static int newlocal(Lexer *lx, OString *name, int mods) {
    FunctionState *fs = lx->fs;
    ParserState *ps = lx->ps;
    checklimit(fs, ps->lvars.len, MAXLONGARGSIZE, "locals");
    crM_growvec(lx->ts, ps->lvars.arr, ps->lvars.size, ps->lvars.len,
                MAXLONGARGSIZE, "locals", LVar);
    LVar *local = &ps->lvars.arr[ps->lvars.len++];
    local->val.mod = mods;
    local->s.name = name;
    local->s.idx = -1;
    return ps->lvars.len - fs->firstlocal - 1;
}


#define newlocallit(lx,lit) \
    newlocal(lx, crL_newstring(lx, "" lit, SLL(lit)), 0)


/*
 * Searches for local variable 'name'.
 */
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e) {
    Lexer *lx = fs->lx;
    for (int i = fs->activelocals - 1; 0 <= i; i--) {
        LVar *local = getlocal(fs, i);
        if (streq(name, local->s.name)) {
            if (cr_unlikely(local->s.idx == -1))
                variniterror(lx, "local", getstrbytes(name));
            initexp(e, EXP_LOCAL, i);
            return e->et;
        }
    }
    return -1;
}


/*
 * Searches for private variable 'name'.
 */
static int searchprivate(FunctionState *fs, OString *name, ExpInfo *e) {
    Lexer *lx = fs->lx;
    for (int i = fs->nprivate - 1; i >= 0; i--) {
        PrivateVar *pv = getprivate(fs, i);
        if (streq(name, pv->s.name)) {
            if (cr_unlikely(testbits(pv->val.mod, UNDEFMODMASK)))
                variniterror(lx, "private", getstrbytes(name));
            initexp(e, EXP_PRIVATE, i);
            return e->et;
        }
    }
    return -1;
}


/* allocate space for new 'UpValInfo' */
static UpValInfo *newupvalue(FunctionState *fs) {
    Function *fn = fs->fn;
    cr_State *ts = fs->lx->ts;
    checklimit(fs, fs->nupvals + 1, MAXLONGARGSIZE, "upvalues");
    crM_growvec(ts, fn->upvals, fn->sizeupvals, fs->nupvals, MAXLONGARGSIZE,
                "upvalues", UpValInfo);
    return &fn->upvals[fs->nupvals++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *e) {
    UpValInfo *uv = newupvalue(fs);
    uv->name = name;
    if (e->et == EXP_LOCAL) { /* local ? */
        uv->onstack = 1;
        uv->idx = e->u.info;
        uv->mod = getlocal(fs, e->u.info)->val.mod;
        cr_assert(streq(name, getlocal(fs, e->u.info)->s.name));
    } else { /* must be upvalue */
        cr_assert(e->et == EXP_UVAL);
        FunctionState *enclosing = fs->prev;
        uv->onstack = 0;
        uv->idx = e->u.info;
        uv->mod = enclosing->fn->upvals[e->u.info].mod;
        cr_assert(streq(name, enclosing->fn->upvals[e->u.info].name));
    }
    return fs->nupvals - 1;
}


/* searches for upvalue 'name' */
static int searchupvalue(FunctionState *fs, OString *name) {
    Function *fn = fs->fn;
    for (int i = 0; i < fs->nupvals; i++)
        if (streq(fn->upvals[i].name, name)) 
            return i;
    return -1;
}


/*
 * Search for variable; if 'name' is not local variable
 * try finding the upvalue, otherwise assume it is global.
 */
static void searchvar(FunctionState *fs, OString *name, ExpInfo *e, int base) {
    if (fs == NULL) { /* global ? */
        initexp(e, EXP_VOID, 0);
    } else {
        int ret = searchlocal(fs, name, e);
        if (ret >= 0) { /* local ? */
            if (ret == EXP_LOCAL && !base)
                scopemarkupval(fs, e->u.info);
        } else { /* try upvalue */
            ret = searchupvalue(fs, name);
            if (ret < 0) {
                searchvar(fs->prev, name, e, 0); /* try enclosing 'fs' */
                if (e->et == EXP_LOCAL || e->et == EXP_UVAL)
                    ret = addupvalue(fs, name, e);
                else 
                    return;
            }
            initexp(e, EXP_UVAL, ret);
        }
    }
}


/* create new global variable */
static void newglobal(Lexer *lx, OString *name, int mods) {
    TValue k, val;
    setstrval(lx->ts, &k, name);
    setemptyval(&val);
    val.mod = mods;
    crH_set(lx->ts, G_(lx->ts)->globals, &k, &val);
}


/* get global variable 'name' or create undefined global */
static void globalvar(FunctionState *fs, OString *name, ExpInfo *e) {
    TValue k;
    Lexer *lx = fs->lx;
    setstrval(lx->ts, &k, name);
    if (isabstkey(crH_get(G_(lx->ts)->globals, &k)))
        newglobal(lx, name, UNDEFMODMASK >> (fs->prev != NULL));
    e->u.str = name;
    e->et = EXP_GLOBAL;
}


/* create new static variable */
static int newprivate(FunctionState *fs, OString *name, int mods) {
    Function *fn = fs->fn;
    cr_State *ts = fs->lx->ts;
    checklimit(fs, fs->nprivate, MAXLONGARGSIZE, "private variables");
    crM_growvec(ts, fn->private, fn->sizeprivate, fs->nprivate,
                MAXLONGARGSIZE, "private variables", PrivateVar);
    PrivateVar *pv = &fn->private[fs->nprivate++];
    pv->s.name = name;
    pv->val.mod = (UNDEFMODMASK | mods);
    return fs->nprivate - 1;
}


/* find variable 'name' */
static void var(Lexer *lx, OString *name, ExpInfo *e) {
    searchvar(lx->fs, name, e, 1);
    if (e->et == EXP_VOID)
        globalvar(lx->fs, name, e);
}


#define varlit(lx,l,e)      var(lx, crL_newstring(lx, "" l, SLL(l)), e)



/* -------------------------------------------------------------------------
 *                              EXPRESSIONS
 * ------------------------------------------------------------------------- */

/* varprivate ::= '@' name */
static void varprivate(Lexer *lx, ExpInfo *e) {
    crL_scan(lx); /* skip '@' */
    OString *name = expect_id(lx);
    int vidx = searchprivate(lx->fs, name, e);
    if (cr_unlikely(vidx < 0))
        crP_semerror(lx, crS_pushfstring(lx->ts, 
                         "undefined static variable '%s'", name));
    initexp(e, EXP_PRIVATE, vidx);
}


static void expid(Lexer *lx, ExpInfo *e) {
    initstring(e, expect_id(lx));
}


/*
 * self ::= 'self'
 */
static void selfkw(Lexer *lx, ExpInfo *e) {
    if (lx->ps->cs != NULL) {
        varlit(lx, "self", e);
        crL_scan(lx);
        return;
    }
    crL_syntaxerror(lx, "can't use 'self' outside of class declaration");
}


/*
 * exprlist ::= expr
 *            | expr ',' exprlist
 */
static int exprlist(Lexer *lx, ExpInfo *e) {
    int n = 1;
    expr(lx, e);
    while (match(lx, ',')) {
        crC_exp2stack(lx->fs, e);
        expr(lx, e);
        n++;
    }
    return n;
}


/* indexed ::= '[' expr ']' */
static void indexed(Lexer *lx, ExpInfo *var, int super) {
    crL_scan(lx); /* skip '[' */
    if (cr_unlikely(eisconstant(var)))
        crP_semerror(lx, "can't index literal constant values");
    crC_exp2stack(lx->fs, var);
    ExpInfo key;
    expr(lx, &key);
    crC_indexed(lx->fs, var, &key, super);
    expect(lx, ']');
}


/* getprop ::= '.' id */
static void getprop(Lexer *lx, ExpInfo *var, int super) {
    crL_scan(lx); /* skip '.' */
    crC_exp2stack(lx->fs, var);
    ExpInfo key;
    expid(lx, &key);
    crC_getproperty(lx->fs, var, &key, super);
    crL_scan(lx);
}


/* 
 * superkw ::= 'super' '.' id 
 *           | 'super' '[' id ']'
 *           | 'super' '[' string ']' 
 */
static void superkw(Lexer *lx, ExpInfo *e) {
    if (cr_unlikely(lx->ps->cs == NULL)) {
        crP_semerror(lx, "'super' outside of method");
    } else if (cr_unlikely(!lx->ps->cs->super)) {
        crL_syntaxerror(lx, "use of 'super' but class does not inherit");
    } else {
        FunctionState *fs = lx->fs;
        varlit(lx, "self", e);
        cr_assert(e->et == EXP_LOCAL);
        crC_exp2stack(fs, e);
        varlit(lx, "super", e);
        cr_assert(e->et == EXP_UVAL);
        crL_scan(lx); /* skip 'super' */
        if (check(lx, '['))
            indexed(lx, e, 1);
        else if (check(lx, '.')) 
            getprop(lx, e, 1);
        else 
            crL_syntaxerror(lx, "'super' expects '.' or '['");
    }
}


/*
 * primaryexp ::= '(' expr ')'
 *              | id
 *              | selfkw
 *              | superkw
 */
static void primaryexp(Lexer *lx, ExpInfo *e) {
    switch (lx->t.tk) {
    case '(':
        crL_scan(lx); /* skip ')' */
        expr(lx, e);
        expect(lx, ')');
        crC_varexp2stack(lx->fs, e);
        break;
    case '@':
        varprivate(lx, e);
        crL_scan(lx);
        break;
    case TK_IDENTIFIER:
        var(lx, e->u.str, e);
        crL_scan(lx);
        break;
    case TK_SELF:
        selfkw(lx, e);
        break;
    case TK_SUPER:
        superkw(lx, e);
        break;
    default:
        crL_syntaxerror(lx, "unexpected symbol");
        break;
    }
}


/* 
 * call ::= '(' ')'
 *        | '(' explist ')' 
 */
static void call(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int base = fs->sp;
    crC_exp2stack(fs, e);
    crL_scan(lx); /* skip '(' */
    if (lx->t.tk != ')') { /* have args ? */
        exprlist(lx, e);
        if (eismulret(e))
            crC_setreturns(fs, e, CR_MULRET);
    } else {
        e->et = EXP_VOID;
    }
    expect(lx, ')');
    int nparams;
    if (eismulret(e)) {
        nparams = CR_MULRET;
    } else {
        if (e->et != EXP_VOID)
            crC_exp2stack(lx->fs, e);
        nparams = fs->sp - base;
    }
    initexp(e, EXP_CALL, crC_call(fs, base, nparams));
    fs->sp = base + 1;
}


/*
 * suffixedexpr ::= primaryexp
 *               | primaryexp dotaccess
 *               | primaryexp call
 *               | primaryexp indexed
 */
static void suffixedexpr(Lexer *lx, ExpInfo *e) {
    primaryexp(lx, e);
    for (;;) {
        switch (lx->t.tk) {
        case '.':
            getprop(lx, e, 0);
            break;
        case '[':
            indexed(lx, e, 0);
            break;
        case '(':
            if (!eisconstant(e))
                crL_syntaxerror(lx, "tried calling constant value");
            call(lx, e);
            break;
        default:
            return;
        }
    }
}


/*
 * simpleexpr ::= int
 *              | flt
 *              | string
 *              | nil
 *              | true
 *              | false
 *              | '...'
 *              | suffixedexpr
 */
static void simpleexpr(Lexer *lx, ExpInfo *e) {
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
        expect_cond(lx, lx->fs->fn->isvararg,
                    "cannot use '...' outside of vararg function");
        initexp(e, EXP_VARARG, crC_emitIL(lx->fs, OP_VARARG, 2));
        break;
    }
    case TK_FN: {
        crL_scan(lx); /* skip 'fn' */
        funcbody(lx, e, lx->line, 0);
        return;
    }
    default:
        suffixedexpr(lx, e);
        return;
    }
    crL_scan(lx);
}


/* get unary operation matching 'token' */
static Unopr getunopr(int token) {
    switch (token) {
    case '-': return OPR_UMIN;
    case '~': return OPR_BNOT;
    case TK_NOT: return OPR_NOT;
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
    case TK_RANGE: return OPR_RANGE;
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
 * If 'left' == 'right' then operator is associative;
 * if 'left' < 'right' then operator is left associative;
 * if 'left' > 'right' then operator is right associative.
 */
static const struct {
    cr_ubyte left; /* left priority */
    cr_ubyte right; /* right priority */
} priority[] = {
    /* unary operators priority */
    [OPR_UMIN]      = { 14, 14 },   /* '-' */
    [OPR_BNOT]      = { 14, 14 },   /* '~' */
    [OPR_NOT]       = { 14, 14 },   /* 'not' */
    /* binary operators priority */
    [OPR_POW]       = { 16, 15 },   /* '**' */
    [OPR_MUL]       = { 13, 13 },   /* '*' */
    [OPR_DIV]       = { 13, 13 },   /* '/' */
    [OPR_MOD]       = { 13, 13 },   /* '%' */
    [OPR_ADD]       = { 12, 12 },   /* '+' */
    [OPR_SUB]       = { 12, 12 },   /* '-' */
    [OPR_SHR]       = { 11, 11 },   /* '>>' */
    [OPR_SHL]       = { 11, 11 },   /* '<<' */
    [OPR_LT]        = { 10, 10 },   /* '<' */
    [OPR_LE]        = { 10, 10 },   /* '<=' */
    [OPR_GT]        = { 10, 10 },   /* '>' */
    [OPR_GE]        = { 10, 10 },   /* '>=' */
    [OPR_EQ]        = {  9,  9 },   /* '==' */
    [OPR_NE]        = {  9,  9 },   /* '!=' */
    [OPR_BAND]      = {  7,  7 },   /* '&' */
    [OPR_BXOR]      = {  6,  6 },   /* '^' */
    [OPR_BOR]       = {  5,  5 },   /* '|' */
    [OPR_AND]       = {  4,  4 },   /* 'and' */
    [OPR_OR]        = {  3,  3 },   /* 'or' */
    [OPR_RANGE]     = {  2,  1 },   /* '..' */
    /* 0 is '=', which is non-associatve (can't chain '=') */
};


/*
 * subexpr ::= simpleexp
 *           | '-' simpleexp
 *           | '!' simpleexp
 *           | '~' simpleexp
 *           | simpleexp '+' subexpr
 *           | simpleexp '-' subexpr
 *           | simpleexp '*' subexpr
 *           | simpleexp '/' subexpr
 *           | simpleexp '%' subexpr
 *           | simpleexp '**' subexpr
 *           | simpleexp '>>' subexpr
 *           | simpleexp '<<' subexpr
 *           | simpleexp '==' subexpr
 *           | simpleexp '<' subexpr
 *           | simpleexp '<=' subexpr
 *           | simpleexp '>' subexpr
 *           | simpleexp '>=' subexpr
 *           | simpleexp '&' subexpr
 *           | simpleexp '^' subexpr
 *           | simpleexp '|' subexpr
 *           | simpleexp 'and' subexpr
 *           | simpleexp 'or' subexpr
 *           | simpleexp '..' subexpr
 */
static Binopr subexpr(Lexer *lx, ExpInfo *e, int limit) {
    enterCstack(lx);
    Unopr uopr = getunopr(lx->t.tk);
    if (uopr != OPR_NOUNOPR) {
        crL_scan(lx); /* skip operator */
        subexpr(lx, e, priority[uopr].right);
        crC_unary(lx->fs, e, uopr);
    } else {
        simpleexpr(lx, e);
    }
    Binopr opr = getbinopr(lx->t.tk);
    while (opr != OPR_NOBINOPR && priority[opr].left > limit) {
        crL_scan(lx); /* skip operator */
        crC_prebinary(lx->fs, e, opr);
        ExpInfo e2;
        Binopr next = subexpr(lx, &e2, priority[opr].right);
        crC_binary(lx->fs, e, &e2, opr);
        opr = next;
    }
    leaveCstack(lx);
    return opr;
}


/* expr ::= subexpr */
static void expr(Lexer *lx, ExpInfo *e) {
    subexpr(lx, e, 0);
}



/* -------------------------------------------------------------------------
 *                              STATEMENTS
 * ------------------------------------------------------------------------- */

/* check if 'var' is 'final' (read-only) */
static void checkreadonly(Lexer *lx, ExpInfo *var) {
    FunctionState *fs = lx->fs;
    OString *id = NULL;
    switch (var->et) {
    case EXP_UVAL: {
        UpValInfo *uv = getupvalue(fs, var->u.info);
        if (uv->mod & VARFINAL)
            id = uv->name;
        break;
    }
    case EXP_LOCAL: {
        LVar *lv = getlocal(fs, var->u.info);
        if (ismod(&lv->val, VARFINAL))
            id = lv->s.name;
        break;
    }
    case EXP_PRIVATE: {
        PrivateVar *sv = getprivate(fs, var->u.info);
        if (ismod(&sv->val, VARFINAL))
            id = sv->s.name;
        break;
    }
    case EXP_GLOBAL: {
        TValue key;
        const TValue *res;
        setstrval(lx->ts, &key, var->u.str);
        res = crH_get(G_(lx->ts)->globals, &key);
        if (!ttisempty(res) && ismod(res, VARFINAL))
            id = var->u.str;
        break;
    }
    default: return; /* only runtime knows :( */
    }
    if (id)
        crP_semerror(lx, crS_pushfstring(lx->ts, 
                         "attempt to assign to final variable '%s'", id));
}


/*
 * Used to chain variables on the left side of
 * the assignment.
 */
struct LHS {
    struct LHS *prev;
    ExpInfo e;
};


/* adjust left and right side of assignment */
static void adjustassign(Lexer *lx, int left, int right, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int need = left - right;
    if (eismulret(e)) {
        int extra = need + 1;
        if (extra < 0)
            extra = 0;
        crC_setreturns(fs, e, extra);
    } else {
        if (e->et != EXP_VOID)
            crC_exp2stack(fs, e);
        if (need > 0)
            crC_nil(fs, need);
    }
    if (need > 0) /* need to reserve stack slots ? */
        crC_reserveslots(fs, need);
    else
        fs->sp += need;
}


/* auxiliary function for multiple variable assignment */
static void assign(Lexer *lx, struct LHS *lhs, int nvars) {
    expect_cond(lx, eisvar(&lhs->e), "expect variable");
    checkreadonly(lx, &lhs->e);
    if (match(lx, ',')) { /* more vars ? */
        struct LHS var;
        var.prev = lhs;
        suffixedexpr(lx, &var.e);
        enterCstack(lx);
        assign(lx, &var, nvars + 1);
        leaveCstack(lx);
    } else { /* right side of assignment '=' */
        ExpInfo e;
        expect(lx, '=');
        int nexps = exprlist(lx, &e);
        if (nexps != nvars)
            adjustassign(lx, nexps, nvars, &e);
        else
            crC_exp2stack(lx->fs, &e);
    }
    if (lhs->e.et == EXP_GLOBAL)
        crC_defineglobal(lx->fs, &lhs->e);
    else
        crC_storevar(lx->fs, &lhs->e);
}


/*
 * exprstm ::= functioncall
 *           | varlist '=' explist
 */
static void exprstm(Lexer *lx) {
    struct LHS var;
    suffixedexpr(lx, &var.e);
    if (check(lx, '=') || check(lx, ',')) {
        var.prev = NULL;
        assign(lx, &var, 1);
    } else {
        expect_cond(lx, var.e.et == EXP_CALL, "syntax error");
        Instruction *call = getinstruction(lx->fs, &var.e);
        SETARG_L(call, 0, 1);
    }
}


/* get variable modifier */
static int attribute(Lexer *lx) {
    const char *mod = getstrbytes(expect_id(lx));
    if (strcmp(mod, "final") == 0) {
        return VARFINAL;
    } else if (strcmp(mod, "private") == 0) {
        return VARPRIVATE;
    } else if (strcmp(mod, "close") == 0) {
        return VARTBC;
    } else {
        crP_semerror(lx, crS_pushfstring(lx->ts, "unknown modifier '%s'", mod));
    }
}


/* 
** modifiers ::= empty
**             | '<' attribute '>'
**             | '<' attribute, ... '>'
*/
static int modifiers(Lexer *lx, const char *name) {
    if (!match(lx, '<')) 
        return 0;
    int bmask = 0;
    do {
        int bit = attribute(lx);
        if (testbit(bmask, bit)) { /* duplicate modifier ? */
            crP_semerror(lx, crS_pushfstring(lx->ts,
                        "variable '%s' has duplicate '%s' modifier",
                        name, lx->t.lit.str));
        } else if (testbit(bmask, VARTBC) && bit == VARPRIVATE) {
            crP_semerror(lx, crS_pushfstring(lx->ts,
                        "variable '%s' can't have both private and close modifiers",
                        name));
        }
        bmask = bit2mask(bmask, bit);
    } while (match(lx, ','));
    expect(lx, '>');
    return bmask;
}


/* create new variable 'name' */
static int newvar(FunctionState *fs, OString *name, ExpInfo *e, int mods) {
    Lexer *lx = fs->lx;
    int vidx = -1;
    if (fs->scope->prev) { /* local ? */
        if (cr_unlikely(testbit(mods, VARPRIVATE))) {
            crP_semerror(lx, crS_pushfstring(lx->ts,
                        "local variables '%s' is already private", name));
        }
        checkvarcollision(fs, name, "local", searchlocal);
        vidx = newlocal(lx, name, mods);
        initexp(e, EXP_LOCAL, vidx);
    } else { /* global */
        cr_assert(!testbit(mods, VARTBC)); /* can't have close */
        if (mods & VARPRIVATE) { /* private global ? */
            checkvarcollision(fs, name, "private", searchprivate);
            vidx = newprivate(fs, name, mods);
            initexp(e, EXP_PRIVATE, vidx);
        } else { /* regular global */
            newglobal(lx, name, mods);
            initglobal(e, name);
        }
    }
    return vidx;
}


/* mark local variable at 'idx' as tbc and close it */
static void marktbc(FunctionState *fs, int idx) {
    if (idx >= 0) {
        scopemarktbc(fs);
        crC_emitIL(fs, OP_TBC, idx);
    }
}


static int closedecl(FunctionState *fs, int vidx) {
    if (cr_unlikely(vidx == -1)) /* closing global ? */
        crP_semerror(fs->lx, "can't close global variables");
    return fs->activelocals;
}


/* check if decl at 'vidx' needs to be closed and return the index */
static void closeletdecl(FunctionState *fs, int nvars, int vidx, int *tbc) {
    if (cr_unlikely(*tbc != -1)) { /* already have close ? */
        crP_semerror(fs->lx, 
                "multiple to-be-closed variables in a single declaration");
    } else {
        *tbc = closedecl(fs, vidx) + nvars;
    }
}


static OString *declname(Lexer *lx, int *mods) {
    cr_assert(mods != NULL);
    OString *name = expect_id(lx);
    *mods = modifiers(lx, getstrbytes(name));
    return name;
}


static OString *newdecl(FunctionState *fs, ExpInfo *var, int *mods, int *vidx) {
    cr_assert(vidx != NULL);
    OString *name = declname(fs->lx, mods);
    *vidx = newvar(fs, name, var, *mods);
    return name;
}


/* declare variable list */
static OString *declarevarlist(FunctionState *fs, ExpInfo *var, int nvars,
                               int *tbc)
{
    int mods, vidx;
    OString *name = newdecl(fs, var, &mods, &vidx);
    if (mods & VARTBC)
        closeletdecl(fs, nvars, vidx, tbc);
    return name;
}


/* declare single variable */
static OString *declarevar(FunctionState *fs, ExpInfo *var, int *tbc) {
    *tbc = -1;
    OString *name = declarevarlist(fs, var, 0, tbc);
    return name;
}


/* auxiliary for assignment of multiple variable definitions */
static void assigndefine(Lexer *lx, struct LHS *lhs, int *tbc, int nvars) {
    FunctionState *fs = lx->fs;
    if (match(lx, ',')) { /* more ids ? */
        struct LHS var;
        var.prev = lhs;
        declarevarlist(fs, &var.e, nvars, tbc);
        enterCstack(lx);
        assigndefine(lx, &var, tbc, nvars + 1);
        leaveCstack(lx);
    } else { /* right side of declaration */
        ExpInfo e;
        expect(lx, '=');
        int nexps = exprlist(lx, &e);
        if (nexps != nvars)
            adjustassign(lx, nexps, nvars, &e);
        else
            crC_exp2stack(fs, &e);
    }
    if (lhs->e.et != EXP_GLOBAL) {
        initvariable(fs, &lhs->e);
        crC_storevar(fs, &lhs->e);
    } else {
        defineglobal(fs, &lhs->e);
    }
}


/* 
 * letdecl ::= 'let' idlist ';'
 *           | 'let' idlist '=' exprlist ';'
 */
static void letdecl(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int tbc = -1;
    struct LHS var;
    crL_scan(lx); /* skip 'let' */
    declarevarlist(fs, &var.e, 0, &tbc);
    if (check(lx, ',') || check(lx, '=')) { /* id list ? */
        var.prev = NULL;
        assigndefine(lx, &var, &tbc, 1);
    } else { /* otherwise assign implicit nil */
        crC_nil(lx->fs, 1);
        crC_reserveslots(fs, 1);
        if (var.e.et != EXP_GLOBAL) {
            initvariable(fs, &var.e);
            crC_storevar(fs, &var.e);
        } else {
            defineglobal(fs, &var.e);
        }
    }
    marktbc(fs, tbc);
    expect(lx, ';');
}


/*
 * stmlist ::= '{' stm '}'
 *         | '{' stm stmlist '}'
 */
static void stmlist(Lexer *lx) {
    while (!check(lx, '}') && !check(lx, TK_EOS))
        stm(lx);
}


/* blockstm ::= '{' stmlist '}' */
static void blockstm(Lexer *lx) {
    Scope s;
    int matchline = lx->line;
    crL_scan(lx); /* skip '{' */
    startscope(lx->fs, &s, 0);
    stmlist(lx);
    endscope(lx->fs);
    expectmatch(lx, '}', '{', matchline);
}


/* 
 * argslist ::= id
 *            | '...'
 *            | id ',' argslist
 */
static void argslist(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Function *fn = fs->fn;
    int nargs = 0;
    int isvararg = 0;
    if (!check(lx, ')')) { /* have args ? */
        do {
            switch (lx->t.tk) {
            case TK_IDENTIFIER: {
                newlocal(lx, expect_id(lx), 0);
                nargs++;
                break;
            }
            case TK_DOTS: {
                crL_scan(lx);
                isvararg = 1;
                break;
            }
            default: crL_syntaxerror(lx, "<identifier> or '...' expected");
            }
        } while (!isvararg && match(lx, ','));
    }
    adjustlocals(lx, nargs);
    fn->arity = nargs;
    if (isvararg)
        setvararg(fs, nargs);
    crC_reserveslots(fs, nargs);
}


/* emit closure instruction */
static void codeclosure(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs->prev;
    e->u.info = crC_emitIL(fs, OP_CLOSURE, fs->nfuncs - 1);
    e->et = EXP_FINEXPR;
}


/* funcbody ::= '(' arglist ')' stmlist */
static void funcbody(Lexer *lx, ExpInfo *v, int linenum, int ismethod) {
    FunctionState newfs;
    Scope scope;
    newfs.fn = addfunction(lx);
    newfs.fn->defline = linenum;
    startfs(lx->fs, lx, &scope);
    int matchline = lx->line; /* line where '(' is located */
    expect(lx, '(');
    if (ismethod) { /* is this method ? */
        newlocallit(lx, "self"); /* create 'self' */
        adjustlocals(lx, 1); /* and initialize it */
    }
    argslist(lx);
    expectmatch(lx, ')', '(', matchline);
    expect(lx, ')');
    matchline = lx->line; /* line where '{' is located */
    expect(lx, '{');
    stmlist(lx);
    expectmatch(lx, '}', '{', matchline);
    codeclosure(lx, v);
    endfs(&newfs);
}


/* fndecl ::= 'fn' id funcbody */
static void fndecl(Lexer *lx, int linenum) {
    FunctionState *fs = lx->fs;
    ExpInfo var, e;
    int tbc;
    crL_scan(lx); /* skip 'fn' */
    declarevar(fs, &var, &tbc);
    initvariable(fs, &var);
    funcbody(lx, &e, linenum, 0);
    if (var.et == EXP_GLOBAL)
        defineglobal(fs, &var);
    else
        crC_storevar(fs, &var);
    marktbc(fs, tbc);
}


/* inherit from superclass */
static void codeinherit(Lexer *lx, OString *name, Scope *s, ExpInfo *var) {
    FunctionState *fs = lx->fs;
    int private = match(lx, '@');
    OString *supname = expect_id(lx);
    if (private) { /* superclass is private global ? */
        /* assert: class is not private */
        searchprivate(fs, supname, var);
    } else if (cr_unlikely(streq(name, supname))) { /* name collision ? */
        crP_semerror(lx, crS_pushfstring(lx->ts,
        "variable '%s' attempt to inherit itself", name));
    } else { /* superclass is local, upvalue or global */
        searchvar(fs, supname, var, 1); /* find superclass */
    }
    crC_varexp2stack(fs, var); /* put superclass on the stack */
    cr_assert(var->et == EXP_FINEXPR);
    startscope(fs, s, 0); /* scope for 'super' */
    newlocallit(lx, "super"); /* local var for 'super' */
    adjustlocals(lx, 1);
    searchvar(fs, name, var, 1); /* search class variable */
    crC_varexp2stack(fs, var); /* put class on the stack */
    cr_assert(var->et == EXP_FINEXPR);
    crC_emitI(fs, OP_INHERIT);
    lx->ps->cs->super = 1;
}


static void startcs(Lexer *lx, ClassState *cs) {
    ParserState *ps = lx->ps;
    cs->prev = ps->cs;
    cs->super = 0;
    ps->cs = cs;
}


/* end current 'ClassState' */
static void endcs(Lexer *lx) {
    ParserState *ps = lx->ps;
    cr_assert(ps->cs != NULL);
    if (ps->cs->super)
        endscope(lx->fs);
    ps->cs = ps->cs->prev;
}


static void codemethod(FunctionState *fs, ExpInfo *var) {
    cr_assert(var->et == EXP_STRING);
    if (sisvmtmethod(var->u.str)) {
        crC_emitIL(fs, OP_SETMM, var->u.str->extra);
    } else {
        crC_exp2stack(fs, var);
        crC_method(fs, var);
    }
}


/* class method */
static void method(Lexer *lx) {
    ExpInfo var, dummy;
    int defline = lx->line; /* method definition start line */
    expect(lx, TK_FN);
    expid(lx, &var);
    funcbody(lx, &dummy, defline, 1);
    codemethod(lx->fs, &var);
}


static void classbody(Lexer *lx) {
    while (!check(lx, '}') && !check(lx, TK_EOS))
        method(lx);
}


/* 
 * classdecl ::= 'class' id classbody
 *             | 'class' id inherits id classbody
 */
static void classdecl(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Scope s; /* scope for 'super' */
    ClassState cs;
    ExpInfo var, e;
    int tbc;
    crL_scan(lx); /* skip 'class' */
    OString *name = declarevar(fs, &var, &tbc);
    initvariable(fs, &var);
    startcs(lx, &cs);
    crC_emitI(fs, OP_CLASS);
    if (match(lx, TK_INHERITS)) /* class object inherits ? */
        codeinherit(lx, name, &s, &e);
    searchvar(fs, name, &e, 1); /* find class variable */
    crC_varexp2stack(fs, &e); /* put class variable on stack */
    cr_assert(e.et == EXP_FINEXPR);
    int matchline = lx->line;
    expect(lx, '{');
    classbody(lx);
    expectmatch(lx, '}', '{', matchline);
    endcs(lx);
    marktbc(fs, tbc);
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
    cr_ubyte havedefault; /* if switch has 'default' case */
    cr_ubyte havenil; /* if switch has 'nil' case */
    cr_ubyte havetrue; /* if switch has '1' case */
    cr_ubyte havefalse; /* if switch has '0' case */
    int jmp; /* code jmp to patch if 'label' expression is not 'CASEMATCH' */
    enum {
        LABELNONE, /* no labels */
        LABELDFL, /* current label is 'default' */
        LABELCASE, /* current label is 'case' */
        LABELMATCH, /* current label is matching literal expression */
    } label;
} SwitchState;


static void initss(SwitchState *ss) {
    { ss->literals.arr = NULL; ss->literals.len = ss->literals.size = 0; }
    ss->jmp = NOJMP;
    ss->label = LABELNONE;
    ss->havedefault = 0;
    ss->havenil = 0;
    ss->havetrue = 0;
    ss->havefalse = 0;
}


/* convert literal information into text */
static const char *li2text(cr_State *ts, LiteralInfo *li) {
    switch (li->tt) {
    case CR_VSTRING:
        return crS_pushfstring(ts, " (%s)", getstrbytes(li->lit.str));
    case CR_VNUMINT: return crS_pushfstring(ts, " (%I)", li->lit.i);
    case CR_VNUMFLT: return crS_pushfstring(ts, " (%N)", li->lit.n);
    default: cr_unreachable();
    }
}


/* find literal info in 'literals' */
static int findli(SwitchState *ss, LiteralInfo *tlit) {
    for (int i = 0; i < ss->literals.len; i++) {
        LiteralInfo *curr = &ss->literals.arr[i];
        if (tlit->tt != curr->tt) /* skip if types don't match */
            continue;
        switch (tlit->tt) {
        case CR_VSTRING: {
            if (streq(tlit->lit.str, curr->lit.str))
                return i;
            break;
        }
        case CR_VNUMINT: {
            if (cri_numeq(tlit->lit.i, curr->lit.i))
                return i;
            break;
        }
        case CR_VNUMFLT: {
            if (cri_numeq(tlit->lit.n, curr->lit.n))
                return i;
            break;
        }
        default: cr_unreachable();
        }
    }
    return -1;
}


/* check for duplicate literal otherwise fill the relevant info */
static LiteralInfo checkduplicate(Lexer *lx, SwitchState *ss, ExpInfo *e) {
    LiteralInfo li;
    int extra = 0;
    const char *what = NULL;
    cr_assert(eisconstant(e));
    switch (e->et) {
    case EXP_FALSE: {
        if (cr_unlikely(ss->havefalse))
            what = "false";
        ss->havefalse = 1;
        break;
    }
    case EXP_TRUE: {
        if (cr_unlikely(ss->havetrue))
            what = "true";
        ss->havetrue = 1;
        break;
    }
    case EXP_NIL: {
        if (cr_unlikely(ss->havenil))
            what = "nil";
        ss->havenil = 1;
        break;
    }
    case EXP_STRING: {
        what = "string";
        li.lit.str = e->u.str;
        li.tt = CR_VSTRING;
        goto findliteral;
    }
    case EXP_INT: {
        what = "integer";
        li.lit.i = e->u.i;
        li.tt = CR_VNUMINT;
        goto findliteral;
    }
    case EXP_FLT: {
        what = "number";
        li.lit.n = e->u.n;
        li.tt = CR_VNUMFLT;
findliteral:;
        int idx = findli(ss, &li);
        if (cr_likely(idx < 0)) 
            what = NULL;
        else
            extra = 1;
        break;
    }
    default: cr_unreachable();
    }
    if (cr_unlikely(what)) {
        crP_semerror(lx, crS_pushfstring(lx->ts,
        "duplicate %s literal%s in switch statement", 
        what, (extra ? li2text(lx->ts, &li) : "")));
    }
    return li;
}


/*
 * Adds new literal information to 'literals' if 
 * 'e' is constant expression.
 */
static int newlitinfo(Lexer *lx, SwitchState *ss, ExpInfo *caseexp) {
    if (eisconstant(caseexp)) {
        LiteralInfo li = checkduplicate(lx, ss, caseexp);
        crM_growvec(lx->ts, ss->literals.arr, ss->literals.size,
                    ss->literals.len, MAXLONGARGSIZE,
                    "switch literals", LiteralInfo);
        ss->literals.arr[ss->literals.len++] = li;
        if (eisconstant(&ss->e)) { /* both are constant expressions ? */
            TValue v1, v2;
            movexp2v(lx->fs, &ss->e, &v1);
            movexp2v(lx->fs, caseexp, &v2);
            return crV_raweq(&v1, &v2);
        }
    }
    return 0;
}


/* add new fall-through case jump */
static void addfallthrujmp(FunctionState *fs) {
    int jmp = crC_jmp(fs, OP_JMP);
    patchlistaddjmp(fs, jmp);
}


/* 
 * Tries to preserve expression 'e' after consuming
 * it, in order to enable more optimizations.
 */
static int codepresexp(FunctionState *fs, ExpInfo *e) {
    ExpInfo pres = *e;
    int isctc = eisconstant(e);
    crC_exp2stack(fs, e);
    if (isctc)
        *e = pres;
    return isctc;
}


/* 
 * switchbody ::= 'case' ':' expr switchbody
 *              | 'default' ':' switchbody
 *              | stm switchbody
 *              | empty
 */
static void switchbody(Lexer *lx, SwitchState *ss, DynCtx *ctx) {
    DynCtx endctx;
    FunctionState *fs = lx->fs;
    int jmp = NOJMP;
    endctx.pc = ctx->pc;
    while (!check(lx, '}') && !check(lx, TK_EOS)) {
        if (check(lx, TK_CASE) || match(lx, TK_DEFAULT)) { /* have case ? */
            if (ss->label != LABELNONE && ss->label != LABELMATCH) {
                addfallthrujmp(fs);
                if (ss->label == LABELCASE)
                    crC_patchtohere(fs, ss->jmp);
            }
            if (match(lx, TK_CASE)) {
                ExpInfo caseexp;
                expr(lx, &caseexp);
                codepresexp(fs, &ss->e);
                expect(lx, ':');
                if (newlitinfo(lx, ss, &caseexp)) { /* compile-time match ? */
                    ss->label = LABELMATCH;
                    loadctx(fs, ctx); /* load 'ctx' */
                    resetpatchlist(fs);
                } else if (ss->label != LABELMATCH) { /* don't have match? */
                    ss->label = LABELCASE;
                    crC_emitI(fs, OP_EQPRESERVE);
                    ss->jmp = crC_test(fs, OP_TESTPOP, 0);
                }
            } else if (!ss->havedefault) { /* first 'default' case ? */
                ss->havedefault = 1;
                ss->label = LABELDFL;
                expect(lx, ':');
            } else {
                crP_semerror(lx, "multiple default cases in switch");
            }
            if ((jmp = patchlistpop(fs)) != NOJMP) /* have jump to patch ? */
                crC_patchtohere(fs, jmp);
        } else if (ss->label != LABELNONE) {
            stm(lx);
            if (ss->label == LABELMATCH && fs->lastwasret) {
                storectx(fs, &endctx);
                storereachablectx(fs);
            }
        } else {
            crP_semerror(lx, "expected 'case' or 'default'");
        }
    }
    if (ss->label == LABELMATCH) { /* have ctmatch */
        cr_assert(endctx.pc > ctx->pc); /* must have valid 'endctx' */
        loadctx(fs, &endctx); /* load it */
    }
}


/* switchstm ::= 'switch' '(' expr ')' '{' switchbody '}' */
static void switchstm(Lexer *lx) {
    SwitchState ss;
    FunctionState *fs = lx->fs;
    DynCtx ctx;
    Scope *oldswitchscope = fs->switchscope;
    Scope s; /* switch scope */
    initss(&ss);
    startscope(fs, &s, CFSWITCH);
    fs->switchscope = &s;
    crL_scan(lx); /* skip 'switch' */
    int matchline = lx->line;
    expect(lx, '(');
    expr(lx, &ss.e);
    codepresexp(fs, &ss.e);
    storectx(fs, &ctx);
    expectmatch(lx, ')', '(', matchline);
    matchline = lx->line;
    expect(lx, '{');
    switchbody(lx, &ss, &ctx);
    expectmatch(lx, '}', '{', matchline);
    endscope(fs); /* end implicit scope */
    fs->switchscope = oldswitchscope;
}


/* condition statement body; for 'forloop', 'whilestm' & 'ifstm' */
static void condbody(Lexer *lx, DynCtx *startctx, ExpInfo *cond, OpCode testop,
                     OpCode jmpop, int condpc, int endclausepc)
{
    FunctionState *fs = lx->fs;
    DynCtx endctx;
    int test, jmp;
    int optaway, condistrue;
    int condisctc = eisconstant(cond);
    int bodypc = currentPC(fs);
    int isloop = scopeisloop(fs->scope);
    cr_assert(isloop == (jmpop == OP_JMPS));
    optaway = condistrue = 0;
    endctx.pc = NOJMP;
    if (condisctc && !(condistrue = eistrue(cond)))
        optaway = 1;
    else
        test = crC_test(fs, testop, 0);
    stm(lx);
    if (optaway) {
        loadctx(fs, startctx);
        resetpatchlist(fs);
    } else if (condisctc && fs->lastwasret) {
        cr_assert(condistrue);
        storectx(fs, &endctx);
        storereachablectx(fs);
    } else {
        jmp = crC_jmp(fs, jmpop);
        if (isloop) { /* loop body ? */
            if (endclausepc != NOJMP) { /* 'for' loop ? */
                crC_patch(fs, jmp, endclausepc);
            } else if (condistrue) { /* 'while' loop with true ctcond ? */
                crC_patch(fs, jmp, bodypc);
                fs->loopstart = bodypc;
            } else { /* else 'while' loop with unknown non-ctcond ? */
                crC_patch(fs, jmp, condpc);
            }
        }
    }
    if (!optaway)
        crC_patchtohere(fs, test);
    if (!isloop && match(lx, TK_ELSE)) {
        stm(lx);
        if (!optaway && !condisctc)
            crC_patchtohere(fs, jmp);
    }
    if (endctx.pc != NOJMP)
        loadctx(fs, &endctx);
}


/* 
 * ifstm ::= 'if' '(' expr ')' condbody
 */
static void ifstm(Lexer *lx) {
    DynCtx ctx;
    ExpInfo e;
    crL_scan(lx); /* skip 'if' */
    storectx(lx->fs, &ctx);
    int matchline = lx->line;
    expect(lx, '(');
    expr(lx, &e);
    codepresexp(lx->fs, &e);
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
        cr_assert(ctx == NULL); /* please provide NULL for clarity */
        fs->loopscope = currctx->loopscope;
        fs->loopstart = currctx->loopstart;
        currctx = currctx->prev;
    }
}


/* start loop scope */
static void startloop(FunctionState *fs, Scope *s, struct LoopCtx *ctx,
                      int cfbits) 
{
    startscope(fs, s, cfbits);
    initloopctx(fs, ctx);
    handleloopctx(fs, ctx, 1);
    fs->loopscope = fs->scope;
    fs->loopstart = currentPC(fs);
}


/* end loop scope */
static void endloop(FunctionState *fs) {
    cr_assert(scopeisloop(fs->scope));
    endscope(fs);
    handleloopctx(fs, NULL, 0); /* load old loop ctx values */
}


/* whilestm ::= 'while' '(' expr ')' condbody */
static void whilestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    DynCtx startctx;
    struct LoopCtx lctx;
    Scope s; /* new 'loopscope' */
    ExpInfo cond;
    crL_scan(lx); /* skip 'while' */
    storectx(fs, &startctx);
    startloop(fs, &s, &lctx, CFLOOP);
    int pcexpr = currentPC(fs);
    int matchline = lx->line;
    expect(lx, '(');
    expr(lx, &cond);
    codepresexp(fs, &cond);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &startctx, &cond, OP_TESTPOP, OP_JMPS, pcexpr, NOJMP);
    endloop(fs);
}


static void foreachvar(Lexer *lx) {
    OString *name = expect_id(lx);
    int mods = modifiers(lx, getstrbytes(name));
    if (cr_unlikely(testbits(mods, bit2mask(VARPRIVATE, VARTBC)))) {
        crP_semerror(lx, crS_pushfstring(lx->ts,
        "local 'for each' loop variable '%s' can only have 'const' modifier",
        name));
    }
    newlocal(lx, expect_id(lx), mods);
}


/* patch for loop jump(back) */
static void patchforjmp(FunctionState *fs, int pc, int target, int back) {
    Instruction *jmp = &fs->fn->code[pc];
    int offset = target - pc;
    if (back)
        offset = -offset;
    if (cr_unlikely(offset > MAXJMP))
        crL_syntaxerror(fs->lx, "control structure (for loop) too long");
    SETARG_L(jmp, 1, offset);
}


/* generic for loop expressions */
static int forexprlist(Lexer *lx, ExpInfo *e, int limit) {
    int nexpr = 1;
    expr(lx, e);
    if (cr_unlikely(eisconstant(e))) {
        crP_semerror(lx, crS_pushfstring(lx->ts,
        "'%s' is invalid iterator function (for loop)",
        crL_tok2str(lx, lx->t.tk)));
    }
    while (match(lx, ',')) {
        crC_exp2stack(lx->fs, e);
        expr(lx, e);
        nexpr++;
    }
    if (cr_unlikely(nexpr > limit))
        limiterror(lx->fs, "expressions", limit);
    return nexpr;
}


/* number of state variables for generic for loop */
#define NUMSTATEVARS    4


/* foreachloop ::= 'for' 'each' idlist 'in' forexprlist '{' stmlist '}' */
static void foreachloop(Lexer *lx) {
    FunctionState *fs = lx->fs;
    DynCtx startctx;
    struct LoopCtx lctx;
    ExpInfo e;
    Scope s;
    int nvars = 1; /* iter func result */
    int base = fs->sp;
    const int forprepsz = SIZEINSTR + (SIZEARGL << 1);
    storectx(fs, &startctx);
    newlocallit(lx, "(for state)"); /* iter func (base) */
    newlocallit(lx, "(for state)"); /* invariant state (base+1) */
    newlocallit(lx, "(for state)"); /* control var (base+2) */
    newlocallit(lx, "(for state)"); /* to-be-closed var (base+3) */
    foreachvar(lx); /* iter func result var (base+4) */
    while (match(lx, ',')) {
        foreachvar(lx);
        nvars++;
    }
    expect(lx, TK_IN);
    adjustassign(lx, NUMSTATEVARS, forexprlist(lx, &e, NUMSTATEVARS), &e);
    adjustlocals(lx, NUMSTATEVARS); /* register state vars */
    scopemarktbc(fs);
    /* runtime space for call (iter func), inv. state, control var */
    crC_checkstack(fs, NUMSTATEVARS - 1);
    int prep = crC_emitILL(fs, OP_FORPREP, base, 0);
    startloop(fs, &s, &lctx, CFLOOP); /* scope for declared vars */
    adjustlocals(lx, nvars);
    crC_reserveslots(fs, nvars); /* locals */
    stm(lx);
    endloop(fs); /* end scope for declared vars */
    patchforjmp(fs, prep, currentPC(fs), 0);
    crC_emitILL(fs, OP_FORCALL, base, nvars);
    int forend = crC_emitILL(fs, OP_FORLOOP, base, 0);
    patchforjmp(fs, forend, prep + forprepsz, 1);
}


/* 'for' loop initializer */
void forinit(Lexer *lx) {
    if (!match(lx, ';')) {
        if (check(lx, TK_LET)) {
            letdecl(lx);
        } else {
            exprstm(lx);
            expect(lx, ';');
        }
    }
}


/* 'for' loop condition */
void forcond(Lexer *lx, ExpInfo *e) {
    if (!match(lx, ';')) {
        expr(lx, e);
        codepresexp(lx->fs, e);
        expect(lx, ';');
    }
}


/* 'for' loop last clause */
void forendclause(Lexer *lx, ExpInfo *cond, int *clausepc) {
    FunctionState *fs = lx->fs;
    int bodyjmp, loopjmp;
    cr_assert(*clausepc == NOJMP);
    if (check(lx, ')')) { /* no end clause ? */
        return; /* convert to 'while' loop */
    } else {
        int inf = eistrue(cond);
        if (!inf) /* loop is not infinite ? */
            bodyjmp = crC_jmp(fs, OP_JMP);
        *clausepc = currentPC(fs);
        exprstm(lx);
        if (!inf) {
            loopjmp = crC_jmp(fs, OP_JMPS);
            crC_patch(fs, loopjmp, fs->loopstart);
            crC_patchtohere(fs, bodyjmp);
        }
        fs->loopstart = *clausepc;
    }
}


/* forloop ::= 'for' '(' expr ')' condbody */
static void forloop(Lexer *lx) {
    FunctionState *fs = lx->fs;
    DynCtx startctx;
    struct LoopCtx lctx;
    Scope s; /* new 'loopscope' */
    ExpInfo cond;
    startloop(fs, &s, &lctx, CFLOOP);
    int matchline = lx->line;
    expect(lx, '(');
    forinit(lx);
    storectx(fs, &startctx);
    int condpc = currentPC(fs);
    forcond(lx, &cond);
    int endclausepc = NOJMP;
    forendclause(lx, &cond, &endclausepc);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &startctx, &cond, OP_TESTPOP, OP_JMPS, condpc, endclausepc);
    endloop(fs);
}

/* 
 * forstm ::= foreachloop
 *          | forloop
 */
static void forstm(Lexer *lx) {
    crL_scan(lx); /* skip 'for' */
    if (match(lx, TK_EACH))
        foreachloop(lx);
    else
        forloop(lx);
}


/* loopstm ::= 'loop' stm */
static void loopstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopCtx ctx;
    Scope s;
    crL_scan(lx); /* skip 'loop' */
    startloop(fs, &s, &ctx, CFLOOP);
    stm(lx);
    endloop(fs);
}


/* continuestm ::= 'continue' ';' */
static void continuestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    crL_scan(lx); /* skip 'continue' */
    if (cr_unlikely(fs->loopstart == NOJMP)) { /* no loop ? */
        cr_assert(fs->loopscope == NULL);
        crP_semerror(lx, "'continue' not in loop statement");
    } else {
        cr_assert(fs->loopscope != NULL);
        int extra = fs->scope->nswscope - fs->loopscope->nswscope;
        poplocals(fs, fs->loopscope->activelocals, extra);
        int jmp = crC_jmp(fs, OP_JMPS);
        crC_patch(fs, jmp, fs->loopstart);
    }
    expect(lx, ';');
}


/* get the most recent control flow scope */
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
    const Scope *s = getcfscope(fs);
    crL_scan(lx); /* skip 'break' */
    if (cr_unlikely(s == NULL)) { /* error ? */
        cr_assert(fs->loopscope == NULL && fs->switchscope == NULL);
        crP_semerror(lx, "'break' not in loop or switch statement");
    } else { /* otherwise add the jmp to patch list */
        poplocals(fs, s->activelocals, scopeisswitch(s));
        patchlistaddjmp(fs, crC_jmp(fs, OP_JMP));
    }
    expect(lx, ';');
}


/* 
 * returnstm ::= 'return' ';' 
 *             | 'return' exprlist ';'
 */
static void returnstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    ExpInfo e;
    int base = fs->sp;
    int nreturns = 0;
    crL_scan(lx); /* skip 'return' */
    if (!check(lx, ';')) { /* have return values ? */
        nreturns = exprlist(lx, &e);
        if (eismulret(&e)) {
            crC_setreturns(fs, &e, CR_MULRET);
            nreturns = CR_MULRET;
        } else {
            crC_exp2stack(fs, &e);
        }
    }
    crC_ret(fs, base, nreturns);
    fs->lastwasret = 1;
    expect(lx, ';');
}


/* 
 * stm ::= whilestm
 *       | forstm
 *       | ifstm
 *       | switchstm
 *       | blockstm
 *       | continuestm
 *       | breakstm
 *       | returnstm
 *       | loopstm
 *       | ';'
 *       | exprstm
 */
static void stm(Lexer *lx) {
    switch (lx->t.tk) {
    case TK_WHILE: whilestm(lx); break;
    case TK_FOR: forstm(lx); break;
    case TK_IF: ifstm(lx); break;
    case TK_SWITCH: switchstm(lx); break;
    case '{': blockstm(lx); break;
    case TK_CONTINUE: continuestm(lx); break;
    case TK_BREAK: breakstm(lx); break;
    case TK_RETURN: returnstm(lx); return;
    case TK_LOOP: loopstm(lx); break;
    case ';': crL_scan(lx); break;
    default: exprstm(lx); expect(lx, ';'); break;
    }
    lx->fs->lastwasret = 0;
}


/* 
 * decl ::= letdecl
 *        | fndecl
 *        | classdecl
 */
static void decl(Lexer *lx) {
    int line = lx->line;
    switch (lx->t.tk) {
    case TK_LET: letdecl(lx); break;
    case TK_FN: fndecl(lx, line); break;
    case TK_CLASS: classdecl(lx); break;
    default: stm(lx); break;
    }
}


/* parse current input until end of stream */
static void parseuntilEOS(Lexer *lx) {
    while (!check(lx, TK_EOS))
        decl(lx);
}



/* compile main function */
static void mainfunc(FunctionState *fs, Lexer *lx) {
    Scope s;
    startfs(fs, lx, &s);
    setvararg(fs, 0); /* main is always vararg */
    crL_scan(lx); /* scan first token */
    parseuntilEOS(lx); /* start parsing */
    cr_assert(lx->t.tk == TK_EOS);
    endfs(fs);
}


/* parse source code */
CrClosure *crP_parse(cr_State *ts, BuffReader *br, Buffer *buff,
                     ParserState *ps, const char *source)
{
    Lexer lx;
    FunctionState fs;
    CrClosure *cl = crF_newCrClosure(ts, 0);
    setcrcl2s(ts, ts->sp.p, cl); /* anchor main function closure */
    crT_incsp(ts);
    lx.tab = crH_new(ts);
    setsv2ht(ts, ts->sp.p, lx.tab); /* anchor scanner htable */
    crT_incsp(ts);
    fs.fn = cl->fn = crF_new(ts);
    crG_objbarrier(ts, cl, cl->fn);
    fs.fn->source = crS_new(ts, source);
    crG_objbarrier(ts, fs.fn, fs.fn->source);
    lx.ps = ps;
    lx.buff = buff;
    crL_setsource(ts, &lx, br, fs.fn->source);
    mainfunc(&fs, &lx); /* Cript main function */
    ts->sp.p--; /* pop scanner htable */
    return cl;
}
