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
#include "crvalue.h"
#include "crstring.h"
#include "crfunction.h"
#include "crvm.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>



/* enter C function frame */
#define entercstack(lx)         cr_state_inccalls((lx)->ts)

/* pop C function frame */
#define leavecstack(lx)         ((lx)->ts->ncalls--)


/* compare 'OString' pointers for equality */
#define streq(s1,s2)        ((s1) == (s2))


/* expect 'cond' to be true or invoke error */
#define expect_cond(lx, cond, err) \
    { if (!(cond)) cr_lex_syntaxerror(lx, err); }


/* 
 * Mask for marking undefined or not fully initialized
 * variables (statics and globals).
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
    int nlocals; /* number of locals outside of this scope */
    cr_ubyte cfbits; /* control flow bits */
    cr_ubyte haveupval; /* set if scope contains upvalue variable */
    cr_ubyte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;


/* dynamic data context (for optimizations) */
struct DynCtx {
    int loopstart;
    int sp;
    int nfuncs;
    int nk;
    int nstatics;
    int pc;
    int nlinfo;
    int nlocals;
    int nupvals;
    int nbrks;
    int needclose;
};


static void storectx(FunctionState *fs, struct DynCtx *ctx) {
    ctx->loopstart = fs->loopstart;
    ctx->sp = fs->sp;
    ctx->nfuncs = fs->nfuncs;
    ctx->nk = fs->nk;
    ctx->nstatics = fs->nstatics;
    ctx->pc = fs->pc;
    ctx->nlinfo = fs->nlinfo;
    ctx->nlocals = fs->nlocals;
    ctx->nupvals = fs->nupvals;
    ctx->nbrks = fs->patches.len;
    ctx->needclose = fs->needclose;
}


static void loadctx(FunctionState *fs, struct DynCtx *ctx) {
    fs->loopstart = ctx->loopstart;
    fs->sp = ctx->sp;
    fs->nfuncs = ctx->nfuncs;
    fs->nk = ctx->nk;
    fs->nstatics = ctx->nstatics;
    fs->pc = ctx->pc;
    fs->nlinfo = ctx->nlinfo;
    fs->nlocals = ctx->nlocals;
    fs->nupvals = ctx->nupvals;
    fs->patches.len = ctx->nbrks;
    fs->needclose = ctx->needclose;
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
    cr_mem_growvec(fs->lx->ts, list->arr, list->size, list->len,
                   MAXLONGARGSIZE, "code jumps");
    list->arr[list->len++] = jmp;
}


/* reset patch list length */
static void resetpatchlist(FunctionState *fs) {
    cr_assert(fs->patches.len > 0);
    fs->patches.list[fs->patches.len - 1].len = 0;
}


/* create new patch list */
static void patchliststart(FunctionState *fs) {
    cr_mem_growvec(fs->lx->ts, fs->patches.list, fs->patches.size, fs->patches.len,
                   MAXLONGARGSIZE, "patch lists");
    fs->patches.list[fs->patches.len++] = (PatchList){0};
}


/* end patch list by patching all pending jumps */
static void patchlistend(FunctionState *fs) {
    cr_assert(fs->patches.len > 0);
    PatchList *list = &fs->patches.list[--fs->patches.len];
    int jmp; /* 'break' jmp */
    while ((jmp = patchlistpop(fs)) != NOJMP)
        cr_code_patchtohere(fs, jmp);
    cr_assert(list->len == 0);
    cr_mem_freearray(fs->lx->ts, list->arr, list->size);
}


static cr_noret expecterror(Lexer *lx, int tk) {
    const char *err = cr_string_pushfstring(lx->ts, "expected %s",
                                            cr_lex_tok2str(lx, tk));
    cr_lex_syntaxerror(lx, err);
}


static cr_noret limiterror(FunctionState *fs, const char *what, int limit) {
    cr_State *ts = fs->lx->ts;
    int line = fs->fn->defline;
    const char *where = (line == 0 ? "main function" :
                        cr_string_pushfstring(ts, "function at line %d", line));
    const char *err = cr_string_pushfstring(ts, "too many %s (limit is %d) in %s",
                                            what, limit, where);
    cr_lex_syntaxerror(fs->lx, err);
}


/* semantic error; variant of syntax error without 'near <token>' */
cr_noret cr_parser_semerror(Lexer *lx, const char *err) {
    lx->t.tk = 0;
    cr_lex_syntaxerror(lx, err);
}


static void checklimit(FunctionState *fs, int n, int limit, const char *what) {
    if (n >= limit)
        limiterror(fs, what, limit);
}


/* variable initialization error */
static cr_noret variniterror(Lexer *lx, const char *vartype, const char *name) {
    cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
                           "can't read %s variable '%s' in its own initializer",
                           vartype, name));
}


/* check for variable collision */
static void checkvarcollision(FunctionState *fs, OString *name, const char *vk,
                              int (*fn)(FunctionState *, OString *, ExpInfo *))
{
    ExpInfo dummy;
    if (cr_unlikely(fn == NULL || fn(fs, name, &dummy) >= 0))
        cr_parser_semerror(fs->lx, cr_string_pushfstring(fs->lx->ts,
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


/* remove local variables up to 'tolevel' */
static void removevars(FunctionState *fs, int tolevel) {
    fs->lx->ps->lvars.len -= (fs->activelocals - tolevel);
    while (fs->activelocals > tolevel) {
        LVarInfo *locinfo = getlocalinfo(fs, --fs->activelocals);
        locinfo->endpc = fs->pc;
    }
}


/* get static variable */
cr_sinline SVar *getstatic(FunctionState *fs, int idx) {
    cr_assert(0 <= idx && idx < fs->fn->nstatics);
    return &fs->fn->statics[idx];
}


/* get upvalue variable */
cr_sinline UpValInfo *getupvalue(FunctionState *fs, int idx) {
    cr_assert(idx < fs->fn->nupvals);
    return &fs->fn->upvals[idx];
}


/* move constant expression to value 'v' */
static void movexp2v(FunctionState *fs, ExpInfo *e, TValue *v) {
    switch (e->et) {
    case EXP_NIL: setnilval(v); break;
    case EXP_FALSE: setbfval(v); break;
    case EXP_TRUE: setbtval(v); break;
    case EXP_STRING: setv2s(NULL, v, e->u.str); break;
    case EXP_INT: setival(v, e->u.i); break;
    case EXP_FLT: setfval(v, e->u.n); break;
    case EXP_K: *v = *getconstant(fs, e); break;
    default: cr_unreachable();
    }
}


/* init expression with generic information */
static void initexp(ExpInfo *e, expt et, int info) {
    e->t = e->f = -1;
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
    cr_mem_growvec(lx->ts, fn->locals, fn->sizelocals, fs->nlocals,
                   MAXLONGARGSIZE, "locals");
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
 * Define static variable by removing UNDEFMODMASK.
 * This mask is not contained in VARBITMASK. 
 */ 
static void definestatic(FunctionState *fs, int vidx) {
    SVar *svar = getstatic(fs, vidx);
    vmod(&svar->val) &= VARBITMASK;
}


/* define variable */
static void codedefine(FunctionState *fs, ExpInfo *var) {
    cr_assert(var->et != EXP_UPVAL); /* can't define upvalue */
    switch (var->et) {
    case EXP_LOCAL: {
        adjustlocals(fs->lx, 1); /* register it into 'locals' */
        break;
    }
    case EXP_STATIC: {
        /* fully initialize static variable */
        definestatic(fs, var->u.info);
        break;
    }
    case EXP_GLOBAL: {
        /* emit define rather than emitting store instruction */
        cr_code_defineglobal(fs, var);
        return;
    }
    default: cr_unreachable();
    }
    cr_code_storevar(fs, var);
}


/* start lexical scope */
static void startscope(FunctionState *fs, Scope *s, int newcfbits) {
    if (newcfbits & (CFLOOP | CFSWITCH)) /* needs a patch list ? */
        patchliststart(fs); /* 'break' statement jumps storage */
    s->nlocals = fs->activelocals;
    s->cfbits = fs->scope->cfbits | newcfbits;
    s->haveupval = 0;
    s->havetbcvar = (fs->scope != NULL && fs->scope->havetbcvar);
    s->prev = fs->scope;
    fs->nswscopes += scopeisswitch(s);
    fs->scope = s;
}


/* get number of active locals relative to 'n' */
#define activelocalsrel(n)      ((fs)->activelocals - (n))


/* end lexical scope */
static void endscope(FunctionState *fs) {
    Scope *s = fs->scope;
    int stklevel = getstacklevel(fs, s->nlocals);
    removevars(fs, s->nlocals);
    cr_assert(s->nlocals == fs->activelocals);
    if (scopeisloop(s) || scopeisswitch(s)) /* has a patch list ? */
        patchlistend(fs);
    if (s->prev && s->haveupval) /* need to close upvalues ? */
        cr_code_S(fs, OP_CLOSE, stklevel);
    cr_code_pop(fs, scopeisswitch(s) + activelocalsrel(s->nlocals));
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
    while(s->nlocals - 1 > vidx)
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
    fs->loopstart = -1;
    fs->sp = 0;
    fs->activelocals = 0;
    fs->firstlocal = lx->ps->lvars.len;
    { fs->patches.len = fs->patches.size = 0; fs->patches.list = NULL; } /* 'brks' */
    fs->needclose = 0;
    fs->fn->source = lx->src;
    cr_gc_objbarrier(lx->ts, fs->fn, fs->fn->source);
    fs->fn->maxstack = 1; /* for 'self' */
    startscope(fs, s, 0); /* start global scope */
}


/* cleanup function state */
static void endfs(FunctionState *fs) {
    Lexer *lx = fs->lx;
    Function *fn = fs->fn;
    cr_State *ts = lx->ts;
    cr_code_ret(fs, fs->activelocals - 1, 0);
    cr_assert(fs->scope && !fs->scope->prev);
    endscope(fs); /* end global scope */
    cr_assert(!fs->scope);
    /* preserve memory; shrink unused space */
    /* using counters in 'fs' as final size */
    cr_mem_shrinkvec(ts, fn->funcs, fn->sizefn, fs->nfuncs);
    cr_mem_shrinkvec(ts, fn->k, fn->sizek, fs->nk);
    cr_mem_shrinkvec(ts, fn->statics, fn->sizestatics, fs->nstatics);
    cr_mem_shrinkvec(ts, fn->code, fn->sizecode, fs->pc);
    cr_mem_shrinkvec(ts, fn->linfo, fn->sizelinfo, fs->nlinfo);
    cr_mem_shrinkvec(ts, fn->locals, fn->sizelocals, fs->nlocals);
    cr_mem_shrinkvec(ts, fn->upvals, fn->sizeupvals, fs->nupvals);
    lx->fs = fs->prev;
    cr_gc_check(ts);
}


/* add function */
static Function *addfunction(Lexer *lx) {
    Function *new;
    cr_State *ts = lx->ts;
    FunctionState *fs = lx->fs;
    Function *fn = fs->fn;
    checklimit(fs, fs->nfuncs + 1, MAXLONGARGSIZE, "functions");
    cr_mem_growvec(ts, fn->funcs, fn->sizefn, fs->nfuncs, MAXLONGARGSIZE,
                   "functions");
    fn->funcs[fs->nfuncs++] = new = cr_function_new(ts);
    cr_gc_objbarrier(ts, fn, new);
    return new;
}


/* set current function as vararg */
static void setvararg(FunctionState *fs, int arity) {
    fs->fn->isvararg = 1;
    cr_code_L(fs, OP_SETVARARG, arity);
}


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
        cr_lex_scan(lx);
        return 1;
    }
    return 0;
}


/* check if 'tk' matches the current token */
static void expect(Lexer *lx, int tk) {
    if (check(lx, tk)) {
        cr_lex_scan(lx);
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
            cr_lex_syntaxerror(lx, cr_string_pushfstring(lx->ts,
            "%s expected (to close %s at line %d)",
            cr_lex_tok2str(lx, what), cr_lex_tok2str(lx, who), linenum));
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
    cr_mem_growvec(lx->ts, ps->lvars.arr, ps->lvars.size, ps->lvars.len,
                   MAXLONGARGSIZE, "locals");
    LVar *local = &ps->lvars.arr[ps->lvars.len++];
    vmod(&local->val) = mods;
    local->s.name = name;
    local->s.idx = -1;
    return ps->lvars.len - fs->firstlocal - 1;
}


#define newlocallit(lx,lit) \
    newlocal(lx, cr_lex_newstring(lx, "" lit, SLL(lit)), 0)


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
 * Searches for static variable 'name'.
 */
static int searchstatic(FunctionState *fs, OString *name, ExpInfo *e) {
    Lexer *lx = fs->lx;
    for (int i = fs->nstatics - 1; i >= 0; i++) {
        SVar *svar = getstatic(fs, i);
        if (streq(name, svar->s.name)) {
            if (cr_unlikely(testbits(vmod(&svar->val), UNDEFMODMASK)))
                variniterror(lx, "static", getstrbytes(name));
            initexp(e, EXP_STATIC, i);
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
    cr_mem_growvec(ts, fn->upvals, fn->sizeupvals, fs->nupvals, MAXLONGARGSIZE,
                   "upvalues");
    return &fn->upvals[fs->nupvals++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *e) {
    UpValInfo *uv = newupvalue(fs);
    uv->name = name;
    if (e->et == EXP_LOCAL) { /* local ? */
        uv->onstack = 1;
        uv->idx = e->u.info;
        uv->mod = vmod(&getlocal(fs, e->u.info)->val);
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
static void newglobal(Lexer *lx, OString *name, int mod) {
    TValue k, val;
    setv2s(lx->ts, &k, name);
    setemptyval(&val);
    vmod(&val) = mod;
    cr_htable_set(lx->ts, GS(lx->ts)->globals, &k, &val);
}


/* 
 * Create new global variable; check for redefinition;
 * check for invalid initialization.
 */
static void defineglobal(Lexer *lx, OString *name, int mod) {
    TValue k, out;
    setv2s(lx->ts, &k, name);
    if (cr_unlikely(cr_htable_get(GS(lx->ts)->globals, &k, &out))) {
        if (testbits(vmod(&out), UNDEFMODMASK)) { /* assign before definition */
            cr_parser_semerror(lx, cr_string_pushfstring(lx->ts, 
            "definition of already present global variable '%s'", name));
        } else { /* redefinition of defined global variable */
            checkvarcollision(lx->fs, name, "global", NULL);
        }
    }
    newglobal(lx, name, mod);
}


/* get global variable 'name' or create undefined global */
static void globalvar(FunctionState *fs, OString *name, ExpInfo *e) {
    Lexer *lx = fs->lx;
    TValue k, dummy;
    setv2s(lx->ts, &k, name);
    setemptyval(&dummy); /* set as undefined */
    if (!cr_htable_get(GS(lx->ts)->globals, &k, &dummy))
        newglobal(lx, name, UNDEFMODMASK >> (fs->prev != NULL));
    e->u.str = name;
    e->et = EXP_GLOBAL;
}


/* create new static variable */
static int newstatic(FunctionState *fs, OString *name, int mods) {
    Function *fn = fs->fn;
    cr_State *ts = fs->lx->ts;
    checklimit(fs, fs->nstatics, MAXLONGARGSIZE, "statics");
    cr_mem_growvec(ts, fn->statics, fn->sizestatics, fs->nstatics,
                   MAXLONGARGSIZE, "statics");
    SVar *svar = &fn->statics[fs->nstatics++];
    svar->s.name = name;
    vmod(&svar->val) = (UNDEFMODMASK | mods);
    return fs->nstatics - 1;
}


/* find variable 'name' */
static void var(Lexer *lx, OString *name, ExpInfo *e) {
    searchvar(lx->fs, name, e, 1);
    if (e->et == EXP_VOID)
        globalvar(lx->fs, name, e);
}


#define varlit(lx,l,e)      var(lx, cr_lex_newstring(lx, "" l, SLL(l)), e)



/*---- EXRESSIONS ----*/
 

/* varstatic ::= '@' name */
static void varstatic(Lexer *lx, ExpInfo *e) {
    cr_lex_scan(lx); /* skip '@' */
    OString *name = expect_id(lx);
    int vidx = searchstatic(lx->fs, name, e);
    if (cr_unlikely(vidx < 0))
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts, 
                               "undefined static variable '%s'", name));
    initexp(e, EXP_STATIC, vidx);
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
        cr_lex_scan(lx);
        return;
    }
    cr_lex_syntaxerror(lx, "can't use 'self' outside of class declaration");
}


/*
 * exprlist ::= expr
 *            | expr ',' exprlist
 */
static int exprlist(Lexer *lx, ExpInfo *e) {
    int n = 1;
    expr(lx, e);
    while (match(lx, ',')) {
        cr_code_exp2stack(lx->fs, e);
        expr(lx, e);
        n++;
    }
    return n;
}


/* indexed ::= '[' expr ']' */
static void indexed(Lexer *lx, ExpInfo *var, int super) {
    cr_lex_scan(lx); /* skip '[' */
    if (cr_unlikely(eisconstant(var)))
        cr_parser_semerror(lx, "can't index literal constant values");
    cr_code_exp2stack(lx->fs, var);
    ExpInfo key;
    expr(lx, &key);
    cr_code_indexed(lx->fs, var, &key, super);
    expect(lx, ']');
}


/* getfield ::= '.' id */
static void getfield(Lexer *lx, ExpInfo *var, int super) {
    cr_lex_scan(lx); /* skip '.' */
    cr_code_exp2stack(lx->fs, var);
    ExpInfo key;
    expid(lx, &key);
    cr_code_getproperty(lx->fs, var, &key, super);
    cr_lex_scan(lx);
}


/* 
 * superkw ::= 'super' '.' id 
 *           | 'super' '[' id ']'
 *           | 'super' '[' string ']' 
 */
static void superkw(Lexer *lx, ExpInfo *e) {
    if (lx->ps->cs == NULL) {
        cr_lex_syntaxerror(lx, "can't use 'super' outside of class declaration");
    } else if (!lx->ps->cs->super) {
        cr_lex_syntaxerror(lx, "class has no superclass");
    } else {
        FunctionState *fs = lx->fs;
        varlit(lx, "self", e);
        cr_assert(e->et == EXP_LOCAL);
        cr_code_exp2stack(fs, e);
        cr_assert(e->et == EXP_FINEXPR);
        varlit(lx, "super", e);
        cr_assert(e->et == EXP_UVAL);
        cr_lex_scan(lx);
        if (check(lx, '['))
            indexed(lx, e, 1);
        else if (check(lx, '.')) 
            getfield(lx, e, 1);
        else 
            cr_lex_syntaxerror(lx, "missing method access after 'super'");
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
        cr_lex_scan(lx); /* skip ')' */
        expr(lx, e);
        expect(lx, ')');
        cr_code_varexp2stack(lx->fs, e);
        break;
    case '@':
        varstatic(lx, e);
        cr_lex_scan(lx);
        break;
    case TK_IDENTIFIER:
        var(lx, e->u.str, e);
        cr_lex_scan(lx);
        break;
    case TK_SELF:
        selfkw(lx, e);
        break;
    case TK_SUPER:
        superkw(lx, e);
        break;
    default:
        cr_lex_syntaxerror(lx, "unexpected symbol");
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
    cr_lex_scan(lx); /* skip '(' */
    if (lx->t.tk != ')') { /* have args ? */
        exprlist(lx, e);
        if (eismulret(e))
            cr_code_setreturns(fs, e, CR_MULRET);
    } else {
        e->et = EXP_VOID;
    }
    expect(lx, ')');
    int nparams;
    if (eismulret(e)) {
        nparams = CR_MULRET;
    } else {
        if (e->et != EXP_VOID)
            cr_code_exp2stack(lx->fs, e);
        nparams = fs->sp - base;
    }
    initexp(e, EXP_CALL, cr_code_call(fs, base, nparams, 1));
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
            getfield(lx, e, 0);
            break;
        case '[':
            indexed(lx, e, 0);
            break;
        case '(':
            if (!eisconstant(e))
                cr_lex_syntaxerror(lx, "can't use '()' on constant values");
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
    case TK_INT:
        initexp(e, EXP_INT, 0);
        e->u.i = lx->t.lit.i;
        break;
    case TK_FLT:
        initexp(e, EXP_FLT, 0);
        e->u.n = lx->t.lit.n;
        break;
    case TK_STRING:
        initexp(e, EXP_STRING, 0);
        e->u.str = lx->t.lit.str;
        break;
    case TK_NIL:
        initexp(e, EXP_NIL, 0);
        break;
    case TK_TRUE:
        initexp(e, EXP_TRUE, 0);
        break;
    case TK_FALSE:
        initexp(e, EXP_FALSE, 0);
        break;
    case TK_DOTS:
        expect_cond(lx, lx->fs->fn->isvararg,
                    "cannot use '...' outside of vararg function");
        initexp(e, EXP_VARARG, cr_code_L(lx->fs, OP_VARARG, 2));
        break;
    default:
        suffixedexpr(lx, e);
        return;
    }
    cr_lex_scan(lx);
}


/* get unary operation matching 'token' */
static Unopr getunopr(int token) {
    switch (token) {
    case '-': return OPR_UMIN;
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
    [OPR_NOT]       = { 14, 14 },   /* '!' */
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
    entercstack(lx);
    Unopr uopr = getunopr(lx->t.tk);
    if (uopr != OPR_NOUNOPR) {
        cr_lex_scan(lx); /* skip operator */
        subexpr(lx, e, priority[uopr].right);
        cr_code_unary(lx->fs, e, uopr);
    } else {
        simpleexpr(lx, e);
    }
    Binopr opr = getbinopr(lx->t.tk);
    while (opr != OPR_NOBINOPR && priority[opr].left > limit) {
        cr_lex_scan(lx); /* skip operator */
        cr_code_prebinary(lx->fs, e, opr);
        ExpInfo e2;
        Binopr next = subexpr(lx, &e2, priority[opr].right);
        cr_code_binary(lx->fs, e, &e2, opr);
        opr = next;
    }
    leavecstack(lx);
    return opr;
}


/* expr ::= subexpr */
static void expr(Lexer *lx, ExpInfo *e) {
    subexpr(lx, e, 0);
}



/*--- STATEMENTS ---*/
/* -------------------------------------------------------------------------
 * EXPR statement
 * ------------------------------------------------------------------------- */

/* check if 'var' is const (read-only) */
static void checkconst(Lexer *lx, ExpInfo *var) {
    FunctionState *fs = lx->fs;
    OString *id = NULL;
    switch (var->et) {
    case EXP_UVAL: {
        UpValInfo *uv = getupvalue(fs, var->u.info);
        if (vmod(uv) & VARCONST)
            id = uv->name;
        break;
    }
    case EXP_LOCAL: {
        LVar *lv = getlocal(fs, var->u.info);
        if (vmod(&lv->val) & VARCONST)
            id = lv->s.name;
        break;
    }
    case EXP_STATIC: {
        SVar *sv = getstatic(fs, var->u.info);
        if (vmod(&sv->val) & VARCONST)
            id = sv->s.name;
        break;
    }
    case EXP_GLOBAL: {
        TValue key, res;
        setv2s(lx->ts, &key, var->u.str);
        if (cr_htable_get(GS(lx->ts)->globals, &key, &res)) {
            if (!ttisempty(&res) && vmod(&res) & VARCONST)
                id = var->u.str;
        }
        break;
    }
    default: return; /* only runtime knows :( */
    }
    if (id) {
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
                           "attempt to assign to const variable '%s'", id));
    }
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
        cr_code_setreturns(fs, e, extra);
    } else {
        if (e->et != EXP_VOID)
            cr_code_exp2stack(fs, e);
        if (need > 0)
            cr_code_nil(fs, need);
    }
    if (need > 0) /* need to reserve stack slots ? */
        cr_code_reserveslots(fs, need);
    else
        fs->sp += need;
}


/* auxiliary function for multiple variable assignment */
static void assign(Lexer *lx, struct LHS *lhs, int nvars) {
    expect_cond(lx, eisvar(&lhs->e), "expect variable");
    checkconst(lx, &lhs->e);
    if (match(lx, ',')) { /* more vars ? */
        struct LHS var;
        var.prev = lhs;
        suffixedexpr(lx, &var.e);
        entercstack(lx);
        assign(lx, &var, nvars + 1);
        leavecstack(lx);
    } else { /* right side of assignment '=' */
        ExpInfo e;
        expect(lx, '=');
        int nexps = exprlist(lx, &e);
        if (nexps != nvars)
            adjustassign(lx, nexps, nvars, &e);
        else
            cr_code_exp2stack(lx->fs, &e);
    }
    if (lhs->e.et == EXP_GLOBAL)
        cr_code_defineglobal(lx->fs, &lhs->e);
    else
        cr_code_storevar(lx->fs, &lhs->e);
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



/* -------------------------------------------------------------------------
 * LET declaration
 * ------------------------------------------------------------------------- */

/* get type modifier */
static int getmod(Lexer *lx) {
    const char *mod = getstrbytes(expect_id(lx));
    if (strcmp(mod, "const") == 0)
        return VARCONST;
    else if (strcmp(mod, "static") == 0)
        return VARSTATIC;
    else if (strcmp(mod, "close") == 0)
        return VARTBC;
    else
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
                               "unknown modifier '%s'", mod));
}


/* get all type modifier */
static int getmodifiers(Lexer *lx, const char *type, const char *name) {
    if (!match(lx, '<')) 
        return 0;
    int bmask = 0;
    do {
        int bit = getmod(lx);
        if (testbit(bmask, bit)) { /* duplicate modifier ? */
            cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
            "%s '%s' has duplicate '%s' modifier", type, name, lx->t.lit.str));
        } else if (testbit(bmask, VARSTATIC) && bit == VARSTATIC) {
            cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
            "%s '%s' can't have both static and close modifiers", type, name));
        }
        bmask = bit2mask(bmask, bit);
    } while (match(lx, ','));
    expect(lx, '>');
    return bmask;
}


/* create new variable 'name' */
static int newvar(FunctionState *fs, OString *name, ExpInfo *e, int mods) {
    int vidx = -1;
    if (fs->scope->prev) { /* local ? */
        if (cr_unlikely(testbit(mods, VARSTATIC)))
            cr_parser_semerror(fs->lx, cr_string_pushfstring(fs->lx->ts,
            "can't define local variables ('%s') as static", name));
        checkvarcollision(fs, name, "local", searchlocal);
        vidx = newlocal(fs->lx, name, mods);
        initexp(e, EXP_LOCAL, vidx);
    } else { /* global */
        cr_assert(!testbit(mods, VARTBC)); /* can't have close */
        if (mods & VARSTATIC) {
            checkvarcollision(fs, name, "static", searchstatic);
            vidx = newstatic(fs, name, mods);
            initexp(e, EXP_STATIC, vidx);
        } else {
            defineglobal(fs->lx, name, mods);
            initglobal(e, name);
        }
    }
    return vidx;
}


/* mark local variable at 'idx' as tbc */
static void closelocal(FunctionState *fs, int idx) {
    cr_assert(idx >= 0);
    scopemarktbc(fs);
    cr_code_L(fs, OP_TBC, idx);
}


/* try to mark variable defined as tbc */
static void closedecl(FunctionState *fs, int nvars, int *tbc, int vidx,
                      int mods)
{
    if (mods & VARTBC) { /* have tbc var ? */
        cr_assert(!testbit(mods, VARSTATIC)); /* can't be static */
        if (cr_unlikely(*tbc == -1)) { /* already have tbc variable ? */
            cr_parser_semerror(fs->lx,
            "multiple to-be-closed variables in a single declaration");
        } else if (cr_unlikely(vidx == -1)) { /* global to-be-closed ? */
            cr_parser_semerror(fs->lx, "global variables can't be closed");
        }
        *tbc = fs->activelocals + nvars;
        cr_assert(tbc >= 0);
        closelocal(fs, *tbc);
    }
}


/* auxiliary for assignment of multiple variable definitions */
static void assigndefine(Lexer *lx, struct LHS *lhs, int *tbc, int nvars) {
    FunctionState *fs = lx->fs;
    if (match(lx, ',')) { /* more ids ? */
        struct LHS var;
        var.prev = lhs;
        OString *name = expect_id(lx);
        int mods = getmodifiers(lx, "variable", getstrbytes(name));
        int vidx = newvar(fs, name, &var.e, mods);
        closedecl(fs, nvars, tbc, vidx, mods);
        entercstack(lx);
        assigndefine(lx, &var, tbc, nvars + 1);
        leavecstack(lx);
    } else { /* right side of declaration */
        ExpInfo e;
        expect(lx, '=');
        int nexps = exprlist(lx, &e);
        if (nexps != nvars)
            adjustassign(lx, nexps, nvars, &e);
        else
            cr_code_exp2stack(fs, &e);
    }
    codedefine(fs, &lhs->e);
}


/* 
 * letdecl ::= 'let' idlist ';'
 *           | 'let' idlist '=' exprlist ';'
 */
static void letdecl(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int tbc = -1; /* -1 indicates there are no tbc variables */
    struct LHS var;
    cr_lex_scan(lx); /* skip 'let' */
    OString *name = expect_id(lx);
    int mods = getmodifiers(lx, "variable", getstrbytes(name));
    int vidx = newvar(fs, name, &var.e, mods);
    closedecl(fs, 0, &tbc, vidx, mods);
    if (check(lx, ',') || check(lx, '=')) { /* id list ? */
        var.prev = NULL;
        assigndefine(lx, &var, &tbc, 1);
    } else { /* otherwise assign implicit nil */
        cr_code_nil(lx->fs, 1);
        cr_code_reserveslots(fs, 1);
        codedefine(lx->fs, &var.e);
    }
    expect(lx, ';');
}



/* -------------------------------------------------------------------------
 * BLOCK statement
 * ------------------------------------------------------------------------- */

/*
 * block ::= stm
 *         | block 
 */
static void block(Lexer *lx) {
    while (!check(lx, '}') && !check(lx, TK_EOS))
        stm(lx);
}


/* blockstm ::= '{' block '}' */
static void blockstm(Lexer *lx) {
    Scope s;
    int matchline = lx->line;
    cr_lex_scan(lx); /* skip '{' */
    startscope(lx->fs, &s, 0);
    block(lx);
    endscope(lx->fs);
    expectmatch(lx, '}', '{', matchline);
}



/* -------------------------------------------------------------------------
 * FN statement
 * ------------------------------------------------------------------------- */

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
                cr_lex_scan(lx);
                isvararg = 1;
                break;
            }
            default: cr_lex_syntaxerror(lx, "<identifier> or '...' expected");
            }
        } while (!isvararg && match(lx, ','));
    }
    adjustlocals(lx, nargs);
    fn->arity = nargs;
    if (isvararg)
        setvararg(fs, nargs);
    cr_code_reserveslots(fs, nargs);
}


/* emit closure instruction */
static void codeclosure(Lexer *lx) {
    FunctionState *fs = lx->fs;
    cr_code_L(fs, OP_CLOSURE, fs->nfuncs);
}


/* funcbody ::= '(' arglist ')' block */
static void funcbody(Lexer *lx, int linenum, int ismethod) {
    FunctionState newfs;
    Scope scope;
    newfs.fn = addfunction(lx);
    newfs.fn->defline = linenum;
    startfs(lx->fs, lx, &scope);
    int matchline = lx->line; /* line where '(' is located */
    expect(lx, '(');
    if (ismethod) { /* is this class method ? */
        newlocallit(lx, "self"); /* create 'self' */
        adjustlocals(lx, 1); /* and initialize it */
    }
    argslist(lx);
    expectmatch(lx, ')', '(', matchline);
    expect(lx, ')');
    matchline = lx->line; /* line where '{' is located */
    expect(lx, '{');
    block(lx);
    expectmatch(lx, '}', '{', matchline);
    codeclosure(lx);
    endfs(&newfs);
}


/* 
 * declname ::= '@' id
 *            | id
 */
static OString *defname(Lexer *lx, ExpInfo *e) {
    int mods = match(lx, '@') * bitmask(VARSTATIC);
    OString *name = expect_id(lx);
    int vidx = newvar(lx->fs, name, e, mods);
    if (e->et == EXP_LOCAL) {
        getlocal(lx->fs, vidx)->s.idx = vidx;
        adjustlocals(lx, 1);
    } else if (e->et == EXP_STATIC) {
        definestatic(lx->fs, vidx);
    }
    return name;
}


/* fnstm ::= 'fn' funcname funcbody */
static void fndecl(Lexer *lx, int linenum) {
    ExpInfo var;
    cr_lex_scan(lx); /* skip 'fn' */
    defname(lx, &var);
    funcbody(lx, linenum, 0);
    codedefine(lx->fs, &var);
}



/* -------------------------------------------------------------------------
 * CLASS declaration
 * ------------------------------------------------------------------------- */

static void codeinherit(Lexer *lx, OString *name, Scope *s, ExpInfo *var) {
    FunctionState *fs = lx->fs;
    OString *supname = expect_id(lx);
    if (cr_unlikely(streq(name, supname))) {
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
        "class '%s' attempt to inherit from itself", name));
    }
    searchvar(fs, supname, var, 1); /* find superclass */
    cr_code_exp2stack(fs, var); /* put superclass on the stack */
    startscope(fs, s, 0);
    newlocallit(lx, "super"); /* local var for superclass */
    adjustlocals(lx, 1);
    searchvar(fs, name, var, 1); /* search for class */
    cr_code_exp2stack(fs, var); /* put class on the stack */
    cr_code(fs, OP_INHERIT);
    lx->ps->cs->super = 1;
}


static void startcs(Lexer *lx, ClassState *cs) {
    ParserState *ps = lx->ps;
    cs->prev = ps->cs;
    cs->super = 0;
    ps->cs = cs;
}


static void endcs(Lexer *lx) {
    ParserState *ps = lx->ps;
    cr_assert(ps->cs != NULL);
    if (ps->cs->super)
        endscope(lx->fs);
    ps->cs = ps->cs->prev;
}


static void codemethod(Lexer *lx) {
    ExpInfo var;
    expid(lx, &var);
    if (sisvmtmethod(var.u.str))
        cr_code_L(lx->fs, OP_OVERLOAD, var.u.str->extra);
    else
        cr_code_method(lx->fs, &var);
}


static void method(Lexer *lx) {
    int defline = lx->line; /* method definition start line */
    expect(lx, TK_FN);
    int matchline = lx->line; /* line to match '{' */
    expect(lx, '{');
    codemethod(lx);
    funcbody(lx, defline, 1);
    expectmatch(lx, '}', '{', matchline);
}


static void classbody(Lexer *lx) {
    while (!check(lx, '}') && !check(lx, TK_EOS))
        method(lx);
}


/* 
 * classdecl ::= 'class' id classbody
 *             | 'class' id '<' idsup classbody
 */
static void classdecl(Lexer *lx) {
    Scope s; /* scope for 'super' */
    ClassState cs;
    ExpInfo var, e;
    cr_lex_scan(lx); /* skip 'class' */
    OString *name = defname(lx, &var);
    startcs(lx, &cs);
    initstring(&e, name);
    cr_code_class(lx->fs, &e);
    if (match(lx, TK_INHERITS)) /* class inherits ? */
        codeinherit(lx, name, &s, &e);
    searchvar(lx->fs, name, &e, 1); /* find class */
    cr_assert(var.et == EXP_LOCAL); /* 'name' must be local */
    cr_code_exp2stack(lx->fs, &e); /* put class on stack */
    int matchline = lx->line;
    expect(lx, '{');
    classbody(lx);
    expectmatch(lx, '}', '{', matchline);
    endcs(lx);
    codedefine(lx->fs, &var);
}



/* -------------------------------------------------------------------------
 * SWITCH statement
 * ------------------------------------------------------------------------- */

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


/* convert literal information into string */
static const char *li2text(cr_State *ts, LiteralInfo *li) {
    switch (li->tt) {
    case CR_VSTRING:
        return cr_string_pushfstring(ts, " (%s)", getstrbytes(li->lit.str));
    case CR_VNUMINT: return cr_string_pushfstring(ts, " (%I)", li->lit.i);
    case CR_VNUMFLT: return cr_string_pushfstring(ts, " (%N)", li->lit.n);
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
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
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
        cr_mem_growvec(lx->ts, ss->literals.arr, ss->literals.size,
                       ss->literals.len, MAXLONGARGSIZE, "switch literals");
        ss->literals.arr[ss->literals.len++] = li;
        if (eisconstant(&ss->e)) { /* both are constant expressions ? */
            TValue v1, v2;
            movexp2v(lx->fs, &ss->e, &v1);
            movexp2v(lx->fs, caseexp, &v2);
            return cr_value_orderEQ(lx->ts, &v1, &v2);
        }
    }
    return 0;
}


/* add new fall-through case jump */
static void addfallthrujmp(FunctionState *fs) {
    int jmp = cr_code_jmp(fs, OP_JMP);
    patchlistaddjmp(fs, jmp);
}


/* 
 * Tries to preserve expression 'e' after consuming
 * it, in order to enable more optimizations.
 */
static int codepresexp(FunctionState *fs, ExpInfo *e) {
    ExpInfo pres = *e;
    int isctc = eisconstant(e);
    cr_code_exp2stack(fs, e);
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
static void switchbody(Lexer *lx, SwitchState *ss, struct DynCtx *ctx) {
    struct DynCtx endctx;
    FunctionState *fs = lx->fs;
    int jmp = NOJMP;
    endctx.pc = ctx->pc;
    while (!check(lx, '}') && !check(lx, TK_EOS)) {
        if (check(lx, TK_CASE) || match(lx, TK_DEFAULT)) { /* have case ? */
            if (ss->label != LABELNONE && ss->label != LABELMATCH) {
                addfallthrujmp(fs);
                if (ss->label == LABELCASE)
                    cr_code_patchtohere(fs, ss->jmp);
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
                } else if (ss->label != LABELMATCH) { /* don't have ctmatch ? */
                    ss->label = LABELCASE;
                    cr_code(fs, OP_EQPRESERVE);
                    ss->jmp = cr_code(fs, OP_JFPOP);
                }
            } else if (!ss->havedefault) { /* first 'default' case ? */
                ss->havedefault = 1;
                ss->label = LABELDFL;
                expect(lx, ':');
            } else {
                cr_parser_semerror(lx, "multiple default cases in switch");
            }
            if ((jmp = patchlistpop(fs)) != NOJMP) /* have jump to patch ? */
                cr_code_patchtohere(fs, jmp);
        } else if (ss->label != LABELNONE) {
            stm(lx);
            /* if previous statement is 'returnstm' and previous
             * case was a compile-time match, store current
             * context into 'endctx' */
            if (ss->label == LABELMATCH && fs->laststmisret)
                storectx(fs, &endctx);
        } else {
            cr_parser_semerror(lx, "expected 'case' or 'default'");
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
    struct DynCtx ctx;
    Scope *oldswitchscope = fs->switchscope;
    Scope s; /* switch scope */
    initss(&ss);
    startscope(fs, &s, CFSWITCH);
    fs->switchscope = &s;
    cr_lex_scan(lx); /* skip 'switch' */
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



/* -------------------------------------------------------------------------
 * IF statement
 * ------------------------------------------------------------------------- */

/* condition statement body; for 'forloop', 'whilestm' & 'ifstm' */
static void condbody(Lexer *lx, struct DynCtx *startctx, ExpInfo *cond,
                     OpCode jfop, OpCode jop, int condpc, int endclausepc)
{
    FunctionState *fs = lx->fs;
    struct DynCtx endctx;
    int fjmp, jmp;
    int optaway, condistrue;
    int condisctc = eisconstant(cond);
    int bodypc = currentPC(fs);
    int isloop = scopeisloop(fs->scope);
    cr_assert(isloop == (jop == OP_JMPS));
    optaway = condistrue = 0;
    endctx.pc = NOJMP;
    if (condisctc && !(condistrue = eistrue(cond)))
        optaway = 1;
    else
        fjmp = cr_code_jmpf(fs, jfop);
    stm(lx);
    if (optaway) {
        loadctx(fs, startctx);
        resetpatchlist(fs);
    } else if (condisctc && fs->laststmisret) {
        cr_assert(condistrue);
        storectx(fs, &endctx);
    } else {
        jmp = cr_code_jmp(fs, jop);
        if (isloop) { /* loop body ? */
            if (endclausepc != NOJMP) { /* 'for' loop ? */
                cr_code_patch(fs, jmp, endclausepc);
            } else if (condistrue) { /* 'while' loop with true ctcond ? */
                cr_code_patch(fs, jmp, bodypc);
                fs->loopstart = bodypc;
            } else { /* else 'while' loop with unknown non-ctcond ? */
                cr_code_patch(fs, jmp, condpc);
            }
        }
    }
    if (!optaway)
        cr_code_patchtohere(fs, fjmp);
    if (!isloop && match(lx, TK_ELSE)) {
        stm(lx);
        if (!optaway && !condisctc)
            cr_code_patchtohere(fs, jmp);
    }
    if (endctx.pc != NOJMP)
        loadctx(fs, &endctx);
}


/* 
 * ifstm ::= 'if' '(' expr ')' condbody
 */
static void ifstm(Lexer *lx) {
    struct DynCtx ctx;
    ExpInfo e;
    cr_lex_scan(lx); /* skip 'if' */
    storectx(lx->fs, &ctx);
    int matchline = lx->line;
    expect(lx, '(');
    expr(lx, &e);
    codepresexp(lx->fs, &e);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &ctx, &e, OP_JFPOP, OP_JMP, NOJMP, NOJMP);
}



/* -------------------------------------------------------------------------
 * WHILE statement
 * ------------------------------------------------------------------------- */

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
    struct DynCtx startctx;
    struct LoopCtx lctx;
    Scope s; /* new 'loopscope' */
    ExpInfo cond;
    cr_lex_scan(lx); /* skip 'while' */
    storectx(fs, &startctx);
    startloop(fs, &s, &lctx, CFLOOP);
    int pcexpr = currentPC(fs);
    int matchline = lx->line;
    expect(lx, '(');
    expr(lx, &cond);
    codepresexp(fs, &cond);
    expectmatch(lx, ')', '(', matchline);
    condbody(lx, &startctx, &cond, OP_JFANDPOP, OP_JMPS, pcexpr, NOJMP);
    endloop(fs);
}


/* number of state variables for generic for loop */
#define NUMSTATEVARS    4


static void foreachvar(Lexer *lx) {
    OString *name = expect_id(lx);
    int mods = getmodifiers(lx, "variable", getstrbytes(name));
    if (cr_unlikely(testbits(mods, bit2mask(VARSTATIC, VARTBC)))) {
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
        "local 'for each' loop variable '%s' can only have 'const' modifier",
        name));
    }
    newlocal(lx, expect_id(lx), mods);
}


/* patch for loop jump(back) */
static void patchforjmp(FunctionState *fs, int pc, int target) {
    Instruction *jmp = &fs->fn->code[pc];
    int offset = pc - target;
    if (cr_unlikely(offset > MAXLONGARGSIZE))
        cr_lex_syntaxerror(fs->lx, "control structure (for loop) too long");
    SETARG_L(jmp, 1, offset);
}


/* generic for loop expressions */
static int forexprlist(Lexer *lx, ExpInfo *e, int limit) {
    int nexpr = 1;
    expr(lx, e);
    if (cr_unlikely(eisconstant(e))) {
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
        "'%s' is invalid iterator function (for loop)",
        cr_lex_tok2str(lx, lx->t.tk)));
    }
    while (match(lx, ',')) {
        cr_code_exp2stack(lx->fs, e);
        expr(lx, e);
        nexpr++;
    }
    if (cr_unlikely(nexpr > limit))
        limiterror(lx->fs, "expressions", limit);
    return nexpr;
}


/* foreachloop ::= 'for' 'each' idlist 'in' forexprlist '{' block '}' */
static void foreachloop(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct DynCtx startctx;
    struct LoopCtx lctx;
    ExpInfo e;
    Scope s;
    int nvars = 1; /* iter func result */
    int base = fs->sp;
    storectx(fs, &startctx);
    newlocallit(lx, "(for state)"); /* iter func */
    newlocallit(lx, "(for state)"); /* invariant state */
    newlocallit(lx, "(for state)"); /* control var */
    newlocallit(lx, "(for state)"); /* to-be-closed var */
    foreachvar(lx); /* iter func result var */
    while (match(lx, ',')) {
        foreachvar(lx);
        nvars++;
    }
    expect(lx, TK_IN);
    adjustassign(lx, NUMSTATEVARS, forexprlist(lx, &e, NUMSTATEVARS), &e);
    adjustlocals(lx, NUMSTATEVARS); /* register state vars */
    scopemarktbc(fs);
    /* runtime space for call (iter func), inv. state, control var */
    cr_code_checkstack(fs, NUMSTATEVARS - 1);
    int prep = cr_code_S(fs, OP_FORPREP, base);
    startloop(fs, &s, &lctx, CFLOOP); /* scope for declared vars */
    adjustlocals(lx, nvars);
    cr_code_reserveslots(fs, nvars); /* locals */
    int matchline = lx->line;
    expect(lx, '{');
    block(lx);
    expectmatch(lx, '}', '{', matchline);
    endloop(fs); /* end scope for declared vars */
    int forend = cr_code_S(fs, OP_FORLOOP, base);
    patchforjmp(fs, forend, prep + INSTSIZE + ARGLSIZE);
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
        int infloop = eistrue(cond);
        if (!infloop)
            bodyjmp = cr_code_jmp(fs, OP_JMP);
        *clausepc = currentPC(fs);
        exprstm(lx);
        if (!infloop) {
            loopjmp = cr_code_jmp(fs, OP_JMPS);
            cr_code_patch(fs, loopjmp, fs->loopstart);
            cr_code_patchtohere(fs, bodyjmp);
        }
        fs->loopstart = *clausepc;
    }
}


/* forloop ::= 'for' '(' expr ')' condbody */
static void forloop(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct DynCtx startctx;
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
    condbody(lx, &startctx, &cond, OP_JFPOP, OP_JMPS, condpc, endclausepc);
    endloop(fs);
}


/* -------------------------------------------------------------------------
 * LOOP statement
 * ------------------------------------------------------------------------- */

/* 
 * forstm ::= foreachloop
 *          | forloop
 */
static void forstm(Lexer *lx) {
    cr_lex_scan(lx); /* skip 'for' */
    if (match(lx, TK_EACH))
        foreachloop(lx);
    else
        forloop(lx);
}


/* -------------------------------------------------------------------------
 * LOOP statement
 * ------------------------------------------------------------------------- */

/* loopstm ::= 'loop' stm */
static void loopstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopCtx ctx;
    Scope s;
    cr_lex_scan(lx); /* skip 'loop' */
    startloop(fs, &s, &ctx, CFLOOP);
    stm(lx);
    endloop(fs);
}


/* -------------------------------------------------------------------------
 * CONTINUE statement
 * ------------------------------------------------------------------------- */

/*
 * continuestm ::= 'continue' ';'
 */
static void continuestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    cr_lex_scan(lx); /* skip 'continue' */
    if (cr_unlikely(fs->loopstart == NOJMP)) { /* no loop ? */
        cr_parser_semerror(lx, "'continue' not in loop statement");
    } else {
        cr_assert(fs->loopscope != NULL);
        int popn = fs->activelocals - fs->scope->nlocals + fs->nswscopes;
        cr_code_pop(fs, popn);
        fs->sp -= popn;
        int jmp = cr_code_jmp(fs, OP_JMPS);
        cr_code_patch(fs, jmp, fs->loopstart);
    }
    expect(lx, ';');
}


/* -------------------------------------------------------------------------
 * BREAK statement
 * ------------------------------------------------------------------------- */

/* 
 * Get control flow scope.
 * Scopes are stack allocated, 'fs' keeps pointers to them.
 * Smaller scope ptr memory address == more recent scope.
 */
cr_sinline Scope *getcfscope(FunctionState *fs) {
    cr_assert((fs->loopscope && fs->switchscope) ==
                (fs->loopscope != fs->switchscope));
    return (fs->loopscope < fs->switchscope ? fs->loopscope : fs->switchscope);
}


/*
 * breakstm ::= 'break' ';'
 */
static void breakstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    const Scope *s = getcfscope(fs);
    cr_lex_scan(lx); /* skip 'break' */
    if (cr_unlikely(!scopeisswitch(s) && !scopeisloop(s))) { /* error ? */
        cr_parser_semerror(lx, "'break' not in loop or switch statement");
    } else { /* otherwise add the jmp to patch list */
        int actvars = fs->activelocals - s->nlocals;
        cr_code_pop(fs, actvars + scopeisswitch(s));
        fs->sp -= actvars;
        patchlistaddjmp(fs, cr_code_jmp(fs, OP_JMP));
    }
    expect(lx, ';');
}


/* -------------------------------------------------------------------------
 * RETURN statement
 * ------------------------------------------------------------------------- */

/* 
 * returnstm ::= 'return' ';' 
 *             | 'return' exprlist ';'
 */
static void returnstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    // TODO
    fs->laststmisret = 1;
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
    case ';': cr_lex_scan(lx); break;
    default: exprstm(lx); expect(lx, ';'); break;
    }
    lx->fs->laststmisret = 0;
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
static void mainfunc(FunctionState *fs, Lexer *lx)
{
    Scope s;
    startfs(fs, lx, &s);
    setvararg(fs, 0); /* main is always vararg */
    cr_lex_scan(lx); /* scan first token */
    parseuntilEOS(lx);
    cr_assert(lx->t.tk == TK_EOS);
    endfs(fs);
}


/* parse source code */
CrClosure *parse(cr_State *ts, BuffReader *br, Buffer *buff, ParserState *ps,
                 const char *source)
{
    Lexer lx;
    FunctionState fs;
    CrClosure *cl = cr_function_newcrclosure(ts, 0);
    setsv2crcl(ts, ts->stacktop.p, cl); /* anchor main function closure */
    cr_vm_inctop(ts);
    lx.tab = cr_htable_new(ts);
    setsv2ht(ts, ts->stacktop.p, lx.tab); /* anchor scanner htable */
    cr_vm_inctop(ts);
    fs.fn = cl->fn = cr_function_new(ts);
    cr_gc_objbarrier(ts, cl, cl->fn);
    fs.fn->source = cr_string_new(ts, source);
    cr_gc_objbarrier(ts, fs.fn, fs.fn->source);
    lx.ps = ps;
    lx.buff = buff;
    cr_lex_setsource(ts, &lx, br, fs.fn->source);
    mainfunc(&fs, &lx); /* Cript main function */
    ts->stacktop.p--; /* pop scanner htable */
    return cl;
}



/* -------------------------------------------------------------------------
 * Protected parsing
 * ------------------------------------------------------------------------- */

/* data for 'pparse' */
typedef struct PPData {
    BuffReader br;
    Buffer buff;
    ParserState ps;
    const char *source;
} PPData;


/* protected 'parse' */
static void pparse(cr_State *ts, void *userdata)
{
    PPData *parsedata = cast(PPData *, userdata);
    CrClosure *cl = parse(ts, &parsedata->br, &parsedata->buff, &parsedata->ps,
                          parsedata->source);
    cr_assert(cl->nupvals <= cl->sizeupvals);
    cr_function_initupvals(ts, cl);
}


/* external interface for 'pparse' */
void cr_parser_pparse(cr_State *ts, cr_reader freader, void *userdata,
                      const char *name)
{
    PPData parsedata;
    ParserState *ps = &parsedata.ps;
    cr_br_init(ts, &parsedata.br, freader, userdata); /* 'br' */
    cr_reader_buffinit(&parsedata.buff); /* 'buff' */
    { ps->lvars.len = ps->lvars.size = 0; ps->lvars.arr = NULL; } /* 'lvars' */
    ps->cs = NULL; /* 'cs' */
    parsedata.source = name; /* 'source' */
    cr_vm_pcall(ts, pparse, &parsedata, savestack(ts, ts->stacktop.p));
}
