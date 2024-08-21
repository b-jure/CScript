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


/* special value for 'mod', describing undefined global vars */
#define UNDEFMOD        ((1 << (VMODSIZE * 8)) - 1)


/* 'cfbits' */
#define CFLOOP      1
#define CFGLOOP     2
#define CFSWITCH    4

/* test 'cfbits' */
#define scopeisloop(s)      testbit((s)->cfbits, CFLOOP)
#define scopeisgloop(s)     testbit((s)->cfbits, CFGLOOP)
#define scopeisswitch(s)    testbit((s)->cfbits, CFSWITCH)


/* lexical scope information */
typedef struct Scope {
    struct Scope *prev; /* implicit linked-list */
    int nlocals; /* number of locals outside of this scope */
    int nstatics; /* number of statics outside of this scope */
    cr_ubyte cfbits; /* control flow bits */
    cr_ubyte haveupval; /* set if scope contains upvalue variable */
    cr_ubyte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;



static cr_noret expecterror(Lexer *lx, int tk)
{
    const char *err = cr_string_pushfstring(lx->ts, "expected %s",
                                            cr_lex_tok2str(lx, tk));
    cr_lex_syntaxerror(lx, err);
}


static cr_noret limiterror(FunctionState *fs, const char *what, int limit)
{
    cr_State *ts = fs->lx->ts;
    int line = fs->fn->defline;
    const char *where = (line == 0 ? "main function" :
                        cr_string_pushfstring(ts, "function at line %d", line));
    const char *err = cr_string_pushfstring(ts, "too many %s (limit is %d) in %s",
                                            what, limit, where);
    cr_lex_syntaxerror(fs->lx, err);
}


/* semantic error; variant of syntax error without 'near <token>' */
cr_noret cr_parser_semerror(Lexer *lx, const char *err)
{
    lx->t.tk = 0;
    cr_lex_syntaxerror(lx, err);
}


static void checklimit(FunctionState *fs, int n, int limit, const char *what)
{
    if (n >= limit)
        limiterror(fs, what, limit);
}


/* variable initialization error */
static cr_noret variniterror(Lexer *lx, const char *vartype, const char *name)
{
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
cr_sinline LVar *getlocal(FunctionState *fs, int idx)
{
    cr_assert(fs->firstlocal + idx < fs->lx->ps->lvars.len);
    return &fs->lx->ps->lvars.arr[fs->firstlocal + idx];
}


/* get global variable value */
cr_sinline void getglobal(cr_State *ts, OString *name, TValue *out)
{
    TValue k;
    setv2s(lx->ts, &k, name);
    cr_htable_get(GS(ts)->globals, &k, out);
}


/* get static variable */
cr_sinline SVar *getstatic(FunctionState *fs, int idx)
{
    cr_assert(fs->firststatic + idx < fs->fn->nsvars);
    return &fs->fn->svars[idx];
}


/* get upvalue variable */
cr_sinline UpValInfo *getupvalue(FunctionState *fs, int idx)
{
    cr_assert(idx < fs->fn->nupvals);
    return &fs->fn->upvals[idx];
}


/* init expression with generic information */
static void initexp(ExpInfo *e, expt et, int info)
{
    e->t = e->f = -1;
    e->et = et;
    e->u.info = info;
}


/* init global variable expression */
static void initglobal(ExpInfo *e, OString *name)
{
    e->t = e->f = NOJMP;
    e->et = EXP_GLOBAL;
    e->u.str = name;
}


static void initstring(ExpInfo *e, OString *s)
{
    e->f = e->t = NOJMP;
    e->et = EXP_STRING;
    e->u.str = s;
}


static Scope *getscope(FunctionState *fs, int idx)
{
    Scope *scope = fs->scope;

    while (scope != NULL) {
        if (scope->nlocals >= idx)
            break;
        scope = scope->prev;
    }
    return scope;
}



static void enterscope(FunctionState *fs, Scope *s, int cfbits)
{
    entercstack(fs->lx);
    s->nlocals = fs->activelocals;
    s->nstatics = fs->activestatics;
    s->cfbits = cfbits;
    s->haveupval = 0;
    s->havetbcvar = (fs->scope != NULL && fs->scope->havetbcvar);
    s->prev = fs->scope;
    fs->scope = s;
}


static void leavescope(FunctionState *fs)
{
    Scope *s = fs->scope;
    fs->scope = s->prev;
    leavecstack(fs->lx);
    /* TODO */
}


/* 
 * Mark scope where variable at idx 'vidx' was defined
 * in order to emit close instruction before the scope
 * gets closed.
 */
static void scopemarkupval(FunctionState *fs, int vidx)
{
    Scope *s = fs->scope;
    while(s->nlocals - 1 > vidx)
        s = s->prev;
    s->haveupval = 1;
    fs->close = 1;
}


/* 
 * Mark current scope as scopee that has a to-be-closed
 * variable.
 */
static void scopemarktbc(FunctionState *fs)
{
    Scope *s = fs->scope;
    s->haveupval = 1;
    s->havetbcvar = 1;
    fs->close = 1;
}


static void startfs(FunctionState *fs, Lexer *lx, Scope *s)
{
    cr_assert(fs->fn != NULL);
    fs->prev = lx->fs;
    fs->lx = lx;
    lx->fs = fs;
    fs->scope = NULL;
    fs->sp = 0;
    fs->activelocals = 0;
    fs->firstlocal = lx->ps->lvars.len;
    fs->firstbreak = lx->ps->breaks.len;
    fs->innerloopstart = -1;
    fs->innerloopdepth = 0;
    fs->innerswitchdepth = 0;
    fs->close = 0;
    fs->fn->source = lx->src;
    cr_gc_objbarrier(lx->ts, fs->fn, fs->fn->source);
    fs->fn->maxstack = 1; /* for the function itself */
    enterscope(fs, s, 0);
}

static void endfs(FunctionState *fs)
{
    cr_State *ts = fs->lx->ts;
    Function *fn = fs->fn;
    Lexer *lx = fs->lx;
    cr_code_ret(fs, fs->activelocals - 1, 0);
    leavescope(fs);
    cr_assert(fs->scope == NULL); /* last scope */
    cr_mem_shrinkvec(ts, fn->fn, fn->sizefn, fn->nfn);
    cr_mem_shrinkvec(ts, fn->constants, fn->sizeconst, fn->nconst);
    cr_mem_shrinkvec(ts, fn->code, fn->sizecode, fn->ncode);
    cr_mem_shrinkvec(ts, fn->linfo, fn->sizelinfo, fn->nlinfo);
    cr_mem_shrinkvec(ts, fn->locals, fn->sizelocals, fn->nlocals);
    cr_mem_shrinkvec(ts, fn->upvals, fn->sizeupvals, fn->nupvals);
    lx->fs = fs->prev;
    cr_gc_check(ts);
}


static Function *addfunction(Lexer *lx)
{
    Function *new;
    cr_State *ts = lx->ts;
    FunctionState *fs = lx->fs;
    Function *fn = fs->fn;
    checklimit(fs, fn->nfn + 1, INT_MAX, "function");
    cr_mem_growvec(ts, fn->fn, fn->sizefn, fn->nfn, INT_MAX, "functions");
    fn->fn[fn->nfn++] = new = cr_function_new(ts);
    cr_gc_objbarrier(ts, fn, new);
    return new;
}


/* set current function as vararg */
static void setvararg(FunctionState *fs, int arity) {
    fs->fn->isvararg = 1;
    cr_code_L(fs, OP_SETVARARG, arity);
}


/* forward declare recursive non-terminals */
static void stm(Lexer *lx);
static void expr(Lexer *lx, ExpInfo *e);



/* check if 'tk' matches the current token */
static void expect(Lexer *lx, int tk)
{
    if (lx->t.tk == tk) {
        cr_lex_scan(lx);
        return;
    }
    expecterror(lx, tk);
}


/* same as 'expectnext' but do not invoke syntax error */
static int match(Lexer *lx, int tk)
{
    if (lx->t.tk == tk) {
        cr_lex_scan(lx);
        return 1;
    }
    return 0;
}


/* same as 'match' but do not advance scanner */
cr_sinline int check(Lexer *lx, int tk)
{
    return (lx->t.tk == tk);
}


static OString *expect_id(Lexer *lx)
{
    expect(lx, TK_IDENTIFIER);
    return lx->t.lit.str;
}



/* -------------------------------------------------------------------------
 * Statements
 * ------------------------------------------------------------------------- */


/* add local debug information into 'locals' */
static int registerlocal(Lexer *lx, FunctionState *fs, OString *name)
{
    Function *fn = fs->fn;
    checklimit(fs, fn->nlocals, MAXLONGARGSIZE, "locals");
    cr_mem_growvec(lx->ts, fn->locals, fn->sizelocals, fn->nlocals,
                   MAXLONGARGSIZE, "locals");
    LVarInfo *lvarinfo = &fn->locals[fn->nlocals++];
    lvarinfo->name = name;
    lvarinfo->alivepc = fn->ncode;
    return fn->nlocals - 1;
}


/*
 * Adjust locals by increment 'nlocals' and registering them
 * inside 'locals'.
 */
static void adjustlocals(Lexer *lx, int nvars)
{
    FunctionState *fs = lx->fs;
    for (int i = 0; i < nvars; nvars--) {
        int idx = fs->activelocals++;
        LVar *local = getlocal(fs, idx);
        local->s.idx = registerlocal(lx, fs, local->s.name);
    }
}


/* adds local variable to the 'lvars' */
static int newlocal(Lexer *lx, OString *name)
{
    FunctionState *fs = lx->fs;
    ParserState *ps = lx->ps;
    checklimit(fs, ps->lvars.len, MAXLONGARGSIZE, "locals");
    cr_mem_growvec(lx->ts, ps->lvars.arr, ps->lvars.size, ps->lvars.len,
                   MAXLONGARGSIZE, "locals");
    LVar *local = &ps->lvars.arr[ps->lvars.len++];
    local->s.name = name;
    local->s.idx = -1;
    return ps->lvars.len - fs->firstlocal - 1;
}


/* same as 'addlocal' but use string literal as name */
#define newlocallit(lx,lit) \
    newlocal(lx, cr_lex_newstring(lx, "" lit, SLL(lit)))


/*
 * Searches for local variable 'name'.
 */
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e)
{
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
static int searchstatic(FunctionState *fs, OString *name, ExpInfo *e)
{
    Lexer *lx = fs->lx;
    for (int i = fs->activestatics - 1; i >= 0; i--) {
        SVar *svar = getstatic(fs, i);
        if (streq(name, svar->s.name)) {
            if (cr_unlikely(svar->s.idx == -1))
                variniterror(lx, "static", getstrbytes(name));
            initexp(e, EXP_STATIC, i);
            return e->et;
        }
    }
    return -1;
}


/* allocate space for new 'UpValInfo' */
static UpValInfo *newupvalue(FunctionState *fs)
{
    Function *fn = fs->fn;
    cr_State *ts = fs->lx->ts;
    checklimit(fs, fn->nupvals + 1, MAXLONGARGSIZE, "upvalues");
    cr_mem_growvec(ts, fn->upvals, fn->sizeupvals, fn->nupvals, MAXLONGARGSIZE,
                   "upvalues");
    return &fn->upvals[fn->nupvals++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *e)
{
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
        cr_assert(streq(name, enclosing->fn->upvalues[e->u.info].name));
    }
    return fs->fn->nupvals - 1;
}


/* searches for upvalue 'name' */
static int searchupvalue(FunctionState *fs, OString *name)
{
    Function *fn = fs->fn;
    for (int i = 0; i < fn->nupvals; i++)
        if (streq(fn->upvals[i].name, name)) 
            return i;
    return -1;
}


/*
 * Search for variable; if 'name' is not local variable
 * try finding the upvalue.
 */
static void searchvar(FunctionState *fs, OString *name, ExpInfo *e, int base)
{
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
static void newglobal(Lexer *lx, OString *name, int mod)
{
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
static void defineglobal(Lexer *lx, OString *name, int mod)
{
    TValue k, out;
    setv2s(lx->ts, &k, name);
    if (cr_unlikely(cr_htable_get(GS(lx->ts)->globals, &k, &out))) {
        if (vmod(&out) == UNDEFMOD) /* declaration of undefined global ? */
            cr_parser_semerror(lx, cr_string_pushfstring(lx->ts, 
                                   "declaring already present global variable "
                                   "'%s'", name));
        else /* redeclaration/redefinition of defined global variable */
            checkvarcollision(lx->fs, name, "global", NULL);
    }
    newglobal(lx, name, mod);
}


/* get global variable 'name' or create undefined global */
static void globalvar(FunctionState *fs, OString *name, ExpInfo *e)
{
    Lexer *lx = fs->lx;
    TValue k, dummy;
    setv2s(lx->ts, &k, name);
    setemptyval(&dummy); /* set as undefined */
    if (!cr_htable_get(GS(lx->ts)->globals, &k, &dummy))
        newglobal(lx, name, UNDEFMOD);
    e->u.str = name;
    e->et = EXP_GLOBAL;
}


/* create new static variable */
static int newstatic(FunctionState *fs, OString *name)
{
    Function *fn = fs->fn;
    cr_State *ts = fs->lx->ts;
    checklimit(fs, fn->nsvars + 1, MAXLONGARGSIZE, "statics");
    cr_mem_growvec(ts, fn->svars, fn->sizesvars, fn->nsvars, MAXLONGARGSIZE,
                   "statics");
    SVar *svar = &fn->svars[fn->nsvars++];
    svar->s.name = name;
    svar->s.idx = -1;
    return fn->nsvars - 1;
}


/* find variable 'name' */
static void varname(Lexer *lx, OString *name, ExpInfo *e)
{
    searchvar(lx->fs, name, e, 1);
    if (e->et == EXP_VOID)
        globalvar(lx->fs, name, e);
}


#define varlit(lx,l,e)      varname(lx, cr_lex_newstring(lx, "" l, SLL(l)), e)


/* find static variable 'name' */
static void staticvar(Lexer *lx, OString *name, ExpInfo *e)
{
    FunctionState *fs = lx->fs;
    while (fs) {
        int idx = searchstatic(fs, name, e);
        if (idx < 0) {
            fs = fs->prev;
        } else {
            initexp(e, EXP_STATIC, idx);
            break;
        }
    }
    if (cr_unlikely(fs == NULL))
        cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
                               "undeclared static variable '%s'", name));
}


static void expid(Lexer *lx, ExpInfo *e)
{
    initstring(e, expect_id(lx));
}


/*
 * self ::= 'self'
 */
static void selfkw(Lexer *lx, ExpInfo *e)
{
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
static int exprlist(Lexer *lx, ExpInfo *e)
{
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
static void indexed(Lexer *lx, ExpInfo *var, int super)
{
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
static void getfield(Lexer *lx, ExpInfo *var, int super)
{
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
static void superkw(Lexer *lx, ExpInfo *e)
{
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
static void primaryexp(Lexer *lx, ExpInfo *e)
{
    switch (lx->t.tk) {
    case '(':
        cr_lex_scan(lx); /* skip ')' */
        expr(lx, e);
        expect(lx, ')');
        cr_code_varexp2stack(lx->fs, e);
        break;
    case '@':
        cr_lex_scan(lx); /* skip '@' */
        staticvar(lx, e->u.str, e);
        cr_lex_scan(lx);
        break;
    case TK_IDENTIFIER:
        varname(lx, e->u.str, e);
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
static void call(Lexer *lx, ExpInfo *e)
{
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
static void suffixedexpr(Lexer *lx, ExpInfo *e)
{
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
static void simpleexpr(Lexer *lx, ExpInfo *e)
{
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
static Unopr getunopr(int token)
{
    switch (token) {
    case '-': return OPR_UMIN;
    case '~': return OPR_BNOT;
    case '!': return OPR_NOT;
    default: return OPR_NOUNOPR;
    }
}


/* get binary operation matching 'token' */
static Binopr getbinopr(int token)
{
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
static Binopr subexpr(Lexer *lx, ExpInfo *e, int limit)
{
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
static void expr(Lexer *lx, ExpInfo *e)
{
    subexpr(lx, e, 0);
}



/* -------------------------------------------------------------------------
 * Statements & Declarations
 * ------------------------------------------------------------------------- */

/* check if 'var' is const (read-only) */
static void checkconst(Lexer *lx, ExpInfo *var)
{
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
typedef struct LHS {
    struct LHS *prev;
    ExpInfo e;
} LHS;


/* adjust left and right side of assignment */
static void adjustassign(Lexer *lx, int left, int right, ExpInfo *e)
{
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
static void assign(Lexer *lx, LHS *lhs, int nvars)
{
    expect_cond(lx, eisvar(&lhs->e), "expect variable");
    checkconst(lx, &lhs->e);
    if (match(lx, ',')) { /* more vars ? */
        LHS var;
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
    cr_code_storevar(lx->fs, &lhs->e);
}


/*
 * exprstm ::= functioncall
 *           | varlist '=' explist
 */
static void exprstm(Lexer *lx)
{
    LHS var;
    suffixedexpr(lx, &var.e);
    if (check(lx, '=') || check(lx, ',')) {
        var.prev = NULL;
        assign(lx, &var, 1);
    } else {
        expect_cond(lx, var.e.et == EXP_CALL, "syntax error");
        setlarg0(getinstruction(lx->fs, &var.e), 1);
    }
}


/* get type modifier */
static int getmod(Lexer *lx)
{
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
static int getmodifiers(Lexer *lx, const char *type, const char *name)
{
    int bmask = 0;
    if (match(lx, '<')) {
        do {
            int bit = getmod(lx);
            if (testbit(bmask, bit)) { /* duplicate modifier ? */
                cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
                                       "%s '%s' has duplicate '%s' modifier",
                                       type, name, lx->t.lit.str));
            } else if (testbit(bmask, VARSTATIC) && bit == VARSTATIC) {
                cr_parser_semerror(lx, cr_string_pushfstring(lx->ts,
                                       "%s '%s' can't have both static and "
                                       "close modifiers"));
            }
            bmask = bit2mask(bmask, bit);
        } while (match(lx, ','));
        expect(lx, '>');
    }
    return bmask;
}


/* declare variable 'name' */
static int declarevar(FunctionState *fs, OString *name, ExpInfo *e, int mods)
{
    int vidx = -1;
    if (mods & VARSTATIC) { /* static ? */
        cr_assert(!testbit(mods, VARTBC)); /* can't have close */
        checkvarcollision(fs, name, "static", searchstatic);
        vidx = newstatic(fs, name);
        vmod(&getstatic(fs, vidx)->val) = cast_ubyte(mods);
        initexp(e, EXP_STATIC, vidx);
    } else if (fs->scope->prev) { /* local ? */
        checkvarcollision(fs, name, "local", searchlocal);
        vidx = newlocal(fs->lx, name);
        vmod(&getlocal(fs, vidx)->val) = cast_ubyte(mods);
        initexp(e, EXP_LOCAL, vidx);
    } else { /* global */
        cr_assert(vidx == -1);
        defineglobal(fs->lx, name, mods);
        initglobal(e, name);
    }
    return vidx;
}


/* mark local variable at 'idx' as tbc */
static void closelocal(FunctionState *fs, int idx)
{
    cr_assert(idx >= 0);
    scopemarktbc(fs);
    cr_code_L(fs, OP_TBC, idx);
}


/* try to mark variable declared as tbc */
static void closedecl(FunctionState *fs, int nvars, int *tbc, int vidx,
                         int mods)
{
    if (mods & VARTBC) { /* have tbc var? */
        cr_assert(!testbit(mods, VARSTATIC)); /* can't be static */
        if (cr_unlikely(*tbc == -1)) /* already have tbc variable ? */
            cr_parser_semerror(fs->lx,
                    "multiple to-be-closed variables in a single declaration");
        if (cr_unlikely(vidx == -1)) /* global to-be-closed ? */
            cr_parser_semerror(fs->lx, "global variables can't be closed");
        *tbc = fs->activelocals + nvars;
        cr_assert(tbc >= 0);
        closelocal(fs, *tbc);
    }
}


/* auxiliary for assignment of multiple variable declarations */
static void assigndecl(Lexer *lx, LHS *lhs, int *tbc, int nvars)
{
    FunctionState *fs = lx->fs;
    if (match(lx, ',')) { /* more ids ? */
        LHS var;
        var.prev = lhs;
        OString *name = expect_id(lx);
        int mods = getmodifiers(lx, "variable", getstrbytes(name));
        int vidx = declarevar(fs, name, &var.e, mods);
        closedecl(fs, nvars, tbc, vidx, mods);
        entercstack(lx);
        assigndecl(lx, &var, tbc, nvars + 1);
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
    if (lhs->e.et == EXP_GLOBAL) /* declaration defines global var */
        cr_code_defineglobal(fs, &lhs->e);
    else
        cr_code_storevar(fs, &lhs->e);
}


/* 
 * letstm ::= 'let' idlist ';'
 *          | 'let' idlist '=' exprlist ';'
 */
static void letstm(Lexer *lx)
{
    FunctionState *fs = lx->fs;
    int tbc = -1; /* -1 indicates there are no tbc variables */
    LHS var;
    OString *name = expect_id(lx);
    int mods = getmodifiers(lx, "variable", getstrbytes(name));
    int vidx = declarevar(fs, name, &var.e, mods);
    closedecl(fs, 0, &tbc, vidx, mods);
    if (check(lx, ',') || check(lx, '=')) { /* id list ? */
        var.prev = NULL;
        assigndecl(lx, &var, &tbc, 1);
    } else { /* otherwise assign implicit nil */
        cr_code_nil(lx->fs, 1);
        cr_code_reserveslots(fs, 1);
        cr_code_storevar(fs, &var.e);
    }
    expect(lx, ';');
}


/*
 * block ::= stm
 *         | stm block 
 */
static void block(Lexer *lx)
{
    while (!check(lx, '}') && !check(lx, TK_EOS))
        stm(lx);
    expect(lx, '}');
}


// arglist ::= name
//           | '...'
//           | name ',' arglist
// static void fnargs(Lexer *lx)
// {
//     do {
//         if (match(fs, TOK_DOT_DOT_DOT)) {
//             fs->fn->p.isvararg = 1;
//             break;
//         }
//         fs->fn->p.arity++;
//         name(fs, expectstr("Expect parameter name."));
//         initlocal(fs, 0);
//     } while (match(fs, TOK_COMMA));
// }


/* blockstm ::= '{' block '}' */
static void blockstm(Lexer *lx)
{
    Scope s;
    enterscope(lx->fs, &s, 0);
    block(lx);
    leavescope(lx->fs);
}


// Create and parse a new FunctionState
// static void fn(FunctionState *fs, FunctionType type)
// {
//     FunctionState Fnew;
//     Scope globscope, S;
//     startfs(fs->ts, &Fnew, &globscope, fs->cclass, type, fs);
//     Fnew.fn->p.source = fs->fn->p.source; // source is same
//     startscope(&Fnew, &S, 0, 0); // no need to end this scope
//     expect(&Fnew, TOK_LPAREN, expectstr("Expect '(' after function name."));
//     if (!check(&Fnew, TOK_RPAREN))
//         fnargs(&Fnew);
//     if (Fnew.fn->p.isvararg)
//         expect(&Fnew, TOK_RPAREN, expectstr("Expect ')' after '...'."));
//     else
//         expect(&Fnew, TOK_RPAREN, expectstr("Expect ')' after parameters."));
//     int arity = Fnew.fn->p.arity;
//     int expected = ominfo[fs->tag].arity;
//     if (cr_unlikely(isom(fs) && expected != arity)) {
//         error(fs, comperrors[CE_OMSIG], fs->ts->faststatic[fs->tag]->storage, expected, arity);
//         Fnew.lexer->panic = 0; // clear panic flag do not sync yet
//     }
//     expect(&Fnew, TOK_LBRACE, expectstr("Expect '{' before function body."));
//     block(&Fnew); // body
//     Function *fn = parse_end(&Fnew);
//     CODEOP(fs, OP_CLOSURE, make_constant(fs, OBJ_VAL(fn)));
//     for (int i = 0; i < fn->p.upvalc; i++) {
//         UpValue *upval = Array_Upvalue_index(Fnew.upvalues, i);
//         CODE(fs, upval->local ? 1 : 0);
//         CODEL(fs, upval->idx);
//     }
//     endfs(&Fnew);
// }


/* fnstm ::= 'fn' id '(' fnargs ')' blockstm */
static void fnstm(Lexer *lx)
{
    OString *id = expect_id(lx);
    int idx = name(fs, expectstr("Expect function name."));
    if (fs->S->depth > 0)
        initlocal(fs, 0); // initialize to allow recursion
    fn(fs, FN_FUNCTION);
    ExpInfo _; // dummy
    if (fs->S->depth == 0)
        INIT_GLOBAL(fs, idx, 0, &_);
}


static void method(FunctionState *fs)
{
    TValue identifier;
    int idx;

    expect(fs, TOK_FN, expectstr("Expect 'fn'."));
    expect(fs, TOK_IDENTIFIER, expectstr("Expect method name."));
    identifier = tokintostr(fs->ts, &PREVT(fs));
    idx = make_constant(fs, identifier);
    fs->tag = id2omtag(fs->ts, asstring(identifier));
    fn(fs, FN_METHOD);
    if (fs->tag != -1)
        CODEOP(fs, OP_OVERLOAD, fs->tag - SS_INIT);
    else
        CODEOP(fs, OP_METHOD, idx);
}

static void classstm(Lexer *lx)
{
    expect(fs, TOK_IDENTIFIER, expectstr("Expect class name."));
    Token class_name = PREVT(fs);
    TValue identifier = tokintostr(fs->ts, &class_name);
    int idx = make_constant(fs, identifier);
    ExpInfo _; // dummy
    CODEOP(fs, OP_CLASS, idx);
    if (fs->S->depth > 0) {
        make_local(fs, &class_name);
        initlocal(fs, 0);
    } else {
        INIT_GLOBAL(fs, MAKE_GLOBAL(fs, &class_name), 0, &_);
    }
    Class cclass;
    cclass.enclosing = fs->cclass;
    cclass.superclass = 0;
    fs->cclass = &cclass;
    _.ins.set = 0;
    Scope S;
    if (match(fs, TOK_IMPL)) { // have superclass ?
        expect(fs, TOK_IDENTIFIER, expectstr("Expect superclass name."));
        codevarprev(fs, &_); // get superclass
        if (nameeq(&PREVT(fs), &class_name))
            error(fs, comperrors[CE_INHERIT], ascstring(identifier));
        startscope(fs, &S, 0, 0);
        local_new(fs, syntoken("super"));
        initlocal(fs, 0);
        var(fs, class_name, &_);
        CODE(fs, OP_INHERIT);
        cclass.superclass = 1;
    }
    var(fs, class_name, &_);
    expect(fs, TOK_LBRACE, expectstr("Expect '{' before class body."));
    while (!check(fs, TOK_RBRACE) && !check(fs, TOK_EOF))
        method(fs);
    expect(fs, TOK_RBRACE, expectstr("Expect '}' after class body."));
    CODE(fs, OP_POP); // Pop the class
    if (cclass.superclass)
        leavescope(fs);
    fs->cclass = cclass.enclosing;
}

static void codecall(FunctionState *fs, ExpInfo *E)
{
    OpCode callop = call(fs, E);
    E->et = EXP_CALL;
    if (callop == OP_CALL0 || callop == OP_CALL1)
        E->ins.code = CODE(fs, callop);
    else
        E->ins.code = CODEOP(fs, callop, 1);
}


static void codeinvoke(FunctionState *fs, ExpInfo *E, int idx)
{
    OpCode callop = call(fs, E);
    E->et = EXP_INVOKE;
    OpCode invokeop;
    switch (callop) {
        case OP_CALL0:;
                      invokeop = OP_INVOKE0;
                      break;
        case OP_CALL1:
                      invokeop = OP_INVOKE1;
                      break;
        case OP_CALL:
                      invokeop = OP_INVOKE;
                      break;
        default:
                      cr_unreachable;
    }
    E->ins.code = CODEOP(fs, invokeop, idx);
    CODEL(fs, 1); // retcnt
}


// 'switch' statement state.
typedef struct {
    int patch; // Jump to patch if case expression does not match
    enum {
        CS_MATCH, // Case is constant expression match
        CS_DFLT, // Case is 'default'
        CS_NONE, // Did not parse any cases yet
        CS_CASE, // Case is 'case'
    } casestate;
    Array_Value constants; // all case constant expressions
    cr_ubyte dflt : 1; // if switch has 'default' case
    cr_ubyte havenil : 1; // if switch has 'nil' case
    cr_ubyte havetrue : 1; // if switch has '1' case
    cr_ubyte havefalse : 1; // if switch has '0' case
} SwitchState;


static cr_inline void SwitchState_init(FunctionState *fs, SwitchState *state)
{
    state->patch = -1;
    state->casestate = CS_NONE;
    state->dflt = 0;
    state->havenil = 0;
    state->havetrue = 0;
    state->havefalse = 0;
    Array_Value_init(&state->constants, fs->ts);
}


#define SwitchState_free(state) Array_Value_free(&(state)->constants, NULL);


/*
 * Updates 'switch' constants and checks if the constant
 * already exists, but only if 'e' is constant expression.
 * TODO: Refactor a bit, logic can be compressed
 */
static cr_inline void switchconstants(FunctionState *fs, SwitchState *state, ExpInfo *E)
{
    if (!etisconst(E->type))
        return;
    switch (E->et) {
        case EXP_FALSE:
            if (cr_unlikely(state->havefalse))
                error(fs, comperrors[CE_SWDUP], "0");
            state->havefalse = 1;
            break;
        case EXP_TRUE:
            if (cr_unlikely(state->havetrue))
                error(fs, comperrors[CE_SWDUP], "1");
            state->havetrue = 1;
            break;
        case EXP_NIL:
            if (cr_unlikely(state->havenil))
                error(fs, comperrors[CE_SWDUP], "nil");
            state->havenil = 1;
            break;
        case EXP_STRING:
        case EXP_NUMBER:;
                        TValue caseval = *CONSTANT(fs, E);
                        cr_ubyte _; // dummy
                        for (int i = 0; i < cast_int(state->constants.len); i++) {
                            if (cr_unlikely(raweq(state->constants.data[i], caseval))) {
                                const char *casename = NULL;
                                if (E->et == EXP_STRING)
                                    casename = vtostr(fs->ts, caseval, 1)->storage;
                                else
                                    casename = dtostr(AS_NUMBER(caseval), &_);
                                error(fs, comperrors[CE_SWDUP], casename);
                                return;
                            }
                        }
                        Array_Value_push(&state->constants, caseval);
                        break;
        default:
                        cr_unreachable;
    }
}

static void switchstm(FunctionState *fs)
{
    Context C;
    Scope S;
    SwitchState swstate;
    cr_ubyte inswitch = fs->S->isswitch;
    ExpInfo E1;
    Array_cr_int fts;
    int sdepth;
    savecontext(fs, &C);
    SwitchState_init(fs, &swstate);
    Array_cr_int_init(&fts, fs->ts);
    startscope(fs, &S, 0, 1); // implicit scope
    startbreaklist(fs);
    expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'switch'."));
    E1.ins.set = 0;
    expr(fs, &E1);
    expect(fs, TOK_RPAREN, expectstr("Expect ')' after condition."));
    expect(fs, TOK_LBRACE, expectstr("Expect '{' after ')'."));
    sdepth = fs->cflow.innersdepth;
    fs->cflow.innersdepth = fs->S->depth;
    // Switch must contain case or default before any statements
    if (!check(fs, TOK_RBRACE) && !check(fs, TOK_EOF) && !check(fs, TOK_CASE) && !check(fs, TOK_DEFAULT))
        error(fs, comperrors[CE_SWNOC]);
    // 'switch' body
    while (!match(fs, TOK_RBRACE) && !check(fs, TOK_EOF)) {
        if (match(fs, TOK_CASE) || match(fs, TOK_DEFAULT)) {
            fs->fn->gotret = 0;
            if (swstate.casestate != CS_NONE) {
                Array_cr_int_push(&fts, CODEJMP(fs, OP_JMP));
                if (swstate.casestate != CS_DFLT && swstate.casestate != CS_MATCH)
                    patchjmp(fs, swstate.patch);
            }
            swstate.casestate = CS_DFLT;
            if (PREVT(fs).type == TOK_CASE) {
                ExpInfo E2;
                E2.ins.set = 0;
                expr(fs, &E2);
                expect(fs, TOK_COLON, expectstr("Expect ':' after 'case'."));
                switchconstants(fs, &swstate, &E2);
                if (eareconstandeq(fs, &E1, &E2)) {
                    restorecontext(fs, &C);
                    swstate.casestate = CS_MATCH;
                } else {
                    CODE(fs, OP_EQ);
                    swstate.casestate = CS_CASE;
                    swstate.patch = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
                }
            } else if (!swstate.dflt) {
                swstate.dflt = 1;
                swstate.casestate = CS_DFLT;
                expect(fs, TOK_COLON, expectstr("Expect ':' after 'default'."));
            } else
                error(fs, comperrors[CE_SWDEF]);
            if (fts.len > 0)
                patchjmp(fs, Array_cr_int_pop(&fts));
        } else {
            stm(fs);
            if (swstate.casestate == CS_MATCH && fs->fn->gotret) {
                // @TODO: Implement optimizations.
                // Also check if last 'stm' was 'breakstm' (same effect)
            }
        }
    }
    if (PREVT(fs).type == TOK_EOF)
        error(fs, comperrors[CE_SWRBR]);
    Array_cr_int_free(&fts, NULL);
    SwitchState_free(&swstate);
    leavescope(fs);
    CODE(fs, OP_POP); // pop switch expression
    patchbreaklist(fs);
    endbreaklist(fs);
    fs->cflow.innersdepth = sdepth;
    fs->S->isswitch = inswitch;
}

static void ifstm(FunctionState *fs)
{
    ExpInfo E;
    Context C;
    int jmptoelse, jmptoend = -1;
    savecontext(fs, &C);
    expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'if'."));
    E.ins.set = 0;
    expr(fs, &E); // condition
    expect(fs, TOK_RPAREN, expectstr("Expect ')' after condition."));
    cr_ubyte remove = 0;
    cr_ubyte istrue = 0;
    if (etisconst(E.type)) {
        rmlastins(fs, &E);
        if (etisfalse(E.et))
            remove = 1;
        else
            istrue = 1;
    } else
        jmptoelse = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
    stm(fs);
    if (!remove) {
        jmptoend = CODEJMP(fs, OP_JMP);
        if (!istrue)
            patchjmp(fs, jmptoelse);
        else if (fs->fn->gotret) { // condition is 1 and 'stm' was a 'returnstm'
                                   // @TODO: Implement optimization.
        }
    } else
        restorecontext(fs, &C);
    fs->fn->gotret = 0;
    if (match(fs, TOK_ELSE)) { // there is 'else' branch?
        stm(fs);
        if (!remove)
            patchjmp(fs, jmptoend);
    }
}

static cr_inline void startloop(FunctionState *fs, int *lstart, int *ldepth)
{
    *lstart = (fs)->cflow.innerlstart;
    *ldepth = (fs)->cflow.innerldepth;
    (fs)->cflow.innerlstart = codeoffset(fs);
    (fs)->cflow.innerldepth = fs->S->depth;
}

static cr_inline void endloop(FunctionState *fs, int lstart, int ldepth)
{
    (fs)->cflow.innerlstart = lstart;
    (fs)->cflow.innerldepth = ldepth;
}

static void whilestm(Lexer *lx)
{
    Scope S;
    Context C;
    ExpInfo E;
    int lstart, ldepth;
    int jmptoend = -1;
    cr_ubyte infinite = 0;
    cr_ubyte remove = 0;
    savecontext(fs, &C);
    startscope(fs, &S, 1, 0);
    startloop(fs, &lstart, &ldepth);
    startbreaklist(fs);
    expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'while'."));
    E.ins.set = 0;
    expr(fs, &E); // conditional
    if (etisconst(E.type)) {
        rmlastins(fs, &E);
        if (etisfalse(E.et))
            remove = 1;
        else
            infinite = 1;
    } else
        jmptoend = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
    expect(fs, TOK_RPAREN, expectstr("Expect ')' after condition."));
    stm(fs); // body
    cr_ubyte gotret = fs->fn->gotret;
    leavescope(fs);
    if (!remove) {
        codeloop(fs, (fs)->cflow.innerlstart);
        if (!infinite) {
            cr_assert(fs->ts, jmptoend != -1, "end jmp invalid but flag is 0.");
            patchjmp(fs, jmptoend);
        } else if (gotret) { // cond 1 and 'stm' was 'returnstm'
                             // @TODO: Implement optimizations
        }
        patchbreaklist(fs);
    } else
        restorecontext(fs, &C);
    endloop(fs, lstart, ldepth);
    endbreaklist(fs);
}

static void forstm(Lexer *lx)
{
    Scope S;
    Context C;
    ExpInfo E;
    int lstart, ldepth;
    int jmptoend = -1;
    cr_ubyte remove = 0;
    cr_ubyte infinite = 0;
    startscope(fs, &S, 1, 0);
    startbreaklist(fs);
    expect(fs, TOK_LPAREN, expectstr("Expect '(' after 'for'."));
    if (match(fs, TOK_SEMICOLON)) // Initializer for-clause
        ; // no initializer
    else if (match(fs, TOK_VAR))
        vardec(fs);
    else if (match(fs, TOK_FIXED))
        constvardec(fs);
    else
        exprstm(fs, 0);
    savecontext(fs, &C);
    startloop(fs, &lstart, &ldepth);
    if (!match(fs, TOK_SEMICOLON)) { // conditional
        E.ins.set = 0;
        expr(fs, &E);
        if (etisconst(E.type)) {
            rmlastins(fs, &E);
            if (etistrue(E.type))
                infinite = 1;
            else
                remove = 1;
        } else
            jmptoend = CODEJMP(fs, OP_JMP_IF_FALSE_POP);
        expect(fs, TOK_SEMICOLON, expectstr("Expect ';' after for-loop condition clause."));
    } else
        infinite = 1;
    if (!match(fs, TOK_RPAREN)) { // last for-clause
        int jmptobody = -1;
        int jmptoincr = -1;
        if (!infinite && !remove)
            jmptobody = CODEJMP(fs, OP_JMP);
        if (!remove)
            jmptoincr = codeoffset(fs);
        exprstm(fs, 1);
        if (!infinite && !remove) {
            codeloop(fs, (fs)->cflow.innerlstart);
            patchjmp(fs, jmptobody);
            (fs)->cflow.innerlstart = jmptoincr;
        }
        expect(fs, TOK_RPAREN, expectstr("Expect ')' after last for-loop clause."));
    }
    stm(fs); // Loop body
    if (!remove) {
        codeloop(fs, (fs)->cflow.innerlstart);
        if (!infinite)
            patchjmp(fs, jmptoend);
        else if (fs->fn->gotret) { // 'stm' was 'returnstm' and conditional is 1
                                   // @TODO: Implement optimizations
        }
        patchbreaklist(fs);
    } else
        restorecontext(fs, &C);
    leavescope(fs);
    endloop(fs, lstart, ldepth);
    endbreaklist(fs);
}

static int foreachvars(FunctionState *fs)
{
    int vars = 0;
    do {
        if (vars >= CR_BYTECODE_MAX)
            error(fs, comperrors[CE_VARLIST], CR_BYTECODE_MAX);
        vars++;
        expect(fs, TOK_IDENTIFIER, expectstr("Expect varname."));
        make_local(fs, &PREVT(fs));
        initlocal(fs, 0);
    } while (match(fs, TOK_COMMA));
    return vars;
}

static void newlocalliteral(FunctionState *fs, const char *name)
{
    Token syntok = syntoken(name);
    local_new(fs, syntok);
    initlocal(fs, 0);
}

static void foreachstm(FunctionState *fs)
{
    Scope S;
    int lstart, ldepth, vars, expc, endjmp;
    ExpInfo E;
    startscope(fs, &S, 1, 0);
    S.isgloop = 1; // set as generic loop
    startbreaklist(fs);
    newlocalliteral(fs, "(for iterator)"); // iterator function
    newlocalliteral(fs, "(for invstate)"); // invariant state
    newlocalliteral(fs, "(for cntlvar)"); // control variable
    vars = foreachvars(fs); // declared vars
    expect(fs, TOK_IN, expectstr("Expect 'in'."));
    E.ins.set = 0;
    expr(fs, &E); // iterator factory
    expect_cond(fs, !etisconst(E.type), "Can't call constant expression.");
    expc = 1;
    if (match(fs, TOK_COMMA))
        expc += exprlist(fs, 2, &E);
    adjustassign(fs, &E, 3, expc);
    startloop(fs, &lstart, &ldepth);
    CODEOP(fs, OP_FOREACH_PREP, vars);
    CODEOP(fs, OP_FOREACH, vars);
    endjmp = CODEJMP(fs, OP_JMP);
    stm(fs);
    CODEPOP(fs, vars);
    codeloop(fs, (fs)->cflow.innerlstart);
    patchjmp(fs, endjmp);
    leavescope(fs);
    patchbreaklist(fs);
    endbreaklist(fs);
    endloop(fs, lstart, ldepth);
}

static void loopstm(FunctionState *fs)
{
    Scope S;
    int lstart, ldepth;
    startscope(fs, &S, 1, 0);
    startbreaklist(fs);
    startloop(fs, &lstart, &ldepth);
    stm(fs);
    if (fs->fn->gotret) {
        // @TODO: Implement optimizations
    }
    codeloop(fs, (fs)->cflow.innerlstart);
    leavescope(fs);
    patchbreaklist(fs);
    endbreaklist(fs);
    endloop(fs, lstart, ldepth);
}

static cr_inline Scope *loopscope(FunctionState *fs)
{
    Scope *S = fs->S;
    while (S != NULL) {
        if (S->isloop)
            return S;
        S = S->prev;
    }
    return NULL;
}

// Return count of switch statements until the first loop 'Scope'
// or the top-level code counting from the current 'Scope' in the 'FunctionState'.
static cr_inline int switchcnt(FunctionState *fs)
{
    Scope *S = fs->S;
    int count = 0;
    while (S != NULL && S->depth > fs->cflow.innerldepth) {
        if (S->isswitch)
            count++;
        S = S->prev;
    }
    return count;
}

static void continuestm(FunctionState *fs)
{
    expect(fs, TOK_SEMICOLON, expectstr("Expect ';' after 'continue'."));
    if (cr_unlikely(fs->cflow.innerlstart == -1))
        error(fs, comperrors[CE_CONT]);
    else {
        Scope *S = loopscope(fs);
        cr_assert(fs->ts, S != NULL, "Loop scope not found but cflow offset is set.");
        int popn = fs->locals.len - (S->isgloop * 3) - S->nlocals + switchcnt(fs);
        CODEPOP(fs, popn);
        codeloop(fs, fs->cflow.innerlstart);
    }
}

static void breakstm(FunctionState *fs)
{
    expect(fs, TOK_SEMICOLON, expectstr("Expect ';' after 'break'."));
    Array_Array_cr_int *arr = &fs->cflow.breaks;
    int popn = 0;
    Scope *scope = NULL;
    switch (inwhat(fs->S, &scope)) {
        case 0: // in switch
            popn++; // switch expression
        case 1: // FALLTHRU (in loop)
            break;
        case -1: // outside
            error(fs, comperrors[CE_BREAK]);
            return;
        default:
            cr_unreachable;
    }
    popn += fs->locals.len - scope->nlocals;
    CODEPOP(fs, popn);
    Array_cr_int *last = Array_Array_cr_int_last(arr);
    Array_cr_int_push(last, CODEJMP(fs, OP_JMP));
}

/// return ::= 'return' ';'
///          | 'return' explist ';'
static void returnstm(FunctionState *fs)
{
}


/// dot ::= '.' name
///       | '.' name call
static void dot(FunctionState *fs, ExpInfo *E)
{
    expect(fs, TOK_IDENTIFIER, expectstr("Expect property name after '.'."));
    TValue identifier = tokintostr(fs->ts, &PREVT(fs));
    int idx = make_constant(fs, identifier);
    if (match(fs, TOK_LPAREN))
        codeinvoke(fs, E, idx);
    else {
        E->et = EXP_INDEXED;
        E->value = idx;
        if (!E->ins.set)
            E->ins.code = CODEOP(fs, OP_GET_PROPERTY, idx);
    }
}


/* 
 * stm ::= letstm
 *       | fnstm
 *       | classstm
 *       | whilestm
 *       | forstm
 *       | foreachstm
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
static void stm(Lexer *lx)
{
    switch (lx->t.tk) {
        case TK_LET:
            cr_lex_scan(lx); /* skip 'let' */
            letstm(lx);
            break;
        case TK_FN:
            cr_lex_scan(lx); /* skip 'fn' */
            fnstm(lx);
            break;
        case TK_CLASS:
            cr_lex_scan(lx); /* skip 'class' */
            classstm(lx);
            break;
        case TK_WHILE:
            cr_lex_scan(lx); /* skip 'while' */
            whilestm(lx);
            break;
        case TK_FOR:
            cr_lex_scan(lx); /* skip 'for' */
            forstm(lx);
            break;
        case TK_FOREACH:
            cr_lex_scan(lx); /* skip 'foreach' */
            foreachstm(lx);
            break;
        case TK_IF:
            cr_lex_scan(lx); /* skip 'if' */
            ifstm(lx);
            break;
        case TK_SWITCH:
            cr_lex_scan(lx); /* skip 'switch' */
            switchstm(lx);
            break;
        case '{':
            cr_lex_scan(lx); /* skip '{' */
            blockstm(lx);
            break;
        case TK_CONTINUE:
            cr_lex_scan(lx); /* skip 'continue' */
            continuestm(lx);
            break;
        case TK_BREAK:
            cr_lex_scan(lx); /* skip 'break' */
            breakstm(lx);
            break;
        case TK_RETURN:
            cr_lex_scan(lx); /* skip 'return' */
            returnstm(lx);
            break;
        case TK_LOOP:
            cr_lex_scan(lx); /* skip 'loop' */
            loopstm(lx);
            break;
        case ';':
            cr_lex_scan(lx); /* skip ';' */
            break;
        default:
            exprstm(lx);
            expect(lx, ';');
            break;
    }
}


/* parse current input until end of stream */
static void parseuntilEOS(Lexer *lx) {
    while (!lx->t.tk != TK_EOS)
        stm(lx);
}



/* compile main function */
static void criptmain(FunctionState *fs, Lexer *lx)
{
    Scope gscope;
    startfs(fs, lx, &gscope);
    setvararg(fs, 0); /* main is always vararg */
    cr_lex_scan(lx); /* scan first token */
    parseuntilEOS(lx);
    cr_assert(lx->t.tk == TK_EOS);
    endfs(fs);
}


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
    criptmain(&fs, &lx); /* Cript main function */
    ts->stacktop.p--; /* pop scanner htable */
    return cl;
}



/* -------------------------------------------------------------------------
 * Protected parsing
 * ------------------------------------------------------------------------- */

/* data for 'pparse' */
typedef struct PPData {
    BuffReader *br;
    Buffer buff;
    ParserState ps;
    const char *source;
} PPData;


/* protected 'parse' */
static void pparse(cr_State *ts, void *userdata)
{
    PPData *pcompile = cast(PPData *, userdata);
    CrClosure *cl = parse(ts, pcompile->br, &pcompile->buff, &pcompile->ps,
            pcompile->source);
    cr_assert(cl->nupvals <= cl->sizeupvals);
    cr_function_initupvals(ts, cl);
}


/* external interface for 'pparse' */
void cr_parser_pparse(cr_State *ts, BuffReader *br, const char *source)
{
    PPData ppdata;
    ppdata.br = br;
    cr_reader_buffinit(&ppdata.buff);
    memset(&ppdata.ps, 0, sizeof(ppdata.ps));
    ppdata.source = source;
    cr_vm_pcall(ts, pparse, &ppdata, savestack(ts, ts->stacktop.p));
}
