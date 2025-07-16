/*
** cparser.c
** CScript Parser
** See Copyright Notice in cscript.h
*/

#define cparser_c
#define CS_CORE

#include "cscriptprefix.h"

#include <string.h>

#include "ccode.h"
#include "cfunction.h"
#include "cgc.h"
#include "clexer.h"
#include "cscriptlimits.h"
#include "cmem.h"
#include "cobject.h"
#include "cobject.h"
#include "cparser.h"
#include "cscriptconf.h"
#include "cstate.h"
#include "cstring.h"
#include "ctable.h"
#include "cvm.h"


/*
** If enabled, it disassembles pre-compiled CScript chunk.
** (Used for internal debugging.)
*/
#if defined(CSI_DISASSEMBLE_BYTECODE)
#include "ctrace.h"
#define unasmfunc(C,p)      csTR_disassemble(C, p)
#else
#define unasmfunc(C,p)      /* no-op */
#endif


/* check if 'tok' matches current token */
#define check(lx, tok)      ((lx)->t.tk == (tok))


/* macros for controlling recursion depth */
#define enterCstack(lx)     csT_incCstack((lx)->C)
#define leaveCstack(lx)     ((lx)->C->nCcalls--)


/* macros for 'lastisend' in function state */
#define stmIsReturn(fs)     ((fs)->lastisend == 1)
#define stmIsBreak(fs)      ((fs)->lastisend == 2)
#define stmIsContinue(fs)   ((fs)->lastisend == 3)
#define stmIsEnd(fs)        ((fs)->lastisend)


/* expect 'cond' to be true or invoke error */
#define expect_cond(lx, cond, err) \
    { if (c_unlikely(!(cond))) csY_syntaxerror(lx, err); }


/* control flow masks */
#define CFML            (1 << 0) /* regular loop */
#define CFMGL           (1 << 1) /* generic loop */
#define CFMS            (1 << 2) /* switch */
#define CFMASK          (CFML | CFMGL | CFMS)

#define is_loop(s)              (testbits((s)->cf, CFML) != 0)
#define is_genloop(s)           (testbits((s)->cf, CFMGL) != 0)
#define is_switch(s)            (testbits((s)->cf, CFMS) != 0)
#define haspendingjumps(s)      testbits((s)->cf, CFMASK)


/* lexical scope information */
typedef struct Scope {
    struct Scope *prev; /* implicit linked-list */
    int nactlocals; /* number of locals outside of this scope */
    int depth; /* scope depth (number of nested scopes) */
    int firstgoto; /* index of first pending goto jump in this block */
    c_ubyte cf; /* control flow */
    c_ubyte haveupval; /* set if scope contains upvalue variable */
    c_ubyte havetbcvar; /* set if scope contains to-be-closed variable */
} Scope;


/* class declaration state */
typedef struct ClassState {
    struct ClassState *prev; /* chain of nested declarations */
    c_ubyte super; /* true if class declaration inherits */
} ClassState;


/* loop state */
typedef struct LoopState {
    struct LoopState *prev; /* chain */
    Scope s; /* loop scope */
    int pcloop; /* pc where loop starts */
} LoopState;


/* 
** Snapshot of function state.
** (Used primarily for optimizations, e.g., trimming dead code.)
*/
typedef struct FuncContext {
    int ps_actlocals;
    int ninstpc;
    int prevpc;
    int prevline;
    int sp;
    int nactlocals;
    int np;
    int nk;
    int pc;
    int nabslineinfo;
    int nlocals;
    int nupvals;
    int pcswtest;
    int lasttarget;
    int lastgoto; /* last pending goto in 'gt' */
    c_ubyte iwthabs;
    c_ubyte needclose;
    c_ubyte opbarrier;
    c_ubyte lastisend;
} FuncContext;


static void storecontext(FunctionState *fs, FuncContext *ctx) {
    ctx->ps_actlocals = fs->lx->ps->actlocals.len;
    ctx->ninstpc = fs->ninstpc;
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
    ctx->pcswtest = fs->pcswtest;
    ctx->lasttarget = fs->lasttarget;
    ctx->lastgoto = fs->lx->ps->gt.len;
    ctx->iwthabs = fs->iwthabs;
    ctx->needclose = fs->needclose;
    ctx->opbarrier = fs->opbarrier;
    ctx->lastisend = fs->lastisend;
}


static void loadcontext(FunctionState *fs, FuncContext *ctx) {
    fs->lx->ps->actlocals.len = ctx->ps_actlocals;
    fs->ninstpc = ctx->ninstpc;
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
    fs->pcswtest = ctx->pcswtest;
    fs->lasttarget = ctx->lasttarget;
    fs->lx->ps->gt.len = ctx->lastgoto;
    fs->iwthabs = ctx->iwthabs;
    fs->needclose = ctx->needclose;
    fs->opbarrier = ctx->opbarrier;
    fs->lastisend = ctx->lastisend;
}


static c_noret expecterror(Lexer *lx, int tk) {
    const char *err = csS_pushfstring(lx->C, "expected %s", csY_tok2str(lx, tk));
    csY_syntaxerror(lx, err);
}


static c_noret limiterror(FunctionState *fs, const char *what, int limit) {
    cs_State *C = fs->lx->C;
    int line = fs->p->defline;
    const char *where = (line == 0 ? "main function" :
                        csS_pushfstring(C, "function at line %d", line));
    const char *err = csS_pushfstring(C, "too many %s (limit is %d) in %s",
                                          what, limit, where);
    csY_syntaxerror(fs->lx, err);
}


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
            csY_syntaxerror(lx, csS_pushfstring(lx->C,
                    "expected %s (to close %s at line %d)",
                    csY_tok2str(lx, what), csY_tok2str(lx, who), linenum));
    }
}


static const char *errstmname(Lexer *lx, const char *err) {
    const char *stm;
    switch (lx->fs->lastisend) {
        case 1: stm = "return"; break;
        case 2: stm = "break"; break;
        case 3: stm = "continue"; break;
        default: return err;
    }
    return csS_pushfstring(lx->C,
            "%s ('%s' must be the last statement in this block)", err, stm);
}


static c_noret expecterrorblk(Lexer *lx) {
    const char *err = csS_pushfstring(lx->C,
                        "expected %s", csY_tok2str(lx, '}'));
    err = errstmname(lx, err);
    csY_syntaxerror(lx, err);
}


/*
** Similar to 'expectmatch' but this is invoked only
** when 'blockstm' expects delimiter '}' which is missing.
*/
static void expectmatchblk(Lexer *lx, int linenum) {
    if (c_unlikely(!match(lx, '}'))) {
        if (lx->line == linenum)
            expecterrorblk(lx);
        else {
            const char *err = csS_pushfstring(lx->C,
                    "expected %s to close %s at line %d",
                    csY_tok2str(lx, '}'), csY_tok2str(lx, '{'), linenum);
            csY_syntaxerror(lx, errstmname(lx, err));
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


/* semantic error; variant of syntax error without 'near <token>' */
c_noret csP_semerror(Lexer *lx, const char *err) {
    lx->t.tk = 0;
    csY_syntaxerror(lx, err);
}


void csP_checklimit(FunctionState *fs, int n, int limit, const char *what) {
    if (c_unlikely(n >= limit)) limiterror(fs, what, limit);
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


static void contadjust(FunctionState *fs, int push) {
    int ncntl = is_genloop(&fs->ls->s) * VAR_N;
    int total = (fs->nactlocals - fs->ls->s.nactlocals - ncntl);
    csC_adjuststack(fs, push ? -total : total);
}


/*
** Adds a new 'break' jump into the goto list.
*/
static int newbreakjump(Lexer *lx, int pc, int bk, int close) {
    GotoList *gl = &lx->ps->gt;
    int n = gl->len;
    csM_growarray(lx->C, gl->arr, gl->size, n, CS_MAXINT, "pending jumps", Goto);
    gl->arr[n].pc = pc;
    gl->arr[n].nactlocals = lx->fs->nactlocals;
    gl->arr[n].close = close;
    gl->arr[n].bk = bk;
    gl->len = n + 1;
    return n;
}


/* 
** Get the most recent control flow scope, or NULL if none
** present.
*/
static const Scope *getcfscope(const FunctionState *fs) {
    const Scope *s = NULL;
    if (fs->switchscope)
        s = fs->switchscope;
    if (fs->ls && (!s || s->depth < fs->ls->s.depth))
        s = &fs->ls->s;
    cs_assert(!s || haspendingjumps(s));
    return s;
}


/*
** Add new pending (break or continue in generic loop) jump to the goto list.
** This language construct is coded as POP followed by a JMP.
** If close is needed, sequence of instructions is CLOSE->POP->JMP.
** If the pending jump is continue inside of generic loop then only JMP
** is emitted as the popping and close is managed by 'continuestm'.
*/
static int newpendingjump(Lexer *lx, int bk, int close, int adjust) {
    FunctionState *fs = lx->fs;
    int pc;
    cs_assert(getopSize(OP_JMP) == getopSize(OP_POP));
    cs_assert(getopSize(OP_JMP) == getopSize(OP_CLOSE));
    if (close) { /* needs close? */
        const Scope *cfs = getcfscope(fs);
        cs_assert(cfs != NULL);
        csC_emitIL(fs, OP_CLOSE, stacklevel(fs, cfs->nactlocals));
    }
    if (bk) { /* break statement? */
        cs_assert(adjust >= 0);
        csC_adjuststack(fs, adjust);
    }
    pc = csC_jmp(fs, OP_JMP);
    return newbreakjump(lx, pc, bk, close);
}


/* 
** Remove local variables up to specified level.
*/
static void removelocals(FunctionState *fs, int tolevel) {
    fs->lx->ps->actlocals.len -= fs->nactlocals - tolevel;
    cs_assert(fs->lx->ps->actlocals.len >= 0);
    while (fs->nactlocals > tolevel) /* set debug information */
        getlocalinfo(fs, --fs->nactlocals)->endpc = currPC;
}


/*
** Patch pending goto jumps ('break' or 'continue' in generic loop).
*/
static void patchpendingjumps(FunctionState *fs, Scope *s) {
    Lexer *lx = fs->lx;
    GotoList *gl = &lx->ps->gt;
    int igt = s->firstgoto; /* first goto in the finishing block */
    cs_assert(haspendingjumps(s));
    while (igt < gl->len) {
        Goto *gt = &gl->arr[igt];
        if (gt->bk) /* 'break' ? */
            csC_patchtohere(fs, gt->pc);
        else { /* 'continue' in generic loop */
            cs_assert(fs->ls && fs->ls->pcloop != NOPC);
            cs_assert(s == &fs->ls->s && is_genloop(s));
            cs_assert(!gt->close);
            csC_patch(fs, gt->pc, fs->ls->pcloop);
        }
        igt++; /* get next */
    }
    lx->ps->gt.len = s->firstgoto; /* remove pending goto jumps */
}


/* init expression with generic information */
static void initexp(ExpInfo *e, expt et, int info) {
    e->t = e->f = NOJMP;
    e->et = et;
    e->u.info = info;
}


#define voidexp(e)      initexp(e, EXP_VOID, 0)


/* 'voidexp' but in C99 initializer syntax */
#define INIT_EXP        { .t = NOJMP, .f = NOJMP, .et = EXP_VOID }


static void initvar(FunctionState *fs, ExpInfo *e, int vidx) {
    e->t = e->f = NOJMP;
    e->et = EXP_LOCAL;
    e->u.var.vidx = vidx;
    e->u.var.sidx = getlocalvar(fs, vidx)->s.sidx;
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
    csM_growarray(lx->C, p->locals, p->sizelocals, fs->nlocals, MAXVARS,
                  "locals", LVarInfo);
    while (osz < p->sizelocals)
        p->locals[osz++].name = NULL;
    p->locals[fs->nlocals].name = name;
    p->locals[fs->nlocals].startpc = currPC;
    csG_objbarrier(lx->C, p, name);
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


static void enterscope(FunctionState *fs, Scope *s, int cf) {
    s->cf = cf;
    if (fs->scope) { /* not a global scope? */
        s->depth = fs->scope->depth + 1;
        s->havetbcvar = fs->scope->havetbcvar;
    } else { /* global scope */
        s->depth = 0;
        s->havetbcvar = 0;
    }
    s->nactlocals = fs->nactlocals;
    s->firstgoto = fs->lx->ps->gt.len;
    s->haveupval = 0;
    s->prev = fs->scope;
    fs->scope = s;
}


static void leavescope(FunctionState *fs) {
    Scope *s = fs->scope;
    int stklevel = stacklevel(fs, s->nactlocals);
    int nvalues = fs->nactlocals - s->nactlocals;
    if (s->prev && s->haveupval) /* need a 'close'? */
        csC_emitIL(fs, OP_CLOSE, stklevel);
    removelocals(fs, s->nactlocals); /* remove scope locals */
    cs_assert(s->nactlocals == fs->nactlocals);
    if (s->prev) { /* not main chunk scope? */
        if (fs->lasttarget == currPC)
            fs->opbarrier |= 2; /* prevent POP merge */
        csC_pop(fs, nvalues); /* pop locals */
        fs->opbarrier &= ~2; /* allow POP merge */
    }
    if (haspendingjumps(s)) /* might have pending jumps? */
        patchpendingjumps(fs, s); /* patch them */
    fs->scope = s->prev; /* go back to the previous scope (if any) */
}


/* 
** Mark scope where variable at compiler index 'level' was defined
** in order to emit close instruction before the scope gets closed.
*/
static void scopemarkupval(FunctionState *fs, int level) {
    Scope *s = fs->scope;
    while (s->nactlocals > level)
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
    fs->cs = NULL;
    fs->ls = NULL;
    fs->prev = lx->fs;
    fs->lx = lx;
    lx->fs = fs;
    fs->scope = fs->switchscope = NULL;
    currPC = fs->prevpc = 0;
    fs->prevline = p->defline;
    fs->sp = 0;
    fs->nactlocals = 0;
    fs->firstlocal = lx->ps->actlocals.len;
    fs->np = 0;
    fs->nk = 0;
    fs->nabslineinfo = 0;
    fs->ninstpc = 0;
    fs->nlocals = 0;
    fs->nupvals = 0;
    fs->pcswtest = 0;
    fs->lasttarget = 0;
    fs->iwthabs = fs->needclose = fs->opbarrier = fs->lastisend = 0;
    p->source = lx->src;
    csG_objbarrier(lx->C, p, p->source);
    p->maxstack = 2; /* stack slots 0/1 are always valid */
    enterscope(fs, s, 0); /* start top-level scope */
}


static void close_func(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Proto *p = fs->p;
    cs_State *C = lx->C;
    cs_assert(fs->scope && !fs->scope->prev);
    leavescope(fs); cs_assert(!fs->scope); /* end final scope */
    if (!stmIsReturn(fs)) /* function missing final return? */
        csC_ret(fs, nvarstack(fs), 0); /* add implicit return */
    csC_finish(fs); /* final code adjustments */
    /* shrink unused memory */
    csM_shrinkarray(C, p->p, p->sizep, fs->np, Proto *);
    csM_shrinkarray(C, p->k, p->sizek, fs->nk, TValue);
    csM_shrinkarray(C, p->code, p->sizecode, currPC, Instruction);
    csM_shrinkarray(C, p->lineinfo, p->sizelineinfo, currPC, c_byte);
    csM_shrinkarray(C, p->abslineinfo, p->sizeabslineinfo, fs->nabslineinfo,
                       AbsLineInfo);
    csM_shrinkarray(C, p->instpc, p->sizeinstpc, fs->ninstpc, int);
    csM_shrinkarray(C, p->locals, p->sizelocals, fs->nlocals, LVarInfo);
    csM_shrinkarray(C, p->upvals, p->sizeupvals, fs->nupvals, UpValInfo);
    lx->fs = fs->prev; /* go back to enclosing function (if any) */
    csG_checkGC(C); /* try to collect garbage memory */
    unasmfunc(fs->lx->C, fs->p);
}


/* add function prototype */
static Proto *addproto(Lexer *lx) {
    cs_State *C = lx->C;
    FunctionState *fs = lx->fs;
    Proto *p = fs->p;
    Proto *clp; /* closure prototype */
    if (fs->np >= p->sizep) {
        int osz = p->sizep;
        csM_growarray(C, p->p, p->sizep, fs->np, MAX_ARG_L, "functions",
                      Proto *);
        while (osz < p->sizep)
            p->p[osz++] = NULL;
    }
    p->p[fs->np++] = clp = csF_newproto(C);
    csG_objbarrier(C, p, clp);
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


/* adds local variable to the 'actlocals' */
static int addlocal(Lexer *lx, OString *name) {
    FunctionState *fs = lx->fs;
    ParserState *ps = lx->ps;
    LVar *local;
    csP_checklimit(fs, ps->actlocals.len + 1 - fs->firstlocal, MAXVARS, "locals");
    csM_growarray(lx->C, ps->actlocals.arr, ps->actlocals.size,
                         ps->actlocals.len, CS_MAXINT, "locals", LVar);
    local = &ps->actlocals.arr[ps->actlocals.len++];
    local->s.kind = VARREG;
    local->s.name = name;
    local->s.pidx = -1;
    return ps->actlocals.len - fs->firstlocal - 1;
}


#define addlocallit(lx,lit) \
        addlocal(lx, csY_newstring(lx, "" lit, LL(lit)))


/*
** Searches for local variable 'name'.
*/
static int searchlocal(FunctionState *fs, OString *name, ExpInfo *e, int limit) {
    for (int i = fs->nactlocals - 1; i >= 0 && i > limit; i--) {
        LVar *lvar = getlocalvar(fs, i);
        if (eqstr(name, lvar->s.name)) { /* found? */
            initvar(fs, e, i);
            return EXP_LOCAL;
        }
    }
    return -1; /* not found */
}


/* allocate space for new 'UpValInfo' */
static UpValInfo *newupvalue(FunctionState *fs) {
    Proto *p = fs->p;
    int osz = p->sizeupvals;
    csP_checklimit(fs, fs->nupvals + 1, MAXUPVAL, "upvalues");
    csM_growarray(fs->lx->C, p->upvals, p->sizeupvals, fs->nupvals,
                  MAXUPVAL, "upvalues", UpValInfo);
    while (osz < p->sizeupvals)
        p->upvals[osz++].name = NULL;
    return &p->upvals[fs->nupvals++];
}


/* add new upvalue 'name' into 'upvalues' */
static int addupvalue(FunctionState *fs, OString *name, ExpInfo *v) {
    UpValInfo *uv = newupvalue(fs);
    FunctionState *prev = fs->prev;
    if (v->et == EXP_LOCAL) { /* local? */
        uv->onstack = 1;
        uv->idx = v->u.var.sidx;
        uv->kind = getlocalvar(prev, v->u.var.vidx)->s.kind;
        cs_assert(eqstr(name, getlocalvar(prev, v->u.var.vidx)->s.name));
    } else { /* must be upvalue */
        cs_assert(v->et == EXP_UVAL);
        uv->onstack = 0;
        uv->idx = cast_ubyte(v->u.info);
        uv->kind = prev->p->upvals[v->u.info].kind;
        cs_assert(eqstr(name, prev->p->upvals[v->u.info].name));
    }
    uv->name = name;
    csG_objbarrier(fs->lx->C, fs->p, name);
    return fs->nupvals - 1;
}


/* searches for upvalue 'name' */
static int searchupvalue(FunctionState *fs, OString *name) {
    UpValInfo *up = fs->p->upvals;
    for (int i = 0; i < fs->nupvals; i++)
        if (eqstr(up[i].name, name)) 
            return i;
    return -1; /* not found */
}


/*
** Find a variable with the given name. If it is upvalue add this upvalue
** into all intermediate functions. If it is not found, set 'var' as EXP_VOID.
*/
static void varaux(FunctionState *fs, OString *name, ExpInfo *var, int base) {
    if (fs == NULL) /* last scope? */
        voidexp(var); /* not found */
    else { /* otherwise search locals/upvalues */
        if (searchlocal(fs, name, var, -1) == EXP_LOCAL) { /* local found? */
            if (!base) /* in recursive call to 'varaux'? */
                scopemarkupval(fs, var->u.var.vidx); /* use local as upvalue */
        } else { /* not found as local at current level; try upvalues */
            int idx = searchupvalue(fs, name); /* try existing upvalues */
            if (idx < 0) { /* not found? */
                varaux(fs->prev, name, var, 0); /* try upper levels */
                if (var->et == EXP_LOCAL || var->et == EXP_UVAL) /* found? */
                    idx = addupvalue(fs, name, var); /* add new upvalue */
                else /* it is global */
                    return; /* done */
            }
            initexp(var, EXP_UVAL, idx); /* new or old upvalue */
        }
    }
}


static void expname(Lexer *lx, ExpInfo *e) {
    initstring(e, str_expectname(lx));
}


// TODO: add docs, there is no more set/get global instead all is __ENV access
/* find variable 'name' */
static void var(Lexer *lx, OString *varname, ExpInfo *var) {
    FunctionState *fs= lx->fs;
    varaux(lx->fs, varname, var, 1);
    if (var->et == EXP_VOID) {
        ExpInfo key;
        varaux(fs, lx->envn, var, 1); /* get environment variable */
        cs_assert(var->et != EXP_VOID); /* this one must exist */
        csC_exp2stack(fs, var); /* put env on stack */
        initstring(&key, varname); /* key is variable name */
        csC_indexed(fs, var, &key, 0); /* env[varname] */
    }
}


#define varlit(lx,l,e)      var(lx, csY_newstring(lx, "" l, LL(l)), e)



/*-------------------------------------------------------------------------
**                              EXPRESSIONS
**------------------------------------------------------------------------- */

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


static void indexed(Lexer *lx, ExpInfo *var, int super) {
    ExpInfo key = INIT_EXP;
    csY_scan(lx); /* skip '[' */
    csC_exp2stack(lx->fs, var);
    expr(lx, &key);
    csC_indexed(lx->fs, var, &key, super);
    expectnext(lx, ']');
}


static void getdotted(Lexer *lx, ExpInfo *v, int super) {
    ExpInfo key = INIT_EXP;
    csY_scan(lx); /* skip '.' */
    csC_exp2stack(lx->fs, v);
    expname(lx, &key);
    csC_getdotted(lx->fs, v, &key, super);
}


/* TODO: implement CALLSUP */
static void superkw(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    if (c_unlikely(fs->cs == NULL))
        csP_semerror(lx, "used 'super' outside of class method definition");
    else if (c_unlikely(!fs->cs->super))
        csY_syntaxerror(lx, "used 'super' but class does not inherit");
    else {
        csY_scan(lx); /* skip 'super' */
        varlit(lx, "self", e);          /* get class... */
        cs_assert(e->et == EXP_LOCAL);  /* which must be local... */
        csC_exp2stack(fs, e);           /* ...and put it on stack */
        if (check(lx, '[')) /* index access? */
            indexed(lx, e, 1);
        else if (check(lx, '.')) /* field access? */
            getdotted(lx, e, 1);
        else /* get superclass */
            csC_emitI(lx->fs, OP_SUPER);
    }
}


static void primaryexp(Lexer *lx, ExpInfo *e) {
    switch (lx->t.tk) {
        case '(': {
            int line = lx->line;
            csY_scan(lx); /* skip ')' */
            expr(lx, e);
            expectmatch(lx, ')', '(', line);
            csC_exp2val(lx->fs, e);
            break;
        }
        case TK_NAME: {
            var(lx, str_expectname(lx), e);
            break;
        }
        case TK_SUPER: {
            superkw(lx, e);
            break;
        }
        default: {
            csY_syntaxerror(lx, "unexpected symbol");
        }
    }
}


/* TODO: implement CALLPROPERTY */
static void call(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int line = lx->line;
    int base = fs->sp - 1;
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
    initexp(e, EXP_CALL, csC_call(fs, base, CS_MULRET));
    csC_fixline(fs, line);
    fs->sp = base; /* call removes function and arguments */
}


static void suffixedexp(Lexer *lx, ExpInfo *e) {
    primaryexp(lx, e);
    for (;;) {
        switch (lx->t.tk) {
            case '.': {
                getdotted(lx, e, 0);
                break;
            }
            case '[': {
                indexed(lx, e, 0);
                break;
            }
            case '(': {
                csC_exp2stack(lx->fs, e);
                call(lx, e);
                break;
            }
            default: return;
        }
    }
}


/* {====================================================================
** List constructor
** ===================================================================== */

typedef struct LConstructor {
    ExpInfo *l; /* list descriptor */
    ExpInfo v; /* last list item descriptor */
    int narray; /* number of list elements already stored */
    int tostore; /* number of list elements pending to be stored */
} LConstructor;


static void listfield(Lexer *lx, LConstructor *c) {
    expr(lx, &c->v);
    c->tostore++;
}


static void checklistlimit(FunctionState *fs, LConstructor *c) {
    int size;
    if (c->narray <= CS_MAXINT - c->tostore)
       size = c->narray + c->tostore;
    else /* otherwise overflow */
        size = CS_MAXINT; /* force error */
    csP_checklimit(fs, size, CS_MAXINT, "elements in a list constructor");
}


static void closelistfield(FunctionState *fs, LConstructor *c) {
    if (c->v.et == EXP_VOID) return; /* there is no list item */
    csC_exp2stack(fs, &c->v); /* put the item on stack */
    voidexp(&c->v); /* now empty */
    if (c->tostore == LISTFIELDS_PER_FLUSH) { /* flush? */
        checklistlimit(fs, c);
        csC_setlist(fs, c->l->u.info, c->narray, c->tostore);
        c->narray += c->tostore; /* add to total */
        c->tostore = 0; /* no more pending items */
    }
}


static void lastlistfield(FunctionState *fs, LConstructor *c) {
    if (c->tostore == 0) return;
    checklistlimit(fs, c);
    if (eismulret(&c->v)) { /* last item has multiple returns? */
        csC_setmulret(fs, &c->v);
        csC_setlist(fs, c->l->u.info, c->narray, CS_MULRET);
        c->narray--; /* do not count last expression */
    } else {
        if (c->v.et != EXP_VOID) /* have item? */
            csC_exp2stack(fs, &c->v); /* ensure it is on stack */
        csC_setlist(fs, c->l->u.info, c->narray, c->tostore);
    }
    c->narray += c->tostore;
}


static void listdef(Lexer *lx, ExpInfo *l) {
    FunctionState *fs = lx->fs;
    int line = lx->line;
    int pc = csC_emitIS(fs, OP_NEWLIST, 0);
    LConstructor c = { .l = l, .v = INIT_EXP };
    initexp(l, EXP_FINEXPR, fs->sp); /* finalize list expression */
    csC_reserveslots(fs, 1); /* space for list */
    expectnext(lx, '[');
    do {
        cs_assert(c.v.et == EXP_VOID || c.tostore > 0);
        if (check(lx, ']')) break; /* delimiter; no more elements */
        closelistfield(fs, &c); /* try to close any pending list elements */
        listfield(lx, &c); /* get list element */
    } while (match(lx, ',') || match(lx, ';'));
    expectmatch(lx, ']', '[', line);
    lastlistfield(fs, &c);
    csC_setlistsize(fs, pc, c.narray);
}

/* }==================================================================== */


/* {====================================================================
** Table consturctor
** ===================================================================== */

typedef struct TConstructor {
    ExpInfo *t; /* table descriptor */
    ExpInfo v; /* last table item descriptor */
    int nhash; /* number of table elements */
} TConstructor;


static void tabindex(Lexer *lx, ExpInfo *e) {
    expectnext(lx, '[');
    expr(lx, e);
    csC_exp2val(lx->fs, e);
    expectnext(lx, ']');
}


static void tabfield(Lexer *lx, TConstructor *c) {
    FunctionState *fs = lx->fs;
    ExpInfo t, k, v;
    if (check(lx, TK_NAME)) {
        csP_checklimit(fs, c->nhash, CS_MAXINT, "records in a table constructor");
        expname(lx, &k);
    } else
        tabindex(lx, &k);
    c->nhash++;
    expectnext(lx, '=');
    t = *c->t; /* copy of table descriptor */
    csC_indexed(fs, &t, &k, 0);
    expr(lx, &v);
    csC_exp2stack(fs, &v);
    csC_pop(fs, csC_store(fs, &t) - 1); /* -1 to keep table */
}


static void tabledef(Lexer *lx, ExpInfo *t) {
    FunctionState *fs = lx->fs;
    int line = lx->line;
    int pc = csC_emitIS(fs, OP_NEWTABLE, 0);
    TConstructor c = { .t = t, .v = INIT_EXP };
    initexp(t, EXP_FINEXPR, fs->sp); /* finalize table expression */
    csC_reserveslots(fs, 1); /* space for table */
    expectnext(lx, '{');
    do {
        if (check(lx, '}')) break; /* delimiter; no more fields */
        tabfield(lx, &c);
    } while (match(lx, ',') || match(lx, ';'));
    expectmatch(lx, '}', '{', line);
    csC_settablesize(fs, pc, c.nhash);
}

/* }==================================================================== */


static void dottedname(Lexer *lx, ExpInfo *v) {
    var(lx, str_expectname(lx), v);
    while (check(lx, '.'))
        getdotted(lx, v, 0);
}


static void method(Lexer *lx) {
    ExpInfo var, dummy;
    int line = lx->line;
    csY_scan(lx); /* skip 'fn' */
    expname(lx, &var);
    funcbody(lx, &dummy, 1, line);
    csC_methodset(lx->fs, &var);
}


static void metafield(Lexer *lx, c_ubyte *amt) {
    FunctionState *fs = lx->fs;
    OString *mtname;
    ExpInfo e;
    int mt;
    expname(lx, &e);
    mtname = e.u.str;
    if (c_unlikely(!ismetatag(mtname)))
        csP_semerror(lx, "invalid metafield name");
    mt = mtname->extra - NUM_KEYWORDS - 1;
    if (c_unlikely(amt[mt]))
        csP_semerror(lx, "metafield redefinition");
    amt[mt] = 1; /* mark as defined */
    expectnext(lx, '=');
    if (check(lx, TK_FN)) { /* function expression? */
        csY_scan(lx); /* skip 'fn' */
        funcbody(lx, &e, 1, lx->line); /* also a method */
    } else
        expr(lx, &e);
    csC_exp2stack(fs, &e);
    expectnext(lx, ';');
    csC_mtset(fs, mt);
}


static int classbody(Lexer *lx) {
    int nmethods = 0;
    c_ubyte amt[CS_MT_NUM] = {0};
    while (!check(lx, '}') && !check(lx, TK_EOS)) {
        if (!check(lx, TK_FN))
            metafield(lx, amt);
        else {
            method(lx);
            nmethods++;
        }
    }
    return nmethods;
}


static void klass(Lexer *lx, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int pc = csC_emitIS(fs, OP_NEWCLASS, 0);
    int line, nm;
    ClassState cs;
    cs.prev = fs->cs; cs.super = 0;
    fs->cs = &cs;
    csC_reserveslots(fs, 1); /* space for class */
    if (match(lx, TK_INHERITS)) { /* class object inherits? */
        int cls = fs->sp - 1;
        ExpInfo v;
        dottedname(lx, &v);             /* get superclass... */
        csC_exp2stack(fs, &v);          /* put it on stack... */
        csC_load(fs, cls);              /* load class... */
        csC_emitI(fs, OP_INHERIT);      /* ...and do the inherit */
        cs.super = 1; /* true; have superclass */
    }
    line = lx->line;
    expectnext(lx, '{');
    nm = classbody(lx);
    if (nm > 0) /* have methods? */
        SET_ARG_S(&fs->p->code[pc], 0, csO_ceillog2(nm + (nm == 1)));
    expectmatch(lx, '}', '{', line);
    if (cs.super) /* have superclass? */
        csC_pop(fs, 2); /* pop superclass and class copy */
    fs->cs = cs.prev;
    if (e) initexp(e, EXP_FINEXPR, pc);
}


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
            initexp(e, EXP_VARARG, csC_vararg(lx->fs, CS_MULRET));
            break;
        }
        case '[': {
            listdef(lx, e);
            return;
        }
        case '{': {
            tabledef(lx, e);
            return;
        }
        case TK_FN: {
            int linenum = lx->line;
            csY_scan(lx);
            funcbody(lx, e, 0, linenum);
            return;
        }
        case TK_CLASS: {
            csY_scan(lx); /* skip 'class' */
            klass(lx, e);
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
        case TK_IDIV: return OPR_IDIV;
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
    c_ubyte left;
    c_ubyte right;
} priority[] = { /* ORDER OPR */
    /* binary operators priority */
    {12, 12}, {12, 12},                         /* '+' '-' */
    {13, 13}, {13, 13}, {13, 13}, {13, 13},     /* '*' '/' '//' '%' */
    {16, 15},                                   /* '**' (right associative) */
    {9, 9}, {9, 9},                             /* '<<' '>>' */
    {6, 6}, {4, 4}, {5, 5},                     /* '&' '|' '^' */
    {11, 10},                                   /* '..' (right associative) */
    {7, 7}, {7, 7},                             /* '==' '!=' */
    {8, 8}, {8, 8},                             /* '<' '<= */
    {8, 8}, {8, 8},                             /* '>' '>= */
    {3, 3}, {2, 2},                             /* 'and' 'or' */
    {1, 1}                                      /* TODO: '?:' (ternary) */
};

#define UNARY_PRIORITY  14  /* priority for unary operators */


static Binopr subexpr(Lexer *lx, ExpInfo *e, int limit) {
    Binopr op;
    Unopr uop;
    enterCstack(lx);
    uop = getunopr(lx->t.tk);
    if (uop != OPR_NOUNOPR) {
        int line = lx->line;
        csY_scan(lx); /* skip operator */
        subexpr(lx, e, UNARY_PRIORITY);
        csC_unary(lx->fs, e, uop, line);
    } else
        simpleexp(lx, e);
    op = getbinopr(lx->t.tk);
    while (op != OPR_NOBINOPR && priority[op].left > limit) {
        ExpInfo e2 = INIT_EXP;
        Binopr next;
        int line = lx->line;
        csY_scan(lx); /* skip operator */
        csC_prebinary(lx->fs, e, op, line);
        next = subexpr(lx, &e2, priority[op].right);
        csC_binary(lx->fs, e, &e2, op, line);
        op = next;
    }
    leaveCstack(lx);
    return op;
}


/* expr ::= subexpr */
static void expr(Lexer *lx, ExpInfo *e) {
    subexpr(lx, e, 0);
}



/* ======================================================================
**                              STATEMENTS
** ====================================================================== */


static void decl_list(Lexer *lx, int blocktk) {
    while (!check(lx, TK_EOS) && !(blocktk && check(lx, blocktk))) {
        if (check(lx, TK_RETURN) || /* if return or... */
                check(lx, TK_CONTINUE) || /* continue or... */
                check(lx, TK_BREAK)) {  /* ...break? */
            stm(lx); /* then it must be the last statement */
            return; /* done */
        } else /* otherwise it is a declaration */
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
            if (uv->kind != VARREG)
                varid = uv->name;
            break;
        }
        case EXP_LOCAL: {
            LVar *lv = getlocalvar(fs, var->u.info);
            if (lv->s.kind != VARREG)
                varid = lv->s.name;
            break;
        }
        default: return; /* cannot be read-only */
    }
    if (varid) {
        const char *msg = csS_pushfstring(lx->C,
            "attempt to assign to read-only variable '%s'", getstr(varid));
        csP_semerror(lx, msg);
    }
}


/* adjust left and right side of an assignment */
static void adjustassign(Lexer *lx, int nvars, int nexps, ExpInfo *e) {
    FunctionState *fs = lx->fs;
    int need = nvars - nexps;
    if (eismulret(e)) {
        need++; /* do not count '...' or the function being called */
        if (need > 0) { /* more variables than values? */
            csC_setreturns(fs, e, need);
            need = 0; /* no more values needed */
        } else /* otherwise more values than variables */
            csC_setreturns(fs, e, 0); /* call should return no values */
    } else {
        if (e->et != EXP_VOID) /* have one or more expressions? */
            csC_exp2stack(fs, e); /* finalize the last expression */
        if (need > 0) { /* more variables than values? */
            csC_nil(fs, need); /* assign them as nil */
            return; /* done */
        }
    }
    if (need > 0) /* more variables than values? */
        csC_reserveslots(fs, need); /* slots for call results or varargs */
    else /* otherwise more values than variables */
        csC_pop(fs, -need); /* pop them (if any) */
}


/*
** Structure to chain all variables on the left side of the
** assignment.
*/
struct LHS_assign {
    struct LHS_assign *prev;
    ExpInfo v;
};


static int assign(Lexer *lx, struct LHS_assign *lhs, int nvars) {
    int left = 0; /* number of values left in the stack after assignment */
    expect_cond(lx, eisvar(&lhs->v), "expect variable");
    checkreadonly(lx, &lhs->v);
    if (match(lx, ',')) { /* more vars ? */
        struct LHS_assign var;
        var.prev = lhs; /* chain previous variable */
        suffixedexp(lx, &var.v);
        enterCstack(lx); /* control recursion depth */
        left = assign(lx, &var, nvars + 1);
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
    if (eisindexed(&lhs->v))
        left += csC_storevar(lx->fs, &lhs->v, left+nvars-1);
    else /* no leftover values */
        csC_store(lx->fs, &lhs->v);
    return left;
}


static void expstm(Lexer *lx) {
    struct LHS_assign v;
    suffixedexp(lx, &v.v);
    if (check(lx, '=') || check(lx, ',')) { /* assignment? */
        v.prev = NULL;
        csC_adjuststack(lx->fs, assign(lx, &v, 1));
    } else { /* otherwise must be call */
        FunctionState *fs = lx->fs;
        expect_cond(lx, v.v.et == EXP_CALL, "syntax error");
        csC_setreturns(fs, &v.v, 0); /* call statement returns nothing */
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
                csS_pushfstring(lx->C, "unknown attribute '%s'", attr));
    }
    return VARREG;
}


static int newlocalvar(Lexer *lx, OString *name, int ign) {
    if (!ign || !(getstrlen(name) == 1 && *getstr(name) == '_')) {
        ExpInfo dummy;
        int limit = lx->fs->scope->nactlocals - 1;
        if (c_unlikely(searchlocal(lx->fs, name, &dummy, limit) >= 0))
            csP_semerror(lx, csS_pushfstring(lx->C,
                        "redefinition of local variable '%s'", getstr(name)));
    }
    return addlocal(lx, name);
}


static void checkclose(FunctionState *fs, int level) {
    if (level != -1) {
        scopemarkclose(fs);
        csC_emitIL(fs, OP_TBC, level);
    }
}


static void localstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int toclose = -1;
    int nvars = 0;
    int kind, vidx;
    int nexps;
    ExpInfo e = INIT_EXP;
    do {
        vidx = newlocalvar(lx, str_expectname(lx), 1);
        kind = getlocalattribute(lx);
        getlocalvar(fs, vidx)->s.kind = kind;
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


static void localfn(Lexer *lx) {
    ExpInfo e;
    FunctionState *fs = lx->fs;
    int fvar = fs->nactlocals; /* function's variable index */
    newlocalvar(lx, str_expectname(lx), 0); /* create new local... */
    adjustlocals(lx, 1); /* ...and register it */
    funcbody(lx, &e, 0, lx->line);
    /* debug information will only see the variable after this point! */
    getlocalinfo(fs, fvar)->startpc = currPC;
}


static void localclass(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int cvar = fs->nactlocals; /* class variable index */
    OString *name;
    name = str_expectname(lx);
    newlocalvar(lx, name, 0); /* create new local... */
    adjustlocals(lx, 1); /* ...and register it */
    klass(lx, NULL);
    /* debug information will only see the variable after this point! */
    getlocalinfo(fs, cvar)->startpc = currPC;
}


static void blockstm(Lexer *lx) {
    int line = lx->line;
    Scope s;
    csY_scan(lx); /* skip '{' */
    enterscope(lx->fs, &s, 0); /* explicit scope */
    decl_list(lx, '}'); /* body */
    expectmatchblk(lx, line);
    leavescope(lx->fs);
}


static void paramlist(Lexer *lx) {
    FunctionState *fs = lx->fs;
    Proto *fn = fs->p;
    int nparams = 0;
    int isvararg = 0;
    if (!check(lx, ')')) { /* have at least one arg? */
        do {
            switch (lx->t.tk) {
                case TK_NAME: {
                    newlocalvar(lx, str_expectname(lx), 1);
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
static void codeclosure(Lexer *lx, ExpInfo *e, int line) {
    FunctionState *fs = lx->fs->prev;
    initexp(e, EXP_FINEXPR, csC_emitIL(fs, OP_CLOSURE, fs->np - 1));
    csC_fixline(fs, line);
    csC_reserveslots(fs, 1); /* space for closure */
}


static void funcbody(Lexer *lx, ExpInfo *v, int ismethod, int line) {
    Scope scope;
    FunctionState newfs;
    newfs.p = addproto(lx);
    newfs.p->defline = line;
    open_func(lx, &newfs, &scope);
    expectnext(lx, '(');
    if (ismethod) { /* is this method ? */
        cs_assert(newfs.prev->cs); /* enclosing func. must have ClassState */
        newfs.cs = newfs.prev->cs; /* set ClassState */
        addlocallit(lx, "self"); /* create 'self' local */
        adjustlocals(lx, 1); /* 'paramlist()' reserves stack slots */
    }
    paramlist(lx); /* get function parameters */
    expectmatch(lx, ')', '(', line);
    if (match(lx, '{')) {
        int curlyline = lx->line;
        decl_list(lx, '}');
        newfs.p->deflastline = lx->line;
        expectmatch(lx, '}', '{', curlyline);
    } else {
        stm(lx);
        newfs.p->deflastline = lx->line;
    }
    codeclosure(lx, v, line);
    cs_assert(ismethod == (newfs.cs != NULL && (newfs.cs == newfs.prev->cs)));
    newfs.cs = NULL; /* clear ClassState (if any) */
    close_func(lx);
}


static void fnstm(Lexer *lx, int linenum) {
    FunctionState *fs = lx->fs;
    ExpInfo var, e;
    csY_scan(lx); /* skip 'fn' */
    dottedname(lx, &var);
    funcbody(lx, &e, 0, linenum);
    checkreadonly(lx, &var);
    csC_storepop(fs, &var, linenum);
}


static void classstm(Lexer *lx, int linenum) {
    FunctionState *fs = lx->fs;
    ExpInfo var;
    csY_scan(lx); /* skip 'class' */
    dottedname(lx, &var);
    klass(lx, NULL);
    checkreadonly(lx, &var);
    csC_storepop(fs, &var, linenum);
}


/* 'switch' statement state. */
typedef struct {
    TValue v; /* constant expression value */
    c_ubyte isconst; /* true if 'e' is constant */
    c_ubyte nomatch; /* true if switch has no compile-time match */
    c_ubyte havedefault; /* if switch has 'default' case */
    c_ubyte havenil; /* if switch has 'nil' case */
    c_ubyte havetrue; /* if switch has 'true' case */
    c_ubyte havefalse; /* if switch has 'false' case */
    int firstli; /* first literal value in parser state 'literals' array */
    int jmp; /* jump that needs patch if 'case' expression is not 'CMATCH' */
    enum { CNONE, CDFLT, CASE, CMATCH, CMISMATCH } c; /* cases */
} SwitchState;


/* convert literal information into text */
static const char *literal2text(cs_State *C, LiteralInfo *li) {
    switch (li->tt) {
        case CS_VNUMINT: return csS_pushfstring(C, " (%I)", li->lit.i);
        case CS_VNUMFLT: return csS_pushfstring(C, " (%f)", li->lit.n);
        case CS_VSHRSTR:case CS_VLNGSTR:
            return csS_pushfstring(C, " (%s)", getstr(li->lit.str));
        default: cs_assert(0); return NULL; /* invalid literal */
    }
}


/* find literal info 'li' in 'literals' */
static int findliteral(Lexer *lx, LiteralInfo *li, int first) {
    ParserState *ps = lx->ps;
    for (int i = first; i < ps->literals.len; i++) { /* O(n) */
        LiteralInfo *curr = &ps->literals.arr[i];
        if (li->tt != curr->tt) /* types don't match? */
            continue; /* skip */
        switch (li->tt) {
            case CS_VSHRSTR: case CS_VLNGSTR:
                if (eqstr(li->lit.str, curr->lit.str))
                    return i; /* found */
                break;
            case CS_VNUMINT:
                if (li->lit.i == curr->lit.i)
                    return i; /* found */
                break;
            case CS_VNUMFLT:
                if (c_numeq(li->lit.n, curr->lit.n))
                    return i; /* found */
                break;
            default: cs_assert(0); break; /* invalid literal */
        }
    }
    return -1; /* not found */
}


/*
** Checks if expression is a duplicate literal value.
*/
static int checkliteral(SwitchState *ss, ExpInfo *e, const char **what) {
    switch (e->et) {
        case EXP_FALSE: {
            if (c_unlikely(ss->havefalse))
                *what = "false";
            ss->havefalse = 1;
            break;
        }
        case EXP_TRUE: {
            if (c_unlikely(ss->havetrue))
                *what = "true";
            ss->havetrue = 1;
            break;
        }
        case EXP_NIL: {
            if (c_unlikely(ss->havenil))
                *what = "nil";
            ss->havenil = 1;
            break;
        }
        default: return 0;
    }
    return 1;
}


/*
** Checks if 'e' is a duplicate constant value and fills the relevant info.
** If 'li' is a duplicate, 'what' and 'extra' are filled accordingly.
*/
static void checkK(Lexer *lx, ExpInfo *e, LiteralInfo *li, int first,
                   int *extra, const char **what) {
    switch (e->et) {
        case EXP_STRING: {
            *what = "string";
            li->lit.str = e->u.str;
            li->tt = e->u.str->tt_;
            goto findliteral;
        }
        case EXP_INT: {
            *what = "integer";
            li->lit.i = e->u.i;
            li->tt = CS_VNUMINT;
            goto findliteral;
        }
        case EXP_FLT: {
            *what = "number";
            li->lit.n = e->u.n;
            li->tt = CS_VNUMFLT;
        findliteral: {
            if (c_likely(findliteral(lx, li, first) < 0))
                *what = NULL;
            else
                *extra = 1;
            break;
        }}
        default: cs_assert(0); break; /* 'e' is not a literal expression */
    }
}


/* check for duplicate literal otherwise fill the relevant info */
static void checkduplicate(Lexer *lx, SwitchState *ss, ExpInfo *e,
                           LiteralInfo *li) {
    int extra = 0;
    const char *what = NULL;
    if (!checkliteral(ss, e, &what))
         checkK(lx, e, li, ss->firstli, &extra, &what);
    if (c_unlikely(what)) { /* have duplicate? */
        const char *msg = csS_pushfstring(lx->C,
                            "duplicate %s literal%s in switch statement",
                            what, (extra ? literal2text(lx->C, li) : ""));
        csP_semerror(lx, msg);
    }
}


static void addliteralinfo(Lexer *lx, SwitchState *ss, ExpInfo *e) {
    ParserState *ps = lx->ps;
    LiteralInfo li;
    checkduplicate(lx, ss, e, &li);
    csP_checklimit(lx->fs, ps->literals.len, MAX_CODE, "switch cases");
    csM_growarray(lx->C, ps->literals.arr, ps->literals.size,
                  ps->literals.len, MAX_CODE, "switch literals", LiteralInfo);
    ps->literals.arr[ps->literals.len++] = li;
}


/* return values of 'checkmatch' */
#define NONEMATCH   0 /* both expressions are not constant expressions */
#define NOMATCH     1 /* both expressions are constants that do not match */
#define MATCH       2 /* expressions are compile time match */

/*
** Checks if 'e' is a compile-time match with the switch expression.
** Additionally it remembers the 'e' if it is a constant value and
** adds it to the list of literals; any duplicate literal value in switch
** is a compile-time error.
*/
static int checkmatch(Lexer *lx, SwitchState *ss, ExpInfo *e) {
    if (eisconstant(e)) {
        addliteralinfo(lx, ss, e);
        if (ss->isconst) { /* both are constant values? */
            TValue v;
            csC_const2v(lx->fs, e, &v);
            return csV_raweq(&ss->v, &v) + 1; /* NOMATCH or MATCH */
        } /* else fall-through */
    } /* else fall-through */
    ss->nomatch = 0; /* we don't know... */
    return NONEMATCH;
}


/* 
** Tries to preserve expression 'e' after consuming it, in order
** to enable more optimizations. Additionally opbarrier is set,
** meaning if 'e' is nil, then it should not be merged with previous,
** as it might get optimized out.
*/
static int codeconstexp(FunctionState *fs, ExpInfo *e) {
    int res = 0;
    fs->opbarrier |= 1;
    csC_exp2val(fs, e);
    if (eisconstant(e)) {
        ExpInfo pres = *e;
        csC_exp2stack(fs, e);
        *e = pres;
        res = 1; /* true; 'e' is a constant */
    }
    fs->opbarrier &= ~1;
    return res;
}


static void removeliterals(Lexer *lx, int nliterals) {
    ParserState *ps = lx->ps;
    if (ps->literals.len < ps->literals.size / 3) /* too many literals? */
        csM_shrinkarray(lx->C, ps->literals.arr, ps->literals.size,
                        ps->literals.size / 2, LiteralInfo);
    ps->literals.len = nliterals;
}


static void switchbody(Lexer *lx, SwitchState *ss, FuncContext *ctxbefore) {
    FunctionState *fs = lx->fs;
    int ftjmp = NOJMP; /* fall-through jump */
    FuncContext ctxdefault = {0};
    FuncContext ctxcase = {0};
    FuncContext ctxend = {0};
    ctxend.pc = -1;
    while (!check(lx, '}') && !check(lx, TK_EOS)) { /* while switch body... */
        if (check(lx, TK_CASE) || match(lx, TK_DEFAULT)) { /* has case?... */
            if (ss->c == CASE && check(lx, TK_CASE)) {
                /* had previous case that is followed by another case */
                cs_assert(ftjmp == NOJMP); /* can't have open 'ftjump' */
                ftjmp = csC_jmp(fs, OP_JMP); /* new fall-through jump */
                csC_patchtohere(fs, ss->jmp); /* patch test jump */
            }
            if (match(lx, TK_CASE)) { /* 'case'? */
                ExpInfo e = INIT_EXP; /* case expression */
                int match, line;
                if (c_unlikely(ss->havedefault))
                    csP_semerror(lx, "'default' must be the last case");
                storecontext(fs, &ctxcase); /* case might get optimized away */
                line = lx->line;
                expr(lx, &e);           /* get the case expression... */
                codeconstexp(fs, &e);   /* ...and put it on stack */
                expectnext(lx, ':');
                match = checkmatch(lx, ss, &e);
                if (match == MATCH) { /* case is compile-time match? */
                    ss->nomatch = 0; /* case is the match */
                    ss->c = CMATCH;
                    loadcontext(fs, ctxbefore); /* load context before switch */
                } else if (match == NOMATCH || ss->c == CMATCH) {
                    /* compile-time mismatch or previous case is a match */
                    if (ss->c != CMATCH) ss->c = CMISMATCH;
                    loadcontext(fs, &ctxcase); /* remove case expression */
                } else { /* else must check for match */
                    cs_assert(!ss->nomatch);
                    ss->c = CASE; /* regular case */
                    csC_emitI(fs, OP_EQPRESERVE); /* EQ but preserves lhs */
                    ss->jmp = csC_test(fs, OP_TESTPOP, 0, line);
                    fs->pcswtest = ss->jmp;
                }
            } else if (!ss->havedefault) { /* don't have 'default'? */
                expectnext(lx, ':');
                cs_assert(ftjmp == NOJMP); /* 'default' does not have ftjmp */
                if (ss->nomatch) { /* all cases are resolved without match? */
                    ss->nomatch = 0; /* default is the match */
                    loadcontext(fs, ctxbefore); /* remove them */
                } else if (ss->c == CASE) /* have test jump? */
                    csC_patchtohere(fs, ss->jmp); /* fix it */
                ss->havedefault = 1; /* now have 'default' */
                ss->c = CDFLT;
                storecontext(fs, &ctxdefault); /* store 'default' context */
            } else /* otherwise duplicate 'default' case */
                csP_semerror(lx, "multiple default cases in switch");
            if (ftjmp != NOJMP) { /* have fall-through jump to patch? */
                csC_patchtohere(fs, ftjmp); /* patch it */
                ftjmp = NOJMP; /* reset */
            }
        } else if (ss->c != CNONE) { /* or have previous case?... */
            stm(lx);
            if (ss->c == CMATCH /* current case is a match, */
                    && stmIsEnd(fs)) { /* and statement ends control flow? */
                cs_assert(ctxend.pc == NOPC); /* context must be free */
                storecontext(fs, &ctxend); /* set current state as end */
            } else if (ss->c == CMISMATCH) /* case optimized away? */
                loadcontext(fs, &ctxcase); /* remove statement */
        } else /* ...otherwise error */
            csP_semerror(lx, "expect at least one 'case' or 'default' label");
    }
    cs_assert(ftjmp == NOJMP); /* no more fall-through jumps */
    if (ctxend.pc != -1) /* had a compile-time match and 'ctxend' is stored? */
        loadcontext(fs, &ctxend); /* trim off dead code */
    else if (ss->c == CASE) /* 'case' is last (have test)? */
        csC_patchtohere(fs, ss->jmp); /* patch it */
    else if (ss->nomatch) /* compile-time no match? */
        loadcontext(fs, ctxbefore); /* remove the whole switch */
    removeliterals(lx, ss->firstli);
}


static void switchstm(Lexer *lx) {
    Scope s;
    int line;
    ExpInfo e;
    FuncContext ctxbefore;
    FunctionState *fs = lx->fs;
    int old_pcswtest = fs->pcswtest;
    Scope *prev = fs->switchscope;
    SwitchState ss = {
        .isconst = 0, .nomatch = 1,
        .firstli = lx->ps->literals.len,
        .jmp = NOJMP,
        .c = CNONE
    };
    enterscope(fs, &s, CFMS);
    storecontext(fs, &ctxbefore);
    fs->switchscope = &s; /* set the current 'switch' scope */
    csY_scan(lx); /* skip 'switch' */
    expr(lx, &e); /* get the 'switch' expression... */
    if ((ss.isconst = codeconstexp(fs, &e))) /* constant expression? */
        csC_const2v(fs, &e, &ss.v); /* get its value */
    addlocallit(lx, "(switch)"); /* switch expression temporary */
    adjustlocals(lx, 1); /* register switch temporary */
    line = lx->line;
    expectnext(lx, '{');
    switchbody(lx, &ss, &ctxbefore);
    expectmatch(lx, '}', '{', line);
    leavescope(fs);
    fs->pcswtest = old_pcswtest;
    fs->switchscope = prev;
}


/* data for 'condbody()' */
typedef struct CondBodyState {
    FuncContext ctxbefore; /* snapshot before the condition (and body) */
    FuncContext ctxend; /* pc != NOPC if dead code was found */
    ExpInfo e; /* condition expression */
    OpCode opT; /* test opcode */
    OpCode opJ; /* jump opcode */
    int isif; /* true if this is 'if' statement body */
    int pcCond; /* pc of condition */
    int pcClause; /* pc of last 'for' loop clause */
    int condline; /* condition line (for test instruction) */
} CondBodyState;


/* condition statement body; for 'forstm', 'whilestm' and 'ifstm' */
static void condbody(Lexer *lx, CondBodyState *cb) {
    FunctionState *fs = lx->fs;
    int optaway, target;
    int bodypc = currPC;
    int istrue = eistrue(&cb->e);
    int test = NOJMP, jump = NOJMP;
    cb->ctxend.pc = NOPC; /* no dead code to begin with */
    optaway = (eisconstant(&cb->e) && !istrue);
    if (!optaway) { /* statement will not be optimized away? */
        if (istrue) { /* condition is true? */
            if (cb->pcClause == NOJMP) { /* not a forloop? */
                loadcontext(fs, &cb->ctxbefore); /* remove condition */
                bodypc = currPC; /* adjust bodypc */
            } /* (otherwise condition is already optimized out) */
        } else /* otherwise emit condition test */
            test = csC_test(fs, cb->opT, 0, cb->condline);
    }
    stm(lx); /* loop/if body */
    if (optaway) /* optimize away this statement? */
        loadcontext(fs, &cb->ctxbefore);
    else if (istrue && (stmIsEnd(fs) || cb->isif)) {
        /* condition is true and body is cflow. end or this is 'if' */
        storecontext(fs, &cb->ctxend); /* this is end, rest is dead code */
    } else { /* otherwise unknown condition value */
        jump = csC_jmp(fs, cb->opJ); /* loop or if jump */
        if (!cb->isif) { /* loop statement? */
            if (cb->pcClause != NOJMP) /* 'for' loop? */
                csC_patch(fs, jump, cb->pcClause); /* jump to last clause */
            else if (istrue) { /* 'while' loop with true condition? */
                csC_patch(fs, jump, bodypc); /* jump to start of the body */
                fs->ls->pcloop = bodypc; /* convert it to infinite loop */ 
            } else /* 'while' loop with non-constant condition */
                csC_patch(fs, jump, cb->pcCond); /* jump back to condition */
        }
    }
    target = currPC; /* set test jump target */
    if (cb->isif) { /* is if statement? */
        if (match(lx, TK_ELSE)) /* have else? */
            stm(lx); /* else body */
        if (target == currPC) { /* no else branch or it got removed? */
            if (jump != NOJMP) { /* have if jump (opJ)? */
                cs_assert(*prevOP(fs) == cb->opJ);
                csC_removelastjump(fs); /* remove that jump */
                target = currPC; /* adjust test target */
            } else cs_assert(optaway || istrue);
        } else if (jump != NOJMP) /* have else branch and 'if' jump (opJ) */
            csC_patchtohere(fs, jump); /* (it jumps over else statement) */
    }
    if (test != NOJMP) /* have condition test? */
        csC_patch(fs, test, target); /* patch it */
    if (cb->ctxend.pc != NOPC) /* statement has "dead" code? */
        loadcontext(fs, &cb->ctxend); /* trim off dead code */
}


static void ifstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    CondBodyState cb = {
        .e = INIT_EXP, .isif = 1,
        .opT = OP_TESTPOP, .opJ = OP_JMP,
        .pcCond = currPC, .pcClause = NOJMP,
    };
    storecontext(fs, &cb.ctxbefore);
    csY_scan(lx); /* skip 'if' */
    cb.condline = lx->line;
    expr(lx, &cb.e);
    codeconstexp(fs, &cb.e);
    condbody(lx, &cb);
}


static void enterloop(FunctionState *fs, struct LoopState *ls, int isgen) {
    enterscope(fs, &ls->s, isgen ? CFMGL : CFML);
    ls->pcloop = currPC;
    ls->prev = fs->ls; /* chain it */
    fs->ls = ls;
}


static void leaveloop(FunctionState *fs) {
    leavescope(fs);
    fs->ls = check_exp(fs->ls, fs->ls->prev);
}


static void whilestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopState ls;
    CondBodyState cb = {
        .e = INIT_EXP, .isif = 0,
        .opT = OP_TESTPOP, .opJ = OP_JMPS,
        .pcCond = currPC, .pcClause = NOJMP,
    };
    csY_scan(lx); /* skip 'while' */
    enterloop(fs, &ls, 0);
    storecontext(fs, &cb.ctxbefore);
    cb.condline = lx->line;
    expr(lx, &cb.e);
    codeconstexp(fs, &cb.e);
    condbody(lx, &cb);
    leaveloop(fs);
}


/* patch for loop jump */
static void patchforjmp(FunctionState *fs, int pc, int target, int back) {
    Instruction *jmp = &fs->p->code[pc];
    int offset = target - (pc + getopSize(*jmp));
    if (back) offset = -offset;
    cs_assert(offset >= 0);
    if (c_unlikely(offset > MAXJMP))
        csY_syntaxerror(fs->lx, "control structure (for loop) too long");
    SET_ARG_L(jmp, 1, offset);
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
        limiterror(lx->fs, "'foreach' expressions", limit);
    return nexpr;
}


static void foreachstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopState ls;
    int forend, prep;
    int nvars = 1; /* number of results for interator */
    int base = fs->sp;
    int line;
    ExpInfo e = INIT_EXP;
    Scope s;
    enterloop(fs, &ls, 1); /* enter loop (scope for control variables) */
    csY_scan(lx); /* skip 'foreach' */
    addlocallit(lx, "(foreach iter)");      /* iterator         (base)   */
    addlocallit(lx, "(foreach invariant)"); /* invariant state  (base+1) */
    addlocallit(lx, "(foreach cntlvar)");   /* control var      (base+2) */
    addlocallit(lx, "(foreach tbcvar)");    /* to-be-closed var (base+3) */
    /* create locals variables */
    newlocalvar(lx, str_expectname(lx), 1); /* at least one var. expected */
    while (match(lx, ',')) {
        newlocalvar(lx, str_expectname(lx), 1);
        nvars++;
    }
    expectnext(lx, TK_IN);
    line = lx->line;
    adjustassign(lx, VAR_N, forexplist(lx, &e, VAR_N), &e);
    adjustlocals(lx, VAR_N); /* register control variables */
    scopemarkclose(fs); /* last control variable might get closed */
    csC_checkstack(fs, 3); /* extra space to call generator */
    prep = csC_emitILL(fs, OP_FORPREP, base, 0);
    enterscope(fs, &s, 0); /* scope for declared locals */
    adjustlocals(lx, nvars); /* register delcared locals */
    csC_reserveslots(fs, nvars); /* space for declared locals */
    stm(lx); /* body */
    leavescope(fs); /* leave declared locals scope */
    patchforjmp(fs, prep, currPC, 0);
    fs->ls->pcloop = currPC; /* generic loop starts here */
    csC_emitILL(fs, OP_FORCALL, base, nvars);
    csC_fixline(fs, line);
    forend = csC_emitILLL(fs, OP_FORLOOP, base, 0, nvars);
    patchforjmp(fs, forend, prep + getopSize(OP_FORPREP), 1);
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
            expstm(lx);
            expectnext(lx, ';');
        }
    }
}


/* 'for' loop condition */
int forcondition(Lexer *lx, ExpInfo *e) {
    int line = lx->line;
    if (!match(lx, ';')) { /* have condition? */
        expr(lx, e);                /* get it... */
        codeconstexp(lx->fs, e);    /* ...and put it on stack */
        line = lx->line; /* update line */
        expectnext(lx, ';');
    } else /* otherwise no condition (infinite loop) */
        initexp(e, EXP_TRUE, 0);
    return line;
}


/* 'for' loop last clause */
void forlastclause(Lexer *lx, FuncContext *ctxbefore, ExpInfo *cond,
                              int condline, int *pcClause) {
    int bodyjmp, loopjmp;
    int inf = eistrue(cond);
    FunctionState *fs = lx->fs;
    cs_assert(*pcClause == NOJMP);
    if (inf) /* infinite loop? */
        loadcontext(fs, ctxbefore); /* remove condition */
    if (check(lx, ')') || check(lx, ';')) /* no end clause? */
        return; /* done (converted to a 'while' or 'loop') */
    else {
        bodyjmp = csC_jmp(fs, OP_JMP); /* insert jump in-between */
        csC_fixline(fs, condline); /* this is condition jump */
        *pcClause = currPC; /* set end clause pc */
        expstm(lx); /* get the end clause expression statement */
        if (!inf) { /* loop is not infinite? */
            loopjmp = csC_jmp(fs, OP_JMPS); /* emit jump, back to cond... */
            csC_patch(fs, loopjmp, fs->ls->pcloop); /* ...and patch it */
        }
        csC_patchtohere(fs, bodyjmp); /* patch jump from condition to body */
        fs->ls->pcloop = *pcClause; /* loop starts at end clause pc */
    }
}


static void forstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopState ls;
    CondBodyState cb = {
        .e = INIT_EXP, .isif = 0,
        .opT = OP_TESTPOP, .opJ = OP_JMPS,
        .pcClause = NOJMP
    };
    Scope s;
    int line, opt;
    csY_scan(lx); /* skip 'for' */
    enterscope(fs, &s, 0); /* enter initializer scope */
    line = lx->line;
    opt = match(lx, '(');
    /* 1st clause (initializer) */
    forinitializer(lx); 
    enterloop(fs, &ls, 0); /* enter loop scope */
    cb.pcCond = fs->ls->pcloop = currPC; /* loop is after initializer clause */
    storecontext(fs, &cb.ctxbefore); /* store context at loop start */
    /* 2nd clause (condition) */
    cb.condline = forcondition(lx, &cb.e);
    /* 3rd clause */
    forlastclause(lx, &cb.ctxbefore, &cb.e, cb.condline, &cb.pcClause);
    if (opt) /* have optional ')' ? */
        expectmatch(lx, ')', '(', line);
    else /* otherwise must terminate last clause with ';' */
        expectnext(lx, ';');
    condbody(lx, &cb); /* forbody */
    leaveloop(fs); /* leave loop scope */
    leavescope(fs); /* leave initializer scope */
}


static void loopstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    struct LoopState ls;
    int jmp, lstart;
    csY_scan(lx); /* skip 'loop' */
    lstart = currPC; /* store the pc where the loop starts */
    enterloop(fs, &ls, 0);
    stm(lx);
    if (!stmIsEnd(fs)) { /* statement (body) does not end control flow? */
        jmp = csC_jmp(fs, OP_JMPS);
        csC_patch(fs, jmp, lstart); /* jump back to loop start */
    }
    leaveloop(fs);
}


/*
** Return true if jump from current scope to 'limit' scope needs a close.
** If 'limit' is NULL, then 'limit' is considered to be the outermost scope.
*/
static int needtoclose(Lexer *lx, const Scope *limit) {
    Scope *s = lx->fs->scope;
    cs_assert(!limit || limit->depth <= s->depth);
    while (s != limit) {
        if (s->haveupval)
            return 1; /* yes */
        s = s->prev;
    }
    return 0; /* no */
}


static void continuestm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    LoopState *ls = fs->ls;
    csY_scan(lx); /* skip 'continue' */
    if (c_unlikely(ls == NULL)) /* not in a loop? */
        csP_semerror(lx, "'continue' outside of a loop statement");
    if (needtoclose(lx, ls->s.prev))
        csC_emitIL(fs, OP_CLOSE, stacklevel(fs, ls->s.nactlocals));
    contadjust(fs, 0);
    if (is_genloop(&ls->s)) /* generic loop? */
        newpendingjump(lx, 0, 0, 0); /* 'continue' compiles as 'break' */
    else /* otherwise regular loop */
        csC_patch(fs, csC_jmp(fs, OP_JMPS), ls->pcloop); /* backpatch */
    expectnext(lx, ';');
    contadjust(fs, 1); /* adjust stack for compiler and symbolic execution */
    fs->lastisend = 3; /* statement is a continue */
}


static void breakstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    const Scope *cfs = getcfscope(fs); /* control flow scope */
    int adjust;
    csY_scan(lx); /* skip 'break' */
    if (c_unlikely(cfs == NULL)) /* no control flow scope? */
        csP_semerror(lx, "'break' outside of a loop or switch statement");
    adjust = fs->nactlocals - cfs->nactlocals;
    newpendingjump(lx, 1, needtoclose(lx, cfs->prev), adjust);
    expectnext(lx, ';');
    /* adjust stack for compiler and symbolic execution */
    csC_adjuststack(fs, -adjust);
    fs->lastisend = 2; /* statement is a break */
}


static void returnstm(Lexer *lx) {
    FunctionState *fs = lx->fs;
    int first = fs->sp;
    int nret = 0;
    ExpInfo e = INIT_EXP;
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
    fs->sp = first; /* removes all temp values */
    fs->lastisend = 1; /* statement is a return */
}


static void stm_(Lexer *lx) {
    int tk = lx->t.tk;
    switch (tk) {
        case TK_FN: fnstm(lx, lx->line); break;
        case TK_CLASS: classstm(lx, lx->line); break;
        case TK_WHILE: whilestm(lx); break;
        case TK_FOR: forstm(lx); break;
        case TK_FOREACH: foreachstm(lx); break;
        case TK_IF: ifstm(lx); break;
        case TK_SWITCH: switchstm(lx); break;
        case '{': blockstm(lx); break;
        case TK_CONTINUE: continuestm(lx); break;
        case TK_BREAK: breakstm(lx); break;
        case TK_RETURN: returnstm(lx); break;
        case TK_LOOP: loopstm(lx); break;
        case ';': csY_scan(lx); break;
        default: {
            expstm(lx);
            expectnext(lx, ';');
            break;
        }
    }
    if (tk != TK_RETURN && tk != TK_CONTINUE && tk != TK_BREAK)
        lx->fs->lastisend = 0; /* clear flag */
}


#define stackinvariant(fs) \
        (fs->p->maxstack >= fs->sp && fs->sp >= nvarstack(fs))


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
            lx->fs->lastisend = 0; /* clear flag */
            break;
        }
        default: {
            stm_(lx);
            break;
        }
    }
    cs_assert(stackinvariant(lx->fs));
    leaveCstack(lx);
}


static void stm(Lexer *lx) {
    enterCstack(lx);
    stm_(lx);
    cs_assert(stackinvariant(lx->fs));
    leaveCstack(lx);
}


/* compile main function */
static void mainfunc(FunctionState *fs, Lexer *lx) {
    Scope s;
    UpValInfo *env;
    open_func(lx, fs, &s);
    setvararg(fs, 0); /* main function is always vararg */
    env = newupvalue(fs);
    env->name = lx->envn;
    env->idx = 0;
    env->onstack = 1;
    env->kind = VARREG;
    csG_objbarrier(lx->C, fs->p, env->name);
    csY_scan(lx); /* scan for first token */
    decl_list(lx, 0); /* parse main body */
    expect(lx, TK_EOS);
    close_func(lx);
}


/* parse source code */
CSClosure *csP_parse(cs_State *C, BuffReader *br, Buffer *buff,
                     ParserState *ps, const char *source) {
    Lexer lx;
    FunctionState fs;
    CSClosure *cl = csF_newCSClosure(C, 1);
    setclCSval2s(C, C->sp.p, cl); /* anchor main function closure */
    csT_incsp(C);
    lx.tab = csH_new(C); /* create table for scanner */
    settval2s(C, C->sp.p, lx.tab); /* anchor it */
    csT_incsp(C);
    fs.p = cl->p = csF_newproto(C);
    csG_objbarrier(C, cl, cl->p);
    fs.p->source = csS_new(C, source);
    csG_objbarrier(C, fs.p, fs.p->source);
    lx.buff = buff;
    lx.ps = ps;
    csY_setinput(C, &lx, br, fs.p->source);
    cs_assert(!ps->gt.len && !ps->literals.len && !ps->actlocals.len);
    mainfunc(&fs, &lx);
    cs_assert(!fs.prev && fs.nupvals == 1 && !lx.fs);
    /* all scopes should be correctly finished */
    cs_assert(ps->actlocals.len == 0 && ps->gt.len == 0);
    C->sp.p--; /* remove scanner table */
    return cl;
}
