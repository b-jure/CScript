/*
** ccode.c
** Bytecode emiting functions
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "ccode.h"
#include "clexer.h"
#include "cbits.h"
#include "climits.h"
#include "cobject.h"
#include "cparser.h"
#include "cgc.h"
#include "cmem.h"


/* check if 'ExpInfo' has jumps */
#define hasjumps(e)     ((e)->t != (e)->f)


/* unary 'opr' to opcode */
#define unopr2op(opr) \
    cast(OpCode, cast_int(opr) - OPR_UNM + OP_NOT)


/* binary operation to OpCode; 'a' anchor, 'b' base */
#define binopr2op(opr,a,b) \
    cast(OpCode, (cast_int(opr) - cast_int(a)) + cast_int(b))



/* check if 'i' fits in long arg */
#define fitsLA(i)       (-LARGMAX <= (i) && (i) <= LARGMAX)

/* check if 'i' fits in short arg */
#define fitsSA(i)       (-SARGMAX <= (i) && (i) <= SARGMAX)



/* 
** OpCode properties table.
** (order 'OpCode')
*/
CSI_DEF const cs_ubyte csC_opProp[NUM_OPCODES] = {
    /*     M  J  T  F      */
    opProp(0, 0, 0, FormatI), /* OP_TRUE */
    opProp(0, 0, 0, FormatI), /* OP_FALSE */
    opProp(0, 0, 0, FormatI), /* OP_NIL */
    opProp(0, 0, 0, FormatIL), /* OP_NILN */
    opProp(0, 0, 0, FormatIS), /* OP_CONST */
    opProp(0, 0, 0, FormatIL), /* OP_CONSTL */
    opProp(0, 0, 0, FormatIL), /* OP_CONSTI */
    opProp(0, 0, 0, FormatIL), /* OP_CONSTF */
    opProp(0, 0, 0, FormatIL), /* OP_VARARGPREP */
    opProp(0, 0, 0, FormatIL), /* OP_VARARG */
    opProp(0, 0, 0, FormatIL), /* OP_CLOSURE */
    opProp(0, 0, 0, FormatIS), /* OP_NEWARRAY */
    opProp(0, 0, 0, FormatI), /* OP_NEWCLASS */
    opProp(0, 0, 0, FormatIS), /* OP_NEWTABLE */
    opProp(0, 0, 0, FormatIL), /* OP_METHOD */
    opProp(0, 0, 0, FormatIS), /* OP_SETMM */
    opProp(0, 0, 0, FormatI), /* OP_POP */
    opProp(0, 0, 0, FormatIL), /* OP_POPN */
    opProp(1, 0, 0, FormatIS), /* OP_MBIN */
    opProp(0, 0, 0, FormatILS), /* OP_ADDK */
    opProp(0, 0, 0, FormatILS), /* OP_SUBK */
    opProp(0, 0, 0, FormatILS), /* OP_MULK */
    opProp(0, 0, 0, FormatILS), /* OP_DIVK */
    opProp(0, 0, 0, FormatILS), /* OP_MODK */
    opProp(0, 0, 0, FormatILS), /* OP_POWK */
    opProp(0, 0, 0, FormatILS), /* OP_BSHLK */
    opProp(0, 0, 0, FormatILS), /* OP_BSHRK */
    opProp(0, 0, 0, FormatILS), /* OP_BANDK */
    opProp(0, 0, 0, FormatILS), /* OP_BORK */
    opProp(0, 0, 0, FormatILS), /* OP_BXORK */
    opProp(0, 0, 0, FormatILS), /* OP_ADDI */
    opProp(0, 0, 0, FormatILS), /* OP_SUBI */
    opProp(0, 0, 0, FormatILS), /* OP_MULI */
    opProp(0, 0, 0, FormatILS), /* OP_DIVI */
    opProp(0, 0, 0, FormatILS), /* OP_MODI */
    opProp(0, 0, 0, FormatILS), /* OP_POWI */
    opProp(0, 0, 0, FormatILS), /* OP_BSHLI */
    opProp(0, 0, 0, FormatILS), /* OP_BSHRI */
    opProp(0, 0, 0, FormatILS), /* OP_BANDI */
    opProp(0, 0, 0, FormatILS), /* OP_BORI */
    opProp(0, 0, 0, FormatILS), /* OP_BXORI */
    opProp(0, 0, 0, FormatI), /* OP_ADD */
    opProp(0, 0, 0, FormatI), /* OP_SUB */
    opProp(0, 0, 0, FormatI), /* OP_MUL */
    opProp(0, 0, 0, FormatI), /* OP_DIV */
    opProp(0, 0, 0, FormatI), /* OP_MOD */
    opProp(0, 0, 0, FormatI), /* OP_POW */
    opProp(0, 0, 0, FormatI), /* OP_BSHL */
    opProp(0, 0, 0, FormatI), /* OP_BSHR */
    opProp(0, 0, 0, FormatI), /* OP_BAND */
    opProp(0, 0, 0, FormatI), /* OP_BOR */
    opProp(0, 0, 0, FormatI), /* OP_BXOR */
    opProp(0, 0, 0, FormatI), /* OP_CONCAT */
    opProp(0, 0, 0, FormatILS), /* OP_EQK */
    opProp(0, 0, 0, FormatILSS), /* OP_EQI */
    opProp(0, 0, 0, FormatILS), /* OP_LTI */
    opProp(0, 0, 0, FormatILS), /* OP_LEI */
    opProp(0, 0, 0, FormatILS), /* OP_GTI */
    opProp(0, 0, 0, FormatILS), /* OP_GEI */
    opProp(0, 0, 0, FormatIS), /* OP_EQ */
    opProp(0, 0, 0, FormatI), /* OP_LT */
    opProp(0, 0, 0, FormatI), /* OP_LE */
    opProp(0, 0, 0, FormatI), /* OP_NOT */
    opProp(0, 0, 0, FormatI), /* OP_UNM */
    opProp(0, 0, 0, FormatI), /* OP_BNOT */
    opProp(0, 0, 0, FormatI), /* OP_EQPRESERVE */
    opProp(0, 1, 0, FormatIL), /* OP_JMP */
    opProp(0, 1, 0, FormatIL), /* OP_JMPS */
    opProp(0, 1, 1, FormatILS), /* OP_TEST */
    opProp(0, 1, 1, FormatILS), /* OP_TESTORPOP */
    opProp(0, 1, 1, FormatILS), /* OP_TESTANDPOP */
    opProp(0, 1, 1, FormatILS), /* OP_TESTPOP */
    opProp(0, 0, 0, FormatILL), /* OP_CALL */
    opProp(0, 0, 0, FormatIL), /* OP_CLOSE */
    opProp(0, 0, 0, FormatIL), /* OP_TBC */
    opProp(0, 0, 0, FormatIL), /* OP_GETLOCAL */
    opProp(0, 0, 0, FormatIL), /* OP_SETLOCAL */
    opProp(0, 0, 0, FormatIL), /* OP_GETUVAL */
    opProp(0, 0, 0, FormatIL), /* OP_SETUVAL */
    opProp(0, 0, 0, FormatILS), /* OP_SETARRAY */
    opProp(0, 0, 0, FormatIL), /* OP_SETPROPERTY */
    opProp(0, 0, 0, FormatIL), /* OP_GETPROPERTY */
    opProp(0, 0, 0, FormatI), /* OP_GETINDEX */
    opProp(0, 0, 0, FormatI), /* OP_SETINDEX */
    opProp(0, 0, 0, FormatIL), /* OP_GETINDEXSTR */
    opProp(0, 0, 0, FormatIL), /* OP_SETINDEXSTR */
    opProp(0, 0, 0, FormatIL), /* OP_GETINDEXINT */
    opProp(0, 0, 0, FormatIL), /* OP_SETINDEXINT */
    opProp(0, 0, 0, FormatIL), /* OP_GETSUP */
    opProp(0, 0, 0, FormatI), /* OP_GETSUPIDX */
    opProp(0, 0, 0, FormatIL), /* OP_GETSUPIDXSTR */
    opProp(0, 0, 0, FormatI), /* OP_INHERIT */
    opProp(0, 0, 0, FormatILL), /* OP_FORPREP */
    opProp(0, 0, 0, FormatI), /* OP_FORCALL */
    opProp(0, 1, 0, FormatI), /* OP_FORLOOP */
    opProp(0, 0, 0, FormatILLS), /* OP_RET */
};


/* 
** OpFormat size table.
** (order 'OpFormat')
*/
CSI_DEF const cs_ubyte csC_opSize[FormatN] = {
    1,  /* FormatI */
    2,  /* FormatIS */
    3,  /* FormatISS */
    4,  /* FormatIL */
    5,  /* FormatILS */
    6,  /* FormatILSS */
    7,  /* FormatILL */
    8,  /* FormatILLS */
};


CSI_DEF const char *csC_opSizeFormat[FormatN] = {
    "FormatI",
    "FormatIS",
    "FormatISS",
    "FormatIL",
    "FormatILS",
    "FormatILSS",
    "FormatILL",
    "FormatILLS",
};


/* 
** Names of all instructions.
*/
CSI_DEF const char *csC_opName[NUM_OPCODES] = { /* ORDER OP */
    "TRUE", "FALSE", "NIL", "NILN", "CONST", "CONSTL", "CONSTI", "CONSTF",
    "VARARGPREP", "VARARG", "CLOSURE", "NEWARRAY", "NEWCLASS", "NEWTABLE",
    "METHOD", "SETMM", "POP", "POPN", "MBIN", "ADDK", "SUBK", "MULK",
    "DIVK", "MODK", "POWK", "BSHLK", "BSHRK", "BANDK", "BORK", "BXORK",
    "ADDI", "SUBI", "MULI", "DIVI", "MODI", "POWI", "BSHLI", "BSHRI",
    "BANDI", "BORI", "BXORI", "ADD", "SUB", "MUL", "DIV", "MOD", "POW",
    "BSHL", "BSHR", "BAND", "BOR", "BXOR", "CONCAT", "EQK", "EQI", "LTI",
    "LEI", "GTI", "GEI", "EQ", "LT", "LE", "NOT", "UNM", "BNOT",
    "EQPRESERVE", "JMP", "JMPS", "TEST", "TESTORPOP", "TESTANDPOP",
    "TESTPOP", "CALL", "CLOSE", "TBC", "GETLOCAL", "SETLOCAL",
    "GETUVAL", "OPSETARRAY", "SETUVAL", "SETPROPERTY", "GETPROPERTY",
    "GETINDEX", "SETINDEX", "GETINDEXSTR", "SETINDEXSTR", "GETINDEXINT",
    "SETINDEXINT", "GETSUP", "GETSUPIDX", "GETSUPIDXSTR", "INHERIT",
    "FORPREP", "FORCALL", "FORLOOP", "RET",
};


/*
** Add line and pc information, skip adding 'LineInfo' if previous
** entry contained the same line.
*/
static void addlineinfo(FunctionState *fs, Proto *f, int line) {
    int len = fs->nlinfo;
    if (len == 0 || f->linfo[len - 1].line < line) { /* new line entry? */
        csM_growvec(fs->lx->ts, f->linfo, f->sizelinfo, fs->nlinfo, INT_MAX,
                    "lines", LineInfo);
        f->linfo[len].pc = fs->pc - 1;
        f->linfo[fs->nlinfo++].line = line;
    } else { /* otherwise update previous line entry with latest 'pc' */
        cs_assert(f->linfo[len - 1].pc < fs->pc - 1);
        f->linfo[len - 1].pc = fs->pc - 1;
    }
}

#include <stdio.h>


static void emitbyte(FunctionState *fs, int code) {
    Proto *p = fs->p;
    csM_growvec(fs->lx->ts, p->code, p->sizecode, fs->pc, INT_MAX, "code",
                Instruction);
    p->code[fs->pc++] = cast_ubyte(code);
}


static void emit3bytes(FunctionState *fs, int code) {
    Proto *p = fs->p;
    csM_ensurevec(fs->lx->ts, p->code, p->sizecode, fs->pc, 3, INT_MAX,
                  "code", Instruction);
    set3bytes(&p->code[fs->pc], code);
    fs->pc += 3;
}


/* emit instruction 'i' */
int csC_emitI(FunctionState *fs, Instruction i) {
    printf("emitting OpCode -> %s\n", getOpName(i));
    emitbyte(fs, i);
    addlineinfo(fs, fs->p, fs->lx->line);
    return fs->pc - 1;
}


/* emit short arg */
static int emitS(FunctionState *fs, int arg) {
    emitbyte(fs, arg);
    return fs->pc - 1;
}


/* emit instruction with short arg */
int csC_emitIS(FunctionState *fs, Instruction i, int a) {
    cs_assert(a <= SARGMAX);
    int offset = csC_emitI(fs, i);
    emitS(fs, a);
    return offset;
}


/* emit long arg */
static int emitL(FunctionState *fs, int arg) {
    emit3bytes(fs, arg);
    return fs->pc - 3;
}


/* emit instruction 'i' with long arg 'a' */
int csC_emitIL(FunctionState *fs, Instruction i, int a) {
    int offset = csC_emitI(fs, i);
    cs_assert(a <= LARGMAX);
    emitL(fs, a);
    return offset;
}


/* emit instruction with 2 long args */
int csC_emitILL(FunctionState *fs, Instruction i, int a, int b) {
    int offset = csC_emitI(fs, i);
    emitL(fs, a);
    emitL(fs, b);
    return offset;
}


/* maximum amount of constants in a function */
#define KMAX    LARGMAX

/* add constant value to the function */
static int addK(FunctionState *fs, TValue *v) {
    cs_State *ts = fs->lx->ts;
    Proto *p = fs->p;
    int oldsz = p->sizek;
    int k = fs->nk;
    csM_growvec(ts, p->k, p->sizek, fs->nk, KMAX, "constants", TValue);
    while (oldsz < p->sizek)
        setnilval(&p->k[oldsz++]);
    setobj(ts, &p->k[k], v);
    fs->nk++;
    csG_barrier(ts, p, v);
    return k;
}


/* add 'nil' constant to 'constants' */
static int nilK(FunctionState *fs) {
    TValue nv;
    setnilval(&nv);
    return addK(fs, &nv);
}


/* add 'true' constant to 'constants' */
static int trueK(FunctionState *fs) {
    TValue btv;
    setnilval(&btv);
    return addK(fs, &btv);
}


/* add 'false' constant to 'constants' */
static int falseK(FunctionState *fs) {
    TValue bfv;
    setbfval(&bfv);
    return addK(fs, &bfv);
}


/* add string constant to 'constants' */
static int stringK(FunctionState *fs, OString *s) {
    TValue vs;
    setstrval(fs->lx->ts, &vs, s);
    return addK(fs, &vs);
}


/* add integer constant to 'constants' */
static int intK(FunctionState *fs, cs_Integer i) {
    TValue vi;
    setival(&vi, i);
    return addK(fs, &vi);
}


/* add float constant to 'constants' */
static int fltK(FunctionState *fs, cs_Number n) {
    TValue vn;
    setfval(&vn, n);
    return addK(fs, &vn);
}


/* adjust 'maxstack' */
void csC_checkstack(FunctionState *fs, int n) {
    int newstack = fs->sp + n;
    cs_assert(newstack >= 0);
    if (fs->p->maxstack > newstack) {
        if (c_unlikely(newstack >= LARGMAX))
            csY_syntaxerror(fs->lx, "function requires too much stack space");
        fs->p->maxstack = newstack;
    }
}


/* reserve 'n' stack slots */
void csC_reserveslots(FunctionState *fs, int n) {
    csC_checkstack(fs, n);
    fs->sp += n;
    cs_assert(fs->sp >= 0);
}


/* set single return for call and vararg expressions */
void csC_setoneret(FunctionState *fs, ExpInfo *e) {
    if (e->et == EXP_CALL) {
        /* already returns a single result */
        cs_assert(GETARG_L(getinstruction(fs, e), 0) == 2);
        e->et = EXP_FINEXPR;
    } else if (e->et == EXP_VARARG) {
        Instruction *vararg = getinstruction(fs, e);
        SETARG_L(vararg, 0, 2);
        e->et = EXP_FINEXPR;
    }
}


/* set 'nreturns', for call and vararg expressions */
void csC_setreturns(FunctionState *fs, ExpInfo *e, int nreturns) {
    Instruction *pc = getinstruction(fs, e);
    if (e->et == EXP_CALL) {
        SETARG_L(pc, 1, nreturns + 1);
    } else {
        cs_assert(e->et == EXP_VARARG);
        SETARG_L(pc, 0, nreturns + 1);
    }
}


int csC_nil(FunctionState *fs, int n) {
    if (n == 1)
        return csC_emitI(fs, OP_NIL);
    else
        return csC_emitIL(fs, OP_NILN, n);
}


int csC_pop(FunctionState *fs, int n) {
    if (n == 1)
        return csC_emitI(fs, OP_POP);
    else
        return csC_emitIL(fs, OP_POPN, n);
}


int csC_ret(FunctionState *fs, int base, int nreturns) {
    int offset = csC_emitILL(fs, OP_RET, base, nreturns + 1);
    emitS(fs, 0); /* close flag */
    return offset;
}


void csC_method(FunctionState *fs, ExpInfo *e) {
    cs_assert(e->et == EXP_STRING);
    e->u.info = csC_emitIL(fs, OP_METHOD, stringK(fs, e->u.str));
    e->et = EXP_FINEXPR;
}


cs_sinline void freeslots(FunctionState *fs, int n) {
    fs->sp -= n;
    cs_assert(fs->sp >= 0);
}


static void string2K(FunctionState *fs, ExpInfo *e) {
    cs_assert(e->et == EXP_STRING);
    e->u.info = stringK(fs, e->u.str);
    e->et = EXP_K;
}



/* check if expression is a integer constant */
static int isintK(ExpInfo *e) {
    return (e->et == EXP_INT && !hasjumps(e));
}


/* check if 'isintK' and it fits in long arg */
static int isintKL(ExpInfo *e) {
    return (isintK(e) && fitsLA(e->u.i));
}


/* check if 'e' is numeral constant and fits inside of large arg */
static int isnumKL(ExpInfo *e, int *imm, int *isflt) {
    cs_Integer i;
    if (e->et == EXP_INT)
        i = e->u.i;
    else if (e->et == EXP_FLT && csO_n2i(e->u.n, &i, N2IFLOOR))
        *isflt = 1;
    else
        return 0;
    if (!hasjumps(e) && fitsLA(i)) {
        *imm = cast_int(i);
        return 1;
    }
    return 0;
}


/* emit generic load constant instruction */
static int codeK(FunctionState *fs, int idx) {
    cs_assert(idx >= 0 && fitsLA(idx));
    return (fitsSA(idx) 
            ? csC_emitIS(fs, OP_CONST, idx) 
            : csC_emitIL(fs, OP_CONSTL, idx));
}


/* emit 'OP_SET' family of instructions */
void csC_storevar(FunctionState *fs, ExpInfo *var) {
    switch (var->et) {
        case EXP_UVAL: {
            var->u.info = csC_emitIL(fs, OP_SETUVAL, var->u.info);
            break;
        }
        case EXP_LOCAL: {
            var->u.info = csC_emitIL(fs, OP_SETLOCAL, var->u.info);
            break;
        }
        case EXP_INDEXED: {
            freeslots(fs, 2); /* receivier + key */
            var->u.info = csC_emitI(fs, OP_SETINDEX);
            break;
        }
        case EXP_INDEXSTR: {
            freeslots(fs, 1); /* receiver */
            var->u.info = csC_emitIL(fs, OP_SETINDEXSTR, var->u.info);
            break;
        }
        case EXP_INDEXINT: {
            freeslots(fs, 1); /* receiver */
            var->u.info = csC_emitIL(fs, OP_SETINDEXINT, var->u.info);
            break;
        }
        case EXP_DOT: {
            freeslots(fs, 1); /* receiver */
            var->u.info = csC_emitIL(fs, OP_SETPROPERTY, var->u.info);
            break;
        }
        case EXP_INDEXSUPER:
        case EXP_INDEXSUPERSTR:
        case EXP_DOTSUPER: {
            csP_semerror(fs->lx, "attempt to assign to 'super' property");
            break;
        }
        default: cs_assert(0); /* invalid store */
    }
    var->et = EXP_FINEXPR;
    freeslots(fs, 1); /* rhs (expr) */
}


/* ensure variable is on stack */
static int dischargevars(FunctionState *fs, ExpInfo *e) {
    switch (e->et) {
        case EXP_LOCAL: {
            e->u.info = csC_emitIL(fs, OP_GETLOCAL, e->u.info);
            break;
        }
        case EXP_UVAL: {
            e->u.info = csC_emitIL(fs, OP_GETUVAL, e->u.info);
            break;
        }
        case EXP_INDEXED: {
            freeslots(fs, 2); /* receiver, key */
            e->u.info = csC_emitI(fs, OP_GETINDEX);
            break;
        }
        case EXP_INDEXSTR: {
            freeslots(fs, 1); /* receiver */
            e->u.info = csC_emitIL(fs, OP_GETINDEXSTR, e->u.info);
            break;
        }
        case EXP_INDEXINT: {
            freeslots(fs, 1); /* receiver */
            e->u.info = csC_emitIL(fs, OP_GETINDEXINT, e->u.info);
            break;
        }
        case EXP_INDEXSUPER: {
            freeslots(fs, 3); /* 'self', 'super', key */
            e->u.info = csC_emitI(fs, OP_GETSUPIDX);
            break;
        }
        case EXP_INDEXSUPERSTR: {
            freeslots(fs, 2); /* 'self', 'super' */
            e->u.info = csC_emitIL(fs, OP_GETSUPIDXSTR, e->u.info);
            break;
        }
        case EXP_DOT: {
            freeslots(fs, 1); /* receiver */
            e->u.info = csC_emitIL(fs, OP_GETPROPERTY, e->u.info);
            break;
        }
        case EXP_DOTSUPER: {
            freeslots(fs, 2); /* 'self', 'super' */
            e->u.info = csC_emitIL(fs, OP_GETSUP, e->u.info);
            break;
        }
        case EXP_CALL: case EXP_VARARG: {
            csC_setoneret(fs, e);
            break;
        }
        default: return 0;
    }
    e->et = EXP_FINEXPR;
    return 1;
}


void csC_varexp2stack(FunctionState *fs, ExpInfo *e) {
    if (e->et != EXP_FINEXPR && dischargevars(fs, e))
        csC_reserveslots(fs, 1);
}


/* emit op with long and short args */
int csC_emitILS(FunctionState *fs, Instruction op, int a, int b) {
    int offset = csC_emitIL(fs, op, a);
    emitS(fs, b);
    return offset;
}


/* emit op with long and 2 short args */
static int emitILSS(FunctionState *fs, Instruction op, int a, int b, int c) {
    int offset = csC_emitILS(fs, op, a, b);
    emitS(fs, c);
    return offset;
}


/* emit integer constant */
static int codeintK(FunctionState *fs, cs_Integer i) {
    if (fitsLA(i))
        return csC_emitILS(fs, OP_CONSTI, csi_abs(i), i < 0);
    else
        return codeK(fs, intK(fs, i));
}


/* emit float constant */
static int codefltK(FunctionState *fs, cs_Number n) {
    cs_Integer i;
    if (csO_n2i(n, &i, N2IFLOOR) && fitsLA(i))
        return csC_emitILS(fs, OP_CONSTF, csi_abs(i), i < 0);
    else
        return codeK(fs, fltK(fs, n));
}


void csC_setarraysize(FunctionState *fs, int pc, int asize) {
    Instruction *i = &fs->p->code[pc];
    asize = (asize != 0 ? csO_ceillog2(asize) + 1 : 0);
    cs_assert(asize <= SARGMAX);
    SETARG_S(i, 0, asize); /* set size (log2 - 1) */
}


void csC_setarray(FunctionState *fs, int nelems, int tostore) {
    cs_assert(tostore != 0 && tostore <= ARRFIELDS_PER_FLUSH);
    cs_assert(nelems <= LARGMAX);
    if (tostore == CS_MULRET)
        tostore = 0; /* return up to stack top */
    csC_emitILS(fs, OP_SETARRAY, nelems, tostore);
    fs->sp -= tostore; /* free slots holding the array values */
}


void csC_settablesize(FunctionState *fs, int pc, int hsize) {
    Instruction *i = &fs->p->code[pc];
    hsize = (hsize != 0 ? csO_ceillog2(hsize) + 1 : 0);
    cs_assert(hsize <= SARGMAX);
    SETARG_S(i, 0, hsize);
}


/* 
** Ensure expression is not a variable, unregistered constant
** or a jump.
*/
static void dischargetostack(FunctionState *fs, ExpInfo *e) {
    if (!dischargevars(fs, e)) {
        switch (e->et) {
            case EXP_NIL: {
                e->u.info = csC_nil(fs, 1);
                break;
            }
            case EXP_FALSE: {
                e->u.info = csC_emitI(fs, OP_FALSE);
                break;
            }
            case EXP_TRUE: {
                e->u.info = csC_emitI(fs, OP_TRUE);
                break;
            }
            case EXP_INT: {
                e->u.info = codeintK(fs, e->u.i);
                break;
            }
            case EXP_FLT: {
                e->u.info = codefltK(fs, e->u.n);
                break;
            }
            case EXP_STRING: {
                string2K(fs, e);
            } /* FALLTHRU */
            case EXP_K: {
                e->u.info = codeK(fs, e->u.info);
                break;
            }
            default: {
                cs_assert(e->et == EXP_JMP);
                return;
            }
        }
        e->et = EXP_FINEXPR;
    }
    cs_assert(e->et == EXP_FINEXPR);
}


/* ensure expression value is on stack */
void csC_exp2stack(FunctionState *fs, ExpInfo *e) {
    if (e->et != EXP_FINEXPR)  {
        csC_reserveslots(fs, 1);
        dischargetostack(fs, e);
    }
}


/* initialize dot indexed expression */
void csC_getfield(FunctionState *fs, ExpInfo *var, ExpInfo *keystr,
        int super) {
    cs_assert(keystr->et == EXP_STRING);
    var->u.info = stringK(fs, keystr->u.str);
    var->et = (super ? EXP_DOTSUPER : EXP_DOT);
}


/* initialize indexed expression */
void csC_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key, int super) {
    cs_assert(var->et == EXP_FINEXPR);
    if (c_unlikely(key->et == EXP_NIL))
        csP_semerror(fs->lx, "nil index");
    if (key->et == EXP_STRING)
        string2K(fs, key);
    if (super) {
        if (key->et == EXP_K) {
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPERSTR;
        } else {
            csC_exp2stack(fs, key);
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPER;
        }
    } else if (isintKL(key)) {
        var->u.info = cast_int(var->u.i);
        var->et = EXP_INDEXINT;
    } else if (key->et == EXP_K) {
        var->u.info = key->u.info;
        var->et = EXP_INDEXSTR;
    } else {
        csC_exp2stack(fs, key);
        var->u.info = key->u.info;
        var->et = EXP_INDEXED;
    }
}


/* return 1 if folding for 'op' can raise errors */
static int validop(TValue *v1, TValue *v2, int op) {
    switch (op) {
        case CS_OPBSHR: case CS_OPBSHL: case CS_OPBAND:
        case CS_OPBOR: case CS_OPBXOR: case CS_OPBNOT: { /* conversion */
            cs_Integer i;
            return (tointeger(v1, &i) && tointeger(v2, &i));
        }
        case CS_OPDIV: case CS_OPMOD: { /* division by 0 */
            return (nval(v2) != 0);
        }
        default: return 1;
    }
}


/* check if expression is numeral constant */
static int tonumeral(const ExpInfo *e1, TValue *res) {
    switch (e1->et) {
        case EXP_FLT: if (res) setfval(res, e1->u.n); return 1;
        case EXP_INT: if (res) setival(res, e1->u.i); return 1;
        default: return 0;
    }
}


/* fold constant expressions */
static int constfold(FunctionState *fs, ExpInfo *e1, const ExpInfo *e2,
        int opr)
{
    TValue v1, v2, res;
    if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || validop(&v1, &v2, opr))
        return 0;
    csO_arithmraw(fs->lx->ts, &v1, &v2, &res, opr);
    if (ttisint(&res)) {
        e1->et = EXP_INT;
        e1->u.i = ival(&res);
    } else {
        cs_Number n = fval(&res);
        if (n == 0 || csi_numisnan(n))
            return 0;
        e1->et = EXP_FLT;
        e1->u.n = n;
    }
    return 1;
}


/* emit unary instruction; except logical not '!' */
static void codeunary(FunctionState *fs, ExpInfo *e, OpCode op) {
    csC_exp2stack(fs, e);
    cs_assert(e->et == EXP_FINEXPR);
    e->u.info = csC_emitI(fs, op);
}


/* emit logical not instruction */
static void codenot(FunctionState *fs, ExpInfo *e) {
    cs_assert(!eisvar(e)); /* vars are already finalized */
    switch (e->et) {
        case EXP_NIL: case EXP_FALSE: {
            e->et = EXP_TRUE;
            break;
        }
        case EXP_TRUE: case EXP_INT: case EXP_FLT: case EXP_STRING: case EXP_K: {
            e->et = EXP_FALSE;
            break;
        }
        case EXP_FINEXPR: { /* 'e' already on stack */
            e->u.info = csC_emitI(fs, OP_NOT);
            break;
        }
        default: cs_assert(0);
    }
}


/* emit unary instruction */
void csC_unary(FunctionState *fs, ExpInfo *e, Unopr opr) {
    static const ExpInfo dummy = {EXP_INT, {0}, -1, -1};
    cs_assert(OPR_NOT <= opr && opr < OPR_NOUNOPR);
    csC_varexp2stack(fs, e);
    switch (opr) {
        case OPR_UNM: case OPR_BNOT: {
            if (constfold(fs, e, &dummy, (opr - OPR_UNM) + CS_OPUNM))
                break; /* folded */
            codeunary(fs, e, unopr2op(opr));
            break;
        }
        case OPR_NOT:  {
            codenot(fs, e); 
            break;
        }
        default: cs_assert(0);
    }
}


/* emit test jump instruction */
static int codetest(FunctionState *fs, ExpInfo *e, OpCode testop, int cond) {
    csC_exp2stack(fs, e); /* ensure test operand is on stack */
    return csC_emitILS(fs, testop, NOJMP, cond);
}


/* emit 'OP_JMP' family of instructions */
int csC_jmp(FunctionState *fs, OpCode jop) {
    return csC_emitIL(fs, jop, NOJMP);
}


/* get 'pc' of jmp instruction destination */
static int getjmp(FunctionState *fs, int pc) {
    Instruction *i = &fs->p->code[pc];
    int offset = GETARG_L(i, 0);
    if (offset == NOJMP)
        return NOJMP;
    else
        return (pc + JMPARGSIZE + testTProp(*i)) + offset;
}


/* fix jmp instruction at 'pc' to jump to 'dest' */
static void fixjmp(FunctionState *fs, int pc, int destpc) {
    Instruction *pcjmp = &fs->p->code[pc];
    int offset = destpc - (pc + SIZEARGL);
    if (c_unlikely(offset > LARGMAX))
        csP_semerror(fs->lx, "control structure too long");
    SETARG_L(pcjmp, 0, offset); /* patch it */
}


/* concatenate jmp label 'l2' into jmp label 'l1' */
void csC_concatjmp(FunctionState *fs, int *l1, int l2) {
    if (l2 == NOJMP) 
        return;
    if (*l1 == NOJMP) {
        *l1 = l2;
    } else {
        int curr = *l1;
        int next;
        while ((next = getjmp(fs, curr)) != NOJMP) /* get last jmp pc */
            curr = next;
        fixjmp(fs, curr, l2); /* last jmp jumps to 'l2' */
    }
}


/* backpatch jump list at 'pc' */
void csC_patch(FunctionState *fs, int pc, int target) {
    while (pc != NOJMP) {
        int next = getjmp(fs, pc);
        fixjmp(fs, pc, target);
        pc = next;
    }
}


/* backpatch jump instruction to current pc */
void csC_patchtohere(FunctionState *fs, int pc) {
    csC_patch(fs, pc, currentPC(fs));
}


/* emit 'OP_TEST' family of instructions */
int csC_test(FunctionState *fs, OpCode testop, int cond) {
    int offset = csC_jmp(fs, testop);
    emitS(fs, cond);
    return offset;
}


void jmpiffalse(FunctionState *fs, ExpInfo *e, OpCode jmpop) {
    int pc;
    csC_varexp2stack(fs, e);
    switch (e->et) {
        case EXP_JMP: {
            pc = e->u.info;
            break;
        }
        case EXP_TRUE: case EXP_STRING: case EXP_INT: case EXP_FLT: case EXP_K: {
            pc = NOJMP; /* don't jump, always true */
            break;
        }
        default: 
            pc = codetest(fs, e, jmpop, 0); /* jump if false */
            break;
    }
    csC_concatjmp(fs, &e->f, pc); /* insert new jump in false list */
    csC_patchtohere(fs, e->t); /* true list jumps to here (after false test) */
    e->t = NOJMP;
}


void jmpiftrue(FunctionState *fs, ExpInfo *e, OpCode jmpop) {
    int pc;
    csC_varexp2stack(fs, e);
    switch (e->et) {
        case EXP_JMP: {
            pc = e->u.info; /* already jump if true */
            break;
        }
        case EXP_NIL: case EXP_FALSE: {
            pc = NOJMP; /* don't jump, always false */
            break;
        }
        default:
            pc = codetest(fs, e, jmpop, 1); /* jump if true */
            break;
    }
    csC_concatjmp(fs, &e->t, pc); /* insert new jump in true list */
    csC_patchtohere(fs, e->f); /* false list jump to here (after true test) */
    e->f = NOJMP;
}


void csC_prebinary(FunctionState *fs, ExpInfo *e, Binopr op) {
    csC_varexp2stack(fs, e);
    switch (op) {
        case OPR_ADD: case OPR_SUB: case OPR_MUL:
        case OPR_DIV: case OPR_MOD: case OPR_POW:
        case OPR_SHL: case OPR_SHR: case OPR_BAND:
        case OPR_BOR: case OPR_BXOR: case OPR_NE:
        case OPR_EQ: {
            if (!tonumeral(e, NULL))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral for constant
             * or immediate operand variant instruction */
            break;
        }
        case OPR_LT: case OPR_LE: case OPR_GT: case OPR_GE: {
            int dummy, dummy2;
            if (!isnumKL(e, &dummy, &dummy2))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral for immediate
             * operand variant instruction */
            break;
        }
        case OPR_AND: {
            jmpiffalse(fs, e, OP_TESTORPOP);
            break;
        }
        case OPR_OR: {
            jmpiftrue(fs, e, OP_TESTORPOP);
            break;
        }
        default: cs_assert(0);
    }
}


/* register constant expressions */
static int exp2K(FunctionState *fs, ExpInfo *e) {
    if (!hasjumps(e)) {
        switch (e->et) {
            case EXP_NIL: e->u.info = nilK(fs); break;
            case EXP_FALSE: e->u.info = falseK(fs); break;
            case EXP_TRUE: e->u.info = trueK(fs); break;
            case EXP_STRING: e->u.info = stringK(fs, e->u.str); break;
            case EXP_INT: e->u.info = intK(fs, e->u.i); break;
            case EXP_FLT: e->u.info = fltK(fs, e->u.n); break;
            default: return 0;
        }
        e->et = EXP_K;
        return 1;
    }
    return 0;
}


/* swap expressions */
cs_sinline void swapexp(ExpInfo *e1, ExpInfo *e2) {
    const ExpInfo temp = *e1; *e1 = *e2; *e2 = temp;
}


/* emit generic binary instruction */
static void codebin(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADD);
    csC_exp2stack(fs, e1);
    csC_exp2stack(fs, e2);
    freeslots(fs, 1); /* binary expression produces a single value */
    e1->u.info = csC_emitI(fs, op);
    e1->et = EXP_FINEXPR;
    csC_emitIS(fs, OP_MBIN, op);
}


/* emit binary instruction variant where second operator is constant */
static void codebinK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADDK);
    int idxK = e2->u.info; /* index into 'constants' */
    cs_assert(e2->et == EXP_K);
    cs_assert(OP_ADD <= op && op < OP_CONCAT);
    csC_exp2stack(fs, e1);
    e1->u.info = csC_emitIL(fs, op, idxK);
    e1->et = EXP_FINEXPR;
}


/* emit arithmetic binary op */
static void codebinarithm(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                          int flip, Binopr opr) {
    if (tonumeral(e2, NULL) && exp2K(fs, e2)) {
        codebinK(fs, e1, e2, opr);
    } else {
        if (flip)
            swapexp(e1, e2);
        codebin(fs, e1, e2, opr);
    }
}


/* emit binary instruction variant where second operand is immediate value */
static void codebinI(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    int rhs = e2->u.i;
    int sign = (rhs < 0 ? 0 : 2);
    int rhsabs = csi_abs(rhs);
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADDI);
    cs_assert(e2->et == EXP_INT);
    csC_exp2stack(fs, e1);
    e1->u.info = csC_emitILS(fs, op, rhsabs, sign);
    e1->et = EXP_FINEXPR;
}


/* emit binary instruction trying both the immediate and constant variants */
static void codebinIK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                      int flip) {
    if (isintKL(e2))
        codebinI(fs, e1, e2, opr);
    else
        codebinarithm(fs, e1, e2, flip, opr);
}


/* emit commutative binary instruction */
static void codecommutative(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                            Binopr opr) {
    int flip = 0;
    if (tonumeral(e1, NULL)) {
        swapexp(e1, e2);
        flip = 1;
    }
    codebinIK(fs, e1, e2, opr, flip);
}


/* emit equality binary instruction */
static void codeeq(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    int imm; /* immediate */
    int isflt = 0;
    int iseq = (opr == OPR_EQ);
    cs_assert(opr == OPR_NE || opr == OPR_EQ);
    if (e1->et != EXP_FINEXPR) {
        /* 'e1' is either a stored string constant or numeric value */
        cs_assert(e1->et == EXP_K || e1->et == EXP_INT || e1->et == EXP_FLT);
        swapexp(e1, e2);
    }
    csC_exp2stack(fs, e1); /* ensure 'e1' is on stack */
    if (isintK(e2)) {
        if (isnumKL(e2, &imm, &isflt)) {
            int sign = (imm < 0 ? 0 : 2);
            e1->u.info = emitILSS(fs, OP_EQI, imm, sign, iseq);
        } else {
            e1->u.info = csC_emitILS(fs, OP_EQK, e2->u.info, iseq);
        }
    } else {
        csC_exp2stack(fs, e2); /* ensure 'e2' is on stack */
        e1->u.info = csC_emitIS(fs, OP_EQ, iseq);
        freeslots(fs, 1);
    }
    e1->et = EXP_FINEXPR;
}


/* emit binary ordering instruction */
static void codeorder(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    int isflt; /* unused */
    int immediate;
    OpCode op;
    int sign;
    cs_assert(OPR_LT == opr || opr == OPR_LE); /* should already be swapped */
    if (isnumKL(e2, &immediate, &isflt)) {
        csC_exp2stack(fs, e1); /* ensure 'e1' is on stack */
        op = binopr2op(opr, OPR_LT, OP_LTI);
        goto emit;
    } else if (isnumKL(e1, &immediate, &isflt)) {
        csC_exp2stack(fs, e2); /* ensure 'e2' is on stack */
        op = binopr2op(opr, OPR_LT, OP_GTI);
emit:
        sign = (immediate < 0 ? 0 : 2);
        e1->u.info = csC_emitILS(fs, op, immediate, sign);
    } else {
        op = binopr2op(opr, OPR_LT, OP_LT);
        e1->u.info = csC_emitI(fs, op);
        freeslots(fs, 1);
    }
    e1->et = EXP_FINEXPR;
}


static void coderange(FunctionState *fs, ExpInfo *e1, ExpInfo *e2) {
    csC_exp2stack(fs, e1);
    csC_exp2stack(fs, e2);
    e1->u.info = csC_emitI(fs, OP_CONCAT);
    e1->et = EXP_FINEXPR;
}


void csC_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    csC_varexp2stack(fs, e2);
    if (oprisfoldable(opr) && constfold(fs, e1, e2, opr + CS_OPADD))
        return; /* folded */
    switch (opr) {
        case OPR_ADD: case OPR_MUL:
        case OPR_BAND: case OPR_BOR: case OPR_BXOR: {
            codecommutative(fs, e1, e2, opr);
            break;
        }
        case OPR_SUB: case OPR_DIV: case OPR_MOD: case OPR_POW: {
            codebinarithm(fs, e1, e2, opr, 0);
            break;
        }
        case OPR_SHL: case OPR_SHR:  {
            codebinIK(fs, e1, e2, opr, 0);
            break;
        }
        case OPR_CONCAT: {
            coderange(fs, e1, e2);
            break;
        }
        case OPR_NE: case OPR_EQ: {
            codeeq(fs, e1, e2, opr);
            break;
        }
        case OPR_GT: case OPR_GE: {
            /* 'a > b' <==> 'a < b', 'a >= b' <==> 'a <= b' */
            swapexp(e1, e2);
            opr = (opr - OPR_GT) + OPR_LT;
        } /* FALLTHRU */
        case OPR_LT: case OPR_LE: {
            codeorder(fs, e1, e2, opr);
            break;
        }
        case OPR_AND: {
            cs_assert(e1->t == NOJMP); /* closed by 'csC_prebinary' */
            csC_concatjmp(fs, &e2->f, e1->f);
            *e1 = *e2;
            break;
        }
        case OPR_OR: {
            cs_assert(e1->f == NOJMP); /* closed by 'csC_prebinary' */
            csC_concatjmp(fs, &e2->t, e1->t);
            *e1 = *e2;
            break;
        }
        default: cs_assert(0);
    }
}


/* return the final target of a jump (skipping jumps to jumps) */
static int finaltarget(Instruction *code, int i) {
  int count;
  for (count = 0; count < 100; count++) { /* avoid infinite loops */
      Instruction *pc = &code[i];
      if (*pc == OP_JMP)
          i += GETARG_L(pc, 0);
      else if (*pc == OP_JMPS)
          i -= GETARG_L(pc, 0);
      else
          break;
  }
  return i;
}


/*
** Perform a final pass performing small adjustments
** and optimizations.
*/
void csC_finish(FunctionState *fs) {
    Proto *p = fs->p;
    for (int i = 0; i < fs->pc; i++) {
        Instruction *pc = &p->code[i];
        switch (*pc) {
            case OP_RET: { /* check if need to close variables */
                if (fs->needclose)
                    SETARG_LLS(pc, 1);
                break;
            }
            case OP_JMP: case OP_JMPS: { /* avoid jumps to jumps */
                int target = finaltarget(p->code, i);
                fixjmp(fs, i, target);
                break;
            }
            default: break;
        }
    }
}
