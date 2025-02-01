/*
** ccode.c
** Bytecode emiting functions
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "ccode.h"
#include "clexer.h"
#include "chashtable.h"
#include "cbits.h"
#include "cdebug.h"
#include "cvm.h"
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


/* binary operation to OpCode */
#define binopr2op(opr,x,from) \
        cast(OpCode, (cast_int(opr) - cast_int(x)) + cast_int(from))


/* binary OpCode to metamethod tag */
#define binop2mm(op) \
        cast(cs_MM, (cast_int(op) - OP_ADD) + cast_int(CS_MM_ADD))



/* check if 'i' fits in long arg */
#define fitsLA(i)       (-MAX_LARG <= (i) && (i) <= MAX_LARG)

/* check if 'i' fits in short arg */
#define fitsSA(i)       (-MAX_SARG <= (i) && (i) <= MAX_SARG)


#define encodesign(x)     ((x) < 0 ? 0 : 2)



/* 
** OpCode properties table.
** (order 'OpCode')
*/
CSI_DEF const c_byte csC_opProp[NUM_OPCODES] = {
    /*     J  F      */
    opProp(0, FormatI), /* OP_TRUE */
    opProp(0, FormatI), /* OP_FALSE */
    opProp(0, FormatI), /* OP_NIL */
    opProp(0, FormatIL), /* OP_NILN */
    opProp(0, FormatIS), /* OP_CONST */
    opProp(0, FormatIL), /* OP_CONSTL */
    opProp(0, FormatILS), /* OP_CONSTI */
    opProp(0, FormatILS), /* OP_CONSTF */
    opProp(0, FormatIL), /* OP_VARARGPREP */
    opProp(0, FormatIL), /* OP_VARARG */
    opProp(0, FormatIL), /* OP_CLOSURE */
    opProp(0, FormatIS), /* OP_NEWARRAY */
    opProp(0, FormatIS), /* OP_NEWCLASS */
    opProp(0, FormatIS), /* OP_NEWTABLE */
    opProp(0, FormatIL), /* OP_METHOD */
    opProp(0, FormatIS), /* OP_SETMM */
    opProp(0, FormatI), /* OP_POP */
    opProp(0, FormatIL), /* OP_POPN */
    opProp(0, FormatIS), /* OP_MBIN */
    opProp(0, FormatIL), /* OP_ADDK */
    opProp(0, FormatIL), /* OP_SUBK */
    opProp(0, FormatIL), /* OP_MULK */
    opProp(0, FormatIL), /* OP_DIVK */
    opProp(0, FormatIL), /* OP_MODK */
    opProp(0, FormatIL), /* OP_POWK */
    opProp(0, FormatIL), /* OP_BSHLK */
    opProp(0, FormatIL), /* OP_BSHRK */
    opProp(0, FormatIL), /* OP_BANDK */
    opProp(0, FormatIL), /* OP_BORK */
    opProp(0, FormatIL), /* OP_BXORK */
    opProp(0, FormatILS), /* OP_ADDI */
    opProp(0, FormatILS), /* OP_SUBI */
    opProp(0, FormatILS), /* OP_MULI */
    opProp(0, FormatILS), /* OP_DIVI */
    opProp(0, FormatILS), /* OP_MODI */
    opProp(0, FormatILS), /* OP_POWI */
    opProp(0, FormatILS), /* OP_BSHLI */
    opProp(0, FormatILS), /* OP_BSHRI */
    opProp(0, FormatILS), /* OP_BANDI */
    opProp(0, FormatILS), /* OP_BORI */
    opProp(0, FormatILS), /* OP_BXORI */
    opProp(0, FormatI), /* OP_ADD */
    opProp(0, FormatI), /* OP_SUB */
    opProp(0, FormatI), /* OP_MUL */
    opProp(0, FormatI), /* OP_DIV */
    opProp(0, FormatI), /* OP_MOD */
    opProp(0, FormatI), /* OP_POW */
    opProp(0, FormatI), /* OP_BSHL */
    opProp(0, FormatI), /* OP_BSHR */
    opProp(0, FormatI), /* OP_BAND */
    opProp(0, FormatI), /* OP_BOR */
    opProp(0, FormatI), /* OP_BXOR */
    opProp(0, FormatIL), /* OP_CONCAT */
    opProp(0, FormatILS), /* OP_EQK */
    opProp(0, FormatILSS), /* OP_EQI */
    opProp(0, FormatILS), /* OP_LTI */
    opProp(0, FormatILS), /* OP_LEI */
    opProp(0, FormatILS), /* OP_GTI */
    opProp(0, FormatILS), /* OP_GEI */
    opProp(0, FormatIS), /* OP_EQ */
    opProp(0, FormatI), /* OP_LT */
    opProp(0, FormatI), /* OP_LE */
    opProp(0, FormatI), /* OP_EQPRESERVE */
    opProp(0, FormatI), /* OP_NOT */
    opProp(0, FormatI), /* OP_UNM */
    opProp(0, FormatI), /* OP_BNOT */
    opProp(1, FormatIL), /* OP_JMP */
    opProp(1, FormatIL), /* OP_JMPS */
    opProp(1, FormatILL), /* OP_BJMP */
    opProp(1, FormatILS), /* OP_TEST */
    opProp(1, FormatILS), /* OP_TESTORPOP */
    opProp(1, FormatILS), /* OP_TESTANDPOP */
    opProp(1, FormatILS), /* OP_TESTPOP */
    opProp(0, FormatILL), /* OP_CALL */
    opProp(0, FormatIL), /* OP_CLOSE */
    opProp(0, FormatIL), /* OP_TBC */
    opProp(0, FormatIL), /* OP_GETGLOBAL */
    opProp(0, FormatIL), /* OP_SETGLOBAL */
    opProp(0, FormatIL), /* OP_GETLOCAL */
    opProp(0, FormatIL), /* OP_SETLOCAL */
    opProp(0, FormatIL), /* OP_GETUVAL */
    opProp(0, FormatIL), /* OP_SETUVAL */
    opProp(0, FormatILS), /* OP_SETARRAY */
    opProp(0, FormatILL), /* OP_SETPROPERTY */
    opProp(0, FormatIL), /* OP_GETPROPERTY */
    opProp(0, FormatI), /* OP_GETINDEX */
    opProp(0, FormatIL), /* OP_SETINDEX */
    opProp(0, FormatIL), /* OP_GETINDEXSTR */
    opProp(0, FormatILL), /* OP_SETINDEXSTR */
    opProp(0, FormatIL), /* OP_GETINDEXINT */
    opProp(0, FormatILL), /* OP_SETINDEXINT */
    opProp(0, FormatIL), /* OP_GETSUP */
    opProp(0, FormatI), /* OP_GETSUPIDX */
    opProp(0, FormatIL), /* OP_GETSUPIDXSTR */
    opProp(0, FormatI), /* OP_INHERIT */
    opProp(0, FormatILL), /* OP_FORPREP */
    opProp(0, FormatILL), /* OP_FORCALL */
    opProp(0, FormatILLL), /* OP_FORLOOP */
    opProp(0, FormatILLS), /* OP_RET */
};


/* 
** OpFormat size table.
** Instruction  = 1 byte
** Short Arg    = 1 byte
** Long Arg     = 3 bytes
*/
CSI_DEF const c_byte csC_opSize[FormatN] = { /* ORDER OPFMT */
    1,  /* FormatI */
    2,  /* FormatIS */
    3,  /* FormatISS */
    4,  /* FormatIL */
    5,  /* FormatILS */
    6,  /* FormatILSS */
    7,  /* FormatILL */
    8,  /* FormatILLS */
    10, /* FormatILLL */
};


CSI_DEF const char *csC_opSizeFormat[FormatN] = { /* ORDER OPFMT */
    "FormatI",
    "FormatIS",
    "FormatISS",
    "FormatIL",
    "FormatILS",
    "FormatILSS",
    "FormatILL",
    "FormatILLS",
    "FormatILLL",
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
    "LEI", "GTI", "GEI", "EQ", "LT", "LE", "EQPRESERVE", "NOT", "UNM",
    "BNOT", "JMP", "JMPS", "BJMP", "TEST", "TESTORPOP", "TESTANDPOP",
    "TESTPOP", "CALL", "CLOSE", "TBC", "GETGLOBAL", "SETGLOBAL",
    "GETLOCAL", "SETLOCAL", "GETUVAL", "SETUVAL", "SETARRAY", "SETPROPERTY",
    "GETPROPERTY", "GETINDEX", "SETINDEX", "GETINDEXSTR", "SETINDEXSTR",
    "GETINDEXINT", "SETINDEXINT", "GETSUP", "GETSUPIDX", "GETSUPIDXSTR",
    "INHERIT", "FORPREP", "FORCALL", "FORLOOP", "RET",
};


/* limit for difference between lines in relative line info. */
#define LIMLINEDIFF     0x80


/*
** Save line info for new instruction. We only store difference
** from the previous line in a singed byte array 'lineinfo' for each
** instruction. Storing only the difference makes it easier to fit this
** information in a single signed byte and save memory. In cases where the
** difference of lines is too large to fit in a 'c_sbyte', or the MAXIWTHABS
** limit is reached, we store absolute line information which is held in
** 'abslineinfo' array. When we do store absolute line info, we also
** indicate the corresponding 'lineinfo' entry with special value ABSLINEINFO,
** which tells us there is absolute line information for this instruction.
**
** Complexity of lookup in turn is something along the lines of O(n/k+k),
** where n is the number of instructions and k is a constant MAXIWTHABS.
** However this approximation does not take into consideration LIMLINEDIFF
** and assumes we do not have cases where the line difference is too high.
*/
static void savelineinfo(FunctionState *fs, Proto *p, int line) {
    int linedif = line - fs->prevline;
    int pc = fs->prevpc; /* last coded instruction */
    int opsize = getOpSize(p->code[pc]); /* size of last coded instruction */
    cs_assert(pc < currPC); /* must of emitted instruction */
    if (c_abs(linedif) >= LIMLINEDIFF || fs->iwthabs++ >= MAXIWTHABS) {
        csM_growarray(fs->lx->C, p->abslineinfo, p->sizeabslineinfo,
                      fs->nabslineinfo, MAXINT, "lines", AbsLineInfo);
        p->abslineinfo[fs->nabslineinfo].pc = pc;
        p->abslineinfo[fs->nabslineinfo++].line = line;
        linedif = ABSLINEINFO; /* signal the absolute line info entry */
        fs->iwthabs = 1; /* reset counter */
    }
    csM_ensurearray(fs->lx->C, p->lineinfo, p->sizelineinfo, pc, opsize,
                    MAXINT, "opcodes", c_sbyte);
    p->lineinfo[pc] = linedif;
    while (--opsize) /* fill func args (if any) */
        p->lineinfo[++pc] = ARGLINEINFO;
    fs->prevline = line; /* last line saved */
}


/*
** Remove line information from the last instruction.
** If line information for that instruction is absolute, set 'iwthabs'
** above its max to force the new (replacing) instruction to have
** absolute line info, too.
*/
static void removelastlineinfo(FunctionState *fs) {
    Proto *p = fs->p;
    int pc = fs->prevpc;
    if (p->lineinfo[pc] != ABSLINEINFO) { /* relative line info? */
        cs_assert(p->lineinfo[pc] >= 0); /* must have valid offset */
        fs->prevline -= p->lineinfo[pc]; /* fix last line saved */
        fs->iwthabs--; /* undo previous increment */
    } else { /* otherwise absolute line info */
        cs_assert(p->abslineinfo[fs->nabslineinfo - 1].pc == pc);
        fs->nabslineinfo--; /* remove it */
        fs->iwthabs = MAXIWTHABS + 1; /* force next line info to be absolute */
    }
}


static void removeinstpc(FunctionState *fs) {
    Proto *p = fs->p;
    int pc = check_exp(fs->ninstpc > 0, p->instpc[--fs->ninstpc]);
    currPC = pc;
    if (fs->ninstpc > 0)
        fs->prevpc = p->instpc[fs->ninstpc - 1];
    else {
        fs->prevpc = pc;
        cs_assert(currPC == 0);
    }
}


/*
** Remove the last instruction created, correcting line information
** accordingly.
*/
static void removelastinstruction(FunctionState *fs) {
    removelastlineinfo(fs);
    removeinstpc(fs);
}


/*
** Remove last instruction which must be a jump.
*/
void csC_removelastjump(FunctionState *fs) {
    cs_assert(opisjump(fs->p->code[fs->prevpc])); /* last inst. is jump */
    removelastinstruction(fs);
}


/*
** Change line information associated with current position, by removing
** previous info and adding it again with new line.
*/
void csC_fixline(FunctionState *fs, int line) {
    removelastlineinfo(fs);
    savelineinfo(fs, fs->p, line);
}


static void emitbyte(FunctionState *fs, int code) {
    Proto *p = fs->p;
    csM_growarray(fs->lx->C, p->code, p->sizecode, currPC, MAXINT, "code",
                  Instruction);
    p->code[currPC++] = cast_byte(code);
}


static void emit3bytes(FunctionState *fs, int code) {
    Proto *p = fs->p;
    csM_ensurearray(fs->lx->C, p->code, p->sizecode, currPC, 3, MAXINT,
                    "code", Instruction);
    set3bytes(&p->code[currPC], code);
    currPC += SIZEARGL;
}


static void addinstpc(FunctionState *fs) {
    Proto *p = fs->p;
    csM_growarray(fs->lx->C, p->instpc, p->sizeinstpc, fs->ninstpc, MAXINT,
                  "code", int);
    fs->prevpc = p->instpc[fs->ninstpc++] = currPC;
}


static int codeinstruction(FunctionState *fs, Instruction i) {
    addinstpc(fs);
    emitbyte(fs, i);
    savelineinfo(fs, fs->p, fs->lx->lastline);
    return currPC - 1;
}


/* code instruction 'i' */
int csC_emitI(FunctionState *fs, Instruction i) {
    cs_assert(SIZEINSTR == sizeof(Instruction));
    cs_assert(fs->prevpc <= currPC);
    return codeinstruction(fs, i);
}


/* code short arg */
static int emitS(FunctionState *fs, int arg) {
    cs_assert(SIZEARGS == sizeof(Instruction));
    cs_assert(0 <= arg && arg <= MAX_SARG);
    emitbyte(fs, arg);
    return currPC - 1;
}


/* code long arg */
static int emitL(FunctionState *fs, int arg) {
    cs_assert(SIZEARGL == (sizeof(Instruction) * 3));
    cs_assert(0 <= arg && arg <= MAX_LARG);
    emit3bytes(fs, arg);
    return currPC - 3;
}


/* code instruction with short arg */
int csC_emitIS(FunctionState *fs, Instruction i, int a) {
    int offset = csC_emitI(fs, i);
    emitS(fs, a);
    return offset;
}


/* code instruction 'i' with long arg 'a' */
int csC_emitIL(FunctionState *fs, Instruction i, int a) {
    int offset = csC_emitI(fs, i);
    emitL(fs, a);
    return offset;
}


/* code instruction with 2 long args */
int csC_emitILL(FunctionState *fs, Instruction i, int a, int b) {
    int offset = csC_emitI(fs, i);
    emitL(fs, a);
    emitL(fs, b);
    return offset;
}


/* code instruction with 3 long args */
int csC_emitILLL(FunctionState *fs, Instruction i, int a, int b, int c) {
    int offset = csC_emitI(fs, i);
    emitL(fs, a);
    emitL(fs, b);
    emitL(fs, c);
    return offset;
}


/* add constant value to the function */
static int addK(FunctionState *fs, TValue *key, TValue *v) {
    TValue val;
    cs_State *C = fs->lx->C;
    Proto *p = fs->p;
    const TValue *index = csH_get(fs->lx->tab, key); /* from scanner table */
    int k, oldsz;
    if (ttisint(index)) { /* value is integer? */
        k = cast_int(ival(index)); /* get the index... */
        /* ...and check if the value is correct */
        if (k < fs->nk && ttypetag(&p->k[k]) == ttypetag(v) &&
                          csV_raweq(&p->k[k], v))
            return k; /* reuse index */
    } /* otherwise constant not found; create a new entry */
    oldsz = p->sizek;
    k = fs->nk;
    setival(&val, k);
    csH_finishset(C, fs->lx->tab, index, key, &val);
    csM_growarray(C, p->k, p->sizek, k, MAX_LARG, "constants", TValue);
    while (oldsz < p->sizek) /* nil out the new part */
        setnilval(&p->k[oldsz++]);
    setobj(C, &p->k[k], v);
    fs->nk++;
    csG_barrier(C, p, v);
    return k; /* new index */
}


/* add 'nil' constant to 'constants' */
static int nilK(FunctionState *fs) {
    TValue nv, key;
    setnilval(&nv);
    sethtval(fs->lx->C, &key, fs->lx->tab);
    return addK(fs, &key, &nv);
}


/* add 'true' constant to 'constants' */
static int trueK(FunctionState *fs) {
    TValue btv;
    setbtval(&btv);
    return addK(fs, &btv, &btv);
}


/* add 'false' constant to 'constants' */
static int falseK(FunctionState *fs) {
    TValue bfv;
    setbfval(&bfv);
    return addK(fs, &bfv, &bfv);
}


/* add string constant to 'constants' */
static int stringK(FunctionState *fs, OString *s) {
    TValue vs;
    setstrval(fs->lx->C, &vs, s);
    return addK(fs, &vs, &vs);
}


/* add integer constant to 'constants' */
static int intK(FunctionState *fs, cs_Integer i) {
    TValue vi;
    setival(&vi, i);
    return addK(fs, &vi, &vi);
}


/*
** Add a float to list of constants and return its index. Floats
** with integral values need a different key, to avoid collision
** with actual integers. To that, we add to the number its smaller
** power-of-two fraction that is still significant in its scale.
** For doubles, that would be 1/2^52.
** (This method is not bulletproof: there may be another float
** with that value, and for floats larger than 2^53 the result is
** still an integer. At worst, this only wastes an entry with
** a duplicate.)
*/
static int fltK(FunctionState *fs, cs_Number n) {
    TValue vn;
    cs_Integer ik;
    setfval(&vn, n);
    if (!csO_n2i(n, &ik, N2IEXACT)) { /* not an integral value? */
        return addK(fs, &vn, &vn); /* use number itself as key */
    } else { /* otherwise must build an alternative key */
        /* number of mantissa bits including the leading bit (1) */
        const int nmb = cs_floatatt(MANT_DIG); 
        /* q = 1.0 * (1/2^52) */
        const cs_Number q = cs_mathop(ldexp)(cs_mathop(1.0), -nmb + 1);
        const cs_Number k = (ik == 0 ? q : n + n*q); /* new key */
        TValue kv;
        setfval(&kv, k);
        /* result is not an integral value, unless value is too large */
        cs_assert(!csO_n2i(k, &ik, N2IEXACT) ||
                   cs_mathop(fabs)(n) >= cs_mathop(1e6));
        return addK(fs, &kv, &vn);
    }
}


/* adjust 'maxstack' */
void csC_checkstack(FunctionState *fs, int n) {
    int newstack = fs->sp + n;
    cs_assert(newstack >= 0);
    if (fs->p->maxstack < newstack) {
        if (c_unlikely(newstack >= MAX_LARG))
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
        cs_assert(GETARG_L(getinstruction(fs, e), 1) == 2);
        e->et = EXP_FINEXPR; /* just mark as finalized */
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
        csC_reserveslots(fs, 1);
    }
    e->et = EXP_FINEXPR;
}


static int adjuststack(FunctionState *fs, OpCode op, int n) {
    Instruction *inst = &prevOP(fs);
    int prevn = 0;
    switch (*inst) {
        case OP_POPN: case OP_NILN: {
            prevn = GETARG_L(inst, 0);
            SETARG_L(inst, 0, n + prevn);
            return fs->prevpc; /* done; do not code new instruction */
        }
        case OP_POP: {
            op = OP_POPN;
            goto nil;
        }
        case OP_NIL: {
            op = OP_NILN;
nil:
            prevn = 1;
            removelastinstruction(fs);
            break;
        }
        default: break; /* nothing to be done */
    }
    n += prevn;
    if (n == 1) {
        cs_assert(op == OP_NIL || op == OP_POP);
        return csC_emitI(fs, op);
    } else {
        cs_assert(op == OP_NILN || op == OP_POPN);
        return csC_emitIL(fs, op, n);
    }
}


static int codenil(FunctionState *fs, int n) {
    return adjuststack(fs, n > 1 ? OP_NILN : OP_NIL, n);
}


int csC_nil(FunctionState *fs, int n) {
    cs_assert(n > 0);
    csC_reserveslots(fs, n);
    return codenil(fs, n);
}


c_sinline void freeslots(FunctionState *fs, int n) {
    fs->sp -= n;
    cs_assert(fs->sp >= 0); /* negative slots are invalid */
}


/* pop values from stack and free stack slots */
int csC_pop(FunctionState *fs, int n) {
    if (n > 0) {
        freeslots(fs, n);
        return adjuststack(fs, n > 1 ? OP_POPN : OP_POP, n);
    }
    return -1; /* nothing to pop */
}


void csC_adjuststack(FunctionState *fs, int left) {
    if (left > 0)
        csC_pop(fs, left);
    else if (left < 0)
        csC_nil(fs, -left);
    /* else stack is already adjusted */
}


int csC_ret(FunctionState *fs, int first, int nreturns) {
    int offset = csC_emitILL(fs, OP_RET, first, nreturns + 1);
    emitS(fs, 0); /* close flag */
    return offset;
}


void csC_method(FunctionState *fs, ExpInfo *e) {
    e->u.info = csC_emitIL(fs, OP_METHOD, stringK(fs, e->u.str));
    e->et = EXP_FINEXPR;
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
    else if (e->et == EXP_FLT && csO_n2i(e->u.n, &i, N2IEXACT))
        *isflt = 1;
    else
        return 0;
    if (!hasjumps(e) && fitsLA(i)) {
        *imm = cast_int(i);
        return 1;
    }
    return 0;
}


/* code generic load constant instruction */
static int codeK(FunctionState *fs, int idx) {
    cs_assert(idx >= 0 && fitsLA(idx));
    return (fitsSA(idx) 
            ? csC_emitIS(fs, OP_CONST, idx) 
            : csC_emitIL(fs, OP_CONSTL, idx));
}


/* code 'OP_SET' family of instructions */
int csC_storevar(FunctionState *fs, ExpInfo *var, int left) {
    int extra = 0;
    switch (var->et) {
        case EXP_GLOBAL: {
            var->u.info = csC_emitIL(fs, OP_SETGLOBAL, stringK(fs, var->u.str));
            break;
        }
        case EXP_UVAL: {
            var->u.info = csC_emitIL(fs, OP_SETUVAL, var->u.info);
            break;
        }
        case EXP_LOCAL: {
            var->u.info = csC_emitIL(fs, OP_SETLOCAL, var->u.info);
            break;
        }
        case EXP_INDEXED: {
            var->u.info = csC_emitIL(fs, OP_SETINDEX, left+2);
            extra = 1; /* extra leftover values */
            break;
        }
        case EXP_INDEXSTR: {
            var->u.info = csC_emitILL(fs, OP_SETINDEXSTR, left+1, var->u.info);
            break;
        }
        case EXP_INDEXINT: {
            var->u.info = csC_emitILL(fs, OP_SETINDEXINT, left+1, var->u.info);
            break;
        }
        case EXP_DOT: {
            var->u.info = csC_emitILL(fs, OP_SETPROPERTY, left+1, var->u.info);
            break;
        }
        case EXP_INDEXSUPER:
        case EXP_INDEXSUPERSTR:
        case EXP_DOTSUPER: {
            csP_semerror(fs->lx, "attempt to assign to 'super' property");
            break;
        }
        default: cs_assert(0); break; /* invalid store */
    }
    var->et = EXP_FINEXPR;
    freeslots(fs, 1); /* value */
    return extra;
}


/* ensure variable is on stack */
static int dischargevars(FunctionState *fs, ExpInfo *e) {
    switch (e->et) {
        case EXP_GLOBAL: {
            e->u.info = csC_emitIL(fs, OP_GETGLOBAL, stringK(fs, e->u.str));
            break;
        }
        case EXP_UVAL: {
            e->u.info = csC_emitIL(fs, OP_GETUVAL, e->u.info);
            break;
        }
        case EXP_LOCAL: {
            e->u.info = csC_emitIL(fs, OP_GETLOCAL, e->u.info);
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
        default: return 0; /* expression is not a variable */
    }
    e->et = EXP_FINEXPR;
    return 1;
}


/* get 'pc' of jump instruction destination */
static int getjump(FunctionState *fs, int pc) {
    Instruction *inst = &fs->p->code[pc];
    int offset = GETARG_L(inst, 0);
    if (offset == 0)
        return NOJMP;
    else
        return (pc + getOpSize(*inst)) + offset;
}


/* fix jmp instruction at 'pc' to jump to 'target' */
static void fixjump(FunctionState *fs, int pc, int target) {
    Instruction *jmp = &fs->p->code[pc];
    int offset = c_abs(target - (pc + getOpSize(*jmp)));
    cs_assert(offset > 0); /* at least one expression in between */
    cs_assert(opisjump(*jmp)); /* 'jmp' is a valid jump instruction */
    if (c_unlikely(offset > MAX_LARG)) /* jump is too large? */
        csP_semerror(fs->lx, "control structure too long");
    SETARG_L(jmp, 0, offset); /* fix the jump */
}


/* concatenate jump list 'l2' into jump list 'l1' */
void csC_concatjl(FunctionState *fs, int *l1, int l2) {
    if (l2 == NOJMP) return;
    if (*l1 == NOJMP) {
        *l1 = l2;
    } else {
        int curr = *l1;
        int next;
        while ((next = getjump(fs, curr)) != NOJMP) /* get last jump pc */
            curr = next;
        fixjump(fs, curr, l2); /* last jump jumps to 'l2' */
    }
}


/* backpatch jump list at 'pc' */
void csC_patch(FunctionState *fs, int pc, int target) {
    while (pc != NOJMP) {
        int next = getjump(fs, pc);
        fixjump(fs, pc, target);
        pc = next;
    }
}


/* backpatch jump instruction to current pc */
void csC_patchtohere(FunctionState *fs, int pc) {
    csC_patch(fs, pc, currPC);
}


static void patchlistaux(FunctionState *fs, int list, int target) {
    while (list != NOJMP) {
        int next = getjump(fs, list);
        fixjump(fs, list, target);
        list = next;
    }
}


static void fixjumplists(FunctionState *fs, ExpInfo *e) {
    cs_assert(e->et == EXP_FINEXPR); /* must already be discharged */
    if (hasjumps(e)) {
        int final = currPC; /* position after whole expression */
        patchlistaux(fs, e->f, final);
        patchlistaux(fs, e->t, final);
        e->f = e->t = NOJMP;
    }
}


TValue *csC_getconstant(FunctionState *fs, ExpInfo *v) {
    cs_assert(eisconstant(v) ||         /* expression is a constant... */
              v->et == EXP_INDEXSTR ||  /* ...or indexed by one */
              v->et == EXP_INDEXINT ||
              v->et == EXP_INDEXSUPERSTR ||
              v->et == EXP_DOT ||
              v->et == EXP_DOTSUPER);
    return &fs->p->k[v->u.info];
}


/*
** Convert constant expression to value 'v'.
*/
void csC_constexp2val(FunctionState *fs, ExpInfo *e, TValue *v) {
    switch (e->et) {
        case EXP_NIL: setnilval(v); break;
        case EXP_FALSE: setbfval(v); break;
        case EXP_TRUE: setbtval(v); break;
        case EXP_INT: setival(v, e->u.i); break;
        case EXP_FLT: setfval(v, e->u.n); break;
        case EXP_STRING: {
            setstrval(cast(cs_State *, NULL), v, e->u.str);
            break;
        }
        case EXP_K: {
            setobj(cast(cs_State *, NULL), v, csC_getconstant(fs, e));
            break;
        }
        default: cs_assert(0);
    }
}


void csC_varexp2stack(FunctionState *fs, ExpInfo *e) {
    int et = e->et;
    if (et != EXP_FINEXPR && dischargevars(fs, e)) {
        fixjumplists(fs, e);
        if (et != EXP_CALL)
            csC_reserveslots(fs, 1);
    }
}


/* code op with long and short args */
int csC_emitILS(FunctionState *fs, Instruction op, int a, int b) {
    int offset = csC_emitIL(fs, op, a);
    emitS(fs, b);
    return offset;
}


/* code op with long and 2 short args */
static int emitILSS(FunctionState *fs, Instruction op, int a, int b, int c) {
    int offset = csC_emitILS(fs, op, a, b);
    emitS(fs, c);
    return offset;
}


/* code integer constant */
static int codeintK(FunctionState *fs, cs_Integer i) {
    if (fitsLA(i))
        return csC_emitILS(fs, OP_CONSTI, c_abs(i), encodesign(i));
    else
        return codeK(fs, intK(fs, i));
}


/* code float constant */
static int codefltK(FunctionState *fs, cs_Number n) {
    cs_Integer i;
    if (csO_n2i(n, &i, N2IEXACT) && fitsLA(i))
        return csC_emitILS(fs, OP_CONSTF, c_abs(i), encodesign(i));
    else
        return codeK(fs, fltK(fs, n));
}


void csC_setarraysize(FunctionState *fs, int pc, int asize) {
    Instruction *inst = &fs->p->code[pc];
    asize = (asize != 0 ? csO_ceillog2(asize) + 1 : 0);
    cs_assert(asize <= MAX_SARG);
    SETARG_S(inst, 0, asize); /* set size (log2 - 1) */
}


void csC_setarray(FunctionState *fs, int nelems, int tostore) {
    cs_assert(tostore != 0 && tostore <= ARRFIELDS_PER_FLUSH);
    if (tostore == CS_MULRET)
        tostore = 0; /* return up to stack top */
    csC_emitILS(fs, OP_SETARRAY, nelems, tostore);
    freeslots(fs, tostore); /* free slots holding the array values */
}


void csC_settablesize(FunctionState *fs, int pc, int hsize) {
    Instruction *inst = &fs->p->code[pc];
    hsize = (hsize != 0 ? csO_ceillog2(hsize) + 1 : 0);
    cs_assert(hsize <= MAX_SARG);
    SETARG_S(inst, 0, hsize);
}


/* finalize expression by ensuring it is on stack */
static void dischargetostack(FunctionState *fs, ExpInfo *e) {
    if (!dischargevars(fs, e)) {
        switch (e->et) {
            case EXP_NIL: {
                e->u.info = codenil(fs, 1);
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
            } /* fall through */
            case EXP_K: {
                e->u.info = codeK(fs, e->u.info);
                break;
            }
            default: return;
        }
        e->et = EXP_FINEXPR;
    }
}


static void exp2stack(FunctionState *fs, ExpInfo *e) {
    int et = e->et;
    if (et != EXP_FINEXPR)  {
        dischargetostack(fs, e);
        if (et != EXP_CALL)
            csC_reserveslots(fs, 1);
    }
}


/* ensure expression value is on stack */
void csC_exp2stack(FunctionState *fs, ExpInfo *e) {
    exp2stack(fs, e);
    fixjumplists(fs, e);
}


/* initialize dot indexed expression */
void csC_getfield(FunctionState *fs, ExpInfo *v, ExpInfo *key, int super) {
    cs_assert(v->et == EXP_FINEXPR); /* 'v' must be on stack */
    v->u.info = stringK(fs, key->u.str);
    v->et = (super ? EXP_DOTSUPER : EXP_DOT);
}


/* initialize indexed expression */
void csC_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key, int super) {
    int strK = 0;
    cs_assert(var->et == EXP_FINEXPR); /* 'var' must be finalized (on stack) */
    if (key->et == EXP_STRING) {
        string2K(fs, key); /* make constant */
        strK = 1;
    }
    if (super) {
        if (strK) {
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPERSTR;
        } else {
            csC_exp2stack(fs, key);
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPER;
        }
    } else if (isintKL(key)) {
        var->u.info = cast_int(key->u.i);
        var->et = EXP_INDEXINT;
    } else if (strK) {
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
                     int op)
{
    TValue v1, v2, res;
    if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || !validop(&v1, &v2, op))
        return 0;
    csO_arithmraw(fs->lx->C, &v1, &v2, &res, op);
    if (ttisint(&res)) {
        e1->et = EXP_INT;
        e1->u.i = ival(&res);
    } else { /* folds neither NaN nor 0.0 (to avoid problems with -0.0) */
        cs_Number n = fval(&res);
        if (n == 0 || c_numisnan(n))
            return 0;
        e1->et = EXP_FLT;
        e1->u.n = n;
    }
    return 1;
}


/* code unary instruction; except logical not '!' */
static void codeunary(FunctionState *fs, ExpInfo *e, OpCode op, int line) {
    csC_exp2stack(fs, e);
    e->u.info = csC_emitI(fs, op);
    csC_fixline(fs, line);
}


/* code logical not instruction */
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


/* code unary instruction */
void csC_unary(FunctionState *fs, ExpInfo *e, Unopr uopr, int line) {
    static const ExpInfo dummy = {EXP_INT, {0}, NOJMP, NOJMP};
    cs_assert(0 <= uopr && uopr < OPR_NOUNOPR);
    csC_varexp2stack(fs, e);
    switch (uopr) {
        case OPR_UNM: case OPR_BNOT: {
            if (constfold(fs, e, &dummy, (uopr - OPR_UNM) + CS_OPUNM))
                break; /* folded */
            codeunary(fs, e, unopr2op(uopr), line);
            break;
        }
        case OPR_NOT:  {
            codenot(fs, e); 
            break;
        }
        default: cs_assert(0); /* invalid unary OPR */
    }
}


/* code test jump instruction */
static int codetest(FunctionState *fs, ExpInfo *e, OpCode testop, int cond) {
    exp2stack(fs, e); /* ensure test operand is on the stack */
    return csC_emitILS(fs, testop, 0, cond);
}


/* code test/jump instruction */
int csC_jmp(FunctionState *fs, OpCode opjump) {
    if (opjump == OP_TESTPOP) /* test jump? */
        freeslots(fs, 1); /* test pops one value */
    if (opjump == OP_BJMP) /* break jump? */
        return csC_emitILL(fs, OP_BJMP, 0, 0);
    else /* otherwise test or regular jump */
        return csC_emitIL(fs, opjump, 0);
}


/* code 'OP_TEST' family of instructions */
int csC_test(FunctionState *fs, OpCode optest, int cond) {
    int offset = csC_jmp(fs, optest);
    emitS(fs, cond);
    return offset;
}


/* 
** Insert new jump into 'e' false list.
** This test jumps over the second expression if the first expression
** is false (nil or false).
*/
void falsejmp(FunctionState *fs, ExpInfo *e, OpCode testop) {
    int pc; /* pc of new jump */
    switch (e->et) {
        case EXP_TRUE: case EXP_STRING: case EXP_INT:
        case EXP_FLT: case EXP_K: { /* constant true expression */
            pc = NOJMP; /* don't jump, always true */
            break;
        }
        default: {
            pc = codetest(fs, e, testop, 0); /* jump if false */
            break;
        }
    }
    csC_concatjl(fs, &e->f, pc); /* insert new jump in false list */
    csC_patchtohere(fs, e->t); /* true list jumps to here (after false test) */
    e->t = NOJMP; /* set true list as empty */
}


/* 
** Insert new jump into 'e' true list.
** This test jumps over the second expression if the first expression
** is true (everything else except nil and false).
*/
void truejmp(FunctionState *fs, ExpInfo *e, OpCode testop) {
    int pc;
    switch (e->et) {
        case EXP_NIL: case EXP_FALSE: {
            pc = NOJMP; /* don't jump, always false */
            break;
        }
        default: {
            pc = codetest(fs, e, testop, 1); /* jump if true */
            break;
        }
    }
    csC_concatjl(fs, &e->t, pc); /* insert new jump in true list */
    csC_patchtohere(fs, e->f); /* false list jump to here (after true test) */
    e->f = NOJMP; /* set false list as empty */
}


void csC_prebinary(FunctionState *fs, ExpInfo *e, Binopr op) {
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
        case OPR_GT: case OPR_GE: {
            /* Do not push expression value on the stack yet!
             * It will swap places with the second expression. */
            break;
        }
        case OPR_LT: case OPR_LE: {
            int dummy, dummy2;
            if (!isnumKL(e, &dummy, &dummy2))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral for immediate
             * operand variant instruction */
            break;
        }
        case OPR_CONCAT: {
            csC_exp2stack(fs, e); /* operand must be on stack */
            break;
        }
        case OPR_AND: {
            falsejmp(fs, e, OP_TESTORPOP); /* jump out if 'e' is false */
            break;
        }
        case OPR_OR: {
            truejmp(fs, e, OP_TESTORPOP); /* jump out if 'e' is true */
            break;
        }
        default: cs_assert(0); /* invalid binary operation */
    }
}


/* register constant expressions */
static int exp2K(FunctionState *fs, ExpInfo *e) {
    if (!hasjumps(e)) {
        int info;
        switch (e->et) { /* move constant to 'p->k[]' */
            case EXP_NIL: info = nilK(fs); break;
            case EXP_FALSE: info = falseK(fs); break;
            case EXP_TRUE: info = trueK(fs); break;
            case EXP_STRING: info = stringK(fs, e->u.str); break;
            case EXP_INT: info = intK(fs, e->u.i); break;
            case EXP_FLT: info = fltK(fs, e->u.n); break;
            case EXP_K: info = e->u.info; break;
            default: return 0; /* not a constant */
        }
        cs_assert(0 <= info && info <= MAX_LARG);
        e->u.info = info;
        e->et = EXP_K;
        return 1;
    }
    return 0;
}


/* swap expressions */
c_sinline void swapexp(ExpInfo *e1, ExpInfo *e2) {
    const ExpInfo temp = *e1;
    *e1 = *e2;
    *e2 = temp;
}


/* code generic binary instruction */
static void codebin(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                    int line) {
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADD);
    csC_exp2stack(fs, e1);
    csC_exp2stack(fs, e2);
    freeslots(fs, 1); /* e2 */
    e1->u.info = csC_emitI(fs, op);
    e1->et = EXP_FINEXPR;
    csC_fixline(fs, line);
    csC_emitIS(fs, OP_MBIN, binop2mm(op));
    csC_fixline(fs, line);
}


/* code binary instruction variant where second operator is constant */
static void codebinK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                     int line) {
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADDK);
    int ik = e2->u.info; /* index into 'constants' */
    cs_assert(OP_ADDK <= op && op <= OP_BXORK);
    cs_assert(e2->et == EXP_K);
    csC_exp2stack(fs, e1);
    e1->u.info = csC_emitIL(fs, op, ik);
    e1->et = EXP_FINEXPR;
    csC_fixline(fs, line);
}


/* code arithmetic binary op */
static void codebinarithm(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                          Binopr opr, int flip, int line) {
    if (tonumeral(e2, NULL) && exp2K(fs, e2)) {
        codebinK(fs, e1, e2, opr, line);
    } else {
        if (flip)
            swapexp(e1, e2);
        codebin(fs, e1, e2, opr, line);
    }
}


/* code binary instruction variant where second operand is immediate value */
static void codebinI(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                     int line) {
    int rhs = e2->u.i;
    int rhsabs = c_abs(rhs);
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADDI);
    cs_assert(e2->et == EXP_INT);
    csC_exp2stack(fs, e1);
    e1->u.info = csC_emitILS(fs, op, rhsabs, encodesign(rhs));
    e1->et = EXP_FINEXPR;
    csC_fixline(fs, line);
}


/* code binary instruction trying both the immediate and constant variants */
static void codebinIK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                      int flip, int line) {
    if (isintKL(e2))
        codebinI(fs, e1, e2, opr, line);
    else
        codebinarithm(fs, e1, e2, opr, flip, line);
}


/* code commutative binary instruction */
static void codecommutative(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                            Binopr opr, int line) {
    int flip = 0;
    if (tonumeral(e1, NULL)) {
        swapexp(e1, e2);
        flip = 1;
    }
    codebinIK(fs, e1, e2, opr, flip, line);
}


/* code equality binary instruction */
static void codeeq(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    int imm; /* immediate */
    int isflt;
    int iseq = (opr == OPR_EQ);
    cs_assert(opr == OPR_NE || opr == OPR_EQ);
    UNUSED(isflt);
    if (e1->et != EXP_FINEXPR) {
        /* 'e1' is either a numerical or stored string constant */
        cs_assert(e1->et == EXP_K || e1->et == EXP_INT || e1->et == EXP_FLT);
        swapexp(e1, e2);
    }
    csC_exp2stack(fs, e1); /* ensure 1st expression is on stack */
    if (isnumKL(e2, &imm, &isflt)) { /* 2nd expression is immediate operand? */
        e1->u.info = emitILSS(fs, OP_EQI, imm, encodesign(imm), iseq);
    } else if (exp2K(fs, e2)) { /* 2nd expression is a constant? */
        e1->u.info = csC_emitILS(fs, OP_EQK, e2->u.info, iseq);
    } else { /* otherwise 2nd expression must be on stack */
        csC_exp2stack(fs, e2); /* ensure 2nd expression is on stack */
        e1->u.info = csC_emitIS(fs, OP_EQ, iseq);
        freeslots(fs, 1); /* e2 */
    }
    e1->et = EXP_FINEXPR;
}


/* code binary ordering instruction */
static void codeorder(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    OpCode op;
    int isflt, imm;
    UNUSED(isflt);
    cs_assert(OPR_LT == opr || OPR_LE == opr); /* already swapped */
    if (isnumKL(e2, &imm, &isflt)) {
        csC_exp2stack(fs, e1); /* ensure 'e1' is on stack */
        op = binopr2op(opr, OPR_LT, OP_LTI);
        goto code;
    } else if (isnumKL(e1, &imm, &isflt)) {
        csC_exp2stack(fs, e2); /* ensure 'e2' is on stack */
        op = binopr2op(opr, OPR_LT, OP_GTI);
code:
        e1->u.info = csC_emitILS(fs, op, imm, encodesign(imm));
    } else {
        csC_exp2stack(fs, e1); /* ensure first operand is on stack */
        csC_exp2stack(fs, e2); /* ensure second operand is on stack */
        op = binopr2op(opr, OPR_LT, OP_LT);
        e1->u.info = csC_emitI(fs, op);
        freeslots(fs, 1); /* e2 */
    }
    e1->et = EXP_FINEXPR;
}


static Instruction *previousinstruction(FunctionState *fs) {
    return &fs->p->code[fs->prevpc];
}


static void codeconcat(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, int line) {
    Instruction *inst = previousinstruction(fs);
    UNUSED(e2);
    if (*inst == OP_CONCAT) { /* 'e2' is a concatenation? */
        int n = GETARG_L(inst, 0);
        SETARG_L(inst, 0, n + 1); /* will concatenate one more element */
    } else { /* 'e2' is not a concatenation */
        e1->u.info = csC_emitIL(fs, OP_CONCAT, 2);
        e1->et = EXP_FINEXPR;
        csC_fixline(fs, line);
    }
    freeslots(fs, 1);
}


void csC_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                int line) {
    if (oprisfoldable(opr) && constfold(fs, e1, e2, opr + CS_OPADD))
        return; /* done (folded) */
    switch (opr) {
        case OPR_ADD: case OPR_MUL:
        case OPR_BAND: case OPR_BOR: case OPR_BXOR: {
            codecommutative(fs, e1, e2, opr, line);
            break;
        }
        case OPR_SUB: case OPR_DIV: case OPR_MOD: case OPR_POW: {
            codebinarithm(fs, e1, e2, opr, 0, line);
            break;
        }
        case OPR_SHL: case OPR_SHR:  {
            codebinIK(fs, e1, e2, opr, 0, line);
            break;
        }
        case OPR_CONCAT: {
            csC_exp2stack(fs, e2); /* second operand must be on stack */
            codeconcat(fs, e1, e2, line);
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
        } /* fall through */
        case OPR_LT: case OPR_LE: {
            codeorder(fs, e1, e2, opr);
            break;
        }
        case OPR_AND: {
            cs_assert(e1->t == NOJMP); /* list closed by 'csC_prebinary' */
            dischargevars(fs, e2);
            csC_concatjl(fs, &e2->f, e1->f);
            *e1 = *e2;
            break;
        }
        case OPR_OR: {
            cs_assert(e1->f == NOJMP); /* list closed by 'csC_prebinary' */
            dischargevars(fs, e2);
            csC_concatjl(fs, &e2->t, e1->t);
            *e1 = *e2;
            break;
        }
        default: cs_assert(0);
    }
}


/* return the final target of a jump (skipping jumps to jumps) */
static int finaltarget(Instruction *code, int i) {
  for (int count = 0; count < 100; count++) { /* avoid infinite loops */
      Instruction *pc = &code[i];
      if (*pc == OP_JMP) { /* jump forward? */
          i += SIZEINSTR + SIZEARGL; /* skip instruction and offset */
          i += GETARG_L(pc, 0);
      } else if (*pc == OP_JMPS) { /* jump back? */
          i += SIZEINSTR + SIZEARGL; /* skip instruction and offset */
          i -= GETARG_L(pc, 0);
      } else /* no jumps */
          break;
  }
  return i;
}


/*
** Perform a final pass performing small adjustments and
** optimizations.
*/
void csC_finish(FunctionState *fs) {
    Proto *p = fs->p;
    Instruction *pc;
    for (int i = 0; i < currPC; i += getOpSize(*pc)) {
        pc = &p->code[i];
        switch (*pc) {
            case OP_RET: { /* check if need to close variables */
                if (fs->needclose)
                    SETARG_LLS(pc, 1); /* set the flag */
                break;
            }
            /* TODO: finaltarget the OP_BJMP and accumulate npop */
            case OP_JMP: case OP_JMPS: { /* avoid jumps to jumps */
                int target = finaltarget(p->code, i);
                fixjump(fs, i, target);
                break;
            }
            default: break;
        }
    }
}
