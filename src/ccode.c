/*
** ccode.c
*  Bytecode emiting functions
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "ccode.h"
#include "clexer.h"
#include "ctable.h"
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
        cast(OpCode, cast_int(opr) - OPR_UNM + OP_UNM)


/* binary operation to OpCode */
#define binopr2op(opr,x,from) \
        cast(OpCode, (cast_int(opr) - cast_int(x)) + cast_int(from))


/* binary OpCode to metamethod tag */
#define binop2mm(op) \
        cast(cs_MM, (cast_int(op) - OP_ADD) + cast_int(CS_MM_ADD))


/*
** Max(Min)imum possible values for immediate operand.
** Maximum limit is a "bit" smaller than 'MAX_ARG_*',
** in order to ensure that the most significant bit is never set.
** This is because immediate operands can be negative values
** and we must be able to code them into the array and later decode them.
** For example, bit pattern of signed integer '1111_1111' encodes value -1,
** first we take absolute value '0000_0001',
** then we set the most significant bit '1000_0001',
** finally we code that result into the array.
** When decoding, presence of most significant bit is checked,
** if present first do '1000_0001 & 0111_1111',
** then convert to signed '(char)(~0000_0001 + 1)'.
** Note: all of this is done on integers, so the exact bit operations
** differ from the ones shown above.
*/
#define MIN_IMM         (MIN_ARG_S>>1)
#define MAX_IMM         (MAX_ARG_S>>1)
#define MIN_IMML        (MIN_ARG_L>>1)
#define MAX_IMML        (MAX_ARG_L>>1)

/*
** Check if value is in range of short/long immediate operand.
*/
#define isIMM(i)        (MIN_IMM <= (i) && (i) <= MAX_IMM)
#define isIMML(i)       (MIN_IMML <= (i) && (i) <= MAX_IMML)


#define encodesign(x)     ((x) < 0 ? 0 : 2)



/* 
** OpCode properties table.
** ORDER OP
*/
CSI_DEF const c_byte csC_opProp[NUM_OPCODES] = {
    /*     J  F      */
    opProp(0, FormatI), /* OP_TRUE */
    opProp(0, FormatI), /* OP_FALSE */
    opProp(0, FormatI), /* OP_SUPER */
    opProp(0, FormatI), /* OP_NIL */
    opProp(0, FormatIL), /* OP_NILN */
    opProp(0, FormatIL), /* OP_LOAD */
    opProp(0, FormatIS), /* OP_CONST */
    opProp(0, FormatIL), /* OP_CONSTL */
    opProp(0, FormatIS), /* OP_CONSTI */
    opProp(0, FormatIL), /* OP_CONSTIL */
    opProp(0, FormatIS), /* OP_CONSTF */
    opProp(0, FormatIL), /* OP_CONSTFL */
    opProp(0, FormatIL), /* OP_VARARGPREP */
    opProp(0, FormatIL), /* OP_VARARG */
    opProp(0, FormatIL), /* OP_CLOSURE */
    opProp(0, FormatIS), /* OP_NEWLIST */
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
    opProp(0, FormatIL), /* OP_IDIVK */
    opProp(0, FormatIL), /* OP_MODK */
    opProp(0, FormatIL), /* OP_POWK */
    opProp(0, FormatIL), /* OP_BSHLK */
    opProp(0, FormatIL), /* OP_BSHRK */
    opProp(0, FormatIL), /* OP_BANDK */
    opProp(0, FormatIL), /* OP_BORK */
    opProp(0, FormatIL), /* OP_BXORK */
    opProp(0, FormatIL), /* OP_ADDI */
    opProp(0, FormatIL), /* OP_SUBI */
    opProp(0, FormatIL), /* OP_MULI */
    opProp(0, FormatIL), /* OP_DIVI */
    opProp(0, FormatIL), /* OP_IDIVI */
    opProp(0, FormatIL), /* OP_MODI */
    opProp(0, FormatIL), /* OP_POWI */
    opProp(0, FormatIL), /* OP_BSHLI */
    opProp(0, FormatIL), /* OP_BSHRI */
    opProp(0, FormatIL), /* OP_BANDI */
    opProp(0, FormatIL), /* OP_BORI */
    opProp(0, FormatIL), /* OP_BXORI */
    opProp(0, FormatIS), /* OP_ADD */
    opProp(0, FormatIS), /* OP_SUB */
    opProp(0, FormatIS), /* OP_MUL */
    opProp(0, FormatIS), /* OP_DIV */
    opProp(0, FormatIS), /* OP_IDIV */
    opProp(0, FormatIS), /* OP_MOD */
    opProp(0, FormatIS), /* OP_POW */
    opProp(0, FormatIS), /* OP_BSHL */
    opProp(0, FormatIS), /* OP_BSHR */
    opProp(0, FormatIS), /* OP_BAND */
    opProp(0, FormatIS), /* OP_BOR */
    opProp(0, FormatIS), /* OP_BXOR */
    opProp(0, FormatIL), /* OP_CONCAT */
    opProp(0, FormatILS), /* OP_EQK */
    opProp(0, FormatILS), /* OP_EQI */
    opProp(0, FormatIL), /* OP_LTI */
    opProp(0, FormatIL), /* OP_LEI */
    opProp(0, FormatIL), /* OP_GTI */
    opProp(0, FormatIL), /* OP_GEI */
    opProp(0, FormatIS), /* OP_EQ */
    opProp(0, FormatIS), /* OP_LT */
    opProp(0, FormatIS), /* OP_LE */
    opProp(0, FormatI), /* OP_EQPRESERVE */
    opProp(0, FormatI), /* OP_UNM */
    opProp(0, FormatI), /* OP_BNOT */
    opProp(0, FormatI), /* OP_NOT */
    opProp(1, FormatIL), /* OP_JMP */
    opProp(1, FormatIL), /* OP_JMPS */
    opProp(1, FormatILL), /* OP_BJMP */
    opProp(1, FormatILS), /* OP_TEST */
    opProp(1, FormatILS), /* OP_TESTORPOP */
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
    opProp(0, FormatILLS), /* OP_SETLIST */
    opProp(0, FormatILL), /* OP_SETPROPERTY */
    opProp(0, FormatIL), /* OP_GETPROPERTY */
    opProp(0, FormatI), /* OP_GETINDEX */
    opProp(0, FormatIL), /* OP_SETINDEX */
    opProp(0, FormatIL), /* OP_GETINDEXSTR */
    opProp(0, FormatILL), /* OP_SETINDEXSTR */
    opProp(0, FormatIS), /* OP_GETINDEXINT */
    opProp(0, FormatIL), /* OP_GETINDEXINTL */
    opProp(0, FormatILS), /* OP_SETINDEXINT */
    opProp(0, FormatILL), /* OP_SETINDEXINTL */
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
** OpFormat size table (in bytes).
*/
CSI_DEF const c_byte csC_opSize[FormatN] = {    /* ORDER OPFMT */
    SIZE_INSTR,                             /* FormatI */
    SIZE_INSTR+SIZE_ARG_S,                  /* FormatIS */
    SIZE_INSTR+SIZE_ARG_S*2,                /* FormatISS */
    SIZE_INSTR+SIZE_ARG_L,                  /* FormatIL */
    SIZE_INSTR+SIZE_ARG_L+SIZE_ARG_S,       /* FormatILS */
    SIZE_INSTR+SIZE_ARG_L*2,                /* FormatILL */
    SIZE_INSTR+SIZE_ARG_L*2+SIZE_ARG_S,     /* FormatILLS */
    SIZE_INSTR+SIZE_ARG_L*3,                /* FormatILLL */
};


CSI_DEF const char *csC_opSizeFormat[FormatN] = { /* ORDER OPFMT */
    "FormatI",
    "FormatIS",
    "FormatISS",
    "FormatIL",
    "FormatILS",
    "FormatILL",
    "FormatILLS",
    "FormatILLL",
};


/* 
** Names of all instructions.
*/
CSI_DEF const char *csC_opName[NUM_OPCODES] = { /* ORDER OP */
"TRUE", "FALSE", "SUPER", "NIL", "NILN", "LOAD", "CONST", "CONSTL", "CONSTI",
"CONSTIL", "CONSTF", "CONSTFL", "VARARGPREP", "VARARG", "CLOSURE", "NEWLIST",
"NEWCLASS", "NEWTABLE", "METHOD", "SETMM", "POP", "POPN", "MBIN", "ADDK",
"SUBK", "MULK", "DIVK", "IDIVK", "MODK", "POWK", "BSHLK", "BSHRK", "BANDK",
"BORK", "BXORK", "ADDI", "SUBI", "MULI", "DIVI", "IDIVI", "MODI", "POWI",
"BSHLI", "BSHRI", "BANDI", "BORI", "BXORI", "ADD", "SUB", "MUL", "DIV", "IDIV",
"MOD", "POW", "BSHL", "BSHR", "BAND", "BOR", "BXOR", "CONCAT", "EQK", "EQI",
"LTI", "LEI", "GTI", "GEI", "EQ", "LT", "LE", "EQPRESERVE", "UNM", "BNOT",
"NOT", "JMP", "JMPS", "BJMP", "TEST", "TESTORPOP", "TESTPOP", "CALL", "CLOSE",
"TBC", "GETGLOBAL", "SETGLOBAL", "GETLOCAL", "SETLOCAL", "GETUVAL", "SETUVAL",
"SETLIST", "SETPROPERTY", "GETPROPERTY", "GETINDEX", "SETINDEX",
"GETINDEXSTR", "SETINDEXSTR", "GETINDEXINT", "GETINDEXINTL", "SETINDEXINT",
"SETINDEXINTL", "GETSUP", "GETSUPIDX", "GETSUPIDXSTR", "INHERIT", "FORPREP",
"FORCALL", "FORLOOP", "RET",
};


/*
** Get absolute value of integer without branching
** (assuming two's complement).
*/
static uint c_abs(int v) {
    int const mask = v >> (sizeof(int)*CHAR_BIT - 1);
    return (v+mask)^mask;
}


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
    cs_assert(testJProp(fs->p->code[fs->prevpc])); /* last inst. is jump */
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
    currPC += SIZE_ARG_L;
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
    return currPC - SIZE_INSTR;
}


/* code instruction 'i' */
int csC_emitI(FunctionState *fs, Instruction i) {
    cs_assert(fs->prevpc <= currPC);
    return codeinstruction(fs, i);
}


/* code short arg */
static int emitS(FunctionState *fs, int arg) {
    cs_assert(0 <= arg && arg <= MAX_ARG_S);
    emitbyte(fs, arg);
    return currPC - SIZE_ARG_S;
}


/* code long arg */
static int emitL(FunctionState *fs, int arg) {
    cs_assert(0 <= arg && arg <= MAX_ARG_L);
    emit3bytes(fs, arg);
    return currPC - SIZE_ARG_L;
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
    csM_growarray(C, p->k, p->sizek, k, MAX_ARG_L, "constants", TValue);
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
    settval(fs->lx->C, &key, fs->lx->tab);
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
        const int nmb = c_floatatt(MANT_DIG); 
        /* q = 1.0 * (1/2^52) */
        const cs_Number q = c_mathop(ldexp)(c_mathop(1.0), -nmb + 1);
        const cs_Number k = (ik == 0 ? q : n + n*q); /* new key */
        TValue kv;
        setfval(&kv, k);
        /* result is not an integral value, unless value is too large */
        cs_assert(!csO_n2i(k, &ik, N2IEXACT) ||
                   c_mathop(fabs)(n) >= c_mathop(1e6));
        return addK(fs, &kv, &vn);
    }
}


/* adjust 'maxstack' */
void csC_checkstack(FunctionState *fs, int n) {
    int newstack = fs->sp + n;
    cs_assert(newstack >= 0);
    if (fs->p->maxstack < newstack) {
        if (c_unlikely(newstack >= MAX_ARG_L))
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
    nreturns++;
    if (e->et == EXP_CALL) {
        SETARG_L(pc, 1, nreturns);
    } else {
        cs_assert(e->et == EXP_VARARG);
        SETARG_L(pc, 0, nreturns);
        csC_reserveslots(fs, 1);
    }
    e->et = EXP_FINEXPR;
}


/*
** Auxiliary function for 'adjuststack', checks if the 'prev' instruction
** adjusts the stack in the same direction as 'new'.
*/
static int sameadjust(OpCode prev, OpCode new) {
    /* adjust instructions do not overlap and are ordered properly */
    cs_assert(OP_POP-1 != OP_NIL && OP_NIL-1 != OP_POP &&
              OP_POPN-1 == OP_POP && OP_NILN-1 == OP_NIL);
    return (prev == new || prev == new-1 || prev-1 == new);
}


static int adjuststack(FunctionState *fs, OpCode op, int n) {
    Instruction *inst = prevOP(fs);
    int prevn = 0;
    cs_assert((n > 1) == (op == OP_POPN || op == OP_NILN));
    if (inst && sameadjust(*inst, op)) { /* can optimize? */
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
            default: cs_assert(0); /* invalid OpCode */
        }
        n += prevn;
    }
    if (n == 1) {
        cs_assert(op == OP_NIL || op == OP_POP);
        return csC_emitI(fs, op);
    } else {
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


void csC_load(FunctionState *fs, int stk) {
    csC_emitIL(fs, OP_LOAD, stk);
    csC_reserveslots(fs, 1);
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



/*
** Check if expression is a integer constant without jumps.
*/
static int isintK(ExpInfo *e) {
    return (e->et == EXP_INT && !hasjumps(e));
}


/*
** Check if 'isintK' and it is in range of long immediate operand.
*/
static int isintKL(ExpInfo *e) {
    return (isintK(e) && isIMML(e->u.i));
}


/*
** Code load constant instruction.
*/
static int codeK(FunctionState *fs, int idx) {
    cs_assert(0 <= idx && idx <= MAX_ARG_L);
    return (idx <= MAX_ARG_S) 
            ? csC_emitIS(fs, OP_CONST, idx) 
            : csC_emitIL(fs, OP_CONSTL, idx);
}


/*
** Encode short immediate operand by moving the sign bit
** from 32nd bit to the 8th bit.
*/
static int imms(int imm) {
    int x = check_exp(imm < 0 && MIN_ARG_S <= imm, c_abs(imm));
    cs_assert(!(x & 0x80)); /* 8th bit must be free */
    return (x|0x80); /* set 8th bit */
}


/*
** Encode long immediate operand by moving the sign bit
** from 32nd bit to the 24th bit.
*/
static int imml(int imm) {
    uint x = check_exp(imm < 0 && MIN_ARG_L <= imm, c_abs(imm));
    cs_assert(!(x & 0x800000)); /* 24th bit must be free */
    return (x|0x800000); /* set 24th bit */
}


/*
** Encode value as immediate operand.
*/
static int encodeimm(int imm) {
    cs_assert(isIMM(imm) || isIMML(imm)); /* must fit */
    if (imm < 0) { /* must encode? */
        if (imm >= MIN_ARG_S)
            imm = imms(imm);
        else
            imm = imml(imm);
    } /* else return as it is */
    return imm;
}


/*
** Check if 'e' is numeral constant that is in range of
** long immediate operand.
*/
static int isnumIK(ExpInfo *e, int *imm) {
    cs_Integer i;
    if (e->et == EXP_INT)
        i = e->u.i;
    else if (!(e->et == EXP_FLT && csO_n2i(e->u.n, &i, N2IEXACT)))
        return 0;
    if (!hasjumps(e) && isIMML(i)) {
        *imm = (i < 0) ? imml(i) : i;
        return 1;
    }
    return 0;
}


static int setindexint(FunctionState *fs, ExpInfo *v, int left) {
    uint imm = check_exp(v->et == EXP_INDEXINT, encodeimm(v->u.info));
    if (isIMM(v->u.info))
        return csC_emitILS(fs, OP_SETINDEXINT, left, imm);
    else
        return csC_emitILL(fs, OP_SETINDEXINTL, left, imm);
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
            var->u.info = setindexint(fs, var, left+1);
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


static int getindexint(FunctionState *fs, ExpInfo *v) {
    uint imm = check_exp(v->et == EXP_INDEXINT, encodeimm(v->u.info));
    if (isIMM(v->u.info))
        return csC_emitIS(fs, OP_GETINDEXINT, imm);
    else
        return csC_emitIL(fs, OP_GETINDEXINTL, imm);
}


/* ensure variable is on stack */
static int dischargevars(FunctionState *fs, ExpInfo *v) {
    switch (v->et) {
        case EXP_GLOBAL: {
            v->u.info = csC_emitIL(fs, OP_GETGLOBAL, stringK(fs, v->u.str));
            break;
        }
        case EXP_UVAL: {
            v->u.info = csC_emitIL(fs, OP_GETUVAL, v->u.info);
            break;
        }
        case EXP_LOCAL: {
            v->u.info = csC_emitIL(fs, OP_GETLOCAL, v->u.info);
            break;
        }
        case EXP_INDEXED: {
            freeslots(fs, 2); /* receiver, key */
            v->u.info = csC_emitI(fs, OP_GETINDEX);
            break;
        }
        case EXP_INDEXSTR: {
            freeslots(fs, 1); /* receiver */
            v->u.info = csC_emitIL(fs, OP_GETINDEXSTR, v->u.info);
            break;
        }
        case EXP_INDEXINT: {
            freeslots(fs, 1); /* receiver */
            v->u.info = getindexint(fs, v);
            break;
        }
        case EXP_INDEXSUPER: {
            freeslots(fs, 2); /* 'self' (instance), key */
            v->u.info = csC_emitI(fs, OP_GETSUPIDX);
            break;
        }
        case EXP_INDEXSUPERSTR: {
            freeslots(fs, 1); /* 'self' (instance) */
            v->u.info = csC_emitIL(fs, OP_GETSUPIDXSTR, v->u.info);
            break;
        }
        case EXP_DOT: {
            freeslots(fs, 1); /* receiver */
            v->u.info = csC_emitIL(fs, OP_GETPROPERTY, v->u.info);
            break;
        }
        case EXP_DOTSUPER: {
            freeslots(fs, 1); /* 'self' (instance) */
            v->u.info = csC_emitIL(fs, OP_GETSUP, v->u.info);
            break;
        }
        case EXP_CALL: case EXP_VARARG: {
            csC_setoneret(fs, v);
            break;
        }
        default: return 0; /* expression is not a variable */
    }
    v->et = EXP_FINEXPR;
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
    cs_assert(testJProp(*jmp)); /* 'jmp' is a valid jump instruction */
    if (c_unlikely(offset > MAX_ARG_L)) /* jump is too large? */
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
        default: cs_assert(0); /* 'e' is not a constant */
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


/* code integer as constant or immediate operand */
static int codeintIK(FunctionState *fs, cs_Integer i) {
    if (isIMM(i))
        return csC_emitIS(fs, OP_CONSTI, encodeimm(i));
    else if (isIMML(i))
        return csC_emitIL(fs, OP_CONSTIL, encodeimm(i));
    else
        return codeK(fs, intK(fs, i));
}


/* code float as constant or immediate operand */
static int codefltIK(FunctionState *fs, cs_Number n) {
    cs_Integer i;
    if (csO_n2i(n, &i, N2IEXACT)) { /* try code as immediate? */
        if (isIMM(i))
            return csC_emitIS(fs, OP_CONSTF, encodeimm(i));
        else if (isIMML(i))
            return csC_emitIL(fs, OP_CONSTFL, encodeimm(i));
    } /* else make a constant */
    return codeK(fs, fltK(fs, n));
}


void csC_setlistsize(FunctionState *fs, int pc, int lsz) {
    Instruction *inst = &fs->p->code[pc];
    lsz = (lsz != 0 ? csO_ceillog2(lsz) + 1 : 0);
    cs_assert(lsz <= MAX_ARG_S);
    SETARG_S(inst, 0, lsz); /* set size (log2 - 1) */
}


static int emitILLS(FunctionState *fs, Instruction i, int a, int b, int c) {
    int offset = csC_emitILL(fs, i, a, b);
    emitS(fs, c);
    return offset;
}


void csC_setlist(FunctionState *fs, int base, int nelems, int tostore) {
    cs_assert(LISTFIELDS_PER_FLUSH <= MAX_ARG_S);
    cs_assert(tostore != 0 && tostore <= LISTFIELDS_PER_FLUSH);
    if (tostore == CS_MULRET) tostore = 0; /* return up to stack top */
    emitILLS(fs, OP_SETLIST, base, nelems, tostore);
    freeslots(fs, tostore); /* free slots holding the list values */
}


void csC_settablesize(FunctionState *fs, int pc, int hsize) {
    Instruction *inst = &fs->p->code[pc];
    hsize = (hsize != 0 ? csO_ceillog2(hsize) + 1 : 0);
    cs_assert(hsize <= MAX_ARG_S);
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
                e->u.info = codeintIK(fs, e->u.i);
                break;
            }
            case EXP_FLT: {
                e->u.info = codefltIK(fs, e->u.n);
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


/*
** Return 0 if folding for 'op' can raise errors.
*/
static int validop(TValue *v1, TValue *v2, int op) {
    switch (op) {
        case CS_OPBSHR: case CS_OPBSHL: case CS_OPBAND:
        case CS_OPBOR: case CS_OPBXOR: case CS_OPBNOT: { /* conversion */
            cs_Integer i;
            return (csO_tointeger(v1, &i, N2IEXACT) &&
                    csO_tointeger(v2, &i, N2IEXACT));
        }
        case CS_OPDIV: case CS_OPIDIV: case CS_OPMOD: /* division by 0 */
            return (nval(v2) != 0);
        default: return 1; /* everything else is valid */
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
        case EXP_TRUE: case EXP_INT: case EXP_FLT:
        case EXP_STRING: case EXP_K: {
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
    freeslots(fs, 1); /* test removes first expression if it goes through */
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


static void patchtruelist(FunctionState *fs, ExpInfo *e) {
    csC_patchtohere(fs, e->t);
    e->t = NOJMP;
}


/* 
** Insert new jump into 'e' false list.
** This test jumps over the second expression if the first expression
** is false (nil or false).
*/
static void andjump(FunctionState *fs, ExpInfo *e) {
    int pc;
    switch (e->et) {
        case EXP_TRUE: case EXP_STRING: case EXP_INT:
        case EXP_FLT: case EXP_K: { /* constant true expression */
            pc = NOJMP; /* don't jump, always true */
            break;
        }
        default: {
            pc = codetest(fs, e, OP_TESTORPOP, 0); /* jump if false */
            if (e->t != NOJMP) {
                int jump = csC_jmp(fs, OP_JMP);
                patchtruelist(fs, e);
                csC_emitI(fs, OP_POP);
                csC_patchtohere(fs, jump);
            }
            break;
        }
    }
    csC_concatjl(fs, &e->f, pc); /* insert new jump in false list */
    patchtruelist(fs, e);
}


static void patchfalselist(FunctionState *fs, ExpInfo *e) {
    csC_patchtohere(fs, e->f);
    e->f = NOJMP;
}


/* 
** Insert new jump into 'e' true list.
** This test jumps over the second expression if the first expression
** is true (everything else except nil and false).
*/
void orjump(FunctionState *fs, ExpInfo *e) {
    int pc;
    switch (e->et) {
        case EXP_NIL: case EXP_FALSE: {
            pc = NOJMP; /* don't jump, always false */
            break;
        }
        default: {
            pc = codetest(fs, e, OP_TESTORPOP, 1); /* jump if true */
            if (e->f != NOJMP) {
                int jump = csC_jmp(fs, OP_JMP);
                patchfalselist(fs, e);
                csC_emitI(fs, OP_POP);
                csC_patchtohere(fs, jump);
            }
            break;
        }
    }
    csC_concatjl(fs, &e->t, pc); /* insert new jump in true list */
    patchfalselist(fs, e);
}


void csC_prebinary(FunctionState *fs, ExpInfo *e, Binopr op) {
    switch (op) {
        case OPR_ADD: case OPR_SUB: case OPR_MUL:
        case OPR_DIV: case OPR_IDIV: case OPR_MOD:
        case OPR_POW: case OPR_SHL: case OPR_SHR:
        case OPR_BAND: case OPR_BOR: case OPR_BXOR:
        case OPR_NE: case OPR_EQ: {
            if (!tonumeral(e, NULL))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral for constant
            ** or immediate operand variant instruction */
            break;
        }
        case OPR_GT: case OPR_GE:
        case OPR_LT: case OPR_LE: {
            int dummy;
            if (!isnumIK(e, &dummy))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral for immediate
            ** operand variant instruction */
            break;
        }
        case OPR_CONCAT: {
            csC_exp2stack(fs, e); /* operand must be on stack */
            break;
        }
        case OPR_AND: {
            andjump(fs, e); /* jump out if 'e' is false */
            break;
        }
        case OPR_OR: {
            orjump(fs, e); /* jump out if 'e' is true */
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
        cs_assert(0 <= info && info <= MAX_ARG_L);
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
                    int commutative, int line) {
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADD);
    int swap = !commutative && (e1->et != EXP_FINEXPR && e2->et == EXP_FINEXPR);
    csC_exp2stack(fs, e1);
    csC_exp2stack(fs, e2);
    freeslots(fs, 1); /* e2 */
    e1->u.info = csC_emitIS(fs, op, swap);
    e1->et = EXP_FINEXPR;
    csC_fixline(fs, line);
    csC_emitIS(fs, OP_MBIN, binop2mm(op) | ((swap) ? 0x80 : 0));
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
                          Binopr opr, int flip, int commutative, int line) {
    if (tonumeral(e2, NULL) && exp2K(fs, e2))
        codebinK(fs, e1, e2, opr, line);
    else {
        if (flip)
            swapexp(e1, e2);
        codebin(fs, e1, e2, opr, commutative, line);
    }
}


/* code binary instruction variant where second operand is immediate value */
static void codebinI(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                     int line) {
    int imm = e2->u.i;
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADDI);
    cs_assert(e2->et == EXP_INT);
    csC_exp2stack(fs, e1);
    e1->u.info = csC_emitIL(fs, op, (imm < 0 ? imml(imm) : imm));
    e1->et = EXP_FINEXPR;
    csC_fixline(fs, line);
}


/* code binary instruction trying both the immediate and constant variants */
static void codebinIK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                      int flip, int commutative, int line) {
    if (isintKL(e2))
        codebinI(fs, e1, e2, opr, line);
    else
        codebinarithm(fs, e1, e2, opr, flip, commutative, line);
}


/* code commutative binary instruction */
static void codecommutative(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                            Binopr opr, int line) {
    int flip = 0;
    if (tonumeral(e1, NULL)) {
        swapexp(e1, e2);
        flip = 1;
    }
    codebinIK(fs, e1, e2, opr, flip, 1, line);
}


/* code equality binary instruction */
static void codeeq(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
    int imm; /* immediate */
    int iseq = (opr == OPR_EQ);
    cs_assert(opr == OPR_NE || opr == OPR_EQ);
    if (e1->et != EXP_FINEXPR) {
        /* 'e1' is either a numerical or stored string constant */
        cs_assert(e1->et == EXP_K || e1->et == EXP_INT || e1->et == EXP_FLT);
        swapexp(e1, e2);
    }
    csC_exp2stack(fs, e1); /* ensure 1st expression is on stack */
    if (isnumIK(e2, &imm)) /* 2nd expression is immediate operand? */
        e1->u.info = csC_emitILS(fs, OP_EQI, imm, iseq);
    else if (exp2K(fs, e2)) /* 2nd expression is a constant? */
        e1->u.info = csC_emitILS(fs, OP_EQK, e2->u.info, iseq);
    else { /* otherwise 2nd expression must be on stack */
        csC_exp2stack(fs, e2); /* ensure 2nd expression is on stack */
        e1->u.info = csC_emitIS(fs, OP_EQ, iseq);
        freeslots(fs, 1); /* e2 */
    }
    e1->et = EXP_FINEXPR;
}


/* code binary ordering instruction */
static void codeorder(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                      Binopr opr, int swapped) {
    OpCode op;
    int imm;
    cs_assert(OPR_LT == opr || OPR_LE == opr); /* already swapped */
    csC_varexp2stack(fs, e1);
    csC_varexp2stack(fs, e2);
    if (isnumIK(e2, &imm)) {
        csC_exp2stack(fs, e1); /* ensure 'e1' is on stack */
        op = binopr2op(opr, OPR_LT, OP_LTI);
        goto code;
    } else if (isnumIK(e1, &imm)) {
        csC_exp2stack(fs, e2); /* ensure 'e2' is on stack */
        op = binopr2op(opr, OPR_LT, OP_GTI);
code:
        e1->u.info = csC_emitIL(fs, op, imm);
    } else {
        int swap = 0;
        if (!swapped)
            swap = (e1->et != EXP_FINEXPR && e2->et == EXP_FINEXPR);
        else if (e2->et == EXP_FINEXPR)
            swap = 1;
        else if (e1->et == EXP_FINEXPR && e2->et != EXP_FINEXPR)
            swap = 0;
        csC_exp2stack(fs, e1); /* ensure first operand is on stack */
        csC_exp2stack(fs, e2); /* ensure second operand is on stack */
        op = binopr2op(opr, OPR_LT, OP_LT);
        e1->u.info = csC_emitIS(fs, op, swap);
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


static int codeaddnI(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                           int line) {
    if (!isintK(e2))
        return 0; /* not an integer constant */
    else {
        cs_Integer i2 = e2->u.i;
        if (!(isIMML(i2)))
            return 0; /* not in the proper range */
        else {
            e2->u.i = -cast_int(i2);
            codebinI(fs, e1, e2, OPR_ADD, line);
            return 1; /* successfully coded */
        }
    }
}


void csC_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                int line) {
    int swapped = 0;
    if (oprisfoldable(opr) && constfold(fs, e1, e2, opr + CS_OPADD))
        return; /* done (folded) */
    switch (opr) {
        case OPR_ADD: case OPR_MUL:
        case OPR_BAND: case OPR_BOR: case OPR_BXOR: {
            codecommutative(fs, e1, e2, opr, line);
            break;
        }
        case OPR_SUB: {
            if (codeaddnI(fs, e1, e2, line))
                break; /* coded as (r1 + -I) */
            /* else */
        } /* fall through */
        case OPR_IDIV: case OPR_DIV: case OPR_MOD: case OPR_POW: {
            csC_varexp2stack(fs, e2);
            codebinIK(fs, e1, e2, opr, 0, 0, line);
            break;
        }
        case OPR_SHL: case OPR_SHR:  {
            csC_varexp2stack(fs, e2);
            codebinIK(fs, e1, e2, opr, 0, 0, line);
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
            csC_varexp2stack(fs, e1);
            csC_varexp2stack(fs, e2);
            swapexp(e1, e2);
            opr = (opr - OPR_GT) + OPR_LT;
            swapped = 1;
        } /* fall through */
        case OPR_LT: case OPR_LE: {
            codeorder(fs, e1, e2, opr, swapped);
            break;
        }
        case OPR_AND: {
            cs_assert(e1->t == NOJMP); /* list closed by 'csC_prebinary' */
            exp2stack(fs, e2);
            csC_concatjl(fs, &e2->f, e1->f);
            *e1 = *e2;
            break;
        }
        case OPR_OR: {
            cs_assert(e1->f == NOJMP); /* list closed by 'csC_prebinary' */
            exp2stack(fs, e2);
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
          i += (SIZE_INSTR+SIZE_ARG_L); /* skip instruction and offset */
          i += GETARG_L(pc, 0);
      } else if (*pc == OP_JMPS) { /* jump back? */
          i += (SIZE_INSTR+SIZE_ARG_L); /* skip instruction and offset */
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
