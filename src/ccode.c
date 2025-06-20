/*
** ccode.c
*  Bytecode emiting functions
** See Copyright Notice in cscript.h
*/

#define ccode_c
#define CS_CORE

#include "cprefix.h"

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


/*
** TODO: change how and/or is compiled, accordingly change
** 'csC_exp2stack' which all together might require changing bytecode.
*/


/* check if expression has jumps */
#define hasjumps(e)     ((e)->t != (e)->f)


/* unary operation to opcode */
#define unopr2op(opr) \
        cast(OpCode, cast_int(opr) - OPR_UNM + OP_UNM)


/* binary operation to opcode */
#define binopr2op(opr,x,from) \
        cast(OpCode, (cast_int(opr) - cast_int(x)) + cast_int(from))


/* binary opcode to metamethod tag */
#define binop2mm(op) \
        ((cast_int(op) - OP_ADD) + cast_int(CS_MM_ADD))


/*
** Max(Min)imum possible values for immediate operand.
** Maximum limit is a value smaller by one "bit" than 'MAX_ARG_*',
** in order to ensure that the most significant bit is never set.
** This is because immediate operands can be negative values
** and we must be able to code them into the array and later decode them.
** For example, 8-bit (char) pattern '1111_1111' encodes value -1, first we
** take absolute value '0000_0001', then we set the most significant
** bit '1000_0001', finally we code that result into the array.
** When decoding, presence of most significant bit is checked, if present,
** first do '1000_0001 & 0111_1111', then convert to signed
** '(char)(~0000_0001 + 1)'.
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


/* 
** OpCode properties table.
** ORDER OP
*/
CSI_DEF const OpProperties csC_opproperties[NUM_OPCODES] = {
    { FormatI, 1, 0, 0 }, /* OP_TRUE */
    { FormatI, 1, 0, 0 }, /* OP_FALSE */
    { FormatI, 0, 0, 1 }, /* OP_SUPER */
    { FormatIL, VD, 0, 0 }, /* OP_NIL */
    { FormatIL, VD, 0, 0 }, /* OP_POP */
    { FormatIL, 1, 0, 0 }, /* OP_LOAD */
    { FormatIS, 1, 0, 0 }, /* OP_CONST */
    { FormatIL, 1, 0, 0 }, /* OP_CONSTL */
    { FormatIS, 1, 0, 0 }, /* OP_CONSTI */
    { FormatIL, 1, 0, 0 }, /* OP_CONSTIL */
    { FormatIS, 1, 0, 0 }, /* OP_CONSTF */
    { FormatIL, 1, 0, 0 }, /* OP_CONSTFL */
    { FormatIL, VD, 0, 0 }, /* OP_VARARGPREP */
    { FormatIL, VD, 0, 0 }, /* OP_VARARG */
    { FormatIL, 1, 0, 0 }, /* OP_CLOSURE */
    { FormatIS, 1, 0, 0 }, /* OP_NEWLIST */
    { FormatIS, 1, 0, 0 }, /* OP_NEWCLASS */
    { FormatIS, 1, 0, 0 }, /* OP_NEWTABLE */
    { FormatIL, 0, 1, 0 }, /* OP_METHOD */
    { FormatIS, 0, 1, 0 }, /* OP_SETMM */
    { FormatIS, 0, 1, 0 }, /* OP_MBIN */
    { FormatIL, 0, 0, 1 }, /* OP_ADDK */
    { FormatIL, 0, 0, 1 }, /* OP_SUBK */
    { FormatIL, 0, 0, 1 }, /* OP_MULK */
    { FormatIL, 0, 0, 1 }, /* OP_DIVK */
    { FormatIL, 0, 0, 1 }, /* OP_IDIVK */
    { FormatIL, 0, 0, 1 }, /* OP_MODK */
    { FormatIL, 0, 0, 1 }, /* OP_POWK */
    { FormatIL, 0, 0, 1 }, /* OP_BSHLK */
    { FormatIL, 0, 0, 1 }, /* OP_BSHRK */
    { FormatIL, 0, 0, 1 }, /* OP_BANDK */
    { FormatIL, 0, 0, 1 }, /* OP_BORK */
    { FormatIL, 0, 0, 1 }, /* OP_BXORK */
    { FormatIL, 0, 0, 1 }, /* OP_ADDI */
    { FormatIL, 0, 0, 1 }, /* OP_SUBI */
    { FormatIL, 0, 0, 1 }, /* OP_MULI */
    { FormatIL, 0, 0, 1 }, /* OP_DIVI */
    { FormatIL, 0, 0, 1 }, /* OP_IDIVI */
    { FormatIL, 0, 0, 1 }, /* OP_MODI */
    { FormatIL, 0, 0, 1 }, /* OP_POWI */
    { FormatIL, 0, 0, 1 }, /* OP_BSHLI */
    { FormatIL, 0, 0, 1 }, /* OP_BSHRI */
    { FormatIL, 0, 0, 1 }, /* OP_BANDI */
    { FormatIL, 0, 0, 1 }, /* OP_BORI */
    { FormatIL, 0, 0, 1 }, /* OP_BXORI */
    { FormatIS, 0, 1, 1 }, /* OP_ADD */
    { FormatIS, 0, 1, 1 }, /* OP_SUB */
    { FormatIS, 0, 1, 1 }, /* OP_MUL */
    { FormatIS, 0, 1, 1 }, /* OP_DIV */
    { FormatIS, 0, 1, 1 }, /* OP_IDIV */
    { FormatIS, 0, 1, 1 }, /* OP_MOD */
    { FormatIS, 0, 1, 1 }, /* OP_POW */
    { FormatIS, 0, 1, 1 }, /* OP_BSHL */
    { FormatIS, 0, 1, 1 }, /* OP_BSHR */
    { FormatIS, 0, 1, 1 }, /* OP_BAND */
    { FormatIS, 0, 1, 1 }, /* OP_BOR */
    { FormatIS, 0, 1, 1 }, /* OP_BXOR */
    { FormatIL, VD, 0, 1 }, /* OP_CONCAT */
    { FormatILS, 0, 0, 1 }, /* OP_EQK */
    { FormatILS, 0, 0, 1 }, /* OP_EQI */
    { FormatIL, 0, 0, 1 }, /* OP_LTI */
    { FormatIL, 0, 0, 1 }, /* OP_LEI */
    { FormatIL, 0, 0, 1 }, /* OP_GTI */
    { FormatIL, 0, 0, 1 }, /* OP_GEI */
    { FormatIS, 0, 1, 1 }, /* OP_EQ */
    { FormatIS, 0, 1, 1 }, /* OP_LT */
    { FormatIS, 0, 1, 1 }, /* OP_LE */
    { FormatI, 0, 0, 1 }, /* OP_EQPRESERVE */
    { FormatI, 0, 0, 1 }, /* OP_UNM */
    { FormatI, 0, 0, 1 }, /* OP_BNOT */
    { FormatI, 0, 0, 1 }, /* OP_NOT */
    { FormatIL, 0, 0, 0 }, /* OP_JMP */
    { FormatIL, 0, 0, 0 }, /* OP_JMPS */
    { FormatIS, 0, 0, 0 }, /* OP_TEST */
    { FormatIS, 0, 1, 0 }, /* OP_TESTPOP */
    { FormatILL, VD, 0, 1 }, /* OP_CALL */
    { FormatIL, 0, 0, 0 }, /* OP_CLOSE */
    { FormatIL, 0, 0, 0 }, /* OP_TBC */
    { FormatIL, 1, 0, 0 }, /* OP_GETLOCAL */
    { FormatIL, 0, 1, 0 }, /* OP_SETLOCAL */
    { FormatIL, 1, 0, 0 }, /* OP_GETUVAL */
    { FormatIL, 0, 1, 0 }, /* OP_SETUVAL */
    { FormatILLS, VD, 0, 0 }, /* OP_SETLIST */
    { FormatILL, 0, 1, 0 }, /* OP_SETPROPERTY */
    { FormatIL, 0, 0, 1 }, /* OP_GETPROPERTY */
    { FormatI, 0, 1, 1 }, /* OP_GETINDEX */
    { FormatIL, 0, 1, 0 }, /* OP_SETINDEX */
    { FormatIL, 0, 0, 1 }, /* OP_GETINDEXSTR */
    { FormatILL, 0, 1, 0 }, /* OP_SETINDEXSTR */
    { FormatIS, 0, 0, 1 }, /* OP_GETINDEXINT */
    { FormatIL, 0, 0, 1 }, /* OP_GETINDEXINTL */
    { FormatILS, 0, 1, 0 }, /* OP_SETINDEXINT */
    { FormatILL, 0, 1, 0 }, /* OP_SETINDEXINTL */
    { FormatIL, 0, 0, 1 }, /* OP_GETSUP */
    { FormatI, 0, 1, 1 }, /* OP_GETSUPIDX */
    { FormatIL, 0, 0, 1 }, /* OP_GETSUPIDXSTR */
    { FormatI, 0, 0, 0 }, /* OP_INHERIT */
    { FormatILL, 0, 0, 0 }, /* OP_FORPREP */
    { FormatILL, VD, 0, 0 }, /* OP_FORCALL */
    { FormatILLL, VD, 0, 0 }, /* OP_FORLOOP */
    { FormatILLS, 0, 0, 0 }, /* OP_RET */
};


/* 
** OpFormat size table (in bytes).
*/
CSI_DEF const c_byte csC_opsize[FormatN] = { /* ORDER OPFMT */
    SIZE_INSTR,                             /* FormatI */
    SIZE_INSTR+SIZE_ARG_S,                  /* FormatIS */
    SIZE_INSTR+SIZE_ARG_S*2,                /* FormatISS */
    SIZE_INSTR+SIZE_ARG_L,                  /* FormatIL */
    SIZE_INSTR+SIZE_ARG_L+SIZE_ARG_S,       /* FormatILS */
    SIZE_INSTR+SIZE_ARG_L*2,                /* FormatILL */
    SIZE_INSTR+SIZE_ARG_L*2+SIZE_ARG_S,     /* FormatILLS */
    SIZE_INSTR+SIZE_ARG_L*3,                /* FormatILLL */
};


/* 
** Names of all instructions.
*/
CSI_DEF const char *csC_opname[NUM_OPCODES] = { /* ORDER OP */
"TRUE", "FALSE", "SUPER", "NIL", "POP", "LOAD", "CONST", "CONSTL",
"CONSTI", "CONSTIL", "CONSTF", "CONSTFL", "VARARGPREP", "VARARG",
"CLOSURE", "NEWLIST", "NEWCLASS", "NEWTABLE", "METHOD", "SETMM", "MBIN", "ADDK",
"SUBK", "MULK", "DIVK", "IDIVK", "MODK", "POWK", "BSHLK", "BSHRK", "BANDK",
"BORK", "BXORK", "ADDI", "SUBI", "MULI", "DIVI", "IDIVI", "MODI", "POWI",
"BSHLI", "BSHRI", "BANDI", "BORI", "BXORI", "ADD", "SUB", "MUL", "DIV", "IDIV",
"MOD", "POW", "BSHL", "BSHR", "BAND", "BOR", "BXOR", "CONCAT", "EQK", "EQI",
"LTI", "LEI", "GTI", "GEI", "EQ", "LT", "LE", "EQPRESERVE", "UNM", "BNOT",
"NOT", "JMP", "JMPS", "TEST", "TESTPOP", "CALL", "CLOSE",
"TBC", "GETLOCAL", "SETLOCAL", "GETUVAL", "SETUVAL", "SETLIST", "SETPROPERTY",
"GETPROPERTY", "GETINDEX", "SETINDEX", "GETINDEXSTR", "SETINDEXSTR",
"GETINDEXINT", "GETINDEXINTL", "SETINDEXINT", "SETINDEXINTL", "GETSUP",
"GETSUPIDX", "GETSUPIDXSTR", "INHERIT", "FORPREP", "FORCALL", "FORLOOP", "RET",
};


/*
** Get absolute value of integer without branching
** (assuming two's complement).
*/
static c_uint c_abs(int v) {
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
    int opsize = getopSize(p->code[pc]); /* size of last coded instruction */
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
        p->lineinfo[++pc] = ABSLINEINFO; /* set as invalid entry */
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
    /* last instruction is jump */
    cs_assert(fs->p->code[fs->prevpc] == OP_JMP ||
              fs->p->code[fs->prevpc] == OP_JMPS);
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


/* emit instruction */
int csC_emitI(FunctionState *fs, Instruction i) {
    cs_assert(fs->prevpc <= currPC);
    addinstpc(fs);
    emitbyte(fs, i);
    savelineinfo(fs, fs->p, fs->lx->lastline);
    return currPC - SIZE_INSTR;
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


int csC_call(FunctionState *fs, int base, int nreturns) {
    cs_assert(nreturns >= CS_MULRET);
    return csC_emitILL(fs, OP_CALL, base, nreturns + 1);
}


int csC_vararg(FunctionState *fs, int nreturns) {
    cs_assert(nreturns >= CS_MULRET);
    return csC_emitIL(fs, OP_VARARG, nreturns + 1);
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
        if (c_unlikely(newstack >= MAX_CODE))
            csY_syntaxerror(fs->lx, "function requires too much stack space");
        fs->p->maxstack = newstack;
    }
}


/* reserve 'n' stack slots */
void csC_reserveslots(FunctionState *fs, int n) {
    cs_assert(n >= 0);
    csC_checkstack(fs, n);
    fs->sp += n;
    cs_assert(fs->sp >= 0);
}


/* check whether last instruction is an open function call */
static int lastisopenfunc(FunctionState *fs) {
    Instruction *pi = prevOP(fs);
    return (*pi == OP_CALL && GET_ARG_L(pi, 1) == 0);
}


/* check whether last instruction is an open vararg expression */
static int lastisopenvarg(FunctionState *fs) {
    Instruction *pi = prevOP(fs);
    return (*pi == OP_VARARG && GET_ARG_L(pi, 0) == 0);
}


/* finalize open call or vararg expression */
static void setreturns(FunctionState *fs, ExpInfo *e, int nreturns) {
    cs_assert(lastisopenvarg(fs) || lastisopenfunc(fs));
    cs_assert(nreturns >= CS_MULRET);
    if (e->et == EXP_CALL) {
        SET_ARG_L(getpi(fs, e), 1, nreturns + 1);
    } else {
        cs_assert(e->et == EXP_VARARG);
        SET_ARG_L(getpi(fs, e), 0, nreturns + 1);
    }
    e->et = EXP_FINEXPR; /* closed */
}


void csC_setreturns(FunctionState *fs, ExpInfo *e, int nreturns) {
    cs_assert(nreturns >= 0); /* for CS_MULRET use 'csC_setmulret' */
    setreturns(fs, e, nreturns);
    csC_reserveslots(fs, nreturns);
}


void csC_setmulret(FunctionState *fs, ExpInfo *e) {
    cs_assert(lastisopenvarg(fs) || lastisopenfunc(fs));
    setreturns(fs, e, CS_MULRET);
}


/*
** Auxiliary function for 'adjuststack', checks if the 'op' instruction
** can be merged.
*/
static int canmerge(FunctionState *fs, OpCode op) {
    static const int okbarrier[] = { 2, 1, }; /* ORDER OP */
    op -= OP_NIL;
    cs_assert(0 <= op && op < 2);
    cs_assert(0 <= fs->opbarrier && fs->opbarrier <= 3);
    return !fs->opbarrier || /* no barrier or... */
           (okbarrier[op] == fs->opbarrier); /* ...no 'op' barrier? */
}


static int adjuststack(FunctionState *fs, OpCode op, int n) {
    Instruction *pi = prevOP(fs);
    if (pi && *pi == op && canmerge(fs, op)) {
        int newn = GET_ARG_L(pi, 0) + n;
        SET_ARG_L(pi, 0, newn);
        return fs->prevpc; /* done; do not code new instruction */
    } else /* otherwise code new instruction */
        return csC_emitIL(fs, op, n);
}


static int codenil(FunctionState *fs, int n) {
    return adjuststack(fs, OP_NIL, n);
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


/* pop values from stack */
int csC_remove(FunctionState *fs, int n) {
    if (n > 0) return adjuststack(fs, OP_POP, n);
    return -1; /* nothing to remove */
}


/* pop values from stack and free compiler stack slots */
int csC_pop(FunctionState *fs, int n) {
    freeslots(fs, n);
    return csC_remove(fs, n);
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
    c_uint x = check_exp(imm < 0 && MIN_ARG_L <= imm, c_abs(imm));
    cs_assert(!(x & 0x800000)); /* 24th bit must be free */
    return (x|0x800000); /* set 24th bit */
}


/*
** Encode value as immediate operand.
*/
static int encodeimm(int imm) {
    cs_assert(isIMM(imm) || isIMML(imm)); /* must fit */
    if (imm < 0) { /* is negative (must be encoded)? */
        if (imm >= MIN_IMM)
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
    c_uint imm = check_exp(v->et == EXP_INDEXINT, encodeimm(v->u.info));
    if (isIMM(v->u.info))
        return csC_emitILS(fs, OP_SETINDEXINT, left, imm);
    else
        return csC_emitILL(fs, OP_SETINDEXINTL, left, imm);
}


/*
** Store value on top of the stack into 'var'.
** 'left' represents leftover values from other expressions in the
** assignment statement, this is needed to properly locate variable
** we are storing.
*/
int csC_storevar(FunctionState *fs, ExpInfo *var, int left) {
    int extra = 0; /* extra leftover values */
    switch (var->et) {
        case EXP_UVAL: {
            var->u.info = csC_emitIL(fs, OP_SETUVAL, var->u.info);
            break;
        }
        case EXP_LOCAL: {
            var->u.info = csC_emitIL(fs, OP_SETLOCAL, var->u.var.sidx);
            break;
        }
        case EXP_INDEXED: {
            var->u.info = csC_emitIL(fs, OP_SETINDEX, left+2);
            extra = 2;
            break;
        }
        case EXP_INDEXSTR: {
            var->u.info = csC_emitILL(fs, OP_SETINDEXSTR, left+1, var->u.info);
            extra = 1;
            break;
        }
        case EXP_INDEXINT: {
            var->u.info = setindexint(fs, var, left+1);
            extra = 1;
            break;
        }
        case EXP_DOT: {
            var->u.info = csC_emitILL(fs, OP_SETPROPERTY, left+1, var->u.info);
            extra = 1;
            break;
        }
        case EXP_INDEXSUPER:
        case EXP_INDEXSUPERSTR:
        case EXP_DOTSUPER: {
            csP_semerror(fs->lx, "can't assign to 'super' property");
            break;
        }
        default: cs_assert(0); break; /* invalid store */
    }
    var->et = EXP_FINEXPR;
    freeslots(fs, 1); /* 'exp' (value) */
    return extra;
}


void csC_storevarpop(FunctionState *fs, ExpInfo *var, int left) {
    csC_pop(fs, csC_storevar(fs, var, left));
}


static int getindexint(FunctionState *fs, ExpInfo *v) {
    c_uint imm = check_exp(v->et == EXP_INDEXINT, encodeimm(v->u.info));
    if (isIMM(v->u.info))
        return csC_emitIS(fs, OP_GETINDEXINT, imm);
    else
        return csC_emitIL(fs, OP_GETINDEXINTL, imm);
}


c_sinline int jumpoffset(Instruction *jmp) {
    int offset = GET_ARG_L(jmp, 0);
    cs_assert(*jmp == OP_JMP || *jmp == OP_JMPS);
    return (*jmp == OP_JMP) ? offset : -offset;
}


c_sinline int destinationpc(Instruction *inst, int pc) {
    return pc + getopSize(*inst) + jumpoffset(inst);
}


/*
** Gets the destination address of a jump instruction.
** Used to traverse a list of jumps.
*/
static int getjump(FunctionState *fs, int pc) {
    Instruction *inst = &fs->p->code[pc];
    int offset = GET_ARG_L(inst, 0);
    if (offset == 0) /* no offset represents end of the list */
        return NOJMP; /* end of the list */
    else
        return destinationpc(inst, pc);
}


/* fix jmp instruction at 'pc' to jump to 'target' */
static void fixjump(FunctionState *fs, int pc, int target) {
    Instruction *jmp = &fs->p->code[pc];
    int offset = c_abs(target - (pc + getopSize(*jmp)));
    cs_assert(*jmp == OP_JMP || *jmp == OP_JMPS);
    if (c_unlikely(offset > MAXJMP)) /* jump is too large? */
        csP_semerror(fs->lx, "control structure too long");
    SET_ARG_L(jmp, 0, offset); /* fix the jump */
}


/* concatenate jump list 'l2' into jump list 'l1' */
void csC_concatjl(FunctionState *fs, int *l1, int l2) {
    if (l2 == NOJMP) return;
    if (*l1 == NOJMP) *l1 = l2;
    else {
        int list = *l1;
        int next;
        while ((next = getjump(fs, list)) != NOJMP) /* get last jump pc */
            list = next;
        fixjump(fs, list, l2); /* last jump jumps to 'l2' */
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
void csC_const2v(FunctionState *fs, ExpInfo *e, TValue *v) {
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


/*
** Ensure expression 'v' is not a variable.
** This additionally reserves stack slot (if one is needed).
** (Expressions may still have jump lists.)
*/
int csC_dischargevars(FunctionState *fs, ExpInfo *v) {
    switch (v->et) {
        case EXP_UVAL: {
            v->u.info = csC_emitIL(fs, OP_GETUVAL, v->u.info);
            break;
        }
        case EXP_LOCAL: {
            v->u.info = csC_emitIL(fs, OP_GETLOCAL, v->u.var.sidx);
            break;
        }
        case EXP_INDEXED: {
            freeslots(fs, 2);
            v->u.info = csC_emitI(fs, OP_GETINDEX);
            break;
        }
        case EXP_INDEXSTR: {
            freeslots(fs, 1);
            v->u.info = csC_emitIL(fs, OP_GETINDEXSTR, v->u.info);
            break;
        }
        case EXP_INDEXINT: {
            freeslots(fs, 1);
            v->u.info = getindexint(fs, v);
            break;
        }
        case EXP_INDEXSUPER: {
            freeslots(fs, 2);
            v->u.info = csC_emitI(fs, OP_GETSUPIDX);
            break;
        }
        case EXP_INDEXSUPERSTR: {
            freeslots(fs, 1);
            v->u.info = csC_emitIL(fs, OP_GETSUPIDXSTR, v->u.info);
            break;
        }
        case EXP_DOT: {
            freeslots(fs, 1);
            v->u.info = csC_emitIL(fs, OP_GETPROPERTY, v->u.info);
            break;
        }
        case EXP_DOTSUPER: {
            freeslots(fs, 1);
            v->u.info = csC_emitIL(fs, OP_GETSUP, v->u.info);
            break;
        }
        case EXP_CALL: case EXP_VARARG: {
            csC_setreturns(fs, v, 1); /* default is one value returned */
            return 1; /* done */
        }
        default: return 0; /* expression is not a variable */
    }
    csC_reserveslots(fs, 1);
    v->et = EXP_FINEXPR;
    return 1;
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
    SET_ARG_S(inst, 0, lsz); /* set size (log2 - 1) */
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
    fs->sp = base + 1; /* free stack slots */
}


void csC_settablesize(FunctionState *fs, int pc, int hsize) {
    Instruction *inst = &fs->p->code[pc];
    hsize = (hsize != 0 ? csO_ceillog2(hsize) + 1 : 0);
    cs_assert(hsize <= MAX_ARG_S);
    SET_ARG_S(inst, 0, hsize);
}


/*
** Ensure expression 'e' is on top of the stack, making 'e'
** a finalized expression.
** This additionally reserves stack slot (if one is needed).
** (Expressions may still have jump lists.)
*/
static void discharge2stack(FunctionState *fs, ExpInfo *e) {
    if (!csC_dischargevars(fs, e)) {
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
        csC_reserveslots(fs, 1);
        e->et = EXP_FINEXPR;
    }
}


/*
** Ensures final expression result is on stop of the stack.
** If expression has jumps, need to patch these jumps to its
** final position.
*/
void csC_exp2stack(FunctionState *fs, ExpInfo *e) {
    discharge2stack(fs, e);
    if (hasjumps(e)) {
        int final = currPC; /* position after the expression */
        patchlistaux(fs, e->f, final);
        patchlistaux(fs, e->t, final);
        e->f = e->t = NOJMP;
    }
    cs_assert(e->f == NOJMP && e->t == NOJMP);
    cs_assert(e->et == EXP_FINEXPR);
}


/*
** Ensures final expression result is either on stack
** or it is a constant.
*/
void csC_exp2val(FunctionState *fs, ExpInfo *e) {
    if (hasjumps(e))
        csC_exp2stack(fs, e);
    else
        csC_dischargevars(fs, e);
}


/*
** Initialize '.' indexed expression.
** If 'super' is true, then this indexing is considered as
** indexing 'super' (superclass).
*/
void csC_getfield(FunctionState *fs, ExpInfo *v, ExpInfo *key, int super) {
    cs_assert(v->et == EXP_FINEXPR); /* 'v' must be on stack */
    v->u.info = stringK(fs, key->u.str);
    v->et = (super ? EXP_DOTSUPER : EXP_DOT);
}


/* 
** Initialize '[]' indexed expression.
** If 'super' is true, then this indexing is considered as
** indexing 'super' (superclass).
*/
void csC_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key, int super) {
    int strK = 0;
    cs_assert(var->et == EXP_FINEXPR); /* 'var' must be finalized (on stack) */
    csC_exp2val(fs, key);
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
** Return false if folding can raise an error.
** Bitwise operations need operands convertible to integers; division
** operations cannot have 0 as divisor.
*/
static int validop(TValue *v1, TValue *v2, int op) {
    switch (op) {
        case CS_OP_BSHR: case CS_OP_BSHL: case CS_OP_BAND:
        case CS_OP_BOR: case CS_OP_BXOR: case CS_OP_BNOT: { /* conversion */
            cs_Integer i;
            return (csO_tointeger(v1, &i, N2IEXACT) &&
                    csO_tointeger(v2, &i, N2IEXACT));
        }
        case CS_OP_DIV: case CS_OP_IDIV: case CS_OP_MOD: /* division by 0 */
            return (nval(v2) != 0);
        default: return 1; /* everything else is valid */
    }
}


/*
** Check if expression is numeral constant, and if
** so, set the value into 'res'.
*/
static int tonumeral(const ExpInfo *e1, TValue *res) {
    switch (e1->et) {
        case EXP_FLT: if (res) setfval(res, e1->u.n); return 1;
        case EXP_INT: if (res) setival(res, e1->u.i); return 1;
        default: return 0;
    }
}


/*
** Try to "constant-fold" an operation; return 1 if successful.
** (In this case, 'e1' has the final result.)
*/
static int constfold(FunctionState *fs, ExpInfo *e1, const ExpInfo *e2,
                     int op) {
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


/*
** Emit code for unary expressions that "produce values"
** (everything but '!').
*/
static void codeunary(FunctionState *fs, ExpInfo *e, OpCode op, int line) {
    csC_exp2stack(fs, e);
    e->u.info = csC_emitI(fs, op);
    e->et = EXP_FINEXPR;
    csC_fixline(fs, line);
}


/*
** Code '!e', doing constant folding.
*/
static void codenot(FunctionState *fs, ExpInfo *e) {
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
        case EXP_FINEXPR: {
            csC_exp2stack(fs, e);
            e->u.info = csC_emitI(fs, OP_NOT);
            break;
        }
        default: cs_assert(0); /* vars should already be discharged */
    }
    /* interchange true and false lists */
    { int temp = e->f; e->f = e->t; e->t = temp; }
}


/*
** Apply prefix operation 'uopr' to expression 'e'.
*/
void csC_unary(FunctionState *fs, ExpInfo *e, Unopr uopr, int line) {
    static const ExpInfo dummy = {EXP_INT, {0}, NOJMP, NOJMP};
    cs_assert(0 <= uopr && uopr < OPR_NOUNOPR);
    csC_dischargevars(fs, e);
    switch (uopr) {
        case OPR_UNM: case OPR_BNOT: {
            if (constfold(fs, e, &dummy, (uopr - OPR_UNM) + CS_OP_UNM))
                break; /* folded */
            codeunary(fs, e, unopr2op(uopr), line);
            break;
        }
        case OPR_NOT:  {
            codenot(fs, e); 
            break;
        }
        default: cs_assert(0); /* invalid unary operation */
    }
}


/* code test/jump instruction */
int csC_jmp(FunctionState *fs, OpCode opjump) {
    cs_assert(opjump == OP_JMP || opjump == OP_JMPS);
    return csC_emitIL(fs, opjump, 0);
}


int csC_test(FunctionState *fs, OpCode optest, int cond, int line) {
    int pcjump;
    cs_assert(optest == OP_TEST || optest == OP_TESTPOP);
    if (optest == OP_TESTPOP)
        freeslots(fs, 1); /* this test pops one value */
    csC_emitIS(fs, optest, cond); /* emit condition test... */
    csC_fixline(fs, line);
    pcjump = csC_jmp(fs, OP_JMP); /* ...followed by a jump */
    csC_fixline(fs, line);
    return pcjump;
}


/* code and/or logical operators */
static int codeAndOr(FunctionState *fs, ExpInfo *e, int cond, int line) {
    int test;
    discharge2stack(fs, e); /* ensure test operand is on the stack */
    test = csC_test(fs, OP_TEST, cond, line); /* emit test */
    csC_pop(fs, 1); /* if it goes through, pop the previous value */
    return test;
}


static void patchexplist(FunctionState *fs, int *l, int target) {
    if (*l != NOJMP) {
        csC_patch(fs, *l, target);
        *l = NOJMP; /* mark 'e->t' or 'e->f' as empty */
    }
}


/* 
** Insert new jump into 'e' false list.
** This test jumps over the second expression if the first expression
** is false (nil or false).
*/
static void codeand(FunctionState *fs, ExpInfo *e, int line) {
    int pc, target;
    switch (e->et) {
        case EXP_TRUE: case EXP_STRING: case EXP_INT:
        case EXP_FLT: case EXP_K: {
            pc = NOJMP; /* don't jump, always true */
            target = currPC;
            break;
        }
        default: {
            pc = codeAndOr(fs, e, 0, line); /* jump if false */
            target = fs->prevpc; /* POP */
        }
    }
    csC_concatjl(fs, &e->f, pc); /* insert new jump in false list */
    patchexplist(fs, &e->t, target); /* patch true list */
}


/* 
** Insert new jump into 'e' true list.
** This test jumps over the second expression if the first expression
** is true (everything else except nil and false).
*/
void codeor(FunctionState *fs, ExpInfo *e, int line) {
    int pc, target;
    switch (e->et) {
        case EXP_NIL: case EXP_FALSE: {
            pc = NOJMP; /* don't jump, always false */
            target = currPC;
            break;
        }
        default: {
            pc = codeAndOr(fs, e, 1, line); /* jump if true */
            target = fs->prevpc; /* POP */
        }
    }
    csC_concatjl(fs, &e->t, pc); /* insert new jump in true list */
    patchexplist(fs, &e->f, target);
}


void csC_prebinary(FunctionState *fs, ExpInfo *e, Binopr op, int line) {
    switch (op) {
        case OPR_ADD: case OPR_SUB: case OPR_MUL:
        case OPR_DIV: case OPR_IDIV: case OPR_MOD:
        case OPR_POW: case OPR_SHL: case OPR_SHR:
        case OPR_BAND: case OPR_BOR: case OPR_BXOR:
        case OPR_NE: case OPR_EQ: {
            if (!tonumeral(e, NULL))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral, which may be folded or used as an
               immediate operand or for a different variant of instruction */
            break;
        }
        case OPR_GT: case OPR_GE:
        case OPR_LT: case OPR_LE: {
            int dummy;
            if (!isnumIK(e, &dummy))
                csC_exp2stack(fs, e);
            /* otherwise keep numeral, which may be used as an immediate
               operand or for a different variant of instruction */
            break;
        }
        case OPR_CONCAT: {
            csC_exp2stack(fs, e); /* operand must be on stack */
            break;
        }
        case OPR_AND: {
            codeand(fs, e, line); /* jump out if 'e' is false */
            break;
        }
        case OPR_OR: {
            codeor(fs, e, line); /* jump out if 'e' is true */
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


/*
** Code generic binary instruction followed by meta binary instruction,
** in case generic binary instruction fails.
*/
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


/*
** Emit code for equality comparisons ('==', '!=').
*/
static void codeEq(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr) {
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


/*
** Emit code for order comparisons.
** 'swapped' tells whether ordering transform was performed
** (see 'csC_binary'), in order to swap the stack values at runtime
** to perform the ordering correctly (this is a limitation of stack-based VM).
*/
static void codeorder(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                      Binopr opr, int swapped) {
    OpCode op;
    int imm;
    cs_assert(OPR_LT == opr || OPR_LE == opr); /* already swapped */
    if (isnumIK(e2, &imm)) {
        /* use immediate operand */
        csC_exp2stack(fs, e1);
        op = binopr2op(opr, OPR_LT, OP_LTI);
    } else if (isnumIK(e1, &imm)) {
        /* transform (A < B) to (B > A) and (A <= B) to (B >= A) */
        csC_exp2stack(fs, e2);
        op = binopr2op(opr, OPR_LT, OP_GTI);
    } else { /* regular case, compare two stack values */
        int swap = 0;
        if (!swapped)
            swap = (e1->et != EXP_FINEXPR && e2->et == EXP_FINEXPR);
        else if (e2->et == EXP_FINEXPR)
            swap = 1;
        else if (e1->et == EXP_FINEXPR && e2->et != EXP_FINEXPR)
            swap = 0;
        csC_exp2stack(fs, e1);
        csC_exp2stack(fs, e2);
        op = binopr2op(opr, OPR_LT, OP_LT);
        e1->u.info = csC_emitIS(fs, op, swap);
        freeslots(fs, 1); /* one stack value is removed */
        goto l_fin;
    }
    e1->u.info = csC_emitIL(fs, op, imm);
l_fin:
    e1->et = EXP_FINEXPR;
}


static Instruction *previousinstruction(FunctionState *fs) {
    return &fs->p->code[fs->prevpc];
}


static void codeconcat(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, int line) {
    Instruction *inst = previousinstruction(fs);
    UNUSED(e2);
    if (*inst == OP_CONCAT) { /* 'e2' is a concatenation? */
        int n = GET_ARG_L(inst, 0);
        SET_ARG_L(inst, 0, n + 1); /* will concatenate one more element */
    } else { /* 'e2' is not a concatenation */
        e1->u.info = csC_emitIL(fs, OP_CONCAT, 2);
        e1->et = EXP_FINEXPR;
        csC_fixline(fs, line);
    }
    freeslots(fs, 1);
}


static int codeaddnegI(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
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


/*
** Finalize code for binary operations, after reading 2nd operand.
*/
void csC_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                                                             int line) {
    int swapped = 0;
    if (oprisfoldable(opr) && constfold(fs, e1, e2, opr + CS_OP_ADD))
        return; /* done (folded) */
    switch (opr) {
        case OPR_ADD: case OPR_MUL:
        case OPR_BAND: case OPR_BOR: case OPR_BXOR: {
            codecommutative(fs, e1, e2, opr, line);
            break;
        }
        case OPR_SUB: {
            if (codeaddnegI(fs, e1, e2, line))
                break; /* coded as (r1 + -I) */
        } /* fall through */
        case OPR_SHL: case OPR_SHR: case OPR_IDIV:
        case OPR_DIV: case OPR_MOD: case OPR_POW: {
            csC_dischargevars(fs, e2);
            codebinIK(fs, e1, e2, opr, 0, 0, line);
            break;
        }
        case OPR_CONCAT: {
            csC_exp2stack(fs, e2); /* second operand must be on stack */
            codeconcat(fs, e1, e2, line);
            break;
        }
        case OPR_NE: case OPR_EQ: {
            codeEq(fs, e1, e2, opr);
            break;
        }
        case OPR_GT: case OPR_GE: {
            /* 'a > b' <==> 'a < b', 'a >= b' <==> 'a <= b' */
            csC_dischargevars(fs, e1);
            csC_dischargevars(fs, e2);
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
            csC_dischargevars(fs, e2);
            csC_concatjl(fs, &e2->f, e1->f);
            *e1 = *e2;
            break;
        }
        case OPR_OR: {
            cs_assert(e1->f == NOJMP); /* list closed by 'csC_prebinary' */
            csC_dischargevars(fs, e2);
            csC_concatjl(fs, &e2->t, e1->t);
            *e1 = *e2;
            break;
        }
        default: cs_assert(0);
    }
}


/* return the final target of a jump (skipping jumps to jumps) */
static int finaltarget(Instruction *code, int i) {
    cs_assert(getopSize(OP_JMP) == getopSize(OP_JMPS));
    for (int count = 0; count < 100; count++) { /* avoid infinite loops */
        Instruction *pc = &code[i];
        if (*pc == OP_JMP || *pc == OP_JMPS)
            i = destinationpc(pc, i);
        else
            break; /* no more jumps */
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
    for (int i = 0; i < currPC; i += getopSize(*pc)) {
        pc = &p->code[i];
        switch (*pc) {
            case OP_RET: { /* check if need to close variables */
                if (fs->needclose)
                    SET_ARG_LLS(pc, 1); /* set the flag */
                break;
            }
            case OP_JMP: case OP_JMPS: { /* avoid jumps to jumps */
                int target = finaltarget(p->code, i);
                if (*pc == OP_JMP && target < i)
                    *pc = OP_JMPS; /* jumps back */
                else if (*pc == OP_JMPS && i < target)
                    *pc = OP_JMP; /* jumps forward */
                cs_assert(target >= 0);
                fixjump(fs, i, target);
                break;
            }
            default: break;
        }
    }
}
