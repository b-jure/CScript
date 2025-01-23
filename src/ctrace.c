/*
** ctrace.c
** Low-level bytecode tracing
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include <stdio.h>
#include <string.h>

#include "ctrace.h"
#include "cmeta.h"
#include "ccode.h"
#include "cdebug.h"
#include "climits.h"
#include "cobject.h"
#include "cstring.h"


#define getIMM(pc, i) \
    ((i) = GETARG_L(pc, 0), (i) *= (GETARG_S(pc, SIZEARGL) - 1))


#define TABSZ   3
#define postab(e) \
    { e; for(int i_ = 0; i_ < TABSZ; i_++) putchar(' '); }



static void startline(const Proto *p, const Instruction *pc) {
    int relpc = pc - p->code;
    postab(printf("[LINE %4d][PC %4d]", csD_getfuncline(p, relpc), relpc));
    fflush(stdout);
}


static void endline(void) {
    putchar('\n');
    fflush(stdout);
}


static int traceOp(OpCode op) {
    postab(printf("%-12s", getOpName(op)));
    fflush(stdout);
    return SIZEINSTR;
}


static int traceS(int s) {
    postab(printf("ArgS=%-8d", s));
    fflush(stdout);
    return SIZEARGS;
}


static int traceL(const Instruction *pc) {
    postab(printf("ArgL=%-8d", get3bytes(pc)));
    fflush(stdout);
    return SIZEARGL;
}


static int traceI(const Proto *p, const Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    endline();
    return SIZEINSTR;
}


static void traceIS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    traceS(*pc);
    endline();
}


static void traceISS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    pc += traceS(*pc);
    traceS(*pc);
    endline();
}


static void traceIL(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    traceL(pc);
    endline();
}


static void traceILS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    pc += traceL(pc);
    traceS(*pc);
    endline();
}


static void traceILSS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    pc += traceL(pc);
    pc += traceS(*pc);
    traceS(*pc);
    endline();
}


static void traceILL(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


static void traceILLL(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    pc += traceL(pc);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


static void traceILLS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceOp(op);
    pc += traceL(pc);
    pc += traceL(pc);
    traceS(*pc);
    endline();
}


/*
** Trace the current OpCode and its arguments.
*/
void csTR_tracepc(cs_State *ts, const Proto *p, const Instruction *pc) {
    csTR_dumpstack(ts, 1, NULL); /* first dump current function stack... */
    switch (getOpFormat(*pc)) { /* ...then trace the instruction */
        case FormatI: traceI(p, pc); break;
        case FormatIS: traceIS(p, pc); break;
        case FormatISS: traceISS(p, pc); break;
        case FormatIL: traceIL(p, pc); break;
        case FormatILS: traceILS(p, pc); break;
        case FormatILSS: traceILSS(p, pc); break;
        case FormatILL: traceILL(p, pc); break;
        case FormatILLS: traceILLS(p, pc); break;
        case FormatILLL: traceILLL(p, pc); break;
        default: cs_assert(0 && "invalid OpCode format"); break;
    }
}


#define nextOp(pc)      ((pc) + getOpSize(*pc))


static void traceNil(void) {
    printf("nil");
}


static void traceTrue(void) {
    printf("true");
}


static void traceFalse(void) {
    printf("false");
}


/* maximum length for string constants */
#define MAXSTRKLEN      25

static void traceString(OString *s) {
    char buff[MAXSTRKLEN + 1];
    csS_strlimit(buff, getstr(s), getstrlen(s), sizeof(buff));
    printf("\"%s\"", buff);
}


static void traceNumber(const TValue *o) {
    printf("%s", csS_numtostr(o, NULL));
}


static void traceValue(const TValue *o) {
    switch (ttypetag(o)) {
        case CS_VNIL: traceNil(); break;
        case CS_VTRUE: traceTrue(); break;
        case CS_VFALSE: traceFalse(); break;
        case CS_VSHRSTR: case CS_VLNGSTR: traceString(strval(o)); break;
        case CS_VNUMINT: case CS_VNUMFLT: traceNumber(o); break;
        default: cs_assert(0 && "invalid 'o' type"); break;
    }
    fflush(stdout);
}


static void traceK(const Proto *p, int index) {
    postab((printf("K@%d=", index), traceValue(&p->k[index])));
    fflush(stdout);
}


static void unasm(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    endline();
}


static void unasmL(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceOp(*pc);
    traceL(pc);
    endline();
}


static void unasmLS(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceOp(*pc);
    pc += traceL(pc);
    traceS(*pc);
    endline();
}


static void unasmLL(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceOp(*pc);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


static void unasmLLL(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceOp(*pc);
    pc += traceL(pc);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


static void unasmKL(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceK(p, GETARG_L(pc, 0));
    endline();
}


static void unasmK(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceK(p, GETARG_S(pc, 0));
    endline();
}


static void unasmIMMint(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Integer i;
    startline(p, pc);
    traceOp(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(traceNumber(&aux));
    endline();
}


static void unasmIMMflt(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Number n;
    startline(p, pc);
    traceOp(*pc);
    getIMM(pc, n);
    setfval(&aux, n);
    postab(traceNumber(&aux));
    endline();
}


static void traceSize(int size) {
    if (size > 0) size = 1 << (size - 1);
    postab(printf("size=%d", size));
    fflush(stdout);
}


static void unasmNewObject(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceSize(GETARG_S(pc, 0));
    endline();
}


static void unasmEQK(const Proto *p, Instruction *pc) {
    TValue aux;
    startline(p, pc);
    traceOp(*pc);
    traceK(p, GETARG_L(pc, 0));
    setival(&aux, GETARG_S(pc, SIZEARGL));
    postab(traceNumber(&aux));
    endline();
}


static void traceCond(int cond) {
    postab(printf("%s", (cond ? "equal" : "not equal")));
    fflush(stdout);
}


static void unasmEQI(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Integer i;
    startline(p, pc);
    traceOp(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(traceNumber(&aux));
    traceCond(GETARG_S(pc, SIZEARGL));
    endline();
}


static void unasmIMMord(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Integer i;
    startline(p, pc);
    traceOp(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(traceNumber(&aux));
    endline();
}


static void traceNparams(int nparams) {
    postab(printf("nparams=%d", nparams));
    fflush(stdout);
}


static void unasmVarargPrep(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceNparams(GETARG_L(pc, 0));
    endline();
}


static void unasmS(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceS(GETARG_S(pc, 0));
    endline();
}


static void traceStackSlot(int index) {
    postab(printf("S@%d", index));
    fflush(stdout);
}


static void traceNres(int nres) {
    const char *res = NULL;
    if (nres < 0)
        res = "multiple";
    else if (nres == 0)
        res = "none";
    if (res) {
        postab(printf("nres=%s", res));
    } else
        postab(printf("nres=%d", nres));
}


static void unasmCall(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceStackSlot(GETARG_L(pc, 0));
    traceNres(GETARG_L(pc, 1) - 1);
    endline();
}


static void traceMetaName(cs_State *ts, cs_MM mm) {
    postab(printf("%s", getstr(G_(ts)->mmnames[mm])));
    fflush(stdout);
}


static void unasmMM(cs_State *ts, const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceMetaName(ts, GETARG_S(pc, 0));
    endline();
}


static void unasmIndexedSet(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceStackSlot(GETARG_L(pc, 0));
    traceK(p, GETARG_L(pc, 1));
    endline();
}


static void traceGlobal(TValue *k, int index) {
    const char *str = getstr(strval(&k[index]));
    postab(printf("G@%s", str));
    fflush(stdout);
}


static void unasmGlobal(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceGlobal(p->k, GETARG_L(pc, 0));
    endline();
}


static void traceLocal(int index) {
    postab(printf("L@%d", index));
    fflush(stdout);
}


static void unasmLocal(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceLocal(GETARG_L(pc, 0));
    endline();
}


static void traceUpVal(UpValInfo *uv, int index) {
    const char *str = getstr(uv[index].name);
    postab(printf("U@%d=%s", index, str));
    fflush(stdout);
}


static void unasmUpvalue(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceUpVal(p->upvals, GETARG_L(pc, 0));
    endline();
}


static void traceOffset(int off) {
    postab(printf("offset=%d", off));
    fflush(stdout);
}


static void traceClose(int close) {
    postab(printf("close=%s", close ? "true" : "false"));
    fflush(stdout);
}


static void traceNpop(int npop) {
    postab(printf("npop=%d", npop));
    fflush(stdout);
}


static void unasmPop(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    if (*pc == OP_POP)
        traceNpop(1);
    else
        traceNpop(GETARG_L(pc, 0));
    endline();
}


static void unasmJmp(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceOffset(GETARG_L(pc, 0));
    endline();
}


static void unasmBreakJmp(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceOffset(GETARG_L(pc, 0));
    traceNpop(GETARG_L(pc, 1));
    endline();
}


static void unasmRet(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceOp(*pc);
    traceStackSlot(GETARG_L(pc, 0));
    traceNres(GETARG_L(pc, 1) - 1);
    traceClose(GETARG_S(pc, (2*SIZEARGL)));
    endline();
}


/*
** Disassemble all of the bytecode in 'p->code'.
** This function provides more detailed semantic information compared
** to 'csTR_trace' when tracing OpCode and its arguments.
*/
void csTR_disassemble(cs_State *ts, const Proto *p) {
    Instruction *pc = p->code;
    if (p->defline == 0)
        printf("%s {\n", getstr(p->source));
    else
        printf("fn at line %d in %s {\n", p->defline, getstr(p->source));
    fflush(stdout);
    while (pc < &p->code[p->sizecode]) {
        printf("    ");
        fflush(stdout);
        switch (*pc) {
            case OP_TRUE: case OP_FALSE: case OP_NIL:
            case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV:
            case OP_MOD: case OP_POW: case OP_BSHL: case OP_BSHR:
            case OP_BAND: case OP_BOR: case OP_BXOR:case OP_LT:
            case OP_LE: case OP_NOT: case OP_UNM: case OP_BNOT:
            case OP_EQPRESERVE: case OP_GETINDEX: case OP_GETSUPIDX:
            case OP_INHERIT: {
                unasm(p, pc);
                break;
            }
            case OP_POP: case OP_POPN: {
                unasmPop(p, pc);
                break;
            }
            case OP_JMP: case OP_JMPS: {
                unasmJmp(p, pc);
                break;
            }
            case OP_NILN: case OP_VARARG: case OP_CLOSURE:
            case OP_CLOSE: case OP_TBC: case OP_CONCAT:
            case OP_GETINDEXINT: case OP_SETINDEX: {
                unasmL(p, pc);
                break;
            }
            case OP_ADDK: case OP_SUBK: case OP_MULK: case OP_DIVK:
            case OP_MODK: case OP_POWK: case OP_BSHLK: case OP_BSHRK:
            case OP_BANDK: case OP_BORK: case OP_BXORK: case OP_CONSTL:
            case OP_GETPROPERTY: case OP_GETINDEXSTR: case OP_METHOD:
            case OP_GETSUP: case OP_GETSUPIDXSTR: {
                unasmKL(p, pc);
                break;
            }
            case OP_ADDI: case OP_SUBI: case OP_MULI:
            case OP_DIVI: case OP_MODI: case OP_POWI:
            case OP_BSHLI: case OP_BSHRI: case OP_BANDI:
            case OP_BORI: case OP_BXORI: {
                unasmIMMint(p, pc);
                break;
            }
            case OP_NEWCLASS: case OP_NEWARRAY: case OP_NEWTABLE: {
                unasmNewObject(p, pc);
                break;
            }
            case OP_LTI: case OP_LEI: case OP_GTI: case OP_GEI: {
                unasmIMMord(p, pc);
                break;
            }
            case OP_VARARGPREP: {
                unasmVarargPrep(p, pc);
                break;
            }
            case OP_GETGLOBAL: case OP_SETGLOBAL: {
                unasmGlobal(p, pc);
                break;
            }
            case OP_GETLOCAL: case OP_SETLOCAL: {
                unasmLocal(p, pc);
                break;
            }
            case OP_GETUVAL: case OP_SETUVAL: {
                unasmUpvalue(p, pc);
                break;
            }
            case OP_FORPREP: case OP_FORCALL: {
                unasmLL(p, pc);
                break;
            }
            case OP_SETINDEXINT: case OP_SETPROPERTY: case OP_SETINDEXSTR: {
                unasmIndexedSet(p, pc);
                break;
            }
            case OP_TEST: case OP_TESTORPOP: case OP_TESTANDPOP:
            case OP_TESTPOP: case OP_SETARRAY: {
                unasmLS(p, pc);
                break;
            }
            case OP_MBIN: case OP_SETMM: {
                unasmMM(ts, p, pc);
                break;
            }
            case OP_FORLOOP: unasmLLL(p, pc); break;
            case OP_CONST: unasmK(p, pc); break;
            case OP_CONSTI: unasmIMMint(p, pc); break;
            case OP_CONSTF: unasmIMMflt(p, pc); break;
            case OP_BJMP: unasmBreakJmp(p, pc); break;
            case OP_EQK: unasmEQK(p, pc); break;
            case OP_EQI: unasmEQI(p, pc); break;
            case OP_EQ: unasmS(p, pc); break;
            case OP_CALL: unasmCall(p, pc); break;
            case OP_RET: unasmRet(p, pc); break;
            default: cs_assert(0 && "invalid OpCode"); break;
        }
        pc = nextOp(pc);
    }
    printf("}\n");
    fflush(stdout);
}


void csTR_dumpstack(cs_State *ts, int level, const char *fmt, ...) {
    CallFrame *cf = ts->cf;
    SPtr prevtop = ts->sp.p;
    if (fmt) {
        va_list ap;
        va_start(ap, fmt);
        vprintf(fmt, ap);
        va_end(ap);
        printf("\n");
    }
    for (int i = 0; cf != NULL && level != 0; i++) {
        level--;
        printf("[LEVEL %3d] %-10s %s ", i, typename(ttype(s2v(cf->func.p))),
                                        cf != ts->cf ? "--" : ">>");
        if (cf->func.p + 1 >= prevtop)
            printf("empty");
        else
            for (SPtr sp = cf->func.p + 1; sp < prevtop; sp++)
                printf("[%s]", typename(ttype(s2v(sp))));
        printf("\n");
        prevtop = cf->func.p;
        cf = cf->prev;
    }
    fflush(stdout);
}
