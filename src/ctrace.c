/*
** ctrace.c
** Low-level bytecode tracing
** See Copyright Notice in cscript.h
*/


#include "cmeta.h"
#define CS_CORE


#include <stdio.h>
#include <string.h>

#include "ctrace.h"
#include "ccode.h"
#include "cdebug.h"
#include "climits.h"
#include "cobject.h"
#include "cstring.h"


#define getIMM(pc, i) \
    ((i) = GETARG_L(pc, 0), (i) *= GETARG_S(pc, SIZEARGL))


/* common signature for tracing functions */
#define TRACEDEF(name) \
    static void trace##name (const Proto *p, const Instruction *pc)


/* common signature for disassembly functions */
#define UNASMDEF(name) \
    static void unasm##name (cs_State *ts, const Proto *p, Instruction *pc)


#define TABSZ   3
#define postab(e) \
    { e; for(int i_ = 0; i_ < TABSZ; i_++) putchar(' '); }


cs_sinline void startline(const Proto *p, const Instruction *pc) {
    postab(printf("[%05d]", csD_getfuncline(p, pcrel(pc, p))));
}


cs_sinline void endline(void) {
    putchar('\n');
    fflush(stdout);
}


cs_sinline int traceop(OpCode op) {
    postab(printf("%s", getOpName(op)));
    return SIZEINSTR;
}


cs_sinline int traceS(const Instruction *pc) {
    postab(printf("S=%d", GETARG_S(pc, 0)));
    return SIZEARGS;
}


cs_sinline int traceL(const Instruction *pc) {
    postab(printf("L=%d", GETARG_L(pc, 0)));
    return SIZEARGL;
}


TRACEDEF(I) {
    startline(p, pc);
    traceop(*pc);
    endline();
}


TRACEDEF(IS) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    traceS(pc);
    endline();
}


TRACEDEF(ISS) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    pc += traceS(pc);
    traceS(pc);
    endline();
}


TRACEDEF(IL) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    traceL(pc);
    endline();
}


TRACEDEF(ILS) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    pc += traceL(pc);
    traceS(pc);
    endline();
}


TRACEDEF(ILSS) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    pc += traceL(pc);
    pc += traceS(pc);
    traceS(pc);
    endline();
}


TRACEDEF(ILL) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


TRACEDEF(ILLS) {
    OpCode op = *pc;
    startline(p, pc);
    traceop(op);
    pc += traceL(pc);
    pc += traceL(pc);
    traceS(pc);
    endline();
}


/*
** Trace the current OpCode and its arguments.
*/
Instruction csTR_tracepc(const Proto *p, const Instruction *pc) {
    switch (getOpFormat(*pc)) {
        case FormatI: traceI(p, pc); break;
        case FormatIS: traceIS(p, pc); break;
        case FormatISS: traceISS(p, pc); break;
        case FormatIL: traceIL(p, pc); break;
        case FormatILS: traceILS(p, pc); break;
        case FormatILSS: traceILSS(p, pc); break;
        case FormatILL: traceILL(p, pc); break;
        case FormatILLS: traceILLS(p, pc); break;
        default: cs_assert(0 && "invalid OpCode format"); break;
    }
    return *pc++;
}


#define nextOp(pc)      ((pc) + getOpSize(*pc))


cs_sinline void tracenil(void) {
    printf("nil");
}


cs_sinline void tracetrue(void) {
    printf("true");
}


cs_sinline void tracefalse(void) {
    printf("false");
}


/* maximum length for string constants */
#define MAXSTRKLEN      20

cs_sinline void tracestring(OString *s) {
    char buff[MAXSTRKLEN + 1];
    csS_strlimit(buff, getstr(s), getstrlen(s), sizeof(buff));
    printf("%s", buff);
}


cs_sinline void tracenum(const TValue *o) {
    printf("%s", csS_numtostr(o, NULL));
}


static void tracevalue(const TValue *o) {
    switch (ttypetag(o)) {
        case CS_VNIL: tracenil(); break;
        case CS_VTRUE: tracetrue(); break;
        case CS_VFALSE: tracefalse(); break;
        case CS_VSHRSTR: case CS_VLNGSTR: tracestring(strval(o)); break;
        case CS_VNUMINT: case CS_VNUMFLT: tracenum(o); break;
        default: cs_assert(0 && "invalid 'o' type"); break;
    }
}


static void traceK(const Proto *p, int index) {
    postab(printf("K[%d]=", index); tracevalue(&p->k[index]));
}


UNASMDEF() {
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    endline();
}


UNASMDEF(L) {
    UNUSED(ts);
    startline(p, pc);
    pc += traceop(*pc);
    traceL(pc);
    endline();
}


UNASMDEF(LS) {
    UNUSED(ts);
    startline(p, pc);
    pc += traceop(*pc);
    pc += traceL(pc);
    traceS(pc);
    endline();
}


UNASMDEF(LL) {
    UNUSED(ts);
    startline(p, pc);
    pc += traceop(*pc);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


UNASMDEF(KL) {
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    traceK(p, GETARG_L(pc, 0));
    endline();
}


UNASMDEF(K) {
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    traceK(p, GETARG_S(pc, 0));
    endline();
}


UNASMDEF(IMMint) {
    TValue aux;
    cs_Integer i;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(tracenum(&aux));
    endline();
}


UNASMDEF(IMMflt) {
    TValue aux;
    cs_Number n;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, n);
    setfval(&aux, n);
    postab(tracenum(&aux));
    endline();
}


UNASMDEF(EQK) {
    TValue aux;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    traceK(p, GETARG_L(pc, 0));
    setival(&aux, GETARG_S(pc, SIZEARGL));
    postab(tracenum(&aux));
    endline();
}


UNASMDEF(EQI) {
    TValue aux;
    cs_Integer i;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(tracenum(&aux));
    endline();
}


UNASMDEF(IMMord) {
    TValue aux;
    cs_Integer i;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(tracenum(&aux));
    endline();
}


UNASMDEF(S) {
    UNUSED(ts);
    startline(p, pc);
    pc += traceop(*pc);
    traceS(pc);
    endline();
}


UNASMDEF(Call) {
    int from, to;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    from = GETARG_L(pc, 0);
    to = GETARG_L(pc, 1);
    postab(printf("(STK@%d,...,STK@%d)", from, to)); 
    endline();
}


UNASMDEF(MM) {
    cs_MM mm;
    startline(p, pc);
    traceop(*pc);
    mm = GETARG_S(pc, 0);
    postab(printf("%s", getstr(G_(ts)->mmnames[mm])));
    endline();
}


UNASMDEF(MBin) {
    OpCode op;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    op = GETARG_S(pc, 0) + OP_ADD;
    postab(printf("%s", getOpName(op)));
    endline();
}


UNASMDEF(Local) {
    const char *str;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    str = getstr(p->locals[GETARG_L(pc, 0)].name);
    postab(printf("%s", str));
    endline();
}


UNASMDEF(Upvalue) {
    const char *str;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    str = getstr(p->upvals[GETARG_L(pc, 0)].name);
    postab(printf("%s", str));
    endline();
}


UNASMDEF(Ret) {
    int nres, nbase, close;
    UNUSED(ts);
    startline(p, pc);
    traceop(*pc);
    nbase = GETARG_L(pc, 0);
    nres = GETARG_L(pc, 1);
    close = GETARG_S(pc, (2*SIZEARGL));
    postab(printf("STK@%d", nbase));
    postab(printf("nres=%d", nres));
    postab(printf("close=%s", (close ? "yes" : "no")));
    endline();
}


/*
** Disassemble all of the bytecode in 'p->code'.
** This function provides more detailed semantic information compared
** to 'csTR_trace' when tracing OpCode and its arguments.
*/
void csTR_disassemble(cs_State *ts, const Proto *p) {
    Instruction *pc = p->code;
    for (int i = 0; i < p->sizecode; i++) {
        switch (*pc) {
            case OP_TRUE: case OP_FALSE: case OP_NIL: case OP_NEWCLASS:
            case OP_POP: case OP_ADD: case OP_SUB: case OP_MUL:
            case OP_DIV: case OP_MOD: case OP_POW: case OP_BSHL:
            case OP_BSHR: case OP_BAND: case OP_BOR: case OP_BXOR:
            case OP_CONCAT: case OP_LT: case OP_LE: case OP_NOT:
            case OP_UNM: case OP_BNOT: case OP_EQPRESERVE:
            case OP_GETINDEX: case OP_SETINDEX: case OP_GETSUPIDX:
            case OP_INHERIT: case OP_FORCALL: case OP_FORLOOP: {
                unasm(ts, p, pc);
                break;
            }
            case OP_NILN: case OP_VARARGPREP: case OP_VARARG:
            case OP_CLOSURE: case OP_POPN: case OP_JMP:
            case OP_JMPS: case OP_CLOSE: case OP_TBC:
            case OP_GETINDEXINT: case OP_SETINDEXINT: {
                unasmL(ts, p, pc);
                break;
            }
            case OP_ADDK: case OP_SUBK: case OP_MULK: case OP_DIVK:
            case OP_MODK: case OP_POWK: case OP_BSHLK: case OP_BSHRK:
            case OP_BANDK: case OP_BORK: case OP_BXORK: case OP_CONSTL:
            case OP_SETPROPERTY: case OP_GETPROPERTY: case OP_GETINDEXSTR:
            case OP_SETINDEXSTR: case OP_METHOD: case OP_GETSUP:
            case OP_GETSUPIDXSTR: {
                unasmKL(ts, p, pc);
                break;
            }
            case OP_ADDI: case OP_SUBI: case OP_MULI:
            case OP_DIVI: case OP_MODI: case OP_POWI:
            case OP_BSHLI: case OP_BSHRI: case OP_BANDI:
            case OP_BORI: case OP_BXORI: {
                unasmIMMint(ts, p, pc);
                break;
            }
            case OP_TEST: case OP_TESTORPOP: case OP_TESTANDPOP:
            case OP_TESTPOP: case OP_NEWARRAY: case OP_NEWTABLE: {
                unasmS(ts, p, pc);
                break;
            }
            case OP_LTI: case OP_LEI: case OP_GTI: case OP_GEI: {
                unasmIMMord(ts, p, pc);
                break;
            }
            case OP_GETLOCAL: case OP_SETLOCAL: {
                unasmLocal(ts, p, pc);
                break;
            }
            case OP_GETUVAL: case OP_SETUVAL: {
                unasmUpvalue(ts, p, pc);
                break;
            }
            case OP_FORPREP: {
                unasmLL(ts, p, pc);
                break;
            }
            case OP_SETARRAY: {
                unasmLS(ts, p, pc);
                break;
            }
            case OP_CONST: unasmK(ts, p, pc); break;
            case OP_CONSTI: unasmIMMint(ts, p, pc); break;
            case OP_CONSTF: unasmIMMflt(ts, p, pc); break;
            case OP_SETMM: unasmMM(ts, p, pc); break;
            case OP_MBIN: unasmMBin(ts, p, pc); break;
            case OP_EQK: unasmEQK(ts, p, pc); break;
            case OP_EQI: unasmEQI(ts, p, pc); break;
            case OP_EQ: unasmS(ts, p, pc); break;
            case OP_CALL: unasmCall(ts, p, pc); break;
            case OP_RET: unasmRet(ts, p, pc); break;
            default: cs_assert(0 && "invalid OpCode"); break;
        }
        pc = nextOp(pc);
    }
    fflush(stdout);
}


void csTR_dumpstack(cs_State *ts, const char *fmt, ...) {
    CallFrame *cf = ts->cf;
    SPtr prevtop = ts->sp.p;
    if (fmt) {
        va_list ap;
        va_start(ap, fmt);
        vprintf(fmt, ap);
        va_end(ap);
    }
    printf("\n");
    for (int i = 0; cf; i++) {
        printf("[%d] >> ", i);
        for (SPtr sp = cf->func.p; sp < prevtop; sp++)
            printf("[%s]", typename(ttypetag(s2v(sp))));
        printf("\n");
        prevtop = cf->func.p;
        cf = cf->prev;
    }
    fflush(stdout);
}
