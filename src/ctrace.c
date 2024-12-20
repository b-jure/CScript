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
    postab(printf("[LINE %4d]", csD_getfuncline(p, pc - p->code)));
}


static void endline(void) {
    putchar('\n');
    fflush(stdout);
}


static int traceop(OpCode op) {
    postab(printf("%-12s", getOpName(op)));
    return SIZEINSTR;
}


static int traceS(const Instruction *pc) {
    postab(printf("ArgS=%-8d", *pc));
    return SIZEARGS;
}


static int traceL(const Instruction *pc) {
    postab(printf("ArgL=%-8d", get3bytes(pc)));
    return SIZEARGL;
}


static int traceI(const Proto *p, const Instruction *pc) {
    startline(p, pc);
    traceop(*pc);
    endline();
    return SIZEINSTR;
}


static void traceIS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    traceS(pc);
    endline();
}


static void traceISS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    pc += traceS(pc);
    traceS(pc);
    endline();
}


static void traceIL(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    traceL(pc);
    endline();
}


static void traceILS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    pc += traceL(pc);
    traceS(pc);
    endline();
}


static void traceILSS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    pc += traceL(pc);
    pc += traceS(pc);
    traceS(pc);
    endline();
}


static void traceILL(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


static void traceILLS(const Proto *p, const Instruction *pc) {
    OpCode op = *pc;
    startline(p, pc);
    pc += traceop(op);
    pc += traceL(pc);
    pc += traceL(pc);
    traceS(pc);
    endline();
}


/*
** Trace the current OpCode and its arguments.
*/
void csTR_tracepc(cs_State *ts, const Proto *p, const Instruction *pc) {
    csTR_dumpstack(ts, 1, NULL); /* first dump current function stack... */
    printf("[PC %4ld]", pc - p->code);
    switch (getOpFormat(*pc)) { /* ...then trace the instruction */
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
}


#define nextOp(pc)      ((pc) + getOpSize(*pc))


static void tracenil(void) {
    printf("nil");
}


static void tracetrue(void) {
    printf("true");
}


static void tracefalse(void) {
    printf("false");
}


/* maximum length for string constants */
#define MAXSTRKLEN      20

static void tracestring(OString *s) {
    char buff[MAXSTRKLEN + 1];
    csS_strlimit(buff, getstr(s), getstrlen(s), sizeof(buff));
    printf("\"%s\"", buff);
}


static void tracenum(const TValue *o) {
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
    postab(printf("K@%d=", index); tracevalue(&p->k[index]));
}


static void unasm(const Proto *p, Instruction *pc) {
    startline(p, pc);
    traceop(*pc);
    endline();
}


static void unasmL(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceop(*pc);
    traceL(pc);
    endline();
}


static void unasmLS(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceop(*pc);
    pc += traceL(pc);
    traceS(pc);
    endline();
}


static void unasmLL(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceop(*pc);
    pc += traceL(pc);
    traceL(pc);
    endline();
}


static void unasmKL(const Proto *p, Instruction *pc) {
    int k = GETARG_L(pc, 0);
    startline(p, pc);
    traceop(*pc);
    traceK(p, k);
    endline();
}


static void unasmK(const Proto *p, Instruction *pc) {
    int k = GETARG_S(pc, 0);
    startline(p, pc);
    traceop(*pc);
    traceK(p, k);
    endline();
}


static void unasmIMMint(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Integer i;
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(tracenum(&aux));
    endline();
}


static void unasmIMMflt(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Number n;
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, n);
    setfval(&aux, n);
    postab(tracenum(&aux));
    endline();
}


static void unasmNewObject(const Proto *p, Instruction *pc) {
    int sz = GETARG_S(pc, 0);
    if (sz > 0) sz = 1 << (sz - 1);
    startline(p, pc);
    traceop(*pc);
    postab(printf("size=%d", sz));
    endline();
}


static void unasmEQK(const Proto *p, Instruction *pc) {
    TValue aux;
    startline(p, pc);
    traceop(*pc);
    traceK(p, GETARG_L(pc, 0));
    setival(&aux, GETARG_S(pc, SIZEARGL));
    postab(tracenum(&aux));
    endline();
}


static void unasmEQI(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Integer i;
    int cond;
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(tracenum(&aux));
    cond = *(pc + SIZEINSTR + SIZEARGL + SIZEARGS);
    postab(printf("%s", (cond ? "equal" : "not equal")));
    endline();
}


static void unasmIMMord(const Proto *p, Instruction *pc) {
    TValue aux;
    cs_Integer i;
    startline(p, pc);
    traceop(*pc);
    getIMM(pc, i);
    setival(&aux, i);
    postab(tracenum(&aux));
    endline();
}


static void unasmVarargPrep(const Proto *p, Instruction *pc) {
    int nparams = GETARG_L(pc, 0);
    startline(p, pc);
    traceop(*pc);
    postab(printf("nparams=%d", nparams));
    endline();
}


static void unasmS(const Proto *p, Instruction *pc) {
    startline(p, pc);
    pc += traceop(*pc);
    traceS(pc);
    endline();
}


static void traceSTK(int sindex) {
    postab(printf("S@%d", sindex));
}


static void unasmCall(const Proto *p, Instruction *pc) {
    int from, nres;
    const char *res = NULL;
    startline(p, pc);
    traceop(*pc);
    from = GETARG_L(pc, 0);
    nres = GETARG_L(pc, 1) - 1;
    traceSTK(from);
    if (nres < 0)
        res = "multiple";
    else if (nres == 0)
        res = "none";
    if (res) {
        postab(printf("nres=%s", res));
    } else
        postab(printf("nres=%d", nres));
    endline();
}


static void unasmMM(cs_State *ts, const Proto *p, Instruction *pc) {
    cs_MM mm;
    startline(p, pc);
    traceop(*pc);
    mm = GETARG_S(pc, 0);
    postab(printf("%s", getstr(G_(ts)->mmnames[mm])));
    endline();
}


static void unasmMBin(cs_State *ts, const Proto *p, Instruction *pc) {
    cs_MM mm;
    startline(p, pc);
    traceop(*pc);
    mm = GETARG_S(pc, 0);
    postab(printf("%s", getstr(G_(ts)->mmnames[mm])));
    endline();
}


static void unasmIndexedSet(const Proto *p, Instruction *pc) {
    int s = GETARG_L(pc, 0);
    int k = GETARG_L(pc, 1);
    startline(p, pc);
    traceop(*pc);
    traceSTK(s);
    traceK(p, k);
    endline();
}


static void unasmGlobal(const Proto *p, Instruction *pc) {
    const char *str;
    int index;
    startline(p, pc);
    traceop(*pc);
    index = GETARG_L(pc, 0);
    str = getstr(strval(&p->k[index]));
    postab(printf("G@%d=\"%s\"", index, str));
    endline();
}


static void unasmLocal(const Proto *p, Instruction *pc) {
    const char *str;
    int index;
    startline(p, pc);
    traceop(*pc);
    index = GETARG_L(pc, 0);
    str = getstr(p->locals[index].name);
    postab(printf("L@%d=\"%s\"", index, str));
    endline();
}


static void unasmUpvalue(const Proto *p, Instruction *pc) {
    const char *str;
    int index;
    startline(p, pc);
    traceop(*pc);
    index = GETARG_L(pc, 0);
    str = getstr(p->upvals[index].name);
    postab(printf("U@%d=\"%s\"", index, str));
    endline();
}


static void unasmRet(const Proto *p, Instruction *pc) {
    int nres, nbase, close;
    startline(p, pc);
    traceop(*pc);
    nbase = GETARG_L(pc, 0);
    nres = GETARG_L(pc, 1);
    close = GETARG_S(pc, (2*SIZEARGL));
    traceSTK(nbase);
    postab(printf("nres=%d", nres - 1));
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
    if (p->defline == 0)
        printf("%s {\n", getstr(p->source));
    else
        printf("fn at line %d in %s {\n", p->defline, getstr(p->source));
    while (pc < &p->code[p->sizecode]) {
        printf("    ");
        switch (*pc) {
            case OP_TRUE: case OP_FALSE: case OP_NIL:
            case OP_POP: case OP_ADD: case OP_SUB:
            case OP_MUL: case OP_DIV: case OP_MOD: case OP_POW:
            case OP_BSHL: case OP_BSHR: case OP_BAND: case OP_BOR:
            case OP_BXOR:case OP_LT: case OP_LE: case OP_NOT:
            case OP_UNM: case OP_BNOT: case OP_EQPRESERVE:
            case OP_GETINDEX: case OP_GETSUPIDX: case OP_INHERIT:
            case OP_FORCALL: case OP_FORLOOP: {
                unasm(p, pc);
                break;
            }
            case OP_NILN: case OP_VARARG: case OP_CLOSURE:
            case OP_POPN: case OP_JMP: case OP_JMPS: case OP_CLOSE:
            case OP_TBC: case OP_CONCAT: case OP_GETINDEXINT:
            case OP_SETINDEX: {
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
            case OP_FORPREP: {
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
            case OP_CONST: unasmK(p, pc); break;
            case OP_CONSTI: unasmIMMint(p, pc); break;
            case OP_CONSTF: unasmIMMflt(p, pc); break;
            case OP_SETMM: unasmMM(ts, p, pc); break;
            case OP_MBIN: unasmMBin(ts, p, pc); break;
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
        printf("[LEVEL %3d] %-10s >> ", i, typename(ttype(s2v(cf->func.p))));
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
