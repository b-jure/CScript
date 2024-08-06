#include "crcode.h"
#include "crlexer.h"
#include "crbits.h"
#include "crlimits.h"
#include "crparser.h"
#include "crstate.h"



/* check if 'ExpInfo' has jumps */
#define hasjumps(e)     ((e)->t != (e)->f)



/* unary 'opr' to opcode */
#define unopr2op(opr) \
    cast(OpCode, cast_int(opr) - OPR_UMIN + OP_NOT)


/* binary 'opr' to opcode */
#define binopr2op(opr) \
    cast(OpCode, cast_int(opr) - OPR_ADD + OP_ADD)



/* check if 'i' fits in long arg */
#define fitsLA(i)       (cast(cr_integer, i) <= MAXLONGARGSIZE)

/* check if 'i' fits in short arg */
#define fitsSA(i)       (cast(cr_integer, i) <= MAXSHRTARGSIZE)



/*
 * Add line and pc information, skip adding 'LineInfo' if previous
 * entry contained the same line.
 */
static void addlineinfo(FunctionState *fs, Function *f, int line)
{
    int len = f->nlinfo;
    if (len <= 0 || f->linfo[len - 1].line < line) {
        cr_mem_growvec(fs->lx->ts, f->linfo, f->sizelinfo, f->nlinfo, INT_MAX,
                       "lines");
        f->linfo[len].pc = f->ncode - 1;
        f->linfo[f->nlinfo++].line = line;
    }
}


/* emit instruction 'i' */
static inline int code(FunctionState *fs, Instruction i)
{
    Function *fn = fs->fn;
    cr_mem_growvec(fs->lx->ts, fn->code, fn->sizecode, fn->ncode, INT_MAX, "code");
    fn->code[fn->ncode++] = i;
    addlineinfo(fs, fn, fs->lx->line);
    return fn->ncode - 1;
}


/* emit short arg */
static int SA(FunctionState *fs, Function *f, int arg)
{
    cr_mem_growvec(fs->lx->ts, f->code, f->sizecode, f->ncode, INT_MAX, "code");
    f->code[f->ncode++] = cast_ubyte(arg & 0xff);
    return f->ncode - 1;
}


/* emit instruction with short arg */
int cr_code_SA(FunctionState *fs, Instruction i, int a)
{
    cr_assert(arg <= MAXSHRTARGSIZE);
    int offset = code(fs, i);
    SA(fs, fs->fn, a);
    return offset;
}


/* emit long arg */
static int LA(FunctionState *fs, Function *f, int idx)
{
    cr_mem_ensurevec(fs->lx->ts, f->code, f->sizecode, f->ncode, 3, INT_MAX,
                     "code");
    setbytes(f->code, idx, 3);
    f->ncode += 3;
    return f->ncode - 3;
}


/* emit instruction 'i' with long arg 'a' */
int cr_code_LA(FunctionState *fs, Instruction i, int a)
{
    cr_assert(a <= MAXLONGARGSIZE);
    int offset = code(fs, i);
    LA(fs, fs->fn, a);
    return offset;
}


/* emit instruction with 2 long args */
cr_sinline int codeLAB(FunctionState *fs, Instruction i, int a, int b)
{
    int offset = cr_code_LA(fs, i, a);
    LA(fs, fs->fn, b);
    return offset;
}


/* emit instruction with 3 long args */
cr_sinline int codeLABC(FunctionState *fs, Instruction i, int a, int b, int c)
{
    int offset = codeLAB(fs, i, a, b);
    LA(fs, fs->fn, c);
    return offset;
}


/* add constant value to the function */
static int addk(FunctionState *fs, TValue *constant)
{
    Function *f = fs->fn;
    cr_mem_growvec(fs->lx->ts, f->constants, f->sizeconst, f->nconst, INT_MAX,
                   "constants");
    f->constants[f->nconst++] = *constant;
    return f->nconst - 1;
}


/* add 'nil' constant to 'constants' */
static int nilk(FunctionState *fs)
{
    TValue nv;
    setnilval(&nv);
    return addk(fs, &nv);
}


/* add 'true' constant to 'constants' */
static int truek(FunctionState *fs)
{
    TValue btv;
    setnilval(&btv);
    return addk(fs, &btv);
}


/* add 'false' constant to 'constants' */
static int falsek(FunctionState *fs)
{
    TValue bfv;
    setbfval(&bfv);
    return addk(fs, &bfv);
}


/* add string constant to 'constants' */
static int stringk(FunctionState *fs, OString *s)
{
    TValue vs;
    setoval(&vs, obj2gco(s));
    return addk(fs, &vs);
}


/* add integer constant to 'constants' */
static int intk(FunctionState *fs, cr_integer i)
{
    TValue vi;
    setival(&vi, i);
    return addk(fs, &vi);
}


/* add float constant to 'constants' */
static int fltk(FunctionState *fs, cr_number n)
{
    TValue vn;
    setfval(&vn, n);
    return addk(fs, &vn);
}


/* adjust 'maxstack' */
void cr_code_checkstack(FunctionState *fs, int n)
{
    int newstack = fs->sp + n;
    if (fs->fn->maxstack > newstack) {
        if (cr_unlikely(newstack >= MAXLONGARGSIZE))
            cr_lex_syntaxerror(fs->lx, "function requires too much stack space");
        fs->fn->maxstack = newstack;
    }
}


/* reserve 'n' stack slots */
void cr_code_reserveslots(FunctionState *fs, int n)
{
    cr_code_checkstack(fs, n);
    fs->sp += n;
}


/* set single return for call and vararg expressions */
void cr_code_setoneret(FunctionState *fs, ExpInfo *e)
{
    if (e->et == EXP_CALL) {
        /* already returns a single result */
        cr_assert(*getlarg0(getinstruction(fs, e), 0) == 2);
        e->et = EXP_FINEXPR;
    } else if (e->et == EXP_VARARG) {
        setlarg0(getinstruction(fs, e), 2);
        e->et = EXP_FINEXPR;
    }
}


/* set 'nreturns', for call and vararg expressions */
void cr_code_setreturns(FunctionState *fs, ExpInfo *e, int nreturns)
{
    Instruction *pc = getinstruction(fs, e);
    if (e->et == EXP_CALL) {
        setlarg1(pc, nreturns + 1);
    } else {
        cr_assert(e->et == EXP_VARARG);
        setlarg0(pc, nreturns + 1);
    }
}


int cr_code_nil(FunctionState *fs, int n)
{
    if (n == 1)
        return code(fs, OP_NIL);
    else
        return cr_code_LA(fs, OP_NILN, n);
}


int cr_code_ret(FunctionState *fs, int base, int nreturns)
{
    OpCode op;
    cr_assert(nreturns >= 0);
    switch (nreturns) {
    case 0: op = OP_RET0; break;
    case 1: op = OP_RET1; break;
    default: op = OP_RET; break;
    }
    return codeLAB(fs, op, base, nreturns + 1);
}


int cr_code_call(FunctionState *fs, int base, int nparams, int nreturns)
{
    OpCode op;
    switch (nparams) {
    case 0: op = OP_CALL0; break;
    case 1: op = OP_CALL1; break;
    default: op = OP_CALL; break;
    }
    return codeLABC(fs, op, base, nparams + 1, nreturns + 1);
}


cr_sinline void freeslots(FunctionState *fs, int n) 
{
    fs->sp -= n;
}


/* emit 'OP_SET' family of instructions */
// void cr_code_storevar(FunctionState *fs, ExpInfo *var, ExpInfo *exp)
// {
//     switch (var->et) {
//     case EXP_LOCAL:
//         freeexp(fs, exp);
//         var->u.info = cr_code_code(fs, OP_SETLVAR);
//         var->et = EXP_FINEXPR;
//         break;
//     case EXP_UVAL:
//         cr_code_dischargevars(fs, exp);
//         freeexp(fs, exp);
//         var->u.info = codelarg(fs, OP_SETUVAL, var->u.info);
//         var->et = EXP_FINEXPR;
//         break;
//     case EXP_GLOBAL:
//         cr_code_dischargevars(fs, exp);
//         var->u.info = codelarg(fs, OP_SETGVAR, e->u.var.);
//         var->et = EXP_FINEXPR;
//         break;
//     case EXP_INDEXK:
//         var->u.info = codelarg(fs, OP_SETINDEXK, var->u.idx);
//         break;
//     case EXP_INDEXRAW:
//         var->u.info = codelarg(fs, OP_SETPROPERTY, var->u.idx);
//         var->et = EXP_FINEXPR;
//         break;
//     case EXP_INDEXED:
//         freestackslot(fs, 1);
//         var->u.info = cr_code_code(fs, OP_SETINDEX);
//         var->et = EXP_FINEXPR;
//         break;
//     default:
//         cr_unreachable();
//         break;
//     }
//     freestackslot(fs, 1);
// }


/* ensure variable is on stack */
int cr_code_dischargevars(FunctionState *fs, ExpInfo *e)
{
    switch (e->et) {
    case EXP_LOCAL:
        e->u.info = cr_code_LA(fs, OP_GETLVAR, e->u.info);
        break;
    case EXP_UVAL:
        e->u.info = cr_code_LA(fs, OP_GETUVAL, e->u.info);
        break;
    case EXP_GLOBAL:
        e->u.info = cr_code_LA(fs, OP_GETGVAR, e->u.info);
        break;
    case EXP_INDEXED:
        freeslots(fs, 2); /* receiver, key */
        e->u.info = code(fs, OP_GETINDEX);
        break;
    case EXP_INDEXSTR:
        freeslots(fs, 1); /* receiver */
        e->u.info = cr_code_LA(fs, OP_GETINDEXSTR, e->u.info);
        break;
    case EXP_INDEXINT:
        freeslots(fs, 1); /* receiver */
        e->u.info = cr_code_LA(fs, OP_GETINDEXINT, e->u.info);
        break;
    case EXP_INDEXSUPER:
        freeslots(fs, 3); /* 'self', 'super', key */
        e->u.info = code(fs, OP_GETSUPIDX);
        break;
    case EXP_INDEXSUPERSTR:
        freeslots(fs, 2); /* 'self', 'super' */
        e->u.info = cr_code_LA(fs, OP_GETSUPIDXSTR, e->u.info);
        break;
    case EXP_DOT:
        freeslots(fs, 1); /* receiver */
        e->u.info = cr_code_LA(fs, OP_GETPROPERTY, e->u.info);
        break;
    case EXP_DOTSUPER:
        freeslots(fs, 2); /* 'self', 'super' */
        e->u.info = cr_code_LA(fs, OP_GETSUP, e->u.info);
        break;
    case EXP_JMP:
        // TODO
        break;
    case EXP_CALL: case EXP_VARARG:
        cr_code_setoneret(fs, e);
        break;
    default: return 0;
    }
    e->et = EXP_FINEXPR;
    return 1;
}


static void string2k(FunctionState *fs, ExpInfo *e)
{
    cr_assert(e->et == EXP_STRING);
    e->u.info = stringk(fs, e->u.str);
    e->et = EXP_STRINGK;
}



/* get value from constant expression */
cr_sinline TValue *k2value(FunctionState *fs, const ExpInfo *e)
{
    cr_assert(e->et == EXP_CONSTANT);
    return &fs->fn->constants[e->u.info];
}


/* emit generic load constant instruction */
static int codek(FunctionState *fs, int idx)
{
    cr_assert(fitsLA(idx));
    return (fitsSA(idx) 
            ? cr_code_SA(fs, OP_CONST, idx) 
            : cr_code_LA(fs, OP_CONSTL, idx));
}


/* 
 * Emit 'OP_CONSTINT/L' in case 'i' can fit in either
 * long or short instruction argument, otherwise register
 * 'i' into 'constants' and fallback to 'codeconstant'.
 */
static int codeintk(FunctionState *fs, cr_integer i)
{
    if (fitsSA(i))
        return cr_code_SA(fs, OP_CONSTINT, i);
    else if (fitsLA(i))
        return cr_code_LA(fs, OP_CONSTINTL, i);
    else
        return codek(fs, intk(fs, i));
}


/* 
 * Emit 'OP_CONSTFLT/L' in case 'n' can fit in either
 * long or short instruction argument, otherwise register
 * 'n' into 'constants' and fallback to 'codeconstant'.
 */
static int codeflt(FunctionState *fs, cr_number n)
{
    cr_integer i;
    if (cr_value_n2i(n, &i, CR_N2IFLOOR) && fitsLA(i)) {
        if (fitsSA(i))
            return cr_code_SA(fs, OP_CONSTFLT, i);
        else
            return cr_code_LA(fs, OP_CONSTFLTL, i);
    } else {
        return codek(fs, fltk(fs, n));
    }
}


/* ensure expression is not a variable or unregistered constant */
static void dischargetostack(FunctionState *fs, ExpInfo *e)
{
    cr_code_dischargevars(fs, e);
    switch (e->et) {
    case EXP_NIL:
        e->u.info = cr_code_nil(fs, 1);
        break;
    case EXP_FALSE:
        e->u.info = code(fs, OP_FALSE);
        break;
    case EXP_TRUE:
        e->u.info = code(fs, OP_TRUE);
        break;
    case EXP_INT:
        e->u.info = codeintk(fs, e->u.i);
        break;
    case EXP_FLT:
        e->u.info = codeflt(fs, e->u.n);
        break;
    case EXP_STRING:
        string2k(fs, e);
        /* FALLTHRU */
    case EXP_STRINGK:
        e->u.info = codek(fs, e->u.info);
        break;
    default:
        cr_assert(e->et == EXP_FINEXPR || e->et == EXP_JMP);
        return;
    }
    e->et = EXP_FINEXPR;
}


/* ensure expression value is on stack */
void cr_code_dischargetostack(FunctionState *fs, ExpInfo *e)
{
    if (e->et != EXP_FINEXPR)  {
        cr_code_reserveslots(fs, 1);
        dischargetostack(fs, e);
    }
}


void cr_code_getproperty(FunctionState *fs, ExpInfo *var, ExpInfo *keystr,
                         int super)
{
    cr_assert(keystr->et == EXP_STRING);
    var->u.info = stringk(fs, keystr->u.str);
    var->et = (super ? EXP_DOTSUPER : EXP_DOT);
}


void cr_code_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key, int super)
{
    cr_assert(var->et == EXP_FINEXPR && var->u.info == fs->sp - 1);
    if (cr_unlikely(key->et == EXP_NIL))
        cr_lex_syntaxerror(fs->lx, "can't use 'nil' as index");
    if (key->et == EXP_STRING)
        string2k(fs, key);
    if (super) {
        if (key->et == EXP_STRINGK) {
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPERSTR;
        } else {
            cr_code_dischargetostack(fs, key);
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPER;
        }
    } else if (key->et == EXP_INT && fitsLA(key->u.info)) {
        cr_assert(key->et != EXP_INDEXSUPER);
        key->u.info = cast_int(var->u.i);
        key->et = EXP_INDEXINT;
    } else {
        cr_code_dischargetostack(fs, key);
        var->u.info = key->u.info;
        var->et = EXP_INDEXED;
    }
}


static int condjmp(FunctionState *fs, ExpInfo *e, int cond)
{
    int pc;
    cr_code_dischargetostack(fs, e);
    freeslots(fs, 1);
    return pc;
}


/* return 1 if folding for 'op' can raise errors */
static int validop(TValue *v1, TValue *v2, int op)
{
    switch (op) {
    case CR_OPBSHR: case CR_OPBSHL: case CR_OPBAND:
    case CR_OPBOR: case CR_OPBXOR: case CR_OPBNOT:; /* conversion */
        cr_integer i;
        return (tointeger(v1, &i) && tointeger(v2, &i));
    case CR_OPDIV: case CR_OPMOD: /* division by 0 */
        return (nval(v2) != 0);
    default: /* rest is valid */
        return 1;
    }
}


static int tonumeral(const ExpInfo *e1, TValue *res)
{
    switch (e1->et) {
    case EXP_FLT: if (res) setfval(res, e1->u.n); return 1;
    case EXP_INT: if (res) setival(res, e1->u.i); return 1;
    default: return 0;
    }
}


static int constfold(FunctionState *fs, ExpInfo *e1, const ExpInfo *e2, int op)
{
    TValue v1, v2, res;
    if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || validop(&v1, &v2, op))
        return 0;
    cr_value_arithmraw(fs->lx->ts, &v1, &v2, &res, op);
    if (ttisint(&res)) {
        e1->et = EXP_INT;
        e1->u.i = ival(&res);
    } else {
        cr_number n = fval(&res);
        if (n == 0 || cri_numisnan(n))
            return 0;
        e1->et = EXP_FLT;
        e1->u.n = n;
    }
    return 1;
}


static void codeunary(FunctionState *fs, ExpInfo *e, OpCode op)
{
    cr_code_dischargetostack(fs, e);
    cr_assert(e->et == EXP_FINEXPR);
    e->u.info = code(fs, op);
}


static void codenot(FunctionState *fs, ExpInfo *e)
{
    cr_assert(!eisvar(e)); /* vars are already finalized */
    switch (e->et) {
    case EXP_NIL: case EXP_FALSE:
        e->et = EXP_TRUE;
        break;
    case EXP_TRUE: case EXP_STRING: case EXP_STRINGK:
        e->et = EXP_FALSE;
        break;
    case EXP_INT: 
        e->et = (e->u.i == 0 ? EXP_TRUE : EXP_FALSE);
        break;
    case EXP_FLT:
        e->et = (e->u.n == 0.0 ? EXP_TRUE : EXP_FALSE);
        break;
    case EXP_JMP:
        // TODO: negate condition
        break;
    case EXP_FINEXPR: /* 'e' already on stack */
        e->u.info = code(fs, OP_NOT);
        break;
    default: cr_unreachable();
    }
}


void cr_code_unary(FunctionState *fs, ExpInfo *e, Unopr opr)
{
    static const ExpInfo dummy = {EXP_INT, 0, -1, -1};
    cr_assert(OPR_NOT <= op && op < OPR_NOUNOPR);
    if (cr_code_dischargevars(fs, e))
        cr_code_reserveslots(fs, 1);
    switch (opr) {
    case OPR_UMIN: case OPR_BNOT:
        if (constfold(fs, e, &dummy, (opr - OPR_NOT) + CR_OPNOT))
            break; /* folded */
        codeunary(fs, e, unopr2op(opr));
        break;
    case OPR_NOT: 
        codenot(fs, e); 
        break;
    default: cr_unreachable();
    }
}


/* jump if zero (false) */
void cr_code_goiftrue(FunctionState *fs, ExpInfo *e) 
{
    int pc;
    cr_code_dischargevars(fs, e);
    switch (e->et) {
    case EXP_TRUE: case EXP_STRING:
        e->u.info = NOJMP;
        break;
    case EXP_JMP:
        break;
    case EXP_INT:
        if (e->u.i != 0) {
            e->u.info = NOJMP;
            break;
        }
        goto dflt;
    case EXP_FLT:
        if (e->u.n != 0.0) {
            e->u.info = NOJMP;
            break;
        }
        /* FALLTHRU */
dflt:
    default: 
        pc = condjmp(fs, e, 0);
        break;
    }
}


void cr_code_goiffalse(FunctionState *fs, ExpInfo *e)
{
    int pc;
    cr_code_dischargevars(fs, e);
    switch (e->et) {
    case EXP_NIL: case EXP_FALSE:
        e->u.info = NOJMP;
        break;
    case EXP_JMP:
        break;
    case EXP_INT:
        if (e->u.i == 0) {
            e->u.info = NOJMP;
            break;
        }
        goto dflt;
    case EXP_FLT:
        if (e->u.n == 0.0) {
            e->u.info = NOJMP;
            break;
        }
        /* FALLTHRU */
dflt:
    default: 
        pc = condjmp(fs, e, 1);
        break;
    }
}


void cr_code_prebinary(FunctionState *fs, ExpInfo *e, Binopr op)
{
    cr_code_dischargevars(fs, e);
    switch (op) {
        case OPR_ADD: case OPR_SUB: case OPR_MUL:
        case OPR_DIV: case OPR_MOD: case OPR_POW:
        case OPR_SHL: case OPR_SHR: case OPR_BAND:
        case OPR_BOR: case OPR_BXOR: case OPR_NE:
        case OPR_EQ: case OPR_LT: case OPR_LE:
        case OPR_GT: case OPR_GE:
            if (!tonumeral(e, NULL))
                cr_code_dischargetostack(fs, e);
            break;
        case OPR_RANGE: { 
            TValue res;
            cr_integer i;
            if (!tonumeral(e, &res) || !tointeger(&res, &i))
                cr_code_dischargetostack(fs, e);
            /* otherwise keep integer in case 
             * this is integer range */
            break;
        }
        case OPR_AND:
            cr_code_goiftrue(fs, e);
            break;
        case OPR_OR:
            cr_code_goiffalse(fs, e);
            break;
        default: cr_unreachable();
    }
}


void cr_code_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr op)
{
    cr_assert(OPR_ADD <= op && op < OPR_NOBINOPR);
    cr_code_dischargevars(fs, e1);
    cr_code_dischargevars(fs, e2);
    if (boprisfoldable(op) && constfold(fs, e1, e2, op + CR_OPADD))
        return; /* folded */
    switch (op) {

    }
    code(fs, binopr2op(op));
}
