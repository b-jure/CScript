#include "crcode.h"
#include "crlexer.h"
#include "crbits.h"
#include "crlimits.h"
#include "crparser.h"
#include "crstate.h"



/* long op */
#define opL(op)         ((op) + 1)


/* check if 'ExpInfo' has jumps */
#define hasjumps(e)     ((e)->t != (e)->f)


/* unary 'opr' to opcode */
#define unopr2op(opr) \
    cast(OpCode, cast_int(opr) - OPR_UMIN + OP_NOT)

/* binary 'opr' to opcode */
#define binopr2op(opr) \
    cast(OpCode, cast_int(opr) - OPR_ADD + OP_ADD)



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


/* emit instruction */
int cr_code_code(FunctionState *fs, Instruction i)
{
    Function *f = fs->fn;
    cr_mem_growvec(fs->lx->ts, f->code, f->sizecode, f->ncode, INT_MAX, "code");
    f->code[f->ncode++] = i;
    addlineinfo(fs, f, fs->lx->line);
    return f->ncode - 1;
}


/* emit short arg */
static int shortarg(FunctionState *fs, Function *f, cr_ubyte arg)
{
    cr_mem_growvec(fs->lx->ts, f->code, f->sizecode, f->ncode, INT_MAX, "code");
    f->code[f->ncode++] = cast_ubyte(arg & 0xff);
    return f->ncode - 1;
}


/* emit instruction with short parameter */
int cr_code_codesarg(FunctionState *fs, Instruction i, int arg)
{
    Function *f = fs->fn;
    int offset = cr_code_code(fs, i);
    shortarg(fs, f, arg);
    return offset;
}


/* emit long arg */
static int longarg(FunctionState *fs, Function *f, int idx)
{
    cr_mem_ensurevec(fs->lx->ts, f->code, f->sizecode, f->ncode, 3, INT_MAX,
                     "code");
    setbytes(f->code, idx, 3);
    f->ncode += 3;
    return f->ncode - 3;
}


/* emit instruction with long parameter */
int cr_code_codelarg(FunctionState *fs, Instruction i, int arg)
{
    Function *f = fs->fn;
    int offset = cr_code_code(fs, i);
    longarg(fs, f, arg);
    return offset;
}


/* emit instruction with generic arg */
int cr_code_codearg(FunctionState *fs, Instruction i, int arg)
{
    Function *f = fs->fn;
    int offset = cr_code_code(fs, i);
    cr_assert(arg >= 0);
    if (arg <= CRI_SHRTPARAM)
        shortarg(fs, f, cast_ubyte(arg));
    else
        longarg(fs, f, arg);
    return offset;
}


/* add constant value to the function */
static int addconstant(FunctionState *fs, TValue *constant)
{
    Function *f = fs->fn;
    cr_mem_growvec(fs->lx->ts, f->constants, f->sizeconst, f->nconst, INT_MAX,
                   "constants");
    f->constants[f->nconst++] = *constant;
    return f->nconst - 1;
}


/* write OP_CONST instruction with float parameter */
int cr_code_flt(FunctionState *fs, cr_number n)
{
    TValue vn;
    setfval(&vn, n);
    int idx = addconstant(fs, &vn);
    cr_code_codelarg(fs, OP_CONST, idx);
    return idx;
}


/* write OP_CONST instruction with integer parameter */
int cr_code_int(FunctionState *fs, cr_integer i)
{
    TValue vi;
    setival(&vi, i);
    int idx = addconstant(fs, &vi);
    cr_code_codelarg(fs, OP_CONST, idx);
    return idx;
}


/* write OP_CONST instruction with string idx */
int cr_code_string(FunctionState *fs, OString *str)
{
    TValue vs;
    setoval(&vs, obj2gco(str));
    int idx = addconstant(fs, &vs);
    cr_code_codelarg(fs, OP_CONST, idx);
    return idx;
}


/* free stack space */
cr_sinline void freestack(FunctionState *fs, ExpInfo *e)
{
    if (e->et == EXP_FINEXPR) {
        if (e->u.info >= fs->activelocals - 1) {
            fs->sp -= 1;
            cr_assert(e->u.info == fs->sp);
        }
    }
}


void cr_code_checkstack(FunctionState *fs, int n)
{
    int newstack = fs->sp + n;
    if (fs->fn->maxstack > newstack) {
        if (cr_unlikely(newstack >= CRI_LONGPARAM))
            cr_lex_syntaxerror(fs->lx, "function requires too much stack space");
        fs->fn->maxstack = newstack;
    }
}


void cr_code_reservestack(FunctionState *fs, int n)
{
    cr_code_checkstack(fs, n);
    fs->sp += n;
}


static int getvar(FunctionState *fs, OpCode op, ExpInfo *e)
{
    cr_assert(op == OP_GETLVAR || op == OP_GETGVAR);
    if (e->u.idx <= CRI_SHRTPARAM)
        return cr_code_codesarg(fs, op, e->u.idx);
    cr_assert(opL(op) == OP_GETLVARL || opL(op) == OP_GETGVARL);
    return cr_code_codelarg(fs, opL(op), e->u.idx);
}


/* set single return, for call and vararg expressions */
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


static int codelarg(FunctionState *fs, Instruction op, int a)
{
    int offset = cr_code_code(fs, op);
    longarg(fs, fs->fn, a);
    return offset;
}


/* emit instruction with 2 long arguments */
static int codellarg(FunctionState *fs, Instruction op, int a, int b)
{
    int offset = codelarg(fs, op, a);
    longarg(fs, fs->fn, b);
    return offset;
}


/* emit instruction with 3 long arguments */
static int codelllarg(FunctionState *fs, Instruction op, int a, int b, int c)
{
    int offset = codellarg(fs, op, a, b);
    longarg(fs, fs->fn, c);
    return offset;
}


int cr_code_nil(FunctionState *fs, int n)
{
    if (n == 1)
        return cr_code_code(fs, OP_NIL);
    else
        return codelarg(fs, OP_NILN, n);
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
    return codellarg(fs, op, base, nreturns + 1);
}


int cr_code_call(FunctionState *fs, int base, int args, int nreturns)
{
    OpCode op;
    switch (args) {
    case 0: op = OP_CALL0; break;
    case 1: op = OP_CALL1; break;
    default: op = OP_CALL; break;
    }
    return codelllarg(fs, op, base, args + 1, nreturns + 1);
}


/* emit 'OP_SET' family of instructions */
void cr_code_storevar(FunctionState *fs, ExpInfo *e)
{
    switch (e->et) {
    case EXP_LOCAL:
        e->u.info = getvar(fs, OP_SETLVAR, e);
        e->et = EXP_FINEXPR;
        break;
    case EXP_UVAL:
        e->u.info = cr_code_codelarg(fs, OP_SETUVAL, e->u.info);
        e->et = EXP_FINEXPR;
        break;
    case EXP_GLOBAL:
        e->u.info = getvar(fs, OP_SETGVAR, e);
        e->et = EXP_FINEXPR;
        break;
    case EXP_INDEXK:
        e->u.info = cr_code_codelarg(fs, OP_SETINDEXK, e->u.idx);
        break;
    case EXP_INDEXRAW:
        e->u.info = cr_code_codelarg(fs, OP_SETPROPERTY, e->u.idx);
        e->et = EXP_FINEXPR;
        break;
    case EXP_INDEXED:
        freestack(fs, 1);
        e->u.info = cr_code_code(fs, OP_SETINDEX);
        e->et = EXP_FINEXPR;
        break;
    default:
        cr_unreachable();
        break;
    }
    freestack(fs, 1);
}


static void movetostack(FunctionState *fs, ExpInfo *e)
{
    if (e->et == EXP_FINEXPR) return;
    cr_code_reservestack(fs, 1);
    int sp = fs->sp - 1;
    cr_code_dischargevar(fs, e);
    switch (e->et) {
    case EXP_NIL:
        cr_code_nil(fs, 1);
        break;
    case EXP_FALSE:
        cr_code_code(fs, OP_FALSE);
        break;
    case EXP_TRUE:
        cr_code_code(fs, OP_TRUE);
        break;
    case EXP_STRING:
        cr_code_string(fs, e->u.str);
        break;
    case EXP_INT:
        cr_code_int(fs, e->u.n);
        break;
    case EXP_FLT:
        cr_code_flt(fs, e->u.n);
        break;
    default:
        cr_assert(e->et == EXP_JMP);
        return;
    }
    e->u.info = sp; /* compiler stack location */
    e->et = EXP_FINEXPR;
}


/* emit 'OP_GET' family of instructions */
void cr_code_dischargevar(FunctionState *fs, ExpInfo *e)
{
    switch (e->et) {
    case EXP_LOCAL:
        e->u.info = getvar(fs, OP_GETLVAR, e);
        e->et = EXP_FINEXPR;
        break;
    case EXP_UVAL:
        e->u.info = cr_code_codelarg(fs, OP_GETUVAL, e->u.info);
        e->et = EXP_FINEXPR;
        break;
    case EXP_GLOBAL:
        e->u.info = getvar(fs, OP_GETGVAR, e);
        e->et = EXP_FINEXPR;
        break;
    case EXP_INDEXK:
        freestack(fs, 1);
        e->u.info = cr_code_codelarg(fs, OP_GETINDEXK, e->u.idx);
        break;
    case EXP_INDEXRAW:
        freestack(fs, 1);
        e->u.info = cr_code_codelarg(fs, OP_GETPROPERTY, e->u.idx);
        e->et = EXP_FINEXPR;
        break;
    case EXP_INDEXRAWSUP:
        freestack(fs, 1);
        e->u.info = cr_code_codelarg(fs, OP_GETSUP, e->u.idx);
        e->et = EXP_FINEXPR;
        break;
    case EXP_INDEXSUP:
        freestack(fs, 1);
        e->u.info = cr_code_codelarg(fs, OP_GETSUPIDX, e->u.idx);
        e->et = EXP_FINEXPR;
        break;
    case EXP_INDEXED:
        freestack(fs, 1);
        e->u.info = cr_code_code(fs, OP_GETINDEX);
        e->et = EXP_FINEXPR;
        break;
    case EXP_CALL: case EXP_VARARG:
        cr_code_setoneret(fs, e);
        break;
    default: break;
    }
}


static int condjmp(FunctionState *fs, ExpInfo *e, int cond)
{
    int pc;
    movetostack(fs, e);
    freestack(fs, 1);
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


static void codenot(FunctionState *fs, ExpInfo *e)
{
    TValue res;
    switch (e->et) {
    case EXP_NIL: case EXP_FALSE:
        e->et = EXP_TRUE;
        break;
    case EXP_TRUE: case EXP_STRING:
        e->et = EXP_FALSE;
        break;
    case EXP_INT: 
        e->et = (e->u.i == 0 ? EXP_TRUE : EXP_FALSE);
        break;
    case EXP_FLT:
        e->et = (e->u.n == 0.0 ? EXP_TRUE : EXP_FALSE);
        break;
    case EXP_JMP: /* TODO(jure): negate jump condition */
        break;
    case EXP_FINEXPR:
        e->u.info = cr_code_code(fs, OP_NOT);
        e->et = EXP_FINEXPR;
        break;
    default: cr_unreachable(); break;
    }
}


void cr_code_unary(FunctionState *fs, ExpInfo *e, Unopr op)
{
    static const ExpInfo dummy = {EXP_INT, 0, -1, -1};
    cr_assert(OPR_NOT <= op && op < OPR_NOUNOPR);
    cr_code_dischargevar(fs, e);
    switch (op) {
    case OPR_UMIN: case OPR_BNOT:
        if (constfold(fs, e, &dummy, op + CR_OPUMIN))
            break; /* folded */
        movetostack(fs, e);
        cr_code_code(fs, unopr2op(op));
        break;
    case OPR_NOT: codenot(fs, e); break;
    default: cr_unreachable(); break;
    }
}


/* jump if zero (false) */
void cr_code_goiftrue(FunctionState *fs, ExpInfo *e) 
{
    int pc;
    cr_code_dischargevar(fs, e);
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
    cr_code_dischargevar(fs, e);
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


void cr_code_infix(FunctionState *fs, ExpInfo *e, Binopr op)
{
    cr_assert(op != OPR_NOBINOPR);
    switch (op) {
        case OPR_ADD: case OPR_SUB: case OPR_MUL:
        case OPR_DIV: case OPR_MOD: case OPR_POW:
        case OPR_SHL: case OPR_SHR: case OPR_BAND:
        case OPR_BOR: case OPR_BXOR:
            if (!tonumeral(e, NULL)) /* not a constant numeral value ? */
                cr_code_dischargevar(fs, e); /* finalize the expression */
            /* otherwise keep the constant value for
             * a chance to fold the binary expression */
            break;
        case OPR_NE: case OPR_EQ: case OPR_LT:
        case OPR_LE: case OPR_GT: case OPR_GE:
            cr_code_dischargevar(fs, e);
            break;
        case OPR_AND:
            cr_code_goiftrue(fs, e);
            break;
        case OPR_OR:
            cr_code_goiffalse(fs, e);
            break;
        default: cr_unreachable(); break;
    }
}


void cr_code_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr op)
{
    cr_assert(OPR_ADD <= op && op < OPR_NOBINOPR);
    cr_code_dischargevar(fs, e1);
    cr_code_dischargevar(fs, e2);
    if (boprisfoldable(op) && constfold(fs, e1, e2, op + CR_OPADD))
        return; /* folded */
    switch (op) {

    }
    cr_code_code(fs, binopr2op(op));
}
