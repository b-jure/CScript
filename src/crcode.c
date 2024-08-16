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


/* binary operation to OpCode; 'a' anchor, 'b' base */
#define binopr2op(opr,a,b) \
    cast(OpCode, (cast_int(opr) - cast_int(a)) + cast_int(b))




/* check if 'i' fits in long arg */
#define fitsLA(i)       (-MAXLONGARGSIZE <= (i) && (i) <= MAXLONGARGSIZE)

/* check if 'i' fits in short arg */
#define fitsSA(i)       (-MAXSHRTARGSIZE <= (i) && (i) <= MAXSHRTARGSIZE)



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
static int Sarg(FunctionState *fs, Function *f, int arg)
{
    cr_mem_growvec(fs->lx->ts, f->code, f->sizecode, f->ncode, INT_MAX, "code");
    f->code[f->ncode++] = cast_ubyte(arg & 0xff);
    return f->ncode - 1;
}


/* emit instruction with short arg */
int cr_code_S(FunctionState *fs, Instruction i, int a)
{
    cr_assert(arg <= MAXSHRTARGSIZE);
    int offset = code(fs, i);
    Sarg(fs, fs->fn, a);
    return offset;
}


/* emit long arg */
static int Larg(FunctionState *fs, Function *f, int idx)
{
    cr_mem_ensurevec(fs->lx->ts, f->code, f->sizecode, f->ncode, 3, INT_MAX,
                     "code");
    setbytes(f->code, idx, 3);
    f->ncode += 3;
    return f->ncode - 3;
}


/* emit instruction 'i' with long arg 'a' */
int cr_code_L(FunctionState *fs, Instruction i, int a)
{
    cr_assert(a <= MAXLONGARGSIZE);
    int offset = code(fs, i);
    Larg(fs, fs->fn, a);
    return offset;
}


/* emit instruction with 2 long args */
static int codeLL(FunctionState *fs, Instruction i, int a, int b)
{
    int offset = cr_code_L(fs, i, a);
    Larg(fs, fs->fn, b);
    return offset;
}


/* emit instruction with 3 long args */
static int codeLLL(FunctionState *fs, Instruction i, int a, int b, int c)
{
    int offset = codeLL(fs, i, a, b);
    Larg(fs, fs->fn, c);
    return offset;
}


/* add constant value to the function */
static int addK(FunctionState *fs, TValue *constant)
{
    Function *f = fs->fn;
    cr_mem_growvec(fs->lx->ts, f->constants, f->sizeconst, f->nconst, INT_MAX,
                   "constants");
    f->constants[f->nconst++] = *constant;
    return f->nconst - 1;
}


/* add 'nil' constant to 'constants' */
static int nilK(FunctionState *fs)
{
    TValue nv;
    setnilval(&nv);
    return addK(fs, &nv);
}


/* add 'true' constant to 'constants' */
static int trueK(FunctionState *fs)
{
    TValue btv;
    setnilval(&btv);
    return addK(fs, &btv);
}


/* add 'false' constant to 'constants' */
static int falseK(FunctionState *fs)
{
    TValue bfv;
    setbfval(&bfv);
    return addK(fs, &bfv);
}


/* add string constant to 'constants' */
static int stringK(FunctionState *fs, OString *s)
{
    TValue vs;
    setoval(&vs, obj2gco(s));
    return addK(fs, &vs);
}


/* add integer constant to 'constants' */
static int intK(FunctionState *fs, cr_integer i)
{
    TValue vi;
    setival(&vi, i);
    return addK(fs, &vi);
}


/* add float constant to 'constants' */
static int fltK(FunctionState *fs, cr_number n)
{
    TValue vn;
    setfval(&vn, n);
    return addK(fs, &vn);
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
        return cr_code_L(fs, OP_NILN, n);
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
    return codeLL(fs, op, base, nreturns + 1);
}


int cr_code_call(FunctionState *fs, int base, int nparams, int nreturns)
{
    OpCode op;
    switch (nparams) {
    case 0: op = OP_CALL0; break;
    case 1: op = OP_CALL1; break;
    default: op = OP_CALL; break;
    }
    return codeLLL(fs, op, base, nparams + 1, nreturns + 1);
}


cr_sinline void freeslots(FunctionState *fs, int n) 
{
    fs->sp -= n;
}


/* emit 'OP_SET' family of instructions */
void cr_code_storevar(FunctionState *fs, ExpInfo *var, ExpInfo *exp)
{
//     switch (var->et) {
//     case EXP_LOCAL:
//         freeexp(fs, exp);
//         var->u.info = cr_code_code(fs, OP_SETLVAR);
//         var->et = EXP_FINEXPR;
//         break;
//     case EXP_UVAL:
//         cr_code_varexp2stack(fs, exp);
//         freeexp(fs, exp);
//         var->u.info = codelarg(fs, OP_SETUVAL, var->u.info);
//         var->et = EXP_FINEXPR;
//         break;
//     case EXP_GLOBAL:
//         cr_code_varexp2stack(fs, exp);
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
}


/* ensure variable is on stack */
static int dischargevars(FunctionState *fs, ExpInfo *e)
{
    switch (e->et) {
    case EXP_LOCAL: {
        e->u.info = cr_code_L(fs, OP_GETLVAR, e->u.info);
        break;
    }
    case EXP_UVAL: {
        e->u.info = cr_code_L(fs, OP_GETUVAL, e->u.info);
        break;
    }
    case EXP_GLOBAL: {
        e->u.info = cr_code_L(fs, OP_GETGVAR, e->u.info);
        break;
    }
    case EXP_INDEXED: {
        freeslots(fs, 2); /* receiver, key */
        e->u.info = code(fs, OP_GETINDEX);
        break;
    }
    case EXP_INDEXSTR: {
        freeslots(fs, 1); /* receiver */
        e->u.info = cr_code_L(fs, OP_GETINDEXSTR, e->u.info);
        break;
    }
    case EXP_INDEXINT: {
        freeslots(fs, 1); /* receiver */
        e->u.info = cr_code_L(fs, OP_GETINDEXINT, e->u.info);
        break;
    }
    case EXP_INDEXSUPER: {
        freeslots(fs, 3); /* 'self', 'super', key */
        e->u.info = code(fs, OP_GETSUPIDX);
        break;
    }
    case EXP_INDEXSUPERSTR: {
        freeslots(fs, 2); /* 'self', 'super' */
        e->u.info = cr_code_L(fs, OP_GETSUPIDXSTR, e->u.info);
        break;
    }
    case EXP_DOT: {
        freeslots(fs, 1); /* receiver */
        e->u.info = cr_code_L(fs, OP_GETPROPERTY, e->u.info);
        break;
    }
    case EXP_DOTSUPER: {
        freeslots(fs, 2); /* 'self', 'super' */
        e->u.info = cr_code_L(fs, OP_GETSUP, e->u.info);
        break;
    }
    case EXP_JMP: {
        // TODO
        break;
    }
    case EXP_CALL: case EXP_VARARG: {
        cr_code_setoneret(fs, e);
        break;
    }
    default: return 0;
    }
    e->et = EXP_FINEXPR;
    return 1;
}


void cr_code_varexp2stack(FunctionState *fs, ExpInfo *e)
{
    if (e->et != EXP_FINEXPR && dischargevars(fs, e))
        cr_code_reserveslots(fs, 1);
}


static void string2K(FunctionState *fs, ExpInfo *e)
{
    cr_assert(e->et == EXP_STRING);
    e->u.info = stringK(fs, e->u.str);
    e->et = EXP_K;
}



/* get value from constant expression */
cr_sinline TValue *K2value(FunctionState *fs, const ExpInfo *e)
{
    cr_assert(e->et == EXP_CONSTANT);
    return &fs->fn->constants[e->u.info];
}


/* check if expression is a integer constant */
static int isintK(ExpInfo *e)
{
    return (e->et == EXP_INT && !hasjumps(e));
}


/* check if 'isintK' and it fits in long arg */
static int isintKL(ExpInfo *e)
{
    return (isintK(e) && fitsLA(e->u.i));
}


/* check if 'e' is numeral constant and fits inside of large arg */
static int isnumKL(ExpInfo *e, int *immediate, int *isflt)
{
    cr_integer i;
    if (e->et == EXP_INT)
        i = e->u.i;
    else if (e->et == EXP_FLT && cr_value_n2i(e->u.n, &i, CR_N2IFLOOR))
        *isflt = 1;
    else
        return 0;
    if (!hasjumps(e) && fitsLA(i)) {
        *immediate = cast_int(i);
        return 1;
    }
    return 0;
}


/* emit generic load constant instruction */
static int codeK(FunctionState *fs, int idx)
{
    cr_assert(idx >= 0 && fitsLA(idx));
    return (fitsSA(idx) 
            ? cr_code_S(fs, OP_CONST, idx) 
            : cr_code_L(fs, OP_CONSTL, idx));
}


/* emit op with long and short args */
static int codeLS(FunctionState *fs, Instruction op, int a, int b)
{
    int offset = cr_code_L(fs, op, a);
    Sarg(fs, fs->fn, b);
    return offset;
}


/* emit op with long and 2 short args */
static int codeLSS(FunctionState *fs, Instruction op, int a, int b, int c)
{
    int offset = codeLS(fs, op, a, b);
    Sarg(fs, fs->fn, c);
    return offset;
}


/* emit integer constant */
static int codeintK(FunctionState *fs, cr_integer i)
{
    if (fitsLA(i))
        return codeLS(fs, OP_CONSTI, cri_abs(i), i < 0);
    else
        return codeK(fs, intK(fs, i));
}


/* emit float constant */
static int codefltK(FunctionState *fs, cr_number n)
{
    cr_integer i;
    if (cr_value_n2i(n, &i, CR_N2IFLOOR) && fitsLA(i))
        return codeLS(fs, OP_CONSTF, cri_abs(i), i < 0);
    else
        return codeK(fs, fltK(fs, n));
}


/* ensure expression is not a variable or unregistered constant */
static void dischargetostack(FunctionState *fs, ExpInfo *e)
{
    dischargevars(fs, e);
    switch (e->et) {
    case EXP_NIL: {
        e->u.info = cr_code_nil(fs, 1);
        break;
    }
    case EXP_FALSE: {
        e->u.info = code(fs, OP_FALSE);
        break;
    }
    case EXP_TRUE: {
        e->u.info = code(fs, OP_TRUE);
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
    default:
        cr_assert(e->et == EXP_FINEXPR || e->et == EXP_JMP);
        return;
    }
    e->et = EXP_FINEXPR;
}


/* ensure expression value is on stack */
void cr_code_exp2stack(FunctionState *fs, ExpInfo *e)
{
    if (e->et != EXP_FINEXPR)  {
        cr_code_reserveslots(fs, 1);
        dischargetostack(fs, e);
    }
}


/* initialize dot indexed expression */
void cr_code_getproperty(FunctionState *fs, ExpInfo *var, ExpInfo *keystr,
                         int super)
{
    cr_assert(keystr->et == EXP_STRING);
    var->u.info = stringK(fs, keystr->u.str);
    var->et = (super ? EXP_DOTSUPER : EXP_DOT);
}


/* initialize indexed expression */
void cr_code_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key, int super)
{
    cr_assert(var->et == EXP_FINEXPR && var->u.info == fs->sp - 1);
    if (cr_unlikely(key->et == EXP_NIL))
        cr_lex_syntaxerror(fs->lx, "can't use 'nil' as index");
    if (key->et == EXP_STRING)
        string2K(fs, key);
    if (super) {
        if (key->et == EXP_K) {
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPERSTR;
        } else {
            cr_code_exp2stack(fs, key);
            var->u.info = key->u.info;
            var->et = EXP_INDEXSUPER;
        }
    } else if (key->et == EXP_INT && fitsLA(key->u.info)) {
        cr_assert(key->et != EXP_INDEXSUPER);
        key->u.info = cast_int(var->u.i);
        key->et = EXP_INDEXINT;
    } else {
        cr_code_exp2stack(fs, key);
        var->u.info = key->u.info;
        var->et = EXP_INDEXED;
    }
}


/* return 1 if folding for 'op' can raise errors */
static int validop(TValue *v1, TValue *v2, int op)
{
    switch (op) {
    case CR_OPBSHR: case CR_OPBSHL: case CR_OPBAND:
    case CR_OPBOR: case CR_OPBXOR: case CR_OPBNOT: { /* conversion */
        cr_integer i;
        return (tointeger(v1, &i) && tointeger(v2, &i));
    }
    case CR_OPDIV: case CR_OPMOD: { /* division by 0 */
        return (nval(v2) != 0);
    }
    default: return 1;
    }
}


/* check if expression is numeral constant */
static int tonumeral(const ExpInfo *e1, TValue *res)
{
    switch (e1->et) {
    case EXP_FLT: if (res) setfval(res, e1->u.n); return 1;
    case EXP_INT: if (res) setival(res, e1->u.i); return 1;
    default: return 0;
    }
}


/* fold constant expressions */
static int constfold(FunctionState *fs, ExpInfo *e1, const ExpInfo *e2, int opr)
{
    TValue v1, v2, res;
    if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || validop(&v1, &v2, opr))
        return 0;
    cr_value_arithmraw(fs->lx->ts, &v1, &v2, &res, opr);
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


/* emit unary instruction; except logical not '!' */
static void codeunary(FunctionState *fs, ExpInfo *e, OpCode op)
{
    cr_code_exp2stack(fs, e);
    cr_assert(e->et == EXP_FINEXPR);
    e->u.info = code(fs, op);
}


/* emit logical not instruction */
static void codenot(FunctionState *fs, ExpInfo *e)
{
    cr_assert(!eisvar(e)); /* vars are already finalized */
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
        e->u.info = code(fs, OP_NOT);
        break;
    }
    default: cr_unreachable();
    }
}


/* emit unary instruction */
void cr_code_unary(FunctionState *fs, ExpInfo *e, Unopr opr)
{
    static const ExpInfo dummy = {EXP_INT, 0, -1, -1};
    cr_assert(OPR_NOT <= op && op < OPR_NOUNOPR);
    cr_code_varexp2stack(fs, e);
    switch (opr) {
    case OPR_UMIN: case OPR_BNOT: {
        if (constfold(fs, e, &dummy, (opr - OPR_NOT) + CR_OPNOT))
            break; /* folded */
        codeunary(fs, e, unopr2op(opr));
        break;
    }
    case OPR_NOT:  {
        codenot(fs, e); 
        break;
    }
    default: cr_unreachable();
    }
}


/* emit code jmp instruction */
static int codejmp(FunctionState *fs, ExpInfo *e, OpCode jmpop)
{
    cr_code_exp2stack(fs, e); /* ensure that condition 'e' is on stack */
    return cr_code_L(fs, jmpop, NOJMP);
}


/* get 'pc' of jmp instruction destination */
static int getjmp(FunctionState *fs, int pc)
{
    int offset = *getlarg0(&fs->fn->code[pc]);
    if (offset == NOJMP)
        return NOJMP;
    else
        return pc + offset + 1; /* destination instruction */
}


/* fix jmp instruction at 'pc' to jump to 'dest' */
static void fixjmp(FunctionState *fs, int pc, int destpc)
{
    Instruction *jmp = &fs->fn->code[pc];
    int offset = destpc - (pc + 1);
    if (cr_unlikely(!(0 <= offset && offset <= MAXLONGARGSIZE)))
        cr_lex_syntaxerror(fs->lx, "control structure too long");
    setlarg0(jmp, offset);
}


/* concatenate jmp label 'l2' into jmp label 'l1' */
void cr_code_concatjmp(FunctionState *fs, int *l1, int l2)
{
    if (l2 == NOJMP) return;
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


/* backpatch jump instruction at 'pc' */
void cr_code_patchjmp(FunctionState *fs, int pc)
{
    int target = fs->fn->ncode; /* current 'pc' */
    while (pc != NOJMP) {
        int next = getjmp(fs, pc);
        fixjmp(fs, pc, target);
        pc = next;
    }
}


/* jump if expression is false */
void cr_code_jmpiffalse(FunctionState *fs, ExpInfo *e, OpCode jmpop)
{
    cr_code_varexp2stack(fs, e);
    switch (e->et) {
    case EXP_TRUE: case EXP_STRING: case EXP_INT: case EXP_FLT: {
        e->f = NOJMP;
        break;
    }
    default: 
        e->f = codejmp(fs, e, jmpop);
        break;
    }
    e->t = NOJMP;
}


/* jump if expression is true */
void cr_code_jmpiftrue(FunctionState *fs, ExpInfo *e, OpCode jmpop)
{
    cr_code_varexp2stack(fs, e);
    switch (e->et) {
    case EXP_NIL: case EXP_FALSE: {
        e->t = NOJMP;
        break;
    }
    default:
        e->t = codejmp(fs, e, jmpop);
        break;
    }
    e->f = NOJMP;
}


void cr_code_prebinary(FunctionState *fs, ExpInfo *e, Binopr op)
{
    cr_code_varexp2stack(fs, e);
    switch (op) {
    case OPR_ADD: case OPR_SUB: case OPR_MUL:
    case OPR_DIV: case OPR_MOD: case OPR_POW:
    case OPR_SHL: case OPR_SHR: case OPR_BAND:
    case OPR_BOR: case OPR_BXOR: case OPR_NE:
    case OPR_EQ: {
        if (!tonumeral(e, NULL))
            cr_code_exp2stack(fs, e);
        /* otherwise keep numeral for constant
         * or immediate operand variant instruction */
        break;
    }
    case OPR_LT: case OPR_LE: case OPR_GT: case OPR_GE: {
        int dummy, dummy2;
        if (!isnumKL(e, &dummy, &dummy2))
            cr_code_exp2stack(fs, e);
        /* otherwise keep numeral for immediate
         * operand variant instruction */
        break;
    }
    case OPR_RANGE: { 
        if (!isintKL(e))
            cr_code_exp2stack(fs, e);
        /* otherwise keep integer in case 
         * this is integer range */
        break;
    }
    case OPR_AND: {
        cr_code_jmpiffalse(fs, e, OP_JFORPOP);
        break;
    }
    case OPR_OR: {
        cr_code_jmpiftrue(fs, e, OP_JTORPOP);
        break;
    }
    default: cr_unreachable();
    }
}


/* register constant expressions */
static int exp2K(FunctionState *fs, ExpInfo *e)
{
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
cr_sinline void swapexp(ExpInfo *e1, ExpInfo *e2)
{
    const ExpInfo temp = *e1; *e1 = *e2; *e2 = temp;
}


/* emit generic binary instruction */
static void codebin(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr)
{
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADD);
    cr_assert(e2->et != EXP_K);
    cr_assert(OP_ADD <= op && op <= OP_BXOR);
    cr_code_exp2stack(fs, e2); /* ensure e2 is on stack */
    freeslots(fs, 1); /* binary expression produces a single value */
    e1->u.info = code(fs, op);
    e1->et = EXP_FINEXPR;
    cr_code_S(fs, OP_MBIN, op);
}


/* emit binary instruction variant where second operator is constant */
static void codebinK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                     int flip)
{
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADD);
    int idxK = e2->u.info; /* index into 'constants' */
    cr_assert(e2->et == EXP_K);
    cr_assert(OP_ADD <= op && op <= OP_RANGE);
    e1->u.info = cr_code_L(fs, op, idxK);
    e1->et = EXP_FINEXPR;
    codeLS(fs, OP_MBINK, idxK, flip);
}


/* emit arithmetic binary op */
static void codebinarithm(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                          Binopr opr, int flip)
{
    if (tonumeral(e2, NULL) && exp2K(fs, e2)) {
        codebinK(fs, e1, e2, opr, flip);
    } else {
        if (flip)
            swapexp(e1, e2);
        codebin(fs, e1, e2, opr);
    }
}


/* emit binary instruction variant where second operand is immediate value */
static void codebinI(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                     int flip)
{
    int rhs = e2->u.i;
    int neg = rhs < 0;
    int rhsabs = cri_abs(rhs);
    OpCode op = binopr2op(opr, OPR_ADD, OP_ADDI);
    cr_assert(e2->et == EXP_INT);
    e1->u.info = codeLS(fs, op, rhsabs, neg);
    e1->et = EXP_FINEXPR;
    codeLSS(fs, OP_MBINI, rhsabs, neg, flip);
}


/* emit binary instruction trying both the immediate and constant variants */
static void codebinIK(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr,
                      int flip)
{
    if (isintKL(e2))
        codebinI(fs, e1, e2, opr, flip);
    else
        codebinarithm(fs, e1, e2, opr, flip);
}


/* emit commutative binary instruction */
static void codecommutative(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                            Binopr opr)
{
    int flip = 0;
    if (tonumeral(e1, NULL)) {
        swapexp(e1, e2);
        flip = 1;
    }
    codebinIK(fs, e1, e2, opr, flip);
}


/* emit equality binary instruction */
static void codeeq(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr)
{
    OpCode op;
    int immediate;
    int isflt = 0;
    int iseq = (opr == OP_EQ);
    cr_assert(opr == OPR_NE || opr == OPR_EQ);
    if (e1->et != EXP_FINEXPR) {
        /* 'e1' is either a stored string constant or numeral */
        cr_assert(e1->et == EXP_K || e1->et == EXP_INT || e1->et == EXP_FLT);
        swapexp(e1, e2);
    }
    cr_code_exp2stack(fs, e1); /* ensure 'e1' is on stack */
    if (isintK(e2)) {
        if (isnumKL(e2, &immediate, &isflt))
            e1->u.info = codeLSS(fs, OP_EQI, immediate, isflt, iseq);
        else
            e1->u.info = codeLS(fs, OP_EQK, e2->u.info, iseq);
    } else {
        e1->u.info = cr_code_S(fs, OP_EQ, iseq);
        freeslots(fs, 1);
    }
    e1->et = EXP_FINEXPR;
}


/* emit binary ordering instruction */
static void codeorder(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr)
{
    int isflt = 0;
    int immediate;
    OpCode op;
    cr_assert(OPR_LT >= opr && opr < OPR_GT); /* should already be swapped */
    if (isnumKL(e2, &immediate, &isflt)) {
        cr_code_exp2stack(fs, e1); /* ensure 'e1' is on stack */
        op = binopr2op(opr, OPR_LT, OP_LTI);
        goto emit;
    } else if (isnumKL(e1, &immediate, &isflt)) {
        cr_code_exp2stack(fs, e2); /* ensure 'e2' is on stack */
        op = binopr2op(opr, OPR_LT, OP_GTI);
emit:
        e1->u.info = codeLS(fs, op, immediate, isflt);
    } else {
        e1->u.info = code(fs, op);
        freeslots(fs, 1);
    }
    e1->et = EXP_FINEXPR;
}


void cr_code_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2, Binopr opr)
{
    cr_assert(OPR_ADD <= op && op < OPR_NOBINOPR);
    cr_code_varexp2stack(fs, e2);
    if (boprisfoldable(opr) && constfold(fs, e1, e2, opr + CR_OPADD))
        return; /* folded */
    switch (opr) {
    case OPR_ADD: case OPR_MUL:
    case OPR_BAND: case OPR_BOR: case OPR_BXOR: {
        codecommutative(fs, e1, e2, opr);
        break;
    }
    case OPR_SUB: case OPR_DIV: case OPR_MOD: case OPR_POW:
    case OPR_SHL: case OPR_SHR:  {
        codebinIK(fs, e1, e2, opr, 0);
        break;
    }
    case OPR_RANGE: {
        // TODO
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
        cr_assert(e1->t == NOJMP);
        cr_code_concatjmp(fs, &e2->f, e1->f);
        *e1 = *e2;
        break;
    }
    case OPR_OR: {
        cr_assert(e1->f == NOJMP);
        cr_code_concatjmp(fs, &e2->t, e1->t);
        *e1 = *e2;
        break;
    }
    default: cr_unreachable();
    }
}
