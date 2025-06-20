/*
** ccode.h
** CScript bytecode and auxiliary functions
** See Copyright Notice in cscript.h
*/

#ifndef ccode_h
#define ccode_h


#include "cbits.h"
#include "cparser.h"


/* 
** Get current pc, this macro expects fs (FunctionState) to be in scope.
*/
#define currPC      (fs->pc)


/* get pointer to instruction from 'ExpInfo' */
#define getpi(fs,e)     (&(fs)->p->code[(e)->u.info])


/* sizes in bytes */
#define SIZE_INSTR          (sizeof(Instruction))
#define SIZE_ARG_S          (sizeof(Instruction))
#define SIZE_ARG_L          (sizeof(Instruction[3]))

/* bit widths */
#define WIDTH_INSTR         (SIZE_INSTR*CHAR_BIT)
#define WIDTH_ARG_S         (SIZE_ARG_S*CHAR_BIT)
#define WIDTH_ARG_L         (SIZE_ARG_L*CHAR_BIT)

/* limits */
#define MAX_INSTR           ((1<<WIDTH_INSTR)-1)
#define MIN_ARG_S           (-(1<<(WIDTH_ARG_S)))
#define MAX_ARG_S           ((1<<WIDTH_ARG_S)-1)
#define MIN_ARG_L           (-(1<<(WIDTH_ARG_L)))
#define MAX_ARG_L           ((1<<WIDTH_ARG_L)-1)
#define MAX_CODE            MAX_ARG_L


/* gets first arg pc */
#define GET_ARG(ip)             ((ip)+SIZE_INSTR)

/* get short/long argument pc */
#define GETPC_S(ip,o)           (GET_ARG(ip)+((o)*SIZE_ARG_S))
#define GETPC_L(ip,o)           (GET_ARG(ip)+((o)*SIZE_ARG_L))


/* get/set short parameter */
#define GET_ARG_S(ip,o)         cast_byte(*GETPC_S(ip,o))
#define SET_ARG_S(ip,o,v)       setbyte(GETPC_S(ip,0), o, v);
#define SET_ARG_LLS(ip,v)       setbyte(GET_ARG(ip), 2*SIZE_ARG_L, v)


/* get/set long arg */
#define GET_ARG_L(ip,o)         get3bytes(GET_ARG(ip) + ((o)*SIZE_ARG_L))
#define SET_ARG_L(ip,o,v)       set3bytes(GETPC_L(ip,o), v)


/*
** Decode short immediate operand by moving the immediate operand
** sign from 8th bit to the 32nd bit.
*/
#define IMM(imm) \
        (((imm)&0x80) ? cast_int(~((imm)&0x7f)+1) : cast_int(imm))

/*
** Decode long immediate operand by moving the immediate operand
** sign from 24th bit to the 32nd bit.
*/
#define IMML(imm) \
        (((imm)&0x00800000) ? cast_int(~((imm)&0xff7fffff)+1) : cast_int(imm))



/* max code jump offset value */
#define MAXJMP      MAX_CODE

#define NOJMP       (-1)  /* value indicating there is no jump */
#define NOPC        NOJMP /* value indicating there is no pc */


/* 
** Binary operations.
*/
typedef enum { /* ORDER OPR */
        /* arithmetic operators */
        OPR_ADD, OPR_SUB, OPR_MUL,
        OPR_DIV, OPR_IDIV, OPR_MOD, OPR_POW,
        /* bitwise operators */
        OPR_SHL, OPR_SHR, OPR_BAND,
        OPR_BOR, OPR_BXOR,
        /* concat operator */
        OPR_CONCAT,
        /* comparison operators */
        OPR_NE, OPR_EQ, OPR_LT,
        OPR_LE, OPR_GT, OPR_GE,
        /* logical operators */
        OPR_AND, OPR_OR,
        OPR_NOBINOPR
} Binopr;


/* true if binary operation 'op' is foldable (arithmetic or bitwise) */
#define oprisfoldable(op)      ((op) <= OPR_BXOR)


/*
** Unary operations.
*/
typedef enum { /* ORDER OP */
    OPR_UNM, OPR_BNOT, OPR_NOT, OPR_NOUNOPR
} Unopr;



typedef enum { /* ORDER OP */
/* ------------------------------------------------------------------------
** Legend for reading OpCodes:
** ':' - value type
** S - short arg (8-bit)
** L - long arg (24-bit)
** V - stack value
** V{x} - stack value at index 'x'
** K{x} - constant at index 'x'
** I(x) - 'x' is immediate operand
** U{x} - upvalue at index 'x'
** OU{x} - open upvalue at index 'x'
** G{x} - global variable, key is K{x}:string
** L{x} - local variable in 'p->locals[x]'
**
** operation     args           description
** ------------------------------------------------------------------------ */
OP_TRUE = 0,/*                'load true constant'                          */
OP_FALSE,/*                   'load false constant'                         */
OP_SUPER,/*        V          'load V.class.superclass'                     */
OP_NIL,/*          L          'load L nils'                                 */
OP_POP,/*          L          'pop L values off the stack'                  */
OP_LOAD,/*         L          'load V{L}'                                   */
OP_CONST,/*        S          'load K{S}'                                   */
OP_CONSTL,/*       L          'load K{L}'                                   */
OP_CONSTI,/*       S          'load integer S'                              */
OP_CONSTIL,/*      L          'load integer L'                              */
OP_CONSTF,/*       S          'load integer S as float'                     */
OP_CONSTFL,/*      L          'load integer L as float'                     */
OP_VARARGPREP,/*   L          'adjust function varargs (L function arity)'  */
OP_VARARG,/*       L          'load L-1 varargs'                            */
OP_CLOSURE,/*      L          'load closure(Enclosing->fns[L])'             */
OP_NEWLIST,/*      S          'create and load new array of size 1<<(S-1)'  */
OP_NEWCLASS,/*     S          'create and load new class of size 1<<(S-1)'  */
OP_NEWTABLE,/*     S          'create and load new table of size 1<<(S-1)'  */
OP_METHOD,/*       L V1 V2    'define method V2 for class V1 under key K{L}'*/
OP_SETMM,/*        S V1 V2    'V1->metalist[S] = V2'                        */

OP_MBIN,/*         V1 V2 S    'V1 S V2'  (S is binop, if S&0x80 swap oper.) */

OP_ADDK,/*         V L     'V + K{L}:number'                                */
OP_SUBK,/*         V L     'V - K{L}:number'                                */
OP_MULK,/*         V L     'V * K{L}:number'                                */
OP_DIVK,/*         V L     'V / K{L}:number'                                */
OP_IDIVK,/*        V L     'V // K{L}:number'                               */
OP_MODK,/*         V L     'V % K{L}:number'                                */
OP_POWK,/*         V L     'V ** K{L}:number'                               */
OP_BSHLK,/*        V L     'V << K{L}:number'                               */
OP_BSHRK,/*        V L     'V >> K{L}:number'                               */
OP_BANDK,/*        V L     'V & K{L}:number'                                */
OP_BORK,/*         V L     'V | K{L}:number'                                */
OP_BXORK,/*        V L     'V ^ K{L}:number'                                */

OP_ADDI,/*         V L     'V + I(L)'                                       */
OP_SUBI,/*         V L     'V - I(L)'                                       */
OP_MULI,/*         V L     'V * I(L)'                                       */
OP_DIVI,/*         V L     'V / I(L)'                                       */
OP_IDIVI,/*        V L     'V // I(L)'                                      */
OP_MODI,/*         V L     'V % I(L)'                                       */
OP_POWI,/*         V L     'V ** I(L)'                                      */
OP_BSHLI,/*        V L     'V << I(L)'                                      */
OP_BSHRI,/*        V L     'V >> I(L)'                                      */
OP_BANDI,/*        V L     'V & I(L)'                                       */
OP_BORI,/*         V L     'V | I(L)'                                       */
OP_BXORI,/*        V L     'V ^ I(L)'                                       */

OP_ADD,/*          V1 V2 S 'V1 + V2'                                        */
OP_SUB,/*          V1 V2 S 'V1 - V2'                                        */
OP_MUL,/*          V1 V2 S 'V1 * V2'                                        */
OP_DIV,/*          V1 V2 S 'V1 / V2  (if (S) swap operands)'                */
OP_IDIV,/*         V1 V2 S 'V1 // V2'                                       */
OP_MOD,/*          V1 V2 S 'V1 % V2'                                        */
OP_POW,/*          V1 V2 S 'V1 ** V2'                                       */
OP_BSHL,/*         V1 V2 S 'V1 << V2'                                       */
OP_BSHR,/*         V1 V2 S 'V1 >> V2'                                       */
OP_BAND,/*         V1 V2 S 'V1 & V2'                                        */
OP_BOR,/*          V1 V2 S 'V1 | V2'                                        */
OP_BXOR,/*         V1 V2 S 'V1 ^ V2'                                        */

OP_CONCAT,/*       L       'V{-L} = V{-L} .. V{L - 1}'                      */

OP_EQK,/*          V L S   '(V == K{L}) == S'                               */

OP_EQI,/*          V L S          '(V == I(L)) == S'                        */
OP_LTI,/*          V L            'V < I(L)'                                */
OP_LEI,/*          V L            'V <= I(L)'                               */
OP_GTI,/*          V L            'V > I(L)'                                */
OP_GEI,/*          V L            'V >= I(L)'                               */ 

OP_EQ,/*           V1 V2 S     '(V1 == V2) == S'                            */
OP_LT,/*           V1 V2 S     '(V1 < V2)  (if (S) swap operands)'          */
OP_LE,/*           V1 V2 S     '(V1 <= V2)'                                 */

OP_EQPRESERVE,/*   V1 V2   'V1 == V2 (preserves V1 operand)'                */

OP_UNM,/*          V       '-V'                                             */
OP_BNOT,/*         V       '~V'                                             */
OP_NOT,/*          V       '!V'                                             */

OP_JMP,/*          L       'pc += L'                                        */
OP_JMPS,/*         L       'pc -= L'                                        */

OP_TEST,/*         V S     'if (!c_isfalse(V) == S) dojump;'                */
OP_TESTPOP,/*      V S     'if (!c_isfalse(V) == S) { dojump; } pop;'       */

OP_CALL,/*  L1 L2  'V{L1},...,V{L1+L2-1} = V{L1}(V{L1+1},...,V{offsp-1})'
                    (check info)                                            */

OP_CLOSE,/*        L           'close all open upvalues >= V{L}'            */
OP_TBC,/*          L           'mark L{L} as to-be-closed'                  */

OP_GETLOCAL,/*     L           'L{L}'                                       */
OP_SETLOCAL,/*     V L         'L{L} = V'                                   */

OP_GETUVAL,/*      L           'U{L}'                                       */
OP_SETUVAL,/*      V L         'U{L} = V'                                   */

OP_SETLIST,/*      L1 L2 S     'V{-L1}[L2+i] = V{-S+i}, 1 <= i <= S         */

OP_SETPROPERTY,/*  V L1 L2     'V{-L1}.K{L2}:string = V'                    */
OP_GETPROPERTY,/*  V  L        'V.K{L}'                                     */

OP_GETINDEX,/*     V1 V2       'V1[V2]'                                     */
OP_SETINDEX,/*     V L         'V{-L}[V{-L + 1}] = V3'                      */

OP_GETINDEXSTR,/*  V L         'V[K{L}:string]'                             */
OP_SETINDEXSTR,/*  V L1 L2     'V{-L1}[K{L2}:string] = V'                   */

OP_GETINDEXINT,/*  V S         'V[I(S):integer]'                            */
OP_GETINDEXINTL,/* V L         'V[I(L):integer]'                            */
OP_SETINDEXINT,/*  V L S       'V{-L}[I(S):integer] = V'                    */
OP_SETINDEXINTL,/* V L1 L2     'V{-L1}[I(L2):integer] = V'                  */

OP_GETSUP,/*       V L         'V.oclass.sclass.methods.K{L}:string'        */
OP_GETSUPIDX,/*    V1 V2       'V1.class.superclass.methods[V2]'            */
OP_GETSUPIDXSTR,/* V L         'V.class.superclass.methods[K{L}:string]'    */

OP_INHERIT,/*     V1 V2        'V2 inherits V1'                             */
OP_FORPREP,/*     L1 L2        'create upvalue V{L1+3}; pc += L2'           */
OP_FORCALL,/*     L1 L2  'V{L1+4},...,V{L1+3+L2} = V{L1}(V{L1+1}, V{L1+2});'*/
OP_FORLOOP,/*L1 L2 L3 'if V{L1+4}!=nil {V{L1}=V{L1+2}; pc-=L2} else pop(L3)'*/

OP_RET,/*         L1 L2 S      'return V{L1}, ... ,V{L1+L2-2}' (check notes)*/
} OpCode;


/*
** Notes:
**
** [OP_CALL]
** L1 is the offset from stack base, where the value being called is located.
** L2 is the number of expected results biased with +1.
** If L2 == 0, then 'sp' is set to last return_result+1.
** 
** [OP_RET]
** L2 is biased with +1, in order to represent multiple returns when the
** number of results is only known during runtime. For example L2 == 0
** represents CS_MULRET, in this case we would return all values up to the
** top. S indicates if current function needs to close any open upvalues or
** to-be-closed variables before returning.
*/


/* number of 'OpCode's */
#define NUM_OPCODES     (OP_RET + 1)


/* instruction format */
typedef enum { /* ORDER OPFMT */
    FormatI,    /* instruction */
    FormatIS,   /* instruction + short arg */
    FormatISS,  /* instruction + 2x short arg */
    FormatIL,   /* instruction + long arg */
    FormatILS,  /* instruction + long arg + short arg */
    FormatILL,  /* instruction + 2x long arg */
    FormatILLS, /* instruction + 2x long arg + short arg */
    FormatILLL, /* instruction + 3x long arg */
    FormatN,    /* total number of instruction formats */
} OpFormat;


#define VD      (INT_MAX) /* flag for variable delta */


typedef struct {
    OpFormat format; /* opcode format */
    int push; /* how many values the opcode pushes */
    int pop; /* how many values the opcode pops */
    c_byte chgsp; /* true if opcode changes value at current stack pointer */
} OpProperties; 


/*
** bits 0-3     instruction format (OpFormat)
** bit  4       instruction is a jump
*/
CSI_DEC(const OpProperties csC_opproperties[NUM_OPCODES];)

#define getopFormat(i)  (csC_opproperties[i].format)
#define getopDelta(i)   (csC_opproperties[i].push - csC_opproperties[i].pop)


/* Instruction format sizes in bytes (or in units of 'Instruction's) */
CSI_DEC(const c_byte csC_opsize[FormatN];)
#define getopSize(i)    (csC_opsize[getopFormat(i)])


/* OpCode names table */ 
CSI_DEC(const char *csC_opname[NUM_OPCODES];)
#define getopName(i)    (csC_opname[i])


/* 
** Number of list items to accumulate before a SETLIST instruction.
** Keep this value under MAX_ARG_S.
*/
#define LISTFIELDS_PER_FLUSH     50


#define csC_store(fs,var)       csC_storevar(fs, var, 0)
#define csC_storepop(fs,var)    csC_storevarpop(fs, var, 0)


#define prevOP(fs)  (((fs)->pc == 0) ? NULL : &(fs)->p->code[(fs)->prevpc])


CSI_FUNC int csC_emitI(FunctionState *fs, Instruction i);
CSI_FUNC int csC_emitIS(FunctionState *fs, Instruction i, int a);
CSI_FUNC int csC_emitIL(FunctionState *fs, Instruction i, int a);
CSI_FUNC int csC_emitILS(FunctionState *fs, Instruction op, int a, int b);
CSI_FUNC int csC_emitILL(FunctionState *fs, Instruction i, int a, int b);
CSI_FUNC int csC_emitILLL(FunctionState *fs, Instruction i, int a, int b, int c);
CSI_FUNC int csC_call(FunctionState *fs, int base, int nreturns);
CSI_FUNC int csC_vararg(FunctionState *fs, int nreturns);
CSI_FUNC void csC_fixline(FunctionState *fs, int line);
CSI_FUNC void csC_removelastjump(FunctionState *fs);
CSI_FUNC void csC_checkstack(FunctionState *fs, int n);
CSI_FUNC void csC_reserveslots(FunctionState *fs, int n);
CSI_FUNC void csC_setreturns(FunctionState *fs, ExpInfo *e, int nreturns);
CSI_FUNC void csC_setmulret(FunctionState *fs, ExpInfo *e);
CSI_FUNC int csC_nil(FunctionState *fs, int n);
CSI_FUNC void csC_load(FunctionState *fs, int stk);
CSI_FUNC int csC_remove(FunctionState *fs, int n);
CSI_FUNC int csC_pop(FunctionState *fs, int n);
CSI_FUNC void csC_adjuststack(FunctionState *fs, int left);
CSI_FUNC int csC_ret(FunctionState *fs, int first, int nreturns);
CSI_FUNC void csC_method(FunctionState *fs, ExpInfo *e);
CSI_FUNC int csC_storevar(FunctionState *fs, ExpInfo *var, int left);
CSI_FUNC void csC_storevarpop(FunctionState *fs, ExpInfo *var, int left);
CSI_FUNC void csC_setlistsize(FunctionState *fs, int pc, int lsz);
CSI_FUNC void csC_setlist(FunctionState *fs, int base, int nelems, int tostore);
CSI_FUNC void csC_settablesize(FunctionState *fs, int pc, int hsize);
CSI_FUNC void csC_const2v(FunctionState *fs, ExpInfo *e, TValue *v);
CSI_FUNC TValue *csC_getconstant(FunctionState *fs, ExpInfo *v);
CSI_FUNC int csC_dischargevars(FunctionState *fs, ExpInfo *e);
CSI_FUNC void csC_exp2stack(FunctionState *fs, ExpInfo *e);
CSI_FUNC void csC_exp2val(FunctionState *fs, ExpInfo *e);
CSI_FUNC void csC_getfield(FunctionState *fs, ExpInfo *var,
                           ExpInfo *keystr, int super);
CSI_FUNC void csC_indexed(FunctionState *fs, ExpInfo *var, ExpInfo *key,
                          int super);
CSI_FUNC void csC_unary(FunctionState *fs, ExpInfo *e, Unopr opr, int line);
CSI_FUNC int csC_jmp(FunctionState *fs, OpCode jop);
CSI_FUNC int csC_test(FunctionState *fs, OpCode optest, int cond, int line);
CSI_FUNC void csC_concatjl(FunctionState *fs, int *l1, int l2);
CSI_FUNC void csC_patch(FunctionState *fs, int pc, int target);
CSI_FUNC void csC_patchtohere(FunctionState *fs, int pc);
CSI_FUNC void csC_prebinary(FunctionState *fs, ExpInfo *e, Binopr op, int line);
CSI_FUNC void csC_binary(FunctionState *fs, ExpInfo *e1, ExpInfo *e2,
                         Binopr opr, int line);
CSI_FUNC void csC_finish(FunctionState *fs);

#endif
