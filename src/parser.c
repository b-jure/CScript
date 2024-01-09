#include "array.h"
#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "err.h"
#include "lexer.h"
#include "mem.h"
#include "object.h"
#include "parser.h"
#include "skconf.h"
#include "sklimits.h"
#include "skmath.h"
#include "value.h"
#include "vmachine.h"


#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>




/* ================== Compile errors ================== */

typedef enum {
    CE_JMPLIMIT,
    CE_LREAD,
    CE_GVARLIMIT,
    CE_LVARLIMIT,
    CE_LREDEF,
    CE_SWDEF,
    CE_SWNOC,
    CE_SWRBR,
    CE_SWDUP,
    CE_CONT,
    CE_BREAK,
    CE_INITRET,
    CE_EXPLIST,
    CE_VARLIST,
    CE_SCOPE,
    CE_UVALLIMIT,
    CE_INHERIT,
    CE_VARARG,
    CE_NAMELIST,
    CE_CALLCONST,
    CE_SELF,
    CE_SUPER,
    CE_NOSUPER,
} CompErr;

const char* comperrors[] = {
    "\tToo much code to jump over, limit is '%d'.\n",
    "\tCan't read local variable %.*s in its own initializer.\n",
    "\tToo many global values defined in script, limit is '%d'.\n",
    "\tToo many local values defined in script, limit is '%d'.\n",
    "\tRedefinition of local variable '%.*s'.\n",
    "\tMultiple 'default' labels.\n",
    "\tCan't have statements before first case.\n",
    "\tExpect '}' at the end of 'switch'.\n",
    "\tAlready have case with constant '%s'.\n",
    "\t'continue' statement not in loop statement.\n",
    "\t'break' statement not in loop or switch statement.\n",
    "\tCan't return a value from '%s' method.\n",
    "\tToo many expressions in explist, expected at most '%d'.\n",
    "\tToo many variables in varlist, limit is '%d'.\n",
    "\tScope nesting limit reached, limit is '%d'.\n",
    "\tToo many upvalues in '%s', limit is '%d'.\n",
    "\t<class '%s'> can't 'impl' itself.\n",
    "\t'...' can only be used inside functions that accept variable number of "
    "arguments.\n",
    "\tToo many names in namelist, limit is '%d'\n",
    "\tAttempted to call a constant value.\n",
    "\tCan't use 'self' outside of a class.\n",
    "\tCan't use 'super' outside of a class.\n",
    "\tCan't use 'super', class does not have a superclass.\n",
};

/* ----------------------------------------------------------- */ // Compile errors




// Get instruction length
#define GET_OP_TYPE(idx, op, E)                                                          \
    ((idx <= UINT8_MAX) ? ((E)->ins.l = false, op) : ((E)->ins.l = true, op##L))

// Return current chunk code offset
#define codeoffset(F) (CHUNK(F)->code.len)

// Any 'stack' size limit
#define INDEX_MAX (MIN(VM_STACK_MAX, BYTECODE_MAX))








// Local variable flags
#define VFIXED_BIT    (1) // Variable is 'fixed' (immutable)
#define VCAPTURED_BIT (2) // Variable is captured by closure
// 3..8 - unused

// Local variable flag setters and getters
#define LFLAG_SET(var, bit)   BIT_SET((var)->flags, bit)
#define LFLAG_CHECK(var, bit) BIT_CHECK((var)->flags, bit)
#define LFLAGS(var)           ((var)->flags)

// Local variable
typedef struct {
    Token name; // Variable name
    int32_t depth; // Scope depth where variable is defined
                   // can be -1 if the variable is not initialized
    uint8_t flags; // Flags that represent some property of this
                   // variable (check above the V[FLAGNAME]_BIT)
} Local;

// Initialize local variable
#define INIT_LOCAL(F, i)                                                                 \
    do {                                                                                 \
        Local* local = Array_Local_index(&(F)->locals, (F)->locals.len - (i + 1));       \
        local->depth = (F)->S->depth;                                                    \
        local->flags = (F)->vflags;                                                      \
    } while(false)








ARRAY_NEW(Array_Int, Int);
ARRAY_NEW(Array_Array_Int, Array_Int)
typedef struct { /* Control Flow context */
    /* We store break offsets inside an array because we don't
     * know in advance where the end of the switch/loop statement
     * is while parsing.
     * When we hit the end of the loop/switch statement,
     * we can just patch each parsed break statement.
     * This approach would be similar for 'continue' in
     * case of 'do while' loop.*/
    Array_Array_Int breaks; /* Break statement offsets */

    int32_t innerlstart; /* Innermost loop start offset */
    int32_t innerldepth; /* Innermost loop scope depth */
    int32_t innersdepth; /* Innermost switch scope depth */
} ControlFlow;








/**
 * UpValue is a local variable that is defined inside of the enclosing function.
 * Contains stack index of that variable inside of the function that encloses,
 * or the index of that upvalue in the enclosing function if the variable to be
 * captured is located outside of the enclosing function (either global scope or
 * another function).
 * The 'local' field indicates if the capture occurred in the enclosing
 * function.
 **/
typedef struct {
    uint32_t idx; // Stack index
    uint8_t flags; // Local flags
    bool local;
} Upvalue;








// In case parser is parsing a class declaration,
// then it will contain at least a single 'Class' struct,
// this serves as additional information when we are parsing
// a function declaration inside of a class 'method' function declaration
// in order to know if we are allowed to use 'self' and/or 'super' keywords to
// reference the currently parsed class (if any) and/or superclass if
// class has a superclass.
typedef struct Class {
    struct Class* enclosing;
    bool superclass;
} Class;








typedef enum {
    EXP_NONE = 0,
    EXP_FALSE,
    EXP_NIL,
    EXP_TRUE,
    EXP_STRING,
    EXP_NUMBER,
    EXP_UPVAL,
    EXP_LOCAL,
    EXP_GLOBAL,
    EXP_INDEXED,
    EXP_CALL,
    EXP_INVOKE_INDEX,
    EXP_INVOKE,
    EXP_VARARG,
    EXP_EXPR,
    EXP_JMP,
} ExpType;

#define etisconst(exptype)   ((exptype) >= EXP_FALSE && (exptype) <= EXP_NUMBER)
#define etisfalse(exptype)   ((exptype) >= EXP_FALSE && (exptype) <= EXP_NIL)
#define etistrue(exptype)    ((exptype) >= EXP_TRUE && (exptype) <= EXP_NUMBER)
#define etisvar(exptype)     ((exptype) >= EXP_UPVAL && (exptype) <= EXP_INDEXED)
#define etiscall(exptype)    ((exptype) >= EXP_CALL || (exptype) <= EXP_INVOKE)
#define ethasmulret(exptype) ((exptype) >= EXP_CALL && (exptype) <= EXP_VARARG)
#define etisliteral(exptype) ((exptype) >= EXP_FALSE && (exptype) <= EXP_TRUE)

#define MULRET    0
#define SINGLEVAL 1
#define NO_JMP    -1
#define NO_CODE   -1
#define NO_VAL    -1

// Expression description
typedef struct {
    ExpType type; // expression type
    struct {
        int32_t t; // jmp to patch if 'true'
        int32_t f; // jmp to patch if 'false'
    } jmp; // code jumps
    struct {
        int32_t code; // instruction index
        bool l; // is this long instruction
        bool set; // should it be a setter or getter
        bool binop; // is this instruction a simple binary operator
    } ins; // instruction info
    int32_t value; // expression value or index
} Exp;

// Initialize expression
static force_inline void Exp_init(Exp* E, ExpType type, int32_t code, int32_t value)
{
    E->type = type;
    E->jmp.t = NO_JMP;
    E->jmp.f = NO_JMP;
    E->ins.code = code;
    E->ins.binop = false;
    E->value = value;
}

// Get current chunk
#define CHUNK(F) (&(F)->fn->chunk)
// Get constant
#define CONSTANT(F, E) Array_Value_index(&CHUNK(F)->constants, (E)->value)
// Get instruction
#define INSTRUCTION(F, E) byteptr(Array_Byte_index(&CHUNK(F)->code, (E)->ins.code))

// Expressions are constants and their Values are equal
#define eareconstandeq(F, E1, E2)                                                        \
    (etisconst((E1)->type) && etisconst((E2)->type) &&                                   \
     raweq((F)->vm, *CONSTANT(F, E1), *CONSTANT(F, E2)))


// Set first long parameter
#define SET_LPARAM(F, E, val) PUT_BYTES3(INSTRUCTION(F, E) + 1, val)
// Set second long parameter
#define SET_LLPARAM(F, E, val) PUT_BYTES3(INSTRUCTION(F, E) + 4, val)
// Set first short parameter
#define SET_SPARAM(F, E, val) PUT_BYTE(INSTRUCTION(F, E) + 1, val)
// Set instruction return count (first param)
#define SET_RETCNT(F, E, cnt) SET_LPARAM(F, E, cnt)
// Set instruction return count (second param)
#define SET_RETCNTL(F, E, cnt) SET_LLPARAM(F, E, cnt)

// Pop last instruction parameter (1 byte)
#define PARAM_POP(F)                                                                     \
    do {                                                                                 \
        sk_assert((F)->vm, CHUNK(F)->code.len >= 1, "Invalid PARAM_POP.");               \
        CHUNK(F)->code.len--;                                                            \
    } while(false)
// Pop last instruction long parameter (3 bytes)
#define LPARAM_POP(F)                                                                    \
    do {                                                                                 \
        sk_assert((F)->vm, CHUNK(F)->code.len >= 3, "Invalid LPARAM_POP.");              \
        CHUNK(F)->code.len -= 3;                                                         \
    } while(false)
// Pop last short/simple instruction (1 byte)
#define SINSTRUCTION_POP(F)                                                              \
    do {                                                                                 \
        sk_assert((F)->vm, CHUNK(F)->code.len >= 1, "Invalid SINSTRUCTION_POP.");        \
        CHUNK(F)->code.len--;                                                            \
    } while(false)
// Pop last instruction with parameter (2 bytes)
#define INSTRUCTION_POP(F)                                                               \
    do {                                                                                 \
        sk_assert((F)->vm, CHUNK(F)->code.len >= 2, "Invalid INSTRUCTION_POP.");         \
        CHUNK(F)->code.len -= 2;                                                         \
    } while(false)
// Pop last instruction with long parameter (4 bytes)
#define LINSTRUCTION_POP(F)                                                              \
    do {                                                                                 \
        sk_assert((F)->vm, CHUNK(F)->code.len >= 4, "Invalid LINSTRUCTION_POP.");        \
        CHUNK(F)->code.len -= 4;                                                         \
    } while(false)

// Pop last constant
#define CONSTANT_POP(F)                                                                  \
    do {                                                                                 \
        sk_assert((F)->vm, CHUNK(F)->constants.len > 0, "Invalid CONSTANT_POP.");        \
        Array_Value_pop(&CHUNK(F)->constants);                                           \
    } while(false)








typedef struct Scope Scope;
struct Scope {
    Scope* prev; // Linked List
    uint32_t localc; // Count of locals up until this scope
    uint8_t isloop : 1; // This scope is loop
    uint8_t isswitch : 1; // This scope is switch
    uint8_t isgloop : 1; // This scope is generic loop
    int32_t depth; // scope depth (index)
};

/*
 * Returns 1 if 'S' is in loop, 0 if inside of a switch
 * and -1 if outside.
 */
static force_inline int32_t inwhat(Scope* S, Scope** target)
{
    int8_t which = -1;
    Scope* head = S;
    for(; head != NULL; head = head->prev) {
        if(head->isswitch) {
            which = 0;
            break;
        }
        if(head->isloop) {
            which = 1;
            break;
        }
    }
    *target = head;
    return which;
}

#define inswitch(S) (inwhat(S) == 0)
#define inloop(S)   (inwhat(S) == 1)








// Type of function being parsed
typedef enum {
    FN_FUNCTION, // Normal function declaration
    FN_METHOD, // Class method function
    FN_INIT, // Class initializer function
    FN_SCRIPT, // Top-level code (implicit function)
} FunctionType;

#define FCLEAR(F, modifier) bclear((F)->vflags, modifier)
#define FSET(F, modifier)   bset((F)->vflags, modifier)
#define FIS(F, modifier)    btest((F)->vflags, modifier)

#define FFIXED 1 // Variable is fixed

ARRAY_NEW(Array_Local, Local);
ARRAY_NEW(Array_Upvalue, Upvalue);

struct Function {
    Function* enclosing; // chain
    VM* vm; // virtual machine
    Class* cclass; // class declaration state
    Lexer* lexer; // grammar lexer
    ControlFlow cflow; // control flow context
    Scope* S; // scope state
    OFunction* fn; // currently parsed function (bytecode chunk)
    FunctionType fn_type;
    Array_Upvalue* upvalues; // captured variables
    uint8_t vflags; // variable flags
    Array_Local locals; // local variables stack
};

static force_inline Scope* getscope(Function* F, int32_t depth)
{
    Scope* scope = F->S;
    while(scope != NULL) {
        if(scope->depth == depth) return scope;
        scope = scope->prev;
    }
    return NULL;
}


/* Default 'Function' stack size */
#define SHORT_STACK_SIZE UINT8_MAX

/* Tokens (previous and current) */
#define PREVT(F) (F)->lexer->previous
#define CURRT(F) (F)->lexer->current

typedef struct {
    int32_t codeoffset;
    int32_t constlen;
    int32_t localc;
    int32_t upvalc;
} Context;

static force_inline void savecontext(Function* F, Context* C)
{
    C->codeoffset = codeoffset(F);
    C->constlen = CHUNK(F)->constants.len;
    C->localc = F->locals.len;
    C->upvalc = F->upvalues->len;
}

// Trim/set length of code and/or constant array
static force_inline void concatcode(Function* F, int32_t codeoffset, int32_t constoffset)
{
    CHUNK(F)->code.len = codeoffset;
    CHUNK(F)->constants.len = constoffset;
}

static force_inline void restorecontext(Function* F, Context* C)
{
    concatcode(F, C->codeoffset, C->constlen);
    F->locals.len = C->localc;
    F->upvalues->len = C->upvalc;
}








// Forward declare
static void dec(Function* F);
static void expr(Function* F, Exp* E);
static void suffixedexp(Function* F, Exp* E);
static void stm(Function* F);








/*========================== ERROR =========================*/

// Compile-time error
static void error(Function* F, const char* error, ...)
{
    Token* token = &PREVT(F);
    va_list args;
    sk_assert(vm, token->type != TOK_ERROR, "Only lexer can register error tokens.");
    // If panic bit is on, then sync the lexer before registering any new errors.
    if(F->lexer->panic) return;
    va_start(args, error);
    regcomperror(F->lexer, error, args);
    va_end(args);
}








//======================= CODE =======================//

// Multi-byte instruction (up to 4 bytes)
#define CODEOP(F, code, param)                                                           \
    Chunk_write_codewparam(CHUNK(F), code, param, PREVT(F).line)

// Single byte instruction
#define CODE(F, byte)                                                                    \
    ({                                                                                   \
        (F)->fn->gotret = ((byte) == OP_RET);                                            \
        Chunk_write(CHUNK(F), byte, PREVT(F).line);                                      \
    })

// 3 byte parameter
#define CODEL(F, bytes)                                                                  \
    do {                                                                                 \
        CODE(F, BYTE(bytes, 0));                                                         \
        CODE(F, BYTE(bytes, 1));                                                         \
        CODE(F, BYTE(bytes, 2));                                                         \
    } while(false)

// Emit jump instruction
#define CODEJMP(F, jmp)                                                                  \
    ({                                                                                   \
        CODEOP(F, jmp, 0);                                                               \
        codeoffset(F) - 3;                                                               \
    })

// Emit pop instruction
#define CODEPOP(F, n)                                                                    \
    do {                                                                                 \
        if(n > 0) {                                                                      \
            if((n) > 1) CODEOP(F, OP_POPN, n);                                           \
            else CODE(F, OP_POP);                                                        \
        }                                                                                \
    } while(false)

// Emit unary instruction
#define CODEUN(F, opr) CODE(F, unopr2op(opr))

// Emit binary instruction
#define CODEBIN(F, opr) CODE(F, binopr2op(opr))

// Emit return instruction
static force_inline void coderet(Function* F, bool explicit, bool gotret)
{
    if(gotret) {
        F->fn->gotret = 1;
        return;
    }
    if(!explicit) CODE(F, OP_RETSTART);
    if(F->fn_type == FN_INIT) CODEOP(F, OP_GET_LOCAL, 0);
    else CODE(F, OP_NIL);
    CODE(F, OP_RET);
}

// Emit loop instruction
static force_inline void codeloop(Function* F, uint32_t start)
{
    CODE(F, OP_LOOP);
    uint32_t offset = codeoffset(F) - start + 3;
    if(unlikely(offset >= VM_JMP_LIMIT)) error(F, comperrors[CE_JMPLIMIT], VM_JMP_LIMIT);
    CODEL(F, offset);
}

// Initialize global variable
#define INIT_GLOBAL(F, idx, vflags, E)                                                   \
    do {                                                                                 \
        (F)->vm->globvars.data[idx].flags = vflags;                                      \
        CODEOP(F, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, E), idx);                           \
    } while(false)


/* Check if token names are equal */
#define nameeq(l, r)                                                                     \
    (((l)->len == (r)->len) && (memcmp((l)->start, (r)->start, (l)->len) == 0))


// Get local variable
static force_inline int32_t get_local(Function* F, Token* name)
{
    for(int32_t i = F->locals.len - 1; i >= 0; i--) {
        Local* local = Array_Local_index(&F->locals, i);
        if(nameeq(name, &local->name)) {
            if(unlikely(local->depth == -1))
                error(F, comperrors[CE_LREAD], name->len, name->start);
            return i;
        }
    }
    return -1;
}

static uint32_t add_upval(Function* F, uint32_t idx, uint8_t flags, bool local)
{
    int32_t upvalc = F->fn->upvalc;
    sk_assert(F->vm, upvalc < cast_int(F->upvalues->len + 1), "Invalid upvalc.");
    for(int32_t i = 0; i < upvalc; i++) {
        Upvalue* upvalue = Array_Upvalue_index(F->upvalues, i);
        if(upvalue->idx == idx && upvalue->local == local) return i; // already exists
    }
    if(unlikely(upvalc >= VM_STACK_LIMIT)) {
        error(F, comperrors[CE_UVALLIMIT], VM_STACK_LIMIT, F->fn->name->storage);
        return 0;
    }
    Upvalue upval = {idx, flags, local};
    if(upvalc == cast_int(F->upvalues->len)) Array_Upvalue_push(F->upvalues, upval);
    else *Array_Upvalue_index(F->upvalues, upvalc) = upval;
    return F->fn->upvalc++;
}

static int32_t get_upval(Function* F, Token* name)
{
    if(F->enclosing == NULL) return -1;
    int32_t idx = get_local(F->enclosing, name);
    if(idx != -1) {
        Local* l = Array_Local_index(&F->enclosing->locals, idx);
        bset(l->flags, VCAPTURED_BIT);
        return add_upval(F, cast_uint(idx), l->flags, true);
    }
    idx = get_upval(F->enclosing, name);
    if(idx != -1) {
        Local* l = Array_Local_index(&F->enclosing->locals, idx);
        return add_upval(F, cast_uint(idx), l->flags, false);
    }
    return -1;
}

static uint32_t globalvar(Function* F, Value identifier)
{
    Value idx;
    Variable glob = {UNDEFINED_VAL, F->vflags};
    VM* vm = F->vm;
    if(!HashTable_get(&vm->globids, identifier, &idx)) {
        if(unlikely((vm)->globvars.len + 1 > VM_GVAR_LIMIT))
            error(F, comperrors[CE_GVARLIMIT], VM_GVAR_LIMIT);
        push(vm, identifier);
        idx = NUMBER_VAL(Array_Variable_push(&vm->globvars, glob));
        HashTable_insert(vm, &vm->globids, identifier, idx);
        pop(vm);
    }
    return cast_uint(AS_NUMBER(idx));
}

#define tokintostr(vm, name) OBJ_VAL(OString_new(vm, (name)->start, (name)->len))

// Make global variable
#define MAKE_GLOBAL(F, name)                                                             \
    ({                                                                                   \
        Value identifier = tokintostr((F)->vm, name);                                    \
        globalvar(F, identifier);                                                        \
    })

static int32_t codevar(Function* F, Token name, Exp* E)
{
    OpCode getop;
    int32_t idx = get_local(F, &name);
    if(idx != -1) {
        E->type = EXP_LOCAL;
        getop = GET_OP_TYPE(idx, OP_GET_LOCAL, E);
    } else if((idx = get_upval(F, &name)) != -1) {
        E->type = EXP_UPVAL;
        E->ins.l = true;
        getop = OP_GET_UPVALUE;
    } else {
        E->type = EXP_GLOBAL;
        idx = MAKE_GLOBAL(F, &name);
        getop = GET_OP_TYPE(idx, OP_GET_GLOBAL, E);
    }
    E->value = idx;
    if(!E->ins.set) return (E->ins.code = CODEOP(F, getop, idx));
    else return (E->ins.code = -1); // this is assignment
}

static force_inline uint32_t codevarprev(Function* F, Exp* E)
{
    return codevar(F, PREVT(F), E);
}

// helper [rmlastins]
static force_inline void popvarins(Function* F, Exp* E)
{
    switch(E->type) {
        case EXP_UPVAL:
        case EXP_LOCAL:
        case EXP_GLOBAL:
            if(E->ins.l) LINSTRUCTION_POP(F);
            else INSTRUCTION_POP(F);
            break;
        case EXP_INDEXED:
            // @?: setters are not reachable ?
            switch(*INSTRUCTION(F, E)) {
                case OP_INDEX:
                    SINSTRUCTION_POP(F);
                    break;
                case OP_GET_PROPERTY:
                case OP_GET_SUPER:
                    LINSTRUCTION_POP(F);
                    break;
                default:
                    unreachable;
            }
            break;
        default:
            unreachable;
    }
}

// helper [rmlastins]
static force_inline void popcallins(Function* F, Exp* E)
{
    switch(E->type) {
        case EXP_CALL:
        case EXP_INVOKE_INDEX:
            LINSTRUCTION_POP(F);
            break;
        case EXP_INVOKE:
            LINSTRUCTION_POP(F);
            LPARAM_POP(F);
            break;
        default:
            unreachable;
    }
}

static void rmlastins(Function* F, Exp* E)
{
    ExpType type = E->type;
    if(etisliteral(type)) SINSTRUCTION_POP(F);
    else if(etisconst(type)) LINSTRUCTION_POP(F);
    else if(etisvar(type)) popvarins(F, E);
    else if(etiscall(type)) popcallins(F, E);
    else switch(type)
        {
            case EXP_JMP:
                goto panic;
            case EXP_EXPR:
                if(E->ins.binop) {
                    LINSTRUCTION_POP(F);
                    break;
                } else {
                panic: // FALLTHRU
                    PANIC("Tried removing 'and'/'or' expression.");
                    unreachable;
                }
            default:
                unreachable;
        }
}








//======================= SCOPE/CFLOW =======================//

// Start new 'Scope'
static force_inline void
startscope(Function* F, Scope* S, uint8_t isloop, uint8_t isswitch)
{
    if(unlikely(F->S->depth >= SK_BYTECODE_MAX))
        error(F, comperrors[CE_SCOPE], SK_BYTECODE_MAX);
    S->localc = F->locals.len;
    S->isloop = isloop;
    S->isswitch = isswitch;
    S->depth = F->S->depth + 1;
    S->prev = F->S;
    F->S = S;
}

// End scope and pop locals and/or close captured locals
static void endscope(Function* F)
{
#define LOCAL_IS_CAPTURED(local) (btest((local)->flags, VCAPTURED_BIT))

    F->fn->gotret = 0;
    int32_t pop = 0;
    Scope* current = F->S;
    F->S = current->prev;
    while(F->locals.len > 0 && Array_Local_last(&F->locals)->depth > F->S->depth) {
        if(LOCAL_IS_CAPTURED(Array_Local_last(&F->locals))) {
            int32_t capture = 1;
            F->locals.len--;
            CODEPOP(F, pop);
            pop = 0; // Reset pop count
            do {
                if(!LOCAL_IS_CAPTURED(Array_Local_last(&F->locals))) {
                    if(capture == 1) CODE(F, OP_CLOSE_UPVAL);
                    else CODEOP(F, OP_CLOSE_UPVALN, capture);
                    break;
                }
                capture++;
                F->locals.len--;
            } while(F->locals.len > 0 &&
                    Array_Local_last(&F->locals)->depth > F->S->depth);
        } else {
            pop++;
            F->locals.len--;
        }
    }
    CODEPOP(F, pop);

#undef LOCAL_IS_CAPTURED
}


static void ControlFlow_init(VM* vm, ControlFlow* cflow)
{
    cflow->innerlstart = -1;
    cflow->innerldepth = 0;
    cflow->innersdepth = 0;
    Array_Array_Int_init(&cflow->breaks, vm);
}

static void ControlFlow_free(ControlFlow* context)
{
    Array_Array_Int_free(&context->breaks, NULL);
    context->innerlstart = -1;
    context->innerldepth = 0;
    context->innersdepth = 0;
}








/*========================== FUNCTION STATE =========================*/

static void F_init(
    Function* F,
    Scope* globscope,
    Class* cclass,
    VM* vm,
    Lexer* lexer,
    FunctionType fn_type,
    Value loaded,
    Function* enclosing)
{
    // Initialize global scope
    globscope->prev = NULL;
    globscope->depth = 0;
    globscope->isloop = 0;
    globscope->isswitch = 0;
    globscope->localc = 1;
    // Initialize Function state
    F->vm = vm;
    F->S = globscope;
    F->enclosing = enclosing;
    F->cclass = cclass;
    F->lexer = lexer;
    vm->F = F;
    F->fn = NULL; // Initialize to NULL so gc does not get confused
    F->fn = OFunction_new(vm);
    F->fn_type = fn_type;
    F->vflags = 0;
    ControlFlow_init(vm, &F->cflow);
    if(enclosing == NULL) {
        F->upvalues = MALLOC(vm, sizeof(Array_Upvalue));
        Array_Upvalue_init(F->upvalues, vm);
    } else F->upvalues = enclosing->upvalues;
    Array_Local_init(&F->locals, vm);
    Array_Local_init_cap(&F->locals, SHORT_STACK_SIZE);
    /* Reserve first stack slot for VM ('self' ObjInstance) */
    F->locals.len++; // Safe, we already initialized array to capacity
    Local* local = F->locals.data;
    local->depth = 0;
    local->flags = 0;
    if(fn_type != FN_FUNCTION) {
        local->name.start = "self";
        local->name.len = 4;
    } else {
        local->name.start = "";
        local->name.len = 0;
    }
    if(fn_type == FN_SCRIPT) F->fn->name = AS_STRING(loaded);
    else F->fn->name = OString_new(vm, PREVT(F).start, PREVT(F).len);
}

void F_free(Function* F)
{
    VM* vm = F->vm;
    ControlFlow_free(&F->cflow);
    if(F->enclosing == NULL) {
        sk_assert(
            F->vm,
            F->fn_type == FN_SCRIPT,
            "Function is top-level but the type is not 'FN_SCRIPT'.");
        Array_Upvalue_free(F->upvalues, NULL);
        FREE(vm, F->upvalues);
    }
    Array_Local_free(&F->locals, NULL);
    vm->F = F->enclosing;
    FREE(vm, F);
}

// Cleanup the function stack in case of internal errors
void _cleanup_function(Function* F)
{
    if(F != NULL) {
        FREE(F->vm, (char*)F->lexer->source);
        for(Function* fn = F; fn != NULL; fn = fn->enclosing)
            F_free(F);
    }
}

void mark_function_roots(VM* vm)
{
    // Mark all functions and token values
    for(Function* current = vm->F; current != NULL; current = current->enclosing) {
        vmark(vm, PREVT(current).value);
        vmark(vm, CURRT(current).value);
        omark(vm, (O*)current->fn);
    }
}








/*========================== PARSING =========================*/

// Advance to the next token
static void advance(Function* F)
{
    PREVT(F) = CURRT(F);
    while(true) {
        CURRT(F) = scan(F->lexer);
        if(CURRT(F).type != TOK_ERROR) break;
    }
}

// Advance and return true if 'type' matches the current token type
static force_inline bool match(Function* F, TokenType type)
{
    if(CURRT(F).type != type) return false;
    advance(F);
    return true;
}

// Sync lexer to the next statement
static void sync(Function* F)
{
    F->lexer->panic = false;
    while(CURRT(F).type != TOK_EOF) {
        if(PREVT(F).type == TOK_SEMICOLON) return;
        switch(CURRT(F).type) {
            case TOK_FOR:
            case TOK_FN:
            case TOK_VAR:
            case TOK_CLASS:
            case TOK_IF:
            case TOK_RETURN:
            case TOK_CONTINUE:
            case TOK_BREAK:
            case TOK_WHILE:
            case TOK_FOREACH:
            case TOK_LOOP:
            case TOK_SWITCH:
                return;
            default:
                advance(F);
                break;
        }
    }
}

// Check if the token type matches the current token type
#define check(F, toktype) (CURRT(F).type == (toktype))

// Invoke compile-time error if 'cond' is false
#define expect_cond(F, cond, err)                                                        \
    if(!cond) error(F, err);

// Invoke compile-time error if 'type' does not match the current token type
static force_inline void expect(Function* F, TokenType type, const char* err)
{
    if(CURRT(F).type == type) {
        advance(F);
        return;
    }
    error(F, err);
}

#define expectstr(str) "\t" str "\n"

// End compilation of the function and emit return instruction
static force_inline OFunction* compile_end(Function* F)
{
    coderet(F, false, F->fn->gotret);
#ifdef SK_DEBUG_PRINT_CODE
    if(!F->lexer->error) {
        OFunction* fn = F->fn;
        Chunk_debug(F->vm, CHUNK(F), fn->name->storage);
    }
#endif
    return F->fn;
}

// Compile source code
OClosure* compile(VM* vm, const char* source, Value name, bool globscope)
{
    vm->script = name;
    HashTable_insert(vm, &vm->loaded, name, EMPTY_VAL);
    Function* F = MALLOC(vm, sizeof(Function));
    Lexer L = L_new(source, vm);
    Scope globalscope, local;
    F_init(F, &globalscope, NULL, vm, &L, FN_SCRIPT, name, vm->F);
    if(!globscope) startscope(F, &local, 0, 0);
    advance(F);
    while(!match(F, TOK_EOF))
        dec(F);
    OFunction* fn = compile_end(F);
    // no need to end scope if 'globscope' is true
    bool err = F->lexer->error;
    F_free(F);
    push(vm, OBJ_VAL(fn));
    OClosure* closure = OClosure_new(vm, fn);
    pop(vm);
    return (err ? NULL : closure);
}

// Create new local variable
static force_inline void local_new(Function* F, Token name)
{
    if(unlikely(cast_int(F->locals.len) >= VM_LVAR_LIMIT)) {
        error(F, comperrors[CE_LVARLIMIT], VM_LVAR_LIMIT);
        return;
    }
    Array_Local_push(&F->locals, (Local){name, -1, F->vflags});
}

// Make local variable but check for redefinitions in local scope
static void make_local(Function* F, Token* name)
{
    for(int32_t i = F->locals.len - 1; i >= 0; i--) {
        Local* local = Array_Local_index(&F->locals, i);
        if(local->depth != -1 && local->depth < F->S->depth) break;
        if(unlikely(nameeq(name, &local->name)))
            error(F, comperrors[CE_LREDEF], name->len, name->start);
    }
    local_new(F, *name);
}

// Patch jump instruction
static force_inline void patchjmp(Function* F, int32_t jmp_offset)
{
    int32_t offset = codeoffset(F) - jmp_offset - 3;
    if(unlikely(offset >= VM_JMP_LIMIT)) error(F, comperrors[CE_JMPLIMIT], VM_JMP_LIMIT);
    PUT_BYTES3(&CHUNK(F)->code.data[jmp_offset], offset);
}

static force_inline void startbreaklist(Function* F)
{
    Array_Int patches;
    Array_Int_init(&patches, F->vm);
    Array_Array_Int_push(&F->cflow.breaks, patches);
}

static force_inline void patchbreaklist(Function* F)
{
    Array_Int* patches = Array_Array_Int_last(&F->cflow.breaks);
    for(int32_t i = 0; i < cast_int(patches->len); i++)
        patchjmp(F, patches->data[i]);
}

static force_inline void endbreaklist(Function* F)
{
    Array_Int last = Array_Array_Int_pop(&F->cflow.breaks);
    Array_Int_free(&last, NULL);
}


static force_inline uint32_t make_constant(Function* F, Value constant)
{
    if(unlikely(cast_int(CHUNK(F)->constants.len) > VM_CONST_LIMIT)) {
        error(
            F,
            "Too many constants created in this chunk, limit is '%d'.\n",
            VM_CONST_LIMIT);
    }
    return Chunk_make_constant(F->vm, CHUNK(F), constant);
}








/*========================== OPERATIONS/PRIORITY =========================*/

typedef enum {
    OPR_NOT = 0,
    OPR_NEGATE,
    OPR_NOUNARYOPR,
} UnaryOpr;

typedef enum {
    OPR_ADD = 0,
    OPR_SUB,
    OPR_MUL,
    OPR_DIV,
    OPR_MOD,
    OPR_POW,
    OPR_NE,
    OPR_EQ,
    OPR_LT,
    OPR_LE,
    OPR_GT,
    OPR_GE,
    OPR_AND,
    OPR_OR,
    OPR_NOBINOPR,
} BinaryOpr;

#define FOLDABLE(opr) ((opr) <= OPR_POW)

// Fetch unary operation
static UnaryOpr getunaryopr(TokenType type)
{
    switch(type) {
        case TOK_BANG:
            return OPR_NOT;
        case TOK_MINUS:
            return OPR_NEGATE;
        default:
            return OPR_NOUNARYOPR;
    }
}

// Fetch binary operation
static BinaryOpr getbinaryopr(TokenType type)
{
    switch(type) {
        case TOK_PLUS:
            return OPR_ADD;
        case TOK_MINUS:
            return OPR_SUB;
        case TOK_STAR:
            return OPR_MUL;
        case TOK_SLASH:
            return OPR_DIV;
        case TOK_PERCENT:
            return OPR_MOD;
        case TOK_CARET:
            return OPR_POW;
        case TOK_BANG_EQUAL:
            return OPR_NE;
        case TOK_EQUAL_EQUAL:
            return OPR_EQ;
        case TOK_LESS:
            return OPR_LT;
        case TOK_LESS_EQUAL:
            return OPR_LE;
        case TOK_GREATER:
            return OPR_GT;
        case TOK_GREATER_EQUAL:
            return OPR_GE;
        case TOK_AND:
            return OPR_AND;
        case TOK_OR:
            return OPR_OR;
        default:
            return OPR_NOBINOPR;
    }
}

// Fetch unary instruction
static OpCode unopr2op(UnaryOpr opr)
{
    switch(opr) {
        case OPR_NOT:
            return OP_NOT;
        case OPR_NEGATE:
            return OP_NEG;
        default:
            unreachable;
    }
}

// Fetch binary instruction
static OpCode binopr2op(BinaryOpr opr)
{
    switch(opr) {
        case OPR_ADD:
            return OP_ADD;
        case OPR_SUB:
            return OP_SUB;
        case OPR_MUL:
            return OP_MUL;
        case OPR_DIV:
            return OP_DIV;
        case OPR_MOD:
            return OP_MOD;
        case OPR_POW:
            return OP_POW;
        case OPR_NE:
            return OP_NOT_EQUAL;
        case OPR_EQ:
            return OP_EQUAL;
        case OPR_LT:
            return OP_LESS;
        case OPR_LE:
            return OP_LESS_EQUAL;
        case OPR_GT:
            return OP_GREATER;
        case OPR_GE:
            return OP_GREATER_EQUAL;
        default:
            /// OPR_AND and OPR_OR do not emit direct bytecode,
            /// they are instead sets of OP_JMP and OP_JMP_IF_FALSE instructions
            unreachable;
    }
}

static const struct {
    uint8_t left; // Left priority
    uint8_t right; // Right priority
} priority[] = {
    [OPR_ADD] = {4, 4}, /* '+' */
    [OPR_SUB] = {4, 4}, /* '-' */
    [OPR_MUL] = {5, 5}, /* '*' */
    [OPR_DIV] = {5, 5}, /* '/' */
    [OPR_MOD] = {5, 5}, /* '%' */
    [OPR_POW] = {8, 7}, /* '^' (right associative) */
    [OPR_NE] = {3, 3}, /* '!=' */
    [OPR_EQ] = {3, 3}, /* '==' */
    [OPR_LT] = {3, 3}, /* '<' */
    [OPR_LE] = {3, 3}, /* '<=' */
    [OPR_GT] = {3, 3}, /* '>' */
    [OPR_GE] = {3, 3}, /* '>=' */
    [OPR_AND] = {2, 2}, /* 'and' */
    [OPR_OR] = {1, 1}, /* 'or' */
};

#define UNARY_PRIORITY 6








/*========================== STATEMENT/DECLARATION =========================*/

// vararg ::= '...'
static force_inline uint32_t vararg(Function* F)
{
    if(!F->fn->isva) error(F, comperrors[CE_VARARG]);
    return CODEOP(F, OP_VALIST, 1);
}

static void setmulret(Function* F, Exp* E)
{
    switch(E->type) {
        case EXP_INVOKE_INDEX:
        case EXP_CALL:
        case EXP_VARARG:
            SET_RETCNT(F, E, MULRET);
            break;
        case EXP_INVOKE:
            SET_RETCNTL(F, E, MULRET);
            break;
        default:
            unreachable;
    }
}

// Adjust assign expressions in case last expression is a function call
static void adjustassign(Function* F, Exp* E, int32_t left, int32_t right)
{
    int32_t leftover = left - right; // Safety: left < right is a compile error
    switch(E->type) {
        case EXP_INVOKE_INDEX:
        case EXP_CALL:
            SET_RETCNT(F, E, leftover + 1);
            break;
        case EXP_INVOKE:
            SET_RETCNTL(F, E, leftover + 1);
            break;
        default:
            if(leftover > 1) CODEOP(F, OP_NILN, leftover);
            else if(leftover == 1) CODE(F, OP_NIL);
            break;
    }
}

// explist ::= expr
//           | expr ',' explist
static int32_t explist(Function* F, int32_t limit, Exp* E)
{
    int32_t left = limit;
    int32_t got = 0;
    do {
        left--;
        got++;
        if(left < 0) error(F, comperrors[CE_EXPLIST], limit);
        expr(F, E);
    } while(match(F, TOK_COMMA));
    return got;
}

static int32_t name(Function* F, const char* errmsg)
{
    expect(F, TOK_IDENTIFIER, errmsg);
    Token* name = &PREVT(F);
    if(F->S->depth > 0) { // If local scope make local variable
        make_local(F, name);
        return -1;
    } // Otherwise make global variable
    return MAKE_GLOBAL(F, name);
}

static Local* upvalvar(Function* F, int32_t idx)
{
    Upvalue* upval = &F->upvalues->data[idx];
    if(!upval->local) upvalvar(F->enclosing, idx);
    return &F->locals.data[upval->idx];
}

// helper [exprstm]
static void codeset(Function* F, Exp* E)
{
    static const char* errfmt =
        "Can't assign to variable '%.*s', it is declared as 'fixed'.";
    switch(E->type) {
        case EXP_UPVAL: {
            Local* local = upvalvar(F, E->value);
            if(unlikely(btest(local->flags, VFIXED_BIT)))
                error(F, errfmt, local->name.len, local->name.start);
            CODEOP(F, OP_SET_UPVALUE, E->value);
            break;
        }
        case EXP_LOCAL: {
            Local* local = &F->locals.data[E->value];
            if(unlikely(btest(local->flags, VFIXED_BIT)))
                error(F, errfmt, local->name.len, local->name.start);
            CODEOP(F, GET_OP_TYPE(E->value, OP_SET_LOCAL, E), E->value);
            break;
        }
        case EXP_GLOBAL:
            CODEOP(F, GET_OP_TYPE(E->value, OP_SET_GLOBAL, E), E->value);
            break;
        case EXP_INDEXED:
            if(E->value == NO_VAL) CODE(F, OP_SET_INDEX);
            else CODEOP(F, OP_SET_PROPERTY, E->value);
            break;
        default:
            return;
    }
}

ARRAY_NEW(Array_Exp, Exp);
// helper [exprstm]
static void codesetall(Function* F, Array_Exp* Earr)
{
    for(int32_t i = 0; i < cast_int(Earr->len); i++) {
        Exp* E = Array_Exp_index(Earr, i);
        codeset(F, E);
    }
}

/// exprstm ::= functioncall
///           | varlist '=' explist
static void exprstm(Function* F, bool lastclause)
{
    Exp E;
    E.ins.set = false;
    suffixedexp(F, &E);
    TokenType next = CURRT(F).type;
    if(next == TOK_EQUAL || next == TOK_COMMA) {
        E.ins.set = true;
        Array_Exp Earr;
        Array_Exp_init(&Earr, F->vm);
        expect_cond(F, etisvar(E.type), expectstr("Expect variable."));
        rmlastins(F, &E); // remove 'OP_GET..'
        Array_Exp_push(&Earr, E);
        int32_t vars = 1;
        while(match(F, TOK_COMMA)) {
            if(unlikely(vars >= SK_BYTECODE_MAX))
                error(F, comperrors[CE_VARLIST], SK_BYTECODE_MAX);
            vars++;
            suffixedexp(F, &E);
            expect_cond(F, etisvar(E.type), expectstr("Expect variable."));
            Array_Exp_push(&Earr, E);
        }
        expect(F, TOK_EQUAL, expectstr("Expect '='."));
        E.ins.set = false;
        int32_t expc = explist(F, vars, &E);
        if(vars != expc) adjustassign(F, &E, vars, expc);
        codesetall(F, &Earr);
        Array_Exp_free(&Earr, NULL);
    } else {
        if(etisvar(E.type)) {
            rmlastins(F, &E); // remove 'OP_GET...'
            CODE(F, OP_NIL);
            codeset(F, &E);
        } else if(etiscall(E.type)) CODE(F, OP_POP);
        else error(F, "Invalid syntax.\n");
    }
    if(!lastclause) expect(F, TOK_SEMICOLON, expectstr("Expect ';'."));
}

static void block(Function* F)
{
    while(!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
        dec(F);
    expect(F, TOK_RBRACE, expectstr("Expect '}' after block."));
}

static force_inline void blockstm(Function* F)
{
    Scope S;
    startscope(F, &S, 0, 0);
    block(F);
    endscope(F);
}

// arglist ::= name
//           | '...'
//           | name ',' arglist
static void arglist(Function* F)
{
    do {
        if(match(F, TOK_DOT_DOT_DOT)) {
            F->fn->isva = true;
            break;
        }
        F->fn->arity++;
        name(F, expectstr("Expect parameter name."));
        INIT_LOCAL(F, 0);
    } while(match(F, TOK_COMMA));
}

// namelist ::= name
//            | name ',' namelist
static uint32_t namelist(Function* F, Array_Int* nameidx)
{
    int32_t names = 0;
    do {
        if(names >= SK_BYTECODE_MAX) error(F, comperrors[CE_NAMELIST], SK_BYTECODE_MAX);
        names++;
        int32_t idx = name(F, expectstr("Expect name.")); // initialize later
        if(F->S->depth == 0) Array_Int_push(nameidx, idx);
    } while(match(F, TOK_COMMA));
    return names;
}

static void codeassign(Function* F, int32_t names, Array_Int* nameidx)
{
    if(F->S->depth > 0) {
        for(int32_t i = 0; i < names; i++)
            INIT_LOCAL(F, i);
        return;
    }
    sk_assert(F->vm, names == cast_int(nameidx->len), "name count != indexes array len.");
    while(nameidx->len > 0) {
        int32_t idx = Array_Int_pop(nameidx);
        Exp _; // dummy
        INIT_GLOBAL(F, idx, F->vflags, &_);
    }
}

// vardec ::= 'var' name ';'
//          | 'var' namelist ';'
//          | 'var' name '=' explist ';'
//          | 'var' namelist '=' explist ';'
//          | 'fixed' 'var' name ';'
//          | 'fixed' 'var' namelist ';'
//          | 'fixed' 'var' name '=' explist ';'
//          | 'fixed' 'var' namelist '=' explist ';'
static void vardec(Function* F)
{
    if(match(F, TOK_FIXED)) {
        if(FIS(F, FFIXED)) error(F, expectstr("Expect variable name."));
        FSET(F, FFIXED);
    }
    Array_Int nameidx;
    Array_Int_init(&nameidx, F->vm);
    int32_t names = namelist(F, &nameidx);
    int32_t expc = 0;
    Exp E;
    E.ins.set = false;
    if(match(F, TOK_EQUAL)) expc = explist(F, names, &E);
    if(names != expc) adjustassign(F, &E, names, expc);
    codeassign(F, names, &nameidx);
    Array_Int_free(&nameidx, NULL);
    expect(F, TOK_SEMICOLON, expectstr("Expect ';'."));
}

// fvardec ::= 'fixed' vardec
static force_inline void fvardec(Function* F)
{
    FSET(F, FFIXED);
    advance(F);
    vardec(F);
}

// Create and parse a new Function
static void fn(Function* F, FunctionType type)
{
    Function* Fnew = MALLOC(F->vm, sizeof(Function));
    Scope globscope, S;
    F_init(Fnew, &globscope, F->cclass, F->vm, F->lexer, type, NIL_VAL, F);
    startscope(Fnew, &S, 0, 0); // no need to end this scope
    expect(Fnew, TOK_LPAREN, expectstr("Expect '(' after function name."));
    if(!check(Fnew, TOK_RPAREN)) arglist(Fnew);
    if(F->fn->isva) expect(Fnew, TOK_RPAREN, expectstr("Expect ')' after '...'."));
    else expect(Fnew, TOK_RPAREN, expectstr("Expect ')' after parameters."));
    expect(Fnew, TOK_LBRACE, expectstr("Expect '{' before function body."));
    block(Fnew); // body
    OFunction* fn = compile_end(Fnew);
    fn->isinit = (type == FN_INIT);
    CODEOP(F, OP_CLOSURE, make_constant(F, OBJ_VAL(fn)));
    for(uint32_t i = 0; i < fn->upvalc; i++) {
        Upvalue* upval = Array_Upvalue_index(Fnew->upvalues, i);
        CODE(F, upval->local ? 1 : 0);
        CODEL(F, upval->idx);
    }
    F_free(Fnew);
}

// fndec ::= 'fn' name '(' arglist ')' '{' block '}'
static void fndec(Function* F)
{
    uint32_t idx = name(F, expectstr("Expect function name."));
    if(F->S->depth > 0) INIT_LOCAL(F, 0); // initialize to allow recursion
    fn(F, FN_FUNCTION);
    Exp _; // dummy
    if(F->S->depth == 0) INIT_GLOBAL(F, idx, 0, &_);
}

static void method(Function* F)
{
    expect(F, TOK_FN, expectstr("Expect 'fn'."));
    expect(F, TOK_IDENTIFIER, expectstr("Expect method name."));
    Value identifier = tokintostr(F->vm, &PREVT(F));
    uint32_t idx = make_constant(F, identifier);
    FunctionType type = FN_METHOD;
    if(AS_STRING(identifier) == F->vm->statics[SS_INIT]) type = FN_INIT;
    fn(F, type);
    if(type == FN_INIT) CODEOP(F, OP_OVERLOAD, SS_INIT);
    CODEOP(F, OP_METHOD, idx);
}

static void classdec(Function* F)
{
    expect(F, TOK_IDENTIFIER, expectstr("Expect class name."));
    Token class_name = PREVT(F);
    Value identifier = tokintostr(F->vm, &class_name);
    uint32_t idx = make_constant(F, identifier);
    Exp _; // dummy
    CODEOP(F, OP_CLASS, idx);
    if(F->S->depth > 0) {
        make_local(F, &class_name);
        INIT_LOCAL(F, 0);
    } else INIT_GLOBAL(F, MAKE_GLOBAL(F, &class_name), 0, &_);
    Class cclass;
    cclass.enclosing = F->cclass;
    cclass.superclass = false;
    F->cclass = &cclass;
    _.ins.set = false;
    Scope S;
    if(match(F, TOK_IMPL)) { // have superclass ?
        expect(F, TOK_IDENTIFIER, expectstr("Expect superclass name."));
        codevarprev(F, &_); // get superclass
        if(nameeq(&PREVT(F), &class_name))
            error(F, comperrors[CE_INHERIT], AS_CSTRING(identifier));
        startscope(F, &S, 0, 0);
        local_new(F, syntoken("super"));
        INIT_LOCAL(F, 0);
        codevar(F, class_name, &_);
        CODE(F, OP_INHERIT);
        cclass.superclass = true;
    }
    codevar(F, class_name, &_);
    expect(F, TOK_LBRACE, expectstr("Expect '{' before class body."));
    while(!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
        method(F);
    expect(F, TOK_RBRACE, expectstr("Expect '}' after class body."));
    CODE(F, OP_POP); // Pop the class
    if(cclass.superclass) endscope(F);
    F->cclass = cclass.enclosing;
}

/// call ::= '(' ')'
///        | '(' explist ')'
static void call(Function* F, Exp* E)
{
    CODE(F, OP_CALLSTART);
    if(!check(F, TOK_RPAREN)) explist(F, VM_ARG_LIMIT, E);
    else E->type = EXP_NONE;
    expect(F, TOK_RPAREN, expectstr("Expect ')'."));
    if(ethasmulret(E->type)) setmulret(F, E);
}

static void codecall(Function* F, Exp* E)
{
    call(F, E);
    E->type = EXP_CALL;
    E->ins.code = CODEOP(F, OP_CALL, 1);
}

static void codeinvoke(Function* F, Exp* E, int32_t idx)
{
    call(F, E);
    E->type = EXP_INVOKE;
    E->ins.code = CODEOP(F, OP_INVOKE, idx);
    CODEL(F, 1); // retcnt
}

static void dec(Function* F)
{
    F->vflags = 0;
    if(match(F, TOK_VAR)) vardec(F);
    else if(match(F, TOK_FIXED)) fvardec(F);
    else if(match(F, TOK_FN)) fndec(F);
    else if(match(F, TOK_CLASS)) classdec(F);
    else stm(F);
    if(F->lexer->panic) sync(F);
}


// 'switch' statement state.
typedef struct {
    int32_t patch; // Jump to patch if case expression does not match
    enum {
        CS_MATCH, // Case is constant expression match
        CS_DFLT, // Case is 'default'
        CS_NONE, // Did not parse any cases yet
        CS_CASE, // Case is 'case'
    } casestate;
    bool dflt; // if switch has 'default' case
    bool havenil; // if switch has 'nil' case
    bool havetrue; // if switch has 'true' case
    bool havefalse; // if switch has 'false' case
    Array_Value constants; // all case constant expressions
} SwitchState;

static force_inline void SwitchState_init(Function* F, SwitchState* state)
{
    state->patch = -1;
    state->casestate = CS_NONE;
    state->dflt = false;
    state->havenil = false;
    state->havetrue = false;
    state->havefalse = false;
    Array_Value_init(&state->constants, F->vm);
}

#define SwitchState_free(state) Array_Value_free(&(state)->constants, NULL);

/*
 * Updates 'switch' constants and checks if the constant
 * already exists, but only if 'e' is constant expression.
 */
static force_inline void switchconstants(Function* F, SwitchState* state, Exp* E)
{
    if(!etisconst(E->type)) return;
    switch(E->type) {
        case EXP_FALSE:
            if(unlikely(state->havefalse)) error(F, comperrors[CE_SWDUP], "false");
            state->havefalse = true;
            break;
        case EXP_TRUE:
            if(unlikely(state->havetrue)) error(F, comperrors[CE_SWDUP], "true");
            state->havetrue = true;
            break;
        case EXP_NIL:
            if(unlikely(state->havenil)) error(F, comperrors[CE_SWDUP], "nil");
            state->havenil = true;
            break;
        case EXP_STRING:
        case EXP_NUMBER:;
            Value caseval = *CONSTANT(F, E);
            uint8_t _; // dummy
            for(int32_t i = 0; i < cast_int(state->constants.len); i++) {
                if(unlikely(raweq(F->vm, state->constants.data[i], caseval))) {
                    const char* casename = NULL;
                    if(E->type == EXP_STRING) casename = vtostr(F->vm, caseval)->storage;
                    else casename = dtostr(AS_NUMBER(caseval), &_);
                    error(F, comperrors[CE_SWDUP], casename);
                    return;
                }
            }
            Array_Value_push(&state->constants, caseval);
            break;
        default:
            unreachable;
    }
}

static void switchstm(Function* F)
{
    Context C;
    Scope S;
    SwitchState swstate;
    uint8_t inswitch = F->S->isswitch;
    Exp E1;
    Array_Int fts;
    int32_t sdepth;
    savecontext(F, &C);
    SwitchState_init(F, &swstate);
    Array_Int_init(&fts, F->vm);
    startscope(F, &S, 0, 1); // implicit scope
    startbreaklist(F);
    expect(F, TOK_LPAREN, expectstr("Expect '(' after 'switch'."));
    E1.ins.set = false;
    expr(F, &E1);
    expect(F, TOK_RPAREN, expectstr("Expect ')' after condition."));
    expect(F, TOK_LBRACE, expectstr("Expect '{' after ')'."));
    sdepth = F->cflow.innersdepth;
    F->cflow.innersdepth = F->S->depth;
    // Switch must contain case or default before any statements
    if(!check(F, TOK_RBRACE) && !check(F, TOK_EOF) && !check(F, TOK_CASE) &&
       !check(F, TOK_DEFAULT))
        error(F, comperrors[CE_SWNOC]);
    // 'switch' body
    while(!match(F, TOK_RBRACE) && !check(F, TOK_EOF)) {
        if(match(F, TOK_CASE) || match(F, TOK_DEFAULT)) {
            F->fn->gotret = 0;
            if(swstate.casestate != CS_NONE) {
                Array_Int_push(&fts, CODEJMP(F, OP_JMP));
                if(swstate.casestate != CS_DFLT && swstate.casestate != CS_MATCH)
                    patchjmp(F, swstate.patch);
            }
            swstate.casestate = CS_DFLT;
            if(PREVT(F).type == TOK_CASE) {
                Exp E2;
                E2.ins.set = false;
                expr(F, &E2);
                expect(F, TOK_COLON, expectstr("Expect ':' after 'case'."));
                switchconstants(F, &swstate, &E2);
                if(eareconstandeq(F, &E1, &E2)) {
                    restorecontext(F, &C);
                    swstate.casestate = CS_MATCH;
                } else {
                    CODE(F, OP_EQ);
                    swstate.casestate = CS_CASE;
                    swstate.patch = CODEJMP(F, OP_JMP_IF_FALSE_POP);
                }
            } else if(!swstate.dflt) {
                swstate.dflt = true;
                swstate.casestate = CS_DFLT;
                expect(F, TOK_COLON, expectstr("Expect ':' after 'default'."));
            } else error(F, comperrors[CE_SWDEF]);
            if(fts.len > 0) patchjmp(F, Array_Int_pop(&fts));
        } else {
            stm(F);
            if(swstate.casestate == CS_MATCH && F->fn->gotret) {
                // @TODO: Implement optimizations.
                // Also check if last 'stm' was 'breakstm' (same effect)
            }
        }
    }
    if(PREVT(F).type == TOK_EOF) error(F, comperrors[CE_SWRBR]);
    Array_Int_free(&fts, NULL);
    SwitchState_free(&swstate);
    endscope(F);
    CODE(F, OP_POP); // pop switch expression
    patchbreaklist(F);
    endbreaklist(F);
    F->cflow.innersdepth = sdepth;
    F->S->isswitch = inswitch;
}

static void ifstm(Function* F)
{
    Exp E;
    Context C;
    int32_t jmptoelse, jmptoend = -1;
    savecontext(F, &C);
    expect(F, TOK_LPAREN, expectstr("Expect '(' after 'if'."));
    E.ins.set = false;
    expr(F, &E); // condition
    expect(F, TOK_RPAREN, expectstr("Expect ')' after condition."));
    bool remove = false;
    bool istrue = false;
    if(etisconst(E.type)) {
        rmlastins(F, &E);
        if(etisfalse(E.type)) remove = true;
        else istrue = true;
    } else jmptoelse = CODEJMP(F, OP_JMP_IF_FALSE_POP);
    stm(F);
    if(!remove) {
        jmptoend = CODEJMP(F, OP_JMP);
        if(!istrue) patchjmp(F, jmptoelse);
        else if(F->fn->gotret) { // condition is true and 'stm' was a 'returnstm'
            // @TODO: Implement optimization.
        }
    } else restorecontext(F, &C);
    F->fn->gotret = 0;
    if(match(F, TOK_ELSE)) { // there is 'else' branch?
        stm(F);
        if(!remove) patchjmp(F, jmptoend);
    }
}

static void startloop(Function* F, int32_t* lstart, int32_t* ldepth)
{
    *lstart = (F)->cflow.innerlstart;
    *ldepth = (F)->cflow.innerldepth;
    (F)->cflow.innerlstart = codeoffset(F);
    (F)->cflow.innerldepth = F->S->depth;
}

static void endloop(Function* F, int32_t lstart, int32_t ldepth)
{
    (F)->cflow.innerlstart = lstart;
    (F)->cflow.innerldepth = ldepth;
}

static void whilestm(Function* F)
{
    Scope S;
    Context C;
    Exp E;
    int32_t lstart, ldepth;
    int32_t jmptoend = -1;
    bool infinite = false;
    bool remove = false;
    savecontext(F, &C);
    startscope(F, &S, 1, 0);
    startloop(F, &lstart, &ldepth);
    startbreaklist(F);
    expect(F, TOK_LPAREN, expectstr("Expect '(' after 'while'."));
    E.ins.set = false;
    expr(F, &E); // conditional
    if(etisconst(E.type)) {
        rmlastins(F, &E);
        if(etisfalse(E.type)) remove = true;
        else infinite = true;
    } else jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
    expect(F, TOK_RPAREN, expectstr("Expect ')' after condition."));
    stm(F); // body
    bool gotret = F->fn->gotret;
    endscope(F);
    if(!remove) {
        codeloop(F, (F)->cflow.innerlstart);
        if(!infinite) {
            sk_assert(F->vm, jmptoend != -1, "end jmp invalid but flag is false.");
            patchjmp(F, jmptoend);
        } else if(gotret) { // cond true and 'stm' was 'returnstm'
            // @TODO: Implement optimizations
        }
        patchbreaklist(F);
    } else restorecontext(F, &C);
    endloop(F, lstart, ldepth);
    endbreaklist(F);
}

static void forstm(Function* F)
{
    Scope S;
    Context C;
    Exp E;
    int32_t lstart, ldepth;
    int32_t jmptoend = -1;
    bool remove = false;
    bool infinite = false;
    startscope(F, &S, 1, 0);
    startbreaklist(F);
    expect(F, TOK_LPAREN, expectstr("Expect '(' after 'for'."));
    if(match(F, TOK_SEMICOLON)) // Initializer for-clause
        ; // no initializer
    else if(match(F, TOK_VAR)) vardec(F);
    else if(match(F, TOK_FIXED)) fvardec(F);
    else exprstm(F, false);
    savecontext(F, &C);
    startloop(F, &lstart, &ldepth);
    if(!match(F, TOK_SEMICOLON)) { // conditional
        E.ins.set = false;
        expr(F, &E);
        if(etisconst(E.type)) {
            rmlastins(F, &E);
            if(etistrue(E.type)) infinite = true;
            else remove = true;
        } else jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
        expect(
            F,
            TOK_SEMICOLON,
            expectstr("Expect ';' after for-loop condition clause."));
    } else infinite = true;
    if(!match(F, TOK_RPAREN)) { // last for-clause
        int32_t jmptobody = -1;
        int32_t jmptoincr = -1;
        if(!infinite && !remove) jmptobody = CODEJMP(F, OP_JMP);
        if(!remove) jmptoincr = codeoffset(F);
        exprstm(F, true);
        if(!infinite && !remove) {
            codeloop(F, (F)->cflow.innerlstart);
            patchjmp(F, jmptobody);
            (F)->cflow.innerlstart = jmptoincr;
        }
        expect(F, TOK_RPAREN, expectstr("Expect ')' after last for-loop clause."));
    }
    stm(F); // Loop body
    if(!remove) {
        codeloop(F, (F)->cflow.innerlstart);
        if(!infinite) patchjmp(F, jmptoend);
        else if(F->fn->gotret) { // 'stm' was 'returnstm' and conditional is true
            // @TODO: Implement optimizations
        }
        patchbreaklist(F);
    } else restorecontext(F, &C);
    endscope(F);
    endloop(F, lstart, ldepth);
    endbreaklist(F);
}

static int32_t foreachvars(Function* F)
{
    int32_t vars = 0;
    do {
        if(vars >= SK_BYTECODE_MAX) error(F, comperrors[CE_VARLIST], SK_BYTECODE_MAX);
        vars++;
        expect(F, TOK_IDENTIFIER, expectstr("Expect varname."));
        make_local(F, &PREVT(F));
        INIT_LOCAL(F, 0);
    } while(match(F, TOK_COMMA));
    return vars;
}

static void newlocalliteral(Function* F, const char* name)
{
    Token syntok = syntoken(name);
    local_new(F, syntok);
    INIT_LOCAL(F, 0);
}

static void foreachstm(Function* F)
{
    Scope S;
    int32_t lstart, ldepth, vars, expc, endjmp;
    Exp E;
    startscope(F, &S, 1, 0);
    S.isgloop = 1; // set as generic loop
    startbreaklist(F);
    newlocalliteral(F, "(for iterator)"); // iterator function
    newlocalliteral(F, "(for invstate)"); // invariant state
    newlocalliteral(F, "(for cntlvar)"); // control variable
    vars = foreachvars(F); // declared vars
    expect(F, TOK_IN, expectstr("Expect 'in'."));
    E.ins.set = false;
    expr(F, &E); // iterator factory
    expect_cond(F, !etisconst(E.type), "Can't call constant expression.");
    expc = 1;
    if(match(F, TOK_COMMA)) expc += explist(F, 2, &E);
    adjustassign(F, &E, 3, expc);
    startloop(F, &lstart, &ldepth);
    CODEOP(F, OP_FOREACH_PREP, vars);
    CODEOP(F, OP_FOREACH, vars);
    endjmp = CODEJMP(F, OP_JMP);
    stm(F);
    CODEPOP(F, vars);
    codeloop(F, (F)->cflow.innerlstart);
    patchjmp(F, endjmp);
    endscope(F);
    patchbreaklist(F);
    endbreaklist(F);
    endloop(F, lstart, ldepth);
}

static void loopstm(Function* F)
{
    Scope S;
    int32_t lstart, ldepth;
    startscope(F, &S, 1, 0);
    startbreaklist(F);
    startloop(F, &lstart, &ldepth);
    stm(F);
    if(F->fn->gotret) {
        // @TODO: Implement optimizations
    }
    codeloop(F, (F)->cflow.innerlstart);
    endscope(F);
    patchbreaklist(F);
    endbreaklist(F);
    endloop(F, lstart, ldepth);
}

static force_inline Scope* loopscope(Function* F)
{
    Scope* S = F->S;
    while(S != NULL) {
        if(S->isloop) return S;
        S = S->prev;
    }
    return NULL;
}

// Return count of switch statements until the first loop 'Scope'
// or the top-level code counting from the current 'Scope' in the 'Function'.
static force_inline int32_t switchcnt(Function* F)
{
    Scope* S = F->S;
    int32_t count = 0;
    while(S != NULL && S->depth > F->cflow.innerldepth) {
        if(S->isswitch) count++;
        S = S->prev;
    }
    return count;
}

static void continuestm(Function* F)
{
    expect(F, TOK_SEMICOLON, expectstr("Expect ';' after 'continue'."));
    if(unlikely(F->cflow.innerlstart == -1)) error(F, comperrors[CE_CONT]);
    else {
        Scope* S = loopscope(F);
        sk_assert(F->vm, S != NULL, "Loop scope not found but cflow offset is set.");
        int32_t popn = F->locals.len - (S->isgloop * 3) - S->localc + switchcnt(F);
        CODEPOP(F, popn);
        codeloop(F, F->cflow.innerlstart);
    }
}

static void breakstm(Function* F)
{
    expect(F, TOK_SEMICOLON, expectstr("Expect ';' after 'break'."));
    Array_Array_Int* arr = &F->cflow.breaks;
    int32_t popn = 0;
    Scope* scope = NULL;
    switch(inwhat(F->S, &scope)) {
        case 0: // in switch
            popn++; // switch expression
        case 1: // FALLTHRU (in loop)
            break;
        case -1: // outside
            error(F, comperrors[CE_BREAK]);
            return;
        default:
            unreachable;
    }
    popn += F->locals.len - scope->localc;
    CODEPOP(F, popn);
    Array_Int* last = Array_Array_Int_last(arr);
    Array_Int_push(last, CODEJMP(F, OP_JMP));
}

/// return ::= 'return' ';'
///          | 'return' explist ';'
static void returnstm(Function* F)
{
    /*
     * @TODO:
     * Optimize even further by removing all of the unreachable code.
     */
    Context C;
    bool gotret = F->fn->gotret;
    FunctionType type = F->fn_type;
    savecontext(F, &C);
    CODE(F, OP_RETSTART);
    if(match(F, TOK_SEMICOLON)) coderet(F, true, gotret);
    else {
        if(type == FN_INIT)
            error(F, comperrors[CE_INITRET], static_strings[SS_INIT].name);
        Exp E;
        E.ins.set = false;
        explist(F, VM_RET_LIMIT, &E);
        expect(F, TOK_SEMICOLON, expectstr("Expect ';' after return statement value/s."));
        if(gotret) {
            F->fn->gotret = 1;
            restorecontext(F, &C);
        } else {
            if(ethasmulret(E.type)) setmulret(F, &E);
            CODE(F, OP_RET);
        }
    }
}


/// dot ::= '.' name
///       | '.' name call
static void dot(Function* F, Exp* E)
{
    expect(F, TOK_IDENTIFIER, expectstr("Expect property name after '.'."));
    Value identifier = tokintostr(F->vm, &PREVT(F));
    uint32_t idx = make_constant(F, identifier);
    if(match(F, TOK_LPAREN)) codeinvoke(F, E, idx);
    else {
        E->type = EXP_INDEXED;
        E->value = idx;
        if(!E->ins.set) E->ins.code = CODEOP(F, OP_GET_PROPERTY, idx);
    }
}

static void stm(Function* F)
{
    if(match(F, TOK_WHILE)) whilestm(F);
    else if(match(F, TOK_FOR)) forstm(F);
    else if(match(F, TOK_FOREACH)) foreachstm(F);
    else if(match(F, TOK_IF)) ifstm(F);
    else if(match(F, TOK_SWITCH)) switchstm(F);
    else if(match(F, TOK_LBRACE)) blockstm(F);
    else if(match(F, TOK_CONTINUE)) continuestm(F);
    else if(match(F, TOK_BREAK)) breakstm(F);
    else if(match(F, TOK_RETURN)) returnstm(F);
    else if(match(F, TOK_LOOP)) loopstm(F);
    else if(match(F, TOK_SEMICOLON))
        ; // empty statement
    else exprstm(F, false);
}

// indexed ::= '[' expr ']'
//           | '[' expr ']' call
static force_inline void indexed(Function* F, Exp* E)
{
    Exp E2;
    E2.ins.set = false;
    expr(F, &E2);
    expect(F, TOK_RBRACK, expectstr("Expect ']'."));
    if(match(F, TOK_LPAREN)) {
        if(etisconst(E2.type)) error(F, comperrors[CE_CALLCONST]);
        call(F, E);
        E->type = EXP_INVOKE_INDEX;
        E->ins.code = CODEOP(F, OP_INVOKE_INDEX, 1);
    } else {
        E->type = EXP_INDEXED;
        E->value = NO_VAL;
        if(!E->ins.set) E->ins.code = CODE(F, OP_INDEX);
    }
}








/*========================== EXPRESSION =========================*/

static void _self(Function* F, Exp* E)
{
    if(F->cclass == NULL) {
        error(F, comperrors[CE_SELF]);
        return;
    }
    advance(F);
    codevarprev(F, E);
}

static void _super(Function* F, Exp* E)
{
    if(!F->cclass) {
        error(F, comperrors[CE_SUPER]);
        return;
    }
    if(!F->cclass->superclass) {
        error(F, comperrors[CE_NOSUPER]);
        return;
    }
    advance(F);
    expect(F, TOK_DOT, expectstr("Expect '.' after 'super'."));
    expect(F, TOK_IDENTIFIER, expectstr("Expect superclass method name after '.'."));
    Value methodname = tokintostr(F->vm, &PREVT(F));
    int32_t idx = make_constant(F, methodname);
    Exp _; // dummy
    _.ins.set = false;
    codevar(F, syntoken("self"), &_);
    if(match(F, TOK_LPAREN)) {
        call(F, E);
        codevar(F, syntoken("super"), &_);
        E->ins.code = CODEOP(F, OP_INVOKE_SUPER, idx);
        CODEL(F, 1);
        E->type = EXP_INVOKE;
    } else {
        codevar(F, syntoken("super"), &_);
        if(E->ins.set) error(F, "Can't assign to methods from superclass.\n");
        E->ins.code = CODEOP(F, OP_GET_SUPER, idx);
        E->type = EXP_INDEXED; // superclass method
    }
}

// primary_exp ::= '(' exp ')'
//               | name
//               | 'self'
static void primaryexp(Function* F, Exp* E)
{
    switch(CURRT(F).type) {
        case TOK_LPAREN:
            advance(F);
            expr(F, E);
            expect(F, TOK_RPAREN, "Expect ')'.\n");
            break;
        case TOK_IDENTIFIER:
            advance(F);
            codevarprev(F, E);
            break;
        case TOK_SELF:
            _self(F, E);
            break;
        case TOK_SUPER:
            _super(F, E);
            break;
        default:
            error(F, "Unexpected symbol.\n");
            return;
    }
}

// suffixedexp ::= primaryexp
//               | primaryexp [dot|call|indexed...]
static void suffixedexp(Function* F, Exp* E)
{
    primaryexp(F, E);
    while(true) {
        switch(CURRT(F).type) {
            case TOK_DOT:
                advance(F);
                dot(F, E);
                break;
            case TOK_LPAREN:
                if(etisconst(E->type)) error(F, comperrors[CE_CALLCONST]);
                advance(F);
                codecall(F, E);
                break;
            case TOK_LBRACK:
                advance(F);
                indexed(F, E);
                break;
            default:
                return;
        }
    }
}

// simpleexp ::= number
//             | string
//             | 'nil'
//             | 'true'
//             | 'false'
//             | '...'
//             | suffixedexp
static void simpleexp(Function* F, Exp* E)
{
    uint32_t constidx;
    ExpType type;
    switch(CURRT(F).type) {
        case TOK_NUMBER:
            type = EXP_NUMBER;
            sk_assert(F->vm, IS_NUMBER(CURRT(F).value), "Expect number.");
            goto constfin;
        case TOK_STRING:
            type = EXP_STRING;
            sk_assert(F->vm, IS_STRING(CURRT(F).value), "Expect string.");
        constfin:
            constidx = make_constant(F, CURRT(F).value);
            Exp_init(E, type, CODEOP(F, OP_CONST, constidx), constidx);
            break;
        case TOK_NIL:
            Exp_init(E, EXP_NIL, CODE(F, OP_NIL), 0);
            break;
        case TOK_TRUE:
            Exp_init(E, EXP_TRUE, CODE(F, OP_TRUE), 0);
            break;
        case TOK_FALSE:
            Exp_init(E, EXP_FALSE, CODE(F, OP_FALSE), 0);
            break;
        case TOK_DOT_DOT_DOT:
            Exp_init(E, EXP_VARARG, vararg(F), 0);
            break;
        default:
            suffixedexp(F, E);
            return;
    }
    advance(F);
}


// Try folding unary operation.
// Example: OP_CONST (1), OP_NEG => OP_CONST (-1)
static bool foldunary(Function* F, UnaryOpr opr, Exp* E)
{
    if(E->type == EXP_NUMBER && opr == OPR_NEGATE) {
        double val = AS_NUMBER(*CONSTANT(F, E));
        if(sk_isnan(val) || val == 0.0) return false;
        *CONSTANT(F, E) = NUMBER_VAL(-val);
        return true;
    }
    return false;
}

// Fold constant number expressions
static void
calcnum(Function* F, BinaryOpr opr, const Exp* E1, const Exp* E2, Value* result)
{
#define BINOP(op, n1, n2) NUMBER_VAL((n1)op(n2))

    double n1 = AS_NUMBER(*CONSTANT(F, E1));
    double n2 = AS_NUMBER(*CONSTANT(F, E2));
    switch(opr) {
        case OPR_ADD:
            *result = BINOP(+, n1, n2);
            break;
        case OPR_SUB:
            *result = BINOP(-, n1, n2);
            break;
        case OPR_MUL:
            *result = BINOP(*, n1, n2);
            break;
        case OPR_DIV:
            *result = BINOP(/, n1, n2);
            break;
        case OPR_MOD:
            *result = BINOP(%, (long int)n1, (long int)n2);
            break;
        case OPR_POW:
            *result = NUMBER_VAL((sk_powl(n1, n2)));
            break;
        default:
            unreachable;
    }

#undef BINOP
}

// Check if the binary operation is valid
static bool validop(Function* F, BinaryOpr opr, const Exp* E1, const Exp* E2)
{
    double n1 = AS_NUMBER(*CONSTANT(F, E1));
    double n2 = AS_NUMBER(*CONSTANT(F, E2));
    return !(opr == OPR_MOD && (sk_floor(n1) != n1 || sk_floor(n2) != n2));
}

// Try folding binary operation
// Example: OP_CONST (1), OP_CONST (2), OP_ADD => OP_CONST (3)
static bool foldbinary(Function* F, BinaryOpr opr, Exp* E1, const Exp* E2)
{
    if(E1->type != E2->type || E1->type != EXP_NUMBER || !validop(F, opr, E1, E2))
        return false;
    Value result;
    calcnum(F, opr, E1, E2, &result);
    if(sk_isnan(AS_NUMBER(result))) return false;
    CONSTANT_POP(F); // Pop constant (E2)
    *CONSTANT(F, E1) = result; // Set new constant value (E1)
    LINSTRUCTION_POP(F); // Pop off the last OP_CONST instruction
    return true;
}

// Emit optimized 'and' instruction
static void codeand(Function* F, Exp* E)
{
    switch(E->type) {
        case EXP_TRUE:
            INSTRUCTION_POP(F);
            goto fin;
        case EXP_STRING:
        case EXP_NUMBER:
            LINSTRUCTION_POP(F);
            CONSTANT_POP(F);
        fin:
            E->jmp.f = NO_JMP;
            break;
        default:
            E->jmp.f = CODEJMP(F, OP_JMP_IF_FALSE_OR_POP);
            E->ins.code = codeoffset(F) - 4; // Index of jump instruction
            break;
    }
    E->jmp.t = NO_JMP;
    E->type = EXP_JMP;
}

// Emit optimized 'or' instruction
static void codeor(Function* F, Exp* E)
{
    switch(E->type) {
        case EXP_NIL:
        case EXP_FALSE:
            INSTRUCTION_POP(F);
            E->jmp.t = NO_JMP;
            break;
        case EXP_STRING:
        case EXP_NUMBER:
        case EXP_TRUE:
            E->jmp.t = codeoffset(F);
            E->jmp.f = CHUNK(F)->constants.len;
            return;
        default:
            E->jmp.f = CODEJMP(F, OP_JMP_IF_FALSE_AND_POP);
            E->jmp.t = CODEJMP(F, OP_JMP);
            patchjmp(F, E->jmp.f);
            break;
    }
    E->jmp.f = NO_JMP;
    E->jmp.t = EXP_JMP;
}

// Emit binary instruction
static void postfix(Function* F, BinaryOpr opr, Exp* E1, Exp* E2)
{
    if(FOLDABLE(opr) && foldbinary(F, opr, E1, E2)) return;
    switch(opr) {
        case OPR_ADD:
        case OPR_SUB:
        case OPR_MUL:
        case OPR_DIV:
        case OPR_MOD:
        case OPR_POW:
        case OPR_NE:
        case OPR_EQ:
        case OPR_LT:
        case OPR_LE:
        case OPR_GT:
        case OPR_GE:
            E1->ins.code = CODEBIN(F, opr);
            E1->type = EXP_EXPR;
            E1->ins.binop = true;
            break;
        case OPR_OR:
            if(E1->type != EXP_JMP) {
                concatcode(F, E1->jmp.t + 3, E1->jmp.f);
                *E1 = *E2;
            } else if(E1->jmp.t != NO_JMP) {
                patchjmp(F, E1->jmp.t);
                *E1 = *E2;
            } else E1->type = EXP_EXPR;
            break;
        case OPR_AND:
            if(E1->jmp.f == NO_JMP) *E1 = *E2;
            else {
                patchjmp(F, E1->jmp.f);
                E1->type = EXP_EXPR;
            }
            break;
        default:
            unreachable;
    }
}

// Intermediate step that tries to optimize/process 'and' and 'or'
// instructions before the second expression gets parsed.
static void shortcircuit(Function* F, BinaryOpr opr, Exp* E)
{
    switch(opr) {
        case OPR_AND:
            codeand(F, E);
            break;
        case OPR_OR:
            codeor(F, E);
            break;
        default:
            return;
    }
}

// Emit prefix instruction (only if folding didn't work)
static force_inline void prefix(Function* F, UnaryOpr opr, Exp* E)
{
    if(!foldunary(F, opr, E)) CODE(F, unopr2op(opr));
}

// subexpr ::= simpleexp
//           | '-' simpleexp
//           | '!' simpleexp
//           | simpleexp '+' subexpr
//           | simpleexp '-' subexpr
//           | simpleexp '*' subexpr
//           | simpleexp '/' subexpr
//           | simpleexp '^' subexpr
//           | simpleexp '%' subexpr
//           | simpleexp '!=' subexpr
//           | simpleexp '==' subexpr
//           | simpleexp '<' subexpr
//           | simpleexp '<=' subexpr
//           | simpleexp '>' subexpr
//           | simpleexp '>=' subexpr
//           | simpleexp 'and' subexpr
//           | simpleexp 'or' subexpr
static BinaryOpr subexp(Function* F, Exp* E1, int32_t limit)
{
    UnaryOpr unaryop = getunaryopr(CURRT(F).type);
    if(unaryop != OPR_NOUNARYOPR) {
        advance(F);
        subexp(F, E1, UNARY_PRIORITY);
        prefix(F, unaryop, E1);
    } else simpleexp(F, E1);
    BinaryOpr binop = getbinaryopr(CURRT(F).type);
    while(binop != OPR_NOBINOPR && priority[binop].left > limit) {
        Exp E2;
        E2.ins.set = false;
        advance(F); // skip binary operator
        shortcircuit(F, binop, E1);
        BinaryOpr nextop = subexp(F, &E2, priority[binop].right);
        postfix(F, binop, E1, &E2);
        binop = nextop;
    }
    return binop;
}

// expr ::= subexpr
static void expr(Function* F, Exp* E)
{
    subexp(F, E, 0);
}
