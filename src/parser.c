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
#include "value.h"
#include "vmachine.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>






// Get instruction length
#define GET_OP_TYPE(idx, op, E)                                                 \
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
    Int   depth; // Scope depth where variable is defined
               // can be -1 if the variable is not initialized
    Byte flags; // Flags that represent some property of this
                // variable (check above the V[FLAGNAME]_BIT)
} Local;

// Initialize local variable
#define INIT_LOCAL(F, i)                                                        \
    do {                                                                        \
        Local* local =                                                          \
            Array_Local_index(&(F)->locals, (F)->locals.len - (i + 1));         \
        local->depth = (F)->S->depth;                                           \
        local->flags = (F)->vflags;                                             \
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

    Int innerlstart; /* Innermost loop start offset */
    Int innerldepth; /* Innermost loop scope depth */
    Int innersdepth; /* Innermost switch scope depth */
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
    UInt idx; // Stack index
    Byte flags; // Local flags
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
    bool          superclass;
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
        Int t; // jmp to patch if 'true'
        Int f; // jmp to patch if 'false'
    } jmp; // code jumps
    struct {
        Int  code; // instruction index
        bool l; // is this long instruction
        bool set; // should it be a setter or getter
        bool binop; // is this instruction a simple binary operator
    } ins; // instruction info
    Int value; // expression value or index
} Exp;

// Initialize expression
sstatic force_inline void Exp_init(Exp* E, ExpType type, Int code, Int value)
{
    E->type      = type;
    E->jmp.t     = NO_JMP;
    E->jmp.f     = NO_JMP;
    E->ins.code  = code;
    E->ins.binop = false;
    E->value     = value;
}

// Get current chunk
#define CHUNK(F) (&(F)->fn->chunk)
// Get constant
#define CONSTANT(F, E) Array_Value_index(&CHUNK(F)->constants, (E)->value)
// Get instruction
#define INSTRUCTION(F, E)                                                       \
    byteptr(Array_Byte_index(&CHUNK(F)->code, (E)->ins.code))

// Expressions are constants and their Values are equal
#define eareconstandeq(F, E1, E2)                                               \
    (etisconst((E1)->type) && etisconst((E2)->type) &&                          \
     veq(*CONSTANT(F, E1), *CONSTANT(F, E2)))


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
#define PARAM_POP(F)                                                            \
    do {                                                                        \
        ASSERT(CHUNK(F)->code.len >= 1, "Invalid PARAM_POP.");                  \
        CHUNK(F)->code.len--;                                                   \
    } while(false)
// Pop last instruction long parameter (3 bytes)
#define LPARAM_POP(F)                                                           \
    do {                                                                        \
        ASSERT(CHUNK(F)->code.len >= 3, "Invalid LPARAM_POP.");                 \
        CHUNK(F)->code.len -= 3;                                                \
    } while(false)
// Pop last short/simple instruction (1 byte)
#define SINSTRUCTION_POP(F)                                                     \
    do {                                                                        \
        ASSERT(CHUNK(F)->code.len >= 1, "Invalid SINSTRUCTION_POP.");           \
        CHUNK(F)->code.len--;                                                   \
    } while(false)
// Pop last instruction with parameter (2 bytes)
#define INSTRUCTION_POP(F)                                                      \
    do {                                                                        \
        ASSERT(CHUNK(F)->code.len >= 2, "Invalid INSTRUCTION_POP.");            \
        CHUNK(F)->code.len -= 2;                                                \
    } while(false)
// Pop last instruction with long parameter (4 bytes)
#define LINSTRUCTION_POP(F)                                                     \
    do {                                                                        \
        ASSERT(CHUNK(F)->code.len >= 4, "Invalid LINSTRUCTION_POP.");           \
        CHUNK(F)->code.len -= 4;                                                \
    } while(false)

// Pop last constant
#define CONSTANT_POP(F)                                                         \
    do {                                                                        \
        ASSERT(CHUNK(F)->constants.len > 0, "Invalid CONSTANT_POP.");           \
        Array_Value_pop(&CHUNK(F)->constants);                                  \
    } while(false)








typedef struct Scope Scope;
struct Scope {
    Scope* prev; // Linked List
    UInt   localc; // Count of locals up until this scope
    Byte   isloop : 1; // This scope is loop
    Byte   isswitch : 1; // This scope is switch
    Byte   isgloop : 1; // This scope is generic loop
    Int    depth; // scope depth (index)
};

/*
 * Returns 1 if 'S' is in loop, 0 if inside of a switch
 * and -1 if outside.
 */
sstatic force_inline Int inwhat(Scope* S, Scope** target)
{
    Int    which = -1;
    Scope* head  = S;
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

#define FCLEAR(F, modifier) BIT_CLEAR((F)->vflags, modifier)
#define FSET(F, modifier)   BIT_SET((F)->vflags, modifier)
#define FIS(F, modifier)    BIT_CHECK((F)->vflags, modifier)

#define FFIXED 1 // Variable is fixed

ARRAY_NEW(Array_Local, Local);
ARRAY_NEW(Array_Upvalue, Upvalue);

struct Function {
    Function*      enclosing; // chain
    VM*            vm; // virtual machine
    Class*         cclass; // class declaration state
    Lexer*         lexer; // grammar lexer
    ControlFlow    cflow; // control flow context
    Scope*         S; // scope state
    OFunction*     fn; // currently parsed function (bytecode chunk)
    FunctionType   fn_type;
    Array_Upvalue* upvalues; // captured variables
    Byte           vflags; // variable flags
    Array_Local    locals; // local variables stack
};

sstatic force_inline Scope* getscope(Function* F, Int depth)
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
    Int codeoffset;
    Int constlen;
    Int localc;
    Int upvalc;
} Context;

sstatic force_inline void savecontext(Function* F, Context* C)
{
    C->codeoffset = codeoffset(F);
    C->constlen   = CHUNK(F)->constants.len;
    C->localc     = F->locals.len;
    C->upvalc     = F->upvalues->len;
}

// Trim/set length of code and/or constant array
sstatic force_inline void
concatcode(Function* F, Int codeoffset, Int constoffset)
{
    CHUNK(F)->code.len      = codeoffset;
    CHUNK(F)->constants.len = constoffset;
}

sstatic force_inline void restorecontext(Function* F, Context* C)
{
    concatcode(F, C->codeoffset, C->constlen);
    F->locals.len    = C->localc;
    F->upvalues->len = C->upvalc;
}








// Forward declare
sstatic void dec(Function* F);
sstatic void expr(Function* F, Exp* E);
sstatic void suffixedexp(Function* F, Exp* E);
sstatic void stm(Function* F);








/*========================== ERROR =========================*/

// Compile-time error
sstatic void error(Function* F, const char* error, ...)
{
    Token*  token = &PREVT(F);
    va_list args;
    // If panic bit is on, then sync the lexer before printing any new errors.
    // If token type is TOK_ERROR then just return because lexer already
    // printed it.
    if(F->lexer->panic || token->type == TOK_ERROR) return;
    va_start(args, error);
    printerror(F->lexer, error, args);
    va_end(args);
}








//======================= CODE =======================//

// Multi-byte instruction (up to 4 bytes)
#define CODEOP(F, code, param)                                                  \
    Chunk_write_codewparam(CHUNK(F), code, param, PREVT(F).line)

// Single byte instruction
#define CODE(F, byte)                                                           \
    ({                                                                          \
        (F)->fn->gotret = (((byte) == OP_RET) | ((byte) == OP_TOPRET));         \
        Chunk_write(CHUNK(F), byte, PREVT(F).line);                             \
    })

// 3 byte parameter
#define CODEL(F, bytes)                                                         \
    do {                                                                        \
        CODE(F, BYTE(bytes, 0));                                                \
        CODE(F, BYTE(bytes, 1));                                                \
        CODE(F, BYTE(bytes, 2));                                                \
    } while(false)

// Emit jump instruction
#define CODEJMP(F, jmp)                                                         \
    ({                                                                          \
        CODEOP(F, jmp, 0);                                                      \
        codeoffset(F) - 3;                                                      \
    })

// Emit pop instruction
#define CODEPOP(F, n)                                                           \
    do {                                                                        \
        if(n > 0) {                                                             \
            if((n) > 1) CODEOP(F, OP_POPN, n);                                  \
            else CODE(F, OP_POP);                                               \
        }                                                                       \
    } while(false)

// Emit unary instruction
#define CODEUN(F, opr) CODE(F, unopr2op(opr))

// Emit binary instruction
#define CODEBIN(F, opr) CODE(F, binopr2op(opr))

// Emit return instruction
sstatic force_inline void coderet(Function* F, bool explicit, bool gotret)
{
    if(gotret) {
        F->fn->gotret = 1;
        return;
    }
    if(!explicit) CODE(F, OP_RETSTART);
    if(F->fn_type == FN_INIT) CODEOP(F, OP_GET_LOCAL, 0);
    else {
        if(F->fn_type == FN_SCRIPT) {
            CODE(F, OP_TRUE);
            CODE(F, OP_TOPRET);
            return;
        }
        CODE(F, OP_NIL);
    }
    CODE(F, OP_RET);
}

// Emit loop instruction
sstatic force_inline void codeloop(Function* F, UInt start)
{
    CODE(F, OP_LOOP);
    UInt offset = codeoffset(F) - start + 3;
    if(offset >= BYTECODE_MAX) JUMP_LIMIT_ERR(F, BYTECODE_MAX);
    CODEL(F, offset);
}

// Initialize global variable
#define INIT_GLOBAL(F, idx, vflags, E)                                          \
    do {                                                                        \
        (F)->vm->globvals[idx].flags = vflags;                                  \
        CODEOP(F, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, E), idx);                  \
    } while(false)

// Check if Tokens are equal
sstatic force_inline bool nameeq(Token* left, Token* right)
{
    return (left->len == right->len) &&
           (memcmp(left->start, right->start, left->len) == 0);
}

// Get local variable
sstatic force_inline Int get_local(Function* F, Token* name)
{
    for(Int i = F->locals.len - 1; i >= 0; i--) {
        Local* local = Array_Local_index(&F->locals, i);
        if(nameeq(name, &local->name)) {
            if(local->depth == -1)
                LOCAL_DEFINITION_ERR(F, name->len, name->start);
            return i;
        }
    }
    return -1;
}

sstatic force_inline UInt
add_upval(Function* F, UInt idx, Byte flags, bool local)
{
    Int upvalc = F->fn->upvalc;
    ASSERT(upvalc < (Int)F->upvalues->len + 1, "Invalid upvalc.");
    for(Int i = 0; i < upvalc; i++) {
        Upvalue* upvalue = Array_Upvalue_index(F->upvalues, i);
        if(upvalue->idx == idx && upvalue->local == local)
            return i; // already exists
    }
    if(unlikely(upvalc >= INDEX_MAX)) {
        UPVALUE_LIMIT_ERR(F, INDEX_MAX, F->fn->name->storage);
        return 0;
    }
    Upvalue upval = {idx, flags, local};
    if(upvalc == (Int)F->upvalues->len) Array_Upvalue_push(F->upvalues, upval);
    else *Array_Upvalue_index(F->upvalues, upvalc) = upval;
    return F->fn->upvalc++;
}

sstatic Int get_upval(Function* F, Token* name)
{
    if(F->enclosing == NULL) return -1;
    Int idx = get_local(F->enclosing, name);
    if(idx != -1) {
        Local* l = Array_Local_index(&F->enclosing->locals, idx);
        LFLAG_SET(l, VCAPTURED_BIT);
        return add_upval(F, (UInt)idx, l->flags, true);
    }
    idx = get_upval(F->enclosing, name);
    if(idx != -1) {
        Local* l = Array_Local_index(&F->enclosing->locals, idx);
        return add_upval(F, (UInt)idx, l->flags, false);
    }
    return -1;
}

sstatic force_inline UInt globalvar(Function* F, Value identifier)
{
    Value    index;
    Variable glob = {UNDEFINED_VAL, F->vflags};
    VM*      vm   = F->vm;
    if(!HashTable_get(&vm->globids, identifier, &index)) {
        if(unlikely((vm)->globlen + 1 > BYTECODE_MAX))
            GLOBALS_LIMIT_ERR(F, BYTECODE_MAX);
        push(vm, identifier);
        index = NUMBER_VAL(GARRAY_PUSH(vm, glob));
        HashTable_insert(vm, &vm->globids, identifier, index);
        pop(vm);
    }
    return (UInt)AS_NUMBER(index);
}

sstatic force_inline Value tokintostr(VM* vm, const Token* name)
{
    return OBJ_VAL(OString_from(vm, name->start, name->len));
}

// Make global variable
#define MAKE_GLOBAL(F, name)                                                    \
    ({                                                                          \
        Value identifier = tokintostr((F)->vm, name);                           \
        globalvar(F, identifier);                                               \
    })

sstatic force_inline Int codevar(Function* F, Token name, Exp* E)
{
    OpCode getop;
    Int    idx = get_local(F, &name);
    if(idx != -1) {
        E->type = EXP_LOCAL;
        getop   = GET_OP_TYPE(idx, OP_GET_LOCAL, E);
    } else if((idx = get_upval(F, &name)) != -1) {
        E->type  = EXP_UPVAL;
        E->ins.l = true;
        getop    = OP_GET_UPVALUE;
    } else {
        E->type = EXP_GLOBAL;
        idx     = MAKE_GLOBAL(F, &name);
        getop   = GET_OP_TYPE(idx, OP_GET_GLOBAL, E);
    }
    E->value = idx;
    if(!E->ins.set) return (E->ins.code = CODEOP(F, getop, idx));
    else return (E->ins.code = -1); // this is assignment
}

sstatic force_inline UInt codevarprev(Function* F, Exp* E)
{
    return codevar(F, PREVT(F), E);
}

// helper [rmlastins]
sstatic force_inline void popvarins(Function* F, Exp* E)
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
sstatic force_inline void popcallins(Function* F, Exp* E)
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

sstatic void rmlastins(Function* F, Exp* E)
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
sstatic force_inline void
startscope(Function* F, Scope* S, Byte isloop, Byte isswitch)
{
    if(unlikely((UInt)F->S->depth >= BYTECODE_MAX))
        SCOPE_LIMIT_ERR(F, BYTECODE_MAX);
    S->localc   = F->locals.len;
    S->isloop   = isloop;
    S->isswitch = isswitch;
    S->depth    = F->S->depth + 1;
    S->prev     = F->S;
    F->S        = S;
}

// End scope and pop locals and/or close captured locals
sstatic void endscope(Function* F)
{
#define LOCAL_IS_CAPTURED(local) (LFLAG_CHECK((local), VCAPTURED_BIT))

    F->fn->gotret  = 0;
    Int    pop     = 0;
    Scope* current = F->S;
    F->S           = current->prev;
    while(F->locals.len > 0 && Array_Local_last(&F->locals)->depth > F->S->depth)
    {
        if(LOCAL_IS_CAPTURED(Array_Local_last(&F->locals))) {
            Int capture = 1;
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


sstatic void ControlFlow_init(VM* vm, ControlFlow* cflow)
{
    cflow->innerlstart = -1;
    cflow->innerldepth = 0;
    cflow->innersdepth = 0;
    Array_Array_Int_init(&cflow->breaks, vm);
}

sstatic void ControlFlow_free(ControlFlow* context)
{
    Array_Array_Int_free(&context->breaks, NULL);
    context->innerlstart = -1;
    context->innerldepth = 0;
    context->innersdepth = 0;
}








/*========================== FUNCTION STATE =========================*/

sstatic void F_init(
    Function*    F,
    Scope*       globscope,
    Class*       cclass,
    VM*          vm,
    Lexer*       lexer,
    FunctionType fn_type,
    Value        loaded,
    Function*    enclosing)
{
    // Initialize global scope
    globscope->prev     = NULL;
    globscope->depth    = 0;
    globscope->isloop   = 0;
    globscope->isswitch = 0;
    globscope->localc   = 1;
    // Initialize Function state
    F->vm        = vm;
    F->S         = globscope;
    F->enclosing = enclosing;
    F->cclass    = cclass;
    F->lexer     = lexer;
    vm->F        = F;
    F->fn        = NULL; // Initialize to NULL so gc does not get confused
    F->fn        = OFunction_new(vm);
    F->fn_type   = fn_type;
    F->vflags    = 0;
    ControlFlow_init(vm, &F->cflow);
    if(enclosing == NULL) {
        F->upvalues = GC_MALLOC(vm, sizeof(Array_Upvalue));
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
        local->name.len   = 4;
    } else {
        local->name.start = "";
        local->name.len   = 0;
    }
    if(fn_type == FN_SCRIPT) F->fn->name = AS_STRING(loaded);
    else F->fn->name = OString_from(vm, PREVT(F).start, PREVT(F).len);
}

void F_free(Function* F)
{
    VM* vm = F->vm;
    ControlFlow_free(&F->cflow);
    if(F->enclosing == NULL) {
        ASSERT(
            F->fn_type == FN_SCRIPT,
            "Function is top-level but the type is not 'FN_SCRIPT'.");
        Array_Upvalue_free(F->upvalues, NULL);
        GC_FREE(vm, F->upvalues, sizeof(Array_Upvalue));
    }
    Array_Local_free(&F->locals, NULL);
    vm->F = F->enclosing;
    GC_FREE(vm, F, sizeof(Function));
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
    for(Function* current = vm->F; current != NULL; current = current->enclosing)
    {
        vmark(vm, PREVT(current).value);
        vmark(vm, CURRT(current).value);
        omark(vm, (O*)current->fn);
    }
}








/*========================== PARSING =========================*/

// Advance to the next token
sstatic void advance(Function* F)
{
    PREVT(F) = CURRT(F);
    while(true) {
        CURRT(F) = scan(F->lexer);
        if(CURRT(F).type != TOK_ERROR) break;
        error(F, CURRT(F).start);
    }
}

// Advance and return true if 'type' matches the current token type
sstatic force_inline bool match(Function* F, TokenType type)
{
    if(CURRT(F).type != type) return false;
    advance(F);
    return true;
}

// Sync lexer to the next statement
sstatic void sync(Function* F)
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
#define expect_cond(F, cond, err)                                               \
    if(!cond) error(F, err);

// Invoke compile-time error if 'type' does not match the current token type
sstatic force_inline void expect(Function* F, TokenType type, const char* err)
{
    if(CURRT(F).type == type) {
        advance(F);
        return;
    }
    error(F, err);
}

// End compilation of the function and emit return instruction
sstatic force_inline OFunction* compile_end(Function* F)
{
    coderet(F, false, F->fn->gotret);
#ifdef DEBUG_PRINT_CODE
    if(!F->lexer->error) {
        OFunction* fn = F->fn;
        Chunk_debug(CHUNK(F), fn->name->storage);
    }
#endif
    return F->fn;
}

// Compile source code
OClosure* compile(VM* vm, const char* source, Value name)
{
    vm->script = name;
    HashTable_insert(vm, &vm->loaded, name, EMPTY_VAL);
    Function* F = MALLOC(vm, sizeof(Function));
    Lexer     L = L_new(source, vm);
    Scope     globalscope;
    F_init(F, &globalscope, NULL, vm, &L, FN_SCRIPT, name, vm->F);
    advance(F);
    while(!match(F, TOK_EOF))
        dec(F);
    OFunction* fn  = compile_end(F);
    bool       err = F->lexer->error;
    F_free(F);
    push(vm, OBJ_VAL(fn));
    OClosure* closure = OClosure_new(vm, fn);
    pop(vm);
    return (err ? NULL : closure);
}

// Create new local variable
sstatic void local_new(Function* F, Token name)
{
    if(unlikely((Int)F->locals.len >= INDEX_MAX)) {
        LOCAL_LIMIT_ERR(F, INDEX_MAX);
        return;
    }
    Array_Local_push(&F->locals, (Local){name, -1, F->vflags});
}

// Make local variable but check for redefinitions in local scope
sstatic void make_local(Function* F, Token* name)
{
    for(Int i = F->locals.len - 1; i >= 0; i--) {
        Local* local = Array_Local_index(&F->locals, i);
        if(local->depth != -1 && local->depth < F->S->depth) break;
        if(nameeq(name, &local->name))
            LOCAL_REDEFINITION_ERR(F, name->len, name->start);
    }
    local_new(F, *name);
}

// Patch jump instruction
sstatic force_inline void patchjmp(Function* F, Int jmp_offset)
{
    Int offset = codeoffset(F) - jmp_offset - 3;
    if(unlikely(offset >= BYTECODE_MAX)) JUMP_LIMIT_ERR(F, BYTECODE_MAX);
    PUT_BYTES3(&CHUNK(F)->code.data[jmp_offset], offset);
}

sstatic force_inline void startbreaklist(Function* F)
{
    Array_Int patches;
    Array_Int_init(&patches, F->vm);
    Array_Array_Int_push(&F->cflow.breaks, patches);
}

sstatic force_inline void patchbreaklist(Function* F)
{
    Array_Int* patches = Array_Array_Int_last(&F->cflow.breaks);
    for(Int i = 0; i < (Int)patches->len; i++)
        patchjmp(F, patches->data[i]);
}

sstatic force_inline void endbreaklist(Function* F)
{
    Array_Int last = Array_Array_Int_pop(&F->cflow.breaks);
    Array_Int_free(&last, NULL);
}


sstatic force_inline UInt make_constant(Function* F, Value constant)
{
    if(unlikely((Int)CHUNK(F)->constants.len > INDEX_MAX))
        CONSTANT_LIMIT_ERR(F, F->fn->name->storage, INDEX_MAX);
    return Chunk_make_constant(F->vm, CHUNK(F), constant);
}








/*========================== OPERATIONS/PRIORITY =========================*/

typedef enum {
    OPR_FALSEY = 0,
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
sstatic UnaryOpr getunaryopr(TokenType type)
{
    switch(type) {
        case TOK_BANG:
            return OPR_FALSEY;
        case TOK_MINUS:
            return OPR_NEGATE;
        default:
            return OPR_NOUNARYOPR;
    }
}

// Fetch binary operation
sstatic BinaryOpr getbinaryopr(TokenType type)
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
sstatic OpCode unopr2op(UnaryOpr opr)
{
    switch(opr) {
        case OPR_NEGATE:
            return OP_NEG;
        case OPR_FALSEY:
            return OP_NOT;
        default:
            unreachable;
    }
}

// Fetch binary instruction
sstatic OpCode binopr2op(BinaryOpr opr)
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

sstatic const struct {
    Byte left; // Left priority
    Byte right; // Right priority
} priority[] = {
    [OPR_ADD] = {4, 4}, /* '+' */
    [OPR_SUB] = {4, 4}, /* '-' */
    [OPR_MUL] = {5, 5}, /* '*' */
    [OPR_DIV] = {5, 5}, /* '/' */
    [OPR_MOD] = {5, 5}, /* '%' */
    [OPR_POW] = {8, 7}, /* '^' (right associative) */
    [OPR_NE]  = {3, 3}, /* '!=' */
    [OPR_EQ]  = {3, 3}, /* '==' */
    [OPR_LT]  = {3, 3}, /* '<' */
    [OPR_LE]  = {3, 3}, /* '<=' */
    [OPR_GT]  = {3, 3}, /* '>' */
    [OPR_GE]  = {3, 3}, /* '>=' */
    [OPR_AND] = {2, 2}, /* 'and' */
    [OPR_OR]  = {1, 1}, /* 'or' */
};

#define UNARY_PRIORITY 6








/*========================== STATEMENT/DECLARATION =========================*/

// vararg ::= '...'
sstatic force_inline UInt vararg(Function* F)
{
    if(!F->fn->isva) VARARG_ERR(F);
    return CODEOP(F, OP_VALIST, 1);
}

sstatic void setmulret(Function* F, Exp* E)
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
sstatic void adjustassign(Function* F, Exp* E, Int left, Int right)
{
    Int leftover = left - right; // Safety: left < right is a compile error
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
sstatic Int explist(Function* F, Int limit, Exp* E)
{
    Int left = limit;
    Int got  = 0;
    do {
        left--;
        got++;
        if(left < 0) EXPLIST_LIMIT_ERR(F, limit);
        expr(F, E);
    } while(match(F, TOK_COMMA));
    return got;
}

sstatic Int name(Function* F, const char* errmsg)
{
    expect(F, TOK_IDENTIFIER, errmsg);
    Token* name = &PREVT(F);
    if(F->S->depth > 0) { // If local scope make local variable
        make_local(F, name);
        return -1;
    } // Otherwise make global variable
    return MAKE_GLOBAL(F, name);
}

// helper [exprstm]
sstatic void codeset(Function* F, Exp* E)
{
    switch(E->type) {
        case EXP_UPVAL:
            CODEOP(F, OP_SET_UPVALUE, E->value);
            break;
        case EXP_LOCAL:
            CODEOP(F, GET_OP_TYPE(E->value, OP_SET_LOCAL, E), E->value);
            break;
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
sstatic void codesetall(Function* F, Array_Exp* Earr)
{
    for(Int i = 0; i < (Int)Earr->len; i++) {
        Exp* E = Array_Exp_index(Earr, i);
        codeset(F, E);
    }
}

/// exprstm ::= functioncall
///           | varlist '=' explist
sstatic void exprstm(Function* F, bool lastclause)
{
    Exp E;
    E.ins.set = false;
    suffixedexp(F, &E);
    TokenType next = CURRT(F).type;
    if(next == TOK_EQUAL || next == TOK_COMMA) {
        E.ins.set = true;
        Array_Exp Earr;
        Array_Exp_init(&Earr, F->vm);
        expect_cond(F, etisvar(E.type), "Expect variable.");
        rmlastins(F, &E); // remove 'OP_GET..'
        Array_Exp_push(&Earr, E);
        Int vars = 1;
        while(match(F, TOK_COMMA)) {
            if(unlikely(vars >= BYTECODE_MAX))
                VARLIST_LIMIT_ERR(F, BYTECODE_MAX);
            vars++;
            suffixedexp(F, &E);
            expect_cond(F, etisvar(E.type), "Expect variable.");
            Array_Exp_push(&Earr, E);
        }
        expect(F, TOK_EQUAL, "Expect '='.");
        E.ins.set = false;
        Int expc  = explist(F, vars, &E);
        if(vars != expc) adjustassign(F, &E, vars, expc);
        codesetall(F, &Earr);
        Array_Exp_free(&Earr, NULL);
    } else {
        if(etisvar(E.type)) {
            rmlastins(F, &E); // remove 'OP_GET...'
            CODE(F, OP_NIL);
            codeset(F, &E);
        } else if(etiscall(E.type)) CODE(F, OP_POP);
        else error(F, "Invalid syntax, expect exprstm or dec.");
    }
    if(!lastclause) expect(F, TOK_SEMICOLON, "Expect ';'.");
}

sstatic void block(Function* F)
{
    while(!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
        dec(F);
    expect(F, TOK_RBRACE, "Expect '}' after block.");
}

sstatic force_inline void blockstm(Function* F)
{
    Scope S;
    startscope(F, &S, 0, 0);
    block(F);
    endscope(F);
}

// arglist ::= name
//           | '...'
//           | name ',' arglist
sstatic void arglist(Function* F)
{
    do {
        if(match(F, TOK_DOT_DOT_DOT)) {
            F->fn->isva = true;
            break;
        }
        F->fn->arity++;
        name(F, "Expect parameter name.");
        INIT_LOCAL(F, 0);
    } while(match(F, TOK_COMMA));
}

// namelist ::= name
//            | name ',' namelist
sstatic UInt namelist(Function* F, Array_Int* nameidx)
{
    Int names = 0;
    do {
        if(names >= BYTECODE_MAX) NAMELIST_LIMIT_ERR(F, BYTECODE_MAX);
        names++;
        Int idx = name(F, "Expect name."); // initialize later
        if(F->S->depth == 0) Array_Int_push(nameidx, idx);
    } while(match(F, TOK_COMMA));
    return names;
}

sstatic void codeassign(Function* F, Int names, Array_Int* nameidx)
{
    if(F->S->depth > 0) {
        for(Int i = 0; i < names; i++)
            INIT_LOCAL(F, i);
        return;
    }
    ASSERT(names == (Int)nameidx->len, "name count != indexes array len.");
    while(nameidx->len > 0) {
        Int idx = Array_Int_pop(nameidx);
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
sstatic void vardec(Function* F)
{
    if(match(F, TOK_FIXED)) {
        if(FIS(F, FFIXED)) error(F, "Expect variable name.");
        FSET(F, FFIXED);
    }
    Array_Int nameidx;
    Array_Int_init(&nameidx, F->vm);
    Int names = namelist(F, &nameidx);
    Int expc  = 0;
    Exp E;
    E.ins.set = false;
    if(match(F, TOK_EQUAL)) expc = explist(F, names, &E);
    if(names != expc) adjustassign(F, &E, names, expc);
    codeassign(F, names, &nameidx);
    Array_Int_free(&nameidx, NULL);
    expect(F, TOK_SEMICOLON, "Expect ';'.");
}

// fvardec ::= 'fixed' vardec
sstatic force_inline void fvardec(Function* F)
{
    FSET(F, FFIXED);
    advance(F);
    vardec(F);
}

// Create and parse a new Function
sstatic void fn(Function* F, FunctionType type)
{
    Function* Fnew = GC_MALLOC(F->vm, sizeof(Function));
    Scope     globscope, S;
    F_init(Fnew, &globscope, F->cclass, F->vm, F->lexer, type, NIL_VAL, F);
    startscope(Fnew, &S, 0, 0); // no need to end this scope
    expect(Fnew, TOK_LPAREN, "Expect '(' after function name.");
    if(!check(Fnew, TOK_RPAREN)) arglist(Fnew);
    if(F->fn->isva) expect(Fnew, TOK_RPAREN, "Expect ')' after '...'.");
    else expect(Fnew, TOK_RPAREN, "Expect ')' after parameters.");
    expect(Fnew, TOK_LBRACE, "Expect '{' before function body.");
    block(Fnew); // body
    OFunction* fn = compile_end(Fnew);
    fn->isinit    = (type == FN_INIT);
    CODEOP(F, OP_CLOSURE, make_constant(F, OBJ_VAL(fn)));
    for(UInt i = 0; i < fn->upvalc; i++) {
        Upvalue* upval = Array_Upvalue_index(Fnew->upvalues, i);
        CODE(F, upval->local ? 1 : 0);
        CODE(F, upval->flags);
        CODEL(F, upval->idx);
    }
    F_free(Fnew);
}

// fndec ::= 'fn' name '(' arglist ')' '{' block '}'
sstatic void fndec(Function* F)
{
    UInt idx = name(F, "Expect function name.");
    if(F->S->depth > 0) INIT_LOCAL(F, 0); // initialize to allow recursion
    fn(F, FN_FUNCTION);
    Exp _; // dummy
    if(F->S->depth == 0) INIT_GLOBAL(F, idx, 0, &_);
}

sstatic void method(Function* F)
{
    expect(F, TOK_FN, "Expect 'fn'.");
    expect(F, TOK_IDENTIFIER, "Expect method name.");
    Value        identifier = tokintostr(F->vm, &PREVT(F));
    UInt         idx        = make_constant(F, identifier);
    FunctionType type       = FN_METHOD;
    if(AS_STRING(identifier) == F->vm->statics[SS_INIT]) type = FN_INIT;
    fn(F, type);
    if(type == FN_INIT) CODEOP(F, OP_OVERLOAD, SS_INIT);
    CODEOP(F, OP_METHOD, idx);
}

sstatic void classdec(Function* F)
{
    expect(F, TOK_IDENTIFIER, "Expect class name.");
    Token class_name = PREVT(F);
    Value identifier = tokintostr(F->vm, &class_name);
    UInt  idx        = make_constant(F, identifier);
    Exp   _; // dummy
    CODEOP(F, OP_CLASS, idx);
    if(F->S->depth > 0) {
        make_local(F, &class_name);
        INIT_LOCAL(F, 0);
    } else INIT_GLOBAL(F, MAKE_GLOBAL(F, &class_name), 0, &_);
    Class cclass;
    cclass.enclosing  = F->cclass;
    cclass.superclass = false;
    F->cclass         = &cclass;
    _.ins.set         = false;
    Scope S;
    if(match(F, TOK_IMPL)) { // have superclass ?
        expect(F, TOK_IDENTIFIER, "Expect superclass name.");
        codevarprev(F, &_); // get superclass
        if(nameeq(&PREVT(F), &class_name))
            CLASS_INHERIT_ERR(F, AS_CSTRING(identifier));
        startscope(F, &S, 0, 0);
        local_new(F, syntoken("super"));
        INIT_LOCAL(F, 0);
        codevar(F, class_name, &_);
        CODE(F, OP_INHERIT);
        cclass.superclass = true;
    }
    codevar(F, class_name, &_);
    expect(F, TOK_LBRACE, "Expect '{' before class body.");
    while(!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
        method(F);
    expect(F, TOK_RBRACE, "Expect '}' after class body.");
    CODE(F, OP_POP); // Pop the class
    if(cclass.superclass) endscope(F);
    F->cclass = cclass.enclosing;
}

/// call ::= '(' ')'
///        | '(' explist ')'
sstatic void call(Function* F, Exp* E)
{
    CODE(F, OP_CALLSTART);
    if(!check(F, TOK_RPAREN)) explist(F, BYTECODE_MAX, E);
    else E->type = EXP_NONE;
    expect(F, TOK_RPAREN, "Expect ')'.");
    if(ethasmulret(E->type)) setmulret(F, E);
}

sstatic void codecall(Function* F, Exp* E)
{
    call(F, E);
    E->type     = EXP_CALL;
    E->ins.code = CODEOP(F, OP_CALL, 1);
}

sstatic void codeinvoke(Function* F, Exp* E, Int idx)
{
    call(F, E);
    E->type     = EXP_INVOKE;
    E->ins.code = CODEOP(F, OP_INVOKE, idx);
    CODEL(F, 1); // retcnt
}

sstatic void dec(Function* F)
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
    Int patch; // Jump to patch if case expression does not match
    enum {
        CS_MATCH, // Case is constant expression match
        CS_DFLT, // Case is 'default'
        CS_NONE, // Did not parse any cases yet
        CS_CASE, // Case is 'case'
    } casestate;
    bool        dflt; // if switch has 'default' case
    bool        havenil; // if switch has 'nil' case
    bool        havetrue; // if switch has 'true' case
    bool        havefalse; // if switch has 'false' case
    Array_Value constants; // all case constant expressions
} SwitchState;

sstatic force_inline void SwitchState_init(Function* F, SwitchState* state)
{
    state->patch     = -1;
    state->casestate = CS_NONE;
    state->dflt      = false;
    state->havenil   = false;
    state->havetrue  = false;
    state->havefalse = false;
    Array_Value_init(&state->constants, F->vm);
}

#define SwitchState_free(state) Array_Value_free(&(state)->constants, NULL);

/*
 * Updates 'switch' constants and checks if the constant
 * already exists, but only if 'e' is constant expression.
 */
sstatic force_inline void
switchconstants(Function* F, SwitchState* state, Exp* E)
{
    if(!etisconst(E->type)) return;
    switch(E->type) {
        case EXP_FALSE:
            if(state->havefalse) SWITCH_DUPLICATE_ERR(F, "false");
            state->havefalse = true;
            break;
        case EXP_TRUE:
            if(state->havetrue) SWITCH_DUPLICATE_ERR(F, "true");
            state->havetrue = true;
            break;
        case EXP_NIL:
            if(state->havenil) SWITCH_DUPLICATE_ERR(F, "nil");
            state->havenil = true;
            break;
        case EXP_STRING:
        case EXP_NUMBER:;
            Value caseval = *CONSTANT(F, E);
            for(Int i = 0; i < (Int)state->constants.len; i++) {
                if(veq(state->constants.data[i], caseval)) {
                    SWITCH_DUPLICATE_ERR(F, vtostr(F->vm, caseval)->storage);
                    return;
                }
            }
            Array_Value_push(&state->constants, caseval);
            break;
        default:
            unreachable;
    }
}

sstatic void switchstm(Function* F)
{
    Context     C;
    Scope       S;
    SwitchState swstate;
    Byte        inswitch = F->S->isswitch;
    Exp         E1;
    Array_Int   fts;
    Int         sdepth;
    savecontext(F, &C);
    SwitchState_init(F, &swstate);
    Array_Int_init(&fts, F->vm);
    startscope(F, &S, 0, 1); // implicit scope
    startbreaklist(F);
    expect(F, TOK_LPAREN, "Expect '(' after 'switch'.");
    E1.ins.set = false;
    expr(F, &E1);
    expect(F, TOK_RPAREN, "Expect ')' after condition.");
    expect(F, TOK_LBRACE, "Expect '{' after ')'.");
    sdepth               = F->cflow.innersdepth;
    F->cflow.innersdepth = F->S->depth;
    // Switch must contain case or default before any statements
    if(!check(F, TOK_RBRACE) && !check(F, TOK_EOF) && !check(F, TOK_CASE) &&
       !check(F, TOK_DEFAULT))
        SWITCH_NOCASE_ERR(F);
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
                expect(F, TOK_COLON, "Expect ':' after 'case'.");
                switchconstants(F, &swstate, &E2);
                if(eareconstandeq(F, &E1, &E2)) {
                    restorecontext(F, &C);
                    swstate.casestate = CS_MATCH;
                } else {
                    CODE(F, OP_EQ);
                    swstate.casestate = CS_CASE;
                    swstate.patch     = CODEJMP(F, OP_JMP_IF_FALSE_POP);
                }
            } else if(!swstate.dflt) {
                swstate.dflt      = true;
                swstate.casestate = CS_DFLT;
                expect(F, TOK_COLON, "Expect ':' after 'default'.");
            } else SWITCH_DEFAULT_ERR(F);
            if(fts.len > 0) patchjmp(F, Array_Int_pop(&fts));
        } else {
            stm(F);
            if(swstate.casestate == CS_MATCH && F->fn->gotret) {
                // @TODO: Implement optimizations.
                // Also check if last 'stm' was 'breakstm' (same effect)
            }
        }
    }
    if(PREVT(F).type == TOK_EOF) SWITCH_RBRACE_ERR(F);
    Array_Int_free(&fts, NULL);
    SwitchState_free(&swstate);
    endscope(F);
    CODE(F, OP_POP); // pop switch expression
    patchbreaklist(F);
    endbreaklist(F);
    F->cflow.innersdepth = sdepth;
    F->S->isswitch       = inswitch;
}

sstatic void ifstm(Function* F)
{
    Exp     E;
    Context C;
    Int     jmptoelse, jmptoend = -1;
    savecontext(F, &C);
    expect(F, TOK_LPAREN, "Expect '(' after 'if'.");
    E.ins.set = false;
    expr(F, &E); // condition
    expect(F, TOK_RPAREN, "Expect ')' after condition.");
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

sstatic void startloop(Function* F, Int* lstart, Int* ldepth)
{
    *lstart                = (F)->cflow.innerlstart;
    *ldepth                = (F)->cflow.innerldepth;
    (F)->cflow.innerlstart = codeoffset(F);
    (F)->cflow.innerldepth = F->S->depth;
}

sstatic void endloop(Function* F, Int lstart, Int ldepth)
{
    (F)->cflow.innerlstart = lstart;
    (F)->cflow.innerldepth = ldepth;
}

sstatic void whilestm(Function* F)
{
    Scope   S;
    Context C;
    Exp     E;
    Int     lstart, ldepth;
    Int     jmptoend = -1;
    bool    infinite = false;
    bool    remove   = false;
    savecontext(F, &C);
    startscope(F, &S, 1, 0);
    startloop(F, &lstart, &ldepth);
    startbreaklist(F);
    expect(F, TOK_LPAREN, "Expect '(' after 'while'.");
    E.ins.set = false;
    expr(F, &E); // conditional
    if(etisconst(E.type)) {
        rmlastins(F, &E);
        if(etisfalse(E.type)) remove = true;
        else infinite = true;
    } else jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
    expect(F, TOK_RPAREN, "Expect ')' after condition.");
    stm(F); // body
    bool gotret = F->fn->gotret;
    endscope(F);
    if(!remove) {
        codeloop(F, (F)->cflow.innerlstart);
        if(!infinite) {
            ASSERT(jmptoend != -1, "end jmp invalid but flag is false.");
            patchjmp(F, jmptoend);
        } else if(gotret) { // cond true and 'stm' was 'returnstm'
            // @TODO: Implement optimizations
        }
        patchbreaklist(F);
    } else restorecontext(F, &C);
    endloop(F, lstart, ldepth);
    endbreaklist(F);
}

sstatic void forstm(Function* F)
{
    Scope   S;
    Context C;
    Exp     E;
    Int     lstart, ldepth;
    Int     jmptoend = -1;
    bool    remove   = false;
    bool    infinite = false;
    startscope(F, &S, 1, 0);
    startbreaklist(F);
    expect(F, TOK_LPAREN, "Expect '(' after 'for'.");
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
        expect(F, TOK_SEMICOLON, "Expect ';' after for-loop condition clause.");
    } else infinite = true;
    if(!match(F, TOK_RPAREN)) { // last for-clause
        Int jmptobody = -1;
        Int jmptoincr = -1;
        if(!infinite && !remove) jmptobody = CODEJMP(F, OP_JMP);
        if(!remove) jmptoincr = codeoffset(F);
        exprstm(F, true);
        if(!infinite && !remove) {
            codeloop(F, (F)->cflow.innerlstart);
            patchjmp(F, jmptobody);
            (F)->cflow.innerlstart = jmptoincr;
        }
        expect(F, TOK_RPAREN, "Expect ')' after last for-loop clause.");
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

sstatic Int foreachvars(Function* F)
{
    Int vars = 0;
    do {
        if(vars >= BYTECODE_MAX) VARLIST_LIMIT_ERR(F, BYTECODE_MAX);
        vars++;
        expect(F, TOK_IDENTIFIER, "Expect varname.");
        make_local(F, &PREVT(F));
        INIT_LOCAL(F, 0);
    } while(match(F, TOK_COMMA));
    return vars;
}

sstatic void newlocalliteral(Function* F, const char* name)
{
    Token syntok = syntoken(name);
    local_new(F, syntok);
    INIT_LOCAL(F, 0);
}

sstatic void foreachstm(Function* F)
{
    Scope S;
    Int   lstart, ldepth, vars, expc, endjmp;
    Exp   E;
    startscope(F, &S, 1, 0);
    S.isgloop = 1; // set as generic loop
    startbreaklist(F);
    newlocalliteral(F, "(for iterator)"); // iterator function
    newlocalliteral(F, "(for invstate)"); // invariant state
    newlocalliteral(F, "(for cntlvar)"); // control variable
    vars = foreachvars(F); // declared vars
    expect(F, TOK_IN, "Expect 'in'.");
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

sstatic void loopstm(Function* F)
{
    Scope S;
    Int   lstart, ldepth;
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

sstatic force_inline Scope* loopscope(Function* F)
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
sstatic force_inline Int switchcnt(Function* F)
{
    Scope* S     = F->S;
    Int    count = 0;
    while(S != NULL && S->depth > F->cflow.innerldepth) {
        if(S->isswitch) count++;
        S = S->prev;
    }
    return count;
}

sstatic void continuestm(Function* F)
{
    expect(F, TOK_SEMICOLON, "Expect ';' after 'continue'.");
    if(F->cflow.innerlstart == -1) CONTINUE_ERR(F);
    else {
        Scope* S = loopscope(F);
        ASSERT(S != NULL, "Loop scope not found but cflow offset is set.");
        Int popn = F->locals.len - (S->isgloop * 3) - S->localc + switchcnt(F);
        CODEPOP(F, popn);
        codeloop(F, F->cflow.innerlstart);
    }
}

sstatic void breakstm(Function* F)
{
    expect(F, TOK_SEMICOLON, "Expect ';' after 'break'.");
    Array_Array_Int* arr   = &F->cflow.breaks;
    Int              popn  = 0;
    Scope*           scope = NULL;
    switch(inwhat(F->S, &scope)) {
        case -1: // outside
            BREAK_ERR(F);
            return;
        case 0: // in switch
            popn++; // switch expression
        case 1: // in loop
            break;
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
sstatic void returnstm(Function* F)
{
    /*
     * @TODO:
     * Optimize even further by removing all of the unreachable code.
     */
    Context      C;
    bool         gotret = F->fn->gotret;
    FunctionType type   = F->fn_type;
    savecontext(F, &C);
    CODE(F, OP_RETSTART);
    if(match(F, TOK_SEMICOLON)) coderet(F, true, gotret);
    else {
        if(type == FN_INIT) RETURN_INIT_ERR(F, static_str[SS_INIT].name);
        Exp E;
        E.ins.set = false;
        explist(F, BYTECODE_MAX, &E);
        expect(F, TOK_SEMICOLON, "Expect ';' after return statement value/s.");
        if(gotret) {
            F->fn->gotret = 1;
            restorecontext(F, &C);
        } else {
            if(ethasmulret(E.type)) setmulret(F, &E);
            if(type == FN_SCRIPT) CODE(F, OP_TOPRET);
            else CODE(F, OP_RET);
        }
    }
}


/// dot ::= '.' name
///       | '.' name call
sstatic void dot(Function* F, Exp* E)
{
    expect(F, TOK_IDENTIFIER, "Expect property name after '.'.");
    Value identifier = tokintostr(F->vm, &PREVT(F));
    UInt  idx        = make_constant(F, identifier);
    if(match(F, TOK_LPAREN)) codeinvoke(F, E, idx);
    else {
        E->type  = EXP_INDEXED;
        E->value = idx;
        if(!E->ins.set) E->ins.code = CODEOP(F, OP_GET_PROPERTY, idx);
    }
}

sstatic void stm(Function* F)
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
sstatic force_inline void indexed(Function* F, Exp* E)
{
    Exp E2;
    E2.ins.set = false;
    expr(F, &E2);
    expect(F, TOK_RBRACK, "Expect ']'.");
    if(match(F, TOK_LPAREN)) {
        if(etisconst(E2.type)) CALL_CONST_ERR(F);
        call(F, E);
        E->type     = EXP_INVOKE_INDEX;
        E->ins.code = CODEOP(F, OP_INVOKE_INDEX, 1);
    } else {
        E->type  = EXP_INDEXED;
        E->value = NO_VAL;
        if(!E->ins.set) E->ins.code = CODE(F, OP_INDEX);
    }
}








/*========================== EXPRESSION =========================*/

sstatic void _self(Function* F, Exp* E)
{
    if(F->cclass == NULL) {
        SELF_ERR(F);
        return;
    }
    advance(F);
    codevarprev(F, E);
}

sstatic void _super(Function* F, Exp* E)
{
    if(!F->cclass) {
        SUPER_ERR(F);
        return;
    }
    if(!F->cclass->superclass) {
        NO_SUPER_ERR(F);
        return;
    }
    advance(F);
    expect(F, TOK_DOT, "Expect '.' after 'super'.");
    expect(F, TOK_IDENTIFIER, "Expect superclass method name after '.'.");
    Value methodname = tokintostr(F->vm, &PREVT(F));
    Int   idx        = make_constant(F, methodname);
    Exp   _; // dummy
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
        if(E->ins.set) error(F, "Can't assign to methods from superclass.");
        E->ins.code = CODEOP(F, OP_GET_SUPER, idx);
        E->type     = EXP_INDEXED; // superclass method
    }
}

// primary_exp ::= '(' exp ')'
//               | name
//               | 'self'
sstatic void primaryexp(Function* F, Exp* E)
{
    switch(CURRT(F).type) {
        case TOK_LPAREN:
            advance(F);
            expr(F, E);
            expect(F, TOK_RPAREN, "Expect ')'.");
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
            error(F, "Unexpected symbol.");
            return;
    }
}

// suffixedexp ::= primaryexp
//               | primaryexp [dot|call|indexed...]
sstatic void suffixedexp(Function* F, Exp* E)
{
    primaryexp(F, E);
    while(true) {
        switch(CURRT(F).type) {
            case TOK_DOT:
                advance(F);
                dot(F, E);
                break;
            case TOK_LPAREN:
                if(etisconst(E->type)) CALL_CONST_ERR(F);
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
sstatic void simpleexp(Function* F, Exp* E)
{
    UInt    constidx;
    ExpType type;
    switch(CURRT(F).type) {
        case TOK_NUMBER:
            type = EXP_NUMBER;
            ASSERT(IS_NUMBER(CURRT(F).value), "Expect number.");
            goto constfin;
        case TOK_STRING:
            type = EXP_STRING;
            ASSERT(IS_STRING(CURRT(F).value), "Expect string.");
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
sstatic bool foldunary(Function* F, UnaryOpr opr, Exp* E)
{
    if(E->type == EXP_NUMBER && opr == OPR_NEGATE) {
        double val = AS_NUMBER(*CONSTANT(F, E));
        if(sisnan(val) || val == 0.0) return false;
        *CONSTANT(F, E) = NUMBER_VAL(-val);
        return true;
    }
    return false;
}

// Calculate result of two constant number values by applying binary
// operation (helper function when folding binary operations)
sstatic void
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
            *result = NUMBER_VAL((spowl(n1, n2)));
            break;
        default:
            unreachable;
    }

#undef BINOP
}

// Check if the binary operation is valid
sstatic bool validop(Function* F, BinaryOpr opr, const Exp* E1, const Exp* E2)
{
    double n1 = AS_NUMBER(*CONSTANT(F, E1));
    double n2 = AS_NUMBER(*CONSTANT(F, E2));
    return !(opr == OPR_MOD && (sfloor(n1) != n1 || sfloor(n2) != n2));
}

// Try folding binary operation
// Example: OP_CONST (1), OP_CONST (2), OP_ADD => OP_CONST (3)
sstatic bool foldbinary(Function* F, BinaryOpr opr, Exp* E1, const Exp* E2)
{
    if(E1->type != E2->type || E1->type != EXP_NUMBER ||
       !validop(F, opr, E1, E2))
        return false;
    Value result;
    calcnum(F, opr, E1, E2, &result);
    if(sisnan(AS_NUMBER(result))) return false;
    CONSTANT_POP(F); // Pop constant (E2)
    *CONSTANT(F, E1) = result; // Set new constant value (E1)
    LINSTRUCTION_POP(F); // Pop off the last OP_CONST instruction
    return true;
}

// Emit optimized 'and' instruction
sstatic void codeand(Function* F, Exp* E)
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
            E->jmp.f    = CODEJMP(F, OP_JMP_IF_FALSE_OR_POP);
            E->ins.code = codeoffset(F) - 4; // Index of jump instruction
            break;
    }
    E->jmp.t = NO_JMP;
    E->type  = EXP_JMP;
}

// Emit optimized 'or' instruction
sstatic void codeor(Function* F, Exp* E)
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
sstatic void postfix(Function* F, BinaryOpr opr, Exp* E1, Exp* E2)
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
            E1->ins.code  = CODEBIN(F, opr);
            E1->type      = EXP_EXPR;
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
sstatic void shortcircuit(Function* F, BinaryOpr opr, Exp* E)
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
sstatic force_inline void prefix(Function* F, UnaryOpr opr, Exp* E)
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
sstatic BinaryOpr subexp(Function* F, Exp* E1, Int limit)
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
sstatic void expr(Function* F, Exp* E)
{
    subexp(F, E, 0);
}
