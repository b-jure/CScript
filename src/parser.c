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
    ((idx <= UINT8_MAX) ? ((E)->ins.l = true, op) : ((E)->ins.l = false, op##L))

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
    Array_Local_index(&(F)->locals, (F)->locals.len - (i + 1))->depth =         \
        (F)->S->depth








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
    EXP_FALSE = 0,
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
    EXP_SELF,
    EXP_EXPR,
    EXP_JMP,
} ExpType;

#define etisconst(exptype)   ((exptype) >= EXP_TRUE && (exptype) <= EXP_NUMBER)
#define etisfalse(exptype)   ((exptype) == EXP_FALSE || (exptype) == EXP_NIL)
#define etistrue(exptype)    ((exptype) >= EXP_TRUE || (exptype) <= EXP_NUMBER)
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

// Expressions are constants and their Values are equal
#define eareconstandeq(F, E1, E2)                                               \
    (etisconst((E1)->type) && etisconst((E2)->type) &&                          \
     veq(*CONSTANT(F, E1), *CONSTANT(F, E2)))

// Get instruction
#define INSTRUCTION(F, E)                                                       \
    ((OpCode*)Array_Byte_index(&CHUNK(F)->code, (E)->ins.code))
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
#define PARAM_POP(F) CHUNK(F)->code.len--
// Pop last instruction long parameter (3 bytes)
#define LPARAM_POP(F) CHUNK(F)->code.len -= 3
// Pop last short/simple instruction (1 byte)
#define SINSTRUCTION_POP(F) CHUNK(F)->code.len--
// Pop last instruction with parameter (2 bytes)
#define INSTRUCTION_POP(F) CHUNK(F)->code.len -= 2
// Pop last instruction with long parameter (4 bytes)
#define LINSTRUCTION_POP(F) CHUNK(F)->code.len -= 4

// Get constant
#define CONSTANT(F, E) Array_Value_index(&CHUNK(F)->constants, (E)->value)
// Pop last constant
#define CONSTANT_POP(F) Array_Value_pop(&CHUNK(F)->constants)








typedef struct Scope Scope;
struct Scope {
    Scope* prev; // Linked List
    UInt   localc; // Count of locals up until this scope
    Byte   isupval : 1; // True if some variable in the scope is an upvalue
    Byte   isloop : 1; // This scope is loop
    Byte   isswitch : 1; // This scope is switch
    UInt   depth; // scope depth (index)
};

/*
 * Returns 1 if 'S' is in loop, 0 if inside of a switch
 * and -1 if outside.
 */
sstatic force_inline Int inwhat(Scope* S)
{
    Scope* head = S;
    for(; head != NULL; head = head->prev) {
        if(head->isswitch) return 0;
        if(head->isloop) return 1;
    }
    return -1;
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
    L*             lexer; // grammar lexer
    ControlFlow    cflow; // control flow context
    Scope*         S; // scope state
    OFunction*     fn; // currently parsed function (bytecode chunk)
    FunctionType   fn_type;
    Array_Upvalue* upvalues; // captured variables (hint: closure)
    Byte           vflags; // variable flags
    Array_Local    locals; // local variables stack
};

/* Default 'Function' stack size */
#define SHORT_STACK_SIZE UINT8_MAX

/* Tokens (previous and current) */
#define PREVT(F) (F)->lexer->previous
#define CURRT(F) (F)->lexer->current

/* Currently parsed chunk */
#define CHUNK(F) (&(F)->fn->chunk)

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








/*========================== DEBUG/ERROR =========================*/

// Debug Function stack
sstatic sdebug void dump_stack(Function* F, Int depth, UInt idx)
{
    bool first        = true;
    Int  cached_depth = depth;
    Int  cached_scope = F->S->depth - depth;
    Int  scope        = cached_scope;
    while(scope--)
        fputs("    ", stderr);
    scope = cached_scope;
    fprintf(stderr, "{ - %d\n", idx);
    fputs("    ", stderr);
    while(depth--)
        fputs("    ", stderr);
    for(UInt i = idx; i < F->locals.len; i++) {
        Local* local = Array_Local_index(&F->locals, i);
        if(local->depth < scope) {
            fputc('\n', stderr);
            dump_stack(F, cached_depth - 1, i);
            fputc('\n', stderr);
            break;
        }
        fprintf(
            stderr,
            "%s%.*s",
            first ? "" : ", ",
            local->name.len,
            local->name.start);
        first = false;
    }
    while(cached_depth--)
        fputs("    ", stderr);
    fprintf(stderr, "\n}\n");
}

sstatic sdebug void debug_local_stack(Function* F)
{
    fprintf(stderr, "\n==== stack_dump ====\n");
    fprintf(
        stderr,
        "depth:  %10d |\ncount:  %10d |\n====================\n",
        F->S->depth,
        (Int)F->locals.len);
    fprintf(stderr, "```\n");
    dump_stack(F, F->S->depth, 0);
    fprintf(stderr, "```\n\n");
}

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
#ifdef DEBUG_LOCAL_STACK
    debug_local_stack(F);
#endif
}








//======================= CODE =======================//

// Multi-byte instruction (up to 4 bytes)
#define CODEOP(F, code, param)                                                  \
    Chunk_write_codewparam(CHUNK(F), code, param, PREVT(F).line)

// Single byte instruction
#define CODE(F, byte) Chunk_write(CHUNK(F), byte, PREVT(F).line)

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
        if((n) > 1) CODEOP(F, OP_POPN, n);                                      \
        else CODE(F, OP_POP);                                                   \
    } while(false)

// Emit unary instruction
#define CODEUN(F, opr) CODE(F, unopr2op(opr))

// Emit binary instruction
#define CODEBIN(F, opr) CODE(F, binopr2op(opr))

// Emit return instruction
sstatic force_inline void coderet(Function* F)
{
    if(F->fn_type == FN_INIT) CODEOP(F, OP_GET_LOCAL, 0);
    else {
        if(F->fn_type == FN_SCRIPT) {
            CODE(F, OP_TRUE);
            CODEOP(F, OP_TOPRET, 1);
            return;
        }
        CODE(F, OP_NIL);
    }
    CODEOP(F, OP_RET, 1);
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
#define INIT_GLOBAL(F, idx, E)                                                  \
    CODEOP(F, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, E), idx);

// Check if Tokens are equal
sstatic force_inline bool Identifier_eq(Token* left, Token* right)
{
    return (left->len == right->len) &&
           (memcmp(left->start, right->start, left->len) == 0);
}

// Get local variable
sstatic force_inline Int get_local(Function* F, Token* name)
{
    for(Int i = F->locals.len - 1; i >= 0; i--) {
        Local* local = Array_Local_index(&F->locals, i);
        if(Identifier_eq(name, &local->name)) {
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
    for(UInt i = 0; i < F->upvalues->len; i++) {
        Upvalue* upvalue = Array_Upvalue_index(F->upvalues, i);
        if(upvalue->idx == idx && upvalue->local == local)
            // Return existing UpValue index
            return i;
    }
    if(unlikely(F->upvalues->len == INDEX_MAX)) {
        UPVALUE_LIMIT_ERR(F, INDEX_MAX, F->fn->name->storage);
        return 0;
    }
    // Otherwise add the UpValue into the array
    F->fn->upvalc++;
    Upvalue upval = {idx, flags, local};
    return Array_Upvalue_push(F->upvalues, upval);
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
        if(unlikely((vm)->globlen + 1 > UINT24_MAX))
            GLOBALS_LIMIT_ERR(F, UINT24_MAX);
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

sstatic force_inline UInt codevarprev(Function* F, Token name, Exp* E)
{
    OpCode getop;
    Int    idx   = get_local(F, &name);
    Byte   flags = -1;
    if(idx != -1) {
        E->type    = EXP_LOCAL;
        Local* var = Array_Local_index(&(F)->locals, idx);
        flags      = LFLAGS(var);
        if(!E->ins.set) getop = GET_OP_TYPE(idx, OP_GET_LOCAL, E);
    } else if((idx = get_upval(F, &name)) != -1) {
        E->type        = EXP_UPVAL;
        Upvalue* upval = Array_Upvalue_index(F->upvalues, idx);
        Local*   var   = Array_Local_index(&F->locals, upval->idx);
        flags          = LFLAGS(var);
        if(!E->ins.set) getop = OP_GET_UPVALUE;
        getop = OP_GET_UPVALUE;
    } else {
        E->type        = EXP_GLOBAL;
        idx            = MAKE_GLOBAL(F, &name);
        Variable* glob = &F->vm->globvals[idx];
        flags          = VAR_FLAGS(glob);
        if(!E->ins.set) getop = GET_OP_TYPE(idx, OP_GET_GLOBAL, E);
    }
    E->value = idx;
    return CODEOP(F, getop, idx);
}

sstatic force_inline UInt codevar(Function* F, Exp* E)
{
    return codevarprev(F, PREVT(F), E);
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
            case EXP_SELF:
                if(E->ins.l) LINSTRUCTION_POP(F);
                else INSTRUCTION_POP(F);
            case EXP_JMP:
                goto panic;
                break;
            case EXP_EXPR:
                if(E->ins.binop) LINSTRUCTION_POP(F);
                else {
                panic:
                    PANIC("Tried removing 'and'/'or' expression.");
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
    S->isupval  = 0;
    S->isloop   = isloop;
    S->isswitch = isswitch;
    S->depth    = F->S->prev->depth + 1;
    S->prev     = F->S;
    F->S        = S;
}

// End scope and pop locals and/or close captured locals
sstatic void endscope(Function* F)
{
#define LOCAL_IS_CAPTURED(local) (LFLAG_CHECK((local), VCAPTURED_BIT))
    Scope* current = F->S;
    // Pop the scope, this is safe no need for NULL check,
    // last scope of the function does not get popped, instead
    // the CallFrame gets popped
    F->S    = current->prev;
    Int pop = 0;
    // If there are some captured variables we need to properly
    // close them while also emitting correct OP_POP[N] instructions.
    if(current->isupval) {
        // These 2 loops represent possible interleaved set of upvalues
        // and locals on the stack.
        while(F->locals.len > 0 &&
              Array_Local_last(&F->locals)->depth > F->S->depth)
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
    } else { // If there are no captured vars, just pop the locals
        pop            = F->locals.len - F->S->localc;
        F->locals.len -= pop;
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
    Class*       cclass,
    VM*          vm,
    L*           lexer,
    FunctionType fn_type,
    Value        loaded,
    Function*    enclosing)
{
    F->vm        = vm;
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
    if(fn_type == FN_SCRIPT) {
        vm->script  = loaded;
        F->fn->name = AS_STRING(loaded);
    } else if(fn_type != FN_SCRIPT)
        F->fn->name = OString_from(vm, PREVT(F).start, PREVT(F).len);
}

void F_free(Function* F)
{
    VM* vm = F->vm;
    ControlFlow_free(&F->cflow);
    if(F->enclosing == NULL) {
        vm->script = NIL_VAL;
        ASSERT(
            F->fn_type == FN_SCRIPT,
            "Function is top-level but the type is not 'FN_SCRIPT'.");
        Array_Upvalue_free(F->upvalues, NULL);
        GC_FREE(vm, F->upvalues, sizeof(Array_Upvalue*));
    } else if(F->enclosing->fn_type == FN_SCRIPT)
        vm->script = OBJ_VAL(F->enclosing->fn->name);
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
            case TOK_WHILE:
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
    if(!cond) error(F, "Syntax error.");

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
    coderet(F);
#ifdef DEBUG_PRINT_CODE
    if(!PFLAG_CHECK(F->lexer, ERROR_BIT)) {
        OFunction* fn = F->fn;
        Chunk_debug(current_chunk(F), fn->name->storage);
    }
#endif
    return F->fn;
}

// Compile source code
OFunction* compile(VM* vm, const char* source, Value name)
{
    push(vm, name);
    HashTable_insert(vm, &vm->loaded, name, EMPTY_VAL);
    pop(vm);
    Function* F     = MALLOC(vm, sizeof(Function));
    L         lexer = L_new(source, vm);
    Scope     globalscope;
    F_init(F, NULL, vm, &lexer, FN_SCRIPT, name, vm->F);
    advance(F);
    while(!match(F, TOK_EOF))
        dec(F);
    OFunction* fn  = compile_end(F);
    bool       err = F->lexer->error;
    F_free(F);
    return (err ? NULL : fn);
}

// Create new local variable
sstatic void local_new(Function* F, Token name)
{
    if(unlikely(F->locals.len >= INDEX_MAX)) {
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
        if(Identifier_eq(name, &local->name))
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
    Array_Int_free(Array_Array_Int_last(&F->cflow.breaks), NULL);
}


sstatic force_inline UInt make_constant(Function* F, Value constant)
{
    if(unlikely(CHUNK(F)->constants.len > INDEX_MAX))
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
    if(!F->fn->isva) {
        error(
            F,
            "'...' can only be used inside functions that accept variable "
            "number of arguments.");
    }
    return CODEOP(F, OP_VALIST, 1);
}

// Adjust assign expressions in case last expression is a function call
sstatic void adjustassign(Function* F, Exp* E, Int left, Int right)
{
    Int leftover = left - right; /// Safety: left < right is a compile error
    switch(E->type) {
        case EXP_INVOKE_INDEX:
        case EXP_CALL:
            SET_RETCNT(F, E, leftover + 1);
            break;
        case EXP_INVOKE:
            SET_RETCNTL(F, E, leftover + 1);
            break;
        default:
            CODEOP(F, OP_NILN, leftover);
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
    if((F)->S->depth > 0) { // If local scope make local variable
        make_local(F, name);
        return -1;
    } // Otherwise make global variable
    return MAKE_GLOBAL(F, name);
}


ARRAY_NEW(Array_Exp, Exp);
// helper [exprstm]
sstatic void codesetall(Function* F, Array_Exp* Earr)
{
    for(Int i = 0; i < Earr->len; i++) {
        Exp* E = Array_Exp_index(Earr, i);
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
}

/// exprstm ::= functioncall
///           | varlist '=' explist
sstatic void exprstm(Function* F)
{
    Exp E;
    suffixedexp(F, &E);
    TokenType next = CURRT(F).type;
    if(next == TOK_EQUAL || next == TOK_COMMA) {
        E.ins.set = true;
        Array_Exp Earr;
        Array_Exp_init(&Earr, F->vm);
        expect_cond(F, etisvar(E.type), "Expect variable.");
        rmlastins(F, &E); // remove last 'OP_GET..' instruction
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
        Int expc = explist(F, vars, &E);
        if(vars != expc) adjustassign(F, &E, vars, expc);
        codesetall(F, &Earr);
        Array_Exp_free(&Earr, NULL);
    } else expect_cond(F, etiscall(E.type), "Syntax error.");
    expect(F, TOK_SEMICOLON, "Expect ';'.");
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

sstatic void codeassign(Function* F, Array_Int* nameidx)
{
    if(F->S->depth > 0) {
        for(Int i = 0; i < nameidx->len; i++)
            INIT_LOCAL(F, i);
        return;
    }
    for(Int i = 0; i < nameidx->len; i++) {
        Int idx = *Array_Int_index(nameidx, i);
        Exp _; // dummy
        CODEOP(F, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, &_), idx);
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
    if(match(F, TOK_EQUAL)) expc = explist(F, names, &E);
    if(names != expc) adjustassign(F, &E, names, expc);
    codeassign(F, &nameidx);
    Array_Int_free(&nameidx, NULL);
    expect(F, TOK_SEMICOLON, "Expect ';'.");
}

// fvardec ::= 'fixed' vardec
sstatic force_inline void fvardec(Function* F)
{
    FSET(F, FFIXED);
    vardec(F);
}

// Create and parse a new Function
sstatic void fn(Function* F, FunctionType type)
{
    Function* Fnew = GC_MALLOC(F->vm, sizeof(Function));
    F_init(Fnew, F->cclass, F->vm, F->lexer, type, NIL_VAL, F);
    expect(Fnew, TOK_LPAREN, "Expect '(' after function name.");
    if(!check(Fnew, TOK_RPAREN)) arglist(Fnew);
    if(F->fn->isva) expect(Fnew, TOK_RPAREN, "'...' must be last argument.");
    else expect(Fnew, TOK_RPAREN, "Expect ')' after parameters.");
    expect(Fnew, TOK_LBRACE, "Expect '{' before function body.");
    block(Fnew); // function body
    OFunction* fn = compile_end(Fnew);
    fn->isinit    = (type == FN_INIT);
    if(fn->upvalc == 0) CODEOP(F, OP_CONST, make_constant(F, OBJ_VAL(fn)));
    else {
        // Create a closure only when function captured variables
        CODEOP(F, OP_CLOSURE, make_constant(F, OBJ_VAL(fn)));
        for(UInt i = 0; i < fn->upvalc; i++) {
            Upvalue* upval = Array_Upvalue_index(Fnew->upvalues, i);
            CODE(F, upval->local ? 1 : 0);
            CODE(F, upval->flags);
            CODEL(F, upval->idx);
        }
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
    if(F->S->depth == 0) CODEOP(F, GET_OP_TYPE(idx, OP_DEFINE_GLOBAL, &_), idx);
}

sstatic void method(VM* vm, Function* F)
{
    expect(F, TOK_FN, "Expect 'fn'.");
    expect(F, TOK_IDENTIFIER, "Expect method name.");
    Value        identifier = tokintostr(vm, &PREVT(F));
    UInt         idx        = make_constant(F, identifier);
    FunctionType type       = FN_METHOD;
    if(AS_STRING(identifier) == vm->statics[SS_INIT]) type = FN_INIT;
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
    CODEOP(F, OP_CLASS, idx);
    if(F->S->depth > 0) {
        make_local(F, &class_name);
        INIT_LOCAL(F, 0);
    } else {
        Exp _; // dummy
        INIT_GLOBAL(F, MAKE_GLOBAL(F, &class_name), &_);
    }
    Class cclass;
    cclass.enclosing  = F->cclass;
    cclass.superclass = false;
    F->cclass         = &cclass;
    Scope S;
    Exp   _; // dummy
    if(match(F, TOK_IMPL)) {
        expect(F, TOK_IDENTIFIER, "Expect superclass name.");
        codevar(F, &_); // Get superclass
        if(Identifier_eq(&PREVT(F), &class_name))
            CLASS_INHERIT_ERR(F, AS_CSTRING(identifier));
        cclass.superclass = true;
        startscope(F, &S, 0, 0);
        local_new(F, syntoken("super"));
        INIT_LOCAL(F, 0);
        codevarprev(F, class_name, &_);
        CODE(F, OP_INHERIT);
    }
    codevarprev(F, class_name, &_);
    expect(F, TOK_LBRACE, "Expect '{' before class body.");
    while(!check(F, TOK_RBRACE) && !check(F, TOK_EOF))
        method(F->vm, F);
    expect(F, TOK_RBRACE, "Expect '}' after class body.");
    CODE(F, OP_POP); // Pop the class
    F->cclass = cclass.enclosing;
    if(cclass.superclass) endscope(F);
}

/// call ::= '(' ')'
///        | '(' explist ')'
sstatic void call(Function* F, Exp* E)
{
    CODE(F, OP_CALLSTART);
    if(!check(F, TOK_RPAREN)) explist(F, BYTECODE_MAX, E);
    expect(F, TOK_RPAREN, "Expect ')'.");
    if(ethasmulret(E->type)) SET_LPARAM(F, E, MULRET);
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
            for(Int i = 0; i < state->constants.len; i++) {
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
    expr(F, &E1);
    expect(F, TOK_RPAREN, "Expect ')' after condition.");
    expect(F, TOK_LBRACE, "Expect '{' after ')'.");
    sdepth               = F->cflow.innersdepth;
    F->cflow.innersdepth = F->S->depth;
    // 'switch' body
    while(!match(F, TOK_RBRACE) && !check(F, TOK_EOF)) {
        if(match(F, TOK_CASE) || match(F, TOK_DEFAULT)) {
            if(swstate.casestate != CS_NONE) {
                Array_Int_push(&fts, CODEJMP(F, OP_JMP));
                if(swstate.casestate != CS_DFLT && swstate.casestate != CS_MATCH)
                    patchjmp(F, swstate.patch);
            }
            swstate.casestate = CS_DFLT;
            if(PREVT(F).type == TOK_CASE) {
                Exp E2;
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
            if(swstate.casestate == CS_NONE) SWITCH_NOCASE_ERR(F);
            stm(F);
        }
    }
    endscope(F);
    if(PREVT(F).type == TOK_EOF) SWITCH_RBRACE_ERR(F);
    Array_Int_free(&fts, NULL);
    SwitchState_free(&swstate);
    patchbreaklist(F);
    endbreaklist(F);
    CODE(F, OP_POP); // pop switch expression
    F->cflow.innersdepth = sdepth;
    F->S->isswitch       = inswitch;
}

sstatic void ifstm(Function* F)
{
    Exp     E;
    Context C;
    Int     localc = F->locals.len; // remove after testing ?
    Int     upvalc = F->upvalues->len; // remove after testing ?
    Int     jmptoelse, jmptoend = -1;
    savecontext(F, &C);
    expect(F, TOK_LPAREN, "Expect '(' after 'if'.");
    expr(F, &E);
    expect(F, TOK_RPAREN, "Expect ')' after condition.");
    bool remove = false;
    bool istrue = false;
    if(etisconst(E.type)) {
        rmlastins(F, &E);
        if(etisfalse(E.type)) remove = true;
        else istrue = true;
    } else jmptoelse = CODEJMP(F, OP_JMP_IF_FALSE_AND_POP);
    stm(F);
    if(!remove) {
        jmptoend = CODEJMP(F, OP_JMP);
        if(!istrue) patchjmp(F, jmptoelse);
    } else {
        ASSERT(F->locals.len == localc, "localc does not match."); // @?
        ASSERT(F->upvalues->len == upvalc, "upvalc does not match."); // @?
        restorecontext(F, &C);
    }
    if(match(F, TOK_ELSE)) { // there is 'else' branch?
        stm(F);
        if(!remove) patchjmp(F, jmptoend);
    }
}

sstatic void startloop(Function* F, Scope* S, Int* lstart, Int* ldepth)
{
    *lstart                = (F)->cflow.innerlstart;
    *ldepth                = (F)->cflow.innerldepth;
    (F)->cflow.innerlstart = codeoffset(F);
    (F)->cflow.innerldepth = S->depth;
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
    Int     upvalc   = F->upvalues->len; // after testing remove?
    Int     localc   = F->locals.len; // after testing remove?
    bool    infinite, remove = false;
    savecontext(F, &C);
    startscope(F, &S, 1, 0);
    startloop(F, &S, &lstart, &ldepth);
    startbreaklist(F);
    expect(F, TOK_LPAREN, "Expect '(' after 'while'.");
    expr(F, &E); // conditional
    if(etisconst(E.type)) {
        rmlastins(F, &E);
        if(etisfalse(E.type)) remove = true;
        else infinite = true;
    } else jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
    expect(F, TOK_RPAREN, "Expect ')' after condition.");
    stm(F);
    endloop(F, lstart, ldepth);
    endscope(F);
    if(!remove) {
        codeloop(F, (F)->cflow.innerlstart);
        if(!infinite) {
            ASSERT(jmptoend != -1, "end jmp invalid but flag is false.");
            patchjmp(F, jmptoend);
        }
        patchbreaklist(F);
    } else {
        // After testing remove these assertions and those vars
        ASSERT(localc = F->locals.len, "local count does not match."); // @?
        ASSERT(upvalc = F->upvalues->len, "upvalue count does not match."); // @?
        restorecontext(F, &C);
    }
    endbreaklist(F);
}

sstatic void forstm(Function* F)
{
    Scope   S;
    Context C;
    Exp     E;
    Int     lstart, ldepth;
    Int     jmptoend = -1;
    Int     upvalc   = F->upvalues->len; // remove after testing ?
    Int     localc   = F->locals.len; // remove after testing ?
    bool    remove, infinite = false;
    savecontext(F, &C);
    startscope(F, &S, 1, 0);
    startbreaklist(F);
    expect(F, TOK_LPAREN, "Expect '(' after 'for'.");
    if(match(F, TOK_SEMICOLON))
        ; // no initializer
    else if(match(F, TOK_VAR)) vardec(F);
    else if(match(F, TOK_FIXED)) fvardec(F);
    else exprstm(F);
    startloop(F, &S, &lstart, &ldepth);
    if(!match(F, TOK_SEMICOLON)) {
        expr(F, &E);
        if(etisconst(E.type)) {
            rmlastins(F, &E);
            if(etistrue(E.type)) infinite = true;
            else remove = true;
        } else jmptoend = CODEJMP(F, OP_JMP_IF_FALSE_POP);
        expect(F, TOK_SEMICOLON, "Expect ';' after for-loop condition clause.");
    }
    if(!match(F, TOK_RPAREN)) {
        Int jmptobody = -1;
        Int jmptoincr = -1;
        if(!infinite && !remove) jmptobody = CODEJMP(F, OP_JMP);
        if(!remove) jmptoincr = codeoffset(F);
        Exp _;
        expr(F, &_);
        CODE(F, OP_POP);
        if(!infinite && !remove) {
            codeloop(F, (F)->cflow.innerlstart);
            patchjmp(F, jmptobody);
            (F)->cflow.innerlstart = jmptoincr;
        }
        expect(F, TOK_RPAREN, "Expect ')' after last for-loop clause.");
    }
    stm(F); // Loop body
    endscope(F);
    if(!remove) {
        codeloop(F, (F)->cflow.innerlstart);
        if(!infinite) patchjmp(F, jmptoend);
        patchbreaklist(F);
    } else {
        // remove these assertions after testing
        ASSERT(localc == F->locals.len, "localc does not match."); // @?
        ASSERT(upvalc == F->upvalues->len, "upvalc does not match."); // @?
        restorecontext(F, &C);
    }
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
    startbreaklist(F);
    newlocalliteral(F, "(for iterator)"); // iterator function
    newlocalliteral(F, "(for invstate)"); // invariant state
    newlocalliteral(F, "(for ctlvar)"); // control variable
    vars = foreachvars(F); // declared vars
    expect(F, TOK_IN, "Expect 'in'.");
    expr(F, &E); // iterator factory
    expect_cond(F, !etisconst(E.type), "Can't call constant expression.");
    expc = 1;
    if(match(F, TOK_COMMA)) expc += explist(F, 2, &E);
    adjustassign(F, &E, 3, expc);
    startloop(F, &S, &lstart, &ldepth);
    CODEOP(F, OP_FOREACH, vars);
    endjmp = CODEJMP(F, OP_JMP);
    stm(F);
    CODEOP(F, OP_FOREACH_END, vars);
    codeloop(F, (F)->cflow.innerlstart);
    endscope(F);
    patchbreaklist(F);
    endbreaklist(F);
    patchjmp(F, endjmp);
    endloop(F, lstart, ldepth);
}

sstatic void loopstm(Function* F)
{
    Scope S;
    Int   lstart, ldepth;
    startscope(F, &S, 1, 0);
    startbreaklist(F);
    startloop(F, &S, &lstart, &ldepth);
    stm(F);
    codeloop(F, (F)->cflow.innerlstart);
    endscope(F);
    patchbreaklist(F);
    endbreaklist(F);
    endloop(F, lstart, ldepth);
}

sstatic void continuestm(Function* F)
{
    Int popn = F->locals.len - F->S->localc;
    expect(F, TOK_SEMICOLON, "Expect ';' after 'continue'.");
    if(F->cflow.innerlstart == -1) CONTINUE_ERR(F);
    if(inswitch(F->S)) popn++;
    CODEPOP(F, popn);
    codeloop(F, F->cflow.innerlstart);
}

sstatic void breakstm(Function* F)
{
    expect(F, TOK_SEMICOLON, "Expect ';' after 'break'.");
    Array_Array_Int* arr = &F->cflow.breaks;
    Int              sdepth;
    Int              popn = 0;
    switch(inwhat(F->S)) {
        case -1: // outside
            BREAK_ERR(F);
            return;
        case 0: // in switch
            sdepth = F->cflow.innersdepth;
            popn++;
            break;
        case 1: // in loop
            sdepth = F->cflow.innerldepth;
            break;
        default:
            unreachable;
    }
    popn += F->locals.len - F->S->localc;
    CODEPOP(F, popn);
    Array_Int_push(Array_Array_Int_last(arr), CODEJMP(F, OP_JMP));
}

sstatic void returnstm(Function* F)
{
    FunctionType type = F->fn_type;
    if(match(F, TOK_SEMICOLON)) coderet(F);
    else {
        if(type == FN_INIT) RETURN_INIT_ERR(F, static_str[SS_INIT].name);
        CODE(F, OP_RETSTART);
        Exp E;
        explist(F, BYTECODE_MAX, &E);
        expect(F, TOK_SEMICOLON, "Expect ';' after return value/s.");
        if(ethasmulret(E.type)) SET_RETCNT(F, &E, MULRET);
        if(type == FN_SCRIPT) CODE(F, OP_TOPRET);
        else CODE(F, OP_RET);
    }
}


/// dot ::= '.' name
///       | '.' name call
sstatic void dot(Function* F, Exp* E)
{
    expect(F, TOK_IDENTIFIER, "Expect property name after '.'.");
    Value identifier = tokintostr(F->vm, &PREVT(F));
    UInt  idx        = make_constant(F, identifier);
    if(match(F, TOK_LPAREN)) {
        codeinvoke(F, E, idx);
    } else {
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
    else exprstm(F);
}

// indexed ::= '[' expr ']'
//           | '[' expr ']' call
sstatic force_inline void indexed(Function* F, Exp* E)
{
    Exp E2;
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
    Exp _;
    codevar(F, &_);
    E->type = EXP_SELF;
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
    expect(F, TOK_DOT, "Expect '.' after 'super'.");
    expect(F, TOK_IDENTIFIER, "Expect superclass method name after '.'.");
    Value id  = tokintostr(F->vm, &PREVT(F));
    Int   idx = make_constant(F, id);
    Exp   _; // dummy
    codevarprev(F, syntoken("self"), &_);
    if(match(F, TOK_LPAREN)) {
        call(F, E);
        codevarprev(F, syntoken("super"), &_);
        E->ins.code = CODEOP(F, OP_INVOKE_SUPER, idx);
        CODEL(F, 1);
        E->type = EXP_INVOKE;
    } else {
        codevarprev(F, syntoken("super"), &_);
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
            codevar(F, E);
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
    UInt constidx;
    switch(CURRT(F).type) {
        case TOK_NUMBER:
            constidx = make_constant(F, PREVT(F).value);
            Exp_init(E, EXP_NUMBER, constidx, CODEOP(F, OP_CONST, constidx));
            break;
        case TOK_STRING:
            constidx = make_constant(F, PREVT(F).value);
            Exp_init(E, EXP_STRING, constidx, CODEOP(F, OP_CONST, constidx));
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

// Intermediate step that tries to optimize 'and' and 'or'
// instructions before the second expression gets parsed.
sstatic void infix(Function* F, BinaryOpr opr, Exp* E)
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
        advance(F); // skip binary operator
        infix(F, binop, E1);
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
