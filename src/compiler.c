#include "array.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "err.h"
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
#define GET_OP_TYPE(idx, op) (idx <= UINT8_MAX) ? op : op##L

// Return current chunk code offset
#define code_offset(C) (current_chunk(C)->code.len)




// Bit mask to extract loop and switch flags from parser.
// Used primarily to save the old flags before continuing to compile
// 'for', 'while' and 'switch' statements.
#define CFLOW_MASK(C) ((uint64_t)(btoul(SWITCH_BIT) | btoul(LOOP_BIT)) & PFLAGS(C))




/* Global compiler state, might remove or expand on upon it.
 * For now it only track total count of local variables. */
typedef struct {
    UInt loc_len; // Total number of local variables across functions.
} GState;

GState gstate = {0};




// Local variable flags
#define VFIXED_BIT (1) // Variable is fixed (immutable)
// 2-7 bits are unused
#define VCAPTURED_BIT (8) // Variable is captured by closure
// Local variable flag setters and getters
#define LFLAG_SET(var, bit)   BIT_SET((var)->flags, bit)
#define LFLAG_CHECK(var, bit) BIT_CHECK((var)->flags, bit)
#define LFLAGS(var)           ((var)->flags)

// Local variable
typedef struct {
    Token name;  // Variable name
    Int   depth; // Scope depth where variable is defined
                 // can be -1 if the variable is not initialized
    Byte flags;  // Bits that each represent some property of this
                 // variable (check above the V[FLAGNAME]_BIT's)
} Local;

/* Max 'Compiler' stack size */
#define LOCAL_STACK_MAX (UINT24_MAX)

/* Default 'Compiler' stack size */
#define SHORT_STACK_SIZE (UINT8_MAX)




/* Array of Int(s) */
ARRAY_NEW(Array_Int, Int);
/* Two dimensional array */
ARRAY_NEW(Array_Array_Int, Array_Int)

/* Control Flow context */
typedef struct {
    /* We store break offsets inside an array because we don't
     * know in advance where the end of the switch/loop statement
     * is while parsing.
     * When we hit the end of the loop/switch statement then
     * we can just patch each parsed break statement.
     * This would be the same for 'continue' in case of 'do while'
     * loop.*/
    Array_Array_Int breaks; /* Break statement offsets */

    Int innermostl_start;  /* Innermost loop start offset */
    Int innermostl_depth;  /* Innermost loop scope depth */
    Int innermostsw_depth; /* Innermost switch scope depth */
} CFCtx;




/**
 * UpValue is a Value that is declared/defined inside of enclosing function.
 * Contains stack index of that variable inside of that enclosing function.
 * The 'local' field indicates if this is the local variable of the innermost
 * enclosing function or the UpValue.
 **/
typedef struct {
    UInt idx;   // Stack index
    bool local; // If upvalue is local to enclosing function
} Upvalue;

// Array of 'Upvalues'
ARRAY_NEW(Array_Upvalue, Upvalue);




// Type of function the compiler is compiling
typedef enum {
    FN_FUNCTION, // Normal function declaration
    FN_METHOD,   // Class method function
    FN_INIT,     // Class initializer function
    FN_SCRIPT,   // Top-level code (implicit function)
} FunctionType;




// In case compiler is compiling a class declaration,
// then it will contain at least a single 'Class' struct,
// this serves as additional information when we are compiling
// a function declaration inside of a class 'method' function declaration
// to know if we are allowed to use 'self' and/or 'super' keywords to
// reference the currently compiled class if any and/or superclass if
// class has a superclass.
typedef struct Class {
    struct Class* enclosing;
    bool          superclass;
} Class;




struct Compiler {
    /* Enclosing compiler */
    Compiler* enclosing;

    /* Currently compiled class */
    Class* cclass;

    /* Grammar parser */
    Parser* parser;

    /* Control flow context, tracks offsets and
     * scope depth for 'continue' and 'break' statements. */
    CFCtx context;

    /* Currently compiled function, meaning we write
     * bytecode to the function chunk in this field. */
    ObjFunction* fn;
    FunctionType fn_type; /* Function type */

    /* Holds UpValues */
    Array_Upvalue* upvalues;

    /* Current scope depth. */
    Int depth;

    /* Count of local variables in this function.
     * In case of this function being FN_SCRIPT that would
     * mean count of variables so far in this scope block (scope > 0) */
    UInt loc_len;

    /* Capacity of 'locals' flexible array. */
    UInt loc_cap;

    /* Array that holds local variables 'Local's.
     * It is a flexible array this way we avoid pointer
     * dereference. */
    Local locals[];
};

/* Allocates 'Compiler' with the default stack size */
#define ALLOC_COMPILER(vm) MALLOC(vm, sizeof(Compiler) + (SHORT_STACK_SIZE * sizeof(Local)))

// Grows 'Compiler', compiler uses flexible array to store Local's
// and this is why the whole 'Compiler' needs to resize.
#define GROW_COMPILER(ptr, oldcap, newcap)                                                         \
    (Compiler*)REALLOC(vm, ptr, sizeof(Compiler) + (newcap * sizeof(Local)))

/* Pointer to pointer to 'Compiler' */
typedef Compiler** PPC;
// Shorthand for dereferencing 'PPC'
#define C() (*ppc)




/* Precedence from LOW-est to HIGH-est (Pratt parsing) */
typedef enum {
    PREC_NONE = 0,
    PREC_ASSIGNMENT,
    PREC_TERNARY,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

/* ParseFn - generic parse function signature. */
typedef void (*ParseFn)(VM*, PPC);

// Used for Pratt parsing algorithm
typedef struct {
    ParseFn    prefix;
    ParseFn    infix;
    Precedence precedence;
} ParseRule;




/* Internal */
SK_INTERNAL(Chunk*) current_chunk(Compiler* C);
SK_INTERNAL(void) C_advance(Compiler* compiler);
SK_INTERNAL(void) C_error(Compiler* compiler, const char* error, ...);
SK_INTERNAL(void) C_free(VM* vm, Compiler* C);
SK_INTERNAL(void) compile_number(VM* vm, PPC ppc);
SK_INTERNAL(void) compile_string(VM* vm, PPC ppc);
SK_INTERNAL(UInt)
parse_varname(VM* vm, PPC ppc, const char* errmsg);
SK_INTERNAL(void) parse_precedence(VM* vm, PPC ppc, Precedence prec);
SK_INTERNAL(void) parse_grouping(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_ternarycond(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_expr(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_dec(VM* vm, PPC ppc);
SK_INTERNAL(void)
parse_dec_var(VM* vm, PPC ppc);
SK_INTERNAL(void)
parse_dec_var_fixed(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_dec_fn(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_stm_block(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_stm(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_variable(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_binary(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_unary(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_literal(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_and(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_or(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_call(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_dot(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_index(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_self(VM* vm, PPC ppc);
SK_INTERNAL(void) parse_super(VM* vm, PPC ppc);




/* Checks for equality between the 'token_type' and the current parser token */
#define C_check(compiler, token_type) ((compiler)->parser->current.type == token_type)

SK_INTERNAL(void) CFCtx_init(CFCtx* context)
{
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
    Array_Array_Int_init(&context->breaks);
}

/* Have to pass VM while initializing 'Compiler' because we are allocating objects. */
SK_INTERNAL(void)
C_init(Compiler* C, Class* cclass, VM* vm, FunctionType fn_type, Compiler* enclosing)
{
    C->enclosing = enclosing;
    C->cclass    = cclass;

    C->fn      = NULL; // Initialize to NULL so gc does not get confused
    C->fn      = ObjFunction_new(vm);
    C->fn_type = fn_type;

    CFCtx_init(&C->context);

    // Initialize Upvalue array only once when initializing outermost compiler.
    // This array then gets passed on to other nested compilers for each
    // function.
    if(enclosing == NULL) {
        C->upvalues = MALLOC(vm, sizeof(Array_Upvalue));
        Array_Upvalue_init(C->upvalues);
    } else {
        C->parser   = enclosing->parser;
        C->upvalues = enclosing->upvalues;
    }

    C->loc_len = 0;
    C->loc_cap = SHORT_STACK_SIZE;
    C->depth   = 0;

    /* Reserve first stack slot for VM ('self' ObjInstance) */
    Local* local = &C->locals[C->loc_len++];
    gstate.loc_len++;
    local->depth = 0;
    local->flags = 0;

    if(fn_type != FN_FUNCTION) {
        local->name.start = "self";
        local->name.len   = 4;
    } else {
        local->name.start = "";
        local->name.len   = 0;
    }

    if(fn_type != FN_SCRIPT) {
        C->fn->name = ObjString_from(vm, C->parser->previous.start, C->parser->previous.len);
    }
}

SK_INTERNAL(force_inline void) C_grow_stack(VM* vm, PPC ppc)
{
    Compiler* C      = C();
    UInt      oldcap = C->loc_cap;

    C->loc_cap = MIN(GROW_ARRAY_CAPACITY(oldcap, SHORT_STACK_SIZE), VM_STACK_MAX);
    C          = GROW_COMPILER(vm, C, C->loc_cap);
    C()        = C;
}

void mark_c_roots(VM* vm)
{
    // Mark token values
    mark_value(vm, vm->compiler->parser->previous.value);
    mark_value(vm, vm->compiler->parser->current.value);

    // Mark all currently compiled functions
    for(Compiler* current = vm->compiler; current != NULL; current = current->enclosing) {
        mark_obj(vm, (Obj*)current->fn);
    }
}

/*========================== EMIT =========================*/

SK_INTERNAL(force_inline UInt) C_make_const(Compiler* C, VM* vm, Value constant)
{
    if(unlikely(current_chunk(C)->clen > MIN(VM_STACK_MAX, UINT24_MAX))) {
        COMPILER_CONSTANT_LIMIT_ERR(C, C->fn->name->storage, MIN(VM_STACK_MAX, UINT24_MAX));
    }

    return Chunk_make_constant(vm, current_chunk(C), constant);
}

SK_INTERNAL(force_inline Value)
Token_into_stringval(VM* vm, const Token* name)
{
    return OBJ_VAL(ObjString_from(vm, name->start, name->len));
}

SK_INTERNAL(force_inline UInt)
C_make_global_identifier(Compiler* C, VM* vm, Value identifier, Byte flags)
{
    Value  index;
    Global glob = {DECLARED_VAL, flags};

    if(!HashTable_get(&vm->globids, identifier, &index)) {
        VM_push(vm, identifier);
        if(unlikely((vm)->globlen + 1 > UINT24_MAX)) {
            COMPILER_GLOBALS_LIMIT_ERR(C, UINT24_MAX);
        }
        index = NUMBER_VAL(GARRAY_PUSH(vm, glob));             // GC
        HashTable_insert(vm, &vm->globids, identifier, index); // GC
        VM_pop(vm);
        return (UInt)AS_NUMBER(index);
    }

    UInt i = (UInt)AS_NUMBER(index);

    if(IS_DECLARED(vm->globvals[i].value)) {
        COMPILER_GLOBAL_REDEFINITION_ERR(C, AS_STRING(identifier)->len, AS_CSTRING(identifier));
    } else {
        vm->globvals[i] = glob;
    }

    return i;
}

// Emit instruction
#define EMIT_OP(c, code, param)                                                                    \
    Chunk_write_codewparam(current_chunk(c), code, param, (c)->parser->previous.line)

// Emit bytecode
#define EMIT_BYTE(c, byte) Chunk_write(current_chunk(c), byte, (c)->parser->previous.line)

// Emit jump instruction
#define EMIT_JMP(c, jmp)                                                                           \
    ({                                                                                             \
        EMIT_OP(c, jmp, 0);                                                                        \
        code_offset(c) - 3;                                                                        \
    })

// Emit 3 bytes
#define EMIT_LBYTE(c, bytes)                                                                       \
    do {                                                                                           \
        EMIT_BYTE(c, BYTE(bytes, 0));                                                              \
        EMIT_BYTE(c, BYTE(bytes, 1));                                                              \
        EMIT_BYTE(c, BYTE(bytes, 2));                                                              \
    } while(false)

SK_INTERNAL(force_inline void) C_emit_return(Compiler* C)
{
    if(PFLAG_CHECK(C->parser, RET_BIT)) {
        return;
    }

    if(C->fn_type == FN_INIT) {
        EMIT_OP(C, OP_GET_LOCAL, 0);
    } else {
        EMIT_BYTE(C, OP_NIL);
    }

    EMIT_BYTE(C, OP_RET);
}

SK_INTERNAL(force_inline void) C_emit_loop(Compiler* C, UInt start)
{
    EMIT_BYTE(C, OP_LOOP);

    UInt offset = current_chunk(C)->code.len - start + 3;

    if(offset >= UINT24_MAX) {
        COMPILER_JUMP_LIMIT_ERR(C, UINT24_MAX);
    }

    EMIT_LBYTE(C, offset);
}

static void C_error(Compiler* C, const char* error, ...)
{
    Token*  token = &C->parser->previous;
    va_list args;

    // If panic bit is on, then sync the parser before printing any new errors.
    // If token type is TOK_ERROR then just return because parser already printed it.
    if(PFLAG_CHECK(C->parser, PANIC_BIT) || token->type == TOK_ERROR) {
        return;
    }

    va_start(args, error);
    print_error(C->parser, error, args);
    va_end(args);

    PFLAG_SET(C->parser, PANIC_BIT);
    PFLAG_SET(C->parser, ERROR_BIT);
}

SK_INTERNAL(void) C_advance(Compiler* C)
{
    C->parser->previous = C->parser->current;

    while(true) {
        C->parser->current = Parser_next(C->parser);
        if(C->parser->current.type != TOK_ERROR) {
            break;
        }

        C_error(C, C->parser->current.start);
    }
}

static void Parser_sync(Compiler* C)
{
    PFLAG_CLEAR(C->parser, PANIC_BIT);

    while(C->parser->current.type != TOK_EOF) {
        if(C->parser->previous.type == TOK_SEMICOLON) {
            return;
        }

        switch(C->parser->current.type) {
            case TOK_FOR:
            case TOK_FN:
            case TOK_VAR:
            case TOK_CLASS:
            case TOK_IF:
            case TOK_RETURN:
            case TOK_WHILE:
                return;
            default:
                C_advance(C);
                break;
        }
    }
}

SK_INTERNAL(force_inline void)
C_expect(Compiler* compiler, TokenType type, const char* error)
{
    if(C_check(compiler, type)) {
        C_advance(compiler);
        return;
    }
    C_error(compiler, error);
}

SK_INTERNAL(force_inline bool) C_match(Compiler* compiler, TokenType type)
{
    if(!C_check(compiler, type)) {
        return false;
    }
    C_advance(compiler);
    return true;
}

SK_INTERNAL(force_inline ObjFunction*) compile_end(Compiler* C)
{
    C_emit_return(C);

#ifdef DEBUG_PRINT_CODE
    if(!PFLAG_CHECK(C->parser, ERROR_BIT)) {
        ObjFunction* fn = C->fn;
        Chunk_debug(current_chunk(C), (fn->name) ? fn->name->storage : "<script>");
    }
#endif

    // Not really needed, we discard the compiler
    // anyway after compiling the function.
    PFLAG_CLEAR(C->parser, RET_BIT);
    return C->fn;
}

ObjFunction* compile(VM* vm, const char* source)
{
    Compiler* C   = ALLOC_COMPILER(vm);
    vm->compiler  = C;
    Parser parser = Parser_new(source, vm);
    C_init(C, NULL, vm, FN_SCRIPT, NULL);
    C->parser = &parser;

    C_advance(C);
    while(!C_match(C, TOK_EOF)) {
        parse_dec(vm, &C);
    }

    ObjFunction* fn  = compile_end(C);
    bool         err = PFLAG_CHECK(C->parser, ERROR_BIT);
    C_free(vm, C);
    return (err ? NULL : fn);
}

SK_INTERNAL(force_inline Chunk*) current_chunk(Compiler* C)
{
    return &C->fn->chunk;
}

SK_INTERNAL(void) CFCtx_free(CFCtx* context)
{
    Array_Array_Int_free(&context->breaks, NULL);
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
}

SK_INTERNAL(void) C_free(VM* vm, Compiler* C)
{
    CFCtx_free(&C->context);
    C->parser->vm->compiler = C->enclosing;
    if(C->enclosing == NULL) {
        ASSERT(C->fn_type == FN_SCRIPT, "Compiler is top-level but the type is not FN_SCRIPT.");
        Array_Upvalue_free(C->upvalues, NULL);
        FREE(vm, C->upvalues);
    }
    FREE(vm, C);
}

/*========================== PARSE ========================
 * PP* (Pratt Parsing algorithm)
 *
 * Parsing rules table,
 * First and second column are function pointers to 'ParseFn',
 * these functions are responsible for parsing the actual expression and most
 * are recursive. First column parse function is used in case token is prefix,
 * while second column parse function is used in case token is inifx. Third
 * column marks the 'Precedence' of the token inside expression. */
static const ParseRule rules[] = {
    [TOK_LBRACK]        = {NULL,                parse_index,       PREC_CALL      },
    [TOK_LPAREN]        = {parse_grouping,      parse_call,        PREC_CALL      },
    [TOK_RPAREN]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_LBRACE]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_RBRACE]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_COMMA]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_DOT]           = {NULL,                parse_dot,         PREC_CALL      },
    [TOK_MINUS]         = {parse_unary,         parse_binary,      PREC_TERM      },
    [TOK_PLUS]          = {NULL,                parse_binary,      PREC_TERM      },
    [TOK_COLON]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_SEMICOLON]     = {NULL,                NULL,              PREC_NONE      },
    [TOK_SLASH]         = {NULL,                parse_binary,      PREC_FACTOR    },
    [TOK_STAR]          = {NULL,                parse_binary,      PREC_FACTOR    },
    [TOK_QMARK]         = {NULL,                parse_ternarycond, PREC_TERNARY   },
    [TOK_BANG]          = {parse_unary,         NULL,              PREC_NONE      },
    [TOK_BANG_EQUAL]    = {NULL,                parse_binary,      PREC_EQUALITY  },
    [TOK_EQUAL]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_EQUAL_EQUAL]   = {NULL,                parse_binary,      PREC_EQUALITY  },
    [TOK_GREATER]       = {NULL,                parse_binary,      PREC_COMPARISON},
    [TOK_GREATER_EQUAL] = {NULL,                parse_binary,      PREC_COMPARISON},
    [TOK_LESS]          = {NULL,                parse_binary,      PREC_COMPARISON},
    [TOK_LESS_EQUAL]    = {NULL,                parse_binary,      PREC_COMPARISON},
    [TOK_IDENTIFIER]    = {parse_variable,      NULL,              PREC_NONE      },
    [TOK_STRING]        = {compile_string,      NULL,              PREC_NONE      },
    [TOK_NUMBER]        = {compile_number,      NULL,              PREC_NONE      },
    [TOK_AND]           = {NULL,                parse_and,         PREC_AND       },
    [TOK_CLASS]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_ELSE]          = {NULL,                NULL,              PREC_NONE      },
    [TOK_FALSE]         = {parse_literal,       NULL,              PREC_NONE      },
    [TOK_FOR]           = {NULL,                NULL,              PREC_NONE      },
    [TOK_FN]            = {NULL,                NULL,              PREC_NONE      },
    [TOK_FIXED]         = {parse_dec_var_fixed, NULL,              PREC_NONE      },
    [TOK_IF]            = {NULL,                NULL,              PREC_NONE      },
    [TOK_NIL]           = {parse_literal,       NULL,              PREC_NONE      },
    [TOK_OR]            = {NULL,                parse_or,          PREC_OR        },
    [TOK_RETURN]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_SUPER]         = {parse_super,         NULL,              PREC_NONE      },
    [TOK_SELF]          = {parse_self,          NULL,              PREC_NONE      },
    [TOK_TRUE]          = {parse_literal,       NULL,              PREC_NONE      },
    [TOK_VAR]           = {parse_dec_var,       NULL,              PREC_NONE      },
    [TOK_WHILE]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_ERROR]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_EOF]           = {NULL,                NULL,              PREC_NONE      },
};

SK_INTERNAL(void) C_new_local(VM* vm, PPC ppc, Token name)
{
    Compiler* C = C();

    if(unlikely(C->loc_len >= C->loc_cap)) {
        if(unlikely(gstate.loc_len >= MIN(VM_STACK_MAX, LOCAL_STACK_MAX))) {
            COMPILER_LOCAL_LIMIT_ERR(C, MIN(VM_STACK_MAX, LOCAL_STACK_MAX));
            return;
        }
        C_grow_stack(vm, ppc);
        C = C();
    }

    Local* local = &C->locals[C->loc_len++];
    gstate.loc_len++;
    local->name  = name;
    local->flags = (((Byte)(PFLAGS(C->parser) >> 8)) & 0xff);
    local->depth = -1;
}

SK_INTERNAL(force_inline bool) Identifier_eq(Token* left, Token* right)
{
    return (left->len == right->len) && (memcmp(left->start, right->start, left->len) == 0);
}

SK_INTERNAL(force_inline Int) C_get_local(Compiler* C, Token* name)
{
    for(Int i = C->loc_len - 1; i >= 0; i--) {
        Local* local = &C->locals[i];
        if(Identifier_eq(name, &local->name)) {
            if(local->depth == -1) {
                COMPILER_LOCAL_DEFINITION_ERR(C, name->len, name->start);
            }
            return i;
        }
    }

    return -1;
}

SK_INTERNAL(void) C_make_local(VM* vm, PPC ppc)
{
    Compiler* C = C();

    Token* name = &C->parser->previous;

    for(Int i = C->loc_len - 1; i >= 0; i++) {
        Local* local = &C->locals[i];

        if(local->depth != -1 && local->depth < C->depth) {
            break;
        }

        if(Identifier_eq(name, &local->name)) {
            COMPILER_LOCAL_REDEFINITION_ERR(C(), name->len, name->start);
        }
    }

    C_new_local(vm, ppc, *name);
}

SK_INTERNAL(Int) C_make_global(Compiler* C, VM* vm, Byte flags)
{
    Value identifier = Token_into_stringval(vm, &C->parser->previous);
    return C_make_global_identifier(C, vm, identifier, flags);
}

SK_INTERNAL(force_inline Int)
make_undefined_global(VM* vm, Compiler* C, Value identifier)
{
    VM_push(vm, identifier);
    Global glob = {UNDEFINED_VAL, false};
    if(unlikely((vm)->globlen + 1 > UINT24_MAX)) {
        COMPILER_GLOBALS_LIMIT_ERR(C, UINT24_MAX);
    }
    Value idx = NUMBER_VAL(GARRAY_PUSH(vm, glob));       // GC
    HashTable_insert(vm, &vm->globids, identifier, idx); // GC
    VM_pop(vm);
    return (Int)AS_NUMBER(idx);
}

// @FIX: repl
SK_INTERNAL(UInt) C_get_global(Compiler* C, VM* vm, Token* name)
{
    Value idx;
    Value identifier = Token_into_stringval(vm, name);

    if(!HashTable_get(&vm->globids, identifier, &idx)) {
        if(C->fn_type != FN_SCRIPT) {
            // Create reference to a global but don't declare or define it
            idx = NUMBER_VAL(make_undefined_global(vm, C, identifier));
        } else {
            // If we are in global scope and there is no global identifier,
            COMPILER_VAR_UNDEFINED_ERR(C, AS_CSTRING(identifier));
            return 0;
        }
    } else {
        UInt i = (UInt)AS_NUMBER(idx);
        // If we are in global scope and there is a global identifier
        // but its value is of type undefined, that means we didn't omit
        // OP_DEFINE_GLOBAL(L) instruction for it.
        if(C->fn_type == FN_SCRIPT && IS_UNDEFINED(vm->globvals[i].value)) {
            COMPILER_VAR_UNDEFINED_ERR(C, AS_CSTRING(identifier));
            return 0;
        }
    }

    return (UInt)AS_NUMBER(idx);
}


SK_INTERNAL(Int) parse_arglist(VM* vm, PPC ppc)
{
    Int argc = 0;

    if(!C_check(C(), TOK_RPAREN)) {
        do {
            parse_expr(vm, ppc);
            if(argc == UINT24_MAX) {
                COMPILER_ARGC_LIMIT_ERR(C(), UINT24_MAX);
            }
            argc++;
        } while(C_match(C(), TOK_COMMA));
    }

    C_expect(C(), TOK_RPAREN, "Expect ')' after function arguments.");
    return argc;
}

SK_INTERNAL(void) parse_call(VM* vm, PPC ppc)
{
    Int argc = parse_arglist(vm, ppc);
    EMIT_OP(C(), GET_OP_TYPE(argc, OP_CALL), argc);
}

SK_INTERNAL(void)
parse_stm_expr(VM* vm, PPC ppc)
{
    parse_expr(vm, ppc);
    C_expect(C(), TOK_SEMICOLON, "Expect ';' after expression.");
    EMIT_BYTE(C(), OP_POP);
}

SK_INTERNAL(void) parse_expr(VM* vm, PPC ppc)
{
    parse_precedence(vm, ppc, PREC_ASSIGNMENT);
}

SK_INTERNAL(void) parse_precedence(VM* vm, PPC ppc, Precedence prec)
{
    // Save old assign bit
    bool assign = PFLAG_CHECK(C()->parser, ASSIGN_BIT);

    C_advance(C());
    ParseFn prefix_fn = rules[C()->parser->previous.type].prefix;
    if(prefix_fn == NULL) {
        COMPILER_EXPECT_EXPRESSION_ERR(C());
        return;
    }

    if(prec <= PREC_ASSIGNMENT) {
        PFLAG_SET(C()->parser, ASSIGN_BIT);
    } else {
        PFLAG_CLEAR(C()->parser, ASSIGN_BIT);
    }

    prefix_fn(vm, ppc);

    while(prec <= rules[C()->parser->current.type].precedence) {
        C_advance(C());
        ParseFn infix_fn = rules[C()->parser->previous.type].infix;
        infix_fn(vm, ppc);
    }

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        COMPILER_INVALID_ASSIGN_ERR(C());
    }

    // Restore assign bit
    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, assign);
}

SK_INTERNAL(void)
parse_dec_var_fixed(VM* vm, PPC ppc)
{
    PFLAG_SET(C()->parser, FIXED_BIT);
    C_expect(C(), TOK_VAR, "Expect 'var' after 'fixed' in variable declaration.");
    parse_dec_var(vm, ppc);
}

SK_INTERNAL(force_inline void) C_initialize_local(Compiler* C)
{
    C->locals[C->loc_len - 1].depth = C->depth;
}

SK_INTERNAL(force_inline void) C_scope_start(Compiler* C)
{
    if(unlikely((UInt)C->depth >= UINT32_MAX - 1)) {
        // @?: Maybe UINT24_MAX instead?
        COMPILER_SCOPE_LIMIT_ERR(C, UINT32_MAX);
    }
    C->depth++;
}

// End scope and pop locals and/or close captured locals
SK_INTERNAL(force_inline void) C_scope_end(Compiler* C)
{
#define LOCAL_IS_CAPTURED(local) (LFLAG_CHECK((local), VCAPTURED_BIT))

    C->depth--;

    Int pop = 0;
    while(C->loc_len > 0 && C->locals[C->loc_len - 1].depth > C->depth) {

        if(LOCAL_IS_CAPTURED(&C->locals[C->loc_len - 1])) {
            Int capture = 1;
            C->loc_len--;
            gstate.loc_len--;

            if(pop == 1) {
                EMIT_BYTE(C, OP_POP);
            } else if(pop > 1) {
                EMIT_OP(C, OP_POPN, pop);
            }

            pop = 0; // Reset pop count

            do {
                if(!LOCAL_IS_CAPTURED(&C->locals[C->loc_len - 1])) {

                    if(capture == 1) {
                        EMIT_BYTE(C, OP_CLOSE_UPVAL);
                    } else {
                        EMIT_OP(C, OP_CLOSE_UPVALN, capture);
                    }

                    break;
                }

                capture++;
                C->loc_len--;
                gstate.loc_len--;
            } while(C->loc_len > 0 && C->locals[C->loc_len - 1].depth > C->depth);

        } else {
            pop++;
            C->loc_len--;
            gstate.loc_len--;
        }
    }

    if(pop == 1) {
        EMIT_BYTE(C, OP_POP);
    } else if(pop > 1) {
        EMIT_OP(C, OP_POPN, pop);
    }

#undef LOCAL_IS_CAPTURED
}

SK_INTERNAL(void)
parse_fn(VM* vm, PPC ppc, FunctionType type)
{
#define C_new() (*ppc_new)

    Compiler* C_new = ALLOC_COMPILER(vm);
    vm->compiler    = C_new;
    C_init(C_new, C()->cclass, vm, type, C());

    uint64_t mask = PFLAGS(C()->parser);
    PFLAG_RESET(C_new->parser);
    PFLAG_TOGGLE(C_new->parser, ERROR_BIT, mask & btoul(ERROR_BIT));

    PPC ppc_new = &C_new;

    C_scope_start(C_new()); // No need to end this scope (hint CallFrame)
    C_expect(C_new(), TOK_LPAREN, "Expect '(' after function name.");

    // Parse function arguments
    if(!C_check(C_new(), TOK_RPAREN)) {
        do {
            C_new()->fn->arity++;
            parse_varname(vm, ppc_new, "Expect parameter name.");
            C_initialize_local(C_new());
        } while(C_match(C_new(), TOK_COMMA));
    }

    C_expect(C_new(), TOK_RPAREN, "Expect ')' after parameters.");
    C_expect(C_new(), TOK_LBRACE, "Expect '{' before function body.");
    parse_stm_block(vm, ppc_new);

    ObjFunction* fn = compile_end(C_new());
    vm->compiler    = C();

    // Restore flags but additionally transfer error flag
    // if error happened in the new compiler.
    mask |= PFLAG_CHECK(C_new()->parser, ERROR_BIT) ? btoul(ERROR_BIT) : 0;
    //
    C_new()->parser->flags &= mask;

    if(fn->upvalc == 0) {
        EMIT_OP(C(), OP_CONST, C_make_const(C(), vm, OBJ_VAL(fn)));
    } else {
        // Create a closure only when we have upvalues
        EMIT_OP(C(), OP_CLOSURE, C_make_const(C(), vm, OBJ_VAL(fn)));
        for(UInt i = 0; i < fn->upvalc; i++) {
            Upvalue* upval = Array_Upvalue_index(C_new()->upvalues, i);
            EMIT_BYTE(C(), upval->local ? 1 : 0);
            EMIT_LBYTE(C(), upval->idx);
        }
    }

    C_free(vm, C_new);

#undef C_new
}

SK_INTERNAL(void) parse_dec_fn(VM* vm, PPC ppc)
{
    UInt idx = parse_varname(vm, ppc, "Expect function name.");

    if(C()->depth > 0) {
        /* Initialize the variable that holds the function, in order to
         * allow usage of the function inside of its body. */
        C_initialize_local(C());
    }

    parse_fn(vm, ppc, FN_FUNCTION);

    if(C()->depth == 0) {
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_DEFINE_GLOBAL), idx);
    }
}

SK_INTERNAL(force_inline UInt) C_add_upval(Compiler* C, UInt idx, bool local)
{
    for(UInt i = 0; i < C->upvalues->len; i++) {
        Upvalue* upvalue = Array_Upvalue_index(C->upvalues, i);
        if(upvalue->idx == idx && upvalue->local == local) {
            // Return existing UpValue index
            return i;
        }
    }

    if(unlikely(C->upvalues->len == MIN(VM_STACK_MAX, UINT24_MAX))) {
        COMPILER_UPVALUE_LIMIT_ERR(C, MIN(VM_STACK_MAX, UINT24_MAX), C->fn->name->storage);
        return 0;
    }

    // Otherwise add the UpValue into the array
    C->fn->upvalc++;
    Upvalue upval = {idx, local};
    return Array_Upvalue_push(C->upvalues, upval);
}


SK_INTERNAL(Int) C_get_upval(Compiler* C, Token* name)
{
    if(C->enclosing == NULL) {
        return -1;
    }

    Int idx = C_get_local(C->enclosing, name);
    if(idx != -1) {
        // Local is captured by Upvalue
        LFLAG_SET(&C->enclosing->locals[idx], VCAPTURED_BIT);
        return C_add_upval(C, (UInt)idx, true);
    }

    idx = C_get_upval(C->enclosing, name);
    if(idx != -1) {
        return C_add_upval(C, (UInt)idx, false);
    }

    return -1;
}


SK_INTERNAL(force_inline void) named_variable(VM* vm, PPC ppc, Token name)
{
    OpCode  setop, getop;
    Int     idx   = C_get_local(C(), &name);
    int16_t flags = -1;

    if(idx != -1) {
        Local* var = &(C())->locals[idx];
        flags      = LFLAGS(var);
        if(idx > SHORT_STACK_SIZE) {
            setop = OP_SET_LOCALL;
            getop = OP_GET_LOCALL;
        } else {
            setop = OP_SET_LOCAL;
            getop = OP_GET_LOCAL;
        }
    } else if((idx = C_get_upval(C(), &name)) != -1) {
        Upvalue* upval = Array_Upvalue_index(C()->upvalues, idx);
        Local*   var   = &C()->locals[upval->idx];
        flags          = LFLAGS(var);
        setop          = OP_SET_UPVALUE;
        getop          = OP_GET_UPVALUE;
    } else {
        idx          = C_get_global(C(), vm, &name);
        Global* glob = &vm->globvals[idx];
        flags        = GLOB_FLAGS(glob);
        if(idx > SHORT_STACK_SIZE) {
            setop = OP_SET_GLOBALL;
            getop = OP_GET_GLOBALL;
        } else {
            setop = OP_SET_GLOBAL;
            getop = OP_GET_GLOBAL;
        }
    }

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        if(BIT_CHECK(flags, VFIXED_BIT)) { /* Statical check for mutability */
            COMPILER_VAR_FIXED_ERR(C(), (Int)name.len, name.start);
        }
        parse_expr(vm, ppc);
        EMIT_OP(C(), setop, idx);
    } else {
        EMIT_OP(C(), getop, idx);
    }
}

SK_INTERNAL(void) parse_method(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_FN, "Expect 'fn'.");
    C_expect(C(), TOK_IDENTIFIER, "Expect method name.");
    Value identifier = Token_into_stringval(vm, &C()->parser->previous);
    UInt  idx        = C_make_const(C(), vm, identifier);

    FunctionType type = FN_METHOD;

    if(AS_STRING(identifier) == vm->statics[SS_INIT]) {
        type = FN_INIT;
    }

    parse_fn(vm, ppc, type);

    if(type == FN_INIT) {
        EMIT_OP(C(), OP_OVERLOAD, SS_INIT);
    }

    EMIT_OP(C(), GET_OP_TYPE(idx, OP_METHOD), idx);
}


SK_INTERNAL(void) parse_dec_class(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_IDENTIFIER, "Expect class name.");

    Token class_name = C()->parser->previous;
    Value identifier = Token_into_stringval(vm, &class_name); // GC

    // Idx for OP_CLASS instruction
    UInt idx = C_make_const(C(), vm, identifier); // GC

    EMIT_OP(C(), GET_OP_TYPE(idx, OP_CLASS), idx);

    if(C()->depth > 0) {
        C_make_local(vm, ppc);
        C_initialize_local(C());
    } else {
        // index for OP_DEFINE_GLOBAL instruction
        UInt gidx = C_make_global_identifier(C(), vm, identifier, PVAR_FLAGS(C()->parser));
        EMIT_OP(C(), GET_OP_TYPE(gidx, OP_DEFINE_GLOBAL), gidx);
    }

    Class cclass;
    cclass.enclosing  = C()->cclass;
    cclass.superclass = false;
    C()->cclass       = &cclass;

    PFLAG_CLEAR(C()->parser, ASSIGN_BIT);

    if(C_match(C(), TOK_IMPL)) {
        C_expect(C(), TOK_IDENTIFIER, "Expect superclass name.");
        parse_variable(vm, ppc);
        if(Identifier_eq(&C()->parser->previous, &class_name)) {
            COMPILER_CLASS_INHERIT_ERR(C(), AS_CSTRING(identifier));
        }
        cclass.superclass = true;
        C_scope_start(C());
        C_new_local(vm, ppc, Token_syn_new("super"));
        C_initialize_local(C());
        named_variable(vm, ppc, class_name);
        EMIT_BYTE(C(), OP_INHERIT);
    }

    named_variable(vm, ppc, class_name); // Push the class

    C_expect(C(), TOK_LBRACE, "Expect '{' before class body.");
    while(!C_check(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        parse_method(vm, ppc);
    }

    C_expect(C(), TOK_RBRACE, "Expect '}' after class body.");
    EMIT_BYTE(C(), OP_POP); // Pop the class

    C()->cclass = cclass.enclosing;

    if(cclass.superclass) {
        C_scope_end(C());
    }
}

SK_INTERNAL(void) parse_dec(VM* vm, PPC ppc)
{
    // Clear upper byte of parser flags
    // for now only 'FIXED' flag
    PFLAG_CLEAR(C()->parser, FIXED_BIT);

    if(C_match(C(), TOK_VAR)) {
        parse_dec_var(vm, ppc);
    } else if(C_match(C(), TOK_FIXED)) {
        parse_dec_var_fixed(vm, ppc);
    } else if(C_match(C(), TOK_FN)) {
        parse_dec_fn(vm, ppc);
    } else if(C_match(C(), TOK_CLASS)) {
        parse_dec_class(vm, ppc);
    } else {
        parse_stm(vm, ppc);
    }

    if(PFLAG_CHECK(C()->parser, PANIC_BIT)) {
        // Sync parser before compiling the next
        // declaration/statement.
        Parser_sync(C());
    }
}

SK_INTERNAL(void)
parse_dec_var(VM* vm, PPC ppc)
{
    if(C_match(C(), TOK_FIXED)) {
        if(PFLAG_CHECK(C()->parser, FIXED_BIT)) {
            C_error(C(), "Expect variable name.");
        }
        PFLAG_SET(C()->parser, FIXED_BIT);
    }

    Int index = parse_varname(vm, ppc, "Expect variable name.");

    if(C_match(C(), TOK_EQUAL)) {
        parse_expr(vm, ppc);
    } else {
        EMIT_BYTE(C(), OP_NIL);
    }

    C_expect(C(), TOK_SEMICOLON, "Expect ';' after variable declaration.");

    if((C())->depth > 0) {
        C_initialize_local(C());
        return;
    }

    EMIT_OP(C(), GET_OP_TYPE(index, OP_DEFINE_GLOBAL), index);
}

SK_INTERNAL(UInt)
parse_varname(VM* vm, PPC ppc, const char* errmsg)
{
    C_expect(C(), TOK_IDENTIFIER, errmsg);

    // If local scope make local variable
    if((C())->depth > 0) {
        C_make_local(vm, ppc);
        return 0;
    }

    // Otherwise make global variable
    return C_make_global(C(), vm, PVAR_FLAGS(C()->parser));
}

SK_INTERNAL(force_inline void)
C_patch_jmp(Compiler* C, UInt jmp_offset)
{
    UInt offset = code_offset(C) - jmp_offset - 3;

    if(unlikely(offset >= UINT24_MAX)) {
        COMPILER_JUMP_LIMIT_ERR(C, UINT24_MAX);
    }

    PUT_BYTES3(&current_chunk(C)->code.data[jmp_offset], offset);
}

SK_INTERNAL(force_inline void) C_add_bstorage(Compiler* C)
{
    Array_Int patches;
    Array_Int_init(&patches);
    Array_Array_Int_push(&C->context.breaks, patches);
}

SK_INTERNAL(force_inline void) C_rm_bstorage(Compiler* C)
{
    Array_Int* patches = Array_Array_Int_last(&C->context.breaks);
    for(Int i = 0; i < (Int)patches->len; i++) {
        C_patch_jmp(C, patches->data[i]);
    }
    Array_Int arr = Array_Array_Int_pop(&C->context.breaks);
    Array_Int_free(&arr, NULL);
}

SK_INTERNAL(void) parse_stm_switch(VM* vm, PPC ppc)
{
    uint64_t mask = CFLOW_MASK(C()->parser);
    PFLAG_CLEAR(C()->parser, LOOP_BIT);
    PFLAG_SET(C()->parser, SWITCH_BIT);

    C_add_bstorage(C());

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'switch'.");
    parse_expr(vm, ppc);
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");
    C_expect(C(), TOK_LBRACE, "Expect '{' after ')'.");

    /* -1 = parsed TOK_DEFAULT
     *  0 = didn't parse TOK_DEFAULT or TOK_CASE yet
     *  >0 = parsed TOK_CASE (stores jmp offset) */
    Int  state = 0;
    bool dflt  = false; /* Set if 'default' is parsed */

    /* fall-through jumps that need patching */
    Array_Int fts;
    Array_Int_init(&fts);

    Int outermostsw_depth          = C()->context.innermostsw_depth;
    C()->context.innermostsw_depth = C()->depth;

    while(!C_match(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        if(C_match(C(), TOK_CASE) || C_match(C(), TOK_DEFAULT)) {
            if(state != 0) {
                Array_Int_push(&fts, EMIT_JMP(C(), OP_JMP));
                if(state != -1) {
                    C_patch_jmp(C(), state);
                }
            }

            state = -1;

            if(C()->parser->previous.type == TOK_CASE) {
                parse_expr(vm, ppc);
                EMIT_BYTE(C(), OP_EQ);
                C_expect(C(), TOK_COLON, "Expect ':' after 'case'.");
                state = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
            } else if(!dflt) {
                dflt = true;
                C_expect(C(), TOK_COLON, "Expect ':' after 'default'.");
            } else {
                COMPILER_SWITCH_DEFAULT_ERR(C());
            }

            if(fts.len > 0) {
                /* Patch Fall-through jump */
                C_patch_jmp(C(), *Array_Int_last(&fts));
            }
        } else {
            if(state == 0) {
                COMPILER_SWITCH_NOCASE_ERR(C());
            }
            parse_stm(vm, ppc);
        }
    }

    if(C()->parser->previous.type == TOK_EOF) {
        COMPILER_SWITCH_RBRACE_ERR(C());
    }

    /* Free fallthrough jumps array */
    Array_Int_free(&fts, NULL);
    /* Patch and remove breaks */
    C_rm_bstorage(C());
    /* Pop switch value */
    EMIT_BYTE(C(), OP_POP);
    /* Restore scope depth */
    C()->context.innermostsw_depth = outermostsw_depth;
    /* Clear switch flag and restore control flow flags */
    PFLAG_CLEAR(C()->parser, SWITCH_BIT);
    C()->parser->flags |= mask;
}

SK_INTERNAL(void) parse_stm_if(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_LPAREN, "Expect '(' after 'if'.");
    parse_expr(vm, ppc); /* Parse conditional */
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional jump instruction */
    UInt else_jmp = EMIT_JMP(C(), OP_JMP_IF_FALSE_AND_POP);

    parse_stm(vm, ppc); /* Parse the code in this branch */

    /* Prevent fall-through if 'else' exists. */
    UInt end_jmp = EMIT_JMP(C(), OP_JMP);

    C_patch_jmp(C(), else_jmp); /* End of 'if' (maybe start of else) */

    if(C_match(C(), TOK_ELSE)) {
        parse_stm(vm, ppc);        /* Parse the else branch */
        C_patch_jmp(C(), end_jmp); /* End of else branch */
    }
}

SK_INTERNAL(void) parse_and(VM* vm, PPC ppc)
{
    UInt jump = EMIT_JMP(C(), OP_JMP_IF_FALSE_OR_POP);
    parse_precedence(vm, ppc, PREC_AND);
    C_patch_jmp(C(), jump);
}

SK_INTERNAL(void) parse_or(VM* vm, PPC ppc)
{
    UInt else_jmp = EMIT_JMP(C(), OP_JMP_IF_FALSE_AND_POP);
    UInt end_jmp  = EMIT_JMP(C(), OP_JMP);
    C_patch_jmp(C(), else_jmp);
    parse_precedence(vm, ppc, PREC_OR);
    C_patch_jmp(C(), end_jmp);
}

SK_INTERNAL(void) parse_stm_while(VM* vm, PPC ppc)
{
    uint64_t mask = CFLOW_MASK(C()->parser);
    PFLAG_CLEAR(C()->parser, SWITCH_BIT);
    PFLAG_SET(C()->parser, LOOP_BIT);

    /* Add loop offset storage for 'break' */
    C_add_bstorage(C());

    Int outermostl_start            = (C())->context.innermostl_start;
    Int outermostl_depth            = (C())->context.innermostl_depth;
    (C())->context.innermostl_start = current_chunk(C())->code.len;
    (C())->context.innermostl_depth = (C())->depth;

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'while'.");
    parse_expr(vm, ppc);
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional exit jump */
    UInt end_jmp = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
    parse_stm(vm, ppc);                                /* Parse loop body */
    C_emit_loop(C(), (C())->context.innermostl_start); /* Jump to the start of the loop */

    C_patch_jmp(C(), end_jmp); /* Set loop exit offset */

    /* Restore the outermost loop start/(scope)depth */
    (C())->context.innermostl_start = outermostl_start;
    (C())->context.innermostl_depth = outermostl_depth;
    /* Remove and patch breaks */
    C_rm_bstorage(C());
    /* Clear loop flag */
    PFLAG_CLEAR(C()->parser, LOOP_BIT);
    /* Restore old flags */
    (C())->parser->flags |= mask;
}

SK_INTERNAL(void) parse_stm_for(VM* vm, PPC ppc)
{
    uint64_t mask = CFLOW_MASK(C()->parser);
    PFLAG_CLEAR(C()->parser, SWITCH_BIT);
    PFLAG_SET(C()->parser, LOOP_BIT);

    /* Add loop offset storage for 'break' */
    C_add_bstorage(C());
    /* Start a new local scope */
    C_scope_start(C());

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'for'.");
    if(C_match(C(), TOK_SEMICOLON)) {
        // No initializer
    } else if(C_match(C(), TOK_VAR)) {
        parse_dec_var(vm, ppc);
    } else if(C_match(C(), TOK_FIXED)) {
        parse_dec_var_fixed(vm, ppc);
    } else {
        parse_stm_expr(vm, ppc);
    }

    /* Save outermost loop start/depth on the stack */
    Int outermostl_start = (C())->context.innermostl_start;
    Int outermostl_depth = (C())->context.innermostl_depth;
    /* Update context inner loop start/depth */
    (C())->context.innermostl_start = current_chunk(C())->code.len;
    (C())->context.innermostl_depth = (C())->depth;

    Int loop_end = -1;
    if(!C_match(C(), TOK_SEMICOLON)) {
        parse_expr(vm, ppc);
        C_expect(C(), TOK_SEMICOLON, "Expect ';' after for-loop condition clause.");
        loop_end = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
    }

    if(!C_match(C(), TOK_RPAREN)) {
        UInt body_start      = EMIT_JMP(C(), OP_JMP);
        UInt increment_start = current_chunk(C())->code.len;
        parse_expr(vm, ppc);
        EMIT_BYTE(C(), OP_POP);
        C_expect(C(), TOK_RPAREN, "Expect ')' after last for-loop clause.");

        C_emit_loop(C(), (C())->context.innermostl_start);
        (C())->context.innermostl_start = increment_start;
        C_patch_jmp(C(), body_start);
    }

    parse_stm(vm, ppc);
    C_emit_loop(C(), (C())->context.innermostl_start);

    if(loop_end != -1) {
        C_patch_jmp(C(), loop_end);
    }

    /* Restore the outermost loop start/depth */
    (C())->context.innermostl_start = outermostl_start;
    (C())->context.innermostl_depth = outermostl_depth;
    /* Remove and patch loop breaks */
    C_rm_bstorage(C());
    /* End the scope */
    C_scope_end(C());
    /* Restore old flags */
    PFLAG_CLEAR(C()->parser, LOOP_BIT);
    (C())->parser->flags |= mask;
}

SK_INTERNAL(void) parse_stm_continue(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'continue'.");

    if(C->context.innermostl_start == -1) {
        COMPILER_CONTINUE_ERR(C);
    }

    Int sdepth = C->context.innermostl_depth;

    UInt popn = 0;
    for(Int i = C->loc_len - 1; i >= 0 && C->locals[i].depth > sdepth; i--) {
        popn++;
    }

    /* If we have continue inside a switch statement
     * then don't forget to pop off the switch value */
    if(PFLAG_CHECK(C->parser, SWITCH_BIT)) {
        popn++;
    }

    // Keep bytecode tidy
    if(popn > 1) {
        EMIT_OP(C, OP_POPN, popn);
    } else {
        EMIT_BYTE(C, OP_POP);
    }

    C_emit_loop(C, C->context.innermostl_start);
}

SK_INTERNAL(void) parse_stm_break(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'break'.");

    Array_Array_Int* arr = &C->context.breaks;

    if(!PFLAG_CHECK(C->parser, LOOP_BIT) && !PFLAG_CHECK(C->parser, SWITCH_BIT)) {
        COMPILER_BREAK_ERR(C);
        return;
    }

    UInt sdepth = PFLAG_CHECK(C->parser, LOOP_BIT) ? C->context.innermostl_depth
                                                   : C->context.innermostsw_depth;

    UInt popn = 0;
    for(Int i = C->loc_len - 1; i >= 0 && C->locals[i].depth > (Int)sdepth; i--) {
        popn++;
    }

    // Keep bytecode tidy
    if(popn > 1) {
        EMIT_OP(C, OP_POPN, popn);
    } else {
        EMIT_BYTE(C, OP_POP);
    }

    Array_Int_push(Array_Array_Int_last(arr), EMIT_JMP(C, OP_JMP));
}

SK_INTERNAL(void) parse_stm_return(VM* vm, PPC ppc)
{
    if(C()->fn_type == FN_SCRIPT) {
        COMPILER_RETURN_SCRIPT_ERR(C());
    }

    if(C_match(C(), TOK_SEMICOLON)) {
        C_emit_return(C());
    } else {
        if(C()->fn_type == FN_INIT) {
            COMPILER_RETURN_INIT_ERR(C(), static_str[SS_INIT].name);
        } else {
            PFLAG_SET(C()->parser, RET_BIT);
        }

        parse_expr(vm, ppc);
        C_expect(C(), TOK_SEMICOLON, "Expect ';' after return value.");
        EMIT_BYTE(C(), OP_RET);
    }
}

SK_INTERNAL(void) parse_stm(VM* vm, PPC ppc)
{
    Compiler* C = C();
    // @TODO: Implement jmp table
    if(C_match(C, TOK_WHILE)) {
        parse_stm_while(vm, ppc);
    } else if(C_match(C, TOK_FOR)) {
        parse_stm_for(vm, ppc);
    } else if(C_match(C, TOK_IF)) {
        parse_stm_if(vm, ppc);
    } else if(C_match(C, TOK_SWITCH)) {
        parse_stm_switch(vm, ppc);
    } else if(C_match(C, TOK_LBRACE)) {
        C_scope_start(C);
        parse_stm_block(vm, ppc);
        C_scope_end(C());
    } else if(C_match(C, TOK_CONTINUE)) {
        parse_stm_continue(C);
    } else if(C_match(C, TOK_BREAK)) {
        parse_stm_break(C);
    } else if(C_match(C, TOK_RETURN)) {
        parse_stm_return(vm, ppc);
    } else {
        parse_stm_expr(vm, ppc);
    }
}

SK_INTERNAL(void) parse_stm_block(VM* vm, PPC ppc)
{
    while(!C_check(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        parse_dec(vm, ppc);
    }
    C_expect(C(), TOK_RBRACE, "Expect '}' after block.");
}

SK_INTERNAL(force_inline void)
compile_number(VM* vm, PPC ppc)
{
    Compiler* C   = C();
    UInt      idx = C_make_const(C, vm, C->parser->previous.value);
    EMIT_OP(C, GET_OP_TYPE(idx, OP_CONST), idx);
}

SK_INTERNAL(force_inline void) parse_variable(VM* vm, PPC ppc)
{
    named_variable(vm, ppc, C()->parser->previous);
}

SK_INTERNAL(force_inline void) compile_string(VM* vm, PPC ppc)
{
    UInt idx = C_make_const(C(), vm, C()->parser->previous.value);
    EMIT_OP(C(), OP_CONST, idx);
}

/* This is the entry point to Pratt parsing */
SK_INTERNAL(force_inline void) parse_grouping(VM* vm, PPC ppc)
{
    parse_expr(vm, ppc);
    C_expect(C(), TOK_RPAREN, "Expect ')' after expression");
}

SK_INTERNAL(void) parse_unary(VM* vm, PPC ppc)
{
    TokenType type = (C())->parser->previous.type;
    parse_precedence(vm, ppc, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            EMIT_BYTE(C(), OP_NEG);
            break;
        case TOK_BANG:
            EMIT_BYTE(C(), OP_NOT);
            break;
        default:
            unreachable;
            return;
    }
}

SK_INTERNAL(void) parse_binary(VM* vm, PPC ppc)
{
    TokenType        type = C()->parser->previous.type;
    const ParseRule* rule = &rules[type];
    parse_precedence(vm, ppc, rule->precedence + 1);

#ifdef SK_PRECOMPUTED_GOTO
    #define TOK_TABLE
    #include "jmptable.h"
    #undef TOK_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break;
#endif
    Compiler* C = C();

    DISPATCH(type)
    {
        CASE(TOK_MINUS)
        {
            EMIT_BYTE(C, OP_SUB);
            BREAK;
        }
        CASE(TOK_PLUS)
        {
            EMIT_BYTE(C, OP_ADD);
            BREAK;
        }
        CASE(TOK_SLASH)
        {
            EMIT_BYTE(C, OP_DIV);
            BREAK;
        }
        CASE(TOK_STAR)
        {
            EMIT_BYTE(C, OP_MUL);
            BREAK;
        }
        CASE(TOK_BANG_EQUAL)
        {
            EMIT_BYTE(C, OP_NOT_EQUAL);
            BREAK;
        }
        CASE(TOK_EQUAL_EQUAL)
        {
            EMIT_BYTE(C, OP_EQUAL);
            BREAK;
        }
        CASE(TOK_GREATER)
        {
            EMIT_BYTE(C, OP_GREATER);
            BREAK;
        }
        CASE(TOK_GREATER_EQUAL)
        {
            EMIT_BYTE(C, OP_GREATER_EQUAL);
            BREAK;
        }
        CASE(TOK_LESS)
        {
            EMIT_BYTE(C, OP_LESS);
            BREAK;
        }
        CASE(TOK_LESS_EQUAL)
        {
            EMIT_BYTE(C, OP_LESS_EQUAL);
            BREAK;
        }
        CASE(TOK_LBRACK)
        CASE(TOK_RBRACK)
        CASE(TOK_LPAREN)
        CASE(TOK_RPAREN)
        CASE(TOK_LBRACE)
        CASE(TOK_RBRACE)
        CASE(TOK_DOT)
        CASE(TOK_COMMA)
        CASE(TOK_COLON)
        CASE(TOK_SEMICOLON)
        CASE(TOK_QMARK)
        CASE(TOK_BANG)
        CASE(TOK_EQUAL)
        CASE(TOK_IDENTIFIER)
        CASE(TOK_STRING)
        CASE(TOK_NUMBER)
        CASE(TOK_AND)
        CASE(TOK_BREAK)
        CASE(TOK_CASE)
        CASE(TOK_CONTINUE)
        CASE(TOK_CLASS)
        CASE(TOK_DEFAULT)
        CASE(TOK_ELSE)
        CASE(TOK_FALSE)
        CASE(TOK_FOR)
        CASE(TOK_FN)
        CASE(TOK_IF)
        CASE(TOK_IMPL)
        CASE(TOK_NIL)
        CASE(TOK_OR)
        CASE(TOK_RETURN)
        CASE(TOK_SUPER)
        CASE(TOK_SELF)
        CASE(TOK_SWITCH)
        CASE(TOK_TRUE)
        CASE(TOK_VAR)
        CASE(TOK_WHILE)
        CASE(TOK_FIXED)
        CASE(TOK_IMPORT)
        CASE(TOK_ERROR)
        CASE(TOK_EOF)
        {
            unreachable;
        }
    }

    unreachable;

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

SK_INTERNAL(force_inline void)
parse_ternarycond(VM* vm, PPC ppc)
{
    UInt jmp_expr2 = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
    // First conditional operator is parsed as if
    // parenthesized (according to C standard)
    parse_precedence(vm, ppc, PREC_TERNARY);
    UInt jmp_end = EMIT_JMP(C(), OP_JMP);
    C_expect(C(), TOK_COLON, "Expect ':' after first 'expr'.");
    C_patch_jmp(C(), jmp_expr2);
    parse_precedence(vm, ppc, PREC_ASSIGNMENT);
    C_patch_jmp(C(), jmp_end);
}

SK_INTERNAL(void) parse_dot(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_IDENTIFIER, "Expect field name after '.'.");
    Value identifier = Token_into_stringval(vm, &C()->parser->previous);
    UInt  idx        = C_make_const(C(), vm, identifier); // GC

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        parse_expr(vm, ppc);
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_SET_PROPERTY), idx);
    } else if(C_match(C(), TOK_LPAREN)) {
        Int argc = parse_arglist(vm, ppc);
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_INVOKE), idx);
        EMIT_LBYTE(C(), argc); // Emit always 24-bit parameter
    } else {
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_GET_PROPERTY), idx);
    }
}


SK_INTERNAL(void) parse_index(VM* vm, PPC ppc)
{
    parse_expr(vm, ppc);
    C_expect(C(), TOK_RBRACK, "Expect ']'.");

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        parse_expr(vm, ppc);
        EMIT_BYTE(C(), OP_SET_INDEX);
    } else if(C_match(C(), TOK_LPAREN)) {
        Int argc = parse_arglist(vm, ppc);
        EMIT_OP(C(), OP_INVOKE_INDEX, argc);
    } else {
        EMIT_BYTE(C(), OP_INDEX);
    }
}

SK_INTERNAL(force_inline void)
parse_literal(unused VM* _, PPC ppc)
{
    Compiler* C = C();
    switch(C->parser->previous.type) {
        case TOK_TRUE:
            EMIT_BYTE(C, OP_TRUE);
            break;
        case TOK_FALSE:
            EMIT_BYTE(C, OP_FALSE);
            break;
        case TOK_NIL:
            EMIT_BYTE(C, OP_NIL);
            break;
        default:
            unreachable;
            return;
    }
}

SK_INTERNAL(void) parse_self(VM* vm, PPC ppc)
{
    if(C()->cclass == NULL) {
        COMPILER_SELF_ERR(C());
        return;
    }

    bool assign = PFLAG_CHECK(C()->parser, ASSIGN_BIT);
    PFLAG_CLEAR(C()->parser, ASSIGN_BIT); // Can't assign to 'self'
    parse_variable(vm, ppc);
    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, assign);
}

SK_INTERNAL(void) parse_super(VM* vm, PPC ppc)
{
    if(C()->cclass == NULL) {
        COMPILER_SUPER_ERR(C());
        return;
    } else if(!C()->cclass->superclass) {
        COMPILER_NO_SUPER_ERR(C());
        return;
    }

    C_expect(C(), TOK_DOT, "Expect '.' after 'super'.");
    C_expect(C(), TOK_IDENTIFIER, "Expect superclass method name after '.'.");

    Value identifier = Token_into_stringval(vm, &C()->parser->previous);
    UInt  idx        = C_make_const(C(), vm, identifier);
    bool  assign     = PFLAG_CHECK(C()->parser, ASSIGN_BIT);

    PFLAG_CLEAR(C()->parser, ASSIGN_BIT);
    named_variable(vm, ppc, Token_syn_new("self"));
    if(C_match(C(), TOK_LPAREN)) {
        UInt argc = parse_arglist(vm, ppc);
        named_variable(vm, ppc, Token_syn_new("super"));
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_INVOKE_SUPER), idx);
        EMIT_LBYTE(C(), argc);
    } else {
        named_variable(vm, ppc, Token_syn_new("super"));
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_GET_SUPER), idx);
    }
    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, assign);
}
