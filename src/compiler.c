#include "array.h"
#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "object.h"
#include "scanner.h"
#include "skconf.h"
#include "value.h"
#include "vmachine.h"

#ifdef DEBUG
    #include "debug.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define GET_OP_TYPE(idx, op) (idx <= UINT8_MAX) ? op : op##L

#define code_offset(C) (current_chunk(C)->code.len)

/* Parser 'flags' bits */
#define ERROR_BIT  1
#define PANIC_BIT  2
#define LOOP_BIT   3
#define SWITCH_BIT 4
#define ASSIGN_BIT 5
#define FN_BIT     6
#define FIXED_BIT  9

#define C_flag_set(C, bit)   BIT_SET((C)->parser.flags, bit)
#define C_flag_is(C, bit)    BIT_CHECK((C)->parser.flags, bit)
#define C_flag_clear(C, bit) BIT_CLEAR((C)->parser.flags, bit)
#define C_flags_clear(C)     ((C)->parser.flags = 0)
#define C_flags(C)           ((C)->parser.flags)

// clang-format off
#define GSTATE_INIT { 0 }
// clang-format on
#define CFLOW_MASK(C) ((uint64_t)(btoul(SWITCH_BIT) | btoul(LOOP_BIT)) & C_flags(C))
#define FN_MASK(C)    ((uint64_t)(btoul(FN_BIT)) & C_flags(C))

/* Global compiler state */
typedef struct {
    UInt loc_len; /* Total number of local variables across functions */
} GState;

GState gstate = GSTATE_INIT;

#define C() (*Cptr)

typedef struct {
    Scanner scanner;
    Token   previous;
    Token   current;
    /*
     * Parser flags.
     * 1 - error bit <- error indicator
     * 2 - panic bit <- panic mode
     * 3 - loop bit <- inside a loop
     * 4 - switch bit <- inside a switch statement
     * 5 - assign bit <- can parse assignment
     * 6 - fn bit <- inside a function
     * 7 - unused
     * 8 - unused
     * 9 - fixed bit <- variable modifier (immutable)
     * ...
     * 64 - unused
     */
    uint64_t flags;
} Parser;

typedef struct {
    Token name;
    Int   depth;
    /*
     * Bits (var modifiers):
     * 1 - fixed
     * ...
     * 8 - unused
     */
    Byte flags;
} Local;

/* Max 'Compiler' stack size, limited to long bytecode instructions */
#define LOCAL_STACK_MAX (UINT24_MAX + 1)

/* Default 'Compiler' stack size */
#define SHORT_STACK_SIZE (UINT8_MAX + 1)

/* Allocates 'Compiler' with the default stack size */
#define ALLOC_COMPILER() MALLOC(sizeof(Compiler) + (SHORT_STACK_SIZE * sizeof(Local)))

/* Grows 'Compiler' stack */
#define GROW_LOCAL_STACK(ptr, oldcap, newcap)                                            \
    (Compiler*)REALLOC(                                                                  \
        ptr,                                                                             \
        sizeof(Compiler) + (oldcap * sizeof(Local)),                                     \
        sizeof(Compiler) + (newcap * sizeof(Local)))

/* Array of HashTable(s) */
DECLARE_ARRAY(HashTable);
DEFINE_ARRAY(HashTable);
/* Array of Int(s) */
DECLARE_ARRAY(Int);
DEFINE_ARRAY(Int);
/* Two dimensional array */
DECLARE_ARRAY(IntArray);
DEFINE_ARRAY(IntArray);

typedef enum {
    FN_FUNCTION,
    FN_SCRIPT,
} FunctionType;

/* Control Flow context */
typedef struct {
    /* We store break offsets inside an array because we don't
     * know in advance where the end of the switch/loop statement
     * is while parsing.
     * When we hit the end of the loop/switch statement then
     * we can just patch each parsed break statement.
     * This would be the same for 'continue' in case of 'do while'
     * loop.*/
    IntArrayArray breaks; /* Break statement offsets */

    Int innermostl_start;  /* Innermost loop start offset */
    Int innermostl_depth;  /* Innermost loop scope depth */
    Int innermostsw_depth; /* Innermost switch scope depth */
} CFCtx;

typedef struct {
    /* Grammar parser */
    Parser parser;

    /* Control flow context, tracks offsets and
     * scope depth for 'continue' and 'break' statements. */
    CFCtx context;

    /* Tracks local variables for each scope. */
    HashTableArray loc_defs;

    /* Currently compiled function, meaning we write
     * bytecode to the function chunk in this field. */
    ObjFunction* fn;
    FunctionType fn_type; /* Function type */

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
} Compiler;

/* We pass everywhere pointer to the pointer of compiler,
 * because of flexible array that stores 'Local' variables.
 * This means 'Compiler' is whole heap allocated and might
 * reallocate to different memory address if locals array
 * surpasses 'SHORT_STACK_SIZE' and after that on each
 * subsequent call to 'reallocate'.
 * This way when expanding we just update the pointer to the
 * new/old pointer returned by reallocate. */
typedef Compiler** CompilerPPtr;

/* Variable modifier bits */
#define VFIXED_BIT               (FIXED_BIT - 8)
#define Var_flag_set(var, bit)   BIT_SET((var)->flags, bit)
#define Var_flag_is(var, bit)    BIT_CHECK((var)->flags, bit)
#define Var_flag_clear(var, bit) BIT_CLEAR((var)->flags, bit)
#define Var_flags_clear(var)     ((var)->flags = 0)
#define Var_flags(var)           ((var)->flags)

/* ParseFn - generic parsing function signature. */
typedef void (*ParseFn)(VM*, CompilerPPtr);

// Used for Pratt parsing algorithm
typedef struct {
    ParseFn    prefix;
    ParseFn    infix;
    Precedence precedence;
} ParseRule;

/* Checks for equality between the 'token_type' and the current parser token */
#define C_check(compiler, token_type) ((compiler)->parser.current.type == token_type)

/* Internal */
SK_INTERNAL(Chunk*) current_chunk(Compiler* C);
SK_INTERNAL(void) C_advance(Compiler* compiler);
SK_INTERNAL(void) C_error(Compiler* compiler, const char* error, ...);
SK_INTERNAL(void) C_free(Compiler* C);
SK_INTERNAL(void) Parser_init(Parser* parser, const char* source);
SK_INTERNAL(void) parse_number(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_string(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(UInt)
parse_varname(VM* vm, CompilerPPtr Cptr, const char* errmsg);
SK_INTERNAL(void) parse_precedence(VM* vm, CompilerPPtr Cptr, Precedence prec);
SK_INTERNAL(void) parse_grouping(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_ternarycond(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_expr(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_dec(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void)
parse_dec_var(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void)
parse_dec_var_fixed(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_dec_fn(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_stm_block(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_stm(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_stm_print(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_variable(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_binary(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_unary(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_literal(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_and(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_or(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_call(VM* vm, CompilerPPtr Cptr);

SK_INTERNAL(void) CFCtx_init(CFCtx* context)
{
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
    IntArrayArray_init(&context->breaks);
}

/* Have to pass VM while initializing 'Compiler' because we are allocating objects. */
SK_INTERNAL(void) C_init(Compiler* C, VM* vm, FunctionType fn_type)
{
    C->fn      = ObjFunction_new(vm);
    C->fn_type = fn_type;

    CFCtx_init(&C->context);
    HashTableArray_init(&C->loc_defs);
    HashTableArray_init_cap(&C->loc_defs, SHORT_STACK_SIZE);

    C->loc_len = 0;
    C->loc_cap = SHORT_STACK_SIZE;
    C->depth   = 0;

    /* Reserve first stack slot for VM */
    Local* local = &C->locals[C->loc_len++];
    gstate.loc_len++;
    local->depth = 0;
    local->flags = 0;

    if(fn_type != FN_SCRIPT) {
        C->fn->name =
            ObjString_from(vm, C->parser.previous.start, C->parser.previous.len);
    }

    local->name.start = "";
    local->name.len   = 0;
}

SK_INTERNAL(force_inline void) C_grow_stack(CompilerPPtr Cptr)
{
    Compiler* C      = C();
    UInt      oldcap = C->loc_cap;

    C->loc_cap = MIN(GROW_ARRAY_CAPACITY(oldcap), UINT24_MAX);
    C          = GROW_LOCAL_STACK(C, oldcap, C->loc_cap);
    C()        = C;
}

/*========================== EMIT =========================*/

SK_INTERNAL(force_inline UInt) C_make_const(Compiler* C, Value constant)
{
    if(unlikely(current_chunk(C)->constants.len > MIN(VM_STACK_MAX, UINT24_MAX))) {
        C_error(
            C,
            "Too many constants defined in a single chunk. This function -> <fn %s>.",
            C->fn->name->storage);
    }

    return Chunk_make_constant(current_chunk(C), constant);
}

SK_INTERNAL(force_inline Value) Token_into_stringval(VM* vm, const Token* name)
{
    return OBJ_VAL(ObjString_from(vm, name->start, name->len));
}

SK_INTERNAL(force_inline UInt)
make_const_identifier(Compiler* C, VM* vm, Value identifier, bool fixed)
{
    Value index;

    if(!HashTable_get(&vm->global_ids, identifier, &index)) {
        index = NUMBER_VAL(
            (double)GlobalArray_push(&vm->global_vals, (Global){UNDEFINED_VAL, fixed}));

        HashTable_insert(&vm->global_ids, identifier, index);
    }

    UInt i = (UInt)AS_NUMBER(index);

    if(IS_DECLARED(vm->global_vals.data[i].value)) {
        C_error(
            C,
            "Redefinition of declared global variable '%s'.",
            AS_CSTRING(identifier));
    }

    return i;
}

SK_INTERNAL(force_inline void) C_emit_byte(Compiler* C, Byte byte)
{
    Chunk_write(current_chunk(C), byte, C->parser.previous.line);
}

SK_INTERNAL(force_inline void) C_emit_return(Compiler* C)
{
    C_emit_byte(C, OP_NIL);
    C_emit_byte(C, OP_RET);
}

SK_INTERNAL(force_inline void) C_emit_op(Compiler* C, OpCode code, UInt param)
{
    Chunk_write_codewparam(current_chunk(C), code, param, C->parser.previous.line);
}

SK_INTERNAL(force_inline UInt) C_emit_jmp(Compiler* C, OpCode jmp)
{
    Chunk_write_codewparam(current_chunk(C), jmp, 0, C->parser.previous.line);
    return code_offset(C) - 3;
}

SK_INTERNAL(force_inline void) _emit_24bit(Compiler* C, UInt bits)
{
    C_emit_byte(C, BYTE(bits, 0));
    C_emit_byte(C, BYTE(bits, 1));
    C_emit_byte(C, BYTE(bits, 2));
}

SK_INTERNAL(force_inline void) C_emit_loop(Compiler* C, UInt start)
{
    C_emit_byte(C, OP_LOOP);

    UInt offset = current_chunk(C)->code.len - start + 3;

    if(offset >= UINT24_MAX) {
        C_error(
            C,
            "Too much code to jump over. Bytecode index limit reached [%u].",
            UINT24_MAX);
    }

    _emit_24bit(C, offset);
}

/*========================= PARSER ========================*/

SK_INTERNAL(force_inline void) Parser_init(Parser* parser, const char* source)
{
    parser->scanner = Scanner_new(source);
    parser->flags   = 0;
}

static void C_error_at(Compiler* C, Token* token, const char* error, va_list args)
{
    if(C_flag_is(C, PANIC_BIT)) {
        return;
    }

    fprintf(stderr, "[line: %u] Error", token->line);

    if(token->type == TOK_EOF) {
        fprintf(stderr, " at end");
    } else if(token->type != TOK_ERROR) {
        fprintf(stderr, " at '%.*s'", token->len, token->start);
    }

    fputs(": ", stderr);
    vfprintf(stderr, error, args);
    fputs("\n", stderr);

    C_flag_set(C, PANIC_BIT);
    C_flag_set(C, ERROR_BIT);
}

static void C_error(Compiler* compiler, const char* error, ...)
{
    va_list args;
    va_start(args, error);
    C_error_at(compiler, &compiler->parser.current, error, args);
    va_end(args);
}

SK_INTERNAL(void) C_advance(Compiler* C)
{
    C->parser.previous = C->parser.current;

    while(true) {
        C->parser.current = Scanner_scan(&C->parser.scanner);
        if(C->parser.current.type != TOK_ERROR) {
            break;
        }

        C_error(C, C->parser.current.start);
    }
}

static void Parser_sync(Compiler* C)
{
    // @TODO: Create precomputed goto table
    C_flag_clear(C, PANIC_BIT);

    while(C->parser.current.type != TOK_EOF) {
        if(C->parser.previous.type == TOK_SEMICOLON) {
            return;
        }

        switch(C->parser.current.type) {
            case TOK_FOR:
            case TOK_FN:
            case TOK_VAR:
            case TOK_CLASS:
            case TOK_IF:
            case TOK_PRINT:
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
    ObjFunction* fn = C->fn;
    C_emit_byte(C, OP_RET);
#ifdef DEBUG_PRINT_CODE
    if(!C_flag_is(C, ERROR_BIT)) {
        Chunk_debug(current_chunk(C), (fn->name) ? fn->name->storage : "<script>");
    }
#endif
    return fn;
}

ObjFunction* compile(VM* vm, const char* source)
{
    Compiler* C = ALLOC_COMPILER();
    C_init(C, vm, FN_SCRIPT);
    Parser_init(&C->parser, source);
    C_advance(C);

    while(!C_match(C, TOK_EOF)) {
        parse_dec(vm, &C);
    }

    ObjFunction* fn = compile_end(C);

    bool err = C_flag_is(C, ERROR_BIT);
    C_free(C);

    return (err ? NULL : fn);
}

SK_INTERNAL(force_inline Chunk*) current_chunk(Compiler* C)
{
    return &C->fn->chunk;
}

SK_INTERNAL(void) CFCtx_free(CFCtx* context)
{
    IntArrayArray_free(&context->breaks);
    IntArrayArray_init(&context->breaks);
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
}

SK_INTERNAL(void) C_free(Compiler* C)
{
    HashTableArray_free(&C->loc_defs);
    CFCtx_free(&C->context);
    free(C);
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
    [TOK_LPAREN]        = {parse_grouping,      parse_call,        PREC_CALL      },
    [TOK_RPAREN]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_LBRACE]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_RBRACE]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_COMMA]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_DOT]           = {NULL,                NULL,              PREC_NONE      },
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
    [TOK_STRING]        = {parse_string,        NULL,              PREC_NONE      },
    [TOK_NUMBER]        = {parse_number,        NULL,              PREC_NONE      },
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
    [TOK_PRINT]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_RETURN]        = {NULL,                NULL,              PREC_NONE      },
    [TOK_SUPER]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_SELF]          = {NULL,                NULL,              PREC_NONE      },
    [TOK_TRUE]          = {parse_literal,       NULL,              PREC_NONE      },
    [TOK_VAR]           = {parse_dec_var,       NULL,              PREC_NONE      },
    [TOK_WHILE]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_ERROR]         = {NULL,                NULL,              PREC_NONE      },
    [TOK_EOF]           = {NULL,                NULL,              PREC_NONE      },
};

SK_INTERNAL(UInt) parse_arglist(VM* vm, CompilerPPtr Cptr)
{
    UInt argc = 0;

    if(!C_check(C(), TOK_RPAREN)) {
        do {
            parse_expr(vm, Cptr);
            if(argc == UINT24_MAX) {
                C_error(C(), "Can't have more than %u arguments.", UINT24_MAX);
            }
            argc++;
        } while(C_match(C(), TOK_COMMA));
    }

    C_expect(C(), TOK_RPAREN, "Expect ')' after function arguments.");
    return argc;
}

SK_INTERNAL(void) parse_call(VM* vm, CompilerPPtr Cptr)
{
    UInt argc = parse_arglist(vm, Cptr);
    C_emit_op(C(), GET_OP_TYPE(argc, OP_CALL), argc);
}

SK_INTERNAL(void)
parse_stm_expr(VM* vm, CompilerPPtr Cptr)
{
    parse_expr(vm, Cptr);
    C_expect(C(), TOK_SEMICOLON, "Expect ';' after expression.");
    C_emit_byte(C(), OP_POP);
}

SK_INTERNAL(void) parse_expr(VM* vm, CompilerPPtr Cptr)
{
    parse_precedence(vm, Cptr, PREC_ASSIGNMENT);
}

SK_INTERNAL(void) parse_precedence(VM* vm, CompilerPPtr Cptr, Precedence prec)
{
    C_advance(C());
    ParseFn prefix_fn = rules[C()->parser.previous.type].prefix;
    if(prefix_fn == NULL) {
        C_error(C(), "Expect expression.");
        return;
    }

    if(prec <= PREC_ASSIGNMENT) {
        C_flag_set(C(), ASSIGN_BIT);
    } else {
        C_flag_clear(C(), ASSIGN_BIT);
    }

    prefix_fn(vm, Cptr);

    while(prec <= rules[(C())->parser.current.type].precedence) {
        C_advance(C());
        ParseFn infix_fn = rules[(C())->parser.previous.type].infix;
        infix_fn(vm, Cptr);
    }

    if(C_flag_is(C(), ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        C_error(C(), "Invalid assignment target.");
    }
}

SK_INTERNAL(void)
parse_dec_var_fixed(VM* vm, CompilerPPtr Cptr)
{
    C_flag_set(C(), FIXED_BIT);
    C_expect(C(), TOK_VAR, "Expect 'var' in variable declaration.");
    parse_dec_var(vm, Cptr);
}

SK_INTERNAL(force_inline void) C_initialize_local(Compiler* C, VM* vm)
{
    Local*     local = &C->locals[C->loc_len - 1]; /* Safe to decrement unsigned int */
    Value      identifier = Token_into_stringval(vm, &local->name);
    HashTable* scope_set  = &C->loc_defs.data[C->depth - 1];

    HashTable_insert(scope_set, identifier, NUMBER_VAL(C->loc_len - 1));
    local->depth = C->depth; /* Not necessary */
}

SK_INTERNAL(force_inline void) C_start_scope(Compiler* C)
{
    HashTable scope_set;
    HashTable_init(&scope_set);

    if(unlikely((UInt)C->depth >= UINT32_MAX - 1)) {
        C_error(C, "Scope nesting depth limit reached [%u].", UINT32_MAX);
    }

    C->depth++;
    HashTableArray_push(&C->loc_defs, scope_set);
}

SK_INTERNAL(force_inline void) C_end_scope(Compiler* C)
{
    UInt      popn       = C->loc_defs.data[C->depth - 1].len;
    HashTable scope_defs = HashTableArray_pop(&C->loc_defs);
    HashTable_free(&scope_defs);
    C->depth--;
    C->loc_len     -= popn;
    gstate.loc_len -= popn;
    C_emit_op(C, OP_POPN, popn);
}

SK_INTERNAL(void)
parse_fn(VM* vm, CompilerPPtr Cptr, FunctionType type)
{
#define C_new() (*Cptr_new)

    Compiler* C_new = ALLOC_COMPILER(); /* Allocate new compiler */
    C_new->parser   = C()->parser;      /* Use the same parser */
    C_init(C_new, vm, type);            /* Initialize the new compiler */

    /* Have to pass the pointer to pointer to Compiler to parse functions, check the
     * typedef for 'CompilerPPtr' in this source file for the rationale behind this. */
    CompilerPPtr Cptr_new = &C_new;

    /* Start new scope */
    C_start_scope(C_new());
    C_expect(C_new(), TOK_LPAREN, "Expect '(' after function name.");

    /* Parse function arguments */
    if(!C_check(C_new(), TOK_RPAREN)) {
        do {
            C_new()->fn->arity++;
            parse_varname(vm, Cptr_new, "Expect parameter name.");
            C_initialize_local(C_new(), vm);
        } while(C_match(C_new, TOK_COMMA));
    }

    C_expect(C_new(), TOK_RPAREN, "Expect ')' after parameters.");
    C_expect(C_new(), TOK_LBRACE, "Expect '{' before function body.");

    /* Parse the function body */
    parse_stm_block(vm, Cptr_new);

    /* End compilation after function body and emit bytecode */
    ObjFunction* fn = compile_end(C_new);

    /* Update the outer parser */
    C()->parser = C_new()->parser;

    /* Emit constant instruction with the value of the parsed function */
    UInt idx = C_make_const(C(), OBJ_VAL(fn));
    C_emit_op(C(), GET_OP_TYPE(idx, OP_CONST), idx);

    /* Free the new compiler allocation */
    C_free(C_new);
#undef C_new
}

SK_INTERNAL(void) parse_dec_fn(VM* vm, CompilerPPtr Cptr)
{
    uint64_t mask = FN_MASK(C());
    C_flag_set(C(), FN_BIT);

    UInt idx = parse_varname(vm, Cptr, "Expect function name.");

    /* Initialize the variable that holds the function, in order to
     * allow usage of the function inside of its body. */
    if(C()->depth > 0) {
        C_initialize_local(C(), vm);
    }

    parse_fn(vm, Cptr, FN_FUNCTION);

    if(C()->depth == 0) {
        // Mark as declared to guard against redefinition
        vm->global_vals.data[idx].value = DECLARED_VAL;
        C_emit_op(C(), GET_OP_TYPE(idx, OP_DEFINE_GLOBAL), idx);
    }

    C_flag_clear(C(), FN_BIT);
    C()->parser.flags |= mask;
}

SK_INTERNAL(void) parse_dec(VM* vm, CompilerPPtr Cptr)
{
    /* Clear variable modifiers from bit 9.. */
    C_flag_clear(C(), FIXED_BIT);

    if(C_match(C(), TOK_VAR)) {
        parse_dec_var(vm, Cptr);
    } else if(C_match(C(), TOK_FIXED)) {
        parse_dec_var_fixed(vm, Cptr);
    } else if(C_match(C(), TOK_FN)) {
        parse_dec_fn(vm, Cptr);
    } else {
        parse_stm(vm, Cptr);
    }

    if(C_flag_is(C(), PANIC_BIT)) {
        Parser_sync(C());
    }
}

SK_INTERNAL(void)
parse_dec_var(VM* vm, CompilerPPtr Cptr)
{
    if(C_match(C(), TOK_FIXED)) {
        C_flag_set(C(), FIXED_BIT);
    }

    Int index = parse_varname(vm, Cptr, "Expect variable name.");

    if(C_match(C(), TOK_EQUAL)) {
        parse_expr(vm, Cptr);
    } else {
        C_emit_byte(C(), OP_NIL);
    }

    C_expect(C(), TOK_SEMICOLON, "Expect ';' after variable declaration.");

    if((C())->depth > 0) {
        C_initialize_local(C(), vm);
        return;
    }

    vm->global_vals.data[index].value = DECLARED_VAL;
    C_emit_op(C(), GET_OP_TYPE(index, OP_DEFINE_GLOBAL), index);
}

SK_INTERNAL(void) C_new_local(CompilerPPtr Cptr)
{
    Compiler* C = C();

    if(unlikely(C->loc_len >= C->loc_cap)) {
        if(unlikely(gstate.loc_len >= MIN(VM_STACK_MAX, LOCAL_STACK_MAX))) {
            C_error(
                C,
                "Too many variables defined in this source file. Limit is [%u].",
                MIN(VM_STACK_MAX, LOCAL_STACK_MAX));
            return;
        }
        C_grow_stack(Cptr);
    }

    Local* local = &C->locals[C->loc_len++];
    gstate.loc_len++;
    local->name  = C->parser.previous;
    local->flags = ((Byte)(C_flags(C) >> 8) & 0xff);
    local->depth = -1; /* Not necessary, can initialize to C->depth instead */
}

SK_INTERNAL(force_inline bool) Identifier_eq(Token* left, Token* right)
{
    return (left->len == right->len) &&
           (memcmp(left->start, right->start, left->len) == 0);
}

SK_INTERNAL(bool) C_local_is_unique(Compiler* C, VM* vm)
{
    Value      identifier = Token_into_stringval(vm, &C->parser.previous);
    HashTable* scope_set  = HashTableArray_index(&C->loc_defs, C->depth - 1);
    return !HashTable_get(scope_set, identifier, NULL);
}

SK_INTERNAL(void) C_make_local(CompilerPPtr Cptr, VM* vm)
{
    Compiler* C = C();
    if(!C_local_is_unique(C, vm)) {
        C_error(
            C,
            "Redefinition of local variable '%.*s'.",
            C->parser.previous.len,
            C->parser.previous.start);
    }
    C_new_local(Cptr);
}

SK_INTERNAL(Int) C_make_global(Compiler* C, VM* vm, bool fixed)
{
    Value identifier = Token_into_stringval(vm, &C->parser.previous);
    UInt  idx        = make_const_identifier(C, vm, identifier, fixed);

    if(unlikely(idx > UINT24_MAX)) {
        C_error(
            C,
            "Too many global variables defined. Bytecode instruction index limit "
            "reached [%u].",
            UINT24_MAX);
    }

    return idx;
}

SK_INTERNAL(force_inline Int) C_make_undefined_global(Compiler* C, VM* vm)
{
    Value idx =
        NUMBER_VAL(GlobalArray_push(&vm->global_vals, (Global){UNDEFINED_VAL, false}));
    Value identifier = Token_into_stringval(vm, &C->parser.previous);

    HashTable_insert(&vm->global_ids, identifier, idx);

    return (Int)AS_NUMBER(idx);
}

SK_INTERNAL(UInt) Global_idx(Compiler* C, VM* vm)
{
    Value idx;
    Value identifier = Token_into_stringval(vm, &C->parser.previous);

    if(!HashTable_get(&vm->global_ids, identifier, &idx)) {
        if(C_flag_is(C, FN_BIT)) {
            // Create reference to a global but don't declare or define it
            idx = NUMBER_VAL(C_make_undefined_global(C, vm));
        } else {
            // If we are in global scope and there is no global identifier,
            C_error(C, "Undefined variable '%s'.", AS_CSTRING(identifier));
            return 0;
        }
    } else {
        UInt i = (UInt)AS_NUMBER(idx);
        // If we are in global scope and there is a global identifier
        // but its value is of type undefined, that means we didn't omit
        // OP_DEFINE_GLOBAL(L) instruction for it.
        // @FIX: Make this somehow work in REPL mode
        if(!C_flag_is(C, FN_BIT) && IS_UNDEFINED(vm->global_vals.data[i].value)) {
            C_error(C, "Undefined variable '%s'.", AS_CSTRING(identifier));
            return 0;
        }
    }

    return (UInt)AS_NUMBER(idx);
}

SK_INTERNAL(UInt)
parse_varname(VM* vm, CompilerPPtr Cptr, const char* errmsg)
{
    C_expect(C(), TOK_IDENTIFIER, errmsg);

    // If local scope make local variable
    if((C())->depth > 0) {
        C_make_local(Cptr, vm);
        return 0;
    }

    // Otherwise make global variable
    return C_make_global(C(), vm, C_flag_is(C(), FIXED_BIT));
}

SK_INTERNAL(force_inline void)
C_patch_jmp(Compiler* C, UInt jmp_offset)
{
    UInt offset = code_offset(C) - jmp_offset - 3;

    if(unlikely(offset >= UINT24_MAX)) {
        C_error(
            C,
            "Too much code to jump over. Bytecode index size limit reached [%u]",
            UINT24_MAX);
    }

    PUT_BYTES3(&current_chunk(C)->code.data[jmp_offset], offset);
}

SK_INTERNAL(force_inline void) C_add_bstorage(Compiler* C)
{
    IntArray patches;
    IntArray_init(&patches);
    IntArrayArray_push(&C->context.breaks, patches);
}

SK_INTERNAL(force_inline void) C_rm_bstorage(Compiler* C)
{
    IntArray* patches = IntArrayArray_last(&C->context.breaks);
    for(Int i = 0; i < (Int)patches->len; i++) {
        C_patch_jmp(C, patches->data[i]);
    }
    IntArray arr = IntArrayArray_pop(&C->context.breaks);
    IntArray_free(&arr);
}

SK_INTERNAL(void) parse_stm_switch(VM* vm, CompilerPPtr Cptr)
{
    uint64_t mask = CFLOW_MASK(C());
    C_flag_clear(C(), LOOP_BIT);
    C_flag_set(C(), SWITCH_BIT);

    C_add_bstorage(C());

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'switch'.");
    parse_expr(vm, Cptr);
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    C_expect(C(), TOK_LBRACE, "Expect '{' after ')'.");

    /* -1 = parsed TOK_DEFAULT
     *  0 = didn't parse TOK_DEFAULT or TOK_CASE yet
     *  >0 = parsed TOK_CASE (stores jmp offset) */
    Int  state = 0;
    bool dflt  = false; /* Set if 'default' is parsed */

    /* fall-through jumps that need patching */
    IntArray fts;
    IntArray_init(&fts);

    Int outermostsw_depth          = C()->context.innermostsw_depth;
    C()->context.innermostsw_depth = C()->depth;

    while(!C_match(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        if(C_match(C(), TOK_CASE) || C_match(C(), TOK_DEFAULT)) {
            if(state != 0) {
                IntArray_push(&fts, C_emit_jmp(C(), OP_JMP));
                if(state != -1) {
                    C_patch_jmp(C(), state);
                }
            }

            state = -1;

            if((C())->parser.previous.type == TOK_CASE) {
                parse_expr(vm, Cptr);
                C_emit_byte(C(), OP_EQ);
                C_expect(C(), TOK_COLON, "Expect ':' after 'case'.");

                state = C_emit_jmp(C(), OP_JMP_IF_FALSE_AND_POP);

            } else if(!dflt) {
                dflt = true;
                C_expect(C(), TOK_COLON, "Expect ':' after 'default'.");
            } else {
                C_error(C(), "Multiple 'default' labels in a single 'switch'.");
            }

            if(fts.len > 0) {
                /* Patch Fall-through jump */
                C_patch_jmp(C(), *IntArray_last(&fts));
            }
        } else {
            if(state == 0) {
                C_error(C(), "Can't have statements before first case.");
            }
            parse_stm(vm, Cptr);
        }
    }

    if((C())->parser.previous.type == TOK_EOF) {
        C_error(C(), "Expect '}' at the end of 'switch'.");
    }

    /* Free fallthrough jumps array */
    IntArray_free(&fts);
    /* Patch and remove breaks */
    C_rm_bstorage(C());
    /* Pop switch value */
    C_emit_byte(C(), OP_POP);
    /* Restore scope depth */
    C()->context.innermostsw_depth = outermostsw_depth;
    /* Clear switch flag and restore control flow flags */
    C_flag_clear(C(), SWITCH_BIT);
    C()->parser.flags |= mask;
}

SK_INTERNAL(void) parse_stm_if(VM* vm, CompilerPPtr Cptr)
{
    C_expect(C(), TOK_LPAREN, "Expect '(' after 'if'.");
    parse_expr(vm, Cptr); /* Parse conditional */
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional jump instruction */
    UInt else_jmp = C_emit_jmp(C(), OP_JMP_IF_FALSE_AND_POP);

    parse_stm(vm, Cptr); /* Parse the code in this branch */

    /* Prevent fall-through if 'else' exists. */
    UInt end_jmp = C_emit_jmp(C(), OP_JMP);

    C_patch_jmp(C(), else_jmp); /* End of 'if' (maybe start of else) */

    if(C_match(C(), TOK_ELSE)) {
        parse_stm(vm, Cptr);       /* Parse the else branch */
        C_patch_jmp(C(), end_jmp); /* End of else branch */
    }
}

SK_INTERNAL(void) parse_and(VM* vm, CompilerPPtr Cptr)
{
    UInt jump = C_emit_jmp(C(), OP_JMP_IF_FALSE_OR_POP);
    parse_precedence(vm, Cptr, PREC_AND);
    C_patch_jmp(C(), jump);
}

SK_INTERNAL(void) parse_or(VM* vm, CompilerPPtr Cptr)
{
    UInt else_jmp = C_emit_jmp(C(), OP_JMP_IF_FALSE_AND_POP);
    UInt end_jmp  = C_emit_jmp(C(), OP_JMP);

    C_patch_jmp(C(), else_jmp);

    parse_precedence(vm, Cptr, PREC_OR);
    C_patch_jmp(C(), end_jmp);
}

SK_INTERNAL(void) parse_stm_while(VM* vm, CompilerPPtr Cptr)
{
    uint64_t mask = CFLOW_MASK(C());
    C_flag_clear(C(), SWITCH_BIT);
    C_flag_set(C(), LOOP_BIT);

    /* Add loop offset storage for 'break' */
    C_add_bstorage(C());

    Int outermostl_start            = (C())->context.innermostl_start;
    Int outermostl_depth            = (C())->context.innermostl_depth;
    (C())->context.innermostl_start = current_chunk(C())->code.len;
    (C())->context.innermostl_depth = (C())->depth;

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'while'.");
    parse_expr(vm, Cptr);
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional exit jump */
    UInt end_jmp = C_emit_jmp(C(), OP_JMP_IF_FALSE_AND_POP);
    parse_stm(vm, Cptr);                               /* Parse loop body */
    C_emit_loop(C(), (C())->context.innermostl_start); /* Jump to the start of the loop */

    C_patch_jmp(C(), end_jmp); /* Set loop exit offset */

    /* Restore the outermost loop start/(scope)depth */
    (C())->context.innermostl_start = outermostl_start;
    (C())->context.innermostl_depth = outermostl_depth;
    /* Remove and patch breaks */
    C_rm_bstorage(C());
    /* Clear loop flag */
    C_flag_clear(C(), LOOP_BIT);
    /* Restore old flags */
    (C())->parser.flags |= mask;
}

SK_INTERNAL(void) parse_stm_for(VM* vm, CompilerPPtr Cptr)
{
    uint64_t mask = CFLOW_MASK(C());
    C_flag_clear(C(), SWITCH_BIT);
    C_flag_set(C(), LOOP_BIT);

    /* Add loop offset storage for 'break' */
    C_add_bstorage(C());
    /* Start a new local scope */
    C_start_scope(C());

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'for'.");
    if(C_match(C(), TOK_SEMICOLON)) {
        // No initializer
    } else if(C_match(C(), TOK_VAR)) {
        parse_dec_var(vm, Cptr);
    } else if(C_match(C(), TOK_FIXED)) {
        parse_dec_var_fixed(vm, Cptr);
    } else {
        parse_stm_expr(vm, Cptr);
    }

    /* Save outermost loop start/depth on the stack */
    Int outermostl_start = (C())->context.innermostl_start;
    Int outermostl_depth = (C())->context.innermostl_depth;
    /* Update context inner loop start/depth */
    (C())->context.innermostl_start = current_chunk(C())->code.len;
    (C())->context.innermostl_depth = (C())->depth;

    Int loop_end = -1;
    if(!C_match(C(), TOK_SEMICOLON)) {
        parse_expr(vm, Cptr);
        C_expect(C(), TOK_SEMICOLON, "Expect ';' (condition).");

        loop_end = C_emit_jmp(C(), OP_JMP_IF_FALSE_AND_POP);
    }

    if(!C_match(C(), TOK_SEMICOLON)) {
        UInt body_start      = C_emit_jmp(C(), OP_JMP);
        UInt increment_start = current_chunk(C())->code.len;
        parse_expr(vm, Cptr);
        C_emit_byte(C(), OP_POP);
        C_expect(C(), TOK_RPAREN, "Expect ')' after last for-loop clause.");

        C_emit_loop(C(), (C())->context.innermostl_start);
        (C())->context.innermostl_start = increment_start;
        C_patch_jmp(C(), body_start);
    }

    parse_stm(vm, Cptr);
    C_emit_loop(C(), (C())->context.innermostl_start);

    if(loop_end != -1) {
        C_patch_jmp(C(), loop_end);
    }

    /* Restore the outermost loop start/depth */
    (C())->context.innermostl_start = outermostl_start;
    (C())->context.innermostl_depth = outermostl_depth;
    /* Remove and patch loop breaks */
    C_rm_bstorage(C());
    /* Finally end the scope */
    C_end_scope(C());
    /* Restore old flags */
    C_flag_clear(C(), LOOP_BIT);
    (C())->parser.flags |= mask;
}

SK_INTERNAL(void) parse_stm_continue(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'continue'.");

    if(C->context.innermostl_start == -1) {
        C_error(C, "'continue' statement not in loop statement.");
    }

    Int sdepth = C->context.innermostl_depth;

    UInt popn = 0;
    for(Int i = C->loc_len - 1; i >= 0 && C->locals[i].depth > sdepth; i--) {
        popn++;
    }

    /* If we have continue inside a switch statement
     * then don't forget to pop off the switch value */
    if(C_flag_is(C, SWITCH_BIT)) {
        popn++;
    }

    C_emit_op(C, OP_POPN, popn);
    C_emit_loop(C, C->context.innermostl_start);
}

SK_INTERNAL(void) parse_stm_break(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'break'.");

    IntArrayArray* arr = &C->context.breaks;

    if(!C_flag_is(C, LOOP_BIT) && !C_flag_is(C, SWITCH_BIT)) {
        C_error(C, "'break' statement not in loop or switch statement.");
        return;
    }

    UInt sdepth = C_flag_is(C, LOOP_BIT) ? C->context.innermostl_depth
                                         : C->context.innermostsw_depth;

    UInt popn = 0;
    for(Int i = C->loc_len - 1; i >= 0 && C->locals[i].depth > (Int)sdepth; i--) {
        popn++;
    }

    C_emit_op(C, OP_POPN, popn);
    IntArray_push(IntArrayArray_last(arr), C_emit_jmp(C, OP_JMP));
}

SK_INTERNAL(void) parse_stm_return(VM* vm, CompilerPPtr Cptr)
{
    if(C()->fn_type == FN_SCRIPT) {
        C_error(C(), "Can't return from top-level code.");
    }

    if(C_match(C(), TOK_SEMICOLON)) {
        C_emit_return(C());
    } else {
        parse_expr(vm, Cptr);
        C_expect(C(), TOK_SEMICOLON, "Expect ';' after return value.");
        C_emit_byte(C(), OP_RET);
    }
}

SK_INTERNAL(void) parse_stm(VM* vm, CompilerPPtr Cptr)
{
    Compiler* C = C();
    // @TODO: Implement goto table
    if(C_match(C, TOK_PRINT)) {
        parse_stm_print(vm, Cptr);
    } else if(C_match(C, TOK_WHILE)) {
        parse_stm_while(vm, Cptr);
    } else if(C_match(C, TOK_FOR)) {
        parse_stm_for(vm, Cptr);
    } else if(C_match(C, TOK_IF)) {
        parse_stm_if(vm, Cptr);
    } else if(C_match(C, TOK_SWITCH)) {
        parse_stm_switch(vm, Cptr);
    } else if(C_match(C, TOK_LBRACE)) {
        C_start_scope(C);
        parse_stm_block(vm, Cptr);
    } else if(C_match(C, TOK_CONTINUE)) {
        parse_stm_continue(C);
    } else if(C_match(C, TOK_BREAK)) {
        parse_stm_break(C);
    } else if(C_match(C, TOK_RETURN)) {
        parse_stm_return(vm, Cptr);
    } else {
        parse_stm_expr(vm, Cptr);
    }
}

SK_INTERNAL(void) parse_stm_block(VM* vm, CompilerPPtr Cptr)
{
    while(!C_check(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        parse_dec(vm, Cptr);
    }

    C_expect(C(), TOK_RBRACE, "Expect '}' after block.");

    C_end_scope(C());
}

SK_INTERNAL(force_inline void)
parse_stm_print(VM* vm, CompilerPPtr Cptr)
{
    parse_expr(vm, Cptr);
    C_expect(C(), TOK_SEMICOLON, "Expect ';' after value");
    C_emit_byte(C(), OP_PRINT);
}

SK_INTERNAL(force_inline void)
parse_number(unused VM* _, CompilerPPtr Cptr)
{
    Compiler* C        = C();
    double    constant = strtod(C->parser.previous.start, NULL);
    UInt      idx      = C_make_const(C, NUMBER_VAL(constant));
    C_emit_op(C, GET_OP_TYPE(idx, OP_CONST), idx);
}

SK_INTERNAL(force_inline Int) Local_idx(Compiler* C, VM* vm, const Token* name)
{
    Value index      = NUMBER_VAL(-1);
    Value identifier = Token_into_stringval(vm, name);

    for(Int i = 0; i < (Int)C->loc_defs.len; i++) {
        HashTable* scope_set = &C->loc_defs.data[i];
        if(HashTable_get(scope_set, identifier, &index)) {
            return (Int)AS_NUMBER(index);
        }
    }

    return -1;
}

SK_INTERNAL(force_inline void) parse_variable(VM* vm, CompilerPPtr Cptr)
{
    const Token* name = &(C())->parser.previous;
    OpCode       setop, getop;
    Int          idx   = Local_idx(C(), vm, name);
    int16_t      flags = -1;

    if(idx != -1) {
        Local* var = &(C())->locals[idx];
        flags      = Var_flags(var);
        setop      = GET_OP_TYPE(idx, OP_SET_LOCAL);
        getop      = GET_OP_TYPE(idx, OP_GET_LOCAL);
    } else {
        idx   = Global_idx(C(), vm);
        flags = vm->global_vals.data[idx].fixed;
        setop = GET_OP_TYPE(idx, OP_SET_GLOBAL);
        getop = GET_OP_TYPE(idx, OP_GET_GLOBAL);
    }

    if(C_flag_is(C(), ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        /* In case this is local variable statically check for mutability */
        if(BIT_CHECK(flags, VFIXED_BIT)) {
            C_error(
                C(),
                "Can't assign to variable '%.*s', it is declared as 'fixed'.",
                name->len,
                name->start);
        }
        parse_expr(vm, Cptr);
        C_emit_op(C(), setop, idx);
    } else {
        C_emit_op(C(), getop, idx);
    }
}

SK_INTERNAL(force_inline void) parse_string(VM* vm, CompilerPPtr Cptr)
{
    Compiler*  C = C();
    ObjString* string =
        ObjString_from(vm, C->parser.previous.start + 1, C->parser.previous.len - 2);
    UInt idx = C_make_const(C, OBJ_VAL(string));
    C_emit_op(C, OP_CONST, idx);
}

/* This is the entry point to Pratt parsing */
SK_INTERNAL(force_inline void) parse_grouping(VM* vm, CompilerPPtr Cptr)
{
    parse_expr(vm, Cptr);
    C_expect(C(), TOK_RPAREN, "Expect ')' after expression");
}

SK_INTERNAL(void) parse_unary(VM* vm, CompilerPPtr Cptr)
{
    TokenType type = (C())->parser.previous.type;
    parse_precedence(vm, Cptr, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            C_emit_byte(C(), OP_NEG);
            break;
        case TOK_BANG:
            C_emit_byte(C(), OP_NOT);
            break;
        default:
            unreachable;
            return;
    }
}

SK_INTERNAL(void) parse_binary(VM* vm, CompilerPPtr Cptr)
{
    TokenType        type = (C())->parser.previous.type;
    const ParseRule* rule = &rules[type];
    parse_precedence(vm, Cptr, rule->precedence + 1);

#ifdef THREADED_CODE
    // IMPORTANT: update accordingly if TokenType enum is changed!
    static const void* jump_table[TOK_EOF + 1] = {
        // Make sure order is the same as in the TokenType enum
        0,       /* TOK_LPAREN */
        0,       /* TOK_RPAREN */
        0,       /* TOK_LBRACE */
        0,       /* TOK_RBRACE */
        0,       /* TOK_DOT */
        0,       /* TOK_COMMA */
        &&minus, /* TOK_MINUS */
        &&plus,  /* TOK_PLUS */
        0,       /* TOK_COLON */
        0,       /* TOK_SEMICOLON */
        &&slash, /* TOK_SLASH */
        &&star,  /* TOK_STAR */
        0,       /* TOK_QMARK */
        0,       /* TOK_BANG */
        &&neq,   /* TOK_BANG_EQUAL */
        0,       /* TOK_EQUAL */
        &&eq,    /* TOK_EQUAL_EQUAL */
        &&gt,    /* TOK_GREATER */
        &&gteq,  /* TOK_GREATER_EQUAL */
        &&lt,    /* TOK_LESS */
        &&lteq,  /* TOK_LESS_EQUAL */
        0,       /* TOK_IDENTIFIER */
        0,       /* TOK_STRING */
        0,       /* TOK_NUMBER */
        0,       /* TOK_AND */
        0,       /* TOK_CLASS */
        0,       /* TOK_ELSE */
        0,       /* TOK_FALSE */
        0,       /* TOK_FOR */
        0,       /* TOK_FN */
        0,       /* TOK_IF */
        0,       /* TOK_IMPL */
        0,       /* TOK_NIL */
        0,       /* TOK_OR */
        0,       /* TOK_PRINT */
        0,       /* TOK_RETURN */
        0,       /* TOK_SUPER */
        0,       /* TOK_SELF */
        0,       /* TOK_TRUE */
        0,       /* TOK_VAR */
        0,       /* TOK_WHILE */
        0,       /* TOK_FIXED */
        0,       /* TOK_ERROR */
        0,       /* TOK_EOF */
    };

    Compiler* C = C();
    goto*     jump_table[type];

minus:
    C_emit_byte(C, OP_SUB);
    return;
plus:
    C_emit_byte(C, OP_ADD);
    return;
slash:
    C_emit_byte(C, OP_DIV);
    return;
star:
    C_emit_byte(C, OP_MUL);
    return;
neq:
    C_emit_byte(C, OP_NOT_EQUAL);
    return;
eq:
    C_emit_byte(C, OP_EQUAL);
    return;
gt:
    C_emit_byte(C, OP_GREATER);
    return;
gteq:
    C_emit_byte(C, OP_GREATER_EQUAL);
    return;
lt:
    C_emit_byte(C, OP_LESS);
    return;
lteq:
    C_emit_byte(C, OP_LESS_EQUAL);
    return;

    unreachable;
#else
    switch(type) {
        case TOK_MINUS:
            emit_byte(C, OP_SUB);
            break;
        case TOK_PLUS:
            emit_byte(C, OP_ADD);
            break;
        case TOK_SLASH:
            emit_byte(C, OP_DIV);
            break;
        case TOK_STAR:
            emit_byte(C, OP_MUL);
            break;
        case TOK_BANG_EQUAL:
            emit_byte(C, OP_NOT_EQUAL);
            break;
        case TOK_EQUAL_EQUAL:
            emit_byte(C, OP_EQUAL);
            break;
        case TOK_GREATER:
            emit_byte(C, OP_GREATER);
            break;
        case TOK_GREATER_EQUAL:
            emit_byte(C, OP_GREATER_EQUAL);
            break;
        case TOK_LESS:
            emit_byte(C, OP_LESS);
            break;
        case TOK_LESS_EQUAL:
            emit_byte(C, OP_LESS_EQUAL);
            break;
        default:
            unreachable;
            return;
    }
#endif
}

SK_INTERNAL(force_inline void)
parse_ternarycond(VM* vm, CompilerPPtr Cptr)
{
    //@TODO: Implement...
    parse_expr(vm, Cptr);
    C_expect(C(), TOK_COLON, "Expect ': expr' (ternary conditional).");
    parse_expr(vm, Cptr);
}

SK_INTERNAL(force_inline void)
parse_literal(unused VM* _, CompilerPPtr Cptr)
{
    Compiler* C = C();
    switch(C->parser.previous.type) {
        case TOK_TRUE:
            C_emit_byte(C, OP_TRUE);
            break;
        case TOK_FALSE:
            C_emit_byte(C, OP_FALSE);
            break;
        case TOK_NIL:
            C_emit_byte(C, OP_NIL);
            break;
        default:
            unreachable;
            return;
    }
}
