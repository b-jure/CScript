#include "array.h"
#include "chunk.h"
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
#define ERROR_BIT  (1)
#define PANIC_BIT  (2)
#define LOOP_BIT   (3)
#define SWITCH_BIT (4)
#define ASSIGN_BIT (5)
#define FN_BIT     (6)
/* Variable 'flags' bits */
#define FIXED_BIT (9)

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
     * 10 - unused
     * ...
     * 64 - unused
     */
    uint64_t flags;
} Parser;

/* Variable modifier bits */
#define VFIXED_BIT    (FIXED_BIT - 8)
#define VCAPTURED_BIT (8)

// Sets the 'bit' in 'var' flags.
#define Var_flag_set(var, bit) BIT_SET((var)->flags, bit)
// Checks if 'bit' is set.
#define Var_flag_is(var, bit) BIT_CHECK((var)->flags, bit)
// Clears the 'bit' in 'var' flags.
#define Var_flag_clear(var, bit) BIT_CLEAR((var)->flags, bit)
// Clears all flags in 'var' flags.
#define Var_flags_clear(var) ((var)->flags = 0)
// Returns 'var' flags.
#define Var_flags(var) ((var)->flags)

typedef struct {
    Token name;
    Int   depth;
    /*
     * Bits (var modifiers):
     * 1 - fixed <- variable is 'fixed'
     * 2 - unused
     * ...
     * 7 - unused
     * 8 - captured <- variable is captured by Upvalue
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
    (Compiler*)REALLOC(ptr, sizeof(Compiler) + (newcap * sizeof(Local)))

/* Array of HashTable(s) */
ARRAY_NEW(Array_Table, HashTable);
/* Array of Int(s) */
ARRAY_NEW(Array_Int, Int);
/* Two dimensional array */
ARRAY_NEW(Array_Array_Int, Array_Int)

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
    Array_Array_Int breaks; /* Break statement offsets */

    Int innermostl_start;  /* Innermost loop start offset */
    Int innermostl_depth;  /* Innermost loop scope depth */
    Int innermostsw_depth; /* Innermost switch scope depth */
} CFCtx;

/* UpValue is a Value that is declared/defined inside of enclosing function.
 * Contains stack index of that variable inside of that enclosing function.
 * This is useful for closures when capturing enclosing function values.
 * The 'local' field indicates if this is the local variable of the innermost
 * enclosing function or the UpValue.
 *
 * More on this:
 * Skooma is a language with first-class functions, meaning functions can be
 * passed around as arguments the same way as numbers, strings, objects.. (also being
 * first-class). Additionally this language supports closures which are basically wrappers
 * around the functions but also pack additional information about the 'environment'.
 * Environment is actually a mapping associating each free variable (variables used
 * locally inside a function, but defined in an enclosing scope) with the value of
 * the enclosing variable.
 * So main thing to understand is that closures can access variables that are defined
 * outside of them, right before the closure definition up to global scope.
 * Now finally the 'UpValue' is a struct that helps us access those variables
 * outside of the closure by accessing the enclosing function and searching
 * its stack for the value at 'idx'.
 * Because we want the closure to capture its environment across multiple enclosing
 * functions, the UpValue might store the UpValue of the other enclosing function.
 * This is bubbling up is performed until we find the actual index of the Value on the
 * stack.
 */
typedef struct {
    UInt idx;
    bool local;
} Upvalue;

ARRAY_NEW(Array_Upvalue, Upvalue);

struct Compiler {
    /* Enclosing compiler */
    Compiler* enclosing;

    /* Grammar parser */
    Parser parser;

    /* Control flow context, tracks offsets and
     * scope depth for 'continue' and 'break' statements. */
    CFCtx context;

    /* Tracks local variables for each scope. */
    Array_Table loc_defs;

    /* Currently compiled function, meaning we write
     * bytecode to the function chunk in this field. */
    ObjFunction* fn;
    FunctionType fn_type; /* Function type */

    /* Holds UpValues */
    Array_Upvalue upvalues;

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

/* We pass everywhere pointer to the pointer of compiler,
 * because of flexible array that stores 'Local' variables.
 * This means 'Compiler' is whole heap allocated and might
 * reallocate to different memory address if locals array
 * surpasses 'SHORT_STACK_SIZE' and after that on each
 * subsequent call to 'reallocate'.
 * This way when expanding we just update the pointer to the
 * new/old pointer returned by reallocate. */
typedef Compiler** CompilerPPtr;

/* ParseFn - generic parsing function signature. */
typedef void (*ParseFn)(VM*, CompilerPPtr);

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
    Array_Array_Int_init(&context->breaks, NULL, arr_reallocate);
}

/* Have to pass VM while initializing 'Compiler' because we are allocating objects. */
SK_INTERNAL(void)
C_init(Compiler* C, Roots* roots, FunctionType fn_type, Compiler* enclosing)
{
    C->enclosing = enclosing;

    C->fn      = NULL; // Initialize to NULL so gc does not get confused
    C->fn      = ObjFunction_new(roots);
    C->fn_type = fn_type;

    CFCtx_init(&C->context);

    Array_Table_init(&C->loc_defs, NULL, arr_reallocate);
    Array_Table_init_cap(&C->loc_defs, SHORT_STACK_SIZE);

    // Initialize Upvalue array only once when initializing outermost compiler.
    // This array then gets passed on to other nested compilers for each
    // function.
    if(enclosing == NULL) {
        Array_Upvalue_init(&C->upvalues, NULL, arr_reallocate);
        Array_Upvalue_init_cap(&C->upvalues, SHORT_STACK_SIZE);
    } else {
        C->upvalues = enclosing->upvalues;
    }

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
            ObjString_from(roots, C->parser.previous.start, C->parser.previous.len);
    }

    local->name.start = "";
    local->name.len   = 0;
}

SK_INTERNAL(void) C_update_roots(Compiler* C, VM* vm)
{
    Roots* r = vm->global_vals.roots;
    r->c     = C;
    r        = C->fn->chunk.constants.roots;
    r->c     = C;
}

SK_INTERNAL(force_inline void) C_grow_stack(CompilerPPtr Cptr, VM* vm)
{
    Compiler* C      = C();
    UInt      oldcap = C->loc_cap;

    C->loc_cap = MIN(GROW_ARRAY_CAPACITY(oldcap), VM_STACK_MAX);
    C          = GROW_LOCAL_STACK(C, oldcap, C->loc_cap);
    C()        = C;
    C_update_roots(C(), vm);
}

void mark_c_roots(VM* vm, Compiler* C)
{
    for(Compiler* current = C; current != NULL; current = current->enclosing) {
        mark_obj(vm, (Obj*)current->fn);
    }
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

SK_INTERNAL(force_inline Value) Token_into_stringval(Roots* roots, const Token* name)
{
    return OBJ_VAL(ObjString_from(roots, name->start, name->len));
}

SK_INTERNAL(force_inline UInt)
make_const_identifier(Compiler* C, VM* vm, Value identifier, bool fixed)
{
    Value index;

    if(!HashTable_get(&vm->global_ids, identifier, &index)) {
        index = NUMBER_VAL(
            (double)Array_Global_push(&vm->global_vals, (Global){UNDEFINED_VAL, fixed}));

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

SK_INTERNAL(force_inline void) C_emit_lbyte(Compiler* C, UInt bits)
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

    C_emit_lbyte(C, offset);
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
    Compiler* C           = ALLOC_COMPILER();
    Roots     roots       = {C, vm};
    vm->global_vals.roots = &roots;
    C_init(C, &roots, FN_SCRIPT, NULL);
    Parser_init(&C->parser, source);

    C_advance(C);
    while(!C_match(C, TOK_EOF)) {
        parse_dec(vm, &C);
    }

    ObjFunction* fn = compile_end(C);

    bool err = C_flag_is(C, ERROR_BIT);


    Roots* r = fn->chunk.constants.roots;
    r->c     = NULL;
    r        = vm->global_vals.roots;
    r->c     = NULL;

    C_free(C);
    return (err ? NULL : fn);
}

SK_INTERNAL(force_inline Chunk*) current_chunk(Compiler* C)
{
    return &C->fn->chunk;
}

SK_INTERNAL(void) CFCtx_free(CFCtx* context)
{
    Array_Array_Int_free(&context->breaks);
    Array_Array_Int_init(&context->breaks, NULL, arr_reallocate);
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
}

SK_INTERNAL(void) C_free(Compiler* C)
{
    // @TODO: add additional parameter to generic array when
    //        freeing elements of the array (free fn pointer)
    for(UInt i = 0; i < C->loc_defs.len; i++) {
        HashTable_free(&C->loc_defs.data[i]);
    }
    Array_Table_free(&C->loc_defs);
    CFCtx_free(&C->context);
    if(C->enclosing == NULL) {
        Array_Upvalue_free(&C->upvalues);
    }
    FREE(C);
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
    // Safe to decrement loc_len (UInt), only gets called after declaring variable
    Local*     local      = &C->locals[C->loc_len - 1];
    Value      identifier = Token_into_stringval(&(Roots){C, vm}, &local->name);
    HashTable* scope_set  = &C->loc_defs.data[C->depth - 1];

    HashTable_insert(scope_set, identifier, NUMBER_VAL(C->loc_len - 1));
    local->depth = C->depth;
}

SK_INTERNAL(force_inline void) C_scope_start(Compiler* C)
{
    HashTable scope_set;
    HashTable_init(&scope_set);

    if(unlikely((UInt)C->depth >= UINT32_MAX - 1)) {
        C_error(C, "Scope nesting depth limit reached [%u].", UINT32_MAX);
    }

    C->depth++;
    Array_Table_push(&C->loc_defs, scope_set);
}

// End scope and pop locals and/or close captured locals
SK_INTERNAL(force_inline void) C_scope_end(Compiler* C)
{
#define LOCAL_IS_CAPTURED(local) (Var_flag_is((local), VCAPTURED_BIT))

    Int    localc = C->loc_defs.data[C->depth - 1].len;
    Local* local  = &C->locals[C->loc_len - 1];

    Int popn = 0;
    while(popn < localc) {
        if(LOCAL_IS_CAPTURED(local)) {
            if(popn > 0) {
                localc -= popn;
                if(popn == 1) {
                    C_emit_byte(C, OP_POP);
                } else {
                    C_emit_op(C, OP_POPN, popn);
                }
            }

            Int closen = 0;
            while(closen < localc && LOCAL_IS_CAPTURED(local)) {
                local--;
                closen++;
            }
            localc -= closen;

            if(closen == 1) {
                C_emit_byte(C, OP_CLOSE_UPVAL);
            } else {
                C_emit_op(C, OP_CLOSE_UPVALN, closen);
            }

            popn = 0;
            continue;
        }
        local--;
        popn++;
    }

    if(popn == 1) {
        C_emit_byte(C, OP_POP);
    } else if(popn > 1) {
        C_emit_op(C, OP_POPN, popn);
    }

    C->depth--;
    UInt len        = C->loc_defs.data[C->depth].len;
    C->loc_len     -= len;
    gstate.loc_len -= len;

#undef LOCAL_IS_CAPTURED
}

SK_INTERNAL(void)
parse_fn(VM* vm, CompilerPPtr Cptr, FunctionType type)
{
#define C_new() (*Cptr_new)

    Compiler* C_new = ALLOC_COMPILER(); /* Allocate new compiler */

    C_new->parser = C()->parser; // UPDATE OUTER COMPILER LATER

    // Update global_vals vm array with new compiler root
    Roots* r = vm->global_vals.roots; // VM root never changes
    r->c     = C_new;

    /* Initialize the new compiler with the new roots */
    C_init(C_new, r, type, C());

    uint64_t mask = C_flags(C());
    C_flags_clear(C_new);
    C_flag_set(C_new, FN_BIT);

    /* Have to pass the pointer to pointer to Compiler to parse functions, check the
     * typedef for 'CompilerPPtr' in this source file for the rationale behind this. */
    CompilerPPtr Cptr_new = &C_new;

    // No need to end this scope, the frame gets popped anyways.
    C_scope_start(C_new());
    C_expect(C_new(), TOK_LPAREN, "Expect '(' after function name.");

    // Parse function arguments
    if(!C_check(C_new(), TOK_RPAREN)) {
        do {
            C_new()->fn->arity++;
            parse_varname(vm, Cptr_new, "Expect parameter name.");
            C_initialize_local(C_new(), vm);
        } while(C_match(C_new, TOK_COMMA));
    }

    C_expect(C_new(), TOK_RPAREN, "Expect ')' after parameters.");
    C_expect(C_new(), TOK_LBRACE, "Expect '{' before function body.");
    parse_stm_block(vm, Cptr_new);

    ObjFunction* fn = compile_end(C_new);

    // Restore old compiler root for global_vals vm array
    r->c = C();
    // Null out compiled function compiler root
    r    = fn->chunk.constants.roots;
    r->c = NULL;

    // Restore flags but additionally transfer error flag
    // if error happened in the new compiler.
    C_flag_clear(C_new(), FN_BIT);
    C_new()->parser.flags |= (mask | C_flag_is(C_new, ERROR_BIT));

    C()->parser = C_new()->parser; // UPDATE OUTER COMPILER

    // Create a closure only when we have upvalues
    if(fn->upvalc == 0) {
        printf(
            "Writting normal function; fn->upvalc = %u, fn = %s\n",
            fn->upvalc,
            fn->name->storage);
        C_emit_op(C(), OP_CONST, C_make_const(C(), OBJ_VAL(fn)));
        printf("Done writting normal function '%s'\n", fn->name->storage);
    } else {
        printf(
            "Writting closure function; fn->upvalc = %u, fn = %s\n",
            fn->upvalc,
            fn->name->storage);
        C_emit_op(C(), OP_CLOSURE, C_make_const(C(), OBJ_VAL(fn)));
        printf("Done writting closure function '%s'\n", fn->name->storage);
        for(UInt i = 0; i < fn->upvalc; i++) {
            Upvalue* upval = Array_Upvalue_index(&C()->upvalues, i);
            C_emit_byte(C(), upval->local ? 1 : 0);
            C_emit_lbyte(C(), upval->idx);
        }
    }

    C_free(C_new);

#undef C_new
}

SK_INTERNAL(void) parse_dec_fn(VM* vm, CompilerPPtr Cptr)
{
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

SK_INTERNAL(void) C_new_local(CompilerPPtr Cptr, VM* vm)
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
        C_grow_stack(Cptr, vm);
        C = C();
    }

    Local* local = &C->locals[C->loc_len++];
    gstate.loc_len++;
    local->name  = C->parser.previous;
    local->flags = (((Byte)(C_flags(C) >> 8)) & 0xff);
    local->depth = -1;
}

SK_INTERNAL(force_inline bool) Identifier_eq(Token* left, Token* right)
{
    return (left->len == right->len) &&
           (memcmp(left->start, right->start, left->len) == 0);
}

SK_INTERNAL(bool) C_local_is_unique(Compiler* C, VM* vm)
{
    Value      identifier = Token_into_stringval(&(Roots){C, vm}, &C->parser.previous);
    HashTable* scope_set  = Array_Table_index(&C->loc_defs, C->depth - 1);
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
    C_new_local(Cptr, vm);
}

SK_INTERNAL(Int) C_make_global(Compiler* C, VM* vm, bool fixed)
{
    Value identifier = Token_into_stringval(&(Roots){C, vm}, &C->parser.previous);
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
        NUMBER_VAL(Array_Global_push(&vm->global_vals, (Global){UNDEFINED_VAL, false}));
    Value identifier = Token_into_stringval(&(Roots){C, vm}, &C->parser.previous);

    HashTable_insert(&vm->global_ids, identifier, idx);

    return (Int)AS_NUMBER(idx);
}

SK_INTERNAL(UInt) C_get_global(Compiler* C, VM* vm)
{
    Value idx;
    Value identifier = Token_into_stringval(&(Roots){C, vm}, &C->parser.previous);

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
    Array_Int patches;
    Array_Int_init(&patches, NULL, arr_reallocate);
    Array_Array_Int_push(&C->context.breaks, patches);
}

SK_INTERNAL(force_inline void) C_rm_bstorage(Compiler* C)
{
    Array_Int* patches = Array_Array_Int_last(&C->context.breaks);
    for(Int i = 0; i < (Int)patches->len; i++) {
        C_patch_jmp(C, patches->data[i]);
    }
    Array_Int arr = Array_Array_Int_pop(&C->context.breaks);
    Array_Int_free(&arr);
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
    Array_Int fts;
    Array_Int_init(&fts, NULL, arr_reallocate);

    Int outermostsw_depth          = C()->context.innermostsw_depth;
    C()->context.innermostsw_depth = C()->depth;

    while(!C_match(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        if(C_match(C(), TOK_CASE) || C_match(C(), TOK_DEFAULT)) {
            if(state != 0) {
                Array_Int_push(&fts, C_emit_jmp(C(), OP_JMP));
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
                C_patch_jmp(C(), *Array_Int_last(&fts));
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
    Array_Int_free(&fts);
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
    C_scope_start(C());

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
        C_expect(C(), TOK_SEMICOLON, "Expect ';' after for-loop condition clause.");

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

    /* End the scope */
    C_scope_end(C());

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

    // Keep bytecode tidy
    if(popn > 1) {
        C_emit_op(C, OP_POPN, popn);
    } else {
        C_emit_byte(C, OP_POP);
    }

    C_emit_loop(C, C->context.innermostl_start);
}

SK_INTERNAL(void) parse_stm_break(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'break'.");

    Array_Array_Int* arr = &C->context.breaks;

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

    // Keep bytecode tidy
    if(popn > 1) {
        C_emit_op(C, OP_POPN, popn);
    } else {
        C_emit_byte(C, OP_POP);
    }

    Array_Int_push(Array_Array_Int_last(arr), C_emit_jmp(C, OP_JMP));
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
        C_scope_start(C);
        parse_stm_block(vm, Cptr);
        C_scope_end(C());
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

SK_INTERNAL(force_inline Int) C_get_local(Compiler* C, Value name)
{
    Value index;
    for(UInt i = 0; i < C->loc_defs.len; i++) {
        HashTable* scope_set = &C->loc_defs.data[i];
        if(HashTable_get(scope_set, name, &index)) {
            if(C->locals[(UInt)AS_NUMBER(index)].depth == -1) {
                C_error(C, "Can't read local variable in its own initializer.");
            }
            return (Int)AS_NUMBER(index);
        }
    }

    return -1;
}

SK_INTERNAL(force_inline UInt) C_add_upval(Compiler* C, UInt idx, bool local)
{
    for(UInt i = 0; i < C->upvalues.len; i++) {
        Upvalue* upvalue = Array_Upvalue_index(&C->upvalues, i);
        if(upvalue->idx == idx && upvalue->local == local) {
            // Return existing UpValue index
            return i;
        }
    }

    if(unlikely(C->upvalues.len == MIN(VM_STACK_MAX, UINT24_MAX))) {
        C_error(
            C,
            "Too many closure variables (upvalues) in function <fn %s>. Limit [%u].",
            MIN(VM_STACK_MAX, UINT24_MAX),
            C->fn->name->storage);
        return 0;
    }

    // Otherwise add the UpValue into the array
    C->fn->upvalc++;
    return Array_Upvalue_push(&C->upvalues, (Upvalue){idx, local});
}

SK_INTERNAL(Int) C_get_upval(Compiler* C, const Value name)
{
    if(C->enclosing == NULL) {
        printf(
            "Enclosing is NULL, will get global with name: %s.\n",
            AS_CSTRING(name));
        return -1;
    }

    Int idx = C_get_local(C->enclosing, name);
    if(idx != -1) {
        // Local is captured by Upvalue
        Var_flag_set(&C->enclosing->locals[idx], VCAPTURED_BIT);
        return C_add_upval(C, (UInt)idx, true);
    }

    idx = C_get_upval(C->enclosing, name);
    if(idx != -1) {
        return C_add_upval(C, (UInt)idx, false);
    }

    return -1;
}

SK_INTERNAL(force_inline void) parse_variable(VM* vm, CompilerPPtr Cptr)
{
    const Value name = Token_into_stringval(&(Roots){C(), vm}, &C()->parser.previous);
    OpCode      setop, getop;
    Int         idx   = C_get_local(C(), name);
    int16_t     flags = -1;

    if(idx != -1) {
        Local* var = &(C())->locals[idx];
        flags      = Var_flags(var);
        setop      = GET_OP_TYPE(idx, OP_SET_LOCAL);
        getop      = GET_OP_TYPE(idx, OP_GET_LOCAL);
    } else if((idx = C_get_upval(C(), name)) != -1) {
        setop = OP_SET_UPVALUE;
        getop = OP_GET_UPVALUE;
    } else {
        idx   = C_get_global(C(), vm);
        flags = vm->global_vals.data[idx].fixed;
        setop = GET_OP_TYPE(idx, OP_SET_GLOBAL);
        getop = GET_OP_TYPE(idx, OP_GET_GLOBAL);
    }

    if(C_flag_is(C(), ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        /* In case this is local variable statically check for mutability */
        if(BIT_CHECK(flags, VFIXED_BIT)) {
            C_error(
                C(),
                "Can't assign to variable '%s', it is declared as 'fixed'.",
                AS_CSTRING(name));
        }
        parse_expr(vm, Cptr);
        C_emit_op(C(), setop, idx);
    } else {
        C_emit_op(C(), getop, idx);
    }
}

SK_INTERNAL(force_inline void) parse_string(VM* vm, CompilerPPtr Cptr)
{
    Compiler*  C      = C();
    ObjString* string = ObjString_from(
        &(Roots){C(), vm},
        C->parser.previous.start + 1,
        C->parser.previous.len - 2);
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
