#include "array.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "object.h"
#include "scanner.h"
#include "value.h"
#include "vmachine.h"
#ifdef DEBUG_PRINT_CODE
    #include "debug.h"
#endif

#include <stdio.h>
#include <stdlib.h>

/* @TODO: Implement break, continue
 *  - need to track the start of the loop
 *  - if there is no start of the loop invoke error
 *  - additionally track the scope depth and pop off all the
 *  locals when continue is invoked, resetting the loop scope locals.
 *  - when break is invoked jump to the end of the loop and remove scope. */

#define GET_OP_TYPE(idx, op) (idx <= UINT8_MAX) ? op : op##L

#define code_offset() (current_chunk()->code.len)

/* Parser 'flags' bits */
#define ERROR_BIT  1
#define PANIC_BIT  2
#define LOOP_BIT   3
#define SWITCH_BIT 4
#define ASSIGN_BIT 5
#define FIXED_BIT  9

#define C_flag_set(C, bit)   BIT_SET((C)->parser.flags, bit)
#define C_flag_is(C, bit)    BIT_CHECK((C)->parser.flags, bit)
#define C_flag_clear(C, bit) BIT_CLEAR((C)->parser.flags, bit)
#define C_flags_clear(C)     ((C)->parser.flags = 0)
#define C_flags(C)           ((C)->parser.flags)

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
     * 6 - unused
     * 7 - unused
     * 8 - unused
     * 9 - fixed bit <- variable modifier (immutable)
     * ...
     * 64 - unused
     */
    uint64_t flags;
} Parser;

typedef struct {
    Token token;
    /*
     * Bits (var modifiers):
     * 1 - fixed
     * ...
     * 8 - unused
     */
    Byte flags;
} Local;

#define LOCAL_STACK_MAX  (UINT24_MAX + 1)
#define SHORT_STACK_SIZE (UINT8_MAX + 1)

#define ALLOC_COMPILER() MALLOC(sizeof(Compiler) + (SHORT_STACK_SIZE * sizeof(Local)))

#define GROW_LOCAL_STACK(ptr, oldcap, newcap)                                            \
    (Compiler*)REALLOC(                                                                  \
        ptr,                                                                             \
        sizeof(Compiler) + (oldcap * sizeof(Local)),                                     \
        sizeof(Compiler) + (newcap * sizeof(Local)))

DECLARE_ARRAY(HashTable);
DEFINE_ARRAY(HashTable);

typedef struct {
    Int            ctjmp;    /* 'continue' jump */
    Int            brjmp;    /* 'break' jump */
    Parser         parser;   /* Grammar parser */
    HashTableArray ldefs;    /* Tracks locals for each scope */
    UInt           depth;    /* Scope depth */
    UInt           llen;     /* Locals count */
    UInt           lcap;     /* Locals array capacity */
    Local          locals[]; /* Locals array (up to 24-bit [LOCAL_STACK_MAX]) */
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
#define VAR_FIXED_BIT (FIXED_BIT - 8)

#define Var_flag_set(var, bit)   BIT_SET((var)->flags, bit)
#define Var_flag_is(var, bit)    BIT_CHECK((var)->flags, bit)
#define Var_flag_clear(var, bit) BIT_CLEAR((var) -.flags, bit)
#define Var_flags_clear(var)     ((var)->flags = 0)
#define Var_flags(var)           ((var)->flags)

/* ParseFn - generic parsing function signature. */
typedef void (*ParseFn)(VM*, CompilerPPtr);

typedef struct {
    ParseFn    prefix;
    ParseFn    infix;
    Precedence precedence;
} ParseRule;

/* Chunk being currently compiled, see 'compile()' */
static Chunk* compiling_chunk;
/* Returns currently stored chunk in 'compiling_chunk' global. */
SK_INTERNAL(Chunk*) current_chunk();

/* Checks for equality between the 'token_type' and the current parser token */
#define C_check(compiler, token_type) ((compiler)->parser.current.type == token_type)

/* Internal */
SK_INTERNAL(void) C_advance(Compiler* compiler);
SK_INTERNAL(void) C_error(Compiler* compiler, const char* error);
SK_INTERNAL(void) Compiler_free(Compiler* C);
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
parse_decvar(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void)
parse_decvar_fixed(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_stm_block(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_stm(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_stm_print(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_variable(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_binary(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_unary(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_literal(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_and(VM* vm, CompilerPPtr Cptr);
SK_INTERNAL(void) parse_or(VM* vm, CompilerPPtr Cptr);

SK_INTERNAL(Compiler*) C_new(const char* source)
{
    Compiler* C = MALLOC(sizeof(Compiler) + ((SHORT_STACK_SIZE) * sizeof(Local)));
    Parser_init(&C->parser, source);
    HashTableArray_init(&C->ldefs);
    HashTableArray_init_cap(&C->ldefs, SHORT_STACK_SIZE);
    C->llen  = 0; /* @FIX: Make this pointer into the locals stack? */
    C->lcap  = SHORT_STACK_SIZE;
    C->depth = 0;
    C_advance(C);
    return C;
}

SK_INTERNAL(force_inline void) C_grow_stack(CompilerPPtr Cptr)
{
    Compiler* C      = *Cptr;
    UInt      oldcap = C->lcap;

    C->lcap = MIN(GROW_ARRAY_CAPACITY(oldcap), UINT24_MAX);
    C       = GROW_LOCAL_STACK(C, oldcap, C->lcap);
    *Cptr   = C;
}

/*========================== EMIT =========================*/

SK_INTERNAL(force_inline UInt) C_make_const(Compiler* C, Value constant)
{
    if(current_chunk()->constants.len <= MIN(VM_STACK_MAX, UINT24_MAX)) {
        return Chunk_make_constant(current_chunk(), constant);
    } else {
        C_error(C, "Too many constants in one chunk.");
        return 0;
    }
}

SK_INTERNAL(force_inline Value) Token_into_stringval(VM* vm, const Token* name)
{
    return OBJ_VAL(ObjString_from(vm, name->start, name->len));
}

SK_INTERNAL(force_inline UInt)
make_constant_identifier(VM* vm, Value identifier, bool fixed)
{
    Value index;
    if(!HashTable_get(&vm->global_ids, identifier, &index)) {
        index = NUMBER_VAL(
            (double)GlobalArray_push(&vm->global_vals, (Global){UNDEFINED_VAL, fixed}));
        HashTable_insert(&vm->global_ids, identifier, index);
    }

    UInt i = (UInt)AS_NUMBER(index);
    // Ensure global redefinition is (not) fixed
    vm->global_vals.data[i].fixed = fixed;
    return i;
}

SK_INTERNAL(force_inline void) C_emit_byte(Compiler* C, Byte byte)
{
    Chunk_write(current_chunk(), byte, C->parser.previous.line);
}

SK_INTERNAL(force_inline void) C_emit_op(Compiler* C, OpCode code, UInt param)
{
    Chunk_write_codewparam(current_chunk(), code, param, C->parser.previous.line);
}

SK_INTERNAL(force_inline UInt) C_emit_jmp(Compiler* C, VM* vm, OpCode jmp)
{
    Chunk_write_codewparam(current_chunk(), jmp, 0, C->parser.previous.line);
    return code_offset() - 3;
}

SK_INTERNAL(force_inline void) C_emit_loop(Compiler* C, UInt start)
{
    C_emit_byte(C, OP_LOOP);

    UInt offset = current_chunk()->code.len - start + 3;

    if(offset >= UINT24_MAX) {
        C_error(C, "Too much code to jump over.");
    }

    C_emit_byte(C, BYTE(offset, 0));
    C_emit_byte(C, BYTE(offset, 1));
    C_emit_byte(C, BYTE(offset, 2));
}

/*========================= PARSER ========================*/

SK_INTERNAL(force_inline void) Parser_init(Parser* parser, const char* source)
{
    parser->scanner = Scanner_new(source);
    parser->flags   = 0;
}

static void C_error_at(Compiler* C, Token* token, const char* error)
{
    if(C_flag_is(C, PANIC_BIT)) {
        return;
    }

    C_flag_set(C, PANIC_BIT);
    fprintf(stderr, "[line: %u] Error", token->line);

    if(token->type == TOK_EOF) {
        fprintf(stderr, " at end");
    } else if(token->type != TOK_ERROR) {
        fprintf(stderr, " at '%.*s'", token->len, token->start);
    }

    fprintf(stderr, ": %s\n", error);
    C_flag_set(C, ERROR_BIT);
}

static void C_error(Compiler* compiler, const char* error)
{
    C_error_at(compiler, &compiler->parser.current, error);
}

SK_INTERNAL(void) C_advance(Compiler* compiler)
{
    compiler->parser.previous = compiler->parser.current;

    while(true) {
        compiler->parser.current = Scanner_scan(&compiler->parser.scanner);
        if(compiler->parser.current.type != TOK_ERROR) {
            break;
        }

        C_error(compiler, compiler->parser.current.start);
    }
}

static void Parser_sync(Compiler* C)
{
    // @ Create precomputed goto table
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

#ifndef DEBUG_TRACE_EXECUTION
SK_INTERNAL(force_inline void)
compile_end(Compiler* compiler)
#else
SK_INTERNAL(force_inline void)
compile_end(Compiler* C, VM* vm)
#endif
{
    C_emit_byte(C, OP_RET);
#ifdef DEBUG_PRINT_CODE
    if(!C_flag_is(C, ERROR_BIT)) {
        Chunk_debug(current_chunk(), "code", vm);
    }
#endif
}

bool compile(VM* vm, const char* source, Chunk* chunk)
{
    Compiler* C     = C_new(source);
    compiling_chunk = chunk;

    while(!C_match(C, TOK_EOF)) {
        parse_dec(vm, &C);
    }

#ifndef DEBUG_TRACE_EXECUTION
    compile_end(C);
#else
    compile_end(C, vm);
#endif
    bool not_err = !C_flag_is(C, ERROR_BIT);
    Compiler_free(C);
    return not_err;
}

SK_INTERNAL(Chunk) * current_chunk()
{
    return compiling_chunk;
}

SK_INTERNAL(void) Compiler_free(Compiler* C)
{
    HashTableArray_free(&C->ldefs);
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
    [TOK_LPAREN]        = {parse_grouping,     NULL,              PREC_NONE      },
    [TOK_RPAREN]        = {NULL,               NULL,              PREC_NONE      },
    [TOK_LBRACE]        = {NULL,               NULL,              PREC_NONE      },
    [TOK_RBRACE]        = {NULL,               NULL,              PREC_NONE      },
    [TOK_COMMA]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_DOT]           = {NULL,               NULL,              PREC_NONE      },
    [TOK_MINUS]         = {parse_unary,        parse_binary,      PREC_TERM      },
    [TOK_PLUS]          = {NULL,               parse_binary,      PREC_TERM      },
    [TOK_COLON]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_SEMICOLON]     = {NULL,               NULL,              PREC_NONE      },
    [TOK_SLASH]         = {NULL,               parse_binary,      PREC_FACTOR    },
    [TOK_STAR]          = {NULL,               parse_binary,      PREC_FACTOR    },
    [TOK_QMARK]         = {NULL,               parse_ternarycond, PREC_TERNARY   },
    [TOK_BANG]          = {parse_unary,        NULL,              PREC_NONE      },
    [TOK_BANG_EQUAL]    = {NULL,               parse_binary,      PREC_EQUALITY  },
    [TOK_EQUAL]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_EQUAL_EQUAL]   = {NULL,               parse_binary,      PREC_EQUALITY  },
    [TOK_GREATER]       = {NULL,               parse_binary,      PREC_COMPARISON},
    [TOK_GREATER_EQUAL] = {NULL,               parse_binary,      PREC_COMPARISON},
    [TOK_LESS]          = {NULL,               parse_binary,      PREC_COMPARISON},
    [TOK_LESS_EQUAL]    = {NULL,               parse_binary,      PREC_COMPARISON},
    [TOK_IDENTIFIER]    = {parse_variable,     NULL,              PREC_NONE      },
    [TOK_STRING]        = {parse_string,       NULL,              PREC_NONE      },
    [TOK_NUMBER]        = {parse_number,       NULL,              PREC_NONE      },
    [TOK_AND]           = {NULL,               parse_and,         PREC_NONE      },
    [TOK_CLASS]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_ELSE]          = {NULL,               NULL,              PREC_NONE      },
    [TOK_FALSE]         = {parse_literal,      NULL,              PREC_NONE      },
    [TOK_FOR]           = {NULL,               NULL,              PREC_NONE      },
    [TOK_FN]            = {NULL,               NULL,              PREC_NONE      },
    [TOK_FIXED]         = {parse_decvar_fixed, NULL,              PREC_NONE      },
    [TOK_IF]            = {NULL,               NULL,              PREC_NONE      },
    [TOK_NIL]           = {parse_literal,      NULL,              PREC_NONE      },
    [TOK_OR]            = {NULL,               parse_or,          PREC_NONE      },
    [TOK_PRINT]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_RETURN]        = {NULL,               NULL,              PREC_NONE      },
    [TOK_SUPER]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_SELF]          = {NULL,               NULL,              PREC_NONE      },
    [TOK_TRUE]          = {parse_literal,      NULL,              PREC_NONE      },
    [TOK_VAR]           = {parse_decvar,       NULL,              PREC_NONE      },
    [TOK_WHILE]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_ERROR]         = {NULL,               NULL,              PREC_NONE      },
    [TOK_EOF]           = {NULL,               NULL,              PREC_NONE      },
};

SK_INTERNAL(void)
parse_stm_expr(VM* vm, CompilerPPtr Cptr)
{
    parse_expr(vm, Cptr);
    C_expect(*Cptr, TOK_SEMICOLON, "Expect ';' after expression.");
    C_emit_byte(*Cptr, OP_POP);
}

SK_INTERNAL(void) parse_expr(VM* vm, CompilerPPtr Cptr)
{
    parse_precedence(vm, Cptr, PREC_ASSIGNMENT);
}

SK_INTERNAL(void) parse_precedence(VM* vm, CompilerPPtr Cptr, Precedence prec)
{
    C_advance(*Cptr);
    ParseFn prefix_fn = rules[(*Cptr)->parser.previous.type].prefix;
    if(prefix_fn == NULL) {
        C_error(*Cptr, "Expect expression.");
        return;
    }

    if(prec <= PREC_ASSIGNMENT) {
        C_flag_set(*Cptr, ASSIGN_BIT);
    } else {
        C_flag_clear(*Cptr, ASSIGN_BIT);
    }
    /* Parse unary operator (prefix) or a literal */
    prefix_fn(vm, Cptr);

    /* Parse binary operator (inifix) with higher or equal precedence if any */
    while(prec <= rules[(*Cptr)->parser.current.type].precedence) {
        C_advance(*Cptr);
        ParseFn infix_fn = rules[(*Cptr)->parser.previous.type].infix;
        infix_fn(vm, Cptr);
    }

    if(C_flag_is(*Cptr, ASSIGN_BIT) && C_match(*Cptr, TOK_EQUAL)) {
        C_error(*Cptr, "Invalid assignment target.");
    }
}

SK_INTERNAL(void)
parse_decvar_fixed(VM* vm, CompilerPPtr Cptr)
{
    C_flag_set(*Cptr, FIXED_BIT);
    C_expect(*Cptr, TOK_VAR, "Expect 'var' in variable declaration.");
    parse_decvar(vm, Cptr);
}

SK_INTERNAL(void) parse_dec(VM* vm, CompilerPPtr Cptr)
{
    C_flags_clear(*Cptr);

    if(C_match(*Cptr, TOK_VAR)) {
        parse_decvar(vm, Cptr);
    } else if(C_match(*Cptr, TOK_FIXED)) {
        parse_decvar_fixed(vm, Cptr);
    } else {
        parse_stm(vm, Cptr);
    }

    if(C_flag_is(*Cptr, PANIC_BIT)) {
        Parser_sync(*Cptr);
    }
}

SK_INTERNAL(force_inline void) C_initialize_local(Compiler* C, VM* vm)
{
    Local*     local      = &C->locals[C->llen - 1]; /* Safe to decrement unsigned int */
    Value      identifier = Token_into_stringval(vm, &local->token);
    HashTable* scope_set  = &C->ldefs.data[C->depth - 1];

    HashTable_insert(scope_set, identifier, NUMBER_VAL(C->llen - 1));
}

SK_INTERNAL(void)
parse_decvar(VM* vm, CompilerPPtr Cptr)
{
    if(C_match(*Cptr, TOK_FIXED)) {
        C_flag_set(*Cptr, FIXED_BIT);
    }

    Int index = parse_varname(vm, Cptr, "Expect variable name.");

    if(C_match(*Cptr, TOK_EQUAL)) {
        parse_expr(vm, Cptr);
    } else {
        C_emit_byte(*Cptr, OP_NIL);
    }

    C_expect(*Cptr, TOK_SEMICOLON, "Expect ';' after variable declaration.");

    // We declared local variable
    if((*Cptr)->depth > 0) {
        // now define/initialize it
        C_initialize_local(*Cptr, vm);
        return;
    }

    // We defined/initialized global variable instead
    C_emit_op(*Cptr, GET_OP_TYPE(index, OP_DEFINE_GLOBAL), index);
}

SK_INTERNAL(void) C_new_local(CompilerPPtr Cptr)
{
    Compiler* C = *Cptr;

    if(unlikely(C->llen >= C->lcap)) {
        if(unlikely(C->llen >= MIN(VM_STACK_MAX, LOCAL_STACK_MAX))) {
            C_error(C, "Too many variables defined in function.");
            return;
        }
        C_grow_stack(Cptr);
    }

    Local* local = &C->locals[C->llen++];
    local->token = C->parser.previous;
    local->flags = ((C_flags(C) >> 8) & 0xff);
}

SK_INTERNAL(force_inline bool) Identifier_eq(Token* left, Token* right)
{
    return (left->len == right->len) &&
           (memcmp(left->start, right->start, left->len) == 0);
}

SK_INTERNAL(bool) C_local_is_unique(Compiler* C, VM* vm)
{
    Value identifier = Token_into_stringval(vm, &C->parser.previous);
    // @FIX: Return pointer instead of a value in _index method
    HashTable scope_set = HashTableArray_index(&C->ldefs, C->depth - 1);
    return !HashTable_get(&scope_set, identifier, NULL);
}

SK_INTERNAL(void) C_make_local(CompilerPPtr Cptr, VM* vm)
{
    Compiler* C = *Cptr;
    if(!C_local_is_unique(C, vm)) {
        C_error(C, "Redefinition of local variable.");
    }
    C_new_local(Cptr);
}

SK_INTERNAL(int64_t) C_make_global(Compiler* C, VM* vm, bool fixed)
{
    Value identifier = Token_into_stringval(vm, &C->parser.previous);
    return make_constant_identifier(vm, identifier, fixed);
}

SK_INTERNAL(UInt) Global_idx(Compiler* C, VM* vm)
{
    Value idx;
    Value identifier = Token_into_stringval(vm, &C->parser.previous);
    if(!HashTable_get(&vm->global_ids, identifier, &idx)) {
        C_error(C, "Undefined variable.");
        return 0;
    }
    return (UInt)AS_NUMBER(idx);
}

SK_INTERNAL(UInt)
parse_varname(VM* vm, CompilerPPtr Cptr, const char* errmsg)
{
    C_expect(*Cptr, TOK_IDENTIFIER, errmsg);
    // If local scope make local variable
    if((*Cptr)->depth > 0) {
        C_make_local(Cptr, vm);
        return 0;
    }

    // Otherwise make global variable
    return C_make_global(*Cptr, vm, C_flag_is(*Cptr, FIXED_BIT));
}

SK_INTERNAL(force_inline void) C_start_scope(Compiler* C)
{
    HashTable scope_set;
    HashTable_init(&scope_set);

    if(unlikely(C->depth >= UINT32_MAX - 1)) {
        C_error(C, "Scope depth limit reached.");
    }
    C->depth++;
    HashTableArray_push(&C->ldefs, scope_set);
}

SK_INTERNAL(force_inline void) C_end_scope(Compiler* C)
{
    C->depth--;
    HashTableArray_pop(&C->ldefs);
    UInt popn = C->ldefs.data[C->depth].len;
    C->llen   -= popn;
    C_emit_op(C, OP_POPN, popn);
}

SK_INTERNAL(force_inline void)
C_patch_jmp(Compiler* C, VM* vm, UInt jmp_offset)
{
    UInt offset = code_offset() - jmp_offset - 3;

    if(unlikely(offset >= UINT24_MAX)) {
        C_error(C, "Too much code to jump over.");
    }

    PUT_BYTES3(&current_chunk()->code.data[jmp_offset], offset);
}

SK_INTERNAL(void) parse_stm_switch(VM* vm, CompilerPPtr Cptr)
{
    UIntArray patches; /* Holds all jump patches for each case */
    UIntArray_init(&patches);

    C_expect(*Cptr, TOK_LPAREN, "Expect '(' after 'switch'.");
    parse_expr(vm, Cptr);
    C_expect(*Cptr, TOK_RPAREN, "Expect ')' after condition.");

    C_expect(*Cptr, TOK_LBRACE, "Expect '{' after ')'.");

    bool dflt = false; /* Set if 'default' is parsed */

    while(C_match(*Cptr, TOK_CASE) || C_match(*Cptr, TOK_DEFAULT)) {
        Int case_end = -1;
        if((*Cptr)->parser.previous.type == TOK_CASE) {
            parse_expr(vm, Cptr);
            C_emit_byte(*Cptr, OP_EQ);
            C_expect(*Cptr, TOK_COLON, "Expect ':' after 'case'.");

            case_end = C_emit_jmp(*Cptr, vm, OP_JMP_IF_FALSE_AND_POP);
            parse_stm(vm, Cptr);

        } else if(!dflt) {
            dflt = true;
            C_expect(*Cptr, TOK_COLON, "Expect ':' after 'default'.");
            parse_stm(vm, Cptr);
        } else {
            C_error(*Cptr, "Multiple 'default' labels in a single 'switch'.");
        }

        UIntArray_push(&patches, C_emit_jmp(*Cptr, vm, OP_JMP_AND_POP));
        if(case_end != -1) {
            C_patch_jmp(*Cptr, vm, case_end);
        }
    }

    C_expect(*Cptr, TOK_RBRACE, "Expect '}'.");

    if(patches.len > 0) {
        while(patches.len) {
            C_patch_jmp(*Cptr, vm, UIntArray_pop(&patches));
        }
    } else {
        C_emit_byte(*Cptr, OP_POP);
    }
}

SK_INTERNAL(void) parse_stm_if(VM* vm, CompilerPPtr Cptr)
{
    C_expect(*Cptr, TOK_LPAREN, "Expect '(' after 'if'.");
    parse_expr(vm, Cptr); /* Parse conditional */
    C_expect(*Cptr, TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional jump instruction */
    UInt iffalse_jmp = C_emit_jmp(*Cptr, vm, OP_JMP_IF_FALSE_AND_POP);

    parse_stm(vm, Cptr); /* Parse the code in this branch */

    /* Prevent fall-through if 'else' exists. */
    UInt iftrue_jmp = C_emit_jmp(*Cptr, vm, OP_JMP);

    C_patch_jmp(*Cptr, vm, iffalse_jmp); /* End of 'if' (maybe start of else) */

    if(C_match(*Cptr, TOK_ELSE)) {
        parse_stm(vm, Cptr);                /* Parse the else branch */
        C_patch_jmp(*Cptr, vm, iftrue_jmp); /* End of else branch */
    }
}

SK_INTERNAL(void) parse_and(VM* vm, CompilerPPtr Cptr)
{
    // @FIX: Make jump if false and pop into a single instruction
    UInt jump = C_emit_jmp(*Cptr, vm, OP_JMP_IF_FALSE_OR_POP);
    parse_precedence(vm, Cptr, PREC_AND); /* Parse right side */
    C_patch_jmp(*Cptr, vm, jump);         /* @TODO: POP after and before jmp */
}

SK_INTERNAL(void) parse_or(VM* vm, CompilerPPtr Cptr)
{
    // @FIX: Make a jump if true (new instruction) and pop into a single
    // instruction
    UInt else_jmp = C_emit_jmp(*Cptr, vm, OP_JMP_IF_FALSE_AND_POP);
    UInt end_jmp  = C_emit_jmp(*Cptr, vm, OP_JMP);

    C_patch_jmp(*Cptr, vm, else_jmp); /* @TODO: POP after jmp */

    parse_precedence(vm, Cptr, PREC_OR);
    C_patch_jmp(*Cptr, vm, end_jmp);
}

SK_INTERNAL(void) parse_stm_while(VM* vm, CompilerPPtr Cptr)
{
    C_flag_set(*Cptr, LOOP_BIT);
    UInt loop_start = current_chunk()->code.len;
    C_expect(*Cptr, TOK_LPAREN, "Expect '(' after 'while'.");
    parse_expr(vm, Cptr);
    C_expect(*Cptr, TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional exit jump */
    UInt end_jmp = C_emit_jmp(*Cptr, vm, OP_JMP_IF_FALSE_AND_POP);
    parse_stm(vm, Cptr);            /* Parse (loop 'body') statement */
    C_emit_loop(*Cptr, loop_start); /* Jump to the start of the loop */

    C_patch_jmp(*Cptr, vm, end_jmp); /* Set loop exit offset */
}

SK_INTERNAL(void) parse_stm_for(VM* vm, CompilerPPtr Cptr)
{
    C_start_scope(*Cptr);
    /*----------- INITIALIZER -------------*/
    C_expect(*Cptr, TOK_LPAREN, "Expect '(' after 'for'.");
    if(C_match(*Cptr, TOK_SEMICOLON)) {
        // No initializer
    } else if(C_match(*Cptr, TOK_VAR)) {
        parse_decvar(vm, Cptr);
    } else if(C_match(*Cptr, TOK_FIXED)) {
        parse_decvar_fixed(vm, Cptr);
    } else {
        parse_stm_expr(vm, Cptr);
    }
    /*--------- END OF INITIALIZER -----------*/

    /*----------- CONDITION -------------*/
    UInt loop_start = current_chunk()->code.len;
    Int  loop_end   = -1;
    if(!C_match(*Cptr, TOK_SEMICOLON)) {
        parse_expr(vm, Cptr);
        C_expect(*Cptr, TOK_SEMICOLON, "Expect ';' (condition).");

        loop_end = C_emit_jmp(*Cptr, vm, OP_JMP_IF_FALSE_AND_POP);
    }
    /*--------- END OF CONDITION -----------*/

    /*----------- INCREMENT -------------*/
    if(!C_match(*Cptr, TOK_SEMICOLON)) {
        UInt body_start      = C_emit_jmp(*Cptr, vm, OP_JMP);
        UInt increment_start = current_chunk()->code.len;
        parse_expr(vm, Cptr);
        C_emit_byte(*Cptr, OP_POP);
        C_expect(*Cptr, TOK_RPAREN, "Expect ')' after last for-loop clause.");

        C_emit_loop(*Cptr, loop_start);
        loop_start = increment_start; /* If there is increment update the loop start */
        C_patch_jmp(*Cptr, vm, body_start);
    }
    /*--------- END OF INCREMENT -----------*/

    parse_stm(vm, Cptr);
    C_emit_loop(*Cptr, loop_start);

    if(loop_end != -1) {
        C_patch_jmp(*Cptr, vm, loop_end);
    }

    Compiler* C = *Cptr;
    C_end_scope(C);
}

SK_INTERNAL(void) parse_stm(VM* vm, CompilerPPtr Cptr)
{
    if(C_match(*Cptr, TOK_PRINT)) {
        parse_stm_print(vm, Cptr);
    } else if(C_match(*Cptr, TOK_WHILE)) {
        parse_stm_while(vm, Cptr);
    } else if(C_match(*Cptr, TOK_FOR)) {
        parse_stm_for(vm, Cptr);
    } else if(C_match(*Cptr, TOK_IF)) {
        parse_stm_if(vm, Cptr);
    } else if(C_match(*Cptr, TOK_SWITCH)) {
        parse_stm_switch(vm, Cptr);
    } else if(C_match(*Cptr, TOK_LBRACE)) {
        parse_stm_block(vm, Cptr);
    } else {
        parse_stm_expr(vm, Cptr);
    }
}

SK_INTERNAL(void) parse_stm_block(VM* vm, CompilerPPtr Cptr)
{
    Compiler* C = *Cptr;
    C_start_scope(C);

    while(!C_check(*Cptr, TOK_RBRACE) && !C_check(*Cptr, TOK_EOF)) {
        parse_dec(vm, Cptr);
    }

    C_expect(*Cptr, TOK_RBRACE, "Expect '}' after block.");
    C_end_scope(C);
}

SK_INTERNAL(force_inline void)
parse_stm_print(VM* vm, CompilerPPtr Cptr)
{
    parse_expr(vm, Cptr);
    C_expect(*Cptr, TOK_SEMICOLON, "Expect ';' after value");
    C_emit_byte(*Cptr, OP_PRINT);
}

SK_INTERNAL(force_inline void)
parse_number(unused VM* _, CompilerPPtr Cptr)
{
    Compiler* C        = *Cptr;
    double    constant = strtod(C->parser.previous.start, NULL);
    UInt      idx      = C_make_const(C, NUMBER_VAL(constant));
    C_emit_op(C, GET_OP_TYPE(idx, OP_CONST), idx);
}

SK_INTERNAL(force_inline Int) Local_idx(Compiler* C, VM* vm, const Token* name)
{
    Value index      = NUMBER_VAL(-1);
    Value identifier = Token_into_stringval(vm, name);
    for(Int i = 0; i < C->ldefs.len; i++) {
        HashTable* scope_set = &C->ldefs.data[i];
        if(HashTable_get(scope_set, identifier, &index)) {
            return (Int)AS_NUMBER(index);
        }
    }
    return -1;
}

SK_INTERNAL(force_inline void) parse_variable(VM* vm, CompilerPPtr Cptr)
{
    const Token* name = &(*Cptr)->parser.previous;
    OpCode       setop, getop;
    Int          idx   = Local_idx(*Cptr, vm, name);
    int16_t      flags = -1;

    if(idx != -1) {
        Local* var = &(*Cptr)->locals[idx];
        flags      = Var_flags(var);
        setop      = GET_OP_TYPE(idx, OP_SET_LOCAL);
        getop      = GET_OP_TYPE(idx, OP_GET_LOCAL);
    } else {
        idx   = Global_idx(*Cptr, vm);
        setop = GET_OP_TYPE(idx, OP_SET_GLOBAL);
        getop = GET_OP_TYPE(idx, OP_GET_GLOBAL);
    }

    if(C_flag_is(*Cptr, ASSIGN_BIT) && C_match(*Cptr, TOK_EQUAL)) {
        /* In case this is local variable statically check for mutability */
        if(flags != -1 && BIT_CHECK(flags, VAR_FIXED_BIT)) {
            C_error(*Cptr, "Can't assign to variable defined as 'fixed'.");
        }
        parse_expr(vm, Cptr);
        C_emit_op(*Cptr, setop, idx);
    } else {
        C_emit_op(*Cptr, getop, idx);
    }
}

SK_INTERNAL(force_inline void) parse_string(VM* vm, CompilerPPtr Cptr)
{
    Compiler*  C = *Cptr;
    ObjString* string =
        ObjString_from(vm, C->parser.previous.start + 1, C->parser.previous.len - 2);
    UInt idx = C_make_const(C, OBJ_VAL(string));
    C_emit_op(C, OP_CONST, idx);
}

/* This is the entry point to Pratt parsing */
SK_INTERNAL(force_inline void) parse_grouping(VM* vm, CompilerPPtr Cptr)
{
    parse_expr(vm, Cptr);
    C_expect(*Cptr, TOK_RPAREN, "Expect ')' after expression");
}

SK_INTERNAL(void) parse_unary(VM* vm, CompilerPPtr Cptr)
{
    TokenType type = (*Cptr)->parser.previous.type;
    parse_precedence(vm, Cptr, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            C_emit_byte(*Cptr, OP_NEG);
            break;
        case TOK_BANG:
            C_emit_byte(*Cptr, OP_NOT);
            break;
        default:
            unreachable;
            return;
    }
}

SK_INTERNAL(void) parse_binary(VM* vm, CompilerPPtr Cptr)
{
    TokenType        type = (*Cptr)->parser.previous.type;
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

    Compiler* C = *Cptr;
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
    C_expect(*Cptr, TOK_COLON, "Expect ': expr' (ternary conditional).");
    parse_expr(vm, Cptr);
}

SK_INTERNAL(force_inline void)
parse_literal(unused VM* _, CompilerPPtr Cptr)
{
    Compiler* C = *Cptr;
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
