#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "object.h"
#include "scanner.h"
#include <stdint.h>

#ifdef DEBUG_PRINT_CODE
    #include "debug.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#define GET_OP_TYPE(idx, op) (idx <= UINT8_MAX) ? op : op##L

typedef struct {
    Scanner scanner;
    Token   previous;
    Token   current;
    Byte    state;
} Parser;

typedef struct {
    Token token;
    UInt  depth;
} Local;

#define LOCAL_STACK_MAX  MAXBYTES(3) + 1
#define SHORT_STACK_SIZE (UINT8_MAX + 1)

#define ALLOC_COMPILER() MALLOC(sizeof(Compiler) + (SHORT_STACK_SIZE * sizeof(Local)))

#define GROW_LOCAL_STACK(ptr, oldcap, newcap)                                            \
    (Compiler*)REALLOC(                                                                  \
        ptr,                                                                             \
        sizeof(Compiler) + (oldcap * sizeof(Local)),                                     \
        sizeof(Compiler) + (newcap * sizeof(Local)))

typedef struct {
    Parser parser;   /* Grammar parser */
    UInt   depth;    /* Scope depth */
    UInt   llen;     /* Locals count */
    UInt   lcap;     /* Locals array capacity */
    Local  locals[]; /* Locals array (up to 24-bit [LOCAL_STACK_MAX]) */
} Compiler;

typedef void (*ParseFn)(VM*, Compiler**, bool);

typedef struct {
    ParseFn    prefix;
    ParseFn    infix;
    Precedence precedence;
} ParseRule;

/* Chunk being currently compiled, see 'compile()' */
static Chunk* compiling_chunk;
/* Returns currently stored chunk in 'compiling_chunk' global. */
SK_INTERNAL(Chunk*) current_chunk();

/* Parser 'state' bits */
#define ERROR_BIT 0
#define PANIC_BIT 1
/* Compiler (parser) 'state' bit manipulation function-like macro definitions.
 * Parser state is represented as a single byte, each bit defining state.
 * For now least significant bit is 'ERROR' bit, bit right after it is 'PANIC' bit. */
#define Compiler_set_error(compiler)   BIT_SET((compiler)->parser.state, ERROR_BIT)
#define Compiler_clear_error(compiler) BIT_CLEAR((compiler)->parser.state, ERROR_BIT)
#define Compiler_set_panic(compiler)   BIT_SET((compiler)->parser.state, PANIC_BIT)
#define Compiler_clear_panic(compiler) BIT_CLEAR((compiler)->parser.state, PANIC_BIT)
#define Compiler_is_error(compiler)    BIT_CHECK((compiler)->parser.state, ERROR_BIT)
#define Compiler_is_panic(compiler)    BIT_CHECK((compiler)->parser.state, PANIC_BIT)
#define Compiler_clear_state(compiler) (compiler)->parser.state = 0
/* Checks for equality between the 'token_type' and the current parser token */
#define Compiler_check(compiler, token_type)                                             \
    ((compiler)->parser.current.type == token_type)

SK_INTERNAL(void) Compiler_advance(Compiler* compiler);
SK_INTERNAL(void) Compiler_error(Compiler* compiler, const char* error);
SK_INTERNAL(void) Parser_init(Parser* parser, const char* source);
/* Internal parse functions */
SK_INTERNAL(void) parse_number(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_string(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(UInt) parse_varname(VM* vm, Compiler** Cptr, const char* errmsg);
SK_INTERNAL(void) parse_precedence(VM* vm, Compiler** Cptr, Precedence prec);
SK_INTERNAL(void) parse_grouping(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_ternarycond(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_expression(VM* vm, Compiler** Cptr);
SK_INTERNAL(void) parse_declaration(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void)
parse_declaration_variable(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_block(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_statement(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_statement_print(VM* vm, Compiler** Cptr, bool can_assign);
SK_INTERNAL(void) parse_variable(VM* vm, Compiler** Cptr, bool can_assign);
// These below use local labels, can't inline them
static void parse_binary(VM* vm, Compiler** Cptr, bool can_assign);
static void parse_unary(VM* vm, Compiler** Cptr, bool can_assign);
static void parse_literal(VM* vm, Compiler** Cptr, bool can_assign);

/*======================== COMPILER =======================*/

SK_INTERNAL(Compiler*) Compiler_new(const char* source)
{
    Compiler* C = MALLOC(sizeof(Compiler) + ((UINT8_MAX + 1) * sizeof(Local)));
    Parser_init(&C->parser, source);
    C->llen  = 0;
    C->lcap  = UINT8_MAX + 1;
    C->depth = 0;
    Compiler_advance(C);
    return C;
}

SK_INTERNAL(force_inline void) Compiler_grow_stack(Compiler** Cptr)
{
    Compiler* C      = *Cptr;
    UInt      oldcap = C->lcap;

    C->lcap = MIN(GROW_ARRAY_CAPACITY(oldcap), MAXBYTES(3));
    C       = GROW_LOCAL_STACK(C, oldcap, C->lcap);
}

/*========================== EMIT =========================*/

SK_INTERNAL(force_inline UInt) make_constant(Compiler* compiler, Value constant)
{
    if(current_chunk()->constants.len <= MAXBYTES(3)) {
        return Chunk_make_constant(current_chunk(), constant);
    } else {
        Compiler_error(compiler, "Too many constants in one chunk.");
        return 0;
    }
}

SK_INTERNAL(force_inline UInt) make_constant_identifier(VM* vm, Token* name)
{
    Value index;
    Value identifier = OBJ_VAL(ObjString_from(vm, name->start, name->len));

    if(!HashTable_get(&vm->global_ids, identifier, &index)) {
        index = NUMBER_VAL((double)ValueArray_push(&vm->global_vals, UNDEFINED_VAL));
        HashTable_insert(&vm->global_ids, identifier, index);
    }

    return (UInt)AS_NUMBER(index);
}

SK_INTERNAL(force_inline void) emit_byte(Compiler* compiler, Byte byte)
{
    Chunk_write(current_chunk(), byte, compiler->parser.previous.line);
}

SK_INTERNAL(force_inline void) emit_return(Compiler* compiler)
{
    emit_byte(compiler, OP_RET);
}

SK_INTERNAL(force_inline void) emit_constant(Compiler* compiler, Value constant)
{
    UInt   idx  = make_constant(compiler, constant);
    OpCode code = GET_OP_TYPE(idx, OP_CONST);
    Chunk_write_codewparam(current_chunk(), code, idx, compiler->parser.previous.line);
}

SK_INTERNAL(force_inline void) emit_global(Compiler* compiler, UInt idx)
{
    // Do not define global if in local scope
    if(compiler->depth > 0) {
        return;
    }
    OpCode code = GET_OP_TYPE(idx, OP_DEFINE_GLOBAL);
    Chunk_write_codewparam(current_chunk(), code, idx, compiler->parser.previous.line);
}

SK_INTERNAL(force_inline void) emit_varop(Compiler* C, OpCode code, UInt idx)
{
    Chunk_write_codewparam(current_chunk(), code, idx, C->parser.previous.line);
}

SK_INTERNAL(force_inline void) emit_popn(Compiler* compiler, UInt n)
{
    Chunk_write_codewparam(current_chunk(), OP_POPN, n, compiler->parser.previous.line);
}

/*========================= PARSER ========================*/

SK_INTERNAL(force_inline void) Parser_init(Parser* parser, const char* source)
{
    parser->scanner = Scanner_new(source);
    parser->state   = 0;
}

static void Compiler_error_at(Compiler* compiler, Token* token, const char* error)
{
    if(Compiler_is_panic(compiler)) {
        return;
    }

    Compiler_set_panic(compiler);
    fprintf(stderr, "[line: %u] Error", token->line);

    if(token->type == TOK_EOF) {
        fprintf(stderr, " at end");
    } else if(token->type != TOK_ERROR) {
        fprintf(stderr, " at '%.*s'", token->len, token->start);
    }

    fprintf(stderr, ": %s\n", error);
    Compiler_set_error(compiler);
}

static void Compiler_error(Compiler* compiler, const char* error)
{
    Compiler_error_at(compiler, &compiler->parser.current, error);
}

SK_INTERNAL(void) Compiler_advance(Compiler* compiler)
{
    compiler->parser.previous = compiler->parser.current;

    while(true) {
        compiler->parser.current = Scanner_scan(&compiler->parser.scanner);
        if(compiler->parser.current.type != TOK_ERROR) {
            break;
        }

        Compiler_error(compiler, compiler->parser.current.start);
    }
}

static void Parser_sync(Compiler* compiler)
{
    // @ Create precomputed goto table
    Compiler_clear_panic(compiler);

    while(compiler->parser.current.type != TOK_EOF) {
        if(compiler->parser.previous.type == TOK_SEMICOLON) {
            return;
        }

        switch(compiler->parser.current.type) {
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
                Compiler_advance(compiler);
                break;
        }
    }
}

SK_INTERNAL(force_inline void)
Compiler_expect(Compiler* compiler, TokenType type, const char* error)
{
    if(Compiler_check(compiler, type)) {
        Compiler_advance(compiler);
        return;
    }
    Compiler_error(compiler, error);
}

SK_INTERNAL(force_inline bool) Compiler_match(Compiler* compiler, TokenType type)
{
    if(!Compiler_check(compiler, type)) {
        return false;
    }
    Compiler_advance(compiler);
    return true;
}

#ifndef DEBUG_TRACE_EXECUTION
SK_INTERNAL(force_inline void) compile_end(Compiler* compiler)
#else
SK_INTERNAL(force_inline void) compile_end(Compiler* compiler, VM* vm)
#endif
{
    emit_return(compiler);
#ifdef DEBUG_PRINT_CODE
    if(!Compiler_is_error(compiler)) {
        Chunk_debug(current_chunk(), "code", vm);
    }
#endif
}

bool compile(VM* vm, const char* source, Chunk* chunk)
{
    Compiler* compiler = Compiler_new(source);
    compiling_chunk    = chunk;

    while(!Compiler_match(compiler, TOK_EOF)) {
        parse_declaration(vm, &compiler, true);
    }

#ifndef DEBUG_TRACE_EXECUTION
    compile_end(&compiler);
#else
    compile_end(compiler, vm);
#endif
    return !Compiler_is_error(compiler);
}

SK_INTERNAL(Chunk) * current_chunk()
{
    return compiling_chunk;
}

/*========================== PARSE ========================
 * PP* (Pratt Parsing algorithm)
 *
 * Parsing rules table,
 * First and second column are function pointers to 'ParseFn',
 * these functions are responsible for parsing the actual expression and most are
 * recursive. First column parse function is used in case token is prefix, while second
 * column parse function is used in case token is inifx. Third column marks the
 * 'Precedence' of the token inside expression. */
static const ParseRule rules[] = {
    [TOK_LPAREN]        = {parse_grouping,             NULL,              PREC_NONE      },
    [TOK_RPAREN]        = {NULL,                       NULL,              PREC_NONE      },
    [TOK_LBRACE]        = {NULL,                       NULL,              PREC_NONE      },
    [TOK_RBRACE]        = {NULL,                       NULL,              PREC_NONE      },
    [TOK_COMMA]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_DOT]           = {NULL,                       NULL,              PREC_NONE      },
    [TOK_MINUS]         = {parse_unary,                parse_binary,      PREC_TERM      },
    [TOK_PLUS]          = {NULL,                       parse_binary,      PREC_TERM      },
    [TOK_COLON]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_SEMICOLON]     = {NULL,                       NULL,              PREC_NONE      },
    [TOK_SLASH]         = {NULL,                       parse_binary,      PREC_FACTOR    },
    [TOK_STAR]          = {NULL,                       parse_binary,      PREC_FACTOR    },
    [TOK_QMARK]         = {NULL,                       parse_ternarycond, PREC_TERNARY   },
    [TOK_BANG]          = {parse_unary,                NULL,              PREC_NONE      },
    [TOK_BANG_EQUAL]    = {NULL,                       parse_binary,      PREC_EQUALITY  },
    [TOK_EQUAL]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_EQUAL_EQUAL]   = {NULL,                       parse_binary,      PREC_EQUALITY  },
    [TOK_GREATER]       = {NULL,                       parse_binary,      PREC_COMPARISON},
    [TOK_GREATER_EQUAL] = {NULL,                       parse_binary,      PREC_COMPARISON},
    [TOK_LESS]          = {NULL,                       parse_binary,      PREC_COMPARISON},
    [TOK_LESS_EQUAL]    = {NULL,                       parse_binary,      PREC_COMPARISON},
    [TOK_IDENTIFIER]    = {parse_variable,             NULL,              PREC_NONE      },
    [TOK_STRING]        = {parse_string,               NULL,              PREC_NONE      },
    [TOK_NUMBER]        = {parse_number,               NULL,              PREC_NONE      },
    [TOK_AND]           = {NULL,                       NULL,              PREC_NONE      },
    [TOK_CLASS]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_ELSE]          = {NULL,                       NULL,              PREC_NONE      },
    [TOK_FALSE]         = {parse_literal,              NULL,              PREC_NONE      },
    [TOK_FOR]           = {NULL,                       NULL,              PREC_NONE      },
    [TOK_FN]            = {NULL,                       NULL,              PREC_NONE      },
    [TOK_IF]            = {NULL,                       NULL,              PREC_NONE      },
    [TOK_NIL]           = {parse_literal,              NULL,              PREC_NONE      },
    [TOK_OR]            = {NULL,                       NULL,              PREC_NONE      },
    [TOK_PRINT]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_RETURN]        = {NULL,                       NULL,              PREC_NONE      },
    [TOK_SUPER]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_SELF]          = {NULL,                       NULL,              PREC_NONE      },
    [TOK_TRUE]          = {parse_literal,              NULL,              PREC_NONE      },
    [TOK_VAR]           = {parse_declaration_variable, NULL,              PREC_NONE      },
    [TOK_WHILE]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_ERROR]         = {NULL,                       NULL,              PREC_NONE      },
    [TOK_EOF]           = {NULL,                       NULL,              PREC_NONE      },
};

SK_INTERNAL(void)
parse_statement_expression(VM* vm, Compiler** Cptr, bool can_assign)
{
    parse_expression(vm, Cptr);
    Compiler_expect(*Cptr, TOK_SEMICOLON, "Expect ';' after expression.");
    emit_byte(*Cptr, OP_POP);
}

SK_INTERNAL(void) parse_expression(VM* vm, Compiler** Cptr)
{
    parse_precedence(vm, Cptr, PREC_ASSIGNMENT);
}

SK_INTERNAL(void) parse_precedence(VM* vm, Compiler** Cptr, Precedence prec)
{
    Compiler_advance(*Cptr);
    ParseFn prefix_fn = rules[(*Cptr)->parser.previous.type].prefix;
    if(prefix_fn == NULL) {
        Compiler_error(*Cptr, "Expect expression.");
        return;
    }

    bool can_assign = prec <= PREC_ASSIGNMENT;
    /* Parse unary operator (prefix) or a literal */
    prefix_fn(vm, Cptr, can_assign);

    /* Parse binary operator (inifix) with higher or equal precedence if any */
    while(prec <= rules[(*Cptr)->parser.current.type].precedence) {
        Compiler_advance(*Cptr);
        ParseFn infix_fn = rules[(*Cptr)->parser.previous.type].infix;
        infix_fn(vm, Cptr, can_assign);
    }

    if(can_assign && Compiler_match(*Cptr, TOK_EQUAL)) {
        Compiler_error(*Cptr, "Invalid assignment target.");
    }
}

SK_INTERNAL(void) parse_declaration(VM* vm, Compiler** Cptr, bool can_assign)
{
    if(Compiler_match(*Cptr, TOK_VAR)) {
        parse_declaration_variable(vm, Cptr, can_assign);
    } else {
        parse_statement(vm, Cptr, can_assign);
    }

    if(Compiler_is_panic(*Cptr)) {
        Parser_sync(*Cptr);
    }
}

SK_INTERNAL(void)
parse_declaration_variable(VM* vm, Compiler** Cptr, bool can_assign)
{
    UInt index = parse_varname(vm, Cptr, "Expect variable name.");

    if(Compiler_match(*Cptr, TOK_EQUAL)) {
        parse_expression(vm, Cptr);
    } else {
        emit_byte(*Cptr, OP_NIL);
    }

    Compiler_expect(*Cptr, TOK_SEMICOLON, "Expect ';' after variable declaration.");
    emit_global(*Cptr, index);
}

SK_INTERNAL(void) Compiler_local_new(Compiler** Cptr)
{
    Compiler* C = *Cptr;

    if(unlikely(C->llen >= C->lcap)) {
        if(unlikely(C->llen >= LOCAL_STACK_MAX)) {
            Compiler_error(C, "Too many variables defined in function.");
            return;
        }
        Compiler_grow_stack(Cptr);
        C = *Cptr;
    }

    Local* local = &C->locals[C->llen++];
    local->token = C->parser.current;
    local->depth = C->depth;
}

SK_INTERNAL(force_inline bool) Identifier_eq(Token* left, Token* right)
{
    return left->len == right->len && memcmp(left->start, right->start, left->len) == 0;
}

SK_INTERNAL(UInt) parse_varname(VM* vm, Compiler** Cptr, const char* errmsg)
{
    Compiler_expect(*Cptr, TOK_IDENTIFIER, errmsg);
    // If local scope make local variable
    if((*Cptr)->depth > 0) {
        Compiler* C    = *Cptr;
        Token*    name = &C->parser.previous;

        for(int64_t i = C->llen - 1; i >= 0; i--) {
            Local* local = &C->locals[i];
            if(local->depth != -1 && local->depth < C->depth) {
                break;
            }

            if(Identifier_eq(name, &local->token)) {
                Compiler_error(
                    C,
                    "Variable with the same name is already defined in this scope.");
            }
        }
        Compiler_local_new(Cptr);
        return 0;
    }
    // Otherwise make global variable
    return make_constant_identifier(vm, &(*Cptr)->parser.previous);
}

SK_INTERNAL(void) parse_statement(VM* vm, Compiler** Cptr, bool can_assign)
{
    if(Compiler_match(*Cptr, TOK_PRINT)) {
        parse_statement_print(vm, Cptr, can_assign);
    } else if(Compiler_match(*Cptr, TOK_LBRACE)) {
        Compiler* C = *Cptr;
        C->depth++; /* Start of scope */
        parse_block(vm, Cptr, can_assign);
        C = *Cptr;
        C->depth--; /* End of scope */
        UInt popped = 0;
        while(C->llen > 0 && C->locals[C->llen - 1].depth > C->depth) {
            C->llen--;
            popped++;
        }
        emit_popn(C, popped);
    } else {
        parse_statement_expression(vm, Cptr, can_assign);
    }
}

SK_INTERNAL(void) parse_block(VM* vm, Compiler** Cptr, bool can_assign)
{
    while(!Compiler_match(*Cptr, TOK_RBRACE) && !Compiler_match(*Cptr, TOK_EOF)) {
        parse_declaration(vm, Cptr, can_assign);
    }
    Compiler_expect(*Cptr, TOK_RBRACE, "Expect '}' after block.");
}

SK_INTERNAL(void) parse_statement_print(VM* vm, Compiler** Cptr, unused bool _)
{
    parse_expression(vm, Cptr);
    Compiler_expect(*Cptr, TOK_SEMICOLON, "Expect ';' after value");
    emit_byte(*Cptr, OP_PRINT);
}

SK_INTERNAL(void) parse_number(unused VM* _, Compiler** Cptr, unused bool __)
{
    Compiler* C        = *Cptr;
    double    constant = strtod(C->parser.previous.start, NULL);
    emit_constant(C, NUMBER_VAL(constant));
}

SK_INTERNAL(int64_t) Local_idx(Compiler* C, Token* name)
{
    for(UInt i = C->llen - 1; i >= 0; i--) {
        Local* local = &C->locals[i];
        if(Identifier_eq(&local->token, name)) {
            return i;
        }
    }
    return -1;
}

SK_INTERNAL(force_inline void) parse_variable(VM* vm, Compiler** Cptr, bool can_assign)
{
    Token* name = &(*Cptr)->parser.previous;
    OpCode setop, getop;
    UInt   idx = Local_idx(*Cptr, name);

    if(idx != -1) {
        setop = GET_OP_TYPE(idx, OP_SET_LOCAL);
        getop = GET_OP_TYPE(idx, OP_GET_LOCAL);
    } else {
        idx   = make_constant_identifier(vm, name);
        setop = GET_OP_TYPE(idx, OP_SET_GLOBAL);
        getop = GET_OP_TYPE(idx, OP_GET_GLOBAL);
    }

    if(can_assign && Compiler_match(*Cptr, TOK_EQUAL)) {
        parse_expression(vm, Cptr);
        emit_varop(*Cptr, setop, idx);
    } else {
        emit_varop(*Cptr, getop, idx);
    }
}

SK_INTERNAL(force_inline void) parse_string(VM* vm, Compiler** Cptr, unused bool _)
{
    Compiler* C = *Cptr;
    emit_constant(
        C,
        OBJ_VAL(ObjString_from(
            vm,
            C->parser.previous.start + 1,
            C->parser.previous.len - 2)));
}

/* This is the entry point to Pratt parsing */
SK_INTERNAL(force_inline void) parse_grouping(VM* vm, Compiler** Cptr, bool can_assign)
{
    parse_expression(vm, Cptr);
    Compiler_expect(*Cptr, TOK_RPAREN, "Expect ')' after expression");
}

static void parse_unary(VM* vm, Compiler** Cptr, unused bool _)
{
    TokenType type = (*Cptr)->parser.previous.type;
    parse_precedence(vm, Cptr, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            emit_byte(*Cptr, OP_NEG);
            break;
        case TOK_BANG:
            emit_byte(*Cptr, OP_NOT);
            break;
        default:
            unreachable;
            return;
    }
}

static void parse_binary(VM* vm, Compiler** Cptr, unused bool _)
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
        0,       /* TOK_ERROR */
        0,       /* TOK_EOF */
    };

    Compiler* C = *Cptr;
    goto*     jump_table[type];

minus:
    emit_byte(C, OP_SUB);
    return;
plus:
    emit_byte(C, OP_ADD);
    return;
slash:
    emit_byte(C, OP_DIV);
    return;
star:
    emit_byte(C, OP_MUL);
    return;
neq:
    emit_byte(C, OP_NOT_EQUAL);
    return;
eq:
    emit_byte(C, OP_EQUAL);
    return;
gt:
    emit_byte(C, OP_GREATER);
    return;
gteq:
    emit_byte(C, OP_GREATER_EQUAL);
    return;
lt:
    emit_byte(C, OP_LESS);
    return;
lteq:
    emit_byte(C, OP_LESS_EQUAL);
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

SK_INTERNAL(force_inline void) parse_ternarycond(VM* vm, Compiler** Cptr, unused bool _)
{
    parse_expression(vm, Cptr);
    Compiler_expect(*Cptr, TOK_COLON, "Expect ': expr' (ternary conditional).");
    parse_expression(vm, Cptr);
}

SK_INTERNAL(force_inline void)
parse_literal(unused VM* _, Compiler** Cptr, unused bool __)
{
    Compiler* C = *Cptr;
    switch(C->parser.previous.type) {
        case TOK_TRUE:
            emit_byte(C, OP_TRUE);
            break;
        case TOK_FALSE:
            emit_byte(C, OP_FALSE);
            break;
        case TOK_NIL:
            emit_byte(C, OP_NIL);
            break;
        default:
            unreachable;
            return;
    }
}
