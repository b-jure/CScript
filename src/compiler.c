#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "object.h"

#ifdef DEBUG_PRINT_CODE
    #include "debug.h"
#endif

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    Token previous;
    Token current;
    Byte  state;
} Parser;

/* Global parser */
static Parser parser;

/* Parser 'state' bits */
#define ERROR_BIT 0
#define PANIC_BIT 1

/* Parser 'state' bit manipulation function-like macro definitions. */
#define Parser_state_set_err()     BIT_SET(parser.state, ERROR_BIT)
#define Parser_state_clear_err()   BIT_CLEAR(parser.state, ERROR_BIT)
#define Parser_state_set_panic()   BIT_SET(parser.state, PANIC_BIT)
#define Parser_state_clear_panic() BIT_CLEAR(parser.state, PANIC_BIT)
#define Parser_state_iserr()       BIT_CHECK(parser.state, ERROR_BIT)
#define Parser_state_ispanic()     BIT_CHECK(parser.state, PANIC_BIT)
#define Parser_state_clear()       parser.state = 0

#define Parser_check(token_type) (parser.current.type == token_type)

/* Chunk being currently compiled, see 'compile()' */
static Chunk* compiling_chunk;
/* Returns currently stored chunk in 'compiling_chunk' global. */
SK_STATIC_INLINE(Chunk*) current_chunk();

/* Advances the parser until token type isn't 'TOK_ERROR',
 * invoking 'Parser_error' each time the token type is 'TOK_ERROR'. */
SK_STATIC_INLINE(void) Parser_advance(Scanner* scanner);
/* Invokes 'Parser_error_at' function with 'token' parameter being
 * the token in the 'parser.current' field. */
SK_STATIC_INLINE(void) Parser_error(const char* error);

/* Internal parse functions */
SK_STATIC_INLINE(void) parse_number(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void) parse_string(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(UInt) parse_varname(VM* vm, Scanner* scanner, const char* errmsg);
SK_STATIC_INLINE(void) parse_precedence(VM* vm, Scanner* scanner, Precedence prec);
SK_STATIC_INLINE(void) parse_grouping(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void) parse_ternarycond(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void) parse_expression(VM* vm, Scanner* scanner);
SK_STATIC_INLINE(void) parse_declaration(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void)
parse_declaration_variable(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void) parse_statement(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void) parse_statement_print(VM* vm, Scanner* scanner, bool can_assign);
SK_STATIC_INLINE(void) parse_variable(VM* vm, Scanner* scanner, bool can_assign);
// These below use local labels, can't inline them
static void parse_binary(VM* vm, Scanner* scanner, bool can_assign);
static void parse_unary(VM* vm, Scanner* scanner, bool can_assign);
static void parse_literal(VM* vm, Scanner* scanner, bool can_assign);

/*========================== EMIT =========================*/

SK_STATIC_INLINE(UInt) make_constant(Value constant)
{
    if(current_chunk()->constants.len <= MAXBYTES(3)) {
        return Chunk_make_constant(current_chunk(), constant);
    } else {
        Parser_error("Too many constants in one chunk.");
        return 0;
    }
}

SK_STATIC_INLINE(UInt) make_constant_identifier(VM* vm, Token* name)
{
    return make_constant(OBJ_VAL(ObjString_from(vm, name->start, name->len)));
}

SK_STATIC_INLINE(void) emit_byte(Byte byte)
{
    Chunk_write(current_chunk(), byte, parser.previous.line);
}

SK_STATIC_INLINE(void) emit_return(void)
{
    emit_byte(OP_RET);
}

#define GET_OP_TYPE(idx, op) (idx <= UINT8_MAX) ? op : op##L

SK_STATIC_INLINE(void) emit_constant(Value constant)
{
    UInt   idx  = make_constant(constant);
    OpCode code = GET_OP_TYPE(idx, OP_CONST);
    Chunk_write_codewidx(current_chunk(), code, idx, parser.previous.line);
}

SK_STATIC_INLINE(void) emit_global(UInt idx)
{
    OpCode code = GET_OP_TYPE(idx, OP_DEFINE_GLOBAL);
    Chunk_write_codewidx(current_chunk(), code, idx, parser.previous.line);
}

SK_STATIC_INLINE(void) emit_global_get(UInt idx)
{
    OpCode code = GET_OP_TYPE(idx, OP_GET_GLOBAL);
    Chunk_write_codewidx(current_chunk(), code, idx, parser.previous.line);
}

SK_STATIC_INLINE(void) emit_global_set(UInt idx)
{
    OpCode code = GET_OP_TYPE(idx, OP_SET_GLOBAL);
    Chunk_write_codewidx(current_chunk(), code, idx, parser.previous.line);
}

#undef GET_OP_TYPE

/*========================= PARSER ========================*/

SK_STATIC_INLINE(void) Parser_init(Scanner* scanner)
{
    Parser_state_clear();
    Parser_advance(scanner);
}

static void Parser_error_at(Token* token, const char* error)
{
    if(Parser_state_ispanic()) {
        return;
    }

    Parser_state_set_panic();
    fprintf(stderr, "[line: %u] Error", token->line);

    if(token->type == TOK_EOF) {
        fprintf(stderr, " at end");
    } else if(token->type != TOK_ERROR) {
        fprintf(stderr, " at '%.*s'", token->len, token->start);
    }

    fprintf(stderr, ": %s\n", error);
    Parser_state_set_err();
}

static void Parser_error(const char* error)
{
    Parser_error_at(&parser.current, error);
}

SK_STATIC_INLINE(void) Parser_advance(Scanner* scanner)
{
    parser.previous = parser.current;

    while(true) {
        parser.current = Scanner_scan(scanner);
        if(parser.current.type != TOK_ERROR) {
            break;
        }

        Parser_error(parser.current.start);
    }
}

static void Parser_sync(Scanner* scanner)
{
    // @ Create precomputed goto table
    Parser_state_clear_panic();

    while(parser.current.type != TOK_EOF) {
        if(parser.previous.type == TOK_SEMICOLON) {
            return;
        }

        switch(parser.current.type) {
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
                Parser_advance(scanner);
                break;
        }
    }
}

SK_STATIC_INLINE(void) Parser_expect(Scanner* scanner, TokenType type, const char* error)
{
    if(Parser_check(type)) {
        Parser_advance(scanner);
        return;
    }
    Parser_error(error);
}

SK_STATIC_INLINE(bool) Parser_match(Scanner* scanner, TokenType type)
{
    if(!Parser_check(type)) {
        return false;
    }
    Parser_advance(scanner);
    return true;
}

SK_STATIC_INLINE(void) compile_end(void)
{
    emit_return();
#ifdef DEBUG_PRINT_CODE
    if(!Parser_state_iserr()) {
        Chunk_debug(current_chunk(), "code");
    }
#endif
}

bool compile(VM* vm, const char* source, Chunk* chunk)
{
    Scanner scanner = Scanner_new(source);
    compiling_chunk = chunk;
    Parser_init(&scanner);

    while(!Parser_match(&scanner, TOK_EOF)) {
        parse_declaration(vm, &scanner, true);
    }

    compile_end();
    return !Parser_state_iserr();
}

SK_STATIC_INLINE(Chunk) * current_chunk()
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

SK_STATIC_INLINE(void)
parse_statement_expression(VM* vm, Scanner* scanner, bool can_assign)
{
    parse_expression(vm, scanner);
    Parser_expect(scanner, TOK_SEMICOLON, "Expect ';' after expression.");
    emit_byte(OP_POP);
}

SK_STATIC_INLINE(void) parse_expression(VM* vm, Scanner* scanner)
{
    parse_precedence(vm, scanner, PREC_ASSIGNMENT);
}

SK_STATIC_INLINE(void) parse_precedence(VM* vm, Scanner* scanner, Precedence prec)
{
    Parser_advance(scanner);
    ParseFn prefix_fn = rules[parser.previous.type].prefix;
    if(prefix_fn == NULL) {
        Parser_error("Expect expression.");
        return;
    }

    bool can_assign = prec <= PREC_ASSIGNMENT;
    /* Parse unary operator (prefix) or a literal */
    prefix_fn(vm, scanner, can_assign);

    /* Parse binary operator (inifix) with higher or equal precedence if any */
    while(prec <= rules[parser.current.type].precedence) {
        Parser_advance(scanner);
        ParseFn infix_fn = rules[parser.previous.type].infix;
        infix_fn(vm, scanner, can_assign);
    }

    if(can_assign && Parser_match(scanner, TOK_EQUAL)) {
        Parser_error("Invalid assignment target.");
    }
}

SK_STATIC_INLINE(void) parse_declaration(VM* vm, Scanner* scanner, bool can_assign)
{
    if(Parser_match(scanner, TOK_VAR)) {
        parse_declaration_variable(vm, scanner, can_assign);
    } else {
        parse_statement(vm, scanner, can_assign);
    }

    if(Parser_state_ispanic()) {
        Parser_sync(scanner);
    }
}

SK_STATIC_INLINE(void)
parse_declaration_variable(VM* vm, Scanner* scanner, bool can_assign)
{
    UInt index = parse_varname(vm, scanner, "Expect variable name.");

    if(Parser_match(scanner, TOK_EQUAL)) {
        parse_expression(vm, scanner);
    } else {
        emit_byte(OP_NIL);
    }

    Parser_expect(scanner, TOK_SEMICOLON, "Expect ';' after variable declaration.");
    emit_global(index);
}

SK_STATIC_INLINE(UInt) parse_varname(VM* vm, Scanner* scanner, const char* errmsg)
{
    Parser_expect(scanner, TOK_IDENTIFIER, errmsg);
    return make_constant_identifier(vm, &parser.previous);
}

SK_STATIC_INLINE(void) parse_statement(VM* vm, Scanner* scanner, bool can_assign)
{
    if(Parser_match(scanner, TOK_PRINT)) {
        parse_statement_print(vm, scanner, can_assign);
    } else {
        parse_statement_expression(vm, scanner, can_assign);
    }
}

SK_STATIC_INLINE(void) parse_statement_print(VM* vm, Scanner* scanner, bool can_assign)
{
    parse_expression(vm, scanner);
    Parser_expect(scanner, TOK_SEMICOLON, "Expect ';' after value");
    emit_byte(OP_PRINT);
}

SK_STATIC_INLINE(void) parse_number(_unused VM* _, _unused Scanner* __, bool can_assign)
{
    double constant = strtod(parser.previous.start, NULL);
    emit_constant(NUMBER_VAL(constant));
}

/* Fetches or creates identifier constant inside a current chunk with the
 * name/identifier stored inside a token 'name'. */
SK_STATIC_INLINE(void) varname(VM* vm, Scanner* scanner, Token* name, bool can_assign)
{
    UInt idx = make_constant_identifier(vm, name);

    if(can_assign && Parser_match(scanner, TOK_EQUAL)) {
        parse_expression(vm, scanner);
        emit_global_set(idx);
    } else {
        emit_global_get(idx);
    }
}

SK_STATIC_INLINE(void) parse_variable(VM* vm, Scanner* scanner, bool can_assign)
{
    varname(vm, scanner, &parser.previous, can_assign);
}

SK_STATIC_INLINE(void) parse_string(VM* vm, _unused Scanner* _, _unused bool __)
{
    emit_constant(
        OBJ_VAL(ObjString_from(vm, parser.previous.start + 1, parser.previous.len - 2)));
}

/* This is the entry point to Pratt parsing */
SK_STATIC_INLINE(void) parse_grouping(VM* vm, Scanner* scanner, bool can_assign)
{
    parse_expression(vm, scanner);
    Parser_expect(scanner, TOK_RPAREN, "Expect ')' after expression");
}

static void parse_unary(VM* vm, Scanner* scanner, _unused bool _)
{
    TokenType type = parser.previous.type;
    parse_precedence(vm, scanner, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            emit_byte(OP_NEG);
            break;
        case TOK_BANG:
            emit_byte(OP_NOT);
            break;
        default:
            _unreachable;
            return;
    }
}

static void parse_binary(VM* vm, Scanner* scanner, _unused bool _)
{
    TokenType        type = parser.previous.type;
    const ParseRule* rule = &rules[type];
    parse_precedence(vm, scanner, rule->precedence + 1);

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

    goto* jump_table[type];

minus:
    emit_byte(OP_SUB);
    return;
plus:
    emit_byte(OP_ADD);
    return;
slash:
    emit_byte(OP_DIV);
    return;
star:
    emit_byte(OP_MUL);
    return;
neq:
    emit_byte(OP_NOT_EQUAL);
    return;
eq:
    emit_byte(OP_EQUAL);
    return;
gt:
    emit_byte(OP_GREATER);
    return;
gteq:
    emit_byte(OP_GREATER_EQUAL);
    return;
lt:
    emit_byte(OP_LESS);
    return;
lteq:
    emit_byte(OP_LESS_EQUAL);
    return;

    _unreachable;
#else
    switch(type) {
        case TOK_MINUS:
            emit_byte(OP_SUB);
            break;
        case TOK_PLUS:
            emit_byte(OP_ADD);
            break;
        case TOK_SLASH:
            emit_byte(OP_DIV);
            break;
        case TOK_STAR:
            emit_byte(OP_MUL);
            break;
        case TOK_BANG_EQUAL:
            emit_byte(OP_NOT_EQUAL);
            break;
        case TOK_EQUAL_EQUAL:
            emit_byte(OP_EQUAL);
            break;
        case TOK_GREATER:
            emit_byte(OP_GREATER);
            break;
        case TOK_GREATER_EQUAL:
            emit_byte(OP_GREATER_EQUAL);
            break;
        case TOK_LESS:
            emit_byte(OP_LESS);
            break;
        case TOK_LESS_EQUAL:
            emit_byte(OP_LESS_EQUAL);
            break;
        default:
            _unreachable;
            return;
    }
#endif
}

SK_STATIC_INLINE(void) parse_ternarycond(VM* vm, Scanner* scanner, _unused bool _)
{
    parse_expression(vm, scanner);
    Parser_expect(
        scanner,
        TOK_COLON,
        "Expect ': \033[3mexpr\033[0m' (ternary conditional).");
    parse_expression(vm, scanner);
}

static void parse_literal(_unused VM* _, _unused Scanner* __, _unused bool ___)
{
    switch(parser.previous.type) {
        case TOK_TRUE:
            emit_byte(OP_TRUE);
            break;
        case TOK_FALSE:
            emit_byte(OP_FALSE);
            break;
        case TOK_NIL:
            emit_byte(OP_NIL);
            break;
        default:
            _unreachable;
            return;
    }
}
