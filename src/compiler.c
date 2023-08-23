#include "compiler.h"

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
#define Parser_state_set_err()   BIT_SET(parser.state, ERROR_BIT)
#define Parser_state_set_panic() BIT_SET(parser.state, PANIC_BIT)
#define Parser_state_iserr()     BIT_CHECK(parser.state, ERROR_BIT)
#define Parser_state_ispanic()   BIT_CHECK(parser.state, PANIC_BIT)
#define Parser_state_clear()     parser.state = 0

/* Chunk being currently compiled, see 'compile()' */
static Chunk* compiling_chunk;

/* Returns currently stored chunk in 'compiling_chunk' global. */
static Chunk* current_chunk();
/* Inits the parser */
static void Parser_init(Scanner* scanner);
/* Helper function for invoking parser errors. */
static void Parser_error_at(Token* token, const char* error);
/* Invokes 'Parser_error_at' function with 'token' parameter being
 * the token in the 'parser.current' field. */
static void Parser_error(const char* error);
/* Advances the parser until token type isn't 'TOK_ERROR',
 * invoking 'Parser_error' each time the token type is 'TOK_ERROR'. */
static void Parser_advance(Scanner* scanner);
/* If current token matches the 'type', then the parser advances,
 * otherwise 'Parser_error' is invoked with the 'error' message. */
static void Parser_expect(Scanner* scanner, TokenType type, const char* error);

static void     emit_byte(Byte byte);
static uint32_t emit_constant(Value constant);
static void     emit_return(void);
static void     compile_end(void);
static void     parse_number(Scanner* scanner);
static void     parse_precedence(Scanner* scanner, Precedence prec);
static void     parse_grouping(Scanner* scanner);
static void     parse_binary(Scanner* scanner);
static void     parse_unary(Scanner* scanner);
static void     parse_ternarycond(Scanner* scanner);
static void     parse_expression(Scanner* scanner);

static void Parser_init(Scanner* scanner)
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

static void Parser_advance(Scanner* scanner)
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

static void Parser_expect(Scanner* scanner, TokenType type, const char* error)
{
    if(parser.current.type == type) {
        Parser_advance(scanner);
        return;
    }
    Parser_error(error);
}

bool compile(const char* source, Chunk* chunk)
{
    Scanner scanner = Scanner_new(source);
    compiling_chunk = chunk;
    Parser_init(&scanner);
    parse_expression(&scanner);
    Parser_expect(&scanner, TOK_EOF, "Expect end of expression.");
    compile_end();
    return !Parser_state_iserr();
}

static void compile_end(void)
{
    emit_return();
#ifdef DEBUG_PRINT_CODE
    if(!Parser_state_iserr()) {
        Chunk_debug(current_chunk(), "code");
    }
#endif
}

static Chunk* current_chunk()
{
    return compiling_chunk;
}

/*========================== EMIT =========================*/
static void emit_byte(Byte byte)
{
    Chunk_write(current_chunk(), byte, parser.previous.line);
}

static uint32_t emit_constant(Value constant)
{
    if(constant <= MAXBYTES(3)) {
        Chunk_write_constant(current_chunk(), constant, parser.previous.line);
    } else {
        Parser_error("Too many constants in one chunk.");
        return 0;
    }

    return (uint32_t)constant;
}

static void emit_return(void)
{
    emit_byte(OP_RET);
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
    [TOK_LPAREN]        = {parse_grouping, NULL,              PREC_NONE   },
    [TOK_RPAREN]        = {NULL,           NULL,              PREC_NONE   },
    [TOK_LBRACE]        = {NULL,           NULL,              PREC_NONE   },
    [TOK_RBRACE]        = {NULL,           NULL,              PREC_NONE   },
    [TOK_COMMA]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_DOT]           = {NULL,           NULL,              PREC_NONE   },
    [TOK_MINUS]         = {parse_unary,    parse_binary,      PREC_TERM   },
    [TOK_PLUS]          = {NULL,           parse_binary,      PREC_TERM   },
    [TOK_COLON]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_SEMICOLON]     = {NULL,           NULL,              PREC_NONE   },
    [TOK_SLASH]         = {NULL,           parse_binary,      PREC_FACTOR },
    [TOK_STAR]          = {NULL,           parse_binary,      PREC_FACTOR },
    [TOK_QMARK]         = {NULL,           parse_ternarycond, PREC_TERNARY},
    [TOK_BANG]          = {NULL,           NULL,              PREC_NONE   },
    [TOK_BANG_EQUAL]    = {NULL,           NULL,              PREC_NONE   },
    [TOK_EQUAL]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_EQUAL_EQUAL]   = {NULL,           NULL,              PREC_NONE   },
    [TOK_GREATER]       = {NULL,           NULL,              PREC_NONE   },
    [TOK_GREATER_EQUAL] = {NULL,           NULL,              PREC_NONE   },
    [TOK_LESS]          = {NULL,           NULL,              PREC_NONE   },
    [TOK_LESS_EQUAL]    = {NULL,           NULL,              PREC_NONE   },
    [TOK_IDENTIFIER]    = {NULL,           NULL,              PREC_NONE   },
    [TOK_STRING]        = {NULL,           NULL,              PREC_NONE   },
    [TOK_NUMBER]        = {parse_number,   NULL,              PREC_NONE   },
    [TOK_AND]           = {NULL,           NULL,              PREC_NONE   },
    [TOK_CLASS]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_ELSE]          = {NULL,           NULL,              PREC_NONE   },
    [TOK_FALSE]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_FOR]           = {NULL,           NULL,              PREC_NONE   },
    [TOK_FN]            = {NULL,           NULL,              PREC_NONE   },
    [TOK_IF]            = {NULL,           NULL,              PREC_NONE   },
    [TOK_NIL]           = {NULL,           NULL,              PREC_NONE   },
    [TOK_OR]            = {NULL,           NULL,              PREC_NONE   },
    [TOK_PRINT]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_RETURN]        = {NULL,           NULL,              PREC_NONE   },
    [TOK_SUPER]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_SELF]          = {NULL,           NULL,              PREC_NONE   },
    [TOK_TRUE]          = {NULL,           NULL,              PREC_NONE   },
    [TOK_VAR]           = {NULL,           NULL,              PREC_NONE   },
    [TOK_WHILE]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_ERROR]         = {NULL,           NULL,              PREC_NONE   },
    [TOK_EOF]           = {NULL,           NULL,              PREC_NONE   },
};

static void parse_expression(Scanner* scanner)
{
    parse_precedence(scanner, PREC_ASSIGNMENT);
}

static void parse_number(_unused Scanner* scanner)
{
    Value constant = strtod(parser.previous.start, NULL);
    emit_constant(constant);
}

/* This is the entry point to Pratt parsing */
static void parse_precedence(Scanner* scanner, Precedence prec)
{
    Parser_advance(scanner);
    ParseFn prefix_fn = rules[parser.previous.type].prefix;
    if(prefix_fn == NULL) {
        Parser_error("Expect expression.");
        return;
    }

    /* Parse unary operator (prefix) or a literal */
    prefix_fn(scanner);

    /* Parse binary operator (inifix) if any */
    while(prec <= rules[parser.current.type].precedence) {
        Parser_advance(scanner);
        ParseFn infix_fn = rules[parser.previous.type].infix;
        infix_fn(scanner);
    }
}

static void parse_grouping(Scanner* scanner)
{
    parse_expression(scanner);
    Parser_expect(scanner, TOK_RPAREN, "Expect ')' after expression");
}

static void parse_unary(Scanner* scanner)
{
    TokenType type = parser.previous.type;
    parse_precedence(scanner, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            emit_byte(OP_NEG);
            break;
        default:
            _unreachable;
            return;
    }
}

static void parse_binary(Scanner* scanner)
{
    TokenType        type = parser.previous.type;
    const ParseRule* rule = &rules[type];
    parse_precedence(scanner, rule->precedence + 1);

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
        default:
            _unreachable;
            return;
    }
}

static void parse_ternarycond(Scanner* scanner)
{
    parse_expression(scanner);
    Parser_expect(scanner, TOK_COLON, "Expect ':' (ternary conditional).");
    parse_expression(scanner);
}
