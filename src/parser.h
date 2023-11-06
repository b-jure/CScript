#ifndef __SKOOMA_SCANNER_H__
#define __SKOOMA_SCANNER_H__

#include "common.h"
#include "mem.h"
#include "value.h"

#include <stdarg.h>

typedef enum {
    // Single character tokens.
    TOK_LBRACK = 0,
    TOK_RBRACK,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_DOT,
    TOK_COMMA,
    TOK_MINUS,
    TOK_PLUS,
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_SLASH,
    TOK_STAR,
    TOK_QMARK,
    // One or two character tokens.
    TOK_BANG,
    TOK_BANG_EQUAL,
    TOK_EQUAL,
    TOK_EQUAL_EQUAL,
    TOK_GREATER,
    TOK_GREATER_EQUAL,
    TOK_LESS,
    TOK_LESS_EQUAL,
    // Literals.
    TOK_IDENTIFIER,
    TOK_STRING,
    TOK_NUMBER,
    // Keywords.
    TOK_AND,
    TOK_BREAK,
    TOK_CASE,
    TOK_CONTINUE,
    TOK_CLASS,
    TOK_DEFAULT,
    TOK_ELSE,
    TOK_FALSE,
    TOK_FOR,
    TOK_FN,
    TOK_IF,
    TOK_IMPL,
    TOK_NIL,
    TOK_OR,
    TOK_RETURN,
    TOK_SUPER,
    TOK_SELF,
    TOK_SWITCH,
    TOK_TRUE,
    TOK_VAR,
    TOK_WHILE,
    TOK_FIXED,

    TOK_ERROR,
    TOK_EOF
} TokenType;

typedef struct {
    // Token type
    TokenType type;

    // String slice
    const char* start;
    UInt        len;

    // Source file line on which token resides
    UInt line;

    // Literal value (constant)
    Value value;
} Token;



// Parser 'flags' bits, bit 0 is read as bit 1.
#define ERROR_BIT  (1) // compile error occured
#define PANIC_BIT  (2) // compiler needs to sync
#define LOOP_BIT   (3) // inside of a loop statement
#define SWITCH_BIT (4) // inside of a switch statement
#define ASSIGN_BIT (5) // can compile assignment

/* Variable 'flags' bits, variable modifiers
 * are stored in the upper byte. */
#define FIXED_BIT (9) // Variable is fixed (immutable)
// 10-16 bits are unused

// Parser flags setters and getters.
#define PFLAG_TOGGLE(parser, bit, on) BIT_TOGGLE((parser)->flags, bit, on)
#define PFLAG_SET(parser, bit)        BIT_SET((parser)->flags, bit)
#define PFLAG_CHECK(parser, bit)      BIT_CHECK((parser)->flags, bit)
#define PFLAG_CLEAR(parser, bit)      BIT_CLEAR((parser)->flags, bit)
#define PFLAG_RESET(parser)           ((parser)->flags = 0)
#define PFLAGS(parser)                ((parser)->flags)
#define PVAR_FLAGS(parser)            (((parser)->flags >> 8) & 0xff)

typedef struct {
    // Pointers to root holders in order
    // to properly mark objects for gc.
    VM* vm;

    // Pointer to the start of the source file,
    // used for cleanup only!
    const char* source;

    // Token slice start
    const char* start;
    // Current byte in the source file
    // (token slice end)
    const char* _current;

    Token previous;
    Token current;

    // Current source file line the parser
    // is parsing.
    UInt line;

    /*
     * Parser flags.
     * 1 - error bit  <- error indicator
     * 2 - panic bit  <- panic mode
     * 3 - loop bit   <- inside a loop
     * 4 - switch bit <- inside a switch statement
     * 5 - assign bit <- can parse assignment
     * 6 - return bit <- parsed return statement
     * 7 - unused
     * 8 - unused
     * 9 - fixed bit  <- variable modifier (immutable)
     * 10 - unused
     * ...
     * 16 - unused
     */
    uint16_t flags;
} Parser;


Parser Parser_new(const char* source, VM* vm);
Token  Parser_next(Parser* scanner);
Token  Token_syn_new(const char* name);
void   print_error(Parser* parser, const char* fmt, va_list args);

#endif
