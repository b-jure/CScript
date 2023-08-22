#include "common.h"
#include "scanner.h"

#include <ctype.h>
#include <string.h>

typedef struct {
    const char* start;
    const char* current;
    UInt        line;
} Scanner;

Scanner scanner;

#define advance()   (*scanner.current++)
#define peek()      (*scanner.current)
#define isend()     (*scanner.current == '\0')
#define peek_next() ((isend()) ? '\0' : *(scanner.current + 1))

static Token     Token_new(TokenType type);
static Token     Token_error(const char* err);
static Token     Token_string(void);
static Token     Token_number(void);
static Token     Token_identifier(void);
static TokenType TokenType_identifier(void);
static void      Scanner_skipws(void);
static bool      Scanner_match(char c);
static TokenType check_keyword(UInt start, UInt end, const char* pattern, TokenType type);

void Scanner_init(const char* source)
{
    scanner.start   = source;
    scanner.current = source;
    scanner.line    = 1;
}

Token Scanner_scan(void)
{
    Scanner_skipws();
    scanner.start = scanner.current;

    if(isend()) {
        return Token_new(TOK_EOF);
    }

    char c = advance();

    if(c == '_' || isalpha(c)) {
        return Token_identifier();
    }

    if(isdigit(c)) {
        return Token_number();
    }

    switch(c) {
        case '(':
            return Token_new(TOK_LPAREN);
        case ')':
            return Token_new(TOK_RPAREN);
        case '{':
            return Token_new(TOK_LBRACE);
        case '}':
            return Token_new(TOK_RBRACE);
        case '.':
            return Token_new(TOK_DOT);
        case ',':
            return Token_new(TOK_COMMA);
        case '-':
            return Token_new(TOK_MINUS);
        case '+':
            return Token_new(TOK_PLUS);
        case ';':
            return Token_new(TOK_SEMICOLON);
        case '/':
            return Token_new(TOK_SLASH);
        case '*':
            return Token_new(TOK_STAR);
        case '!':
            return Token_new(Scanner_match('=') ? TOK_BANG_EQUAL : TOK_BANG);
        case '=':
            return Token_new(Scanner_match('=') ? TOK_EQUAL_EQUAL : TOK_EQUAL);
        case '>':
            return Token_new(Scanner_match('=') ? TOK_GREATER_EQUAL : TOK_GREATER);
        case '<':
            return Token_new(Scanner_match('=') ? TOK_LESS_EQUAL : TOK_LESS);
        case '"':
            return Token_string();
        default:
            return Token_error("Unexpected character.");
    }
}

static bool Scanner_match(char c)
{
    if(isend() || c != peek()) {
        return false;
    }
    scanner.current++;
    return true;
}

static void Scanner_skipws(void)
{
    register char c;

    while(true) {
        switch((c = peek())) {
            case '\n':
                scanner.line++;
                break;
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '/':
                if((c = peek_next()) == '/') {
                    while(peek() != '\n' && !isend()) {
                        advance();
                    }
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static Token Token_new(TokenType type)
{
    Token token;
    token.type  = type;
    token.start = scanner.start;
    token.len   = scanner.current - scanner.start;
    token.line  = scanner.line;
    return token;
}

static Token Token_error(const char* err)
{
    Token token;
    token.type  = TOK_ERROR;
    token.start = err;
    token.len   = strlen(err);
    token.line  = scanner.line;
    return token;
}

static Token Token_string(void)
{
    while(peek() != '"' && !isend()) {
        if(peek() == '\n') {
            scanner.line++;
        }
        advance();
    }

    if(isend()) {
        return Token_error("Unterminated string, missing closing quotes '\"'");
    }

    advance();
    return Token_new(TOK_STRING);
}

static Token Token_number(void)
{
    advance();
    while(isdigit(peek())) {
        advance();
    }

    if(peek() == '.' && isdigit(peek_next())) {
        advance();
        while(isdigit(peek())) {
            advance();
        }
    }

    return Token_new(TOK_NUMBER);
}

static Token Token_identifier(void)
{
    register char c;

    advance();
    while(isalnum((c = peek())) || c == '_') {
        advance();
    }

    return Token_new(TokenType_identifier());
}

static TokenType TokenType_identifier(void)
{
    switch(*scanner.start) {
        case 'a':
            return check_keyword(1, 2, "nd", TOK_AND);
        case 'c':
            return check_keyword(1, 4, "lass", TOK_CLASS);
        case 'e':
            return check_keyword(1, 3, "lse", TOK_ELSE);
        case 'f':
            if(scanner.current - scanner.start > 1) {
                switch(scanner.start[1]) {
                    case 'a':
                        return check_keyword(2, 3, "lse", TOK_FALSE);
                    case 'n':
                        if(scanner.current - scanner.start == 2) {
                            return TOK_FN;
                        } else {
                            return TOK_IDENTIFIER;
                        }
                    case 'o':
                        return check_keyword(2, 1, "r", TOK_FOR);
                    default:
                        break;
                }
            }
            break;
        case 'i':
            if(scanner.current - scanner.start > 1) {
                switch(scanner.start[1]) {
                    case 'm':
                        check_keyword(2, 2, "pl", TOK_IMPL);
                    case 'f':
                        if(scanner.current - scanner.start == 2) {
                            return TOK_IF;
                        } else {
                            return TOK_IDENTIFIER;
                        }
                    default:
                        break;
                }
            }
            break;
        case 'n':
            return check_keyword(1, 2, "il", TOK_NIL);
        case 'o':
            return check_keyword(1, 1, "r", TOK_OR);
        case 'p':
            return check_keyword(1, 4, "rint", TOK_PRINT);
        case 'r':
            return check_keyword(1, 5, "eturn", TOK_RETURN);
        case 's':
            if(scanner.current - scanner.start > 1) {
                switch(scanner.start[1]) {
                    case 'u':
                        return check_keyword(2, 3, "per", TOK_SUPER);
                    case 'e':
                        return check_keyword(2, 2, "lf", TOK_SELF);
                    default:
                        break;
                }
            }
            break;
        case 't':
            return check_keyword(1, 3, "rue", TOK_TRUE);
        case 'v':
            return check_keyword(1, 2, "ar", TOK_VAR);
        case 'w':
            return check_keyword(1, 4, "hile", TOK_WHILE);
        default:
            break;
    }
    return TOK_IDENTIFIER;
}

static TokenType
check_keyword(UInt start, UInt length, const char* pattern, TokenType type)
{
    if(scanner.current - scanner.start == start + length &&
       memcmp(scanner.start + start, pattern, length) == 0)
    {
        return type;
    }

    return TOK_IDENTIFIER;
}
