#include "common.h"
#include "object.h"
#include "parser.h"
#include "skconf.h"
#include "value.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#define MAX_ERR_STRING 50
#define ERR_LEN(len)   (MIN(MAX_ERR_STRING, len))

typedef enum {
    NUM_HEX,
    NUM_DEC,
    NUM_OCT,
} NumberType;

SK_INTERNAL(force_inline Token) Token_new(Parser* parser, TokenType type);
SK_INTERNAL(Token) Token_error(Parser* parser, const char* err);
SK_INTERNAL(force_inline Token) parse_string(Parser* parser);
SK_INTERNAL(force_inline Token) parse_hex(Parser* parser);
SK_INTERNAL(force_inline Token) parse_octal(Parser* parser);
SK_INTERNAL(force_inline Token) parse_decimal(Parser* parser);
SK_INTERNAL(force_inline Token) Token_identifier(Parser* parser);
SK_INTERNAL(TokenType) TokenType_identifier(Parser* parser);
SK_INTERNAL(force_inline void) Parser_skipws(Parser* parser);
SK_INTERNAL(force_inline bool) Parser_match(Parser* parser, char c);
SK_INTERNAL(TokenType)
check_keyword(Parser* parser, UInt start, UInt end, const char* pattern, TokenType type);



#define peek(parser)      (*(parser)->_current)
#define isend(parser)     (peek(parser) == '\0')
#define peek_next(parser) ((isend(parser)) ? '\0' : *((parser)->_current + 1))

void print_error(Parser* parser, const char* err, va_list args)
{
    const Token* token = &parser->previous;

    fprintf(stderr, "[line: %u] Error", token->line);

    if(token->type == TOK_EOF) {
        fprintf(stderr, " at end of file");
    } else if(token->type != TOK_ERROR) {
        fprintf(stderr, " at '%.*s'", token->len, token->start);
    }

    fputs(": ", stderr);
    vfprintf(stderr, err, args);
    putc('\n', stderr);
}

SK_INTERNAL(void) Parser_error(Parser* parser, const char* err, ...)
{
    va_list args;

    PFLAG_SET(parser, PANIC_BIT);
    PFLAG_SET(parser, ERROR_BIT);

    va_start(args, err);
    print_error(parser, err, args);
    va_end(args);
}

SK_INTERNAL(force_inline void) advance(Parser* parser)
{
    if(*parser->_current++ == '\n') {
        parser->line++;
    }
}

SK_INTERNAL(force_inline char) nextchar(Parser* parser)
{
    char c = *parser->_current++;
    if(c == '\n') {
        parser->line++;
    }
    return c;
}

Parser Parser_new(const char* source, VM* vm)
{
    return (Parser){
        .vm       = vm,
        .start    = source,
        ._current = source,
        .line     = 1,
        .flags    = 0,
    };
}

Token Parser_next(Parser* parser)
{
    Parser_skipws(parser);
    parser->start = parser->_current;

    if(isend(parser)) {
        return Token_new(parser, TOK_EOF);
    }

    int c = nextchar(parser);

    if(c == '_' || isalpha(c)) {
        return Token_identifier(parser);
    }

    if(isdigit(c)) {
        if(peek(parser) == 'x' || peek(parser) == 'X') {
            return parse_hex(parser);
        } else if(c == '0' && isdigit(peek(parser))) {
            return parse_octal(parser);
        } else {
            return parse_decimal(parser);
        }
    }

#ifdef SK_PRECOMPUTED_GOTO
    #define ERR &&err
    // IMPORTANT: update accordingly if TokenType enum changes!
    static const void* jump_table[UINT8_MAX + 1] = {
        // Must be the same order as in ASCII Table - https://www.asciitable.com
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      &&bang,   &&string, ERR,         ERR,     ERR,      ERR,       ERR,
        &&lparen, &&rparen, &&star,   &&plus,      &&comma, &&minus,  &&dot,     &&slash,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      &&colon,  &&semicolon, &&less,  &&equal,  &&greater, &&qmark,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      &&lbrack,    ERR,     &&rbrack, ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      &&lbrace,    ERR,     &&rbrace, ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,      ERR,       ERR,
    };
    #undef ERR

    goto* jump_table[c];

lbrack:
    return Token_new(parser, TOK_LBRACK);
rbrack:
    return Token_new(parser, TOK_RBRACK);
lparen:
    return Token_new(parser, TOK_LPAREN);
rparen:
    return Token_new(parser, TOK_RPAREN);
lbrace:
    return Token_new(parser, TOK_LBRACE);
rbrace:
    return Token_new(parser, TOK_RBRACE);
dot:
    return Token_new(parser, TOK_DOT);
comma:
    return Token_new(parser, TOK_COMMA);
minus:
    return Token_new(parser, TOK_MINUS);
plus:
    return Token_new(parser, TOK_PLUS);
colon:
    return Token_new(parser, TOK_COLON);
semicolon:
    return Token_new(parser, TOK_SEMICOLON);
slash:
    return Token_new(parser, TOK_SLASH);
star:
    return Token_new(parser, TOK_STAR);
qmark:
    return Token_new(parser, TOK_QMARK);
bang:
    return Token_new(parser, Parser_match(parser, '=') ? TOK_BANG_EQUAL : TOK_BANG);
equal:
    return Token_new(parser, Parser_match(parser, '=') ? TOK_EQUAL_EQUAL : TOK_EQUAL);
greater:
    return Token_new(parser, Parser_match(parser, '=') ? TOK_GREATER_EQUAL : TOK_GREATER);
less:
    return Token_new(parser, Parser_match(parser, '=') ? TOK_LESS_EQUAL : TOK_LESS);
string:
    return parse_string(parser);
err:
    Parser_error(parser, "Unexpected character '%c'.", c);
    return Token_new(parser, TOK_ERROR);

    unreachable;

#else
    switch(c) {
        case '[':
            return Token_new(parser, TOK_LBRACK);
        case ']':
            return Token_new(parser, TOK_RBRACK);
        case '(':
            return Token_new(parser, TOK_LPAREN);
        case ')':
            return Token_new(parser, TOK_RPAREN);
        case '{':
            return Token_new(parser, TOK_LBRACE);
        case '}':
            return Token_new(parser, TOK_RBRACE);
        case '.':
            return Token_new(parser, TOK_DOT);
        case ',':
            return Token_new(parser, TOK_COMMA);
        case '-':
            return Token_new(parser, TOK_MINUS);
        case '+':
            return Token_new(parser, TOK_PLUS);
        case ';':
            return Token_new(parser, TOK_SEMICOLON);
        case ':':
            return Token_new(parser, TOK_COLON);
        case '?':
            return Token_new(parser, TOK_QMARK);
        case '/':
            return Token_new(parser, TOK_SLASH);
        case '*':
            return Token_new(parser, TOK_STAR);
        case '!':
            return Token_new(
                parser,
                Parser_match(parser, '=') ? TOK_BANG_EQUAL : TOK_BANG);
        case '=':
            return Token_new(
                parser,
                Parser_match(parser, '=') ? TOK_EQUAL_EQUAL : TOK_EQUAL);
        case '>':
            return Token_new(
                parser,
                Parser_match(parser, '=') ? TOK_GREATER_EQUAL : TOK_GREATER);
        case '<':
            return Token_new(
                parser,
                Parser_match(parser, '=') ? TOK_LESS_EQUAL : TOK_LESS);
        case '"':
            return parse_string(parser);
        default:
            return Token_error(parser, "Unexpected character.");
    }
#endif
}

SK_INTERNAL(bool) Parser_match(Parser* parser, char c)
{
    if(isend(parser) || c != peek(parser)) {
        return false;
    }
    parser->_current++;
    return true;
}

SK_INTERNAL(force_inline void) Parser_skipws(Parser* parser)
{
    while(true) {
        switch(peek(parser)) {
            case '\n':
                nextchar(parser);
                break;
            case ' ':
            case '\r':
            case '\t':
                nextchar(parser);
                break;
            case '/':
                if(peek_next(parser) == '/') {
                    while(peek(parser) != '\n' && !isend(parser)) {
                        nextchar(parser);
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

Token Token_syn_new(const char* name)
{
    return (Token){
        .type  = 0,
        .line  = 0,
        .start = name,
        .len   = (UInt)strlen(name),
    };
}

SK_INTERNAL(force_inline Token) Token_new(Parser* parser, TokenType type)
{
    Token token;
    token.type  = type;
    token.start = parser->start;
    token.len   = parser->_current - parser->start;
    token.line  = parser->line;
    return token;
}

SK_INTERNAL(force_inline Token) Token_error(Parser* parser, const char* err)
{
    Token token;
    token.type  = TOK_ERROR;
    token.start = err;
    token.len   = strlen(err);
    token.line  = parser->line;
    return token;
}

SK_INTERNAL(force_inline Int) parse_hex_digit(Parser* parser)
{
    char hex = nextchar(parser);

    if(hex >= '0' && hex <= '9') {
        return hex - '0';
    } else if(hex >= 'a' && hex <= 'f') {
        return (hex - 'a') + 10;
    } else if(hex >= 'A' && hex <= 'F') {
        return (hex - 'A') + 10;
    }

    parser->_current--;
    return -1;
}

SK_INTERNAL(force_inline Int) parse_esc_hex(Parser* parser)
{
    Int number = 0;

    for(UInt i = 0; i < 2; i++) {
        if(peek(parser) == '"' || peek(parser) == '\0') {
            Parser_error(parser, "Incomplete hex escape sequence.");
            parser->_current--;
            break;
        }

        Int digit = parse_hex_digit(parser);

        if(digit == -1) {
            Parser_error(parser, "Invalid hexadecimal escape sequence.");
            break;
        }

        number = (number << 4) | digit;
    }

    return number;
}

// @TODO: Prevent __init__ from being called like this: class.__init__()
SK_INTERNAL(force_inline Token) parse_string(Parser* parser)
{
    Array_Byte buffer;
    Array_Byte_init(&buffer);

    while(true) {
        char c = nextchar(parser);

        if(c == '\0') {
            Parser_error(parser, "Unterminated string.");
            parser->_current--;
            break;
        } else if(c == '\r') {
            continue;
        } else if(c == '"') {
            break;
        } else if(c == '\\') {
            switch((c = nextchar(parser))) {
                case '"':
                    Array_Byte_push(&buffer, '"');
                    break;
                case '%':
                    Array_Byte_push(&buffer, '%');
                    break;
                case '0':
                    Array_Byte_push(&buffer, '\0');
                    break;
                case '\\':
                    Array_Byte_push(&buffer, '\\');
                    break;
                case 'a':
                    Array_Byte_push(&buffer, '\a');
                    break;
                case 'b':
                    Array_Byte_push(&buffer, '\b');
                    break;
                case 'e':
                    Array_Byte_push(&buffer, '\33');
                    break;
                case 'f':
                    Array_Byte_push(&buffer, '\f');
                    break;
                case 'n':
                    Array_Byte_push(&buffer, '\n');
                    break;
                case 'r':
                    Array_Byte_push(&buffer, '\r');
                    break;
                case 't':
                    Array_Byte_push(&buffer, '\t');
                    break;
                case 'v':
                    Array_Byte_push(&buffer, '\v');
                    break;
                case 'x':
                    Array_Byte_push(&buffer, (Byte)parse_esc_hex(parser));
                    break;
            }
        } else {
            Array_Byte_push(&buffer, c);
        }
    }

    ObjString* string = ObjString_from(parser->vm, (void*)buffer.data, buffer.len);

    Array_Byte_free(&buffer, NULL);

    Token token = Token_new(parser, TOK_STRING);
    token.value = OBJ_VAL(string);
    return token;
}

SK_INTERNAL(Token) Token_number(Parser* parser, NumberType type)
{
    errno = 0;

    Value number;

    if(type == NUM_HEX) {
        number = NUMBER_VAL(strtoll(parser->start, NULL, 16));
    } else if(type == NUM_DEC) {
        number = NUMBER_VAL(strtod(parser->start, NULL));
    } else {
        number = NUMBER_VAL(strtoll(parser->start, NULL, 8));
    }

    if(errno == ERANGE) {
        Parser_error(
            parser,
            "Too large number constant '%.*s'...",
            ERR_LEN(parser->_current - parser->start),
            parser->start);
        number = NUMBER_VAL(0);
    }


    Token token = Token_new(parser, TOK_NUMBER);
    token.value = number;
    return token;
}

SK_INTERNAL(Token) parse_octal(Parser* parser)
{
    nextchar(parser); // skip leading zero

    char c;
    while(isdigit((c = peek(parser)))) {
        if(c >= '8') {
            Parser_error(parser, "Invalid digit '%c' in octal number.", c);
            break;
        }
        advance(parser);
    }

    return Token_number(parser, NUM_OCT);
}

SK_INTERNAL(Token) parse_hex(Parser* parser)
{
    nextchar(parser); // skip 'x' or 'X'

    if(!isxdigit(peek(parser))) {
        Parser_error(parser, "Invalid hexadecimal constant.");
    }

    while(parse_hex_digit(parser) != -1)
        ;

    return Token_number(parser, NUM_HEX);
}

SK_INTERNAL(Token) parse_decimal(Parser* parser)
{
    while(isdigit(peek(parser))) {
        advance(parser);
    }

    if(peek(parser) == '.') {
        advance(parser); // skip '.'

        while(isdigit(peek(parser))) {
            advance(parser);
        }

        if(peek(parser) == 'e' || peek(parser) == 'E') {
            advance(parser); // skip 'e' or 'E'

            if(peek(parser) == '+' || peek(parser) == '-') {
                advance(parser); // skip '+' or '-'
            }

            if(!isdigit(peek(parser))) {
                Parser_error(parser, "Exponent has no digits.");
            } else {
                nextchar(parser); // skip digit
                while(isdigit(peek(parser))) {
                    advance(parser);
                }
            }
        }
    }

    return Token_number(parser, NUM_DEC);
}

SK_INTERNAL(force_inline Token) Token_identifier(Parser* parser)
{
    char c;
    while(isalnum((c = peek(parser))) || c == '_') {
        advance(parser);
    }

    return Token_new(parser, TokenType_identifier(parser));
}

SK_INTERNAL(TokenType) TokenType_identifier(Parser* parser)
{
#ifdef SK_PRECOMPUTED_GOTO
    #define RET &&ret
    // IMPORTANT: update accordingly if parser tokens change!
    static const void* jump_table[UINT8_MAX + 1] = {
        // Make sure the order is the same as in ASCII Table - https://www.asciitable.com
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, &&a, &&b, &&c, &&d, &&e, &&f, RET, RET, &&i, RET, RET, RET, RET, &&n, &&o,
        RET, RET, &&r, &&s, &&t, RET, &&v, &&w, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
    };
    #undef RET

    goto* jump_table[(Int)*parser->start];

a:
    return check_keyword(parser, 1, 2, "nd", TOK_AND);
b:
    return check_keyword(parser, 1, 4, "reak", TOK_BREAK);
c:
    if(parser->_current - parser->start > 1) {
        switch(parser->start[1]) {
            case 'a':
                return check_keyword(parser, 2, 2, "se", TOK_CASE);
            case 'l':
                return check_keyword(parser, 2, 3, "ass", TOK_CLASS); // Lmao
            case 'o':
                return check_keyword(parser, 2, 6, "ntinue", TOK_CONTINUE);
            default:
                break;
        }
    }
    goto ret;
d:
    return check_keyword(parser, 1, 6, "efault", TOK_DEFAULT);
e:
    return check_keyword(parser, 1, 3, "lse", TOK_ELSE);
f:
    if(parser->_current - parser->start > 1) {
        switch(parser->start[1]) {
            case 'a':
                return check_keyword(parser, 2, 3, "lse", TOK_FALSE);
            case 'i':
                return check_keyword(parser, 2, 3, "xed", TOK_FIXED);
            case 'n':
                if(parser->_current - parser->start == 2) {
                    return TOK_FN;
                } else {
                    return TOK_IDENTIFIER;
                }
            case 'o':
                return check_keyword(parser, 2, 1, "r", TOK_FOR);
            default:
                break;
        }
    }
    goto ret;
i:
    if(parser->_current - parser->start > 1) {
        switch(parser->start[1]) {
            case 'm':
                return check_keyword(parser, 2, 2, "pl", TOK_IMPL);
            case 'f':
                if(parser->_current - parser->start == 2) {
                    return TOK_IF;
                } else {
                    return TOK_IDENTIFIER;
                }
            default:
                break;
        }
    }
    goto ret;
n:
    return check_keyword(parser, 1, 2, "il", TOK_NIL);
o:
    return check_keyword(parser, 1, 1, "r", TOK_OR);
r:
    return check_keyword(parser, 1, 5, "eturn", TOK_RETURN);
s:
    if(parser->_current - parser->start > 1) {
        switch(parser->start[1]) {
            case 'u':
                return check_keyword(parser, 2, 3, "per", TOK_SUPER);
            case 'e':
                return check_keyword(parser, 2, 2, "lf", TOK_SELF);
            case 'w':
                return check_keyword(parser, 2, 4, "itch", TOK_SWITCH);
            default:
                break;
        }
    }
    goto ret;
t:
    return check_keyword(parser, 1, 3, "rue", TOK_TRUE);
v:
    return check_keyword(parser, 1, 2, "ar", TOK_VAR);
w:
    return check_keyword(parser, 1, 4, "hile", TOK_WHILE);

#else
    switch(*parser->start) {
        case 'a':
            return check_keyword(parser, 1, 2, "nd", TOK_AND);
        case 'b':
            return check_keyword(parser, 1, 4, "reak", TOK_BREAK);
        case 'c':
            if(parser->_current - parser->start > 1) {
                switch(parser->start[1]) {
                    case 'a':
                        return check_keyword(parser, 2, 2, "se", TOK_CASE);
                    case 'l':
                        return check_keyword(parser, 2, 3, "ass", TOK_CLASS); // Lmao
                    case 'o':
                        return check_keyword(parser, 2, 6, "ntinue", TOK_CONTINUE);
                    default:
                        break;
                }
            }
            break;
        case 'd':
            return check_keyword(parser, 1, 6, "efault", TOK_DEFAULT);
        case 'e':
            return check_keyword(parser, 1, 3, "lse", TOK_ELSE);
        case 'f':
            if(parser->_current - parser->start > 1) {
                switch(parser->start[1]) {
                    case 'a':
                        return check_keyword(parser, 2, 3, "lse", TOK_FALSE);
                    case 'i':
                        return check_keyword(parser, 2, 3, "xed", TOK_FIXED);
                    case 'n':
                        if(parser->_current - parser->start == 2) {
                            return TOK_FN;
                        } else {
                            return TOK_IDENTIFIER;
                        }
                    case 'o':
                        return check_keyword(parser, 2, 1, "r", TOK_FOR);
                    default:
                        break;
                }
            }
            break;
        case 'i':
            if(parser->_current - parser->start > 1) {
                switch(parser->start[1]) {
                    case 'm':
                        check_keyword(parser, 2, 2, "pl", TOK_IMPL);
                    case 'f':
                        if(parser->_current - parser->start == 2) {
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
            return check_keyword(parser, 1, 2, "il", TOK_NIL);
        case 'o':
            return check_keyword(parser, 1, 1, "r", TOK_OR);
        case 'r':
            return check_keyword(parser, 1, 5, "eturn", TOK_RETURN);
        case 's':
            if(parser->_current - parser->start > 1) {
                switch(parser->start[1]) {
                    case 'u':
                        return check_keyword(parser, 2, 3, "per", TOK_SUPER);
                    case 'e':
                        return check_keyword(parser, 2, 2, "lf", TOK_SELF);
                    case 'w':
                        return check_keyword(parser, 2, 4, "itch", TOK_SWITCH);
                    default:
                        break;
                }
            }
            break;
        case 't':
            return check_keyword(parser, 1, 3, "rue", TOK_TRUE);
        case 'v':
            return check_keyword(parser, 1, 2, "ar", TOK_VAR);
        case 'w':
            return check_keyword(parser, 1, 4, "hile", TOK_WHILE);
        default:
            break;
    }
#endif
ret:
    return TOK_IDENTIFIER;
}

SK_INTERNAL(force_inline TokenType)
check_keyword(
    Parser*     parser,
    UInt        start,
    UInt        length,
    const char* pattern,
    TokenType   type)
{
    if(parser->_current - parser->start == start + length &&
       memcmp(parser->start + start, pattern, length) == 0)
    {
        return type;
    }

    return TOK_IDENTIFIER;
}
