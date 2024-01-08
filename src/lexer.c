#include "common.h"
#include "debug.h"
#include "lexer.h"
#include "object.h"
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

// 'synthetic' token
Token syntoken(const char* name)
{
    return (Token){
        .type = 0,
        .line = 0,
        .start = name,
        .len = (UInt)strlen(name),
    };
}

static force_inline Token token(Lexer* lexer, TokenType type)
{
    Token token;
    token.type = type;
    token.start = lexer->start;
    token.len = lexer->_current - lexer->start;
    token.line = lexer->line;
    return token;
}

static force_inline Token errtoken(Lexer* lexer, const char* err)
{
    Token token;
    token.type = TOK_ERROR;
    token.start = err;
    token.len = strlen(err);
    token.line = lexer->line;
    return token;
}

#define peek(lexer)      (*(lexer)->_current)
#define isend(lexer)     (peek(lexer) == '\0')
#define peek_next(lexer) ((isend(lexer)) ? '\0' : *((lexer)->_current + 1))

/* Register compile-time error */
void regcomperror(Lexer* lexer, const char* err, va_list args)
{
    static const char* prefix_fmt = "[%s][line: %u] Error";
    bool preverr = lexer->error;
    lexer->panic = true;
    lexer->error = true;
    VM* vm = lexer->vm;
    const Token* token = &lexer->previous;
    OString* prefix = OString_fmt(vm, prefix_fmt, AS_CSTRING(vm->script), token->line);
    push(vm, OBJ_VAL(prefix));
    if(token->type == TOK_EOF) {
        push(vm, OBJ_VAL(OString_fmt(vm, " at end of file: ")));
    } else if(token->type != TOK_ERROR) {
        push(vm, OBJ_VAL(OString_fmt(vm, " at '%.*s': ", token->len, token->start)));
    } else goto pusherr;
    concatonstack(vm);
pusherr:
    push(vm, OBJ_VAL(OString_fmt_from(vm, err, args)));
    concatonstack(vm);
    if(preverr) concatonstack(vm);
}

static void lexerror(Lexer* lexer, const char* err, ...)
{
    va_list args;
    va_start(args, err);
    regcomperror(lexer, err, args);
    va_end(args);
}


#define advance(lexer)                                                                   \
    if(*lexer->_current++ == '\n') lexer->line++;


static force_inline char nextchar(Lexer* lexer)
{
    char c = *lexer->_current++;
    if(c == '\n') lexer->line++;
    return c;
}

Lexer L_new(const char* source, VM* vm)
{
    return (Lexer){
        .vm = vm,
        .source = source,
        .start = source,
        ._current = source,
        .line = 1,
        .panic = false,
        .error = false,
    };
}

static force_inline Int hexdigit(Lexer* lexer)
{
    char hex = nextchar(lexer);
    if(hex >= '0' && hex <= '9') return hex - '0';
    else if(hex >= 'a' && hex <= 'f') return (hex - 'a') + 10;
    else if(hex >= 'A' && hex <= 'F') return (hex - 'A') + 10;
    lexer->_current--;
    return -1;
}

static force_inline Int eschex(Lexer* lexer)
{
    Int number = 0;
    for(UInt i = 0; i < 2; i++) {
        if(peek(lexer) == '"' || peek(lexer) == '\0') {
            lexerror(lexer, "Incomplete hex escape sequence.");
            lexer->_current--;
            break;
        }
        Int digit = hexdigit(lexer);
        if(digit == -1) {
            lexerror(lexer, "Invalid hexadecimal escape sequence.");
            break;
        }
        number = (number << 4) | digit;
    }
    return number;
}

static force_inline Token string(Lexer* lexer)
{
    Array_Byte buffer;
    Array_Byte_init(&buffer, lexer->vm);
    while(true) {
        char c = nextchar(lexer);
        if(c == '\0') {
            lexerror(lexer, "Unterminated string.");
            lexer->_current--;
            break;
        } else if(c == '\r') continue;
        else if(c == '"') break;
        else if(c == '\\') {
            switch((c = nextchar(lexer))) {
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
                    Array_Byte_push(&buffer, (Byte)eschex(lexer));
                    break;
            }
        } else Array_Byte_push(&buffer, c);
    }
    OString* string = OString_new(lexer->vm, (void*)buffer.data, buffer.len);
    Array_Byte_free(&buffer, NULL);
    Token tok = token(lexer, TOK_STRING);
    tok.value = OBJ_VAL(string);
    return tok;
}

static Token numtoken(Lexer* lexer, NumberType type)
{
    errno = 0;
    Value number;
    switch(type) {
        case NUM_HEX:
            number = NUMBER_VAL(strtoll(lexer->start, NULL, 16));
            break;
        case NUM_DEC:
            number = NUMBER_VAL(strtod(lexer->start, NULL));
            break;
        case NUM_OCT:
            number = NUMBER_VAL(strtoll(lexer->start, NULL, 8));
            break;
        default:
            unreachable;
    }
    if(errno == ERANGE) {
        lexerror(
            lexer,
            "Too large number constant '%.*s'...",
            ERR_LEN(lexer->_current - lexer->start),
            lexer->start);
        number = NUMBER_VAL(0);
    }
    Token tok = token(lexer, TOK_NUMBER);
    tok.value = number;
    return tok;
}

static Token octal(Lexer* lexer)
{
    nextchar(lexer); // skip leading zero
    char c;
    while(isdigit((c = peek(lexer)))) {
        if(c >= '8') {
            lexerror(lexer, "Invalid digit '%c' in octal number.", c);
            break;
        }
        advance(lexer);
    }
    return numtoken(lexer, NUM_OCT);
}

static Token hex(Lexer* lexer)
{
    nextchar(lexer); // skip 'x' or 'X'
    if(!isxdigit(peek(lexer))) lexerror(lexer, "Invalid hexadecimal constant.");
    while(hexdigit(lexer) != -1)
        ;
    return numtoken(lexer, NUM_HEX);
}

static Token decimal(Lexer* lexer)
{
    while(isdigit(peek(lexer)))
        advance(lexer);
    if(peek(lexer) == '.') {
        advance(lexer); // skip '.'
        while(isdigit(peek(lexer)))
            advance(lexer);
        if(peek(lexer) == 'e' || peek(lexer) == 'E') {
            advance(lexer); // skip 'e' or 'E'
            if(peek(lexer) == '+' || peek(lexer) == '-')
                advance(lexer); // skip '+' or '-'
            if(!isdigit(peek(lexer))) lexerror(lexer, "Exponent has no digits.");
            else {
                nextchar(lexer); // skip digit
                while(isdigit(peek(lexer)))
                    advance(lexer);
            }
        }
    }
    return numtoken(lexer, NUM_DEC);
}

static force_inline TokenType
keyword(Lexer* lexer, UInt start, UInt length, const char* pattern, TokenType type)
{
    if(lexer->_current - lexer->start == start + length &&
       memcmp(lexer->start + start, pattern, length) == 0)
        return type;
    return TOK_IDENTIFIER;
}

static TokenType TokenType_identifier(Lexer* lexer)
{
#ifdef SK_PRECOMPUTED_GOTO
#define RET &&ret
    // IMPORTANT: update accordingly if lexer tokens change!
    static const void* jump_table[UINT8_MAX + 1] = {
        // Make sure the order is the same as in ASCII Table -
        // https://www.asciitable.com
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
        RET, &&a, &&b, &&c, &&d, &&e, &&f, RET, RET, &&i, RET, RET, &&l, RET, &&n, &&o,
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

    goto* jump_table[(Int)*lexer->start];
a:
    return keyword(lexer, 1, 2, "nd", TOK_AND);
b:
    return keyword(lexer, 1, 4, "reak", TOK_BREAK);
c:
    if(lexer->_current - lexer->start > 1) {
        switch(lexer->start[1]) {
            case 'a':
                return keyword(lexer, 2, 2, "se", TOK_CASE);
            case 'l':
                return keyword(lexer, 2, 3, "ass", TOK_CLASS); // Lmao
            case 'o':
                return keyword(lexer, 2, 6, "ntinue", TOK_CONTINUE);
            default:
                break;
        }
    }
    goto ret;
d:
    return keyword(lexer, 1, 6, "efault", TOK_DEFAULT);
e:
    return keyword(lexer, 1, 3, "lse", TOK_ELSE);
f:
    if(lexer->_current - lexer->start > 1) {
        switch(lexer->start[1]) {
            case 'a':
                return keyword(lexer, 2, 3, "lse", TOK_FALSE);
            case 'i':
                return keyword(lexer, 2, 3, "xed", TOK_FIXED);
            case 'n':
                if(lexer->_current - lexer->start == 2) return TOK_FN;
                else return TOK_IDENTIFIER;
            case 'o':
                if(lexer->_current - lexer->start > 3)
                    return keyword(lexer, 2, 5, "reach", TOK_FOREACH);
                else return keyword(lexer, 2, 1, "r", TOK_FOR);
            default:
                break;
        }
    }
    goto ret;
i:
    if(lexer->_current - lexer->start > 1) {
        switch(lexer->start[1]) {
            case 'm':
                return keyword(lexer, 2, 2, "pl", TOK_IMPL);
            case 'n':
                if(lexer->_current - lexer->start == 2) return TOK_IN;
                else return TOK_IDENTIFIER;
            case 'f':
                if(lexer->_current - lexer->start == 2) return TOK_IF;
                else return TOK_IDENTIFIER;
            default:
                break;
        }
    }
    goto ret;
l:
    return keyword(lexer, 1, 3, "oop", TOK_LOOP);
n:
    return keyword(lexer, 1, 2, "il", TOK_NIL);
o:
    return keyword(lexer, 1, 1, "r", TOK_OR);
r:
    return keyword(lexer, 1, 5, "eturn", TOK_RETURN);
s:
    if(lexer->_current - lexer->start > 1) {
        switch(lexer->start[1]) {
            case 'u':
                return keyword(lexer, 2, 3, "per", TOK_SUPER);
            case 'e':
                return keyword(lexer, 2, 2, "lf", TOK_SELF);
            case 'w':
                return keyword(lexer, 2, 4, "itch", TOK_SWITCH);
            default:
                break;
        }
    }
    goto ret;
t:
    return keyword(lexer, 1, 3, "rue", TOK_TRUE);
v:
    return keyword(lexer, 1, 2, "ar", TOK_VAR);
w:
    return keyword(lexer, 1, 4, "hile", TOK_WHILE);

#else
    switch(*lexer->start) {
        case 'a':
            return keyword(lexer, 1, 2, "nd", TOK_AND);
        case 'b':
            return keyword(lexer, 1, 4, "reak", TOK_BREAK);
        case 'c':
            if(lexer->_current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'a':
                        return keyword(lexer, 2, 2, "se", TOK_CASE);
                    case 'l':
                        return keyword(
                            lexer,
                            2,
                            3,
                            "ass", // Lmao
                            TOK_CLASS);
                    case 'o':
                        return keyword(lexer, 2, 6, "ntinue", TOK_CONTINUE);
                    default:
                        break;
                }
            }
            break;
        case 'd':
            return keyword(lexer, 1, 6, "efault", TOK_DEFAULT);
        case 'e':
            return keyword(lexer, 1, 3, "lse", TOK_ELSE);
        case 'f':
            if(lexer->_current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'a':
                        return keyword(lexer, 2, 3, "lse", TOK_FALSE);
                    case 'i':
                        return keyword(lexer, 2, 3, "xed", TOK_FIXED);
                    case 'n':
                        if(lexer->_current - lexer->start == 2) return TOK_FN;
                        else return TOK_IDENTIFIER;
                        if(lexer->_current - lexer->start > 3)
                            return keyword(lexer, 2, 5, "reach", TOK_FOREACH);
                        else return keyword(lexer, 2, 1, "r", TOK_FOR);
                    default:
                        break;
                }
            }
            break;
        case 'i':
            if(lexer->_current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'm':
                        keyword(lexer, 2, 2, "pl", TOK_IMPL);
                    case 'n':
                        if(lexer->_current - lexer->start == 2) return TOK_IN;
                        else return TOK_IDENTIFIER;
                    case 'f':
                        if(lexer->_current - lexer->start == 2) return TOK_IF;
                        else return TOK_IDENTIFIER;
                    default:
                        break;
                }
            }
            break;
        case 'l':
            return keyword(lexer, 1, 3, "oop", TOK_LOOP);
        case 'n':
            return keyword(lexer, 1, 2, "il", TOK_NIL);
        case 'o':
            return keyword(lexer, 1, 1, "r", TOK_OR);
        case 'r':
            return keyword(lexer, 1, 5, "eturn", TOK_RETURN);
        case 's':
            if(lexer->_current - lexer->start > 1) {
                switch(lexer->start[1]) {
                    case 'u':
                        return keyword(lexer, 2, 3, "per", TOK_SUPER);
                    case 'e':
                        return keyword(lexer, 2, 2, "lf", TOK_SELF);
                    case 'w':
                        return keyword(lexer, 2, 4, "itch", TOK_SWITCH);
                    default:
                        break;
                }
            }
            break;
        case 't':
            return keyword(lexer, 1, 3, "rue", TOK_TRUE);
        case 'v':
            return keyword(lexer, 1, 2, "ar", TOK_VAR);
        case 'w':
            return keyword(lexer, 1, 4, "hile", TOK_WHILE);
        default:
            break;
    }
#endif
ret:
    return TOK_IDENTIFIER;
}

static force_inline Token idtoken(Lexer* lexer)
{
    char c;
    while(isalnum((c = peek(lexer))) || c == '_')
        advance(lexer);
    return token(lexer, TokenType_identifier(lexer));
}

static force_inline void skipws(Lexer* lexer)
{
    while(true) {
        switch(peek(lexer)) {
            case '\n':
                nextchar(lexer);
                break;
            case ' ':
            case '\r':
            case '\t':
                nextchar(lexer);
                break;
            case '/':
                if(peek_next(lexer) == '/')
                    while(peek(lexer) != '\n' && !isend(lexer))
                        nextchar(lexer);
                else return;
                break;
            default:
                return;
        }
    }
}

static bool lmatch(Lexer* lexer, char c)
{
    if(isend(lexer) || c != peek(lexer)) return false;
    lexer->_current++;
    return true;
}

Token scan(Lexer* lexer)
{
    skipws(lexer);
    lexer->start = lexer->_current;
    if(isend(lexer)) return token(lexer, TOK_EOF);
    int c = nextchar(lexer);
    if(c == '_' || isalpha(c)) return idtoken(lexer);
    if(isdigit(c)) {
        if(c == '0') {
            if(peek(lexer) == 'x' || peek(lexer) == 'X') return hex(lexer);
            else if(isdigit(peek(lexer))) return octal(lexer);
        }
        return decimal(lexer);
    }
#ifdef SK_PRECOMPUTED_GOTO
#define ERR &&err
    // IMPORTANT: update accordingly if TokenType enum changes!
    static const void* jump_table[UINT8_MAX + 1] = {
        // order = https://www.asciitable.com
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      &&bang,   &&string, ERR,         ERR,     &&percent, ERR,       ERR,
        &&lparen, &&rparen, &&star,   &&plus,      &&comma, &&minus,   &&dot,     &&slash,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      &&colon,  &&semicolon, &&less,  &&equal,   &&greater, &&qmark,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      &&lbrack,    ERR,     &&rbrack,  &&caret,   ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      &&lbrace,    ERR,     &&rbrace,  ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
        ERR,      ERR,      ERR,      ERR,         ERR,     ERR,       ERR,       ERR,
    };
#undef ERR
    goto* jump_table[c];
lbrack:
    return token(lexer, TOK_LBRACK);
rbrack:
    return token(lexer, TOK_RBRACK);
caret:
    return token(lexer, TOK_CARET);
lparen:
    return token(lexer, TOK_LPAREN);
rparen:
    return token(lexer, TOK_RPAREN);
lbrace:
    return token(lexer, TOK_LBRACE);
rbrace:
    return token(lexer, TOK_RBRACE);
dot:;
    if(peek(lexer) == TOK_DOT && peek_next(lexer) == TOK_DOT) {
        advance(lexer);
        advance(lexer);
        return token(lexer, TOK_DOT_DOT_DOT);
    } else return token(lexer, TOK_DOT);
comma:
    return token(lexer, TOK_COMMA);
minus:
    return token(lexer, TOK_MINUS);
plus:
    return token(lexer, TOK_PLUS);
colon:
    return token(lexer, TOK_COLON);
semicolon:
    return token(lexer, TOK_SEMICOLON);
slash:
    return token(lexer, TOK_SLASH);
star:
    return token(lexer, TOK_STAR);
qmark:
    return token(lexer, TOK_QMARK);
percent:
    return token(lexer, TOK_PERCENT);
bang:
    return token(lexer, lmatch(lexer, '=') ? TOK_BANG_EQUAL : TOK_BANG);
equal:
    return token(lexer, lmatch(lexer, '=') ? TOK_EQUAL_EQUAL : TOK_EQUAL);
greater:
    return token(lexer, lmatch(lexer, '=') ? TOK_GREATER_EQUAL : TOK_GREATER);
less:
    return token(lexer, lmatch(lexer, '=') ? TOK_LESS_EQUAL : TOK_LESS);
string:
    return string(lexer);
err:
    lexerror(lexer, "Unexpected character '%c'.", c);
    return token(lexer, TOK_ERROR);
    unreachable;
#else
    switch(c) {
        case '[':
            return Token_new(lexer, TOK_LBRACK);
        case ']':
            return Token_new(lexer, TOK_RBRACK);
        case '(':
            return Token_new(lexer, TOK_LPAREN);
        case ')':
            return Token_new(lexer, TOK_RPAREN);
        case '{':
            return Token_new(lexer, TOK_LBRACE);
        case '}':
            return Token_new(lexer, TOK_RBRACE);
        case '.':
            if(peek(lexer) == TOK_DOT && peek_next(lexer) == TOK_DOT) {
                advance(lexer);
                advance(lexer);
                return Token_new(lexer, TOK_DOT_DOT_DOT);
            } else return Token_new(lexer, TOK_DOT);
        case ',':
            return Token_new(lexer, TOK_COMMA);
        case '-':
            return Token_new(lexer, TOK_MINUS);
        case '+':
            return Token_new(lexer, TOK_PLUS);
        case ';':
            return Token_new(lexer, TOK_SEMICOLON);
        case ':':
            return Token_new(lexer, TOK_COLON);
        case '?':
            return Token_new(lexer, TOK_QMARK);
        case '/':
            return Token_new(lexer, TOK_SLASH);
        case '*':
            return Token_new(lexer, TOK_STAR);
        case '!':
            return Token_new(lexer, lmatch(lexer, '=') ? TOK_BANG_EQUAL : TOK_BANG);
        case '%':
            return Token_new(lexer, TOK_PERCENT);
        case '=':
            return Token_new(lexer, lmatch(lexer, '=') ? TOK_EQUAL_EQUAL : TOK_EQUAL);
        case '>':
            return Token_new(lexer, lmatch(lexer, '=') ? TOK_GREATER_EQUAL : TOK_GREATER);
        case '<':
            return Token_new(lexer, lmatch(lexer, '=') ? TOK_LESS_EQUAL : TOK_LESS);
        case '"':
            return string(lexer);
        default:
            return errtoken(lexer, "Unexpected character.");
    }
#endif
}
