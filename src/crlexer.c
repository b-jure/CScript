/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crlexer.h"
#include "crdebug.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crstring.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>



/* maximum size of error token string */
#define MAXERR          CR_MAXSRC
#define errlen(len)     (MIN(MAXERR, len))


/* checks for end of stream */
#define isend(c)        ((c) == CREOF)


/* fetch the next character and store it as current char */
#define advance(lx)     ((lx)->c = brgetc((lx)->br))


/* save current character into lexer buffer */
#define save(lx)        savec(lx, (lx)->c)

/* save the current character into lexer buffer and advance */
#define save_and_advance(lx)            (save(lx), advance(lx))

/* same as 'save_and_advance' except the character is 'c' */
#define savec_and_advance(lx, c)        (savec(lx, c), advance(lx))


#define lbptr(lx)       cr_reader_buff((lx)->buff)
#define lblen(lx)       cr_reader_bufflen((lx)->buff)
#define lbsize(lx)  cr_reader_buffsize((lx)->buff)
#define popc(lx)        cr_reader_buffpop((lx)->buff)



/* token string literals (static strings) */
static const char *tkstr[] = {
    "and", "break", "case", "continue", "class",
    "default", "else", "false", "for", "foreach",
    "fn", "if", "in", "inherits", "nil", "or",
    "return", "super", "self", "switch", "true",
    "let", "while", "loop", "const", "static",
    "!=", "==", ">=", "<=", "<<", ">>", "..", "...", "<eof>",
    "<number>", "<integer>", "<string>", "<identifier>"
};



/* type of digit */
typedef enum Dig {
    DigDec,
    DigHex,
    DigOct,
} Dig;



void cr_lex_setsource(cr_State *ts, Lexer *lx, BuffReader *br, OString *source)
{
    cr_assert(lx->ps != NULL);
    lx->c = brgetc(br); /* fetch first char */
    lx->line = 1;
    lx->lastline = 1;
    lx->ts = ts;
    lx->fs = NULL;
    lx->tab = NULL;
    lx->br = br;
    lx->src = source;
    cr_reader_buffresize(ts, lx->buff, CRI_MINBUFFER);
}


void cr_lex_init(cr_State *ts)
{
    int i;
    OString *s;

    /* intern all keywords */
    for (i = 0; i < NUM_KEYWORDS; i++) {
        s = cr_string_new(ts, tkstr[i]);
        s->bits = bitmask(STRKWBIT);
        s->extra = i;
        cr_gc_fix(ts, obj2gco(s));
    }
}


static void inclinenr(Lexer *lx)
{
    if (cr_unlikely(lx->line >= INT_MAX))
        cr_debug_runerror(lx->ts, "too many lines in a chunk");
    lx->line++;
}


/* forward declare for 'savec' */
static cr_noret lexerror(Lexer *lx, const char *err, int token);


/* pushes character into token buffer */
cr_sinline void savec(Lexer *lx, int c)
{
    if (lblen(lx) >= lbsize(lx)) {
        if (lbsize(lx) >= CRMAXSIZE >> 1)
            lexerror(lx, "lexical element too long", 0);
        size_t newsize = lbsize(lx) << 1;
        cr_reader_buffresize(lx->ts, lx->buff, newsize);
    }
    lbptr(lx)[lblen(lx)++] = c;
}


/* if current char matches 'c' advance */
cr_sinline int lxmatch(Lexer *lexer, int c)
{
    if (isend(lexer->c) || c != lexer->c)
        return 0;
    advance(lexer);
    return 1;
}


const char *cr_lex_tok2str(Lexer *lx, int t)
{
    cr_assert(token <= TK_IDENTIFIER);
    if (t >= FIRSTTK) {
        const char *str = tkstr[t - FIRSTTK];
        if (t < TK_EOS)
            return cr_string_pushfstring(lx->ts, "'%s'", str);
        return str;
    } else {
        if (isprint(t))
            return cr_string_pushfstring(lx->ts, "'%c'", t);
        return cr_string_pushfstring(lx->ts, "'\\%d'", t);
    }
}


static const char *lextok2str(Lexer *lx, int t)
{
    switch (t) {
    case TK_FLT: case TK_INT:
    case TK_STRING: case TK_IDENTIFIER:
        savec(lx, '\0');
        return cr_string_pushfstring(lx->ts, "'%s'", lbptr(lx));
    default:
        return cr_lex_tok2str(lx, t);
    }
}


static cr_noret lexerror(Lexer *lx, const char *err, int token)
{
    cr_State *ts;

    ts = lx->ts;
    err = cr_debug_info(ts, err, lx->src, lx->line);
    if (token)
        cr_string_pushfstring(ts, "%s near %s", err, lextok2str(lx, token));
    cr_state_throw(ts, CR_ERRSYNTAX);
}


/* external interface for 'lexerror' */
cr_noret cr_lex_syntaxerror(Lexer *lx, const char *err)
{
    lexerror(lx, err, lx->t.tk);
}


/* create new string and fix it inside of lexer htable */
OString *cr_lex_newstring(Lexer *lx, const char *str, size_t len)
{
    cr_State *ts = lx->ts;
    OString *s = cr_string_newl(ts, str, len);
    TValue *stks = s2v(ts->stacktop.p++);
    setv2s(ts, stks, s); /* temp anchor */
    cr_htable_set(lx->ts, lx->tab, stks, stks);
    ts->stacktop.p--; /* pop */
    return s;
}


/* --------------------------------------------------------------------------
 * Read comments
 * -------------------------------------------------------------------------- */

static void readcomment(Lexer *lx)
{
    while (!isend(lx->c) && lx->c != '\n')
        advance(lx);
}


static void readlongcomment(Lexer *lx)
{
readmore:
    switch (lx->c) {
    case CREOF:
        return;
    case '\n':
        advance(lx);
        inclinenr(lx);
        goto readmore;
    case '*':
        advance(lx);
        if (lx->c == '/')
            return;
        goto readmore;
    default:
        advance(lx);
        goto readmore;
    }
}


/* --------------------------------------------------------------------------
 * Read string
 * -------------------------------------------------------------------------- */

/* get new hex digit from current char */
static int hexdigit(Lexer *lx)
{
    int c = lx->c;
    if (isxdigit(c))
        return cr_string_hexvalue(c);
    return -1;
}


/* parse hex escape sequence '\xyy' */
static int eschex(Lexer *lx)
{
    advance(lx); /* skip 'x' */
    int number = 0;
    for (int i = 0; i < 2; i++) {
        int digit;
        if (cr_unlikely(isend(lx->c) || lx->c == '"'))
            lexerror(lx, "incomplete hexadecimal escape sequence", TK_STRING);
        if (cr_unlikely((digit = hexdigit(lx)) == -1))
            lexerror(lx, "invalid hexadecimal escape sequence", TK_STRING);
        number = (number << 4) | digit;
        advance(lx);
    }
    return number;
}


/* create string token and handle the escape sequences */
static void readstring(Lexer *lx, Literal *k)
{
    advance(lx); /* skip '"' */
    while (lx->c != '"') {
        switch (lx->c) {
        case CREOF:
            lexerror(lx, "unterminated string", CREOF);
            break;
        case '\r': case '\n':
            lexerror(lx, "unterminated string", TK_STRING);
            break;
        case '\\':
            advance(lx);
            switch (lx->c) {
            case '\"': case '\'': case '\\':
                save_and_advance(lx);
                break;
            case '0': savec_and_advance(lx, '\0'); break;
            case 'a': savec_and_advance(lx, '\a'); break;
            case 'b': savec_and_advance(lx, '\b'); break;
            case 'e': savec_and_advance(lx, '\x1B'); break;
            case 'f': savec_and_advance(lx, '\f'); break;
            case 'n': savec_and_advance(lx, '\n'); break;
            case 'r': savec_and_advance(lx, '\r'); break;
            case 't': savec_and_advance(lx, '\t'); break;
            case 'v': savec_and_advance(lx, '\v'); break;
            case 'x': savec(lx, cast_int(eschex(lx))); break;
            case CREOF: break; /* raise error next iteration */
            default: lexerror(lx, "unknown escape sequence", lx->c);
            }
        default: save_and_advance(lx); break;
        }
    }
    advance(lx); /* skip '"' */
    k->str = cr_lex_newstring(lx, lbptr(lx), lblen(lx));
}


/* --------------------------------------------------------------------------
 * Read number
 * -------------------------------------------------------------------------- */


/* convert lexer buffer bytes into number constant */
static int lexstr2num(Lexer *lx, Literal *k)
{
    int of;
    TValue o;

    if (cr_string_tonum(lbptr(lx), &o, &of) == 0)
        lexerror(lx, "invalid number literal", TK_FLT);
    else if (of > 0)
        lexerror(lx, "number literal overflows", TK_FLT);
    else if (of < 0)
        lexerror(lx, "number literal underflows", TK_FLT);
    if (ttisint(&o)) {
        k->i = ival(&o);
        return TK_INT;
    } else {
        cr_assert(ttisflt(&o));
        k->n = fval(&o);
        return TK_FLT;
    }
}


/*
 * Read digits, additionally allow '_' separators
 * if these are not mantissa digits denoted by 'fp'.
 */
static int auxreaddigs(Lexer *lx, Dig d, int fp)
{
    int digits = 0;
    for (;; advance(lx)) {
        if (!fp && lx->c == '_') continue;
        switch (d) {
        case DigDec: if (!isdigit(lx->c)) return digits; break;
        case DigHex: if (!isxdigit(lx->c)) return digits; break;
        case DigOct: if (!isodigit(lx->c)) return digits; break;
        default: cr_unreachable(); break;
        }
        save(lx);
        digits++;
    }
}


/*
 * Read exponent digits.
 * Exponent must have at least 1 decimal digit.
 */
static int auxreadexp(Lexer *lx)
{
    int digits;

    if (lx->c == '-' || lx->c == '+')
        save_and_advance(lx);
    if ((digits = auxreaddigs(lx, DigDec, 0) == 0))
        lexerror(lx, "exponent has no digits", TK_FLT);
    return digits;
}


/* read decimal number */
static int readdecnum(Lexer *lx, Literal *k)
{
    cr_assert(isdigit(lx->c) || lx->c == '.');
    auxreaddigs(lx, DigDec, 0);
    if (lx->c == '.') {
        save_and_advance(lx);
        auxreaddigs(lx, DigDec, 1);
        if (lx->c == 'e' || lx->c == 'E') {
            save_and_advance(lx);
            auxreadexp(lx);
        }
    }
    savec(lx, '\0');
    return lexstr2num(lx, k);
}


/* read hexadecimal number */
static int readhexnum(Lexer *lx, Literal *k)
{
    int fp, exp, digits;

    advance(lx); /* skip 'x'/'X' */
    digits = auxreaddigs(lx, DigHex, 0);
    if ((fp = lx->c == '.')) {
        save_and_advance(lx);
        if (auxreaddigs(lx, DigHex, 1) == 0 && !digits)
            lexerror(lx, "missing significand", TK_FLT);
    } else if (!digits) {
        lexerror(lx, "invalid suffix 'x' to number constant", TK_FLT);
    }
    if ((exp = (lx->c == 'p' || lx->c == 'P'))) {
        save_and_advance(lx);
        auxreadexp(lx);
    }
    if (fp && !exp)
        lexerror(lx, "missing exponent", TK_FLT);
    savec(lx, '\0');
    return lexstr2num(lx, k);
}


/* read octal number */
static int readoctnum(Lexer *lx, Literal *k)
{
    cr_assert(isodigit(lx->c));
    auxreaddigs(lx, DigOct, 0);
    savec(lx, '\0');
    return lexstr2num(lx, k);
}


/* read number */
static int readnumber(Lexer *lx, Literal *k)
{
    cr_ubyte c = lx->c; /* cache previous digit */
    save_and_advance(lx);
    if (c == '0' && (lx->c == 'x' || lx->c == 'X'))
        return readhexnum(lx, k);
    else if (c == '0' && isodigit(lx->c))
        return readoctnum(lx, k);
    else
        return readdecnum(lx, k);
}



/* --------------------------------------------------------------------------
 * Scanner
 * -------------------------------------------------------------------------- */

/* scan for tokens */
static int scan(Lexer *lx, Literal *k)
{
    int c;
    OString *s;

    cr_reader_buffreset(lx->buff);
readmore:
    switch (lx->c) {
    case ' ': case '\r': case '\t':
        advance(lx);
        goto readmore;
    case '\n':
        inclinenr(lx);
        goto readmore;
    case '#':
        advance(lx);
        readcomment(lx);
        goto readmore;
    case '/':
        advance(lx);
        if (lxmatch(lx, '/')) 
            readcomment(lx);
        else if (lxmatch(lx, '*')) 
            readlongcomment(lx);
        else 
            return '/';
        goto readmore;
    case '"':
        readstring(lx, k);
        return TK_STRING;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return readnumber(lx, k);
    case '@':
        advance(lx);
        return '@';
    case '!':
        advance(lx);
        if (lxmatch(lx, '=')) 
            return TK_NE;
        return '!';
    case '=':
        advance(lx);
        if (lxmatch(lx, '=')) 
            return TK_EQ;
        return '=';
    case '>':
        advance(lx);
        if (lxmatch(lx, '>')) 
            return TK_SHR;
        else if (lxmatch(lx, '=')) 
            return TK_GE;
        return '>';
    case '<':
        advance(lx);
        if (lxmatch(lx, '<')) 
            return TK_SHL;
        else if (lxmatch(lx, '=')) 
            return TK_LE;
        return '<';
    case '*':
        advance(lx);
        if (lxmatch(lx, '*')) 
            return TK_POW;
        return '*';
    case '.':
        save_and_advance(lx);
        if (lxmatch(lx, '.')) {
            if (lxmatch(lx, '.'))
                return TK_DOTS;
            return TK_RANGE;
        }
        if (!isdigit(lx->c)) 
            return '.';
        return readnumber(lx, k);
    case CREOF:
        return TK_EOS;
    default:
        if (isalpha(lx->c)) {
            do {
                save_and_advance(lx);
            } while (isalnum(lx->c));
            s = cr_lex_newstring(lx, lbptr(lx), lblen(lx));
            k->str = s;
            if (siskeyword(s))
                return s->extra + FIRSTTK;
            return TK_IDENTIFIER;
        }
        c = lx->c;
        advance(lx);
        return c;
    }
}


/* fetch next token into 't' */
void varstatic(Lexer *lx)
{
    lx->lastline = lx->line;
    if (lx->tahead.tk != TK_EOS) {
        lx->t = lx->tahead;
        lx->tahead.tk = TK_EOS;
    } else {
        lx->t.tk = scan(lx, &lx->t.lit);
    }
}


/* fetch next token into 'tahead' */
int cr_lex_scanahead(Lexer *lx)
{
    cr_assert(lx->t.tk != TK_EOS);
    return (lx->t.tk = scan(lx, &lx->t.lit));
}
