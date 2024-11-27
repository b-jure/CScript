/*
** clexer.c
** Scanner
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cobject.h"
#include "ctypes.h"
#include "cgc.h"
#include "clexer.h"
#include "cdebug.h"
#include "cprotected.h"
#include "cstate.h"
#include "chashtable.h"
#include "cstring.h"
#include "creader.h"


#define currIsNewline(lx)       ((lx)->c == '\r' || (lx)->c == '\n')

#define currIsEnd(lx)           ((lx)->c == CSEOF)


/* fetch the next character and store it as current char */
#define advance(lx)     ((lx)->c = brgetc((lx)->br))

/* save current character into lexer buffer */
#define save(lx)        savec(lx, (lx)->c)

/* save the current character into lexer buffer and advance */
#define save_and_advance(lx)            (save(lx), advance(lx))



static const char *tkstr[] = { /* ORDER TK */
    "and", "break", "case", "continue", "class",
    "default", "else", "false", "for", "each", "fn", "if",
    "in", "inherits", "nil", "or", "return", "super",
    "switch", "true", "while", "loop", "final", "local",
    "!=", "==", ">=", "<=", "<<", ">>", "**", "..",
    "...", "<eof>",
    "<number>", "<integer>", "<string>", "<name>"
};



/* type of digit */
typedef enum DigType {
    DigDec,
    DigHex,
    DigOct,
} DigType;



void csY_setinput(cs_State *ts, Lexer *lx, BuffReader *br, OString *source) {
    cs_assert(lx->ps != NULL);
    lx->c = brgetc(br); /* fetch first char */
    lx->line = 1;
    lx->lastline = 1;
    lx->ts = ts;
    lx->fs = NULL;
    lx->tahead.tk = TK_EOS; /* no lookahead token */
    lx->br = br;
    lx->src = source;
    lx->envname = csS_newlit(ts, CS_ENV);
    csR_buffresize(ts, lx->buff, CS_MINBUFFER);
}


void csY_init(cs_State *ts) {
    /* intern and fix all keywords */
    OString *env = csS_newlit(ts, CS_ENV); /* create env name... */
    csG_fix(ts, obj2gco(env)); /* ...and fix it */
    for (int i = 0; i < NUM_KEYWORDS; i++) { /* internalize keywords */
        OString *s = csS_new(ts, tkstr[i]);
        s->extra = i + 1;
        csG_fix(ts, obj2gco(s));
    }
}


static void inclinenr(Lexer *lx) {
    if (c_unlikely(lx->line >= MAX_INT))
        csD_runerror(lx->ts, "too many lines in a chunk");
    lx->line++;
}


/* forward declare for 'savec' */
static cs_noret lexerror(Lexer *lx, const char *err, int token);


/* pushes character into token buffer */
cs_sinline void savec(Lexer *lx, int c) {
    if (csR_bufflen(lx->buff) >= csR_buffsize(lx->buff)) {
        size_t newsize;
        if (csR_buffsize(lx->buff) >= MAXSIZE / 2)
            lexerror(lx, "lexical element too long", 0);
        newsize = csR_buffsize(lx->buff) * 2;
        csR_buffresize(lx->ts, lx->buff, newsize);
    }
    csR_buff(lx->buff)[csR_bufflen(lx->buff)++] = cast_char(c);
}


/* if current char matches 'c' advance */
cs_sinline int lxmatch(Lexer *lx, int c) {
    if (currIsEnd(lx) || c != lx->c)
        return 0;
    advance(lx);
    return 1;
}


const char *csY_tok2str(Lexer *lx, int t) {
    cs_assert(t <= TK_NAME);
    if (t >= FIRSTTK) {
        const char *str = tkstr[t - FIRSTTK];
        if (t < TK_EOS)
            return csS_pushfstring(lx->ts, "'%s'", str);
        return str;
    } else {
        if (cisprint(t))
            return csS_pushfstring(lx->ts, "'%c'", t);
        else
            return csS_pushfstring(lx->ts, "'\\%d'", t);
    }
}


static const char *lextok2str(Lexer *lx, int t) {
    switch (t) {
        case TK_FLT: case TK_INT:
        case TK_STRING: case TK_NAME: {
            savec(lx, '\0');
            return csS_pushfstring(lx->ts, "'%s'", csR_buff(lx->buff));
        }
        default: return csY_tok2str(lx, t);
    }
}


static cs_noret lexerror(Lexer *lx, const char *err, int token) {
    cs_State *ts = lx->ts;
    err = csD_addinfo(ts, err, lx->src, lx->line);
    if (token)
        csS_pushfstring(ts, "%s near %s", err, lextok2str(lx, token));
    csPR_throw(ts, CS_ERRSYNTAX);
}


/* external interface for 'lexerror' */
cs_noret csY_syntaxerror(Lexer *lx, const char *err) {
    lexerror(lx, err, lx->t.tk);
}


/* create new string and fix it inside of lexer htable */
OString *csY_newstring(Lexer *lx, const char *str, size_t len) {
    cs_State *ts = lx->ts;
    OString *s = csS_newl(ts, str, len);
    TValue *ss = s2v(ts->sp.p++);
    cs_assert(lx->tab != NULL);
    setstrval(ts, ss, s); /* assuming EXTRA_STACK */
    csH_set(lx->ts, lx->tab, ss, ss);
    ts->sp.p--; /* remove ss */
    return s;
}


/* -----------------------------------------------------------------------
** Read comments
** ----------------------------------------------------------------------- */

static void read_linecomment(Lexer *lx) {
    while (!currIsEnd(lx) && !currIsNewline(lx))
        advance(lx);
}


static void read_multilinecomment(Lexer *lx) {
readmore:
    switch (lx->c) {
    case CSEOF:
        return;
    case '\r': case '\n':
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


/* -----------------------------------------------------------------------
** Read string
** ----------------------------------------------------------------------- */


static void checkcond(Lexer *lx, int c, const char *msg) {
    if (!c) {
        if (lx->c != CSEOF)
            save_and_advance(lx); /* add current to buffer for error message */
        lexerror(lx, msg, TK_STRING);
    }
}


static int expect_hexdig(Lexer *lx){
    save_and_advance(lx);
    checkcond(lx, cisxdigit(lx->c), "hexadecimal digit expected");
    return csS_hexvalue(lx->c);
}


static int read_hexesc(Lexer *lx) {
    int hd = expect_hexdig(lx);
    hd = (hd << 4) + expect_hexdig(lx);
    csR_buffpopn(lx->buff, 2); /* remove saved chars from buffer */
    return hd;
}


/*
** This function does not verify if the UTF-8 escape sequence is
** valid, rather it only ensures the sequence is in bounds of
** UTF-8 4 byte sequence. If the escape sequence is strict UTF-8
** sequence, then it indicates that to caller through 'strict'.
*/
static unsigned long read_utf8esc(Lexer *lx, int *strict) {
    ulong r;
    int i = 4; /* chars to be removed: '\', 'u', '{', and first digit */
    cs_assert(strict != NULL);
    save_and_advance(lx); /* skip 'u' */
    if (lx->c == '[') { /* strict? */
        save_and_advance(lx); /* skip '[' */
        *strict = 1; /* indicate this is strict utf8 */
    } else
        checkcond(lx, lx->c == '{', "missing '{'");
    r = expect_hexdig(lx); /* must have at least one digit */
    /* Read up to 7 hexadecimal digits, the last digit must be less than
    ** 0x1F as the upper 5 bits are reserved for UTF-8 encoding.  */
    while (cast_void(save_and_advance(lx)), cisxdigit(lx->c)) {
        i++;
        checkcond(lx, r <= (0x7FFFFFFFu >> 4), "UTF-8 value too large");
        r = (r << 4) + csS_hexvalue(lx->c);
    }
    if (*strict)
        checkcond(lx, lx->c == ']', "missing ']'");
    else
        checkcond(lx, lx->c == '}', "missing '}'");
    advance(lx); /* skip '}' or ']' */
    csR_buffpopn(lx->buff, i); /* remove saved chars from buffer */
    return r;
}


/* 
** UTF-8 encoding lengths.
** Invalid first bytes:
** 1000XXXX(8), 1001XXXX(9), 1010XXXX(A), 1011XXXX(B)
*/
static cs_ubyte const utf8len_[] = {
/* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
   1,1,1,1,1,1,1,1,0,0,0,0,2,2,3,4
};

/* 
** Get the length of the UTF-8 encoding by looking at the most
** significant 4 bits of the first byte.
*/
#define utf8len(n)      utf8len_[(n&0xFF)>>4]


static int check_utf8(Lexer *lx, ulong n) {
    if (utf8len(n))
        lexerror(lx, "invalid first byte in UTF-8 sequence", 0);
    else if (n <= 0x7F) /* ascii? */
        return 1; /* ok; valid ascii */
    else if ((0xC280 <= n && n <= 0xDFBF) && /* 2-byte sequence... */
                ((n & 0xE0C0) == 0xC080)) /* ...and is UTF-8? */
        return 2; /* ok; valid 2-byte UTF-8 sequence */
    else if (c_unlikely(0xEDA080 <= n && n <= 0xEDBFBF)) /* surrogate? */
        lexerror(lx, "UTF-8 sequence encodes UTF-16 surrogate pair", 0);
    else if ((0xE0A080 <= n && n <= 0xEFBFBF) && /* 3-byte sequence... */
                ((n & 0xF0C0C0) == 0xE08080)) /* ...and is UTF-8? */
        return 3; /* ok; valid 3-byte UTF-8 sequence */
    else if ((0xF0908080 <= n && n <= 0xF48FBFBF) && /* 4-byte sequence... */
                ((n & 0xF8C0C0C0) == 0xF0808080)) /* ...and is UTF-8? */
        return 4; /* ok; valid 4-byte UTF-8 sequence */
    lexerror(lx, "escape sequence value is not a valid UTF-8", 0);
    return 0;
}


static void checked_utf8esc(char *buff, ulong n, int len) {
    cs_assert(n <= 0x7FFFFFFFu);
    do {
        buff[UTF8BUFFSZ - len] = cast_char(0xFF & n);
        n >>= 1;
        len--;
    } while (len > 0);
    cs_assert(n == 0);
}


static void utf8esc(Lexer *lx) {
    char buff[UTF8BUFFSZ];
    int strict;
    int n = read_utf8esc(lx, &strict);
    if (strict) { /* n should already be valid UTF-8? */
        int temp = n;
        n = check_utf8(lx, n);
        checked_utf8esc(buff, temp, n);
    } else /* otherwise create non-strict UTF-8 sequence */
        n = csS_utf8esc(buff, n);
    for (; n > 0; n--) /* add 'buff' to string */
        savec(lx, buff[UTF8BUFFSZ - n]);
}


static int read_decesc(Lexer *lx) {
    int i;
    int r = 0;
    for (i = 0; i < 3 && cisdigit(lx->c); i++) {
        r = 10 * r + lx->c - '0';
        save_and_advance(lx);
    }
    checkcond(lx, r <= UCHAR_MAX, "decimal escape too large");
    csR_buffpopn(lx->buff, i); /* remove read digits from buffer */
    return r;
}


/* create string token and handle the escape sequences */
static void read_string(Lexer *lx, Literal *k) {
    save_and_advance(lx); /* skip '"' */
    while (lx->c != '"') {
        switch (lx->c) {
            case CSEOF:
                lexerror(lx, "unterminated string", TK_EOS);
                break; /* to avoid warnings */
            case '\r': case '\n':
                lexerror(lx, "unterminated string", TK_STRING);
                break; /* to avoid warnings */
            case '\\': {
                int c;
                advance(lx);
                switch (lx->c) {
                    case '0': c = '\0'; goto read_save;
                    case 'a': c = '\a'; goto read_save;
                    case 'b': c = '\b'; goto read_save;
                    case 'f': c = '\f'; goto read_save;
                    case 'n': c = '\n'; goto read_save;
                    case 'r': c = '\a'; goto read_save;
                    case 't': c = '\a'; goto read_save;
                    case 'v': c = '\a'; goto read_save;
                    case 'e': c = '\x1B'; goto read_save;
                    case 'x': c = read_hexesc(lx); goto read_save;
                    case 'u': utf8esc(lx); goto no_save;
                    case '\"': case '\'': case '\\':
                        c = lx->c; goto read_save;
                    case '\n': case '\r':
                        inclinenr(lx); c = '\n'; goto only_save;
                    case CSEOF: goto no_save; /* raise err on next iteration */
                    default: {
                        checkcond(lx, cisdigit(lx->c), "invalid escape sequence");
                        c = read_decesc(lx); /* '\ddd' */
                        goto only_save;
                    }
                }
                read_save:
                    advance(lx);
                    /* fall through */
                only_save:
                    csR_buffpop(lx->buff); /* remove '\\' */
                    savec(lx, c);
                    /* fall through */
                no_save: break;
            }
            default: save_and_advance(lx);
        }
    }
    save_and_advance(lx); /* skip '"' */
    k->str = csY_newstring(lx, csR_buff(lx->buff)+1, csR_bufflen(lx->buff)-2);
}



/* -----------------------------------------------------------------------
** Read number
** ----------------------------------------------------------------------- */

/* convert lexer buffer bytes into number constant */
static int lexstr2num(Lexer *lx, Literal *k) {
    int ovf;
    TValue o;
    if (csS_tonum(csR_buff(lx->buff), &o, &ovf) == 0)
        lexerror(lx, "invalid number literal", TK_FLT);
    else if (ovf > 0)
        lexerror(lx, "number literal overflows", TK_FLT);
    else if (ovf < 0)
        lexerror(lx, "number literal underflows", TK_FLT);
    if (ttisint(&o)) {
        k->i = ival(&o);
        return TK_INT;
    } else {
        cs_assert(ttisflt(&o));
        k->n = fval(&o);
        return TK_FLT;
    }
}


/*
** Read digits, additionally allow '_' separators if these are
** not decimal part digits denoted by 'fp'.
*/
static int read_digits(Lexer *lx, DigType dt, int fp) {
    int digits = 0;
    for (;;) {
        if (!fp && lx->c == '_') /* digits separator? */
            goto only_read; /* skip */
        switch (dt) { /* otherwise get the digit */
            case DigDec: if (!cisdigit(lx->c)) return digits; break;
            case DigHex: if (!cisxdigit(lx->c)) return digits; break;
            case DigOct: if (!cisodigit(lx->c)) return digits; break;
            default: cs_assert(0); break;
        }
        save(lx);
    only_read:
        advance(lx);
        digits++;
    }
}


/*
** Same as 'read_digits' but checks if there is at least one 'dt'
** digit, otherwise invokes error.
*/
static int expect_digits(Lexer *lx, DigType dt, int fp) {
    int digits = read_digits(lx, dt, fp);
    if (c_unlikely(digits == 0))
        lexerror(lx, "at least one digit expected", TK_FLT);
    return digits;
}


/*
** Read exponent digits.
** Exponent must have at least 1 decimal digit.
*/
static int read_exponent(Lexer *lx) {
    if (lx->c == '-' || lx->c == '+') /* have exponent sign? */
        save_and_advance(lx); /* save and skip it */
    return expect_digits(lx, DigDec, 0);
}


/* read base 10 number */
static int read_decimalnum(Lexer *lx, Literal *k) {
    read_digits(lx, DigDec, 0);
    if (lx->c == '.') { /* have decimal point? */
        save_and_advance(lx);
        expect_digits(lx, DigDec, 1);
        if (lx->c == 'e' || lx->c == 'E') { /* have exponent symbol? */
            save_and_advance(lx); /* save and skip it */
            read_exponent(lx);
        }
    }
    savec(lx, '\0');
    return lexstr2num(lx, k);
}


/* read base 16 number */
static int read_hexadecimalnum(Lexer *lx, Literal *k) {
    int fp, exp;
    advance(lx); /* skip 'x'/'X' */
    int digits = read_digits(lx, DigHex, 0);
    if ((fp = lx->c == '.')) {
        save_and_advance(lx);
        if (c_unlikely(read_digits(lx, DigHex, 1) == 0 && !digits))
            lexerror(lx, "missing significand", TK_FLT);
    } else if (!digits) {
        lexerror(lx, "invalid suffix 'x' to number constant", TK_FLT);
    }
    if ((exp = (lx->c == 'p' || lx->c == 'P'))) {
        save_and_advance(lx);
        read_exponent(lx);
    }
    if (fp && !exp)
        lexerror(lx, "missing exponent", TK_FLT);
    savec(lx, '\0');
    return lexstr2num(lx, k);
}


/* read base 8 number */
static int read_octalnum(Lexer *lx, Literal *k) {
    read_digits(lx, DigOct, 0);
    if (c_unlikely(cisdigit(lx->c))) {

    }
    savec(lx, '\0');
    return lexstr2num(lx, k);
}


/* read number */
static int read_number(Lexer *lx, Literal *k) {
    cs_ubyte c = lx->c; /* cache previous digit */
    save_and_advance(lx); /* skip first digit */
    if (c == '0' && (lx->c == 'x' || lx->c == 'X'))
        return read_hexadecimalnum(lx, k);
    else if (c == '0' && cisdigit(lx->c))
        return read_octalnum(lx, k);
    else
        return read_decimalnum(lx, k);
}



/* -----------------------------------------------------------------------
** Scanner
** ----------------------------------------------------------------------- */

/* scan for tokens */
static int scan(Lexer *lx, Literal *k) {
    csR_buffreset(lx->buff);
    for (;;) {
        switch (lx->c) {
            case ' ': case '\t': case '\f': case '\v': {
                advance(lx);
                break;
            }
            case '\n': case '\r':  {
                advance(lx);
                inclinenr(lx);
                break;
            }
            case '#': {
                advance(lx);
                read_linecomment(lx);
                break;
            }
            case '/': {
                advance(lx);
                if (lxmatch(lx, '/')) 
                    read_linecomment(lx);
                else if (lxmatch(lx, '*')) 
                    read_multilinecomment(lx);
                else 
                    return '/';
                break;
            }
            case '"': {
                read_string(lx, k);
                return TK_STRING;
            }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                return read_number(lx, k);
            }
            case '!': {
                advance(lx);
                if (lxmatch(lx, '=')) 
                    return TK_NE;
                return '!';
            }
            case '=': {
                advance(lx);
                if (lxmatch(lx, '=')) 
                    return TK_EQ;
                return '=';
            }
            case '>': {
                advance(lx);
                if (lxmatch(lx, '>')) 
                    return TK_SHR;
                else if (lxmatch(lx, '=')) 
                    return TK_GE;
                return '>';
            }
            case '<': {
                advance(lx);
                if (lxmatch(lx, '<')) 
                    return TK_SHL;
                else if (lxmatch(lx, '=')) 
                    return TK_LE;
                return '<';
            }
            case '*': {
                advance(lx);
                if (lxmatch(lx, '*')) 
                    return TK_POW;
                return '*';
            }
            case '.': {
                save_and_advance(lx);
                if (lxmatch(lx, '.')) {
                    if (lxmatch(lx, '.'))
                        return TK_DOTS;
                    return TK_CONCAT;
                }
                if (!cisdigit(lx->c)) 
                    return '.';
                return read_number(lx, k);
            }
            case CSEOF: {
                return TK_EOS;
            }
            default: {
                if (cisalpha(lx->c)) {
                    OString *s;
                    do {
                        save_and_advance(lx);
                    } while (cisalnum(lx->c));
                    s = csY_newstring(lx, csR_buff(lx->buff),
                                          csR_bufflen(lx->buff));
                    k->str = s;
                    if (isreserved(s))
                        return s->extra + FIRSTTK - 1;
                    else
                        return TK_NAME;
                } else {
                    int c = lx->c;
                    advance(lx);
                    return c;
                }
            }
        }
    }
}


/* fetch next token into 't' */
void csY_scan(Lexer *lx) {
    lx->lastline = lx->line;
    if (lx->tahead.tk != TK_EOS) {
        lx->t = lx->tahead;
        lx->tahead.tk = TK_EOS;
    } else {
        lx->t.tk = scan(lx, &lx->t.lit);
    }
}


/* fetch next token into 'tahead' */
int csY_scanahead(Lexer *lx) {
    cs_assert(lx->t.tk != TK_EOS);
    lx->tahead.tk = scan(lx, &lx->t.lit);
    return lx->tahead.tk;
}
