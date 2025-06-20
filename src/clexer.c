/*
** clexer.c
** Scanner
** See Copyright Notice in cscript.h
*/

#define clexer_c
#define CS_CORE

#include <memory.h>

#include "cprefix.h"

#include "cobject.h"
#include "ctypes.h"
#include "cgc.h"
#include "clexer.h"
#include "cdebug.h"
#include "cprotected.h"
#include "cstate.h"
#include "ctable.h"
#include "cstring.h"
#include "creader.h"


#define currIsNewline(lx)       ((lx)->c == '\r' || (lx)->c == '\n')

#define currIsEnd(lx)           ((lx)->c == CSEOF)


/* fetch the next character and store it as current char */
#define advance(lx)     ((lx)->c = brgetc((lx)->br))

/* save current character into lexer buffer */
#define save(lx)        savec(lx, (lx)->c)

/* save the current character into lexer buffer and advance */
#define save_and_advance(lx)        (save(lx), advance(lx))


static const char *tkstr[] = { /* ORDER TK */
    "and", "break", "case", "continue", "class",
    "default", "else", "false", "for", "foreach", "fn", "if",
    "in", "inherits", "nil", "or", "return", "super",
    "switch", "true", "while", "loop", "local", "inf", "infinity",
    "//", "!=", "==", ">=", "<=", "<<", ">>", "**", "..",
    "...", "<eof>",
    "<number>", "<integer>", "<string>", "<name>"
};


/* type of digit */
typedef enum DigType {
    DigDec,
    DigHex,
    DigOct,
} DigType;


void csY_setinput(cs_State *C, Lexer *lx, BuffReader *br, OString *source) {
    cs_assert(lx->ps != NULL);
    lx->c = brgetc(br); /* fetch first char */
    lx->line = 1;
    lx->lastline = 1;
    lx->C = C;
    lx->fs = NULL;
    lx->tahead.tk = TK_EOS; /* no lookahead token */
    lx->br = br;
    lx->src = source;
    lx->envn = csS_newlit(C, CS_ENV);
    csR_buffresize(C, lx->buff, CSI_MINBUFFER);
}


void csY_init(cs_State *C) {
    OString *e = csS_newlit(C, CS_ENV); /* create env name */
    csG_fix(C, obj2gco(e)); /* never collect this name */
    /* create keyword names and never collect them */
    cs_assert(NUM_KEYWORDS <= MAXBYTE);
    for (int i = 0; i < NUM_KEYWORDS; i++) {
        OString *s = csS_new(C, tkstr[i]);
        s->extra = cast_byte(i + 1);
        csG_fix(C, obj2gco(s));
    }
}


/* forward declare */
static c_noret lexerror(Lexer *lx, const char *err, int token);


/* pushes character into token buffer */
c_sinline void savec(Lexer *lx, int c) {
    if (csR_bufflen(lx->buff) >= csR_buffsize(lx->buff)) {
        size_t newsize;
        if (csR_buffsize(lx->buff) >= MAXSIZE / 2)
            lexerror(lx, "lexical element too long", 0);
        newsize = csR_buffsize(lx->buff) * 2;
        csR_buffresize(lx->C, lx->buff, newsize);
    }
    csR_buff(lx->buff)[csR_bufflen(lx->buff)++] = cast_char(c);
}


/* if current char matches 'c' advance */
c_sinline int lxmatch(Lexer *lx, int c) {
    if (c == lx->c) {
        advance(lx);
        return 1;
    }
    return 0;
}


const char *csY_tok2str(Lexer *lx, int t) {
    cs_assert(t <= TK_NAME);
    if (t >= FIRSTTK) {
        const char *str = tkstr[t - FIRSTTK];
        if (t < TK_EOS)
            return csS_pushfstring(lx->C, "'%s'", str);
        return str;
    } else {
        if (cisprint(t))
            return csS_pushfstring(lx->C, "'%c'", t);
        else
            return csS_pushfstring(lx->C, "'\\%d'", t);
    }
}


static const char *lextok2str(Lexer *lx, int t) {
    switch (t) {
        case TK_FLT: case TK_INT:
        case TK_STRING: case TK_NAME: {
            savec(lx, '\0');
            return csS_pushfstring(lx->C, "'%s'", csR_buff(lx->buff));
        }
        default: return csY_tok2str(lx, t);
    }
}


static const char *lexmsg(Lexer *lx, const char *msg, int token) {
    cs_State *C = lx->C;
    msg = csD_addinfo(C, msg, lx->src, lx->line);
    if (token)
        msg = csS_pushfstring(C, "%s near %s", msg, lextok2str(lx, token));
    return msg;
}


static void lexwarn(Lexer *lx, const char *msg, int token) {
    cs_State *C = lx->C;
    SPtr oldsp = C->sp.p;
    csT_warning(C, lexmsg(lx, msg, token), 0);
    C->sp.p = oldsp; /* remove warning messages */
}


static c_noret lexerror(Lexer *lx, const char *err, int token) {
    lexmsg(lx, err, token);
    csPR_throw(lx->C, CS_STATUS_ESYNTAX);
}


/* external interface for 'lexerror' */
c_noret csY_syntaxerror(Lexer *lx, const char *err) {
    lexerror(lx, err, lx->t.tk);
}


static void inclinenr(Lexer *lx) {
    int old_c = lx->c;
    cs_assert(currIsNewline(lx));
    advance(lx); /* skip '\n' or '\r' */
    if (currIsNewline(lx) && lx->c != old_c) /* have "\r\n" or "\n\r"? */
        advance(lx); /* skip it */
    if (c_unlikely(++lx->line >= MAXINT))
        lexerror(lx, "too many lines in a chunk", 0);
}


/* create new string and fix it inside of lexer htable */
OString *csY_newstring(Lexer *lx, const char *str, size_t l) {
    cs_State *C = lx->C;
    OString *s = csS_newl(C, str, l);
    const TValue *o = csH_getstr(lx->tab, s);
    if (!ttisnil(o)) /* string already present? */
        s = keystrval(cast(Node *, o));
    else {
        TValue *stkv = s2v(C->sp.p++); /* reserve stack space for string */
        setstrval(C, stkv, s); /* anchor */
        csH_setstr(C, lx->tab, strval(stkv), stkv); /* t[string] = string */
        csG_checkGC(C);
        C->sp.p--; /* remove string from stack */
    }
    return s;
}


/* -----------------------------------------------------------------------
** Read comments
** ----------------------------------------------------------------------- */

/* read single line comment */
static void read_comment(Lexer *lx) {
    while (!currIsEnd(lx) && !currIsNewline(lx))
        advance(lx);
}


/* read comment potentially spanning multiple lines */
static void read_longcomment(Lexer *lx) {
    for (;;) {
        switch (lx->c) {
            case CSEOF: return;
            case '\r': case '\n':
                inclinenr(lx); break;
            case '*':
                advance(lx);
                if (lxmatch(lx, '/'))
                    return;
                break;
            default: advance(lx); break;
        }
    }
}


/* -----------------------------------------------------------------------
** Read string
** ----------------------------------------------------------------------- */


static void checkcond(Lexer *lx, int cond, const char *msg) {
    if (c_unlikely(!cond)) { /* condition fails? */
        if (lx->c != CSEOF) /* not end-of-file? */
            save_and_advance(lx); /* add current char to buffer for err msg */
        lexerror(lx, msg, TK_STRING); /* invoke syntax error */
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
    c_ulong r;
    int i = 4; /* chars to be removed: '\', 'u', '{', and first digit */
    cs_assert(strict != NULL);
    *strict = 0;
    save_and_advance(lx); /* skip 'u' */
    if (lx->c == '[') /* strict? */
        *strict = 1; /* indicate this is strict utf8 */
    else
        checkcond(lx, lx->c == '{', "missing '{'");
    r = expect_hexdig(lx); /* must have at least one digit */
    /* Read up to 7 hexadecimal digits (last digit is reserved for UTF-8) */
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
static c_byte const utf8len_[] = {
/* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
   1,1,1,1,1,1,1,1,0,0,0,0,2,2,3,4
};

/* 
** Get the length of the UTF-8 encoding by looking at the most
** significant 4 bits of the first byte.
*/
#define utf8len(n)      utf8len_[((n) & 0xFF) >> 4]


static int check_utf8(Lexer *lx, c_ulong n) {
    if (!utf8len(n))
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
    lexerror(lx, "escape sequence is not a valid UTF-8", 0);
    return 0;
}


static void utf8verfied(char *buff, c_ulong n, int len) {
    int i = 1; /* number of bytes in the buffer */
    cs_assert(n <= 0x7FFFFFFFu);
    do {
        buff[UTF8BUFFSZ - i++] = cast_char(0xFF & n);
        n >>= 8; /* fetch next byte in the sequence (if any) */
    } while (--len > 0);
    cs_assert(n == 0);
}


static void utf8esc(Lexer *lx) {
    char buff[UTF8BUFFSZ];
    int strict;
    int n = read_utf8esc(lx, &strict);
    if (strict) { /* n should already be valid UTF-8? */
        int temp = n;
        n = check_utf8(lx, n);
        utf8verfied(buff, temp, n);
    } else /* otherwise create non-strict UTF-8 sequence */
        n = csS_utf8esc(buff, n);
    for (; n > 0; n--) /* add 'buff' to string */
        savec(lx, buff[UTF8BUFFSZ - n]);
}


static int read_decesc(Lexer *lx) {
    int i;
    int r = 0;
    for (i = 0; i < 3 && cisdigit(lx->c); i++) {
        r = 10 * r + ctodigit(lx->c);
        save_and_advance(lx);
    }
    checkcond(lx, r <= UCHAR_MAX, "decimal escape too large");
    csR_buffpopn(lx->buff, i); /* remove read digits from buffer */
    return r;
}


static size_t skipsep(Lexer *lx) {
    size_t count = 0;
    int s = lx->c;
    cs_assert(s == '[' || s == ']');
    save_and_advance(lx);
    while (lx->c == '=') {
        save_and_advance(lx);
        count++;
    }
    return (lx->c == s) ? count + 2
                        : (count == 0) ? 1 : 0;
}


// TODO: add docs for long strings
static void read_long_string(Lexer *lx, Literal *k, size_t sep) {
    int line = lx->line; /* initial line */
    save_and_advance(lx); /* skip second '[' */
    if (currIsNewline(lx)) /* string starts with a newline? */
        inclinenr(lx); /* skip it */
    for (;;) {
        switch (lx->c) {
            case CSEOF: { /* error */
                const char *msg = csS_pushfstring(lx->C,
                    "unterminated long string (starting at line %d)", line);
                lexerror(lx, msg, TK_EOS);
                break; /* to avoid warnings */
            }
            case ']': {
                if (skipsep(lx) == sep) {
                    save_and_advance(lx); /* skip 2nd ']' */
                    goto endloop;
                }
                break;
            }
            case '\n': case '\r': {
                savec(lx, '\n');
                inclinenr(lx);
                break;
            }
            default: {
                save_and_advance(lx);
                break;
            }
        }
    }
endloop:
    k->str = csY_newstring(lx, csR_buff(lx->buff) + sep,
                               csR_bufflen(lx->buff) - 2 * sep);
}


/* create string token and handle the escape sequences */
static void read_string(Lexer *lx, Literal *k) {
    save_and_advance(lx); /* skip '"' */
    while (lx->c != '"') {
        switch (lx->c) {
            case CSEOF: /* end of file */
                lexerror(lx, "unterminated string", TK_EOS);
                break; /* to avoid warnings */
            case '\r': case '\n': /* newline */
                lexerror(lx, "unterminated string", TK_STRING);
                break; /* to avoid warnings */
            case '\\': { /* escape sequences */
                int c; /* final character to be saved */
                save_and_advance(lx); /* keep '\\' for error messages */
                switch (lx->c) {
                    case 'a': c = '\a'; goto read_save;
                    case 'b': c = '\b'; goto read_save;
                    case 't': c = '\t'; goto read_save;
                    case 'n': c = '\n'; goto read_save;
                    case 'v': c = '\v'; goto read_save;
                    case 'f': c = '\f'; goto read_save;
                    case 'r': c = '\r'; goto read_save;
                    case 'e': c = '\x1B'; goto read_save;
                    case 'x': c = read_hexesc(lx); goto read_save;
                    case 'u': utf8esc(lx); goto no_save;
                    case '\n': case '\r':
                              inclinenr(lx); c = '\n'; goto only_save;
                    case '\"': case '\'': case '\\':
                              c = lx->c; goto read_save;
                    case CSEOF: goto no_save; /* raise err on next iter. */
                    default: {
                        checkcond(lx, cisdigit(lx->c),
                                "invalid escape sequence");
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
            no_save:
                break;
            }
            default: save_and_advance(lx);
        }
    } /* while byte is not a delimiter */
    save_and_advance(lx); /* skip delimiter */
    k->str = csY_newstring(lx, csR_buff(lx->buff)+1, csR_bufflen(lx->buff)-2);
}



/* -----------------------------------------------------------------------
** Read number
** ----------------------------------------------------------------------- */

/* reads a literal character */
static int read_char(Lexer *lx, Literal *k) {
    int c;
    save_and_advance(lx); /* skip ' */
    switch (lx->c) {
        case CSEOF:
            lexerror(lx, "unterminated character constant", TK_EOS);
            break; /* to avoid warnings */
        case '\r': case '\n':
            lexerror(lx, "unterminated character constant", TK_INT);
        case '\\': {
            save_and_advance(lx); /* keep '\\' for error messages */
            switch (lx->c) {
                case 'a': c = '\a'; break;
                case 'b': c = '\b'; break;
                case 't': c = '\t'; break;
                case 'n': c = '\n'; break;
                case 'v': c = '\v'; break;
                case 'f': c = '\f'; break;
                case 'r': c = '\r'; break;
                case 'x': c = read_hexesc(lx); break;
                case '\"': case '\'': case '\\':
                    c = lx->c; break;
                default: { /* error */
                    checkcond(lx, 0, "invalid escape sequence");
                    return 0; /* unreachable; to avoid warnings */
                }
            }
            csR_buffpop(lx->buff);
            break;
        }
        default: {
            c = cast_byte(lx->c);
            break;
        }
    }
    savec(lx, c);
    advance(lx);
    if (c_unlikely(lx->c != '\''))
        lexerror(lx, "malformed character constant", TK_INT);
    advance(lx);
    k->i = c;
    return TK_INT;
}


/* convert lexer buffer bytes into number constant */
static int lexstr2num(Lexer *lx, Literal *k) {
    int f; /* flag for float overflow; -1 underflow; 1 overflow; 0 ok */
    TValue o;
    savec(lx, '\0'); /* terminate */
    if (c_unlikely(csS_tonum(csR_buff(lx->buff), &o, &f) == 0))
        lexerror(lx, "malformed number", TK_FLT);
    if (c_unlikely(f > 0)) /* overflow? */
        lexwarn(lx, "number constant overflows", TK_FLT);
    else if (c_unlikely(f < 0)) /* underflow? */
        lexwarn(lx, "number constant underflows", TK_FLT);
    if (ttisint(&o)) { /* integer constant? */
        k->i = ival(&o);
        return TK_INT;
    } else { /* otherwise float constant */
        cs_assert(ttisflt(&o));
        k->n = fval(&o);
        return TK_FLT;
    }
}


static int check_next(Lexer *lx, int c) {
    if (lx->c == c) {
        advance(lx);
        return 1;
    } else
        return 0;
}


static int check_next2(Lexer *lx, const char *set) {
    if (lx->c == set[0] || lx->c == set[1]) {
        save_and_advance(lx);
        return 1;
    } else
        return 0;
}


/*
** Read digits, additionally allow '_' separators if these are
** not digits in the fractional part denoted by 'frac'.
*/
static int read_digits(Lexer *lx, DigType dt, int frac) {
    int digits = 0;
    for (;;) {
        if (!frac && check_next(lx, '_')) /* separator? */
            continue; /* skip */
        switch (dt) { /* otherwise get the digit */
            case DigDec: if (!cisdigit(lx->c)) return digits; break;
            case DigHex: if (!cisxdigit(lx->c)) return digits; break;
            case DigOct: if (!cisodigit(lx->c)) return digits; break;
            default: cs_assert(0); break; /* invalid 'dt' */
        }
        save_and_advance(lx);
        digits++;
    }
}


/* read exponent digits (in base 10) */
static int read_exponent(Lexer *lx) {
    int gotzero = 0;
    check_next2(lx, "-+"); /* optional sign */
    if (check_next(lx, '0')) { /* leading zero? */
        gotzero = 1;
        while (lx->c == '_' || lx->c == '0')
            advance(lx); /* skip separators and leading zeros */
    }
    if (cisalpha(lx->c)) { /* exponent touching a letter? */
        save_and_advance(lx); /* force an error */
        return 0; /* no digits */
    } else if (cisdigit(lx->c)) { /* got at least one non-zero digit? */
        return read_digits(lx, DigDec, 0);
    } else if (c_unlikely(!gotzero)) { /* no digits? */
        lexerror(lx, "at least one exponent digit expected", TK_FLT);
    } else { /* one or more leading zeros */
        savec(lx, '0'); /* save only one leading zero */
        return 1; /* (one zero) */
    }
}


/* read base 10 (decimal) numeral */
static int read_decnum(Lexer *lx, Literal *k, int c) {
    int fp = (c == '.'); /* check if '.' is first */
    int ndigs = read_digits(lx, DigDec, fp) + !fp;
    if (!fp && lx->c == '.') { /* have fractional part? ('.' was not first) */
        save_and_advance(lx); /* skip '.' */
        ndigs += read_digits(lx, DigDec, 1);
    }
    if (check_next2(lx, "eE")) { /* have exponent? */
        if (c_unlikely(fp && !ndigs)) /* no integral or fractional part? */
            lexerror(lx, "no digits in integral or fractional part", TK_FLT);
        if (read_exponent(lx) == 0) /* error? */
            goto convert;
    }
    if (cisalpha(lx->c)) /* numeral touching a letter? */
        save_and_advance(lx); /* force an error */
convert:
    return lexstr2num(lx, k);
}


/* read base 16 (hexadecimal) numeral */
static int read_hexnum(Lexer *lx, Literal *k) {
    int ndigs = 0;
    int gotexp, gotrad;
    if (cisxdigit(lx->c)) /* can't have separator before first digit */
        ndigs = read_digits(lx, DigHex, 0);
    if ((gotrad = lx->c == '.')) {
        save_and_advance(lx);
        if (c_unlikely(!read_digits(lx, DigHex, 1) && !ndigs))
            lexerror(lx,"hexadecimal constant expects at least one digit", TK_FLT);
    } else if (!ndigs)
        lexerror(lx, "invalid suffix 'x|X' on integer constant", TK_FLT);
    if ((gotexp = check_next2(lx, "pP"))) { /* have exponent? */
        if (read_exponent(lx) == 0) /* error? */
            goto convert;
    }
    if (c_unlikely(gotrad && !gotexp))
        lexerror(lx, "hexadecimal float constant is missing exponent 'p|P'", TK_FLT);
    if (cisalpha(lx->c)) /* numeral touching a letter? */
        save_and_advance(lx); /* force an error */
convert:
    return lexstr2num(lx, k);
}


/* read base 8 (octal) numeral */
static int read_octnum(Lexer *lx, Literal *k) {
    int digits = read_digits(lx, DigOct, 0);
    if (digits == 0 || cisdigit(lx->c)) /* no digits or has decimal digit? */
        return read_decnum(lx, k, lx->c); /* try as decimal numeral */
    else if (cisalpha(lx->c)) /* octal numeral touching a letter? */
        save_and_advance(lx); /* force an error */
    return lexstr2num(lx, k);
}


/* read a numeral string */
static int read_numeral(Lexer *lx, Literal *k) {
    int c = lx->c;
    save_and_advance(lx);
    if (c == '0' && check_next2(lx, "xX"))
        return read_hexnum(lx, k);
    else if (c == '0' && cisdigit(lx->c))
        return read_octnum(lx, k);
    else
        return read_decnum(lx, k, c);
}


/* {======================================================================
** Scanner
** ======================================================================= */

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
                inclinenr(lx);
                break;
            }
            case '#': {
                advance(lx);
                read_comment(lx);
                break;
            }
            case '/': { // TODO: add docs (single line comment is also '///')
                advance(lx);
                if (lxmatch(lx, '/')) {
                    if (lxmatch(lx, '/'))
                        read_comment(lx);
                    else
                        return TK_IDIV;
                } else if (lxmatch(lx, '*'))
                    read_longcomment(lx);
                else 
                    return '/';
                break;
            }
            case '"': {
                read_string(lx, k);
                return TK_STRING;
            }
            case '[': {
                size_t sep = skipsep(lx);
                if (sep >= 2) {
                    read_long_string(lx, k, sep);
                    return TK_STRING;
                } else if (sep == 0) /* '[=...' missing second bracket? */
                    lexerror(lx, "invalid long string delimiter", TK_STRING);
                return '[';
            }
            case '\'': {
                return read_char(lx, k);
            }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                return read_numeral(lx, k);
            }
            case '!': {
                advance(lx);
                if (lxmatch(lx, '=')) 
                    return TK_NE;
                else
                    return '!';
            }
            case '=': {
                advance(lx);
                if (lxmatch(lx, '=')) 
                    return TK_EQ;
                else
                    return '=';
            }
            case '>': {
                advance(lx);
                if (lxmatch(lx, '>')) 
                    return TK_SHR;
                else if (lxmatch(lx, '=')) 
                    return TK_GE;
                else
                    return '>';
            }
            case '<': {
                advance(lx);
                if (lxmatch(lx, '<')) 
                    return TK_SHL;
                else if (lxmatch(lx, '=')) 
                    return TK_LE;
                else
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
                    else
                        return TK_CONCAT;
                }
                if (!cisdigit(lx->c)) 
                    return '.';
                else
                    return read_decnum(lx, k, '.');
            }
            case CSEOF: {
                return TK_EOS;
            }
            default: {
                if (!cisalpha(lx->c) && lx->c != '_') {
                    int c = lx->c;
                    advance(lx);
                    return c;
                } else {
                    do {
                        save_and_advance(lx);
                    } while (cisalnum(lx->c) || lx->c == '_');
                    k->str = csY_newstring(lx, csR_buff(lx->buff),
                                               csR_bufflen(lx->buff));
                    if (isreserved(k->str)) { /* reserved keywword? */
                        int tk = k->str->extra + FIRSTTK - 1;
                        if (tk == TK_INF || tk == TK_INFINITY)
                            return lexstr2num(lx, k);
                        else
                            return tk;
                    } else /* identifier */
                        return TK_NAME;
                }
            }
        }
    }
}


/* fetch next token into 't' */
void csY_scan(Lexer *lx) {
    lx->lastline = lx->line;
    if (lx->tahead.tk == TK_EOS)
        lx->t.tk = scan(lx, &lx->t.lit);
    else {
        lx->t = lx->tahead;
        lx->tahead.tk = TK_EOS;
    }
}


/* fetch next token into 'tahead' */
int csY_scanahead(Lexer *lx) {
    cs_assert(lx->t.tk != TK_EOS);
    lx->tahead.tk = scan(lx, &lx->t.lit);
    return lx->tahead.tk;
}

/* }====================================================================== */
