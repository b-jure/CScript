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

#include <ctype.h>
#include <stdio.h>
#include <string.h>



/* maximum size of error token string */
#define MAXERR		CR_MAXSRC
#define errlen(len)	(MIN(MAXERR, len))


/* checks for end of stream */
#define isend(c)	((c) == CREOF)


/* increment line number if 'c' is newline character */
#define incrline(lx, c) \
	((c) == '\n' || (c) == '\r' ? ((lx)->currline++, (c)) : (c))


/* 
 * Fetch new character from BuffReader, also increment
 * the line number if newline character was fetched. 
 */
#define nextchar(lx)	incrline(lx, brgetc((lx)->br))


/* fetch the next character and store it as current char */
#define advance(lx)	((lx)->c = nextchar(lx))


/* go back one character */
#define goback(lx, oldc)	(brungetc((lx)->br), (lx)->c = (oldc))


/* push current character into lexer buffer */
#define pushl(lx)	pushc(lx, (lx)->c)

/* push the current character into lexer buffer and advance */
#define advance_and_push(lx)	(pushl(lx), advance(lx))

/* same as 'advance_and_push' except the character is 'c' */
#define advance_and_pushc(lx, c)	(pushc(lx, c), advance(lx))


/* pointer to start of lexer buffer */
#define lbptr(lx)	((lx)->buff.ptr)

/* length of lexer buffer */
#define lblen(lx)	((lx)->buff.len)

/* pop character from lexer buffer */
#define popc(lx)	(lbptr(lx)[--lblen(lx)])






static const char *lexerrors[] = {
	"\tIncomplete hex escape sequence.\n",
	"\tInvalid hexadecimal escape sequence.\n",
	"\tUnterminated string.\n",
	"\tToo large number constant '%.*s'...\n",
	"\tInvalid digit '%c' in octal number.\n",
	"\tInvalid hexadecimal constant.\n",
	"\tInvalid decimal constant.\n",
	"\tUnkown escape sequence '\\%c'.\n",
	"\tToken contains too many characters, limit is %d.\n",
};


static const char *tkstr[] = {
	"and", "break", "case", "continue", "class",
	"default", "else", "false", "for", "foreach",
	"fn", "if", "in", "inherits", "nil", "or",
	"return", "super", "self", "switch", "true",
	"let", "while", "loop", "const",
	"!=", "==", ">=", "<=", "<<", ">>", "...", "<eof>",
	"<number>", "<integer>", "<string>", "<identifier>"
};


void cr_lr_init(VM *vm, Lexer *lx, BuffReader *br, OString *source)
{
	int i;

	lx->vm = vm;
	lx->fs = NULL;
	lx->br = br;
	lx->src = source;
	lx->c = brgetc(lx->br); /* prime reader */
	lx->currline = 1;
	lx->prevline = 1;
	lx->skip = 0;
	cr_mm_createvec(vm, &lx->buff, CRMAXSIZE<<1, "token");
	cr_mm_reallocvec(vm, &lx->buff, CRI_MINBUFFER);
	/* intern keywords */
	for (i = 0; i < NUM_KEYWORDS; i++)
		cr_ht_intern(lx->vm, tkstr[i]);
}


void cr_lr_free(Lexer *lx)
{
	freevec(lx->vm, &lx->buff);
}


/* pushes character into token buffer */
cr_sinline void pushc(Lexer *lx, int c)
{
	cr_mm_growvec(lx->vm, &lx->buff);
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


cr_sinline void skipcomment(Lexer *lx)
{
	while (!isend(lx->c) && lx->c != '\n')
		advance(lx);
}


cr_sinline void skiplcomment(Lexer *lx) 
{
	while (!isend(lx->c)) {
		advance(lx);
		if (lxmatch(lx, '*') && lx->c == '/')
			break;
	}
}


/* skip spaces and comments */
static void skipws(Lexer *lx)
{
read_more:
	switch (lx->c) {
	case '\n':
	case ' ':
	case '\r':
	case '\t':
		advance(lx);
		goto read_more;
	case '#':
		advance(lx);
		goto comment;
	case '/':
		advance(lx);
		if (lxmatch(lx, '/')) {
			goto comment;
		} else if (lxmatch(lx, '*')) {
			skiplcomment(lx);
			goto read_more;
		}
		goback(lx, '/');
		/* FALLTHRU */
	default:
		break;
	comment:
		skipcomment(lx);
		goto read_more;
	}
}


const char *cr_lr_tok2str(Lexer *lx, int token)
{
	const char *str;

	cr_assert(token <= TK_IDENTIFIER);
	if (token >= FIRSTTK) {
		str = tkstr[token - FIRSTTK];
		if (token < TK_EOS) {
			return cr_ot_pushfstring(lx->vm, "'%s'", str);
		} else {
			return str;
		}
	} else {
		if (isprint(token))
			return cr_ot_pushfstring(lx->vm, "'%c'", token);
		else
			return cr_ot_pushfstring(lx->vm, "'\\%d'", token);
	}
}


static const char *tokstr(Lexer *lx, int token)
{
	switch (token) {
		case TK_FLT: case TK_INT:
		case TK_STRING: case TK_IDENTIFIER:
			pushc(lx, '\0');
			return cr_ot_pushfstring(lx->vm, "'%s'", lbptr(lx));
		default:
			return cr_lr_tok2str(lx, token);
	}
}


static cr_noret lexerror(Lexer *lx, const char *err, int token)
{
	VM *vm;

	vm = lx->vm;
	err = cr_dg_info(vm, err, lx->src, lx->currline);
	if (token)
		cr_ot_pushfstring(vm, "%s near %s", err, tokstr(lx, token));
	cr_dg_throw(vm, CR_ERRSYNTAX);
}


/* external interface for 'lexerror' */
void cr_lr_syntaxerror(Lexer *lx, const char *err)
{
	lexerror(lx, err, lx->current.tk);
}


/* get new hex digit from current char */
static int hexdigit(Lexer *lx)
{
	int c;

	c = lx->c;
	if (isxdigit(c))
		return cr_ot_hexvalue(c);
	return -1;
}


/* parse hex escape sequence '\x' */
static int eschex(Lexer *lx)
{
	int i;
	int number;
	int digit;

	advance(lx); /* skip 'x' */
	number = 0;
	for (i = 0; i < 2; i++) {
		if (cr_unlikely(isend(lx->c) || lx->c == '"'))
			cr_lr_syntaxerror(lx, "incomplete hex escape sequence");
		digit = hexdigit(lx);
		if (cr_unlikely(digit == -1))
			cr_lr_syntaxerror(lx, "invalid hexadecimal escape sequence");
		number = (number << 4) | digit;
		advance(lx);
	}
	return number;
}


/* create string token and handle the escape sequences */
static void readstring(Lexer *lx)
{
	advance(lx); /* skip '"' */
	for (;;) {
		if (cr_unlikely(isend(lx->c) || lx->c == '\n' || lx->c == '\r'))
			cr_lr_syntaxerror(lx, "string not terminated");
		if (lx->c == '"')
			break;
		if (lx->c == '\\') {
			advance(lx);
			switch (lx->c) {
			case '"':
			case '\'':
			case '%':
			case '\\':
			case '?':
				advance_and_push(lx);
				break;
			case '0':
				advance_and_pushc(lx, '\0');
				break;
			case 'a':
				advance_and_pushc(lx, '\a');
				break;
			case 'b':
				advance_and_pushc(lx, '\b');
				break;
			case 'e':
				advance_and_pushc(lx, '\33');
				break;
			case 'f':
				advance_and_pushc(lx, '\f');
				break;
			case 'n':
				advance_and_pushc(lx, '\n');
				break;
			case 'r':
				advance_and_pushc(lx, '\r');
				break;
			case 't':
				advance_and_pushc(lx, '\t');
				break;
			case 'v':
				advance_and_pushc(lx, '\v');
				break;
			case 'x': {
				pushc(lx, cast_int(eschex(lx)));
				break;
			}
			case CREOF: /* raise error next iteration */
				break;
			default:
				cr_lr_syntaxerror(lx, "unknown escape sequence");
			}
		} else {
			pushc(lx, lx->c);
		}
	}
	advance(lx);
	lx->current.k.str = cr_ot_newstring(lx->vm, lbptr(lx), lblen(lx));
}


/* auxiliary to 'readnumber()' */
cr_sinline void readdigits(Lexer *lx)
{
	for (;; advance(lx)) {
		if (lx->c == '_')
			continue;
		if (!isdigit(lx->c))
			break;
		pushl(lx);
	}
}


/* read decimal number */
static void readdecimal(Lexer *lx)
{
	readdigits(lx);
	if (lx->c == '.') {
		advance_and_push(lx);
		while (isdigit(lx->c))
			advance_and_push(lx);
		if (lx->c == 'e' || lx->c == 'E') {
			advance_and_push(lx);
			if (lx->c == '+' || lx->c == '-')
				advance_and_push(lx);
			if (cr_unlikely(!isdigit(lx->c)))
				cr_lr_syntaxerror(lx, "invalid decimal constant");
			else {
				advance_and_push(lx);
				readdigits(lx);
			}
		}
	}
	pushc(lx, '\0');
	errno = 0;
	if (!lx->skip)
		*res = NUMBER_VAL(strtod(lbptr(lx), NULL));
	else {
		lx->skip = 0;
		*res = NUMBER_VAL(0.0);
	}
	popc(lx); // '\0'
}


/* Create number value from string in hexadecimal format */
static void hex(Lexer *lexer, Value *res)
{
	advance(lexer); // skip 'x' | 'X'
	while (isxdigit(lexer->c))
		advance_and_push(lexer);
	if (cr_unlikely(lblen(lexer) == 0))
		lexerror(lexer, lexerrors[LE_HEXCONST]);
	pushc(lexer, '\0');
	errno = 0;
	if (!lexer->skip)
		*res = NUMBER_VAL(strtoll(lbptr(lexer), NULL, 16));
	else {
		lexer->skip = 0;
		*res = NUMBER_VAL(0.0);
	}
	popc(lexer); // '\0'
}


/* Create number value from string in octal format */
static cr_inline void octal(Lexer *lexer, Value *res)
{
	while (lexer->c >= '0' && lexer->c <= '7')
		advance_and_push(lexer);
	pushc(lexer, '\0');
	*res = NUMBER_VAL(strtoll(lbptr(lexer), NULL, 8));
	popc(lexer); // '\0'
}


/* Check if number constant overflows after conversion, auxiliary to 'number'. */
static cr_inline void checkoverflow(Lexer *lexer, Value *n)
{
	if (cr_unlikely(errno == ERANGE)) {
		lexerror(lexer, lexerrors[LE_LARGECONST], errlen(lblen(lexer)), lbptr(lexer));
		*n = NUMBER_VAL(0);
		errno = 0;
	}
}

/* Create number token */
static Token number(Lexer *lexer)
{
	Value num;
	char c = lexer->c;
	advance_and_push(lexer);
	if (c == '0' && (lexer->c == 'x' || lexer->c == 'X')) // hexadecimal?
		hex(lexer, &num);
	else if (c == '0' && (lexer->c >= '0' && lexer->c <= '7')) // octal ?
		octal(lexer, &num);
	else // otherwise it must be decimal
		decimal(lexer, &num);
	checkoverflow(lexer, &num);
	return token(lexer, TOK_NUMBER, num);
}


/* Check if rest of the slice matches the pattern and
 * return appropriate 'TokenType' otherwise return 'TOK_IDENTIFIER'.
 * Auxiliary to 'TokenType_identifier' */
static cr_inline TType keyword(Lexer *lexer, uint32_t start, uint32_t length, const char *pattern,
		TType type)
{
	advance(lexer); // skip past the keyword for the next scan
	if (lblen(lexer) == start + length && memcmp(lbptr(lexer) + start, pattern, length) == 0)
		return type;
	return TOK_IDENTIFIER;
}

/* Return appropriate 'TokenType' for the identifier */
static TType TokenType_identifier(Lexer *lexer)
{
#ifdef CR_PRECOMPUTED_GOTO
#define RET &&ret
	// IMPORTANT: update accordingly if lexer tokens change!
	// Make sure the order is the same as in ASCII Table - https://www.asciitable.com
	static const void *ASCII_table[UINT8_MAX + 1] = {
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, &&a, &&b, &&c,
		&&d, &&e, &&f, RET, RET, &&i, RET, RET, &&l, RET, &&n, &&o, RET, RET, &&r, &&s, &&t, RET, &&v, &&w,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
		RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET, RET,
	};
#undef RET
	goto *ASCII_table[lexer->c];
a:
	return keyword(lexer, 1, 2, "nd", TOK_AND);
b:
	return keyword(lexer, 1, 4, "reak", TOK_BREAK);
c:
	if (lblen(lexer) > 1) {
		switch (lbptr(lexer)[1]) {
			case 'a':
				return keyword(lexer, 2, 2, "se", TOK_CASE);
			case 'l':
				return keyword(lexer, 2, 3, "ass", TOK_CLASS); // lmao
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
	if (lblen(lexer) > 1) {
		switch (lbptr(lexer)[1]) {
			case 'a':
				return keyword(lexer, 2, 3, "lse", TOK_FALSE);
			case 'i':
				return keyword(lexer, 2, 3, "xed", TOK_FIXED);
			case 'n':
				if (lblen(lexer) == 2)
					return TOK_FN;
				else
					return TOK_IDENTIFIER;
			case 'o':
				if (lblen(lexer) > 3)
					return keyword(lexer, 2, 5, "reach", TOK_FOREACH);
				else
					return keyword(lexer, 2, 1, "r", TOK_FOR);
			default:
				break;
		}
	}
	goto ret;
i:
	if (lblen(lexer) > 1) {
		switch (lbptr(lexer)[1]) {
			case 'm':
				return keyword(lexer, 2, 2, "pl", TOK_IMPL);
			case 'n':
				if (lblen(lexer) == 2)
					return TOK_IN;
				else
					return TOK_IDENTIFIER;
			case 'f':
				if (lblen(lexer) == 2)
					return TOK_IF;
				else
					return TOK_IDENTIFIER;
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
	if (lblen(lexer) > 1) {
		switch (lbptr(lexer)[1]) {
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
	switch (lexer->c) {
		case 'a':
			return keyword(lexer, 1, 2, "nd", TOK_AND);
		case 'b':
			return keyword(lexer, 1, 4, "reak", TOK_BREAK);
		case 'c':
			if (lblen(lexer) > 1) {
				switch (lbptr(lexer)[1]) {
					case 'a':
						return keyword(lexer, 2, 2, "se", TOK_CASE);
					case 'l':
						return keyword(lexer, 2, 3, "ass", TOK_CLASS);
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
			if (lblen(lexer) > 1) {
				switch (lbptr(lexer)[1]) {
					case 'a':
						return keyword(lexer, 2, 3, "lse", TOK_FALSE);
					case 'i':
						return keyword(lexer, 2, 3, "xed", TOK_FIXED);
					case 'n':
						if (lblen(lexer) == 2)
							return TOK_FN;
						else
							return TOK_IDENTIFIER;
						if (lblen(lexer) > 3)
							return keyword(lexer, 2, 5, "reach", TOK_FOREACH);
						else
							return keyword(lexer, 2, 1, "r", TOK_FOR);
					default:
						break;
				}
			}
			break;
		case 'i':
			if (lblen(lexer) > 1) {
				switch (lbptr(lexer)[1]) {
					case 'm':
						keyword(lexer, 2, 2, "pl", TOK_IMPL);
					case 'n':
						if (lblen(lexer) == 2)
							return TOK_IN;
						else
							return TOK_IDENTIFIER;
					case 'f':
						if (lblen(lexer) == 2)
							return TOK_IF;
						else
							return TOK_IDENTIFIER;
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
			if (lblen(lexer) > 1) {
				switch (lbptr(lexer)[1]) {
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

static cr_inline Token idtoken(Lexer *lexer)
{
	while (isalnum(lexer->c) || lexer->c == '_')
		advance_and_push(lexer);
	return token(lexer, TokenType_identifier(lexer), EMPTY_VAL);
}

/* -------------------------------------------------- */ // Tokens




/* =================== Scanning =================== */

Token scan(Lexer *lexer)
{
	lexer->buffer.len = 0; // reset buffer
	skipws(lexer);
	if (lexer->c == CREOF)
		return token(lexer, TOK_EOF, EMPTY_VAL);
	if (lexer->c == '_' || isalpha(lexer->c))
		return idtoken(lexer);
	if (isdigit(lexer->c))
		return number(lexer);
#ifdef CR_PRECOMPUTED_GOTO
#define ERR &&err
	// https://www.asciitable.com
	static const void *ASCII_table[UINT8_MAX + 1] = {
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       &&bang,	 &&string, ERR,	     ERR,   &&percent, ERR,	ERR,
		&&lparen, &&rparen, &&star,    &&plus,	 &&comma,  &&minus,  &&dot, &&slash,   ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       &&colon, &&semicolon,
		&&less,	  &&equal,  &&greater, &&qmark,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  &&lbrack, ERR,       &&rbrack, &&caret,  ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       &&lbrace, ERR,	   &&rbrace, ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,	     ERR,   ERR,       ERR,	ERR,
		ERR,	  ERR,	    ERR,       ERR,	 ERR,	   ERR,
	};
#undef ERR
	goto *ASCII_table[lexer->c];
lbrack:
	return keywordtoken(lexer, TOK_LBRACK);
rbrack:
	return keywordtoken(lexer, TOK_RBRACK);
caret:
	return keywordtoken(lexer, TOK_CARET);
lparen:
	return keywordtoken(lexer, TOK_LPAREN);
rparen:
	return keywordtoken(lexer, TOK_RPAREN);
lbrace:
	return keywordtoken(lexer, TOK_LBRACE);
rbrace:
	return keywordtoken(lexer, TOK_RBRACE);
dot:
	advance(lexer);
	int32_t fallbackc = lexer->c;
	if (lmatch(lexer, '.')) { // is '...' ?
		if (lmatch(lexer, '.'))
			return token(lexer, TOK_DOT_DOT_DOT, EMPTY_VAL);
		else
			goback(lexer, fallbackc);
	}
	return token(lexer, TOK_DOT, EMPTY_VAL);
comma:
	return keywordtoken(lexer, TOK_COMMA);
minus:
	return keywordtoken(lexer, TOK_MINUS);
plus:
	return keywordtoken(lexer, TOK_PLUS);
colon:
	return keywordtoken(lexer, TOK_COLON);
semicolon:
	return keywordtoken(lexer, TOK_SEMICOLON);
slash:
	return keywordtoken(lexer, TOK_SLASH);
star:
	return keywordtoken(lexer, TOK_STAR);
qmark:
	return keywordtoken(lexer, TOK_QMARK);
percent:
	return keywordtoken(lexer, TOK_PERCENT);
bang:
	return token(lexer, lmatch(lexer, '=') ? TOK_BANG_EQUAL : (advance(lexer), TOK_BANG), EMPTY_VAL);
equal:
	return token(lexer, lmatch(lexer, '=') ? TOK_EQUAL_EQUAL : (advance(lexer), TOK_EQUAL), EMPTY_VAL);
greater:
	return token(lexer, lmatch(lexer, '=') ? TOK_GREATER_EQUAL : (advance(lexer), TOK_GREATER), EMPTY_VAL);
less:
	return token(lexer, lmatch(lexer, '=') ? TOK_LESS_EQUAL : (advance(lexer), TOK_LESS), EMPTY_VAL);
string:
	return string(lexer);
err:
	advance(lexer);
	return errtoken(lexer, "Unexpected character.");
#else
	switch (lexer->c) {
		case '[':
			return keywordtoken(lexer, TOK_LBRACK);
		case ']':
			return keywordtoken(lexer, TOK_RBRACK);
		case '(':
			return keywordtoken(lexer, TOK_LPAREN);
		case ')':
			return keywordtoken(lexer, TOK_RPAREN);
		case '{':
			return keywordtoken(lexer, TOK_LBRACE);
		case '}':
			return keywordtoken(lexer, TOK_RBRACE);
		case '.':
			advance(lexer);
			int32_t fallbackc = lexer->c;
			if (lxmatch(lexer, '.')) { // is '...' ?
				if (lxmatch(lexer, '.'))
					return token(lexer, TOK_DOT_DOT_DOT, EMPTY_VAL);
				else
					goback(lexer, fallbackc);
			}
			return token(lexer, TOK_DOT, EMPTY_VAL);
		case ',':
			return keywordtoken(lexer, TOK_COMMA);
		case '-':
			return keywordtoken(lexer, TOK_MINUS);
		case '+':
			return keywordtoken(lexer, TOK_PLUS);
		case ';':
			return keywordtoken(lexer, TOK_SEMICOLON);
		case ':':
			return keywordtoken(lexer, TOK_COLON);
		case '?':
			return keywordtoken(lexer, TOK_QMARK);
		case '/':
			return keywordtoken(lexer, TOK_SLASH);
		case '*':
			return keywordtoken(lexer, TOK_STAR);
		case '%':
			return keywordtoken(lexer, TOK_PERCENT);
		case '!':
			return token(lexer, lmatch(lexer, '=') ? TOK_BANG_EQUAL : (advance(lexer), TOK_BANG), EMPTY_VAL);
		case '=':
			return token(lexer, lmatch(lexer, '=') ? TOK_EQUAL_EQUAL : (advance(lexer), TOK_EQUAL), EMPTY_VAL);
		case '>':
			return token(lexer, lmatch(lexer, '=') ? TOK_GREATER_EQUAL : (advance(lexer), TOK_GREATER), EMPTY_VAL);
		case '<':
			return token(lexer, lmatch(lexer, '=') ? TOK_LESS_EQUAL : (advance(lexer), TOK_LESS), EMPTY_VAL);
		case '"':
			return string(lexer);
		default:
			advance(lexer);
			return errtoken(lexer, "Unexpected character.");
	}
#endif
}

/* ----------------------------------------------- */ // Scanning
