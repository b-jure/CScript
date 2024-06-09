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

#include <ctype.h>
#include <stdio.h>
#include <string.h>



/* Maximum size of error token string. */
#define MAXERR		CR_MAXSRC
#define errlen(len)	(MIN(MAXERR, len))



/* errors */
typedef enum {
	LE_INCHEXESC, LE_INVHEXESC, LE_UNTSTR,
	LE_LARGECONST, LE_DIGOCTAL, LE_HEXCONST,
	LE_DECCONST, LE_ESCSEQ, LE_TOKENLIMIT,
} LexErr;

/* error description */
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



void cr_lr_init(VM *vm, Lexer *lx, BuffReader *br, OString *source)
{
	lx->vm = vm;
	lx->fs = NULL;
	lx->br = br;
	lx->src = source;
	lx->c = brgetc(lx->br);
	lx->currline = 1;
	lx->prevline = 1;
	lx->skip = 0;
	cr_mm_createvec(vm, &lx->buf, MAXSIZE>>1, "chars");
	cr_mm_reallocvec(vm, &lx->buf, CR_MINBUFFER);
}


void cr_lr_free(Lexer *lx)
{
	freevec(lx->vm, &lx->buf);
}


static cr_noret lexerror(Lexer *lexer, const char *err, va_list ap)
{
	// TODO
}


void cr_lr_syntaxerror(Lexer *L, const char *err, va_list args)
{
	VM *vm;
	OString *prefix;
	cr_ubyte preverr;
	const Token *token;
	const char *src;

	L->error = 1;
	vm = L->vm;
	preverr = L->error;
	token = &L->previous;
	src = ascstring(L->src);
	prefix = OString_fmt(vm, "[%s][line: %u] Error", src, token->line);
	push(vm, OBJ_VAL(prefix));

	if (token->tt == TOK_EOF) {
		push(vm, OBJ_VAL(OString_fmt(vm, " at end of file: ")));
	} else if (token->tt != TOK_ERROR) {
		push(vm, OBJ_VAL(OString_fmt(vm, " at '%.*s': ", token->len, token->start)));
	} else
		goto pusherr;
	concatonstack(vm);
pusherr:
	push(vm, OBJ_VAL(OString_fmt_from(vm, err, args)));
	concatonstack(vm);
	if (preverr)
		concatonstack(vm);
}


/* ------------------------------------------------------------ */ // register error




/* ================== Lexer buffer and auxiliary functions ================== */

/* Checks for end of stream */
#define isend(c) ((c) == CREOF)

/* Increments line number if 'c' is newline character */
#define incrline(l, c) ((c) == '\n' || (c) == '\r' ? ((l)->line++, c) : c)

/* Fetches new character from BuffReader, also increments
 * the line number if newline character was fetched. */
#define nextchar(l) incrline(l, brgetc((l)->br))

/* Fetches the next character and stores it as current char */
#define advance(l) ((l)->c = nextchar(l))

/* Go back one character (byte) */
#define fallback(l, oldc) (brungetc((l)->br), (l)->c = oldc)

/* Push current character into lexer buffer */
#define pushl(l) pushc(l, (l)->c)

/* Pushes the current character into lexer buffer and
 * advances the lexer */
#define advance_and_push(l) (pushl(l), advance(l))

/* Same as 'advance_and_push' except the character is 'c' */
#define advance_and_pushc(l, c) (pushc(l, c), advance(l))

/* pointer to start of lexer buffer */
#define lbptr(lexer) cast_charp((lexer)->buffer.data)

/* length of lexer buffer */
#define lblen(lexer) (lexer)->buffer.len

/* Pop character from lexer buffer */
#define lbpop(l) (Array_ubyte_pop(&lexer->buffer))



/* Pushes 'c' into lexer buffer */
static void pushc(Lexer *lexer, int32_t c)
{
	if (cr_unlikely(lblen(lexer) >= LEX_TOKEN_LEN_LIMIT)) {
		lexerror(lexer, lexerrors[LE_TOKENLIMIT], LEX_TOKEN_LEN_LIMIT);
		lexer->skip = 1;
	}
	Array_ubyte_push(&lexer->buffer, c);
}


/* Check if current character matches 'c', if so
 * return 1 and advance the lexer, otherwise return 0. */
static cr_ubyte lmatch(Lexer *lexer, int32_t c)
{
	if (isend(lexer->c) || c != lexer->c)
		return 0;
	advance(lexer);
	return 1;
}


/* Skip white-space and comments. */
static void skipws(Lexer *lexer)
{
read_more:
	switch (lexer->c) {
	case '\n':
	case ' ':
	case '\r':
	case '\t':
		advance(lexer);
		goto read_more;
	case '#':
		advance(lexer);
		while (!isend(lexer->c) && lexer->c != '\n')
			advance(lexer);
		goto read_more;
	default:
		break;
	}
}

/* -------------------------------------------------- */ // Lexer buffer and aux functions




/* =================== Tokens =================== */

/* Create 'synthetic' token */
Token syntoken(const char *name)
{
	return (Token){
		.type = 0,
		.line = 0,
		.start = name,
		.len = cast(cr_ubyte, strlen(name)),
	};
}

/* Create generic token */
static Token token(Lexer *lexer, TType type, Value value)
{
	Token token;
	token.tt = type;
	token.value = value;
	token.start = lbptr(lexer);
	token.len = cast(cr_ubyte, lblen(lexer));
	token.line = lexer->currentline;
	return token;
}

/* Keyword tokens contain no values */
#define keywordtoken(lexer, type) (advance(lexer), token(lexer, type, EMPTY_VAL))



/* Create error token with 'err' message */
static cr_inline Token errtoken(Lexer *lexer, const char *err)
{
	Token token;
	token.tt = TOK_ERROR;
	token.start = err;
	token.len = cast(cr_ubyte, strlen(err));
	token.line = lexer->currentline;
	return token;
}


/* Get new hex digit from current char */
static int8_t hexdigit(Lexer *lexer)
{
	if (lexer->c >= '0' && lexer->c <= '9')
		return lexer->c - '0';
	else if (lexer->c >= 'a' && lexer->c <= 'f')
		return (lexer->c - 'a') + 10;
	else if (lexer->c >= 'A' && lexer->c <= 'F')
		return (lexer->c - 'A') + 10;
	return -1;
}


/* Parse hex escape sequence '\x' */
static cr_ubyte eschex(Lexer *lexer)
{
	advance(lexer); // skip 'x'
	cr_ubyte number = 0;
	for (cr_ubyte i = 0; i < 2; i++) {
		if (cr_unlikely(isend(lexer->c) || lexer->c == '"')) {
			lexerror(lexer, lexerrors[LE_INCHEXESC]);
			break;
		}
		int8_t digit = hexdigit(lexer);
		if (digit == -1) {
			lexerror(lexer, lexerrors[LE_INVHEXESC]);
			break;
		}
		number = (number << 4) | digit;
		advance(lexer);
	}
	return number;
}


/* Create string token and escape the escape sequences if any */
static cr_inline Token string(Lexer *lexer)
{
	advance(lexer); // skip first '"'
	while (1) {
		if (cr_unlikely(isend(lexer->c) || lexer->c == '\n' || lexer->c == '\r')) {
			lexerror(lexer, lexerrors[LE_UNTSTR]);
			break;
		}
		if (lexer->c == '"')
			break;
		if (lexer->c == '\\') {
			advance(lexer);
			switch (lexer->c) {
			case '"':
			case '\'':
			case '%':
			case '\\':
			case '?':
				advance_and_push(lexer);
				break;
			case '0':
				advance_and_pushc(lexer, '\0');
				break;
			case 'a':
				advance_and_pushc(lexer, '\a');
				break;
			case 'b':
				advance_and_pushc(lexer, '\b');
				break;
			case 'e':
				advance_and_pushc(lexer, '\33');
				break;
			case 'f':
				advance_and_pushc(lexer, '\f');
				break;
			case 'n':
				advance_and_pushc(lexer, '\n');
				break;
			case 'r':
				advance_and_pushc(lexer, '\r');
				break;
			case 't':
				advance_and_pushc(lexer, '\t');
				break;
			case 'v':
				advance_and_pushc(lexer, '\v');
				break;
			case 'x': {
				pushc(lexer, cast(int32_t, eschex(lexer)));
				break;
			}
			case CREOF: // raise error next iteration
				break;
			default:
				lexerror(lexer, lexerrors[LE_ESCSEQ], lexer->c);
				break;
			}
		} else
			pushc(lexer, lexer->c);
	}
	advance(lexer);
	OString *string = OString_new(lexer->vm, lbptr(lexer), lblen(lexer));
	Token tok = token(lexer, TOK_STRING, OBJ_VAL(string));
	return tok;
}


/* Create number value from string in decimal format */
static void decimal(Lexer *lexer, Value *res)
{
	for (;; advance(lexer)) {
		if (lexer->c == '_')
			continue;
		if (!isdigit(lexer->c))
			break;
		pushl(lexer);
	}
	if (lexer->c == '.') {
		advance_and_push(lexer);
		while (isdigit(lexer->c))
			advance_and_push(lexer);
		if (lexer->c == 'e' || lexer->c == 'E') {
			advance_and_push(lexer);
			if (lexer->c == '+' || lexer->c == '-')
				advance_and_push(lexer);
			if (cr_unlikely(!isdigit(lexer->c)))
				lexerror(lexer, lexerrors[LE_DECCONST]);
			else {
				advance_and_push(lexer);
				for (;; advance(lexer)) {
					if (lexer->c == '_')
						continue;
					if (!isdigit(lexer->c))
						break;
					pushl(lexer);
				}
			}
		}
	}
	pushc(lexer, '\0');
	errno = 0;
	if (!lexer->skip)
		*res = NUMBER_VAL(strtod(lbptr(lexer), NULL));
	else {
		lexer->skip = 0;
		*res = NUMBER_VAL(0.0);
	}
	lbpop(lexer); // '\0'
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
	lbpop(lexer); // '\0'
}


/* Create number value from string in octal format */
static cr_inline void octal(Lexer *lexer, Value *res)
{
	while (lexer->c >= '0' && lexer->c <= '7')
		advance_and_push(lexer);
	pushc(lexer, '\0');
	*res = NUMBER_VAL(strtoll(lbptr(lexer), NULL, 8));
	lbpop(lexer); // '\0'
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
			fallback(lexer, fallbackc);
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
		if (lmatch(lexer, '.')) { // is '...' ?
			if (lmatch(lexer, '.'))
				return token(lexer, TOK_DOT_DOT_DOT, EMPTY_VAL);
			else
				fallback(lexer, fallbackc);
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
