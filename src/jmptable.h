#ifndef SKJMPTABLE_H
#define SKJMPTABLE_H


#if defined(OP_TABLE)

#undef DISPATCH

#undef CASE

#undef BREAK

/* Redefine dispatch from switch into goto */
#define DISPATCH(x) goto* optable[x];

/* Redefine case into label */
#define CASE(label) L_##label:

/* Redefine break into another goto/dispatch */
#define BREAK DISPATCH(READ_BYTE())

/* Make sure the order is the same as in the OpCode enum */
static const void* const optable[OPCODE_N] = {
    &&L_OP_TRUE,
    &&L_OP_FALSE,
    &&L_OP_NIL,
    &&L_OP_NILN,
    &&L_OP_NEG,
    &&L_OP_ADD,
    &&L_OP_SUB,
    &&L_OP_MUL,
    &&L_OP_DIV,
    &&L_OP_MOD,
    &&L_OP_POW,
    &&L_OP_NOT,
    &&L_OP_VALIST,
    &&L_OP_NOT_EQUAL,
    &&L_OP_EQUAL,
    &&L_OP_EQ,
    &&L_OP_GREATER,
    &&L_OP_GREATER_EQUAL,
    &&L_OP_LESS,
    &&L_OP_LESS_EQUAL,
    &&L_OP_POP,
    &&L_OP_POPN,
    &&L_OP_CONST,
    &&L_OP_DEFINE_GLOBAL,
    &&L_OP_DEFINE_GLOBALL,
    &&L_OP_GET_GLOBAL,
    &&L_OP_GET_GLOBALL,
    &&L_OP_SET_GLOBAL,
    &&L_OP_SET_GLOBALL,
    &&L_OP_GET_LOCAL,
    &&L_OP_GET_LOCALL,
    &&L_OP_SET_LOCAL,
    &&L_OP_SET_LOCALL,
    &&L_OP_JMP_IF_FALSE,
    &&L_OP_JMP_IF_FALSE_POP,
    &&L_OP_JMP_IF_FALSE_OR_POP,
    &&L_OP_JMP_IF_FALSE_AND_POP,
    &&L_OP_JMP,
    &&L_OP_JMP_AND_POP,
    &&L_OP_LOOP,
    &&L_OP_CALL,
    &&L_OP_CLOSURE,
    &&L_OP_GET_UPVALUE,
    &&L_OP_SET_UPVALUE,
    &&L_OP_CLOSE_UPVAL,
    &&L_OP_CLOSE_UPVALN,
    &&L_OP_CLASS,
    &&L_OP_SET_PROPERTY,
    &&L_OP_GET_PROPERTY,
    &&L_OP_INDEX,
    &&L_OP_SET_INDEX,
    &&L_OP_INVOKE_INDEX,
    &&L_OP_METHOD,
    &&L_OP_INVOKE,
    &&L_OP_OVERLOAD,
    &&L_OP_INHERIT,
    &&L_OP_GET_SUPER,
    &&L_OP_INVOKE_SUPER,
    &&L_OP_CALLSTART,
    &&L_OP_RETSTART,
    &&L_OP_FOREACH,
    &&L_OP_FOREACH_PREP,
    &&L_OP_TOPRET,
    &&L_OP_RET,
};

#elif defined(OBJ_TABLE)

#undef DISPATCH

#undef CASE

#undef BREAK

#define DISPATCH(x) goto* objtable[x];

#define CASE(label) L_##label:

#define BREAK return

static const void* objtable[OBJ_BOUND_METHOD + 1] = {
    &&L_OBJ_STRING,
    &&L_OBJ_FUNCTION,
    &&L_OBJ_CLOSURE,
    &&L_OBJ_NATIVE,
    &&L_OBJ_UPVAL,
    &&L_OBJ_CLASS,
    &&L_OBJ_INSTANCE,
    &&L_OBJ_BOUND_METHOD,
};

#elif defined(VAL_TABLE)

#undef DISPATCH

#undef CASE

#undef BREAK

#define DISPATCH(x) goto* valtable[x];

#define CASE(label) L_##label:

#define BREAK continue

static const void* valtable[] = {
    &&L_VAL_BOOL,
    &&L_VAL_NUMBER,
    &&L_VAL_NIL,
    &&L_VAL_OBJ,
    /* Unreachable */
    NULL,
};

#elif defined(TOK_TABLE)

#undef DISPATCH

#undef CASE

#undef BREAK

#define DISPATCH(x) goto* toktable[x];

#define CASE(label) L_##label:

#define BREAK return

static const void* toktable[TOK_EOF + 1] = {
    &&L_TOK_LBRACK,    &&L_TOK_RBRACK,      &&L_TOK_LPAREN,     &&L_TOK_RPAREN,
    &&L_TOK_LBRACE,    &&L_TOK_RBRACE,      &&L_TOK_DOT,        &&L_TOK_DOT_DOT_DOT,
    &&L_TOK_COMMA,     &&L_TOK_MINUS,       &&L_TOK_PLUS,       &&L_TOK_COLON,
    &&L_TOK_SEMICOLON, &&L_TOK_SLASH,       &&L_TOK_STAR,       &&L_TOK_PERCENT,
    &&L_TOK_CARET,     &&L_TOK_QMARK,       &&L_TOK_BANG,       &&L_TOK_BANG_EQUAL,
    &&L_TOK_EQUAL,     &&L_TOK_EQUAL_EQUAL, &&L_TOK_GREATER,    &&L_TOK_GREATER_EQUAL,
    &&L_TOK_LESS,      &&L_TOK_LESS_EQUAL,  &&L_TOK_IDENTIFIER, &&L_TOK_STRING,
    &&L_TOK_NUMBER,    &&L_TOK_AND,         &&L_TOK_BREAK,      &&L_TOK_CASE,
    &&L_TOK_CONTINUE,  &&L_TOK_CLASS,       &&L_TOK_DEFAULT,    &&L_TOK_ELSE,
    &&L_TOK_FALSE,     &&L_TOK_FOR,         &&L_TOK_FOREACH,    &&L_TOK_FN,
    &&L_TOK_IF,        &&L_TOK_IN,          &&L_TOK_IMPL,       &&L_TOK_NIL,
    &&L_TOK_OR,        &&L_TOK_RETURN,      &&L_TOK_SUPER,      &&L_TOK_SELF,
    &&L_TOK_SWITCH,    &&L_TOK_TRUE,        &&L_TOK_VAR,        &&L_TOK_WHILE,
    &&L_TOK_LOOP,      &&L_TOK_FIXED,       &&L_TOK_ERROR,      &&L_TOK_EOF,
};

#endif

#endif
