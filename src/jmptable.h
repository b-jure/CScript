#ifndef __SKOOMA_JMPTABLE_H__
#define __SKOOMA_JMPTABLE_H__

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
    &&L_OP_NEG,
    &&L_OP_ADD,
    &&L_OP_SUB,
    &&L_OP_MUL,
    &&L_OP_DIV,
    &&L_OP_NOT,
    &&L_OP_NOT_EQUAL,
    &&L_OP_EQUAL,
    &&L_OP_EQ,
    &&L_OP_GREATER,
    &&L_OP_GREATER_EQUAL,
    &&L_OP_LESS,
    &&L_OP_LESS_EQUAL,
    &&L_OP_PRINT,
    &&L_OP_POP,
    &&L_OP_POPN,
    &&L_OP_CONST,
    &&L_OP_CONSTL,
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
    &&L_OP_JMP_IF_FALSE_OR_POP,
    &&L_OP_JMP_IF_FALSE_AND_POP,
    &&L_OP_JMP,
    &&L_OP_JMP_AND_POP,
    &&L_OP_LOOP,
    &&L_OP_CALL,
    &&L_OP_CALLL,
    &&L_OP_CLOSURE,
    &&L_OP_GET_UPVALUE,
    &&L_OP_SET_UPVALUE,
    &&L_OP_CLOSE_UPVAL,
    &&L_OP_CLOSE_UPVALN,
    &&L_OP_CLASS,
    &&L_OP_CLASSL,
    &&L_OP_SET_PROPERTY,
    &&L_OP_SET_PROPERTYL,
    &&L_OP_GET_PROPERTY,
    &&L_OP_GET_PROPERTYL,
    &&L_OP_SET_DYNPROPERTY,
    &&L_OP_GET_DYNPROPERTY,
    &&L_OP_METHOD,
    &&L_OP_METHODL,
    &&L_OP_INVOKE,
    &&L_OP_INVOKEL,
    &&L_OP_OVERLOAD,
    &&L_OP_INHERIT,
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
    /* Placeholders */
    NULL,
    NULL,
};

#endif
#endif
