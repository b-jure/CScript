#ifndef __SKOOMA_JMPTABLE_H__
#define __SKOOMA_JMPTABLE_H__

#undef DISPATCH

#undef CASE

#undef BREAK

/* Redefine dispatch from switch into goto */
#define DISPATCH(x) goto *optable[x];

/* Redefine case into label */
#define CASE(label) L_##label:

/* Redefine break into another goto/dispatch */
#define BREAK VM_DISPATCH(READ_BYTE())

/* Make sure the order is the same as in the OpCode enum */
static const void *const optable[OPCODE_N] = {
    &&L_OP_TRUE,          &&L_OP_FALSE,
    &&L_OP_NIL,           &&L_OP_NEG,
    &&L_OP_ADD,           &&L_OP_SUB,
    &&L_OP_MUL,           &&L_OP_DIV,
    &&L_OP_NOT,           &&L_OP_NOT_EQUAL,
    &&L_OP_EQUAL,         &&L_OP_GREATER,
    &&L_OP_GREATER_EQUAL, &&L_OP_LESS,
    &&L_OP_LESS_EQUAL,    &&L_OP_PRINT,
    &&L_OP_POP,           &&L_OP_POPN,
    &&L_OP_CONST,         &&L_OP_CONSTL,
    &&L_OP_DEFINE_GLOBAL, &&L_OP_DEFINE_GLOBALL,
    &&L_OP_GET_GLOBAL,    &&L_OP_GET_GLOBALL,
    &&L_OP_SET_GLOBAL,    &&L_OP_SET_GLOBALL,
    &&L_OP_GET_LOCAL,     &&L_OP_GET_LOCALL,
    &&L_OP_SET_LOCAL,     &&L_OP_SET_LOCALL,
    &&L_OP_RET,
};

#endif
