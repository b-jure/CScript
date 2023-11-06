#include "array.h"
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "err.h"
#include "mem.h"
#include "object.h"
#include "parser.h"
#include "skconf.h"
#include "value.h"
#include "vmachine.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>




// Get instruction length
#define GET_OP_TYPE(idx, op) (idx <= UINT8_MAX) ? op : op##L

// Return current chunk code offset
#define code_offset(C) (current_chunk(C)->code.len)



// Any 'stack' size limit
#define BYTECODE_MAX (MIN(VM_STACK_MAX, UINT24_MAX))



// Bit mask to extract loop and switch flags from parser.
// Used primarily to save the old flags before continuing to compile
// 'for', 'while' and 'switch' statements.
#define CFLOW_MASK(C) ((uint64_t)(btoul(SWITCH_BIT) | btoul(LOOP_BIT)) & PFLAGS(C))




// Local variable flags
#define VFIXED_BIT (1) // Variable is fixed (immutable)
// 2-7 bits are unused
#define VCAPTURED_BIT (8) // Variable is captured by closure
// Local variable flag setters and getters
#define LFLAG_SET(var, bit)   BIT_SET((var)->flags, bit)
#define LFLAG_CHECK(var, bit) BIT_CHECK((var)->flags, bit)
#define LFLAGS(var)           ((var)->flags)

// Local variable
typedef struct {
    Token name;  // Variable name
    Int   depth; // Scope depth where variable is defined
                 // can be -1 if the variable is not initialized
    Byte flags;  // Flags that represent some property of this
                 // variable (check above the V[FLAGNAME]_BIT)
} Local;

// Initialize local variable
#define INIT_LOCAL(C) (C)->locals[(C)->loc_len - 1].depth = (C)->depth




/* Default 'Compiler' stack size */
#define SHORT_STACK_SIZE (UINT8_MAX)




/* Array of Int(s) */
ARRAY_NEW(Array_Int, Int);
/* Two dimensional array */
ARRAY_NEW(Array_Array_Int, Array_Int)

/* Control Flow context */
typedef struct {
    /* We store break offsets inside an array because we don't
     * know in advance where the end of the switch/loop statement
     * is while parsing.
     * When we hit the end of the loop/switch statement then
     * we can just patch each parsed break statement.
     * This would be the same for 'continue' in case of 'do while'
     * loop.*/
    Array_Array_Int breaks; /* Break statement offsets */

    Int innermostl_start;  /* Innermost loop start offset */
    Int innermostl_depth;  /* Innermost loop scope depth */
    Int innermostsw_depth; /* Innermost switch scope depth */
} CFCtx;




/**
 * UpValue is a Value that is declared/defined inside of enclosing function.
 * Contains stack index of that variable inside of that enclosing function.
 * The 'local' field indicates if this is the local variable of the innermost
 * enclosing function or the UpValue.
 **/
typedef struct {
    UInt idx;   // Stack index
    bool local; // If upvalue is local to enclosing function
} Upvalue;

// Array of 'Upvalues'
ARRAY_NEW(Array_Upvalue, Upvalue);




// Type of function the compiler is compiling
typedef enum {
    FN_FUNCTION, // Normal function declaration
    FN_METHOD,   // Class method function
    FN_INIT,     // Class initializer function
    FN_SCRIPT,   // Top-level code (implicit function)
} FunctionType;




// In case compiler is compiling a class declaration,
// then it will contain at least a single 'Class' struct,
// this serves as additional information when we are compiling
// a function declaration inside of a class 'method' function declaration
// to know if we are allowed to use 'self' and/or 'super' keywords to
// reference the currently compiled class if any and/or superclass if
// class has a superclass.
typedef struct Class {
    struct Class* enclosing;
    bool          superclass;
} Class;




struct Compiler {
    /* Enclosing compiler */
    Compiler* enclosing;

    /* Currently compiled class */
    Class* cclass;

    /* Grammar parser */
    Parser* parser;

    /* Control flow context, tracks offsets and
     * scope depth for 'continue' and 'break' statements. */
    CFCtx context;

    /* Currently compiled function, meaning we write
     * bytecode to the function chunk in this field. */
    ObjFunction* fn;
    FunctionType fn_type; /* Function type */

    /* Holds UpValues */
    Array_Upvalue* upvalues;

    /* Current scope depth. */
    Int depth;

    /* Count of local variables in this function.
     * In case of this function being FN_SCRIPT that would
     * mean count of variables so far in this scope block (scope > 0) */
    UInt loc_len;

    /* Capacity of 'locals' flexible array. */
    UInt loc_cap;

    /* Array that holds local variables 'Local's.
     * It is a flexible array this way we avoid pointer
     * dereference. */
    Local locals[];
};

/* Allocates 'Compiler' with the default stack size */
#define ALLOC_COMPILER(vm) MALLOC(vm, sizeof(Compiler) + (SHORT_STACK_SIZE * sizeof(Local)))

// Grows 'Compiler', compiler uses flexible array to store Local's
// and this is why the whole 'Compiler' needs to resize.
#define GROW_COMPILER(ptr, oldcap, newcap)                                                         \
    (Compiler*)REALLOC(vm, ptr, sizeof(Compiler) + (newcap * sizeof(Local)))


typedef Compiler** PPC;

// Shorthands
#define C()           (*ppc)
#define PREV_TOKEN(C) (C)->parser->previous
#define CURR_TOKEN(C) (C)->parser->current




/* Precedence from LOW-est to HIGH-est (Pratt parsing) */
typedef enum {
    PREC_NONE = 0,
    PREC_ASSIGNMENT,
    PREC_TERNARY,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

/* ParseFn - generic parse function signature. */
typedef void (*ParseFn)(VM*, PPC);

// Used for Pratt parsing algorithm
typedef struct {
    ParseFn    prefix;
    ParseFn    infix;
    Precedence precedence;
} ParseRule;




/* Internal */
sstatic Chunk* current_chunk(Compiler* C);
sstatic void   C_advance(Compiler* compiler);
sstatic void   C_error(Compiler* compiler, const char* error, ...);
sstatic void   compile_number(VM* vm, PPC ppc);
sstatic void   compile_string(VM* vm, PPC ppc);
sstatic UInt   compile_varname(VM* vm, PPC ppc, const char* errmsg);
sstatic void   compile_precedence(VM* vm, PPC ppc, Precedence prec);
sstatic void   compile_grouping(VM* vm, PPC ppc);
sstatic void   compile_ternarycond(VM* vm, PPC ppc);
sstatic void   compile_expr(VM* vm, PPC ppc);
sstatic void   compile_dec(VM* vm, PPC ppc);
sstatic void   compile_dec_var(VM* vm, PPC ppc);
sstatic void   compile_dec_fixed(VM* vm, PPC ppc);
sstatic void   compile_dec_fn(VM* vm, PPC ppc);
sstatic void   compile_stm_block(VM* vm, PPC ppc);
sstatic void   compile_stm(VM* vm, PPC ppc);
sstatic void   compile_variable(VM* vm, PPC ppc);
sstatic void   compile_binary(VM* vm, PPC ppc);
sstatic void   compile_unary(VM* vm, PPC ppc);
sstatic void   compile_literal(VM* vm, PPC ppc);
sstatic void   compile_and(VM* vm, PPC ppc);
sstatic void   compile_or(VM* vm, PPC ppc);
sstatic void   compile_call(VM* vm, PPC ppc);
sstatic void   compile_dot(VM* vm, PPC ppc);
sstatic void   compile_index(VM* vm, PPC ppc);
sstatic void   compile_self(VM* vm, PPC ppc);
sstatic void   compile_super(VM* vm, PPC ppc);
sstatic void   compile_fn(VM* vm, PPC ppc, FunctionType type);




/* Checks for equality between the 'token_type' and the current parser token */
#define C_check(compiler, token_type) (CURR_TOKEN(compiler).type == token_type)

sstatic void CFCtx_init(VM* vm, CFCtx* context)
{
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
    Array_Array_Int_init(&context->breaks, vm);
}

sstatic void C_init(
    Compiler*    C,
    Class*       cclass,
    VM*          vm,
    Parser*      parser,
    FunctionType fn_type,
    Value        loaded,
    Compiler*    enclosing)
{
    C->enclosing = enclosing;
    C->cclass    = cclass;
    C->parser    = parser;
    vm->compiler = C;

    CFCtx_init(vm, &C->context);

    C->fn      = NULL; // Initialize to NULL so gc does not get confused
    C->fn      = ObjFunction_new(vm);
    C->fn_type = fn_type;

    // Initialize Upvalue array only once when initializing outermost compiler.
    // This array then gets passed on to other nested compilers for each function.
    if(enclosing == NULL) {
        C->upvalues = MALLOC(vm, sizeof(Array_Upvalue));
        Array_Upvalue_init(C->upvalues, vm);
    } else {
        C->upvalues = enclosing->upvalues;
    }

    C->loc_len = 0;
    C->loc_cap = SHORT_STACK_SIZE;
    C->depth   = 0;

    /* Reserve first stack slot for VM ('self' ObjInstance) */
    Local* local = &C->locals[C->loc_len++];
    local->depth = 0;
    local->flags = 0;

    if(fn_type != FN_FUNCTION) {
        local->name.start = "self";
        local->name.len   = 4;
    } else {
        local->name.start = "";
        local->name.len   = 0;
    }

    if(fn_type == FN_SCRIPT) {
        vm->script  = loaded;
        C->fn->name = AS_STRING(loaded);
    } else if(fn_type != FN_SCRIPT) {
        C->fn->name = ObjString_from(vm, PREV_TOKEN(C).start, PREV_TOKEN(C).len);
    }
}

sstatic force_inline void C_grow_stack(VM* vm, PPC ppc)
{
    Compiler* C      = C();
    UInt      oldcap = C->loc_cap;

    if(unlikely(oldcap == VM_STACK_MAX)) {
        fprintf(
            stderr,
            "Compiler stack overflow, too many local variables. Limit [%u]",
            (UInt)VM_STACK_MAX);
        CLEANUP(vm);
    }

    C->loc_cap = MIN(GROW_ARRAY_CAPACITY(oldcap, SHORT_STACK_SIZE), VM_STACK_MAX);
    C          = GROW_COMPILER(vm, C, C->loc_cap);
    C()        = C;
}

void mark_c_roots(VM* vm)
{
    // Mark all currently compiled functions
    for(Compiler* current = vm->compiler; current != NULL; current = current->enclosing) {
        // In case we are loading a script which contains its own parser.
        mark_value(vm, PREV_TOKEN(vm->compiler).value);
        mark_value(vm, CURR_TOKEN(vm->compiler).value);
        mark_obj(vm, (Obj*)current->fn);
    }
}

/*========================== EMIT =========================*/

sstatic force_inline UInt C_make_const(Compiler* C, VM* vm, Value constant)
{
    if(unlikely(current_chunk(C)->clen > BYTECODE_MAX)) {
        COMPILER_CONSTANT_LIMIT_ERR(C, C->fn->name->storage, BYTECODE_MAX);
    }

    return Chunk_make_constant(vm, current_chunk(C), constant);
}

sstatic force_inline Value Token_into_stringval(VM* vm, const Token* name)
{
    return OBJ_VAL(ObjString_from(vm, name->start, name->len));
}

sstatic force_inline UInt
C_make_global_identifier(Compiler* C, VM* vm, Value identifier, Byte flags)
{
    Value    index;
    Variable glob = {UNDEFINED_VAL, flags};

    if(!HashTable_get(&vm->globids, identifier, &index)) {
        if(unlikely((vm)->globlen + 1 > UINT24_MAX)) {
            COMPILER_GLOBALS_LIMIT_ERR(C, UINT24_MAX);
        }

        VM_push(vm, identifier);
        index = NUMBER_VAL(GARRAY_PUSH(vm, glob));             // GC
        HashTable_insert(vm, &vm->globids, identifier, index); // GC
        VM_pop(vm);
    }

    return (UInt)AS_NUMBER(index);
}

// Emit instruction
#define EMIT_OP(c, code, param)                                                                    \
    Chunk_write_codewparam(current_chunk(c), code, param, PREV_TOKEN(c).line)

// Emit bytecode
#define EMIT_BYTE(c, byte) Chunk_write(current_chunk(c), byte, PREV_TOKEN(c).line)

// Emit jump instruction
#define EMIT_JMP(c, jmp)                                                                           \
    ({                                                                                             \
        EMIT_OP(c, jmp, 0);                                                                        \
        code_offset(c) - 3;                                                                        \
    })

// Emit 3 bytes
#define EMIT_LBYTE(c, bytes)                                                                       \
    do {                                                                                           \
        EMIT_BYTE(c, BYTE(bytes, 0));                                                              \
        EMIT_BYTE(c, BYTE(bytes, 1));                                                              \
        EMIT_BYTE(c, BYTE(bytes, 2));                                                              \
    } while(false)

sstatic force_inline void C_emit_return(Compiler* C)
{
    if(PFLAG_CHECK(C->parser, RET_BIT)) {
        return;
    }

    if(C->fn_type == FN_INIT) {
        EMIT_OP(C, OP_GET_LOCAL, 0);
    } else if(C->fn_type == FN_SCRIPT) {
        EMIT_BYTE(C, OP_TRUE);
    } else {
        EMIT_BYTE(C, OP_NIL);
    }

    EMIT_BYTE(C, OP_RET);
}

sstatic force_inline void C_emit_loop(Compiler* C, UInt start)
{
    EMIT_BYTE(C, OP_LOOP);

    UInt offset = current_chunk(C)->code.len - start + 3;

    if(offset >= UINT24_MAX) {
        COMPILER_JUMP_LIMIT_ERR(C, UINT24_MAX);
    }

    EMIT_LBYTE(C, offset);
}

sstatic sdebug void dump_stack(Compiler* C, Int depth, UInt idx)
{
    bool first        = true;
    Int  cached_depth = depth;
    Int  cached_scope = C->depth - depth;
    Int  scope        = cached_scope;

    while(scope--) {
        fputs("    ", stderr);
    }
    scope = cached_scope;
    fprintf(stderr, "{ - %d\n", idx);
    while(depth--) {
        fputs("    ", stderr);
    }

    for(UInt i = idx; i < C->loc_len; i++) {
        Local* local = &C->locals[i];
        if(local->depth < scope) {
            fputc('\n', stderr);
            dump_stack(C, cached_depth - 1, i);
            fputc('\n', stderr);
            break;
        }
        fprintf(stderr, "%s%.*s", first ? "" : ", ", local->name.len, local->name.start);
        first = false;
    }

    while(cached_depth--) {
        fputs("    ", stderr);
    }
    fprintf(stderr, "\n}\n");
}

sdebug void debug_local_stack(Compiler* C)
{
    fprintf(stderr, "\n==== stack_dump ====\n");
    fprintf(stderr, "depth:  %10d |\ncount:  %10d |\n====================\n", C->depth, C->loc_len);
    fprintf(stderr, "```\n");
    dump_stack(C, C->depth, 0);
    fprintf(stderr, "```\n\n");
}

sstatic void C_error(Compiler* C, const char* error, ...)
{
    Token*  token = &PREV_TOKEN(C);
    va_list args;

    // If panic bit is on, then sync the parser before printing any new errors.
    // If token type is TOK_ERROR then just return because parser already printed it.
    if(PFLAG_CHECK(C->parser, PANIC_BIT) || token->type == TOK_ERROR) {
        return;
    }

    va_start(args, error);
    print_error(C->parser, error, args);
    va_end(args);

#ifdef DEBUG_LOCAL_STACK
    dump_stack(C, C->depth, 0);
#endif
}

sstatic void C_advance(Compiler* C)
{
    PREV_TOKEN(C) = CURR_TOKEN(C);

    while(true) {
        CURR_TOKEN(C) = Parser_next(C->parser);
        if(CURR_TOKEN(C).type != TOK_ERROR) {
            break;
        }

        C_error(C, CURR_TOKEN(C).start);
    }
}

sstatic void Parser_sync(Compiler* C)
{
    PFLAG_CLEAR(C->parser, PANIC_BIT);

    while(CURR_TOKEN(C).type != TOK_EOF) {
        if(PREV_TOKEN(C).type == TOK_SEMICOLON) {
            return;
        }

        switch(CURR_TOKEN(C).type) {
            case TOK_FOR:
            case TOK_FN:
            case TOK_VAR:
            case TOK_CLASS:
            case TOK_IF:
            case TOK_RETURN:
            case TOK_WHILE:
                return;
            default:
                C_advance(C);
                break;
        }
    }
}

sstatic force_inline void C_expect(Compiler* compiler, TokenType type, const char* error)
{
    if(C_check(compiler, type)) {
        C_advance(compiler);
        return;
    }
    C_error(compiler, error);
}

sstatic force_inline bool C_match(Compiler* compiler, TokenType type)
{
    if(!C_check(compiler, type)) {
        return false;
    }
    C_advance(compiler);
    return true;
}

sstatic force_inline ObjFunction* compile_end(Compiler* C)
{
    C_emit_return(C);

#ifdef DEBUG_PRINT_CODE
    if(!PFLAG_CHECK(C->parser, ERROR_BIT)) {
        ObjFunction* fn = C->fn;
        Chunk_debug(current_chunk(C), (fn->name) ? fn->name->storage : "<script>");
    }
#endif

    // Not really needed, parser gets its flags back
    // from the bit mask we cached.
    PFLAG_CLEAR(C->parser, RET_BIT);
    return C->fn;
}

ObjFunction* compile(VM* vm, const char* source, Value name)
{
    VM_push_temp(vm, name);
    HashTable_insert(vm, &vm->loaded, name, EMPTY_VAL);
    Compiler* C      = ALLOC_COMPILER(vm);
    Parser    parser = Parser_new(source, vm);
    VM_pop_temp(vm);
    C_init(C, NULL, vm, &parser, FN_SCRIPT, name, vm->compiler);

    C_advance(C);
    while(!C_match(C, TOK_EOF)) {
        compile_dec(vm, &C);
    }
    ObjFunction* fn  = compile_end(C);
    bool         err = PFLAG_CHECK(C->parser, ERROR_BIT);
    C_free(vm, C);
    return (err ? NULL : fn);
}

sstatic force_inline Chunk* current_chunk(Compiler* C)
{
    return &C->fn->chunk;
}

sstatic void CFCtx_free(CFCtx* context)
{
    Array_Array_Int_free(&context->breaks, NULL);
    context->innermostl_start  = -1;
    context->innermostl_depth  = 0;
    context->innermostsw_depth = 0;
}

void C_free(VM* vm, Compiler* C)
{
    CFCtx_free(&C->context);
    if(C->enclosing == NULL) {
        vm->script = NIL_VAL;
        ASSERT(C->fn_type == FN_SCRIPT, "Compiler is top-level but the type is not FN_SCRIPT.");
        Array_Upvalue_free(C->upvalues, NULL);
        FREE(vm, C->upvalues);
    } else if(C->enclosing->fn_type == FN_SCRIPT) {
        vm->script = OBJ_VAL(C->enclosing->fn->name);
    }

    vm->compiler = C->enclosing;
    FREE(vm, C);
}

void _cleanup_compiler(VM* vm, Compiler* C)
{
    if(C != NULL) {
        FREE(vm, (char*)C->parser->source);
        for(Compiler* comp = C; comp != NULL; comp = comp->enclosing) {
            C_free(vm, C);
        }
    }
}

/*========================== PARSE ========================
 * PP* (Pratt Parsing algorithm)
 *
 * Parsing rules table,
 * First and second column are function pointers to 'ParseFn',
 * these functions are responsible for parsing the actual expression and most
 * are recursive. First column parse function is used in case token is prefix,
 * while second column parse function is used in case token is inifx. Third
 * column marks the 'Precedence' of the token inside expression. */
sstatic const ParseRule rules[] = {
    [TOK_LBRACK]        = {NULL,              compile_index,       PREC_CALL      },
    [TOK_LPAREN]        = {compile_grouping,  compile_call,        PREC_CALL      },
    [TOK_RPAREN]        = {NULL,              NULL,                PREC_NONE      },
    [TOK_LBRACE]        = {NULL,              NULL,                PREC_NONE      },
    [TOK_RBRACE]        = {NULL,              NULL,                PREC_NONE      },
    [TOK_COMMA]         = {NULL,              NULL,                PREC_NONE      },
    [TOK_DOT]           = {NULL,              compile_dot,         PREC_CALL      },
    [TOK_MINUS]         = {compile_unary,     compile_binary,      PREC_TERM      },
    [TOK_PLUS]          = {NULL,              compile_binary,      PREC_TERM      },
    [TOK_COLON]         = {NULL,              NULL,                PREC_NONE      },
    [TOK_SEMICOLON]     = {NULL,              NULL,                PREC_NONE      },
    [TOK_SLASH]         = {NULL,              compile_binary,      PREC_FACTOR    },
    [TOK_STAR]          = {NULL,              compile_binary,      PREC_FACTOR    },
    [TOK_QMARK]         = {NULL,              compile_ternarycond, PREC_TERNARY   },
    [TOK_BANG]          = {compile_unary,     NULL,                PREC_NONE      },
    [TOK_BANG_EQUAL]    = {NULL,              compile_binary,      PREC_EQUALITY  },
    [TOK_EQUAL]         = {NULL,              NULL,                PREC_NONE      },
    [TOK_EQUAL_EQUAL]   = {NULL,              compile_binary,      PREC_EQUALITY  },
    [TOK_GREATER]       = {NULL,              compile_binary,      PREC_COMPARISON},
    [TOK_GREATER_EQUAL] = {NULL,              compile_binary,      PREC_COMPARISON},
    [TOK_LESS]          = {NULL,              compile_binary,      PREC_COMPARISON},
    [TOK_LESS_EQUAL]    = {NULL,              compile_binary,      PREC_COMPARISON},
    [TOK_IDENTIFIER]    = {compile_variable,  NULL,                PREC_NONE      },
    [TOK_STRING]        = {compile_string,    NULL,                PREC_NONE      },
    [TOK_NUMBER]        = {compile_number,    NULL,                PREC_NONE      },
    [TOK_AND]           = {NULL,              compile_and,         PREC_AND       },
    [TOK_CLASS]         = {NULL,              NULL,                PREC_NONE      },
    [TOK_ELSE]          = {NULL,              NULL,                PREC_NONE      },
    [TOK_FALSE]         = {compile_literal,   NULL,                PREC_NONE      },
    [TOK_FOR]           = {NULL,              NULL,                PREC_NONE      },
    [TOK_FN]            = {NULL,              NULL,                PREC_NONE      },
    [TOK_FIXED]         = {compile_dec_fixed, NULL,                PREC_NONE      },
    [TOK_IF]            = {NULL,              NULL,                PREC_NONE      },
    [TOK_NIL]           = {compile_literal,   NULL,                PREC_NONE      },
    [TOK_OR]            = {NULL,              compile_or,          PREC_OR        },
    [TOK_RETURN]        = {NULL,              NULL,                PREC_NONE      },
    [TOK_SUPER]         = {compile_super,     NULL,                PREC_NONE      },
    [TOK_SELF]          = {compile_self,      NULL,                PREC_NONE      },
    [TOK_TRUE]          = {compile_literal,   NULL,                PREC_NONE      },
    [TOK_VAR]           = {compile_dec_var,   NULL,                PREC_NONE      },
    [TOK_WHILE]         = {NULL,              NULL,                PREC_NONE      },
    [TOK_ERROR]         = {NULL,              NULL,                PREC_NONE      },
    [TOK_EOF]           = {NULL,              NULL,                PREC_NONE      },
};

sstatic void C_new_local(VM* vm, PPC ppc, Token name)
{
    Compiler* C = C();

    if(unlikely(C->loc_len >= C->loc_cap)) {
        if(unlikely(C->loc_len >= BYTECODE_MAX)) {
            COMPILER_LOCAL_LIMIT_ERR(C, BYTECODE_MAX);
            return;
        }
        C_grow_stack(vm, ppc);
        C = C();
    }

    Local* local = &C->locals[C->loc_len++];
    local->name  = name;
    local->flags = (((Byte)(PFLAGS(C->parser) >> 8)) & 0xff);
    local->depth = -1;
}

sstatic force_inline bool Identifier_eq(Token* left, Token* right)
{
    return (left->len == right->len) && (memcmp(left->start, right->start, left->len) == 0);
}

#define MAKE_GLOBAL(C, vm, name, flags)                                                            \
    ({                                                                                             \
        Value identifier = Token_into_stringval(vm, name);                                         \
        C_make_global_identifier(C, vm, identifier, flags);                                        \
    })

#define GET_GLOBAL(C, vm, name) MAKE_GLOBAL(C, vm, name, 0)


sstatic force_inline Int C_get_local(Compiler* C, Token* name)
{
    for(Int i = C->loc_len - 1; i >= 0; i--) {
        Local* local = &C->locals[i];
        if(Identifier_eq(name, &local->name)) {
            if(local->depth == -1) {
                COMPILER_LOCAL_DEFINITION_ERR(C, name->len, name->start);
            }
            return i;
        }
    }

    return -1;
}

sstatic void C_make_local(VM* vm, PPC ppc, Token* name)
{
    Compiler* C = C();

    for(Int i = C->loc_len - 1; i >= 0; i--) {
        Local* local = &C->locals[i];

        if(local->depth != -1 && local->depth < C->depth) {
            break;
        }

        if(Identifier_eq(name, &local->name)) {
            COMPILER_LOCAL_REDEFINITION_ERR(C(), name->len, name->start);
        }
    }

    C_new_local(vm, ppc, *name);
}

sstatic Int compile_arglist(VM* vm, PPC ppc)
{
    Int argc = 0;

    if(!C_check(C(), TOK_RPAREN)) {
        do {
            compile_expr(vm, ppc);
            if(argc == UINT24_MAX) {
                COMPILER_ARGC_LIMIT_ERR(C(), UINT24_MAX);
            }
            argc++;
        } while(C_match(C(), TOK_COMMA));
    }

    C_expect(C(), TOK_RPAREN, "Expect ')' after function arguments.");
    return argc;
}

sstatic void compile_call(VM* vm, PPC ppc)
{
    Int argc = compile_arglist(vm, ppc);
    EMIT_OP(C(), GET_OP_TYPE(argc, OP_CALL), argc);
}

sstatic void compile_stm_expr(VM* vm, PPC ppc)
{
    compile_expr(vm, ppc);
    C_expect(C(), TOK_SEMICOLON, "Expect ';' after expression.");
    EMIT_BYTE(C(), OP_POP);
}

sstatic void compile_expr(VM* vm, PPC ppc)
{
    compile_precedence(vm, ppc, PREC_ASSIGNMENT);
}

sstatic void compile_precedence(VM* vm, PPC ppc, Precedence prec)
{
    // Save old assign bit
    bool assign = PFLAG_CHECK(C()->parser, ASSIGN_BIT);

    C_advance(C());
    ParseFn prefix_fn = rules[PREV_TOKEN(C()).type].prefix;
    if(prefix_fn == NULL) {
        COMPILER_EXPECT_EXPRESSION_ERR(C());
        return;
    }

    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, prec <= PREC_ASSIGNMENT);

    prefix_fn(vm, ppc);

    while(prec <= rules[CURR_TOKEN(C()).type].precedence) {
        C_advance(C());
        ParseFn infix_fn = rules[PREV_TOKEN(C()).type].infix;
        infix_fn(vm, ppc);
    }

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        COMPILER_INVALID_ASSIGN_ERR(C());
    }

    // Restore assign bit
    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, assign);
}

sstatic void compile_dec_fixed(VM* vm, PPC ppc)
{
    PFLAG_SET(C()->parser, FIXED_BIT);
    C_expect(C(), TOK_VAR, "Expect 'var' after 'fixed'.");
    compile_dec_var(vm, ppc);
}

sstatic force_inline void C_scope_start(Compiler* C)
{
    if(unlikely((UInt)C->depth >= UINT32_MAX - 1)) {
        // @?: Is this the correct limit?
        COMPILER_SCOPE_LIMIT_ERR(C, UINT32_MAX);
    }
    C->depth++;
}

// End scope and pop locals and/or close captured locals
sstatic force_inline void C_scope_end(Compiler* C)
{
#define LOCAL_IS_CAPTURED(local) (LFLAG_CHECK((local), VCAPTURED_BIT))

    C->depth--;

    Int pop = 0;
    while(C->loc_len > 0 && C->locals[C->loc_len - 1].depth > C->depth) {

        if(LOCAL_IS_CAPTURED(&C->locals[C->loc_len - 1])) {
            Int capture = 1;
            C->loc_len--;

            if(pop == 1) {
                EMIT_BYTE(C, OP_POP);
            } else if(pop > 1) {
                EMIT_OP(C, OP_POPN, pop);
            }

            pop = 0; // Reset pop count

            do {
                if(!LOCAL_IS_CAPTURED(&C->locals[C->loc_len - 1])) {

                    if(capture == 1) {
                        EMIT_BYTE(C, OP_CLOSE_UPVAL);
                    } else {
                        EMIT_OP(C, OP_CLOSE_UPVALN, capture);
                    }

                    break;
                }

                capture++;
                C->loc_len--;
            } while(C->loc_len > 0 && C->locals[C->loc_len - 1].depth > C->depth);

        } else {
            pop++;
            C->loc_len--;
        }
    }

    if(pop == 1) {
        EMIT_BYTE(C, OP_POP);
    } else if(pop > 1) {
        EMIT_OP(C, OP_POPN, pop);
    }

#undef LOCAL_IS_CAPTURED
}

sstatic void compile_fn(VM* vm, PPC ppc, FunctionType type)
{
#define C_new() (*ppc_new)

    Compiler* C_new = ALLOC_COMPILER(vm);
    vm->compiler    = C_new;
    C_init(C_new, C()->cclass, vm, C()->parser, type, NIL_VAL, C());

    uint64_t mask = PFLAGS(C()->parser);
    PFLAG_RESET(C_new->parser);
    PFLAG_TOGGLE(C_new->parser, ERROR_BIT, mask & btoul(ERROR_BIT));

    PPC ppc_new = &C_new;

    C_scope_start(C_new()); // No need to end this scope (hint CallFrame)
    C_expect(C_new(), TOK_LPAREN, "Expect '(' after function name.");

    // Parse function arguments
    if(!C_check(C_new(), TOK_RPAREN)) {
        do {
            C_new()->fn->arity++;
            compile_varname(vm, ppc_new, "Expect parameter name.");
            INIT_LOCAL(C_new());
        } while(C_match(C_new(), TOK_COMMA));
    }

    C_expect(C_new(), TOK_RPAREN, "Expect ')' after parameters.");
    C_expect(C_new(), TOK_LBRACE, "Expect '{' before function body.");
    compile_stm_block(vm, ppc_new);

    ObjFunction* fn = compile_end(C_new());
    vm->compiler    = C();

    // Restore flags but additionally transfer error flag
    // if error happened in the new compiler.
    mask |= PFLAG_CHECK(C_new()->parser, ERROR_BIT) ? btoul(ERROR_BIT) : 0;
    //
    C_new()->parser->flags &= mask;

    if(fn->upvalc == 0) {
        EMIT_OP(C(), OP_CONST, C_make_const(C(), vm, OBJ_VAL(fn)));
    } else {
        // Create a closure only when we have upvalues
        EMIT_OP(C(), OP_CLOSURE, C_make_const(C(), vm, OBJ_VAL(fn)));
        for(UInt i = 0; i < fn->upvalc; i++) {
            Upvalue* upval = Array_Upvalue_index(C_new()->upvalues, i);
            EMIT_BYTE(C(), upval->local ? 1 : 0);
            EMIT_LBYTE(C(), upval->idx);
        }
    }

    C_free(vm, C_new);

#undef C_new
}

sstatic void compile_dec_fn(VM* vm, PPC ppc)
{
    UInt idx = compile_varname(vm, ppc, "Expect function name.");

    if(C()->depth > 0) {
        /* Initialize the variable that holds the function, in order to
         * allow usage of the function inside of its body. */
        INIT_LOCAL(C());
    }

    compile_fn(vm, ppc, FN_FUNCTION);

    if(C()->depth == 0) {
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_DEFINE_GLOBAL), idx);
    }
}

sstatic force_inline UInt add_upval(Compiler* C, UInt idx, bool local)
{
    for(UInt i = 0; i < C->upvalues->len; i++) {
        Upvalue* upvalue = Array_Upvalue_index(C->upvalues, i);
        if(upvalue->idx == idx && upvalue->local == local) {
            // Return existing UpValue index
            return i;
        }
    }

    if(unlikely(C->upvalues->len == BYTECODE_MAX)) {
        COMPILER_UPVALUE_LIMIT_ERR(C, BYTECODE_MAX, C->fn->name->storage);
        return 0;
    }

    // Otherwise add the UpValue into the array
    C->fn->upvalc++;
    Upvalue upval = {idx, local};
    return Array_Upvalue_push(C->upvalues, upval);
}


sstatic Int get_upval(VM* vm, Compiler* C, Token* name)
{
    if(C->enclosing == NULL) {
        return -1;
    }

    Int idx = C_get_local(C->enclosing, name);
    if(idx != -1) {
        // Local is captured by Upvalue
        LFLAG_SET(&C->enclosing->locals[idx], VCAPTURED_BIT);
        return add_upval(C, (UInt)idx, true);
    }

    idx = get_upval(vm, C->enclosing, name);
    if(idx != -1) {
        return add_upval(C, (UInt)idx, false);
    }

    return -1;
}


sstatic force_inline void named_variable(VM* vm, PPC ppc, Token name)
{
    OpCode  setop, getop;
    Int     idx   = C_get_local(C(), &name);
    int16_t flags = -1;

    if(idx != -1) {
        Local* var = &(C())->locals[idx];
        flags      = LFLAGS(var);
        if(idx > SHORT_STACK_SIZE) {
            setop = OP_SET_LOCALL;
            getop = OP_GET_LOCALL;
        } else {
            setop = OP_SET_LOCAL;
            getop = OP_GET_LOCAL;
        }
    } else if((idx = get_upval(vm, C(), &name)) != -1) {
        Upvalue* upval = Array_Upvalue_index(C()->upvalues, idx);
        Local*   var   = &C()->locals[upval->idx];
        flags          = LFLAGS(var);
        setop          = OP_SET_UPVALUE;
        getop          = OP_GET_UPVALUE;
    } else {
        idx            = GET_GLOBAL(C(), vm, &name);
        Variable* glob = &vm->globvals[idx];
        flags          = VAR_FLAGS(glob);
        if(idx > SHORT_STACK_SIZE) {
            setop = OP_SET_GLOBALL;
            getop = OP_GET_GLOBALL;
        } else {
            setop = OP_SET_GLOBAL;
            getop = OP_GET_GLOBAL;
        }
    }

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        if(BIT_CHECK(flags, VFIXED_BIT)) { /* Statical check for mutability */
            COMPILER_VAR_FIXED_ERR(C(), (Int)name.len, name.start);
        }
        compile_expr(vm, ppc);
        EMIT_OP(C(), setop, idx);
    } else {
        EMIT_OP(C(), getop, idx);
    }
}

sstatic void compile_method(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_FN, "Expect 'fn'.");
    C_expect(C(), TOK_IDENTIFIER, "Expect method name.");
    Value identifier = Token_into_stringval(vm, &PREV_TOKEN(C()));
    UInt  idx        = C_make_const(C(), vm, identifier);

    FunctionType type = FN_METHOD;

    if(AS_STRING(identifier) == vm->statics[SS_INIT]) {
        type = FN_INIT;
    }

    compile_fn(vm, ppc, type);

    if(type == FN_INIT) {
        EMIT_OP(C(), OP_OVERLOAD, SS_INIT);
    }

    EMIT_OP(C(), GET_OP_TYPE(idx, OP_METHOD), idx);
}

sstatic void compile_dec_class(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_IDENTIFIER, "Expect class name.");

    Token class_name = PREV_TOKEN(C());
    Value identifier = Token_into_stringval(vm, &class_name); // GC

    // Idx for OP_CLASS instruction
    UInt idx = C_make_const(C(), vm, identifier); // GC
    EMIT_OP(C(), GET_OP_TYPE(idx, OP_CLASS), idx);

    if(C()->depth > 0) {
        C_make_local(vm, ppc, &class_name);
        INIT_LOCAL(C());
    } else {
        // index for OP_DEFINE_GLOBAL instruction
        UInt gidx = MAKE_GLOBAL(C(), vm, &class_name, PVAR_FLAGS(C()->parser));
        EMIT_OP(C(), GET_OP_TYPE(gidx, OP_DEFINE_GLOBAL), gidx);
    }

    Class cclass;
    cclass.enclosing  = C()->cclass;
    cclass.superclass = false;
    C()->cclass       = &cclass;

    PFLAG_CLEAR(C()->parser, ASSIGN_BIT);

    if(C_match(C(), TOK_IMPL)) {
        C_expect(C(), TOK_IDENTIFIER, "Expect superclass name.");
        compile_variable(vm, ppc); // Get superclass
        if(Identifier_eq(&PREV_TOKEN(C()), &class_name)) {
            COMPILER_CLASS_INHERIT_ERR(C(), AS_CSTRING(identifier));
        }
        cclass.superclass = true;
        C_scope_start(C());
        C_new_local(vm, ppc, Token_syn_new("super"));
        INIT_LOCAL(C());
        named_variable(vm, ppc, class_name); // Get subclass (this class)
        EMIT_BYTE(C(), OP_INHERIT);
    }

    named_variable(vm, ppc, class_name); // Push the class

    C_expect(C(), TOK_LBRACE, "Expect '{' before class body.");
    while(!C_check(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        compile_method(vm, ppc);
    }

    C_expect(C(), TOK_RBRACE, "Expect '}' after class body.");
    EMIT_BYTE(C(), OP_POP); // Pop the class

    C()->cclass = cclass.enclosing;

    if(cclass.superclass) {
        C_scope_end(C());
    }
}

sstatic void compile_dec(VM* vm, PPC ppc)
{
    // Clear upper byte of parser flags
    // for now only 'FIXED' flag
    PFLAG_CLEAR(C()->parser, FIXED_BIT);
    PFLAG_SET(C()->parser, ASSIGN_BIT);

    if(C_match(C(), TOK_VAR)) {
        compile_dec_var(vm, ppc);
    } else if(C_match(C(), TOK_FIXED)) {
        compile_dec_fixed(vm, ppc);
    } else if(C_match(C(), TOK_FN)) {
        compile_dec_fn(vm, ppc);
    } else if(C_match(C(), TOK_CLASS)) {
        compile_dec_class(vm, ppc);
    } else {
        compile_stm(vm, ppc);
    }

    if(PFLAG_CHECK(C()->parser, PANIC_BIT)) {
        // Sync parser before compiling the next
        // declaration/statement.
        Parser_sync(C());
    }
}

sstatic void compile_dec_var(VM* vm, PPC ppc)
{
    if(C_match(C(), TOK_FIXED)) {
        if(PFLAG_CHECK(C()->parser, FIXED_BIT)) {
            C_error(C(), "Expect variable name.");
        }
        PFLAG_SET(C()->parser, FIXED_BIT);
    }

    Int index = compile_varname(vm, ppc, "Expect variable name.");

    if(C_match(C(), TOK_EQUAL)) {
        compile_expr(vm, ppc);
    } else {
        EMIT_BYTE(C(), OP_NIL);
    }

    C_expect(C(), TOK_SEMICOLON, "Expect ';' after variable declaration.");

    if((C())->depth > 0) {
        INIT_LOCAL(C());
        return;
    }

    EMIT_OP(C(), GET_OP_TYPE(index, OP_DEFINE_GLOBAL), index);
}

sstatic UInt compile_varname(VM* vm, PPC ppc, const char* errmsg)
{
    C_expect(C(), TOK_IDENTIFIER, errmsg);
    Token* name = &PREV_TOKEN(C());

    // If local scope make local variable
    if((C())->depth > 0) {
        C_make_local(vm, ppc, name);
        return 0;
    }

    // Otherwise make global variable
    return MAKE_GLOBAL(C(), vm, name, PVAR_FLAGS(C()->parser));
}

sstatic force_inline void C_patch_jmp(Compiler* C, UInt jmp_offset)
{
    UInt offset = code_offset(C) - jmp_offset - 3;

    if(unlikely(offset >= UINT24_MAX)) {
        COMPILER_JUMP_LIMIT_ERR(C, UINT24_MAX);
    }

    PUT_BYTES3(&current_chunk(C)->code.data[jmp_offset], offset);
}

sstatic force_inline void add_breakstorage(VM* vm, Compiler* C)
{
    Array_Int patches;
    Array_Int_init(&patches, vm);
    Array_Array_Int_push(&C->context.breaks, patches);
}

sstatic force_inline void remove_breakstorage(Compiler* C)
{
    Array_Int* patches = Array_Array_Int_last(&C->context.breaks);
    for(Int i = 0; i < (Int)patches->len; i++) {
        C_patch_jmp(C, patches->data[i]);
    }
    Array_Int arr = Array_Array_Int_pop(&C->context.breaks);
    Array_Int_free(&arr, NULL);
}

sstatic void compile_stm_switch(VM* vm, PPC ppc)
{
    uint64_t mask = CFLOW_MASK(C()->parser);
    PFLAG_CLEAR(C()->parser, LOOP_BIT);
    PFLAG_SET(C()->parser, SWITCH_BIT);

    add_breakstorage(vm, C());

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'switch'.");
    compile_expr(vm, ppc);
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");
    C_expect(C(), TOK_LBRACE, "Expect '{' after ')'.");

    /* -1 = parsed TOK_DEFAULT
     *  0 = didn't parse TOK_DEFAULT or TOK_CASE yet
     *  >0 = parsed TOK_CASE (stores jmp offset) */
    Int  state = 0;
    bool dflt  = false; /* Set if 'default' is parsed */

    /* fall-through jumps that need patching */
    Array_Int fts;
    Array_Int_init(&fts, vm);

    Int outermostsw_depth          = C()->context.innermostsw_depth;
    C()->context.innermostsw_depth = C()->depth;

    while(!C_match(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        if(C_match(C(), TOK_CASE) || C_match(C(), TOK_DEFAULT)) {
            if(state != 0) {
                Array_Int_push(&fts, EMIT_JMP(C(), OP_JMP));
                if(state != -1) {
                    C_patch_jmp(C(), state);
                }
            }

            state = -1;

            if(PREV_TOKEN(C()).type == TOK_CASE) {
                compile_expr(vm, ppc);
                EMIT_BYTE(C(), OP_EQ);
                C_expect(C(), TOK_COLON, "Expect ':' after 'case'.");
                state = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
            } else if(!dflt) {
                dflt = true;
                C_expect(C(), TOK_COLON, "Expect ':' after 'default'.");
            } else {
                COMPILER_SWITCH_DEFAULT_ERR(C());
            }

            if(fts.len > 0) {
                /* Patch Fall-through jump */
                C_patch_jmp(C(), *Array_Int_last(&fts));
            }
        } else {
            if(state == 0) {
                COMPILER_SWITCH_NOCASE_ERR(C());
            }
            compile_stm(vm, ppc);
        }
    }

    if(PREV_TOKEN(C()).type == TOK_EOF) {
        COMPILER_SWITCH_RBRACE_ERR(C());
    }

    /* Free fallthrough jumps array */
    Array_Int_free(&fts, NULL);
    /* Patch and remove breaks */
    remove_breakstorage(C());
    /* Pop switch value */
    EMIT_BYTE(C(), OP_POP);
    /* Restore scope depth */
    C()->context.innermostsw_depth = outermostsw_depth;
    /* Clear switch flag and restore control flow flags */
    PFLAG_CLEAR(C()->parser, SWITCH_BIT);
    C()->parser->flags |= mask;
}

sstatic void compile_stm_if(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_LPAREN, "Expect '(' after 'if'.");
    compile_expr(vm, ppc); /* Parse conditional */
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional jump instruction */
    UInt else_jmp = EMIT_JMP(C(), OP_JMP_IF_FALSE_AND_POP);

    compile_stm(vm, ppc); /* Parse the code in this branch */

    /* Prevent fall-through if 'else' exists. */
    UInt end_jmp = EMIT_JMP(C(), OP_JMP);

    C_patch_jmp(C(), else_jmp); /* End of 'if' (maybe start of else) */

    if(C_match(C(), TOK_ELSE)) {
        compile_stm(vm, ppc);      /* Parse the else branch */
        C_patch_jmp(C(), end_jmp); /* End of else branch */
    }
}

sstatic void compile_and(VM* vm, PPC ppc)
{
    UInt jump = EMIT_JMP(C(), OP_JMP_IF_FALSE_OR_POP);
    compile_precedence(vm, ppc, PREC_AND);
    C_patch_jmp(C(), jump);
}

sstatic void compile_or(VM* vm, PPC ppc)
{
    UInt else_jmp = EMIT_JMP(C(), OP_JMP_IF_FALSE_AND_POP);
    UInt end_jmp  = EMIT_JMP(C(), OP_JMP);
    C_patch_jmp(C(), else_jmp);
    compile_precedence(vm, ppc, PREC_OR);
    C_patch_jmp(C(), end_jmp);
}

sstatic void compile_stm_while(VM* vm, PPC ppc)
{
    uint64_t mask = CFLOW_MASK(C()->parser);
    PFLAG_CLEAR(C()->parser, SWITCH_BIT);
    PFLAG_SET(C()->parser, LOOP_BIT);

    /* Add loop offset storage for 'break' */
    add_breakstorage(vm, C());

    Int outermostl_start            = (C())->context.innermostl_start;
    Int outermostl_depth            = (C())->context.innermostl_depth;
    (C())->context.innermostl_start = current_chunk(C())->code.len;
    (C())->context.innermostl_depth = (C())->depth;

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'while'.");
    compile_expr(vm, ppc);
    C_expect(C(), TOK_RPAREN, "Expect ')' after condition.");

    /* Setup the conditional exit jump */
    UInt end_jmp = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
    compile_stm(vm, ppc);                              /* Parse loop body */
    C_emit_loop(C(), (C())->context.innermostl_start); /* Jump to the start of the loop */

    C_patch_jmp(C(), end_jmp); /* Set loop exit offset */

    /* Restore the outermost loop start/(scope)depth */
    (C())->context.innermostl_start = outermostl_start;
    (C())->context.innermostl_depth = outermostl_depth;
    /* Remove and patch breaks */
    remove_breakstorage(C());
    /* Clear loop flag */
    PFLAG_CLEAR(C()->parser, LOOP_BIT);
    /* Restore old flags */
    (C())->parser->flags |= mask;
}

sstatic void compile_stm_for(VM* vm, PPC ppc)
{
    uint64_t mask = CFLOW_MASK(C()->parser);
    PFLAG_CLEAR(C()->parser, SWITCH_BIT);
    PFLAG_SET(C()->parser, LOOP_BIT);

    /* Add loop offset storage for 'break' */
    add_breakstorage(vm, C());
    /* Start a new local scope */
    C_scope_start(C());

    C_expect(C(), TOK_LPAREN, "Expect '(' after 'for'.");
    if(C_match(C(), TOK_SEMICOLON)) {
        // No initializer
    } else if(C_match(C(), TOK_VAR)) {
        compile_dec_var(vm, ppc);
    } else if(C_match(C(), TOK_FIXED)) {
        compile_dec_fixed(vm, ppc);
    } else {
        compile_stm_expr(vm, ppc);
    }

    /* Save outermost loop start/depth on the stack */
    Int outermostl_start = (C())->context.innermostl_start;
    Int outermostl_depth = (C())->context.innermostl_depth;
    /* Update context inner loop start/depth */
    (C())->context.innermostl_start = current_chunk(C())->code.len;
    (C())->context.innermostl_depth = (C())->depth;

    Int loop_end = -1;
    if(!C_match(C(), TOK_SEMICOLON)) {
        compile_expr(vm, ppc);
        C_expect(C(), TOK_SEMICOLON, "Expect ';' after for-loop condition clause.");
        loop_end = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
    }

    if(!C_match(C(), TOK_RPAREN)) {
        UInt body_start      = EMIT_JMP(C(), OP_JMP);
        UInt increment_start = current_chunk(C())->code.len;
        compile_expr(vm, ppc);
        EMIT_BYTE(C(), OP_POP);
        C_expect(C(), TOK_RPAREN, "Expect ')' after last for-loop clause.");

        C_emit_loop(C(), (C())->context.innermostl_start);
        (C())->context.innermostl_start = increment_start;
        C_patch_jmp(C(), body_start);
    }

    compile_stm(vm, ppc);
    C_emit_loop(C(), (C())->context.innermostl_start);

    if(loop_end != -1) {
        C_patch_jmp(C(), loop_end);
    }

    /* Restore the outermost loop start/depth */
    (C())->context.innermostl_start = outermostl_start;
    (C())->context.innermostl_depth = outermostl_depth;
    /* Remove and patch loop breaks */
    remove_breakstorage(C());
    /* End the scope */
    C_scope_end(C());
    /* Restore old flags */
    PFLAG_CLEAR(C()->parser, LOOP_BIT);
    (C())->parser->flags |= mask;
}

sstatic void compile_stm_continue(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'continue'.");

    if(C->context.innermostl_start == -1) {
        COMPILER_CONTINUE_ERR(C);
    }

    Int sdepth = C->context.innermostl_depth;

    UInt popn = 0;
    for(Int i = C->loc_len - 1; i >= 0 && C->locals[i].depth > sdepth; i--) {
        popn++;
    }

    /* If we have continue inside a switch statement
     * then don't forget to pop off the switch value */
    if(PFLAG_CHECK(C->parser, SWITCH_BIT)) {
        popn++;
    }

    // Keep bytecode tidy
    if(popn > 1) {
        EMIT_OP(C, OP_POPN, popn);
    } else {
        EMIT_BYTE(C, OP_POP);
    }

    C_emit_loop(C, C->context.innermostl_start);
}

sstatic void compile_stm_break(Compiler* C)
{
    C_expect(C, TOK_SEMICOLON, "Expect ';' after 'break'.");

    Array_Array_Int* arr = &C->context.breaks;

    if(!PFLAG_CHECK(C->parser, LOOP_BIT) && !PFLAG_CHECK(C->parser, SWITCH_BIT)) {
        COMPILER_BREAK_ERR(C);
        return;
    }

    UInt sdepth = PFLAG_CHECK(C->parser, LOOP_BIT) ? C->context.innermostl_depth
                                                   : C->context.innermostsw_depth;

    UInt popn = 0;
    for(Int i = C->loc_len - 1; i >= 0 && C->locals[i].depth > (Int)sdepth; i--) {
        popn++;
    }

    // Keep bytecode tidy
    if(popn > 1) {
        EMIT_OP(C, OP_POPN, popn);
    } else {
        EMIT_BYTE(C, OP_POP);
    }

    Array_Int_push(Array_Array_Int_last(arr), EMIT_JMP(C, OP_JMP));
}

sstatic void compile_stm_return(VM* vm, PPC ppc)
{
    PFLAG_SET(C()->parser, RET_BIT);

    if(C()->fn_type == FN_SCRIPT) {
        PFLAG_SET(C()->parser, TOPRET_BIT);
    }

    if(C_match(C(), TOK_SEMICOLON)) {
        C_emit_return(C());
    } else {
        if(C()->fn_type == FN_INIT) {
            COMPILER_RETURN_INIT_ERR(C(), static_str[SS_INIT].name);
        }

        compile_expr(vm, ppc);
        C_expect(C(), TOK_SEMICOLON, "Expect ';' after return value.");
        EMIT_BYTE(C(), OP_RET);
    }

    PFLAG_CLEAR(C()->parser, TOPRET_BIT);
}

sstatic void compile_stm(VM* vm, PPC ppc)
{
    Compiler* C = C();
    // @TODO: Implement jmp table
    if(C_match(C, TOK_WHILE)) {
        compile_stm_while(vm, ppc);
    } else if(C_match(C, TOK_FOR)) {
        compile_stm_for(vm, ppc);
    } else if(C_match(C, TOK_IF)) {
        compile_stm_if(vm, ppc);
    } else if(C_match(C, TOK_SWITCH)) {
        compile_stm_switch(vm, ppc);
    } else if(C_match(C, TOK_LBRACE)) {
        C_scope_start(C);
        compile_stm_block(vm, ppc);
        C_scope_end(C());
    } else if(C_match(C, TOK_CONTINUE)) {
        compile_stm_continue(C);
    } else if(C_match(C, TOK_BREAK)) {
        compile_stm_break(C);
    } else if(C_match(C, TOK_RETURN)) {
        compile_stm_return(vm, ppc);
    } else {
        compile_stm_expr(vm, ppc);
    }
}

sstatic void compile_stm_block(VM* vm, PPC ppc)
{
    while(!C_check(C(), TOK_RBRACE) && !C_check(C(), TOK_EOF)) {
        compile_dec(vm, ppc);
    }
    C_expect(C(), TOK_RBRACE, "Expect '}' after block.");
}

sstatic force_inline void compile_number(VM* vm, PPC ppc)
{
    Compiler* C   = C();
    UInt      idx = C_make_const(C, vm, PREV_TOKEN(C).value);
    EMIT_OP(C, GET_OP_TYPE(idx, OP_CONST), idx);
}

sstatic force_inline void compile_variable(VM* vm, PPC ppc)
{
    named_variable(vm, ppc, PREV_TOKEN(C()));
}

sstatic force_inline void compile_string(VM* vm, PPC ppc)
{
    UInt idx = C_make_const(C(), vm, PREV_TOKEN(C()).value);
    EMIT_OP(C(), GET_OP_TYPE(idx, OP_CONST), idx);
}

/* This is the entry point to Pratt parsing */
sstatic force_inline void compile_grouping(VM* vm, PPC ppc)
{
    compile_expr(vm, ppc);
    C_expect(C(), TOK_RPAREN, "Expect ')' after expression");
}

sstatic void compile_unary(VM* vm, PPC ppc)
{
    TokenType type = PREV_TOKEN(C()).type;
    compile_precedence(vm, ppc, PREC_UNARY);

    switch(type) {
        case TOK_MINUS:
            EMIT_BYTE(C(), OP_NEG);
            break;
        case TOK_BANG:
            EMIT_BYTE(C(), OP_NOT);
            break;
        default:
            unreachable;
            return;
    }
}

sstatic void compile_binary(VM* vm, PPC ppc)
{
    TokenType        type = PREV_TOKEN(C()).type;
    const ParseRule* rule = &rules[type];
    compile_precedence(vm, ppc, rule->precedence + 1);

#ifdef S_PRECOMPUTED_GOTO
    #define TOK_TABLE
    #include "jmptable.h"
    #undef TOK_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break;
#endif
    Compiler* C = C();

    DISPATCH(type)
    {
        CASE(TOK_MINUS)
        {
            EMIT_BYTE(C, OP_SUB);
            BREAK;
        }
        CASE(TOK_PLUS)
        {
            EMIT_BYTE(C, OP_ADD);
            BREAK;
        }
        CASE(TOK_SLASH)
        {
            EMIT_BYTE(C, OP_DIV);
            BREAK;
        }
        CASE(TOK_STAR)
        {
            EMIT_BYTE(C, OP_MUL);
            BREAK;
        }
        CASE(TOK_BANG_EQUAL)
        {
            EMIT_BYTE(C, OP_NOT_EQUAL);
            BREAK;
        }
        CASE(TOK_EQUAL_EQUAL)
        {
            EMIT_BYTE(C, OP_EQUAL);
            BREAK;
        }
        CASE(TOK_GREATER)
        {
            EMIT_BYTE(C, OP_GREATER);
            BREAK;
        }
        CASE(TOK_GREATER_EQUAL)
        {
            EMIT_BYTE(C, OP_GREATER_EQUAL);
            BREAK;
        }
        CASE(TOK_LESS)
        {
            EMIT_BYTE(C, OP_LESS);
            BREAK;
        }
        CASE(TOK_LESS_EQUAL)
        {
            EMIT_BYTE(C, OP_LESS_EQUAL);
            BREAK;
        }
        CASE(TOK_LBRACK)
        CASE(TOK_RBRACK)
        CASE(TOK_LPAREN)
        CASE(TOK_RPAREN)
        CASE(TOK_LBRACE)
        CASE(TOK_RBRACE)
        CASE(TOK_DOT)
        CASE(TOK_COMMA)
        CASE(TOK_COLON)
        CASE(TOK_SEMICOLON)
        CASE(TOK_QMARK)
        CASE(TOK_BANG)
        CASE(TOK_EQUAL)
        CASE(TOK_IDENTIFIER)
        CASE(TOK_STRING)
        CASE(TOK_NUMBER)
        CASE(TOK_AND)
        CASE(TOK_BREAK)
        CASE(TOK_CASE)
        CASE(TOK_CONTINUE)
        CASE(TOK_CLASS)
        CASE(TOK_DEFAULT)
        CASE(TOK_ELSE)
        CASE(TOK_FALSE)
        CASE(TOK_FOR)
        CASE(TOK_FN)
        CASE(TOK_IF)
        CASE(TOK_IMPL)
        CASE(TOK_NIL)
        CASE(TOK_OR)
        CASE(TOK_RETURN)
        CASE(TOK_SUPER)
        CASE(TOK_SELF)
        CASE(TOK_SWITCH)
        CASE(TOK_TRUE)
        CASE(TOK_VAR)
        CASE(TOK_WHILE)
        CASE(TOK_FIXED)
        CASE(TOK_ERROR)
        CASE(TOK_EOF)
        {
            unreachable;
        }
    }

    unreachable;

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

sstatic force_inline void compile_ternarycond(VM* vm, PPC ppc)
{
    UInt jmp_expr2 = EMIT_JMP(C(), OP_JMP_IF_FALSE_POP);
    // First conditional operator is parsed as if
    // parenthesized (according to C standard)
    compile_precedence(vm, ppc, PREC_TERNARY);
    UInt jmp_end = EMIT_JMP(C(), OP_JMP);
    C_expect(C(), TOK_COLON, "Expect ':' after first 'expr'.");
    C_patch_jmp(C(), jmp_expr2);
    compile_precedence(vm, ppc, PREC_ASSIGNMENT);
    C_patch_jmp(C(), jmp_end);
}

sstatic void compile_dot(VM* vm, PPC ppc)
{
    C_expect(C(), TOK_IDENTIFIER, "Expect property name after '.'.");
    Value identifier = Token_into_stringval(vm, &PREV_TOKEN(C())); // GC
    UInt  idx        = C_make_const(C(), vm, identifier);          // GC

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        compile_expr(vm, ppc);
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_SET_PROPERTY), idx);
    } else if(C_match(C(), TOK_LPAREN)) {
        Int argc = compile_arglist(vm, ppc);
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_INVOKE), idx);
        EMIT_LBYTE(C(), argc); // Emit always 24-bit parameter
    } else {
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_GET_PROPERTY), idx);
    }
}


sstatic void compile_index(VM* vm, PPC ppc)
{
    compile_expr(vm, ppc);
    C_expect(C(), TOK_RBRACK, "Expect ']'.");

    if(PFLAG_CHECK(C()->parser, ASSIGN_BIT) && C_match(C(), TOK_EQUAL)) {
        compile_expr(vm, ppc);
        EMIT_BYTE(C(), OP_SET_INDEX);
    } else if(C_match(C(), TOK_LPAREN)) {
        Int argc = compile_arglist(vm, ppc);
        EMIT_OP(C(), OP_INVOKE_INDEX, argc);
    } else {
        EMIT_BYTE(C(), OP_INDEX);
    }
}

sstatic force_inline void compile_literal(unused VM* _, PPC ppc)
{
    Compiler* C = C();
    switch(PREV_TOKEN(C).type) {
        case TOK_TRUE:
            EMIT_BYTE(C, OP_TRUE);
            break;
        case TOK_FALSE:
            EMIT_BYTE(C, OP_FALSE);
            break;
        case TOK_NIL:
            if(PFLAG_CHECK(C()->parser, TOPRET_BIT)) {
                C_error(C(), "Top-level function can't return 'nil'.");
            }
            EMIT_BYTE(C, OP_NIL);
            break;
        default:
            unreachable;
            return;
    }
}

sstatic void compile_self(VM* vm, PPC ppc)
{
    if(C()->cclass == NULL) {
        COMPILER_SELF_ERR(C());
        return;
    }

    bool assign = PFLAG_CHECK(C()->parser, ASSIGN_BIT);
    PFLAG_CLEAR(C()->parser, ASSIGN_BIT); // Can't assign to 'self'
    compile_variable(vm, ppc);            // fetch 'self' variable
    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, assign);
}

sstatic void compile_super(VM* vm, PPC ppc)
{
    if(C()->cclass == NULL) {
        COMPILER_SUPER_ERR(C());
        return;
    } else if(!C()->cclass->superclass) {
        COMPILER_NO_SUPER_ERR(C());
        return;
    }

    C_expect(C(), TOK_DOT, "Expect '.' after 'super'.");
    C_expect(C(), TOK_IDENTIFIER, "Expect superclass method name after '.'.");

    Value identifier = Token_into_stringval(vm, &PREV_TOKEN(C()));
    UInt  idx        = C_make_const(C(), vm, identifier);
    bool  assign     = PFLAG_CHECK(C()->parser, ASSIGN_BIT);

    PFLAG_CLEAR(C()->parser, ASSIGN_BIT);
    named_variable(vm, ppc, Token_syn_new("self"));
    if(C_match(C(), TOK_LPAREN)) {
        UInt argc = compile_arglist(vm, ppc);
        named_variable(vm, ppc, Token_syn_new("super"));
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_INVOKE_SUPER), idx);
        EMIT_LBYTE(C(), argc);
    } else {
        named_variable(vm, ppc, Token_syn_new("super"));
        EMIT_OP(C(), GET_OP_TYPE(idx, OP_GET_SUPER), idx);
    }
    PFLAG_TOGGLE(C()->parser, ASSIGN_BIT, assign);
}
