#include "common.h"
#include "compiler.h"
#include "mem.h"
#include "object.h"
#include "vmachine.h"

#ifdef DEBUG_TRACE_EXECUTION
    #include "debug.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// @TODO: Make some errors into warnings, for example emit warning when negating boolean
// values by converting them implicitly during compilation into numbers.

#define stack_peek(vm, top) ((Value) * ((vm)->sp - (top + 1)))
#define stack_reset(vm)     (vm)->sp = (vm)->stack
#define stack_size(vm)      ((vm)->sp - (vm)->stack)

/* This doesn't get inlined with gcc having -O3 optimizations enabled
 * this is the reason behind the force_inline attribute  */
SK_STATIC_INLINE(void) VM_push(VM* vm, Value val)
{
    if(_likely(vm->sp - vm->stack < STACK_MAX)) {
        *vm->sp++ = val;
    } else {
        fprintf(stderr, "Skooma: stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

SK_STATIC_INLINE(Value) VM_pop(VM* vm)
{
    return *--vm->sp;
}

void VM_error(VM* vm, const char* errfmt, ...)
{
    va_list ap;
    va_start(ap, errfmt);
    vfprintf(stderr, errfmt, ap);
    va_end(ap);
    fputs("\n", stderr);

    UInt line = Chunk_getline(vm->chunk, vm->ip - vm->chunk->code.data - 1);
    fprintf(stderr, "[line: %u] in script\n", line);
    stack_reset(vm);
}

/* NIL and FALSE are always falsey */
SK_STATIC_INLINE(bool) isfalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

SK_STATIC_INLINE(uint8_t) bool_to_str(char** str, bool boolean)
{
    if(boolean) {
        *str = "true";
        return sizeof("true") - 1;
    }
    *str = "false";
    return sizeof("false") - 1;
}

SK_STATIC_INLINE(uint8_t) nil_to_str(char** str)
{
    *str = "nil";
    return sizeof("nil") - 1;
}

/* Current number is stored in static memory to prevent the need
 * for allocating before creating the 'ObjString', keep in mind this
 * function is only safe for usage inside a concatenate function.
 * Additionally concatenate function guarantees there is only one possible number Value,
 * therefore there is no need for two static buffers inside of it. */
SK_STATIC_INLINE(uint8_t) double_to_str(char** str, double dbl)
{
    static char buffer[30] = {0};
    uint8_t     len        = snprintf(buffer, 30, "%f", dbl);
    /* Trim excess zeroes and the floating point '.' if no fractional part */
    for(uint8_t i = len - 1; i > 0; i--) {
        if(buffer[i] == '0' || buffer[i] == '.') {
            len--;
        } else {
            break;
        }
    }
    *str = buffer;
    return len;
}

static ObjString* concatenate(VM* vm, Value a, Value b)
{
    Value  value[2] = {a, b};
    size_t len[2]   = {0};
    char*  str[2]   = {0};

    for(int i = 0; i < 2; i++) {
#ifdef THREADED_CODE
        // NOTE: In case ValueType enum gets modified update this table
        static const void* jump_table[] = {
            // Keep this in the same order as in the ValueType enum
            &&vbool,   /* VAL_BOOL */
            &&vnumber, /* VAL_NUMBER */
            &&vnil,    /* VAL_NIL */
            &&vobj,    /* VAL_OBJ */
            NULL,      /* VAL_EMPTY */
        };

        goto* jump_table[value[i].type];

    vbool:
        len[i] = bool_to_str(&str[i], AS_BOOL(value[i]));
        continue;
    vnumber:
        len[i] = double_to_str(&str[i], AS_NUMBER(value[i]));
        continue;
    vnil:
        len[i] = nil_to_str(&str[i]);
        continue;
    vobj:
        str[i] = AS_CSTRING(value[i]);
        len[i] = AS_STRING(value[i])->len;
        continue;
#else
        switch(value[i].type) {
            case VAL_BOOL:
                len[i] = bool_to_str(&str[i], AS_BOOL(value[i]));
                break;
            case VAL_NUMBER:
                len[i] = double_to_str(&str[i], AS_NUMBER(value[i]));
                break;
            case VAL_NIL:
                len[i] = nil_to_str(&str[i]);
                break;
            case VAL_OBJ:
                str[i] = AS_CSTRING(value[i]);
                len[i] = AS_STRING(value[i])->len;
                break;
            case VAL_EMPTY:
            default:
                _unreachable;
        }
#endif
    }
    return ObjString_from_concat(vm, str[0], len[0], str[1], len[1]);
}

/*
 *
 */
/*======================= VM core functions ==========================*/

VM* VM_new(void)
{
    VM* vm = NULL;
    vm     = MALLOC(sizeof(VM));
    VM_init(vm);
    return vm;
}

void VM_init(VM* vm)
{
    vm->chunk   = NULL;
    vm->ip      = NULL;
    vm->objects = NULL;
    stack_reset(vm);
    HashTable_init(&vm->globals);
    HashTable_init(&vm->strings);
}

static InterpretResult VM_run(VM* vm)
{
#define BINARY_OP(value_type, op)                                                        \
    do {                                                                                 \
        if(!IS_NUMBER(stack_peek(vm, 0)) || !IS_NUMBER(stack_peek(vm, 1))) {             \
            VM_error(vm, "Operands must be numbers (binary operation '" #op "').");      \
            return INTERPRET_RUNTIME_ERROR;                                              \
        }                                                                                \
        double b = AS_NUMBER(VM_pop(vm));                                                \
        double a = AS_NUMBER(VM_pop(vm));                                                \
        VM_push(vm, value_type(a op b));                                                 \
    } while(false)

#define CONCAT_OR_ADD()                                                                  \
    do {                                                                                 \
        if(IS_NUMBER(stack_peek(vm, 0)) && IS_NUMBER(stack_peek(vm, 1))) {               \
            double b = AS_NUMBER(VM_pop(vm));                                            \
            double a = AS_NUMBER(VM_pop(vm));                                            \
            VM_push(vm, NUMBER_VAL((a + b)));                                            \
        } else {                                                                         \
            Value b = VM_pop(vm);                                                        \
            Value a = VM_pop(vm);                                                        \
            VM_push(vm, OBJ_VAL(concatenate(vm, a, b)));                                 \
        }                                                                                \
    } while(false)

#define READ_BYTE()      (*vm->ip++)
#define READ_CONSTANT()  vm->chunk->constants.data[READ_BYTE()]
#define READ_CONSTANTL() vm->chunk->constants.data[GET_BYTES3(vm->ip)]
#define READ_STRING()    AS_STRING(READ_CONSTANT())
#define READ_STRINGL()   AS_STRING(READ_CONSTANTL())
#define VM_DISPATCH(x)   switch(x)
#define VM_CASE(label)   case label:
#define VM_BREAK         break

    while(true) {
#ifdef THREADED_CODE
    #include "jmptable.h"
#endif
#ifdef DEBUG_TRACE_EXECUTION
    #undef VM_BREAK
    #define VM_BREAK continue
        printf("           ");
        for(Value* ptr = vm->stack; ptr < vm->sp; ptr++) {
            printf("[");
            Value_print(*ptr);
            printf("]");
        }
        printf("\n");
        Instruction_debug(vm->chunk, (UInt)(vm->ip - vm->chunk->code.data));
#endif
        VM_DISPATCH(READ_BYTE())
        {
            VM_CASE(OP_CONST)
            {
                VM_push(vm, READ_CONSTANT());
                VM_BREAK;
            }
            VM_CASE(OP_CONSTL)
            {
                VM_push(vm, READ_CONSTANTL());
                vm->ip += 3;
                VM_BREAK;
            }
            VM_CASE(OP_TRUE)
            {
                VM_push(vm, BOOL_VAL(true));
                VM_BREAK;
            }
            VM_CASE(OP_FALSE)
            {
                VM_push(vm, BOOL_VAL(false));
                VM_BREAK;
            }
            VM_CASE(OP_NIL)
            {
                VM_push(vm, NIL_VAL);
                VM_BREAK;
            }
            VM_CASE(OP_NEG)
            {
                if(!IS_NUMBER(stack_peek(vm, 0))) {
                    VM_error(vm, "Operand must be a number (unary negation '-').");
                    return INTERPRET_RUNTIME_ERROR;
                }
                AS_NUMBER_REF(vm->sp - 1) = -AS_NUMBER(stack_peek(vm, 0));
                VM_BREAK;
            }
            VM_CASE(OP_ADD)
            {
                CONCAT_OR_ADD();
                VM_BREAK;
            }
            VM_CASE(OP_SUB)
            {
                BINARY_OP(NUMBER_VAL, -);
                VM_BREAK;
            }
            VM_CASE(OP_MUL)
            {
                BINARY_OP(NUMBER_VAL, *);
                VM_BREAK;
            }
            VM_CASE(OP_DIV)
            {
                BINARY_OP(NUMBER_VAL, /);
                VM_BREAK;
            }
            VM_CASE(OP_NOT)
            {
                *(vm->sp - 1) = BOOL_VAL(isfalsey(stack_peek(vm, 0)));
                VM_BREAK;
            }
            VM_CASE(OP_NOT_EQUAL)
            {
                Value a = VM_pop(vm);
                Value b = VM_pop(vm);
                VM_push(vm, BOOL_VAL(!Value_eq(b, a)));
                VM_BREAK;
            }
            VM_CASE(OP_EQUAL)
            {
                Value a = VM_pop(vm);
                Value b = VM_pop(vm);
                VM_push(vm, BOOL_VAL(Value_eq(b, a)));
                VM_BREAK;
            }
            VM_CASE(OP_GREATER)
            {
                BINARY_OP(BOOL_VAL, >);
                VM_BREAK;
            }
            VM_CASE(OP_GREATER_EQUAL)
            {
                BINARY_OP(BOOL_VAL, >=);
                VM_BREAK;
            }
            VM_CASE(OP_LESS)
            {
                BINARY_OP(BOOL_VAL, <);
                VM_BREAK;
            }
            VM_CASE(OP_LESS_EQUAL)
            {
                BINARY_OP(BOOL_VAL, <=);
                VM_BREAK;
            }
            VM_CASE(OP_PRINT)
            {
                Value_print(VM_pop(vm));
                printf("\n");
                VM_BREAK;
            }
            VM_CASE(OP_POP)
            {
                VM_pop(vm);
                VM_BREAK;
            }
            VM_CASE(OP_DEFINE_GLOBAL)
            {
                ObjString* name = READ_STRING();
                HashTable_insert(&vm->globals, OBJ_VAL(name), stack_peek(vm, 0));
                VM_pop(vm);
                VM_BREAK;
            }
            VM_CASE(OP_DEFINE_GLOBALL)
            {
                ObjString* name = READ_STRINGL();
                vm->ip          += 3;
                HashTable_insert(&vm->globals, OBJ_VAL(name), stack_peek(vm, 0));
                VM_pop(vm);
                VM_BREAK;
            }
            VM_CASE(OP_GET_GLOBAL)
            {
                VM_BREAK;
            }
            VM_CASE(OP_GET_GLOBALL)
            {
                VM_BREAK;
            }
            VM_CASE(OP_RET)
            {
                return INTERPRET_OK;
            }
        }
    }

    _unreachable;

#undef VM_BINARY_OP
#undef VM_CONCAT_OR_ADD
#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_CONSTANTL
#undef READ_STRING
#undef READ_STRINGL
#undef VM_DISPATCH
#undef VM_CASE
#undef VM_BREAK
}

InterpretResult VM_interpret(VM* vm, const char* source)
{
    Chunk chunk;
    Chunk_init(&chunk);

    if(!compile(vm, source, &chunk)) {
        Chunk_free(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm->chunk = &chunk;
    vm->ip    = vm->chunk->code.data;

    InterpretResult result = VM_run(vm);

    Chunk_free(&chunk);
    return result;
}

void VM_free(VM* vm)
{
    if(_likely(vm->chunk != NULL)) { // @TODO: Remove this?
        Chunk_free(vm->chunk);
    }
    HashTable_free(&vm->globals);
    HashTable_free(&vm->strings);
    MFREE_LIST(vm->objects, Obj_free);
    MFREE(vm, sizeof(VM));
}
