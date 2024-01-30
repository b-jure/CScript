#ifndef SKOOMA_ERR_H
#define SKOOMA_ERR_H

#include "skooma.h"
#include "value.h"


/* ==================== runtime errors ====================== */

/* Lower-level error invocation, skips the part where
 * we check if the function is protected and instead
 * prints the stack trace and invokes panic handler. */
sk_noret printandpanic(VM* vm);

/* Generic runtime error */
sk_noret runerror(VM* vm, int8_t status);

/* Memory allocation error */
sk_noret memerror(VM* vm);

/* Ordering error */
sk_noret ordererror(VM* vm, Value a, Value b);

/* Binary/Unary arithmetic operation error */
#define operror(vm, l, r, op)                                                                      \
    (arisbin(op) ? binoperror(vm, l, r, cast(sk_om, op)) : unoperror(vm, l, cast(sk_om, op)))

/* Binary arithmetic operation error */
sk_noret binoperror(VM* vm, Value a, Value b, sk_om op);

/* Unary arithmetic operation error */
sk_noret unoperror(VM* vm, Value a, sk_om op);

/* Display method (__display__) error */
sk_noret disperror(VM* vm, Value result);

/* Object string format error */
sk_noret ofmterror(VM* vm, int8_t c, Value callee);

/* Stack overflow error */
sk_noret sovferror(VM* vm);

/* Undefined property error */
sk_noret udperror(VM* vm, Value property, OClass* oclass);

/* Return count stack overflow */
sk_noret retovferror(VM* vm, const char* fn);

/* Function invalid argument count error */
sk_noret arityerror(VM* vm, int32_t expected, int32_t got);

/* Call stack overflow (frame count) */
sk_noret fcovferror(VM* vm);

/* Called non-callable value */
sk_noret callerror(VM* vm, Value callee);

/* Invalid property access error */
sk_noret ipaerror(VM* vm, Value notinstance);

/* Global variable redefinition error */
sk_noret redefgerror(VM* vm, const char* gname);

/* Undefined global variable error */
sk_noret udgerror(VM* vm, const char* gname);

/* Assigning to variable defined as 'fixed' error */
sk_noret fixederror(VM* vm, const char* var);

/* 'nil' index error */
sk_noret nilidxerror(VM* vm);

/* Inheritance error */
sk_noret inheriterror(VM* vm, Value notclass);

/* ---------------------------------------------------------- */ // runtime errors


#endif
