#include "core.h"
#include "err.h"
#include "object.h"
#include "value.h"

#include <time.h>

#define UNUSED(x) (void)(x)

//------------------------- NATIVE -------------------------//

/**
 * Determines processor time.
 * @ret - approximation of processor time used by the program in seconds.
 * @err - if the processor time used is not available or its value cannot
 *        be represented.
 **/
NATIVE(clock)
{
    clock_t time = clock();

    if(likely(time != -1)) {
        argv[-1] = NUMBER_VAL((double)time / CLOCKS_PER_SEC);
        return true;
    }

    argv[-1] = OBJ_VAL(ERR_NEW(vm, CLOCK_ERR));
    return false;
}

#define NATIVE_FIELD_ERR(argv, name)                                                     \
    do {                                                                                 \
        ObjString* err = NULL;                                                           \
        if(unlikely(!IS_INSTANCE(argv[0]))) {                                            \
            err = ERR_NEW(vm, name##_INSTANCE_ERR);                                      \
        } else if(unlikely(!IS_STRING(argv[1]))) {                                       \
            err = ERR_NEW(vm, name##_FIELD_ERR);                                         \
        }                                                                                \
        argv[-1] = OBJ_VAL(err);                                                         \
    } while(false)

/**
 * Checks if ObjInstance contains field.
 * @ret - bool, true if it contains, false otherwise
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
NATIVE(isfield)
{
    if(likely(IS_INSTANCE(argv[0]) && IS_STRING(argv[1]))) {
        ObjInstance* instance = AS_INSTANCE(argv[0]);
        Value        _dummy;
        argv[-1] = BOOL_VAL(HashTable_get(&instance->fields, argv[1], &_dummy));
        return true;
    }

    NATIVE_FIELD_ERR(argv, ISFIELD);
    return false;
}

/**
 * Deletes the field from ObjInstance.
 * @ret - bool, true if field was removed false otherwise
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
NATIVE(delfield)
{
    if(likely(IS_INSTANCE(argv[0]) && IS_STRING(argv[1]))) {
        ObjInstance* instance = AS_INSTANCE(argv[0]);
        argv[-1]              = BOOL_VAL(HashTable_remove(&instance->fields, argv[1]));
        return true;
    }

    NATIVE_FIELD_ERR(argv, DELFIELD);
    return false;
}

/**
 * Creates or sets the value the field of ObjInstance.
 * @ret - bool, true if field was created otherwise false (field value changed)
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
NATIVE(setfield)
{
    if(likely(IS_INSTANCE(argv[0]) && IS_STRING(argv[1]))) {
        ObjInstance* instance = AS_INSTANCE(argv[0]);
        argv[-1] =
            BOOL_VAL(HashTable_insert(vm, NULL, &instance->fields, argv[1], argv[2]));
        return true;
    }

    NATIVE_FIELD_ERR(argv, SETFIELD);
    return false;
}

/**
 * Prints a string and a newline.
 * @ret - returns 'true'.
 **/
NATIVE(printl)
{
    UNUSED(vm);
    Value_print(argv[0]);
    printf("\n");
    argv[-1] = TRUE_VAL;
    return true;
}

/**
 * Converts the value into string.
 * @ret - returns 'string' of the value.
 **/
NATIVE(tostr)
{
    argv[-1] = OBJ_VAL(VALSTR(vm, argv[0]));
    return true;
}


/**
 * Changes the garbage collector heap growth factor.
 * Smaller value means more frequent garbage collection.
 * If the value is '0' then the default gc growth factor will be used.
 * @err - if the value is neither '0' and bigger than '1',
 * @ret - returns 'true'.
 **/
NATIVE(gcfactor)
{
    Value factor = argv[0];
    if(unlikely(!IS_NUMBER(factor) || (AS_NUMBER(factor) <= 1 && AS_NUMBER(factor) != 0)))
    {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, GC_FACTOR_ARG_ERR));
        return false;
    }
    gc_grow_factor = AS_NUMBER(factor);
    argv[-1]       = TRUE_VAL;
    return true;
}


/**
 * Toggles garbage collection between 'auto' and 'manual'.
 * On 'manual' there will be no garbage collection and user
 * is responsible for invoking the collector using 'native_gccollect'.
 * On 'auto' garbage collection will proceed automatically as per
 * default.
 * @err - in case 'mode' argument is not a string or is a string
 *        but it is not 'auto' or 'manual' string.
 * @ret - returns 'true' if garbage collection is set as 'auto',
 *        otherwise 'false'.
 **/
NATIVE(gcmode)
{
    Value      mode = argv[0];
    ObjString* err  = NULL;

    if(unlikely(!IS_STRING(mode))) {
        err = ERR_NEW(vm, GC_MODE_ARG_ERR);
    } else if(unlikely(
                  AS_STRING(mode) != vm->statics[SS_MANU] &&
                  AS_STRING(mode) != vm->statics[SS_AUTO]))
    {
        err = ERR_NEW(vm, GC_MODE_INVALID_MODE_ERR);
    } else {
        goto fin;
    }

    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    bool manual = AS_STRING(mode) == vm->statics[SS_MANU];
    GC_TOGGLE(vm, GC_MANUAL_BIT, manual);
    argv[-1] = BOOL_VAL(!manual);
    return true;
}

/**
 * Invokes garbage collector and starts a collection.
 * It does mark and sweep collection.
 * @ret - returns number of bytes collected.
 **/
NATIVE(gccollect)
{
    argv[-1] = NUMBER_VAL((double)gc(vm, NULL));
    return true;
}

/**
 * Returns memory left before next collection.
 * @ret - memory in bytes before next gc.
 **/
NATIVE(gcleft)
{
    argv[-1] = NUMBER_VAL(((double)vm->gc_next - vm->gc_allocated));
    return true;
}

/**
 * Returns program current memory usage.
 * @ret - memory usage in bytes.
 **/
NATIVE(gcusage)
{
    argv[-1] = NUMBER_VAL((double)vm->gc_allocated);
    return true;
}

/**
 * Returns memory limit on which the next garbage
 * collection will trigger.
 * @ret - return amount of memory in bytes
 *        at which next collection triggers.
 **/
NATIVE(gcnext)
{
    argv[-1] = NUMBER_VAL((double)vm->gc_next);
    return true;
}

/**
 * Set the new collector limit in bytes, when the
 * memory limit is reached collection is triggered,
 * but only if the gcmode() is set to 'auto'.
 * @ret - returns the old limit in bytes
 * @err - if the limit is negative.
 **/
NATIVE(gcset)
{
    Value      bytes = argv[0];
    ObjString* err   = NULL;
    if(unlikely(!IS_NUMBER(bytes))) {
        err = ERR_NEW(vm, GC_SET_ARG_ERR);
    } else if(unlikely(AS_NUMBER(bytes) < 0)) {
        err = ERR_NEW(vm, GC_SET_NEGATIVE_LIMIT_ERR);
    } else {
        goto fin;
    }

    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    argv[-1]    = NUMBER_VAL(vm->gc_next);
    vm->gc_next = AS_NUMBER(bytes);
    return true;
}
