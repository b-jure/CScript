#include "core.h"
#include "err.h"
#include "object.h"

/**
 * Changes the garbage collector heap growth factor.
 * Smaller value means more frequent garbage collection.
 * If the value is '0' then the default gc growth factor will be used.
 * @err - if the value is neither '0' and bigger than '1',
 * @ret - returns 'true'.
 **/
snative(gcfactor)
{
    UNUSED(argc);
    Value factor = argv[0];
    if(unlikely(!IS_NUMBER(factor) || (AS_NUMBER(factor) <= 1 && AS_NUMBER(factor) != 0))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, GC_FACTOR_ARG_ERR));
        return false;
    }
    vm->config.gc_grow_factor = AS_NUMBER(factor);
    argv[-1]                  = TRUE_VAL;
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
snative(gcmode)
{
    UNUSED(argc);
    Value      mode = argv[0];
    OString* err  = NULL;

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
snative(gccollect)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL((double)gc(vm));
    return true;
}

/**
 * Returns memory left before next collection.
 * @ret - memory in bytes before next gc.
 **/
snative(gcleft)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL(((double)vm->gc_next - vm->gc_allocated));
    return true;
}

/**
 * Returns program current memory usage.
 * @ret - memory usage in bytes.
 **/
snative(gcusage)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL((double)vm->gc_allocated);
    return true;
}

/**
 * Returns memory limit on which the next garbage
 * collection will trigger.
 * @ret - return amount of memory in bytes
 *        at which next collection triggers.
 **/
snative(gcnext)
{
    UNUSED(argc);
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
snative(gcset)
{
    UNUSED(argc);
    Value      bytes = argv[0];
    OString* err   = NULL;
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

/**
 * Checks if the garbage collector is in automatic mode (running).
 * @ret - 'true' if the gc is in 'auto' mode (running),
 *        'false' otherwise (if the mode is 'manual').
 **/
snative(gcisauto)
{
    UNUSED(argc);
    argv[-1] = !GC_CHECK(vm, GC_MANUAL_BIT);
    return true;
}
