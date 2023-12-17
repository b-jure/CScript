#include "core.h"
#include "err.h"
#include "object.h"

#include <ctype.h>

/**
 * Converts the value into string.
 * @ret - returns 'string' of the value.
 **/
snative(tostr)
{
    vm->sp   -= argc;
    argv[-1]  = OBJ_VAL(vtostr(vm, argv[0]));
    pushn(vm, retcnt, NIL_VAL);
    return true;
}

/**
 * Checks if the 'Value' is a string.
 * @ret - returns 'true' if the value is a string type,
 *        otherwise 'false'.
 **/
snative(isstr)
{
    vm->sp   -= argc;
    argv[-1]  = BOOL_VAL(IS_STRING(argv[0]));
    pushn(vm, retcnt, NIL_VAL);
    return true;
}

/**
 * Returns the length of the string.
 * @err - if the value is not a string error is invoked,
 *        otherwise return string length (in bytes).
 **/
snative(strlen)
{
    Value string  = argv[0];
    vm->sp       -= argc;
    if(unlikely(!IS_STRING(string))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRLEN_FIRST_ARG_TYPE_ERR));
        return false;
    }
    argv[-1] = NUMBER_VAL(AS_STRING(string)->len);
    pushn(vm, retcnt, NIL_VAL);
    return true;
}

/**
 * Looks for the first match of the 'pattern' in the string.
 * @ret - if it find a match it returns the index of where the
          pattern starts in the string (starting from 0), if
          pattern was not found it returns 'nil'.
          Invokes error if 'haystack' or a 'needle' (pattern) is not a string.
 **/
snative(strpat)
{
    vm->sp           -= argc;
    Value    string   = argv[0];
    Value    pattern  = argv[1];
    OString* err      = NULL;
    if(unlikely(!IS_STRING(string))) err = ERR_NEW(vm, STRPAT_FIRST_ARG_TYPE_ERR);
    else if(unlikely(!IS_STRING(pattern))) err = ERR_NEW(vm, STRPAT_SECOND_ARG_TYPE_ERR);
    else goto fin;
    argv[-1] = OBJ_VAL(err);
    return false;
fin:;
    OString* haystack = AS_STRING(string);
    OString* needle   = AS_STRING(pattern);
    char*    start    = strstr(haystack->storage, needle->storage);
    argv[-1]          = start == NULL ? NIL_VAL : NUMBER_VAL(start - haystack->storage);
    pushn(vm, retcnt, NIL_VAL);
    return true;
}

/**
 * Returns a substring of 'string', starting from index 'i' ending
 * at index 'j'. Both 'i' and 'j' can be negative which means they
 * are indexing from the end of the string (in reverse).
 * If 'i' is less than 0, it is corrected to the reverse index.
 * If 'j' is higher than the 'string' length it is corrected to
 * the 'string' length ('string' length is actually - 1 because of index counting).
 * If after corrections 'i' is higher than 'j' empty string is returned.
 * @ret - substring of 'string' spanning from 'i' to 'j'.
 * @err - if first argument is not 'string' or both 'i' and 'j'
 *        are not numbers.
 **/
snative(strsub)
{
    vm->sp          -= argc;
    Value    string  = argv[0];
    Value    i       = argv[1];
    Value    j       = argv[2];
    OString* err     = NULL;
    if(unlikely(!IS_STRING(string))) err = ERR_NEW(vm, STRSUB_FIRST_ARG_TYPE_ERR);
    else if(unlikely(
                (!IS_NUMBER(i) || !IS_NUMBER(j)) &&
                (sfloor(AS_NUMBER(i)) != AS_NUMBER(i) ||
                 sfloor(AS_NUMBER(j)) != AS_NUMBER(j))))
        err = ERR_NEW(vm, STRSUB_INDICES_TYPE_ERR);
    else goto fin;
    argv[-1] = OBJ_VAL(err);
    return false;
fin:;
    OString* substr = AS_STRING(string);
    int64_t  ii     = AS_NUMBER(i);
    int64_t  ij     = AS_NUMBER(j);
    int64_t  len    = substr->len + 1;
    if(ii < 0) ii = len + ii < 0 ? 0 : len + ii;
    if(ij < 0) ij = len + ij < 0 ? 0 : len + ij;
    if(ij > len) ij = len;
    if(ii > ij) argv[-1] = OBJ_VAL(OString_from(vm, "", 0));
    else argv[-1] = OBJ_VAL(OString_from(vm, substr->storage + ii, ij - ii));
    return true;
}

snative(strbyte)
{
    UNUSED(argc);
    Value value = argv[0];
    Value index = argv[1];

    OString* err = NULL;

    if(unlikely(!IS_STRING(value))) {
        err = ERR_NEW(vm, STRBYTE_FIRST_ARG_TYPE_ERR);
    } else if(unlikely(!IS_NUMBER(index) || sfloor(AS_NUMBER(index)) != AS_NUMBER(index)))
    {
        err = ERR_NEW(vm, STRBYTE_SECOND_ARG_TYPE_ERR);
    } else {
        goto fin;
    }
    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    OString* string = AS_STRING(value);
    Int      slen   = string->len;
    Int      idx    = AS_NUMBER(index);

    if(idx < 0 || idx > slen - 1) { // Index out of range
        argv[-1] = NIL_VAL;
    } else {
        argv[-1] = NUMBER_VAL(string->storage[idx]);
    }
    return true;
}

sstatic force_inline OString*
changecase(VM* vm, OString* string, int (*changecasefn)(int))
{
    UInt        slen = string->len;
    const char* str  = string->storage;
    char        buffer[slen];

    UInt i = 0;
    while(i < slen) {
        buffer[i++] = changecasefn(*str++);
    }
    buffer[i] = '\0';

    return OString_from(vm, buffer, slen);
}

snative(strlower)
{
    UNUSED(argc);

    Value value = argv[0];

    if(unlikely(!IS_STRING(value))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRLOWER_ARG_ERR));
        return false;
    }

    argv[-1] = OBJ_VAL(changecase(vm, AS_STRING(value), tolower));
    return true;
}

snative(strupper)
{
    UNUSED(argc);

    Value value = argv[0];

    if(unlikely(!IS_STRING(value))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRUPPER_ARG_ERR));
        return false;
    }

    argv[-1] = OBJ_VAL(changecase(vm, AS_STRING(value), toupper));
    return true;
}

sstatic force_inline OString* revstring(VM* vm, OString* string)
{
    char buffer[string->len];
    UInt slen = string->len - 1;
    UInt i    = 0;
    while(i <= slen) {
        buffer[i] = string->storage[slen - i];
        i++;
    }
    buffer[i] = '\0';
    return OString_from(vm, buffer, slen + 1);
}

snative(strrev)
{
    UNUSED(argc);

    Value value = argv[0];

    if(unlikely(!IS_STRING(value))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRREV_ARG_ERR));
        return false;
    }
    argv[-1] = OBJ_VAL(revstring(vm, AS_STRING(value)));
    return true;
}

sstatic force_inline OString* concatstring(VM* vm, OString* left, OString* right)
{
    size_t len = left->len + right->len;
    char   buffer[len + 1];
    memcpy(buffer, left->storage, left->len);
    memcpy(buffer + left->len, right->storage, right->len);
    buffer[len] = '\0';
    return OString_from(vm, buffer, len);
}

snative(strconcat)
{
    UNUSED(argc);

    Value left  = argv[0];
    Value right = argv[1];

    OString* err = NULL;

    if(unlikely(!IS_STRING(left))) {
        err = ERR_NEW(vm, STRCONCAT_FIRST_ARG_TYPE_ERR);
    } else if(unlikely(!IS_STRING(right))) {
        err = ERR_NEW(vm, STRCONCAT_SECOND_ARG_TYPE_ERR);
    } else {
        goto fin;
    }
    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    argv[-1] = OBJ_VAL(concatstring(vm, AS_STRING(left), AS_STRING(right)));
    return true;
}

snative(byte)
{
    UNUSED(argc);
    Value string = argv[0];
    if(unlikely(!IS_STRING(string))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, BYTE_ARG_ERR));
        return false;
    }
    argv[-1] = NUMBER_VAL(AS_STRING(string)->storage[0]);
    return true;
}
