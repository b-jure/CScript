#include "crbuffer.h"
#include "crerr.h"

#include <memory.h>


#define ARRAY_INITIAL_SIZE cast(cr_uint, 8)

#define GROW_ARRAY_CAPACITY(cap) ((cap) < ARRAY_INITIAL_SIZE ? ARRAY_INITIAL_SIZE : ((cap) * 2))

#define BUF_REALLOC(buf, oldcap, newcap) \
	(char *)gcrealloc(buf->vm, buf->ptr, oldcap * sizeof(char), newcap * sizeof(char))


static void buf_grow(Buffer *buf)
{
	cr_uint oldcap;

	oldcap = buf->cap;
	if (unlikely(buf->cap >= CR_BYTECODE_MAX))
		bclimiterror(buf->vm, "Buffer capacity limit");
	if (oldcap != 0 && !ispow2(oldcap)) {
		topow2(oldcap);
		oldcap >>= 1;
	}
	buf->cap = MIN(GROW_ARRAY_CAPACITY(oldcap), CR_BYTECODE_MAX);
	buf->ptr = BUF_REALLOC(buf, oldcap, buf->cap);
}

static void buf_grow_to(Buffer *buf, cr_uint cap)
{
	cr_uint oldcap;

	oldcap = buf->cap;
	if (cap != 0 && !ispow2(cap))
		topow2(cap);
	buf->ptr = BUF_REALLOC(buf, oldcap, cap);
	buf->cap = cap;
}

void buf_init(Buffer *buf, VM *vm)
{
	buf->cap = 0;
	buf->len = 0;
	buf->ptr = NULL;
	buf->vm = vm;
}

void buf_initcap(Buffer *buf, VM *vm, cr_uint cap)
{
	if (unlikely(cap >= BUFLIMIT))
		bclimiterror(vm, "'Buffer' capacity limit");
	buf_init(buf, vm);
	buf_grow_to(buf, cap);
}

void buf_ensure(Buffer *buf, cr_uint size)
{
	if (buf->cap < buf->len + size)
		buf_grow_to(buf, buf->len + size);
}

char *buf_at(Buffer *buf, cr_uint idx)
{
	return &buf->ptr[idx];
}

char *buf_last(Buffer *buf)
{
	return &buf->ptr[buf->len - 1];
}

char *buf_first(Buffer *buf)
{
	return buf->ptr;
}

cr_uint buf_push(Buffer *buf, char c)
{
	if (unlikely(buf->cap <= buf->len))
		buf_grow(buf);
	buf->ptr[buf->len] = c;
	return buf->len++;
}

char buf_pop(Buffer *buf)
{
	if (unlikely(buf->len <= 0))
		return '\0';
	return buf->ptr[--buf->len];
}

void buf_insert(Buffer *buf, cr_uint idx, char c)
{
	char *src;

	buf_ensure(buf, 1);
	src = buf->ptr + idx;
	memmove(src + 1, src, (buf->len - idx) * sizeof(char));
	buf->len++;
	buf->ptr[idx] = c;
}

char buf_remove(Buffer *buf, cr_uint idx)
{
	char *dest;
	char ret;

	if (buf->len == idx)
		buf_pop(buf);
	dest = buf->ptr + idx;
	ret = *dest;
	memmove(dest, dest + 1, (buf->len - idx - 1) * sizeof(char));
	buf->len--;
	return ret;
}

void buf_free(Buffer *buf)
{
	GC_FREE(buf->vm, buf->ptr, buf->cap);
}
