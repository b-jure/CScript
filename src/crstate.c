#include "crstate.h"
#include "crgc.h"


static void preinitstate(TState *ts)
{

}


CR_API TState *cr_newstate(cr_alloc allocator, void *ud)
{
	GState *gs;
	TState *ts;

	gs->realloc = allocator;
	cr_gc_init(&gs->gc);
	preinitstate(ts);
	return ts;
}
