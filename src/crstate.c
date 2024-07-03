#include "crstate.h"
#include "crgc.h"


static void preinitstate(cr_State *ts)
{

}


CR_API cr_State *cr_newstate(cr_alloc allocator, void *ud)
{
	GState *gs;
	cr_State *ts;

	gs->realloc = allocator;
	cr_gc_init(&gs->gc);
	preinitstate(ts);
	return ts;
}
