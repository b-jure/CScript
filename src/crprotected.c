#include "crprotected.h"
#include "crparser.h"
#include "crobject.h"
#include "crfunction.h"
#include "crstate.h"


/* -------------------------------------------------------------------------
 * Protected call
 * ------------------------------------------------------------------------- */

/* data for 'pcall' */



/* -------------------------------------------------------------------------
 * Protected parser
 * ------------------------------------------------------------------------- */

/* data for 'pparse' */
typedef struct PPData {
    BuffReader br;
    Buffer buff;
    ParserState ps;
    const char *source;
} PPData;


/* protected 'parse' */
static void pparse(cr_State *ts, void *userdata)
{
    PPData *parsedata = cast(PPData *, userdata);
    CrClosure *cl = crP_parse(ts, &parsedata->br, &parsedata->buff, &parsedata->ps,
                          parsedata->source);
    cr_assert(cl->nupvals <= cl->sizeupvals);
    crF_initupvals(ts, cl);
}


/* external interface for 'pparse' */
void crP_pparse(cr_State *ts, cr_fReader freader, void *userdata,
                      const char *name)
{
    PPData parsedata;
    ParserState *ps = &parsedata.ps;
    crR_init(ts, &parsedata.br, freader, userdata); /* 'br' */
    crR_buffinit(&parsedata.buff); /* 'buff' */
    { ps->lvars.len = ps->lvars.size = 0; ps->lvars.arr = NULL; } /* 'lvars' */
    ps->cs = NULL; /* 'cs' */
    parsedata.source = name; /* 'source' */
    crPr_call(ts, pparse, &parsedata, savestack(ts, ts->sp.p));
}
