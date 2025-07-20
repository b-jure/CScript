/*
** treglib.c
** Standard library for pattern-matching
** See Copyright Notice in tokudae.h
*/

#define treglib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include <stdio.h>
#include <ttype.h>
#include <stddef.h>

#include "tokudae.h"

#include "tstrlib.h"
#include "tokudaeaux.h"
#include "tokudaelib.h"


/*
** Maximum number of captures that a pattern can do during
** pattern-matching. This limit is arbitrary, but must fit in
** an unsigned char.
*/
#if !defined(TOKU_MAXCAPTURES)
#define TOKU_MAXCAPTURES		32
#endif


#define CAP_UNFINISHED	(-1)
#define CAP_POSITION	(-2)


typedef struct MatchState {
    const char *srt_init; /* init of source string */
    const char *srt_end; /* end ('\0') of source string */
    const char *p_end; /* end ('\0') of pattern */
    toku_State *T;
    int matchdepth; /* control for recursive depth (to avoid C stack overflow) */
    unsigned char level; /* total number of captures (finished or unfinished) */
    struct {
        const char *init;
        ptrdiff_t len;
    } capture[TOKU_MAXCAPTURES];
} MatchState;


/* recursive function */
static const char *match(MatchState *ms, const char *s, const char *p);


/* maximum recursion depth for 'match' */
#if !defined(MAXCCALLS)
#define MAXCCALLS	200
#endif


#define T_ESC		'%'
#define SPECIALS	"^$*+?.([%-"


static int check_capture(MatchState *ms, int l) {
    l -= '1';
    if (t_unlikely(l < 0 || l >= ms->level ||
                   ms->capture[l].len == CAP_UNFINISHED))
        return tokuL_error(ms->C, "invalid capture index %%%d", l + 1);
    return l;
}


static int capture_to_close(MatchState *ms) {
    int level = ms->level;
    for (level--; level>=0; level--)
        if (ms->capture[level].len == CAP_UNFINISHED) return level;
    return tokuL_error(ms->C, "invalid pattern capture");
}


static const char *class_end(MatchState *ms, const char *p) {
    switch (*p++) {
        case T_ESC: {
            if (t_unlikely(p == ms->p_end))
                tokuL_error(ms->C, "malformed pattern (ends with '%%')");
            return p+1;
        }
        case '[': {
            if (*p == '^') p++;
            do { /* look for a ']' */
                if (t_unlikely(p == ms->p_end))
                    tokuL_error(ms->C, "malformed pattern (missing ']')");
                if (*(p++) == T_ESC && p < ms->p_end)
                    p++; /* skip escapes (e.g. '%]') */
            } while (*p != ']');
            return p+1;
        }
        default: {
            return p;
        }
    }
}


static int match_class(int c, int cl) {
    int res;
    switch (tolower(cl)) {
        case 'a' : res = isalpha(c); break;
        case 'c' : res = iscntrl(c); break;
        case 'd' : res = isdigit(c); break;
        case 'g' : res = isgraph(c); break;
        case 'l' : res = islower(c); break;
        case 'p' : res = ispunct(c); break;
        case 's' : res = isspace(c); break;
        case 'u' : res = isupper(c); break;
        case 'w' : res = isalnum(c); break;
        case 'x' : res = isxdigit(c); break;
        default: return (cl == c);
    }
    return (islower(cl) ? res : !res);
}


static int match_bracket_class(int c, const char *p, const char *ec) {
    int sig = 1;
    if (*(p+1) == '^') {
        sig = 0;
        p++; /* skip the '^' */
    }
    while (++p < ec) {
        if (*p == T_ESC) {
            p++;
            if (match_class(c, uchar(*p)))
                return sig;
        }
        else if ((*(p+1) == '-') && (p+2 < ec)) {
            p+=2;
            if (uchar(*(p-2)) <= c && c <= uchar(*p))
                return sig;
        }
        else if (uchar(*p) == c) return sig;
    }
    return !sig;
}


static int single_match(MatchState *ms, const char *s, const char *p,
                        const char *ep) {
    if (s >= ms->srt_end)
        return 0;
    else {
        int c = uchar(*s);
        switch (*p) {
            case '.': return 1; /* matches any char */
            case T_ESC: return match_class(c, uchar(*(p+1)));
            case '[': return match_bracket_class(c, p, ep-1);
            default:  return (uchar(*p) == c);
        }
    }
}


static const char *match_balance(MatchState *ms, const char *s,
                                const char *p) {
    if (t_unlikely(p >= ms->p_end - 1))
        tokuL_error(ms->C, "malformed pattern (missing arguments to '%%b')");
    if (*s != *p)
        return NULL;
    else {
        int b = *p;
        int e = *(p+1);
        int cont = 1;
        while (++s < ms->srt_end) {
            if (*s == e) {
                if (--cont == 0)
                    return s+1;
            }
            else if (*s == b) cont++;
        }
    }
    return NULL; /* string ends out of balance */
}


static const char *max_expand(MatchState *ms, const char *s,
                              const char *p, const char *ep) {
    ptrdiff_t i = 0; /* counts maximum expand for item */
    while (single_match(ms, s + i, p, ep)) i++;
    /* keeps trying to match with the maximum repetitions */
    while (i>=0) {
        const char *res = match(ms, (s+i), ep+1);
        if (res) return res;
        i--; /* else didn't match; reduce 1 repetition to try again */
    }
    return NULL;
}


static const char *min_expand(MatchState *ms, const char *s,
                              const char *p, const char *ep) {
    for (;;) {
        const char *res = match(ms, s, ep+1);
        if (res != NULL)
            return res;
        else if (single_match(ms, s, p, ep))
            s++; /* try with one more repetition */
        else return NULL;
    }
}


static const char *start_capture(MatchState *ms, const char *s,
                                 const char *p, int what) {
    const char *res;
    int level = ms->level;
    if (level >= TOKU_MAXCAPTURES)
        tokuL_error(ms->C, "too many captures");
    ms->capture[level].init = s;
    ms->capture[level].len = what;
    ms->level = level+1;
    if ((res=match(ms, s, p)) == NULL) /* match failed? */
        ms->level--; /* undo capture */
    return res;
}


static const char *end_capture(MatchState *ms, const char *s, const char *p) {
    int l = capture_to_close(ms);
    const char *res;
    ms->capture[l].len = s - ms->capture[l].init; /* close capture */
    if ((res = match(ms, s, p)) == NULL) /* match failed? */
        ms->capture[l].len = CAP_UNFINISHED; /* undo capture */
    return res;
}


static const char *match_capture(MatchState *ms, const char *s, int l) {
    size_t len;
    l = check_capture(ms, l);
    len = ms->capture[l].len;
    if ((size_t)(ms->srt_end-s) >= len &&
            memcmp(ms->capture[l].init, s, len) == 0)
        return s+len;
    else
        return NULL;
}


static const char *match(MatchState *ms, const char *s, const char *p) {
    if (t_unlikely(ms->matchdepth-- == 0))
        tokuL_error(ms->C, "pattern too complex");
init: /* using goto to optimize tail recursion */
    if (p != ms->p_end) { /* not end of pattern? */
        switch (*p) {
            case '(': { /* start capture */
                if (*(p + 1) == ')') /* position capture? */
                    s = start_capture(ms, s, p + 2, CAP_POSITION);
                else
                    s = start_capture(ms, s, p + 1, CAP_UNFINISHED);
                break;
            }
            case ')': { /* end capture */
                s = end_capture(ms, s, p + 1);
                break;
            }
            case '$': {
                if ((p + 1) != ms->p_end) /* is the '$' the last char in p? */
                    goto dflt; /* no; go to default */
                s = (s == ms->srt_end) ? s : NULL; /* check end of string */
                break;
            }
            case T_ESC: { /* escaped seq. not in the format class[*+?-]? */
                switch (*(p + 1)) {
                    case 'b': { /* balanced string? */
                        s = match_balance(ms, s, p + 2);
                        if (s != NULL) {
                            p += 4; goto init; /* return match(ms, s, p+4); */
                        } /* else fail (s == NULL) */
                        break;
                    }
                    case 'f': { /* frontier? */
                        const char *ep; char previous;
                        p += 2;
                        if (t_unlikely(*p != '['))
                            tokuL_error(ms->C, "missing '[' after '%%f' in pattern");
                        ep = class_end(ms, p); /* points to what is next */
                        previous = (s == ms->srt_init) ? '\0' : *(s - 1);
                        if (!match_bracket_class(uchar(previous), p, ep - 1) &&
                                match_bracket_class(uchar(*s), p, ep - 1)) {
                            p = ep; goto init; /* return match(ms, s, ep); */
                        }
                        s = NULL; /* match failed */
                        break;
                    }
                    case '0': case '1': case '2': case '3':
                    case '4': case '5': case '6': case '7':
                    case '8': case '9': { /* capture results (%0-%9)? */
                        s = match_capture(ms, s, uchar(*(p + 1)));
                        if (s != NULL) {
                            p += 2; goto init; /* return match(ms, s, p+2) */
                        }
                        break;
                    }
                    default: goto dflt;
                }
                break;
            }
            default: dflt: { /* pattern class plus optional suffix */
                const char *ep = class_end(ms, p); /* points to opt. suffix */
                /* does not match at least once? */
                if (!single_match(ms, s, p, ep)) {
                    if (*ep == '*' || *ep == '?' || *ep == '-') {
                        /* accept empty */
                        p = ep + 1; goto init; /* return match(ms, s, ep+1); */
                    } else /* '+' or no suffix */
                        s = NULL; /* fail */
                } else { /* matched once */
                    switch (*ep) { /* handle optional suffix */
                        case '?': { /* optional */
                            const char *res;
                            if ((res = match(ms, s + 1, ep + 1)) != NULL)
                                s = res;
                            else {
                                p = ep + 1;
                                goto init; /* return match(ms, s, ep+1); */
                            }
                            break;
                        }
                        case '+': /* 1 or more repetitions */
                            s++; /* 1 match already done */
                            /* fall through */
                        case '*': /* 0 or more repetitions */
                            s = max_expand(ms, s, p, ep);
                            break;
                        case '-': /* 0 or more repetitions (minimum) */
                            s = min_expand(ms, s, p, ep);
                            break;
                        default: /* no suffix */
                            s++; p = ep;
                            goto init; /* return match(ms, s+1, ep); */
                    }
                }
                break;
            }
        }
    }
    ms->matchdepth++;
    return s;
}


/*
** get information about the i-th capture. If there are no captures
** and 'i==0', return information about the whole match, which
** is the range 's'..'e'. If the capture is a string, return
** its length and put its address in '*cap'. If it is an integer
** (a position), push it on the stack and return CAP_POSITION.
*/
static size_t get_onecapture(MatchState *ms, int i, const char *s,
                             const char *e, const char **cap) {
    if (i >= ms->level) {
        if (t_unlikely(i != 0))
            tokuL_error(ms->C, "invalid capture index %%%d", i + 1);
        *cap = s;
        return e - s;
    } else {
        ptrdiff_t capl = ms->capture[i].len;
        *cap = ms->capture[i].init;
        if (t_unlikely(capl == CAP_UNFINISHED))
            tokuL_error(ms->C, "unfinished capture");
        else if (capl == CAP_POSITION)
            toku_push_integer(ms->C, ms->capture[i].init - ms->srt_init);
        return capl;
    }
}


/*
** Push the i-th capture on the stack.
*/
static void push_onecapture(MatchState *ms, int i, const char *s,
                                                   const char *e) {
    const char *cap;
    ptrdiff_t l = get_onecapture(ms, i, s, e, &cap);
    if (l != CAP_POSITION)
        toku_push_lstring(ms->C, cap, l);
    /* else position was already pushed */
}


static int push_captures(MatchState *ms, const char *s, const char *e) {
    int i;
    int nlevels = (ms->level == 0 && s) ? 1 : ms->level;
    tokuL_check_stack(ms->C, nlevels, "too many captures");
    for (i = 0; i < nlevels; i++)
        push_onecapture(ms, i, s, e);
    return nlevels; /* number of strings pushed */
}


/* check whether pattern has no special characters */
static int nospecials(const char *p, size_t l) {
    size_t upto = 0;
    do {
        if (strpbrk(p + upto, SPECIALS))
            return 0; /* pattern has a special character */
        upto += strlen(p + upto) + 1; /* may have more after \0 */
    } while (upto <= l);
    return 1; /* no special chars found */
}


static void prep_state(MatchState *ms, toku_State *T,
                      const char *s, size_t ls, const char *p, size_t lp) {
    ms->C = C;
    ms->matchdepth = MAXCCALLS;
    ms->srt_init = s;
    ms->srt_end = s + ls;
    ms->p_end = p + lp;
}


static void re_prep_state(MatchState *ms) {
    ms->level = 0;
    toku_assert(ms->matchdepth == MAXCCALLS);
}


static int find_aux(toku_State *T, int find) {
    size_t ls, lp;
    const char *s = tokuL_check_lstring(C, 0, &ls);
    const char *p = tokuL_check_lstring(C, 1, &lp);
    size_t init = posrelStart(tokuL_opt_integer(C, 2, 0), ls);
    if (init > ls || (ls != 0 && init == ls)) { /* start after string's end? */
        tokuL_push_fail(C); /* cannot find anything */
        return 1;
    }
    /* explicit request or no special characters? */
    if (find && (toku_to_bool(C, 3) || nospecials(p, lp))) {
        /* do a plain search */
        const char *s2 = findstr(s + init, ls - init, p, lp, 0);
        if (s2) {
            toku_push_integer(C, (s2 - s));
            toku_push_integer(C, (s2 - s) + lp - 1);
            return 2;
        }
    } else {
        MatchState ms;
        const char *s1 = s + init;
        int anchor = (*p == '^');
        if (anchor) {
            p++; lp--; /* skip anchor character */
        }
        prep_state(&ms, C, s, ls, p, lp);
        do {
            const char *res;
            re_prep_state(&ms);
            if ((res=match(&ms, s1, p)) != NULL) {
                if (find) {
                    toku_push_integer(C, s1 - s); /* start */
                    toku_push_integer(C, (res - s) - 1); /* end */
                    return push_captures(&ms, NULL, 0) + 2;
                } else
                    return push_captures(&ms, s1, res);
            }
        } while (s1++ < ms.srt_end && !anchor);
    }
    tokuL_push_fail(C); /* not found */
    return 1;
}


static int reg_find(toku_State *T) {
    return find_aux(C, 1);
}


static int reg_match(toku_State *T) {
    return find_aux(C, 0);
}


/* state for 'reg_gmatch' */
typedef struct GMatchState {
    const char *src; /* current position */
    const char *p; /* pattern */
    const char *lastmatch; /* end of last match */
    MatchState ms; /* match state */
} GMatchState;


static int gmatch_aux(toku_State *T) {
    GMatchState *gm = (GMatchState *)toku_to_userdata(C, toku_upvalueindex(2));
    const char *src;
    gm->ms.C = C;
    for (src = gm->src; src <= gm->ms.srt_end; src++) {
        const char *e;
        re_prep_state(&gm->ms);
        if ((e = match(&gm->ms, src, gm->p)) != NULL && e != gm->lastmatch) {
            gm->src = gm->lastmatch = e;
            return push_captures(&gm->ms, src, e);
        }
    }
    return 0;  /* not found */
}


static int reg_gmatch(toku_State *T) {
    size_t ls, lp;
    const char *s = tokuL_check_lstring(C, 0, &ls);
    const char *p = tokuL_check_lstring(C, 1, &lp);
    size_t init = posrelStart(tokuL_opt_integer(C, 2, 0), ls);
    GMatchState *gm;
    toku_setntop(C, 2); /* keep strings on closure to avoid being collected */
    gm = (GMatchState *)toku_push_userdata(C, sizeof(GMatchState), 0);
    if (init > ls) /* start after string's end? */
        init = ls + 1; /* avoid overflows in 's + init' */
    prep_state(&gm->ms, C, s, ls, p, lp);
    gm->src = s + init; gm->p = p; gm->lastmatch = NULL;
    toku_push_cclosure(C, gmatch_aux, 3);
    return 1;
}


static void add_s(MatchState *ms, tokuL_Buffer *b, const char *s,
                                                 const char *e) {
    size_t l;
    toku_State *T = ms->C;
    const char *news = toku_to_lstring(C, 2, &l);
    const char *p;
    while ((p = cast_charp(memchr(news, T_ESC, l))) != NULL) {
        tokuL_buff_push_lstring(b, news, cast_sizet(p - news));
        p++; /* skip T_ESC */
        if (*p == T_ESC) /* '%%' */
            tokuL_buff_push(b, *p);
        else if (*p == '0') /* '%0' */
            tokuL_buff_push_lstring(b, s, cast_sizet(e - s));
        else if (isdigit(uchar(*p))) { /* '%n' */
            const char *cap;
            ptrdiff_t resl = get_onecapture(ms, *p - '1', s, e, &cap);
            if (resl == CAP_POSITION) {
                tokuL_to_lstring(C, -1, NULL); /* conv. position to string */
                toku_remove(C, -2); /* remove position */
                tokuL_buff_push_stack(b); /* add pos. to accumulated result */
            } else
                tokuL_buff_push_lstring(b, cap, cast_sizet(resl));
        } else
            tokuL_error(C, "invalid use of '%c' in replacement string", T_ESC);
        l -= p + 1 - news;
        news = p + 1;
    }
    tokuL_buff_push_lstring(b, news, l);
}


/*
** Add the replacement value to the string buffer 'b'.
** Return true if the original string was changed. (Function calls and
** table indexing resulting in nil or false do not change the subject.)
*/
static int add_value(MatchState *ms, tokuL_Buffer *b, const char *s,
                                     const char *e, int tr) {
    toku_State *T = ms->C;
    switch (tr) {
        case TOKU_T_FUNCTION: { /* call the function */
            int n;
            toku_push(C, 2); /* push the function */
            n = push_captures(ms, s, e); /* all captures as arguments */
            toku_call(C, n, 1); /* call it */
            break;
        }
        case TOKU_T_LIST: case TOKU_T_INSTANCE: case TOKU_T_TABLE: {
            /* index the list/instance/table */
            push_onecapture(ms, 0, s, e); /* first capture is the index */
            toku_get(C, 2);
            break;
        }
        default: { /* TOKU_T_STRING */
            add_s(ms, b, s, e); /* add value to the buffer */
            return 1; /* something changed */
        }
    }
    if (!toku_to_bool(C, -1)) { /* nil or false? */
        toku_pop(C, 1); /* remove value */
        tokuL_buff_push_lstring(b, s, cast_sizet(e-s)); /* keep original text */
        return 0; /* no changes */
    } else if (t_unlikely(!toku_is_string(C, -1))) {
        return tokuL_error(C, "invalid replacement value (a %s)",
                            tokuL_typename(C, -1));
    } else {
        tokuL_buff_push_stack(b); /* add result to accumulator */
        return 1; /* something changed */
    }
}


static int reg_gsub(toku_State *T) {
    size_t srcl, lp;
    const char *src = tokuL_check_lstring(C, 0, &srcl); /* subject */
    const char *p = tokuL_check_lstring(C, 1, &lp); /* pattern */
    const char *lastmatch = NULL; /* end of last match */
    int tr = toku_type(C, 2); /* replacement type */
    toku_Integer max_s = tokuL_opt_integer(C, 3, (toku_Integer)srcl + 1);
    int anchor = (*p == '^');
    toku_Integer n = 0; /* replacement count */
    int changed = 0; /* change flag */
    MatchState ms;
    tokuL_Buffer b;
    tokuL_expect_arg(C, tr == TOKU_T_STRING || tr == TOKU_T_FUNCTION ||
                      tr == TOKU_T_TABLE ||  tr == TOKU_T_INSTANCE ||
                      tr == TOKU_T_LIST, 2, "string/function/table/instance/list");
    tokuL_buff_init(C, &b);
    if (anchor)
        p++, lp--; /* skip anchor character */
    prep_state(&ms, C, src, srcl, p, lp);
    while (n < max_s) {
        const char *e;
        re_prep_state(&ms); /* (re)prepare state for new match */
        if ((e = match(&ms, src, p)) != NULL && e != lastmatch) { /* match? */
            n++;
            changed = add_value(&ms, &b, src, e, tr) | changed;
            src = lastmatch = e;
        } else if (src < ms.srt_end) /* otherwise, skip one character */
            tokuL_buff_push(&b, *src++);
        else break; /* end of subject */
        if (anchor) break;
    }
    if (!changed) /* no changes? */
        toku_push(C, 0); /* return original string */
    else { /* something changed */
        tokuL_buff_push_lstring(&b, src, cast_sizet(ms.srt_end-src));
        tokuL_buff_end(&b); /* create and return new string */
    }
    toku_push_integer(C, n); /* number of substitutions */
    return 2;
}


// TODO: add docs
static const tokuL_Entry reglib[] = {
    {"find", reg_find},
    {"match", reg_match},
    {"gmatch", reg_gmatch},
    {"gsub", reg_gsub},
    {NULL, NULL}
};


CSMOD_API int tokuopen_reg(toku_State *T) {
    tokuL_push_lib(C, reglib);
    return 1;
}
