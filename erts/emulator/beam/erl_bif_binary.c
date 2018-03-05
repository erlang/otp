/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * NOTE: This file contains the BIF's for the *module* binary in stdlib.
 * other BIF's concerning binaries are in binary.c.
 */


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERL_WANT_HIPE_BIF_WRAPPER__
#include "bif.h"
#undef ERL_WANT_HIPE_BIF_WRAPPER__
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_bif_unique.h"


/*
 * The native implementation functions for the module binary.
 * Searching is implemented using either Boyer-Moore or Aho-Corasick
 * depending on number of searchstrings (BM if one, AC if more than one).
 * Native implementation is mostly for efficiency, nothing
 * (except binary:referenced_byte_size) really *needs* to be implemented
 * in native code.
 */

/* #define HARDDEBUG */

/* Init and local variables */

static Export binary_find_trap_export;
static BIF_RETTYPE binary_find_trap(BIF_ALIST_3);
static Export binary_longest_prefix_trap_export;
static BIF_RETTYPE binary_longest_prefix_trap(BIF_ALIST_3);
static Export binary_longest_suffix_trap_export;
static BIF_RETTYPE binary_longest_suffix_trap(BIF_ALIST_3);
static Export binary_copy_trap_export;
static BIF_RETTYPE binary_copy_trap(BIF_ALIST_2);
static Uint max_loop_limit;

static BIF_RETTYPE
binary_match(Process *p, Eterm arg1, Eterm arg2, Eterm arg3, Uint flags);
static BIF_RETTYPE
binary_split(Process *p, Eterm arg1, Eterm arg2, Eterm arg3);

void erts_init_bif_binary(void)
{
    erts_init_trap_export(&binary_find_trap_export,
			  am_erlang, am_binary_find_trap, 3,
			  &binary_find_trap);

    erts_init_trap_export(&binary_longest_prefix_trap_export,
			  am_erlang, am_binary_longest_prefix_trap, 3,
			  &binary_longest_prefix_trap);

    erts_init_trap_export(&binary_longest_suffix_trap_export,
			  am_erlang, am_binary_longest_suffix_trap, 3,
			  &binary_longest_suffix_trap);

    erts_init_trap_export(&binary_copy_trap_export,
			  am_erlang, am_binary_copy_trap, 2,
			  &binary_copy_trap);

    max_loop_limit = 0;
    return;
}

/*
 * Setting the loop_limit for searches for debugging
 */
Sint erts_binary_set_loop_limit(Sint limit)
{
    Sint save = (Sint) max_loop_limit;
    if (limit <= 0) {
	max_loop_limit = 0;
    } else {
	max_loop_limit = (Uint) limit;
    }

    return save;
}

static Uint get_reds(Process *p, int loop_factor)
{
    Uint reds = ERTS_BIF_REDS_LEFT(p) * loop_factor;
    Uint tmp = max_loop_limit;
    if (tmp != 0 && tmp < reds) {
	return tmp;
    }
    if (!reds) {
	reds = 1;
    }
    return reds;
}

/*
 * A micro allocator used when building search structures, just a convenience
 * for building structures inside a pre-allocated magic binary using
 * conventional malloc-like interface.
 */

#define MYALIGN(Size) (SIZEOF_VOID_P * (((Size) / SIZEOF_VOID_P) + \
                       !!(((Size) % SIZEOF_VOID_P))))

#ifdef DEBUG
#define CHECK_ALLOCATOR(My) ASSERT((My).current <= ((My).mem + (My).size))
#else
#define CHECK_ALLOCATOR(My) /* nothing */
#endif

typedef struct _my_allocator {
    Uint size;
    byte *current;
    byte *mem;
} MyAllocator;

static void init_my_allocator(MyAllocator *my, Uint siz, byte *array)
{
    ASSERT((siz % SIZEOF_VOID_P) == 0);
    my->size = siz;
    my->mem = array;
    my->current = my->mem;
}

static void *my_alloc(MyAllocator *my, Uint size)
{
    void *ptr = my->current;
    my->current += MYALIGN(size);
    return ptr;
}

/*
 * The search functionality.
 *
 * The search is byte oriented, which works nicely for UTF-8 as well as
 * latin1 data
 */

#define ALPHABET_SIZE 256

typedef struct _findall_data {
    Uint pos;
    Uint len;
#ifdef HARDDEBUG
    Uint id;
#endif
    Eterm epos;
    Eterm elen;
} FindallData;

typedef struct _ac_node {
#ifdef HARDDEBUG
    Uint32 id;                        /* To identify h pointer targets when
					 dumping */
#endif
    Uint32 d;                         /* Depth in trie, also represents the
					 length (-1) of the matched string if
					 in final set */
    Sint32 final;                     /* Members in final set represent
				       * matches.
				       * The set representation is scattered
				       * among the nodes in this way:
				       * >0 -> this represents a member of
				       * the final set, <0 -> member of
				       * final set somewhere in the failure
				       * chain,
				       * 0 -> not member of the final set */
    struct _ac_node *h;                /* h(Hode) is the failure function */
    struct _ac_node *g[ALPHABET_SIZE]; /* g(Node,Character) is the
					  transition function */
} ACNode;

typedef struct _ac_trie {
#ifdef HARDDEBUG
    Uint32 idc;
#endif
    Uint32 counter;                    /* Number of added patterns */
    ACNode *root;                      /* pointer to the root state */
} ACTrie;

typedef struct _bm_data {
    byte *x;
    Sint len;
    Sint *goodshift;
    Sint badshift[ALPHABET_SIZE];
} BMData;

typedef struct _ac_find_all_state {
    ACNode *q;
    Uint pos;
    Uint len;
    Uint m;
    Uint allocated;
    FindallData *out;
} ACFindAllState;

typedef struct _ac_find_first_state {
    ACNode *q;
    Uint pos;
    Uint len;
    ACNode *candidate;
    Uint candidate_start;
} ACFindFirstState;

typedef struct _bm_find_all_state {
    Sint pos;
    Sint len;
    Uint m;
    Uint allocated;
    FindallData *out;
} BMFindAllState;

typedef struct _bm_find_first_state {
    Sint pos;
    Sint len;
} BMFindFirstState;

typedef enum _bf_return {
    BF_RESTART = -3,
    BF_NOT_FOUND,
    BF_BADARG,
    BF_OK
} BFReturn;

typedef struct _binary_find_all_context {
    ErtsHeapFactory factory;
    Eterm term;
    Sint head;
    Sint tail;
    Uint end_pos;
    Uint size;
    FindallData *data;
    union {
	ACFindAllState ac;
	BMFindAllState bm;
    } d;
} BinaryFindAllContext;

typedef struct _binary_find_first_context {
    Uint pos;
    Uint len;
    union {
	ACFindFirstState ac;
	BMFindFirstState bm;
    } d;
} BinaryFindFirstContext;

typedef struct _binary_find_context BinaryFindContext;

typedef struct _binary_find_search {
    void (*init) (BinaryFindContext *);
    BFReturn (*find) (BinaryFindContext *, byte *);
    void (*done) (BinaryFindContext *);
} BinaryFindSearch;

typedef Eterm (*BinaryFindResult)(Process *, Eterm, BinaryFindContext **);

typedef enum _binary_find_state {
    BFSearch,
    BFResult,
    BFDone
} BinaryFindState;

struct _binary_find_context {
    Eterm pat_type;
    Eterm pat_term;
    Binary *pat_bin;
    Uint flags;
    Uint hsstart;
    Uint hsend;
    int loop_factor;
    int exported;
    Uint reds;
    BinaryFindState state;
    Eterm trap_term;
    BinaryFindSearch *search;
    BinaryFindResult not_found;
    BinaryFindResult found;
    union {
	BinaryFindAllContext fa;
	BinaryFindFirstContext ff;
    } u;
};

#ifdef HARDDEBUG
static void dump_bm_data(BMData *bm);
static void dump_ac_trie(ACTrie *act);
static void dump_ac_node(ACNode *node, int indent, int ch);
#endif

/*
 * The needed size of binary data for a search structure - given the
 * accumulated string lengths.
 */
#define BM_SIZE(StrLen) 	      /* StrLen: length of searchstring */ \
((MYALIGN(sizeof(Sint) * (StrLen))) + /* goodshift array */                \
 MYALIGN(StrLen) +                    /* searchstring saved */             \
 (MYALIGN(sizeof(BMData))))           /* Structure */

#define AC_SIZE(StrLens)       /* StrLens: sum of all searchstring lengths */ \
((MYALIGN(sizeof(ACNode)) *                                                   \
((StrLens)+1)) + 	       /* The actual nodes (including rootnode) */    \
 MYALIGN(sizeof(ACTrie)))      /* Structure */


/*
 * Callback for the magic binary
 */
static int cleanup_my_data_ac(Binary *bp)
{
    return 1;
}
static int cleanup_my_data_bm(Binary *bp)
{
    return 1;
}

/*
 * Initiate a (allocated) micro allocator and fill in the base
 * for an Aho-Corasick search trie, given the accumulated length of the search
 * strings.
 */
static ACTrie *create_acdata(MyAllocator *my, Uint len,
			     ACNode ***qbuff /* out */,
			     Binary **the_bin /* out */)
{
    Uint datasize = AC_SIZE(len);
    ACTrie *act;
    ACNode *acn;
    Binary *mb = erts_create_magic_binary(datasize,cleanup_my_data_ac);
    byte *data = ERTS_MAGIC_BIN_DATA(mb);

    init_my_allocator(my, datasize, data);
    act = my_alloc(my, sizeof(ACTrie)); /* Important that this is the first
					   allocation */
    act->counter = 0;
    act->root = acn = my_alloc(my, sizeof(ACNode));
    acn->d = 0;
    acn->final = 0;
    acn->h = NULL;
    sys_memset(acn->g, 0, sizeof(ACNode *) * ALPHABET_SIZE);
#ifdef HARDDEBUG
    act->idc = 0;
    acn->id = 0;
#endif
    *qbuff = erts_alloc(ERTS_ALC_T_TMP, sizeof(ACNode *) * len);
    *the_bin = mb;
    return act;
}

/*
 * The same initialization of allocator and basic data for Boyer-Moore.
 */
static BMData *create_bmdata(MyAllocator *my, byte *x, Uint len,
			     Binary **the_bin /* out */)
{
    Uint datasize = BM_SIZE(len);
    BMData *bmd;
    Binary *mb = erts_create_magic_binary(datasize,cleanup_my_data_bm);
    byte *data = ERTS_MAGIC_BIN_DATA(mb);
    init_my_allocator(my, datasize, data);
    bmd = my_alloc(my, sizeof(BMData));
    bmd->x = my_alloc(my,len);
    sys_memcpy(bmd->x,x,len);
    bmd->len = len;
    bmd->goodshift = my_alloc(my,sizeof(Uint) * len);
    *the_bin = mb;
    return bmd;
}

/*
 * Compilation of search structures
 */

/*
 * Aho Corasick - Build a Trie and fill in the failure functions
 * when all strings are added.
 * The algorithm is nicely described by Dieter Bühler of University of
 * Tübingen:
 * http://www-sr.informatik.uni-tuebingen.de/~buehler/AC/AC.html
 */

/*
 * Helper called once for each search pattern
 */
static void ac_add_one_pattern(MyAllocator *my, ACTrie *act, byte *x, Uint len)
{
    ACNode *acn = act->root;
    Uint32 n = ++act->counter; /* Always increase counter, even if it's a
				  duplicate as this may identify the pattern
				  in the final set (not in current interface
				  though) */
    Uint i = 0;

    while(i < len) {
	if (acn->g[x[i]] != NULL) {
	    /* node exists, continue */
	    acn = acn->g[x[i]];
	    ++i;
	} else {
	    /* allocate a new node */
	    ACNode *nn = my_alloc(my,sizeof(ACNode));
#ifdef HARDDEBUG
	    nn->id = ++(act->idc);
#endif
	    nn->d = i+1;
	    nn->h = act->root;
	    nn->final = 0;
	    sys_memset(nn->g, 0, sizeof(ACNode *) * ALPHABET_SIZE);
	    acn->g[x[i]] = nn;
	    ++i;
	    acn = nn;
	}
    }
    if (acn->final == 0) { /* New pattern, add to final set */
	acn->final = n;
    }
}

/*
 * Called when all search patterns are added.
 */
static void ac_compute_failure_functions(ACTrie *act, ACNode **qbuff)
{
    ACNode *root = act->root;
    ACNode *parent;
    int i;
    int qh = 0,qt = 0;
    ACNode *child, *r;

    /* Set all children of the root to have the root as failure function */
    for (i = 0; i < ALPHABET_SIZE; ++i) {
	if (root->g[i] != NULL) {
	    root->g[i]->h = root;
	    /* Add to que for later traversal */
	    qbuff[qt++] = root->g[i];
	}
    }

    /* So, now we've handled children of the root state, traverse the
       rest of the trie BF... */
    while (qh < qt) {
	parent = qbuff[qh++];
	for (i = 0; i < ALPHABET_SIZE; ++ i) {
	    if ((child = parent->g[i]) != NULL) {
		/* Visit this node to */
		qbuff[qt++] = child;
		/* Search for correct failure function, follow the parent's
		   failure function until you find a similar transition
		   function to this child's */
		r =  parent->h;
		while (r != NULL && r->g[i] == NULL) {
		    r = r->h;
		}
		if (r == NULL) {
		    /* Replace NULL failures with the root as we go */
		    child->h = (root->g[i] == NULL) ? root : root->g[i];
		} else {
		    child->h = r->g[i];
		    /*
		     * The "final" set is scattered among the nodes. When
		     * the failure function points to a member of the final
		     * set, we have a match, but we might not see it in the
		     * current node if we dont mark it as a special type of
		     * final, i.e. foolow the failure function and you will
		     * find a real member of final set. This is marked with
		     * a negative string id and only done if this node does
		     * not represent a member in the final set.
		     */
		    if (!(child->final) && (child->h->final)) {
			child->final = -1;
		    }
		}
	    }
	}
    }
    /* Finally the failure function of the root should point to itself */
    root->h = root;
}


/*
 * The actual searching for needles in the haystack...
 * Find first match using Aho-Coracick Trie
 * return pattern number and fill in mpos + mlen if found, otherwise return 0
 * Return the matching pattern that *starts* first, and ends
 * last (difference when overlapping), hence the candidate thing.
 * Basic AC finds the first end before the first start...
 *
 */
static void ac_init_find_first_match(BinaryFindContext *ctx)
{
    ACFindFirstState *state = &(ctx->u.ff.d.ac);
    ACTrie *act = ERTS_MAGIC_BIN_DATA(ctx->pat_bin);
    state->q = act->root;
    state->pos = ctx->hsstart;
    state->len = ctx->hsend;
    state->candidate = NULL;
    state->candidate_start = 0;
}

#define AC_LOOP_FACTOR 10

static BFReturn ac_find_first_match(BinaryFindContext *ctx, byte *haystack)
{
    ACFindFirstState *state = &(ctx->u.ff.d.ac);
    Uint *mpos = &(ctx->u.ff.pos);
    Uint *mlen = &(ctx->u.ff.len);
    Uint *reductions = &(ctx->reds);
    ACNode *q = state->q;
    Uint i = state->pos;
    ACNode *candidate = state->candidate, *r;
    Uint len = state->len;
    Uint candidate_start = state->candidate_start;
    Uint rstart;
    register Uint reds = *reductions;

    while (i < len) {
	if (--reds == 0) {
	    state->q = q;
	    state->pos = i;
	    state->len = len;
	    state->candidate = candidate;
	    state->candidate_start = candidate_start;
	    return BF_RESTART;
	}

	while (q->g[haystack[i]] == NULL && q->h != q) {
	    q = q->h;
	}
	if (q->g[haystack[i]] != NULL) {
	    q = q->g[haystack[i]];
	}
#ifdef HARDDEBUG
	erts_printf("ch = %c, Current: %u\n", (int) haystack[i], (unsigned) q->id);
#endif
	++i;
	if (candidate != NULL && (i - q->d) > candidate_start) {
	    break;
	}
	if (q->final) {
	    r = q;
	    while (r->final < 0)
		r = r->h;
	    rstart = i - r->d;
	    if (candidate == NULL || rstart < candidate_start ||
		(rstart == candidate_start && candidate->d < q->d)) {
		candidate_start = rstart;
		candidate = r;
	    }
	}
    }
    *reductions = reds;
    if (!candidate) {
	return BF_NOT_FOUND;
    }
#ifdef HARDDEBUG
    dump_ac_node(candidate,0,'?');
#endif
    *mpos = candidate_start;
    *mlen = candidate->d;
    return BF_OK;
}

static void ac_init_find_all(BinaryFindContext *ctx)
{
    ACFindAllState *state = &(ctx->u.fa.d.ac);
    ACTrie *act = ERTS_MAGIC_BIN_DATA(ctx->pat_bin);
    state->q = act->root;
    state->pos = ctx->hsstart;
    state->len = ctx->hsend;
    state->m = 0;
    state->allocated = 0;
    state->out = NULL;
}

static void ac_clean_find_all(BinaryFindContext *ctx)
{
    ACFindAllState *state = &(ctx->u.fa.d.ac);
    if (state->out != NULL) {
	erts_free(ERTS_ALC_T_BINARY_FIND, state->out);
    }
#ifdef HARDDEBUG
    state->out = NULL;
    state->allocated = 0;
#endif
}

/*
 * Differs to the find_first function in that it stores all matches and the values
 * arte returned only in the state.
 */
static BFReturn ac_find_all_non_overlapping(BinaryFindContext *ctx, byte *haystack)
{
    ACFindAllState *state = &(ctx->u.fa.d.ac);
    Uint *reductions = &(ctx->reds);
    ACNode *q = state->q;
    Uint i = state->pos;
    Uint rstart;
    ACNode *r;
    Uint len = state->len;
    Uint m = state->m, save_m;
    Uint allocated = state->allocated;
    FindallData *out = state->out;
    register Uint reds = *reductions;

    while (i < len) {
	if (--reds == 0) {
	    state->q = q;
	    state->pos = i;
	    state->len = len;
	    state->m = m;
	    state->allocated = allocated;
	    state->out = out;
	    return BF_RESTART;
	}
	while (q->g[haystack[i]] == NULL && q->h != q) {
	    q = q->h;
	}
	if (q->g[haystack[i]] != NULL) {
	    q = q->g[haystack[i]];
	}
	++i;
	if (q->final) {
	    r = q;
	    while (r->final) {
		while (r->final < 0)
		    r = r->h;
#ifdef HARDDEBUG
		erts_printf("Trying to add %u\n",(unsigned) r->final);
#endif
		rstart = i - r->d;
		save_m = m;
		while (m > 0 && (out[m-1].pos > rstart ||
				 (out[m-1].pos == rstart &&
				  out[m-1].len < r->d))) {
#ifdef HARDDEBUG
		    erts_printf("Popping %u\n",(unsigned) out[m-1].id);
#endif
		    --m;
		}
#ifdef HARDDEBUG
		if (m > 0) {
		    erts_printf("Pos %u\n",out[m-1].pos);
		    erts_printf("Len %u\n",out[m-1].len);
		}
		erts_printf("Rstart %u\n",rstart);
#endif
		if (m == 0 ||  out[m-1].pos + out[m-1].len <= rstart) {
		    if (m >= allocated) {
			if (!allocated) {
			    allocated = 10;
			    out = erts_alloc(ERTS_ALC_T_BINARY_FIND,
					     sizeof(FindallData) * allocated);
			} else {
			    allocated *= 2;
			    out = erts_realloc(ERTS_ALC_T_BINARY_FIND, out,
					       sizeof(FindallData) *
					       allocated);
			}
		    }
		    out[m].pos = rstart;
		    out[m].len = r->d;
#ifdef HARDDEBUG
		    out[m].id = r->final;
#endif
		    ++m;
#ifdef HARDDEBUG
		    erts_printf("Pushing %u\n",(unsigned) out[m-1].id);
#endif
		} else {
#ifdef HARDDEBUG
		    erts_printf("Backtracking %d steps\n",save_m - m);
#endif
		    m = save_m;
		}
		r = r->h;
	    }
	}
    }
    *reductions = reds;
    state->m = m;
    state->out = out;
    return (m == 0) ? BF_NOT_FOUND : BF_OK;
}

/*
 * Boyer Moore - most obviously implemented more or less exactly as
 * Christian Charras and Thierry Lecroq describe it in "Handbook of
 * Exact String-Matching Algorithms"
 * http://www-igm.univ-mlv.fr/~lecroq/string/
 */

/*
 * Call this to compute badshifts array
 */
static void compute_badshifts(BMData *bmd)
{
    Sint i;
    Sint m = bmd->len;

    for (i = 0; i < ALPHABET_SIZE; ++i) {
	bmd->badshift[i] = m;
    }
    for (i = 0; i < m - 1; ++i) {
	bmd->badshift[bmd->x[i]] = m - i - 1;
    }
}

/* Helper for "compute_goodshifts" */
static void compute_suffixes(byte *x, Sint m, Sint *suffixes)
{
    int f,g,i;

    suffixes[m - 1] = m;

    f = 0; /* To avoid use before set warning */

    g = m - 1;

    for (i = m - 2; i >= 0; --i) {
	if (i > g && suffixes[i + m - 1 - f] < i - g) {
	    suffixes[i] = suffixes[i + m - 1 - f];
	} else {
	    if (i < g) {
		g = i;
	    }
	    f = i;
	    while ( g >= 0 && x[g] == x[g + m - 1 - f] ) {
		--g;
	    }
	    suffixes[i] = f - g;
	}
    }
}

/*
 * Call this to compute goodshift array
 */
static void compute_goodshifts(BMData *bmd)
{
    Sint m = bmd->len;
    byte *x = bmd->x;
    Sint i, j;
    Sint *suffixes = erts_alloc(ERTS_ALC_T_TMP, m * sizeof(Sint));

    compute_suffixes(x, m, suffixes);

    for (i = 0; i < m; ++i) {
	bmd->goodshift[i] = m;
    }

    j = 0;

    for (i = m - 1; i >= -1; --i) {
	if (i == -1 || suffixes[i] == i + 1) {
	    while (j < m - 1 - i) {
		if (bmd->goodshift[j] == m) {
		    bmd->goodshift[j] = m - 1 - i;
		}
		++j;
	    }
	}
    }
    for (i = 0; i <= m - 2; ++i) {
	bmd->goodshift[m - 1 - suffixes[i]] = m - 1 - i;
    }
    erts_free(ERTS_ALC_T_TMP, suffixes);
}

#define BM_LOOP_FACTOR 10 /* Should we have a higher value? */

static void bm_init_find_first_match(BinaryFindContext *ctx)
{
    BMFindFirstState *state = &(ctx->u.ff.d.bm);
    state->pos = ctx->hsstart;
    state->len = ctx->hsend;
}

static BFReturn bm_find_first_match(BinaryFindContext *ctx, byte *haystack)
{
    BMFindFirstState *state = &(ctx->u.ff.d.bm);
    BMData *bmd = ERTS_MAGIC_BIN_DATA(ctx->pat_bin);
    Uint *mpos = &(ctx->u.ff.pos);
    Uint *mlen = &(ctx->u.ff.len);
    Uint *reductions = &(ctx->reds);
    Sint blen = bmd->len;
    Sint len = state->len;
    Sint *gs = bmd->goodshift;
    Sint *bs = bmd->badshift;
    byte *needle = bmd->x;
    Sint i;
    Sint j = state->pos;
    register Uint reds = *reductions;

    while (j <= len - blen) {
	if (--reds == 0) {
	    state->pos = j;
	    return BF_RESTART;
	}
	for (i = blen - 1; i >= 0 && needle[i] == haystack[i + j]; --i)
	    ;
	if (i < 0) { /* found */
	    *reductions = reds;
	    *mpos = (Uint) j;
	    *mlen = (Uint) blen;
	    return BF_OK;
	}
	j += MAX(gs[i],bs[haystack[i+j]] - blen + 1 + i);
    }
    *reductions = reds;
    return BF_NOT_FOUND;
}

static void bm_init_find_all(BinaryFindContext *ctx)
{
    BMFindAllState *state = &(ctx->u.fa.d.bm);
    state->pos = ctx->hsstart;
    state->len = ctx->hsend;
    state->m = 0;
    state->allocated = 0;
    state->out = NULL;
}

static void bm_clean_find_all(BinaryFindContext *ctx)
{
    BMFindAllState *state = &(ctx->u.fa.d.bm);
    if (state->out != NULL) {
	erts_free(ERTS_ALC_T_BINARY_FIND, state->out);
    }
#ifdef HARDDEBUG
    state->out = NULL;
    state->allocated = 0;
#endif
}

/*
 * Differs to the find_first function in that it stores all matches and the
 * values are returned only in the state.
 */
static BFReturn bm_find_all_non_overlapping(BinaryFindContext *ctx, byte *haystack)
{
    BMFindAllState *state = &(ctx->u.fa.d.bm);
    BMData *bmd = ERTS_MAGIC_BIN_DATA(ctx->pat_bin);
    Uint *reductions = &(ctx->reds);
    Sint blen = bmd->len;
    Sint len = state->len;
    Sint *gs = bmd->goodshift;
    Sint *bs = bmd->badshift;
    byte *needle = bmd->x;
    Sint i;
    Sint j = state->pos;
    Uint m = state->m;
    Uint allocated = state->allocated;
    FindallData *out = state->out;
    register Uint reds = *reductions;

    while (j <= len - blen) {
	if (--reds == 0) {
	    state->pos = j;
	    state->m = m;
	    state->allocated = allocated;
	    state->out = out;
	    return BF_RESTART;
	}
	for (i = blen - 1; i >= 0 && needle[i] == haystack[i + j]; --i)
	    ;
	if (i < 0) { /* found */
	    if (m >= allocated) {
		if (!allocated) {
		    allocated = 10;
		    out = erts_alloc(ERTS_ALC_T_BINARY_FIND,
				     sizeof(FindallData) * allocated);
		} else {
		    allocated *= 2;
		    out = erts_realloc(ERTS_ALC_T_BINARY_FIND, out,
				       sizeof(FindallData) * allocated);
		}
	    }
	    out[m].pos = j;
	    out[m].len = blen;
	    ++m;
	    j += blen;
	} else {
	    j += MAX(gs[i],bs[haystack[i+j]] - blen + 1 + i);
	}
    }
    state->m = m;
    state->out = out;
    *reductions = reds;
    return (m == 0) ? BF_NOT_FOUND : BF_OK;
}

/*
 * Interface functions (i.e. "bif's")
 */

/*
 * Search functionality interfaces
 */

static int do_binary_match_compile(Eterm argument, Eterm *tag, Binary **binp)
{
    Eterm t, b, comp_term = NIL;
    Uint characters;
    Uint words;

    characters = 0;
    words = 0;

    if (is_list(argument)) {
	t = argument;
	while (is_list(t)) {
	    b = CAR(list_val(t));
	    t = CDR(list_val(t));
	    if (!is_binary(b)) {
		goto badarg;
	    }
	    if (binary_bitsize(b) != 0) {
		goto badarg;
	    }
	    if (binary_size(b) == 0) {
		goto badarg;
	    }
	    ++words;
	    characters += binary_size(b);
	}
	if (is_not_nil(t)) {
	    goto badarg;
	}
	if (words > 1) {
	    comp_term = argument;
	} else {
	    comp_term = CAR(list_val(argument));
	}
    } else if (is_binary(argument)) {
	if (binary_bitsize(argument) != 0) {
	    goto badarg;
	}
	words = 1;
	comp_term = argument;
	characters = binary_size(argument);
    }

    if (characters == 0) {
	goto badarg;
    }
    ASSERT(words > 0);

    if (words == 1) {
	byte *bytes;
	Uint bitoffs, bitsize;
	byte *temp_alloc = NULL;
	MyAllocator my;
	BMData *bmd;
	Binary *bin;

	ERTS_GET_BINARY_BYTES(comp_term, bytes, bitoffs, bitsize);
	if (bitoffs != 0) {
	    bytes = erts_get_aligned_binary_bytes(comp_term, &temp_alloc);
	}
	bmd = create_bmdata(&my, bytes, characters, &bin);
	compute_badshifts(bmd);
	compute_goodshifts(bmd);
	erts_free_aligned_binary_bytes(temp_alloc);
	CHECK_ALLOCATOR(my);
	*tag = am_bm;
	*binp = bin;
	return 0;
    } else {
	ACTrie *act;
	MyAllocator my;
	ACNode **qbuff;
	Binary *bin;

	act = create_acdata(&my, characters, &qbuff, &bin);
	t = comp_term;
	while (is_list(t)) {
	    byte *bytes;
	    Uint bitoffs, bitsize;
	    byte *temp_alloc = NULL;
	    b = CAR(list_val(t));
	    t = CDR(list_val(t));
	    ERTS_GET_BINARY_BYTES(b, bytes, bitoffs, bitsize);
	    if (bitoffs != 0) {
		bytes = erts_get_aligned_binary_bytes(b, &temp_alloc);
	    }
	    ac_add_one_pattern(&my,act,bytes,binary_size(b));
	    erts_free_aligned_binary_bytes(temp_alloc);
	}
	ac_compute_failure_functions(act,qbuff);
	CHECK_ALLOCATOR(my);
	erts_free(ERTS_ALC_T_TMP,qbuff);
	*tag = am_ac;
	*binp = bin;
	return 0;
    }
 badarg:
    return -1;
}

BIF_RETTYPE binary_compile_pattern_1(BIF_ALIST_1)
{
    Binary *bin;
    Eterm tag, ret;
    Eterm *hp;

    if (do_binary_match_compile(BIF_ARG_1,&tag,&bin)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE+3);
    ret = erts_mk_magic_ref(&hp, &MSO(BIF_P), bin);
    ret = TUPLE2(hp, tag, ret);
    BIF_RET(ret);
}

#define BF_FLAG_GLOBAL		0x01
#define BF_FLAG_SPLIT_TRIM	0x02
#define BF_FLAG_SPLIT_TRIM_ALL	0x04

static void bf_context_init(BinaryFindContext *ctx, BinaryFindResult not_found,
			    BinaryFindResult single, BinaryFindResult global,
			    Binary *pat_bin);
static BinaryFindContext *bf_context_export(Process *p, BinaryFindContext *src);
static int bf_context_destructor(Binary *ctx_bin);
#ifdef HARDDEBUG
static void bf_context_dump(BinaryFindContext *ctx);
#endif

static BinaryFindSearch bf_search_ac_global = {
    ac_init_find_all,
    ac_find_all_non_overlapping,
    ac_clean_find_all
};

static BinaryFindSearch bf_search_ac_single = {
    ac_init_find_first_match,
    ac_find_first_match,
    NULL
};

static BinaryFindSearch bf_search_bm_global = {
    bm_init_find_all,
    bm_find_all_non_overlapping,
    bm_clean_find_all
};

static BinaryFindSearch bf_search_bm_single = {
    bm_init_find_first_match,
    bm_find_first_match,
    NULL
};

static void bf_context_init(BinaryFindContext *ctx, BinaryFindResult not_found,
			    BinaryFindResult single, BinaryFindResult global,
			    Binary *pat_bin)
{
    ctx->exported = 0;
    ctx->state = BFSearch;
    ctx->not_found = not_found;
    if (ctx->flags & BF_FLAG_GLOBAL) {
	ctx->found = global;
	if (ctx->pat_type == am_bm) {
	    ctx->search = &bf_search_bm_global;
	    ctx->loop_factor = BM_LOOP_FACTOR;
	} else if (ctx->pat_type == am_ac) {
	    ctx->search = &bf_search_ac_global;
	    ctx->loop_factor = AC_LOOP_FACTOR;
	}
    } else {
	ctx->found = single;
	if (ctx->pat_type == am_bm) {
	    ctx->search = &bf_search_bm_single;
	    ctx->loop_factor = BM_LOOP_FACTOR;
	} else if (ctx->pat_type == am_ac) {
	    ctx->search = &bf_search_ac_single;
	    ctx->loop_factor = AC_LOOP_FACTOR;
	}
    }
    ctx->trap_term = THE_NON_VALUE;
    ctx->pat_bin = pat_bin;
    ctx->search->init(ctx);
}

static BinaryFindContext *bf_context_export(Process *p, BinaryFindContext *src)
{
    Binary *ctx_bin;
    BinaryFindContext *ctx;
    Eterm *hp;

    ASSERT(src->exported == 0);
    ctx_bin = erts_create_magic_binary(sizeof(BinaryFindContext),
				       bf_context_destructor);
    ctx = ERTS_MAGIC_BIN_DATA(ctx_bin);
    sys_memcpy(ctx, src, sizeof(BinaryFindContext));
    if (ctx->pat_bin != NULL && ctx->pat_term == THE_NON_VALUE) {
	hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE * 2);
	ctx->pat_term = erts_mk_magic_ref(&hp, &MSO(p), ctx->pat_bin);
    } else {
	hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
    }
    ctx->trap_term = erts_mk_magic_ref(&hp, &MSO(p), ctx_bin);
    ctx->exported = 1;
    return ctx;
}

static int bf_context_destructor(Binary *ctx_bin)
{
    BinaryFindContext *ctx;

    ctx = ERTS_MAGIC_BIN_DATA(ctx_bin);
    if (ctx->state != BFDone) {
	if (ctx->search->done != NULL) {
	    ctx->search->done(ctx);
	}
	ctx->state = BFDone;
    }
    return 1;
}

#ifdef HARDDEBUG
static void bf_context_dump(BinaryFindContext *ctx)
{
    if (ctx->pat_type == am_bm) {
	BMData *bm;
	bm = ERTS_MAGIC_BIN_DATA(ctx->pat_bin);
	dump_bm_data(bm);
    } else {
	ACTrie *act;
	act = ERTS_MAGIC_BIN_DATA(ctx->pat_bin);
	dump_ac_trie(act);
    }
}
#endif

static Eterm do_match_not_found_result(Process *p, Eterm subject, BinaryFindContext **ctxp);
static Eterm do_match_single_result(Process *p, Eterm subject, BinaryFindContext **ctxp);
static Eterm do_match_global_result(Process *p, Eterm subject, BinaryFindContext **ctxp);
static Eterm do_split_not_found_result(Process *p, Eterm subject, BinaryFindContext **ctxp);
static Eterm do_split_single_result(Process *p, Eterm subject, BinaryFindContext **ctxp);
static Eterm do_split_global_result(Process *p, Eterm subject, BinaryFindContext **ctxp);

static BFReturn maybe_binary_match_compile(BinaryFindContext *ctx, Eterm arg, Binary **pat_bin)
{
    Eterm *tp;
    ctx->pat_term = THE_NON_VALUE;
    if (is_tuple(arg)) {
	tp = tuple_val(arg);
	if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
	    return BF_BADARG;
	}
	if (((tp[1] != am_bm) && (tp[1] != am_ac)) ||
	    !is_internal_magic_ref(tp[2])) {
	    return BF_BADARG;
	}
	*pat_bin = erts_magic_ref2bin(tp[2]);
	if ((tp[1] == am_bm &&
	     ERTS_MAGIC_BIN_DESTRUCTOR(*pat_bin) != cleanup_my_data_bm) ||
	    (tp[1] == am_ac &&
	     ERTS_MAGIC_BIN_DESTRUCTOR(*pat_bin) != cleanup_my_data_ac)) {
	    *pat_bin = NULL;
	    return BF_BADARG;
	}
	ctx->pat_type = tp[1];
	ctx->pat_term = tp[2];
    } else if (do_binary_match_compile(arg, &(ctx->pat_type), pat_bin) != 0) {
	return BF_BADARG;
    }
    return BF_OK;
}

static int parse_match_opts_list(Eterm l, Eterm bin, Uint *posp, Uint *endp)
{
    Eterm *tp;
    Uint pos;
    Sint len;
    if (l == THE_NON_VALUE || l == NIL) {
	/* Invalid term or NIL, we're called from binary_match(es)_2 or
	   have no options*/
	*posp = 0;
	*endp = binary_size(bin);
	return 0;
    } else if (is_list(l)) {
	while(is_list(l)) {
	    Eterm t = CAR(list_val(l));
	    Uint orig_size;
	    if (!is_tuple(t)) {
		goto badarg;
	    }
	    tp = tuple_val(t);
	    if (arityval(*tp) != 2) {
		goto badarg;
	    }
	    if (tp[1] != am_scope || is_not_tuple(tp[2])) {
		goto badarg;
	    }
	    tp = tuple_val(tp[2]);
	    if (arityval(*tp) != 2) {
		goto badarg;
	    }
	    if (!term_to_Uint(tp[1], &pos)) {
		goto badarg;
	    }
	    if (!term_to_Sint(tp[2], &len)) {
		goto badarg;
	    }
	    if (len < 0) {
		Uint lentmp = -(Uint)len;
		/* overflow */
		if ((Sint)lentmp < 0) {
		    goto badarg;
		}
		len = lentmp;
		pos -= len;
	    }
	    /* overflow */
	    if ((pos + len) < pos || (len > 0 && (pos + len) == pos)) {
		goto badarg;
	    }
	    *endp = len + pos;
	    *posp = pos;
	    if ((orig_size = binary_size(bin)) < pos ||
		orig_size < (*endp)) {
		goto badarg;
	    }
	    l = CDR(list_val(l));
	}
	return 0;
    } else {
    badarg:
	return 1;
    }
}

static int parse_split_opts_list(Eterm l, Eterm bin, Uint *posp, Uint *endp, Uint *optp)
{
    Eterm *tp;
    Uint pos;
    Sint len;
    *optp = 0;
    *posp = 0;
    *endp = binary_size(bin);
    if (l == THE_NON_VALUE || l == NIL) {
	return 0;
    } else if (is_list(l)) {
	while(is_list(l)) {
	    Eterm t = CAR(list_val(l));
	    Uint orig_size;
	    if (is_atom(t)) {
		if (t == am_global) {
		    *optp |= BF_FLAG_GLOBAL;
		    l = CDR(list_val(l));
		    continue;
		}
		if (t == am_trim) {
		    *optp |= BF_FLAG_SPLIT_TRIM;
		    l = CDR(list_val(l));
		    continue;
		}
		if (t == am_trim_all) {
		    *optp |= BF_FLAG_SPLIT_TRIM_ALL;
		    l = CDR(list_val(l));
		    continue;
		}
	    }
	    if (!is_tuple(t)) {
		goto badarg;
	    }
	    tp = tuple_val(t);
	    if (arityval(*tp) != 2) {
		goto badarg;
	    }
	    if (tp[1] != am_scope || is_not_tuple(tp[2])) {
		goto badarg;
	    }
	    tp = tuple_val(tp[2]);
	    if (arityval(*tp) != 2) {
		goto badarg;
	    }
	    if (!term_to_Uint(tp[1], &pos)) {
		goto badarg;
	    }
	    if (!term_to_Sint(tp[2], &len)) {
		goto badarg;
	    }
	    if (len < 0) {
		Uint lentmp = -(Uint)len;
		/* overflow */
		if ((Sint)lentmp < 0) {
		    goto badarg;
		}
		len = lentmp;
		pos -= len;
	    }
	    /* overflow */
	    if ((pos + len) < pos || (len > 0 && (pos + len) == pos)) {
		goto badarg;
	    }
	    *endp = len + pos;
	    *posp = pos;
	    if ((orig_size = binary_size(bin)) < pos ||
		orig_size < (*endp)) {
		goto badarg;
	    }
	    l = CDR(list_val(l));
	}
	return 0;
    } else {
    badarg:
	return 1;
    }
}

static BFReturn do_binary_find(Process *p, Eterm subject, BinaryFindContext **ctxp,
			       Binary *pat_bin, Binary *ctx_bin, Eterm *res_term)
{
    BinaryFindContext *ctx;
    int is_first_call;
    Uint initial_reds;
    BFReturn runres;

    if (ctx_bin == NULL) {
	is_first_call = 1;
	ctx = *ctxp;
    } else {
	is_first_call = 0;
	ctx = ERTS_MAGIC_BIN_DATA(ctx_bin);
	ctx->pat_bin = pat_bin;
	*ctxp = ctx;
    }

    initial_reds = ctx->reds = get_reds(p, ctx->loop_factor);

    switch (ctx->state) {
    case BFSearch: {
	byte *bytes;
	Uint bitoffs, bitsize;
	byte *temp_alloc = NULL;

	ERTS_GET_BINARY_BYTES(subject, bytes, bitoffs, bitsize);
	if (bitsize != 0) {
	    goto badarg;
	}
	if (bitoffs != 0) {
	    bytes = erts_get_aligned_binary_bytes(subject, &temp_alloc);
	}
#ifdef HARDDEBUG
	bf_context_dump(ctx);
#endif
	runres = ctx->search->find(ctx, bytes);
	if (runres == BF_NOT_FOUND) {
	    *res_term = ctx->not_found(p, subject, &ctx);
	    *ctxp = ctx;
	} else if (runres == BF_RESTART) {
#ifdef HARDDEBUG
	    if (ctx->pat_type == am_ac) {
		erts_printf("Trap ac!\n");
	    } else {
		erts_printf("Trap bm!\n");
	    }
#endif
	    if (is_first_call) {
		ctx = bf_context_export(p, ctx);
		*ctxp = ctx;
		erts_set_gc_state(p, 0);
	    }
	    erts_free_aligned_binary_bytes(temp_alloc);
	    *res_term = THE_NON_VALUE;
	    BUMP_ALL_REDS(p);
	    return BF_RESTART;
	} else {
	    *res_term = ctx->found(p, subject, &ctx);
	    *ctxp = ctx;
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	if (*res_term == THE_NON_VALUE) {
	    if (is_first_call) {
		erts_set_gc_state(p, 0);
	    }
	    BUMP_ALL_REDS(p);
	    return BF_RESTART;
	}
	if (ctx->search->done != NULL) {
	    ctx->search->done(ctx);
	}
	ctx->state = BFDone;
	if (!is_first_call) {
	    erts_set_gc_state(p, 1);
	}
	BUMP_REDS(p, (initial_reds - ctx->reds) / ctx->loop_factor);
	return BF_OK;
    }
    case BFResult: {
	*res_term = ctx->found(p, subject, &ctx);
	*ctxp = ctx;
	if (*res_term == THE_NON_VALUE) {
	    if (is_first_call) {
		erts_set_gc_state(p, 0);
	    }
	    BUMP_ALL_REDS(p);
	    return BF_RESTART;
	}
	if (ctx->search->done != NULL) {
	    ctx->search->done(ctx);
	}
	ctx->state = BFDone;
	if (!is_first_call) {
	    erts_set_gc_state(p, 1);
	}
	BUMP_REDS(p, (initial_reds - ctx->reds) / ctx->loop_factor);
	return BF_OK;
    }
    default:
	ASSERT(!"Unknown state in do_binary_find");
    }

badarg:
    if (!is_first_call) {
	if (ctx->search->done != NULL) {
	    ctx->search->done(ctx);
	}
	ctx->state = BFDone;
	erts_set_gc_state(p, 1);
    }
    return BF_BADARG;
}

static BIF_RETTYPE
binary_match(Process *p, Eterm arg1, Eterm arg2, Eterm arg3, Uint flags)
{
    BinaryFindContext c_buff;
    BinaryFindContext *ctx = &c_buff;
    Binary *pat_bin;
    int runres;
    Eterm result;

    if (is_not_binary(arg1) || binary_bitsize(arg1) != 0) {
	goto badarg;
    }
    ctx->flags = flags;
    if (parse_match_opts_list(arg3, arg1, &(ctx->hsstart), &(ctx->hsend))) {
	goto badarg;
    }
    if (ctx->hsend == 0) {
	result = do_match_not_found_result(p, arg1, &ctx);
	BIF_RET(result);
    }
    if (maybe_binary_match_compile(ctx, arg2, &pat_bin) != BF_OK) {
	goto badarg;
    }
    bf_context_init(ctx, do_match_not_found_result, do_match_single_result,
		    do_match_global_result, pat_bin);
    runres = do_binary_find(p, arg1, &ctx, pat_bin, NULL, &result);
    if (runres == BF_OK && ctx->pat_term == THE_NON_VALUE) {
	erts_bin_free(pat_bin);
    }
    switch (runres) {
    case BF_OK:
	BIF_RET(result);
    case BF_RESTART:
	ASSERT(result == THE_NON_VALUE && ctx->trap_term != result && ctx->pat_term != result);
	BIF_TRAP3(&binary_find_trap_export, p, arg1, ctx->trap_term, ctx->pat_term);
    default:
	goto badarg;
    }
badarg:
    BIF_ERROR(p, BADARG);
}

BIF_RETTYPE binary_match_2(BIF_ALIST_2)
{
    return binary_match(BIF_P, BIF_ARG_1, BIF_ARG_2, THE_NON_VALUE, 0);
}

BIF_RETTYPE binary_match_3(BIF_ALIST_3)
{
    return binary_match(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, 0);
}

BIF_RETTYPE binary_matches_2(BIF_ALIST_2)
{
    return binary_match(BIF_P, BIF_ARG_1, BIF_ARG_2, THE_NON_VALUE, BF_FLAG_GLOBAL);
}

BIF_RETTYPE binary_matches_3(BIF_ALIST_3)
{
    return binary_match(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, BF_FLAG_GLOBAL);
}

static BIF_RETTYPE
binary_split(Process *p, Eterm arg1, Eterm arg2, Eterm arg3)
{
    BinaryFindContext c_buff;
    BinaryFindContext *ctx = &c_buff;
    Binary *pat_bin;
    int runres;
    Eterm result;

    if (is_not_binary(arg1) || binary_bitsize(arg1) != 0) {
	goto badarg;
    }
    if (parse_split_opts_list(arg3, arg1, &(ctx->hsstart), &(ctx->hsend), &(ctx->flags))) {
	goto badarg;
    }
    if (ctx->hsend == 0) {
	result = do_split_not_found_result(p, arg1, &ctx);
	BIF_RET(result);
    }
    if (maybe_binary_match_compile(ctx, arg2, &pat_bin) != BF_OK) {
	goto badarg;
    }
    bf_context_init(ctx, do_split_not_found_result, do_split_single_result,
		    do_split_global_result, pat_bin);
    runres = do_binary_find(p, arg1, &ctx, pat_bin, NULL, &result);
    if (runres == BF_OK && ctx->pat_term == THE_NON_VALUE) {
	erts_bin_free(pat_bin);
    }
    switch (runres) {
    case BF_OK:
	BIF_RET(result);
    case BF_RESTART:
	ASSERT(result == THE_NON_VALUE && ctx->trap_term != result && ctx->pat_term != result);
	BIF_TRAP3(&binary_find_trap_export, p, arg1, ctx->trap_term, ctx->pat_term);
    default:
	goto badarg;
    }
badarg:
    BIF_ERROR(p, BADARG);
}

BIF_RETTYPE binary_split_2(BIF_ALIST_2)
{
    return binary_split(BIF_P, BIF_ARG_1, BIF_ARG_2, THE_NON_VALUE);
}

BIF_RETTYPE binary_split_3(BIF_ALIST_3)
{
    return binary_split(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

static Eterm do_match_not_found_result(Process *p, Eterm subject, BinaryFindContext **ctxp)
{
    if ((*ctxp)->flags & BF_FLAG_GLOBAL) {
	return NIL;
    } else {
	return am_nomatch;
    }
}

static Eterm do_match_single_result(Process *p, Eterm subject, BinaryFindContext **ctxp)
{
    BinaryFindContext *ctx = (*ctxp);
    BinaryFindFirstContext *ff = &(ctx->u.ff);
    Eterm erlen;
    Eterm *hp;
    Eterm ret;

    erlen = erts_make_integer((Uint)(ff->len), p);
    ret = erts_make_integer(ff->pos, p);
    hp = HAlloc(p, 3);
    ret = TUPLE2(hp, ret, erlen);

    return ret;
}

static Eterm do_match_global_result(Process *p, Eterm subject, BinaryFindContext **ctxp)
{
    BinaryFindContext *ctx = (*ctxp);
    BinaryFindAllContext *fa = &(ctx->u.fa);
    FindallData *fad;
    Eterm tpl;
    Sint i;
    register Uint reds = ctx->reds;

    if (ctx->state == BFSearch) {
	if (ctx->pat_type == am_ac) {
	    fa->data = fa->d.ac.out;
	    fa->size = fa->d.ac.m;
	} else {
	    fa->data = fa->d.bm.out;
	    fa->size = fa->d.bm.m;
	}
	fa->tail = fa->size - 1;
	fa->head = 0;
	fa->end_pos = 0;
	fa->term = NIL;
	if (ctx->exported == 0 && ((fa->size * 2) >= reds)) {
	    ctx = bf_context_export(p, ctx);
	    *ctxp = ctx;
	    fa = &(ctx->u.fa);
	}
	erts_factory_proc_prealloc_init(&(fa->factory), p, fa->size * (3 + 2));
	ctx->state = BFResult;
    }

    fad = fa->data;

    if (fa->end_pos == 0) {
	for (i = fa->head; i < fa->size; ++i) {
	    if (--reds == 0) {
		ASSERT(ctx->exported == 1);
		fa->head = i;
		ctx->reds = reds;
		return THE_NON_VALUE;
	    }
	    fad[i].epos = erts_make_integer(fad[i].pos, p);
	    fad[i].elen = erts_make_integer(fad[i].len, p);
	}
	fa->end_pos = 1;
	fa->head = fa->tail;
    }

    for (i = fa->head; i >= 0; --i) {
	if (--reds == 0) {
	    ASSERT(ctx->exported == 1);
	    fa->head = i;
	    ctx->reds = reds;
	    return THE_NON_VALUE;
	}
	tpl = TUPLE2(fa->factory.hp, fad[i].epos, fad[i].elen);
	fa->factory.hp += 3;
	fa->term = CONS(fa->factory.hp, tpl, fa->term);
	fa->factory.hp += 2;
    }
    ctx->reds = reds;
    erts_factory_close(&(fa->factory));

    return fa->term;
}

static Eterm do_split_not_found_result(Process *p, Eterm subject, BinaryFindContext **ctxp)
{
    BinaryFindContext *ctx = (*ctxp);
    Eterm *hp;
    Eterm ret;

    if (ctx->flags & (BF_FLAG_SPLIT_TRIM | BF_FLAG_SPLIT_TRIM_ALL)
        && binary_size(subject) == 0) {
	return NIL;
    }
    hp = HAlloc(p, 2);
    ret = CONS(hp, subject, NIL);
    return ret;
}

static Eterm do_split_single_result(Process *p, Eterm subject, BinaryFindContext **ctxp)
{
    BinaryFindContext *ctx = (*ctxp);
    BinaryFindFirstContext *ff = &(ctx->u.ff);
    Sint pos;
    Sint len;
    size_t orig_size;
    Eterm orig;
    Uint offset;
    Uint bit_offset;
    Uint bit_size;
    ErlSubBin *sb1;
    ErlSubBin *sb2;
    Eterm *hp;
    Eterm ret;

    pos = ff->pos;
    len = ff->len;

    orig_size = binary_size(subject);

    if ((ctx->flags & (BF_FLAG_SPLIT_TRIM | BF_FLAG_SPLIT_TRIM_ALL)) &&
	(orig_size - pos - len) == 0) {
	if (pos == 0) {
	    ret = NIL;
	} else {
	    hp = HAlloc(p, (ERL_SUB_BIN_SIZE + 2));
	    ERTS_GET_REAL_BIN(subject, orig, offset, bit_offset, bit_size);
	    sb1 = (ErlSubBin *) hp;
	    sb1->thing_word = HEADER_SUB_BIN;
	    sb1->size = pos;
	    sb1->offs = offset;
	    sb1->orig = orig;
	    sb1->bitoffs = bit_offset;
	    sb1->bitsize = bit_size;
	    sb1->is_writable = 0;
	    hp += ERL_SUB_BIN_SIZE;

	    ret = CONS(hp, make_binary(sb1), NIL);
	    hp += 2;
	}
    } else {
	if ((ctx->flags & BF_FLAG_SPLIT_TRIM_ALL) && (pos == 0)) {
	    hp = HAlloc(p, 1 * (ERL_SUB_BIN_SIZE + 2));
	    ERTS_GET_REAL_BIN(subject, orig, offset, bit_offset, bit_size);
	    sb1 = NULL;
	} else {
	    hp = HAlloc(p, 2 * (ERL_SUB_BIN_SIZE + 2));
	    ERTS_GET_REAL_BIN(subject, orig, offset, bit_offset, bit_size);
	    sb1 = (ErlSubBin *) hp;
	    sb1->thing_word = HEADER_SUB_BIN;
	    sb1->size = pos;
	    sb1->offs = offset;
	    sb1->orig = orig;
	    sb1->bitoffs = bit_offset;
	    sb1->bitsize = 0;
	    sb1->is_writable = 0;
	    hp += ERL_SUB_BIN_SIZE;
	}

	sb2 = (ErlSubBin *) hp;
	sb2->thing_word = HEADER_SUB_BIN;
	sb2->size = orig_size - pos - len;
	sb2->offs = offset + pos + len;
	sb2->orig = orig;
	sb2->bitoffs = bit_offset;
	sb2->bitsize = bit_size;
	sb2->is_writable = 0;
	hp += ERL_SUB_BIN_SIZE;

	ret = CONS(hp, make_binary(sb2), NIL);
	hp += 2;
	if (sb1 != NULL) {
	    ret = CONS(hp, make_binary(sb1), ret);
	    hp += 2;
	}
    }
    return ret;
}

static Eterm do_split_global_result(Process *p, Eterm subject, BinaryFindContext **ctxp)
{
    BinaryFindContext *ctx = (*ctxp);
    BinaryFindAllContext *fa = &(ctx->u.fa);
    FindallData *fad;
    Eterm orig;
    size_t orig_size;
    Uint offset;
    Uint bit_offset;
    Uint bit_size;
    ErlSubBin *sb;
    Uint do_trim;
    Sint i;
    register Uint reds = ctx->reds;

    if (ctx->state == BFSearch) {
	if (ctx->pat_type == am_ac) {
	    fa->data = fa->d.ac.out;
	    fa->size = fa->d.ac.m;
	} else {
	    fa->data = fa->d.bm.out;
	    fa->size = fa->d.bm.m;
	}
	fa->tail = fa->size - 1;
	fa->head = fa->tail;
	orig_size = binary_size(subject);
	fa->end_pos = (Uint)(orig_size);
	fa->term = NIL;
	if (ctx->exported == 0 && ((fa->head + 1) >= reds)) {
	    ctx = bf_context_export(p, ctx);
	    *ctxp = ctx;
	    fa = &(ctx->u.fa);
	}
	erts_factory_proc_prealloc_init(&(fa->factory), p, (fa->size + 1) * (ERL_SUB_BIN_SIZE + 2));
	ctx->state = BFResult;
    }

    ERTS_GET_REAL_BIN(subject, orig, offset, bit_offset, bit_size);
    ASSERT(bit_size == 0);
    fad = fa->data;
    do_trim = ctx->flags & (BF_FLAG_SPLIT_TRIM | BF_FLAG_SPLIT_TRIM_ALL);

    for (i = fa->head; i >= 0; --i) {
	if (--reds == 0) {
	    ASSERT(ctx->exported == 1);
	    fa->head = i;
	    ctx->reds = reds;
	    if (!do_trim && (ctx->flags & BF_FLAG_SPLIT_TRIM)) {
		ctx->flags &= ~BF_FLAG_SPLIT_TRIM;
	    }
	    return THE_NON_VALUE;
	}
	sb = (ErlSubBin *)(fa->factory.hp);
	sb->size = fa->end_pos - (fad[i].pos + fad[i].len);
	if (!(sb->size == 0 && do_trim)) {
	    sb->thing_word = HEADER_SUB_BIN;
	    sb->offs = offset + fad[i].pos + fad[i].len;
	    sb->orig = orig;
	    sb->bitoffs = bit_offset;
	    sb->bitsize = 0;
	    sb->is_writable = 0;
	    fa->factory.hp += ERL_SUB_BIN_SIZE;
	    fa->term = CONS(fa->factory.hp, make_binary(sb), fa->term);
	    fa->factory.hp += 2;
	    do_trim &= ~BF_FLAG_SPLIT_TRIM;
	}
	fa->end_pos = fad[i].pos;
    }

    fa->head = i;
    ctx->reds = reds;

    sb = (ErlSubBin *)(fa->factory.hp);
    sb->size = fad[0].pos;
    if (!(sb->size == 0 && do_trim)) {
	sb->thing_word = HEADER_SUB_BIN;
	sb->offs = offset;
	sb->orig = orig;
	sb->bitoffs = bit_offset;
	sb->bitsize = 0;
	sb->is_writable = 0;
	fa->factory.hp += ERL_SUB_BIN_SIZE;
	fa->term = CONS(fa->factory.hp, make_binary(sb), fa->term);
	fa->factory.hp += 2;
    }
    erts_factory_close(&(fa->factory));

    return fa->term;
}

static BIF_RETTYPE binary_find_trap(BIF_ALIST_3)
{
    int runres;
    Eterm result;
    Binary *ctx_bin = erts_magic_ref2bin(BIF_ARG_2);
    Binary *pat_bin = erts_magic_ref2bin(BIF_ARG_3);
    BinaryFindContext *ctx = NULL;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(ctx_bin) == bf_context_destructor);
    runres = do_binary_find(BIF_P, BIF_ARG_1, &ctx, pat_bin, ctx_bin, &result);
    if (runres == BF_OK) {
	ASSERT(result != THE_NON_VALUE);
	BIF_RET(result);
    } else {
	ASSERT(result == THE_NON_VALUE && ctx->trap_term != result && ctx->pat_term != result);
	BIF_TRAP3(&binary_find_trap_export, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }
}

BIF_RETTYPE erts_binary_part(Process *p, Eterm binary, Eterm epos, Eterm elen)
{
    Uint pos;
    Sint len;
    size_t orig_size;
    Eterm orig;
    Uint offset;
    Uint bit_offset;
    Uint bit_size;
    Eterm* hp;
    ErlSubBin* sb;

    if (is_not_binary(binary)) {
	goto badarg;
    }
    if (!term_to_Uint(epos, &pos)) {
	goto badarg;
    }
    if (!term_to_Sint(elen, &len)) {
	goto badarg;
    }
    if (len < 0) {
	Uint lentmp = -(Uint)len;
	/* overflow */
	if ((Sint)lentmp < 0) {
	    goto badarg;
	}
	len = lentmp;
	if (len > pos) {
	    goto badarg;
	}
	pos -= len;
    }
    /* overflow */
    if ((pos + len) < pos || (len > 0 && (pos + len) == pos)){
	goto badarg;
    }
    if ((orig_size = binary_size(binary)) < pos ||
	orig_size < (pos + len)) {
	goto badarg;
    }



    hp = HAlloc(p, ERL_SUB_BIN_SIZE);

    ERTS_GET_REAL_BIN(binary, orig, offset, bit_offset, bit_size);
    sb = (ErlSubBin *) hp;
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = len;
    sb->offs = offset + pos;
    sb->orig = orig;
    sb->bitoffs = bit_offset;
    sb->bitsize = 0;
    sb->is_writable = 0;

    BIF_RET(make_binary(sb));

 badarg:
    BIF_ERROR(p, BADARG);
}

#define ERTS_NEED_GC(p, need) ((HEAP_LIMIT((p)) - HEAP_TOP((p))) <= (need))

BIF_RETTYPE erts_gc_binary_part(Process *p, Eterm *reg, Eterm live, int range_is_tuple)
{
    Uint pos;
    Sint len;
    size_t orig_size;
    Eterm orig;
    Uint offset;
    Uint bit_offset;
    Uint bit_size;
    Eterm* hp;
    ErlSubBin* sb;
    Eterm binary;
    Eterm *tp;
    Eterm epos, elen;
    int extra_args;


    if (range_is_tuple) {
	Eterm tpl = reg[live];
	extra_args = 1;
	if (is_not_tuple(tpl)) {
	    goto badarg;
	}
	tp = tuple_val(tpl);
	if (arityval(*tp) != 2) {
	    goto badarg;
	}

	epos = tp[1];
	elen = tp[2];
    } else {
	extra_args = 2;
	epos = reg[live-1];
	elen = reg[live];
    }
    binary = reg[live-extra_args];

    if (is_not_binary(binary)) {
	goto badarg;
    }
    if (!term_to_Uint(epos, &pos)) {
	goto badarg;
    }
    if (!term_to_Sint(elen, &len)) {
	goto badarg;
    }
    if (len < 0) {
	Uint lentmp = -(Uint)len;
	/* overflow */
	if ((Sint)lentmp < 0) {
	    goto badarg;
	}
	len = lentmp;
	if (len > pos) {
	    goto badarg;
	}
	pos -= len;
    }
    /* overflow */
    if ((pos + len) < pos || (len > 0 && (pos + len) == pos)) {
	goto badarg;
    }
    if ((orig_size = binary_size(binary)) < pos ||
	orig_size < (pos + len)) {
	goto badarg;
    }

    if (ERTS_NEED_GC(p, ERL_SUB_BIN_SIZE)) {
	erts_garbage_collect(p, ERL_SUB_BIN_SIZE, reg, live+1-extra_args); /* I don't need the tuple
									      or indices any more */
	binary = reg[live-extra_args];
    }

    hp = p->htop;
    p->htop += ERL_SUB_BIN_SIZE;

    ERTS_GET_REAL_BIN(binary, orig, offset, bit_offset, bit_size);

    sb = (ErlSubBin *) hp;
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = len;
    sb->offs = offset + pos;
    sb->orig = orig;
    sb->bitoffs = bit_offset;
    sb->bitsize = 0;
    sb->is_writable = 0;

    BIF_RET(make_binary(sb));

 badarg:
    BIF_ERROR(p, BADARG);
}
/*************************************************************
 * The actual guard BIFs are in erl_bif_guard.c
 * but the implementation of both the non-gc and the gc
 * variants are here. Note that the functions are named so that they do
 * not clash with the guard bif's erlang:binary_part/2,3
 *************************************************************/

BIF_RETTYPE binary_binary_part_3(BIF_ALIST_3)
{
    return erts_binary_part(BIF_P,BIF_ARG_1,BIF_ARG_2, BIF_ARG_3);
}

BIF_RETTYPE binary_binary_part_2(BIF_ALIST_2)
{
    Eterm *tp;
    if (is_not_tuple(BIF_ARG_2)) {
	goto badarg;
    }
    tp = tuple_val(BIF_ARG_2);
    if (arityval(*tp) != 2) {
	goto badarg;
    }
    return erts_binary_part(BIF_P,BIF_ARG_1,tp[1], tp[2]);
 badarg:
   BIF_ERROR(BIF_P,BADARG);
}

typedef struct {
    int type;            /* CL_TYPE_XXX */
    byte *temp_alloc;    /* Used for erts_get/free_aligned, i.e. CL_TYPE_ALIGNED */
    unsigned char *buff; /* Used for all types, malloced if CL_TYPE_HEAP */
    Uint bufflen;        /* The length (in bytes) of buffer */
} CommonData;

#define COMMON_LOOP_FACTOR 10

#define DIRECTION_PREFIX 0
#define DIRECTION_SUFFIX 1

#define CL_OK 0
#define CL_RESTART 1

/* The type field in the above structure */
#define CL_TYPE_EMPTY 0 /* End of array */
#define CL_TYPE_HEAP 1
#define CL_TYPE_ALIGNED 2
#define CL_TYPE_COMMON 3 /* emacsulated */
#define CL_TYPE_HEAP_NOALLOC 4 /* Will need allocating when trapping */


static int do_search_forward(CommonData *cd, Uint *posp, Uint *redsp)
{
    Uint pos = *posp;
    Sint reds = (Sint) *redsp;
    int i;
    unsigned char current = 0;

    for(;;) {
	for(i = 0; cd[i].type != CL_TYPE_EMPTY; ++i) {
	    if (pos >= cd[i].bufflen) {
		*posp = pos;
		if (reds > 0) {
		    *redsp = (Uint) reds;
		} else {
		    *redsp = 0;
		}
		return CL_OK;
	    }
	    if (i == 0) {
		current = cd[i].buff[pos];
	    } else {
		if (cd[i].buff[pos] != current) {
		    *posp = pos;
		    if (reds > 0) {
			*redsp = (Uint) reds;
		    } else {
			*redsp = 0;
		    }
		    return CL_OK;
		}
	    }
	    --reds;
	}
	++pos;
	if (reds <= 0) {
	    *posp = pos;
	    *redsp = 0;
	    return CL_RESTART;
	}
    }
}
static int do_search_backward(CommonData *cd, Uint *posp, Uint *redsp)
{
    Uint pos = *posp;
    Sint reds = (Sint) *redsp;
    int i;
    unsigned char current = 0;

    for(;;) {
	for(i = 0; cd[i].type != CL_TYPE_EMPTY; ++i) {
	    if (pos >= cd[i].bufflen) {
		*posp = pos;
		if (reds > 0) {
		    *redsp = (Uint) reds;
		} else {
		    *redsp = 0;
		}
		return CL_OK;
	    }
	    if (i == 0) {
		current = cd[i].buff[cd[i].bufflen - 1 - pos];
	    } else {
		if (cd[i].buff[cd[i].bufflen - 1 - pos] != current) {
		    *posp = pos;
		    if (reds > 0) {
			*redsp = (Uint) reds;
		    } else {
			*redsp = 0;
		    }
		    return CL_OK;
		}
	    }
	    --reds;
	}
	++pos;
	if (reds <= 0) {
	    *posp = pos;
	    *redsp = 0;
	    return CL_RESTART;
	}
    }
}

static int cleanup_common_data(Binary *bp)
{
    int i;
    CommonData *cd;
    cd = (CommonData *) ERTS_MAGIC_BIN_DATA(bp);
    for (i=0;cd[i].type != CL_TYPE_EMPTY;++i) {
	switch (cd[i].type) {
	case CL_TYPE_HEAP:
	    erts_free(ERTS_ALC_T_BINARY_BUFFER,cd[i].buff);
	    break;
	case CL_TYPE_ALIGNED:
	    erts_free_aligned_binary_bytes_extra(cd[i].temp_alloc, ERTS_ALC_T_BINARY_BUFFER);
	    break;
	default:
	    break;
	}
    }
    return 1;
}

static BIF_RETTYPE do_longest_common(Process *p, Eterm list, int direction)
{
    Eterm l = list;
    int n = 0;
    Binary *mb;
    CommonData *cd;
    int i = 0;
    Uint reds = get_reds(p, COMMON_LOOP_FACTOR);
    Uint save_reds = reds;
    int res;
    Export *trapper;
    Uint pos;
    Eterm epos;
    Eterm *hp;
    Eterm bin_term;
    Eterm b;

    /* First just count the number of binaries */
    while (is_list(l)) {
	b = CAR(list_val(l));
	if (!is_binary(b)) {
	    goto badarg;
	}
	++n;
	l = CDR(list_val(l));
    }
    if (l != NIL || n == 0) {
	goto badarg;
    }

    /* OK, now create a buffer of the right size, we can do a magic binary right away,
       that's not too costly. */
    mb = erts_create_magic_binary((n+1)*sizeof(CommonData),cleanup_common_data);
    cd = (CommonData *) ERTS_MAGIC_BIN_DATA(mb);
    l = list;
    while (is_list(l)) {
	ERTS_DECLARE_DUMMY(Uint bitoffs);
	Uint bitsize;
	ERTS_DECLARE_DUMMY(Uint offset);
	Eterm real_bin;
	ProcBin* pb;

	cd[i].type = CL_TYPE_EMPTY;
	b = CAR(list_val(l));
	ERTS_GET_REAL_BIN(b, real_bin, offset, bitoffs, bitsize);
	if (bitsize != 0) {
	    erts_bin_free(mb);
	    goto badarg;
	}
	cd[i].bufflen = binary_size(b);
	cd[i].temp_alloc = NULL;
	if (*(binary_val(real_bin)) == HEADER_PROC_BIN) {
	    pb = (ProcBin *) binary_val(real_bin);
	    if (pb->flags) {
		erts_emasculate_writable_binary(pb);
	    }
	    cd[i].buff = erts_get_aligned_binary_bytes_extra(b, &(cd[i].temp_alloc),
							     ERTS_ALC_T_BINARY_BUFFER,0);
	    cd[i].type = (cd[i].temp_alloc != NULL) ? CL_TYPE_ALIGNED : CL_TYPE_COMMON;
	} else { /* Heap binary */
	    cd[i].buff = erts_get_aligned_binary_bytes_extra(b, &(cd[i].temp_alloc),
							     ERTS_ALC_T_BINARY_BUFFER,0);
	    /* CL_TYPE_HEAP_NOALLOC means you have to copy if trapping */
	    cd[i].type = (cd[i].temp_alloc != NULL) ? CL_TYPE_ALIGNED : CL_TYPE_HEAP_NOALLOC;
	}
	++i;
	l = CDR(list_val(l));
    }
    cd[i].type = CL_TYPE_EMPTY;
#if defined(DEBUG) || defined(VALGRIND)
    cd[i].temp_alloc = NULL;
    cd[i].buff = NULL;
    cd[i].bufflen = 0;
#endif

    pos = 0;
    if (direction == DIRECTION_PREFIX) {
	trapper = &binary_longest_prefix_trap_export;
	res = do_search_forward(cd,&pos,&reds);
    } else {
	ASSERT(direction == DIRECTION_SUFFIX);
	trapper = &binary_longest_suffix_trap_export;
	res = do_search_backward(cd,&pos,&reds);
    }
    epos = erts_make_integer(pos,p);
    if (res == CL_OK) {
	erts_bin_free(mb);
	BUMP_REDS(p, (save_reds - reds) / COMMON_LOOP_FACTOR);
	BIF_RET(epos);
    } else {
	ASSERT(res == CL_RESTART);
	/* Copy all heap binaries that are not already copied (aligned) */
	for(i = 0; i < n; ++i) {
	    if (cd[i].type == CL_TYPE_HEAP_NOALLOC) {
		unsigned char *tmp = cd[i].buff;
		cd[i].buff = erts_alloc(ERTS_ALC_T_BINARY_BUFFER, cd[i].bufflen);
		sys_memcpy(cd[i].buff,tmp,cd[i].bufflen);
		cd[i].type = CL_TYPE_HEAP;
	    }
	}
	hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
	bin_term = erts_mk_magic_ref(&hp, &MSO(p), mb);
	BUMP_ALL_REDS(p);
	BIF_TRAP3(trapper, p, bin_term, epos,list);
    }
 badarg:
    BIF_ERROR(p,BADARG);
}

static BIF_RETTYPE do_longest_common_trap(Process *p, Eterm bin_term, Eterm current_pos,
					  Eterm orig_list, int direction)
{
    Uint reds = get_reds(p, COMMON_LOOP_FACTOR);
    Uint save_reds = reds;
    Uint pos;
    Binary *bin;
    CommonData *cd;
    int res;
    Eterm epos;
    Export *trapper;

#ifdef DEBUG
    int r;
    r = term_to_Uint(current_pos, &pos);
    ASSERT(r != 0);
#else
    term_to_Uint(current_pos, &pos);
#endif
    bin = erts_magic_ref2bin(bin_term);
    cd = (CommonData *) ERTS_MAGIC_BIN_DATA(bin);
    if (direction == DIRECTION_PREFIX) {
	trapper = &binary_longest_prefix_trap_export;
	res = do_search_forward(cd,&pos,&reds);
    } else {
	ASSERT(direction == DIRECTION_SUFFIX);
	trapper = &binary_longest_suffix_trap_export;
	res = do_search_backward(cd,&pos,&reds);
    }
    epos = erts_make_integer(pos,p);
    if (res == CL_OK) {
	BUMP_REDS(p, (save_reds - reds) / COMMON_LOOP_FACTOR);
	BIF_RET(epos);
    } else {
	ASSERT(res == CL_RESTART);
	/* Copy all heap binaries that are not already copied (aligned) */
	BUMP_ALL_REDS(p);
	BIF_TRAP3(trapper, p, bin_term, epos, orig_list);
    }
}

static BIF_RETTYPE binary_longest_prefix_trap(BIF_ALIST_3)
{
    return do_longest_common_trap(BIF_P,BIF_ARG_1,BIF_ARG_2,BIF_ARG_3,DIRECTION_PREFIX);
}

static BIF_RETTYPE binary_longest_suffix_trap(BIF_ALIST_3)
{
    return do_longest_common_trap(BIF_P,BIF_ARG_1,BIF_ARG_2,BIF_ARG_3,DIRECTION_SUFFIX);
}

BIF_RETTYPE binary_longest_common_prefix_1(BIF_ALIST_1)
{
    return do_longest_common(BIF_P,BIF_ARG_1,DIRECTION_PREFIX);
}

BIF_RETTYPE binary_longest_common_suffix_1(BIF_ALIST_1)
{
    return do_longest_common(BIF_P,BIF_ARG_1,DIRECTION_SUFFIX);
}

BIF_RETTYPE binary_first_1(BIF_ALIST_1)
{
    byte* bytes;
    Uint byte_size;
    Uint bit_offs;
    Uint bit_size;
    Uint res;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    byte_size = binary_size(BIF_ARG_1);
    if (!byte_size) {
	goto badarg;
    }
    ERTS_GET_BINARY_BYTES(BIF_ARG_1,bytes,bit_offs,bit_size);
    if (bit_size) {
	goto badarg;
    }
    if (bit_offs) {
	res = ((((Uint) bytes[0]) << bit_offs) | (((Uint) bytes[1]) >> (8-bit_offs))) & 0xFF;
    } else {
	res = bytes[0];
    }
    BIF_RET(make_small(res));
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

BIF_RETTYPE binary_last_1(BIF_ALIST_1)
{
    byte* bytes;
    Uint byte_size;
    Uint bit_offs;
    Uint bit_size;
    Uint res;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    byte_size = binary_size(BIF_ARG_1);
    if (!byte_size) {
	goto badarg;
    }
    ERTS_GET_BINARY_BYTES(BIF_ARG_1,bytes,bit_offs,bit_size);
    if (bit_size) {
	goto badarg;
    }
    if (bit_offs) {
	res = ((((Uint) bytes[byte_size-1]) << bit_offs) |
	       (((Uint) bytes[byte_size]) >> (8-bit_offs))) & 0xFF;
    } else {
	res = bytes[byte_size-1];
    }
    BIF_RET(make_small(res));
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

BIF_RETTYPE binary_at_2(BIF_ALIST_2)
{
    byte* bytes;
    Uint byte_size;
    Uint bit_offs;
    Uint bit_size;
    Uint res;
    Uint index;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    byte_size = binary_size(BIF_ARG_1);
    if (!byte_size) {
	goto badarg;
    }
    if (!term_to_Uint(BIF_ARG_2, &index)) {
	goto badarg;
    }
    if (index >= byte_size) {
	goto badarg;
    }
    ERTS_GET_BINARY_BYTES(BIF_ARG_1,bytes,bit_offs,bit_size);
    if (bit_size) {
	goto badarg;
    }
    if (bit_offs) {
	res = ((((Uint) bytes[index]) << bit_offs) |
	       (((Uint) bytes[index+1]) >> (8-bit_offs))) & 0xFF;
    } else {
	res = bytes[index];
    }
    BIF_RET(make_small(res));
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

HIPE_WRAPPER_BIF_DISABLE_GC(binary_list_to_bin, 1)

BIF_RETTYPE binary_list_to_bin_1(BIF_ALIST_1)
{
    return erts_list_to_binary_bif(BIF_P, BIF_ARG_1, bif_export[BIF_binary_list_to_bin_1]);
}

typedef struct {
    Uint times_left;
    Uint source_size;
    int source_type;
    byte *source;
    byte *temp_alloc;
    Uint result_pos;
    Binary *result;
} CopyBinState;

#define BC_TYPE_EMPTY 0
#define BC_TYPE_HEAP 1
#define BC_TYPE_ALIGNED 2 /* May or may not point to (emasculated) binary, temp_alloc field is set
			     so that erts_free_aligned_binary_bytes_extra can handle either */


#define BINARY_COPY_LOOP_FACTOR 100

static int cleanup_copy_bin_state(Binary *bp)
{
    CopyBinState *cbs = (CopyBinState *) ERTS_MAGIC_BIN_DATA(bp);
    if (cbs->result != NULL) {
	erts_bin_free(cbs->result);
	cbs->result = NULL;
    }
    switch (cbs->source_type) {
    case BC_TYPE_HEAP:
	erts_free(ERTS_ALC_T_BINARY_BUFFER,cbs->source);
	break;
    case BC_TYPE_ALIGNED:
	erts_free_aligned_binary_bytes_extra(cbs->temp_alloc,
					     ERTS_ALC_T_BINARY_BUFFER);
	break;
    default:
	/* otherwise do nothing */
	break;
    }
    cbs->source_type =  BC_TYPE_EMPTY;
    return 1;
}

/*
 * Binary *erts_bin_nrml_alloc(Uint size);
 * Binary *erts_bin_realloc(Binary *bp, Uint size);
 * void erts_bin_free(Binary *bp);
 */
static BIF_RETTYPE do_binary_copy(Process *p, Eterm bin, Eterm en)
{
    Uint n;
    byte *bytes;
    ERTS_DECLARE_DUMMY(Uint bit_offs);
    Uint bit_size;
    size_t size;
    Uint reds = get_reds(p, BINARY_COPY_LOOP_FACTOR);
    Uint target_size;
    byte *t;
    Uint pos;


    if (is_not_binary(bin)) {
	goto badarg;
    }
    if (!term_to_Uint(en, &n)) {
	goto badarg;
    }
    if (!n) {
	Eterm res_term = erts_new_heap_binary(p,NULL,0,&bytes);
	BIF_RET(res_term);
    }
    ERTS_GET_BINARY_BYTES(bin,bytes,bit_offs,bit_size);
    if (bit_size != 0) {
	goto badarg;
    }

    size = binary_size(bin);
    target_size = size * n;

    if ((target_size - size) >= reds) {
	Eterm orig;
	ERTS_DECLARE_DUMMY(Uint offset);
	ERTS_DECLARE_DUMMY(Uint bit_offset);
	ERTS_DECLARE_DUMMY(Uint bit_size);
	CopyBinState *cbs;
	Eterm *hp;
	Eterm trap_term;
	int i;

	/* We will trap, set up the structure for trapping right away */
	Binary *mb = erts_create_magic_binary(sizeof(CopyBinState),
					      cleanup_copy_bin_state);
	cbs = ERTS_MAGIC_BIN_DATA(mb);

	cbs->temp_alloc = NULL;
	cbs->source = NULL;

	ERTS_GET_REAL_BIN(bin, orig, offset, bit_offset, bit_size);
	if (*(binary_val(orig)) == HEADER_PROC_BIN) {
	    ProcBin* pb = (ProcBin *) binary_val(orig);
	    if (pb->flags) {
		erts_emasculate_writable_binary(pb);
	    }
	    cbs->source =
		erts_get_aligned_binary_bytes_extra(bin,
						    &(cbs->temp_alloc),
						    ERTS_ALC_T_BINARY_BUFFER,
						    0);
	    cbs->source_type = BC_TYPE_ALIGNED;
	} else { /* Heap binary */
	    cbs->source =
		erts_get_aligned_binary_bytes_extra(bin,
						    &(cbs->temp_alloc),
						    ERTS_ALC_T_BINARY_BUFFER,
						    0);
	    if (!(cbs->temp_alloc)) { /* alignment not needed, need to copy */
		byte *tmp = erts_alloc(ERTS_ALC_T_BINARY_BUFFER,size);
		sys_memcpy(tmp,cbs->source,size);
		cbs->source = tmp;
		cbs->source_type = BC_TYPE_HEAP;
	    } else {
		cbs->source_type = BC_TYPE_ALIGNED;
	    }
	}
	cbs->result = erts_bin_nrml_alloc(target_size); /* Always offheap
							   if trapping */
	t = (byte *) cbs->result->orig_bytes; /* No offset or anything */
	pos = 0;
	i = 0;
	while (pos < reds) {
	    sys_memcpy(t+pos,cbs->source, size);
	    pos += size;
	    ++i;
	}
	cbs->source_size = size;
	cbs->result_pos = pos;
	cbs->times_left = n-i;
	hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
	trap_term = erts_mk_magic_ref(&hp, &MSO(p), mb);
	BUMP_ALL_REDS(p);
	BIF_TRAP2(&binary_copy_trap_export, p, bin, trap_term);
    } else {
	Eterm res_term;
	byte *temp_alloc = NULL;
	byte *source =
	    erts_get_aligned_binary_bytes(bin,
					  &temp_alloc);
	if (target_size <= ERL_ONHEAP_BIN_LIMIT) {
	    res_term = erts_new_heap_binary(p,NULL,target_size,&t);
	} else {
	    res_term = erts_new_mso_binary(p,NULL,target_size);
	    t = ((ProcBin *) binary_val(res_term))->bytes;
	}
	pos = 0;
	while (pos < target_size) {
	    sys_memcpy(t+pos,source, size);
	    pos += size;
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	BUMP_REDS(p,pos / BINARY_COPY_LOOP_FACTOR);
	BIF_RET(res_term);
    }
 badarg:
    BIF_ERROR(p,BADARG);
}

BIF_RETTYPE binary_copy_trap(BIF_ALIST_2)
{
    Uint n;
    size_t size;
    Uint reds = get_reds(BIF_P, BINARY_COPY_LOOP_FACTOR);
    byte *t;
    Uint pos;
    Binary *mb = erts_magic_ref2bin(BIF_ARG_2);
    CopyBinState *cbs = (CopyBinState *) ERTS_MAGIC_BIN_DATA(mb);
    Uint opos;

    /* swapout... */
    n = cbs->times_left;
    size = cbs->source_size;
    opos = pos = cbs->result_pos;
    t = (byte *) cbs->result->orig_bytes; /* "well behaved" binary */
    if ((n-1) * size >= reds) {
	Uint i = 0;
	while ((pos - opos) < reds) {
	    sys_memcpy(t+pos,cbs->source, size);
	    pos += size;
	    ++i;
	}
	cbs->result_pos = pos;
	cbs->times_left -= i;
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP2(&binary_copy_trap_export, BIF_P, BIF_ARG_1, BIF_ARG_2);
    } else {
	Binary *save;
        Eterm resbin;
	Uint target_size = cbs->result->orig_size;
	while (pos < target_size) {
	    sys_memcpy(t+pos,cbs->source, size);
	    pos += size;
	}
	save = cbs->result;
	cbs->result = NULL;
	cleanup_copy_bin_state(mb); /* now cbs is dead */

        resbin = erts_build_proc_bin(&MSO(BIF_P),
                                     HAlloc(BIF_P, PROC_BIN_SIZE),
                                     save);
        BUMP_REDS(BIF_P,(pos - opos) / BINARY_COPY_LOOP_FACTOR);
	BIF_RET(resbin);
    }
}


BIF_RETTYPE binary_copy_1(BIF_ALIST_1)
{
    return do_binary_copy(BIF_P,BIF_ARG_1,make_small(1));
}

BIF_RETTYPE binary_copy_2(BIF_ALIST_2)
{
    return do_binary_copy(BIF_P,BIF_ARG_1,BIF_ARG_2);
}

BIF_RETTYPE binary_referenced_byte_size_1(BIF_ALIST_1)
{
    ErlSubBin *sb;
    ProcBin *pb;
    Eterm res;
    Eterm bin = BIF_ARG_1;

    if (is_not_binary(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    sb = (ErlSubBin *) binary_val(bin);
    if (sb->thing_word == HEADER_SUB_BIN) {
	bin = sb->orig;
    }
    pb = (ProcBin *) binary_val(bin);
    if (pb->thing_word == HEADER_PROC_BIN) {
	res = erts_make_integer((Uint) pb->val->orig_size, BIF_P);
    } else { /* heap binary */
	res = erts_make_integer((Uint) ((ErlHeapBin *) pb)->size, BIF_P);
    }
    BIF_RET(res);
}

#define END_BIG 0
#define END_SMALL 1

#ifdef WORDS_BIGENDIAN
#define END_NATIVE END_BIG
#else
#define END_NATIVE END_SMALL
#endif

static int get_need(Uint u) {
#if defined(ARCH_64)
    if (u > 0xFFFFFFFFUL) {
	if (u > 0xFFFFFFFFFFFFUL) {
	    if (u > 0xFFFFFFFFFFFFFFUL) {
		return 8;
	    }
	    return 7;
	}
	if (u > 0xFFFFFFFFFFUL) {
	    return 6;
	}
	return 5;
    }
#endif
    if (u > 0xFFFFUL) {
	if (u > 0xFFFFFFUL) {
	    return 4;
	}
	return 3;
    }
    if (u > 0xFFUL) {
	return 2;
    }
    return 1;
}

static BIF_RETTYPE do_encode_unsigned(Process *p, Eterm uns, Eterm endianess)
{
    Eterm res;
    if ((is_not_small(uns) && is_not_big(uns)) || is_not_atom(endianess) ||
	(endianess != am_big && endianess != am_little)) {
	goto badarg;
    }
    if (is_small(uns)) {
	Sint x = signed_val(uns);
	Uint u;
	int n,i;
	byte *b;

	if (x < 0) {
	    goto badarg;
	}

	u = (Uint) x;
	n = get_need(u);
	ASSERT(n <= ERL_ONHEAP_BIN_LIMIT);
	res = erts_new_heap_binary(p, NULL, n, &b);
	if (endianess == am_big) {
	    for(i=n-1;i>=0;--i) {
		b[i] = u & 0xFF;
		u >>= 8;
	    }
	} else {
	    for(i=0;i<n;++i) {
		b[i] = u & 0xFF;
		u >>= 8;
	    }
	}
	BIF_RET(res);
    } else {
	/* Big */
	Eterm *bigp = big_val(uns);
	Uint n;
	dsize_t num_parts = BIG_SIZE(bigp);
	Eterm res;
	byte *b;
	ErtsDigit d;

	if(BIG_SIGN(bigp)) {
	    goto badarg;
	}
	n = (num_parts-1)*sizeof(ErtsDigit)+get_need(BIG_DIGIT(bigp,(num_parts-1)));
	if (n <= ERL_ONHEAP_BIN_LIMIT) {
	    res = erts_new_heap_binary(p,NULL,n,&b);
	} else {
	    res = erts_new_mso_binary(p,NULL,n);
	    b = ((ProcBin *) binary_val(res))->bytes;
	}

	if (endianess == am_big) {
	    Sint i,j;
	    j = 0;
	    d = BIG_DIGIT(bigp,0);
	    for (i=n-1;i>=0;--i) {
		b[i] = d & 0xFF;
		if (!((++j) % sizeof(ErtsDigit))) {
		    d = BIG_DIGIT(bigp,j / sizeof(ErtsDigit));
		} else {
		    d >>= 8;
		}
	    }
	} else {
	    Sint i,j;
	    j = 0;
	    d = BIG_DIGIT(bigp,0);
	    for (i=0;i<n;++i) {
		b[i] = d & 0xFF;
		if (!((++j) % sizeof(ErtsDigit))) {
		    d = BIG_DIGIT(bigp,j / sizeof(ErtsDigit));
		} else {
		    d >>= 8;
		}
	    }

	}
	BIF_RET(res);
    }
 badarg:
    BIF_ERROR(p,BADARG);
}

static BIF_RETTYPE do_decode_unsigned(Process *p, Eterm uns, Eterm endianess)
{
    byte *bytes;
    Uint bitoffs, bitsize;
    Uint size;
    Eterm res;

    if (is_not_binary(uns) || is_not_atom(endianess) ||
	(endianess != am_big && endianess != am_little)) {
	goto badarg;
    }
    ERTS_GET_BINARY_BYTES(uns, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	goto badarg;
    }
    /* align while rolling */
    size = binary_size(uns);
    if (bitoffs) {
	if (endianess == am_big) {
	    while (size && (((((Uint) bytes[0]) << bitoffs) |
			    (((Uint) bytes[1]) >> (8-bitoffs))) & 0xFF) == 0) {
		++bytes;
		--size;
	    }
	} else {
	    while(size &&
		  (((((Uint) bytes[size-1]) << bitoffs) |
		    (((Uint) bytes[size]) >> (8-bitoffs))) & 0xFF) == 0) {
		--size;
	    }
	}
    } else {
	if (endianess == am_big) {
	    while (size && *bytes == 0) {
		++bytes;
		--size;
	    }
	} else {
	    while(size && bytes[size-1] == 0) {
		--size;
	    }
	}
    }
    if (!size) {
	BIF_RET(make_small(0));
    }

    if (size <= sizeof(Uint)) {
	Uint u = 0;
	Sint i;

	if (endianess == am_big) {
		if (bitoffs) {
		    for(i=0;i<size;++i) {
			u <<=8;
			u |= (((((Uint) bytes[i]) << bitoffs) |
			       (((Uint) bytes[i+1]) >> (8-bitoffs))) & 0xFF);
		    }
		} else {
		    for(i=0;i<size;++i) {
			u <<=8;
			u |= bytes[i];
		    }
		}
	} else {

		if (bitoffs) {
		    for(i=size-1;i>=0;--i) {
			u <<=8;
			u |= (((((Uint) bytes[i]) << bitoffs) |
			       (((Uint) bytes[i+1]) >> (8-bitoffs))) & 0xFF);
		    }
		} else {
		    for(i=size-1;i>=0;--i) {
			u <<=8;
			u |= bytes[i];
		    }
		}
	}
	res = erts_make_integer(u,p);
	BIF_RET(res);
    } else {
	/* Assume big, as we stripped away all zeroes from the MSB part of the binary */
	dsize_t num_parts = size / sizeof(ErtsDigit) + !!(size % sizeof(ErtsDigit));
	Eterm *bigp;

	bigp = HAlloc(p, BIG_NEED_SIZE(num_parts));
	*bigp = make_pos_bignum_header(num_parts);
	res = make_big(bigp);

	if (endianess == am_big) {
	    Sint i,j;
	    ErtsDigit *d;
	    j = size;
	    d = &(BIG_DIGIT(bigp,num_parts - 1));
	    *d = 0;
	    i = 0;
	    if(bitoffs) {
		for (;;){
		    (*d) <<= 8;
		    (*d) |= (((((Uint) bytes[i]) << bitoffs) |
			      (((Uint) bytes[i+1]) >> (8-bitoffs))) & 0xFF);
		    if (++i >= size) {
			break;
		    }
		    if (!(--j % sizeof(ErtsDigit))) {
			--d;
			*d = 0;
		    }
		}
	    } else {
		for (;;){
		    (*d) <<= 8;
		    (*d) |= bytes[i];
		    if (++i >= size) {
			break;
		    }
		    if (!(--j % sizeof(ErtsDigit))) {
			--d;
			*d = 0;
		    }
		}
	    }
	} else {
	    Sint i,j;
	    ErtsDigit *d;
	    j = size;
	    d = &(BIG_DIGIT(bigp,num_parts - 1));
	    *d = 0;
	    i = size-1;
	    if (bitoffs) {
		for (;;){
		    (*d) <<= 8;
		    (*d) |= (((((Uint) bytes[i]) << bitoffs) |
			      (((Uint) bytes[i+1]) >> (8-bitoffs))) & 0xFF);
		    if (--i < 0) {
			break;
		    }
		    if (!(--j % sizeof(ErtsDigit))) {
			--d;
			*d = 0;
		    }
		}
	    } else {
		for (;;){
		    (*d) <<= 8;
		    (*d) |= bytes[i];
		    if (--i < 0) {
			break;
		    }
		    if (!(--j % sizeof(ErtsDigit))) {
			--d;
			*d = 0;
		    }
		}
	    }
	}
	BIF_RET(res);
    }
 badarg:
    BIF_ERROR(p,BADARG);
}

BIF_RETTYPE binary_encode_unsigned_1(BIF_ALIST_1)
{
    return do_encode_unsigned(BIF_P,BIF_ARG_1,am_big);
}

BIF_RETTYPE binary_encode_unsigned_2(BIF_ALIST_2)
{
    return do_encode_unsigned(BIF_P,BIF_ARG_1,BIF_ARG_2);
}

BIF_RETTYPE binary_decode_unsigned_1(BIF_ALIST_1)
{
    return do_decode_unsigned(BIF_P,BIF_ARG_1,am_big);
}

BIF_RETTYPE binary_decode_unsigned_2(BIF_ALIST_2)
{
    return do_decode_unsigned(BIF_P,BIF_ARG_1,BIF_ARG_2);
}

/*
 * Hard debug functions (dump) for the search structures
 */

#ifdef HARDDEBUG
static void dump_bm_data(BMData *bm)
{
    int i,j;
    erts_printf("Dumping Boyer-Moore structure.\n");
    erts_printf("=============================\n");
    erts_printf("Searchstring [%ld]:\n", bm->len);
    erts_printf("<<");
    for (i = 0; i < bm->len; ++i) {
	if (i > 0) {
	    erts_printf(", ");
	}
	erts_printf("%d", (int) bm->x[i]);
	if (bm->x[i] >= 'A') {
	    erts_printf(" ($%c)",(char) bm->x[i]);
	}
    }
    erts_printf(">>\n");
    erts_printf("GoodShift array:\n");
    for (i = 0; i < bm->len; ++i) {
	erts_printf("GoodShift[%d]: %ld\n", i, bm->goodshift[i]);
    }
    erts_printf("BadShift array:\n");
    j = 0;
    for (i = 0; i < ALPHABET_SIZE; i += j) {
	for (j = 0; i + j < ALPHABET_SIZE && j < 6; ++j) {
	    erts_printf("BS[%03d]:%02ld, ", i+j, bm->badshift[i+j]);
	}
	erts_printf("\n");
    }
}

static void dump_ac_node(ACNode *node, int indent, int ch) {
    int i;
    char *spaces = erts_alloc(ERTS_ALC_T_TMP, 10 * indent + 1);
    sys_memset(spaces,' ',10*indent);
    spaces[10*indent] = '\0';
    erts_printf("%s-> %c\n",spaces,ch);
    erts_printf("%sId: %u\n",spaces,(unsigned) node->id);
    erts_printf("%sD: %u\n",spaces,(unsigned)node->d);
    erts_printf("%sFinal: %d\n",spaces,(int)node->final);
    erts_printf("%sFail: %u\n",spaces,(unsigned)node->h->id);
    erts_free(ERTS_ALC_T_TMP,spaces);
    for(i=0;i<ALPHABET_SIZE;++i) {
	if (node->g[i] != NULL && node->g[i] != node) {
	    dump_ac_node(node->g[i],indent+1,i);
	}
    }
}


static void dump_ac_trie(ACTrie *act)
{
    erts_printf("Aho Corasick Trie dump.\n");
    erts_printf("=======================\n");
    erts_printf("Node counter: %u\n", (unsigned) act->idc);
    erts_printf("Searchstring counter: %u\n", (unsigned) act->counter);
    erts_printf("Trie:\n");
    dump_ac_node(act->root, 0, '0');
    return;
}
#endif
