/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
#include "bif.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"


/*
 * The native implementation functions for the module binary.
 * Searching is implemented using aither Boyer-More or Aho-Corasick
 * depending on number of searchstrings (BM if one, AC if more than one).
 * Native implementation is mostly for efficiency, nothing
 * (except binary:referenced_byte_size) really *needs* to be implemented
 * in native code.
 */

/* #define HARDDEBUG */

/* Init and local variables */

static Export binary_match_trap_export;
static BIF_RETTYPE binary_match_trap(BIF_ALIST_3);
static Export binary_matches_trap_export;
static BIF_RETTYPE binary_matches_trap(BIF_ALIST_3);
static Export binary_longest_prefix_trap_export;
static BIF_RETTYPE binary_longest_prefix_trap(BIF_ALIST_3);
static Export binary_longest_suffix_trap_export;
static BIF_RETTYPE binary_longest_suffix_trap(BIF_ALIST_3);
static Uint max_loop_limit;


void erts_init_bif_binary(void)
{
    sys_memset((void *) &binary_match_trap_export, 0, sizeof(Export));
    binary_match_trap_export.address = &binary_match_trap_export.code[3];
    binary_match_trap_export.code[0] = am_erlang;
    binary_match_trap_export.code[1] = am_binary_match_trap;
    binary_match_trap_export.code[2] = 3;
    binary_match_trap_export.code[3] = (BeamInstr) em_apply_bif;
    binary_match_trap_export.code[4] = (BeamInstr) &binary_match_trap;

    sys_memset((void *) &binary_matches_trap_export, 0, sizeof(Export));
    binary_matches_trap_export.address = &binary_matches_trap_export.code[3];
    binary_matches_trap_export.code[0] = am_erlang;
    binary_matches_trap_export.code[1] = am_binary_matches_trap;
    binary_matches_trap_export.code[2] = 3;
    binary_matches_trap_export.code[3] = (BeamInstr) em_apply_bif;
    binary_matches_trap_export.code[4] = (BeamInstr) &binary_matches_trap;

    sys_memset((void *) &binary_longest_prefix_trap_export, 0, sizeof(Export));
    binary_longest_prefix_trap_export.address = &binary_longest_prefix_trap_export.code[3];
    binary_longest_prefix_trap_export.code[0] = am_erlang;
    binary_longest_prefix_trap_export.code[1] = am_binary_longest_prefix_trap;
    binary_longest_prefix_trap_export.code[2] = 3;
    binary_longest_prefix_trap_export.code[3] = (BeamInstr) em_apply_bif;
    binary_longest_prefix_trap_export.code[4] = (BeamInstr) &binary_longest_prefix_trap;

    sys_memset((void *) &binary_longest_suffix_trap_export, 0, sizeof(Export));
    binary_longest_suffix_trap_export.address = &binary_longest_suffix_trap_export.code[3];
    binary_longest_suffix_trap_export.code[0] = am_erlang;
    binary_longest_suffix_trap_export.code[1] = am_binary_longest_suffix_trap;
    binary_longest_suffix_trap_export.code[2] = 3;
    binary_longest_suffix_trap_export.code[3] = (BeamInstr) em_apply_bif;
    binary_longest_suffix_trap_export.code[4] = (BeamInstr) &binary_longest_suffix_trap;

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
    return reds;
}

/*
 * A micro allocator used when building search structures, just a convenience
 * for building structures inside a pre alocated magic binary using
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


#ifndef MAX
#define MAX(A,B) (((A) > (B)) ? (A) : B)
#endif

/*
 * Callback for the magic binary
 */
static void cleanup_my_data(Binary *bp)
{
    return;
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
    Binary *mb = erts_create_magic_binary(datasize,cleanup_my_data);
    byte *data = ERTS_MAGIC_BIN_DATA(mb);

    init_my_allocator(my, datasize, data);
    act = my_alloc(my, sizeof(ACTrie)); /* Important that this is the first
					   allocation */
    act->counter = 0;
    act->root = acn = my_alloc(my, sizeof(ACNode));
    acn->d = 0;
    acn->final = 0;
    acn->h = NULL;
    memset(acn->g, 0, sizeof(ACNode *) * ALPHABET_SIZE);
#ifdef HARDDEBUG
    act->idc = 0;
    acn->id = 0;
#endif
    *qbuff = erts_alloc(ERTS_ALC_T_TMP, sizeof(ACNode *) * len);
    *the_bin = mb;
    return act;
}

/*
 * The same initialization of allocator and basic data for Boyer-More.
 */
static BMData *create_bmdata(MyAllocator *my, byte *x, Uint len,
			     Binary **the_bin /* out */)
{
    Uint datasize = BM_SIZE(len);
    BMData *bmd;
    Binary *mb = erts_create_magic_binary(datasize,cleanup_my_data);
    byte *data = ERTS_MAGIC_BIN_DATA(mb);
    init_my_allocator(my, datasize, data);
    bmd = my_alloc(my, sizeof(BMData));
    bmd->x = my_alloc(my,len);
    memcpy(bmd->x,x,len);
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
    Uint32 n = ++act->counter; /* Always increase conter, even if it's a
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
	    memset(nn->g, 0, sizeof(ACNode *) * ALPHABET_SIZE);
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
		/* Search for correct failure function, follow the parents
		   failure function until you find a similar transition
		   funtion to this childs */
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
typedef struct {
    ACNode *q;
    Uint pos;
    Uint len;
    ACNode *candidate;
    Uint candidate_start;
} ACFindFirstState;


static void ac_init_find_first_match(ACFindFirstState *state, ACTrie *act, Sint startpos, Uint len)
{
    state->q = act->root;
    state->pos = startpos;
    state->len = len;
    state->candidate = NULL;
    state->candidate_start = 0;
}
#define AC_OK 0
#define AC_NOT_FOUND -1
#define AC_RESTART -2

#define AC_LOOP_FACTOR 10

static int ac_find_first_match(ACFindFirstState *state, byte *haystack,
				Uint *mpos, Uint *mlen, Uint *reductions)
{
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
	    return AC_RESTART;
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
	return AC_NOT_FOUND;
    }
#ifdef HARDDEBUG
    dump_ac_node(candidate,0,'?');
#endif
    *mpos = candidate_start;
    *mlen = candidate->d;
    return AC_OK;
}

typedef struct _findall_data {
    Uint pos;
    Uint len;
#ifdef HARDDEBUG
    Uint id;
#endif
    Eterm epos;
    Eterm elen;
} FindallData;

typedef struct {
    ACNode *q;
    Uint pos;
    Uint len;
    Uint m;
    Uint allocated;
    FindallData *out;
} ACFindAllState;

static void ac_init_find_all(ACFindAllState *state, ACTrie *act, Sint startpos, Uint len)
{
    state->q = act->root;
    state->pos = startpos;
    state->len = len;
    state->m = 0;
    state->allocated = 0;
    state->out = NULL;
}

static void ac_restore_find_all(ACFindAllState *state, char *buff)
{
    memcpy(state,buff,sizeof(ACFindAllState));
    state->out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) * (state->allocated));
    memcpy(state->out,buff+sizeof(ACFindAllState),sizeof(FindallData)*state->m);
}

static void ac_serialize_find_all(ACFindAllState *state, char *buff)
{
    memcpy(buff,state,sizeof(ACFindAllState));
    memcpy(buff+sizeof(ACFindAllState),state->out,sizeof(FindallData)*state->m);
}

static void ac_clean_find_all(ACFindAllState *state)
{
    if (state->out != NULL) {
	erts_free(ERTS_ALC_T_TMP, state->out);
    }
#ifdef HARDDEBUG
    state->out = NULL;
    state->allocated = 0;
#endif
}

#define SIZEOF_AC_SERIALIZED_FIND_ALL_STATE(S) \
          (sizeof(ACFindAllState)+(sizeof(FindallData)*(S).m))

/*
 * Differs to the find_first function in that it stores all matches and the values
 * arte returned only in the state.
 */
static int ac_find_all_non_overlapping(ACFindAllState *state, byte *haystack,
				       Uint *reductions)
{
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
	    return AC_RESTART;
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
			    out = erts_alloc(ERTS_ALC_T_TMP,
					     sizeof(FindallData) * allocated);
			} else {
			    allocated *= 2;
			    out = erts_realloc(ERTS_ALC_T_TMP, out,
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
    return (m == 0) ? AC_NOT_FOUND : AC_OK;
}

/*
 * Boyer More - most obviously implemented more or less exactly as
 * Christian Charras and Thierry Lecroq describes it in "Handbook of
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
	if (i > g && suffixes[i + m - f] < i - g) {
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
    Sint *suffixes = erts_alloc(ERTS_ALC_T_TMP, m * sizeof(Uint));

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

typedef struct {
    Sint pos;
    Sint len;
} BMFindFirstState;

#define BM_OK 0 /* used only for find_all */
#define BM_NOT_FOUND -1
#define BM_RESTART -2
#define BM_LOOP_FACTOR 10 /* Should we have a higher value? */

static void bm_init_find_first_match(BMFindFirstState *state, Sint startpos,
				     Uint len)
{
    state->pos = startpos;
    state->len = (Sint) len;
}


static Sint bm_find_first_match(BMFindFirstState *state, BMData *bmd,
				byte *haystack, Uint *reductions)
{
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
	    return BM_RESTART;
	}
	for (i = blen - 1; i >= 0 && needle[i] == haystack[i + j]; --i)
	    ;
	if (i < 0) { /* found */
	    *reductions = reds;
	    return j;
	}
	j += MAX(gs[i],bs[haystack[i+j]] - blen + 1 + i);
    }
    *reductions = reds;
    return BM_NOT_FOUND;
}

typedef struct {
    Sint pos;
    Sint len;
    Uint m;
    Uint allocated;
    FindallData *out;
} BMFindAllState;

static void bm_init_find_all(BMFindAllState *state, Sint startpos, Uint len)
{
    state->pos = startpos;
    state->len = (Sint) len;
    state->m = 0;
    state->allocated = 0;
    state->out = NULL;
}

static void bm_restore_find_all(BMFindAllState *state, char *buff)
{
    memcpy(state,buff,sizeof(BMFindAllState));
    state->out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) *
			    (state->allocated));
    memcpy(state->out,buff+sizeof(BMFindAllState),
	   sizeof(FindallData)*state->m);
}

static void bm_serialize_find_all(BMFindAllState *state, char *buff)
{
    memcpy(buff,state,sizeof(BMFindAllState));
    memcpy(buff+sizeof(BMFindAllState),state->out,
	   sizeof(FindallData)*state->m);
}

static void bm_clean_find_all(BMFindAllState *state)
{
    if (state->out != NULL) {
	erts_free(ERTS_ALC_T_TMP, state->out);
    }
#ifdef HARDDEBUG
    state->out = NULL;
    state->allocated = 0;
#endif
}

#define SIZEOF_BM_SERIALIZED_FIND_ALL_STATE(S) \
          (sizeof(BMFindAllState)+(sizeof(FindallData)*(S).m))

/*
 * Differs to the find_first function in that it stores all matches and the
 * values are returned only in the state.
 */
static Sint bm_find_all_non_overlapping(BMFindAllState *state,
					BMData *bmd, byte *haystack,
					Uint *reductions)
{
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
	    return BM_RESTART;
	}
	for (i = blen - 1; i >= 0 && needle[i] == haystack[i + j]; --i)
	    ;
	if (i < 0) { /* found */
	    if (m >= allocated) {
		if (!allocated) {
		    allocated = 10;
		    out = erts_alloc(ERTS_ALC_T_TMP, sizeof(FindallData) * allocated);
		} else {
		    allocated *= 2;
		    out = erts_realloc(ERTS_ALC_T_TMP, out,
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
    return (m == 0) ? BM_NOT_FOUND : BM_OK;
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
    hp = HAlloc(BIF_P, PROC_BIN_SIZE+3);
    ret = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), bin);
    ret = TUPLE2(hp, tag, ret);
    BIF_RET(ret);
}

#define DO_BIN_MATCH_OK 0
#define DO_BIN_MATCH_BADARG -1
#define DO_BIN_MATCH_RESTART -2

static int do_binary_match(Process *p, Eterm subject, Uint hsstart, Uint hsend,
			   Eterm type, Binary *bin, Eterm state_term,
			   Eterm *res_term)
{
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
    if (state_term != NIL) {
	Eterm *ptr = big_val(state_term);
	type = ptr[1];
    }

    if (type == am_bm) {
	BMData *bm;
	Sint pos;
	Eterm ret;
	Eterm *hp;
	BMFindFirstState state;
	Uint reds = get_reds(p, BM_LOOP_FACTOR);
	Uint save_reds = reds;

	bm = (BMData *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_bm_data(bm);
#endif
	if (state_term == NIL) {
	    bm_init_find_first_match(&state, hsstart, hsend);
	} else {
	    Eterm *ptr = big_val(state_term);
	    memcpy(&state,ptr+2,sizeof(state));
	}
#ifdef HARDDEBUG
	erts_printf("(bm) state->pos = %ld, state->len = %lu\n",state.pos,
		    state.len);
#endif
	pos = bm_find_first_match(&state, bm, bytes, &reds);
	if (pos == BM_NOT_FOUND) {
	    ret = am_nomatch;
	} else if (pos == BM_RESTART) {
	    int x = (sizeof(BMFindFirstState) / sizeof(Eterm)) +
		!!(sizeof(BMFindFirstState) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap bm!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    memcpy(hp+2,&state,sizeof(state));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    Eterm erlen = erts_make_integer((Uint) bm->len, p);
	    ret = erts_make_integer(pos,p);
	    hp = HAlloc(p,3);
	    ret = TUPLE2(hp, ret, erlen);
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	BUMP_REDS(p, (save_reds - reds) / BM_LOOP_FACTOR);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    } else if (type == am_ac) {
	ACTrie *act;
	Uint pos, rlen;
	int acr;
	ACFindFirstState state;
	Eterm ret;
	Eterm *hp;
	Uint reds = get_reds(p, AC_LOOP_FACTOR);
	Uint save_reds = reds;

	act = (ACTrie *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_ac_trie(act);
#endif
	if (state_term == NIL) {
	    ac_init_find_first_match(&state, act, hsstart, hsend);
	} else {
	    Eterm *ptr = big_val(state_term);
	    memcpy(&state,ptr+2,sizeof(state));
	}
	acr = ac_find_first_match(&state, bytes, &pos, &rlen, &reds);
	if (acr == AC_NOT_FOUND) {
	    ret = am_nomatch;
	} else if (acr == AC_RESTART) {
	    int x = (sizeof(state) / sizeof(Eterm)) +
		!!(sizeof(BMFindFirstState) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap ac!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    memcpy(hp+2,&state,sizeof(state));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    Eterm epos = erts_make_integer(pos+hsstart,p);
	    Eterm erlen = erts_make_integer(rlen,p);
	    hp = HAlloc(p,3);
	    ret = TUPLE2(hp, epos, erlen);
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	BUMP_REDS(p, (save_reds - reds) / AC_LOOP_FACTOR);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    }
 badarg:
    return DO_BIN_MATCH_BADARG;
}

static int do_binary_matches(Process *p, Eterm subject, Uint hsstart,
			     Uint hsend, Eterm type, Binary *bin,
			     Eterm state_term, Eterm *res_term)
{
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
    if (state_term != NIL) {
	Eterm *ptr = big_val(state_term);
	type = ptr[1];
    }

    if (type == am_bm) {
	BMData *bm;
	Sint pos;
	Eterm ret,tpl;
	Eterm *hp;
	BMFindAllState state;
	Uint reds = get_reds(p, BM_LOOP_FACTOR);
	Uint save_reds = reds;

	bm = (BMData *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_bm_data(bm);
#endif
	if (state_term == NIL) {
	    bm_init_find_all(&state, hsstart, hsend);
	} else {
	    Eterm *ptr = big_val(state_term);
	    bm_restore_find_all(&state,(char *) (ptr+2));
	}

	pos = bm_find_all_non_overlapping(&state, bm, bytes, &reds);
	if (pos == BM_NOT_FOUND) {
	    ret = NIL;
	} else if (pos == BM_RESTART) {
	    int x =
		(SIZEOF_BM_SERIALIZED_FIND_ALL_STATE(state) / sizeof(Eterm)) +
		!!(SIZEOF_BM_SERIALIZED_FIND_ALL_STATE(state) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap bm!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    bm_serialize_find_all(&state, (char *) (hp+2));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    bm_clean_find_all(&state);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    FindallData *fad = state.out;
	    int i;
	    for (i = 0; i < state.m; ++i) {
		fad[i].epos = erts_make_integer(fad[i].pos,p);
		fad[i].elen = erts_make_integer(fad[i].len,p);
	    }
	    hp = HAlloc(p,state.m * (3 + 2));
	    ret = NIL;
	    for (i = state.m - 1; i >= 0; --i) {
		tpl = TUPLE2(hp, fad[i].epos, fad[i].elen);
		hp +=3;
		ret = CONS(hp,tpl,ret);
		hp += 2;
	    }
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	bm_clean_find_all(&state);
	BUMP_REDS(p, (save_reds - reds) / BM_LOOP_FACTOR);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    } else if (type == am_ac) {
	ACTrie *act;
	int acr;
	ACFindAllState state;
	Eterm ret,tpl;
	Eterm *hp;
	Uint reds = get_reds(p, AC_LOOP_FACTOR);
	Uint save_reds = reds;

	act = (ACTrie *) ERTS_MAGIC_BIN_DATA(bin);
#ifdef HARDDEBUG
	dump_ac_trie(act);
#endif
	if (state_term == NIL) {
	    ac_init_find_all(&state, act, hsstart, hsend);
	} else {
	    Eterm *ptr = big_val(state_term);
	    ac_restore_find_all(&state,(char *) (ptr+2));
	}
	acr = ac_find_all_non_overlapping(&state, bytes, &reds);
	if (acr == AC_NOT_FOUND) {
	    ret = NIL;
	} else if (acr == AC_RESTART) {
	    int x =
		(SIZEOF_AC_SERIALIZED_FIND_ALL_STATE(state) / sizeof(Eterm)) +
		!!(SIZEOF_AC_SERIALIZED_FIND_ALL_STATE(state) % sizeof(Eterm));
#ifdef HARDDEBUG
	    erts_printf("Trap ac!\n");
#endif
	    hp = HAlloc(p,x+2);
	    hp[0] = make_pos_bignum_header(x+1);
	    hp[1] = type;
	    ac_serialize_find_all(&state, (char *) (hp+2));
	    *res_term = make_big(hp);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    ac_clean_find_all(&state);
	    return DO_BIN_MATCH_RESTART;
	} else {
	    FindallData *fad = state.out;
	    int i;
	    for (i = 0; i < state.m; ++i) {
		fad[i].epos = erts_make_integer(fad[i].pos,p);
		fad[i].elen = erts_make_integer(fad[i].len,p);
	    }
	    hp = HAlloc(p,state.m * (3 + 2));
	    ret = NIL;
	    for (i = state.m - 1; i >= 0; --i) {
		tpl = TUPLE2(hp, fad[i].epos, fad[i].elen);
		hp +=3;
		ret = CONS(hp,tpl,ret);
		hp += 2;
	    }
	}
	erts_free_aligned_binary_bytes(temp_alloc);
	ac_clean_find_all(&state);
	BUMP_REDS(p, (save_reds - reds) / AC_LOOP_FACTOR);
	*res_term = ret;
	return DO_BIN_MATCH_OK;
    }
 badarg:
    return DO_BIN_MATCH_BADARG;
}

static int parse_match_opts_list(Eterm l, Eterm bin, Uint *posp, Uint *endp)
{
    Eterm *tp;
    Uint pos;
    Sint len;
    if (l == ((Eterm) 0) || l == NIL) {
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
		Sint lentmp = -len;
		if (-lentmp != len) {
		    goto badarg;
		}
		len = lentmp;
		pos -= len;
	    }
	    if (((pos + len) - len) != pos) {
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

static BIF_RETTYPE binary_match_trap(BIF_ALIST_3)
{
    int runres;
    Eterm result;
    Binary *bin = ((ProcBin *) binary_val(BIF_ARG_3))->val;
    runres = do_binary_match(BIF_P,BIF_ARG_1,0,0,NIL,bin,BIF_ARG_2,&result);
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_match_trap_export, BIF_P, BIF_ARG_1, result,
		  BIF_ARG_3);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

static BIF_RETTYPE binary_matches_trap(BIF_ALIST_3)
{
    int runres;
    Eterm result;
    Binary *bin = ((ProcBin *) binary_val(BIF_ARG_3))->val;
    runres = do_binary_matches(BIF_P,BIF_ARG_1,0,0,NIL,bin,BIF_ARG_2,&result);
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_matches_trap_export, BIF_P, BIF_ARG_1, result,
		  BIF_ARG_3);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}


BIF_RETTYPE binary_match_3(BIF_ALIST_3)
{
    Uint hsstart;
    Uint hsend;
    Eterm *tp;
    Eterm type;
    Binary *bin;
    Eterm bin_term = NIL;
    int runres;
    Eterm result;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    if (parse_match_opts_list(BIF_ARG_3,BIF_ARG_1,&hsstart,&hsend)) {
	goto badarg;
    }
    if (hsend == 0) {
	BIF_RET(am_nomatch);
    }
    if (is_tuple(BIF_ARG_2)) {
	tp = tuple_val(BIF_ARG_2);
	if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
	    goto badarg;
	}
	if (((tp[1] != am_bm) && (tp[1] != am_ac)) ||
	    !ERTS_TERM_IS_MAGIC_BINARY(tp[2])) {
	    goto badarg;
	}
	type = tp[1];
	bin = ((ProcBin *) binary_val(tp[2]))->val;
	if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != cleanup_my_data) {
	    goto badarg;
	}
	bin_term = tp[2];
    } else if (do_binary_match_compile(BIF_ARG_2,&type,&bin)) {
	goto badarg;
    }
    runres = do_binary_match(BIF_P,BIF_ARG_1,hsstart,hsend,type,bin,NIL,&result);
    if (runres == DO_BIN_MATCH_RESTART && bin_term == NIL) {
	Eterm *hp = HAlloc(BIF_P, PROC_BIN_SIZE);
	bin_term = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), bin);
    } else if (bin_term == NIL) {
	erts_bin_free(bin);
    }
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_match_trap_export, BIF_P, BIF_ARG_1, result, bin_term);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}

BIF_RETTYPE binary_matches_3(BIF_ALIST_3)
{
    Uint hsstart, hsend;
    Eterm *tp;
    Eterm type;
    Binary *bin;
    Eterm bin_term = NIL;
    int runres;
    Eterm result;

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    if (parse_match_opts_list(BIF_ARG_3,BIF_ARG_1,&hsstart,&hsend)) {
	goto badarg;
    }
    if (hsend == 0) {
	BIF_RET(am_nomatch);
    }
    if (is_tuple(BIF_ARG_2)) {
	tp = tuple_val(BIF_ARG_2);
	if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
	    goto badarg;
	}
	if (((tp[1] != am_bm) && (tp[1] != am_ac)) ||
	    !ERTS_TERM_IS_MAGIC_BINARY(tp[2])) {
	    goto badarg;
	}
	type = tp[1];
	bin = ((ProcBin *) binary_val(tp[2]))->val;
	if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != cleanup_my_data) {
	    goto badarg;
	}
	bin_term = tp[2];
    } else if (do_binary_match_compile(BIF_ARG_2,&type,&bin)) {
	goto badarg;
    }
    runres = do_binary_matches(BIF_P,BIF_ARG_1,hsstart,hsend,type,bin,
			       NIL,&result);
    if (runres == DO_BIN_MATCH_RESTART && bin_term == NIL) {
	Eterm *hp = HAlloc(BIF_P, PROC_BIN_SIZE);
	bin_term = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), bin);
    } else if (bin_term == NIL) {
	erts_bin_free(bin);
    }
    switch (runres) {
    case DO_BIN_MATCH_OK:
	BIF_RET(result);
    case DO_BIN_MATCH_RESTART:
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&binary_matches_trap_export, BIF_P, BIF_ARG_1, result,
		  bin_term);
    default:
	goto badarg;
    }
 badarg:
    BIF_ERROR(BIF_P,BADARG);
}


BIF_RETTYPE binary_match_2(BIF_ALIST_2)
{
    return binary_match_3(BIF_P,BIF_ARG_1,BIF_ARG_2,((Eterm) 0));
}


BIF_RETTYPE binary_matches_2(BIF_ALIST_2)
{
    return binary_matches_3(BIF_P,BIF_ARG_1,BIF_ARG_2,((Eterm) 0));
}

BIF_RETTYPE binary_part_3(BIF_ALIST_3)
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

    if (is_not_binary(BIF_ARG_1)) {
	goto badarg;
    }
    if (!term_to_Uint(BIF_ARG_2, &pos)) {
	goto badarg;
    }
    if (!term_to_Sint(BIF_ARG_3, &len)) {
	goto badarg;
    }
    if (len < 0) {
	Sint lentmp = -len;
	if (-lentmp != len) {
	    goto badarg;
	}
	len = lentmp;
	pos -= len;
    }
    if (((pos + len) - len) != pos) {
	goto badarg;
    }
    if ((orig_size = binary_size(BIF_ARG_1)) < pos ||
	orig_size < (pos + len)) {
	goto badarg;
    }



    hp = HAlloc(BIF_P, ERL_SUB_BIN_SIZE);

    ERTS_GET_REAL_BIN(BIF_ARG_1, orig, offset, bit_offset, bit_size);
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
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE binary_part_2(BIF_ALIST_2)
{
    Eterm *tp;
    if (is_not_tuple(BIF_ARG_2)) {
	goto badarg;
    }
    tp = tuple_val(BIF_ARG_2);
    if (arityval(*tp) != 2) {
	goto badarg;
    }
    return binary_part_3(BIF_P,BIF_ARG_1,tp[1], tp[2]);
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

static void cleanup_common_data(Binary *bp)
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
    return;
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
       thats not to costly. */
    mb = erts_create_magic_binary((n+1)*sizeof(CommonData),cleanup_common_data);
    cd = (CommonData *) ERTS_MAGIC_BIN_DATA(mb);
    l = list;
    while (is_list(l)) {
	Uint bitoffs;
	Uint bitsize;
	Uint offset;
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
		memcpy(cd[i].buff,tmp,cd[i].bufflen);
	    }
	}
	hp = HAlloc(p, PROC_BIN_SIZE);
	bin_term = erts_mk_magic_binary_term(&hp, &MSO(p), mb);
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
    ASSERT(ERTS_TERM_IS_MAGIC_BINARY(bin_term));
    bin = ((ProcBin *) binary_val(bin_term))->val;
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
	BIF_TRAP3(&binary_longest_prefix_trap_export, p, bin_term, epos, orig_list);
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



/*
 * Hard debug functions (dump) for the search structures
 */

#ifdef HARDDEBUG
static void dump_bm_data(BMData *bm)
{
    int i,j;
    erts_printf("Dumping Boyer-More structure.\n");
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
    memset(spaces,' ',10*indent);
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
