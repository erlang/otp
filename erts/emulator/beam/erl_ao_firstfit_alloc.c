/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2018. All Rights Reserved.
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
 * Description:	A family of "first fit" allocator strategies
 *              based on a Red-Black (binary search) Tree. The search,
 *              insert, and delete operations are all O(log n) operations
 *              on a Red-Black Tree.
 *              Red-Black Trees are described in "Introduction to Algorithms",
 *              by Thomas H. Cormen, Charles E. Leiserson, and Ronald L. Riverest.
 *
 *              This module is a callback-module for erl_alloc_util.c
 *
 * AOFF Algorithm:
 *              The tree nodes are ordered in address order.
 *              Every node also keeps the size of the largest block in its
 *              sub-tree ('max_sz'). By that we can start from root and keep
 *              left (for low addresses) while dismissing entire sub-trees with
 *              too small blocks.
 * Bestfit within carrier:
 *              The only difference for "bestfit within carrier" is the tree
 *              sorting order. Blocks within the same carrier are sorted
 *              wrt size instead of address. The 'max_sz' field is maintained
 *              in order to dismiss entire carriers with too small blocks. 
 * Age Order:
 *      	Carriers are ordered by creation time instead of address.
 *      	Oldest carrier with a large enough free block is chosen.
 *      	No age order supported for blocks.
 *
 * Authors: 	Rickard Green/Sverker Eriksson
 */


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "global.h"
#define GET_ERL_AOFF_ALLOC_IMPL
#include "erl_ao_firstfit_alloc.h"

#ifdef DEBUG
# define IS_DEBUG 1
#if 0
#define HARD_DEBUG
#endif
#else
# define IS_DEBUG 0
#undef HARD_DEBUG
#endif

#define MIN_MBC_SZ		(16*1024)
#define MIN_MBC_FIRST_FREE_SZ	(4*1024)

#define TREE_NODE_FLG		(((Uint) 1) << 0)
#define RED_FLG			(((Uint) 1) << 1)
#ifdef HARD_DEBUG
#  define LEFT_VISITED_FLG	(((Uint) 1) << 2)
#  define RIGHT_VISITED_FLG	(((Uint) 1) << 3)
#endif
#ifdef DEBUG
#  define IS_BF_FLG	        (((Uint) 1) << 4)
#endif

#define IS_TREE_NODE(N)		(((AOFF_RBTree_t *) (N))->flags & TREE_NODE_FLG)
#define IS_LIST_ELEM(N)		(!IS_TREE_NODE(((AOFF_RBTree_t *) (N))))

#define SET_TREE_NODE(N)	(((AOFF_RBTree_t *) (N))->flags |= TREE_NODE_FLG)
#define SET_LIST_ELEM(N)	(((AOFF_RBTree_t *) (N))->flags &= ~TREE_NODE_FLG)

#define IS_RED(N)		(((AOFF_RBTree_t *) (N)) \
				 && ((AOFF_RBTree_t *) (N))->flags & RED_FLG)
#define IS_BLACK(N)		(!IS_RED(((AOFF_RBTree_t *) (N))))

#define SET_RED(N)		(((AOFF_RBTree_t *) (N))->flags |= RED_FLG)
#define SET_BLACK(N)		(((AOFF_RBTree_t *) (N))->flags &= ~RED_FLG)

#if 1
#define RBT_ASSERT	ASSERT
#else
#define RBT_ASSERT(x)
#endif

#define AOFF_BLK_SZ(B) MBC_FBLK_SZ(&(B)->hdr)

#define AOFF_LIST_NEXT(N) (((AOFF_RBTree_t*)(N))->u.next)
#define AOFF_LIST_PREV(N) (((AOFF_RBTree_t*)(N))->parent)

typedef struct AOFF_Carrier_t_ AOFF_Carrier_t;

struct AOFF_Carrier_t_ {
    Carrier_t crr;
    AOFF_RBTree_t rbt_node;        /* My node in the carrier tree */
    AOFF_RBTree_t* root;           /* Root of my block tree */
    enum AOFFSortOrder blk_order;
};

#define RBT_NODE_TO_MBC(PTR) ErtsContainerStruct((PTR), AOFF_Carrier_t, rbt_node)

/* 
   To support carrier migration we keep two kinds of rb-trees:
   1. One tree of carriers for each allocator instance.
   2. One tree of free blocks for each carrier. 
   Both trees use the same node structure AOFF_RBTree_t and implementation.
   Carrier nodes thus contain a phony Block_t header 'rbt_node.hdr'.
   The size value of such a phony block is the size of the largest free block in
   that carrier, i.e same as 'max_sz' of the root node of its block tree.  
*/

#ifdef HARD_DEBUG
#  define HARD_CHECK_IS_MEMBER(ROOT,NODE) ASSERT(rbt_is_member(ROOT,NODE))
#  define HARD_CHECK_TREE(CRR,ORDER,ROOT,SZ) check_tree(CRR, ORDER, ROOT, SZ)
static AOFF_RBTree_t * check_tree(Carrier_t*, enum AOFFSortOrder, AOFF_RBTree_t*, Uint);
#else
#  define HARD_CHECK_IS_MEMBER(ROOT,NODE)
#  define HARD_CHECK_TREE(CRR,ORDER,ROOT,SZ)
#endif


/* Calculate 'max_sz' of tree node x by only looking at 'max_sz' of the
 * direct children of x and the size x itself.
 */
static ERTS_INLINE Uint node_max_size(AOFF_RBTree_t *x)
{
    Uint sz = AOFF_BLK_SZ(x);
    if (x->left && x->left->max_sz > sz) {
	sz = x->left->max_sz;
    }
    if (x->right && x->right->max_sz > sz) {
	sz = x->right->max_sz;
    }
    return sz;
}

/* Set new possibly lower 'max_sz' of node and propagate change toward root
*/
static ERTS_INLINE void lower_max_size(AOFF_RBTree_t *node,
				       AOFF_RBTree_t* stop_at)
{
    AOFF_RBTree_t* x = node;
    Uint old_max = x->max_sz;
    Uint new_max = node_max_size(x);

    if (new_max < old_max) {
	x->max_sz = new_max;
	while ((x=x->parent) != stop_at && x->max_sz == old_max) {
	    x->max_sz = node_max_size(x);
	}
	ASSERT(x == stop_at || x->max_sz > old_max);
    }
    else ASSERT(new_max == old_max);
}

/*
 * Set possibly new larger 'max_sz' of node and propagate change toward root
 */
void erts_aoff_larger_max_size(AOFF_RBTree_t *node)
{
    AOFF_RBTree_t* x = node;
    const Uint new_sz = node->hdr.bhdr;

    ASSERT(!x->left  || x->left->max_sz  <= x->max_sz);
    ASSERT(!x->right || x->right->max_sz <= x->max_sz);

    while (new_sz > x->max_sz) {
        x->max_sz = new_sz;
        x = x->parent;
        if (!x)
            break;
    }
}

/* Compare nodes for both carrier and block trees */
static ERTS_INLINE SWord cmp_blocks(enum AOFFSortOrder order,
				    AOFF_RBTree_t* lhs, AOFF_RBTree_t* rhs)
{
    ASSERT(lhs != rhs);
    if (order == FF_AGEFF) {
	Sint64 diff = lhs->u.birth_time - rhs->u.birth_time;
 #ifdef ARCH_64
        if (diff)
            return diff;
 #else
        if (diff < 0)
            return -1;
        else if (diff > 0)
            return 1;
 #endif
    }
    else {
	ASSERT(order == FF_AOFF || FBLK_TO_MBC(&lhs->hdr) == FBLK_TO_MBC(&rhs->hdr));
	if (order != FF_AOFF) {
	    SWord diff = (SWord)AOFF_BLK_SZ(lhs) - (SWord)AOFF_BLK_SZ(rhs);
	    if (diff || order == FF_BF) return diff;
	}
    }
    return (char*)lhs - (char*)rhs;
}

/* Compare candidate block. Only for block tree */
static ERTS_INLINE SWord cmp_cand_blk(enum AOFFSortOrder order,
				      Block_t* cand_blk, AOFF_RBTree_t* rhs)
{
    ASSERT(order != FF_AGEFF);
    if (order != FF_AOFF) {
	if (BLK_TO_MBC(cand_blk) == FBLK_TO_MBC(&rhs->hdr)) {
	    SWord diff = (SWord)MBC_BLK_SZ(cand_blk) - (SWord)MBC_FBLK_SZ(&rhs->hdr);
	    if (diff || order == FF_BF) return diff;
	}
    }
    return (char*)cand_blk - (char*)rhs;
}


/* Prototypes of callback functions */
static Block_t*	aoff_get_free_block(Allctr_t *, Uint, Block_t *, Uint);
static void aoff_link_free_block(Allctr_t *, Block_t*);
static void aoff_unlink_free_block(Allctr_t *allctr, Block_t *del);
static void aoff_creating_mbc(Allctr_t*, Carrier_t*);
#ifdef DEBUG
static void aoff_destroying_mbc(Allctr_t*, Carrier_t*);
#endif
static void aoff_add_mbc(Allctr_t*, Carrier_t*);
static void aoff_remove_mbc(Allctr_t*, Carrier_t*);
static UWord aoff_largest_fblk_in_mbc(Allctr_t*, Carrier_t*);

static Block_t *aoff_first_fblk_in_mbc(Allctr_t *, Carrier_t *);
static Block_t *aoff_next_fblk_in_mbc(Allctr_t *, Carrier_t *, Block_t *);

/* Generic tree functions used by both carrier and block trees. */
static void rbt_delete(AOFF_RBTree_t** root, AOFF_RBTree_t* del);
static void rbt_insert(enum AOFFSortOrder, AOFF_RBTree_t** root, AOFF_RBTree_t* blk);
static AOFF_RBTree_t* rbt_search(AOFF_RBTree_t* root, Uint size);

static Eterm info_options(Allctr_t *, char *, fmtfn_t *, void *, Uint **, Uint *);
static void init_atoms(void);


static int atoms_initialized = 0;

#ifndef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
static erts_atomic64_t birth_time_counter;
#endif

void
erts_aoffalc_init(void)
{
    atoms_initialized = 0;
#ifndef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    erts_atomic64_init_nob(&birth_time_counter, 0);
#endif
}

Allctr_t *
erts_aoffalc_start(AOFFAllctr_t *alc,
		   AOFFAllctrInit_t* aoffinit,
		   AllctrInit_t *init)
{
    struct {
	int dummy;
	AOFFAllctr_t allctr;
    } zero = {0};
    /* The struct with a dummy element first is used in order to avoid (an
       incorrect) gcc warning. gcc warns if {0} is used as initializer of
       a struct when the first member is a struct (not if, for example,
       the third member is a struct). */

    Allctr_t *allctr = (Allctr_t *) alc;

    sys_memcpy((void *) alc, (void *) &zero.allctr, sizeof(AOFFAllctr_t));

    if (aoffinit->blk_order == FF_CHAOS) {
        const enum AOFFSortOrder orders[3] = {FF_AOFF, FF_AOBF, FF_BF};
        int index = init->ix % (sizeof(orders) / sizeof(orders[0]));

        ASSERT(init->alloc_no == ERTS_ALC_A_TEST);
        aoffinit->blk_order = orders[index];
    }

    if (aoffinit->crr_order == FF_CHAOS) {
        const enum AOFFSortOrder orders[2] = {FF_AGEFF, FF_AOFF};
        int index = init->ix % (sizeof(orders) / sizeof(orders[0]));

        ASSERT(init->alloc_no == ERTS_ALC_A_TEST);
        aoffinit->crr_order = orders[index];
    }

    alc->blk_order                      = aoffinit->blk_order;
    alc->crr_order                      = aoffinit->crr_order;
    allctr->mbc_header_size		= sizeof(AOFF_Carrier_t);
    allctr->min_mbc_size		= MIN_MBC_SZ;
    allctr->min_mbc_first_free_size	= MIN_MBC_FIRST_FREE_SZ;
    allctr->min_block_size              = sizeof(AOFF_RBTree_t);

    allctr->vsn_str			= ERTS_ALC_AOFF_ALLOC_VSN_STR;


    /* Callback functions */

    allctr->get_free_block		= aoff_get_free_block;
    allctr->link_free_block		= aoff_link_free_block;
    allctr->unlink_free_block           = aoff_unlink_free_block;
    allctr->info_options		= info_options;

    allctr->get_next_mbc_size		= NULL;
    allctr->creating_mbc		= aoff_creating_mbc;
#ifdef DEBUG
    allctr->destroying_mbc		= aoff_destroying_mbc;
#else
    allctr->destroying_mbc		= NULL;
#endif
    allctr->add_mbc                     = aoff_add_mbc;
    allctr->remove_mbc                  = aoff_remove_mbc;
    allctr->largest_fblk_in_mbc         = aoff_largest_fblk_in_mbc;
    allctr->first_fblk_in_mbc           = aoff_first_fblk_in_mbc;
    allctr->next_fblk_in_mbc            = aoff_next_fblk_in_mbc;
    allctr->init_atoms			= init_atoms;

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    allctr->check_block			= NULL;
    allctr->check_mbc			= NULL;
#endif

    allctr->atoms_initialized		= 0;

    if (!erts_alcu_start(allctr, init))
	return NULL;

    return allctr;
}

/*
 * Red-Black Tree operations needed
 */

static ERTS_INLINE void
left_rotate(AOFF_RBTree_t **root, AOFF_RBTree_t *x)
{
    AOFF_RBTree_t *y = x->right;
    x->right = y->left;
    if (y->left)
	y->left->parent = x;
    y->parent = x->parent;
    if (!y->parent) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == x->parent->left)
	x->parent->left = y;
    else {
	RBT_ASSERT(x == x->parent->right);
	x->parent->right = y;
    }
    y->left = x;
    x->parent = y;

    y->max_sz = x->max_sz;
    x->max_sz = node_max_size(x);
    ASSERT(y->max_sz >= x->max_sz);
}

static ERTS_INLINE void
right_rotate(AOFF_RBTree_t **root, AOFF_RBTree_t *x)
{
    AOFF_RBTree_t *y = x->left;
    x->left = y->right;
    if (y->right)
	y->right->parent = x;
    y->parent = x->parent;
    if (!y->parent) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == x->parent->right)
	x->parent->right = y;
    else {
	RBT_ASSERT(x == x->parent->left);
	x->parent->left = y;
    }
    y->right = x;
    x->parent = y;
    y->max_sz = x->max_sz;
    x->max_sz = node_max_size(x);
    ASSERT(y->max_sz >= x->max_sz);
}


/*
 * Replace node x with node y
 * NOTE: block header of y is not changed
 */
static ERTS_INLINE void
replace(AOFF_RBTree_t **root, AOFF_RBTree_t *x, AOFF_RBTree_t *y)
{

    if (!x->parent) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == x->parent->left)
	x->parent->left = y;
    else {
	RBT_ASSERT(x == x->parent->right);
	x->parent->right = y;
    }
    if (x->left) {
	RBT_ASSERT(x->left->parent == x);
	x->left->parent = y;
    }
    if (x->right) {
	RBT_ASSERT(x->right->parent == x);
	x->right->parent = y;
    }

    y->flags	= x->flags;
    y->parent	= x->parent;
    y->right	= x->right;
    y->left	= x->left;
    y->max_sz   = x->max_sz;
}

static void
tree_insert_fixup(AOFF_RBTree_t** root, AOFF_RBTree_t *blk)
{
    AOFF_RBTree_t *x = blk, *y;

    /*
     * Rearrange the tree so that it satisfies the Red-Black Tree properties
     */

    RBT_ASSERT(x != *root && IS_RED(x->parent));
    do {

	/*
	 * x and its parent are both red. Move the red pair up the tree
	 * until we get to the root or until we can separate them.
	 */

	RBT_ASSERT(IS_RED(x));
	RBT_ASSERT(IS_BLACK(x->parent->parent));
	RBT_ASSERT(x->parent->parent);

	if (x->parent == x->parent->parent->left) {
	    y = x->parent->parent->right;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		x = x->parent;
		SET_BLACK(x);
		x = x->parent;
		SET_RED(x);
	    }
	    else {

		if (x == x->parent->right) {
		    x = x->parent;
		    left_rotate(root, x);
		}

		RBT_ASSERT(x == x->parent->parent->left->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent));
		RBT_ASSERT(IS_BLACK(x->parent->parent));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(x->parent);
		SET_RED(x->parent->parent);
		right_rotate(root, x->parent->parent);

		RBT_ASSERT(x == x->parent->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent->right));
		RBT_ASSERT(IS_BLACK(x->parent));
		break;
	    }
	}
	else {
	    RBT_ASSERT(x->parent == x->parent->parent->right);
	    y = x->parent->parent->left;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		x = x->parent;
		SET_BLACK(x);
		x = x->parent;
		SET_RED(x);
	    }
	    else {

		if (x == x->parent->left) {
		    x = x->parent;
		    right_rotate(root, x);
		}

		RBT_ASSERT(x == x->parent->parent->right->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent));
		RBT_ASSERT(IS_BLACK(x->parent->parent));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(x->parent);
		SET_RED(x->parent->parent);
		left_rotate(root, x->parent->parent);

		RBT_ASSERT(x == x->parent->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent->left));
		RBT_ASSERT(IS_BLACK(x->parent));
		break;
	    }
	}
    } while (x != *root && IS_RED(x->parent));

    SET_BLACK(*root);
}

static void
aoff_unlink_free_block(Allctr_t *allctr, Block_t *blk)
{
    AOFF_RBTree_t* del = (AOFF_RBTree_t*)blk;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) FBLK_TO_MBC(&del->hdr);

    (void)allctr;

    ASSERT(crr->rbt_node.hdr.bhdr == crr->root->max_sz);
    HARD_CHECK_TREE(&crr->crr, crr->blk_order, crr->root, 0);

    if (crr->blk_order == FF_BF) {
	ASSERT(del->flags & IS_BF_FLG);
	if (IS_LIST_ELEM(del)) {
	    /* Remove from list */
	    ASSERT(AOFF_LIST_PREV(del));
	    ASSERT(AOFF_LIST_PREV(del)->flags & IS_BF_FLG);
	    AOFF_LIST_NEXT(AOFF_LIST_PREV(del)) = AOFF_LIST_NEXT(del);
	    if (AOFF_LIST_NEXT(del)) {
		ASSERT(AOFF_LIST_NEXT(del)->flags & IS_BF_FLG);
		AOFF_LIST_PREV(AOFF_LIST_NEXT(del)) = AOFF_LIST_PREV(del);
	    }
	    return;
	}
	else if (AOFF_LIST_NEXT(del)) {
	    /* Replace tree node by next element in list... */
	    
	    ASSERT(AOFF_BLK_SZ(AOFF_LIST_NEXT(del)) == AOFF_BLK_SZ(del));
	    ASSERT(IS_LIST_ELEM(AOFF_LIST_NEXT(del)));

	    replace(&crr->root, (AOFF_RBTree_t*)del, AOFF_LIST_NEXT(del));

	    HARD_CHECK_TREE(&crr->crr, crr->blk_order, crr->root, 0);
	    return;
	}
    }

    rbt_delete(&crr->root, (AOFF_RBTree_t*)del);

    HARD_CHECK_TREE(&crr->crr, crr->blk_order, crr->root, 0);

    /* Update the carrier tree with a potentially new (lower) max_sz
     */
    if (crr->root) {
	if (crr->rbt_node.hdr.bhdr == crr->root->max_sz) {
	    return;
	}
	ASSERT(crr->rbt_node.hdr.bhdr > crr->root->max_sz);
	crr->rbt_node.hdr.bhdr = crr->root->max_sz;
    }
    else {
	crr->rbt_node.hdr.bhdr = 0;
    }
    lower_max_size(&crr->rbt_node, NULL);
}


static void
rbt_delete(AOFF_RBTree_t** root, AOFF_RBTree_t* del)
{
    Uint spliced_is_black;
    AOFF_RBTree_t *x, *y, *z = del;
    AOFF_RBTree_t null_x; /* null_x is used to get the fixup started when we
			splice out a node without children. */

    HARD_CHECK_IS_MEMBER(*root, del);

    null_x.parent = NULL;

    /* Remove node from tree... */

    /* Find node to splice out */
    if (!z->left || !z->right)
	y = z;
    else
	/* Set y to z:s successor */
	for(y = z->right; y->left; y = y->left);
    /* splice out y */
    x = y->left ? y->left : y->right;
    spliced_is_black = IS_BLACK(y);
    if (x) {
	x->parent = y->parent;
    }
    else if (spliced_is_black) {
	x = &null_x;
	x->flags = 0;
	SET_BLACK(x);
	x->right = x->left = NULL;
	x->max_sz = 0;
	x->parent = y->parent;
	y->left = x;
    }

    if (!y->parent) {
	RBT_ASSERT(*root == y);
	*root = x;
    }
    else {
	if (y == y->parent->left) {
	    y->parent->left = x;
	}
	else {
	    RBT_ASSERT(y == y->parent->right);
	    y->parent->right = x;
	}
	if (y->parent != z) {
	    lower_max_size(y->parent, (y==z ? NULL : z));
	}
    }
    if (y != z) {
	/* We spliced out the successor of z; replace z by the successor */
	ASSERT(z != &null_x);
	replace(root, z, y);
	lower_max_size(y, NULL);
    }

    if (spliced_is_black) {
	/* We removed a black node which makes the resulting tree
	   violate the Red-Black Tree properties. Fixup tree... */

	while (IS_BLACK(x) && x->parent) {

	    /*
	     * x has an "extra black" which we move up the tree
	     * until we reach the root or until we can get rid of it.
	     *
	     * y is the sibbling of x
	     */

	    if (x == x->parent->left) {
		y = x->parent->right;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(x->parent));
		    SET_RED(x->parent);
		    left_rotate(root, x->parent);
		    y = x->parent->right;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->left) && IS_BLACK(y->right)) {
		    SET_RED(y);
		    x = x->parent;
		}
		else {
		    if (IS_BLACK(y->right)) {
			SET_BLACK(y->left);
			SET_RED(y);
			right_rotate(root, y);
			y = x->parent->right;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(x->parent)) {

			SET_BLACK(x->parent);
			SET_RED(y);
		    }
		    RBT_ASSERT(y->right);
		    SET_BLACK(y->right);
		    left_rotate(root, x->parent);
		    x = *root;
		    break;
		}
	    }
	    else {
		RBT_ASSERT(x == x->parent->right);
		y = x->parent->left;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(x->parent));
		    SET_RED(x->parent);
		    right_rotate(root, x->parent);
		    y = x->parent->left;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->right) && IS_BLACK(y->left)) {
		    SET_RED(y);
		    x = x->parent;
		}
		else {
		    if (IS_BLACK(y->left)) {
			SET_BLACK(y->right);
			SET_RED(y);
			left_rotate(root, y);
			y = x->parent->left;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(x->parent)) {
			SET_BLACK(x->parent);
			SET_RED(y);
		    }
		    RBT_ASSERT(y->left);
		    SET_BLACK(y->left);
		    right_rotate(root, x->parent);
		    x = *root;
		    break;
		}
	    }
	}
	SET_BLACK(x);

	if (null_x.parent) {
	    if (null_x.parent->left == &null_x)
		null_x.parent->left = NULL;
	    else {
		RBT_ASSERT(null_x.parent->right == &null_x);
		null_x.parent->right = NULL;
	    }
	    RBT_ASSERT(!null_x.left);
	    RBT_ASSERT(!null_x.right);
	}
	else if (*root == &null_x) {
	    *root = NULL;
	    RBT_ASSERT(!null_x.left);
	    RBT_ASSERT(!null_x.right);
	}
    }
}

static void
aoff_link_free_block(Allctr_t *allctr, Block_t *block)
{
    AOFF_RBTree_t *blk = (AOFF_RBTree_t *) block;
    AOFF_RBTree_t *crr_node;
    AOFF_Carrier_t *blk_crr = (AOFF_Carrier_t*) FBLK_TO_MBC(block);
    Uint blk_sz = AOFF_BLK_SZ(blk);

    (void)allctr;

    ASSERT(allctr == ERTS_ALC_CARRIER_TO_ALLCTR(&blk_crr->crr));
    ASSERT(blk_crr->rbt_node.hdr.bhdr == (blk_crr->root ? blk_crr->root->max_sz : 0));
    HARD_CHECK_TREE(&blk_crr->crr, blk_crr->blk_order, blk_crr->root, 0);

    rbt_insert(blk_crr->blk_order, &blk_crr->root, blk);

    /*
     * Update carrier tree with a potentially new (larger) max_sz
     */
    crr_node = &blk_crr->rbt_node;
    if (blk_sz > crr_node->hdr.bhdr) {
        ASSERT(blk_sz == blk_crr->root->max_sz);
        crr_node->hdr.bhdr = blk_sz;
        while (blk_sz > crr_node->max_sz) {
            crr_node->max_sz = blk_sz;
            crr_node = crr_node->parent;
            if (!crr_node) break;
        }
    }
    HARD_CHECK_TREE(NULL, alc->crr_order, alc->mbc_root, 0);
}

static void
rbt_insert(enum AOFFSortOrder order, AOFF_RBTree_t** root, AOFF_RBTree_t* blk)
{
    Uint blk_sz = AOFF_BLK_SZ(blk);

#ifdef DEBUG
    blk->flags  = (order == FF_BF) ? IS_BF_FLG : 0;
#else
    blk->flags  = 0;
#endif
    blk->left	= NULL;
    blk->right	= NULL;
    blk->max_sz = blk_sz;

    if (!*root) {
	blk->parent = NULL;
	SET_BLACK(blk);
	*root = blk;
    }
    else {
	AOFF_RBTree_t *x = *root;
	while (1) {
	    SWord diff;
	    if (x->max_sz < blk_sz) {
		x->max_sz = blk_sz;
	    }
	    diff = cmp_blocks(order, blk, x);
	    if (diff < 0) {
		if (!x->left) {
		    blk->parent = x;
		    x->left = blk;
		    break;
		}
		x = x->left;
	    }
	    else if (diff > 0) {
		if (!x->right) {
		    blk->parent = x;
		    x->right = blk;
		    break;
		}
		x = x->right;
	    }
	    else {
		ASSERT(order == FF_BF);
		ASSERT(blk->flags & IS_BF_FLG);
		ASSERT(x->flags & IS_BF_FLG);
		SET_LIST_ELEM(blk);
		AOFF_LIST_NEXT(blk) = AOFF_LIST_NEXT(x);
		AOFF_LIST_PREV(blk) = x;
		if (AOFF_LIST_NEXT(x))
		    AOFF_LIST_PREV(AOFF_LIST_NEXT(x)) = blk;
		AOFF_LIST_NEXT(x) = blk;
		return;
	    }
	}

	/* Insert block into size tree */
	RBT_ASSERT(blk->parent);

	SET_RED(blk);
	if (IS_RED(blk->parent))
	    tree_insert_fixup(root, blk);
    }
    if (order == FF_BF) {
	SET_TREE_NODE(blk);
	AOFF_LIST_NEXT(blk) = NULL;
    }
}

static AOFF_RBTree_t*
rbt_search(AOFF_RBTree_t* root, Uint size)
{
    AOFF_RBTree_t* x = root;

    ASSERT(x);
    for (;;) {
	if (x->left && x->left->max_sz >= size) {
	    x = x->left;
	}
	else if (AOFF_BLK_SZ(x) >= size) {
	    return x;
	}
	else {
	    x = x->right;
	    if (!x) {
		return NULL;
	    }
	}
    }
}

Carrier_t* aoff_lookup_pooled_mbc(Allctr_t* allctr, Uint size)
{
    AOFF_RBTree_t* node;

    if (!allctr->cpool.pooled_tree)
	return NULL;
    node = rbt_search(allctr->cpool.pooled_tree, size);
    return node ? ErtsContainerStruct(node, Carrier_t, cpool.pooled) : NULL;
}

static Block_t *
aoff_get_free_block(Allctr_t *allctr, Uint size,
		    Block_t *cand_blk, Uint cand_size)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_RBTree_t *crr_node = alc->mbc_root;
    AOFF_Carrier_t* crr;
    AOFF_RBTree_t *blk = NULL;
#ifdef HARD_DEBUG
    AOFF_RBTree_t* dbg_blk;
#endif

    ASSERT(!cand_blk || cand_size >= size);

    /* Get first-fit carrier
     */
    if (!crr_node || !(blk=rbt_search(crr_node, size))) {
	return NULL;
    }
    crr = RBT_NODE_TO_MBC(blk);

    /* Get block within carrier tree
     */
#ifdef HARD_DEBUG
    dbg_blk = HARD_CHECK_TREE(&crr->crr, crr->blk_order, crr->root, size);
#endif

    blk = rbt_search(crr->root, size);
    ASSERT(blk);

#ifdef HARD_DEBUG
    ASSERT(blk == dbg_blk);
#endif

    if (!blk)
	return NULL;

    if (cand_blk && cmp_cand_blk(crr->blk_order, cand_blk, blk) < 0) {
	return NULL; /* cand_blk was better */
    }

    aoff_unlink_free_block(allctr, (Block_t *) blk);

    return (Block_t *) blk;
}

static ERTS_INLINE Sint64 get_birth_time(void)
{
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return (Sint64) erts_os_monotonic_time();
#else
    return (Sint64) erts_atomic64_inc_read_nob(&birth_time_counter);
#endif
}

static void aoff_creating_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) carrier;
    AOFF_RBTree_t **root = &alc->mbc_root;
    Sint64 bt = get_birth_time();

    HARD_CHECK_TREE(NULL, alc->crr_order, *root, 0);

    crr->rbt_node.hdr.bhdr = 0;

    /* While birth time is only used for FF_AGEFF, we have to set it for all
     * types as we can be migrated to an instance that uses it and we don't
     * want to mess its order up. */
    crr->rbt_node.u.birth_time = bt;
    crr->crr.cpool.pooled.u.birth_time = bt;

    rbt_insert(alc->crr_order, root, &crr->rbt_node);

    /* aoff_link_free_block will add free block later */
    crr->root = NULL;

    HARD_CHECK_TREE(NULL, alc->crr_order, *root, 0);

    /* When a carrier has been migrated, its block order may differ from that
     * of the allocator it's been migrated to. */
    crr->blk_order = alc->blk_order;
}

#define IS_CRR_IN_TREE(CRR,ROOT) \
    ((CRR)->rbt_node.parent || (ROOT) == &(CRR)->rbt_node)

#ifdef DEBUG
static void aoff_destroying_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) carrier;

    ASSERT(!IS_CRR_IN_TREE(crr, alc->mbc_root));
}
#endif

static void aoff_add_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) carrier;
    AOFF_RBTree_t **root = &alc->mbc_root;

    ASSERT(!IS_CRR_IN_TREE(crr, *root));
    HARD_CHECK_TREE(NULL, alc->crr_order, *root, 0);

    rbt_insert(alc->crr_order, root, &crr->rbt_node);

    HARD_CHECK_TREE(NULL, alc->crr_order, *root, 0);
}

void aoff_add_pooled_mbc(Allctr_t *allctr, Carrier_t *crr)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_RBTree_t **root = &allctr->cpool.pooled_tree;

    ASSERT(allctr == crr->cpool.orig_allctr);
    HARD_CHECK_TREE(NULL, 0, *root, 0);

    /* Link carrier in address order tree
     */
    rbt_insert(alc->crr_order, root, &crr->cpool.pooled);

    HARD_CHECK_TREE(NULL, 0, *root, 0);
}

static void aoff_remove_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    AOFF_RBTree_t **root = &((AOFFAllctr_t*)allctr)->mbc_root;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*)carrier;

    ASSERT(allctr == ERTS_ALC_CARRIER_TO_ALLCTR(carrier));

    if (!IS_CRR_IN_TREE(crr,*root))
	return;

    HARD_CHECK_TREE(NULL, alc->crr_order, *root, 0);

    rbt_delete(root, &crr->rbt_node);
    crr->rbt_node.parent = NULL;
    crr->rbt_node.left = NULL;
    crr->rbt_node.right = NULL;
    crr->rbt_node.max_sz = crr->rbt_node.hdr.bhdr;

    HARD_CHECK_TREE(NULL, alc->crr_order, *root, 0);
}

void aoff_remove_pooled_mbc(Allctr_t *allctr, Carrier_t *crr)
{
    ASSERT(allctr == crr->cpool.orig_allctr);

    HARD_CHECK_TREE(NULL, 0, allctr->cpool.pooled_tree, 0);

    rbt_delete(&allctr->cpool.pooled_tree, &crr->cpool.pooled);
#ifdef DEBUG
    crr->cpool.pooled.parent = NULL;
    crr->cpool.pooled.left = NULL;
    crr->cpool.pooled.right = NULL;
    crr->cpool.pooled.max_sz = 0;
#endif
    HARD_CHECK_TREE(NULL, 0, allctr->cpool.pooled_tree, 0);

}


static UWord aoff_largest_fblk_in_mbc(Allctr_t* allctr, Carrier_t* carrier)
{
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) carrier;

    ASSERT(allctr == ERTS_ALC_CARRIER_TO_ALLCTR(carrier));
    ASSERT(crr->rbt_node.hdr.bhdr == (crr->root ? crr->root->max_sz : 0));
    return crr->rbt_node.hdr.bhdr;
}

static Block_t *aoff_first_fblk_in_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*)carrier;

    (void)allctr;

    if (crr->root) {
        AOFF_RBTree_t *blk;

        /* Descend to the rightmost block of the tree. */
        for (blk = crr->root; blk->right; blk = blk->right);

        return (Block_t*)blk;
    }

    return NULL;
}

static Block_t *aoff_next_fblk_in_mbc(Allctr_t *allctr, Carrier_t *carrier,
                                      Block_t *block)
{
    AOFF_RBTree_t *parent, *blk;

    (void)allctr;
    (void)carrier;

    blk = (AOFF_RBTree_t*)block;

    if (blk->left) {
        /* Descend to the rightmost block of the left subtree. */
        for (blk = blk->left; blk->right; blk = blk->right);

        return (Block_t*)blk;
    }

    while (blk->parent) {
        parent = blk->parent;

        /* If we ascend from the right we know we haven't visited our parent
         * yet, because we always descend as far as we can to the right when
         * entering a subtree. */
        if (parent->right == blk) {
            ASSERT(parent->left != blk);
            return (Block_t*)parent;
        }

        /* If we ascend from the left we know we've already visited our
         * parent, and will need to keep ascending until we do so from the
         * right or reach the end of the tree. */
        ASSERT(parent->left == blk);
        blk = parent;
    }

    return NULL;
}

/*
 * info_options()
 */

static const char* flavor_str[2][3] = {
    {"ageffcaoff", "ageffcaobf", "ageffcbf"},
    {      "aoff",  "aoffcaobf",  "aoffcbf"}
};
static Eterm flavor_atoms[2][3];

static struct {
    Eterm as;
} am;

static void ERTS_INLINE atom_init(Eterm *atom, const char *name)
{
    *atom = am_atom_put(name, sys_strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(void)
{
    int i, j;

    if (atoms_initialized)
	return;

    AM_INIT(as);

    for (i = 0; i < 2; i++)
        for (j = 0; j < 3; j++)
            atom_init(&flavor_atoms[i][j], flavor_str[i][j]);

    atoms_initialized = 1;
}


#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple

static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

static Eterm
info_options(Allctr_t *allctr,
	     char *prefix,
	     fmtfn_t *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    AOFFAllctr_t* alc = (AOFFAllctr_t*) allctr;
    Eterm res = THE_NON_VALUE;

    ASSERT(alc->crr_order >= 0 && alc->crr_order <= 1);
    ASSERT(alc->blk_order >= 1 && alc->blk_order <= 3);

    if (print_to_p) {
	erts_print(*print_to_p,
		   print_to_arg,
		   "%sas: %s\n",
		   prefix,
		   flavor_str[alc->crr_order][alc->blk_order-1]);
    }

    if (hpp || szp) {

	if (!atoms_initialized)
	    erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error: Atoms not initialized",
		     __FILE__, __LINE__);;

	res = NIL;
	add_2tup(hpp, szp, &res, am.as,
                 flavor_atoms[alc->crr_order][alc->blk_order-1]);
    }

    return res;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE:  erts_aoffalc_test() is only supposed to be used for testing.       *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_aoffalc_test()                                                    *
\*                                                                           */

UWord
erts_aoffalc_test(UWord op, UWord a1, UWord a2)
{
    switch (op) {
    case 0x500: return (UWord) ((AOFFAllctr_t *) a1)->blk_order == FF_AOBF;
    case 0x501: {
	AOFF_RBTree_t *node = ((AOFFAllctr_t *) a1)->mbc_root;
	Uint size = (Uint) a2;
	node = node ? rbt_search(node, size) : NULL;
	return (UWord) (node ? RBT_NODE_TO_MBC(node)->root : NULL);
    }
    case 0x502:	return (UWord) ((AOFF_RBTree_t *) a1)->parent;
    case 0x503:	return (UWord) ((AOFF_RBTree_t *) a1)->left;
    case 0x504:	return (UWord) ((AOFF_RBTree_t *) a1)->right;
    case 0x505:	return (UWord) AOFF_LIST_NEXT(a1);
    case 0x506:	return (UWord) IS_BLACK((AOFF_RBTree_t *) a1);
    case 0x507:	return (UWord) IS_TREE_NODE((AOFF_RBTree_t *) a1);
    case 0x508: return (UWord) 0; /* IS_BF_ALGO */
    case 0x509: return (UWord) ((AOFF_RBTree_t *) a1)->max_sz;
    case 0x50a: return (UWord) ((AOFFAllctr_t *) a1)->blk_order == FF_BF;
    case 0x50b:	return (UWord) AOFF_LIST_PREV(a1);
    default:	ASSERT(0); return ~((UWord) 0);
    }
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */


#ifdef HARD_DEBUG
static int rbt_is_member(AOFF_RBTree_t* root, AOFF_RBTree_t* node)
{
    while (node != root) {
        if (!node->parent || (node->parent->left != node &&
                              node->parent->right != node)) {
            return 0;
        }
	node = node->parent;
    }
    return 1;
}

#define IS_LEFT_VISITED(FB)	((FB)->flags & LEFT_VISITED_FLG)
#define IS_RIGHT_VISITED(FB)	((FB)->flags & RIGHT_VISITED_FLG)

#define SET_LEFT_VISITED(FB)	((FB)->flags |= LEFT_VISITED_FLG)
#define SET_RIGHT_VISITED(FB)	((FB)->flags |= RIGHT_VISITED_FLG)

#define UNSET_LEFT_VISITED(FB)	((FB)->flags &= ~LEFT_VISITED_FLG)
#define UNSET_RIGHT_VISITED(FB)	((FB)->flags &= ~RIGHT_VISITED_FLG)


#if 0
#  define PRINT_TREE
#else
#  undef PRINT_TREE
#endif

#ifdef PRINT_TREE
static void print_tree(AOFF_RBTree_t*);
#endif

/*
 * Checks that the order between parent and children are correct,
 * and that the Red-Black Tree properies are satisfied. if size > 0,
 * check_tree() returns the node that satisfies "address order first fit"
 *
 * The Red-Black Tree properies are:
 *   1. Every node is either red or black.
 *   2. Every leaf (NIL) is black.
 *   3. If a node is red, then both its children are black.
 *   4. Every simple path from a node to a descendant leaf
 *      contains the same number of black nodes.
 *
 *   + own.max_size == MAX(own.size, left.max_size, right.max_size)
 */

static AOFF_RBTree_t *
check_tree(Carrier_t* within_crr, enum AOFFSortOrder order, AOFF_RBTree_t* root, Uint size)
{
    AOFF_RBTree_t *res = NULL;
    Sint blacks;
    Sint curr_blacks;
    AOFF_RBTree_t *x;
    Carrier_t* crr;
    Uint depth, max_depth, node_cnt;

#ifdef PRINT_TREE
    print_tree(root);
#endif
    ASSERT((within_crr && order >= FF_AOFF) ||
           (!within_crr && order <= FF_AOFF));

    if (!root)
	return res;

    x = root;
    ASSERT(IS_BLACK(x));
    ASSERT(!x->parent);
    curr_blacks = 1;
    blacks = -1;
    depth = 1;
    max_depth = 0;
    node_cnt = 0;

    while (x) {
	if (!IS_LEFT_VISITED(x)) {
	    SET_LEFT_VISITED(x);
	    if (x->left) {
		x = x->left;
		++depth;
		if (IS_BLACK(x))
		    curr_blacks++;
		continue;
	    }
	    else {
		if (blacks < 0)
		    blacks = curr_blacks;
		ASSERT(blacks == curr_blacks);
	    }
	}

	if (!IS_RIGHT_VISITED(x)) {
	    SET_RIGHT_VISITED(x);
	    if (x->right) {
		x = x->right;
		++depth;
		if (IS_BLACK(x))
		    curr_blacks++;
		continue;
	    }
	    else {
		if (blacks < 0)
		    blacks = curr_blacks;
		ASSERT(blacks == curr_blacks);
	    }
	}

	++node_cnt;
	if (depth > max_depth)
	    max_depth = depth;

	if (within_crr) {
	    crr = FBLK_TO_MBC(&x->hdr);
	    ASSERT(crr == within_crr);
	    ASSERT((char*)x > (char*)crr);
	    ASSERT(((char*)x + AOFF_BLK_SZ(x)) <= ((char*)crr + CARRIER_SZ(crr)));

	}
	if (order == FF_BF) {
	    AOFF_RBTree_t* y = x;
	    AOFF_RBTree_t* nxt = AOFF_LIST_NEXT(y);
	    ASSERT(IS_TREE_NODE(x));
	    while (nxt) {
		ASSERT(IS_LIST_ELEM(nxt));
		ASSERT(AOFF_BLK_SZ(nxt) == AOFF_BLK_SZ(x));
		ASSERT(FBLK_TO_MBC(&nxt->hdr) == within_crr);
		ASSERT(AOFF_LIST_PREV(nxt) == y);
		y = nxt;
		nxt = AOFF_LIST_NEXT(nxt);
	    }
	}

	if (IS_RED(x)) {
	    ASSERT(IS_BLACK(x->right));
	    ASSERT(IS_BLACK(x->left));
	}

	ASSERT(x->parent || x == root);

	if (x->left) {
	    ASSERT(x->left->parent == x);
	    ASSERT(cmp_blocks(order, x->left, x) < 0);
	    ASSERT(x->left->max_sz <= x->max_sz);
	}

	if (x->right) {
	    ASSERT(x->right->parent == x);
	    ASSERT(cmp_blocks(order, x->right, x) > 0);
	    ASSERT(x->right->max_sz <= x->max_sz);
	}
	ASSERT(x->max_sz >= AOFF_BLK_SZ(x));
	ASSERT(x->max_sz == AOFF_BLK_SZ(x)
	       || x->max_sz == (x->left ? x->left->max_sz : 0)
	       || x->max_sz == (x->right ? x->right->max_sz : 0));

	if (size && AOFF_BLK_SZ(x) >= size) {
	    if (!res || cmp_blocks(order, x, res) < 0) {
		res = x;
	    }
	}

	UNSET_LEFT_VISITED(x);
	UNSET_RIGHT_VISITED(x);
	if (IS_BLACK(x))
	    curr_blacks--;
	x = x->parent;
	--depth;
    }
    ASSERT(depth == 0 || (!root && depth==1));
    ASSERT(curr_blacks == 0);
    ASSERT((1 << (max_depth/2)) <= node_cnt);

    UNSET_LEFT_VISITED(root);
    UNSET_RIGHT_VISITED(root);

    return res;

}


#ifdef PRINT_TREE
#define INDENT_STEP 2

#include <stdio.h>

static void
print_tree_aux(AOFF_RBTree_t *x, int indent)
{
    int i;

    if (x) {
	print_tree_aux(x->right, indent + INDENT_STEP);
	for (i = 0; i < indent; i++) {
	    putc(' ', stderr);
	}
	fprintf(stderr, "%s: sz=%lu addr=0x%lx max_size=%u\r\n",
		IS_BLACK(x) ? "BLACK" : "RED",
		AOFF_BLK_SZ(x), (Uint)x, (unsigned)x->max_sz);
	print_tree_aux(x->left,  indent + INDENT_STEP);
    }
}


static void
print_tree(AOFF_RBTree_t* root)
{
    fprintf(stderr, " --- AOFF tree begin ---\r\n");
    print_tree_aux(root, 0);
    fprintf(stderr, " --- AOFF tree end ---\r\n");
}

#endif /* PRINT_TREE */

#endif /* HARD_DEBUG */
