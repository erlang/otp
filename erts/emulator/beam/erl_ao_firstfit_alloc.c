/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2013. All Rights Reserved.
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
 * Description:	An "address order first fit" allocator
 *              based on a Red-Black (binary search) Tree. The search,
 *              insert, and delete operations are all O(log n) operations
 *              on a Red-Black Tree.
 *              Red-Black Trees are described in "Introduction to Algorithms",
 *              by Thomas H. Cormen, Charles E. Leiserson, and Ronald L. Riverest.
 *
 *              This module is a callback-module for erl_alloc_util.c
 *
 * AOFF Algorithm:
 *              The tree nodes are free-blocks ordered in address order.
 *              Every node also keeps the size of the largest block in its
 *              sub-tree ('max_size'). By that we can start from root and keep
 *              left (for low addresses) while dismissing entire sub-trees with
 *              too small blocks.
 * AOFFCBF:
 *              The only difference for "bestfit within carrier" is the tree
 *              sorting order. Blocks are first sorted wrt carrier address
 *              and then wrt size if they belong to the same carrier. 
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
#if 0
#define HARD_DEBUG
#endif
#else
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

#define IS_RED(N)		(((AOFF_RBTree_t *) (N)) \
				 && ((AOFF_RBTree_t *) (N))->flags & RED_FLG)
#define IS_BLACK(N)		(!IS_RED(((AOFF_RBTree_t *) (N))))

#define SET_RED(N)		(((AOFF_RBTree_t *) (N))->flags |= RED_FLG)
#define SET_BLACK(N)		(((AOFF_RBTree_t *) (N))->flags &= ~RED_FLG)

#undef ASSERT
#define ASSERT ASSERT_EXPR

#if 1
#define RBT_ASSERT	ASSERT
#else
#define RBT_ASSERT(x)
#endif


/* Types... */
typedef struct AOFF_RBTree_t_ AOFF_RBTree_t;

struct AOFF_RBTree_t_ {
    Block_t hdr;
    Uint flags;
    AOFF_RBTree_t *parent;
    AOFF_RBTree_t *left;
    AOFF_RBTree_t *right;
    Uint max_sz;  /* of all blocks in this sub-tree */
};
#define AOFF_BLK_SZ(B) MBC_FBLK_SZ(&(B)->hdr)

typedef struct AOFF_Carrier_t_ AOFF_Carrier_t;

struct AOFF_Carrier_t_ {
    Carrier_t crr;
    AOFF_Carrier_t* next;
    AOFF_Carrier_t** prevp;
    AOFF_RBTree_t* root;
};

#ifdef HARD_DEBUG
static void check_carriers(AOFFAllctr_t* alc, AOFF_Carrier_t *crr, Uint32 flags);
static AOFF_RBTree_t * check_tree(AOFFAllctr_t* alc, AOFF_RBTree_t* root, Uint);
#endif


/* Calculate 'max_size' of tree node x by only looking at the direct children
 * of x and x itself.
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

/* Set new possibly lower 'max_size' of node and propagate change toward root
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

static ERTS_INLINE SWord cmp_blocks(AOFFAllctr_t* alc,
				    AOFF_RBTree_t* lhs, AOFF_RBTree_t* rhs)
{
    ASSERT(FBLK_TO_MBC(&lhs->hdr) == FBLK_TO_MBC(&rhs->hdr));
    if (alc->bf_within_carrier) {
	SWord diff = (SWord)AOFF_BLK_SZ(lhs) - (SWord)AOFF_BLK_SZ(rhs);
	if (diff) return diff;
    }
    return (char*)lhs - (char*)rhs;
}

static ERTS_INLINE SWord cmp_cand_blk(AOFFAllctr_t* alc,
				      Block_t* cand_blk, AOFF_RBTree_t* rhs)
{
    if (alc->bf_within_carrier) {
	if (BLK_TO_MBC(cand_blk) == FBLK_TO_MBC(&rhs->hdr)) {
	    SWord diff = (SWord)MBC_BLK_SZ(cand_blk) - (SWord)MBC_FBLK_SZ(&rhs->hdr);
	    if (diff) return diff;
	}
    }
    return (char*)cand_blk - (char*)rhs;
}


/* Prototypes of callback functions */
static Block_t*	aoff_get_free_block(Allctr_t *, Uint, Block_t *, Uint, Uint32 flags);
static void aoff_link_free_block(Allctr_t *, Block_t*, Uint32 flags);
static void aoff_unlink_free_block(Allctr_t *allctr, Block_t *del, Uint32 flags);
static void aoff_creating_mbc(Allctr_t*, Carrier_t*, Uint32 flags);
static void aoff_destroying_mbc(Allctr_t*, Carrier_t*, Uint32 flags);

static Eterm info_options(Allctr_t *, char *, int *, void *, Uint **, Uint *);
static void init_atoms(void);



#ifdef DEBUG

/* Destroy all tree fields */
#define DESTROY_TREE_NODE(N)						\
  sys_memset((void *) (((Block_t *) (N)) + 1),				\
	     0xff,							\
	     (sizeof(AOFF_RBTree_t) - sizeof(Block_t)))

#else

#define DESTROY_TREE_NODE(N)

#endif


static int atoms_initialized = 0;

void
erts_aoffalc_init(void)
{
    atoms_initialized = 0;
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

    alc->bf_within_carrier              = aoffinit->bf_within_carrier;
    allctr->mbc_header_size		= sizeof(AOFF_Carrier_t);
    allctr->min_mbc_size		= MIN_MBC_SZ;
    allctr->min_mbc_first_free_size	= MIN_MBC_FIRST_FREE_SZ;
    allctr->min_block_size		= sizeof(AOFF_RBTree_t);

    allctr->vsn_str			= aoffinit->bf_within_carrier ?
	ERTS_ALC_AOFF_CBF_ALLOC_VSN_STR : ERTS_ALC_AOFF_ALLOC_VSN_STR;


    /* Callback functions */

    allctr->get_free_block		= aoff_get_free_block;
    allctr->link_free_block		= aoff_link_free_block;
    allctr->unlink_free_block	        = aoff_unlink_free_block;
    allctr->info_options		= info_options;

    allctr->get_next_mbc_size		= NULL;
    allctr->creating_mbc		= aoff_creating_mbc;
    allctr->destroying_mbc		= aoff_destroying_mbc;
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

    y->max_sz = x->max_sz;
    lower_max_size(y, NULL);
    DESTROY_TREE_NODE(x);
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
aoff_unlink_free_block(Allctr_t *allctr, Block_t *del, Uint32 flags)
{
#ifdef HARD_DEBUG
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
#endif
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) FBLK_TO_MBC(del);
    AOFF_RBTree_t **root = &crr->root;
    Uint spliced_is_black;
    AOFF_RBTree_t *x, *y, *z = (AOFF_RBTree_t *) del;
    AOFF_RBTree_t null_x; /* null_x is used to get the fixup started when we
			splice out a node without children. */

    null_x.parent = NULL;

#ifdef HARD_DEBUG
    check_carriers(alc, crr, flags);
    check_tree(alc, *root, 0);
#endif

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
	replace(root, z, y);
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

    DESTROY_TREE_NODE(del);

#ifdef HARD_DEBUG
    check_tree(alc, *root, 0);
#endif
}

static void
aoff_link_free_block(Allctr_t *allctr, Block_t *block, Uint32 flags)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_RBTree_t *blk = (AOFF_RBTree_t *) block;
    AOFF_RBTree_t **root;
    AOFF_Carrier_t *blk_crr = (AOFF_Carrier_t*) FBLK_TO_MBC(block);
    Uint blk_sz = AOFF_BLK_SZ(blk);

#ifdef HARD_DEBUG
    check_carriers(alc, blk_crr, flags);
    check_tree(alc, blk_crr->root, 0);
#endif

    root = &blk_crr->root;
    blk->flags	= 0;
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
	    if (x->max_sz < blk_sz) {
		x->max_sz = blk_sz;
	    }
	    if (cmp_blocks(alc, blk, x) < 0) {
		if (!x->left) {
		    blk->parent = x;
		    x->left = blk;
		    break;
		}
		x = x->left;
	    }
	    else {
		if (!x->right) {
		    blk->parent = x;
		    x->right = blk;
		    break;
		}
		x = x->right;
	    }
	}

	/* Insert block into size tree */
	RBT_ASSERT(blk->parent);

	SET_RED(blk);
	if (IS_RED(blk->parent))
	    tree_insert_fixup(root, blk);
    }

#ifdef HARD_DEBUG
    check_tree(alc, *root, 0);
#endif
}

static Block_t *
aoff_get_free_block(Allctr_t *allctr, Uint size,
		    Block_t *cand_blk, Uint cand_size, Uint32 flags)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_Carrier_t *crr = ((flags & ERTS_ALCU_FLG_SBMBC)
			   ? alc->sbmbc_first : alc->mbc_first);
    AOFF_RBTree_t *x, *blk = NULL;
#ifdef HARD_DEBUG
    AOFF_RBTree_t* dbg_blk;
#endif

    ASSERT(!cand_blk || cand_size >= size);

    /* Get first-fit carrier
     */
    for (;;) {
	if (!crr)
	    return NULL;
	if (crr->root && crr->root->max_sz >= size)
	    break;
	crr = crr->next;
    }

    /* Get block within carrier tree
     */
    x = crr->root;
#ifdef HARD_DEBUG
    dbg_blk = check_tree(alc, x, size);
#endif

    while (x) {
	if (x->left && x->left->max_sz >= size) {
	    x = x->left;
	}
	else if (AOFF_BLK_SZ(x) >= size) {
	    blk = x;
	    break;
	}
	else {
	    x = x->right;
	}
    }

#ifdef HARD_DEBUG
    ASSERT(blk == dbg_blk);
#endif

    if (!blk)
	return NULL;

    if (cand_blk && cmp_cand_blk(alc, cand_blk, blk) < 0) {
	return NULL; /* cand_blk was better */
    }

    aoff_unlink_free_block(allctr, (Block_t *) blk, flags);

    return (Block_t *) blk;
}

static void aoff_creating_mbc(Allctr_t *allctr, Carrier_t *carrier, Uint32 flags)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) carrier;
    AOFF_Carrier_t **prevp = ((flags & ERTS_ALCU_FLG_SBMBC)
			      ? &alc->sbmbc_first : &alc->mbc_first);
    AOFF_Carrier_t *p;


    /* Link carrier in address order
     */
    for (p = *prevp; p && crr > p; p = p->next) {
	prevp = &p->next;
    }
    ASSERT(crr != p);

    crr->next = p;
    crr->prevp = prevp;
    *prevp = crr;
    if (p) {
	ASSERT(prevp == p->prevp);
	p->prevp = &crr->next;
    }

    /* aoff_link_free_block will add free block later */
    crr->root = NULL;
}

static void aoff_destroying_mbc(Allctr_t *allctr, Carrier_t *carrier, Uint32 flags)
{
    AOFFAllctr_t *alc = (AOFFAllctr_t *) allctr;
    AOFF_Carrier_t *crr = (AOFF_Carrier_t*) carrier;
    AOFF_Carrier_t **pp = ((flags & ERTS_ALCU_FLG_SBMBC)
			   ? &alc->sbmbc_first : &alc->mbc_first);

    /* Unlink carrier
     */
    while (*pp != crr) {
	ASSERT(*pp && (*pp)->prevp == pp);
	pp = &(*pp)->next;
    }
    *pp = crr->next;
    if (crr->next) {
	ASSERT(crr->next->prevp == &crr->next);
	crr->next->prevp = crr->prevp;
    }
}


/*
 * info_options()
 */

static struct {
    Eterm as;
    Eterm aoff;
    Eterm aoffcbf;
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(void)
{
#ifdef DEBUG
    Eterm *atom;
#endif

    if (atoms_initialized)
	return;

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	*atom = THE_NON_VALUE;
    }
#endif
    AM_INIT(as);
    AM_INIT(aoff);
    AM_INIT(aoffcbf);

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	ASSERT(*atom != THE_NON_VALUE);
    }
#endif

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
	     int *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    AOFFAllctr_t* alc = (AOFFAllctr_t*) allctr;
    Eterm res = THE_NON_VALUE;

    if (print_to_p) {
	erts_print(*print_to_p,
		   print_to_arg,
		   "%sas: %s\n",
		   prefix,
		   alc->bf_within_carrier ? "aoffcbf" : "aoff");
    }

    if (hpp || szp) {
	
	if (!atoms_initialized)
	    erl_exit(1, "%s:%d: Internal error: Atoms not initialized",
		     __FILE__, __LINE__);;

	res = NIL;
	add_2tup(hpp, szp, &res, am.as,
		 alc->bf_within_carrier ? am.aoffcbf : am.aoff);
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
    case 0x501: {
	AOFF_Carrier_t* crr = ((AOFFAllctr_t *) a1)->mbc_first;
	Uint size = (Uint) a2;
	for (;;) {
	    if (!crr)
		return (UWord)NULL;
	    if (crr->root && crr->root->max_sz >= size)
		break;
	    crr = crr->next;
	}	
	return (UWord) crr->root;
    }
    case 0x502:	return (UWord) ((AOFF_RBTree_t *) a1)->parent;
    case 0x503:	return (UWord) ((AOFF_RBTree_t *) a1)->left;
    case 0x504:	return (UWord) ((AOFF_RBTree_t *) a1)->right;
    case 0x506:	return (UWord) IS_BLACK((AOFF_RBTree_t *) a1);
    case 0x508: return (UWord) 0; /* IS_BF_ALGO */
    case 0x509: return (UWord) ((AOFF_RBTree_t *) a1)->max_sz;
    case 0x50a: return (UWord) ((AOFFAllctr_t *) a1)->bf_within_carrier;
    default:	ASSERT(0); return ~((UWord) 0);
    }
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */


#ifdef HARD_DEBUG

static void check_carriers(AOFFAllctr_t* alc, AOFF_Carrier_t *crr, Uint32 flags)
{ 
    AOFF_Carrier_t *p = ((flags & ERTS_ALCU_FLG_SBMBC)
			 ? alc->sbmbc_first : alc->mbc_first);
    int found_it = 0;

    for ( ; p; p = p->next) {
	if (p == crr) {
	    ASSERT(!found_it);
	    found_it = 1;
	}
	ASSERT(!p->next || p->next > p);
    }
    ASSERT(found_it);
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
check_tree(AOFFAllctr_t* alc, AOFF_RBTree_t* root, Uint size)
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

	crr = FBLK_TO_MBC(&x->hdr);
	ASSERT((char*)x > (char*)crr);
	ASSERT(((char*)x + AOFF_BLK_SZ(x)) <= ((char*)crr + CARRIER_SZ(crr)));

	if (IS_RED(x)) {
	    ASSERT(IS_BLACK(x->right));
	    ASSERT(IS_BLACK(x->left));
	}

	ASSERT(x->parent || x == root);

	if (x->left) {
	    ASSERT(x->left->parent == x);
	    ASSERT(cmp_blocks(alc, x->left, x) < 0);
	    ASSERT(x->left->max_sz <= x->max_sz);	    
	}

	if (x->right) {
	    ASSERT(x->right->parent == x);
	    ASSERT(cmp_blocks(alc, x->right, x) > 0);
	    ASSERT(x->right->max_sz <= x->max_sz);	    
	}
	ASSERT(x->max_sz >= AOFF_BLK_SZ(x));
	ASSERT(x->max_sz == AOFF_BLK_SZ(x)
	       || x->max_sz == (x->left ? x->left->max_sz : 0)
	       || x->max_sz == (x->right ? x->right->max_sz : 0));

	if (size && AOFF_BLK_SZ(x) >= size) {
	    if (!res || cmp_blocks(alc, x, res) < 0) {
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
	fprintf(stderr, "%s: sz=%lu addr=0x%lx max_size=%lu\r\n",
		IS_BLACK(x) ? "BLACK" : "RED",
		AOFF_BLK_SZ(x), (Uint)x, x->max_sz);
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

