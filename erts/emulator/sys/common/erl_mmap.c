/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2013. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <sys.h>
#include <erl_mmap.h>
#include <stddef.h>

#ifdef DEBUG
#  define RBT_DEBUG
#endif
#ifdef RBT_DEBUG
#  define RBT_ASSERT	ERTS_ASSERT
#  define IF_RBT_DEBUG(C) C
#else
#  define RBT_ASSERT(x)
#  define IF_RBT_DEBUG(C)
#endif

typedef struct RBTNode_ RBTNode;
struct RBTNode_ {
    RBTNode *parent;
    RBTNode *left;
    RBTNode *right;
    int flags;
};

enum SortOrder {
    ADDR_ORDER,
    SZ_ADDR_ORDER,
    SZ_REVERSE_ADDR_ORDER
};

typedef struct {
    RBTNode* root;
    enum SortOrder order;
}RBTree;

#define RED_FLG      (1)
#define IS_RED(N)    ((N) && ((N)->flags & RED_FLG))
#define IS_BLACK(N)  (!IS_RED(N))
#define SET_RED(N)   ((N)->flags |= RED_FLG)
#define SET_BLACK(N) ((N)->flags &= ~RED_FLG)

#define HARD_DEBUG /*SVERK*/
#ifdef HARD_DEBUG
#  define HARD_CHECK_IS_MEMBER(ROOT,NODE) rbt_assert_is_member(ROOT,NODE)
#  define HARD_CHECK_TREE(TREE,SZ) check_tree(TREE, SZ)
static int rbt_assert_is_member(RBTNode* root, RBTNode* node);
static RBTNode* check_tree(RBTree* tree, Uint);
#else
#  define HARD_CHECK_IS_MEMBER(ROOT,NODE)
#  define HARD_CHECK_TREE(TREE,SZ)
#endif


typedef struct {
    RBTNode snode;
    RBTNode anode;
    char* start;
    char* end;
}ErtsFreeSegDesc;

static ERTS_INLINE ErtsFreeSegDesc* anode_to_desc(RBTNode* anode)
{
    return (ErtsFreeSegDesc*) ((char*)anode - offsetof(ErtsFreeSegDesc, anode));
}

static ERTS_INLINE ErtsFreeSegDesc* snode_to_desc(RBTNode* snode)
{
    return (ErtsFreeSegDesc*) ((char*)snode - offsetof(ErtsFreeSegDesc, snode));
}

static ERTS_INLINE ErtsFreeSegDesc* node_to_desc(enum SortOrder order, RBTNode* node)
{
    return order==ADDR_ORDER ? anode_to_desc(node) : snode_to_desc(node);
}

typedef struct {
    RBTree stree;
    RBTree atree;
}ErtsFreeSegMap;


#ifdef HARD_DEBUG
static ERTS_INLINE SWord cmp_blocks(enum SortOrder order,
                                     RBTNode* lhs, RBTNode* rhs)
{
    ErtsFreeSegDesc* ldesc = node_to_desc(order, lhs);
    ErtsFreeSegDesc* rdesc = node_to_desc(order, rhs);
    RBT_ASSERT(lhs != rhs);
    if (order != ADDR_ORDER) {
        SWord lsz = ldesc->end - ldesc->start;
        SWord rsz = rdesc->end - rdesc->start;
	SWord diff = lsz - rsz;
	if (diff) return diff;
    }
    if (order != SZ_REVERSE_ADDR_ORDER) {
        return (char*)ldesc->start - (char*)rdesc->start;
    }
    else {
        return (char*)rdesc->start - (char*)ldesc->start;
    }
}
#endif

static ERTS_INLINE SWord cmp_with_block(enum SortOrder order,
                                        SWord sz, char* addr, RBTNode* rhs)
{
    ErtsFreeSegDesc* rdesc;
    if (order != ADDR_ORDER) {
        rdesc = snode_to_desc(rhs);
        {
            SWord rhs_sz = rdesc->end - rdesc->start;
            SWord diff = sz - rhs_sz;
            if (diff) return diff;
        }
    }
    else
        rdesc = anode_to_desc(rhs);

    if (order != SZ_REVERSE_ADDR_ORDER)
        return addr - (char*)rdesc->start;
    else
        return (char*)rdesc->start - addr;
}


static ERTS_INLINE void
left_rotate(RBTNode **root, RBTNode *x)
{
    RBTNode *y = x->right;
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

    /*SVERK y->max_sz = x->max_sz;
    x->max_sz = node_max_size(x);
    ASSERT(y->max_sz >= x->max_sz);*/
}

static ERTS_INLINE void
right_rotate(RBTNode **root, RBTNode *x)
{
    RBTNode *y = x->left;
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
    /*SVERK y->max_sz = x->max_sz;
    x->max_sz = node_max_size(x);
    ASSERT(y->max_sz >= x->max_sz);*/
}

/*
 * Replace node x with node y
 * NOTE: block header of y is not changed
 */
static ERTS_INLINE void
replace(RBTNode **root, RBTNode *x, RBTNode *y)
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
    /*SVERK y->max_sz   = x->max_sz;*/
}

static void
tree_insert_fixup(RBTNode** root, RBTNode *blk)
{
    RBTNode *x = blk, *y;

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
rbt_delete(RBTNode** root, RBTNode* del)
{
    Uint spliced_is_black;
    RBTNode *x, *y, *z = del;
    RBTNode null_x; /* null_x is used to get the fixup started when we
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
        /*SVERK x->max_sz = 0;*/
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
	/*SVERK if (y->parent != z) {
	    lower_max_size(y->parent, (y==z ? NULL : z));
	}*/
    }
    if (y != z) {
	/* We spliced out the successor of z; replace z by the successor */
	RBT_ASSERT(z != &null_x);
	replace(root, z, y);
	/*SVERK lower_max_size(y, NULL);*/
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
rbt_insert(enum SortOrder order, RBTNode** root, RBTNode* blk)
{
#ifdef RBT_DEBUG
    ErtsFreeSegDesc *dbg_under=NULL, *dbg_over=NULL;
#endif
    ErtsFreeSegDesc* desc = node_to_desc(order, blk);
    char* blk_addr = desc->start;
    SWord blk_sz = desc->end - desc->start;
    /*SVERK Uint blk_sz = AOFF_BLK_SZ(blk);*/

    blk->flags  = 0;
    blk->left	= NULL;
    blk->right	= NULL;
    /*SVERK blk->max_sz = blk_sz;*/

    if (!*root) {
	blk->parent = NULL;
	SET_BLACK(blk);
	*root = blk;
    }
    else {
	RBTNode *x = *root;
	while (1) {
	    SWord diff;
	    /*SVERK if (x->max_sz < blk_sz) {
		x->max_sz = blk_sz;
	    }*/
	    diff = cmp_with_block(order, blk_sz, blk_addr, x);
	    if (diff < 0) {
                IF_RBT_DEBUG(dbg_over = node_to_desc(order, x));
		if (!x->left) {
		    blk->parent = x;
		    x->left = blk;
		    break;
		}
		x = x->left;
	    }
	    else {
                RBT_ASSERT(diff > 0);
                IF_RBT_DEBUG(dbg_under = node_to_desc(order, x));
                if (!x->right) {
                    blk->parent = x;
                    x->right = blk;
                    break;
                }
                x = x->right;
            }
	    /*SVERK else {
		ASSERT(flavor == AOFF_BF);
		ASSERT(blk->flags & IS_BF_FLG);
		ASSERT(x->flags & IS_BF_FLG);
		SET_LIST_ELEM(blk);
		LIST_NEXT(blk) = LIST_NEXT(x);
		LIST_PREV(blk) = x;
		if (LIST_NEXT(x))
		    LIST_PREV(LIST_NEXT(x)) = blk;
		LIST_NEXT(x) = blk;
		return;
	    }*/
	}

	/* Insert block into size tree */
	RBT_ASSERT(blk->parent);
#ifdef RBT_DEBUG
        if (!order) {
            RBT_ASSERT(!dbg_under || dbg_under->end < desc->start);
            RBT_ASSERT(!dbg_over || dbg_over->start > desc->end);
        }
#endif
	SET_RED(blk);
	if (IS_RED(blk->parent))
	    tree_insert_fixup(root, blk);
    }
    /*SVERK if (flavor == AOFF_BF) {
	SET_TREE_NODE(blk);
	LIST_NEXT(blk) = NULL;
    }*/
}


/* The API to keep track of a bunch of separated free segments
   (non-overlapping and non-adjacent).
 */
static void init_free_seg_map(ErtsFreeSegMap*, int reverse_ao);
static void adjacent_free_seg(ErtsFreeSegMap*, char* start, char* end,
                              ErtsFreeSegDesc** under, ErtsFreeSegDesc** over);
static void insert_free_seg(ErtsFreeSegMap*, ErtsFreeSegDesc*, char* start, char* end);
static void resize_free_seg(ErtsFreeSegMap*, ErtsFreeSegDesc*, char* start, char* end);
static void delete_free_seg(ErtsFreeSegMap*, ErtsFreeSegDesc*);
static ErtsFreeSegDesc* lookup_free_seg(ErtsFreeSegMap*, SWord sz);


static void init_free_seg_map(ErtsFreeSegMap* map, int reverse_ao)
{
    map->atree.root = NULL;
    map->atree.order = ADDR_ORDER;
    map->stree.root = NULL;
    map->stree.order = reverse_ao ? SZ_REVERSE_ADDR_ORDER : SZ_ADDR_ORDER;
}

static void adjacent_free_seg(ErtsFreeSegMap* map, char* start, char* end,
                              ErtsFreeSegDesc** under, ErtsFreeSegDesc** over)
{
    RBTNode* x = map->atree.root;

    *under = NULL;
    *over = NULL;
    while (x) {
	if (start < anode_to_desc(x)->start) {
            RBT_ASSERT(end <= anode_to_desc(x)->start);
            if (end == anode_to_desc(x)->start) {
                RBT_ASSERT(!*over);
                *over = anode_to_desc(x);
            }
            x = x->left;
	}
	else {
            RBT_ASSERT(start >= anode_to_desc(x)->end);
            if (start == anode_to_desc(x)->end) {
                RBT_ASSERT(!*under);
                *under = anode_to_desc(x);
            }
            x = x->right;
        }
    }
}

static void insert_free_seg(ErtsFreeSegMap* map, ErtsFreeSegDesc* desc,
                            char* start, char* end)
{
    desc->start = start;
    desc->end = end;
    rbt_insert(map->atree.order, &map->atree.root, &desc->anode);
    rbt_insert(map->stree.order, &map->stree.root, &desc->snode);
}

static void resize_free_seg(ErtsFreeSegMap* map, ErtsFreeSegDesc* desc,
                            char* start, char* end)
{
#ifdef DEBUG
    ErtsFreeSegDesc *dbg_under, *dbg_over;
    rbt_delete(&map->atree.root, &desc->anode);
    adjacent_free_seg(map, start, end, &dbg_under, &dbg_over);
    RBT_ASSERT(dbg_under == NULL && dbg_over == NULL);
    rbt_insert(map->atree.order, &map->atree.root, &desc->anode);
#endif
    rbt_delete(&map->stree.root, &desc->snode);
    desc->start = start;
    desc->end = end;
    rbt_insert(map->stree.order, &map->stree.root, &desc->snode);
}

static void delete_free_seg(ErtsFreeSegMap* map, ErtsFreeSegDesc* desc)
{
    rbt_delete(&map->atree.root, &desc->anode);
    rbt_delete(&map->stree.root, &desc->snode);
}

static ErtsFreeSegDesc* lookup_free_seg(ErtsFreeSegMap* map, SWord need_sz)
{
    RBTNode* x = map->stree.root;
    ErtsFreeSegDesc* best_desc = NULL;

    while (x) {
        ErtsFreeSegDesc* desc = snode_to_desc(x);
        SWord seg_sz = desc->end - desc->start;

	if (seg_sz < need_sz) {
	    x = x->right;
	}
	else {
            best_desc = desc;
	    x = x->left;
	}
    }
    return best_desc;
}


void erts_mmap_init(ErtsMMapInit* init)
{
#ifdef HARD_DEBUG
    erts_fprintf(stderr, "SVERK: scs = %bpu\n", init->scs);
    erts_fprintf(stderr, "SVERK: sco = %i\n", init->sco);
    erts_fprintf(stderr, "SVERK: scmgc = %i\n", init->scmgc);

    {
        void test_it(void);
        test_it();
    }
#endif
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */


#ifdef HARD_DEBUG

static int rbt_assert_is_member(RBTNode* root, RBTNode* node)
{
    while (node != root) {
	RBT_ASSERT(node->parent);
	RBT_ASSERT(node->parent->left == node || node->parent->right == node);
	node = node->parent;
    }
    return 1;
}

#define LEFT_VISITED_FLG 0x1000
#define THIS_VISITED_FLG 0x100
#define RIGHT_VISITED_FLG 0x10
#define IS_LEFT_VISITED(FB)	((FB)->flags & LEFT_VISITED_FLG)
#define IS_THIS_VISITED(FB)	((FB)->flags & THIS_VISITED_FLG)
#define IS_RIGHT_VISITED(FB)	((FB)->flags & RIGHT_VISITED_FLG)

#define SET_LEFT_VISITED(FB)	((FB)->flags |= LEFT_VISITED_FLG)
#define SET_THIS_VISITED(FB)	((FB)->flags |= THIS_VISITED_FLG)
#define SET_RIGHT_VISITED(FB)	((FB)->flags |= RIGHT_VISITED_FLG)

#define UNSET_LEFT_VISITED(FB)	((FB)->flags &= ~LEFT_VISITED_FLG)
#define UNSET_THIS_VISITED(FB)	((FB)->flags &= ~THIS_VISITED_FLG)
#define UNSET_RIGHT_VISITED(FB)	((FB)->flags &= ~RIGHT_VISITED_FLG)



#if 1 /*SVERK*/
#  define PRINT_TREE
#else
#  undef PRINT_TREE
#endif

#ifdef PRINT_TREE
static void print_tree(enum SortOrder order, RBTNode*);
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
 */

static RBTNode *
check_tree(RBTree* tree, Uint size)
{
    RBTNode *res = NULL;
    Sint blacks;
    Sint curr_blacks;
    RBTNode *x;
    Uint depth, max_depth, node_cnt;
    ErtsFreeSegDesc* seg = NULL;
    ErtsFreeSegDesc* prev_seg = NULL;

#ifdef PRINT_TREE
    print_tree(tree->order, tree->root);
#endif

    if (!tree->root)
	return res;

    x = tree->root;
    RBT_ASSERT(IS_BLACK(x));
    RBT_ASSERT(!x->parent);
    curr_blacks = 1;
    blacks = -1;
    depth = 1;
    max_depth = 0;
    node_cnt = 0;

    /* Traverse tree in sorting order */
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
		RBT_ASSERT(blacks == curr_blacks);
	    }
	}

        if (!IS_THIS_VISITED(x)) {
            SET_THIS_VISITED(x);
            ++node_cnt;
            if (depth > max_depth)
                max_depth = depth;

            if (IS_RED(x)) {
                RBT_ASSERT(IS_BLACK(x->right));
                RBT_ASSERT(IS_BLACK(x->left));
            }

            RBT_ASSERT(x->parent || x == tree->root);

            if (x->left) {
                RBT_ASSERT(x->left->parent == x);
                RBT_ASSERT(cmp_blocks(tree->order, x->left, x) < 0);
            }

            if (x->right) {
                RBT_ASSERT(x->right->parent == x);
                RBT_ASSERT(cmp_blocks(tree->order, x->right, x) > 0);
            }

            seg = node_to_desc(tree->order, x);
            RBT_ASSERT(seg->start < seg->end);
            if (size && (seg->end - seg->start) >= size) {
                if (!res || cmp_blocks(tree->order, x, res) < 0) {
                    res = x;
                }
            }
            if (tree->order == ADDR_ORDER) {
                RBT_ASSERT(!prev_seg || prev_seg->end < seg->start);
                prev_seg = seg;
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
		RBT_ASSERT(blacks == curr_blacks);
	    }
	}

	UNSET_LEFT_VISITED(x);
        UNSET_THIS_VISITED(x);
        UNSET_RIGHT_VISITED(x);
	if (IS_BLACK(x))
	    curr_blacks--;
	x = x->parent;
	--depth;
    }
    RBT_ASSERT(depth == 0 || (!tree->root && depth==1));
    RBT_ASSERT(curr_blacks == 0);
    RBT_ASSERT((1 << (max_depth/2)) <= node_cnt);

    UNSET_LEFT_VISITED(tree->root);
    UNSET_THIS_VISITED(tree->root);
    UNSET_RIGHT_VISITED(tree->root);

    return res;
}


#ifdef PRINT_TREE
#define INDENT_STEP 2

#include <stdio.h>

static void
print_tree_aux(enum SortOrder order, RBTNode *x, int indent)
{
    int i;

    if (x) {
        ErtsFreeSegDesc* desc = node_to_desc(order, x);
	print_tree_aux(order, x->right, indent + INDENT_STEP);
	for (i = 0; i < indent; i++) {
	    putc(' ', stderr);
	}
	fprintf(stderr, "%s: sz=%lx [%p - %p] desc=%p\r\n",
		IS_BLACK(x) ? "BLACK" : "RED",
                desc->end - desc->start, desc->start, desc->end, desc);
	print_tree_aux(order, x->left,  indent + INDENT_STEP);
    }
}


static void
print_tree(enum SortOrder order, RBTNode* root)
{
    static const char* type[] = {"Address","Size-Address","Size-RevAddress"};
    fprintf(stderr, " --- %s ordered tree begin ---\r\n", type[order]);
    print_tree_aux(order, root, 0);
    fprintf(stderr, " --- %s ordered tree end ---\r\n", type[order]);
}

#endif /* PRINT_TREE */


static ErtsFreeSegDesc* new_desc(void)
{
    return (ErtsFreeSegDesc*) malloc(sizeof(ErtsFreeSegDesc));
}

void test_it(void)
{
    ErtsFreeSegMap map;
    ErtsFreeSegDesc *desc, *under, *over, *d1, *d2;
    int i;

    for (i=0; i<2; i++) {
        init_free_seg_map(&map, i);

        insert_free_seg(&map, new_desc(), (char*)0x11000, (char*)0x12000);
        check_tree(&map.atree, 0); check_tree(&map.stree, 0);
        insert_free_seg(&map, new_desc(), (char*)0x13000, (char*)0x14000);
        check_tree(&map.atree, 0); check_tree(&map.stree, 0);
        insert_free_seg(&map, new_desc(), (char*)0x15000, (char*)0x17000);
        check_tree(&map.atree, 0); check_tree(&map.stree, 0);
        insert_free_seg(&map, new_desc(), (char*)0x8000, (char*)0x10000);
        check_tree(&map.atree, 0); check_tree(&map.stree, 0);

        desc = lookup_free_seg(&map, 0x500);
        ERTS_ASSERT(desc->start == (char*)(i?0x13000L:0x11000L));

        desc = lookup_free_seg(&map, 0x1500);
        ERTS_ASSERT(desc->start == (char*)0x15000);

        adjacent_free_seg(&map, (char*)0x6666, (char*)0x7777, &under, &over);
        ERTS_ASSERT(!under && !over);

        adjacent_free_seg(&map, (char*)0x6666, (char*)0x8000, &under, &over);
        ERTS_ASSERT(!under && over->start == (char*)0x8000);

        adjacent_free_seg(&map, (char*)0x10000, (char*)0x10500, &under, &over);
        ERTS_ASSERT(under->end == (char*)0x10000 && !over);

        adjacent_free_seg(&map, (char*)0x10100, (char*)0x10500, &under, &over);
        ERTS_ASSERT(!under && !over);

        adjacent_free_seg(&map, (char*)0x10100, (char*)0x11000, &under, &over);
        ERTS_ASSERT(!under && over && over->start == (char*)0x11000);

        adjacent_free_seg(&map, (char*)0x12000, (char*)0x12500, &under, &over);
        ERTS_ASSERT(under && under->end == (char*)0x12000 && !over);

        adjacent_free_seg(&map, (char*)0x12000, (char*)0x13000, &under, &over);
        ERTS_ASSERT(under && under->end == (char*)0x12000 &&
                    over && over->start == (char*)0x13000);

        adjacent_free_seg(&map, (char*)0x12500, (char*)0x13000, &under, &over);
        ERTS_ASSERT(!under && over && over->start == (char*)0x13000);

        d1 = lookup_free_seg(&map, 0x500);
        ERTS_ASSERT(d1->start == (char*)(i?0x13000L:0x11000L));

        resize_free_seg(&map, d1, d1->start - 0x800, (char*)d1->end);
        check_tree(&map.atree, 0); check_tree(&map.stree, 0);

        d2 = lookup_free_seg(&map, 0x1200);
        ERTS_ASSERT(d2 == d1);

        delete_free_seg(&map, d1);
        check_tree(&map.atree, 0); check_tree(&map.stree, 0);

        d1 = lookup_free_seg(&map, 0x1200);
        ERTS_ASSERT(d1->start == (char*)0x15000);
    }
}

#endif /* HARD_DEBUG */
