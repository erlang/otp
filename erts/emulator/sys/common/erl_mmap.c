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

#include "sys.h"
#include <stddef.h>
#include "erl_smp.h"
#include "erl_mmap.h"

#if defined(DEBUG) || 0
#  undef ERTS_MMAP_DEBUG
#  define ERTS_MMAP_DEBUG
#endif

/* #define ERTS_MMAP_DEBUG_FILL_AREAS */

#ifdef ERTS_MMAP_DEBUG
#  define ERTS_MMAP_ASSERT(A)						\
    ((void) (!(A)							\
	     ? erts_mmap_assert_failed(#A, __func__, __FILE__, __LINE__)\
	     : 1))
static int
erts_mmap_assert_failed(const char *a, const char *func, const char *file, int line)
{
    erts_fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
		 (char *) file, line, (char *) func, (char *) a);
    abort();
    return 0;
}
#else
#  define ERTS_MMAP_ASSERT(A) ((void) 1)
#endif

/*
 * `mmap_state.sa.bot` and `mmap_state.sua.top` are read only after
 * initialization, but the other pointers are not; i.e., only
 * ERTS_MMAP_IN_SUPERCARRIER() is allowed without the mutex held.
 */
#define ERTS_MMAP_IN_SUPERCARRIER(PTR)					\
    (((UWord) (PTR)) - ((UWord) mmap_state.sa.bot)			\
     < ((UWord) mmap_state.sua.top) - ((UWord) mmap_state.sa.bot))
#define ERTS_MMAP_IN_SUPERALIGNED_AREA(PTR)				\
    (ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&mmap_state.mtx)),	\
     (((UWord) (PTR)) - ((UWord) mmap_state.sa.bot)			\
      < ((UWord) mmap_state.sa.top) - ((UWord) mmap_state.sa.bot)))
#define ERTS_MMAP_IN_SUPERUNALIGNED_AREA(PTR)				\
    (ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&mmap_state.mtx)),	\
     (((UWord) (PTR)) - ((UWord) mmap_state.sua.bot)			\
      < ((UWord) mmap_state.sua.top) - ((UWord) mmap_state.sua.bot)))

int erts_have_erts_mmap;
UWord erts_page_inv_mask;

#if defined(DEBUG) || defined(ERTS_MMAP_DEBUG)
#  undef RBT_DEBUG
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
    UWord parent_and_color;     /* color in bit 0 of parent ptr */
    RBTNode *left;
    RBTNode *right;
};

#define RED_FLG      (1)
#define IS_RED(N)    ((N) && ((N)->parent_and_color & RED_FLG))
#define IS_BLACK(N)  (!IS_RED(N))
#define SET_RED(N)   ((N)->parent_and_color |= RED_FLG)
#define SET_BLACK(N) ((N)->parent_and_color &= ~RED_FLG)

static ERTS_INLINE RBTNode* parent(RBTNode* node)
{
    return (RBTNode*) (node->parent_and_color & ~RED_FLG);
}

static ERTS_INLINE void set_parent(RBTNode* node, RBTNode* parent)
{
    RBT_ASSERT(!((UWord)parent & RED_FLG));
    node->parent_and_color = ((UWord)parent) | (node->parent_and_color & RED_FLG);
}

static ERTS_INLINE UWord parent_and_color(RBTNode* parent, int color)
{
    RBT_ASSERT(!((UWord)parent & RED_FLG));
    RBT_ASSERT(!(color & ~RED_FLG));
    return ((UWord)parent) | color;
}


enum SortOrder {
    ADDR_ORDER,             /* only address order */
    SZ_ADDR_ORDER,          /* first size then address order as tiebreaker */
    SZ_REVERSE_ADDR_ORDER   /* first size then reverse address order */
};

typedef struct {
    RBTNode* root;
    enum SortOrder order;
}RBTree;

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

typedef struct {
    RBTree stree;
    RBTree atree;
}ErtsFreeSegMap;

static struct {
    int (*reserve_physical)(char *, UWord);
    void (*unreserve_physical)(char *, UWord);
    int supercarrier;
    int no_os_mmap;
    /*
     * Super unaligend area is located above super aligned
     * area. That is, `sa.bot` is beginning of the super
     * carrier, `sua.top` is the end of the super carrier,
     * and sa.top and sua.bot moves towards eachother.
     */
    struct {
	char *top;
	char *bot;
	ErtsFreeSegMap map;
    } sua;
    struct {
	char *top;
	char *bot;
	ErtsFreeSegMap map;
    } sa;
#if HAVE_MMAP && (!defined(MAP_ANON) && !defined(MAP_ANONYMOUS))
    int mmap_fd;
#endif
    erts_smp_mtx_t mtx;
    char *desc_free_list;
    struct {
	struct {
	    UWord total;
	    struct {
		UWord total;
		UWord sa;
		UWord sua;
	    } used;
	} supercarrier;
	struct {
	    UWord used;
	} os;
    } size;
} mmap_state;

#define ERTS_MMAP_SIZE_SC_SA_INC(SZ) 						\
    do {									\
	mmap_state.size.supercarrier.used.total += (SZ);			\
	mmap_state.size.supercarrier.used.sa += (SZ);				\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.total		\
			 <= mmap_state.size.supercarrier.total);		\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.sa			\
			 <= mmap_state.size.supercarrier.used.total);		\
    } while (0)
#define ERTS_MMAP_SIZE_SC_SA_DEC(SZ) 						\
    do {									\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.total >= (SZ));	\
	mmap_state.size.supercarrier.used.total -= (SZ);			\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.sa >= (SZ));		\
	mmap_state.size.supercarrier.used.sa -= (SZ);				\
    } while (0)
#define ERTS_MMAP_SIZE_SC_SUA_INC(SZ) 						\
    do {									\
	mmap_state.size.supercarrier.used.total += (SZ);			\
	mmap_state.size.supercarrier.used.sua += (SZ);				\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.total		\
			 <= mmap_state.size.supercarrier.total);		\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.sua			\
			 <= mmap_state.size.supercarrier.used.total);		\
    } while (0)
#define ERTS_MMAP_SIZE_SC_SUA_DEC(SZ) 						\
    do {									\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.total >= (SZ));	\
	mmap_state.size.supercarrier.used.total -= (SZ);			\
	ERTS_MMAP_ASSERT(mmap_state.size.supercarrier.used.sua >= (SZ));	\
	mmap_state.size.supercarrier.used.sua -= (SZ);				\
    } while (0)
#define ERTS_MMAP_SIZE_OS_INC(SZ)						\
    do {									\
	ERTS_MMAP_ASSERT(mmap_state.size.os.used + (SZ) >= (SZ));		\
	mmap_state.size.os.used += (SZ);					\
    } while (0)
#define ERTS_MMAP_SIZE_OS_DEC(SZ) 						\
    do {									\
	ERTS_MMAP_ASSERT(mmap_state.size.os.used >= (SZ));			\
	mmap_state.size.os.used -= (SZ);					\
    } while (0)

static void
add_free_desc_area(char *start, char *end)
{
    if (end > start && sizeof(ErtsFreeSegDesc) <= end - start) {
	ErtsFreeSegDesc *prev_desc, *desc;
	char *desc_end;

	prev_desc = (ErtsFreeSegDesc *) start;
	prev_desc->start = mmap_state.desc_free_list;
	desc = (ErtsFreeSegDesc *) (start + sizeof(ErtsFreeSegDesc));
	desc_end = start + 2*sizeof(ErtsFreeSegDesc);

	while (desc_end <= end) {
	    desc->start = (char *) prev_desc;
	    prev_desc = desc;
	    desc = (ErtsFreeSegDesc *) desc_end;
	    desc_end += sizeof(ErtsFreeSegDesc);
	}
	mmap_state.desc_free_list = (char *) prev_desc;
    }
}

static ERTS_INLINE ErtsFreeSegDesc *
alloc_desc(void)
{
    ErtsFreeSegDesc *res;
    res = (ErtsFreeSegDesc *) mmap_state.desc_free_list;
    if (res)
	mmap_state.desc_free_list = res->start;
    return res;
}

static ERTS_INLINE void
free_desc(ErtsFreeSegDesc *desc)
{
    desc->start = mmap_state.desc_free_list;
    mmap_state.desc_free_list = (char *) desc;
}

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
	set_parent(y->left, x);
    set_parent(y, parent(x));
    if (!parent(y)) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == parent(x)->left)
	parent(x)->left = y;
    else {
	RBT_ASSERT(x == parent(x)->right);
	parent(x)->right = y;
    }
    y->left = x;
    set_parent(x, y);

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
	set_parent(y->right, x);
    set_parent(y, parent(x));
    if (!parent(y)) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == parent(x)->right)
	parent(x)->right = y;
    else {
	RBT_ASSERT(x == parent(x)->left);
	parent(x)->left = y;
    }
    y->right = x;
    set_parent(x, y);
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

    if (!parent(x)) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == parent(x)->left)
	parent(x)->left = y;
    else {
	RBT_ASSERT(x == parent(x)->right);
	parent(x)->right = y;
    }
    if (x->left) {
	RBT_ASSERT(parent(x->left) == x);
	set_parent(x->left, y);
    }
    if (x->right) {
	RBT_ASSERT(parent(x->right) == x);
	set_parent(x->right, y);
    }

    /*y->flags	= x->flags;*/
    y->parent_and_color	= x->parent_and_color;
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

    RBT_ASSERT(x != *root && IS_RED(parent(x)));
    do {

	/*
	 * x and its parent are both red. Move the red pair up the tree
	 * until we get to the root or until we can separate them.
	 */

	RBT_ASSERT(IS_RED(x));
	RBT_ASSERT(IS_BLACK(parent(parent(x))));
	RBT_ASSERT(parent(parent(x)));

	if (parent(x) == parent(parent(x))->left) {
	    y = parent(parent(x))->right;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		x = parent(x);
		SET_BLACK(x);
		x = parent(x);
		SET_RED(x);
	    }
	    else {

		if (x == parent(x)->right) {
		    x = parent(x);
		    left_rotate(root, x);
		}

		RBT_ASSERT(x == parent(parent(x))->left->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(parent(x)));
		RBT_ASSERT(IS_BLACK(parent(parent(x))));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(parent(x));
		SET_RED(parent(parent(x)));
		right_rotate(root, parent(parent(x)));

		RBT_ASSERT(x == parent(x)->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(parent(x)->right));
		RBT_ASSERT(IS_BLACK(parent(x)));
		break;
	    }
	}
	else {
	    RBT_ASSERT(parent(x) == parent(parent(x))->right);
	    y = parent(parent(x))->left;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		x = parent(x);
		SET_BLACK(x);
		x = parent(x);
		SET_RED(x);
	    }
	    else {

		if (x == parent(x)->left) {
		    x = parent(x);
		    right_rotate(root, x);
		}

		RBT_ASSERT(x == parent(parent(x))->right->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(parent(x)));
		RBT_ASSERT(IS_BLACK(parent(parent(x))));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(parent(x));
		SET_RED(parent(parent(x)));
		left_rotate(root, parent(parent(x)));

		RBT_ASSERT(x == parent(x)->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(parent(x)->left));
		RBT_ASSERT(IS_BLACK(parent(x)));
		break;
	    }
	}
    } while (x != *root && IS_RED(parent(x)));

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

    null_x.parent_and_color = parent_and_color(NULL, !RED_FLG);

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
	set_parent(x, parent(y));
    }
    else if (spliced_is_black) {
	x = &null_x;
        /*x->flags = 0;
	SET_BLACK(x);*/
	x->right = x->left = NULL;
        /*SVERK x->max_sz = 0;*/
	x->parent_and_color = parent_and_color(parent(y), !RED_FLG);
	y->left = x;
    }

    if (!parent(y)) {
	RBT_ASSERT(*root == y);
	*root = x;
    }
    else {
	if (y == parent(y)->left) {
	    parent(y)->left = x;
	}
	else {
	    RBT_ASSERT(y == parent(y)->right);
	    parent(y)->right = x;
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

	while (IS_BLACK(x) && parent(x)) {

	    /*
	     * x has an "extra black" which we move up the tree
	     * until we reach the root or until we can get rid of it.
	     *
	     * y is the sibbling of x
	     */

	    if (x == parent(x)->left) {
		y = parent(x)->right;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(parent(x)));
		    SET_RED(parent(x));
		    left_rotate(root, parent(x));
		    y = parent(x)->right;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->left) && IS_BLACK(y->right)) {
		    SET_RED(y);
		    x = parent(x);
		}
		else {
		    if (IS_BLACK(y->right)) {
			SET_BLACK(y->left);
			SET_RED(y);
			right_rotate(root, y);
			y = parent(x)->right;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(parent(x))) {

			SET_BLACK(parent(x));
			SET_RED(y);
		    }
		    RBT_ASSERT(y->right);
		    SET_BLACK(y->right);
		    left_rotate(root, parent(x));
		    x = *root;
		    break;
		}
	    }
	    else {
		RBT_ASSERT(x == parent(x)->right);
		y = parent(x)->left;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(parent(x)));
		    SET_RED(parent(x));
		    right_rotate(root, parent(x));
		    y = parent(x)->left;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->right) && IS_BLACK(y->left)) {
		    SET_RED(y);
		    x = parent(x);
		}
		else {
		    if (IS_BLACK(y->left)) {
			SET_BLACK(y->right);
			SET_RED(y);
			left_rotate(root, y);
			y = parent(x)->left;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(parent(x))) {
			SET_BLACK(parent(x));
			SET_RED(y);
		    }
		    RBT_ASSERT(y->left);
		    SET_BLACK(y->left);
		    right_rotate(root, parent(x));
		    x = *root;
		    break;
		}
	    }
	}
	SET_BLACK(x);

	if (parent(&null_x)) {
	    if (parent(&null_x)->left == &null_x)
		parent(&null_x)->left = NULL;
	    else {
		RBT_ASSERT(parent(&null_x)->right == &null_x);
		parent(&null_x)->right = NULL;
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

    /*blk->flags  = 0;*/
    blk->left	= NULL;
    blk->right	= NULL;
    /*SVERK blk->max_sz = blk_sz;*/

    if (!*root) {
	blk->parent_and_color = parent_and_color(NULL, !RED_FLG);
        /*SET_BLACK(blk);*/
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
		    blk->parent_and_color = parent_and_color(x, RED_FLG);
		    x->left = blk;
		    break;
		}
		x = x->left;
	    }
	    else {
                RBT_ASSERT(diff > 0);
                IF_RBT_DEBUG(dbg_under = node_to_desc(order, x));
                if (!x->right) {
                    blk->parent_and_color = parent_and_color(x, RED_FLG);
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
	RBT_ASSERT(parent(blk));
#ifdef RBT_DEBUG
        if (!order) {
            RBT_ASSERT(!dbg_under || dbg_under->end < desc->start);
            RBT_ASSERT(!dbg_over || dbg_over->start > desc->end);
        }
#endif
	/*SET_RED(blk);*/
        RBT_ASSERT(IS_RED(blk));
	if (IS_RED(parent(blk)))
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

#if ERTS_HAVE_OS_MMAP
/* Implementation of os_mmap()/os_munmap()/os_mremap()... */

#if HAVE_MMAP
#  define ERTS_MMAP_PROT		(PROT_READ|PROT_WRITE)
#  if defined(MAP_ANONYMOUS)
#    define ERTS_MMAP_FLAGS		(MAP_ANON|MAP_PRIVATE)
#    define ERTS_MMAP_FD		(-1)
#  elif defined(MAP_ANON)
#    define ERTS_MMAP_FLAGS		(MAP_ANON|MAP_PRIVATE)
#    define ERTS_MMAP_FD		(-1)
#  else
#    define ERTS_MMAP_FLAGS		(MAP_PRIVATE)
#    define ERTS_MMAP_FD		mmap_state.mmap_fd
#  endif
#endif

static ERTS_INLINE void *
os_mmap(UWord size, int try_superalign)
{
#if HAVE_MMAP
    void *res;
#ifdef MAP_ALIGN
    if (try_superalign)
	res = mmap((void *) ERTS_SUPERALIGNED_SIZE, size, ERTS_MMAP_PROT,
		   ERTS_MMAP_FLAGS|MAP_ALIGN, ERTS_MMAP_FD, 0);
    else
#endif
	res = mmap((void *) 0, size, ERTS_MMAP_PROT,
		   ERTS_MMAP_FLAGS, ERTS_MMAP_FD, 0);
    if (res == MAP_FAILED)
	return NULL;
    return res;
#elif HAVE_VIRTUALALLOC
    return (void *) VirtualAlloc(NULL, (SIZE_T) size,
				 MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
#else
#  error "missing mmap() or similar"
#endif
}

static ERTS_INLINE void
os_munmap(void *ptr, UWord size)
{
#if HAVE_MMAP
#ifdef ERTS_MMAP_DEBUG
    int res =
#endif
	munmap(ptr, size);
    ERTS_MMAP_ASSERT(res == 0);
#elif HAVE_VIRTUALALLOC
#ifdef DEBUG
    BOOL res =
#endif
	VirtualFree((LPVOID) ptr, (SIZE_T) 0, MEM_RELEASE);
    ERTS_MMAP_ASSERT(res != 0);
#else
#  error "missing munmap() or similar"
#endif
}

#ifdef ERTS_HAVE_OS_MREMAP
#  if HAVE_MREMAP
#    if defined(__NetBSD__)
#      define ERTS_MREMAP_FLAGS  (0)
#    else
#      define ERTS_MREMAP_FLAGS  (MREMAP_MAYMOVE)
#    endif
#  endif
static ERTS_INLINE void *
os_mremap(void *ptr, UWord old_size, UWord new_size, int try_superalign)
{
    void *new_seg;
#if HAVE_MREMAP
    new_seg = mremap(ptr, (size_t) old_size,
#  if defined(__NetBSD__)
		     NULL,
#  endif
		     (size_t) new_size, ERTS_MREMAP_FLAGS);
    if (new_seg == (void *) MAP_FAILED)
	return NULL;
    return new_seg;
#else
#  error "missing mremap() or similar"
#endif
}
#endif

#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
#if HAVE_MMAP

#define ERTS_MMAP_RESERVE_PROT		(ERTS_MMAP_PROT)
#define ERTS_MMAP_RESERVE_FLAGS		(ERTS_MMAP_FLAGS|MAP_FIXED)
#define ERTS_MMAP_UNRESERVE_PROT	(PROT_NONE)
#define ERTS_MMAP_UNRESERVE_FLAGS	(ERTS_MMAP_FLAGS|MAP_NORESERVE|MAP_FIXED)
#define ERTS_MMAP_VIRTUAL_PROT		(PROT_NONE)
#define ERTS_MMAP_VIRTUAL_FLAGS		(ERTS_MMAP_FLAGS|MAP_NORESERVE)

static int
os_reserve_physical(char *ptr, UWord size)
{
    void *res = mmap((void *) ptr, (size_t) size, ERTS_MMAP_RESERVE_PROT,
		     ERTS_MMAP_RESERVE_FLAGS, ERTS_MMAP_FD, 0);
    if (res == (void *) MAP_FAILED)
	return 0;
    return 1;
}

static void
os_unreserve_physical(char *ptr, UWord size)
{
    void *res = mmap((void *) ptr, (size_t) size, ERTS_MMAP_UNRESERVE_PROT,
		     ERTS_MMAP_UNRESERVE_FLAGS, ERTS_MMAP_FD, 0);
    if (res == (void *) MAP_FAILED)
	erl_exit(ERTS_ABORT_EXIT, "Failed to unreserve memory");
}

static void *
os_mmap_virtual(char *ptr, UWord size)
{
    void *res = mmap((void *) ptr, (size_t) size, ERTS_MMAP_VIRTUAL_PROT,
		     ERTS_MMAP_VIRTUAL_FLAGS, ERTS_MMAP_FD, 0);
    if (res == (void *) MAP_FAILED)
	return NULL;
    return res;
}

#else
#error "Missing reserve/unreserve physical memory implementation"
#endif
#endif /* ERTS_HAVE_OS_RESERVE_PHYSICAL_MEMORY */

#endif /* ERTS_HAVE_OS_MMAP */

static int reserve_noop(char *ptr, UWord size)
{
#ifdef ERTS_MMAP_DEBUG_FILL_AREAS
    Uint32 *uip, *end = (Uint32 *) (ptr + size);

    for (uip = (Uint32 *) ptr; uip < end; uip++)
	ERTS_MMAP_ASSERT(*uip == (Uint32) 0xdeadbeef);
    for (uip = (Uint32 *) ptr; uip < end; uip++)
	*uip = (Uint32) 0xfeedfeed;
#endif
    return 1;
}

static void unreserve_noop(char *ptr, UWord size)
{
#ifdef ERTS_MMAP_DEBUG_FILL_AREAS
    Uint32 *uip, *end = (Uint32 *) (ptr + size);

    for (uip = (Uint32 *) ptr; uip < end; uip++)
	*uip = (Uint32) 0xdeadbeef;
#endif
}

void *
erts_mmap(Uint32 flags, UWord *sizep)
{
    char *seg;
    UWord asize = ERTS_PAGEALIGNED_CEILING(*sizep);

    /* Map in premapped supercarrier */
    if (mmap_state.supercarrier && !(ERTS_MMAPFLG_OS_ONLY & flags)) {
	char *end;
	ErtsFreeSegDesc *desc;
	Uint32 superaligned = (ERTS_MMAPFLG_SUPERALIGNED & flags);

	erts_smp_mtx_lock(&mmap_state.mtx);

	if (!superaligned) {
	    desc = lookup_free_seg(&mmap_state.sua.map, asize);
	    if (desc) {
		seg = desc->start;
		end = seg+asize;
		if (!mmap_state.reserve_physical(seg, asize))
		    goto supercarrier_reserve_failure;
		if (desc->end == end) {
		    delete_free_seg(&mmap_state.sua.map, desc);
		    free_desc(desc);
		}
		else {
		    ERTS_MMAP_ASSERT(end < desc->end);
		    resize_free_seg(&mmap_state.sua.map, desc, end, desc->end);
		}
		ERTS_MMAP_SIZE_SC_SUA_INC(asize);
		goto supercarrier_success;
	    }

	    if (asize <= mmap_state.sua.bot - mmap_state.sa.top) {
		if (!mmap_state.reserve_physical(mmap_state.sua.bot - asize,
						 asize))
		    goto supercarrier_reserve_failure;
		mmap_state.sua.bot -= asize;
		seg = mmap_state.sua.bot;
		ERTS_MMAP_SIZE_SC_SUA_INC(asize);
		goto supercarrier_success;
	    }
	}

	asize = ERTS_SUPERALIGNED_CEILING(asize);

	desc = lookup_free_seg(&mmap_state.sa.map, asize);
	if (desc) {
	    seg = desc->start;
	    end = seg+asize;
	    if (!mmap_state.reserve_physical(seg, asize))
		goto supercarrier_reserve_failure;
	    if (desc->end == end) {
		delete_free_seg(&mmap_state.sa.map, desc);
		free_desc(desc);
	    }
	    else {
		ERTS_MMAP_ASSERT(end < desc->end);
		resize_free_seg(&mmap_state.sa.map, desc, end, desc->end);
	    }
	    ERTS_MMAP_SIZE_SC_SA_INC(asize);
	    goto supercarrier_success;
	}

	if (superaligned) {

	    if (asize <= mmap_state.sua.bot - mmap_state.sa.top) {
		seg = (void *) mmap_state.sa.top;
		if (!mmap_state.reserve_physical(seg, asize))
		    goto supercarrier_reserve_failure;
		mmap_state.sa.top += asize;
		ERTS_MMAP_SIZE_SC_SA_INC(asize);
		goto supercarrier_success;
	    }

	    desc = lookup_free_seg(&mmap_state.sua.map, asize + ERTS_SUPERALIGNED_SIZE);
	    if (desc) {
		char *org_start = desc->start;
		char *org_end = desc->end;

		seg = (char *) ERTS_SUPERALIGNED_CEILING(org_start);
		end = seg + asize;
		if (!mmap_state.reserve_physical(seg, asize))
		    goto supercarrier_reserve_failure;
		if (org_start != seg) {
		    ERTS_MMAP_ASSERT(org_start < seg);
		    resize_free_seg(&mmap_state.sua.map, desc, org_start, seg);
		    desc = NULL;
		}
		if (end != org_end) {
		    ERTS_MMAP_ASSERT(end < org_end);
		    if (desc)
			resize_free_seg(&mmap_state.sua.map, desc, end, org_end);
		    else {
			desc = alloc_desc();
			if (!desc)
			    add_free_desc_area(end, org_end);
			else
			    insert_free_seg(&mmap_state.sua.map, desc, end, org_end);
		    }
		}
		ERTS_MMAP_SIZE_SC_SA_INC(asize);
		goto supercarrier_success;
	    }
	}

	erts_smp_mtx_unlock(&mmap_state.mtx);
    }

#if ERTS_HAVE_OS_MMAP
    /* Map using OS primitives */
    if (!(ERTS_MMAPFLG_SUPERCARRIER_ONLY & flags) && !mmap_state.no_os_mmap) {
	if (!(ERTS_MMAPFLG_SUPERALIGNED & flags)) {
	    seg = os_mmap(asize, 0);
	    if (!seg)
		return NULL;
	}
	else {
	    asize = ERTS_SUPERALIGNED_CEILING(*sizep);
	    seg = os_mmap(asize, 1);
	    if (!seg)
		return NULL;

	    if (!ERTS_IS_SUPERALIGNED(seg)) {
		char *ptr;
		UWord sz;

		os_munmap(seg, asize);

		ptr = os_mmap(asize + ERTS_SUPERALIGNED_SIZE, 1);
		if (!ptr)
		    return NULL;

		seg = (char *) ERTS_SUPERALIGNED_CEILING(ptr);
		sz = (UWord) (seg - ptr);
		ERTS_MMAP_ASSERT(sz <= ERTS_SUPERALIGNED_SIZE);
		if (sz)
		    os_munmap(ptr, sz);
		sz = ERTS_SUPERALIGNED_SIZE - sz; 
		if (sz)
		    os_munmap(seg+asize, sz);
	    }
	}

	ERTS_MMAP_SIZE_OS_INC(asize);
	*sizep = asize;
	return (void *) seg;
    }
#endif
    *sizep = 0;
    return NULL;

supercarrier_success:

#ifdef ERTS_MMAP_DEBUG
    if ((ERTS_MMAPFLG_SUPERALIGNED & flags)
	|| ERTS_MMAP_IN_SUPERALIGNED_AREA(seg)) {
	ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(seg));
	ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(asize));
    }
    else {
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(seg));
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));
    }
#endif

    erts_smp_mtx_unlock(&mmap_state.mtx);

    *sizep = asize;
    return (void *) seg;

supercarrier_reserve_failure:
    erts_smp_mtx_unlock(&mmap_state.mtx);

    *sizep = 0;
    return NULL;

}

void
erts_munmap(Uint32 flags, void **ptrp, UWord *sizep)
{
    void *ptr = *ptrp;
    UWord size = *sizep;
    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(size));
    if (!ERTS_MMAP_IN_SUPERCARRIER(ptr)) {
	ERTS_MMAP_ASSERT(!mmap_state.no_os_mmap);
#if ERTS_HAVE_OS_MMAP
	ERTS_MMAP_SIZE_OS_DEC(size);
	os_munmap(ptr, size);
#endif
    }
    else {
	char *start, *end;
	ErtsFreeSegMap *map;
	ErtsFreeSegDesc *prev, *next, *desc;

	ERTS_MMAP_ASSERT(mmap_state.supercarrier);

	start = (char *) ptr;
	end = start + size;

	erts_smp_mtx_lock(&mmap_state.mtx);

	if (ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)) {

	    start = (char *) ERTS_SUPERALIGNED_CEILING(start);
	    end = (char *) ERTS_SUPERALIGNED_FLOOR(end);

	    size = (UWord) (end - start);
	    *ptrp = start;
	    *sizep = size;

	    map = &mmap_state.sa.map;
	    adjacent_free_seg(map, start, end, &prev, &next);

	    ERTS_MMAP_SIZE_SC_SA_DEC(size);
	    if (end == mmap_state.sa.top) {
		ERTS_MMAP_ASSERT(!next);
		if (prev) {
		    start = prev->start;
		    delete_free_seg(map, prev);
		    free_desc(prev);
		}
		mmap_state.sa.top = start;
		goto supercarrier_success;
	    }
	}
	else {
	    ERTS_MMAP_ASSERT(ERTS_MMAP_IN_SUPERUNALIGNED_AREA(ptr));

	    map = &mmap_state.sua.map;
	    adjacent_free_seg(map, start, end, &prev, &next);

	    ERTS_MMAP_SIZE_SC_SUA_DEC(size);
	    if (start == mmap_state.sua.bot) {
		ERTS_MMAP_ASSERT(!prev);
		if (next) {
		    end = next->end;
		    delete_free_seg(map, next);
		    free_desc(next);
		}
		mmap_state.sua.bot = end;
		goto supercarrier_success;
	    }
	}

	desc = NULL;

	if (next) {
	    ERTS_MMAP_ASSERT(end < next->end);
	    end = next->end;
	    if (prev) {
		delete_free_seg(map, next);
		free_desc(next);
		goto save_prev;
	    }
	    desc = next;
	} else if (prev) {
	save_prev:
	    ERTS_MMAP_ASSERT(prev->start < start);
	    start = prev->start;
	    desc = prev;
	}

	if (desc)
	    resize_free_seg(map, desc, start, end);
	else {
	    desc = alloc_desc();
	    if (desc)
		insert_free_seg(map, desc, start, end);
	    else {
		if (map == &mmap_state.sa.map)
		    ERTS_MMAP_SIZE_SC_SA_INC(size);
		else
		    ERTS_MMAP_SIZE_SC_SUA_INC(size);
		add_free_desc_area(start, end);
	    }
	}

    supercarrier_success:
	erts_smp_mtx_unlock(&mmap_state.mtx);

	mmap_state.unreserve_physical((char *) ptr, size);
    }
}

static void *
remap_move(Uint32 flags, void *ptr, UWord old_size, UWord *sizep)
{
    UWord size = *sizep;
    UWord um_size = old_size;
    void *um_ptr = ptr;
    void *new_ptr = erts_mmap(flags, &size);
    if (!new_ptr)
	return NULL;
    *sizep = size;
    if (old_size < size)
	size = old_size;
    sys_memcpy(new_ptr, ptr, (size_t) size);
    erts_munmap(flags, &um_ptr, &um_size);
    ERTS_MMAP_ASSERT(um_ptr == ptr);
    ERTS_MMAP_ASSERT(um_size == old_size);
    return new_ptr;
}

void *
erts_mremap(Uint32 flags, void *ptr, UWord old_size, UWord *sizep)
{
    void *new_ptr;
    Uint32 superaligned;
    UWord asize;

    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(old_size));
    ERTS_MMAP_ASSERT(sizep && ERTS_IS_PAGEALIGNED(*sizep));

    if (!ERTS_MMAP_IN_SUPERCARRIER(ptr)) {

	ERTS_MMAP_ASSERT(!mmap_state.no_os_mmap);

	if (!(ERTS_MMAPFLG_OS_ONLY & flags) && mmap_state.supercarrier) {
	    new_ptr = remap_move(ERTS_MMAPFLG_SUPERCARRIER_ONLY|flags, ptr,
				 old_size, sizep);
	    if (new_ptr)
		return new_ptr;
	}

	if (ERTS_MMAPFLG_SUPERCARRIER_ONLY & flags)
	    return NULL;

#if ERTS_HAVE_OS_MREMAP || ERTS_HAVE_GENUINE_OS_MMAP
	superaligned = (ERTS_MMAPFLG_SUPERALIGNED & flags);

	if (superaligned) {
	    asize = ERTS_SUPERALIGNED_CEILING(*sizep);
	    if (asize == old_size && ERTS_IS_SUPERALIGNED(ptr)) {
		*sizep = asize;
		return ptr;
	    }
	}
	else {
	    asize = ERTS_PAGEALIGNED_CEILING(*sizep);
	    if (asize == old_size) {
		*sizep = asize;
		return ptr;
	    }
	}

#if ERTS_HAVE_GENUINE_OS_MMAP
	if (asize < old_size
	    && (!superaligned
		|| ERTS_IS_SUPERALIGNED(ptr))) {
	    UWord um_sz;
	    new_ptr = ((char *) ptr) + asize;
	    ERTS_MMAP_ASSERT((((char *)ptr) + old_size) > (char *) new_ptr);
	    um_sz = (UWord) ((((char *) ptr) + old_size) - (char *) new_ptr); 
	    ERTS_MMAP_SIZE_OS_DEC(um_sz);
	    os_munmap(new_ptr, um_sz);
	    *sizep = asize;
	    return ptr;
	}
#endif
#if ERTS_HAVE_OS_MREMAP
	if (superaligned)
	    return remap_move(flags, new_ptr, old_size, sizep);
	else {
	    new_ptr = os_mremap(ptr, old_size, asize, 0);
	    if (!new_ptr)
		return NULL;
	    if (asize > old_size)
		ERTS_MMAP_SIZE_OS_INC(asize - old_size);
	    else
		ERTS_MMAP_SIZE_OS_DEC(old_size - asize);
	    *sizep = asize;
	    return new_ptr;
	}
#endif
#endif
    }
    else { /* In super carrier */
	char *start, *end, *new_end;
	ErtsFreeSegMap *map;
	ErtsFreeSegDesc *prev, *next, *desc;

	ERTS_MMAP_ASSERT(mmap_state.supercarrier);

	if (ERTS_MMAPFLG_OS_ONLY & flags)
	    return remap_move(flags, ptr, old_size, sizep);

	superaligned = (ERTS_MMAPFLG_SUPERALIGNED & flags);

	asize = (superaligned
		 ? ERTS_SUPERALIGNED_CEILING(*sizep)
		 : ERTS_PAGEALIGNED_CEILING(*sizep));

	erts_smp_mtx_lock(&mmap_state.mtx);

	if (ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)
	    ? (!superaligned && lookup_free_seg(&mmap_state.sua.map, asize))
	    : (superaligned && lookup_free_seg(&mmap_state.sa.map, asize))) {
	    erts_smp_mtx_unlock(&mmap_state.mtx);
	    /*
	     * Segment currently in wrong area (due to a previous memory
	     * shortage), move it to the right area.
	     * (remap_move() will succeed)
	     */
	    return remap_move(ERTS_MMAPFLG_SUPERCARRIER_ONLY|flags, ptr,
			      old_size, sizep);
	}

	if (asize == old_size) {
	    new_ptr = ptr;
	    goto supercarrier_resize_success;
	}

	start = (char *) ptr;
	end = start + old_size;
	new_end = start+asize;

	if (asize < old_size) {
	    new_ptr = ptr;
	    if (!ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)) {
		ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
		ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(old_size));
		ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));
		map = &mmap_state.sua.map;
		ERTS_MMAP_SIZE_SC_SUA_DEC(old_size - asize);
	    }
	    else {
		ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(ptr));
		ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(old_size));
		if (!superaligned) {
		    /* must be a superaligned size in this area */
		    asize = ERTS_SUPERALIGNED_CEILING(asize);
		    ERTS_MMAP_ASSERT(asize <= old_size);
		    if (asize == old_size)
			goto supercarrier_resize_success;
		    new_end = start+asize;
		}
		ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(asize));
		if (end == mmap_state.sa.top) {
		    mmap_state.sa.top = new_end;
		    mmap_state.unreserve_physical(((char *) ptr) + asize,
						  old_size - asize);
		    goto supercarrier_resize_success;
		}
		ERTS_MMAP_SIZE_SC_SA_DEC(old_size - asize);
		map = &mmap_state.sa.map;
	    }
	    
	    adjacent_free_seg(map, start, end, &prev, &next);

	    if (next)
		resize_free_seg(map, next, new_end, next->end);
	    else {
		desc = alloc_desc();
		if (desc)
		    insert_free_seg(map, desc, new_end, end);
		else {
		    if (map == &mmap_state.sa.map)
			ERTS_MMAP_SIZE_SC_SA_INC(old_size - asize);
		    else
			ERTS_MMAP_SIZE_SC_SUA_INC(old_size - asize);
		    add_free_desc_area(new_end, end);
		    goto supercarrier_resize_success;
		}
	    }
	    mmap_state.unreserve_physical(((char *) ptr) + asize,
					  old_size - asize);
	    goto supercarrier_resize_success;
	}

	if (!ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)) {
	    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
	    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(old_size));
	    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));

	    adjacent_free_seg(&mmap_state.sua.map, start, end, &prev, &next);

	    if (next && new_end <= next->end) {
		if (!mmap_state.reserve_physical(((char *) ptr) + old_size,
						 asize - old_size))
		    goto supercarrier_reserve_failure;
		if (new_end < next->end)
		    resize_free_seg(&mmap_state.sua.map, next, new_end, next->end);
		else {
		    delete_free_seg(&mmap_state.sua.map, next);
		    free_desc(next);
		}
		new_ptr = ptr;
		ERTS_MMAP_SIZE_SC_SUA_INC(asize - old_size);
		goto supercarrier_resize_success;
	    }
	}
	else { /* Superaligned area */
	    ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(ptr));
	    ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(old_size));

	    if (!superaligned) {
		/* must be a superaligned size in this area */
		asize = ERTS_PAGEALIGNED_CEILING(asize);
		new_end = start+asize;
	    }

	    ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(asize));

	    if (end == mmap_state.sa.top) {
		if (new_end <= mmap_state.sua.bot) {
		    if (!mmap_state.reserve_physical(((char *) ptr) + old_size,
						     asize - old_size))
			goto supercarrier_reserve_failure;
		    mmap_state.sa.top = new_end;
		    new_ptr = ptr;
		    ERTS_MMAP_SIZE_SC_SA_INC(asize - old_size);
		    goto supercarrier_resize_success;
		}
	    }
	    else {
		adjacent_free_seg(&mmap_state.sa.map, start, end, &prev, &next);
		if (next && new_end <= next->end) {
		    if (!mmap_state.reserve_physical(((char *) ptr) + old_size,
						     asize - old_size))
			goto supercarrier_reserve_failure;
		    if (new_end < next->end)
			resize_free_seg(&mmap_state.sa.map, next, new_end, next->end);
		    else {
			delete_free_seg(&mmap_state.sa.map, next);
			free_desc(next);
		    }
		    new_ptr = ptr;
		    ERTS_MMAP_SIZE_SC_SA_INC(asize - old_size);
		    goto supercarrier_resize_success;
		}
	    }
	}
	erts_smp_mtx_unlock(&mmap_state.mtx);

	/* Failed to resize... */
    }

    return remap_move(flags, ptr, old_size, sizep);

supercarrier_resize_success:

#ifdef ERTS_MMAP_DEBUG
    if ((ERTS_MMAPFLG_SUPERALIGNED & flags)
	|| ERTS_MMAP_IN_SUPERALIGNED_AREA(new_ptr)) {
	ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(new_ptr));
	ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(asize));
    }
    else {
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(new_ptr));
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));
    }
#endif

    erts_smp_mtx_unlock(&mmap_state.mtx);

    *sizep = asize;
    return new_ptr;

supercarrier_reserve_failure:

    erts_smp_mtx_unlock(&mmap_state.mtx);
    *sizep = 0;
    return NULL;
    
}

int erts_mmap_in_supercarrier(void *ptr)
{
    return ERTS_MMAP_IN_SUPERCARRIER(ptr);
}

void
erts_mmap_init(ErtsMMapInit *init)
{
    int virtual_map = 0;
    char *start = NULL, *end = NULL;
    UWord pagesize;
#if defined(__WIN32__)
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    pagesize = (UWord) sysinfo.dwPageSize;
#elif defined(_SC_PAGESIZE)
    pagesize = (UWord) sysconf(_SC_PAGESIZE);
#elif defined(HAVE_GETPAGESIZE)
    pagesize = (UWord) getpagesize();
#else
#  error "Do not know how to get page size"
#endif
#if defined(HARD_DEBUG)  || 0
    erts_fprintf(stderr, "erts_mmap: scs = %bpu\n", init->scs);
    erts_fprintf(stderr, "erts_mmap: sco = %i\n", init->sco);
    erts_fprintf(stderr, "erts_mmap: scmgc = %i\n", init->scmgc);
#endif
    erts_page_inv_mask = pagesize - 1;
    if (pagesize & erts_page_inv_mask)
	erl_exit(-1, "erts_mmap: Invalid pagesize: %bpu\n",
		 pagesize);

    erts_have_erts_mmap = 0;

    mmap_state.reserve_physical = reserve_noop;
    mmap_state.unreserve_physical = unreserve_noop;

#if HAVE_MMAP && !defined(MAP_ANON)
    mmap_state.mmap_fd = open("/dev/zero", O_RDWR);
    if (mmap_state.mmap_fd < 0)
	erl_exit(-1, "erts_mmap: Failed to open /dev/zero\n");
#endif

    erts_smp_mtx_init(&mmap_state.mtx, "erts_mmap");

#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
    if (init->virtual_range.start) {
	char *ptr;
	UWord sz;
	ptr = (char *) ERTS_PAGEALIGNED_CEILING(init->virtual_range.start);
	end = (char *) ERTS_PAGEALIGNED_FLOOR(init->virtual_range.end);
	sz = end - ptr;
	start = os_mmap_virtual(ptr, sz);
	if (!start || start > ptr || start >= end)
	    erl_exit(-1,
		     "erts_mmap: Failed to create virtual range for super carrier\n");
	sz = start - ptr;
	if (sz)
	    os_munmap(end, sz);
	mmap_state.reserve_physical = os_reserve_physical;
	mmap_state.unreserve_physical = os_unreserve_physical;
	virtual_map = 1;
    }
    else
#endif
	if (init->predefined_area.start) {
	    start = init->predefined_area.start;
	    end = init->predefined_area.end;
	    if (end != (void *) 0 && end < start)
		end = start;
    }
#if ERTS_HAVE_OS_MMAP
    else if (init->scs) {
	UWord sz;
	sz = ERTS_PAGEALIGNED_CEILING(init->scs);
#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
	if (!init->scrpm) {
	    start = os_mmap_virtual(NULL, sz);
	    mmap_state.reserve_physical = os_reserve_physical;
	    mmap_state.unreserve_physical = os_unreserve_physical;
	    virtual_map = 1;
	}
	else
#endif
	{
	    /*
	     * The whole supercarrier will by physically
	     * reserved all the time.
	     */
	    start = os_mmap(sz, 1);
	}
	if (!start)
	    erl_exit(-1,
		     "erts_mmap: Failed to create super carrier of size %bpu MB\n",
		     init->scs/1024/1024);
	end = start + sz;
#ifdef ERTS_MMAP_DEBUG_FILL_AREAS
	if (!virtual_map) {
	    Uint32 *uip;

	    for (uip = (Uint32 *) start; uip < (Uint32 *) end; uip++)
		*uip = (Uint32) 0xdeadbeef;
	}
#endif
    }
    if (!mmap_state.no_os_mmap)
	erts_have_erts_mmap |= ERTS_HAVE_ERTS_OS_MMAP;
#endif

    mmap_state.size.supercarrier.total = 0;
    mmap_state.size.supercarrier.used.total = 0;
    mmap_state.size.supercarrier.used.sa = 0;
    mmap_state.size.supercarrier.used.sua = 0;
    mmap_state.size.os.used = 0;

    if (!start) {
	mmap_state.sa.bot = NULL;
	mmap_state.sua.top = NULL;
	mmap_state.sa.bot = NULL;
	mmap_state.sua.top = NULL;
	mmap_state.no_os_mmap = 0;
    }
    else {
	size_t desc_size;

	mmap_state.no_os_mmap = init->sco;

	desc_size = init->scmgc;
	if (desc_size < 100)
	    desc_size = 100;
	desc_size *= sizeof(ErtsFreeSegDesc);
	if ((desc_size
	     + ERTS_SUPERALIGNED_SIZE
	     + ERTS_PAGEALIGNED_SIZE) > end - start)
	    erl_exit(-1, "erts_mmap: No space for segments in super carrier\n");

	mmap_state.sa.bot = start;
	mmap_state.sa.bot += desc_size;
	mmap_state.sa.bot = (char *) ERTS_SUPERALIGNED_CEILING(mmap_state.sa.bot);
	mmap_state.sa.top = mmap_state.sa.bot;
	mmap_state.sua.top = (char *) ERTS_SUPERALIGNED_FLOOR(end);
	mmap_state.sua.bot = mmap_state.sua.top;

	mmap_state.size.os.used += (UWord) (mmap_state.sa.bot - start);

	if (end == (void *) 0) {
	    /*
	     * Very unlikely, but we need a guarantee
	     * that `mmap_state.sua.top` always will
	     * compare as larger than all segment pointers
	     * into the super carrier...
	     */
	    mmap_state.sua.top -= ERTS_PAGEALIGNED_SIZE;
	    mmap_state.size.os.used += ERTS_PAGEALIGNED_SIZE;
	}

	mmap_state.size.supercarrier.total = (UWord) (mmap_state.sua.top - mmap_state.sa.bot);

	/*
	 * Area before (and after) super carrier
	 * will be used for free segment descritors.
	 */
	mmap_state.desc_free_list = NULL;
#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
	if (virtual_map && mmap_state.sa.bot - start > 0)
	    os_reserve_physical(start, mmap_state.sa.bot - start);
#endif
	add_free_desc_area(start, mmap_state.sa.bot);
#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
	if (virtual_map && end - mmap_state.sua.top > 0)
	    os_reserve_physical(mmap_state.sua.top, end - mmap_state.sua.top);
#endif
	add_free_desc_area(mmap_state.sua.top, end);

	init_free_seg_map(&mmap_state.sa.map, 0);
	init_free_seg_map(&mmap_state.sua.map, 1);

	mmap_state.supercarrier = 1;
	erts_have_erts_mmap |= ERTS_HAVE_ERTS_SUPERCARRIER_MMAP;
    }

#if !ERTS_HAVE_OS_MMAP
    mmap_state.no_os_mmap = 1;
#endif
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */


#ifdef HARD_DEBUG

static int rbt_assert_is_member(RBTNode* root, RBTNode* node)
{
    while (node != root) {
	RBT_ASSERT(parent(node));
	RBT_ASSERT(parent(node)->left == node || parent(node)->right == node);
	node = parent(node);
    }
    return 1;
}


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
    enum { RECURSE_LEFT, CHECK_NODE, RECURSE_RIGHT, RETURN_TO_PARENT }state;

#ifdef PRINT_TREE
    print_tree(tree->order, tree->root);
#endif

    if (!tree->root)
	return res;

    x = tree->root;
    RBT_ASSERT(IS_BLACK(x));
    RBT_ASSERT(!parent(x));
    curr_blacks = 1;
    blacks = -1;
    depth = 1;
    max_depth = 0;
    node_cnt = 0;
    state = RECURSE_LEFT;

    /* Traverse tree in sorting order */
    while (x) {
        switch (state) {
        case RECURSE_LEFT:
            if (x->left) {
                RBT_ASSERT(parent(x->left) == x);
                x = x->left;
                ++depth;
                if (IS_BLACK(x))
                    curr_blacks++;
                state = RECURSE_LEFT;
            }
            else {
                if (blacks < 0)
                    blacks = curr_blacks;
                RBT_ASSERT(blacks == curr_blacks);
                state = CHECK_NODE;
            }
            break;

        case CHECK_NODE:
            ++node_cnt;
            if (depth > max_depth)
                max_depth = depth;

            if (IS_RED(x)) {
                RBT_ASSERT(IS_BLACK(x->right));
                RBT_ASSERT(IS_BLACK(x->left));
            }

            RBT_ASSERT(parent(x) || x == tree->root);

            if (x->left) {
                RBT_ASSERT(cmp_blocks(tree->order, x->left, x) < 0);
            }
            if (x->right) {
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
            state = RECURSE_RIGHT;
            break;

        case RECURSE_RIGHT:
            if (x->right) {
                RBT_ASSERT(parent(x->right) == x);
                x = x->right;
                ++depth;
                if (IS_BLACK(x))
                    curr_blacks++;
                state = RECURSE_LEFT;
            }
            else {
                if (blacks < 0)
                    blacks = curr_blacks;
                RBT_ASSERT(blacks == curr_blacks);
                state = RETURN_TO_PARENT;
            }
            break;

        case RETURN_TO_PARENT:
            if (parent(x)) {
                if (x == parent(x)->left) {
                    state = CHECK_NODE;
                }
                else {
                    RBT_ASSERT(x == parent(x)->right);
                    state = RETURN_TO_PARENT;
                }
            }
            if (IS_BLACK(x))
                curr_blacks--;
            x = parent(x);
            --depth;
            break;
        }
    }
    RBT_ASSERT(depth == 0 || (!tree->root && depth==1));
    RBT_ASSERT(curr_blacks == 0);
    RBT_ASSERT((1 << (max_depth/2)) <= node_cnt);

    return res;
}

#endif /* HARD_DEBUG */


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

void test_it(void)
{
    ErtsFreeSegMap map;
    ErtsFreeSegDesc *desc, *under, *over, *d1, *d2;
    int i;

    for (i=0; i<2; i++) {
        init_free_seg_map(&map, i);

        insert_free_seg(&map, alloc_desc(), (char*)0x11000, (char*)0x12000);
        HARD_CHECK_TREE(&map.atree, 0); HARD_CHECK_TREE(&map.stree, 0);
        insert_free_seg(&map, alloc_desc(), (char*)0x13000, (char*)0x14000);
        HARD_CHECK_TREE(&map.atree, 0); HARD_CHECK_TREE(&map.stree, 0);
        insert_free_seg(&map, alloc_desc(), (char*)0x15000, (char*)0x17000);
        HARD_CHECK_TREE(&map.atree, 0); HARD_CHECK_TREE(&map.stree, 0);
        insert_free_seg(&map, alloc_desc(), (char*)0x8000, (char*)0x10000);
        HARD_CHECK_TREE(&map.atree, 0); HARD_CHECK_TREE(&map.stree, 0);

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
        HARD_CHECK_TREE(&map.atree, 0); HARD_CHECK_TREE(&map.stree, 0);

        d2 = lookup_free_seg(&map, 0x1200);
        ERTS_ASSERT(d2 == d1);

        delete_free_seg(&map, d1);
        HARD_CHECK_TREE(&map.atree, 0); HARD_CHECK_TREE(&map.stree, 0);

        d1 = lookup_free_seg(&map, 0x1200);
        ERTS_ASSERT(d1->start == (char*)0x15000);
    }
}
