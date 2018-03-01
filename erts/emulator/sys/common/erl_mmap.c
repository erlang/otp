/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_WANT_MEM_MAPPERS
#include "sys.h"
#include "erl_process.h"
#include "atom.h"
#include "erl_mmap.h"
#include <stddef.h>

#if HAVE_ERTS_MMAP

/* #define ERTS_MMAP_OP_RINGBUF_SZ 100 */

#if defined(DEBUG) || 0
#  undef ERTS_MMAP_DEBUG
#  define ERTS_MMAP_DEBUG
#  ifndef ERTS_MMAP_OP_RINGBUF_SZ
#    define ERTS_MMAP_OP_RINGBUF_SZ 100
#  endif
#endif

#ifndef ERTS_MMAP_OP_RINGBUF_SZ
#  define ERTS_MMAP_OP_RINGBUF_SZ 0
#endif

/* #define ERTS_MMAP_DEBUG_FILL_AREAS */

#ifdef ERTS_MMAP_DEBUG
#  define ERTS_MMAP_ASSERT ERTS_ASSERT
#else
#  define ERTS_MMAP_ASSERT(A) ((void) 1)
#endif

/*
 * `mm->sa.bot` and `mm->sua.top` are read only after
 * initialization, but the other pointers are not; i.e., only
 * ERTS_MMAP_IN_SUPERCARRIER() is allowed without the mutex held.
 */
#define ERTS_MMAP_IN_SUPERCARRIER(PTR)					\
    (((UWord) (PTR)) - ((UWord) mm->sa.bot)			\
     < ((UWord) mm->sua.top) - ((UWord) mm->sa.bot))
#define ERTS_MMAP_IN_SUPERALIGNED_AREA(PTR)				\
    (ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&mm->mtx)),	\
     (((UWord) (PTR)) - ((UWord) mm->sa.bot)			\
      < ((UWord) mm->sa.top) - ((UWord) mm->sa.bot)))
#define ERTS_MMAP_IN_SUPERUNALIGNED_AREA(PTR)				\
    (ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&mm->mtx)),	\
     (((UWord) (PTR)) - ((UWord) mm->sua.bot)			\
      < ((UWord) mm->sua.top) - ((UWord) mm->sua.bot)))

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
    SA_SZ_ADDR_ORDER,        /* first super-aligned size then address order */
    SZ_REVERSE_ADDR_ORDER   /* first size then reverse address order */
};
#ifdef HARD_DEBUG
static const char* sort_order_names[] = {"Address","SuperAlignedSize-Address","Size-RevAddress"};
#endif

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

#if ERTS_MMAP_OP_RINGBUF_SZ

static int mmap_op_ix;

typedef enum {
    ERTS_OP_TYPE_NONE,
    ERTS_OP_TYPE_MMAP,
    ERTS_OP_TYPE_MUNMAP,
    ERTS_OP_TYPE_MREMAP
} ErtsMMapOpType;

typedef struct {
    ErtsMMapOpType type;
    void *result;
    UWord in_size;
    UWord out_size;
    void *old_ptr;
    UWord old_size;
} ErtsMMapOp;

static ErtsMMapOp mmap_ops[ERTS_MMAP_OP_RINGBUF_SZ];

#define ERTS_MMAP_OP_RINGBUF_INIT()				\
    do {							\
	int ix__;						\
	for (ix__ = 0; ix__ < ERTS_MMAP_OP_RINGBUF_SZ; ix__++) {\
	    mmap_ops[ix__].type = ERTS_OP_TYPE_NONE;		\
	    mmap_ops[ix__].result = NULL;			\
	    mmap_ops[ix__].in_size = 0;				\
	    mmap_ops[ix__].out_size = 0;			\
	    mmap_ops[ix__].old_ptr = NULL;			\
	    mmap_ops[ix__].old_size = 0;			\
	}							\
	mmap_op_ix = ERTS_MMAP_OP_RINGBUF_SZ-1;			\
    } while (0)

#define ERTS_MMAP_OP_START(SZ) 					\
    do {							\
	int ix__;						\
	if (++mmap_op_ix >= ERTS_MMAP_OP_RINGBUF_SZ)		\
	    mmap_op_ix = 0;					\
	ix__ = mmap_op_ix;					\
	mmap_ops[ix__].type = ERTS_OP_TYPE_MMAP;		\
	mmap_ops[ix__].result = NULL;				\
	mmap_ops[ix__].in_size = (SZ);				\
	mmap_ops[ix__].out_size = 0;				\
	mmap_ops[ix__].old_ptr = NULL;				\
	mmap_ops[ix__].old_size = 0;				\
    } while (0)

#define ERTS_MMAP_OP_END(PTR, SZ)				\
    do {							\
	int ix__ = mmap_op_ix;					\
	mmap_ops[ix__].result = (PTR);				\
	mmap_ops[ix__].out_size = (SZ);				\
    } while (0)

#define ERTS_MMAP_OP_LCK(RES, IN_SZ, OUT_SZ)			\
    do {							\
	erts_mtx_lock(&mm->mtx);			\
	ERTS_MMAP_OP_START((IN_SZ));				\
	ERTS_MMAP_OP_END((RES), (OUT_SZ));			\
	erts_mtx_unlock(&mm->mtx);			\
    } while (0)

#define ERTS_MUNMAP_OP(PTR, SZ)					\
    do {							\
	int ix__;						\
	if (++mmap_op_ix >= ERTS_MMAP_OP_RINGBUF_SZ)		\
	    mmap_op_ix = 0;					\
	ix__ = mmap_op_ix;					\
	mmap_ops[ix__].type = ERTS_OP_TYPE_MUNMAP;		\
	mmap_ops[ix__].result = NULL;				\
	mmap_ops[ix__].in_size = 0;				\
	mmap_ops[ix__].out_size = 0;				\
	mmap_ops[ix__].old_ptr = (PTR);				\
	mmap_ops[ix__].old_size = (SZ);				\
    } while (0)

#define ERTS_MUNMAP_OP_LCK(PTR, SZ)				\
    do {							\
	erts_mtx_lock(&mm->mtx);			\
	ERTS_MUNMAP_OP((PTR), (SZ));				\
	erts_mtx_unlock(&mm->mtx);			\
    } while (0)

#define ERTS_MREMAP_OP_START(OLD_PTR, OLD_SZ, IN_SZ)		\
    do {							\
	int ix__;						\
	if (++mmap_op_ix >= ERTS_MMAP_OP_RINGBUF_SZ)		\
	    mmap_op_ix = 0;					\
	ix__ = mmap_op_ix;					\
	mmap_ops[ix__].type = ERTS_OP_TYPE_MREMAP;		\
	mmap_ops[ix__].result = NULL;				\
	mmap_ops[ix__].in_size = (IN_SZ);			\
	mmap_ops[ix__].out_size = (OLD_SZ);			\
	mmap_ops[ix__].old_ptr = (OLD_PTR);			\
	mmap_ops[ix__].old_size = (OLD_SZ);			\
    } while (0)

#define ERTS_MREMAP_OP_END(PTR, SZ)				\
    do {							\
	int ix__ = mmap_op_ix;					\
	mmap_ops[ix__].result = (PTR);				\
	mmap_ops[mmap_op_ix].out_size = (SZ);			\
    } while (0)

#define ERTS_MREMAP_OP_LCK(RES, OLD_PTR, OLD_SZ, IN_SZ, OUT_SZ)	\
    do {							\
	erts_mtx_lock(&mm->mtx);			\
	ERTS_MREMAP_OP_START((OLD_PTR), (OLD_SZ), (IN_SZ));	\
	ERTS_MREMAP_OP_END((RES), (OUT_SZ));			\
	erts_mtx_unlock(&mm->mtx);			\
    } while (0)

#define ERTS_MMAP_OP_ABORT()					\
    do {							\
	int ix__ = mmap_op_ix;					\
	mmap_ops[ix__].type = ERTS_OP_TYPE_NONE;		\
	mmap_ops[ix__].result = NULL;				\
	mmap_ops[ix__].in_size = 0;				\
	mmap_ops[ix__].out_size = 0;				\
	mmap_ops[ix__].old_ptr = NULL;				\
	mmap_ops[ix__].old_size = 0;				\
	if (--mmap_op_ix < 0)					\
	    mmap_op_ix = ERTS_MMAP_OP_RINGBUF_SZ-1;		\
    } while (0)

#else

#define ERTS_MMAP_OP_RINGBUF_INIT()
#define ERTS_MMAP_OP_START(SZ)
#define ERTS_MMAP_OP_END(PTR, SZ)
#define ERTS_MMAP_OP_LCK(RES, IN_SZ, OUT_SZ)
#define ERTS_MUNMAP_OP(PTR, SZ)
#define ERTS_MUNMAP_OP_LCK(PTR, SZ)
#define ERTS_MREMAP_OP_START(OLD_PTR, OLD_SZ, IN_SZ)
#define ERTS_MREMAP_OP_END(PTR, SZ)
#define ERTS_MREMAP_OP_LCK(RES, OLD_PTR, OLD_SZ, IN_SZ, OUT_SZ)
#define ERTS_MMAP_OP_ABORT()

#endif

typedef struct {
    RBTNode snode;      /* node in 'stree' */
    RBTNode anode;      /* node in 'atree' */
    char* start;
    char* end;
}ErtsFreeSegDesc;

typedef struct {
    RBTree stree;       /* size ordered tree */
    RBTree atree;       /* address ordered tree */
    Uint nseg;
}ErtsFreeSegMap;

struct ErtsMemMapper_ {
    int (*reserve_physical)(char *, UWord);
    void (*unreserve_physical)(char *, UWord);
    int supercarrier;
    int no_os_mmap;
    /*
     * Super unaligned area is located above super aligned
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
    erts_mtx_t mtx;
    struct {
	char *free_list;
	char *unused_start;
	char *unused_end;
	char *new_area_hint;
        Uint reserved;
    } desc;
    struct {
	UWord free_seg_descs;
	struct {
	    UWord curr;
	    UWord max;
	} free_segs;
    } no;
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
};

ErtsMemMapper erts_dflt_mmapper;

#if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
ErtsMemMapper erts_literal_mmapper;
char* erts_literals_start;
UWord erts_literals_size;
#endif


#define ERTS_MMAP_SIZE_SC_SA_INC(SZ) 						\
    do {									\
	mm->size.supercarrier.used.total += (SZ);			\
	mm->size.supercarrier.used.sa += (SZ);				\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.total		\
			 <= mm->size.supercarrier.total);		\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.sa			\
			 <= mm->size.supercarrier.used.total);		\
    } while (0)
#define ERTS_MMAP_SIZE_SC_SA_DEC(SZ) 						\
    do {									\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.total >= (SZ));	\
	mm->size.supercarrier.used.total -= (SZ);			\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.sa >= (SZ));		\
	mm->size.supercarrier.used.sa -= (SZ);				\
    } while (0)
#define ERTS_MMAP_SIZE_SC_SUA_INC(SZ) 						\
    do {									\
	mm->size.supercarrier.used.total += (SZ);			\
	mm->size.supercarrier.used.sua += (SZ);				\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.total		\
			 <= mm->size.supercarrier.total);		\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.sua			\
			 <= mm->size.supercarrier.used.total);		\
    } while (0)
#define ERTS_MMAP_SIZE_SC_SUA_DEC(SZ) 						\
    do {									\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.total >= (SZ));	\
	mm->size.supercarrier.used.total -= (SZ);			\
	ERTS_MMAP_ASSERT(mm->size.supercarrier.used.sua >= (SZ));	\
	mm->size.supercarrier.used.sua -= (SZ);				\
    } while (0)
#define ERTS_MMAP_SIZE_OS_INC(SZ)						\
    do {									\
	ERTS_MMAP_ASSERT(mm->size.os.used + (SZ) >= (SZ));		\
	mm->size.os.used += (SZ);					\
    } while (0)
#define ERTS_MMAP_SIZE_OS_DEC(SZ) 						\
    do {									\
	ERTS_MMAP_ASSERT(mm->size.os.used >= (SZ));			\
	mm->size.os.used -= (SZ);					\
    } while (0)


static void
add_free_desc_area(ErtsMemMapper* mm, char *start, char *end)
{
    ERTS_MMAP_ASSERT(end == (void *) 0 || end > start);
    if (sizeof(ErtsFreeSegDesc) <= ((UWord) end) - ((UWord) start)) {
	UWord no;
	ErtsFreeSegDesc *prev_desc, *desc;
	char *desc_end;

	no = 1;
	prev_desc = (ErtsFreeSegDesc *) start;
	prev_desc->start = mm->desc.free_list;
	desc = (ErtsFreeSegDesc *) (start + sizeof(ErtsFreeSegDesc));
	desc_end = start + 2*sizeof(ErtsFreeSegDesc);

	while (desc_end <= end) {
	    desc->start = (char *) prev_desc;
	    prev_desc = desc;
	    desc = (ErtsFreeSegDesc *) desc_end;
	    desc_end += sizeof(ErtsFreeSegDesc);
	    no++;
	}
	mm->desc.free_list = (char *) prev_desc;
	mm->no.free_seg_descs += no;
    }
}

static ErtsFreeSegDesc *
add_unused_free_desc_area(ErtsMemMapper* mm)
{
    char *ptr;
    if (!mm->desc.unused_start)
	return NULL;

    ERTS_MMAP_ASSERT(mm->desc.unused_end);
    ERTS_MMAP_ASSERT(ERTS_PAGEALIGNED_SIZE
		     <= mm->desc.unused_end - mm->desc.unused_start);

    ptr = mm->desc.unused_start + ERTS_PAGEALIGNED_SIZE;
    add_free_desc_area(mm, mm->desc.unused_start, ptr);

    if ((mm->desc.unused_end - ptr) >= ERTS_PAGEALIGNED_SIZE)
	mm->desc.unused_start = ptr;
    else
	mm->desc.unused_end = mm->desc.unused_start = NULL;

    ERTS_MMAP_ASSERT(mm->desc.free_list);
    return (ErtsFreeSegDesc *) mm->desc.free_list;
}

static ERTS_INLINE ErtsFreeSegDesc *
alloc_desc(ErtsMemMapper* mm)
{
    ErtsFreeSegDesc *res;
    res = (ErtsFreeSegDesc *) mm->desc.free_list;
    if (!res) {
	res = add_unused_free_desc_area(mm);
	if (!res)
	    return NULL;
    }
    mm->desc.free_list = res->start;
    ASSERT(mm->no.free_segs.curr < mm->no.free_seg_descs);
    mm->no.free_segs.curr++;
    if (mm->no.free_segs.max < mm->no.free_segs.curr)
	mm->no.free_segs.max = mm->no.free_segs.curr;
    return res;
}

static ERTS_INLINE void
free_desc(ErtsMemMapper* mm, ErtsFreeSegDesc *desc)
{
    desc->start = mm->desc.free_list;
    mm->desc.free_list = (char *) desc;
    ERTS_MMAP_ASSERT(mm->no.free_segs.curr > 0);
    mm->no.free_segs.curr--;
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

static ERTS_INLINE SWord usable_size(enum SortOrder order,
                                     ErtsFreeSegDesc* desc)
{
    return ((order == SA_SZ_ADDR_ORDER) ?
            ERTS_SUPERALIGNED_FLOOR(desc->end) - ERTS_SUPERALIGNED_CEILING(desc->start)
            : desc->end - desc->start);
}

#ifdef HARD_DEBUG
static ERTS_INLINE SWord cmp_nodes(enum SortOrder order,
                                   RBTNode* lhs, RBTNode* rhs)
{
    ErtsFreeSegDesc* ldesc = node_to_desc(order, lhs);
    ErtsFreeSegDesc* rdesc = node_to_desc(order, rhs);
    RBT_ASSERT(lhs != rhs);
    if (order != ADDR_ORDER) {
        SWord diff = usable_size(order, ldesc) - usable_size(order, rdesc);
	if (diff) return diff;
    }
    if (order != SZ_REVERSE_ADDR_ORDER) {
        return (char*)ldesc->start - (char*)rdesc->start;
    }
    else {
        return (char*)rdesc->start - (char*)ldesc->start;
    }
}
#endif  /* HARD_DEBUG */

static ERTS_INLINE SWord cmp_with_node(enum SortOrder order,
                                       SWord sz, char* addr, RBTNode* rhs)
{
    ErtsFreeSegDesc* rdesc;
    if (order != ADDR_ORDER) {
        SWord diff;
        rdesc = snode_to_desc(rhs);
        diff = sz - usable_size(order, rdesc);
        if (diff) return diff;
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
}

/*
 * Replace node x with node y
 * NOTE: segment descriptor of y is not changed
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

    y->parent_and_color	= x->parent_and_color;
    y->right	= x->right;
    y->left	= x->left;
}

static void
tree_insert_fixup(RBTNode** root, RBTNode *node)
{
    RBTNode *x = node, *y, *papa_x, *granpa_x;

    /*
     * Rearrange the tree so that it satisfies the Red-Black Tree properties
     */

    papa_x = parent(x);
    RBT_ASSERT(x != *root && IS_RED(papa_x));
    do {

	/*
	 * x and its parent are both red. Move the red pair up the tree
	 * until we get to the root or until we can separate them.
	 */

        granpa_x = parent(papa_x);
	RBT_ASSERT(IS_RED(x));
	RBT_ASSERT(IS_BLACK(granpa_x));
	RBT_ASSERT(granpa_x);

	if (papa_x == granpa_x->left) {
	    y = granpa_x->right;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		SET_BLACK(papa_x);
		SET_RED(granpa_x);
                x = granpa_x;
	    }
	    else {

		if (x == papa_x->right) {
		    left_rotate(root, papa_x);
                    papa_x = x;
                    x = papa_x->left;
		}

		RBT_ASSERT(x == granpa_x->left->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(papa_x));
		RBT_ASSERT(IS_BLACK(granpa_x));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(papa_x);
		SET_RED(granpa_x);
		right_rotate(root, granpa_x);

		RBT_ASSERT(x == parent(x)->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(parent(x)->right));
		RBT_ASSERT(IS_BLACK(parent(x)));
		break;
	    }
	}
	else {
	    RBT_ASSERT(papa_x == granpa_x->right);
	    y = granpa_x->left;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		SET_BLACK(papa_x);
		SET_RED(granpa_x);
                x = granpa_x;
	    }
	    else {

		if (x == papa_x->left) {
		    right_rotate(root, papa_x);
                    papa_x = x;
                    x = papa_x->right;
		}

		RBT_ASSERT(x == granpa_x->right->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(papa_x));
		RBT_ASSERT(IS_BLACK(granpa_x));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(papa_x);
		SET_RED(granpa_x);
		left_rotate(root, granpa_x);

		RBT_ASSERT(x == parent(x)->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(parent(x)->left));
		RBT_ASSERT(IS_BLACK(parent(x)));
		break;
	    }
	}
    } while (x != *root && (papa_x=parent(x), IS_RED(papa_x)));

    SET_BLACK(*root);
}

static void
rbt_delete(RBTree* tree, RBTNode* del)
{
    Uint spliced_is_black;
    RBTNode *x, *y, *z = del, *papa_y;
    RBTNode null_x; /* null_x is used to get the fixup started when we
			splice out a node without children. */

    HARD_CHECK_IS_MEMBER(tree->root, del);
    HARD_CHECK_TREE(tree, 0);

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
    papa_y = parent(y);
    if (x) {
	set_parent(x, papa_y);
    }
    else if (spliced_is_black) {
	x = &null_x;
	x->right = x->left = NULL;
	x->parent_and_color = parent_and_color(papa_y, !RED_FLG);
	y->left = x;
    }

    if (!papa_y) {
	RBT_ASSERT(tree->root == y);
	tree->root = x;
    }
    else {
	if (y == papa_y->left) {
	    papa_y->left = x;
	}
	else {
	    RBT_ASSERT(y == papa_y->right);
	    papa_y->right = x;
	}
    }
    if (y != z) {
	/* We spliced out the successor of z; replace z by the successor */
	RBT_ASSERT(z != &null_x);
	replace(&tree->root, z, y);
    }

    if (spliced_is_black) {
        RBTNode* papa_x;
	/* We removed a black node which makes the resulting tree
	   violate the Red-Black Tree properties. Fixup tree... */

        papa_x = parent(x);
	while (IS_BLACK(x) && papa_x) {

	    /*
	     * x has an "extra black" which we move up the tree
	     * until we reach the root or until we can get rid of it.
	     *
	     * y is the sibbling of x
	     */

	    if (x == papa_x->left) {
		y = papa_x->right;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(papa_x));
		    SET_RED(papa_x);
		    left_rotate(&tree->root, papa_x);
                    RBT_ASSERT(papa_x == parent(x));
		    y = papa_x->right;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->left) && IS_BLACK(y->right)) {
		    SET_RED(y);
		}
		else {
		    if (IS_BLACK(y->right)) {
			SET_BLACK(y->left);
			SET_RED(y);
			right_rotate(&tree->root, y);
                        RBT_ASSERT(papa_x == parent(x));
			y = papa_x->right;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(papa_x)) {

			SET_BLACK(papa_x);
			SET_RED(y);
		    }
		    RBT_ASSERT(y->right);
		    SET_BLACK(y->right);
		    left_rotate(&tree->root, papa_x);
		    x = tree->root;
		    break;
		}
	    }
	    else {
		RBT_ASSERT(x == papa_x->right);
		y = papa_x->left;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(papa_x));
		    SET_RED(papa_x);
		    right_rotate(&tree->root, papa_x);
                    RBT_ASSERT(papa_x == parent(x));
		    y = papa_x->left;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->right) && IS_BLACK(y->left)) {
		    SET_RED(y);
		}
		else {
		    if (IS_BLACK(y->left)) {
			SET_BLACK(y->right);
			SET_RED(y);
			left_rotate(&tree->root, y);
                        RBT_ASSERT(papa_x == parent(x));
			y = papa_x->left;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(papa_x)) {
			SET_BLACK(papa_x);
			SET_RED(y);
		    }
		    RBT_ASSERT(y->left);
		    SET_BLACK(y->left);
		    right_rotate(&tree->root, papa_x);
		    x = tree->root;
		    break;
		}
	    }
            x = papa_x;
            papa_x = parent(x);
	}
	SET_BLACK(x);

        papa_x = parent(&null_x);
	if (papa_x) {
	    if (papa_x->left == &null_x)
		papa_x->left = NULL;
	    else {
		RBT_ASSERT(papa_x->right == &null_x);
		papa_x->right = NULL;
	    }
	    RBT_ASSERT(!null_x.left);
	    RBT_ASSERT(!null_x.right);
	}
	else if (tree->root == &null_x) {
	    tree->root = NULL;
	    RBT_ASSERT(!null_x.left);
	    RBT_ASSERT(!null_x.right);
	}
    }
    HARD_CHECK_TREE(tree, 0);
}


static void
rbt_insert(RBTree* tree, RBTNode* node)
{
#ifdef RBT_DEBUG
    ErtsFreeSegDesc *dbg_under=NULL, *dbg_over=NULL;
#endif
    ErtsFreeSegDesc* desc = node_to_desc(tree->order, node);
    char* seg_addr = desc->start;
    SWord seg_sz = desc->end - desc->start;

    HARD_CHECK_TREE(tree, 0);

    node->left	= NULL;
    node->right	= NULL;

    if (!tree->root) {
	node->parent_and_color = parent_and_color(NULL, !RED_FLG);
	tree->root = node;
    }
    else {
	RBTNode *x = tree->root;
	while (1) {
	    SWord diff = cmp_with_node(tree->order, seg_sz, seg_addr, x);
	    if (diff < 0) {
                IF_RBT_DEBUG(dbg_over = node_to_desc(tree->order, x));
		if (!x->left) {
		    node->parent_and_color = parent_and_color(x, RED_FLG);
		    x->left = node;
		    break;
		}
		x = x->left;
	    }
	    else {
                RBT_ASSERT(diff > 0);
                IF_RBT_DEBUG(dbg_under = node_to_desc(tree->order, x));
                if (!x->right) {
                    node->parent_and_color = parent_and_color(x, RED_FLG);
                    x->right = node;
                    break;
                }
                x = x->right;
            }
	}

	RBT_ASSERT(parent(node));
#ifdef RBT_DEBUG
        if (tree->order == ADDR_ORDER) {
            RBT_ASSERT(!dbg_under || dbg_under->end < desc->start);
            RBT_ASSERT(!dbg_over || dbg_over->start > desc->end);
        }
#endif
        RBT_ASSERT(IS_RED(node));
	if (IS_RED(parent(node)))
	    tree_insert_fixup(&tree->root, node);
    }
    HARD_CHECK_TREE(tree, 0);
}

/*
 * Traverse tree in (reverse) sorting order
 */
static void
rbt_foreach_node(RBTree* tree,
                 void (*fn)(RBTNode*,void*),
                 void* arg, int reverse)
{
#ifdef HARD_DEBUG
    Sint blacks = -1;
    Sint curr_blacks = 1;
    Uint depth = 1;
    Uint max_depth = 0;
    Uint node_cnt = 0;
#endif
    enum { RECURSE_LEFT, DO_NODE, RECURSE_RIGHT, RETURN_TO_PARENT }state;
    RBTNode *x = tree->root;

    RBT_ASSERT(!x || !parent(x));

    state = reverse ? RECURSE_RIGHT : RECURSE_LEFT;
    while (x) {
        switch (state) {
        case RECURSE_LEFT:
            if (x->left) {
                RBT_ASSERT(parent(x->left) == x);
              #ifdef HARD_DEBUG
                ++depth;
                if (IS_BLACK(x->left))
                    curr_blacks++;
              #endif
                x = x->left;
                state = reverse ? RECURSE_RIGHT : RECURSE_LEFT;
            }
            else {
              #ifdef HARD_DEBUG
                if (blacks < 0)
                    blacks = curr_blacks;
                RBT_ASSERT(blacks == curr_blacks);
              #endif
                state = reverse ? RETURN_TO_PARENT : DO_NODE;
            }
            break;

        case DO_NODE:
          #ifdef HARD_DEBUG
            ++node_cnt;
            if (depth > max_depth)
                max_depth = depth;
          #endif
            (*fn) (x, arg); /* Do it! */
            state = reverse ? RECURSE_LEFT : RECURSE_RIGHT;
            break;

        case RECURSE_RIGHT:
            if (x->right) {
                RBT_ASSERT(parent(x->right) == x);
              #ifdef HARD_DEBUG
                ++depth;
                if (IS_BLACK(x->right))
                    curr_blacks++;
              #endif
                x = x->right;
                state = reverse ? RECURSE_RIGHT : RECURSE_LEFT;
            }
            else {
              #ifdef HARD_DEBUG
                if (blacks < 0)
                    blacks = curr_blacks;
                RBT_ASSERT(blacks == curr_blacks);
              #endif
                state = reverse ? DO_NODE : RETURN_TO_PARENT;
            }
            break;

        case RETURN_TO_PARENT:
          #ifdef HARD_DEBUG
            if (IS_BLACK(x))
                curr_blacks--;
            --depth;
          #endif
            if (parent(x)) {
                if (x == parent(x)->left) {
                    state = reverse ? RETURN_TO_PARENT : DO_NODE;
                }
                else {
                    RBT_ASSERT(x == parent(x)->right);
                    state = reverse ? DO_NODE : RETURN_TO_PARENT;
                }
            }
            x = parent(x);
            break;
        }
    }
#ifdef HARD_DEBUG
    RBT_ASSERT(depth == 0 || (!tree->root && depth==1));
    RBT_ASSERT(curr_blacks == 0);
    RBT_ASSERT((1 << (max_depth/2)) <= node_cnt);
#endif
}

#if defined(RBT_DEBUG) || defined(HARD_DEBUG_MSEG)
static RBTNode* rbt_prev_node(RBTNode* node)
{
    RBTNode* x;
    if (node->left) {
        for (x=node->left; x->right; x=x->right)
            ;
        return x;
    }
    for (x=node; parent(x); x=parent(x)) {
        if (parent(x)->right == x)
            return parent(x);
    }
    return NULL;
}
static RBTNode* rbt_next_node(RBTNode* node)
{
    RBTNode* x;
    if (node->right) {
        for (x=node->right; x->left; x=x->left)
            ;
        return x;
    }
    for (x=node; parent(x); x=parent(x)) {
        if (parent(x)->left == x)
            return parent(x);
    }
    return NULL;
}
#endif /* RBT_DEBUG || HARD_DEBUG_MSEG */


/* The API to keep track of a bunch of separated (free) segments
   (non-overlapping and non-adjacent).
 */
static void init_free_seg_map(ErtsFreeSegMap*, enum SortOrder);
static void adjacent_free_seg(ErtsFreeSegMap*, char* start, char* end,
                              ErtsFreeSegDesc** under, ErtsFreeSegDesc** over);
static void insert_free_seg(ErtsFreeSegMap*, ErtsFreeSegDesc*, char* start, char* end);
static void resize_free_seg(ErtsFreeSegMap*, ErtsFreeSegDesc*, char* start, char* end);
static void delete_free_seg(ErtsFreeSegMap*, ErtsFreeSegDesc*);
static ErtsFreeSegDesc* lookup_free_seg(ErtsFreeSegMap*, SWord sz);


static void init_free_seg_map(ErtsFreeSegMap* map, enum SortOrder order)
{
    map->atree.root = NULL;
    map->atree.order = ADDR_ORDER;
    map->stree.root = NULL;
    map->stree.order = order;
    map->nseg = 0;
}

/* Lookup directly adjacent free segments to the given area [start->end].
 * The given area must not contain any free segments.
 */
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

/* Initialize 'desc' and insert as new free segment [start->end].
 * The new segment must not contain or be adjacent to any free segment in 'map'.
 */
static void insert_free_seg(ErtsFreeSegMap* map, ErtsFreeSegDesc* desc,
                            char* start, char* end)
{
    desc->start = start;
    desc->end = end;
    rbt_insert(&map->atree, &desc->anode);
    rbt_insert(&map->stree, &desc->snode);
    map->nseg++;
}

/* Resize existing free segment 'desc' to [start->end].
 * The new segment location must overlap the old location and
 * it must not contain or be adjacent to any other free segment in 'map'.
 */
static void resize_free_seg(ErtsFreeSegMap* map, ErtsFreeSegDesc* desc,
                            char* start, char* end)
{
#ifdef RBT_DEBUG
    RBTNode* prev = rbt_prev_node(&desc->anode);
    RBTNode* next = rbt_next_node(&desc->anode);
    RBT_ASSERT(!prev || anode_to_desc(prev)->end < start);
    RBT_ASSERT(!next || anode_to_desc(next)->start > end);
#endif
    rbt_delete(&map->stree, &desc->snode);
    desc->start = start;
    desc->end = end;
    rbt_insert(&map->stree, &desc->snode);
}

/* Delete existing free segment 'desc' from 'map'.
 */
static void delete_free_seg(ErtsFreeSegMap* map, ErtsFreeSegDesc* desc)
{
    rbt_delete(&map->atree, &desc->anode);
    rbt_delete(&map->stree, &desc->snode);
    map->nseg--;
}

/* Lookup a free segment in 'map' with a size of at least 'need_sz' usable bytes.
 */
static ErtsFreeSegDesc* lookup_free_seg(ErtsFreeSegMap* map, SWord need_sz)
{
    RBTNode* x = map->stree.root;
    ErtsFreeSegDesc* best_desc = NULL;
    const enum SortOrder order = map->stree.order;

    while (x) {
        ErtsFreeSegDesc* desc = snode_to_desc(x);
        SWord seg_sz = usable_size(order, desc);

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

struct build_arg_t
{
    Process* p;
    Eterm* hp;
    Eterm acc;
};

static void build_free_seg_tuple(RBTNode* node, void* arg)
{
    struct build_arg_t* a = (struct build_arg_t*)arg;
    ErtsFreeSegDesc* desc = anode_to_desc(node);
    Eterm start= erts_bld_uword(&a->hp, NULL, (UWord)desc->start);
    Eterm end = erts_bld_uword(&a->hp, NULL, (UWord)desc->end);
    Eterm tpl = TUPLE2(a->hp, start, end);

    a->hp += 3;
    a->acc = CONS(a->hp, tpl, a->acc);
    a->hp += 2;
}

static
Eterm build_free_seg_list(Process* p, ErtsFreeSegMap* map)
{
    struct build_arg_t barg;
    Eterm* hp_end;
    const Uint may_need = map->nseg * (2 + 3 + 2*2);  /* cons + tuple + bigs */

    barg.p = p;
    barg.hp = HAlloc(p, may_need);
    hp_end = barg.hp + may_need;
    barg.acc = NIL;
    rbt_foreach_node(&map->atree, build_free_seg_tuple, &barg, 1);

    RBT_ASSERT(barg.hp <= hp_end);
    HRelease(p, hp_end, barg.hp);
    return barg.acc;
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
#    define ERTS_MMAP_FD		mm->mmap_fd
#  endif
#endif

static ERTS_INLINE void *
os_mmap(void *hint_ptr, UWord size, int try_superalign)
{
#if HAVE_MMAP
    void *res;
#ifdef MAP_ALIGN
    if (try_superalign)
	res = mmap((void *) ERTS_SUPERALIGNED_SIZE, size, ERTS_MMAP_PROT,
		   ERTS_MMAP_FLAGS|MAP_ALIGN, ERTS_MMAP_FD, 0);
    else
#endif
	res = mmap((void *) hint_ptr, size, ERTS_MMAP_PROT,
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
#if defined(__FreeBSD__)
#define ERTS_MMAP_UNRESERVE_FLAGS	(ERTS_MMAP_FLAGS|MAP_FIXED)
#else
#define ERTS_MMAP_UNRESERVE_FLAGS	(ERTS_MMAP_FLAGS|MAP_NORESERVE|MAP_FIXED)
#endif /* __FreeBSD__ */
#define ERTS_MMAP_VIRTUAL_PROT		(PROT_NONE)
#if defined(__FreeBSD__)
#define ERTS_MMAP_VIRTUAL_FLAGS		(ERTS_MMAP_FLAGS)
#else
#define ERTS_MMAP_VIRTUAL_FLAGS		(ERTS_MMAP_FLAGS|MAP_NORESERVE)
#endif /* __FreeBSD__ */

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
	erts_exit(ERTS_ABORT_EXIT, "Failed to unreserve memory");
}

static void *
os_mmap_virtual(char *ptr, UWord size)
{
    int flags = ERTS_MMAP_VIRTUAL_FLAGS;
    void* res;

    res = mmap((void *) ptr, (size_t) size, ERTS_MMAP_VIRTUAL_PROT,
               flags, ERTS_MMAP_FD, 0);
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

static UWord
alloc_desc_insert_free_seg(ErtsMemMapper* mm,
                           ErtsFreeSegMap *map, char* start, char* end)
{
    char *ptr;
    ErtsFreeSegMap *da_map;
    ErtsFreeSegDesc *desc = alloc_desc(mm);
    if (desc) {
	insert_free_seg(map, desc, start, end);
	return 0;
    }

    /*
     * Ahh; ran out of free segment descriptors.
     *
     * First try to map a new page...
     */

#if ERTS_HAVE_OS_MMAP
    if (!mm->no_os_mmap) {
        ptr = os_mmap(mm->desc.new_area_hint, ERTS_PAGEALIGNED_SIZE, 0);
        if (ptr) {
            mm->desc.new_area_hint = ptr+ERTS_PAGEALIGNED_SIZE;
            ERTS_MMAP_SIZE_OS_INC(ERTS_PAGEALIGNED_SIZE);
            add_free_desc_area(mm, ptr, ptr+ERTS_PAGEALIGNED_SIZE);
            desc = alloc_desc(mm);
            ERTS_MMAP_ASSERT(desc);
            insert_free_seg(map, desc, start, end);
            return 0;
        }
    }
#endif

    /*
     * ...then try to find a good place in the supercarrier...
     */
    da_map = &mm->sua.map;
    desc = lookup_free_seg(da_map, ERTS_PAGEALIGNED_SIZE);
    if (desc) {
	if (mm->reserve_physical(desc->start, ERTS_PAGEALIGNED_SIZE))
	    ERTS_MMAP_SIZE_SC_SUA_INC(ERTS_PAGEALIGNED_SIZE);
	else
	    desc = NULL;
	
    }
    else {
	da_map = &mm->sa.map;
	desc = lookup_free_seg(da_map, ERTS_PAGEALIGNED_SIZE);
	if (desc) {
	    if (mm->reserve_physical(desc->start, ERTS_PAGEALIGNED_SIZE))
		ERTS_MMAP_SIZE_SC_SA_INC(ERTS_PAGEALIGNED_SIZE);
	    else
		desc = NULL;
	}
    }
    if (desc) {
	char *da_end = desc->start + ERTS_PAGEALIGNED_SIZE;
	add_free_desc_area(mm, desc->start, da_end);
	if (da_end != desc->end)
	    resize_free_seg(da_map, desc, da_end, desc->end);
	else {
	    delete_free_seg(da_map, desc);
	    free_desc(mm, desc);
	}

	desc = alloc_desc(mm);
	ERTS_MMAP_ASSERT(desc);
	insert_free_seg(map, desc, start, end);
	return 0;
    }

    /*
     * ... and then as last resort use the first page of the
     * free segment we are trying to insert for free descriptors.
     */
    ptr = start + ERTS_PAGEALIGNED_SIZE;
    ERTS_MMAP_ASSERT(ptr <= end);

    add_free_desc_area(mm, start, ptr);

    if (ptr != end) {
	desc = alloc_desc(mm);
	ERTS_MMAP_ASSERT(desc);
	insert_free_seg(map, desc, ptr, end);
    }

    return ERTS_PAGEALIGNED_SIZE;
}

void *
erts_mmap(ErtsMemMapper* mm, Uint32 flags, UWord *sizep)
{
    char *seg;
    UWord asize = ERTS_PAGEALIGNED_CEILING(*sizep);

    /* Map in premapped supercarrier */
    if (mm->supercarrier && !(ERTS_MMAPFLG_OS_ONLY & flags)) {
	char *end;
	ErtsFreeSegDesc *desc;
	Uint32 superaligned = (ERTS_MMAPFLG_SUPERALIGNED & flags);

	erts_mtx_lock(&mm->mtx);

	ERTS_MMAP_OP_START(*sizep);

	if (!superaligned) {
	    desc = lookup_free_seg(&mm->sua.map, asize);
	    if (desc) {
		seg = desc->start;
		end = seg+asize;
		if (!mm->reserve_physical(seg, asize))
		    goto supercarrier_reserve_failure;
		if (desc->end == end) {
		    delete_free_seg(&mm->sua.map, desc);
		    free_desc(mm, desc);
		}
		else {
		    ERTS_MMAP_ASSERT(end < desc->end);
		    resize_free_seg(&mm->sua.map, desc, end, desc->end);
		}
		ERTS_MMAP_SIZE_SC_SUA_INC(asize);
		goto supercarrier_success;
	    }

	    if (asize <= mm->sua.bot - mm->sa.top) {
		if (!mm->reserve_physical(mm->sua.bot - asize, asize))
		    goto supercarrier_reserve_failure;
		mm->sua.bot -= asize;
		seg = mm->sua.bot;
		ERTS_MMAP_SIZE_SC_SUA_INC(asize);
		goto supercarrier_success;
	    }
	}

	asize = ERTS_SUPERALIGNED_CEILING(asize);

	desc = lookup_free_seg(&mm->sa.map, asize);
	if (desc) {
	    char *start = seg = desc->start;
	    seg = (char *) ERTS_SUPERALIGNED_CEILING(seg);
	    end = seg+asize;
	    if (!mm->reserve_physical(start, (UWord) (end - start)))
		goto supercarrier_reserve_failure;
	    ERTS_MMAP_SIZE_SC_SA_INC(asize);
	    if (desc->end == end) {
		if (start != seg)
		    resize_free_seg(&mm->sa.map, desc, start, seg);
		else {
		    delete_free_seg(&mm->sa.map, desc);
		    free_desc(mm, desc);
		}
	    }
	    else {
		ERTS_MMAP_ASSERT(end < desc->end);
		resize_free_seg(&mm->sa.map, desc, end, desc->end);
		if (start != seg) {
		    UWord ad_sz;
		    ad_sz = alloc_desc_insert_free_seg(mm, &mm->sua.map,
						       start, seg);
		    start += ad_sz;
		    if (start != seg)
			mm->unreserve_physical(start, (UWord) (seg - start));
		}
	    }
	    goto supercarrier_success;
	}

	if (superaligned) {
	    char *start = mm->sa.top;
	    seg = (char *) ERTS_SUPERALIGNED_CEILING(start);

	    if (asize + (seg - start) <= mm->sua.bot - start) {
		end = seg + asize;
		if (!mm->reserve_physical(start, (UWord) (end - start)))
		    goto supercarrier_reserve_failure;
		mm->sa.top = end;
		ERTS_MMAP_SIZE_SC_SA_INC(asize);
		if (start != seg) {
		    UWord ad_sz;
		    ad_sz = alloc_desc_insert_free_seg(mm, &mm->sua.map,
						       start, seg);
		    start += ad_sz;
		    if (start != seg)
			mm->unreserve_physical(start, (UWord) (seg - start));
		}
		goto supercarrier_success;
	    }

	    desc = lookup_free_seg(&mm->sua.map, asize + ERTS_SUPERALIGNED_SIZE);
	    if (desc) {
		char *org_start = desc->start;
		char *org_end = desc->end;

		seg = (char *) ERTS_SUPERALIGNED_CEILING(org_start);
		end = seg + asize;
		if (!mm->reserve_physical(seg, (UWord) (org_end - seg)))
		    goto supercarrier_reserve_failure;
		ERTS_MMAP_SIZE_SC_SUA_INC(asize);
		if (org_start != seg) {
		    ERTS_MMAP_ASSERT(org_start < seg);
		    resize_free_seg(&mm->sua.map, desc, org_start, seg);
		    desc = NULL;
		}
		if (end != org_end) {
		    UWord ad_sz = 0;
		    ERTS_MMAP_ASSERT(end < org_end);
		    if (desc)
			resize_free_seg(&mm->sua.map, desc, end, org_end);
		    else
			ad_sz = alloc_desc_insert_free_seg(mm, &mm->sua.map,
							   end, org_end);
		    end += ad_sz;
		    if (end != org_end)
			mm->unreserve_physical(end,
						      (UWord) (org_end - end));
		}
		goto supercarrier_success;
	    }
	}

	ERTS_MMAP_OP_ABORT();
	erts_mtx_unlock(&mm->mtx);
    }

#if ERTS_HAVE_OS_MMAP
    /* Map using OS primitives */
    if (!(ERTS_MMAPFLG_SUPERCARRIER_ONLY & flags) && !mm->no_os_mmap) {
	if (!(ERTS_MMAPFLG_SUPERALIGNED & flags)) {
	    seg = os_mmap(NULL, asize, 0);
	    if (!seg)
		goto failure;
	}
	else {
	    asize = ERTS_SUPERALIGNED_CEILING(*sizep);
	    seg = os_mmap(NULL, asize, 1);
	    if (!seg)
		goto failure;

	    if (!ERTS_IS_SUPERALIGNED(seg)) {
		char *ptr;
		UWord sz;

		os_munmap(seg, asize);

		ptr = os_mmap(NULL, asize + ERTS_SUPERALIGNED_SIZE, 1);
		if (!ptr)
		    goto failure;

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

	ERTS_MMAP_OP_LCK(seg, *sizep, asize);
	ERTS_MMAP_SIZE_OS_INC(asize);
	*sizep = asize;
	return (void *) seg;
    }
failure:
#endif
    ERTS_MMAP_OP_LCK(NULL, *sizep, 0);
    *sizep = 0;
    return NULL;

supercarrier_success:

#ifdef ERTS_MMAP_DEBUG
    if (ERTS_MMAPFLG_SUPERALIGNED & flags) {
	ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(seg));
	ERTS_MMAP_ASSERT(ERTS_IS_SUPERALIGNED(asize));
    }
    else {
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(seg));
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));
    }
#endif

    ERTS_MMAP_OP_END(seg, asize);
    erts_mtx_unlock(&mm->mtx);

    *sizep = asize;
    return (void *) seg;

supercarrier_reserve_failure:
    erts_mtx_unlock(&mm->mtx);
    *sizep = 0;
    return NULL;
}

void
erts_munmap(ErtsMemMapper* mm, Uint32 flags, void *ptr, UWord size)
{
    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(size));

    if (!ERTS_MMAP_IN_SUPERCARRIER(ptr)) {
	ERTS_MMAP_ASSERT(!mm->no_os_mmap);
#if ERTS_HAVE_OS_MMAP
	ERTS_MUNMAP_OP_LCK(ptr, size);
	ERTS_MMAP_SIZE_OS_DEC(size);
	os_munmap(ptr, size);
#endif
    }
    else {
	char *start, *end;
	ErtsFreeSegMap *map;
	ErtsFreeSegDesc *prev, *next, *desc;
	UWord ad_sz = 0;

	ERTS_MMAP_ASSERT(mm->supercarrier);

	start = (char *) ptr;
	end = start + size;

	erts_mtx_lock(&mm->mtx);

	ERTS_MUNMAP_OP(ptr, size);

	if (ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)) {

	    map = &mm->sa.map;
	    adjacent_free_seg(map, start, end, &prev, &next);

	    ERTS_MMAP_SIZE_SC_SA_DEC(size);
	    if (end == mm->sa.top) {
		ERTS_MMAP_ASSERT(!next);
		if (prev) {
		    start = prev->start;
		    delete_free_seg(map, prev);
		    free_desc(mm, prev);
		}
		mm->sa.top = start;
		goto supercarrier_success;
	    }
	}
	else {
	    map = &mm->sua.map;
	    adjacent_free_seg(map, start, end, &prev, &next);

	    ERTS_MMAP_SIZE_SC_SUA_DEC(size);
	    if (start == mm->sua.bot) {
		ERTS_MMAP_ASSERT(!prev);
		if (next) {
		    end = next->end;
		    delete_free_seg(map, next);
		    free_desc(mm, next);
		}
		mm->sua.bot = end;
		goto supercarrier_success;
	    }
	}

	desc = NULL;

	if (next) {
	    ERTS_MMAP_ASSERT(end < next->end);
	    end = next->end;
	    if (prev) {
		delete_free_seg(map, next);
		free_desc(mm, next);
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
	else
	    ad_sz = alloc_desc_insert_free_seg(mm, map, start, end);
	
    supercarrier_success: {
	    UWord unres_sz;

	    ERTS_MMAP_ASSERT(size >= ad_sz);
	    unres_sz = size - ad_sz;
	    if (unres_sz)
		mm->unreserve_physical(((char *) ptr) + ad_sz, unres_sz);

            erts_mtx_unlock(&mm->mtx);
	}
    }
}

static void *
remap_move(ErtsMemMapper* mm,
           Uint32 flags, void *ptr, UWord old_size, UWord *sizep)
{
    UWord size = *sizep;
    void *new_ptr = erts_mmap(mm, flags, &size);
    if (!new_ptr)
	return NULL;
    *sizep = size;
    if (old_size < size)
	size = old_size;
    sys_memcpy(new_ptr, ptr, (size_t) size);
    erts_munmap(mm, flags, ptr, old_size);
    return new_ptr;
}

void *
erts_mremap(ErtsMemMapper* mm,
            Uint32 flags, void *ptr, UWord old_size, UWord *sizep)
{
    void *new_ptr;
    Uint32 superaligned;
    UWord asize;

    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(old_size));
    ERTS_MMAP_ASSERT(sizep && ERTS_IS_PAGEALIGNED(*sizep));

    if (!ERTS_MMAP_IN_SUPERCARRIER(ptr)) {

	ERTS_MMAP_ASSERT(!mm->no_os_mmap);

	if (!(ERTS_MMAPFLG_OS_ONLY & flags) && mm->supercarrier) {
	    new_ptr = remap_move(mm, ERTS_MMAPFLG_SUPERCARRIER_ONLY|flags,
                                 ptr, old_size, sizep);
	    if (new_ptr)
		return new_ptr;
	}

	if (ERTS_MMAPFLG_SUPERCARRIER_ONLY & flags) {
	    ERTS_MREMAP_OP_LCK(NULL, ptr, old_size, *sizep, old_size);
	    return NULL;
	}

#if defined(ERTS_HAVE_OS_MREMAP) || defined(ERTS_HAVE_GENUINE_OS_MMAP)
	superaligned = (ERTS_MMAPFLG_SUPERALIGNED & flags);

	if (superaligned) {
	    asize = ERTS_SUPERALIGNED_CEILING(*sizep);
	    if (asize == old_size && ERTS_IS_SUPERALIGNED(ptr)) {
		ERTS_MREMAP_OP_LCK(ptr, ptr, old_size, *sizep, asize);
		*sizep = asize;
		return ptr;
	    }
	}
	else {
	    asize = ERTS_PAGEALIGNED_CEILING(*sizep);
	    if (asize == old_size) {
		ERTS_MREMAP_OP_LCK(ptr, ptr, old_size, *sizep, asize);
		*sizep = asize;
		return ptr;
	    }
	}

#ifdef ERTS_HAVE_GENUINE_OS_MMAP
	if (asize < old_size
	    && (!superaligned
		|| ERTS_IS_SUPERALIGNED(ptr))) {
	    UWord um_sz;
	    new_ptr = ((char *) ptr) + asize;
	    ERTS_MMAP_ASSERT((((char *)ptr) + old_size) > (char *) new_ptr);
	    um_sz = (UWord) ((((char *) ptr) + old_size) - (char *) new_ptr); 
	    ERTS_MMAP_SIZE_OS_DEC(um_sz);
	    os_munmap(new_ptr, um_sz);
	    ERTS_MREMAP_OP_LCK(ptr, ptr, old_size, *sizep, asize);
	    *sizep = asize;
	    return ptr;
	}
#endif
#ifdef ERTS_HAVE_OS_MREMAP
	if (superaligned)
	    return remap_move(mm, flags, new_ptr, old_size, sizep);
	else {
	    new_ptr = os_mremap(ptr, old_size, asize, 0);
	    if (!new_ptr)
		return NULL;
	    if (asize > old_size)
		ERTS_MMAP_SIZE_OS_INC(asize - old_size);
	    else
		ERTS_MMAP_SIZE_OS_DEC(old_size - asize);
	    ERTS_MREMAP_OP_LCK(new_ptr, ptr, old_size, *sizep, asize);
	    *sizep = asize;
	    return new_ptr;
	}
#endif
#endif
    }
    else { /* In super carrier */
	char *start, *end, *new_end;
	ErtsFreeSegMap *map;
	ErtsFreeSegDesc *prev, *next;
	UWord ad_sz = 0;

	ERTS_MMAP_ASSERT(mm->supercarrier);

	if (ERTS_MMAPFLG_OS_ONLY & flags)
	    return remap_move(mm, flags, ptr, old_size, sizep);

	superaligned = (ERTS_MMAPFLG_SUPERALIGNED & flags);

	asize = (superaligned
		 ? ERTS_SUPERALIGNED_CEILING(*sizep)
		 : ERTS_PAGEALIGNED_CEILING(*sizep));

	erts_mtx_lock(&mm->mtx);

	if (ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)
	    ? (!superaligned && lookup_free_seg(&mm->sua.map, asize))
	    : (superaligned && lookup_free_seg(&mm->sa.map, asize))) {
	    erts_mtx_unlock(&mm->mtx);
	    /*
	     * Segment currently in wrong area (due to a previous memory
	     * shortage), move it to the right area.
	     * (remap_move() will succeed)
	     */
	    return remap_move(mm, ERTS_MMAPFLG_SUPERCARRIER_ONLY|flags,
                              ptr, old_size, sizep);
	}

	ERTS_MREMAP_OP_START(ptr, old_size, *sizep);

	if (asize == old_size) {
	    new_ptr = ptr;
	    goto supercarrier_resize_success;
	}

	start = (char *) ptr;
	end = start + old_size;
	new_end = start+asize;

	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(old_size));
	ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));

	if (asize < old_size) {
	    UWord unres_sz;
	    new_ptr = ptr;
	    if (!ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)) {
		map = &mm->sua.map;
		ERTS_MMAP_SIZE_SC_SUA_DEC(old_size - asize);
	    }
	    else {
		if (end == mm->sa.top) {
		    mm->sa.top = new_end;
		    mm->unreserve_physical(((char *) ptr) + asize,
						  old_size - asize);
		    goto supercarrier_resize_success;
		}
		ERTS_MMAP_SIZE_SC_SA_DEC(old_size - asize);
		map = &mm->sa.map;
	    }
	    
	    adjacent_free_seg(map, start, end, &prev, &next);

	    if (next)
		resize_free_seg(map, next, new_end, next->end);
	    else
		ad_sz = alloc_desc_insert_free_seg(mm, map, new_end, end);
	    ERTS_MMAP_ASSERT(old_size - asize >= ad_sz);
	    unres_sz = old_size - asize - ad_sz;
	    if (unres_sz)
		mm->unreserve_physical(((char *) ptr) + asize + ad_sz,
					      unres_sz);
	    goto supercarrier_resize_success;
	}

	if (!ERTS_MMAP_IN_SUPERALIGNED_AREA(ptr)) {
	    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(ptr));
	    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(old_size));
	    ERTS_MMAP_ASSERT(ERTS_IS_PAGEALIGNED(asize));

	    adjacent_free_seg(&mm->sua.map, start, end, &prev, &next);

	    if (next && new_end <= next->end) {
		if (!mm->reserve_physical(((char *) ptr) + old_size,
                                          asize - old_size))
		    goto supercarrier_reserve_failure;
		if (new_end < next->end)
		    resize_free_seg(&mm->sua.map, next, new_end, next->end);
		else {
		    delete_free_seg(&mm->sua.map, next);
		    free_desc(mm, next);
		}
		new_ptr = ptr;
		ERTS_MMAP_SIZE_SC_SUA_INC(asize - old_size);
		goto supercarrier_resize_success;
	    }
	}
	else { /* Superaligned area */

	    if (end == mm->sa.top) {
		if (new_end <= mm->sua.bot) {
		    if (!mm->reserve_physical(((char *) ptr) + old_size,
                                              asize - old_size))
			goto supercarrier_reserve_failure;
		    mm->sa.top = new_end;
		    new_ptr = ptr;
		    ERTS_MMAP_SIZE_SC_SA_INC(asize - old_size);
		    goto supercarrier_resize_success;
		}
	    }
	    else {
		adjacent_free_seg(&mm->sa.map, start, end, &prev, &next);
		if (next && new_end <= next->end) {
		    if (!mm->reserve_physical(((char *) ptr) + old_size,
                                              asize - old_size))
			goto supercarrier_reserve_failure;
		    if (new_end < next->end)
			resize_free_seg(&mm->sa.map, next, new_end, next->end);
		    else {
			delete_free_seg(&mm->sa.map, next);
			free_desc(mm, next);
		    }
		    new_ptr = ptr;
		    ERTS_MMAP_SIZE_SC_SA_INC(asize - old_size);
		    goto supercarrier_resize_success;
		}
	    }
	}

	ERTS_MMAP_OP_ABORT();
	erts_mtx_unlock(&mm->mtx);

	/* Failed to resize... */
    }

    return remap_move(mm, flags, ptr, old_size, sizep);

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

    ERTS_MREMAP_OP_END(new_ptr, asize);
    erts_mtx_unlock(&mm->mtx);

    *sizep = asize;
    return new_ptr;

supercarrier_reserve_failure:
    ERTS_MREMAP_OP_END(NULL, old_size);
    erts_mtx_unlock(&mm->mtx);
    *sizep = old_size;
    return NULL;
    
}

int erts_mmap_in_supercarrier(ErtsMemMapper* mm, void *ptr)
{
    return ERTS_MMAP_IN_SUPERCARRIER(ptr);
}

static struct {
    Eterm options;
    Eterm total;
    Eterm total_sa;
    Eterm total_sua;
    Eterm used;
    Eterm used_sa;
    Eterm used_sua;
    Eterm max;
    Eterm allocated;
    Eterm reserved;
    Eterm sizes;
    Eterm free_segs;
    Eterm supercarrier;
    Eterm os;
    Eterm scs;
    Eterm sco;
    Eterm scrpm;
    Eterm scrfsd;

    int is_initialized;
    erts_mtx_t init_mutex;
}am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void init_atoms(void)
{
    erts_mtx_lock(&am.init_mutex);

    if (!am.is_initialized) {
        AM_INIT(options);
        AM_INIT(total);
        AM_INIT(total_sa);
        AM_INIT(total_sua);
        AM_INIT(used);
        AM_INIT(used_sa);
        AM_INIT(used_sua);
        AM_INIT(max);
        AM_INIT(allocated);
        AM_INIT(reserved);
        AM_INIT(sizes);
        AM_INIT(free_segs);
        AM_INIT(supercarrier);
        AM_INIT(os);
        AM_INIT(scs);
        AM_INIT(sco);
        AM_INIT(scrpm);
        AM_INIT(scrfsd);
        am.is_initialized = 1;
    }
    erts_mtx_unlock(&am.init_mutex);
};


#ifdef HARD_DEBUG_MSEG
static void hard_dbg_mseg_init(void);
#endif

void
erts_mmap_init(ErtsMemMapper* mm, ErtsMMapInit *init)
{
    static int is_first_call = 1;
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
    erts_fprintf(stderr, "erts_mmap: scrfsd = %i\n", init->scrfsd);
#endif
    erts_page_inv_mask = pagesize - 1;
    if (pagesize & erts_page_inv_mask)
	erts_exit(1, "erts_mmap: Invalid pagesize: %bpu\n",
		 pagesize);

    ERTS_MMAP_OP_RINGBUF_INIT();

    mm->supercarrier = 0;
    mm->reserve_physical = reserve_noop;
    mm->unreserve_physical = unreserve_noop;

#if HAVE_MMAP && !defined(MAP_ANON)
    mm->mmap_fd = open("/dev/zero", O_RDWR);
    if (mm->mmap_fd < 0)
	erts_exit(1, "erts_mmap: Failed to open /dev/zero\n");
#endif

    erts_mtx_init(&mm->mtx, "erts_mmap", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    if (is_first_call) {
        erts_mtx_init(&am.init_mutex, "mmap_init_atoms", NIL,
            ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    }

#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
    if (init->virtual_range.start) {
	char *ptr;
	UWord sz;
	ptr = (char *) ERTS_PAGEALIGNED_CEILING(init->virtual_range.start);
	end = (char *) ERTS_PAGEALIGNED_FLOOR(init->virtual_range.end);
	sz = end - ptr;
	start = os_mmap_virtual(ptr, sz);
	if (!start || start > ptr || start >= end)
	    erts_exit(1,
		     "erts_mmap: Failed to create virtual range for super carrier\n");
	sz = start - ptr;
	if (sz)
	    os_munmap(end, sz);
	mm->reserve_physical = os_reserve_physical;
	mm->unreserve_physical = os_unreserve_physical;
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
	    mm->reserve_physical = os_reserve_physical;
	    mm->unreserve_physical = os_unreserve_physical;
	    virtual_map = 1;
	}
	else
#endif
	{
	    /*
	     * The whole supercarrier will by physically
	     * reserved all the time.
	     */
	    start = os_mmap(NULL, sz, 1);
	}
	if (!start)
	    erts_exit(1,
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
#endif

    mm->no.free_seg_descs = 0;
    mm->no.free_segs.curr = 0;
    mm->no.free_segs.max = 0;

    mm->size.supercarrier.total = 0;
    mm->size.supercarrier.used.total = 0;
    mm->size.supercarrier.used.sa = 0;
    mm->size.supercarrier.used.sua = 0;
    mm->size.os.used = 0;

    mm->desc.new_area_hint = NULL;

    if (!start) {
	mm->sa.bot = NULL;
	mm->sua.top = NULL;
	mm->sa.bot = NULL;
	mm->sua.top = NULL;
	mm->no_os_mmap = 0;
        mm->supercarrier = 0;
    }
    else {
	size_t desc_size;

	mm->no_os_mmap = init->sco;

	desc_size = init->scrfsd;
	if (desc_size < 100)
	    desc_size = 100;
	desc_size *= sizeof(ErtsFreeSegDesc);
	if ((desc_size
	     + ERTS_SUPERALIGNED_SIZE
	     + ERTS_PAGEALIGNED_SIZE) > end - start)
	    erts_exit(1, "erts_mmap: No space for segments in super carrier\n");

	mm->sa.bot = start;
	mm->sa.bot += desc_size;
	mm->sa.bot = (char *) ERTS_SUPERALIGNED_CEILING(mm->sa.bot);
	mm->sa.top = mm->sa.bot;
	mm->sua.top = end;
	mm->sua.bot = mm->sua.top;

	mm->size.supercarrier.used.total += (UWord) (mm->sa.bot - start);

	mm->desc.free_list = NULL;
        mm->desc.reserved = 0;

	if (end == (void *) 0) {
	    /*
	     * Very unlikely, but we need a guarantee
	     * that `mm->sua.top` always will
	     * compare as larger than all segment pointers
	     * into the super carrier...
	     */
	    mm->sua.top -= ERTS_PAGEALIGNED_SIZE;
	    mm->size.supercarrier.used.total += ERTS_PAGEALIGNED_SIZE;
#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
	    if (!virtual_map || os_reserve_physical(mm->sua.top, ERTS_PAGEALIGNED_SIZE))
#endif
		add_free_desc_area(mm, mm->sua.top, end);
            mm->desc.reserved += (end - mm->sua.top) / sizeof(ErtsFreeSegDesc);
	}

	mm->size.supercarrier.total = (UWord) (mm->sua.top - start);

	/*
	 * Area before (and after) super carrier
	 * will be used for free segment descritors.
	 */
#ifdef ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION
	if (virtual_map && !os_reserve_physical(start, mm->sa.bot - start))
	    erts_exit(1, "erts_mmap: Failed to reserve physical memory for descriptors\n");
#endif
	mm->desc.unused_start = start;
	mm->desc.unused_end = mm->sa.bot;
        mm->desc.reserved += ((mm->desc.unused_end - start)
                                     / sizeof(ErtsFreeSegDesc));

	init_free_seg_map(&mm->sa.map, SA_SZ_ADDR_ORDER);
	init_free_seg_map(&mm->sua.map, SZ_REVERSE_ADDR_ORDER);

	mm->supercarrier = 1;

	mm->desc.new_area_hint = end;

    }

#if !ERTS_HAVE_OS_MMAP
    mm->no_os_mmap = 1;
#endif

#ifdef HARD_DEBUG_MSEG
    hard_dbg_mseg_init();
#endif

#if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
   if (mm == &erts_literal_mmapper) {
       erts_literals_start = erts_literal_mmapper.sa.bot;
       erts_literals_size  = erts_literal_mmapper.sua.top - erts_literals_start;
    }
#endif
    is_first_call = 0;
}


static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = erts_bld_cons(hpp, szp, erts_bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

Eterm erts_mmap_info(ErtsMemMapper* mm,
                     fmtfn_t *print_to_p,
                     void *print_to_arg,
                     Eterm** hpp, Uint* szp,
                     struct erts_mmap_info_struct* emis)
{
    Eterm size_tags[] = { am.total, am.total_sa, am.total_sua, am.used, am.used_sa, am.used_sua };
    Eterm seg_tags[] = { am.used, am.max, am.allocated, am.reserved, am.used_sa, am.used_sua };
    Eterm group[2];
    Eterm group_tags[] = { am.sizes, am.free_segs };
    Eterm list[3];
    Eterm list_tags[3]; /* { am.options, am.supercarrier, am.os } */
    int lix = 0;
    Eterm res = THE_NON_VALUE;

    if (!hpp) {
        erts_mtx_lock(&mm->mtx);
        emis->sizes[0] = mm->size.supercarrier.total;
        emis->sizes[1] = mm->sa.top  - mm->sa.bot;
        emis->sizes[2] = mm->sua.top - mm->sua.bot;
        emis->sizes[3] = mm->size.supercarrier.used.total;
        emis->sizes[4] = mm->size.supercarrier.used.sa;
        emis->sizes[5] = mm->size.supercarrier.used.sua;

        emis->segs[0] = mm->no.free_segs.curr;
        emis->segs[1] = mm->no.free_segs.max;
        emis->segs[2] = mm->no.free_seg_descs;
        emis->segs[3] = mm->desc.reserved;
        emis->segs[4] = mm->sa.map.nseg;
        emis->segs[5] = mm->sua.map.nseg;

        emis->os_used = mm->size.os.used;
        erts_mtx_unlock(&mm->mtx);
    }

    list[lix] = erts_mmap_info_options(mm, "option ", print_to_p, print_to_arg,
                                       hpp, szp);
    list_tags[lix] = am.options;
    lix++;


    if (print_to_p) {
        fmtfn_t to = *print_to_p;
	void *arg = print_to_arg;
        if (mm->supercarrier) {
            const char* prefix = "supercarrier ";
            erts_print(to, arg, "%stotal size: %bpu\n", prefix, emis->sizes[0]);
            erts_print(to, arg, "%stotal sa size: %bpu\n", prefix, emis->sizes[1]);
            erts_print(to, arg, "%stotal sua size: %bpu\n", prefix, emis->sizes[2]);
            erts_print(to, arg, "%sused size: %bpu\n", prefix, emis->sizes[3]);
            erts_print(to, arg, "%sused sa size: %bpu\n", prefix, emis->sizes[4]);
            erts_print(to, arg, "%sused sua size: %bpu\n", prefix, emis->sizes[5]);
            erts_print(to, arg, "%sused free segs: %bpu\n", prefix, emis->segs[0]);
            erts_print(to, arg, "%smax free segs: %bpu\n", prefix, emis->segs[1]);
            erts_print(to, arg, "%sallocated free segs: %bpu\n", prefix, emis->segs[2]);
            erts_print(to, arg, "%sreserved free segs: %bpu\n", prefix, emis->segs[3]);
            erts_print(to, arg, "%ssa free segs: %bpu\n", prefix, emis->segs[4]);
            erts_print(to, arg, "%ssua free segs: %bpu\n", prefix, emis->segs[5]);
        }
        if (!mm->no_os_mmap) {
            erts_print(to, arg, "os mmap size used: %bpu\n", emis->os_used);
        }
    }


    if (hpp || szp) {
        if (!am.is_initialized) {
            init_atoms();
        }

        if (mm->supercarrier) {
            group[0] = erts_bld_atom_uword_2tup_list(hpp, szp,
                                                     sizeof(size_tags)/sizeof(Eterm),
                                                     size_tags, emis->sizes);
            group[1] = erts_bld_atom_uword_2tup_list(hpp, szp,
                                                     sizeof(seg_tags)/sizeof(Eterm),
                                                     seg_tags, emis->segs);
            list[lix] = erts_bld_2tup_list(hpp, szp, 2, group_tags, group);
            list_tags[lix] = am.supercarrier;
            lix++;
        }

        if (!mm->no_os_mmap) {
            group[0] = erts_bld_atom_uword_2tup_list(hpp, szp,
                                                      1, &am.used, &emis->os_used);
            list[lix] = erts_bld_2tup_list(hpp, szp, 1, group_tags, group);
            list_tags[lix] = am.os;
            lix++;
        }
        res = erts_bld_2tup_list(hpp, szp, lix, list_tags, list);
    }
    return res;
}

Eterm erts_mmap_info_options(ErtsMemMapper* mm,
                             char *prefix,
                             fmtfn_t *print_to_p,
                             void *print_to_arg,
                             Uint **hpp,
                             Uint *szp)
{
    const UWord scs = mm->sua.top - mm->sa.bot;
    const Eterm sco = mm->no_os_mmap ? am_true : am_false;
    const Eterm scrpm = (mm->reserve_physical == reserve_noop) ? am_true : am_false;
    Eterm res = THE_NON_VALUE;

    if (print_to_p) {
        fmtfn_t to = *print_to_p;
	void *arg = print_to_arg;
        erts_print(to, arg, "%sscs: %bpu\n", prefix, scs);
        if (mm->supercarrier) {
            erts_print(to, arg, "%ssco: %T\n", prefix, sco);
            erts_print(to, arg, "%sscrpm: %T\n", prefix, scrpm);
            erts_print(to, arg, "%sscrfsd: %beu\n", prefix, mm->desc.reserved);
        }
    }

    if (hpp || szp) {
        if (!am.is_initialized) {
            init_atoms();
        }

        res = NIL;
        if (mm->supercarrier) {
            add_2tup(hpp, szp, &res, am.scrfsd,
                     erts_bld_uint(hpp,szp, mm->desc.reserved));
            add_2tup(hpp, szp, &res, am.scrpm, scrpm);
            add_2tup(hpp, szp, &res, am.sco, sco);
        }
        add_2tup(hpp, szp, &res, am.scs, erts_bld_uword(hpp, szp, scs));
    }
    return res;
}

#endif /* HAVE_ERTS_MMAP */

Eterm erts_mmap_debug_info(Process* p)
{
#if HAVE_ERTS_MMAP
    ErtsMemMapper* mm = &erts_dflt_mmapper;

    if (mm->supercarrier) {
        ERTS_DECL_AM(sabot);
        ERTS_DECL_AM(satop);
        ERTS_DECL_AM(suabot);
        ERTS_DECL_AM(suatop);
        Eterm sa_list, sua_list, list;
        Eterm tags[] = { AM_sabot, AM_satop, AM_suabot, AM_suatop };
        UWord values[4];
        Eterm *hp, *hp_end;
        Uint may_need;

        erts_mtx_lock(&mm->mtx);
        values[0] = (UWord)mm->sa.bot;
        values[1] = (UWord)mm->sa.top;
        values[2] = (UWord)mm->sua.bot;
        values[3] = (UWord)mm->sua.top;
        sa_list = build_free_seg_list(p, &mm->sa.map);
        sua_list = build_free_seg_list(p, &mm->sua.map);
        erts_mtx_unlock(&mm->mtx);

        may_need = 4*(2+3+2) + 2*(2+3);
        hp = HAlloc(p, may_need);
        hp_end = hp + may_need;

        list = erts_bld_atom_uword_2tup_list(&hp, NULL,
                                            sizeof(values)/sizeof(*values),
                                            tags, values);

        sa_list = TUPLE2(hp, am_atom_put("sa_free_segs",12), sa_list); hp+=3;
        sua_list = TUPLE2(hp, am_atom_put("sua_free_segs",13), sua_list); hp+=3;
        list = CONS(hp, sua_list, list); hp+=2;
        list = CONS(hp, sa_list, list); hp+=2;

        ASSERT(hp <= hp_end);
        HRelease(p, hp_end, hp);
        return list;
    }
#endif
    return am_undefined;
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


#if 0
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

struct check_arg_t {
    RBTree* tree;
    ErtsFreeSegDesc* prev_seg;
    Uint size;
    RBTNode *res;
};
static void check_node_callback(RBTNode* x, void* arg);


static RBTNode *
check_tree(RBTree* tree, Uint size)
{
    struct check_arg_t carg;
    carg.tree = tree;
    carg.prev_seg = NULL;
    carg.size = size;
    carg.res = NULL;

#ifdef PRINT_TREE
    print_tree(tree->order, tree->root);
#endif

    if (!tree->root)
	return NULL;

    RBT_ASSERT(IS_BLACK(tree->root));
    RBT_ASSERT(!parent(tree->root));

    rbt_foreach_node(tree, check_node_callback, &carg, 0);

    return carg.res;
}

static void check_node_callback(RBTNode* x, void* arg)
{
    struct check_arg_t* a = (struct check_arg_t*) arg;
    ErtsFreeSegDesc* seg;

    if (IS_RED(x)) {
        RBT_ASSERT(IS_BLACK(x->right));
        RBT_ASSERT(IS_BLACK(x->left));
    }

    RBT_ASSERT(parent(x) || x == a->tree->root);

    if (x->left) {
        RBT_ASSERT(cmp_nodes(a->tree->order, x->left, x) < 0);
    }
    if (x->right) {
        RBT_ASSERT(cmp_nodes(a->tree->order, x->right, x) > 0);
    }

    seg = node_to_desc(a->tree->order, x);
    RBT_ASSERT(seg->start < seg->end);
    if (a->size && (seg->end - seg->start) >= a->size) {
        if (!a->res || cmp_nodes(a->tree->order, x, a->res) < 0) {
            a->res = x;
        }
    }
    if (a->tree->order == ADDR_ORDER) {
        RBT_ASSERT(!a->prev_seg || a->prev_seg->end < seg->start);
        a->prev_seg = seg;
    }
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
    fprintf(stderr, " --- %s ordered tree begin ---\r\n", sort_order_names[order]);
    print_tree_aux(order, root, 0);
    fprintf(stderr, " --- %s ordered tree end ---\r\n", sort_order_names[order]);
}

#endif /* PRINT_TREE */


#ifdef FREE_SEG_API_SMOKE_TEST

void test_it(void)
{
    ErtsFreeSegMap map;
    ErtsFreeSegDesc *desc, *under, *over, *d1, *d2;
    const int i = 1; /* reverse addr order */

    {
        init_free_seg_map(&map, SZ_REVERSE_ADDR_ORDER);

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

#endif /* FREE_SEG_API_SMOKE_TEST */


#ifdef HARD_DEBUG_MSEG

/*
 * Debug stuff used by erl_mseg to check that it does the right thing.
 * The reason for keeping it here is that we (ab)use the rb-tree code
 * for keeping track of *allocated* segments.
 */

typedef struct ErtsFreeSegDesc_fake_ {
    /*RBTNode snode;    Save memory by skipping unused size tree node */
    RBTNode anode;      /* node in 'atree' */
    union {
        char* start;
        struct ErtsFreeSegDesc_fake_* next_free;
    }u;
    char* end;
}ErtsFreeSegDesc_fake;

static ErtsFreeSegDesc_fake hard_dbg_mseg_desc_pool[10000];
static ErtsFreeSegDesc_fake* hard_dbg_mseg_desc_first;
RBTree hard_dbg_mseg_tree;

static erts_mtx_t hard_dbg_mseg_mtx;

static void hard_dbg_mseg_init(void)
{
    ErtsFreeSegDesc_fake* p;

    erts_mtx_init(&hard_dbg_mseg_mtx, "hard_dbg_mseg", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DEBUG);
    hard_dbg_mseg_tree.root = NULL;
    hard_dbg_mseg_tree.order = ADDR_ORDER;

    p = &hard_dbg_mseg_desc_pool[(sizeof(hard_dbg_mseg_desc_pool) /
                                  sizeof(*hard_dbg_mseg_desc_pool)) - 1];
    p->u.next_free = NULL;
    while (--p >= hard_dbg_mseg_desc_pool) {
        p->u.next_free = (p+1);
    }
    hard_dbg_mseg_desc_first = &hard_dbg_mseg_desc_pool[0];
}

static ErtsFreeSegDesc* hard_dbg_alloc_desc(void)
{
    ErtsFreeSegDesc_fake* p = hard_dbg_mseg_desc_first;
    ERTS_ASSERT(p || !"HARD_DEBUG_MSEG: Out of mseg descriptors");
    hard_dbg_mseg_desc_first = p->u.next_free;

    /* Creative pointer arithmetic to return something that looks like
     * a ErtsFreeSegDesc as long as we don't use the absent 'snode'.
     */
    return (ErtsFreeSegDesc*) ((char*)p - offsetof(ErtsFreeSegDesc,anode));
}

static void hard_dbg_free_desc(ErtsFreeSegDesc* desc)
{
    ErtsFreeSegDesc_fake* p = (ErtsFreeSegDesc_fake*) &desc->anode;
    memset(p, 0xfe, sizeof(*p));
    p->u.next_free = hard_dbg_mseg_desc_first;
    hard_dbg_mseg_desc_first = p;
}

static void check_seg_writable(void* seg, UWord sz)
{
    UWord* seg_end = (UWord*)((char*)seg + sz);
    volatile UWord* p;
    ERTS_ASSERT(ERTS_IS_PAGEALIGNED(seg));
    ERTS_ASSERT(ERTS_IS_PAGEALIGNED(sz));
    for (p=(UWord*)seg; p<seg_end; p += (ERTS_INV_PAGEALIGNED_MASK+1)/sizeof(UWord)) {
        UWord write_back = *p;
        *p = 0xfade2b1acc;
        *p = write_back;
    }
}

void hard_dbg_insert_mseg(void* seg, UWord sz)
{
    check_seg_writable(seg, sz);
    erts_mtx_lock(&hard_dbg_mseg_mtx);
    {
        ErtsFreeSegDesc *desc = hard_dbg_alloc_desc();
        RBTNode *prev, *next;
        desc->start = (char*)seg;
        desc->end = desc->start + sz - 1; /* -1 to allow adjacent segments in tree */
        rbt_insert(&hard_dbg_mseg_tree, &desc->anode);
        prev = rbt_prev_node(&desc->anode);
        next = rbt_next_node(&desc->anode);
        ERTS_ASSERT(!prev || anode_to_desc(prev)->end < desc->start);
        ERTS_ASSERT(!next || anode_to_desc(next)->start > desc->end);
    }
    erts_mtx_unlock(&hard_dbg_mseg_mtx);
}

static ErtsFreeSegDesc* hard_dbg_lookup_seg_at(RBTree* tree, char* start)
{
    RBTNode* x = tree->root;

    while (x) {
        ErtsFreeSegDesc* desc = anode_to_desc(x);
	if (start < desc->start) {
            x = x->left;
	}
        else if (start > desc->start) {
            ERTS_ASSERT(start > desc->end);
            x = x->right;
        }
	else
            return desc;
    }
    return NULL;
}

void hard_dbg_remove_mseg(void* seg, UWord sz)
{
    check_seg_writable(seg, sz);
    erts_mtx_lock(&hard_dbg_mseg_mtx);
    {
        ErtsFreeSegDesc* desc = hard_dbg_lookup_seg_at(&hard_dbg_mseg_tree, (char*)seg);
        ERTS_ASSERT(desc);
        ERTS_ASSERT(desc->start == (char*)seg);
        ERTS_ASSERT(desc->end == (char*)seg + sz - 1);

        rbt_delete(&hard_dbg_mseg_tree, &desc->anode);
        hard_dbg_free_desc(desc);
    }
    erts_mtx_unlock(&hard_dbg_mseg_mtx);
}

#endif /* HARD_DEBUG_MSEG */
