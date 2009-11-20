/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#include "testcase_driver.h"
#include "allocator_test.h"

#define NO_BLOCKS 100000

#define RIGHT_VISITED (1 << 0)
#define LEFT_VISITED (1 << 1)

typedef struct {
    Allctr_t *allocator;
    void **blk;
    void **fence;
} rbtree_test_data;

#if 0
#define PRINT_TREE
#endif

#ifdef PRINT_TREE

#define INDENT_STEP 5

#include <stdio.h>
static void
print_tree_aux(TestCaseState_t *tcs, RBT_t *x, int indent)
{
    if (!x) {
	char frmt[20];
	sprintf(frmt, "%%%ds%%s\n", indent);
	testcase_printf(tcs, frmt, "", "BLACK: nil");
    }
    else {
	print_tree_aux(tcs, RBT_RIGHT(x), indent + INDENT_STEP);
	{
	    char frmt[40];
	    sprintf(frmt, "%%%ds%%s: sz=%%lu addr=0x%%lx\n", indent);
	    testcase_printf(tcs,
			    frmt,
			    "",
			    RBT_IS_BLACK(x) ? "BLACK" : "RED",
			    BLK_SZ(x),
			    (Ulong) x);
	}
	print_tree_aux(tcs, RBT_LEFT(x), indent + INDENT_STEP);
    }
}


static void
print_tree(TestCaseState_t *tcs, RBT_t *root, int aobf)
{
    char *type = aobf ? "Size-Adress" : "Size";
    testcase_printf(tcs, " --- %s tree begin ---\r\n", type);
    print_tree_aux(tcs, root, 0);
    testcase_printf(tcs, " --- %s tree end ---\r\n", type);
}

#endif

static RBT_t *
check_tree(TestCaseState_t *tcs, Allctr_t *alc, Ulong size)
{
    int i, max_i, address_order;
    char stk[128];
    RBT_t *root, *x, *y, *res;
    Ulong x_sz, y_sz, is_x_black;
    long blacks, curr_blacks;

    res = NULL;

    address_order = IS_AOBF(alc);
    root = RBT_ROOT(alc);

#ifdef PRINT_TREE
    print_tree(tcs, root, address_order);
#endif

    max_i = i = -1;
    curr_blacks = 0;
    blacks = -1;

    if (!root)
	goto done;

    stk[++i] = 0;

    ASSERT(tcs, RBT_IS_BLACK(root));
    ASSERT(tcs, !RBT_PARENT(root));
    x = root;
    curr_blacks++;

    while (x) {

	ASSERT(tcs, i <= 128);

	if (!(stk[i] & LEFT_VISITED)) {
	    stk[i] |= LEFT_VISITED;
	    y = RBT_LEFT(x);
	    if (RBT_IS_BLACK(y))
		curr_blacks++;
	    if (y) {
		x = y;
		stk[++i] = 0;
		continue;
	    }
	    else {
		if (blacks < 0)
		    blacks = curr_blacks;
		ASSERT(tcs, blacks == curr_blacks);
		curr_blacks--;
	    }
	}

	if (!(stk[i] & RIGHT_VISITED)) {
	    stk[i] |= RIGHT_VISITED;
	    y = RBT_RIGHT(x);
	    if (RBT_IS_BLACK(y))
		curr_blacks++;
	    if (y) {
		x = y;
		stk[++i] = 0;
		continue;
	    }
	    else {
		if (blacks < 0)
		    blacks = curr_blacks;
		ASSERT(tcs, blacks == curr_blacks);
		curr_blacks--;
	    }
	}


	/* Check x ... */
	
	is_x_black = RBT_IS_BLACK(x);
	x_sz = BLK_SZ(x);


	if (!is_x_black) {
	    ASSERT(tcs, RBT_IS_BLACK(RBT_RIGHT(x)));
	    ASSERT(tcs, RBT_IS_BLACK(RBT_LEFT(x)));
	}

	ASSERT(tcs, RBT_PARENT(x) || x == root);

	y = RBT_LEFT(x);
	if (y) {
	    y_sz = BLK_SZ(y);
	    ASSERT(tcs, RBT_PARENT(y) == x);
	    if (address_order) {
		ASSERT(tcs, y_sz < x_sz || (y_sz == x_sz && y < x));
	    }
	    else {
		ASSERT(tcs, RBT_IS_TREE(y));
		ASSERT(tcs, y_sz < x_sz);
	    }
	}

	y = RBT_RIGHT(x);
	if (y) {
	    y_sz = BLK_SZ(y);
	    ASSERT(tcs, RBT_PARENT(y) == x);
	    if (address_order) {
		ASSERT(tcs, y_sz > x_sz || (y_sz == x_sz && y > x));
	    }
	    else {
		ASSERT(tcs, RBT_IS_TREE(y));
		ASSERT(tcs, y_sz > x_sz);
	    }
	}

	if (!address_order) {
	    Ulong l_sz;
	    RBTL_t *l = RBT_NEXT(x);
	    for (l = RBT_NEXT(x); l; l = RBT_NEXT(l)) {
		l_sz = BLK_SZ(l);
		ASSERT(tcs, l_sz == x_sz);
		ASSERT(tcs, !RBT_IS_TREE(l));
	    }
	}

	if (size && x_sz >= size) {
	    if (!res)
		res = x;
	    else {
		y_sz = BLK_SZ(res);
		if (address_order) {
		    if (x_sz < y_sz || (x_sz == y_sz && x < res))
			res = x;
		}
		else {
		    if (!res || x_sz < y_sz)
			res = x;
		}
	    }
	}

	if (max_i < i)
	    max_i = i;
	if (is_x_black)
	    curr_blacks--;
	x = RBT_PARENT(x);
	i--;
    }

 done:
    ASSERT(tcs, curr_blacks == 0);
    ASSERT(tcs, i == -1);

    testcase_printf(tcs, "Red-Black Tree OK! Max depth = %d; "
		    "Black depth = %d\n", max_i+1, blacks < 0 ? 0 : blacks);

    return res;

}

static void
do_check(TestCaseState_t *tcs, Allctr_t *a, Ulong size)
{
    Ulong sz = ((size + 7) / 8)*8;
    void *tmp;
    Block_t *x, *y;

    x = (Block_t *) check_tree(tcs, a, sz);
    tmp = ALLOC(a, sz - ABLK_HDR_SZ);
    ASSERT(tcs, tmp);
    y = UMEM2BLK(tmp);
    if (IS_AOBF(a)) {
	ASSERT(tcs, x == y);
    }
    else {
	ASSERT(tcs, BLK_SZ(x) == BLK_SZ(y));
    }
    FREE(a, tmp);
}


static void
test_it(TestCaseState_t *tcs)
{
    int i;
    Allctr_t a = ((rbtree_test_data *) tcs->extra)->allocator;
    void **blk = ((rbtree_test_data *) tcs->extra)->blk;
    void **fence = ((rbtree_test_data *) tcs->extra)->fence;
    Ulong min_blk_sz;

    min_blk_sz = MIN_BLK_SZ(a);

    for (i = 0; i < NO_BLOCKS; i++) {
	blk[i] = ALLOC(a, min_blk_sz + i % 500);
	fence[i] = ALLOC(a, 1);
	ASSERT(tcs, blk[i] && fence[i]);
    }

    for (i = 0; i < NO_BLOCKS; i++) {
	if (i % 3 == 0) {
	    FREE(a, blk[i]);
	    blk[i] = NULL;
	}
	if (i % (NO_BLOCKS/2) == 0)
	    do_check(tcs, a, 50);
    }

    for (i = 0; i < NO_BLOCKS; i++) {
	if (i % 5 == 0 && blk[i]) {
	    FREE(a, blk[i]);
	    blk[i] = NULL;
	}
	if (i % (NO_BLOCKS/2) == 0)
	    do_check(tcs, a, 200);
    }

    for (i = 0; i < NO_BLOCKS; i++) {
	if (blk[i]) {
	    FREE(a, blk[i]);
	    blk[i] = NULL;
	}
	if (i % (NO_BLOCKS/2) == 0)
	    do_check(tcs, a, 100);
    }

    do_check(tcs, a, 250);

    for (i = 0; i < NO_BLOCKS; i++) {
	FREE(a, fence[i]);
	if (i % (NO_BLOCKS/3) == 0)
	    do_check(tcs, a, 300);
    }

    ASSERT(tcs, RBT_ROOT(a));
    ASSERT(tcs, !RBT_LEFT(RBT_ROOT(a)));
    ASSERT(tcs, !RBT_RIGHT(RBT_ROOT(a)));
}


char *
testcase_name(void)
{
    return "rbtree";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    if (tcs->extra) {
	rbtree_test_data *td = tcs->extra;
	tcs->extra = NULL;
	if (td->allocator)
	    STOP_ALC(td->allocator);
	if (td->blk)
	    testcase_free((void *) td->blk);
	if (td->fence)
	    testcase_free((void *) td->fence);
	testcase_free((void *) td);
    }
}

void
testcase_run(TestCaseState_t *tcs)
{
    char *argv1[] = {"-tasbf", NULL};
    char *argv2[] = {"-tasaobf", NULL};
    Allctr_t *a;
    rbtree_test_data *td;

    /* Best fit... */

    testcase_printf(tcs, "Setup...\n");

    td = (rbtree_test_data *) testcase_alloc(sizeof(rbtree_test_data));
    ASSERT(tcs, td);
    tcs->extra = (void *) td;
    td->allocator = NULL;
    td->blk = (void **) testcase_alloc(sizeof(void *)*NO_BLOCKS);
    td->fence = (void **) testcase_alloc(sizeof(void *)*NO_BLOCKS);
    ASSERT(tcs, td->blk && td->fence);

    testcase_printf(tcs, "Starting test of best fit...\n");

    td->allocator = a = START_ALC("rbtree_bf_", 0, argv1);

    ASSERT(tcs, a);
    ASSERT(tcs, !IS_AOBF(a));

    test_it(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "Best fit test succeeded!\n");

    /* Address order best fit... */

    testcase_printf(tcs, "Starting test of address order best fit...\n");

    td->allocator = a = START_ALC("rbtree_aobf_", 0, argv2);

    ASSERT(tcs, a);
    ASSERT(tcs, IS_AOBF(a));

    test_it(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "Address order best fit test succeeded!\n");

}
