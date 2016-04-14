/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#include "testcase_driver.h"
#include "allocator_test.h"

int NO_BLOCKS;

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

/* Ugly hack to steer the test code towards the right allocator */
#define RBT_OP(CMD) (current_rbt_type_op_base + (CMD))
static enum {
    BESTFIT_OP_BASE = 0x200,
    AO_FIRSTFIT_OP_BASE = 0x500
}current_rbt_type_op_base;


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
print_tree(TestCaseState_t *tcs, RBT_t *root)
{
    testcase_printf(tcs, " --- Tree begin ---\r\n");
    print_tree_aux(tcs, root, 0);
    testcase_printf(tcs, " --- Tree end ---\r\n");
}

#endif

static RBT_t *
check_tree(TestCaseState_t *tcs, Allctr_t *alc, Ulong size)
{
    enum { BF, AOBF, AOFF } type;
    int i, max_i;
    char stk[128];
    RBT_t *root, *x, *y, *res;
    Ulong x_sz, y_sz, is_x_black;
    long blacks, curr_blacks;
    int have_max_sz;

    res = NULL;

    if (IS_AOBF(alc)) type = AOBF;
    else if (IS_BF(alc)) type = BF;
    else type = AOFF;

    have_max_sz = !IS_BF_ALGO(alc);

    root = RBT_ROOT(alc, size);

#ifdef PRINT_TREE
    print_tree(tcs, root);
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
	    switch (type) {
	    case AOBF:
		ASSERT(tcs, y_sz < x_sz || (y_sz == x_sz && y < x));
		break;
	    case BF:
		ASSERT(tcs, RBT_IS_TREE(y));
		ASSERT(tcs, y_sz < x_sz);
		break;
	    case AOFF:
		ASSERT(tcs, y < x);
		break;
	    }
	    if (have_max_sz) {
		ASSERT(tcs, RBT_MAX_SZ(y) <= RBT_MAX_SZ(x));
	    }
	}

	y = RBT_RIGHT(x);
	if (y) {
	    y_sz = BLK_SZ(y);
	    ASSERT(tcs, RBT_PARENT(y) == x);
	    switch (type) {
	    case AOBF:
		ASSERT(tcs, y_sz > x_sz || (y_sz == x_sz && y > x));
		break;
	    case BF:
		ASSERT(tcs, RBT_IS_TREE(y));
		ASSERT(tcs, y_sz > x_sz);
		break;
	    case AOFF:
		ASSERT(tcs, y > x);
		break;
	    }
	    if (have_max_sz) {
		ASSERT(tcs, RBT_MAX_SZ(y) <= RBT_MAX_SZ(x));
	    }
	}

	if (type == BF) {
	    Ulong l_sz;
	    RBTL_t *l, *prev=x;
	    for (l = RBT_NEXT(x); l; l = RBT_NEXT(l)) {
		l_sz = BLK_SZ(l);
		ASSERT(tcs, l_sz == x_sz);
		ASSERT(tcs, !RBT_IS_TREE(l));
		ASSERT(tcs, RBT_PREV(l) == prev);
		prev = l;
	    }
	}

	if (size && x_sz >= size) {
	    if (!res)
		res = x;
	    else {
		y_sz = BLK_SZ(res);
		switch (type) {
		case AOBF:
		    if (x_sz < y_sz || (x_sz == y_sz && x < res))
			res = x;
		    break;
		case BF:
		    if (x_sz < y_sz)
			res = x;
		    break;
		case AOFF:
		    if (x < res) {
			res = x;
		    }
		    break;
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

    /*
    testcase_printf(tcs, "Red-Black Tree OK! Max depth = %d; "
		    "Black depth = %d\n", max_i+1, blacks < 0 ? 0 : blacks);
    */
    return res;

}

static void
do_check(TestCaseState_t *tcs, Allctr_t *a, Ulong size, int ignore_null)
{
    Ulong sz = ((size + 7) / 8)*8;
    void *tmp;
    Block_t *x, *y;

    x = (Block_t *) check_tree(tcs, a, sz);
    if (!x && ignore_null)
	return;

    tmp = ALLOC(a, sz - ABLK_HDR_SZ);
    ASSERT(tcs, tmp);
    y = UMEM2BLK(tmp);
    if (!IS_BF(a)) {
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
    Allctr_t* a = ((rbtree_test_data *) tcs->extra)->allocator;
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
	    do_check(tcs, a, 50, 0);
    }

    for (i = 0; i < NO_BLOCKS; i++) {
	if (i % 5 == 0 && blk[i]) {
	    FREE(a, blk[i]);
	    blk[i] = NULL;
	}
	if (i % (NO_BLOCKS/2) == 0)
	    do_check(tcs, a, 200, 0);
    }

    for (i = 0; i < NO_BLOCKS; i++) {
	if (blk[i]) {
	    FREE(a, blk[i]);
	    blk[i] = NULL;
	}
	if (i % (NO_BLOCKS/2) == 0)
	    do_check(tcs, a, 100, 0);
    }

    do_check(tcs, a, 250, 0);

    for (i = 0; i < NO_BLOCKS; i++) {
	FREE(a, fence[i]);
	if (i % (NO_BLOCKS/3) == 0)
	    do_check(tcs, a, 300, 0);
    }

    ASSERT(tcs, RBT_ROOT(a,0));
    ASSERT(tcs, !RBT_LEFT(RBT_ROOT(a,0)));
    ASSERT(tcs, !RBT_RIGHT(RBT_ROOT(a,0)));
}


static int is_single_ablk_in_mbc(Allctr_t* a, void* ptr, void* crr)
{
    Block_t* blk = UMEM2BLK(ptr);
    if (crr == BLK_TO_MBC(blk)) {
	Block_t* first = MBC_TO_FIRST_BLK(a, crr);
	if (blk == first || (IS_FREE_BLK(first) && blk == NXT_BLK(first))) {
	    Block_t* nxt;
	    if (IS_LAST_BLK(blk)) {
		return 1;
	    }
	    nxt = NXT_BLK(blk);
	    return IS_FREE_BLK(nxt) && IS_LAST_BLK(nxt);
	}
    }
    return 0;
}

static void
test_carrier_migration(TestCaseState_t *tcs)
{
    int i, j;
    Allctr_t* a = ((rbtree_test_data *) tcs->extra)->allocator;
    void **blk = ((rbtree_test_data *) tcs->extra)->blk;
    void **fence = ((rbtree_test_data *) tcs->extra)->fence;
    void *crr, *p, *free_crr;
    Ulong min_blk_sz;

    min_blk_sz = MIN_BLK_SZ(a);

    for (i = 0; i < NO_BLOCKS; i++) {
	blk[i] = ALLOC(a, min_blk_sz + i % 500);
	fence[i] = ALLOC(a, 1);
	ASSERT(tcs, blk[i] && fence[i]);
    }

    for (j = 0; j < NO_BLOCKS; j += 997) {
	crr = BLK_TO_MBC(UMEM2BLK(blk[j]));
	REMOVE_MBC(a, crr);

	for (i = 0; i < NO_BLOCKS; i++) {
	    if (i % 3 == 0) {
		if (is_single_ablk_in_mbc(a, blk[i], crr)) {
		    crr = NULL; /* about to destroy the removed mbc */
		}
		FREE(a, blk[i]);
		blk[i] = NULL;
	    }
	    if (i % (NO_BLOCKS/2) == 0)
		do_check(tcs, a, 50, 1);
	}
	
	for (i = 0; i < NO_BLOCKS; i++) {
	    if (i % 3 == 0) {
		ASSERT(tcs, !blk[i]);
		blk[i] = ALLOC(a, min_blk_sz + i % 500);
		ASSERT(tcs, BLK_TO_MBC(UMEM2BLK(blk[i])) != crr);
	    }
	    if (i % (NO_BLOCKS/2) == 0)
		do_check(tcs, a, 50, 1);
	}
	if (crr) {
	    ADD_MBC(a, crr);
	}
    }
	
    for (crr = FIRST_MBC(a); crr; crr = NEXT_C(crr)) {
	REMOVE_MBC(a, crr);
    }

    p = ALLOC(a, 1);
    free_crr = BLK_TO_MBC(UMEM2BLK(p));
    FREE(a, p);

    for (crr = FIRST_MBC(a); crr; crr = NEXT_C(crr)) {
	ASSERT(tcs, free_crr != crr);
    }

    ASSERT(tcs, !RBT_ROOT(a,0));
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
    char *argv3[] = {"-tasaoff", NULL};
    char *argv4[] = {"-tasaoffcaobf", NULL};
    char *argv5[] = {"-tasaoffcbf", NULL};
    Allctr_t *a;
    rbtree_test_data *td;

    NO_BLOCKS = 100*1000;
    if (enif_is_identical(tcs->build_type,
                          enif_make_atom(tcs->curr_env,"valgrind"))) {
        NO_BLOCKS /= 10;
    }

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

    current_rbt_type_op_base = BESTFIT_OP_BASE;
    td->allocator = a = START_ALC("rbtree_bf_", 0, argv1);

    ASSERT(tcs, a);
    ASSERT(tcs, IS_BF_ALGO(a));
    ASSERT(tcs, !IS_AOBF(a));
    ASSERT(tcs, IS_BF(a));

    test_it(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "Best fit test succeeded!\n");

    /* Address order best fit... */

    testcase_printf(tcs, "Starting test of address order best fit...\n");

    current_rbt_type_op_base = BESTFIT_OP_BASE;
    td->allocator = a = START_ALC("rbtree_aobf_", 0, argv2);

    ASSERT(tcs, a);
    ASSERT(tcs, IS_BF_ALGO(a));
    ASSERT(tcs, IS_AOBF(a));
    ASSERT(tcs, !IS_BF(a));

    test_it(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "Address order best fit test succeeded!\n");

    /* Address order first fit... */

    testcase_printf(tcs, "Starting test of address order first fit...\n");

    current_rbt_type_op_base = AO_FIRSTFIT_OP_BASE;
    td->allocator = a = START_ALC("rbtree_aoff_", 0, argv3);

    ASSERT(tcs, a);
    ASSERT(tcs, !IS_BF_ALGO(a));
    ASSERT(tcs, !IS_AOBF(a));
    ASSERT(tcs, !IS_BF(a));

    test_it(tcs);
    test_carrier_migration(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "Address order first fit test succeeded!\n");

    /* Address order first fit, aobf within carrier */

    testcase_printf(tcs, "Starting test of aoffcaobf...\n");

    current_rbt_type_op_base = AO_FIRSTFIT_OP_BASE;
    td->allocator = a = START_ALC("rbtree_aoffcaobf_", 0, argv4);

    ASSERT(tcs, a);
    ASSERT(tcs, !IS_BF_ALGO(a));
    ASSERT(tcs, IS_AOBF(a));
    ASSERT(tcs, !IS_BF(a));

    test_it(tcs);
    test_carrier_migration(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "aoffcaobf test succeeded!\n");

    /* Address order first fit, bf within carrier */

    testcase_printf(tcs, "Starting test of aoffcbf...\n");

    current_rbt_type_op_base = AO_FIRSTFIT_OP_BASE;
    td->allocator = a = START_ALC("rbtree_aoffcbf_", 0, argv5);

    ASSERT(tcs, a);
    ASSERT(tcs, !IS_BF_ALGO(a));
    ASSERT(tcs, !IS_AOBF(a));
    ASSERT(tcs, IS_BF(a));

    test_it(tcs);
    test_carrier_migration(tcs);

    STOP_ALC(a);
    td->allocator = NULL;

    testcase_printf(tcs, "aoffcaobf test succeeded!\n");

}

ERL_NIF_INIT(rbtree, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
