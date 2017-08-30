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
#include <string.h>

#define CEILING(X, U) ((((X)+(U)-1)/(U))*(U))

void
check_ablk(TestCaseState_t *tcs, Allctr_t *a, void *ptr, Ulong umem_sz)
{
    Ulong unit_sz = UNIT_SZ;
    Block_t *blk = UMEM2BLK(ptr);
    Block_t *nxt_blk = NXT_BLK(blk);
    Ulong real_sz = ((Ulong) nxt_blk) - ((Ulong) (blk));
    ASSERT(tcs, real_sz == BLK_SZ(blk));
    ASSERT(tcs, !IS_FREE_BLK(blk));
    ASSERT(tcs, real_sz >= CEILING(ABLK_HDR_SZ + umem_sz, unit_sz));
    if (real_sz > MIN_BLK_SZ(a)
	&& real_sz > CEILING(ABLK_HDR_SZ+umem_sz, unit_sz)) {
	ASSERT(tcs,
	       real_sz <= CEILING(MIN_BLK_SZ(a)+ABLK_HDR_SZ+umem_sz,
				  unit_sz));
	ASSERT(tcs, IS_LAST_BLK(blk) || !IS_FREE_BLK(nxt_blk));
    }
}

void
setup_sequence(TestCaseState_t *tcs, Allctr_t *a, Ulong bsz, int no,
	       void *res[])
{
    Carrier_t *c;
    Block_t *blk;
    int i;

    testcase_printf(tcs,
		    "Setting up a sequence of %d blocks of size %lu\n",
		    no, bsz);
    c = FIRST_MBC(a);
    ASSERT(tcs, !NEXT_C(c));
    blk = MBC_TO_FIRST_BLK(a, c);
    ASSERT(tcs, IS_LAST_BLK(blk));

    for (i = 0; i < no; i++)
	res[i] = ALLOC(a, bsz);
    for (i = 0; i < no; i++)
	ASSERT(tcs, res[i]);

    testcase_printf(tcs, "Checking that sequence was set up as expected\n");

    for (i = 1; i < no; i++)
	ASSERT(tcs, NXT_BLK(UMEM2BLK(res[i-1])) == UMEM2BLK(res[i]));

    blk = NXT_BLK(UMEM2BLK(res[no-1]));
    ASSERT(tcs, IS_LAST_BLK(blk));

    testcase_printf(tcs, "Sequence ok\n");

    /* If we fail in setup_sequence(), it doesn't mean that something is
       wrong. It is just a faulty assumption in setup_sequence() about
       how blocks are going to be placed.
       Fix setup_sequence()... */
}

static void
test_free(TestCaseState_t *tcs, Allctr_t *a, Ulong bsz)
{
    Block_t *blk;
    void *p[7];

    testcase_printf(tcs," --- Testing free() with block size %lu ---\n",bsz);

    setup_sequence(tcs, a, bsz, 7, p);

    check_ablk(tcs, a, p[0], bsz);
    check_ablk(tcs, a, p[1], bsz);
    check_ablk(tcs, a, p[2], bsz);
    check_ablk(tcs, a, p[3], bsz);
    check_ablk(tcs, a, p[4], bsz);
    check_ablk(tcs, a, p[5], bsz);
    check_ablk(tcs, a, p[6], bsz);

    /* Coalescing with previous block */
    FREE(a, p[2]);
    FREE(a, p[3]); 

    blk = NXT_BLK(UMEM2BLK(p[1]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[4]));

    /* Coalescing with next block */

    FREE(a, p[1]);
    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[4]));
    
    /* Coalescing with next and previous block */

    FREE(a, p[5]);
    FREE(a, p[4]);

    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[6]));
    
    /* Cleanup */

    FREE(a, p[0]);
    FREE(a, p[6]);

    testcase_printf(tcs," --- free() with block size %lu succeded ---\n",bsz);
}

static void
test_realloc(TestCaseState_t *tcs, Allctr_t *a, Ulong bsz)
{
    Block_t *blk;
    void *ptr;
    void *p[3];
    Ulong nbsz;

    testcase_printf(tcs," --- Testing realloc() with block size %lu ---\n",
		    bsz);

    setup_sequence(tcs, a, bsz, 3, p);

    check_ablk(tcs, a, p[0], bsz);
    check_ablk(tcs, a, p[1], bsz);
    check_ablk(tcs, a, p[2], bsz);

    /* Grow to the end of the carrier */
    blk = NXT_BLK(UMEM2BLK(p[2]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, IS_LAST_BLK(blk));
    nbsz = bsz + BLK_SZ(blk);
    ptr = REALLOC(a, p[2], nbsz);
    ASSERT(tcs, p[2] == ptr);
    check_ablk(tcs, a, p[2], nbsz);
    blk = UMEM2BLK(p[2]);
    ASSERT(tcs, IS_LAST_BLK(blk));

    /* Shrink from the end of the carrier */
    ptr = REALLOC(a, p[2], bsz);
    ASSERT(tcs, p[2] == ptr);
    blk = UMEM2BLK(p[2]);
    ASSERT(tcs, !IS_LAST_BLK(blk));
    blk = NXT_BLK(blk);
    ASSERT(tcs, IS_LAST_BLK(blk));
    check_ablk(tcs, a, p[2], bsz);

    /* Shrink and coalecse with next free */

    FREE(a, p[1]);

    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, IS_FREE_BLK(blk));

    nbsz = bsz/2;
    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);

    check_ablk(tcs, a, p[0], nbsz);

    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[2]));

    /* Grow into next free; but leave free block at end */

    nbsz *= 3;
    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);

    check_ablk(tcs, a, p[0], nbsz);
    blk = NXT_BLK(UMEM2BLK(p[0]));

    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[2]));

    /* Grow upto next alloced block by allocating just enough so that no
       free block fits between them */
    nbsz = BLK_SZ(blk) + UMEM_SZ(UMEM2BLK(p[0]));
    nbsz -= MIN_BLK_SZ(a) - 1;

    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);
    check_ablk(tcs, a, p[0], nbsz);
    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, !IS_FREE_BLK(blk));
    ASSERT(tcs, blk == UMEM2BLK(p[2]));

    /* Grow into unused part at end */
    nbsz += MIN_BLK_SZ(a) - 1;
    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);
    check_ablk(tcs, a, p[0], nbsz);
    ASSERT(tcs, !IS_FREE_BLK(blk));
    ASSERT(tcs, blk == UMEM2BLK(p[2]));

    /* Shrink *almost* as much so that a free block would fit between the
       allocated blocks, and make sure that we don't get a free block
       in between */
    nbsz -= MIN_BLK_SZ(a) - 1;
    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);
    check_ablk(tcs, a, p[0], nbsz);
    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, !IS_FREE_BLK(blk));
    ASSERT(tcs, blk == UMEM2BLK(p[2]));

    /* Shrink just as much so that a free block can fit between
       the alloced blocks */
    nbsz -= 1;
    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);
    check_ablk(tcs, a, p[0], nbsz);
    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, blk < UMEM2BLK(p[2]));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[2]));

    /* Shrink so little that no free block would fit between allocated
       blocks, and make sure that we shrink the allocated block and
       coalesce the extra free part with the next free block. */
    nbsz -= MIN_BLK_SZ(a) - 1;
    ptr = REALLOC(a, p[0], nbsz);
    ASSERT(tcs, p[0] == ptr);
    check_ablk(tcs, a, p[0], nbsz);
    blk = NXT_BLK(UMEM2BLK(p[0]));
    ASSERT(tcs, IS_FREE_BLK(blk));
    ASSERT(tcs, blk < UMEM2BLK(p[2]));
    ASSERT(tcs, NXT_BLK(blk) == UMEM2BLK(p[2]));

    /* Cleanup */
    FREE(a, p[0]);
    FREE(a, p[2]);

    testcase_printf(tcs, " --- realloc() with block size %lu succeded ---\n",
		    bsz);

}

char *
testcase_name(void)
{
    return "coalesce";
}

void
testcase_run(TestCaseState_t *tcs)
{
    char *argv_org[] = {"-tsmbcs511","-tmmbcs511", "-tsbct512", "-trmbcmt100", "-tas", NULL, NULL};
    char *alg[] = {"af", "gf", "bf", "aobf", "aoff", "aoffcbf", "aoffcaobf", NULL};
    int i;

    for (i = 0; alg[i]; i++) {
	Ulong sz;
	Allctr_t *a;
	char *argv[sizeof(argv_org)/sizeof(argv_org[0])];
	memcpy((void *) argv, (void *) argv_org, sizeof(argv_org));

	argv[5] = alg[i];
	testcase_printf(tcs, " *** Starting \"%s\" allocator *** \n", alg[i]);
	a = START_ALC("coalesce_", 0, argv);
	ASSERT(tcs, a);
	tcs->extra = (void *) a;

	sz = MIN_BLK_SZ(a) - ABLK_HDR_SZ;
	test_free(tcs, a, sz);
	sz += 1;
	test_free(tcs, a, sz);
	sz *= 4; 
	test_free(tcs, a, sz);
	sz += 1;
	test_free(tcs, a, sz);
	sz *= 10;
	test_free(tcs, a, sz);

	sz = MIN_BLK_SZ(a)*4 - ABLK_HDR_SZ;
	test_realloc(tcs, a, sz);
	sz += 1;
	test_realloc(tcs, a, sz);
	sz *= 4; 
	test_realloc(tcs, a, sz);
	sz += 1;
	test_realloc(tcs, a, sz);
	sz *= 10;
	test_realloc(tcs, a, sz);

	testcase_printf(tcs, " *** Stopping \"%s\" allocator *** \n", alg[i]);
	STOP_ALC(a);
	tcs->extra = NULL;
    }
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    if (tcs->extra)
	STOP_ALC((Allctr_t *) tcs->extra);
}

ERL_NIF_INIT(coalesce, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
