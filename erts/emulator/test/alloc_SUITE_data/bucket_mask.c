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
#include <stdio.h>

#define SBCT (512*1024)

char *
testcase_name(void)
{
    return "bucket_mask";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    if (tcs->extra) {
	STOP_ALC(tcs->extra);
	tcs->extra = NULL;
    }
}

void
testcase_run(TestCaseState_t *tcs)
{
    void *tmp;
    void **fence;
    void **blk;
    Ulong sz;
    Ulong smbcs;
    int i;
    int bi;
    int bi_tests;
    Ulong sbct = (SBCT/1024)*1024;
    Ulong min_blk_sz;
    Ulong ablk_hdr_sz = ABLK_HDR_SZ;
    char smbcs_buf[30];
    char sbct_buf[30];
    int no_bkts = (int) NO_OF_BKTS;
    char *argv1[] = {"-tasgf", "-tmmbcs0", sbct_buf, NULL};
    char *argv2[] = {"-tasgf", "-tmmbcs0", sbct_buf, NULL, NULL};
    Allctr_t *a;

    sprintf(sbct_buf, "-tsbct%lu", sbct/1024);

    a = START_ALC("bkt_mask_1_", 0, argv1);
    tcs->extra = (void *) a;
    ASSERT(tcs, a);

    min_blk_sz = MIN_BLK_SZ(a);
    smbcs = 2*(no_bkts*sizeof(void *) + min_blk_sz) + min_blk_sz;
    for (i = 0; i < no_bkts; i++) {
	sz = BKT_MIN_SZ(a, i);
	if (sz >= sbct)
	    break;
	smbcs += sz + min_blk_sz;
    }

    bi_tests = i;
    testcase_printf(tcs, "Will test %d buckets\n", bi_tests);

    STOP_ALC(a);
    tcs->extra = NULL;

    smbcs /= 1024;
    smbcs++;

    testcase_printf(tcs, "smbcs = %lu\n", smbcs);
    sprintf(smbcs_buf, "-tsmbcs%lu", smbcs);
    argv2[3] = smbcs_buf;

    a = START_ALC("bkt_mask_2_", 0, argv2);
    tcs->extra = (void *) a;
    ASSERT(tcs, a);

    blk = (void **) ALLOC(a, no_bkts*sizeof(void *));
    fence = (void **) ALLOC(a, no_bkts*sizeof(void *));

    ASSERT(tcs, blk && fence);

    testcase_printf(tcs, "Allocating blocks and fences\n");
    for (i = 0; i < bi_tests; i++) {
	sz = BKT_MIN_SZ(a, i);
	blk[i] = ALLOC(a, sz - ablk_hdr_sz);
	fence[i] = ALLOC(a, 1);
	ASSERT(tcs, blk[i] && fence[i]);
    }

    tmp = (void *) UMEM2BLK(fence[bi_tests - 1]);
    tmp = (void *) NXT_BLK((Block_t *) tmp);
    ASSERT(tcs, IS_LAST_BLK(tmp));
    sz = BLK_SZ((Block_t *) tmp);
    testcase_printf(tcs, "Allocating leftover size = %lu\n", sz);
    tmp = ALLOC(a, sz - ablk_hdr_sz);
    ASSERT(tcs, tmp);

    bi = FIND_BKT(a, 0);
    ASSERT(tcs, bi < 0);

    for (i = 0; i < bi_tests; i++) {
	sz = BKT_MIN_SZ(a, i);
	testcase_printf(tcs, "Testing bucket %d\n", i);
	FREE(a, blk[i]);
	bi = FIND_BKT(a, i);
	ASSERT(tcs, bi == i);
	blk[i] = ALLOC(a, sz - ablk_hdr_sz);
	bi = FIND_BKT(a, i);
	ASSERT(tcs, bi != i);
    }

    for (i = 0; i < bi_tests; i++) {
	FREE(a, blk[i]);
	FREE(a, fence[i]);
    }

    FREE(a, (void *) blk);
    FREE(a, (void *) fence);

    bi = FIND_BKT(a, 0);
    ASSERT(tcs, bi == no_bkts - 1);

    FREE(a, tmp);

    bi = FIND_BKT(a, 0);
    ASSERT(tcs, bi < 0);

    STOP_ALC(a);
    tcs->extra = NULL;
}

