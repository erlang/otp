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

#include "erl_int_sizes_config.h"

#include "testcase_driver.h"
#include "allocator_test.h"
#include <stdio.h>

#if defined(__WIN32__) && SIZEOF_VOID_P == 8
/* Use larger threashold for win64 as block alignment
   is 16 bytes and not 8 */
#define SBCT ((1024*1024))
#else
#define SBCT ((512*1024))
#endif

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
    typedef struct linked_block {
	struct linked_block* next;
    }Linked;
    Linked* link = NULL;
    Linked* fence_list;
    Linked* pad_list;
    void* tmp;
    void **blk;
    Ulong sz;
    Ulong residue;
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
    smbcs = (no_bkts*sizeof(void *) + min_blk_sz) + min_blk_sz;
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

    ASSERT(tcs, blk);
    fence_list = NULL;

    testcase_printf(tcs, "Allocating blocks and fences\n");
    for (i = 0; i < bi_tests; i++) {
	sz = BKT_MIN_SZ(a, i);
	blk[i] = ALLOC(a, sz - ablk_hdr_sz);
	link = (Linked*) ALLOC(a, sizeof(Linked));
	ASSERT(tcs, blk[i] && link);
	link->next = fence_list;
	fence_list = link;
    }

    pad_list = 0;
    do {
	tmp = (void *) UMEM2BLK(link); /* last allocated */
	tmp = (void *) NXT_BLK((Block_t *) tmp);
	ASSERT(tcs, IS_LAST_BLK(tmp));
	sz = BLK_SZ((Block_t *) tmp);
	if (sz >= sbct) {
	    residue = sz;
	    sz = sbct - min_blk_sz;
	    residue -= sz;
	}
	else {
	    residue = 0;
	}
	testcase_printf(tcs, "Allocating leftover size = %lu, residue = %lu\n", sz, residue);
	link = (Linked*) ALLOC(a, sz - ablk_hdr_sz);
	ASSERT(tcs, link);
	link->next = pad_list;
	pad_list = link;
    } while (residue);

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
    }
    while (fence_list) {
	link = fence_list;
	fence_list = link->next;
	FREE(a, link);
    }

    FREE(a, (void *) blk);

    bi = FIND_BKT(a, 0);
    ASSERT(tcs, bi == no_bkts - 1);

    while (pad_list) {
	link = pad_list;
	pad_list = link->next;
	FREE(a, link);
    }

    bi = FIND_BKT(a, 0);
    ASSERT(tcs, bi < 0);

    STOP_ALC(a);
    tcs->extra = NULL;
}

ERL_NIF_INIT(bucket_mask, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
