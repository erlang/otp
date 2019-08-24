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
#include <stdio.h>

#define MAX_TEST_SIZE 100000000

char *
testcase_name(void)
{
    return "bucket_index";
}

void test_it(TestCaseState_t *tcs, unsigned sbct);

void
testcase_run(TestCaseState_t *tcs)
{
    testcase_printf(tcs, "No of buckets = %lu:\n\n", NO_OF_BKTS);

    test_it(tcs, 1);
    test_it(tcs, 0);
    test_it(tcs, 1024);
    test_it(tcs, 10240);
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
test_it(TestCaseState_t *tcs, unsigned sbct)
{
    Ulong max_cont_test_sz;
    char sbct_buf[21];
    char *argv[] = {"-tas", "gf", "-tsbct", NULL, NULL};
    int no_changes;
    Ulong bi;
    Ulong min_sz;
    Ulong prev_bi;
    Ulong sz;
    Allctr_t *a;
    
    no_changes = 0;
    prev_bi = -1;

    if (sbct) {
	sprintf(sbct_buf, "%d", sbct);
	argv[3] = sbct_buf;
    }
    else
	argv[2] = NULL;

    max_cont_test_sz = 2*sbct*1024;
    if (max_cont_test_sz < 1000000)
	max_cont_test_sz = 1000000;

    testcase_printf(tcs, "Testing with sbct = %s\n",
		    sbct ? sbct_buf : "default");
    a = START_ALC("bkt_ix_", 0, argv);
    tcs->extra = (void *) a;
    ASSERT(tcs, a);

    sz = MIN_BLK_SZ(a);
    while(sz < ((((Ulong)1) << 31) - 1)) {
	bi = BKT_IX(a, sz);
	if (prev_bi != bi) {
	    ASSERT(tcs, prev_bi + 1 == bi);

	    min_sz = BKT_MIN_SZ(a, bi);

	    ASSERT(tcs, sz == min_sz);

	    testcase_printf(tcs, "sz=%d->ix=%d ", sz, bi);
	    no_changes++;
	}
	prev_bi = bi;
	if (sz < max_cont_test_sz)
	    sz++;
	else
	    sz += 100000000;
    }
    testcase_printf(tcs, "\n\n");
    ASSERT(tcs, no_changes == NO_OF_BKTS);

    STOP_ALC(a);
    tcs->extra = NULL;

    testcase_printf(tcs, "Test with sbct=%s succeeded\n",
		    sbct ? sbct_buf : "default");
}

ERL_NIF_INIT(bucket_index, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
