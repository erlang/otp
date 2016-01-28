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

#define MAX_SEGS 10

typedef struct {
    void *ptr;
    Ulong size;
} seg_t;

char *
testcase_name(void)
{
    return "mseg_clear_cache";
}

void
testcase_run(TestCaseState_t *tcs)
{
    int i;
    Ulong n;
    seg_t *seg;

    if (!HAVE_MSEG())
	testcase_skipped(tcs, "No mseg_alloc; nothing to test");

    seg = (seg_t *) testcase_alloc(sizeof(seg_t)*(MAX_SEGS+1));

    ASSERT(tcs, seg);

    for (i = 0; i <= MAX_SEGS; i++)
	seg[i].ptr = NULL;

    tcs->extra = &seg[0];

    for (i = 0; i < MAX_SEGS; i++) {
	seg[i].size = 1 << 18;
	seg[i].ptr = MSEG_ALLOC(&seg[i].size);
	ASSERT(tcs, seg[i].ptr);
	ASSERT(tcs, seg[i].size >= (1 << 18));
    }

    n = MSEG_NO();
    testcase_printf(tcs, "MSEG_NO() = %lu\n", n);

    ASSERT(tcs, n >= MAX_SEGS);

    testcase_printf(tcs, "Deallocating half of the segments\n");
    for (i = MAX_SEGS-1; i >= MAX_SEGS/2; i--) {
	MSEG_DEALLOC(seg[i].ptr, seg[i].size);
	seg[i].ptr = NULL;
    }

    n = MSEG_NO();
    testcase_printf(tcs, "MSEG_NO() = %lu\n", n);

    ASSERT(tcs, n >= MAX_SEGS/2);

    n = MSEG_CACHE_SIZE();
    testcase_printf(tcs, "MSEG_CACHE_SIZE() = %lu\n", n);
    ASSERT(tcs, n > 0); 

    testcase_printf(tcs, "MSEG_CLEAR_CACHE()\n");
    MSEG_CLEAR_CACHE();

    n = MSEG_CACHE_SIZE();
    testcase_printf(tcs, "MSEG_CACHE_SIZE() = %lu\n", n);

    ASSERT(tcs, n == 0); 

}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    if (tcs->extra) {
	seg_t *seg = (seg_t *) tcs->extra;
	int i;
	for (i = 0; seg[i].ptr; i++)
	    MSEG_DEALLOC(seg[i].ptr, seg[i].size);
	testcase_free((void *) seg);
	tcs->extra = NULL;
    }
}

ERL_NIF_INIT(mseg_clear_cache, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
