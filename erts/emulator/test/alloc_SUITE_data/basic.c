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

char *
testcase_name(void)
{
    return "basic";
}

void
testcase_run(TestCaseState_t *tcs)
{
    Carrier_t *c;
    Block_t *blk;
    void *p;
    Allctr_t *a = START_ALC("basic_", 0, NULL);
    tcs->extra = (void *) a;

    ASSERT(tcs, a);

    p = ALLOC(a, 10);
    ASSERT(tcs, p);
    p = REALLOC(a, p, 15);
    ASSERT(tcs, p);
    FREE(a, p);

    c = FIRST_MBC(a);
    ASSERT(tcs, !NEXT_C(c));
    blk = MBC_TO_FIRST_BLK(a, c);
    ASSERT(tcs, IS_LAST_BLK(blk));
    ASSERT(tcs, IS_FREE_BLK(blk));

    STOP_ALC((Allctr_t *) a);
    tcs->extra = NULL;

}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    if (tcs->extra)
	STOP_ALC((Allctr_t *) tcs->extra);
}

ERL_NIF_INIT(basic, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
