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
    blk = MBC2FBLK(a, c);
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
