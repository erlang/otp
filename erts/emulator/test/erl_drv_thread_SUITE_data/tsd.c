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
#include <stdio.h>

#define NO_OF_THREADS 17
#define NO_OF_KEYS 4711

struct {
    int alive;
    ErlDrvTid tid;
} test_thr[NO_OF_THREADS] = {0};

struct {
    int used;
    ErlDrvTSDKey key;
} test_key[NO_OF_KEYS] = {0};

typedef struct {
    int n;
} thr_arg_t;

static void *tf(void *vta)
{
    int i;
    int thr_val = (((thr_arg_t *) vta)->n << 16);
    
    for (i = 0; i < NO_OF_KEYS; i++)
	erl_drv_tsd_set(test_key[i].key, (void *) (thr_val | i));

    for (i = 0; i < NO_OF_KEYS; i++)
	if (erl_drv_tsd_get(test_key[i].key) != ((void *) (thr_val | i)))
	    return (void *) 1;

    for (i = 0; i < NO_OF_KEYS; i++)
	erl_drv_tsd_set(test_key[i].key, NULL);

    for (i = 0; i < NO_OF_KEYS; i++)
	if (erl_drv_tsd_get(test_key[i].key) != NULL)
	    return (void *) 2;

    return (void *) 3;
}

void
thr_key_clnup(void)
{
    int i;
    for (i = 0; i < NO_OF_KEYS; i++)
	erl_drv_tsd_set(test_key[i].key, NULL);
}

void
testcase_run(TestCaseState_t *tcs)
{
    int i, r;
    thr_arg_t ta[NO_OF_THREADS];
    ErlDrvSysInfo sinfo;

    testcase_printf(tcs, "Initializing\n");

    driver_system_info(&sinfo, sizeof(ErlDrvSysInfo));

    for (i = 0; i < NO_OF_KEYS; i++) {
	char name[100];
	sprintf(name, "key %d", i);
	r = erl_drv_tsd_key_create(name, &test_key[i].key);
	ASSERT(tcs, r == 0);
	test_key[i].used = 1;
    }

    for (i = 0; i < NO_OF_KEYS; i++)
	erl_drv_tsd_set(test_key[i].key,
			    (void *) (((NO_OF_THREADS+1) << 16) | i));

    if (!sinfo.thread_support) 
	testcase_printf(tcs, "No thread support; testing tsd in one thread\n");
    else {
	testcase_printf(tcs, "Creating %d threads\n", NO_OF_THREADS);

	/* Create the threads */
	for (i = 0; i < NO_OF_THREADS; i++) {
	    char name[100];
	    ta[i].n = 0;
	    sprintf(name, "thread %d", i);
	    r = erl_drv_thread_create(name,
				      &test_thr[i].tid,
				      tf,
				      (void *) &ta[i],
				      NULL);
	    ASSERT_CLNUP(tcs, r == 0, thr_key_clnup());
	    test_thr[i].alive = 1;
	}
    }

    testcase_printf(tcs, "Testing tsd\n");

    for (i = 0; i < NO_OF_KEYS; i++)
	ASSERT_CLNUP(tcs,
		     ((void *) (((NO_OF_THREADS+1) << 16) | i)
		      == erl_drv_tsd_get(test_key[i].key)),
		     thr_key_clnup());

    testcase_printf(tcs, "Joining threads\n");

    if (sinfo.thread_support) {
	/* Join the threads */
	for (i = 0; i < NO_OF_THREADS; i++) {
	    void *res;
	    r = erl_drv_thread_join(test_thr[i].tid, &res);
	    test_thr[i].alive = 0;
	    ASSERT_CLNUP(tcs, r == 0, thr_key_clnup());
	    ASSERT_CLNUP(tcs, res == ((void *) 3), thr_key_clnup());
	}
    }

    thr_key_clnup();

    for (i = 0; i < NO_OF_KEYS; i++)
	ASSERT(tcs, NULL == erl_drv_tsd_get(test_key[i].key));

    testcase_printf(tcs, "Destroying keys\n");

    for (i = 0; i < NO_OF_KEYS; i++)
	if (test_key[i].used) {
	    test_key[i].used = 0;
	    erl_drv_tsd_key_destroy(test_key[i].key);
	}

    testcase_printf(tcs, "done\n");

    if (!sinfo.thread_support)
	testcase_succeeded(tcs, "No thread support; only one thread tested");
}

char *
testcase_name(void)
{
    return "tsd";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    int i;
    for (i = 0; i < NO_OF_THREADS; i++)
	if (test_thr[i].alive) {
	    test_thr[i].alive = 0;
	    erl_drv_thread_join(test_thr[i].tid, NULL);
	}
    
    for (i = 0; i < NO_OF_KEYS; i++)
	if (test_key[i].used) {
	    test_key[i].used = 0;
	    erl_drv_tsd_key_destroy(test_key[i].key);
	}
}
