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

#ifdef __WIN32__
#include <windows.h>
#else
#include <unistd.h>
#endif
#include <errno.h>

#define NO_OF_THREADS 2

static int die;
static int cw_passed;
static int res_tf0;
static int res_tf1;
static ErlDrvMutex *mtx;
static ErlDrvCond *cnd;
static int need_join[NO_OF_THREADS];
static ErlDrvTid tid[NO_OF_THREADS];
static ErlDrvThreadOpts *topts;

typedef struct {
    int n;
} thr_arg_t;

static void
do_sleep(unsigned secs)
{
#ifdef __WIN32__
	Sleep((DWORD) secs*1000);
#else
	sleep(secs);
#endif
}

static void *tf0(void *vta)
{
    if (((thr_arg_t *) vta)->n == 0) {

	erl_drv_mutex_lock(mtx);

	erl_drv_cond_wait(cnd, mtx);

	if (die) {
	    erl_drv_mutex_unlock(mtx);
	    return NULL;
	}

	cw_passed++;

	erl_drv_cond_wait(cnd, mtx);

	if (die) {
	    erl_drv_mutex_unlock(mtx);
	    return NULL;
	}

	cw_passed++;

	erl_drv_mutex_unlock(mtx);
	if (erl_drv_equal_tids(erl_drv_thread_self(), tid[0]))
	    res_tf0 = 0;
    }

    return (void *) &res_tf0;
}


static void *tf1(void *vta)
{

    if (((thr_arg_t *) vta)->n == 1) {

	erl_drv_mutex_lock(mtx);

	erl_drv_cond_wait(cnd, mtx);

	if (die) {
	    erl_drv_mutex_unlock(mtx);
	    return NULL;
	}

	cw_passed++;

	erl_drv_cond_wait(cnd, mtx);

	if (die) {
	    erl_drv_mutex_unlock(mtx);
	    return NULL;
	}

	cw_passed++;

	erl_drv_mutex_unlock(mtx);

	if (erl_drv_equal_tids(erl_drv_thread_self(), tid[1]))
	    res_tf1 = 1;

	erl_drv_thread_exit((void *) &res_tf1);

	res_tf1 = 4711;
    }
    return NULL;
}

void
testcase_run(TestCaseState_t *tcs)
{
    int i, r;
    void *tres[2];
    thr_arg_t ta[2];
    ErlDrvTid my_tid;
    ErlDrvSysInfo sinfo;

    driver_system_info(&sinfo, sizeof(ErlDrvSysInfo));
    if (!sinfo.thread_support)
	testcase_skipped(tcs, "No thread support; nothing to test");

    testcase_printf(tcs, "Initializing\n");

    cw_passed = 0;
    die = 0;
    my_tid = erl_drv_thread_self();

    for (i = 0; i < NO_OF_THREADS; i++)
	need_join[i] = 0;

    res_tf0 = 17;
    res_tf1 = 17;

    mtx = NULL;
    cnd = NULL;
    /* Create mutex and cond */
    mtx = erl_drv_mutex_create("mutex");
    ASSERT(tcs, mtx);
    cnd = erl_drv_cond_create("cond");
    ASSERT(tcs, cnd);
    topts = erl_drv_thread_opts_create("thread opts");
    ASSERT(tcs, topts);
    topts->suggested_stack_size = 0; /* As small as possible */

    testcase_printf(tcs, "Creating threads 0 & 1\n");

    /* Create the threads */
    ta[0].n = 0;
    r = erl_drv_thread_create("thread 0", &tid[0], tf0, (void *) &ta[0], topts);
    ASSERT(tcs, r == 0);
    need_join[0] = 1;

    ta[1].n = 1;
    r = erl_drv_thread_create("thread 1", &tid[1], tf1, (void *) &ta[1], NULL);
    ASSERT(tcs, r == 0);
    need_join[1] = 1;

    testcase_printf(tcs, "Testing tids\n");

    ASSERT(tcs, !erl_drv_equal_tids(tid[0], my_tid));
    ASSERT(tcs, !erl_drv_equal_tids(tid[1], my_tid));
    ASSERT(tcs, !erl_drv_equal_tids(tid[0], tid[1]));
    ASSERT(tcs, erl_drv_equal_tids(my_tid, erl_drv_thread_self()));

    testcase_printf(tcs, "Testing mutex/cond\n");

    /* Make sure the threads waits on cond wait */
    do_sleep(1);

    erl_drv_mutex_lock(mtx);

    ASSERT_CLNUP(tcs, cw_passed == 0, erl_drv_mutex_unlock(mtx));

    /* Let one thread pass one cond wait */
    erl_drv_cond_signal(cnd);

    erl_drv_mutex_unlock(mtx);

    do_sleep(1);

    erl_drv_mutex_lock(mtx);

    ASSERT_CLNUP(tcs, cw_passed == 1, erl_drv_mutex_unlock(mtx));


    /* Let both threads pass one cond wait */
    erl_drv_cond_broadcast(cnd);

    erl_drv_mutex_unlock(mtx);

    do_sleep(1);

    erl_drv_mutex_lock(mtx);

    ASSERT_CLNUP(tcs, cw_passed == 3, erl_drv_mutex_unlock(mtx));


    /* Let the thread that only have passed one cond wait pass the other one */
    erl_drv_cond_signal(cnd);

    erl_drv_mutex_unlock(mtx);

    do_sleep(1);

    erl_drv_mutex_lock(mtx);

    ASSERT_CLNUP(tcs, cw_passed == 4, erl_drv_mutex_unlock(mtx));


    testcase_printf(tcs, "Testing join\n");

    /* Both threads should have passed both cond waits and exited;
       join them and check returned values */

    erl_drv_thread_join(tid[0], &tres[0]);
    ASSERT_CLNUP(tcs, r == 0, erl_drv_mutex_unlock(mtx));
    need_join[0] = 0;

    ASSERT_CLNUP(tcs, tres[0] == &res_tf0, erl_drv_mutex_unlock(mtx));
    ASSERT_CLNUP(tcs, res_tf0 == 0, erl_drv_mutex_unlock(mtx));

    r = erl_drv_thread_join(tid[1], &tres[1]);
    ASSERT_CLNUP(tcs, r == 0, erl_drv_mutex_unlock(mtx));
    need_join[1] = 0;

    ASSERT_CLNUP(tcs, tres[1] == &res_tf1, erl_drv_mutex_unlock(mtx));
    ASSERT_CLNUP(tcs, res_tf1 == 1, erl_drv_mutex_unlock(mtx));

    /* Test signaling when noone waits */

    erl_drv_cond_signal(cnd);

    /* Test broadcasting when noone waits */

    erl_drv_cond_broadcast(cnd);

    erl_drv_mutex_unlock(mtx);

    erl_drv_mutex_destroy(mtx);
    mtx = NULL;

    erl_drv_cond_destroy(cnd);
    cnd = NULL;

    erl_drv_thread_opts_destroy(topts);
    topts = NULL;

    testcase_printf(tcs, "done\n");
}

char *
testcase_name(void)
{
    return "basic";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    int i;
    for (i = 0; i < NO_OF_THREADS; i++) {
	if (need_join[i]) {
	    erl_drv_mutex_lock(mtx);
	    die = 1;
	    erl_drv_cond_broadcast(cnd);
	    erl_drv_mutex_unlock(mtx);
	    erl_drv_thread_join(tid[i], NULL);
	}
    }
    if (mtx)
	erl_drv_mutex_destroy(mtx);
    if (cnd)
	erl_drv_cond_destroy(cnd);
    if (topts)
	erl_drv_thread_opts_destroy(topts);
}
