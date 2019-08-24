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

#define NO_OF_THREADS 17

struct {
    int alive;
    ErlDrvTid tid;
} test_thr[NO_OF_THREADS] = {0};


static int die;
static int ready;
static int rlocked;
static int rwlocked;
static int do_rlock;
static int do_rwlock;
static ErlDrvMutex *mtx;
static ErlDrvCond *cnd;
static ErlDrvRWLock *rwlck;

static void
do_sleep(unsigned secs)
{
#ifdef __WIN32__
    Sleep((DWORD) secs*1000);
#else
    sleep(secs);
#endif
}

static void *tf(void *unused)
{

    erl_drv_mutex_lock(mtx);
    ready++;
    if (ready == NO_OF_THREADS)
	erl_drv_cond_broadcast(cnd);
    while (!do_rlock)
	erl_drv_cond_wait(cnd, mtx);
    erl_drv_mutex_unlock(mtx);

    erl_drv_rwlock_rlock(rwlck);

    /* make sure everyone rlocks at the same time */
    erl_drv_mutex_lock(mtx);
    rlocked++;
    if (rlocked == NO_OF_THREADS)
	erl_drv_cond_broadcast(cnd);
    while (rlocked != NO_OF_THREADS && !die)
	erl_drv_cond_wait(cnd, mtx);
    erl_drv_mutex_unlock(mtx);

    erl_drv_rwlock_runlock(rwlck);

    erl_drv_mutex_lock(mtx);
    while (!do_rwlock && !die)
	erl_drv_cond_wait(cnd, mtx);
    if (die) {
	erl_drv_mutex_unlock(mtx);
	return NULL;
    }
    erl_drv_mutex_unlock(mtx);

    erl_drv_rwlock_rwlock(rwlck);
    rwlocked++;
    erl_drv_rwlock_rwunlock(rwlck);

    return NULL;
}

void
testcase_run(TestCaseState_t *tcs)
{
    int i, r;
    ErlDrvSysInfo sinfo;

    driver_system_info(&sinfo, sizeof(ErlDrvSysInfo));
    if (!sinfo.thread_support)
	testcase_skipped(tcs, "No thread support; nothing to test");

    testcase_printf(tcs, "Initializing\n");
    die = 0;
    ready = 0;
    rlocked = 0;
    rwlocked = 0;
    do_rlock = 0;
    do_rwlock = 0;

    mtx = erl_drv_mutex_create("test mutex");
    cnd = erl_drv_cond_create("test cond");
    rwlck = erl_drv_rwlock_create("test rwlock");
    ASSERT(tcs, mtx && cnd && rwlck);

    testcase_printf(tcs, "Creating %d threads\n", NO_OF_THREADS);
    /* Create the threads */
    for (i = 0; i < NO_OF_THREADS; i++) {
	char name[100];
	sprintf(name, "thread %d", i);
	r = erl_drv_thread_create(name,
				  &test_thr[i].tid,
				  tf,
				  NULL,
				  NULL);
	ASSERT(tcs, r == 0);
	test_thr[i].alive = 1;
    }

    testcase_printf(tcs, "Testing\n");
    erl_drv_rwlock_rwlock(rwlck);
    
    erl_drv_mutex_lock(mtx);
    while (ready != NO_OF_THREADS)
	erl_drv_cond_wait(cnd, mtx);
    do_rlock = 1;
    erl_drv_cond_broadcast(cnd);
    erl_drv_mutex_unlock(mtx);

    do_sleep(1);

    erl_drv_mutex_lock(mtx);

    ASSERT_CLNUP(tcs,
		 rlocked == 0,
		 do {
		     erl_drv_mutex_unlock(mtx);
		     erl_drv_rwlock_rwunlock(rwlck);
		 } while (0));

    erl_drv_mutex_unlock(mtx);
    erl_drv_rwlock_rwunlock(rwlck);

    do_sleep(2);

    erl_drv_mutex_lock(mtx);
    ASSERT_CLNUP(tcs, rlocked == NO_OF_THREADS, erl_drv_mutex_unlock(mtx));
    do_rwlock = 1;
    erl_drv_cond_broadcast(cnd);
    erl_drv_mutex_unlock(mtx);
    
    testcase_printf(tcs, "Joining threads\n");
    /* Join the threads */
    for (i = 0; i < NO_OF_THREADS; i++) {
	void *res;
	r = erl_drv_thread_join(test_thr[i].tid, NULL);
	test_thr[i].alive = 0;
	ASSERT(tcs, r == 0);
    }

    erl_drv_mutex_lock(mtx);
    ASSERT_CLNUP(tcs, rwlocked == NO_OF_THREADS, erl_drv_mutex_unlock(mtx));
    erl_drv_mutex_unlock(mtx);

    erl_drv_mutex_destroy(mtx);
    mtx = NULL;
    erl_drv_cond_destroy(cnd);
    cnd = NULL;
    erl_drv_rwlock_destroy(rwlck);
    rwlck = NULL;

    testcase_printf(tcs, "done\n");
}

char *
testcase_name(void)
{
    return "rwlock";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    int i;
    for (i = 0; i < NO_OF_THREADS; i++) {
	if (test_thr[i].alive) {
	    erl_drv_mutex_lock(mtx);
	    die = 1;
	    erl_drv_cond_broadcast(cnd);
	    erl_drv_mutex_unlock(mtx);
	    erl_drv_thread_join(test_thr[i].tid, NULL);
	}
    }

    if (mtx)
	erl_drv_mutex_destroy(mtx);
    if (cnd)
	erl_drv_cond_destroy(cnd);
    if (rwlck)
	erl_drv_rwlock_destroy(rwlck);
}
