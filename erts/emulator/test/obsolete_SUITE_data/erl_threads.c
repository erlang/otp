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

#ifndef __WIN32__

#define NO_OF_THREADS 2

#include <unistd.h>
#include <errno.h>

static int die;
static int cw_passed;
static int res_tf0;
static int res_tf1;
static erl_mutex_t mtx;
static erl_cond_t cnd;
static erl_thread_t tid[NO_OF_THREADS];
static int need_join[NO_OF_THREADS];

typedef struct {
    int n;
} thr_arg_t;


static void *tf0(void *vta)
{
    int r;

    if (((thr_arg_t *) vta)->n != 0)
	goto fail;

    r = erts_mutex_lock(mtx);
    if (r != 0) {
	erts_mutex_unlock(mtx);
	goto fail;
    }

    r = erts_cond_wait(cnd, mtx);
    if (r != 0 || die) {
	erts_mutex_unlock(mtx);
	goto fail;
    }

    cw_passed++;

    r = erts_cond_wait(cnd, mtx);
    if (r != 0 || die) {
	erts_mutex_unlock(mtx);
	goto fail;
    }

    cw_passed++;

    r = erts_mutex_unlock(mtx);
    if (r != 0)
	goto fail;

    res_tf0 = 0;

    return (void *) &res_tf0;

 fail:
    return NULL;
}


static void *tf1(void *vta)
{
    int r;

    if (((thr_arg_t *) vta)->n != 1)
	goto fail;

    r = erts_mutex_lock(mtx);
    if (r != 0) {
	erts_mutex_unlock(mtx);
	goto fail;
    }

    r = erts_cond_wait(cnd, mtx);
    if (r != 0 || die) {
	erts_mutex_unlock(mtx);
	goto fail;
    }

    cw_passed++;

    r = erts_cond_wait(cnd, mtx);
    if (r != 0 || die) {
	erts_mutex_unlock(mtx);
	goto fail;
    }

    cw_passed++;

    r = erts_mutex_unlock(mtx);
    if (r != 0)
	goto fail;

    res_tf1 = 1;

    erts_thread_exit((void *) &res_tf1);

    res_tf1 = 4711;

 fail:
    return NULL;
}

#endif /* #ifndef __WIN32__ */

void
testcase_run(TestCaseState_t *tcs)
{
#ifdef __WIN32__
    testcase_skipped(tcs, "Nothing to test; not supported on windows.");
#else
    int i, r;
    void *tres[NO_OF_THREADS];
    thr_arg_t ta[NO_OF_THREADS];
    erl_thread_t t1;

    die = 0;
    cw_passed = 0;

    for (i = 0; i < NO_OF_THREADS; i++)
	need_join[i] = 0;

    res_tf0 = 17;
    res_tf1 = 17;

    cnd = mtx = NULL;

    /* Create mutex and cond */
    mtx = erts_mutex_create();
    ASSERT(tcs, mtx);
    cnd = erts_cond_create();
    ASSERT(tcs, cnd);

    /* Create the threads */
    ta[0].n = 0;
    r = erts_thread_create(&tid[0], tf0, (void *) &ta[0], 0);
    ASSERT(tcs, r == 0);
    need_join[0] = 1;

    ta[1].n = 1;
    r = erts_thread_create(&tid[1], tf1, (void *) &ta[1], 0);
    ASSERT(tcs, r == 0);
    need_join[1] = 1;

    /* Make sure the threads waits on cond wait */
    sleep(1);

    r = erts_mutex_lock(mtx);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    ASSERT_CLNUP(tcs, cw_passed == 0, (void) erts_mutex_unlock(mtx));


    /* Let one thread pass one cond wait */
    r = erts_cond_signal(cnd);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    r = erts_mutex_unlock(mtx);
    ASSERT(tcs, r == 0);

    sleep(1);

    r = erts_mutex_lock(mtx);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    ASSERT_CLNUP(tcs, cw_passed == 1, (void) erts_mutex_unlock(mtx));


    /* Let both threads pass one cond wait */
    r = erts_cond_broadcast(cnd);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    r = erts_mutex_unlock(mtx);
    ASSERT(tcs, r == 0);

    sleep(1);

    r = erts_mutex_lock(mtx);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    ASSERT_CLNUP(tcs, cw_passed == 3, (void) erts_mutex_unlock(mtx));


    /* Let the thread that only have passed one cond wait pass the other one */
    r = erts_cond_signal(cnd);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    r = erts_mutex_unlock(mtx);
    ASSERT(tcs, r == 0);

    sleep(1);

    r = erts_mutex_lock(mtx);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    ASSERT_CLNUP(tcs, cw_passed == 4, (void) erts_mutex_unlock(mtx));

    /* Both threads should have passed both cond waits and exited;
       join them and check returned values */

    r = erts_thread_join(tid[0], &tres[0]);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));
    need_join[0] = 0;

    ASSERT_CLNUP(tcs, tres[0] == &res_tf0, (void) erts_mutex_unlock(mtx));
    ASSERT_CLNUP(tcs, res_tf0 == 0, (void) erts_mutex_unlock(mtx));

    r = erts_thread_join(tid[1], &tres[1]);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));
    need_join[1] = 0;

    ASSERT_CLNUP(tcs, tres[1] == &res_tf1, (void) erts_mutex_unlock(mtx));
    ASSERT_CLNUP(tcs, res_tf1 == 1, (void) erts_mutex_unlock(mtx));

    /* Test signaling when noone waits */

    r = erts_cond_signal(cnd);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    /* Test broadcasting when noone waits */

    r = erts_cond_broadcast(cnd);
    ASSERT_CLNUP(tcs, r == 0, (void) erts_mutex_unlock(mtx));

    /* erts_cond_timedwait() not supported anymore */
    r = erts_cond_timedwait(cnd, mtx, 1000);
    ASSERT_CLNUP(tcs, r != 0, (void) erts_mutex_unlock(mtx));
    ASSERT_CLNUP(tcs,
		 strcmp(erl_errno_id(r), "enotsup") == 0,
		 (void) erts_mutex_unlock(mtx));

    r = erts_mutex_unlock(mtx);
    ASSERT(tcs, r == 0);

    r = erts_mutex_destroy(mtx);
    ASSERT(tcs, r == 0);
    mtx = NULL;

    r = erts_cond_destroy(cnd);
    ASSERT(tcs, r == 0);
    cnd = NULL;

    /* ... */
    t1 = erts_thread_self();

    if (cw_passed == 4711) {
	/* We don't want to execute this just check that the
	   symbol/symbols is/are defined */
	erts_thread_kill(t1);
    }

#endif /* #ifndef __WIN32__ */
}

char *
testcase_name(void)
{
    return "erl_threads";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    int i;
    for (i = 0; i < NO_OF_THREADS; i++) {
	if (need_join[i]) {
	    erts_mutex_lock(mtx);
	    die = 1;
	    erts_cond_broadcast(cnd);
	    erts_mutex_unlock(mtx);
	    erts_thread_join(tid[1], NULL);
	}
    }
    if (mtx)
	erts_mutex_destroy(mtx);
    if (cnd)
	erts_cond_destroy(cnd);
}

