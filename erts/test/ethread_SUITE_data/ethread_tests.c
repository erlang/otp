/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
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
 * %CopyrightEnd%
 */

/*
 * Description: Test suite for the ethread thread library.
 * Author: Rickard Green
 */

#define ETHR_NO_SUPP_THR_LIB_NOT_FATAL
#include "ethread.h"
#include "erl_misc_utils.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifndef __WIN32__
#include <unistd.h>
#endif
#include <limits.h>

/*
 * Auxiliary functions
 */

#define PRINT_VA_LIST(FRMT)						\
do {									\
    if (FRMT && FRMT != '\0') {						\
	va_list args;							\
	va_start(args, FRMT);						\
	vfprintf(stderr, FRMT, args);					\
	va_end(args);							\
    }									\
} while (0)

#define ASSERT(B)							\
do {									\
    if (!(B))								\
	fail("%s:%d: Assertion \"%s\" failed!",__FILE__,__LINE__,#B);	\
} while (0)


#define ASSERT_PRINT(B, PRNT)						\
do {									\
    if (!(B)) {								\
	print PRNT;							\
	fail("%s:%d: Assertion \"%s\" failed!",__FILE__,__LINE__,#B);	\
    }									\
} while (0)

#define ASSERT_EQ(VAR, VAL, FSTR)					\
do {									\
    if ((VAR) != (VAL)) {						\
	print("%s=" FSTR "\n", #VAR, (VAR));				\
	fail("%s:%d: Assertion \"%s == " FSTR "\" failed!",		\
	     __FILE__, __LINE__, #VAR, (VAL));				\
    }									\
} while (0)

#ifdef __WIN32_
#define EOL "\r\n"
#else
#define EOL "\n"
#endif

static void
print_eol(void)
{
    fprintf(stderr, EOL);
    fflush(stderr);
}

static void print_line(char *frmt,...)
{
    PRINT_VA_LIST(frmt);
    print_eol();
}

#if 0 /* Currently not used; silence annoying warning... */
static void print(char *frmt,...)
{
    PRINT_VA_LIST(frmt);
}
#endif

static void fail(char *frmt,...)
{
    char *abrt_env;
    print_eol();
    fprintf(stderr, "ETHR-TEST-FAILURE");
    PRINT_VA_LIST(frmt);
    print_eol();
    abrt_env = getenv("ERL_ABORT_ON_FAILURE");
    if (abrt_env && strcmp("true", abrt_env) == 0)
	abort();
    else
	exit(1);
}

static void skip(char *frmt,...)
{
    print_eol();
    fprintf(stderr, "ETHR-TEST-SKIP");
    PRINT_VA_LIST(frmt);
    print_eol();
    exit(0);
}

static void succeed(char *frmt,...)
{
    print_eol();
    fprintf(stderr, "ETHR-TEST-SUCCESS");
    PRINT_VA_LIST(frmt);
    print_eol();
    exit(0);
}

static void
do_sleep(unsigned secs)
{
    while (erts_milli_sleep(secs*1000) != 0);
}

#define WAIT_UNTIL_INTERVAL 10

#define WAIT_UNTIL_LIM(TEST, LIM)				\
do {								\
    int ms__ = (LIM)*1000;					\
    while (!(TEST)) {						\
	while (erts_milli_sleep(WAIT_UNTIL_INTERVAL) != 0);	\
	ms__ -= WAIT_UNTIL_INTERVAL;				\
	if (ms__ <= 0)						\
	    break;						\
    }								\
} while (0)

static void
send_my_pid(void)
{
#ifndef __WIN32__
    int pid = (int) getpid();
    fprintf(stderr, EOL "ETHR-TEST-PID%d" EOL, pid);
#endif
}

/*
 * The test-cases
 */

#ifndef ETHR_NO_THREAD_LIB

/*
 * The create join thread test case.
 *
 * Tests ethr_thr_create and ethr_thr_join.
 */

#define CJTT_NO_THREADS 64
ethr_tid cjtt_tids[CJTT_NO_THREADS + 1];
int cjtt_ix[CJTT_NO_THREADS + 1];
int cjtt_res[CJTT_NO_THREADS + 1];
void *cjtt_thread(void *vpix)
{
    int ix = *((int *) vpix);
    cjtt_res[ix] = ix;
    return (void *) &cjtt_res[ix]; 
}

static void
create_join_thread_test(void)
{
    int i, res;

    for (i = 1; i <= CJTT_NO_THREADS; i++) {
	cjtt_ix[i] = i;
	cjtt_res[i] = 0;
    }

    for (i = 1; i <= CJTT_NO_THREADS; i++) {
	res = ethr_thr_create(&cjtt_tids[i],
			      cjtt_thread,
			      (void *) &cjtt_ix[i],
			      NULL);
	ASSERT(res == 0);
    }

    for (i = 1; i <= CJTT_NO_THREADS; i++) {
	void *tres;
	res = ethr_thr_join(cjtt_tids[i], &tres);
	ASSERT(res == 0);
	ASSERT(tres == &cjtt_res[i]);
	ASSERT(cjtt_res[i] == i);
    }

}


/*
 * The eq tid test case.
 *
 * Tests ethr_equal_tids.
 */

#define ETT_THREADS 100000

static ethr_tid ett_tids[3];
static ethr_mutex ett_mutex;
static ethr_cond ett_cond;
static int ett_terminate;

static void *
ett_thread(void *my_tid)
{

    ASSERT(!ethr_equal_tids(ethr_self(), ett_tids[0]));
    ASSERT(ethr_equal_tids(ethr_self(), *((ethr_tid *) my_tid)));

    return NULL;
}

static void *
ett_thread2(void *unused)
{
    int res;
    ethr_mutex_lock(&ett_mutex);
    while (!ett_terminate) {
	res = ethr_cond_wait(&ett_cond, &ett_mutex);
	ASSERT(res == 0);
    }
    ethr_mutex_unlock(&ett_mutex);
    return NULL;
}

static void
equal_tids_test(void)
{
    int res, i;

    res = ethr_mutex_init(&ett_mutex);
    ASSERT(res == 0);
    res = ethr_cond_init(&ett_cond);
    ASSERT(res == 0);
    ett_tids[0] = ethr_self();
    
    res = ethr_thr_create(&ett_tids[1], ett_thread, (void *) &ett_tids[1], NULL);
    ASSERT(res == 0);

    ASSERT(ethr_equal_tids(ethr_self(), ett_tids[0]));
    ASSERT(!ethr_equal_tids(ethr_self(), ett_tids[1]));

    res = ethr_thr_join(ett_tids[1], NULL);

    res = ethr_thr_create(&ett_tids[2], ett_thread, (void *) &ett_tids[2], NULL);
    ASSERT(res == 0);

    ASSERT(ethr_equal_tids(ethr_self(), ett_tids[0]));
    ASSERT(!ethr_equal_tids(ethr_self(), ett_tids[1]));
    ASSERT(!ethr_equal_tids(ethr_self(), ett_tids[2]));

#if 0
    /* This fails on some linux platforms. Until we decides if a tid
     * is allowed to be reused right away or not, we disable the test.
     */

    ASSERT(!ethr_equal_tids(ett_tids[1], ett_tids[2]));
#endif

    res = ethr_thr_join(ett_tids[2], NULL);
    ASSERT(res == 0);

    /* Second part of test */

    ett_terminate = 0;

    res = ethr_thr_create(&ett_tids[1], ett_thread2, NULL, NULL);
    ASSERT(res == 0);

    ASSERT(!ethr_equal_tids(ett_tids[0], ett_tids[1]));

    for (i = 0; i < ETT_THREADS; i++) {
	res = ethr_thr_create(&ett_tids[2], ett_thread, (void*)&ett_tids[2], NULL);
	ASSERT(res == 0);

	ASSERT(!ethr_equal_tids(ett_tids[0], ett_tids[2]));
	ASSERT(!ethr_equal_tids(ett_tids[1], ett_tids[2]));

	res = ethr_thr_join(ett_tids[2], NULL);
	ASSERT(res == 0);
    }

    ethr_mutex_lock(&ett_mutex);
    ett_terminate = 1;
    ethr_cond_signal(&ett_cond);
    ethr_mutex_unlock(&ett_mutex);
    res = ethr_thr_join(ett_tids[1], NULL);
    ASSERT(res == 0);

    res = ethr_cond_destroy(&ett_cond);
    ASSERT(res == 0);
    res = ethr_mutex_destroy(&ett_mutex);
    ASSERT(res == 0);

}

/*
 * The mutex test case.
 *
 * Tests mutexes.
 */

static ethr_mutex mt_mutex;
static int mt_data;

void *
mt_thread(void *unused)
{
    print_line("Aux thread tries to lock mutex");
    ethr_mutex_lock(&mt_mutex);
    print_line("Aux thread locked mutex");

    ASSERT(mt_data == 0);

    mt_data = 1;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(mt_data == 1);

    ethr_mutex_unlock(&mt_mutex);
    print_line("Aux thread unlocked mutex");

    return NULL;
}


static void
mutex_test(void)
{
    int res;
    ethr_tid tid;

    print_line("Trying to initialize mutex");
    res = ethr_mutex_init(&mt_mutex);
    ASSERT(res == 0);
    print_line("Initialized mutex");

    mt_data = 0;

    print_line("Main thread tries to lock mutex");
    ethr_mutex_lock(&mt_mutex);
    print_line("Main thread locked mutex");

    ASSERT(mt_data == 0);

    print_line("Main thread about to create aux thread");
    res = ethr_thr_create(&tid, mt_thread, NULL, NULL);
    ASSERT(res == 0);
    print_line("Main thread created aux thread");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(mt_data == 0);

    ethr_mutex_unlock(&mt_mutex);
    print_line("Main thread unlocked mutex");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    print_line("Main thread tries to lock mutex");
    ethr_mutex_lock(&mt_mutex);
    print_line("Main thread locked mutex");

    ASSERT(mt_data == 1);

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(mt_data == 1);

    ethr_mutex_unlock(&mt_mutex);
    print_line("Main thread unlocked mutex");

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);
    print_line("Main thread joined aux thread");

    res = ethr_mutex_destroy(&mt_mutex);
    ASSERT(res == 0);
    print_line("Main thread destroyed mutex");

}

/*
 * The try lock mutex test case.
 *
 * Tests try lock mutex operation.
 */

static ethr_mutex tlmt_mtx1;
static ethr_mutex tlmt_mtx2;
static ethr_cond tlmt_cnd2;

static int tlmt_mtx1_locked;
static int tlmt_mtx1_do_unlock;

static void *
tlmt_thread(void *unused)
{
    int res;

    ethr_mutex_lock(&tlmt_mtx1);
    ethr_mutex_lock(&tlmt_mtx2);

    tlmt_mtx1_locked = 1;
    ethr_cond_signal(&tlmt_cnd2);

    while (!tlmt_mtx1_do_unlock) {
	res = ethr_cond_wait(&tlmt_cnd2, &tlmt_mtx2);
	ASSERT(res == 0 || res == EINTR);
    }

    ethr_mutex_unlock(&tlmt_mtx2);
    ethr_mutex_unlock(&tlmt_mtx1);

    ethr_mutex_lock(&tlmt_mtx2);
    tlmt_mtx1_locked = 0;
    ethr_cond_signal(&tlmt_cnd2);
    ethr_mutex_unlock(&tlmt_mtx2);

    return NULL;
}

static void
try_lock_mutex_test(void)
{
    int i, res;
    ethr_tid tid;

    res = ethr_mutex_init(&tlmt_mtx1);
    ASSERT(res == 0);
    res = ethr_mutex_init(&tlmt_mtx2);
    ASSERT(res == 0);
    res = ethr_cond_init(&tlmt_cnd2);
    ASSERT(res == 0);

    tlmt_mtx1_locked = 0;
    tlmt_mtx1_do_unlock = 0;

    res = ethr_thr_create(&tid, tlmt_thread, NULL, NULL);
    ASSERT(res == 0);

    ethr_mutex_lock(&tlmt_mtx2);

    while (!tlmt_mtx1_locked) {
	res = ethr_cond_wait(&tlmt_cnd2, &tlmt_mtx2);
	ASSERT(res == 0 || res == EINTR);
    }

    ethr_mutex_unlock(&tlmt_mtx2);

    for (i = 0; i < 10; i++) {
	res = ethr_mutex_trylock(&tlmt_mtx1);
	ASSERT(res == EBUSY);
    }

    ethr_mutex_lock(&tlmt_mtx2);

    tlmt_mtx1_do_unlock = 1;
    ethr_cond_signal(&tlmt_cnd2);

    while (tlmt_mtx1_locked) {
	res = ethr_cond_wait(&tlmt_cnd2, &tlmt_mtx2);
	ASSERT(res == 0 || res == EINTR);
    }

    ethr_mutex_unlock(&tlmt_mtx2);

    res = ethr_mutex_trylock(&tlmt_mtx1);
    ASSERT(res == 0);

    ethr_mutex_unlock(&tlmt_mtx1);

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);

    res = ethr_mutex_destroy(&tlmt_mtx1);
    ASSERT(res == 0);
    res = ethr_mutex_destroy(&tlmt_mtx2);
    ASSERT(res == 0);
    res = ethr_cond_destroy(&tlmt_cnd2);
    ASSERT(res == 0);
}

/*
 * The cond wait test case.
 *
 * Tests ethr_cond_wait with ethr_cond_signal and ethr_cond_broadcast.
 */


static ethr_mutex cwt_mutex;
static ethr_cond cwt_cond;
static int cwt_counter;

void *
cwt_thread(void *unused)
{
    int res;

    ethr_mutex_lock(&cwt_mutex);

    do {
	res = ethr_cond_wait(&cwt_cond, &cwt_mutex);
    } while (res == EINTR);
    ASSERT(res == 0);

    cwt_counter++;

    ethr_mutex_unlock(&cwt_mutex);

    return NULL;
}

static void
cond_wait_test(void)
{
    ethr_tid tid1, tid2;
    int res;

    res = ethr_mutex_init(&cwt_mutex);
    ASSERT(res == 0);
    res = ethr_cond_init(&cwt_cond);
    ASSERT(res == 0);

    /* Wake with signal */

    cwt_counter = 0;

    res = ethr_thr_create(&tid1, cwt_thread, NULL, NULL);
    ASSERT(res == 0);
    res = ethr_thr_create(&tid2, cwt_thread, NULL, NULL);
    ASSERT(res == 0);

    do_sleep(1); /* Make sure threads waits on cond var */

    ethr_mutex_lock(&cwt_mutex);

    ethr_cond_signal(&cwt_cond); /* Wake one thread */

    do_sleep(1); /* Make sure awakened thread waits on mutex */

    ASSERT(cwt_counter == 0);

    ethr_mutex_unlock(&cwt_mutex);

    do_sleep(1);  /* Let awakened thread proceed */

    ethr_mutex_lock(&cwt_mutex);

    ASSERT(cwt_counter == 1);

    ethr_cond_signal(&cwt_cond); /* Wake the other thread */

    do_sleep(1); /* Make sure awakened thread waits on mutex */

    ASSERT(cwt_counter == 1);

    ethr_mutex_unlock(&cwt_mutex);

    do_sleep(1);  /* Let awakened thread proceed */

    ethr_mutex_lock(&cwt_mutex);

    ASSERT(cwt_counter == 2);

    ethr_mutex_unlock(&cwt_mutex);

    res = ethr_thr_join(tid1, NULL);
    ASSERT(res == 0);

    res = ethr_thr_join(tid2, NULL);
    ASSERT(res == 0);


    /* Wake with broadcast */

    cwt_counter = 0;

    res = ethr_thr_create(&tid1, cwt_thread, NULL, NULL);
    ASSERT(res == 0);
    res = ethr_thr_create(&tid2, cwt_thread, NULL, NULL);
    ASSERT(res == 0);

    do_sleep(1); /* Make sure threads waits on cond var */

    ethr_mutex_lock(&cwt_mutex);

    ethr_cond_broadcast(&cwt_cond); /* Wake the threads */

    do_sleep(1); /* Make sure awakened threads wait on mutex */

    ASSERT(cwt_counter == 0);

    ethr_mutex_unlock(&cwt_mutex);

    do_sleep(1);  /* Let awakened threads proceed */

    ethr_mutex_lock(&cwt_mutex);

    ASSERT(cwt_counter == 2);

    ethr_mutex_unlock(&cwt_mutex);

    res = ethr_thr_join(tid1, NULL);
    ASSERT(res == 0);

    res = ethr_thr_join(tid2, NULL);
    ASSERT(res == 0);

    res = ethr_mutex_destroy(&cwt_mutex);
    ASSERT(res == 0);
    res = ethr_cond_destroy(&cwt_cond);
    ASSERT(res == 0);

}

/*
 * The broadcast test case.
 *
 * Tests that a ethr_cond_broadcast really wakes up all waiting threads.
 */

#define BCT_THREADS 64
#define BCT_NO_OF_WAITS 100

static int bct_woken = 0;
static int bct_waiting = 0;
static int bct_done = 0;
static ethr_mutex bct_mutex;
static ethr_cond bct_cond;
static ethr_cond bct_cntrl_cond;


static void *
bct_thread(void *unused)
{
    int res;

    ethr_mutex_lock(&bct_mutex);

    while (!bct_done) {

	bct_waiting++;
	if (bct_waiting == BCT_THREADS)
	    ethr_cond_signal(&bct_cntrl_cond);
	do {
	    res = ethr_cond_wait(&bct_cond, &bct_mutex);
	} while (res == EINTR);
	ASSERT(res == 0);
	bct_woken++;
	if (bct_woken == BCT_THREADS)
	    ethr_cond_signal(&bct_cntrl_cond);

    }

    ethr_mutex_unlock(&bct_mutex);

    return NULL;
}

static void
broadcast_test(void)
{
    int res, i;
    ethr_tid tid[BCT_THREADS];

    res = ethr_mutex_init(&bct_mutex);
    ASSERT(res == 0);
    res = ethr_cond_init(&bct_cntrl_cond);
    ASSERT(res == 0);
    res = ethr_cond_init(&bct_cond);
    ASSERT(res == 0);

    for (i = 0; i < BCT_THREADS; i++) {
	res = ethr_thr_create(&tid[i], bct_thread, NULL, NULL);
	ASSERT(res == 0);

    }

    ethr_mutex_lock(&bct_mutex);

    for (i = 0; i < BCT_NO_OF_WAITS; i++) {

	while (bct_waiting != BCT_THREADS) {
	    res = ethr_cond_wait(&bct_cntrl_cond, &bct_mutex);
	    ASSERT(res == 0 || res == EINTR);
	}

	bct_waiting = 0;
	bct_woken = 0;

	/* Wake all threads */
	ethr_cond_broadcast(&bct_cond);

	while (bct_woken != BCT_THREADS) {
	    res = ethr_cond_wait(&bct_cntrl_cond, &bct_mutex);
	    ASSERT(res == 0 || res == EINTR);
	}

    }

    bct_done = 1;

    /* Wake all threads */
    ethr_cond_broadcast(&bct_cond);

    ethr_mutex_unlock(&bct_mutex);

    for (i = 0; i < BCT_THREADS; i++) {
	res = ethr_thr_join(tid[i], NULL);
	ASSERT(res == 0);
    }

    res = ethr_mutex_destroy(&bct_mutex);
    ASSERT(res == 0);
    res = ethr_cond_destroy(&bct_cntrl_cond);
    ASSERT(res == 0);
    res = ethr_cond_destroy(&bct_cond);
    ASSERT(res == 0);

}

/*
 * The detached thread test case.
 *
 * Tests detached threads.
 */

#define DT_THREADS (50*1024)
#define DT_BATCH_SIZE 64

static ethr_mutex dt_mutex;
static ethr_cond dt_cond;
static int dt_count;
static int dt_limit;

static void *
dt_thread(void *unused)
{
    ethr_mutex_lock(&dt_mutex);

    dt_count++;

    if (dt_count >= dt_limit)
	ethr_cond_signal(&dt_cond);

    ethr_mutex_unlock(&dt_mutex);

    return NULL;
}

static void
detached_thread_test(void)
{
    ethr_thr_opts thr_opts = ETHR_THR_OPTS_DEFAULT_INITER;
    ethr_tid tid[DT_BATCH_SIZE];
    int i, j, res;

    res = ethr_mutex_init(&dt_mutex);
    ASSERT(res == 0);
    res = ethr_cond_init(&dt_cond);
    ASSERT(res == 0);

    thr_opts.detached = 1;
    dt_count = 0;
    dt_limit = 0;

    for (i = 0; i < DT_THREADS/DT_BATCH_SIZE; i++) {

	dt_limit += DT_BATCH_SIZE;

	for (j = 0; j < DT_BATCH_SIZE; j++) {
	    res = ethr_thr_create(&tid[j], dt_thread, NULL, &thr_opts);
	    ASSERT(res == 0);
	}

	ethr_mutex_lock(&dt_mutex);
	while (dt_count < dt_limit) {
	    res = ethr_cond_wait(&dt_cond, &dt_mutex);
	    ASSERT(res == 0 || res == EINTR);
	}
	ethr_mutex_unlock(&dt_mutex);

	print_line("dt_count = %d", dt_count);
    }
    do_sleep(1);
}



/*
 * The max threads test case.
 *
 * Tests
 */
#define MTT_TIMES 10
#define MTT_HARD_LIMIT (80000)

static int mtt_terminate;
static ethr_mutex mtt_mutex;
static ethr_cond mtt_cond;
static char mtt_string[22*MTT_TIMES]; /* 22 is enough for ", %d" */


void *mtt_thread(void *unused)
{
    int res;

    ethr_mutex_lock(&mtt_mutex);

    while (!mtt_terminate) {
	res = ethr_cond_wait(&mtt_cond, &mtt_mutex);
	ASSERT(res == 0 || res == EINTR);
    }

    ethr_mutex_unlock(&mtt_mutex);

    return NULL; 
}


static int
mtt_create_join_threads(void)
{
    int no_tids = 100, ix = 0, res = 0, no_threads;
    ethr_tid *tids;

    mtt_terminate = 0;

    tids = (ethr_tid *) malloc(sizeof(ethr_tid)*no_tids);
    ASSERT(tids);

    print_line("Beginning to create threads");

    while (1) {
	if (ix >= no_tids) {
	    no_tids += 100;
	    if (no_tids > MTT_HARD_LIMIT) {
		print_line("Hit the hard limit on number of threads (%d)!", 
			   MTT_HARD_LIMIT);
		break;
	    }
	    tids = (ethr_tid *) realloc((void *)tids, sizeof(ethr_tid)*no_tids);
	    ASSERT(tids);
	}
	res = ethr_thr_create(&tids[ix], mtt_thread, NULL, NULL);
	if (res != 0) {
	    break;
	}
	ix++;
    }

    no_threads = ix;

    print_line("%d = ethr_thr_create()", res);
    print_line("Number of created threads: %d", no_threads);

    ethr_mutex_lock(&mtt_mutex);

    mtt_terminate = 1;

    ethr_cond_broadcast(&mtt_cond);

    ethr_mutex_unlock(&mtt_mutex);

    while (ix) {
	res = ethr_thr_join(tids[--ix], NULL);
	ASSERT(res == 0);
    }

    print_line("All created threads terminated");

    free((void *) tids);

    return no_threads;

}

static void
max_threads_test(void)
{
    int no_threads[MTT_TIMES], i, up, down, eq, res;
    char *str;

    res = ethr_mutex_init(&mtt_mutex);
    ASSERT(res == 0);
    res = ethr_cond_init(&mtt_cond);
    ASSERT(res == 0);

    for (i = 0; i < MTT_TIMES; i++) {
	no_threads[i] = mtt_create_join_threads();
    }

    str = &mtt_string[0];
    eq = up = down = 0;
    for (i = 0; i < MTT_TIMES; i++) {
	if (i == 0) {
	    str += sprintf(str, "%d", no_threads[i]);
	    continue;
	}

	str += sprintf(str, ", %d", no_threads[i]);

	if (no_threads[i] < no_threads[i-1])
	    down++;
	else if (no_threads[i] > no_threads[i-1])
	    up++;
	else
	    eq++;
    }

    print_line("Max created threads: %s", mtt_string);

    /* We fail if the no of threads we are able to create constantly decrease */
    ASSERT(!down || up || eq);

    succeed("Max created threads: %s", mtt_string);

}

/*
 * The tsd test case.
 *
 * Tests thread specific data.
 */

#define TT_THREADS 10
static ethr_tsd_key tt_key;

static void *
tt_thread(void *arg)
{
    int res = ethr_tsd_set(tt_key, arg);
    ASSERT(res == 0);
    return ethr_tsd_get(tt_key);
}

static void
tsd_test(void)
{
    void *tres;
    int i, res;
    ethr_tid tid[TT_THREADS];
    int values[TT_THREADS];

    res = ethr_tsd_key_create(&tt_key,"tsd_test");
    ASSERT(res == 0);

    for (i = 1; i < TT_THREADS; i++) {
	res = ethr_thr_create(&tid[i], tt_thread, (void *) &values[i], NULL);
	ASSERT(res == 0);
    }

    tres = tt_thread((void *) &values[0]);
    ASSERT(tres == (void *) &values[0]);

    for (i = 1; i < TT_THREADS; i++) {
	res = ethr_thr_join(tid[i], &tres);
	ASSERT(res == 0);
	ASSERT(tres == (void *) &values[i]);
    }

    res = ethr_tsd_key_delete(tt_key);
    ASSERT(res == 0);
}


/*
 * The spinlock test case.
 *
 * Tests spinlocks.
 */

static ethr_spinlock_t st_spinlock;
static int st_data;

void *
st_thread(void *unused)
{
    print_line("Aux thread tries to lock spinlock");
    ethr_spin_lock(&st_spinlock);
    print_line("Aux thread locked spinlock");

    ASSERT(st_data == 0);

    st_data = 1;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(st_data == 1);

    ethr_spin_unlock(&st_spinlock);
    print_line("Aux thread unlocked spinlock");

    return NULL;
}


static void
spinlock_test(void)
{
    int res;
    ethr_tid tid;

    print_line("Trying to initialize spinlock");
    res = ethr_spinlock_init(&st_spinlock);
    ASSERT(res == 0);
    print_line("Initialized spinlock");

    st_data = 0;

    print_line("Main thread tries to lock spinlock");
    ethr_spin_lock(&st_spinlock);
    print_line("Main thread locked spinlock");

    ASSERT(st_data == 0);

    print_line("Main thread about to create aux thread");
    res = ethr_thr_create(&tid, st_thread, NULL, NULL);
    ASSERT(res == 0);
    print_line("Main thread created aux thread");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(st_data == 0);

    ethr_spin_unlock(&st_spinlock);
    print_line("Main thread unlocked spinlock");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    print_line("Main thread tries to lock spinlock");
    ethr_spin_lock(&st_spinlock);
    print_line("Main thread locked spinlock");

    ASSERT(st_data == 1);

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(st_data == 1);

    ethr_spin_unlock(&st_spinlock);
    print_line("Main thread unlocked spinlock");

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);
    print_line("Main thread joined aux thread");

    res = ethr_spinlock_destroy(&st_spinlock);
    ASSERT(res == 0);
    print_line("Main thread destroyed spinlock");

}


/*
 * The rwspinlock test case.
 *
 * Tests rwspinlocks.
 */

static ethr_rwlock_t rwst_rwspinlock;
static int rwst_data;

void *
rwst_thread(void *unused)
{
    int data;

    print_line("Aux thread tries to read lock rwspinlock");
    ethr_read_lock(&rwst_rwspinlock);
    print_line("Aux thread read locked rwspinlock");

    ASSERT(rwst_data == 4711);

    print_line("Aux thread tries to read unlock rwspinlock");
    ethr_read_unlock(&rwst_rwspinlock);
    print_line("Aux thread read unlocked rwspinlock");

    print_line("Aux thread tries to write lock rwspinlock");
    ethr_write_lock(&rwst_rwspinlock);
    print_line("Aux thread write locked rwspinlock");

    data = ++rwst_data;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(rwst_data == data);
    ++rwst_data;

    print_line("Aux thread tries to write unlock rwspinlock");
    ethr_write_unlock(&rwst_rwspinlock);
    print_line("Aux thread write unlocked rwspinlock");

    return NULL;
}


static void
rwspinlock_test(void)
{
    int data;
    int res;
    ethr_tid tid;

    print_line("Trying to initialize rwspinlock");
    res = ethr_rwlock_init(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Initialized rwspinlock");

    rwst_data = 4711;

    print_line("Main thread tries to read lock rwspinlock");
    ethr_read_lock(&rwst_rwspinlock);
    print_line("Main thread read locked rwspinlock");

    ASSERT(rwst_data == 4711);

    print_line("Main thread about to create aux thread");
    res = ethr_thr_create(&tid, rwst_thread, NULL, NULL);
    ASSERT(res == 0);
    print_line("Main thread created aux thread");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rwst_data == 4711);

    print_line("Main thread tries to read unlock rwspinlock");
    ethr_read_unlock(&rwst_rwspinlock);
    print_line("Main thread read unlocked rwspinlock");

    print_line("Main thread tries to write lock rwspinlock");
    ethr_write_lock(&rwst_rwspinlock);
    print_line("Main thread write locked rwspinlock");

    data = ++rwst_data;

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rwst_data == data);
    ++rwst_data;

    print_line("Main thread tries to write unlock rwspinlock");
    ethr_write_unlock(&rwst_rwspinlock);
    print_line("Main thread write unlocked rwspinlock");

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);
    print_line("Main thread joined aux thread");

    res = ethr_rwlock_destroy(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Main thread destroyed rwspinlock");

}


/*
 * The rwmutex test case.
 *
 * Tests rwmutexes.
 */

static ethr_rwmutex rwmt_rwmutex;
static int rwmt_data;

void *
rwmt_thread(void *unused)
{
    int data;

    print_line("Aux thread tries to read lock rwmutex");
    ethr_rwmutex_rlock(&rwmt_rwmutex);
    print_line("Aux thread read locked rwmutex");

    ASSERT(rwmt_data == 4711);

    print_line("Aux thread tries to read unlock rwmutex");
    ethr_rwmutex_runlock(&rwmt_rwmutex);
    print_line("Aux thread read unlocked rwmutex");

    print_line("Aux thread tries to write lock rwmutex");
    ethr_rwmutex_rwlock(&rwmt_rwmutex);
    print_line("Aux thread write locked rwmutex");

    data = ++rwmt_data;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(rwmt_data == data);
    ++rwmt_data;

    print_line("Aux thread tries to write unlock rwmutex");
    ethr_rwmutex_rwunlock(&rwmt_rwmutex);
    print_line("Aux thread write unlocked rwmutex");

    return NULL;
}


static void
rwmutex_test(void)
{
    int data;
    int res;
    ethr_tid tid;

    print_line("Trying to initialize rwmutex");
    res = ethr_rwmutex_init(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Initialized rwmutex");

    rwmt_data = 4711;

    print_line("Main thread tries to read lock rwmutex");
    ethr_rwmutex_rlock(&rwmt_rwmutex);
    print_line("Main thread read locked rwmutex");

    ASSERT(rwmt_data == 4711);

    print_line("Main thread about to create aux thread");
    res = ethr_thr_create(&tid, rwmt_thread, NULL, NULL);
    ASSERT(res == 0);
    print_line("Main thread created aux thread");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rwmt_data == 4711);

    print_line("Main thread tries to read unlock rwmutex");
    ethr_rwmutex_runlock(&rwmt_rwmutex);
    print_line("Main thread read unlocked rwmutex");

    print_line("Main thread tries to write lock rwmutex");
    ethr_rwmutex_rwlock(&rwmt_rwmutex);
    print_line("Main thread write locked rwmutex");

    data = ++rwmt_data;

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rwmt_data == data);
    ++rwmt_data;

    print_line("Main thread tries to write unlock rwmutex");
    ethr_rwmutex_rwunlock(&rwmt_rwmutex);
    print_line("Main thread write unlocked rwmutex");

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);
    print_line("Main thread joined aux thread");

    res = ethr_rwmutex_destroy(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Main thread destroyed rwmutex");

}

/*
 * The atomic test case.
 *
 * Tests atomics.
 */

#define AT_AINT32_MAX 0x7fffffff
#define AT_AINT32_MIN 0x80000000

#define AT_THREADS 4
#define AT_ITER 10000

long at_set_val, at_rm_val, at_max_val;

static ethr_atomic_t at_ready;
static ethr_atomic_t at_go;
static ethr_atomic_t at_done;
static ethr_atomic_t at_data;

#define AT_TEST_INIT(T, A, B) \
do { \
    ethr_ ## A ## _init ## B(&A, 17); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 17); \
} while (0)

#define AT_TEST_SET(T, A, B) \
do { \
    ethr_ ## A ## _set ## B(&A, 4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 4711); \
} while (0)

#define AT_TEST_XCHG(T, A, B) \
do { \
    ethr_ ## A ## _set ## B(&A, 4711); \
    ASSERT(ethr_ ## A ## _xchg ## B(&A, 17) == 4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 17); \
} while (0)

#define AT_TEST_CMPXCHG(T, A, B) \
do { \
    ethr_ ## A ## _set ## B(&A, 4711); \
    ASSERT(ethr_ ## A ## _cmpxchg ## B(&A, 17, 33) == 4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 4711); \
    ASSERT(ethr_ ## A ## _cmpxchg ## B(&A, 17, 4711) == 4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 17); \
} while (0)

#define AT_TEST_ADD_READ(T, A, B) \
do { \
    T var_ = AT_AINT32_MAX; \
    var_ += 4711; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MAX); \
    ASSERT(ethr_ ## A ## _add_read ## B(&A, 4711) == var_); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    var_ = AT_AINT32_MIN; \
    var_ -= 4711; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MIN); \
    ASSERT(ethr_ ## A ## _add_read ## B(&A, -4711) == var_); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    ethr_ ## A ## _set ## B(&A, 4711); \
    ASSERT(ethr_ ## A ## _add_read ## B(&A, 10) == 4721); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 4721); \
} while (0)

#define AT_TEST_ADD(T, A, B) \
do { \
    T var_ = AT_AINT32_MAX; \
    var_ += 4711; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MAX); \
    ethr_ ## A ## _add ## B(&A, 4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    var_ = AT_AINT32_MIN; \
    var_ -= 4711; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MIN); \
    ethr_ ## A ## _add ## B(&A, -4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    ethr_ ## A ## _set ## B(&A, 11); \
    ethr_ ## A ## _add ## B(&A, 4700); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 4711); \
} while (0)

#define AT_TEST_INC_READ(T, A, B) \
do { \
    T var_ = AT_AINT32_MAX; \
    var_++; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MAX); \
    ASSERT(ethr_ ## A ## _inc_read ## B(&A) == var_); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    ethr_ ## A ## _set ## B(&A, 4710); \
    ASSERT(ethr_ ## A ## _inc_read ## B(&A) == 4711); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 4711); \
} while (0)

#define AT_TEST_DEC_READ(T, A, B) \
do { \
    T var_ = AT_AINT32_MIN; \
    var_--; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MIN); \
    ASSERT(ethr_ ## A ## _dec_read ## B(&A) == var_); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    ethr_ ## A ## _set ## B(&A, 17); \
    ASSERT(ethr_ ## A ## _dec_read ## B(&A) == 16); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 16); \
} while (0)


#define AT_TEST_INC(T, A, B) \
do { \
    T var_ = AT_AINT32_MAX; \
    var_++; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MAX); \
    ethr_ ## A ## _inc ## B(&A); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    ethr_ ## A ## _set ## B(&A, 4710); \
    ethr_ ## A ## _inc ## B(&A); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 4711); \
} while (0)

#define AT_TEST_DEC(T, A, B) \
do { \
    T var_ = AT_AINT32_MIN; \
    var_--; \
    ethr_ ## A ## _set ## B(&A, AT_AINT32_MIN); \
    ethr_ ## A ## _dec ## B(&A); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == var_); \
    ethr_ ## A ## _set ## B(&A, 17); \
    ethr_ ## A ## _dec ## B(&A); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 16); \
} while (0)

#define AT_TEST_READ_BAND(T, A, B) \
do { \
    ethr_ ## A ## _set ## B(&A, 0x13131313); \
    ASSERT(ethr_ ## A ## _read_band ## B(&A, 0x31313131) == 0x13131313); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 0x11111111); \
} while (0)

#define AT_TEST_READ_BOR(T, A, B) \
do { \
    ethr_ ## A ## _set ## B(&A, 0x11111111); \
    ASSERT(ethr_ ## A ## _read_bor ## B(&A, 0x23232323) == 0x11111111); \
    ASSERT(ethr_ ## A ## _read ## B(&A) == 0x33333333); \
} while (0)

ethr_atomic32_t atomic32;
ethr_atomic_t atomic;
ethr_dw_atomic_t dw_atomic;

static void
atomic_basic_test(void)
{
    /*
     * Verify that each op does what it is expected
     * to do for at least one input.
     */

    print_line("AT_AINT32_MAX=%d",AT_AINT32_MAX);
    print_line("AT_AINT32_MIN=%d",AT_AINT32_MIN);

    AT_TEST_INIT(ethr_sint32_t, atomic32, );
    AT_TEST_SET(ethr_sint32_t, atomic32, );
    AT_TEST_XCHG(ethr_sint32_t, atomic32, );
    AT_TEST_CMPXCHG(ethr_sint32_t, atomic32, );
    AT_TEST_ADD_READ(ethr_sint32_t, atomic32, );
    AT_TEST_ADD(ethr_sint32_t, atomic32, );
    AT_TEST_INC_READ(ethr_sint32_t, atomic32, );
    AT_TEST_DEC_READ(ethr_sint32_t, atomic32, );
    AT_TEST_INC(ethr_sint32_t, atomic32, );
    AT_TEST_DEC(ethr_sint32_t, atomic32, );
    AT_TEST_READ_BAND(ethr_sint32_t, atomic32, );
    AT_TEST_READ_BOR(ethr_sint32_t, atomic32, );

    AT_TEST_INIT(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_SET(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_XCHG(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_CMPXCHG(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_ADD_READ(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_ADD(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_INC_READ(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_DEC_READ(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_INC(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_DEC(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_READ_BAND(ethr_sint32_t, atomic32, _acqb);
    AT_TEST_READ_BOR(ethr_sint32_t, atomic32, _acqb);

    AT_TEST_INIT(ethr_sint32_t, atomic32, _relb);
    AT_TEST_SET(ethr_sint32_t, atomic32, _relb);
    AT_TEST_XCHG(ethr_sint32_t, atomic32, _relb);
    AT_TEST_CMPXCHG(ethr_sint32_t, atomic32, _relb);
    AT_TEST_ADD_READ(ethr_sint32_t, atomic32, _relb);
    AT_TEST_ADD(ethr_sint32_t, atomic32, _relb);
    AT_TEST_INC_READ(ethr_sint32_t, atomic32, _relb);
    AT_TEST_DEC_READ(ethr_sint32_t, atomic32, _relb);
    AT_TEST_INC(ethr_sint32_t, atomic32, _relb);
    AT_TEST_DEC(ethr_sint32_t, atomic32, _relb);
    AT_TEST_READ_BAND(ethr_sint32_t, atomic32, _relb);
    AT_TEST_READ_BOR(ethr_sint32_t, atomic32, _relb);

    AT_TEST_INIT(ethr_sint32_t, atomic32, _rb);
    AT_TEST_SET(ethr_sint32_t, atomic32, _rb);
    AT_TEST_XCHG(ethr_sint32_t, atomic32, _rb);
    AT_TEST_CMPXCHG(ethr_sint32_t, atomic32, _rb);
    AT_TEST_ADD_READ(ethr_sint32_t, atomic32, _rb);
    AT_TEST_ADD(ethr_sint32_t, atomic32, _rb);
    AT_TEST_INC_READ(ethr_sint32_t, atomic32, _rb);
    AT_TEST_DEC_READ(ethr_sint32_t, atomic32, _rb);
    AT_TEST_INC(ethr_sint32_t, atomic32, _rb);
    AT_TEST_DEC(ethr_sint32_t, atomic32, _rb);
    AT_TEST_READ_BAND(ethr_sint32_t, atomic32, _rb);
    AT_TEST_READ_BOR(ethr_sint32_t, atomic32, _rb);

    AT_TEST_INIT(ethr_sint32_t, atomic32, _wb);
    AT_TEST_SET(ethr_sint32_t, atomic32, _wb);
    AT_TEST_XCHG(ethr_sint32_t, atomic32, _wb);
    AT_TEST_CMPXCHG(ethr_sint32_t, atomic32, _wb);
    AT_TEST_ADD_READ(ethr_sint32_t, atomic32, _wb);
    AT_TEST_ADD(ethr_sint32_t, atomic32, _wb);
    AT_TEST_INC_READ(ethr_sint32_t, atomic32, _wb);
    AT_TEST_DEC_READ(ethr_sint32_t, atomic32, _wb);
    AT_TEST_INC(ethr_sint32_t, atomic32, _wb);
    AT_TEST_DEC(ethr_sint32_t, atomic32, _wb);
    AT_TEST_READ_BAND(ethr_sint32_t, atomic32, _wb);
    AT_TEST_READ_BOR(ethr_sint32_t, atomic32, _wb);

    AT_TEST_INIT(ethr_sint32_t, atomic32, _mb);
    AT_TEST_SET(ethr_sint32_t, atomic32, _mb);
    AT_TEST_XCHG(ethr_sint32_t, atomic32, _mb);
    AT_TEST_CMPXCHG(ethr_sint32_t, atomic32, _mb);
    AT_TEST_ADD_READ(ethr_sint32_t, atomic32, _mb);
    AT_TEST_ADD(ethr_sint32_t, atomic32, _mb);
    AT_TEST_INC_READ(ethr_sint32_t, atomic32, _mb);
    AT_TEST_DEC_READ(ethr_sint32_t, atomic32, _mb);
    AT_TEST_INC(ethr_sint32_t, atomic32, _mb);
    AT_TEST_DEC(ethr_sint32_t, atomic32, _mb);
    AT_TEST_READ_BAND(ethr_sint32_t, atomic32, _mb);
    AT_TEST_READ_BOR(ethr_sint32_t, atomic32, _mb);

    AT_TEST_INIT(ethr_sint_t, atomic, );
    AT_TEST_SET(ethr_sint_t, atomic, );
    AT_TEST_XCHG(ethr_sint_t, atomic, );
    AT_TEST_CMPXCHG(ethr_sint_t, atomic, );
    AT_TEST_ADD_READ(ethr_sint_t, atomic, );
    AT_TEST_ADD(ethr_sint_t, atomic, );
    AT_TEST_INC_READ(ethr_sint_t, atomic, );
    AT_TEST_DEC_READ(ethr_sint_t, atomic, );
    AT_TEST_INC(ethr_sint_t, atomic, );
    AT_TEST_DEC(ethr_sint_t, atomic, );
    AT_TEST_READ_BAND(ethr_sint_t, atomic, );
    AT_TEST_READ_BOR(ethr_sint_t, atomic, );

    AT_TEST_INIT(ethr_sint_t, atomic, _acqb);
    AT_TEST_SET(ethr_sint_t, atomic, _acqb);
    AT_TEST_XCHG(ethr_sint_t, atomic, _acqb);
    AT_TEST_CMPXCHG(ethr_sint_t, atomic, _acqb);
    AT_TEST_ADD_READ(ethr_sint_t, atomic, _acqb);
    AT_TEST_ADD(ethr_sint_t, atomic, _acqb);
    AT_TEST_INC_READ(ethr_sint_t, atomic, _acqb);
    AT_TEST_DEC_READ(ethr_sint_t, atomic, _acqb);
    AT_TEST_INC(ethr_sint_t, atomic, _acqb);
    AT_TEST_DEC(ethr_sint_t, atomic, _acqb);
    AT_TEST_READ_BAND(ethr_sint_t, atomic, _acqb);
    AT_TEST_READ_BOR(ethr_sint_t, atomic, _acqb);

    AT_TEST_INIT(ethr_sint_t, atomic, _relb);
    AT_TEST_SET(ethr_sint_t, atomic, _relb);
    AT_TEST_XCHG(ethr_sint_t, atomic, _relb);
    AT_TEST_CMPXCHG(ethr_sint_t, atomic, _relb);
    AT_TEST_ADD_READ(ethr_sint_t, atomic, _relb);
    AT_TEST_ADD(ethr_sint_t, atomic, _relb);
    AT_TEST_INC_READ(ethr_sint_t, atomic, _relb);
    AT_TEST_DEC_READ(ethr_sint_t, atomic, _relb);
    AT_TEST_INC(ethr_sint_t, atomic, _relb);
    AT_TEST_DEC(ethr_sint_t, atomic, _relb);
    AT_TEST_READ_BAND(ethr_sint_t, atomic, _relb);
    AT_TEST_READ_BOR(ethr_sint_t, atomic, _relb);

    AT_TEST_INIT(ethr_sint_t, atomic, _rb);
    AT_TEST_SET(ethr_sint_t, atomic, _rb);
    AT_TEST_XCHG(ethr_sint_t, atomic, _rb);
    AT_TEST_CMPXCHG(ethr_sint_t, atomic, _rb);
    AT_TEST_ADD_READ(ethr_sint_t, atomic, _rb);
    AT_TEST_ADD(ethr_sint_t, atomic, _rb);
    AT_TEST_INC_READ(ethr_sint_t, atomic, _rb);
    AT_TEST_DEC_READ(ethr_sint_t, atomic, _rb);
    AT_TEST_INC(ethr_sint_t, atomic, _rb);
    AT_TEST_DEC(ethr_sint_t, atomic, _rb);
    AT_TEST_READ_BAND(ethr_sint_t, atomic, _rb);
    AT_TEST_READ_BOR(ethr_sint_t, atomic, _rb);

    AT_TEST_INIT(ethr_sint_t, atomic, _wb);
    AT_TEST_SET(ethr_sint_t, atomic, _wb);
    AT_TEST_XCHG(ethr_sint_t, atomic, _wb);
    AT_TEST_CMPXCHG(ethr_sint_t, atomic, _wb);
    AT_TEST_ADD_READ(ethr_sint_t, atomic, _wb);
    AT_TEST_ADD(ethr_sint_t, atomic, _wb);
    AT_TEST_INC_READ(ethr_sint_t, atomic, _wb);
    AT_TEST_DEC_READ(ethr_sint_t, atomic, _wb);
    AT_TEST_INC(ethr_sint_t, atomic, _wb);
    AT_TEST_DEC(ethr_sint_t, atomic, _wb);
    AT_TEST_READ_BAND(ethr_sint_t, atomic, _wb);
    AT_TEST_READ_BOR(ethr_sint_t, atomic, _wb);

    AT_TEST_INIT(ethr_sint_t, atomic, _mb);
    AT_TEST_SET(ethr_sint_t, atomic, _mb);
    AT_TEST_XCHG(ethr_sint_t, atomic, _mb);
    AT_TEST_CMPXCHG(ethr_sint_t, atomic, _mb);
    AT_TEST_ADD_READ(ethr_sint_t, atomic, _mb);
    AT_TEST_ADD(ethr_sint_t, atomic, _mb);
    AT_TEST_INC_READ(ethr_sint_t, atomic, _mb);
    AT_TEST_DEC_READ(ethr_sint_t, atomic, _mb);
    AT_TEST_INC(ethr_sint_t, atomic, _mb);
    AT_TEST_DEC(ethr_sint_t, atomic, _mb);
    AT_TEST_READ_BAND(ethr_sint_t, atomic, _mb);
    AT_TEST_READ_BOR(ethr_sint_t, atomic, _mb);

    /* Double word */
    {
	ethr_dw_sint_t dw0, dw1;
	dw0.sint[0] = 4711;
	dw0.sint[1] = 4712;

	/* init */
	ethr_dw_atomic_init(&dw_atomic, &dw0);
	ethr_dw_atomic_read(&dw_atomic, &dw1);
	ETHR_ASSERT(dw1.sint[0] == 4711);
	ETHR_ASSERT(dw1.sint[1] == 4712);
	
	/* set */
	dw0.sint[0] = 42;
	dw0.sint[1] = ~((ethr_sint_t) 0);
	ethr_dw_atomic_set(&dw_atomic, &dw0);
	ethr_dw_atomic_read(&dw_atomic, &dw1);
	ASSERT(dw1.sint[0] == 42);
	ASSERT(dw1.sint[1] == ~((ethr_sint_t) 0));

	/* cmpxchg */
	dw0.sint[0] = 17;
	dw0.sint[1] = 18;
	dw1.sint[0] = 19;
	dw1.sint[1] = 20;
	ASSERT(!ethr_dw_atomic_cmpxchg(&dw_atomic, &dw1, &dw0));
	ethr_dw_atomic_read(&dw_atomic, &dw0);
	ASSERT(dw0.sint[0] == 42);
	ASSERT(dw0.sint[1] == ~((ethr_sint_t) 0));

	ASSERT(ethr_dw_atomic_cmpxchg(&dw_atomic, &dw1, &dw0));

	ethr_dw_atomic_read(&dw_atomic, &dw0);
	ASSERT(dw0.sint[0] == 19);
	ASSERT(dw0.sint[1] == 20);
    }
}


#define AT_DW_MIN 12
#define AT_DW_MAX 42
#define AT_DW_THREADS (AT_DW_MAX - AT_DW_MIN + 1)

#define AT_DW_LOOPS 200000
#define AT_DW_R_LOOPS 10

ethr_dw_atomic_t at_dw_atomic;

void
at_dw_valid(ethr_dw_sint_t *dw)
{
    int i;
    char c;
    char *cp;

    ASSERT(dw->sint[0] == dw->sint[1]);

    cp = (char *) &dw->sint[0];
    c = cp[0];

    ASSERT(AT_DW_MIN <= c && c <= AT_DW_MAX);

    for (i = 0; i < sizeof(ethr_sint_t); i++)
	ASSERT(c == cp[i]);
}

void *
at_dw_thr(void *vval)
{
    int l, r;
    ethr_sint_t val = (ethr_sint_t) vval;
    ethr_dw_sint_t dw;
    ethr_dw_sint_t my_dw;

    my_dw.sint[0] = val;
    my_dw.sint[1] = val;

    ethr_dw_atomic_set(&at_dw_atomic, &my_dw);
    for (l = 0; l < AT_DW_LOOPS; l++) {
	for (r = 0; r < AT_DW_R_LOOPS; r++) {
	    ethr_dw_atomic_read(&at_dw_atomic, &dw);
	    at_dw_valid(&dw);
	}
	ethr_dw_atomic_set(&at_dw_atomic, &my_dw);
	for (r = 0; r < AT_DW_R_LOOPS; r++) {
	    ethr_dw_atomic_read(&at_dw_atomic, &dw);
	    at_dw_valid(&dw);
	}
	dw.sint[0] = 0;
	dw.sint[1] = 0;
	while (1) {
	    if (ethr_dw_atomic_cmpxchg(&at_dw_atomic, &my_dw, &dw))
		break;
	}
    }
}

static void
dw_atomic_massage_test(void)
{
    int i, res;
    ethr_tid tid[AT_DW_THREADS];
    ethr_thr_opts thr_opts = ETHR_THR_OPTS_DEFAULT_INITER;
    ethr_dw_sint_t dw;

    dw.sint[0] = dw.sint[1] = 0;

    ethr_dw_atomic_init(&at_dw_atomic, &dw);

    for (i = AT_DW_MIN; i <= AT_DW_MAX; i++) {
	ethr_sint_t val;
	memset(&val, i, sizeof(ethr_sint_t));
	res = ethr_thr_create(&tid[i-AT_DW_MIN], at_dw_thr, (void *) val, &thr_opts);
	ASSERT(res == 0);
    }
    for (i = AT_DW_MIN; i <= AT_DW_MAX; i++) {
	res = ethr_thr_join(tid[i-AT_DW_MIN], NULL);
	ASSERT(res == 0);
    }
}

void *
at_thread(void *unused)
{
    int i;
    long val, go;

    val = ethr_atomic_inc_read(&at_ready);
    ASSERT(val > 0);
    ASSERT(val <= AT_THREADS);

    do {
	go = ethr_atomic_read(&at_go);
    } while (!go);

    for (i = 0; i < AT_ITER; i++) {
	val = ethr_atomic_read_bor(&at_data, at_set_val);
	ASSERT(val >= (i == 0 ? 0 : at_set_val) + (long) 4711);
	ASSERT(val <= at_max_val);

	val = ethr_atomic_read_band(&at_data, ~at_rm_val);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	val = ethr_atomic_read(&at_data);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	val = ethr_atomic_inc_read(&at_data);
	ASSERT(val > at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	val = ethr_atomic_dec_read(&at_data);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	ethr_atomic_inc(&at_data);

	ethr_atomic_dec(&at_data);

	val = ethr_atomic_add_read(&at_data, (long) 4711);
	ASSERT(val >= at_set_val + (long) 2*4711);
	ASSERT(val <= at_max_val);

	ethr_atomic_add(&at_data, (long) -4711);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);
    }

    ethr_atomic_inc(&at_done);
    return NULL;
}

static void
atomic_test(void)
{
    long data_init, data_final, val;
    int res, i;
    ethr_tid tid[AT_THREADS];
    ethr_thr_opts thr_opts = ETHR_THR_OPTS_DEFAULT_INITER;

    atomic_basic_test();

#if ETHR_SIZEOF_PTR > 4
	at_rm_val = ((long) 1) << 57;
	at_set_val = ((long) 1) << 60;
#else
	at_rm_val = ((long) 1) << 27;
	at_set_val = ((long) 1) << 30;
#endif

    at_max_val = at_set_val + at_rm_val + ((long) AT_THREADS + 1) * 4711;
    data_init = at_rm_val + (long) 4711;
    data_final = at_set_val + (long) 4711;

    thr_opts.detached = 1;

    print_line("Initializing");
    ethr_atomic_init(&at_ready, 0);
    ethr_atomic_init(&at_go, 0);
    ethr_atomic_init(&at_done, data_init);
    ethr_atomic_init(&at_data, data_init);

    val = ethr_atomic_read(&at_data);
    ASSERT(val == data_init);
    ethr_atomic_set(&at_done, 0);
    val = ethr_atomic_read(&at_done);
    ASSERT(val == 0);

    print_line("Creating threads");
    for (i = 0; i < AT_THREADS; i++) {
	res = ethr_thr_create(&tid[i], at_thread, NULL, &thr_opts);
	ASSERT(res == 0);
    }

    print_line("Waiting for threads to ready up");
    do {
	val = ethr_atomic_read(&at_ready);
	ASSERT(val >= 0);
	ASSERT(val <= AT_THREADS);
    } while (val != AT_THREADS);

    print_line("Letting threads loose");
    val = ethr_atomic_xchg(&at_go, 17);
    ASSERT(val == 0);
    val = ethr_atomic_read(&at_go);
    ASSERT(val == 17);


    print_line("Waiting for threads to finish");
    do {
	val = ethr_atomic_read(&at_done);
	ASSERT(val >= 0);
	ASSERT(val <= AT_THREADS);
    } while (val != AT_THREADS);

    print_line("Checking result");
    val = ethr_atomic_read(&at_data);
    ASSERT(res == 0);
    ASSERT(val == data_final);
    print_line("Result ok");
    
}


#endif /* #ifndef ETHR_NO_THREAD_LIB */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * The dispatcher                                                            *
\*                                                                           */

int
main(int argc, char *argv[])
{
    if (argc < 2)
	fail("To few arguments for test case");

#ifndef ETHR_NO_THREAD_LIB
    {
	char *testcase;

	send_my_pid();

	testcase = argv[1];

	if (ethr_init(NULL) != 0 || ethr_late_init(NULL) != 0)
	    fail("Failed to initialize the ethread library");

	if (strcmp(testcase, "create_join_thread") == 0)
	    create_join_thread_test();
	else if (strcmp(testcase, "equal_tids") == 0)
	    equal_tids_test();
	else if (strcmp(testcase, "mutex") == 0)
	    mutex_test();
	else if (strcmp(testcase, "try_lock_mutex") == 0)
	    try_lock_mutex_test();
	else if (strcmp(testcase, "cond_wait") == 0)
	    cond_wait_test();
	else if (strcmp(testcase, "broadcast") == 0)
	    broadcast_test();
	else if (strcmp(testcase, "detached_thread") == 0)
	    detached_thread_test();
	else if (strcmp(testcase, "max_threads") == 0)
	    max_threads_test();
	else if (strcmp(testcase, "tsd") == 0)
	    tsd_test();
	else if (strcmp(testcase, "spinlock") == 0)
	    spinlock_test();
	else if (strcmp(testcase, "rwspinlock") == 0)
	    rwspinlock_test();
	else if (strcmp(testcase, "rwmutex") == 0)
	    rwmutex_test();
	else if (strcmp(testcase, "atomic") == 0)
	    atomic_test();
	else if (strcmp(testcase, "dw_atomic_massage") == 0)
	    dw_atomic_massage_test();
	else
	    skip("Test case \"%s\" not implemented yet", testcase);

	succeed(NULL);
    }
#else /* #ifndef ETHR_NO_THREAD_LIB */
    skip("No ethread library to test");
#endif /* #ifndef ETHR_NO_THREAD_LIB */

    return 0;
}
