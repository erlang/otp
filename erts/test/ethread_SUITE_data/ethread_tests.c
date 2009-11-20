/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
}

static void print_line(char *frmt,...)
{
    PRINT_VA_LIST(frmt);
    print_eol();
}

static void print(char *frmt,...)
{
    PRINT_VA_LIST(frmt);
}

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
	int *tres;
	res = ethr_thr_join(cjtt_tids[i], (void **) &tres);
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
static ethr_mutex ett_mutex = ETHR_MUTEX_INITER;
static ethr_cond ett_cond = ETHR_COND_INITER;
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
    res = ethr_mutex_lock(&ett_mutex);
    ASSERT(res == 0);
    while (!ett_terminate) {
	res = ethr_cond_wait(&ett_cond, &ett_mutex);
	ASSERT(res == 0);
    }
    res = ethr_mutex_unlock(&ett_mutex);
    ASSERT(res == 0);
    return NULL;
}

static void
equal_tids_test(void)
{
    int res, i;

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

    res = ethr_mutex_lock(&ett_mutex);
    ASSERT(res == 0);
    ett_terminate = 1;
    res = ethr_cond_signal(&ett_cond);
    ASSERT(res == 0);
    res = ethr_mutex_unlock(&ett_mutex);
    ASSERT(res == 0);
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

static ethr_mutex mt_mutex = ETHR_MUTEX_INITER;
static int mt_data;

void *
mt_thread(void *unused)
{
    int res;

    print_line("Aux thread tries to lock mutex");
    res = ethr_mutex_lock(&mt_mutex);
    ASSERT(res == 0);
    print_line("Aux thread locked mutex");

    ASSERT(mt_data == 0);

    mt_data = 1;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(mt_data == 1);

    res = ethr_mutex_unlock(&mt_mutex);
    ASSERT(res == 0);
    print_line("Aux thread unlocked mutex");

    return NULL;
}


static void
mutex_test(void)
{
    int do_restart = 1;
    int res;
    ethr_tid tid;

    print_line("Running test with statically initialized mutex");

 restart:
    mt_data = 0;

    print_line("Main thread tries to lock mutex");
    res = ethr_mutex_lock(&mt_mutex);
    ASSERT(res == 0);
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

    res = ethr_mutex_unlock(&mt_mutex);
    ASSERT(res == 0);
    print_line("Main thread unlocked mutex");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    print_line("Main thread tries to lock mutex");
    res = ethr_mutex_lock(&mt_mutex);
    ASSERT(res == 0);
    print_line("Main thread locked mutex");

    ASSERT(mt_data == 1);

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(mt_data == 1);

    res = ethr_mutex_unlock(&mt_mutex);
    ASSERT(res == 0);
    print_line("Main thread unlocked mutex");

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);
    print_line("Main thread joined aux thread");

    res = ethr_mutex_destroy(&mt_mutex);
    ASSERT(res == 0);
    print_line("Main thread destroyed mutex");

    if (do_restart) {
	do_restart = 0;

	print_line("Running test with dynamically initialized mutex");

	print_line("Trying to initialize mutex");
	res = ethr_mutex_init(&mt_mutex);
	ASSERT(res == 0);
	print_line("Initialized mutex");

	goto restart;

    }

}

/*
 * The try lock mutex test case.
 *
 * Tests try lock mutex operation.
 */

static ethr_mutex tlmt_mtx1 = ETHR_MUTEX_INITER;
static ethr_mutex tlmt_mtx2 = ETHR_MUTEX_INITER;
static ethr_cond tlmt_cnd2 = ETHR_COND_INITER;

static int tlmt_mtx1_locked;
static int tlmt_mtx1_do_unlock;

static void *
tlmt_thread(void *unused)
{
    int res;

    res = ethr_mutex_lock(&tlmt_mtx1);
    ASSERT(res == 0);
    res = ethr_mutex_lock(&tlmt_mtx2);
    ASSERT(res == 0);

    tlmt_mtx1_locked = 1;
    res = ethr_cond_signal(&tlmt_cnd2);
    ASSERT(res == 0);

    while (!tlmt_mtx1_do_unlock) {
	res = ethr_cond_wait(&tlmt_cnd2, &tlmt_mtx2);
	ASSERT(res == 0 || res == EINTR);
    }

    res = ethr_mutex_unlock(&tlmt_mtx2);
    ASSERT(res == 0);
    res = ethr_mutex_unlock(&tlmt_mtx1);
    ASSERT(res == 0);

    res = ethr_mutex_lock(&tlmt_mtx2);
    ASSERT(res == 0);
    tlmt_mtx1_locked = 0;
    res = ethr_cond_signal(&tlmt_cnd2);
    ASSERT(res == 0);
    res = ethr_mutex_unlock(&tlmt_mtx2);
    ASSERT(res == 0);

    return NULL;
}

static void
try_lock_mutex_test(void)
{
    int i, res;
    ethr_tid tid;

    tlmt_mtx1_locked = 0;
    tlmt_mtx1_do_unlock = 0;

    res = ethr_thr_create(&tid, tlmt_thread, NULL, NULL);
    ASSERT(res == 0);

    res = ethr_mutex_lock(&tlmt_mtx2);
    ASSERT(res == 0);

    while (!tlmt_mtx1_locked) {
	res = ethr_cond_wait(&tlmt_cnd2, &tlmt_mtx2);
	ASSERT(res == 0 || res == EINTR);
    }

    res = ethr_mutex_unlock(&tlmt_mtx2);
    ASSERT(res == 0);

    for (i = 0; i < 10; i++) {
	res = ethr_mutex_trylock(&tlmt_mtx1);
	ASSERT(res == EBUSY);
    }

    res = ethr_mutex_lock(&tlmt_mtx2);
    ASSERT(res == 0);

    tlmt_mtx1_do_unlock = 1;
    res = ethr_cond_signal(&tlmt_cnd2);
    ASSERT(res == 0);

    while (tlmt_mtx1_locked) {
	res = ethr_cond_wait(&tlmt_cnd2, &tlmt_mtx2);
	ASSERT(res == 0 || res == EINTR);
    }

    res = ethr_mutex_unlock(&tlmt_mtx2);
    ASSERT(res == 0);

    res = ethr_mutex_trylock(&tlmt_mtx1);
    ASSERT(res == 0);

    res = ethr_mutex_unlock(&tlmt_mtx1);
    ASSERT(res == 0);

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
 * The recursive mutex test case.
 *
 * Tests recursive mutexes.
 */

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT

static ethr_mutex rmt_mutex
#ifdef ETHR_REC_MUTEX_INITER 
                            = ETHR_REC_MUTEX_INITER
#endif
                                                   ;
static int rmt_data;

void *
rmt_thread(void *unused)
{
    int res;

    print_line("Aux thread tries to lock mutex");
    res = ethr_mutex_lock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Aux thread locked mutex");

    ASSERT(rmt_data == 0);

    rmt_data = 1;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(rmt_data == 1);

    res = ethr_mutex_unlock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Aux thread unlocked mutex");

    return NULL;
}

#endif

static void
recursive_mutex_test(void)
{
#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
    int do_restart = 1;
    int res;
    ethr_tid tid;

#ifdef ETHR_REC_MUTEX_INITER
    print_line("Running test with statically initialized mutex");
#else
    goto dynamic_init;
#endif

 restart:
    rmt_data = 0;

    print_line("Main thread tries to lock mutex");
    res = ethr_mutex_lock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread locked mutex");

    print_line("Main thread tries to lock mutex again");
    res = ethr_mutex_lock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread locked mutex again");

    ASSERT(rmt_data == 0);

    print_line("Main thread about to create aux thread");
    res = ethr_thr_create(&tid, rmt_thread, NULL, NULL);
    ASSERT(res == 0);
    print_line("Main thread created aux thread");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rmt_data == 0);

    res = ethr_mutex_unlock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread unlocked mutex");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rmt_data == 0);

    res = ethr_mutex_unlock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread unlocked mutex again");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    print_line("Main thread tries to lock mutex");
    res = ethr_mutex_lock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread locked mutex");

    ASSERT(rmt_data == 1);

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rmt_data == 1);

    res = ethr_mutex_unlock(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread unlocked mutex");

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);
    print_line("Main thread joined aux thread");

    res = ethr_mutex_destroy(&rmt_mutex);
    ASSERT(res == 0);
    print_line("Main thread destroyed mutex");

    if (do_restart) {
#ifndef ETHR_REC_MUTEX_INITER
    dynamic_init:
#endif
	do_restart = 0;

	print_line("Running test with dynamically initialized mutex");

	print_line("Trying to initialize mutex");
	res = ethr_rec_mutex_init(&rmt_mutex);
	ASSERT(res == 0);
	print_line("Initialized mutex");

	goto restart;
    }

#ifndef ETHR_REC_MUTEX_INITER
    succeed("Static initializer for recursive mutexes not supported");
#endif

#else /* #ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT */
    skip("Recursive mutexes not supported");
#endif /* #ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT */
}

/*
 * The time now test.
 *
 * Tests ethr_time_now by comparing time values with Erlang.
 */
#define TNT_MAX_TIME_DIFF 200000
#define TNT_MAX_TIME_VALUES 52

static void
time_now_test(void)
{
    int scanf_res, time_now_res, i, no_values, max_abs_diff;
    static ethr_timeval tv[TNT_MAX_TIME_VALUES];
    static int ms[TNT_MAX_TIME_VALUES];

    i = 0;
    do {
	ASSERT(i < TNT_MAX_TIME_VALUES);
	scanf_res = scanf("%d", &ms[i]);
	time_now_res = ethr_time_now(&tv[i]);
	ASSERT(scanf_res == 1);
	ASSERT(time_now_res == 0);
#if 0
	print_line("Got %d; %ld:%ld", ms[i], tv[i].tv_sec, tv[i].tv_nsec);
#endif
	i++;
    } while (ms[i-1] >= 0);

    no_values = i-1;

    ASSERT(ms[0] == 0);

    print_line("TNT_MAX_TIME_DIFF = %d (us)", TNT_MAX_TIME_DIFF);

    max_abs_diff = 0;

    for (i = 1; i < no_values; i++) {
	long diff;
	long tn_us;
	long e_us;

	tn_us = (tv[i].tv_sec - tv[0].tv_sec) * 1000000;
	tn_us += (tv[i].tv_nsec - tv[0].tv_nsec)/1000;

	e_us = ms[i]*1000;
	
	diff = e_us - tn_us;

	print_line("Erlang time = %ld us; ethr_time_now = %ld us; diff %ld us",
		   e_us, tn_us, diff);

	if (max_abs_diff < abs((int) diff)) {
	    max_abs_diff = abs((int) diff);
	}

	ASSERT(e_us - TNT_MAX_TIME_DIFF <= tn_us);
	ASSERT(tn_us <= e_us + TNT_MAX_TIME_DIFF);
    }

    print_line("Max absolute diff = %d us", max_abs_diff);
    succeed("Max absolute diff = %d us", max_abs_diff);
}

/*
 * The cond wait test case.
 *
 * Tests ethr_cond_wait with ethr_cond_signal and ethr_cond_broadcast.
 */


static ethr_mutex cwt_mutex = ETHR_MUTEX_INITER;
static ethr_cond cwt_cond = ETHR_COND_INITER;
static int cwt_counter;

void *
cwt_thread(void *is_timedwait_test_ptr)
{
    int use_timedwait = *((int *) is_timedwait_test_ptr);
    int res;

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    if (use_timedwait) {
	ethr_timeval tv;
	res = ethr_time_now(&tv);
	ASSERT(res == 0);
	tv.tv_sec += 3600; /* Make sure we won't time out */

	do {
	    res = ethr_cond_timedwait(&cwt_cond, &cwt_mutex, &tv);
	} while (res == EINTR);
	ASSERT(res == 0);
    }
    else {
	do {
	    res = ethr_cond_wait(&cwt_cond, &cwt_mutex);
	} while (res == EINTR);
	ASSERT(res == 0);
    }

    cwt_counter++;

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    return NULL;
}

static void
cond_wait_test(int is_timedwait_test)
{
    int do_restart = !is_timedwait_test;
    ethr_tid tid1, tid2;
    int res;

    if (!is_timedwait_test)
	print_line("Running test with statically initialized mutex and cond");

 restart:
    /* Wake with signal */

    cwt_counter = 0;

    res = ethr_thr_create(&tid1, cwt_thread, (void *) &is_timedwait_test, NULL);
    ASSERT(res == 0);
    res = ethr_thr_create(&tid2, cwt_thread, (void *) &is_timedwait_test, NULL);
    ASSERT(res == 0);

    do_sleep(1); /* Make sure threads waits on cond var */

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    res = ethr_cond_signal(&cwt_cond); /* Wake one thread */
    ASSERT(res == 0);

    do_sleep(1); /* Make sure awakened thread waits on mutex */

    ASSERT(cwt_counter == 0);

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    do_sleep(1);  /* Let awakened thread proceed */

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    ASSERT(cwt_counter == 1);

    res = ethr_cond_signal(&cwt_cond); /* Wake the other thread */
    ASSERT(res == 0);

    do_sleep(1); /* Make sure awakened thread waits on mutex */

    ASSERT(cwt_counter == 1);

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    do_sleep(1);  /* Let awakened thread proceed */

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    ASSERT(cwt_counter == 2);

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    res = ethr_thr_join(tid1, NULL);
    ASSERT(res == 0);

    res = ethr_thr_join(tid2, NULL);
    ASSERT(res == 0);


    /* Wake with broadcast */

    cwt_counter = 0;

    res = ethr_thr_create(&tid1, cwt_thread, (void *) &is_timedwait_test, NULL);
    ASSERT(res == 0);
    res = ethr_thr_create(&tid2, cwt_thread, (void *) &is_timedwait_test, NULL);
    ASSERT(res == 0);

    do_sleep(1); /* Make sure threads waits on cond var */

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    res = ethr_cond_broadcast(&cwt_cond); /* Wake the threads */
    ASSERT(res == 0);

    do_sleep(1); /* Make sure awakened threads wait on mutex */

    ASSERT(cwt_counter == 0);

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    do_sleep(1);  /* Let awakened threads proceed */

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    ASSERT(cwt_counter == 2);

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    res = ethr_thr_join(tid1, NULL);
    ASSERT(res == 0);

    res = ethr_thr_join(tid2, NULL);
    ASSERT(res == 0);

    res = ethr_mutex_destroy(&cwt_mutex);
    ASSERT(res == 0);
    res = ethr_cond_destroy(&cwt_cond);
    ASSERT(res == 0);

    if (do_restart) {
	do_restart = 0;
	res = ethr_mutex_init(&cwt_mutex);
	ASSERT(res == 0);
	res = ethr_cond_init(&cwt_cond);
	ASSERT(res == 0);
	print_line("Running test with dynamically initialized mutex and cond");
	goto restart;
    }
}

/*
 * The cond timedwait test case.
 *
 * Tests ethr_cond_timedwait with ethr_cond_signal and ethr_cond_broadcast.
 */

#define CTWT_MAX_TIME_DIFF 100000

static long
ctwt_check_timeout(long to)
{
    int res;
    ethr_timeval tva, tvb;
    long diff, abs_diff;

    res = ethr_time_now(&tva);
    ASSERT(res == 0);

    tva.tv_sec += to / 1000;
    tva.tv_nsec += (to % 1000) * 1000000;
    if (tva.tv_nsec >= 1000000000) {
	tva.tv_sec++;
	tva.tv_nsec -= 1000000000;
	ASSERT(tva.tv_nsec < 1000000000);
    }

    do {
	res = ethr_cond_timedwait(&cwt_cond, &cwt_mutex, &tva);
    } while (res == EINTR);
    ASSERT(res == ETIMEDOUT);

    res = ethr_time_now(&tvb);
    ASSERT(res == 0);

    diff = (tvb.tv_sec - tva.tv_sec) * 1000000;
    diff += (tvb.tv_nsec - tva.tv_nsec)/1000;

    print("Timeout=%ld; ", to);
    print("tva.tv_sec=%ld tva.tv_nsec=%ld; ", tva.tv_sec, tva.tv_nsec);
    print("tvb.tv_sec=%ld tvb.tv_nsec=%ld; ", tvb.tv_sec, tvb.tv_nsec);
    print_line("diff (tvb - tva) = %ld us", diff);

    abs_diff = (long) abs((int) diff);

    ASSERT(CTWT_MAX_TIME_DIFF >= abs_diff);
    return abs_diff;
}

static void
cond_timedwait_test(void)
{
    int do_restart = 1;
    long abs_diff, max_abs_diff = 0;
    int res;

#define CTWT_UPD_MAX_DIFF if (abs_diff > max_abs_diff) max_abs_diff = abs_diff;

    print_line("Running test with statically initialized mutex and cond");

    print_line("CTWT_MAX_TIME_DIFF=%d", CTWT_MAX_TIME_DIFF);

 restart:

    res = ethr_mutex_lock(&cwt_mutex);
    ASSERT(res == 0);

    abs_diff = ctwt_check_timeout(300);
    CTWT_UPD_MAX_DIFF;
    abs_diff = ctwt_check_timeout(700);
    CTWT_UPD_MAX_DIFF;
    abs_diff = ctwt_check_timeout(1000);
    CTWT_UPD_MAX_DIFF;
    abs_diff = ctwt_check_timeout(2300);
    CTWT_UPD_MAX_DIFF;
    abs_diff = ctwt_check_timeout(5100);
    CTWT_UPD_MAX_DIFF;

    res = ethr_mutex_unlock(&cwt_mutex);
    ASSERT(res == 0);

    cond_wait_test(1);

    if (do_restart) {
	do_restart = 0;
	res = ethr_mutex_init(&cwt_mutex);
	ASSERT(res == 0);
	res = ethr_cond_init(&cwt_cond);
	ASSERT(res == 0);
	print_line("Running test with dynamically initialized mutex and cond");
	goto restart;
    }

    print_line("Max absolute diff = %d us", max_abs_diff);
    succeed("Max absolute diff = %d us", max_abs_diff);

#undef CTWT_UPD_MAX_DIFF
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
static ethr_mutex bct_mutex = ETHR_MUTEX_INITER;
static ethr_cond bct_cond = ETHR_COND_INITER;
static ethr_cond bct_cntrl_cond = ETHR_COND_INITER;


static void *
bct_thread(void *unused)
{
    int res;

    res = ethr_mutex_lock(&bct_mutex);
    ASSERT(res == 0);

    while (!bct_done) {

	bct_waiting++;
	if (bct_waiting == BCT_THREADS) {
	    res = ethr_cond_signal(&bct_cntrl_cond);
	    ASSERT(res == 0);
	}
	do {
	    res = ethr_cond_wait(&bct_cond, &bct_mutex);
	} while (res == EINTR);
	ASSERT(res == 0);
	bct_woken++;
	if (bct_woken == BCT_THREADS) {
	    res = ethr_cond_signal(&bct_cntrl_cond);
	    ASSERT(res == 0);
	}

    }

    res = ethr_mutex_unlock(&bct_mutex);
    ASSERT(res == 0);

    return NULL;
}

static void
broadcast_test(void)
{
    int res, i;
    ethr_tid tid[BCT_THREADS];

    for (i = 0; i < BCT_THREADS; i++) {
	res = ethr_thr_create(&tid[i], bct_thread, NULL, NULL);
	ASSERT(res == 0);

    }

    res = ethr_mutex_lock(&bct_mutex);
    ASSERT(res == 0);

    for (i = 0; i < BCT_NO_OF_WAITS; i++) {

	while (bct_waiting != BCT_THREADS) {
	    res = ethr_cond_wait(&bct_cntrl_cond, &bct_mutex);
	    ASSERT(res == 0 || res == EINTR);
	}

	bct_waiting = 0;
	bct_woken = 0;

	/* Wake all threads */
	res = ethr_cond_broadcast(&bct_cond);
	ASSERT(res == 0);

	while (bct_woken != BCT_THREADS) {
	    res = ethr_cond_wait(&bct_cntrl_cond, &bct_mutex);
	    ASSERT(res == 0 || res == EINTR);
	}

    }

    bct_done = 1;

    /* Wake all threads */
    res = ethr_cond_broadcast(&bct_cond);
    ASSERT(res == 0);

    res = ethr_mutex_unlock(&bct_mutex);
    ASSERT(res == 0);

    for (i = 0; i < BCT_THREADS - 1; i++) {
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

static ethr_mutex dt_mutex = ETHR_MUTEX_INITER;
static ethr_cond dt_cond = ETHR_COND_INITER;
static int dt_count;
static int dt_limit;

static void *
dt_thread(void *unused)
{
    int res;

    res = ethr_mutex_lock(&dt_mutex);
    ASSERT(res == 0);

    dt_count++;

    if (dt_count >= dt_limit)
	ethr_cond_signal(&dt_cond);

    res = ethr_mutex_unlock(&dt_mutex);
    ASSERT(res == 0);

    return NULL;
}

static void
detached_thread_test(void)
{
    ethr_thr_opts thr_opts = ETHR_THR_OPTS_DEFAULT_INITER;
    ethr_tid tid[DT_BATCH_SIZE];
    int i, j, res;

    thr_opts.detached = 1;
    dt_count = 0;
    dt_limit = 0;

    for (i = 0; i < DT_THREADS/DT_BATCH_SIZE; i++) {

	dt_limit += DT_BATCH_SIZE;

	for (j = 0; j < DT_BATCH_SIZE; j++) {
	    res = ethr_thr_create(&tid[j], dt_thread, NULL, &thr_opts);
	    ASSERT(res == 0);
	}

	res = ethr_mutex_lock(&dt_mutex);
	ASSERT(res == 0);
	while (dt_count < dt_limit) {
	    res = ethr_cond_wait(&dt_cond, &dt_mutex);
	    ASSERT(res == 0 || res == EINTR);
	}
	res = ethr_mutex_unlock(&dt_mutex);
	ASSERT(res == 0);

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

static int mtt_terminate;
static ethr_mutex mtt_mutex = ETHR_MUTEX_INITER;
static ethr_cond mtt_cond = ETHR_COND_INITER;
static char mtt_string[22*MTT_TIMES]; /* 22 is enough for ", %d" */


void *mtt_thread(void *unused)
{
    int res;

    res = ethr_mutex_lock(&mtt_mutex);
    ASSERT(res == 0);

    while (!mtt_terminate) {
	res = ethr_cond_wait(&mtt_cond, &mtt_mutex);
	ASSERT(res == 0 || res == EINTR);
    }

    res = ethr_mutex_unlock(&mtt_mutex);
    ASSERT(res == 0);

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
	    tids = (ethr_tid *) realloc((void *)tids, sizeof(ethr_tid)*no_tids);
	    ASSERT(tids);
	}
	res = ethr_thr_create(&tids[ix], mtt_thread, NULL, NULL);
	if (res != 0)
	    break;
	ix++;
    } while (res == 0);

    no_threads = ix;

    print_line("%d = ethr_thr_create()", res);
    print_line("Number of created threads: %d", no_threads);

    res = ethr_mutex_lock(&mtt_mutex);
    ASSERT(res == 0);

    mtt_terminate = 1;

    res = ethr_cond_broadcast(&mtt_cond);
    ASSERT(res == 0);

    res = ethr_mutex_unlock(&mtt_mutex);
    ASSERT(res == 0);

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
    int no_threads[MTT_TIMES], i, up, down, eq;
    char *str;

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
 * The forksafety test case.
 *
 * Tests forksafety.
 */
#ifdef __WIN32__
#define NO_FORK_PRESENT
#endif

#ifndef NO_FORK_PRESENT

static ethr_mutex ft_test_inner_mutex = ETHR_MUTEX_INITER;
static ethr_mutex ft_test_outer_mutex = ETHR_MUTEX_INITER;
static ethr_mutex ft_go_mutex = ETHR_MUTEX_INITER;
static ethr_cond ft_go_cond = ETHR_COND_INITER;
static int ft_go;
static int ft_have_forked;

static void *
ft_thread(void *unused)
{
    int res;

    res = ethr_mutex_lock(&ft_test_outer_mutex);
    ASSERT(res == 0);

    res = ethr_mutex_lock(&ft_go_mutex);
    ASSERT(res == 0);

    ft_go = 1;
    res = ethr_cond_signal(&ft_go_cond);
    ASSERT(res == 0);
    res = ethr_mutex_unlock(&ft_go_mutex);
    ASSERT(res == 0);

    do_sleep(1);
    ASSERT(!ft_have_forked);

    res = ethr_mutex_lock(&ft_test_inner_mutex);
    ASSERT(res == 0);

    res = ethr_mutex_unlock(&ft_test_inner_mutex);
    ASSERT(res == 0);

    do_sleep(1);
    ASSERT(!ft_have_forked);

    res = ethr_mutex_unlock(&ft_test_outer_mutex);
    ASSERT(res == 0);

    do_sleep(1);
    ASSERT(ft_have_forked);
    

    return NULL;
}

#endif /* #ifndef NO_FORK_PRESENT */

static void
forksafety_test(void)
{
#ifdef NO_FORK_PRESENT
    skip("No fork() present; nothing to test");
#elif defined(DEBUG)
    skip("Doesn't work in debug build");
#else
    char snd_msg[] = "ok, bye!";
    char rec_msg[sizeof(snd_msg)*2];
    ethr_tid tid;
    int res;
    int fds[2];


    res = ethr_mutex_set_forksafe(&ft_test_inner_mutex);
    if (res == ENOTSUP) {
	skip("Forksafety not supported on this platform!");
    }
    ASSERT(res == 0);
    res = ethr_mutex_set_forksafe(&ft_test_outer_mutex);
    ASSERT(res == 0);


    res = pipe(fds);
    ASSERT(res == 0);

    ft_go = 0;
    ft_have_forked = 0;

    res = ethr_mutex_lock(&ft_go_mutex);
    ASSERT(res == 0);
    
    res = ethr_thr_create(&tid, ft_thread, NULL, NULL);
    ASSERT(res == 0);

    do {
	res = ethr_cond_wait(&ft_go_cond, &ft_go_mutex);
    } while (res == EINTR || !ft_go);
    ASSERT(res == 0);

    res = ethr_mutex_unlock(&ft_go_mutex);
    ASSERT(res == 0);

    res = fork();
    ft_have_forked = 1;
    if (res == 0) {
	close(fds[0]);
	res = ethr_mutex_lock(&ft_test_outer_mutex);
	if (res != 0)
	    _exit(1);
	res = ethr_mutex_lock(&ft_test_inner_mutex);
	if (res != 0)
	    _exit(1);
	res = ethr_mutex_unlock(&ft_test_inner_mutex);
	if (res != 0)
	    _exit(1);
	res = ethr_mutex_unlock(&ft_test_outer_mutex);
	if (res != 0)
	    _exit(1);

	res = ethr_mutex_destroy(&ft_test_inner_mutex);
	if (res != 0)
	    _exit(1);
	res = ethr_mutex_destroy(&ft_test_outer_mutex);
	if (res != 0)
	    _exit(1);

	res = (int) write(fds[1], (void *) snd_msg, sizeof(snd_msg));
	if (res != sizeof(snd_msg))
	    _exit(1);
	close(fds[1]);
	_exit(0);
    }
    ASSERT(res > 0);
    close(fds[1]);

    res = (int) read(fds[0], (void *) rec_msg, sizeof(rec_msg));
    ASSERT(res == (int) sizeof(snd_msg));
    ASSERT(strcmp(snd_msg, rec_msg) == 0);

    close(fds[0]);
#endif
}


/*
 * The vfork test case.
 *
 * Tests vfork with threads.
 */

#ifdef __WIN32__
#define NO_VFORK_PRESENT
#endif

#ifndef NO_VFORK_PRESENT

#undef vfork

static ethr_mutex vt_mutex = ETHR_MUTEX_INITER;

static void *
vt_thread(void *vprog)
{
    char *prog = (char *) vprog;
    int res;
    char snd_msg[] = "ok, bye!";
    char rec_msg[sizeof(snd_msg)*2];
    int fds[2];
    char closefd[20];
    char writefd[20];

    res = pipe(fds);
    ASSERT(res == 0);

    res = sprintf(closefd, "%d", fds[0]);
    ASSERT(res <= 20);
    res = sprintf(writefd, "%d", fds[1]);
    ASSERT(res <= 20);

    print("parent: About to vfork and execute ");
    print("execlp(\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", NULL)",
	  prog, prog, "vfork", "exec", snd_msg, closefd, writefd);
    print_line(" in child");
    res = vfork();
    if (res == 0) {
	execlp(prog, prog, "vfork", "exec", snd_msg, closefd, writefd, NULL);
	_exit(1);
    }
    ASSERT(res > 0);

    print_line("parent: I'm back");

    close(fds[1]);

    res = (int) read(fds[0], (void *) rec_msg, sizeof(rec_msg));
    print_line("parent: %d = read()", res);
    print_line("parent: rec_msg=\"%s\"", rec_msg);
    ASSERT(res == (int) sizeof(snd_msg));
    ASSERT(strcmp(snd_msg, rec_msg) == 0);

    close(fds[0]);

    return NULL;
}

#endif /* #ifndef NO_VFORK_PRESENT */

static void
vfork_test(int argc, char *argv[])
{
#ifdef NO_VFORK_PRESENT
    skip("No vfork() present; nothing to test");
#else
    int res;
    ethr_tid tid;

    if (argc == 6 && strcmp("exec", argv[2]) == 0) {
	/* We are child after vfork() and execlp() ... */

	char *snd_msg;
	int closefd;
	int writefd;

	snd_msg = argv[3];
	closefd = atoi(argv[4]);
	writefd = atoi(argv[5]);

	print_line("child: snd_msg=\"%s\"; closefd=%d writefd=%d",
		   snd_msg, closefd, writefd);

	close(closefd);

	res = (int) write(writefd, (void *) snd_msg, strlen(snd_msg)+1);
	print_line("child: %d = write()", res);
	if (res != strlen(snd_msg)+1)
	    exit(1);
	close(writefd);
	print_line("child: bye");
	exit(0);
    }
    ASSERT(argc == 2);

    res = ethr_mutex_set_forksafe(&vt_mutex);
    ASSERT(res == 0 || res == ENOTSUP);
    res = ethr_mutex_lock(&vt_mutex);
    ASSERT(res == 0);

    res = ethr_thr_create(&tid, vt_thread, (void *) argv[0], NULL);
    ASSERT(res == 0);

    do_sleep(1);

    res = ethr_mutex_unlock(&vt_mutex);
    ASSERT(res == 0);

    res = ethr_thr_join(tid, NULL);
    ASSERT(res == 0);

    res = ethr_mutex_destroy(&vt_mutex);
    ASSERT(res == 0);
#endif
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

    res = ethr_tsd_key_create(&tt_key);
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
    int res;

    print_line("Aux thread tries to lock spinlock");
    res = ethr_spin_lock(&st_spinlock);
    ASSERT(res == 0);
    print_line("Aux thread locked spinlock");

    ASSERT(st_data == 0);

    st_data = 1;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(st_data == 1);

    res = ethr_spin_unlock(&st_spinlock);
    ASSERT(res == 0);
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
    res = ethr_spin_lock(&st_spinlock);
    ASSERT(res == 0);
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

    res = ethr_spin_unlock(&st_spinlock);
    ASSERT(res == 0);
    print_line("Main thread unlocked spinlock");

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    print_line("Main thread tries to lock spinlock");
    res = ethr_spin_lock(&st_spinlock);
    ASSERT(res == 0);
    print_line("Main thread locked spinlock");

    ASSERT(st_data == 1);

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(st_data == 1);

    res = ethr_spin_unlock(&st_spinlock);
    ASSERT(res == 0);
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
    int res;

    print_line("Aux thread tries to read lock rwspinlock");
    res = ethr_read_lock(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Aux thread read locked rwspinlock");

    ASSERT(rwst_data == 4711);

    print_line("Aux thread tries to read unlock rwspinlock");
    res = ethr_read_unlock(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Aux thread read unlocked rwspinlock");

    print_line("Aux thread tries to write lock rwspinlock");
    res = ethr_write_lock(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Aux thread write locked rwspinlock");

    data = ++rwst_data;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(rwst_data == data);
    ++rwst_data;

    print_line("Aux thread tries to write unlock rwspinlock");
    res = ethr_write_unlock(&rwst_rwspinlock);
    ASSERT(res == 0);
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
    res = ethr_read_lock(&rwst_rwspinlock);
    ASSERT(res == 0);
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
    res = ethr_read_unlock(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Main thread read unlocked rwspinlock");

    print_line("Main thread tries to write lock rwspinlock");
    res = ethr_write_lock(&rwst_rwspinlock);
    ASSERT(res == 0);
    print_line("Main thread write locked rwspinlock");

    data = ++rwst_data;

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rwst_data == data);
    ++rwst_data;

    print_line("Main thread tries to write unlock rwspinlock");
    res = ethr_write_unlock(&rwst_rwspinlock);
    ASSERT(res == 0);
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
    int res;

    print_line("Aux thread tries to read lock rwmutex");
    res = ethr_rwmutex_rlock(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Aux thread read locked rwmutex");

    ASSERT(rwmt_data == 4711);

    print_line("Aux thread tries to read unlock rwmutex");
    res = ethr_rwmutex_runlock(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Aux thread read unlocked rwmutex");

    print_line("Aux thread tries to write lock rwmutex");
    res = ethr_rwmutex_rwlock(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Aux thread write locked rwmutex");

    data = ++rwmt_data;
    print_line("Aux thread wrote");

    print_line("Aux thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Aux thread woke up");

    ASSERT(rwmt_data == data);
    ++rwmt_data;

    print_line("Aux thread tries to write unlock rwmutex");
    res = ethr_rwmutex_rwunlock(&rwmt_rwmutex);
    ASSERT(res == 0);
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
    res = ethr_rwmutex_rlock(&rwmt_rwmutex);
    ASSERT(res == 0);
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
    res = ethr_rwmutex_runlock(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Main thread read unlocked rwmutex");

    print_line("Main thread tries to write lock rwmutex");
    res = ethr_rwmutex_rwlock(&rwmt_rwmutex);
    ASSERT(res == 0);
    print_line("Main thread write locked rwmutex");

    data = ++rwmt_data;

    print_line("Main thread goes to sleep for 1 second");
    do_sleep(1);
    print_line("Main thread woke up");

    ASSERT(rwmt_data == data);
    ++rwmt_data;

    print_line("Main thread tries to write unlock rwmutex");
    res = ethr_rwmutex_rwunlock(&rwmt_rwmutex);
    ASSERT(res == 0);
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

#define AT_THREADS 4
#define AT_ITER 10000

long at_set_val, at_rm_val, at_max_val;

static ethr_atomic_t at_ready;
static ethr_atomic_t at_go;
static ethr_atomic_t at_done;
static ethr_atomic_t at_data;

void *
at_thread(void *unused)
{
    int res, i;
    long val, go;
    

    res = ethr_atomic_inctest(&at_ready, &val);
    ASSERT(res == 0);
    ASSERT(val > 0);
    ASSERT(val <= AT_THREADS);

    do {
	res = ethr_atomic_read(&at_go, &go);
	ASSERT(res == 0);
    } while (!go);

    for (i = 0; i < AT_ITER; i++) {
	res = ethr_atomic_or_old(&at_data, at_set_val, &val);
	ASSERT(res == 0);
	ASSERT(val >= (i == 0 ? 0 : at_set_val) + (long) 4711);
	ASSERT(val <= at_max_val);

	res = ethr_atomic_and_old(&at_data, ~at_rm_val, &val);
	ASSERT(res == 0);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	res = ethr_atomic_read(&at_data, &val);
	ASSERT(res == 0);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	res = ethr_atomic_inctest(&at_data, &val);
	ASSERT(res == 0);
	ASSERT(val > at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	res = ethr_atomic_dectest(&at_data, &val);
	ASSERT(res == 0);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);

	res = ethr_atomic_inc(&at_data);
	ASSERT(res == 0);

	res = ethr_atomic_dec(&at_data);
	ASSERT(res == 0);

	res = ethr_atomic_addtest(&at_data, (long) 4711, &val);
	ASSERT(res == 0);
	ASSERT(val >= at_set_val + (long) 2*4711);
	ASSERT(val <= at_max_val);

	res = ethr_atomic_add(&at_data, (long) -4711);
	ASSERT(res == 0);
	ASSERT(val >= at_set_val + (long) 4711);
	ASSERT(val <= at_max_val);
    }

    res = ethr_atomic_inc(&at_done);
    ASSERT(res == 0);
    return NULL;
}


static void
atomic_test(void)
{
    long data_init, data_final, val;
    int res, i;
    ethr_tid tid[AT_THREADS];
    ethr_thr_opts thr_opts = ETHR_THR_OPTS_DEFAULT_INITER;

    if (sizeof(long) > 4) {
	at_rm_val = ((long) 1) << 57;
	at_set_val = ((long) 1) << 60;
    }
    else {
	at_rm_val = ((long) 1) << 27;
	at_set_val = ((long) 1) << 30;
    }

    at_max_val = at_set_val + at_rm_val + ((long) AT_THREADS + 1) * 4711;
    data_init = at_rm_val + (long) 4711;
    data_final = at_set_val + (long) 4711;

    thr_opts.detached = 1;

    print_line("Initializing");
    res = ethr_atomic_init(&at_ready, 0);
    ASSERT(res == 0);
    res = ethr_atomic_init(&at_go, 0);
    ASSERT(res == 0);
    res = ethr_atomic_init(&at_done, data_init);
    ASSERT(res == 0);
    res = ethr_atomic_init(&at_data, data_init);
    ASSERT(res == 0);

    res = ethr_atomic_read(&at_data, &val);
    ASSERT(res == 0);
    ASSERT(val == data_init);
    res = ethr_atomic_set(&at_done, 0);
    ASSERT(res == 0);
    res = ethr_atomic_read(&at_done, &val);
    ASSERT(res == 0);
    ASSERT(val == 0);

    print_line("Creating threads");
    for (i = 0; i < AT_THREADS; i++) {
	res = ethr_thr_create(&tid[i], at_thread, NULL, &thr_opts);
	ASSERT(res == 0);
    }

    print_line("Waiting for threads to ready up");
    do {
	res = ethr_atomic_read(&at_ready, &val);
	ASSERT(res == 0);
	ASSERT(val >= 0);
	ASSERT(val <= AT_THREADS);
    } while (val != AT_THREADS);

    print_line("Letting threads loose");
    res = ethr_atomic_xchg(&at_go, 17, &val);
    ASSERT(res == 0);
    ASSERT(val == 0);
    res = ethr_atomic_read(&at_go, &val);
    ASSERT(res == 0);
    ASSERT(val == 17);


    print_line("Waiting for threads to finish");
    do {
	res = ethr_atomic_read(&at_done, &val);
	ASSERT(res == 0);
	ASSERT(val >= 0);
	ASSERT(val <= AT_THREADS);
    } while (val != AT_THREADS);

    print_line("Checking result");
    res = ethr_atomic_read(&at_data, &val);
    ASSERT(res == 0);
    ASSERT(val == data_final);
    print_line("Result ok");
    
}


/*
 * The gate test case.
 *
 * Tests gates.
 */

#define GT_THREADS 10

static ethr_atomic_t gt_wait1;
static ethr_atomic_t gt_wait2;
static ethr_atomic_t gt_done;

static ethr_gate gt_gate1;
static ethr_gate gt_gate2;

void *
gt_thread(void *thr_no)
{
    int no = (int)(long) thr_no;
    int swait = no % 2 == 0;
    int res;
    long done;


    do {

	res = ethr_atomic_inc(&gt_wait1);
	ASSERT(res == 0);

	if (swait)
	    res = ethr_gate_swait(&gt_gate1, INT_MAX);
	else
	    res = ethr_gate_wait(&gt_gate1);
	ASSERT(res == 0);

	res = ethr_atomic_dec(&gt_wait1);
	ASSERT(res == 0);

	res = ethr_atomic_inc(&gt_wait2);
	ASSERT(res == 0);

	if (swait)
	    res = ethr_gate_swait(&gt_gate2, INT_MAX);
	else
	    res = ethr_gate_wait(&gt_gate2);
	ASSERT(res == 0);

	res = ethr_atomic_dec(&gt_wait2);
	ASSERT(res == 0);

	res = ethr_atomic_read(&gt_done, &done);
	ASSERT(res == 0);
    } while (!done);
    return NULL;
}


static void
gate_test(void)
{
    long val;
    int res, i;
    ethr_tid tid[GT_THREADS];

    print_line("Initializing");
    res = ethr_atomic_init(&gt_wait1, 0);
    ASSERT_EQ(res, 0, "%d");
    res = ethr_atomic_init(&gt_wait2, 0);
    ASSERT_EQ(res, 0, "%d");
    res = ethr_atomic_init(&gt_done, 0);
    ASSERT_EQ(res, 0, "%d");
    res = ethr_gate_init(&gt_gate1);
    ASSERT_EQ(res, 0, "%d");
    res = ethr_gate_init(&gt_gate2);
    ASSERT_EQ(res, 0, "%d");

    print_line("Creating threads");
    for (i = 0; i < GT_THREADS; i++) {
	res = ethr_thr_create(&tid[i], gt_thread, (void *) i, NULL);
	ASSERT_EQ(res, 0, "%d");
    }

    print_line("Waiting for threads to ready up");
    do {
	res = ethr_atomic_read(&gt_wait1, &val);
	ASSERT_EQ(res, 0, "%d");
	ASSERT(0 <= val && val <= GT_THREADS);
    } while (val != GT_THREADS);

    print_line("Testing");

    res = ethr_gate_let_through(&gt_gate1, 8);
    ASSERT_EQ(res, 0, "%d");

    WAIT_UNTIL_LIM((res = ethr_atomic_read(&gt_wait2, &val),
		    (res != 0 || val == 8)),
		   60);

    res = ethr_atomic_read(&gt_wait1, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, GT_THREADS - 8, "%ld");

    res = ethr_atomic_read(&gt_wait2, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, 8, "%ld");
    
    res = ethr_gate_let_through(&gt_gate2, 4);
    ASSERT_EQ(res, 0, "%d");

    WAIT_UNTIL_LIM((res = ethr_atomic_read(&gt_wait2, &val),
		    (res != 0 || val == 4)),
		   60);

    res = ethr_atomic_read(&gt_wait1, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, GT_THREADS - 4, "%ld");

    res = ethr_atomic_read(&gt_wait2, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, 4, "%ld");
    
    res = ethr_gate_let_through(&gt_gate1, GT_THREADS);
    ASSERT_EQ(res, 0, "%d");

    WAIT_UNTIL_LIM((res = ethr_atomic_read(&gt_wait2, &val),
		    (res != 0 || val == GT_THREADS)),
		   60);
    res = ethr_atomic_read(&gt_wait1, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, 0, "%ld");

    res = ethr_atomic_read(&gt_wait2, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, GT_THREADS, "%ld");
    
    res = ethr_gate_let_through(&gt_gate2, GT_THREADS);
    ASSERT_EQ(res, 0, "%d");

    WAIT_UNTIL_LIM((res = ethr_atomic_read(&gt_wait2, &val),
		    (res != 0 || val == 4)),
		   60);
    res = ethr_atomic_read(&gt_wait1, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, GT_THREADS - 4, "%ld");

    res = ethr_atomic_read(&gt_wait2, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, 4, "%ld");
    
    res = ethr_atomic_set(&gt_done, 1);
    ASSERT_EQ(res, 0, "%d");

    res = ethr_gate_let_through(&gt_gate2, GT_THREADS);
    ASSERT_EQ(res, 0, "%d");
    res = ethr_gate_let_through(&gt_gate1, GT_THREADS - 4);
    ASSERT_EQ(res, 0, "%d");

    WAIT_UNTIL_LIM(((res = ethr_atomic_read(&gt_wait1, &val)) != 0
		    || (val == 0
			&& ((res = ethr_atomic_read(&gt_wait2, &val)) != 0
			    || val == 0))),
		   60);

    res = ethr_atomic_read(&gt_wait1, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, 0, "%ld");

    res = ethr_atomic_read(&gt_wait2, &val);
    ASSERT_EQ(res, 0, "%d");
    ASSERT_EQ(val, 0, "%ld");
    
    print_line("Joining threads");
    for (i = 0; i < GT_THREADS; i++) {
	res = ethr_thr_join(tid[i], NULL);
	ASSERT_EQ(res, 0, "%d");
    }

    res = ethr_gate_destroy(&gt_gate1);
    ASSERT_EQ(res, 0, "%d");
    res = ethr_gate_destroy(&gt_gate2);
    ASSERT_EQ(res, 0, "%d");

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
	int res;

	send_my_pid();

	testcase = argv[1];
	res = ethr_init(NULL);

	if (res != 0)
	    fail("Failed to initialize the ethread library");

	if (strcmp(testcase, "create_join_thread") == 0)
	    create_join_thread_test();
	else if (strcmp(testcase, "equal_tids") == 0)
	    equal_tids_test();
	else if (strcmp(testcase, "mutex") == 0)
	    mutex_test();
	else if (strcmp(testcase, "try_lock_mutex") == 0)
	    try_lock_mutex_test();
	else if (strcmp(testcase, "recursive_mutex") == 0)
	    recursive_mutex_test();
	else if (strcmp(testcase, "time_now") == 0)
	    time_now_test();
	else if (strcmp(testcase, "cond_wait") == 0)
	    cond_wait_test(0);
	else if (strcmp(testcase, "cond_timedwait") == 0)
	    cond_timedwait_test();
	else if (strcmp(testcase, "broadcast") == 0)
	    broadcast_test();
	else if (strcmp(testcase, "detached_thread") == 0)
	    detached_thread_test();
	else if (strcmp(testcase, "max_threads") == 0)
	    max_threads_test();
	else if (strcmp(testcase, "forksafety") == 0)
	    forksafety_test();
	else if (strcmp(testcase, "vfork") == 0)
	    vfork_test(argc, argv);
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
	else if (strcmp(testcase, "gate") == 0)
	    gate_test();
	else
	    skip("Test case \"%s\" not implemented yet", testcase);

	succeed(NULL);
    }
#else /* #ifndef ETHR_NO_THREAD_LIB */
    skip("No ethread library to test");
#endif /* #ifndef ETHR_NO_THREAD_LIB */

    return 0;
}
