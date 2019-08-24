/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013-2016. All Rights Reserved.
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


#ifndef __WIN32__
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "testcase_driver.h"
#include "allocator_test.h"

#define FATAL_ASSERT(A)						\
    ((void) ((A)						\
	     ? 1						\
	     : (fatal_assert_failed(#A,				\
				    (char *) __FILE__,		\
				    __LINE__),			\
		0)))

static void
fatal_assert_failed(char* expr, char* file, int line)
{
    fflush(stdout);
    fprintf(stderr, "%s:%d: Assertion failed: %s\n",
	    file, line, expr);
    fflush(stderr);
    abort();
}

#define TEST_NO_THREADS 10
#define TEST_NO_CARRIERS_PER_THREAD 100000
#define TEST_CARRIERS_OFFSET 5

static Allctr_t *alloc = NULL;

static void stop_allocator(void)
{
    if (alloc) {
	STOP_ALC(alloc);
	alloc = NULL;
    }
}


void *thread_func(void *arg);

char *
testcase_name(void)
{
    return "cpool";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    stop_allocator();
}

void *
thread_func(void *arg)
{
    Carrier_t *crr = (Carrier_t *) arg;
    int i;

    for (i = 0; i < (TEST_NO_CARRIERS_PER_THREAD+TEST_CARRIERS_OFFSET); i++) {
	int d;
	if (i < TEST_NO_CARRIERS_PER_THREAD) {
	    (void) CPOOL_INSERT(alloc, crr[i]);
	    if ((i & 0x7) == 0)
		FATAL_ASSERT(CPOOL_IS_IN_POOL(alloc, crr[i]));
	}
	d = i-TEST_CARRIERS_OFFSET;
	if (d >= 0) {
	    (void) CPOOL_DELETE(alloc, crr[d]);
	    if ((d & 0x7) == 0)
		FATAL_ASSERT(!CPOOL_IS_IN_POOL(alloc, crr[d]));
	}
    }
    for (i = 0; i < TEST_NO_CARRIERS_PER_THREAD; i++)
	FATAL_ASSERT(!CPOOL_IS_IN_POOL(alloc, crr[i]));
    return NULL;
}

static struct {
    erts_thread tid;
    Carrier_t *crr[TEST_NO_CARRIERS_PER_THREAD];
} threads[TEST_NO_THREADS] = {{0}};

void
testcase_run(TestCaseState_t *tcs)
{
    int no_threads, t, c;
    char *block, *p;
    Ulong zcrr_sz;

    if (!IS_SMP_ENABLED)
	testcase_skipped(tcs, "No SMP support");

    alloc = START_ALC("Zero carrier allocator", 1, NULL);

    zcrr_sz = ZERO_CRR_SIZE;

    block = p = ALLOC(alloc, zcrr_sz*TEST_NO_THREADS*TEST_NO_CARRIERS_PER_THREAD);

    ASSERT(tcs, block != NULL);    

    for (t = 0; t < TEST_NO_THREADS; t++) {
	for (c = 0; c < TEST_NO_CARRIERS_PER_THREAD; c++) {
	    Carrier_t *crr = (Carrier_t *) p;
	    p += zcrr_sz;
	    (void) ZERO_CRR_INIT(alloc, crr);
	    threads[t].crr[c] = crr;
	}
    }

    no_threads = 0;
    for (t = 0; t < TEST_NO_THREADS; t++) {
    	threads[t].tid = THR_CREATE(thread_func, (void *) threads[t].crr);
	if (threads[t].tid) {
	    testcase_printf(tcs, "Successfully created thread %d\n", t);
	    no_threads++;
	}
	else {
	    testcase_printf(tcs, "Failed to create thread %d\n", t);
	    break;
	}
    }

    for (t = 0; t < no_threads; t++)
	THR_JOIN(threads[t].tid);

    FATAL_ASSERT(CPOOL_IS_EMPTY(alloc));

    FREE(alloc, block);

    ASSERT(tcs, no_threads == TEST_NO_THREADS);
}

ERL_NIF_INIT(cpool, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
