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

#ifdef __WIN32__
#undef HAVE_VSNPRINTF
#define HAVE_VSNPRINTF 1
#define vsnprintf _vsnprintf
#endif

#ifndef HAVE_VSNPRINTF
#define HAVE_VSNPRINTF 0
#endif

#define NO_OF_ALLOC_SEQS 6
#define NO_OF_THREADS (18)
#define NO_OF_BLOCKS 10
#define NO_OF_OPS_PER_BL 200
#define SBC_THRESHOLD 8192

#define BLOCK_ID(TN, BN) ((TN) << 4 | (BN))

#define ERR_BUF_SZ 4096
static char err_buf[ERR_BUF_SZ];
static volatile int tc_failed;
static int dead_thread_no;
static erts_mutex tc_mutex;
static erts_cond tc_cond;

static void exit_thread(int t_no, int do_lock)
{
    if (do_lock)
	THR_MTX_LOCK(tc_mutex);

    while (dead_thread_no >= 0)
	THR_COND_WAIT(tc_cond, tc_mutex);
    dead_thread_no = t_no;

    THR_COND_BCAST(tc_cond);
    THR_MTX_UNLOCK(tc_mutex);
    THR_EXIT(NULL);
}

static void fail(int t_no, char *frmt, ...)
{
    char buf[10];
    size_t bufsz = sizeof(buf);
    va_list va;

    THR_MTX_LOCK(tc_mutex);

    va_start(va, frmt);
#if HAVE_VSNPRINTF
    vsnprintf(err_buf, ERR_BUF_SZ, frmt, va);
#else
    vsprintf(err_buf, frmt, va);
#endif
    va_end(va);

    tc_failed = 1;

    if (enif_getenv("ERL_ABORT_ON_FAILURE", buf, &bufsz) == 0
	&& strcmp("true", buf) == 0) {
	fprintf(stderr, "Testcase \"%s\" failed: %s\n",
		testcase_name(), err_buf);
	abort();
    }

    exit_thread(t_no, 0);
}

static Allctr_t *alloc_ts_1 = NULL;
static Allctr_t *alloc_ts_2 = NULL;

static void stop_allocators(void)
{
    if (alloc_ts_1) {
	STOP_ALC(alloc_ts_1);
	alloc_ts_1 = NULL;
    }
    if (alloc_ts_2) {
	STOP_ALC(alloc_ts_2);
	alloc_ts_2 = NULL;
    }
}


void *thread_func(void *arg);

typedef struct {
    Allctr_t *a;
    int t_no;
    int no_ops_per_bl;
} ThreadData;


char *
testcase_name(void)
{
    return "threads";
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    stop_allocators();
}

void
testcase_run(TestCaseState_t *tcs)
{
    struct {
	erts_thread tid;
	ThreadData arg;
    } threads[NO_OF_THREADS+1] = {{0}};
    int no_threads;
    int i;
    char sbct_buf[10];
    char *argv_org[] = {"-tasaobf", "-tmmsbc5000", "-tmmmbc5000", "-tsbct",
			&sbct_buf[0], NULL};
    char *argv[sizeof(argv_org)/sizeof(argv_org[0])];

    if (!IS_THREADS_ENABLED)
	testcase_skipped(tcs, "Threads not enabled");

    alloc_ts_1 = NULL;
    alloc_ts_2 = NULL;

    err_buf[0] = '\0';

    sprintf(sbct_buf, "%d", SBC_THRESHOLD/1024);
    
    memcpy((void *) argv, argv_org, sizeof(argv_org));
    alloc_ts_1 = START_ALC("threads_ts_1", 1, argv);
    ASSERT(tcs, alloc_ts_1);
    memcpy((void *) argv, argv_org, sizeof(argv_org));
    alloc_ts_2 = START_ALC("threads_ts_2", 1, argv);
    ASSERT(tcs, alloc_ts_2);

    ASSERT(tcs, IS_ALLOC_THREAD_SAFE(alloc_ts_1));
    ASSERT(tcs, IS_ALLOC_THREAD_SAFE(alloc_ts_2));

    tc_mutex = THR_MTX_CREATE();
    tc_cond = THR_COND_CREATE();

    THR_MTX_LOCK(tc_mutex);

    dead_thread_no = -1;
    no_threads = 0;

    for(i = 1; i <= NO_OF_THREADS; i++) {
	char *alc;

	threads[i].arg.no_ops_per_bl = NO_OF_OPS_PER_BL;

	if (i % 2 == 0) {
	    alc = "threads_ts_1";
	    threads[i].arg.a = alloc_ts_1;
	}
	else {
	    alc = "threads_ts_2";
	    threads[i].arg.a = alloc_ts_2;
	}
	threads[i].arg.t_no = i;

	threads[i].tid = THR_CREATE(thread_func, (void *) &threads[i].arg);
	if (threads[i].tid) {
	    testcase_printf(tcs, "Successfully created thread %d "
			    "using %s_alloc\n", i, alc);
	    no_threads++;
	}
	else {
	    tc_failed = 1;
	    sprintf(err_buf, "Failed to create thread %d\n", i);
	    break;
	}

    }

    while (no_threads) {
	THR_COND_WAIT(tc_cond, tc_mutex);
	if (dead_thread_no >= 0) {
	    no_threads--;
	    THR_JOIN(threads[dead_thread_no].tid);
	    testcase_printf(tcs, "Thread %d died\n", dead_thread_no);
	    dead_thread_no = -1;
	    THR_COND_BCAST(tc_cond);
	}
    }

    THR_MTX_UNLOCK(tc_mutex);
    THR_MTX_DESTROY(tc_mutex);
    THR_COND_DESTROY(tc_cond);

    stop_allocators();

    if (tc_failed)
	testcase_failed(tcs, "%s", err_buf);
}

Ulong alloc_seq_1[] = {
  17,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*20,
  SBC_THRESHOLD*2,
  17,
  0
};

Ulong alloc_seq_2[] = {
  SBC_THRESHOLD*20,
  SBC_THRESHOLD*2,
  17,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*20,
  0
};

Ulong alloc_seq_3[] = {
  1,
  SBC_THRESHOLD/10,
  SBC_THRESHOLD/9,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD/1,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*9,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD*9,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/9,
  SBC_THRESHOLD/10,
  1,
  0
};

Ulong alloc_seq_4[] = {
  SBC_THRESHOLD*2,
  SBC_THRESHOLD*3,
  SBC_THRESHOLD*7,
  SBC_THRESHOLD*8,
  SBC_THRESHOLD*5,
  SBC_THRESHOLD*6,
  SBC_THRESHOLD*1,
  SBC_THRESHOLD*10,
  SBC_THRESHOLD*4,
  SBC_THRESHOLD*2,
  0
};

Ulong alloc_seq_5[] = {
  SBC_THRESHOLD/2,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/1,
  SBC_THRESHOLD/10,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/2,
  SBC_THRESHOLD/3,
  SBC_THRESHOLD/7,
  SBC_THRESHOLD/8,
  SBC_THRESHOLD/5,
  SBC_THRESHOLD/6,
  SBC_THRESHOLD/1,
  SBC_THRESHOLD/10,
  SBC_THRESHOLD/4,
  SBC_THRESHOLD/2,
  0
};

Ulong alloc_seq_6[] = {
    1, 50, 100, 50, 23, 46, 2345, 23, 54, 2, 0
};

Ulong *alloc_seqs[NO_OF_ALLOC_SEQS] = {
  alloc_seq_1,
  alloc_seq_2,
  alloc_seq_3,
  alloc_seq_4,
  alloc_seq_5,
  alloc_seq_6
};

typedef struct {
    unsigned char *p;
    Ulong  s;
    int   i;
    Ulong *as;
} block;

#define CHECK_BLOCK_DATA(T, P, S, D) \
  check_block_data(__FILE__, __LINE__, (T), (P), (S), (D))

static void
check_block_data(char *file, int line, int t_no,
		 unsigned char *p, Ulong sz, int d)
{
    Ulong i;
    for (i = 0; i < sz; i++)
	if (p[i] != (unsigned char) d)
	    fail(t_no, "%s:%d: Thread no %d found clobbered data! "
		 "found id=%d; expected id=%d\n",
		 file, line, t_no, (int) p[i], d);
}

static void
alloc_op(int t_no, Allctr_t *a, block *bp, int id, int clean_up)
{
    if (tc_failed)
	exit_thread(t_no, 1);

    if(bp->p)
	CHECK_BLOCK_DATA(t_no, bp->p, bp->s, id);

    if(bp->as[bp->i] == 0 || clean_up) {
	FREE(a, bp->p);
	bp->p = NULL;
	bp->s = 0;
	bp->i = 0; /* start from the beginning again */
	return;
    }

    if(!bp->p) {
	bp->s = bp->as[bp->i];
	bp->p = (unsigned char *) ALLOC(a, bp->s);
	if(!bp->p)
	    fail(t_no, "ALLOC(%lu) failed [id=%d])\n", bp->s, id);
	memset((void *) bp->p, (unsigned char)id, (size_t) bp->s);
    }
    else {
	unsigned char *p = (unsigned char *) REALLOC(a, bp->p, bp->as[bp->i]);
	if(!p)
	    fail(t_no, "REALLOC(0x%lx, %lu) failed [id=%d]\n",
		 (Ulong) bp->p, bp->as[bp->i], id);

	if(bp->s < bp->as[bp->i]) {
	    CHECK_BLOCK_DATA(t_no, p, bp->s, id);
	    memset((void *) p, (unsigned char)id, (size_t) bp->as[bp->i]);
	}
	else
	    CHECK_BLOCK_DATA(t_no, p, bp->as[bp->i], id);

	bp->s = bp->as[bp->i];
	bp->p = p;
    }

    bp->i++;
}


void *
thread_func(void *arg)
{
    int i, j;
    ThreadData *td = ((ThreadData *) arg);
    block bs[NO_OF_BLOCKS];

    for(i = 0; i < NO_OF_BLOCKS; i++) {
	bs[i].p = NULL;
	bs[i].s = 0;
	bs[i].i = 0;
	bs[i].as = alloc_seqs[i % NO_OF_ALLOC_SEQS];
    }

    for(i = 0; i < td->no_ops_per_bl; i++) {

	for(j = 0; j < NO_OF_BLOCKS; j++)
	    alloc_op(td->t_no, td->a, &bs[j], BLOCK_ID(td->t_no, j), 0);
    }

    for(j = 0; j < NO_OF_BLOCKS; j++)
	alloc_op(td->t_no, td->a, &bs[j], BLOCK_ID(td->t_no, j), 1);

    exit_thread(td->t_no, 1);
    return NULL;
}

ERL_NIF_INIT(threads, testcase_nif_funcs, testcase_nif_init,
	     NULL, NULL, NULL);
