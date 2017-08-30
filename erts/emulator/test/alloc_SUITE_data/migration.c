/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
 * Test the carrier migration logic
 */

#ifndef __WIN32__
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#endif
#include <stdio.h>
#include <stdlib.h>
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


char *
testcase_name(void)
{
    return "migration";
}

/* Turns out random_r() is a nonstandard glibc extension.
#define HAVE_RANDOM_R
*/
#ifdef HAVE_RANDOM_R

typedef struct { struct random_data rnd; char rndbuf[32]; } MyRandState;

static void myrand_init(MyRandState* mrs, unsigned int seed)
{
    int res;
    memset(&mrs->rnd, 0, sizeof(mrs->rnd));
    res = initstate_r(seed, mrs->rndbuf, sizeof(mrs->rndbuf), &mrs->rnd);
    FATAL_ASSERT(res == 0);
}

static int myrand(MyRandState* mrs)
{
    int32_t x;
    int res = random_r(&mrs->rnd, &x);
    FATAL_ASSERT(res == 0);
    return (int)x;
}

#else /* !HAVE_RANDOM_R */

typedef unsigned int MyRandState;

static void myrand_init(MyRandState* mrs, unsigned int seed)
{
    *mrs = seed;
}

static int myrand(MyRandState* mrs)
{
    /* Taken from rand(3) man page.
     * Modified to return a full 31-bit value by using low half of *mrs as well.
     */
    *mrs = (*mrs) * 1103515245 + 12345;
    return (int) (((*mrs >> 16) | (*mrs << 16)) & ~(1 << 31));
}

#endif /* !HAVE_RANDOM_R */

#define MAX_BLOCK_PER_THR 200
#define BLOCKS_PER_MBC 10
#define MAX_ROUNDS 10000

typedef struct MyBlock_ {
    struct MyBlock_* next;
    struct MyBlock_** prevp;
} MyBlock;

typedef struct {
    MyBlock* blockv[MAX_BLOCK_PER_THR];
    MyRandState rand_state;
    enum { GROWING, SHRINKING, CLEANUP, DONE } phase;
    int nblocks;
    int goal_nblocks;
    int round;
    int nr_of_migrations;
    int nr_of_carriers;
    int max_blocks_in_mbc;
    int block_size;
    int max_nblocks;
} MigrationState;

typedef struct {
    ErlNifMutex* mtx;
    int nblocks;
    MyBlock* first;
    MigrationState* employer;
} MyCrrInfo;


static int crr_info_offset = -1;
static void (*orig_create_mbc_fn)(Allctr_t *allctr, Carrier_t *carrier);
static void (*orig_destroying_mbc_fn)(Allctr_t *allctr, Carrier_t *carrier);

static void my_creating_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    MyCrrInfo* mci = (MyCrrInfo*) ((char*)carrier + crr_info_offset);
    if (orig_create_mbc_fn)
	orig_create_mbc_fn(allctr, carrier);

    mci->mtx = enif_mutex_create("alloc_SUITE.migration");
    mci->nblocks = 0;
    mci->first = NULL;
    mci->employer = NULL;
}

static void my_destroying_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    MyCrrInfo* mci = (MyCrrInfo*) ((char*)carrier + crr_info_offset);

    FATAL_ASSERT(mci->nblocks == 0);
    FATAL_ASSERT(mci->first == NULL);
    enif_mutex_destroy(mci->mtx);

    if (orig_destroying_mbc_fn)
	orig_destroying_mbc_fn(allctr, carrier);
}

static int migration_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    void* creating_mbc_arg = (void*)my_creating_mbc;
    void* destroying_mbc_arg = (void*)my_destroying_mbc;

    if (testcase_nif_init(env, priv_data, load_info))
	return -1;

    crr_info_offset = SET_TEST_MBC_USER_HEADER(sizeof(MyCrrInfo),
					       &creating_mbc_arg,
					       &destroying_mbc_arg);
    FATAL_ASSERT(crr_info_offset >= 0);
    orig_create_mbc_fn = creating_mbc_arg;
    orig_destroying_mbc_fn = destroying_mbc_arg;

    return 0;
}

static void add_block(MyBlock* p, MigrationState* state)
{
    MyCrrInfo* mci = (MyCrrInfo*)((char*)BLK_TO_MBC(UMEM2BLK_TEST(p)) + crr_info_offset);

    enif_mutex_lock(mci->mtx);
    if (++mci->nblocks > state->max_blocks_in_mbc)
	state->max_blocks_in_mbc = mci->nblocks;
    p->next = mci->first;
    p->prevp = &mci->first;
    mci->first = p;
    if (p->next)
	p->next->prevp = &p->next;
    if (mci->employer != state) {
	if (!mci->employer) {
	    FATAL_ASSERT(mci->nblocks == 1);
	    state->nr_of_carriers++;
	}
	else {
	    state->nr_of_migrations++;
	}
	mci->employer = state;
    }
    enif_mutex_unlock(mci->mtx);
}

static void remove_block(MyBlock* p)
{
    MyCrrInfo* mci = (MyCrrInfo*)((char*)BLK_TO_MBC(UMEM2BLK_TEST(p)) + crr_info_offset);

    enif_mutex_lock(mci->mtx);
    mci->nblocks--;
    if (p->next)
	p->next->prevp = p->prevp;
    *p->prevp = p->next;
    enif_mutex_unlock(mci->mtx);
}

static int rand_int(MigrationState* state, int low, int high)
{
    int x;
    FATAL_ASSERT(high >= low);
    x = myrand(&state->rand_state);
    return low + (x % (high+1-low));
}


static void do_cleanup(TestCaseState_t *tcs, MigrationState* state)
{
    if (state->nblocks == 0) {
	state->phase = DONE;
	testcase_printf(tcs, "%d: Done %d rounds", tcs->thr_nr, state->round);
	testcase_printf(tcs, "%d: Cleanup all blocks", tcs->thr_nr);
	testcase_printf(tcs, "%d: Empty carriers detected = %d", tcs->thr_nr,
			state->nr_of_carriers);
	testcase_printf(tcs, "%d: Migrations detected     = %d", tcs->thr_nr,
			state->nr_of_migrations);
	testcase_printf(tcs, "%d: Max blocks in carrier   = %d", tcs->thr_nr,
			state->max_blocks_in_mbc);
    }
    else {
	state->nblocks--;
	if (state->blockv[state->nblocks]) {
	    remove_block(state->blockv[state->nblocks]);
	    FREE_TEST(state->blockv[state->nblocks]);
	}
    }
}


void
testcase_run(TestCaseState_t *tcs)
{
    MigrationState* state = (MigrationState*) tcs->extra;

    if (!tcs->extra) {
	if (!IS_SMP_ENABLED)
	    testcase_skipped(tcs, "No SMP support");

	tcs->extra = enif_alloc(sizeof(MigrationState));
	state = (MigrationState*) tcs->extra;
	memset(state->blockv, 0, sizeof(state->blockv));
	myrand_init(&state->rand_state, tcs->thr_nr);
	state->phase = GROWING;
	state->nblocks = 0;
	state->round = 0;
	state->nr_of_migrations = 0;
	state->nr_of_carriers = 0;
	state->max_blocks_in_mbc = 0;
	state->block_size = GET_TEST_MBC_SIZE() / (BLOCKS_PER_MBC+1);
	if (MAX_BLOCK_PER_THR * state->block_size < tcs->free_mem) {
	    state->max_nblocks = MAX_BLOCK_PER_THR;
	} else {
	    state->max_nblocks = tcs->free_mem / state->block_size;
	}
	state->goal_nblocks = rand_int(state, 1, state->max_nblocks);
    }

    switch (state->phase) {
    case GROWING: {
	MyBlock* p;
	FATAL_ASSERT(!state->blockv[state->nblocks]);
	p = ALLOC_TEST(rand_int(state, state->block_size/2, state->block_size));
	FATAL_ASSERT(p);
	add_block(p, state);
	state->blockv[state->nblocks] = p;
	if (++state->nblocks >= state->goal_nblocks) {
	    /*testcase_printf(tcs, "%d: Grown to %d blocks", tcs->thr_nr, state->nblocks);*/
	    state->phase = SHRINKING;
	    state->goal_nblocks = rand_int(state, 0, state->goal_nblocks-1);
	}
	else
	    FATAL_ASSERT(!state->blockv[state->nblocks]);
	break;
    }
    case SHRINKING: {
	int ix = rand_int(state, 0, state->nblocks-1);
	FATAL_ASSERT(state->blockv[ix]);
	remove_block(state->blockv[ix]);
	FREE_TEST(state->blockv[ix]);
	state->blockv[ix] = state->blockv[--state->nblocks];
	state->blockv[state->nblocks] = NULL;

	if (state->nblocks <= state->goal_nblocks) {
	    /*testcase_printf(tcs, "%d: Shrunk to %d blocks", tcs->thr_nr, state->nblocks);*/
	    if (++state->round >= MAX_ROUNDS) {
		state->phase = CLEANUP;
	    } else {
		state->phase = GROWING;
		state->goal_nblocks = rand_int(state, state->goal_nblocks+1, state->max_nblocks);
	    }
	}
	break;
    }
    case CLEANUP:
	do_cleanup(tcs, state);
	break;

    default:
	FATAL_ASSERT(!"Invalid phase");
    }

    if (state->phase == DONE) {
    }
    else {
	testcase_continue(tcs);
    }
}

void
testcase_cleanup(TestCaseState_t *tcs)
{
    MigrationState* state = (MigrationState*) tcs->extra;

    while (state->phase != DONE)
	do_cleanup(tcs, state);

    enif_free(tcs->extra);
    tcs->extra = NULL;
}


ERL_NIF_INIT(migration, testcase_nif_funcs, migration_init,
	     NULL, NULL, NULL);
