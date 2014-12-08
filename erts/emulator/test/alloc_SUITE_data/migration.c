/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014. All Rights Reserved.
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

void
testcase_cleanup(TestCaseState_t *tcs)
{
}

#define MAX_BLOCK_PER_THR 100
#define MAX_ROUNDS 10

typedef struct MyBlock_ {
    struct MyBlock_* next;
    struct MyBlock_** prevp;
} MyBlock;

typedef struct {
    MyBlock* blockv[MAX_BLOCK_PER_THR];
    enum { GROWING, SHRINKING, CLEANUP, DONE } phase;
    int ix;
    int round;
} MigrationState;

typedef struct {
    ErlDrvMutex* mtx;
    int nblocks;
    MyBlock* first;
} MyCrrInfo;


static int crr_info_offset = -1;
static void (*orig_create_mbc_fn)(Allctr_t *allctr, Carrier_t *carrier);
static void (*orig_destroying_mbc_fn)(Allctr_t *allctr, Carrier_t *carrier);

static void my_creating_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    MyCrrInfo* mci = (MyCrrInfo*) ((char*)carrier + crr_info_offset);
    if (orig_create_mbc_fn)
	orig_create_mbc_fn(allctr, carrier);

    mci->mtx = erl_drv_mutex_create("alloc_SUITE.migration");
    mci->nblocks = 0;
    mci->first = NULL;
}

static void my_destroying_mbc(Allctr_t *allctr, Carrier_t *carrier)
{
    MyCrrInfo* mci = (MyCrrInfo*) ((char*)carrier + crr_info_offset);

    FATAL_ASSERT(mci->nblocks == 0);
    FATAL_ASSERT(mci->first == NULL);
    erl_drv_mutex_destroy(mci->mtx);

    if (orig_destroying_mbc_fn)
	orig_destroying_mbc_fn(allctr, carrier);
}


static void setup(TestCaseState_t* tcs)
{
    void* creating_mbc_arg = (void*)my_creating_mbc;
    void* destroying_mbc_arg = (void*)my_destroying_mbc;
    crr_info_offset = SET_TEST_MBC_USER_HEADER(sizeof(MyCrrInfo),
					       &creating_mbc_arg,
					       &destroying_mbc_arg);
    ASSERT(tcs, crr_info_offset >= 0);
    orig_create_mbc_fn = creating_mbc_arg;
    orig_destroying_mbc_fn = destroying_mbc_arg;
}

static void add_block(MyBlock* p)
{
    MyCrrInfo* mci = (MyCrrInfo*)((char*)BLK_TO_MBC(UMEM2BLK_TEST(p)) + crr_info_offset);

    erl_drv_mutex_lock(mci->mtx);
    mci->nblocks++;
    p->next = mci->first;
    p->prevp = &mci->first;
    mci->first = p;
    if (p->next)
	p->next->prevp = &p->next;
    erl_drv_mutex_unlock(mci->mtx);
}

static void remove_block(MyBlock* p)
{
    MyCrrInfo* mci = (MyCrrInfo*)((char*)BLK_TO_MBC(UMEM2BLK_TEST(p)) + crr_info_offset);

    erl_drv_mutex_lock(mci->mtx);
    mci->nblocks--;
    if (p->next)
	p->next->prevp = p->prevp;
    *p->prevp = p->next;
    erl_drv_mutex_unlock(mci->mtx);
}

void
testcase_run(TestCaseState_t *tcs)
{
    MigrationState* state = (MigrationState*) tcs->extra;

    if (tcs->command_len == 4
	&& memcmp(tcs->command, "init", tcs->command_len) == 0) {
	setup(tcs);
	return;
    }

    if (!tcs->extra) {
	if (!IS_SMP_ENABLED)
	    testcase_skipped(tcs, "No SMP support");

	tcs->extra = driver_alloc(sizeof(MigrationState));
	state = (MigrationState*) tcs->extra;
	memset(state->blockv, 0, sizeof(state->blockv));
	state->phase = GROWING;
	state->ix = 0;
	state->round = 0;
    }

    switch (state->phase) {
    case GROWING: {
	MyBlock* p;
	FATAL_ASSERT(!state->blockv[state->ix]);
	p = ALLOC_TEST((1 << 18) / 5);
	FATAL_ASSERT(p);
	add_block(p);
	state->blockv[state->ix] = p;
	do {
	    if (++state->ix >= MAX_BLOCK_PER_THR) {
		state->phase = SHRINKING;
		state->ix = 0;
		break;
	    }
	} while (state->blockv[state->ix] != NULL);
	break;
    }
    case SHRINKING:
	FATAL_ASSERT(state->blockv[state->ix]);
	remove_block(state->blockv[state->ix]);
	FREE_TEST(state->blockv[state->ix]);
	state->blockv[state->ix] = NULL;

	state->ix += 1 + ((state->ix % 3) == 0);
	if (state->ix >= MAX_BLOCK_PER_THR) {
	    if (++state->round >= MAX_ROUNDS) {
		state->phase = CLEANUP;
	    } else {
		state->phase = GROWING;
	    }
	    state->ix = 0;
	}
	break;

    case CLEANUP:
	if (state->blockv[state->ix]) {
	    remove_block(state->blockv[state->ix]);
	    FREE_TEST(state->blockv[state->ix]);
	}
	if (++state->ix >= MAX_BLOCK_PER_THR)
	    state->phase = DONE;
	break;

    default:
	FATAL_ASSERT(!"Invalid phase");
    }

    if (state->phase == DONE) {
	driver_free(tcs->extra);
	tcs->extra = NULL;
    }
    else
	testcase_continue(tcs);
}
