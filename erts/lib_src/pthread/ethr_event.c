/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
 * Author: Rickard Green
 */

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHR_EVENT_IMPL__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ethread.h"

#if defined(ETHR_LINUX_FUTEX_IMPL__)
/* --- Linux futex implementation of ethread events ------------------------- */

#include <sched.h>
#include <errno.h>

#define ETHR_YIELD_AFTER_BUSY_LOOPS 50

int
ethr_event_init(ethr_event *e)
{
    ethr_atomic32_init(&e->futex, ETHR_EVENT_OFF__);
    return 0;
}

int
ethr_event_destroy(ethr_event *e)
{
    return 0;
}

static ETHR_INLINE int
wait__(ethr_event *e, int spincount)
{
    unsigned sc = spincount;
    int res;
    ethr_sint32_t val;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;

    if (spincount < 0)
	ETHR_FATAL_ERROR__(EINVAL);

    while (1) {
	while (1) {
	    val = ethr_atomic32_read(&e->futex);
	    if (val == ETHR_EVENT_ON__)
		return 0;
	    if (sc == 0)
		break;
	    sc--;
	    ETHR_SPIN_BODY;
	    if (--until_yield == 0) {
		until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
		res = ETHR_YIELD();
		if (res != 0)
		    ETHR_FATAL_ERROR__(res);
	    }
	}

	if (val != ETHR_EVENT_OFF_WAITER__) {
	    val = ethr_atomic32_cmpxchg(&e->futex,
					ETHR_EVENT_OFF_WAITER__,
					ETHR_EVENT_OFF__);

	    if (val == ETHR_EVENT_ON__)
		return 0;
	    ETHR_ASSERT(val == ETHR_EVENT_OFF__);
	}

	res = ETHR_FUTEX__(&e->futex,
			   ETHR_FUTEX_WAIT__,
			   ETHR_EVENT_OFF_WAITER__);
	if (res == EINTR)
	    break;
	if (res != 0 && res != EWOULDBLOCK)
	    ETHR_FATAL_ERROR__(res);
    }

    return res;
}

#elif defined(ETHR_PTHREADS)
/* --- Posix mutex/cond implementation of events ---------------------------- */

int
ethr_event_init(ethr_event *e)
{
    int res;
    ethr_atomic32_init(&e->state, ETHR_EVENT_OFF__);
    res = pthread_mutex_init(&e->mtx, NULL);
    if (res != 0)
	return res;
    res = pthread_cond_init(&e->cnd, NULL);
    if (res != 0) {
	pthread_mutex_destroy(&e->mtx);
	return res;
    }
    return 0;
}

int
ethr_event_destroy(ethr_event *e)
{
    int res;
    res = pthread_mutex_destroy(&e->mtx);
    if (res != 0)
	return res;
    res = pthread_cond_destroy(&e->cnd);
    if (res != 0)
	return res;
    return 0;
}

static ETHR_INLINE int
wait__(ethr_event *e, int spincount)
{
    int sc = spincount;
    ethr_sint32_t val;
    int res, ulres;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;

    if (spincount < 0)
	ETHR_FATAL_ERROR__(EINVAL);

    while (1) {
	val = ethr_atomic32_read(&e->state);
	if (val == ETHR_EVENT_ON__)
	    return 0;
	if (sc == 0)
	    break;
	sc--;
	ETHR_SPIN_BODY;
	if (--until_yield == 0) {
	    until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
	    res = ETHR_YIELD();
	    if (res != 0)
		ETHR_FATAL_ERROR__(res);
	}
    }

    if (val != ETHR_EVENT_OFF_WAITER__) {
	val = ethr_atomic32_cmpxchg(&e->state,
				    ETHR_EVENT_OFF_WAITER__,
				    ETHR_EVENT_OFF__);
	if (val == ETHR_EVENT_ON__)
	    return 0;
	ETHR_ASSERT(val == ETHR_EVENT_OFF__);
    }

    ETHR_ASSERT(val == ETHR_EVENT_OFF_WAITER__
		|| val == ETHR_EVENT_OFF__);

    res = pthread_mutex_lock(&e->mtx);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);

    while (1) {

	val = ethr_atomic32_read(&e->state);
	if (val == ETHR_EVENT_ON__)
	    break;

	res = pthread_cond_wait(&e->cnd, &e->mtx);
	if (res == EINTR)
	    break;
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);
    }

    ulres = pthread_mutex_unlock(&e->mtx);
    if (ulres != 0)
	ETHR_FATAL_ERROR__(ulres);

    return res; /* 0 || EINTR */
}

#else
#error No ethread event implementation
#endif

void
ethr_event_reset(ethr_event *e)
{
    ethr_event_reset__(e);
}

void
ethr_event_set(ethr_event *e)
{
    ethr_event_set__(e);
}

int
ethr_event_wait(ethr_event *e)
{
    return wait__(e, 0);
}

int
ethr_event_swait(ethr_event *e, int spincount)
{
    return wait__(e, spincount);
}
