/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
 * Author: Rickard Green
 */

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHR_EVENT_IMPL__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#  define __DARWIN__ 1
#endif
#ifdef __DARWIN__
#  define _DARWIN_UNLIMITED_SELECT
#endif

#include "ethread.h"
#undef ETHR_INCLUDE_MONOTONIC_CLOCK__
#define ETHR_INCLUDE_MONOTONIC_CLOCK__
#include "ethr_internal.h"

#if defined(ETHR_LINUX_FUTEX_IMPL__)
/* --- Linux futex implementation of ethread events ------------------------- */

#include <sched.h>
#include <errno.h>

#define ETHR_YIELD_AFTER_BUSY_LOOPS 50

void
ethr_init_event__(void)
{

}

int
ethr_event_init(ethr_event *e)
{
    ethr_atomic32_init(&e->futex, ETHR_EVENT_OFF__);
    return 0;
}

int
ethr_event_prepare_timed(ethr_event *e)
{
    return 0;
}

int
ethr_event_destroy(ethr_event *e)
{
    return 0;
}

static ETHR_INLINE int
wait__(ethr_event *e, int spincount, ethr_sint64_t timeout)
{
    unsigned sc = spincount;
    int res;
    ethr_sint32_t val;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
    ethr_sint64_t time = 0; /* SHUT UP annoying faulty warning... */
    struct timespec ts, *tsp;
#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
    ethr_sint64_t start = 0; /* SHUT UP annoying faulty warning... */
#endif

    if (spincount < 0)
	ETHR_FATAL_ERROR__(EINVAL);

    if (timeout < 0) {
	tsp = NULL;
    }
    else {
#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
	start = ethr_get_monotonic_time();
#endif
	tsp = &ts;
	time = timeout;
	if (spincount == 0) {
	    val = ethr_atomic32_read(&e->futex);
	    if (val == ETHR_EVENT_ON__)
		goto return_event_on;
	    goto set_timeout;
	}
    }

    while (1) {
	while (1) {
	    val = ethr_atomic32_read(&e->futex);
	    if (val == ETHR_EVENT_ON__)
		goto return_event_on;
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

	if (timeout >= 0) {
#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
	    time = timeout - (ethr_get_monotonic_time() - start);
#endif
	set_timeout:
	    if (time <= 0) {
		val = ethr_atomic32_read(&e->futex);
		if (val == ETHR_EVENT_ON__)
		    goto return_event_on;
		return ETIMEDOUT;
	    }
	    ts.tv_sec = time / (1000*1000*1000);
	    ts.tv_nsec = time % (1000*1000*1000);
	}

	if (val != ETHR_EVENT_OFF_WAITER__) {
	    val = ethr_atomic32_cmpxchg(&e->futex,
					ETHR_EVENT_OFF_WAITER__,
					ETHR_EVENT_OFF__);

	    if (val == ETHR_EVENT_ON__)
		goto return_event_on;
	    ETHR_ASSERT(val == ETHR_EVENT_OFF__);
	}

	res = ETHR_FUTEX__(&e->futex,
			   ETHR_FUTEX_WAIT__,
			   ETHR_EVENT_OFF_WAITER__,
			   tsp);
	switch (res) {
	case EINTR:
	case ETIMEDOUT:
	    return res;
	case 0:
	case EWOULDBLOCK:
	    break;
	default:
	    ETHR_FATAL_ERROR__(res);
	}
    }

return_event_on:

    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);

    return 0;

}

#elif defined(ETHR_PTHREADS)
/* --- Posix mutex/cond pipe/select implementation of events ---------------- */

#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/select.h>
#include <errno.h>
#include <string.h>

#ifdef __DARWIN__

struct ethr_event_fdsets___ {
    fd_set *rsetp;
    fd_set *esetp;
    size_t mem_size;
    fd_mask mem[1];
};

#endif

static void
setup_nonblocking_pipe(ethr_event *e)
{
    int flgs;
    int res;

    res = pipe(e->fd);
    if (res != 0)
	ETHR_FATAL_ERROR__(errno);

    ETHR_ASSERT(e->fd[0] >= 0 && e->fd[1] >= 0);

    flgs = fcntl(e->fd[0], F_GETFL, 0);
    fcntl(e->fd[0], F_SETFL, flgs | O_NONBLOCK);
    flgs = fcntl(e->fd[1], F_GETFL, 0);
    fcntl(e->fd[1], F_SETFL, flgs | O_NONBLOCK);


#ifndef __DARWIN__
    if (e->fd[0] >= FD_SETSIZE)
	ETHR_FATAL_ERROR__(ENOTSUP);
#else
    {
	int nmasks;
	ethr_event_fdsets__ *fdsets;
	size_t mem_size;

	nmasks = (e->fd[0]+NFDBITS)/NFDBITS;
	mem_size = 2*nmasks*sizeof(fd_mask);
	if (mem_size < 2*sizeof(fd_set)) {
	    mem_size = 2*sizeof(fd_set);
	    nmasks = mem_size/(2*sizeof(fd_mask));
	}

	fdsets = malloc(sizeof(ethr_event_fdsets__)
			+ mem_size
			- sizeof(fd_mask));
	if (!fdsets)
	    ETHR_FATAL_ERROR__(ENOMEM);
	fdsets->rsetp = (fd_set *) (char *) &fdsets->mem[0];
	fdsets->esetp = (fd_set *) (char *) &fdsets->mem[nmasks];
	fdsets->mem_size = mem_size;
	e->fdsets = fdsets;
    }
#endif

    ETHR_MEMBAR(ETHR_StoreStore);
}

#define ETHR_EVENT_INVALID_FD__ -1
#define ETHR_EVENT_COND_TIMEDWAIT__ -2

#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
static pthread_condattr_t monotonic_clock_cond_attr;
#endif
static pthread_condattr_t *monotonic_clock_cond_attr_p;

#ifndef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
#  undef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
#endif
#ifndef ETHR_MONOTONIC_CLOCK_ID
#  undef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
#endif

void
ethr_init_event__(void)
{
    monotonic_clock_cond_attr_p = NULL;
#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
    if (!ethr_get_monotonic_time_is_broken()
	&& pthread_condattr_init(&monotonic_clock_cond_attr) == 0) {
	if (pthread_condattr_setclock(&monotonic_clock_cond_attr,
				      ETHR_MONOTONIC_CLOCK_ID) == 0)
	    monotonic_clock_cond_attr_p = &monotonic_clock_cond_attr;
	else
	    pthread_condattr_destroy(&monotonic_clock_cond_attr);
    }
#endif
}

int
ethr_event_init(ethr_event *e)
{
    int res;

    ethr_atomic32_init(&e->state, ETHR_EVENT_OFF__);

    res = pthread_mutex_init(&e->mtx, NULL);
    if (res != 0)
	return res;

    res = pthread_cond_init(&e->cnd, monotonic_clock_cond_attr_p);
    if (res != 0) {
	pthread_mutex_destroy(&e->mtx);
	return res;
    }

#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
    /*
     * If ethr_get_monotonic_time() is broken we
     * fall back on the pipe/select solution...
     */
    if (monotonic_clock_cond_attr_p) {
	e->fd[0] = e->fd[1] = ETHR_EVENT_COND_TIMEDWAIT__;
	return 0;
    }
#endif

    e->fd[0] = e->fd[1] = ETHR_EVENT_INVALID_FD__;

#ifdef __DARWIN__
    e->fdsets = NULL;
#endif

    return 0;
}

int
ethr_event_prepare_timed(ethr_event *e)
{
    if (e->fd[0] == ETHR_EVENT_INVALID_FD__)
	setup_nonblocking_pipe(e);

    return 0;
}

int
ethr_event_destroy(ethr_event *e)
{
    int res;
    if (e->fd[0] >= 0) {
	close(e->fd[0]);
	close(e->fd[1]);
    }
#ifdef __DARWIN__
    if (e->fdsets)
	free(e->fdsets);
#endif
    res = pthread_mutex_destroy(&e->mtx);
    if (res != 0)
	return res;
    return pthread_cond_destroy(&e->cnd);
}

static ETHR_INLINE int
wait__(ethr_event *e, int spincount, ethr_sint64_t timeout)
{
    int sc = spincount;
    ethr_sint32_t val;
    int res, ulres;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
    ethr_sint64_t time = 0; /* SHUT UP annoying faulty warning... */
#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
    ethr_sint64_t start = 0; /* SHUT UP annoying faulty warning... */
#endif
#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
    struct timespec cond_timeout;
#endif

    val = ethr_atomic32_read(&e->state);
    if (val == ETHR_EVENT_ON__)
	goto return_event_on;

    if (timeout < 0) {
	if (spincount == 0)
	    goto set_event_off_waiter;
    }
    if (timeout == 0)
	return ETIMEDOUT;
    else {
	time = timeout;
	switch (e->fd[0]) {
	case ETHR_EVENT_INVALID_FD__:
#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
	    start = ethr_get_monotonic_time();
#endif
	    setup_nonblocking_pipe(e);
	    break;
#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
	case ETHR_EVENT_COND_TIMEDWAIT__:
	    time += ethr_get_monotonic_time();
	    cond_timeout.tv_sec = time / (1000*1000*1000);
	    cond_timeout.tv_nsec = time % (1000*1000*1000);
	    if (spincount == 0)
		goto set_event_off_waiter;
	    break;
#endif
	default:
	    /* Already initialized pipe... */
	    if (spincount == 0)
		goto set_select_timeout;
#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
	    start = ethr_get_monotonic_time();
#endif
	    break;
	}
    }

    if (spincount < 0)
	ETHR_FATAL_ERROR__(EINVAL);

    while (1) {
	val = ethr_atomic32_read(&e->state);
	if (val == ETHR_EVENT_ON__)
	    goto return_event_on;
	
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

    if (timeout < 0
#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
	|| e->fd[0] == ETHR_EVENT_COND_TIMEDWAIT__
#endif
	) {

    set_event_off_waiter:

	if (val != ETHR_EVENT_OFF_WAITER__) {
	    ethr_sint32_t act;
	    act = ethr_atomic32_cmpxchg(&e->state,
					ETHR_EVENT_OFF_WAITER__,
					val);
	    if (act == ETHR_EVENT_ON__)
		goto return_event_on;
	    ETHR_ASSERT(act == val);
	}

	res = pthread_mutex_lock(&e->mtx);
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);

	while (1) {

	    val = ethr_atomic32_read(&e->state);
	    if (val == ETHR_EVENT_ON__) {
		ETHR_ASSERT(res == 0);
		ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
		break;
	    }

#ifdef ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC
	    if (timeout > 0) {
		res = pthread_cond_timedwait(&e->cnd, &e->mtx, &cond_timeout);
		if (res == EINTR || res == ETIMEDOUT)
		    break;
	    }
	    else
#endif
	    {
		res = pthread_cond_wait(&e->cnd, &e->mtx);
		if (res == EINTR)
		    break;
	    }
	    if (res != 0)
		ETHR_FATAL_ERROR__(res);
	}

	ulres = pthread_mutex_unlock(&e->mtx);
	if (ulres != 0)
	    ETHR_FATAL_ERROR__(ulres);

    }
    else {
	int fd;
	int sres;
	ssize_t rres;
#ifndef __DARWIN__
	fd_set rset, eset;
#endif
	fd_set *rsetp, *esetp;
	struct timeval select_timeout;

#ifdef ETHR_HAVE_ETHR_GET_MONOTONIC_TIME
	time -= ethr_get_monotonic_time() - start;
	if (time <= 0)
	    return ETIMEDOUT;
#endif

    set_select_timeout:

	ETHR_ASSERT(time > 0);

	/*
	 * timeout in nano-second, but we can only wait
	 * for micro-seconds...
	 */
	time = ((time - 1) / 1000) + 1;

	select_timeout.tv_sec = time / (1000*1000);
	select_timeout.tv_usec = time % (1000*1000);

	ETHR_ASSERT(val != ETHR_EVENT_ON__);

	fd = e->fd[0];

	/* Cleanup pipe... */
	do {
	    char buf[64];
	    rres = read(fd, buf, sizeof(buf));
	} while (rres > 0 || (rres < 0 && errno == EINTR));
	if (rres < 0 && errno != EAGAIN && errno != EWOULDBLOCK)
	    ETHR_FATAL_ERROR__(errno);

	/*
	 * Need to verify that state is still off
	 * after cleaning the pipe...
	 */
	if (val == ETHR_EVENT_OFF_WAITER_SELECT__) {
	    val = ethr_atomic32_read(&e->state);
	    if (val == ETHR_EVENT_ON__)
		goto return_event_on;
	}
	else {
	    ethr_sint32_t act;
	    act = ethr_atomic32_cmpxchg(&e->state,
					ETHR_EVENT_OFF_WAITER_SELECT__,
					val);
	    if (act == ETHR_EVENT_ON__)
		goto return_event_on;
	    ETHR_ASSERT(act == val);
	}


#ifdef __DARWIN__
	rsetp = e->fdsets->rsetp;
	esetp = e->fdsets->esetp;
	memset((void *) &e->fdsets->mem[0], 0, e->fdsets->mem_size);
#else
	FD_ZERO(&rset);
	FD_ZERO(&eset);
	rsetp = &rset;
	esetp = &eset;
#endif

	FD_SET(fd, rsetp);
	FD_SET(fd, esetp);

	sres = select(fd + 1, rsetp, NULL, esetp, &select_timeout);
	if (sres == 0)
	    res = ETIMEDOUT;
	else {
	    res = EINTR;
	    if (sres < 0 && errno != EINTR)
		ETHR_FATAL_ERROR__(errno);
	    /* else:
	     *   Event is *probably* set, but it can be a
	     *   lingering writer. That is, it is important
	     *   that we verify that it actually is set. If
	     *   it isn't, return EINTR (spurious wakeup).
	     */
	}

	val = ethr_atomic32_read(&e->state);
	if (val == ETHR_EVENT_ON__)
	    goto return_event_on;

    }

    return res;

return_event_on:

    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);

    return 0;
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
    return wait__(e, 0, -1);
}

int
ethr_event_swait(ethr_event *e, int spincount)
{
    return wait__(e, spincount, -1);
}

int
ethr_event_twait(ethr_event *e, ethr_sint64_t timeout)
{
    return wait__(e, 0, timeout);
}

int
ethr_event_stwait(ethr_event *e, int spincount, ethr_sint64_t timeout)
{
    return wait__(e, spincount, timeout);
}
