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

#if defined(ETHR_HAVE_LINUX_FUTEX) && defined(ETHR_HAVE_32BIT_NATIVE_ATOMIC_OPS)
/* --- Linux futex implementation of ethread events ------------------------- */
#define ETHR_LINUX_FUTEX_IMPL__

#include <sys/syscall.h>
#include <unistd.h>
#include <linux/futex.h>
#include <sys/time.h>

#define ETHR_EVENT_OFF_WAITER__		((ethr_sint32_t) -1)
#define ETHR_EVENT_OFF__		((ethr_sint32_t) 1)
#define ETHR_EVENT_ON__ 		((ethr_sint32_t) 0)

#if defined(FUTEX_WAIT_PRIVATE) && defined(FUTEX_WAKE_PRIVATE)
#  define ETHR_FUTEX_WAIT__ FUTEX_WAIT_PRIVATE
#  define ETHR_FUTEX_WAKE__ FUTEX_WAKE_PRIVATE
#else
#  define ETHR_FUTEX_WAIT__ FUTEX_WAIT
#  define ETHR_FUTEX_WAKE__ FUTEX_WAKE
#endif

typedef struct {
    ethr_atomic32_t futex;
} ethr_event;

#define ETHR_FUTEX__(FTX, OP, VAL, TIMEOUT)		\
  (-1 == syscall(__NR_futex,				\
		 (void *) ethr_atomic32_addr((FTX)),	\
		 (OP),					\
		 (int) (VAL),				\
		 (TIMEOUT),				\
		 NULL,					\
		 0)					\
   ? errno : 0)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    ethr_sint32_t val;
    val = ethr_atomic32_xchg_mb(&e->futex, ETHR_EVENT_ON__);
    if (val == ETHR_EVENT_OFF_WAITER__) {
	int res = ETHR_FUTEX__(&e->futex, ETHR_FUTEX_WAKE__, 1, NULL);
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);
    }
}

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_reset)(ethr_event *e)
{
    ethr_atomic32_set(&e->futex, ETHR_EVENT_OFF__);
    ETHR_MEMORY_BARRIER;
}

#endif

#elif defined(ETHR_PTHREADS)
/* --- Posix mutex/cond pipe/select implementation of events ---------------- */

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#  define __DARWIN__ 1
#endif

#ifdef __DARWIN__
typedef struct ethr_event_fdsets___ ethr_event_fdsets__;
#endif

typedef struct {
    ethr_atomic32_t state;
    pthread_mutex_t mtx;
    pthread_cond_t cnd;
    int fd[2];
#ifdef __DARWIN__
    ethr_event_fdsets__ *fdsets;
#endif
} ethr_event;

#define ETHR_EVENT_OFF_WAITER_SELECT__	((ethr_sint32_t) -2)
#define ETHR_EVENT_OFF_WAITER__		((ethr_sint32_t) -1)
#define ETHR_EVENT_OFF__		((ethr_sint32_t) 1)
#define ETHR_EVENT_ON__ 		((ethr_sint32_t) 0)

#define ETHR_EVENT_IS_WAITING__(VAL) ((VAL) < 0)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

#ifndef ETHR_HAVE_PTHREAD_TIMED_COND_MONOTONIC
#include <unistd.h>
#include <errno.h>
#endif

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    ethr_sint32_t val;
    val = ethr_atomic32_xchg_mb(&e->state, ETHR_EVENT_ON__);
    if (ETHR_EVENT_IS_WAITING__(val)) {
	int res;
	if (val == ETHR_EVENT_OFF_WAITER_SELECT__) {
	    ssize_t wres;
	    int fd = e->fd[1];
	    ETHR_ASSERT(fd >= 0);
	    do {
		wres = write(fd, "!", 1);
	    } while (wres < 0 && errno == EINTR);
	    if (wres < 0 && errno != EAGAIN && errno != EWOULDBLOCK)
		ETHR_FATAL_ERROR__(errno);
	}
	else {
	    res = pthread_mutex_lock(&e->mtx);
	    if (res != 0)
		ETHR_FATAL_ERROR__(res);
	    res = pthread_cond_signal(&e->cnd);
	    if (res != 0)
		ETHR_FATAL_ERROR__(res);
	    res = pthread_mutex_unlock(&e->mtx);
	    if (res != 0)
		ETHR_FATAL_ERROR__(res);
	}
    }
}

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_reset)(ethr_event *e)
{
    ethr_atomic32_set(&e->state, ETHR_EVENT_OFF__);
    ETHR_MEMORY_BARRIER;
}

#endif

#endif

int ethr_event_init(ethr_event *e);
int ethr_event_prepare_timed(ethr_event *e);
int ethr_event_destroy(ethr_event *e);
int ethr_event_wait(ethr_event *e);
int ethr_event_swait(ethr_event *e, int spincount);
int ethr_event_twait(ethr_event *e, ethr_sint64_t timeout);
int ethr_event_stwait(ethr_event *e, int spincount, ethr_sint64_t timeout);
#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)
void ethr_event_set(ethr_event *e);
void ethr_event_reset(ethr_event *e);
#endif
