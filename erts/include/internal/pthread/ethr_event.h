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

#if defined(ETHR_HAVE_LINUX_FUTEX) && defined(ETHR_HAVE_NATIVE_ATOMICS)
/* --- Linux futex implementation of ethread events ------------------------- */
#define ETHR_LINUX_FUTEX_IMPL__

#include <sys/syscall.h>
#include <unistd.h>
#include <linux/futex.h>
#include <sys/time.h>

/*
 * Note: Linux futexes operate on 32-bit integers, but
 *       ethr_native_atomic_t are 64-bits on 64-bit
 *       platforms. This has to be taken into account.
 *       Therefore, in each individual value used each
 *       byte look the same.
 */

#if ETHR_SIZEOF_PTR == 8

#define ETHR_EVENT_OFF_WAITER__		0xffffffffffffffffL
#define ETHR_EVENT_OFF__		0x7777777777777777L
#define ETHR_EVENT_ON__ 		0L

#elif  ETHR_SIZEOF_PTR == 4

#define ETHR_EVENT_OFF_WAITER__		0xffffffffL
#define ETHR_EVENT_OFF__		0x77777777L
#define ETHR_EVENT_ON__ 		0L

#else

#error ehrm...

#endif

#if defined(FUTEX_WAIT_PRIVATE) && defined(FUTEX_WAKE_PRIVATE)
#  define ETHR_FUTEX_WAIT__ FUTEX_WAIT_PRIVATE
#  define ETHR_FUTEX_WAKE__ FUTEX_WAKE_PRIVATE
#else
#  define ETHR_FUTEX_WAIT__ FUTEX_WAIT
#  define ETHR_FUTEX_WAKE__ FUTEX_WAKE
#endif

typedef struct {
    ethr_atomic_t futex;
} ethr_event;

#define ETHR_FUTEX__(FTX, OP, VAL) \
  (-1 == syscall(__NR_futex, (void *) (FTX), (OP), (int) (VAL), NULL, NULL, 0)\
   ? errno : 0)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    long val;
    ETHR_WRITE_MEMORY_BARRIER;
    val = ethr_atomic_xchg(&e->futex, ETHR_EVENT_ON__);
    if (val == ETHR_EVENT_OFF_WAITER__) {
	int res = ETHR_FUTEX__(&e->futex, ETHR_FUTEX_WAKE__, 1);
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);
    }
}

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_reset)(ethr_event *e)
{
    ethr_atomic_set(&e->futex, ETHR_EVENT_OFF__);
    ETHR_MEMORY_BARRIER;
}

#endif

#elif defined(ETHR_PTHREADS)
/* --- Posix mutex/cond implementation of events ---------------------------- */

typedef struct {
    ethr_atomic_t state;
    pthread_mutex_t mtx;
    pthread_cond_t cnd;
} ethr_event;

#define ETHR_EVENT_OFF_WAITER__		-1L
#define ETHR_EVENT_OFF__		1L
#define ETHR_EVENT_ON__ 		0L

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    long val;
    ETHR_WRITE_MEMORY_BARRIER;
    val = ethr_atomic_xchg(&e->state, ETHR_EVENT_ON__);
    if (val == ETHR_EVENT_OFF_WAITER__) {
	int res = pthread_mutex_lock(&e->mtx);
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

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_reset)(ethr_event *e)
{
    ethr_atomic_set(&e->state, ETHR_EVENT_OFF__);
    ETHR_MEMORY_BARRIER;
}

#endif

#endif

int ethr_event_init(ethr_event *e);
int ethr_event_destroy(ethr_event *e);
int ethr_event_wait(ethr_event *e);
int ethr_event_swait(ethr_event *e, int spincount);
#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)
void ethr_event_set(ethr_event *e);
void ethr_event_reset(ethr_event *e);
#endif
