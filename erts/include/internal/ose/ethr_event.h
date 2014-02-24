/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2011. All Rights Reserved.
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

//#define USE_PTHREAD_API

#define ETHR_EVENT_OFF_WAITER__		-1L
#define ETHR_EVENT_OFF__		1L
#define ETHR_EVENT_ON__ 		0L

#ifdef USE_PTHREAD_API

typedef struct {
    ethr_atomic32_t state;
    pthread_mutex_t mtx;
    pthread_cond_t cnd;
} ethr_event;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    ethr_sint32_t val;
    val = ethr_atomic32_xchg_mb(&e->state, ETHR_EVENT_ON__);
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
    ethr_atomic32_set(&e->state, ETHR_EVENT_OFF__);
    ETHR_MEMORY_BARRIER;
}

#endif

#else

typedef struct {
    ethr_atomic32_t state;
    PROCESS proc;
} ethr_event;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

static void ETHR_INLINE
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    ethr_sint32_t val = ethr_atomic32_xchg_mb(&e->state, ETHR_EVENT_ON__);
    if (val == ETHR_EVENT_OFF_WAITER__) {
#ifdef DEBUG
      OSFSEMVAL fsem_val = get_fsem(e->proc);

      /* There is a race in this assert.
	 This is because the state is set before the wait call in wait__.
	 We hope that a delay of 10 ms is enough */
      if (fsem_val == 0)
	delay(10);
      ETHR_ASSERT(get_fsem(e->proc) == -1);
#endif
      signal_fsem(e->proc);
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
int ethr_event_destroy(ethr_event *e);
int ethr_event_wait(ethr_event *e);
int ethr_event_swait(ethr_event *e, int spincount);
#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)
void ethr_event_set(ethr_event *e);
void ethr_event_reset(ethr_event *e);
#endif
