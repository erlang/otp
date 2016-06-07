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

#define ETHR_EVENT_OFF_WAITER__		((ethr_sint32_t) -1)
#define ETHR_EVENT_OFF__		((ethr_sint32_t) 1)
#define ETHR_EVENT_ON__ 		((ethr_sint32_t) 0)

typedef struct {
    ethr_atomic32_t state;
    HANDLE handle;
} ethr_event;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_EVENT_IMPL__)

#pragma intrinsic(_InterlockedExchange)

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_event_set)(ethr_event *e)
{
    /* _InterlockedExchange() imply a full memory barrier which is important */
    ethr_sint32_t state = ethr_atomic32_xchg_wb(&e->state, ETHR_EVENT_ON__);
    if (state == ETHR_EVENT_OFF_WAITER__) {
	if (!SetEvent(e->handle))
	    ETHR_FATAL_ERROR__(ethr_win_get_errno__());
    }
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_event_reset)(ethr_event *e)
{
    ethr_atomic32_set(&e->state, ETHR_EVENT_OFF__);
    ETHR_MEMORY_BARRIER;
}

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
