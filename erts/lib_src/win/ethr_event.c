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

#include "ethread.h"
#include "ethr_internal.h"

/* --- Windows implementation of thread events ------------------------------ */

void
ethr_init_event__(void)
{

}

int
ethr_event_init(ethr_event *e)
{
    ethr_atomic32_init(&e->state, ETHR_EVENT_OFF__);
    e->handle = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (e->handle == INVALID_HANDLE_VALUE)
	return ethr_win_get_errno__();
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
    BOOL res = CloseHandle(e->handle);
    return res == 0 ? ethr_win_get_errno__() : 0;
}

void
ethr_event_set(ethr_event *e)
{
    ethr_event_set__(e);
}

void
ethr_event_reset(ethr_event *e)
{
    ethr_event_reset__(e);
}

static ETHR_INLINE int
wait(ethr_event *e, int spincount, ethr_sint64_t timeout)
{
    DWORD code, tmo;
    int sc, res, until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;

    if (timeout < 0)
	tmo = INFINITE;
    else if (timeout == 0) {
	ethr_sint32_t state = ethr_atomic32_read(&e->state);
	if (state == ETHR_EVENT_ON__) {
	    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
	    return 0;
	}
	return ETIMEDOUT;
    }	    
    else {
	/*
	 * Timeout in nano-seconds, but we can only
	 * wait for milli-seconds...
	 */
	tmo = (DWORD) (timeout - 1) / (1000*1000) + 1;
    }

    if (spincount < 0)
	ETHR_FATAL_ERROR__(EINVAL);

    sc = spincount;

    while (1) {
	ethr_sint32_t state;
	while (1) {
	    state = ethr_atomic32_read(&e->state);
	    if (state == ETHR_EVENT_ON__) {
		ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
		return 0;
	    }
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

	if (state != ETHR_EVENT_OFF_WAITER__) {
	    state = ethr_atomic32_cmpxchg(&e->state,
					  ETHR_EVENT_OFF_WAITER__,
					  ETHR_EVENT_OFF__);
	    if (state == ETHR_EVENT_ON__)
		return 0;
	    ETHR_ASSERT(state == ETHR_EVENT_OFF__);
	}

	code = WaitForSingleObject(e->handle, tmo);
        if (code == WAIT_TIMEOUT)
            return ETIMEDOUT;
	if (code != WAIT_OBJECT_0)
	    ETHR_FATAL_ERROR__(ethr_win_get_errno__());
    }

}

int
ethr_event_wait(ethr_event *e)
{
    return wait(e, 0, -1);
}

int
ethr_event_swait(ethr_event *e, int spincount)
{
    return wait(e, spincount, -1);
}

int
ethr_event_twait(ethr_event *e, ethr_sint64_t timeout)
{
    return wait(e, 0, timeout);
}

int
ethr_event_stwait(ethr_event *e, int spincount, ethr_sint64_t timeout)
{
    return wait(e, spincount, timeout);
}
