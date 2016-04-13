/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
 * TIMING WHEEL
 * 
 * Timeouts kept in an wheel. A timeout is measured relative to the
 * current slot (tiw_pos) in the wheel, and inserted at slot 
 * (tiw_pos + timeout) % TIW_SIZE. Each timeout also has a count
 * equal to timeout/TIW_SIZE, which is needed since the time axis
 * is wrapped arount the wheel. 
 *
 * Several slots may be processed in one operation. If the number of
 * slots is greater that the wheel size, the wheel is only traversed
 * once,
 *
 * The following example shows a time axis where there is one timeout
 * at each "tick", and where 1, 2, 3 ... wheel slots are released in
 * one operation. The notation "<x" means "release all items with
 * counts less than x". 
 *
 * Size of wheel: 4
 * 
 *   --|----|----|----|----|----|----|----|----|----|----|----|----|----
 *    0.0  0.1  0.2  0.3  1.0  1.1  1.2  1.3  2.0  2.1  2.2  2.3  3.0
 * 
 * 1   [    )
 *     <1  0.1  0.2  0.3  0.0  1.1  1.2  1.3  1.0  2.1  2.2  2.3  2.0
 * 
 * 2   [         )
 *     <1   <1  0.2  0.3  0.0  0.1  1.2  1.3  1.0  1.1  2.2  2.3  2.0
 * 
 * 3   [              )
 *     <1   <1   <1  0.3  0.0  0.1  0.2  1.3  1.0  1.1  1.2  2.3  2.0
 * 
 * 4   [                   )
 *     <1   <1   <1   <1  0.0  0.1  0.2  0.3  1.0  1.1  1.2  1.3  2.0
 * 
 * 5   [                        )
 *     <2   <1   <1   <1.      0.1  0.2  0.3  0.0  1.1  1.2  1.3  1.0
 * 
 * 6   [                             )
 *     <2   <2   <1   <1.           0.2  0.3  0.0  0.1  1.2  1.3  1.0
 * 
 * 7   [                                  )
 *     <2   <2   <2   <1.                0.3  0.0  0.1  0.2  1.3  1.0
 * 
 * 8   [                                       )   
 *     <2   <2   <2   <2.                     0.0  0.1  0.2  0.3  1.0
 * 
 * 9   [                                            )
 *     <3   <2   <2   <2.                          0.1  0.2  0.3  0.0
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"

#define ERTS_MONOTONIC_DAY ERTS_SEC_TO_MONOTONIC(60*60*24)
#define ERTS_CLKTCKS_DAY ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_DAY)

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ASSERT_NO_LOCKED_LOCKS		erts_lc_check_exact(NULL, 0)
#else
#define ASSERT_NO_LOCKED_LOCKS
#endif

#if 0
#  define ERTS_TW_DEBUG
#endif
#if defined(DEBUG) && !defined(ERTS_TW_DEBUG)
#  define ERTS_TW_DEBUG
#endif

#undef ERTS_TW_ASSERT
#if defined(ERTS_TW_DEBUG)
#  define ERTS_TW_ASSERT(E) ERTS_ASSERT(E)
#else
#  define ERTS_TW_ASSERT(E) ((void) 1)
#endif

#ifdef ERTS_TW_DEBUG
#  define ERTS_TWHEEL_BUMP_YIELD_LIMIT 5
#else
#  define ERTS_TWHEEL_BUMP_YIELD_LIMIT 100
#endif

/* Actual interval time chosen by sys_init_time() */

#if SYS_CLOCK_RESOLUTION == 1
#  define TIW_ITIME 1
#  define TIW_ITIME_IS_CONSTANT
#else
static int tiw_itime; /* Constant after init */
#  define TIW_ITIME tiw_itime
#endif

struct ErtsTimerWheel_ {
    ErtsTWheelTimer *w[ERTS_TIW_SIZE];
    ErtsMonotonicTime pos;
    Uint nto;
    struct {
	ErtsTWheelTimer *head;
	ErtsTWheelTimer *tail;
	Uint nto;
    } at_once;
    int yield_slot;
    int yield_slots_left;
    int yield_start_pos;
    ErtsTWheelTimer sentinel;
    int true_next_timeout_time;
    ErtsMonotonicTime next_timeout_time;
};

static ERTS_INLINE ErtsMonotonicTime
find_next_timeout(ErtsSchedulerData *esdp,
		  ErtsTimerWheel *tiw,
		  int search_all,
		  ErtsMonotonicTime curr_time,       /* When !search_all */
		  ErtsMonotonicTime max_search_time) /* When !search_all */
{
    int start_ix, tiw_pos_ix;
    ErtsTWheelTimer *p;
    int true_min_timeout = 0;
    ErtsMonotonicTime min_timeout, min_timeout_pos, slot_timeout_pos;

    if (tiw->nto == 0) { /* no timeouts in wheel */
	if (!search_all)
	    min_timeout_pos = tiw->pos;
	else {
	    curr_time = erts_get_monotonic_time(esdp);
	    tiw->pos = min_timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time);
	}
	min_timeout_pos += ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_DAY);
	goto found_next;
    }

    slot_timeout_pos = min_timeout_pos = tiw->pos;
    if (search_all)
	min_timeout_pos += ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_DAY);
    else
	min_timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time + max_search_time);

    start_ix = tiw_pos_ix = (int) (tiw->pos & (ERTS_TIW_SIZE-1));

    do {
	if (++slot_timeout_pos >= min_timeout_pos)
	    break;

	p = tiw->w[tiw_pos_ix];

	if (p) {
	    ErtsTWheelTimer *end = p;

	    do  {
		ErtsMonotonicTime timeout_pos;
		timeout_pos = p->timeout_pos;
		if (min_timeout_pos > timeout_pos) {
		    true_min_timeout = 1;
		    min_timeout_pos = timeout_pos;
		    if (min_timeout_pos <= slot_timeout_pos)
			goto found_next;
		}
		p = p->next;
	    } while (p != end);
	}

	tiw_pos_ix++;
	if (tiw_pos_ix == ERTS_TIW_SIZE)
	    tiw_pos_ix = 0;
    } while (start_ix != tiw_pos_ix);

found_next:

    min_timeout = ERTS_CLKTCKS_TO_MONOTONIC(min_timeout_pos);
    tiw->next_timeout_time = min_timeout;
    tiw->true_next_timeout_time = true_min_timeout;

    return min_timeout;
}

static ERTS_INLINE void
insert_timer_into_slot(ErtsTimerWheel *tiw, int slot, ErtsTWheelTimer *p)
{
    ERTS_TW_ASSERT(slot >= 0);
    ERTS_TW_ASSERT(slot < ERTS_TIW_SIZE);
    p->slot = slot;
    if (!tiw->w[slot]) {
	tiw->w[slot] = p;
	p->next = p;
	p->prev = p;
    }
    else {
	ErtsTWheelTimer *next, *prev;
	next = tiw->w[slot];
	prev = next->prev;
	p->next = next;
	p->prev = prev;
	prev->next = p;
	next->prev = p;
    }
}

static ERTS_INLINE void
remove_timer(ErtsTimerWheel *tiw, ErtsTWheelTimer *p)
{
    int slot = p->slot;
    ERTS_TW_ASSERT(slot != ERTS_TWHEEL_SLOT_INACTIVE);

    if (slot >= 0) {
	/*
	 * Timer in wheel or in circular
	 * list of timers currently beeing
	 * triggered (referred by sentinel).
	 */
	ERTS_TW_ASSERT(slot < ERTS_TIW_SIZE);

	if (p->next == p) {
	    ERTS_TW_ASSERT(tiw->w[slot] == p);
	    tiw->w[slot] = NULL;
	}
	else {
	    if (tiw->w[slot] == p)
		tiw->w[slot] = p->next;
	    p->prev->next = p->next;
	    p->next->prev = p->prev;
	}
    }
    else {
	/* Timer in "at once" queue... */
	ERTS_TW_ASSERT(slot == ERTS_TWHEEL_SLOT_AT_ONCE);
	if (p->prev)
	    p->prev->next = p->next;
	else {
	    ERTS_TW_ASSERT(tiw->at_once.head == p);
	    tiw->at_once.head = p->next;
	}
	if (p->next)
	    p->next->prev = p->prev;
	else {
	    ERTS_TW_ASSERT(tiw->at_once.tail == p);
	    tiw->at_once.tail = p->prev;
	}
	ERTS_TW_ASSERT(tiw->at_once.nto > 0);
	tiw->at_once.nto--;
    }

    p->slot = ERTS_TWHEEL_SLOT_INACTIVE;

    tiw->nto--;
}

ErtsMonotonicTime
erts_check_next_timeout_time(ErtsSchedulerData *esdp)
{
    ErtsTimerWheel *tiw = esdp->timer_wheel;
    ErtsMonotonicTime time;
    ERTS_MSACC_DECLARE_CACHE_X();
    if (tiw->true_next_timeout_time)
	return tiw->next_timeout_time;
    ERTS_MSACC_PUSH_AND_SET_STATE_CACHED_X(ERTS_MSACC_STATE_TIMERS);
    time = find_next_timeout(esdp, tiw, 1, 0, 0);
    ERTS_MSACC_POP_STATE_M_X();
    return time;
}

#ifndef ERTS_TW_DEBUG
#define ERTS_DBG_CHK_SAFE_TO_SKIP_TO(TIW, TO) ((void) 0)
#else
#define ERTS_DBG_CHK_SAFE_TO_SKIP_TO(TIW, TO) debug_check_safe_to_skip_to((TIW), (TO))
static void
debug_check_safe_to_skip_to(ErtsTimerWheel *tiw, ErtsMonotonicTime skip_to_pos)
{
    int slots, ix;
    ErtsTWheelTimer *tmr;
    ErtsMonotonicTime tmp;

    ix = (int) (tiw->pos & (ERTS_TIW_SIZE-1));
    tmp = skip_to_pos - tiw->pos;
    ERTS_TW_ASSERT(tmp >= 0);
    if (tmp < (ErtsMonotonicTime) ERTS_TIW_SIZE)
	slots = (int) tmp;
    else
	slots = ERTS_TIW_SIZE;

     while (slots > 0) {
	 tmr = tiw->w[ix];
	 if (tmr) {
	     ErtsTWheelTimer *end = tmr;
	     do {
		 ERTS_TW_ASSERT(tmr->timeout_pos > skip_to_pos);
		 tmr = tmr->next;
	     } while (tmr != end);
	 }
	 ix++;
	 if (ix == ERTS_TIW_SIZE)
	     ix = 0;
	 slots--;
    }
}
#endif

static ERTS_INLINE void
timeout_timer(ErtsTWheelTimer *p)
{
    ErlTimeoutProc timeout;
    void *arg;
    p->slot = ERTS_TWHEEL_SLOT_INACTIVE;
    timeout = p->u.func.timeout;
    arg = p->u.func.arg;
    (*timeout)(arg);
    ASSERT_NO_LOCKED_LOCKS;
}

void
erts_bump_timers(ErtsTimerWheel *tiw, ErtsMonotonicTime curr_time)
{
    int tiw_pos_ix, slots, yielded_slot_restarted, yield_count;
    ErtsMonotonicTime bump_to, tmp_slots, old_pos;
    ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_TIMERS);

    yield_count = ERTS_TWHEEL_BUMP_YIELD_LIMIT;

    /*
     * In order to be fair we always continue with work
     * where we left off when restarting after a yield.
     */

    if (tiw->yield_slot >= 0) {
	yielded_slot_restarted = 1;
	tiw_pos_ix = tiw->yield_slot;
	slots = tiw->yield_slots_left;
	bump_to = tiw->pos;
	old_pos = tiw->yield_start_pos;
	goto restart_yielded_slot;
    }

    do {

	yielded_slot_restarted = 0;

	bump_to = ERTS_MONOTONIC_TO_CLKTCKS(curr_time);

	while (1) {
	    ErtsTWheelTimer *p;

	    old_pos = tiw->pos;

	    if (tiw->nto == 0) {
	    empty_wheel:
		ERTS_DBG_CHK_SAFE_TO_SKIP_TO(tiw, bump_to);
		tiw->true_next_timeout_time = 0;
		tiw->next_timeout_time = curr_time + ERTS_MONOTONIC_DAY;
		tiw->pos = bump_to;
		tiw->yield_slot = ERTS_TWHEEL_SLOT_INACTIVE;
                ERTS_MSACC_POP_STATE_M_X();
		return;
	    }

	    p = tiw->at_once.head;
	    while (p) {
		if (--yield_count <= 0) {
		    ERTS_TW_ASSERT(tiw->nto > 0);
		    ERTS_TW_ASSERT(tiw->at_once.nto > 0);
		    tiw->yield_slot = ERTS_TWHEEL_SLOT_AT_ONCE;
		    tiw->true_next_timeout_time = 1;
		    tiw->next_timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(old_pos);
                    ERTS_MSACC_POP_STATE_M_X();
		    return;
		}

		ERTS_TW_ASSERT(tiw->nto > 0);
		ERTS_TW_ASSERT(tiw->at_once.nto > 0);
		tiw->nto--;
		tiw->at_once.nto--;
		tiw->at_once.head = p->next;
		if (p->next)
		    p->next->prev = NULL;
		else
		    tiw->at_once.tail = NULL;

		timeout_timer(p);

		p = tiw->at_once.head;
	    }

	    if (tiw->pos >= bump_to) {
                ERTS_MSACC_POP_STATE_M_X();
		break;
            }

	    if (tiw->nto == 0)
		goto empty_wheel;

	    if (tiw->true_next_timeout_time) {
		ErtsMonotonicTime skip_until_pos;
		/*
		 * No need inspecting slots where we know no timeouts
		 * to trigger should reside.
		 */

		skip_until_pos = ERTS_MONOTONIC_TO_CLKTCKS(tiw->next_timeout_time);
		if (skip_until_pos > bump_to)
		    skip_until_pos = bump_to;

		skip_until_pos--;

		if (skip_until_pos > tiw->pos) {
		    ERTS_DBG_CHK_SAFE_TO_SKIP_TO(tiw, skip_until_pos);

		    tiw->pos = skip_until_pos;
		}
	    }

	    tiw_pos_ix = (int) ((tiw->pos+1) & (ERTS_TIW_SIZE-1));
	    tmp_slots = (bump_to - tiw->pos);
	    if (tmp_slots < (ErtsMonotonicTime) ERTS_TIW_SIZE)
		slots = (int) tmp_slots;
	    else
		slots = ERTS_TIW_SIZE;

	    tiw->pos = bump_to;

	    while (slots > 0) {

		p = tiw->w[tiw_pos_ix];
		if (p) {
		    if (p->next == p) {
			ERTS_TW_ASSERT(tiw->sentinel.next == &tiw->sentinel);
			ERTS_TW_ASSERT(tiw->sentinel.prev == &tiw->sentinel);
		    }
		    else {
			tiw->sentinel.next = p->next;
			tiw->sentinel.prev = p->prev;
			tiw->sentinel.next->prev = &tiw->sentinel;
			tiw->sentinel.prev->next = &tiw->sentinel;
		    }
		    tiw->w[tiw_pos_ix] = NULL;

		    while (1) {

			if (p->timeout_pos > bump_to) {
			    /* Very unusual case... */
			    ++yield_count;
			    insert_timer_into_slot(tiw, tiw_pos_ix, p);
			}
			else {
			    /* Normal case... */
			    timeout_timer(p);
			    tiw->nto--;
			}

		    restart_yielded_slot:

			p = tiw->sentinel.next;
			if (p == &tiw->sentinel) {
			    ERTS_TW_ASSERT(tiw->sentinel.prev == &tiw->sentinel);
			    break;
			}

			if (--yield_count <= 0) {
			    tiw->true_next_timeout_time = 1;
			    tiw->next_timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(old_pos);
			    tiw->yield_slot = tiw_pos_ix;
			    tiw->yield_slots_left = slots;
			    tiw->yield_start_pos = old_pos;
                            ERTS_MSACC_POP_STATE_M_X();
			    return; /* Yield! */
			}

			tiw->sentinel.next = p->next;
			p->next->prev = &tiw->sentinel;
		    }
		}
		tiw_pos_ix++;
		if (tiw_pos_ix == ERTS_TIW_SIZE)
		    tiw_pos_ix = 0;
		slots--;
	    }
	}

    } while (yielded_slot_restarted);

    tiw->yield_slot = ERTS_TWHEEL_SLOT_INACTIVE;
    tiw->true_next_timeout_time = 0;
    tiw->next_timeout_time = curr_time + ERTS_MONOTONIC_DAY;

    /* Search at most two seconds ahead... */
    (void) find_next_timeout(NULL, tiw, 0, curr_time, ERTS_SEC_TO_MONOTONIC(2));
    ERTS_MSACC_POP_STATE_M_X();
}

Uint
erts_timer_wheel_memory_size(void)
{
    return sizeof(ErtsTimerWheel)*erts_no_schedulers;
}

ErtsTimerWheel *
erts_create_timer_wheel(ErtsSchedulerData *esdp)
{
    ErtsMonotonicTime mtime;
    int i;
    ErtsTimerWheel *tiw;
    tiw = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_TIMER_WHEEL,
					     sizeof(ErtsTimerWheel));
    for(i = 0; i < ERTS_TIW_SIZE; i++)
	tiw->w[i] = NULL;

    mtime = erts_get_monotonic_time(esdp);
    tiw->pos = ERTS_MONOTONIC_TO_CLKTCKS(mtime);
    tiw->nto = 0;
    tiw->at_once.head = NULL;
    tiw->at_once.tail = NULL;
    tiw->at_once.nto = 0;
    tiw->yield_slot = ERTS_TWHEEL_SLOT_INACTIVE;
    tiw->true_next_timeout_time = 0;
    tiw->next_timeout_time = mtime + ERTS_MONOTONIC_DAY;
    tiw->sentinel.next = &tiw->sentinel;
    tiw->sentinel.prev = &tiw->sentinel;
    tiw->sentinel.u.func.timeout = NULL;
    tiw->sentinel.u.func.cancel = NULL;
    tiw->sentinel.u.func.arg = NULL;
    return tiw;
}

ErtsNextTimeoutRef
erts_get_next_timeout_reference(ErtsTimerWheel *tiw)
{
    return (ErtsNextTimeoutRef) &tiw->next_timeout_time;
}


/* this routine links the time cells into a free list at the start
   and sets the time queue as empty */
void
erts_init_time(int time_correction, ErtsTimeWarpMode time_warp_mode)
{
    int itime;

    /* system dependent init; must be done before do_time_init()
       if timer thread is enabled */
    itime = erts_init_time_sup(time_correction, time_warp_mode);
#ifdef TIW_ITIME_IS_CONSTANT 
    if (itime != TIW_ITIME) {
	erts_exit(ERTS_ABORT_EXIT, "timer resolution mismatch %d != %d", itime, TIW_ITIME);
    }
#else
    tiw_itime = itime;
#endif
}

void
erts_twheel_set_timer(ErtsTimerWheel *tiw,
		      ErtsTWheelTimer *p, ErlTimeoutProc timeout,
		      ErlCancelProc cancel, void *arg,
		      ErtsMonotonicTime timeout_pos)
{
    ErtsMonotonicTime timeout_time;
    ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_TIMERS);

    p->u.func.timeout = timeout;
    p->u.func.cancel = cancel;
    p->u.func.arg = arg;

    ERTS_TW_ASSERT(p->slot == ERTS_TWHEEL_SLOT_INACTIVE);

    if (timeout_pos <= tiw->pos) {
	tiw->nto++;
	tiw->at_once.nto++;
	p->next = NULL;
	p->prev = tiw->at_once.tail;
	if (tiw->at_once.tail) {
	    ERTS_TW_ASSERT(tiw->at_once.head);
	    tiw->at_once.tail->next = p;
	}
	else {	
	    ERTS_TW_ASSERT(!tiw->at_once.head);
	    tiw->at_once.head = p;
	}
	tiw->at_once.tail = p;
	p->timeout_pos = tiw->pos;
	p->slot = ERTS_TWHEEL_SLOT_AT_ONCE;
	timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(tiw->pos);
    }
    else {
	int slot;

	/* calculate slot */
	slot = (int) (timeout_pos & (ERTS_TIW_SIZE-1));

	insert_timer_into_slot(tiw, slot, p);

	tiw->nto++;

	timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(timeout_pos);
	p->timeout_pos = timeout_pos;
    }

    if (timeout_time < tiw->next_timeout_time) {
	tiw->true_next_timeout_time = 1;
	tiw->next_timeout_time = timeout_time;
    }
    ERTS_MSACC_POP_STATE_M_X();
}

void
erts_twheel_cancel_timer(ErtsTimerWheel *tiw, ErtsTWheelTimer *p)
{
    if (p->slot != ERTS_TWHEEL_SLOT_INACTIVE) {
	ErlCancelProc cancel;
	void *arg;
        ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_TIMERS);
	remove_timer(tiw, p);
	cancel = p->u.func.cancel;
	arg = p->u.func.arg;
	if (cancel)
	    (*cancel)(arg);
        ERTS_MSACC_POP_STATE_M_X();
    }
}

void
erts_twheel_debug_foreach(ErtsTimerWheel *tiw,
			  void (*tclbk)(void *),
			  void (*func)(void *,
				       ErtsMonotonicTime,
				       void *),
			  void *arg)
{
    ErtsTWheelTimer *tmr;
    int ix;

    tmr = tiw->sentinel.next;
    while (tmr != &tiw->sentinel) {
	if (tmr->u.func.timeout == tclbk)
	    (*func)(arg, tmr->timeout_pos, tmr->u.func.arg);
	tmr = tmr->next;
    }

    for (tmr = tiw->at_once.head; tmr; tmr = tmr->next) {
	if (tmr->u.func.timeout == tclbk)
	    (*func)(arg, tmr->timeout_pos, tmr->u.func.arg);
    }

    for (ix = 0; ix < ERTS_TIW_SIZE; ix++) {
	tmr = tiw->w[ix];
	if (tmr) {
	    do {
		if (tmr->u.func.timeout == tclbk)
		    (*func)(arg, tmr->timeout_pos, tmr->u.func.arg);
		tmr = tmr->next;
	    } while (tmr != tiw->w[ix]);
	}
    }
}

#ifdef ERTS_TW_DEBUG
void erts_p_slpq(void)
{
    erts_printf("Not yet implemented...\n");
#if 0
    ErtsMonotonicTime current_time = erts_get_monotonic_time(NULL);
    int i;
    ErtsTWheelTimer* p;
  
    /* print the whole wheel, starting at the current position */
    erts_printf("\ncurrent time = %bps tiw_pos = %d tiw_nto %d\n",
		current_time, tiw->pos, tiw->nto);
    i = tiw->pos;
    if (tiw->w[i] != NULL) {
	erts_printf("%d:\n", i);
	for(p = tiw->w[i]; p != NULL; p = p->next) {
	    erts_printf(" (timeout time %bps, slot %d)\n",
			ERTS_CLKTCKS_TO_MONOTONIC(p->timeout_pos),
			p->slot);
	}
    }
    for(i = ((i+1) & (ERTS_TIW_SIZE-1)); i != (tiw->pos & (ERTS_TIW_SIZE-1)); i = ((i+1) & (ERTS_TIW_SIZE-1))) {
	if (tiw->w[i] != NULL) {
	    erts_printf("%d:\n", i);
	    for(p = tiw->w[i]; p != NULL; p = p->next) {
		erts_printf(" (timeout time %bps, slot %d)\n",
			    ERTS_CLKTCKS_TO_MONOTONIC(p->timeout_pos), p->slot);
	    }
	}
    }
#endif
}
#endif /* ERTS_TW_DEBUG */
