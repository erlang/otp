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
 * TIMER WHEEL
 *
 *
 * The time scale used for timers is Erlang monotonic time. The
 * time unit used is ERTS specific clock ticks. A clock tick is
 * currently defined to 1 millisecond. That is, the resolution of
 * timers triggered by the runtime system is 1 millisecond.
 * 
 * When a timer is set, it is determined at what Erlang monotonic
 * time, in clock ticks, it should be triggered.
 *
 * The 'pos' field of the wheel corresponds to current time of
 * the wheel. That is, it corresponds to Erlang monotonic time in
 * clock tick time unit. The 'pos' field of the wheel is
 * monotonically increased when erts_bump_timers() is called. All
 * timers in the wheel that have a time less than or equal to
 * 'pos' are triggered by the bump operation. The bump operation
 * may however be spread over multiple calls to erts_bump_timers()
 * if there are a lots of timers to trigger.
 *
 * Each scheduler thread maintains its own timer wheel. The timer
 * wheel of a scheduler, however, actually consists of two wheels.
 * A soon wheel and a later wheel.
 *
 *
 * -- The Soon Wheel --
 *
 * The soon wheel contain timers that should be triggered soon.
 * That is, they are soon to be triggered. Each slot in the soon
 * wheel is 1 clock tick wide. The number of slots in the soon
 * wheel is currently 2¹⁴. That is, it contains timers in the
 * range ('pos', 'pos' + 2¹⁴] which corresponds to a bit more
 * than 16 seconds.
 *
 * When the bump operation is started, 'pos' is moved forward to a
 * position that corresponds to current Erlang monotonic time. Then
 * all timers that are in the range (old 'pos', new 'pos'] are
 * triggered. During a bump operation, the soon wheel may contain
 * timers in the two, possibly overlapping, ranges (old 'pos',
 * old 'pos' + 2¹⁴], and (new 'pos', new 'pos' + 2¹⁴]. This may
 * occur even if the bump operation doesn't yield, due to timeout
 * callbacks inserting new timers.
 *
 *
 * -- The Later Wheel --
 *
 * The later wheel contain timers that are further away from 'pos'
 * than the width of the soon timer wheel. That is, currently
 * timers further away from 'pos' than 2¹⁴ clock ticks. The width
 * of each slot in the later wheel is half the width of the soon
 * wheel. That is, each slot is currently 2¹³ clock ticks wide
 * which corresponds to about 8 seconds. If three timers of the
 * times 'pos' + 17000, 'pos' + 18000, and 'pos' + 19000 are
 * inserted, they will all end up in the same slot in the later
 * wheel.
 *
 * The number of slots in the later wheel is currently the same as
 * in the soon wheel, i.e. 2¹⁴. That is, one revolution of the later
 * wheel currently corresponds to 2¹⁴×2¹³ clock ticks which is
 * almost 37 ½ hour. Timers even further away than that are put in
 * the later slot identified by their time modulo the size of the later
 * wheel. Such timers are however very uncommon. Most timers used
 * by the runtime system will utilize the high level timer API.
 * The high level timer implementation will not insert timers
 * further away then one revolution into the later wheel. It will
 * instead keep such timers in a tree of very long timers. The
 * high level timer implementation utilize one timer wheel timer
 * for the management of this tree of timers. This timer is set to
 * the closest timeout in the tree. This timer may however be
 * further away than one revolution in the later wheel.
 *
 * The 'later.pos' field identifies next position in the later wheel.
 * 'later.pos' is always increased by the width of a later wheel slot.
 * That is, currently 2¹³ clock ticks. When 'pos' is moved (during
 * a bump operation) closer to 'later.pos' than the width of a later
 * wheel slot, i.e. currently when 'pos' + 2¹³ ≥ 'later.pos', we
 * inspect the slot identified by 'later.pos' and then move 'later.pos'
 * forward. When inspecting the later slot we move all timers in the
 * slot, that are in the soon wheel range, from the later wheel to
 * the soon wheel. Timers one or more revolutions of the later wheel
 * away are kept in the slot.
 *
 * During normal operation, timers originally located in the later
 * wheel will currently be moved into the soon wheel about 8 to
 * 16 seconds before they should be triggered. During extremely
 * heavy load, the scheduler might however be heavily delayed, so
 * the code must be prepared for situations where time for
 * triggering the timer has passed when we inspect the later wheel
 * slot, and then trigger the timer immediately. We must also be
 * prepared to inspect multiple later wheel slots at once due to the
 * delay.
 *
 *
 * -- Slot Management --
 *
 * All timers of a slot are placed in a circular double linked
 * list. This makes insertion and removal of a timer O(1).
 *
 * While bumping timers in a slot, we move the circular list
 * away from the slot, and refer to it from the 'sentinel'
 * field. The list will stay there until we are done with it
 * even if the bump operation should yield. The cancel operation
 * can remove the timer from this position as well as from the
 * slot position by just removing it from the circular double
 * linked list that it is in.
 *
 * -- At Once List --
 *
 * If a timer is set that has a time earlier or equal to 'pos',
 * it is not inserted into the wheel. It is instead inserted,
 * into a list referred to by the 'at_once' field. When the
 * bump operation is performed thise timers will be triggered
 * at once.
 *
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
#  define ERTS_TW_HARD_DEBUG
#endif

#if defined(ERTS_TW_HARD_DEBUG) && !defined(ERTS_TW_DEBUG)
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
#  define ERTS_TWHEEL_BUMP_YIELD_LIMIT        500
#else
#  define ERTS_TWHEEL_BUMP_YIELD_LIMIT        10000
#endif
#define ERTS_TW_COST_SLOT                     1
#define ERTS_TW_COST_SLOT_MOVE                5
#define ERTS_TW_COST_TIMEOUT                  100

/*
 * Every slot in the soon wheel is a clock tick (as defined
 * by ERTS) wide. A clock tick is currently 1 milli second.
 */

#define ERTS_TW_SOON_WHEEL_FIRST_SLOT 0
#define ERTS_TW_SOON_WHEEL_END_SLOT \
    (ERTS_TW_SOON_WHEEL_FIRST_SLOT + ERTS_TW_SOON_WHEEL_SIZE)

#define ERTS_TW_SOON_WHEEL_MASK (ERTS_TW_SOON_WHEEL_SIZE-1)

/*
 * Every slot in the later wheel is as wide as half the size
 * of the soon wheel.
 */

#define ERTS_TW_LATER_WHEEL_SHIFT (ERTS_TW_SOON_WHEEL_BITS - 1)
#define ERTS_TW_LATER_WHEEL_SLOT_SIZE \
    ((ErtsMonotonicTime) (1 << ERTS_TW_LATER_WHEEL_SHIFT))
#define ERTS_TW_LATER_WHEEL_POS_MASK \
    (~((ErtsMonotonicTime) (1 << ERTS_TW_LATER_WHEEL_SHIFT)-1))

#define ERTS_TW_LATER_WHEEL_FIRST_SLOT ERTS_TW_SOON_WHEEL_SIZE
#define ERTS_TW_LATER_WHEEL_END_SLOT \
    (ERTS_TW_LATER_WHEEL_FIRST_SLOT + ERTS_TW_LATER_WHEEL_SIZE)

#define ERTS_TW_LATER_WHEEL_MASK (ERTS_TW_LATER_WHEEL_SIZE-1)

/* Actual interval time chosen by sys_init_time() */

#if SYS_CLOCK_RESOLUTION == 1
#  define TIW_ITIME 1
#  define TIW_ITIME_IS_CONSTANT
#else
static int tiw_itime; /* Constant after init */
#  define TIW_ITIME tiw_itime
#endif

struct ErtsTimerWheel_ {
    ErtsTWheelTimer *w[ERTS_TW_SOON_WHEEL_SIZE + ERTS_TW_LATER_WHEEL_SIZE];
    ErtsMonotonicTime pos;
    Uint nto;
    struct {
	ErtsTWheelTimer *head;
	ErtsTWheelTimer *tail;
	Uint nto;
    } at_once;
    struct {
        Uint nto;
    } soon;
    struct {
        ErtsMonotonicTime pos;
    } later;
    int yield_slot;
    int yield_slots_left;
    ErtsTWheelTimer sentinel;
    int true_next_timeout_time;
    ErtsMonotonicTime next_timeout_time;
};

#define ERTS_TW_BUMP_LATER_WHEEL(TIW) \
    ((tiw)->pos + ERTS_TW_LATER_WHEEL_SLOT_SIZE >= (TIW)->later.pos)

static int bump_later_wheel(ErtsTimerWheel *tiw, int *yield_count_p);

static ERTS_INLINE int
soon_slot(ErtsMonotonicTime soon_pos)
{
    ErtsMonotonicTime slot = soon_pos;
    slot &= ERTS_TW_SOON_WHEEL_MASK;

    ERTS_TW_ASSERT(ERTS_TW_SOON_WHEEL_FIRST_SLOT <= slot);
    ERTS_TW_ASSERT(slot < ERTS_TW_SOON_WHEEL_END_SLOT);

    return (int) slot;
}

static ERTS_INLINE int
later_slot(ErtsMonotonicTime later_pos)
{
    ErtsMonotonicTime slot = later_pos;
    slot >>= ERTS_TW_LATER_WHEEL_SHIFT;
    slot &= ERTS_TW_LATER_WHEEL_MASK;
    slot += ERTS_TW_LATER_WHEEL_FIRST_SLOT;

    ERTS_TW_ASSERT(ERTS_TW_LATER_WHEEL_FIRST_SLOT <= slot);
    ERTS_TW_ASSERT(slot < ERTS_TW_LATER_WHEEL_END_SLOT);

    return (int) slot;
}

#ifdef ERTS_TW_HARD_DEBUG
#define ERTS_HARD_DBG_CHK_WHEELS(TIW, CHK_MIN_TPOS) \
    hrd_dbg_check_wheels((TIW), (CHK_MIN_TPOS))
static void hrd_dbg_check_wheels(ErtsTimerWheel *tiw, int check_min_tpos);
#else
#define ERTS_HARD_DBG_CHK_WHEELS(TIW, CHK_MIN_TPOS)
#endif

static ERTS_INLINE ErtsMonotonicTime
find_next_timeout(ErtsSchedulerData *esdp,
		  ErtsTimerWheel *tiw,
		  int search_all,
		  ErtsMonotonicTime curr_time,       /* When !search_all */
		  ErtsMonotonicTime max_search_time) /* When !search_all */
{
    int start_ix, tiw_pos_ix, pos_inc, wheel_start_ix, wheel_end_ix;
    int true_min_timeout = 0;
    ErtsMonotonicTime min_timeout, min_timeout_pos, slot_timeout_pos;

    ERTS_HARD_DBG_CHK_WHEELS(tiw, 0);

    ERTS_TW_ASSERT(tiw->yield_slot == ERTS_TWHEEL_SLOT_INACTIVE);

    if (tiw->nto == 0) { /* no timeouts in wheel */
	if (!search_all)
	    min_timeout_pos = tiw->pos;
	else {
	    curr_time = erts_get_monotonic_time(esdp);
	    tiw->pos = min_timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time);
	}
	min_timeout_pos += ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_DAY);
	goto done;
    }

    if (search_all)
        min_timeout_pos = tiw->pos + ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_DAY);
    else
        min_timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time + max_search_time);

    if (!tiw->soon.nto) {
        /* Select later wheel... */
        slot_timeout_pos = tiw->later.pos;
        start_ix = tiw_pos_ix = later_slot(slot_timeout_pos);
        pos_inc = ERTS_TW_LATER_WHEEL_SLOT_SIZE;
        wheel_start_ix = ERTS_TW_LATER_WHEEL_FIRST_SLOT;
        wheel_end_ix = ERTS_TW_LATER_WHEEL_END_SLOT;
        /* Pre-timeout for move from later to soon wheel... */
        slot_timeout_pos -= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
    }
    else {
        /* Select soon wheel... */

        /*
         * Besides inspecting the soon wheel we
         * may also have to inspect two slots in the
         * later wheel which potentially can trigger
         * timeouts before timeouts in soon wheel...
         */
        slot_timeout_pos = tiw->later.pos;
        slot_timeout_pos -= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
        if (slot_timeout_pos < min_timeout_pos) {
            int fslot = later_slot(tiw->later.pos);
            if (tiw->w[fslot]) {
                min_timeout_pos = slot_timeout_pos;
                true_min_timeout = 1;
            }
            else {
                slot_timeout_pos += ERTS_TW_LATER_WHEEL_SLOT_SIZE;
                if (slot_timeout_pos < min_timeout_pos) {
                    fslot++;
                    if (fslot == ERTS_TW_LATER_WHEEL_END_SLOT)
                        fslot = ERTS_TW_LATER_WHEEL_FIRST_SLOT;
                    if (tiw->w[fslot]) {
                        min_timeout_pos = slot_timeout_pos;
                        true_min_timeout = 1;
                    }
                }
            }
        }

        slot_timeout_pos = tiw->pos;
        start_ix = tiw_pos_ix = soon_slot(tiw->pos);
        pos_inc = 1;
        wheel_start_ix = ERTS_TW_SOON_WHEEL_FIRST_SLOT;
        wheel_end_ix = ERTS_TW_SOON_WHEEL_END_SLOT;
    }

    /*
     * Scan selected wheel...
     */
    do {
        if (tiw->w[tiw_pos_ix]) {
            min_timeout_pos = slot_timeout_pos;
            true_min_timeout = 1;
            break;
        }

        slot_timeout_pos += pos_inc;
        if (slot_timeout_pos >= min_timeout_pos)
            break;

        tiw_pos_ix++;
        if (tiw_pos_ix == wheel_end_ix)
            tiw_pos_ix = wheel_start_ix;
    } while (start_ix != tiw_pos_ix);

done:

    min_timeout = ERTS_CLKTCKS_TO_MONOTONIC(min_timeout_pos);
    tiw->next_timeout_time = min_timeout;
    tiw->true_next_timeout_time = true_min_timeout;

    ERTS_HARD_DBG_CHK_WHEELS(tiw, 1);

    return min_timeout;
}

static ERTS_INLINE void
insert_timer_into_at_once_list(ErtsTimerWheel *tiw, ErtsTWheelTimer *p)
{
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
    p->slot = ERTS_TWHEEL_SLOT_AT_ONCE;
}

static ERTS_INLINE void
insert_timer_into_slot(ErtsTimerWheel *tiw, int slot, ErtsTWheelTimer *p)
{
    ERTS_TW_ASSERT(ERTS_TW_SOON_WHEEL_FIRST_SLOT <= slot);
    ERTS_TW_ASSERT(slot < ERTS_TW_LATER_WHEEL_END_SLOT);
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
    if (slot < ERTS_TW_SOON_WHEEL_END_SLOT) {
        ERTS_TW_ASSERT(p->timeout_pos < tiw->pos + ERTS_TW_SOON_WHEEL_SIZE);
        tiw->soon.nto++;
    }
}

static ERTS_INLINE void
remove_timer(ErtsTimerWheel *tiw, ErtsTWheelTimer *p)
{
    int slot = p->slot;
    ERTS_TW_ASSERT(slot != ERTS_TWHEEL_SLOT_INACTIVE);

    if (slot >= ERTS_TW_SOON_WHEEL_FIRST_SLOT) {
	/*
	 * Timer in wheel or in circular
	 * list of timers currently beeing
	 * triggered (referred by sentinel).
	 */
	ERTS_TW_ASSERT(slot < ERTS_TW_LATER_WHEEL_END_SLOT);

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
        if (slot < ERTS_TW_SOON_WHEEL_END_SLOT)
            tiw->soon.nto--;
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

    ix = soon_slot(tiw->pos);
    tmp = skip_to_pos - tiw->pos;
    ERTS_TW_ASSERT(tmp >= 0);
    if (tmp < (ErtsMonotonicTime) ERTS_TW_SOON_WHEEL_SIZE)
	slots = (int) tmp;
    else
	slots = ERTS_TW_SOON_WHEEL_SIZE;

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
	 if (ix == ERTS_TW_SOON_WHEEL_END_SLOT)
	     ix = ERTS_TW_SOON_WHEEL_FIRST_SLOT;
	 slots--;
    }

    ix = later_slot(tiw->later.pos);
    tmp = skip_to_pos;
    tmp &= ERTS_TW_LATER_WHEEL_POS_MASK;
    if (tmp > tiw->later.pos) {
        tmp -= tiw->later.pos;
        tmp /= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
        ERTS_TW_ASSERT(tmp >= 0);
        if (tmp < (ErtsMonotonicTime) ERTS_TW_LATER_WHEEL_SIZE)
            slots = (int) tmp;
        else
            slots = ERTS_TW_LATER_WHEEL_SIZE;

        while (slots > 0) {
            tmr = tiw->w[ix];
            if (tmr) {
                ErtsMonotonicTime tpos = tmr->timeout_pos;
                ErtsTWheelTimer *end = tmr;
                do {
                    tpos &= ERTS_TW_LATER_WHEEL_POS_MASK;
                    tpos -= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
                    ERTS_TW_ASSERT(tpos > skip_to_pos);
                    tmr = tmr->next;
                } while (tmr != end);
            }
            ix++;
            if (ix == ERTS_TW_LATER_WHEEL_END_SLOT)
                ix = ERTS_TW_LATER_WHEEL_FIRST_SLOT;
            slots--;
        }
    }
}
#endif

static ERTS_INLINE void
timeout_timer(ErtsTWheelTimer *p)
{
    ErlTimeoutProc timeout;
    void *arg;
    p->slot = ERTS_TWHEEL_SLOT_INACTIVE;
    timeout = p->timeout;
    arg = p->arg;
    (*timeout)(arg);
    ASSERT_NO_LOCKED_LOCKS;
}

void
erts_bump_timers(ErtsTimerWheel *tiw, ErtsMonotonicTime curr_time)
{
    int tiw_pos_ix, yielded_slot_restarted, yield_count, slots;
    ErtsMonotonicTime bump_to;
    ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_TIMERS);

    yield_count = ERTS_TWHEEL_BUMP_YIELD_LIMIT;

    /*
     * In order to be fair we always continue with work
     * where we left off when restarting after a yield.
     */

    if (tiw->yield_slot >= ERTS_TW_SOON_WHEEL_FIRST_SLOT) {
	yielded_slot_restarted = 1;
	bump_to = tiw->pos;
        if (tiw->yield_slot >= ERTS_TW_LATER_WHEEL_FIRST_SLOT)
            goto restart_yielded_later_slot;
	tiw_pos_ix = tiw->yield_slot;
	slots = tiw->yield_slots_left;
        ASSERT(0 <= slots && slots <= ERTS_TW_SOON_WHEEL_SIZE);
        tiw->yield_slot = ERTS_TWHEEL_SLOT_INACTIVE;
        goto restart_yielded_soon_slot;
    }

    do {

	yielded_slot_restarted = 0;

	bump_to = ERTS_MONOTONIC_TO_CLKTCKS(curr_time);

	while (1) {
	    ErtsTWheelTimer *p;

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
		if (yield_count <= 0) {
		    ERTS_TW_ASSERT(tiw->nto > 0);
		    ERTS_TW_ASSERT(tiw->at_once.nto > 0);
		    tiw->yield_slot = ERTS_TWHEEL_SLOT_AT_ONCE;
		    tiw->true_next_timeout_time = 1;
		    tiw->next_timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(bump_to);
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

                yield_count -= ERTS_TW_COST_TIMEOUT;

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

                    skip_until_pos++;
                    skip_until_pos &= ERTS_TW_LATER_WHEEL_POS_MASK;
                    if (tiw->later.pos < skip_until_pos)
                        tiw->later.pos = skip_until_pos;
		}
	    }

            {
                ErtsMonotonicTime tmp_slots = bump_to - tiw->pos;
                tmp_slots = (bump_to - tiw->pos);
                if (tmp_slots < ERTS_TW_SOON_WHEEL_SIZE)
                    slots = (int) tmp_slots;
                else
                    slots = ERTS_TW_SOON_WHEEL_SIZE;
            }

            tiw_pos_ix = soon_slot(tiw->pos+1);
	    tiw->pos = bump_to;

            /* Timeout timers in soon wheel */
	    while (slots) {

                yield_count -= ERTS_TW_COST_SLOT;

		p = tiw->w[tiw_pos_ix];
		if (p) {
                    /* timeout callback need tiw->pos to be up to date */
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

                        ERTS_TW_ASSERT(ERTS_TW_SOON_WHEEL_FIRST_SLOT <= p->slot
                                       && p->slot < ERTS_TW_SOON_WHEEL_END_SLOT);
                        tiw->soon.nto--;
                        if (p->timeout_pos <= bump_to) {
                            timeout_timer(p);
                            tiw->nto--;
                            yield_count -= ERTS_TW_COST_TIMEOUT;
                        }
                        else {
                            /* uncommon case */
                            insert_timer_into_slot(tiw, tiw_pos_ix, p);
                            yield_count -= ERTS_TW_COST_SLOT_MOVE;
                        }

		    restart_yielded_soon_slot:

			p = tiw->sentinel.next;
			if (p == &tiw->sentinel) {
			    ERTS_TW_ASSERT(tiw->sentinel.prev == &tiw->sentinel);
			    break;
			}

			if (yield_count <= 0) {
			    tiw->true_next_timeout_time = 1;
			    tiw->next_timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(bump_to);
			    tiw->yield_slot = tiw_pos_ix;
			    tiw->yield_slots_left = slots;
                            ERTS_MSACC_POP_STATE_M_X();
			    return; /* Yield! */
			}

			tiw->sentinel.next = p->next;
			p->next->prev = &tiw->sentinel;
		    }
		}

                tiw_pos_ix++;
                if (tiw_pos_ix == ERTS_TW_SOON_WHEEL_END_SLOT)
                    tiw_pos_ix = ERTS_TW_SOON_WHEEL_FIRST_SLOT;
                slots--;
	    }

            if (ERTS_TW_BUMP_LATER_WHEEL(tiw)) {
            restart_yielded_later_slot:
                if (bump_later_wheel(tiw, &yield_count))
                    return; /* Yield! */
            }
	}

    } while (yielded_slot_restarted);

    tiw->true_next_timeout_time = 0;
    tiw->next_timeout_time = curr_time + ERTS_MONOTONIC_DAY;

    /* Search at most two seconds ahead... */
    (void) find_next_timeout(NULL, tiw, 0, curr_time, ERTS_SEC_TO_MONOTONIC(2));
    ERTS_MSACC_POP_STATE_M_X();
}

static int
bump_later_wheel(ErtsTimerWheel *tiw, int *ycount_p)
{
    ErtsMonotonicTime cpos = tiw->pos;
    ErtsMonotonicTime later_pos = tiw->later.pos;
    int ycount = *ycount_p;
    int slots, fslot;

    ERTS_HARD_DBG_CHK_WHEELS(tiw, 0);

    if (tiw->yield_slot >= ERTS_TW_LATER_WHEEL_FIRST_SLOT) {
        fslot = tiw->yield_slot;
        slots = tiw->yield_slots_left;
        ASSERT(0 <= slots && slots <= ERTS_TW_LATER_WHEEL_SIZE);
        tiw->yield_slot = ERTS_TWHEEL_SLOT_INACTIVE;
        goto restart_yielded_slot;
    }
    else {
        ErtsMonotonicTime tmp_slots = cpos;
        tmp_slots += ERTS_TW_LATER_WHEEL_SLOT_SIZE;
        tmp_slots -= later_pos;
        tmp_slots /= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
        if (tmp_slots < ERTS_TW_LATER_WHEEL_SIZE)
            slots = (int) tmp_slots;
        else
            slots = ERTS_TW_LATER_WHEEL_SIZE;
    }

    while (slots >= 0) {
        ErtsTWheelTimer *p;

        fslot = later_slot(later_pos);

        ycount -= ERTS_TW_COST_SLOT;

        p = tiw->w[fslot];

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
            tiw->w[fslot] = NULL;

            while (1) {
                ErtsMonotonicTime tpos = p->timeout_pos;

                ERTS_TW_ASSERT(tpos >= later_pos);
                ERTS_TW_ASSERT(p->slot == fslot);

                if (tpos >= later_pos + ERTS_TW_LATER_WHEEL_SLOT_SIZE) {
                    /* keep in later slot; very uncommon... */
                    insert_timer_into_slot(tiw, fslot, p);
                    ycount -= ERTS_TW_COST_SLOT_MOVE;
                }
                else {
                    ERTS_TW_ASSERT(tpos < cpos + ERTS_TW_SOON_WHEEL_SIZE);
                    if (tpos > cpos) {
                        /* move into soon wheel */
                        insert_timer_into_slot(tiw, soon_slot(tpos), p);
                        ycount -= ERTS_TW_COST_SLOT_MOVE;
                    }
                    else {
                        /* trigger at once */
                        timeout_timer(p);
                        tiw->nto--;
                        ycount -= ERTS_TW_COST_TIMEOUT;
                    }
                }

            restart_yielded_slot:

                p = tiw->sentinel.next;
                if (p == &tiw->sentinel) {
                    ERTS_TW_ASSERT(tiw->sentinel.prev == &tiw->sentinel);
                    break;
                }

                if (ycount < 0) {
                    tiw->later.pos = later_pos;
		    tiw->true_next_timeout_time = 1;
		    tiw->next_timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(cpos);
                    tiw->yield_slot = fslot;
                    tiw->yield_slots_left = slots;
                    *ycount_p = 0;
                    ERTS_HARD_DBG_CHK_WHEELS(tiw, 0);
                    return 1; /* Yield! */
                }

                tiw->sentinel.next = p->next;
                p->next->prev = &tiw->sentinel;
            }
        }
        slots--;
        later_pos += ERTS_TW_LATER_WHEEL_SLOT_SIZE;
    }

    ERTS_HARD_DBG_CHK_WHEELS(tiw, 0);

    tiw->later.pos = later_pos;

    *ycount_p = ycount;

    return 0;
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
    int i = ERTS_TW_SOON_WHEEL_FIRST_SLOT;
    ErtsTimerWheel *tiw;
    tiw = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_TIMER_WHEEL,
					     sizeof(ErtsTimerWheel));
    for(; i < ERTS_TW_LATER_WHEEL_END_SLOT; i++)
	tiw->w[i] = NULL;

    mtime = erts_get_monotonic_time(esdp);
    tiw->pos = ERTS_MONOTONIC_TO_CLKTCKS(mtime);
    tiw->nto = 0;
    tiw->at_once.head = NULL;
    tiw->at_once.tail = NULL;
    tiw->at_once.nto = 0;
    tiw->soon.nto = 0;
    tiw->later.pos = tiw->pos;
    tiw->later.pos &= ERTS_TW_LATER_WHEEL_POS_MASK;
    tiw->later.pos += ERTS_TW_LATER_WHEEL_SLOT_SIZE;
    tiw->yield_slot = ERTS_TWHEEL_SLOT_INACTIVE;
    tiw->true_next_timeout_time = 0;
    tiw->next_timeout_time = mtime + ERTS_MONOTONIC_DAY;
    tiw->sentinel.next = &tiw->sentinel;
    tiw->sentinel.prev = &tiw->sentinel;
    tiw->sentinel.timeout = NULL;
    tiw->sentinel.arg = NULL;
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
		      void *arg, ErtsMonotonicTime timeout_pos)
{
    ErtsMonotonicTime timeout_time;
    ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_TIMERS);

    p->timeout = timeout;
    p->arg = arg;

    ERTS_TW_ASSERT(p->slot == ERTS_TWHEEL_SLOT_INACTIVE);

    tiw->nto++;
    if (timeout_pos <= tiw->pos) {
        p->timeout_pos = tiw->pos;
        insert_timer_into_at_once_list(tiw, p);
	timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(tiw->pos);
    }
    else {
	int slot;

	p->timeout_pos = timeout_pos;

	/* calculate slot */
        if (timeout_pos < tiw->pos + ERTS_TW_SOON_WHEEL_SIZE) {
            /* soon wheel */
            slot = soon_slot(timeout_pos);
        }
        else {
            /* later wheel */
            slot = later_slot(timeout_pos);

            /*
             * Next timeout due to this timeout
             * should be in good time before the
             * actual timeout (one later wheel slot
             * size). This, in order to move it
             * from the later wheel to the soon
             * wheel.
             */
            timeout_pos &= ERTS_TW_LATER_WHEEL_POS_MASK;
            timeout_pos -= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
        }

	insert_timer_into_slot(tiw, slot, p);
        timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(timeout_pos);
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
        ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_TIMERS);
	remove_timer(tiw, p);
        tiw->nto--;
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
	if (tmr->timeout == tclbk)
	    (*func)(arg, tmr->timeout_pos, tmr->arg);
	tmr = tmr->next;
    }

    for (tmr = tiw->at_once.head; tmr; tmr = tmr->next) {
	if (tmr->timeout == tclbk)
	    (*func)(arg, tmr->timeout_pos, tmr->arg);
    }

    for (ix = ERTS_TW_SOON_WHEEL_FIRST_SLOT;
         ix < ERTS_TW_LATER_WHEEL_END_SLOT;
         ix++) {
	tmr = tiw->w[ix];
	if (tmr) {
	    do {
		if (tmr->timeout == tclbk)
		    (*func)(arg, tmr->timeout_pos, tmr->arg);
		tmr = tmr->next;
	    } while (tmr != tiw->w[ix]);
	}
    }
}

#ifdef ERTS_TW_HARD_DEBUG

static void
hrd_dbg_check_wheels(ErtsTimerWheel *tiw, int check_min_tpos)
{
    int ix, soon_tmo, later_tmo, at_once_tmo;
    ErtsMonotonicTime min_tpos;

    min_tpos = ERTS_MONOTONIC_TO_CLKTCKS(tiw->next_timeout_time);

    soon_tmo = 0;
    for (ix = ERTS_TW_SOON_WHEEL_FIRST_SLOT;
         ix < ERTS_TW_SOON_WHEEL_END_SLOT;
         ix++) {
        ErtsTWheelTimer *p;

        p = tiw->w[ix];
        if (p) {
            ErtsTWheelTimer *first = p;
            do {
                ErtsMonotonicTime tpos = p->timeout_pos;
                ERTS_TW_ASSERT(p->slot == ix);
                soon_tmo++;
                ERTS_TW_ASSERT(ix == soon_slot(tpos));
                ERTS_TW_ASSERT(p->timeout_pos < tiw->pos + ERTS_TW_SOON_WHEEL_SIZE);
                ERTS_TW_ASSERT(!check_min_tpos || tpos >= min_tpos);
                ERTS_TW_ASSERT(p->next->prev == p);
                p = p->next;
            } while (p != first);
        }
    }

    later_tmo = 0;
    for (ix = ERTS_TW_LATER_WHEEL_FIRST_SLOT;
         ix < ERTS_TW_LATER_WHEEL_END_SLOT;
         ix++) {
        ErtsTWheelTimer *p;

        p = tiw->w[ix];
        if (p) {
            ErtsTWheelTimer *first = p;
            do {
                ErtsMonotonicTime tpos = p->timeout_pos;
                ERTS_TW_ASSERT(p->slot == ix);
                later_tmo++;
                ERTS_TW_ASSERT(later_slot(tpos) == ix);
                tpos &= ERTS_TW_LATER_WHEEL_POS_MASK;
                tpos -= ERTS_TW_LATER_WHEEL_SLOT_SIZE;
                ERTS_TW_ASSERT(!check_min_tpos || tpos >= min_tpos);
                ERTS_TW_ASSERT(p->next->prev == p);
                p = p->next;
            } while (p != first);
        }
    }

    if (tiw->yield_slot >= 0) {
        ErtsTWheelTimer *p = tiw->sentinel.next;
        while (p != &tiw->sentinel) {
            ErtsMonotonicTime tpos = p->timeout_pos;
            if (tiw->yield_slot >= ERTS_TW_SOON_WHEEL_SIZE) {
                later_tmo++;
                ERTS_TW_ASSERT(p->slot == later_slot(tpos));
            }
            else {
                soon_tmo++;
                ERTS_TW_ASSERT(p->slot == (tpos & ERTS_TW_SOON_WHEEL_MASK));
                ERTS_TW_ASSERT(tpos < tiw->pos + ERTS_TW_SOON_WHEEL_SIZE);
            }
            p = p->next;
        }
    }

    at_once_tmo = 0;
    if (tiw->at_once.head) {
        ErtsTWheelTimer *p = tiw->at_once.head;
        while (p) {
            ErtsMonotonicTime tpos = p->timeout_pos;
            at_once_tmo++;
            ERTS_TW_ASSERT(tpos <= tiw->pos);
            p = p->next;
        }
    }

    ERTS_TW_ASSERT(tiw->at_once.nto == at_once_tmo);
    ERTS_TW_ASSERT(tiw->soon.nto == soon_tmo);
    ERTS_TW_ASSERT(tiw->nto == soon_tmo + later_tmo + at_once_tmo);
}

#endif
