/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ASSERT_NO_LOCKED_LOCKS		erts_lc_check_exact(NULL, 0)
#else
#define ASSERT_NO_LOCKED_LOCKS
#endif

#define ERTS_MONOTONIC_DAY ERTS_SEC_TO_MONOTONIC(60*60*24)
#define ERTS_CLKTCKS_DAY ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_DAY)


/* BEGIN tiw_lock protected variables 
**
** The individual timer cells in tiw are also protected by the same mutex.
*/

/* timing wheel size NEED to be a power of 2 */
#ifdef SMALL_MEMORY
#define TIW_SIZE (1 << 13)
#else
#define TIW_SIZE (1 << 20)
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
    ErlTimer *w[TIW_SIZE];
    ErtsMonotonicTime pos;
    Uint nto;
    struct {
	ErlTimer *head;
	ErlTimer **tail;
	Uint nto;
    } at_once;
    int true_next_timeout_time;
    ErtsMonotonicTime next_timeout_time;
    erts_atomic64_t next_timeout;
    erts_smp_atomic32_t is_bumping;
    erts_smp_mtx_t lock;
};

ErtsTimerWheel *erts_default_timer_wheel; /* managed by aux thread */

static ERTS_INLINE ErtsTimerWheel *
get_timer_wheel(ErlTimer *p)
{
    return (ErtsTimerWheel *) erts_smp_atomic_read_acqb(&p->wheel);
}

static ERTS_INLINE void
set_timer_wheel(ErlTimer *p, ErtsTimerWheel *tiw)
{
    erts_smp_atomic_set_relb(&p->wheel, (erts_aint_t) tiw);
}

static ERTS_INLINE void
init_next_timeout(ErtsTimerWheel *tiw,
		  ErtsMonotonicTime time)
{
    erts_atomic64_init_nob(&tiw->next_timeout,
			   (erts_aint64_t) time);
}

static ERTS_INLINE void
set_next_timeout(ErtsTimerWheel *tiw,
		 ErtsMonotonicTime time,
		 int true_timeout)
{
    tiw->true_next_timeout_time = true_timeout;
    tiw->next_timeout_time = time;
    erts_atomic64_set_relb(&tiw->next_timeout,
			   (erts_aint64_t) time);
}

/* get the time (in units of TIW_ITIME) to the next timeout,
   or -1 if there are no timeouts                     */

static ERTS_INLINE ErtsMonotonicTime
find_next_timeout(ErtsTimerWheel *tiw,
		  ErtsMonotonicTime curr_time,
		  ErtsMonotonicTime max_search_time)
{
    int start_ix, tiw_pos_ix;
    ErlTimer *p;
    int true_min_timeout;
    ErtsMonotonicTime min_timeout, min_timeout_pos, slot_timeout_pos, timeout_limit;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&tiw->lock));

    if (tiw->true_next_timeout_time)
	return tiw->next_timeout_time;

    /* We never set next timeout beyond timeout_limit */
    timeout_limit = curr_time + ERTS_MONOTONIC_DAY;

    if (tiw->nto == 0) { /* no timeouts in wheel */
	true_min_timeout = tiw->true_next_timeout_time = 0;
	min_timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(timeout_limit);
	goto found_next;
    }

    /*
     * Don't want others entering trying to bump
     * timers while we are checking...
     */
    set_next_timeout(tiw, timeout_limit, 0);

    true_min_timeout = 1;
    slot_timeout_pos = tiw->pos;
    min_timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time + max_search_time);

    start_ix = tiw_pos_ix = (int) (tiw->pos & (TIW_SIZE-1));

    do {
	slot_timeout_pos++;
	if (slot_timeout_pos >= min_timeout_pos) {
	    true_min_timeout = 0;
	    break;
	}

	p = tiw->w[tiw_pos_ix];

	while (p) {
	    ErtsMonotonicTime timeout_pos;
	    ASSERT(p != p->next);
	    timeout_pos = p->timeout_pos;
	    if (min_timeout_pos > timeout_pos) {
		min_timeout_pos = timeout_pos;
		if (min_timeout_pos <= slot_timeout_pos)
		    goto found_next;
	    }
	    p = p->next;
	}

	tiw_pos_ix++;
	if (tiw_pos_ix == TIW_SIZE)
	    tiw_pos_ix = 0;
    } while (start_ix != tiw_pos_ix);

found_next:

    min_timeout = ERTS_CLKTCKS_TO_MONOTONIC(min_timeout_pos);
    if (min_timeout != tiw->next_timeout_time)
	set_next_timeout(tiw, min_timeout, true_min_timeout);

    return min_timeout;
}

static void
remove_timer(ErtsTimerWheel *tiw, ErlTimer *p)
{
    /* first */
    if (!p->prev) {
	tiw->w[p->slot] = p->next;
	if(p->next)
	    p->next->prev = NULL;
    } else {
	p->prev->next = p->next;
    }

    /* last */
    if (!p->next) {
	if (p->prev)
	    p->prev->next = NULL;
    } else {
	p->next->prev = p->prev;
    }

    p->next = NULL;
    p->prev = NULL;

    set_timer_wheel(p, NULL);
    tiw->nto--;
}

ErtsMonotonicTime
erts_check_next_timeout_time(ErtsTimerWheel *tiw,
			     ErtsMonotonicTime max_search_time)
{
    ErtsMonotonicTime next, curr;

    curr = erts_get_monotonic_time();

    erts_smp_mtx_lock(&tiw->lock);

    next = find_next_timeout(tiw, curr, max_search_time);

    erts_smp_mtx_unlock(&tiw->lock);

    return next;
}

#ifndef DEBUG
#define ERTS_DBG_CHK_SAFE_TO_SKIP_TO(TIW, TO) ((void) 0)
#else
#define ERTS_DBG_CHK_SAFE_TO_SKIP_TO(TIW, TO) debug_check_safe_to_skip_to((TIW), (TO))
static void
debug_check_safe_to_skip_to(ErtsTimerWheel *tiw, ErtsMonotonicTime skip_to_pos)
{
    int slots, ix;
    ErlTimer *tmr;
    ErtsMonotonicTime tmp;

    ix = (int) (tiw->pos & (TIW_SIZE-1));
    tmp = skip_to_pos - tiw->pos;
    ASSERT(tmp >= 0);
    if (tmp < (ErtsMonotonicTime) TIW_SIZE)
	slots = (int) tmp;
    else
	slots = TIW_SIZE;

     while (slots > 0) {
	tmr = tiw->w[ix];
	while (tmr) {
	    ASSERT(tmr->timeout_pos > skip_to_pos);
	    tmr = tmr->next;
	}
	ix++;
	if (ix == TIW_SIZE)
	    ix = 0;
	slots--;
    }
}
#endif

void
erts_bump_timers(ErtsTimerWheel *tiw, ErtsMonotonicTime curr_time)
{
    int tiw_pos_ix, slots;
    ErlTimer *p, *timeout_head, **timeout_tail;
    ErtsMonotonicTime bump_to, tmp_slots;

    if (erts_smp_atomic32_cmpxchg_nob(&tiw->is_bumping, 1, 0) != 0)
	return; /* Another thread is currently bumping... */

    bump_to = ERTS_MONOTONIC_TO_CLKTCKS(curr_time);

    erts_smp_mtx_lock(&tiw->lock);

    if (tiw->pos >= bump_to) {
	timeout_head = NULL;
	goto done;
    }

    /* Don't want others here while we are bumping... */
    set_next_timeout(tiw, curr_time + ERTS_MONOTONIC_DAY, 0);

    if (!tiw->at_once.head) {
	timeout_head = NULL;
	timeout_tail = &timeout_head;
    }
    else {
	ASSERT(tiw->nto >= tiw->at_once.nto);
	timeout_head = tiw->at_once.head;
	timeout_tail = tiw->at_once.tail;
	tiw->nto -= tiw->at_once.nto;
	tiw->at_once.head = NULL;
	tiw->at_once.tail = &tiw->at_once.head;
	tiw->at_once.nto = 0;
    }

    if (tiw->nto == 0) {
	ERTS_DBG_CHK_SAFE_TO_SKIP_TO(tiw, bump_to);
	tiw->pos = bump_to;
	goto done;
    }

    if (tiw->true_next_timeout_time) {
	ErtsMonotonicTime skip_until_pos;
	/*
	 * No need inspecting slots where we know no timeouts
	 * to trigger should reside.
	 */

	skip_until_pos = ERTS_MONOTONIC_TO_CLKTCKS(tiw->next_timeout_time);
	if (skip_until_pos > bump_to)
	    skip_until_pos = bump_to;

	ERTS_DBG_CHK_SAFE_TO_SKIP_TO(tiw, skip_until_pos);
	ASSERT(skip_until_pos > tiw->pos);

	tiw->pos = skip_until_pos - 1;
    }

    tiw_pos_ix = (int) ((tiw->pos+1) & (TIW_SIZE-1));
    tmp_slots = (bump_to - tiw->pos);
    if (tmp_slots < (ErtsMonotonicTime) TIW_SIZE)
	slots = (int) tmp_slots;
    else
	slots = TIW_SIZE;
 
    while (slots > 0) {
	p = tiw->w[tiw_pos_ix];
	while (p) {
	    ErlTimer *next = p->next;
	    ASSERT(p != next);
	    if (p->timeout_pos <= bump_to) { /* we have a timeout */
		/* Remove from list */
		remove_timer(tiw, p);
		*timeout_tail = p;	/* Insert in timeout queue */
		timeout_tail = &p->next;
	    }
	    p = next;
	}
	tiw_pos_ix++;
	if (tiw_pos_ix == TIW_SIZE)
	    tiw_pos_ix = 0;
	slots--;
    }

    ASSERT(tmp_slots >= (ErtsMonotonicTime) TIW_SIZE
	   || tiw_pos_ix == (int) ((bump_to+1) & (TIW_SIZE-1)));

    tiw->pos = bump_to;

    /* Search at most two seconds ahead... */
    (void) find_next_timeout(tiw, curr_time, ERTS_SEC_TO_MONOTONIC(2));

done:

    erts_smp_mtx_unlock(&tiw->lock);
    
    erts_smp_atomic32_set_nob(&tiw->is_bumping, 0);

    /* Call timedout timers callbacks */
    while (timeout_head) {
	ErlTimeoutProc timeout;
	void *arg;
	p = timeout_head;
	timeout_head = p->next;
	/* Here comes hairy use of the timer fields!
	 * They are reset without having the lock.
	 * It is assumed that no code but this will
	 * accesses any field until the ->timeout
	 * callback is called.
	 */
	ASSERT(p->timeout_pos <= bump_to);
	p->next = NULL;
	p->prev = NULL;
	p->slot = 0;
	timeout = p->timeout;
	arg = p->arg;
	(*timeout)(arg);
    }
}

Uint
erts_timer_wheel_memory_size(void)
{
#ifdef ERTS_SMP
    return sizeof(ErtsTimerWheel)*(1 + erts_no_schedulers);
#else
    return sizeof(ErtsTimerWheel);
#endif
}

ErtsTimerWheel *
erts_create_timer_wheel(int no)
{
    ErtsMonotonicTime mtime;
    int i;
    ErtsTimerWheel *tiw;
    tiw = (ErtsTimerWheel *) erts_alloc(ERTS_ALC_T_TIMER_WHEEL,
					sizeof(ErtsTimerWheel));
    for(i = 0; i < TIW_SIZE; i++)
	tiw->w[i] = NULL;

    erts_smp_atomic32_init_nob(&tiw->is_bumping, 0);
    erts_smp_mtx_init_x(&tiw->lock, "timer_wheel", make_small(no));

    mtime = erts_get_monotonic_time();
    tiw->pos = ERTS_MONOTONIC_TO_CLKTCKS(mtime);
    tiw->nto = 0;
    tiw->at_once.head = NULL;
    tiw->at_once.tail = &tiw->at_once.head;
    tiw->at_once.nto = 0;
    tiw->true_next_timeout_time = 0;
    tiw->next_timeout_time = mtime + ERTS_MONOTONIC_DAY;
    init_next_timeout(tiw, mtime + ERTS_MONOTONIC_DAY);
    return tiw;
}

ErtsNextTimeoutRef
erts_get_next_timeout_reference(ErtsTimerWheel *tiw)
{
    return (ErtsNextTimeoutRef) &tiw->next_timeout;
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
	erl_exit(ERTS_ABORT_EXIT, "timer resolution mismatch %d != %d", itime, TIW_ITIME);
    }
#else
    tiw_itime = itime;
#endif

    erts_default_timer_wheel = erts_create_timer_wheel(0);
}

void
erts_set_timer(ErlTimer *p, ErlTimeoutProc timeout,
	       ErlCancelProc cancel, void *arg, Uint to)
{
    ErtsMonotonicTime timeout_time, timeout_pos;
    ErtsMonotonicTime curr_time;
    ErtsTimerWheel *tiw;
    ErtsSchedulerData *esdp;

    curr_time = erts_get_monotonic_time();
    esdp = erts_get_scheduler_data();
    if (esdp)
	tiw = esdp->timer_wheel;
    else
	tiw = erts_default_timer_wheel;

    erts_smp_mtx_lock(&tiw->lock);

    if (get_timer_wheel(p))
	ERTS_INTERNAL_ERROR("Double set timer");

    p->timeout = timeout;
    p->cancel = cancel;
    p->arg = arg;

    if (to == 0) {
	timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time);
	tiw->nto++;
	tiw->at_once.nto++;
	*tiw->at_once.tail = p;
	tiw->at_once.tail = &p->next;
	p->next = NULL;
	p->timeout_pos = timeout_pos;
	timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(timeout_pos);
    }
    else {
	int tm;
	ErtsMonotonicTime ticks;

	ticks = ERTS_MSEC_TO_CLKTCKS(to);
	timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(curr_time - 1) + 1 + ticks;

	/* calculate slot */
	tm = (int) (timeout_pos & (TIW_SIZE-1));
	p->slot = (Uint) tm;
  
	/* insert at head of list at slot */
	p->next = tiw->w[tm];
	p->prev = NULL;
	if (p->next != NULL)
	    p->next->prev = p;
	tiw->w[tm] = p;

	tiw->nto++;

	timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(timeout_pos);
	p->timeout_pos = timeout_pos;

	ASSERT(ERTS_MSEC_TO_MONOTONIC(to) <= timeout_time - curr_time);
	ASSERT(timeout_time - curr_time
	       < ERTS_MSEC_TO_MONOTONIC(to) + ERTS_CLKTCKS_TO_MONOTONIC(1));
    }

    if (timeout_time < tiw->next_timeout_time)
	set_next_timeout(tiw, timeout_time, 1);

    set_timer_wheel(p, tiw);

    erts_smp_mtx_unlock(&tiw->lock);

#if defined(ERTS_SMP)
    if (tiw == erts_default_timer_wheel)
	erts_interupt_aux_thread_timed(timeout_time);
#endif

}

void
erts_cancel_timer(ErlTimer *p)
{
    ErtsTimerWheel *tiw;
    ErlCancelProc cancel;
    void *arg;

    tiw = get_timer_wheel(p);
    if (!tiw)
	return;
    
    erts_smp_mtx_lock(&tiw->lock);
    if (tiw != get_timer_wheel(p))
	cancel = NULL;
    else {
	remove_timer(tiw, p);
	p->slot = 0;

	cancel = p->cancel;
	arg = p->arg;
    }
    erts_smp_mtx_unlock(&tiw->lock);

    if (cancel)
	(*cancel)(arg);
}

/*
  Returns the amount of time left in ms until the timer 'p' is triggered.
  0 is returned if 'p' isn't active.
  0 is returned also if the timer is overdue (i.e., would have triggered
  immediately if it hadn't been cancelled).
*/
Uint
erts_time_left(ErlTimer *p)
{
    ErtsTimerWheel *tiw;
    ErtsMonotonicTime current_time, timeout_time;

    tiw = get_timer_wheel(p);
    if (!tiw)
	return 0;

    erts_smp_mtx_lock(&tiw->lock);
    if (tiw != get_timer_wheel(p))
	timeout_time = ERTS_MONOTONIC_TIME_MIN;
    else
	timeout_time = ERTS_CLKTCKS_TO_MONOTONIC(p->timeout_pos);
    erts_smp_mtx_unlock(&tiw->lock);

    current_time = erts_get_monotonic_time();
    if (timeout_time <= current_time)
	return 0;
    return (Uint) ERTS_MONOTONIC_TO_MSEC(timeout_time - current_time);
}

#ifdef DEBUG
void erts_p_slpq(void)
{
    ErtsTimerWheel *tiw = erts_default_timer_wheel;
    ErtsMonotonicTime current_time = erts_get_monotonic_time();
    int i;
    ErlTimer* p;
  
    erts_smp_mtx_lock(&tiw->lock);

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
    for(i = ((i+1) & (TIW_SIZE-1)); i != (tiw->pos & (TIW_SIZE-1)); i = ((i+1) & (TIW_SIZE-1))) {
	if (tiw->w[i] != NULL) {
	    erts_printf("%d:\n", i);
	    for(p = tiw->w[i]; p != NULL; p = p->next) {
		erts_printf(" (timeout time %bps, slot %d)\n",
			    ERTS_CLKTCKS_TO_MONOTONIC(p->timeout_pos), p->slot);
	    }
	}
    }

    erts_smp_mtx_unlock(&tiw->lock);
}
#endif /* DEBUG */
