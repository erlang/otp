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

static erts_smp_mtx_t tiw_lock;


/* BEGIN tiw_lock protected variables 
**
** The individual timer cells in tiw are also protected by the same mutex.
*/

#ifdef SMALL_MEMORY
#define TIW_SIZE 8192
#else
#define TIW_SIZE 65536		/* timing wheel size (should be a power of 2) */
#endif
static ErlTimer** tiw;		/* the timing wheel, allocated in init_time() */
static Uint tiw_pos;		/* current position in wheel */
static Uint tiw_nto;		/* number of timeouts in wheel */
static Uint tiw_min;
static ErlTimer *tiw_min_ptr;

/* END tiw_lock protected variables */

/* Actual interval time chosen by sys_init_time() */

#if SYS_CLOCK_RESOLUTION == 1
#  define TIW_ITIME 1
#  define TIW_ITIME_IS_CONSTANT
#else
static int tiw_itime; /* Constant after init */
#  define TIW_ITIME tiw_itime
#endif

erts_smp_atomic32_t do_time;	/* set at clock interrupt */
static ERTS_INLINE erts_short_time_t do_time_read(void)
{
    return erts_smp_atomic32_read_acqb(&do_time);
}

static ERTS_INLINE erts_short_time_t do_time_update(void)
{
    return do_time_read();
}

static ERTS_INLINE void do_time_init(void)
{
    erts_smp_atomic32_init_nob(&do_time, 0);
}

/* get the time (in units of TIW_ITIME) to the next timeout,
   or -1 if there are no timeouts                     */

static erts_short_time_t next_time_internal(void) /* PRE: tiw_lock taken by caller */
{
    int i, tm, nto;
    Uint32 min;
    ErlTimer* p;
    erts_short_time_t dt;
  
    if (tiw_nto == 0)
	return -1;	/* no timeouts in wheel */

    if (tiw_min_ptr) {
	min = tiw_min;
	dt  = do_time_read();
	return ((min >= dt) ? (min - dt) : 0);
    }
  
    /* start going through wheel to find next timeout */
    tm = nto = 0;
    min = (Uint32) -1;	/* max Uint32 */
    i = tiw_pos;
    do {
	p = tiw[i];
	while (p != NULL) {
	    nto++;
	    if (p->count == 0) {
		/* found next timeout */
		dt = do_time_read();
		/* p->count is zero */
		tiw_min_ptr = p;
		tiw_min     = tm;
		return ((tm >= dt) ? (tm - dt) : 0);
	    } else {
		/* keep shortest time in 'min' */
		if (tm + p->count*TIW_SIZE < min) {
		    min = tm + p->count*TIW_SIZE;
		    tiw_min_ptr = p;
		    tiw_min     = min;
		}
	    }
	    p = p->next;
	}
	/* when we have found all timeouts the shortest time will be in min */
	if (nto == tiw_nto) break;
	tm++;
	i = (i + 1) % TIW_SIZE;
    } while (i != tiw_pos);
    dt = do_time_read();
    if (min <= (Uint32) dt)
	return 0;
    if ((min - (Uint32) dt) > (Uint32) ERTS_SHORT_TIME_T_MAX)
	return ERTS_SHORT_TIME_T_MAX;
    return (erts_short_time_t) (min - (Uint32) dt);
}

static void remove_timer(ErlTimer *p) {
    /* first */
    if (!p->prev) {
	tiw[p->slot] = p->next;
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
    /* Make sure cancel callback isn't called */
    p->active = 0;
    tiw_nto--;
}

/* Private export to erl_time_sup.c */
erts_short_time_t erts_next_time(void)
{
    erts_short_time_t ret;

    erts_smp_mtx_lock(&tiw_lock);
    (void)do_time_update();
    ret = next_time_internal();
    erts_smp_mtx_unlock(&tiw_lock);
    return ret;
}

static ERTS_INLINE void bump_timer_internal(erts_short_time_t dt) /* PRE: tiw_lock is write-locked */
{
    Uint keep_pos;
    Uint count;
    ErlTimer *p, **prev, *timeout_head, **timeout_tail;
    Uint dtime = (Uint) dt;  

    /* no need to bump the position if there aren't any timeouts */
    if (tiw_nto == 0) {
	erts_smp_mtx_unlock(&tiw_lock);
	return;
    }

    /* if do_time > TIW_SIZE we want to go around just once */
    count = (Uint)(dtime / TIW_SIZE) + 1;
    keep_pos = (tiw_pos + dtime) % TIW_SIZE;
    if (dtime > TIW_SIZE) dtime = TIW_SIZE;
  
    timeout_head = NULL;
    timeout_tail = &timeout_head;
    while (dtime > 0) {
	/* this is to decrease the counters with the right amount */
	/* when dtime >= TIW_SIZE */
	if (tiw_pos == keep_pos) count--;
	prev = &tiw[tiw_pos];
	while ((p = *prev) != NULL) {
	    ASSERT( p != p->next);
	    if (p->count < count) {     /* we have a timeout */
		/* remove min time */
		if (tiw_min_ptr == p) {
		    tiw_min_ptr = NULL;
		    tiw_min = 0;
		}

		/* Remove from list */
		remove_timer(p);
		*timeout_tail = p;	/* Insert in timeout queue */
		timeout_tail = &p->next;
	    }
	    else {
		/* no timeout, just decrease counter */
		p->count -= count;
		prev = &p->next;
	    }
	}
	tiw_pos = (tiw_pos + 1) % TIW_SIZE;
	dtime--;
    }
    tiw_pos = keep_pos;
    if (tiw_min_ptr)
	tiw_min -= dt;
    
    erts_smp_mtx_unlock(&tiw_lock);
    
    /* Call timedout timers callbacks */
    while (timeout_head) {
	p = timeout_head;
	timeout_head = p->next;
	/* Here comes hairy use of the timer fields!
	 * They are reset without having the lock.
	 * It is assumed that no code but this will
	 * accesses any field until the ->timeout
	 * callback is called.
	 */
	p->next = NULL;
	p->prev = NULL;
	p->slot = 0;
	(*p->timeout)(p->arg);
    }
}

void erts_bump_timer(erts_short_time_t dt) /* dt is value from do_time */
{
    erts_smp_mtx_lock(&tiw_lock);
    bump_timer_internal(dt);
}

Uint
erts_timer_wheel_memory_size(void)
{
    return (Uint) TIW_SIZE * sizeof(ErlTimer*);
}

/* this routine links the time cells into a free list at the start
   and sets the time queue as empty */
void
erts_init_time(void)
{
    int i, itime;

    /* system dependent init; must be done before do_time_init()
       if timer thread is enabled */
    itime = erts_init_time_sup();
#ifdef TIW_ITIME_IS_CONSTANT 
    if (itime != TIW_ITIME) {
	erl_exit(ERTS_ABORT_EXIT, "timer resolution mismatch %d != %d", itime, TIW_ITIME);
    }
#else
    tiw_itime = itime;
#endif

    erts_smp_mtx_init(&tiw_lock, "timer_wheel");

    tiw = (ErlTimer**) erts_alloc(ERTS_ALC_T_TIMER_WHEEL,
				  TIW_SIZE * sizeof(ErlTimer*));
    for(i = 0; i < TIW_SIZE; i++)
	tiw[i] = NULL;
    do_time_init();
    tiw_pos = tiw_nto = 0;
    tiw_min_ptr = NULL;
    tiw_min = 0;
}




/*
** Insert a process into the time queue, with a timeout 't'
*/
static void
insert_timer(ErlTimer* p, Uint t)
{
    Uint tm;
    Uint64 ticks;

    /* The current slot (tiw_pos) in timing wheel is the next slot to be
     * be processed. Hence no extra time tick is needed.
     *
     * (x + y - 1)/y is precisely the "number of bins" formula.
     */
    ticks = (t + (TIW_ITIME - 1)) / TIW_ITIME;

    /* 
     * Ticks must be a Uint64, or the addition may overflow here,
     * resulting in an incorrect value for p->count below.
     */
    ticks += do_time_update(); /* Add backlog of unprocessed time */
    
    /* calculate slot */
    tm = (ticks + tiw_pos) % TIW_SIZE;
    p->slot = (Uint) tm;
    p->count = (Uint) (ticks / TIW_SIZE);
  
    /* insert at head of list at slot */
    p->next = tiw[tm];
    p->prev = NULL;
    if (p->next != NULL)
	p->next->prev = p;
    tiw[tm] = p;


    /* insert min time */
    if ((tiw_nto == 0) || ((tiw_min_ptr != NULL) && (ticks < tiw_min))) {
	tiw_min = ticks;
	tiw_min_ptr = p;
    }
    if ((tiw_min_ptr == p) && (ticks > tiw_min)) {
	/* some other timer might be 'min' now */
	tiw_min = 0;
	tiw_min_ptr = NULL;
    }

    tiw_nto++;
}

void
erts_set_timer(ErlTimer* p, ErlTimeoutProc timeout, ErlCancelProc cancel,
	      void* arg, Uint t)
{

    erts_deliver_time();
    erts_smp_mtx_lock(&tiw_lock);
    if (p->active) { /* XXX assert ? */
	erts_smp_mtx_unlock(&tiw_lock);
	return;
    }
    p->timeout = timeout;
    p->cancel = cancel;
    p->arg = arg;
    p->active = 1;
    insert_timer(p, t);
    erts_smp_mtx_unlock(&tiw_lock);
#if defined(ERTS_SMP)
    if (t <= (Uint) ERTS_SHORT_TIME_T_MAX)
	erts_sys_schedule_interrupt_timed(1, (erts_short_time_t) t);
#endif
}

void
erts_cancel_timer(ErlTimer* p)
{
    erts_smp_mtx_lock(&tiw_lock);
    if (!p->active) { /* allow repeated cancel (drivers) */
	erts_smp_mtx_unlock(&tiw_lock);
	return;
    }

    /* is it the 'min' timer, remove min */
    if (p == tiw_min_ptr) {
	tiw_min_ptr = NULL;
	tiw_min     = 0;
    }

    remove_timer(p);
    p->slot = p->count = 0;

    if (p->cancel != NULL) {
	erts_smp_mtx_unlock(&tiw_lock);
	(*p->cancel)(p->arg);
	return;
    }
    erts_smp_mtx_unlock(&tiw_lock);
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
    Uint left;
    erts_short_time_t dt;

    erts_smp_mtx_lock(&tiw_lock);

    if (!p->active) {
	erts_smp_mtx_unlock(&tiw_lock);
	return 0;
    }

    if (p->slot < tiw_pos)
	left = (p->count + 1) * TIW_SIZE + p->slot - tiw_pos;
    else
	left = p->count * TIW_SIZE + p->slot - tiw_pos;
    dt = do_time_read();
    if (left < dt)
	left = 0;
    else
	left -= dt;

    erts_smp_mtx_unlock(&tiw_lock);

    return (Uint) left * TIW_ITIME;
}

#ifdef DEBUG
void erts_p_slpq(void)
{
    int i;
    ErlTimer* p;
  
    erts_smp_mtx_lock(&tiw_lock);

    /* print the whole wheel, starting at the current position */
    erts_printf("\ntiw_pos = %d tiw_nto %d\n", tiw_pos, tiw_nto);
    i = tiw_pos;
    if (tiw[i] != NULL) {
	erts_printf("%d:\n", i);
	for(p = tiw[i]; p != NULL; p = p->next) {
	    erts_printf(" (count %d, slot %d)\n",
			p->count, p->slot);
	}
    }
    for(i = (i+1)%TIW_SIZE; i != tiw_pos; i = (i+1)%TIW_SIZE) {
	if (tiw[i] != NULL) {
	    erts_printf("%d:\n", i);
	    for(p = tiw[i]; p != NULL; p = p->next) {
		erts_printf(" (count %d, slot %d)\n",
			    p->count, p->slot);
	    }
	}
    }

    erts_smp_mtx_unlock(&tiw_lock);
}
#endif /* DEBUG */
