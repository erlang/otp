/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
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


#if defined(ERTS_TIMER_THREAD) || 1
/* I don't yet know why, but using a mutex instead of a spinlock
   or spin-based rwlock avoids excessive delays at startup. */
static erts_smp_rwmtx_t tiw_lock;
#define tiw_read_lock()		erts_smp_rwmtx_rlock(&tiw_lock)
#define tiw_read_unlock()	erts_smp_rwmtx_runlock(&tiw_lock)
#define tiw_write_lock()	erts_smp_rwmtx_rwlock(&tiw_lock)
#define tiw_write_unlock()	erts_smp_rwmtx_rwunlock(&tiw_lock)
#define tiw_init_lock()		erts_smp_rwmtx_init(&tiw_lock, "timer_wheel")
#else
static erts_smp_rwlock_t tiw_lock;
#define tiw_read_lock()		erts_smp_read_lock(&tiw_lock)
#define tiw_read_unlock()	erts_smp_read_unlock(&tiw_lock)
#define tiw_write_lock()	erts_smp_write_lock(&tiw_lock)
#define tiw_write_unlock()	erts_smp_write_unlock(&tiw_lock)
#define tiw_init_lock()		erts_smp_rwlock_init(&tiw_lock, "timer_wheel")
#endif

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

/* END tiw_lock protected variables */

/* Actual interval time chosen by sys_init_time() */
static int itime; /* Constant after init */

#if defined(ERTS_TIMER_THREAD)
static SysTimeval time_start;	/* start of current time interval */
static long ticks_end;		/* time_start+ticks_end == time_wakeup */
static long ticks_latest;	/* delta from time_start at latest time update*/

static ERTS_INLINE long time_gettimeofday(SysTimeval *now)
{
    long elapsed;

    erts_get_timeval(now);
    now->tv_usec = 1000 * (now->tv_usec / 1000); /* ms resolution */
    elapsed = (1000 * (now->tv_sec - time_start.tv_sec) +
	       (now->tv_usec - time_start.tv_usec) / 1000);
    // elapsed /= CLOCK_RESOLUTION;
    return elapsed;
}

static long do_time_update(void)
{
    SysTimeval now;
    long elapsed;

    elapsed = time_gettimeofday(&now);
    ticks_latest = elapsed;
    return elapsed;
}

static ERTS_INLINE long do_time_read(void)
{
    return ticks_latest;
}

static long do_time_reset(void)
{
    SysTimeval now;
    long elapsed;

    elapsed = time_gettimeofday(&now);
    time_start = now;
    ticks_end = LONG_MAX;
    ticks_latest = 0;
    return elapsed;
}

static ERTS_INLINE void do_time_init(void)
{
    (void)do_time_reset();
}

#else
erts_smp_atomic_t do_time;	/* set at clock interrupt */
static ERTS_INLINE long do_time_read(void) { return erts_smp_atomic_read(&do_time); }
static ERTS_INLINE long do_time_update(void) { return do_time_read(); }
static ERTS_INLINE void do_time_init(void) { erts_smp_atomic_init(&do_time, 0L); }
#endif

/* get the time (in units of itime) to the next timeout,
   or -1 if there are no timeouts                     */

static int next_time_internal(void) /* PRE: tiw_lock taken by caller */
{
    int i, tm, nto;
    unsigned int min;
    ErlTimer* p;
    long dt;
  
    if (tiw_nto == 0)
	return -1;	/* no timeouts in wheel */
  
    /* start going through wheel to find next timeout */
    tm = nto = 0;
    min = (unsigned int) -1;	/* max unsigned int */
    i = tiw_pos;
    do {
	p = tiw[i];
	while (p != NULL) {
	    nto++;
	    if (p->count == 0) {
		/* found next timeout */
		dt = do_time_read();
		return ((tm >= dt) ? (tm - dt) : 0);
	    } else {
		/* keep shortest time in 'min' */
		if (tm + p->count*TIW_SIZE < min)
		    min = tm + p->count*TIW_SIZE;
	    }
	    p = p->next;
	}
	/* when we have found all timeouts the shortest time will be in min */
	if (nto == tiw_nto) break;
	tm++;
	i = (i + 1) % TIW_SIZE;
    } while (i != tiw_pos);
    dt = do_time_read();
    return ((min >= dt) ? (min - dt) : 0);
}

#if !defined(ERTS_TIMER_THREAD)
/* Private export to erl_time_sup.c */
int next_time(void)
{
    int ret;

    tiw_write_lock();
    (void)do_time_update();
    ret = next_time_internal();
    tiw_write_unlock();
    return ret;
}
#endif

static ERTS_INLINE void bump_timer_internal(long dt) /* PRE: tiw_lock is write-locked */
{
    Uint keep_pos;
    Uint count;
    ErlTimer *p, **prev, *timeout_head, **timeout_tail;
    Uint dtime = (unsigned long)dt;  

    /* no need to bump the position if there aren't any timeouts */
    if (tiw_nto == 0) {
	tiw_write_unlock();
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
	    if (p->count < count) {     /* we have a timeout */
		*prev = p->next;	/* Remove from list */
		tiw_nto--;
		p->next = NULL;
		p->active = 0;		/* Make sure cancel callback
					   isn't called */
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
    
    tiw_write_unlock();
    
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
	p->slot = 0;
	(*p->timeout)(p->arg);
    }
}

#if defined(ERTS_TIMER_THREAD)
static void timer_thread_bump_timer(void)
{
    tiw_write_lock();
    bump_timer_internal(do_time_reset());
}
#else
void bump_timer(long dt) /* dt is value from do_time */
{
    tiw_write_lock();
    bump_timer_internal(dt);
}
#endif

Uint
erts_timer_wheel_memory_size(void)
{
    return (Uint) TIW_SIZE * sizeof(ErlTimer*);
}

#if defined(ERTS_TIMER_THREAD)
static struct erts_iwait *timer_thread_iwait;

static int timer_thread_setup_delay(SysTimeval *rem_time)
{
    long elapsed;
    int ticks;

    tiw_write_lock();
    elapsed = do_time_update();
    ticks = next_time_internal();
    if (ticks == -1)	/* timer queue empty */
	ticks = 100*1000*1000;
    if (elapsed > ticks)
	elapsed = ticks;
    ticks -= elapsed;
    //ticks *= CLOCK_RESOLUTION;
    rem_time->tv_sec = ticks / 1000;
    rem_time->tv_usec = 1000 * (ticks % 1000);
    ticks_end = ticks;
    tiw_write_unlock();
    return ticks;
}

static void *timer_thread_start(void *ignore)
{
    SysTimeval delay;

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("timer");
#endif
    erts_register_blockable_thread();

    for(;;) {
	if (timer_thread_setup_delay(&delay)) {
	    erts_smp_activity_begin(ERTS_ACTIVITY_WAIT, NULL, NULL, NULL);
	    ASSERT_NO_LOCKED_LOCKS;
	    erts_iwait_wait(timer_thread_iwait, &delay);
	    ASSERT_NO_LOCKED_LOCKS;
	    erts_smp_activity_end(ERTS_ACTIVITY_WAIT, NULL, NULL, NULL);
	}
	else
	    erts_smp_chk_system_block(NULL, NULL, NULL);
	timer_thread_bump_timer();
	ASSERT_NO_LOCKED_LOCKS;
    }
    /*NOTREACHED*/
    return NULL;
}

static ERTS_INLINE void timer_thread_post_insert(Uint ticks)
{
    if ((Sint)ticks < ticks_end)
	erts_iwait_interrupt(timer_thread_iwait);
}

static void timer_thread_init(void)
{
    erts_thr_opts_t opts = ERTS_THR_OPTS_DEFAULT_INITER;
    erts_tid_t tid;

    opts->detached = 1;

    timer_thread_iwait = erts_iwait_init();
    erts_thr_create(&tid, timer_thread_start, NULL, &opts);
}

#else
static ERTS_INLINE void timer_thread_post_insert(Uint ticks) { }
static ERTS_INLINE void timer_thread_init(void) { }
#endif

/* this routine links the time cells into a free list at the start
   and sets the time queue as empty */
void
init_time(void)
{
    int i;

    /* system dependent init; must be done before do_time_init()
       if timer thread is enabled */
    itime = erts_init_time_sup();

    tiw_init_lock();

    tiw = (ErlTimer**) erts_alloc(ERTS_ALC_T_TIMER_WHEEL,
				  TIW_SIZE * sizeof(ErlTimer*));
    for(i = 0; i < TIW_SIZE; i++)
	tiw[i] = NULL;
    do_time_init();
    tiw_pos = tiw_nto = 0;

    timer_thread_init();
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
    ticks = (t + itime - 1) / itime;

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
    tiw[tm] = p;
    tiw_nto++;

    timer_thread_post_insert(ticks);
}

void
erl_set_timer(ErlTimer* p, ErlTimeoutProc timeout, ErlCancelProc cancel,
	      void* arg, Uint t)
{
    erts_deliver_time();
    tiw_write_lock();
    if (p->active) { /* XXX assert ? */
	tiw_write_unlock();
	return;
    }
    p->timeout = timeout;
    p->cancel = cancel;
    p->arg = arg;
    p->active = 1;
    insert_timer(p, t);
    tiw_write_unlock();
#if defined(ERTS_SMP) && !defined(ERTS_TIMER_THREAD)
    if (t <= (Uint) LONG_MAX)
	erts_sys_schedule_interrupt_timed(1, (long) t);
#endif
}

void
erl_cancel_timer(ErlTimer* p)
{
    ErlTimer *tp;
    ErlTimer **prev;

    tiw_write_lock();
    if (!p->active) { /* allow repeated cancel (drivers) */
	tiw_write_unlock();
	return;
    }
    /* find p in linked list at slot p->slot and remove it */
    prev = &tiw[p->slot];
    while ((tp = *prev) != NULL) {
	if (tp == p) {
	    *prev = p->next;	/* Remove from list */
	    tiw_nto--;
	    p->next = NULL;
	    p->slot = p->count = 0;
	    p->active = 0;
	    if (p->cancel != NULL) {
		tiw_write_unlock();
		(*p->cancel)(p->arg);
	    } else {
		tiw_write_unlock();
	    }
	    return;
	} else {
	    prev = &tp->next;
	}
    }
    tiw_write_unlock();
}

/*
  Returns the amount of time left in ms until the timer 'p' is triggered.
  0 is returned if 'p' isn't active.
  0 is returned also if the timer is overdue (i.e., would have triggered
  immediately if it hadn't been cancelled).
*/
Uint
time_left(ErlTimer *p)
{
    Uint left;
    long dt;

    tiw_read_lock();

    if (!p->active) {
	tiw_read_unlock();
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

    tiw_read_unlock();

    return left * itime;
}

#ifdef DEBUG

void p_slpq()
{
    int i;
    ErlTimer* p;
  
    tiw_read_lock();

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

    tiw_read_unlock();
}

#endif /* DEBUG */
