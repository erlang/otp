/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define WANT_NONBLOCKING

#include "sys.h"
#include "erl_alloc.h"
#include "erl_poll.h"
#include "erl_time.h"
#include "erl_msacc.h"

/*
 * Some debug macros 
 */

/*#define HARDDEBUG */
#ifdef HARDDEBUG
#ifdef HARDTRACE
#define HARDTRACEF(X) my_debug_printf##X
#else
#define HARDTRACEF(X)
#endif

#define HARDDEBUGF(X) my_debug_printf##X
static void my_debug_printf(char *fmt, ...)
{
    char buffer[1024];
    va_list args;

    va_start(args, fmt);
    erts_vsnprintf(buffer,1024,fmt,args);
    va_end(args);
    erts_fprintf(stderr,"%s\r\n",buffer);
}
#else
#define HARDTRACEF(X)
#define HARDDEBUGF(X)
#endif

#ifdef DEBUG
#define NoMansLandFill 0xFD	/* fill no-man's land with this */
#define DeadLandFill   0xDD	/* fill free objects with this */
#define CleanLandFill  0xCD	/* fill new objects with this */

static void consistency_check(struct _Waiter* w);
static void* debug_alloc(ErtsAlcType_t, Uint);
static void* debug_realloc(ErtsAlcType_t, void *, Uint, Uint);

#  define SEL_ALLOC	debug_alloc
#  define SEL_REALLOC	debug_realloc
#  define SEL_FREE	erts_free

static void *debug_alloc(ErtsAlcType_t type, Uint size)
{
    void* p = erts_alloc(type, size);
    memset(p, CleanLandFill, size);
    return p;
}

static void *debug_realloc(ErtsAlcType_t type, void *ptr, Uint prev_size,
			   Uint size)
{
    void *p;
    size_t fill_size;
    void *fill_ptr;

    if (prev_size > size) {
	size_t fill_size = (size_t) (prev_size - size);
	void *fill_ptr = (void *) (((char *) ptr) + size);
	memset(fill_ptr, NoMansLandFill, fill_size);
    }

    p = erts_realloc(type, ptr, size);

    if (size > prev_size) {
	size_t fill_size = (size_t) (size - prev_size);
	void *fill_ptr = (void *) (((char *) p) + prev_size);
	memset(fill_ptr, CleanLandFill, fill_size);
    }

    return p;
}
#else
#  define SEL_ALLOC	erts_alloc
#  define SEL_REALLOC	realloc_wrap
#  define SEL_FREE	erts_free

static ERTS_INLINE void *
realloc_wrap(ErtsAlcType_t t, void *p, Uint ps, Uint s)
{
    return erts_realloc(t, p, s);
}
#endif


#ifdef HARD_POLL_DEBUG
#define OP_SELECT 1
#define OP_DESELECT 2
#define OP_FIRED 3
#define OP_READ_BEGIN 4
#define OP_READ_DONE 5
#define OP_WRITE_BEGIN 6
#define OP_WRITE_DONE 7
#define OP_REPORTED 8
#define OP_DIED 9
#define OP_ASYNC_INIT 10
#define OP_ASYNC_IMMED 11
#define OP_FD_MOVED 12

static struct {
    int op;
    ErtsSysFdType active;
    int xdata;
} debug_save_ops[1024];

static int num_debug_save_ops = 0;

static ErtsSysFdType active_debug_fd;
static int active_debug_fd_set = 0;

static erts_mtx_t save_ops_mtx;

static void poll_debug_init(void)
{
    erts_mtx_init(&save_ops_mtx, "save_ops_lock");
}

void poll_debug_set_active_fd(ErtsSysFdType fd)
{
    erts_mtx_lock(&save_ops_mtx);
    active_debug_fd_set = 1;
    active_debug_fd = fd;
    erts_mtx_unlock(&save_ops_mtx);
}

static void do_save_op(ErtsSysFdType fd, int op, int xdata)
{
    erts_mtx_lock(&save_ops_mtx);
    if (fd == active_debug_fd && num_debug_save_ops < 1024) {
	int x = num_debug_save_ops++;
	debug_save_ops[x].op = op;
	debug_save_ops[x].active = fd;
	debug_save_ops[x].xdata = xdata;
    }
    erts_mtx_unlock(&save_ops_mtx);
}

void poll_debug_moved(ErtsSysFdType fd, int s1, int s2)
{
    do_save_op(fd,OP_FD_MOVED,s1 | (s2 << 16));
}

void poll_debug_select(ErtsSysFdType fd, int mode)
{
    do_save_op(fd,OP_SELECT,mode);
}

void poll_debug_deselect(ErtsSysFdType fd)
{
    do_save_op(fd,OP_DESELECT,0);
}

void poll_debug_fired(ErtsSysFdType fd)
{
    do_save_op(fd,OP_FIRED,0);
}

void poll_debug_read_begin(ErtsSysFdType fd)
{
    do_save_op(fd,OP_READ_BEGIN,0);
}

void poll_debug_read_done(ErtsSysFdType fd, int bytes)
{
    do_save_op(fd,OP_READ_DONE,bytes);
}

void poll_debug_async_initialized(ErtsSysFdType fd)
{
    do_save_op(fd,OP_ASYNC_INIT,0);
}

void poll_debug_async_immediate(ErtsSysFdType fd, int bytes)
{
    do_save_op(fd,OP_ASYNC_IMMED,bytes);
}

void poll_debug_write_begin(ErtsSysFdType fd)
{
    do_save_op(fd,OP_WRITE_BEGIN,0);
}

void poll_debug_write_done(ErtsSysFdType fd, int bytes)
{
    do_save_op(fd,OP_WRITE_DONE,bytes);
}

void poll_debug_reported(ErtsSysFdType fd, int mode)
{
    do_save_op(fd,OP_REPORTED,mode);
}

void poll_debug_died(ErtsSysFdType fd)
{
    do_save_op(fd,OP_DIED,0);
}

#endif /* DEBUG */

/*
 * End of debug macros
 */



/*
 * Handles that we poll, but that are actually signalled from outside 
 * this module
 */

extern HANDLE erts_service_event;
extern HANDLE erts_sys_break_event;


/*
 * The structure we hold for each event (i.e. fd)
 */
typedef struct _EventData {
    HANDLE event;		/* For convenience. */
    ErtsPollEvents mode;	/* The current select mode. */
    struct _EventData *next;	/* Next in free or delete lists. */
} EventData;

/*
 * The structure to represent a waiter thread
 */
typedef struct _Waiter {
    HANDLE events[MAXIMUM_WAIT_OBJECTS];     /* The events. */
    EventData* evdata[MAXIMUM_WAIT_OBJECTS]; /* Pointers to associated data. */
    int active_events;		/* Number of events to wait for */
    int total_events;		/* Total number of events in the arrays. */
    int highwater;              /* Events processed up to here */
    EventData evdata_heap[MAXIMUM_WAIT_OBJECTS]; /* Pre-allocated EventDatas */
    EventData* first_free_evdata; /* Index of first free EventData object. */
    HANDLE go_ahead;		/* The waiter may continue. (Auto-reset) */
    void *xdata;                /* used when thread parameter */
    erts_tid_t this;            /* Thread "handle" of this waiter */
    erts_mtx_t mtx;             /* Mutex for updating/reading pollset, but the
				   currently used set require thread stopping 
				   to be updated */
} Waiter;

/*
 * The structure for a pollset. There can currently be only one...
 */
struct ErtsPollSet_ {
    Waiter** waiter;
    int allocated_waiters;  /* Size ow waiter array */ 
    int num_waiters;	    /* Number of waiter threads. */
    int restore_events;        /* Tells us to restore waiters events 
				  next time around */
    HANDLE event_io_ready;     /* To be used when waiting for io */
    /* These are used to wait for workers to enter standby */
    volatile int standby_wait_counter; /* Number of threads to wait for */
    CRITICAL_SECTION standby_crit;     /* CS to guard the counter */
    HANDLE standby_wait_event;         /* Event signalled when counte == 0 */
    erts_atomic32_t wakeup_state;
#ifdef ERTS_SMP
    erts_smp_mtx_t mtx;
#endif
    erts_atomic64_t timeout_time;
};

#ifdef ERTS_SMP

#define ERTS_POLLSET_LOCK(PS) \
  erts_smp_mtx_lock(&(PS)->mtx)
#define ERTS_POLLSET_UNLOCK(PS) \
  erts_smp_mtx_unlock(&(PS)->mtx)

#else

#define ERTS_POLLSET_LOCK(PS)
#define ERTS_POLLSET_UNLOCK(PS)

#endif

/*
 * Communication with sys_interrupt
 */

#ifdef ERTS_SMP
extern erts_smp_atomic32_t erts_break_requested;
#define ERTS_SET_BREAK_REQUESTED \
  erts_smp_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 1)
#define ERTS_UNSET_BREAK_REQUESTED \
  erts_smp_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 0)
#else
extern volatile int erts_break_requested;
#define ERTS_SET_BREAK_REQUESTED (erts_break_requested = 1)
#define ERTS_UNSET_BREAK_REQUESTED (erts_break_requested = 0)
#endif

static erts_mtx_t break_waiter_lock;
static HANDLE break_happened_event;
static erts_atomic32_t break_waiter_state;
#define BREAK_WAITER_GOT_BREAK 1
#define BREAK_WAITER_GOT_HALT 2


/* 
 * Forward declarations
 */

static void *threaded_waiter(void *param);
static void *break_waiter(void *param);

/*
 * Sychronization macros and functions
 */
#define START_WAITER(PS, w) \
    SetEvent((w)->go_ahead)

#define STOP_WAITER(PS,w) \
do { \
    setup_standby_wait((PS),1); \
    SetEvent((w)->events[0]); \
    wait_standby(PS); \
} while(0)

#define START_WAITERS(PS) \
do { \
    int i; \
    for (i = 0; i < (PS)->num_waiters; i++) { \
	SetEvent((PS)->waiter[i]->go_ahead); \
    } \
 } while(0)

#define STOP_WAITERS(PS) \
do { \
    int i; \
    setup_standby_wait((PS),(PS)->num_waiters); \
    for (i = 0; i < (PS)->num_waiters; i++) { \
	SetEvent((PS)->waiter[i]->events[0]); \
    } \
    wait_standby(PS); \
 } while(0)

static ERTS_INLINE void
init_timeout_time(ErtsPollSet ps)
{
    erts_atomic64_init_nob(&ps->timeout_time,
			   (erts_aint64_t) ERTS_MONOTONIC_TIME_MAX);
}

static ERTS_INLINE void
set_timeout_time(ErtsPollSet ps, ErtsMonotonicTime time)
{
    erts_atomic64_set_relb(&ps->timeout_time,
			   (erts_aint64_t) time);
}

static ERTS_INLINE ErtsMonotonicTime
get_timeout_time(ErtsPollSet ps)
{
    return (ErtsMonotonicTime) erts_atomic64_read_acqb(&ps->timeout_time);
}

#define ERTS_POLL_NOT_WOKEN		((erts_aint32_t) 0)
#define ERTS_POLL_WOKEN_IO_READY	((erts_aint32_t) 1)
#define ERTS_POLL_WOKEN_INTR		((erts_aint32_t) 2)
#define ERTS_POLL_WOKEN_TIMEDOUT	((erts_aint32_t) 3)

static ERTS_INLINE int
is_io_ready(ErtsPollSet ps)
{
    return erts_atomic32_read_nob(&ps->wakeup_state) == ERTS_POLL_WOKEN_IO_READY;
}

static ERTS_INLINE void
woke_up(ErtsPollSet ps)
{
    if (erts_atomic32_read_nob(&ps->wakeup_state) == ERTS_POLL_NOT_WOKEN)
	erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
				  ERTS_POLL_WOKEN_TIMEDOUT,
				  ERTS_POLL_NOT_WOKEN);
#ifdef DEBUG
    {
	erts_aint32_t wakeup_state = erts_atomic32_read_nob(&ps->wakeup_state);
	switch (wakeup_state) {
	case ERTS_POLL_WOKEN_IO_READY:
	case ERTS_POLL_WOKEN_INTR:
	case ERTS_POLL_WOKEN_TIMEDOUT:
	    break;
	default:
	    ASSERT(0);
	    break;
	}
    }
#endif
}

static ERTS_INLINE int
wakeup_cause(ErtsPollSet ps)
{
    int res;
    erts_aint32_t wakeup_state = erts_atomic32_read_acqb(&ps->wakeup_state);
    switch (wakeup_state) {
    case ERTS_POLL_WOKEN_IO_READY:
	res = 0;
	break;
    case ERTS_POLL_WOKEN_INTR:
	res = EINTR;
	break;
    case ERTS_POLL_WOKEN_TIMEDOUT:
	res = ETIMEDOUT;
	break;
    default:
	res = 0;
	erts_exit(ERTS_ABORT_EXIT,
		 "%s:%d: Internal error: Invalid wakeup_state=%d\n",
		 __FILE__, __LINE__, (int) wakeup_state);
    }
    return res;
}

static ERTS_INLINE DWORD
poll_wait_timeout(ErtsPollSet ps, ErtsMonotonicTime timeout_time)
{
    ErtsMonotonicTime current_time, diff_time, timeout;

    if (timeout_time == ERTS_POLL_NO_TIMEOUT) {
    no_timeout:
	set_timeout_time(ps, ERTS_MONOTONIC_TIME_MIN);
	woke_up(ps);
	return (DWORD) 0;
    }

    current_time = erts_get_monotonic_time(NULL);
    diff_time = timeout_time - current_time;
    if (diff_time <= 0)
	goto no_timeout;

    /* Round up to nearest milli second */
    timeout = (ERTS_MONOTONIC_TO_MSEC(diff_time - 1) + 1);
    if (timeout > INT_MAX)
	timeout = INT_MAX; /* Also prevents DWORD overflow */

    set_timeout_time(ps, current_time + ERTS_MSEC_TO_MONOTONIC(timeout));

    ResetEvent(ps->event_io_ready);
    /*
     * Since we don't know the internals of ResetEvent() we issue
     * a memory barrier as a safety precaution ensuring that
     * the load of wakeup_state wont be reordered with stores made
     * by ResetEvent().
     */
    ERTS_THR_MEMORY_BARRIER;
    if (erts_atomic32_read_nob(&ps->wakeup_state) != ERTS_POLL_NOT_WOKEN)
	return (DWORD) 0;

    return (DWORD) timeout;
}

static ERTS_INLINE void
wake_poller(ErtsPollSet ps, int io_ready)
{
    erts_aint32_t wakeup_state;
    if (io_ready) {
        wakeup_state = erts_atomic32_xchg_relb(&ps->wakeup_state,
                                               ERTS_POLL_WOKEN_IO_READY);
    }
    else {
	ERTS_THR_MEMORY_BARRIER;
	wakeup_state = erts_atomic32_read_nob(&ps->wakeup_state);
	while (wakeup_state != ERTS_POLL_WOKEN_IO_READY
	       && wakeup_state != ERTS_POLL_WOKEN_INTR) {
	    erts_aint32_t act = erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
							  ERTS_POLL_WOKEN_INTR,
							  wakeup_state);
	    if (act == wakeup_state) {
		wakeup_state = act;
		break;
	    }
	    wakeup_state = act;
	}
    }
    if (wakeup_state == ERTS_POLL_NOT_WOKEN) {
	/*
	 * Since we don't know the internals of SetEvent() we issue
	 * a memory barrier as a safety precaution ensuring that
	 * the store we just made to wakeup_state wont be reordered
	 * with loads in SetEvent().
	 */
	ERTS_THR_MEMORY_BARRIER;
	SetEvent(ps->event_io_ready);
    }
}

static ERTS_INLINE void
reset_io_ready(ErtsPollSet ps)
{
    erts_atomic32_set_nob(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN);
}

static ERTS_INLINE void
restore_io_ready(ErtsPollSet ps)
{
    erts_atomic32_set_nob(&ps->wakeup_state, ERTS_POLL_WOKEN_IO_READY);
}

/*
 * notify_io_ready() is used by threads waiting for events, when
 * notifying a poller thread about I/O ready.
 */
static ERTS_INLINE void
notify_io_ready(ErtsPollSet ps)
{
    wake_poller(ps, 1);
}

static ERTS_INLINE void
reset_interrupt(ErtsPollSet ps)
{
    /* We need to keep io-ready if set */
    erts_aint32_t wakeup_state = erts_atomic32_read_nob(&ps->wakeup_state);
    while (wakeup_state != ERTS_POLL_WOKEN_IO_READY
	   && wakeup_state != ERTS_POLL_NOT_WOKEN) {
	erts_aint32_t act = erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
						      ERTS_POLL_NOT_WOKEN,
						      wakeup_state);
	if (wakeup_state == act)
	    break;
	wakeup_state = act;
    }
    ERTS_THR_MEMORY_BARRIER;
}

static ERTS_INLINE void
set_interrupt(ErtsPollSet ps)
{
    wake_poller(ps, 0);
}

static void setup_standby_wait(ErtsPollSet ps, int num_threads)
{
    EnterCriticalSection(&(ps->standby_crit));
    ps->standby_wait_counter = num_threads;
    ResetEvent(ps->standby_wait_event);
    LeaveCriticalSection(&(ps->standby_crit));
}

static void signal_standby(ErtsPollSet ps) 
{
    EnterCriticalSection(&(ps->standby_crit));
    --(ps->standby_wait_counter);
    if (ps->standby_wait_counter < 0) {
	LeaveCriticalSection(&(ps->standby_crit));
	erts_exit(ERTS_ERROR_EXIT,"Standby signalled by more threads than expected");
    }
    if (!(ps->standby_wait_counter)) {
	SetEvent(ps->standby_wait_event);
    }
    LeaveCriticalSection(&(ps->standby_crit));
}

static void wait_standby(ErtsPollSet ps)
{
    WaitForSingleObject(ps->standby_wait_event,INFINITE);
}

static void remove_event_from_set(Waiter *w, int j)
{
    w->evdata[j]->event = INVALID_HANDLE_VALUE;
    w->evdata[j]->mode = 0;
    w->evdata[j]->next = w->first_free_evdata;
    w->first_free_evdata = w->evdata[j];
    
    /*
     * If the event is active, we will overwrite it
     * with the last active event and make the hole
     * the first non-active event.
     */
    
    if (j < w->active_events) {
	w->active_events--;
	w->highwater--;
	w->total_events--;
	w->events[j] = w->events[w->active_events];
	w->evdata[j] = w->evdata[w->active_events];
	w->events[w->active_events] = w->events[w->highwater];
	w->evdata[w->active_events] = w->evdata[w->highwater];
	w->events[w->highwater] =  w->events[w->total_events];
	w->evdata[w->highwater] =  w->evdata[w->total_events];
    } else if (j < w->highwater) {
	w->highwater--;
	w->total_events--;
	w->events[j] = w->events[w->highwater];
	w->evdata[j] = w->evdata[w->highwater];
	w->events[w->highwater] =  w->events[w->total_events];
	w->evdata[w->highwater] =  w->evdata[w->total_events];
    } else {
	w->total_events--;
	w->events[j] = w->events[w->total_events];
	w->evdata[j] = w->evdata[w->total_events];
    }
    
#ifdef DEBUG
    w->events[w->total_events] = (HANDLE) CleanLandFill;
    w->evdata[w->total_events] = (EventData *) CleanLandFill;
    consistency_check(w);
#endif
}    

/* 
 * Thread handling
 */

#ifdef DEBUG
static void consistency_check(Waiter* w)
{
    int i;
    
    ASSERT(w->active_events <= w->total_events);
    ASSERT(w->evdata[0] == NULL);

    for (i = 1; i < w->total_events; i++) {
	ASSERT(w->events[i] == w->evdata[i]->event);
	ASSERT(w->evdata[i]->mode != 0);
    }
}

#endif

static void new_waiter(ErtsPollSet ps)
{
    register Waiter* w;
    DWORD tid;			/* Id for thread. */
    erts_tid_t thread;
    int i;
    int tres;

    if (ps->num_waiters == ps->allocated_waiters) {
	Uint old_size = sizeof(Waiter *)*ps->allocated_waiters;
	ps->allocated_waiters += 64;
	ps->waiter = SEL_REALLOC(ERTS_ALC_T_WAITER_OBJ,
				 (void *) ps->waiter,
				 old_size,
				 sizeof(Waiter *) * (ps->allocated_waiters));
    }
	
    w = (Waiter *) SEL_ALLOC(ERTS_ALC_T_WAITER_OBJ, sizeof(Waiter));
    ps->waiter[ps->num_waiters] = w;

    w->events[0] = CreateAutoEvent(FALSE);
    w->evdata[0] = NULL;	/* Should never be used. */
    w->active_events = 1;
    w->highwater = 1;
    w->total_events = 1;
    erts_mtx_init(&w->mtx, "pollwaiter");


    /*
     * Form the free list of EventData objects.
     */

    w->evdata_heap[0].next = 0;	/* Last in free list. */
    for (i = 1; i < MAXIMUM_WAIT_OBJECTS; i++) {
	w->evdata_heap[i].next = w->evdata_heap+i-1;
    }
    w->first_free_evdata = w->evdata_heap+MAXIMUM_WAIT_OBJECTS-1;

    /*
     * Create the other events.
     */

    w->go_ahead = CreateAutoEvent(FALSE);

    /*
     * Create the thread.
     */
    w->xdata = ps;
    erts_thr_create(&thread, &threaded_waiter, w, NULL);
    w->this = thread;

    /*
     * Finally, done.
     */

    (ps->num_waiters)++;
}

static void *break_waiter(void *param)
{
    HANDLE harr[2];
    int i = 0;
    harr[i++] = erts_sys_break_event;
    if (erts_service_event != NULL) {
	harr[i++] = erts_service_event;
    }

    for(;;) {
	switch (WaitForMultipleObjects(i,harr,FALSE,INFINITE)) {
	case WAIT_OBJECT_0:
	    ResetEvent(harr[0]);
	    erts_mtx_lock(&break_waiter_lock);
	    erts_atomic32_set_nob(&break_waiter_state,BREAK_WAITER_GOT_BREAK);
	    ERTS_THR_MEMORY_BARRIER;
	    SetEvent(break_happened_event);
	    erts_mtx_unlock(&break_waiter_lock);
	    break;
	case (WAIT_OBJECT_0+1):
	    ResetEvent(harr[1]);
	    erts_mtx_lock(&break_waiter_lock);
	    erts_atomic32_set_nob(&break_waiter_state,BREAK_WAITER_GOT_HALT);
	    ERTS_THR_MEMORY_BARRIER;
	    SetEvent(break_happened_event);
	    erts_mtx_unlock(&break_waiter_lock);
	    break;
	default:
	    erts_exit(ERTS_ERROR_EXIT,"Unexpected event in break_waiter");
	}
    }
}
		    
static void *threaded_waiter(void *param)
{
    register Waiter* w = (Waiter *) param;
    ErtsPollSet ps = (ErtsPollSet) w->xdata;
#ifdef HARD_POLL_DEBUG2
    HANDLE oold_fired[64];
    int num_oold_fired;
    HANDLE old_fired[64];
    int num_old_fired = 0;
    HANDLE fired[64];
    int num_fired = 0;
    HANDLE errors[1024];
    int num_errors = 0;
    HANDLE save_events[64];
    int save_active_events;
    int save_total_events;
    int save_highwater;
#endif

 again:
    WaitForSingleObject(w->go_ahead, INFINITE);
    /* Atomic enough when just checking, skip lock */
    if (w->total_events == 0) {
	return NULL;
    }
    if (w->active_events == 0) {
	goto again;
    }
    ASSERT(w->evdata[0] == NULL);
#ifdef HARD_POLL_DEBUG2
    num_oold_fired = num_old_fired;
    memcpy(oold_fired,old_fired,num_old_fired*sizeof(HANDLE));
    num_old_fired = num_fired;
    memcpy(old_fired,fired,num_fired*sizeof(HANDLE));
    num_fired = 0;
#endif
    for (;;) {
	int i;
	int j;
#ifdef HARD_POLL_DEBUG2
	erts_mtx_lock(&w->mtx);
	memcpy(save_events,w->events,w->active_events*sizeof(HANDLE));
	save_active_events = w->active_events;
	save_total_events = w->total_events;
	save_highwater = w->highwater;
	erts_mtx_unlock(&w->mtx);	
#endif
	i = WaitForMultipleObjects(w->active_events, w->events, FALSE, INFINITE);
	switch (i) {
	case WAIT_FAILED:
	    DEBUGF(("Wait failed: %s\n", last_error()));
	    erts_mtx_lock(&w->mtx);
	    /* Dont wait for our signal event */
	    for (j = 1; j < w->active_events; j++) {
		int tmp;
		if ((tmp = WaitForSingleObject(w->events[j], 0)) 
		    == WAIT_FAILED) {
		    DEBUGF(("Invalid handle: i = %d, handle = 0x%0x\n",
			    j, w->events[j]));
#ifdef HARD_POLL_DEBUG2
		    if (num_errors < 1024)
			errors[num_errors++] = w->events[j];
#endif
#ifdef HARD_POLL_DEBUG
		    poll_debug_died(w->events[j]);
#endif
		    remove_event_from_set(w,j);
#ifdef DEBUG
		    consistency_check(w);
#endif
		} else if (tmp == WAIT_OBJECT_0) {
		    i = WAIT_OBJECT_0 + j;
		    goto event_happened;
		}
	    }
	    erts_mtx_unlock(&w->mtx);
	    break;
	case WAIT_OBJECT_0:
	    signal_standby(ps);
	    goto again;
#ifdef DEBUG
	case WAIT_TIMEOUT:
	    ASSERT(0);
#endif
	default:
	    erts_mtx_lock(&w->mtx);
#ifdef HARD_POLL_DEBUG2
	    {
		int x = memcmp(save_events,w->events,w->active_events*sizeof(HANDLE));
		ASSERT(x == 0 && save_active_events == w->active_events);
	    }
#endif
event_happened:
#ifdef DEBUG
	    consistency_check(w);
#endif
	    ASSERT(WAIT_OBJECT_0 < i && i < WAIT_OBJECT_0+w->active_events);
	    notify_io_ready(ps);

	    /* 
	     * The main thread wont start working on our arrays untill we're
	     * stopped, so we can work in peace although the main thread runs 
	     */
	    ASSERT(i >= WAIT_OBJECT_0+1);
	    i -= WAIT_OBJECT_0;
	    ASSERT(i >= 1);
	    w->active_events--;
	    HARDDEBUGF(("i = %d, a,h,t = %d,%d,%d",i,
			w->active_events, w->highwater, w->total_events));
#ifdef HARD_POLL_DEBUG2
	    fired[num_fired++] = w->events[i];
#endif
#ifdef HARD_POLL_DEBUG
	    poll_debug_fired(w->events[i]);
#endif
	    if (i < w->active_events) {
		HANDLE te = w->events[i];
		EventData* tp = w->evdata[i];
		w->events[i] = w->events[w->active_events];
		w->evdata[i] = w->evdata[w->active_events];
		w->events[w->active_events] = te;
		w->evdata[w->active_events] = tp;
	    }
	    HARDDEBUGF(("i = %d, a,h,t = %d,%d,%d",i,
			w->active_events, w->highwater, w->total_events));
#ifdef DEBUG
	    consistency_check(w);
#endif
	    erts_mtx_unlock(&w->mtx);	    
	    break;
	}
    }
}

/*
 * The actual adding and removing from pollset utilities 
 */

static int set_driver_select(ErtsPollSet ps, HANDLE event, ErtsPollEvents mode)
{
    int i;
    int best_waiter = -1;	/* The waiter with lowest number of events. */
    int lowest = MAXIMUM_WAIT_OBJECTS; /* Lowest number of events
					* in any waiter.
					*/
    EventData* ev;
    Waiter* w;

    /*
     * Find the waiter which is least busy.
     */

#ifdef HARD_POLL_DEBUG
    poll_debug_select(event, mode);
#endif

    /* total_events can no longer be read without the lock, it's changed in the waiter */
    for (i = 0; i < ps->num_waiters; i++) {
	erts_mtx_lock(&(ps->waiter[i]->mtx));
	if (ps->waiter[i]->total_events < lowest) {
	    lowest = ps->waiter[i]->total_events;
	    best_waiter = i;
	}
	erts_mtx_unlock(&(ps->waiter[i]->mtx));
    }

    /*
     * Stop the selected waiter, or start a new waiter if all were busy.
     */

    if (best_waiter >= 0) {
	w = ps->waiter[best_waiter];
	STOP_WAITER(ps,w);
	erts_mtx_lock(&w->mtx);
    } else {
	new_waiter(ps);
	w = ps->waiter[(ps->num_waiters)-1];
	erts_mtx_lock(&w->mtx);
    }

#ifdef DEBUG
    consistency_check(w);
#endif

    /*
     * Allocate and initialize an EventData structure.
     */

    ev = w->first_free_evdata;
    w->first_free_evdata = ev->next;
    ev->event = event;
    ev->mode = mode;
    ev->next = NULL;

    /*
     * At this point, the selected waiter (newly-created or not) is
     * standing by.  Put the new event into the active part of the array.
     */

    if (w->active_events < w->total_events) {
	/*
	 * Move the first event beyond the active part of the array to
	 * the very end to make place for the new event.
	 */

#ifdef HARD_POLL_DEBUG
	poll_debug_moved(w->events[w->highwater],w->highwater,w->total_events);
#endif
	w->events[w->total_events] = w->events[w->highwater];
	w->evdata[w->total_events] = w->evdata[w->highwater];
#ifdef HARD_POLL_DEBUG
	poll_debug_moved(w->events[w->active_events],w->active_events,w->highwater);
#endif
	w->events[w->highwater] = w->events[w->active_events];
	w->evdata[w->highwater] = w->evdata[w->active_events];

    }
    w->events[w->active_events] = event;
    w->evdata[w->active_events] = ev;
    w->active_events++;
    w->highwater++;
    w->total_events++;

#ifdef DEBUG
    consistency_check(w);
#endif
    erts_mtx_unlock(&w->mtx);
    START_WAITER(ps,w);
    HARDDEBUGF(("add select %d %d %d %d",best_waiter,
		w->active_events,w->highwater,w->total_events));
    return mode;
}


static int cancel_driver_select(ErtsPollSet ps, HANDLE event)
{
    int i;

    ASSERT(event != INVALID_HANDLE_VALUE);
 restart:
    for (i = 0; i < ps->num_waiters; i++) {
	Waiter* w = ps->waiter[i];
	int j;

	erts_mtx_lock(&w->mtx);
#ifdef DEBUG
	consistency_check(w);
#endif
	for (j = 0; j < w->total_events; j++) {
	    if (w->events[j] == event) {
		int stopped = 0;
		/*
		 * Free the event's EventData structure.
		 */

		if (j < w->active_events) {
		    HARDDEBUGF(("Stopped in remove select"));
		    stopped = 1;
		    erts_mtx_unlock(&w->mtx);
		    STOP_WAITER(ps,w);
		    erts_mtx_lock(&w->mtx);
		    if ( j >= w->active_events || w->events[j] != event) {
			/* things happened while unlocked */
			START_WAITER(ps,w);
			erts_mtx_unlock(&w->mtx);
			goto restart;
		    }
		}
#ifdef HARD_POLL_DEBUG
		poll_debug_deselect(w->events[j]);
#endif
		remove_event_from_set(w, j);
		if (stopped) {
		    START_WAITER(ps,w);
		}
		HARDDEBUGF(("removed select %d,%d %d %d %d",i,j,
			    w->active_events,w->highwater,w->total_events));
		break;
	    }
	}
	erts_mtx_unlock(&w->mtx);
    }
    return 0;
}

/*
 * Interface functions
 */

void  erts_poll_interrupt(ErtsPollSet ps, int set /* bool */)
{
    HARDTRACEF(("In erts_poll_interrupt(%d)",set));
    if (!set)
	reset_interrupt(ps);
    else
	set_interrupt(ps);
    HARDTRACEF(("Out erts_poll_interrupt(%d)",set));
}

void erts_poll_interrupt_timed(ErtsPollSet ps,
			       int set /* bool */,
			       ErtsMonotonicTime timeout_time)
{
    HARDTRACEF(("In erts_poll_interrupt_timed(%d,%ld)",set,timeout_time));
    if (!set)
	reset_interrupt(ps);
    else if (get_timeout_time(ps) > timeout_time)
	set_interrupt(ps);
    HARDTRACEF(("Out erts_poll_interrupt_timed"));
}


/*
 * Windows is special, there is actually only one event type, and
 * the only difference between ERTS_POLL_EV_IN and ERTS_POLL_EV_OUT
 * is which driver callback will eventually be called.
 */
static ErtsPollEvents do_poll_control(ErtsPollSet ps,
				      ErtsSysFdType fd,
				      ErtsPollEvents pe,
				      int on /* bool */)
{
    HANDLE event = (HANDLE) fd;
    ErtsPollEvents mode;
    ErtsPollEvents result;
    ASSERT(event != INVALID_HANDLE_VALUE);

    if (on) {
	if (pe & ERTS_POLL_EV_IN || !(pe & ERTS_POLL_EV_OUT )) {
	    mode = ERTS_POLL_EV_IN;
	} else {
	    mode = ERTS_POLL_EV_OUT; /* ready output only in this case */
	}
	result = set_driver_select(ps, event, mode);
    } else {
	result = cancel_driver_select(ps, event);
    }
    return result;
}

ErtsPollEvents erts_poll_control(ErtsPollSet ps,
				 ErtsSysFdType fd,
				 ErtsPollEvents pe,
				 int on,
				 int* do_wake) /* In: Wake up polling thread */
				               /* Out: Poller is woken */
{
    ErtsPollEvents result;
    HARDTRACEF(("In erts_poll_control(0x%08X, %u, %d)",(unsigned long) fd, (unsigned) pe, on));
    ERTS_POLLSET_LOCK(ps);
    result=do_poll_control(ps,fd,pe,on);
    ERTS_POLLSET_UNLOCK(ps);
    *do_wake = 0; /* Never any need to wake polling threads on windows */
    HARDTRACEF(("Out erts_poll_control -> %u",(unsigned) result));
    return result;
}

void erts_poll_controlv(ErtsPollSet ps,
			ErtsPollControlEntry pcev[],
			int len)
{
    int i;
    int hshur = 0;
    int do_wake = 0;

    HARDTRACEF(("In erts_poll_controlv(%d)",len));
    ERTS_POLLSET_LOCK(ps);

    for (i = 0; i < len; i++) {
	pcev[i].events = do_poll_control(ps,
					 pcev[i].fd,
					 pcev[i].events,
					 pcev[i].on);
    }
    ERTS_POLLSET_UNLOCK(ps);
    HARDTRACEF(("Out erts_poll_controlv"));
}

int erts_poll_wait(ErtsPollSet ps,
		   ErtsPollResFd pr[],
		   int *len,
		   ErtsMonotonicTime timeout_time)
{
    int no_fds;
    DWORD timeout;
    EventData* ev;
    int res = 0;
    int num = 0;
    int n; 
    int i;
    int break_state;

    HARDTRACEF(("In erts_poll_wait"));
    ERTS_POLLSET_LOCK(ps);

    if (!is_io_ready(ps) && ps->restore_events) {
	HARDDEBUGF(("Restore events: %d",ps->num_waiters));
	ps->restore_events = 0;
	for (i = 0; i < ps->num_waiters; ++i) {
	   Waiter* w = ps->waiter[i];
	   erts_mtx_lock(&w->mtx);
	   HARDDEBUGF(("Maybe reset %d %d %d %d",i,
		       w->active_events,w->highwater,w->total_events));
	   if (w->active_events < w->total_events) {
	       erts_mtx_unlock(&w->mtx);
	       STOP_WAITER(ps,w);
	       HARDDEBUGF(("Need reset %d %d %d %d",i,
			   w->active_events,w->highwater,w->total_events));
	       erts_mtx_lock(&w->mtx);	       
	       /* Need reset, just check that it doesn't have got more to tell */
	       if (w->highwater != w->active_events) {
		   HARDDEBUGF(("Oups!"));
		   /* Oups, got signalled before we took the lock, can't reset */
		   if(!is_io_ready(ps)) {
		       erts_exit(ERTS_ERROR_EXIT,"Internal error: "
				"Inconsistent io structures in erl_poll.\n");
		   }
		   START_WAITER(ps,w);
		   erts_mtx_unlock(&w->mtx);
		   ps->restore_events = 1; 
		   continue;
	       }
	       w->active_events = w->highwater =  w->total_events;
	       START_WAITER(ps,w);
	       erts_mtx_unlock(&w->mtx);
	   } else {
	       erts_mtx_unlock(&w->mtx);
	   }
	}
    }

    no_fds = *len;

#ifdef ERTS_POLL_MAX_RES
    if (no_fds >= ERTS_POLL_MAX_RES)
	no_fds = ERTS_POLL_MAX_RES;
#endif

    timeout = poll_wait_timeout(ps, timeout_time);

    /*HARDDEBUGF(("timeout = %ld",(long) timeout));*/

    if (timeout > 0 && !erts_atomic32_read_nob(&break_waiter_state)) {
	HANDLE harr[2] = {ps->event_io_ready, break_happened_event};
	int num_h = 2;
        ERTS_MSACC_PUSH_STATE_M();

	HARDDEBUGF(("Start waiting %d [%d]",num_h, (int) timeout));
	ERTS_POLLSET_UNLOCK(ps);
#ifdef ERTS_SMP
	erts_thr_progress_prepare_wait(NULL);
#endif
        ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
	WaitForMultipleObjects(num_h, harr, FALSE, timeout);
#ifdef ERTS_SMP
	erts_thr_progress_finalize_wait(NULL);
#endif
        ERTS_MSACC_POP_STATE_M();
	ERTS_POLLSET_LOCK(ps);
	HARDDEBUGF(("Stop waiting %d [%d]",num_h, (int) timeout));
	woke_up(ps);
    }

    ERTS_UNSET_BREAK_REQUESTED;
    if(erts_atomic32_read_nob(&break_waiter_state)) {
	erts_mtx_lock(&break_waiter_lock);
	break_state = erts_atomic32_read_nob(&break_waiter_state);
	erts_atomic32_set_nob(&break_waiter_state,0);
	ResetEvent(break_happened_event);
	erts_mtx_unlock(&break_waiter_lock);
	switch (break_state) {
	case  BREAK_WAITER_GOT_BREAK:
	    ERTS_SET_BREAK_REQUESTED;
	    break;
	case  BREAK_WAITER_GOT_HALT:
	    erts_exit(0,"");
	    break;
	default:
	    break;
	}
    }

    res = wakeup_cause(ps);
    if (res != 0) {
	HARDDEBUGF(("%s!", res == EINTR ? "EINTR" : "ETIMEDOUT"));
	goto done;
    }

    reset_io_ready(ps);

    n = ps->num_waiters;	

    for (i = 0; i < n; i++) {
	Waiter* w = ps->waiter[i];
	int j;
	int first;
	int last;
	erts_mtx_lock(&w->mtx);
#ifdef DEBUG
	consistency_check(w);
#endif

	first = w->active_events;
	last = w->highwater;
	w->highwater = w->active_events;

	for (j = last-1; j >= first; --j) {
	    if (num >= no_fds) {
		w->highwater=j+1;
		erts_mtx_unlock(&w->mtx);
		/* This might mean we still have data to report,
		   restore flag indicating I/O ready! */
		restore_io_ready(ps);
		HARDDEBUGF(("To many FD's to report!"));
		goto done;
	    }
	    HARDDEBUGF(("SET! Restore events"));
	    ps->restore_events = 1;
	    HARDDEBUGF(("Report %d,%d",i,j));
	    pr[num].fd = (ErtsSysFdType) w->events[j];
	    pr[num].events = w->evdata[j]->mode;
#ifdef HARD_POLL_DEBUG
	    poll_debug_reported(w->events[j],w->highwater | (j << 16));
	    poll_debug_reported(w->events[j],first | (last << 16));
#endif
	    ++num;
	}

#ifdef DEBUG
	consistency_check(w);
#endif
	erts_mtx_unlock(&w->mtx);
    }
 done:
    set_timeout_time(ps, ERTS_MONOTONIC_TIME_MAX);
    *len = num;
    ERTS_POLLSET_UNLOCK(ps);
    HARDTRACEF(("Out erts_poll_wait"));
    return res;

}

int erts_poll_max_fds(void)
{
    int res = sys_max_files();
    HARDTRACEF(("In/Out erts_poll_max_fds -> %d",res));
    return res;
}

void erts_poll_info(ErtsPollSet ps,
		    ErtsPollInfo *pip)
{
    Uint size = 0;
    Uint num_events = 0;
    int i;

    HARDTRACEF(("In erts_poll_info"));
    ERTS_POLLSET_LOCK(ps);

    size += sizeof(struct ErtsPollSet_);
    size += sizeof(Waiter *) * ps->allocated_waiters;
    for (i = 0; i < ps->num_waiters; ++i) {
	Waiter *w = ps->waiter[i];
	if (w != NULL) {
	    size += sizeof(Waiter);
	    erts_mtx_lock(&w->mtx);
	    size += sizeof(EventData) * w->total_events;
	    num_events += (w->total_events - 1); /* First event is internal */
	    erts_mtx_unlock(&w->mtx);
	}
    }

    pip->primary = "WaitForMultipleObjects"; 

    pip->fallback = NULL;

    pip->kernel_poll = NULL;

    pip->memory_size = size;

    pip->poll_set_size = num_events;

    pip->fallback_poll_set_size = 0;

    pip->lazy_updates = 0;

    pip->pending_updates = 0;

    pip->batch_updates = 0;

    pip->concurrent_updates = 0;
    ERTS_POLLSET_UNLOCK(ps);

    pip->max_fds = erts_poll_max_fds();
    HARDTRACEF(("Out erts_poll_info"));

}

ErtsPollSet erts_poll_create_pollset(void)
{
    ErtsPollSet ps = SEL_ALLOC(ERTS_ALC_T_POLLSET,
			       sizeof(struct ErtsPollSet_));
    HARDTRACEF(("In erts_poll_create_pollset"));

    ps->num_waiters = 0;
    ps->allocated_waiters = 64;
    ps->waiter = SEL_ALLOC(ERTS_ALC_T_WAITER_OBJ,
			   sizeof(Waiter *)*ps->allocated_waiters);
    InitializeCriticalSection(&(ps->standby_crit));
    ps->standby_wait_counter = 0;
    ps->event_io_ready = CreateManualEvent(FALSE);
    ps->standby_wait_event = CreateManualEvent(FALSE); 
    ps->restore_events = 0;

    erts_atomic32_init_nob(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN);
#ifdef ERTS_SMP
    erts_smp_mtx_init(&ps->mtx, "pollset");
#endif
    init_timeout_time(ps);

    HARDTRACEF(("Out erts_poll_create_pollset"));
    return ps;
}

void erts_poll_destroy_pollset(ErtsPollSet ps)
{
    int i;
    HARDTRACEF(("In erts_poll_destroy_pollset"));
    ERTS_POLLSET_LOCK(ps);
    STOP_WAITERS(ps);
    for (i=0;i<ps->num_waiters;++i) {
	Waiter *w = ps->waiter[i];
	void *dummy;
	erts_tid_t t = w->this;
	/* Assume we're alone, no locking here... */
	w->active_events = w->total_events = w->highwater = 0;
	START_WAITER(ps,w);
	erts_thr_join(t,&dummy);
	CloseHandle(w->go_ahead);
	CloseHandle(w->events[0]);
	erts_mtx_destroy(&w->mtx);
	SEL_FREE(ERTS_ALC_T_WAITER_OBJ, (void *) w);
    }
    SEL_FREE(ERTS_ALC_T_WAITER_OBJ,ps->waiter);
    CloseHandle(ps->event_io_ready);
    CloseHandle(ps->standby_wait_event);
    ERTS_POLLSET_UNLOCK(ps);
#ifdef ERTS_SMP
    erts_smp_mtx_destroy(&ps->mtx);
#endif
    SEL_FREE(ERTS_ALC_T_POLLSET, (void *) ps);
    HARDTRACEF(("Out erts_poll_destroy_pollset"));
}

/*
 * Actually mostly initializes the friend module sys_interrupt...
 */
void  erts_poll_init(void)
{
    erts_tid_t thread;

#ifdef HARD_POLL_DEBUG
    poll_debug_init();
#endif

    HARDTRACEF(("In erts_poll_init"));
    erts_sys_break_event = CreateManualEvent(FALSE);

    erts_mtx_init(&break_waiter_lock,"break_waiter_lock");
    break_happened_event = CreateManualEvent(FALSE);
    erts_atomic32_init_nob(&break_waiter_state, 0); 

    erts_thr_create(&thread, &break_waiter, NULL, NULL);
    ERTS_UNSET_BREAK_REQUESTED;
    HARDTRACEF(("Out erts_poll_init"));
}

/*
 * Non windows friendly interface, not used when fd's are not continous
 */
void  erts_poll_get_selected_events(ErtsPollSet ps,
				    ErtsPollEvents ev[],
				    int len)
{
    int i;
    HARDTRACEF(("In erts_poll_get_selected_events"));
    for (i = 0; i < len; ++i)
	ev[i] = 0;
    HARDTRACEF(("Out erts_poll_get_selected_events"));
}
