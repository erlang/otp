/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
 * Description:	Poll interface suitable for ERTS with or without
 *              SMP support.
 *
 *		The interface is currently implemented using:
 *		- select
 *		- poll
 *              - /dev/poll
 *              - epoll with poll or select as fallback
 *              - kqueue with poll or select as fallback
 *
 *		Some time in the future it will also be
 *		implemented using Solaris ports.
 *
 *
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#if defined(__DARWIN__) || defined(__APPLE__) && defined(__MACH__)
/* Setting _DARWIN_UNLIMITED_SELECT before including sys/select.h enables
 * the version of select() that does not place a limit on the fd_set.
 */
#  define _DARWIN_UNLIMITED_SELECT
#endif

#ifndef WANT_NONBLOCKING
#  define WANT_NONBLOCKING
#endif
#define ERTS_WANT_GOT_SIGUSR1

#include "erl_poll.h"
#if ERTS_POLL_USE_KQUEUE
#  include <sys/types.h>
#  include <sys/event.h>
#  include <sys/time.h>
#endif
#if ERTS_POLL_USE_SELECT
#  ifdef SYS_SELECT_H
#    include <sys/select.h>
#  endif
#endif
#ifdef NO_SYSCONF
#  if ERTS_POLL_USE_SELECT
#    include <sys/param.h>
#  else
#    include <limits.h>
#  endif
#endif
#include "erl_thr_progress.h"
#include "erl_driver.h"
#include "erl_alloc.h"
#include "erl_msacc.h"

#if !defined(ERTS_POLL_USE_EPOLL) \
    && !defined(ERTS_POLL_USE_DEVPOLL)  \
    && !defined(ERTS_POLL_USE_POLL) \
    && !defined(ERTS_POLL_USE_SELECT)
#error "Missing implementation of erts_poll()"
#endif

#if defined(ERTS_KERNEL_POLL_VERSION) && !ERTS_POLL_USE_KERNEL_POLL
#error "Missing kernel poll implementation of erts_poll()"
#endif

#if defined(ERTS_NO_KERNEL_POLL_VERSION) && ERTS_POLL_USE_KERNEL_POLL
#error "Kernel poll used when it shouldn't be used"
#endif

#if 0
#define ERTS_POLL_DEBUG_PRINT
#endif

#if defined(DEBUG) && 0
#define HARD_DEBUG
#endif

#ifdef _DARWIN_UNLIMITED_SELECT
typedef struct {
    size_t sz;
    fd_set* ptr;
}ERTS_fd_set;
#  define ERTS_FD_CLR(fd, fds)	FD_CLR((fd), (fds)->ptr)
#  define ERTS_FD_SET(fd, fds)	FD_SET((fd), (fds)->ptr)
#  define ERTS_FD_ISSET(fd,fds) FD_ISSET((fd), (fds)->ptr)
#  define ERTS_FD_ZERO(fds)	memset((fds)->ptr, 0, (fds)->sz)
#  define ERTS_FD_SIZE(n)	((((n)+NFDBITS-1)/NFDBITS)*sizeof(fd_mask))

static void ERTS_FD_COPY(ERTS_fd_set *src, ERTS_fd_set *dst)
{
    if (dst->sz != src->sz) {
	dst->ptr = dst->ptr
	    ? erts_realloc(ERTS_ALC_T_SELECT_FDS, dst->ptr, src->sz)
	    : erts_alloc(ERTS_ALC_T_SELECT_FDS, src->sz);
	dst->sz = src->sz;
    }
    memcpy(dst->ptr, src->ptr, src->sz);
}

static ERTS_INLINE
int ERTS_SELECT(int nfds, ERTS_fd_set *readfds, ERTS_fd_set *writefds,
		ERTS_fd_set *exceptfds, struct timeval *timeout)
{
    ASSERT(!readfds || readfds->sz >= ERTS_FD_SIZE(nfds));
    ASSERT(!writefds || writefds->sz >= ERTS_FD_SIZE(nfds));
    ASSERT(!exceptfds);
    return select(nfds, 
		  (readfds ? readfds->ptr : NULL ),
		  (writefds ? writefds->ptr : NULL),
		  NULL,
		  timeout);
}

#else /* !_DARWIN_UNLIMITED_SELECT */
#  define ERTS_fd_set	fd_set
#  define ERTS_FD_CLR	FD_CLR
#  define ERTS_FD_ISSET FD_ISSET
#  define ERTS_FD_SET	FD_SET
#  define ERTS_FD_ZERO	FD_ZERO
#  define ERTS_FD_COPY(src,dst) (*(dst) = *(src))
#  define ERTS_SELECT	select
#endif

#define ERTS_POLL_USE_BATCH_UPDATE_POLLSET (ERTS_POLL_USE_DEVPOLL \
					    || ERTS_POLL_USE_KQUEUE)
#define ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE \
   (defined(ERTS_SMP) || ERTS_POLL_USE_KERNEL_POLL || ERTS_POLL_USE_POLL)

#define ERTS_POLL_USE_CONCURRENT_UPDATE \
   (defined(ERTS_SMP) && ERTS_POLL_USE_EPOLL)

#define ERTS_POLL_COALESCE_KP_RES (ERTS_POLL_USE_KQUEUE || ERTS_POLL_USE_EPOLL)

#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
#  define ERTS_POLL_ASYNC_INTERRUPT_SUPPORT 1
#else
#  define ERTS_POLL_ASYNC_INTERRUPT_SUPPORT 0
#endif

#define ERTS_POLL_USE_WAKEUP_PIPE \
   (ERTS_POLL_ASYNC_INTERRUPT_SUPPORT || defined(USE_THREADS))

#ifdef ERTS_SMP

#define ERTS_POLLSET_LOCK(PS) \
  erts_smp_mtx_lock(&(PS)->mtx)
#define ERTS_POLLSET_UNLOCK(PS) \
  erts_smp_mtx_unlock(&(PS)->mtx)

#define ERTS_POLLSET_SET_POLLED_CHK(PS) \
  ((int) erts_atomic32_xchg_nob(&(PS)->polled, (erts_aint32_t) 1))
#define ERTS_POLLSET_UNSET_POLLED(PS) \
  erts_atomic32_set_nob(&(PS)->polled, (erts_aint32_t) 0)
#define ERTS_POLLSET_IS_POLLED(PS) \
  ((int) erts_atomic32_read_nob(&(PS)->polled))

#else

#define ERTS_POLLSET_LOCK(PS)
#define ERTS_POLLSET_UNLOCK(PS)
#define ERTS_POLLSET_SET_POLLED_CHK(PS) 0
#define ERTS_POLLSET_UNSET_POLLED(PS)
#define ERTS_POLLSET_IS_POLLED(PS) 0

#endif

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
#define ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(PS) \
  erts_smp_atomic32_set_nob(&(PS)->have_update_requests, (erts_aint32_t) 1)
#define ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(PS) \
  erts_smp_atomic32_set_nob(&(PS)->have_update_requests, (erts_aint32_t) 0)
#define ERTS_POLLSET_HAVE_UPDATE_REQUESTS(PS) \
  ((int) erts_smp_atomic32_read_nob(&(PS)->have_update_requests))
#else
#define ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(PS)
#define ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(PS)
#define ERTS_POLLSET_HAVE_UPDATE_REQUESTS(PS) 0
#endif

#if ERTS_POLL_USE_FALLBACK
#  if ERTS_POLL_USE_POLL
#    define ERTS_POLL_NEED_FALLBACK(PS) ((PS)->no_poll_fds > 1)
#  elif ERTS_POLL_USE_SELECT
#    define ERTS_POLL_NEED_FALLBACK(PS) ((PS)->no_select_fds > 1)
#  endif
#endif
/*
 * --- Data types ------------------------------------------------------------
 */

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
#define ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE 128

typedef struct ErtsPollSetUpdateRequestsBlock_ ErtsPollSetUpdateRequestsBlock;
struct ErtsPollSetUpdateRequestsBlock_ {
    ErtsPollSetUpdateRequestsBlock *next;
    int len;
    int fds[ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE];
};

#endif


#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
#  define ERTS_POLL_FD_FLG_INURQ	(((unsigned short) 1) << 0)
#endif
#if ERTS_POLL_USE_FALLBACK
#  define ERTS_POLL_FD_FLG_INFLBCK	(((unsigned short) 1) << 1)
#  define ERTS_POLL_FD_FLG_USEFLBCK	(((unsigned short) 1) << 2)
#endif
#if ERTS_POLL_USE_KERNEL_POLL || defined(ERTS_SMP)
#  define ERTS_POLL_FD_FLG_RST		(((unsigned short) 1) << 3)
#endif
typedef struct {
#if ERTS_POLL_USE_POLL
    int pix;
#endif
    ErtsPollEvents used_events;
    ErtsPollEvents events;
#if ERTS_POLL_COALESCE_KP_RES
    unsigned short res_ev_ix;
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE || ERTS_POLL_USE_FALLBACK
    unsigned short flags;
#endif

} ErtsFdStatus;


#if ERTS_POLL_COALESCE_KP_RES
/* res_ev_ix max value */
#define ERTS_POLL_MAX_RES ((1 << sizeof(unsigned short)*8) - 1)
#endif

#if ERTS_POLL_USE_KQUEUE

#define ERTS_POLL_KQ_OP_HANDLED			1
#define ERTS_POLL_KQ_OP_DEL_R			2
#define ERTS_POLL_KQ_OP_DEL_W			3
#define ERTS_POLL_KQ_OP_ADD_R			4
#define ERTS_POLL_KQ_OP_ADD_W			5
#define ERTS_POLL_KQ_OP_ADD2_R			6
#define ERTS_POLL_KQ_OP_ADD2_W			7

#endif

struct ErtsPollSet_ {
    ErtsPollSet next;
    int internal_fd_limit;
    ErtsFdStatus *fds_status;
    erts_smp_atomic_t no_of_user_fds;
    int fds_status_len;
#if ERTS_POLL_USE_KERNEL_POLL
    int kp_fd;
    int res_events_len;
#if ERTS_POLL_USE_EPOLL
    struct epoll_event *res_events;
#elif ERTS_POLL_USE_KQUEUE
    struct kevent *res_events;
#elif ERTS_POLL_USE_DEVPOLL
    struct pollfd *res_events;
#endif
#endif /* ERTS_POLL_USE_KERNEL_POLL */
#if ERTS_POLL_USE_POLL
    int next_poll_fds_ix;
    int no_poll_fds;
    int poll_fds_len;
    struct pollfd*poll_fds;
#elif ERTS_POLL_USE_SELECT
    int next_sel_fd;
    int max_fd;
#if ERTS_POLL_USE_FALLBACK
    int no_select_fds;
#endif
    ERTS_fd_set input_fds;
    ERTS_fd_set res_input_fds;
    ERTS_fd_set output_fds;
    ERTS_fd_set res_output_fds;
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    ErtsPollSetUpdateRequestsBlock update_requests;
    ErtsPollSetUpdateRequestsBlock *curr_upd_req_block;
    erts_smp_atomic32_t have_update_requests;
#endif
#ifdef ERTS_SMP
    erts_atomic32_t polled;
    erts_smp_mtx_t mtx;
#endif
#if ERTS_POLL_USE_WAKEUP_PIPE
    int wake_fds[2];
#endif
#if ERTS_POLL_USE_TIMERFD
    int timer_fd;
#endif
#if ERTS_POLL_USE_FALLBACK
    int fallback_used;
#endif
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    erts_atomic32_t wakeup_state;
#endif
    erts_atomic64_t timeout_time;
#ifdef ERTS_POLL_COUNT_AVOIDED_WAKEUPS
    erts_smp_atomic_t no_avoided_wakeups;
    erts_smp_atomic_t no_avoided_interrupts;
    erts_smp_atomic_t no_interrupt_timed;
#endif
};

void erts_silence_warn_unused_result(long unused);
static void fatal_error(char *format, ...);
static void fatal_error_async_signal_safe(char *error_str);

static int max_fds = -1;
static ErtsPollSet pollsets;
static erts_smp_spinlock_t pollsets_lock;

#if ERTS_POLL_USE_POLL

static ERTS_INLINE short
ev2pollev(ErtsPollEvents ev)
{
#if !ERTS_POLL_USE_FALLBACK || ERTS_POLL_USE_KQUEUE
    return ERTS_POLL_EV_E2N(ev);
#else /* Note, we only map events we are interested in */
    short res_ev = (short) 0;
    if (ev & ERTS_POLL_EV_IN)
	res_ev |= ERTS_POLL_EV_NKP_IN;
    if (ev & ERTS_POLL_EV_OUT)
	res_ev |= ERTS_POLL_EV_NKP_OUT;
    return res_ev;
#endif
}

static ERTS_INLINE ErtsPollEvents
pollev2ev(short ev)
{
#if !ERTS_POLL_USE_FALLBACK || ERTS_POLL_USE_KQUEUE
    return ERTS_POLL_EV_N2E(ev);
#else /* Note, we only map events we are interested in */
    ErtsPollEvents res_ev = (ErtsPollEvents) 0;
    if (ev & ERTS_POLL_EV_NKP_IN)
	res_ev |= ERTS_POLL_EV_IN;
    if (ev & ERTS_POLL_EV_NKP_OUT)
	res_ev |= ERTS_POLL_EV_OUT;
    if (ev & ERTS_POLL_EV_NKP_ERR)
	res_ev |= ERTS_POLL_EV_ERR;
    if (ev & ERTS_POLL_EV_NKP_NVAL)
	res_ev |= ERTS_POLL_EV_NVAL;
    return res_ev;
#endif
}

#endif

#ifdef HARD_DEBUG
static void check_poll_result(ErtsPollResFd pr[], int len);
#if ERTS_POLL_USE_DEVPOLL
static void check_poll_status(ErtsPollSet ps);
#endif /* ERTS_POLL_USE_DEVPOLL */
#endif /* HARD_DEBUG */
#ifdef ERTS_POLL_DEBUG_PRINT
static void print_misc_debug_info(void);
#endif

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

#define ERTS_POLL_NOT_WOKEN	0
#define ERTS_POLL_WOKEN		-1
#define ERTS_POLL_WOKEN_INTR	1

static ERTS_INLINE void
reset_wakeup_state(ErtsPollSet ps)
{
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    erts_atomic32_set_mb(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN);
#endif
}

static ERTS_INLINE int
is_woken(ErtsPollSet ps)
{
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    return erts_atomic32_read_acqb(&ps->wakeup_state) != ERTS_POLL_NOT_WOKEN;
#else
    return 0;
#endif
}

static ERTS_INLINE int
is_interrupted_reset(ErtsPollSet ps)
{
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    return (erts_atomic32_xchg_acqb(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN)
	    == ERTS_POLL_WOKEN_INTR);
#else
    return 0;
#endif
}

static ERTS_INLINE void
woke_up(ErtsPollSet ps)
{
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    erts_aint32_t wakeup_state = erts_atomic32_read_acqb(&ps->wakeup_state);
    if (wakeup_state == ERTS_POLL_NOT_WOKEN)
	(void) erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
					 ERTS_POLL_WOKEN,
					 ERTS_POLL_NOT_WOKEN);
    ASSERT(erts_atomic32_read_nob(&ps->wakeup_state) != ERTS_POLL_NOT_WOKEN);
#endif
}

/*
 * --- Wakeup pipe -----------------------------------------------------------
 */

#if ERTS_POLL_USE_WAKEUP_PIPE

static ERTS_INLINE void
wake_poller(ErtsPollSet ps, int interrupted, int async_signal_safe)
{
    int wake;
    if (async_signal_safe)
	wake = 1;
    else {
	erts_aint32_t wakeup_state;
	if (!interrupted)
	    wakeup_state = erts_atomic32_cmpxchg_relb(&ps->wakeup_state,
						      ERTS_POLL_WOKEN,
						      ERTS_POLL_NOT_WOKEN);
	else
            wakeup_state = erts_atomic32_xchg_relb(&ps->wakeup_state,
                                                   ERTS_POLL_WOKEN_INTR);
	wake = wakeup_state == ERTS_POLL_NOT_WOKEN;
    }
    /*
     * NOTE: This function might be called from signal handlers in the
     *       non-smp case; therefore, it has to be async-signal safe in
     *       the non-smp case.
     */
    if (wake) {
	ssize_t res;
	if (ps->wake_fds[1] < 0)
	    return; /* Not initialized yet */
	do {
	    /* write() is async-signal safe (according to posix) */
	    res = write(ps->wake_fds[1], "!", 1);
	} while (res < 0 && errno == EINTR);
	if (res <= 0 && errno != ERRNO_BLOCK) {
	    if (async_signal_safe)
		fatal_error_async_signal_safe(__FILE__
					      ":XXX:wake_poller(): "
					      "Failed to write on wakeup pipe\n");
	    else
		fatal_error("%s:%d:wake_poller(): "
			    "Failed to write to wakeup pipe fd=%d: "
			    "%s (%d)\n",
			    __FILE__, __LINE__,
			    ps->wake_fds[1],
			    erl_errno_id(errno), errno);
	}
    }
}

static ERTS_INLINE void
cleanup_wakeup_pipe(ErtsPollSet ps)
{
#if ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    int intr = 0;
#endif
    int fd = ps->wake_fds[0];
    int res;
    do {
	char buf[32];
	res = read(fd, buf, sizeof(buf));
#if ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
	if (res > 0)
	    intr = 1;
#endif
    } while (res > 0 || (res < 0 && errno == EINTR));
    if (res < 0 && errno != ERRNO_BLOCK) {
	fatal_error("%s:%d:cleanup_wakeup_pipe(): "
		    "Failed to read on wakeup pipe fd=%d: "
		    "%s (%d)\n",
		    __FILE__, __LINE__,
		    fd,
		    erl_errno_id(errno), errno);
    }
#if ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    if (intr)
	erts_atomic32_set_nob(&ps->wakeup_state, ERTS_POLL_WOKEN_INTR);
#endif
}

static void
create_wakeup_pipe(ErtsPollSet ps)
{
    int do_wake = 0;
    int wake_fds[2];
    ps->wake_fds[0] = -1;
    ps->wake_fds[1] = -1;
    if (pipe(wake_fds) < 0) {
	fatal_error("%s:%d:create_wakeup_pipe(): "
		    "Failed to create pipe: %s (%d)\n",
		    __FILE__,
		    __LINE__,
		    erl_errno_id(errno),
		    errno);
    }
    SET_NONBLOCKING(wake_fds[0]);
    SET_NONBLOCKING(wake_fds[1]);

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("wakeup fds = {%d, %d}\n", wake_fds[0], wake_fds[1]);
#endif

    ERTS_POLL_EXPORT(erts_poll_control)(ps,
					wake_fds[0],
					ERTS_POLL_EV_IN,
					1, &do_wake);
#if ERTS_POLL_USE_FALLBACK
    /* We depend on the wakeup pipe being handled by kernel poll */
    if (ps->fds_status[wake_fds[0]].flags & ERTS_POLL_FD_FLG_INFLBCK)
	fatal_error("%s:%d:create_wakeup_pipe(): Internal error\n",
		    __FILE__, __LINE__);
#endif
    if (ps->internal_fd_limit <= wake_fds[1])
	ps->internal_fd_limit = wake_fds[1] + 1;
    if (ps->internal_fd_limit <= wake_fds[0])
	ps->internal_fd_limit = wake_fds[0] + 1;
    ps->wake_fds[0] = wake_fds[0];
    ps->wake_fds[1] = wake_fds[1];
}

#endif /* ERTS_POLL_USE_WAKEUP_PIPE */

/*
 * --- timer fd -----------------------------------------------------------
 */

#if ERTS_POLL_USE_TIMERFD

/* We use the timerfd when using epoll_wait to get high accuracy
   timeouts, i.e. we want to sleep with < ms accuracy. */

static void
create_timerfd(ErtsPollSet ps)
{
    int do_wake = 0;
    int timer_fd;
    timer_fd = timerfd_create(CLOCK_MONOTONIC,0);
    ERTS_POLL_EXPORT(erts_poll_control)(ps,
					timer_fd,
					ERTS_POLL_EV_IN,
					1, &do_wake);
#if ERTS_POLL_USE_FALLBACK
    /* We depend on the wakeup pipe being handled by kernel poll */
    if (ps->fds_status[timer_fd].flags & ERTS_POLL_FD_FLG_INFLBCK)
	fatal_error("%s:%d:create_wakeup_pipe(): Internal error\n",
		    __FILE__, __LINE__);
#endif
    if (ps->internal_fd_limit <= timer_fd)
	ps->internal_fd_limit = timer_fd + 1;
    ps->timer_fd = timer_fd;
}

static ERTS_INLINE void
timerfd_set(ErtsPollSet ps, struct itimerspec *its)
{
#ifdef DEBUG
    struct itimerspec old_its;
    int res;
    res = timerfd_settime(ps->timer_fd, 0, its, &old_its);
    ASSERT(res == 0);
    ASSERT(old_its.it_interval.tv_sec == 0 &&
           old_its.it_interval.tv_nsec == 0 &&
           old_its.it_value.tv_sec == 0 &&
           old_its.it_value.tv_nsec == 0);

#else
    timerfd_settime(ps->timer_fd, 0, its, NULL);
#endif
}

static ERTS_INLINE int
timerfd_clear(ErtsPollSet ps, int res, int max_res) {

    struct itimerspec its;
    /* we always have to clear the timer */
    its.it_interval.tv_sec = 0;
    its.it_interval.tv_nsec = 0;
    its.it_value.tv_sec = 0;
    its.it_value.tv_nsec = 0;
    timerfd_settime(ps->timer_fd, 0, &its, NULL);

    /* only timeout fd triggered */
    if (res == 1 && ps->res_events[0].data.fd == ps->timer_fd)
        return 0;

    return res;
}

#endif /* ERTS_POLL_USE_TIMERFD */


/*
 * --- Poll set update requests ----------------------------------------------
 */
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE

static ERTS_INLINE void
enqueue_update_request(ErtsPollSet ps, int fd)
{
    ErtsPollSetUpdateRequestsBlock *urqbp;

    ASSERT(fd < ps->fds_status_len);

    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INURQ)
	return;

    if (ps->update_requests.len == 0)
	ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(ps);

    urqbp = ps->curr_upd_req_block;

    if (urqbp->len == ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE) {
	ASSERT(!urqbp->next);
	urqbp = erts_alloc(ERTS_ALC_T_POLLSET_UPDREQ,
			   sizeof(ErtsPollSetUpdateRequestsBlock));
	ps->curr_upd_req_block->next = urqbp;
	ps->curr_upd_req_block = urqbp;
	urqbp->next = NULL;
	urqbp->len = 0;
    }

    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INURQ;
    urqbp->fds[urqbp->len++] = fd;
}

static ERTS_INLINE void
free_update_requests_block(ErtsPollSet ps,
			   ErtsPollSetUpdateRequestsBlock *urqbp)
{
    if (urqbp != &ps->update_requests)
	erts_free(ERTS_ALC_T_POLLSET_UPDREQ, (void *) urqbp);
    else {
	urqbp->next = NULL;
	urqbp->len = 0;
    }
}

#endif /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

/*
 * --- Growing poll set structures -------------------------------------------
 */

#ifndef ERTS_KERNEL_POLL_VERSION   /* only one shared implementation */

#define ERTS_FD_TABLE_MIN_LENGTH	1024
#define ERTS_FD_TABLE_EXP_THRESHOLD	(2048*1024)

int erts_poll_new_table_len (int old_len, int need_len)
{
    int new_len;

    ASSERT(need_len > old_len);
    if (need_len < ERTS_FD_TABLE_MIN_LENGTH) {
	new_len = ERTS_FD_TABLE_MIN_LENGTH;
    }
    else {
        new_len = old_len;
        do {            
            if (new_len < ERTS_FD_TABLE_EXP_THRESHOLD)
                new_len *= 2;
            else
                new_len += ERTS_FD_TABLE_EXP_THRESHOLD;

        } while (new_len < need_len);
    }
    ASSERT(new_len >= need_len);
    return new_len;
}
#endif

#if ERTS_POLL_USE_KERNEL_POLL
static void
grow_res_events(ErtsPollSet ps, int new_len)
{
    size_t new_size = sizeof(
#if ERTS_POLL_USE_EPOLL
	struct epoll_event
#elif ERTS_POLL_USE_DEVPOLL
	struct pollfd
#elif ERTS_POLL_USE_KQUEUE
	struct kevent
#endif
	) * erts_poll_new_table_len(ps->res_events_len, new_len);
    /* We do not need to save previously stored data */
    if (ps->res_events)
	erts_free(ERTS_ALC_T_POLL_RES_EVS, ps->res_events);
    ps->res_events = erts_alloc(ERTS_ALC_T_POLL_RES_EVS, new_size);
    ps->res_events_len = new_len;
}
#endif /* ERTS_POLL_USE_KERNEL_POLL */

#if ERTS_POLL_USE_POLL
static void
grow_poll_fds(ErtsPollSet ps, int min_ix)
{
    int i;
    int new_len = erts_poll_new_table_len(ps->poll_fds_len, min_ix + 1);
    if (new_len > max_fds)
	new_len = max_fds;
    ps->poll_fds = (ps->poll_fds_len
		    ? erts_realloc(ERTS_ALC_T_POLL_FDS,
				   ps->poll_fds,
				   sizeof(struct pollfd)*new_len)
		    : erts_alloc(ERTS_ALC_T_POLL_FDS,
				 sizeof(struct pollfd)*new_len));
    for (i = ps->poll_fds_len; i < new_len; i++) {
	ps->poll_fds[i].fd = -1;
	ps->poll_fds[i].events = (short) 0;
	ps->poll_fds[i].revents = (short) 0;
    }
    ps->poll_fds_len = new_len;
}
#endif

#ifdef _DARWIN_UNLIMITED_SELECT
static void
grow_select_fds(int fd, ERTS_fd_set* fds)
{
    int new_len = erts_poll_new_table_len(fds->sz, fd + 1);
    if (new_len > max_fds)
	new_len = max_fds;
    new_len = ERTS_FD_SIZE(new_len);
    fds->ptr = fds->sz
	? erts_realloc(ERTS_ALC_T_SELECT_FDS, fds->ptr, new_len)
	: erts_alloc(ERTS_ALC_T_SELECT_FDS, new_len);
    memset((char*)fds->ptr + fds->sz, 0, new_len - fds->sz);
    fds->sz = new_len;
}
static ERTS_INLINE void
ensure_select_fds(int fd, ERTS_fd_set* in, ERTS_fd_set* out)
{
    ASSERT(in->sz == out->sz);
    if (ERTS_FD_SIZE(fd+1) > in->sz) {
	grow_select_fds(fd, in);
	grow_select_fds(fd, out);
    }
}
#else
#  define ensure_select_fds(fd, in, out) do {} while(0)
#endif /* _DARWIN_UNLIMITED_SELECT */

static void
grow_fds_status(ErtsPollSet ps, int min_fd)
{
    int i;
    int new_len = erts_poll_new_table_len(ps->fds_status_len, min_fd + 1);
    ASSERT(min_fd < max_fds);
    if (new_len > max_fds)
	new_len = max_fds;
    ps->fds_status = (ps->fds_status_len
		      ? erts_realloc(ERTS_ALC_T_FD_STATUS,
				     ps->fds_status,
				     sizeof(ErtsFdStatus)*new_len)
		      : erts_alloc(ERTS_ALC_T_FD_STATUS,
				   sizeof(ErtsFdStatus)*new_len));
    for (i = ps->fds_status_len; i < new_len; i++) {
#if ERTS_POLL_USE_POLL
	ps->fds_status[i].pix = -1;
#endif
	ps->fds_status[i].used_events = (ErtsPollEvents) 0;
	ps->fds_status[i].events = (ErtsPollEvents) 0;
#if ERTS_POLL_COALESCE_KP_RES
	ps->fds_status[i].res_ev_ix = (unsigned short) ERTS_POLL_MAX_RES;
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE || ERTS_POLL_USE_FALLBACK
	ps->fds_status[i].flags = (unsigned short) 0;
#endif
    }
    ps->fds_status_len = new_len;
}

/*
 * --- Selecting fd to poll on -----------------------------------------------
 */

#if ERTS_POLL_USE_FALLBACK
static int update_fallback_pollset(ErtsPollSet ps, int fd);
#endif

static ERTS_INLINE int
need_update(ErtsPollSet ps, int fd)
{
#if ERTS_POLL_USE_KERNEL_POLL
    int reset;
#endif

    ASSERT(fd < ps->fds_status_len);

#if ERTS_POLL_USE_KERNEL_POLL
    reset = (int) (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST);
    if (reset && !ps->fds_status[fd].used_events) {
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
	reset = 0;
    }
#elif defined(ERTS_SMP)
    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
#endif

    if (ps->fds_status[fd].used_events != ps->fds_status[fd].events)
	return 1;

#if ERTS_POLL_USE_KERNEL_POLL
    return reset;
#else
    return 0;
#endif
}

#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET

#if ERTS_POLL_USE_KQUEUE
#define ERTS_POLL_MIN_BATCH_BUF_SIZE 128
#else
#define ERTS_POLL_MIN_BATCH_BUF_SIZE 64
#endif

typedef struct {
    int len;
    int size;
#if ERTS_POLL_USE_DEVPOLL
    struct pollfd *buf;
#elif ERTS_POLL_USE_KQUEUE
    struct kevent *buf;
    struct kevent *ebuf;
#endif
} ErtsPollBatchBuf;


static ERTS_INLINE void
setup_batch_buf(ErtsPollSet ps, ErtsPollBatchBuf *bbp)
{
    bbp->len = 0;
#if ERTS_POLL_USE_DEVPOLL
    bbp->size = ps->res_events_len;
    bbp->buf = ps->res_events;
#elif ERTS_POLL_USE_KQUEUE
    bbp->size = ps->res_events_len/2;
    bbp->buf = ps->res_events;
    bbp->ebuf = bbp->buf + bbp->size;
#endif
}


#if ERTS_POLL_USE_DEVPOLL

static void
write_batch_buf(ErtsPollSet ps, ErtsPollBatchBuf *bbp)
{
    ssize_t wres;
    char *buf = (char *) bbp->buf;
    size_t buf_size = sizeof(struct pollfd)*bbp->len;
    
    while (1) {
	wres = write(ps->kp_fd, (void *) buf, buf_size);
	if (wres < 0) {
	    if (errno == EINTR)
		continue;
	    fatal_error("%s:%d:write_batch_buf(): "
			"Failed to write to /dev/poll: "
			"%s (%d)\n",
			__FILE__, __LINE__,
			erl_errno_id(errno), errno);
	}
	buf_size -= wres;
	if (buf_size <= 0)
	    break;
	buf += wres;
    }

    if (buf_size < 0) {
	fatal_error("%s:%d:write_devpoll_buf(): Internal error\n",
		    __FILE__, __LINE__);
    }
    bbp->len = 0;
}

#elif ERTS_POLL_USE_KQUEUE

static void
write_batch_buf(ErtsPollSet ps, ErtsPollBatchBuf *bbp)
{
    int res;
    int len = bbp->len;
    struct kevent *buf = bbp->buf;
    struct timespec ts = {0, 0};

    do {
	res = kevent(ps->kp_fd, buf, len, NULL, 0, &ts);
    } while (res < 0 && errno == EINTR);
    if (res < 0) {
	int i;
	struct kevent *ebuf = bbp->ebuf;
	do {
	    res = kevent(ps->kp_fd, buf, len, ebuf, len, &ts);
	} while (res < 0 && errno == EINTR);
	if (res < 0) {
	    fatal_error("%s:%d: kevent() failed: %s (%d)\n",
			__FILE__, __LINE__, erl_errno_id(errno), errno);
	}
	for (i = 0; i < res; i++) {
	    if (ebuf[i].flags & EV_ERROR) {
		short filter;
		int fd = (int) ebuf[i].ident;

		switch ((int) (long) ebuf[i].udata) {

		    /*
		     * Since we use a lazy update approach EV_DELETE will
		     * frequently fail. This since kqueue automatically
		     * removes a file descriptor that is closed from the
		     * poll set.
		     */
		case ERTS_POLL_KQ_OP_DEL_R:
		case ERTS_POLL_KQ_OP_DEL_W:
		case ERTS_POLL_KQ_OP_HANDLED:
		    break;

		    /*
		     * According to the kqueue man page EVFILT_READ support
		     * does not imply EVFILT_WRITE support; therefore,
		     * if an EV_ADD fail, we may have to remove other
		     * events on this fd in the kqueue pollset before
		     * adding fd to the fallback pollset.
		     */
		case ERTS_POLL_KQ_OP_ADD_W:
		    if (ps->fds_status[fd].used_events & ERTS_POLL_EV_IN) {
			filter = EVFILT_READ;
			goto rm_add_fb;
		    }
		    goto add_fb;
		case ERTS_POLL_KQ_OP_ADD_R:
		    if (ps->fds_status[fd].used_events & ERTS_POLL_EV_OUT) {
			filter = EVFILT_WRITE;
			goto rm_add_fb;
		    }
		    goto add_fb;
		case ERTS_POLL_KQ_OP_ADD2_W:
		case ERTS_POLL_KQ_OP_ADD2_R: {
		    int j;
		    for (j = i+1; j < res; j++) {
			if (fd == (int) ebuf[j].ident) {
			    ebuf[j].udata = (void *) ERTS_POLL_KQ_OP_HANDLED;
			    if (!(ebuf[j].flags & EV_ERROR)) {
				switch ((int) (long) ebuf[j].udata) {
				case ERTS_POLL_KQ_OP_ADD2_W:
				    filter = EVFILT_WRITE;
				    goto rm_add_fb;
				case ERTS_POLL_KQ_OP_ADD2_R:
				    filter = EVFILT_READ;
				    goto rm_add_fb;
				default:
				    fatal_error("%s:%d:write_batch_buf(): "
						"Internal error",
						__FILE__, __LINE__);
				    break;
				}
			    }
			    goto add_fb;
			}
		    }
		    /* The other add succeded... */
		    filter = ((((int) (long) ebuf[i].udata)
			       == ERTS_POLL_KQ_OP_ADD2_W)
			      ? EVFILT_READ
			      : EVFILT_WRITE);
		rm_add_fb:
		    { 
			struct kevent kev;
			struct timespec ts = {0, 0};
			EV_SET(&kev, fd, filter, EV_DELETE, 0, 0, 0);
			(void) kevent(ps->kp_fd, &kev, 1, NULL, 0, &ts);
		    }

		add_fb:
		    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_USEFLBCK;
		    ASSERT(ps->fds_status[fd].used_events);
		    ps->fds_status[fd].used_events = 0;
		    erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
		    update_fallback_pollset(ps, fd);
		    ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
		    break;
		}
		default:
		    fatal_error("%s:%d:write_batch_buf(): Internal error",
				__FILE__, __LINE__);
		    break;
		}
	    }
	}
    }
    bbp->len = 0;
}

#endif /* ERTS_POLL_USE_KQUEUE */

static ERTS_INLINE void
batch_update_pollset(ErtsPollSet ps, int fd, ErtsPollBatchBuf *bbp)
{
    int buf_len;
#if ERTS_POLL_USE_DEVPOLL
    short events;
    struct pollfd *buf;
#elif ERTS_POLL_USE_KQUEUE
    struct kevent *buf;
#endif

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Doing lazy update on fd=%d\n", fd);
#endif

    if (!need_update(ps, fd))
	return;

    /* Make sure we have room for at least maximum no of entries
       per fd */
    if (bbp->size - bbp->len < 2)
	write_batch_buf(ps, bbp);

    buf_len = bbp->len;
    buf = bbp->buf;

    ASSERT(fd < ps->fds_status_len);

#if ERTS_POLL_USE_DEVPOLL
    events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
    if (!events) {
	buf[buf_len].events = POLLREMOVE;
	erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
    }
    else if (!ps->fds_status[fd].used_events) {
	buf[buf_len].events = events;
	erts_smp_atomic_inc_nob(&ps->no_of_user_fds);
    }
    else {
	if ((ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST)
	    || (ps->fds_status[fd].used_events & ~events)) {
	    /* Reset or removed events... */
	    buf[buf_len].fd = fd;
	    buf[buf_len].events = POLLREMOVE;
	    buf[buf_len++].revents = 0;
	}
	buf[buf_len].events = events;
    }
    buf[buf_len].fd = fd;
    buf[buf_len++].revents = 0;

#elif ERTS_POLL_USE_KQUEUE

    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK) {
	if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_USEFLBCK)
	    update_fallback_pollset(ps, fd);
	else { /* Remove from fallback and try kqueue */
	    ErtsPollEvents events = ps->fds_status[fd].events;
	    ps->fds_status[fd].events = (ErtsPollEvents) 0;
	    update_fallback_pollset(ps, fd);
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    if (events) {
		ps->fds_status[fd].events = events;
		goto try_kqueue;
	    }
	}
    }
    else {
	ErtsPollEvents events, used_events;
	int mod_w, mod_r;
    try_kqueue:
	events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
	used_events = ERTS_POLL_EV_E2N(ps->fds_status[fd].used_events);
	if (!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST)) {
	    if (!used_events &&
		(events & ERTS_POLL_EV_IN) && (events & ERTS_POLL_EV_OUT))
		goto do_add_rw;
	    mod_r = ((events & ERTS_POLL_EV_IN)
		     != (used_events & ERTS_POLL_EV_IN));
	    mod_w = ((events & ERTS_POLL_EV_OUT)
		     != (used_events & ERTS_POLL_EV_OUT));
	    goto do_mod;
	}
	else { /* Reset */
	    if ((events & ERTS_POLL_EV_IN) && (events & ERTS_POLL_EV_OUT)) {
	    do_add_rw:
		EV_SET(&buf[buf_len], fd, EVFILT_READ, EV_ADD,
		       0, 0, (void *) ERTS_POLL_KQ_OP_ADD2_R);
		buf_len++;
		EV_SET(&buf[buf_len], fd, EVFILT_WRITE, EV_ADD,
		       0, 0, (void *) ERTS_POLL_KQ_OP_ADD2_W);
		buf_len++;

	    }
	    else {
		mod_r = 1;
		mod_w = 1;
	    do_mod:
		if (mod_r) {
		    if (events & ERTS_POLL_EV_IN) {
			EV_SET(&buf[buf_len], fd, EVFILT_READ, EV_ADD,
			       0, 0, (void *) ERTS_POLL_KQ_OP_ADD_R);
			buf_len++;
		    }
		    else if (used_events & ERTS_POLL_EV_IN) {
			EV_SET(&buf[buf_len], fd, EVFILT_READ, EV_DELETE,
			       0, 0, (void *) ERTS_POLL_KQ_OP_DEL_R);
			buf_len++;
		    }
		}
		if (mod_w) {
		    if (events & ERTS_POLL_EV_OUT) {
			EV_SET(&buf[buf_len], fd, EVFILT_WRITE, EV_ADD,
			       0, 0, (void *) ERTS_POLL_KQ_OP_ADD_W);
			buf_len++;
		    }
		    else if (used_events & ERTS_POLL_EV_OUT) {
			EV_SET(&buf[buf_len], fd, EVFILT_WRITE, EV_DELETE,
			       0, 0, (void *) ERTS_POLL_KQ_OP_DEL_W);
			buf_len++;
		    }
		}
	    }
	}
	if (used_events) {
	    if (!events) {
		erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
	    }
	}
	else {
	    if (events)
		erts_smp_atomic_inc_nob(&ps->no_of_user_fds);
	}
	ASSERT((events & ~(ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) == 0);
	ASSERT((used_events & ~(ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) == 0);
    }	    

#endif

    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
    ps->fds_status[fd].used_events = ps->fds_status[fd].events;

    bbp->len = buf_len;
}

#else /* !ERTS_POLL_USE_BATCH_UPDATE_POLLSET */

#if ERTS_POLL_USE_EPOLL
static int
#if ERTS_POLL_USE_CONCURRENT_UPDATE
conc_update_pollset(ErtsPollSet ps, int fd, int *update_fallback)
#else
update_pollset(ErtsPollSet ps, int fd)
#endif
{
    int res;
    int op;
    struct epoll_event epe_templ;
    struct epoll_event epe;

    ASSERT(fd < ps->fds_status_len);

    if (!need_update(ps, fd))
	return 0;

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Doing update on fd=%d\n", fd);
#endif
    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK) {
#if ERTS_POLL_USE_CONCURRENT_UPDATE
	if (!*update_fallback) {
	    *update_fallback = 1;
	    return 0;
	}
#endif
	if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_USEFLBCK) {
	    return update_fallback_pollset(ps, fd);
	}
	else { /* Remove from fallback and try epoll */
	    ErtsPollEvents events = ps->fds_status[fd].events;
	    ps->fds_status[fd].events = (ErtsPollEvents) 0;
	    res = update_fallback_pollset(ps, fd);
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    if (!events)
		return res;
	    ps->fds_status[fd].events = events;
	}
    }

    epe_templ.events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
    epe_templ.data.fd = fd;

#ifdef VALGRIND
    /* Silence invalid valgrind warning ... */
    memset((void *) &epe.data, 0, sizeof(epoll_data_t));
#endif

    if (epe_templ.events && ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST) {
	do {
	    /* We init 'epe' every time since epoll_ctl() may modify it
	       (not declared const and not documented as const). */
	    epe.events = epe_templ.events;
	    epe.data.fd = epe_templ.data.fd;
	    res = epoll_ctl(ps->kp_fd, EPOLL_CTL_DEL, fd, &epe);
	} while (res != 0 && errno == EINTR);
	erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
	ps->fds_status[fd].used_events = 0;
    }

    if (!epe_templ.events) {
	/* A note on EPOLL_CTL_DEL: linux kernel versions before 2.6.9
	   need a non-NULL event pointer even though it is ignored... */
	op = EPOLL_CTL_DEL;
	erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
    }
    else if (!ps->fds_status[fd].used_events) {
	op = EPOLL_CTL_ADD;
	erts_smp_atomic_inc_nob(&ps->no_of_user_fds);
    }
    else {
	op = EPOLL_CTL_MOD;
    }

    do {
	/* We init 'epe' every time since epoll_ctl() may modify it
	   (not declared const and not documented as const). */
	epe.events = epe_templ.events;
	epe.data.fd = epe_templ.data.fd;
	res = epoll_ctl(ps->kp_fd, op, fd, &epe);
    } while (res != 0 && errno == EINTR);

#if defined(ERTS_POLL_DEBUG_PRINT) && 1
    {
	int saved_errno = errno;
	erts_printf("%s = epoll_ctl(%d, %s, %d, {Ox%x, %d})\n",
		     res == 0 ? "0" : erl_errno_id(errno),
		     ps->kp_fd,
		     (op == EPOLL_CTL_ADD
		      ? "EPOLL_CTL_ADD"
		      : (op == EPOLL_CTL_MOD
			 ? "EPOLL_CTL_MOD"
			 : (op == EPOLL_CTL_DEL
			    ? "EPOLL_CTL_DEL"
			    : "UNKNOWN"))),
		     fd,
		     epe_templ.events,
		     fd);
	errno = saved_errno;
    }
#endif
    if (res == 0)
	ps->fds_status[fd].used_events = ps->fds_status[fd].events;
    else {
	switch (op) {
	case EPOLL_CTL_MOD:
	    epe.events = 0;
	    do {
		/* We init 'epe' every time since epoll_ctl() may modify it
		   (not declared const and not documented as const). */
		epe.events = 0;
		epe.data.fd = fd;
		res = epoll_ctl(ps->kp_fd, EPOLL_CTL_DEL, fd, &epe);
	    } while (res != 0 && errno == EINTR);
	    ps->fds_status[fd].used_events = 0;
	/* Fall through ... */
	case EPOLL_CTL_ADD: {
	    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_USEFLBCK;
	    erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
#if ERTS_POLL_USE_CONCURRENT_UPDATE
	    if (!*update_fallback) {
		*update_fallback = 1;
		return 0;
	    }
#endif
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    res = update_fallback_pollset(ps, fd);
	    ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
	    break;
	}
	case EPOLL_CTL_DEL: {
	    /*
	     * Since we use a lazy update approach EPOLL_CTL_DEL will
	     * frequently fail. This since epoll automatically removes
	     * a filedescriptor that is closed from the poll set.
	     */
	    ps->fds_status[fd].used_events = 0;
	    res = 0;
	    break;
	}
	default:
	    fatal_error("%s:%d:update_pollset(): Internal error\n",
			__FILE__, __LINE__);
	    break;
	}
    }
    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
    return res;
}

#if ERTS_POLL_USE_CONCURRENT_UPDATE
static int
update_pollset(ErtsPollSet ps, int fd)
{
    int update_fallback = 1;
    return conc_update_pollset(ps, fd, &update_fallback);
}
#endif

#endif /* ERTS_POLL_USE_EPOLL */

#endif /* ERTS_POLL_USE_BATCH_UPDATE_POLLSET */

#if ERTS_POLL_USE_POLL || ERTS_POLL_USE_SELECT || ERTS_POLL_USE_FALLBACK

#if ERTS_POLL_USE_FALLBACK
static int update_fallback_pollset(ErtsPollSet ps, int fd)
#else
static int update_pollset(ErtsPollSet ps, int fd)
#endif
{
#ifdef ERTS_POLL_DEBUG_PRINT
#if ERTS_POLL_USE_FALLBACK
    erts_printf("Doing fallback update on fd=%d\n", fd);
#else
    erts_printf("Doing update on fd=%d\n", fd);
#endif
#endif

    ASSERT(fd < ps->fds_status_len);
#if ERTS_POLL_USE_FALLBACK
    ASSERT(ps->fds_status[fd].used_events
	   ? (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK)
	   : (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_USEFLBCK));
#endif

    if (!need_update(ps, fd))
	return 0;

#if ERTS_POLL_USE_FALLBACK
    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
#endif

#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
    if (!ps->fds_status[fd].events) {
	int pix = ps->fds_status[fd].pix;
	int last_pix;
	if (pix < 0) {
#if ERTS_POLL_USE_FALLBACK
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
#endif
	    return -1;
	}
#if ERTS_POLL_USE_FALLBACK
	ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
#endif
	erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
	last_pix = --ps->no_poll_fds;
	if (pix != last_pix) {
	/* Move last pix to this pix */
	    ps->poll_fds[pix].fd = ps->poll_fds[last_pix].fd;
	    ps->poll_fds[pix].events = ps->poll_fds[last_pix].events;
	    ps->poll_fds[pix].revents = ps->poll_fds[last_pix].revents;
	    ps->fds_status[ps->poll_fds[pix].fd].pix = pix;
	}
	/* Clear last pix */
	ps->poll_fds[last_pix].fd = -1;
	ps->poll_fds[last_pix].events = (short) 0;
	ps->poll_fds[last_pix].revents = (short) 0;
	/* Clear this fd status */
	ps->fds_status[fd].pix = -1;
	ps->fds_status[fd].used_events = (ErtsPollEvents) 0;
#if ERTS_POLL_USE_FALLBACK
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INFLBCK;
#endif
    }
    else {
	int pix = ps->fds_status[fd].pix;
	if (pix < 0) {
#if ERTS_POLL_USE_FALLBACK
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK)
		    || fd == ps->kp_fd);
#endif
	    erts_smp_atomic_inc_nob(&ps->no_of_user_fds);
	    ps->fds_status[fd].pix = pix = ps->no_poll_fds++;
	    if (pix >= ps->poll_fds_len)
		grow_poll_fds(ps, pix);
	    ps->poll_fds[pix].fd = fd;
	    ps->fds_status[fd].pix = pix;
#if ERTS_POLL_USE_FALLBACK
	    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INFLBCK;
#endif
	}

#if ERTS_POLL_USE_FALLBACK
	ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
#endif

	/* Events to be used in next poll */
	ps->poll_fds[pix].events = ev2pollev(ps->fds_status[fd].events);
	if (ps->poll_fds[pix].revents) {
	    /* Remove result events that we should not poll for anymore */
	    ps->poll_fds[pix].revents
		&= ev2pollev(~(~ps->fds_status[fd].used_events
			       & ps->fds_status[fd].events));
	}
	/* Save events to be used in next poll */
	ps->fds_status[fd].used_events = ps->fds_status[fd].events;
    }
    return 0;
#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
    {
	ErtsPollEvents events = ps->fds_status[fd].events;
	ensure_select_fds(fd, &ps->input_fds, &ps->output_fds);
	if ((ERTS_POLL_EV_IN & events)
	    != (ERTS_POLL_EV_IN & ps->fds_status[fd].used_events)) {
	    if (ERTS_POLL_EV_IN & events) {
		ERTS_FD_SET(fd, &ps->input_fds);
	    }
	    else {
		ERTS_FD_CLR(fd, &ps->input_fds);
	    }
	}
	if ((ERTS_POLL_EV_OUT & events)
	    != (ERTS_POLL_EV_OUT & ps->fds_status[fd].used_events)) {
	    if (ERTS_POLL_EV_OUT & events) {
		ERTS_FD_SET(fd, &ps->output_fds);
	    }
	    else {
		ERTS_FD_CLR(fd, &ps->output_fds);
	    }
	}

	if (!ps->fds_status[fd].used_events) {
	    ASSERT(events);
	    erts_smp_atomic_inc_nob(&ps->no_of_user_fds);
#if ERTS_POLL_USE_FALLBACK
	    ps->no_select_fds++;
	    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INFLBCK;
#endif
	}
	else if (!events) {
	    ASSERT(ps->fds_status[fd].used_events);
	    erts_smp_atomic_dec_nob(&ps->no_of_user_fds);
	    ps->fds_status[fd].events = events;
#if ERTS_POLL_USE_FALLBACK
	    ps->no_select_fds--;
	    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INFLBCK;
#endif
	}

	ps->fds_status[fd].used_events = events;

	if (events && fd > ps->max_fd)
	    ps->max_fd = fd;
	else if (!events && fd == ps->max_fd) {
	    int max = ps->max_fd;
	    for (max = ps->max_fd; max >= 0; max--)
		if (ps->fds_status[max].used_events)
		    break;
	    ps->max_fd = max;
	}
    }
    return 0;
#endif
}

#endif /* ERTS_POLL_USE_POLL || ERTS_POLL_USE_SELECT || ERTS_POLL_USE_FALLBACK */

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE

static void
handle_update_requests(ErtsPollSet ps)
{
    ErtsPollSetUpdateRequestsBlock *urqbp = &ps->update_requests;
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
    ErtsPollBatchBuf bb;
    setup_batch_buf(ps, &bb);
#endif

    while (urqbp) {
	ErtsPollSetUpdateRequestsBlock *free_urqbp = urqbp;
	int i;
	int len = urqbp->len;
	for (i = 0; i < len; i++) {
	    int fd = urqbp->fds[i];
	    ASSERT(fd < ps->fds_status_len);
	    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INURQ;
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
	    batch_update_pollset(ps, fd, &bb);
#else
	    update_pollset(ps, fd);
#endif
	}

	free_urqbp = urqbp;
	urqbp = urqbp->next;

	free_update_requests_block(ps, free_urqbp);

    }

#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
    if (bb.len)
	write_batch_buf(ps, &bb);
#endif

    ps->curr_upd_req_block = &ps->update_requests;

#if ERTS_POLL_USE_DEVPOLL && defined(HARD_DEBUG)
    check_poll_status(ps);
#endif

    ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(ps);
}

#endif /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

static ERTS_INLINE ErtsPollEvents
poll_control(ErtsPollSet ps, int fd, ErtsPollEvents events, int on, int *do_wake)
{
    ErtsPollEvents new_events;

    if (fd < ps->internal_fd_limit || fd >= max_fds) {
	if (fd < 0) {
	    new_events = ERTS_POLL_EV_ERR;
	    goto done;
	}
#if ERTS_POLL_USE_KERNEL_POLL
	if (fd == ps->kp_fd) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#endif
#if ERTS_POLL_USE_WAKEUP_PIPE
	if (fd == ps->wake_fds[0] || fd == ps->wake_fds[1]) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#endif
#if ERTS_POLL_USE_TIMERFD
	if (fd == ps->timer_fd) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#endif
    }

    if (fd >= ps->fds_status_len)
	grow_fds_status(ps, fd);

    ASSERT(fd < ps->fds_status_len);

    new_events = ps->fds_status[fd].events;

    if (events == 0) {
	*do_wake = 0;
	goto done;
    }

    if (on)
	new_events |= events;
    else
	new_events &= ~events;

    if (new_events == (ErtsPollEvents) 0) {
#if ERTS_POLL_USE_KERNEL_POLL || defined(ERTS_SMP)
	ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_RST;
#endif
#if ERTS_POLL_USE_FALLBACK
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_USEFLBCK;
#endif
    }

    ps->fds_status[fd].events = new_events;

    if (new_events == ps->fds_status[fd].used_events
#if ERTS_POLL_USE_KERNEL_POLL || defined(ERTS_SMP)
	&& !(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST)
#endif
	) {
	*do_wake = 0;
	goto done;
    }

#if !ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    if (update_pollset(ps, fd) != 0)
	new_events = ERTS_POLL_EV_ERR;
#else /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

#if ERTS_POLL_USE_CONCURRENT_UPDATE
    if (ERTS_POLLSET_IS_POLLED(ps)) {
	int update_fallback = 0;
	conc_update_pollset(ps, fd, &update_fallback);
	if (!update_fallback) {
	    *do_wake = 0; /* no need to wake kernel poller */
	    goto done;
	}
    }
#endif

    enqueue_update_request(ps, fd);
	
#ifdef ERTS_SMP
    /*
     * If new events have been added, we need to wake up the
     * polling thread, but if events have been removed we don't.
     */
    if ((new_events && (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST))
	|| (~ps->fds_status[fd].used_events & new_events))
	*do_wake = 1;
#endif /* ERTS_SMP */

#endif /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

 done:
#ifdef ERTS_POLL_DEBUG_PRINT
     erts_printf("0x%x = poll_control(ps, %d, 0x%x, %s) do_wake=%d\n",
		 (int) new_events, fd, (int) events, (on ? "on" : "off"), *do_wake);
#endif
    return new_events;
}

void
ERTS_POLL_EXPORT(erts_poll_controlv)(ErtsPollSet ps,
				     ErtsPollControlEntry pcev[],
				     int len)
{
    int i;
    int do_wake;
    int final_do_wake = 0;

    ERTS_POLLSET_LOCK(ps);

    for (i = 0; i < len; i++) {
	do_wake = 0;
	pcev[i].events = poll_control(ps,
				      pcev[i].fd,
				      pcev[i].events,
				      pcev[i].on,
				      &do_wake);
	final_do_wake |= do_wake;
    }

    ERTS_POLLSET_UNLOCK(ps);

#ifdef ERTS_SMP
    if (final_do_wake)
	wake_poller(ps, 0, 0);
#endif /* ERTS_SMP */

}

ErtsPollEvents
ERTS_POLL_EXPORT(erts_poll_control)(ErtsPollSet ps,
				    ErtsSysFdType fd,
				    ErtsPollEvents events,
				    int on,
				    int* do_wake) /* In: Wake up polling thread */
				                  /* Out: Poller is woken */
{
    ErtsPollEvents res;

    ERTS_POLLSET_LOCK(ps);

    res = poll_control(ps, fd, events, on, do_wake);

    ERTS_POLLSET_UNLOCK(ps);

#ifdef ERTS_SMP
    if (*do_wake) {
	wake_poller(ps, 0, 0);
    }
#endif /* ERTS_SMP */

    return res;
}

/*
 * --- Wait on poll set ------------------------------------------------------
 */

#if ERTS_POLL_USE_KERNEL_POLL

static ERTS_INLINE int
save_kp_result(ErtsPollSet ps, ErtsPollResFd pr[], int max_res, int chk_fds_res)
{
    int res = 0;
    int i;
    int n = chk_fds_res < max_res ? chk_fds_res : max_res;
#if ERTS_POLL_USE_WAKEUP_PIPE
    int wake_fd = ps->wake_fds[0];
#endif
#if ERTS_POLL_USE_TIMERFD
    int timer_fd = ps->timer_fd;
#endif

    for (i = 0; i < n; i++) {

#if ERTS_POLL_USE_EPOLL		/* --- epoll ------------------------------- */

	if (ps->res_events[i].events) {
	    int fd = ps->res_events[i].data.fd;
	    int ix;
	    ErtsPollEvents revents;
#if ERTS_POLL_USE_WAKEUP_PIPE
	    if (fd == wake_fd) {
		cleanup_wakeup_pipe(ps);
		continue;
	    }
#endif
#if ERTS_POLL_USE_TIMERFD
            if (fd == timer_fd) {
                continue;
            }
#endif
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    /* epoll_wait() can repeat the same fd in result array... */
	    ix = (int) ps->fds_status[fd].res_ev_ix;
	    ASSERT(ix >= 0);
	    if (ix >= res || pr[ix].fd != fd) {
		ix = res;
		pr[ix].fd = fd;
		pr[ix].events = (ErtsPollEvents) 0;
	    }

	    revents = ERTS_POLL_EV_N2E(ps->res_events[i].events);
	    pr[ix].events |= revents;
	    if (revents) {
		if (res == ix) {
		    ps->fds_status[fd].res_ev_ix = (unsigned short) ix;
		    res++;
		}
	    }
	}

#elif ERTS_POLL_USE_KQUEUE	/* --- kqueue ------------------------------ */

	struct kevent *ev;
	int fd;
	int ix;

	ev = &ps->res_events[i];
	fd = (int) ev->ident;
	ASSERT(fd < ps->fds_status_len);
	ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	ix = (int) ps->fds_status[fd].res_ev_ix;

	ASSERT(ix >= 0);
	if (ix >= res || pr[ix].fd != fd) {
	    ix = res;
	    pr[ix].fd = (int) ev->ident;
	    pr[ix].events = (ErtsPollEvents) 0;
	}

	if (ev->filter == EVFILT_READ) {
#if ERTS_POLL_USE_WAKEUP_PIPE
	    if (fd == wake_fd) {
		cleanup_wakeup_pipe(ps);
		continue;
	    }
#endif
	    pr[ix].events |= ERTS_POLL_EV_IN;
	}
	else if (ev->filter == EVFILT_WRITE)
	    pr[ix].events |= ERTS_POLL_EV_OUT;
	if (ev->flags & (EV_ERROR|EV_EOF)) {
	    if ((ev->flags & EV_ERROR) && (((int) ev->data) == EBADF))
		pr[ix].events |= ERTS_POLL_EV_NVAL;
	    else
		pr[ix].events |= ERTS_POLL_EV_ERR;
	}
	if (pr[ix].events) {
	    if (res == ix) {
		ps->fds_status[fd].res_ev_ix = (unsigned short) ix;
		res++;
	    }
	}

#elif ERTS_POLL_USE_DEVPOLL	/* --- devpoll ----------------------------- */

	if (ps->res_events[i].revents) {
	    int fd = ps->res_events[i].fd;
	    ErtsPollEvents revents;
#if ERTS_POLL_USE_WAKEUP_PIPE
	    if (fd == wake_fd) {
		cleanup_wakeup_pipe(ps);
		continue;
	    }
#endif
#if ERTS_POLL_USE_TIMERFD
	    if (fd == timer_fd) {
		continue;
	    }
#endif
	    revents = ERTS_POLL_EV_N2E(ps->res_events[i].events);
	    pr[res].fd = fd;
	    pr[res].events = revents;
	    res++;
	}

#endif

    }

    return res;
}

#endif /* ERTS_POLL_USE_KERNEL_POLL */

#if ERTS_POLL_USE_FALLBACK

static int
get_kp_results(ErtsPollSet ps, ErtsPollResFd pr[], int max_res)
{
    int res;
#if ERTS_POLL_USE_KQUEUE
    struct timespec ts = {0, 0};
#endif

    if (max_res > ps->res_events_len)
	grow_res_events(ps, max_res);

    do {
#if ERTS_POLL_USE_EPOLL
	res = epoll_wait(ps->kp_fd, ps->res_events, max_res, 0);
#elif ERTS_POLL_USE_KQUEUE
	res = kevent(ps->kp_fd, NULL, 0, ps->res_events, max_res, &ts);
#endif
    } while (res < 0 && errno == EINTR);

    if (res < 0) {
	fatal_error("%s:%d: %s() failed: %s (%d)\n",
		    __FILE__, __LINE__,
#if ERTS_POLL_USE_EPOLL
		    "epoll_wait",
#elif ERTS_POLL_USE_KQUEUE
		    "kevent",
#endif
		    erl_errno_id(errno), errno);
    }

    return save_kp_result(ps, pr, max_res, res);
}

#endif /* ERTS_POLL_USE_FALLBACK */



static ERTS_INLINE int
save_poll_result(ErtsPollSet ps, ErtsPollResFd pr[], int max_res,
		 int chk_fds_res, int ebadf)
{
#if ERTS_POLL_USE_DEVPOLL
    return save_kp_result(ps, pr, max_res, chk_fds_res);
#elif ERTS_POLL_USE_FALLBACK
    if (!ps->fallback_used)
	return save_kp_result(ps, pr, max_res, chk_fds_res);
    else
#endif /* ERTS_POLL_USE_FALLBACK */
    {

#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
	int res = 0;
#if ERTS_POLL_USE_WAKEUP_PIPE && !ERTS_POLL_USE_FALLBACK 
	int wake_fd = ps->wake_fds[0];
#endif
	int i, first_ix, end_ix;

	/*
	 * In order to be somewhat fair, we continue on the poll_fds
	 * index where we stopped last time.
	 */
	first_ix = i = ((ps->next_poll_fds_ix < ps->no_poll_fds)
			? ps->next_poll_fds_ix
			: 0);
	end_ix = ps->no_poll_fds;

	while (1) {
	    while (i < end_ix && res < max_res) {
		if (ps->poll_fds[i].revents != (short) 0) {
		    int fd = ps->poll_fds[i].fd;
		    ErtsPollEvents revents;
#if ERTS_POLL_USE_FALLBACK
		    if (fd == ps->kp_fd) {
			res += get_kp_results(ps, &pr[res], max_res-res);
			i++;
			continue;
		    }
#elif ERTS_POLL_USE_WAKEUP_PIPE
		    if (fd == wake_fd) {
			cleanup_wakeup_pipe(ps);
			i++;
			continue;
		    }
#endif
		    revents = pollev2ev(ps->poll_fds[i].revents);
		    pr[res].fd = fd;
		    pr[res].events = revents;
		    res++;
		}
		i++;
	    }
	    if (res == max_res || i == first_ix)
		break;
	    ASSERT(i == ps->no_poll_fds);
	    i = 0;
	    end_ix = first_ix;
	}

	ps->next_poll_fds_ix = i;
	return res;

#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
	int res = 0;
#if ERTS_POLL_USE_WAKEUP_PIPE && !ERTS_POLL_USE_FALLBACK 
	int wake_fd = ps->wake_fds[0];
#endif
	int fd, first_fd, end_fd;

	/*
	 * In order to be fair, we continue on the fd where we stopped
	 * last time.
	 */
	first_fd = fd = ps->next_sel_fd <= ps->max_fd ? ps->next_sel_fd : 0;
	end_fd = ps->max_fd + 1;

	if (!ebadf) {
	    while (1) {
		while (fd < end_fd && res < max_res) {

		    pr[res].events = (ErtsPollEvents) 0;
		    if (ERTS_FD_ISSET(fd, &ps->res_input_fds)) {
#if ERTS_POLL_USE_FALLBACK
			if (fd == ps->kp_fd) {
			    res += get_kp_results(ps, &pr[res], max_res-res);
			    fd++;
			    continue;
			}
#elif ERTS_POLL_USE_WAKEUP_PIPE
			if (fd == wake_fd) {
			    cleanup_wakeup_pipe(ps);
			    fd++;
			    continue;
			}
#endif
			pr[res].events |= ERTS_POLL_EV_IN;
		    }
		    if (ERTS_FD_ISSET(fd, &ps->res_output_fds))
			pr[res].events |= ERTS_POLL_EV_OUT;
		    if (pr[res].events) {
			pr[res].fd = fd;
			res++;
		    }
		    fd++;
		}
		if (res == max_res || fd == first_fd)
		    break;
		ASSERT(fd == ps->max_fd + 1);
		fd = 0;
		end_fd = first_fd;
	    }
	}
	else {
	    /*
	     * Bad file descriptors in poll set.
	     *
	     * This only happens when running poorly written
	     * drivers. This code could be optimized, but we
	     * don't bother since it should never happen...
	     */
	    while (1) {
		while (fd < end_fd && res < max_res) {
		    if (ps->fds_status[fd].events) {
			int sres;
			ERTS_fd_set *iset = NULL;
			ERTS_fd_set *oset = NULL;
			if (ps->fds_status[fd].events & ERTS_POLL_EV_IN) {
			    iset = &ps->res_input_fds;
			    ERTS_FD_ZERO(iset);
			    ERTS_FD_SET(fd, iset);
			}
			if (ps->fds_status[fd].events & ERTS_POLL_EV_OUT) {
			    oset = &ps->res_output_fds;
			    ERTS_FD_ZERO(oset);
			    ERTS_FD_SET(fd, oset);			
			}
			do {
			    /* Initiate 'tv' each time;
			       select() may modify it */
			    SysTimeval tv = {0, 0};
			    sres = ERTS_SELECT(ps->max_fd+1, iset, oset, NULL, &tv);
			} while (sres < 0 && errno == EINTR);
			if (sres < 0) {
#if ERTS_POLL_USE_FALLBACK
			    if (fd == ps->kp_fd) {
				res += get_kp_results(ps,
						      &pr[res],
						      max_res-res);
				fd++;
				continue;
			    }
#elif ERTS_POLL_USE_WAKEUP_PIPE
			    if (fd == wake_fd) {
				cleanup_wakeup_pipe(ps);
				fd++;
				continue;
			    }
#endif
			    pr[res].fd = fd;
			    pr[res].events = ERTS_POLL_EV_NVAL;
			    res++;
			}
			else if (sres > 0) {
			    pr[res].fd = fd;
			    if (iset && ERTS_FD_ISSET(fd, iset)) {
#if ERTS_POLL_USE_FALLBACK
				if (fd == ps->kp_fd) {
				    res += get_kp_results(ps,
							  &pr[res],
							  max_res-res);
				    fd++;
				    continue;
				}
#elif ERTS_POLL_USE_WAKEUP_PIPE
				if (fd == wake_fd) {
				    cleanup_wakeup_pipe(ps);
				    fd++;
				    continue;
				}
#endif
				pr[res].events |= ERTS_POLL_EV_IN;
			    }
			    if (oset && ERTS_FD_ISSET(fd, oset)) {
				pr[res].events |= ERTS_POLL_EV_OUT;
			    }
			    ASSERT(pr[res].events);
			    res++;
			}
		    }
		    fd++;
		}
		if (res == max_res || fd == first_fd)
		    break;
		ASSERT(fd == ps->max_fd + 1);
		fd = 0;
		end_fd = first_fd;
	    }
	}
	ps->next_sel_fd = fd;
	return res;
#endif
    }
}

static ERTS_INLINE ErtsMonotonicTime
get_timeout(ErtsPollSet ps,
	    int resolution,
	    ErtsMonotonicTime timeout_time)
{
    ErtsMonotonicTime timeout, save_timeout_time;

    if (timeout_time == ERTS_POLL_NO_TIMEOUT) {
	save_timeout_time = ERTS_MONOTONIC_TIME_MIN;
	timeout = 0;
    }
    else {
	ErtsMonotonicTime diff_time, current_time;
	current_time = erts_get_monotonic_time(NULL);
	diff_time = timeout_time - current_time;
	if (diff_time <= 0) {
	    save_timeout_time = ERTS_MONOTONIC_TIME_MIN;
	    timeout = 0;
	}
	else {
	    save_timeout_time = current_time;
	    switch (resolution) {
	    case 1000:
		/* Round up to nearest even milli second */
		timeout = ERTS_MONOTONIC_TO_MSEC(diff_time - 1) + 1;
		if (timeout > (ErtsMonotonicTime) INT_MAX)
		    timeout = (ErtsMonotonicTime) INT_MAX;
		save_timeout_time += ERTS_MSEC_TO_MONOTONIC(timeout);
		break;
	    case 1000000:
		/* Round up to nearest even micro second */
		timeout = ERTS_MONOTONIC_TO_USEC(diff_time - 1) + 1;
		save_timeout_time += ERTS_USEC_TO_MONOTONIC(timeout);
		break;
	    case 1000000000:
		/* Round up to nearest even nano second */
		timeout = ERTS_MONOTONIC_TO_NSEC(diff_time - 1) + 1;
		save_timeout_time += ERTS_NSEC_TO_MONOTONIC(timeout);
		break;
	    default:
		ERTS_INTERNAL_ERROR("Invalid resolution");
		timeout = 0;
		save_timeout_time = 0;
		break;
	    }
	}
    }
    set_timeout_time(ps, save_timeout_time);
    return timeout;
}

#if ERTS_POLL_USE_SELECT

static ERTS_INLINE int
get_timeout_timeval(ErtsPollSet ps,
		    SysTimeval *tvp,
		    ErtsMonotonicTime timeout_time)
{
    ErtsMonotonicTime timeout = get_timeout(ps,
					    1000*1000,
					    timeout_time);

    if (!timeout) {
	tvp->tv_sec = 0;
	tvp->tv_usec = 0;

	return 0;
    }
    else {
	ErtsMonotonicTime sec = timeout/(1000*1000);
	tvp->tv_sec = sec;
	tvp->tv_usec = timeout - sec*(1000*1000);

	ASSERT(tvp->tv_sec >= 0);
	ASSERT(tvp->tv_usec >= 0);
	ASSERT(tvp->tv_usec < 1000*1000);

	return !0;
    }

}

#endif

#if ERTS_POLL_USE_KQUEUE || (ERTS_POLL_USE_POLL && defined(HAVE_PPOLL)) || ERTS_POLL_USE_TIMERFD

static ERTS_INLINE int
get_timeout_timespec(ErtsPollSet ps,
		     struct timespec *tsp,
		     ErtsMonotonicTime timeout_time)
{
    ErtsMonotonicTime timeout = get_timeout(ps,
					    1000*1000*1000,
					    timeout_time);

    if (!timeout) {
	tsp->tv_sec = 0;
	tsp->tv_nsec = 0;
	return 0;
    }
    else {
	ErtsMonotonicTime sec = timeout/(1000*1000*1000);
	tsp->tv_sec = sec;
	tsp->tv_nsec = timeout - sec*(1000*1000*1000);

	ASSERT(tsp->tv_sec >= 0);
	ASSERT(tsp->tv_nsec >= 0);
	ASSERT(tsp->tv_nsec < 1000*1000*1000);

	return !0;
    }
}

#endif

#if ERTS_POLL_USE_TIMERFD

static ERTS_INLINE int
get_timeout_itimerspec(ErtsPollSet ps,
                       struct itimerspec *itsp,
                       ErtsMonotonicTime timeout_time)
{

    itsp->it_interval.tv_sec = 0;
    itsp->it_interval.tv_nsec = 0;

    return get_timeout_timespec(ps, &itsp->it_value, timeout_time);
}

#endif

static ERTS_INLINE int
check_fd_events(ErtsPollSet ps, ErtsMonotonicTime timeout_time, int max_res)
{
    int res;
    ERTS_MSACC_PUSH_STATE_M();
    if (erts_smp_atomic_read_nob(&ps->no_of_user_fds) == 0
	&& timeout_time == ERTS_POLL_NO_TIMEOUT) {
	/* Nothing to poll and zero timeout; done... */
	return 0;
    }
    else {
	int timeout;
#if ERTS_POLL_USE_FALLBACK
	if (!(ps->fallback_used = ERTS_POLL_NEED_FALLBACK(ps))) {

#if ERTS_POLL_USE_EPOLL		/* --- epoll ------------------------------- */
	    if (max_res > ps->res_events_len)
		grow_res_events(ps, max_res);
#if ERTS_POLL_USE_TIMERFD
            {
                struct itimerspec its;
                timeout = get_timeout_itimerspec(ps, &its, timeout_time);
                if (timeout) {
#ifdef ERTS_SMP
                    erts_thr_progress_prepare_wait(NULL);
#endif
                    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
                    timerfd_set(ps, &its);
                    res = epoll_wait(ps->kp_fd, ps->res_events, max_res, -1);
                    res = timerfd_clear(ps, res, max_res);
                } else {
                    res = epoll_wait(ps->kp_fd, ps->res_events, max_res, 0);
                }
            }
#else /* !ERTS_POLL_USE_TIMERFD */
	    timeout = (int) get_timeout(ps, 1000, timeout_time);
            if (timeout) {
#ifdef ERTS_SMP
		erts_thr_progress_prepare_wait(NULL);
#endif
                ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
            }
	    res = epoll_wait(ps->kp_fd, ps->res_events, max_res, timeout);
#endif /* !ERTS_POLL_USE_TIMERFD */
#elif ERTS_POLL_USE_KQUEUE	/* --- kqueue ------------------------------ */
	    struct timespec ts;
	    if (max_res > ps->res_events_len)
		grow_res_events(ps, max_res);
	    timeout = get_timeout_timespec(ps, &ts, timeout_time);
            if (timeout) {
#ifdef ERTS_SMP
		erts_thr_progress_prepare_wait(NULL);
#endif
		ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
            }
	    res = kevent(ps->kp_fd, NULL, 0, ps->res_events, max_res, &ts);
#endif				/* ----------------------------------------- */
	}
	else /* use fallback (i.e. poll() or select()) */
#endif /* ERTS_POLL_USE_FALLBACK */
	{

#if ERTS_POLL_USE_DEVPOLL	/* --- devpoll ----------------------------- */
	    /*
	     * The ioctl() will fail with EINVAL on Solaris 10 if dp_nfds
	     * is set too high. dp_nfds should not be set greater than
	     * the maximum number of file descriptors in the poll set.
	     */
	    struct dvpoll poll_res;
	    int nfds = (int) erts_smp_atomic_read_nob(&ps->no_of_user_fds);
#if ERTS_POLL_USE_WAKEUP_PIPE
	    nfds++; /* Wakeup pipe */
#endif
	    timeout = (int) get_timeout(ps, 1000, timeout_time);
	    poll_res.dp_nfds = nfds < max_res ? nfds : max_res;
	    if (poll_res.dp_nfds > ps->res_events_len)
		grow_res_events(ps, poll_res.dp_nfds);
	    poll_res.dp_fds = ps->res_events;
	    if (timeout) {
#ifdef ERTS_SMP
		erts_thr_progress_prepare_wait(NULL);
#endif
                ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
            }
	    poll_res.dp_timeout = timeout;
	    res = ioctl(ps->kp_fd, DP_POLL, &poll_res);
#elif ERTS_POLL_USE_POLL && defined(HAVE_PPOLL)	/* --- ppoll ---------------- */
            struct timespec ts;
	    timeout = get_timeout_timespec(ps, &ts, timeout_time);
            if (timeout) {
#ifdef ERTS_SMP
		erts_thr_progress_prepare_wait(NULL);
#endif
		ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
            }
            res = ppoll(ps->poll_fds, ps->no_poll_fds, &ts, NULL);
#elif ERTS_POLL_USE_POLL        /* --- poll --------------------------------- */
	    timeout = (int) get_timeout(ps, 1000, timeout_time);

	    if (timeout) {
#ifdef ERTS_SMP
		erts_thr_progress_prepare_wait(NULL);
#endif
		ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
            }
	    res = poll(ps->poll_fds, ps->no_poll_fds, timeout);
#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
	    SysTimeval to;
	    timeout = get_timeout_timeval(ps, &to, timeout_time);

	    ERTS_FD_COPY(&ps->input_fds, &ps->res_input_fds);
	    ERTS_FD_COPY(&ps->output_fds, &ps->res_output_fds);

	    if (timeout) {
#ifdef ERTS_SMP
		erts_thr_progress_prepare_wait(NULL);
#endif
		ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
	    }
	    res = ERTS_SELECT(ps->max_fd + 1,
			      &ps->res_input_fds,
			      &ps->res_output_fds,
			      NULL,
			      &to);
#ifdef ERTS_SMP
	    if (timeout) {
		erts_thr_progress_finalize_wait(NULL);
		ERTS_MSACC_POP_STATE_M();
	    }
	    if (res < 0
		&& errno == EBADF
		&& ERTS_POLLSET_HAVE_UPDATE_REQUESTS(ps)) {
		/*
		 * This may have happened because another thread deselected
		 * a fd in our poll set and then closed it, i.e. the driver
		 * behaved correctly. We wan't to avoid looking for a bad
		 * fd, that may even not exist anymore. Therefore, handle
		 * update requests and try again.
		 *
		 * We don't know how much of the timeout is left; therfore,
		 * we use a zero timeout. If no error occur and no events
		 * have triggered, we fake an EAGAIN error and let the caller
		 * restart us.
		 */
		to.tv_sec = 0;
		to.tv_usec = 0;
		ERTS_POLLSET_LOCK(ps);
		handle_update_requests(ps);
		ERTS_POLLSET_UNLOCK(ps);
		res = ERTS_SELECT(ps->max_fd + 1,
				  &ps->res_input_fds,
				  &ps->res_output_fds,
				  NULL,
				  &to);
		if (res == 0) {
		    errno = EAGAIN;
		    res = -1;
		}
	    }
#endif /* ERTS_SMP */
	    return res;
#endif				/* ----------------------------------------- */
	}
	if (timeout) {
#ifdef ERTS_SMP
	    erts_thr_progress_finalize_wait(NULL);
#endif
	    ERTS_MSACC_POP_STATE_M();
	}
	return res;
    }
}

int
ERTS_POLL_EXPORT(erts_poll_wait)(ErtsPollSet ps,
				 ErtsPollResFd pr[],
				 int *len,
				 ErtsMonotonicTime timeout_time)
{
    ErtsMonotonicTime to;
    int res, no_fds;
    int ebadf = 0;
#ifdef ERTS_SMP
    int ps_locked = 0;
#endif

    no_fds = *len;
#ifdef ERTS_POLL_MAX_RES
    if (no_fds >= ERTS_POLL_MAX_RES)
	no_fds = ERTS_POLL_MAX_RES;
#endif

    *len = 0;

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Entering erts_poll_wait(), timeout_time=%bps\n",
		timeout_time);
#endif

    if (ERTS_POLLSET_SET_POLLED_CHK(ps)) {
	res = EINVAL; /* Another thread is in erts_poll_wait()
			 on this pollset... */
	goto done;
    }

    to = (is_woken(ps)
	  ? ERTS_POLL_NO_TIMEOUT /* Use zero timeout */
	  : timeout_time);

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    if (ERTS_POLLSET_HAVE_UPDATE_REQUESTS(ps)) {
	ERTS_POLLSET_LOCK(ps);
	handle_update_requests(ps);
	ERTS_POLLSET_UNLOCK(ps);
    }
#endif

    res = check_fd_events(ps, to, no_fds);

    woke_up(ps);

    if (res == 0) {
	res = ETIMEDOUT;
    }
    else if (res < 0) {
#if ERTS_POLL_USE_SELECT
	if (errno == EBADF) {
	    ebadf = 1;
	    goto save_results;
	}
#endif
	res = errno;
    }
    else {
#if ERTS_POLL_USE_SELECT
    save_results:
#endif

#ifdef ERTS_SMP
	ps_locked = 1;
	ERTS_POLLSET_LOCK(ps);
#endif

	no_fds = save_poll_result(ps, pr, no_fds, res, ebadf);

#ifdef HARD_DEBUG
	check_poll_result(pr, no_fds);
#endif

	res = (no_fds == 0 ? (is_interrupted_reset(ps) ? EINTR : EAGAIN) : 0);
	*len = no_fds;
    }

#ifdef ERTS_SMP
    if (ps_locked)
	ERTS_POLLSET_UNLOCK(ps);
    ERTS_POLLSET_UNSET_POLLED(ps);
#endif

 done:
    set_timeout_time(ps, ERTS_MONOTONIC_TIME_MAX);
#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Leaving %s = erts_poll_wait()\n",
		 res == 0 ? "0" : erl_errno_id(res));
#endif

    return res;
}

/*
 * --- Interrupt a thread doing erts_poll_wait() -----------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_interrupt)(ErtsPollSet ps, int set)
{
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    if (!set)
	reset_wakeup_state(ps);
    else
	wake_poller(ps, 1, 0);
#endif
}

#if ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
void
ERTS_POLL_EXPORT(erts_poll_async_sig_interrupt)(ErtsPollSet ps)
{
    /*
     * NOTE: This function is called from signal handlers, it,
     *       therefore, it has to be async-signal safe.
     */
    wake_poller(ps, 1, 1);
}
#endif

/*
 * erts_poll_interrupt_timed():
 *   If 'set' != 0, interrupt thread blocked in erts_poll_wait() if it
 * is not guaranteed that it will timeout before 'msec' milli seconds.
 */
void
ERTS_POLL_EXPORT(erts_poll_interrupt_timed)(ErtsPollSet ps,
					    int set,
					    ErtsMonotonicTime timeout_time)
{
#if ERTS_POLL_ASYNC_INTERRUPT_SUPPORT || defined(ERTS_SMP)
    if (!set)
	reset_wakeup_state(ps);
    else {
	ErtsMonotonicTime max_wait_time = get_timeout_time(ps);
	if (max_wait_time > timeout_time)
	    wake_poller(ps, 1, 0);
#ifdef ERTS_POLL_COUNT_AVOIDED_WAKEUPS
	else {
	    if (ERTS_POLLSET_IS_POLLED(ps))
		erts_smp_atomic_inc_nob(&ps->no_avoided_wakeups);
	    erts_smp_atomic_inc_nob(&ps->no_avoided_interrupts);
	}
	erts_smp_atomic_inc_nob(&ps->no_interrupt_timed);
#endif
    }
#endif
}

int
ERTS_POLL_EXPORT(erts_poll_max_fds)(void)
{
    return max_fds;
}
/*
 * --- Initialization --------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_init)(void)
{
    erts_smp_spinlock_init(&pollsets_lock, "pollsets_lock");
    pollsets = NULL;

    errno = 0;

#if !defined(NO_SYSCONF)
    max_fds = sysconf(_SC_OPEN_MAX);
#elif ERTS_POLL_USE_SELECT
    max_fds = NOFILE;
#else
    max_fds = OPEN_MAX;
#endif

#if ERTS_POLL_USE_SELECT && defined(FD_SETSIZE) && \
	!defined(_DARWIN_UNLIMITED_SELECT)
    if (max_fds > FD_SETSIZE)
	max_fds = FD_SETSIZE;
#endif

    if (max_fds < 0)
	fatal_error("erts_poll_init(): Failed to get max number of files: %s\n",
		    erl_errno_id(errno));

#ifdef ERTS_POLL_DEBUG_PRINT
    print_misc_debug_info();
#endif
}

ErtsPollSet
ERTS_POLL_EXPORT(erts_poll_create_pollset)(void)
{
#if ERTS_POLL_USE_KERNEL_POLL
    int kp_fd;
#endif
    ErtsPollSet ps = erts_alloc(ERTS_ALC_T_POLLSET,
				sizeof(struct ErtsPollSet_));
    ps->internal_fd_limit = 0;
    ps->fds_status = NULL;
    ps->fds_status_len = 0;
    erts_smp_atomic_init_nob(&ps->no_of_user_fds, 0);
#if ERTS_POLL_USE_KERNEL_POLL
    ps->kp_fd = -1;
#if ERTS_POLL_USE_EPOLL
    kp_fd = epoll_create(256);
    ps->res_events_len = 0;
    ps->res_events = NULL;
#elif ERTS_POLL_USE_DEVPOLL
    kp_fd = open("/dev/poll", O_RDWR);
    ps->res_events_len = 0;
    ps->res_events = NULL;
#elif ERTS_POLL_USE_KQUEUE
    kp_fd = kqueue();
    ps->res_events_len = 0;
    ps->res_events = NULL;
#endif
    if (kp_fd < 0)
	fatal_error("erts_poll_create_pollset(): Failed to "
#if ERTS_POLL_USE_EPOLL
		    "create epoll set"
#elif ERTS_POLL_USE_DEVPOLL
		    "to open /dev/poll"
#elif ERTS_POLL_USE_KQUEUE
		    "create kqueue"
#endif
		    ": %s (%d)\n",
		    erl_errno_id(errno), errno);
#endif /* ERTS_POLL_USE_KERNEL_POLL */
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
    /* res_events is also used as write buffer */
    grow_res_events(ps, ERTS_POLL_MIN_BATCH_BUF_SIZE);
#endif
#if ERTS_POLL_USE_POLL
    ps->next_poll_fds_ix = 0;
    ps->no_poll_fds = 0;
    ps->poll_fds_len = 0;
    ps->poll_fds = NULL;
#elif ERTS_POLL_USE_SELECT
    ps->next_sel_fd = 0;
    ps->max_fd = -1;
#if ERTS_POLL_USE_FALLBACK
    ps->no_select_fds = 0;
#endif
#ifdef _DARWIN_UNLIMITED_SELECT
    ps->input_fds.sz = 0;
    ps->input_fds.ptr = NULL;
    ps->res_input_fds.sz = 0;
    ps->res_input_fds.ptr = NULL;
    ps->output_fds.sz = 0;
    ps->output_fds.ptr = NULL;
    ps->res_output_fds.sz = 0;
    ps->res_output_fds.ptr = NULL;
#else
    ERTS_FD_ZERO(&ps->input_fds);
    ERTS_FD_ZERO(&ps->res_input_fds);
    ERTS_FD_ZERO(&ps->output_fds);
    ERTS_FD_ZERO(&ps->res_output_fds);
#endif
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    ps->update_requests.next = NULL;
    ps->update_requests.len = 0;
    ps->curr_upd_req_block = &ps->update_requests;
    erts_smp_atomic32_init_nob(&ps->have_update_requests, 0);
#endif
#ifdef ERTS_SMP
    erts_atomic32_init_nob(&ps->polled, 0);
    erts_smp_mtx_init(&ps->mtx, "pollset");
#endif
#if defined(USE_THREADS) || ERTS_POLL_ASYNC_INTERRUPT_SUPPORT
    erts_atomic32_init_nob(&ps->wakeup_state, (erts_aint32_t) 0);
#endif
#if ERTS_POLL_USE_WAKEUP_PIPE
    create_wakeup_pipe(ps);
#endif
#if ERTS_POLL_USE_TIMERFD
    create_timerfd(ps);
#endif
#if ERTS_POLL_USE_FALLBACK
    if (kp_fd >= ps->fds_status_len)
	grow_fds_status(ps, kp_fd);
    /* Force kernel poll fd into fallback (poll/select) set */
    ps->fds_status[kp_fd].flags
	|= ERTS_POLL_FD_FLG_INFLBCK|ERTS_POLL_FD_FLG_USEFLBCK;
    {
	int do_wake = 0;
	ERTS_POLL_EXPORT(erts_poll_control)(ps, kp_fd, ERTS_POLL_EV_IN, 1,
					    &do_wake);
    }    
#endif
#if ERTS_POLL_USE_KERNEL_POLL
    if (ps->internal_fd_limit <= kp_fd)
	ps->internal_fd_limit = kp_fd + 1;
    ps->kp_fd = kp_fd;
#endif
    init_timeout_time(ps);
#ifdef ERTS_POLL_COUNT_AVOIDED_WAKEUPS
    erts_smp_atomic_init_nob(&ps->no_avoided_wakeups, 0);
    erts_smp_atomic_init_nob(&ps->no_avoided_interrupts, 0);
    erts_smp_atomic_init_nob(&ps->no_interrupt_timed, 0);
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    handle_update_requests(ps);
#endif
#if ERTS_POLL_USE_FALLBACK
    ps->fallback_used = 0;
#endif
    erts_smp_atomic_set_nob(&ps->no_of_user_fds, 0); /* Don't count wakeup pipe and fallback fd */

    erts_smp_spin_lock(&pollsets_lock);
    ps->next = pollsets;
    pollsets = ps;
    erts_smp_spin_unlock(&pollsets_lock);

    return ps;
}

void
ERTS_POLL_EXPORT(erts_poll_destroy_pollset)(ErtsPollSet ps)
{

    if (ps->fds_status)
	erts_free(ERTS_ALC_T_FD_STATUS, (void *) ps->fds_status);

#if ERTS_POLL_USE_EPOLL
    if (ps->kp_fd >= 0)
	close(ps->kp_fd);
    if (ps->res_events)
	erts_free(ERTS_ALC_T_POLL_RES_EVS, (void *) ps->res_events);
#elif ERTS_POLL_USE_DEVPOLL
    if (ps->kp_fd >= 0)
	close(ps->kp_fd);
    if (ps->res_events)
	erts_free(ERTS_ALC_T_POLL_RES_EVS, (void *) ps->res_events);
#elif ERTS_POLL_USE_POLL
    if (ps->poll_fds)
	erts_free(ERTS_ALC_T_POLL_FDS, (void *) ps->poll_fds);
#elif ERTS_POLL_USE_SELECT
#ifdef _DARWIN_UNLIMITED_SELECT
    if (ps->input_fds.ptr)
	erts_free(ERTS_ALC_T_SELECT_FDS, (void *) ps->input_fds.ptr);
    if (ps->res_input_fds.ptr)
	erts_free(ERTS_ALC_T_SELECT_FDS, (void *) ps->res_input_fds.ptr);
    if (ps->output_fds.ptr)
	erts_free(ERTS_ALC_T_SELECT_FDS, (void *) ps->output_fds.ptr);
    if (ps->res_output_fds.ptr)
	erts_free(ERTS_ALC_T_SELECT_FDS, (void *) ps->res_output_fds.ptr);
#endif
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    {
	ErtsPollSetUpdateRequestsBlock *urqbp = ps->update_requests.next;
	while (urqbp) {
	    ErtsPollSetUpdateRequestsBlock *free_urqbp = urqbp;
	    urqbp = urqbp->next;
	    free_update_requests_block(ps, free_urqbp);
	}
    }
#endif
#ifdef ERTS_SMP
    erts_smp_mtx_destroy(&ps->mtx);
#endif
#if ERTS_POLL_USE_WAKEUP_PIPE
    if (ps->wake_fds[0] >= 0)
	close(ps->wake_fds[0]);
    if (ps->wake_fds[1] >= 0)
	close(ps->wake_fds[1]);
#endif
#if ERTS_POLL_USE_TIMERFD
    if (ps->timer_fd >= 0)
        close(ps->timer_fd);
#endif

    erts_smp_spin_lock(&pollsets_lock);
    if (ps == pollsets)
	pollsets = pollsets->next;
    else {
	ErtsPollSet prev_ps;
	for (prev_ps = pollsets; ps != prev_ps->next; prev_ps = prev_ps->next)
            ;
	ASSERT(ps == prev_ps->next);
	prev_ps->next = ps->next;
    }
    erts_smp_spin_unlock(&pollsets_lock);

    erts_free(ERTS_ALC_T_POLLSET, (void *) ps);
}

/*
 * --- Info ------------------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_info)(ErtsPollSet ps, ErtsPollInfo *pip)
{
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    int pending_updates;
#endif
    Uint size = 0;

    ERTS_POLLSET_LOCK(ps);

    size += sizeof(struct ErtsPollSet_);
    size += ps->fds_status_len*sizeof(ErtsFdStatus);

#if ERTS_POLL_USE_EPOLL
    size += ps->res_events_len*sizeof(struct epoll_event);
#elif ERTS_POLL_USE_DEVPOLL
    size += ps->res_events_len*sizeof(struct pollfd);
#elif ERTS_POLL_USE_KQUEUE
    size += ps->res_events_len*sizeof(struct kevent);
#endif

#if ERTS_POLL_USE_POLL
    size += ps->poll_fds_len*sizeof(struct pollfd);
#elif ERTS_POLL_USE_SELECT
#ifdef _DARWIN_UNLIMITED_SELECT
    size += ps->input_fds.sz + ps->res_input_fds.sz
	 + ps->output_fds.sz + ps->res_output_fds.sz;
#endif
#endif

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    {
	ErtsPollSetUpdateRequestsBlock *urqbp = ps->update_requests.next;
	pending_updates = ps->update_requests.len;
	while (urqbp) {
	    size += sizeof(ErtsPollSetUpdateRequestsBlock);
	    pending_updates += urqbp->len;
	    urqbp = urqbp->next;
	}
    }
#endif

    pip->primary = 
#if ERTS_POLL_USE_KQUEUE
	"kqueue"
#elif ERTS_POLL_USE_EPOLL
	"epoll"
#elif ERTS_POLL_USE_DEVPOLL
	"/dev/poll"
#elif ERTS_POLL_USE_POLL
	"poll"
#elif ERTS_POLL_USE_SELECT
	"select"
#endif
	;

    pip->fallback = 
#if !ERTS_POLL_USE_FALLBACK
	NULL
#elif ERTS_POLL_USE_POLL
	"poll"
#elif ERTS_POLL_USE_SELECT
	"select"
#endif
	;

    pip->kernel_poll = 
#if !ERTS_POLL_USE_KERNEL_POLL
	NULL
#elif ERTS_POLL_USE_KQUEUE
	"kqueue"
#elif ERTS_POLL_USE_EPOLL
	"epoll"
#elif ERTS_POLL_USE_DEVPOLL
	"/dev/poll"
#endif
	;

    pip->memory_size = size;

    pip->poll_set_size = (int) erts_smp_atomic_read_nob(&ps->no_of_user_fds);
#if ERTS_POLL_USE_WAKEUP_PIPE
    pip->poll_set_size++; /* Wakeup pipe */
#endif
#if ERTS_POLL_USE_TIMERFD
    pip->poll_set_size++; /* timerfd */
#endif

    pip->fallback_poll_set_size =
#if !ERTS_POLL_USE_FALLBACK
	0
#elif ERTS_POLL_USE_POLL
	ps->no_poll_fds
#elif ERTS_POLL_USE_SELECT
	ps->no_select_fds
#endif
	;

#if ERTS_POLL_USE_FALLBACK
    /* If only kp_fd is in fallback poll set we don't use fallback... */
    if (pip->fallback_poll_set_size == 1)
	pip->fallback_poll_set_size = 0;
    else
	pip->poll_set_size++; /* kp_fd */
#endif

    pip->lazy_updates =
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
	1
#else
	0
#endif
	;

    pip->pending_updates = 
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
	pending_updates
#else
	0
#endif
	;

    pip->batch_updates = 
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
	1
#else
	0
#endif
	;

    pip->concurrent_updates =
#if ERTS_POLL_USE_CONCURRENT_UPDATE
	1
#else
	0
#endif
	;

    pip->max_fds = max_fds;

#ifdef ERTS_POLL_COUNT_AVOIDED_WAKEUPS
    pip->no_avoided_wakeups = erts_smp_atomic_read_nob(&ps->no_avoided_wakeups);
    pip->no_avoided_interrupts = erts_smp_atomic_read_nob(&ps->no_avoided_interrupts);
    pip->no_interrupt_timed = erts_smp_atomic_read_nob(&ps->no_interrupt_timed);
#endif

    ERTS_POLLSET_UNLOCK(ps);

}

/*
 * Fatal error...
 */

#ifndef ERTS_GOT_SIGUSR1
#  define ERTS_GOT_SIGUSR1 0
#endif

static void
fatal_error(char *format, ...)
{
    va_list ap;

    if (ERTS_SOMEONE_IS_CRASH_DUMPING || ERTS_GOT_SIGUSR1) {
	/*
	 * Crash dump writing and reception of sigusr1 (which will
	 * result in a crash dump) closes all file descriptors. This
	 * typically results in a fatal error for erts_poll() (wakeup
	 * pipes and kernel poll fds are closed).
	 *
	 * We ignore the error and let the crash dump writing continue...
	 */
	return;
    }
    va_start(ap, format);
    erts_vfprintf(stderr, format, ap);
    va_end(ap);
    abort();
}

static void
fatal_error_async_signal_safe(char *error_str)
{
    if (ERTS_SOMEONE_IS_CRASH_DUMPING || ERTS_GOT_SIGUSR1) {
	/* See comment above in fatal_error() */
	return;
    }
    if (error_str) {
	int len = 0;
	while (error_str[len])
	    len++;
	if (len) {
	    /* async signal safe */
	    erts_silence_warn_unused_result(write(2, error_str, len));
	}
    }
    abort();
}
 
/*
 * --- Debug -----------------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_get_selected_events)(ErtsPollSet ps,
						ErtsPollEvents ev[],
						int len)
{
    int fd;
    ERTS_POLLSET_LOCK(ps);
    for (fd = 0; fd < len; fd++) {
	if (fd >= ps->fds_status_len)
	    ev[fd] = 0;
	else {
	    ev[fd] = ps->fds_status[fd].events;
            if (
#if ERTS_POLL_USE_WAKEUP_PIPE
                fd == ps->wake_fds[0] || fd == ps->wake_fds[1] ||
#endif
#if ERTS_POLL_USE_TIMERFD
                fd == ps->timer_fd ||
#endif
#if ERTS_POLL_USE_KERNEL_POLL
                fd == ps->kp_fd ||
#endif
                0)
                ev[fd] |= ERTS_POLL_EV_NVAL;
	}
    }
    ERTS_POLLSET_UNLOCK(ps);

}

#ifdef HARD_DEBUG

static void
check_poll_result(ErtsPollResFd pr[], int len)
{
    int i, j;

    for (i = 0; i < len; i++) {
	ASSERT(pr[i].fd >= 0);
	ASSERT(pr[i].fd < max_fds);
	for (j = 0; j < len; j++) {
	    ASSERT(i == j || pr[i].fd != pr[j].fd);
	}
    }
}


#if ERTS_POLL_USE_DEVPOLL

static void
check_poll_status(ErtsPollSet ps)
{
    int i;
    for (i = 0; i < ps->fds_status_len; i++) {
	int ires;
	struct pollfd dp_fd;
	short events = ERTS_POLL_EV_E2N(ps->fds_status[i].events);
	
	dp_fd.fd = i;
	dp_fd.events = (short) 0;
	dp_fd.revents = (short) 0;

	ires = ioctl(ps->kp_fd, DP_ISPOLLED, &dp_fd);

	if (ires == 0) {
	    ASSERT(!events);
	}
	else if (ires == 1) {
	    ASSERT(events);
	    ASSERT(events == dp_fd.revents);
	}
	else {
	    ASSERT(0);
	}
	ASSERT(dp_fd.fd == i);
	ASSERT(ps->fds_status[i].events == ps->fds_status[i].used_events);
    }
}

#endif /* ERTS_POLL_USE_DEVPOLL */
#endif /* HARD_DEBUG */

#ifdef ERTS_POLL_DEBUG_PRINT
static void
print_misc_debug_info(void)
{
    erts_printf("erts_poll using: %s lazy_updates:%s batch_updates:%s\n",
#if ERTS_POLL_USE_KQUEUE
		"kqueue"
#elif ERTS_POLL_USE_EPOLL
		"epoll"
#elif ERTS_POLL_USE_DEVPOLL
		"/dev/poll"
#endif
#if ERTS_POLL_USE_FALLBACK
		"-"
#endif
#if ERTS_POLL_USE_POLL
		"poll"
#elif ERTS_POLL_USE_SELECT
		"select"
#endif
		,
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
		"true"
#else
		"false"
#endif
		,
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
		"true"
#else
		"false"
#endif
		);

    erts_printf("ERTS_POLL_EV_IN=0x%x\n"
		"ERTS_POLL_EV_OUT=0x%x\n"
		"ERTS_POLL_EV_NVAL=0x%x\n"
		"ERTS_POLL_EV_ERR=0x%x\n",
		ERTS_POLL_EV_IN,
		ERTS_POLL_EV_OUT,
		ERTS_POLL_EV_NVAL,
		ERTS_POLL_EV_ERR);

#ifdef FD_SETSIZE
    erts_printf("FD_SETSIZE=%d\n", FD_SETSIZE);
#endif
}
    
#endif
