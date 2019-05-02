/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2018. All Rights Reserved.
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

/**
 * @description Poll interface suitable for ERTS
 *
 *		The interface is currently implemented using:
 *		- select
 *		- poll
 *              - /dev/poll
 *              - epoll with poll or select as fallback
 *              - kqueue with poll or select as fallback
 *
 *
 * @author Rickard Green
 * @author Lukas Larsson
 *
 * There are two major different implementations off IO polling in this
 * file. The concurrent and non-concurrent implementations.
 * When available epoll/kqueue are used to implement the concurrent
 * versions. poll, select and dev/poll use non-concurrent updates.
 *
 * Concurrent version:
 *   In the concurrent version erts_poll_control directly modifies
 *   the kernel pollset without waking the thread that is waiting
 *   on events. Also the ErtsPollResFd type is directly mapped to
 *   the native event type, so no extra copying is needed. Note that
 *   as no locking at all is done, fds can be triggered that have been
 *   removed from the pollset. The check_io layer has to deal with this.
 *
 * Non-concurrent version:
 *   In the non-concurrent version, the pollset has an internal representation
 *   of the pollset that is updated by erts_poll_control. When an fd is updated,
 *   its number is placed in the update request queue and then the waiting thread
 *   is woken in order to see the change. The internal data in the pollset is
 *   protected by a mutex that has to be taken by both the modifying and waiting
 *   thread at different times.
 *
 *   The non-concurrent pollset cannot have fd's closed in it while a thread is
 *   waiting on that fd. In order to fix this, when an ERTS_POLL_OP_DEL command
 *   is issued, the fd is marked as closing and the waiting thread is woken. The
 *   fd is then returned in the waiting threads results as ERTS_POLL_EV_NONE.
 *
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

#include "erl_thr_progress.h"
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
#elif defined(_DARWIN_UNLIMITED_SELECT)
#  undef _DARWIN_UNLIMITED_SELECT
#endif
#ifdef NO_SYSCONF
#  if ERTS_POLL_USE_SELECT
#    include <sys/param.h>
#  else
#    include <limits.h>
#  endif
#endif
#include "erl_driver.h"
#include "erl_alloc.h"
#include "erl_msacc.h"
#include "erl_misc_utils.h"

#if !defined(ERTS_POLL_USE_EPOLL) \
    && !defined(ERTS_POLL_USE_DEVPOLL)  \
    && !defined(ERTS_POLL_USE_POLL) \
    && !defined(ERTS_POLL_USE_SELECT)
#error "Missing implementation of erts_poll()"
#endif

#if 0
#define ERTS_POLL_DEBUG_PRINT 1

#define DEBUG_PRINT(FMT, PS, ...)                                       \
    do {                                                                \
        int myerrno = errno;                                            \
        erts_printf("%d: " FMT "\r\n", (PS)->id, ##__VA_ARGS__);        \
        errno = myerrno;                                                \
    } while(0)

/* Define to print info about modifications done to each fd */
#define DEBUG_PRINT_FD(FMT, PS, FD, ...) DEBUG_PRINT("%d: " FMT, PS, FD, ##__VA_ARGS__)
/* Define to print entry and exit from erts_poll_wait (can be very spammy) */
// #define DEBUG_PRINT_WAIT(FMT, PS, ...) DEBUG_PRINT(FMT, PS, ##__VA_ARGS__)
// #define DEBUG_PRINT_WAIT(FMT, PS, ...) do { if ((PS)->id != -1) DEBUG_PRINT(FMT, PS, ##__VA_ARGS__); } while(0)

#else
#define ERTS_POLL_DEBUG_PRINT 0
#define DEBUG_PRINT(...)
#endif

#ifndef DEBUG_PRINT_FD
#define DEBUG_PRINT_FD(...)
#endif
#ifndef DEBUG_PRINT_WAIT
#define DEBUG_PRINT_WAIT(...)
#endif


#if defined(_DARWIN_UNLIMITED_SELECT) && ERTS_POLL_USE_SELECT
typedef struct {
    size_t sz;
    fd_set* ptr;
}ERTS_fd_set;

#  define ERTS_FD_ZERO(fds)	memset((fds)->ptr, 0, (fds)->sz)
#  define ERTS_FD_SIZE(n)	((((n)+NFDBITS-1)/NFDBITS)*sizeof(fd_mask))

static ERTS_INLINE void ERTS_FD_CLR(int fd, ERTS_fd_set *fds)
{
    ASSERT(ERTS_FD_SIZE(fd+1) <= fds->sz);
    FD_CLR(fd, fds->ptr);
}
static ERTS_INLINE void ERTS_FD_SET(int fd, ERTS_fd_set *fds)
{
    ASSERT(ERTS_FD_SIZE(fd+1) <= fds->sz);
    FD_SET(fd, fds->ptr);
}
static ERTS_INLINE int ERTS_FD_ISSET(int fd, ERTS_fd_set *fds)
{
    ASSERT(ERTS_FD_SIZE(fd+1) <= fds->sz);
    return FD_ISSET(fd, fds->ptr);
}

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

#define ERTS_POLL_IS_FALLBACK (ERTS_POLL_USE_POLL || ERTS_POLL_USE_SELECT) && ERTS_ENABLE_KERNEL_POLL

#define ERTS_POLL_USE_CONCURRENT_UPDATE (ERTS_POLL_USE_EPOLL || ERTS_POLL_USE_KQUEUE)

#define ERTS_POLL_USE_WAKEUP(ps) (!ERTS_POLL_USE_CONCURRENT_UPDATE || (ps)->id < 0)

#if !ERTS_POLL_USE_CONCURRENT_UPDATE

#define ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(PS) \
  erts_atomic32_set_nob(&(PS)->have_update_requests, (erts_aint32_t) 1)
#define ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(PS) \
  erts_atomic32_set_nob(&(PS)->have_update_requests, (erts_aint32_t) 0)
#define ERTS_POLLSET_HAVE_UPDATE_REQUESTS(PS) \
  ((int) erts_atomic32_read_nob(&(PS)->have_update_requests))

#define ERTS_POLLSET_LOCK(PS) \
  erts_mtx_lock(&(PS)->mtx)
#define ERTS_POLLSET_UNLOCK(PS) \
  erts_mtx_unlock(&(PS)->mtx)

#else

#define ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(PS)
#define ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(PS)
#define ERTS_POLLSET_HAVE_UPDATE_REQUESTS(PS) 0

#define ERTS_POLLSET_LOCK(PS)
#define ERTS_POLLSET_UNLOCK(PS)

#endif

/*
 * --- Data types ------------------------------------------------------------
 */

#if !ERTS_POLL_USE_CONCURRENT_UPDATE

#define ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE 128

typedef struct ErtsPollSetUpdateRequestsBlock_ ErtsPollSetUpdateRequestsBlock;
struct ErtsPollSetUpdateRequestsBlock_ {
    ErtsPollSetUpdateRequestsBlock *next;
    int len;
    int fds[ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE];
};

#  define ERTS_POLL_FD_FLG_INURQ	(((unsigned short) 1) << 0)
#  define ERTS_POLL_FD_FLG_RST		(((unsigned short) 1) << 1)

typedef struct {
#if ERTS_POLL_USE_POLL
    int pix;
#endif

    ErtsPollEvents used_events;
    ErtsPollEvents events;
    unsigned short flags;

} ErtsFdStatus;

#endif

/*
 * This struct is not really exported, but it's nice to
 * get unique names in debugger for kp/nkp
 */
struct ERTS_POLL_EXPORT(erts_pollset) {
    int id;
    int internal_fd_limit;
    erts_atomic_t no_of_user_fds;

#if ERTS_POLL_USE_KERNEL_POLL
    int kp_fd;
    int oneshot;
#endif /* ERTS_POLL_USE_KERNEL_POLL */

#if ERTS_POLL_USE_POLL
    int next_poll_fds_ix;
    int no_poll_fds;
    int poll_fds_len;
    struct pollfd *poll_fds;
#elif ERTS_POLL_USE_SELECT
    int next_sel_fd;
    int max_fd;
    ERTS_fd_set input_fds;
    ERTS_fd_set res_input_fds;
    ERTS_fd_set output_fds;
    ERTS_fd_set res_output_fds;
#elif ERTS_POLL_USE_DEVPOLL
    struct pollfd *poll_fds;
    int poll_fds_ix;
#endif

#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    ErtsFdStatus *fds_status;
    int fds_status_len;
    ErtsPollSetUpdateRequestsBlock update_requests;
    ErtsPollSetUpdateRequestsBlock *curr_upd_req_block;
    erts_atomic32_t have_update_requests;
    erts_mtx_t mtx;
#else
    int do_wakeup;
#endif

#if ERTS_POLL_USE_TIMERFD
    int timer_fd;
#endif
    ErtsMonotonicTime timeout_time;
    erts_atomic32_t wakeup_state;
    int wake_fds[2];
};

void erts_silence_warn_unused_result(long unused);
static void fatal_error(char *format, ...);

static int max_fds = -1;

#if ERTS_POLL_USE_POLL

#if !ERTS_POLL_IS_FALLBACK
static ERTS_INLINE short ev2pollev(ErtsPollEvents ev)
{
    return ERTS_POLL_EV_E2N(ev);
}

static ERTS_INLINE ErtsPollEvents pollev2ev(short ev)
{
    return ERTS_POLL_EV_N2E(ev);
}

#else /* ERTS_POLL_IS_FALLBACK */

static ERTS_INLINE short
ev2pollev(ErtsPollEvents ev)
{
    short res_ev = (short) 0;
    if (ev & ERTS_POLL_EV_IN)
       res_ev |= ERTS_POLL_EV_NKP_IN;
    if (ev & ERTS_POLL_EV_OUT)
       res_ev |= ERTS_POLL_EV_NKP_OUT;
    return res_ev;
}

static ERTS_INLINE ErtsPollEvents
pollev2ev(short ev)
{
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
}

#endif /* !ERTS_POLL_IS_FALLBACK */

#endif /* ERTS_POLL_USE_POLL */


#ifdef HARD_DEBUG
static void check_poll_result(ErtsPollResFd pr[], int len);
#endif /* HARD_DEBUG */
#if ERTS_POLL_USE_DEVPOLL && defined(DEBUG)
static void check_poll_status(ErtsPollSet *ps);
#endif /* ERTS_POLL_USE_DEVPOLL && DEBUG */
static void print_misc_debug_info(void);
#if ERTS_POLL_USE_EPOLL
uint32_t epoll_events(int kp_fd, int fd);
#endif

#define ERTS_POLL_NOT_WOKEN	0
#define ERTS_POLL_WOKEN		-1
#define ERTS_POLL_WOKEN_INTR	1

static ERTS_INLINE void
reset_wakeup_state(ErtsPollSet *ps)
{
    erts_atomic32_set_mb(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN);
}

static ERTS_INLINE int
is_woken(ErtsPollSet *ps)
{
    return erts_atomic32_read_acqb(&ps->wakeup_state) != ERTS_POLL_NOT_WOKEN;
}

static ERTS_INLINE int
is_interrupted_reset(ErtsPollSet *ps)
{
    return (erts_atomic32_xchg_acqb(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN)
	    == ERTS_POLL_WOKEN_INTR);
}

static ERTS_INLINE void
woke_up(ErtsPollSet *ps)
{
    erts_aint32_t wakeup_state = erts_atomic32_read_acqb(&ps->wakeup_state);
    if (wakeup_state == ERTS_POLL_NOT_WOKEN)
	(void) erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
					 ERTS_POLL_WOKEN,
					 ERTS_POLL_NOT_WOKEN);
    ASSERT(erts_atomic32_read_nob(&ps->wakeup_state) != ERTS_POLL_NOT_WOKEN);
}

/*
 * --- Wakeup pipe -----------------------------------------------------------
 */

static ERTS_INLINE void
wake_poller(ErtsPollSet *ps, int interrupted)
{
    int wake;
    erts_aint32_t wakeup_state;
    if (!interrupted)
        wakeup_state = erts_atomic32_cmpxchg_relb(&ps->wakeup_state,
                                                  ERTS_POLL_WOKEN,
                                                  ERTS_POLL_NOT_WOKEN);
    else
        wakeup_state = erts_atomic32_xchg_relb(&ps->wakeup_state,
                                               ERTS_POLL_WOKEN_INTR);
    wake = wakeup_state == ERTS_POLL_NOT_WOKEN;

    if (wake)
    {
	ssize_t res;
        DEBUG_PRINT_WAIT("wake_poller(%d)", ps, interrupted);
	if (ps->wake_fds[1] < 0)
	    return; /* Not initialized yet */
	do {
	    /* write() is async-signal safe (according to posix) */
	    res = write(ps->wake_fds[1], "!", 1);
	} while (res < 0 && errno == EINTR);
	if (res <= 0 && errno != ERRNO_BLOCK) {
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
cleanup_wakeup_pipe(ErtsPollSet *ps)
{
    int intr = 0;
    int fd = ps->wake_fds[0];
    int res;
    do {
	char buf[32];
	res = read(fd, buf, sizeof(buf));
	if (res > 0)
	    intr = 1;
    } while (res > 0 || (res < 0 && errno == EINTR));
    if (res < 0 && errno != ERRNO_BLOCK) {
	fatal_error("%s:%d:cleanup_wakeup_pipe(): "
		    "Failed to read on wakeup pipe fd=%d: "
		    "%s (%d)\n",
		    __FILE__, __LINE__,
		    fd,
		    erl_errno_id(errno), errno);
    }
    if (intr)
	erts_atomic32_set_nob(&ps->wakeup_state, ERTS_POLL_WOKEN_INTR);
}

static void
create_wakeup_pipe(ErtsPollSet *ps)
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

    DEBUG_PRINT("wakeup fds = {%d, %d}", ps, wake_fds[0], wake_fds[1]);

    ERTS_POLL_EXPORT(erts_poll_control)(ps,
					wake_fds[0],
                                        ERTS_POLL_OP_ADD,
					ERTS_POLL_EV_IN,
                                        &do_wake);
    if (ps->internal_fd_limit <= wake_fds[1])
	ps->internal_fd_limit = wake_fds[1] + 1;
    if (ps->internal_fd_limit <= wake_fds[0])
	ps->internal_fd_limit = wake_fds[0] + 1;
    ps->wake_fds[0] = wake_fds[0];
    ps->wake_fds[1] = wake_fds[1];
}

/*
 * --- timer fd -----------------------------------------------------------
 */

#if ERTS_POLL_USE_TIMERFD

/* We use the timerfd when using epoll_wait to get high accuracy
   timeouts, i.e. we want to sleep with < ms accuracy. */

static void
create_timerfd(ErtsPollSet *ps)
{
    int do_wake = 0;
    int timer_fd = timerfd_create(CLOCK_MONOTONIC,0);
    ERTS_POLL_EXPORT(erts_poll_control)(ps,
					timer_fd,
                                        ERTS_POLL_OP_ADD,
					ERTS_POLL_EV_IN,
                                        &do_wake);
    if (ps->internal_fd_limit <= timer_fd)
	ps->internal_fd_limit = timer_fd + 1;
    ps->timer_fd = timer_fd;
}

static ERTS_INLINE void
timerfd_set(ErtsPollSet *ps, struct itimerspec *its)
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
timerfd_clear(ErtsPollSet *ps, ErtsPollResFd pr[], int res, int max_res) {

    struct itimerspec its;
    /* we always have to clear the timer */
    its.it_interval.tv_sec = 0;
    its.it_interval.tv_nsec = 0;
    its.it_value.tv_sec = 0;
    its.it_value.tv_nsec = 0;
    timerfd_settime(ps->timer_fd, 0, &its, NULL);

    /* only timeout fd triggered */
    if (res == 1 && pr[0].data.fd == ps->timer_fd)
        return 0;

    return res;
}

#endif /* ERTS_POLL_USE_TIMERFD */

/*
 * --- Poll set update requests ----------------------------------------------
 */

#if !ERTS_POLL_USE_CONCURRENT_UPDATE

static ERTS_INLINE void
enqueue_update_request(ErtsPollSet *ps, int fd)
{
    ErtsPollSetUpdateRequestsBlock *urqbp;

    ASSERT(fd < ps->fds_status_len);

    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INURQ)
	return;

    if (ps->update_requests.len == 0)
	ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(ps);

    urqbp = ps->curr_upd_req_block;

    if (urqbp->len == ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE) {
	urqbp = erts_alloc(ERTS_ALC_T_POLLSET_UPDREQ,
			   sizeof(ErtsPollSetUpdateRequestsBlock));
	urqbp->next = ps->curr_upd_req_block;
	urqbp->len = 0;
	ps->curr_upd_req_block = urqbp;
    }

    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INURQ;
    urqbp->fds[urqbp->len++] = fd;
}

static ERTS_INLINE void
free_update_requests_block(ErtsPollSet *ps,
			   ErtsPollSetUpdateRequestsBlock *urqbp)
{
    if (urqbp != &ps->update_requests)
	erts_free(ERTS_ALC_T_POLLSET_UPDREQ, (void *) urqbp);
    else {
	urqbp->len = 0;
    }
}

#endif /* !ERTS_POLL_USE_CONCURRENT_UPDATE */

/*
 * --- Growing poll set structures -------------------------------------------
 */

#if !ERTS_NO_KERNEL_POLL_VERSION || !ERTS_ENABLE_KERNEL_POLL
/* only one shared implementation */

#define ERTS_FD_TABLE_MIN_LENGTH	1024
#define ERTS_FD_TABLE_EXP_THRESHOLD	(2048*1024)

int erts_poll_new_table_len(int old_len, int need_len)
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

#if ERTS_POLL_USE_POLL
static void
grow_poll_fds(ErtsPollSet *ps, int min_ix)
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
static ERTS_INLINE int
check_select_fds(int fd, ERTS_fd_set* in, ERTS_fd_set* out)
{
    ASSERT(in->sz == out->sz);
    return (ERTS_FD_SIZE(fd+1) <= in->sz);
}
#else
#  define ensure_select_fds(fd, in, out) do {} while(0)
#  define check_select_fds(fd, in, out) (1)
#endif /* _DARWIN_UNLIMITED_SELECT */

#if !ERTS_POLL_USE_CONCURRENT_UPDATE
static void
grow_fds_status(ErtsPollSet *ps, int min_fd)
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
	ps->fds_status[i].flags = (unsigned short) 0;
    }
    ps->fds_status_len = new_len;
}
#endif

/*
 * --- Selecting fd to poll on -----------------------------------------------
 */

#if ERTS_POLL_USE_EPOLL
static int
update_pollset(ErtsPollSet *ps, int fd, ErtsPollOp op, ErtsPollEvents events)
{
    int res;
    int epoll_op = EPOLL_CTL_MOD;
    struct epoll_event epe_templ;
    struct epoll_event epe;

    epe_templ.events = ERTS_POLL_EV_E2N(events);
    epe_templ.data.fd = fd;

    if (ps->oneshot)
        epe_templ.events |= EPOLLONESHOT;

#ifdef VALGRIND
    /* Silence invalid valgrind warning ... */
    memset((void *) &epe.data, 0, sizeof(epoll_data_t));
#endif

    switch (op) {
    case ERTS_POLL_OP_DEL:
	/* A note on EPOLL_CTL_DEL: linux kernel versions before 2.6.9
	   need a non-NULL event pointer even though it is ignored... */
        epoll_op = EPOLL_CTL_DEL;
        epe_templ.events = 0;
	erts_atomic_dec_nob(&ps->no_of_user_fds);
        break;
    case ERTS_POLL_OP_ADD:
        epoll_op = EPOLL_CTL_ADD;
	erts_atomic_inc_nob(&ps->no_of_user_fds);
        break;
    case ERTS_POLL_OP_MOD:
        epoll_op = EPOLL_CTL_MOD;
        break;
    default:
        ASSERT(0);
        break;
    }

    do {
	/* We init 'epe' every time since epoll_ctl() may modify it
	   (not declared const and not documented as const). */
	epe.events = epe_templ.events;
	epe.data.fd = epe_templ.data.fd;
	res = epoll_ctl(ps->kp_fd, epoll_op, fd, &epe);
    } while (res != 0 && errno == EINTR);

#if ERTS_POLL_DEBUG_PRINT
    {
	int saved_errno = errno;
	DEBUG_PRINT_FD("%s = epoll_ctl(%d, %s, %d, {0x%x, %d})",
                       ps, fd,
                       res == 0 ? "0" : erl_errno_id(errno),
                       ps->kp_fd,
                       (epoll_op == EPOLL_CTL_ADD
                        ? "EPOLL_CTL_ADD"
                        : (epoll_op == EPOLL_CTL_MOD
                           ? "EPOLL_CTL_MOD"
                           : (epoll_op == EPOLL_CTL_DEL
                              ? "EPOLL_CTL_DEL"
                              : "UNKNOWN"))),
                       fd,
                       epe_templ.events,
                       fd);
	errno = saved_errno;
    }
#endif
    if (res != 0) {
	switch (op) {
	case ERTS_POLL_OP_MOD:
	    epe.events = 0;
	    do {
		/* We init 'epe' every time since epoll_ctl() may modify it
		   (not declared const and not documented as const). */
		epe.events = 0;
		epe.data.fd = fd;
		res = epoll_ctl(ps->kp_fd, EPOLL_CTL_DEL, fd, &epe);
	    } while (res != 0 && errno == EINTR);
	/* Fall through ... */
	case ERTS_POLL_OP_ADD: {
	    erts_atomic_dec_nob(&ps->no_of_user_fds);
            res = ERTS_POLL_EV_NVAL;
	    break;
	}
	case ERTS_POLL_OP_DEL: {
	    /*
	     * Since we use a lazy update approach EPOLL_CTL_DEL will
	     * frequently fail. This since epoll automatically removes
	     * a filedescriptor that is closed from the poll set.
	     */
	    res = 0;
	    break;
	}
	default:
	    fatal_error("%s:%d:update_pollset(): Internal error\n",
			__FILE__, __LINE__);
	    break;
	}
    } else {
        res = events;
    }
    return res;
}

#endif /* ERTS_POLL_USE_EPOLL */

#if ERTS_POLL_USE_KQUEUE

/* Some versions of the EV_SET macro used kevp multiple times,
   so we define out own version that make sure that it is safe
   to do kevp++ in the argument list. */
#define ERTS_EV_SET(kevp, a, b, c, f) do {              \
        struct kevent *kevp_ = kevp;                    \
        EV_SET(kevp_, a, b, c, 0, 0, f);                \
    } while(0)

static int
update_pollset(ErtsPollSet *ps, int fd, ErtsPollOp op, ErtsPollEvents events)
{
    int res = 0, len = 0;
    struct kevent evts[2];
    struct timespec ts = {0, 0};
    uint32_t oneshot = 0;

    if (op == ERTS_POLL_OP_ADD) {
        /* This is a hack to make the "noshell" option work; kqueue can poll
         * these fds but will not report EV_EOF, so we return NVAL to use the
         * fallback instead.
         *
         * This may be common to all pipes but we have no way to tell whether
         * an fd is a pipe or not. */
        switch (fd) {
        case STDIN_FILENO:
        case STDOUT_FILENO:
        case STDERR_FILENO:
            return ERTS_POLL_EV_NVAL;
        default:
            break;
        }
    }

#if defined(EV_DISPATCH) && !(defined(__OpenBSD__) || defined(__NetBSD__))
    /* If we have EV_DISPATCH we use it, unless we are on OpenBSD/NetBSD as the
       behavior of EV_EOF seems to be edge triggered there and we need it
       to be level triggered.

       The kevent descriptions for both read and write are added on OP_ADD
       and removed on OP_DEL. And then after than only EV_ENABLE|EV_DISPATCH
       are used.

       It could be possible to not modify the pollset when disabling and/or
       deleting events, but that may cause the poll threads to be awoken
       a lot more than they should so we take the cost here instead of
       in the poll thread.

       Note: We need to have EV_DISPATCH both when the event is enabled and
       disabled, as otherwise the event may be triggered twice on each re-arm.
       Not sure if this is intended or not (can't find anything about it in the
       man page), but it seems to be the way it works...
    */

    if (ps->oneshot)
        oneshot = EV_DISPATCH;

    if (op == ERTS_POLL_OP_DEL) {
        erts_atomic_dec_nob(&ps->no_of_user_fds);
        /* We could probably skip this delete, do we want to? */
        ERTS_EV_SET(&evts[len++], fd, EVFILT_READ, EV_DELETE, (void *) 0);
        ERTS_EV_SET(&evts[len++], fd, EVFILT_WRITE, EV_DELETE, (void *) 0);
    } else if (op == ERTS_POLL_OP_ADD) {
        uint32_t flags;
        erts_atomic_inc_nob(&ps->no_of_user_fds);

        flags = EV_ADD|oneshot;
        flags |= ((events & ERTS_POLL_EV_IN) ? 0 : EV_DISABLE);
        ERTS_EV_SET(&evts[len++], fd, EVFILT_READ, flags, (void *) ERTS_POLL_EV_IN);

        flags = EV_ADD|oneshot;
        flags |= ((events & ERTS_POLL_EV_OUT) ? 0 : EV_DISABLE);
        ERTS_EV_SET(&evts[len++], fd, EVFILT_WRITE, flags, (void *) ERTS_POLL_EV_OUT);
    } else {
        uint32_t flags;
        ASSERT(op == ERTS_POLL_OP_MOD);

        flags = oneshot;
        flags |= (events & ERTS_POLL_EV_IN) ? EV_ENABLE : EV_DISABLE;
        ERTS_EV_SET(&evts[len++], fd, EVFILT_READ, flags, (void *) ERTS_POLL_EV_IN);

        flags = oneshot;
        flags |= (events & ERTS_POLL_EV_OUT) ? EV_ENABLE : EV_DISABLE;
        ERTS_EV_SET(&evts[len++], fd, EVFILT_WRITE, flags, (void *) ERTS_POLL_EV_OUT);
    }
#else
    uint32_t flags = EV_ADD|EV_ENABLE;

    if (ps->oneshot) flags |= EV_ONESHOT;

    if (op == ERTS_POLL_OP_DEL) {
        erts_atomic_dec_nob(&ps->no_of_user_fds);
        /* We don't do anything when a delete is issued. The fds will be removed
           when they are triggered, or when they are closed. */
        if (ps->oneshot)
            events = 0;
        else {
            flags = EV_DELETE;
            events = ERTS_POLL_EV_IN;
        }
    } else if (op == ERTS_POLL_OP_ADD) {
        erts_atomic_inc_nob(&ps->no_of_user_fds);
        /* Only allow EV_IN in non-oneshot poll-sets */
        ASSERT(ps->oneshot || events == ERTS_POLL_EV_IN);
    } else if (!ps->oneshot) {
        ASSERT(op == ERTS_POLL_OP_MOD);
        /* If we are not oneshot and do a mod we should disable the FD.
           We assume that it is only the read side that is active as
           currently only read is selected upon in the non-oneshot
           poll-sets. */
        if (!events)
            flags = EV_DISABLE;
        else
            flags = EV_ENABLE;
        events = ERTS_POLL_EV_IN;
    }

    if (events & ERTS_POLL_EV_IN) {
        ERTS_EV_SET(&evts[len++], fd, EVFILT_READ, flags, (void *) ERTS_POLL_EV_IN);
    }
    if (events & ERTS_POLL_EV_OUT) {
        ERTS_EV_SET(&evts[len++], fd, EVFILT_WRITE, flags, (void *) ERTS_POLL_EV_OUT);
    }

#endif
    if (len)
        do {
            res = kevent(ps->kp_fd, evts, len, NULL, 0, &ts);
        } while (res < 0 && errno == EINTR);
#if ERTS_POLL_DEBUG_PRINT
    {
        int saved_errno = errno, i;
        char keventb[255], *keventbp = keventb;
        if (res < 0)
            keventbp += sprintf(keventbp,"%s = ",erl_errno_id(saved_errno));
        else
            keventbp += sprintf(keventbp,"%d = ",res);
        keventbp += sprintf(keventbp, "kevent(%d, {",ps->kp_fd);
        for (i = 0; i < len; i++) {
            const char *flags = "UNKNOWN";
            if (evts[i].flags == (EV_DELETE)) flags = "EV_DELETE";
            if (evts[i].flags == (EV_ADD)) flags = "EV_ADD";
            if (evts[i].flags == (EV_ADD|EV_ONESHOT)) flags = "EV_ADD|EV_ONESHOT";
            if (evts[i].flags == (EV_ENABLE)) flags = "EV_ENABLE";
            if (evts[i].flags == (EV_DISABLE)) flags = "EV_DISABLE";
            if (evts[i].flags == (EV_ADD|EV_DISABLE)) flags = "EV_ADD|EV_DISABLE";
#ifdef EV_DISPATCH
            if (evts[i].flags == (EV_ADD|EV_DISPATCH)) flags = "EV_ADD|EV_DISPATCH";
            if (evts[i].flags == (EV_ENABLE|EV_DISPATCH)) flags = "EV_ENABLE|EV_DISPATCH";
            if (evts[i].flags == (EV_DISABLE|EV_DISPATCH)) flags = "EV_DISABLE|EV_DISABLE";
#endif

            keventbp += sprintf(keventbp, "%s{%lu, %s, %s}",i > 0 ? ", " : "",
                                evts[i].ident,
                                (evts[i].filter == EVFILT_READ
                                 ? "EVFILT_READ"
                                 : (evts[i].filter == EVFILT_WRITE
                                    ? "EVFILT_WRITE"
                                    : "UNKNOWN")), flags);
        }
        keventbp += sprintf(keventbp, "}, %d)", len);
        DEBUG_PRINT_FD("%s", ps, fd, keventb);
        errno = saved_errno;
    }
#endif
    if (res < 0) {
        if (op != ERTS_POLL_OP_DEL) {
#ifdef EV_RECEIPT
            struct kevent receipt_evts[2];
            len = 0;
            ERTS_EV_SET(&evts[len++], fd, EVFILT_WRITE, EV_DELETE|EV_RECEIPT, (void *) 0);
            ERTS_EV_SET(&evts[len++], fd, EVFILT_READ, EV_DELETE|EV_RECEIPT, (void *) 0);
            do {
                res = kevent(ps->kp_fd, evts, len, receipt_evts, 2, &ts);
            } while (res < 0 && errno == EINTR);
#else
            ERTS_EV_SET(&evts[0], fd, EVFILT_WRITE, EV_DELETE, (void *) 0);
            do {
                res = kevent(ps->kp_fd, evts, 1, NULL, 0, &ts);
            } while (res < 0 && errno == EINTR);
            ERTS_EV_SET(&evts[0], fd, EVFILT_READ, EV_DELETE, (void *) 0);
            do {
                res = kevent(ps->kp_fd, evts, 1, NULL, 0, &ts);
            } while (res < 0 && errno == EINTR);
#endif
            if (op == ERTS_POLL_OP_ADD)
                erts_atomic_dec_nob(&ps->no_of_user_fds);
            events = ERTS_POLL_EV_NVAL;
        } else
            events = 0;
    }
    return events;
}

#endif /* ERTS_POLL_USE_KQUEUE */

#if !ERTS_POLL_USE_CONCURRENT_UPDATE

static ERTS_INLINE void
init_batch_update(ErtsPollSet *ps, int len)
{
#if ERTS_POLL_USE_DEVPOLL
    ASSERT(ps->poll_fds == NULL);
    ps->poll_fds = erts_alloc(ERTS_ALC_T_TMP, sizeof(struct pollfd) * len);
    ps->poll_fds_ix = 0;
#endif
}

static ERTS_INLINE void
write_batch_update(ErtsPollSet *ps)
{
#if ERTS_POLL_USE_DEVPOLL
    ssize_t wres;
    char *buf = (char *) ps->poll_fds;
    size_t buf_size = sizeof(struct pollfd)*ps->poll_fds_ix;

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
#if ERTS_POLL_DEBUG_PRINT
        {
            int saved_errno = errno, i;
            char devpollb[2048], *devpollbp = devpollb;
            devpollbp += sprintf(devpollbp, "%d = devpoll(%d, {", wres, ps->kp_fd);
            for (i = 0; i < wres / sizeof(struct pollfd); i++) {
                if (devpollbp == devpollb)
                    devpollbp += sprintf(devpollbp, "%d = devpoll(%d, {", wres, ps->kp_fd);
                devpollbp += sprintf(devpollbp, "%s{fd = %d, events = %s}",
                                     i > 0 ? ", " : "",
                                     ps->poll_fds[i].fd,
                                     ev2str(ps->poll_fds[i].events));
                if (devpollbp - devpollb > 512) {
                    devpollbp += sprintf(devpollbp, "}, %d)", ps->poll_fds_ix);
                    DEBUG_PRINT("%s", ps, devpollb);
                    devpollbp = devpollb;
                }
            }
            devpollbp += sprintf(devpollbp, "}, %d)", ps->poll_fds_ix);
            DEBUG_PRINT("%s", ps, devpollb);
            errno = saved_errno;
        }
#endif

	buf_size -= wres;
	if (buf_size <= 0)
	    break;
	buf += wres;
    }

    if (buf_size < 0) {
	fatal_error("%s:%d:write_devpoll_buf(): Internal error\n",
		    __FILE__, __LINE__);
    }
    erts_free(ERTS_ALC_T_TMP, ps->poll_fds);
    ps->poll_fds = NULL;
#endif
}

static ERTS_INLINE int
need_update(ErtsPollSet *ps, int fd, int *resetp)
{
    int reset;
    ASSERT(fd < ps->fds_status_len);

    reset = (int) (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST);
    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;

    *resetp = reset;

    if (reset || ps->fds_status[fd].used_events != ps->fds_status[fd].events)
	return 1;

    return 0;
}

static int update_pollset(ErtsPollSet *ps, ErtsPollResFd pr[], int fd)
{
    int res = 0, reset = 0;
    ErtsPollEvents events = ps->fds_status[fd].events;
    ASSERT(fd < ps->fds_status_len);

    if (!need_update(ps, fd, &reset))
	return res;

#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
    if (!events) {
	int pix = ps->fds_status[fd].pix;
	int last_pix;

        if (reset) {
            /* When a fd has been reset, we tell the caller of erts_poll_wait
               this by setting the fd as ERTS_POLL_EV_NONE */
            ERTS_POLL_RES_SET_FD(&pr[res], fd);
            ERTS_POLL_RES_SET_EVTS(&pr[res], ERTS_POLL_EV_NONE);
            DEBUG_PRINT_FD("trig %s (poll)", ps, fd, ev2str(ERTS_POLL_EV_NONE));
            res++;
        }

	if (pix < 0) {
	    return res;
	}
	erts_atomic_dec_nob(&ps->no_of_user_fds);
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

    }
    else {
	int pix = ps->fds_status[fd].pix;
	if (pix < 0) {
	    erts_atomic_inc_nob(&ps->no_of_user_fds);
	    ps->fds_status[fd].pix = pix = ps->no_poll_fds++;
	    if (pix >= ps->poll_fds_len)
		grow_poll_fds(ps, pix);
	    ps->poll_fds[pix].fd = fd;
	    ps->fds_status[fd].pix = pix;
	}

	/* Events to be used in next poll */
	ps->poll_fds[pix].events = ev2pollev(events);
	if (ps->poll_fds[pix].revents) {
	    /* Remove result events that we should not poll for anymore */
	    ps->poll_fds[pix].revents
		&= ev2pollev(~(~ps->fds_status[fd].used_events
			       & events));
	}
	/* Save events to be used in next poll */
	ps->fds_status[fd].used_events = events;
    }
    return res;
#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
    if (!events) {

        if (reset) {
            /* When a fd has been reset, we tell the caller of erts_poll_wait
               this by setting the fd as ERTS_POLL_EV_NONE */
            ERTS_POLL_RES_SET_FD(&pr[res], fd);
            ERTS_POLL_RES_SET_EVTS(&pr[res], ERTS_POLL_EV_NONE);
            DEBUG_PRINT_FD("trig %s (select)", ps, fd, ev2str(ERTS_POLL_EV_NONE));
            res++;
        }

        if (check_select_fds(fd, &ps->input_fds, &ps->output_fds)) {
            ERTS_FD_CLR(fd, &ps->input_fds);
            ERTS_FD_CLR(fd, &ps->output_fds);
        }

        if (ps->fds_status[fd].used_events) {
            erts_atomic_dec_nob(&ps->no_of_user_fds);
            ps->fds_status[fd].used_events = (ErtsPollEvents) 0;
        }

        if (fd == ps->max_fd) {
            int max = ps->max_fd;
            for (max = ps->max_fd; max >= 0; max--)
                if (ps->fds_status[max].used_events)
                    break;
            ps->max_fd = max;
        }

    } else {

	ensure_select_fds(fd, &ps->input_fds, &ps->output_fds);

        if (!ps->fds_status[fd].used_events)
            erts_atomic_inc_nob(&ps->no_of_user_fds);

        if (events & ERTS_POLL_EV_IN)
            ERTS_FD_SET(fd, &ps->input_fds);
        else
            ERTS_FD_CLR(fd, &ps->input_fds);

        if (events & ERTS_POLL_EV_OUT)
            ERTS_FD_SET(fd, &ps->output_fds);
        else
            ERTS_FD_CLR(fd, &ps->output_fds);

	ps->fds_status[fd].used_events = events;

        if (fd > ps->max_fd)
            ps->max_fd = fd;
    }

    return res;
#elif ERTS_POLL_USE_DEVPOLL

    if (!events) {

        if (reset) {
            /* When a fd has been reset, we tell the caller of erts_poll_wait
               this by setting the fd as ERTS_POLL_EV_NONE */
            ERTS_POLL_RES_SET_FD(&pr[res], fd);
            ERTS_POLL_RES_SET_EVTS(&pr[res], ERTS_POLL_EV_NONE);
            DEBUG_PRINT_FD("trig %s (devpoll)", ps, fd, ev2str(ERTS_POLL_EV_NONE));
            res++;
        }

        ps->poll_fds[ps->poll_fds_ix].fd = fd;
        ps->poll_fds[ps->poll_fds_ix].revents = 0;
        ps->poll_fds[ps->poll_fds_ix++].events = POLLREMOVE;

        if (ps->fds_status[fd].used_events) {
            erts_atomic_dec_nob(&ps->no_of_user_fds);
            ps->fds_status[fd].used_events = 0;
        }

    } else {
        if (!ps->fds_status[fd].used_events) {
            erts_atomic_inc_nob(&ps->no_of_user_fds);
        }
        ps->poll_fds[ps->poll_fds_ix].fd = fd;
        ps->poll_fds[ps->poll_fds_ix].revents = 0;
        ps->poll_fds[ps->poll_fds_ix++].events = ERTS_POLL_EV_E2N(events);
        ps->fds_status[fd].used_events = ps->fds_status[fd].events;
    }

    return res;
#endif
}

static int
handle_update_requests(ErtsPollSet *ps, ErtsPollResFd pr[], int no_fds)
{
    int res = 0;
    ErtsPollSetUpdateRequestsBlock *urqbp = ps->curr_upd_req_block;

    while (urqbp) {
	ErtsPollSetUpdateRequestsBlock *free_urqbp = urqbp;
	int i;
	int len = urqbp->len;

        init_batch_update(ps, len);

	for (i = 0; i < len; i++) {
	    int fd = urqbp->fds[i];
	    ASSERT(fd < ps->fds_status_len);
            ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INURQ);

            /* We have run out of PollResFd slots to put results in,
               so we yield here and return later for more. */
            if (res == no_fds && pr != NULL) {
                memmove(urqbp->fds, urqbp->fds+i, sizeof(int) * (len - i));
                urqbp->len -= i;
                ps->curr_upd_req_block = urqbp;
                write_batch_update(ps);
                return res;
            }

            if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INURQ) {
                ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INURQ;
                res += update_pollset(ps, pr + res, fd);
            }
	}

	free_urqbp = urqbp;
	urqbp = urqbp->next;

	free_update_requests_block(ps, free_urqbp);

        write_batch_update(ps);

    }

    ps->curr_upd_req_block = &ps->update_requests;

#if ERTS_POLL_USE_DEVPOLL && defined(HARD_DEBUG)
    check_poll_status(ps);
#endif

    ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(ps);
    return res;
}

#endif /* !ERTS_POLL_USE_CONCURRENT_UPDATE */

static ERTS_INLINE ErtsPollEvents
poll_control(ErtsPollSet *ps, int fd, ErtsPollOp op,
             ErtsPollEvents events, int *do_wake)
{
    ErtsPollEvents new_events;

    if (fd < ps->internal_fd_limit || fd >= max_fds) {
	if (fd < 0 || fd >= max_fds) {
	    new_events = ERTS_POLL_EV_ERR;
	    goto done;
	}
#if ERTS_POLL_USE_KERNEL_POLL
	if (fd == ps->kp_fd) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#endif
	if (fd == ps->wake_fds[0] || fd == ps->wake_fds[1]) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#if ERTS_POLL_USE_TIMERFD
        if (fd == ps->timer_fd) {
	    new_events = ERTS_POLL_EV_NVAL;
            goto done;
        }
#endif
    }

#if ERTS_POLL_USE_CONCURRENT_UPDATE

    new_events = update_pollset(ps, fd, op, events);

#else /* !ERTS_POLL_USE_CONCURRENT_UPDATE */
    if (fd >= ps->fds_status_len)
	grow_fds_status(ps, fd);

    ASSERT(fd < ps->fds_status_len);

    if (op == ERTS_POLL_OP_DEL) {
        ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_RST;
        ps->fds_status[fd].events = 0;
        *do_wake = 1;
    } else if (op == ERTS_POLL_OP_ADD) {
        ASSERT(ps->fds_status[fd].events == 0);
        ps->fds_status[fd].events = events;
        *do_wake = 1;
    } else {
        ASSERT(op == ERTS_POLL_OP_MOD);
        ps->fds_status[fd].events = events;
        *do_wake = 1;
    }
    new_events = ps->fds_status[fd].events;

    enqueue_update_request(ps, fd);

#endif /* !ERTS_POLL_USE_CONCURRENT_UPDATE */

 done:
    DEBUG_PRINT_FD("%s = %s(%p, %d, %s, %s) do_wake=%d",
                   ps, fd, ev2str(new_events), __FUNCTION__, ps,
                   fd, op2str(op), ev2str(events), *do_wake);
    return new_events;
}

ErtsPollEvents
ERTS_POLL_EXPORT(erts_poll_control)(ErtsPollSet *ps,
				    ErtsSysFdType fd,
                                    ErtsPollOp op,
				    ErtsPollEvents events,
				    int* do_wake) /* In: Wake up polling thread */
				                  /* Out: Poller is woken */
{
    ErtsPollEvents res;

    ERTS_POLLSET_LOCK(ps);

    res = poll_control(ps, fd, op, events, do_wake);

    ERTS_POLLSET_UNLOCK(ps);

    if (*do_wake)
	wake_poller(ps, 0);

    return res;
}

/*
 * --- Wait on poll set ------------------------------------------------------
 */

#if ERTS_POLL_USE_KERNEL_POLL

static ERTS_INLINE int
ERTS_POLL_EXPORT(save_result)(ErtsPollSet *ps, ErtsPollResFd pr[], int max_res, int chk_fds_res, int ebadf)
{
    int n = chk_fds_res < max_res ? chk_fds_res : max_res, i;
    int res = n;
    int wake_fd = ps->wake_fds[0];

    if (ERTS_POLL_USE_WAKEUP(ps) || ERTS_POLL_DEBUG_PRINT || ERTS_POLL_USE_TIMERFD) {

        for (i = 0; i < n; i++) {
            int fd = ERTS_POLL_RES_GET_FD(&pr[i]);
#if ERTS_POLL_DEBUG_PRINT
            ErtsPollEvents evts = ERTS_POLL_RES_GET_EVTS(pr+i);

            if (fd != wake_fd
#if ERTS_POLL_USE_TIMERFD
                && fd != ps->timer_fd
#endif
                )
                DEBUG_PRINT_FD("trig %s (%s)", ps, fd,
                               ev2str(evts),
#if ERTS_POLL_USE_KQUEUE
                               "kqueue"
#elif ERTS_POLL_USE_EPOLL
                               "epoll"
#else
                               "/dev/poll"
#endif
                    );
#endif

            if (ERTS_POLL_USE_WAKEUP(ps) && fd == wake_fd) {
                cleanup_wakeup_pipe(ps);
                ERTS_POLL_RES_SET_FD(&pr[i], -1);
                ERTS_POLL_RES_SET_EVTS(&pr[i], ERTS_POLL_EV_NONE);
                res--;
            }
#if ERTS_POLL_USE_TIMERFD
            else if (fd == ps->timer_fd) {
                ERTS_POLL_RES_SET_FD(&pr[i], -1);
                ERTS_POLL_RES_SET_EVTS(&pr[i], ERTS_POLL_EV_NONE);
                res--;
            }
#endif
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
            else {
                /* Reset the events to emulate ONESHOT semantics */
                ps->fds_status[fd].events = 0;
                enqueue_update_request(ps, fd);
            }
#endif
        }
    }

    if (res == 0)
        return res;
    else
        return n;
}

#else /* !ERTS_POLL_USE_KERNEL_POLL */

static ERTS_INLINE int
ERTS_POLL_EXPORT(save_result)(ErtsPollSet *ps, ErtsPollResFd pr[], int max_res, int chk_fds_res, int ebadf)
{
#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
	int res = 0;
	int wake_fd = ps->wake_fds[0];
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
		    if (fd == wake_fd) {
			cleanup_wakeup_pipe(ps);
			i++;
			continue;
		    }
		    revents = pollev2ev(ps->poll_fds[i].revents);
		    ERTS_POLL_RES_SET_FD(&pr[res], fd);
		    ERTS_POLL_RES_SET_EVTS(&pr[res], revents);

                    /* If an fd returns as error, we may want to check the
                       update_requests queue to see if it has been reset
                       before delivering the result?!?! This should allow
                       the user to do driver_dselect + close without waiting
                       for stop_select... */

                    DEBUG_PRINT_FD("trig %s (poll)", ps, ERTS_POLL_RES_GET_FD(&pr[res]),
                                   ev2str(ERTS_POLL_RES_GET_EVTS(&pr[res])));

		    res++;

                    /* Clear the events for this fd in order to mimic
                       how epoll ONESHOT works */
                    ps->fds_status[fd].events = 0;
                    enqueue_update_request(ps, fd);
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
	int wake_fd = ps->wake_fds[0];
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
                    ErtsPollEvents events = 0;
		    if (ERTS_FD_ISSET(fd, &ps->res_input_fds)) {
			if (fd == wake_fd) {
			    cleanup_wakeup_pipe(ps);
			    fd++;
			    continue;
			}
			events |= ERTS_POLL_EV_IN;
		    }
		    if (ERTS_FD_ISSET(fd, &ps->res_output_fds))
			events |= ERTS_POLL_EV_OUT;
		    if (events) {
                        ERTS_POLL_RES_SET_FD(&pr[res], fd);
                        ERTS_POLL_RES_SET_EVTS(&pr[res], events);
			res++;
                        ps->fds_status[fd].events = 0;
                        enqueue_update_request(ps, fd);
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
			    if (fd == wake_fd) {
				cleanup_wakeup_pipe(ps);
				fd++;
				continue;
			    }
                            ERTS_POLL_RES_SET_FD(&pr[res], fd);
                            ERTS_POLL_RES_SET_EVTS(&pr[res], ERTS_POLL_EV_NVAL);
			    res++;
			}
			else if (sres > 0) {
                            ErtsPollEvents events = 0;
                            ERTS_POLL_RES_SET_FD(&pr[res], fd);
			    if (iset && ERTS_FD_ISSET(fd, iset)) {
				if (fd == wake_fd) {
				    cleanup_wakeup_pipe(ps);
				    fd++;
				    continue;
				}
				events |= ERTS_POLL_EV_IN;
			    }
			    if (oset && ERTS_FD_ISSET(fd, oset)) {
				events |= ERTS_POLL_EV_OUT;
			    }
			    ASSERT(events);
                            ERTS_POLL_RES_SET_EVTS(&pr[res], events);
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
#endif /* ERTS_POLL_USE_SELECT */
}

#endif /* !ERTS_POLL_USE_KERNEL_POLL */

static ERTS_INLINE ErtsMonotonicTime
get_timeout(ErtsPollSet *ps,
	    int resolution,
	    ErtsMonotonicTime timeout_time)
{
    ErtsMonotonicTime timeout;

    if (timeout_time == ERTS_POLL_NO_TIMEOUT) {
	timeout = 0;
    }
    else if (timeout_time == ERTS_POLL_INF_TIMEOUT) {
        timeout = -1;
    }
    else {
	ErtsMonotonicTime diff_time, current_time;
	current_time = erts_get_monotonic_time(NULL);
	diff_time = timeout_time - current_time;
	if (diff_time <= 0) {
	    timeout = 0;
	}
	else {
	    switch (resolution) {
	    case 1000:
		/* Round up to nearest even milli second */
		timeout = ERTS_MONOTONIC_TO_MSEC(diff_time - 1) + 1;
		if (timeout > (ErtsMonotonicTime) INT_MAX)
		    timeout = (ErtsMonotonicTime) INT_MAX;
		timeout -= ERTS_PREMATURE_TIMEOUT(timeout, 1000);
		break;
	    case 1000000:
		/* Round up to nearest even micro second */
		timeout = ERTS_MONOTONIC_TO_USEC(diff_time - 1) + 1;
		timeout -= ERTS_PREMATURE_TIMEOUT(timeout, 1000*1000);
		break;
	    case 1000000000:
		/* Round up to nearest even nano second */
		timeout = ERTS_MONOTONIC_TO_NSEC(diff_time - 1) + 1;
		timeout -= ERTS_PREMATURE_TIMEOUT(timeout, 1000*1000*1000);
		break;
	    default:
		ERTS_INTERNAL_ERROR("Invalid resolution");
		timeout = 0;
		break;
	    }
	}
    }
    return timeout;
}

#if ERTS_POLL_USE_SELECT

static ERTS_INLINE int
get_timeout_timeval(ErtsPollSet *ps,
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
    else if (timeout == -1) {
        return -1;
    }
    else {
	ErtsMonotonicTime sec = timeout/(1000*1000);
	tvp->tv_sec = sec;
	tvp->tv_usec = timeout - sec*(1000*1000);

	ASSERT(tvp->tv_sec >= 0);
	ASSERT(tvp->tv_usec >= 0);
	ASSERT(tvp->tv_usec < 1000*1000);

	return 1;
    }

}

#endif

#if ERTS_POLL_USE_KQUEUE || (ERTS_POLL_USE_POLL && defined(HAVE_PPOLL)) || ERTS_POLL_USE_TIMERFD

static ERTS_INLINE int
get_timeout_timespec(ErtsPollSet *ps,
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
    else if (timeout == -1) {
        return -1;
    }
    else {
	ErtsMonotonicTime sec = timeout/(1000*1000*1000);
	tsp->tv_sec = sec;
	tsp->tv_nsec = timeout - sec*(1000*1000*1000);

	ASSERT(tsp->tv_sec >= 0);
	ASSERT(tsp->tv_nsec >= 0);
	ASSERT(tsp->tv_nsec < 1000*1000*1000);

	return 1;
    }
}

#endif

#if ERTS_POLL_USE_TIMERFD

static ERTS_INLINE int
get_timeout_itimerspec(ErtsPollSet *ps,
                       struct itimerspec *itsp,
                       ErtsMonotonicTime timeout_time)
{

    itsp->it_interval.tv_sec = 0;
    itsp->it_interval.tv_nsec = 0;

    return get_timeout_timespec(ps, &itsp->it_value, timeout_time);
}
 
#endif

static ERTS_INLINE int
check_fd_events(ErtsPollSet *ps, ErtsPollResFd pr[], int max_res, ErtsMonotonicTime timeout_time)
{
    int res;
    int timeout;
    DEBUG_PRINT_WAIT("Entering check_fd_events(), timeout=%d", ps, timeout_time);
    {
#if ERTS_POLL_USE_EPOLL                /* --- epoll ------------------------------- */
#if ERTS_POLL_USE_TIMERFD
        struct itimerspec its;
        timeout = get_timeout_itimerspec(ps, &its, timeout_time);
        if (timeout > 0) {
            timerfd_set(ps, &its);
            res = epoll_wait(ps->kp_fd, pr, max_res, -1);
            res = timerfd_clear(ps, pr, res, max_res);
        } else {
            res = epoll_wait(ps->kp_fd, pr, max_res, timeout);
        }
#else /* !ERTS_POLL_USE_TIMERFD */
        timeout = (int) get_timeout(ps, 1000, timeout_time);
        res = epoll_wait(ps->kp_fd, pr, max_res, timeout);
#endif /* !ERTS_POLL_USE_TIMERFD */
#elif ERTS_POLL_USE_KQUEUE     /* --- kqueue ------------------------------ */
        struct timespec ts;
        struct timespec *tsp;
        timeout = get_timeout_timespec(ps, &ts, timeout_time);
        tsp = timeout < 0 ? NULL : &ts;
        res = kevent(ps->kp_fd, NULL, 0, pr, max_res, tsp);
#elif ERTS_POLL_USE_DEVPOLL	/* --- devpoll ----------------------------- */
        /*
         * The ioctl() will fail with EINVAL on Solaris 10 if dp_nfds
         * is set too high. dp_nfds should not be set greater than
         * the maximum number of file descriptors in the poll set.
         */
        struct dvpoll poll_res;
        int nfds = (int) erts_atomic_read_nob(&ps->no_of_user_fds) + 1 /* wakeup pipe */;
        poll_res.dp_nfds = nfds < max_res ? nfds : max_res;
        poll_res.dp_fds = pr;
        poll_res.dp_timeout = (int) get_timeout(ps, 1000, timeout_time);
        res = ioctl(ps->kp_fd, DP_POLL, &poll_res);
#elif ERTS_POLL_USE_POLL && defined(HAVE_PPOLL)	/* --- ppoll ---------------- */
        struct timespec ts;
        struct timespec *tsp = &ts;
        timeout = get_timeout_timespec(ps, &ts, timeout_time);
        if (timeout < 0) tsp = NULL;
        res = ppoll(ps->poll_fds, ps->no_poll_fds, tsp, NULL);
#elif ERTS_POLL_USE_POLL        /* --- poll --------------------------------- */
        timeout = (int) get_timeout(ps, 1000, timeout_time);
        res = poll(ps->poll_fds, ps->no_poll_fds, timeout);
#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
        SysTimeval tv;
        SysTimeval *tvp;
        timeout = get_timeout_timeval(ps, &tv, timeout_time);
        tvp = timeout < 0 ? NULL : &tv;

        ERTS_FD_COPY(&ps->input_fds, &ps->res_input_fds);
        ERTS_FD_COPY(&ps->output_fds, &ps->res_output_fds);

        res = ERTS_SELECT(ps->max_fd + 1,
                          &ps->res_input_fds,
                          &ps->res_output_fds,
                          NULL,
                          tvp);
#endif				/* ----------------------------------------- */
    }
    DEBUG_PRINT_WAIT("Leaving check_fd_events(), res=%d", ps, res);
    return res;
}

int
ERTS_POLL_EXPORT(erts_poll_wait)(ErtsPollSet *ps,
				 ErtsPollResFd pr[],
				 int *len,
                                 ErtsThrPrgrData *tpd,
                                 ErtsMonotonicTime timeout_time)
{
    int res, no_fds, used_fds = 0;
    int ebadf = 0;
    int do_wait;
    int ps_locked = 0;
    ERTS_MSACC_DECLARE_CACHE();

    no_fds = *len;
    *len = 0;
    ASSERT(no_fds > 0);

#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    if (ERTS_POLLSET_HAVE_UPDATE_REQUESTS(ps)) {
	ERTS_POLLSET_LOCK(ps);
	used_fds = handle_update_requests(ps, pr, no_fds);
	ERTS_POLLSET_UNLOCK(ps);

        if (used_fds == no_fds) {
            *len = used_fds;
            return 0;
        }
    }
#endif

    do_wait = !is_woken(ps) && used_fds == 0 && timeout_time != ERTS_POLL_NO_TIMEOUT;

    DEBUG_PRINT_WAIT("Entering %s(), do_wait=%d", ps, __FUNCTION__, do_wait);

    if (do_wait) {
        tpd = tpd ? tpd : erts_thr_prgr_data(NULL);
        erts_thr_progress_prepare_wait(tpd);
        ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_SLEEP);
    } else
        timeout_time = ERTS_POLL_NO_TIMEOUT;

    while (1) {
        res = check_fd_events(ps, pr + used_fds, no_fds - used_fds, timeout_time);
        if (res != 0)
            break;
        if (timeout_time == ERTS_POLL_NO_TIMEOUT)
            break;
        if (erts_get_monotonic_time(NULL) >= timeout_time)
	    break;
    }

#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    if (res < 0
        && errno == EBADF
        && ERTS_POLLSET_HAVE_UPDATE_REQUESTS(ps)) {
        /*
         * This may have happened because another thread deselected
         * a fd in our poll set and then closed it, i.e. the driver
         * behaved correctly. We wan't to avoid looking for a bad
         * fd, that may even not exist anymore. Therefore, handle
         * update requests and try again. This behaviour should only
         * happen when using SELECT as the polling mechanism.
         */
        ERTS_POLLSET_LOCK(ps);
        used_fds += handle_update_requests(ps, pr + used_fds, no_fds - used_fds);
        if (used_fds == no_fds) {
            *len = used_fds;
            ERTS_POLLSET_UNLOCK(ps);
            return 0;
        }
        res = check_fd_events(ps, pr + used_fds, no_fds - used_fds, ERTS_POLL_NO_TIMEOUT);
        /* Keep the lock over the non-blocking poll in order to not
           get any nasty races happening. */
        ERTS_POLLSET_UNLOCK(ps);
        if (res == 0) {
            errno = EAGAIN;
            res = -1;
        }
    }
#endif

    if (do_wait) {
        erts_thr_progress_finalize_wait(tpd);
        ERTS_MSACC_UPDATE_CACHE();
        ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_CHECK_IO);
    }

    if (ERTS_POLL_USE_WAKEUP(ps))
        woke_up(ps);

    if (res < 0) {
#if ERTS_POLL_USE_SELECT
	if (errno == EBADF) {
	    ebadf = 1;
	    goto save_results;
	}
#endif
	res = errno;
    }
    else if (res == 0) {
        res = used_fds == 0 ? ETIMEDOUT : 0;
#ifdef HARD_DEBUG
	check_poll_result(pr, used_fds);
#endif
        *len = used_fds;
    } else {
#if ERTS_POLL_USE_SELECT
    save_results:
#endif
	ps_locked = 1;
	ERTS_POLLSET_LOCK(ps);

	used_fds += ERTS_POLL_EXPORT(save_result)(ps, pr + used_fds, no_fds - used_fds, res, ebadf);

#ifdef HARD_DEBUG
	check_poll_result(pr, used_fds);
#endif

	res = (used_fds == 0 ? (is_interrupted_reset(ps) ? EINTR : EAGAIN) : 0);
	*len = used_fds;
    }

    if (ps_locked)
	ERTS_POLLSET_UNLOCK(ps);

    DEBUG_PRINT_WAIT("Leaving %s = %s(len = %d)", ps,
                     res == 0 ? "0" : erl_errno_id(res), __FUNCTION__, *len);

    return res;
}

/*
 * --- Interrupt a thread doing erts_poll_wait() -----------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_interrupt)(ErtsPollSet *ps, int set)
{
    DEBUG_PRINT_WAIT("poll_interrupt(%d)", ps, set);
    if (ERTS_POLL_USE_WAKEUP(ps)) {
        if (!set)
            reset_wakeup_state(ps);
        else
            wake_poller(ps, 1);
    }
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
ERTS_POLL_EXPORT(erts_poll_init)(int *concurrent_updates)
{

    errno = 0;

    if (concurrent_updates) {
#if ERTS_POLL_USE_CONCURRENT_UPDATE
        *concurrent_updates = 1;
#else
        *concurrent_updates = 0;
#endif
    }

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

    print_misc_debug_info();
}

ErtsPollSet *
ERTS_POLL_EXPORT(erts_poll_create_pollset)(int id)
{
#if ERTS_POLL_USE_KERNEL_POLL
    int kp_fd;
#endif
    ErtsPollSet *ps = erts_alloc(ERTS_ALC_T_POLLSET,
				sizeof(struct ERTS_POLL_EXPORT(erts_pollset)));
    ps->id = id;
    ps->internal_fd_limit = 0;
    erts_atomic_init_nob(&ps->no_of_user_fds, 0);
#if ERTS_POLL_USE_KERNEL_POLL
    ps->kp_fd = -1;
#if ERTS_POLL_USE_EPOLL
    kp_fd = epoll_create(256);
#elif ERTS_POLL_USE_DEVPOLL
    kp_fd = open("/dev/poll", O_RDWR);
#elif ERTS_POLL_USE_KQUEUE
    kp_fd = kqueue();
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
#if ERTS_POLL_USE_POLL
    ps->next_poll_fds_ix = 0;
    ps->no_poll_fds = 0;
    ps->poll_fds_len = 0;
    ps->poll_fds = NULL;
#elif ERTS_POLL_USE_SELECT
    ps->next_sel_fd = 0;
    ps->max_fd = -1;
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
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    ps->fds_status = NULL;
    ps->fds_status_len = 0;
    ps->update_requests.next = NULL;
    ps->update_requests.len = 0;
    ps->curr_upd_req_block = &ps->update_requests;
    erts_atomic32_init_nob(&ps->have_update_requests, 0);
    erts_mtx_init(&ps->mtx, "pollset", NIL, ERTS_LOCK_FLAGS_CATEGORY_IO);
#endif
#if ERTS_POLL_USE_KERNEL_POLL
    if (ps->internal_fd_limit <= kp_fd)
	ps->internal_fd_limit = kp_fd + 1;
    ps->kp_fd = kp_fd;
    if (ps->id == -1)
        ps->oneshot = 0;
    else
        ps->oneshot = 1;
#endif

    erts_atomic32_init_nob(&ps->wakeup_state, (erts_aint32_t) 0);
    create_wakeup_pipe(ps);

#if ERTS_POLL_USE_TIMERFD
    create_timerfd(ps);
#endif

#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    handle_update_requests(ps, NULL, 0);
    cleanup_wakeup_pipe(ps);
#endif
#if ERTS_POLL_USE_KERNEL_POLL && (defined(__DARWIN__) || defined(__APPLE__) && defined(__MACH__))
    {
        /*
         * Using kqueue on OS X is a mess of brokenness...
         *
         * On OS X version older than 15.6 (i.e. OS X El Capitan released in July 2015),
         * a thread waiting in kevent is not woken if an event is inserted into the kqueue
         * by another thread and the event becomes ready. However if a new call to kevent
         * is done by the waiting thread, the new event is found.
         *
         * So on effected OS X versions we could trigger the wakeup pipe so that
         * the waiters will be woken and re-issue the kevent. However...
         *
         * On OS X version older then 16 (i.e. OS X Sierra released in September 2016),
         * running the emulator driver_SUITE smp_select testcase consistently causes a
         * kernel panic. I don't know why or what events that trigger it. But it seems
         * like updates of the pollset while another thread is sleeping in it Creates
         * some kind of race that triggers the kernel panic.
         *
         * So to deal with this, the erts configure check what OS X version is run
         * and only enabled kernel poll on OS X 16 or newer. In addition, if someone
         * attempts to compile Erlang on OS X 16 and then run it on OS X 15, we do the
         * run-time check below to disallow this.
         */
        int major, minor, build;
        os_version(&major,&minor,&build);
        if (major < 16) {
            erts_fprintf(stderr,"BROKEN KQUEUE!\n"
                         "Erlang has been compiled with kernel-poll support,\n"
                         "but this OS X version is known to have kernel bugs\n"
                         "when using kernel-poll. You have two options:\n"
                         " 1) update to a newer OS X version (OS X Sierra or newer)\n"
                         " 2) recompile erlang without kernel-poll support\n");
            erts_exit(1, "");
        }
    }
#endif
    erts_atomic_set_nob(&ps->no_of_user_fds, 0); /* Don't count wakeup pipe and fallback fd */

    return ps;
}

/*
 * --- Info ------------------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_info)(ErtsPollSet *ps, ErtsPollInfo *pip)
{
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    int pending_updates;
#endif
    Uint size = 0;

    ERTS_POLLSET_LOCK(ps);

    size += sizeof(struct ERTS_POLL_EXPORT(erts_pollset));
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    size += ps->fds_status_len*sizeof(ErtsFdStatus);
#endif

#if ERTS_POLL_USE_POLL
    size += ps->poll_fds_len*sizeof(struct pollfd);
#elif ERTS_POLL_USE_SELECT
#ifdef _DARWIN_UNLIMITED_SELECT
    size += ps->input_fds.sz + ps->res_input_fds.sz
	 + ps->output_fds.sz + ps->res_output_fds.sz;
#endif
#endif

#if !ERTS_POLL_USE_CONCURRENT_UPDATE
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

    pip->poll_set_size = (int) erts_atomic_read_nob(&ps->no_of_user_fds);
    pip->poll_set_size++; /* Wakeup pipe */

    pip->lazy_updates =
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
	1
#else
	0
#endif
	;

    pip->pending_updates = 
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
	pending_updates
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

    pip->is_fallback =
#if ERTS_POLL_IS_FALLBACK
        1
#else
        0
#endif
        ;

    pip->batch_updates =
#if ERTS_POLL_USE_DEVPOLL
        1
#else
        0
#endif
        ;

    pip->max_fds = max_fds;

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

/*
 * --- Debug -----------------------------------------------------------------
 */

#if ERTS_POLL_USE_EPOLL
uint32_t epoll_events(int kp_fd, int fd)
{
    /* For epoll we read the information about what is selected upon from the proc fs.*/
    char fname[30];
    char s[256];
    FILE *f;
    unsigned int pos, flags, mnt_id;
    int line = 0;
    sprintf(fname,"/proc/%d/fdinfo/%d",getpid(), kp_fd);
    f = fopen(fname,"r");
    if (!f) {
        fprintf(stderr,"failed to open file %s, errno = %d\n", fname, errno);
        ASSERT(0);
        return 0;
    }
    if (fscanf(f,"pos:\t%x\nflags:\t%x", &pos, &flags) != 2) {
        fprintf(stderr,"failed to parse file %s, errno = %d\n", fname, errno);
        ASSERT(0);
        return 0;
    }
    if (fscanf(f,"\nmnt_id:\t%x\n", &mnt_id));
    line += 3;
    while (fgets(s, sizeof(s) / sizeof(*s), f)) {
        /* tfd:       10 events: 40000019 data:       180000000a */
        int ev_fd;
        uint32_t events;
        uint64_t data;
        if (sscanf(s,"tfd:%d events:%x data:%llx", &ev_fd, &events,
                   (unsigned long long*)&data) != 3) {
            fprintf(stderr,"failed to parse file %s on line %d, errno = %d\n", fname,
                    line,
                    errno);
            fclose(f);
            return 0;
        }
        if (fd == ev_fd) {
            fclose(f);
            return events;
        }
    }
    fclose(f);
    return 0;
}
#endif

void
ERTS_POLL_EXPORT(erts_poll_get_selected_events)(ErtsPollSet *ps,
						ErtsPollEvents ev[],
						int len)
{
    int fd;
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    ERTS_POLLSET_LOCK(ps);
    for (fd = 0; fd < len; fd++) {
	if (fd >= ps->fds_status_len)
	    ev[fd] = 0;
	else {
	    ev[fd] = ps->fds_status[fd].events;
            if (
                fd == ps->wake_fds[0] || fd == ps->wake_fds[1] ||
#if ERTS_POLL_USE_KERNEL_POLL
                fd == ps->kp_fd ||
#endif
                0)
                ev[fd] |= ERTS_POLL_EV_NVAL;
	}
    }
    ERTS_POLLSET_UNLOCK(ps);
#elif ERTS_POLL_USE_EPOLL

    /* For epoll we read the information about what is selected upon from the proc fs.*/
    char fname[30];
    char s[256];
    FILE *f;
    unsigned int pos, flags, mnt_id;
    int line = 0;
    sprintf(fname,"/proc/%d/fdinfo/%d",getpid(), ps->kp_fd);
    for (fd = 0; fd < len; fd++)
        ev[fd] = ERTS_POLL_EV_NONE;
    f = fopen(fname,"r");
    if (!f) {
        fprintf(stderr,"failed to open file %s, errno = %d\n", fname, errno);
        return;
    }
    if (fscanf(f,"pos:\t%x\nflags:\t%x", &pos, &flags) != 2) {
        fprintf(stderr,"failed to parse file %s, errno = %d\n", fname, errno);
        ASSERT(0);
        fclose(f);
        return;
    }
    if (fscanf(f,"\nmnt_id:\t%x\n", &mnt_id));
    line += 3;
    while (fgets(s, sizeof(s) / sizeof(*s), f)) {
        /* tfd:       10 events: 40000019 data:       180000000a */
        int fd;
        uint32_t events;
        uint64_t data;
        if (sscanf(s,"tfd:%d events:%x data:%llx", &fd, &events,
                   (unsigned long long*)&data) != 3) {
            fprintf(stderr,"failed to parse file %s on line %d, errno = %d\n",
                    fname, line, errno);
            ASSERT(0);
            fclose(f);
            return;
        }
        if (fd == ps->wake_fds[0] || fd == ps->wake_fds[1])
            continue;
#if ERTS_POLL_USE_TIMERFD
        if (fd == ps->timer_fd)
            continue;
#endif
        data &= 0xFFFFFFFF;
        ASSERT(fd == data);
        /* Events are the events that are being monitored, which of course include
           error and hup events, but we are only interested in IN/OUT events */
        ev[fd] = (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT) & ERTS_POLL_EV_N2E(events);
        line++;
    }
    fclose(f);
#else
    for (fd = 0; fd < len; fd++)
        ev[fd] = ERTS_POLL_EV_NONE;
#endif
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


#if ERTS_POLL_USE_DEVPOLL && defined(DEBUG)

static void
check_poll_status(ErtsPollSet *ps)
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

static void
print_misc_debug_info(void)
{
#if ERTS_POLL_DEBUG_PRINT
    erts_printf("erts_poll using: %s lazy_updates:%s\n",
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
		,
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
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
#endif
}

#ifdef ERTS_ENABLE_LOCK_COUNT
void ERTS_POLL_EXPORT(erts_lcnt_enable_pollset_lock_count)(ErtsPollSet *pollset, int enable)
{
#if !ERTS_POLL_USE_CONCURRENT_UPDATE
    if(enable) {
        erts_lcnt_install_new_lock_info(&pollset->mtx.lcnt, "pollset_rm", NIL,
            ERTS_LOCK_TYPE_MUTEX | ERTS_LOCK_FLAGS_CATEGORY_IO);
    } else {
        erts_lcnt_uninstall(&pollset->mtx.lcnt);
    }
#endif
    return;
}
#endif
