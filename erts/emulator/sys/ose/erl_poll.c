/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2012. All Rights Reserved.
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
 * Description:	Poll interface suitable for ERTS on OSE with or without
 *              SMP support.
 *
 *		The interface is currently implemented using:
 *                - receive + receive_fsem
 *
 * Author: 	Lukas Larsson
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_thr_progress.h"
#include "erl_driver.h"
#include "erl_alloc.h"
#include "erl_poll.h"

#define NOFILE    4096

/*
 * Some debug macros
 */

/* #define HARDDEBUG
#define HARDTRACE*/
#ifdef HARDDEBUG
#ifdef HARDTRACE
#define HARDTRACEF(X, ...) { fprintf(stderr, X, __VA_ARGS__); fprintf(stderr,"\r\n"); }
#else
#define HARDTRACEF(...)
#endif

#else
#define HARDTRACEF(X,...)
#define HARDDEBUGF(...)
#endif

#if 0
#define ERTS_POLL_DEBUG_PRINT
#endif

#if defined(DEBUG) && 0
#define HARD_DEBUG
#endif

#  define SEL_ALLOC	erts_alloc
#  define SEL_REALLOC	realloc_wrap
#  define SEL_FREE	erts_free

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
 * --- Data types ------------------------------------------------------------
 */

union SIGNAL {
    SIGSELECT sig_no;
};

typedef struct erts_sigsel_item_ ErtsSigSelItem;

struct erts_sigsel_item_ {
    ErtsSigSelItem *next;
    ErtsSysFdType fd;
    ErtsPollEvents events;
};

typedef struct erts_sigsel_info_ ErtsSigSelInfo;

struct erts_sigsel_info_ {
    ErtsSigSelInfo *next;
    SIGSELECT signo;
    ErlDrvOseEventId (*decode)(union SIGNAL* sig);
    ErtsSigSelItem *fds;
};

struct ErtsPollSet_ {
    SIGSELECT *sigs;
    ErtsSigSelInfo *info;
    Uint sig_count;
    Uint item_count;
    PROCESS interrupt;
    erts_atomic32_t wakeup_state;
    erts_atomic64_t timeout_time;
#ifdef ERTS_SMP
    erts_smp_mtx_t mtx;
#endif
};

static int max_fds = -1;

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

#define ERTS_POLL_NOT_WOKEN		((erts_aint32_t) (1 << 0))
#define ERTS_POLL_WOKEN_INTR		((erts_aint32_t) (1 << 1))
#define ERTS_POLL_WOKEN_TIMEDOUT	((erts_aint32_t) (1 << 2))
#define ERTS_POLL_WOKEN_IO_READY	((erts_aint32_t) (1 << 3))
#define ERTS_POLL_SLEEPING	        ((erts_aint32_t) (1 << 4))

/* signal list prototypes */
static ErtsSigSelInfo *get_sigsel_info(ErtsPollSet ps, SIGSELECT signo);
static ErtsSigSelItem *get_sigsel_item(ErtsPollSet ps, ErtsSysFdType fd);
static ErtsSigSelInfo *add_sigsel_info(ErtsPollSet ps, ErtsSysFdType fd,
				       ErlDrvOseEventId (*decode)(union SIGNAL* sig));
static ErtsSigSelItem *add_sigsel_item(ErtsPollSet ps, ErtsSysFdType fd,
				       ErlDrvOseEventId (*decode)(union SIGNAL* sig));
static int del_sigsel_info(ErtsPollSet ps, ErtsSigSelInfo *info);
static int del_sigsel_item(ErtsPollSet ps, ErtsSigSelItem *item);
static int update_sigsel(ErtsPollSet ps);

static ErtsSigSelInfo *
get_sigsel_info(ErtsPollSet ps, SIGSELECT signo) {
    ErtsSigSelInfo *curr = ps->info;
    while (curr != NULL) {
	if (curr->signo == signo)
	    return curr;
	curr = curr->next;
    }
    return NULL;
}

static ErtsSigSelItem *
get_sigsel_item(ErtsPollSet ps, ErtsSysFdType fd) {
    ErtsSigSelInfo *info = get_sigsel_info(ps,fd->signo);
    ErtsSigSelItem *curr;

    if (info == NULL)
	return NULL;

    curr = info->fds;

    while (curr != NULL) {
	if (curr->fd->id == fd->id) {
	    ASSERT(curr->fd->signo == fd->signo);
	    return curr;
	}
	curr = curr->next;
    }
    return NULL;
}

static ErtsSigSelInfo *
add_sigsel_info(ErtsPollSet ps, ErtsSysFdType fd,
		ErlDrvOseEventId (*decode)(union SIGNAL* sig)) {
    ErtsSigSelInfo *info = SEL_ALLOC(ERTS_ALC_T_POLLSET,
		       sizeof(ErtsSigSelInfo));
    info->next = ps->info;
    info->fds = NULL;
    info->signo = fd->signo;
    info->decode = decode;
    ps->info = info;
    ps->sig_count++;
    return info;
}

static ErtsSigSelItem *
add_sigsel_item(ErtsPollSet ps, ErtsSysFdType fd,
		ErlDrvOseEventId (*decode)(union SIGNAL* sig)) {
    ErtsSigSelInfo *info = get_sigsel_info(ps,fd->signo);
    ErtsSigSelItem *item = SEL_ALLOC(ERTS_ALC_T_POLLSET,
			   sizeof(ErtsSigSelItem));
    if (info == NULL)
	info = add_sigsel_info(ps, fd, decode);
    if (info->decode != decode) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "erts_poll_control() inconsistency: multiple resolve_signal functions for same signal (%d)\n",
	    fd->signo);
	erts_send_error_to_logger_nogl(dsbufp);
    }
    ASSERT(info->decode == decode);
    item->next = info->fds;
    item->fd = fd;
    item->events = 0;
    info->fds = item;
    ps->item_count++;
    return item;
}

static int del_sigsel_info(ErtsPollSet ps, ErtsSigSelInfo *info) {
    ErtsSigSelInfo *curr, *prev;

    if (ps->info == info) {
	ps->info = ps->info->next;
    } else {
	curr = ps->info->next;
	prev = ps->info;

	while (curr != info) {
	    if (curr == NULL)
		return 1;
	    prev = curr;
	    curr = curr->next;
	}
	prev->next = curr->next;
    }

    ps->sig_count--;
    SEL_FREE(ERTS_ALC_T_POLLSET, info);
    return 0;
}

static int del_sigsel_item(ErtsPollSet ps, ErtsSigSelItem *item) {
    ErtsSigSelInfo *info = get_sigsel_info(ps,item->fd->signo);
    ErtsSigSelItem *curr, *prev;

    ps->item_count--;
    ASSERT(ps->item_count >= 0);

    if (info->fds == item) {
	info->fds = info->fds->next;
	SEL_FREE(ERTS_ALC_T_POLLSET,item);
	if (info->fds == NULL)
	    return del_sigsel_info(ps,info);
	return 0;
    }

    curr = info->fds->next;
    prev = info->fds;

    while (curr != item) {
	if (curr == NULL) {
	    /* We did not find an item to delete so we have to
	     * increment item count again.
	     */
	    ps->item_count++;
	    return 1;
	}
	prev = curr;
	curr = curr->next;
    }
    prev->next = curr->next;
    SEL_FREE(ERTS_ALC_T_POLLSET,item);
    return 0;
}

#ifdef ERTS_SMP

static void update_redir_tables(ErtsPollSet ps) {
  struct OS_redir_entry *redir_table;
  PROCESS sched_1 = ERTS_SCHEDULER_IX(0)->tid.id;
  int i;
  redir_table = SEL_ALLOC(ERTS_ALC_T_POLLSET,
			  sizeof(struct OS_redir_entry)*(ps->sig_count+1));

  redir_table[0].sig = ps->sig_count+1;
  redir_table[0].pid = 0;

  for (i = 1; i < ps->sig_count+1; i++) {
    redir_table[i].sig = ps->sigs[i];
    redir_table[i].pid = sched_1;
  }

  for (i = 1; i < erts_no_schedulers; i++) {
    ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(i);
    set_redirection(esdp->tid.id,redir_table);
  }

  SEL_FREE(ERTS_ALC_T_POLLSET,redir_table);
}

#endif

static int update_sigsel(ErtsPollSet ps) {
    ErtsSigSelInfo *info = ps->info;

    int i;

    if (ps->sigs != NULL)
	SEL_FREE(ERTS_ALC_T_POLLSET,ps->sigs);

    if (ps->sig_count == 0) {
	/* If there are no signals we place a non-valid signal to make sure that
	 * we do not trigger on a any unrelated signals which are sent to the
	 * process.
	 */
	ps->sigs = SEL_ALLOC(ERTS_ALC_T_POLLSET,sizeof(SIGSELECT)*(2));
	ps->sigs[0] = 1;
	ps->sigs[1] = ERTS_SIGNAL_INVALID;
	return 0;
    }

    ps->sigs = SEL_ALLOC(ERTS_ALC_T_POLLSET,sizeof(SIGSELECT)*(ps->sig_count+1));
    ps->sigs[0] = ps->sig_count;

    for (i = 1; info != NULL; i++, info = info->next)
	ps->sigs[i] = info->signo;

#ifdef ERTS_SMP
    update_redir_tables(ps);
#endif

    return 0;
}

static ERTS_INLINE void
wake_poller(ErtsPollSet ps)
{
  erts_aint32_t wakeup_state;

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
  if (wakeup_state == ERTS_POLL_SLEEPING) {
    /*
     * Since we don't know the internals of signal_fsem() we issue
     * a memory barrier as a safety precaution ensuring that
     * the store we just made to wakeup_state wont be reordered
     * with loads in signal_fsem().
     */
    ERTS_THR_MEMORY_BARRIER;
    signal_fsem(ps->interrupt);
  }
}

static ERTS_INLINE void
reset_interrupt(ErtsPollSet ps)
{
    /* We need to keep io-ready if set */
    erts_aint32_t wakeup_state = erts_atomic32_read_nob(&ps->wakeup_state);
    while (wakeup_state != ERTS_POLL_NOT_WOKEN &&
	   wakeup_state != ERTS_POLL_SLEEPING) {
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
    wake_poller(ps);
}

void erts_poll_interrupt(ErtsPollSet ps,int set) {
    HARDTRACEF("erts_poll_interrupt called!\n");

    if (!set)
	reset_interrupt(ps);
    else
	set_interrupt(ps);

}

void erts_poll_interrupt_timed(ErtsPollSet ps,
			       int set,
			       ErtsTimeoutTime timeout_time) {
    HARDTRACEF("erts_poll_interrupt_timed called!\n");

    if (!set)
	reset_interrupt(ps);
    else if (get_timeout_time(ps) > timeout_time)
        set_interrupt(ps);
}

ErtsPollEvents erts_poll_control(ErtsPollSet ps, ErtsSysFdType fd,
	ErtsPollEvents pe, int on, int* do_wake) {
    ErtsSigSelItem *curr;
    ErtsPollEvents new_events;
    int old_sig_count;

    HARDTRACEF(
	    "%ux: In erts_poll_control, fd = %d, pe = %d, on = %d, *do_wake = %d, curr = 0x%xu",
	    ps, fd, pe, on, do_wake, curr);

    ERTS_POLLSET_LOCK(ps);

    if (on && (pe & ERTS_POLL_EV_IN) && (pe & ERTS_POLL_EV_OUT)) {
      /* Check to make sure both in and out are not used at the same time */
      new_events = ERTS_POLL_EV_NVAL;
      goto done;
    }

    curr = get_sigsel_item(ps, fd);
    old_sig_count = ps->sig_count;

    if (curr == NULL && on) {
	curr = add_sigsel_item(ps, fd, fd->resolve_signal);
    } else if (curr == NULL && !on) {
        new_events = ERTS_POLL_EV_NVAL;
	goto done;
    }

    new_events = curr->events;

    if (pe == 0) {
	*do_wake = 0;
	goto done;
    }

    if (on) {
	new_events |= pe;
	curr->events = new_events;
    } else {
	new_events &= ~pe;
	curr->events = new_events;
	if (new_events == 0 && del_sigsel_item(ps, curr)) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
    }

    if (ps->sig_count != old_sig_count) {
      if (update_sigsel(ps))
	new_events = ERTS_POLL_EV_NVAL;
    }
done:
    ERTS_POLLSET_UNLOCK(ps);
    HARDTRACEF("%ux: Out erts_poll_control", ps);
    return new_events;
}

int erts_poll_wait(ErtsPollSet ps,
		   ErtsPollResFd pr[],
		   int *len,
		   ErtsMonotonicTime timeout_time)
{
    int res = ETIMEDOUT, no_fds, currid = 0;
    OSTIME timeout;
    union SIGNAL *sig;
    ErtsMonotonicTime current_time, diff_time, timeout;
    // HARDTRACEF("%ux: In erts_poll_wait",ps);
    if (ps->interrupt == (PROCESS)0)
      ps->interrupt = current_process();

    ASSERT(current_process() == ps->interrupt);
    ASSERT(get_fsem(current_process()) == 0);
    ASSERT(erts_atomic32_read_nob(&ps->wakeup_state) &
	   (ERTS_POLL_NOT_WOKEN | ERTS_POLL_WOKEN_INTR));
    /* Max no of spots avable in pr */
    no_fds = *len;

    *len = 0;

    /* erts_printf("Entering erts_poll_wait(), timeout_time=%bps\n",
		   timeout_time); */

    if (timeout_time == ERTS_POLL_NO_TIMEOUT) {
    no_timeout:
	timeout = (OSTIME) 0;
	save_timeout_time = ERTS_MONOTONIC_TIME_MIN;
    }
    else {
	ErtsMonotonicTime current_time, diff_time;
	current_time = erts_get_monotonic_time(NULL);
	diff_time = timeout_time - current_time;
	if (diff_time <= 0)
	    goto no_timeout;
	diff_time = (ERTS_MONOTONIC_TO_MSEC(diff_time - 1) + 1);
	if (diff_time > INT_MAX)
	    diff_time = INT_MAX;
	timeout = (OSTIME) diff_time;
	save_timeout_time = current_time;
	save_timeout_time += ERTS_MSEC_TO_MONOTONIC(diff_time);
    }

    set_timeout_time(ps, save_timeout_time);

    while (currid < no_fds) {
      if (timeout > 0) {
	erts_aint32_t act = erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
						      ERTS_POLL_SLEEPING,
						      ERTS_POLL_NOT_WOKEN);
	if (act == ERTS_POLL_NOT_WOKEN) {
#ifdef ERTS_SMP
	  erts_thr_progress_prepare_wait(NULL);
#endif
	  sig = receive_fsem(timeout, ps->sigs, 1);
#ifdef ERTS_SMP
	  erts_thr_progress_finalize_wait(NULL);
#endif
	} else {
	  ASSERT(act == ERTS_POLL_WOKEN_INTR);
	  sig = OS_RCV_FSEM;
	}
      } else
	  sig = receive_w_tmo(0, ps->sigs);

	if (sig == NULL) {
	  if (timeout > 0) {
	    erts_aint32_t act = erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
							  ERTS_POLL_WOKEN_TIMEDOUT,
							  ERTS_POLL_SLEEPING);
	    if (act == ERTS_POLL_WOKEN_INTR)
	      /* Restore fsem as it was signaled but we got a timeout */
	      wait_fsem(1);
	    } else
	    erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
				      ERTS_POLL_WOKEN_TIMEDOUT,
				      ERTS_POLL_NOT_WOKEN);
	    break;
	} else if (sig == OS_RCV_FSEM) {
	  ASSERT(erts_atomic32_read_nob(&ps->wakeup_state) == ERTS_POLL_WOKEN_INTR);
	  break;
	}
       {
          ErtsSigSelInfo *info = get_sigsel_info(ps, sig->sig_no);
          struct erts_sys_fd_type fd = { sig->sig_no, info->decode(sig) };
          ErtsSigSelItem *item = get_sigsel_item(ps, &fd);

	  ASSERT(sig);
          if (currid == 0 && timeout > 0) {
	    erts_aint32_t act = erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
							  ERTS_POLL_WOKEN_IO_READY,
							  ERTS_POLL_SLEEPING);
	    if (act == ERTS_POLL_WOKEN_INTR) {
	      /* Restore fsem as it was signaled but we got a msg */
	      wait_fsem(1);
	      act = erts_atomic32_cmpxchg_nob(&ps->wakeup_state,
					      ERTS_POLL_WOKEN_IO_READY,
					      ERTS_POLL_WOKEN_INTR);
	    }
	  } else if (currid == 0) {
	    erts_atomic32_set_nob(&ps->wakeup_state,
				  ERTS_POLL_WOKEN_IO_READY);
	  }

	  if (item == NULL) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(
                dsbufp,
                "erts_poll_wait() failed: found unkown signal id %d (signo %u) "
		"(curr_proc 0x%x)\n",
                fd.id, fd.signo, current_process());
             erts_send_error_to_logger_nogl(dsbufp);
	     timeout = 0;
             /* Under normal circumstances the signal is deallocated by the
              * driver that issued the select operation. But in this case
              * there's no driver waiting for such signal so we have to
              * deallocate it here */
             if (sig)
                 free_buf(&sig);
	  } else {
	    int i;
	    struct erts_sys_fd_type *fd = NULL;
	    ErtsPollOseMsgList *tl,*new;

	    /* Check if this fd has already been triggered by a previous signal */
	    for (i = 0; i < currid;i++) {
	      if (pr[i].fd == item->fd) {
		fd = pr[i].fd;
		pr[i].events |= item->events;
		break;
	      }
	    }

	    /* First time this fd is triggered */
	    if (fd == NULL) {
	      pr[currid].fd = item->fd;
	      pr[currid].events = item->events;
	      fd = item->fd;
	      timeout = 0;
	      currid++;
	    }

	    /* Insert new signal in approriate list */
	    new = erts_alloc(ERTS_ALC_T_FD_SIG_LIST,sizeof(ErtsPollOseMsgList));
	    new->next = NULL;
	    new->data = sig;

	    ethr_mutex_lock(&fd->mtx);
	    tl = fd->msgs;

	    if (tl == NULL) {
	      fd->msgs = new;
	    } else {
	      while (tl->next != NULL)
		tl = tl->next;
	      tl->next = new;
	    }
	    ethr_mutex_unlock(&fd->mtx);
          }

       }
    }

    {
       erts_aint32_t wakeup_state = erts_atomic32_read_nob(&ps->wakeup_state);

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
          case ERTS_POLL_NOT_WOKEN:
             /* This happens when we get an invalid signal only */
             res = EINVAL;
             break;
          default:
             res = 0;
             erl_exit(ERTS_ABORT_EXIT,
                      "%s:%d: Internal error: Invalid wakeup_state=%d\n",
                      __FILE__, __LINE__, (int) wakeup_state);
       }
    }

    erts_atomic32_set_nob(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN);
    set_timeout_time(ps, ERTS_MONOTONIC_TIME_MAX);

    *len = currid;

    // HARDTRACEF("%ux: Out erts_poll_wait",ps);
    return res;
}

int erts_poll_max_fds(void)
{

    HARDTRACEF("In/Out erts_poll_max_fds -> %d",max_fds);
    return max_fds;
}

void erts_poll_info(ErtsPollSet ps,
		    ErtsPollInfo *pip)
{
    Uint size = 0;
    Uint num_events = 0;

    size += sizeof(struct ErtsPollSet_);
    size += sizeof(ErtsSigSelInfo)*ps->sig_count;
    size += sizeof(ErtsSigSelItem)*ps->item_count;
    size += sizeof(SIGSELECT)*(ps->sig_count+1);

    pip->primary = "receive_fsem";

    pip->fallback = NULL;

    pip->kernel_poll = NULL;

    pip->memory_size = size;

    pip->poll_set_size = num_events;

    pip->fallback_poll_set_size = 0;

    pip->lazy_updates = 0;

    pip->pending_updates = 0;

    pip->batch_updates = 0;

    pip->concurrent_updates = 0;


    pip->max_fds = erts_poll_max_fds();
    HARDTRACEF("%ux: Out erts_poll_info",ps);

}

ErtsPollSet erts_poll_create_pollset(void)
{
    ErtsPollSet ps = SEL_ALLOC(ERTS_ALC_T_POLLSET,
			       sizeof(struct ErtsPollSet_));

    ps->sigs       = NULL;
    ps->sig_count  = 0;
    ps->item_count = 0;
    ps->info       = NULL;
    ps->interrupt  = (PROCESS)0;
    erts_atomic32_init_nob(&ps->wakeup_state, ERTS_POLL_NOT_WOKEN);
    init_timeout_time(ps);
#ifdef ERTS_SMP
    erts_smp_mtx_init(&ps->mtx, "pollset");
#endif
    update_sigsel(ps);
    HARDTRACEF("%ux: Out erts_poll_create_pollset",ps);
    return ps;
}

void erts_poll_destroy_pollset(ErtsPollSet ps)
{
    ErtsSigSelInfo *info;
    for (info = ps->info; ps->info != NULL; info = ps->info, ps->info = ps->info->next) {
	ErtsSigSelItem *item;
	for (item = info->fds; info->fds != NULL; item = info->fds, info->fds = info->fds->next)
	    SEL_FREE(ERTS_ALC_T_POLLSET, item);
	SEL_FREE(ERTS_ALC_T_POLLSET, info);
    }

    SEL_FREE(ERTS_ALC_T_POLLSET,ps->sigs);

#ifdef ERTS_SMP
    erts_smp_mtx_destroy(&ps->mtx);
#endif

    SEL_FREE(ERTS_ALC_T_POLLSET,ps);
}

void  erts_poll_init(void)
{
    HARDTRACEF("In %s", __FUNCTION__);
    max_fds = 256;

    HARDTRACEF("Out %s", __FUNCTION__);
}


/* OSE driver functions */

union SIGNAL *erl_drv_ose_get_signal(ErlDrvEvent drv_ev) {
    struct erts_sys_fd_type *ev = (struct erts_sys_fd_type *)drv_ev;
    ethr_mutex_lock(&ev->mtx);
    if (ev->msgs == NULL) {
      ethr_mutex_unlock(&ev->mtx);
      return NULL;
    } else {
      ErtsPollOseMsgList *msg = ev->msgs;
      union SIGNAL *sig = (union SIGNAL*)msg->data;
      ASSERT(msg->data);
      ev->msgs = msg->next;
      ethr_mutex_unlock(&ev->mtx);
      erts_free(ERTS_ALC_T_FD_SIG_LIST,msg);
      restore(sig);
      return sig;
    }
}

ErlDrvEvent
erl_drv_ose_event_alloc(SIGSELECT signo, ErlDrvOseEventId id,
			ErlDrvOseEventId (*resolve_signal)(union SIGNAL *sig), void *extra) {
  struct erts_sys_fd_type *ev = erts_alloc(ERTS_ALC_T_DRV_EV,
					   sizeof(struct erts_sys_fd_type));
  ev->signo = signo;
  ev->extra = extra;
  ev->id = id;
  ev->msgs = NULL;
  ev->resolve_signal = resolve_signal;
  ethr_mutex_init(&ev->mtx);
  return (ErlDrvEvent)ev;
}

void erl_drv_ose_event_free(ErlDrvEvent drv_ev) {
  struct erts_sys_fd_type *ev = (struct erts_sys_fd_type *)drv_ev;
  ASSERT(ev->msgs == NULL);
  ethr_mutex_destroy(&ev->mtx);
  erts_free(ERTS_ALC_T_DRV_EV,ev);
}

void erl_drv_ose_event_fetch(ErlDrvEvent drv_ev, SIGSELECT *signo,
                             ErlDrvOseEventId *id, void **extra) {
  struct erts_sys_fd_type *ev = (struct erts_sys_fd_type *)drv_ev;
  if (signo)
    *signo = ev->signo;
  if (extra)
    *extra = ev->extra;
  if (id)
    *id = ev->id;
}
