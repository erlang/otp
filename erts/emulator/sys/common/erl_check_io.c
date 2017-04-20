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
 * Description:	Check I/O
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERL_CHECK_IO_C__
#define ERTS_WANT_BREAK_HANDLING
#ifndef WANT_NONBLOCKING
#  define WANT_NONBLOCKING
#endif
#include "sys.h"
#include "global.h"
#include "erl_port.h"
#include "erl_check_io.h"
#include "erl_thr_progress.h"
#include "erl_bif_unique.h"
#include "dtrace-wrapper.h"
#include "lttng-wrapper.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
#  include "safe_hash.h"
#  define DRV_EV_STATE_HTAB_SIZE 1024
#endif

typedef char EventStateType;
#define ERTS_EV_TYPE_NONE     ((EventStateType) 0)
#define ERTS_EV_TYPE_DRV_SEL  ((EventStateType) 1) /* driver_select */
#define ERTS_EV_TYPE_DRV_EV   ((EventStateType) 2) /* driver_event */
#define ERTS_EV_TYPE_STOP_USE ((EventStateType) 3) /* pending stop_select */
#define ERTS_EV_TYPE_NIF      ((EventStateType) 4) /* enif_select */
#define ERTS_EV_TYPE_STOP_NIF ((EventStateType) 5) /* pending nif stop */

typedef char EventStateFlags;
#define ERTS_EV_FLAG_USED         ((EventStateFlags) 1)   /* ERL_DRV_USE has been turned on */
#define ERTS_EV_FLAG_DEFER_IN_EV  ((EventStateFlags) 2)
#define ERTS_EV_FLAG_DEFER_OUT_EV ((EventStateFlags) 4)

#ifdef DEBUG
#  define ERTS_ACTIVE_FD_INC 2
#else
#  define ERTS_ACTIVE_FD_INC 128
#endif

#define ERTS_CHECK_IO_POLL_RES_LEN 512

#if defined(ERTS_KERNEL_POLL_VERSION)
#  define ERTS_CIO_EXPORT(FUNC) FUNC ## _kp
#elif defined(ERTS_NO_KERNEL_POLL_VERSION)
#  define ERTS_CIO_EXPORT(FUNC) FUNC ## _nkp
#else
#  define ERTS_CIO_EXPORT(FUNC) FUNC
#endif

#define ERTS_CIO_HAVE_DRV_EVENT \
  (ERTS_POLL_USE_POLL && !ERTS_POLL_USE_KERNEL_POLL)

#define ERTS_CIO_POLL_CTL	ERTS_POLL_EXPORT(erts_poll_control)
#define ERTS_CIO_POLL_CTLV	ERTS_POLL_EXPORT(erts_poll_controlv)
#define ERTS_CIO_POLL_WAIT	ERTS_POLL_EXPORT(erts_poll_wait)
#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
#define ERTS_CIO_POLL_AS_INTR 	ERTS_POLL_EXPORT(erts_poll_async_sig_interrupt)
#endif
#define ERTS_CIO_POLL_INTR 	ERTS_POLL_EXPORT(erts_poll_interrupt)
#define ERTS_CIO_POLL_INTR_TMD	ERTS_POLL_EXPORT(erts_poll_interrupt_timed)
#define ERTS_CIO_NEW_POLLSET 	ERTS_POLL_EXPORT(erts_poll_create_pollset)
#define ERTS_CIO_FREE_POLLSET	ERTS_POLL_EXPORT(erts_poll_destroy_pollset)
#define ERTS_CIO_POLL_MAX_FDS	ERTS_POLL_EXPORT(erts_poll_max_fds)
#define ERTS_CIO_POLL_INIT	ERTS_POLL_EXPORT(erts_poll_init)
#define ERTS_CIO_POLL_INFO	ERTS_POLL_EXPORT(erts_poll_info)

#define GET_FD(fd) fd

static struct pollset_info
{
    ErtsPollSet ps;
    erts_smp_atomic_t in_poll_wait;        /* set while doing poll */
    struct {
	int six; /* start index */
	int eix; /* end index */
	erts_smp_atomic32_t no;
	int size;
	ErtsSysFdType *array;
    } active_fd;
#ifdef ERTS_SMP
    struct removed_fd* removed_list;       /* list of deselected fd's*/
    erts_smp_spinlock_t removed_list_lock;
#endif
}pollset;
#define NUM_OF_POLLSETS 1

typedef struct {
#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    SafeHashBucket hb;
#endif
    ErtsSysFdType fd;
    struct {
	ErtsDrvSelectDataState *select;   /* ERTS_EV_TYPE_DRV_SEL */
#if ERTS_CIO_HAVE_DRV_EVENT
	ErtsDrvEventDataState *event;     /* ERTS_EV_TYPE_DRV_EV */
#endif
        ErtsNifSelectDataState *nif;      /* ERTS_EV_TYPE_NIF */
        union {
            erts_driver_t*  drv_ptr;    /* ERTS_EV_TYPE_STOP_USE */
            ErtsResource* resource;   /* ERTS_EV_TYPE_STOP_NIF */
        }stop;
    } driver;
    ErtsPollEvents events;
    unsigned short remove_cnt; /* number of removed_fd's referring to this fd */
    EventStateType type;
    EventStateFlags flags;
} ErtsDrvEventState;

#ifdef ERTS_SMP
struct removed_fd {
    struct removed_fd *next;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    ErtsSysFdType fd;
#else
    ErtsDrvEventState* state;
    #ifdef DEBUG
    ErtsSysFdType fd;
    #endif
#endif

};
#endif

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
static int max_fds = -1;
#endif
#define DRV_EV_STATE_LOCK_CNT 16
static union {
    erts_smp_mtx_t lck;
    byte _cache_line_alignment[64];
}drv_ev_state_locks[DRV_EV_STATE_LOCK_CNT];

#ifdef ERTS_SMP
static ERTS_INLINE erts_smp_mtx_t* fd_mtx(ErtsSysFdType fd)
{
    int hash = (int)fd;
# ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    hash ^= (hash >> 9);
# endif
    return &drv_ev_state_locks[hash % DRV_EV_STATE_LOCK_CNT].lck;
}
#else
#  define fd_mtx(fd) NULL
#endif

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS

static erts_smp_atomic_t drv_ev_state_len;
static ErtsDrvEventState *drv_ev_state;
static erts_smp_mtx_t drv_ev_state_grow_lock; /* prevent lock-hogging of racing growers */

#else
static SafeHash drv_ev_state_tab;
static int num_state_prealloc;
static ErtsDrvEventState *state_prealloc_first;
erts_smp_spinlock_t state_prealloc_lock;

static ERTS_INLINE ErtsDrvEventState *hash_get_drv_ev_state(ErtsSysFdType fd)
{
    ErtsDrvEventState tmpl;
    tmpl.fd = fd;
    return  (ErtsDrvEventState *) safe_hash_get(&drv_ev_state_tab, (void *) &tmpl);
}

static ERTS_INLINE ErtsDrvEventState* hash_new_drv_ev_state(ErtsSysFdType fd)
{
    ErtsDrvEventState tmpl;
    tmpl.fd = fd;
    tmpl.driver.select = NULL;
#if ERTS_CIO_HAVE_DRV_EVENT
    tmpl.driver.event = NULL;
#endif
    tmpl.driver.nif = NULL;
    tmpl.driver.stop.drv_ptr = NULL;
    tmpl.events = 0;
    tmpl.remove_cnt = 0;
    tmpl.type = ERTS_EV_TYPE_NONE;
    tmpl.flags = 0;
    return  (ErtsDrvEventState *) safe_hash_put(&drv_ev_state_tab, (void *) &tmpl);
}

static ERTS_INLINE void hash_erase_drv_ev_state(ErtsDrvEventState *state)
{
    ASSERT(state->remove_cnt == 0);
    safe_hash_erase(&drv_ev_state_tab, (void *) state);
}

#endif /* !ERTS_SYS_CONTINOUS_FD_NUMBERS */

static void stale_drv_select(Eterm id, ErtsDrvEventState *state, int mode);
static void drv_select_steal(ErlDrvPort ix, ErtsDrvEventState *state,
                             int mode, int on);
static void nif_select_steal(ErtsDrvEventState *state, int mode,
                             ErtsResource* resource, Eterm ref);

static void print_drv_select_op(erts_dsprintf_buf_t *dsbufp,
                                ErlDrvPort ix, ErtsSysFdType fd, int mode, int on);
static void print_nif_select_op(erts_dsprintf_buf_t*, ErtsSysFdType,
                                int mode, ErtsResource*, Eterm ref);

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
static void drv_select_large_fd_error(ErlDrvPort, ErtsSysFdType, int, int);
static void nif_select_large_fd_error(ErtsSysFdType, int, ErtsResource*,Eterm ref);
#endif
#if ERTS_CIO_HAVE_DRV_EVENT
static void drv_event_steal(ErlDrvPort ix, ErtsDrvEventState *state,
			ErlDrvEventData event_data);
static void print_drv_event_op(erts_dsprintf_buf_t *dsbufp,
                               ErlDrvPort, ErtsSysFdType, ErlDrvEventData);
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
static void event_large_fd_error(ErlDrvPort, ErtsSysFdType, ErlDrvEventData);
#endif
#endif
static void
steal_pending_stop_use(erts_dsprintf_buf_t*, ErlDrvPort, ErtsDrvEventState*,
                       int mode, int on);
static void
steal_pending_stop_nif(erts_dsprintf_buf_t *dsbufp, ErtsResource*,
                       ErtsDrvEventState *state, int mode, int on);

#ifdef ERTS_SMP
ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(removed_fd, struct removed_fd, 64, ERTS_ALC_T_FD_LIST)
#endif

static ERTS_INLINE void
init_iotask(ErtsIoTask *io_task)
{
    erts_port_task_handle_init(&io_task->task);
    erts_smp_atomic_init_nob(&io_task->executed_time, ~((erts_aint_t) 0));
}

static ERTS_INLINE int
is_iotask_active(ErtsIoTask *io_task, erts_aint_t current_cio_time)
{    
    if (erts_port_task_is_scheduled(&io_task->task))
	return 1;
    if (erts_smp_atomic_read_nob(&io_task->executed_time) == current_cio_time)
	return 1;
    return 0;
}

static ERTS_INLINE ErtsDrvSelectDataState *
alloc_drv_select_data(void)
{
    ErtsDrvSelectDataState *dsp = erts_alloc(ERTS_ALC_T_DRV_SEL_D_STATE,
					     sizeof(ErtsDrvSelectDataState));
    dsp->inport = NIL;
    dsp->outport = NIL;
    init_iotask(&dsp->iniotask);
    init_iotask(&dsp->outiotask);
    return dsp;
}

static ERTS_INLINE ErtsNifSelectDataState *
alloc_nif_select_data(void)
{
    ErtsNifSelectDataState *dsp = erts_alloc(ERTS_ALC_T_NIF_SEL_D_STATE,
					     sizeof(ErtsNifSelectDataState));
    dsp->in.pid = NIL;
    dsp->out.pid = NIL;
    dsp->in.ddeselect_cnt = 0;
    dsp->out.ddeselect_cnt = 0;
    return dsp;
}

static ERTS_INLINE void
free_drv_select_data(ErtsDrvSelectDataState *dsp)
{
    ASSERT(!erts_port_task_is_scheduled(&dsp->iniotask.task));
    ASSERT(!erts_port_task_is_scheduled(&dsp->outiotask.task));
    erts_free(ERTS_ALC_T_DRV_SEL_D_STATE, dsp);   
}

static ERTS_INLINE void
free_nif_select_data(ErtsNifSelectDataState *dsp)
{
    erts_free(ERTS_ALC_T_NIF_SEL_D_STATE, dsp);
}

#if ERTS_CIO_HAVE_DRV_EVENT

static ERTS_INLINE ErtsDrvEventDataState *
alloc_drv_event_data(void)
{
    ErtsDrvEventDataState *dep = erts_alloc(ERTS_ALC_T_DRV_EV_D_STATE,
					    sizeof(ErtsDrvEventDataState));
    dep->port = NIL;
    dep->data = NULL;
    dep->removed_events = 0;
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
    dep->deferred_events = 0;
#endif
    init_iotask(&dep->iotask);
    return dep;
}

static ERTS_INLINE void
free_drv_event_data(ErtsDrvEventDataState *dep)
{
    ASSERT(!erts_port_task_is_scheduled(&dep->iotask.task));
    erts_free(ERTS_ALC_T_DRV_EV_D_STATE, dep);   
}

#endif /* ERTS_CIO_HAVE_DRV_EVENT */

static ERTS_INLINE void
remember_removed(ErtsDrvEventState *state, struct pollset_info* psi)
{
#ifdef ERTS_SMP
    struct removed_fd *fdlp;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(fd_mtx(state->fd)));
    if (erts_smp_atomic_read_nob(&psi->in_poll_wait)) {
	state->remove_cnt++;
	ASSERT(state->remove_cnt > 0);
	fdlp = removed_fd_alloc();
    #if defined(ERTS_SYS_CONTINOUS_FD_NUMBERS) || defined(DEBUG)
    	fdlp->fd = state->fd;
    #endif
    #ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
	fdlp->state = state;
    #endif
	erts_smp_spin_lock(&psi->removed_list_lock);
	fdlp->next = psi->removed_list;
	psi->removed_list = fdlp;
	erts_smp_spin_unlock(&psi->removed_list_lock);
    }
#endif
}


static ERTS_INLINE int
is_removed(ErtsDrvEventState *state)
{
#ifdef ERTS_SMP
    /* Note that there is a possible race here, where an fd is removed
       (increasing remove_cnt) and then added again just before erts_poll_wait
       is called by erts_check_io. Any polled event on the re-added fd will then
       be falsely ignored. But that does not matter, as the event will trigger
       again next time erl_check_io is called. */
    return state->remove_cnt > 0;
#else
    return 0;
#endif
}

static void
forget_removed(struct pollset_info* psi)
{
#ifdef ERTS_SMP
    struct removed_fd* fdlp;
    struct removed_fd* tofree;

    /* Fast track: if (atomic_ptr(removed_list)==NULL) return; */

    erts_smp_spin_lock(&psi->removed_list_lock);
    fdlp = psi->removed_list;
    psi->removed_list = NULL;
    erts_smp_spin_unlock(&psi->removed_list_lock);

    while (fdlp) {
        ErtsResource* resource = NULL;
	erts_driver_t* drv_ptr = NULL;
	erts_smp_mtx_t* mtx;
	ErtsSysFdType fd;
	ErtsDrvEventState *state;

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
	fd = fdlp->fd;
	mtx = fd_mtx(fd);
	erts_smp_mtx_lock(mtx);
	state = &drv_ev_state[(int) fd];
#else
	state = fdlp->state;
	fd = state->fd;
	ASSERT(fd == fdlp->fd);
	mtx = fd_mtx(fd);
	erts_smp_mtx_lock(mtx);
#endif
	ASSERT(state->remove_cnt > 0);
	if (--state->remove_cnt == 0) {
	    switch (state->type) {
	    case ERTS_EV_TYPE_STOP_NIF:
		/* Now we can call stop */
		resource = state->driver.stop.resource;
                state->driver.stop.resource = NULL;
                ASSERT(resource);
		state->type = ERTS_EV_TYPE_NONE;
		state->flags &= ~ERTS_EV_FLAG_USED;
                goto case_ERTS_EV_TYPE_NONE;

	    case ERTS_EV_TYPE_STOP_USE:
		/* Now we can call stop_select */
		drv_ptr = state->driver.stop.drv_ptr;
		ASSERT(drv_ptr);
		state->type = ERTS_EV_TYPE_NONE;
		state->flags &= ~ERTS_EV_FLAG_USED;
		state->driver.stop.drv_ptr = NULL;
		/* Fall through */
            case ERTS_EV_TYPE_NONE:
            case_ERTS_EV_TYPE_NONE:
#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
		hash_erase_drv_ev_state(state);
#endif
		break;
	    case ERTS_EV_TYPE_DRV_SEL:
	    case ERTS_EV_TYPE_DRV_EV:
		break;
	    default:
		ASSERT(0);
	    }
	}
	erts_smp_mtx_unlock(mtx);
	if (drv_ptr) {
	    int was_unmasked = erts_block_fpe();
	    DTRACE1(driver_stop_select, drv_ptr->name);
	    LTTNG1(driver_stop_select, drv_ptr->name);
	    (*drv_ptr->stop_select) ((ErlDrvEvent) fd, NULL);
	    erts_unblock_fpe(was_unmasked);
	    if (drv_ptr->handle) {
		erts_ddll_dereference_driver(drv_ptr->handle);
	    }
	}
        if (resource) {
            erts_resource_stop(resource, (ErlNifEvent)fd, 0);
            enif_release_resource(resource->data);
        }

	tofree = fdlp;
	fdlp = fdlp->next;
	removed_fd_free(tofree);
    }
#endif /* ERTS_SMP */
}

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
static void
grow_drv_ev_state(int min_ix)
{
    int i;
    int old_len;
    int new_len;

    erts_smp_mtx_lock(&drv_ev_state_grow_lock);
    old_len = erts_smp_atomic_read_nob(&drv_ev_state_len);
    if (min_ix >= old_len) {
        new_len = erts_poll_new_table_len(old_len, min_ix + 1);
        if (new_len > max_fds)
            new_len = max_fds;

	for (i=0; i<DRV_EV_STATE_LOCK_CNT; i++) { /* lock all fd's */
	    erts_smp_mtx_lock(&drv_ev_state_locks[i].lck);
	}
	drv_ev_state = (drv_ev_state
			? erts_realloc(ERTS_ALC_T_DRV_EV_STATE,
				       drv_ev_state,
				       sizeof(ErtsDrvEventState)*new_len)
			: erts_alloc(ERTS_ALC_T_DRV_EV_STATE,
				     sizeof(ErtsDrvEventState)*new_len));
	for (i = old_len; i < new_len; i++) {
	    drv_ev_state[i].fd = (ErtsSysFdType) i;
	    drv_ev_state[i].driver.select = NULL;
#if ERTS_CIO_HAVE_DRV_EVENT
	    drv_ev_state[i].driver.event = NULL;
#endif
	    drv_ev_state[i].driver.stop.drv_ptr = NULL;
            drv_ev_state[i].driver.nif = NULL;
	    drv_ev_state[i].events = 0;
	    drv_ev_state[i].remove_cnt = 0;
	    drv_ev_state[i].type = ERTS_EV_TYPE_NONE;
	    drv_ev_state[i].flags = 0;
	}
	erts_smp_atomic_set_nob(&drv_ev_state_len, new_len);
	for (i=0; i<DRV_EV_STATE_LOCK_CNT; i++) {
	    erts_smp_mtx_unlock(&drv_ev_state_locks[i].lck);
	}
    }
    /*else already grown by racing thread */

    erts_smp_mtx_unlock(&drv_ev_state_grow_lock);
}
#endif /* ERTS_SYS_CONTINOUS_FD_NUMBERS */


static ERTS_INLINE void
abort_task(Eterm id, ErtsPortTaskHandle *pthp, EventStateType type)
{
    if (is_not_nil(id) && erts_port_task_is_scheduled(pthp)) {
	erts_port_task_abort(pthp);
	ASSERT(erts_is_port_alive(id));
    }
}

static ERTS_INLINE void
abort_tasks(ErtsDrvEventState *state, int mode)
{
    switch (mode) {
    case 0: check_type:
	switch (state->type) {
#if ERTS_CIO_HAVE_DRV_EVENT
	case ERTS_EV_TYPE_DRV_EV:
	    abort_task(state->driver.event->port,
		       &state->driver.event->iotask.task,
		       ERTS_EV_TYPE_DRV_EV);
	    return;
#endif
        case ERTS_EV_TYPE_NIF:
	case ERTS_EV_TYPE_NONE:
	    return;
	default:
	    ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL);
	    /* Fall through */
	}
    case ERL_DRV_READ|ERL_DRV_WRITE:
    case ERL_DRV_WRITE:
	ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL);
	abort_task(state->driver.select->outport,
		   &state->driver.select->outiotask.task,
		   state->type);
	if (mode == ERL_DRV_WRITE)
	    break;
    case ERL_DRV_READ:
	ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL);
	abort_task(state->driver.select->inport,
		   &state->driver.select->iniotask.task,
		   state->type);
	break;
    default:
	goto check_type;
    }
}

static void
deselect(ErtsDrvEventState *state, int mode)
{
    int do_wake = 0;
    ErtsPollEvents rm_events;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(fd_mtx(state->fd)));
    ASSERT(state->events);

    abort_tasks(state, mode);

    if (!mode)
	rm_events = state->events;
    else {
	rm_events = 0;
	ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL);
	if (mode & ERL_DRV_READ) {
	    state->driver.select->inport = NIL;
	    rm_events |= ERTS_POLL_EV_IN;
	}
	if (mode & ERL_DRV_WRITE) {
	    state->driver.select->outport = NIL;
	    rm_events |= ERTS_POLL_EV_OUT;
	}
    }

    state->events = ERTS_CIO_POLL_CTL(pollset.ps, state->fd, rm_events, 0, &do_wake);

    if (!(state->events)) {
	switch (state->type) {
        case ERTS_EV_TYPE_NIF:
            state->driver.nif->in.pid = NIL;
            state->driver.nif->out.pid = NIL;
            state->driver.nif->in.ddeselect_cnt = 0;
            state->driver.nif->out.ddeselect_cnt = 0;
            enif_release_resource(state->driver.stop.resource);
            state->driver.stop.resource = NULL;
            break;
	case ERTS_EV_TYPE_DRV_SEL:
	    state->driver.select->inport = NIL;
	    state->driver.select->outport = NIL;
	    break;
#if ERTS_CIO_HAVE_DRV_EVENT
	case ERTS_EV_TYPE_DRV_EV:
	    state->driver.event->port = NIL;
	    state->driver.event->data = NULL;
	    state->driver.event->removed_events = (ErtsPollEvents) 0;
	    break;
#endif
	case ERTS_EV_TYPE_NONE:
	    break;
	default:
	    ASSERT(0);
	    break;
	}
	    
	state->type = ERTS_EV_TYPE_NONE;
	state->flags &= ~ERTS_EV_FLAG_USED;
	remember_removed(state, &pollset);
    }
}

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
#  define IS_FD_UNKNOWN(state) ((state)->type == ERTS_EV_TYPE_NONE && (state)->remove_cnt == 0)
#else
#  define IS_FD_UNKNOWN(state) ((state) == NULL)
#endif

static ERTS_INLINE void
check_fd_cleanup(ErtsDrvEventState *state,
#if ERTS_CIO_HAVE_DRV_EVENT
		 ErtsDrvEventDataState **free_event,
#endif
		 ErtsDrvSelectDataState **free_select,
                 ErtsNifSelectDataState **free_nif)
{
    erts_aint_t current_cio_time;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(fd_mtx(state->fd)));

    current_cio_time = erts_smp_atomic_read_acqb(&erts_check_io_time);
    *free_select = NULL;
    if (state->driver.select
	&& (state->type != ERTS_EV_TYPE_DRV_SEL)
	&& !is_iotask_active(&state->driver.select->iniotask, current_cio_time)
	&& !is_iotask_active(&state->driver.select->outiotask, current_cio_time)) {
	
	*free_select = state->driver.select;
	state->driver.select = NULL;
    }

    *free_nif = NULL;
    if (state->driver.nif && (state->type != ERTS_EV_TYPE_NIF)) {
        *free_nif = state->driver.nif;
        state->driver.nif = NULL;
    }

#if ERTS_CIO_HAVE_DRV_EVENT
    *free_event = NULL;
    if (state->driver.event
	&& (state->type != ERTS_EV_TYPE_DRV_EV)
	&& !is_iotask_active(&state->driver.event->iotask,  current_cio_time)) {
	
	*free_event = state->driver.event;
	state->driver.event = NULL;
    }
#endif

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (((state->type != ERTS_EV_TYPE_NONE)
	 | state->remove_cnt
#if ERTS_CIO_HAVE_DRV_EVENT
	 | (state->driver.event != NULL)
#endif
	 | (state->driver.select != NULL)) == 0) {

	hash_erase_drv_ev_state(state);

    }
#endif
}

static ERTS_INLINE int
check_cleanup_active_fd(ErtsSysFdType fd,
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
			ErtsPollControlEntry *pce,
			int *pce_ix,
#endif
			erts_aint_t current_cio_time,
                        int may_sleep)
{
    ErtsDrvEventState *state;
    int active = 0;
    erts_smp_mtx_t *mtx = fd_mtx(fd);
    void *free_select = NULL;
    void *free_nif = NULL;
#if ERTS_CIO_HAVE_DRV_EVENT
    void *free_event = NULL;
#endif
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
    ErtsPollEvents evon = 0, evoff = 0;
#endif

    erts_smp_mtx_lock(mtx);

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    state = &drv_ev_state[(int) fd];
#else
    state = hash_get_drv_ev_state(fd); /* may be NULL! */
    if (state)
#endif
    {
	if (state->driver.select) {
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
	    if (is_iotask_active(&state->driver.select->iniotask, current_cio_time)) {
		active = 1;
		if ((state->events & ERTS_POLL_EV_IN)
		    && !(state->flags & ERTS_EV_FLAG_DEFER_IN_EV)) {
		    evoff |= ERTS_POLL_EV_IN;
		    state->flags |= ERTS_EV_FLAG_DEFER_IN_EV;
		}
	    }
	    else if (state->flags & ERTS_EV_FLAG_DEFER_IN_EV) {
		if (state->events & ERTS_POLL_EV_IN)
		    evon |= ERTS_POLL_EV_IN;
		state->flags &= ~ERTS_EV_FLAG_DEFER_IN_EV;
	    }
	    if (is_iotask_active(&state->driver.select->outiotask, current_cio_time)) {
		active = 1;
		if ((state->events & ERTS_POLL_EV_OUT)
		    && !(state->flags & ERTS_EV_FLAG_DEFER_OUT_EV)) {
		    evoff |= ERTS_POLL_EV_OUT;
		    state->flags |= ERTS_EV_FLAG_DEFER_OUT_EV;
		}
	    }
	    else if (state->flags & ERTS_EV_FLAG_DEFER_OUT_EV) {
		if (state->events & ERTS_POLL_EV_OUT)
		    evon |= ERTS_POLL_EV_OUT;
		state->flags &= ~ERTS_EV_FLAG_DEFER_OUT_EV;
	    }
	    if (active)
		(void) 0;
	    else
#else
	    if (is_iotask_active(&state->driver.select->iniotask, current_cio_time)
		|| is_iotask_active(&state->driver.select->outiotask, current_cio_time))
		active = 1;
	    else
#endif
		if (state->type != ERTS_EV_TYPE_DRV_SEL) {
		free_select = state->driver.select;
		state->driver.select = NULL;
	    }
	}

        if (state->driver.nif) {
            ErtsPollEvents rm_events = 0;
            if (state->driver.nif->in.ddeselect_cnt) {
                ASSERT(state->type == ERTS_EV_TYPE_NIF);
                ASSERT(state->events & ERTS_POLL_EV_IN);
                ASSERT(is_nil(state->driver.nif->in.pid));
                if (may_sleep || state->driver.nif->in.ddeselect_cnt == 1) {
                    rm_events = ERTS_POLL_EV_IN;
                    state->driver.nif->in.ddeselect_cnt = 0;
                }
            }
            if (state->driver.nif->out.ddeselect_cnt) {
                ASSERT(state->type == ERTS_EV_TYPE_NIF);
                ASSERT(state->events & ERTS_POLL_EV_OUT);
                ASSERT(is_nil(state->driver.nif->out.pid));
                if (may_sleep || state->driver.nif->out.ddeselect_cnt == 1) {
                    rm_events |= ERTS_POLL_EV_OUT;
                    state->driver.nif->out.ddeselect_cnt = 0;
                }
            }
            if (rm_events) {
                int do_wake = 0;
                state->events = ERTS_CIO_POLL_CTL(pollset.ps, state->fd,
                                                  rm_events, 0, &do_wake);
            }
            if (state->events)
                active = 1;
            else if (state->type != ERTS_EV_TYPE_NIF) {
                free_nif = state->driver.nif;
                state->driver.nif = NULL;
            }
        }

#if ERTS_CIO_HAVE_DRV_EVENT
	if (state->driver.event) {
	    if (is_iotask_active(&state->driver.event->iotask, current_cio_time)) {
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
		ErtsPollEvents evs = state->events & ~state->driver.event->deferred_events;
		if (evs) {
		    evoff |= evs;
		    state->driver.event->deferred_events |= evs;
		}
#endif
		active = 1;
	    }
	    else if (state->type != ERTS_EV_TYPE_DRV_EV) {
		free_event = state->driver.event;
		state->driver.event = NULL;
	    }
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
	    else {
		ErtsPollEvents evs = state->events & state->driver.event->deferred_events;
		if (evs) {
		    evon |= evs;
		    state->driver.event->deferred_events = 0;
		}
	    }
#endif

	}
#endif

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
	if (((state->type != ERTS_EV_TYPE_NONE) | state->remove_cnt | active) == 0)
	    hash_erase_drv_ev_state(state);
#endif

    }

    erts_smp_mtx_unlock(mtx);

    if (free_select)
	free_drv_select_data(free_select);
    if (free_nif)
        free_nif_select_data(free_nif);
#if ERTS_CIO_HAVE_DRV_EVENT
    if (free_event)
	free_drv_event_data(free_event);
#endif

#if ERTS_CIO_DEFER_ACTIVE_EVENTS
    if (evoff) {
	ErtsPollControlEntry *pcep = &pce[(*pce_ix)++];
	pcep->fd = fd;
	pcep->events = evoff;
	pcep->on = 0;
    }
    if (evon) {
	ErtsPollControlEntry *pcep = &pce[(*pce_ix)++];
	pcep->fd = fd;
	pcep->events = evon;
	pcep->on = 1;
    }
#endif

    return active;
}

static void
check_cleanup_active_fds(erts_aint_t current_cio_time, int may_sleep)
{
    int six = pollset.active_fd.six;
    int eix = pollset.active_fd.eix;
    erts_aint32_t no = erts_smp_atomic32_read_dirty(&pollset.active_fd.no);
    int size = pollset.active_fd.size;
    int ix = six;
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
    /* every fd might add two entries */
    Uint pce_sz = 2*sizeof(ErtsPollControlEntry)*no;
    ErtsPollControlEntry *pctrl_entries = (pce_sz
					   ? erts_alloc(ERTS_ALC_T_TMP, pce_sz)
					   : NULL);
    int pctrl_ix = 0;
#endif

    while (ix != eix) {
	ErtsSysFdType fd = pollset.active_fd.array[ix];
	int nix = ix + 1;
	if (nix >= size)
	    nix = 0;
	ASSERT(fd != ERTS_SYS_FD_INVALID);
	if (!check_cleanup_active_fd(fd,
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
				     pctrl_entries,
				     &pctrl_ix,
#endif
				     current_cio_time,
                                     may_sleep)) {
	    no--;
	    if (ix == six) {
#ifdef DEBUG
		pollset.active_fd.array[ix] = ERTS_SYS_FD_INVALID;
#endif
		six = nix;
	    }
	    else {
		pollset.active_fd.array[ix] = pollset.active_fd.array[six];
#ifdef DEBUG
		pollset.active_fd.array[six] = ERTS_SYS_FD_INVALID;
#endif
		six++;
		if (six >= size)
		    six = 0;
	    }
	}
	ix = nix;
    }

#if ERTS_CIO_DEFER_ACTIVE_EVENTS
    ASSERT(pctrl_ix <= pce_sz/sizeof(ErtsPollControlEntry));
    if (pctrl_ix)
	ERTS_CIO_POLL_CTLV(pollset.ps, pctrl_entries, pctrl_ix);
    if (pctrl_entries)
	erts_free(ERTS_ALC_T_TMP, pctrl_entries);
#endif

    pollset.active_fd.six = six;
    pollset.active_fd.eix = eix;
    erts_smp_atomic32_set_relb(&pollset.active_fd.no, no);
}

static void grow_active_fds(void)
{
    ASSERT(pollset.active_fd.six == pollset.active_fd.eix);
    pollset.active_fd.six = 0;
    pollset.active_fd.eix = pollset.active_fd.size;
    pollset.active_fd.size += ERTS_ACTIVE_FD_INC;
    pollset.active_fd.array = erts_realloc(ERTS_ALC_T_ACTIVE_FD_ARR,
                                           pollset.active_fd.array,
                                           pollset.active_fd.size*sizeof(ErtsSysFdType));
#ifdef DEBUG
    {
        int i;
        for (i = pollset.active_fd.eix + 1; i < pollset.active_fd.size; i++)
            pollset.active_fd.array[i] = ERTS_SYS_FD_INVALID;
    }
#endif
}

static ERTS_INLINE void
add_active_fd(ErtsSysFdType fd)
{
    int eix = pollset.active_fd.eix;
    int size = pollset.active_fd.size;
    
    pollset.active_fd.array[eix] = fd;

    erts_smp_atomic32_set_relb(&pollset.active_fd.no,
			       (erts_smp_atomic32_read_dirty(&pollset.active_fd.no)
				+ 1));

    eix++;
    if (eix >= size)
	eix = 0;
    pollset.active_fd.eix = eix;

    if (pollset.active_fd.six == eix) {
        grow_active_fds();
    }
}

int
ERTS_CIO_EXPORT(driver_select)(ErlDrvPort ix,
			       ErlDrvEvent e,
			       int mode,
			       int on)
{
    void (*stop_select_fn)(ErlDrvEvent, void*) = NULL;
    Port *prt = erts_drvport2port(ix);
    Eterm id = erts_drvport2id(ix);
    ErtsSysFdType fd = (ErtsSysFdType) e;
    ErtsPollEvents ctl_events = (ErtsPollEvents) 0;
    ErtsPollEvents new_events, old_events;
    ErtsDrvEventState *state;
    int wake_poller;
    int ret;
#if ERTS_CIO_HAVE_DRV_EVENT
    ErtsDrvEventDataState *free_event = NULL;
#endif
    ErtsDrvSelectDataState *free_select = NULL;
    ErtsNifSelectDataState *free_nif = NULL;
#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(name, 64);
#endif

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if ((unsigned)fd >= (unsigned)erts_smp_atomic_read_nob(&drv_ev_state_len)) {
	if (fd < 0) {
	    return -1;    
	}
	if (fd >= max_fds) {
	    drv_select_large_fd_error(ix, fd, mode, on);
	    return -1;
	}
	grow_drv_ev_state(fd);
    }
#endif

    erts_smp_mtx_lock(fd_mtx(fd));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    state = &drv_ev_state[(int) fd];
#else
    state = hash_get_drv_ev_state(fd); /* may be NULL! */
#endif

    if (!on && (mode&ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE) {
	if (IS_FD_UNKNOWN(state)) {
	    /* fast track to stop_select callback */
	    stop_select_fn = prt->drv_ptr->stop_select;
#ifdef USE_VM_PROBES
	    strncpy(name, prt->drv_ptr->name,
		    sizeof(DTRACE_CHARBUF_NAME(name))-1);
	    name[sizeof(name)-1] = '\0';
#endif
	    ret = 0;
	    goto done_unknown;
	}
	mode |= (ERL_DRV_READ | ERL_DRV_WRITE);
	wake_poller = 1; /* to eject fd from pollset (if needed) */
    }
    else wake_poller = 0;

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (state == NULL) {
	state = hash_new_drv_ev_state(fd);
    }
#endif

    switch (state->type) {
#if ERTS_CIO_HAVE_DRV_EVENT
    case ERTS_EV_TYPE_DRV_EV:
#endif
    case ERTS_EV_TYPE_NIF:
        drv_select_steal(ix, state, mode, on);
        break;
    case ERTS_EV_TYPE_STOP_USE: {
        erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
        print_drv_select_op(dsbufp, ix, state->fd, mode, on);
        steal_pending_stop_use(dsbufp, ix, state, mode, on);
        if (state->type == ERTS_EV_TYPE_STOP_USE) {
            ret = 0;
            goto done; /* stop_select still pending */
        }
        ASSERT(state->type == ERTS_EV_TYPE_NONE);
        break;
    }
    case ERTS_EV_TYPE_STOP_NIF: {
        erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
        print_drv_select_op(dsbufp, ix, state->fd, mode, on);
        steal_pending_stop_nif(dsbufp, NULL, state, mode, on);
        ASSERT(state->type == ERTS_EV_TYPE_NONE);
        break;

    }}

    if (mode & ERL_DRV_READ) {
	if (state->type == ERTS_EV_TYPE_DRV_SEL) {
	    Eterm owner = state->driver.select->inport;
	    if (owner != id && is_not_nil(owner))
		drv_select_steal(ix, state, mode, on);
	}
	ctl_events |= ERTS_POLL_EV_IN;
    }
    if (mode & ERL_DRV_WRITE) {
	if (state->type == ERTS_EV_TYPE_DRV_SEL) {
	    Eterm owner = state->driver.select->outport;
	    if (owner != id && is_not_nil(owner))
		drv_select_steal(ix, state, mode, on);
	}
	ctl_events |= ERTS_POLL_EV_OUT;
    }	

    ASSERT((state->type == ERTS_EV_TYPE_DRV_SEL) ||
	   (state->type == ERTS_EV_TYPE_NONE && !state->events));

    if (!on && !(state->flags & ERTS_EV_FLAG_USED) 
	&& state->events && !(state->events & ~ctl_events)) {	
	/* Old driver removing all events. At least wake poller.
	   It will not make close() 100% safe but it will prevent
	   actions delayed by poll timeout. */
	wake_poller = 1;
    }

    new_events = ERTS_CIO_POLL_CTL(pollset.ps, state->fd, ctl_events, on, &wake_poller);

    if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
	if (state->type == ERTS_EV_TYPE_DRV_SEL && !state->events) {
	    state->type = ERTS_EV_TYPE_NONE;
	    state->flags &= ~ERTS_EV_FLAG_USED;
	    state->driver.select->inport = NIL;
	    state->driver.select->outport = NIL;
	}
	ret = -1;
	goto done;
    }

    old_events = state->events;

    ASSERT(on
	   ? (new_events == (state->events | ctl_events))
	   : (new_events == (state->events & ~ctl_events)));

    ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL
	   || state->type == ERTS_EV_TYPE_NONE);

    state->events = new_events;
    if (ctl_events) {
	if (on) {
	    if (!state->driver.select)
		state->driver.select = alloc_drv_select_data();
	    if (state->type == ERTS_EV_TYPE_NONE)
		state->type = ERTS_EV_TYPE_DRV_SEL;
	    ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL);
	    if (ctl_events & ERTS_POLL_EV_IN)
		state->driver.select->inport = id;
	    if (ctl_events & ERTS_POLL_EV_OUT)
		state->driver.select->outport = id;
	    if (mode & ERL_DRV_USE) {
		state->flags |= ERTS_EV_FLAG_USED;
	    }
	}
	else { /* off */
	    if (state->type == ERTS_EV_TYPE_DRV_SEL) {
		if (ctl_events & ERTS_POLL_EV_IN) {
		    abort_tasks(state, ERL_DRV_READ);
		    state->driver.select->inport = NIL;
		}
		if (ctl_events & ERTS_POLL_EV_OUT) {
		    abort_tasks(state, ERL_DRV_WRITE);
		    state->driver.select->outport = NIL;
		}
		if (new_events == 0) {
		    if (old_events != 0) {
			remember_removed(state, &pollset);
		    }		    
		    if ((mode & ERL_DRV_USE) || !(state->flags & ERTS_EV_FLAG_USED)) {
			state->type = ERTS_EV_TYPE_NONE;
			state->flags &= ~ERTS_EV_FLAG_USED;
		    }
		    /*else keep it, as fd will probably be selected upon again */
		}
	    }
	    if ((mode & ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE) {
		erts_driver_t* drv_ptr = prt->drv_ptr;
		ASSERT(new_events==0);
		if (state->remove_cnt == 0 || !wake_poller) {
		    /* Safe to close fd now as it is not in pollset
		       or there was no need to eject fd (kernel poll) */
		    stop_select_fn = drv_ptr->stop_select;
#ifdef USE_VM_PROBES
		    strncpy(name, prt->drv_ptr->name, sizeof(name)-1);
		    name[sizeof(name)-1] = '\0';
#endif
		}
		else {
		    /* Not safe to close fd, postpone stop_select callback. */
		    state->type = ERTS_EV_TYPE_STOP_USE;
		    state->driver.stop.drv_ptr = drv_ptr;
		    if (drv_ptr->handle) {
			erts_ddll_reference_referenced_driver(drv_ptr->handle);
		    }
		}
	    }
	}
    }
   
    ret = 0;

done:

    check_fd_cleanup(state,
#if ERTS_CIO_HAVE_DRV_EVENT
		     &free_event,
#endif
		     &free_select,
                     &free_nif);

done_unknown:
    erts_smp_mtx_unlock(fd_mtx(fd));
    if (stop_select_fn) {
	int was_unmasked = erts_block_fpe();
	DTRACE1(driver_stop_select, name);
	LTTNG1(driver_stop_select, "unknown");
	(*stop_select_fn)(e, NULL);
	erts_unblock_fpe(was_unmasked);
    }
    if (free_select)
	free_drv_select_data(free_select);
    if (free_nif)
        free_nif_select_data(free_nif);

#if ERTS_CIO_HAVE_DRV_EVENT
    if (free_event)
	free_drv_event_data(free_event);
#endif
    return ret;
}

int
ERTS_CIO_EXPORT(enif_select)(ErlNifEnv* env,
                             ErlNifEvent e,
                             enum ErlNifSelectFlags mode,
                             void* obj,
                             const ErlNifPid* pid,
                             Eterm ref)
{
    int on;
    ErtsResource* resource = DATA_TO_RESOURCE(obj);
    ErtsSysFdType fd = (ErtsSysFdType) e;
    ErtsPollEvents ctl_events = (ErtsPollEvents) 0;
    ErtsPollEvents new_events, old_events;
    ErtsDrvEventState *state;
    int wake_poller;
    int ret;
    enum { NO_STOP=0, CALL_STOP, CALL_STOP_AND_RELEASE } call_stop = NO_STOP;
#if ERTS_CIO_HAVE_DRV_EVENT
    ErtsDrvEventDataState *free_event = NULL;
#endif
    ErtsDrvSelectDataState *free_select = NULL;
    ErtsNifSelectDataState *free_nif = NULL;
#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(name, 64);
#endif

    ASSERT(!(resource->monitors && resource->monitors->is_dying));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if ((unsigned)fd >= (unsigned)erts_smp_atomic_read_nob(&drv_ev_state_len)) {
	if (fd < 0) {
	    return INT_MIN | ERL_NIF_SELECT_INVALID_EVENT;
	}
	if (fd >= max_fds) {
	    nif_select_large_fd_error(fd, mode, resource, ref);
            return INT_MIN | ERL_NIF_SELECT_INVALID_EVENT;
	}
	grow_drv_ev_state(fd);
    }
#endif

    erts_smp_mtx_lock(fd_mtx(fd));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    state = &drv_ev_state[(int) fd];
#else
    state = hash_get_drv_ev_state(fd); /* may be NULL! */
#endif

    if (mode & ERL_NIF_SELECT_STOP) {
        ASSERT(resource->type->stop);
        if (IS_FD_UNKNOWN(state)) {
            /* fast track to stop callback */
            call_stop = CALL_STOP;
            ret = ERL_NIF_SELECT_STOP_CALLED;
            goto done_unknown;
        }
        on = 0;
        mode = ERL_DRV_READ | ERL_DRV_WRITE | ERL_DRV_USE;
        wake_poller = 1; /* to eject fd from pollset (if needed) */
        ctl_events = ERTS_POLL_EV_IN | ERTS_POLL_EV_OUT;
    }
    else {
        on = 1;
        ASSERT(mode);
        wake_poller = 0;
        if (mode & ERL_DRV_READ) {
            ctl_events |= ERTS_POLL_EV_IN;
        }
        if (mode & ERL_DRV_WRITE) {
            ctl_events |= ERTS_POLL_EV_OUT;
        }
    }

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (state == NULL) {
	state = hash_new_drv_ev_state(fd);
    }
#endif

    switch (state->type) {
    case ERTS_EV_TYPE_NIF:
        /*
         * Changing resource is considered stealing.
         * Changing process and/or ref is ok (I think?).
         */
        if (state->driver.stop.resource != resource)
            nif_select_steal(state, ERL_DRV_READ | ERL_DRV_WRITE, resource, ref);
        break;
#if ERTS_CIO_HAVE_DRV_EVENT
    case ERTS_EV_TYPE_DRV_EV:
#endif
    case ERTS_EV_TYPE_DRV_SEL:
        nif_select_steal(state, mode, resource, ref);
        break;
    case ERTS_EV_TYPE_STOP_USE: {
        erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
        print_nif_select_op(dsbufp, fd, mode, resource, ref);
        steal_pending_stop_use(dsbufp, ERTS_INVALID_ERL_DRV_PORT, state, mode, on);
        ASSERT(state->type == ERTS_EV_TYPE_NONE);
        break;
    }
    case ERTS_EV_TYPE_STOP_NIF: {
        erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
        print_nif_select_op(dsbufp, fd, mode, resource, ref);
        steal_pending_stop_nif(dsbufp, resource, state, mode, on);
        if (state->type == ERTS_EV_TYPE_STOP_NIF) {
            ret = ERL_NIF_SELECT_STOP_SCHEDULED;  /* ?? */
            goto done;
        }
        ASSERT(state->type == ERTS_EV_TYPE_NONE);
        break;
    }}

    ASSERT((state->type == ERTS_EV_TYPE_NIF) ||
	   (state->type == ERTS_EV_TYPE_NONE && !state->events));

    new_events = ERTS_CIO_POLL_CTL(pollset.ps, state->fd, ctl_events, on, &wake_poller);

    if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
	if (state->type == ERTS_EV_TYPE_NIF && !state->events) {
	    state->type = ERTS_EV_TYPE_NONE;
	    state->flags &= ~ERTS_EV_FLAG_USED;
	    state->driver.nif->in.pid = NIL;
	    state->driver.nif->out.pid = NIL;
            state->driver.nif->in.ddeselect_cnt = 0;
            state->driver.nif->out.ddeselect_cnt = 0;
            state->driver.stop.resource = NULL;
	}
	ret = INT_MIN | ERL_NIF_SELECT_FAILED;
	goto done;
    }

    old_events = state->events;

    ASSERT(on
	   ? (new_events == (state->events | ctl_events))
	   : (new_events == (state->events & ~ctl_events)));

    ASSERT(state->type == ERTS_EV_TYPE_NIF
	   || state->type == ERTS_EV_TYPE_NONE);

    state->events = new_events;
    if (on) {
        const Eterm recipient = pid ? pid->pid : env->proc->common.id;
        Uint32* refn;
        if (!state->driver.nif)
            state->driver.nif = alloc_nif_select_data();
        if (state->type == ERTS_EV_TYPE_NONE) {
            state->type = ERTS_EV_TYPE_NIF;
            state->driver.stop.resource = resource;
            enif_keep_resource(resource->data);
        }
        ASSERT(state->type == ERTS_EV_TYPE_NIF);
        ASSERT(state->driver.stop.resource == resource);
        if (ctl_events & ERTS_POLL_EV_IN) {
            state->driver.nif->in.pid = recipient;
            if (is_immed(ref)) {
                state->driver.nif->in.immed = ref;
            } else {
                ASSERT(is_internal_ref(ref));
                refn = internal_ref_numbers(ref);
                state->driver.nif->in.immed = THE_NON_VALUE;
                state->driver.nif->in.refn[0] = refn[0];
                state->driver.nif->in.refn[1] = refn[1];
                state->driver.nif->in.refn[2] = refn[2];
            }
            state->driver.nif->in.ddeselect_cnt = 0;
        }
        if (ctl_events & ERTS_POLL_EV_OUT) {
            state->driver.nif->out.pid = recipient;
            if (is_immed(ref)) {
                state->driver.nif->out.immed = ref;
            } else {
                ASSERT(is_internal_ref(ref));
                refn = internal_ref_numbers(ref);
                state->driver.nif->out.immed = THE_NON_VALUE;
                state->driver.nif->out.refn[0] = refn[0];
                state->driver.nif->out.refn[1] = refn[1];
                state->driver.nif->out.refn[2] = refn[2];
            }
            state->driver.nif->out.ddeselect_cnt = 0;
        }
        ret = 0;
    }
    else { /* off */
        if (state->type == ERTS_EV_TYPE_NIF) {
            state->driver.nif->in.pid = NIL;
            state->driver.nif->out.pid = NIL;
            state->driver.nif->in.ddeselect_cnt = 0;
            state->driver.nif->out.ddeselect_cnt = 0;
            if (old_events != 0) {
                remember_removed(state, &pollset);
            }
        }
        ASSERT(new_events==0);
        if (state->remove_cnt == 0 || !wake_poller) {
            /*
             * Safe to close fd now as it is not in pollset
             * or there was no need to eject fd (kernel poll)
             */
            if (state->type == ERTS_EV_TYPE_NIF) {
                ASSERT(state->driver.stop.resource == resource);
                call_stop = CALL_STOP_AND_RELEASE;
                state->driver.stop.resource = NULL;
            }
            else {
                ASSERT(!state->driver.stop.resource);
                call_stop = CALL_STOP;
            }
            state->type = ERTS_EV_TYPE_NONE;
            ret = ERL_NIF_SELECT_STOP_CALLED;
        }
        else {
            /* Not safe to close fd, postpone stop_select callback. */
            if (state->type == ERTS_EV_TYPE_NONE) {
                ASSERT(!state->driver.stop.resource);
                state->driver.stop.resource = resource;
                enif_keep_resource(resource);
            }
            state->type = ERTS_EV_TYPE_STOP_NIF;
            ret = ERL_NIF_SELECT_STOP_SCHEDULED;
        }
    }

done:

    check_fd_cleanup(state,
#if ERTS_CIO_HAVE_DRV_EVENT
		     &free_event,
#endif
		     &free_select,
                     &free_nif);

done_unknown:
    erts_smp_mtx_unlock(fd_mtx(fd));
    if (call_stop) {
        erts_resource_stop(resource, (ErlNifEvent)fd, 1);
        if (call_stop == CALL_STOP_AND_RELEASE) {
            enif_release_resource(resource->data);
        }
    }
    if (free_select)
	free_drv_select_data(free_select);
    if (free_nif)
        free_nif_select_data(free_nif);

#if ERTS_CIO_HAVE_DRV_EVENT
    if (free_event)
	free_drv_event_data(free_event);
#endif
    return ret;
}


int
ERTS_CIO_EXPORT(driver_event)(ErlDrvPort ix,
			      ErlDrvEvent e,
			      ErlDrvEventData event_data)
{
#if !ERTS_CIO_HAVE_DRV_EVENT
    return -1;
#else
    ErtsSysFdType fd = (ErtsSysFdType) e;
    ErtsPollEvents events;
    ErtsPollEvents add_events;
    ErtsPollEvents remove_events;
    Eterm id = erts_drvport2id(ix);
    ErtsDrvEventState *state;
    int do_wake = 0;
    int ret;
#if ERTS_CIO_HAVE_DRV_EVENT
    ErtsDrvEventDataState *free_event;
#endif
    ErtsDrvSelectDataState *free_select;
    ErtsNifSelectDataState *free_nif;
    Port *prt = erts_drvport2port(ix);

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if ((unsigned)fd >= (unsigned)erts_smp_atomic_read_nob(&drv_ev_state_len)) {
	if (fd < 0)
	    return -1;    
	if (fd >= max_fds) {
	    event_large_fd_error(ix, fd, event_data);
	    return -1;
	}
	grow_drv_ev_state(fd);
    }
#endif

    erts_smp_mtx_lock(fd_mtx(fd));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    state = &drv_ev_state[(int) fd];
#else
    /* Could use hash_new directly, but want to keep the normal case fast */
    state = hash_get_drv_ev_state(fd);
    if (state == NULL) {
	state = hash_new_drv_ev_state(fd);
    }
#endif

    switch (state->type) {
    case ERTS_EV_TYPE_DRV_EV:
	if (state->driver.event->port == id) break;
	/*fall through*/
    case ERTS_EV_TYPE_DRV_SEL:
	drv_event_steal(ix, state, event_data);
	break;
    case ERTS_EV_TYPE_STOP_USE: {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	print_drv_event_op(dsbufp, ix, fd, event_data);
	steal_pending_stop_use(dsbufp, ix, state, 0, 1);
	break;
      }
    }

    ASSERT(state->type == ERTS_EV_TYPE_DRV_EV
	   || state->type == ERTS_EV_TYPE_NONE);

    events = state->events;

    if (!event_data) {
	remove_events = events;
	add_events = 0;
    }
    else {
	remove_events = ~event_data->events & events;
	add_events = ~events & event_data->events;
    }

    if (add_events) {
	events = ERTS_CIO_POLL_CTL(pollset.ps, state->fd, add_events, 1, &do_wake);
	if (events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
	    ret = -1;
	    goto done;
	}
    }
    if (remove_events) {
	events = ERTS_CIO_POLL_CTL(pollset.ps, state->fd, remove_events, 0, &do_wake);
	if (events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
	    ret = -1;
	    goto done;
	}
    }
    if (event_data && event_data->events != 0) {
	if (state->type == ERTS_EV_TYPE_DRV_EV) {
	    state->driver.event->removed_events &= ~add_events;
	    state->driver.event->removed_events |= remove_events;
	}
	else {
	    if (!state->driver.event)
		state->driver.event = alloc_drv_event_data();
	    state->driver.event->port = id;
	    state->driver.event->removed_events = (ErtsPollEvents) 0;
	    state->type = ERTS_EV_TYPE_DRV_EV;
	}
	state->driver.event->data = event_data;
    }
    else {
	if (state->type == ERTS_EV_TYPE_DRV_EV) {
	    abort_tasks(state, 0);
	    state->driver.event->port = NIL;
	    state->driver.event->data = NULL;
	    state->driver.event->removed_events = (ErtsPollEvents) 0;
	}
	state->type = ERTS_EV_TYPE_NONE;
	remember_removed(state, &pollset);
    }
    state->events = events;
    ASSERT(event_data ? events == event_data->events : events == 0); 

    ret = 0;

done:

    check_fd_cleanup(state,
#if ERTS_CIO_HAVE_DRV_EVENT
		     &free_event,
#endif
		     &free_select,
                     &free_nif);

    erts_smp_mtx_unlock(fd_mtx(fd));

    if (free_select)
	free_drv_select_data(free_select);
    if (free_nif)
        free_nif_select_data(free_nif);
#if ERTS_CIO_HAVE_DRV_EVENT
    if (free_event)
	free_drv_event_data(free_event);
#endif

    return ret;
#endif
}

static ERTS_INLINE int
chk_stale(Eterm id, ErtsDrvEventState *state, int mode)
{
    if (is_nil(id))
	return 0;
    if (erts_is_port_alive(id))
	return 1; /* Steal */
    stale_drv_select(id, state, mode);
    return 0;
}

static int
need2steal(ErtsDrvEventState *state, int mode)
{
    int do_steal = 0;
    switch (state->type) {
    case ERTS_EV_TYPE_DRV_SEL:
	if (mode & ERL_DRV_READ)
	    do_steal |= chk_stale(state->driver.select->inport,
				  state,
				  ERL_DRV_READ);
	if (mode & ERL_DRV_WRITE)
	    do_steal |= chk_stale(state->driver.select->outport,
				  state,
				  ERL_DRV_WRITE);
	break;
    case ERTS_EV_TYPE_NIF:
        ASSERT(state->driver.stop.resource);
        do_steal = 1;
        break;

#if ERTS_CIO_HAVE_DRV_EVENT
    case ERTS_EV_TYPE_DRV_EV:
	do_steal |= chk_stale(state->driver.event->port, state, 0);
	break;
#endif
    case ERTS_EV_TYPE_STOP_USE:
    case ERTS_EV_TYPE_STOP_NIF:
        ASSERT(0);
	break;
    default:
	break;
    }
    return do_steal;
}

static void
print_driver_name(erts_dsprintf_buf_t *dsbufp, Eterm id)
{
    ErtsPortNames *pnp = erts_get_port_names(id, ERTS_INVALID_ERL_DRV_PORT);
    if (!pnp->name && !pnp->driver_name)
	erts_dsprintf(dsbufp, "%s ", "<unknown>");
    else {
	if (pnp->name) {
	    if (!pnp->driver_name || strcmp(pnp->driver_name, pnp->name) == 0)
		erts_dsprintf(dsbufp, "%s ", pnp->name);
	    else
		erts_dsprintf(dsbufp, "%s (%s) ", pnp->driver_name, pnp->name);
	}
	else if (pnp->driver_name) {
	    erts_dsprintf(dsbufp, "%s ", pnp->driver_name);
	}
    }
    erts_free_port_names(pnp);
}

static void
steal(erts_dsprintf_buf_t *dsbufp, ErtsDrvEventState *state, int mode)
{
    erts_dsprintf(dsbufp, "stealing control of fd=%d from ", (int) GET_FD(state->fd));
    switch (state->type) {
    case ERTS_EV_TYPE_DRV_SEL: {
	int deselect_mode = 0;
	Eterm iid = state->driver.select->inport;
	Eterm oid = state->driver.select->outport;
	if ((mode & ERL_DRV_READ) && (is_not_nil(iid))) {
	    erts_dsprintf(dsbufp, "input driver ");
	    print_driver_name(dsbufp, iid);
	    erts_dsprintf(dsbufp, "%T ", iid);
	    deselect_mode |= ERL_DRV_READ;
	}
	if ((mode & ERL_DRV_WRITE) && is_not_nil(oid)) {
	    if (deselect_mode) {
	    erts_dsprintf(dsbufp, "and ");
	    }
	    erts_dsprintf(dsbufp, "output driver ");
	    print_driver_name(dsbufp, oid);
	    erts_dsprintf(dsbufp, "%T ", oid);
	    deselect_mode |= ERL_DRV_WRITE;
	}
	if (deselect_mode)
	    deselect(state, deselect_mode);
	else {
	    erts_dsprintf(dsbufp, "no one", (int) GET_FD(state->fd));
	    ASSERT(0);
	}
	erts_dsprintf(dsbufp, "\n");
	break;
    }
    case ERTS_EV_TYPE_NIF: {
        Eterm iid = state->driver.nif->in.pid;
        Eterm oid = state->driver.nif->out.pid;
        const char* with = "with";
        ErlNifResourceType* rt = state->driver.stop.resource->type;

        erts_dsprintf(dsbufp, "resource %T:%T", rt->module, rt->name);

        if (is_not_nil(iid)) {
            erts_dsprintf(dsbufp, " %s in-pid %T", with, iid);
            with = "and";
        }
        if (is_not_nil(oid)) {
            erts_dsprintf(dsbufp, " %s out-pid %T", with, oid);
        }
        deselect(state, 0);
        erts_dsprintf(dsbufp, "\n");
        break;
    }
#if ERTS_CIO_HAVE_DRV_EVENT
    case ERTS_EV_TYPE_DRV_EV: {
	Eterm eid = state->driver.event->port;
	if (is_nil(eid)) {
	    erts_dsprintf(dsbufp, "no one", (int) state->fd);
	    ASSERT(0);
	}
	else {
	    erts_dsprintf(dsbufp, "event driver ");
	    print_driver_name(dsbufp, eid);
	    erts_dsprintf(dsbufp, "%T ", eid);
	}
	erts_dsprintf(dsbufp, "\n");
	deselect(state, 0);
	break;
    }
#endif
    case ERTS_EV_TYPE_STOP_USE:
    case ERTS_EV_TYPE_STOP_NIF: {
	ASSERT(0);
	break;
    }
    default:
	erts_dsprintf(dsbufp, "no one\n", (int) GET_FD(state->fd));
	ASSERT(0);
    }
}

static void
print_drv_select_op(erts_dsprintf_buf_t *dsbufp,
                    ErlDrvPort ix, ErtsSysFdType fd, int mode, int on)
{
    Port *pp = erts_drvport2port(ix);
    erts_dsprintf(dsbufp,
		  "driver_select(%p, %d,%s%s%s%s, %d) "
		  "by ",
		  ix,
		  (int) GET_FD(fd),
		  mode & ERL_DRV_READ ? " ERL_DRV_READ" : "",
		  mode & ERL_DRV_WRITE ? " ERL_DRV_WRITE" : "",
		  mode & ERL_DRV_USE ? " ERL_DRV_USE" : "",
		  mode & (ERL_DRV_USE_NO_CALLBACK & ~ERL_DRV_USE) ? "_NO_CALLBACK" : "",
		  on);
    print_driver_name(dsbufp, pp != ERTS_INVALID_ERL_DRV_PORT ? pp->common.id : NIL);
    erts_dsprintf(dsbufp, "driver %T ", pp != ERTS_INVALID_ERL_DRV_PORT ? pp->common.id : NIL);
}

static void
print_nif_select_op(erts_dsprintf_buf_t *dsbufp,
                    ErtsSysFdType fd, int mode,
                    ErtsResource* resource, Eterm ref)
{
    erts_dsprintf(dsbufp,
		  "enif_select(_, %d,%s%s%s, %T:%T, %T) ",
		  (int) GET_FD(fd),
		  mode & ERL_NIF_SELECT_READ ? " READ" : "",
		  mode & ERL_NIF_SELECT_WRITE ? " WRITE" : "",
		  mode & ERL_NIF_SELECT_STOP ? " STOP" : "",
		  resource->type->module,
                  resource->type->name,
                  ref);
}


static void
drv_select_steal(ErlDrvPort ix, ErtsDrvEventState *state, int mode, int on)
{
    if (need2steal(state, mode)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	print_drv_select_op(dsbufp, ix, state->fd, mode, on);
	steal(dsbufp, state, mode);
	erts_send_error_to_logger_nogl(dsbufp);
    }
}

static void
nif_select_steal(ErtsDrvEventState *state, int mode,
                 ErtsResource* resource, Eterm ref)
{
    if (need2steal(state, mode)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	print_nif_select_op(dsbufp, state->fd, mode, resource, ref);
	steal(dsbufp, state, mode);
	erts_send_error_to_logger_nogl(dsbufp);
    }
}

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
static void
large_fd_error_common(erts_dsprintf_buf_t *dsbufp, ErtsSysFdType fd)
{
    erts_dsprintf(dsbufp,
		  "fd=%d is larger than the largest allowed fd=%d\n",
		  (int) fd, max_fds - 1);
}

static void
drv_select_large_fd_error(ErlDrvPort ix, ErtsSysFdType fd, int mode, int on)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
    print_drv_select_op(dsbufp, ix, fd, mode, on);
    erts_dsprintf(dsbufp, "failed: ");
    large_fd_error_common(dsbufp, fd);
    erts_send_error_to_logger_nogl(dsbufp);
}
static void
nif_select_large_fd_error(ErtsSysFdType fd, int mode,
                          ErtsResource* resource, Eterm ref)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
    print_nif_select_op(dsbufp, fd, mode, resource, ref);
    erts_dsprintf(dsbufp, "failed: ");
    large_fd_error_common(dsbufp, fd);
    erts_send_error_to_logger_nogl(dsbufp);
}
#endif /* ERTS_SYS_CONTINOUS_FD_NUMBERS */



static void
steal_pending_stop_use(erts_dsprintf_buf_t *dsbufp, ErlDrvPort ix,
                       ErtsDrvEventState *state, int mode, int on)
{
    int cancel = 0;
    ASSERT(state->type == ERTS_EV_TYPE_STOP_USE);

    if (on) {
	/* Either fd-owner changed its mind about closing
	 * or closed fd before stop_select callback and fd is now reused.
	 * In either case stop_select should not be called.
	 */
        cancel = 1;
    }
    else if ((mode & ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE) {
	Port *prt = erts_drvport2port(ix);
	if (prt == ERTS_INVALID_ERL_DRV_PORT
            || prt->drv_ptr != state->driver.stop.drv_ptr) {
	    /* Some other driver or nif wants the stop_select callback */
            cancel = 1;
        }
    }

    if (cancel) {
        erts_dsprintf(dsbufp, "called before stop_select was called for driver '%s'\n",
                      state->driver.stop.drv_ptr->name);
        if (state->driver.stop.drv_ptr->handle) {
            erts_ddll_dereference_driver(state->driver.stop.drv_ptr->handle);
        }
        state->type = ERTS_EV_TYPE_NONE;
        state->flags &= ~ERTS_EV_FLAG_USED;
        state->driver.stop.drv_ptr = NULL;
    }
    else {
        erts_dsprintf(dsbufp, "ignored repeated call\n");
    }
    erts_send_error_to_logger_nogl(dsbufp);
}

static void
steal_pending_stop_nif(erts_dsprintf_buf_t *dsbufp, ErtsResource* resource,
                       ErtsDrvEventState *state, int mode, int on)
{
    int cancel = 0;

    ASSERT(state->type == ERTS_EV_TYPE_STOP_NIF);
    ASSERT(state->driver.stop.resource);

    if (on) {
        ASSERT(mode & (ERL_NIF_SELECT_READ | ERL_NIF_SELECT_WRITE));
        /* Either fd-owner changed its mind about closing
         * or closed fd before stop callback and fd is now reused.
         * In either case, stop should not be called.
         */
        cancel = 1;
    }
    else if ((mode & ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE
             && resource != state->driver.stop.resource) {
        /* Some driver or other resource wants the stop callback */
        cancel = 1;
    }

    if (cancel) {
        ErlNifResourceType* rt = state->driver.stop.resource->type;
        erts_dsprintf(dsbufp, "called before stop was called for NIF resource %T:%T\n",
                      rt->module, rt->name);

        enif_release_resource(state->driver.stop.resource);
        state->type = ERTS_EV_TYPE_NONE;
        state->flags &= ~ERTS_EV_FLAG_USED;
        state->driver.stop.resource = NULL;
    }
    else {
        erts_dsprintf(dsbufp, "ignored repeated call\n");
    }
    erts_send_error_to_logger_nogl(dsbufp);

}


#if ERTS_CIO_HAVE_DRV_EVENT

static void
print_drv_event_op(erts_dsprintf_buf_t *dsbufp,
                   ErlDrvPort ix, ErtsSysFdType fd, ErlDrvEventData event_data)
{
    Port *pp = erts_drvport2port(ix);
    erts_dsprintf(dsbufp, "driver_event(%p, %d, ", ix, (int) fd);
    if (!event_data)
	erts_dsprintf(dsbufp, "NULL");
    else
	erts_dsprintf(dsbufp, "{0x%x, 0x%x}",
		      (unsigned int) event_data->events,
		      (unsigned int) event_data->revents);
    erts_dsprintf(dsbufp, ") by ");
    if (pp != ERTS_INVALID_ERL_DRV_PORT)
	print_driver_name(dsbufp, pp->common.id);
    erts_dsprintf(dsbufp, "driver %T ", pp != ERTS_INVALID_ERL_DRV_PORT ? pp->common.id : NIL);
}

static void
drv_event_steal(ErlDrvPort ix, ErtsDrvEventState *state, ErlDrvEventData event_data)
{
    if (need2steal(state, ERL_DRV_READ|ERL_DRV_WRITE)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	print_drv_event_op(dsbufp, ix, state->fd, event_data);
	steal(dsbufp, state, ERL_DRV_READ|ERL_DRV_WRITE);
	erts_send_error_to_logger_nogl(dsbufp);
    }
    else if (state->type == ERTS_EV_TYPE_DRV_SEL) {
	ASSERT(state->flags & ERTS_EV_FLAG_USED);
	deselect(state, 0);
    }
}

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
static void
event_large_fd_error(ErlDrvPort ix, ErtsSysFdType fd, ErlDrvEventData event_data)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
    print_drv_event_op(dsbufp, ix, fd, event_data);
    erts_dsprintf(dsbufp, "failed: ");
    large_fd_error_common(dsbufp, fd);
    erts_send_error_to_logger_nogl(dsbufp);
}
#endif
#endif

static ERTS_INLINE int
io_task_schedule_allowed(ErtsDrvEventState *state,
			 ErtsPortTaskType type,
			 erts_aint_t current_cio_time)
{
    ErtsIoTask *io_task;

    switch (type) {
    case ERTS_PORT_TASK_INPUT:
	if (!state->driver.select)
	    return 0;
#if ERTS_CIO_HAVE_DRV_EVENT
	if (state->driver.event)
	    return 0;
#endif
	io_task = &state->driver.select->iniotask;
	break;
    case ERTS_PORT_TASK_OUTPUT:
	if (!state->driver.select)
	    return 0;
#if ERTS_CIO_HAVE_DRV_EVENT
	if (state->driver.event)
	    return 0;
#endif
	io_task = &state->driver.select->outiotask;
	break;
#if ERTS_CIO_HAVE_DRV_EVENT
    case ERTS_PORT_TASK_EVENT:
	if (!state->driver.event)
	    return 0;
	if (state->driver.select)
	    return 0;
	io_task = &state->driver.event->iotask;
	break;
#endif
    default:
	ERTS_INTERNAL_ERROR("Invalid I/O-task type");
	return 0;
    }

    return !is_iotask_active(io_task, current_cio_time);
}

static ERTS_INLINE void
iready(Eterm id, ErtsDrvEventState *state, erts_aint_t current_cio_time)
{
    if (io_task_schedule_allowed(state,
				 ERTS_PORT_TASK_INPUT,
				 current_cio_time)) {
	ErtsIoTask *iotask = &state->driver.select->iniotask;
	erts_smp_atomic_set_nob(&iotask->executed_time, current_cio_time);
	if (erts_port_task_schedule(id,
				    &iotask->task,
				    ERTS_PORT_TASK_INPUT,
				    (ErlDrvEvent) state->fd) != 0) {
	    stale_drv_select(id, state, ERL_DRV_READ);
	}
	add_active_fd(state->fd);
    }
}

static ERTS_INLINE void
oready(Eterm id, ErtsDrvEventState *state, erts_aint_t current_cio_time)
{
    if (io_task_schedule_allowed(state,
				 ERTS_PORT_TASK_OUTPUT,
				 current_cio_time)) {
	ErtsIoTask *iotask = &state->driver.select->outiotask;
	erts_smp_atomic_set_nob(&iotask->executed_time, current_cio_time);
	if (erts_port_task_schedule(id,
				    &iotask->task,
				    ERTS_PORT_TASK_OUTPUT,
				    (ErlDrvEvent) state->fd) != 0) {
	    stale_drv_select(id, state, ERL_DRV_WRITE);
	}
	add_active_fd(state->fd);
    }
}

static ERTS_INLINE void
send_event_tuple(struct erts_nif_select_event* e, ErtsResource* resource,
                 Eterm event_atom)
{
    Process* rp = erts_proc_lookup(e->pid);
    ErtsProcLocks rp_locks = 0;
    ErtsMessage* mp;
    ErlOffHeap* ohp;
    ErtsBinary* bin;
    Eterm* hp;
    Uint hsz;
    Eterm resource_term, ref_term, tuple;

    if (!rp) {
        return;
    }

    bin = ERTS_MAGIC_BIN_FROM_UNALIGNED_DATA(resource);

     /* {select, Resource, Ref, EventAtom} */
    if (is_value(e->immed)) {
        hsz = 5 + ERTS_MAGIC_REF_THING_SIZE;
    }
    else {
        hsz = 5 + ERTS_MAGIC_REF_THING_SIZE + ERTS_REF_THING_SIZE;
    }

    mp = erts_alloc_message_heap(rp, &rp_locks, hsz, &hp, &ohp);

    resource_term = erts_mk_magic_ref(&hp, ohp, &bin->binary);
    if (is_value(e->immed)) {
        ASSERT(is_immed(e->immed));
        ref_term = e->immed;
    }
    else {
        write_ref_thing(hp, e->refn[0], e->refn[1], e->refn[2]);
        ref_term = make_internal_ref(hp);
        hp += ERTS_REF_THING_SIZE;
    }
    tuple = TUPLE4(hp, am_select, resource_term, ref_term, event_atom);

    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    erts_queue_message(rp, rp_locks, mp, tuple, am_system);

    if (rp_locks)
        erts_smp_proc_unlock(rp, rp_locks);
}


#if ERTS_CIO_HAVE_DRV_EVENT
static ERTS_INLINE void
eready(Eterm id, ErtsDrvEventState *state, ErlDrvEventData event_data,
       erts_aint_t current_cio_time)
{
    if (io_task_schedule_allowed(state,
				 ERTS_PORT_TASK_EVENT,
				 current_cio_time)) {
	ErtsIoTask *iotask = &state->driver.event->iotask;
	erts_smp_atomic_set_nob(&iotask->executed_time, current_cio_time);
	if (erts_port_task_schedule(id,
				    &iotask->task,
				    ERTS_PORT_TASK_EVENT,
				    (ErlDrvEvent) state->fd,
				    event_data) != 0) {
	    stale_drv_select(id, state, 0);
	}
	add_active_fd(state->fd);
    }
}
#endif

static void bad_fd_in_pollset(ErtsDrvEventState *, Eterm inport, Eterm outport);

#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
void
ERTS_CIO_EXPORT(erts_check_io_async_sig_interrupt)(void)
{
    ERTS_CIO_POLL_AS_INTR(pollset.ps);
}
#endif

void
ERTS_CIO_EXPORT(erts_check_io_interrupt)(int set)
{
    ERTS_CIO_POLL_INTR(pollset.ps, set);
}

void
ERTS_CIO_EXPORT(erts_check_io_interrupt_timed)(int set,
					       ErtsMonotonicTime timeout_time)
{
    ERTS_CIO_POLL_INTR_TMD(pollset.ps, set, timeout_time);
}

#if !ERTS_CIO_DEFER_ACTIVE_EVENTS
/*
 * Number of ignored events, for a lingering fd added by enif_select(),
 * until we deselect fd-event from pollset.
 */
# define ERTS_NIF_DELAYED_DESELECT 20
#else
/* Disable delayed deselect as pollset cannot handle active events */
# define ERTS_NIF_DELAYED_DESELECT 1
#endif

void
ERTS_CIO_EXPORT(erts_check_io)(int do_wait)
{
    ErtsPollResFd *pollres;
    int pollres_len;
    ErtsMonotonicTime timeout_time;
    int poll_ret, i;
    erts_aint_t current_cio_time;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ASSERT(esdp);

 restart:

#ifdef ERTS_BREAK_REQUESTED
    if (ERTS_BREAK_REQUESTED)
	erts_do_break_handling();
#endif

#ifdef ERTS_SIGNAL_STATE /* ifndef ERTS_SMP */
    if (ERTS_SIGNAL_STATE) {
        erts_handle_signal_state();
    }
#endif

    /* Figure out timeout value */
    timeout_time = (do_wait
		    ? erts_check_next_timeout_time(esdp)
		    : ERTS_POLL_NO_TIMEOUT /* poll only */);

    /*
     * No need for an atomic inc op when incrementing
     * erts_check_io_time, since only one thread can
     * check io at a time.
     */
    current_cio_time = erts_smp_atomic_read_dirty(&erts_check_io_time);
    current_cio_time++;
    erts_smp_atomic_set_relb(&erts_check_io_time, current_cio_time);

    check_cleanup_active_fds(current_cio_time,
                             timeout_time != ERTS_POLL_NO_TIMEOUT);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    pollres_len = erts_smp_atomic32_read_dirty(&pollset.active_fd.no) + ERTS_CHECK_IO_POLL_RES_LEN;

    pollres = erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsPollResFd)*pollres_len);

    erts_smp_atomic_set_nob(&pollset.in_poll_wait, 1);

    poll_ret = ERTS_CIO_POLL_WAIT(pollset.ps, pollres, &pollres_len, timeout_time);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

#ifdef ERTS_BREAK_REQUESTED
    if (ERTS_BREAK_REQUESTED)
	erts_do_break_handling();
#endif


#ifdef ERTS_SIGNAL_STATE /* ifndef ERTS_SMP */
    if (ERTS_SIGNAL_STATE) {
        erts_handle_signal_state();
    }
#endif


    if (poll_ret != 0) {
	erts_smp_atomic_set_nob(&pollset.in_poll_wait, 0);
	forget_removed(&pollset);
	erts_free(ERTS_ALC_T_TMP, pollres);
	if (poll_ret == EAGAIN) {
	    goto restart;
	}

	if (poll_ret != ETIMEDOUT
	    && poll_ret != EINTR
#ifdef ERRNO_BLOCK
	    && poll_ret != ERRNO_BLOCK
#endif
	    ) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "erts_poll_wait() failed: %s (%d)\n",
			  erl_errno_id(poll_ret), poll_ret);
	    erts_send_error_to_logger_nogl(dsbufp);
	}
	return;
    }

    for (i = 0; i < pollres_len; i++) {

	ErtsSysFdType fd = (ErtsSysFdType) pollres[i].fd;
	ErtsDrvEventState *state;

	erts_smp_mtx_lock(fd_mtx(fd));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
	state = &drv_ev_state[ (int) fd];
#else
	state = hash_get_drv_ev_state(fd);
	if (!state) {
	    goto next_pollres;
	}
#endif

	/* Skip this fd if it was removed from pollset */
	if (is_removed(state)) {
	    goto next_pollres;
	}

	switch (state->type) {
	case ERTS_EV_TYPE_DRV_SEL: { /* Requested via driver_select()... */
	    ErtsPollEvents revents = pollres[i].events;

            if (revents & ERTS_POLL_EV_ERR) {
                /*
                 * Handle error events by triggering all in/out events
                 * that the driver has selected.
                 * We *do not* want to call a callback that corresponds
		 * to an event not selected.
                 */
                revents = state->events;
            }
            else {
                revents &= (state->events | ERTS_POLL_EV_NVAL);
            }

            if (revents & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
		if (revents & ERTS_POLL_EV_OUT) {
		    oready(state->driver.select->outport, state, current_cio_time);
		}
		/* Someone might have deselected input since revents
		   was read (true also on the non-smp emulator since
		   oready() may have been called); therefore, update
		   revents... */
		revents &= state->events;
		if (revents & ERTS_POLL_EV_IN) {
		    iready(state->driver.select->inport, state, current_cio_time);
		}
	    }
	    else if (revents & ERTS_POLL_EV_NVAL) {
		bad_fd_in_pollset(state,
                                  state->driver.select->inport,
                                  state->driver.select->outport);
		add_active_fd(state->fd);
	    }
	    break;
	}

        case ERTS_EV_TYPE_NIF: { /* Requested via enif_select()... */
            struct erts_nif_select_event in = {NIL};
            struct erts_nif_select_event out = {NIL};
            ErtsResource* resource = NULL;
            ErtsPollEvents revents = pollres[i].events;

            if (revents & ERTS_POLL_EV_ERR) {
                /*
                 * Handle error events by triggering all in/out events
                 * that the NIF has selected.
                 * We *do not* want to send a message that corresponds
		 * to an event not selected.
                 */
                revents = state->events;
            }
            else {
                revents &= (state->events | ERTS_POLL_EV_NVAL);
            }

            if (revents & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
                if (revents & ERTS_POLL_EV_OUT) {
                    if (is_not_nil(state->driver.nif->out.pid)) {
                        out = state->driver.nif->out;
                        resource = state->driver.stop.resource;
                        state->driver.nif->out.ddeselect_cnt = ERTS_NIF_DELAYED_DESELECT;
                        state->driver.nif->out.pid = NIL;
                        add_active_fd(state->fd);
                    }
                    else {
                        ASSERT(state->driver.nif->out.ddeselect_cnt >= 2);
                        state->driver.nif->out.ddeselect_cnt--;
                    }
                }
                if (revents & ERTS_POLL_EV_IN) {
                    if (is_not_nil(state->driver.nif->in.pid)) {
                        in = state->driver.nif->in;
                        resource = state->driver.stop.resource;
                        state->driver.nif->in.ddeselect_cnt = ERTS_NIF_DELAYED_DESELECT;
                        state->driver.nif->in.pid = NIL;
                        add_active_fd(state->fd);
                    }
                    else {
                        ASSERT(state->driver.nif->in.ddeselect_cnt >= 2);
                        state->driver.nif->in.ddeselect_cnt--;
                    }
                }
            }
            else if (revents & ERTS_POLL_EV_NVAL) {
                bad_fd_in_pollset(state, NIL, NIL);
                add_active_fd(state->fd);
            }

#ifdef ERTS_SMP
            erts_smp_mtx_unlock(fd_mtx(fd));
#endif
            if (is_not_nil(in.pid)) {
                send_event_tuple(&in, resource, am_ready_input);
            }
            if (is_not_nil(out.pid)) {
                send_event_tuple(&out, resource, am_ready_output);
            }
            goto next_pollres_unlocked;
        }

#if ERTS_CIO_HAVE_DRV_EVENT
	case ERTS_EV_TYPE_DRV_EV: { /* Requested via driver_event()... */
	    ErlDrvEventData event_data;
	    ErtsPollEvents revents;
	    ASSERT(state->driver.event);
	    ASSERT(state->driver.event->data);
	    event_data = state->driver.event->data;
	    revents = pollres[i].events;
	    revents &= ~state->driver.event->removed_events;

	    if (revents) {
		event_data->events = state->events;
		event_data->revents = revents;
		eready(state->driver.event->port, state, event_data, current_cio_time);
	    }
	    break;
	}
#endif

	case ERTS_EV_TYPE_NONE: /* Deselected ... */
	    break;

	default: { /* Error */
	    erts_dsprintf_buf_t *dsbufp;
	    dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Invalid event request type for fd in erts_poll()! "
			  "fd=%d, event request type=%sd\n", (int) state->fd,
			  (int) state->type);
	    ASSERT(0);
	    deselect(state, 0);
	    add_active_fd(state->fd);
	    break;
	}
	}

	next_pollres:;
#ifdef ERTS_SMP
	erts_smp_mtx_unlock(fd_mtx(fd));
#endif
        next_pollres_unlocked:;
    }

    erts_smp_atomic_set_nob(&pollset.in_poll_wait, 0);
    erts_free(ERTS_ALC_T_TMP, pollres);
    forget_removed(&pollset);
}

static void
bad_fd_in_pollset(ErtsDrvEventState *state, Eterm inport, Eterm outport)
{
    ErtsPollEvents events = state->events;
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();

    if (events & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
	char *io_str;
	Eterm port = NIL;
	if ((events & ERTS_POLL_EV_IN) && (events & ERTS_POLL_EV_OUT)) {
	    io_str = "input/output";
	    if (inport == outport)
		port = inport;
	}
	else {
	    if (events & ERTS_POLL_EV_IN) {
		io_str = "input";
		port = inport;
	    }
	    else {
		io_str = "output";
		port = outport;
	    }
	}
	erts_dsprintf(dsbufp,
		      "Bad %s fd in erts_poll()! fd=%d, ",
		      io_str, (int) state->fd);
        if (state->type == ERTS_EV_TYPE_DRV_SEL) {
            if (is_nil(port)) {
                ErtsPortNames *ipnp = erts_get_port_names(inport, ERTS_INVALID_ERL_DRV_PORT);
                ErtsPortNames *opnp = erts_get_port_names(outport, ERTS_INVALID_ERL_DRV_PORT);
                erts_dsprintf(dsbufp, "ports=%T/%T, drivers=%s/%s, names=%s/%s\n",
                              is_nil(inport) ? am_undefined : inport,
                              is_nil(outport) ? am_undefined : outport,
                              ipnp->driver_name ? ipnp->driver_name : "<unknown>",
                              opnp->driver_name ? opnp->driver_name : "<unknown>",
                              ipnp->name ? ipnp->name : "<unknown>",
                              opnp->name ? opnp->name : "<unknown>");
                erts_free_port_names(ipnp);
                erts_free_port_names(opnp);
            }
            else {
                ErtsPortNames *pnp = erts_get_port_names(port, ERTS_INVALID_ERL_DRV_PORT);
                erts_dsprintf(dsbufp, "port=%T, driver=%s, name=%s\n",
                              is_nil(port) ? am_undefined : port,
                              pnp->driver_name ? pnp->driver_name : "<unknown>",
                              pnp->name ? pnp->name : "<unknown>");
                erts_free_port_names(pnp);
            }
        }
        else {
            ErlNifResourceType* rt;
            ASSERT(state->type == ERTS_EV_TYPE_NIF);
            ASSERT(state->driver.stop.resource);
            rt = state->driver.stop.resource->type;
            erts_dsprintf(dsbufp, "resource={%T,%T}\n", rt->module, rt->name);
        }
    }
    else {
	erts_dsprintf(dsbufp, "Bad fd in erts_poll()! fd=%d\n", (int) state->fd);
    }
    erts_send_error_to_logger_nogl(dsbufp);

    /* unmap entry */
    deselect(state, 0);
}

static void
stale_drv_select(Eterm id, ErtsDrvEventState *state, int mode)
{
    erts_stale_drv_select(id, ERTS_INVALID_ERL_DRV_PORT, (ErlDrvEvent) state->fd, mode, 0);
    deselect(state, mode);
}

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS

static SafeHashValue drv_ev_state_hash(void *des)
{
    SafeHashValue val = (SafeHashValue) ((ErtsDrvEventState *) des)->fd;
    return val ^ (val >> 8);  /* Good enough for aligned pointer values? */
}

static int drv_ev_state_cmp(void *des1, void *des2)
{
    return ( ((ErtsDrvEventState *) des1)->fd == ((ErtsDrvEventState *) des2)->fd
	    ? 0 : 1);
}

static void *drv_ev_state_alloc(void *des_tmpl)
{
    ErtsDrvEventState *evstate;
    erts_smp_spin_lock(&state_prealloc_lock);
    if (state_prealloc_first == NULL) {
	erts_smp_spin_unlock(&state_prealloc_lock);
	evstate = (ErtsDrvEventState *) 
	    erts_alloc(ERTS_ALC_T_DRV_EV_STATE, sizeof(ErtsDrvEventState));
    } else {
	evstate = state_prealloc_first;
	state_prealloc_first = (ErtsDrvEventState *) evstate->hb.next;
	--num_state_prealloc;
	erts_smp_spin_unlock(&state_prealloc_lock);
    }
    /* XXX: Already valid data if prealloced, could ignore template! */
    *evstate = *((ErtsDrvEventState *) des_tmpl);

    return (void *) evstate;
}

static void drv_ev_state_free(void *des)
{
    erts_smp_spin_lock(&state_prealloc_lock);
    ((ErtsDrvEventState *) des)->hb.next = &state_prealloc_first->hb;
    state_prealloc_first = (ErtsDrvEventState *) des;
    ++num_state_prealloc;
    erts_smp_spin_unlock(&state_prealloc_lock);
}
#endif

void
ERTS_CIO_EXPORT(erts_init_check_io)(void)
{
    ERTS_CT_ASSERT((INT_MIN & (ERL_NIF_SELECT_STOP_CALLED |
                               ERL_NIF_SELECT_STOP_SCHEDULED |
                               ERL_NIF_SELECT_INVALID_EVENT |
                               ERL_NIF_SELECT_FAILED)) == 0);
    erts_smp_atomic_init_nob(&erts_check_io_time, 0);
    erts_smp_atomic_init_nob(&pollset.in_poll_wait, 0);

    ERTS_CIO_POLL_INIT();
    pollset.ps = ERTS_CIO_NEW_POLLSET();

    pollset.active_fd.six = 0;
    pollset.active_fd.eix = 0;
    erts_smp_atomic32_init_nob(&pollset.active_fd.no, 0);
    pollset.active_fd.size = ERTS_ACTIVE_FD_INC;
    pollset.active_fd.array = erts_alloc(ERTS_ALC_T_ACTIVE_FD_ARR,
					 sizeof(ErtsSysFdType)*ERTS_ACTIVE_FD_INC);
#ifdef DEBUG
    {
	int i;
	for (i = 0; i < ERTS_ACTIVE_FD_INC; i++)
	    pollset.active_fd.array[i] = ERTS_SYS_FD_INVALID;
    }
#endif


#ifdef ERTS_SMP
    init_removed_fd_alloc();
    pollset.removed_list = NULL;
    erts_smp_spinlock_init(&pollset.removed_list_lock,
			   "pollset_rm_list");
    {
	int i;
	for (i=0; i<DRV_EV_STATE_LOCK_CNT; i++) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	    erts_smp_mtx_init_x(&drv_ev_state_locks[i].lck, "drv_ev_state", make_small(i));
#else
	    erts_smp_mtx_init(&drv_ev_state_locks[i].lck, "drv_ev_state");
#endif
	}
    }
#endif
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    max_fds = ERTS_CIO_POLL_MAX_FDS();
    erts_smp_atomic_init_nob(&drv_ev_state_len, 0);
    drv_ev_state = NULL;
    erts_smp_mtx_init(&drv_ev_state_grow_lock, "drv_ev_state_grow");
#else
    {
	SafeHashFunctions hf;
	hf.hash = &drv_ev_state_hash;
	hf.cmp = &drv_ev_state_cmp;
	hf.alloc = &drv_ev_state_alloc;
	hf.free = &drv_ev_state_free;
	num_state_prealloc = 0;
	state_prealloc_first = NULL;
	erts_smp_spinlock_init(&state_prealloc_lock,"state_prealloc");

	safe_hash_init(ERTS_ALC_T_DRV_EV_STATE, &drv_ev_state_tab, "drv_ev_state_tab", 
		       DRV_EV_STATE_HTAB_SIZE, hf);
    }
#endif
}

int
ERTS_CIO_EXPORT(erts_check_io_max_files)(void)
{
#ifdef  ERTS_SYS_CONTINOUS_FD_NUMBERS
    return max_fds;
#else
    return ERTS_POLL_EXPORT(erts_poll_max_fds)();
#endif
}

Uint
ERTS_CIO_EXPORT(erts_check_io_size)(void)
{
    Uint res;
    ErtsPollInfo pi;
    ERTS_CIO_POLL_INFO(pollset.ps, &pi);
    res = pi.memory_size;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    res += sizeof(ErtsDrvEventState) * erts_smp_atomic_read_nob(&drv_ev_state_len);
#else
    res += safe_hash_table_sz(&drv_ev_state_tab);
    {
	SafeHashInfo hi;
	safe_hash_get_info(&hi, &drv_ev_state_tab);
	res += hi.objs * sizeof(ErtsDrvEventState);
    }
    erts_smp_spin_lock(&state_prealloc_lock);
    res += num_state_prealloc * sizeof(ErtsDrvEventState);
    erts_smp_spin_unlock(&state_prealloc_lock);
#endif
    return res;
}

Eterm
ERTS_CIO_EXPORT(erts_check_io_info)(void *proc)
{
    Process *p = (Process *) proc;
    Eterm tags[16], values[16], res;
    Uint sz, *szp, *hp, **hpp, memory_size;
    Sint i;
    ErtsPollInfo pi;
    erts_aint_t cio_time = erts_smp_atomic_read_acqb(&erts_check_io_time);
    int active_fds = (int) erts_smp_atomic32_read_acqb(&pollset.active_fd.no);

    while (1) {
	erts_aint_t post_cio_time;
	int post_active_fds;

	ERTS_CIO_POLL_INFO(pollset.ps, &pi);

	post_cio_time = erts_smp_atomic_read_mb(&erts_check_io_time);
	post_active_fds = (int) erts_smp_atomic32_read_acqb(&pollset.active_fd.no);
	if (cio_time == post_cio_time && active_fds == post_active_fds)
	    break;
	cio_time = post_cio_time;
	active_fds = post_active_fds;
    }

    memory_size = pi.memory_size;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    memory_size += sizeof(ErtsDrvEventState) * erts_smp_atomic_read_nob(&drv_ev_state_len);
#else
    memory_size += safe_hash_table_sz(&drv_ev_state_tab);
    {
	SafeHashInfo hi;
	safe_hash_get_info(&hi, &drv_ev_state_tab);
	memory_size += hi.objs * sizeof(ErtsDrvEventState);
    }
    erts_smp_spin_lock(&state_prealloc_lock);
    memory_size += num_state_prealloc * sizeof(ErtsDrvEventState);
    erts_smp_spin_unlock(&state_prealloc_lock);
#endif

    hpp = NULL;
    szp = &sz;
    sz = 0;

 bld_it:
    i = 0;

    tags[i] = erts_bld_atom(hpp, szp, "name");
    values[i++] = erts_bld_atom(hpp, szp, "erts_poll");

    tags[i] = erts_bld_atom(hpp, szp, "primary");
    values[i++] = erts_bld_atom(hpp, szp, pi.primary);

    tags[i] = erts_bld_atom(hpp, szp, "fallback");
    values[i++] = erts_bld_atom(hpp, szp, pi.fallback ? pi.fallback : "false");

    tags[i] = erts_bld_atom(hpp, szp, "kernel_poll");
    values[i++] = erts_bld_atom(hpp, szp,
				pi.kernel_poll ? pi.kernel_poll : "false");

    tags[i] = erts_bld_atom(hpp, szp, "memory_size");
    values[i++] = erts_bld_uint(hpp, szp, memory_size);

    tags[i] = erts_bld_atom(hpp, szp, "total_poll_set_size");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.poll_set_size);

    if (pi.fallback) {
	tags[i] = erts_bld_atom(hpp, szp, "fallback_poll_set_size");
	values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.fallback_poll_set_size);
    }

    tags[i] = erts_bld_atom(hpp, szp, "lazy_updates");
    values[i++] = pi.lazy_updates ? am_true : am_false;

    if (pi.lazy_updates) {
	tags[i] = erts_bld_atom(hpp, szp, "pending_updates");
	values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.pending_updates);
    }

    tags[i] = erts_bld_atom(hpp, szp, "batch_updates");
    values[i++] = pi.batch_updates ? am_true : am_false;

    tags[i] = erts_bld_atom(hpp, szp, "concurrent_updates");
    values[i++] = pi.concurrent_updates ? am_true : am_false;

    tags[i] = erts_bld_atom(hpp, szp, "max_fds");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.max_fds);

    tags[i] = erts_bld_atom(hpp, szp, "active_fds");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) active_fds);

#ifdef ERTS_POLL_COUNT_AVOIDED_WAKEUPS
    tags[i] = erts_bld_atom(hpp, szp, "no_avoided_wakeups");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.no_avoided_wakeups);

    tags[i] = erts_bld_atom(hpp, szp, "no_avoided_interrupts");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.no_avoided_interrupts);

    tags[i] = erts_bld_atom(hpp, szp, "no_interrupt_timed");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.no_interrupt_timed);
#endif

    res = erts_bld_2tup_list(hpp, szp, i, tags, values);

    if (!hpp) {
	hp = HAlloc(p, sz);
	hpp = &hp;
	szp = NULL;
	goto bld_it;
    }

    return res;
}

static ERTS_INLINE ErtsPollEvents
print_events(ErtsPollEvents ev)
{
    int first = 1;
    if(ev & ERTS_POLL_EV_IN) {
	ev &= ~ERTS_POLL_EV_IN;
	erts_printf("%s%s", first ? "" : "|", "IN");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_OUT) {
	ev &= ~ERTS_POLL_EV_OUT;
	erts_printf("%s%s", first ? "" : "|", "OUT");
	first = 0;
    }
    /* The following should not appear... */
    if(ev & ERTS_POLL_EV_NVAL) {
	erts_printf("%s%s", first ? "" : "|", "NVAL");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_ERR) {
	erts_printf("%s%s", first ? "" : "|", "ERR");
	first = 0;
    }
    if (ev)
	erts_printf("%s0x%b32x", first ? "" : "|", (Uint32) ev);
    return ev;
}

typedef struct {
    int used_fds;
    int num_errors;
    int no_driver_select_structs;
    int no_driver_event_structs;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    int internal_fds;
    ErtsPollEvents *epep;
#endif
} IterDebugCounters;

static void doit_erts_check_io_debug(void *vstate, void *vcounters)
{
    ErtsDrvEventState *state = (ErtsDrvEventState *) vstate;
    IterDebugCounters *counters = (IterDebugCounters *) vcounters;
    ErtsPollEvents cio_events = state->events;
    ErtsSysFdType fd = state->fd;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    int internal = 0;
    ErtsPollEvents ep_events = counters->epep[(int) fd];
#endif
    int err = 0;

#if defined(HAVE_FSTAT) && !defined(NO_FSTAT_ON_SYS_FD_TYPE)
    struct stat stat_buf;
#endif

    if (state->driver.select)
	counters->no_driver_select_structs++;
#if ERTS_CIO_HAVE_DRV_EVENT
    if (state->driver.event)
	counters->no_driver_event_structs++;
#endif

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (state->events || ep_events) {
	if (ep_events & ERTS_POLL_EV_NVAL) {
	    ep_events &= ~ERTS_POLL_EV_NVAL;
	    internal = 1;
	    counters->internal_fds++;
	}
	else
	    counters->used_fds++;
#else
    if (state->events) {
	counters->used_fds++;
#endif
	
	erts_printf("fd=%d ", (int) fd);
	
#if defined(HAVE_FSTAT) && !defined(NO_FSTAT_ON_SYS_FD_TYPE)
	if (fstat((int) fd, &stat_buf) < 0)
	    erts_printf("type=unknown ");
	else {
	    erts_printf("type=");
#ifdef S_ISSOCK
	    if (S_ISSOCK(stat_buf.st_mode))
		erts_printf("sock ");
	    else
#endif
#ifdef S_ISFIFO
	    if (S_ISFIFO(stat_buf.st_mode))
		erts_printf("fifo ");
	    else
#endif
#ifdef S_ISCHR
	    if (S_ISCHR(stat_buf.st_mode))
		erts_printf("chr ");
	    else
#endif
#ifdef S_ISDIR
	    if (S_ISDIR(stat_buf.st_mode))
		erts_printf("dir ");
	    else
#endif
#ifdef S_ISBLK
	    if (S_ISBLK(stat_buf.st_mode))
		erts_printf("blk ");
	    else
#endif
#ifdef S_ISREG
	    if (S_ISREG(stat_buf.st_mode))
		erts_printf("reg ");
	    else
#endif
#ifdef S_ISLNK
	    if (S_ISLNK(stat_buf.st_mode))
		erts_printf("lnk ");
	    else
#endif
#ifdef S_ISDOOR
	    if (S_ISDOOR(stat_buf.st_mode))
		erts_printf("door ");
	    else
#endif
#ifdef S_ISWHT
	    if (S_ISWHT(stat_buf.st_mode))
		erts_printf("wht ");
	    else
#endif
#ifdef S_ISXATTR
	    if (S_ISXATTR(stat_buf.st_mode))
		erts_printf("xattr ");
	    else
#endif
		erts_printf("unknown ");
	}
#else
	erts_printf("type=unknown ");
#endif

	if (state->type == ERTS_EV_TYPE_DRV_SEL) {
	    erts_printf("driver_select ");
	    
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
	    if (internal) {
		erts_printf("internal ");
		err = 1;		    
	    }
	    
	    if (cio_events == ep_events) {
		erts_printf("ev=");
		if (print_events(cio_events) != 0)
		    err = 1;
	    }
	    else {
		err = 1;
		erts_printf("cio_ev=");
		print_events(cio_events);
		erts_printf(" ep_ev=");
		print_events(ep_events);
	    }
#else
	    if (print_events(cio_events) != 0)
		err = 1;
#endif
	    erts_printf(" ");
	    if (cio_events & ERTS_POLL_EV_IN) {
		Eterm id = state->driver.select->inport;
		if (is_nil(id)) {
		    erts_printf("inport=none inname=none indrv=none ");
		    err = 1;
		}
		else {
		    ErtsPortNames *pnp = erts_get_port_names(id, ERTS_INVALID_ERL_DRV_PORT);
		    erts_printf(" inport=%T inname=%s indrv=%s ",
				id,
				pnp->name ? pnp->name : "unknown",
				(pnp->driver_name
				 ? pnp->driver_name
				 : "unknown"));
		    erts_free_port_names(pnp);
		}
	    }
	    if (cio_events & ERTS_POLL_EV_OUT) {
		Eterm id = state->driver.select->outport;
		if (is_nil(id)) {
		    erts_printf("outport=none outname=none outdrv=none ");
		    err = 1;
		}
		else {
		    ErtsPortNames *pnp = erts_get_port_names(id, ERTS_INVALID_ERL_DRV_PORT);
		    erts_printf(" outport=%T outname=%s outdrv=%s ",
				id,
				pnp->name ? pnp->name : "unknown",
				(pnp->driver_name
				 ? pnp->driver_name
				 : "unknown"));
		    erts_free_port_names(pnp);
		}
	    }
	}
        else if (state->type == ERTS_EV_TYPE_NIF) {
            ErtsResource* r;
            erts_printf("enif_select ");

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
            if (internal) {
                erts_printf("internal ");
                err = 1;
            }

            if (cio_events == ep_events) {
                erts_printf("ev=");
                if (print_events(cio_events) != 0)
                    err = 1;
            }
            else {
                err = 1;
                erts_printf("cio_ev=");
                print_events(cio_events);
                erts_printf(" ep_ev=");
                print_events(ep_events);
            }
#else
            if (print_events(cio_events) != 0)
                err = 1;
#endif
            erts_printf(" inpid=%T dd_cnt=%b32d", state->driver.nif->in.pid,
                        state->driver.nif->in.ddeselect_cnt);
            erts_printf(" outpid=%T dd_cnt=%b32d", state->driver.nif->out.pid,
                        state->driver.nif->out.ddeselect_cnt);
            r = state->driver.stop.resource;
            erts_printf(" resource=%p(%T:%T)", r, r->type->module, r->type->name);
        }
#if ERTS_CIO_HAVE_DRV_EVENT
	else if (state->type == ERTS_EV_TYPE_DRV_EV) {
	    Eterm id;
	    erts_printf("driver_event ");
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
	    if (internal) {
		erts_printf("internal ");
		err = 1;		    
	    }
	    if (cio_events == ep_events) {
		erts_printf("ev=0x%b32x", (Uint32) cio_events);
	    }
	    else {
		err = 1;
		erts_printf("cio_ev=0x%b32x", (Uint32) cio_events);
		erts_printf(" ep_ev=0x%b32x", (Uint32) ep_events);
	    }
#else
	    erts_printf("ev=0x%b32x", (Uint32) cio_events);
#endif
	    id = state->driver.event->port;
	    if (is_nil(id)) {
		erts_printf(" port=none name=none drv=none ");
		err = 1;
	    }
	    else {
		ErtsPortNames *pnp = erts_get_port_names(id, ERTS_INVALID_ERL_DRV_PORT);
		erts_printf(" port=%T name=%s drv=%s ",
			    id,
			    pnp->name ? pnp->name : "unknown",
			    (pnp->driver_name
			     ? pnp->driver_name
			     : "unknown"));
		erts_free_port_names(pnp);
	    }
	}
#endif
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
	else if (internal) {
	    erts_printf("internal ");
	    if (cio_events) {
		err = 1;
		erts_printf("cio_ev=");
		print_events(cio_events);
	    }
	    if (ep_events) {
		erts_printf("ep_ev=");
		print_events(ep_events);
	    }
	}
#endif
	else {
	    err = 1;
	    erts_printf("control_type=%d ", (int)state->type);
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
	    if (cio_events == ep_events) {
                erts_printf("ev=");
                print_events(cio_events);
	    }
	    else {
		erts_printf("cio_ev="); print_events(cio_events);
		erts_printf(" ep_ev="); print_events(ep_events);
	    }
#else
	    erts_printf("ev=0x%b32x", (Uint32) cio_events);
#endif
	}
	
	if (err) {
	    counters->num_errors++;
	    erts_printf(" ERROR");
	}
	erts_printf("\n");
    }
}
    
int
ERTS_CIO_EXPORT(erts_check_io_debug)(ErtsCheckIoDebugInfo *ciodip)
{
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    int fd, len;
#endif
    IterDebugCounters counters;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    ErtsDrvEventState null_des;

    null_des.driver.select = NULL;
#if ERTS_CIO_HAVE_DRV_EVENT
    null_des.driver.event = NULL;
#endif
    null_des.driver.stop.drv_ptr = NULL;
    null_des.events = 0;
    null_des.remove_cnt = 0;
    null_des.type = ERTS_EV_TYPE_NONE;
#endif

    erts_printf("--- fds in pollset --------------------------------------\n");

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    erts_smp_thr_progress_block(); /* stop the world to avoid messy locking */

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    counters.epep = erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsPollEvents)*max_fds);
    ERTS_POLL_EXPORT(erts_poll_get_selected_events)(pollset.ps, counters.epep, max_fds);
    counters.internal_fds = 0;
#endif
    counters.used_fds = 0;
    counters.num_errors = 0;
    counters.no_driver_select_structs = 0;
    counters.no_driver_event_structs = 0;

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    len = erts_smp_atomic_read_nob(&drv_ev_state_len);
    for (fd = 0; fd < len; fd++) {
	doit_erts_check_io_debug((void *) &drv_ev_state[fd], (void *) &counters);
    }
    for ( ; fd < max_fds; fd++) {
	null_des.fd = fd;
	doit_erts_check_io_debug((void *) &null_des, (void *) &counters);
    }
#else
    safe_hash_for_each(&drv_ev_state_tab, &doit_erts_check_io_debug, (void *) &counters);
#endif

    erts_smp_thr_progress_unblock();

    ciodip->no_used_fds = counters.used_fds;
    ciodip->no_driver_select_structs = counters.no_driver_select_structs;
    ciodip->no_driver_event_structs = counters.no_driver_event_structs;

    erts_printf("\n");
    erts_printf("used fds=%d\n", counters.used_fds);
    erts_printf("Number of driver_select() structures=%d\n", counters.no_driver_select_structs);
#if ERTS_CIO_HAVE_DRV_EVENT
    erts_printf("Number of driver_event() structures=%d\n", counters.no_driver_event_structs);
#endif
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    erts_printf("internal fds=%d\n", counters.internal_fds);
#endif
    erts_printf("---------------------------------------------------------\n");
    fflush(stdout);
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    erts_free(ERTS_ALC_T_TMP, (void *) counters.epep);
#endif

    return counters.num_errors;
}

