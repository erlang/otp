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

/*
 * Description:	Check I/O
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERL_CHECK_IO_C__
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

#if 0
#define DEBUG_PRINT(FMT, ...) erts_printf(FMT "\r\n", ##__VA_ARGS__)
#define DEBUG_PRINT_FD(FMT, STATE, ...)                                 \
    DEBUG_PRINT("%d: " FMT " (ev=%s, ac=%s, flg=%d)",                   \
                (STATE) ? (STATE)->fd : (ErtsSysFdType)-1, ##__VA_ARGS__, \
                ev2str((STATE) ? (STATE)->events : ERTS_POLL_EV_NONE),  \
                ev2str((STATE) ? (STATE)->active_events : ERTS_POLL_EV_NONE), \
                (STATE) ? (STATE)->flags : ERTS_EV_FLAG_CLEAR)
#define DEBUG_PRINT_MODE
#else
#define DEBUG_PRINT(...)
#endif

#ifndef DEBUG_PRINT_FD
#define DEBUG_PRINT_FD(...)
#endif

#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
#  include "safe_hash.h"
#  define DRV_EV_STATE_HTAB_SIZE 1024
#endif

typedef enum {
    ERTS_EV_TYPE_NONE     = 0,
    ERTS_EV_TYPE_DRV_SEL  = 1, /* driver_select */
    ERTS_EV_TYPE_STOP_USE = 2, /* pending stop_select */
    ERTS_EV_TYPE_NIF      = 3, /* enif_select */
    ERTS_EV_TYPE_STOP_NIF = 4  /* pending nif stop */
} EventStateType;

typedef enum {
    ERTS_EV_FLAG_CLEAR         = 0,
    ERTS_EV_FLAG_USED          = 1,   /* ERL_DRV_USE has been turned on */
#ifdef ERTS_ENABLE_KERNEL_POLL
    ERTS_EV_FLAG_FALLBACK      = 2,   /* Set when kernel poll rejected fd
                                         and it was put in the nkp version */
#else
    ERTS_EV_FLAG_FALLBACK      = ERTS_EV_FLAG_CLEAR,
#endif

    /* Combinations */
    ERTS_EV_FLAG_USED_FALLBACK = ERTS_EV_FLAG_USED | ERTS_EV_FLAG_FALLBACK
} EventStateFlags;

#define flag2str(flags)                                                 \
    ((flags) == ERTS_EV_FLAG_CLEAR ? "CLEAR" :                          \
     ((flags) == ERTS_EV_FLAG_USED ? "USED" :                           \
      ((flags) == ERTS_EV_FLAG_FALLBACK ? "FLBK" :                      \
       ((flags) == ERTS_EV_FLAG_USED_FALLBACK ? "USED|FLBK" : "ERROR"))))

/* How many events that can be handled at once by one erts_poll_wait call */
#define ERTS_CHECK_IO_POLL_RES_LEN 512

/* Each I/O Poll Thread has one ErtsPollThread each. The ps field
   can point to either a private ErtsPollSet or a shared one.
   At the moment only kqueue and epoll pollsets can be
   shared across threads.
*/
typedef struct erts_poll_thread
{
    ErtsPollSet *ps;
    ErtsPollResFd *pollres;
    int pollres_len;
} ErtsPollThread;

/* pollsetv contains pointers to the ErtsPollSets that are in use.
 * Which pollset to use is determined by hashing the fd.
 */
static ErtsPollSet **pollsetv;
#if ERTS_POLL_USE_FALLBACK
static ErtsPollSet *flbk_pollset;
#endif
static ErtsPollThread *psiv;

typedef struct {
#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    SafeHashBucket hb;
#endif
    ErtsSysFdType fd;
    struct {
	ErtsDrvSelectDataState *select;   /* ERTS_EV_TYPE_DRV_SEL */
        ErtsNifSelectDataState *nif;      /* ERTS_EV_TYPE_NIF */
        union {
            erts_driver_t*  drv_ptr;    /* ERTS_EV_TYPE_STOP_USE */
            ErtsResource* resource;   /* ERTS_EV_TYPE_STOP_NIF */
        } stop;
    } driver;
    ErtsPollEvents events; /* The events that have been selected upon */
    ErtsPollEvents active_events; /* The events currently active in the pollset */
    EventStateType type;
    EventStateFlags flags;
} ErtsDrvEventState;

struct drv_ev_state_shared {

    union {
        erts_mtx_t lck;
        byte _cache_line_alignment[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_mtx_t))];
    } locks[ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT];

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    int max_fds;
    erts_atomic_t len;
    ErtsDrvEventState *v;
    erts_mtx_t grow_lock; /* prevent lock-hogging of racing growers */
#else
    SafeHash tab;
    int num_prealloc;
    ErtsDrvEventState *prealloc_first;
    erts_spinlock_t prealloc_lock;
#endif
};

int ERTS_WRITE_UNLIKELY(erts_no_pollsets) = 1;
int ERTS_WRITE_UNLIKELY(erts_no_poll_threads) = 1;
struct drv_ev_state_shared drv_ev_state;

static ERTS_INLINE int fd_hash(ErtsSysFdType fd) {
    int hash = (int)fd;
# ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    hash ^= (hash >> 9);
# endif
    return hash;
}

static ERTS_INLINE erts_mtx_t* fd_mtx(ErtsSysFdType fd)
{
    return &drv_ev_state.locks[fd_hash(fd) % ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT].lck;
}

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS

static ERTS_INLINE ErtsDrvEventState *get_drv_ev_state(ErtsSysFdType fd)
{
    return &drv_ev_state.v[(int) fd];
}

#define new_drv_ev_state(State, fd) (State)
#define erase_drv_ev_state(State)

static ERTS_INLINE int grow_drv_ev_state(ErtsSysFdType fd) {
    int i;
    int old_len;
    int new_len;

    if ((unsigned)fd >= (unsigned)erts_atomic_read_nob(&drv_ev_state.len)) {

	if (fd < 0 || fd >= drv_ev_state.max_fds)
            return 0;

        erts_mtx_lock(&drv_ev_state.grow_lock);
        old_len = erts_atomic_read_nob(&drv_ev_state.len);
        if (fd >= old_len) {
            new_len = erts_poll_new_table_len(old_len, fd + 1);
            if (new_len > drv_ev_state.max_fds)
                new_len = drv_ev_state.max_fds;

            for (i=0; i<ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT; i++) { /* lock all fd's */
                erts_mtx_lock(&drv_ev_state.locks[i].lck);
            }
            drv_ev_state.v = (drv_ev_state.v
                              ? erts_realloc(ERTS_ALC_T_DRV_EV_STATE,
                                             drv_ev_state.v,
                                             sizeof(ErtsDrvEventState)*new_len)
                              : erts_alloc(ERTS_ALC_T_DRV_EV_STATE,
                                           sizeof(ErtsDrvEventState)*new_len));
            ERTS_CT_ASSERT(ERTS_EV_TYPE_NONE == 0);
            sys_memzero(drv_ev_state.v+old_len,
                        sizeof(ErtsDrvEventState) * (new_len - old_len));
            for (i = old_len; i < new_len; i++) {
                drv_ev_state.v[i].fd = (ErtsSysFdType) i;
            }
            erts_atomic_set_nob(&drv_ev_state.len, new_len);
            for (i=0; i<ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT; i++) {
                erts_mtx_unlock(&drv_ev_state.locks[i].lck);
            }
        }
        /*else already grown by racing thread */

        erts_mtx_unlock(&drv_ev_state.grow_lock);
    }
    return 1;
}

static int drv_ev_state_len(void)
{
    return erts_atomic_read_nob(&drv_ev_state.len);
}

#else /* !ERTS_SYS_CONTINOUS_FD_NUMBERS */

static ERTS_INLINE ErtsDrvEventState *get_drv_ev_state(ErtsSysFdType fd)
{
    ErtsDrvEventState tmpl;
    tmpl.fd = fd;
    return  (ErtsDrvEventState *) safe_hash_get(&drv_ev_state.tab, (void *) &tmpl);
}

static ERTS_INLINE ErtsDrvEventState* new_drv_ev_state(ErtsDrvEventState *state,
                                                       ErtsSysFdType fd)
{
    ErtsDrvEventState tmpl;

    if (state)
        return state;

    tmpl.fd = fd;
    tmpl.driver.select = NULL;
    tmpl.driver.nif = NULL;
    tmpl.driver.stop.drv_ptr = NULL;
    tmpl.events = 0;
    tmpl.active_events = 0;
    tmpl.type = ERTS_EV_TYPE_NONE;
    tmpl.flags = 0;

    return  (ErtsDrvEventState *) safe_hash_put(&drv_ev_state.tab, (void *) &tmpl);
}

static ERTS_INLINE void erase_drv_ev_state(ErtsDrvEventState *state)
{
    safe_hash_erase(&drv_ev_state.tab, (void *) state);
}

static int drv_ev_state_len(void)
{
    return erts_atomic_read_nob(&drv_ev_state.tab.nitems);
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
static void
steal_pending_stop_use(erts_dsprintf_buf_t*, ErlDrvPort, ErtsDrvEventState*,
                       int mode, int on);
static void
steal_pending_stop_nif(erts_dsprintf_buf_t *dsbufp, ErtsResource*,
                       ErtsDrvEventState *state, int mode, int on);
static ERTS_INLINE void
check_fd_cleanup(ErtsDrvEventState *state,
		 ErtsDrvSelectDataState **free_select,
                 ErtsNifSelectDataState **free_nif);
static ERTS_INLINE void iready(Eterm id, ErtsDrvEventState *state);
static ERTS_INLINE void oready(Eterm id, ErtsDrvEventState *state);
#ifdef DEBUG_PRINT_MODE
static char *drvmode2str(int mode);
static char *nifmode2str(enum ErlNifSelectFlags mode);
#endif

static ERTS_INLINE void
init_iotask(ErtsIoTask *io_task, ErtsSysFdType fd)
{
    erts_port_task_handle_init(&io_task->task);
    io_task->fd = fd;
}

static ERTS_INLINE int
is_iotask_active(ErtsIoTask *io_task)
{
    if (erts_port_task_is_scheduled(&io_task->task))
	return 1;
    return 0;
}

static ERTS_INLINE ErtsDrvSelectDataState *
alloc_drv_select_data(ErtsSysFdType fd)
{
    ErtsDrvSelectDataState *dsp = erts_alloc(ERTS_ALC_T_DRV_SEL_D_STATE,
					     sizeof(ErtsDrvSelectDataState));
    dsp->inport = NIL;
    dsp->outport = NIL;
    init_iotask(&dsp->iniotask, fd);
    init_iotask(&dsp->outiotask, fd);
    return dsp;
}

static ERTS_INLINE ErtsNifSelectDataState *
alloc_nif_select_data(void)
{
    ErtsNifSelectDataState *dsp = erts_alloc(ERTS_ALC_T_NIF_SEL_D_STATE,
					     sizeof(ErtsNifSelectDataState));
    dsp->in.pid = NIL;
    dsp->out.pid = NIL;
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

static ERTS_INLINE int
get_pollset_id(ErtsSysFdType fd)
{
    return fd_hash(fd) % erts_no_pollsets;
}

static ERTS_INLINE ErtsPollSet *
get_pollset(ErtsSysFdType fd)
{
    return pollsetv[get_pollset_id(fd)];
}

#if ERTS_POLL_USE_FALLBACK
static ERTS_INLINE ErtsPollSet *
get_fallback(void)
{
    return flbk_pollset;
}
#endif

/*
 * Place a fd within a pollset. This will automatically use
 * the fallback ps if needed.
 */
static ERTS_INLINE ErtsPollEvents
erts_io_control_wakeup(ErtsDrvEventState *state, ErtsPollOp op,
                       ErtsPollEvents pe, int *wake_poller)
{
    ErtsSysFdType fd = state->fd;
    ErtsPollEvents res = 0;
    EventStateFlags flags = state->flags;

    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(fd_mtx(state->fd)));

    if (!(flags & ERTS_EV_FLAG_FALLBACK)) {
        res = erts_poll_control(get_pollset(fd), fd, op, pe, wake_poller);

#if ERTS_POLL_USE_FALLBACK
        if (op == ERTS_POLL_OP_ADD && res == ERTS_POLL_EV_NVAL) {
            /* When an add fails with NVAL, the poll/kevent operation could not
               put that fd in the pollset, so we instead put it into a fallback pollset */
            state->flags |= ERTS_EV_FLAG_FALLBACK;
            res = erts_poll_control_flbk(get_fallback(), fd, op, pe, wake_poller);
        }
    } else {
        ASSERT(op != ERTS_POLL_OP_ADD);
        res = erts_poll_control_flbk(get_fallback(), fd, op, pe, wake_poller);
#endif
    }

    return res;
}

static ERTS_INLINE ErtsPollEvents
erts_io_control(ErtsDrvEventState *state, ErtsPollOp op, ErtsPollEvents pe)
{
    int wake_poller = 0;
    return erts_io_control_wakeup(state, op, pe, &wake_poller);
}

/* ToDo: Was inline in erl_check_io.h but now need struct erts_poll_thread */
void
erts_io_notify_port_task_executed(ErtsPortTaskType type,
                                  ErtsPortTaskHandle *pthp,
                                  void (*reset_handle)(ErtsPortTaskHandle *))
{
    ErtsIoTask *itp = ErtsContainerStruct(pthp, ErtsIoTask, task);
    ErtsSysFdType fd = itp->fd;
    erts_mtx_t *mtx = fd_mtx(fd);
    int active_events;
    ErtsDrvEventState *state;
    ErtsDrvSelectDataState *free_select = NULL;
    ErtsNifSelectDataState *free_nif = NULL;

    erts_mtx_lock(mtx);
    state = get_drv_ev_state(fd);

    active_events = state->active_events;

    switch (type) {
    case ERTS_PORT_TASK_INPUT:

        DEBUG_PRINT_FD("executed ready_input", state);

        ASSERT(!(state->active_events & ERTS_POLL_EV_IN));
        if (state->events & ERTS_POLL_EV_IN)
            active_events |= ERTS_POLL_EV_IN;
        break;
    case ERTS_PORT_TASK_OUTPUT:

        DEBUG_PRINT_FD("executed ready_output", state);

        ASSERT(!(state->active_events & ERTS_POLL_EV_OUT));
        if (state->events & ERTS_POLL_EV_OUT)
            active_events |= ERTS_POLL_EV_OUT;
        break;
    default:
        erts_exit(ERTS_ABORT_EXIT, "Invalid IO port task type");
        break;
    }

    reset_handle(pthp);

    if (active_events) {
        /* This is not needed if active_events has not changed */
        if (state->active_events != active_events) {
            ErtsPollEvents new_events;
            state->active_events = active_events;
            new_events = erts_io_control(state, ERTS_POLL_OP_MOD, active_events);

            /* We were unable to re-insert the fd into the pollset, signal the callback. */
            if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
                if (active_events & ERTS_POLL_EV_IN)
                    iready(state->driver.select->inport, state);
                if (active_events & ERTS_POLL_EV_OUT)
                    oready(state->driver.select->outport, state);
                state->active_events = 0;
            }
        }
    } else {
        check_fd_cleanup(state, &free_select, &free_nif);
    }

    erts_mtx_unlock(mtx);

    if (free_select)
        free_drv_select_data(free_select);
    if (free_nif)
        free_nif_select_data(free_nif);
}

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
    ErtsPollEvents rm_events;
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(fd_mtx(state->fd)));

    abort_tasks(state, mode);

    if (!mode) {
	rm_events = state->events;
    } else {
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

    state->events &= ~rm_events;
    state->active_events &= ~rm_events;

    if (!(state->events)) {
        erts_io_control(state, ERTS_POLL_OP_DEL, 0);
	switch (state->type) {
        case ERTS_EV_TYPE_NIF:
            state->driver.nif->in.pid = NIL;
            state->driver.nif->out.pid = NIL;
            enif_release_resource(state->driver.stop.resource->data);
            state->driver.stop.resource = NULL;
            break;
	case ERTS_EV_TYPE_DRV_SEL:
	    state->driver.select->inport = NIL;
	    state->driver.select->outport = NIL;
	    break;
	case ERTS_EV_TYPE_NONE:
	    break;
	default:
	    ASSERT(0);
	    break;
	}
	state->type = ERTS_EV_TYPE_NONE;
	state->flags = 0;
    } else {
        ErtsPollEvents new_events =
            erts_io_control(state, ERTS_POLL_OP_MOD, state->active_events);

        /* We were unable to re-insert the fd into the pollset, signal the callback. */
        if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
            if (state->active_events & ERTS_POLL_EV_IN)
                iready(state->driver.select->inport, state);
            if (state->active_events & ERTS_POLL_EV_OUT)
                oready(state->driver.select->outport, state);
            state->active_events = 0;
        }
    }
}

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
#  define IS_FD_UNKNOWN(state) ((state)->type == ERTS_EV_TYPE_NONE)
#else
#  define IS_FD_UNKNOWN(state) ((state) == NULL)
#endif

static ERTS_INLINE void
check_fd_cleanup(ErtsDrvEventState *state,
		 ErtsDrvSelectDataState **free_select,
                 ErtsNifSelectDataState **free_nif)
{
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(fd_mtx(state->fd)));
    *free_select = NULL;
    if (state->driver.select
	&& (state->type != ERTS_EV_TYPE_DRV_SEL)
	&& !is_iotask_active(&state->driver.select->iniotask)
	&& !is_iotask_active(&state->driver.select->outiotask)) {

	*free_select = state->driver.select;
	state->driver.select = NULL;
    }

    *free_nif = NULL;
    if (state->driver.nif && (state->type != ERTS_EV_TYPE_NIF)) {
        *free_nif = state->driver.nif;
        state->driver.nif = NULL;
    }

    if (((state->type != ERTS_EV_TYPE_NONE)
         | (state->driver.nif != NULL)
	 | (state->driver.select != NULL)) == 0) {

	erase_drv_ev_state(state);
    }
}

#ifdef __WIN32__
# define MUST_DEFER(MAY_SLEEP) 1
#else
# define MUST_DEFER(MAY_SLEEP) (MAY_SLEEP)
#endif

int
driver_select(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
{
    void (*stop_select_fn)(ErlDrvEvent, void*) = NULL;
    Port *prt = erts_drvport2port(ix);
    Eterm id = erts_drvport2id(ix);
    ErtsSysFdType fd = (ErtsSysFdType) e;
    ErtsPollEvents ctl_events = (ErtsPollEvents) 0;
    ErtsPollEvents old_events;
    ErtsPollEvents new_events;
    ErtsPollOp ctl_op = ERTS_POLL_OP_MOD;
    ErtsDrvEventState *state;
    int wake_poller = 0;
    int ret;
    ErtsDrvSelectDataState *free_select = NULL;
    ErtsNifSelectDataState *free_nif = NULL;
#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(name, 64);
#endif
    ERTS_MSACC_PUSH_AND_SET_STATE(ERTS_MSACC_STATE_CHECK_IO);

    if (prt == ERTS_INVALID_ERL_DRV_PORT) {
        ERTS_MSACC_POP_STATE();
	return -1;
    }

    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (!grow_drv_ev_state(fd)) {
        if (fd > 0) drv_select_large_fd_error(ix, fd, mode, on);
        ERTS_MSACC_POP_STATE();
        return -1;
    }
#endif

    erts_mtx_lock(fd_mtx(fd));

    state = get_drv_ev_state(fd); /* may be NULL! */

    DEBUG_PRINT_FD("driver_select(%T, %p, %s, %d)",
                   state, id, fd, drvmode2str(mode), on);

    if (!on) {
        if (IS_FD_UNKNOWN(state)) {
            if ((mode&ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE) {
                /* fast track to stop_select callback */
                stop_select_fn = prt->drv_ptr->stop_select;
        #ifdef USE_VM_PROBES
                strncpy(name, prt->drv_ptr->name,
                        sizeof(DTRACE_CHARBUF_NAME(name))-1);
                name[sizeof(name)-1] = '\0';
        #endif
            }
            ret = 0;
            goto done_unknown;
        }
        else if ((mode&ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE) {
            mode |= (ERL_DRV_READ | ERL_DRV_WRITE);
        }
    }

    state = new_drv_ev_state(state, fd);

    switch (state->type) {
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

    }
    default: break;
    }

    if (mode & ERL_DRV_READ) {
	if (state->type == ERTS_EV_TYPE_DRV_SEL) {
	    Eterm owner = state->driver.select->inport;
	    if (owner != id && is_not_nil(owner))
		drv_select_steal(ix, state, mode, on);
	}
        ctl_events = ERTS_POLL_EV_IN;
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

    old_events = state->events;

    if (on) {
        ctl_events &= ~old_events;
        state->events |= ctl_events;
        if (ctl_events & ERTS_POLL_EV_IN && (!state->driver.select || !is_iotask_active(&state->driver.select->iniotask)))
            state->active_events |= ERTS_POLL_EV_IN;
        if (ctl_events & ERTS_POLL_EV_OUT && (!state->driver.select || !is_iotask_active(&state->driver.select->outiotask)))
            state->active_events |= ERTS_POLL_EV_OUT;
        if (old_events == 0 && !(state->flags & ERTS_EV_FLAG_USED)) {
            ctl_op = ERTS_POLL_OP_ADD;
        }
    }
    else {
        ctl_events &= old_events;
        state->events &= ~ctl_events;
        state->active_events &= ~ctl_events;

        if (!state->events) {
            if (!(state->flags & ERTS_EV_FLAG_USED) || mode & ERL_DRV_USE)
                ctl_op = ERTS_POLL_OP_DEL;
        }
    }

    if (ctl_events || ctl_op == ERTS_POLL_OP_DEL) {

        new_events = erts_io_control_wakeup(state, ctl_op,
                                            state->active_events,
                                            &wake_poller);

        ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL || state->type == ERTS_EV_TYPE_NONE);
    }

    if (on) {
        if (ctl_events) {
	    if (!state->driver.select)
		state->driver.select = alloc_drv_select_data(state->fd);
	    if (state->type == ERTS_EV_TYPE_NONE)
		state->type = ERTS_EV_TYPE_DRV_SEL;
	    ASSERT(state->type == ERTS_EV_TYPE_DRV_SEL);
	    if (ctl_events & ERTS_POLL_EV_IN) {
		state->driver.select->inport = id;
                if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL))
                    iready(id, state);
            }
	    if (ctl_events & ERTS_POLL_EV_OUT) {
		state->driver.select->outport = id;
                if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL))
                    oready(id, state);
            }
	    if (mode & ERL_DRV_USE)
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
            if (state->events == 0) {
                if ((mode & ERL_DRV_USE) || !(state->flags & ERTS_EV_FLAG_USED)) {
                    state->type = ERTS_EV_TYPE_NONE;
                    state->flags = 0;
                }
                /*else keep it, as fd will probably be selected upon again */
            }
        }
        if ((mode & ERL_DRV_USE_NO_CALLBACK) == ERL_DRV_USE) {
            erts_driver_t* drv_ptr = prt->drv_ptr;
            ASSERT(state->events==0);
            if (!wake_poller) {
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
   
    ret = 0;

done:

    check_fd_cleanup(state,
		     &free_select,
                     &free_nif);

done_unknown:
    erts_mtx_unlock(fd_mtx(fd));
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

    ERTS_MSACC_POP_STATE();

    return ret;
}

int
enif_select(ErlNifEnv* env,
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
    ErtsPollEvents old_events;
    ErtsPollOp ctl_op = ERTS_POLL_OP_MOD;
    ErtsDrvEventState *state;
    int ret, wake_poller = 0;
    enum { NO_STOP=0, CALL_STOP, CALL_STOP_AND_RELEASE } call_stop = NO_STOP;
    ErtsDrvSelectDataState *free_select = NULL;
    ErtsNifSelectDataState *free_nif = NULL;

    ASSERT(!resource->monitors);

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (!grow_drv_ev_state(fd)) {
        if (fd > 0) nif_select_large_fd_error(fd, mode, resource, ref);
        return INT_MIN | ERL_NIF_SELECT_INVALID_EVENT;
    }
#endif

    erts_mtx_lock(fd_mtx(fd));

    state = get_drv_ev_state(fd); /* may be NULL! */

    DEBUG_PRINT_FD("enif_select(%T, %d, %s, %p, %T, %T)",
                   state, env->proc->common.id, fd, nifmode2str(mode), resource,
                   pid ? pid->pid : THE_NON_VALUE, ref);

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
        ctl_events = ERTS_POLL_EV_IN | ERTS_POLL_EV_OUT;
        ctl_op = ERTS_POLL_OP_DEL;
    }
    else {
        on = 1;
        ASSERT(mode);
        if (mode & ERL_DRV_READ) {
            ctl_events |= ERTS_POLL_EV_IN;
        }
        if (mode & ERL_DRV_WRITE) {
            ctl_events |= ERTS_POLL_EV_OUT;
        }
    }

    state = new_drv_ev_state(state,fd);

    switch (state->type) {
    case ERTS_EV_TYPE_NIF:
        /*
         * Changing resource is considered stealing.
         * Changing process and/or ref is ok (I think?).
         */
        if (state->driver.stop.resource != resource)
            nif_select_steal(state, ERL_DRV_READ | ERL_DRV_WRITE, resource, ref);
        break;
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
    }
    default: break;
    }

    ASSERT((state->type == ERTS_EV_TYPE_NIF) ||
	   (state->type == ERTS_EV_TYPE_NONE && !state->events));

    old_events = state->events;

    if (on) {
        ctl_events &= ~old_events;
        state->events |= ctl_events;
        state->active_events |= ctl_events;
        if (state->type == ERTS_EV_TYPE_NONE)
            ctl_op = ERTS_POLL_OP_ADD;
    }
    else {
        ctl_events &= old_events;
        state->events &= ~ctl_events;
        state->active_events &= ~ctl_events;
    }

    if (ctl_events || ctl_op == ERTS_POLL_OP_DEL) {
        ErtsPollEvents new_events;

        new_events = erts_io_control_wakeup(state, ctl_op,
                                            state->active_events,
                                            &wake_poller);

        if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
            if (state->type == ERTS_EV_TYPE_NIF && !old_events) {
                state->type = ERTS_EV_TYPE_NONE;
                state->flags = 0;
                state->driver.nif->in.pid = NIL;
                state->driver.nif->out.pid = NIL;
                state->driver.stop.resource = NULL;
            }
            ret = INT_MIN | ERL_NIF_SELECT_FAILED;
            goto done;
        }
        ASSERT(new_events == state->events);
    }

    ASSERT(state->type == ERTS_EV_TYPE_NIF
	   || state->type == ERTS_EV_TYPE_NONE);

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
        if (mode & ERL_DRV_READ) {
            state->driver.nif->in.pid = recipient;
            if (is_immed(ref)) {
                state->driver.nif->in.immed = ref;
            } else {
                ASSERT(is_internal_ref(ref));
                refn = internal_ref_numbers(ref);
                state->driver.nif->in.immed = THE_NON_VALUE;
                sys_memcpy(state->driver.nif->in.refn, refn,
                           sizeof(state->driver.nif->in.refn));
            }
        }
        if (mode & ERL_DRV_WRITE) {
            state->driver.nif->out.pid = recipient;
            if (is_immed(ref)) {
                state->driver.nif->out.immed = ref;
            } else {
                ASSERT(is_internal_ref(ref));
                refn = internal_ref_numbers(ref);
                state->driver.nif->out.immed = THE_NON_VALUE;
                sys_memcpy(state->driver.nif->out.refn, refn,
                           sizeof(state->driver.nif->out.refn));
            }
        }
        ret = 0;
    }
    else { /* off */
        if (state->type == ERTS_EV_TYPE_NIF) {
            state->driver.nif->in.pid = NIL;
            state->driver.nif->out.pid = NIL;
        }
        ASSERT(state->events==0);
        if (!wake_poller) {
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
		     &free_select,
                     &free_nif);

done_unknown:
    erts_mtx_unlock(fd_mtx(fd));
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

    return ret;
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
    erts_dsprintf(dsbufp, "stealing control of fd=%d from ", (int) state->fd);
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
	    erts_dsprintf(dsbufp, "no one", (int) state->fd);
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
    case ERTS_EV_TYPE_STOP_USE:
    case ERTS_EV_TYPE_STOP_NIF: {
	ASSERT(0);
	break;
    }
    default:
	erts_dsprintf(dsbufp, "no one\n", (int) state->fd);
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
		  (int) fd,
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
		  (int) fd,
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
		  (int) fd, drv_ev_state.max_fds - 1);
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
        state->flags = 0;
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

        enif_release_resource(state->driver.stop.resource->data);
        state->type = ERTS_EV_TYPE_NONE;
        state->flags = 0;
        state->driver.stop.resource = NULL;
    }
    else {
        erts_dsprintf(dsbufp, "ignored repeated call\n");
    }
    erts_send_error_to_logger_nogl(dsbufp);

}

static ERTS_INLINE int
io_task_schedule_allowed(ErtsDrvEventState *state,
			 ErtsPortTaskType type)
{
    ErtsIoTask *io_task;

    switch (type) {
    case ERTS_PORT_TASK_INPUT:
	if (!state->driver.select)
	    return 0;
	io_task = &state->driver.select->iniotask;
	break;
    case ERTS_PORT_TASK_OUTPUT:
	if (!state->driver.select)
	    return 0;
	io_task = &state->driver.select->outiotask;
	break;
    default:
	ERTS_INTERNAL_ERROR("Invalid I/O-task type");
	return 0;
    }

    return !is_iotask_active(io_task);
}

static ERTS_INLINE void
iready(Eterm id, ErtsDrvEventState *state)
{
    if (io_task_schedule_allowed(state,
				 ERTS_PORT_TASK_INPUT)) {
	ErtsIoTask *iotask = &state->driver.select->iniotask;
	if (erts_port_task_schedule(id,
				    &iotask->task,
				    ERTS_PORT_TASK_INPUT,
				    (ErlDrvEvent) state->fd) != 0) {
	    stale_drv_select(id, state, ERL_DRV_READ);
	} else {
            DEBUG_PRINT_FD("schedule ready_input(%T, %d)",
                           state, id, state->fd);
        }
    }
}

static ERTS_INLINE void
oready(Eterm id, ErtsDrvEventState *state)
{
    if (io_task_schedule_allowed(state,
				 ERTS_PORT_TASK_OUTPUT)) {
	ErtsIoTask *iotask = &state->driver.select->outiotask;
	if (erts_port_task_schedule(id,
				    &iotask->task,
				    ERTS_PORT_TASK_OUTPUT,
				    (ErlDrvEvent) state->fd) != 0) {
	    stale_drv_select(id, state, ERL_DRV_WRITE);
	} else {
            DEBUG_PRINT_FD("schedule ready_output(%T, %d)", state, id, state->fd);
        }
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
        erts_proc_unlock(rp, rp_locks);
}

static void bad_fd_in_pollset(ErtsDrvEventState *, Eterm inport, Eterm outport);

void
erts_check_io_interrupt(ErtsPollThread *psi, int set)
{
    if (psi) {
#if ERTS_POLL_USE_FALLBACK
        if (psi->ps == get_fallback()) {
            erts_poll_interrupt_flbk(psi->ps, set);
            return;
        }
#endif
        erts_poll_interrupt(psi->ps, set);
    }
}

ErtsPollThread *
erts_create_pollset_thread(int id) {
    return psiv+id;
}

void
erts_check_io(ErtsPollThread *psi)
{
    int pollres_len;
    int poll_ret, i;
    ERTS_MSACC_PUSH_AND_SET_STATE(ERTS_MSACC_STATE_CHECK_IO);

 restart:

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    pollres_len = psi->pollres_len;

#if ERTS_POLL_USE_FALLBACK
    if (psi->ps == get_fallback()) {

        poll_ret = erts_poll_wait_flbk(psi->ps, psi->pollres, &pollres_len);

    } else
#endif
    {
        poll_ret = erts_poll_wait(psi->ps, psi->pollres, &pollres_len);
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    if (poll_ret != 0) {

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
        ERTS_MSACC_POP_STATE();
	return;
    }

    for (i = 0; i < pollres_len; i++) {

        erts_driver_t* drv_ptr = NULL;
        ErtsResource* resource = NULL;
        ErtsDrvSelectDataState *free_select = NULL;
        ErtsNifSelectDataState *free_nif = NULL;
	ErtsSysFdType fd = (ErtsSysFdType) ERTS_POLL_RES_GET_FD(&psi->pollres[i]);
	ErtsDrvEventState *state;
        ErtsPollEvents revents;

	erts_mtx_lock(fd_mtx(fd));

	state = get_drv_ev_state(fd);

	if (!state) {
            erts_mtx_unlock(fd_mtx(fd));
            continue;
	}

        revents = ERTS_POLL_RES_GET_EVTS(&psi->pollres[i]);

        DEBUG_PRINT_FD("triggered %s", state, ev2str(revents));

        if (revents & ERTS_POLL_EV_ERR) {
            /*
             * Handle error events by triggering all in/out events
             * that has been selected on.
             * We *do not* want to call a callback that corresponds
             * to an event not selected.
             */
            revents = state->active_events;
            state->active_events = 0;
        } else {

            /* Disregard any events that are not active at the moment,
               for instance this could happen if the driver/nif does
               select/deselect in rapid succession. */
            revents &= state->active_events | ERTS_POLL_EV_NVAL;
            state->active_events &= ~revents;

            /* Reactivate the poll op if there are still active events */
            if (state->active_events) {
                ErtsPollEvents new_events;
                DEBUG_PRINT_FD("re-enable %s", state, ev2str(state->active_events));

                new_events = erts_io_control(state, ERTS_POLL_OP_MOD, state->active_events);

                /* Unable to re-enable the fd, signal all callbacks */
                if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
                    revents |= state->active_events;
                    state->active_events = 0;
                }
            }
        }

	switch (state->type) {
	case ERTS_EV_TYPE_DRV_SEL: { /* Requested via driver_select()... */

            if (revents & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
		if (revents & ERTS_POLL_EV_OUT) {
		    oready(state->driver.select->outport, state);
		}
		/* Someone might have deselected input since revents
		   was read (true also on the non-smp emulator since
		   oready() may have been called); therefore, update
		   revents... */
                revents &= state->events;
		if (revents & ERTS_POLL_EV_IN) {
		    iready(state->driver.select->inport, state);
		}
	    }
	    else if (revents & ERTS_POLL_EV_NVAL) {
		bad_fd_in_pollset(state,
                                  state->driver.select->inport,
                                  state->driver.select->outport);
                check_fd_cleanup(state, &free_select, &free_nif);
	    }
	    break;
	}

        case ERTS_EV_TYPE_NIF: { /* Requested via enif_select()... */
            struct erts_nif_select_event in = {NIL};
            struct erts_nif_select_event out = {NIL};
            ErtsResource* resource = NULL;

            if (revents & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
                if (revents & ERTS_POLL_EV_OUT) {
                    if (is_not_nil(state->driver.nif->out.pid)) {
                        out = state->driver.nif->out;
                        resource = state->driver.stop.resource;
                        state->driver.nif->out.pid = NIL;
                    }
                }
                if (revents & ERTS_POLL_EV_IN) {
                    if (is_not_nil(state->driver.nif->in.pid)) {
                        in = state->driver.nif->in;
                        resource = state->driver.stop.resource;
                        state->driver.nif->in.pid = NIL;
                    }
                }
                state->events &= ~revents;
            }
            else if (revents & ERTS_POLL_EV_NVAL) {
                bad_fd_in_pollset(state, NIL, NIL);
                check_fd_cleanup(state, &free_select, &free_nif);
            }

            erts_mtx_unlock(fd_mtx(fd));

            if (is_not_nil(in.pid)) {
                send_event_tuple(&in, resource, am_ready_input);
            }
            if (is_not_nil(out.pid)) {
                send_event_tuple(&out, resource, am_ready_output);
            }
            continue;
        }

        case ERTS_EV_TYPE_STOP_NIF: {
            resource = state->driver.stop.resource;
            state->type = ERTS_EV_TYPE_NONE;
            goto case_ERTS_EV_TYPE_NONE;
        }

        case ERTS_EV_TYPE_STOP_USE: {
#if ERTS_POLL_USE_FALLBACK
            ASSERT(psi->ps == get_fallback());
#endif
            drv_ptr = state->driver.stop.drv_ptr;
            state->type = ERTS_EV_TYPE_NONE;
            /* fallthrough */
	case ERTS_EV_TYPE_NONE: /* Deselected ... */
        case_ERTS_EV_TYPE_NONE:
            ASSERT(!state->events && !state->active_events && !state->flags);
            check_fd_cleanup(state, &free_select, &free_nif);
	    break;
        }

	default: { /* Error */
	    erts_dsprintf_buf_t *dsbufp;
	    dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Invalid event request type for fd in erts_poll()! "
			  "fd=%d, event request type=%d\n", (int) state->fd,
			  (int) state->type);
	    ASSERT(0);
	    deselect(state, 0);
	    break;
	}
	}

	erts_mtx_unlock(fd_mtx(fd));

        if (drv_ptr) {
            int was_unmasked = erts_block_fpe();
            DTRACE1(driver_stop_select, drv_ptr->name);
            LTTNG1(driver_stop_select, drv_ptr->name);
            (*drv_ptr->stop_select)((ErlDrvEvent) fd, NULL);
            erts_unblock_fpe(was_unmasked);
            if (drv_ptr->handle) {
		erts_ddll_dereference_driver(drv_ptr->handle);
	    }
        }
        if (resource) {
            erts_resource_stop(resource, (ErlNifEvent)fd, 1);
            enif_release_resource(resource->data);
        }
        if (free_select)
            free_drv_select_data(free_select);
        if (free_nif)
            free_nif_select_data(free_nif);
    }

    /* The entire pollres array was filled with events,
     * grow it for the next call. We do this for two reasons:
     * 1. Pulling out more events in on go will increase throughput
     * 2. If the polling implementation is not fair, this will make
     *    sure that we get all fds that we can. i.e. if 12 fds are
     *    constantly active, but we only have a pollres_len of 10,
     *    two of the fds may never be triggered depending on what the
     *    kernel decides to do.
     **/
    if (pollres_len == psi->pollres_len) {
        int ev_state_len = drv_ev_state_len();
        erts_free(ERTS_ALC_T_POLLSET, psi->pollres);
        psi->pollres_len *= 2;
        /* Never grow it larger than the current drv_ev_state.len size */
        if (psi->pollres_len > ev_state_len)
            psi->pollres_len = ev_state_len;
        psi->pollres = erts_alloc(ERTS_ALC_T_POLLSET,
                                  sizeof(ErtsPollResFd) * psi->pollres_len);
    }

    ERTS_MSACC_POP_STATE();
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
    erts_spin_lock(&drv_ev_state.prealloc_lock);
    if (drv_ev_state.prealloc_first == NULL) {
	erts_spin_unlock(&drv_ev_state.prealloc_lock);
	evstate = (ErtsDrvEventState *) 
	    erts_alloc(ERTS_ALC_T_DRV_EV_STATE, sizeof(ErtsDrvEventState));
    } else {
	evstate = drv_ev_state.prealloc_first;
	drv_ev_state.prealloc_first = (ErtsDrvEventState *) evstate->hb.next;
	--drv_ev_state.num_prealloc;
	erts_spin_unlock(&drv_ev_state.prealloc_lock);
    }
    /* XXX: Already valid data if prealloced, could ignore template! */
    *evstate = *((ErtsDrvEventState *) des_tmpl);

    return (void *) evstate;
}

static void drv_ev_state_free(void *des)
{
    erts_spin_lock(&drv_ev_state.prealloc_lock);
    ((ErtsDrvEventState *) des)->hb.next = &drv_ev_state.prealloc_first->hb;
    drv_ev_state.prealloc_first = (ErtsDrvEventState *) des;
    ++drv_ev_state.num_prealloc;
    erts_spin_unlock(&drv_ev_state.prealloc_lock);
}
#endif

#define ERTS_MAX_NO_OF_POLL_THREADS ERTS_MAX_NO_OF_SCHEDULERS

static char *
get_arg(char* rest, char** argv, int* ip)
{
    int i = *ip;
    if (*rest == '\0') {
	if (argv[i+1] == NULL) {
	    erts_fprintf(stderr, "too few arguments\n");
	    erts_usage();
	}
        argv[i++] = NULL;
        rest = argv[i];
    }
    argv[i] = NULL;
    *ip = i;
    return rest;
}

static void
parse_args(int *argc, char **argv, int concurrent_waiters)
{
    int i = 0, j;
    int no_pollsets = 0, no_poll_threads = 0,
        no_pollsets_percentage = 0,
        no_poll_threads_percentage = 0;
    ASSERT(argc && argv);
    while (i < *argc) {
	if(argv[i][0] == '-') {
	    switch (argv[i][1]) {
            case 'I': {
                if (strncmp(argv[i]+2, "Ot", 2) == 0) {
                    char *arg = get_arg(argv[i]+4, argv, &i);
                    if (sscanf(arg, "%d", &no_poll_threads) != 1 ||
                        no_poll_threads < 1 ||
                        ERTS_MAX_NO_OF_POLL_THREADS < no_poll_threads) {
                        erts_fprintf(stderr,"bad I/O poll threads number: %s\n", arg);
                        erts_usage();
                    }
                } else if (strncmp(argv[i]+2, "Op", 3) == 0) {
                    char *arg = get_arg(argv[i]+4, argv, &i);
                    if (sscanf(arg, "%d", &no_pollsets) != 1 ||
                        no_pollsets < 1) {
                        erts_fprintf(stderr,"bad I/O pollset number: %s\n", arg);
                        erts_usage();
                    }
                } else if (strncmp(argv[i]+2, "OPt", 4) == 0) {
                    char *arg = get_arg(argv[i]+5, argv, &i);
                    if (sscanf(arg, "%d", &no_poll_threads_percentage) != 1 ||
                        no_poll_threads_percentage < 0 ||
                        no_poll_threads_percentage > 100) {
                        erts_fprintf(stderr,"bad I/O poll thread percentage number: %s\n", arg);
                        erts_usage();
                    }
                } else if (strncmp(argv[i]+2, "OPp", 4) == 0) {
                    char *arg = get_arg(argv[i]+5, argv, &i);
                    if (sscanf(arg, "%d", &no_pollsets_percentage) != 1 ||
                        no_pollsets_percentage < 0 ||
                        no_pollsets_percentage > 100) {
                        erts_fprintf(stderr,"bad I/O pollset percentage number: %s\n", arg);
                        erts_usage();
                    }
                } else {
                    break;
                }
                break;
            }
            case 'K':
                (void)get_arg(argv[i]+2, argv, &i);
                break;
            case '-':
                goto args_parsed;
            default:
                break;
            }
        }
        i++;
    }

args_parsed:

    if (!concurrent_waiters) {
        no_pollsets = no_poll_threads;
        no_pollsets_percentage = 100;
    }

    if (no_poll_threads == 0) {
        if (no_poll_threads_percentage == 0)
            no_poll_threads = 1; /* This is the default */
        else {
            no_poll_threads = erts_no_schedulers * no_poll_threads_percentage / 100;
            if (no_poll_threads < 1)
                no_poll_threads = 1;
        }
    }

    if (no_pollsets == 0) {
        if (no_pollsets_percentage == 0)
            no_pollsets = 1; /* This is the default */
        else {
            no_pollsets = no_poll_threads * no_pollsets_percentage / 100;
            if (no_pollsets < 1)
                no_pollsets = 1;
        }
    }

    if (no_poll_threads < no_pollsets) {
        erts_fprintf(stderr,
                     "number of IO poll threads has to be greater or equal to "
                     "the number of \nIO pollsets. Current values are set to: \n"
                     "  -IOt %d -IOp %d\n",
                     no_poll_threads, no_pollsets);
        erts_usage();
    }

    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;

    erts_no_pollsets = no_pollsets;
    erts_no_poll_threads = no_poll_threads;
}

void
erts_init_check_io(int *argc, char **argv)
{
    int j, concurrent_waiters, no_poll_threads;
    ERTS_CT_ASSERT((INT_MIN & (ERL_NIF_SELECT_STOP_CALLED |
                               ERL_NIF_SELECT_STOP_SCHEDULED |
                               ERL_NIF_SELECT_INVALID_EVENT |
                               ERL_NIF_SELECT_FAILED)) == 0);


    erts_poll_init(&concurrent_waiters);
#if ERTS_POLL_USE_FALLBACK
    erts_poll_init_flbk(NULL);
#endif

    parse_args(argc, argv, concurrent_waiters);

    /* Create the actual pollsets */
    pollsetv = erts_alloc(ERTS_ALC_T_POLLSET,sizeof(ErtsPollSet *) * erts_no_pollsets);

    for (j=0; j < erts_no_pollsets; j++)
        pollsetv[j] = erts_poll_create_pollset(j);

#if ERTS_POLL_USE_FALLBACK
    flbk_pollset = erts_poll_create_pollset_flbk(-1);
#endif

    no_poll_threads = erts_no_poll_threads;
#if ERTS_POLL_USE_FALLBACK
    no_poll_threads++;
#endif

    psiv = erts_alloc(ERTS_ALC_T_POLLSET, sizeof(ErtsPollThread) * no_poll_threads);

#if ERTS_POLL_USE_FALLBACK
    psiv[0].pollres_len = ERTS_CHECK_IO_POLL_RES_LEN;
    psiv[0].pollres = erts_alloc(ERTS_ALC_T_POLLSET,
        sizeof(ErtsPollResFd) * ERTS_CHECK_IO_POLL_RES_LEN);
    psiv[0].ps = get_fallback();
    psiv++;
#endif

    for (j = 0; j < erts_no_poll_threads; j++) {
        psiv[j].pollres_len = ERTS_CHECK_IO_POLL_RES_LEN;
        psiv[j].pollres = erts_alloc(ERTS_ALC_T_POLLSET,
                                      sizeof(ErtsPollResFd) * ERTS_CHECK_IO_POLL_RES_LEN);
        psiv[j].ps = pollsetv[j % erts_no_pollsets];
    }

    for (j=0; j < ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT; j++) {
        erts_mtx_init(&drv_ev_state.locks[j].lck, "drv_ev_state", make_small(j),
                          ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_IO);
    }

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    drv_ev_state.max_fds = erts_poll_max_fds();
    erts_atomic_init_nob(&drv_ev_state.len, 0);
    drv_ev_state.v = NULL;
    erts_mtx_init(&drv_ev_state.grow_lock, "drv_ev_state_grow", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_IO);
#else
    {
	SafeHashFunctions hf;
	hf.hash = &drv_ev_state_hash;
	hf.cmp = &drv_ev_state_cmp;
	hf.alloc = &drv_ev_state_alloc;
	hf.free = &drv_ev_state_free;
	drv_ev_state.num_prealloc = 0;
	drv_ev_state.prealloc_first = NULL;
	erts_spinlock_init(&drv_ev_state.prealloc_lock, "state_prealloc", NIL,
                               ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_IO);
	safe_hash_init(ERTS_ALC_T_DRV_EV_STATE, &drv_ev_state.tab, "drv_ev_state_tab",
            ERTS_LOCK_FLAGS_CATEGORY_IO, DRV_EV_STATE_HTAB_SIZE, hf);
    }
#endif
}

int
erts_check_io_max_files(void)
{
#ifdef  ERTS_SYS_CONTINOUS_FD_NUMBERS
    return drv_ev_state.max_fds;
#else
    return erts_poll_max_fds();
#endif
}

Uint
erts_check_io_size(void)
{
    Uint res = 0;
    ErtsPollInfo pi;
    int i;

#if ERTS_POLL_USE_FALLBACK
    erts_poll_info(get_fallback(), &pi);
    res += pi.memory_size;
#endif

    for (i = 0; i < erts_no_pollsets; i++) {
        erts_poll_info(pollsetv[i], &pi);
        res += pi.memory_size;
    }
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    res += sizeof(ErtsDrvEventState) * erts_atomic_read_nob(&drv_ev_state.len);
#else
    res += safe_hash_table_sz(&drv_ev_state.tab);
    {
	SafeHashInfo hi;
	safe_hash_get_info(&hi, &drv_ev_state.tab);
	res += hi.objs * sizeof(ErtsDrvEventState);
    }
    erts_spin_lock(&drv_ev_state.prealloc_lock);
    res += drv_ev_state.num_prealloc * sizeof(ErtsDrvEventState);
    erts_spin_unlock(&drv_ev_state.prealloc_lock);
#endif
    return res;
}

Eterm
erts_check_io_info(void *proc)
{
    Process *p = (Process *) proc;
    Eterm tags[16], values[16], res, list = NIL;
    Uint sz, *szp, *hp, **hpp;
    ErtsPollInfo *piv;
    Sint i, j = 0, len;
    int no_pollsets = erts_no_pollsets + ERTS_POLL_USE_FALLBACK;
    ERTS_CT_ASSERT(ERTS_POLL_USE_FALLBACK == 0 || ERTS_POLL_USE_FALLBACK == 1);

    piv = erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsPollInfo) * no_pollsets);

#if ERTS_POLL_USE_FALLBACK
    erts_poll_info_flbk(get_fallback(), &piv[0]);
    piv[0].poll_threads = 1;
    piv[0].active_fds = 0;
    piv++;
#endif

    for (j = 0; j < erts_no_pollsets; j++) {
        erts_poll_info(pollsetv[j], &piv[j]);
        piv[j].active_fds = 0;
        piv[j].poll_threads = erts_no_poll_threads / erts_no_pollsets;
        if (erts_no_poll_threads % erts_no_pollsets > j)
            piv[j].poll_threads++;
    }

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    i = 0;
    erts_mtx_lock(&drv_ev_state.grow_lock);
    len = erts_atomic_read_nob(&drv_ev_state.len);
    for (i = 0; i < ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT; i++) {
        erts_mtx_lock(&drv_ev_state.locks[i].lck);
        for (j = i; j < len; j+=ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT) {
            ErtsDrvEventState *state = get_drv_ev_state(j);
            int pollsetid = get_pollset_id(j);
            ASSERT(fd_mtx(j) == &drv_ev_state.locks[i].lck);
            if (state->flags & ERTS_EV_FLAG_FALLBACK)
                pollsetid = -1;
            if (state->driver.select
                && (state->type == ERTS_EV_TYPE_DRV_SEL)
                && (is_iotask_active(&state->driver.select->iniotask)
                    || is_iotask_active(&state->driver.select->outiotask)))
                piv[pollsetid].active_fds++;
        }
        erts_mtx_unlock(&drv_ev_state.locks[i].lck);
    }
    erts_mtx_unlock(&drv_ev_state.grow_lock);

    piv[0].memory_size += sizeof(ErtsDrvEventState) * erts_atomic_read_nob(&drv_ev_state.len);
#else
    piv[0].memory_size += safe_hash_table_sz(&drv_ev_state.tab);
    {
        SafeHashInfo hi;
        safe_hash_get_info(&hi, &drv_ev_state.tab);
        piv[0].memory_size += hi.objs * sizeof(ErtsDrvEventState);
    }
    erts_spin_lock(&drv_ev_state.prealloc_lock);
    piv[0].memory_size += drv_ev_state.num_prealloc * sizeof(ErtsDrvEventState);
    erts_spin_unlock(&drv_ev_state.prealloc_lock);
#endif

    hpp = NULL;
    szp = &sz;
    sz = 0;

    piv -= ERTS_POLL_USE_FALLBACK;

 bld_it:

    for (j = no_pollsets-1; j >= 0; j--) {
        i = 0;

        tags[i] = erts_bld_atom(hpp, szp, "name");
        values[i++] = erts_bld_atom(hpp, szp, "erts_poll");

        tags[i] = erts_bld_atom(hpp, szp, "primary");
        values[i++] = erts_bld_atom(hpp, szp, piv[j].primary);

        tags[i] = erts_bld_atom(hpp, szp, "kernel_poll");
        values[i++] = erts_bld_atom(hpp, szp,
                                    piv[j].kernel_poll ? piv[j].kernel_poll : "false");

        tags[i] = erts_bld_atom(hpp, szp, "memory_size");
        values[i++] = erts_bld_uint(hpp, szp, piv[j].memory_size);

        tags[i] = erts_bld_atom(hpp, szp, "total_poll_set_size");
        values[i++] = erts_bld_uint(hpp, szp, piv[j].poll_set_size);

        tags[i] = erts_bld_atom(hpp, szp, "lazy_updates");
        values[i++] = piv[j].lazy_updates ? am_true : am_false;

        tags[i] = erts_bld_atom(hpp, szp, "pending_updates");
        values[i++] = erts_bld_uint(hpp, szp, piv[j].pending_updates);

        tags[i] = erts_bld_atom(hpp, szp, "batch_updates");
        values[i++] = piv[j].batch_updates ? am_true : am_false;

        tags[i] = erts_bld_atom(hpp, szp, "concurrent_updates");
        values[i++] = piv[j].concurrent_updates ? am_true : am_false;

        tags[i] = erts_bld_atom(hpp, szp, "fallback");
        values[i++] = piv[j].is_fallback ? am_true : am_false;

        tags[i] = erts_bld_atom(hpp, szp, "max_fds");
        values[i++] = erts_bld_uint(hpp, szp, piv[j].max_fds);

        tags[i] = erts_bld_atom(hpp, szp, "active_fds");
        values[i++] = erts_bld_uint(hpp, szp, piv[j].active_fds);

        tags[i] = erts_bld_atom(hpp, szp, "poll_threads");
        values[i++] = erts_bld_uint(hpp, szp, piv[j].poll_threads);

        res = erts_bld_2tup_list(hpp, szp, i, tags, values);

        if (!hpp) {
            *szp += 2;
        }
        else {
            list = CONS(*hpp, res, list);
            *hpp += 2;
        }
    }

    if (!hpp) {
	hp = HAlloc(p, sz);
	hpp = &hp;
	szp = NULL;
	goto bld_it;
    }

    erts_free(ERTS_ALC_T_TMP, piv);

    return list;
}

static ERTS_INLINE ErtsPollEvents
print_events(erts_dsprintf_buf_t *dsbufp, ErtsPollEvents ev)
{
    int first = 1;
    if(ev == ERTS_POLL_EV_NONE) {
        erts_dsprintf(dsbufp, "N/A");
        return 0;
    }
    if(ev & ERTS_POLL_EV_IN) {
	ev &= ~ERTS_POLL_EV_IN;
	erts_dsprintf(dsbufp, "%s%s", first ? "" : "|", "IN");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_OUT) {
	ev &= ~ERTS_POLL_EV_OUT;
	erts_dsprintf(dsbufp, "%s%s", first ? "" : "|", "OUT");
	first = 0;
    }
    /* The following should not appear... */
    if(ev & ERTS_POLL_EV_NVAL) {
	erts_dsprintf(dsbufp, "%s%s", first ? "" : "|", "NVAL");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_ERR) {
	erts_dsprintf(dsbufp, "%s%s", first ? "" : "|", "ERR");
	first = 0;
    }
    if (ev)
	erts_dsprintf(dsbufp, "%s0x%b32x", first ? "" : "|", (Uint32) ev);
    return ev;
}

static ERTS_INLINE void
print_flags(erts_dsprintf_buf_t *dsbufp, EventStateFlags f)
{
    const char* delim = "";
    if(f & ERTS_EV_FLAG_USED) {
	erts_dsprintf(dsbufp, "%s","USED");
	delim = "|";
    }
    if(f & ERTS_EV_FLAG_FALLBACK) {
	erts_dsprintf(dsbufp, "%s%s", delim, "FLBK");
        delim = "|";
    }
}

#ifdef DEBUG_PRINT_MODE

static ERTS_INLINE char *
drvmode2str(int mode) {
    switch (mode) {
    case ERL_DRV_READ|ERL_DRV_USE: return "READ|USE";
    case ERL_DRV_WRITE|ERL_DRV_USE: return "WRITE|USE";
    case ERL_DRV_READ|ERL_DRV_WRITE|ERL_DRV_USE: return "READ|WRITE|USE";
    case ERL_DRV_USE: return "USE";
    case ERL_DRV_READ: return "READ";
    case ERL_DRV_WRITE: return "WRITE";
    case ERL_DRV_READ|ERL_DRV_WRITE: return "READ|WRITE";
    default: return "UNKNOWN";
    }
}

static ERTS_INLINE char *
nifmode2str(enum ErlNifSelectFlags mode) {
    switch (mode) {
    case ERL_NIF_SELECT_READ: return "READ";
    case ERL_NIF_SELECT_WRITE: return "WRITE";
    case ERL_NIF_SELECT_STOP: return "STOP";
    default: return "UNKNOWN";
    }
}

#endif

typedef struct {
    int used_fds;
    int num_errors;
    int no_driver_select_structs;
    int no_enif_select_structs;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    int internal_fds;
    ErtsPollEvents *epep;
#endif
} IterDebugCounters;

static int erts_debug_print_checkio_state(erts_dsprintf_buf_t *dsbufp,
                                          ErtsDrvEventState *state,
                                          ErtsPollEvents ep_events,
                                          int internal)
{
#if defined(HAVE_FSTAT) && !defined(NO_FSTAT_ON_SYS_FD_TYPE)
    struct stat stat_buf;
#endif
    ErtsSysFdType fd = state->fd;
    ErtsPollEvents cio_events = state->events;
    int err = 0;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    ErtsPollEvents aio_events = state->active_events;
#endif
    erts_dsprintf(dsbufp, "pollset=%d fd=%d ",
                state->flags & ERTS_EV_FLAG_FALLBACK ? -1 : get_pollset_id(fd), (int) fd);

#if defined(HAVE_FSTAT) && !defined(NO_FSTAT_ON_SYS_FD_TYPE)
    if (fstat((int) fd, &stat_buf) < 0)
        erts_dsprintf(dsbufp, "type=unknown ");
    else {
        erts_dsprintf(dsbufp, "type=");
#ifdef S_ISSOCK
        if (S_ISSOCK(stat_buf.st_mode))
            erts_dsprintf(dsbufp, "sock ");
        else
#endif
#ifdef S_ISFIFO
	    if (S_ISFIFO(stat_buf.st_mode))
		erts_dsprintf(dsbufp, "fifo ");
	    else
#endif
#ifdef S_ISCHR
                if (S_ISCHR(stat_buf.st_mode))
                    erts_dsprintf(dsbufp, "chr ");
                else
#endif
#ifdef S_ISDIR
                    if (S_ISDIR(stat_buf.st_mode))
                        erts_dsprintf(dsbufp, "dir ");
                    else
#endif
#ifdef S_ISBLK
                        if (S_ISBLK(stat_buf.st_mode))
                            erts_dsprintf(dsbufp, "blk ");
                        else
#endif
#ifdef S_ISREG
                            if (S_ISREG(stat_buf.st_mode))
                                erts_dsprintf(dsbufp, "reg ");
                            else
#endif
#ifdef S_ISLNK
                                if (S_ISLNK(stat_buf.st_mode))
                                    erts_dsprintf(dsbufp, "lnk ");
                                else
#endif
#ifdef S_ISDOOR
                                    if (S_ISDOOR(stat_buf.st_mode))
                                        erts_dsprintf(dsbufp, "door ");
                                    else
#endif
#ifdef S_ISWHT
                                        if (S_ISWHT(stat_buf.st_mode))
                                            erts_dsprintf(dsbufp, "wht ");
                                        else
#endif
#ifdef S_ISXATTR
                                            if (S_ISXATTR(stat_buf.st_mode))
                                                erts_dsprintf(dsbufp, "xattr ");
                                            else
#endif
                                                erts_dsprintf(dsbufp, "unknown ");
    }
#else
    erts_dsprintf(dsbufp, "type=unknown ");
#endif

    if (state->type == ERTS_EV_TYPE_DRV_SEL) {
        erts_dsprintf(dsbufp, "driver_select ");

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
        if (internal) {
            erts_dsprintf(dsbufp, "internal ");
            err = 1;
        }
        if (aio_events == cio_events) {
            if (cio_events == ep_events) {
                erts_dsprintf(dsbufp, "ev=");
                if (print_events(dsbufp, cio_events) != 0)
                    err = 1;
            }
            else {
                ErtsPollEvents ev = cio_events;
                if (ev != ep_events && ep_events != ERTS_POLL_EV_NONE)
                    err = 1;
                erts_dsprintf(dsbufp, "cio_ev=");
                print_events(dsbufp, cio_events);
                erts_dsprintf(dsbufp, " ep_ev=");
                print_events(dsbufp, ep_events);
            }
        } else {
            erts_dsprintf(dsbufp, "cio_ev=");
            print_events(dsbufp, cio_events);
            erts_dsprintf(dsbufp, " aio_ev=");
            print_events(dsbufp, aio_events);
            if ((aio_events != ep_events && ep_events != ERTS_POLL_EV_NONE) ||
                (aio_events != 0 && ep_events == ERTS_POLL_EV_NONE)) {
                erts_dsprintf(dsbufp, " ep_ev=");
                print_events(dsbufp, ep_events);
                err = 1;
            }
        }
#else
        if (print_events(dsbufp, cio_events) != 0)
            err = 1;
#endif
        erts_dsprintf(dsbufp, " ");
        if (cio_events & ERTS_POLL_EV_IN) {
            Eterm id = state->driver.select->inport;
            if (is_nil(id)) {
                erts_dsprintf(dsbufp, "inport=none inname=none indrv=none ");
                err = 1;
            }
            else {
                ErtsPortNames *pnp = erts_get_port_names(id, ERTS_INVALID_ERL_DRV_PORT);
                erts_dsprintf(dsbufp, " inport=%T inname=%s indrv=%s ",
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
                erts_dsprintf(dsbufp, "outport=none outname=none outdrv=none ");
                err = 1;
            }
            else {
                ErtsPortNames *pnp = erts_get_port_names(id, ERTS_INVALID_ERL_DRV_PORT);
                erts_dsprintf(dsbufp, " outport=%T outname=%s outdrv=%s ",
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
        erts_dsprintf(dsbufp, "enif_select ");

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
        if (internal) {
            erts_dsprintf(dsbufp, "internal ");
            err = 1;
        }

        if (cio_events == ep_events) {
            erts_dsprintf(dsbufp, "ev=");
            if (print_events(dsbufp, cio_events) != 0)
                err = 1;
        }
        else {
            err = 1;
            erts_dsprintf(dsbufp, "cio_ev=");
            print_events(dsbufp, cio_events);
            erts_dsprintf(dsbufp, " ep_ev=");
            print_events(dsbufp, ep_events);
        }
#else
        if (print_events(dsbufp, cio_events) != 0)
            err = 1;
#endif
        erts_dsprintf(dsbufp, " inpid=%T", state->driver.nif->in.pid);
        erts_dsprintf(dsbufp, " outpid=%T", state->driver.nif->out.pid);
        r = state->driver.stop.resource;
        erts_dsprintf(dsbufp, " resource=%p(%T:%T)", r, r->type->module, r->type->name);
    }
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    else if (internal) {
        erts_dsprintf(dsbufp, "internal ");
        if (cio_events) {
            err = 1;
            erts_dsprintf(dsbufp, "cio_ev=");
            print_events(dsbufp, cio_events);
        }
        if (ep_events) {
            erts_dsprintf(dsbufp, "ep_ev=");
            print_events(dsbufp, ep_events);
        }
    }
#endif
    else {
        err = 1;
        erts_dsprintf(dsbufp, "control_type=%d ", (int)state->type);
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
        if (cio_events == ep_events) {
            erts_dsprintf(dsbufp, "ev=");
            print_events(dsbufp, cio_events);
        }
        else {
            erts_dsprintf(dsbufp, "cio_ev="); print_events(dsbufp, cio_events);
            erts_dsprintf(dsbufp, " ep_ev="); print_events(dsbufp, ep_events);
        }
#else
        erts_dsprintf(dsbufp, "ev=0x%b32x", (Uint32) cio_events);
#endif
    }

    erts_dsprintf(dsbufp, " flags="); print_flags(dsbufp, state->flags);
    if (err) {
        erts_dsprintf(dsbufp, " ERROR");
    }
    erts_dsprintf(dsbufp, "\r\n");
    return err;
}

static void doit_erts_check_io_debug(void *vstate, void *vcounters,
                                     erts_dsprintf_buf_t *dsbufp)
{
    ErtsDrvEventState *state = (ErtsDrvEventState *) vstate;
    IterDebugCounters *counters = (IterDebugCounters *) vcounters;
    int internal = 0;
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    ErtsSysFdType fd = state->fd;
    ErtsPollEvents ep_events = counters->epep[(int) fd];
#else
    ErtsPollEvents ep_events = ERTS_POLL_EV_NONE;
#endif

    if (state->driver.select) {
	counters->no_driver_select_structs++;
        ASSERT(state->events || (ep_events != 0 && ep_events != ERTS_POLL_EV_NONE));
    }
    if (state->driver.nif) {
        counters->no_enif_select_structs++;
        ASSERT(state->events || (ep_events != 0 && ep_events != ERTS_POLL_EV_NONE));
    }

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    if (state->events || (ep_events != 0 && ep_events != ERTS_POLL_EV_NONE)) {
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
	if (erts_debug_print_checkio_state(dsbufp, state, ep_events, internal)) {
	    counters->num_errors++;
	}
    }
}

/* ciodpi can be NULL when called from etp-commands */
int
erts_check_io_debug(ErtsCheckIoDebugInfo *ciodip)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    int fd, len, i;
#endif
    IterDebugCounters counters = {0};
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    ErtsDrvEventState null_des;

    null_des.driver.select = NULL;
    null_des.driver.nif = NULL;
    null_des.driver.stop.drv_ptr = NULL;
    null_des.events = 0;
    null_des.type = ERTS_EV_TYPE_NONE;
    null_des.flags = 0;

    counters.epep = erts_alloc(ERTS_ALC_T_TMP,
                               sizeof(ErtsPollEvents)*drv_ev_state.max_fds);
#endif


#if defined(ERTS_ENABLE_LOCK_CHECK)
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif

    if (ciodip)
        erts_thr_progress_block(); /* stop the world to avoid messy locking */

#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    len = erts_atomic_read_nob(&drv_ev_state.len);

#if ERTS_POLL_USE_FALLBACK
    erts_dsprintf(dsbufp, "--- fds in flbk pollset ---------------------------------\n");
    erts_poll_get_selected_events_flbk(get_fallback(), counters.epep,
                                       drv_ev_state.max_fds);
    for (fd = 0; fd < len; fd++) {
        if (drv_ev_state.v[fd].flags & ERTS_EV_FLAG_FALLBACK)
            doit_erts_check_io_debug(&drv_ev_state.v[fd], &counters, dsbufp);
    }
#endif
    erts_dsprintf(dsbufp, "--- fds in pollset --------------------------------------\n");

    for (i = 0; i < erts_no_pollsets; i++) {
        erts_poll_get_selected_events(pollsetv[i],
                                      counters.epep,
                                      drv_ev_state.max_fds);
        for (fd = 0; fd < len; fd++) {
            if (!(drv_ev_state.v[fd].flags & ERTS_EV_FLAG_FALLBACK)
                && get_pollset_id(fd) == i)
                doit_erts_check_io_debug(&drv_ev_state.v[fd], &counters, dsbufp);
        }
    }
    for (fd = len ; fd < drv_ev_state.max_fds; fd++) {
        null_des.fd = fd;
        doit_erts_check_io_debug(&null_des, &counters, dsbufp);
    }
#else
    safe_hash_for_each(&drv_ev_state.tab, &doit_erts_check_io_debug,
                       &counters, dsbufp);
#endif

    if (ciodip)
        erts_thr_progress_unblock();

    if (ciodip) {
        ciodip->no_used_fds = counters.used_fds;
        ciodip->no_driver_select_structs = counters.no_driver_select_structs;
        ciodip->no_enif_select_structs = counters.no_enif_select_structs;
    }

    erts_dsprintf(dsbufp, "\n");
    erts_dsprintf(dsbufp, "used fds=%d\n", counters.used_fds);
    erts_dsprintf(dsbufp, "Number of driver_select() structures=%d\n", counters.no_driver_select_structs);
    erts_dsprintf(dsbufp, "Number of enif_select() structures=%d\n", counters.no_enif_select_structs);
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    erts_dsprintf(dsbufp, "internal fds=%d\n", counters.internal_fds);
#endif
    erts_dsprintf(dsbufp, "---------------------------------------------------------\n");
    erts_send_error_to_logger_nogl(dsbufp);
#ifdef ERTS_SYS_CONTINOUS_FD_NUMBERS
    erts_free(ERTS_ALC_T_TMP, (void *) counters.epep);
#endif

    return counters.num_errors;
}

#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_update_cio_locks(int enable) {
    int i;
#ifndef ERTS_SYS_CONTINOUS_FD_NUMBERS
    erts_lcnt_enable_hash_lock_count(&drv_ev_state.tab, ERTS_LOCK_FLAGS_CATEGORY_IO, enable);
#else
    (void)enable;
#endif

#if ERTS_POLL_USE_FALLBACK
    erts_lcnt_enable_pollset_lock_count_flbk(get_fallback(), enable);
#endif

    for (i = 0; i < erts_no_pollsets; i++)
        erts_lcnt_enable_pollset_lock_count(pollsetv[i], enable);
}
#endif /* ERTS_ENABLE_LOCK_COUNT */
