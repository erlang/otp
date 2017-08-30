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
 * I/O routines for manipulating ports.
 */

#define ERL_IO_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"

/* must be included BEFORE global.h (since it includes erl_driver.h) */
#include "erl_sys_driver.h"
#include "erl_nif.h"

#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "dist.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_version.h"
#include "error.h"
#include "erl_async.h"
#define ERTS_WANT_EXTERNAL_TAGS
#include "external.h"
#include "dtrace-wrapper.h"
#include "lttng-wrapper.h"
#include "erl_map.h"
#include "erl_bif_unique.h"
#include "erl_hl_timer.h"
#include "erl_time.h"

extern ErlDrvEntry fd_driver_entry;
extern ErlDrvEntry vanilla_driver_entry;
extern ErlDrvEntry spawn_driver_entry;
#ifndef __WIN32__
extern ErlDrvEntry forker_driver_entry;
#endif
extern ErlDrvEntry *driver_tab[]; /* table of static drivers, only used during initialization */

erts_driver_t *driver_list; /* List of all drivers, static and dynamic. */
erts_smp_rwmtx_t erts_driver_list_lock; /* Mutex for driver list */
static erts_smp_tsd_key_t driver_list_lock_status_key; /*stop recursive locks when calling 
							 driver init */
static erts_smp_tsd_key_t driver_list_last_error_key;  /* Save last DDLL error on a 
							  per thread basis (for BC interfaces) */

ErtsPTab erts_port erts_align_attribute(ERTS_CACHE_LINE_SIZE); /* The port table */

const ErlDrvTermData driver_term_nil = (ErlDrvTermData)NIL;

const Port erts_invalid_port = {{ERTS_INVALID_PORT}};

erts_driver_t vanilla_driver;
erts_driver_t spawn_driver;
#ifndef __WIN32__
erts_driver_t forker_driver;
#endif
erts_driver_t fd_driver;

int erts_port_synchronous_ops = 0;
int erts_port_schedule_all_ops = 0;
int erts_port_parallelism = 0;

static erts_atomic64_t bytes_in;
static erts_atomic64_t bytes_out;

static void deliver_result(Port *p, Eterm sender, Eterm pid, Eterm res);
static int init_driver(erts_driver_t *, ErlDrvEntry *, DE_Handle *);
static void terminate_port(Port *p);
static void pdl_init(void);
static int driver_failure_term(ErlDrvPort ix, Eterm term, int eof);
#ifdef ERTS_SMP
static void driver_monitor_lock_pdl(Port *p);
static void driver_monitor_unlock_pdl(Port *p);
#define DRV_MONITOR_LOOKUP_PORT_LOCK_PDL(Port) erts_thr_drvport2port((Port), 1)
#define DRV_MONITOR_LOCK_PDL(Port) driver_monitor_lock_pdl(Port)
#define DRV_MONITOR_UNLOCK_PDL(Port) driver_monitor_unlock_pdl(Port)
#else
#define DRV_MONITOR_LOOKUP_PORT_LOCK_PDL(Port) erts_thr_drvport2port((Port), 0)
#define DRV_MONITOR_LOCK_PDL(Port) /* nothing */
#define DRV_MONITOR_UNLOCK_PDL(Port) /* nothing */
#endif

#define ERL_SMALL_IO_BIN_LIMIT (4*ERL_ONHEAP_BIN_LIMIT)
#define SMALL_WRITE_VEC  16

static ERTS_INLINE ErlIOQueue*
drvport2ioq(ErlDrvPort drvport)
{
    Port *prt = erts_thr_drvport2port(drvport, 0);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return NULL;
    return &prt->ioq;
}

static ERTS_INLINE int
is_port_ioq_empty(Port *pp)
{
    int res;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    if (!pp->port_data_lock)
	res = (pp->ioq.size == 0);
    else {
	ErlDrvPDL pdl = pp->port_data_lock;
	erts_mtx_lock(&pdl->mtx);
	res = (pp->ioq.size == 0);
	erts_mtx_unlock(&pdl->mtx);
    }
    return res;
}

int
erts_is_port_ioq_empty(Port *pp)
{
    return is_port_ioq_empty(pp);
}

Uint
erts_port_ioq_size(Port *pp)
{
    int res;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    if (!pp->port_data_lock)
	res = pp->ioq.size;
    else {
	ErlDrvPDL pdl = pp->port_data_lock;
	erts_mtx_lock(&pdl->mtx);
	res = pp->ioq.size;
	erts_mtx_unlock(&pdl->mtx);
    }
    return (Uint) res;
}

/*
 * Line buffered I/O.
 */
typedef struct line_buf_context {
    LineBuf **b;
    char *buf;
    ErlDrvSizeT left;
    ErlDrvSizeT retlen;
} LineBufContext;

#define LINEBUF_EMPTY 0
#define LINEBUF_EOL 1
#define LINEBUF_NOEOL 2
#define LINEBUF_ERROR -1

#define LINEBUF_STATE(LBC) ((*(LBC).b)->data[0])

#define LINEBUF_DATA(LBC) (((*(LBC).b)->data) + 1)
#define LINEBUF_DATALEN(LBC) ((LBC).retlen)

#define LINEBUF_INITIAL 100

#ifdef USE_VM_PROBES
#define DTRACE_FORMAT_COMMON_PID_AND_PORT(PID, PORT)         \
    DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);       \
    DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);          \
                                                             \
    dtrace_pid_str((PID), process_str);                      \
    dtrace_port_str((PORT), port_str);
#define DTRACE_FORMAT_COMMON_PROC_AND_PORT(PID, PORT)        \
    DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);       \
    DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);          \
                                                             \
    dtrace_proc_str((PID), process_str);                     \
    dtrace_port_str((PORT), port_str);

void
dtrace_drvport_str(ErlDrvPort drvport, char *port_buf)
{
    Port *port = erts_drvport2port(drvport);

    if (port != ERTS_INVALID_ERL_DRV_PORT)
	erts_snprintf(port_buf, DTRACE_TERM_BUF_SIZE, "#Port<%lu.%lu>",
		      port_channel_no(port->common.id),
		      port_number(port->common.id));
    else
	erts_snprintf(port_buf, DTRACE_TERM_BUF_SIZE, "#Port<INVALID>",
		      port_channel_no(port->common.id),
		      port_number(port->common.id));
}

#endif

static ERTS_INLINE void
kill_port(Port *pp)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    ERTS_TRACER_CLEAR(&ERTS_TRACER(pp));
    erts_ptab_delete_element(&erts_port, &pp->common); /* Time of death */
    erts_port_task_free_port(pp);
    /* In non-smp case the port structure may have been deallocated now */
}

#ifdef ERTS_SMP

#ifdef ERTS_ENABLE_LOCK_CHECK
int
erts_lc_is_port_locked(Port *prt)
{
    if (!prt)
	return 0;
    ERTS_SMP_LC_ASSERT(prt->lock);
    return erts_smp_lc_mtx_is_locked(prt->lock);
}
#endif

#endif /* #ifdef ERTS_SMP */

static void initq(Port* prt);

#if defined(ERTS_ENABLE_LOCK_CHECK) || defined(ERTS_ENABLE_LOCK_COUNT)
#define ERTS_PORT_INIT_INSTR_NEED_ID 1
#else
#define ERTS_PORT_INIT_INSTR_NEED_ID 0
#endif

static ERTS_INLINE void port_init_instr(Port *prt
#if ERTS_PORT_INIT_INSTR_NEED_ID
				   , Eterm id
#endif
    )
{
#if !ERTS_PORT_INIT_INSTR_NEED_ID
    Eterm id = NIL; /* Not used */
#endif

    /*
     * Stuff that need to be initialized with the port id
     * in the instrumented case, but not in the normal case.
     */
#ifdef ERTS_SMP
    ASSERT(prt->drv_ptr && prt->lock);
    if (!prt->drv_ptr->lock) {
	char *lock_str = "port_lock";
	erts_mtx_init_locked_x(prt->lock, lock_str, id,
#ifdef ERTS_ENABLE_LOCK_COUNT
			       (erts_lcnt_rt_options & ERTS_LCNT_OPT_PORTLOCK)
#else
			       0
#endif
			       );
    }
#endif
    erts_port_task_init_sched(&prt->sched, id);
}

#if !ERTS_PORT_INIT_INSTR_NEED_ID
static ERTS_INLINE void port_init_instr_abort(Port *prt)
{
#ifdef ERTS_SMP
    ASSERT(prt->drv_ptr && prt->lock);
    if (!prt->drv_ptr->lock) {
	erts_mtx_unlock(prt->lock);
	erts_mtx_destroy(prt->lock);
    }
#endif
    erts_port_task_fini_sched(&prt->sched);
}
#endif

static void insert_port_struct(void *vprt, Eterm data)
{
    Port *prt = (Port *) vprt;
    Eterm id = make_internal_port(data);
#if ERTS_PORT_INIT_INSTR_NEED_ID
    /*
     * This cannot be done earlier in the instrumented
     * case since we don't now 'id' until now.
     */
    port_init_instr(prt, id);
#endif
    prt->common.id = id;
    erts_atomic32_init_relb(&prt->state, ERTS_PORT_SFLG_INITIALIZING);
}

#define ERTS_CREATE_PORT_FLAG_PARALLELISM		(1 << 0)

static Port *create_port(char *name,
			 erts_driver_t *driver,
			 erts_mtx_t *driver_lock,
			 int create_flags,
			 Eterm pid,
			 int *enop)
{
    ErtsPortTaskBusyPortQ *busy_port_queue;
    Port *prt;
    char *p;
    size_t port_size, busy_port_queue_size, size;
    erts_aint32_t state = ERTS_PORT_SFLG_CONNECTED;
    erts_aint32_t x_pts_flgs = 0;

#ifdef ERTS_SMP
    ErtsRunQueue *runq;
    if (!driver_lock) {
	/* Align size for mutex following port struct */
	port_size = size = ERTS_ALC_DATA_ALIGN_SIZE(sizeof(Port));
	size += sizeof(erts_mtx_t);
    }
    else
#endif
	port_size = size = ERTS_ALC_DATA_ALIGN_SIZE(sizeof(Port));

#ifdef DEBUG
    /* Make sure the debug flags survives until port is freed */
    state |= ERTS_PORT_SFLG_PORT_DEBUG;
#endif


    busy_port_queue_size
	= ((driver->flags & ERL_DRV_FLAG_NO_BUSY_MSGQ)
	   ? 0
	   : ERTS_ALC_DATA_ALIGN_SIZE(sizeof(ErtsPortTaskBusyPortQ)));
    size += busy_port_queue_size;

    size += sys_strlen(name) + 1;

    p = erts_alloc_fnf(ERTS_ALC_T_PORT, size);
    if (!p) {
	if (enop)
	    *enop = ENOMEM;
	return NULL;
    }

    prt = (Port *) p;
    p += port_size;

    if (!busy_port_queue_size)
	busy_port_queue = NULL;
    else {
	busy_port_queue = (ErtsPortTaskBusyPortQ *) p;
	p += busy_port_queue_size;
    }

#ifdef ERTS_SMP
    if (driver_lock) {
	prt->lock = driver_lock;
	erts_mtx_lock(driver_lock);
    }
    else {
	prt->lock = (erts_mtx_t *) p;
	p += sizeof(erts_mtx_t);
	state |= ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK;
    }
    if (erts_get_scheduler_data())
        runq = erts_get_runq_current(NULL);
    else
        runq = ERTS_RUNQ_IX(0);
    erts_smp_atomic_set_nob(&prt->run_queue, (erts_aint_t) runq);

    prt->xports = NULL;
#else
    erts_atomic32_init_nob(&prt->refc, 1);
    prt->cleanup = 0;
#endif
    
    erts_port_task_pre_init_sched(&prt->sched, busy_port_queue);

    prt->name = p;
    sys_strcpy(p, name);
    prt->drv_ptr = driver;
    ERTS_P_LINKS(prt) = NULL;
    ERTS_P_MONITORS(prt) = NULL;
    prt->linebuf = NULL;
    prt->suspended = NULL;
    erts_init_port_data(prt);
    prt->port_data_lock = NULL;
    prt->control_flags = 0;
    prt->bytes_in = 0;
    prt->bytes_out = 0;
    prt->dist_entry = NULL;
    ERTS_PORT_INIT_CONNECTED(prt, pid);
    prt->common.u.alive.reg = NULL;
    ERTS_PTMR_INIT(prt);
    erts_port_task_handle_init(&prt->timeout_task);
    erts_smp_atomic_init_nob(&prt->psd, (erts_aint_t) NULL);
    prt->async_open_port = NULL;
    prt->drv_data = (SWord) 0;
    prt->os_pid = -1;

    /* Set default tracing */
    erts_get_default_port_tracing(&ERTS_TRACE_FLAGS(prt), &ERTS_TRACER(prt));

    ERTS_CT_ASSERT(offsetof(Port,common) == 0);

#if !ERTS_PORT_INIT_INSTR_NEED_ID
    /*
     * When 'id' isn't needed (the normal case), it is better to
     * do the initialization here avoiding unnecessary contention
     * on table...
     */
    port_init_instr(prt);
#endif

    if (!erts_ptab_new_element(&erts_port,
			       &prt->common,
			       (void *) prt,
			       insert_port_struct)) {

#if !ERTS_PORT_INIT_INSTR_NEED_ID
	port_init_instr_abort(prt);
#endif
#ifdef ERTS_SMP
	if (driver_lock)
	    erts_mtx_unlock(driver_lock);
#endif
	if (enop)
	    *enop = 0;
	erts_free(ERTS_ALC_T_PORT, prt);
	return NULL;
    }

    ASSERT(prt == (Port *) (erts_ptab_pix2intptr_nob(
				&erts_port,
				internal_port_index(prt->common.id))));

    initq(prt);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (erts_port_schedule_all_ops)
	x_pts_flgs |= ERTS_PTS_FLG_FORCE_SCHED;

    if (create_flags & ERTS_CREATE_PORT_FLAG_PARALLELISM)
	x_pts_flgs |= ERTS_PTS_FLG_PARALLELISM;

    if (x_pts_flgs)
	erts_smp_atomic32_read_bor_nob(&prt->sched.flags, x_pts_flgs);

    erts_atomic32_set_relb(&prt->state, state);
    return prt;
}

#ifndef ERTS_SMP
void
erts_port_cleanup(Port *prt)
{
    if (prt->drv_ptr && prt->drv_ptr->handle)
	erts_ddll_dereference_driver(prt->drv_ptr->handle);
    prt->drv_ptr = NULL;
    erts_port_dec_refc(prt);
}
#endif

void
erts_port_free(Port *prt)
{
#if defined(ERTS_SMP) || defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
    erts_aint32_t state = erts_atomic32_read_nob(&prt->state);
#endif
    ERTS_LC_ASSERT(state & (ERTS_PORT_SFLG_INITIALIZING
			    | ERTS_PORT_SFLG_FREE));
    ASSERT(state & ERTS_PORT_SFLG_PORT_DEBUG);

    ERTS_LC_ASSERT(erts_atomic_read_nob(&prt->common.refc.atmc) == 0);

    erts_port_task_fini_sched(&prt->sched);

    if (prt->async_open_port) {
        erts_free(ERTS_ALC_T_PRTSD, prt->async_open_port);
        prt->async_open_port = NULL;
    }

#ifdef ERTS_SMP
    ASSERT(prt->lock);
    if (state & ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK)
	erts_mtx_destroy(prt->lock);

    /*
     * We cannot dereference a driver using driver
     * locking until here in smp case. Otherwise,
     * the driver lock may still be in use by others.
     *
     * In the non-smp case we cannot do it here since
     * this function may be called by non-scheduler
     * threads. This is done in erts_port_cleanup()
     * in the non-smp case.
     */
    if (prt->drv_ptr->handle)
	erts_ddll_dereference_driver(prt->drv_ptr->handle);
#endif
    erts_free(ERTS_ALC_T_PORT, prt);
}

/*
** Initialize v_start to point to the small fixed vector.
** Once (reallocated) we never reset the pointer to the small vector
** This is a possible optimisation.
*/
static void initq(Port* prt)
{
    ErlIOQueue* q = &prt->ioq;

    ERTS_LC_ASSERT(!prt->port_data_lock);

    q->size = 0;
    q->v_head = q->v_tail = q->v_start = q->v_small;
    q->v_end = q->v_small + SMALL_IO_QUEUE;
    q->b_head = q->b_tail = q->b_start = q->b_small;
    q->b_end = q->b_small + SMALL_IO_QUEUE;
}

static void stopq(Port* prt)
{
    ErlIOQueue* q;
    ErlDrvBinary** binp;

    if (prt->port_data_lock)
	driver_pdl_lock(prt->port_data_lock);

    q = &prt->ioq;
    binp = q->b_head;

    if (q->v_start != q->v_small)
	erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);

    while(binp < q->b_tail) {
	if (*binp != NULL)
	    driver_free_binary(*binp);
	binp++;
    }
    if (q->b_start != q->b_small)
	erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
    q->v_start = q->v_end = q->v_head = q->v_tail = NULL;
    q->b_start = q->b_end = q->b_head = q->b_tail = NULL;
    q->size = 0;

    if (prt->port_data_lock) {
	driver_pdl_unlock(prt->port_data_lock);
	driver_pdl_dec_refc(prt->port_data_lock);
    }
}

int
erts_save_suspend_process_on_port(Port *prt, Process *process)
{
    int saved;
    erts_aint32_t flags;
    erts_port_task_sched_lock(&prt->sched);
    flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
    saved = (flags & ERTS_PTS_FLGS_BUSY) && !(flags & ERTS_PTS_FLG_EXIT);
    if (saved)
	erts_proclist_store_last(&prt->suspended, erts_proclist_create(process));
    erts_port_task_sched_unlock(&prt->sched);
    return saved;
}

/*
   Opens a driver.
   Returns the non-negative port number, if successful.
   If there is an error, -1 or -2 or -3 is returned. -2 means that
   there is valid error information in *error_number_ptr.
   Returning -3 means that an error in the given options was detected
   (*error_number_ptr must contain either BADARG or SYSTEM_LIMIT).
   The driver start function must obey the same conventions.
*/
Port *
erts_open_driver(erts_driver_t* driver,	/* Pointer to driver. */
		 Eterm pid,		/* Current process. */
		 char* name,		/* Driver name. */
		 SysDriverOpts* opts,	/* Options. */
		 int *error_type_ptr,	/* error type */
		 int *error_number_ptr)	/* errno in case of error type -2 */
{

#undef ERTS_OPEN_DRIVER_RET
#define ERTS_OPEN_DRIVER_RET(Prt, EType, ENo)	\
    do {					\
	if (error_type_ptr)			\
	    *error_type_ptr = (EType);		\
	if (error_number_ptr)			\
	    *error_number_ptr = (ENo);		\
	return (Prt);				\
    } while (0)

    ErlDrvData drv_data = 0;
    Port *port;
    int fpe_was_unmasked;
    int error_type, error_number;
    int port_errno = 0;
    erts_mtx_t *driver_lock = NULL;
    int cprt_flgs = 0;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    erts_smp_rwmtx_rlock(&erts_driver_list_lock);
    if (!driver) {
	for (driver = driver_list; driver; driver = driver->next) {
	    if (sys_strcmp(driver->name, name) == 0)
		break;
	}
	if (!driver) { 
	    erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	    ERTS_OPEN_DRIVER_RET(NULL, -3, BADARG);
	}
    }
    if (driver == &spawn_driver) {
	char *p;
	erts_driver_t *d;

	/*
	 * Dig out the name of the driver or port program.
	 */

	if (!(opts->spawn_type & ERTS_SPAWN_EXECUTABLE)) {
	    /* No spawn driver default */
	    driver = NULL;
	}


	if (opts->spawn_type != ERTS_SPAWN_EXECUTABLE) {
	    p = name;
	    while(*p != '\0' && *p != ' ')
		p++;
	    if (*p == '\0')
		p = NULL;
	    else
		*p = '\0';

	    /*
	     * Search for a driver having this name.  Defaults to spawn_driver
	     * if not found.
	     */
	    
	    for (d = driver_list; d; d = d->next) {
		if (strcmp(d->name, name) == 0 && 
		    erts_ddll_driver_ok(d->handle)) {
		    driver = d;
		    break;
		}
	    }
	    if (p != NULL)
		*p = ' ';
	}
    }

    if (driver == NULL || (driver != &spawn_driver && opts->exit_status)) {
	erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	ERTS_OPEN_DRIVER_RET(NULL, -3, BADARG);
    }

#ifdef ERTS_SMP
    driver_lock = driver->lock;
#endif

    if (driver->handle != NULL) {
	erts_ddll_increment_port_count(driver->handle);
	erts_ddll_reference_driver(driver->handle);
    }
    erts_smp_rwmtx_runlock(&erts_driver_list_lock);

    /*
     * We'll set up the port before calling the start function,
     * to allow message sending and setting timers in the start function.
     */

    if (opts->parallelism)
	cprt_flgs |= ERTS_CREATE_PORT_FLAG_PARALLELISM;

    port = create_port(name, driver, driver_lock, cprt_flgs, pid, &port_errno);
    if (!port) {
	if (driver->handle) {
	    erts_smp_rwmtx_rlock(&erts_driver_list_lock);
	    erts_ddll_decrement_port_count(driver->handle);
	    erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	    erts_ddll_dereference_driver(driver->handle);
	}
	if (port_errno)
	    ERTS_OPEN_DRIVER_RET(NULL, -2, port_errno);
	else
	    ERTS_OPEN_DRIVER_RET(NULL, -3, SYSTEM_LIMIT);
    }
    
    if (IS_TRACED_FL(port, F_TRACE_PORTS)) {
	trace_port_open(port,
			pid,
			erts_atom_put((byte *) port->name,
				      strlen(port->name),
				      ERTS_ATOM_ENC_LATIN1,
				      1));
    }

    error_number = error_type = 0;
    if (driver->start) {
        ERTS_MSACC_PUSH_STATE_M();
	if (IS_TRACED_FL(port, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(port, am_in, am_open);
	}
	port->caller = pid;
#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(driver_start)) {
            DTRACE_FORMAT_COMMON_PID_AND_PORT(pid, port)
            DTRACE3(driver_start, process_str, driver->name, port_str);
        }
#endif

	ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);

#ifdef USE_LTTNG_VM_TRACEPOINTS
        if (LTTNG_ENABLED(driver_start)) {
            lttng_decl_portbuf(port_str);
            lttng_decl_procbuf(proc_str);
            lttng_pid_to_str(pid, proc_str);
            lttng_port_to_str(port, port_str);
            LTTNG3(driver_start, proc_str, driver->name, port_str);
        }
#endif

	fpe_was_unmasked = erts_block_fpe();
	drv_data = (*driver->start)(ERTS_Port2ErlDrvPort(port), name, opts);
	if (((SWord) drv_data) == -1)
	    error_type = -1;
	else if (((SWord) drv_data) == -2) {
	    /*
	     * We need to save errno quickly after the
	     * call to the 'start' callback before
	     * something else modify it.
	     */
	    error_type = -2;
	    error_number = errno;
	}
	else if (((SWord) drv_data) == -3) {
	    error_type = -3;
	    error_number = BADARG;
	}

	erts_unblock_fpe(fpe_was_unmasked);
	ERTS_MSACC_POP_STATE_M();
	port->caller = NIL;
	if (IS_TRACED_FL(port, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(port, am_out, am_open);
	}
#ifdef ERTS_SMP
	if (port->xports)
	    erts_port_handle_xports(port);
	ASSERT(!port->xports);
#endif
    }

    if (error_type) {
	/*
	 * Must clean up the port.
	 */
	erts_cancel_port_timer(port);
	stopq(port);
	if (port->linebuf != NULL) {
	    erts_free(ERTS_ALC_T_LINEBUF,
		      (void *) port->linebuf);
	    port->linebuf = NULL;
	}
	if (driver->handle != NULL) {
	    erts_smp_rwmtx_rlock(&erts_driver_list_lock);
	    erts_ddll_decrement_port_count(driver->handle);
	    erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	}
	kill_port(port);
	erts_port_release(port);
	ERTS_OPEN_DRIVER_RET(NULL, error_type, error_number);
    }
    port->drv_data = (UWord) drv_data;
    ERTS_OPEN_DRIVER_RET(port, 0, 0);

#undef ERTS_OPEN_DRIVER_RET
}

#ifdef ERTS_SMP

struct ErtsXPortsList_ {
    ErtsXPortsList *next;
    Port *port;
};

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(xports_list, ErtsXPortsList, 50, ERTS_ALC_T_XPORTS_LIST)

#endif

/*
 * Driver function to create new instances of a driver
 * Historical reason: to be used with inet_drv for creating
 * accept sockets inorder to avoid a global table.
 */
ErlDrvPort
driver_create_port(ErlDrvPort creator_port_ix, /* Creating port */
		   ErlDrvTermData pid,    /* Owner/Caller */
		   char* name,            /* Driver name */
		   ErlDrvData drv_data)   /* Driver data */
{
    int cprt_flgs = 0;
    Port *creator_port;
    Port* port;
    erts_driver_t *driver;
    Process *rp;
    erts_mtx_t *driver_lock = NULL;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    /* Need to be called from a scheduler thread */
    if (!erts_get_scheduler_id())
	return ERTS_INVALID_ERL_DRV_PORT;

    creator_port = erts_drvport2port(creator_port_ix);
    if (creator_port == ERTS_INVALID_ERL_DRV_PORT)
	return ERTS_INVALID_ERL_DRV_PORT;

    rp = erts_proc_lookup(pid);
    if (!rp)
	return ERTS_INVALID_ERL_DRV_PORT;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(creator_port));

    driver = creator_port->drv_ptr;
    erts_smp_rwmtx_rlock(&erts_driver_list_lock);
    if (!erts_ddll_driver_ok(driver->handle)) {
	erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	return ERTS_INVALID_ERL_DRV_PORT;
    }

    if (driver->handle != NULL) {
	erts_ddll_increment_port_count(driver->handle);
	erts_ddll_reference_referenced_driver(driver->handle);
    }

#ifdef ERTS_SMP
    driver_lock = driver->lock;
#endif

    erts_smp_rwmtx_runlock(&erts_driver_list_lock);

    /* Inherit parallelism flag from parent */
    if (ERTS_PTS_FLG_PARALLELISM &
	erts_smp_atomic32_read_nob(&creator_port->sched.flags))
	cprt_flgs |= ERTS_CREATE_PORT_FLAG_PARALLELISM;
    port = create_port(name, driver, driver_lock, cprt_flgs, pid, NULL);
    if (!port) {
	if (driver->handle) {
	    erts_smp_rwmtx_rlock(&erts_driver_list_lock);
	    erts_ddll_decrement_port_count(driver->handle);
	    erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	    erts_ddll_dereference_driver(driver->handle);
	}
	return ERTS_INVALID_ERL_DRV_PORT;
    }
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(port));

    erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
    if (ERTS_PROC_IS_EXITING(rp)) {
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	if (driver->handle) {
	    erts_smp_rwmtx_rlock(&erts_driver_list_lock);
	    erts_ddll_decrement_port_count(driver->handle);
	    erts_smp_rwmtx_runlock(&erts_driver_list_lock);
	}
	kill_port(port);
	erts_port_release(port);
	return ERTS_INVALID_ERL_DRV_PORT;
    }

    erts_add_link(&ERTS_P_LINKS(port), LINK_PID, pid);
    erts_add_link(&ERTS_P_LINKS(rp), LINK_PID, port->common.id);
    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);

#ifdef ERTS_SMP
    if (!driver_lock) {
	ErtsXPortsList *xplp = xports_list_alloc();
	xplp->port = port;
	xplp->next = creator_port->xports;
	creator_port->xports = xplp;
    }
#endif

    port->drv_data = (UWord) drv_data;

    return ERTS_Port2ErlDrvPort(port);
}

#ifdef ERTS_SMP
int erts_port_handle_xports(Port *prt)
{
    int reds = 0;
    ErtsXPortsList *xplp;

    ASSERT(prt);
    xplp = prt->xports;
    ASSERT(xplp);
    while (xplp) {
	Port *rprt = xplp->port;
	ErtsXPortsList *free_xplp;
	erts_aint32_t state;
	if (rprt->xports)
	    reds += erts_port_handle_xports(rprt);
	state = erts_atomic32_read_nob(&rprt->state);
	if ((state & ERTS_PORT_SFLG_CLOSING) && erts_is_port_ioq_empty(rprt)) {
	    terminate_port(rprt);
	    reds += ERTS_PORT_REDS_TERMINATE;
	}
	erts_port_release(rprt);
	free_xplp = xplp;
	xplp = xplp->next;
	xports_list_free(free_xplp);
	reds++;
    }
    prt->xports = NULL;
    return reds;
}
#endif

/* Fills a possibly deep list of chars and binaries into vec
** Small characters are first stored in the buffer buf of length ln
** binaries found are copied and linked into msoh
** Return  vector length on succsess,
**        -1 on overflow
**        -2 on type error
*/

#ifdef DEBUG
#define MAX_SYSIOVEC_IOVLEN (1ull << (32 - 1))
#else
#define MAX_SYSIOVEC_IOVLEN (1ull << (sizeof(((SysIOVec*)0)->iov_len) * 8 - 1))
#endif

static ERTS_INLINE void
io_list_to_vec_set_vec(SysIOVec **iov, ErlDrvBinary ***binv,
                        ErlDrvBinary *bin, byte *ptr, Uint len,
                        int *vlen)
{
    while (len > MAX_SYSIOVEC_IOVLEN) {
        (*iov)->iov_base = ptr;
        (*iov)->iov_len = MAX_SYSIOVEC_IOVLEN;
        ptr += MAX_SYSIOVEC_IOVLEN;
        len -= MAX_SYSIOVEC_IOVLEN;
        (*iov)++;
        (*vlen)++;
        *(*binv)++ = bin;
    }
    (*iov)->iov_base = ptr;
    (*iov)->iov_len = len;
    *(*binv)++ = bin;
    (*iov)++;
    (*vlen)++;
}

static int
io_list_to_vec(Eterm obj,	/* io-list */
	       SysIOVec* iov,	/* io vector */
	       ErlDrvBinary** binv, /* binary reference vector */
	       ErlDrvBinary* cbin, /* binary to store characters */
	       ErlDrvSizeT bin_limit)	/* small binaries limit */
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    byte *buf  = (byte*)cbin->orig_bytes;
    Uint len = cbin->orig_size;
    Uint csize  = 0;
    int vlen   = 0;
    byte* cptr = buf;

    goto L_jump_start;  /* avoid push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0)
		    goto L_overflow;
		*buf++ = unsigned_val(obj);
		csize++;
		len--;
	    } else if (is_binary(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto handle_binary;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;    /* on head */
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }	    
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		goto handle_binary;
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    Eterm real_bin;
	    Uint offset;
	    Eterm* bptr;
	    ErlDrvSizeT size;
	    int bitoffs;
	    int bitsize;

	handle_binary:
	    size = binary_size(obj);
	    ERTS_GET_REAL_BIN(obj, real_bin, offset, bitoffs, bitsize);
	    ASSERT(bitsize == 0);
	    bptr = binary_val(real_bin);
	    if (*bptr == HEADER_PROC_BIN) {
		ProcBin* pb = (ProcBin *) bptr;
		if (bitoffs != 0) {
		    if (len < size) {
			goto L_overflow;
		    }
		    erts_copy_bits(pb->bytes+offset, bitoffs, 1,
				   (byte *) buf, 0, 1, size*8);
		    csize += size;
		    buf += size;
		    len -= size;
		} else if (bin_limit && size < bin_limit) {
		    if (len < size) {
			goto L_overflow;
		    }
		    sys_memcpy(buf, pb->bytes+offset, size);
		    csize += size;
		    buf += size;
		    len -= size;
		} else {
		    if (csize != 0) {
                        io_list_to_vec_set_vec(&iov, &binv, cbin,
                                               cptr, csize, &vlen);
			cptr = buf;
			csize = 0;
		    }
		    if (pb->flags) {
			erts_emasculate_writable_binary(pb);
		    }
                    io_list_to_vec_set_vec(
                        &iov, &binv, Binary2ErlDrvBinary(pb->val),
                        pb->bytes+offset, size, &vlen);
		}
	    } else {
		ErlHeapBin* hb = (ErlHeapBin *) bptr;
		if (len < size) {
		    goto L_overflow;
		}
		copy_binary_to_buffer(buf, 0,
				      ((byte *) hb->data)+offset, bitoffs,
				      8*size);
		csize += size;
		buf += size;
		len -= size;
	    }
	} else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    if (csize != 0) {
        io_list_to_vec_set_vec(&iov, &binv, cbin, cptr, csize, &vlen);
    }

    DESTROY_ESTACK(s);
    return vlen;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

#define IO_LIST_VEC_COUNT(obj)						\
do {									\
    Uint _size = binary_size(obj);					\
    Eterm _real;							\
    ERTS_DECLARE_DUMMY(Uint _offset);					\
    int _bitoffs;							\
    int _bitsize;							\
    ERTS_GET_REAL_BIN(obj, _real, _offset, _bitoffs, _bitsize);		\
    if (_bitsize != 0) goto L_type_error;				\
    if (thing_subtag(*binary_val(_real)) == REFC_BINARY_SUBTAG &&	\
	_bitoffs == 0) {						\
	b_size += _size;                                                \
        if (b_size < _size) goto L_overflow_error;			\
	in_clist = 0;							\
        v_size++;                                                       \
        /* If iov_len is smaller then Uint we split the binary into*/   \
        /* multiple smaller (2GB) elements in the iolist.*/             \
	v_size += _size / MAX_SYSIOVEC_IOVLEN;                          \
        if (_size >= ERL_SMALL_IO_BIN_LIMIT) {				\
            p_in_clist = 0;						\
            p_v_size++;							\
        } else {							\
            p_c_size += _size;						\
            if (!p_in_clist) {						\
                p_in_clist = 1;						\
                p_v_size++;						\
            }								\
        }								\
    } else {								\
	c_size += _size;						\
        if (c_size < _size) goto L_overflow_error;			\
	if (!in_clist) {						\
	    in_clist = 1;						\
	    v_size++;							\
	}								\
	p_c_size += _size;						\
	if (!p_in_clist) {						\
	    p_in_clist = 1;						\
	    p_v_size++;							\
	}								\
    }									\
} while (0)


/* 
 * Returns 0 if successful and a non-zero value otherwise.
 *
 * Return values through pointers:
 *    *vsize      - SysIOVec size needed for a writev
 *    *csize      - Number of bytes not in binary (in the common binary)
 *    *pvsize     - SysIOVec size needed if packing small binaries
 *    *pcsize     - Number of bytes in the common binary if packing
 *    *total_size - Total size of iolist in bytes
 */

static int 
io_list_vec_len(Eterm obj, int* vsize, Uint* csize,
		Uint* pvsize, Uint* pcsize,
		ErlDrvSizeT* total_size)
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    Uint v_size = 0;
    Uint c_size = 0;
    Uint b_size = 0;
    Uint in_clist = 0;
    Uint p_v_size = 0;
    Uint p_c_size = 0;
    Uint p_in_clist = 0;
    Uint total;

    goto L_jump_start;  /* avoid a push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);

	    if (is_byte(obj)) {
		c_size++;
		if (c_size == 0) {
		    goto L_overflow_error;
		}
		if (!in_clist) {
		    in_clist = 1;
		    v_size++;
		}
		p_c_size++;
		if (!p_in_clist) {
		    p_in_clist = 1;
		    p_v_size++;
		}
	    }
	    else if (is_binary(obj)) {
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;   /* on head */
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }

	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list;   /* on tail */
	    else if (is_binary(obj)) {  /* binary tail is OK */
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	}
	else if (is_binary(obj)) {
	    IO_LIST_VEC_COUNT(obj);
	}
	else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    total = c_size + b_size;
    if (total < c_size) {
	goto L_overflow_error;
    }
    *total_size = (ErlDrvSizeT) total;

    DESTROY_ESTACK(s);
    *vsize = v_size;
    *csize = c_size;
    *pvsize = p_v_size;
    *pcsize = p_c_size;
    return 0;

 L_type_error:
 L_overflow_error:
    DESTROY_ESTACK(s);
    return 1;
}

typedef enum {
    ERTS_TRY_IMM_DRV_CALL_OK,
    ERTS_TRY_IMM_DRV_CALL_BUSY_LOCK,
    ERTS_TRY_IMM_DRV_CALL_INVALID_PORT,
    ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS
} ErtsTryImmDrvCallResult;

typedef struct {
    Process *c_p; /* Currently executing process (unlocked) */
    Port *port; /* Port to operate on */
    Eterm port_op; /* port operation as an atom */
    erts_aint32_t state; /* in: invalid state; out: read state (if read) */
    erts_aint32_t sched_flags; /* in: invalid flags; out: read flags (if read) */
    int async; /* Asynchronous operation */
    int pre_chk_sched_flags; /* Check sched flags before lock? */
    int fpe_was_unmasked;
    int reds_left_in;
} ErtsTryImmDrvCallState;

#define ERTS_INIT_TRY_IMM_DRV_CALL_STATE(C_P, PRT, SFLGS, PTS_FLGS, A, PRT_OP) \
    {(C_P), (PRT), (PRT_OP), (SFLGS), (PTS_FLGS), (A), 1, 0}

/*
 * Try doing an immediate driver callback call from a process. If
 * this fail, the operation should be scheduled in the normal case...
 * Returns: ok to do the call, or error (lock busy, does not exist, etc)
 */
static ERTS_INLINE ErtsTryImmDrvCallResult
try_imm_drv_call(ErtsTryImmDrvCallState *sp)
{
    unsigned int prof_runnable_ports;
    ErtsTryImmDrvCallResult res;
    int reds_left_in;
    erts_aint32_t act, exp, invalid_state, invalid_sched_flags;
    Port *prt = sp->port;
    Process *c_p = sp->c_p;

    ASSERT(is_atom(sp->port_op));

    invalid_sched_flags = ERTS_PTS_FLGS_FORCE_SCHEDULE_OP;
    invalid_sched_flags |= sp->sched_flags;
    if (sp->async)
	invalid_sched_flags |= ERTS_PTS_FLG_PARALLELISM;

    if (sp->pre_chk_sched_flags) {
	sp->sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
	if (sp->sched_flags & invalid_sched_flags)
	    return ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS;
    }

    if (erts_smp_port_trylock(prt) == EBUSY)
	return ERTS_TRY_IMM_DRV_CALL_BUSY_LOCK;

    invalid_state = sp->state;
    sp->state = erts_atomic32_read_nob(&prt->state);
    if (sp->state & invalid_state) {
	res = ERTS_TRY_IMM_DRV_CALL_INVALID_PORT;
	goto locked_fail;
    }

    prof_runnable_ports = erts_system_profile_flags.runnable_ports;
    if (prof_runnable_ports)
	erts_port_task_sched_lock(&prt->sched);

    act = erts_smp_atomic32_read_nob(&prt->sched.flags);

    do {
	erts_aint32_t new;
	
	if (act & invalid_sched_flags) {
	    res = ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS;
	    sp->sched_flags = act;
	    goto locked_fail;
	}
	exp = act;
	new = act | ERTS_PTS_FLG_EXEC_IMM;
	act = erts_smp_atomic32_cmpxchg_mb(&prt->sched.flags, new, exp);
    } while (act != exp);
    
    sp->sched_flags = act;

    if (!c_p)
	reds_left_in = CONTEXT_REDS/10;
    else {
	if (IS_TRACED_FL(c_p, F_TRACE_SCHED_PROCS))
	    trace_sched(c_p, ERTS_PROC_LOCK_MAIN, am_out);
	/*
	 * No status lock held while sending runnable
	 * proc trace messages. It is however not needed
	 * in this case, since only this thread can send
	 * such messages for this process until the process
	 * has been scheduled out.
	 */
	if (erts_system_profile_flags.runnable_procs
	    && erts_system_profile_flags.exclusive)
	    profile_runnable_proc(c_p, am_inactive);

	reds_left_in = ERTS_BIF_REDS_LEFT(c_p);
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
    }

    ASSERT(0 <= reds_left_in && reds_left_in <= CONTEXT_REDS);
    sp->reds_left_in = reds_left_in;
    prt->reds = CONTEXT_REDS - reds_left_in;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prof_runnable_ports | IS_TRACED_FL(prt, F_TRACE_SCHED_PORTS)) {
	if (prof_runnable_ports && !(act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	    profile_runnable_port(prt, am_active);
	if (IS_TRACED_FL(prt, F_TRACE_SCHED_PORTS))
	    trace_sched_ports_where(prt, am_in, sp->port_op);
	if (prof_runnable_ports)
	    erts_port_task_sched_unlock(&prt->sched);
    }

    sp->fpe_was_unmasked = erts_block_fpe();

    return ERTS_TRY_IMM_DRV_CALL_OK;

locked_fail:
    erts_port_release(prt);
    return res;
}

static ERTS_INLINE void
finalize_imm_drv_call(ErtsTryImmDrvCallState *sp)
{
    int reds;
    Port *prt = sp->port;
    Process *c_p = sp->c_p;
    erts_aint32_t act;
    unsigned int prof_runnable_ports;

    reds = prt->reds;
    reds += erts_port_driver_callback_epilogue(prt, NULL);

    erts_unblock_fpe(sp->fpe_was_unmasked);

    prof_runnable_ports = erts_system_profile_flags.runnable_ports;
    if (prof_runnable_ports)
	erts_port_task_sched_lock(&prt->sched);

    act = erts_smp_atomic32_read_band_mb(&prt->sched.flags,
					 ~ERTS_PTS_FLG_EXEC_IMM);
    ERTS_SMP_LC_ASSERT(act & ERTS_PTS_FLG_EXEC_IMM);

    if (prof_runnable_ports | IS_TRACED_FL(prt, F_TRACE_SCHED_PORTS)) {
	if (IS_TRACED_FL(prt, F_TRACE_SCHED_PORTS))
	    trace_sched_ports_where(prt, am_out, sp->port_op);
	if (prof_runnable_ports) {
	    if (!(act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
		profile_runnable_port(prt, am_inactive);
	    erts_port_task_sched_unlock(&prt->sched);
	}
    }

    erts_port_release(prt);

    if (c_p) {
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

	if (reds != (CONTEXT_REDS - sp->reds_left_in)) {
	    int bump_reds = reds - (CONTEXT_REDS - sp->reds_left_in);
	    ASSERT(bump_reds > 0);
	    BUMP_REDS(c_p, bump_reds);
	}

	if (IS_TRACED_FL(c_p, F_TRACE_SCHED_PROCS))
	    trace_sched(c_p, ERTS_PROC_LOCK_MAIN, am_in);
	/*
	 * No status lock held while sending runnable
	 * proc trace messages. It is however not needed
	 * in this case, since only this thread can send
	 * such messages for this process until the process
	 * has been scheduled out.
	 */
	if (erts_system_profile_flags.runnable_procs
	    && erts_system_profile_flags.exclusive)
	    profile_runnable_proc(c_p, am_active);
    }
}

/*
 * force_imm_drv_call()/finalize_force_imm_drv_call() should *only*
 * be used while crash dumping...
 */
static ErtsTryImmDrvCallResult
force_imm_drv_call(ErtsTryImmDrvCallState *sp)
{
    erts_aint32_t invalid_state;
    Port *prt = sp->port;

    ASSERT(ERTS_IS_CRASH_DUMPING);
    ASSERT(is_atom(sp->port_op));

    invalid_state = sp->state;
    sp->state = erts_atomic32_read_nob(&prt->state);
    if (sp->state & invalid_state)
	return ERTS_TRY_IMM_DRV_CALL_INVALID_PORT;

    sp->fpe_was_unmasked = erts_block_fpe();

    return ERTS_TRY_IMM_DRV_CALL_OK;
}

static void
finalize_force_imm_drv_call(ErtsTryImmDrvCallState *sp)
{
    erts_unblock_fpe(sp->fpe_was_unmasked);
}

#define ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE (REF_THING_SIZE + 3)

static ERTS_INLINE void
queue_port_sched_op_reply(Process *rp,
			  ErtsProcLocks rp_locks,
                          ErtsHeapFactory* factory,
			  Uint32 *ref_num,
			  Eterm msg,
			  Port* prt)
{
    Eterm* hp = erts_produce_heap(factory, ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE, 0);
    Eterm ref;

    ref= make_internal_ref(hp);
    write_ref_thing(hp, ref_num[0], ref_num[1], ref_num[2]);
    hp += REF_THING_SIZE;

    msg = TUPLE2(hp, ref, msg);

    erts_factory_trim_and_close(factory, &msg, 1);

    erts_queue_message(rp, rp_locks, factory->message, msg,
		       prt ? prt->common.id : am_undefined);
}

static void
port_sched_op_reply(Eterm to, Uint32 *ref_num, Eterm msg, Port* prt)
{
    Process *rp = erts_proc_lookup_raw(to);
    if (rp) {
        ErtsHeapFactory factory;
	Eterm msg_copy;
	Uint hsz, msg_sz;
	ErtsProcLocks rp_locks = 0;

	hsz = ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE;
	if (is_immed(msg))
	    msg_sz = 0;
	else {
	    msg_sz = size_object(msg);
	    hsz += msg_sz;
	}

	(void) erts_factory_message_create(&factory, rp,
					   &rp_locks, hsz);
	msg_copy = (is_immed(msg)
		    ? msg
		    : copy_struct(msg, msg_sz,
				  &factory.hp,
				  factory.off_heap));

	queue_port_sched_op_reply(rp,
				  rp_locks,
                                  &factory,
				  ref_num,
				  msg_copy,
				  prt);

	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
    }
}


ErtsPortOpResult
erts_schedule_proc2port_signal(Process *c_p,
			       Port *prt,
			       Eterm caller,
			       Eterm *refp,
			       ErtsProc2PortSigData *sigdp,
			       int task_flags,
			       ErtsPortTaskHandle *pthp,
			       ErtsProc2PortSigCallback callback)
{
    int sched_res;
    if (!refp) {
	if (c_p)
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
    }
    else {
	ASSERT(c_p);
	sigdp->flags |= ERTS_P2P_SIG_DATA_FLG_REPLY;
	erts_make_ref_in_array(sigdp->ref);
	*refp = erts_proc_store_ref(c_p, sigdp->ref);

	/*
	 * Caller needs to wait for a message containing
	 * the ref that we just created. No such message
	 * can exist in callers message queue at this time.
	 * We therefore move the save pointer of the
	 * callers message queue to the end of the queue.
	 *
	 * NOTE: It is of vital importance that the caller
	 *       immediately do a receive unconditionaly
	 *       waiting for the message with the reference;
	 *       otherwise, next receive will *not* work
	 *       as expected!
	 */
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);

	if (ERTS_PROC_PENDING_EXIT(c_p)) {
	    /* need to exit caller instead */
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	    KILL_CATCHES(c_p);
	    c_p->freason = EXC_EXIT;
	    return ERTS_PORT_OP_CALLER_EXIT;
	}

	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	c_p->msg.save = c_p->msg.last;

        erts_smp_proc_unlock(c_p, (ERTS_PROC_LOCKS_MSG_RECEIVE
                                   | ERTS_PROC_LOCK_MAIN));
    }


    sigdp->caller = caller;

    /* Schedule port close call for later execution... */
    sched_res = erts_port_task_schedule(prt->common.id,
					pthp,
					ERTS_PORT_TASK_PROC_SIG,
					sigdp,
					callback,
					task_flags);

    if (c_p)
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

    if (sched_res != 0) {
	if (refp) {
	    /*
	     * We need to restore the message queue save
	     * pointer to the beginning of the message queue
	     * since the caller now wont wait for a message
	     * containing the reference created above...
	     */
	    ASSERT(c_p);
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	    JOIN_MESSAGE(c_p);
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	    *refp = NIL;
	}
	return ERTS_PORT_OP_DROPPED;
    }
    return ERTS_PORT_OP_SCHEDULED;
}

static int
erts_schedule_port2port_signal(Eterm port_num, ErtsProc2PortSigData *sigdp,
                               int task_flags,
                               ErtsProc2PortSigCallback callback)
{
    Port *prt = erts_port_lookup_raw(port_num);

    if (!prt)
        return -1;

    sigdp->caller = ERTS_INVALID_PID;

    return erts_port_task_schedule(prt->common.id,
                                   NULL,
                                   ERTS_PORT_TASK_PROC_SIG,
                                   sigdp,
                                   callback,
                                   task_flags);
}

static ERTS_INLINE void
send_badsig(Port *prt) {
    ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
    Process* rp;
    Eterm connected = ERTS_PORT_GET_CONNECTED(prt);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_LC_ASSERT(erts_get_scheduler_id());

    ASSERT(is_internal_pid(connected));

    rp = erts_proc_lookup_raw(connected);
    if (rp) {
	erts_smp_proc_lock(rp, rp_locks);
	if (!ERTS_PROC_IS_EXITING(rp))
	    (void) erts_send_exit_signal(NULL,
					 prt->common.id,
					 rp,
					 &rp_locks, 
					 am_badsig,
					 NIL,
					 NULL,
					 0);
	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
    } /* exit sent */
} /* send_badsig */

static void
badsig_received(int bang_op, Port *prt,
		erts_aint32_t state,
		int bad_output_value) {
    /*
     * if (bang_op)
     *   we are part of a "Prt ! Something" operation
     * else
     *   we are part of a call to a port BIF
     * behave accordingly...
     */
    if (!(state & ERTS_PORT_SFLGS_INVALID_LOOKUP)) {
	if (bad_output_value) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Bad value on output port '%s'\n", prt->name);
	    erts_send_error_to_logger_nogl(dsbufp);
	}
	if (bang_op)
	    send_badsig(prt);
    } /* not invalid */
} /* behaved accordingly */

static int
port_badsig(Port *prt, erts_aint32_t state, int op,
            ErtsProc2PortSigData *sigdp) {
    if (op == ERTS_PROC2PORT_SIG_EXEC)
	badsig_received(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BANG_OP,
			prt,
			state,
			sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BAD_OUTPUT);
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, am_badarg, prt);
    return ERTS_PORT_REDS_BADSIG;
} /* port_badsig */
/* bad_port_signal() will
 * - preserve signal order of signals.
 * - send a 'badsig' exit signal to connected process if 'from' is an
 *   internal pid and the port is alive when the bad signal reaches
 *   it.
 */

static ErtsPortOpResult
bad_port_signal(Process *c_p,
		int flags,
		Port *prt,
		Eterm from,
		Eterm *refp,
		Eterm port_op)
{
    ErtsProc2PortSigData *sigdp;
    ErtsTryImmDrvCallResult try_call_res;
    ErtsTryImmDrvCallState try_call_state
	= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
	    c_p,
	    prt,
	    ERTS_PORT_SFLGS_INVALID_LOOKUP,
	    0,
	    !refp,
	    port_op);

    try_call_res = try_imm_drv_call(&try_call_state);
    switch (try_call_res) {
    case ERTS_TRY_IMM_DRV_CALL_OK:
	badsig_received(flags & ERTS_PORT_SIG_FLG_BANG_OP,
			prt,
			try_call_state.state,
			flags & ERTS_PORT_SIG_FLG_BAD_OUTPUT);
	finalize_imm_drv_call(&try_call_state);
	if (c_p)
	    BUMP_REDS(c_p, ERTS_PORT_REDS_BADSIG);
	return ERTS_PORT_OP_BADARG;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	return ERTS_PORT_OP_DROPPED;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS:
    case ERTS_TRY_IMM_DRV_CALL_BUSY_LOCK:
	/* Schedule badsig() call instead... */
	break;
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = (flags & ~ERTS_P2P_SIG_TYPE_MASK) | ERTS_P2P_SIG_TYPE_BAD;

    return erts_schedule_proc2port_signal(c_p,
					  prt,
					  c_p->common.id,
					  refp,
					  sigdp,
					  0,
					  NULL,
					  port_badsig);
}


/*
 * Driver outputv() callback
 */

static ERTS_INLINE void
call_driver_outputv(int bang_op,
		    Eterm caller,
		    Eterm from,
		    Port *prt,
		    erts_driver_t *drv,
		    ErlIOVec *evp)
{
    /*
     * if (bang_op)
     *   we are part of a "Prt ! {From, {command, Data}}" operation
     * else
     *   we are part of a call to port_command/[2,3]
     * behave accordingly...
     */
    if (bang_op && from != ERTS_PORT_GET_CONNECTED(prt))
	send_badsig(prt);
    else {
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
	ErlDrvSizeT size = evp->size;
	ERTS_MSACC_PUSH_AND_SET_STATE_M(ERTS_MSACC_STATE_PORT);

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt)	
			   || ERTS_IS_CRASH_DUMPING);


        if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
            trace_port_receive(prt, caller, am_commandv, evp);

#ifdef USE_VM_PROBES
	if (DTRACE_ENABLED(driver_outputv)) {
	    DTRACE_FORMAT_COMMON_PID_AND_PORT(caller, prt);
	    DTRACE4(driver_outputv, process_str, port_str, prt->name, size);
	}
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
        if (LTTNG_ENABLED(driver_outputv)) {
            lttng_decl_portbuf(port_str);
            lttng_decl_procbuf(proc_str);
            lttng_pid_to_str(caller, proc_str);
            lttng_port_to_str(prt, port_str);
            LTTNG4(driver_outputv, proc_str, port_str, prt->name, size);
        }
#endif

	prt->caller = caller;
	(*drv->outputv)((ErlDrvData) prt->drv_data, evp);
	prt->caller = NIL;

	prt->bytes_out += size;
	if (esdp)
	    esdp->io.out += (Uint64) size;
	else
	    erts_atomic64_add_nob(&bytes_out, (erts_aint64_t) size);

	ERTS_MSACC_POP_STATE_M();
    }
}

static ERTS_INLINE void
cleanup_scheduled_outputv(ErlIOVec *ev, ErlDrvBinary *cbinp)
{
    int i;
    /* Need to free all binaries */
    for (i = 1; i < ev->vsize; i++)
	if (ev->binv[i])
	    driver_free_binary(ev->binv[i]);
    if (cbinp)
	driver_free_binary(cbinp);
}

static int
port_sig_outputv(Port *prt, erts_aint32_t state, int op, ErtsProc2PortSigData *sigdp)
{
    Eterm reply;

    switch (op) {
    case ERTS_PROC2PORT_SIG_EXEC:
	/* Execution of a scheduled outputv() call */

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

	if (state & ERTS_PORT_SFLGS_INVALID_LOOKUP)
	    reply = am_badarg;
	else {
	    call_driver_outputv(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BANG_OP,
				sigdp->caller,
				sigdp->u.outputv.from,
				prt,
				prt->drv_ptr,
				sigdp->u.outputv.evp);
	    reply = am_true;
	}
	break;
    case ERTS_PROC2PORT_SIG_ABORT_NOSUSPEND:
	reply = am_false;
	break;
    default:
	reply = am_badarg;
	break;
    }

    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, reply, prt);

    cleanup_scheduled_outputv(sigdp->u.outputv.evp,
			      sigdp->u.outputv.cbinp);

    return ERTS_PORT_REDS_CMD_OUTPUTV;
}

/*
 * Driver output() callback
 */

static ERTS_INLINE void
call_driver_output(int bang_op,
		   Eterm caller,
		   Eterm from,
		   Port *prt,
		   erts_driver_t *drv,
		   char *bufp,
		   ErlDrvSizeT size)
{
    /*
     * if (bang_op)
     *   we are part of a "Prt ! {From, {command, Data}}" operation
     * else
     *   we are part of a call to port_command/[2,3]
     * behave accordingly...
     */
    if (bang_op && from != ERTS_PORT_GET_CONNECTED(prt))
	send_badsig(prt);
    else {
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
        ERTS_MSACC_PUSH_AND_SET_STATE_M(ERTS_MSACC_STATE_PORT);
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt)	
			   || ERTS_IS_CRASH_DUMPING);

#ifdef USE_VM_PROBES
	if (DTRACE_ENABLED(driver_output)) {
	    DTRACE_FORMAT_COMMON_PID_AND_PORT(caller, prt);
	    DTRACE4(driver_output, process_str, port_str, prt->name, size);
	}
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
        if (LTTNG_ENABLED(driver_output)) {
            lttng_decl_portbuf(port_str);
            lttng_decl_procbuf(proc_str);
            lttng_pid_to_str(caller, proc_str);
            lttng_port_to_str(prt, port_str);
            LTTNG4(driver_output, proc_str, port_str, prt->name, size);
        }
#endif

        if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
            trace_port_receive(prt, caller, am_command, bufp, size);

	prt->caller = caller;
	(*drv->output)((ErlDrvData) prt->drv_data, bufp, size);
	prt->caller = NIL;

	prt->bytes_out += size;
	if (esdp)
	    esdp->io.out += (Uint64) size;
	else
	    erts_atomic64_add_nob(&bytes_out, (erts_aint64_t) size);

	ERTS_MSACC_POP_STATE_M();
    }
}

static ERTS_INLINE void
cleanup_scheduled_output(char *bufp)
{
    erts_free(ERTS_ALC_T_DRV_CMD_DATA, bufp);
}

static int
port_sig_output(Port *prt, erts_aint32_t state, int op, ErtsProc2PortSigData *sigdp)
{
    Eterm reply;

    switch (op) {
    case ERTS_PROC2PORT_SIG_EXEC:
	/* Execution of a scheduled output() call */

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

	if (state & ERTS_PORT_SFLGS_INVALID_LOOKUP)
	    reply = am_badarg;
	else {
	    call_driver_output(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BANG_OP,
			       sigdp->caller,
			       sigdp->u.output.from,
			       prt,
			       prt->drv_ptr,
			       sigdp->u.output.bufp,
			       sigdp->u.output.size);
	    reply = am_true;
	}
	break;
    case ERTS_PROC2PORT_SIG_ABORT_NOSUSPEND:
	reply = am_false;
	break;
    default:
	reply = am_badarg;
	break;
    }

    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, reply, prt);

    cleanup_scheduled_output(sigdp->u.output.bufp);

    return ERTS_PORT_REDS_CMD_OUTPUT;
}


/*
 * This erts_port_output will always create a port task.
 * The call is treated as a port_command call, i.e. no
 * badsig i generated if the input in invalid. However
 * an error_logger message is generated.
 */
int
erts_port_output_async(Port *prt, Eterm from, Eterm list)
{

    ErtsPortOpResult res;
    ErtsProc2PortSigData *sigdp;
    erts_driver_t *drv = prt->drv_ptr;
    size_t size;
    int task_flags;
    ErtsProc2PortSigCallback port_sig_callback;
    ErlDrvBinary *cbin = NULL;
    ErlIOVec *evp = NULL;
    char *buf = NULL;
    ErtsPortTaskHandle *ns_pthp;

    if (drv->outputv) {
        ErlIOVec ev;
	SysIOVec* ivp;
	ErlDrvBinary**  bvp;
        int vsize;
	Uint csize;
	Uint pvsize;
	Uint pcsize;
        size_t iov_offset, binv_offset, alloc_size;
        Uint blimit = 0;
        char *ptr;
        int i;

        Eterm* bptr = NULL;
        Uint offset;

        if (is_binary(list)) {
            /* We optimize for when we get a procbin without offset */
            Eterm real_bin;
            int bitoffs;
            int bitsize;
            ERTS_GET_REAL_BIN(list, real_bin, offset, bitoffs, bitsize);
            bptr = binary_val(real_bin);
            if (*bptr == HEADER_PROC_BIN && bitoffs == 0) {
                size = binary_size(list);
                vsize = 1;
            } else
                bptr = NULL;
        }

        if (!bptr) {
            if (io_list_vec_len(list, &vsize, &csize, &pvsize, &pcsize, &size))
                goto bad_value;

            /* To pack or not to pack (small binaries) ...? */
            if (vsize >= SMALL_WRITE_VEC) {
                /* Do pack */
                vsize = pvsize + 1;
                csize = pcsize;
                blimit = ERL_SMALL_IO_BIN_LIMIT;
            }
            cbin = driver_alloc_binary(csize);
            if (!cbin)
                erts_alloc_enomem(ERTS_ALC_T_DRV_BINARY, ERTS_SIZEOF_Binary(csize));
        }


        iov_offset = ERTS_ALC_DATA_ALIGN_SIZE(sizeof(ErlIOVec));
	binv_offset = iov_offset;
        binv_offset += ERTS_ALC_DATA_ALIGN_SIZE((vsize+1)*sizeof(SysIOVec));
        alloc_size = binv_offset;
	alloc_size += (vsize+1)*sizeof(ErlDrvBinary *);

        sigdp = erts_port_task_alloc_p2p_sig_data_extra(alloc_size, (void**)&ptr);

        evp = (ErlIOVec *) ptr;
        ivp = evp->iov = (SysIOVec *) (ptr + iov_offset);
        bvp = evp->binv = (ErlDrvBinary **) (ptr + binv_offset);

        ivp[0].iov_base = NULL;
	ivp[0].iov_len = 0;
	bvp[0] = NULL;

        if (bptr) {
            ProcBin* pb = (ProcBin *) bptr;

            ivp[1].iov_base = pb->bytes+offset;
            ivp[1].iov_len = size;
            bvp[1] = Binary2ErlDrvBinary(pb->val);

            evp->vsize = 1;
        } else {

            evp->vsize = io_list_to_vec(list, ivp+1, bvp+1, cbin, blimit);
            if (evp->vsize < 0) {
                if (evp != &ev)
                    erts_free(ERTS_ALC_T_DRV_CMD_DATA, evp);
                driver_free_binary(cbin);
                goto bad_value;
            }
        }
#if 0
	/* This assertion may say something useful, but it can
        be falsified during the emulator test suites. */
	ASSERT(evp->vsize == vsize);
#endif
	evp->vsize++;
	evp->size = size;  /* total size */

        /* Need to increase refc on all binaries */
        for (i = 1; i < evp->vsize; i++)
            if (bvp[i])
                driver_binary_inc_refc(bvp[i]);

	sigdp->flags = ERTS_P2P_SIG_TYPE_OUTPUTV;
	sigdp->u.outputv.from = from;
	sigdp->u.outputv.evp = evp;
	sigdp->u.outputv.cbinp = cbin;
	port_sig_callback = port_sig_outputv;
    } else {
        ErlDrvSizeT ERTS_DECLARE_DUMMY(r);

	/*
	 * Apperently there exist code that write 1 byte to
	 * much in buffer. Where it resides I don't know, but
	 * we can live with one byte extra allocated...
	 */

        if (erts_iolist_size(list, &size))
            goto bad_value;

        buf = erts_alloc(ERTS_ALC_T_DRV_CMD_DATA, size + 1);

        r = erts_iolist_to_buf(list, buf, size);
        ASSERT(ERTS_IOLIST_TO_BUF_SUCCEEDED(r));

	sigdp = erts_port_task_alloc_p2p_sig_data();
	sigdp->flags = ERTS_P2P_SIG_TYPE_OUTPUT;
	sigdp->u.output.from = from;
	sigdp->u.output.bufp = buf;
	sigdp->u.output.size = size;
	port_sig_callback = port_sig_output;
    }
    sigdp->flags = 0;
    ns_pthp = NULL;
    task_flags = 0;

    res = erts_schedule_proc2port_signal(NULL,
					 prt,
					 ERTS_INVALID_PID,
					 NULL,
					 sigdp,
					 task_flags,
					 ns_pthp,
					 port_sig_callback);

    if (res != ERTS_PORT_OP_SCHEDULED) {
	if (drv->outputv)
	    cleanup_scheduled_outputv(evp, cbin);
	else
	    cleanup_scheduled_output(buf);
	return 1;
    }
    return 1;

bad_value:

    /*
     * We call badsig directly here as this function is called with
     * the main lock of the calling process still held.
     * At the moment this operation is always not a bang_op, so
     * only an error_logger message should be generated, no badsig.
    */

    badsig_received(0, prt, erts_atomic32_read_nob(&prt->state), 1);

    return 0;

}

ErtsPortOpResult
erts_port_output(Process *c_p,
		 int flags,
		 Port *prt,
		 Eterm from,
		 Eterm list,
		 Eterm *refp)
{
    ErtsPortOpResult res;
    ErtsProc2PortSigData *sigdp = NULL;
    erts_driver_t *drv = prt->drv_ptr;
    size_t size;
    int try_call;
    erts_aint32_t sched_flags, busy_flgs, invalid_flags;
    int task_flags;
    ErtsProc2PortSigCallback port_sig_callback;
    ErlDrvBinary *cbin = NULL;
    ErlIOVec *evp = NULL;
    char *buf = NULL;
    int force_immediate_call = (flags & ERTS_PORT_SIG_FLG_FORCE_IMM_CALL);
    int async_nosuspend;
    ErtsPortTaskHandle *ns_pthp;

    ASSERT((flags & ~(ERTS_PORT_SIG_FLG_BANG_OP
		      | ERTS_PORT_SIG_FLG_ASYNC
		      | ERTS_PORT_SIG_FLG_NOSUSPEND
		      | ERTS_PORT_SIG_FLG_FORCE
		      | ERTS_PORT_SIG_FLG_FORCE_IMM_CALL)) == 0);

    busy_flgs = ((flags & ERTS_PORT_SIG_FLG_FORCE)
		 ? ((erts_aint32_t) 0)
		 : ERTS_PTS_FLGS_BUSY);
    invalid_flags = busy_flgs;
    if (!refp)
	invalid_flags |= ERTS_PTS_FLG_PARALLELISM;

    /*
     * Assumes caller have checked that port is valid...
     */

    sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
    if (sched_flags & (busy_flgs|ERTS_PTS_FLG_EXIT))
	return ((sched_flags & ERTS_PTS_FLG_EXIT)
		? ERTS_PORT_OP_DROPPED
		: ERTS_PORT_OP_BUSY);

    async_nosuspend = ((flags & (ERTS_PORT_SIG_FLG_ASYNC
				 | ERTS_PORT_SIG_FLG_NOSUSPEND
				 | ERTS_PORT_SIG_FLG_FORCE))
		      == (ERTS_PORT_SIG_FLG_ASYNC
			  | ERTS_PORT_SIG_FLG_NOSUSPEND));

    try_call = (force_immediate_call /* crash dumping */
		|| !(sched_flags & (invalid_flags
				    | ERTS_PTS_FLGS_FORCE_SCHEDULE_OP)));

#ifdef USE_VM_PROBES
    if(DTRACE_ENABLED(port_command)) {
	DTRACE_FORMAT_COMMON_PID_AND_PORT(c_p ? c_p->common.id : ERTS_INVALID_PID, prt);
	DTRACE4(port_command, process_str, port_str, prt->name, "command");
    }
#endif
    if (drv->outputv) {
	ErlIOVec ev;
	SysIOVec iv[SMALL_WRITE_VEC];
	ErlDrvBinary* bv[SMALL_WRITE_VEC];
	SysIOVec* ivp;
	ErlDrvBinary**  bvp;
	int vsize;
	Uint csize;
	Uint pvsize;
	Uint pcsize;
	Uint blimit;
	size_t iov_offset, binv_offset, alloc_size;

	if (io_list_vec_len(list, &vsize, &csize, &pvsize, &pcsize, &size))
	    goto bad_value;

	iov_offset = ERTS_ALC_DATA_ALIGN_SIZE(sizeof(ErlIOVec));
	binv_offset = iov_offset;
	binv_offset += ERTS_ALC_DATA_ALIGN_SIZE((vsize+1)*sizeof(SysIOVec));
	alloc_size = binv_offset;
	alloc_size += (vsize+1)*sizeof(ErlDrvBinary *);

	if (try_call && vsize < SMALL_WRITE_VEC) {
	    ivp = ev.iov = iv;
	    bvp = ev.binv = bv;
	    evp = &ev;
	}
	else {
            char *ptr;
            if (try_call) {
                ptr = erts_alloc(ERTS_ALC_T_TMP, alloc_size);
            } else {
                sigdp = erts_port_task_alloc_p2p_sig_data_extra(
                    alloc_size, (void**)&ptr);
            }
	    evp = (ErlIOVec *) ptr;
	    ivp = evp->iov = (SysIOVec *) (ptr + iov_offset);
	    bvp = evp->binv = (ErlDrvBinary **) (ptr + binv_offset);
	}

	/* To pack or not to pack (small binaries) ...? */
	if (vsize < SMALL_WRITE_VEC) {
	    /* Do NOT pack */
	    blimit = 0;
	}
	else {
	    /* Do pack */
	    vsize = pvsize + 1;
	    csize = pcsize;
	    blimit = ERL_SMALL_IO_BIN_LIMIT;
	}
	/* Use vsize and csize from now on */

	cbin = driver_alloc_binary(csize);
	if (!cbin)
	    erts_alloc_enomem(ERTS_ALC_T_DRV_BINARY, ERTS_SIZEOF_Binary(csize));

	/* Element 0 is for driver usage to add header block */
	ivp[0].iov_base = NULL;
	ivp[0].iov_len = 0;
	bvp[0] = NULL;
	evp->vsize = io_list_to_vec(list, ivp+1, bvp+1, cbin, blimit);
	if (evp->vsize < 0) {
            if (evp != &ev) {
                if (try_call)
                    erts_free(ERTS_ALC_T_TMP, evp);
                else
                    erts_port_task_free_p2p_sig_data(sigdp);
            }
	    driver_free_binary(cbin);
	    goto bad_value;
	}
#if 0
	/* This assertion may say something useful, but it can
	   be falsified during the emulator test suites. */
	ASSERT(evp->vsize == vsize);
#endif
	evp->vsize++;
	evp->size = size;  /* total size */

	if (!try_call) {
	    int i;
	    /* Need to increase refc on all binaries */
	    for (i = 1; i < evp->vsize; i++)
		if (bvp[i])
		    driver_binary_inc_refc(bvp[i]);
	}
	else {
	    int i;
	    ErlIOVec *new_evp;
	    ErtsTryImmDrvCallResult try_call_res;
	    ErtsTryImmDrvCallState try_call_state
		= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
		    c_p,
		    prt,
		    ERTS_PORT_SFLGS_INVALID_LOOKUP,
		    invalid_flags,
		    !refp,
		    am_command);

	    try_call_state.pre_chk_sched_flags = 0; /* already checked */
	    if (force_immediate_call)
		try_call_res = force_imm_drv_call(&try_call_state);
	    else
		try_call_res = try_imm_drv_call(&try_call_state);
	    switch (try_call_res) {
	    case ERTS_TRY_IMM_DRV_CALL_OK:
		call_driver_outputv(flags & ERTS_PORT_SIG_FLG_BANG_OP,
				    c_p ? c_p->common.id : ERTS_INVALID_PID,
				    from,
				    prt,
				    drv,
				    evp);
		if (force_immediate_call)
		    finalize_force_imm_drv_call(&try_call_state);
		else
		    finalize_imm_drv_call(&try_call_state);
		/* Fall through... */
	    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
		driver_free_binary(cbin);
		if (evp != &ev) {
                    ASSERT(!sigdp);
		    erts_free(ERTS_ALC_T_TMP, evp);
                }
		if (try_call_res != ERTS_TRY_IMM_DRV_CALL_OK)
		    return ERTS_PORT_OP_DROPPED;
		if (c_p)
		    BUMP_REDS(c_p, ERTS_PORT_REDS_CMD_OUTPUTV);
		return ERTS_PORT_OP_DONE;
	    case ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS:
		sched_flags = try_call_state.sched_flags;
		if (async_nosuspend
		    && (sched_flags & (busy_flgs|ERTS_PTS_FLG_EXIT))) {
		    driver_free_binary(cbin);
                    if (evp != &ev) {
                        ASSERT(!sigdp);
			erts_free(ERTS_ALC_T_TMP, evp);
                    }
		    return ((sched_flags & ERTS_PTS_FLG_EXIT)
			    ? ERTS_PORT_OP_DROPPED
			    : ERTS_PORT_OP_BUSY);
		}
	    case ERTS_TRY_IMM_DRV_CALL_BUSY_LOCK:
		/* Schedule outputv() call instead... */
		break;
	    }

	    /* Need to increase refc on all binaries */
	    for (i = 1; i < evp->vsize; i++)
		if (bvp[i])
		    driver_binary_inc_refc(bvp[i]);

            /* The port task and iovec is allocated in the
               same structure as an optimization. This
               is especially important in erts_port_output_async
               of when !try_call */
            ASSERT(sigdp == NULL);
            sigdp = erts_port_task_alloc_p2p_sig_data_extra(
                alloc_size, (void**)&new_evp);

	    if (evp != &ev) {
                /* Copy from TMP alloc to port task */
		sys_memcpy((void *) new_evp, (void *) evp, alloc_size);
		new_evp->iov = (SysIOVec *) (((char *) new_evp)
					     + iov_offset);
		bvp = new_evp->binv = (ErlDrvBinary **) (((char *) new_evp)
							 + binv_offset);

#ifdef DEBUG
		ASSERT(new_evp->vsize == evp->vsize);
		ASSERT(new_evp->size == evp->size);
		for (i = 0; i < evp->vsize; i++) {
		    ASSERT(new_evp->iov[i].iov_len == evp->iov[i].iov_len);
		    ASSERT(new_evp->iov[i].iov_base == evp->iov[i].iov_base);
		    ASSERT(new_evp->binv[i] == evp->binv[i]);
		}
#endif

		erts_free(ERTS_ALC_T_TMP, evp);
	    }
	    else { /* from stack allocated structure; offsets may differ */

		sys_memcpy((void *) new_evp, (void *) evp, sizeof(ErlIOVec));
		new_evp->iov = (SysIOVec *) (((char *) new_evp)
					     + iov_offset);
		sys_memcpy((void *) new_evp->iov,
			   (void *) evp->iov,
			   evp->vsize * sizeof(SysIOVec));
		new_evp->binv = (ErlDrvBinary **) (((char *) new_evp)
						   + binv_offset);
		sys_memcpy((void *) new_evp->binv,
			   (void *) evp->binv,
			   evp->vsize * sizeof(ErlDrvBinary *));

#ifdef DEBUG
		ASSERT(new_evp->vsize == evp->vsize);
		ASSERT(new_evp->size == evp->size);
		for (i = 0; i < evp->vsize; i++) {
		    ASSERT(new_evp->iov[i].iov_len == evp->iov[i].iov_len);
		    ASSERT(new_evp->iov[i].iov_base == evp->iov[i].iov_base);
		    ASSERT(new_evp->binv[i] == evp->binv[i]);
		}
#endif

	    }

	    evp = new_evp;
	}

	sigdp->flags = ERTS_P2P_SIG_TYPE_OUTPUTV;
	sigdp->u.outputv.from = from;
	sigdp->u.outputv.evp = evp;
	sigdp->u.outputv.cbinp = cbin;
	port_sig_callback = port_sig_outputv;
    }
    else {
	ErlDrvSizeT r;

	/*
	 * Apperently there exist code that write 1 byte to
	 * much in buffer. Where it resides I don't know, but
	 * we can live with one byte extra allocated...
	 */

	if (!try_call) {
	    if (erts_iolist_size(list, &size))
		goto bad_value;

	    buf = erts_alloc(ERTS_ALC_T_DRV_CMD_DATA, size + 1);

	    r = erts_iolist_to_buf(list, buf, size);
	    ASSERT(ERTS_IOLIST_TO_BUF_SUCCEEDED(r));
	}
	else {
	    char *new_buf;
	    ErtsTryImmDrvCallResult try_call_res;
	    ErtsTryImmDrvCallState try_call_state
		= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
		    c_p,
		    prt,
		    ERTS_PORT_SFLGS_INVALID_LOOKUP,
		    invalid_flags,
		    !refp,
		    am_command);

	    /* Try with an 8KB buffer first (will often be enough I guess). */
	    size = 8*1024;

	    buf = erts_alloc(ERTS_ALC_T_TMP, size + 1);
	    r = erts_iolist_to_buf(list, buf, size);

	    if (ERTS_IOLIST_TO_BUF_SUCCEEDED(r)) {
		ASSERT(r <= size);
		size -= r;
	    }
	    else {
		erts_free(ERTS_ALC_T_TMP, buf);
		if (r == ERTS_IOLIST_TO_BUF_TYPE_ERROR)
		    goto bad_value;
		ASSERT(r == ERTS_IOLIST_TO_BUF_OVERFLOW);
		if (erts_iolist_size(list, &size))
		    goto bad_value;
		buf = erts_alloc(ERTS_ALC_T_TMP, size + 1); 
		r = erts_iolist_to_buf(list, buf, size);
		ASSERT(ERTS_IOLIST_TO_BUF_SUCCEEDED(r));
	    }

	    try_call_state.pre_chk_sched_flags = 0; /* already checked */
	    if (force_immediate_call)
		try_call_res = force_imm_drv_call(&try_call_state);
	    else
		try_call_res = try_imm_drv_call(&try_call_state);
	    switch (try_call_res) {
	    case ERTS_TRY_IMM_DRV_CALL_OK:
		call_driver_output(flags & ERTS_PORT_SIG_FLG_BANG_OP,
				   c_p ? c_p->common.id : ERTS_INVALID_PID,
				   from,
				   prt,
				   drv,
				   buf,
				   size);
		if (force_immediate_call)
		    finalize_force_imm_drv_call(&try_call_state);
		else
		    finalize_imm_drv_call(&try_call_state);
		/* Fall through... */
	    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
		erts_free(ERTS_ALC_T_TMP, buf);
		if (try_call_res != ERTS_TRY_IMM_DRV_CALL_OK)
		    return ERTS_PORT_OP_DROPPED;
		if (c_p)
		    BUMP_REDS(c_p, ERTS_PORT_REDS_CMD_OUTPUT);
		return ERTS_PORT_OP_DONE;
	    case ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS:
		sched_flags = try_call_state.sched_flags;
		if (async_nosuspend
		    && (sched_flags & (busy_flgs|ERTS_PTS_FLG_EXIT))) {
		    erts_free(ERTS_ALC_T_TMP, buf);
		    return ((sched_flags & ERTS_PTS_FLG_EXIT)
			    ? ERTS_PORT_OP_DROPPED
			    : ERTS_PORT_OP_BUSY);
		}
	    case ERTS_TRY_IMM_DRV_CALL_BUSY_LOCK:
		/* Schedule outputv() call instead... */
		break;
	    }

	    new_buf = erts_alloc(ERTS_ALC_T_DRV_CMD_DATA, size + 1);
	    sys_memcpy(new_buf, buf, size);
	    erts_free(ERTS_ALC_T_TMP, buf);
	    buf = new_buf;
	}

	sigdp = erts_port_task_alloc_p2p_sig_data();
	sigdp->flags = ERTS_P2P_SIG_TYPE_OUTPUT;
	sigdp->u.output.from = from;
	sigdp->u.output.bufp = buf;
	sigdp->u.output.size = size;
	port_sig_callback = port_sig_output;
    }

    task_flags = ERTS_PT_FLG_WAIT_BUSY;
    sigdp->flags |= flags;
    ns_pthp = NULL;
    if (flags & (ERTS_P2P_SIG_DATA_FLG_FORCE|ERTS_P2P_SIG_DATA_FLG_NOSUSPEND)) {
	task_flags = 0;
	if (flags & ERTS_P2P_SIG_DATA_FLG_FORCE)
	    sigdp->flags &= ~ERTS_P2P_SIG_DATA_FLG_NOSUSPEND;
	else if (async_nosuspend) {
	    ErtsSchedulerData *esdp = (c_p
				       ? erts_proc_sched_data(c_p)
				       : erts_get_scheduler_data());
	    ASSERT(esdp);
	    ns_pthp = &esdp->nosuspend_port_task_handle;
	    sigdp->flags &= ~ERTS_P2P_SIG_DATA_FLG_NOSUSPEND;
	}
	else if (flags & ERTS_P2P_SIG_DATA_FLG_NOSUSPEND)
	    task_flags = ERTS_PT_FLG_NOSUSPEND;
    }

    ASSERT(ns_pthp || !async_nosuspend);
    ASSERT(async_nosuspend || !ns_pthp);

    res = erts_schedule_proc2port_signal(c_p,
					 prt,
					 c_p ? c_p->common.id : ERTS_INVALID_PID,
					 refp,
					 sigdp,
					 task_flags,
					 ns_pthp,
					 port_sig_callback);

    if (res != ERTS_PORT_OP_SCHEDULED) {
	if (drv->outputv)
	    cleanup_scheduled_outputv(evp, cbin);
	else
	    cleanup_scheduled_output(buf);
	return res;
    }

    if (!(flags & ERTS_PORT_SIG_FLG_FORCE)) {
	sched_flags = erts_smp_atomic32_read_acqb(&prt->sched.flags);
	if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT)) {
	    if (async_nosuspend)
		erts_port_task_tmp_handle_detach(ns_pthp);
	}
	else {
	    if (!async_nosuspend)
		return ERTS_PORT_OP_BUSY_SCHEDULED;
	    else {
		if (erts_port_task_abort(ns_pthp) == 0)
		    return ERTS_PORT_OP_BUSY;
		else
		    erts_port_task_tmp_handle_detach(ns_pthp);
	    }
	}
    }
    return res;

bad_value:

    flags |= ERTS_PORT_SIG_FLG_BAD_OUTPUT;
    return bad_port_signal(c_p, flags, prt, from, refp, am_command);
}

static ERTS_INLINE ErtsPortOpResult
call_deliver_port_exit(int bang_op,
		       Eterm from,
		       Port *prt,
		       erts_aint32_t state,
		       Eterm reason,
		       int broken_link)
{
    /*
     * if (bang_op)
     *   we are part of a "Prt ! {From, close}" operation
     * else
     *   we are part of a call to port_close(Port)
     * behave accordingly...
     */

    if (state & ERTS_PORT_SFLGS_INVALID_LOOKUP)
	return ERTS_PORT_OP_DROPPED;

    if (bang_op && from != ERTS_PORT_GET_CONNECTED(prt)) {
	send_badsig(prt);
	return ERTS_PORT_OP_DROPPED;
    }

    if (broken_link) {
	ErtsLink *lnk = erts_remove_link(&ERTS_P_LINKS(prt), from);
	if (lnk)
	    erts_destroy_link(lnk);
	else
	    return ERTS_PORT_OP_DROPPED;
    }

    if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
        trace_port_receive(prt, from, am_close);

    if (!erts_deliver_port_exit(prt, from, reason, bang_op, broken_link))
	return ERTS_PORT_OP_DROPPED;

#ifdef USE_VM_PROBES
    if(DTRACE_ENABLED(port_command) && bang_op) {
	DTRACE_FORMAT_COMMON_PID_AND_PORT(from, prt);
	DTRACE4(port_command, process_str, port_str, prt->name, "close");
    }
#endif

    return ERTS_PORT_OP_DONE;
}

static int
port_sig_exit(Port *prt,
	      erts_aint32_t state,
	      int op,
	      ErtsProc2PortSigData *sigdp)
{
    Eterm msg = am_badarg;
    if (op == ERTS_PROC2PORT_SIG_EXEC) {
	ErtsPortOpResult res;
	int bang_op = sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BANG_OP;
	int broken_link = sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BROKEN_LINK;
	res = call_deliver_port_exit(bang_op,
				     sigdp->u.exit.from,
				     prt,
				     state,
				     sigdp->u.exit.reason,
				     broken_link);

	if (res == ERTS_PORT_OP_DONE)
	    msg = am_true;
    }
    if (sigdp->u.exit.bp)
	free_message_buffer(sigdp->u.exit.bp);
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, msg, prt);

    return ERTS_PORT_REDS_EXIT;
}

ErtsPortOpResult
erts_port_exit(Process *c_p,
	       int flags,
	       Port *prt,
	       Eterm from,
	       Eterm reason,
	       Eterm *refp)
{
    ErtsPortOpResult res;
    ErtsProc2PortSigData *sigdp;
    ErlHeapFragment *bp = NULL;

    ASSERT((flags & ~(ERTS_PORT_SIG_FLG_BANG_OP
		      | ERTS_PORT_SIG_FLG_ASYNC
		      | ERTS_PORT_SIG_FLG_BROKEN_LINK
		      | ERTS_PORT_SIG_FLG_FORCE_SCHED)) == 0);

#ifndef __WIN32__
    if (prt->drv_ptr == &forker_driver)
        return ERTS_PORT_OP_DROPPED;
#endif

    if (!(flags & ERTS_PORT_SIG_FLG_FORCE_SCHED)) {
	ErtsTryImmDrvCallState try_call_state
	    = ERTS_INIT_TRY_IMM_DRV_CALL_STATE(c_p,
					       prt,
					       ERTS_PORT_SFLGS_INVALID_LOOKUP,
					       0,
					       !refp,
					       am_close);


	switch (try_imm_drv_call(&try_call_state)) {
	case ERTS_TRY_IMM_DRV_CALL_OK: {
	    res = call_deliver_port_exit(flags & ERTS_PORT_SIG_FLG_BANG_OP,
					 c_p ? c_p->common.id : from,
					 prt,
					 try_call_state.state,
					 reason,
					 flags & ERTS_PORT_SIG_FLG_BROKEN_LINK);
	    finalize_imm_drv_call(&try_call_state);
	    if (res == ERTS_PORT_OP_DONE && c_p)
		BUMP_REDS(c_p, ERTS_PORT_REDS_EXIT);
	    return res;
	}
	case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	    return ERTS_PORT_OP_DROPPED;
	default:
	    /* Schedule call instead... */
	    break;
	}
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_EXIT | flags;
    sigdp->u.exit.from = from;

    if (is_immed(reason)) {
	sigdp->u.exit.reason = reason;
	sigdp->u.exit.bp = NULL;
    }
    else {
	Eterm *hp;
	Uint hsz = size_object(reason);
	bp = new_message_buffer(hsz);
	sigdp->u.exit.bp = bp;
	hp = bp->mem;
	sigdp->u.exit.reason = copy_struct(reason,
					   hsz,
					   &hp,
					   &bp->off_heap);
    }

    res = erts_schedule_proc2port_signal(c_p,
					 prt,
					 c_p ? c_p->common.id : from,
					 refp,
					 sigdp,
					 0,
					 NULL,
					 port_sig_exit);

    if (res == ERTS_PORT_OP_DROPPED) {
	if (bp)
	    free_message_buffer(bp);
    }

    return res;
}

static ErtsPortOpResult
set_port_connected(int bang_op,
		   Eterm from,
		   Port *prt,
		   erts_aint32_t state,
		   Eterm connect)
{
    /*
     * if (bang_op)
     *   we are part of a "Prt ! {From, {connect, Connect}}" operation
     * else
     *   we are part of a call to port_connect(Port, Connect)
     * behave accordingly...
     */

    if (state & ERTS_PORT_SFLGS_INVALID_LOOKUP)
	return ERTS_PORT_OP_DROPPED;

    if (bang_op) { /* Bang operation */
	if (is_not_internal_pid(connect) || ERTS_PORT_GET_CONNECTED(prt) != from) {
	    send_badsig(prt);
	    return ERTS_PORT_OP_DROPPED;
	}

        if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
            trace_port_receive(prt, from, am_connect, connect);

	ERTS_PORT_SET_CONNECTED(prt, connect);
	deliver_result(prt, prt->common.id, from, am_connected);

#ifdef USE_VM_PROBES
	if(DTRACE_ENABLED(port_command)) {
	    DTRACE_FORMAT_COMMON_PID_AND_PORT(from, prt);
	    DTRACE4(port_command, process_str, port_str, prt->name, "connect");
	}
#endif
    }
    else { /* Port BIF operation */
	Process *rp = erts_proc_lookup_raw(connect);
	if (!rp)
	    return ERTS_PORT_OP_DROPPED;
	erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
	if (ERTS_PROC_IS_EXITING(rp)) {
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	    return ERTS_PORT_OP_DROPPED;
	}

	erts_add_link(&ERTS_P_LINKS(rp), LINK_PID, prt->common.id);
	erts_add_link(&ERTS_P_LINKS(prt), LINK_PID, connect);

        if (IS_TRACED_FL(rp, F_TRACE_PROCS))
            trace_proc(NULL, 0, rp, am_getting_linked, prt->common.id);

	ERTS_PORT_SET_CONNECTED(prt, connect);

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);

        if (IS_TRACED_FL(prt, F_TRACE_PORTS))
            trace_port(prt, am_getting_linked, connect);
        if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
            trace_port_receive(prt, from, am_connect, connect);
        if (IS_TRACED_FL(prt, F_TRACE_SEND)) {
            Eterm hp[3];
            trace_port_send(prt, from, TUPLE2(hp, prt->common.id, am_connected), 1);
        }

#ifdef USE_VM_PROBES
	if (DTRACE_ENABLED(port_connect)) {
	    DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);
	    DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);
	    DTRACE_CHARBUF(newprocess_str, DTRACE_TERM_BUF_SIZE);

	    dtrace_pid_str(connect, process_str);
	    erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)), "%T", prt->common.id);
	    dtrace_proc_str(rp, newprocess_str);
	    DTRACE4(port_connect, process_str, port_str, prt->name, newprocess_str);
	}
#endif
    }

    return ERTS_PORT_OP_DONE;
}

static int
port_sig_connect(Port *prt, erts_aint32_t state, int op, ErtsProc2PortSigData *sigdp)
{
    Eterm msg = am_badarg;
    if (op == ERTS_PROC2PORT_SIG_EXEC) {
	ErtsPortOpResult res;
	res = set_port_connected(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_BANG_OP,
				 sigdp->u.connect.from,
				 prt,
				 state,
				 sigdp->u.connect.connected);
	if (res == ERTS_PORT_OP_DONE)
	    msg = am_true;
    }
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, msg, prt);
    return ERTS_PORT_REDS_CONNECT;
}

ErtsPortOpResult
erts_port_connect(Process *c_p,
		  int flags,
		  Port *prt,
		  Eterm from,
		  Eterm connect,
		  Eterm *refp)
{
    ErtsProc2PortSigData *sigdp;
    Eterm connect_id;
    ErtsTryImmDrvCallState try_call_state
	= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(c_p,
					   prt,
					   ERTS_PORT_SFLGS_INVALID_LOOKUP,
					   0,
					   !refp,
					   am_connect);

    ASSERT((flags & ~(ERTS_PORT_SIG_FLG_BANG_OP
		      | ERTS_PORT_SIG_FLG_ASYNC)) == 0);

    if (is_not_internal_pid(connect))
	connect_id = NIL; /* Fail in op (for signal order) */
    else
	connect_id = connect;

    switch (try_imm_drv_call(&try_call_state)) {
    case ERTS_TRY_IMM_DRV_CALL_OK: {
	ErtsPortOpResult res;
	res = set_port_connected(flags & ERTS_PORT_SIG_FLG_BANG_OP,
				 from,
				 prt,
				 try_call_state.state,
				 connect_id);
	finalize_imm_drv_call(&try_call_state);
	if (res == ERTS_PORT_OP_DONE)
	    BUMP_REDS(c_p, ERTS_PORT_REDS_CONNECT);
	return res;
    }
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	return ERTS_PORT_OP_DROPPED;
    default:
	/* Schedule call instead... */
	break;
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_CONNECT | flags;

    sigdp->u.connect.from = from;
    sigdp->u.connect.connected = connect_id;
    
    return erts_schedule_proc2port_signal(c_p,
					  prt,
					  c_p->common.id,
					  refp,
					  sigdp,
					  0,
					  NULL,
					  port_sig_connect);
}

static void
port_unlink(Port *prt, Eterm from)
{
    ErtsLink *lnk = erts_remove_link(&ERTS_P_LINKS(prt), from);
    if (lnk) {
        if (IS_TRACED_FL(prt, F_TRACE_PORTS))
            trace_port(prt, am_getting_unlinked, from);
	erts_destroy_link(lnk);
    }
}

static int
port_sig_unlink(Port *prt, erts_aint32_t state, int op, ErtsProc2PortSigData *sigdp)
{
    if (op == ERTS_PROC2PORT_SIG_EXEC)
	port_unlink(prt, sigdp->u.unlink.from);
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, am_true, prt);
    return ERTS_PORT_REDS_UNLINK;
}

ErtsPortOpResult
erts_port_unlink(Process *c_p, Port *prt, Eterm from, Eterm *refp)
{
    ErtsProc2PortSigData *sigdp;
    ErtsTryImmDrvCallState try_call_state
	= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(c_p,
					   prt,
					   ERTS_PORT_SFLGS_DEAD,
					   0,
					   !refp,
					   am_unlink);

    switch (try_imm_drv_call(&try_call_state)) {
    case ERTS_TRY_IMM_DRV_CALL_OK:
	port_unlink(prt, from);
	finalize_imm_drv_call(&try_call_state);
	BUMP_REDS(c_p, ERTS_PORT_REDS_UNLINK);
	return ERTS_PORT_OP_DONE;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	return ERTS_PORT_OP_DROPPED;
    default:
	/* Schedule call instead... */
	break;
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_UNLINK;
    sigdp->u.unlink.from = from;
    
    return erts_schedule_proc2port_signal(c_p,
					  prt,
					  c_p ? c_p->common.id : from,
					  refp,
					  sigdp,
					  0,
					  NULL,
					  port_sig_unlink);
}

static void
port_link_failure(Eterm port_id, Eterm linker)
{
    Process *rp;
    ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
    ASSERT(is_internal_pid(linker));
    rp = erts_pid2proc(NULL, 0, linker, rp_locks);
    if (rp) {
	ErtsLink *rlnk = erts_remove_link(&ERTS_P_LINKS(rp), port_id);
	if (rlnk) {
	    int xres = erts_send_exit_signal(NULL,
					     port_id,
					     rp,
					     &rp_locks, 
					     am_noproc,
					     NIL,
					     NULL,
					     0);
	    if (xres >= 0) {
		/* We didn't exit the process and it is traced */
		if (IS_TRACED_FL(rp, F_TRACE_PROCS))
		    trace_proc(NULL, 0, rp, am_getting_unlinked, port_id);
	    }
	    if (rp_locks)
		erts_smp_proc_unlock(rp, rp_locks);
	}
    }
}

static void
port_link(Port *prt, erts_aint32_t state, Eterm to)
{
    if (IS_TRACED_FL(prt, F_TRACE_PORTS))
        trace_port(prt, am_getting_linked, to);
    if (!(state & ERTS_PORT_SFLGS_INVALID_LOOKUP)) {
	erts_add_link(&ERTS_P_LINKS(prt), LINK_PID, to);
    } else {
	port_link_failure(prt->common.id, to);
        if (IS_TRACED_FL(prt, F_TRACE_PORTS))
            trace_port(prt, am_unlink, to);
    }
}

static int
port_sig_link(Port *prt, erts_aint32_t state, int op, ErtsProc2PortSigData *sigdp)
{
    if (op == ERTS_PROC2PORT_SIG_EXEC)
	port_link(prt, state, sigdp->u.link.to);
    else {
	port_link_failure(sigdp->u.link.port, sigdp->u.link.to);
    }
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY)
	port_sched_op_reply(sigdp->caller, sigdp->ref, am_true, prt);
    return ERTS_PORT_REDS_LINK;
}

ErtsPortOpResult
erts_port_link(Process *c_p, Port *prt, Eterm to, Eterm *refp)
{
    ErtsProc2PortSigData *sigdp;
    ErtsTryImmDrvCallState try_call_state
	= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(c_p,
					   prt,
					   ERTS_PORT_SFLGS_INVALID_LOOKUP,
					   0,
					   !refp,
					   am_link);

    switch (try_imm_drv_call(&try_call_state)) {
    case ERTS_TRY_IMM_DRV_CALL_OK:
	port_link(prt, try_call_state.state, to);
	finalize_imm_drv_call(&try_call_state);
	BUMP_REDS(c_p, ERTS_PORT_REDS_LINK);
	return ERTS_PORT_OP_DONE;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	return ERTS_PORT_OP_BADARG;
    default:
	/* Schedule call instead... */
	break;
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_LINK;
    sigdp->u.link.port = prt->common.id;
    sigdp->u.link.to = to;
    
    return erts_schedule_proc2port_signal(c_p,
					  prt,
					  c_p ? c_p->common.id : to,
					  refp,
					  sigdp,
					  0,
					  NULL,
					  port_sig_link);
}

static void
port_monitor_failure(Eterm port_id, Eterm origin, Eterm ref_DOWN)
{
    Process       *origin_p;
    ErtsProcLocks p_locks = ERTS_PROC_LOCK_LINK;
    ASSERT(is_internal_pid(origin));

    origin_p = erts_pid2proc(NULL, 0, origin, p_locks);
    if (! origin_p) { return; }

    /* Send the DOWN message immediately. Ref is made on the fly because
     * caller has never seen it yet. */
    erts_queue_monitor_message(origin_p, &p_locks, ref_DOWN,
                               am_port, port_id, am_noproc);
    erts_smp_proc_unlock(origin_p, p_locks);
}

/* Origin wants to monitor port Prt. State contains possible error, which has
 * happened just before. Name is either NIL or an atom, if user monitors
 * a port by name. Ref is premade reference that will be returned to user */
static void
port_monitor(Port *prt, erts_aint32_t state, Eterm origin,
             Eterm name, Eterm ref)
{
    Eterm name_or_nil = is_atom(name) ? name : NIL;

    ASSERT(is_pid(origin));
    ASSERT(is_atom(name) || is_port(name) || name == NIL);
    ASSERT(is_internal_ref(ref));

    if (!(state & ERTS_PORT_SFLGS_INVALID_LOOKUP)) {
        ErtsProcLocks p_locks = ERTS_PROC_LOCK_LINK;

        Process *origin_p = erts_pid2proc(NULL, 0, origin, p_locks);
        if (! origin_p) {
            goto failure;
        }
        erts_add_monitor(&ERTS_P_MONITORS(origin_p), MON_ORIGIN, ref,
                         prt->common.id, name_or_nil);
        erts_add_monitor(&ERTS_P_MONITORS(prt), MON_TARGET, ref,
                         origin, name_or_nil);

        erts_smp_proc_unlock(origin_p, p_locks);
    } else {
failure:
        port_monitor_failure(prt->common.id, origin, ref);
    }
}

static int
port_sig_monitor(Port *prt, erts_aint32_t state, int op,
                 ErtsProc2PortSigData *sigdp)
{
    Eterm hp[REF_THING_SIZE];
    Eterm ref = make_internal_ref(&hp);
    write_ref_thing(hp, sigdp->ref[0], sigdp->ref[1], sigdp->ref[2]);

    if (op == ERTS_PROC2PORT_SIG_EXEC) {
        /* erts_add_monitor call inside port_monitor will copy ref from hp */
        port_monitor(prt, state,
                     sigdp->u.monitor.origin,
                     sigdp->u.monitor.name,
                     ref);
    } else {
        port_monitor_failure(sigdp->u.monitor.name,
                             sigdp->u.monitor.origin,
                             ref);
    }
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY) {
        port_sched_op_reply(sigdp->caller, sigdp->ref, am_true, prt);
    }
    return ERTS_PORT_REDS_MONITOR;
}

/* Creates monitor between Origin and Target. Ref must be initialized to
 * a reference (ref may be rewritten to be used to serve additionally as a
 * signal id). Name is atom if user monitors port by name or NIL */
ErtsPortOpResult
erts_port_monitor(Process *origin, Port *port, Eterm name, Eterm *refp)
{
    ErtsProc2PortSigData *sigdp;
    ErtsTryImmDrvCallState try_call_state
        = ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
                origin, port, ERTS_PORT_SFLGS_INVALID_LOOKUP,
                0,
                0, /* trap_ref is always set so !trap_ref always is false */
                am_monitor);

    ASSERT(origin);
    ASSERT(port);
    ASSERT(is_atom(name) || is_port(name));
    ASSERT(refp);

    switch (try_imm_drv_call(&try_call_state)) {
    case ERTS_TRY_IMM_DRV_CALL_OK:
        port_monitor(port, try_call_state.state, origin->common.id, name, *refp);
        finalize_imm_drv_call(&try_call_state);
        BUMP_REDS(origin, ERTS_PORT_REDS_MONITOR);
        return ERTS_PORT_OP_DONE;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
        return ERTS_PORT_OP_BADARG;
    default:
        break; /* Schedule call instead... */
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_MONITOR;
    sigdp->u.monitor.origin = origin->common.id;
    sigdp->u.monitor.name   = name; /* either named monitor, or port id */

    /* Ref contents will be initialized here */
    return erts_schedule_proc2port_signal(origin, port, origin->common.id,
                                          refp, sigdp, 0, NULL,
                                          port_sig_monitor);
}

static void
port_demonitor_failure(Eterm port_id, Eterm origin, Eterm ref)
{
    Process       *origin_p;
    ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;
    ErtsMonitor   *mon1;
    ASSERT(is_internal_pid(origin));

    origin_p = erts_pid2proc(NULL, 0, origin, rp_locks);
    if (! origin_p) { return; }

    /* do not send any DOWN messages, drop monitors on process */
    mon1 = erts_remove_monitor(&ERTS_P_MONITORS(origin_p), ref);
    if (mon1 != NULL) {
        erts_destroy_monitor(mon1);
    }

    erts_smp_proc_unlock(origin_p, rp_locks);
}

/* Origin wants to demonitor port Prt. State contains possible error, which has
 * happened just before. Ref is reference to monitor */
static void
port_demonitor(Port *port, erts_aint32_t state, Eterm origin, Eterm ref)
{
    ASSERT(port);
    ASSERT(is_pid(origin));
    ASSERT(is_internal_ref(ref));

    if (!(state & ERTS_PORT_SFLGS_INVALID_LOOKUP)) {
        ErtsProcLocks p_locks = ERTS_PROC_LOCK_LINK;
        Process *origin_p = erts_pid2proc(NULL, 0, origin, p_locks);
        if (origin_p) {
            ErtsMonitor *mon1 = erts_remove_monitor(&ERTS_P_MONITORS(origin_p),
                                                    ref);
            if (mon1 != NULL) {
                erts_destroy_monitor(mon1);
            }
        }
        if (1) {
            ErtsMonitor *mon2 = erts_remove_monitor(&ERTS_P_MONITORS(port),
                                                    ref);
            if (mon2 != NULL) {
                erts_destroy_monitor(mon2);
            }
        }
        if (origin_p) { /* when origin is dying, it won't be found */
            erts_smp_proc_unlock(origin_p, p_locks);
        }
    } else {
        port_demonitor_failure(port->common.id, origin, ref);
    }
}

static int
port_sig_demonitor(Port *prt, erts_aint32_t state, int op,
                   ErtsProc2PortSigData *sigdp)
{
    Eterm hp[REF_THING_SIZE];
    Eterm ref = make_internal_ref(&hp);
    write_ref_thing(hp, sigdp->u.demonitor.ref[0],
                    sigdp->u.demonitor.ref[1],
                    sigdp->u.demonitor.ref[2]);
    if (op == ERTS_PROC2PORT_SIG_EXEC) {
        port_demonitor(prt, state, sigdp->u.demonitor.origin, ref);
    } else {
        port_demonitor_failure(sigdp->u.demonitor.name,
                               sigdp->u.demonitor.origin,
                               ref);
    }
    if (sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY) {
        port_sched_op_reply(sigdp->caller, sigdp->ref, am_true, prt);
    }
    return ERTS_PORT_REDS_DEMONITOR;
}

/* Removes monitor between origin and target, identified by ref.
 * Mode defines normal or relaxed demonitor rules (process is at death) */
ErtsPortOpResult erts_port_demonitor(Process *origin, ErtsDemonitorMode mode,
                                     Port *target, Eterm ref,
                                     Eterm *trap_ref)
{
    Process *c_p = mode == ERTS_PORT_DEMONITOR_NORMAL ? origin : NULL;
    ErtsProc2PortSigData *sigdp;
    ErtsTryImmDrvCallState try_call_state
        = ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
                c_p,
                target, ERTS_PORT_SFLGS_INVALID_LOOKUP,
                0,
                !trap_ref,
                am_demonitor);

    ASSERT(origin);
    ASSERT(target);
    ASSERT(is_internal_ref(ref));

    switch (try_imm_drv_call(&try_call_state)) {
    case ERTS_TRY_IMM_DRV_CALL_OK:
        port_demonitor(target, try_call_state.state, origin->common.id, ref);
        finalize_imm_drv_call(&try_call_state);
        if (mode == ERTS_PORT_DEMONITOR_NORMAL) {
            BUMP_REDS(origin, ERTS_PORT_REDS_DEMONITOR);
        }
        return ERTS_PORT_OP_DONE;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
        return ERTS_PORT_OP_BADARG;
    default:
        break; /* Schedule call instead... */
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_DEMONITOR;
    sigdp->u.demonitor.origin = origin->common.id;
    sigdp->u.demonitor.name = target->common.id;
    {
        RefThing *reft = ref_thing_ptr(ref);
        /* Start from 1 skip ref arity */
        sys_memcpy(sigdp->u.demonitor.ref,
                   internal_thing_ref_numbers(reft),
                   sizeof(sigdp->u.demonitor.ref));
    }

    /* Ref contents will be initialized here */
    return erts_schedule_proc2port_signal(c_p, target, origin->common.id,
                                          trap_ref, sigdp, 0, NULL,
                                          port_sig_demonitor);
}

static void
init_ack_send_reply(Port *port, Eterm resp)
{

    if (!is_internal_port(resp)) {
        Process *rp = erts_proc_lookup_raw(port->async_open_port->to);
        erts_smp_proc_lock(rp, ERTS_PROC_LOCK_LINK);
        erts_remove_link(&ERTS_P_LINKS(port), port->async_open_port->to);
        erts_remove_link(&ERTS_P_LINKS(rp), port->common.id);
        erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    }
    port_sched_op_reply(port->async_open_port->to,
                        port->async_open_port->ref,
                        resp,
			port);

    erts_free(ERTS_ALC_T_PRTSD, port->async_open_port);
    port->async_open_port = NULL;
}

void
erl_drv_init_ack(ErlDrvPort ix, ErlDrvData res) {
    Port *port = erts_drvport2port(ix);
    SWord err_type = (SWord)res;
    Eterm resp;

    if (port == ERTS_INVALID_ERL_DRV_PORT && port->async_open_port)
        return;

    if (port->async_open_port) {
        switch(err_type) {
        case -3:
            resp = am_badarg;
            break;
        case -2: {
            char *str = erl_errno_id(errno);
            resp = erts_atom_put((byte *) str, strlen(str),
                                 ERTS_ATOM_ENC_LATIN1, 1);
            break;
        }
        case -1:
            resp = am_einval;
            break;
        default:
            resp = port->common.id;
            break;
        }

        init_ack_send_reply(port, resp);

        if (err_type == -1 || err_type == -2 || err_type == -3)
            driver_failure_term(ix, am_normal, 0);
        port->drv_data = err_type;
    }
}

void
erl_drv_set_os_pid(ErlDrvPort ix, ErlDrvSInt pid) {
    Port *port = erts_drvport2port(ix);

    if (port == ERTS_INVALID_ERL_DRV_PORT)
        return;

    port->os_pid = (SWord)pid;

}

void erts_init_io(int port_tab_size,
		  int port_tab_size_ignore_files,
		  int legacy_port_tab)
{
    ErlDrvEntry** dp;
    UWord common_element_size;
    erts_smp_rwmtx_opt_t drv_list_rwmtx_opts = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    drv_list_rwmtx_opts.type = ERTS_SMP_RWMTX_TYPE_EXTREMELY_FREQUENT_READ;
    drv_list_rwmtx_opts.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_atomic64_init_nob(&bytes_in, 0);
    erts_atomic64_init_nob(&bytes_out, 0);

    common_element_size = ERTS_ALC_DATA_ALIGN_SIZE(sizeof(Port));
    common_element_size += ERTS_ALC_DATA_ALIGN_SIZE(sizeof(ErtsPortTaskBusyPortQ));
    common_element_size += 10; /* name */
#ifdef ERTS_SMP
    common_element_size += sizeof(erts_mtx_t);

    init_xports_list_alloc();
#endif

    pdl_init();

    if (!port_tab_size_ignore_files) {
	int max_files = sys_max_files();
	if (port_tab_size < max_files)
	    port_tab_size = max_files;
    }

    if (port_tab_size > ERTS_MAX_PORTS)
	port_tab_size = ERTS_MAX_PORTS;
    else if (port_tab_size < ERTS_MIN_PORTS)
	port_tab_size = ERTS_MIN_PORTS;

    erts_smp_rwmtx_init_opt(&erts_driver_list_lock,
			    &drv_list_rwmtx_opts,
			    "driver_list");
    driver_list = NULL;
    erts_smp_tsd_key_create(&driver_list_lock_status_key,
			    "erts_driver_list_lock_status_key");
    erts_smp_tsd_key_create(&driver_list_last_error_key,
			    "erts_driver_list_last_error_key");

    erts_ptab_init_table(&erts_port,
			 ERTS_ALC_T_PORT_TABLE,
			 NULL,
			 (ErtsPTabElementCommon *) &erts_invalid_port.common,
			 port_tab_size,
			 common_element_size, /* Doesn't need to be excact */
			 "port_table",
			 legacy_port_tab,
			 1);

    sys_init_io();

    erts_smp_tsd_set(driver_list_lock_status_key, (void *) 1);
    erts_smp_rwmtx_rwlock(&erts_driver_list_lock);

    init_driver(&fd_driver, &fd_driver_entry, NULL);
    init_driver(&vanilla_driver, &vanilla_driver_entry, NULL);
    init_driver(&spawn_driver, &spawn_driver_entry, NULL);
#ifndef __WIN32__
    init_driver(&forker_driver, &forker_driver_entry, NULL);
#endif
    erts_init_static_drivers();
    for (dp = driver_tab; *dp != NULL; dp++)
	erts_add_driver_entry(*dp, NULL, 1);

    erts_smp_tsd_set(driver_list_lock_status_key, NULL);
    erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
}

#if defined(ERTS_ENABLE_LOCK_COUNT) && defined(ERTS_SMP)
static ERTS_INLINE void lcnt_enable_drv_lock_count(erts_driver_t *dp, int enable)
{
    if (dp->lock) {
	if (enable)
	    erts_lcnt_init_lock_x(&dp->lock->lcnt,
				  "driver_lock",
				  ERTS_LCNT_LT_MUTEX,
				  erts_atom_put((byte*)dp->name,
						sys_strlen(dp->name),
						ERTS_ATOM_ENC_LATIN1,
						1));

	else
	    erts_lcnt_destroy_lock(&dp->lock->lcnt);

    }
}

static ERTS_INLINE void lcnt_enable_port_lock_count(Port *prt, int enable)
{
    erts_aint32_t state = erts_atomic32_read_nob(&prt->state);
    if (!enable) {
	erts_lcnt_destroy_lock(&prt->sched.mtx.lcnt);
	if (state & ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK)
	    erts_lcnt_destroy_lock(&prt->lock->lcnt);
    }
    else {
	erts_lcnt_init_lock_x(&prt->sched.mtx.lcnt,
			      "port_sched_lock",
			      ERTS_LCNT_LT_MUTEX,
			      prt->common.id);
	if (state & ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK)
	    erts_lcnt_init_lock_x(&prt->lock->lcnt,
				  "port_lock",
				  ERTS_LCNT_LT_MUTEX,
				  prt->common.id);
    }
}

void erts_lcnt_enable_io_lock_count(int enable) {
    erts_driver_t *dp;
    int ix, max = erts_ptab_max(&erts_port);
    Port *prt;

    for (ix = 0; ix < max; ix++) {
	if ((prt = erts_pix2port(ix)) != NULL) {
	    lcnt_enable_port_lock_count(prt, enable);
        }
    } /* for all ports */

    lcnt_enable_drv_lock_count(&vanilla_driver, enable);
    lcnt_enable_drv_lock_count(&spawn_driver, enable);
#ifndef __WIN32__
    lcnt_enable_drv_lock_count(&forker_driver, enable);
#endif
    lcnt_enable_drv_lock_count(&fd_driver, enable);
    /* enable lock counting in all drivers */
    for (dp = driver_list; dp; dp = dp->next) {
	lcnt_enable_drv_lock_count(dp, enable);
    }
} /* enable/disable lock counting of ports */
#endif /* defined(ERTS_ENABLE_LOCK_COUNT) && defined(ERTS_SMP) */
/*
 * Buffering of data when using line oriented I/O on ports
 */

/* 
 * Buffer states 
 */
#define LINEBUF_MAIN 0
#define LINEBUF_FULL 1
#define LINEBUF_CR_INSIDE 2
#define LINEBUF_CR_AFTER 3

/*
 * Creates a LineBuf to be added to the port structure,
 * Returns: Pointer to a newly allocated and initialized LineBuf.
 * Parameters:
 * bufsiz - The (maximum) size of the line buffer.
 */
LineBuf *allocate_linebuf(bufsiz)
int bufsiz;
{
    int ovsiz = (bufsiz < LINEBUF_INITIAL) ? bufsiz : LINEBUF_INITIAL;
    LineBuf *lb = (LineBuf *) erts_alloc(ERTS_ALC_T_LINEBUF,
					 sizeof(LineBuf)+ovsiz);
    lb->ovsiz = ovsiz;
    lb->bufsiz = bufsiz;
    lb->ovlen = 0;
    lb->data[0] = LINEBUF_MAIN; /* state */
    return lb;
}

/*
 * Initializes a LineBufContext to be used in calls to read_linebuf
 * or flush_linebuf.
 * Returns: 0 if ok, <0 on error.
 * Parameters:
 * lc - Pointer to an allocated LineBufContext.
 * lb - Pointer to a LineBuf structure (probably from the Port structure).
 * buf - A buffer containing the data to be read and split to lines.
 * len - The number of bytes in buf.
 */
static int init_linebuf_context(LineBufContext *lc, LineBuf **lb,
				char *buf, ErlDrvSizeT len)
{
    if(lc == NULL || lb == NULL)
	return -1;
    lc->b = lb;
    lc->buf = buf;
    lc->left = len;
    return 0;
}

static void resize_linebuf(LineBuf **b)
{
    int newsiz = (((*b)->ovsiz * 2) > (*b)->bufsiz) ? (*b)->bufsiz : 
	(*b)->ovsiz * 2;
    *b = (LineBuf *) erts_realloc(ERTS_ALC_T_LINEBUF,
				  (void *) *b,
				  sizeof(LineBuf)+newsiz);
    (*b)->ovsiz = newsiz;
}

/*
 * Delivers all data in the buffer regardless of newlines (always
 * an LINEBUF_NOEOL. Has to be called until it return LINEBUF_EMPTY.
 * Return values and barameters as read_linebuf (see below).
 */
static int flush_linebuf(LineBufContext *bp)
{
    bp->retlen = (*bp->b)->ovlen;
    switch(LINEBUF_STATE(*bp)){
    case LINEBUF_CR_INSIDE:
	if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
	    resize_linebuf(bp->b);
	LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = '\r';
	++bp->retlen; /* fall through instead of switching state... */
    case LINEBUF_MAIN:
    case LINEBUF_FULL:
	(*bp->b)->ovlen = 0;
	LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	if(!bp->retlen)
	    return LINEBUF_EMPTY;
	return LINEBUF_NOEOL;
    case LINEBUF_CR_AFTER:
	LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	(*bp->b)->ovlen = 0;
	if(!bp->retlen)
	    return LINEBUF_EMPTY;
	return LINEBUF_NOEOL;
    default:
	return LINEBUF_ERROR;
    }    
}

/*
 * Reads input from a buffer and "chops" it up in lines.
 * Has to be called repeatedly until it returns LINEBUF_EMPTY
 * to get all lines in buffer.
 * Handles both <LF> and <CR><LF> style newlines.
 * On Unix, this is slightly incorrect, as <CR><LF> is NOT to be regarded
 * as a newline together, but i treat newlines equally in all systems
 * to avoid putting this in sys.c or clutter it with #ifdef's.
 * Returns: LINEBUF_EMPTY if there is no more data that can be
 *   determined as a line (only part of a line left), LINEBUF_EOL if a whole
 *   line could be delivered and LINEBUF_NOEOL if the buffer size has been
 *   exceeded. The data and the data length can be accesed through the 
 *   LINEBUF_DATA and the LINEBUF_DATALEN macros applied to the LineBufContext.
 * Parameters: 
 * bp - A LineBufContext that is initialized with 
 *   the init_linebuf_context call. The context has to be retained during
 *   all calls that returns other than LINEBUF_EMPTY. When LINEBUF_EMPTY
 *   is returned the context can be discarded and a new can be created when new
 *   data arrives (the state is saved in the Port structure).
 */
static int read_linebuf(LineBufContext *bp)
{
    for(;;){
	if(bp->left == 0)
	    return LINEBUF_EMPTY;
	if(*bp->buf == '\n'){
	    LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	    ++(bp->buf);
	    --(bp->left);
	    bp->retlen = (*bp->b)->ovlen;
	    (*bp->b)->ovlen = 0;
	    return LINEBUF_EOL;
	}
	switch(LINEBUF_STATE(*bp)){
	case LINEBUF_MAIN:
	    if((*bp->b)->ovlen == (*bp->b)->bufsiz)
		LINEBUF_STATE(*bp) = LINEBUF_FULL;
	    else if(*bp->buf == '\r'){
		++(bp->buf);
		--(bp->left);
		LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	    } else {
		if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
		    resize_linebuf(bp->b);
		LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = *((bp->buf)++);
		--(bp->left);
	    }
	    continue;
	case LINEBUF_FULL:
	    if(*bp->buf == '\r'){
		++(bp->buf);
		--(bp->left);
		LINEBUF_STATE(*bp) = LINEBUF_CR_AFTER;
	    } else {
		bp->retlen = (*bp->b)->ovlen;
		(*bp->b)->ovlen = 0;
		LINEBUF_STATE(*bp) = LINEBUF_MAIN;
		return LINEBUF_NOEOL;
	    }
	    continue;
	case LINEBUF_CR_INSIDE:
	    if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
		resize_linebuf(bp->b);
	    LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = '\r';
	    LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	    continue;
	case LINEBUF_CR_AFTER:
	    bp->retlen = (*bp->b)->ovlen;
	    (*bp->b)->ovlen = 0;
	    LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	    return LINEBUF_NOEOL;
	default:
	    return LINEBUF_ERROR;
	}
    }
}

static void
deliver_result(Port *prt, Eterm sender, Eterm pid, Eterm res)
{
    Process *rp;
    ErtsProcLocks rp_locks = 0;
    int scheduler = erts_get_scheduler_id() != 0;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    ASSERT(!prt || prt->common.id == sender);
#ifdef ERTS_SMP
    ASSERT(!prt || erts_lc_is_port_locked(prt));
#endif

    ASSERT(is_internal_port(sender) && is_internal_pid(pid));

    rp = (scheduler
	  ? erts_proc_lookup(pid)
	  : erts_pid2proc_opt(NULL, 0, pid, 0, ERTS_P2P_FLG_INC_REFC));

    if (prt && IS_TRACED_FL(prt, F_TRACE_SEND)) {
        Eterm hp[3];
        trace_port_send(prt, pid, TUPLE2(hp, sender, res), !!rp);
    }

    if (rp) {
	Eterm tuple;
	ErtsMessage *mp;
	ErlOffHeap *ohp;
	Eterm* hp;
	Uint sz_res;

	sz_res = size_object(res);
	mp = erts_alloc_message_heap(rp, &rp_locks,
				     sz_res + 3, &hp, &ohp);
	res = copy_struct(res, sz_res, &hp, ohp);
	tuple = TUPLE2(hp, sender, res);
	erts_queue_message(rp, rp_locks, mp, tuple, sender);

	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
	if (!scheduler)
	    erts_proc_dec_refc(rp);

    }
}


/* 
 * Deliver a "read" message.
 * hbuf -- byte that are always formated as a list
 * hlen -- number of byte in header
 * buf  -- data 
 * len  -- length of data
 */

static void deliver_read_message(Port* prt, erts_aint32_t state, Eterm to,
				 char *hbuf, ErlDrvSizeT hlen,
				 char *buf, ErlDrvSizeT len, int eol)
{
    ErlDrvSizeT need;
    Eterm listp;
    Eterm tuple;
    Process* rp;
    Eterm* hp;
    ErtsMessage *mp;
    ErlOffHeap *ohp;
    ErtsProcLocks rp_locks = 0;
    int scheduler = erts_get_scheduler_id() != 0;
    int trace_send = IS_TRACED_FL(prt, F_TRACE_SEND);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));    
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    need = 3 + 3 + 2*hlen;

    if (state & ERTS_PORT_SFLG_LINEBUF_IO) {
	need += 3;
    }
    if ((state & ERTS_PORT_SFLG_BINARY_IO) && buf != NULL) {
	need += PROC_BIN_SIZE;
    } else {
	need += 2*len;
    }

    rp = (scheduler
	  ? erts_proc_lookup(to)
	  : erts_pid2proc_opt(NULL, 0, to, 0, ERTS_P2P_FLG_INC_REFC));

    if (!rp)
	return;

    mp = erts_alloc_message_heap(trace_send ? NULL : rp, &rp_locks, need, &hp, &ohp);

    listp = NIL;
    if ((state & ERTS_PORT_SFLG_BINARY_IO) == 0) {
	listp = buf_to_intlist(&hp, buf, len, listp);
    } else if (buf != NULL) {
	ProcBin* pb;
	Binary* bptr;

	bptr = erts_bin_nrml_alloc(len);
	erts_refc_init(&bptr->refc, 1);
	sys_memcpy(bptr->orig_bytes, buf, len);

	pb = (ProcBin *) hp;
	pb->thing_word = HEADER_PROC_BIN;
	pb->size = len;
	pb->next = ohp->first;
	ohp->first = (struct erl_off_heap_header*)pb;
	pb->val = bptr;
	pb->bytes = (byte*) bptr->orig_bytes;
	pb->flags = 0;
	hp += PROC_BIN_SIZE;

	OH_OVERHEAD(ohp, pb->size / sizeof(Eterm));
	listp = make_binary(pb);
    }

    /* Prepend the header */
    if (hlen > 0) {
	listp = buf_to_intlist(&hp, hbuf, hlen, listp);
    }

    if (state & ERTS_PORT_SFLG_LINEBUF_IO){
	listp = TUPLE2(hp, (eol) ? am_eol : am_noeol, listp); 
	hp += 3;
    }
    tuple = TUPLE2(hp, am_data, listp);
    hp += 3;

    tuple = TUPLE2(hp, prt->common.id, tuple);
    hp += 3;

    if (trace_send)
        trace_port_send(prt, to, tuple, 1);

    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    erts_queue_message(rp, rp_locks, mp, tuple, prt->common.id);
    if (rp_locks)
	erts_smp_proc_unlock(rp, rp_locks);
    if (!scheduler)
	erts_proc_dec_refc(rp);
}

/* 
 * Deliver all lines in a line buffer, repeats calls to
 * deliver_read_message, and takes the same parameters.
 */
static void deliver_linebuf_message(Port* prt, erts_aint_t state,
				    Eterm to, 
				    char* hbuf, ErlDrvSizeT hlen,
				    char *buf, ErlDrvSizeT len)
{
    LineBufContext lc;
    int ret;
    if(init_linebuf_context(&lc,&(prt->linebuf), buf, len) < 0)
	return;
    while((ret = read_linebuf(&lc)) > LINEBUF_EMPTY)
	deliver_read_message(prt, state, to, hbuf, hlen, LINEBUF_DATA(lc), 
			     LINEBUF_DATALEN(lc), (ret == LINEBUF_EOL));
}

/*
 * Deliver any nonterminated lines in the line buffer before the
 * port gets closed.
 * Has to be called before terminate_port.
 * Parameters:
 * prt -  Pointer to a Port structure for this port.
 */
static void flush_linebuf_messages(Port *prt, erts_aint32_t state)
{
    LineBufContext lc;
    int ret;

    ERTS_SMP_LC_ASSERT(!prt || erts_lc_is_port_locked(prt));

    if (!prt)
	return;

    if (!(state & ERTS_PORT_SFLG_LINEBUF_IO))
	return;

    if(init_linebuf_context(&lc,&(prt->linebuf), NULL, 0) < 0)
	return;
    while((ret = flush_linebuf(&lc)) > LINEBUF_EMPTY)
	deliver_read_message(prt,
			     state,
			     ERTS_PORT_GET_CONNECTED(prt),
			     NULL,
			     0,
			     LINEBUF_DATA(lc), 
			     LINEBUF_DATALEN(lc),
			     (ret == LINEBUF_EOL));
}    

static void
deliver_vec_message(Port* prt,			/* Port */
		    Eterm to,			/* Receiving pid */
		    char* hbuf,			/* "Header" buffer... */
		    ErlDrvSizeT hlen,		/* ... and its length */
		    ErlDrvBinary** binv,	/* Vector of binaries */
		    SysIOVec* iov,		/* I/O vector */
		    int vsize,			/* Size of binv & iov */
		    ErlDrvSizeT csize)		/* Size of characters in
						   iov (not hlen) */
{
    ErlDrvSizeT need;
    Eterm listp;
    Eterm tuple;
    Process* rp;
    Eterm* hp;
    ErtsMessage *mp;
    ErlOffHeap *ohp;
    ErtsProcLocks rp_locks = 0;
    int scheduler = erts_get_scheduler_id() != 0;
    erts_aint32_t state;
    int trace_send = IS_TRACED_FL(prt, F_TRACE_SEND);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    /*
     * Check arguments for validity.
     */


    rp = (scheduler
	  ? erts_proc_lookup(to)
	  : erts_pid2proc_opt(NULL, 0, to, 0, ERTS_P2P_FLG_INC_REFC));
    if (!rp)
	return;

    state = erts_atomic32_read_nob(&prt->state);
    /*
     * Calculate the exact number of heap words needed.
     */

    need = 3 + 3;		/* Heap space for two tuples */
    if (state & ERTS_PORT_SFLG_BINARY_IO) {
	need += (2+PROC_BIN_SIZE)*vsize - 2 + hlen*2;
    } else {
	need += (hlen+csize)*2;
    }

    mp = erts_alloc_message_heap(trace_send ? NULL : rp, &rp_locks, need, &hp, &ohp);

    listp = NIL;
    iov += vsize;

    if ((state & ERTS_PORT_SFLG_BINARY_IO) == 0) {
	Eterm* thp = hp;
	while (vsize--) {
	    iov--;
	    listp = buf_to_intlist(&thp, iov->iov_base, iov->iov_len, listp);
	}
	hp = thp;
    } else {
	binv += vsize;
	while (vsize--) {
	    ErlDrvBinary* b;
	    ProcBin* pb = (ProcBin*) hp;
	    byte* base;

	    iov--;
	    binv--;
	    if ((b = *binv) == NULL) {
		b = driver_alloc_binary(iov->iov_len);
		sys_memcpy(b->orig_bytes, iov->iov_base, iov->iov_len);
		base = (byte*) b->orig_bytes;
	    } else {
		/* Must increment reference count, caller calls free */
		driver_binary_inc_refc(b);
		base = iov->iov_base;
	    }
	    pb->thing_word = HEADER_PROC_BIN;
	    pb->size = iov->iov_len;
	    pb->next = ohp->first;
	    ohp->first = (struct erl_off_heap_header*)pb;
	    pb->val = ErlDrvBinary2Binary(b);
	    pb->bytes = base;
	    pb->flags = 0;
	    hp += PROC_BIN_SIZE;
	    
	    OH_OVERHEAD(ohp, iov->iov_len / sizeof(Eterm));

	    if (listp == NIL) {  /* compatible with deliver_bin_message */
		listp = make_binary(pb);
	    } else {
		listp = CONS(hp, make_binary(pb), listp);
		hp += 2;
	    }
	}
    }

    if (hlen != 0) {		/* Prepend the header */
	Eterm* thp = hp;
	listp = buf_to_intlist(&thp, hbuf, hlen, listp);
	hp = thp;
    }

    tuple = TUPLE2(hp, am_data, listp);
    hp += 3;
    tuple = TUPLE2(hp, prt->common.id, tuple);
    hp += 3;

    if (IS_TRACED_FL(prt, F_TRACE_SEND))
        trace_port_send(prt, to, tuple, 1);

    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    erts_queue_message(rp, rp_locks, mp, tuple, prt->common.id);
    erts_smp_proc_unlock(rp, rp_locks);
    if (!scheduler)
	erts_proc_dec_refc(rp);
}


static void deliver_bin_message(Port*  prt,         /* port */
				Eterm to,           /* receiving pid */
				char* hbuf,         /* "header" buffer */
				ErlDrvSizeT hlen,   /* and it's length */
				ErlDrvBinary* bin,  /* binary data */
				ErlDrvSizeT offs,   /* offset into binary */
				ErlDrvSizeT len)    /* length of binary */
{
    SysIOVec vec;

    vec.iov_base = bin->orig_bytes+offs;
    vec.iov_len = len;
    deliver_vec_message(prt, to, hbuf, hlen, &bin, &vec, 1, len);
}

/* flush the port I/O queue and terminate if empty */
/* 
 * Note. 
 *
 * The test for ERTS_PORT_SFLGS_DEAD is important since the
 * driver's  flush function might call driver_async, which when using no
 * threads and being short circuited will notice that the io queue is empty
 * (after calling the driver's async_ready) and recursively call 
 * terminate_port. So when we get back here, the port is already terminated.
 */
static void flush_port(Port *p)
{
    int fpe_was_unmasked;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));

    if (p->drv_ptr->flush != NULL) {
        ERTS_MSACC_PUSH_STATE_M();
#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(driver_flush)) {
            DTRACE_FORMAT_COMMON_PID_AND_PORT(ERTS_PORT_GET_CONNECTED(p), p)
            DTRACE3(driver_flush, process_str, port_str, p->name);
        }
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
        if (LTTNG_ENABLED(driver_flush)) {
            lttng_decl_portbuf(port_str);
            lttng_decl_procbuf(proc_str);
            lttng_pid_to_str(ERTS_PORT_GET_CONNECTED(p), proc_str);
            lttng_port_to_str(p, port_str);
            LTTNG3(driver_flush, proc_str, port_str, p->name);
        }
#endif


        if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(p, am_in, am_flush);
	}
        ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);
	fpe_was_unmasked = erts_block_fpe();
	(*p->drv_ptr->flush)((ErlDrvData)p->drv_data);
	erts_unblock_fpe(fpe_was_unmasked);
	ERTS_MSACC_POP_STATE_M();
        if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(p, am_out, am_flush);
	}
#ifdef ERTS_SMP
	if (p->xports)
	    erts_port_handle_xports(p);
	ASSERT(!p->xports);
#endif
    }
    if ((erts_atomic32_read_nob(&p->state) & ERTS_PORT_SFLGS_DEAD) == 0
	&& is_port_ioq_empty(p)) {
	terminate_port(p);
    }
}

/* stop and delete a port that is ERTS_PORT_SFLG_CLOSING */
static void
terminate_port(Port *prt)
{
    Eterm send_closed_port_id;
    Eterm connected_id = NIL /* Initialize to silence compiler */;
    erts_driver_t *drv;
    erts_aint32_t state;
    ErtsPrtSD *psd;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    ASSERT(!ERTS_P_LINKS(prt));
    ASSERT(!ERTS_P_MONITORS(prt));

    /* state may be altered by kill_port() below */
    state = erts_atomic32_read_band_nob(&prt->state,
					~ERTS_PORT_SFLG_SEND_CLOSED);
    if (state & ERTS_PORT_SFLG_SEND_CLOSED) {
	send_closed_port_id = prt->common.id;
	connected_id = ERTS_PORT_GET_CONNECTED(prt);
    }
    else {
	send_closed_port_id = NIL;
    }

    if (ERTS_PTMR_IS_SET(prt))
	erts_cancel_port_timer(prt);

    drv = prt->drv_ptr;
    if ((drv != NULL) && (drv->stop != NULL)) {
	int fpe_was_unmasked = erts_block_fpe();
	ERTS_MSACC_PUSH_AND_SET_STATE_M(ERTS_MSACC_STATE_PORT);
#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(driver_stop)) {
            DTRACE_FORMAT_COMMON_PID_AND_PORT(connected_id, prt)
            DTRACE3(driver_stop, process_str, drv->name, port_str);
        }
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
        if (LTTNG_ENABLED(driver_stop)) {
            lttng_decl_portbuf(port_str);
            lttng_decl_procbuf(proc_str);
            lttng_pid_to_str(connected_id, proc_str);
            lttng_port_to_str(prt, port_str);
            LTTNG3(driver_stop, proc_str, port_str, drv->name);
        }
#endif

	(*drv->stop)((ErlDrvData)prt->drv_data);
	erts_unblock_fpe(fpe_was_unmasked);
	ERTS_MSACC_POP_STATE_M();
#ifdef ERTS_SMP
	if (prt->xports)
	    erts_port_handle_xports(prt);
	ASSERT(!prt->xports);
#endif
    }

    if (is_internal_port(send_closed_port_id)
        && IS_TRACED_FL(prt, F_TRACE_SEND))
        trace_port_send(prt, connected_id, am_closed, 1);

    if(drv->handle != NULL) {
	erts_smp_rwmtx_rlock(&erts_driver_list_lock);
	erts_ddll_decrement_port_count(drv->handle); 
	erts_smp_rwmtx_runlock(&erts_driver_list_lock);
    }
    stopq(prt);        /* clear queue memory */
    if(prt->linebuf != NULL){
	erts_free(ERTS_ALC_T_LINEBUF, (void *) prt->linebuf);
	prt->linebuf = NULL;
    }

    erts_cleanup_port_data(prt);

    psd = (ErtsPrtSD *) erts_smp_atomic_read_nob(&prt->psd);
    if (psd)
	erts_free(ERTS_ALC_T_PRTSD, psd);

    ASSERT(prt->dist_entry == NULL);

    kill_port(prt);

    /*
     * We don't want to send the closed message until after the
     * port has been removed from the port table (in kill_port()).
     */
    if ((state & ERTS_PORT_SFLG_HALT)
	&& (erts_smp_atomic32_dec_read_nob(&erts_halt_progress) == 0)) {
	erts_port_release(prt); /* We will exit and never return */
	erts_flush_async_exit(erts_halt_code, "");
    }
    if (is_internal_port(send_closed_port_id))
	deliver_result(NULL, send_closed_port_id, connected_id, am_closed);
}

void
erts_terminate_port(Port *pp)
{
    terminate_port(pp);
}

static void port_fire_one_monitor(ErtsMonitor *mon, void *ctx0);
static void sweep_one_monitor(ErtsMonitor *mon, void *vpsc)
{
    switch (mon->type) {
    case MON_ORIGIN: {
        ErtsMonitor *rmon;
        Process *rp;

        ASSERT(is_internal_pid(mon->pid));
        rp = erts_pid2proc(NULL, 0, mon->pid, ERTS_PROC_LOCK_LINK);
        if (!rp) {
            goto done;
        }
        rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), mon->ref);
        erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
        if (rmon == NULL) {
            goto done;
        }
        erts_destroy_monitor(rmon);
    } break;
    case MON_TARGET: {
        port_fire_one_monitor(mon, vpsc); /* forward call */
    } break;
    }
 done:
    erts_destroy_monitor(mon);
}



typedef struct {
    Port *port;
    Eterm reason;
} SweepContext;

static void sweep_one_link(ErtsLink *lnk, void *vpsc)
{
    SweepContext *psc = vpsc;
    DistEntry *dep;
    Process *rp;
    Eterm port_id = psc->port->common.id;

    ASSERT(lnk->type == LINK_PID);

    if (IS_TRACED_FL(psc->port, F_TRACE_PORTS))
        trace_port(psc->port, am_unlink, lnk->pid);

    if (is_external_pid(lnk->pid)) {
	dep = external_pid_dist_entry(lnk->pid);
	if(dep != erts_this_dist_entry) {
	    ErtsDistLinkData dld;
	    ErtsDSigData dsd;
	    int code;
	    code = erts_dsig_prepare(&dsd, dep, NULL, ERTS_DSP_NO_LOCK, 0);
	    switch (code) {
	    case ERTS_DSIG_PREP_NOT_ALIVE:
	    case ERTS_DSIG_PREP_NOT_CONNECTED:
		break;
	    case ERTS_DSIG_PREP_CONNECTED:
		erts_remove_dist_link(&dld, port_id, lnk->pid, dep);
		erts_destroy_dist_link(&dld);
		code = erts_dsig_send_exit(&dsd, port_id, lnk->pid,
					   psc->reason);
		ASSERT(code == ERTS_DSIG_SEND_OK);
		break;
	    default:
		ASSERT(! "Invalid dsig prepare result");
		break;
	    }
	}
    } else {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
	ASSERT(is_internal_pid(lnk->pid));
	rp = erts_pid2proc(NULL, 0, lnk->pid, rp_locks);
	if (rp) {
	    ErtsLink *rlnk = erts_remove_link(&ERTS_P_LINKS(rp), port_id);

	    if (rlnk) {
		int xres = erts_send_exit_signal(NULL,
						 port_id,
						 rp,
						 &rp_locks, 
						 psc->reason,
						 NIL,
						 NULL,
						 0);
		if (xres >= 0) {
                    if (rp_locks & ERTS_PROC_LOCKS_XSIG_SEND) {
                        erts_smp_proc_unlock(rp, ERTS_PROC_LOCKS_XSIG_SEND);
                        rp_locks &= ~ERTS_PROC_LOCKS_XSIG_SEND;
                    }
		    /* We didn't exit the process and it is traced */
		    if (IS_TRACED_FL(rp, F_TRACE_PROCS))
			trace_proc(NULL, 0, rp, am_getting_unlinked, port_id);
		}
		erts_destroy_link(rlnk);
	    }

	    erts_smp_proc_unlock(rp, rp_locks);
	}
    }
    erts_destroy_link(lnk);
}

static void
port_fire_one_monitor(ErtsMonitor *mon, void *ctx0)
{
    Process      *origin;
    ErtsProcLocks origin_locks;

    if (mon->type != MON_TARGET || ! is_pid(mon->pid)) {
        return;
    }
    /*
     * Proceed here if someone monitors us, we (port) are the target and
     * origin is some process
     */
    origin_locks = ERTS_PROC_LOCKS_MSG_SEND | ERTS_PROC_LOCK_LINK;

    origin = erts_pid2proc(NULL, 0, mon->pid, origin_locks);
    if (origin) {
        DeclareTmpHeapNoproc(lhp,3);
        SweepContext    *ctx = (SweepContext *)ctx0;
        ErtsMonitor     *rmon;
        Eterm           watched = (is_atom(mon->name)
                       ? TUPLE2(lhp, mon->name, erts_this_dist_entry->sysname)
                       : ctx->port->common.id);

        erts_queue_monitor_message(origin, &origin_locks, mon->ref, am_port,
                                   watched, ctx->reason);
        UnUseTmpHeapNoproc(3);

        rmon = erts_remove_monitor(&ERTS_P_MONITORS(origin), mon->ref);
        erts_smp_proc_unlock(origin, origin_locks);

        if (rmon) {
            erts_destroy_monitor(rmon);
        }
    }
}

/* 'from' is sending 'this_port' an exit signal, (this_port must be internal).
 * If reason is normal we don't do anything, *unless* from is our connected
 * process in which case we close the port. Any other reason kills the port.
 * If 'from' is ourself we always die.
 * When a driver has data in ioq then driver will be set to closing
 * and become inaccessible to the processes. One exception exists and
 * that is to kill a port till reason kill. Then the port is stopped.
 * 
 */

int
erts_deliver_port_exit(Port *prt, Eterm from, Eterm reason, int send_closed,
                       int drop_normal)
{
   ErtsLink *lnk;
   Eterm modified_reason;
   erts_aint32_t state, set_state_flags;

   ERTS_SMP_CHK_NO_PROC_LOCKS;
   ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

   modified_reason = (reason == am_kill) ? am_killed : reason;

#ifdef USE_VM_PROBES
   if (DTRACE_ENABLED(port_exit)) {
       DTRACE_CHARBUF(from_str, DTRACE_TERM_BUF_SIZE);
       DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);
       DTRACE_CHARBUF(reason_str, 64);

       erts_snprintf(from_str, sizeof(DTRACE_CHARBUF_NAME(from_str)), "%T", from);
       dtrace_port_str(prt, port_str);
       erts_snprintf(reason_str, sizeof(DTRACE_CHARBUF_NAME(reason_str)), "%T",
                     modified_reason);
       DTRACE4(port_exit, from_str, port_str, prt->name, reason_str);
   }
#endif

   state = erts_atomic32_read_nob(&prt->state);
   if (state & (ERTS_PORT_SFLGS_DEAD
		| ERTS_PORT_SFLG_EXITING
		| ERTS_PORT_SFLG_CLOSING))
       return 0;

   if (reason == am_normal && from != ERTS_PORT_GET_CONNECTED(prt)
       && from != prt->common.id && drop_normal) {
       return 0;
   }

   set_state_flags = ERTS_PORT_SFLG_EXITING;
   if (send_closed)
       set_state_flags |= ERTS_PORT_SFLG_SEND_CLOSED;

   erts_port_task_sched_enter_exiting_state(&prt->sched);
   
   state = erts_atomic32_read_bor_mb(&prt->state, set_state_flags);
   state |= set_state_flags;

   if (IS_TRACED_FL(prt, F_TRACE_PORTS))
        trace_port(prt, am_closed, reason);

   erts_trace_check_exiting(prt->common.id);

   set_busy_port(ERTS_Port2ErlDrvPort(prt), 0);

   if (prt->common.u.alive.reg != NULL)
       (void) erts_unregister_name(NULL, 0, prt, prt->common.u.alive.reg->name);

   {
       SweepContext sc = {prt, modified_reason};
       lnk = ERTS_P_LINKS(prt);
       ERTS_P_LINKS(prt) = NULL;
       erts_sweep_links(lnk, &sweep_one_link, &sc);
   }
   DRV_MONITOR_LOCK_PDL(prt);
   {
       SweepContext ctx = {prt, modified_reason};
       ErtsMonitor *moni = ERTS_P_MONITORS(prt);
       ERTS_P_MONITORS(prt) = NULL;
       erts_sweep_monitors(moni, &sweep_one_monitor, &ctx);
   } 
   DRV_MONITOR_UNLOCK_PDL(prt);

   if ((state & ERTS_PORT_SFLG_DISTRIBUTION) && prt->dist_entry) {
       erts_do_net_exits(prt->dist_entry, modified_reason);
       erts_deref_dist_entry(prt->dist_entry);
       prt->dist_entry = NULL;
       erts_atomic32_read_band_relb(&prt->state,
				    ~ERTS_PORT_SFLG_DISTRIBUTION);
   }
       
   if ((reason != am_kill) && !is_port_ioq_empty(prt)) {
       /* must turn exiting flag off */
       erts_atomic32_read_bset_relb(&prt->state,
				    (ERTS_PORT_SFLG_EXITING
				     | ERTS_PORT_SFLG_CLOSING),
				    ERTS_PORT_SFLG_CLOSING);
      flush_port(prt);
   }
   else {
       terminate_port(prt);
   }

   return 1;
}

/* About the states ERTS_PORT_SFLG_EXITING and ERTS_PORT_SFLG_CLOSING used above.
**
** ERTS_PORT_SFLG_EXITING is a recursion protection for erts_deliver_port_exit().
** It is unclear  whether this state is necessary or not, it might be possible
** to merge it with ERTS_PORT_SFLG_CLOSING. ERTS_PORT_SFLG_EXITING only persists
** over a section of sequential (but highly recursive) code.
**
** ERTS_PORT_SFLG_CLOSING is a state where the port is in Limbo, waiting to
** pass on. All links are removed, and the port receives in/out-put events so
** as soon as the port queue gets empty terminate_port() is called.
*/



/* Command should be of the form
**   {PID, close}
**   {PID, {command, io-list}}
**   {PID, {connect, New_PID}}
*/
ErtsPortOpResult
erts_port_command(Process *c_p,
		  int flags,
		  Port *port,
		  Eterm command,
		  Eterm *refp)
{
    Eterm *tp;

    ASSERT(port);

    flags |= ERTS_PORT_SIG_FLG_BANG_OP;
    if (!erts_port_synchronous_ops) {
	flags |= ERTS_PORT_SIG_FLG_ASYNC;
	refp = NULL;
    }

    if (is_tuple_arity(command, 2)) {
	Eterm cntd;
	tp = tuple_val(command);
	cntd = tp[1];
	if (is_internal_pid(cntd)) {
	    if (tp[2] == am_close) {
		flags &= ~ERTS_PORT_SIG_FLG_NOSUSPEND;
		return erts_port_exit(c_p, flags, port, cntd, am_normal, refp);
	    } else if (is_tuple_arity(tp[2], 2)) {
		tp = tuple_val(tp[2]);
		if (tp[1] == am_command) {
		    return erts_port_output(c_p, flags, port, cntd, tp[2], refp);
		}
		else if (tp[1] == am_connect) {
		    flags &= ~ERTS_PORT_SIG_FLG_NOSUSPEND;
		    return erts_port_connect(c_p, flags, port, cntd, tp[2], refp);
		}
	    }
	}
    }

    /* badsig */
    flags &= ~ERTS_PORT_SIG_FLG_NOSUSPEND;
    return bad_port_signal(c_p, flags, port, c_p->common.id, refp, am_command);
}

static ERTS_INLINE ErtsPortOpResult
call_driver_control(Eterm caller,
		    Port *prt,
		    unsigned int command,
		    char *bufp,
		    ErlDrvSizeT size,
		    char **resp_bufp,
		    ErlDrvSizeT *from_size)
{
    ErlDrvSSizeT cres;
    ERTS_MSACC_PUSH_STATE_M();

    if (!prt->drv_ptr->control)
	return ERTS_PORT_OP_BADARG;


#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(port_control) || DTRACE_ENABLED(driver_control)) {
        DTRACE_FORMAT_COMMON_PID_AND_PORT(caller, prt);
        DTRACE4(port_control, process_str, port_str, prt->name, command);
        DTRACE5(driver_control, process_str, port_str, prt->name,
                command, size);
    }
#endif

    if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
        trace_port_receive(prt, caller, am_control, command, bufp, size);

    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);

#ifdef USE_LTTNG_VM_TRACEPOINTS
    if (LTTNG_ENABLED(driver_control)) {
        lttng_decl_procbuf(proc_str);
        lttng_decl_portbuf(port_str);
        lttng_pid_to_str(caller, proc_str);
        lttng_port_to_str(prt, port_str);
        LTTNG5(driver_control, proc_str, port_str, prt->name, command, size);
    }
#endif

    prt->caller = caller;
    cres = prt->drv_ptr->control((ErlDrvData) prt->drv_data,
				 command,
				 bufp,
				 size,
				 resp_bufp,
				 *from_size);
    prt->caller = NIL;

    ERTS_MSACC_POP_STATE_M();

    if (cres < 0)
	return ERTS_PORT_OP_BADARG;

    if (IS_TRACED_FL(prt, F_TRACE_SEND))
        trace_port_send_binary(prt, caller, am_control, *resp_bufp, cres);

    *from_size = (ErlDrvSizeT) cres;

    return ERTS_PORT_OP_DONE;
}

static void
cleanup_scheduled_control(Binary *binp, char *bufp)
{
    if (binp) {
	if (erts_refc_dectest(&binp->refc, 0) == 0)
	    erts_bin_free(binp);
    }
    else {
	if (bufp)
	    erts_free(ERTS_ALC_T_DRV_CTRL_DATA, bufp);
    }
}


static ERTS_INLINE Uint
port_control_result_size(int control_flags,
			 char *resp_bufp,
			 ErlDrvSizeT *resp_size,
			 char *pre_alloc_buf)
{
    if (!resp_bufp)
	return (Uint) 0;

    if (control_flags & PORT_CONTROL_FLAG_BINARY) {
	if (resp_bufp != pre_alloc_buf) {
	    ErlDrvBinary *dbin = (ErlDrvBinary *) resp_bufp;
	    *resp_size = dbin->orig_size;
	    if (*resp_size > ERL_ONHEAP_BIN_LIMIT)
		return PROC_BIN_SIZE;
	}
	ASSERT(*resp_size <= ERL_ONHEAP_BIN_LIMIT);
	return (Uint) heap_bin_size((*resp_size));
    }

    return (Uint) 2*(*resp_size);
}

static ERTS_INLINE Eterm
write_port_control_result(int control_flags,
			  char *resp_bufp,
			  ErlDrvSizeT resp_size,
			  char *pre_alloc_buf,
			  Eterm **hpp,
			  ErlOffHeap *ohp)
{
    Eterm res;
    if (!resp_bufp)
	return NIL;
    if (control_flags & PORT_CONTROL_FLAG_BINARY) {
	/* Binary result */
	ErlDrvBinary *dbin;
	ErlHeapBin *hbin;

	if (resp_bufp == pre_alloc_buf)
	    dbin = NULL;
	else {
	    dbin = (ErlDrvBinary *) resp_bufp;
	    if (dbin->orig_size > ERL_ONHEAP_BIN_LIMIT) {
		ProcBin* pb = (ProcBin *) *hpp;
		*hpp += PROC_BIN_SIZE;
		pb->thing_word = HEADER_PROC_BIN;
		pb->size = dbin->orig_size;
		pb->next = ohp->first;
		ohp->first = (struct erl_off_heap_header *) pb;
		pb->val = ErlDrvBinary2Binary(dbin);
		pb->bytes = (byte*) dbin->orig_bytes;
		pb->flags = 0;
		OH_OVERHEAD(ohp, dbin->orig_size / sizeof(Eterm));
		return make_binary(pb);
	    }
	    resp_bufp = dbin->orig_bytes;
	    resp_size = dbin->orig_size;
	}

	hbin = (ErlHeapBin *) *hpp;
	*hpp += heap_bin_size(resp_size);
	ASSERT(resp_size <= ERL_ONHEAP_BIN_LIMIT);
	hbin->thing_word = header_heap_bin(resp_size);
	hbin->size = resp_size;
	sys_memcpy(hbin->data, resp_bufp, resp_size);
	if (dbin)
	    driver_free_binary(dbin);
	return make_binary(hbin);
    }

    /* List result */
    res = buf_to_intlist(hpp, resp_bufp, resp_size, NIL);
    if (resp_bufp != pre_alloc_buf)
	driver_free(resp_bufp);
    return res;
}

static int
port_sig_control(Port *prt,
		 erts_aint32_t state,
		 int op,
		 ErtsProc2PortSigData *sigdp)
{
    ASSERT(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY);

    if (op == ERTS_PROC2PORT_SIG_EXEC) {
	char resp_buf[ERL_ONHEAP_BIN_LIMIT];
	ErlDrvSizeT resp_size = sizeof(resp_buf);
	char *resp_bufp = &resp_buf[0];
	ErtsPortOpResult res;

	res = call_driver_control(sigdp->caller,
				  prt,
				  sigdp->u.control.command,
				  sigdp->u.control.bufp,
				  sigdp->u.control.size,
				  &resp_bufp,
				  &resp_size);

	if (res == ERTS_PORT_OP_DONE) {
	    Eterm msg;
            ErtsHeapFactory factory;
	    Process *rp;
	    ErtsProcLocks rp_locks = 0;
	    Uint hsz, rsz;
	    int control_flags;

	    rp = sigdp->caller == ERTS_INVALID_PID ? NULL : erts_proc_lookup_raw(sigdp->caller);
	    if (!rp)
		goto done;

	    control_flags = prt->control_flags;

	    rsz = port_control_result_size(control_flags,
					    resp_bufp,
					    &resp_size,
					    &resp_buf[0]);
	    hsz = rsz + ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE;


	    (void) erts_factory_message_create(&factory, rp,
					       &rp_locks, hsz);

	    msg = write_port_control_result(control_flags,
					    resp_bufp,
					    resp_size,
					    &resp_buf[0],
					    &factory.hp,
					    factory.off_heap);
	    queue_port_sched_op_reply(rp,
				      rp_locks,
                                      &factory,
				      sigdp->ref,
				      msg,
				      prt);

	    if (rp_locks)
		erts_smp_proc_unlock(rp, rp_locks);
	    goto done;
	}
    }

    /* failure */

    if (sigdp->caller != ERTS_INVALID_PID)
        port_sched_op_reply(sigdp->caller, sigdp->ref, am_badarg, prt);

done:

    cleanup_scheduled_control(sigdp->u.control.binp,
			      sigdp->u.control.bufp);

    return ERTS_PORT_REDS_CONTROL;
}

/*
 * This is an asynchronous control call. I.e. it will not return anything
 * to the caller.
 */
int
erl_drv_port_control(Eterm port_num, char cmd, char* buff, ErlDrvSizeT size)
{
    ErtsProc2PortSigData *sigdp = erts_port_task_alloc_p2p_sig_data();

    sigdp->flags = ERTS_P2P_SIG_TYPE_CONTROL | ERTS_P2P_SIG_DATA_FLG_REPLY;
    sigdp->u.control.binp = NULL;
    sigdp->u.control.command = cmd;
    sigdp->u.control.bufp = buff;
    sigdp->u.control.size = size;

    return erts_schedule_port2port_signal(port_num, sigdp, 0, port_sig_control);
}

ErtsPortOpResult
erts_port_control(Process* c_p,
		  Port *prt,
		  unsigned int command,
		  Eterm data,
		  Eterm *retvalp)
{
    ErtsPortOpResult res;
    char *bufp = NULL;
    ErlDrvSizeT size = 0;
    int try_call;
    int tmp_alloced = 0;
    erts_aint32_t sched_flags;
    Binary *binp;
    int copy;
    ErtsProc2PortSigData *sigdp;

    sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
    if (sched_flags & ERTS_PTS_FLG_EXIT)
	return ERTS_PORT_OP_BADARG;

    try_call = !(sched_flags & ERTS_PTS_FLGS_FORCE_SCHEDULE_OP);

    if (is_binary(data) && binary_bitoffset(data) == 0) {
	byte *bytep;
	ERTS_DECLARE_DUMMY(Uint bitoffs);
	ERTS_DECLARE_DUMMY(Uint bitsize);
	ERTS_GET_BINARY_BYTES(data, bytep, bitoffs, bitsize);
	bufp = (char *) bytep;
	size = binary_size(data);
    } else {
	int r;

	if (!try_call) {
	    if (erts_iolist_size(data, &size))
		return ERTS_PORT_OP_BADARG;
	    bufp = erts_alloc(ERTS_ALC_T_DRV_CTRL_DATA, size);
	    r = erts_iolist_to_buf(data, bufp, size);
	    ASSERT(r == 0);
	}
	else {
	    /* Try with an 8KB buffer first (will often be enough I guess). */
	    size = 8*1024;
	    bufp = erts_alloc(ERTS_ALC_T_TMP, size);
	    tmp_alloced = 1;

	    r = erts_iolist_to_buf(data, bufp, size);
	    if (ERTS_IOLIST_TO_BUF_SUCCEEDED(r)) {
		size -= r;
	    } else {
		if (r == ERTS_IOLIST_TO_BUF_TYPE_ERROR) { /* Type error */
		    erts_free(ERTS_ALC_T_TMP, bufp);
		    return ERTS_PORT_OP_BADARG;
		}
		else {
		    ASSERT(r == ERTS_IOLIST_TO_BUF_OVERFLOW); /* Overflow */
		    erts_free(ERTS_ALC_T_TMP, bufp);
		    if (erts_iolist_size(data, &size))
			return ERTS_PORT_OP_BADARG; /* Type error */
		}
		bufp = erts_alloc(ERTS_ALC_T_TMP, size);
		r = erts_iolist_to_buf(data, bufp, size);
		ASSERT(r == 0);
	    }
	}
    }

    if (try_call) {
	char resp_buf[ERL_ONHEAP_BIN_LIMIT];
	char* resp_bufp = &resp_buf[0];
	ErlDrvSizeT resp_size = sizeof(resp_buf);
	ErtsTryImmDrvCallResult try_call_res;
	ErtsTryImmDrvCallState try_call_state
	    = ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
		c_p,
		prt,
		ERTS_PORT_SFLGS_INVALID_LOOKUP,
		0,
		0,
		am_control);

	try_call_res = try_imm_drv_call(&try_call_state);
	switch (try_call_res) {
	case ERTS_TRY_IMM_DRV_CALL_OK: {
	    Eterm *hp;
	    Uint hsz;
	    int control_flags;

	    res = call_driver_control(c_p->common.id,
				      prt,
				      command,
				      bufp,
				      size,
				      &resp_bufp,
				      &resp_size);

	    control_flags = prt->control_flags;

	    finalize_imm_drv_call(&try_call_state);
	    if (tmp_alloced)
		erts_free(ERTS_ALC_T_TMP, bufp);
	    if (res == ERTS_PORT_OP_BADARG) {
		return ERTS_PORT_OP_BADARG;
	    }

	    hsz = port_control_result_size(control_flags,
					   resp_bufp,
					   &resp_size,
					   &resp_buf[0]);
	    hp = HAlloc(c_p, hsz);
	    *retvalp = write_port_control_result(control_flags,
						 resp_bufp,
						 resp_size,
						 &resp_buf[0],
						 &hp,
						 &c_p->off_heap);
	    BUMP_REDS(c_p, ERTS_PORT_REDS_CONTROL);
	    return ERTS_PORT_OP_DONE;
	}
	case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	    if (tmp_alloced)
		erts_free(ERTS_ALC_T_TMP, bufp);
	    return ERTS_PORT_OP_BADARG;
	default:
	    /* Schedule control() call instead... */
	    break;
	}
    }

    /* Convert data into something that can be scheduled */

    copy = tmp_alloced;

    binp = NULL;

    if (is_binary(data) && binary_bitoffset(data) == 0) {
	Eterm *ebinp = binary_val(data);
	ASSERT(!tmp_alloced);
	if (*ebinp == HEADER_SUB_BIN)
	    ebinp = binary_val(((ErlSubBin *) ebinp)->orig);
	if (*ebinp != HEADER_PROC_BIN)
	    copy = 1;
	else {
	    binp = ((ProcBin *) ebinp)->val;
	    ASSERT(bufp <= bufp + size);
	    ASSERT(binp->orig_bytes <= bufp
		   && bufp + size <= binp->orig_bytes + binp->orig_size);
	    erts_refc_inc(&binp->refc, 1);
	}
    }

    if (copy) {
	char *old_bufp = bufp;
	bufp = erts_alloc(ERTS_ALC_T_DRV_CTRL_DATA, size);
	sys_memcpy(bufp, old_bufp, size);
	if (tmp_alloced)
	    erts_free(ERTS_ALC_T_TMP, old_bufp);
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_CONTROL;
    sigdp->u.control.binp = binp;
    sigdp->u.control.command = command;
    sigdp->u.control.bufp = bufp;
    sigdp->u.control.size = size;
    
    res = erts_schedule_proc2port_signal(c_p,
					 prt,
					 c_p->common.id,
					 retvalp,
					 sigdp,
					 0,
					 NULL,
					 port_sig_control);
    if (res != ERTS_PORT_OP_SCHEDULED) {
	cleanup_scheduled_control(binp, bufp);
	return ERTS_PORT_OP_BADARG;
    }
    return res;
}

static ERTS_INLINE ErtsPortOpResult
call_driver_call(Eterm caller,
		 Port *prt,
		 unsigned int command,
		 char *bufp,
		 ErlDrvSizeT size,
		 char **resp_bufp,
		 ErlDrvSizeT *from_size,
		 unsigned *ret_flagsp)
{
    ErlDrvSSizeT cres;
    ERTS_MSACC_PUSH_STATE_M();

    if (!prt->drv_ptr->call)
	return ERTS_PORT_OP_BADARG;

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(driver_call)) {
        DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);

        dtrace_pid_str(caller, process_str);
        dtrace_port_str(prt, port_str);
        DTRACE5(driver_call, process_str, port_str, prt->name, command, size);
    }
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
    if (LTTNG_ENABLED(driver_call)) {
        lttng_decl_procbuf(proc_str);
        lttng_decl_portbuf(port_str);
        lttng_pid_to_str(caller,proc_str);
        lttng_port_to_str(prt, port_str);
        LTTNG5(driver_call, proc_str, port_str, prt->name, command, size);
    }
#endif

    if (IS_TRACED_FL(prt, F_TRACE_RECEIVE))
        trace_port_receive(prt, caller, am_call, command, bufp, size);

    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);

    prt->caller = caller;
    cres = prt->drv_ptr->call((ErlDrvData) prt->drv_data,
			      command,
			      bufp,
			      size,
			      resp_bufp,
			      *from_size,
			      ret_flagsp);
    prt->caller = NIL;

    ERTS_MSACC_POP_STATE_M();

    if (cres <= 0
	|| ((byte) (*resp_bufp)[0]) != VERSION_MAGIC)
	return ERTS_PORT_OP_BADARG;

    if (IS_TRACED_FL(prt, F_TRACE_SEND))
        trace_port_send_binary(prt, caller, am_call, *resp_bufp, cres);

    *from_size = (ErlDrvSizeT) cres;

    return ERTS_PORT_OP_DONE;
}


static
void cleanup_scheduled_call(char *bufp)
{
    if (bufp)
	erts_free(ERTS_ALC_T_DRV_CALL_DATA, bufp);
}

static int
port_sig_call(Port *prt,
	      erts_aint32_t state,
	      int op,
	      ErtsProc2PortSigData *sigdp)
{
    char resp_buf[256];
    ErlDrvSizeT resp_size = sizeof(resp_buf);
    char *resp_bufp = &resp_buf[0];
    unsigned ret_flags = 0U;


    ASSERT(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY);

    if (op == ERTS_PROC2PORT_SIG_EXEC) {
	ErtsPortOpResult res;

	res = call_driver_call(sigdp->caller,
			       prt,
			       sigdp->u.call.command,
			       sigdp->u.call.bufp,
			       sigdp->u.call.size,
			       &resp_bufp,
			       &resp_size,
			       &ret_flags);

	if (res == ERTS_PORT_OP_DONE) {
	    Eterm msg;
	    Eterm *hp;
	    Process *rp;
	    ErtsProcLocks rp_locks = 0;
	    Sint hsz;

	    rp = erts_proc_lookup_raw(sigdp->caller);
	    if (!rp)
		goto done;

	    hsz = erts_decode_ext_size((byte *) resp_bufp, resp_size);
	    if (hsz >= 0) {
                ErtsHeapFactory factory;
		byte *endp;

		hsz += 3; /* ok tuple */
		hsz += ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE;

                (void) erts_factory_message_create(&factory, rp, &rp_locks, hsz);
		endp = (byte *) resp_bufp;
		msg = erts_decode_ext(&factory, &endp, 0);
		if (is_value(msg)) {
                    hp = erts_produce_heap(&factory,
                                           3,
                                           ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE);
		    msg = TUPLE2(hp, am_ok, msg);

		    queue_port_sched_op_reply(rp,
					      rp_locks,
                                              &factory,
					      sigdp->ref,
					      msg,
					      prt);

		    if (rp_locks)
			erts_smp_proc_unlock(rp, rp_locks);
		    goto done;
		}
		if (rp_locks)
		    erts_smp_proc_unlock(rp, rp_locks);
	    }
	}
    }

    port_sched_op_reply(sigdp->caller, sigdp->ref, am_badarg, prt);

done:

    if (resp_bufp != &resp_buf[0] && !(ret_flags & DRIVER_CALL_KEEP_BUFFER))
	driver_free(resp_bufp);

    cleanup_scheduled_call(sigdp->u.call.bufp);

    return ERTS_PORT_REDS_CALL;
}


ErtsPortOpResult
erts_port_call(Process* c_p,
	       Port *prt,
	       unsigned int command,
	       Eterm data,
	       Eterm *retvalp)
{
    ErtsPortOpResult res;
    char input_buf[256];
    char *bufp;
    byte *endp;
    ErlDrvSizeT size;
    int try_call;
    erts_aint32_t sched_flags;
    ErtsProc2PortSigData *sigdp;

    sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
    if (sched_flags & ERTS_PTS_FLG_EXIT) {
	return ERTS_PORT_OP_BADARG;
    }

    try_call = !(sched_flags & ERTS_PTS_FLGS_FORCE_SCHEDULE_OP);

    size = erts_encode_ext_size(data);

    if (!try_call)
	bufp = erts_alloc(ERTS_ALC_T_DRV_CALL_DATA, size);
    else if (size <= sizeof(input_buf))
	bufp = &input_buf[0];
    else
	bufp = erts_alloc(ERTS_ALC_T_TMP, size);

    endp = (byte *) bufp;
    erts_encode_ext(data, &endp);

    if (endp - (byte *) bufp > size)
	ERTS_INTERNAL_ERROR("erts_internal:port_call() - Buffer overflow");

    size = endp - (byte *) bufp;

    if (try_call) {
	char resp_buf[255];
	char* resp_bufp = &resp_buf[0];
	ErlDrvSizeT resp_size = sizeof(resp_buf);
	ErtsTryImmDrvCallResult try_call_res;
	ErtsTryImmDrvCallState try_call_state
	    = ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
		c_p,
		prt,
		ERTS_PORT_SFLGS_INVALID_LOOKUP,
		0,
		0,
		am_call);

	try_call_res = try_imm_drv_call(&try_call_state);
	switch (try_call_res) {
	case ERTS_TRY_IMM_DRV_CALL_OK: {
            ErtsHeapFactory factory;
	    Sint hsz;
	    unsigned ret_flags = 0U;
	    Eterm term;
            Eterm* hp;

	    res = call_driver_call(c_p->common.id,
				   prt,
				   command,
				   bufp,
				   size,
				   &resp_bufp,
				   &resp_size,
				   &ret_flags);

	    finalize_imm_drv_call(&try_call_state);
	    if (bufp != &input_buf[0])
		erts_free(ERTS_ALC_T_TMP, bufp);
	    if (res == ERTS_PORT_OP_BADARG)
		return ERTS_PORT_OP_BADARG;
	    hsz = erts_decode_ext_size((byte *) resp_bufp, resp_size);
	    if (hsz < 0)
		return ERTS_PORT_OP_BADARG;
	    hsz += 3;
            erts_factory_proc_prealloc_init(&factory, c_p, hsz);
	    endp = (byte *) resp_bufp;
	    term = erts_decode_ext(&factory, &endp, 0);
	    if (term == THE_NON_VALUE)
		return ERTS_PORT_OP_BADARG;
            hp = erts_produce_heap(&factory,3,0);
	    *retvalp = TUPLE2(hp, am_ok, term);
            erts_factory_close(&factory);
	    if (resp_bufp != &resp_buf[0]
		&& !(ret_flags & DRIVER_CALL_KEEP_BUFFER))
		driver_free(resp_bufp);
	    BUMP_REDS(c_p, ERTS_PORT_REDS_CALL);
	    return ERTS_PORT_OP_DONE;
	}
	case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	    if (bufp != &input_buf[0])
		erts_free(ERTS_ALC_T_TMP, bufp);
	    return ERTS_PORT_OP_BADARG;
	default:
	    /* Schedule call() call instead... */
	    break;
	}
    }

    /* Convert data into something that can be scheduled */

    if (bufp == &input_buf[0] || try_call) {
	char *new_bufp = erts_alloc(ERTS_ALC_T_DRV_CALL_DATA, size);
	sys_memcpy(new_bufp, bufp, size);
	if (bufp != &input_buf[0])
	    erts_free(ERTS_ALC_T_TMP, bufp);
	bufp = new_bufp;
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_CALL;
    sigdp->u.call.command = command;
    sigdp->u.call.bufp = bufp;
    sigdp->u.call.size = size;
    
    res = erts_schedule_proc2port_signal(c_p,
					 prt,
					 c_p->common.id,
					 retvalp,
					 sigdp,
					 0,
					 NULL,
					 port_sig_call);
    if (res != ERTS_PORT_OP_SCHEDULED) {
	cleanup_scheduled_call(bufp);
	return ERTS_PORT_OP_BADARG;
    }
    return res;
}

static Eterm
make_port_info_term(Eterm **hpp_start,
		    Eterm **hpp,
		    Uint *hszp,
		    ErlHeapFragment **bpp,
		    Port *prt,
		    Eterm item)
{
    ErlOffHeap *ohp;

    if (is_value(item)) {
	if (erts_bld_port_info(NULL, NULL, hszp, prt, item) == am_false)
	    return THE_NON_VALUE;
	if (*hszp) {
	    *bpp = new_message_buffer(*hszp);
	    *hpp_start = *hpp = (*bpp)->mem;
	    ohp = &(*bpp)->off_heap;
	}
	else {
	    *bpp = NULL;
	    *hpp_start = *hpp = NULL;
	    ohp = NULL;
	}
	return erts_bld_port_info(hpp, ohp, NULL, prt, item);
    }
    else {
	int i;
	int len;
	int start;
	static Eterm item[] = ERTS_PORT_INFO_1_ITEMS;
        Eterm value[sizeof(item)/sizeof(item[0])];

	start = 0;
	len = sizeof(item)/sizeof(item[0]);

	for (i = start; i < sizeof(item)/sizeof(item[0]); i++) {
	    ASSERT(is_atom(item[i]));
	    value[i] = erts_bld_port_info(NULL, NULL, hszp, prt, item[i]);
	}

	if (value[0] == am_undefined) {
	    start++;
	    len--;
	}

	erts_bld_list(NULL, hszp, len, &value[start]);

	*bpp = new_message_buffer(*hszp);
	*hpp_start = *hpp = (*bpp)->mem;
	ohp = &(*bpp)->off_heap;

	for (i = start; i < sizeof(item)/sizeof(item[0]); i++)
	    value[i] = erts_bld_port_info(hpp, ohp, NULL, prt, item[i]);

	return erts_bld_list(hpp, NULL, len, &value[start]);
    }
}

static int
port_sig_info(Port *prt,
	      erts_aint32_t state,
	      int op,
	      ErtsProc2PortSigData *sigdp)
{
    ASSERT(sigdp->flags & ERTS_P2P_SIG_DATA_FLG_REPLY);
    if (op != ERTS_PROC2PORT_SIG_EXEC)
	port_sched_op_reply(sigdp->caller, sigdp->ref, am_undefined, prt);
    else {
	Eterm *hp, *hp_start;
	Uint hsz;
	ErlHeapFragment *bp;
	Eterm value;
	Process *rp;
	ErtsProcLocks rp_locks = 0;

	rp = erts_proc_lookup_raw(sigdp->caller);
	if (!rp)
	    return ERTS_PORT_REDS_INFO;

	hsz = ERTS_QUEUE_PORT_SCHED_OP_REPLY_SIZE;
	value = make_port_info_term(&hp_start,
				    &hp,
				    &hsz,
				    &bp,
				    prt,
				    sigdp->u.info.item);
	if (is_value(value)) {
            ErtsHeapFactory factory;
	    ErtsMessage *mp = erts_alloc_message(0, NULL);
	    mp->data.heap_frag = bp;
            erts_factory_selfcontained_message_init(&factory, mp, hp);
	    queue_port_sched_op_reply(rp,
				      rp_locks,
                                      &factory,
				      sigdp->ref,
				      value,
				      prt);
	}
	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
    }
    return ERTS_PORT_REDS_INFO;
}

ErtsPortOpResult
erts_port_info(Process* c_p,
	       Port *prt,
	       Eterm item,
	       Eterm *retvalp)
{
    ErtsProc2PortSigData *sigdp;
    ErtsTryImmDrvCallResult try_call_res;
    ErtsTryImmDrvCallState try_call_state
	= ERTS_INIT_TRY_IMM_DRV_CALL_STATE(
	    c_p,
	    prt,
	    ERTS_PORT_SFLGS_INVALID_LOOKUP,
	    0,
	    0,
	    am_info);

    try_call_res = try_imm_drv_call(&try_call_state);
    switch (try_call_res) {
    case ERTS_TRY_IMM_DRV_CALL_OK: {
	Eterm *hp, *hp_start;
	ErlHeapFragment *bp;
	Uint hsz = 0;
	Eterm value = make_port_info_term(&hp_start, &hp, &hsz, &bp, prt, item);
	finalize_imm_drv_call(&try_call_state);
	if (is_non_value(value))
	    return ERTS_PORT_OP_BADARG;
	else if (is_immed(value))
	    *retvalp = value;
	else {
	    Uint used_h_size = hp - hp_start;
	    hp = HAlloc(c_p, used_h_size);
	    *retvalp = copy_struct(value, used_h_size, &hp, &MSO(c_p));
	    free_message_buffer(bp);
	}
	BUMP_REDS(c_p, ERTS_PORT_REDS_INFO);
	return ERTS_PORT_OP_DONE;
    }
    case ERTS_TRY_IMM_DRV_CALL_INVALID_PORT:
	return ERTS_PORT_OP_DROPPED;
    case ERTS_TRY_IMM_DRV_CALL_INVALID_SCHED_FLAGS:
    case ERTS_TRY_IMM_DRV_CALL_BUSY_LOCK:
	/* Schedule call instead... */
	break;
    }

    sigdp = erts_port_task_alloc_p2p_sig_data();
    sigdp->flags = ERTS_P2P_SIG_TYPE_INFO;
    sigdp->u.info.item = item;

    return erts_schedule_proc2port_signal(c_p,
					  prt,
					  c_p->common.id,
					  retvalp,
					  sigdp,
					  0,
					  NULL,
					  port_sig_info);
}

typedef struct {
    Uint sched_id;
    Eterm pid;
    Uint32 refn[ERTS_REF_NUMBERS];
    erts_smp_atomic32_t refc;
} ErtsIOBytesReq;

static void
reply_io_bytes(void *vreq)
{
    ErtsIOBytesReq *req = (ErtsIOBytesReq *) vreq;
    Process *rp;

    rp = erts_proc_lookup(req->pid);
    if (rp) {
	ErlOffHeap *ohp;
	ErtsMessage *mp;
	ErtsProcLocks rp_locks;
	Eterm ref, msg, ein, eout, *hp;
	Uint64 in, out;
	Uint hsz;
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
	Uint sched_id = esdp->no;
	in = esdp->io.in;
	out = esdp->io.out;
	if (req->sched_id != sched_id)
	    rp_locks = 0;
	else {
	    in += (Uint64) erts_atomic64_read_nob(&bytes_in);
	    out += (Uint64) erts_atomic64_read_nob(&bytes_out);
	    rp_locks = ERTS_PROC_LOCK_MAIN;
	}

	hsz = 5 /* 4-tuple */ + REF_THING_SIZE;

	erts_bld_uint64(NULL, &hsz, in);
	erts_bld_uint64(NULL, &hsz, out);

	mp = erts_alloc_message_heap(rp, &rp_locks, hsz, &hp, &ohp);

	ref = make_internal_ref(hp);
	write_ref_thing(hp, req->refn[0], req->refn[1], req->refn[2]);
	hp += REF_THING_SIZE;

	ein = erts_bld_uint64(&hp, NULL, in);
	eout = erts_bld_uint64(&hp, NULL, out);

	msg = TUPLE4(hp, ref, make_small(sched_id), ein, eout);

	erts_queue_message(rp, rp_locks, mp, msg, am_system);

	if (req->sched_id == sched_id)
	    rp_locks &= ~ERTS_PROC_LOCK_MAIN;
	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
    }

    if (erts_smp_atomic32_dec_read_nob(&req->refc) == 0)
	erts_free(ERTS_ALC_T_IOB_REQ, req);
}

Eterm
erts_request_io_bytes(Process *c_p)
{
    Uint *hp;
    Eterm ref;
    Uint32 *refn;
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    ErtsIOBytesReq *req = erts_alloc(ERTS_ALC_T_IOB_REQ,
				     sizeof(ErtsIOBytesReq));

    hp = HAlloc(c_p, REF_THING_SIZE);
    ref = erts_sched_make_ref_in_buffer(esdp, hp);
    refn = internal_ref_numbers(ref);

    req->sched_id = esdp->no;
    req->pid = c_p->common.id;
    req->refn[0] = refn[0];
    req->refn[1] = refn[1];
    req->refn[2] = refn[2];
    erts_smp_atomic32_init_nob(&req->refc,
			       (erts_aint32_t) erts_no_schedulers);

#ifdef ERTS_SMP
    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
					  reply_io_bytes,
					  (void *) req);
#endif

    reply_io_bytes((void *) req);

    return ref;
}


typedef struct {
    int to;
    void *arg;
} prt_one_lnk_data;

static void prt_one_monitor(ErtsMonitor *mon, void *vprtd)
{
    prt_one_lnk_data *prtd = (prt_one_lnk_data *) vprtd;
    erts_print(prtd->to, prtd->arg, "(%T,%T)", mon->pid,mon->ref);
}

static void prt_one_lnk(ErtsLink *lnk, void *vprtd)
{
    prt_one_lnk_data *prtd = (prt_one_lnk_data *) vprtd;
    erts_print(prtd->to, prtd->arg, "%T", lnk->pid);
}

void
print_port_info(Port *p, int to, void *arg)
{
    erts_aint32_t state = erts_atomic32_read_nob(&p->state);

    if (state & ERTS_PORT_SFLGS_DEAD)
	return;

    erts_print(to, arg, "=port:%T\n", p->common.id);
    erts_print(to, arg, "Slot: %d\n", internal_port_index(p->common.id));
    if (state & ERTS_PORT_SFLG_CONNECTED) {
	erts_print(to, arg, "Connected: %T", ERTS_PORT_GET_CONNECTED(p));
	erts_print(to, arg, "\n");
    }

    if (ERTS_P_LINKS(p)) {
	prt_one_lnk_data prtd;
	prtd.to = to;
	prtd.arg = arg;
	erts_print(to, arg, "Links: ");
	erts_doforall_links(ERTS_P_LINKS(p), &prt_one_lnk, &prtd);
	erts_print(to, arg, "\n");
    }
    if (ERTS_P_MONITORS(p)) {
	prt_one_lnk_data prtd;
	prtd.to = to;
	prtd.arg = arg;
	erts_print(to, arg, "Monitors: ");
	erts_doforall_monitors(ERTS_P_MONITORS(p), &prt_one_monitor, &prtd);
	erts_print(to, arg, "\n");
    }

    if (p->common.u.alive.reg != NULL)
	erts_print(to, arg, "Registered as: %T\n", p->common.u.alive.reg->name);

    if (p->drv_ptr == &fd_driver) {
	erts_print(to, arg, "Port is UNIX fd not opened by emulator: %s\n", p->name);
    } else if (p->drv_ptr == &vanilla_driver) {
	erts_print(to, arg, "Port is a file: %s\n",p->name);
    } else if (p->drv_ptr == &spawn_driver) {
	erts_print(to, arg, "Port controls external process: %s\n",p->name);
#ifndef __WIN32__
    } else if (p->drv_ptr == &forker_driver) {
	erts_print(to, arg, "Port controls forker process: %s\n",p->name);
#endif
    } else {
	erts_print(to, arg, "Port controls linked-in driver: %s\n",p->name);
    }
}

void
set_busy_port(ErlDrvPort dprt, int on)
{
    Port *prt;
    erts_aint32_t flags;

#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(port_str, 16);
#endif

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    prt = erts_drvport2port(dprt);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return;

    if (on) {
	flags = erts_smp_atomic32_read_bor_acqb(&prt->sched.flags,
						ERTS_PTS_FLG_BUSY_PORT);
	if (flags & ERTS_PTS_FLG_BUSY_PORT)
	    return; /* Already busy */

	if (flags & ERTS_PTS_FLG_HAVE_NS_TASKS)
	    erts_port_task_abort_nosuspend_tasks(prt);

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(port_busy)) {
            erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                          "%T", prt->common.id);
            DTRACE1(port_busy, port_str);
        }
#endif
    } else {
	flags = erts_smp_atomic32_read_band_acqb(&prt->sched.flags,
						 ~ERTS_PTS_FLG_BUSY_PORT);
	if (!(flags & ERTS_PTS_FLG_BUSY_PORT))
	    return; /* Already non-busy */

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(port_not_busy)) {
            erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                          "%T", prt->common.id);
            DTRACE1(port_not_busy, port_str);
        }
#endif
	if (prt->dist_entry) {
	    /*
	     * Processes suspended on distribution ports are
	     * normally queued on the dist entry.
	     */
	    erts_dist_port_not_busy(prt);
	}

	if (!(flags & ERTS_PTS_FLG_BUSY_PORT_Q))
	    erts_port_resume_procs(prt);
    }
}

void
erts_port_resume_procs(Port *prt)
{
    /*
     * Resume, in a round-robin fashion, all processes waiting on the port.
     * 
     * This version submitted by Tony Rogvall. The earlier version used
     * to resume the processes in order, which caused starvation of all but
     * the first process.
     */
    ErtsProcList *plp;

    erts_port_task_sched_lock(&prt->sched);
    plp = prt->suspended;
    prt->suspended = NULL;
    erts_port_task_sched_unlock(&prt->sched);

    if (erts_proclist_fetch(&plp, NULL)) {

#ifdef USE_VM_PROBES
	/*
	 * Hrm, for blocked dist ports, plp always seems to be NULL.
	 * That's not so fun.
	 * Well, another way to get the same info is using a D
	 * script to correlate an earlier process-port_blocked+pid
	 * event with a later process-scheduled event.  That's
	 * subject to the multi-CPU races with how events are
	 * handled, but hey, that way works most of the time.
	 */
	if (DTRACE_ENABLED(process_port_unblocked)) {
	    DTRACE_CHARBUF(port_str, 16);
	    DTRACE_CHARBUF(pid_str, 16);
	    ErtsProcList* plp2 = plp;

	    erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)), "%T", prt->common.id);
	    while (plp2 != NULL) {
		erts_snprintf(pid_str, sizeof(DTRACE_CHARBUF_NAME(pid_str)), "%T", plp2->pid);
		DTRACE2(process_port_unblocked, pid_str, port_str);
	    }
	}
#endif

	/* First proc should be resumed last */
	if (plp->next) {
	    plp->next->prev = NULL;
	    erts_resume_processes(plp->next);
	    plp->next = NULL;
	}
	erts_resume_processes(plp);
    }
}

void set_port_control_flags(ErlDrvPort port_num, int flags)
{
    Port *prt = erts_drvport2port(port_num);
    if (prt != ERTS_INVALID_ERL_DRV_PORT)
	prt->control_flags = flags;
}

int get_port_flags(ErlDrvPort ix)
{
    int flags;
    Port *prt;
    erts_aint32_t state;

    prt = erts_drvport2port_state(ix, &state);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return 0;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    flags = 0;
    if (state & ERTS_PORT_SFLG_BINARY_IO)
	flags |= PORT_FLAG_BINARY;
    if (state & ERTS_PORT_SFLG_LINEBUF_IO)
	flags |= PORT_FLAG_LINE;

    return flags;
}

void erts_raw_port_command(Port* p, byte* buf, Uint len)
{
    int fpe_was_unmasked;
    ERTS_MSACC_PUSH_STATE_M();

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));

    if (len > (Uint) INT_MAX)
	erts_exit(ERTS_ABORT_EXIT,
		 "Absurdly large data buffer (%beu bytes) passed to"
		 "output callback of %s driver.\n",
		 len,
		 p->drv_ptr->name ? p->drv_ptr->name : "unknown");

    p->caller = NIL;
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(driver_output)) {
        DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);

        dtrace_port_str(p, port_str);
        DTRACE4(driver_output, "-raw-", port_str, p->name, len);
    }
#endif
    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);
    fpe_was_unmasked = erts_block_fpe();
    (*p->drv_ptr->output)((ErlDrvData)p->drv_data, (char*) buf, (int) len);
    erts_unblock_fpe(fpe_was_unmasked);
    ERTS_MSACC_POP_STATE_M();
}

int async_ready(Port *p, void* data)
{
    int need_free = 1;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (p) {
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));
	if (p->drv_ptr->ready_async != NULL) {
	    ERTS_MSACC_PUSH_AND_SET_STATE_M(ERTS_MSACC_STATE_PORT);
#ifdef USE_VM_PROBES
            if (DTRACE_ENABLED(driver_ready_async)) {
                DTRACE_FORMAT_COMMON_PID_AND_PORT(ERTS_PORT_GET_CONNECTED(p), p)
                DTRACE3(driver_ready_async, process_str, port_str, p->name);
            }
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS 
            if (LTTNG_ENABLED(driver_ready_async)) {
                lttng_decl_portbuf(port_str);
                lttng_decl_procbuf(proc_str);
                lttng_pid_to_str(ERTS_PORT_GET_CONNECTED(p), proc_str);
                lttng_port_to_str(p, port_str);
                LTTNG3(driver_ready_async, proc_str, port_str, p->name);
            }
#endif
	    (*p->drv_ptr->ready_async)((ErlDrvData)p->drv_data, data);
	    need_free = 0;
	    ERTS_MSACC_POP_STATE_M();

	}
	erts_port_driver_callback_epilogue(p, NULL);
    }
    return need_free;
}

static void
report_missing_drv_callback(Port *p, char *drv_type, char *callback)
{
    ErtsPortNames *pnp = erts_get_port_names(p->common.id,
					     ERTS_Port2ErlDrvPort(p));
    char *unknown = "<unknown>";
    char *drv_name = pnp->driver_name ? pnp->driver_name : unknown;
    char *prt_name = pnp->name ? pnp->name : unknown;
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();    
    erts_dsprintf(dsbufp, "%T: %s driver '%s' ", p->common.id, drv_type, drv_name);
    if (sys_strcmp(drv_name, prt_name) != 0)
	erts_dsprintf(dsbufp, "(%s) ", prt_name);
    erts_dsprintf(dsbufp, "does not implement the %s callback!\n", callback);
    erts_free_port_names(pnp);
    erts_send_error_to_logger_nogl(dsbufp);
}

void
erts_stale_drv_select(Eterm port,
		      ErlDrvPort drv_port,
		      ErlDrvEvent hndl,
		      int mode,
		      int deselect)
{
    char *type;
    ErtsPortNames *pnp;
    erts_dsprintf_buf_t *dsbufp;

    if (drv_port == ERTS_INVALID_ERL_DRV_PORT) {
	Port *prt = erts_port_lookup_raw(port);
	if (prt)
	    drv_port = ERTS_Port2ErlDrvPort(prt);
	else
	    drv_port = ERTS_INVALID_ERL_DRV_PORT;
    }

    pnp = erts_get_port_names(port, drv_port);

    switch (mode) {
    case ERL_DRV_READ | ERL_DRV_WRITE:
	type = "Input/Output";
	goto deselect;
    case ERL_DRV_WRITE:
	type = "Output";
	goto deselect;
    case ERL_DRV_READ:
	type = "Input";
    deselect:
	if (deselect) {
	    driver_select(drv_port, hndl,
			  mode | ERL_DRV_USE_NO_CALLBACK,
			  0);
	}
	break;
    default:
	type = "Event";
	if (deselect)
	    driver_event(drv_port, hndl, NULL);
	break;
    }

    dsbufp = erts_create_logger_dsbuf();
    erts_dsprintf(dsbufp,
		  "%T: %s: %s driver gone away without deselecting!\n",
		  port,
		  pnp->name ? pnp->name : "<unknown>",
		  type);
    erts_free_port_names(pnp);
    erts_send_error_to_logger_nogl(dsbufp);
}

ErtsPortNames *
erts_get_port_names(Eterm id, ErlDrvPort drv_port)
{
    Port *prt;
    ErtsPortNames *pnp;
    ASSERT(is_nil(id) || is_internal_port(id));

    prt = ERTS_ErlDrvPort2Port(drv_port);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	prt = erts_port_lookup_raw(id);

    if (!prt) {
	pnp = erts_alloc(ERTS_ALC_T_PORT_NAMES, sizeof(ErtsPortNames));
	pnp->name = NULL;
	pnp->driver_name = NULL;
    }
    else {
	int do_realloc = 1;
	int len = -1;
	size_t pnp_len = sizeof(ErtsPortNames);
#ifndef DEBUG
	pnp_len += 100; /* In most cases 100 characters will be enough... */
	ASSERT(prt->common.id == id);
#endif
	pnp = erts_alloc(ERTS_ALC_T_PORT_NAMES, pnp_len);
	do {
	    int nlen;
	    char *name, *driver_name;
	    if (len > 0) {
		erts_free(ERTS_ALC_T_PORT_NAMES, pnp);
		pnp_len = sizeof(ErtsPortNames) + len;
		pnp = erts_alloc(ERTS_ALC_T_PORT_NAMES, pnp_len);
	    }
	    name = prt->name;
	    len = nlen = name ? sys_strlen(name) + 1 : 0;
	    driver_name = (prt->drv_ptr ? prt->drv_ptr->name : NULL);
	    len += driver_name ? sys_strlen(driver_name) + 1 : 0;
	    if (len <= pnp_len - sizeof(ErtsPortNames)) {
		if (!name)
		    pnp->name = NULL;
		else {
		    pnp->name = ((char *) pnp) + sizeof(ErtsPortNames);
		    sys_strcpy(pnp->name, name);
		}
		if (!driver_name)
		    pnp->driver_name = NULL;
		else {
		    pnp->driver_name = (((char *) pnp)
					+ sizeof(ErtsPortNames)
					+ nlen);
		    sys_strcpy(pnp->driver_name, driver_name);
		}
		do_realloc = 0;
	    }
	} while (do_realloc);
    }
    return pnp;
}

void
erts_free_port_names(ErtsPortNames *pnp)
{
    erts_free(ERTS_ALC_T_PORT_NAMES, pnp);
}

ErlDrvTermData driver_mk_term_nil(void)
{
    return driver_term_nil;
}

void driver_report_exit(ErlDrvPort ix, int status)
{
   Eterm* hp;
   ErlOffHeap *ohp;
   Eterm tuple;
   Process *rp;
   Eterm pid;
   ErtsMessage *mp;
   ErtsProcLocks rp_locks = 0;
   int scheduler = erts_get_scheduler_id() != 0;
   Port* prt = erts_drvport2port(ix);
   int trace_send = IS_TRACED_FL(prt, F_TRACE_SEND);

   if (prt == ERTS_INVALID_ERL_DRV_PORT)
       return;

   ERTS_SMP_CHK_NO_PROC_LOCKS;
   ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

   pid = ERTS_PORT_GET_CONNECTED(prt);
   ASSERT(is_internal_pid(pid));

   rp = (scheduler
	 ? erts_proc_lookup(pid)
	 : erts_pid2proc_opt(NULL, 0, pid, 0, ERTS_P2P_FLG_INC_REFC));
   if (!rp)
       return;

   mp = erts_alloc_message_heap(trace_send ? NULL : rp, &rp_locks, 3+3, &hp, &ohp);

   tuple = TUPLE2(hp, am_exit_status, make_small(status));
   hp += 3;
   tuple = TUPLE2(hp, prt->common.id, tuple);

    if (IS_TRACED_FL(prt, F_TRACE_SEND))
        trace_port_send(prt, pid, tuple, 1);

   ERL_MESSAGE_TOKEN(mp) = am_undefined;
   erts_queue_message(rp, rp_locks, mp, tuple, prt->common.id);

   erts_smp_proc_unlock(rp, rp_locks);
   if (!scheduler)
       erts_proc_dec_refc(rp);
}

#define ERTS_B2T_STATES_DEF_STATES_SZ 5
#define ERTS_B2T_STATES_DEF_STATES_INC 100

struct b2t_states__ {
    int len;
    int ix;
    int used;
    ErtsBinary2TermState *state;
    ErtsBinary2TermState def_states[ERTS_B2T_STATES_DEF_STATES_SZ];
#ifdef DEBUG
    byte **org_ext;
    byte *def_org_ext[ERTS_B2T_STATES_DEF_STATES_SZ];
#endif
};

static ERTS_INLINE void
init_b2t_states(struct b2t_states__ *b2tsp)
{
    b2tsp->len = ERTS_B2T_STATES_DEF_STATES_SZ;
    b2tsp->ix = 0;
    b2tsp->used = 0;
    b2tsp->state = &b2tsp->def_states[0];
#ifdef DEBUG
    b2tsp->org_ext = &b2tsp->def_org_ext[0];
#endif
}

static ERTS_INLINE void
grow_b2t_states(struct b2t_states__ *b2tsp)
{
    if (b2tsp->state != &b2tsp->def_states[0]) {
	b2tsp->len += ERTS_B2T_STATES_DEF_STATES_INC;
	b2tsp->state = erts_realloc(ERTS_ALC_T_TMP,
				    b2tsp->state,
				    sizeof(ErtsBinary2TermState)*b2tsp->len);
#ifdef DEBUG
	b2tsp->org_ext = erts_realloc(ERTS_ALC_T_TMP,
				      b2tsp->org_ext,
				      sizeof(char *)*b2tsp->len);
#endif
    }
    else {
	ErtsBinary2TermState *new_states;
	new_states = erts_alloc(ERTS_ALC_T_TMP,
				(sizeof(ErtsBinary2TermState)
				 *ERTS_B2T_STATES_DEF_STATES_INC));
	sys_memcpy((void *) new_states,
		   (void *) b2tsp->state,
		   sizeof(ErtsBinary2TermState)*ERTS_B2T_STATES_DEF_STATES_SZ);
	b2tsp->state = new_states;
	b2tsp->len = ERTS_B2T_STATES_DEF_STATES_INC;
#ifdef DEBUG
	{
	    byte **new_org_ext = erts_alloc(ERTS_ALC_T_TMP,
					    (sizeof(char *)
					     *ERTS_B2T_STATES_DEF_STATES_INC));
	    sys_memcpy((void *) new_org_ext,
		       (void *) b2tsp->org_ext,
		       sizeof(char *)*ERTS_B2T_STATES_DEF_STATES_SZ);
	    b2tsp->org_ext = new_org_ext;
	}
#endif
    }
}

static ERTS_INLINE void
cleanup_b2t_states(struct b2t_states__ *b2tsp)
{
    if (b2tsp->state != &b2tsp->def_states[0]) {
	erts_free(ERTS_ALC_T_TMP, b2tsp->state);
#ifdef DEBUG
	erts_free(ERTS_ALC_T_TMP, b2tsp->org_ext);
#endif
    }
}


/*
 * Generate an Erlang term from data in an array (representing a simple stack
 * machine to build terms).
 * Returns:
 * 	-1 on error in input data
 *       0 if the message was not delivered (bad to pid or closed port)
 *       1 if the message was delivered successfully
 */

static int
driver_deliver_term(Port *prt, Eterm to, ErlDrvTermData* data, int len)
{
#define HEAP_EXTRA 200
#define ERTS_DDT_FAIL do { res = -1; goto done; } while (0)
    Uint need = 0;
    int depth = 0;
    int res = 0;
    ErlDrvTermData* ptr;
    ErlDrvTermData* ptr_end;
    DECLARE_ESTACK(stack); 
    Eterm mess;
    Process* rp = NULL;
    ErtsHeapFactory factory;
    ErtsProcLocks rp_locks = 0;
    struct b2t_states__ b2t;
    int scheduler;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_UNDEF(mess,NIL);
    ERTS_UNDEF(scheduler,1);

    factory.mode = FACTORY_CLOSED;
    init_b2t_states(&b2t);

    /*
     * We used to check port and process here. In the SMP enabled emulator,
     * however, we don't want to that until we have verified the term.
     */

    /*
     * Check ErlDrvTermData for consistency and calculate needed heap size
     * and stack depth.
     */
    ptr = data;
    ptr_end = ptr + len;

    while (ptr < ptr_end) {
	ErlDrvTermData tag = *ptr++;

#define ERTS_DDT_CHK_ENOUGH_ARGS(NEED) \
	if (ptr+((NEED)-1) >= ptr_end) ERTS_DDT_FAIL;

	switch(tag) {
	case ERL_DRV_NIL: /* no arguments */
	    depth++;
	    break;
	case ERL_DRV_ATOM: /* atom argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if (is_not_atom(ptr[0])) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_INT:  /* signed int argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    /* check for bignum */
	    if (!IS_SSMALL((Sint)ptr[0]))
		need += BIG_UINT_HEAP_SIZE;  /* use small_to_big */
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_UINT:  /* unsigned int argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    /* check for bignum */
	    if (!IS_USMALL(0, (Uint)ptr[0]))
		need += BIG_UINT_HEAP_SIZE;  /* use small_to_big */
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_INT64:  /* pointer to signed 64-bit int argument  */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    erts_bld_sint64(NULL, &need, *((Sint64 *) ptr[0]));
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_UINT64:  /* pointer to unsigned 64-bit int argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    erts_bld_uint64(NULL, &need, *((Uint64 *) ptr[0]));
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_PORT:  /* port argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if (is_not_internal_port(ptr[0])) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_BINARY: { /* ErlDrvBinary*, size, offs */
	    ErlDrvBinary* b;
	    Uint size;
	    Uint offset;
	    ERTS_DDT_CHK_ENOUGH_ARGS(3);
	    b = (ErlDrvBinary*) ptr[0];
	    size = ptr[1];
	    offset = ptr[2];
	    if (!b || size + offset > b->orig_size)
		ERTS_DDT_FAIL; /* No binary or outside the binary */
	    need += (size <= ERL_ONHEAP_BIN_LIMIT
			 ? heap_bin_size(size)
			 : PROC_BIN_SIZE);
	    ptr += 3;
	    depth++;
	    break;
	}
	case ERL_DRV_BUF2BINARY: { /* char*, size */
	    byte *bufp;
	    Uint size;
	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    bufp = (byte *) ptr[0];
	    size = (Uint) ptr[1];
	    if (!bufp && size > 0) ERTS_DDT_FAIL; 
	    need += (size <= ERL_ONHEAP_BIN_LIMIT
		     ? heap_bin_size(size)
		     : PROC_BIN_SIZE);
	    ptr += 2;
	    depth++;
	    break;
	}
	case ERL_DRV_STRING: /* char*, length */
	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    if ((char *) ptr[0] == NULL || (int) ptr[1] < 0) ERTS_DDT_FAIL;
	    need += ptr[1] * 2;
	    ptr += 2;
	    depth++;
	    break;
	case ERL_DRV_STRING_CONS: /* char*, length */
	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    if ((char *) ptr[0] == NULL || (int) ptr[1] < 0) ERTS_DDT_FAIL;
	    need += ptr[1] * 2;
	    if (depth < 1) ERTS_DDT_FAIL;
	    ptr += 2;
	    break;
	case ERL_DRV_LIST: /* int */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if ((int) ptr[0] <= 0) ERTS_DDT_FAIL;
	    need += (ptr[0]-1)*2;  /* list cells */
	    depth -= ptr[0];
	    if (depth < 0) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_TUPLE: { /* int */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if ((int) ptr[0] < 0) ERTS_DDT_FAIL;
	    need += ptr[0]+1;   /* vector positions + arityval */
	    depth -= ptr[0];
	    if (depth < 0) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	}
	case ERL_DRV_PID: /* pid argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if (is_not_internal_pid(ptr[0])) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_FLOAT: /* double * */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    need += FLOAT_SIZE_OBJECT;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_EXT2TERM: { /* char *ext, int size */
	    byte* ext;
	    Sint size;
	    Sint hsz;

	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    ext = (byte *) ptr[0];
	    size = (Sint) ptr[1];
	    if (!ext || size <= 0)
		ERTS_DDT_FAIL;
	    if (b2t.len <= b2t.ix)
		grow_b2t_states(&b2t);
#ifdef DEBUG
	    b2t.org_ext[b2t.ix] = ext;
#endif
	    hsz = erts_binary2term_prepare(&b2t.state[b2t.ix], ext, size);
	    if (hsz < 0)
		ERTS_DDT_FAIL; /* Invalid data */
	    b2t.state[b2t.ix++].heap_size = hsz;
	    need += hsz;
	    ptr += 2;
	    depth++;
	    break;
	}
	case ERL_DRV_MAP: { /* int */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if ((int) ptr[0] < 0) ERTS_DDT_FAIL;
            if (ptr[0] > MAP_SMALL_MAP_LIMIT) {
                need += HASHMAP_ESTIMATED_HEAP_SIZE(ptr[0]);
            } else {
                need += MAP_HEADER_FLATMAP_SZ + 1 + 2*ptr[0];
            }
	    depth -= 2*ptr[0];
	    if (depth < 0) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	}

	default:
	    ERTS_DDT_FAIL;
	}
#undef ERTS_DDT_CHK_ENOUGH_ARGS
    }

    if ((depth != 1) || (ptr != ptr_end))
	ERTS_DDT_FAIL;

    b2t.used = b2t.ix;
    b2t.ix = 0;

    /*
     * The term is OK. Go ahead and validate the process.
     */

    /*
     * Increase refc on proc if done from a non-scheduler thread.
     */
    scheduler = erts_get_scheduler_id() != 0;
    rp = (scheduler
	  ? erts_proc_lookup(to)
	  : erts_pid2proc_opt(NULL, 0, to, 0, ERTS_P2P_FLG_INC_REFC));
    if (!rp) {
        if (!prt || !IS_TRACED_FL(prt, F_TRACE_SEND))
            goto done;
        if (!erts_is_tracer_proc_enabled_send(NULL, 0, &prt->common))
            goto done;

	res = -2;

        /* We allocate a temporary heap to be used to create
           the message that may be sent using tracing */
        erts_factory_tmp_init(&factory, erts_alloc(ERTS_ALC_T_DRIVER, need*sizeof(Eterm)),
                              need, ERTS_ALC_T_DRIVER);

    } else {
        /* We force the creation of a heap fragment (rp == NULL) when send
           tracing so that we don't have the main lock of the process while
           tracing */
        Process *trace_rp = prt && IS_TRACED_FL(prt, F_TRACE_SEND) ? NULL : rp;
        (void) erts_factory_message_create(&factory, trace_rp, &rp_locks, need);
        res = 1;
    }

    /*
     * Interpret the instructions and build the term.
     */
    ptr = data;
    while (ptr < ptr_end) {
	ErlDrvTermData tag = *ptr++;

	switch(tag) {
	case ERL_DRV_NIL: /* no arguments */
	    mess = NIL;
	    break;

	case ERL_DRV_ATOM: /* atom argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_INT:  /* signed int argument */
	    erts_reserve_heap(&factory, BIG_UINT_HEAP_SIZE);
	    if (IS_SSMALL((Sint)ptr[0]))
		mess = make_small((Sint)ptr[0]);
	    else {
		mess = small_to_big((Sint)ptr[0], factory.hp);
		factory.hp += BIG_UINT_HEAP_SIZE;
	    }
	    ptr++;
	    break;

	case ERL_DRV_UINT:  /* unsigned int argument */
	    erts_reserve_heap(&factory, BIG_UINT_HEAP_SIZE);
	    if (IS_USMALL(0, (Uint)ptr[0]))
		mess = make_small((Uint)ptr[0]);
	    else {
		mess = uint_to_big((Uint)ptr[0], factory.hp);
		factory.hp += BIG_UINT_HEAP_SIZE;
	    }
	    ptr++;
	    break;

	case ERL_DRV_INT64: /* pointer to unsigned 64-bit int argument */
	    erts_reserve_heap(&factory, BIG_NEED_FOR_BITS(64));
	    mess = erts_bld_sint64(&factory.hp, NULL, *((Sint64 *) ptr[0]));
	    ptr++;
	    break;

	case ERL_DRV_UINT64: /* pointer to unsigned 64-bit int argument */
	    erts_reserve_heap(&factory, BIG_NEED_FOR_BITS(64));
	    mess = erts_bld_uint64(&factory.hp, NULL, *((Uint64 *) ptr[0]));
	    ptr++;
	    break;

	case ERL_DRV_PORT:  /* port argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_BINARY: { /* ErlDrvBinary*, size, offs */
	    ErlDrvBinary* b = (ErlDrvBinary*) ptr[0];
	    Uint size = ptr[1];
	    Uint offset = ptr[2];

	    if (esdp)
		esdp->io.in += (Uint64) size;
	    else
		erts_atomic64_add_nob(&bytes_in, (erts_aint64_t) size);

	    if (size <= ERL_ONHEAP_BIN_LIMIT) {
		ErlHeapBin* hbp = (ErlHeapBin *) erts_produce_heap(&factory,
								   heap_bin_size(size), HEAP_EXTRA);
		hbp->thing_word = header_heap_bin(size);
		hbp->size = size;
		if (size > 0) {
		    sys_memcpy((void *) hbp->data, (void *) (((byte*) b->orig_bytes) + offset), size);
		}
		mess = make_binary(hbp);
	    }
	    else {
		ProcBin* pb = (ProcBin *) erts_produce_heap(&factory,
							    PROC_BIN_SIZE, HEAP_EXTRA);
		driver_binary_inc_refc(b);  /* caller will free binary */
		pb->thing_word = HEADER_PROC_BIN;
		pb->size = size;
		pb->next = factory.off_heap->first;
		factory.off_heap->first = (struct erl_off_heap_header*)pb;
		pb->val = ErlDrvBinary2Binary(b);
		pb->bytes = ((byte*) b->orig_bytes) + offset;
		pb->flags = 0;
		mess =  make_binary(pb);
		OH_OVERHEAD(factory.off_heap, pb->size / sizeof(Eterm));
	    }
	    ptr += 3;
	    break;
	}

	case ERL_DRV_BUF2BINARY: { /* char*, size */
	    byte *bufp = (byte *) ptr[0];
	    Uint size = (Uint) ptr[1];

	    if (esdp)
		esdp->io.in += (Uint64) size;
	    else
		erts_atomic64_add_nob(&bytes_in, (erts_aint64_t) size);

	    if (size <= ERL_ONHEAP_BIN_LIMIT) {
		ErlHeapBin* hbp = (ErlHeapBin *) erts_produce_heap(&factory,
								   heap_bin_size(size),
								   HEAP_EXTRA);
		hbp->thing_word = header_heap_bin(size);
		hbp->size = size;
		if (size > 0) {
		    ASSERT(bufp);
		    sys_memcpy((void *) hbp->data, (void *) bufp, size);
		}
		mess = make_binary(hbp);
	    }
	    else {
		ProcBin* pbp;
		Binary* bp = erts_bin_nrml_alloc(size);
		ASSERT(bufp);
		erts_refc_init(&bp->refc, 1);
		sys_memcpy((void *) bp->orig_bytes, (void *) bufp, size);
		pbp = (ProcBin *) erts_produce_heap(&factory,
						    PROC_BIN_SIZE, HEAP_EXTRA);
		pbp->thing_word = HEADER_PROC_BIN;
		pbp->size = size;
		pbp->next = factory.off_heap->first;
		factory.off_heap->first = (struct erl_off_heap_header*)pbp;
		pbp->val = bp;
		pbp->bytes = (byte*) bp->orig_bytes;
		pbp->flags = 0;
		OH_OVERHEAD(factory.off_heap, pbp->size / sizeof(Eterm));
		mess = make_binary(pbp);
	    }
	    ptr += 2;
	    break;
	}

	case ERL_DRV_STRING: /* char*, length */
	    if (esdp)
		esdp->io.in += (Uint64) ptr[1];
	    else
		erts_atomic64_add_nob(&bytes_in, (erts_aint64_t) ptr[1]);
	    erts_reserve_heap(&factory, 2*ptr[1]);
	    mess = buf_to_intlist(&factory.hp, (char*)ptr[0], ptr[1], NIL);
	    ptr += 2;
	    break;

	case ERL_DRV_STRING_CONS:  /* char*, length */
	    mess = ESTACK_POP(stack);
	    erts_reserve_heap(&factory, 2*ptr[1]);
	    mess = buf_to_intlist(&factory.hp, (char*)ptr[0], ptr[1], mess);
	    ptr += 2;
	    break;

	case ERL_DRV_LIST: { /* unsigned */
	    Uint i = (int) ptr[0]; /* i > 0 */

	    mess = ESTACK_POP(stack);
	    i--;
	    erts_reserve_heap(&factory, 2*i);
	    while(i > 0) {
		Eterm hd = ESTACK_POP(stack);

		mess = CONS(factory.hp, hd, mess);
		factory.hp += 2;
		i--;
	    }
	    ptr++;
	    break;
	}

	case ERL_DRV_TUPLE: { /* int */
	    int size = (int)ptr[0];
	    Eterm* tp = erts_produce_heap(&factory, size+1, HEAP_EXTRA);

	    *tp = make_arityval(size);
	    mess = make_tuple(tp);

	    tp += size;   /* point at last element */

	    while(size--) {
		*tp-- = ESTACK_POP(stack);
	    }
	    ptr++;
	    break;
	}

	case ERL_DRV_PID: /* pid argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_FLOAT: { /* double * */
	    FloatDef f;
	    Eterm* fp = erts_produce_heap(&factory, FLOAT_SIZE_OBJECT, HEAP_EXTRA);

	    mess = make_float(fp);
	    f.fd = *((double *) ptr[0]);
            if (!erts_isfinite(f.fd))
                ERTS_DDT_FAIL;
	    PUT_DOUBLE(f, fp);
	    ptr++;
	    break;
	}

	case ERL_DRV_EXT2TERM: /* char *ext, int size */
	    ASSERT(b2t.org_ext[b2t.ix] == (byte *) ptr[0]);

	    erts_reserve_heap(&factory, b2t.state[b2t.ix].heap_size);
	    mess = erts_binary2term_create(&b2t.state[b2t.ix++], &factory);
	    if (mess == THE_NON_VALUE)
		ERTS_DDT_FAIL;
	    ptr += 2;
	    break;

	case ERL_DRV_MAP: { /* int */
	    int size = (int)ptr[0];
            if (size > MAP_SMALL_MAP_LIMIT) {
                int ix = 2*size;
                Eterm* leafs;

		erts_produce_heap(&factory, ix, HEAP_EXTRA);
		leafs = factory.hp;
                while(ix--) { *--leafs = ESTACK_POP(stack); }

                mess = erts_hashmap_from_array(&factory, leafs, size, 1);
                if (is_non_value(mess))
                    ERTS_DDT_FAIL;
            } else {
                Eterm* vp;
                flatmap_t *mp;
		Eterm* tp = erts_produce_heap(&factory,
					      2*size + 1 + MAP_HEADER_FLATMAP_SZ,
					      HEAP_EXTRA);

                *tp = make_arityval(size);

                mp = (flatmap_t*) (tp + 1 + size);
                mp->thing_word = MAP_HEADER_FLATMAP;
                mp->size = size;
                mp->keys = make_tuple(tp);
                mess = make_flatmap(mp);

                tp += size;    /* point at last key */
                vp = factory.hp - 1;   /* point at last value */

                while(size--) {
                    *vp-- = ESTACK_POP(stack);
                    *tp-- = ESTACK_POP(stack);
                }
                if (!erts_validate_and_sort_flatmap(mp))
                    ERTS_DDT_FAIL;
            }
            ptr++;
	    break;
	}

	}
	ESTACK_PUSH(stack, mess);
    }

 done:

    if (res > 0) {
	Eterm from = am_undefined;
	mess = ESTACK_POP(stack);  /* get resulting value */
	erts_factory_trim_and_close(&factory, &mess, 1);

	if (prt) {
	    if (IS_TRACED_FL(prt, F_TRACE_SEND)) {
		trace_port_send(prt, to, mess, 1);
	    }
	    from = prt->common.id;
	}

	/* send message */
        ERL_MESSAGE_TOKEN(factory.message) = am_undefined;
	erts_queue_message(rp, rp_locks, factory.message, mess, from);
    }
    else if (res == -2) {
        /* this clause only happens when we were requested to
           generate a send trace, but the process to send to
           did not exist any more */
        mess = ESTACK_POP(stack);  /* get resulting value */

        trace_port_send(prt, to, mess, 0);

	erts_factory_trim_and_close(&factory, &mess, 1);
        erts_free(ERTS_ALC_T_DRIVER, factory.hp_start);
        res = 0;
    }
    else {
	if (b2t.ix > b2t.used)
	    b2t.used = b2t.ix;
	for (b2t.ix = 0; b2t.ix < b2t.used; b2t.ix++)
	    erts_binary2term_abort(&b2t.state[b2t.ix]);
        if (factory.mode != FACTORY_CLOSED) {
            ERL_MESSAGE_TERM(factory.message) = am_undefined;
            erts_factory_undo(&factory);
        }
    }
    if (rp) {
	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
	if (!scheduler)
	    erts_proc_dec_refc(rp);
    }
    cleanup_b2t_states(&b2t);
    DESTROY_ESTACK(stack);
    return res;
#undef ERTS_DDT_FAIL
#undef HEAP_EXTRA
}

static ERTS_INLINE int
deliver_term_check_port(ErlDrvTermData port_id, Eterm *connected_p,
                        Port **trace_prt)
{
#ifdef ERTS_SMP
    ErtsThrPrgrDelayHandle dhndl = erts_thr_progress_unmanaged_delay();
#endif
    erts_aint32_t state;
    Port *prt = erts_port_lookup_raw((Eterm) port_id);
    if (!prt)
	return -1;
    state = erts_atomic32_read_nob(&prt->state);
    if (state & (ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
		 | ERTS_PORT_SFLG_CLOSING)) {
	if (state & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	    return -1;
	else
	    return 0;
    }
    if (connected_p) {
#ifdef ERTS_SMP
	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	    ETHR_MEMBAR(ETHR_LoadLoad);
#endif
	*connected_p = ERTS_PORT_GET_CONNECTED(prt);
    }
#ifdef ERTS_SMP
    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED) {
	erts_thr_progress_unmanaged_continue(dhndl);
	ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
    } else
#endif
    {
        *trace_prt = prt;
    }
    ERTS_SMP_LC_ASSERT(dhndl == ERTS_THR_PRGR_DHANDLE_MANAGED
		       ? erts_lc_is_port_locked(prt)
		       : !erts_lc_is_port_locked(prt));
    return 1;
}

int erl_drv_output_term(ErlDrvTermData port_id, ErlDrvTermData* data, int len)
{
    /* May be called from arbitrary thread */
    Eterm connected;
    Port *prt = NULL;
    int res = deliver_term_check_port(port_id, &connected, &prt);
    if (res <= 0)
	return res;
    return driver_deliver_term(prt, connected, data, len);
}

/*
 * driver_output_term() is deprecated, and has been scheduled for
 * removal in OTP-R17. It is replaced by erl_drv_output_term()
 * above.
 */
int 
driver_output_term(ErlDrvPort drvport, ErlDrvTermData* data, int len)
{
    erts_aint32_t state;
    Port* prt;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    /* NOTE! It *not* safe to access 'drvport' from unmanaged threads. */
    prt = erts_drvport2port_state(drvport, &state);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1; /* invalid (dead) */
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if (state & ERTS_PORT_SFLG_CLOSING)
	return 0;

    return driver_deliver_term(prt, ERTS_PORT_GET_CONNECTED(prt), data, len);
}

int erl_drv_send_term(ErlDrvTermData port_id,
		      ErlDrvTermData to,
		      ErlDrvTermData* data,
		      int len)
{
    /* May be called from arbitrary thread */
    Port *prt = NULL;
    int res = deliver_term_check_port(port_id, NULL, &prt);
    if (res <= 0)
	return res;
    return driver_deliver_term(prt, to, data, len);
}

/*
 * driver_send_term() is deprecated, and has been scheduled for
 * removal in OTP-R17. It is replaced by erl_drv_send_term() above.
 */
int
driver_send_term(ErlDrvPort drvport,
		 ErlDrvTermData to,
		 ErlDrvTermData* data,
		 int len)
{
    /*
     * NOTE! It is *not* safe to access the 'drvport' parameter
     * from unmanaged threads. Also note that it is impossible
     * to make this access safe without using a less efficient
     * internal data representation for ErlDrvPort.
     */
    Port* prt = NULL;
    ERTS_SMP_CHK_NO_PROC_LOCKS;
#ifdef ERTS_SMP
    if (erts_thr_progress_is_managed_thread())
#endif
    {
	erts_aint32_t state;
	prt = erts_drvport2port_state(drvport, &state);
	if (prt == ERTS_INVALID_ERL_DRV_PORT)
	    return -1; /* invalid (dead) */
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
	if (state & ERTS_PORT_SFLG_CLOSING)
	    return 0;
    }
    return driver_deliver_term(prt, to, data, len);
}


/*
 * Output a binary with hlen bytes from hbuf as list header
 * and data is len length of bin starting from offset offs.
 */

int driver_output_binary(ErlDrvPort ix, char* hbuf, ErlDrvSizeT hlen,
			 ErlDrvBinary* bin, ErlDrvSizeT offs, ErlDrvSizeT len)
{
    erts_aint32_t state;
    Port* prt = erts_drvport2port_state(ix, &state);
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if (state & ERTS_PORT_SFLG_CLOSING)
	return 0;

    prt->bytes_in += (hlen + len);
    if (esdp)
	esdp->io.in += (Uint64) (hlen + len);
    else
	erts_atomic64_add_nob(&bytes_in, (erts_aint64_t) (hlen + len));
    if (state & ERTS_PORT_SFLG_DISTRIBUTION) {
	return erts_net_message(prt,
				prt->dist_entry,
				(byte*) hbuf, hlen,
				(byte*) (bin->orig_bytes+offs), len);
    }
    else
	deliver_bin_message(prt, ERTS_PORT_GET_CONNECTED(prt), 
			    hbuf, hlen, bin, offs, len);
    return 0;
}

/* driver_output2:
** Delivers hlen bytes from hbuf to the port owner as a list;
** after that, the port settings apply, buf is sent as binary or list.
**
** Example: if hlen = 3 then the port owner will receive the data
** [H1,H2,H3 | T]
*/
int driver_output2(ErlDrvPort ix, char* hbuf, ErlDrvSizeT hlen,
		   char* buf, ErlDrvSizeT len)
{
    erts_aint32_t state;
    Port* prt = erts_drvport2port_state(ix, &state);
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if (state & ERTS_PORT_SFLG_CLOSING)
	return 0;
    
    prt->bytes_in += (hlen + len);
    if (esdp)
	esdp->io.in += (Uint64) (hlen + len);
    else
	erts_atomic64_add_nob(&bytes_in, (erts_aint64_t) (hlen + len));
    if (state & ERTS_PORT_SFLG_DISTRIBUTION) {
	if (len == 0)
	    return erts_net_message(prt,
				    prt->dist_entry,
				    NULL, 0,
				    (byte*) hbuf, hlen);
	else
	    return erts_net_message(prt,
				    prt->dist_entry,
				    (byte*) hbuf, hlen,
				    (byte*) buf, len);
    }
    else if (state & ERTS_PORT_SFLG_LINEBUF_IO)
	deliver_linebuf_message(prt, state, ERTS_PORT_GET_CONNECTED(prt),
				hbuf, hlen, buf, len);
    else
	deliver_read_message(prt, state, ERTS_PORT_GET_CONNECTED(prt),
			     hbuf, hlen, buf, len, 0);
    return 0;
}

/* Interface functions available to driver writers */

int driver_output(ErlDrvPort ix, char* buf, ErlDrvSizeT len)
{
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    return driver_output2(ix, NULL, 0, buf, len);
}

int driver_outputv(ErlDrvPort ix, char* hbuf, ErlDrvSizeT hlen,
		   ErlIOVec* vec, ErlDrvSizeT skip)
{
    int n;
    ErlDrvSizeT len;
    ErlDrvSizeT size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    Port* prt;
    erts_aint32_t state;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    ASSERT(vec->size >= skip);
    if (vec->size <= skip)
	return driver_output2(ix, hbuf, hlen, NULL, 0);
    size = vec->size - skip;   /* Size of remaining bytes in vector */

    prt = erts_drvport2port_state(ix, &state);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (state & ERTS_PORT_SFLG_CLOSING)
	return 0;

    /* size > 0 ! */
    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;
    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	} else {
	    iov->iov_base = ((char *)(iov->iov_base)) + skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while (skip > 0);

    /* XXX handle distribution !!! */
    prt->bytes_in += (hlen + size);
    if (esdp)
	esdp->io.in += (Uint64) (hlen + size);
    else
	erts_atomic64_add_nob(&bytes_in, (erts_aint64_t) (hlen + size));
    deliver_vec_message(prt, ERTS_PORT_GET_CONNECTED(prt), hbuf, hlen,
			binv, iov, n, size);
    return 0;
}

/* Copy bytes from a vector into a buffer
** input is a vector a buffer and a max length
** return bytes copied
*/
ErlDrvSizeT driver_vec_to_buf(ErlIOVec *vec, char *buf, ErlDrvSizeT len)
{
    SysIOVec* iov = vec->iov;
    int n = vec->vsize;
    ErlDrvSizeT orig_len = len;

    while(n--) {
	size_t ilen = iov->iov_len;
	if (ilen < len) {
	    sys_memcpy(buf, iov->iov_base, ilen);
	    len -= ilen;
	    buf += ilen;
	    iov++;
	}
	else {
	    sys_memcpy(buf, iov->iov_base, len);
	    return orig_len;
	}
    }
    return (orig_len - len);
}


/*
 * - driver_alloc_binary() is thread safe (efile driver depend on it).
 * - driver_realloc_binary(), and driver_free_binary() are *not* thread safe.
 */

/*
 * reference count on driver binaries...
 */

ErlDrvSInt
driver_binary_get_refc(ErlDrvBinary *dbp)
{
    Binary* bp = ErlDrvBinary2Binary(dbp);
    return (ErlDrvSInt) erts_refc_read(&bp->refc, 1);
}

ErlDrvSInt
driver_binary_inc_refc(ErlDrvBinary *dbp)
{
    Binary* bp = ErlDrvBinary2Binary(dbp);
    return (ErlDrvSInt) erts_refc_inctest(&bp->refc, 2);
}

ErlDrvSInt
driver_binary_dec_refc(ErlDrvBinary *dbp)
{
    Binary* bp = ErlDrvBinary2Binary(dbp);
    return (ErlDrvSInt) erts_refc_dectest(&bp->refc, 1);
}


/*
** Allocation/Deallocation of binary objects 
*/

ErlDrvBinary*
driver_alloc_binary(ErlDrvSizeT size)
{
    Binary* bin;

    bin = erts_bin_drv_alloc_fnf((Uint) size);
    if (!bin)
	return NULL; /* The driver write must take action */
    erts_refc_init(&bin->refc, 1);
    return Binary2ErlDrvBinary(bin);
}

/* Reallocate space hold by binary */

ErlDrvBinary* driver_realloc_binary(ErlDrvBinary* bin, ErlDrvSizeT size)
{
    Binary* oldbin;
    Binary* newbin;

    if (!bin) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Bad use of driver_realloc_binary(%p, %lu): "
		      "called with ",
		      bin, (unsigned long)size);
	if (!bin) {
	    erts_dsprintf(dsbufp, "NULL pointer as first argument");
	}
	erts_send_warning_to_logger_nogl(dsbufp);
	if (!bin)
	    return driver_alloc_binary(size);
    }

    oldbin = ErlDrvBinary2Binary(bin);
    newbin = (Binary *) erts_bin_realloc_fnf(oldbin, size);
    if (!newbin)
	return NULL;

    return Binary2ErlDrvBinary(newbin);
}


void driver_free_binary(ErlDrvBinary* dbin)
{
    Binary *bin;
    if (!dbin) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Bad use of driver_free_binary(%p): called with "
		      "NULL pointer as argument", dbin);
	erts_send_warning_to_logger_nogl(dsbufp);
	return;
    }

    bin = ErlDrvBinary2Binary(dbin);
    if (erts_refc_dectest(&bin->refc, 0) == 0)
	erts_bin_free(bin);
}


/* 
 * Allocation/deallocation of memory for drivers 
 */

void *driver_alloc(ErlDrvSizeT size)
{
    return erts_alloc_fnf(ERTS_ALC_T_DRV, (Uint) size);
}

void *driver_realloc(void *ptr, ErlDrvSizeT size)
{
    return erts_realloc_fnf(ERTS_ALC_T_DRV, ptr, (Uint) size);
}

void driver_free(void *ptr)
{
    erts_free(ERTS_ALC_T_DRV, ptr);
}

/*
 * Port Data Lock
 */

static void
pdl_init(void)
{
}

static ERTS_INLINE void
pdl_init_refc(ErlDrvPDL pdl)
{
    erts_atomic_init_nob(&pdl->refc, 1);
}

static ERTS_INLINE ErlDrvSInt
pdl_read_refc(ErlDrvPDL pdl)
{
    erts_aint_t refc = erts_atomic_read_nob(&pdl->refc);
    ERTS_LC_ASSERT(refc >= 0);
    return (ErlDrvSInt) refc;
}

static ERTS_INLINE void
pdl_inc_refc(ErlDrvPDL pdl)
{
    erts_atomic_inc_nob(&pdl->refc);
    ERTS_LC_ASSERT(driver_pdl_get_refc(pdl) > 1);
}

static ERTS_INLINE ErlDrvSInt
pdl_inctest_refc(ErlDrvPDL pdl)
{
    erts_aint_t refc = erts_atomic_inc_read_nob(&pdl->refc);
    ERTS_LC_ASSERT(refc > 1);
    return (ErlDrvSInt) refc;
}

#if 0 /* unused */
static ERTS_INLINE void
pdl_dec_refc(ErlDrvPDL pdl)
{
    erts_atomic_dec_nob(&pdl->refc);
    ERTS_LC_ASSERT(driver_pdl_get_refc(pdl) > 0);
}
#endif

static ERTS_INLINE ErlDrvSInt
pdl_dectest_refc(ErlDrvPDL pdl)
{
    erts_aint_t refc = erts_atomic_dec_read_nob(&pdl->refc);
    ERTS_LC_ASSERT(refc >= 0);
    return (ErlDrvSInt) refc;
}

static ERTS_INLINE void pdl_destroy(ErlDrvPDL pdl)
{
    ERTS_LC_ASSERT(driver_pdl_get_refc(pdl) == 0);
    erts_mtx_destroy(&pdl->mtx);
    erts_port_dec_refc(pdl->prt);
    erts_free(ERTS_ALC_T_PORT_DATA_LOCK, pdl);
}

#ifdef ERTS_SMP

static void driver_monitor_lock_pdl(Port *p) {
    if (p->port_data_lock) {
	driver_pdl_lock(p->port_data_lock);
    }
    /* Now we either have the port lock or the port_data_lock */
    ERTS_LC_ASSERT(!p->port_data_lock
		   || erts_lc_mtx_is_locked(&(p->port_data_lock->mtx)));
    ERTS_SMP_LC_ASSERT(p->port_data_lock
		       || erts_lc_is_port_locked(p));
}

static void driver_monitor_unlock_pdl(Port *p) {
    /* We should either have the port lock or the port_data_lock */
    ERTS_LC_ASSERT(!p->port_data_lock
		   || erts_lc_mtx_is_locked(&(p->port_data_lock->mtx)));
    ERTS_SMP_LC_ASSERT(p->port_data_lock
		       || erts_lc_is_port_locked(p));
    if (p->port_data_lock) {
	driver_pdl_unlock(p->port_data_lock);
    }
}

#endif

/*
 * exported driver_pdl_* functions ...
 */

ErlDrvPDL
driver_pdl_create(ErlDrvPort dp)
{
    ErlDrvPDL pdl;
    Port *pp = erts_drvport2port(dp);
    if (pp == ERTS_INVALID_ERL_DRV_PORT || pp->port_data_lock)
	return NULL;
    pdl = erts_alloc(ERTS_ALC_T_PORT_DATA_LOCK,
		     sizeof(struct erl_drv_port_data_lock));
    erts_mtx_init_x(&pdl->mtx, "port_data_lock", pp->common.id, 1);
    pdl_init_refc(pdl);
    erts_port_inc_refc(pp);
    pdl->prt = pp;
    pp->port_data_lock = pdl;
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_create(%T) -> 0x%08X\r\n",pp->common.id,(unsigned) pdl);
#endif
    return pdl;
}

void
driver_pdl_lock(ErlDrvPDL pdl)
{
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_lock(0x%08X)\r\n",(unsigned) pdl);
#endif
    pdl_inc_refc(pdl);
    erts_mtx_lock(&pdl->mtx);
}

void
driver_pdl_unlock(ErlDrvPDL pdl)
{
    ErlDrvSInt refc;
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_unlock(0x%08X)\r\n",(unsigned) pdl);
#endif
    erts_mtx_unlock(&pdl->mtx);
    refc = pdl_dectest_refc(pdl);
    if (!refc)
	pdl_destroy(pdl);
}

ErlDrvSInt
driver_pdl_get_refc(ErlDrvPDL pdl)
{
    return pdl_read_refc(pdl);
}

ErlDrvSInt
driver_pdl_inc_refc(ErlDrvPDL pdl)
{
    ErlDrvSInt refc = pdl_inctest_refc(pdl);
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_inc_refc(%p) -> %bed\r\n",
		 pdl, refc);
#endif
    return refc;
}

ErlDrvSInt
driver_pdl_dec_refc(ErlDrvPDL pdl)
{
    ErlDrvSInt refc = pdl_dectest_refc(pdl);
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_dec_refc(%p) -> %bpd\r\n",
		 pdl, refc);
#endif
    if (!refc)
	pdl_destroy(pdl);
    return refc;
}

/* expand queue to hold n elements in tail or head */
static int expandq(ErlIOQueue* q, int n, int tail)
/* tail: 0 if make room in head, make room in tail otherwise */
{
    int h_sz;  /* room before header */
    int t_sz;  /* room after tail */
    int q_sz;  /* occupied */
    int nvsz;
    SysIOVec* niov;
    ErlDrvBinary** nbinv;

    h_sz = q->v_head - q->v_start;
    t_sz = q->v_end -  q->v_tail;
    q_sz = q->v_tail - q->v_head;

    if (tail && (n <= t_sz)) /* do we need to expand tail? */
	return 0;
    else if (!tail && (n <= h_sz))  /* do we need to expand head? */
	return 0;
    else if (n > (h_sz + t_sz)) { /* need to allocate */
	/* we may get little extra but it ok */
	nvsz = (q->v_end - q->v_start) + n; 

	niov = erts_alloc_fnf(ERTS_ALC_T_IOQ, nvsz * sizeof(SysIOVec));
	if (!niov)
	    return -1;
	nbinv = erts_alloc_fnf(ERTS_ALC_T_IOQ, nvsz * sizeof(ErlDrvBinary**));
	if (!nbinv) {
	    erts_free(ERTS_ALC_T_IOQ, (void *) niov);
	    return -1;
	}
	if (tail) {
	    sys_memcpy(niov, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_head = q->v_start;
	    q->v_tail = q->v_head + q_sz;

	    sys_memcpy(nbinv, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	    if (q->b_start != q->b_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_head = q->b_start;
	    q->b_tail = q->b_head + q_sz;	
	}
	else {
	    sys_memcpy(niov+nvsz-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_tail = q->v_end;
	    q->v_head = q->v_tail - q_sz;
	    
	    sys_memcpy(nbinv+nvsz-q_sz, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	    if (q->b_start != q->b_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_tail = q->b_end;
	    q->b_head = q->b_tail - q_sz;
	}
    }
    else if (tail) {  /* move to beginning to make room in tail */
	sys_memmove(q->v_start, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_head = q->v_start;
	q->v_tail = q->v_head + q_sz;
	sys_memmove(q->b_start, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	q->b_head = q->b_start;
	q->b_tail = q->b_head + q_sz;
    }
    else {   /* move to end to make room */
	sys_memmove(q->v_end-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_tail = q->v_end;
	q->v_head = q->v_tail-q_sz;
	sys_memmove(q->b_end-q_sz, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	q->b_tail = q->b_end;
	q->b_head = q->b_tail-q_sz;
    }

    return 0;
}



/* Put elements from vec at q tail */
int driver_enqv(ErlDrvPort ix, ErlIOVec* vec, ErlDrvSizeT skip)
{
    int n;
    size_t len;
    ErlDrvSizeT size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    ErlDrvBinary*  b;
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL)
	return -1;

    ASSERT(vec->size >= skip);       /* debug only */
    if (vec->size <= skip)
	return 0;
    size = vec->size - skip;

    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;

    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base = ((char *)(iov->iov_base)) + skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while(skip > 0);

    if (q->v_tail + n >= q->v_end)
	expandq(q, n, 1);

    /* Queue and reference all binaries (remove zero length items) */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* speical case create binary ! */
		b = driver_alloc_binary(len);
		sys_memcpy(b->orig_bytes, iov->iov_base, len);
		*q->b_tail++ = b;
		q->v_tail->iov_len = len;
		q->v_tail->iov_base = b->orig_bytes;
		q->v_tail++;
	    }
	    else {
		driver_binary_inc_refc(b);
		*q->b_tail++ = b;
		*q->v_tail++ = *iov;
	    }
	}
	iov++;
	binv++;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}

/* Put elements from vec at q head */
int driver_pushqv(ErlDrvPort ix, ErlIOVec* vec, ErlDrvSizeT skip)
{
    int n;
    size_t len;
    ErlDrvSizeT size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    ErlDrvBinary* b;
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL)
	return -1;

    if (vec->size <= skip)
	return 0;
    size = vec->size - skip;

    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;

    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base = ((char *)(iov->iov_base)) + skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while(skip > 0);

    if (q->v_head - n < q->v_start)
	expandq(q, n, 0);

    /* Queue and reference all binaries (remove zero length items) */
    iov += (n-1);  /* move to end */
    binv += (n-1); /* move to end */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* speical case create binary ! */
		b = driver_alloc_binary(len);
		sys_memcpy(b->orig_bytes, iov->iov_base, len);
		*--q->b_head = b;
		q->v_head--;
		q->v_head->iov_len = len;
		q->v_head->iov_base = b->orig_bytes;
	    }
	    else {
		driver_binary_inc_refc(b);
		*--q->b_head = b;
		*--q->v_head = *iov;
	    }
	}
	iov--;
	binv--;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}


/*
** Remove size bytes from queue head
** Return number of bytes that remain in queue
*/
ErlDrvSizeT driver_deq(ErlDrvPort ix, ErlDrvSizeT size)
{
    ErlIOQueue* q = drvport2ioq(ix);
    ErlDrvSizeT len;

    if ((q == NULL) || (q->size < size))
	return -1;
    q->size -= size;
    while (size > 0) {
	ASSERT(q->v_head != q->v_tail);

	len = q->v_head->iov_len;
	if (len <= size) {
	    size -= len;
	    driver_free_binary(*q->b_head);
	    *q->b_head++ = NULL;
	    q->v_head++;
	}
	else {
	    q->v_head->iov_base = ((char *)(q->v_head->iov_base)) + size;
	    q->v_head->iov_len -= size;
	    size = 0;
	}
    }

    /* restart pointers (optimised for enq) */
    if (q->v_head == q->v_tail) {
	q->v_head = q->v_tail = q->v_start;
	q->b_head = q->b_tail = q->b_start;
    }
    return q->size;
}


ErlDrvSizeT driver_peekqv(ErlDrvPort ix, ErlIOVec *ev) {
    ErlIOQueue *q = drvport2ioq(ix);
    ASSERT(ev);

    if (! q) {
	return (ErlDrvSizeT) -1;
    } else {
	if ((ev->vsize = q->v_tail - q->v_head) == 0) {
	    ev->size = 0;
	    ev->iov = NULL;
	    ev->binv = NULL;
	} else {
	    ev->size = q->size;
	    ev->iov = q->v_head;
	    ev->binv = q->b_head;
	}
	return q->size;
    }
}

SysIOVec* driver_peekq(ErlDrvPort ix, int* vlenp)  /* length of io-vector */
{
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL) {
	*vlenp = -1;
	return NULL;
    }
    if ((*vlenp = (q->v_tail - q->v_head)) == 0)
	return NULL;
    return q->v_head;
}


ErlDrvSizeT driver_sizeq(ErlDrvPort ix)
{
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL)
	return (size_t) -1;
    return q->size;
}


/* Utils */

/* Enqueue a binary */
int driver_enq_bin(ErlDrvPort ix, ErlDrvBinary* bin,
		   ErlDrvSizeT offs, ErlDrvSizeT len)
{
    SysIOVec      iov;
    ErlIOVec      ev;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    iov.iov_base = bin->orig_bytes + offs;
    iov.iov_len  = len;
    ev.vsize = 1;
    ev.size = len;
    ev.iov = &iov;
    ev.binv = &bin;
    return driver_enqv(ix, &ev, 0);
}

int driver_enq(ErlDrvPort ix, char* buffer, ErlDrvSizeT len)
{
    int code;
    ErlDrvBinary* bin;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    if ((bin = driver_alloc_binary(len)) == NULL)
	return -1;
    sys_memcpy(bin->orig_bytes, buffer, len);
    code = driver_enq_bin(ix, bin, 0, len);
    driver_free_binary(bin);  /* dereference */
    return code;
}

int driver_pushq_bin(ErlDrvPort ix, ErlDrvBinary* bin,
		     ErlDrvSizeT offs, ErlDrvSizeT len)
{
    SysIOVec      iov;
    ErlIOVec      ev;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    iov.iov_base = bin->orig_bytes + offs;
    iov.iov_len  = len;
    ev.vsize = 1;
    ev.size = len;
    ev.iov = &iov;
    ev.binv = &bin;
    return driver_pushqv(ix, &ev, 0);
}

int driver_pushq(ErlDrvPort ix, char* buffer, ErlDrvSizeT len)
{
    int code;
    ErlDrvBinary* bin;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;

    if ((bin = driver_alloc_binary(len)) == NULL)
	return -1;
    sys_memcpy(bin->orig_bytes, buffer, len);
    code = driver_pushq_bin(ix, bin, 0, len);
    driver_free_binary(bin);  /* dereference */
    return code;
}

int driver_set_timer(ErlDrvPort ix, unsigned long t)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    if (prt->drv_ptr->timeout == NULL)
	return -1;

    erts_set_port_timer(prt, (Sint64) t);
    return 0;
}

int driver_cancel_timer(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    erts_cancel_port_timer(prt);
    return 0;
}

int
driver_read_timer(ErlDrvPort ix, unsigned long* t)
{
    Port* prt = erts_drvport2port(ix);
    Sint64 left;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    left = erts_read_port_timer(prt);
    if (left < 0)
	left = 0;

    *t = (unsigned long) left;

    return 0;
}

int 
driver_get_now(ErlDrvNowData *now_data)
{
    Uint mega,secs,micro;
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (now_data == NULL) {
	return -1;
    }
    get_now(&mega,&secs,&micro);
    now_data->megasecs = (unsigned long) mega;
    now_data->secs = (unsigned long) secs;
    now_data->microsecs = (unsigned long) micro;
    return 0;
}

ErlDrvTime
erl_drv_monotonic_time(ErlDrvTimeUnit time_unit)
{
    return (ErlDrvTime) erts_napi_monotonic_time((int) time_unit);
}

ErlDrvTime
erl_drv_time_offset(ErlDrvTimeUnit time_unit)
{
    return (ErlDrvTime) erts_napi_time_offset((int) time_unit);
}

ErlDrvTime
erl_drv_convert_time_unit(ErlDrvTime val,
			  ErlDrvTimeUnit from,
			  ErlDrvTimeUnit to)
{
    return (ErlDrvTime) erts_napi_convert_time_unit((ErtsMonotonicTime) val,
						    (int) from,
						    (int) to);
}

static void ref_to_driver_monitor(Eterm ref, ErlDrvMonitor *mon)
{
    RefThing *refp;
    ASSERT(is_internal_ref(ref));
    ERTS_CT_ASSERT(sizeof(RefThing) <= sizeof(ErlDrvMonitor));
    refp = ref_thing_ptr(ref);
    memset(mon,0,sizeof(ErlDrvMonitor));
    memcpy(mon,refp,sizeof(RefThing));
}


static int do_driver_monitor_process(Port *prt,
				     Eterm *buf,
				     ErlDrvTermData process,
				     ErlDrvMonitor *monitor)
{
    Process *rp;
    Eterm ref;

    if (prt->drv_ptr->process_exit == NULL) {
	return -1;
    }
    rp = erts_pid2proc_opt(NULL, 0,
			   (Eterm) process, ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);
    if (!rp) {
	return 1;
    }

    ref = erts_make_ref_in_buffer(buf);
    erts_add_monitor(&ERTS_P_MONITORS(prt), MON_ORIGIN, ref, rp->common.id, NIL);
    erts_add_monitor(&ERTS_P_MONITORS(rp), MON_TARGET, ref, prt->common.id, NIL);

    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    ref_to_driver_monitor(ref,monitor);
    return 0;
}

/*
 * This can be called from a non scheduler thread iff a port_data_lock exists
 */
int driver_monitor_process(ErlDrvPort drvport,
			   ErlDrvTermData process,
			   ErlDrvMonitor *monitor)
{
    Port *prt;
    int ret;
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsSchedulerData *sched = erts_get_scheduler_data();
#endif

    prt = DRV_MONITOR_LOOKUP_PORT_LOCK_PDL(drvport);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    /* Now (in SMP) we should have either the port lock (if we have a scheduler) or the port data lock
       (if we're a driver thread) */
    ERTS_SMP_LC_ASSERT((sched != NULL || prt->port_data_lock));
    {
	DeclareTmpHeapNoproc(buf,REF_THING_SIZE);
	UseTmpHeapNoproc(REF_THING_SIZE);
	ret = do_driver_monitor_process(prt,buf,process,monitor);
	UnUseTmpHeapNoproc(REF_THING_SIZE);
    }
    DRV_MONITOR_UNLOCK_PDL(prt);
    return ret;
}

static int do_driver_demonitor_process(Port *prt, Eterm *buf,
				       const ErlDrvMonitor *monitor)
{
    Process *rp;
    Eterm ref;
    ErtsMonitor *mon;
    Eterm to;

    memcpy(buf,monitor,sizeof(Eterm)*REF_THING_SIZE);
    ref = make_internal_ref(buf);
    mon = erts_lookup_monitor(ERTS_P_MONITORS(prt), ref);
    if (mon == NULL) {
	return 1;
    }
    ASSERT(mon->type == MON_ORIGIN);
    to = mon->pid;
    ASSERT(is_internal_pid(to));
    rp = erts_pid2proc_opt(NULL,
			   0,
			   to,
			   ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);
    mon = erts_remove_monitor(&ERTS_P_MONITORS(prt), ref);
    if (mon) {
	erts_destroy_monitor(mon);
    }
    if (rp) {
	ErtsMonitor *rmon;
	rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), ref);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	if (rmon != NULL) {
	    erts_destroy_monitor(rmon);
	}
    } 
    return 0;
}

int driver_demonitor_process(ErlDrvPort drvport,
			     const ErlDrvMonitor *monitor)
{
    Port *prt;
    int ret;
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsSchedulerData *sched = erts_get_scheduler_data();
#endif

    prt = DRV_MONITOR_LOOKUP_PORT_LOCK_PDL(drvport);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    /* Now we should have either the port lock (if we have a scheduler) or the port data lock
       (if we're a driver thread) */
    ERTS_SMP_LC_ASSERT((sched != NULL || prt->port_data_lock));
    {
	DeclareTmpHeapNoproc(buf,REF_THING_SIZE);
	UseTmpHeapNoproc(REF_THING_SIZE);
	ret = do_driver_demonitor_process(prt,buf,monitor);
	UnUseTmpHeapNoproc(REF_THING_SIZE);
    }
    DRV_MONITOR_UNLOCK_PDL(prt);
    return ret;
}

static ErlDrvTermData do_driver_get_monitored_process(Port *prt, Eterm *buf,
					    const ErlDrvMonitor *monitor)
{
    Eterm ref;
    ErtsMonitor *mon;
    Eterm to;

    memcpy(buf,monitor,sizeof(Eterm)*REF_THING_SIZE);
    ref = make_internal_ref(buf);
    mon = erts_lookup_monitor(ERTS_P_MONITORS(prt), ref);
    if (mon == NULL) {
	return driver_term_nil;
    }
    ASSERT(mon->type == MON_ORIGIN);
    to = mon->pid;
    ASSERT(is_internal_pid(to));
    return (ErlDrvTermData) to;
}


ErlDrvTermData driver_get_monitored_process(ErlDrvPort drvport,
					    const ErlDrvMonitor *monitor)
{
    Port *prt;
    ErlDrvTermData ret;
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsSchedulerData *sched = erts_get_scheduler_data();
#endif

    prt = DRV_MONITOR_LOOKUP_PORT_LOCK_PDL(drvport);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return driver_term_nil;

    /* Now we should have either the port lock (if we have a scheduler) or the port data lock
       (if we're a driver thread) */
    ERTS_SMP_LC_ASSERT((sched != NULL || prt->port_data_lock));
    {
	DeclareTmpHeapNoproc(buf,REF_THING_SIZE);
	UseTmpHeapNoproc(REF_THING_SIZE);
	ret = do_driver_get_monitored_process(prt,buf,monitor);
	UnUseTmpHeapNoproc(REF_THING_SIZE);
    }
    DRV_MONITOR_UNLOCK_PDL(prt);
    return ret;
}


int driver_compare_monitors(const ErlDrvMonitor *monitor1,
			    const ErlDrvMonitor *monitor2)
{
    return memcmp(monitor1,monitor2,sizeof(ErlDrvMonitor));
}

void erts_fire_port_monitor(Port *prt, Eterm ref)
{
    ErtsMonitor *rmon;
    void (*callback)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
    ErlDrvMonitor drv_monitor;
    int fpe_was_unmasked;
    ERTS_MSACC_PUSH_STATE_M();

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    ASSERT(prt->drv_ptr != NULL);    
    DRV_MONITOR_LOCK_PDL(prt);
    if (erts_lookup_monitor(ERTS_P_MONITORS(prt), ref) == NULL) {
	DRV_MONITOR_UNLOCK_PDL(prt);
	return;
    }
    callback = prt->drv_ptr->process_exit;
    ASSERT(callback != NULL);
    ref_to_driver_monitor(ref,&drv_monitor);
    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);
    DRV_MONITOR_UNLOCK_PDL(prt);
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(driver_process_exit)) {
        DTRACE_FORMAT_COMMON_PID_AND_PORT(ERTS_PORT_GET_CONNECTED(prt), prt)
        DTRACE3(driver_process_exit, process_str, port_str, prt->name);
    }
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
    if (LTTNG_ENABLED(driver_process_exit)) {
        lttng_decl_portbuf(port_str);
        lttng_decl_procbuf(proc_str);
        lttng_pid_to_str(ERTS_PORT_GET_CONNECTED(prt), proc_str);
        lttng_port_to_str(prt, port_str);
        LTTNG3(driver_process_exit, proc_str, port_str, prt->name);
    }
#endif
    fpe_was_unmasked = erts_block_fpe();
    (*callback)((ErlDrvData) (prt->drv_data), &drv_monitor);
    erts_unblock_fpe(fpe_was_unmasked);
    DRV_MONITOR_LOCK_PDL(prt);
    ERTS_MSACC_POP_STATE_M();
    /* remove monitor *after* callback */
    rmon = erts_remove_monitor(&ERTS_P_MONITORS(prt), ref);
    DRV_MONITOR_UNLOCK_PDL(prt);
    if (rmon) {
	erts_destroy_monitor(rmon);
    }
}


static int
driver_failure_term(ErlDrvPort ix, Eterm term, int eof)
{
    erts_aint32_t state;
    Port* prt = erts_drvport2port_state(ix, &state);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (prt->async_open_port)
        init_ack_send_reply(prt, prt->common.id);
    if (eof)
	flush_linebuf_messages(prt, state);
    if (state & ERTS_PORT_SFLG_CLOSING) {
	terminate_port(prt);
    } else if (eof && (state & ERTS_PORT_SFLG_SOFT_EOF)) {
	deliver_result(prt, prt->common.id, ERTS_PORT_GET_CONNECTED(prt), am_eof);
    } else {
	/* XXX UGLY WORK AROUND, Let erts_deliver_port_exit() terminate the port */
	if (prt->port_data_lock)
	    driver_pdl_lock(prt->port_data_lock);
	prt->ioq.size = 0;
	if (prt->port_data_lock)
	    driver_pdl_unlock(prt->port_data_lock);
	erts_deliver_port_exit(prt, prt->common.id, eof ? am_normal : term, 0, 0);
    }
    return 0;
}



/*
** Do a (soft) exit. unlink the connected process before doing
** driver posix error or (normal)
*/
int driver_exit(ErlDrvPort ix, int err)
{
    Port* prt = erts_drvport2port(ix);
    Process* rp;
    ErtsLink *lnk, *rlnk = NULL;
    Eterm connected;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
  
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
        return -1;

    connected = ERTS_PORT_GET_CONNECTED(prt);
    rp = erts_pid2proc(NULL, 0, connected, ERTS_PROC_LOCK_LINK);
    if (rp) {
	rlnk = erts_remove_link(&ERTS_P_LINKS(rp),prt->common.id);
    }

    lnk = erts_remove_link(&ERTS_P_LINKS(prt), connected);

#ifdef ERTS_SMP
    if (rp)
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
#endif

    if (rlnk != NULL) {
	erts_destroy_link(rlnk);
    }

    if (lnk != NULL) {
	erts_destroy_link(lnk);
    }

    if (err == 0)
        return driver_failure_term(ix, am_normal, 0);
    else {
        char* err_str = erl_errno_id(err);
        Eterm am_err = erts_atom_put((byte *) err_str, sys_strlen(err_str),
				     ERTS_ATOM_ENC_LATIN1, 1);
        return driver_failure_term(ix, am_err, 0);
    }
}


int driver_failure(ErlDrvPort ix, int code)
{
    return driver_failure_term(ix, make_small(code), code == 0);
}

int driver_failure_atom(ErlDrvPort ix, char* string)
{
    return driver_failure_term(ix,
			       erts_atom_put((byte *) string,
					     strlen(string),
					     ERTS_ATOM_ENC_LATIN1,
					     1),
			       0);
}

int driver_failure_posix(ErlDrvPort ix, int err)
{
    return driver_failure_atom(ix, erl_errno_id(err));
}

int driver_failure_eof(ErlDrvPort ix)
{
    return driver_failure_term(ix, NIL, 1);
}



ErlDrvTermData driver_mk_atom(char* string)
{
    Eterm am = erts_atom_put((byte *) string,
			     sys_strlen(string),
			     ERTS_ATOM_ENC_LATIN1,
			     1);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    return (ErlDrvTermData) am;
}

ErlDrvTermData driver_mk_port(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return (ErlDrvTermData) NIL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    return (ErlDrvTermData) prt->common.id;
}

ErlDrvTermData driver_connected(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return NIL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    return ERTS_PORT_GET_CONNECTED(prt);
}

ErlDrvTermData driver_caller(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return NIL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    return prt->caller;
}

int driver_lock_driver(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    DE_Handle* dh;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    erts_smp_rwmtx_rwlock(&erts_driver_list_lock);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if ((dh = (DE_Handle*)prt->drv_ptr->handle ) == NULL) {
	erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
	return -1;
    }
    erts_ddll_lock_driver(dh, prt->drv_ptr->name);
    erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
    return 0;
}


static int maybe_lock_driver_list(void) 
{
    void *rec_lock;
    rec_lock = erts_smp_tsd_get(driver_list_lock_status_key);
    if (rec_lock == 0) {
	erts_smp_rwmtx_rwlock(&erts_driver_list_lock);
	return 1;
    }
    return 0;
}
static void maybe_unlock_driver_list(int doit)
{
    if (doit) {
	erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
    }
}
/* 
   These old interfaces are certainly not MT friendly. Hopefully they are only used internally,
   but you never know, so they are kept for BC. As The sys ddll code has no notion
   of locking, I use the driver list lock to mutex this from the code in erl_bif_ddll.c.
   To allow dynamic code loading in the init functions of a driver, recursive locking is 
   handled as in add_driver_entry etc.
   A TSD variable holds the last error for a thread, so that code like
   ...
   x = driver_dl_open(...);
   if (x == NULL)
      y = driver_dl_error();
   ...
   works as long as execution happens in one driver callback even in an SMP emulator. 
   Writing code using these interfaces spanning several driver callbacks between loading/lookup 
   and error handling may give undesired results...
*/
void *driver_dl_open(char * path)
{
    void *ptr;
    int res;
    int *last_error_p = erts_smp_tsd_get(driver_list_last_error_key);
    int locked = maybe_lock_driver_list();
    if ((res = erts_sys_ddll_open(path, &ptr, NULL)) == 0) {
	maybe_unlock_driver_list(locked);
	return ptr;
    } else {
	if (!last_error_p) {
	    last_error_p = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, sizeof(int));
	    erts_smp_tsd_set(driver_list_last_error_key,last_error_p);
	}
	*last_error_p = res;
	maybe_unlock_driver_list(locked);
	return NULL;
    }
}

void *driver_dl_sym(void * handle, char *func_name)
{
    void *ptr;
    int res;
    int *last_error_p = erts_smp_tsd_get(driver_list_lock_status_key);
    int locked = maybe_lock_driver_list();
    if ((res = erts_sys_ddll_sym(handle, func_name, &ptr)) == 0) {
	maybe_unlock_driver_list(locked);
	return ptr;
    } else {
	if (!last_error_p) {
	    last_error_p = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, sizeof(int));
	    erts_smp_tsd_set(driver_list_lock_status_key,last_error_p);
	}
	*last_error_p = res;
	maybe_unlock_driver_list(locked);
	return NULL;
    }
}
    
int driver_dl_close(void *handle)
{
    int res;
    int locked = maybe_lock_driver_list();
    res = erts_sys_ddll_close(handle);
    maybe_unlock_driver_list(locked);
    return res;
}

char *driver_dl_error(void) 
{
    char *res;
    int *last_error_p = erts_smp_tsd_get(driver_list_lock_status_key);
    int locked = maybe_lock_driver_list();
    res = erts_ddll_error((last_error_p != NULL) ? (*last_error_p) : ERL_DE_ERROR_UNSPECIFIED); 
    maybe_unlock_driver_list(locked);
    return res;
}


#define ERL_DRV_SYS_INFO_SIZE(LAST_FIELD) \
  (offsetof(ErlDrvSysInfo, LAST_FIELD) \
   + sizeof(((ErlDrvSysInfo *) 0)->LAST_FIELD))

void
driver_system_info(ErlDrvSysInfo *sip, size_t si_size)
{
    /*
     * When adding fields in the ErlDrvSysInfo struct
     * remember to increment ERL_DRV_EXTENDED_MINOR_VERSION
     */

    /*
     * 'smp_support' is the last field in the first version
     * of ErlDrvSysInfo (introduced in driver version 1.0).
     */
    if (!sip || si_size < ERL_DRV_SYS_INFO_SIZE(smp_support)) 
	erts_exit(ERTS_ERROR_EXIT,
		 "driver_system_info(%p, %ld) called with invalid arguments\n",
		 sip, si_size);

    /*
     * 'smp_support' is the last field in the first version
     * of ErlDrvSysInfo (introduced in driver version 1.0).
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(smp_support)) {
	sip->driver_major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
	sip->driver_minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
	sip->erts_version = ERLANG_VERSION;
	sip->otp_release = ERLANG_OTP_RELEASE;
	sip->thread_support = 
#ifdef USE_THREADS
	    1
#else
	    0
#endif
	    ;
	sip->smp_support = 
#ifdef ERTS_SMP
	    1
#else
	    0
#endif
	    ;

    }

    /*
     * 'scheduler_threads' is the last field in the second version
     * of ErlDrvSysInfo (introduced in driver version 1.1).
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(scheduler_threads)) {
	sip->async_threads = erts_async_max_threads;
	sip->scheduler_threads = erts_no_schedulers;
    }
    /*
     * 'nif_minor_version' is the last field in the third version
     * (driver version 1.5, NIF version 1.0) 
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(nif_minor_version)) {
	sip->nif_major_version = ERL_NIF_MAJOR_VERSION;
	sip->nif_minor_version = ERL_NIF_MINOR_VERSION;
    }
    /*
     * 'dirty_scheduler_support' is the last field in the 4th version
     * (driver version 3.1, NIF version 2.7)
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(dirty_scheduler_support)) {
	sip->dirty_scheduler_support =
#ifdef ERTS_DIRTY_SCHEDULERS
	    1
#else
	    0
#endif
	    ;
    }

}


static ERTS_INLINE Port *
get_current_port(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ASSERT(esdp);
    ASSERT(esdp->current_port);
    return esdp->current_port;
}

/*
 * Default callbacks used if not supplied by driver.
 */

static void
no_output_callback(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{

}

static void
no_event_callback(ErlDrvData drv_data, ErlDrvEvent event, ErlDrvEventData event_data)
{
    Port *prt = get_current_port();
    report_missing_drv_callback(prt, "Event", "event()");
    driver_event(ERTS_Port2ErlDrvPort(prt), event, NULL);
}

static void
no_ready_input_callback(ErlDrvData drv_data, ErlDrvEvent event)
{
    Port *prt = get_current_port();
    report_missing_drv_callback(prt, "Input", "ready_input()");
    driver_select(ERTS_Port2ErlDrvPort(prt), event, 
		  (ERL_DRV_READ | ERL_DRV_USE_NO_CALLBACK), 0);
}

static void
no_ready_output_callback(ErlDrvData drv_data, ErlDrvEvent event)
{
    Port *prt = get_current_port();
    report_missing_drv_callback(prt, "Output", "ready_output()");
    driver_select(ERTS_Port2ErlDrvPort(prt), event,
		  (ERL_DRV_WRITE | ERL_DRV_USE_NO_CALLBACK), 0);
}

static void
no_timeout_callback(ErlDrvData drv_data)
{

}

static void
no_stop_select_callback(ErlDrvEvent event, void* private)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();    
    erts_dsprintf(dsbufp, "Driver does not implement stop_select callback "
			  "(event=%ld, private=%p)!\n", (long)event, private);
    erts_send_error_to_logger_nogl(dsbufp);
}

#define IS_DRIVER_VERSION_GE(DE,MAJOR,MINOR) \
    ((DE)->major_version >= (MAJOR) && (DE)->minor_version >= (MINOR))

static int
init_driver(erts_driver_t *drv, ErlDrvEntry *de, DE_Handle *handle)
{
    drv->name = de->driver_name;
    ASSERT(de->extended_marker == ERL_DRV_EXTENDED_MARKER);
    ASSERT(de->major_version >= 2);
    drv->version.major = de->major_version;
    drv->version.minor = de->minor_version;
    drv->flags = de->driver_flags;
    drv->handle = handle;
#ifdef ERTS_SMP
    if (drv->flags & ERL_DRV_FLAG_USE_PORT_LOCKING)
	drv->lock = NULL;
    else {
	drv->lock = erts_alloc(ERTS_ALC_T_DRIVER_LOCK,
			       sizeof(erts_mtx_t));
	erts_mtx_init_x(drv->lock,
			"driver_lock",
#if defined(ERTS_ENABLE_LOCK_CHECK) || defined(ERTS_ENABLE_LOCK_COUNT)
			    erts_atom_put((byte *) drv->name,
					  sys_strlen(drv->name),
					  ERTS_ATOM_ENC_LATIN1,
					  1),
#else
			NIL,
#endif
			1
	    );
    }
#endif
    drv->entry = de;

    drv->start = de->start;
    drv->stop = de->stop;
    drv->finish = de->finish;
    drv->flush = de->flush;
    drv->output = de->output ? de->output : no_output_callback;
    drv->outputv = de->outputv;
    drv->control = de->control;
    drv->call = de->call;
    drv->event = de->event ? de->event : no_event_callback;
    drv->ready_input = de->ready_input ? de->ready_input : no_ready_input_callback;
    drv->ready_output = de->ready_output ? de->ready_output : no_ready_output_callback;
    drv->timeout = de->timeout ? de->timeout : no_timeout_callback;
    drv->ready_async = de->ready_async;
    drv->process_exit = de->process_exit;
    drv->emergency_close = IS_DRIVER_VERSION_GE(de,3,2) ? de->emergency_close : NULL;
    if (de->stop_select)
	drv->stop_select = de->stop_select;
    else
	drv->stop_select = no_stop_select_callback;

    if (!de->init)
	return 0;
    else {
	int res;
	int fpe_was_unmasked = erts_block_fpe();
        DTRACE4(driver_init, drv->name, drv->version.major, drv->version.minor,
                drv->flags);
        LTTNG4(driver_init, drv->name, drv->version.major, drv->version.minor,
                drv->flags);
	res = (*de->init)();
	erts_unblock_fpe(fpe_was_unmasked);
	return res;
    }
}

#undef IS_DRIVER_VERSION_GE

void
erts_destroy_driver(erts_driver_t *drv)
{
#ifdef ERTS_SMP
    if (drv->lock) {
	erts_smp_mtx_destroy(drv->lock);
	erts_free(ERTS_ALC_T_DRIVER_LOCK, drv->lock);
    }
#endif    
    erts_free(ERTS_ALC_T_DRIVER, drv);
}

/* 
 * Functions for maintaining a list of driver_entry struct
 * Exposed in the driver interface, and therefore possibly locking directly.
 */

void add_driver_entry(ErlDrvEntry *drv){
    void *rec_lock;
    rec_lock = erts_smp_tsd_get(driver_list_lock_status_key);
    /* 
     * Ignore result of erts_add_driver_entry, the init is not
     * allowed to fail when drivers are added by drivers.
     */
    erts_add_driver_entry(drv, NULL, rec_lock != NULL); 
}

int erts_add_driver_entry(ErlDrvEntry *de, DE_Handle *handle, int driver_list_locked)
{
    erts_driver_t *dp = erts_alloc(ERTS_ALC_T_DRIVER, sizeof(erts_driver_t));
    int res;

    if (!driver_list_locked) {
	erts_smp_rwmtx_rwlock(&erts_driver_list_lock);
    }

    dp->next = driver_list;
    dp->prev = NULL;
    if (driver_list != NULL) {
	driver_list->prev = dp;
    }
    driver_list = dp;

    if (!driver_list_locked) {
	erts_smp_tsd_set(driver_list_lock_status_key, (void *) 1);
    }

    res = init_driver(dp, de, handle);

    if (res != 0) {
	/* 
	 * Remove it all again...
	 */
	driver_list = dp->next;
	if (driver_list != NULL) {
	    driver_list->prev = NULL;
	}
	erts_destroy_driver(dp);
    }
	
    if (!driver_list_locked) {
	erts_smp_tsd_set(driver_list_lock_status_key, NULL);
	erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
    }
    return res;
}

/* Not allowed for dynamic drivers */
int remove_driver_entry(ErlDrvEntry *drv)
{
    erts_driver_t *dp;
    void *rec_lock;
    
    rec_lock = erts_smp_tsd_get(driver_list_lock_status_key);
    if (rec_lock == NULL) {
	erts_smp_rwmtx_rwlock(&erts_driver_list_lock);
    }
    dp = driver_list;
    while (dp && dp->entry != drv)
	dp = dp->next;
    if (dp) {
	if (dp->handle) {
	    if (rec_lock == NULL) {
		erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
	    }
	    return -1;
	}
	if (dp->prev == NULL) {
	    driver_list = dp->next;
	} else {
	    dp->prev->next = dp->next;
	}
	if (dp->next != NULL) {
	    dp->next->prev = dp->prev;
	}
	erts_destroy_driver(dp);
	if (rec_lock == NULL) {
	    erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
	}
	return 1;
    }
    if (rec_lock == NULL) {
	erts_smp_rwmtx_rwunlock(&erts_driver_list_lock);
    }
    return 0;
}

/* very useful function that can be used in entries that are not used
 * so that not every driver writer must supply a personal version
 */
int null_func(void)
{
    return 0;
}

int
erl_drv_putenv(const char *key, char *value)
{
    return erts_sys_putenv_raw((char*)key, value);
}

int
erl_drv_getenv(const char *key, char *value, size_t *value_size)
{
    return erts_sys_getenv_raw((char*)key, value, value_size);
}

/* get heart_port
 * used by erl_crash_dump
 * - uses the fact that heart_port is registered when starting heart
 */

Port *erts_get_heart_port(void)
{
    int ix, max = erts_ptab_max(&erts_port);

    for (ix = 0; ix < max; ix++) {
	struct reg_proc *reg;
	Port *port = erts_pix2port(ix);

	if (!port)
	    continue;
	/* only examine undead or alive ports */
	if (erts_atomic32_read_nob(&port->state) & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	    continue;
	/* immediate atom compare */
	reg = port->common.u.alive.reg;
	if (reg && reg->name == am_heart_port) {
	    return port;
	}
    }

    return NULL;
}

void erts_emergency_close_ports(void)
{
    int ix, max = erts_ptab_max(&erts_port);

    for (ix = 0; ix < max; ix++) {
	Port *port = erts_pix2port(ix);

	if (!port)
	    continue;
	/* only examine undead or alive ports */
	if (erts_atomic32_read_nob(&port->state) & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	    continue;

	/* emergency close socket */
	if (port->drv_ptr->emergency_close) {
	    port->drv_ptr->emergency_close((ErlDrvData) port->drv_data);
	}
    }
}
