/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2012. All Rights Reserved.
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
 * Description:	Scheduling of port tasks
 *
 * Author: 	Rickard Green
 */

#define ERL_PORT_TASK_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "erl_port_task.h"
#include "dist.h"
#include "dtrace-wrapper.h"
#include <stdarg.h>

#if defined(DEBUG) && 0
#define HARD_DEBUG
#endif

/*
 * Costs in reductions for some port operations.
 */
#define ERTS_PORT_REDS_EXECUTE		10
#define ERTS_PORT_REDS_FREE		100
#define ERTS_PORT_REDS_TIMEOUT		400
#define ERTS_PORT_REDS_INPUT		400
#define ERTS_PORT_REDS_OUTPUT		400
#define ERTS_PORT_REDS_EVENT		400
#define ERTS_PORT_REDS_TERMINATE	200

#ifdef USE_VM_PROBES
#define DTRACE_DRIVER(PROBE_NAME, PP)                              \
    if (DTRACE_ENABLED(driver_ready_input)) {                      \
        DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);         \
        DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);            \
                                                                   \
        dtrace_pid_str(PP->connected, process_str);                \
        dtrace_port_str(PP, port_str);                             \
        DTRACE3(PROBE_NAME, process_str, port_str, PP->name);      \
    }
#else
#define  DTRACE_DRIVER(PROBE_NAME, PP) do {} while(0)
#endif

erts_smp_atomic_t erts_port_task_outstanding_io_tasks;

#define ERTS_PT_STATE_SCHEDULED		0
#define ERTS_PT_STATE_ABORTED		1
#define ERTS_PT_STATE_EXECUTING		2

struct ErtsPortTask_ {
    erts_smp_atomic32_t state;
    ErtsPortTaskType type;
    union {
	struct {
	    ErtsPortTask *next;
	    ErtsPortTaskHandle *handle;
	    union {
		struct { /* I/O tasks */
		    ErlDrvEvent event;
		    ErlDrvEventData event_data;
		} io;
	    } u;
	} alive;
	ErtsThrPrgrLaterOp release;
    } u;
};

static void begin_port_cleanup(Port *pp, ErtsPortTask **execq);

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(port_task,
				 ErtsPortTask,
				 1000,
				 ERTS_ALC_T_PORT_TASK)

#ifdef ERTS_SMP
static void
call_port_task_free(void *vptp)
{
    port_task_free((ErtsPortTask *) vptp);
}
#endif

static ERTS_INLINE void
schedule_port_task_free(ErtsPortTask *ptp)
{
#ifdef ERTS_SMP
    erts_schedule_thr_prgr_later_op(call_port_task_free,
				    (void *) ptp,
				    &ptp->u.release);
#else
    port_task_free(ptp);
#endif
}

/*
 * Task handle manipulation.
 */

static ERTS_INLINE void
reset_port_task_handle(ErtsPortTaskHandle *pthp)
{
    erts_smp_atomic_set_relb(pthp, (erts_aint_t) NULL);
}

static ERTS_INLINE ErtsPortTask *
handle2task(ErtsPortTaskHandle *pthp)
{
    return (ErtsPortTask *) erts_smp_atomic_read_acqb(pthp);
}

static ERTS_INLINE void
reset_handle(ErtsPortTask *ptp)
{
    if (ptp->u.alive.handle) {
	ASSERT(ptp == handle2task(ptp->u.alive.handle));
	reset_port_task_handle(ptp->u.alive.handle);
    }
}

static ERTS_INLINE void
set_handle(ErtsPortTask *ptp, ErtsPortTaskHandle *pthp)
{
    ptp->u.alive.handle = pthp;
    if (pthp) {
	erts_smp_atomic_set_relb(pthp, (erts_aint_t) ptp);
	ASSERT(ptp == handle2task(ptp->u.alive.handle));
    }
}

/*
 * Port queue operations
 */

static ERTS_INLINE void
enqueue_port(ErtsRunQueue *runq, Port *pp)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));
    pp->sched.next = NULL;
    if (runq->ports.end) {
	ASSERT(runq->ports.start);
	runq->ports.end->sched.next = pp;
    }
    else {
	ASSERT(!runq->ports.start);
	runq->ports.start = pp;
    }

    runq->ports.end = pp;
    ASSERT(runq->ports.start && runq->ports.end);

    erts_smp_inc_runq_len(runq, &runq->ports.info, ERTS_PORT_PRIO_LEVEL);
}

static ERTS_INLINE Port *
pop_port(ErtsRunQueue *runq)
{
    Port *pp = runq->ports.start;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));
    if (!pp) {
	ASSERT(!runq->ports.end);
    }
    else {
	runq->ports.start = runq->ports.start->sched.next;
	if (!runq->ports.start) {
	    ASSERT(runq->ports.end == pp);
	    runq->ports.end = NULL;
	}
	erts_smp_dec_runq_len(runq, &runq->ports.info, ERTS_PORT_PRIO_LEVEL);
    }

    ASSERT(runq->ports.start || !runq->ports.end);
    ASSERT(runq->ports.end || !runq->ports.start);
    return pp;
}

/*
 * Task queue operations
 */

static ERTS_INLINE erts_aint32_t
enqueue_task(Port *pp, ErtsPortTask *ptp)
{
    erts_aint32_t flags;
    ptp->u.alive.next = NULL;
    erts_port_task_sched_lock(&pp->sched);
    flags = erts_smp_atomic32_read_nob(&pp->sched.flags);
    if (!(flags & ERTS_PTS_FLG_EXIT)) {
	if (pp->sched.taskq.in.last) {
	    ASSERT(pp->sched.taskq.in.first);
	    ASSERT(!pp->sched.taskq.in.last->u.alive.next);

	    pp->sched.taskq.in.last->u.alive.next = ptp;
	}
	else {
	    ASSERT(!pp->sched.taskq.in.first);

	    pp->sched.taskq.in.first = ptp;
	}
	pp->sched.taskq.in.last = ptp;
    }
    erts_port_task_sched_unlock(&pp->sched);
    return flags;
}

static ERTS_INLINE ErtsPortTask *
pop_task(Port *pp, ErtsPortTask **execqp)
{
    ErtsPortTask *ptp;

    ptp = *execqp;
    if (ptp) {
	*execqp = ptp->u.alive.next;
	return ptp;
    }

    ASSERT(!pp->sched.taskq.local);

    erts_port_task_sched_lock(&pp->sched);
    ptp = pp->sched.taskq.in.first;
    pp->sched.taskq.in.first = NULL;
    pp->sched.taskq.in.last = NULL;
    if (ptp)
	*execqp = ptp->u.alive.next;
    else
	erts_smp_atomic32_read_band_nob(&pp->sched.flags,
					~ERTS_PTS_FLG_HAVE_TASKS);
    erts_port_task_sched_unlock(&pp->sched);


    return ptp;
}

static ERTS_INLINE void
prepare_exec(Port *pp, ErtsPortTask **execqp)
{
    erts_aint32_t act;
    *execqp = pp->sched.taskq.local;

    /* guess a likely value */
    act = ERTS_PTS_FLG_HAVE_TASKS|ERTS_PTS_FLG_IN_RUNQ;

    while (1) {
	erts_aint32_t new, exp;

	new = exp = act;

	new &= ~ERTS_PTS_FLG_IN_RUNQ;
	new |= ERTS_PTS_FLG_EXEC;

	act = erts_smp_atomic32_cmpxchg_nob(&pp->sched.flags, new, exp);

	ASSERT(act & ERTS_PTS_FLG_IN_RUNQ);

	if (exp == act)
	    break;
    }
}

/* finalize_exec() return value != 0 if port should remain active */
static ERTS_INLINE int
finalize_exec(Port *pp, ErtsPortTask **execq)
{
    erts_aint32_t act;

    pp->sched.taskq.local = *execq;
    *execq = NULL;

    /* guess a likely value */
    act = ERTS_PTS_FLG_EXEC;
    if (execq)
	act |= ERTS_PTS_FLG_HAVE_TASKS;

    while (1) {
	erts_aint32_t new, exp;

	new = exp = act;

	new &= ~ERTS_PTS_FLG_EXEC;
	if (act & ERTS_PTS_FLG_HAVE_TASKS)
	    new |= ERTS_PTS_FLG_IN_RUNQ;

	act = erts_smp_atomic32_cmpxchg_relb(&pp->sched.flags, new, exp);

	ASSERT(!(act & ERTS_PTS_FLG_IN_RUNQ));

	if (exp == act)
	    break;
    }

    return (act & ERTS_PTS_FLG_HAVE_TASKS) != 0;
}

/*
 * Abort a scheduled task.
 */

int
erts_port_task_abort(ErtsPortTaskHandle *pthp)
{
    int res;
    ErtsPortTask *ptp;
#ifdef ERTS_SMP
    ErtsThrPrgrDelayHandle dhndl = erts_thr_progress_unmanaged_delay();
#endif

    ptp = handle2task(pthp);
    if (!ptp)
	res = -1;
    else {
	erts_aint32_t old_state;

#ifdef DEBUG
	ErtsPortTaskHandle *saved_pthp = ptp->u.alive.handle;
	ERTS_SMP_READ_MEMORY_BARRIER;
	old_state = erts_smp_atomic32_read_nob(&ptp->state);
	if (old_state == ERTS_PT_STATE_SCHEDULED) {
	    ASSERT(saved_pthp == pthp);
	}
#endif

	old_state = erts_smp_atomic32_cmpxchg_nob(&ptp->state,
						  ERTS_PT_STATE_ABORTED,
						  ERTS_PT_STATE_SCHEDULED);
	if (old_state != ERTS_PT_STATE_SCHEDULED)
	    res = - 1; /* Task already aborted, executing, or executed */
	else {

	    reset_port_task_handle(pthp);

	    switch (ptp->type) {
	    case ERTS_PORT_TASK_INPUT:
	    case ERTS_PORT_TASK_OUTPUT:
	    case ERTS_PORT_TASK_EVENT:
		ASSERT(erts_smp_atomic_read_nob(
			   &erts_port_task_outstanding_io_tasks) > 0);
		erts_smp_atomic_dec_relb(&erts_port_task_outstanding_io_tasks);
		break;
	    default:
		break;
	    }

	    res = 0;
	}
    }

#ifdef ERTS_SMP
    erts_thr_progress_unmanaged_continue(dhndl);
#endif

    return res;
}

/*
 * Schedule a task.
 */

int
erts_port_task_schedule(Eterm id,
			ErtsPortTaskHandle *pthp,
			ErtsPortTaskType type,
			...)
{
#ifdef ERTS_SMP
    ErtsRunQueue *xrunq;
    ErtsThrPrgrDelayHandle dhndl;
#endif
    ErtsRunQueue *runq;
    Port *pp;
    ErtsPortTask *ptp = NULL;
    erts_aint32_t act;

    if (pthp && erts_port_task_is_scheduled(pthp)) {
	ASSERT(0);
	erts_port_task_abort(pthp);
    }

    ASSERT(is_internal_port(id));

#ifdef ERTS_SMP
    dhndl = erts_thr_progress_unmanaged_delay();
#endif

    pp = erts_port_lookup_raw(id);

#ifdef ERTS_SMP
    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED) {
	if (pp)
	    erts_port_inc_refc(pp);
	erts_thr_progress_unmanaged_continue(dhndl);
    }
#endif

    if (!pp)
	goto fail;

    ptp = port_task_alloc();

    ptp->type = type;

    erts_smp_atomic32_init_nob(&ptp->state, ERTS_PT_STATE_SCHEDULED);

    switch (type) {
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT: {
	va_list argp;
	va_start(argp, type);
	ptp->u.alive.u.io.event = va_arg(argp, ErlDrvEvent);
	va_end(argp);
	erts_smp_atomic_inc_relb(&erts_port_task_outstanding_io_tasks);
	break;
    }
    case ERTS_PORT_TASK_EVENT: {
	va_list argp;
	va_start(argp, type);
	ptp->u.alive.u.io.event = va_arg(argp, ErlDrvEvent);
	ptp->u.alive.u.io.event_data = va_arg(argp, ErlDrvEventData);
	va_end(argp);
	erts_smp_atomic_inc_relb(&erts_port_task_outstanding_io_tasks);
	break;
    }
    default:
	break;
    }

    set_handle(ptp, pthp);

    act = enqueue_task(pp, ptp);
    if (act & ERTS_PTS_FLG_EXIT) {
	reset_handle(ptp);
	goto fail;
    }

    while (1) {
	erts_aint32_t new, exp;

	if ((act & ERTS_PTS_FLG_HAVE_TASKS)
	    && (act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	    goto done; /* Done */

	new = exp = act;
	new |= ERTS_PTS_FLG_HAVE_TASKS;
	if (!(act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	    new |= ERTS_PTS_FLG_IN_RUNQ;

	act = erts_smp_atomic32_cmpxchg_relb(&pp->sched.flags, new, exp);

	if (exp == act) {
	    if (!(act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
		break; /* Need to enqueue port */
	    goto done; /* Done */
	}

	if (act & ERTS_PTS_FLG_EXIT)
	    goto done; /* Died after our task insert... */
    }

    /* Enqueue port on run-queue */

    runq = erts_port_runq(pp);
    if (!runq)
	ERTS_INTERNAL_ERROR("Missing run-queue");

#ifdef ERTS_SMP
    xrunq = erts_check_emigration_need(runq, ERTS_PORT_PRIO_LEVEL);
    if (xrunq) {
	/* Port emigrated ... */
	erts_smp_atomic_set_nob(&pp->run_queue, (erts_aint_t) xrunq);
	erts_smp_runq_unlock(runq);
	runq = erts_port_runq(pp);
	if (!runq)
	    ERTS_INTERNAL_ERROR("Missing run-queue");
    }
#endif

    enqueue_port(runq, pp);
	    
    if (erts_system_profile_flags.runnable_ports) {
	profile_runnable_port(pp, am_active);
    }

    erts_smp_runq_unlock(runq);

    erts_smp_notify_inc_runq(runq);

done:

#ifdef ERTS_SMP
    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	erts_port_dec_refc(pp);
#endif

    return 0;

fail:

#ifdef ERTS_SMP
    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	erts_port_dec_refc(pp);
#endif

    if (ptp)
	port_task_free(ptp);

    return -1;
}

void
erts_port_task_free_port(Port *pp)
{
    erts_aint32_t flags;
    ErtsRunQueue *runq;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    ASSERT(!(erts_atomic32_read_nob(&pp->state) & ERTS_PORT_SFLGS_DEAD));

    runq = erts_port_runq(pp);
    if (!runq)
	ERTS_INTERNAL_ERROR("Missing run-queue");
    erts_port_task_sched_lock(&pp->sched);
    flags = erts_smp_atomic32_read_bor_relb(&pp->sched.flags,
					    ERTS_PTS_FLG_EXIT);
    erts_port_task_sched_unlock(&pp->sched);
    erts_atomic32_read_bset_relb(&pp->state,
				 (ERTS_PORT_SFLG_CLOSING
				  | ERTS_PORT_SFLG_FREE),
				 ERTS_PORT_SFLG_FREE);

    erts_smp_runq_unlock(runq);

    if (!(flags & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	begin_port_cleanup(pp, NULL);
}

/*
 * Execute scheduled tasks of a port.
 *
 * erts_port_task_execute() is called by scheduler threads between
 * scheduling of processes. Run-queue lock should be held by caller.
 */

int
erts_port_task_execute(ErtsRunQueue *runq, Port **curr_port_pp)
{
    Port *pp;
    ErtsPortTask *execq;
    int res = 0;
    int reds = ERTS_PORT_REDS_EXECUTE;
    erts_aint_t io_tasks_executed = 0;
    int fpe_was_unmasked;
    erts_aint32_t state;
    int active;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    pp = pop_port(runq);
    if (!pp) {
	res = 0;
	goto done;
    }

    erts_smp_runq_unlock(runq);

    *curr_port_pp = pp;
    
    if (erts_sched_stat.enabled) {
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
	Uint old = ERTS_PORT_SCHED_ID(pp, esdp->no);
	int migrated = old && old != esdp->no;

	erts_smp_spin_lock(&erts_sched_stat.lock);
	erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].total_executed++;
	erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].executed++;
	if (migrated) {
	    erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].total_migrated++;
	    erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].migrated++;
	}
	erts_smp_spin_unlock(&erts_sched_stat.lock);
    }

    prepare_exec(pp, &execq);

    erts_smp_port_lock(pp);

    /* trace port scheduling, in */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports(pp, am_in);
    }

    fpe_was_unmasked = erts_block_fpe();

    state = erts_atomic_read_nob(&pp->state);
    goto begin_handle_tasks;

    while (1) {
	erts_aint32_t task_state;
	ErtsPortTask *ptp;

	ptp = pop_task(pp, &execq);
	if (!ptp)
	    break;

	task_state = erts_smp_atomic32_cmpxchg_nob(&ptp->state,
						   ERTS_PT_STATE_EXECUTING,
						   ERTS_PT_STATE_SCHEDULED);
	if (task_state != ERTS_PT_STATE_SCHEDULED) {
	    ASSERT(task_state == ERTS_PT_STATE_ABORTED);
	    goto aborted_port_task;
	}

	reset_handle(ptp);

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
	ERTS_SMP_CHK_NO_PROC_LOCKS;
	ASSERT(pp->drv_ptr);

	switch (ptp->type) {
	case ERTS_PORT_TASK_TIMEOUT:
	    reds += ERTS_PORT_REDS_TIMEOUT;
	    if (!(erts_atomic32_read_nob(&pp->state) & ERTS_PORT_SFLGS_DEAD)) {
                DTRACE_DRIVER(driver_timeout, pp);
		(*pp->drv_ptr->timeout)((ErlDrvData) pp->drv_data);
            }
	    break;
	case ERTS_PORT_TASK_INPUT:
	    reds += ERTS_PORT_REDS_INPUT;
	    ASSERT((erts_atomic32_read_nob(&pp->state)
		    & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_ready_input, pp);
	    /* NOTE some windows drivers use ->ready_input for input and output */
	    (*pp->drv_ptr->ready_input)((ErlDrvData) pp->drv_data,
					ptp->u.alive.u.io.event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_OUTPUT:
	    reds += ERTS_PORT_REDS_OUTPUT;
	    ASSERT((erts_atomic32_read_nob(&pp->state)
		    & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_ready_output, pp);
	    (*pp->drv_ptr->ready_output)((ErlDrvData) pp->drv_data,
					 ptp->u.alive.u.io.event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_EVENT:
	    reds += ERTS_PORT_REDS_EVENT;
	    ASSERT((erts_atomic32_read_nob(&pp->state)
		    & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_event, pp);
	    (*pp->drv_ptr->event)((ErlDrvData) pp->drv_data,
				  ptp->u.alive.u.io.event,
				  ptp->u.alive.u.io.event_data);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_DIST_CMD:
	    reds += erts_dist_command(pp, CONTEXT_REDS-reds);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT,
		     "Invalid port task type: %d\n",
		     (int) ptp->type);
	    break;
	}

	state = erts_atomic32_read_nob(&pp->state);
	if ((state & ERTS_PORT_SFLG_CLOSING) && erts_is_port_ioq_empty(pp)) {
	    reds += ERTS_PORT_REDS_TERMINATE;
	    erts_terminate_port(pp);
	    state = erts_atomic32_read_nob(&pp->state);
	}

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

#ifdef ERTS_SMP
	if (pp->xports)
	    erts_smp_xports_unlock(pp);
	ASSERT(!pp->xports);
#endif

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

    aborted_port_task:
	schedule_port_task_free(ptp);

    begin_handle_tasks:
	if (state & ERTS_PORT_SFLG_FREE) {
	    reds += ERTS_PORT_REDS_FREE;

	    begin_port_cleanup(pp, &execq);

	    break;
	}

	if (reds >= CONTEXT_REDS)
	    break;
    }

    erts_unblock_fpe(fpe_was_unmasked);

    /* trace port scheduling, out */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports(pp, am_out);
    }

    if (io_tasks_executed) {
	ASSERT(erts_smp_atomic_read_nob(&erts_port_task_outstanding_io_tasks)
	       >= io_tasks_executed);
	erts_smp_atomic_add_relb(&erts_port_task_outstanding_io_tasks,
				 -1*io_tasks_executed);
    }

#ifdef ERTS_SMP
    ASSERT(runq == (ErtsRunQueue *) erts_smp_atomic_read_nob(&pp->run_queue));
#endif

    active = finalize_exec(pp, &execq);

    erts_port_release(pp);

    *curr_port_pp = NULL;

    erts_smp_runq_lock(runq);
 
    if (!active) {
	if (erts_system_profile_flags.runnable_ports)
	    profile_runnable_port(pp, am_inactive);
    }
    else {
#ifdef ERTS_SMP
	ErtsRunQueue *xrunq;
#endif

	ASSERT(!(erts_atomic32_read_nob(&pp->state) & ERTS_PORT_SFLGS_DEAD));

#ifdef ERTS_SMP
	xrunq = erts_check_emigration_need(runq, ERTS_PORT_PRIO_LEVEL);
	if (!xrunq) {
#endif
	    enqueue_port(runq, pp);
	    /* No need to notify ourselves about inc in runq. */
#ifdef ERTS_SMP
	}
	else {
	    /* Port emigrated ... */
	    erts_smp_atomic_set_nob(&pp->run_queue, (erts_aint_t) xrunq);
	    erts_smp_runq_unlock(runq);

	    xrunq = erts_port_runq(pp);
	    ASSERT(xrunq);
	    enqueue_port(xrunq, pp);
	    erts_smp_runq_unlock(xrunq);
	    erts_smp_notify_inc_runq(xrunq);

	    erts_smp_runq_lock(runq);
	}
#endif
    }

 done:
    res = (erts_smp_atomic_read_nob(&erts_port_task_outstanding_io_tasks)
	   != (erts_aint_t) 0);

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    ERTS_PORT_REDUCTIONS_EXECUTED(runq, reds);

    return res;
}

#ifdef ERTS_SMP
static void
release_port(void *vport)
{
    erts_port_dec_refc((Port *) vport);
}
#endif

static void
begin_port_cleanup(Port *pp, ErtsPortTask **execqp)
{
    int i, max;
    ErtsPortTask *qs[2];

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));


    /*
     * Handle remaining tasks...
     */

    max = 0;
    if (execqp && *execqp) {
	qs[max++] = *execqp;
	*execqp = NULL;
    }

    erts_port_task_sched_lock(&pp->sched);
    qs[max] = pp->sched.taskq.in.first;
    pp->sched.taskq.in.first = NULL;
    pp->sched.taskq.in.last = NULL;
    erts_port_task_sched_unlock(&pp->sched);
    if (qs[max])
	max++;

    for (i = 0; i < max; i++) {
	while (1) {
	    erts_aint32_t state;
	    ErtsPortTask *ptp = qs[i];
	    if (!ptp)
		break;

	    qs[i] = ptp->u.alive.next;

	    /* Normal case here is aborted tasks... */
	    state = erts_smp_atomic32_read_nob(&ptp->state);
	    if (state == ERTS_PT_STATE_ABORTED)
		goto aborted_port_task;

	    state = erts_smp_atomic32_cmpxchg_nob(&ptp->state,
						  ERTS_PT_STATE_EXECUTING,
						  ERTS_PT_STATE_SCHEDULED);
	    if (state != ERTS_PT_STATE_SCHEDULED) {
		ASSERT(state == ERTS_PT_STATE_ABORTED);
		goto aborted_port_task;
	    }

	    reset_handle(ptp);

	    switch (ptp->type) {
	    case ERTS_PORT_TASK_TIMEOUT:
		break;
	    case ERTS_PORT_TASK_INPUT:
		erts_stale_drv_select(pp->common.id,
				      ptp->u.alive.u.io.event,
				      DO_READ,
				      1);
		break;
	    case ERTS_PORT_TASK_OUTPUT:
		erts_stale_drv_select(pp->common.id,
				      ptp->u.alive.u.io.event,
				      DO_WRITE,
				      1);
		break;
	    case ERTS_PORT_TASK_EVENT:
		erts_stale_drv_select(pp->common.id,
				      ptp->u.alive.u.io.event,
				      0,
				      1);
		break;
	    case ERTS_PORT_TASK_DIST_CMD:
		break;
	    default:
		erl_exit(ERTS_ABORT_EXIT,
			 "Invalid port task type: %d\n",
			 (int) ptp->type);
	    }

	aborted_port_task:
	    schedule_port_task_free(ptp);
	}
    }

    erts_smp_atomic32_read_band_nob(&pp->sched.flags,
				    ~ERTS_PTS_FLG_HAVE_TASKS);

    /*
     * Schedule cleanup of port structure...
     */
#ifdef ERTS_SMP
    erts_schedule_thr_prgr_later_op(release_port,
				    (void *) pp,
				    &pp->common.u.release);
#else
    pp->cleanup = 1;
#endif
}

int
erts_port_is_scheduled(Port *pp)
{
    erts_aint32_t flags = erts_smp_atomic32_read_acqb(&pp->sched.flags);
    return (flags & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)) != 0;
}

#ifdef ERTS_SMP

void
erts_enqueue_port(ErtsRunQueue *rq, Port *pp)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    ASSERT(rq == (ErtsRunQueue *) erts_smp_atomic_read_nob(&pp->run_queue));
    ASSERT(erts_smp_atomic32_read_nob(&pp->sched.flags) & ERTS_PTS_FLG_IN_RUNQ);
    enqueue_port(rq, pp);
}

Port *
erts_dequeue_port(ErtsRunQueue *rq)
{
    Port *pp;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    pp = pop_port(rq);
    ASSERT(!pp
	   || rq == (ErtsRunQueue *) erts_smp_atomic_read_nob(&pp->run_queue));
    ASSERT(!pp || (erts_smp_atomic32_read_nob(&pp->sched.flags)
		   & ERTS_PTS_FLG_IN_RUNQ));
    return pp;
}

#endif

/*
 * Initialize the module.
 */
void
erts_port_task_init(void)
{
    erts_smp_atomic_init_nob(&erts_port_task_outstanding_io_tasks,
			     (erts_aint_t) 0);
    init_port_task_alloc();
}
