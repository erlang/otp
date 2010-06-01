/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2006-2010. All Rights Reserved.
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

#if defined(DEBUG) && 0
#define HARD_DEBUG
#endif

/*
 * Costs in reductions for some port operations.
 */
#define ERTS_PORT_REDS_EXECUTE		0
#define ERTS_PORT_REDS_FREE		50
#define ERTS_PORT_REDS_TIMEOUT		200
#define ERTS_PORT_REDS_INPUT		200
#define ERTS_PORT_REDS_OUTPUT		200
#define ERTS_PORT_REDS_EVENT		200
#define ERTS_PORT_REDS_TERMINATE	100


#define ERTS_PORT_TASK_INVALID_PORT(P, ID) \
  ((erts_port_status_get((P)) & ERTS_PORT_SFLGS_DEAD) || (P)->id != (ID))

#define ERTS_PORT_IS_IN_RUNQ(RQ, P) \
  ((P)->sched.next || (P)->sched.prev || (RQ)->ports.start == (P))

#define ERTS_PORT_NOT_IN_RUNQ(P)	\
do {					\
    (P)->sched.prev = NULL;		\
    (P)->sched.next = NULL;		\
} while (0)

erts_smp_atomic_t erts_port_task_outstanding_io_tasks;

struct ErtsPortTaskQueue_ {
    ErtsPortTask *first;
    ErtsPortTask *last;
    Port *port;
};

struct ErtsPortTask_ {
    ErtsPortTask *prev;
    ErtsPortTask *next;
    ErtsPortTaskQueue *queue;
    ErtsPortTaskHandle *handle;
    ErtsPortTaskType type;
    ErlDrvEvent event;
    ErlDrvEventData event_data;
};

#ifdef HARD_DEBUG
#define ERTS_PT_CHK_PORTQ(RQ) check_port_queue((RQ), NULL, 0)
#define ERTS_PT_CHK_PRES_PORTQ(RQ, PP) check_port_queue((RQ), (PP), -1)
#define ERTS_PT_CHK_IN_PORTQ(RQ, PP) check_port_queue((RQ), (PP), 1)
#define ERTS_PT_CHK_NOT_IN_PORTQ(RQ, PP) check_port_queue((RQ), (PP), 0)
#define ERTS_PT_CHK_TASKQ(Q) check_task_queue((Q), NULL, 0)
#define ERTS_PT_CHK_IN_TASKQ(Q, T) check_task_queue((Q), (T), 1)
#define ERTS_PT_CHK_NOT_IN_TASKQ(Q, T) check_task_queue((Q), (T), 0)
static void
check_port_queue(Port *chk_pp, int inq);
static void
check_task_queue(ErtsPortTaskQueue *ptqp,
		 ErtsPortTask *chk_ptp,
		 int inq);
#else
#define ERTS_PT_CHK_PORTQ(RQ)
#define ERTS_PT_CHK_PRES_PORTQ(RQ, PP)
#define ERTS_PT_CHK_IN_PORTQ(RQ, PP)
#define ERTS_PT_CHK_NOT_IN_PORTQ(RQ, PP)
#define ERTS_PT_CHK_TASKQ(Q)
#define ERTS_PT_CHK_IN_TASKQ(Q, T)
#define ERTS_PT_CHK_NOT_IN_TASKQ(Q, T)
#endif

static void handle_remaining_tasks(ErtsRunQueue *runq, Port *pp);

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(port_task,
				 ErtsPortTask,
				 200,
				 ERTS_ALC_T_PORT_TASK)
ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(port_taskq,
				 ErtsPortTaskQueue,
				 50,
				 ERTS_ALC_T_PORT_TASKQ)

/*
 * Task handle manipulation.
 */

static ERTS_INLINE ErtsPortTask *
handle2task(ErtsPortTaskHandle *pthp)
{
    return (ErtsPortTask *) erts_smp_atomic_read(pthp);
}

static ERTS_INLINE void
reset_handle(ErtsPortTask *ptp)
{
    if (ptp->handle) {
	ASSERT(ptp == handle2task(ptp->handle));
	erts_smp_atomic_set(ptp->handle, (long) NULL);
    }
}

static ERTS_INLINE void
set_handle(ErtsPortTask *ptp, ErtsPortTaskHandle *pthp)
{
    ptp->handle = pthp;
    if (pthp) {
	erts_smp_atomic_set(pthp, (long) ptp);
	ASSERT(ptp == handle2task(ptp->handle));
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
    pp->sched.prev = runq->ports.end;
    if (runq->ports.end) {
	ASSERT(runq->ports.start);
	runq->ports.end->sched.next = pp;
    }
    else {
	ASSERT(!runq->ports.start);
	runq->ports.start = pp;
    }

    runq->ports.info.len++;
    if (runq->ports.info.max_len < runq->ports.info.len)
	runq->ports.info.max_len = runq->ports.info.len;
    runq->len++;
    if (runq->max_len < runq->len)
	runq->max_len = runq->len;
    runq->ports.end = pp;
    ASSERT(runq->ports.start && runq->ports.end);
}

static ERTS_INLINE void
dequeue_port(ErtsRunQueue *runq, Port *pp)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));
    if (pp->sched.next)
	pp->sched.next->sched.prev = pp->sched.prev;
    else {
	ASSERT(runq->ports.end == pp);
	runq->ports.end = pp->sched.prev;
    }
    if (pp->sched.prev)
	pp->sched.prev->sched.next = pp->sched.next;
    else {
	ASSERT(runq->ports.start == pp);
	runq->ports.start = pp->sched.next;
    }

    ASSERT(runq->ports.info.len > 0);
    runq->ports.info.len--;
    ASSERT(runq->len > 0);
    runq->len--;
    ASSERT(runq->ports.start || !runq->ports.end);
    ASSERT(runq->ports.end || !runq->ports.start);
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
	if (runq->ports.start)
	    runq->ports.start->sched.prev = NULL;
	else {
	    ASSERT(runq->ports.end == pp);
	    runq->ports.end = NULL;
	}
	ASSERT(runq->ports.info.len > 0);
	runq->ports.info.len--;
	ASSERT(runq->len > 0);
	runq->len--;
    }

    ASSERT(runq->ports.start || !runq->ports.end);
    ASSERT(runq->ports.end || !runq->ports.start);
    return pp;
}


#ifdef HARD_DEBUG

static void
check_port_queue(ErtsRunQueue *runq, Port *chk_pp, int inq)
{
    Port *pp;
    Port *last_pp;
    Port *first_pp = runq->ports.start;
    int no_forward = 0, no_backward = 0;
    int found_forward = 0, found_backward = 0;
    if (!first_pp) {
	ASSERT(!runq->ports.end);
    }
    else {
	ASSERT(!first_pp->sched.prev);
	for (pp = first_pp; pp; pp = pp->sched.next) {
	    ASSERT(pp->sched.taskq);
	    if (pp->sched.taskq->first)
		no_forward++;
	    if (chk_pp == pp)
		found_forward = 1;
	    if (!pp->sched.prev) {
		ASSERT(first_pp == pp);
	    }
	    if (!pp->sched.next) {
		ASSERT(runq->ports.end == pp);
		last_pp = pp;
	    }
	}
	for (pp = last_pp; pp; pp = pp->sched.prev) {
	    ASSERT(pp->sched.taskq);
	    if (pp->sched.taskq->last)
		no_backward++;
	    if (chk_pp == pp)
		found_backward = 1;
	    if (!pp->sched.prev) {
		ASSERT(first_pp == pp);
	    }
	    if (!pp->sched.next) {
		ASSERT(runq->ports.end == pp);
	    }
	    check_task_queue(pp->sched.taskq, NULL, 0);
	}
	ASSERT(no_forward == no_backward);
    }
    ASSERT(no_forward == runq->ports.info.len);
    if (chk_pp) {
	if (chk_pp->sched.taskq || chk_pp->sched.exe_taskq) {
	    ASSERT(chk_pp->sched.taskq != chk_pp->sched.exe_taskq);
	}
	ASSERT(!chk_pp->sched.taskq || chk_pp->sched.taskq->first);
	if (inq < 0)
	    inq = chk_pp->sched.taskq && !chk_pp->sched.exe_taskq;
	if (inq) {
	    ASSERT(found_forward && found_backward);
	}
	else {
	    ASSERT(!found_forward && !found_backward);
	}
    }
}

#endif

/*
 * Task queue operations
 */

static ERTS_INLINE ErtsPortTaskQueue *
port_taskq_init(ErtsPortTaskQueue *ptqp, Port *pp)
{
    if (ptqp) {
	ptqp->first = NULL;
	ptqp->last = NULL;
	ptqp->port = pp;
    }
    return ptqp;
}

static ERTS_INLINE void
enqueue_task(ErtsPortTaskQueue *ptqp, ErtsPortTask *ptp)
{
    ERTS_PT_CHK_NOT_IN_TASKQ(ptqp, ptp);
    ptp->next = NULL;
    ptp->prev = ptqp->last;
    ptp->queue = ptqp;
    if (ptqp->last) {
	ASSERT(ptqp->first);
	ptqp->last->next = ptp;
    }
    else {
	ASSERT(!ptqp->first);
	ptqp->first = ptp;
    }
    ptqp->last = ptp;
    ERTS_PT_CHK_IN_TASKQ(ptqp, ptp);
}

static ERTS_INLINE void
push_task(ErtsPortTaskQueue *ptqp, ErtsPortTask *ptp)
{
    ERTS_PT_CHK_NOT_IN_TASKQ(ptqp, ptp);
    ptp->next = ptqp->first;
    ptp->prev = NULL;
    ptp->queue = ptqp;
    if (ptqp->first) {
	ASSERT(ptqp->last);
	ptqp->first->prev = ptp;
    }
    else {
	ASSERT(!ptqp->last);
	ptqp->last = ptp;
    }
    ptqp->first = ptp;
    ERTS_PT_CHK_IN_TASKQ(ptqp, ptp);
}

static ERTS_INLINE void
dequeue_task(ErtsPortTask *ptp)
{
    ASSERT(ptp);
    ASSERT(ptp->queue);
    ERTS_PT_CHK_IN_TASKQ(ptp->queue, ptp);
    if (ptp->next)
	ptp->next->prev = ptp->prev;
    else {
	ASSERT(ptp->queue->last == ptp);
	ptp->queue->last = ptp->prev;
    }
    if (ptp->prev)
	ptp->prev->next = ptp->next;
    else {
	ASSERT(ptp->queue->first == ptp);
	ptp->queue->first = ptp->next;
    }

    ASSERT(ptp->queue->first || !ptp->queue->last);
    ASSERT(ptp->queue->last || !ptp->queue->first);
    ERTS_PT_CHK_NOT_IN_TASKQ(ptp->queue, ptp);
}

static ERTS_INLINE ErtsPortTask *
pop_task(ErtsPortTaskQueue *ptqp)
{
    ErtsPortTask *ptp = ptqp->first;
    if (!ptp) {
	ASSERT(!ptqp->last);
    }
    else {
	ERTS_PT_CHK_IN_TASKQ(ptqp, ptp);
	ASSERT(!ptp->prev);
	ptqp->first = ptp->next;
	if (ptqp->first)
	    ptqp->first->prev = NULL;
	else {
	    ASSERT(ptqp->last == ptp);
	    ptqp->last = NULL;
	}
	ASSERT(ptp->queue->first || !ptp->queue->last);
	ASSERT(ptp->queue->last || !ptp->queue->first);
    }
    ERTS_PT_CHK_NOT_IN_TASKQ(ptqp, ptp);
    return ptp;
}

#ifdef HARD_DEBUG

static void
check_task_queue(ErtsPortTaskQueue *ptqp,
		 ErtsPortTask *chk_ptp,
		 int inq)
{
    ErtsPortTask *ptp;
    ErtsPortTask *last_ptp;
    ErtsPortTask *first_ptp = ptqp->first;
    int found_forward = 0, found_backward = 0;
    if (!first_ptp) {
	ASSERT(!ptqp->last);
    }
    else {
	ASSERT(!first_ptp->prev);
	for (ptp = first_ptp; ptp; ptp = ptp->next) {
	    ASSERT(ptp->queue == ptqp);
	    if (chk_ptp == ptp)
		found_forward = 1;
	    if (!ptp->prev) {
		ASSERT(first_ptp == ptp);
	    }
	    if (!ptp->next) {
		ASSERT(ptqp->last == ptp);
		last_ptp = ptp;
	    }
	}
	for (ptp = last_ptp; ptp; ptp = ptp->prev) {
	    ASSERT(ptp->queue == ptqp);
	    if (chk_ptp == ptp)
		found_backward = 1;
	    if (!ptp->prev) {
		ASSERT(first_ptp == ptp);
	    }
	    if (!ptp->next) {
		ASSERT(ptqp->last == ptp);
	    }
	}
    }
    if (chk_ptp) {
	if (inq) {
	    ASSERT(found_forward && found_backward);
	}
	else {
	    ASSERT(!found_forward && !found_backward);
	}
    }
}
#endif

/*
 * Abort a scheduled task.
 */

int
erts_port_task_abort(Eterm id, ErtsPortTaskHandle *pthp)
{
    ErtsRunQueue *runq;
    ErtsPortTaskQueue *ptqp;
    ErtsPortTask *ptp;
    Port *pp;
    int port_is_dequeued = 0;

    pp = &erts_port[internal_port_index(id)];
    runq = erts_port_runq(pp);

    ptp = handle2task(pthp);

    if (!ptp) {
	erts_smp_runq_unlock(runq);
	return 1;
    }

    ASSERT(ptp->handle == pthp);
    ptqp = ptp->queue;
    ASSERT(pp == ptqp->port);

    ERTS_PT_CHK_PRES_PORTQ(runq, pp);
    ASSERT(ptqp);
    ASSERT(ptqp->first);

    dequeue_task(ptp);
    reset_handle(ptp);

    switch (ptp->type) {
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT:
    case ERTS_PORT_TASK_EVENT:
	ASSERT(erts_smp_atomic_read(&erts_port_task_outstanding_io_tasks) > 0);
	erts_smp_atomic_dec(&erts_port_task_outstanding_io_tasks);
	break;
    default:
	break;
    }

    ASSERT(ptqp == pp->sched.taskq || ptqp == pp->sched.exe_taskq);

    if (ptqp->first || pp->sched.taskq != ptqp)
	ptqp = NULL;
    else {
	pp->sched.taskq = NULL;
	if (!pp->sched.exe_taskq) {
	    dequeue_port(runq, pp);
	    ERTS_PORT_NOT_IN_RUNQ(pp);
	    port_is_dequeued = 1;
	}
    }

    ERTS_PT_CHK_PRES_PORTQ(runq, pp);

    erts_smp_runq_unlock(runq);
    
    if (erts_system_profile_flags.runnable_ports && port_is_dequeued) {
    	profile_runnable_port(pp, am_inactive);
    }

    port_task_free(ptp);
    if (ptqp)
	port_taskq_free(ptqp);

    return 0;
}

/*
 * Schedule a task.
 */

int
erts_port_task_schedule(Eterm id,
			ErtsPortTaskHandle *pthp,
			ErtsPortTaskType type,
			ErlDrvEvent event,
			ErlDrvEventData event_data)
{
    ErtsRunQueue *runq;
    Port *pp;
    ErtsPortTask *ptp;
    int enq_port = 0;

    /*
     * NOTE:	We might not have the port lock here. We are only
     *		allowed to access the 'sched', 'tab_status',
     *          and 'id' fields of the port struct while
     *          tasks_lock is held.
     */

    if (pthp && erts_port_task_is_scheduled(pthp)) {
	ASSERT(0);
	erts_port_task_abort(id, pthp);
    }

    ptp = port_task_alloc();

    ASSERT(is_internal_port(id));
    pp = &erts_port[internal_port_index(id)];
    runq = erts_port_runq(pp);

    if (!runq || ERTS_PORT_TASK_INVALID_PORT(pp, id)) {
	if (runq)
	    erts_smp_runq_unlock(runq);
	return -1;
    }

    ASSERT(!erts_port_task_is_scheduled(pthp));

    ERTS_PT_CHK_PRES_PORTQ(runq, pp);

    if (!pp->sched.taskq) {
	pp->sched.taskq = port_taskq_init(port_taskq_alloc(), pp);
	enq_port = !pp->sched.exe_taskq;
    }

#ifdef ERTS_SMP
    if (enq_port) {
	ErtsRunQueue *xrunq = erts_check_emigration_need(runq, ERTS_PORT_PRIO_LEVEL);
	if (xrunq) {
	    /* Port emigrated ... */
	    erts_smp_atomic_set(&pp->run_queue, (long) xrunq);
	    erts_smp_runq_unlock(runq);
	    runq = xrunq;
	}
    }
#endif

    ASSERT(!enq_port || !(runq->flags & ERTS_RUNQ_FLG_SUSPENDED));

    ASSERT(pp->sched.taskq);
    ASSERT(ptp);

    ptp->type = type;
    ptp->event = event;
    ptp->event_data = event_data;

    set_handle(ptp, pthp);

    switch (type) {
    case ERTS_PORT_TASK_FREE:
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_port_task_schedule(): Cannot schedule free task\n");
	break;
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT:
    case ERTS_PORT_TASK_EVENT:
	erts_smp_atomic_inc(&erts_port_task_outstanding_io_tasks);
	/* Fall through... */
    default:
	enqueue_task(pp->sched.taskq, ptp);
	break;
    }

#ifndef ERTS_SMP
    /*
     * When (!enq_port && !pp->sched.exe_taskq) is true in the smp case,
     * the port might not be in the run queue. If this is the case, another
     * thread is in the process of enqueueing the port. This very seldom
     * occur, but do occur and is a valid scenario. Debug info showing this
     * enqueue in progress must be introduced before we can enable (modified
     * versions of these) assertions in the smp case again.
     */
#if defined(HARD_DEBUG)
    if (pp->sched.exe_taskq || enq_port)
	ERTS_PT_CHK_NOT_IN_PORTQ(runq, pp);
    else
	ERTS_PT_CHK_IN_PORTQ(runq, pp);
#elif defined(DEBUG)
    if (!enq_port && !pp->sched.exe_taskq) {
	/* We should be in port run q */
	ASSERT(pp->sched.prev || runq->ports.start == pp);
    }
#endif
#endif

    if (!enq_port) {
	ERTS_PT_CHK_PRES_PORTQ(runq, pp);
    }
    else {
	enqueue_port(runq, pp);
	ERTS_PT_CHK_PRES_PORTQ(runq, pp);
	    
	if (erts_system_profile_flags.runnable_ports) {
	    profile_runnable_port(pp, am_active);
	}

	erts_smp_notify_inc_runq(runq);
    }
    erts_smp_runq_unlock(runq);
    return 0;
}

void
erts_port_task_free_port(Port *pp)
{
    ErtsRunQueue *runq;
    int port_is_dequeued = 0;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    ASSERT(!(pp->status & ERTS_PORT_SFLGS_DEAD));
    runq = erts_port_runq(pp);
    ASSERT(runq);
    ERTS_PT_CHK_PRES_PORTQ(runq, pp);
    if (pp->sched.exe_taskq) {
	/* I (this thread) am currently executing this port, free it
	   when scheduled out... */
	ErtsPortTask *ptp = port_task_alloc();
	erts_smp_port_state_lock(pp);
	ASSERT(erts_smp_atomic_read(&erts_ports_alive) > 0);
	erts_smp_atomic_dec(&erts_ports_alive);
	pp->status &= ~ERTS_PORT_SFLG_CLOSING;
	pp->status |= ERTS_PORT_SFLG_FREE_SCHEDULED;
	erts_may_save_closed_port(pp);
	erts_smp_port_state_unlock(pp);
	ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&pp->refc) > 1);
	ptp->type = ERTS_PORT_TASK_FREE;
	ptp->event = (ErlDrvEvent) -1;
	ptp->event_data = NULL;
	set_handle(ptp, NULL);
	push_task(pp->sched.exe_taskq, ptp);
	ERTS_PT_CHK_PRES_PORTQ(runq, pp);
	erts_smp_runq_unlock(runq);
    }
    else {
	ErtsPortTaskQueue *ptqp = pp->sched.taskq;
	if (ptqp) {
	    dequeue_port(runq, pp);
	    ERTS_PORT_NOT_IN_RUNQ(pp);
	    port_is_dequeued = 1;
	}
	erts_smp_port_state_lock(pp);
	erts_smp_atomic_dec(&erts_ports_alive);
	pp->status &= ~ERTS_PORT_SFLG_CLOSING;
	pp->status |= ERTS_PORT_SFLG_FREE_SCHEDULED;
	erts_may_save_closed_port(pp);
	erts_smp_port_state_unlock(pp);
#ifdef ERTS_SMP
	erts_smp_atomic_dec(&pp->refc); /* Not alive */
#endif
	ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&pp->refc) > 0); /* Lock */
	handle_remaining_tasks(runq, pp); /* May release runq lock */
	ASSERT(!pp->sched.exe_taskq && (!ptqp || !ptqp->first));
	pp->sched.taskq = NULL;
	ERTS_PT_CHK_PRES_PORTQ(runq, pp);
#ifndef ERTS_SMP
	ASSERT(pp->status & ERTS_PORT_SFLG_PORT_DEBUG);
	erts_port_status_set(pp, ERTS_PORT_SFLG_FREE);	
#endif
	erts_smp_runq_unlock(runq);

	if (erts_system_profile_flags.runnable_ports && port_is_dequeued) {
    	    profile_runnable_port(pp, am_inactive);
    	}

	if (ptqp)
	    port_taskq_free(ptqp);
    }
}

typedef struct {
    ErtsRunQueue *runq;
    int *resp;
} ErtsPortTaskExeBlockData;

static void
prepare_for_block(void *vd)
{
    ErtsPortTaskExeBlockData *d = (ErtsPortTaskExeBlockData *) vd;
    erts_smp_runq_unlock(d->runq);
}

static void
resume_after_block(void *vd)
{
    ErtsPortTaskExeBlockData *d = (ErtsPortTaskExeBlockData *) vd;
    erts_smp_runq_lock(d->runq);
    if (d->resp)
	*d->resp = erts_smp_atomic_read(&erts_port_task_outstanding_io_tasks) != (long) 0;
}

/*
 * Run all scheduled tasks for the first port in run queue. If
 * new tasks appear while running reschedule port (free task is
 * an exception; it is always handled instantly).
 *
 * erts_port_task_execute() is called by scheduler threads between
 * scheduleing of processes. Sched lock should be held by caller.
 */

int
erts_port_task_execute(ErtsRunQueue *runq, Port **curr_port_pp)
{
    int port_was_enqueued = 0;
    Port *pp;
    ErtsPortTaskQueue *ptqp;
    ErtsPortTask *ptp;
    int res = 0;
    int reds = ERTS_PORT_REDS_EXECUTE;
    long io_tasks_executed = 0;
    int fpe_was_unmasked;
    ErtsPortTaskExeBlockData blk_data = {runq, NULL};

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    erts_smp_activity_begin(ERTS_ACTIVITY_IO,
			    prepare_for_block,
			    resume_after_block,
			    (void *) &blk_data);

    ERTS_PT_CHK_PORTQ(runq);

    pp = pop_port(runq);
    if (!pp) {
	res = 0;
	goto done;
    }

    ERTS_PORT_NOT_IN_RUNQ(pp);

    *curr_port_pp = pp;

    ASSERT(pp->sched.taskq);
    ASSERT(pp->sched.taskq->first);
    ptqp = pp->sched.taskq;
    pp->sched.taskq = NULL;

    ASSERT(!pp->sched.exe_taskq);
    pp->sched.exe_taskq = ptqp;

    if (erts_smp_port_trylock(pp) == EBUSY) {
	erts_smp_runq_unlock(runq);
	erts_smp_port_lock(pp);
	erts_smp_runq_lock(runq);
    }
    
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

    /* trace port scheduling, in */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports(pp, am_in);
    }

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

    ERTS_PT_CHK_PRES_PORTQ(runq, pp);
    ptp = pop_task(ptqp);

    fpe_was_unmasked = erts_block_fpe();

    while (ptp) {
	ASSERT(pp->sched.taskq != pp->sched.exe_taskq);

	reset_handle(ptp);
	erts_smp_runq_unlock(runq);

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
	ERTS_SMP_CHK_NO_PROC_LOCKS;
	ASSERT(pp->drv_ptr);

	switch (ptp->type) {
	case ERTS_PORT_TASK_FREE: /* May be pushed in q at any time */
	    reds += ERTS_PORT_REDS_FREE;
	    erts_smp_runq_lock(runq);

	    erts_unblock_fpe(fpe_was_unmasked);
	    ASSERT(pp->status & ERTS_PORT_SFLG_FREE_SCHEDULED);
	    if (ptqp->first || (pp->sched.taskq && pp->sched.taskq->first))
		handle_remaining_tasks(runq, pp);
	    ASSERT(!ptqp->first
		   && (!pp->sched.taskq || !pp->sched.taskq->first));
#ifdef ERTS_SMP
	    erts_smp_atomic_dec(&pp->refc); /* Not alive */
	    ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&pp->refc) > 0); /* Lock */
#else
	    erts_port_status_bor_set(pp, ERTS_PORT_SFLG_FREE);	    
#endif

	    port_task_free(ptp);
	    if (pp->sched.taskq)
		port_taskq_free(pp->sched.taskq);
	    pp->sched.taskq = NULL;

	    goto tasks_done;
	case ERTS_PORT_TASK_TIMEOUT:
	    reds += ERTS_PORT_REDS_TIMEOUT;
	    if (!(pp->status & ERTS_PORT_SFLGS_DEAD))
		(*pp->drv_ptr->timeout)((ErlDrvData) pp->drv_data);
	    break;
	case ERTS_PORT_TASK_INPUT:
	    reds += ERTS_PORT_REDS_INPUT;
	    ASSERT((pp->status & ERTS_PORT_SFLGS_DEAD) == 0);
	    /* NOTE some windows drivers use ->ready_input for input and output */
	    (*pp->drv_ptr->ready_input)((ErlDrvData) pp->drv_data, ptp->event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_OUTPUT:
	    reds += ERTS_PORT_REDS_OUTPUT;
	    ASSERT((pp->status & ERTS_PORT_SFLGS_DEAD) == 0);
	    (*pp->drv_ptr->ready_output)((ErlDrvData) pp->drv_data, ptp->event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_EVENT:
	    reds += ERTS_PORT_REDS_EVENT;
	    ASSERT((pp->status & ERTS_PORT_SFLGS_DEAD) == 0);
	    (*pp->drv_ptr->event)((ErlDrvData) pp->drv_data, ptp->event, ptp->event_data);
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

	if ((pp->status & ERTS_PORT_SFLG_CLOSING)
	    && erts_is_port_ioq_empty(pp)) {
	    reds += ERTS_PORT_REDS_TERMINATE;
	    erts_terminate_port(pp);
	}

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

#ifdef ERTS_SMP
	if (pp->xports)
	    erts_smp_xports_unlock(pp);
	ASSERT(!pp->xports);
#endif

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

	port_task_free(ptp);

	erts_smp_runq_lock(runq);

	ptp = pop_task(ptqp);
    }

 tasks_done:

    erts_unblock_fpe(fpe_was_unmasked);

    if (io_tasks_executed) {
	ASSERT(erts_smp_atomic_read(&erts_port_task_outstanding_io_tasks) >= io_tasks_executed);
	erts_smp_atomic_add(&erts_port_task_outstanding_io_tasks, -1*io_tasks_executed);
    }

    *curr_port_pp = NULL;

#ifdef ERTS_SMP
    ASSERT(runq == (ErtsRunQueue *) erts_smp_atomic_read(&pp->run_queue));
#endif

    if (!pp->sched.taskq) {
	ASSERT(pp->sched.exe_taskq);
	pp->sched.exe_taskq = NULL;
    }
    else {
#ifdef ERTS_SMP
	ErtsRunQueue *xrunq;
#endif

	ASSERT(!(pp->status & ERTS_PORT_SFLGS_DEAD));
	ASSERT(pp->sched.taskq->first);

#ifdef ERTS_SMP
	xrunq = erts_check_emigration_need(runq, ERTS_PORT_PRIO_LEVEL);
	if (!xrunq) {
#endif
	    enqueue_port(runq, pp);
	    ASSERT(pp->sched.exe_taskq);
	    pp->sched.exe_taskq = NULL;
	    /* No need to notify ourselves about inc in runq. */
#ifdef ERTS_SMP
	}
	else {
	    /* Port emigrated ... */
	    erts_smp_atomic_set(&pp->run_queue, (long) xrunq);
	    enqueue_port(xrunq, pp);
	    ASSERT(pp->sched.exe_taskq);
	    pp->sched.exe_taskq = NULL;
	    erts_smp_notify_inc_runq(xrunq);
	    erts_smp_runq_unlock(xrunq);
	}
#endif
	port_was_enqueued = 1;
    }

    res = erts_smp_atomic_read(&erts_port_task_outstanding_io_tasks) != (long) 0;

    ERTS_PT_CHK_PRES_PORTQ(runq, pp);

    port_taskq_free(ptqp);

    if (erts_system_profile_flags.runnable_ports && (port_was_enqueued != 1)) {
    	profile_runnable_port(pp, am_inactive);
    }

    /* trace port scheduling, out */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports(pp, am_out);
    }
#ifndef ERTS_SMP
    erts_port_release(pp);
#else
    {
	long refc;
	erts_smp_mtx_unlock(pp->lock);
	refc = erts_smp_atomic_dectest(&pp->refc);
	ASSERT(refc >= 0);
	if (refc == 0) {
	    erts_smp_runq_unlock(runq);
	    erts_port_cleanup(pp); /* Might aquire runq lock */
	    erts_smp_runq_lock(runq);
	    res = erts_smp_atomic_read(&erts_port_task_outstanding_io_tasks) != (long) 0;
	}
    }
#endif

 done:
    blk_data.resp = &res;
    erts_smp_activity_end(ERTS_ACTIVITY_IO,
			  prepare_for_block,
			  resume_after_block,
			  (void *) &blk_data);

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    ERTS_PORT_REDUCTIONS_EXECUTED(runq, reds);

    return res;
}

/*
 * Handle remaining tasks after a free task.
 */

static void
handle_remaining_tasks(ErtsRunQueue *runq, Port *pp)
{
    int i;
    ErtsPortTask *ptp;
    ErtsPortTaskQueue *ptqps[] = {pp->sched.exe_taskq, pp->sched.taskq};

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

    for (i = 0; i < sizeof(ptqps)/sizeof(ErtsPortTaskQueue *); i++) {
	if (!ptqps[i])
	    continue;

	ptp = pop_task(ptqps[i]);
	while (ptp) {
	    reset_handle(ptp);
	    erts_smp_runq_unlock(runq);

	    switch (ptp->type) {
	    case ERTS_PORT_TASK_FREE:
	    case ERTS_PORT_TASK_TIMEOUT:
		break;
	    case ERTS_PORT_TASK_INPUT:
		erts_stale_drv_select(pp->id, ptp->event, DO_READ, 1);
		break;
	    case ERTS_PORT_TASK_OUTPUT:
		erts_stale_drv_select(pp->id, ptp->event, DO_WRITE, 1);
		break;
	    case ERTS_PORT_TASK_EVENT:
		erts_stale_drv_select(pp->id, ptp->event, 0, 1);
		break;
	    case ERTS_PORT_TASK_DIST_CMD:
		break;
	    default:
		erl_exit(ERTS_ABORT_EXIT,
			 "Invalid port task type: %d\n",
			 (int) ptp->type);
	    }

	    port_task_free(ptp);

	    erts_smp_runq_lock(runq);
	    ptp = pop_task(ptqps[i]);
	}
    }

    ASSERT(!pp->sched.taskq || !pp->sched.taskq->first);
}

int
erts_port_is_scheduled(Port *pp)
{
    int res;
    ErtsRunQueue *runq = erts_port_runq(pp);
    res = pp->sched.taskq || pp->sched.exe_taskq;
    erts_smp_runq_unlock(runq);
    return res;
}

#ifdef ERTS_SMP

ErtsMigrateResult
erts_port_migrate(Port *prt, int *prt_locked,
		  ErtsRunQueue *from_rq, int *from_locked,
		  ErtsRunQueue *to_rq, int *to_locked)
{
    ERTS_SMP_LC_ASSERT(*from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(from_rq, *from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(to_rq, *to_locked);

    ASSERT(!erts_common_run_queue);

    if (!*from_locked || !*to_locked) {
	if (from_rq < to_rq) {
	    if (!*to_locked) {
		if (!*from_locked)
		    erts_smp_runq_lock(from_rq);
		erts_smp_runq_lock(to_rq);
	    }
	    else if (erts_smp_runq_trylock(from_rq) == EBUSY) {
		erts_smp_runq_unlock(to_rq);
		erts_smp_runq_lock(from_rq);
		erts_smp_runq_lock(to_rq);
	    }
	}
	else {
	    if (!*from_locked) {
		if (!*to_locked)
		    erts_smp_runq_lock(to_rq);
		erts_smp_runq_lock(from_rq);
	    }
	    else if (erts_smp_runq_trylock(to_rq) == EBUSY) {
		erts_smp_runq_unlock(from_rq);
		erts_smp_runq_lock(to_rq);
		erts_smp_runq_lock(from_rq);
	    }
	}
	*to_locked = *from_locked = 1;
    }
    ERTS_SMP_LC_CHK_RUNQ_LOCK(from_rq, *from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(to_rq, *to_locked);

    /* Refuse to migrate to a suspended run queue */
    if (to_rq->flags & ERTS_RUNQ_FLG_SUSPENDED)
	return ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED;
    if (from_rq != (ErtsRunQueue *) erts_smp_atomic_read(&prt->run_queue))
	return ERTS_MIGRATE_FAILED_RUNQ_CHANGED;
    if (!ERTS_PORT_IS_IN_RUNQ(from_rq, prt))
	return ERTS_MIGRATE_FAILED_NOT_IN_RUNQ;
    dequeue_port(from_rq, prt);
    erts_smp_atomic_set(&prt->run_queue, (long) to_rq);
    enqueue_port(to_rq, prt);
    erts_smp_notify_inc_runq(to_rq);
    return ERTS_MIGRATE_SUCCESS;
}

#endif

/*
 * Initialize the module.
 */
void
erts_port_task_init(void)
{
    erts_smp_atomic_init(&erts_port_task_outstanding_io_tasks, (long) 0);
    init_port_task_alloc();
    init_port_taskq_alloc();
}
