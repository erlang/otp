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
#define ERTS_HARD_DEBUG_TASK_QUEUES
#else
#undef ERTS_HARD_DEBUG_TASK_QUEUES
#endif

#ifdef ERTS_HARD_DEBUG_TASK_QUEUES
static void chk_task_queues(Port *pp, ErtsPortTask *execq, int processing_busy_queue);
#define ERTS_PT_DBG_CHK_TASK_QS(PP, EQ, PBQ) \
    chk_task_queues((PP), (EQ), (PBQ))
#else
#define ERTS_PT_DBG_CHK_TASK_QS(PP, EQ, PBQ)
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
    if (DTRACE_ENABLED(PROBE_NAME)) {                              \
        DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);         \
        DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);            \
                                                                   \
        dtrace_pid_str(ERTS_PORT_GET_CONNECTED(PP), process_str);  \
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

typedef union {
    struct { /* I/O tasks */
	ErlDrvEvent event;
	ErlDrvEventData event_data;
    } io;
    struct {
	ErtsProc2PortSigCallback callback;
	ErtsProc2PortSigData data;
    } psig;
} ErtsPortTaskTypeData;

struct ErtsPortTask_ {
    erts_smp_atomic32_t state;
    ErtsPortTaskType type;
    union {
	struct {
	    ErtsPortTask *next;
	    ErtsPortTaskHandle *handle;
	    int flags;
	    Uint32 ref[ERTS_MAX_REF_NUMBERS];
	    ErtsPortTaskTypeData td;
	} alive;
	ErtsThrPrgrLaterOp release;
    } u;
};

struct ErtsPortTaskHandleList_ {
    ErtsPortTaskHandle handle;
    union {
	ErtsPortTaskHandleList *next;
#ifdef ERTS_SMP
	ErtsThrPrgrLaterOp release;
#endif
    } u;
};

typedef struct ErtsPortTaskBusyCaller_ ErtsPortTaskBusyCaller;
struct ErtsPortTaskBusyCaller_ {
    ErtsPortTaskBusyCaller *next;
    Eterm caller;
    SWord count;
    ErtsPortTask *last;
};

#define ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS 17
struct ErtsPortTaskBusyCallerTable_ {
    ErtsPortTaskBusyCaller *bucket[ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS];
    ErtsPortTaskBusyCaller pre_alloc_busy_caller;
};


static void begin_port_cleanup(Port *pp, ErtsPortTask **execq);

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(port_task,
				 ErtsPortTask,
				 1000,
				 ERTS_ALC_T_PORT_TASK)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(busy_caller_table,
				 ErtsPortTaskBusyCallerTable,
				 50,
				 ERTS_ALC_T_BUSY_CALLER_TAB)

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

static ERTS_INLINE ErtsPortTask *
p2p_sig_data_to_task(ErtsProc2PortSigData *sigdp)
{
    ErtsPortTask *ptp;
    char *ptr = (char *) sigdp;
    ptr -= offsetof(ErtsPortTask, u.alive.td.psig.data);
    ptp = (ErtsPortTask *) ptr;
    ASSERT(ptp->type == ERTS_PORT_TASK_PROC_SIG);
    return ptp;
}

ErtsProc2PortSigData *
erts_port_task_alloc_p2p_sig_data(void)
{
    ErtsPortTask *ptp = port_task_alloc();

    ptp->type = ERTS_PORT_TASK_PROC_SIG;
    ptp->u.alive.flags = ERTS_PT_FLG_SIG_DEP;
    erts_smp_atomic32_init_nob(&ptp->state, ERTS_PT_STATE_SCHEDULED);

    ASSERT(ptp == p2p_sig_data_to_task(&ptp->u.alive.td.psig.data));

    return &ptp->u.alive.td.psig.data;
}

static ERTS_INLINE Eterm
task_caller(ErtsPortTask *ptp)
{
    Eterm caller;

    ASSERT(ptp->type == ERTS_PORT_TASK_PROC_SIG);

    caller = ptp->u.alive.td.psig.data.caller;

    ASSERT(is_internal_pid(caller) || is_internal_port(caller));

    return caller;
}

/*
 * Busy queue management
 */

static ERTS_INLINE int
caller2bix(Eterm caller)
{
    ASSERT(is_internal_pid(caller) || is_internal_port(caller));
    return (int) (_GET_PID_DATA(caller) % ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS);
}


static void
popped_from_busy_queue(Port *pp, ErtsPortTask *ptp, int last)
{
    ErtsPortTaskBusyCaller **prev_bcpp = NULL, *bcp;
    ErtsPortTaskBusyCallerTable *tabp = pp->sched.taskq.local.busy.table;
    Eterm caller = task_caller(ptp);
    int bix = caller2bix(caller);

    ASSERT(is_internal_pid(caller));

    ASSERT(tabp);
    bcp = tabp->bucket[bix];
    prev_bcpp = &tabp->bucket[bix];
    ASSERT(bcp);
    while (bcp->caller != caller) {
	prev_bcpp = &bcp->next;
	bcp = bcp->next;
	ASSERT(bcp);
    }
    ASSERT(bcp->count > 0);
    if (--bcp->count != 0) {
	ASSERT(!last);
    }
    else {
	*prev_bcpp = bcp->next;
	if (bcp == &tabp->pre_alloc_busy_caller)
	    bcp->caller = am_undefined;
	else
	    erts_free(ERTS_ALC_T_BUSY_CALLER, bcp);
	if (last) {
#ifdef DEBUG
	    erts_aint32_t flags =
#endif
		erts_smp_atomic32_read_band_nob(
		    &pp->sched.flags,
		    ~ERTS_PTS_FLG_HAVE_BUSY_TASKS);
	    ASSERT(flags & ERTS_PTS_FLG_HAVE_BUSY_TASKS);
#ifdef DEBUG
	    for (bix = 0; bix < ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS; bix++) {
		ASSERT(!tabp->bucket[bix]);
	    }
#endif
	    busy_caller_table_free(tabp);
	    pp->sched.taskq.local.busy.first = NULL;
	    pp->sched.taskq.local.busy.last = NULL;
	    pp->sched.taskq.local.busy.table = NULL;
	}
    }
}

static void
busy_wait_move_to_busy_queue(Port *pp, ErtsPortTask *ptp)
{
    ErtsPortTaskBusyCallerTable *tabp = pp->sched.taskq.local.busy.table;
    Eterm caller = task_caller(ptp);
    ErtsPortTaskBusyCaller *bcp;
    int bix;

    ASSERT(is_internal_pid(caller));
    /*
     * Port is busy and this task type needs to wait until not busy.
     */

    ASSERT(ptp->u.alive.flags & ERTS_PT_FLG_WAIT_BUSY);

    ptp->u.alive.next = NULL;
    if (pp->sched.taskq.local.busy.last) {
	ASSERT(pp->sched.taskq.local.busy.first);
	pp->sched.taskq.local.busy.last->u.alive.next = ptp;
    }
    else {
	int i;
	erts_aint32_t flags;

	pp->sched.taskq.local.busy.first = ptp;
	flags = erts_smp_atomic32_read_bor_nob(&pp->sched.flags,
					       ERTS_PTS_FLG_HAVE_BUSY_TASKS);
	ASSERT(!(flags & ERTS_PTS_FLG_HAVE_BUSY_TASKS));

	ASSERT(!tabp);

	tabp = busy_caller_table_alloc();
	pp->sched.taskq.local.busy.table = tabp;
	for (i = 0; i < ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS; i++)
	    tabp->bucket[i] = NULL;
	tabp->pre_alloc_busy_caller.caller = am_undefined;
    }
    pp->sched.taskq.local.busy.last = ptp;

    bix = caller2bix(caller);
    ASSERT(tabp);
    bcp = tabp->bucket[bix];

    while (bcp && bcp->caller != caller)
	bcp = bcp->next;

    if (bcp)
	bcp->count++;
    else {
	if (tabp->pre_alloc_busy_caller.caller == am_undefined)
	    bcp = &tabp->pre_alloc_busy_caller;
	else
	    bcp = erts_alloc(ERTS_ALC_T_BUSY_CALLER,
			     sizeof(ErtsPortTaskBusyCaller));
	bcp->caller = caller;
	bcp->count = 1;
	bcp->next = tabp->bucket[bix];
	tabp->bucket[bix] = bcp;
    }

    bcp->last = ptp;
}

static ERTS_INLINE int
check_sig_dep_move_to_busy_queue(Port *pp, ErtsPortTask *ptp)
{
    ErtsPortTaskBusyCallerTable *tabp = pp->sched.taskq.local.busy.table;
    ErtsPortTask *last_ptp;
    ErtsPortTaskBusyCaller *bcp;
    int bix;
    Eterm caller;

    ASSERT(ptp->u.alive.flags & ERTS_PT_FLG_SIG_DEP);
    ASSERT(pp->sched.taskq.local.busy.last);
    ASSERT(tabp);


    /*
     * We are either not busy, or the task does not imply wait on busy port.
     * However, due to the signaling order requirements the task might depend
     * on other tasks in the busy queue.
     */

    caller = task_caller(ptp);
    bix = caller2bix(caller);
    bcp = tabp->bucket[bix];
    while (bcp && bcp->caller != caller)
	bcp = bcp->next;

    if (!bcp)
	return 0;

    /*
     * There are other tasks that we depend on in the busy queue;
     * move into busy queue.
     */

    bcp->count++;
    last_ptp = bcp->last;
    ptp->u.alive.next = last_ptp->u.alive.next;
    if (!ptp->u.alive.next) {
	ASSERT(pp->sched.taskq.local.busy.last == last_ptp);
	pp->sched.taskq.local.busy.last = ptp;
    }
    last_ptp->u.alive.next = ptp;
    bcp->last = ptp;

    return 1;
}

static void
no_sig_dep_move_from_busyq(Port *pp)
{
    ErtsPortTaskBusyCallerTable *tabp = pp->sched.taskq.local.busy.table;
    ErtsPortTask *first_ptp, *last_ptp, *ptp;
    ErtsPortTaskBusyCaller **prev_bcpp = NULL, *bcp = NULL;

    /*
     * Move tasks at the head of the busy queue that no longer
     * have any dependencies to busy wait tasks into the ordinary
     * queue.
     */

    first_ptp = ptp = pp->sched.taskq.local.busy.first;

    ASSERT(ptp && !(ptp->u.alive.flags & ERTS_PT_FLG_WAIT_BUSY));
    ASSERT(tabp);

    do {
	Eterm caller = task_caller(ptp);

	if (!bcp || bcp->caller != caller) {
	    int bix = caller2bix(caller);

	    prev_bcpp = &tabp->bucket[bix];
	    bcp = tabp->bucket[bix];
	    ASSERT(bcp);
	    while (bcp->caller != caller) {
		ASSERT(bcp);
		prev_bcpp = &bcp->next;
		bcp = bcp->next;
	    }
	}

	ASSERT(bcp->caller == caller);
	ASSERT(bcp->count > 0);

	if (--bcp->count == 0) {
	    *prev_bcpp = bcp->next;
	    if (bcp == &tabp->pre_alloc_busy_caller)
		bcp->caller = am_undefined;
	    else
		erts_free(ERTS_ALC_T_BUSY_CALLER, bcp);
	}

	last_ptp = ptp;
	ptp = ptp->u.alive.next;
    } while (ptp && !(ptp->u.alive.flags & ERTS_PT_FLG_WAIT_BUSY));

    pp->sched.taskq.local.busy.first = last_ptp->u.alive.next;
    if (!pp->sched.taskq.local.busy.first) {
#ifdef DEBUG
	int bix;
	erts_aint32_t flags =
#endif
	    erts_smp_atomic32_read_band_nob(
		&pp->sched.flags,
		~ERTS_PTS_FLG_HAVE_BUSY_TASKS);
	ASSERT(flags & ERTS_PTS_FLG_HAVE_BUSY_TASKS);
#ifdef DEBUG
	for (bix = 0; bix < ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS; bix++) {
	    ASSERT(!tabp->bucket[bix]);
	}
#endif
	busy_caller_table_free(tabp);
	pp->sched.taskq.local.busy.last = NULL;
	pp->sched.taskq.local.busy.table = NULL;
    }
    last_ptp->u.alive.next = pp->sched.taskq.local.first;
    pp->sched.taskq.local.first = first_ptp;
}

#ifdef ERTS_HARD_DEBUG_TASK_QUEUES

static void
chk_task_queues(Port *pp, ErtsPortTask *execq, int processing_busy_queue)
{
    Sint tot_count, tot_table_count;
    int bix;
    ErtsPortTask *ptp, *last;
    ErtsPortTask *first = processing_busy_queue ? execq : pp->sched.taskq.local.busy.first;
    ErtsPortTask *nb_task_queue = processing_busy_queue ? pp->sched.taskq.local.first : execq;
    ErtsPortTaskBusyCallerTable *tabp = pp->sched.taskq.local.busy.table;
    ErtsPortTaskBusyCaller *bcp;

    if (!first) {
	ASSERT(!tabp);
	ASSERT(!pp->sched.taskq.local.busy.last);
	ASSERT(!(erts_smp_atomic32_read_nob(&pp->sched.flags) & ERTS_PTS_FLG_HAVE_BUSY_TASKS));
	return;
    }

    ASSERT(erts_smp_atomic32_read_nob(&pp->sched.flags) & ERTS_PTS_FLG_HAVE_BUSY_TASKS);
    ASSERT(tabp);

    tot_count = 0;
    ptp = first;
    while (ptp) {
	Sint count = 0;
	Eterm caller = task_caller(ptp);
	int bix = caller2bix(caller);
	for (bcp = tabp->bucket[bix]; bcp; bcp = bcp->next)
	    if (bcp->caller == caller)
		break;
	ASSERT(bcp && bcp->caller == caller);

	ASSERT(bcp->last);
	while (1) {
	    ErtsPortTask *ptp2;

	    ASSERT(caller == task_caller(ptp));
	    count++;
	    tot_count++;
	    last = ptp;

	    for (ptp2 = nb_task_queue; ptp2; ptp2 = ptp2->u.alive.next) {
		ASSERT(ptp != ptp2);
	    }

	    if (ptp == bcp->last)
		break;
	    ptp = ptp->u.alive.next;
	}

	ASSERT(count == bcp->count);
	ptp = ptp->u.alive.next;
    }

    tot_table_count = 0;
    for (bix = 0; bix < ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS; bix++) {
	for (bcp = tabp->bucket[bix]; bcp; bcp = bcp->next)
	    tot_table_count += bcp->count;
    }

    ASSERT(tot_count == tot_table_count);

    ASSERT(last == pp->sched.taskq.local.busy.last);
}

#endif /* ERTS_HARD_DEBUG_TASK_QUEUES */

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
 * No-suspend handles.
 */

#ifdef ERTS_SMP
static void
free_port_task_handle_list(void *vpthlp)
{
    erts_free(ERTS_ALC_T_PT_HNDL_LIST, vpthlp);
}
#endif

static void
schedule_port_task_handle_list_free(ErtsPortTaskHandleList *pthlp)
{
#ifdef ERTS_SMP
    erts_schedule_thr_prgr_later_op(free_port_task_handle_list,
				    (void *) pthlp,
				    &pthlp->u.release);
#else
    erts_free(ERTS_ALC_T_PT_HNDL_LIST, pthlp);
#endif
}

static ERTS_INLINE void
abort_nosuspend_task(ErtsPortTaskType type,
		     ErtsPortTaskTypeData *tdp)
{

    if (type != ERTS_PORT_TASK_PROC_SIG)
	ERTS_INTERNAL_ERROR("Invalid no-suspend port task type");

    tdp->psig.callback(NULL,
		       ERTS_PORT_SFLG_INVALID,
		       ERTS_PROC2PORT_SIG_ABORT_NOSUSPEND,
		       &tdp->psig.data);
}


static void
save_nosuspend_handle(Port *pp, ErtsPortTask *ptp)
{
    erts_aint32_t act;
    ErtsPortTaskHandleList *pthlp = erts_alloc(ERTS_ALC_T_PT_HNDL_LIST,
					       sizeof(ErtsPortTaskHandleList));

    set_handle(ptp, &pthlp->handle);

    ASSERT(ptp == handle2task(&pthlp->handle));
    ASSERT(ptp->u.alive.handle == &pthlp->handle);

    act = erts_smp_atomic32_read_nob(&pp->sched.flags);

    if (!(act & ERTS_PTS_FLG_BUSY)) {

	erts_port_task_sched_lock(&pp->sched);
	pthlp->u.next = pp->sched.taskq.local.busy.nosuspend;
	pp->sched.taskq.local.busy.nosuspend = pthlp;
	erts_port_task_sched_unlock(&pp->sched);

	act = erts_smp_atomic32_read_nob(&pp->sched.flags);

	while (1) {
	    erts_aint32_t exp, new;

	    if ((act & (ERTS_PTS_FLG_BUSY|ERTS_PTS_FLG_HAVE_NS_TASKS))
		== ERTS_PTS_FLG_HAVE_NS_TASKS)
		return;

	    if (act & ERTS_PTS_FLG_BUSY) {
		erts_aint32_t s;
		s = erts_smp_atomic32_cmpxchg_nob(&ptp->state,
						  ERTS_PT_STATE_ABORTED,
						  ERTS_PT_STATE_SCHEDULED);
		if (s == ERTS_PT_STATE_SCHEDULED) {
		    reset_port_task_handle(&pthlp->handle);
		    break; /* Abort task */
		}
		/* Else: someone else handled it */
		return;
	    }

	    new = exp = act;

	    new |= ERTS_PTS_FLG_HAVE_NS_TASKS;

	    act = erts_smp_atomic32_cmpxchg_relb(&pp->sched.flags, new, exp);
	    if (act == exp)
		return;

	}
    }


    abort_nosuspend_task(ptp->type, &ptp->u.alive.td);
}


static ErtsPortTaskHandleList *
get_free_nosuspend_handles(Port *pp)
{
    ErtsPortTaskHandleList *nshp, *last_nshp = NULL;

    ERTS_SMP_LC_ASSERT(erts_port_task_sched_lock_is_locked(&pp->sched));

    nshp = pp->sched.taskq.local.busy.nosuspend;

    while (nshp && !erts_port_task_is_scheduled(&nshp->handle)) {
	last_nshp = nshp;
	nshp = nshp->u.next;
    }

    if (!last_nshp)
	nshp = NULL;
    else {
	nshp = pp->sched.taskq.local.busy.nosuspend;
	pp->sched.taskq.local.busy.nosuspend = last_nshp->u.next;
	last_nshp->u.next = NULL;
	if (!pp->sched.taskq.local.busy.nosuspend)
	    erts_smp_atomic32_read_band_nob(&pp->sched.flags,
					    ~ERTS_PTS_FLG_HAVE_NS_TASKS);
    }
    return nshp;
}

static void
free_nosuspend_handles(ErtsPortTaskHandleList *free_nshp)
{
    while (free_nshp) {
	ErtsPortTaskHandleList *nshp = free_nshp;
	free_nshp = free_nshp->u.next;
	schedule_port_task_handle_list_free(nshp);
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

static ERTS_INLINE void
prepare_exec(Port *pp, ErtsPortTask **execqp, int *processing_busy_q_p)
{
    erts_aint32_t act = erts_smp_atomic32_read_nob(&pp->sched.flags);

    if (!pp->sched.taskq.local.busy.first || (act & ERTS_PTS_FLG_BUSY)) {
	*execqp = pp->sched.taskq.local.first;
	*processing_busy_q_p = 0;
    }
    else {
	*execqp = pp->sched.taskq.local.busy.first;
	*processing_busy_q_p = 1;
    }

    ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);

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
finalize_exec(Port *pp, ErtsPortTask **execq, int processing_busy_q)
{
    erts_aint32_t act;

    if (!processing_busy_q)
	pp->sched.taskq.local.first = *execq;
    else {
	pp->sched.taskq.local.busy.first = *execq;
	ASSERT(*execq);
    }

    ERTS_PT_DBG_CHK_TASK_QS(pp, *execq, processing_busy_q);

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

static ERTS_INLINE erts_aint32_t
select_queue_for_exec(Port *pp, ErtsPortTask **execqp, int *processing_busy_q_p)
{
    erts_aint32_t flags = erts_smp_atomic32_read_nob(&pp->sched.flags);

    ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);

    if (flags & ERTS_PTS_FLG_BUSY) {
	if (*processing_busy_q_p) {
	    ErtsPortTask *ptp;

	    ptp = pp->sched.taskq.local.busy.first = *execqp;
	    if (!ptp)
		pp->sched.taskq.local.busy.last = NULL;
	    else if (!(ptp->u.alive.flags & ERTS_PT_FLG_WAIT_BUSY))
		no_sig_dep_move_from_busyq(pp);

	    *execqp = pp->sched.taskq.local.first;
	    *processing_busy_q_p = 0;

	    ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);
	}

	return flags;
    }

    /* Not busy */

    if (!*processing_busy_q_p && pp->sched.taskq.local.busy.first) {
	pp->sched.taskq.local.first = *execqp;
	*execqp = pp->sched.taskq.local.busy.first;
	*processing_busy_q_p = 1;

	ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);
    }

    return flags;
}

/*
 * check_task_for_exec() returns a value !0 if the task
 * is ok to execute; otherwise 0.
 */
static ERTS_INLINE int
check_task_for_exec(Port *pp,
		    erts_aint32_t flags,
		    ErtsPortTask **execqp,
		    int *processing_busy_q_p,
		    ErtsPortTask *ptp)
{

    if (!*processing_busy_q_p) {
	/* Processing normal queue */

	ERTS_PT_DBG_CHK_TASK_QS(pp, ptp, *processing_busy_q_p);

	if ((flags & ERTS_PTS_FLG_BUSY)
	    && (ptp->u.alive.flags & ERTS_PT_FLG_WAIT_BUSY)) {

	    busy_wait_move_to_busy_queue(pp, ptp);
	    ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);

	    return 0;
	}

	if (pp->sched.taskq.local.busy.last
	    && (ptp->u.alive.flags & ERTS_PT_FLG_SIG_DEP)) {

	    int res = !check_sig_dep_move_to_busy_queue(pp, ptp);
	    ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);

	    return res;
	}

    }
    else {
	/* Processing busy queue */

	ASSERT(!(flags & ERTS_PTS_FLG_BUSY));

	ERTS_PT_DBG_CHK_TASK_QS(pp, ptp, *processing_busy_q_p);

	popped_from_busy_queue(pp, ptp, !*execqp);

	if (!*execqp) {
	    *execqp = pp->sched.taskq.local.first;
	    *processing_busy_q_p = 0;
	}

	ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);

    }

    return 1;
}

static ErtsPortTask *
fetch_in_queue(Port *pp, ErtsPortTask **execqp)
{
    ErtsPortTask *ptp;
    ErtsPortTaskHandleList *free_nshp = NULL;

    erts_port_task_sched_lock(&pp->sched);

    ptp = pp->sched.taskq.in.first;
    pp->sched.taskq.in.first = NULL;
    pp->sched.taskq.in.last = NULL;
    if (ptp)
	*execqp = ptp->u.alive.next;
    else
	erts_smp_atomic32_read_band_nob(&pp->sched.flags,
					~ERTS_PTS_FLG_HAVE_TASKS);

    
    if (pp->sched.taskq.local.busy.nosuspend)
	free_nshp = get_free_nosuspend_handles(pp);

    erts_port_task_sched_unlock(&pp->sched);

    if (free_nshp)
	free_nosuspend_handles(free_nshp);

    return ptp;
}

static ERTS_INLINE ErtsPortTask *
select_task_for_exec(Port *pp,
		     ErtsPortTask **execqp,
		     int *processing_busy_q_p)
{
    ErtsPortTask *ptp;
    erts_aint32_t flags;

    flags = select_queue_for_exec(pp, execqp, processing_busy_q_p);

    while (1) {
	ptp = *execqp;
	if (ptp)
	    *execqp = ptp->u.alive.next;
	else {
	    ptp = fetch_in_queue(pp, execqp);
	    if (!ptp)
		return NULL;
	}
	if (check_task_for_exec(pp, flags, execqp, processing_busy_q_p, ptp))
	    return ptp;
    }
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
	    case ERTS_PORT_TASK_PROC_SIG:
		ptp->u.alive.td.psig.callback(NULL,
					      ERTS_PORT_SFLG_INVALID,
					      ERTS_PROC2PORT_SIG_ABORT,
					      &ptp->u.alive.td.psig.data);
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

void
erts_port_task_abort_nosuspend_tasks(Port *pp)
{
    ErtsPortTaskHandleList *abort_list;
#ifdef ERTS_SMP
    ErtsThrPrgrDelayHandle dhndl = ERTS_THR_PRGR_DHANDLE_INVALID;
#endif

    erts_port_task_sched_lock(&pp->sched);
    erts_smp_atomic32_read_band_nob(&pp->sched.flags,
				    ~ERTS_PTS_FLG_HAVE_NS_TASKS);
    abort_list = pp->sched.taskq.local.busy.nosuspend;
    pp->sched.taskq.local.busy.nosuspend = NULL;
    erts_port_task_sched_unlock(&pp->sched);

    while (abort_list) {
#ifdef DEBUG
	ErtsPortTaskHandle *saved_pthp;
#endif
	ErtsPortTaskType type;
	ErtsPortTaskTypeData td;
	ErtsPortTaskHandle *pthp;
	ErtsPortTask *ptp;
	ErtsPortTaskHandleList *pthlp;
	erts_aint32_t old_state;

	pthlp = abort_list;
	abort_list = pthlp->u.next;

#ifdef ERTS_SMP
	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	    dhndl = erts_thr_progress_unmanaged_delay();
#endif

	pthp = &pthlp->handle;
	ptp = handle2task(pthp);
	if (!ptp) {
#ifdef ERTS_SMP
	    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
		erts_thr_progress_unmanaged_continue(dhndl);
#endif
	    schedule_port_task_handle_list_free(pthlp);
	    continue;
	}

#ifdef DEBUG
	saved_pthp = ptp->u.alive.handle;
	ERTS_SMP_READ_MEMORY_BARRIER;
	old_state = erts_smp_atomic32_read_nob(&ptp->state);
	if (old_state == ERTS_PT_STATE_SCHEDULED) {
	    ASSERT(saved_pthp == pthp);
	}
#endif

	old_state = erts_smp_atomic32_cmpxchg_nob(&ptp->state,
						  ERTS_PT_STATE_ABORTED,
						  ERTS_PT_STATE_SCHEDULED);
	if (old_state != ERTS_PT_STATE_SCHEDULED) {
	    /* Task already aborted, executing, or executed */
#ifdef ERTS_SMP
	    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
		erts_thr_progress_unmanaged_continue(dhndl);
#endif
	    schedule_port_task_handle_list_free(pthlp);
	    continue;
	}

	reset_port_task_handle(pthp);

	type = ptp->type;
	td = ptp->u.alive.td;

#ifdef ERTS_SMP
	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	    erts_thr_progress_unmanaged_continue(dhndl);
#endif
	schedule_port_task_handle_list_free(pthlp);

	abort_nosuspend_task(type, &td);
    }
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

    if (type != ERTS_PORT_TASK_PROC_SIG) {
	ptp = port_task_alloc();

	ptp->type = type;
	ptp->u.alive.flags = 0;

	erts_smp_atomic32_init_nob(&ptp->state, ERTS_PT_STATE_SCHEDULED);

	set_handle(ptp, pthp);
    }

    switch (type) {
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT: {
	va_list argp;
	va_start(argp, type);
	ptp->u.alive.td.io.event = va_arg(argp, ErlDrvEvent);
	va_end(argp);
	erts_smp_atomic_inc_relb(&erts_port_task_outstanding_io_tasks);
	break;
    }
    case ERTS_PORT_TASK_EVENT: {
	va_list argp;
	va_start(argp, type);
	ptp->u.alive.td.io.event = va_arg(argp, ErlDrvEvent);
	ptp->u.alive.td.io.event_data = va_arg(argp, ErlDrvEventData);
	va_end(argp);
	erts_smp_atomic_inc_relb(&erts_port_task_outstanding_io_tasks);
	break;
    }
    case ERTS_PORT_TASK_PROC_SIG: {
	ErtsProc2PortSigData *sigdp;
	va_list argp;
	ASSERT(!pthp);
	va_start(argp, type);
	sigdp = va_arg(argp, ErtsProc2PortSigData *);
	ptp = p2p_sig_data_to_task(sigdp);
	ptp->u.alive.td.psig.callback = va_arg(argp, ErtsProc2PortSigCallback);
 	ptp->u.alive.flags |= va_arg(argp, int);
	va_end(argp);
	if (ptp->u.alive.flags & ERTS_PT_FLG_NOSUSPEND)
	    save_nosuspend_handle(pp, ptp);
	else
	    set_handle(ptp, pthp);
	break;
    }
    default:
	break;
    }

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
    ErtsProcList *suspended;
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
    suspended = pp->suspended;
    pp->suspended = NULL;
    erts_port_task_sched_unlock(&pp->sched);
    erts_atomic32_read_bset_relb(&pp->state,
				 (ERTS_PORT_SFLG_CLOSING
				  | ERTS_PORT_SFLG_FREE),
				 ERTS_PORT_SFLG_FREE);

    erts_smp_runq_unlock(runq);

    if (erts_proclist_fetch(&suspended, NULL))
	erts_resume_processes(suspended);

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
    int processing_busy_q;
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

    prepare_exec(pp, &execq, &processing_busy_q);

    erts_smp_port_lock(pp);

    /* trace port scheduling, in */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports(pp, am_in);
    }

    fpe_was_unmasked = erts_block_fpe();

    state = erts_atomic32_read_nob(&pp->state);
    goto begin_handle_tasks;

    while (1) {
	erts_aint32_t task_state;
	ErtsPortTask *ptp;

	ptp = select_task_for_exec(pp, &execq, &processing_busy_q);
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
	    if (!(state & ERTS_PORT_SFLGS_DEAD)) {
                DTRACE_DRIVER(driver_timeout, pp);
		(*pp->drv_ptr->timeout)((ErlDrvData) pp->drv_data);
            }
	    break;
	case ERTS_PORT_TASK_INPUT:
	    reds += ERTS_PORT_REDS_INPUT;
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_ready_input, pp);
	    /* NOTE some windows drivers use ->ready_input for input and output */
	    (*pp->drv_ptr->ready_input)((ErlDrvData) pp->drv_data,
					ptp->u.alive.td.io.event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_OUTPUT:
	    reds += ERTS_PORT_REDS_OUTPUT;
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_ready_output, pp);
	    (*pp->drv_ptr->ready_output)((ErlDrvData) pp->drv_data,
					 ptp->u.alive.td.io.event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_EVENT:
	    reds += ERTS_PORT_REDS_EVENT;
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_event, pp);
	    (*pp->drv_ptr->event)((ErlDrvData) pp->drv_data,
				  ptp->u.alive.td.io.event,
				  ptp->u.alive.td.io.event_data);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_PROC_SIG:
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
	    reds += ptp->u.alive.td.psig.callback(pp,
						  state,
						  ERTS_PROC2PORT_SIG_EXEC,
						  &ptp->u.alive.td.psig.data);
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

    active = finalize_exec(pp, &execq, processing_busy_q);

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
				      ptp->u.alive.td.io.event,
				      DO_READ,
				      1);
		break;
	    case ERTS_PORT_TASK_OUTPUT:
		erts_stale_drv_select(pp->common.id,
				      ptp->u.alive.td.io.event,
				      DO_WRITE,
				      1);
		break;
	    case ERTS_PORT_TASK_EVENT:
		erts_stale_drv_select(pp->common.id,
				      ptp->u.alive.td.io.event,
				      0,
				      1);
		break;
	    case ERTS_PORT_TASK_DIST_CMD:
		break;
	    case ERTS_PORT_TASK_PROC_SIG:
		ptp->u.alive.td.psig.callback(NULL,
					      ERTS_PORT_SFLG_INVALID,
					      ERTS_PROC2PORT_SIG_ABORT_CLOSED,
					      &ptp->u.alive.td.psig.data);
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
    init_busy_caller_table_alloc();
}
