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
#include "erl_check_io.h"
#include "dtrace-wrapper.h"
#include "lttng-wrapper.h"
#include "erl_check_io.h"
#include <stdarg.h>

/*
 * ERTS_PORT_CALLBACK_VREDS: Limit the amount of callback calls we do...
 */
#define ERTS_PORT_CALLBACK_VREDS (CONTEXT_REDS/20)

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
#ifdef USE_LTTNG_VM_TRACEPOINTS
#define LTTNG_DRIVER(TRACEPOINT, PP)                              \
    if (LTTNG_ENABLED(TRACEPOINT)) {                              \
        lttng_decl_portbuf(port_str);                             \
        lttng_decl_procbuf(proc_str);                             \
        lttng_pid_to_str(ERTS_PORT_GET_CONNECTED(PP), proc_str);  \
        lttng_port_to_str((PP), port_str);                        \
        LTTNG3(TRACEPOINT, proc_str, port_str, (PP)->name);       \
    }
#else
#define LTTNG_DRIVER(TRACEPOINT, PP) do {} while(0)
#endif

#define ERTS_LC_VERIFY_RQ(RQ, PP)				\
    do {							\
	ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));		\
	ERTS_LC_ASSERT((RQ) == erts_get_runq_port((PP)));       \
    } while (0)

#define ERTS_PT_STATE_SCHEDULED		0
#define ERTS_PT_STATE_ABORTED		1
#define ERTS_PT_STATE_EXECUTING		2

typedef union {
    struct { /* I/O tasks */
	ErlDrvEvent event;
#if ERTS_POLL_USE_SCHEDULER_POLLING
        int is_scheduler_event;
#endif
    } io;
    struct {
	ErtsProc2PortSigCallback callback;
	ErtsProc2PortSigData data;
    } psig;
} ErtsPortTaskTypeData;

struct ErtsPortTask_ {
    erts_atomic32_t state;
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
	ErtsThrPrgrLaterOp release;
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

#if ERTS_POLL_USE_SCHEDULER_POLLING
erts_atomic_t erts_port_task_outstanding_io_tasks;
#endif

static void begin_port_cleanup(Port *pp,
			       ErtsPortTask **execq,
			       int *processing_busy_q_p);

ERTS_THR_PREF_QUICK_ALLOC_IMPL(port_task,
                               ErtsPortTask,
                               1000,
                               ERTS_ALC_T_PORT_TASK)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(busy_caller_table,
				 ErtsPortTaskBusyCallerTable,
				 50,
				 ERTS_ALC_T_BUSY_CALLER_TAB)

static void
call_port_task_free(void *vptp)
{
    port_task_free((ErtsPortTask *) vptp);
}

static ERTS_INLINE void
schedule_port_task_free(ErtsPortTask *ptp)
{
    erts_schedule_thr_prgr_later_cleanup_op(call_port_task_free,
					    (void *) ptp,
					    &ptp->u.release,
					    sizeof(ErtsPortTask));
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

static ERTS_INLINE ErtsProc2PortSigData *
p2p_sig_data_init(ErtsPortTask *ptp)
{

    ptp->type = ERTS_PORT_TASK_PROC_SIG;
    ptp->u.alive.flags = ERTS_PT_FLG_SIG_DEP;
    erts_atomic32_init_nob(&ptp->state, ERTS_PT_STATE_SCHEDULED);

    ASSERT(ptp == p2p_sig_data_to_task(&ptp->u.alive.td.psig.data));

    return &ptp->u.alive.td.psig.data;
}

ErtsProc2PortSigData *
erts_port_task_alloc_p2p_sig_data(void)
{
    ErtsPortTask *ptp = port_task_alloc();

    return p2p_sig_data_init(ptp);
}

ErtsProc2PortSigData *
erts_port_task_alloc_p2p_sig_data_extra(size_t extra, void **extra_ptr)
{
    ErtsPortTask *ptp = erts_alloc(ERTS_ALC_T_PORT_TASK,
                                   sizeof(ErtsPortTask) + extra);

    *extra_ptr = ptp+1;

    return p2p_sig_data_init(ptp);
}

void
erts_port_task_free_p2p_sig_data(ErtsProc2PortSigData *sigdp)
{
    schedule_port_task_free(p2p_sig_data_to_task(sigdp));
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
		erts_atomic32_read_band_nob(
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
#ifdef DEBUG
	erts_aint32_t flags;
#endif
	pp->sched.taskq.local.busy.first = ptp;

#ifdef DEBUG
	flags = 
#endif
	    erts_atomic32_read_bor_nob(&pp->sched.flags,
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
	    erts_atomic32_read_band_nob(
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
	ASSERT(!(erts_atomic32_read_nob(&pp->sched.flags) & ERTS_PTS_FLG_HAVE_BUSY_TASKS));
	return;
    }

    ASSERT(erts_atomic32_read_nob(&pp->sched.flags) & ERTS_PTS_FLG_HAVE_BUSY_TASKS);
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
    erts_atomic_set_relb(pthp, (erts_aint_t) NULL);
}

static ERTS_INLINE ErtsPortTask *
handle2task(ErtsPortTaskHandle *pthp)
{
    return (ErtsPortTask *) erts_atomic_read_acqb(pthp);
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
reset_executed_io_task_handle(Port *prt, ErtsPortTask *ptp)
{
    if (ptp->u.alive.handle) {
	ASSERT(ptp == handle2task(ptp->u.alive.handle));
#if ERTS_POLL_USE_SCHEDULER_POLLING
        if (ptp->u.alive.td.io.is_scheduler_event) {
            if ((erts_atomic32_read_nob(&prt->state) & ERTS_PORT_SFLG_CHECK_FD_CLEANUP)) {
                erts_io_notify_port_task_executed(ptp->type, ptp->u.alive.handle,
                                                  reset_port_task_handle);
                erts_atomic32_read_band_nob(&prt->state, ~ERTS_PORT_SFLG_CHECK_FD_CLEANUP);
            } else {
                reset_port_task_handle(ptp->u.alive.handle);
            }
        } else
#endif
        {
            /* The port task handle is reset inside task_executed */
            erts_io_notify_port_task_executed(ptp->type, ptp->u.alive.handle,
                                              reset_port_task_handle);
        }
    }
}

static ERTS_INLINE void
set_handle(ErtsPortTask *ptp, ErtsPortTaskHandle *pthp)
{
    ptp->u.alive.handle = pthp;
    if (pthp) {
	erts_atomic_set_relb(pthp, (erts_aint_t) ptp);
	ASSERT(ptp == handle2task(ptp->u.alive.handle));
    }
}

static ERTS_INLINE void
set_tmp_handle(ErtsPortTask *ptp, ErtsPortTaskHandle *pthp)
{
    ptp->u.alive.handle = NULL;
    if (pthp) {
	/*
	 * IMPORTANT! Task either need to be aborted, or task handle
	 * need to be detached before thread progress has been made.
	 */
	erts_atomic_set_relb(pthp, (erts_aint_t) ptp);
    }
}


/*
 * Busy port queue management
 */

static erts_aint32_t
check_unset_busy_port_q(Port *pp,
			erts_aint32_t flags,
			ErtsPortTaskBusyPortQ *bpq)
{
    ErlDrvSizeT qsize, low;
    int resume_procs = 0;

    ASSERT(bpq);
    ERTS_LC_ASSERT(erts_lc_is_port_locked(pp));

    erts_port_task_sched_lock(&pp->sched);
    qsize = (ErlDrvSizeT) erts_atomic_read_nob(&bpq->size);
    low = (ErlDrvSizeT) erts_atomic_read_nob(&bpq->low);
    if (qsize < low) {
	erts_aint32_t mask = ~(ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q
			       | ERTS_PTS_FLG_BUSY_PORT_Q);
	flags = erts_atomic32_read_band_relb(&pp->sched.flags, mask);
	if ((flags & ERTS_PTS_FLGS_BUSY) == ERTS_PTS_FLG_BUSY_PORT_Q)
	    resume_procs = 1;
    }
    else if (flags & ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q) {
	flags = erts_atomic32_read_band_relb(&pp->sched.flags,
						 ~ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q);
	flags &= ~ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q;
    }
    erts_port_task_sched_unlock(&pp->sched);
    if (resume_procs)
	erts_port_resume_procs(pp);

    return flags;
}

static ERTS_INLINE void
aborted_proc2port_data(Port *pp, ErlDrvSizeT size)
{
    ErtsPortTaskBusyPortQ *bpq;
    erts_aint32_t flags;
    ErlDrvSizeT qsz;

    ASSERT(pp->sched.taskq.bpq);

    if (size == 0)
	return;

    bpq = pp->sched.taskq.bpq;

    qsz = (ErlDrvSizeT) erts_atomic_add_read_acqb(&bpq->size,
						      (erts_aint_t) -size);
    ASSERT(qsz + size > qsz);
    flags = erts_atomic32_read_nob(&pp->sched.flags);
    ASSERT(pp->sched.taskq.bpq);
    if ((flags & (ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q
		  | ERTS_PTS_FLG_BUSY_PORT_Q)) != ERTS_PTS_FLG_BUSY_PORT_Q)
	return;
    if (qsz < (ErlDrvSizeT) erts_atomic_read_nob(&bpq->low))
	erts_atomic32_read_bor_nob(&pp->sched.flags,
				       ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q);
}

static ERTS_INLINE void
dequeued_proc2port_data(Port *pp, ErlDrvSizeT size)
{
    ErtsPortTaskBusyPortQ *bpq;
    erts_aint32_t flags;
    ErlDrvSizeT qsz;

    ASSERT(pp->sched.taskq.bpq);

    if (size == 0)
	return;

    bpq = pp->sched.taskq.bpq;

    qsz = (ErlDrvSizeT) erts_atomic_add_read_acqb(&bpq->size,
						      (erts_aint_t) -size);
    ASSERT(qsz + size > qsz);
    flags = erts_atomic32_read_nob(&pp->sched.flags);
    if (!(flags & ERTS_PTS_FLG_BUSY_PORT_Q))
	return;
    if (qsz < (ErlDrvSizeT) erts_atomic_read_acqb(&bpq->low))
	check_unset_busy_port_q(pp, flags, bpq);
}

static ERTS_INLINE erts_aint32_t
enqueue_proc2port_data(Port *pp,
		       ErtsProc2PortSigData *sigdp,
		       erts_aint32_t flags)
{
    ErtsPortTaskBusyPortQ *bpq = pp->sched.taskq.bpq;
    if (sigdp && bpq) {
	ErlDrvSizeT size = erts_proc2port_sig_command_data_size(sigdp);
	if (size) {
	    erts_aint_t asize = erts_atomic_add_read_acqb(&bpq->size,
							      (erts_aint_t) size);
	    ErlDrvSizeT qsz = (ErlDrvSizeT) asize;

	    ASSERT(qsz - size < qsz);

	    if (!(flags & ERTS_PTS_FLG_BUSY_PORT_Q) && qsz > bpq->high) {
		flags = erts_atomic32_read_bor_acqb(&pp->sched.flags,
							ERTS_PTS_FLG_BUSY_PORT_Q);
		flags |= ERTS_PTS_FLG_BUSY_PORT_Q;
		qsz = (ErlDrvSizeT) erts_atomic_read_acqb(&bpq->size);
		if (qsz < (ErlDrvSizeT) erts_atomic_read_nob(&bpq->low)) {
		    flags = (erts_atomic32_read_bor_relb(
				 &pp->sched.flags,
				 ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q));
		    flags |= ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q;
		}
	    }
	    ASSERT(!(flags & ERTS_PTS_FLG_EXIT));
	}
    }
    return flags;
}

/*
 * erl_drv_busy_msgq_limits() is called by drivers either reading or
 * writing the limits.
 *
 * A limit of zero is interpreted as a read only request (using a
 * limit of zero would not be useful). Other values are interpreted
 * as a write-read request.
 */

void
erl_drv_busy_msgq_limits(ErlDrvPort dport, ErlDrvSizeT *lowp, ErlDrvSizeT *highp)
{
    Port *pp = erts_drvport2port(dport);
    ErtsPortTaskBusyPortQ *bpq;
    int written = 0, resume_procs = 0;
    ErlDrvSizeT low, high;

    if (pp == ERTS_INVALID_ERL_DRV_PORT || !(bpq = pp->sched.taskq.bpq)) {
	if (lowp)
	    *lowp = ERL_DRV_BUSY_MSGQ_DISABLED;
	if (highp)
	    *highp = ERL_DRV_BUSY_MSGQ_DISABLED;
	return;
    }

    low = lowp ? *lowp : 0;
    high = highp ? *highp : 0;

    erts_port_task_sched_lock(&pp->sched);

    if (low == ERL_DRV_BUSY_MSGQ_DISABLED
	|| high == ERL_DRV_BUSY_MSGQ_DISABLED) {
	/* Disable busy msgq feature */
	erts_aint32_t flags;
	pp->sched.taskq.bpq = NULL;
	flags = ~(ERTS_PTS_FLG_BUSY_PORT_Q|ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q);
	flags = erts_atomic32_read_band_acqb(&pp->sched.flags, flags);
	if ((flags & ERTS_PTS_FLGS_BUSY) == ERTS_PTS_FLG_BUSY_PORT_Q)
	    resume_procs = 1;
    }
    else {

	if (!low)
	    low = (ErlDrvSizeT) erts_atomic_read_nob(&bpq->low);
	else {
	    if (bpq->high < low)
		bpq->high = low;
	    erts_atomic_set_relb(&bpq->low, (erts_aint_t) low);
	    written = 1;
	}
    
	if (!high)
	    high = bpq->high;
	else {
	    if (low > high) {
		low = high;
		erts_atomic_set_relb(&bpq->low, (erts_aint_t) low);
	    }
	    bpq->high = high;
	    written = 1;
	}

	if (written) {
	    ErlDrvSizeT size = (ErlDrvSizeT) erts_atomic_read_nob(&bpq->size);
	    if (size > high)
		erts_atomic32_read_bor_relb(&pp->sched.flags,
						ERTS_PTS_FLG_BUSY_PORT_Q);
	    else if (size < low)
		erts_atomic32_read_bor_relb(&pp->sched.flags,
						ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q);
	}
    }

    erts_port_task_sched_unlock(&pp->sched);

    if (resume_procs)
	erts_port_resume_procs(pp);
    if (lowp)
	*lowp = low;
    if (highp)
	*highp = high;
}

/*
 * No-suspend handles.
 */

static void
free_port_task_handle_list(void *vpthlp)
{
    erts_free(ERTS_ALC_T_PT_HNDL_LIST, vpthlp);
}

static void
schedule_port_task_handle_list_free(ErtsPortTaskHandleList *pthlp)
{
    erts_schedule_thr_prgr_later_cleanup_op(free_port_task_handle_list,
					    (void *) pthlp,
					    &pthlp->u.release,
					    sizeof(ErtsPortTaskHandleList));
}

static ERTS_INLINE void
abort_signal_task(Port *pp,
                  int abort_type,
                  ErtsPortTaskType type,
                  ErtsPortTaskTypeData *tdp,
                  int bpq_data)
{

    ASSERT(type == ERTS_PORT_TASK_PROC_SIG);

    if (!bpq_data)
	tdp->psig.callback(NULL,
			   ERTS_PORT_SFLG_INVALID,
			   abort_type,
			   &tdp->psig.data);
    else {
	ErlDrvSizeT size = erts_proc2port_sig_command_data_size(&tdp->psig.data);
	tdp->psig.callback(NULL,
			   ERTS_PORT_SFLG_INVALID,
			   abort_type,
			   &tdp->psig.data);
	aborted_proc2port_data(pp, size);
    }
}


static ERTS_INLINE void
abort_nosuspend_task(Port *pp,
		     ErtsPortTaskType type,
		     ErtsPortTaskTypeData *tdp,
		     int bpq_data)
{
    abort_signal_task(pp, ERTS_PROC2PORT_SIG_ABORT_NOSUSPEND, type, tdp, bpq_data);
}

static ErtsPortTaskHandleList *
get_free_nosuspend_handles(Port *pp)
{
    ErtsPortTaskHandleList *nshp, *last_nshp = NULL;

    ERTS_LC_ASSERT(erts_port_task_sched_lock_is_locked(&pp->sched));

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
	    erts_atomic32_read_band_nob(&pp->sched.flags,
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
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));
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

    erts_inc_runq_len(runq, &runq->ports.info, ERTS_PORT_PRIO_LEVEL);

    if (ERTS_RUNQ_FLGS_GET_NOB(runq) & ERTS_RUNQ_FLG_HALTING)
	erts_non_empty_runq(runq);
}

static ERTS_INLINE Port *
pop_port(ErtsRunQueue *runq)
{
    Port *pp = runq->ports.start;
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));
    if (!pp) {
	ASSERT(!runq->ports.end);
    }
    else {
	runq->ports.start = runq->ports.start->sched.next;
	if (!runq->ports.start) {
	    ASSERT(runq->ports.end == pp);
	    runq->ports.end = NULL;
	}
	erts_dec_runq_len(runq, &runq->ports.info, ERTS_PORT_PRIO_LEVEL);
    }

    ASSERT(runq->ports.start || !runq->ports.end);
    ASSERT(runq->ports.end || !runq->ports.start);
    return pp;
}

/*
 * Task queue operations
 */

static ERTS_INLINE int
enqueue_task(Port *pp,
	     ErtsPortTask *ptp,
	     ErtsProc2PortSigData *sigdp,
	     ErtsPortTaskHandleList *ns_pthlp,
	     erts_aint32_t *flagsp)

{
    int res;
    erts_aint32_t fail_flags = ERTS_PTS_FLG_EXIT;
    erts_aint32_t flags;
    ptp->u.alive.next = NULL;
    if (ns_pthlp)
	fail_flags |= ERTS_PTS_FLG_BUSY_PORT;
    erts_port_task_sched_lock(&pp->sched);
    flags = erts_atomic32_read_nob(&pp->sched.flags);
    if (flags & fail_flags)
	res = 0;
    else {
	if (ns_pthlp) {
	    ns_pthlp->u.next = pp->sched.taskq.local.busy.nosuspend;
	    pp->sched.taskq.local.busy.nosuspend = ns_pthlp;
	}
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
	flags = enqueue_proc2port_data(pp, sigdp, flags);
	res = 1;
    }
    erts_port_task_sched_unlock(&pp->sched);
    *flagsp = flags;
    return res;
}

static ERTS_INLINE void
prepare_exec(Port *pp, ErtsPortTask **execqp, int *processing_busy_q_p)
{
    erts_aint32_t act = erts_atomic32_read_nob(&pp->sched.flags);

    if (!pp->sched.taskq.local.busy.first || (act & ERTS_PTS_FLG_BUSY_PORT)) {
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

	act = erts_atomic32_cmpxchg_nob(&pp->sched.flags, new, exp);

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
    unsigned int prof_runnable_ports;

    if (!processing_busy_q)
	pp->sched.taskq.local.first = *execq;
    else {
	pp->sched.taskq.local.busy.first = *execq;
	ASSERT(*execq);
    }

    ERTS_PT_DBG_CHK_TASK_QS(pp, *execq, processing_busy_q);

    *execq = NULL;

    act = erts_atomic32_read_nob(&pp->sched.flags);
    if (act & ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q)
	act = check_unset_busy_port_q(pp, act, pp->sched.taskq.bpq);

    prof_runnable_ports = erts_system_profile_flags.runnable_ports;
    if (prof_runnable_ports)
	erts_port_task_sched_lock(&pp->sched);

    while (1) {
	erts_aint32_t new, exp;

	new = exp = act;

	new &= ~ERTS_PTS_FLG_EXEC;
	if (act & ERTS_PTS_FLG_HAVE_TASKS)
	    new |= ERTS_PTS_FLG_IN_RUNQ;

	act = erts_atomic32_cmpxchg_relb(&pp->sched.flags, new, exp);

	ERTS_LC_ASSERT(!(act & ERTS_PTS_FLG_IN_RUNQ));
	ERTS_LC_ASSERT(!(act & ERTS_PTS_FLG_EXEC_IMM));

	if (exp == act)
	    break;
    }

    if (prof_runnable_ports | IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
	/* trace port scheduling, out */
	if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS))
	    trace_sched_ports(pp, am_out);
	if (prof_runnable_ports) {
	    if (!(act & (ERTS_PTS_FLG_EXEC_IMM|ERTS_PTS_FLG_HAVE_TASKS)))
		profile_runnable_port(pp, am_inactive);
	    erts_port_task_sched_unlock(&pp->sched);
	}
    }

    return (act & ERTS_PTS_FLG_HAVE_TASKS) != 0;
}

static ERTS_INLINE erts_aint32_t
select_queue_for_exec(Port *pp, ErtsPortTask **execqp, int *processing_busy_q_p)
{
    erts_aint32_t flags = erts_atomic32_read_nob(&pp->sched.flags);

    if (flags & ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q)
	flags = check_unset_busy_port_q(pp, flags, pp->sched.taskq.bpq);

    ERTS_PT_DBG_CHK_TASK_QS(pp, *execqp, *processing_busy_q_p);

    if (flags & ERTS_PTS_FLG_BUSY_PORT) {
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

	if ((flags & ERTS_PTS_FLG_BUSY_PORT)
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

	ASSERT(!(flags & ERTS_PTS_FLG_BUSY_PORT));

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
	erts_atomic32_read_band_nob(&pp->sched.flags,
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
 * Cut time slice
 */

int
erl_drv_consume_timeslice(ErlDrvPort dprt, int percent)
{
    Port *pp = erts_drvport2port(dprt);
    if (pp == ERTS_INVALID_ERL_DRV_PORT)
	return -1;
    if (percent < 1)
	percent = 1;
    else if (100 < percent)
	percent = 100;
    pp->reds += percent*((CONTEXT_REDS+99)/100);
    if (pp->reds < CONTEXT_REDS)
	return 0;
    pp->reds = CONTEXT_REDS;
    return 1;
}

void
erts_port_task_tmp_handle_detach(ErtsPortTaskHandle *pthp)
{
    ERTS_LC_ASSERT(erts_thr_progress_lc_is_delaying());
    reset_port_task_handle(pthp);
}

/*
 * Abort a scheduled task.
 */

int
erts_port_task_abort(ErtsPortTaskHandle *pthp)
{
    int res;
    ErtsPortTask *ptp;
    ErtsThrPrgrDelayHandle dhndl = erts_thr_progress_unmanaged_delay();

    ptp = handle2task(pthp);
    if (!ptp)
	res = -1;
    else {
	erts_aint32_t old_state;

#ifdef DEBUG
	ErtsPortTaskHandle *saved_pthp = ptp->u.alive.handle;
	ERTS_THR_READ_MEMORY_BARRIER;
	old_state = erts_atomic32_read_nob(&ptp->state);
	if (old_state == ERTS_PT_STATE_SCHEDULED) {
	    ASSERT(!saved_pthp || saved_pthp == pthp);
	}
#endif

	old_state = erts_atomic32_cmpxchg_nob(&ptp->state,
						  ERTS_PT_STATE_ABORTED,
						  ERTS_PT_STATE_SCHEDULED);
	if (old_state != ERTS_PT_STATE_SCHEDULED)
	    res = - 1; /* Task already aborted, executing, or executed */
	else {
	    reset_port_task_handle(pthp);

#if ERTS_POLL_USE_SCHEDULER_POLLING
            switch (ptp->type) {
	    case ERTS_PORT_TASK_INPUT:
	    case ERTS_PORT_TASK_OUTPUT:
                if (ptp->u.alive.td.io.is_scheduler_event) {
                    ASSERT(erts_atomic_read_nob(
                               &erts_port_task_outstanding_io_tasks) > 0);
                    erts_atomic_dec_relb(&erts_port_task_outstanding_io_tasks);
                }
		break;
	    default:
		break;
	    }
#endif

	    res = 0;
	}
    }

    erts_thr_progress_unmanaged_continue(dhndl);

    return res;
}

void
erts_port_task_abort_nosuspend_tasks(Port *pp)
{
    ErtsPortTaskHandleList *abort_list;
    ErtsThrPrgrDelayHandle dhndl = ERTS_THR_PRGR_DHANDLE_INVALID;

    erts_port_task_sched_lock(&pp->sched);
    erts_atomic32_read_band_nob(&pp->sched.flags,
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

	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	    dhndl = erts_thr_progress_unmanaged_delay();

	pthp = &pthlp->handle;
	ptp = handle2task(pthp);
	if (!ptp) {
	    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
		erts_thr_progress_unmanaged_continue(dhndl);
	    schedule_port_task_handle_list_free(pthlp);
	    continue;
	}

#ifdef DEBUG
	saved_pthp = ptp->u.alive.handle;
	ERTS_THR_READ_MEMORY_BARRIER;
	old_state = erts_atomic32_read_nob(&ptp->state);
	if (old_state == ERTS_PT_STATE_SCHEDULED) {
	    ASSERT(saved_pthp == pthp);
	}
#endif

	old_state = erts_atomic32_cmpxchg_nob(&ptp->state,
						  ERTS_PT_STATE_ABORTED,
						  ERTS_PT_STATE_SCHEDULED);
	if (old_state != ERTS_PT_STATE_SCHEDULED) {
	    /* Task already aborted, executing, or executed */
	    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
		erts_thr_progress_unmanaged_continue(dhndl);
	    schedule_port_task_handle_list_free(pthlp);
	    continue;
	}

	reset_port_task_handle(pthp);

	type = ptp->type;
	td = ptp->u.alive.td;

	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	    erts_thr_progress_unmanaged_continue(dhndl);
	schedule_port_task_handle_list_free(pthlp);

	abort_nosuspend_task(pp, type, &td, pp->sched.taskq.bpq != NULL);
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
    ErtsProc2PortSigData *sigdp = NULL;
    ErtsPortTaskHandleList *ns_pthlp = NULL;
    ErtsRunQueue *xrunq;
    ErtsThrPrgrDelayHandle dhndl;
    ErtsRunQueue *runq;
    Port *pp;
    ErtsPortTask *ptp = NULL;
    erts_aint32_t act, add_flags;
    unsigned int prof_runnable_ports;

    ERTS_LC_ASSERT(!pthp || !erts_port_task_is_scheduled(pthp));

    ASSERT(is_internal_port(id));

    dhndl = erts_thr_progress_unmanaged_delay();

    pp = erts_port_lookup_raw(id);

    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED) {
	if (pp)
	    erts_port_inc_refc(pp);
	erts_thr_progress_unmanaged_continue(dhndl);
    }

    if (type != ERTS_PORT_TASK_PROC_SIG) {
        if (!pp)
            goto fail;

	ptp = port_task_alloc();

	ptp->type = type;
	ptp->u.alive.flags = 0;

	erts_atomic32_init_nob(&ptp->state, ERTS_PT_STATE_SCHEDULED);

	set_handle(ptp, pthp);
    }

    switch (type) {
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT: {
	va_list argp;
	va_start(argp, type);
	ptp->u.alive.td.io.event = va_arg(argp, ErlDrvEvent);
#if ERTS_POLL_USE_SCHEDULER_POLLING
        ptp->u.alive.td.io.is_scheduler_event = va_arg(argp, int);
#endif
	va_end(argp);
#if ERTS_POLL_USE_SCHEDULER_POLLING
        if (ptp->u.alive.td.io.is_scheduler_event)
            erts_atomic_inc_relb(&erts_port_task_outstanding_io_tasks);
#endif
	break;
    }
    case ERTS_PORT_TASK_PROC_SIG: {
	va_list argp;
	va_start(argp, type);
	sigdp = va_arg(argp, ErtsProc2PortSigData *);
	ptp = p2p_sig_data_to_task(sigdp);
	ptp->u.alive.td.psig.callback = va_arg(argp, ErtsProc2PortSigCallback);
 	ptp->u.alive.flags |= va_arg(argp, int);
	va_end(argp);
        if (!pp)
            goto fail;

	if (!(ptp->u.alive.flags & ERTS_PT_FLG_NOSUSPEND))
	    set_tmp_handle(ptp, pthp);
	else {
	    ns_pthlp = erts_alloc(ERTS_ALC_T_PT_HNDL_LIST,
				  sizeof(ErtsPortTaskHandleList));
	    set_handle(ptp, &ns_pthlp->handle);
	}
	break;
    }
    default:
	break;
    }

    if (!enqueue_task(pp, ptp, sigdp, ns_pthlp, &act)) {
	reset_handle(ptp);
	if (ns_pthlp && !(act & ERTS_PTS_FLG_EXIT))
	    goto abort_nosuspend;
	else
	    goto fail;
    }

    add_flags = ERTS_PTS_FLG_HAVE_TASKS;
    if (ns_pthlp)
	add_flags |= ERTS_PTS_FLG_HAVE_NS_TASKS;

    prof_runnable_ports = erts_system_profile_flags.runnable_ports;
    if (prof_runnable_ports)
	erts_port_task_sched_lock(&pp->sched);

    while (1) {
	erts_aint32_t new, exp;

	if ((act & add_flags) == add_flags
	    && (act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	    goto done; /* Done */

	new = exp = act;
	new |= add_flags;
	if (!(act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	    new |= ERTS_PTS_FLG_IN_RUNQ;

	act = erts_atomic32_cmpxchg_relb(&pp->sched.flags, new, exp);

	if (exp == act) {
	    if (!(act & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
		break; /* Need to enqueue port */
	    goto done; /* Done */
	}

	if (act & ERTS_PTS_FLG_EXIT)
	    goto done; /* Died after our task insert... */
    }

    if (prof_runnable_ports) {
	if (!(act & ERTS_PTS_FLG_EXEC_IMM))
	    profile_runnable_port(pp, am_active);
	erts_port_task_sched_unlock(&pp->sched);
	prof_runnable_ports = 0;
    }

    /* Enqueue port on run-queue */

    runq = erts_port_runq(pp);

    xrunq = erts_check_emigration_need(runq, ERTS_PORT_PRIO_LEVEL);
    ERTS_LC_ASSERT(runq != xrunq);
    ERTS_LC_VERIFY_RQ(runq, pp);
    if (xrunq) {
	/* Emigrate port ... */
        erts_set_runq_port(pp, xrunq);
	erts_runq_unlock(runq);
	runq = erts_port_runq(pp);
    }

    enqueue_port(runq, pp);

    erts_runq_unlock(runq);

    erts_notify_inc_runq(runq);

done:

    if (prof_runnable_ports)
	erts_port_task_sched_unlock(&pp->sched);

    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	erts_port_dec_refc(pp);

    return 0;

abort_nosuspend:

    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	erts_port_dec_refc(pp);

    abort_nosuspend_task(pp, ptp->type, &ptp->u.alive.td, 0);

    ASSERT(ns_pthlp);
    erts_free(ERTS_ALC_T_PT_HNDL_LIST, ns_pthlp);

    ASSERT(ptp);
    port_task_free(ptp);

    return 0;

fail:

    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	erts_port_dec_refc(pp);

    if (ptp) {
        if (ptp->type == ERTS_PORT_TASK_PROC_SIG)
            abort_signal_task(pp, ERTS_PROC2PORT_SIG_ABORT,
                              ptp->type, &ptp->u.alive.td, 0);
	port_task_free(ptp);
    }

    if (ns_pthlp)
	erts_free(ERTS_ALC_T_PT_HNDL_LIST, ns_pthlp);

    return -1;
}

void
erts_port_task_free_port(Port *pp)
{
    erts_aint32_t flags;
    ErtsRunQueue *runq;

    ERTS_LC_ASSERT(erts_lc_is_port_locked(pp));
    ASSERT(!(erts_atomic32_read_nob(&pp->state) & ERTS_PORT_SFLGS_DEAD));

    runq = erts_port_runq(pp);
    erts_port_task_sched_lock(&pp->sched);
    flags = erts_atomic32_read_bor_relb(&pp->sched.flags,
					    ERTS_PTS_FLG_EXIT);
    erts_port_task_sched_unlock(&pp->sched);
    erts_atomic32_read_bset_relb(&pp->state,
				 (ERTS_PORT_SFLG_CONNECTED
				  | ERTS_PORT_SFLG_EXITING
				  | ERTS_PORT_SFLG_CLOSING
				  | ERTS_PORT_SFLG_FREE),
				 ERTS_PORT_SFLG_FREE);

    erts_runq_unlock(runq);

    if (!(flags & (ERTS_PTS_FLG_IN_RUNQ|ERTS_PTS_FLG_EXEC)))
	begin_port_cleanup(pp, NULL, NULL);
}

/*
 * Execute scheduled tasks of a port.
 *
 * erts_port_task_execute() is called by scheduler threads between
 * scheduling of processes. Run-queue lock should be held by caller.
 */

void
erts_port_task_execute(ErtsRunQueue *runq, Port **curr_port_pp)
{
    Port *pp;
    ErtsPortTask *execq;
    int processing_busy_q;
    int vreds = 0;
    int reds = 0;
    int fpe_was_unmasked;
    erts_aint32_t state;
    int active;
    Uint64 start_time = 0;
    ErtsSchedulerData *esdp = runq->scheduler;
#if ERTS_POLL_USE_SCHEDULER_POLLING
    erts_aint_t io_tasks_executed = 0;
#endif
    ERTS_MSACC_PUSH_STATE_M();

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));

    pp = pop_port(runq);
    if (!pp) {
	goto done;
    }

    ERTS_LC_VERIFY_RQ(runq, pp);

    erts_runq_unlock(runq);

    *curr_port_pp = pp;
    
    if (erts_sched_stat.enabled) {
	Uint old = ERTS_PORT_SCHED_ID(pp, esdp->no);
	int migrated = old && old != esdp->no;

	erts_spin_lock(&erts_sched_stat.lock);
	erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].total_executed++;
	erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].executed++;
	if (migrated) {
	    erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].total_migrated++;
	    erts_sched_stat.prio[ERTS_PORT_PRIO_LEVEL].migrated++;
	}
	erts_spin_unlock(&erts_sched_stat.lock);
    }

    prepare_exec(pp, &execq, &processing_busy_q);

    erts_port_lock(pp);

    /* trace port scheduling, in */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports(pp, am_in);
    }

    fpe_was_unmasked = erts_block_fpe();

    state = erts_atomic32_read_nob(&pp->state);
    pp->reds = ERTS_PORT_REDS_EXECUTE;
    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_PORT);
    goto begin_handle_tasks;

    while (1) {
	erts_aint32_t task_state;
	ErtsPortTask *ptp;

	ptp = select_task_for_exec(pp, &execq, &processing_busy_q);
	if (!ptp)
	    break;

	task_state = erts_atomic32_cmpxchg_nob(&ptp->state,
						   ERTS_PT_STATE_EXECUTING,
						   ERTS_PT_STATE_SCHEDULED);
	if (task_state != ERTS_PT_STATE_SCHEDULED) {
	    ASSERT(task_state == ERTS_PT_STATE_ABORTED);
	    goto aborted_port_task;
	}

	if (erts_system_monitor_long_schedule != 0) {
	    start_time = erts_timestamp_millis();
	}

	ERTS_LC_ASSERT(erts_lc_is_port_locked(pp));
	ERTS_CHK_NO_PROC_LOCKS;
	ASSERT(pp->drv_ptr);

	switch (ptp->type) {
	case ERTS_PORT_TASK_TIMEOUT:
	    reset_handle(ptp);
	    if (!ERTS_PTMR_IS_TIMED_OUT(pp))
		reds = 0;
	    else {
		ERTS_PTMR_CLEAR(pp);
		reds = ERTS_PORT_REDS_TIMEOUT;
		if (!(state & ERTS_PORT_SFLGS_DEAD)) {
		    DTRACE_DRIVER(driver_timeout, pp);
		    LTTNG_DRIVER(driver_timeout, pp);
                    if (IS_TRACED_FL(pp, F_TRACE_RECEIVE))
                        trace_port(pp, am_receive, am_timeout);
		    (*pp->drv_ptr->timeout)((ErlDrvData) pp->drv_data);
		}
	    }
	    break;
	case ERTS_PORT_TASK_INPUT:
	    reds = ERTS_PORT_REDS_INPUT;
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_ready_input, pp);
            LTTNG_DRIVER(driver_ready_input, pp);
	    /* NOTE some windows drivers use ->ready_input
	       for input and output */
	    (*pp->drv_ptr->ready_input)((ErlDrvData) pp->drv_data,
					ptp->u.alive.td.io.event);
	    reset_executed_io_task_handle(pp, ptp);
#if ERTS_POLL_USE_SCHEDULER_POLLING
            if (ptp->u.alive.td.io.is_scheduler_event)
                io_tasks_executed++;
#endif
	    break;
	case ERTS_PORT_TASK_OUTPUT:
	    reds = ERTS_PORT_REDS_OUTPUT;
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
            DTRACE_DRIVER(driver_ready_output, pp);
            LTTNG_DRIVER(driver_ready_output, pp);
	    (*pp->drv_ptr->ready_output)((ErlDrvData) pp->drv_data,
					 ptp->u.alive.td.io.event);
	    reset_executed_io_task_handle(pp, ptp);
#if ERTS_POLL_USE_SCHEDULER_POLLING
            if (ptp->u.alive.td.io.is_scheduler_event)
                io_tasks_executed++;
#endif
	    break;
	case ERTS_PORT_TASK_PROC_SIG: {
	    ErtsProc2PortSigData *sigdp = &ptp->u.alive.td.psig.data;
	    reset_handle(ptp);
	    ASSERT((state & ERTS_PORT_SFLGS_DEAD) == 0);
	    if (!pp->sched.taskq.bpq)
		reds = ptp->u.alive.td.psig.callback(pp,
						     state,
						     ERTS_PROC2PORT_SIG_EXEC,
						     sigdp);
	    else {
		ErlDrvSizeT size = erts_proc2port_sig_command_data_size(sigdp);
		reds = ptp->u.alive.td.psig.callback(pp,
						     state,
						     ERTS_PROC2PORT_SIG_EXEC,
						     sigdp);
		dequeued_proc2port_data(pp, size);
	    }
	    break;
	}
	case ERTS_PORT_TASK_DIST_CMD:
	    reset_handle(ptp);
	    reds = erts_dist_command(pp, CONTEXT_REDS - pp->reds);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT,
		     "Invalid port task type: %d\n",
		     (int) ptp->type);
	    break;
	}

	reds += erts_port_driver_callback_epilogue(pp, &state);

	if (start_time != 0) {
	    Sint64 diff = erts_timestamp_millis() - start_time;
	    if (diff > 0 && (Uint) diff >  erts_system_monitor_long_schedule) {
		monitor_long_schedule_port(pp,ptp->type,(Uint) diff);
	    }
	}
	start_time = 0;

    aborted_port_task:
	schedule_port_task_free(ptp);

    begin_handle_tasks:
	if (state & ERTS_PORT_SFLG_FREE) {
	    reds += ERTS_PORT_REDS_FREE;
	    begin_port_cleanup(pp, &execq, &processing_busy_q);

	    break;
	}

	vreds += ERTS_PORT_CALLBACK_VREDS;
	reds += ERTS_PORT_CALLBACK_VREDS;

	pp->reds += reds;
	reds = 0;

	if (pp->reds >= CONTEXT_REDS)
	    break;
    }

    erts_unblock_fpe(fpe_was_unmasked);
    ERTS_MSACC_POP_STATE_M();

#if ERTS_POLL_USE_SCHEDULER_POLLING
    if (io_tasks_executed) {
        ASSERT(erts_atomic_read_nob(&erts_port_task_outstanding_io_tasks)
	       >= io_tasks_executed);
        erts_atomic_add_relb(&erts_port_task_outstanding_io_tasks,
				 -1*io_tasks_executed);
    }
#endif

    ASSERT(runq == erts_get_runq_port(pp));

    active = finalize_exec(pp, &execq, processing_busy_q);

    reds = pp->reds - vreds;

    erts_port_release(pp);

    *curr_port_pp = NULL;

    erts_runq_lock(runq);
 
    if (active) {
	ErtsRunQueue *xrunq;

	ASSERT(!(erts_atomic32_read_nob(&pp->state) & ERTS_PORT_SFLGS_DEAD));

	xrunq = erts_check_emigration_need(runq, ERTS_PORT_PRIO_LEVEL);
	ERTS_LC_ASSERT(runq != xrunq);
	ERTS_LC_VERIFY_RQ(runq, pp);
	if (!xrunq) {
	    enqueue_port(runq, pp);
	    /* No need to notify ourselves about inc in runq. */
	}
	else {
	    /* Emigrate port... */
            erts_set_runq_port(pp, xrunq);
	    erts_runq_unlock(runq);

	    xrunq = erts_port_runq(pp);
	    enqueue_port(xrunq, pp);
	    erts_runq_unlock(xrunq);
	    erts_notify_inc_runq(xrunq);

	    erts_runq_lock(runq);
	}
    }

 done:

    runq->scheduler->reductions += reds;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));
    ERTS_PORT_REDUCTIONS_EXECUTED(esdp, runq, reds);
}

static void
release_port(void *vport)
{
    erts_port_dec_refc((Port *) vport);
}

static void
schedule_release_port(void *vport) {
  Port *pp = (Port*)vport;
  /* This is only used when a port release was ordered from a non-scheduler */
  erts_schedule_thr_prgr_later_op(release_port,
				  (void *) pp,
				  &pp->common.u.release);
}


static void
begin_port_cleanup(Port *pp, ErtsPortTask **execqp, int *processing_busy_q_p)
{
    int i, max;
    ErtsPortTaskBusyCallerTable *tabp;
    ErtsPortTask *qs[3];
    ErtsPortTaskHandleList *free_nshp = NULL;
    ErtsProcList *plp;

    ERTS_LC_ASSERT(erts_lc_is_port_locked(pp));

    /*
     * Abort remaining tasks...
     *
     * We want to process queues in the following order in order
     * to preserve signal ordering guarantees:
     *  1. Local busy queue
     *  2. Local queue
     *  3. In queue
     */

    max = 0;
    if (!execqp) {
	if (pp->sched.taskq.local.busy.first)
	    qs[max++] = pp->sched.taskq.local.busy.first;
	if (pp->sched.taskq.local.first)
	    qs[max++] = pp->sched.taskq.local.first;
    }
    else {
	if (*processing_busy_q_p) {
	    if (*execqp)
		qs[max++] = *execqp;
	    if (pp->sched.taskq.local.first)
		qs[max++] = pp->sched.taskq.local.first;
	}
	else {
	    if (pp->sched.taskq.local.busy.first)
		qs[max++] = pp->sched.taskq.local.busy.first;
	    if (*execqp)
		qs[max++] = *execqp;
	}
	*execqp = NULL;
	*processing_busy_q_p = 0;
    }
    pp->sched.taskq.local.busy.first = NULL;
    pp->sched.taskq.local.busy.last = NULL;
    pp->sched.taskq.local.first = NULL;
    tabp = pp->sched.taskq.local.busy.table;
    if (tabp) {
	int bix;
	for (bix = 0; bix < ERTS_PORT_TASK_BUSY_CALLER_TABLE_BUCKETS; bix++) {
	    ErtsPortTaskBusyCaller *bcp = tabp->bucket[bix];
	    while (bcp) {
		ErtsPortTaskBusyCaller *free_bcp = bcp;
		bcp = bcp->next;
		if (free_bcp != &tabp->pre_alloc_busy_caller)
		    erts_free(ERTS_ALC_T_BUSY_CALLER, free_bcp);
	    }
	}

	busy_caller_table_free(tabp);
	pp->sched.taskq.local.busy.table = NULL;
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
	    state = erts_atomic32_read_nob(&ptp->state);
	    if (state == ERTS_PT_STATE_ABORTED)
		goto aborted_port_task;

	    state = erts_atomic32_cmpxchg_nob(&ptp->state,
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
				      ERTS_Port2ErlDrvPort(pp),
				      ptp->u.alive.td.io.event,
				      DO_READ,
				      1);
		break;
	    case ERTS_PORT_TASK_OUTPUT:
		erts_stale_drv_select(pp->common.id,
				      ERTS_Port2ErlDrvPort(pp),
				      ptp->u.alive.td.io.event,
				      DO_WRITE,
				      1);
		break;
	    case ERTS_PORT_TASK_DIST_CMD:
		break;
	    case ERTS_PORT_TASK_PROC_SIG: {
		ErtsProc2PortSigData *sigdp = &ptp->u.alive.td.psig.data;
		if (!pp->sched.taskq.bpq)
		    ptp->u.alive.td.psig.callback(NULL,
						  ERTS_PORT_SFLG_INVALID,
						  ERTS_PROC2PORT_SIG_ABORT_CLOSED,
						  sigdp);
		else {
		    ErlDrvSizeT size = erts_proc2port_sig_command_data_size(sigdp);
		    ptp->u.alive.td.psig.callback(NULL,
						  ERTS_PORT_SFLG_INVALID,
						  ERTS_PROC2PORT_SIG_ABORT_CLOSED,
						  sigdp);
		    aborted_proc2port_data(pp, size);
		}
		break;
	    }
	    default:
		erts_exit(ERTS_ABORT_EXIT,
			 "Invalid port task type: %d\n",
			 (int) ptp->type);
	    }

	aborted_port_task:
	    schedule_port_task_free(ptp);
	}
    }

    erts_atomic32_read_band_nob(&pp->sched.flags,
				    ~(ERTS_PTS_FLG_HAVE_BUSY_TASKS
				      |ERTS_PTS_FLG_HAVE_TASKS
				      |ERTS_PTS_FLGS_BUSY));

    erts_port_task_sched_lock(&pp->sched);

    /* Cleanup nosuspend handles... */
    free_nshp = (pp->sched.taskq.local.busy.nosuspend
		 ? get_free_nosuspend_handles(pp)
		 : NULL);
    ASSERT(!pp->sched.taskq.local.busy.nosuspend);

    /* Make sure not to leave any processes suspended on the port... */
    plp = pp->suspended;
    pp->suspended = NULL;

    erts_port_task_sched_unlock(&pp->sched);

    if (free_nshp)
	free_nosuspend_handles(free_nshp);

    if (erts_proclist_fetch(&plp, NULL)) {
#ifdef USE_VM_PROBES
	if (DTRACE_ENABLED(process_port_unblocked)) {
	    DTRACE_CHARBUF(port_str, 16);
	    DTRACE_CHARBUF(pid_str, 16);
	    ErtsProcList* plp2 = plp;

	    erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)), "%T", pp->common.id);
	    while (plp2 != NULL) {
		erts_snprintf(pid_str, sizeof(DTRACE_CHARBUF_NAME(pid_str)), "%T", plp2->u.pid);
		DTRACE2(process_port_unblocked, pid_str, port_str);
	    }
	}
#endif
	erts_resume_processes(plp);
    }

    /*
     * Schedule cleanup of port structure...
     */
    /* We might not be a scheduler, eg. traceing to port we are sys_msg_dispatcher */
    if (!erts_get_scheduler_data()) {
      erts_schedule_misc_aux_work(1, schedule_release_port, (void*)pp);
    } else {
      /* Has to be more or less immediate to release any driver */
      erts_schedule_thr_prgr_later_op(release_port,
				      (void *) pp,
				      &pp->common.u.release);
    }
}


void
erts_enqueue_port(ErtsRunQueue *rq, Port *pp)
{
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));
    ASSERT(rq == erts_get_runq_port(pp));
    ASSERT(erts_atomic32_read_nob(&pp->sched.flags) & ERTS_PTS_FLG_IN_RUNQ);
    enqueue_port(rq, pp);
}

Port *
erts_dequeue_port(ErtsRunQueue *rq)
{
    Port *pp;
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));
    pp = pop_port(rq);
    ASSERT(!pp || rq == erts_get_runq_port(pp));
    ASSERT(!pp || (erts_atomic32_read_nob(&pp->sched.flags)
		   & ERTS_PTS_FLG_IN_RUNQ));
    return pp;
}


/*
 * Initialize the module.
 */
void
erts_port_task_init(void)
{
#if ERTS_POLL_USE_SCHEDULER_POLLING
    erts_atomic_init_nob(&erts_port_task_outstanding_io_tasks,
                         (erts_aint_t) 0);
#endif    
    init_port_task_alloc(erts_no_schedulers + erts_no_poll_threads
                         + 1); /* aux_thread */
    init_busy_caller_table_alloc();
}
