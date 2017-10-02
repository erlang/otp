/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2017. All Rights Reserved.
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

#ifndef ERTS_PORT_TASK_H_BASIC_TYPES__
#define ERTS_PORT_TASK_H_BASIC_TYPES__
#include "erl_sys_driver.h"
#include "erl_threads.h"
#define ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_port.h"
#undef ERL_PORT_GET_PORT_TYPE_ONLY__
typedef erts_atomic_t ErtsPortTaskHandle;
#endif

#ifndef ERTS_PORT_TASK_ONLY_BASIC_TYPES__
#ifndef ERL_PORT_TASK_H__
#define ERL_PORT_TASK_H__

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#if (defined(ERL_PROCESS_C__) \
     || defined(ERL_PORT_TASK_C__) \
     || defined(ERL_IO_C__) \
     || (ERTS_GLB_INLINE_INCL_FUNC_DEF \
	 && defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF)))
#define ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif

#define ERTS_PT_FLG_WAIT_BUSY		(1 << 0)
#define ERTS_PT_FLG_SIG_DEP		(1 << 1)
#define ERTS_PT_FLG_NOSUSPEND		(1 << 2)
#define ERTS_PT_FLG_REF			(1 << 3)
#define ERTS_PT_FLG_BAD_OUTPUT		(1 << 4)

typedef enum {
    ERTS_PORT_TASK_INPUT,
    ERTS_PORT_TASK_OUTPUT,
    ERTS_PORT_TASK_TIMEOUT,
    ERTS_PORT_TASK_DIST_CMD,
    ERTS_PORT_TASK_PROC_SIG
} ErtsPortTaskType;

#define ERTS_PTS_FLG_IN_RUNQ			(((erts_aint32_t) 1) <<  0)
#define ERTS_PTS_FLG_EXEC			(((erts_aint32_t) 1) <<  1)
#define ERTS_PTS_FLG_HAVE_TASKS			(((erts_aint32_t) 1) <<  2)
#define ERTS_PTS_FLG_EXIT			(((erts_aint32_t) 1) <<  3)
#define ERTS_PTS_FLG_BUSY_PORT			(((erts_aint32_t) 1) <<  4)
#define ERTS_PTS_FLG_BUSY_PORT_Q		(((erts_aint32_t) 1) <<  5)
#define ERTS_PTS_FLG_CHK_UNSET_BUSY_PORT_Q	(((erts_aint32_t) 1) <<  6)
#define ERTS_PTS_FLG_HAVE_BUSY_TASKS		(((erts_aint32_t) 1) <<  7)
#define ERTS_PTS_FLG_HAVE_NS_TASKS		(((erts_aint32_t) 1) <<  8)
#define ERTS_PTS_FLG_PARALLELISM		(((erts_aint32_t) 1) <<  9)
#define ERTS_PTS_FLG_FORCE_SCHED		(((erts_aint32_t) 1) << 10)
#define ERTS_PTS_FLG_EXITING			(((erts_aint32_t) 1) << 11)
#define ERTS_PTS_FLG_EXEC_IMM			(((erts_aint32_t) 1) << 12)

#define ERTS_PTS_FLGS_BUSY \
    (ERTS_PTS_FLG_BUSY_PORT | ERTS_PTS_FLG_BUSY_PORT_Q)

#define ERTS_PTS_FLGS_FORCE_SCHEDULE_OP		\
    (ERTS_PTS_FLG_EXIT				\
     | ERTS_PTS_FLG_HAVE_BUSY_TASKS		\
     | ERTS_PTS_FLG_HAVE_TASKS			\
     | ERTS_PTS_FLG_EXEC			\
     | ERTS_PTS_FLG_EXEC_IMM			\
     | ERTS_PTS_FLG_FORCE_SCHED			\
     | ERTS_PTS_FLG_EXITING)

#define ERTS_PORT_TASK_DEFAULT_BUSY_PORT_Q_HIGH			8192
#define ERTS_PORT_TASK_DEFAULT_BUSY_PORT_Q_LOW			4096

typedef struct {
    ErlDrvSizeT high;
    erts_atomic_t low;
    erts_atomic_t size;
}  ErtsPortTaskBusyPortQ;

typedef struct ErtsPortTask_ ErtsPortTask;
typedef struct ErtsPortTaskBusyCallerTable_ ErtsPortTaskBusyCallerTable;
typedef struct ErtsPortTaskHandleList_ ErtsPortTaskHandleList;

typedef struct {
    Port *next;
    struct {
	struct {
	    struct {
		ErtsPortTask *first;
		ErtsPortTask *last;
		ErtsPortTaskBusyCallerTable *table;
		ErtsPortTaskHandleList *nosuspend;
	    } busy;
	    ErtsPortTask *first;
	} local;
	struct {
	    ErtsPortTask *first;
	    ErtsPortTask *last;
	} in;
	ErtsPortTaskBusyPortQ *bpq;
    } taskq;
    erts_atomic32_t flags;
    erts_mtx_t mtx;
} ErtsPortTaskSched;

ERTS_GLB_INLINE void erts_port_task_handle_init(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE int erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE void erts_port_task_pre_init_sched(ErtsPortTaskSched *ptsp,
						   ErtsPortTaskBusyPortQ *bpq);
ERTS_GLB_INLINE void erts_port_task_init_sched(ErtsPortTaskSched *ptsp,
					       Eterm id);
ERTS_GLB_INLINE void erts_port_task_fini_sched(ErtsPortTaskSched *ptsp);
ERTS_GLB_INLINE void erts_port_task_sched_lock(ErtsPortTaskSched *ptsp);
ERTS_GLB_INLINE void erts_port_task_sched_unlock(ErtsPortTaskSched *ptsp);
ERTS_GLB_INLINE int erts_port_task_sched_lock_is_locked(ErtsPortTaskSched *ptsp);
ERTS_GLB_INLINE void erts_port_task_sched_enter_exiting_state(ErtsPortTaskSched *ptsp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_port_task_handle_init(ErtsPortTaskHandle *pthp)
{
    erts_atomic_init_nob(pthp, (erts_aint_t) NULL);
}

ERTS_GLB_INLINE int
erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp)
{
    return ((void *) erts_atomic_read_acqb(pthp)) != NULL;
}

ERTS_GLB_INLINE void erts_port_task_pre_init_sched(ErtsPortTaskSched *ptsp,
						   ErtsPortTaskBusyPortQ *bpq)
{
    if (bpq) {
	erts_aint_t low = (erts_aint_t) ERTS_PORT_TASK_DEFAULT_BUSY_PORT_Q_LOW;
	erts_atomic_init_nob(&bpq->low, low);
 	bpq->high = (ErlDrvSizeT) ERTS_PORT_TASK_DEFAULT_BUSY_PORT_Q_HIGH;
	erts_atomic_init_nob(&bpq->size, (erts_aint_t) 0);
    }
    ptsp->taskq.bpq = bpq;
}

ERTS_GLB_INLINE void
erts_port_task_init_sched(ErtsPortTaskSched *ptsp, Eterm instr_id)
{
    char *lock_str = "port_sched_lock";
    ptsp->next = NULL;
    ptsp->taskq.local.busy.first = NULL;
    ptsp->taskq.local.busy.last = NULL;
    ptsp->taskq.local.busy.table = NULL;
    ptsp->taskq.local.busy.nosuspend = NULL;
    ptsp->taskq.local.first = NULL;
    ptsp->taskq.in.first = NULL;
    ptsp->taskq.in.last = NULL;
    erts_atomic32_init_nob(&ptsp->flags, 0);
    erts_mtx_init(&ptsp->mtx, lock_str, instr_id, ERTS_LOCK_FLAGS_CATEGORY_IO);
}

ERTS_GLB_INLINE void
erts_port_task_sched_lock(ErtsPortTaskSched *ptsp)
{
    erts_mtx_lock(&ptsp->mtx);
}

ERTS_GLB_INLINE void
erts_port_task_sched_unlock(ErtsPortTaskSched *ptsp)
{
    erts_mtx_unlock(&ptsp->mtx);
}

ERTS_GLB_INLINE int
erts_port_task_sched_lock_is_locked(ErtsPortTaskSched *ptsp)
{
#if defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_mtx_is_locked(&ptsp->mtx);
#else
    return 0;
#endif
}


ERTS_GLB_INLINE void
erts_port_task_fini_sched(ErtsPortTaskSched *ptsp)
{
    erts_mtx_destroy(&ptsp->mtx);
}

ERTS_GLB_INLINE void
erts_port_task_sched_enter_exiting_state(ErtsPortTaskSched *ptsp)
{
    erts_atomic32_read_bor_nob(&ptsp->flags, ERTS_PTS_FLG_EXITING);
}

#endif

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
void erts_port_task_execute(ErtsRunQueue *, Port **);
void erts_port_task_init(void);
#endif

/* generated for 'port_task' quick allocator */
void erts_port_task_pre_alloc_init_thread(void);

void erts_port_task_tmp_handle_detach(ErtsPortTaskHandle *);
int erts_port_task_abort(ErtsPortTaskHandle *);

void erts_port_task_abort_nosuspend_tasks(Port *);

int erts_port_task_schedule(Eterm,
			    ErtsPortTaskHandle *,
			    ErtsPortTaskType,
			    ...);
void erts_port_task_free_port(Port *);
ErtsProc2PortSigData *erts_port_task_alloc_p2p_sig_data(void);
ErtsProc2PortSigData *erts_port_task_alloc_p2p_sig_data_extra(size_t extra, void **extra_ptr);
void erts_port_task_free_p2p_sig_data(ErtsProc2PortSigData *sigdp);

void erts_enqueue_port(ErtsRunQueue *rq, Port *pp);
Port *erts_dequeue_port(ErtsRunQueue *rq);
#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif /* ERL_PORT_TASK_H__ */
#endif /* ERTS_PORT_TASK_ONLY_BASIC_TYPES__ */
