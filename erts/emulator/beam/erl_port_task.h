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

#ifndef ERTS_PORT_TASK_H_BASIC_TYPES__
#define ERTS_PORT_TASK_H_BASIC_TYPES__
#include "erl_sys_driver.h"
#include "erl_smp.h"
typedef erts_smp_atomic_t ErtsPortTaskHandle;
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

typedef enum {
    ERTS_PORT_TASK_INPUT,
    ERTS_PORT_TASK_OUTPUT,
    ERTS_PORT_TASK_EVENT,
    ERTS_PORT_TASK_TIMEOUT,
    ERTS_PORT_TASK_DIST_CMD
} ErtsPortTaskType;

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
/* NOTE: Do not access any of the exported variables directly */
extern erts_smp_atomic_t erts_port_task_outstanding_io_tasks;
#endif

#define ERTS_PTS_FLG_IN_RUNQ		(((erts_aint32_t) 1) << 0)
#define ERTS_PTS_FLG_EXEC		(((erts_aint32_t) 1) << 1)
#define ERTS_PTS_FLG_HAVE_TASKS		(((erts_aint32_t) 1) << 2)
#define ERTS_PTS_FLG_EXIT		(((erts_aint32_t) 1) << 3)
#define ERTS_PTS_FLG_BUSY		(((erts_aint32_t) 1) << 4)


typedef struct ErtsPortTask_ ErtsPortTask;

typedef struct {
    Port *next;
    struct {
	ErtsPortTask *local;
	struct {
	    ErtsPortTask *first;
	    ErtsPortTask *last;
	} in;
    } taskq;
    erts_smp_atomic32_t flags;
#ifdef ERTS_SMP
    erts_mtx_t mtx;
#endif
} ErtsPortTaskSched;

ERTS_GLB_INLINE void erts_port_task_handle_init(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE int erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE void erts_port_task_init_sched(ErtsPortTaskSched *ptsp,
					       Eterm id);
ERTS_GLB_INLINE void erts_port_task_fini_sched(ErtsPortTaskSched *ptsp);
ERTS_GLB_INLINE void erts_port_task_sched_lock(ErtsPortTaskSched *ptsp);
ERTS_GLB_INLINE void erts_port_task_sched_unlock(ErtsPortTaskSched *ptsp);

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
ERTS_GLB_INLINE int erts_port_task_have_outstanding_io_tasks(void);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_port_task_handle_init(ErtsPortTaskHandle *pthp)
{
    erts_smp_atomic_init_nob(pthp, (erts_aint_t) NULL);
}

ERTS_GLB_INLINE int
erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp)
{
    return ((void *) erts_smp_atomic_read_nob(pthp)) != NULL;
}

ERTS_GLB_INLINE void
erts_port_task_init_sched(ErtsPortTaskSched *ptsp, Eterm instr_id)
{
    ptsp->next = NULL;
    ptsp->taskq.local = NULL;
    ptsp->taskq.in.first = NULL;
    ptsp->taskq.in.last = NULL;
    erts_smp_atomic32_init_nob(&ptsp->flags, 0);
#ifdef ERTS_SMP
    erts_mtx_init_x(&ptsp->mtx, "port_sched_lock", instr_id);
#endif
}

ERTS_GLB_INLINE void
erts_port_task_sched_lock(ErtsPortTaskSched *ptsp)
{
#ifdef ERTS_SMP
    erts_mtx_lock(&ptsp->mtx);
#endif
}

ERTS_GLB_INLINE void
erts_port_task_sched_unlock(ErtsPortTaskSched *ptsp)
{
#ifdef ERTS_SMP
    erts_mtx_unlock(&ptsp->mtx);
#endif
}

ERTS_GLB_INLINE void
erts_port_task_fini_sched(ErtsPortTaskSched *ptsp)
{
#ifdef ERTS_SMP
    erts_mtx_destroy(&ptsp->mtx);
#endif
}

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS

ERTS_GLB_INLINE int
erts_port_task_have_outstanding_io_tasks(void)
{
    return (erts_smp_atomic_read_acqb(&erts_port_task_outstanding_io_tasks)
	    != 0);
}

#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

#endif

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
int erts_port_task_execute(ErtsRunQueue *, Port **);
void erts_port_task_init(void);
#endif

int erts_port_task_abort(ErtsPortTaskHandle *);
int erts_port_task_schedule(Eterm,
			    ErtsPortTaskHandle *,
			    ErtsPortTaskType,
			    ...);
void erts_port_task_free_port(Port *);
int erts_port_is_scheduled(Port *);

#ifdef ERTS_SMP
void erts_enqueue_port(ErtsRunQueue *rq, Port *pp);
Port *erts_dequeue_port(ErtsRunQueue *rq);
#endif
#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif /* ERL_PORT_TASK_H__ */
#endif /* ERTS_PORT_TASK_ONLY_BASIC_TYPES__ */
