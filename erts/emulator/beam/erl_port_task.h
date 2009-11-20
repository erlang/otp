/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2009. All Rights Reserved.
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
    ERTS_PORT_TASK_FREE,
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

typedef struct ErtsPortTask_ ErtsPortTask;
typedef struct ErtsPortTaskQueue_ ErtsPortTaskQueue;

typedef struct {
    Port *next;
    Port *prev;
    ErtsPortTaskQueue *taskq;
    ErtsPortTaskQueue *exe_taskq;
} ErtsPortTaskSched;

ERTS_GLB_INLINE void erts_port_task_handle_init(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE int erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE void erts_port_task_init_sched(ErtsPortTaskSched *ptsp);
#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
ERTS_GLB_INLINE int erts_port_task_have_outstanding_io_tasks(void);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_port_task_handle_init(ErtsPortTaskHandle *pthp)
{
    erts_smp_atomic_init(pthp, (long) NULL);
}

ERTS_GLB_INLINE int
erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp)
{
    return ((void *) erts_smp_atomic_read(pthp)) != NULL;
}

ERTS_GLB_INLINE void
erts_port_task_init_sched(ErtsPortTaskSched *ptsp)
{
    ptsp->next = NULL;
    ptsp->prev = NULL;
    ptsp->taskq = NULL;
    ptsp->exe_taskq = NULL;
}

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS

ERTS_GLB_INLINE int
erts_port_task_have_outstanding_io_tasks(void)
{
    return erts_smp_atomic_read(&erts_port_task_outstanding_io_tasks) != 0;
}

#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

#endif

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
int erts_port_task_execute(ErtsRunQueue *, Port **);
void erts_port_task_init(void);
#endif

int erts_port_task_abort(Eterm id, ErtsPortTaskHandle *);
int erts_port_task_schedule(Eterm,
			    ErtsPortTaskHandle *,
			    ErtsPortTaskType,
			    ErlDrvEvent,
			    ErlDrvEventData);
void erts_port_task_free_port(Port *);
int erts_port_is_scheduled(Port *);
#ifdef ERTS_SMP
ErtsMigrateResult erts_port_migrate(Port *,
				    int *,
				    ErtsRunQueue *,
				    int *,
				    ErtsRunQueue *,
				    int *);
#endif
#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif /* ERL_PORT_TASK_H__ */
#endif /* ERTS_PORT_TASK_ONLY_BASIC_TYPES__ */
