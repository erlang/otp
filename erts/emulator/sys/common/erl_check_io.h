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
 * Description:	Check I/O
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_CHECK_IO_H__
#define ERL_CHECK_IO_H__

#include "sys.h"
#include "erl_sys_driver.h"

Uint erts_check_io_size(void);
Eterm erts_check_io_info(void *);
int erts_check_io_max_files(void);
void erts_check_io_interrupt(int);
void erts_check_io_interrupt_timed(int, ErtsMonotonicTime);
void erts_check_io(int);
void erts_init_check_io(void);
#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_update_cio_locks(int enable);
#endif

extern erts_atomic_t erts_check_io_time;

typedef struct {
    ErtsPortTaskHandle task;
    erts_atomic_t executed_time;
} ErtsIoTask;

ERTS_GLB_INLINE void erts_io_notify_port_task_executed(ErtsPortTaskHandle *pthp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_io_notify_port_task_executed(ErtsPortTaskHandle *pthp)
{
    ErtsIoTask *itp = (ErtsIoTask *) (((char *) pthp) - offsetof(ErtsIoTask, task));
    erts_aint_t ci_time = erts_atomic_read_acqb(&erts_check_io_time);
    erts_atomic_set_relb(&itp->executed_time, ci_time);
}

#endif

#endif /*  ERL_CHECK_IO_H__ */

#if !defined(ERL_CHECK_IO_C__) && !defined(ERTS_ALLOC_C__)
#define ERL_CHECK_IO_INTERNAL__
#endif

#ifndef ERL_CHECK_IO_INTERNAL__
#define ERL_CHECK_IO_INTERNAL__
#include "erl_poll.h"
#include "erl_port_task.h"

#ifdef __WIN32__
/*
 * Current erts_poll implementation for Windows cannot handle
 * active events in the set of events polled.
 */
#  define ERTS_CIO_DEFER_ACTIVE_EVENTS 1
#else
#  define ERTS_CIO_DEFER_ACTIVE_EVENTS 0
#endif

typedef struct {
    Eterm inport;
    Eterm outport;
    ErtsIoTask iniotask;
    ErtsIoTask outiotask;
} ErtsDrvSelectDataState;

struct erts_nif_select_event {
    Eterm pid;
    Eterm immed;
    Uint32 refn[ERTS_REF_NUMBERS];
    Sint32 ddeselect_cnt; /* 0:  No delayed deselect in progress
                           * 1:  Do deselect before next poll
                           * >1: Countdown of ignored events
                           */
};

typedef struct {
    struct erts_nif_select_event in;
    struct erts_nif_select_event out;
} ErtsNifSelectDataState;

#endif /* #ifndef ERL_CHECK_IO_INTERNAL__ */
