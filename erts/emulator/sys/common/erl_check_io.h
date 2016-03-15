/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

#ifdef ERTS_ENABLE_KERNEL_POLL

int driver_select_kp(ErlDrvPort, ErlDrvEvent, int, int);
int driver_select_nkp(ErlDrvPort, ErlDrvEvent, int, int);
int driver_event_kp(ErlDrvPort, ErlDrvEvent, ErlDrvEventData);
int driver_event_nkp(ErlDrvPort, ErlDrvEvent, ErlDrvEventData);
Uint erts_check_io_size_kp(void);
Uint erts_check_io_size_nkp(void);
Eterm erts_check_io_info_kp(void *);
Eterm erts_check_io_info_nkp(void *);
int erts_check_io_max_files_kp(void);
int erts_check_io_max_files_nkp(void);
#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
void erts_check_io_async_sig_interrupt_kp(void);
void erts_check_io_async_sig_interrupt_nkp(void);
#endif
void erts_check_io_interrupt_kp(int);
void erts_check_io_interrupt_nkp(int);
void erts_check_io_interrupt_timed_kp(int, ErtsMonotonicTime);
void erts_check_io_interrupt_timed_nkp(int, ErtsMonotonicTime);
void erts_check_io_kp(int);
void erts_check_io_nkp(int);
void erts_init_check_io_kp(void);
void erts_init_check_io_nkp(void);
int erts_check_io_debug_kp(ErtsCheckIoDebugInfo *);
int erts_check_io_debug_nkp(ErtsCheckIoDebugInfo *);

#else /* !ERTS_ENABLE_KERNEL_POLL */

Uint erts_check_io_size(void);
Eterm erts_check_io_info(void *);
int erts_check_io_max_files(void);
#ifdef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
void erts_check_io_async_sig_interrupt(void);
#endif
void erts_check_io_interrupt(int);
void erts_check_io_interrupt_timed(int, ErtsMonotonicTime);
void erts_check_io(int);
void erts_init_check_io(void);

#endif

extern erts_smp_atomic_t erts_check_io_time;

typedef struct {
    ErtsPortTaskHandle task;
    erts_smp_atomic_t executed_time;
} ErtsIoTask;

ERTS_GLB_INLINE void erts_io_notify_port_task_executed(ErtsPortTaskHandle *pthp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_io_notify_port_task_executed(ErtsPortTaskHandle *pthp)
{
    ErtsIoTask *itp = (ErtsIoTask *) (((char *) pthp) - offsetof(ErtsIoTask, task));
    erts_aint_t ci_time = erts_smp_atomic_read_acqb(&erts_check_io_time);
    erts_smp_atomic_set_relb(&itp->executed_time, ci_time);
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

/*
 * ErtsDrvEventDataState is used by driver_event() which is almost never
 * used. We allocate ErtsDrvEventDataState separate since we dont wan't
 * the size of ErtsDrvEventState to increase due to driver_event()
 * information.
 */
typedef struct {
    Eterm port;
    ErlDrvEventData data;
    ErtsPollEvents removed_events;
#if ERTS_CIO_DEFER_ACTIVE_EVENTS
    ErtsPollEvents deferred_events;
#endif
    ErtsIoTask iotask;
} ErtsDrvEventDataState;

typedef struct {
    Eterm inport;
    Eterm outport;
    ErtsIoTask iniotask;
    ErtsIoTask outiotask;
} ErtsDrvSelectDataState;
#endif /* #ifndef ERL_CHECK_IO_INTERNAL__ */
