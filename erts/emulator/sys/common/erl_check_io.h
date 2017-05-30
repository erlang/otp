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

/**
 * @description Check I/O, a cross platform IO polling framework for ERTS
 *
 * @author Rickard Green
 * @author Lukas Larsson
 */

#ifndef ERL_CHECK_IO_H__
#define ERL_CHECK_IO_H__

#include "sys.h"
#include "erl_sys_driver.h"

/** @brief a structure that is used by each polling thread */
struct erts_poll_thread;

/**
 * Get the memory size of the check io framework
 */
Uint erts_check_io_size(void);
/**
 * Returns an Eterm with information about all the pollsets active at the
 * moment.
 *
 * @param proc the Process* to allocate the result on. It is passed as
 *  void * because of header include problems.
 */
Eterm erts_check_io_info(void *proc);
/**
 * Should be called when a port IO task has been executed in order to re-enable
 * or clear the information about the fd.
 *
 * @param type The type of event that has been completed.
 * @param handle The port task handle of the event.
 * @param reset A function pointer to be called when the port task handle
 *  should be reset.
 */
void erts_io_notify_port_task_executed(ErtsPortTaskType type,
                                       ErtsPortTaskHandle *handle,
                                       void (*reset)(ErtsPortTaskHandle *));
/**
 * Returns the maximum number of fds that the check io framework can handle.
 */
int erts_check_io_max_files(void);
/**
 * Called by any thread that should check for new IO events. This function will
 * not return unless erts_check_io_interrupt(pt, 1) is called by another thread.
 *
 * @param pt the poll thread structure to use.
 */
void erts_check_io(struct erts_poll_thread *pt);
/**
 * Initialize the check io framework. This function will parse the arguments
 * and delete any entries that it is interested in.
 *
 * @param argc the number of arguments
 * @param argv an array with the arguments
 */
void erts_init_check_io(int *argc, char **argv);
/**
 * Interrupt the poll thread so that it can execute other code.
 *
 * Should be called with set = 0 by the waiting thread before calling
 * erts_check_io.
 *
 * @param pt the poll thread to wake
 * @param set whether to set or clear the interrupt flag
 */
void erts_check_io_interrupt(struct erts_poll_thread *pt, int set);
/**
 * Create a new poll thread structure that is associated with the number no.
 * It is the callers responsibility that no is unique.
 */
struct erts_poll_thread* erts_create_pollset_thread(int no);
#ifdef ERTS_ENABLE_LOCK_COUNT
/**
 * Toggle lock counting on all check io locks
 */
void erts_lcnt_update_cio_locks(int enable);
#endif

typedef struct {
    ErtsPortTaskHandle task;
    ErtsSysFdType fd;
} ErtsIoTask;


#endif /*  ERL_CHECK_IO_H__ */

#if !defined(ERL_CHECK_IO_C__) && !defined(ERTS_ALLOC_C__)
#define ERL_CHECK_IO_INTERNAL__
#endif

#define ERTS_CHECK_IO_DRV_EV_STATE_LOCK_CNT 128

/* Controls how many pollsets to allocate. Fd's are hashed into
   each pollset based on the FD. When doing non-concurrent updates
   there will be one pollset per thread.
*/
extern int erts_no_pollsets;
extern int erts_no_poll_threads;


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
#  define ERTS_CIO_DEFER_ACTIVE_EVENTS 1
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
