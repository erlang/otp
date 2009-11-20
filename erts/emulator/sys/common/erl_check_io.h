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
 * Description:	Check I/O
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_CHECK_IO_H__
#define ERL_CHECK_IO_H__

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
void erts_check_io_interrupt_kp(int);
void erts_check_io_interrupt_nkp(int);
void erts_check_io_interrupt_timed_kp(int, long);
void erts_check_io_interrupt_timed_nkp(int, long);
void erts_check_io_kp(int);
void erts_check_io_nkp(int);
void erts_init_check_io_kp(void);
void erts_init_check_io_nkp(void);
int erts_check_io_debug_kp(void);
int erts_check_io_debug_nkp(void);

#else /* !ERTS_ENABLE_KERNEL_POLL */

Uint erts_check_io_size(void);
Eterm erts_check_io_info(void *);
int erts_check_io_max_files(void);
void erts_check_io_interrupt(int);
void erts_check_io_interrupt_timed(int, long);
void erts_check_io(int);
void erts_init_check_io(void);

#endif

#endif /*  ERL_CHECK_IO_H__ */

#if !defined(ERL_CHECK_IO_C__) && !defined(ERTS_ALLOC_C__)
#define ERL_CHECK_IO_INTERNAL__
#endif

#ifndef ERL_CHECK_IO_INTERNAL__
#define ERL_CHECK_IO_INTERNAL__
#include "erl_poll.h"
#include "erl_port_task.h"

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
    ErtsPortTaskHandle task;
} ErtsDrvEventDataState;

typedef struct {
    Eterm inport;
    Eterm outport;
    ErtsPortTaskHandle intask;
    ErtsPortTaskHandle outtask;
} ErtsDrvSelectDataState;
#endif /* #ifndef ERL_CHECK_IO_INTERNAL__ */
