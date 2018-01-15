/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
 * Purpose: Interrupt handling in windows.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_alloc.h"
#include "erl_thr_progress.h"
#include "erl_driver.h"
#include "../../drivers/win32/win_con.h"

#if defined(__GNUC__)
#  define WIN_SYS_INLINE __inline__
#elif defined(__WIN32__)
#  define WIN_SYS_INLINE __forceinline
#endif

erts_atomic32_t erts_break_requested;
#define ERTS_SET_BREAK_REQUESTED \
  erts_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 1)
#define ERTS_UNSET_BREAK_REQUESTED \
  erts_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 0)

extern int nohup;
HANDLE erts_sys_break_event = NULL;

void erts_do_break_handling(void)
{
    /*
     * Most functions that do_break() calls are intentionally not thread safe;
     * therefore, make sure that all threads but this one are blocked before
     * proceeding!
     */
    erts_thr_progress_block();
    /* call the break handling function, reset the flag */
    do_break();

    ResetEvent(erts_sys_break_event);
    ERTS_UNSET_BREAK_REQUESTED;

    erts_thr_progress_unblock();
}


BOOL WINAPI ctrl_handler_ignore_break(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
	return TRUE;
	break;
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
	if (nohup)
	    return TRUE;
	/* else pour through... */
    case CTRL_CLOSE_EVENT:
	erts_exit(0, "");
	break;
    }
    return TRUE;
}

void erts_set_ignore_break(void) {
    ConSetCtrlHandler(ctrl_handler_ignore_break);
    SetConsoleCtrlHandler(ctrl_handler_ignore_break, TRUE);
}

BOOL WINAPI ctrl_handler_replace_intr(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
    case CTRL_C_EVENT:
	return FALSE;
    case CTRL_BREAK_EVENT:
	SetEvent(erts_sys_break_event);
	break;
    case CTRL_LOGOFF_EVENT:
	if (nohup)
	    return TRUE;
	/* else pour through... */
    case CTRL_CLOSE_EVENT:
    case CTRL_SHUTDOWN_EVENT:
	erts_exit(0, "");
	break;
    }
    return TRUE;
}


/* Don't use ctrl-c for break handler but let it be 
   used by the shell instead (see user_drv.erl) */
void erts_replace_intr(void) {
    ConSetCtrlHandler(ctrl_handler_replace_intr);
    SetConsoleCtrlHandler(ctrl_handler_replace_intr, TRUE);
}

BOOL WINAPI ctrl_handler(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
	SetEvent(erts_sys_break_event);
	break;
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
	if (nohup)
	    return TRUE;
	/* else pour through... */
    case CTRL_CLOSE_EVENT:
	erts_exit(0, "");
	break;
    }
    return TRUE;
}

void init_break_handler()
{
    ConSetCtrlHandler(ctrl_handler);
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
}

