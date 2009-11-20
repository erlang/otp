/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
 * This file handles differences between operating systems.
 * This should be the only place with conditional compilation
 * depending on the type of OS.
 */

#ifndef _ERL_WIN_SYS_H
#define _ERL_WIN_SYS_H

#define HAS_STDARG

#ifdef __GNUC__
#ifdef pid_t
/* Really... */
#undef pid_t
#endif
#endif
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>
#include <process.h>
#include <malloc.h>
#ifndef __GNUC__
#include <direct.h>
#endif
#include "erl_errno.h"
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <sys/timeb.h>
#pragma comment(linker,"/manifestdependency:\"type='win32' "\
		"name='Microsoft.Windows.Common-Controls' "\
		"version='6.0.0.0' processorArchitecture='*' "\
		"publicKeyToken='6595b64144ccf1df' language='*'\"")

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN

/*
 * Define MAXPATHLEN in terms of MAXPATH if available.
 */

#ifndef MAXPATH
#define MAXPATH MAX_PATH
#endif /* MAXPATH */

#ifndef MAXPATHLEN
#define MAXPATHLEN MAXPATH
#endif /* MAXPATHLEN */

/*
 * Various configuration options, used to be in the Makefile.
 */

#define NO_ASINH
#define NO_ACOSH
#define NO_ATANH
#define NO_ERF
#define NO_ERFC

#define NO_SYSLOG
#define NO_SYSCONF
#define NO_DAEMON
#define NO_PWD
/*#define HAVE_MEMMOVE*/

#define strncasecmp _strnicmp

/*
 * Practial Windows specific macros.
 */

#define CreateAutoEvent(state) CreateEvent(NULL, FALSE, state, NULL)
#define CreateManualEvent(state) CreateEvent(NULL, TRUE, state, NULL)


/*
 * Our own type of "FD's"
 */
#define ERTS_SYS_FD_TYPE HANDLE
#define NO_FSTAT_ON_SYS_FD_TYPE 1 /* They are events, not files */

#define HAVE_ERTS_CHECK_IO_DEBUG
int erts_check_io_debug(void);

/*
 * For erl_time_sup
 */
#define HAVE_GETHRTIME

#define sys_init_hrtime() /* Nothing */

#define SYS_CLK_TCK 1000
#define SYS_CLOCK_RESOLUTION 1

typedef struct {
    long tv_sec;
    long tv_usec;
} SysTimeval;

typedef struct {
    clock_t tms_utime;
    clock_t tms_stime;
    clock_t tms_cutime;
    clock_t tms_cstime;
} SysTimes;

#define HAVE_INT64 1
#if defined (__GNUC__)
typedef unsigned long long Uint64;
typedef long long          Sint64;

typedef long long SysHrTime;
#else
typedef ULONGLONG Uint64;
typedef LONGLONG  Sint64;

typedef LONGLONG SysHrTime;
#endif

extern int sys_init_time(void);
extern void sys_gettimeofday(SysTimeval *tv);
extern SysHrTime sys_gethrtime(void);
extern clock_t sys_times(SysTimes *buffer);

extern char *win_build_environment(char *);

typedef struct {
    char *environment_strings;
    char *next_string;
} GETENV_STATE;

void erts_sys_env_init(void);

/*
 ** These are to avoid irritating warnings
 */
#pragma warning(disable : 4244)
#pragma warning(disable : 4018)

/*
 * Floating point support.
 */

extern volatile int erl_fp_exception;

#include <float.h>
#if defined (__GNUC__)
int _finite(double x);
#endif
#endif

/*#define NO_FPE_SIGNALS*/
#define erts_get_current_fp_exception() NULL
#define __ERTS_FP_CHECK_INIT(fpexnp) do {} while (0)
#define __ERTS_FP_ERROR(fpexnp, f, Action) if (!_finite(f)) { Action; } else {}
#define __ERTS_FP_ERROR_THOROUGH(fpexnp, f, Action) __ERTS_FP_ERROR(fpexnp, f, Action)
#define __ERTS_SAVE_FP_EXCEPTION(fpexnp)
#define __ERTS_RESTORE_FP_EXCEPTION(fpexnp)

#define ERTS_FP_CHECK_INIT(p)		__ERTS_FP_CHECK_INIT(&(p)->fp_exception)
#define ERTS_FP_ERROR(p, f, A)		__ERTS_FP_ERROR(&(p)->fp_exception, f, A)
#define ERTS_SAVE_FP_EXCEPTION(p)	__ERTS_SAVE_FP_EXCEPTION(&(p)->fp_exception)
#define ERTS_RESTORE_FP_EXCEPTION(p)	__ERTS_RESTORE_FP_EXCEPTION(&(p)->fp_exception)
#define ERTS_FP_ERROR_THOROUGH(p, f, A)	__ERTS_FP_ERROR_THOROUGH(&(p)->fp_exception, f, A)

#define erts_sys_block_fpe() 0
#define erts_sys_unblock_fpe(x) do{}while(0)

#define SIZEOF_SHORT   2
#define SIZEOF_INT     4
#define SIZEOF_LONG    4
#define SIZEOF_VOID_P  4
#define SIZEOF_SIZE_T  4
#define SIZEOF_OFF_T   4

/*
 * Seems to be missing.
 */
#ifndef __GNUC__
typedef long ssize_t;
#endif

/* Threads */
#ifdef USE_THREADS
int init_async(int);
int exit_async(void);
#endif
