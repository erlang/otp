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
#ifndef __ERL_VXWORKS_SYS_H__
#define __ERL_VXWORKS_SYS_H__

/* stdarg.h don't work without this one... */
#include <vxWorks.h>

#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>
#define index StringIndexFunctionThatIDontWantDeclared
#include <string.h>
#undef index



#include <sys/times.h>
#include <time.h>/* xxxP */

#include <dirent.h>
#include <sys/stat.h>

/* xxxP from unix_sys.h  begin */

/*
 * Make sure that MAXPATHLEN is defined.
 */

#ifndef MAXPATHLEN
#   ifdef PATH_MAX
#       define MAXPATHLEN PATH_MAX
#   else
#       define MAXPATHLEN 2048
#   endif
#endif

/* xxxP end */


/* Unimplemented math functions */
#define NO_ASINH
#define NO_ACOSH
#define NO_ATANH
#define NO_ERF
#define NO_ERFC

/* Stuff that is useful for port programs, drivers, etc */
#ifndef VXWORKS
#define VXWORKS
#endif

#define DONT_USE_MAIN
#define NO_FSYNC
#define NO_MKDIR_MODE
#define NO_UMASK
#define NO_SYMBOLIC_LINKS
#define NO_DEVICE_FILES
#define NO_UID
#define NO_ACCESS
#define NO_FCNTL
#define NO_SYSLOG
#define NO_SYSCONF
#define NO_PWD			/* XXX Means what? */
#define NO_DAEMON
/* This chooses ~250 reductions instead of 500 in config.h */
#if (CPU == CPU32)
#define SLOW_PROCESSOR
#endif

/*
 * Even though we does not always have small memories on VxWorks
 * we certainly does not have virtual memory.
 */
#if !defined(LARGE_MEMORY)
#define SMALL_MEMORY
#endif

/*************** Floating point exception handling ***************/

/* There are no known ways to customize the handling of invalid floating
   point operations, such as matherr() or ieee_handler(), in VxWorks 5.1. */

#if (CPU == MC68040 || CPU == CPU32 || CPU == PPC860 || CPU == PPC32 ||        \
     CPU == PPC603 || CPU == PPC604 || CPU == SIMSPARCSOLARIS)

/* VxWorks 5.1 on Motorola 68040 never generates SIGFPE, but sets the
   result of invalid floating point ops to Inf and NaN - unfortunately
   the way to test for those values is undocumented and hidden in a
   "private" include file...  */
/* Haven't found any better way, as of yet, for ppc860 xxxP*/

#include <private/mathP.h>
#define NO_FPE_SIGNALS
#define erts_get_current_fp_exception() NULL
#define __ERTS_FP_CHECK_INIT(fpexnp) do {} while (0)
#define __ERTS_FP_ERROR(fpexnp, f, Action) if (isInf(f) || isNan(f)) { Action; } else {}
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

#if (CPU == PPC603)
/* Need fppLib to change the Floating point registers  
   (fix_registers in sys.c)*/

#include <fppLib.h>

#endif /* PPC603 */

#else

Unsupported CPU value !

#endif

typedef void *GETENV_STATE;

#define HAVE_GETHRTIME

extern int erts_clock_rate;

#define SYS_CLK_TCK (erts_clock_rate)

#define SYS_CLOCK_RESOLUTION 1

typedef struct _vxworks_tms {
    clock_t tms_utime;
    clock_t tms_stime;
    clock_t tms_cutime;
    clock_t tms_cstime;
} SysTimes;

typedef long long SysHrTime;

typedef struct timeval SysTimeval;

extern int sys_init_hrtime(void);
extern SysHrTime sys_gethrtime(void);
extern void sys_gettimeofday(SysTimeval *tvp);
extern clock_t sys_times(SysTimes *t);

#define SIZEOF_SHORT   2
#define SIZEOF_INT     4
#define SIZEOF_LONG    4
#define SIZEOF_VOID_P  4
#define SIZEOF_SIZE_T  4
#define SIZEOF_OFF_T   4

/*
 * Temporary buffer *only* used in sys code.
 */
#define SYS_TMP_BUF_SIZE 65536

/* Need to be able to interrupt erts_poll_wait() from signal handler */
#define ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT

#endif /* __ERL_VXWORKS_SYS_H__ */
