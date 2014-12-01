/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
 *
 * This file handles differences between different Unix systems.
 * This should be the only place with conditional compilation
 * depending on the type of OS.
 */

#ifndef _ERL_UNIX_SYS_H
#define _ERL_UNIX_SYS_H

#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#ifndef QNX
#include <memory.h>
#endif

#if defined(__sun__) && defined(__SVR4) && !defined(__EXTENSIONS__)
#   define __EXTENSIONS__
#   include <sys/types.h>
#   undef __EXTENSIONS__
#else
#   include <sys/types.h>
#endif
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include "erl_errno.h"
#include <signal.h>


#if HAVE_SYS_SOCKETIO_H
#   include <sys/socketio.h>
#endif
#if HAVE_SYS_SOCKIO_H
#   include <sys/sockio.h>
#endif

#ifdef HAVE_NET_ERRNO_H
#include <net/errno.h>
#endif

#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

#ifndef HAVE_MMAP
#   define HAVE_MMAP 0
#endif

#if HAVE_MMAP
#   include <sys/mman.h>
#endif

#if TIME_WITH_SYS_TIME
#   include <sys/time.h>
#   include <time.h>
#else
#   if HAVE_SYS_TIME_H
#       include <sys/time.h>
#   else
#       include <time.h>
#   endif
#endif

#include <sys/times.h>

#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#ifdef QNX
#include <process.h>
#include <sys/qnx_glob.h>
#endif

#include <pwd.h>

#ifndef HZ
#define HZ 60
#endif

#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
#endif
#include <netdb.h>

#ifdef HAVE_POSIX_MEMALIGN
#  define ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC 1
#endif

/*
 * Make sure that MAXPATHLEN is defined.
 */
#ifdef GETHRTIME_WITH_CLOCK_GETTIME
#undef HAVE_GETHRTIME
#define HAVE_GETHRTIME 1
#endif

#ifndef MAXPATHLEN
#   ifdef PATH_MAX
#       define MAXPATHLEN PATH_MAX
#   else
#       define MAXPATHLEN 2048
#   endif
#endif

/*
 * Min number of async threads
 */
#define  ERTS_MIN_NO_OF_ASYNC_THREADS 0

/* File descriptors are numbers anc consecutively allocated on Unix */
#define  ERTS_SYS_CONTINOUS_FD_NUMBERS

#ifndef ERTS_SMP
#  undef ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
#  define ERTS_POLL_NEED_ASYNC_INTERRUPT_SUPPORT
#endif

#ifndef ENABLE_CHILD_WAITER_THREAD
#  ifdef ERTS_SMP
#    define ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
void erts_check_children(void);
#  endif
#endif

typedef void *GETENV_STATE;

/*
** For the erl_timer_sup module.
*/
typedef time_t erts_time_t;

typedef struct timeval SysTimeval;

#define sys_gettimeofday(Arg) ((void) gettimeofday((Arg), NULL))

typedef struct tms SysTimes;

extern int erts_ticks_per_sec;

#define SYS_CLK_TCK (erts_ticks_per_sec)

#define sys_times(Arg) times(Arg)

#define ERTS_WRAP_SYS_TIMES 1
extern int erts_ticks_per_sec_wrap;
#define SYS_CLK_TCK_WRAP (erts_ticks_per_sec_wrap)
extern clock_t sys_times_wrap(void);

#ifdef HAVE_GETHRTIME
#ifdef GETHRTIME_WITH_CLOCK_GETTIME
typedef long long SysHrTime;

extern SysHrTime sys_gethrtime(void);
#define sys_init_hrtime() /* Nothing */

#else /* Real gethrtime (Solaris) */

typedef hrtime_t SysHrTime;

#define sys_gethrtime() gethrtime()
#define sys_init_hrtime() /* Nothing */

#endif /* GETHRTIME_WITH_CLOCK_GETTIME */
#endif /* HAVE_GETHRTIME */

#if (defined(HAVE_GETHRVTIME) || defined(HAVE_CLOCK_GETTIME))
typedef long long SysCpuTime;
typedef struct timespec SysTimespec;

#if defined(HAVE_GETHRVTIME)
#define sys_gethrvtime() gethrvtime()
#define sys_get_proc_cputime(t,tp) (t) = sys_gethrvtime(), \
                                   (tp).tv_sec = (time_t)((t)/1000000000LL), \
                                   (tp).tv_nsec = (long)((t)%1000000000LL)
int sys_start_hrvtime(void);
int sys_stop_hrvtime(void);

#elif defined(HAVE_CLOCK_GETTIME)
#define sys_clock_gettime(cid,tp) clock_gettime((cid),&(tp))
#define sys_get_proc_cputime(t,tp) sys_clock_gettime(CLOCK_PROCESS_CPUTIME_ID,(tp))

#endif
#endif

/* No use in having other resolutions than 1 Ms. */
#define SYS_CLOCK_RESOLUTION 1

/* These are defined in sys.c */
#if defined(SIG_SIGSET)		/* Old SysV */
RETSIGTYPE (*sys_sigset())();
#elif defined(SIG_SIGNAL)	/* Old BSD */
RETSIGTYPE (*sys_sigset())();
#else
RETSIGTYPE (*sys_sigset(int, RETSIGTYPE (*func)(int)))(int);
#endif
extern void sys_sigrelease(int);
extern void sys_sigblock(int);
extern void sys_stop_cat(void);

/*
 * Handling of floating point exceptions.
 */

#ifdef USE_ISINF_ISNAN		/* simulate finite() */
#  define isfinite(f) (!isinf(f) && !isnan(f))
#  define HAVE_ISFINITE
#elif (defined(__GNUC__) && !defined(__llvm__)) && defined(HAVE_FINITE)
/* We use finite in gcc as it emits assembler instead of
   the function call that isfinite emits. The assembler is
   significantly faster. */
#  ifdef isfinite
#     undef isfinite
#  endif
#  define isfinite finite
#  ifndef HAVE_ISFINITE
#    define HAVE_ISFINITE
#  endif
#elif defined(isfinite) && !defined(HAVE_ISFINITE)
#  define HAVE_ISFINITE
#elif !defined(HAVE_ISFINITE) && defined(HAVE_FINITE)
#  define isfinite finite
#  define HAVE_ISFINITE
#endif

#ifdef NO_FPE_SIGNALS

#define erts_get_current_fp_exception() NULL
#ifdef ERTS_SMP
#define erts_thread_init_fp_exception() do{}while(0)
#endif
#  define __ERTS_FP_CHECK_INIT(fpexnp) do {} while (0)
#  define __ERTS_FP_ERROR(fpexnp, f, Action) if (!isfinite(f)) { Action; } else {}
#  define __ERTS_FP_ERROR_THOROUGH(fpexnp, f, Action) __ERTS_FP_ERROR(fpexnp, f, Action)
#  define __ERTS_SAVE_FP_EXCEPTION(fpexnp)
#  define __ERTS_RESTORE_FP_EXCEPTION(fpexnp)

#define erts_sys_block_fpe() 0
#define erts_sys_unblock_fpe(x) do{}while(0)

#else /* !NO_FPE_SIGNALS */

extern volatile unsigned long *erts_get_current_fp_exception(void);
#ifdef ERTS_SMP
extern void erts_thread_init_fp_exception(void);
#endif
#  if (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)
#    define erts_fwait(fpexnp,f) \
	__asm__ __volatile__("fwait" : "=m"(*(fpexnp)) : "m"(f))
#  elif (defined(__powerpc__) || defined(__ppc__)) && defined(__GNUC__)
#    define erts_fwait(fpexnp,f) \
	__asm__ __volatile__("" : "=m"(*(fpexnp)) : "fm"(f))
#  elif defined(__sparc__) && defined(__linux__) && defined(__GNUC__)
#    define erts_fwait(fpexnp,f) \
	__asm__ __volatile__("" : "=m"(*(fpexnp)) : "em"(f))
#  else
#    define erts_fwait(fpexnp,f) \
	__asm__ __volatile__("" : "=m"(*(fpexnp)) : "g"(f))
#  endif
#  if (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)
     extern void erts_restore_fpu(void);
#  else
#    define erts_restore_fpu() /*empty*/
#  endif
#  if (!defined(__GNUC__) || \
       (__GNUC__ < 2) || \
       (__GNUC__ == 2 && __GNUC_MINOR < 96)) && \
      !defined(__builtin_expect)
#    define __builtin_expect(x, expected_value) (x)
#  endif
static __inline__ int erts_check_fpe(volatile unsigned long *fp_exception, double f)
{
    erts_fwait(fp_exception, f);
    if (__builtin_expect(*fp_exception == 0, 1))
       return 0;
    *fp_exception = 0;
    erts_restore_fpu();
    return 1;
}
#  undef erts_fwait
#  undef erts_restore_fpu
extern void erts_fp_check_init_error(volatile unsigned long *fp_exception);
static __inline__ void __ERTS_FP_CHECK_INIT(volatile unsigned long *fp_exception)
{
    if (__builtin_expect(*fp_exception == 0, 1))
	return;
    erts_fp_check_init_error(fp_exception);
}
#  define __ERTS_FP_ERROR(fpexnp, f, Action) do { if (erts_check_fpe((fpexnp),(f))) { Action; } } while (0)
#  define __ERTS_SAVE_FP_EXCEPTION(fpexnp) unsigned long old_erl_fp_exception = *(fpexnp)
#  define __ERTS_RESTORE_FP_EXCEPTION(fpexnp) \
              do { *(fpexnp) = old_erl_fp_exception; } while (0)
   /* This is for library calls where we don't trust the external
      code to always throw floating-point exceptions on errors. */
static __inline__ int erts_check_fpe_thorough(volatile unsigned long *fp_exception, double f)
{
    return erts_check_fpe(fp_exception, f) || !isfinite(f);
}
#  define __ERTS_FP_ERROR_THOROUGH(fpexnp, f, Action) \
  do { if (erts_check_fpe_thorough((fpexnp),(f))) { Action; } } while (0)

int erts_sys_block_fpe(void);
void erts_sys_unblock_fpe(int);

#endif /* !NO_FPE_SIGNALS */

#define ERTS_FP_CHECK_INIT(p)		__ERTS_FP_CHECK_INIT(&(p)->fp_exception)
#define ERTS_FP_ERROR(p, f, A)		__ERTS_FP_ERROR(&(p)->fp_exception, f, A)
#define ERTS_FP_ERROR_THOROUGH(p, f, A)	__ERTS_FP_ERROR_THOROUGH(&(p)->fp_exception, f, A)


#ifdef NEED_CHILD_SETUP_DEFINES
/* The child setup argv[] */
#define CS_ARGV_PROGNAME_IX	0		/* Program name		*/
#define CS_ARGV_UNBIND_IX	1		/* Unbind from cpu	*/
#define CS_ARGV_WD_IX		2		/* Working directory	*/
#define CS_ARGV_CMD_IX		3		/* Command		*/
#define CS_ARGV_FD_CR_IX	4		/* Fd close range	*/
#define CS_ARGV_DUP2_OP_IX(N)	((N) + 5)	/* dup2 operations	*/

#define CS_ARGV_NO_OF_DUP2_OPS	3		/* Number of dup2 ops	*/
#define CS_ARGV_NO_OF_ARGS	8		/* Number of arguments	*/
#endif /* #ifdef NEED_CHILD_SETUP_DEFINES */

/* Threads */
#ifdef USE_THREADS
extern int init_async(int);
extern int exit_async(void);
#endif

#define ERTS_EXIT_AFTER_DUMP _exit

#endif /* #ifndef _ERL_UNIX_SYS_H */
