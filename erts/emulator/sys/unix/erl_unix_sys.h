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
#include <memory.h>

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
#include <setjmp.h>

#ifdef HAVE_SYS_SOCKETIO_H
#   include <sys/socketio.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
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

#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif

#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#include <pwd.h>

#ifndef HZ
#define HZ 60
#endif

#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
#endif
#include <netdb.h>

#ifdef HAVE_MACH_ABSOLUTE_TIME
#include <mach/mach_time.h>
#endif

#ifdef HAVE_POSIX_MEMALIGN
#  define ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC 1
#endif

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

/*
 * Min number of async threads
 */
#define  ERTS_MIN_NO_OF_ASYNC_THREADS 0

/* File descriptors are numbers anc consecutively allocated on Unix */
#define  ERTS_SYS_CONTINOUS_FD_NUMBERS


void erts_sys_env_init(void);

/*
** For the erl_timer_sup module.
*/
typedef time_t erts_time_t;

typedef struct timeval SysTimeval;

#define sys_gettimeofday(Arg) ((void) gettimeofday((Arg), NULL))

typedef struct tms SysTimes;

#define SYS_CLK_TCK (erts_sys_time_data__.r.o.ticks_per_sec)

#define sys_times(Arg) times(Arg)

#if SIZEOF_LONG == 8
typedef long ErtsMonotonicTime;
typedef long ErtsSysHrTime;
#elif SIZEOF_LONG_LONG == 8
typedef long long ErtsMonotonicTime;
typedef long long ErtsSysHrTime;
#else
#error No signed 64-bit type found...
#endif

typedef ErtsMonotonicTime ErtsSystemTime;
typedef ErtsSysHrTime ErtsSysPerfCounter;

#define ERTS_MONOTONIC_TIME_MIN ((ErtsMonotonicTime) (1ULL << 63))
#define ERTS_MONOTONIC_TIME_MAX (~ERTS_MONOTONIC_TIME_MIN)

/*
 * OS monotonic time and OS system time
 */
#undef ERTS_OS_TIMES_INLINE_FUNC_PTR_CALL__

#if defined(OS_SYSTEM_TIME_USING_CLOCK_GETTIME) \
    && defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
#  if defined(__linux__)
#    define ERTS_OS_TIMES_INLINE_FUNC_PTR_CALL__ 1
#  endif
#endif

ErtsSystemTime erts_os_system_time(void);

#undef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
#undef ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT
#undef ERTS_OS_MONOTONIC_INLINE_FUNC_PTR_CALL__

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
#  define ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT 1
#  define ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT (1000*1000*1000)
#  if defined(__linux__)
#    define ERTS_OS_MONOTONIC_INLINE_FUNC_PTR_CALL__ 1
#  endif
#elif defined(OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME)
#  define ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT 1
#  define ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT (1000*1000*1000)
#elif defined(OS_MONOTONIC_TIME_USING_GETHRTIME)
#  define ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT 1
#  define ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT (1000*1000*1000)
#elif defined(OS_MONOTONIC_TIME_USING_TIMES)
#  define ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT 1
/* Time unit determined at runtime... */
#  define ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT 0
#else /* No OS monotonic available... */
#  define ERTS_COMPILE_TIME_MONOTONIC_TIME_UNIT (1000*1000)
#endif

/*
 * erts_sys_hrtime() is the highest resolution
 * time function found. Time unit is nano-seconds.
 * It may or may not be monotonic.
 */
ErtsSysHrTime erts_sys_hrtime(void);
#define ERTS_HRTIME_UNIT (1000*1000*1000)

struct erts_sys_time_read_only_data__ {
#ifdef ERTS_OS_MONOTONIC_INLINE_FUNC_PTR_CALL__
    ErtsMonotonicTime (*os_monotonic_time)(void);
#endif
#ifdef ERTS_OS_TIMES_INLINE_FUNC_PTR_CALL__
    void (*os_times)(ErtsMonotonicTime *, ErtsSystemTime *);
#endif
    ErtsSysPerfCounter (*perf_counter)(void);
    ErtsSysPerfCounter perf_counter_unit;
    int ticks_per_sec;
};

typedef struct {
    union {
	struct erts_sys_time_read_only_data__ o;
	char align__[(((sizeof(struct erts_sys_time_read_only_data__) - 1)
		       / ASSUMED_CACHE_LINE_SIZE) + 1)
		     * ASSUMED_CACHE_LINE_SIZE];
    } r;
} ErtsSysTimeData__;

extern ErtsSysTimeData__ erts_sys_time_data__;

#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT

#ifdef ERTS_OS_MONOTONIC_INLINE_FUNC_PTR_CALL__
ERTS_GLB_INLINE
#endif
ErtsMonotonicTime erts_os_monotonic_time(void);

#ifdef ERTS_OS_TIMES_INLINE_FUNC_PTR_CALL__
ERTS_GLB_INLINE
#endif
void erts_os_times(ErtsMonotonicTime *, ErtsSystemTime *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ERTS_OS_MONOTONIC_INLINE_FUNC_PTR_CALL__

ERTS_GLB_INLINE ErtsMonotonicTime
erts_os_monotonic_time(void)
{
    return (*erts_sys_time_data__.r.o.os_monotonic_time)();
}

#endif /* ERTS_OS_MONOTONIC_INLINE_FUNC_PTR_CALL__ */

#ifdef ERTS_OS_TIMES_INLINE_FUNC_PTR_CALL__

ERTS_GLB_INLINE void
erts_os_times(ErtsMonotonicTime *mtimep, ErtsSystemTime *stimep)
{
    (*erts_sys_time_data__.r.o.os_times)(mtimep, stimep);
}

#endif /* ERTS_OS_TIMES_INLINE_FUNC_PTR_CALL__ */

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT */

/*
 * Functions for getting the performance counter
 */

ERTS_GLB_INLINE ErtsSysPerfCounter erts_sys_perf_counter(void);
#define erts_sys_perf_counter_unit() erts_sys_time_data__.r.o.perf_counter_unit

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_FORCE_INLINE ErtsSysPerfCounter
erts_sys_perf_counter()
{
    return (*erts_sys_time_data__.r.o.perf_counter)();
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/*
 * Functions for measuring CPU time
 *
 * Note that gethrvtime is time per process and clock_gettime is per thread.
 */

#if (defined(HAVE_GETHRVTIME) || defined(HAVE_CLOCK_GETTIME_CPU_TIME))
typedef long long SysCpuTime;
typedef struct timespec SysTimespec;

#if defined(HAVE_GETHRVTIME)
#define sys_gethrvtime() gethrvtime()
#define sys_get_cputime(t,tp) (t) = sys_gethrvtime(), \
        (tp).tv_sec = (time_t)((t)/1000000000LL),     \
        (tp).tv_nsec = (long)((t)%1000000000LL)
int sys_start_hrvtime(void);
int sys_stop_hrvtime(void);

#elif defined(HAVE_CLOCK_GETTIME_CPU_TIME)
#define sys_clock_gettime(cid,tp) clock_gettime((cid),&(tp))
#define sys_get_cputime(t,tp) sys_clock_gettime(CLOCK_THREAD_CPUTIME_ID,(tp))

#endif
#endif

/* No use in having other resolutions than 1 Ms. */
#define SYS_CLOCK_RESOLUTION 1

/* These are defined in sys.c */
typedef void (*SIGFUNC)(int);
extern SIGFUNC sys_signal(int, SIGFUNC);
extern void sys_sigrelease(int);
extern void sys_sigblock(int);
extern void sys_init_suspend_handler(void);
extern void erts_sys_unix_later_init(void);

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

#define erts_isfinite isfinite

#ifdef NO_FPE_SIGNALS

#define erts_get_current_fp_exception() NULL
#define erts_thread_init_fp_exception() do{}while(0)
#  define __ERTS_FP_CHECK_INIT(fpexnp) do {} while (0)
#  define __ERTS_FP_ERROR(fpexnp, f, Action) if (!isfinite(f)) { Action; } else {}
#  define __ERTS_FP_ERROR_THOROUGH(fpexnp, f, Action) __ERTS_FP_ERROR(fpexnp, f, Action)
#  define __ERTS_SAVE_FP_EXCEPTION(fpexnp)
#  define __ERTS_RESTORE_FP_EXCEPTION(fpexnp)

#define erts_sys_block_fpe() 0
#define erts_sys_unblock_fpe(x) do{}while(0)

#else /* !NO_FPE_SIGNALS */

extern volatile unsigned long *erts_get_current_fp_exception(void);
extern void erts_thread_init_fp_exception(void);
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


/* Threads */
extern int init_async(int);
extern int exit_async(void);

#define ERTS_EXIT_AFTER_DUMP _exit

#if !defined(__APPLE__) && !defined(__MACH__)
/* Some OS X versions do not allow (ab)using signal handlers like this */
#define ERTS_HAVE_TRY_CATCH 1

/* We try to simulate a try catch in C with the help of signal handlers.
 * Only use this as a very last resort, as it is not very portable and
 * quite unstable. It is also not thread safe, so make sure that only
 * one thread can call this at a time!
 */
extern void erts_sys_sigsegv_handler(int);
extern jmp_buf erts_sys_sigsegv_jmp;
#define ERTS_SYS_TRY_CATCH(EXPR,CATCH)                                  \
    do {                                                                \
        SIGFUNC prev_handler = sys_signal(SIGSEGV,                      \
                                          erts_sys_sigsegv_handler);    \
        if (!setjmp(erts_sys_sigsegv_jmp)) {                            \
            EXPR;                                                       \
        } else {                                                        \
            CATCH;                                                      \
        }                                                               \
        sys_signal(SIGSEGV,prev_handler);                               \
    } while(0)
#endif

#endif /* #ifndef _ERL_UNIX_SYS_H */
