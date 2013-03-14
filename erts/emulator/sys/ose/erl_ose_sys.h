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

#ifndef _ERL_OSE_SYS_H
#define _ERL_OSE_SYS_H

#include "ose.h"
#undef NIL
#include "ramlog.h"

#include "fcntl.h"
#include "math.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/param.h"
#include "sys/time.h"
#include "time.h"
#include "dirent.h"
#include "ethread.h"

/* FIXME: configuration options */
#define ERTS_SCHED_MIN_SPIN
#define ERTS_SCHED_ONLY_POLL_SCHED_1
#define NO_SYSCONF 1
#define OPEN_MAX FOPEN_MAX

#define MAP_ANON MAP_ANONYMOUS

#ifndef HAVE_MMAP
#   define HAVE_MMAP 0
#endif

#if HAVE_MMAP
#   include "sys/mman.h"
#endif

typedef struct ErtsPollOseMsgList_ {
  struct ErtsPollOseMsgList_ *next;
  void *data;
} ErtsPollOseMsgList;

struct erts_sys_fd_type {
    SIGSELECT signo;
    int id;
    ErtsPollOseMsgList *imsgs;
    ErtsPollOseMsgList *omsgs;
    ethr_mutex mtx;
};


/*
 * Our own type of "FD's"
 */
#define ERTS_SYS_FD_TYPE struct erts_sys_fd_type*
#define NO_FSTAT_ON_SYS_FD_TYPE 1 /* They are signals, not files */

#include "sys/stat.h"

/* FIXME mremap is not defined in OSE - POSIX issue */
extern void *mremap (void *__addr, size_t __old_len, size_t __new_len,
                        int __flags, ...);

/* FIXME: mremap constants */
#define MREMAP_MAYMOVE  1
#define MREMAP_FIXED    2

typedef void *GETENV_STATE;

/*
** For the erl_timer_sup module.
*/
#define HAVE_GETHRTIME

typedef long long SysHrTime;
extern SysHrTime sys_gethrtime(void);

void sys_init_hrtime(void);

typedef time_t erts_time_t;

typedef struct timeval SysTimeval;

#define sys_gettimeofday(Arg) ((void) gettimeofday((Arg), NULL))

typedef struct {
    clock_t tms_utime;
    clock_t tms_stime;
    clock_t tms_cutime;
    clock_t tms_cstime;
} SysTimes;

extern int erts_ticks_per_sec;

#define SYS_CLK_TCK (erts_ticks_per_sec)

extern clock_t sys_times(SysTimes *buffer);

/* No use in having other resolutions than 1 Ms. */
#define SYS_CLOCK_RESOLUTION 1

#ifdef NO_FPE_SIGNALS

#define erts_get_current_fp_exception() NULL
#ifdef ERTS_SMP
#define erts_thread_init_fp_exception() do{}while(0)
#endif
#  define __ERTS_FP_CHECK_INIT(fpexnp) do {} while (0)
#  define __ERTS_FP_ERROR(fpexnp, f, Action) if (!finite(f)) { Action; } else {}
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
    return erts_check_fpe(fp_exception, f) || !finite(f);
}
#  define __ERTS_FP_ERROR_THOROUGH(fpexnp, f, Action) \
  do { if (erts_check_fpe_thorough((fpexnp),(f))) { Action; } } while (0)

int erts_sys_block_fpe(void);
void erts_sys_unblock_fpe(int);

#endif /* !NO_FPE_SIGNALS */

#define ERTS_FP_CHECK_INIT(p)		__ERTS_FP_CHECK_INIT(&(p)->fp_exception)
#define ERTS_FP_ERROR(p, f, A)		__ERTS_FP_ERROR(&(p)->fp_exception, f, A)
#define ERTS_FP_ERROR_THOROUGH(p, f, A)	__ERTS_FP_ERROR_THOROUGH(&(p)->fp_exception, f, A)

/* FIXME: force HAVE_GETPAGESIZE and stub getpagesize */
#ifndef HAVE_GETPAGESIZE
#define HAVE_GETPAGESIZE 1
#endif

extern int getpagesize(void);

#ifndef HZ
#define HZ 60
#endif

/* OSE5 doesn't provide limits.h so a number of macros should be
 * added manually */

#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif

/* Minimum and maximum values a `signed int' can hold.  */
#ifndef INT_MAX
#define INT_MAX	2147483647
#endif

#ifndef INT_MIN
#define INT_MIN	(-INT_MAX - 1)
#endif

#ifndef UINT_MAX
#  define UINT_MAX   4294967295U
#endif

#endif  /* _ERL_OSE_SYS_H */
