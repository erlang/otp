/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2017. All Rights Reserved.
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
 * Description: Thread library for use in the ERTS and other OTP
 *              applications.
 * Author: Rickard Green
 */

#ifndef ETHREAD_H__
#define ETHREAD_H__

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#  include "ethread_header_config.h"
#endif

#include <stdlib.h>
#include "ethread_inline.h"
#include "erl_errno.h"

#if defined(DEBUG)
#  define ETHR_DEBUG
#endif

#if defined(__PPC__) || defined (__POWERPC)
/* OSE compiler should be fixed! */
#define __ppc__
#endif

#if defined(ETHR_DEBUG)
#  undef ETHR_XCHK
#  define  ETHR_XCHK 1
#else
#  ifndef ETHR_XCHK
#    define ETHR_XCHK 0
#  endif
#endif

#if defined(ETHR_DEBUG) || !defined(ETHR_INLINE) || ETHR_XCHK \
    || (defined(__GNUC__) && defined(ERTS_MIXED_CYGWIN_VC)) \
    || (defined(__GNUC__) && defined(ERTS_MIXED_MSYS_VC))
#  undef ETHR_INLINE
#  define ETHR_INLINE 
#  undef ETHR_FORCE_INLINE
#  define ETHR_FORCE_INLINE
#  undef ETHR_TRY_INLINE_FUNCS
#endif

/* Assume 64-byte cache line size */
#define ETHR_CACHE_LINE_SIZE ASSUMED_CACHE_LINE_SIZE
#define ETHR_CACHE_LINE_MASK (ETHR_CACHE_LINE_SIZE - 1)

#define ETHR_CACHE_LINE_ALIGN_SIZE(SZ) \
  (((((SZ) - 1) / ETHR_CACHE_LINE_SIZE) + 1) * ETHR_CACHE_LINE_SIZE)

#ifndef ETHR_INLINE_FUNC_NAME_
#  define ETHR_INLINE_FUNC_NAME_(X) X
#endif

#if !defined(__func__)
#  if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L
#    if !defined(__GNUC__) ||  __GNUC__ < 2
#      define __func__ "[unknown_function]"
#    else
#      define __func__ __FUNCTION__
#    endif
#  endif
#endif

int ethr_assert_failed(const char *file, int line, const char *func, char *a);
#ifdef ETHR_DEBUG
#define ETHR_ASSERT(A) \
  ((void) ((A) ? 1 : ethr_assert_failed(__FILE__, __LINE__, __func__, #A)))
#else
#define ETHR_ASSERT(A) ((void) 1)
#endif

#if defined(__GNUC__)
#  define ETHR_PROTO_NORETURN__ void __attribute__((noreturn))
#  define ETHR_IMPL_NORETURN__ void
#elif defined(__WIN32__) && defined(_MSC_VER)
#  define ETHR_PROTO_NORETURN__ __declspec(noreturn) void
#  define ETHR_IMPL_NORETURN__ __declspec(noreturn) void
#else
#  define ETHR_PROTO_NORETURN__ void
#  define ETHR_IMPL_NORETURN__ void
#endif

#if defined(ETHR_PTHREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The pthread implementation                                                *
\*                                                                           */

#if defined(__linux__) && !defined(_GNU_SOURCE)
#error "_GNU_SOURCE not defined. Please, compile all files with -D_GNU_SOURCE."
#endif

#ifdef ETHR_HAVE_PTHREAD_SETNAME_NP_1
#define _DARWIN_C_SOURCE
#endif

#if defined(ETHR_NEED_NPTL_PTHREAD_H)
#include <nptl/pthread.h>
#elif defined(ETHR_HAVE_MIT_PTHREAD_H)
#include <pthread/mit/pthread.h>
#elif defined(ETHR_HAVE_PTHREAD_H)
#include <pthread.h>
#endif

/* Types */

typedef pthread_t ethr_tid;

typedef pthread_key_t ethr_tsd_key;

#define ETHR_HAVE_ETHR_SIG_FUNCS 1

#if defined(PURIFY) || defined(VALGRIND)
#  define ETHR_FORCE_PTHREAD_RWLOCK
#  define ETHR_FORCE_PTHREAD_MUTEX
#endif

#if !defined(ETHR_FORCE_PTHREAD_RWLOCK)
#  define ETHR_USE_OWN_RWMTX_IMPL__
#endif

#if !defined(ETHR_FORCE_PTHREAD_MUTEX) && 0
#  define ETHR_USE_OWN_MTX_IMPL__
#endif

#elif defined(ETHR_WIN32_THREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The native win32 threads implementation                                   *
\*                                                                           */

#if !defined(_WIN32_WINNT)
#error "_WIN32_WINNT not defined. Please, compile all files with -D_WIN32_WINNT=0x0403"
#elif _WIN32_WINNT < 0x0403
#error "_WIN32_WINNT defined to a value less than 0x0403. Please, compile all files with -D_WIN32_WINNT=0x0403"
#endif

#ifdef WIN32_LEAN_AND_MEAN
#  define ETHR_WIN32_LEAN_AND_MEAN_ALREADY_DEFINED
#else
#  define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#ifndef ETHR_WIN32_LEAN_AND_MEAN_ALREADY_DEFINED
#  undef WIN32_LEAN_AND_MEAN
#endif

#if defined(_MSC_VER)

#if ETHR_SIZEOF_LONG == 4
#define ETHR_HAVE_INT32_T 1
typedef long ethr_sint32_t;
typedef unsigned long ethr_uint32_t;
#endif

#if ETHR_SIZEOF___INT64 == 8
#define ETHR_HAVE_INT64_T 1
typedef __int64 ethr_sint64_t;
typedef unsigned __int64 ethr_uint64_t;
#endif

#endif

struct ethr_join_data_;

/* Types */
typedef struct {
    long id;
    struct ethr_join_data_ *jdata;
} ethr_tid; /* thread id type */

typedef DWORD ethr_tsd_key;

#undef ETHR_HAVE_ETHR_SIG_FUNCS

#define ETHR_USE_OWN_RWMTX_IMPL__

#define ETHR_YIELD() (Sleep(0), 0)

#else /* No supported thread lib found */

#ifdef ETHR_NO_SUPP_THR_LIB_NOT_FATAL
#define ETHR_NO_THREAD_LIB
#else
#error "No supported thread lib found"
#endif

#endif

#ifndef ETHR_HAVE_INT32_T
#if ETHR_SIZEOF_INT == 4
#define ETHR_HAVE_INT32_T 1
typedef int ethr_sint32_t;
typedef unsigned int ethr_uint32_t;
#elif ETHR_SIZEOF_LONG == 4
#define ETHR_HAVE_INT32_T 1
typedef long ethr_sint32_t;
typedef unsigned long ethr_uint32_t;
#endif
#endif

#ifndef ETHR_HAVE_INT64_T
#if ETHR_SIZEOF_INT == 8
#define ETHR_HAVE_INT64_T 1
typedef int ethr_sint64_t;
typedef unsigned int ethr_uint64_t;
#elif ETHR_SIZEOF_LONG == 8
#define ETHR_HAVE_INT64_T 1
typedef long ethr_sint64_t;
typedef unsigned long ethr_uint64_t;
#elif ETHR_SIZEOF_LONG_LONG == 8
#define ETHR_HAVE_INT64_T 1
typedef long long ethr_sint64_t;
typedef unsigned long long ethr_uint64_t;
#endif
#endif

#if ETHR_SIZEOF_PTR == 4
#ifndef ETHR_HAVE_INT32_T
#error "No 32-bit integer type found"
#endif
typedef ethr_sint32_t ethr_sint_t;
typedef ethr_uint32_t ethr_uint_t;
#elif ETHR_SIZEOF_PTR == 8
#ifndef ETHR_HAVE_INT64_T
#error "No 64-bit integer type found"
#endif
typedef ethr_sint64_t ethr_sint_t;
typedef ethr_uint64_t ethr_uint_t;
#endif

#if defined(ETHR_SIZEOF___INT128_T) && ETHR_SIZEOF___INT128_T == 16
#define ETHR_HAVE_INT128_T
typedef __int128_t ethr_sint128_t;
typedef __uint128_t ethr_uint128_t;
#endif

#define ETHR_FATAL_ERROR__(ERR) \
  ethr_fatal_error__(__FILE__, __LINE__, __func__, (ERR))

ETHR_PROTO_NORETURN__ ethr_fatal_error__(const char *file,
					 int line,
					 const char *func,
					 int err);

#if !ETHR_AT_LEAST_GCC_VSN__(2, 96, 0)
#define __builtin_expect(X, Y) (X)
#endif

#if ETHR_AT_LEAST_GCC_VSN__(3, 1, 1)
#  define ETHR_CHOOSE_EXPR __builtin_choose_expr
#else
#  define ETHR_CHOOSE_EXPR(B, E1, E2) ((B) ? (E1) : (E2))
#endif

#if ((defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))) \
     || (defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_AMD64))))
#  define ETHR_X86_RUNTIME_CONF__

#  define ETHR_X86_RUNTIME_CONF_HAVE_META(feature)              \
    (__builtin_expect(ethr_runtime__.conf.have_##feature != 0, 1))
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_META(feature)           \
    (__builtin_expect(ethr_runtime__.conf.have_##feature == 0, 0))

#  define ETHR_X86_RUNTIME_CONF_HAVE_DW_CMPXCHG__       \
    ETHR_X86_RUNTIME_CONF_HAVE_META(dw_cmpxchg)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_DW_CMPXCHG__    \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(dw_cmpxchg)
#  define ETHR_X86_RUNTIME_CONF_HAVE_SSE2__     \
    ETHR_X86_RUNTIME_CONF_HAVE_META(sse2)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__  \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(sse2)
#  define ETHR_X86_RUNTIME_CONF_HAVE_RDTSCP__   \
    ETHR_X86_RUNTIME_CONF_HAVE_META(rdtscp)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_RDTSCP__        \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(rdtscp)
#  define ETHR_X86_RUNTIME_CONF_HAVE_CONSTANT_TSC__     \
    ETHR_X86_RUNTIME_CONF_HAVE_META(constant_tsc)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_CONSTANT_TSC__   \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(nonstop_tsc)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NONSTOP_TSC__     \
    ETHR_X86_RUNTIME_CONF_HAVE_META(nonstop_tsc)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_NONSTOP_TSC__   \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(nonstop_tsc)
#  define ETHR_X86_RUNTIME_CONF_HAVE_TSC_RELIABLE__     \
    ETHR_X86_RUNTIME_CONF_HAVE_META(tsc_reliable)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_TSC_RELIABLE_TSC__   \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(tsc_reliable)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NONSTOP_TSC_S3__     \
    ETHR_X86_RUNTIME_CONF_HAVE_META(nonstop_tsc_s3)
#  define ETHR_X86_RUNTIME_CONF_HAVE_NO_NONSTOP_TSC_S3__        \
    ETHR_X86_RUNTIME_CONF_HAVE_NO_META(nonstop_tsc_s3)

#endif

#if (defined(__GNUC__) \
     && !defined(ETHR_PPC_HAVE_LWSYNC) \
     && !defined(ETHR_PPC_HAVE_NO_LWSYNC) \
     && (defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)))
#  define ETHR_PPC_RUNTIME_CONF__

#  define ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__ \
  (__builtin_expect(ethr_runtime__.conf.have_lwsync != 0, 1))  
#  define ETHR_PPC_RUNTIME_CONF_HAVE_NO_LWSYNC__ \
  (__builtin_expect(ethr_runtime__.conf.have_lwsync == 0, 0))
#endif

typedef struct {
#if defined(ETHR_X86_RUNTIME_CONF__)
    int have_dw_cmpxchg;
    int have_sse2;
    int have_rdtscp;
    int have_constant_tsc;
    int have_tsc_reliable;
    int have_nonstop_tsc;
    int have_nonstop_tsc_s3;
#endif
#if defined(ETHR_PPC_RUNTIME_CONF__)
    int have_lwsync;
#endif
    int dummy;
} ethr_runtime_conf_t;


typedef union {
    ethr_runtime_conf_t conf;
    char pad__[ETHR_CACHE_LINE_ALIGN_SIZE(sizeof(ethr_runtime_conf_t))+ETHR_CACHE_LINE_SIZE];
} ethr_runtime_t;


extern ethr_runtime_t ethr_runtime__;

/* For native CPU-optimised atomics, spinlocks, and rwlocks. */
#if !defined(ETHR_DISABLE_NATIVE_IMPLS)
#  if defined(__GNUC__)
#    if defined(ETHR_PREFER_GCC_NATIVE_IMPLS)
#      include "gcc/ethread.h"
#    elif defined(ETHR_PREFER_LIBATOMIC_OPS_NATIVE_IMPLS)
#      include "libatomic_ops/ethread.h"
#    endif
#    if !defined(ETHR_HAVE_NATIVE_ATOMIC32) && !defined(ETHR_HAVE_NATIVE_ATOMIC64)
#      if ETHR_SIZEOF_PTR == 4
#        if defined(__i386__)
#          include "i386/ethread.h"
#        elif (defined(__powerpc__)||defined(__ppc__))&&!defined(__powerpc64__)
#          include "ppc32/ethread.h"
#        elif defined(__sparc__)
#          include "sparc32/ethread.h"
#        elif defined(__tile__)
#          include "tile/ethread.h"
#        endif
#      elif ETHR_SIZEOF_PTR == 8
#        if defined(__x86_64__)
#          include "x86_64/ethread.h"
#        elif defined(__sparc__) && defined(__arch64__)
#          include "sparc64/ethread.h"
#        endif
#      endif
#      if ETHR_HAVE_GCC___ATOMIC_BUILTINS
#        include "gcc/ethread.h"
#      endif
#      include "libatomic_ops/ethread.h"
#      include "gcc/ethread.h"
#    endif
#  elif defined(ETHR_HAVE_LIBATOMIC_OPS)
#    include "libatomic_ops/ethread.h"
#  elif defined(ETHR_WIN32_THREADS)
#    include "win/ethread.h"
#  endif
#endif /* !ETHR_DISABLE_NATIVE_IMPLS */

#if !defined(ETHR_HAVE_NATIVE_ATOMIC32) && !defined(ETHR_HAVE_NATIVE_ATOMIC64) && !defined(ETHR_DISABLE_NATIVE_IMPLS) && defined(ETHR_SMP_REQUIRE_NATIVE_IMPLS)
#error "No native ethread implementation found. If you want to use fallbacks you have to disable native ethread support with configure."
#endif

#include "ethr_atomics.h" /* The atomics API */

#if defined(__GNUC__)
#  ifndef ETHR_SPIN_BODY
#    if defined(__i386__) || defined(__x86_64__)
#      define ETHR_SPIN_BODY __asm__ __volatile__("rep;nop" : : : "memory")
#    elif defined(__ia64__)
#      define ETHR_SPIN_BODY __asm__ __volatile__("hint @pause" : : : "memory")
#    elif defined(__sparc__)
#      define ETHR_SPIN_BODY __asm__ __volatile__("membar #LoadLoad")
#    else
#      define ETHR_SPIN_BODY ETHR_COMPILER_BARRIER
#    endif
#  endif
#elif defined(ETHR_WIN32_THREADS)
#  ifndef ETHR_SPIN_BODY
#    define ETHR_SPIN_BODY do {YieldProcessor();ETHR_COMPILER_BARRIER;} while(0)
#  endif
#endif

#define ETHR_YIELD_AFTER_BUSY_LOOPS 50

#ifndef ETHR_SPIN_BODY
#  define ETHR_SPIN_BODY ETHR_COMPILER_BARRIER
#endif

#ifndef ETHR_YIELD
#  if defined(ETHR_HAVE_SCHED_YIELD)
#    ifdef ETHR_HAVE_SCHED_H
#      include <sched.h>
#    endif
#    include <errno.h>
#    if defined(ETHR_SCHED_YIELD_RET_INT)
#      define ETHR_YIELD() (sched_yield() < 0 ? errno : 0)
#    else
#      define ETHR_YIELD() (sched_yield(), 0)
#    endif
#  elif defined(ETHR_HAVE_PTHREAD_YIELD)
#    if defined(ETHR_PTHREAD_YIELD_RET_INT)
#      define ETHR_YIELD() pthread_yield()
#    else
#      define ETHR_YIELD() (pthread_yield(), 0)
#    endif
#  else
#    define ETHR_YIELD() (ethr_compiler_barrier(), 0)
#  endif
#endif

#if defined(VALGRIND)
/* mutex as fallback for spinlock for VALGRIND. */
#  undef ETHR_HAVE_NATIVE_SPINLOCKS
#  undef ETHR_HAVE_NATIVE_RWSPINLOCKS
#else
#  include "ethr_optimized_fallbacks.h"
#endif

typedef struct {
    void *(*thread_create_prepare_func)(void);
    void (*thread_create_parent_func)(void *);
    void (*thread_create_child_func)(void *);
} ethr_init_data;

#define ETHR_INIT_DATA_DEFAULT_INITER {NULL, NULL, NULL}

typedef struct {
    void *(*alloc)(size_t);
    void *(*realloc)(void *, size_t);
    void (*free)(void *);
} ethr_memory_allocator;

#define ETHR_MEM_ALLOC_DEF_INITER__ {NULL, NULL, NULL}

typedef struct {
    ethr_memory_allocator std;
    ethr_memory_allocator sl;
    ethr_memory_allocator ll;
} ethr_memory_allocators;

#define ETHR_MEM_ALLOCS_DEF_INITER__					\
  {ETHR_MEM_ALLOC_DEF_INITER__,						\
   ETHR_MEM_ALLOC_DEF_INITER__,						\
   ETHR_MEM_ALLOC_DEF_INITER__}

typedef struct {
    ethr_memory_allocators mem;
    int reader_groups;
    int main_threads;
} ethr_late_init_data;

#define ETHR_LATE_INIT_DATA_DEFAULT_INITER				\
  {ETHR_MEM_ALLOCS_DEF_INITER__, 0, 0}

typedef struct {
    int detached;			/* boolean (default false) */
    int suggested_stack_size;		/* kilo words (default sys dependent) */
    char *name;                         /* max 14 char long (default no-name) */
} ethr_thr_opts;

#define ETHR_THR_OPTS_DEFAULT_INITER {0, -1, NULL}

#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)
#  define ETHR_NEED_SPINLOCK_PROTOTYPES__
#  define ETHR_NEED_RWSPINLOCK_PROTOTYPES__
#endif

int ethr_init(ethr_init_data *);
int ethr_late_init(ethr_late_init_data *);
int ethr_install_exit_handler(void (*funcp)(void));
int ethr_thr_create(ethr_tid *, void * (*)(void *), void *, ethr_thr_opts *);
int ethr_thr_join(ethr_tid, void **);
int ethr_thr_detach(ethr_tid);
void ethr_thr_exit(void *);
ethr_tid ethr_self(void);
int ethr_getname(ethr_tid, char *, size_t);
void ethr_setname(char *);
int ethr_equal_tids(ethr_tid, ethr_tid);

int ethr_tsd_key_create(ethr_tsd_key *,char *);
int ethr_tsd_key_delete(ethr_tsd_key);
int ethr_tsd_set(ethr_tsd_key, void *);
void *ethr_tsd_get(ethr_tsd_key);

void *ethr_get_stacklimit(void);
int ethr_set_stacklimit(void *limit);

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#include <signal.h>
int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset);
int ethr_sigwait(const sigset_t *set, int *sig);
int ethr_kill(const ethr_tid tid, const int sig);
#endif

void ethr_compiler_barrier(void);

#if defined(ETHR_HAVE_NATIVE_SPINLOCKS)
typedef ethr_native_spinlock_t ethr_spinlock_t;
#elif defined(__WIN32__)
typedef CRITICAL_SECTION ethr_spinlock_t;
#else
typedef pthread_mutex_t ethr_spinlock_t;
#endif

#ifdef ETHR_NEED_SPINLOCK_PROTOTYPES__
int ethr_spinlock_init(ethr_spinlock_t *);
int ethr_spinlock_destroy(ethr_spinlock_t *);
void ethr_spin_unlock(ethr_spinlock_t *);
void ethr_spin_lock(ethr_spinlock_t *);
#endif

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_init)(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_SPINLOCKS
    ethr_native_spinlock_init(lock);
    return 0;
#elif defined(__WIN32__)
    if (!InitializeCriticalSectionAndSpinCount((CRITICAL_SECTION *) lock, INT_MAX))
	return ethr_win_get_errno__();
    return 0;
#else
    return pthread_mutex_init((pthread_mutex_t *) lock, NULL);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_destroy)(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_SPINLOCKS
    return ethr_native_spinlock_destroy(lock);
#elif defined(__WIN32__)
    DeleteCriticalSection((CRITICAL_SECTION *) lock);
    return 0;
#else
    return pthread_mutex_destroy((pthread_mutex_t *) lock);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_spin_unlock)(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_SPINLOCKS
    ethr_native_spin_unlock(lock);
#elif defined(__WIN32__)
    LeaveCriticalSection((CRITICAL_SECTION *) lock);
#else
    int err = pthread_mutex_unlock((pthread_mutex_t *) lock);
    if (err)
	ETHR_FATAL_ERROR__(err);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_spin_lock)(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_SPINLOCKS
    ethr_native_spin_lock(lock);
#elif defined(__WIN32__)
    EnterCriticalSection((CRITICAL_SECTION *) lock);
#else
    int err = pthread_mutex_lock((pthread_mutex_t *) lock);
    if (err)
	ETHR_FATAL_ERROR__(err);
#endif
}

#endif /* ETHR_TRY_INLINE_FUNCS */

typedef struct ethr_ts_event_ ethr_ts_event; /* Needed by ethr_mutex.h */

#if defined(ETHR_WIN32_THREADS)
#  include "win/ethr_event.h"
#elif defined(ETHR_PTHREADS)
#  include "pthread/ethr_event.h"
#endif

int ethr_set_main_thr_status(int, int);
int ethr_get_main_thr_status(int *);

struct ethr_ts_event_ {
    ethr_ts_event *next;
    ethr_ts_event *prev;
    ethr_event event;
    void *udata;
    ethr_atomic32_t uaflgs;
    unsigned uflgs;
    unsigned iflgs;		/* for ethr lib only */
    short rgix;			/* for ethr lib only */
    short mtix;			/* for ethr lib only */
};

#define ETHR_TS_EV_ETHREAD	(((unsigned) 1) << 0)
#define ETHR_TS_EV_INITED	(((unsigned) 1) << 1)
#define ETHR_TS_EV_TMP		(((unsigned) 1) << 2)
#define ETHR_TS_EV_MAIN_THR	(((unsigned) 1) << 3)

int ethr_get_tmp_ts_event__(ethr_ts_event **tsepp);
int ethr_free_ts_event__(ethr_ts_event *tsep);
int ethr_make_ts_event__(ethr_ts_event **tsepp);

#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHREAD_IMPL__)
ethr_ts_event *ethr_get_ts_event(void);
void ethr_leave_ts_event(ethr_ts_event *);
#endif

#if defined(ETHR_PTHREADS)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHREAD_IMPL__)

extern pthread_key_t ethr_ts_event_key__;

static ETHR_INLINE ethr_ts_event *
ETHR_INLINE_FUNC_NAME_(ethr_get_ts_event)(void)
{
    ethr_ts_event *tsep = pthread_getspecific(ethr_ts_event_key__);
    if (!tsep) {
	int res = ethr_make_ts_event__(&tsep);
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);
	ETHR_ASSERT(tsep);
    }
    return tsep;
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_leave_ts_event)(ethr_ts_event *tsep)
{

}

#endif

#elif defined(ETHR_WIN32_THREADS)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHREAD_IMPL__)

extern DWORD ethr_ts_event_key__;

static ETHR_INLINE ethr_ts_event *
ETHR_INLINE_FUNC_NAME_(ethr_get_ts_event)(void)
{
    ethr_ts_event *tsep = TlsGetValue(ethr_ts_event_key__);
    if (!tsep) {
	int res = ethr_get_tmp_ts_event__(&tsep);
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);
	ETHR_ASSERT(tsep);
    }
    return tsep;
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_leave_ts_event)(ethr_ts_event *tsep)
{
    if (tsep->iflgs & ETHR_TS_EV_TMP) {
	int res = ethr_free_ts_event__(tsep);
	if (res != 0)
	    ETHR_FATAL_ERROR__(res);
    }
}

#endif

#endif

#include "ethr_mutex.h" /* Need atomic declarations and tse */

#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
typedef ethr_native_rwlock_t ethr_rwlock_t;
#else
typedef ethr_rwmutex ethr_rwlock_t;
#endif

#ifdef ETHR_NEED_RWSPINLOCK_PROTOTYPES__
int ethr_rwlock_init(ethr_rwlock_t *);
int ethr_rwlock_destroy(ethr_rwlock_t *);
void ethr_read_unlock(ethr_rwlock_t *);
void ethr_read_lock(ethr_rwlock_t *);
void ethr_write_unlock(ethr_rwlock_t *);
void ethr_write_lock(ethr_rwlock_t *);
#endif

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_init)(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
    ethr_native_rwlock_init(lock);
    return 0;
#else
    return ethr_rwmutex_init_opt((ethr_rwmutex *) lock, NULL);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_destroy)(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
    return ethr_native_rwlock_destroy(lock);
#else
    return ethr_rwmutex_destroy((ethr_rwmutex *) lock);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_read_unlock)(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
    ethr_native_read_unlock(lock);
#else
    ethr_rwmutex_runlock((ethr_rwmutex *) lock);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_read_lock)(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
    ethr_native_read_lock(lock);
#else
    ethr_rwmutex_rlock((ethr_rwmutex *) lock);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_write_unlock)(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
    ethr_native_write_unlock(lock);
#else
    ethr_rwmutex_rwunlock((ethr_rwmutex *) lock);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_write_lock)(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
    ethr_native_write_lock(lock);
#else
    ethr_rwmutex_rwlock((ethr_rwmutex *) lock);
#endif
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* #ifndef ETHREAD_H__ */
