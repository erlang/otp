/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
#include "erl_errno.h"

/*
 * Extra memory barrier requirements:
 * - ethr_atomic_or_old() needs to enforce a memory barrier sufficient
 *   for a lock operation.
 * - ethr_atomic_and_old() needs to enforce a memory barrier sufficient
 *   for an unlock operation.
 * - ethr_atomic_cmpxchg() needs to enforce a memory barrier sufficient
 *   for a lock and unlock operation.
 */


#undef ETHR_USE_RWMTX_FALLBACK
#undef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
#undef ETHR_HAVE_OPTIMIZED_LOCKS

typedef struct {
    long tv_sec;
    long tv_nsec;
} ethr_timeval;

#if defined(DEBUG)
#  undef ETHR_XCHK
#  define  ETHR_XCHK 1
#else
#  ifndef ETHR_XCHK
#    define ETHR_XCHK 0
#  endif
#endif

#undef ETHR_INLINE
#if defined(__GNUC__)
#  define ETHR_INLINE __inline__
#elif defined(__WIN32__)
#  define ETHR_INLINE __forceinline
#endif
#if defined(DEBUG) || !defined(ETHR_INLINE) || ETHR_XCHK \
    || (defined(__GNUC__) && defined(ERTS_MIXED_CYGWIN_VC))
#  undef ETHR_INLINE
#  define ETHR_INLINE 
#  undef ETHR_TRY_INLINE_FUNCS
#endif
#ifdef ETHR_FORCE_INLINE_FUNCS
#  define ETHR_TRY_INLINE_FUNCS
#endif

#if !defined(ETHR_DISABLE_NATIVE_IMPLS) \
    && (defined(PURIFY) || defined(VALGRIND) || defined(ERTS_MIXED_CYGWIN_VC))
#  define ETHR_DISABLE_NATIVE_IMPLS
#endif

#define ETHR_RWMUTEX_INITIALIZED 	0x99999999
#define ETHR_MUTEX_INITIALIZED		0x77777777
#define ETHR_COND_INITIALIZED		0x55555555

#define ETHR_CACHE_LINE_SIZE 64

#ifdef ETHR_INLINE_FUNC_NAME_
#  define ETHR_CUSTOM_INLINE_FUNC_NAME_
#else
#  define ETHR_INLINE_FUNC_NAME_(X) X
#endif

#define ETHR_COMPILER_BARRIER ethr_compiler_barrier()
#ifdef __GNUC__
#  undef ETHR_COMPILER_BARRIER
#  define ETHR_COMPILER_BARRIER __asm__ __volatile__("":::"memory")
#endif

#ifdef DEBUG
#define ETHR_ASSERT(A) \
  ((void) ((A) ? 1 : ethr_assert_failed(__FILE__, __LINE__, #A)))
int ethr_assert_failed(char *f, int l, char *a);
#else
#define ETHR_ASSERT(A) ((void) 1)
#endif

#if defined(ETHR_PTHREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The pthread implementation                                                *
\*                                                                           */

#if defined(__linux__) && !defined(_GNU_SOURCE)
#error "_GNU_SOURCE not defined. Please, compile all files with -D_GNU_SOURCE."
#endif

#if defined(ETHR_HAVE_MIT_PTHREAD_H)
#include <pthread/mit/pthread.h>
#elif defined(ETHR_HAVE_PTHREAD_H)
#include <pthread.h>
#endif

/* Types */

typedef pthread_t ethr_tid;

typedef struct ethr_mutex_ ethr_mutex;
struct ethr_mutex_ {
    pthread_mutex_t pt_mtx;
    int is_rec_mtx;
    ethr_mutex *prev;
    ethr_mutex *next;
#if ETHR_XCHK
    int initialized;
#endif
};

typedef struct ethr_cond_ ethr_cond;
struct ethr_cond_ {
    pthread_cond_t pt_cnd;
#if ETHR_XCHK
    int initialized;
#endif
};

#ifndef ETHR_HAVE_PTHREAD_RWLOCK_INIT
#define ETHR_USE_RWMTX_FALLBACK
#else
typedef struct ethr_rwmutex_ ethr_rwmutex;
struct ethr_rwmutex_ {
    pthread_rwlock_t pt_rwlock;
#if ETHR_XCHK
    int initialized;
#endif
};
#endif

/* Static initializers */
#if ETHR_XCHK
#define ETHR_MUTEX_XCHK_INITER	, ETHR_MUTEX_INITIALIZED
#define ETHR_COND_XCHK_INITER	, ETHR_COND_INITIALIZED
#else
#define ETHR_MUTEX_XCHK_INITER
#define ETHR_COND_XCHK_INITER
#endif

#define ETHR_MUTEX_INITER {PTHREAD_MUTEX_INITIALIZER, 0, NULL, NULL ETHR_MUTEX_XCHK_INITER}
#define ETHR_COND_INITER {PTHREAD_COND_INITIALIZER ETHR_COND_XCHK_INITER}

#if defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETTYPE) \
    || defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
#  define ETHR_HAVE_ETHR_REC_MUTEX_INIT 1
#  ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#    define ETHR_REC_MUTEX_INITER \
            {PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP, 1, NULL, NULL ETHR_MUTEX_XCHK_INITER}
#  endif
#else
#  undef ETHR_HAVE_ETHR_REC_MUTEX_INIT
#endif

#ifndef ETHR_HAVE_PTHREAD_ATFORK
#  define ETHR_NO_FORKSAFETY 1
#endif

typedef pthread_key_t ethr_tsd_key;

#define ETHR_HAVE_ETHR_SIG_FUNCS 1

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_mutex_trylock)(ethr_mutex *mtx)
{
    return pthread_mutex_trylock(&mtx->pt_mtx);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_mutex_lock)(ethr_mutex *mtx)
{
    return pthread_mutex_lock(&mtx->pt_mtx);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_mutex_unlock)(ethr_mutex *mtx)
{
    return pthread_mutex_unlock(&mtx->pt_mtx);
}

#ifdef ETHR_HAVE_PTHREAD_RWLOCK_INIT

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwmutex_tryrlock)(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_tryrdlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwmutex_rlock)(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_rdlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwmutex_runlock)(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_unlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwmutex_tryrwlock)(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_trywrlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwmutex_rwlock)(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_wrlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwmutex_rwunlock)(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_unlock(&rwmtx->pt_rwlock);
}

#endif /* ETHR_HAVE_PTHREAD_RWLOCK_INIT */

#endif /* ETHR_TRY_INLINE_FUNCS */

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

/* Types */
typedef long ethr_tid; /* thread id type */
typedef struct {
    volatile int initialized;
    CRITICAL_SECTION cs;
#if ETHR_XCHK
    int is_rec_mtx;
#endif
} ethr_mutex;

typedef struct cnd_wait_event__ cnd_wait_event_;

typedef struct {
    volatile int initialized;
    CRITICAL_SECTION cs;
    cnd_wait_event_ *queue;
    cnd_wait_event_ *queue_end;
} ethr_cond;

#define ETHR_USE_RWMTX_FALLBACK

/* Static initializers */

#define ETHR_MUTEX_INITER {0}
#define ETHR_COND_INITER {0}

#define ETHR_REC_MUTEX_INITER ETHR_MUTEX_INITER

#define ETHR_HAVE_ETHR_REC_MUTEX_INIT 1

typedef DWORD ethr_tsd_key;

#undef ETHR_HAVE_ETHR_SIG_FUNCS

#ifdef ETHR_TRY_INLINE_FUNCS
int ethr_fake_static_mutex_init(ethr_mutex *mtx);

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_mutex_trylock)(ethr_mutex *mtx)
{
    if (!mtx->initialized) {
	int res = ethr_fake_static_mutex_init(mtx);
	if (res != 0)
	    return res;
    }
    return TryEnterCriticalSection(&mtx->cs) ? 0 : EBUSY;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_mutex_lock)(ethr_mutex *mtx)
{
    if (!mtx->initialized) {
	int res = ethr_fake_static_mutex_init(mtx);
	if (res != 0)
	    return res;
    }
    EnterCriticalSection(&mtx->cs);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_mutex_unlock)(ethr_mutex *mtx)
{
    LeaveCriticalSection(&mtx->cs);
    return 0;
}

#endif /* #ifdef ETHR_TRY_INLINE_FUNCS */

#ifdef ERTS_MIXED_CYGWIN_VC

/* atomics */

#ifdef _MSC_VER
#  if _MSC_VER < 1300
#    define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0 /* Dont trust really old compilers */
#  else
#    if defined(_M_IX86)
#      define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 1
#    else /* I.e. IA64 */
#      if _MSC_VER >= 1400 
#        define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 1
#      else
#        define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0
#      endif
#    endif
#  endif
#  if _MSC_VER >= 1400
#    include <intrin.h>
#    undef ETHR_COMPILER_BARRIER
#    define ETHR_COMPILER_BARRIER _ReadWriteBarrier()
#  endif
#pragma intrinsic(_ReadWriteBarrier)
#pragma intrinsic(_InterlockedAnd)
#pragma intrinsic(_InterlockedOr)
#else
#    define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0 
#endif

#define ETHR_HAVE_OPTIMIZED_ATOMIC_OPS 1
#define ETHR_HAVE_OPTIMIZED_LOCKS 1

typedef struct {
    volatile LONG value;
} ethr_atomic_t;

typedef struct {
    volatile LONG locked;
} ethr_spinlock_t;

typedef struct {
    volatile LONG counter;
} ethr_rwlock_t;
#define ETHR_WLOCK_FLAG__ (((LONG) 1) << 30)

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_init)(ethr_atomic_t *var, long i)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    var->value = (LONG) i;
#else
    (void) InterlockedExchange(&var->value, (LONG) i);
#endif
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(ethr_atomic_t *var, long i)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    var->value = (LONG) i;
#else
    (void) InterlockedExchange(&var->value, (LONG) i);
#endif
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(ethr_atomic_t *var, long *i)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    *i = var->value;
#else
    *i = InterlockedExchangeAdd(&var->value, (LONG) 0);
#endif
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_add)(ethr_atomic_t *var, long incr)
{
    (void) InterlockedExchangeAdd(&var->value, (LONG) incr);
    return 0;
}   
    
static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_addtest)(ethr_atomic_t *var,
					    long i,
					    long *testp)
{
    *testp = InterlockedExchangeAdd(&var->value, (LONG) i);
    *testp += i;
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc)(ethr_atomic_t *var)
{
    (void) InterlockedIncrement(&var->value);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(ethr_atomic_t *var)
{
    (void) InterlockedDecrement(&var->value);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inctest)(ethr_atomic_t *var, long *testp)
{
    *testp = (long) InterlockedIncrement(&var->value);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dectest)(ethr_atomic_t *var, long *testp)
{
    *testp = (long) InterlockedDecrement(&var->value);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_and_old)(ethr_atomic_t *var,
					    long mask,
					    long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     *
     * According to msdn _InterlockedAnd() provides a full
     * memory barrier.
     */
    *old = (long) _InterlockedAnd(&var->value, mask);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_or_old)(ethr_atomic_t *var,
					   long mask,
					   long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     *
     * According to msdn _InterlockedOr() provides a full
     * memory barrier.
     */
    *old = (long) _InterlockedOr(&var->value, mask);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(ethr_atomic_t *var,
					    long new,
                                            long expected,
					    long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     *
     * According to msdn _InterlockedCompareExchange() provides a full
     * memory barrier.
     */
    *old = _InterlockedCompareExchange(&var->value, (LONG) new, (LONG) expected);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_xchg)(ethr_atomic_t *var,
					 long new,
					 long *old)
{
    *old = (long) InterlockedExchange(&var->value, (LONG) new);
    return 0;
}

/*
 * According to msdn InterlockedExchange() provides a full
 * memory barrier. 
 */

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_init)(ethr_spinlock_t *lock)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    lock->locked = (LONG) 0;
#else
    (void) InterlockedExchange(&lock->locked, (LONG) 0);
#endif
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_destroy)(ethr_spinlock_t *lock)
{
    return 0;
}


static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spin_unlock)(ethr_spinlock_t *lock)
{
    ETHR_COMPILER_BARRIER;
    {
#ifdef DEBUG
	LONG old =
#endif
	    InterlockedExchange(&lock->locked, (LONG) 0);
#ifdef DEBUG
	ETHR_ASSERT(old == 1);
#endif
    }
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spin_lock)(ethr_spinlock_t *lock)
{
    LONG old;
    do {
	old = InterlockedExchange(&lock->locked, (LONG) 1);
    } while (old != (LONG) 0);
    ETHR_COMPILER_BARRIER;
    return 0;
}

/*
 * According to msdn InterlockedIncrement, InterlockedDecrement,
 * and InterlockedExchangeAdd(), _InterlockedAnd, and _InterlockedOr
 * provides full memory barriers.
 */
static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_init)(ethr_rwlock_t *lock)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    lock->counter = (LONG) 0;
#else
    (void) InterlockedExchange(&lock->counter, (LONG) 0);
#endif
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_destroy)(ethr_rwlock_t *lock)
{
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_read_unlock)(ethr_rwlock_t *lock)
{
    ETHR_COMPILER_BARRIER;
    {
#ifdef DEBUG
	LONG old =
#endif
	    InterlockedDecrement(&lock->counter);
	ETHR_ASSERT(old != 0);
    }
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_read_lock)(ethr_rwlock_t *lock)
{
    while (1) {
	LONG old = InterlockedIncrement(&lock->counter);
	if ((old & ETHR_WLOCK_FLAG__) == 0)
	    break; /* Got read lock */
	/* Restore and wait for writers to unlock */
	old = InterlockedDecrement(&lock->counter);
	while (old & ETHR_WLOCK_FLAG__) {
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
	    old = lock->counter;
#else
	    old = InterlockedExchangeAdd(&lock->counter, (LONG) 0);
#endif
	}
    }
    ETHR_COMPILER_BARRIER;
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_write_unlock)(ethr_rwlock_t *lock)
{
    ETHR_COMPILER_BARRIER;
    {
#ifdef DEBUG
	LONG old =
#endif
	    _InterlockedAnd(&lock->counter, ~ETHR_WLOCK_FLAG__);
	ETHR_ASSERT(old & ETHR_WLOCK_FLAG__);
    }
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_write_lock)(ethr_rwlock_t *lock)
{
    LONG old;
    do {
	old = _InterlockedOr(&lock->counter, ETHR_WLOCK_FLAG__);
    } while (old & ETHR_WLOCK_FLAG__);
    /* We got the write part of the lock; wait for readers to unlock */
    while ((old & ~ETHR_WLOCK_FLAG__) != 0) {
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
	old = lock->counter;
#else
	old = InterlockedExchangeAdd(&lock->counter, (LONG) 0);
#endif
	ETHR_ASSERT(old & ETHR_WLOCK_FLAG__);
    }
    ETHR_COMPILER_BARRIER;
    return 0;
}

#endif /* #ifdef ETHR_TRY_INLINE_FUNCS */

#endif /* #ifdef ERTS_MIXED_CYGWIN_VC */

#else /* No supported thread lib found */

#ifdef ETHR_NO_SUPP_THR_LIB_NOT_FATAL
#define ETHR_NO_THREAD_LIB
#else
#error "No supported thread lib found"
#endif

#endif

/* __builtin_expect() is needed by both native atomics code 
 * and the fallback code */
#if !defined(__GNUC__) || (__GNUC__ < 2) || (__GNUC__ == 2 && __GNUC_MINOR__ < 96)
#define __builtin_expect(X, Y) (X)
#endif

/* For CPU-optimised atomics, spinlocks, and rwlocks. */
#if !defined(ETHR_DISABLE_NATIVE_IMPLS) && defined(__GNUC__)
#  if ETHR_SIZEOF_PTR == 4
#    if defined(__i386__)
#      include "i386/ethread.h"
#    elif (defined(__powerpc__) || defined(__ppc__)) && !defined(__powerpc64__)
#      include "ppc32/ethread.h"
#    elif defined(__sparc__)
#      include "sparc32/ethread.h"
#    elif defined(__tile__)
#      include "tile/ethread.h"
#    endif
#  elif ETHR_SIZEOF_PTR == 8
#    if defined(__x86_64__)
#      include "x86_64/ethread.h"
#    elif defined(__sparc__) && defined(__arch64__)
#      include "sparc64/ethread.h"
#    endif
#  endif
#endif /* !defined(ETHR_DISABLE_NATIVE_IMPLS) && defined(__GNUC__) */

#ifdef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
#  undef ETHR_HAVE_NATIVE_ATOMICS
#endif
#ifdef ETHR_HAVE_OPTIMIZED_LOCKS
#  undef ETHR_HAVE_NATIVE_LOCKS
#endif

#ifdef ETHR_HAVE_NATIVE_ATOMICS
#define ETHR_HAVE_OPTIMIZED_ATOMIC_OPS 1
#endif
#ifdef ETHR_HAVE_NATIVE_LOCKS
#define ETHR_HAVE_OPTIMIZED_LOCKS 1
#endif

typedef struct {
    unsigned open;
    ethr_mutex mtx;
    ethr_cond cnd;
} ethr_gate;

#ifdef ETHR_HAVE_NATIVE_ATOMICS
/*
 * Map ethread native atomics to ethread API atomics.
 */
typedef ethr_native_atomic_t ethr_atomic_t;
#endif

#ifdef ETHR_HAVE_NATIVE_LOCKS
/*
 * Map ethread native spinlocks to ethread API spinlocks.
 */
typedef ethr_native_spinlock_t ethr_spinlock_t;
/*
 * Map ethread native rwlocks to ethread API rwlocks.
 */
typedef ethr_native_rwlock_t ethr_rwlock_t;
#endif

#ifdef ETHR_USE_RWMTX_FALLBACK
typedef struct {
    ethr_mutex mtx;
    ethr_cond rcnd;
    ethr_cond wcnd;
    unsigned readers;
    unsigned waiting_readers;
    unsigned waiting_writers;
#if ETHR_XCHK
    int initialized;
#endif
} ethr_rwmutex;
#endif

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
typedef long ethr_atomic_t;
#endif

#ifndef ETHR_HAVE_OPTIMIZED_LOCKS

#if defined(ETHR_WIN32_THREADS)
typedef struct {
    CRITICAL_SECTION cs;
} ethr_spinlock_t;
typedef struct {
    CRITICAL_SECTION cs;
    unsigned counter;
} ethr_rwlock_t;

int ethr_do_spinlock_init(ethr_spinlock_t *lock);
int ethr_do_rwlock_init(ethr_rwlock_t *lock);

#define ETHR_RWLOCK_WRITERS (((unsigned) 1) << 31)

#elif defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
typedef struct {
    pthread_spinlock_t spnlck;
} ethr_spinlock_t;
typedef struct {
    pthread_spinlock_t spnlck;
    unsigned counter;
} ethr_rwlock_t;
#define ETHR_RWLOCK_WRITERS (((unsigned) 1) << 31)

#else /* ethr mutex/rwmutex */

typedef struct {
    ethr_mutex mtx;
} ethr_spinlock_t;

typedef struct {
    ethr_rwmutex rwmtx;
} ethr_rwlock_t;

#endif /* end mutex/rwmutex */
#endif /* ETHR_HAVE_OPTIMIZED_LOCKS */

typedef struct {
    void *(*alloc)(size_t);
    void *(*realloc)(void *, size_t);
    void (*free)(void *);
    void *(*thread_create_prepare_func)(void);
    void (*thread_create_parent_func)(void *);
    void (*thread_create_child_func)(void *);
} ethr_init_data;

#define ETHR_INIT_DATA_DEFAULT_INITER {malloc, realloc, free, NULL, NULL, NULL}

typedef struct {
    int detached;			/* boolean (default false) */
    int suggested_stack_size;		/* kilo words (default sys dependent) */
} ethr_thr_opts;

#define ETHR_THR_OPTS_DEFAULT_INITER {0, -1}

#if defined(ETHR_CUSTOM_INLINE_FUNC_NAME_) || !defined(ETHR_TRY_INLINE_FUNCS)
#  define ETHR_NEED_MTX_PROTOTYPES__
#  define ETHR_NEED_RWMTX_PROTOTYPES__
#  define ETHR_NEED_SPINLOCK_PROTOTYPES__
#  define ETHR_NEED_ATOMIC_PROTOTYPES__
#endif

#if !defined(ETHR_NEED_RWMTX_PROTOTYPES__) && defined(ETHR_USE_RWMTX_FALLBACK)
#  define ETHR_NEED_RWMTX_PROTOTYPES__
#endif

int ethr_init(ethr_init_data *);
int ethr_install_exit_handler(void (*funcp)(void));
int ethr_thr_create(ethr_tid *, void * (*)(void *), void *, ethr_thr_opts *);
int ethr_thr_join(ethr_tid, void **);
int ethr_thr_detach(ethr_tid);
void ethr_thr_exit(void *);
ethr_tid ethr_self(void);
int ethr_equal_tids(ethr_tid, ethr_tid);
int ethr_mutex_init(ethr_mutex *);
#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
int ethr_rec_mutex_init(ethr_mutex *);
#endif
int ethr_mutex_destroy(ethr_mutex *);
int ethr_mutex_set_forksafe(ethr_mutex *);
int ethr_mutex_unset_forksafe(ethr_mutex *);
#ifdef ETHR_NEED_MTX_PROTOTYPES__
int ethr_mutex_trylock(ethr_mutex *);
int ethr_mutex_lock(ethr_mutex *);
int ethr_mutex_unlock(ethr_mutex *);
#endif
int ethr_cond_init(ethr_cond *);
int ethr_cond_destroy(ethr_cond *);
int ethr_cond_signal(ethr_cond *);
int ethr_cond_broadcast(ethr_cond *);
int ethr_cond_wait(ethr_cond *, ethr_mutex *);
int ethr_cond_timedwait(ethr_cond *, ethr_mutex *, ethr_timeval *);

int ethr_rwmutex_init(ethr_rwmutex *);
int ethr_rwmutex_destroy(ethr_rwmutex *);
#ifdef ETHR_NEED_RWMTX_PROTOTYPES__
int ethr_rwmutex_tryrlock(ethr_rwmutex *);
int ethr_rwmutex_rlock(ethr_rwmutex *);
int ethr_rwmutex_runlock(ethr_rwmutex *);
int ethr_rwmutex_tryrwlock(ethr_rwmutex *);
int ethr_rwmutex_rwlock(ethr_rwmutex *);
int ethr_rwmutex_rwunlock(ethr_rwmutex *);
#endif

#ifdef ETHR_NEED_ATOMIC_PROTOTYPES__
int ethr_atomic_init(ethr_atomic_t *, long);
int ethr_atomic_set(ethr_atomic_t *, long);
int ethr_atomic_read(ethr_atomic_t *, long *);
int ethr_atomic_inctest(ethr_atomic_t *, long *);
int ethr_atomic_dectest(ethr_atomic_t *, long *);
int ethr_atomic_inc(ethr_atomic_t *);
int ethr_atomic_dec(ethr_atomic_t *);
int ethr_atomic_addtest(ethr_atomic_t *, long, long *);
int ethr_atomic_add(ethr_atomic_t *, long);
int ethr_atomic_and_old(ethr_atomic_t *, long, long *);
int ethr_atomic_or_old(ethr_atomic_t *, long, long *);
int ethr_atomic_xchg(ethr_atomic_t *, long, long *);
int ethr_atomic_cmpxchg(ethr_atomic_t *, long, long, long *);
#endif

#ifdef ETHR_NEED_SPINLOCK_PROTOTYPES__
int ethr_spinlock_init(ethr_spinlock_t *);
int ethr_spinlock_destroy(ethr_spinlock_t *);
int ethr_spin_unlock(ethr_spinlock_t *);
int ethr_spin_lock(ethr_spinlock_t *);

int ethr_rwlock_init(ethr_rwlock_t *);
int ethr_rwlock_destroy(ethr_rwlock_t *);
int ethr_read_unlock(ethr_rwlock_t *);
int ethr_read_lock(ethr_rwlock_t *);
int ethr_write_unlock(ethr_rwlock_t *);
int ethr_write_lock(ethr_rwlock_t *);
#endif

int ethr_time_now(ethr_timeval *);
int ethr_tsd_key_create(ethr_tsd_key *);
int ethr_tsd_key_delete(ethr_tsd_key);
int ethr_tsd_set(ethr_tsd_key, void *);
void *ethr_tsd_get(ethr_tsd_key);

int ethr_gate_init(ethr_gate *);
int ethr_gate_destroy(ethr_gate *);
int ethr_gate_close(ethr_gate *);
int ethr_gate_let_through(ethr_gate *, unsigned);
int ethr_gate_wait(ethr_gate *);
int ethr_gate_swait(ethr_gate *, int);

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#include <signal.h>
int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset);
int ethr_sigwait(const sigset_t *set, int *sig);
#endif

void ethr_compiler_barrier(void);

#ifdef ETHR_TRY_INLINE_FUNCS

#ifdef ETHR_HAVE_NATIVE_ATOMICS

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_init)(ethr_atomic_t *var, long i)
{
    ethr_native_atomic_init(var, i);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(ethr_atomic_t *var, long i)
{
    ethr_native_atomic_set(var, i);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(ethr_atomic_t *var, long *i)
{
    *i = ethr_native_atomic_read(var);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_add)(ethr_atomic_t *var, long incr)
{
    ethr_native_atomic_add(var, incr);
    return 0;
}   
    
static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_addtest)(ethr_atomic_t *var,
					    long i,
					    long *testp)
{
    *testp = ethr_native_atomic_add_return(var, i);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc)(ethr_atomic_t *var)
{
    ethr_native_atomic_inc(var);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(ethr_atomic_t *var)
{
    ethr_native_atomic_dec(var);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inctest)(ethr_atomic_t *var, long *testp)
{
    *testp = ethr_native_atomic_inc_return(var);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dectest)(ethr_atomic_t *var, long *testp)
{
    *testp = ethr_native_atomic_dec_return(var);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_and_old)(ethr_atomic_t *var,
					    long mask,
					    long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     */
    *old = ethr_native_atomic_and_retold(var, mask);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_or_old)(ethr_atomic_t *var,
					   long mask,
					   long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     */
    *old = ethr_native_atomic_or_retold(var, mask);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_xchg)(ethr_atomic_t *var,
					 long new,
					 long *old)
{
    *old = ethr_native_atomic_xchg(var, new);
    return 0;
}   

/*
 * If *var == *old, replace *old with new, else do nothing.
 * In any case return the original value of *var in *old.
 */
static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(ethr_atomic_t *var,
					    long new,
                                            long expected,
					    long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     */
    *old = ethr_native_atomic_cmpxchg(var, new, expected);
    return 0;
}

#endif /* ETHR_HAVE_NATIVE_ATOMICS */

#ifdef ETHR_HAVE_NATIVE_LOCKS

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_init)(ethr_spinlock_t *lock)
{
    ethr_native_spinlock_init(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_destroy)(ethr_spinlock_t *lock)
{
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spin_unlock)(ethr_spinlock_t *lock)
{
    ethr_native_spin_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spin_lock)(ethr_spinlock_t *lock)
{
    ethr_native_spin_lock(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_init)(ethr_rwlock_t *lock)
{
    ethr_native_rwlock_init(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_destroy)(ethr_rwlock_t *lock)
{
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_read_unlock)(ethr_rwlock_t *lock)
{
    ethr_native_read_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_read_lock)(ethr_rwlock_t *lock)
{
    ethr_native_read_lock(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_write_unlock)(ethr_rwlock_t *lock)
{
    ethr_native_write_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_write_lock)(ethr_rwlock_t *lock)
{
    ethr_native_write_lock(lock);
    return 0;
}

#endif /* ETHR_HAVE_NATIVE_LOCKS */

#endif /* ETHR_TRY_INLINE_FUNCS */

/*
 * Fallbacks for atomics used in absence of optimized implementation.
 */
#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS

#define ETHR_ATOMIC_ADDR_BITS 4
#define ETHR_ATOMIC_ADDR_SHIFT 3

typedef struct {
    union {
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
	pthread_spinlock_t spnlck;
#else
	ethr_mutex mtx;
#endif
	char buf[ETHR_CACHE_LINE_SIZE];
    } u;
} ethr_atomic_protection_t;

extern ethr_atomic_protection_t ethr_atomic_protection__[1 << ETHR_ATOMIC_ADDR_BITS];


#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)

#define ETHR_ATOMIC_PTR2LCK__(PTR) \
(&ethr_atomic_protection__[((((unsigned long) (PTR)) >> ETHR_ATOMIC_ADDR_SHIFT) \
			& ((1 << ETHR_ATOMIC_ADDR_BITS) - 1))].u.spnlck)


#define ETHR_ATOMIC_OP_FALLBACK_IMPL__(AP, EXPS)			\
do {									\
    pthread_spinlock_t *slp__ = ETHR_ATOMIC_PTR2LCK__((AP));		\
    int res__ = pthread_spin_lock(slp__);				\
    if (res__ != 0)							\
	return res__;							\
    { EXPS; }								\
    return pthread_spin_unlock(slp__);					\
} while (0)

#else /* ethread mutex */

#define ETHR_ATOMIC_PTR2LCK__(PTR) \
(&ethr_atomic_protection__[((((unsigned long) (PTR)) >> ETHR_ATOMIC_ADDR_SHIFT) \
		     & ((1 << ETHR_ATOMIC_ADDR_BITS) - 1))].u.mtx)

#define ETHR_ATOMIC_OP_FALLBACK_IMPL__(AP, EXPS)			\
do {									\
    ethr_mutex *mtxp__ = ETHR_ATOMIC_PTR2LCK__((AP));			\
    int res__ = ETHR_INLINE_FUNC_NAME_(ethr_mutex_lock)(mtxp__);	\
    if (res__ != 0)							\
	return res__;							\
    { EXPS; }								\
    return ETHR_INLINE_FUNC_NAME_(ethr_mutex_unlock)(mtxp__);		\
} while (0)

#endif /* end ethread mutex */

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_init)(ethr_atomic_t *var, long i)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = (ethr_atomic_t) i);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(ethr_atomic_t *var, long i)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = (ethr_atomic_t) i);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(ethr_atomic_t *var, long *i)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *i = (long) *var);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inctest)(ethr_atomic_t *incp, long *testp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(incp, *testp = (long) ++(*incp));
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dectest)(ethr_atomic_t *decp, long *testp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(decp, *testp = (long) --(*decp));
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_add)(ethr_atomic_t *var, long incr)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += incr);
}   
    
static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_addtest)(ethr_atomic_t *incp,
					    long i,
					    long *testp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(incp, *incp += i; *testp = *incp);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc)(ethr_atomic_t *incp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(incp, ++(*incp));
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(ethr_atomic_t *decp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(decp, --(*decp));
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_and_old)(ethr_atomic_t *var,
					    long mask,
					    long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     */
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *old = *var; *var &= mask);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_or_old)(ethr_atomic_t *var,
					   long mask,
					   long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     */
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *old = *var; *var |= mask);
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_xchg)(ethr_atomic_t *var,
					 long new,
					 long *old)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *old = *var; *var = new);
}   

/*
 * If *var == *old, replace *old with new, else do nothing.
 * In any case return the original value of *var in *old.
 */
static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(ethr_atomic_t *var,
					    long new,
                                            long expected,
					    long *old)
{
    /*
     * See "Extra memory barrier requirements" note at the top
     * of the file.
     */
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(
      var,
      long old_val = *var;
      *old = old_val;
      if (__builtin_expect(old_val == expected, 1))
          *var = new;
      );
    return 0;
}

#endif /* #ifdef ETHR_TRY_INLINE_FUNCS */
#endif /* #ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS */

/*
 * Fallbacks for spin locks, and rw spin locks used in absence of
 * optimized implementation.
 */
#ifndef ETHR_HAVE_OPTIMIZED_LOCKS

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_init)(ethr_spinlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    return pthread_spin_init(&lock->spnlck, 0);
#else
    return ethr_mutex_init(&lock->mtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spinlock_destroy)(ethr_spinlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    return pthread_spin_destroy(&lock->spnlck);
#else
    return ethr_mutex_destroy(&lock->mtx);
#endif
}


static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spin_unlock)(ethr_spinlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    return pthread_spin_unlock(&lock->spnlck);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_mutex_unlock)(&lock->mtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_spin_lock)(ethr_spinlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    return pthread_spin_lock(&lock->spnlck);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_mutex_lock)(&lock->mtx);
#endif
}

#ifdef ETHR_USE_RWMTX_FALLBACK
#define ETHR_RWLOCK_RWMTX_FALLBACK_NAME_(X) X
#else
#define ETHR_RWLOCK_RWMTX_FALLBACK_NAME_(X) ETHR_INLINE_FUNC_NAME_(X)
#endif

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_init)(ethr_rwlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    lock->counter = 0;
    return pthread_spin_init(&lock->spnlck, 0);
#else
    return ethr_rwmutex_init(&lock->rwmtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_rwlock_destroy)(ethr_rwlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    return pthread_spin_destroy(&lock->spnlck);
#else
    return ethr_rwmutex_destroy(&lock->rwmtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_read_unlock)(ethr_rwlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    int res = pthread_spin_lock(&lock->spnlck);
    if (res != 0)
	return res;
    lock->counter--;
    return pthread_spin_unlock(&lock->spnlck);
#else
    return ETHR_RWLOCK_RWMTX_FALLBACK_NAME_(ethr_rwmutex_runlock)(&lock->rwmtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_read_lock)(ethr_rwlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    int locked = 0;
    do {
	int res = pthread_spin_lock(&lock->spnlck);
	if (res != 0)
	    return res;
	if ((lock->counter & ETHR_RWLOCK_WRITERS) == 0) {
	    lock->counter++;
	    locked = 1;
	}
	res = pthread_spin_unlock(&lock->spnlck);
	if (res != 0)
	    return res;
    } while (!locked);
    return 0;
#else
    return ETHR_RWLOCK_RWMTX_FALLBACK_NAME_(ethr_rwmutex_rlock)(&lock->rwmtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_write_unlock)(ethr_rwlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    lock->counter = 0;
    return pthread_spin_unlock(&lock->spnlck);
#else
    return ETHR_RWLOCK_RWMTX_FALLBACK_NAME_(ethr_rwmutex_rwunlock)(&lock->rwmtx);
#endif
}

static ETHR_INLINE int
ETHR_INLINE_FUNC_NAME_(ethr_write_lock)(ethr_rwlock_t *lock)
{
#if defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
    while (1) {
	int res = pthread_spin_lock(&lock->spnlck);
	if (res != 0)
	    return res;
	lock->counter |= ETHR_RWLOCK_WRITERS;
	if (lock->counter == ETHR_RWLOCK_WRITERS)
	    return 0;
	res = pthread_spin_unlock(&lock->spnlck);
	if (res != 0)
	    return res;
    }
#else
    return ETHR_RWLOCK_RWMTX_FALLBACK_NAME_(ethr_rwmutex_rwlock)(&lock->rwmtx);
#endif
}

#endif /* #ifdef ETHR_TRY_INLINE_FUNCS */

#endif /* ETHR_HAVE_OPTIMIZED_LOCKS */

#if defined(ETHR_HAVE_OPTIMIZED_LOCKS) || defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
# define ETHR_HAVE_OPTIMIZED_SPINLOCK
#endif

#endif /* #ifndef ETHREAD_H__ */
