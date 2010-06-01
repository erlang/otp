/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
 * Description: A Thread library for use in the ERTS and other OTP
 *              applications.
 * Author: Rickard Green
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#undef ETHR_STACK_GUARD_SIZE 

#if defined(ETHR_PTHREADS)

#ifdef ETHR_TIME_WITH_SYS_TIME
#  include <time.h>
#  include <sys/time.h>
#else
#  ifdef ETHR_HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#ifdef ETHR_HAVE_PTHREAD_ATTR_SETGUARDSIZE
#  define ETHR_STACK_GUARD_SIZE (pagesize)
#endif

#elif defined(ETHR_WIN32_THREADS)

#undef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <winerror.h>

#else
#error "Missing thread implementation"
#endif

#include <limits.h>

#define ETHR_FORCE_INLINE_FUNCS
#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#include "ethread.h"

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#error Missing configure defines
#endif

/*
 * ----------------------------------------------------------------------------
 * Common stuff
 * ----------------------------------------------------------------------------
 */

#define ETHR_MAX_THREADS 2048 /* Has to be an even power of 2 */

static int ethr_not_inited = 1;

#define ASSERT(A) ETHR_ASSERT((A))

static void *(*allocp)(size_t) = malloc;
static void *(*reallocp)(void *, size_t) = realloc;
static void (*freep)(void *) = free;

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
ethr_atomic_protection_t ethr_atomic_protection__[1 << ETHR_ATOMIC_ADDR_BITS];
#endif

void *(*thread_create_prepare_func)(void) = NULL;
void (*thread_create_parent_func)(void *) = NULL;
void (*thread_create_child_func)(void *) = NULL;

typedef struct ethr_xhndl_list_ ethr_xhndl_list;
struct ethr_xhndl_list_ {
    ethr_xhndl_list *next;
    void (*funcp)(void);
};

static size_t pagesize;
#define ETHR_PAGE_ALIGN(SZ) (((((size_t) (SZ)) - 1)/pagesize + 1)*pagesize)
static size_t min_stack_size; /* kilo words */
static size_t max_stack_size; /* kilo words */
#define ETHR_B2KW(B) ((((size_t) (B)) - 1)/(sizeof(void *)*1024) + 1)
#define ETHR_KW2B(KW) (((size_t) (KW))*sizeof(void *)*1024)

ethr_mutex xhndl_mtx;
ethr_xhndl_list *xhndl_list;

static int
init_common(ethr_init_data *id)
{
    int res;
    if (id) {
	allocp				= id->alloc;
	reallocp			= id->realloc;
	freep				= id->free;
	thread_create_prepare_func	= id->thread_create_prepare_func;
	thread_create_parent_func	= id->thread_create_parent_func;
	thread_create_child_func	= id->thread_create_child_func;
    }
    if (!allocp || !reallocp || !freep)
	return EINVAL;

#ifdef _SC_PAGESIZE
    pagesize = (size_t) sysconf(_SC_PAGESIZE);
#elif defined(HAVE_GETPAGESIZE)
    pagesize = (size_t) getpagesize();
#else
    pagesize = (size_t) 4*1024; /* Guess 4 KB */
#endif

    /* User needs at least 4 KB */
    min_stack_size = 4*1024;
#if SIZEOF_VOID_P == 8
    /* Double that on 64-bit archs */
    min_stack_size *= 2;
#endif
    /* On some systems as much as about 4 KB is used by the system */
    min_stack_size += 4*1024;
    /* There should be room for signal handlers */
#ifdef SIGSTKSZ
    min_stack_size += SIGSTKSZ;
#else
    min_stack_size += pagesize;
#endif
    /* The system may think that we need more stack */
#if defined(PTHREAD_STACK_MIN)
    if (min_stack_size < PTHREAD_STACK_MIN)
	min_stack_size = PTHREAD_STACK_MIN;
#elif defined(_SC_THREAD_STACK_MIN)
    {
	size_t thr_min_stk_sz = (size_t) sysconf(_SC_THREAD_STACK_MIN);
	if (min_stack_size < thr_min_stk_sz)
	    min_stack_size = thr_min_stk_sz;
    }
#endif
    /* The guard is at least on some platforms included in the stack size
       passed when creating threads */
#ifdef ETHR_STACK_GUARD_SIZE
    min_stack_size += ETHR_STACK_GUARD_SIZE;
#endif
    min_stack_size = ETHR_PAGE_ALIGN(min_stack_size);

    min_stack_size = ETHR_B2KW(min_stack_size);

    max_stack_size = 32*1024*1024;
#if SIZEOF_VOID_P == 8
    max_stack_size *= 2;
#endif
    max_stack_size = ETHR_B2KW(max_stack_size);

    xhndl_list = NULL;

    res = ethr_mutex_init(&xhndl_mtx);
    if (res != 0)
	return res;

    res = ethr_mutex_set_forksafe(&xhndl_mtx);
    if (res != 0 && res != ENOTSUP)
	return res;

    return 0;
}

int
ethr_install_exit_handler(void (*funcp)(void))
{
    ethr_xhndl_list *xhp;
    int res;

#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif

    if (!funcp)
	return EINVAL;

    xhp = (ethr_xhndl_list *) (*allocp)(sizeof(ethr_xhndl_list));
    if (!xhp)
	return ENOMEM;

    res = ethr_mutex_lock__(&xhndl_mtx);
    if (res != 0) {
	(*freep)((void *) xhp);
	return res;
    }

    xhp->funcp = funcp;
    xhp->next = xhndl_list;
    xhndl_list = xhp;

    res = ethr_mutex_unlock__(&xhndl_mtx);
    if (res != 0)
	abort();

    return res;
}

static void
run_exit_handlers(void)
{
    int res;
    ethr_xhndl_list *xhp;

    res = ethr_mutex_lock__(&xhndl_mtx);
    if (res != 0)
	abort();

    xhp = xhndl_list;

    res = ethr_mutex_unlock__(&xhndl_mtx);
    if (res != 0)
	abort();

    for (; xhp; xhp = xhp->next)
	(*xhp->funcp)();
}

#if defined(ETHR_PTHREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * pthread implementation                                                    *
\*                                                                           */

typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t cnd;
    int initialized;
    void *(*thr_func)(void *);
    void *arg;
    void *prep_func_res;
} thr_wrap_data_;

static int no_ethreads;
static ethr_mutex no_ethrs_mtx;

#ifndef ETHR_HAVE_PTHREAD_ATFORK
#define ETHR_HAVE_PTHREAD_ATFORK 0
#endif

#if !ETHR_HAVE_PTHREAD_ATFORK
#warning "Cannot enforce fork-safety"
#endif

#ifdef ETHR_HAVE_PTHREAD_RWLOCK_INIT
static pthread_rwlockattr_t write_pref_attr_data;
static pthread_rwlockattr_t *write_pref_attr;
#endif

/*
 * ----------------------------------------------------------------------------
 * Static functions
 * ----------------------------------------------------------------------------
 */

/*
 * Functions with safe_ prefix aborts on failure. To be used when
 * we cannot recover after failure.
 */

static ETHR_INLINE void
safe_mutex_lock(pthread_mutex_t *mtxp)
{
    int res = pthread_mutex_lock(mtxp);
    if (res != 0)
	abort();
}

static ETHR_INLINE void
safe_mutex_unlock(pthread_mutex_t *mtxp)
{
    int res = pthread_mutex_unlock(mtxp);
    if (res != 0)
	abort();
}

static ETHR_INLINE void
safe_cond_signal(pthread_cond_t *cndp)
{
    int res = pthread_cond_signal(cndp);
    if (res != 0)
	abort();
}

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT

static volatile int rec_mtx_attr_need_init = 1;
static pthread_mutexattr_t rec_mtx_attr;

static int init_rec_mtx_attr(void);

#endif

#if ETHR_HAVE_PTHREAD_ATFORK

static ethr_mutex forksafe_mtx = ETHR_MUTEX_INITER;

static void lock_mutexes(void)
{
    ethr_mutex *m = &forksafe_mtx;
    do {

	safe_mutex_lock(&m->pt_mtx);

	m = m->next;

    } while (m != &forksafe_mtx);
}

static void unlock_mutexes(void)
{
    ethr_mutex *m = forksafe_mtx.prev;
    do {

	safe_mutex_unlock(&m->pt_mtx);

	m = m->prev;

    } while (m->next != &forksafe_mtx);
}

#if ETHR_INIT_MUTEX_IN_CHILD_AT_FORK

static void reinit_mutexes(void)
{
    ethr_mutex *m = forksafe_mtx.prev;
    do {
	pthread_mutexattr_t *attrp = NULL;

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
	if (m->is_rec_mtx) {
	    if (rec_mtx_attr_need_init) {
		int res = init_rec_mtx_attr();
		if (res != 0)
		    abort();
	    }
	    attrp = &rec_mtx_attr;
	}
#endif
	if (pthread_mutex_init(&m->pt_mtx, attrp) != 0)
	    abort();

	m = m->prev;

    } while (m->next != &forksafe_mtx);
}

#endif

static int
init_forksafe(void)
{
    static int init_done = 0;
    int res = 0;

    if (init_done)
	return res;

    forksafe_mtx.prev = &forksafe_mtx;
    forksafe_mtx.next = &forksafe_mtx;

    res = pthread_atfork(lock_mutexes,
			 unlock_mutexes,
#if ETHR_INIT_MUTEX_IN_CHILD_AT_FORK
			 reinit_mutexes
#else
			 unlock_mutexes
#endif
	);

    init_done = 1;
    return res;
}

#endif


#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT

#if defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETTYPE)

#define SET_REC_MUTEX_ATTR(AP) \
  pthread_mutexattr_settype((AP), PTHREAD_MUTEX_RECURSIVE);

#elif defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)

#define SET_REC_MUTEX_ATTR(AP) \
  pthread_mutexattr_setkind_np((AP), PTHREAD_MUTEX_RECURSIVE_NP);

#else

#error "Don't know how to set recursive mutex attributes"

#endif

static int
init_rec_mtx_attr(void)
{
    int res, mres;
    static pthread_mutex_t attrinit_mtx = PTHREAD_MUTEX_INITIALIZER;

    mres = pthread_mutex_lock(&attrinit_mtx);
    if (mres != 0)
	return mres;
    /* Got here under race conditions; check again ... */
    if (!rec_mtx_attr_need_init)
	res = 0;
    else {
	res = pthread_mutexattr_init(&rec_mtx_attr);
	if (res == 0) {
	    res = SET_REC_MUTEX_ATTR(&rec_mtx_attr);
	    if (res == 0)
		rec_mtx_attr_need_init = 0;
	    else
		(void) pthread_mutexattr_destroy(&rec_mtx_attr);
	}
    }

    mres = pthread_mutex_unlock(&attrinit_mtx);
    if (mres != 0)
	return mres;
    return res;
}

#endif /* #if ETHR_HAVE_ETHR_REC_MUTEX_INIT */

static ETHR_INLINE void thr_exit_cleanup(void)
{
    run_exit_handlers();
    safe_mutex_lock(&no_ethrs_mtx.pt_mtx);
    ASSERT(no_ethreads > 0);
    no_ethreads--;
    safe_mutex_unlock(&no_ethrs_mtx.pt_mtx);
}

static void *thr_wrapper(void *vtwd)
{
    void *res;
    thr_wrap_data_ *twd = (thr_wrap_data_ *) vtwd;
    void *(*thr_func)(void *) = twd->thr_func;
    void *arg = twd->arg;

    safe_mutex_lock(&twd->mtx);

    if (thread_create_child_func)
	(*thread_create_child_func)(twd->prep_func_res);

    twd->initialized = 1;

    safe_cond_signal(&twd->cnd);
    safe_mutex_unlock(&twd->mtx);

    res = (*thr_func)(arg);
    thr_exit_cleanup();
    return res;
}


/*
 * ----------------------------------------------------------------------------
 * Exported functions
 * ----------------------------------------------------------------------------
 */

int
ethr_init(ethr_init_data *id)
{
    int res;

    if (!ethr_not_inited)
	return EINVAL;

    ethr_not_inited = 0;

    res = init_common(id);
    if (res != 0)
	goto error;

#if ETHR_HAVE_PTHREAD_ATFORK
    init_forksafe();
#endif

    no_ethreads = 1;
    res = ethr_mutex_init(&no_ethrs_mtx);
    if (res != 0)
	goto error;
    res = ethr_mutex_set_forksafe(&no_ethrs_mtx);
    if (res != 0 && res != ENOTSUP)
	goto error;

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
    {
	int i;
	for (i = 0; i < (1 << ETHR_ATOMIC_ADDR_BITS); i++) {
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
	    res = pthread_spin_init(&ethr_atomic_protection__[i].u.spnlck, 0);
#else
	    res = ethr_mutex_init(&ethr_atomic_protection__[i].u.mtx);
#endif
	    if (res != 0)
		goto error;
	}
    }
#endif

#ifdef ETHR_HAVE_PTHREAD_RWLOCK_INIT
#if defined(ETHR_HAVE_PTHREAD_RWLOCKATTR_SETKIND_NP) \
    && defined(ETHR_HAVE_PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)
    res = pthread_rwlockattr_init(&write_pref_attr_data);
    if (res != 0)
	goto error;
    res = pthread_rwlockattr_setkind_np(
	&write_pref_attr_data,
	PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
    if (res != 0)
	goto error;
    write_pref_attr = &write_pref_attr_data;
#else
    write_pref_attr = NULL;
#endif
#endif


    return 0;

 error:
    ethr_not_inited = 1;
    return res;

}

int
ethr_thr_create(ethr_tid *tid, void * (*func)(void *), void *arg,
		ethr_thr_opts *opts)
{
    thr_wrap_data_ twd;
    pthread_attr_t attr;
    int res, dres;
    int use_stack_size = (opts && opts->suggested_stack_size >= 0
			  ? opts->suggested_stack_size
			  : -1 /* Use system default */);

#ifdef ETHR_MODIFIED_DEFAULT_STACK_SIZE
    if (use_stack_size < 0)
	use_stack_size = ETHR_MODIFIED_DEFAULT_STACK_SIZE;
#endif

    twd.initialized = 0;
    twd.thr_func = func;
    twd.arg = arg;

#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!tid || !func) {
	ASSERT(0);
	return EINVAL;
    }
#endif

    /* Call prepare func if it exist */
    if (thread_create_prepare_func)
	twd.prep_func_res = (*thread_create_prepare_func)();
    else
	twd.prep_func_res = NULL;

    /* Set som thread attributes */
    res = pthread_attr_init(&attr);
    if (res != 0)
	goto cleanup_parent_func;
    res = pthread_mutex_init(&twd.mtx, NULL);
    if (res != 0)
	goto cleanup_attr_destroy;
    res = pthread_cond_init(&twd.cnd, NULL);
    if (res != 0)
	goto cleanup_mutex_destroy;

    /* Schedule child thread in system scope (if possible) ... */
    res = pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
    if (res != 0 && res != ENOTSUP)
	goto cleanup_cond_destroy;

    if (use_stack_size >= 0) {
	size_t suggested_stack_size = (size_t) use_stack_size;
	size_t stack_size;
#ifdef DEBUG
	suggested_stack_size /= 2; /* Make sure we got margin */
#endif
#ifdef ETHR_STACK_GUARD_SIZE
	/* The guard is at least on some platforms included in the stack size
	   passed when creating threads */
	suggested_stack_size += ETHR_B2KW(ETHR_STACK_GUARD_SIZE);
#endif
	if (suggested_stack_size < min_stack_size)
	    stack_size = ETHR_KW2B(min_stack_size);
	else if (suggested_stack_size > max_stack_size)
	    stack_size = ETHR_KW2B(max_stack_size);
	else
	    stack_size = ETHR_PAGE_ALIGN(ETHR_KW2B(suggested_stack_size));
	(void) pthread_attr_setstacksize(&attr, stack_size);
    }

#ifdef ETHR_STACK_GUARD_SIZE
    (void) pthread_attr_setguardsize(&attr, ETHR_STACK_GUARD_SIZE);
#endif

    /* Detached or joinable... */
    res = pthread_attr_setdetachstate(&attr,
				      (opts && opts->detached
				       ? PTHREAD_CREATE_DETACHED
				       : PTHREAD_CREATE_JOINABLE));
    if (res != 0)
	goto cleanup_cond_destroy;
    
    res = pthread_mutex_lock(&twd.mtx);

    if (res != 0)
	goto cleanup_cond_destroy;

    safe_mutex_lock(&no_ethrs_mtx.pt_mtx);
    if (no_ethreads < ETHR_MAX_THREADS) {
	no_ethreads++;
	safe_mutex_unlock(&no_ethrs_mtx.pt_mtx);
    }
    else {
	res = EAGAIN;
	safe_mutex_unlock(&no_ethrs_mtx.pt_mtx);
	goto cleanup_mutex_unlock;
    }

    res = pthread_create((pthread_t *) tid, &attr, thr_wrapper, (void *) &twd);

    if (res != 0) {
	safe_mutex_lock(&no_ethrs_mtx.pt_mtx);
	ASSERT(no_ethreads > 0);
	no_ethreads--;
	safe_mutex_unlock(&no_ethrs_mtx.pt_mtx);
    }
    else {

	/* Wait for child to initialize... */
	while (!twd.initialized) {
	    res = pthread_cond_wait(&twd.cnd, &twd.mtx);
	    if (res != 0 && res != EINTR)
		break;
	}

    }

    /* Cleanup... */
 cleanup_mutex_unlock:
    dres = pthread_mutex_unlock(&twd.mtx);
    if (res == 0)
	res = dres;
 cleanup_cond_destroy:
    dres = pthread_cond_destroy(&twd.cnd);
    if (res == 0)
	res = dres;
 cleanup_mutex_destroy:
    dres = pthread_mutex_destroy(&twd.mtx);
    if (res == 0)
	res = dres;
 cleanup_attr_destroy:
    dres = pthread_attr_destroy(&attr);
    if (res == 0)
	res = dres;
 cleanup_parent_func:
    if (thread_create_parent_func)
	(*thread_create_parent_func)(twd.prep_func_res);

    return res;
}

int
ethr_thr_join(ethr_tid tid, void **res)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    return pthread_join((pthread_t) tid, res);
}

int
ethr_thr_detach(ethr_tid tid)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    return pthread_detach((pthread_t) tid);
}

void
ethr_thr_exit(void *res)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return;
    }
#endif
    thr_exit_cleanup();
    pthread_exit(res);
}

ethr_tid
ethr_self(void)
{
    return (ethr_tid) pthread_self();
}

int
ethr_equal_tids(ethr_tid tid1, ethr_tid tid2)
{
    return pthread_equal((pthread_t) tid1, (pthread_t) tid2);
}


/*
 * Mutex functions
 */


int
ethr_mutex_init(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx) {
	ASSERT(0);
	return EINVAL;
    }
    mtx->initialized = ETHR_MUTEX_INITIALIZED;
#endif
    mtx->prev = NULL;
    mtx->next = NULL;
    mtx->is_rec_mtx = 0;
    return pthread_mutex_init(&mtx->pt_mtx, NULL);
}

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT

int
ethr_rec_mutex_init(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx) {
	ASSERT(0);
	return EINVAL;
    }
    mtx->initialized = ETHR_MUTEX_INITIALIZED;
#endif
    if (rec_mtx_attr_need_init)
	init_rec_mtx_attr();

    mtx->prev = NULL;
    mtx->next = NULL;
    mtx->is_rec_mtx = 1;
    return pthread_mutex_init(&mtx->pt_mtx, &rec_mtx_attr);
}

#endif /* #if ETHR_HAVE_ETHR_REC_MUTEX_INIT */

int
ethr_mutex_destroy(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (mtx->next) {
	ASSERT(mtx->prev);
	ethr_mutex_unset_forksafe(mtx);
    }
#if ETHR_XCHK
    mtx->initialized = 0;
#endif
    return pthread_mutex_destroy(&mtx->pt_mtx);
}

int ethr_mutex_set_forksafe(ethr_mutex *mtx)
{
    int res;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
#if ETHR_HAVE_PTHREAD_ATFORK
    res = pthread_mutex_lock(&forksafe_mtx.pt_mtx);
    if (res != 0)
	return res;
    if (!forksafe_mtx.next) {
	ASSERT(!forksafe_mtx.prev);
	init_forksafe();
    }
    if (mtx->next) {
	/* forksafe already set for this mutex */ 
	ASSERT(mtx->prev);
    }
    else {
	mtx->next = forksafe_mtx.next;
	mtx->prev = &forksafe_mtx;
	forksafe_mtx.next->prev = mtx;
	forksafe_mtx.next = mtx;
    }

    res = pthread_mutex_unlock(&forksafe_mtx.pt_mtx);

#else /* #if ETHR_HAVE_PTHREAD_ATFORK */
    res = ENOTSUP;
#endif /* #if ETHR_HAVE_PTHREAD_ATFORK */
    return res;
}

int ethr_mutex_unset_forksafe(ethr_mutex *mtx)
{
    int res;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
#if ETHR_HAVE_PTHREAD_ATFORK
    res = pthread_mutex_lock(&forksafe_mtx.pt_mtx);
    if (res != 0)
	return res;
    if (!forksafe_mtx.next) {
	ASSERT(!forksafe_mtx.prev);
	init_forksafe();
    }
    if (!mtx->next) {
	/* forksafe already unset for this mutex */ 
	ASSERT(!mtx->prev);
    }
    else {
	mtx->prev->next = mtx->next;
	mtx->next->prev = mtx->prev;
	mtx->next = NULL;
	mtx->prev = NULL;
    }
    res = pthread_mutex_unlock(&forksafe_mtx.pt_mtx);

#else /* #if ETHR_HAVE_PTHREAD_ATFORK */
    res = ENOTSUP;
#endif /* #if ETHR_HAVE_PTHREAD_ATFORK */
    return res;
}

int
ethr_mutex_trylock(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_mutex_trylock__(mtx);
}

int
ethr_mutex_lock(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_mutex_lock__(mtx);
}

int
ethr_mutex_unlock(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_mutex_unlock__(mtx);
}

/*
 * Condition variable functions
 */

int
ethr_cond_init(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd) {
	ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = ETHR_COND_INITIALIZED;
#endif
    return pthread_cond_init(&cnd->pt_cnd, NULL);
}

int
ethr_cond_destroy(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd || cnd->initialized != ETHR_COND_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = 0;
#endif
    return pthread_cond_destroy(&cnd->pt_cnd);
}

int
ethr_cond_signal(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd || cnd->initialized != ETHR_COND_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return pthread_cond_signal(&cnd->pt_cnd);
}

int
ethr_cond_broadcast(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd || cnd->initialized != ETHR_COND_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return pthread_cond_broadcast(&cnd->pt_cnd);
}

int
ethr_cond_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd
	|| cnd->initialized != ETHR_COND_INITIALIZED
	|| !mtx
	|| mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return pthread_cond_wait(&cnd->pt_cnd, &mtx->pt_mtx);
}

int
ethr_cond_timedwait(ethr_cond *cnd, ethr_mutex *mtx, ethr_timeval *timeout)
{
    struct timespec to;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd
	|| cnd->initialized != ETHR_COND_INITIALIZED
	|| !mtx
	|| mtx->initialized != ETHR_MUTEX_INITIALIZED
	|| !timeout) {
	ASSERT(0);
	return EINVAL;
    }
#endif

    to.tv_sec = timeout->tv_sec;
    to.tv_nsec = timeout->tv_nsec;

    return pthread_cond_timedwait(&cnd->pt_cnd, &mtx->pt_mtx, &to);
}


#ifdef ETHR_HAVE_PTHREAD_RWLOCK_INIT

int
ethr_rwmutex_init(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx) {
	ASSERT(0);
	return EINVAL;
    }
    rwmtx->initialized = ETHR_RWMUTEX_INITIALIZED;
#endif
    return pthread_rwlock_init(&rwmtx->pt_rwlock, write_pref_attr);
}

int
ethr_rwmutex_destroy(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = pthread_rwlock_destroy(&rwmtx->pt_rwlock);
#if ETHR_XCHK
    rwmtx->initialized = 0;
#endif
    return res;
}

int
ethr_rwmutex_tryrlock(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwmutex_tryrlock__(rwmtx);
}

int
ethr_rwmutex_rlock(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwmutex_rlock__(rwmtx);
}

int
ethr_rwmutex_runlock(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwmutex_runlock__(rwmtx);
}

int
ethr_rwmutex_tryrwlock(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwmutex_tryrwlock__(rwmtx);
}

int
ethr_rwmutex_rwlock(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwmutex_rwlock__(rwmtx);
}

int
ethr_rwmutex_rwunlock(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwmutex_rwunlock__(rwmtx);
}

#endif /* #ifdef ETHR_HAVE_PTHREAD_RWLOCK_INIT */

/*
 * Current time
 */

int
ethr_time_now(ethr_timeval *time)
{
    int res;
    struct timeval tv;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!time) {
	ASSERT(0);
	return EINVAL;
    }
#endif

    res = gettimeofday(&tv, NULL);
    time->tv_sec = (long) tv.tv_sec;
    time->tv_nsec = ((long) tv.tv_usec)*1000;
    return res;
}

/*
 * Thread specific data
 */

int
ethr_tsd_key_create(ethr_tsd_key *keyp)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!keyp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return pthread_key_create((pthread_key_t *) keyp, NULL);
}

int
ethr_tsd_key_delete(ethr_tsd_key key)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    return pthread_key_delete((pthread_key_t) key);
}

int
ethr_tsd_set(ethr_tsd_key key, void *value)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    return pthread_setspecific((pthread_key_t) key, value);
}

void *
ethr_tsd_get(ethr_tsd_key key)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return NULL;
    }
#endif
    return pthread_getspecific((pthread_key_t) key);
}

/*
 * Signal functions
 */

#if ETHR_HAVE_ETHR_SIG_FUNCS

int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!set && !oset) {
	ASSERT(0);
	return EINVAL;
    }
#endif
  return pthread_sigmask(how, set, oset);
}

int ethr_sigwait(const sigset_t *set, int *sig)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!set || !sig) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (sigwait(set, sig) < 0)
	return errno;
    return 0;
}

#endif /* #if ETHR_HAVE_ETHR_SIG_FUNCS */

#elif defined(ETHR_WIN32_THREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Native win32 threads implementation                                       *
\*                                                                           */

#define INVALID_TID -1

/* The spin count values are more or less taken out of the blue */
#define ETHR_MUTEX_SPIN_COUNT	5000
#define ETHR_COND_SPIN_COUNT	1000

ethr_tid serial_shift; /* Bits to shift serial when constructing a tid */
ethr_tid last_serial; /* Last thread table serial used */
ethr_tid last_ix; /* Last thread table index used */
ethr_tid thr_ix_mask; /* Mask used to mask out thread table index from a tid */

/* Event used for conditional variables. On per thread. */
/*typedef struct cnd_wait_event__ cnd_wait_event_;*/
struct cnd_wait_event__ {
    HANDLE handle;
    cnd_wait_event_ *prev;
    cnd_wait_event_ *next;
    int in_queue;
};

/* Thread specific data. Stored in the thread table */
typedef struct {
    ethr_tid thr_id;
    HANDLE thr_handle;
    ethr_tid joiner;
    void *result;
    cnd_wait_event_ wait_event;
} thr_data_;

/* Argument passed to thr_wrapper() */
typedef struct {
    void * (*func)(void *);
    void * arg;
    thr_data_ *ptd;
    thr_data_ *td;
    int res;
    void *prep_func_res;
} thr_wrap_data_;


static CRITICAL_SECTION thr_table_cs; /* Critical section used to protect
					 the thread table from concurrent
					 accesses. */
static CRITICAL_SECTION fake_static_init_cs; /* Critical section used to protect
					initialazition of 'statically
					initialized' mutexes */
static thr_data_ * thr_table[ETHR_MAX_THREADS]; /* The thread table */

static DWORD tls_own_thr_data;

static thr_data_ main_thr_data;

#define THR_IX(TID)	((TID) & thr_ix_mask)
#define OWN_THR_DATA	((thr_data_ *) TlsGetValue(tls_own_thr_data))

/*
 * ----------------------------------------------------------------------------
 * Static functions
 * ----------------------------------------------------------------------------
 */

static int
get_errno(void)
{
    switch (GetLastError()) {
    case ERROR_INVALID_FUNCTION:		return EINVAL;	/* 1	*/
    case ERROR_FILE_NOT_FOUND:			return ENOENT;	/* 2	*/
    case ERROR_PATH_NOT_FOUND:			return ENOENT;	/* 3	*/
    case ERROR_TOO_MANY_OPEN_FILES:		return EMFILE;	/* 4	*/
    case ERROR_ACCESS_DENIED:			return EACCES;	/* 5	*/
    case ERROR_INVALID_HANDLE:			return EBADF;	/* 6	*/
    case ERROR_ARENA_TRASHED:			return ENOMEM;	/* 7	*/
    case ERROR_NOT_ENOUGH_MEMORY:		return ENOMEM;	/* 8	*/
    case ERROR_INVALID_BLOCK:			return ENOMEM;	/* 9	*/
    case ERROR_BAD_ENVIRONMENT:			return E2BIG;	/* 10	*/
    case ERROR_BAD_FORMAT:			return ENOEXEC;	/* 11	*/
    case ERROR_INVALID_ACCESS:			return EINVAL;	/* 12	*/
    case ERROR_INVALID_DATA:			return EINVAL;	/* 13	*/
    case ERROR_OUTOFMEMORY:			return ENOMEM;	/* 14	*/
    case ERROR_INVALID_DRIVE:			return ENOENT;	/* 15	*/
    case ERROR_CURRENT_DIRECTORY:		return EACCES;	/* 16	*/
    case ERROR_NOT_SAME_DEVICE:			return EXDEV;	/* 17	*/
    case ERROR_NO_MORE_FILES:			return ENOENT;	/* 18	*/
    case ERROR_WRITE_PROTECT:			return EACCES;	/* 19	*/
    case ERROR_BAD_UNIT:			return EACCES;	/* 20	*/
    case ERROR_NOT_READY:			return EACCES;	/* 21	*/
    case ERROR_BAD_COMMAND:			return EACCES;	/* 22	*/
    case ERROR_CRC:				return EACCES;	/* 23	*/
    case ERROR_BAD_LENGTH:			return EACCES;	/* 24	*/
    case ERROR_SEEK:				return EACCES;	/* 25	*/
    case ERROR_NOT_DOS_DISK:			return EACCES;	/* 26	*/
    case ERROR_SECTOR_NOT_FOUND:		return EACCES;	/* 27	*/
    case ERROR_OUT_OF_PAPER:			return EACCES;	/* 28	*/
    case ERROR_WRITE_FAULT:			return EACCES;	/* 29	*/
    case ERROR_READ_FAULT:			return EACCES;	/* 30	*/
    case ERROR_GEN_FAILURE:			return EACCES;	/* 31	*/
    case ERROR_SHARING_VIOLATION:		return EACCES;	/* 32	*/
    case ERROR_LOCK_VIOLATION:			return EACCES;	/* 33	*/
    case ERROR_WRONG_DISK:			return EACCES;	/* 34	*/
    case ERROR_SHARING_BUFFER_EXCEEDED:		return EACCES;	/* 36	*/
    case ERROR_BAD_NETPATH:			return ENOENT;	/* 53	*/
    case ERROR_NETWORK_ACCESS_DENIED:		return EACCES;	/* 65	*/
    case ERROR_BAD_NET_NAME:			return ENOENT;	/* 67	*/
    case ERROR_FILE_EXISTS:			return EEXIST;	/* 80	*/
    case ERROR_CANNOT_MAKE:			return EACCES;	/* 82	*/
    case ERROR_FAIL_I24:			return EACCES;	/* 83	*/
    case ERROR_INVALID_PARAMETER:		return EINVAL;	/* 87	*/
    case ERROR_NO_PROC_SLOTS:			return EAGAIN;	/* 89	*/
    case ERROR_DRIVE_LOCKED:			return EACCES;	/* 108	*/
    case ERROR_BROKEN_PIPE:			return EPIPE;	/* 109	*/
    case ERROR_DISK_FULL:			return ENOSPC;	/* 112	*/
    case ERROR_INVALID_TARGET_HANDLE:		return EBADF;	/* 114	*/
    case ERROR_WAIT_NO_CHILDREN:		return ECHILD;	/* 128	*/
    case ERROR_CHILD_NOT_COMPLETE:		return ECHILD;	/* 129	*/
    case ERROR_DIRECT_ACCESS_HANDLE:		return EBADF;	/* 130	*/
    case ERROR_NEGATIVE_SEEK:			return EINVAL;	/* 131	*/
    case ERROR_SEEK_ON_DEVICE:			return EACCES;	/* 132	*/
    case ERROR_DIR_NOT_EMPTY:			return ENOTEMPTY;/* 145	*/
    case ERROR_NOT_LOCKED:			return EACCES;	/* 158	*/
    case ERROR_BAD_PATHNAME:			return ENOENT;	/* 161	*/
    case ERROR_MAX_THRDS_REACHED:		return EAGAIN;	/* 164	*/
    case ERROR_LOCK_FAILED:			return EACCES;	/* 167	*/
    case ERROR_ALREADY_EXISTS:			return EEXIST;	/* 183	*/
    case ERROR_INVALID_STARTING_CODESEG:	return ENOEXEC;	/* 188	*/
    case ERROR_INVALID_STACKSEG:		return ENOEXEC;	/* 189	*/
    case ERROR_INVALID_MODULETYPE:		return ENOEXEC;	/* 190	*/
    case ERROR_INVALID_EXE_SIGNATURE:		return ENOEXEC;	/* 191	*/
    case ERROR_EXE_MARKED_INVALID:		return ENOEXEC;	/* 192	*/
    case ERROR_BAD_EXE_FORMAT:			return ENOEXEC;	/* 193	*/
    case ERROR_ITERATED_DATA_EXCEEDS_64k:	return ENOEXEC;	/* 194	*/
    case ERROR_INVALID_MINALLOCSIZE:		return ENOEXEC;	/* 195	*/
    case ERROR_DYNLINK_FROM_INVALID_RING:	return ENOEXEC;	/* 196	*/
    case ERROR_IOPL_NOT_ENABLED:		return ENOEXEC;	/* 197	*/
    case ERROR_INVALID_SEGDPL:			return ENOEXEC;	/* 198	*/
    case ERROR_AUTODATASEG_EXCEEDS_64k:		return ENOEXEC;	/* 199	*/
    case ERROR_RING2SEG_MUST_BE_MOVABLE:	return ENOEXEC;	/* 200	*/
    case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:	return ENOEXEC;	/* 201	*/
    case ERROR_INFLOOP_IN_RELOC_CHAIN:		return ENOEXEC;	/* 202	*/
    case ERROR_FILENAME_EXCED_RANGE:		return ENOENT;	/* 206	*/
    case ERROR_NESTING_NOT_ALLOWED:		return EAGAIN;	/* 215	*/
    case ERROR_NOT_ENOUGH_QUOTA:		return ENOMEM;	/* 1816	*/
    default:					return EINVAL;
    }
}

static ETHR_INLINE thr_data_ *
tid2thr(ethr_tid tid)
{
    ethr_tid ix;
    thr_data_ *td;
    
    if (tid < 0)
	return NULL;
    ix = THR_IX(tid);
    if (ix >= ETHR_MAX_THREADS)
	return NULL;
    td = thr_table[ix];
    if (!td)
	return NULL;
    if (td->thr_id != tid)
	return NULL;
    return td;
}

static ETHR_INLINE void
new_tid(ethr_tid *new_tid, ethr_tid *new_serial, ethr_tid *new_ix)
{
    ethr_tid tmp_serial = last_serial;
    ethr_tid tmp_ix = last_ix + 1;
    ethr_tid start_ix = tmp_ix;


    do {
	if (tmp_ix >= ETHR_MAX_THREADS) {
	    tmp_serial++;
	    if ((tmp_serial << serial_shift) < 0)
		tmp_serial = 0;
	    tmp_ix = 0;
	}
	if (!thr_table[tmp_ix]) {
	    *new_tid	= (tmp_serial << serial_shift) | tmp_ix;
	    *new_serial	= tmp_serial;
	    *new_ix	= tmp_ix;
	    return;
	}
	tmp_ix++;
    } while (tmp_ix != start_ix);

    *new_tid	= INVALID_TID;
    *new_serial	= INVALID_TID;
    *new_ix	= INVALID_TID;

}


static void thr_exit_cleanup(thr_data_ *td, void *res)
{

    ASSERT(td == OWN_THR_DATA);

    run_exit_handlers();

    EnterCriticalSection(&thr_table_cs);
    CloseHandle(td->wait_event.handle);
    if (td->thr_handle == INVALID_HANDLE_VALUE) {
	/* We are detached; cleanup thread table */
	ASSERT(td->joiner == INVALID_TID);
	ASSERT(td == thr_table[THR_IX(td->thr_id)]);
	thr_table[THR_IX(td->thr_id)] = NULL;
	if (td != &main_thr_data)
	    (*freep)((void *) td);
    }
    else {
	/* Save result and let joining thread cleanup */
	td->result = res;
    }
    LeaveCriticalSection(&thr_table_cs);
}

static unsigned __stdcall thr_wrapper(LPVOID args)
{
    void *(*func)(void*) = ((thr_wrap_data_ *) args)->func;
    void *arg = ((thr_wrap_data_ *) args)->arg;
    thr_data_ *td = ((thr_wrap_data_ *) args)->td;

    td->wait_event.handle = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (td->wait_event.handle == INVALID_HANDLE_VALUE
	|| !TlsSetValue(tls_own_thr_data, (LPVOID) td)) {
	((thr_wrap_data_ *) args)->res = get_errno();
	if (td->wait_event.handle != INVALID_HANDLE_VALUE)
	    CloseHandle(td->wait_event.handle);
	SetEvent(((thr_wrap_data_ *) args)->ptd->wait_event.handle);
	_endthreadex((unsigned) 0);
	ASSERT(0);
    }

    td->wait_event.prev = NULL;
    td->wait_event.next = NULL;
    td->wait_event.in_queue = 0;

    if (thread_create_child_func)
	(*thread_create_child_func)(((thr_wrap_data_ *) args)->prep_func_res);

    ASSERT(td == OWN_THR_DATA);

    ((thr_wrap_data_ *) args)->res = 0;
    SetEvent(((thr_wrap_data_ *) args)->ptd->wait_event.handle);

    thr_exit_cleanup(td, (*func)(arg));
    return 0;
}

int
ethr_fake_static_mutex_init(ethr_mutex *mtx)
{
    EnterCriticalSection((CRITICAL_SECTION *) &fake_static_init_cs);
    /* Got here under race conditions; check again... */
    if (!mtx->initialized) {
	if (!InitializeCriticalSectionAndSpinCount(&mtx->cs,
						   ETHR_MUTEX_SPIN_COUNT))
	    return get_errno();
	mtx->initialized = ETHR_MUTEX_INITIALIZED;
    }
    LeaveCriticalSection((CRITICAL_SECTION *) &fake_static_init_cs);
    return 0;
}

static int
fake_static_cond_init(ethr_cond *cnd)
{
    EnterCriticalSection((CRITICAL_SECTION *) &fake_static_init_cs);
    /* Got here under race conditions; check again... */
    if (!cnd->initialized) {
	if (!InitializeCriticalSectionAndSpinCount(&cnd->cs,
						   ETHR_COND_SPIN_COUNT))
	    return get_errno();
	cnd->queue = NULL;
	cnd->queue_end = NULL;
	cnd->initialized = ETHR_COND_INITIALIZED;
    }
    LeaveCriticalSection((CRITICAL_SECTION *) &fake_static_init_cs);
    return 0;
}

#ifdef __GNUC__
#define LL_LITERAL(X) X##LL
#else
#define LL_LITERAL(X) X##i64
#endif

#define EPOCH_JULIAN_DIFF LL_LITERAL(11644473600)

static ETHR_INLINE void
get_curr_time(long *sec, long *nsec)
{
    SYSTEMTIME t;
    FILETIME ft;
    LONGLONG lft;

    GetSystemTime(&t);
    SystemTimeToFileTime(&t, &ft);
    memcpy(&lft, &ft, sizeof(lft));
    *nsec = ((long) (lft % LL_LITERAL(10000000)))*100;
    *sec = (long) ((lft / LL_LITERAL(10000000)) - EPOCH_JULIAN_DIFF);
}

static cnd_wait_event_ *cwe_freelist;
static CRITICAL_SECTION cwe_cs;

static int
alloc_cwe(cnd_wait_event_ **cwe_res)
{
    cnd_wait_event_ *cwe;
    EnterCriticalSection(&cwe_cs);
    cwe = cwe_freelist;
    if (cwe) {
	cwe_freelist = cwe->next;
	LeaveCriticalSection(&cwe_cs);
    }
    else {
	LeaveCriticalSection(&cwe_cs);
	cwe = (*allocp)(sizeof(cnd_wait_event_));
	if (!cwe)
	    return ENOMEM;
	cwe->handle = CreateEvent(NULL, FALSE, FALSE, NULL);
	if (cwe->handle == INVALID_HANDLE_VALUE) {
	    int res = get_errno();
	    (*freep)(cwe);
	    return res;
	}
    }
    *cwe_res = cwe;
    return 0;
}

static
free_cwe(cnd_wait_event_ *cwe)
{
    EnterCriticalSection(&cwe_cs);
    cwe->next = cwe_freelist;
    cwe_freelist = cwe;
    LeaveCriticalSection(&cwe_cs);
}

static ETHR_INLINE int
condwait(ethr_cond *cnd,
	 ethr_mutex *mtx,
	 int with_timeout,
	 ethr_timeval *timeout)
{
    int res;
    thr_data_ *td;
    cnd_wait_event_ *cwe;
    DWORD code;
    long time; /* time until timeout in milli seconds */

#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }

    if (!mtx
	|| mtx->initialized != ETHR_MUTEX_INITIALIZED
	|| !cnd
	|| (cnd->initialized && cnd->initialized != ETHR_COND_INITIALIZED)
	|| (with_timeout && !timeout)) {
	ASSERT(0);
	return EINVAL;
    }
#endif

    td = OWN_THR_DATA;
    if (td)
	cwe = &td->wait_event;
    else { /* A non-ethread thread */
	res = alloc_cwe(&cwe);
	if (res != 0)
	    return res;
    }

    if (!cnd->initialized)
	fake_static_cond_init(cnd);
    EnterCriticalSection(&cnd->cs);

    ASSERT(!cwe->in_queue);
    if (cnd->queue_end) {
	ASSERT(cnd->queue);
	cwe->prev = cnd->queue_end;
	cwe->next = NULL;
	cnd->queue_end->next = cwe;
	cnd->queue_end = cwe;
    }
    else {
	ASSERT(!cnd->queue);
	cwe->prev = NULL;
	cwe->next = NULL;
	cnd->queue = cwe;
	cnd->queue_end = cwe;
    }
    cwe->in_queue = 1;

    LeaveCriticalSection(&cnd->cs);
 
    LeaveCriticalSection(&mtx->cs);

    if (!with_timeout)
	time = INFINITE;
    else {
	long sec, nsec;
	ASSERT(timeout);
	get_curr_time(&sec, &nsec);
	time = (timeout->tv_sec - sec)*1000;
	time += (timeout->tv_nsec - nsec + 500)/1000000;
	if (time < 0)
	    time = 0;
    }

    /* wait for event to signal */
    code = WaitForSingleObject(cwe->handle, time);

    EnterCriticalSection(&mtx->cs);

    if (code == WAIT_OBJECT_0) {
	/* We were woken by a signal or a broadcast ... */
	res = 0;

	/* ... no need to remove event from wait queue since this was
	   taken care of by the signal or broadcast */
#ifdef DEBUG
	EnterCriticalSection(&cnd->cs);
	ASSERT(!cwe->in_queue);
	LeaveCriticalSection(&cnd->cs);
#endif

    }
    else {
	/* We timed out... */
	res = ETIMEDOUT;

	/* ... probably have to remove event from wait queue ... */
	EnterCriticalSection(&cnd->cs);

	if (cwe->in_queue) { /* ... but we must check that we are in queue
				since a signal or broadcast after timeout
				may have removed us from the queue */
	    if (cwe->prev) {
		cwe->prev->next = cwe->next;
	    }
	    else {
		ASSERT(cnd->queue == cwe);
		cnd->queue = cwe->next;
	    }

	    if (cwe->next) {
		cwe->next->prev = cwe->prev;
	    }
	    else {
		ASSERT(cnd->queue_end == cwe);
		cnd->queue_end = cwe->prev;
	    }
	    cwe->in_queue = 0;
	}

	LeaveCriticalSection(&cnd->cs);

    }

    if (!td)
	free_cwe(cwe);

    return res;

}


/*
 * ----------------------------------------------------------------------------
 * Exported functions
 * ----------------------------------------------------------------------------
 */

int
ethr_init(ethr_init_data *id)
{
#ifdef _WIN32_WINNT
    DWORD major = (_WIN32_WINNT >> 8) & 0xff;
    DWORD minor = _WIN32_WINNT & 0xff;
    OSVERSIONINFO os_version;
#endif
    int err = 0;
    thr_data_ *td = &main_thr_data;
    unsigned long i;

    if (!ethr_not_inited)
	return EINVAL;

#ifdef _WIN32_WINNT
    os_version.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&os_version);
    if (os_version.dwPlatformId != VER_PLATFORM_WIN32_NT
	|| os_version.dwMajorVersion < major
	|| (os_version.dwMajorVersion == major
	    && os_version.dwMinorVersion < minor))
	return ENOTSUP;
#endif

    ASSERT(ETHR_MAX_THREADS > 0);
    for (i = ETHR_MAX_THREADS - 1, serial_shift = 0;
	 i;
	 serial_shift++, i >>= 1);
    thr_ix_mask = ~(~((ethr_tid) 0) << serial_shift);

    tls_own_thr_data = TlsAlloc();
    if (tls_own_thr_data == TLS_OUT_OF_INDEXES)
	goto error;

    last_serial = 0;
    last_ix = 0;

    td->thr_id = 0;
    td->thr_handle = GetCurrentThread();
    td->joiner = INVALID_TID;
    td->result = NULL;
    td->wait_event.handle = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (td->wait_event.handle == INVALID_HANDLE_VALUE)
	goto error;
    td->wait_event.prev = NULL;
    td->wait_event.next = NULL;
    td->wait_event.in_queue = 0;
    thr_table[0] = td;

    if (!TlsSetValue(tls_own_thr_data, (LPVOID) td))
	goto error;

    ASSERT(td == OWN_THR_DATA);


    cwe_freelist = NULL;
    if (!InitializeCriticalSectionAndSpinCount(&cwe_cs,
					       ETHR_MUTEX_SPIN_COUNT))
	goto error;

    for (i = 1; i < ETHR_MAX_THREADS; i++)
	thr_table[i] = NULL;

    if (!InitializeCriticalSectionAndSpinCount(&thr_table_cs,
					       ETHR_MUTEX_SPIN_COUNT))
	goto error;
    if (!InitializeCriticalSectionAndSpinCount(&fake_static_init_cs,
					       ETHR_MUTEX_SPIN_COUNT))
	goto error;
    ethr_not_inited = 0;

    err = init_common(id);
    if (err)
	goto error;

    return 0;

 error:
    ethr_not_inited = 1;
    if (err == 0)
	err = get_errno();
    ASSERT(err != 0);
    if (td->thr_handle != INVALID_HANDLE_VALUE)
	CloseHandle(td->thr_handle);
    if (td->wait_event.handle != INVALID_HANDLE_VALUE)
	CloseHandle(td->wait_event.handle);
    return err;
}

/*
 * Thread functions.
 */

int
ethr_thr_create(ethr_tid *tid, void * (*func)(void *), void *arg,
		ethr_thr_opts *opts)
{
    int err = 0;
    thr_wrap_data_ twd;
    thr_data_ *my_td, *child_td = NULL;
    ethr_tid child_tid, child_serial, child_ix;
    DWORD code;
    unsigned ID;
    unsigned stack_size = 0; /* 0 = system default */
    int use_stack_size = (opts && opts->suggested_stack_size >= 0
			  ? opts->suggested_stack_size
			  : -1 /* Use system default */);

#ifdef ETHR_MODIFIED_DEFAULT_STACK_SIZE
    if (use_stack_size < 0)
	use_stack_size = ETHR_MODIFIED_DEFAULT_STACK_SIZE;
#endif

#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!tid || !func) {
	ASSERT(0);
	return EINVAL;
    }
#endif

    my_td = OWN_THR_DATA;
    if (!my_td) {
	/* Only ethreads are allowed to call this function */
	ASSERT(0);
	return EACCES;
    }

    if (use_stack_size >= 0) {
	size_t suggested_stack_size = (size_t) use_stack_size;
#ifdef DEBUG
	suggested_stack_size /= 2; /* Make sure we got margin */
#endif
	if (suggested_stack_size < min_stack_size)
	    stack_size = (unsigned) ETHR_KW2B(min_stack_size);
	else if (suggested_stack_size > max_stack_size)
	    stack_size = (unsigned) ETHR_KW2B(max_stack_size);
	else
	    stack_size =
		(unsigned) ETHR_PAGE_ALIGN(ETHR_KW2B(suggested_stack_size));
    }

    EnterCriticalSection(&thr_table_cs);

    /* Call prepare func if it exist */
    if (thread_create_prepare_func)
	twd.prep_func_res = (*thread_create_prepare_func)();
    else
	twd.prep_func_res = NULL;

    /* Find a new thread id to use */
    new_tid(&child_tid, &child_serial, &child_ix);
    if (child_tid == INVALID_TID) {
	err = EAGAIN;
	goto error;
    }

    ASSERT(child_ix == THR_IX(child_tid));

    *tid = child_tid;

    ASSERT(!thr_table[child_ix]);

    /* Alloc thread data */
    thr_table[child_ix] = child_td = (thr_data_ *) (*allocp)(sizeof(thr_data_));
    if (!child_td) {
	err = ENOMEM;
	goto error;
    }

    /* Init thread data */

    child_td->thr_id		= child_tid;
    child_td->thr_handle	= INVALID_HANDLE_VALUE;
    child_td->joiner		= INVALID_TID;
    child_td->result		= NULL;
    /* 'child_td->wait_event' is initialized by child thread */


    /* Init thread wrapper data */

    twd.func			= func;
    twd.arg			= arg;
    twd.ptd			= my_td;
    twd.td			= child_td;
    twd.res			= 0;

    ASSERT(!my_td->wait_event.in_queue);

    /* spawn the thr_wrapper function */
    child_td->thr_handle = (HANDLE) _beginthreadex(NULL,
						   stack_size,
						   thr_wrapper,
						   (LPVOID) &twd,
						   0,
						   &ID);
    if (child_td->thr_handle == (HANDLE) 0) {
	child_td->thr_handle = INVALID_HANDLE_VALUE;
	goto error;
    }

    ASSERT(child_td->thr_handle != INVALID_HANDLE_VALUE);

    /* Wait for child to finish initialization */
    code = WaitForSingleObject(my_td->wait_event.handle, INFINITE);
    if (twd.res || code != WAIT_OBJECT_0) {
	err = twd.res;
	goto error;
    }

    if (opts && opts->detached) {
	CloseHandle(child_td->thr_handle);
	child_td->thr_handle = INVALID_HANDLE_VALUE;
    }

    last_serial	= child_serial;
    last_ix	= child_ix;

    ASSERT(thr_table[child_ix] == child_td);

    if (thread_create_parent_func)
	(*thread_create_parent_func)(twd.prep_func_res);

    LeaveCriticalSection(&thr_table_cs);

    return 0;

 error:

    if (err == 0)
	err = get_errno();
    ASSERT(err != 0);

    if (thread_create_parent_func)
	(*thread_create_parent_func)(twd.prep_func_res);

    if (child_ix != INVALID_TID) {

	if (child_td) {
	    ASSERT(thr_table[child_ix] == child_td);

	    if (child_td->thr_handle != INVALID_HANDLE_VALUE) {
		WaitForSingleObject(child_td->thr_handle, INFINITE);
		CloseHandle(child_td->thr_handle);
	    }

	    (*freep)((void *) child_td);
	    thr_table[child_ix] = NULL;
	}
    }

    *tid = INVALID_TID;

    LeaveCriticalSection(&thr_table_cs);
    return err;
}

int ethr_thr_join(ethr_tid tid, void **res)
{
    int err = 0;
    DWORD code;
    thr_data_ *td;
    thr_data_ *my_td;

#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif

    my_td = OWN_THR_DATA;

    if (!my_td) {
	/* Only ethreads are allowed to call this function */
	ASSERT(0);
	return EACCES;
    }

    EnterCriticalSection(&thr_table_cs);

    td = tid2thr(tid);
    if (!td)
	err = ESRCH;
    else if (td->thr_handle == INVALID_HANDLE_VALUE /* i.e. detached */
	     || td->joiner != INVALID_TID) /* i.e. someone else is joining */
	err = EINVAL;
    else if (my_td == td)
	err = EDEADLK;
    else
	td->joiner = my_td->thr_id;

    LeaveCriticalSection(&thr_table_cs);

    if (err)
	goto error;

    /* Wait for thread to terminate */
    code = WaitForSingleObject(td->thr_handle, INFINITE);
    if (code != WAIT_OBJECT_0)
	goto error;

    EnterCriticalSection(&thr_table_cs);

    ASSERT(td == tid2thr(tid));
    ASSERT(td->thr_handle != INVALID_HANDLE_VALUE);
    ASSERT(td->joiner == my_td->thr_id);

    if (res)
	*res = td->result;

    CloseHandle(td->thr_handle);
    ASSERT(td == thr_table[THR_IX(td->thr_id)]);
    thr_table[THR_IX(td->thr_id)] = NULL;
    if (td != &main_thr_data)
	(*freep)((void *) td);

    LeaveCriticalSection(&thr_table_cs);

    return 0;

 error:
    if (err == 0)
	err = get_errno();
    ASSERT(err != 0);
    return err;
}


int
ethr_thr_detach(ethr_tid tid)
{
    int res;
    DWORD code;
    thr_data_ *td;

#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif

    if (!OWN_THR_DATA) {
	/* Only ethreads are allowed to call this function */
	ASSERT(0);
	return EACCES;
    }

    EnterCriticalSection(&thr_table_cs);

    td = tid2thr(tid);
    if (!td)
	res = ESRCH;
    if (td->thr_handle == INVALID_HANDLE_VALUE /* i.e. detached */
	|| td->joiner != INVALID_TID) /* i.e. someone is joining */
	res = EINVAL;
    else {
	res = 0;
	CloseHandle(td->thr_handle);
	td->thr_handle = INVALID_HANDLE_VALUE;
    }

    LeaveCriticalSection(&thr_table_cs);

    return res;
}


void
ethr_thr_exit(void *res)
{
    thr_data_ *td;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return;
    }
#endif
    td = OWN_THR_DATA;
    if (!td) {
	/* Only ethreads are allowed to call this function */
	ASSERT(0);
	return;
    }
    thr_exit_cleanup(td, res);
    _endthreadex((unsigned) 0);
}

ethr_tid
ethr_self(void)
{
    thr_data_ *td;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return INVALID_TID;
    }
#endif
    /* It is okay for non-ethreads (i.e. native win32 threads) to call
       ethr_self(). They will however be returned the INVALID_TID. */
    td = OWN_THR_DATA;
    if (!td)
	return INVALID_TID;
    return td->thr_id;
}

int
ethr_equal_tids(ethr_tid tid1, ethr_tid tid2)
{
    /* INVALID_TID does not equal any tid, not even the INVALID_TID */
    return tid1 == tid2 && tid1 != INVALID_TID;
}

/*
 * Mutex functions.
 */

int
ethr_mutex_init(ethr_mutex *mtx)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (!InitializeCriticalSectionAndSpinCount(&mtx->cs, ETHR_MUTEX_SPIN_COUNT))
	return get_errno();
    mtx->initialized = ETHR_MUTEX_INITIALIZED;
#if ETHR_XCHK
    mtx->is_rec_mtx = 0;
#endif
    return 0;
}

int
ethr_rec_mutex_init(ethr_mutex *mtx)
{
    int res;
    res = ethr_mutex_init(mtx);
#if ETHR_XCHK
    mtx->is_rec_mtx = 1;
#endif
    return res;
}

int
ethr_mutex_destroy(ethr_mutex *mtx)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    DeleteCriticalSection(&mtx->cs);
    mtx->initialized = 0;
    return 0;
}

int ethr_mutex_set_forksafe(ethr_mutex *mtx)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    return 0; /* No fork() */
}

int ethr_mutex_unset_forksafe(ethr_mutex *mtx)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    return 0; /* No fork() */
}

int
ethr_mutex_trylock(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx
	|| (mtx->initialized && mtx->initialized != ETHR_MUTEX_INITIALIZED)) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (!mtx->initialized) {
	int res = ethr_fake_static_mutex_init(mtx);
	if (res != 0)
	    return res;
    }
    return ethr_mutex_trylock__(mtx);
}

int
ethr_mutex_lock(ethr_mutex *mtx)
{
    int res;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx
	|| (mtx->initialized && mtx->initialized != ETHR_MUTEX_INITIALIZED)) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_mutex_lock__(mtx);
}

int
ethr_mutex_unlock(ethr_mutex *mtx)
{
#if ETHR_XCHK
    int res;
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_mutex_unlock__(mtx);
}

/*
 * Condition variable functions.
 */

int
ethr_cond_init(ethr_cond *cnd)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (!InitializeCriticalSectionAndSpinCount(&cnd->cs, ETHR_COND_SPIN_COUNT))
	return get_errno();
    cnd->queue = NULL;
    cnd->queue_end = NULL;
    cnd->initialized = ETHR_COND_INITIALIZED;
    return 0;
}

int
ethr_cond_destroy(ethr_cond *cnd)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd
	|| (cnd->initialized && cnd->initialized != ETHR_COND_INITIALIZED)
	|| cnd->queue) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    DeleteCriticalSection(&cnd->cs);
    cnd->initialized = 0;
    return 0;
}

int
ethr_cond_signal(ethr_cond *cnd)
{
    cnd_wait_event_ *cwe;
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd
	|| (cnd->initialized && cnd->initialized != ETHR_COND_INITIALIZED)) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (!cnd->initialized) {
	int res = fake_static_cond_init(cnd);
	if (res != 0)
	    return res;
    }
    EnterCriticalSection(&cnd->cs);
    cwe = cnd->queue;
    if (cwe) {
	ASSERT(cwe->in_queue);
	SetEvent(cnd->queue->handle);
	if (cwe->next)
	    cwe->next->prev = NULL;
	else {
	    ASSERT(cnd->queue_end == cnd->queue);
	    cnd->queue_end = NULL;
	}
	cnd->queue = cwe->next;
	cwe->in_queue = 0;
    }
    LeaveCriticalSection(&cnd->cs);
    return 0;
}

int
ethr_cond_broadcast(ethr_cond *cnd)
{
    cnd_wait_event_ *cwe;

#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!cnd
	|| (cnd->initialized && cnd->initialized != ETHR_COND_INITIALIZED)) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (!cnd->initialized) {
	int res = fake_static_cond_init(cnd);
	if (res != 0)
	    return res;
    }
    EnterCriticalSection(&cnd->cs);
    for (cwe = cnd->queue; cwe; cwe = cwe->next) {
	ASSERT(cwe->in_queue);
	SetEvent(cwe->handle);
	cwe->in_queue = 0;
    }
    cnd->queue = NULL;
    cnd->queue_end = NULL;
    LeaveCriticalSection(&cnd->cs);
    return 0;

}

int
ethr_cond_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
    return condwait(cnd, mtx, 0, NULL);
}

int
ethr_cond_timedwait(ethr_cond *cnd, ethr_mutex *mtx, ethr_timeval *timeout)
{
    return condwait(cnd, mtx, 1, timeout);
}

int
ethr_time_now(ethr_timeval *time)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!time) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    get_curr_time(&time->tv_sec, &time->tv_nsec);
    return 0;
}

/*
 * Thread specific data
 */

int
ethr_tsd_key_create(ethr_tsd_key *keyp)
{
    DWORD key;
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!keyp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    key = TlsAlloc();
    if (key == TLS_OUT_OF_INDEXES)
	return get_errno();
    *keyp = (ethr_tsd_key) key;
    return 0;
}

int
ethr_tsd_key_delete(ethr_tsd_key key)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    if (!TlsFree((DWORD) key))
	return get_errno();
    return 0;
}

int
ethr_tsd_set(ethr_tsd_key key, void *value)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
#endif
    if (!TlsSetValue((DWORD) key, (LPVOID) value))
	return get_errno();
    return 0;
}

void *
ethr_tsd_get(ethr_tsd_key key)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return NULL;
    }
#endif
    return (void *) TlsGetValue((DWORD) key);
}

/* Misc */

#ifndef ETHR_HAVE_OPTIMIZED_LOCKS

int
ethr_do_spinlock_init(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    if (InitializeCriticalSectionAndSpinCount(&lock->cs, INT_MAX))
	return 0;
    else
	return get_errno();
}

int
ethr_do_rwlock_init(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    lock->counter = 0;
    if (InitializeCriticalSectionAndSpinCount(&lock->cs, INT_MAX))
	return 0;
    else
	return get_errno();    
}

#endif /* #ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS */

#else
#error "Missing thread implementation"
#endif

/* Atomics */

int
ethr_atomic_init(ethr_atomic_t *var, long i)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
       ASSERT(0);
       return EACCES;
    }  
    if (!var) {
       ASSERT(0);
       return EINVAL;
    }  
#endif
    return ethr_atomic_init__(var, i);
}

int
ethr_atomic_set(ethr_atomic_t *var, long i)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
       ASSERT(0);
       return EACCES;
    }  
    if (!var) {
       ASSERT(0);
       return EINVAL;
    }  
#endif
    return ethr_atomic_set__(var, i);
}

int
ethr_atomic_read(ethr_atomic_t *var, long *i)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
       ASSERT(0);
       return EACCES;
    }  
    if (!var || !i) {
       ASSERT(0);
       return EINVAL;
    }  
#endif
    return ethr_atomic_read__(var, i);
}


int
ethr_atomic_addtest(ethr_atomic_t *var, long incr, long *testp)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
       ASSERT(0);
       return EACCES;
    }  
    if (!var || !testp) {
       ASSERT(0);
       return EINVAL;
    }  
#endif
    return ethr_atomic_addtest__(var, incr, testp);
}   
    
int
ethr_atomic_inctest(ethr_atomic_t *incp, long *testp)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!incp || !testp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_atomic_inctest__(incp, testp);
}

int
ethr_atomic_dectest(ethr_atomic_t *decp, long *testp)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!decp || !testp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_atomic_dectest__(decp, testp);
}

int
ethr_atomic_add(ethr_atomic_t *var, long incr)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
       ASSERT(0);
       return EACCES;
    }  
    if (!var) {
       ASSERT(0);
       return EINVAL;
    }  
#endif
    return ethr_atomic_add__(var, incr);
}   
    
int 
ethr_atomic_inc(ethr_atomic_t *incp)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!incp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_atomic_inc__(incp);
}

int
ethr_atomic_dec(ethr_atomic_t *decp)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!decp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_atomic_dec__(decp);
}

int
ethr_atomic_and_old(ethr_atomic_t *var, long mask, long *old)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!var || !old) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_atomic_and_old__(var, mask, old);
}

int
ethr_atomic_or_old(ethr_atomic_t *var, long mask, long *old)
{
#if ETHR_XCHK
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!var || !old) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_atomic_or_old__(var, mask, old);
}

int
ethr_atomic_xchg(ethr_atomic_t *var, long new, long *old)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }  
    if (!var || !old) {
	ASSERT(0);
	return EINVAL;
    }  
#endif
    return ethr_atomic_xchg__(var, new, old);
}   

int
ethr_atomic_cmpxchg(ethr_atomic_t *var, long new, long expected, long *old)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }  
    if (!var || !old) {
	ASSERT(0);
	return EINVAL;
    }  
#endif
    return ethr_atomic_cmpxchg__(var, new, expected, old);
}

/* Spinlocks and rwspinlocks */

int
ethr_spinlock_init(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_spinlock_init__(lock);
}

int
ethr_spinlock_destroy(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_spinlock_destroy__(lock);
}


int
ethr_spin_unlock(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_spin_unlock__(lock);
}

int
ethr_spin_lock(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_spin_lock__(lock);
}

int
ethr_rwlock_init(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwlock_init__(lock);
}

int
ethr_rwlock_destroy(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwlock_destroy__(lock);
}

int
ethr_read_unlock(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_read_unlock__(lock);
}

int
ethr_read_lock(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_read_lock__(lock);
}

int
ethr_write_unlock(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_write_unlock__(lock);
}

int
ethr_write_lock(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_write_lock__(lock);
}


int
ethr_gate_init(ethr_gate *gp)
{
    int res;
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!gp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_init(&gp->mtx);
    if (res != 0)
	return res;
    res = ethr_cond_init(&gp->cnd);
    if (res != 0) {
	ethr_mutex_destroy(&gp->mtx);
	return res;
    }
    gp->open = 0;
    return 0;
}

int
ethr_gate_destroy(ethr_gate *gp)
{
    int res, dres;
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!gp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_destroy(&gp->mtx);
    dres = ethr_cond_destroy(&gp->cnd);
    if (res == 0)
	res = dres;
    gp->open = 0;
    return res;
}

int
ethr_gate_close(ethr_gate *gp)
{
    int res;
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!gp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_lock__(&gp->mtx);
    if (res != 0)
	return res;
    gp->open = 0;
    res = ethr_mutex_unlock__(&gp->mtx);
    return res;
}

int
ethr_gate_let_through(ethr_gate *gp, unsigned no)
{
    int res, ures;
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!gp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_lock__(&gp->mtx);
    if (res != 0)
	return res;
    gp->open += no;
    res = (gp->open == 1
	   ? ethr_cond_signal(&gp->cnd)
	   : ethr_cond_broadcast(&gp->cnd));
    ures = ethr_mutex_unlock__(&gp->mtx);
    if (res != 0)
	res = ures;
    return res;
}

int
ethr_gate_swait(ethr_gate *gp, int spincount)
{
    int res, ures, n;
#if ETHR_XCHK 
    if (ethr_not_inited) {
	ASSERT(0);
	return EACCES;
    }
    if (!gp) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    n = spincount;
    res = ethr_mutex_lock__(&gp->mtx);
    if (res != 0)
	return res;
    while (n >= 0 && !gp->open) {
	res = ethr_mutex_unlock__(&gp->mtx);
	if (res != 0)
	    return res;
	res = ethr_mutex_lock__(&gp->mtx);
	if (res != 0)
	    return res;
	n--;
    }
    while (!gp->open) {
	res = ethr_cond_wait(&gp->cnd, &gp->mtx);
	if (res != 0 && res != EINTR)
	    goto done;
    }
    gp->open--;
 done:
    ures = ethr_mutex_unlock__(&gp->mtx);
    if (res == 0)
	res = ures;
    return res;
}


int
ethr_gate_wait(ethr_gate *gp)
{
    return ethr_gate_swait(gp, 0);
}


/* rwmutex fallback */
#ifdef ETHR_USE_RWMTX_FALLBACK

int
ethr_rwmutex_init(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (!rwmtx) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_init(&rwmtx->mtx);
    if (res != 0)
	return res;
    ethr_cond_init(&rwmtx->rcnd);
    if (res != 0)
	goto error_cleanup1;
    res = ethr_cond_init(&rwmtx->wcnd);
    if (res != 0)
	goto error_cleanup2;
    rwmtx->readers = 0;
    rwmtx->waiting_readers = 0;
    rwmtx->waiting_writers = 0;
#if ETHR_XCHK
    rwmtx->initialized = ETHR_RWMUTEX_INITIALIZED;
#endif
    return 0;
 error_cleanup2:
    ethr_cond_destroy(&rwmtx->rcnd);
 error_cleanup1:
    ethr_mutex_destroy(&rwmtx->mtx);
    return res;
}

int
ethr_rwmutex_destroy(ethr_rwmutex *rwmtx)
{
    int res, pres;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
    rwmtx->initialized = 0;
#endif
    res = ethr_mutex_destroy(&rwmtx->mtx);
    pres = ethr_cond_destroy(&rwmtx->rcnd);
    if (res == 0)
	res = pres;
    pres = ethr_cond_destroy(&rwmtx->wcnd);
    if (res == 0)
	res = pres;
    return res;
}

int
ethr_rwmutex_tryrlock(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_trylock__(&rwmtx->mtx);
    if (res != 0)
	return res;
    if (rwmtx->waiting_writers) {
	res = ethr_mutex_unlock__(&rwmtx->mtx);
	if (res == 0)
	    return EBUSY;
	return res;
    }
    rwmtx->readers++;
    return ethr_mutex_unlock__(&rwmtx->mtx);
}

int
ethr_rwmutex_rlock(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_lock__(&rwmtx->mtx);
    if (res != 0)
	return res;
    while (rwmtx->waiting_writers) {
	rwmtx->waiting_readers++;
	res = ethr_cond_wait(&rwmtx->rcnd, &rwmtx->mtx);
	rwmtx->waiting_readers--;
	if (res != 0 && res != EINTR) {
	    (void) ethr_mutex_unlock__(&rwmtx->mtx);
	    return res;
	}
    }
    rwmtx->readers++;
    return ethr_mutex_unlock__(&rwmtx->mtx);
}

int
ethr_rwmutex_runlock(ethr_rwmutex *rwmtx)
{
    int res, ures;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_lock__(&rwmtx->mtx);
    if (res != 0)
	return res;
    rwmtx->readers--;
    if (!rwmtx->readers && rwmtx->waiting_writers)
	res = ethr_cond_signal(&rwmtx->wcnd);
    ures = ethr_mutex_unlock__(&rwmtx->mtx);
    if (res == 0)
	res = ures;
    return res;
}

int
ethr_rwmutex_tryrwlock(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_trylock__(&rwmtx->mtx);
    if (res != 0)
	return res;
    if (!rwmtx->readers && !rwmtx->waiting_writers)
	return 0;
    else {
	res = ethr_mutex_unlock__(&rwmtx->mtx);
	if (res == 0)
	    return EBUSY;
	return res;
    }
}

int
ethr_rwmutex_rwlock(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = ethr_mutex_lock__(&rwmtx->mtx);
    if (res != 0)
	return res;
    if (!rwmtx->readers && !rwmtx->waiting_writers) 
	return 0;
    
    while (rwmtx->readers) {
	rwmtx->waiting_writers++;
	res = ethr_cond_wait(&rwmtx->wcnd, &rwmtx->mtx);
	rwmtx->waiting_writers--;
	if (res != 0 && res != EINTR) {
	    (void) ethr_rwmutex_rwunlock(rwmtx);
	    return res;
	}
    }
    return 0;
}

int
ethr_rwmutex_rwunlock(ethr_rwmutex *rwmtx)
{
    int res, ures;
#if ETHR_XCHK
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ASSERT(0);
	return EINVAL;
    }
#endif
    res = 0;
    if (rwmtx->waiting_writers)
	res = ethr_cond_signal(&rwmtx->wcnd);
    else if (rwmtx->waiting_readers)
	res = ethr_cond_broadcast(&rwmtx->rcnd);
    ures = ethr_mutex_unlock__(&rwmtx->mtx);
    if (res == 0)
	res = ures;
    return res;
}

#endif /* #ifdef ETHR_USE_RWMTX_FALLBACK */

void
ethr_compiler_barrier(void)
{

}

#ifdef DEBUG

#include <stdio.h>
int ethr_assert_failed(char *f, int l, char *a)
{
    fprintf(stderr, "%s:%d: Assertion failed: %s\n", f, l, a);
    abort();
    return 0;
}

#endif


