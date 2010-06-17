/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010. All Rights Reserved.
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

#include <stdio.h>

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHR_AUX_IMPL__

#include "ethread.h"
#include "ethr_internal.h"
#include <string.h>
#include <limits.h>

#ifndef __WIN32__
#include <unistd.h>
#endif

#define ERTS_TS_EV_ALLOC_DEFAULT_POOL_SIZE 100
#define ERTS_TS_EV_ALLOC_POOL_SIZE 25

erts_cpu_info_t *ethr_cpu_info__;

int ethr_not_completely_inited__ = 1;
int ethr_not_inited__ = 1;

ethr_memory_allocators ethr_mem__ = ETHR_MEM_ALLOCS_DEF_INITER__;

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
ethr_atomic_protection_t ethr_atomic_protection__[1 << ETHR_ATOMIC_ADDR_BITS];
#endif

void *(*ethr_thr_prepare_func__)(void) = NULL;
void (*ethr_thr_parent_func__)(void *) = NULL;
void (*ethr_thr_child_func__)(void *) = NULL;

typedef struct ethr_xhndl_list_ ethr_xhndl_list;
struct ethr_xhndl_list_ {
    ethr_xhndl_list *next;
    void (*funcp)(void);
};

size_t ethr_pagesize__;
size_t ethr_min_stack_size__; /* kilo words */
size_t ethr_max_stack_size__; /* kilo words */

ethr_rwmutex xhndl_rwmtx;
ethr_xhndl_list *xhndl_list;

static int main_threads;

static int init_ts_event_alloc(void);

int
ethr_init_common__(ethr_init_data *id)
{
    int res;
    if (id) {
	ethr_thr_prepare_func__	= id->thread_create_prepare_func;
	ethr_thr_parent_func__	= id->thread_create_parent_func;
	ethr_thr_child_func__	= id->thread_create_child_func;
    }

    ethr_cpu_info__ = erts_cpu_info_create();
    if (!ethr_cpu_info__)
	return ENOMEM;

#ifdef _SC_PAGESIZE
    ethr_pagesize__ = (size_t) sysconf(_SC_PAGESIZE);
#elif defined(HAVE_GETPAGESIZE)
    ethr_pagesize__ = (size_t) getpagesize();
#else
    ethr_pagesize__ = (size_t) 4*1024; /* Guess 4 KB */
#endif

    /* User needs at least 4 KB */
    ethr_min_stack_size__ = 4*1024;
#if SIZEOF_VOID_P == 8
    /* Double that on 64-bit archs */
    ethr_min_stack_size__ *= 2;
#endif
    /* On some systems as much as about 4 KB is used by the system */
    ethr_min_stack_size__ += 4*1024;
    /* There should be room for signal handlers */
#ifdef SIGSTKSZ
    ethr_min_stack_size__ += SIGSTKSZ;
#else
    ethr_min_stack_size__ += ethr_pagesize__;
#endif
    /* The system may think that we need more stack */
#if defined(PTHREAD_STACK_MIN)
    if (ethr_min_stack_size__ < PTHREAD_STACK_MIN)
	ethr_min_stack_size__ = PTHREAD_STACK_MIN;
#elif defined(_SC_THREAD_STACK_MIN)
    {
	size_t thr_min_stk_sz = (size_t) sysconf(_SC_THREAD_STACK_MIN);
	if (ethr_min_stack_size__ < thr_min_stk_sz)
	    ethr_min_stack_size__ = thr_min_stk_sz;
    }
#endif
    /* The guard is at least on some platforms included in the stack size
       passed when creating threads */
#ifdef ETHR_STACK_GUARD_SIZE
    ethr_min_stack_size__ += ETHR_STACK_GUARD_SIZE;
#endif
    ethr_min_stack_size__ = ETHR_PAGE_ALIGN(ethr_min_stack_size__);

    ethr_min_stack_size__ = ETHR_B2KW(ethr_min_stack_size__);

    ethr_max_stack_size__ = 32*1024*1024;
#if SIZEOF_VOID_P == 8
    ethr_max_stack_size__ *= 2;
#endif
    ethr_max_stack_size__ = ETHR_B2KW(ethr_max_stack_size__);

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS
    {
	int i;
	for (i = 0; i < (1 << ETHR_ATOMIC_ADDR_BITS); i++) {
	    res = ethr_spinlock_init(&ethr_atomic_protection__[i].u.lck);
	    if (res != 0)
		return res;
	}
    }
#endif

    res = ethr_mutex_lib_init(erts_get_cpu_configured(ethr_cpu_info__));
    if (res != 0)
	return res;

    xhndl_list = NULL;

    return 0;
}

int
ethr_late_init_common__(ethr_late_init_data *lid)
{
    ethr_ts_event *tsep = NULL;
    int reader_groups;
    int res;
    int i;
    ethr_memory_allocator *m[] = {&ethr_mem__.std,
				  &ethr_mem__.sl,
				  &ethr_mem__.ll};
    if (lid)
	ethr_mem__ = lid->mem;
    if (!ethr_mem__.std.alloc
	|| !ethr_mem__.std.realloc
	|| !ethr_mem__.std.free) {
	ethr_mem__.std.alloc = malloc;
	ethr_mem__.std.realloc = realloc;
	ethr_mem__.std.free = free;
    }
    for (i = 0; i < sizeof(m)/sizeof(m[0]); i++) {
	if (!m[i]->alloc || !m[i]->realloc || !m[i]->free) {
	    m[i]->alloc = ethr_mem__.std.alloc;
	    m[i]->realloc = ethr_mem__.std.realloc;
	    m[i]->free = ethr_mem__.std.free;
	}

    }
    res = init_ts_event_alloc();
    if (res != 0)
	return res;
    res = ethr_make_ts_event__(&tsep);
    if (res == 0)
	tsep->iflgs |= ETHR_TS_EV_ETHREAD;
    if (!lid) {
	main_threads = 0;
	reader_groups = 0;
    }
    else {
	if (lid->main_threads < 0 || USHRT_MAX < lid->main_threads)
	    return res;
	main_threads = lid->main_threads;
	reader_groups = lid->reader_groups;
    }
    res = ethr_mutex_lib_late_init(reader_groups, main_threads);
    if (res != 0)
	return res;
    ethr_not_completely_inited__ = 0; /* Need it for
					 rwmutex_init */
    res = ethr_rwmutex_init(&xhndl_rwmtx);
    ethr_not_completely_inited__ = 1;
    if (res != 0)
	return res;
    return 0;
}

int
ethr_install_exit_handler(void (*funcp)(void))
{
    ethr_xhndl_list *xhp;

#if ETHR_XCHK
    if (ethr_not_completely_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
#endif

    if (!funcp)
	return EINVAL;

    xhp = (ethr_xhndl_list *) ethr_mem__.std.alloc(sizeof(ethr_xhndl_list));
    if (!xhp)
	return ENOMEM;

    ethr_rwmutex_rwlock(&xhndl_rwmtx);

    xhp->funcp = funcp;
    xhp->next = xhndl_list;
    xhndl_list = xhp;

    ethr_rwmutex_rwunlock(&xhndl_rwmtx);

    return 0;
}

void
ethr_run_exit_handlers__(void)
{
    ethr_xhndl_list *xhp;

    ethr_rwmutex_rlock(&xhndl_rwmtx);

    xhp = xhndl_list;

    ethr_rwmutex_runlock(&xhndl_rwmtx);

    for (; xhp; xhp = xhp->next)
	(*xhp->funcp)();
}

/*
 * Thread specific event alloc, etc.
 *
 * Note that we don't know when it is safe to destroy an event, but
 * we know when it is safe to reuse it. ts_event_free() therefore
 * never destroys an event (but makes freed events available for
 * reuse).
 *
 * We could easily keep track of the usage of events, and by this
 * make it possible to destroy events. We would however suffer a
 * performance penalty for this and save very little memory.
 */

typedef union {
    ethr_ts_event ts_ev;
    char align[ETHR_CACHE_LINE_ALIGN_SIZE(sizeof(ethr_ts_event))];
} ethr_aligned_ts_event;

static ethr_spinlock_t ts_ev_alloc_lock;
static ethr_ts_event *free_ts_ev;

#if SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int EthrPtrSzUInt;
#elif  SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long EthrPtrSzUInt;
#else
#error No pointer sized integer type
#endif

static ethr_ts_event *ts_event_pool(int size, ethr_ts_event **endpp)
{
    int i;
    ethr_aligned_ts_event *atsev;
    atsev = ethr_mem__.std.alloc(sizeof(ethr_aligned_ts_event) * size
				 + ETHR_CACHE_LINE_SIZE);
    if (!atsev)
	return NULL;
    if ((((EthrPtrSzUInt) atsev) & ETHR_CACHE_LINE_MASK) == 0)
	atsev = ((ethr_aligned_ts_event *)
		 ((((EthrPtrSzUInt) atsev) & ~ETHR_CACHE_LINE_MASK)
		  + ETHR_CACHE_LINE_SIZE));
    for (i = 1; i < size; i++) {
	atsev[i-1].ts_ev.next = &atsev[i].ts_ev;
	ethr_atomic_init(&atsev[i-1].ts_ev.uaflgs, 0);
	atsev[i-1].ts_ev.iflgs = 0;
    }
    ethr_atomic_init(&atsev[size-1].ts_ev.uaflgs, 0);
    atsev[size-1].ts_ev.iflgs = 0;
    atsev[size-1].ts_ev.next = NULL;
    if (endpp)
	*endpp = &atsev[size-1].ts_ev;
    return &atsev[0].ts_ev;
}

static int init_ts_event_alloc(void)
{
    free_ts_ev = ts_event_pool(ERTS_TS_EV_ALLOC_DEFAULT_POOL_SIZE,
			       NULL);
    if (!free_ts_ev)
	return ENOMEM;
    return ethr_spinlock_init(&ts_ev_alloc_lock);
}

static ethr_ts_event *ts_event_alloc(void)
{
    ethr_ts_event *ts_ev;
    ethr_spin_lock(&ts_ev_alloc_lock);
    if (free_ts_ev) {
	ts_ev = free_ts_ev;
	free_ts_ev = ts_ev->next;
	ethr_spin_unlock(&ts_ev_alloc_lock);
    }
    else {
	ethr_ts_event *ts_ev_pool_end;
	ethr_spin_unlock(&ts_ev_alloc_lock);

	ts_ev = ts_event_pool(ERTS_TS_EV_ALLOC_POOL_SIZE, &ts_ev_pool_end);
	if (!ts_ev)
	    return NULL;

	ethr_spin_lock(&ts_ev_alloc_lock);
	ts_ev_pool_end->next = free_ts_ev;
	free_ts_ev = ts_ev->next;
	ethr_spin_unlock(&ts_ev_alloc_lock);
    }
    return ts_ev;
}

static void ts_event_free(ethr_ts_event *ts_ev)
{
    ETHR_ASSERT(!ts_ev->udata);
    ethr_spin_lock(&ts_ev_alloc_lock);
    ts_ev->next = free_ts_ev;
    free_ts_ev = ts_ev;
    ethr_spin_unlock(&ts_ev_alloc_lock);
}

int ethr_make_ts_event__(ethr_ts_event **tsepp)
{
    int res;
    ethr_ts_event *tsep = *tsepp;

    if (!tsep) {
	tsep = ts_event_alloc();
	if (!tsep)
	    return ENOMEM;
    }

    if ((tsep->iflgs & ETHR_TS_EV_INITED) == 0) {
	res = ethr_event_init(&tsep->event);
	if (res != 0) {
	    ts_event_free(tsep);
	    return res;
	}
    }

    tsep->iflgs = ETHR_TS_EV_INITED;
    tsep->udata = NULL;
    tsep->rgix = 0;
    tsep->mtix = 0;

    res = ethr_set_tse__(tsep);
    if (res != 0 && tsepp && *tsepp) {
	ts_event_free(tsep);
	return res;
    }

    if (tsepp)
	*tsepp = tsep;

    return 0;
}

int ethr_get_tmp_ts_event__(ethr_ts_event **tsepp)
{
    int res;
    ethr_ts_event *tsep = *tsepp;

    if (!tsep) {
	tsep = ts_event_alloc();
	if (!tsep)
	    return ENOMEM;
    }

    if ((tsep->iflgs & ETHR_TS_EV_INITED) == 0) {
	res = ethr_event_init(&tsep->event);
	if (res != 0) {
	    ts_event_free(tsep);
	    return res;
	}
    }

    tsep->iflgs = ETHR_TS_EV_INITED|ETHR_TS_EV_TMP;
    tsep->udata = NULL;

    if (tsepp)
	*tsepp = tsep;

    return 0;
}

int ethr_free_ts_event__(ethr_ts_event *tsep)
{
    ts_event_free(tsep);
    return 0;
}

void ethr_ts_event_destructor__(void *vtsep)
{
    if (vtsep) {
	ethr_ts_event *tsep = (ethr_ts_event *) vtsep;
	ts_event_free(tsep);
	ethr_set_tse__(NULL);
    }
}

int ethr_set_main_thr_status(int on, int no)
{
    ethr_ts_event *tsep = ethr_get_tse__();
    if (!tsep)
	return EINVAL;
    if (on) {
	if (no < 1 || main_threads < no)
	    return EINVAL;
	tsep->mtix = (unsigned short) no;
	tsep->iflgs |= ETHR_TS_EV_MAIN_THR;
    }
    else {
	tsep->iflgs &= ~ETHR_TS_EV_MAIN_THR;
	tsep->mtix = (unsigned short) 0;
    }
    return 0;
}

int ethr_get_main_thr_status(int *on)
{
    ethr_ts_event *tsep = ethr_get_tse__();
    if (!tsep)
	*on = 0;
    else {
	if (tsep->iflgs & ETHR_TS_EV_MAIN_THR)
	    *on = 1;
	else
	    *on = 0;
    }
    return 0;
}


/* Atomics */

void
ethr_atomic_init(ethr_atomic_t *var, long i)
{
    ETHR_ASSERT(var);
    ethr_atomic_init__(var, i);
}

void
ethr_atomic_set(ethr_atomic_t *var, long i)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_set__(var, i);
}

long
ethr_atomic_read(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read__(var);
}


long
ethr_atomic_add_read(ethr_atomic_t *var, long incr)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_add_read__(var, incr);
}   

long
ethr_atomic_inc_read(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_inc_read__(var);
}

long
ethr_atomic_dec_read(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_dec_read__(var);
}

void
ethr_atomic_add(ethr_atomic_t *var, long incr)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_add__(var, incr);
}   
    
void
ethr_atomic_inc(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_inc__(var);
}

void
ethr_atomic_dec(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_dec__(var);
}

long
ethr_atomic_read_band(ethr_atomic_t *var, long mask)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read_band__(var, mask);
}

long
ethr_atomic_read_bor(ethr_atomic_t *var, long mask)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read_bor__(var, mask);
}

long
ethr_atomic_xchg(ethr_atomic_t *var, long new)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_xchg__(var, new);
}   

long
ethr_atomic_cmpxchg(ethr_atomic_t *var, long new, long expected)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_cmpxchg__(var, new, expected);
}

long
ethr_atomic_read_acqb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read_acqb__(var);
}

long
ethr_atomic_inc_read_acqb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_inc_read_acqb__(var);
}

void
ethr_atomic_set_relb(ethr_atomic_t *var, long i)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_set_relb__(var, i);
}

void
ethr_atomic_dec_relb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_dec_relb__(var);
}

long
ethr_atomic_dec_read_relb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_dec_read_relb__(var);
}

long
ethr_atomic_cmpxchg_acqb(ethr_atomic_t *var, long new, long exp)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_cmpxchg_acqb__(var, new, exp);
}

long
ethr_atomic_cmpxchg_relb(ethr_atomic_t *var, long new, long exp)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_cmpxchg_relb__(var, new, exp);
}


/* Spinlocks and rwspinlocks */

int
ethr_spinlock_init(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (!lock) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_spinlock_init__(lock);
}

int
ethr_spinlock_destroy(ethr_spinlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_spinlock_destroy__(lock);
}

void
ethr_spin_unlock(ethr_spinlock_t *lock)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(lock);
    ethr_spin_unlock__(lock);
}

void
ethr_spin_lock(ethr_spinlock_t *lock)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(lock);
    ethr_spin_lock__(lock);
}

int
ethr_rwlock_init(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (!lock) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwlock_init__(lock);
}

int
ethr_rwlock_destroy(ethr_rwlock_t *lock)
{
#if ETHR_XCHK 
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!lock) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    return ethr_rwlock_destroy__(lock);
}

void
ethr_read_unlock(ethr_rwlock_t *lock)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(lock);
    ethr_read_unlock__(lock);
}

void
ethr_read_lock(ethr_rwlock_t *lock)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(lock);
    ethr_read_lock__(lock);
}

void
ethr_write_unlock(ethr_rwlock_t *lock)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(lock);
    ethr_write_unlock__(lock);
}

void
ethr_write_lock(ethr_rwlock_t *lock)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(lock);
    ethr_write_lock__(lock);
}

ETHR_IMPL_NORETURN__ ethr_fatal_error__(const char *file,
					int line,
					const char *func,
					int err)
{
    char *errstr;
    if (err == ENOTSUP)
	errstr = "Operation not supported";
    else {
	errstr = strerror(err);
	if (!errstr)
	    errstr = "Unknown error";
    }
    fprintf(stderr, "%s:%d: Fatal error in %s(): %s (%d)\n",
	    file, line, func, errstr, err);
    ethr_abort__();
}

int ethr_assert_failed(const char *file, int line, const char *func, char *a)
{
    fprintf(stderr, "%s:%d: %s(): Assertion failed: %s\n", file, line, func, a);
    ethr_abort__();
    return 0;
}
