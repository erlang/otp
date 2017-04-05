/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

#define ERTS_TS_EV_ALLOC_DEFAULT_POOL_SIZE 2048
#define ERTS_TS_EV_ALLOC_POOL_SIZE 32

erts_cpu_info_t *ethr_cpu_info__;

int ethr_not_completely_inited__ = 1;
int ethr_not_inited__ = 1;

ethr_memory_allocators ethr_mem__ = ETHR_MEM_ALLOCS_DEF_INITER__;

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

static ethr_tsd_key ethr_stacklimit_key__;

static int main_threads;

static int init_ts_event_alloc(void);

ethr_runtime_t ethr_runtime__
#ifdef __GNUC__
__attribute__ ((aligned (ETHR_CACHE_LINE_SIZE)))
#endif
    ;

#if defined(ETHR_X86_RUNTIME_CONF__)

/*
 * x86/x86_64 specifics shared between windows and
 * pthread implementations.
 */

#define ETHR_IS_X86_VENDOR(V, B, C, D) \
   (sizeof(V) == 13 && is_x86_vendor((V), (B), (C), (D)))

static ETHR_INLINE int
is_x86_vendor(char *str, int ebx, int ecx, int edx)
{
    return (*((int *) &str[0]) == ebx
	    && *((int *) &str[sizeof(int)]) == edx
	    && *((int *) &str[sizeof(int)*2]) == ecx);
}

static void
x86_init(void)
{
    int eax, ebx, ecx, edx;

    eax = ebx = ecx = edx = 0;

    ethr_x86_cpuid__(&eax, &ebx, &ecx, &edx);

    if (eax > 0
	&& (ETHR_IS_X86_VENDOR("GenuineIntel", ebx, ecx, edx)
	    || ETHR_IS_X86_VENDOR("AuthenticAMD", ebx, ecx, edx))) {
	eax = 1;
	ethr_x86_cpuid__(&eax, &ebx, &ecx, &edx);
    }
    else {
	/*
	 * The meaning of the feature flags for this
	 * vendor have not been verified.
	 */
	eax = ebx = ecx = edx = 0;
    }

    /*
     * The feature flags tested below have only been verified
     * for vendors checked above. Also note that only these
     * feature flags have been verified to have these specific
     * meanings. If another feature flag test is introduced,
     * it has to be verified to have the same meaning for all
     * vendors above.
     */

#if ETHR_SIZEOF_PTR == 8
    /* bit 13 of ecx is set if we have cmpxchg16b */
    ethr_runtime__.conf.have_dw_cmpxchg = (ecx & (1 << 13));
#elif ETHR_SIZEOF_PTR == 4
    /* bit 8 of edx is set if we have cmpxchg8b */
    ethr_runtime__.conf.have_dw_cmpxchg = (edx & (1 << 8));
#else
#  error "Not supported"
#endif
    /* bit 26 of edx is set if we have sse2 */
    ethr_runtime__.conf.have_sse2 = (edx & (1 << 26));

    /* check if we have extended feature set */
    eax = 0x80000000;
    ethr_x86_cpuid__(&eax, &ebx, &ecx, &edx);

    if (eax < 0x80000001)
        return;

    if (eax >= 0x80000007) {
        /* Advanced Power Management Information */
        eax = 0x80000007;
	ethr_x86_cpuid__(&eax, &ebx, &ecx, &edx);

        /* I got the values below from:
           http://lxr.free-electrons.com/source/arch/x86/include/asm/cpufeature.h
           They can be gotten from the intel/amd manual as well.
        */

        ethr_runtime__.conf.have_constant_tsc  = (edx & (1 <<  8));
        ethr_runtime__.conf.have_tsc_reliable   = (edx & (1 << 23));
        ethr_runtime__.conf.have_nonstop_tsc    = (edx & (1 << 24));
        ethr_runtime__.conf.have_nonstop_tsc_s3 = (edx & (1 << 30));

    }

    /* Extended Processor Info and Feature Bits */
    eax = 0x80000001;
    ethr_x86_cpuid__(&eax, &ebx, &ecx, &edx);

    /* bit 27 of edx is set if we have rdtscp */
    ethr_runtime__.conf.have_rdtscp = (edx & (1 << 27));

}

#endif /* ETHR_X86_RUNTIME_CONF__ */


int
ethr_init_common__(ethr_init_data *id)
{
    int res;

    ethr_init_event__();

#if defined(ETHR_X86_RUNTIME_CONF__)
    x86_init();
#endif

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
#if defined(ETHR_HAVE_USABLE_PTHREAD_STACK_MIN)
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

    ethr_max_stack_size__ = 32*1024*1024;
#if SIZEOF_VOID_P == 8
    ethr_max_stack_size__ *= 2;
#endif
    ethr_max_stack_size__ = ETHR_PAGE_ALIGN(ethr_max_stack_size__);

    res = ethr_init_atomics();
    if (res != 0)
	return res;

    res = ethr_mutex_lib_init(erts_get_cpu_configured(ethr_cpu_info__));
    if (res != 0)
	return res;

    res = ethr_tsd_key_create(&ethr_stacklimit_key__, "stacklimit");
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

/*
 * Stack limit
 */

void *ethr_get_stacklimit(void)
{
    return ethr_tsd_get(ethr_stacklimit_key__);
}

int ethr_set_stacklimit(void *limit)
{
    void *prev = ethr_tsd_get(ethr_stacklimit_key__);
    if (prev)
        return EACCES;
    if (!limit)
        return EINVAL;
    return ethr_tsd_set(ethr_stacklimit_key__, limit);
}

/* internal stacklimit (thread creation) */

void
ethr_set_stacklimit__(char *prev_c, size_t stacksize)
{
    /*
     * We *don't* want this function inlined, i.e., it is
     * risky to call this function from another function
     * in ethr_aux.c
     */
    void *limit = NULL;
    char c;
    int res;

    if (stacksize) {
        char *start;
        if (&c > prev_c) {
            start = (char *) ((((ethr_uint_t) prev_c)
                               / ethr_pagesize__)
                              * ethr_pagesize__);
            limit = start + stacksize;
        }
        else {
            start = (char *) (((((ethr_uint_t) prev_c) - 1)
                               / ethr_pagesize__ + 1)
                              * ethr_pagesize__);
            limit = start - stacksize;
        }
    }

    res = ethr_tsd_set(ethr_stacklimit_key__, limit);
    if (res != 0)
        ethr_abort__();
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

static ethr_ts_event *ts_event_pool(int size, ethr_ts_event **endpp)
{
    int i;
    ethr_aligned_ts_event *atsev;
    atsev = ethr_mem__.std.alloc(sizeof(ethr_aligned_ts_event) * size
				 + ETHR_CACHE_LINE_SIZE - 1);
    if (!atsev)
	return NULL;
    if ((((ethr_uint_t) atsev) & ETHR_CACHE_LINE_MASK) != 0)
	atsev = ((ethr_aligned_ts_event *)
		 ((((ethr_uint_t) atsev) & ~ETHR_CACHE_LINE_MASK)
		  + ETHR_CACHE_LINE_SIZE));
    for (i = 1; i < size; i++) {
	atsev[i-1].ts_ev.next = &atsev[i].ts_ev;
	ethr_atomic32_init(&atsev[i-1].ts_ev.uaflgs, 0);
	atsev[i-1].ts_ev.iflgs = 0;
    }
    ethr_atomic32_init(&atsev[size-1].ts_ev.uaflgs, 0);
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
